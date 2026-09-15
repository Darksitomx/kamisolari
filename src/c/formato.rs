//! Cadenas de formato estilo `println!` / `format!`.
//!
//! Subconjunto soportado:
//! - `{{` y `}}` → llaves literales.
//! - `{}` → siguiente argumento.
//! - `{0}`, `{1}` → argumento posicional.
//! - `{nombre}` → captura del scope (variable visible).
//! - `{:?}`, `{0:?}`, `{nombre:?}` → formato Debug.
//!
//! Todo lo demás (`{:x}`, `{:.2}`, `{:<5}`, `{nombre.campo}`…) se rechaza
//! con un error claro: C no tiene ese formato gratis.

/// Un pedazo de una cadena de formato ya parseada.
#[derive(Debug, Clone, PartialEq)]
pub enum Segmento {
    /// Texto literal (ya con `{{`/`}}` colapsados).
    Literal(String),
    /// Hueco a rellenar con un argumento.
    Hueco(Hueco),
}

/// Referencia al argumento de un hueco.
#[derive(Debug, Clone, PartialEq)]
pub enum ArgRef {
    /// `{}`: el siguiente argumento en orden.
    Auto,
    /// `{0}`: argumento por posición.
    Pos(usize),
    /// `{nombre}`: captura del scope.
    Nombre(String),
}

/// Un hueco `{...}`.
#[derive(Debug, Clone, PartialEq)]
pub struct Hueco {
    pub arg: ArgRef,
    /// `true` con `:?` (Debug), `false` con Display.
    pub debug: bool,
}

/// Parsea una cadena de formato (valor ya sin escapes de Rust).
pub fn parsea(cadena: &str) -> Result<Vec<Segmento>, String> {
    let mut segs = Vec::new();
    let mut lit = String::new();
    let mut chars = cadena.chars().peekable();

    let mut vacia_lit = |segs: &mut Vec<Segmento>, lit: &mut String| {
        if !lit.is_empty() {
            segs.push(Segmento::Literal(std::mem::take(lit)));
        }
    };

    while let Some(c) = chars.next() {
        match c {
            '{' => match chars.next() {
                Some('{') => lit.push('{'),
                Some('}') => {
                    vacia_lit(&mut segs, &mut lit);
                    segs.push(Segmento::Hueco(Hueco {
                        arg: ArgRef::Auto,
                        debug: false,
                    }));
                }
                Some(resto) => {
                    let mut inner = String::new();
                    inner.push(resto);
                    let mut cerrada = false;
                    for c2 in chars.by_ref() {
                        if c2 == '}' {
                            cerrada = true;
                            break;
                        }
                        inner.push(c2);
                    }
                    if !cerrada {
                        return Err("llave `{` sin cerrar".into());
                    }
                    vacia_lit(&mut segs, &mut lit);
                    segs.push(Segmento::Hueco(parsea_hueco(&inner)?));
                }
                None => return Err("llave `{` sin cerrar al final".into()),
            },
            '}' => match chars.next() {
                Some('}') => lit.push('}'),
                _ => return Err("llave `}` suelta (usa `}}` para imprimirla)".into()),
            },
            _ => lit.push(c),
        }
    }
    vacia_lit(&mut segs, &mut lit);

    // Rust prohíbe mezclar `{}` con `{0}`/`{nombre}`.
    let hay_auto = segs.iter().any(|s| {
        matches!(s, Segmento::Hueco(h) if matches!(h.arg, ArgRef::Auto))
    });
    let hay_expl = segs.iter().any(|s| {
        matches!(s, Segmento::Hueco(h) if !matches!(h.arg, ArgRef::Auto))
    });
    if hay_auto && hay_expl {
        return Err("no mezcles `{}` con `{0}` o `{nombre}` en el mismo formato".into());
    }
    Ok(segs)
}

fn parsea_hueco(inner: &str) -> Result<Hueco, String> {
    // Forma: [arg] [: espec] — solo aceptamos espec vacío o `?`/`#?`.
    let (arg_txt, espec) = match inner.split_once(':') {
        Some((a, e)) => (a, Some(e)),
        None => (inner, None),
    };
    let debug = match espec {
        None | Some("") => false,
        Some("?") | Some("#?") => true,
        Some(otro) => {
            return Err(format!(
                "especificador de formato `:{otro}` no soportado en C (solo `{{}}` y `{{:?}}`)"
            ))
        }
    };
    let arg_txt = arg_txt.trim();
    if arg_txt.is_empty() {
        // `{:` o `{}` ya se maneja arriba; `{:` solo es error.
        if espec.is_some() {
            return Err("hueco `{:` vacío".into());
        }
        return Ok(Hueco {
            arg: ArgRef::Auto,
            debug,
        });
    }
    if let Ok(pos) = arg_txt.parse::<usize>() {
        return Ok(Hueco {
            arg: ArgRef::Pos(pos),
            debug,
        });
    }
    if es_ident(arg_txt) {
        return Ok(Hueco {
            arg: ArgRef::Nombre(arg_txt.to_string()),
            debug,
        });
    }
    Err(format!(
        "hueco `{{{inner}}}` no soportado (usa `{{}}`, `{{0}}`, `{{nombre}}` o `{{:?}}`)"
    ))
}

fn es_ident(s: &str) -> bool {
    let mut cs = s.chars();
    match cs.next() {
        Some(c) if c == '_' || c.is_alphabetic() => {}
        _ => return false,
    }
    cs.all(|c| c == '_' || c.is_alphanumeric())
}

/// Asigna cada hueco a un índice de argumento explícito.
///
/// `n_args` = número de argumentos posicionales disponibles (sin contar
/// capturas, que se resuelven contra el scope).
/// Devuelve por cada hueco: `Some(i)` si usa el argumento `i`,
/// `None` si es captura `{nombre}` (el llamador resuelve el nombre).
pub fn asigna(huecos: &[Hueco], n_args: usize) -> Result<Vec<Option<usize>>, String> {
    let mut out = Vec::with_capacity(huecos.len());
    let mut auto = 0usize;
    let mut usados = vec![false; n_args];
    for h in huecos {
        match &h.arg {
            ArgRef::Auto => {
                if auto >= n_args {
                    return Err(format!(
                        "el formato pide {} argumentos pero solo hay {n_args}",
                        auto + 1
                    ));
                }
                usados[auto] = true;
                out.push(Some(auto));
                auto += 1;
            }
            ArgRef::Pos(i) => {
                if *i >= n_args {
                    return Err(format!(
                        "el formato usa `{{{i}}}` pero solo hay {n_args} argumentos"
                    ));
                }
                usados[*i] = true;
                out.push(Some(*i));
            }
            ArgRef::Nombre(_) => out.push(None),
        }
    }
    if let Some(i) = usados.iter().position(|u| !u) {
        if n_args > 0 {
            return Err(format!(
                "el argumento {i} nunca se usa en el formato (¿sobra o falta un `{{}}`?)"
            ));
        }
    }
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basico() {
        let s = parsea("hola {} fin").unwrap();
        assert_eq!(s.len(), 3);
        assert!(matches!(s[1], Segmento::Hueco(_)));
    }

    #[test]
    fn llaves_escapadas() {
        let s = parsea("{{}} {{{}}}").unwrap();
        assert_eq!(
            s,
            vec![
                Segmento::Literal("{} {".into()),
                Segmento::Hueco(Hueco {
                    arg: ArgRef::Auto,
                    debug: false,
                }),
                Segmento::Literal("}".into()),
            ]
        );
    }

    #[test]
    fn debug_y_nombres() {
        let s = parsea("{x:?} {0}").unwrap();
        assert!(matches!(
            &s[0],
            Segmento::Hueco(Hueco {
                arg: ArgRef::Nombre(n),
                debug: true
            }) if n == "x"
        ));
    }

    #[test]
    fn rechaza_formato_raro() {
        assert!(parsea("{:.2}").is_err());
        assert!(parsea("{:x}").is_err());
        assert!(parsea("{a.b}").is_err());
        assert!(parsea("{").is_err());
        assert!(parsea("}").is_err());
    }

    #[test]
    fn mezcla_prohibida() {
        assert!(parsea("{} {0}").is_err());
    }

    #[test]
    fn cuenta_argumentos() {
        let s = parsea("{} {}").unwrap();
        let huecos: Vec<Hueco> = s
            .iter()
            .filter_map(|x| match x {
                Segmento::Hueco(h) => Some(h.clone()),
                _ => None,
            })
            .collect();
        assert!(asigna(&huecos, 2).is_ok());
        assert!(asigna(&huecos, 1).is_err());
        assert!(asigna(&huecos, 3).is_err());
    }
}
