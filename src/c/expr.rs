//! Pase 2: expresiones a C.
//!
//! `baja_expr` baja cualquier expresión a un [`ExVal`] (código + tipo +
//! lugar). `consume` lo convierte a código final: mueve variables dueñas,
//! clona lugares droppables y aplica el tipo esperado (coerciones reales:
//! `&String`→`&str`, `&Vec`→`&[...]`, `&[T;N]`→`&[T]`).
//!
//! Aritmética entera con signo: chequea overflow como Rust-debug (excepto
//! `wrapping_*`, que envuelve). Índices: chequean rango con `kami_indice`.

use syn::spanned::Spanned;

use super::baja::{Bajador, Destino, InfoBucle, Receptor};
use super::errores::CError;
use super::formato::{asigna, parsea, ArgRef, Segmento};
use super::tipos::{self, CType, LargoArreglo};

/// Valor bajado: código C + tipo + (opcional) lugar asignable.
#[derive(Debug, Clone)]
pub struct ExVal {
    /// Código C que produce el valor (expresión pura).
    pub c: String,
    /// Tipo kamisolari del valor.
    pub tipo: CType,
    /// Variable que se MUEVE al consumir (dueña leída por valor).
    pub movible: Option<String>,
    /// Lugar asignable (`x`, `s.campo`, `v[i]`), si lo hay.
    pub lugar: Option<String>,
    /// Inicializador de arreglo (se materializa en `sea`/asignación).
    pub arreglo: Option<ArrayInit>,
}

/// Elemento de `[a, b, c]` (ya consumido).
#[derive(Debug, Clone)]
pub struct ElemLista {
    pub c: String,
    pub tipo: CType,
}

/// Inicializador de arreglo diferido.
#[derive(Debug, Clone)]
pub enum ArrayInit {
    Lista(Vec<ElemLista>),
    Repite { c: String, tipo: CType, n: String },
}

impl ExVal {
    pub(crate) fn puro(c: String, tipo: CType) -> Self {
        ExVal { c, tipo, movible: None, lugar: None, arreglo: None }
    }
    pub(crate) fn lugar(c: String, tipo: CType, lugar: String) -> Self {
        ExVal { c, tipo, movible: None, lugar: Some(lugar), arreglo: None }
    }
    pub(crate) fn movible(c: String, tipo: CType, n: String) -> Self {
        ExVal { c, tipo, movible: Some(n), lugar: None, arreglo: None }
    }
}

fn recorta_c(s: &str) -> String {
    let t: String = s.chars().take(48).collect();
    if s.chars().count() > 48 {
        format!("{t}…")
    } else {
        t
    }
}

impl Bajador {
    pub(crate) fn err_en<S: Spanned>(&self, codigo: &'static str, msg: String, sp: S) -> CError {
        CError::nuevo(codigo, msg).con_span(&sp, &self.lineas).con_archivo(self.archivo.clone())
    }

    // ==================================================================
    // Entrada principal
    // ==================================================================

    /// Baja una expresión. `esp` es el tipo esperado (guía literales,
    /// `Ninguno`, `vec![]`, constructores...). `_` se trata como ausencia.
    pub(crate) fn baja_expr(
        &mut self,
        e: &syn::Expr,
        esp: Option<&CType>,
    ) -> Result<ExVal, CError> {
        let esp = match esp {
            Some(CType::Infer) | None => None,
            s => s,
        };
        match e {
            syn::Expr::Array(x) => self.ex_array(x, esp),
            syn::Expr::Assign(x) => self.ex_assign(x, esp),
            syn::Expr::Async(x) => Err(self.err_en("C0003", "bloques `asinc` no caben en C".into(), x)),
            syn::Expr::Await(x) => Err(self.err_en("C0003", "`.await` no cabe en C (sin executor)".into(), x)),
            syn::Expr::Binary(x) => self.ex_binary(x, esp),
            syn::Expr::Block(x) => {
                if x.label.is_some() {
                    return Err(self.err_en("C0003", "bloques con etiqueta no caben".into(), x)
                        .ayuda("usa un `ciclo` con etiqueta o reestructura el código"));
                }
                self.ex_bloque(&x.block, esp)
            }
            syn::Expr::Break(x) => {
                self.baja_break(x)?;
                self.divergente(esp)
            }
            syn::Expr::Call(x) => self.ex_call(x, esp),
            syn::Expr::Cast(x) => self.ex_cast(x, esp),
            syn::Expr::Closure(x) => Err(self.err_en(
                "C0003",
                "cierres (`|x| ...`) no caben en C (sin lambdas ni traits Fn)".into(),
                x,
            )
            .ayuda("usa una función libre o un método; `sort_by` acepta comparadores libres")),
            syn::Expr::Const(x) => self.ex_bloque(&x.block, esp),
            syn::Expr::Continue(x) => {
                self.baja_continue(x)?;
                self.divergente(esp)
            }
            syn::Expr::Field(x) => self.ex_field(x, esp),
            syn::Expr::ForLoop(x) => self.ex_for(x),
            syn::Expr::Group(x) => self.baja_expr(&x.expr, esp),
            syn::Expr::If(x) => self.ex_if(x, esp),
            syn::Expr::Index(x) => self.ex_index(x, esp),
            syn::Expr::Infer(x) => Err(self.err_en("C0005", "`_` como valor no cabe".into(), x)),
            syn::Expr::Let(x) => Err(self.err_en(
                "C0003",
                "`sea` como expresión solo vale en `si sea` / `mientras sea`".into(),
                x,
            )),
            syn::Expr::Lit(x) => self.ex_lit(x, esp),
            syn::Expr::Loop(x) => self.ex_loop(x, esp),
            syn::Expr::Macro(x) => self.ex_macro(&x.mac, esp),
            syn::Expr::Match(x) => self.ex_match(x, esp),
            syn::Expr::MethodCall(x) => self.ex_methodcall(x, esp),
            syn::Expr::Paren(x) => self.baja_expr(&x.expr, esp),
            syn::Expr::Path(x) => self.ex_path(x, esp),
            syn::Expr::Range(x) => Err(self.err_en(
                "C0003",
                "rangos como valor no caben (solo en `para` y rebanadas)".into(),
                x,
            )
            .ayuda("itera el rango con `para i en a..b` o materialízalo en una `Lista`")),
            syn::Expr::RawAddr(x) => self.ex_rawaddr(x),
            syn::Expr::Reference(x) => self.ex_ref(x, esp),
            syn::Expr::Repeat(x) => self.ex_repite(x, esp),
            syn::Expr::Return(x) => {
                self.baja_return(x)?;
                self.divergente(esp)
            }
            syn::Expr::Struct(x) => self.ex_struct(x, esp),
            syn::Expr::Try(x) => self.ex_try(x, esp),
            syn::Expr::TryBlock(x) => Err(self.err_en("C0003", "bloques `try` no caben".into(), x)),
            syn::Expr::Tuple(x) => self.ex_tuple(x, esp),
            syn::Expr::Unary(x) => self.ex_unary(x, esp),
            syn::Expr::Unsafe(x) => self.ex_bloque(&x.block, esp),
            syn::Expr::Verbatim(x) => Err(self.err_en("C0003", "expresión no reconocida".into(), x)),
            syn::Expr::While(x) => self.ex_while(x),
            syn::Expr::Yield(x) => Err(self.err_en("C0003", "generadores no caben".into(), x)),
            _ => Err(self.err_en("C0003", "expresión no soportada".into(), e)),
        }
    }

    /// Valor divergente (`return`/`break`/`continue` en posición de valor):
    /// código muerto que compila (cero del tipo esperado).
    fn divergente(&self, esp: Option<&CType>) -> Result<ExVal, CError> {
        match esp {
            Some(t) => Ok(ExVal::puro(t.cero()?, CType::Infer)),
            None => Ok(ExVal::puro("0".into(), CType::Infer)),
        }
    }

    // ==================================================================
    // Consumo y coerciones
    // ==================================================================

    /// Convierte un valor bajado a código C final.
    ///
    /// - Variable movible → marca y pasa el nombre.
    /// - Prvalue → directo (+ marca si es temporal).
    /// - Lugar Copy → se lee directo.
    /// - Lugar dueño → se CLONA (no hay movimientos parciales).
    pub(crate) fn consume(
        &mut self,
        v: ExVal,
        esp: Option<&CType>,
    ) -> Result<String, CError> {
        if v.arreglo.is_some() {
            let (tmp, t) = self.aloja_arreglo(v)?;
            let v2 = ExVal::lugar(tmp.clone(), t, tmp);
            return self.consume(v2, esp);
        }
        let esp = match esp {
            Some(CType::Infer) | None => None,
            s => s,
        };
        if let Some(m) = v.movible {
            self.marca_movida(&m);
            self.activa_bandera(&m);
            return self.ajusta_esperado(v.c, &v.tipo, esp);
        }
        if v.lugar.is_none() {
            let c = v.c;
            self.marca_movida_si_temp_pub(&c);
            return self.ajusta_esperado(c, &v.tipo, esp);
        }
        if !self.necesita_drop(&v.tipo) {
            return self.ajusta_esperado(v.c, &v.tipo, esp);
        }
        let t = self.clona_a_temp(v.c, v.lugar, &v.tipo)?;
        self.ajusta_esperado(t, &v.tipo, esp)
    }

    /// Aplica el tipo esperado (solo coerciones reales de Rust).
    fn ajusta_esperado(
        &mut self,
        c: String,
        t: &CType,
        e: Option<&CType>,
    ) -> Result<String, CError> {
        let Some(e) = e else { return Ok(c) };
        if t == e {
            return Ok(c);
        }
        match (t, e) {
            (CType::Infer, _) => return Ok(e.cero()?),
            // `&String` / `&mut String` → `&str`.
            (CType::Ref(_, x), CType::VistaTexto) if matches!(**x, CType::Texto) => {
                Ok(format!("kami_texto_cstr({c})"))
            }
            // `&Vec<T>` / `&mut Vec<T>` → `&[T]`; `&[T; N]` → `&[T]`.
            (CType::Ref(m1, x), CType::Rebana(y, m2)) => {
                if !m1 && *m2 {
                    return Err(CError::nuevo(
                        "C0005",
                        "no puedes pedir `&mut [...]` desde `&`".to_string(),
                    )
                    .con_archivo(self.archivo.clone()));
                }
                match &**x {
                    CType::Vec(z) if z.as_ref() == y.as_ref() => {
                        let m = self.registra_mono(e)?;
                        Ok(format!("({m}){{ ({c})->datos, ({c})->largo }}"))
                    }
                    CType::Arreglo(z, l) if z.as_ref() == y.as_ref() => {
                        let m = self.registra_mono(e)?;
                        let es = z.deletrea()?;
                        Ok(format!("({m}){{ ({es}*)({c}), {} }}", l.deletrea()))
                    }
                    _ => Err(CError::nuevo(
                        "C0005",
                        format!("no sé ver `{}` como `{}`", self.muestra(t), self.muestra(e)),
                    )
                    .con_archivo(self.archivo.clone())),
                }
            }
            _ => Err(CError::nuevo(
                "C0005",
                format!("se esperaba `{}`, llegó `{}`", self.muestra(e), self.muestra(t)),
            )
            .con_archivo(self.archivo.clone())
            .ayuda(format!("cerca de: {}", recorta_c(&c)))),
        }
    }

    /// `&lugar` (o temporal direccionado si es prvalue).
    pub(crate) fn direcciona_pub(
        &mut self,
        c: String,
        lugar: Option<String>,
        tipo: &CType,
    ) -> Result<String, CError> {
        if let Some(l) = lugar {
            Ok(format!("&({l})"))
        } else {
            let t = self.clona_a_temp(c, None, tipo)?;
            Ok(format!("&{t}"))
        }
    }

    /// Registra un mono y devuelve su nombre C.
    fn registra_mono(&mut self, t: &CType) -> Result<String, CError> {
        self.registra_uso_pub(t)?;
        t.mangle().map_err(|m| CError::nuevo("C0002", m).con_archivo(self.archivo.clone()))
    }

    /// Nombre corto de un tipo para mensajes (nunca falla).
    pub(crate) fn muestra(&self, t: &CType) -> String {
        t.deletrea().unwrap_or_else(|_| format!("{t:?}"))
    }

    // ==================================================================
    // Literales
    // ==================================================================

    fn ex_lit(&mut self, x: &syn::ExprLit, esp: Option<&CType>) -> Result<ExVal, CError> {
        self.baja_lit(&x.lit, esp)
    }

    fn baja_lit(&mut self, lit: &syn::Lit, esp: Option<&CType>) -> Result<ExVal, CError> {
        match lit {
            syn::Lit::Int(i) => self.baja_lit_int(i, esp),
            syn::Lit::Float(f) => self.baja_lit_float(f, esp),
            syn::Lit::Bool(b) => {
                if let Some(e) = esp {
                    if !matches!(e, CType::Bool) {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, llegó booleano", self.muestra(e)),
                            lit,
                        ));
                    }
                }
                Ok(ExVal::puro(if b.value { "true" } else { "false" }.into(), CType::Bool))
            }
            syn::Lit::Char(c) => {
                if let Some(e) = esp {
                    if !matches!(e, CType::Char) {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, llegó `char`", self.muestra(e)),
                            lit,
                        ));
                    }
                }
                let ch = c.value();
                let l = if (ch as u32) < 128 {
                    format!("'{}'", tipos::escapa_char_c(ch))
                } else {
                    format!("0x{:X}", ch as u32)
                };
                Ok(ExVal::puro(format!("((KamiChar){l})"), CType::Char))
            }
            syn::Lit::Str(s) => {
                if let Some(e) = esp {
                    if !matches!(e, CType::VistaTexto) {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, llegó `&str`", self.muestra(e)),
                            lit,
                        )
                        .ayuda("para `Texto` usa `.to_string()` o `String::from(...)`"));
                    }
                }
                let v = s.value();
                Ok(ExVal::puro(format!("\"{}\"", tipos::escapa_cadena_c(&v)), CType::VistaTexto))
            }
            syn::Lit::Byte(b) => {
                if let Some(e) = esp {
                    if !matches!(e, CType::U8) {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, llegó byte `u8`", self.muestra(e)),
                            lit,
                        ));
                    }
                }
                Ok(ExVal::puro(format!("{}", b.value()), CType::U8))
            }
            syn::Lit::ByteStr(b) => {
                // `b"..."` → `&[u8; N]` estático.
                let bytes = b.value();
                let n = bytes.len();
                let nm = format!("__bs{}", self.etiqueta_n);
                self.etiqueta_n += 1;
                let cs: Vec<String> = bytes.iter().map(|x| x.to_string()).collect();
                let decl_n = if n == 0 { 1 } else { n };
                self.consts_out.push(format!(
                    "static const uint8_t {nm}[{decl_n}] = {{{}}};",
                    if n == 0 { "0".into() } else { cs.join(", ") }
                ));
                let t = CType::Ref(
                    false,
                    Box::new(CType::Arreglo(Box::new(CType::U8), LargoArreglo::Lit(n))),
                );
                Ok(ExVal::puro(format!("(&{nm})"), t))
            }
            syn::Lit::CStr(s) => {
                if let Some(e) = esp {
                    if !matches!(e, CType::VistaTexto) {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, llegó `&str`", self.muestra(e)),
                            lit,
                        ));
                    }
                }
                let tok = s.token().to_string();
                let inner = tok.trim_start_matches('c').to_string();
                Ok(ExVal::puro(inner, CType::VistaTexto))
            }
            _ => Err(self.err_en("C0003", "literal no soportado".into(), lit)),
        }
    }

    fn baja_lit_int(
        &mut self,
        i: &syn::LitInt,
        esp: Option<&CType>,
    ) -> Result<ExVal, CError> {
        let base = i
            .base10_parse::<i128>()
            .map_err(|_| self.err_en("C0005", "entero fuera de rango".into(), i))?;
        let tipo_suf: Option<CType> = match i.suffix() {
            "" => None,
            "i8" => Some(CType::I8),
            "i16" => Some(CType::I16),
            "i32" => Some(CType::I32),
            "i64" => Some(CType::I64),
            "isize" => Some(CType::Isize),
            "u8" => Some(CType::U8),
            "u16" => Some(CType::U16),
            "u32" => Some(CType::U32),
            "u64" => Some(CType::U64),
            "usize" => Some(CType::Usize),
            "i128" | "u128" => {
                return Err(self.err_en("C0003", "128 bits no caben en C".into(), i)
                    .ayuda("usa `i64`/`u64` o una biblioteca de bigints"))
            }
            otro => return Err(self.err_en("C0005", format!("sufijo `{otro}` desconocido"), i)),
        };
        let t = if let Some(ts) = tipo_suf {
            if let Some(e) = esp {
                if &ts != e {
                    return Err(self.err_en(
                        "C0005",
                        format!("sufijo `{}` choca con lo esperado `{}`", i.suffix(), self.muestra(e)),
                        i,
                    ));
                }
            }
            ts
        } else if let Some(e) = esp {
            match e {
                t if t.es_entero() && !matches!(t, CType::Char) => e.clone(),
                _ => {
                    return Err(self.err_en(
                        "C0005",
                        format!("se esperaba `{}`, llegó entero", self.muestra(e)),
                        i,
                    ))
                }
            }
        } else {
            CType::I32
        };
        let (min, max): (i128, i128) = match t {
            CType::I8 => (-128, 127),
            CType::I16 => (-32768, 32767),
            CType::I32 => (i32::MIN as i128, i32::MAX as i128),
            CType::I64 | CType::Isize => (i64::MIN as i128, i64::MAX as i128),
            CType::U8 => (0, 255),
            CType::U16 => (0, 65535),
            CType::U32 => (0, u32::MAX as i128),
            CType::U64 | CType::Usize => (0, u64::MAX as i128),
            _ => unreachable!(),
        };
        if base < min || base > max {
            return Err(self.err_en(
                "C0005",
                format!("`{base}` no cabe en `{}`", self.muestra(&t)),
                i,
            ));
        }
        let c = match t {
            CType::I64 => format!("((int64_t){base})"),
            CType::U64 => format!("((uint64_t){base})"),
            CType::Isize => format!("((ptrdiff_t){base})"),
            CType::Usize => format!("((size_t){base})"),
            _ => format!("{base}"),
        };
        Ok(ExVal::puro(c, t))
    }

    fn baja_lit_float(
        &mut self,
        f: &syn::LitFloat,
        esp: Option<&CType>,
    ) -> Result<ExVal, CError> {
        let t = match f.suffix() {
            "" => match esp {
                Some(CType::F32) => CType::F32,
                Some(CType::F64) => CType::F64,
                Some(e) => {
                    return Err(self.err_en(
                        "C0005",
                        format!("se esperaba `{}`, llegó flotante", self.muestra(e)),
                        f,
                    ))
                }
                None => CType::F64,
            },
            "f32" => CType::F32,
            "f64" => CType::F64,
            otro => return Err(self.err_en("C0005", format!("sufijo `{otro}` desconocido"), f)),
        };
        if let Some(e) = esp {
            if !matches!(f.suffix(), "") && &t != e {
                return Err(self.err_en(
                    "C0005",
                    format!("sufijo `{}` choca con lo esperado `{}`", f.suffix(), self.muestra(e)),
                    f,
                ));
            }
        }
        f.base10_parse::<f64>()
            .map_err(|_| self.err_en("C0005", "flotante inválido".into(), f))?;
        let mut d: String = f.base10_digits().replace('_', "");
        if !d.contains(&['.', 'e', 'E'][..]) {
            d.push_str(".0");
        }
        let c = if matches!(t, CType::F32) { format!("{d}f") } else { d };
        Ok(ExVal::puro(c, t))
    }

    // ==================================================================
    // Rutas (variables, consts, variantes, funciones)
    // ==================================================================

    fn ex_path(&mut self, x: &syn::ExprPath, esp: Option<&CType>) -> Result<ExVal, CError> {
        if x.qself.is_some() {
            return Err(self.err_en("C0003", "rutas `<T as R>::x` como valor no caben".into(), x)
                .ayuda("nombra el tipo o método sin calificador"));
        }
        let segs: Vec<String> =
            x.path.segments.iter().map(|s| tipos::desnuda(&s.ident)).collect();
        if segs.len() == 1 {
            match segs[0].as_str() {
                "true" | "verdadero" => return self.ex_bool(true, esp, x),
                "false" | "falso" => return self.ex_bool(false, esp, x),
                _ => {}
            }
            return self.ex_path_simple(&segs[0], x, esp);
        }
        self.ex_path_largo(&segs, x, esp)
    }

    fn ex_bool<S: Spanned>(&self, v: bool, esp: Option<&CType>, sp: S) -> Result<ExVal, CError> {
        if let Some(e) = esp {
            if !matches!(e, CType::Bool) {
                return Err(self.err_en(
                    "C0005",
                    format!("se esperaba `{}`, llegó booleano", self.muestra(e)),
                    sp,
                ));
            }
        }
        Ok(ExVal::puro(if v { "true" } else { "false" }.into(), CType::Bool))
    }

    fn ex_path_simple(
        &mut self,
        n: &str,
        x: &syn::ExprPath,
        esp: Option<&CType>,
    ) -> Result<ExVal, CError> {
        // 1. Variable local / parámetro.
        if let Some((_, v)) = self.busca(n) {
            if v.movida {
                return Err(self.err_en("C0008", format!("`{n}` se movió antes"), x)
                    .ayuda("clónalo antes de moverlo (`.clone()`) o reordena el código"));
            }
            let t = v.tipo.clone();
            let nc = v.nombre_c.clone();
            if v.duena && self.necesita_drop(&t) {
                return Ok(ExVal::movible(nc, t, n.to_string()));
            }
            return Ok(ExVal::lugar(nc.clone(), t, nc));
        }
        // 2. `Ninguno` / constructores pelados.
        if n == "None" {
            return self.ex_ninguno(x, esp, 0);
        }
        if n == "Some" || n == "Ok" || n == "Err" {
            return Err(self.err_en("C0003", format!("`{n}` sin argumentos no es un valor"), x)
                .ayuda(format!("llámalo: `{n}(valor)`")));
        }
        // 3. Const / estático.
        let raiz = x.path.leading_colon.is_some();
        if let Some(info) = self.busca_const_pub(&[n.to_string()], raiz) {
            return self.ex_const_valor(&info, esp, x);
        }
        // 4. Variante unitaria pelada (vía `use E::*`).
        let cands = self.busca_variante_pelada(n);
        if cands.len() == 1 {
            let (en, v) = &cands[0];
            if matches!(v.campos, super::baja::CamposVariante::Unit) {
                return Ok(ExVal::puro(format!("{en}_{}", v.nombre), CType::Usuario(en.clone())));
            }
            return Err(self.err_en("C0003", format!("`{n}` lleva argumentos"), x)
                .ayuda(format!("constrúyelo: `{en}::{n}(...)`")));
        } else if cands.len() > 1 {
            let donde: Vec<String> = cands.iter().map(|(e, _)| e.clone()).collect();
            return Err(self.err_en(
                "C0003",
                format!("`{n}` es variante de varios: {}", donde.join(", ")),
                x,
            )
            .ayuda("califica con el enum: `MiEnum::Variante`"));
        }
        // 5. Función libre como valor.
        if let Some((nc, f)) = self.resuelve_fn_pub(&[n.to_string()], raiz) {
            let t = CType::FnPtr {
                params: f.params.iter().map(|(_, t)| t.clone()).collect(),
                ret: Box::new(f.ret),
            };
            return Ok(ExVal::puro(nc, t));
        }
        Err(self.err_en("C0003", format!("no conozco `{n}`"), x)
            .ayuda("revisa el nombre; `use` no cambia rutas en este backend"))
    }

    /// `Ninguno` / `Option::None`: el interior sale del turbofish o del esperado.
    fn ex_ninguno(
        &mut self,
        x: &syn::ExprPath,
        esp: Option<&CType>,
        seg_tf: usize,
    ) -> Result<ExVal, CError> {
        let inner_tf = self.lee_turbofish(&x.path.segments[seg_tf])?;
        let inner_esp = match esp {
            Some(CType::Opcion(inner)) => Some(inner.as_ref().clone()),
            Some(e) => {
                return Err(self.err_en(
                    "C0005",
                    format!("se esperaba `{}`, llegó `Ninguno`", self.muestra(e)),
                    x,
                ))
            }
            None => None,
        };
        let inner = match (inner_tf, inner_esp) {
            (Some(a), Some(b)) if a != b => {
                return Err(self.err_en(
                    "C0005",
                    format!("`Ninguno` choca: `{}` vs `{}`", self.muestra(&a), self.muestra(&b)),
                    x,
                ))
            }
            (Some(a), _) => a,
            (_, Some(b)) => b,
            (None, None) => {
                return Err(self.err_en("C0005", "`Ninguno` necesita tipo conocido".into(), x)
                    .ayuda("anota: `sea x: Opcion<i32> = Ninguno;`"))
            }
        };
        let t = CType::Opcion(Box::new(inner));
        let m = self.registra_mono(&t)?;
        Ok(ExVal::puro(format!("({m}){{0}}"), t))
    }

    /// Lee `<T>` de un segmento (`None::<i32>`). `None` si no hay.
    fn lee_turbofish(&mut self, seg: &syn::PathSegment) -> Result<Option<CType>, CError> {
        let syn::PathArguments::AngleBracketed(ab) = &seg.arguments else { return Ok(None) };
        let mut tipos = Vec::new();
        for a in &ab.args {
            match a {
                syn::GenericArgument::Type(ty) => tipos.push(self.baja_tipo(ty)?),
                syn::GenericArgument::Lifetime(_) => {}
                _ => {
                    return Err(CError::nuevo("C0003", "genérico const no cabe".to_string())
                        .con_archivo(self.archivo.clone()))
                }
            }
        }
        if tipos.len() == 1 {
            Ok(Some(tipos.into_iter().next().unwrap()))
        } else {
            Ok(None)
        }
    }

    /// Const/estático por ruta (módulos de adentro hacia afuera).
    pub(crate) fn busca_const_pub(
        &self,
        ruta: &[String],
        desde_raiz: bool,
    ) -> Option<super::baja::InfoConst> {
        if desde_raiz {
            return self.consts.get(&ruta.join("__")).cloned();
        }
        for k in (0..=self.mods.len()).rev() {
            let mut v: Vec<String> = self.mods[..k].to_vec();
            v.extend(ruta.iter().cloned());
            if let Some(i) = self.consts.get(&v.join("__")) {
                return Some(i.clone());
            }
        }
        None
    }

    fn ex_const_valor(
        &self,
        info: &super::baja::InfoConst,
        esp: Option<&CType>,
        x: &syn::ExprPath,
    ) -> Result<ExVal, CError> {
        if let Some(e) = esp {
            if e != &info.tipo {
                return Err(self.err_en(
                    "C0005",
                    format!("se esperaba `{}`, `{}` es `{}`", self.muestra(e), info.nombre_c, self.muestra(&info.tipo)),
                    x,
                ));
            }
        }
        if let Some(d) = &info.define {
            return Ok(ExVal::puro(format!("({d})"), info.tipo.clone()));
        }
        Ok(ExVal::lugar(info.nombre_c.clone(), info.tipo.clone(), info.nombre_c.clone()))
    }

    /// Variantes llamadas `n` en todos los enums (para rutas peladas).
    pub(crate) fn busca_variante_pelada(&self, n: &str) -> Vec<(String, super::baja::InfoVariante)> {
        let mut out = Vec::new();
        for (en, info) in &self.enums {
            for v in &info.variantes {
                if v.nombre == n {
                    out.push((en.clone(), v.clone()));
                }
            }
        }
        out
    }

    /// Función libre por ruta. Devuelve (nombre_c, firma).
    fn resuelve_fn_pub(
        &self,
        ruta: &[String],
        desde_raiz: bool,
    ) -> Option<(String, super::baja::Firma)> {
        if desde_raiz {
            let c = ruta.join("__");
            return self.fns.get(&c).cloned().map(|f| (c, f));
        }
        for k in (0..=self.mods.len()).rev() {
            let mut v: Vec<String> = self.mods[..k].to_vec();
            v.extend(ruta.iter().cloned());
            let c = v.join("__");
            if let Some(f) = self.fns.get(&c) {
                return Some((c, f.clone()));
            }
        }
        None
    }

    fn ex_path_largo(
        &mut self,
        segs: &[String],
        x: &syn::ExprPath,
        esp: Option<&CType>,
    ) -> Result<ExVal, CError> {
        let raiz = x.path.leading_colon.is_some();
        if segs.len() == 2 && (segs[0] == "Option" || segs[0] == "Opcion") && segs[1] == "None" {
            return self.ex_ninguno(x, esp, 0);
        }
        if segs.len() == 2
            && matches!(segs[0].as_str(), "Option" | "Result" | "Opcion" | "Resultado")
            && matches!(segs[1].as_str(), "Some" | "Ok" | "Err")
        {
            return Err(self.err_en(
                "C0003",
                format!("`{}::{}` sin argumentos no es un valor", segs[0], segs[1]),
                x,
            )
            .ayuda("llámalo con su valor"));
        }
        let ruta = self.normaliza_ruta(segs);
        // Const / estático / asociada.
        if let Some(info) = self.busca_const_pub(&ruta, raiz) {
            return self.ex_const_valor(&info, esp, x);
        }
        // `Enum::Variante` unitaria o `T::metodo` como puntero.
        if ruta.len() >= 2 {
            let (pref, ult) = (&ruta[..ruta.len() - 1], &ruta[ruta.len() - 1]);
            if let Some(en) = self.resuelve_tipo_pub(pref, raiz) {
                if let Some(info) = self.enums.get(&en).cloned() {
                    if let Some(v) = info.variantes.iter().find(|v| &v.nombre == ult) {
                        if matches!(v.campos, super::baja::CamposVariante::Unit) {
                            return Ok(ExVal::puro(
                                format!("{en}_{}", v.nombre),
                                CType::Usuario(en.clone()),
                            ));
                        }
                        return Err(self.err_en("C0003", format!("`{ult}` lleva argumentos"), x)
                            .ayuda(format!("constrúyelo: `{en}::{ult}(...)`")));
                    }
                }
                if let Some(v) = self.ex_metodo_valor(&en, ult, x)? {
                    return Ok(v);
                }
            }
        }
        // `mod::funcion` como valor.
        if let Some((nc, f)) = self.resuelve_fn_pub(&ruta, raiz) {
            let t = CType::FnPtr {
                params: f.params.iter().map(|(_, t)| t.clone()).collect(),
                ret: Box::new(f.ret),
            };
            return Ok(ExVal::puro(nc, t));
        }
        // Consts de std (`i32::MAX`, `f64::consts::PI`...).
        if let Some(v) = self.ex_std_const(segs, esp, x)? {
            return Ok(v);
        }
        if matches!(segs[0].as_str(), "Vec" | "Lista" | "String" | "Texto" | "Option" | "Result") {
            return Err(self.err_en(
                "C0003",
                format!("`{}` de std es inline: llámalo, no lo nombres", segs.join("::")),
                x,
            ));
        }
        Err(self.err_en("C0003", format!("no reconozco `{}`", segs.join("::")), x)
            .ayuda("revisa el nombre y la ruta (módulos con `::`)"))
    }

    /// Resuelve prefijos `crate`/`self`/`super` contra el módulo actual.
    fn normaliza_ruta(&self, segs: &[String]) -> Vec<String> {
        let mut base = self.mods.clone();
        let mut i = 0;
        if !segs.is_empty() && segs[0] == "crate" {
            base.clear();
            i = 1;
        } else if !segs.is_empty() && segs[0] == "self" {
            i = 1;
        } else {
            while i < segs.len() && segs[i] == "super" {
                base.pop();
                i += 1;
            }
        }
        base.extend(segs[i..].iter().cloned());
        base
    }

    /// Nombre C de un struct/enum por ruta (módulos de adentro hacia afuera).
    pub(crate) fn resuelve_tipo_pub(&self, ruta: &[String], raiz: bool) -> Option<String> {
        if raiz {
            let c = ruta.join("__");
            return (self.structs.contains_key(&c) || self.enums.contains_key(&c)).then(|| c);
        }
        for k in (0..=self.mods.len()).rev() {
            let mut v: Vec<String> = self.mods[..k].to_vec();
            v.extend(ruta.iter().cloned());
            let c = v.join("__");
            if self.structs.contains_key(&c) || self.enums.contains_key(&c) {
                return Some(c);
            }
        }
        None
    }

    /// `T::metodo` como puntero a función (inherente o de un único rasgo).
    fn ex_metodo_valor(
        &self,
        tipo_n: &str,
        metodo: &str,
        x: &syn::ExprPath,
    ) -> Result<Option<ExVal>, CError> {
        if let Some(sig) = self.metodos.get(&(tipo_n.to_string(), metodo.to_string())) {
            return Ok(Some(self.fn_ptr_metodo(sig, tipo_n)));
        }
        let mut cands: Vec<(String, super::baja::SigMetodo)> = Vec::new();
        for ((r, t), mapa) in &self.impl_rasgos {
            if t == tipo_n {
                if let Some(sig) = mapa.get(metodo) {
                    cands.push((r.clone(), sig.clone()));
                }
            }
        }
        if cands.len() == 1 {
            let (r, sig) = &cands[0];
            let mut sig2 = sig.clone();
            sig2.params = sig2
                .params
                .iter()
                .map(|(n, t)| (n.clone(), super::baja::sustituye_self(t, r, tipo_n)))
                .collect();
            sig2.ret = super::baja::sustituye_self(&sig2.ret, r, tipo_n);
            return Ok(Some(self.fn_ptr_metodo(&sig2, tipo_n)));
        } else if cands.len() > 1 {
            return Err(self.err_en(
                "C0007",
                format!("`{metodo}` está en varios rasgos para `{tipo_n}`"),
                x,
            )
            .ayuda("llámalo sobre un valor (`x.metodo()`) para desambiguar"));
        }
        Ok(None)
    }

    /// Puntero a función de un método (receptor como primer parámetro).
    fn fn_ptr_metodo(&self, sig: &super::baja::SigMetodo, tipo_n: &str) -> ExVal {
        let mut params = Vec::new();
        match sig.receptor {
            Receptor::Valor => params.push(CType::Usuario(tipo_n.to_string())),
            Receptor::Ref => {
                params.push(CType::Ref(false, Box::new(CType::Usuario(tipo_n.to_string()))))
            }
            Receptor::Mut => {
                params.push(CType::Ref(true, Box::new(CType::Usuario(tipo_n.to_string()))))
            }
            Receptor::Ninguno => {}
        }
        params.extend(sig.params.iter().map(|(_, t)| t.clone()));
        ExVal::puro(
            sig.nombre_c.clone(),
            CType::FnPtr { params, ret: Box::new(sig.ret.clone()) },
        )
    }

    /// Comprueba compatibilidad con el tipo esperado (rutas a consts).
    fn espera_tipo(
        &self,
        esp: Option<&CType>,
        t: &CType,
        x: &syn::ExprPath,
    ) -> Result<(), CError> {
        if let Some(e) = esp {
            if e != t {
                return Err(self.err_en(
                    "C0005",
                    format!("se esperaba `{}`, llegó `{}`", self.muestra(e), self.muestra(t)),
                    x,
                ));
            }
        }
        Ok(())
    }

    /// Consts de std: `i32::MAX`, `f32::MAX`, `f64::consts::PI`, `char::MAX`...
    fn ex_std_const(
        &mut self,
        segs: &[String],
        esp: Option<&CType>,
        x: &syn::ExprPath,
    ) -> Result<Option<ExVal>, CError> {
        if segs.len() == 2 {
            let t = match segs[0].as_str() {
                "i8" => Some(CType::I8),
                "i16" => Some(CType::I16),
                "i32" => Some(CType::I32),
                "i64" => Some(CType::I64),
                "isize" => Some(CType::Isize),
                "u8" => Some(CType::U8),
                "u16" => Some(CType::U16),
                "u32" => Some(CType::U32),
                "u64" => Some(CType::U64),
                "usize" => Some(CType::Usize),
                _ => None,
            };
            if let Some(t) = t {
                let pref = match t {
                    CType::I8 => "INT8",
                    CType::I16 => "INT16",
                    CType::I32 => "INT32",
                    CType::I64 => "INT64",
                    CType::Isize => "PTRDIFF",
                    CType::U8 => "UINT8",
                    CType::U16 => "UINT16",
                    CType::U32 => "UINT32",
                    CType::U64 => "UINT64",
                    CType::Usize => "SIZE",
                    _ => unreachable!(),
                };
                let c = match segs[1].as_str() {
                    "MAX" => Some(format!("{pref}_MAX")),
                    "MIN" => Some(if t.es_sin_signo() {
                        "0".into()
                    } else {
                        format!("{pref}_MIN")
                    }),
                    _ => None,
                };
                if let Some(c) = c {
                    self.espera_tipo(esp, &t, x)?;
                    return Ok(Some(ExVal::puro(c, t)));
                }
                return Ok(None);
            }
            if segs[0] == "char" && segs[1] == "MAX" {
                self.espera_tipo(esp, &CType::Char, x)?;
                return Ok(Some(ExVal::puro("((KamiChar)0x10FFFF)".into(), CType::Char)));
            }
            if segs[0] == "f32" || segs[0] == "f64" {
                let es32 = segs[0] == "f32";
                if let Some((c, t)) = self.const_float(segs[1].as_str(), es32) {
                    self.espera_tipo(esp, &t, x)?;
                    return Ok(Some(ExVal::puro(c, t)));
                }
                return Ok(None);
            }
        }
        if segs.len() == 3 && (segs[0] == "f32" || segs[0] == "f64") && segs[1] == "consts" {
            let es32 = segs[0] == "f32";
            let t = if es32 { CType::F32 } else { CType::F64 };
            let suf = if es32 { "f" } else { "" };
            let v: Option<&str> = match segs[2].as_str() {
                "PI" => Some("3.141592653589793"),
                "TAU" => Some("6.283185307179586"),
                "FRAC_PI_2" => Some("1.5707963267948966"),
                "FRAC_PI_3" => Some("1.0471975511965976"),
                "FRAC_PI_4" => Some("0.7853981633974483"),
                "FRAC_PI_6" => Some("0.5235987755982988"),
                "FRAC_PI_8" => Some("0.39269908169872414"),
                "FRAC_1_PI" => Some("0.3183098861837907"),
                "FRAC_2_PI" => Some("0.6366197723675814"),
                "FRAC_2_SQRT_PI" => Some("1.1283791670955126"),
                "SQRT_2" => Some("1.4142135623730951"),
                "FRAC_1_SQRT_2" => Some("0.7071067811865476"),
                "E" => Some("2.718281828459045"),
                "LOG2_E" => Some("1.4426950408889634"),
                "LOG10_E" => Some("0.4342944819032518"),
                "LN_2" => Some("0.6931471805599453"),
                "LN_10" => Some("2.302585092994046"),
                _ => None,
            };
            if let Some(v) = v {
                self.espera_tipo(esp, &t, x)?;
                return Ok(Some(ExVal::puro(format!("{v}{suf}"), t)));
            }
        }
        Ok(None)
    }

    /// Consts asociadas de flotantes (`MAX`, `INFINITY`, `DIGITS`...).
    fn const_float(&mut self, nombre: &str, es32: bool) -> Option<(String, CType)> {
        let t = if es32 { CType::F32 } else { CType::F64 };
        let suf = if es32 { "f" } else { "" };
        match nombre {
            "MAX" => Some((
                if es32 { "3.40282347e+38f".into() } else { "1.7976931348623157e+308".into() },
                t,
            )),
            "MIN" => Some((
                if es32 { "-3.40282347e+38f".into() } else { "-1.7976931348623157e+308".into() },
                t,
            )),
            "MIN_POSITIVE" => Some((
                if es32 { "1.17549435e-38f".into() } else { "2.2250738585072014e-308".into() },
                t,
            )),
            "EPSILON" => Some((
                if es32 { "1.1920929e-7f".into() } else { "2.220446049250313e-16".into() },
                t,
            )),
            "INFINITY" => {
                self.usa_math = true;
                Some((if es32 { "((float)INFINITY)".into() } else { "(INFINITY)".into() }, t))
            }
            "NEG_INFINITY" => {
                self.usa_math = true;
                Some((
                    if es32 { "(-((float)INFINITY))".into() } else { "(-(INFINITY))".into() },
                    t,
                ))
            }
            "NAN" => {
                self.usa_math = true;
                Some((if es32 { "((float)NAN)".into() } else { "(NAN)".into() }, t))
            }
            "DIGITS" => Some((if es32 { "6".into() } else { "15".into() }, CType::U32)),
            "MANTISSA_DIGITS" => {
                Some((if es32 { "24".into() } else { "53".into() }, CType::U32))
            }
            "MIN_EXP" => {
                Some((if es32 { "(-125)".into() } else { "(-1021)".into() }, CType::I32))
            }
            "MAX_EXP" => {
                Some((if es32 { "128".into() } else { "1024".into() }, CType::I32))
            }
            "MIN_10_EXP" => {
                Some((if es32 { "(-37)".into() } else { "(-307)".into() }, CType::I32))
            }
            "MAX_10_EXP" => {
                Some((if es32 { "38".into() } else { "308".into() }, CType::I32))
            }
            "RADIX" => Some(("2".into(), CType::U32)),
            _ => {
                let _ = suf;
                None
            }
        }
    }

    // ==================================================================
    // Binarios, unarios, asignación
    // ==================================================================

    fn ex_binary(&mut self, x: &syn::ExprBinary, esp: Option<&CType>) -> Result<ExVal, CError> {
        match &x.op {
            syn::BinOp::Add(_) | syn::BinOp::Sub(_) | syn::BinOp::Mul(_)
            | syn::BinOp::Div(_) | syn::BinOp::Rem(_) => self.ex_bin_arith(x, esp),
            syn::BinOp::BitAnd(_) | syn::BinOp::BitOr(_) | syn::BinOp::BitXor(_)
            | syn::BinOp::Shl(_) | syn::BinOp::Shr(_) => self.ex_bin_bits(x, esp),
            syn::BinOp::And(_) | syn::BinOp::Or(_) => self.ex_bin_logico(x, esp),
            syn::BinOp::Eq(_) | syn::BinOp::Ne(_) | syn::BinOp::Lt(_)
            | syn::BinOp::Le(_) | syn::BinOp::Gt(_) | syn::BinOp::Ge(_) => self.ex_bin_cmp(x, esp),
            o if matches!(
                o,
                syn::BinOp::AddAssign(_)
                    | syn::BinOp::SubAssign(_)
                    | syn::BinOp::MulAssign(_)
                    | syn::BinOp::DivAssign(_)
                    | syn::BinOp::RemAssign(_)
                    | syn::BinOp::BitAndAssign(_)
                    | syn::BinOp::BitOrAssign(_)
                    | syn::BinOp::BitXorAssign(_)
                    | syn::BinOp::ShlAssign(_)
                    | syn::BinOp::ShrAssign(_)
            ) => self
                .ex_assign_op(x, esp)
                .map(|_| ExVal::puro("0".into(), CType::Vacio)),
            _ => Err(self.err_en("C0003", "operador no soportado".into(), x)),
        }
    }

    /// `+ - * / %` con chequeo de overflow (como Rust-debug).
    fn ex_bin_arith(
        &mut self,
        x: &syn::ExprBinary,
        esp: Option<&CType>,
    ) -> Result<ExVal, CError> {
        let numerico = esp.map(|e| e.es_numero()).unwrap_or(false);
        let e_op = if numerico { esp } else { None };
        let l = self.baja_expr(&x.left, e_op)?;
        let r = self.baja_expr(&x.right, e_op)?;
        // ¿Concatenación? (`String + &str`).
        if matches!(x.op, syn::BinOp::Add(_))
            && (matches!(l.tipo, CType::Texto | CType::VistaTexto)
                || matches!(r.tipo, CType::Texto | CType::VistaTexto))
        {
            return self.ex_concat(l, r, x);
        }
        // Tipo resultado (los divergentes adoptan al hermano).
        let t = match (&l.tipo, &r.tipo) {
            (CType::Infer, CType::Infer) => {
                esp.cloned().filter(|e| e.es_numero()).unwrap_or(CType::I32)
            }
            (CType::Infer, t) | (t, CType::Infer) => t.clone(),
            (a, b) if a == b => a.clone(),
            (a, b) => {
                return Err(self.err_en(
                    "C0005",
                    format!("`{}` y `{}` no operan juntos", self.muestra(a), self.muestra(b)),
                    x,
                ))
            }
        };
        if !t.es_numero() || matches!(t, CType::Char) {
            return Err(self.err_en("C0003", format!("`{}` no es aritmético", self.muestra(&t)), x));
        }
        if let Some(e) = esp {
            if e != &t {
                return Err(self.err_en(
                    "C0005",
                    format!("se esperaba `{}`, la operación da `{}`", self.muestra(e), self.muestra(&t)),
                    x,
                ));
            }
        }
        let cl = self.consume(l, Some(&t))?;
        let cr = self.consume(r, Some(&t))?;
        let (op, nombre) = match x.op {
            syn::BinOp::Add(_) => ("+", "suma"),
            syn::BinOp::Sub(_) => ("-", "resta"),
            syn::BinOp::Mul(_) => ("*", "multiplicación"),
            syn::BinOp::Div(_) => ("/", "división"),
            _ => ("%", "resto"),
        };
        if t.es_flotante() {
            return Ok(ExVal::puro(format!("(({cl}) {op} ({cr}))"), t));
        }
        match x.op {
            syn::BinOp::Add(_) | syn::BinOp::Sub(_) | syn::BinOp::Mul(_) => {
                let builtin = match op {
                    "+" => "add",
                    "-" => "sub",
                    _ => "mul",
                };
                let tt = self.temp(t.clone())?;
                self.emite(format!("if (__builtin_{builtin}_overflow(({cl}), ({cr}), &{tt})) {{"));
                self.emite(format!("    KAMI_PANICO(\"desbordamiento en {nombre} {}\");", self.muestra(&t)));
                self.emite("}");
                Ok(ExVal::puro(tt, t))
            }
            _ => {
                // Div/resto: hoist (chequeo + uso evaluarían dos veces).
                let tl = self.temp(t.clone())?;
                self.emite(format!("{tl} = ({cl});"));
                self.marca_movida_si_temp_pub(&cl);
                let tr = self.temp(t.clone())?;
                self.emite(format!("{tr} = ({cr});"));
                self.marca_movida_si_temp_pub(&cr);
                self.emite("if (({tr}) == 0) { KAMI_PANICO(\"división por cero\"); }"
                    .replace("{tr}", &tr));
                if !t.es_sin_signo() {
                    let minv = match t {
                        CType::I8 => "INT8_MIN",
                        CType::I16 => "INT16_MIN",
                        CType::I32 => "INT32_MIN",
                        CType::I64 => "INT64_MIN",
                        CType::Isize => "PTRDIFF_MIN",
                        _ => unreachable!(),
                    };
                    self.emite(format!(
                        "if (({tl}) == ({minv}) && ({tr}) == -1) {{ KAMI_PANICO(\"desbordamiento en {nombre}\"); }}"
                    ));
                }
                Ok(ExVal::puro(format!("(({tl}) {op} ({tr}))"), t))
            }
        }
    }

    /// Entero de verdad (`char` y flotantes no valen para bits).
    fn es_ent(t: &CType) -> bool {
        t.es_numero() && !t.es_flotante() && !matches!(t, CType::Char)
    }

    /// `& | ^ << >>` (chequeo del corrimiento como Rust).
    fn ex_bin_bits(&mut self, x: &syn::ExprBinary, esp: Option<&CType>) -> Result<ExVal, CError> {
        let es_corr = matches!(x.op, syn::BinOp::Shl(_) | syn::BinOp::Shr(_));
        let lit_ok = esp.map(|e| e.es_numero() || *e == CType::Bool).unwrap_or(false);
        let e_l = if lit_ok && (esp.map(|e| e.es_numero()).unwrap_or(false) || !es_corr) {
            esp
        } else {
            None
        };
        let l = self.baja_expr(&x.left, e_l)?;
        let r = self.baja_expr(&x.right, if es_corr { None } else { e_l })?;
        let t = match (&l.tipo, &r.tipo) {
            (CType::Infer, CType::Infer) => {
                esp.cloned().filter(|e| e.es_numero() || *e == CType::Bool).unwrap_or(CType::I32)
            }
            (CType::Infer, t) | (t, CType::Infer) => t.clone(),
            (a, b) if a == b => a.clone(),
            (a, b) if es_corr && Self::es_ent(a) && Self::es_ent(b) => a.clone(),
            (a, b) => {
                return Err(self.err_en(
                    "C0005",
                    format!("`{}` y `{}` no operan juntos", self.muestra(a), self.muestra(b)),
                    x,
                ))
            }
        };
        if es_corr {
            if !Self::es_ent(&t) {
                return Err(self.err_en(
                    "C0003",
                    format!("el corrimiento necesita enteros, no `{}`", self.muestra(&t)),
                    x,
                ));
            }
        } else if !(Self::es_ent(&t) || t == CType::Bool) {
            return Err(self.err_en(
                "C0003",
                format!("bits necesita enteros o `bool`, no `{}`", self.muestra(&t)),
                x,
            ));
        }
        if let Some(e) = esp {
            if e != &t {
                return Err(self.err_en(
                    "C0005",
                    format!("se esperaba `{}`, la operación da `{}`", self.muestra(e), self.muestra(&t)),
                    x,
                ));
            }
        }
        let cl = self.consume(l, Some(&t))?;
        let trt = r.tipo.clone();
        let cr = self.consume(r, if es_corr { Some(&trt) } else { Some(&t) })?;
        if !es_corr {
            let op = match x.op {
                syn::BinOp::BitAnd(_) => "&",
                syn::BinOp::BitOr(_) => "|",
                _ => "^",
            };
            return Ok(ExVal::puro(format!("(({cl}) {op} ({cr}))"), t));
        }
        // Corrimientos: hoist + chequeo del monto.
        let tl = self.temp(t.clone())?;
        self.emite(format!("{tl} = ({cl});"));
        self.marca_movida_si_temp_pub(&cl);
        let trt2 = if trt == CType::Infer { t.clone() } else { trt.clone() };
        let tr = self.temp(trt2.clone())?;
        self.emite(format!("{tr} = ({cr});"));
        self.marca_movida_si_temp_pub(&cr);
        let bits = match t {
            CType::I8 | CType::U8 => 8,
            CType::I16 | CType::U16 => 16,
            CType::I32 | CType::U32 => 32,
            CType::I64 | CType::U64 => 64,
            _ => (std::mem::size_of::<usize>() * 8) as i32,
        };
        if trt2.es_sin_signo() {
            self.emite(format!("if (({tr}) >= {bits}) {{ KAMI_PANICO(\"corrimiento fuera de rango\"); }}"));
        } else {
            self.emite(format!(
                "if (({tr}) < 0 || ({tr}) >= {bits}) {{ KAMI_PANICO(\"corrimiento fuera de rango\"); }}"
            ));
        }
        if matches!(x.op, syn::BinOp::Shl(_)) {
            if t.es_sin_signo() {
                Ok(ExVal::puro(format!("(({tl}) << ({tr}))"), t))
            } else {
                let spell = t.deletrea()?;
                let uspell = match t {
                    CType::I8 => "uint8_t",
                    CType::I16 => "uint16_t",
                    CType::I32 => "uint32_t",
                    CType::I64 => "uint64_t",
                    _ => "uintptr_t",
                };
                Ok(ExVal::puro(format!("(({spell})(({uspell})({tl}) << ({tr})))"), t))
            }
        } else {
            Ok(ExVal::puro(format!("(({tl}) >> ({tr}))"), t))
        }
    }

    /// `&& ||` (cortocircuito preservado).
    fn ex_bin_logico(
        &mut self,
        x: &syn::ExprBinary,
        esp: Option<&CType>,
    ) -> Result<ExVal, CError> {
        if let Some(e) = esp {
            if *e != CType::Bool {
                return Err(self.err_en(
                    "C0005",
                    format!("se esperaba `{}`, `&&`/`||` dan `bool`", self.muestra(e)),
                    x,
                ));
            }
        }
        let l = self.baja_expr(&x.left, Some(&CType::Bool))?;
        if l.tipo != CType::Bool && l.tipo != CType::Infer {
            return Err(self.err_en("C0005", format!("`{}` no es `bool`", self.muestra(&l.tipo)), &x.left));
        }
        let r = self.baja_expr(&x.right, Some(&CType::Bool))?;
        if r.tipo != CType::Bool && r.tipo != CType::Infer {
            return Err(self.err_en("C0005", format!("`{}` no es `bool`", self.muestra(&r.tipo)), &x.right));
        }
        let cl = self.consume(l, Some(&CType::Bool))?;
        let cr = self.consume(r, Some(&CType::Bool))?;
        let op = if matches!(x.op, syn::BinOp::And(_)) { "&&" } else { "||" };
        Ok(ExVal::puro(format!("(({cl}) {op} ({cr}))"), CType::Bool))
    }

    /// `== != < <= > >=`.
    fn ex_bin_cmp(&mut self, x: &syn::ExprBinary, esp: Option<&CType>) -> Result<ExVal, CError> {
        if let Some(e) = esp {
            if *e != CType::Bool {
                return Err(self.err_en(
                    "C0005",
                    format!("se esperaba `{}`, comparar da `bool`", self.muestra(e)),
                    x,
                ));
            }
        }
        // Si la izquierda es literal, baja la derecha primero para guiar (`5 == x`).
        let (l, r) = if matches!(&*x.left, syn::Expr::Lit(_)) {
            let r = self.baja_expr(&x.right, None)?;
            let g = Self::guia_cmp(&r.tipo);
            let l = self.baja_expr(&x.left, g)?;
            (l, r)
        } else {
            let l = self.baja_expr(&x.left, None)?;
            let g = Self::guia_cmp(&l.tipo);
            let r = self.baja_expr(&x.right, g)?;
            (l, r)
        };
        if matches!(x.op, syn::BinOp::Eq(_) | syn::BinOp::Ne(_)) {
            self.emite_eq(l, r, x)
        } else {
            self.emite_ord(l, r, x)
        }
    }

    /// Guía para el hermano en comparaciones (los literales adoptan el tipo).
    fn guia_cmp(t: &CType) -> Option<&CType> {
        if t.es_numero() || matches!(t, CType::Bool | CType::Char) {
            Some(t)
        } else {
            None
        }
    }

    /// `==` / `!=` (toman prestado, no mueven).
    fn emite_eq(&mut self, l: ExVal, r: ExVal, x: &syn::ExprBinary) -> Result<ExVal, CError> {
        let neg = matches!(x.op, syn::BinOp::Ne(_));
        let op = if neg { "!=" } else { "==" };
        let es_texto = |t: &CType| matches!(t, CType::Texto | CType::VistaTexto);
        // Dueño/vista mezclados: `strcmp` directo (la vista no admite `&KamiTexto`).
        if es_texto(&l.tipo) && es_texto(&r.tipo) && l.tipo != r.tipo {
            let (po, cv) = if l.tipo == CType::Texto {
                let lo = l.lugar.clone();
                let po = self.direcciona_pub(l.c, lo, &CType::Texto)?;
                (po, r.c)
            } else {
                let lo = r.lugar.clone();
                let po = self.direcciona_pub(r.c, lo, &CType::Texto)?;
                (po, l.c)
            };
            return Ok(ExVal::puro(
                format!("(strcmp(kami_texto_cstr({po}), ({cv})) {op} 0)"),
                CType::Bool,
            ));
        }
        // Punteros crudos: compara direcciones (Rust), no objetivos.
        if matches!(l.tipo, CType::Ptr(_, _)) || matches!(r.tipo, CType::Ptr(_, _)) {
            if l.tipo != r.tipo {
                return Err(self.err_en(
                    "C0005",
                    format!("`{}` y `{}` no comparables", self.muestra(&l.tipo), self.muestra(&r.tipo)),
                    x,
                ));
            }
            return Ok(ExVal::puro(format!("(({}) {op} ({}))", l.c, r.c), CType::Bool));
        }
        // `() == ()`.
        if matches!(l.tipo, CType::Vacio) && matches!(r.tipo, CType::Vacio) {
            return Ok(ExVal::puro(if neg { "0".into() } else { "1".into() }, CType::Bool));
        }
        // Tipo común (los divergentes adoptan al hermano).
        let t = match (&l.tipo, &r.tipo) {
            (CType::Infer, CType::Infer) => CType::Bool,
            (CType::Infer, t) | (t, CType::Infer) => t.clone(),
            (a, b) if a == b => a.clone(),
            (a, b) => {
                return Err(self.err_en(
                    "C0005",
                    format!("`{}` y `{}` no comparables", self.muestra(a), self.muestra(b)),
                    x,
                ))
            }
        };
        let la = l.lugar.clone();
        let lb = r.lugar.clone();
        let e = self.iguales_val(l.c, la, r.c, lb, &t).map_err(|e| self.con_archivo(e))?;
        Ok(ExVal::puro(if neg { format!("(!({e}))") } else { e }, CType::Bool))
    }

    /// `< <= > >=`.
    fn emite_ord(&mut self, l: ExVal, r: ExVal, x: &syn::ExprBinary) -> Result<ExVal, CError> {
        let op = match x.op {
            syn::BinOp::Lt(_) => "<",
            syn::BinOp::Le(_) => "<=",
            syn::BinOp::Gt(_) => ">",
            _ => ">=",
        };
        let es_texto = |t: &CType| matches!(t, CType::Texto | CType::VistaTexto);
        if es_texto(&l.tipo) && es_texto(&r.tipo) {
            if l.tipo == CType::Texto && r.tipo == CType::Texto {
                let lo = l.lugar.clone();
                let po = self.direcciona_pub(l.c, lo, &CType::Texto)?;
                let lo2 = r.lugar.clone();
                let qo = self.direcciona_pub(r.c, lo2, &CType::Texto)?;
                return Ok(ExVal::puro(
                    format!("(kami_texto_compara({po}, {qo}) {op} 0)"),
                    CType::Bool,
                ));
            }
            let ca = if l.tipo == CType::Texto {
                let lo = l.lugar.clone();
                let po = self.direcciona_pub(l.c, lo, &CType::Texto)?;
                format!("kami_texto_cstr({po})")
            } else {
                l.c
            };
            let cb = if r.tipo == CType::Texto {
                let lo = r.lugar.clone();
                let po = self.direcciona_pub(r.c, lo, &CType::Texto)?;
                format!("kami_texto_cstr({po})")
            } else {
                r.c
            };
            return Ok(ExVal::puro(format!("(strcmp(({ca}), ({cb})) {op} 0)"), CType::Bool));
        }
        if matches!(l.tipo, CType::Ptr(_, _)) || matches!(r.tipo, CType::Ptr(_, _)) {
            if l.tipo != r.tipo {
                return Err(self.err_en(
                    "C0005",
                    format!("`{}` y `{}` no ordenables", self.muestra(&l.tipo), self.muestra(&r.tipo)),
                    x,
                ));
            }
            return Ok(ExVal::puro(format!("(({}) {op} ({}))", l.c, r.c), CType::Bool));
        }
        let t = match (&l.tipo, &r.tipo) {
            (CType::Infer, CType::Infer) => CType::I32,
            (CType::Infer, t) | (t, CType::Infer) => t.clone(),
            (a, b) if a == b => a.clone(),
            (a, b) => {
                return Err(self.err_en(
                    "C0005",
                    format!("`{}` y `{}` no ordenables", self.muestra(a), self.muestra(b)),
                    x,
                ))
            }
        };
        if t == CType::Bool {
            return Err(self.err_en("C0003", "`bool` no se ordena".into(), x));
        }
        if !t.es_numero() || matches!(t, CType::Char) {
            // `char` sí se ordena en Rust; entra por aquí solo si `es_numero` lo excluye.
            if !matches!(t, CType::Char) {
                return Err(self.err_en(
                    "C0003",
                    format!("`{}` no ordenable (compara campos o usa `match`)", self.muestra(&t)),
                    x,
                ));
            }
        }
        Ok(ExVal::puro(format!("(({}) {op} ({}))", l.c, r.c), CType::Bool))
    }

    /// `String + &str` (mueve la izquierda).
    fn ex_concat(&mut self, l: ExVal, r: ExVal, x: &syn::ExprBinary) -> Result<ExVal, CError> {
        if l.tipo != CType::Texto {
            return Err(self.err_en(
                "C0005",
                format!("el `+` de texto mueve un `String` a la izquierda, no `{}`", self.muestra(&l.tipo)),
                &x.left,
            ));
        }
        if r.tipo != CType::VistaTexto {
            return Err(self.err_en(
                "C0005",
                format!("el `+` de texto pide `&str` a la derecha, no `{}`", self.muestra(&r.tipo)),
                &x.right,
            ));
        }
        let cl = self.consume(l, Some(&CType::Texto))?;
        let cr = self.consume(r, Some(&CType::VistaTexto))?;
        Ok(ExVal::puro(format!("kami_texto_mas(({cl}), ({cr}))"), CType::Texto))
    }

    /// `! - *`.
    fn ex_unary(&mut self, x: &syn::ExprUnary, esp: Option<&CType>) -> Result<ExVal, CError> {
        match x.op {
            syn::UnOp::Not(_) => {
                let inner = self.baja_expr(&x.expr, esp)?;
                let mut t = inner.tipo.clone();
                if t == CType::Infer {
                    t = esp.cloned().filter(|e| *e == CType::Bool || Self::es_ent(e)).unwrap_or(CType::I32);
                }
                if t != CType::Bool && !Self::es_ent(&t) {
                    return Err(self.err_en(
                        "C0003",
                        format!("`!` no aplica a `{}`", self.muestra(&t)),
                        x,
                    ));
                }
                if let Some(e) = esp {
                    if e != &t {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `!` da `{}`", self.muestra(e), self.muestra(&t)),
                            x,
                        ));
                    }
                }
                let c = self.consume(inner, Some(&t))?;
                Ok(ExVal::puro(format!("(!({c}))"), t))
            }
            syn::UnOp::Neg(_) => {
                // Dobla literales: `-128i8` vale (el parser separa el signo).
                if let syn::Expr::Lit(lit) = &*x.expr {
                    if let syn::Lit::Int(li) = &lit.lit {
                        return self.ex_neg_lit(li, esp, x);
                    }
                }
                let guia_neg = |e: &CType| {
                    (e.es_numero() && !e.es_sin_signo() && !matches!(e, CType::Char))
                        || e.es_flotante()
                };
                let inner = self.baja_expr(&x.expr, esp.filter(|e| guia_neg(e)))?;
                let mut t = inner.tipo.clone();
                if t == CType::Infer {
                    t = esp.cloned().filter(guia_neg).unwrap_or(CType::I32);
                }
                let negable = (t.es_numero() && !t.es_sin_signo() && !matches!(t, CType::Char))
                    || t.es_flotante();
                if !negable {
                    return Err(self.err_en(
                        "C0003",
                        format!("no se puede negar `{}`", self.muestra(&t)),
                        x,
                    ));
                }
                if let Some(e) = esp {
                    if e != &t {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, negar da `{}`", self.muestra(e), self.muestra(&t)),
                            x,
                        ));
                    }
                }
                let c = self.consume(inner, Some(&t))?;
                if t.es_flotante() {
                    return Ok(ExVal::puro(format!("(-({c}))"), t));
                }
                // Negación chequeada (`-MIN` desborda).
                let tt = self.temp(t.clone())?;
                self.emite(format!("{tt} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let minv = match t {
                    CType::I8 => "INT8_MIN",
                    CType::I16 => "INT16_MIN",
                    CType::I32 => "INT32_MIN",
                    CType::I64 => "INT64_MIN",
                    _ => "PTRDIFF_MIN",
                };
                self.emite(format!("if (({tt}) == ({minv})) {{ KAMI_PANICO(\"desbordamiento en negación\"); }}"));
                Ok(ExVal::puro(format!("(-({tt}))"), t))
            }
            syn::UnOp::Deref(_) => {
                // `*p` como valor: copia (solo `Copy`); asignar es `ex_assign`.
                let inner = self.baja_expr(&x.expr, None)?;
                match inner.tipo.clone() {
                    CType::Ref(_, p) | CType::Caja(p) => {
                        if self.necesita_drop(&p) {
                            return Err(self.err_en(
                                "C0008",
                                "no se puede mover desde `*` (clona con `.clone()`)".into(),
                                &x.expr,
                            ));
                        }
                        let c = inner.c;
                        Ok(ExVal::puro(format!("(*({c}))"), (*p).clone()))
                    }
                    CType::Ptr(_, p) => {
                        if self.necesita_drop(&p) {
                            return Err(self.err_en(
                                "C0008",
                                "no se puede mover desde `*` crudo".into(),
                                &x.expr,
                            ));
                        }
                        let c = inner.c;
                        Ok(ExVal::puro(format!("(*({c}))"), (*p).clone()))
                    }
                    _ => Err(self.err_en(
                        "C0003",
                        format!("`*` necesita referencia o puntero, no `{}`", self.muestra(&inner.tipo)),
                        &x.expr,
                    )),
                }
            }
            _ => Err(self.err_en("C0003", "operador unario no soportado".into(), x)),
        }
    }

    /// Literal negado (`-128i8` vale; `-5u8` no).
    fn ex_neg_lit(
        &mut self,
        li: &syn::LitInt,
        esp: Option<&CType>,
        x: &syn::ExprUnary,
    ) -> Result<ExVal, CError> {
        let suf = li.suffix();
        if matches!(suf, "u8" | "u16" | "u32" | "u64" | "usize") {
            return Err(self.err_en("C0003", format!("no se puede negar `{suf}`"), x));
        }
        let t = if suf.is_empty() {
            match esp {
                Some(e) if e.es_numero() && !e.es_sin_signo() && !matches!(e, CType::Char) => e.clone(),
                Some(e) => {
                    return Err(self.err_en(
                        "C0005",
                        format!("se esperaba `{}`, negar da entero con signo", self.muestra(e)),
                        x,
                    ))
                }
                None => CType::I32,
            }
        } else {
            match suf {
                "i8" => CType::I8,
                "i16" => CType::I16,
                "i32" => CType::I32,
                "i64" => CType::I64,
                "isize" => CType::Isize,
                _ => return Err(self.err_en("C0003", format!("sufijo `{suf}` raro tras `-`"), x)),
            }
        };
        let v: i128 = li.base10_parse::<i128>().map_err(|_| {
            self.err_en("C0006", format!("literal `{}` muy grande", li.base10_digits()), x)
        })?;
        let neg = -v;
        let cabe = match t {
            CType::I8 => neg >= -128,
            CType::I16 => neg >= -32768,
            CType::I32 => neg >= -2147483648,
            CType::I64 => neg >= -9223372036854775808,
            CType::Isize => neg >= -(std::mem::size_of::<isize>() as i128 * 128),
            _ => false,
        };
        if !cabe {
            return Err(self.err_en("C0006", format!("`-{}` no cabe en `{}`", v, self.muestra(&t)), x));
        }
        if let Some(e) = esp {
            if e != &t {
                return Err(self.err_en(
                    "C0005",
                    format!("se esperaba `{}`, el literal da `{}`", self.muestra(e), self.muestra(&t)),
                    x,
                ));
            }
        }
        let base = neg.to_string();
        if matches!(t, CType::I64) {
            let spell = t.deletrea().map_err(|e| self.con_archivo(e))?;
            return Ok(ExVal::puro(format!("(({spell}){base})"), t));
        }
        if suf.is_empty() {
            Ok(ExVal::puro(base, t))
        } else {
            Ok(ExVal::puro(format!("{base}{suf}"), t))
        }
    }

    /// `=` y `op=` (valen `()`).
    fn ex_assign(&mut self, x: &syn::ExprAssign, esp: Option<&CType>) -> Result<ExVal, CError> {
        if let Some(e) = esp {
            if *e != CType::Vacio {
                return Err(self.err_en(
                    "C0005",
                    format!("se esperaba `{}`, asignar vale `()`", self.muestra(e)),
                    x,
                ));
            }
        }
        let (pl, t_dest, n_rust) = self.lugar_assign(&x.left)?;
        let v = self.baja_expr(&x.right, Some(&t_dest))?;
        let c = self.consume(v, Some(&t_dest))?;
        // Suelta lo viejo (salvo variable ya movida: no hay nada que soltar).
        let ya_movida = n_rust
            .as_deref()
            .and_then(|n| self.busca(n))
            .map(|(_, vi)| vi.movida)
            .unwrap_or(false);
        if !ya_movida {
            for l in self.libera_lugar(&pl, &t_dest) {
                self.emite(l);
            }
        }
        self.emite(format!("({pl}) = ({c});"));
        if let Some(n) = n_rust {
            self.revive(&n);
        }
        Ok(ExVal::puro("0".into(), CType::Vacio))
    }

    /// Revive una variable reasignada (vale de nuevo, su bandera se apaga).
    fn revive(&mut self, nombre: &str) {
        let b = if let Some((i, _)) = self.busca(nombre) {
            let v = self.pila[i].vars.get_mut(nombre).unwrap();
            v.movida = false;
            v.bandera.clone()
        } else {
            None
        };
        if let Some(b) = b {
            self.emite(format!("{b} = false;"));
        }
    }

    /// Baja un destino de asignación a (lugar C, tipo, nombre Rust si es variable).
    fn lugar_assign(&mut self, t: &syn::Expr) -> Result<(String, CType, Option<String>), CError> {
        match t {
            syn::Expr::Path(p) if p.path.segments.len() == 1 && p.qself.is_none() => {
                let n = p.path.segments[0].ident.to_string();
                if let Some((_, v)) = self.busca(&n) {
                    return Ok((v.nombre_c.clone(), v.tipo.clone(), Some(n)));
                }
                Err(self.err_en("C0004", format!("`{n}` no existe o no asignable"), t))
            }
            syn::Expr::Paren(p) => self.lugar_assign(&p.expr),
            syn::Expr::Field(f) => {
                let b = self.baja_expr(&f.base, None)?;
                if b.lugar.is_none() && b.movible.is_none() {
                    return Err(self.err_en(
                        "C0003",
                        "no se puede asignar a un temporal".into(),
                        &f.base,
                    ));
                }
                let (t_base, flechita) = match &b.tipo {
                    CType::Ref(_, p) | CType::Caja(p) => ((**p).clone(), true),
                    otro => (otro.clone(), false),
                };
                let (nc, tf) = self.campo_info(&t_base, &f.member, f)?;
                let sep = if flechita { "->" } else { "." };
                Ok((format!("(({}){sep}{nc})", b.c), tf, None))
            }
            syn::Expr::Index(i) => {
                let (pl, te) = self.indice_lugar(&i.expr, &i.index)?;
                Ok((pl, te, None))
            }
            syn::Expr::Unary(u) if matches!(u.op, syn::UnOp::Deref(_)) => {
                // Asignar por desreferencia toma prestado (no mueve).
                let inner = self.baja_expr(&u.expr, None)?;
                let pt = match &inner.tipo {
                    CType::Ref(false, _) => {
                        return Err(self.err_en(
                            "C0003",
                            "no se puede asignar por referencia compartida".into(),
                            &u.expr,
                        ))
                    }
                    CType::Ref(true, p) | CType::Caja(p) | CType::Ptr(_, p) => (**p).clone(),
                    otro => {
                        return Err(self.err_en(
                            "C0003",
                            format!("no se puede asignar por `{}`", self.muestra(otro)),
                            &u.expr,
                        ))
                    }
                };
                Ok((format!("(*({}))", inner.c), pt, None))
            }
            _ => Err(self.err_en("C0003", "eso no es asignable".into(), t)),
        }
    }

    /// `+= -= *= /= %= &= |= ^= <<= >>=` (el lugar se evalúa una sola vez).
    fn ex_assign_op(&mut self, x: &syn::ExprBinary, esp: Option<&CType>) -> Result<(), CError> {
        if let Some(e) = esp {
            if *e != CType::Vacio {
                return Err(self.err_en(
                    "C0005",
                    format!("se esperaba `{}`, asignar vale `()`", self.muestra(e)),
                    x,
                ));
            }
        }
        let (pl, t, n_rust) = self.lugar_assign(&x.left)?;
        // `op=` lee: una variable movida no vale.
        if let Some(n) = &n_rust {
            if self.busca(n).map(|(_, vi)| vi.movida).unwrap_or(false) {
                return Err(self.err_en("C0008", format!("`{n}` se movió antes"), &x.left));
            }
        }
        // `s += vista`.
        if t == CType::Texto {
            if !matches!(x.op, syn::BinOp::AddAssign(_)) {
                return Err(self.err_en("C0003", "a `String` solo se le suma (`+=`)".into(), x));
            }
            let v = self.baja_expr(&x.right, Some(&CType::VistaTexto))?;
            if v.tipo != CType::VistaTexto {
                return Err(self.err_en(
                    "C0005",
                    format!("`+=` de texto pide `&str`, no `{}`", self.muestra(&v.tipo)),
                    &x.right,
                ));
            }
            let c = self.consume(v, Some(&CType::VistaTexto))?;
            self.emite(format!("kami_texto_empuja(&({pl}), ({c}));"));
            return Ok(());
        }
        let es_corr = matches!(x.op, syn::BinOp::ShlAssign(_) | syn::BinOp::ShrAssign(_));
        let es_bit = matches!(
            x.op,
            syn::BinOp::BitAndAssign(_) | syn::BinOp::BitOrAssign(_) | syn::BinOp::BitXorAssign(_)
        );
        if es_corr {
            if !Self::es_ent(&t) {
                return Err(self.err_en(
                    "C0003",
                    format!("corrimiento no aplica a `{}`", self.muestra(&t)),
                    x,
                ));
            }
        } else if es_bit {
            if !(Self::es_ent(&t) || t == CType::Bool) {
                return Err(self.err_en(
                    "C0003",
                    format!("bits no aplica a `{}`", self.muestra(&t)),
                    x,
                ));
            }
        } else if !t.es_numero() || matches!(t, CType::Char | CType::Bool) {
            return Err(self.err_en(
                "C0003",
                format!("`op=` no aplica a `{}`", self.muestra(&t)),
                x,
            ));
        }
        let v = self.baja_expr(&x.right, if es_corr { None } else { Some(&t) })?;
        if es_corr {
            if v.tipo != CType::Infer && !Self::es_ent(&v.tipo) {
                return Err(self.err_en(
                    "C0005",
                    format!("el corrimiento pide entero, no `{}`", self.muestra(&v.tipo)),
                    &x.right,
                ));
            }
        } else if v.tipo != t && v.tipo != CType::Infer {
            return Err(self.err_en(
                "C0005",
                format!("`{}` y `{}` no operan juntos", self.muestra(&t), self.muestra(&v.tipo)),
                x,
            ));
        }
        // Hoist de la dirección: el lugar se evalúa una sola vez.
        let p = self.temp(CType::Ref(true, Box::new(t.clone())))?;
        self.emite(format!("{p} = &({pl});"));
        let tv = v.tipo.clone();
        let c = self.consume(v, Some(if es_corr { &tv } else { &t }))?;
        let flotante = t.es_flotante();
        match x.op {
            syn::BinOp::AddAssign(_) | syn::BinOp::SubAssign(_) | syn::BinOp::MulAssign(_) => {
                let op = match x.op {
                    syn::BinOp::AddAssign(_) => "+",
                    syn::BinOp::SubAssign(_) => "-",
                    _ => "*",
                };
                if flotante {
                    self.emite(format!("(*{p}) = ((*({p})) {op} ({c}));"));
                    return Ok(());
                }
                let (builtin, nombre) = match op {
                    "+" => ("add", "suma"),
                    "-" => ("sub", "resta"),
                    _ => ("mul", "multiplicación"),
                };
                self.emite(format!("if (__builtin_{builtin}_overflow((*{p}), ({c}), &(*{p}))) {{"));
                self.emite(format!("    KAMI_PANICO(\"desbordamiento en {nombre} {}\");", self.muestra(&t)));
                self.emite("}");
                Ok(())
            }
            syn::BinOp::DivAssign(_) | syn::BinOp::RemAssign(_) => {
                let (op, nombre) = if matches!(x.op, syn::BinOp::DivAssign(_)) {
                    ("/", "división")
                } else {
                    ("%", "resto")
                };
                if flotante {
                    self.emite(format!("(*{p}) = ((*({p})) {op} ({c}));"));
                    return Ok(());
                }
                let tc = self.temp(t.clone())?;
                self.emite(format!("{tc} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                self.emite(format!("if (({tc}) == 0) {{ KAMI_PANICO(\"división por cero\"); }}"));
                if !t.es_sin_signo() {
                    let minv = match t {
                        CType::I8 => "INT8_MIN",
                        CType::I16 => "INT16_MIN",
                        CType::I32 => "INT32_MIN",
                        CType::I64 => "INT64_MIN",
                        _ => "PTRDIFF_MIN",
                    };
                    self.emite(format!(
                        "if ((*{p}) == ({minv}) && ({tc}) == -1) {{ KAMI_PANICO(\"desbordamiento en {nombre}\"); }}"
                    ));
                }
                self.emite(format!("(*{p}) = ((*({p})) {op} ({tc}));"));
                Ok(())
            }
            syn::BinOp::BitAndAssign(_) | syn::BinOp::BitOrAssign(_) | syn::BinOp::BitXorAssign(_) => {
                let op = match x.op {
                    syn::BinOp::BitAndAssign(_) => "&",
                    syn::BinOp::BitOrAssign(_) => "|",
                    _ => "^",
                };
                self.emite(format!("(*{p}) = ((*({p})) {op} ({c}));"));
                Ok(())
            }
            _ => {
                // Corrimientos: hoist del monto + chequeo.
                let tvt = if tv == CType::Infer { t.clone() } else { tv.clone() };
                let tc = self.temp(tvt.clone())?;
                self.emite(format!("{tc} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let bits = match t {
                    CType::I8 | CType::U8 => 8,
                    CType::I16 | CType::U16 => 16,
                    CType::I32 | CType::U32 => 32,
                    CType::I64 | CType::U64 => 64,
                    _ => (std::mem::size_of::<usize>() * 8) as i32,
                };
                if tvt.es_sin_signo() {
                    self.emite(format!("if (({tc}) >= {bits}) {{ KAMI_PANICO(\"corrimiento fuera de rango\"); }}"));
                } else {
                    self.emite(format!(
                        "if (({tc}) < 0 || ({tc}) >= {bits}) {{ KAMI_PANICO(\"corrimiento fuera de rango\"); }}"
                    ));
                }
                if matches!(x.op, syn::BinOp::ShlAssign(_)) {
                    if t.es_sin_signo() {
                        self.emite(format!("(*{p}) = ((*({p})) << ({tc}));"));
                    } else {
                        let spell = t.deletrea().map_err(|e| self.con_archivo(e))?;
                        let uspell = match t {
                            CType::I8 => "uint8_t",
                            CType::I16 => "uint16_t",
                            CType::I32 => "uint32_t",
                            CType::I64 => "uint64_t",
                            _ => "uintptr_t",
                        };
                        self.emite(format!("(*{p}) = (({spell})((({uspell})(*({p}))) << ({tc})));"));
                    }
                } else {
                    self.emite(format!("(*{p}) = ((*({p})) >> ({tc}));"));
                }
                Ok(())
            }
        }
    }

    // ==================================================================
    // Llamadas
    // ==================================================================

    /// Llamadas `f(args)`.
    fn ex_call(&mut self, x: &syn::ExprCall, esp: Option<&CType>) -> Result<ExVal, CError> {
        // 1. `f(args)` con `f` ruta (función, ctor, asociada, std).
        if let syn::Expr::Path(p) = &*x.func {
            if p.qself.is_none() {
                if let Some(v) = self.ex_call_path(p, &x.args, esp, x)? {
                    return Ok(v);
                }
            } else {
                return self.ex_call_qself(p, &x.args, esp, x);
            }
        }
        // 2. Callee valor (variable con puntero-fn, método-como-valor...).
        let f = self.baja_expr(&x.func, None)?;
        match &f.tipo {
            CType::FnPtr { params, ret } => {
                if params.len() != x.args.len() {
                    return Err(self.err_en(
                        "C0005",
                        format!("ese puntero-fn pide {}, llegaron {}", params.len(), x.args.len()),
                        &x.func,
                    ));
                }
                let mut cs = Vec::new();
                for (a, pt) in x.args.iter().zip(params.iter()) {
                    cs.push(self.conv_arg(a, pt)?);
                }
                let t = (**ret).clone();
                if let Some(e) = esp {
                    if e != &t {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, llamar da `{}`", self.muestra(e), self.muestra(&t)),
                            x,
                        ));
                    }
                }
                Ok(ExVal::puro(format!("({})({})", f.c, cs.join(", ")), t))
            }
            _ => Err(self.err_en(
                "C0003",
                format!("`{}` no se puede llamar", self.muestra(&f.tipo)),
                &x.func,
            )
            .ayuda("solo funciones, métodos y punteros-fn se llaman")),
        }
    }

    /// Baja un argumento y lo convierte al parámetro (mueve o copia).
    fn conv_arg(&mut self, a: &syn::Expr, pt: &CType) -> Result<String, CError> {
        let v = self.baja_expr(a, Some(pt))?;
        if v.arreglo.is_some() {
            return self.conv_arreglo(v, pt);
        }
        self.consume(v, Some(pt))
    }

    /// Llama a una función de usuario ya resuelta.
    fn ex_call_user(
        &mut self,
        nombre_c: &str,
        f: &super::baja::Firma,
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        esp: Option<&CType>,
        x: &syn::ExprCall,
    ) -> Result<ExVal, CError> {
        if !f.variadic && f.params.len() != args.len() {
            return Err(self.err_en(
                "C0005",
                format!("`{nombre_c}` pide {}, llegaron {}", f.params.len(), args.len()),
                x,
            ));
        }
        if f.variadic && args.len() < f.params.len() {
            return Err(self.err_en(
                "C0005",
                format!("`{nombre_c}` pide al menos {}, llegaron {}", f.params.len(), args.len()),
                x,
            ));
        }
        let mut cs = Vec::new();
        for (i, a) in args.iter().enumerate() {
            match f.params.get(i) {
                Some((_, pt)) => cs.push(self.conv_arg(a, pt)?),
                None => {
                    // Cola variádica (`extern`): pasa tal cual (C promueve solo).
                    let v = self.baja_expr(a, None)?;
                    cs.push(self.consume(v, None)?);
                }
            }
        }
        let t = f.ret.clone();
        if let Some(e) = esp {
            if e != &t {
                return Err(self.err_en(
                    "C0005",
                    format!("se esperaba `{}`, `{nombre_c}` da `{}`", self.muestra(e), self.muestra(&t)),
                    x,
                ));
            }
        }
        Ok(ExVal::puro(format!("({nombre_c}({}))", cs.join(", ")), t))
    }

    /// `f(args)` con `f` ruta. `None` = no es función conocida (quizá variable llamable).
    fn ex_call_path(
        &mut self,
        p: &syn::ExprPath,
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        esp: Option<&CType>,
        x: &syn::ExprCall,
    ) -> Result<Option<ExVal>, CError> {
        let segs: Vec<String> = p.path.segments.iter().map(|s| s.ident.to_string()).collect();
        let raiz = p.path.leading_colon.is_some();
        let turbo_ult = matches!(
            p.path.segments.last().map(|s| &s.arguments),
            Some(syn::PathArguments::AngleBracketed(_))
        );
        if segs.len() == 1 {
            let n = &segs[0];
            if n == "Some" || n == "Ok" || n == "Err" {
                return self.ex_ctor_llamada(n, args, esp, x).map(Some);
            }
            if n == "None" {
                return Err(self.err_en("C0003", "`None` no se llama (es un valor)".into(), x)
                    .ayuda("escribe `None` sin paréntesis"));
            }
            if n == "drop" || n == "size_of" || n == "align_of" {
                return self.ex_std_fn(n, p, args, esp, x).map(Some);
            }
            if let Some((nc, f)) = self.resuelve_fn_pub(&segs, raiz) {
                if turbo_ult {
                    return Err(self.err_en("C0003", "funciones sin genéricos".into(), x)
                        .ayuda("quita el `::<...>`"));
                }
                return self.ex_call_user(&nc, &f, args, esp, x).map(Some);
            }
            if let Some(v) = self.ex_ctor_tupla(&segs, raiz, args, esp, x)? {
                return Ok(Some(v));
            }
            return Ok(None);
        }
        // De aquí en adelante la ruta debe existir (las variables no tienen `::`).
        let ruta = self.ruta_abs(&segs, raiz);
        if let Some((nc, f)) = self.resuelve_fn_pub(&ruta, true) {
            if turbo_ult {
                return Err(self.err_en("C0003", "funciones sin genéricos".into(), x)
                    .ayuda("quita el `::<...>`"));
            }
            return self.ex_call_user(&nc, &f, args, esp, x).map(Some);
        }
        if ruta.len() >= 2 {
            if let Some(v) = self.ex_ctor_tupla(&ruta, true, args, esp, x)? {
                return Ok(Some(v));
            }
            if let Some(v) = self.ex_call_asociada_ruta(&ruta, args, esp, x)? {
                return Ok(Some(v));
            }
            if let Some(v) = self.ex_std_multi(&ruta, p, args, esp, x)? {
                return Ok(Some(v));
            }
            return Err(self.err_en("C0004", format!("`{}` no existe", ruta.join("::")), x));
        }
        Err(self.err_en("C0004", format!("`{}` no existe", ruta.join("::")), x))
    }

    /// Ruta absoluta: `::` ignora el módulo actual; si no, normaliza.
    fn ruta_abs(&self, segs: &[String], raiz: bool) -> Vec<String> {
        if raiz {
            let mut r = segs.to_vec();
            if r.first().is_some_and(|s| s == "crate") {
                r.remove(0);
            }
            r
        } else {
            self.normaliza_ruta(segs)
        }
    }

    /// Saca los tipos del turbofish (`f::<A, B>`): primer segmento con `<...>`.
    fn tipos_turbo(&self, p: &syn::ExprPath, x: &syn::ExprCall) -> Result<Vec<CType>, CError> {
        for s in &p.path.segments {
            if let syn::PathArguments::AngleBracketed(ab) = &s.arguments {
                let mut ts = Vec::new();
                for a in &ab.args {
                    match a {
                        syn::GenericArgument::Type(t) => ts.push(self.tipo_de_arg(t, x)?),
                        syn::GenericArgument::Lifetime(_) => {}
                        _ => {
                            return Err(self.err_en(
                                "C0003",
                                "ese argumento genérico no cabe".into(),
                                x,
                            ))
                        }
                    }
                }
                return Ok(ts);
            }
        }
        Ok(Vec::new())
    }

    /// Convierte un tipo de turbofish a `CType` (primitivos, usuario, `Vec`/`Option`/`Result`).
    fn tipo_de_arg(&self, t: &syn::Type, x: &syn::ExprCall) -> Result<CType, CError> {
        match t {
            syn::Type::Group(g) => self.tipo_de_arg(&g.elem, x),
            syn::Type::Paren(p) => self.tipo_de_arg(&p.elem, x),
            syn::Type::Infer(_) => {
                Err(self.err_en("C0003", "anota el tipo del turbofish".into(), x))
            }
            syn::Type::Tuple(tp) => {
                if tp.elems.is_empty() {
                    return Ok(CType::Vacio);
                }
                let mut cs = Vec::new();
                for e in &tp.elems {
                    cs.push(self.tipo_de_arg(e, x)?);
                }
                Ok(CType::Tupla(cs))
            }
            syn::Type::Path(tp) if tp.qself.is_none() => {
                let segs: Vec<String> =
                    tp.path.segments.iter().map(|s| s.ident.to_string()).collect();
                let ult = tp.path.segments.last().unwrap();
                // ¿Genéricos anidados? (`Vec<i32>`, `Result<T, E>`).
                if let syn::PathArguments::AngleBracketed(ab) = &ult.arguments {
                    let mut inner = Vec::new();
                    for a in &ab.args {
                        match a {
                            syn::GenericArgument::Type(t2) => inner.push(self.tipo_de_arg(t2, x)?),
                            syn::GenericArgument::Lifetime(_) => {}
                            _ => {
                                return Err(self.err_en(
                                    "C0003",
                                    "ese argumento genérico no cabe".into(),
                                    x,
                                ))
                            }
                        }
                    }
                    let nb = ult.ident.to_string();
                    if (nb == "Vec" || nb == "Option") && inner.len() == 1 {
                        let v = inner.remove(0);
                        if nb == "Vec" {
                            return Ok(CType::Vec(Box::new(v)));
                        }
                        return Ok(CType::Opcion(Box::new(v)));
                    }
                    if nb == "Result" && inner.len() == 2 {
                        let e = inner.remove(1);
                        let v = inner.remove(0);
                        return Ok(CType::Resultado(Box::new(v), Box::new(e)));
                    }
                    return Err(self.err_en(
                        "C0003",
                        format!("`{nb}` con genéricos no cabe aquí"),
                        x,
                    ));
                }
                // `Self` dentro de `impl`.
                if segs == ["Self"] {
                    match &self.tipo_self {
                        Some(n) => return Ok(CType::Usuario(n.clone())),
                        None => return Err(self.err_en("C0003", "`Self` fuera de `impl`".into(), x)),
                    }
                }
                // Primitivos y `String`.
                if segs.len() == 1 {
                    let t = match segs[0].as_str() {
                        "bool" => Some(CType::Bool),
                        "char" => Some(CType::Char),
                        "i8" => Some(CType::I8),
                        "i16" => Some(CType::I16),
                        "i32" => Some(CType::I32),
                        "i64" => Some(CType::I64),
                        "isize" => Some(CType::Isize),
                        "u8" => Some(CType::U8),
                        "u16" => Some(CType::U16),
                        "u32" => Some(CType::U32),
                        "u64" => Some(CType::U64),
                        "usize" => Some(CType::Usize),
                        "f32" => Some(CType::F32),
                        "f64" => Some(CType::F64),
                        "String" => Some(CType::Texto),
                        _ => None,
                    };
                    if let Some(t) = t {
                        return Ok(t);
                    }
                    if segs[0] == "str" {
                        return Err(self.err_en("C0003", "`str` no tiene tamaño".into(), x));
                    }
                }
                // Usuario (struct/enum).
                if let Some(nc) = self.resuelve_tipo_pub(&segs, tp.path.leading_colon.is_some()) {
                    return Ok(CType::Usuario(nc));
                }
                Err(self.err_en("C0003", format!("tipo `{}` desconocido", segs.join("::")), x))
            }
            _ => Err(self.err_en("C0003", "ese tipo no cabe en turbofish".into(), x)
                .ayuda("usa un tipo con nombre, primitivo o tupla")),
        }
    }

    /// Fns sueltas de std (`drop`, `size_of`, `align_of` importadas o en preludio).
    fn ex_std_fn(
        &mut self,
        n: &str,
        p: &syn::ExprPath,
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        esp: Option<&CType>,
        x: &syn::ExprCall,
    ) -> Result<ExVal, CError> {
        match n {
            "drop" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`drop` lleva un valor".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Vacio {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `drop` vale `()`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let v = self.baja_expr(&args[0], None)?;
                if v.movible.is_none() && v.lugar.is_some() && self.necesita_drop(&v.tipo) {
                    return Err(self.err_en(
                        "C0008",
                        "`drop` mueve; ese lugar no se puede mover".into(),
                        &args[0],
                    ));
                }
                let es_lugar = v.movible.is_some() || v.lugar.is_some();
                let tt = v.tipo.clone();
                let c = self.consume(v, None)?;
                if es_lugar {
                    for l in self.libera_lugar(&c, &tt) {
                        self.emite(l);
                    }
                } else if self.necesita_drop(&tt) {
                    let t = self.temp(tt.clone())?;
                    self.emite(format!("{t} = ({c});"));
                    self.marca_movida_si_temp_pub(&c);
                    for l in self.libera_lugar(&t, &tt) {
                        self.emite(l);
                    }
                }
                Ok(ExVal::puro("0".into(), CType::Vacio))
            }
            _ => {
                // `size_of::<T>()` / `align_of::<T>()`.
                let ts = self.tipos_turbo(p, x)?;
                if ts.len() != 1 {
                    return Err(self.err_en("C0003", format!("`{n}::<T>` lleva un tipo"), x));
                }
                if !args.is_empty() {
                    return Err(self.err_en("C0005", format!("`{n}::<T>()` no lleva argumentos"), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Usize {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `{n}` da `usize`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let t = &ts[0];
                if *t == CType::Vacio {
                    return Ok(ExVal::puro("((size_t)1)".into(), CType::Usize));
                }
                let s = t.deletrea().map_err(|e| self.con_archivo(e))?;
                let kw = if n == "size_of" { "sizeof" } else { "_Alignof" };
                Ok(ExVal::puro(format!("(({kw}({s})))"), CType::Usize))
            }
        }
    }

    /// Llamadas std con `::` (`String::from`, `Vec::new`, `std::mem::size_of`, `T::from`...).
    /// `None` = no es std conocida.
    fn ex_std_multi(
        &mut self,
        ruta: &[String],
        p: &syn::ExprPath,
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        esp: Option<&CType>,
        x: &syn::ExprCall,
    ) -> Result<Option<ExVal>, CError> {
        // `std::mem::size_of::<T>()` y familia.
        if let Some(ult) = ruta.last() {
            if ult == "size_of" || ult == "align_of" {
                if ruta[..ruta.len() - 1].iter().any(|s| s == "mem") {
                    return self.ex_std_fn(ult, p, args, esp, x).map(Some);
                }
                return Ok(None);
            }
        }
        // `Option::Some(x)`, `Result::Ok(x)` calificados.
        if ruta.len() >= 2 {
            let (a, b) = (&ruta[ruta.len() - 2], &ruta[ruta.len() - 1]);
            if (a == "Option" && b == "Some") || (a == "Result" && (b == "Ok" || b == "Err")) {
                return self.ex_ctor_llamada(b, args, esp, x).map(Some);
            }
        }
        // `std::mem::...`, `core::ptr::...` (ruta calificada de 3).
        if ruta.len() == 3 && (ruta[0] == "std" || ruta[0] == "core") {
            let (t0, m) = (ruta[1].as_str(), ruta[2].as_str());
            if t0 == "mem" {
                if m == "size_of" || m == "align_of" {
                    return self.ex_std_fn(m, p, args, esp, x).map(Some);
                }
                return self.ex_mem_fn(m, args, esp, x).map(Some);
            }
            if t0 == "ptr" {
                return self.ex_ptr_fn(m, args, esp, x).map(Some);
            }
            if t0 == "slice" && (m == "from_ref" || m == "from_mut") {
                return self.ex_slice_from(m, args, esp, x).map(Some);
            }
            return Ok(None);
        }
        if ruta.len() != 2 {
            return Ok(None);
        }
        let (t0, m) = (ruta[0].as_str(), ruta[1].as_str());
        match (t0, m) {
            ("String", "new") => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`String::new()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Texto {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `String::new` da `String`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                Ok(Some(ExVal::puro("kami_texto_nuevo()".into(), CType::Texto)))
            }
            ("String", "from") => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`String::from` lleva un valor".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Texto {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `String::from` da `String`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let v = self.baja_expr(&args[0], None)?;
                let c = match &v.tipo {
                    CType::VistaTexto => {
                        let c = self.consume(v, Some(&CType::VistaTexto))?;
                        format!("kami_texto_desde(({c}))")
                    }
                    CType::Texto => self.consume(v, Some(&CType::Texto))?,
                    CType::Ref(_, t) if matches!(**t, CType::Texto) => {
                        let tt = v.tipo.clone();
                        let c = self.consume(v, Some(&tt))?;
                        format!("kami_texto_clona(({c}))")
                    }
                    _ => {
                        return Err(self.err_en(
                            "C0005",
                            format!("`String::from` pide `&str`/`String`, no `{}`", self.muestra(&v.tipo)),
                            &args[0],
                        ))
                    }
                };
                Ok(Some(ExVal::puro(c, CType::Texto)))
            }
            ("Vec", "new") => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`Vec::new()` no lleva argumentos".into(), x));
                }
                self.ex_vec_new(None, esp, x).map(Some)
            }
            ("Vec", "with_capacity") => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`Vec::with_capacity` lleva un largo".into(), x));
                }
                let v = self.baja_expr(&args[0], Some(&CType::Usize))?;
                let c = self.consume(v, Some(&CType::Usize))?;
                self.ex_vec_new(Some(c), esp, x).map(Some)
            }
            ("Option", "None") => Err(self.err_en(
                "C0003",
                "`Option::None` no se llama (es un valor)".into(),
                x,
            )
            .ayuda("escribe `None` sin paréntesis")),
            _ if m == "from" && Self::es_ent_num(t0) => {
                self.ex_from_num(t0, args, esp, x).map(Some)
            }
            _ if m == "from_str" && (Self::es_ent_num(t0) || t0 == "bool") => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", format!("`{t0}::from_str` lleva una cadena"), x));
                }
                let t_dest = if t0 == "bool" {
                    CType::Bool
                } else {
                    Self::num_por_nombre(t0).unwrap()
                };
                let t_r = CType::Resultado(Box::new(t_dest), Box::new(CType::VistaTexto));
                match esp {
                    Some(e) if e == &t_r => {}
                    Some(e) => {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `{t0}::from_str` da `Result`", self.muestra(e)),
                            x,
                        ))
                    }
                    None => {
                        return Err(self.err_en(
                            "C0005",
                            format!("`{t0}::from_str` necesita tipo esperado (anota el `let`)"),
                            x,
                        ))
                    }
                }
                let v = self.baja_expr(&args[0], Some(&CType::VistaTexto))?;
                let c = self.consume(v, Some(&CType::VistaTexto))?;
                let vacia: syn::punctuated::Punctuated<syn::Expr, syn::token::Comma> =
                    syn::punctuated::Punctuated::new();
                self.ex_parse(c, &[], &vacia, esp, x).map(Some)
            }
            _ if m == "from_str_radix"
                && matches!(
                    t0,
                    "i8" | "i16" | "i32" | "i64" | "isize" | "u8" | "u16" | "u32" | "u64" | "usize"
                ) =>
            {
                if args.len() != 2 {
                    return Err(self.err_en("C0005", format!("`{t0}::from_str_radix` lleva cadena y base"), x));
                }
                let t_dest = Self::num_por_nombre(t0).unwrap();
                let t_r = CType::Resultado(Box::new(t_dest.clone()), Box::new(CType::VistaTexto));
                match esp {
                    Some(e) if e == &t_r => {}
                    Some(e) => {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `Result`", self.muestra(e)),
                            x,
                        ))
                    }
                    None => {
                        return Err(self.err_en(
                            "C0005",
                            format!("`{t0}::from_str_radix` necesita tipo esperado (anota el `let`)"),
                            x,
                        ))
                    }
                }
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let v0 = self.baja_expr(&args[0], Some(&CType::VistaTexto))?;
                let cs = self.consume(v0, Some(&CType::VistaTexto))?;
                let v1 = self.baja_expr(&args[1], Some(&CType::U32))?;
                let cr = self.consume(v1, Some(&CType::U32))?;
                let tc = self.temp(CType::VistaTexto)?;
                self.emite(format!("{tc} = ({cs});"));
                self.marca_movida_si_temp_pub(&cs);
                let trad = self.temp(CType::U32)?;
                self.emite(format!("{trad} = ({cr});"));
                self.marca_movida_si_temp_pub(&cr);
                let to = self.temp(t_r.clone())?;
                self.emite(format!("memset(&({to}), 0, sizeof({to}));"));
                let spell = t_dest.deletrea().map_err(|e| self.con_archivo(e))?;
                let (poslim, neglim) = match t_dest {
                    CType::I8 => ("127", "128"),
                    CType::I16 => ("32767", "32768"),
                    CType::I32 => ("2147483647", "2147483648"),
                    CType::I64 | CType::Isize => ("9223372036854775807", "9223372036854775808ULL"),
                    CType::U8 => ("255", "0"),
                    CType::U16 => ("65535", "0"),
                    CType::U32 => ("4294967295", "0"),
                    CType::U64 | CType::Usize => ("18446744073709551615ULL", "0"),
                    _ => unreachable!(),
                };
                let lim_e = if t_dest.es_sin_signo() {
                    format!("(unsigned __int128)({poslim})")
                } else {
                    format!("(unsigned __int128)(((__neg) ? ({neglim}) : ({poslim})))")
                };
                self.emite(format!("if ((({trad}) < 2) || (({trad}) > 36)) {{"));
                self.emite(format!("    ({to}).es_ok = false;"));
                self.emite(format!("    ({to}).datos.err = \"base inválida\";"));
                self.emite("} else {".to_string());
                self.emite("{".to_string());
                self.emite(format!("    const char *__p = ({tc});"));
                self.emite("    int __neg = 0;".to_string());
                self.emite("    unsigned __int128 __a = 0;".to_string());
                self.emite("    int __ok = 1;".to_string());
                self.emite("    int __nd = 0;".to_string());
                self.emite("    if (((*__p) == '+') || ((*__p) == '-')) { if ((*__p) == '-') { __neg = 1; } __p++; }".to_string());
                if t_dest.es_sin_signo() {
                    self.emite("    if ((__neg)) { __ok = 0; }".to_string());
                }
                self.emite("    for (; ((*__p) && (__ok)); __p++) {".to_string());
                self.emite("        int __d = (((*__p) >= '0' && (*__p) <= '9') ? ((*__p) - '0') : (((*__p) >= 'a' && (*__p) <= 'z') ? ((*__p) - 'a' + 10) : (((*__p) >= 'A' && (*__p) <= 'Z') ? ((*__p) - 'A' + 10) : -1)));".to_string());
                self.emite(format!("        if (((__d) < 0) || ((__d) >= (int)({trad}))) {{ __ok = 0; break; }}"));
                self.emite("        __nd++;".to_string());
                self.emite(format!("        __a = __a * (unsigned __int128)({trad}) + (unsigned __int128)(__d);"));
                self.emite(format!("        if ((__a > ({lim_e}))) {{ __ok = 0; break; }}"));
                self.emite("    }".to_string());
                self.emite("    if ((__nd == 0)) { __ok = 0; }".to_string());
                self.emite(format!("    if ((__ok)) {{ ({to}).es_ok = true; ({to}).datos.ok = (({spell})((__neg) ? (0) - (__a) : (__a))); }}"));
                self.emite(format!("    else {{ ({to}).es_ok = false; ({to}).datos.err = \"número inválido\"; }}"));
                self.emite("}".to_string());
                self.emite("}".to_string());
                Ok(Some(ExVal::movible(to.clone(), t_r, to)))
            }
            _ if (m == "from_be_bytes" || m == "from_le_bytes" || m == "from_ne_bytes")
                && matches!(
                    t0,
                    "i8" | "i16" | "i32" | "i64" | "isize" | "u8" | "u16" | "u32" | "u64" | "usize"
                ) =>
            {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", format!("`{t0}::{m}` lleva bytes"), x));
                }
                let t = Self::num_por_nombre(t0).unwrap();
                let nb = (Self::ancho(&t) / 8) as usize;
                if let Some(e) = esp {
                    if e != &t {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `{t0}`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let v = self.baja_expr(&args[0], None)?;
                match &v.tipo {
                    CType::Arreglo(e, LargoArreglo::Lit(k)) if e.as_ref() == &CType::U8 && *k == nb => {}
                    _ => {
                        return Err(self.err_en(
                            "C0005",
                            format!("`{t0}::{m}` pide `[u8; {nb}]`, llegó `{}`", self.muestra(&v.tipo)),
                            &args[0],
                        ))
                    }
                }
                let c = self.consume(v, None)?;
                if m == "from_ne_bytes" {
                    let tr = self.temp(t.clone())?;
                    self.emite(format!("memcpy(&({tr}), ({c}), {nb});"));
                    self.marca_movida_si_temp_pub(&c);
                    return Ok(Some(ExVal::movible(tr.clone(), t, tr)));
                }
                let uspell = match nb {
                    1 => "uint8_t",
                    2 => "uint16_t",
                    4 => "uint32_t",
                    _ => "uint64_t",
                };
                let mut partes = Vec::new();
                for i in 0..nb {
                    let idx = if m == "from_be_bytes" { nb - 1 - i } else { i };
                    let sh = i * 8;
                    if sh == 0 {
                        partes.push(format!("((({uspell})(({c})[{idx}])))"));
                    } else {
                        partes.push(format!("(((({uspell})(({c})[{idx}])) << {sh}))"));
                    }
                }
                let spell = t.deletrea().map_err(|e| self.con_archivo(e))?;
                Ok(Some(ExVal::puro(format!("(({spell})({}))", partes.join(" | ")), t)))
            }
            ("f32", "from_bits") | ("f64", "from_bits") => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", format!("`{t0}::from_bits` lleva bits"), x));
                }
                let (t, tu, nb) = if t0 == "f32" {
                    (CType::F32, CType::U32, 4)
                } else {
                    (CType::F64, CType::U64, 8)
                };
                if let Some(e) = esp {
                    if e != &t {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `{t0}`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let v = self.baja_expr(&args[0], Some(&tu))?;
                let cu = self.consume(v, Some(&tu))?;
                let tt = self.temp(tu)?;
                self.emite(format!("{tt} = ({cu});"));
                self.marca_movida_si_temp_pub(&cu);
                let tr = self.temp(t.clone())?;
                self.emite(format!("memcpy(&({tr}), &({tt}), {nb});"));
                Ok(Some(ExVal::puro(tr, t)))
            }
            ("char", "from_digit") => {
                if args.len() != 2 {
                    return Err(self.err_en("C0005", "`char::from_digit` lleva dígito y base".into(), x));
                }
                let t_r = CType::Opcion(Box::new(CType::Char));
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `Option<char>`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let v0 = self.baja_expr(&args[0], Some(&CType::U32))?;
                let c0 = self.consume(v0, Some(&CType::U32))?;
                let v1 = self.baja_expr(&args[1], Some(&CType::U32))?;
                let c1 = self.consume(v1, Some(&CType::U32))?;
                let tn = self.temp(CType::U32)?;
                self.emite(format!("{tn} = ({c0});"));
                self.marca_movida_si_temp_pub(&c0);
                let trad = self.temp(CType::U32)?;
                self.emite(format!("{trad} = ({c1});"));
                self.marca_movida_si_temp_pub(&c1);
                let to = self.temp(t_r.clone())?;
                self.emite(format!("memset(&({to}), 0, sizeof({to}));"));
                self.emite(format!("if (((({trad}) >= 2) && (({trad}) <= 36)) && (({tn}) < ({trad}))) {{"));
                self.emite(format!("    ({to}).tiene = true;"));
                self.emite(format!("    ({to}).valor = (((({tn}) < 10) ? (({tn}) + 48) : (({tn}) + 87)));"));
                self.emite("}".to_string());
                Ok(Some(ExVal::movible(to.clone(), t_r, to)))
            }
            ("char", "from_u32") => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`char::from_u32` lleva un valor".into(), x));
                }
                let t_r = CType::Opcion(Box::new(CType::Char));
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `Option<char>`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let v = self.baja_expr(&args[0], Some(&CType::U32))?;
                let cu = self.consume(v, Some(&CType::U32))?;
                let tu = self.temp(CType::U32)?;
                self.emite(format!("{tu} = ({cu});"));
                self.marca_movida_si_temp_pub(&cu);
                let to = self.temp(t_r.clone())?;
                self.emite(format!("memset(&({to}), 0, sizeof({to}));"));
                self.emite(format!("if (((({tu}) <= 0x10FFFF) && !((({tu}) >= 0xD800) && (({tu}) <= 0xDFFF)))) {{"));
                self.emite(format!("    ({to}).tiene = true;"));
                self.emite(format!("    ({to}).valor = ({tu});"));
                self.emite("}".to_string());
                Ok(Some(ExVal::movible(to.clone(), t_r, to)))
            }
            ("char", "from_u32_unchecked") => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`char::from_u32_unchecked` lleva un valor".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Char {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `char`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let v = self.baja_expr(&args[0], Some(&CType::U32))?;
                let cu = self.consume(v, Some(&CType::U32))?;
                Ok(Some(ExVal::puro(format!("(({cu}))"), CType::Char)))
            }
            ("char", "from_str") => {
                let mut e = self.err_en("C0003", "`char::from_str` tiene error innombrable".into(), x);
                e.ayuda = Some("toma el primer `char` con un bucle o rebanada".into());
                Err(e)
            }
            ("Box", "new") => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`Box::new` lleva un valor".into(), x));
                }
                let guia_in: Option<CType> = match esp {
                    Some(CType::Caja(i)) => Some(i.as_ref().clone()),
                    Some(e) => {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `Box::new` da `Box`", self.muestra(e)),
                            x,
                        ))
                    }
                    None => None,
                };
                let v = self.baja_expr(&args[0], guia_in.as_ref())?;
                let inner = guia_in.clone().unwrap_or_else(|| v.tipo.clone());
                if matches!(inner, CType::Vacio | CType::Rebana(_, _) | CType::Dyn(_) | CType::Arreglo(_, _)) {
                    let mut e = self.err_en(
                        "C0003",
                        format!("`Box` sobre `{}` sin soporte", self.muestra(&inner)),
                        &args[0],
                    );
                    e.ayuda = Some("encaja el valor en un `struct`".into());
                    return Err(e);
                }
                let t = CType::Caja(Box::new(inner.clone()));
                let c = self.consume(v, Some(&inner))?;
                let se = inner.deletrea().map_err(|e| self.con_archivo(e))?;
                let tr = self.temp(t.clone())?;
                self.emite(format!("{tr} = malloc(sizeof({se}));"));
                self.emite(format!("if ((({tr}) == NULL)) {{ KAMI_PANICO(\"sin memoria\"); }}"));
                self.emite(format!("(*({tr})) = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                Ok(Some(ExVal::movible(tr.clone(), t, tr)))
            }
            ("Box", "leak") | ("Box", "into_raw") => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", format!("`Box::{m}` lleva una caja"), x));
                }
                let v = self.baja_expr(&args[0], None)?;
                let inner = match &v.tipo {
                    CType::Caja(i) => i.as_ref().clone(),
                    _ => {
                        return Err(self.err_en(
                            "C0005",
                            format!("`Box::{m}` pide `Box`, no `{}`", self.muestra(&v.tipo)),
                            &args[0],
                        ))
                    }
                };
                let t_r = if m == "leak" {
                    CType::Ref(true, Box::new(inner))
                } else {
                    CType::Ptr(true, Box::new(inner))
                };
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `Box::{m}` da otro puntero", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let c = self.consume(v, None)?;
                self.marca_movida_si_temp_pub(&c);
                Ok(Some(ExVal::puro(c, t_r)))
            }
            ("Box", "from_raw") => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`Box::from_raw` lleva un puntero".into(), x));
                }
                let v = self.baja_expr(&args[0], None)?;
                let inner = match &v.tipo {
                    CType::Ptr(true, i) => i.as_ref().clone(),
                    _ => {
                        return Err(self.err_en(
                            "C0005",
                            format!("`Box::from_raw` pide `*mut T`, no `{}`", self.muestra(&v.tipo)),
                            &args[0],
                        ))
                    }
                };
                let t = CType::Caja(Box::new(inner));
                if let Some(e) = esp {
                    if e != &t {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `Box`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let c = self.consume(v, None)?;
                let tr = self.temp(t.clone())?;
                self.emite(format!("{tr} = ({c});"));
                Ok(Some(ExVal::movible(tr.clone(), t, tr)))
            }
            ("Box", "default" | "new_with" | "new_uninit" | "try_new" | "try_new_uninit") => {
                let mut e = self.err_en("C0003", format!("`Box::{m}` no tiene soporte"), x);
                e.ayuda = Some("usa `Box::new`".into());
                Err(e)
            }
            ("String", "from_utf8_unchecked") => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`String::from_utf8_unchecked` lleva bytes".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Texto {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `String`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let v = self.baja_expr(&args[0], None)?;
                match &v.tipo {
                    CType::Vec(i) if i.as_ref() == &CType::U8 => {}
                    _ => {
                        return Err(self.err_en(
                            "C0005",
                            format!("pide `Vec<u8>`, no `{}`", self.muestra(&v.tipo)),
                            &args[0],
                        ))
                    }
                }
                let t_v = v.tipo.clone();
                let c = self.consume(v, Some(&t_v))?;
                let ts = self.temp(t_v)?;
                self.emite(format!("{ts} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let tr = self.temp(CType::Texto)?;
                self.emite(format!("({tr}).datos = (({ts}).datos);"));
                self.emite(format!("({tr}).largo = (({ts}).largo);"));
                self.emite(format!("({tr}).capacidad = (({ts}).capacidad);"));
                self.marca_movida_si_temp_pub(&ts);
                Ok(Some(ExVal::movible(tr.clone(), CType::Texto, tr)))
            }
            ("String", "from_utf8" | "from_utf8_lossy" | "from_raw_parts" | "from_utf16" | "from_utf16_lossy") => {
                let mut e = self.err_en("C0003", format!("`String::{m}` no tiene soporte"), x);
                e.ayuda = Some("usa `String::from` o `from_utf8_unchecked`".into());
                Err(e)
            }
            ("str", "from_utf8" | "from_utf8_unchecked" | "from_raw_parts" | "from_boxed_utf8_unchecked") => {
                let mut e = self.err_en("C0003", format!("`str::{m}` no tiene soporte"), x);
                e.ayuda = Some("usa literales `&str` o rebanadas".into());
                Err(e)
            }
            ("slice", "from_ref") | ("slice", "from_mut") => {
                self.ex_slice_from(m, args, esp, x).map(Some)
            }
            ("mem", m) => self.ex_mem_fn(m, args, esp, x).map(Some),
            ("ptr", m) => self.ex_ptr_fn(m, args, esp, x).map(Some),
            ("array", "from_fn") | ("array", "try_from_fn") => {
                let mut e = self.err_en("C0003", format!("`array::{m}` pide una clausura"), x);
                e.ayuda = Some("llena a mano con un `for`".into());
                Err(e)
            }
            ("array", "from_ref") => {
                let mut e = self.err_en("C0003", "`array::from_ref` no tiene soporte".into(), x);
                e.ayuda = Some("usa `slice::from_ref`".into());
                Err(e)
            }
            ("Vec", "from") => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`Vec::from` lleva un valor".into(), x));
                }
                let v = self.baja_expr(&args[0], None)?;
                let vt = v.tipo.clone();
                match &vt {
                    CType::Arreglo(elem, largo) => {
                        let inner = elem.as_ref().clone();
                        let t_r = CType::Vec(Box::new(inner.clone()));
                        if let Some(e) = esp {
                            if e != &t_r {
                                return Err(self.err_en(
                                    "C0005",
                                    format!("se esperaba `{}`, da `Vec`", self.muestra(e)),
                                    x,
                                ));
                            }
                        }
                        self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                        let at = if v.arreglo.is_some() {
                            let (tmp, _) = self.aloja_arreglo(v)?;
                            tmp
                        } else if v.movible.is_some() || v.lugar.is_some() {
                            v.c.clone()
                        } else {
                            let t = self.temp(vt.clone())?;
                            self.emite(format!("{t} = ({});", v.c));
                            t
                        };
                        let n_c = largo.deletrea();
                        let se = inner.deletrea().map_err(|e| self.con_archivo(e))?;
                        let tr = self.temp(t_r.clone())?;
                        self.emite(format!("({tr}).largo = ({n_c});"));
                        self.emite(format!("({tr}).capacidad = ({n_c});"));
                        self.emite(format!(
                            "({tr}).datos = (({n_c}) ? malloc((({n_c}) * sizeof({se}))) : NULL);"
                        ));
                        self.emite(format!(
                            "if ((({n_c}) != 0) && (({tr}).datos == NULL)) {{ KAMI_PANICO(\"sin memoria\"); }}"
                        ));
                        let idx = format!("i{}", self.etiqueta_n);
                        self.etiqueta_n += 1;
                        self.emite(format!("for (size_t {idx} = 0; {idx} < (size_t)({n_c}); ++{idx}) {{"));
                        // Dueños: se clona (el arreglo sigue vivo); Copy: memcpy directo.
                        if self.necesita_drop(&inner) {
                            let te = self.clona_a_temp(format!("(({at})[{idx}])"), None, &inner)?;
                            if matches!(inner, CType::Arreglo(_, _)) {
                                self.emite(format!("memcpy((({tr}).datos[{idx}]), ({te}), sizeof((({tr}).datos[{idx}])));"));
                            } else {
                                self.emite(format!("(({tr}).datos[{idx}]) = ({te});"));
                            }
                            self.marca_movida_si_temp_pub(&te);
                        } else if matches!(inner, CType::Arreglo(_, _)) {
                            self.emite(format!("memcpy((({tr}).datos[{idx}]), (({at})[{idx}]), sizeof((({tr}).datos[{idx}])));"));
                        } else {
                            self.emite(format!("(({tr}).datos[{idx}]) = ((({at})[{idx}]));"));
                        }
                        self.emite("}".to_string());
                        Ok(Some(ExVal::movible(tr.clone(), t_r, tr)))
                    }
                    CType::Rebana(elem, _) => {
                        let inner = elem.as_ref().clone();
                        let t_r = CType::Vec(Box::new(inner.clone()));
                        if let Some(e) = esp {
                            if e != &t_r {
                                return Err(self.err_en(
                                    "C0005",
                                    format!("se esperaba `{}`, da `Vec`", self.muestra(e)),
                                    x,
                                ));
                            }
                        }
                        self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                        let bt = if v.movible.is_some() || v.lugar.is_some() {
                            v.c.clone()
                        } else {
                            let t = self.temp(vt.clone())?;
                            self.emite(format!("{t} = ({});", v.c));
                            t
                        };
                        let se = inner.deletrea().map_err(|e| self.con_archivo(e))?;
                        let tr = self.temp(t_r.clone())?;
                        self.emite(format!("({tr}).largo = (({bt}).largo);"));
                        self.emite(format!("({tr}).capacidad = (({bt}).largo);"));
                        self.emite(format!(
                            "({tr}).datos = ((({bt}).largo) ? malloc((((({bt}).largo)) * sizeof({se}))) : NULL);"
                        ));
                        self.emite(format!(
                            "if (((({bt}).largo) != 0) && (({tr}).datos == NULL)) {{ KAMI_PANICO(\"sin memoria\"); }}"
                        ));
                        let idx = format!("i{}", self.etiqueta_n);
                        self.etiqueta_n += 1;
                        self.emite(format!(
                            "for (size_t {idx} = 0; {idx} < (({bt}).largo); ++{idx}) {{"
                        ));
                        if self.necesita_drop(&inner) {
                            let te = self.clona_a_temp(format!("(({bt}).datos[{idx}])"), None, &inner)?;
                            if matches!(inner, CType::Arreglo(_, _)) {
                                self.emite(format!("memcpy((({tr}).datos[{idx}]), ({te}), sizeof((({tr}).datos[{idx}])));"));
                            } else {
                                self.emite(format!("(({tr}).datos[{idx}]) = ({te});"));
                            }
                            self.marca_movida_si_temp_pub(&te);
                        } else if matches!(inner, CType::Arreglo(_, _)) {
                            self.emite(format!("memcpy((({tr}).datos[{idx}]), (({bt}).datos[{idx}]), sizeof((({tr}).datos[{idx}])));"));
                        } else {
                            self.emite(format!("(({tr}).datos[{idx}]) = (((({bt}).datos[{idx}])));"));
                        }
                        self.emite("}".to_string());
                        Ok(Some(ExVal::movible(tr.clone(), t_r, tr)))
                    }
                    CType::Ref(_, a) if matches!(a.as_ref(), CType::Arreglo(_, _)) => {
                        let (elem, largo) = match a.as_ref() {
                            CType::Arreglo(e, l) => (e.as_ref().clone(), l.clone()),
                            _ => unreachable!(),
                        };
                        let inner = elem;
                        let t_r = CType::Vec(Box::new(inner.clone()));
                        if let Some(e) = esp {
                            if e != &t_r {
                                return Err(self.err_en(
                                    "C0005",
                                    format!("se esperaba `{}`, da `Vec`", self.muestra(e)),
                                    x,
                                ));
                            }
                        }
                        self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                        let c = self.consume(v, None)?;
                        let n_c = largo.deletrea();
                        let se = inner.deletrea().map_err(|e| self.con_archivo(e))?;
                        let tr = self.temp(t_r.clone())?;
                        self.emite(format!("({tr}).largo = ({n_c});"));
                        self.emite(format!("({tr}).capacidad = ({n_c});"));
                        self.emite(format!(
                            "({tr}).datos = (({n_c}) ? malloc((({n_c}) * sizeof({se}))) : NULL);"
                        ));
                        self.emite(format!(
                            "if ((({n_c}) != 0) && (({tr}).datos == NULL)) {{ KAMI_PANICO(\"sin memoria\"); }}"
                        ));
                        let idx = format!("i{}", self.etiqueta_n);
                        self.etiqueta_n += 1;
                        self.emite(format!("for (size_t {idx} = 0; {idx} < (size_t)({n_c}); ++{idx}) {{"));
                        // Prestado: siempre se clona lo que pide drop.
                        if self.necesita_drop(&inner) {
                            let te = self.clona_a_temp(format!("(((*({c})))[{idx}])"), None, &inner)?;
                            if matches!(inner, CType::Arreglo(_, _)) {
                                self.emite(format!("memcpy((({tr}).datos[{idx}]), ({te}), sizeof((({tr}).datos[{idx}])));"));
                            } else {
                                self.emite(format!("(({tr}).datos[{idx}]) = ({te});"));
                            }
                            self.marca_movida_si_temp_pub(&te);
                        } else if matches!(inner, CType::Arreglo(_, _)) {
                            self.emite(format!("memcpy((({tr}).datos[{idx}]), (((*({c})))[{idx}]), sizeof((({tr}).datos[{idx}])));"));
                        } else {
                            self.emite(format!("(({tr}).datos[{idx}]) = (((((*({c})))[{idx}])));"));
                        }
                        self.emite("}".to_string());
                        Ok(Some(ExVal::movible(tr.clone(), t_r, tr)))
                    }
                    CType::Texto => {
                        let t_r = CType::Vec(Box::new(CType::U8));
                        if let Some(e) = esp {
                            if e != &t_r {
                                return Err(self.err_en(
                                    "C0005",
                                    format!("se esperaba `{}`, da `Vec<u8>`", self.muestra(e)),
                                    x,
                                ));
                            }
                        }
                        self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                        let c = self.consume(v, Some(&CType::Texto))?;
                        let tr = self.temp(t_r.clone())?;
                        self.emite(format!("({tr}).datos = (({c}).datos);"));
                        self.emite(format!("({tr}).largo = (({c}).largo);"));
                        self.emite(format!("({tr}).capacidad = (({c}).capacidad);"));
                        self.marca_movida_si_temp_pub(&c);
                        Ok(Some(ExVal::movible(tr.clone(), t_r, tr)))
                    }
                    CType::Vec(i2) => {
                        let t_r = CType::Vec(i2.clone());
                        if let Some(e) = esp {
                            if e != &t_r {
                                return Err(self.err_en(
                                    "C0005",
                                    format!("se esperaba `{}`, da `Vec`", self.muestra(e)),
                                    x,
                                ));
                            }
                        }
                        self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                        let c = self.consume(v, Some(&t_r))?;
                        let tr = self.temp(t_r.clone())?;
                        self.emite(format!("({tr}).datos = (({c}).datos);"));
                        self.emite(format!("({tr}).largo = (({c}).largo);"));
                        self.emite(format!("({tr}).capacidad = (({c}).capacidad);"));
                        self.marca_movida_si_temp_pub(&c);
                        Ok(Some(ExVal::movible(tr.clone(), t_r, tr)))
                    }
                    _ => Err(self.err_en(
                        "C0005",
                        format!("`Vec::from` no sale de `{}`", self.muestra(&vt)),
                        &args[0],
                    )
                    .ayuda("usa arreglo, rebanada, `String` o `Vec`")),
                }
            }
            ("Vec", "from_raw_parts") => {
                if args.len() != 3 {
                    return Err(self.err_en("C0005", "`Vec::from_raw_parts` lleva puntero, largo y capacidad".into(), x));
                }
                let inner = match esp {
                    Some(CType::Vec(i)) => i.as_ref().clone(),
                    Some(e) => {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `Vec`", self.muestra(e)),
                            x,
                        ))
                    }
                    None => {
                        return Err(self.err_en(
                            "C0005",
                            "`Vec::from_raw_parts` necesita tipo esperado (anota el `let`)".into(),
                            x,
                        ))
                    }
                };
                let t_r = CType::Vec(Box::new(inner.clone()));
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let t_p = CType::Ptr(true, Box::new(inner));
                let v0 = self.baja_expr(&args[0], Some(&t_p))?;
                let v1 = self.baja_expr(&args[1], Some(&CType::Usize))?;
                let v2 = self.baja_expr(&args[2], Some(&CType::Usize))?;
                let c0 = self.consume(v0, Some(&t_p))?;
                let c1 = self.consume(v1, Some(&CType::Usize))?;
                let c2 = self.consume(v2, Some(&CType::Usize))?;
                let tr = self.temp(t_r.clone())?;
                self.emite(format!("({tr}).datos = ({c0});"));
                self.emite(format!("({tr}).largo = ({c1});"));
                self.emite(format!("({tr}).capacidad = ({c2});"));
                Ok(Some(ExVal::movible(tr.clone(), t_r, tr)))
            }
            ("Vec", "from_iter") => {
                let mut e = self.err_en("C0003", "`Vec::from_iter` pide un iterador".into(), x);
                e.ayuda = Some("llena con `push` en un `for`".into());
                Err(e)
            }
            ("bool", "then") => {
                let mut e = self.err_en("C0003", "`bool::then` pide una clausura".into(), x);
                e.ayuda = Some("usa `if` o `.then_some()`".into());
                Err(e)
            }
            _ if matches!(
                t0,
                "Rc" | "Weak" | "Arc" | "Cell" | "RefCell" | "OnceCell" | "OnceLock" | "LazyLock"
                    | "Mutex" | "RwLock" | "AtomicBool" | "AtomicUsize" | "AtomicIsize"
                    | "AtomicI8" | "AtomicI16" | "AtomicI32" | "AtomicI64" | "AtomicU8"
                    | "AtomicU16" | "AtomicU32" | "AtomicU64" | "AtomicPtr" | "Ordering"
            ) =>
            {
                let mut e = self.err_en("C0003", format!("`{t0}::{m}` necesita hilos o interior mutables"), x);
                e.ayuda = Some("ese tipo no tiene soporte en C".into());
                Err(e)
            }
            _ if matches!(
                t0,
                "HashMap" | "HashSet" | "BTreeMap" | "BTreeSet" | "VecDeque" | "LinkedList"
                    | "BinaryHeap"
            ) =>
            {
                let mut e = self.err_en("C0003", format!("`{t0}::{m}`: colección sin soporte"), x);
                e.ayuda = Some("usa `Vec` con búsqueda lineal".into());
                Err(e)
            }
            _ if matches!(
                t0,
                "CString" | "CStr" | "Duration" | "Instant" | "SystemTime" | "Error" | "PhantomData"
                    | "Any" | "TypeId" | "Wrapping" | "Saturating"
            ) =>
            {
                let mut e = self.err_en("C0003", format!("`{t0}::{m}` no tiene soporte"), x);
                e.ayuda = Some("ese tipo no cabe en C".into());
                Err(e)
            }
            _ => Ok(None),
        }
    }

    /// ¿Nombre de tipo numérico (`T::from`)?
    fn es_ent_num(t0: &str) -> bool {
        matches!(
            t0,
            "i8" | "i16" | "i32" | "i64" | "isize" | "u8" | "u16" | "u32" | "u64" | "usize" | "f32"
                | "f64"
        )
    }

    // ==================================================================
    // Métodos
    // ==================================================================

    /// `recv.metodo(args)` con autoderef.
    fn ex_methodcall(
        &mut self,
        x: &syn::ExprMethodCall,
        esp: Option<&CType>,
    ) -> Result<ExVal, CError> {
        let m = x.method.to_string();
        let b = self.baja_expr(&x.receiver, None)?;
        // Métodos inherentes de `Box` ganan al pelar (`b.as_ref()` es de la caja).
        if matches!(b.tipo, CType::Caja(_)) && matches!(m.as_str(), "as_ref" | "as_mut") {
            return self.ex_metodo_caja(b, &m, esp, x);
        }
        let (base, d, mut_ext) = Self::pela(&b.tipo);
        // 1. Usuario (inherente o rasgo).
        if let CType::Usuario(n) = &base {
            if let Some(sig) = self.resuelve_metodo(n, &m, x)? {
                if x.turbofish.is_some() {
                    return Err(self.err_en("C0003", "métodos sin genéricos".into(), x)
                        .ayuda("quita el `::<...>`"));
                }
                return self.llama_metodo_user(b, &base, d, mut_ext, &sig, &x.args, esp, x);
            }
            return Err(self.err_en("C0004", format!("`{n}` no tiene método `{m}`"), x)
                .ayuda("¿lo definiste en un `impl`?"));
        }
        // 2. Std según el tipo base.
        let tb = self.tipos_turbo_met(x)?;
        if let Some(v) = self.ex_metodo_std(b, &base, d, mut_ext, &m, &tb, &x.args, esp, x)? {
            return Ok(v);
        }
        Err(self.err_en(
            "C0004",
            format!("`{}` no tiene método `{m}`", self.muestra(&base)),
            x,
        ))
    }

    /// Pela `&`/`Box` del receptor: (tipo base, nº de pasos, `mut` del paso externo).
    fn pela(tipo: &CType) -> (CType, usize, Option<bool>) {
        let mut d = 0;
        let mut ext = None;
        let mut t = tipo;
        loop {
            match t {
                CType::Ref(mt, inner) => {
                    if d == 0 {
                        ext = Some(*mt);
                    }
                    d += 1;
                    t = inner;
                }
                CType::Caja(inner) => {
                    d += 1;
                    t = inner;
                }
                _ => return (t.clone(), d, ext),
            }
        }
    }

    /// Tipos del turbofish de un método (`s.parse::<i32>()`).
    fn tipos_turbo_met(&self, x: &syn::ExprMethodCall) -> Result<Vec<CType>, CError> {
        let mut ts = Vec::new();
        if let Some(ab) = &x.turbofish {
            for a in &ab.args {
                match a {
                    syn::GenericArgument::Type(t) => {
                        ts.push(self.tipo_de_turbo_met(t, x)?);
                    }
                    syn::GenericArgument::Lifetime(_) => {}
                    _ => return Err(self.err_en("C0003", "ese argumento genérico no cabe".into(), x)),
                }
            }
        }
        Ok(ts)
    }

    /// `syn::Type` → `CType` para turbofish de método (reusa el de llamada).
    fn tipo_de_turbo_met(&self, t: &syn::Type, x: &syn::ExprMethodCall) -> Result<CType, CError> {
        // Mismo conversor que en llamadas (el span solo importa si falla).
        match t {
            syn::Type::Group(g) => self.tipo_de_turbo_met(&g.elem, x),
            syn::Type::Paren(p) => self.tipo_de_turbo_met(&p.elem, x),
            syn::Type::Infer(_) => Err(self.err_en("C0003", "anota el tipo del turbofish".into(), x)),
            syn::Type::Tuple(tp) => {
                if tp.elems.is_empty() {
                    return Ok(CType::Vacio);
                }
                let mut cs = Vec::new();
                for e in &tp.elems {
                    cs.push(self.tipo_de_turbo_met(e, x)?);
                }
                Ok(CType::Tupla(cs))
            }
            syn::Type::Path(tp) if tp.qself.is_none() => {
                let segs: Vec<String> =
                    tp.path.segments.iter().map(|s| s.ident.to_string()).collect();
                let ult = tp.path.segments.last().unwrap();
                if let syn::PathArguments::AngleBracketed(ab) = &ult.arguments {
                    let mut inner = Vec::new();
                    for a in &ab.args {
                        match a {
                            syn::GenericArgument::Type(t2) => {
                                inner.push(self.tipo_de_turbo_met(t2, x)?)
                            }
                            syn::GenericArgument::Lifetime(_) => {}
                            _ => {
                                return Err(self.err_en("C0003", "ese argumento genérico no cabe".into(), x))
                            }
                        }
                    }
                    let nb = ult.ident.to_string();
                    if (nb == "Vec" || nb == "Option") && inner.len() == 1 {
                        let v = inner.remove(0);
                        if nb == "Vec" {
                            return Ok(CType::Vec(Box::new(v)));
                        }
                        return Ok(CType::Opcion(Box::new(v)));
                    }
                    if nb == "Result" && inner.len() == 2 {
                        let e = inner.remove(1);
                        let v = inner.remove(0);
                        return Ok(CType::Resultado(Box::new(v), Box::new(e)));
                    }
                    return Err(self.err_en("C0003", format!("`{nb}` con genéricos no cabe aquí"), x));
                }
                if segs == ["Self"] {
                    match &self.tipo_self {
                        Some(n) => return Ok(CType::Usuario(n.clone())),
                        None => return Err(self.err_en("C0003", "`Self` fuera de `impl`".into(), x)),
                    }
                }
                if segs.len() == 1 {
                    let t = match segs[0].as_str() {
                        "bool" => Some(CType::Bool),
                        "char" => Some(CType::Char),
                        "i8" => Some(CType::I8),
                        "i16" => Some(CType::I16),
                        "i32" => Some(CType::I32),
                        "i64" => Some(CType::I64),
                        "isize" => Some(CType::Isize),
                        "u8" => Some(CType::U8),
                        "u16" => Some(CType::U16),
                        "u32" => Some(CType::U32),
                        "u64" => Some(CType::U64),
                        "usize" => Some(CType::Usize),
                        "f32" => Some(CType::F32),
                        "f64" => Some(CType::F64),
                        "String" => Some(CType::Texto),
                        _ => None,
                    };
                    if let Some(t) = t {
                        return Ok(t);
                    }
                    if segs[0] == "str" {
                        return Err(self.err_en("C0003", "`str` no tiene tamaño".into(), x));
                    }
                }
                if let Some(nc) = self.resuelve_tipo_pub(&segs, tp.path.leading_colon.is_some()) {
                    return Ok(CType::Usuario(nc));
                }
                Err(self.err_en("C0003", format!("tipo `{}` desconocido", segs.join("::")), x))
            }
            _ => Err(self.err_en("C0003", "ese tipo no cabe en turbofish".into(), x)
                .ayuda("usa un tipo con nombre, primitivo o tupla")),
        }
    }

    /// Método de usuario/rasgo para un tipo (llamada con `.`).
    fn resuelve_metodo(
        &self,
        tipo_n: &str,
        m: &str,
        x: &syn::ExprMethodCall,
    ) -> Result<Option<super::baja::SigMetodo>, CError> {
        if let Some(sig) = self.metodos.get(&(tipo_n.to_string(), m.to_string())) {
            return Ok(Some(sig.clone()));
        }
        let mut cands: Vec<(String, super::baja::SigMetodo)> = Vec::new();
        for ((r, t), mapa) in &self.impl_rasgos {
            if t == tipo_n {
                if let Some(sig) = mapa.get(m) {
                    cands.push((r.clone(), sig.clone()));
                }
            }
        }
        if cands.len() == 1 {
            let (r, mut sig2) = cands.into_iter().next().unwrap();
            sig2.params = sig2
                .params
                .iter()
                .map(|(n, t)| (n.clone(), super::baja::sustituye_self(t, &r, tipo_n)))
                .collect();
            sig2.ret = super::baja::sustituye_self(&sig2.ret, &r, tipo_n);
            return Ok(Some(sig2));
        } else if cands.len() > 1 {
            return Err(self.err_en(
                "C0007",
                format!("`{m}` está en varios rasgos para `{tipo_n}`"),
                x,
            )
            .ayuda(format!("desambigua: `<{tipo_n} as Rasgo>::{m}(...)`")));
        }
        Ok(None)
    }

    /// Emite la llamada a un método de usuario ya resuelto.
    fn llama_metodo_user(
        &mut self,
        b: ExVal,
        base: &CType,
        d: usize,
        mut_ext: Option<bool>,
        sig: &super::baja::SigMetodo,
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        esp: Option<&CType>,
        x: &syn::ExprMethodCall,
    ) -> Result<ExVal, CError> {
        if matches!(sig.receptor, super::baja::Receptor::Ninguno) {
            return Err(self.err_en(
                "C0003",
                format!("`{}` es asociada; llámala como `Tipo::{}()`", sig.nombre_c, x.method),
                x,
            ));
        }
        if sig.params.len() != args.len() {
            return Err(self.err_en(
                "C0005",
                format!("`{}` pide {}, llegaron {}", x.method, sig.params.len(), args.len()),
                x,
            ));
        }
        let rc = self.ajusta_receptor(b, base, d, mut_ext, sig.receptor, x)?;
        let mut cs = vec![rc];
        for (a, (_, pt)) in args.iter().zip(sig.params.iter()) {
            cs.push(self.conv_arg(a, pt)?);
        }
        let t = sig.ret.clone();
        if let Some(e) = esp {
            if e != &t {
                return Err(self.err_en(
                    "C0005",
                    format!(
                        "se esperaba `{}`, `.{}()` da `{}`",
                        self.muestra(e),
                        x.method,
                        self.muestra(&t)
                    ),
                    x,
                ));
            }
        }
        Ok(ExVal::puro(format!("({}({}))", sig.nombre_c, cs.join(", ")), t))
    }

    /// Pasa el receptor según `Receptor` (prestar no marca; mover sí).
    fn ajusta_receptor(
        &mut self,
        b: ExVal,
        base: &CType,
        d: usize,
        mut_ext: Option<bool>,
        r: super::baja::Receptor,
        x: &syn::ExprMethodCall,
    ) -> Result<String, CError> {
        match r {
            super::baja::Receptor::Ninguno => {
                Err(self.err_en("C0002", "método sin receptor (bug interno)".into(), x))
            }
            super::baja::Receptor::Valor => {
                if d == 0 {
                    if b.movible.is_none() && b.lugar.is_some() && self.necesita_drop(&b.tipo) {
                        return Err(self.err_en(
                            "C0008",
                            "ese lugar no se puede mover al método".into(),
                            &x.receiver,
                        ));
                    }
                    return self.consume(b, None);
                }
                if self.necesita_drop(base) {
                    return Err(self.err_en(
                        "C0008",
                        "no se puede mover desde una desreferencia".into(),
                        &x.receiver,
                    ));
                }
                let mut c = b.c;
                for _ in 0..d {
                    c = format!("(*({c}))");
                }
                Ok(c)
            }
            super::baja::Receptor::Ref | super::baja::Receptor::Mut => {
                if r == super::baja::Receptor::Mut && mut_ext == Some(false) {
                    return Err(self.err_en(
                        "C0003",
                        format!("`.{}()` pide `&mut`", x.method),
                        &x.receiver,
                    ));
                }
                if d == 0 {
                    if b.movible.is_some() || b.lugar.is_some() {
                        return Ok(format!("&({})", b.c));
                    }
                    let t = self.temp(base.clone())?;
                    self.emite(format!("{t} = ({c});", c = b.c));
                    self.marca_movida_si_temp_pub(&b.c);
                    return Ok(format!("&{t}"));
                }
                let mut c = b.c;
                for _ in 1..d {
                    c = format!("(*({c}))");
                }
                Ok(c)
            }
        }
    }

    /// `as_ref` / `as_mut` inherentes de `Box` (la caja YA es puntero).
    fn ex_metodo_caja(
        &mut self,
        b: ExVal,
        m: &str,
        esp: Option<&CType>,
        x: &syn::ExprMethodCall,
    ) -> Result<ExVal, CError> {
        if !x.args.is_empty() {
            return Err(self.err_en("C0005", format!("`.{m}()` no lleva argumentos"), x));
        }
        let inner = match &b.tipo {
            CType::Caja(p) => (**p).clone(),
            _ => return Err(self.err_en("C0002", "caja esperada (bug interno)".into(), x)),
        };
        let t = CType::Ref(m == "as_mut", Box::new(inner));
        if let Some(e) = esp {
            if e != &t {
                return Err(self.err_en(
                    "C0005",
                    format!("se esperaba `{}`, `.{m}()` da otro préstamo", self.muestra(e)),
                    x,
                ));
            }
        }
        Ok(ExVal::puro(b.c, t))
    }

    /// `T::metodo(recv, ...)` (asociada con receptor explícito).
    fn ex_call_asociada_ruta(
        &mut self,
        ruta: &[String],
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        esp: Option<&CType>,
        x: &syn::ExprCall,
    ) -> Result<Option<ExVal>, CError> {
        if ruta.len() < 2 {
            return Ok(None);
        }
        let tipo_m = ruta[..ruta.len() - 1].join("__");
        let m = &ruta[ruta.len() - 1];
        if let Some(sig) = self.metodos.get(&(tipo_m.clone(), m.clone())) {
            let sig = sig.clone();
            let nom_q = format!("{}::{m}", ruta[..ruta.len() - 1].join("::"));
            return self.llama_asociada(&sig, &nom_q, args, esp, x).map(Some);
        }
        let mut cands: Vec<(String, super::baja::SigMetodo)> = Vec::new();
        for ((r, t), mapa) in &self.impl_rasgos {
            if t == &tipo_m {
                if let Some(sig) = mapa.get(m) {
                    cands.push((r.clone(), sig.clone()));
                }
            }
        }
        if cands.len() == 1 {
            let (r, mut sig2) = cands.into_iter().next().unwrap();
            sig2.params = sig2
                .params
                .iter()
                .map(|(n, t)| (n.clone(), super::baja::sustituye_self(t, &r, &tipo_m)))
                .collect();
            sig2.ret = super::baja::sustituye_self(&sig2.ret, &r, &tipo_m);
            let nom_q = format!("{}::{m}", ruta[..ruta.len() - 1].join("::"));
            return self.llama_asociada(&sig2, &nom_q, args, esp, x).map(Some);
        } else if cands.len() > 1 {
            return Err(self.err_en(
                "C0007",
                format!("`{m}` está en varios rasgos para `{tipo_m}`"),
                x,
            )
            .ayuda(format!("desambigua: `<{tipo_m} as Rasgo>::{m}(...)`")));
        }
        Ok(None)
    }

    /// Emite `T::m(recv?, args)` ya resuelto (asociada o UFCS).
    fn llama_asociada(
        &mut self,
        sig: &super::baja::SigMetodo,
        nom_q: &str,
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        esp: Option<&CType>,
        x: &syn::ExprCall,
    ) -> Result<ExVal, CError> {
        use super::baja::Receptor::*;
        let n_espera = sig.params.len() + if matches!(sig.receptor, Ninguno) { 0 } else { 1 };
        if n_espera != args.len() {
            return Err(self.err_en(
                "C0005",
                format!("`{nom_q}` pide {}, llegaron {}", n_espera, args.len()),
                x,
            ));
        }
        let mut cs = Vec::new();
        let mut it = args.iter();
        match sig.receptor {
            Ninguno => {}
            Valor => {
                let a = it.next().unwrap();
                let sp = a.clone();
                let b = self.baja_expr(a, None)?;
                let (base, d, _) = Self::pela(&b.tipo);
                if d == 0 {
                    if b.movible.is_none() && b.lugar.is_some() && self.necesita_drop(&b.tipo) {
                        return Err(self.err_en("C0008", "ese lugar no se puede mover".into(), &sp));
                    }
                    cs.push(self.consume(b, None)?);
                } else {
                    if self.necesita_drop(&base) {
                        return Err(self.err_en(
                            "C0008",
                            "no se puede mover desde una desreferencia".into(),
                            &sp,
                        ));
                    }
                    let mut c = b.c;
                    for _ in 0..d {
                        c = format!("(*({c}))");
                    }
                    cs.push(c);
                }
            }
            Ref | Mut => {
                let a = it.next().unwrap();
                let sp = a.clone();
                let b = self.baja_expr(a, None)?;
                let (base, d, mut_ext) = Self::pela(&b.tipo);
                if matches!(sig.receptor, Mut) && mut_ext == Some(false) {
                    return Err(self.err_en("C0003", format!("`{nom_q}` pide `&mut`"), &sp));
                }
                if d == 0 {
                    if b.movible.is_some() || b.lugar.is_some() {
                        cs.push(format!("&({})", b.c));
                    } else {
                        let t = self.temp(base.clone())?;
                        self.emite(format!("{t} = ({c});", c = b.c));
                        self.marca_movida_si_temp_pub(&b.c);
                        cs.push(format!("&{t}"));
                    }
                } else {
                    let mut c = b.c;
                    for _ in 1..d {
                        c = format!("(*({c}))");
                    }
                    cs.push(c);
                }
            }
        }
        for (a, (_, pt)) in it.zip(sig.params.iter()) {
            cs.push(self.conv_arg(a, pt)?);
        }
        let t = sig.ret.clone();
        if let Some(e) = esp {
            if e != &t {
                return Err(self.err_en(
                    "C0005",
                    format!("se esperaba `{}`, `{nom_q}` da `{}`", self.muestra(e), self.muestra(&t)),
                    x,
                ));
            }
        }
        Ok(ExVal::puro(format!("({}({}))", sig.nombre_c, cs.join(", ")), t))
    }

    /// `<T as Rasgo>::metodo(recv, ...)` (UFCS).
    fn ex_call_qself(
        &mut self,
        p: &syn::ExprPath,
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        esp: Option<&CType>,
        x: &syn::ExprCall,
    ) -> Result<ExVal, CError> {
        let q = p.qself.as_ref().unwrap();
        let segs: Vec<&syn::PathSegment> = p.path.segments.iter().collect();
        if segs.len() < 2 {
            return Err(self.err_en("C0003", "UFCS raro: `<T as R>::m()`".into(), x));
        }
        let m = segs[segs.len() - 1].ident.to_string();
        if matches!(segs[segs.len() - 1].arguments, syn::PathArguments::AngleBracketed(_)) {
            return Err(self.err_en("C0003", "métodos sin genéricos".into(), x));
        }
        let mut tt: &syn::Type = &q.ty;
        loop {
            match tt {
                syn::Type::Group(g) => tt = &g.elem,
                syn::Type::Paren(pa) => tt = &pa.elem,
                _ => break,
            }
        }
        let tipo_m = match tt {
            syn::Type::Path(tp) if tp.qself.is_none() => {
                let ss: Vec<String> =
                    tp.path.segments.iter().map(|s| s.ident.to_string()).collect();
                if tp.path.segments.iter().any(|s| !matches!(s.arguments, syn::PathArguments::None))
                {
                    return Err(self.err_en("C0003", "UFCS solo sobre tipos con nombre".into(), x));
                }
                if ss == ["Self"] {
                    match &self.tipo_self {
                        Some(n) => n.clone(),
                        None => return Err(self.err_en("C0003", "`Self` fuera de `impl`".into(), x)),
                    }
                } else {
                    self.ruta_abs(&ss, tp.path.leading_colon.is_some()).join("__")
                }
            }
            _ => return Err(self.err_en("C0003", "UFCS solo sobre tipos con nombre".into(), x)),
        };
        let rs: Vec<String> = segs[..segs.len() - 1].iter().map(|s| s.ident.to_string()).collect();
        let rasgo_m = self.ruta_abs(&rs, p.path.leading_colon.is_some()).join("__");
        let mapa = self.impl_rasgos.get(&(rasgo_m.clone(), tipo_m.clone()));
        match mapa.and_then(|mp| mp.get(&m)) {
            Some(sig) => {
                let mut sig2 = sig.clone();
                sig2.params = sig2
                    .params
                    .iter()
                    .map(|(n, t)| (n.clone(), super::baja::sustituye_self(t, &rasgo_m, &tipo_m)))
                    .collect();
                sig2.ret = super::baja::sustituye_self(&sig2.ret, &rasgo_m, &tipo_m);
                let nom_q = format!("<{tipo_m} as {rasgo_m}>::{m}");
                self.llama_asociada(&sig2, &nom_q, args, esp, x)
            }
            None => Err(self.err_en(
                "C0004",
                format!("`<{tipo_m} as {rasgo_m}>::{m}` no existe"),
                x,
            )),
        }
    }

    /// Métodos std según el tipo base. `None` = ese tipo no tiene ese método.
    #[allow(clippy::too_many_arguments)]
    fn ex_metodo_std(
        &mut self,
        b: ExVal,
        base: &CType,
        d: usize,
        mut_ext: Option<bool>,
        m: &str,
        tb: &[CType],
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        esp: Option<&CType>,
        x: &syn::ExprMethodCall,
    ) -> Result<Option<ExVal>, CError> {
        match base {
            CType::Opcion(_) => self.ex_opcion(b, base, d, mut_ext, m, args, esp, x),
            CType::Resultado(_, _) => self.ex_resultado(b, base, d, mut_ext, m, args, esp, x),
            CType::Texto => self.ex_texto(b, base, d, mut_ext, m, tb, args, esp, x),
            CType::VistaTexto => self.ex_vista(b, base, d, m, tb, args, esp, x),
            CType::Vec(_) => self.ex_vec(b, base, d, mut_ext, m, args, esp, x),
            CType::Rebana(_, _) => self.ex_rebana(b, base, d, m, args, esp, x),
            CType::Arreglo(_, _) => self.ex_arreglo(b, base, d, m, args, esp, x),
            CType::Ref(_, _) | CType::Ptr(_, _) => self.ex_puntero(b, base, d, m, args, esp, x),
            CType::Bool => self.ex_bool_met(b, base, d, m, args, esp, x),
            CType::Char => self.ex_caracter(b, base, d, m, tb, args, esp, x),
            t if t.es_numero() => self.ex_num(b, base, d, m, tb, args, esp, x),
            _ => Ok(None),
        }
    }

    /// Lugar prestado del receptor: sitio, hoist o estrellas (autoderef).
    fn recv_lugar(&mut self, b: ExVal, base: &CType, d: usize) -> Result<String, CError> {
        if b.arreglo.is_some() {
            let (tmp, _) = self.aloja_arreglo(b)?;
            return Ok(tmp);
        }
        if d > 0 {
            let mut c = b.c;
            for _ in 0..d {
                c = format!("(*({c}))");
            }
            return Ok(c);
        }
        if b.movible.is_some() || b.lugar.is_some() {
            return Ok(b.c);
        }
        let t = self.temp(base.clone())?;
        self.emite(format!("{t} = ({c});", c = b.c));
        self.marca_movida_si_temp_pub(&b.c);
        Ok(t)
    }

    /// Saca un campo de un dueño consumido (`o.unwrap()`...): sin dueño es prvalue.
    fn mueve_campo(
        &mut self,
        c_dueno: String,
        dueno_var: Option<String>,
        campo: &str,
        t_campo: CType,
    ) -> ExVal {
        let _ = self;
        let c = format!("({c_dueno}).{campo}");
        match dueno_var {
            Some(n) => ExVal::movible(c, t_campo, n),
            None => ExVal::puro(c, t_campo),
        }
    }

    /// El receptor por valor debe ser movible (no lugar prestado droppable, no `&`).
    fn recv_movible(
        &mut self,
        b: ExVal,
        d: usize,
        m: &str,
        x: &syn::ExprMethodCall,
    ) -> Result<(String, Option<String>), CError> {
        if d > 0 {
            return Err(self.err_en(
                "C0008",
                format!("`.{m}()` mueve; no vale sobre préstamo"),
                &x.receiver,
            ));
        }
        if b.movible.is_none() && b.lugar.is_some() && self.necesita_drop(&b.tipo) {
            return Err(self.err_en(
                "C0008",
                format!("`.{m}()` mueve; ese lugar no se puede mover"),
                &x.receiver,
            ));
        }
        let dueno = b.movible.clone();
        let c = self.consume(b, None)?;
        Ok((c, dueno))
    }

    /// Métodos de `Option` (núcleo; el resto llega en el siguiente tramo).
    fn ex_opcion(
        &mut self,
        b: ExVal,
        base: &CType,
        d: usize,
        _mut_ext: Option<bool>,
        m: &str,
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        esp: Option<&CType>,
        x: &syn::ExprMethodCall,
    ) -> Result<Option<ExVal>, CError> {
        let inner = match base {
            CType::Opcion(p) => (**p).clone(),
            _ => return Ok(None),
        };
        match m {
            "is_some" | "is_none" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", format!("`.{m}()` no lleva argumentos"), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Bool {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da `bool`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let pl = self.recv_lugar(b, base, d)?;
                let bang = if m == "is_some" { "" } else { "!" };
                Ok(Some(ExVal::puro(format!("({bang}(({pl}).tiene))"), CType::Bool)))
            }
            "unwrap" | "expect" => {
                let msg_c = if m == "expect" {
                    if args.len() != 1 {
                        return Err(self.err_en("C0005", "`.expect()` lleva un mensaje".into(), x));
                    }
                    let v = self.baja_expr(&args[0], Some(&CType::VistaTexto))?;
                    if v.tipo != CType::VistaTexto {
                        return Err(self.err_en(
                            "C0005",
                            format!("`.expect()` pide `&str`, no `{}`", self.muestra(&v.tipo)),
                            &args[0],
                        ));
                    }
                    Some(self.consume(v, Some(&CType::VistaTexto))?)
                } else {
                    if !args.is_empty() {
                        return Err(self.err_en("C0005", "`.unwrap()` no lleva argumentos".into(), x));
                    }
                    None
                };
                if let Some(e) = esp {
                    if e != &inner {
                        return Err(self.err_en(
                            "C0005",
                            format!(
                                "se esperaba `{}`, `.{m}()` da `{}`",
                                self.muestra(e),
                                self.muestra(&inner)
                            ),
                            x,
                        ));
                    }
                }
                let (c, dueno) = self.recv_movible(b, d, m, x)?;
                // Hoist si es prvalue (`(c)` se usa dos veces).
                let (c, dueno) = match dueno {
                    Some(n) => (c, Some(n)),
                    None => {
                        let t = self.temp(base.clone())?;
                        self.emite(format!("{t} = ({c});"));
                        self.marca_movida_si_temp_pub(&c);
                        self.marca_movida(&t);
                        self.activa_bandera(&t);
                        (t.clone(), Some(t))
                    }
                };
                match msg_c {
                    Some(ms) => self.emite(format!(
                        "if (!(({c}).tiene)) {{ KAMI_PANICO(\"%s (era None)\", ({ms})); }}"
                    )),
                    None => self.emite(format!("if (!(({c}).tiene)) {{ KAMI_PANICO(\"unwrap de None\"); }}")),
                }
                Ok(Some(self.mueve_campo(c, dueno, "valor", inner)))
            }
            "unwrap_or" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`.unwrap_or()` lleva un valor".into(), x));
                }
                if let Some(e) = esp {
                    if e != &inner {
                        return Err(self.err_en(
                            "C0005",
                            format!(
                                "se esperaba `{}`, `.unwrap_or()` da `{}`",
                                self.muestra(e),
                                self.muestra(&inner)
                            ),
                            x,
                        ));
                    }
                }
                let v = self.baja_expr(&args[0], Some(&inner))?;
                let cd = self.consume(v, Some(&inner))?;
                let (c, dueno) = self.recv_movible(b, d, m, x)?;
                let (c, dueno) = match dueno {
                    Some(n) => (c, Some(n)),
                    None => {
                        let t = self.temp(base.clone())?;
                        self.emite(format!("{t} = ({c});"));
                        self.marca_movida_si_temp_pub(&c);
                        self.marca_movida(&t);
                        self.activa_bandera(&t);
                        (t.clone(), Some(t))
                    }
                };
                // Truco swap: mueve el defecto siempre, libera si sobra (sin banderas).
                let td = self.temp(inner.clone())?;
                self.emite(format!("{td} = ({cd});"));
                self.marca_movida_si_temp_pub(&cd);
                let tu = self.temp(inner.clone())?;
                self.emite(format!("{tu} = ({td});"));
                self.marca_movida(&td);
                self.activa_bandera(&td);
                self.emite(format!("if (({c}).tiene) {{"));
                for l in self.libera_lugar(&tu, &inner) {
                    self.emite(format!("    {l}"));
                }
                self.emite(format!("    {tu} = ({c}).valor;"));
                self.emite("}");
                let _ = dueno;
                Ok(Some(ExVal::movible(tu.clone(), inner, tu)))
            }
            "unwrap_or_default" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.unwrap_or_default()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if e != &inner {
                        return Err(self.err_en(
                            "C0005",
                            format!(
                                "se esperaba `{}`, da `{}`",
                                self.muestra(e),
                                self.muestra(&inner)
                            ),
                            x,
                        ));
                    }
                }
                let (c, dueno) = self.recv_movible(b, d, m, x)?;
                let (c, dueno) = match dueno {
                    Some(n) => (c, Some(n)),
                    None => {
                        let t = self.temp(base.clone())?;
                        self.emite(format!("{t} = ({c});"));
                        self.marca_movida_si_temp_pub(&c);
                        self.marca_movida(&t);
                        self.activa_bandera(&t);
                        (t.clone(), Some(t))
                    }
                };
                let tu = self.temp(inner.clone())?;
                self.emite(format!("if (({c}).tiene) {{"));
                self.emite(format!("    {tu} = ({c}).valor;"));
                self.emite("} else {");
                self.defecto_a_lugar(&tu, &inner).map_err(|e| self.con_archivo(e))?;
                self.emite("}");
                let _ = dueno;
                Ok(Some(ExVal::movible(tu.clone(), inner, tu)))
            }
            "ok_or" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`.ok_or()` lleva un error".into(), x));
                }
                let guia_e = match esp {
                    Some(CType::Resultado(_, e)) => Some(&**e),
                    _ => None,
                };
                let v = self.baja_expr(&args[0], guia_e)?;
                let te = v.tipo.clone();
                let ce = self.consume(v, guia_e)?;
                let t_res = CType::Resultado(Box::new(inner.clone()), Box::new(te.clone()));
                if let Some(e) = esp {
                    if e != &t_res {
                        return Err(self.err_en(
                            "C0005",
                            format!(
                                "se esperaba `{}`, `.ok_or()` da otro `Result`",
                                self.muestra(e)
                            ),
                            x,
                        ));
                    }
                }
                self.registra_mono(&t_res).map_err(|e| self.con_archivo(e))?;
                let (c, dueno) = self.recv_movible(b, d, m, x)?;
                let (c, dueno) = match dueno {
                    Some(n) => (c, Some(n)),
                    None => {
                        let t = self.temp(base.clone())?;
                        self.emite(format!("{t} = ({c});"));
                        self.marca_movida_si_temp_pub(&c);
                        self.marca_movida(&t);
                        self.activa_bandera(&t);
                        (t.clone(), Some(t))
                    }
                };
                let te2 = self.temp(te.clone())?;
                self.emite(format!("{te2} = ({ce});"));
                self.marca_movida_si_temp_pub(&ce);
                let tr = self.temp(t_res.clone())?;
                self.emite(format!("{tr}.es_ok = false;"));
                self.emite(format!("{tr}.datos.err = ({te2});"));
                self.marca_movida(&te2);
                self.activa_bandera(&te2);
                self.emite(format!("if (({c}).tiene) {{"));
                for l in self.libera_lugar(&format!("{tr}.datos.err"), &te) {
                    self.emite(format!("    {l}"));
                }
                self.emite(format!("    {tr}.es_ok = true;"));
                self.emite(format!("    {tr}.datos.ok = ({c}).valor;"));
                self.emite("}");
                let _ = dueno;
                Ok(Some(ExVal::movible(tr.clone(), t_res, tr)))
            }
            "and" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`.and()` lleva un `Option`".into(), x));
                }
                let v = self.baja_expr(&args[0], esp)?;
                if !matches!(v.tipo, CType::Opcion(_)) {
                    return Err(self.err_en(
                        "C0005",
                        format!("`.and()` pide `Option`, no `{}`", self.muestra(&v.tipo)),
                        &args[0],
                    ));
                }
                let t_r = v.tipo.clone();
                let cb = self.consume(v, esp)?;
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.and()` da otro `Option`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let (c, dueno) = self.recv_movible(b, d, m, x)?;
                let (c, _dueno) = match dueno {
                    Some(n) => (c, Some(n)),
                    None => {
                        let t = self.temp(base.clone())?;
                        self.emite(format!("{t} = ({c});"));
                        self.marca_movida_si_temp_pub(&c);
                        self.marca_movida(&t);
                        self.activa_bandera(&t);
                        (t.clone(), Some(t))
                    }
                };
                let tb2 = self.temp(t_r.clone())?;
                self.emite(format!("{tb2} = ({cb});"));
                self.marca_movida_si_temp_pub(&cb);
                let tr = self.temp(t_r.clone())?;
                self.emite(format!("{tr} = ({tb2});"));
                self.marca_movida(&tb2);
                self.activa_bandera(&tb2);
                self.emite(format!("if (!(({c}).tiene)) {{"));
                for l in self.libera_lugar(&tr, &t_r) {
                    self.emite(format!("    {l}"));
                }
                self.emite(format!("    {tr}.tiene = false;"));
                self.emite("} else {");
                for l in self.libera_lugar(&format!("({c}).valor"), &inner) {
                    self.emite(format!("    {l}"));
                }
                self.emite("}");
                Ok(Some(ExVal::movible(tr.clone(), t_r, tr)))
            }
            "or" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`.or()` lleva un `Option`".into(), x));
                }
                let v = self.baja_expr(&args[0], Some(base))?;
                let cb = self.consume(v, Some(base))?;
                if let Some(e) = esp {
                    if e != base {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.or()` da `{}`", self.muestra(e), self.muestra(base)),
                            x,
                        ));
                    }
                }
                let (c, dueno) = self.recv_movible(b, d, m, x)?;
                let (c, dueno) = match dueno {
                    Some(n) => (c, Some(n)),
                    None => {
                        let t = self.temp(base.clone())?;
                        self.emite(format!("{t} = ({c});"));
                        self.marca_movida_si_temp_pub(&c);
                        self.marca_movida(&t);
                        self.activa_bandera(&t);
                        (t.clone(), Some(t))
                    }
                };
                let tb2 = self.temp(base.clone())?;
                self.emite(format!("{tb2} = ({cb});"));
                self.marca_movida_si_temp_pub(&cb);
                let tr = self.temp(base.clone())?;
                self.emite(format!("{tr} = ({tb2});"));
                self.marca_movida(&tb2);
                self.activa_bandera(&tb2);
                self.emite(format!("if (({c}).tiene) {{"));
                for l in self.libera_lugar(&tr, base) {
                    self.emite(format!("    {l}"));
                }
                self.emite(format!("    {tr} = ({c});"));
                self.emite("}");
                let _ = dueno;
                Ok(Some(ExVal::movible(tr.clone(), base.clone(), tr)))
            }
            "as_ref" | "as_mut" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", format!("`.{m}()` no lleva argumentos"), x));
                }
                let es_mut = m == "as_mut";
                if es_mut && !Self::presta_mut_ok(&b.tipo) {
                    return Err(self.err_en(
                        "C0003",
                        "`.as_mut()` pide `&mut` en toda la cadena".into(),
                        &x.receiver,
                    ));
                }
                let t_r = CType::Opcion(Box::new(CType::Ref(
                    es_mut,
                    Box::new(inner.clone()),
                )));
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da otro `Option`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let pl = self.recv_lugar(b, base, d)?;
                let tr = self.temp(t_r.clone())?;
                self.emite(format!("memset(&({tr}), 0, sizeof({tr}));"));
                self.emite(format!("{tr}.tiene = (({pl}).tiene);"));
                self.emite(format!("if (({pl}).tiene) {{ {tr}.valor = &(({pl}).valor); }}"));
                Ok(Some(ExVal::movible(tr.clone(), t_r, tr)))
            }
            "take" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.take()` no lleva argumentos".into(), x));
                }
                if !Self::presta_mut_ok(&b.tipo) {
                    return Err(self.err_en(
                        "C0003",
                        "`.take()` pide `&mut` en toda la cadena".into(),
                        &x.receiver,
                    ));
                }
                if let Some(e) = esp {
                    if e != base {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.take()` da `{}`", self.muestra(e), self.muestra(base)),
                            x,
                        ));
                    }
                }
                let pl = self.recv_lugar(b, base, d)?;
                let tr = self.temp(base.clone())?;
                self.emite(format!("{tr} = ({pl});"));
                self.emite(format!("({pl}).tiene = false;"));
                Ok(Some(ExVal::movible(tr.clone(), base.clone(), tr)))
            }
            _ => Ok(None),
        }
    }

    /// Métodos de `Result` (núcleo; `ok`/`err`/combinadores llegan después).
    fn ex_resultado(
        &mut self,
        b: ExVal,
        base: &CType,
        d: usize,
        _mut_ext: Option<bool>,
        m: &str,
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        esp: Option<&CType>,
        x: &syn::ExprMethodCall,
    ) -> Result<Option<ExVal>, CError> {
        let (tv, te) = match base {
            CType::Resultado(a, e) => ((**a).clone(), (**e).clone()),
            _ => return Ok(None),
        };
        match m {
            "is_ok" | "is_err" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", format!("`.{m}()` no lleva argumentos"), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Bool {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da `bool`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let pl = self.recv_lugar(b, base, d)?;
                let bang = if m == "is_ok" { "" } else { "!" };
                Ok(Some(ExVal::puro(format!("({bang}(({pl}).es_ok))"), CType::Bool)))
            }
            "unwrap" | "expect" => {
                let msg_c = if m == "expect" {
                    if args.len() != 1 {
                        return Err(self.err_en("C0005", "`.expect()` lleva un mensaje".into(), x));
                    }
                    let v = self.baja_expr(&args[0], Some(&CType::VistaTexto))?;
                    if v.tipo != CType::VistaTexto {
                        return Err(self.err_en(
                            "C0005",
                            format!("`.expect()` pide `&str`, no `{}`", self.muestra(&v.tipo)),
                            &args[0],
                        ));
                    }
                    Some(self.consume(v, Some(&CType::VistaTexto))?)
                } else {
                    if !args.is_empty() {
                        return Err(self.err_en("C0005", "`.unwrap()` no lleva argumentos".into(), x));
                    }
                    None
                };
                if let Some(e) = esp {
                    if e != &tv {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da `{}`", self.muestra(e), self.muestra(&tv)),
                            x,
                        ));
                    }
                }
                let (c, dueno) = self.recv_movible(b, d, m, x)?;
                let (c, dueno) = match dueno {
                    Some(n) => (c, Some(n)),
                    None => {
                        let t = self.temp(base.clone())?;
                        self.emite(format!("{t} = ({c});"));
                        self.marca_movida_si_temp_pub(&c);
                        self.marca_movida(&t);
                        self.activa_bandera(&t);
                        (t.clone(), Some(t))
                    }
                };
                self.emite(format!("if (!(({c}).es_ok)) {{"));
                let dt = self
                    .depurar_a_temp(format!("({c}).datos.err"), Some(format!("({c}).datos.err")), &te)
                    .map_err(|e| self.con_archivo(e))?;
                match msg_c {
                    Some(ms) => self.emite(format!(
                        "KAMI_PANICO(\"%s: %s\", ({ms}), kami_texto_cstr(&{dt}));"
                    )),
                    None => self.emite(format!(
                        "KAMI_PANICO(\"unwrap de Err: %s\", kami_texto_cstr(&{dt}));"
                    )),
                }
                self.emite("}");
                Ok(Some(self.mueve_campo(c, dueno, "datos.ok", tv)))
            }
            "unwrap_err" | "expect_err" => {
                let msg_c = if m == "expect_err" {
                    if args.len() != 1 {
                        return Err(self.err_en("C0005", "`.expect_err()` lleva un mensaje".into(), x));
                    }
                    let v = self.baja_expr(&args[0], Some(&CType::VistaTexto))?;
                    if v.tipo != CType::VistaTexto {
                        return Err(self.err_en(
                            "C0005",
                            format!("`.expect_err()` pide `&str`, no `{}`", self.muestra(&v.tipo)),
                            &args[0],
                        ));
                    }
                    Some(self.consume(v, Some(&CType::VistaTexto))?)
                } else {
                    if !args.is_empty() {
                        return Err(self.err_en("C0005", "`.unwrap_err()` no lleva argumentos".into(), x));
                    }
                    None
                };
                if let Some(e) = esp {
                    if e != &te {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da `{}`", self.muestra(e), self.muestra(&te)),
                            x,
                        ));
                    }
                }
                let (c, dueno) = self.recv_movible(b, d, m, x)?;
                let (c, dueno) = match dueno {
                    Some(n) => (c, Some(n)),
                    None => {
                        let t = self.temp(base.clone())?;
                        self.emite(format!("{t} = ({c});"));
                        self.marca_movida_si_temp_pub(&c);
                        self.marca_movida(&t);
                        self.activa_bandera(&t);
                        (t.clone(), Some(t))
                    }
                };
                self.emite(format!("if ((({c}).es_ok)) {{"));
                let dt = self
                    .depurar_a_temp(format!("({c}).datos.ok"), Some(format!("({c}).datos.ok")), &tv)
                    .map_err(|e| self.con_archivo(e))?;
                match msg_c {
                    Some(ms) => self.emite(format!(
                        "KAMI_PANICO(\"%s: %s\", ({ms}), kami_texto_cstr(&{dt}));"
                    )),
                    None => self.emite(format!(
                        "KAMI_PANICO(\"unwrap_err de Ok: %s\", kami_texto_cstr(&{dt}));"
                    )),
                }
                self.emite("}");
                Ok(Some(self.mueve_campo(c, dueno, "datos.err", te)))
            }
            "unwrap_or" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`.unwrap_or()` lleva un valor".into(), x));
                }
                if let Some(e) = esp {
                    if e != &tv {
                        return Err(self.err_en(
                            "C0005",
                            format!(
                                "se esperaba `{}`, `.unwrap_or()` da `{}`",
                                self.muestra(e),
                                self.muestra(&tv)
                            ),
                            x,
                        ));
                    }
                }
                let v = self.baja_expr(&args[0], Some(&tv))?;
                let cd = self.consume(v, Some(&tv))?;
                let (c, dueno) = self.recv_movible(b, d, m, x)?;
                let (c, _dueno) = match dueno {
                    Some(n) => (c, Some(n)),
                    None => {
                        let t = self.temp(base.clone())?;
                        self.emite(format!("{t} = ({c});"));
                        self.marca_movida_si_temp_pub(&c);
                        self.marca_movida(&t);
                        self.activa_bandera(&t);
                        (t.clone(), Some(t))
                    }
                };
                let td = self.temp(tv.clone())?;
                self.emite(format!("{td} = ({cd});"));
                self.marca_movida_si_temp_pub(&cd);
                let tu = self.temp(tv.clone())?;
                self.emite(format!("{tu} = ({td});"));
                self.marca_movida(&td);
                self.activa_bandera(&td);
                self.emite(format!("if (({c}).es_ok) {{"));
                for l in self.libera_lugar(&tu, &tv) {
                    self.emite(format!("    {l}"));
                }
                self.emite(format!("    {tu} = ({c}).datos.ok;"));
                self.emite("} else {");
                for l in self.libera_lugar(&format!("({c}).datos.err"), &te) {
                    self.emite(format!("    {l}"));
                }
                self.emite("}");
                Ok(Some(ExVal::movible(tu.clone(), tv, tu)))
            }
            "unwrap_or_default" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.unwrap_or_default()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if e != &tv {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `{}`", self.muestra(e), self.muestra(&tv)),
                            x,
                        ));
                    }
                }
                let (c, dueno) = self.recv_movible(b, d, m, x)?;
                let (c, _dueno) = match dueno {
                    Some(n) => (c, Some(n)),
                    None => {
                        let t = self.temp(base.clone())?;
                        self.emite(format!("{t} = ({c});"));
                        self.marca_movida_si_temp_pub(&c);
                        self.marca_movida(&t);
                        self.activa_bandera(&t);
                        (t.clone(), Some(t))
                    }
                };
                let tu = self.temp(tv.clone())?;
                self.emite(format!("if (({c}).es_ok) {{"));
                self.emite(format!("    {tu} = ({c}).datos.ok;"));
                self.emite("} else {");
                for l in self.libera_lugar(&format!("({c}).datos.err"), &te) {
                    self.emite(format!("    {l}"));
                }
                self.defecto_a_lugar(&tu, &tv).map_err(|e| self.con_archivo(e))?;
                self.emite("}");
                Ok(Some(ExVal::movible(tu.clone(), tv, tu)))
            }
            "ok" | "err" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", format!("`.{m}()` no lleva argumentos"), x));
                }
                let a_err = m == "err";
                let t_in = if a_err { te.clone() } else { tv.clone() };
                let t_r = CType::Opcion(Box::new(t_in.clone()));
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da otro `Option`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let (c, dueno) = self.recv_movible(b, d, m, x)?;
                let (c, _dueno) = match dueno {
                    Some(n) => (c, Some(n)),
                    None => {
                        let t = self.temp(base.clone())?;
                        self.emite(format!("{t} = ({c});"));
                        self.marca_movida_si_temp_pub(&c);
                        self.marca_movida(&t);
                        self.activa_bandera(&t);
                        (t.clone(), Some(t))
                    }
                };
                let tr = self.temp(t_r.clone())?;
                self.emite(format!("memset(&({tr}), 0, sizeof({tr}));"));
                let (cond, campo_ok, campo_no, t_no) = if a_err {
                    ("!(({c}).es_ok)", "datos.err", "datos.ok", tv.clone())
                } else {
                    ("(({c}).es_ok)", "datos.ok", "datos.err", te.clone())
                };
                let cond = cond.replace("{c}", &c);
                self.emite(format!("if ({cond}) {{"));
                self.emite(format!("    {tr}.tiene = true;"));
                self.emite(format!("    {tr}.valor = ({c}).{campo_ok};"));
                self.emite("} else {");
                for l in self.libera_lugar(&format!("({c}).{campo_no}"), &t_no) {
                    self.emite(format!("    {l}"));
                }
                self.emite("}");
                Ok(Some(ExVal::movible(tr.clone(), t_r, tr)))
            }
            "and" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`.and()` lleva un `Result`".into(), x));
                }
                let v = self.baja_expr(&args[0], esp)?;
                let (tu2, te2) = match &v.tipo {
                    CType::Resultado(a, e) => ((**a).clone(), (**e).clone()),
                    _ => {
                        return Err(self.err_en(
                            "C0005",
                            format!("`.and()` pide `Result`, no `{}`", self.muestra(&v.tipo)),
                            &args[0],
                        ))
                    }
                };
                if te2 != te {
                    return Err(self.err_en(
                        "C0005",
                        format!(
                            "`.and()` pide el mismo error `{}`, no `{}`",
                            self.muestra(&te),
                            self.muestra(&te2)
                        ),
                        &args[0],
                    ));
                }
                let t_r = v.tipo.clone();
                let cb = self.consume(v, esp)?;
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.and()` da otro `Result`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let _ = tu2;
                let (c, _dueno) = self.recv_movible(b, d, m, x)?;
                let tb2 = self.temp(t_r.clone())?;
                self.emite(format!("{tb2} = ({cb});"));
                self.marca_movida_si_temp_pub(&cb);
                let tr = self.temp(t_r.clone())?;
                self.emite(format!("{tr} = ({tb2});"));
                self.marca_movida(&tb2);
                self.activa_bandera(&tb2);
                self.emite(format!("if (({c}).es_ok) {{"));
                for l in self.libera_lugar(&format!("({c}).datos.ok"), &tv) {
                    self.emite(format!("    {l}"));
                }
                self.emite("} else {");
                for l in self.libera_lugar(&tr, &t_r) {
                    self.emite(format!("    {l}"));
                }
                self.emite(format!("    {tr}.es_ok = false;"));
                self.emite(format!("    {tr}.datos.err = ({c}).datos.err;"));
                self.emite("}");
                Ok(Some(ExVal::movible(tr.clone(), t_r, tr)))
            }
            "or" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`.or()` lleva un `Result`".into(), x));
                }
                let v = self.baja_expr(&args[0], Some(base))?;
                let cb = self.consume(v, Some(base))?;
                if let Some(e) = esp {
                    if e != base {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.or()` da `{}`", self.muestra(e), self.muestra(base)),
                            x,
                        ));
                    }
                }
                let (c, dueno) = self.recv_movible(b, d, m, x)?;
                let (c, _dueno) = match dueno {
                    Some(n) => (c, Some(n)),
                    None => {
                        let t = self.temp(base.clone())?;
                        self.emite(format!("{t} = ({c});"));
                        self.marca_movida_si_temp_pub(&c);
                        self.marca_movida(&t);
                        self.activa_bandera(&t);
                        (t.clone(), Some(t))
                    }
                };
                let tb2 = self.temp(base.clone())?;
                self.emite(format!("{tb2} = ({cb});"));
                self.marca_movida_si_temp_pub(&cb);
                let tr = self.temp(base.clone())?;
                self.emite(format!("{tr} = ({tb2});"));
                self.marca_movida(&tb2);
                self.activa_bandera(&tb2);
                self.emite(format!("if (({c}).es_ok) {{"));
                for l in self.libera_lugar(&tr, base) {
                    self.emite(format!("    {l}"));
                }
                self.emite(format!("    {tr} = ({c});"));
                self.emite("} else {");
                for l in self.libera_lugar(&format!("({c}).datos.err"), &te) {
                    self.emite(format!("    {l}"));
                }
                self.emite("}");
                Ok(Some(ExVal::movible(tr.clone(), base.clone(), tr)))
            }
            _ => Ok(None),
        }
    }

    /// Métodos de `String` (núcleo; `parse`/`clear`/`pop` llegan después).
    #[allow(clippy::too_many_arguments)]
    fn ex_texto(
        &mut self,
        b: ExVal,
        base: &CType,
        d: usize,
        _mut_ext: Option<bool>,
        m: &str,
        _tb: &[CType],
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        esp: Option<&CType>,
        x: &syn::ExprMethodCall,
    ) -> Result<Option<ExVal>, CError> {
        if *base != CType::Texto {
            return Ok(None);
        }
        match m {
            "len" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.len()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Usize {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.len()` da `usize`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let pl = self.recv_lugar(b, base, d)?;
                Ok(Some(ExVal::puro(format!("(kami_texto_largo(&({pl})))"), CType::Usize)))
            }
            "is_empty" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.is_empty()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Bool {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.is_empty()` da `bool`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let pl = self.recv_lugar(b, base, d)?;
                Ok(Some(ExVal::puro(format!("(kami_texto_vacio(&({pl})))"), CType::Bool)))
            }
            "as_str" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.as_str()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::VistaTexto {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.as_str()` da `&str`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let pl = self.recv_lugar(b, base, d)?;
                Ok(Some(ExVal::puro(format!("(kami_texto_cstr(&({pl})))"), CType::VistaTexto)))
            }
            "as_bytes" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.as_bytes()` no lleva argumentos".into(), x));
                }
                let t_r = CType::Rebana(Box::new(CType::U8), false);
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.as_bytes()` da `&[u8]`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let mg = self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let pl = self.recv_lugar(b, base, d)?;
                Ok(Some(ExVal::puro(
                    format!(
                        "({mg}){{ (const uint8_t*)kami_texto_cstr(&({pl})), kami_texto_largo(&({pl})) }}"
                    ),
                    t_r,
                )))
            }
            "push_str" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`.push_str()` lleva un `&str`".into(), x));
                }
                if !Self::presta_mut_ok(&b.tipo) {
                    return Err(self.err_en(
                        "C0003",
                        "`.push_str()` pide `&mut` en toda la cadena".into(),
                        &x.receiver,
                    ));
                }
                if let Some(e) = esp {
                    if *e != CType::Vacio {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.push_str()` vale `()`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let v = self.baja_expr(&args[0], Some(&CType::VistaTexto))?;
                if v.tipo != CType::VistaTexto {
                    return Err(self.err_en(
                        "C0005",
                        format!("`.push_str()` pide `&str`, no `{}`", self.muestra(&v.tipo)),
                        &args[0],
                    ));
                }
                let c = self.consume(v, Some(&CType::VistaTexto))?;
                let pl = self.recv_lugar(b, base, d)?;
                self.emite(format!("kami_texto_empuja(&({pl}), ({c}));"));
                Ok(Some(ExVal::puro("0".into(), CType::Vacio)))
            }
            "push" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`.push()` lleva un `char`".into(), x));
                }
                if !Self::presta_mut_ok(&b.tipo) {
                    return Err(self.err_en(
                        "C0003",
                        "`.push()` pide `&mut` en toda la cadena".into(),
                        &x.receiver,
                    ));
                }
                if let Some(e) = esp {
                    if *e != CType::Vacio {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.push()` vale `()`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let v = self.baja_expr(&args[0], Some(&CType::Char))?;
                let c = self.consume(v, Some(&CType::Char))?;
                let pl = self.recv_lugar(b, base, d)?;
                self.emite(format!("kami_texto_empuja_char(&({pl}), ({c}));"));
                Ok(Some(ExVal::puro("0".into(), CType::Vacio)))
            }
            "to_string" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.to_string()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Texto {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.to_string()` da `String`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let pl = self.recv_lugar(b, base, d)?;
                Ok(Some(ExVal::puro(format!("(kami_texto_clona(&({pl})))"), CType::Texto)))
            }
            "contains" | "starts_with" | "ends_with" => {
                if args.len() != 1 {
                    return Err(self.err_en(
                        "C0005",
                        format!("`.{m}()` lleva un patrón `&str`"),
                        x,
                    ));
                }
                if let Some(e) = esp {
                    if *e != CType::Bool {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da `bool`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let v = self.baja_expr(&args[0], Some(&CType::VistaTexto))?;
                if v.tipo != CType::VistaTexto {
                    return Err(self.err_en(
                        "C0005",
                        format!("`.{m}()` pide patrón `&str`, no `{}`", self.muestra(&v.tipo)),
                        &args[0],
                    )
                    .ayuda("solo patrones `&str` (ni `char` ni clausuras)"));
                }
                let cp = self.consume(v, Some(&CType::VistaTexto))?;
                let pl = self.recv_lugar(b, base, d)?;
                let tp = self.temp(CType::VistaTexto)?;
                self.emite(format!("{tp} = ({cp});"));
                self.marca_movida_si_temp_pub(&cp);
                let ta = self.temp(CType::VistaTexto)?;
                self.emite(format!("{ta} = kami_texto_cstr(&({pl}));"));
                match m {
                    "contains" => Ok(Some(ExVal::puro(
                        format!("((strstr(({ta}), ({tp})) != NULL))"),
                        CType::Bool,
                    ))),
                    "starts_with" => Ok(Some(ExVal::puro(
                        format!("((strncmp(({ta}), ({tp}), strlen({tp})) == 0))"),
                        CType::Bool,
                    ))),
                    _ => {
                        let na = self.temp(CType::Usize)?;
                        self.emite(format!("{na} = strlen({ta});"));
                        let np = self.temp(CType::Usize)?;
                        self.emite(format!("{np} = strlen({tp});"));
                        Ok(Some(ExVal::puro(
                            format!(
                                "((({na}) >= ({np})) && (memcmp(({ta}) + ({na}) - ({np}), ({tp}), ({np})) == 0))"
                            ),
                            CType::Bool,
                        )))
                    }
                }
            }
            "parse" => {
                let pl = self.recv_lugar(b, base, d)?;
                let cstr = format!("kami_texto_cstr(&({pl}))");
                self.ex_parse(cstr, _tb, args, esp, x).map(Some)
            }
            "clear" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.clear()` no lleva argumentos".into(), x));
                }
                if !Self::presta_mut_ok(&b.tipo) {
                    return Err(self.err_en(
                        "C0003",
                        "`.clear()` pide `&mut` en toda la cadena".into(),
                        &x.receiver,
                    ));
                }
                if let Some(e) = esp {
                    if *e != CType::Vacio {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.clear()` vale `()`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let pl = self.recv_lugar(b, base, d)?;
                self.emite(format!("(({pl}).largo) = 0;"));
                self.emite(format!("if (({pl}).datos) (({pl}).datos)[0] = 0;"));
                Ok(Some(ExVal::puro("0".into(), CType::Vacio)))
            }
            "truncate" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`.truncate()` lleva un largo".into(), x));
                }
                if !Self::presta_mut_ok(&b.tipo) {
                    return Err(self.err_en(
                        "C0003",
                        "`.truncate()` pide `&mut` en toda la cadena".into(),
                        &x.receiver,
                    ));
                }
                if let Some(e) = esp {
                    if *e != CType::Vacio {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.truncate()` vale `()`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let v = self.baja_expr(&args[0], Some(&CType::Usize))?;
                let cn = self.consume(v, Some(&CType::Usize))?;
                let pl = self.recv_lugar(b, base, d)?;
                let tn = self.temp(CType::Usize)?;
                self.emite(format!("{tn} = ({cn});"));
                self.marca_movida_si_temp_pub(&cn);
                self.emite(format!("if (({tn}) < (({pl}).largo)) {{"));
                self.emite(format!(
                    "    if ((({tn}) != 0) && ((((unsigned char)(({pl}).datos[({tn})])) & 0xC0) == 0x80)) {{ KAMI_PANICO(\"truncate fuera de borde\"); }}"
                ));
                self.emite(format!("    (({pl}).largo) = ({tn});"));
                self.emite(format!("    (({pl}).datos)[({tn})] = 0;"));
                self.emite("}");
                Ok(Some(ExVal::puro("0".into(), CType::Vacio)))
            }
            "pop" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.pop()` no lleva argumentos".into(), x));
                }
                if !Self::presta_mut_ok(&b.tipo) {
                    return Err(self.err_en(
                        "C0003",
                        "`.pop()` pide `&mut` en toda la cadena".into(),
                        &x.receiver,
                    ));
                }
                let t_r = CType::Opcion(Box::new(CType::Char));
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.pop()` da `Option<char>`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let pl = self.recv_lugar(b, base, d)?;
                let chs = CType::Char.deletrea().map_err(|e| self.con_archivo(e))?;
                let tr = self.temp(t_r.clone())?;
                self.emite(format!("memset(&({tr}), 0, sizeof({tr}));"));
                let tn = self.temp(CType::Usize)?;
                self.emite(format!("{tn} = kami_texto_largo(&({pl}));"));
                self.emite(format!("if (({tn}) > 0) {{"));
                let tstr = self.temp(CType::VistaTexto)?;
                self.emite(format!("    {tstr} = kami_texto_cstr(&({pl}));"));
                let ti = self.temp(CType::Usize)?;
                self.emite(format!("    {ti} = ({tn}) - 1;"));
                self.emite(format!(
                    "    while ((({ti}) > 0) && ((((unsigned char)({tstr})[({ti})]) & 0xC0) == 0x80)) {{ ({ti})--; }}"
                ));
                let tl = self.temp(CType::Usize)?;
                self.emite(format!("    {tl} = ({tn}) - ({ti});"));
                let tch = self.temp(CType::Char)?;
                self.emite(format!("    {tch} = ({chs})0;"));
                self.emite(format!("    if (({tl}) == 1) {{ {tch} = ({chs})(unsigned char)({tstr})[({ti})]; }}"));
                self.emite(format!(
                    "    else if (({tl}) == 2) {{ {tch} = ({chs})(((((unsigned char)({tstr})[({ti})]) & 0x1F) << 6) | (((unsigned char)({tstr})[({ti}) + 1]) & 0x3F)); }}"
                ));
                self.emite(format!(
                    "    else if (({tl}) == 3) {{ {tch} = ({chs})(((((unsigned char)({tstr})[({ti})]) & 0x0F) << 12) | ((((unsigned char)({tstr})[({ti}) + 1]) & 0x3F) << 6) | (((unsigned char)({tstr})[({ti}) + 2]) & 0x3F)); }}"
                ));
                self.emite(format!(
                    "    else {{ {tch} = ({chs})(((((unsigned char)({tstr})[({ti})]) & 0x07) << 18) | ((((unsigned char)({tstr})[({ti}) + 1]) & 0x3F) << 12) | ((((unsigned char)({tstr})[({ti}) + 2]) & 0x3F) << 6) | (((unsigned char)({tstr})[({ti}) + 3]) & 0x3F)); }}"
                ));
                self.emite(format!("    {tr}.tiene = true;"));
                self.emite(format!("    {tr}.valor = ({tch});"));
                self.emite(format!("    (({pl}).largo) = ({ti});"));
                self.emite(format!("    (({pl}).datos)[({ti})] = 0;"));
                self.emite("}");
                Ok(Some(ExVal::movible(tr.clone(), t_r, tr)))
            }
            "split" | "split_whitespace" | "lines" | "chars" | "bytes" | "trim" | "to_uppercase"
            | "to_lowercase" | "as_mut_str" => Err(self.err_en(
                "C0003",
                format!("`.{m}()` no cabe (iteradores/vistas con largo/`&mut str`)"),
                x,
            )
            .ayuda("recorre `.as_bytes()` con índices")),
            _ => Ok(None),
        }
    }

    /// Métodos de `&str` (la vista es `const char *` NUL-terminado).
    #[allow(clippy::too_many_arguments)]
    fn ex_vista(
        &mut self,
        b: ExVal,
        base: &CType,
        d: usize,
        m: &str,
        tb: &[CType],
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        esp: Option<&CType>,
        x: &syn::ExprMethodCall,
    ) -> Result<Option<ExVal>, CError> {
        if *base != CType::VistaTexto {
            return Ok(None);
        }
        match m {
            "len" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.len()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Usize {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.len()` da `usize`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let pl = self.recv_lugar(b, base, d)?;
                Ok(Some(ExVal::puro(format!("(strlen(({pl})))"), CType::Usize)))
            }
            "is_empty" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.is_empty()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Bool {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.is_empty()` da `bool`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let pl = self.recv_lugar(b, base, d)?;
                Ok(Some(ExVal::puro(format!("((({pl})[0] == 0))"), CType::Bool)))
            }
            "as_bytes" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.as_bytes()` no lleva argumentos".into(), x));
                }
                let t_r = CType::Rebana(Box::new(CType::U8), false);
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.as_bytes()` da `&[u8]`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let mg = self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let pl = self.recv_lugar(b, base, d)?;
                Ok(Some(ExVal::puro(
                    format!("({mg}){{ (const uint8_t*)({pl}), strlen({pl}) }}"),
                    t_r,
                )))
            }
            "to_string" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.to_string()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Texto {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.to_string()` da `String`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let pl = self.recv_lugar(b, base, d)?;
                Ok(Some(ExVal::puro(format!("(kami_texto_desde(({pl})))"), CType::Texto)))
            }
            "into" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.into()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Texto {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.into()` da `String`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let pl = self.recv_lugar(b, base, d)?;
                Ok(Some(ExVal::puro(format!("(kami_texto_desde(({pl})))"), CType::Texto)))
            }
            "contains" | "starts_with" | "ends_with" => {
                if args.len() != 1 {
                    return Err(self.err_en(
                        "C0005",
                        format!("`.{m}()` lleva un patrón `&str`"),
                        x,
                    ));
                }
                if let Some(e) = esp {
                    if *e != CType::Bool {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da `bool`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let v = self.baja_expr(&args[0], Some(&CType::VistaTexto))?;
                if v.tipo != CType::VistaTexto {
                    return Err(self.err_en(
                        "C0005",
                        format!("`.{m}()` pide patrón `&str`, no `{}`", self.muestra(&v.tipo)),
                        &args[0],
                    )
                    .ayuda("solo patrones `&str` (ni `char` ni clausuras)"));
                }
                let cp = self.consume(v, Some(&CType::VistaTexto))?;
                let pl = self.recv_lugar(b, base, d)?;
                let tp = self.temp(CType::VistaTexto)?;
                self.emite(format!("{tp} = ({cp});"));
                self.marca_movida_si_temp_pub(&cp);
                let ta = self.temp(CType::VistaTexto)?;
                self.emite(format!("{ta} = ({pl});"));
                self.marca_movida_si_temp_pub(&pl);
                match m {
                    "contains" => Ok(Some(ExVal::puro(
                        format!("((strstr(({ta}), ({tp})) != NULL))"),
                        CType::Bool,
                    ))),
                    "starts_with" => Ok(Some(ExVal::puro(
                        format!("((strncmp(({ta}), ({tp}), strlen({tp})) == 0))"),
                        CType::Bool,
                    ))),
                    _ => {
                        let na = self.temp(CType::Usize)?;
                        self.emite(format!("{na} = strlen({ta});"));
                        let np = self.temp(CType::Usize)?;
                        self.emite(format!("{np} = strlen({tp});"));
                        Ok(Some(ExVal::puro(
                            format!(
                                "((({na}) >= ({np})) && (memcmp(({ta}) + ({na}) - ({np}), ({tp}), ({np})) == 0))"
                            ),
                            CType::Bool,
                        )))
                    }
                }
            }
            "parse" => {
                let pl = self.recv_lugar(b, base, d)?;
                self.ex_parse(pl, tb, args, esp, x).map(Some)
            }
            "split" | "split_whitespace" | "lines" | "chars" | "bytes" | "trim" | "to_uppercase"
            | "to_lowercase" => Err(self.err_en(
                "C0003",
                format!("`.{m}()` no cabe (iteradores/vistas con largo)"),
                x,
            )
            .ayuda("recorre `.as_bytes()` con índices")),
            _ => Ok(None),
        }
    }

    /// `s.parse::<T>()` → `Result<T, &str>` (el error es texto: sin `ParseIntError`).
    fn ex_parse<S: Spanned + Copy>(
        &mut self,
        cstr: String,
        tb: &[CType],
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        esp: Option<&CType>,
        x: S,
    ) -> Result<ExVal, CError> {
        if !args.is_empty() {
            return Err(self.err_en("C0005", "`.parse()` no lleva argumentos".into(), x));
        }
        if tb.len() > 1 {
            return Err(self.err_en("C0005", "`.parse::<T>()` lleva un tipo".into(), x));
        }
        let t_dest = match (tb.first(), esp) {
            (Some(t), Some(e)) if t != e => {
                return Err(self.err_en(
                    "C0005",
                    format!(
                        "el turbofish dice `{}`, el contexto pide `{}`",
                        self.muestra(t),
                        self.muestra(e)
                    ),
                    x,
                ))
            }
            (Some(t), _) => t.clone(),
            (None, Some(CType::Resultado(t, _))) => (**t).clone(),
            (None, Some(_)) => {
                return Err(self.err_en("C0005", "`.parse()` da `Result<T, &str>`".into(), x))
            }
            (None, None) => {
                return Err(self.err_en("C0003", "anota el tipo: `.parse::<i32>()`".into(), x))
            }
        };
        let es_int = t_dest.es_entero() && !matches!(t_dest, CType::Char);
        let es_float = t_dest.es_flotante();
        let es_bool = t_dest == CType::Bool;
        if !es_int && !es_float && !es_bool {
            return Err(self.err_en(
                "C0003",
                format!("`{}` no parseable (enteros, flotantes, `bool`)", self.muestra(&t_dest)),
                x,
            ));
        }
        let t_r = CType::Resultado(Box::new(t_dest.clone()), Box::new(CType::VistaTexto));
        if let Some(e) = esp {
            if e != &t_r {
                return Err(self.err_en(
                    "C0005",
                    format!("se esperaba `{}`, `.parse()` da otro `Result`", self.muestra(e)),
                    x,
                ));
            }
        }
        self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
        let ts = self.temp(CType::VistaTexto)?;
        self.emite(format!("{ts} = ({cstr});"));
        self.marca_movida_si_temp_pub(&cstr);
        let tr = self.temp(t_r.clone())?;
        self.emite(format!("{tr}.es_ok = false;"));
        if es_bool {
            self.emite(format!("{tr}.datos.err = \"bool inválido (¿`true`/`false`?)\";"));
            self.emite(format!("if (strcmp(({ts}), \"true\") == 0) {{"));
            self.emite(format!("    {tr}.es_ok = true;"));
            self.emite(format!("    {tr}.datos.ok = true;"));
            self.emite(format!("}} else if (strcmp(({ts}), \"false\") == 0) {{"));
            self.emite(format!("    {tr}.es_ok = true;"));
            self.emite(format!("    {tr}.datos.ok = false;"));
            self.emite("}");
            return Ok(ExVal::movible(tr.clone(), t_r, tr));
        }
        if es_float {
            let spell = t_dest.deletrea().map_err(|e| self.con_archivo(e))?;
            self.emite(format!("{tr}.datos.err = \"flotante inválido\";"));
            self.emite("do {".to_string());
            self.emite(format!("    if (({ts})[0] == 0) {{ {tr}.datos.err = \"vacío\"; break; }}"));
            self.emite(format!(
                "    char __fc = ({ts})[0]; if (__fc == ' ' || __fc == '\\t' || __fc == '\\n' || __fc == '\\r') break;"
            ));
            self.emite(format!(
                "    const char *__fs = ({ts}) + (__fc == '+' || __fc == '-' ? 1 : 0);"
            ));
            self.emite("    if (__fs[0] == '0' && (__fs[1] == 'x' || __fs[1] == 'X')) break;".to_string());
            self.emite(format!("    char *__fe; double __fv = strtod(({ts}), &__fe);"));
            self.emite(format!("    if (__fe == ({ts}) || *__fe != 0) break;"));
            self.emite(format!("    {tr}.es_ok = true;"));
            self.emite(format!("    {tr}.datos.ok = ({spell})__fv;"));
            self.emite("} while (0);".to_string());
            return Ok(ExVal::movible(tr.clone(), t_r, tr));
        }
        // Enteros: pre-barrido estricto + `strtol(l)` + rango (sin `errno`).
        let con_signo = !t_dest.es_sin_signo();
        let w64 = std::mem::size_of::<usize>() == 8;
        let (func, vspell, minv, maxv): (&str, &str, &str, &str) = match t_dest {
            CType::I8 => ("strtoll", "long long", "INT8_MIN", "INT8_MAX"),
            CType::I16 => ("strtoll", "long long", "INT16_MIN", "INT16_MAX"),
            CType::I32 => ("strtoll", "long long", "INT32_MIN", "INT32_MAX"),
            CType::I64 => ("strtoll", "long long", "INT64_MIN", "INT64_MAX"),
            CType::Isize => ("strtoll", "long long", "PTRDIFF_MIN", "PTRDIFF_MAX"),
            CType::U8 => ("strtoull", "unsigned long long", "0", "UINT8_MAX"),
            CType::U16 => ("strtoull", "unsigned long long", "0", "UINT16_MAX"),
            CType::U32 => ("strtoull", "unsigned long long", "0", "UINT32_MAX"),
            CType::U64 => ("strtoull", "unsigned long long", "0", "UINT64_MAX"),
            CType::Usize => ("strtoull", "unsigned long long", "0", "SIZE_MAX"),
            _ => unreachable!(),
        };
        // Puerta de largo solo para 64 bits (el clamp de `strtoll` sería ambiguo).
        let gate: Option<(usize, &str, &str)> = match t_dest {
            CType::I64 => Some((19, "9223372036854775807", "9223372036854775808")),
            CType::U64 => Some((20, "18446744073709551615", "")),
            CType::Isize if w64 => Some((19, "9223372036854775807", "9223372036854775808")),
            CType::Usize if w64 => Some((20, "18446744073709551615", "")),
            _ => None,
        };
        let spell = t_dest.deletrea().map_err(|e| self.con_archivo(e))?;
        self.emite("do {".to_string());
        self.emite(format!("    if (({ts})[0] == 0) {{ {tr}.datos.err = \"vacío\"; break; }}"));
        self.emite(format!("    const char *__p = ({ts}); int __neg = 0;"));
        self.emite("    if (*__p == '-') {".to_string());
        if con_signo {
            self.emite("        __neg = 1; __p++;".to_string());
        } else {
            self.emite(format!("        {tr}.datos.err = \"signo `-` en sin signo\"; break;"));
        }
        self.emite("    }".to_string());
        self.emite(format!(
            "    if (*__p < '0' || *__p > '9') {{ {tr}.datos.err = \"dígito inválido\"; break; }}"
        ));
        self.emite("    const char *__q = __p;".to_string());
        self.emite("    while (*__q >= '0' && *__q <= '9') __q++;".to_string());
        self.emite(format!(
            "    if (*__q != 0) {{ {tr}.datos.err = \"dígito inválido\"; break; }}"
        ));
        self.emite("    size_t __nd = (size_t)(__q - __p);".to_string());
        if let Some((md, pos, neg)) = gate {
            self.emite(format!(
                "    if (__nd > {md}) {{ {tr}.datos.err = (__neg ? \"muy chico\" : \"muy grande\"); break; }}"
            ));
            if con_signo {
                self.emite(format!(
                    "    if (__nd == {md} && strcmp(__p, __neg ? \"{neg}\" : \"{pos}\") > 0) {{ {tr}.datos.err = (__neg ? \"muy chico\" : \"muy grande\"); break; }}"
                ));
            } else {
                self.emite(format!(
                    "    if (__nd == {md} && strcmp(__p, \"{pos}\") > 0) {{ {tr}.datos.err = \"muy grande\"; break; }}"
                ));
            }
        }
        self.emite(format!("    {vspell} __v = {func}(({ts}), NULL, 10);"));
        if gate.is_none() {
            if con_signo {
                self.emite(format!(
                    "    if ((__v) < ({minv}) || (__v) > ({maxv})) {{ {tr}.datos.err = ((__v) < 0 ? \"muy chico\" : \"muy grande\"); break; }}"
                ));
            } else {
                self.emite(format!(
                    "    if ((__v) > ({maxv})) {{ {tr}.datos.err = \"muy grande\"; break; }}"
                ));
            }
        }
        self.emite(format!("    {tr}.es_ok = true;"));
        self.emite(format!("    {tr}.datos.ok = ({spell})__v;"));
        self.emite("} while (0);".to_string());
        Ok(ExVal::movible(tr.clone(), t_r, tr))
    }

    /// Receptor numérico por valor (`Copy`): consume o estrellas (autoderef).
    fn recv_num(&mut self, b: ExVal, base: &CType, d: usize) -> Result<String, CError> {
        if d == 0 {
            return self.consume(b, Some(base));
        }
        let mut c = b.c;
        for _ in 0..d {
            c = format!("(*({c}))");
        }
        Ok(c)
    }

    /// Ancho en bits de un entero.
    fn ancho(t: &CType) -> i32 {
        match t {
            CType::I8 | CType::U8 => 8,
            CType::I16 | CType::U16 => 16,
            CType::I32 | CType::U32 => 32,
            CType::I64 | CType::U64 => 64,
            _ => (std::mem::size_of::<usize>() * 8) as i32,
        }
    }

    /// Métodos numéricos (núcleo; rotaciones/potencias/checked/floats llegan después).
    #[allow(clippy::too_many_arguments)]
    fn ex_num(
        &mut self,
        b: ExVal,
        base: &CType,
        d: usize,
        m: &str,
        _tb: &[CType],
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        esp: Option<&CType>,
        x: &syn::ExprMethodCall,
    ) -> Result<Option<ExVal>, CError> {
        if !base.es_numero() || matches!(base, CType::Char) {
            return Ok(None);
        }
        let t = base.clone();
        let flotante = t.es_flotante();
        match m {
            "to_string" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.to_string()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Texto {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.to_string()` da `String`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let tt = self.temp(CType::Texto)?;
                self.emite(format!("{tt} = kami_texto_nuevo();"));
                if flotante {
                    let fmt = if t == CType::F32 { "%.9g" } else { "%.16g" };
                    self.emite(format!("kami_texto_empuja_fmt(&{tt}, \"{fmt}\", (double)({c}));"));
                } else if t.es_sin_signo() {
                    self.emite(format!("kami_texto_empuja_fmt(&{tt}, \"%llu\", (unsigned long long)({c}));"));
                } else {
                    self.emite(format!("kami_texto_empuja_fmt(&{tt}, \"%lld\", (long long)({c}));"));
                }
                Ok(Some(ExVal::movible(tt.clone(), CType::Texto, tt)))
            }
            "abs" => {
                if flotante || t.es_sin_signo() {
                    return self.ex_float_fallback(b, base, d, m, args, esp, x);
                }
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.abs()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if e != &t {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.abs()` da `{}`", self.muestra(e), self.muestra(&t)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let tt = self.temp(t.clone())?;
                self.emite(format!("{tt} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let minv = match t {
                    CType::I8 => "INT8_MIN",
                    CType::I16 => "INT16_MIN",
                    CType::I32 => "INT32_MIN",
                    CType::I64 => "INT64_MIN",
                    _ => "PTRDIFF_MIN",
                };
                self.emite(format!("if (({tt}) == ({minv})) {{ KAMI_PANICO(\"desbordamiento en abs\"); }}"));
                Ok(Some(ExVal::puro(format!("((({tt}) < 0) ? (-({tt})) : ({tt}))"), t)))
            }
            "signum" => {
                if flotante {
                    return self.ex_float_fallback(b, base, d, m, args, esp, x);
                }
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.signum()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if e != &t {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.signum()` da `{}`", self.muestra(e), self.muestra(&t)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let tt = self.temp(t.clone())?;
                self.emite(format!("{tt} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                Ok(Some(ExVal::puro(
                    format!("((({tt}) > 0) ? 1 : ((({tt}) < 0) ? -1 : 0))"),
                    t,
                )))
            }
            "min" | "max" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", format!("`.{m}()` lleva otro valor"), x));
                }
                if let Some(e) = esp {
                    if e != &t {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da `{}`", self.muestra(e), self.muestra(&t)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let v = self.baja_expr(&args[0], Some(&t))?;
                let co = self.consume(v, Some(&t))?;
                let ta = self.temp(t.clone())?;
                self.emite(format!("{ta} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let tb2 = self.temp(t.clone())?;
                self.emite(format!("{tb2} = ({co});"));
                self.marca_movida_si_temp_pub(&co);
                let op = if m == "min" { "<" } else { ">" };
                if flotante {
                    self.usa_math = true;
                    Ok(Some(ExVal::puro(
                        format!(
                            "((isnan({ta})) ? ({tb2}) : ((isnan({tb2})) ? ({ta}) : ((({ta}) {op} ({tb2})) ? ({ta}) : ({tb2}))))"
                        ),
                        t,
                    )))
                } else {
                    Ok(Some(ExVal::puro(
                        format!("((({ta}) {op} ({tb2})) ? ({ta}) : ({tb2}))"),
                        t,
                    )))
                }
            }
            "clamp" => {
                if args.len() != 2 {
                    return Err(self.err_en("C0005", "`.clamp()` lleva mínimo y máximo".into(), x));
                }
                if let Some(e) = esp {
                    if e != &t {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.clamp()` da `{}`", self.muestra(e), self.muestra(&t)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let v0 = self.baja_expr(&args[0], Some(&t))?;
                let c0 = self.consume(v0, Some(&t))?;
                let v1 = self.baja_expr(&args[1], Some(&t))?;
                let c1 = self.consume(v1, Some(&t))?;
                let ta = self.temp(t.clone())?;
                self.emite(format!("{ta} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let t0 = self.temp(t.clone())?;
                self.emite(format!("{t0} = ({c0});"));
                self.marca_movida_si_temp_pub(&c0);
                let t1 = self.temp(t.clone())?;
                self.emite(format!("{t1} = ({c1});"));
                self.marca_movida_si_temp_pub(&c1);
                if flotante {
                    self.usa_math = true;
                    self.emite(format!(
                        "if ((isnan({t0})) || (isnan({t1})) || (({t0}) > ({t1}))) {{ KAMI_PANICO(\"clamp con cotas inválidas\"); }}"
                    ));
                    Ok(Some(ExVal::puro(
                        format!(
                            "((isnan({ta})) ? ({ta}) : ((({ta}) < ({t0})) ? ({t0}) : ((({ta}) > ({t1})) ? ({t1}) : ({ta}))))"
                        ),
                        t,
                    )))
                } else {
                    self.emite(format!(
                        "if (({t0}) > ({t1})) {{ KAMI_PANICO(\"clamp con min mayor que max\"); }}"
                    ));
                    Ok(Some(ExVal::puro(
                        format!("((({ta}) < ({t0})) ? ({t0}) : ((({ta}) > ({t1})) ? ({t1}) : ({ta})))"),
                        t,
                    )))
                }
            }
            "is_positive" | "is_negative" => {
                if flotante {
                    return self.ex_float_fallback(b, base, d, m, args, esp, x);
                }
                if !args.is_empty() {
                    return Err(self.err_en("C0005", format!("`.{m}()` no lleva argumentos"), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Bool {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da `bool`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                if m == "is_negative" && t.es_sin_signo() {
                    // Sin signo: siempre falso, pero evalúa (efectos).
                    let tt = self.temp(t.clone())?;
                    self.emite(format!("{tt} = ({c});"));
                    self.marca_movida_si_temp_pub(&c);
                    return Ok(Some(ExVal::puro("0".into(), CType::Bool)));
                }
                let op = if m == "is_positive" { ">" } else { "<" };
                Ok(Some(ExVal::puro(format!("((({c}) {op} 0))"), CType::Bool)))
            }
            "count_ones" | "count_zeros" => {
                if flotante {
                    return Ok(None);
                }
                if !args.is_empty() {
                    return Err(self.err_en("C0005", format!("`.{m}()` no lleva argumentos"), x));
                }
                if let Some(e) = esp {
                    if *e != CType::U32 {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da `u32`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let w = Self::ancho(&t);
                let pop = if w == 64 {
                    format!("__builtin_popcountll((unsigned long long)({c}))")
                } else {
                    format!("__builtin_popcount((unsigned)({c}))")
                };
                if m == "count_ones" {
                    Ok(Some(ExVal::puro(format!("(({pop}))"), CType::U32)))
                } else {
                    Ok(Some(ExVal::puro(format!("(({w} - ({pop})))"), CType::U32)))
                }
            }
            "leading_zeros" | "leading_ones" | "trailing_zeros" | "trailing_ones" => {
                if flotante {
                    return Ok(None);
                }
                if !args.is_empty() {
                    return Err(self.err_en("C0005", format!("`.{m}()` no lleva argumentos"), x));
                }
                if let Some(e) = esp {
                    if *e != CType::U32 {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da `u32`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let w = Self::ancho(&t);
                let tt = self.temp(t.clone())?;
                self.emite(format!("{tt} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let val = if m.ends_with("_ones") { format!("~({tt})") } else { tt.clone() };
                if m.starts_with("leading") {
                    let (bi, adj) = if w == 64 {
                        ("__builtin_clzll", 0)
                    } else {
                        ("__builtin_clz", 32 - w)
                    };
                    let core = if adj == 0 {
                        format!("{bi}((unsigned{ll})({val}))", ll = if w == 64 { " long long" } else { "" })
                    } else {
                        format!("({bi}((unsigned)({val})) - {adj})")
                    };
                    Ok(Some(ExVal::puro(
                        format!("((({val}) == 0) ? {w} : ({core}))"),
                        CType::U32,
                    )))
                } else {
                    let bi = if w == 64 { "__builtin_ctzll" } else { "__builtin_ctz" };
                    let cast = if w == 64 { "unsigned long long" } else { "unsigned" };
                    Ok(Some(ExVal::puro(
                        format!("((({val}) == 0) ? {w} : ({bi}(({cast})({val}))))"),
                        CType::U32,
                    )))
                }
            }
            "is_power_of_two" => {
                if flotante {
                    return Ok(None);
                }
                if !args.is_empty() {
                    return Err(self.err_en("C0005", format!("`.{m}()` no lleva argumentos"), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Bool {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da `bool`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let tt = self.temp(t.clone())?;
                self.emite(format!("{tt} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                Ok(Some(ExVal::puro(
                    format!("((((({tt}) != 0)) && ((({tt}) & (({tt}) - 1)) == 0)))"),
                    CType::Bool,
                )))
            }
            "rotate_left" | "rotate_right" => {
                if flotante {
                    return Ok(None);
                }
                if args.len() != 1 {
                    return Err(self.err_en("C0005", format!("`.{m}()` lleva un monto"), x));
                }
                if let Some(e) = esp {
                    if e != &t {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da `{}`", self.muestra(e), self.muestra(&t)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let v = self.baja_expr(&args[0], Some(&CType::U32))?;
                let cn = self.consume(v, Some(&CType::U32))?;
                let w = Self::ancho(&t);
                let tt = self.temp(t.clone())?;
                self.emite(format!("{tt} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let tn = self.temp(CType::U32)?;
                self.emite(format!("{tn} = ({cn});"));
                self.marca_movida_si_temp_pub(&cn);
                let tm = self.temp(CType::U32)?;
                self.emite(format!("{tm} = (({tn}) & ({}));", w - 1));
                let (izq, der) = if m == "rotate_left" { ("<<", ">>") } else { (">>", "<<") };
                if t.es_sin_signo() {
                    let masc = match w {
                        8 => " & 0xFF",
                        16 => " & 0xFFFF",
                        _ => "",
                    };
                    Ok(Some(ExVal::puro(
                        format!(
                            "((((({tt}) {izq} ({tm})) | (({tt}) {der} ((({w} - ({tm})) & ({}))))){masc}))",
                            w - 1
                        ),
                        t,
                    )))
                } else {
                    let spell = t.deletrea().map_err(|e| self.con_archivo(e))?;
                    let uspell = match t {
                        CType::I8 => "uint8_t",
                        CType::I16 => "uint16_t",
                        CType::I32 => "uint32_t",
                        CType::I64 => "uint64_t",
                        _ => "uintptr_t",
                    };
                    let masc = match w {
                        8 => " & 0xFF",
                        16 => " & 0xFFFF",
                        _ => "",
                    };
                    Ok(Some(ExVal::puro(
                        format!(
                            "(({spell})(((({uspell})({tt}) {izq} ({tm})) | ((({uspell})({tt}) {der} ((({w} - ({tm})) & ({}))))){masc})))",
                            w - 1
                        ),
                        t,
                    )))
                }
            }
            "pow" => {
                if flotante {
                    return Ok(None);
                }
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`.pow()` lleva un exponente".into(), x));
                }
                if let Some(e) = esp {
                    if e != &t {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.pow()` da `{}`", self.muestra(e), self.muestra(&t)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let v = self.baja_expr(&args[0], Some(&CType::U32))?;
                let ce = self.consume(v, Some(&CType::U32))?;
                let tr = self.temp(t.clone())?;
                self.emite(format!("{tr} = 1;"));
                let tb2 = self.temp(t.clone())?;
                self.emite(format!("{tb2} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let te = self.temp(CType::U32)?;
                self.emite(format!("{te} = ({ce});"));
                self.marca_movida_si_temp_pub(&ce);
                self.emite(format!("while ((({te}) > 0)) {{"));
                self.emite(format!("    if ((({te}) & 1)) {{"));
                self.emite(format!("        if (__builtin_mul_overflow(({tr}), ({tb2}), &({tr}))) {{ KAMI_PANICO(\"desbordamiento en pow {}\"); }}", self.muestra(&t)));
                self.emite("    }".to_string());
                self.emite(format!("    ({te}) >>= 1;"));
                self.emite(format!("    if (({te})) {{"));
                self.emite(format!("        if (__builtin_mul_overflow(({tb2}), ({tb2}), &({tb2}))) {{ KAMI_PANICO(\"desbordamiento en pow {}\"); }}", self.muestra(&t)));
                self.emite("    }".to_string());
                self.emite("}".to_string());
                Ok(Some(ExVal::puro(tr, t)))
            }
            "div_ceil" => {
                if flotante {
                    return Ok(None);
                }
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`.div_ceil()` lleva un divisor".into(), x));
                }
                if let Some(e) = esp {
                    if e != &t {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.div_ceil()` da `{}`", self.muestra(e), self.muestra(&t)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let v = self.baja_expr(&args[0], Some(&t))?;
                let cd = self.consume(v, Some(&t))?;
                let ta = self.temp(t.clone())?;
                self.emite(format!("{ta} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let tb2 = self.temp(t.clone())?;
                self.emite(format!("{tb2} = ({cd});"));
                self.marca_movida_si_temp_pub(&cd);
                self.emite(format!("if ((({tb2}) == 0)) {{ KAMI_PANICO(\"división por cero\"); }}"));
                if !t.es_sin_signo() {
                    let minv = match t {
                        CType::I8 => "INT8_MIN",
                        CType::I16 => "INT16_MIN",
                        CType::I32 => "INT32_MIN",
                        CType::I64 => "INT64_MIN",
                        _ => "PTRDIFF_MIN",
                    };
                    self.emite(format!(
                        "if ((({ta}) == ({minv})) && (({tb2}) == -1)) {{ KAMI_PANICO(\"desbordamiento en div_ceil\"); }}"
                    ));
                }
                let td = self.temp(t.clone())?;
                self.emite(format!("{td} = (({ta}) / ({tb2}));"));
                let tr = self.temp(t.clone())?;
                self.emite(format!("{tr} = (({ta}) % ({tb2}));"));
                if t.es_sin_signo() {
                    self.emite(format!("if ((({tr}) > 0)) {{ ({td}) = (({td}) + 1); }}"));
                } else {
                    self.emite(format!(
                        "if (((({tr}) > 0) && (({tb2}) > 0)) || ((({tr}) < 0) && (({tb2}) < 0))) {{ ({td}) = (({td}) + 1); }}"
                    ));
                }
                Ok(Some(ExVal::puro(td, t)))
            }
            "div_euclid" | "rem_euclid" => {
                if flotante {
                    return self.ex_float_fallback(b, base, d, m, args, esp, x);
                }
                if args.len() != 1 {
                    return Err(self.err_en("C0005", format!("`.{m}()` lleva un divisor"), x));
                }
                if let Some(e) = esp {
                    if e != &t {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da `{}`", self.muestra(e), self.muestra(&t)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let v = self.baja_expr(&args[0], Some(&t))?;
                let cd = self.consume(v, Some(&t))?;
                let ta = self.temp(t.clone())?;
                self.emite(format!("{ta} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let tb2 = self.temp(t.clone())?;
                self.emite(format!("{tb2} = ({cd});"));
                self.marca_movida_si_temp_pub(&cd);
                self.emite(format!("if ((({tb2}) == 0)) {{ KAMI_PANICO(\"división por cero\"); }}"));
                if !t.es_sin_signo() {
                    let minv = match t {
                        CType::I8 => "INT8_MIN",
                        CType::I16 => "INT16_MIN",
                        CType::I32 => "INT32_MIN",
                        CType::I64 => "INT64_MIN",
                        _ => "PTRDIFF_MIN",
                    };
                    self.emite(format!(
                        "if ((({ta}) == ({minv})) && (({tb2}) == -1)) {{ KAMI_PANICO(\"desbordamiento en {m}\"); }}"
                    ));
                }
                if m == "div_euclid" {
                    let tq = self.temp(t.clone())?;
                    self.emite(format!("{tq} = (({ta}) / ({tb2}));"));
                    if !t.es_sin_signo() {
                        let tr = self.temp(t.clone())?;
                        self.emite(format!("{tr} = (({ta}) % ({tb2}));"));
                        self.emite(format!("if ((({tr}) < 0)) {{"));
                        self.emite(format!("    if ((({tb2}) > 0)) {{ ({tq})--; }} else {{ ({tq})++; }}"));
                        self.emite("}".to_string());
                    }
                    Ok(Some(ExVal::puro(tq, t)))
                } else {
                    let tr = self.temp(t.clone())?;
                    self.emite(format!("{tr} = (({ta}) % ({tb2}));"));
                    if !t.es_sin_signo() {
                        let spell = t.deletrea().map_err(|e| self.con_archivo(e))?;
                        let uspell = match t {
                            CType::I8 => "uint8_t",
                            CType::I16 => "uint16_t",
                            CType::I32 => "uint32_t",
                            CType::I64 => "uint64_t",
                            _ => "uintptr_t",
                        };
                        self.emite(format!("if ((({tr}) < 0)) {{"));
                        self.emite(format!(
                            "    ({tr}) = ({spell})((({uspell})({tr})) + ((({tb2}) < 0) ? ((unsigned{ll})0 - (({uspell})({tb2}))) : (({uspell})({tb2}))));",
                            ll = if uspell == "uintptr_t" { " long" } else { "" }
                        ));
                        self.emite("}".to_string());
                    }
                    Ok(Some(ExVal::puro(tr, t)))
                }
            }
            "ilog2" | "checked_ilog2" => {
                if flotante {
                    return Ok(None);
                }
                if !args.is_empty() {
                    return Err(self.err_en("C0005", format!("`.{m}()` no lleva argumentos"), x));
                }
                let chequeado = m == "checked_ilog2";
                let t_r = if chequeado {
                    CType::Opcion(Box::new(CType::U32))
                } else {
                    CType::U32
                };
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da `{}`", self.muestra(e), self.muestra(&t_r)),
                            x,
                        ));
                    }
                }
                if chequeado {
                    self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                }
                let c = self.recv_num(b, base, d)?;
                let w = Self::ancho(&t);
                let tt = self.temp(t.clone())?;
                self.emite(format!("{tt} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let (bi, adj) = if w == 64 {
                    ("__builtin_clzll((unsigned long long)".to_string(), 0)
                } else {
                    ("__builtin_clz((unsigned)".to_string(), 32 - w)
                };
                let núcleo = if adj == 0 {
                    format!("{bi}({tt}))")
                } else {
                    format!("({bi}({tt})) - {adj})")
                };
                let val = format!("(({} - 1) - ({núcleo}))", w);
                if !chequeado {
                    self.emite(format!("if ((({tt}) == 0)) {{ KAMI_PANICO(\"ilog de cero\"); }}"));
                    Ok(Some(ExVal::puro(val, CType::U32)))
                } else {
                    let tr = self.temp(t_r.clone())?;
                    self.emite(format!("memset(&({tr}), 0, sizeof({tr}));"));
                    self.emite(format!("if ((({tt}) != 0)) {{"));
                    self.emite(format!("    {tr}.tiene = true;"));
                    self.emite(format!("    {tr}.valor = ({val});"));
                    self.emite("}".to_string());
                    Ok(Some(ExVal::movible(tr.clone(), t_r, tr)))
                }
            }
            "next_power_of_two" | "checked_next_power_of_two" => {
                if flotante {
                    return Ok(None);
                }
                if !args.is_empty() {
                    return Err(self.err_en("C0005", format!("`.{m}()` no lleva argumentos"), x));
                }
                let chequeado = m == "checked_next_power_of_two";
                let t_r = if chequeado { CType::Opcion(Box::new(t.clone())) } else { t.clone() };
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da `{}`", self.muestra(e), self.muestra(&t_r)),
                            x,
                        ));
                    }
                }
                if chequeado {
                    self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                }
                let c = self.recv_num(b, base, d)?;
                let w = Self::ancho(&t);
                let limite: u128 = match t {
                    CType::I8 => 64,
                    CType::I16 => 16384,
                    CType::I32 => 1073741824,
                    CType::I64 => 4611686018427387904,
                    CType::Isize if w == 64 => 4611686018427387904,
                    CType::Isize => 1073741824,
                    CType::U8 => 128,
                    CType::U16 => 32768,
                    CType::U32 => 2147483648,
                    CType::U64 => 9223372036854775808,
                    CType::Usize if w == 64 => 9223372036854775808,
                    CType::Usize => 2147483648,
                    _ => unreachable!(),
                };
                let lim_c = if limite > i64::MAX as u128 {
                    format!("{limite}ULL")
                } else {
                    format!("{limite}")
                };
                let tt = self.temp(t.clone())?;
                self.emite(format!("{tt} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let spell = t.deletrea().map_err(|e| self.con_archivo(e))?;
                let (bi, adj) = if w == 64 {
                    ("__builtin_clzll((unsigned long long)".to_string(), 0)
                } else {
                    ("__builtin_clz((unsigned)".to_string(), 32 - w)
                };
                let núcleo = if adj == 0 {
                    format!("{bi}(({tt}) - 1))")
                } else {
                    format!("({bi}(({tt}) - 1)) - {adj})")
                };
                let val = format!("(({spell})(1ULL << (({w}) - ({núcleo}))))");
                if !chequeado {
                    let tr = self.temp(t.clone())?;
                    self.emite(format!("if ((({tt}) <= 1)) {{ ({tr}) = 1; }}"));
                    self.emite(format!("else if ((({tt}) > ({lim_c}))) {{ KAMI_PANICO(\"next_power_of_two desborda\"); }}"));
                    self.emite(format!("else {{ ({tr}) = ({val}); }}"));
                    Ok(Some(ExVal::puro(tr, t)))
                } else {
                    let tr = self.temp(t_r.clone())?;
                    self.emite(format!("memset(&({tr}), 0, sizeof({tr}));"));
                    self.emite(format!("{tr}.tiene = true;"));
                    self.emite(format!("if ((({tt}) <= 1)) {{ ({tr}).valor = 1; }}"));
                    self.emite(format!("else if ((({tt}) > ({lim_c}))) {{ ({tr}).tiene = false; }}"));
                    self.emite(format!("else {{ ({tr}).valor = ({val}); }}"));
                    Ok(Some(ExVal::movible(tr.clone(), t_r, tr)))
                }
            }
            "wrapping_add" | "wrapping_sub" | "wrapping_mul" => {
                if flotante {
                    return Ok(None);
                }
                if args.len() != 1 {
                    return Err(self.err_en("C0005", format!("`.{m}()` lleva otro valor"), x));
                }
                if let Some(e) = esp {
                    if e != &t {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da `{}`", self.muestra(e), self.muestra(&t)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let v = self.baja_expr(&args[0], Some(&t))?;
                let co = self.consume(v, Some(&t))?;
                let op = match m {
                    "wrapping_add" => "+",
                    "wrapping_sub" => "-",
                    _ => "*",
                };
                let spell = t.deletrea().map_err(|e| self.con_archivo(e))?;
                let uspell = match t {
                    CType::I8 | CType::U8 => "uint8_t",
                    CType::I16 | CType::U16 => "uint16_t",
                    CType::I32 | CType::U32 => "uint32_t",
                    CType::I64 | CType::U64 => "uint64_t",
                    _ => "uintptr_t",
                };
                Ok(Some(ExVal::puro(
                    format!("(({spell})((({uspell})({c})) {op} (({uspell})({co})))))"),
                    t,
                )))
            }
            "wrapping_div" | "wrapping_rem" => {
                if flotante {
                    return Ok(None);
                }
                if args.len() != 1 {
                    return Err(self.err_en("C0005", format!("`.{m}()` lleva un divisor"), x));
                }
                if let Some(e) = esp {
                    if e != &t {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da `{}`", self.muestra(e), self.muestra(&t)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let v = self.baja_expr(&args[0], Some(&t))?;
                let cd = self.consume(v, Some(&t))?;
                let ta = self.temp(t.clone())?;
                self.emite(format!("{ta} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let tb2 = self.temp(t.clone())?;
                self.emite(format!("{tb2} = ({cd});"));
                self.marca_movida_si_temp_pub(&cd);
                self.emite(format!("if ((({tb2}) == 0)) {{ KAMI_PANICO(\"división por cero\"); }}"));
                let op = if m == "wrapping_div" { "/" } else { "%" };
                if t.es_sin_signo() {
                    Ok(Some(ExVal::puro(format!("((({ta}) {op} ({tb2})))"), t)))
                } else {
                    let minv = match t {
                        CType::I8 => "INT8_MIN",
                        CType::I16 => "INT16_MIN",
                        CType::I32 => "INT32_MIN",
                        CType::I64 => "INT64_MIN",
                        _ => "PTRDIFF_MIN",
                    };
                    let env = if m == "wrapping_div" { minv.to_string() } else { "0".to_string() };
                    Ok(Some(ExVal::puro(
                        format!(
                            "((((({ta}) == ({minv})) && (({tb2}) == -1)) ? ({env}) : ((({ta}) {op} ({tb2})))))"
                        ),
                        t,
                    )))
                }
            }
            "wrapping_neg" => {
                if flotante {
                    return Ok(None);
                }
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.wrapping_neg()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if e != &t {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `{}`", self.muestra(e), self.muestra(&t)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let spell = t.deletrea().map_err(|e| self.con_archivo(e))?;
                let uspell = match t {
                    CType::I8 | CType::U8 => "uint8_t",
                    CType::I16 | CType::U16 => "uint16_t",
                    CType::I32 | CType::U32 => "uint32_t",
                    CType::I64 | CType::U64 => "uint64_t",
                    _ => "uintptr_t",
                };
                Ok(Some(ExVal::puro(format!("(({spell})((({uspell})0) - (({uspell})({c}))))"), t)))
            }
            "wrapping_shl" | "wrapping_shr" => {
                if flotante {
                    return Ok(None);
                }
                if args.len() != 1 {
                    return Err(self.err_en("C0005", format!("`.{m}()` lleva un monto"), x));
                }
                if let Some(e) = esp {
                    if e != &t {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da `{}`", self.muestra(e), self.muestra(&t)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let v = self.baja_expr(&args[0], Some(&CType::U32))?;
                let cn = self.consume(v, Some(&CType::U32))?;
                let w = Self::ancho(&t);
                let op = if m == "wrapping_shl" { "<<" } else { ">>" };
                let spell = t.deletrea().map_err(|e| self.con_archivo(e))?;
                let uspell = match t {
                    CType::I8 | CType::U8 => "uint8_t",
                    CType::I16 | CType::U16 => "uint16_t",
                    CType::I32 | CType::U32 => "uint32_t",
                    CType::I64 | CType::U64 => "uint64_t",
                    _ => "uintptr_t",
                };
                Ok(Some(ExVal::puro(
                    format!("(({spell})((({uspell})({c})) {op} ((({cn}) & ({})))))", w - 1),
                    t,
                )))
            }
            "wrapping_abs" => {
                if flotante || t.es_sin_signo() {
                    return Ok(None);
                }
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.wrapping_abs()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if e != &t {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `{}`", self.muestra(e), self.muestra(&t)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let tt = self.temp(t.clone())?;
                self.emite(format!("{tt} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let minv = match t {
                    CType::I8 => "INT8_MIN",
                    CType::I16 => "INT16_MIN",
                    CType::I32 => "INT32_MIN",
                    CType::I64 => "INT64_MIN",
                    _ => "PTRDIFF_MIN",
                };
                Ok(Some(ExVal::puro(
                    format!("(((({tt}) == ({minv})) ? ({minv}) : (((({tt}) < 0) ? (-({tt})) : ({tt})))))"),
                    t,
                )))
            }
            "wrapping_pow" => {
                if flotante {
                    return Ok(None);
                }
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`.wrapping_pow()` lleva un exponente".into(), x));
                }
                if let Some(e) = esp {
                    if e != &t {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `{}`", self.muestra(e), self.muestra(&t)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let v = self.baja_expr(&args[0], Some(&CType::U32))?;
                let ce = self.consume(v, Some(&CType::U32))?;
                let spell = t.deletrea().map_err(|e| self.con_archivo(e))?;
                let uspell = match t {
                    CType::I8 | CType::U8 => "uint8_t",
                    CType::I16 | CType::U16 => "uint16_t",
                    CType::I32 | CType::U32 => "uint32_t",
                    CType::I64 | CType::U64 => "uint64_t",
                    _ => "uintptr_t",
                };
                let tr = self.temp(t.clone())?;
                self.emite(format!("{tr} = 1;"));
                let tb2 = self.temp(t.clone())?;
                self.emite(format!("{tb2} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let te = self.temp(CType::U32)?;
                self.emite(format!("{te} = ({ce});"));
                self.marca_movida_si_temp_pub(&ce);
                self.emite(format!("while ((({te}) > 0)) {{"));
                self.emite(format!("    if ((({te}) & 1)) {{ ({tr}) = ({spell})((({uspell})({tr})) * (({uspell})({tb2}))); }}"));
                self.emite(format!("    ({te}) >>= 1;"));
                self.emite(format!("    ({tb2}) = ({spell})((({uspell})({tb2})) * (({uspell})({tb2})));"));
                self.emite("}".to_string());
                Ok(Some(ExVal::puro(tr, t)))
            }
            "to_be" | "to_le" | "from_be" | "from_le" | "to_ne" | "from_ne" => {
                if flotante {
                    return Ok(None);
                }
                if !args.is_empty() {
                    return Err(self.err_en("C0005", format!("`.{m}()` no lleva argumentos"), x));
                }
                if let Some(e) = esp {
                    if e != &t {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da `{}`", self.muestra(e), self.muestra(&t)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let w = Self::ancho(&t);
                if w == 8 || m == "to_ne" || m == "from_ne" {
                    return Ok(Some(ExVal::puro(format!("({c})"), t)));
                }
                let tt = self.temp(t.clone())?;
                self.emite(format!("{tt} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let tl = self.temp(CType::Bool)?;
                self.emite("bool __es_arqv;".to_string().replace("bool __es_arqv;", &format!("{tl} = ((*(const unsigned char*)&(uint16_t){{1}}) == 1);")));
                let spell = t.deletrea().map_err(|e| self.con_archivo(e))?;
                let (bi, uspell) = match w {
                    16 => ("__builtin_bswap16", "uint16_t"),
                    32 => ("__builtin_bswap32", "uint32_t"),
                    _ => ("__builtin_bswap64", "uint64_t"),
                };
                let sw = format!("(({spell})({bi}((({uspell})({tt})))))");
                let e = if m == "to_be" || m == "from_be" {
                    format!("((({tl}) ? ({sw}) : ({tt})))")
                } else {
                    format!("((({tl}) ? ({tt}) : ({sw})))")
                };
                Ok(Some(ExVal::puro(e, t)))
            }
            "checked_add" | "checked_sub" | "checked_mul" => {
                if flotante {
                    return Ok(None);
                }
                if args.len() != 1 {
                    return Err(self.err_en("C0005", format!("`.{m}()` lleva otro valor"), x));
                }
                let t_r = CType::Opcion(Box::new(t.clone()));
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da `Option`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let c = self.recv_num(b, base, d)?;
                let v = self.baja_expr(&args[0], Some(&t))?;
                let co = self.consume(v, Some(&t))?;
                let builtin = match m {
                    "checked_add" => "add",
                    "checked_sub" => "sub",
                    _ => "mul",
                };
                let tv = self.temp(t.clone())?;
                let tr = self.temp(t_r.clone())?;
                self.emite(format!("memset(&({tr}), 0, sizeof({tr}));"));
                self.emite(format!(
                    "if (!__builtin_{builtin}_overflow((({c})), (({co})), &({tv}))) {{ ({tr}).tiene = true; ({tr}).valor = ({tv}); }}"
                ));
                Ok(Some(ExVal::movible(tr.clone(), t_r, tr)))
            }
            "checked_div" | "checked_rem" => {
                if flotante {
                    return Ok(None);
                }
                if args.len() != 1 {
                    return Err(self.err_en("C0005", format!("`.{m}()` lleva un divisor"), x));
                }
                let t_r = CType::Opcion(Box::new(t.clone()));
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da `Option`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let c = self.recv_num(b, base, d)?;
                let v = self.baja_expr(&args[0], Some(&t))?;
                let cd = self.consume(v, Some(&t))?;
                let ta = self.temp(t.clone())?;
                self.emite(format!("{ta} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let tb2 = self.temp(t.clone())?;
                self.emite(format!("{tb2} = ({cd});"));
                self.marca_movida_si_temp_pub(&cd);
                let op = if m == "checked_div" { "/" } else { "%" };
                let tr = self.temp(t_r.clone())?;
                self.emite(format!("memset(&({tr}), 0, sizeof({tr}));"));
                if t.es_sin_signo() {
                    self.emite(format!(
                        "if ((({tb2}) != 0)) {{ ({tr}).tiene = true; ({tr}).valor = (({ta}) {op} ({tb2})); }}"
                    ));
                } else {
                    let minv = match t {
                        CType::I8 => "INT8_MIN",
                        CType::I16 => "INT16_MIN",
                        CType::I32 => "INT32_MIN",
                        CType::I64 => "INT64_MIN",
                        _ => "PTRDIFF_MIN",
                    };
                    self.emite(format!(
                        "if (((({tb2}) != 0) && !((({ta}) == ({minv})) && (({tb2}) == -1)))) {{ ({tr}).tiene = true; ({tr}).valor = (({ta}) {op} ({tb2})); }}"
                    ));
                }
                Ok(Some(ExVal::movible(tr.clone(), t_r, tr)))
            }
            "checked_neg" => {
                if flotante {
                    return Ok(None);
                }
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.checked_neg()` no lleva argumentos".into(), x));
                }
                let t_r = CType::Opcion(Box::new(t.clone()));
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `Option`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let c = self.recv_num(b, base, d)?;
                let tt = self.temp(t.clone())?;
                self.emite(format!("{tt} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let tr = self.temp(t_r.clone())?;
                self.emite(format!("memset(&({tr}), 0, sizeof({tr}));"));
                if t.es_sin_signo() {
                    self.emite(format!("if ((({tt}) == 0)) {{ ({tr}).tiene = true; ({tr}).valor = 0; }}"));
                } else {
                    let minv = match t {
                        CType::I8 => "INT8_MIN",
                        CType::I16 => "INT16_MIN",
                        CType::I32 => "INT32_MIN",
                        CType::I64 => "INT64_MIN",
                        _ => "PTRDIFF_MIN",
                    };
                    self.emite(format!(
                        "if ((({tt}) != ({minv}))) {{ ({tr}).tiene = true; ({tr}).valor = (-({tt})); }}"
                    ));
                }
                Ok(Some(ExVal::movible(tr.clone(), t_r, tr)))
            }
            "checked_shl" | "checked_shr" => {
                if flotante {
                    return Ok(None);
                }
                if args.len() != 1 {
                    return Err(self.err_en("C0005", format!("`.{m}()` lleva un monto"), x));
                }
                let t_r = CType::Opcion(Box::new(t.clone()));
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da `Option`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let c = self.recv_num(b, base, d)?;
                let v = self.baja_expr(&args[0], Some(&CType::U32))?;
                let cn = self.consume(v, Some(&CType::U32))?;
                let w = Self::ancho(&t);
                let op = if m == "checked_shl" { "<<" } else { ">>" };
                let tr = self.temp(t_r.clone())?;
                self.emite(format!("memset(&({tr}), 0, sizeof({tr}));"));
                let tn = self.temp(CType::U32)?;
                self.emite(format!("{tn} = ({cn});"));
                self.marca_movida_si_temp_pub(&cn);
                let tt = self.temp(t.clone())?;
                self.emite(format!("{tt} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let val = if m == "checked_shl" && !t.es_sin_signo() {
                    let spell = t.deletrea().map_err(|e| self.con_archivo(e))?;
                    let uspell = match t {
                        CType::I8 => "uint8_t",
                        CType::I16 => "uint16_t",
                        CType::I32 => "uint32_t",
                        CType::I64 => "uint64_t",
                        _ => "uintptr_t",
                    };
                    format!("(({spell})((({uspell})({tt})) {op} ({tn})))")
                } else {
                    format!("(({tt}) {op} ({tn}))")
                };
                self.emite(format!("if ((({tn}) < {w})) {{ ({tr}).tiene = true; ({tr}).valor = ({val}); }}"));
                Ok(Some(ExVal::movible(tr.clone(), t_r, tr)))
            }
            "checked_abs" => {
                if flotante || t.es_sin_signo() {
                    return Ok(None);
                }
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.checked_abs()` no lleva argumentos".into(), x));
                }
                let t_r = CType::Opcion(Box::new(t.clone()));
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `Option`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let c = self.recv_num(b, base, d)?;
                let tt = self.temp(t.clone())?;
                self.emite(format!("{tt} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let minv = match t {
                    CType::I8 => "INT8_MIN",
                    CType::I16 => "INT16_MIN",
                    CType::I32 => "INT32_MIN",
                    CType::I64 => "INT64_MIN",
                    _ => "PTRDIFF_MIN",
                };
                let tr = self.temp(t_r.clone())?;
                self.emite(format!("memset(&({tr}), 0, sizeof({tr}));"));
                self.emite(format!(
                    "if ((({tt}) != ({minv}))) {{ ({tr}).tiene = true; ({tr}).valor = ((({tt}) < 0) ? (-({tt})) : ({tt})); }}"
                ));
                Ok(Some(ExVal::movible(tr.clone(), t_r, tr)))
            }
            "checked_pow" => {
                if flotante {
                    return Ok(None);
                }
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`.checked_pow()` lleva un exponente".into(), x));
                }
                let t_r = CType::Opcion(Box::new(t.clone()));
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `Option`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let c = self.recv_num(b, base, d)?;
                let v = self.baja_expr(&args[0], Some(&CType::U32))?;
                let ce = self.consume(v, Some(&CType::U32))?;
                let tr = self.temp(t.clone())?;
                self.emite(format!("{tr} = 1;"));
                let tb2 = self.temp(t.clone())?;
                self.emite(format!("{tb2} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let te = self.temp(CType::U32)?;
                self.emite(format!("{te} = ({ce});"));
                self.marca_movida_si_temp_pub(&ce);
                let tok = self.temp(CType::Bool)?;
                self.emite(format!("{tok} = true;"));
                self.emite(format!("while ((({te}) > 0)) {{"));
                self.emite(format!("    if ((({te}) & 1)) {{"));
                self.emite(format!("        if (__builtin_mul_overflow(({tr}), ({tb2}), &({tr}))) {{ ({tok}) = false; break; }}"));
                self.emite("    }".to_string());
                self.emite(format!("    ({te}) >>= 1;"));
                self.emite(format!("    if (({te})) {{"));
                self.emite(format!("        if (__builtin_mul_overflow(({tb2}), ({tb2}), &({tb2}))) {{ ({tok}) = false; break; }}"));
                self.emite("    }".to_string());
                self.emite("}".to_string());
                let to = self.temp(t_r.clone())?;
                self.emite(format!("memset(&({to}), 0, sizeof({to}));"));
                self.emite(format!("if (({tok})) {{ ({to}).tiene = true; ({to}).valor = ({tr}); }}"));
                Ok(Some(ExVal::movible(to.clone(), t_r, to)))
            }
            "saturating_add" | "saturating_sub" | "saturating_mul" => {
                if flotante {
                    return Ok(None);
                }
                if args.len() != 1 {
                    return Err(self.err_en("C0005", format!("`.{m}()` lleva otro valor"), x));
                }
                if let Some(e) = esp {
                    if e != &t {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da `{}`", self.muestra(e), self.muestra(&t)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let v = self.baja_expr(&args[0], Some(&t))?;
                let co = self.consume(v, Some(&t))?;
                let builtin = match m {
                    "saturating_add" => "add",
                    "saturating_sub" => "sub",
                    _ => "mul",
                };
                let ta = self.temp(t.clone())?;
                self.emite(format!("{ta} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let tb2 = self.temp(t.clone())?;
                self.emite(format!("{tb2} = ({co});"));
                self.marca_movida_si_temp_pub(&co);
                let tv = self.temp(t.clone())?;
                let (minv, maxv) = match t {
                    CType::I8 => ("INT8_MIN", "INT8_MAX"),
                    CType::I16 => ("INT16_MIN", "INT16_MAX"),
                    CType::I32 => ("INT32_MIN", "INT32_MAX"),
                    CType::I64 => ("INT64_MIN", "INT64_MAX"),
                    CType::Isize => ("PTRDIFF_MIN", "PTRDIFF_MAX"),
                    CType::U8 => ("0", "UINT8_MAX"),
                    CType::U16 => ("0", "UINT16_MAX"),
                    CType::U32 => ("0", "UINT32_MAX"),
                    CType::U64 => ("0", "UINT64_MAX"),
                    _ => ("0", "SIZE_MAX"),
                };
                let dir = if t.es_sin_signo() {
                    match m {
                        "saturating_sub" => minv.to_string(),
                        _ => maxv.to_string(),
                    }
                } else {
                    match m {
                        "saturating_add" => format!("((({ta}) < 0) ? ({minv}) : ({maxv}))"),
                        "saturating_sub" => format!("((({tb2}) < 0) ? ({maxv}) : ({minv}))"),
                        _ => format!("((((({ta}) < 0) != (({tb2}) < 0))) ? ({minv}) : ({maxv}))"),
                    }
                };
                self.emite(format!(
                    "if (__builtin_{builtin}_overflow((({ta})), (({tb2})), &({tv}))) {{ ({tv}) = ({dir}); }}"
                ));
                Ok(Some(ExVal::puro(tv, t)))
            }
            "saturating_div" => {
                if flotante {
                    return Ok(None);
                }
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`.saturating_div()` lleva un divisor".into(), x));
                }
                if let Some(e) = esp {
                    if e != &t {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `{}`", self.muestra(e), self.muestra(&t)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let v = self.baja_expr(&args[0], Some(&t))?;
                let cd = self.consume(v, Some(&t))?;
                let ta = self.temp(t.clone())?;
                self.emite(format!("{ta} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let tb2 = self.temp(t.clone())?;
                self.emite(format!("{tb2} = ({cd});"));
                self.marca_movida_si_temp_pub(&cd);
                self.emite(format!("if ((({tb2}) == 0)) {{ KAMI_PANICO(\"división por cero\"); }}"));
                if t.es_sin_signo() {
                    Ok(Some(ExVal::puro(format!("((({ta}) / ({tb2})))"), t)))
                } else {
                    let (minv, maxv) = match t {
                        CType::I8 => ("INT8_MIN", "INT8_MAX"),
                        CType::I16 => ("INT16_MIN", "INT16_MAX"),
                        CType::I32 => ("INT32_MIN", "INT32_MAX"),
                        CType::I64 => ("INT64_MIN", "INT64_MAX"),
                        _ => ("PTRDIFF_MIN", "PTRDIFF_MAX"),
                    };
                    Ok(Some(ExVal::puro(
                        format!(
                            "((((({ta}) == ({minv})) && (({tb2}) == -1)) ? ({maxv}) : ((({ta}) / ({tb2})))))"
                        ),
                        t,
                    )))
                }
            }
            "saturating_neg" => {
                if flotante {
                    return Ok(None);
                }
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.saturating_neg()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if e != &t {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `{}`", self.muestra(e), self.muestra(&t)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let tt = self.temp(t.clone())?;
                self.emite(format!("{tt} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                if t.es_sin_signo() {
                    return Ok(Some(ExVal::puro("0".into(), t)));
                }
                let (minv, maxv) = match t {
                    CType::I8 => ("INT8_MIN", "INT8_MAX"),
                    CType::I16 => ("INT16_MIN", "INT16_MAX"),
                    CType::I32 => ("INT32_MIN", "INT32_MAX"),
                    CType::I64 => ("INT64_MIN", "INT64_MAX"),
                    _ => ("PTRDIFF_MIN", "PTRDIFF_MAX"),
                };
                Ok(Some(ExVal::puro(
                    format!("(((({tt}) == ({minv})) ? ({maxv}) : ((0) - ({tt})))))"),
                    t,
                )))
            }
            "saturating_abs" => {
                if flotante || t.es_sin_signo() {
                    return Ok(None);
                }
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.saturating_abs()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if e != &t {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `{}`", self.muestra(e), self.muestra(&t)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let tt = self.temp(t.clone())?;
                self.emite(format!("{tt} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let (minv, maxv) = match t {
                    CType::I8 => ("INT8_MIN", "INT8_MAX"),
                    CType::I16 => ("INT16_MIN", "INT16_MAX"),
                    CType::I32 => ("INT32_MIN", "INT32_MAX"),
                    CType::I64 => ("INT64_MIN", "INT64_MAX"),
                    _ => ("PTRDIFF_MIN", "PTRDIFF_MAX"),
                };
                Ok(Some(ExVal::puro(
                    format!(
                        "((((({tt}) == ({minv})) ? ({maxv}) : (((({tt}) < 0) ? (-({tt})) : ({tt}))))))"
                    ),
                    t,
                )))
            }
            "saturating_pow" => {
                if flotante {
                    return Ok(None);
                }
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`.saturating_pow()` lleva un exponente".into(), x));
                }
                if let Some(e) = esp {
                    if e != &t {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `{}`", self.muestra(e), self.muestra(&t)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let v = self.baja_expr(&args[0], Some(&CType::U32))?;
                let ce = self.consume(v, Some(&CType::U32))?;
                let (minv, maxv) = match t {
                    CType::I8 => ("INT8_MIN", "INT8_MAX"),
                    CType::I16 => ("INT16_MIN", "INT16_MAX"),
                    CType::I32 => ("INT32_MIN", "INT32_MAX"),
                    CType::I64 => ("INT64_MIN", "INT64_MAX"),
                    CType::Isize => ("PTRDIFF_MIN", "PTRDIFF_MAX"),
                    CType::U8 => ("0", "UINT8_MAX"),
                    CType::U16 => ("0", "UINT16_MAX"),
                    CType::U32 => ("0", "UINT32_MAX"),
                    CType::U64 => ("0", "UINT64_MAX"),
                    _ => ("0", "SIZE_MAX"),
                };
                let tr = self.temp(t.clone())?;
                self.emite(format!("{tr} = 1;"));
                let tb2 = self.temp(t.clone())?;
                self.emite(format!("{tb2} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let te = self.temp(CType::U32)?;
                self.emite(format!("{te} = ({ce});"));
                self.marca_movida_si_temp_pub(&ce);
                let sat = if t.es_sin_signo() {
                    maxv.to_string()
                } else {
                    let te0 = self.temp(CType::U32)?;
                    self.emite(format!("{te0} = ({te});"));
                    let tneg = self.temp(CType::Bool)?;
                    self.emite(format!("{tneg} = ((({tb2}) < 0) && ((({te0}) & 1) != 0));"));
                    format!("((({tneg}) ? ({minv}) : ({maxv})))")
                };
                self.emite(format!("while ((({te}) > 0)) {{"));
                self.emite(format!("    if ((({te}) & 1)) {{"));
                self.emite(format!(
                    "        if (__builtin_mul_overflow(({tr}), ({tb2}), &({tr}))) {{ ({tr}) = ({sat}); break; }}"
                ));
                self.emite("    }".to_string());
                self.emite(format!("    ({te}) >>= 1;"));
                self.emite(format!("    if (({te})) {{"));
                self.emite(format!(
                    "        if (__builtin_mul_overflow(({tb2}), ({tb2}), &({tb2}))) {{ ({tr}) = ({sat}); break; }}"
                ));
                self.emite("    }".to_string());
                self.emite("}".to_string());
                Ok(Some(ExVal::puro(tr, t)))
            }
            "overflowing_add" | "overflowing_sub" | "overflowing_mul" => {
                if flotante {
                    return Ok(None);
                }
                if args.len() != 1 {
                    return Err(self.err_en("C0005", format!("`.{m}()` lleva otro valor"), x));
                }
                let t_r = CType::Tupla(vec![t.clone(), CType::Bool]);
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da tupla", self.muestra(e)),
                            x,
                        ));
                    }
                }
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let c = self.recv_num(b, base, d)?;
                let v = self.baja_expr(&args[0], Some(&t))?;
                let co = self.consume(v, Some(&t))?;
                let builtin = match m {
                    "overflowing_add" => "add",
                    "overflowing_sub" => "sub",
                    _ => "mul",
                };
                let tv = self.temp(t.clone())?;
                let tr = self.temp(t_r.clone())?;
                self.emite(format!("({tr})._1 = __builtin_{builtin}_overflow((({c})), (({co})), &({tv}));"));
                self.emite(format!("({tr})._0 = ({tv});"));
                Ok(Some(ExVal::movible(tr.clone(), t_r, tr)))
            }
            "overflowing_div" | "overflowing_rem" => {
                if flotante {
                    return Ok(None);
                }
                if args.len() != 1 {
                    return Err(self.err_en("C0005", format!("`.{m}()` lleva un divisor"), x));
                }
                let t_r = CType::Tupla(vec![t.clone(), CType::Bool]);
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da tupla", self.muestra(e)),
                            x,
                        ));
                    }
                }
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let c = self.recv_num(b, base, d)?;
                let v = self.baja_expr(&args[0], Some(&t))?;
                let cd = self.consume(v, Some(&t))?;
                let ta = self.temp(t.clone())?;
                self.emite(format!("{ta} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let tb2 = self.temp(t.clone())?;
                self.emite(format!("{tb2} = ({cd});"));
                self.marca_movida_si_temp_pub(&cd);
                self.emite(format!("if ((({tb2}) == 0)) {{ KAMI_PANICO(\"división por cero\"); }}"));
                let op = if m == "overflowing_div" { "/" } else { "%" };
                let tr = self.temp(t_r.clone())?;
                if t.es_sin_signo() {
                    self.emite(format!("({tr})._0 = (({ta}) {op} ({tb2}));"));
                    self.emite(format!("({tr})._1 = false;"));
                } else {
                    let minv = match t {
                        CType::I8 => "INT8_MIN",
                        CType::I16 => "INT16_MIN",
                        CType::I32 => "INT32_MIN",
                        CType::I64 => "INT64_MIN",
                        _ => "PTRDIFF_MIN",
                    };
                    let env = if m == "overflowing_div" { minv.to_string() } else { "0".to_string() };
                    self.emite(format!("if (((({ta}) == ({minv})) && (({tb2}) == -1))) {{"));
                    self.emite(format!("    ({tr})._0 = ({env});"));
                    self.emite(format!("    ({tr})._1 = true;"));
                    self.emite("} else {".to_string());
                    self.emite(format!("    ({tr})._0 = (({ta}) {op} ({tb2}));"));
                    self.emite(format!("    ({tr})._1 = false;"));
                    self.emite("}".to_string());
                }
                Ok(Some(ExVal::movible(tr.clone(), t_r, tr)))
            }
            "overflowing_neg" => {
                if flotante {
                    return Ok(None);
                }
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.overflowing_neg()` no lleva argumentos".into(), x));
                }
                let t_r = CType::Tupla(vec![t.clone(), CType::Bool]);
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da tupla", self.muestra(e)),
                            x,
                        ));
                    }
                }
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let c = self.recv_num(b, base, d)?;
                let tt = self.temp(t.clone())?;
                self.emite(format!("{tt} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let tr = self.temp(t_r.clone())?;
                if t.es_sin_signo() {
                    let spell = t.deletrea().map_err(|e| self.con_archivo(e))?;
                    let uspell = match t {
                        CType::U8 => "uint8_t",
                        CType::U16 => "uint16_t",
                        CType::U32 => "uint32_t",
                        CType::U64 => "uint64_t",
                        _ => "size_t",
                    };
                    self.emite(format!("({tr})._0 = ({spell})((({uspell})0) - (({uspell})({tt})));"));
                    self.emite(format!("({tr})._1 = (({tt}) != 0);"));
                } else {
                    let minv = match t {
                        CType::I8 => "INT8_MIN",
                        CType::I16 => "INT16_MIN",
                        CType::I32 => "INT32_MIN",
                        CType::I64 => "INT64_MIN",
                        _ => "PTRDIFF_MIN",
                    };
                    self.emite(format!("if ((({tt}) == ({minv}))) {{"));
                    self.emite(format!("    ({tr})._0 = ({minv});"));
                    self.emite(format!("    ({tr})._1 = true;"));
                    self.emite("} else {".to_string());
                    self.emite(format!("    ({tr})._0 = (-({tt}));"));
                    self.emite(format!("    ({tr})._1 = false;"));
                    self.emite("}".to_string());
                }
                Ok(Some(ExVal::movible(tr.clone(), t_r, tr)))
            }
            "overflowing_shl" | "overflowing_shr" => {
                if flotante {
                    return Ok(None);
                }
                if args.len() != 1 {
                    return Err(self.err_en("C0005", format!("`.{m}()` lleva un monto"), x));
                }
                let t_r = CType::Tupla(vec![t.clone(), CType::Bool]);
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da tupla", self.muestra(e)),
                            x,
                        ));
                    }
                }
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let c = self.recv_num(b, base, d)?;
                let v = self.baja_expr(&args[0], Some(&CType::U32))?;
                let cn = self.consume(v, Some(&CType::U32))?;
                let w = Self::ancho(&t);
                let op = if m == "overflowing_shl" { "<<" } else { ">>" };
                let tt = self.temp(t.clone())?;
                self.emite(format!("{tt} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let tn = self.temp(CType::U32)?;
                self.emite(format!("{tn} = ({cn});"));
                self.marca_movida_si_temp_pub(&cn);
                let tm = self.temp(CType::U32)?;
                self.emite(format!("{tm} = (({tn}) & ({}));", w - 1));
                let spell = t.deletrea().map_err(|e| self.con_archivo(e))?;
                let uspell = match t {
                    CType::I8 | CType::U8 => "uint8_t",
                    CType::I16 | CType::U16 => "uint16_t",
                    CType::I32 | CType::U32 => "uint32_t",
                    CType::I64 | CType::U64 => "uint64_t",
                    _ => "uintptr_t",
                };
                let tr = self.temp(t_r.clone())?;
                self.emite(format!("({tr})._0 = ({spell})((({uspell})({tt})) {op} ({tm}));"));
                self.emite(format!("({tr})._1 = ((({tn}) >= {w}));"));
                Ok(Some(ExVal::movible(tr.clone(), t_r, tr)))
            }
            "overflowing_abs" => {
                if flotante || t.es_sin_signo() {
                    return Ok(None);
                }
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.overflowing_abs()` no lleva argumentos".into(), x));
                }
                let t_r = CType::Tupla(vec![t.clone(), CType::Bool]);
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da tupla", self.muestra(e)),
                            x,
                        ));
                    }
                }
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let c = self.recv_num(b, base, d)?;
                let tt = self.temp(t.clone())?;
                self.emite(format!("{tt} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let minv = match t {
                    CType::I8 => "INT8_MIN",
                    CType::I16 => "INT16_MIN",
                    CType::I32 => "INT32_MIN",
                    CType::I64 => "INT64_MIN",
                    _ => "PTRDIFF_MIN",
                };
                let tr = self.temp(t_r.clone())?;
                self.emite(format!("if ((({tt}) == ({minv}))) {{"));
                self.emite(format!("    ({tr})._0 = ({minv});"));
                self.emite(format!("    ({tr})._1 = true;"));
                self.emite("} else {".to_string());
                self.emite(format!("    ({tr})._0 = ((({tt}) < 0) ? (-({tt})) : ({tt}));"));
                self.emite(format!("    ({tr})._1 = false;"));
                self.emite("}".to_string());
                Ok(Some(ExVal::movible(tr.clone(), t_r, tr)))
            }
            "overflowing_pow" => {
                if flotante {
                    return Ok(None);
                }
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`.overflowing_pow()` lleva un exponente".into(), x));
                }
                let t_r = CType::Tupla(vec![t.clone(), CType::Bool]);
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da tupla", self.muestra(e)),
                            x,
                        ));
                    }
                }
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let c = self.recv_num(b, base, d)?;
                let v = self.baja_expr(&args[0], Some(&CType::U32))?;
                let ce = self.consume(v, Some(&CType::U32))?;
                let tr = self.temp(t.clone())?;
                self.emite(format!("{tr} = 1;"));
                let tb2 = self.temp(t.clone())?;
                self.emite(format!("{tb2} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let te = self.temp(CType::U32)?;
                self.emite(format!("{te} = ({ce});"));
                self.marca_movida_si_temp_pub(&ce);
                let tf = self.temp(CType::Bool)?;
                self.emite(format!("{tf} = false;"));
                self.emite(format!("while ((({te}) > 0)) {{"));
                self.emite(format!("    if ((({te}) & 1)) {{ ({tf}) = (({tf}) || __builtin_mul_overflow(({tr}), ({tb2}), &({tr}))); }}"));
                self.emite(format!("    ({te}) >>= 1;"));
                self.emite(format!("    if (({te})) {{ ({tf}) = (({tf}) || __builtin_mul_overflow(({tb2}), ({tb2}), &({tb2}))); }}"));
                self.emite("}".to_string());
                let to = self.temp(t_r.clone())?;
                self.emite(format!("({to})._0 = ({tr});"));
                self.emite(format!("({to})._1 = ({tf});"));
                Ok(Some(ExVal::movible(to.clone(), t_r, to)))
            }
            "sqrt" | "cbrt" | "sin" | "cos" | "tan" | "asin" | "acos" | "atan" | "sinh"
            | "cosh" | "tanh" | "asinh" | "acosh" | "atanh" | "exp" | "exp2" | "exp_m1"
            | "ln" | "log2" | "log10" | "ln_1p" | "ceil" | "floor" | "round" | "trunc"
            | "round_ties_even" | "powf" | "powi" | "atan2" | "hypot" | "log" | "mul_add"
            | "recip" | "fract" | "to_degrees" | "to_radians" | "copysign" | "is_nan"
            | "is_infinite" | "is_finite" | "to_bits" => {
                if !flotante {
                    return Ok(None);
                }
                self.ex_float_fn(b, base, d, m, args, esp, x).map(Some)
            }
            "to_be_bytes" | "to_le_bytes" | "to_ne_bytes" => {
                if flotante {
                    return Ok(None);
                }
                if !args.is_empty() {
                    return Err(self.err_en("C0005", format!("`.{m}()` no lleva argumentos"), x));
                }
                let nb = (Self::ancho(&t) / 8) as usize;
                let t_r = CType::Arreglo(Box::new(CType::U8), LargoArreglo::Lit(nb));
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da `[u8; {nb}]`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let tt = self.temp(t.clone())?;
                self.emite(format!("{tt} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let mut outs = Vec::new();
                if m == "to_ne_bytes" {
                    for i in 0..nb {
                        outs.push(ElemLista {
                            c: format!("(((const uint8_t*)(&({tt})))[{i}])"),
                            tipo: CType::U8,
                        });
                    }
                } else {
                    let uspell = match nb {
                        1 => "uint8_t",
                        2 => "uint16_t",
                        4 => "uint32_t",
                        _ => "uint64_t",
                    };
                    for i in 0..nb {
                        let sh = if m == "to_be_bytes" { (nb - 1 - i) * 8 } else { i * 8 };
                        outs.push(ElemLista {
                            c: format!("((uint8_t)(((({uspell})({tt})) >> {sh}) & 0xFF))"),
                            tipo: CType::U8,
                        });
                    }
                }
                Ok(Some(ExVal {
                    c: String::new(),
                    tipo: t_r,
                    movible: None,
                    lugar: None,
                    arreglo: Some(ArrayInit::Lista(outs)),
                }))
            }
            "is_ascii" => {
                if t != CType::U8 {
                    return Ok(None);
                }
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.is_ascii()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Bool {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `bool`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                Ok(Some(ExVal::puro(format!("((({c}) < 0x80))"), CType::Bool)))
            }
            "is_ascii_alphabetic" | "is_ascii_alphanumeric" | "is_ascii_digit"
            | "is_ascii_hexdigit" | "is_ascii_lowercase" | "is_ascii_uppercase"
            | "is_ascii_whitespace" | "is_ascii_graphic" | "is_ascii_punctuation"
            | "is_ascii_control" => {
                if t != CType::U8 {
                    return Ok(None);
                }
                if !args.is_empty() {
                    return Err(self.err_en("C0005", format!("`.{m}()` no lleva argumentos"), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Bool {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `bool`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let tt = self.temp(CType::U8)?;
                self.emite(format!("{tt} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let l = format!("((({tt}) >= 'a') && (({tt}) <= 'z'))");
                let u = format!("((({tt}) >= 'A') && (({tt}) <= 'Z'))");
                let dg = format!("((({tt}) >= '0') && (({tt}) <= '9'))");
                let e = match m {
                    "is_ascii_alphabetic" => format!("(({l} || {u}))"),
                    "is_ascii_alphanumeric" => format!("(({l} || {u} || {dg}))"),
                    "is_ascii_digit" => dg,
                    "is_ascii_hexdigit" => format!(
                        "(({dg} || ((({tt}) >= 'a') && (({tt}) <= 'f')) || ((({tt}) >= 'A') && (({tt}) <= 'F'))))"
                    ),
                    "is_ascii_lowercase" => l,
                    "is_ascii_uppercase" => u,
                    "is_ascii_whitespace" => format!(
                        "(((({tt}) == ' ') || (({tt}) == '\\t') || (({tt}) == '\\n') || (({tt}) == 0x0C) || (({tt}) == '\\r')))"
                    ),
                    "is_ascii_graphic" => format!("((({tt}) >= 0x21) && (({tt}) <= 0x7E))"),
                    "is_ascii_punctuation" => {
                        format!("((((({tt}) >= 0x21) && (({tt}) <= 0x7E)) && !({l} || {u} || {dg})))")
                    }
                    _ => format!("((({tt}) < 0x20) || (({tt}) == 0x7F))"),
                };
                Ok(Some(ExVal::puro(e, CType::Bool)))
            }
            "to_ascii_lowercase" | "to_ascii_uppercase" => {
                if t != CType::U8 {
                    return Ok(None);
                }
                if !args.is_empty() {
                    return Err(self.err_en("C0005", format!("`.{m}()` no lleva argumentos"), x));
                }
                if let Some(e) = esp {
                    if *e != CType::U8 {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `u8`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let tt = self.temp(CType::U8)?;
                self.emite(format!("{tt} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let e = if m == "to_ascii_lowercase" {
                    format!("((((( {tt}) >= 'A') && (({tt}) <= 'Z')) ? (({tt}) + 32) : ({tt})))")
                } else {
                    format!("((((( {tt}) >= 'a') && (({tt}) <= 'z')) ? (({tt}) - 32) : ({tt})))")
                };
                Ok(Some(ExVal::puro(e, CType::U8)))
            }
            "eq_ignore_ascii_case" => {
                if t != CType::U8 {
                    return Ok(None);
                }
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`.eq_ignore_ascii_case()` lleva un byte".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Bool {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `bool`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let v = self.baja_expr(&args[0], Some(&CType::U8))?;
                let ca = self.consume(v, Some(&CType::U8))?;
                let t0 = self.temp(CType::U8)?;
                self.emite(format!("{t0} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let t1 = self.temp(CType::U8)?;
                self.emite(format!("{t1} = ({ca});"));
                self.marca_movida_si_temp_pub(&ca);
                let lo0 = format!("((((({t0}) >= 'A') && (({t0}) <= 'Z')) ? (({t0}) + 32) : ({t0})))");
                let lo1 = format!("((((({t1}) >= 'A') && (({t1}) <= 'Z')) ? (({t1}) + 32) : ({t1})))");
                Ok(Some(ExVal::puro(format!("(({lo0}) == ({lo1}))"), CType::Bool)))
            }
            "make_ascii_lowercase" | "make_ascii_uppercase" => {
                if t != CType::U8 {
                    return Ok(None);
                }
                if !args.is_empty() {
                    return Err(self.err_en("C0005", format!("`.{m}()` no lleva argumentos"), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Vacio {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` vale `()`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                if d > 0 && !Self::presta_mut_ok(&b.tipo) {
                    return Err(self.err_en(
                        "C0003",
                        format!("`.{m}()` pide `&mut` en toda la cadena"),
                        &x.receiver,
                    ));
                }
                let pl = self.recv_lugar(b, base, d)?;
                if m == "make_ascii_lowercase" {
                    self.emite(format!(
                        "if (((({pl}) >= 'A') && (({pl}) <= 'Z'))) {{ ({pl}) = (({pl}) + 32); }}"
                    ));
                } else {
                    self.emite(format!(
                        "if (((({pl}) >= 'a') && (({pl}) <= 'z'))) {{ ({pl}) = (({pl}) - 32); }}"
                    ));
                }
                Ok(Some(ExVal::puro("0".into(), CType::Vacio)))
            }
            _ => Ok(None),
        }
    }

    /// Métodos flotantes cuyos nombres colisionan con los enteros (`abs`, `signum`...).
    #[allow(clippy::too_many_arguments)]
    fn ex_float_fallback(
        &mut self,
        b: ExVal,
        base: &CType,
        d: usize,
        m: &str,
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        esp: Option<&CType>,
        x: &syn::ExprMethodCall,
    ) -> Result<Option<ExVal>, CError> {
        if !base.es_flotante() {
            return Ok(None);
        }
        let t = base.clone();
        self.usa_math = true;
        match m {
            "abs" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.abs()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if e != &t {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.abs()` da `{}`", self.muestra(e), self.muestra(&t)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let f = if t == CType::F32 { "fabsf" } else { "fabs" };
                Ok(Some(ExVal::puro(format!("(({f}(({c}))))"), t)))
            }
            "signum" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.signum()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if e != &t {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `{}`", self.muestra(e), self.muestra(&t)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let tt = self.temp(t.clone())?;
                self.emite(format!("{tt} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                Ok(Some(ExVal::puro(
                    format!("(((({tt}) > 0) ? 1 : (((({tt}) < 0) ? -1 : ({tt})))))"),
                    t,
                )))
            }
            "is_positive" | "is_negative" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", format!("`.{m}()` no lleva argumentos"), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Bool {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da `bool`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                if m == "is_positive" {
                    Ok(Some(ExVal::puro(format!("((!signbit({c})))"), CType::Bool)))
                } else {
                    Ok(Some(ExVal::puro(format!("((signbit({c}) != 0))"), CType::Bool)))
                }
            }
            "div_euclid" | "rem_euclid" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", format!("`.{m}()` lleva un divisor"), x));
                }
                if let Some(e) = esp {
                    if e != &t {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da `{}`", self.muestra(e), self.muestra(&t)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let v = self.baja_expr(&args[0], Some(&t))?;
                let cd = self.consume(v, Some(&t))?;
                let f = if t == CType::F32 { "truncf" } else { "trunc" };
                let fm = if t == CType::F32 { "fmodf" } else { "fmod" };
                let fa = if t == CType::F32 { "fabsf" } else { "fabs" };
                let ta = self.temp(t.clone())?;
                self.emite(format!("{ta} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let tb2 = self.temp(t.clone())?;
                self.emite(format!("{tb2} = ({cd});"));
                self.marca_movida_si_temp_pub(&cd);
                let tr = self.temp(t.clone())?;
                self.emite(format!("{tr} = ({fm}(({ta}), ({tb2})));"));
                if m == "div_euclid" {
                    let tq = self.temp(t.clone())?;
                    self.emite(format!("{tq} = ({f}(({ta}) / ({tb2})));"));
                    self.emite(format!("if ((({tr}) < 0)) {{"));
                    self.emite(format!("    if ((({tb2}) > 0)) {{ ({tq}) = (({tq}) - 1); }} else {{ ({tq}) = (({tq}) + 1); }}"));
                    self.emite("}".to_string());
                    Ok(Some(ExVal::puro(tq, t)))
                } else {
                    self.emite(format!("if ((({tr}) < 0)) {{ ({tr}) = (({tr}) + ({fa}(({tb2})))); }}"));
                    Ok(Some(ExVal::puro(tr, t)))
                }
            }
            _ => Ok(None),
        }
    }

    /// Funciones flotantes sueltas (`sqrt`, `sin`, `ln`, `ceil`...).
    fn ex_float_fn(
        &mut self,
        b: ExVal,
        base: &CType,
        d: usize,
        m: &str,
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        esp: Option<&CType>,
        x: &syn::ExprMethodCall,
    ) -> Result<ExVal, CError> {
        let t = base.clone();
        self.usa_math = true;
        // Nombre C desde el nombre Rust (las que difieren).
        let cn = match m {
            "ln" => "log",
            "exp_m1" => "expm1",
            "ln_1p" => "log1p",
            "round_ties_even" => "rint",
            other => other,
        };
        match m {
            "sqrt" | "cbrt" | "sin" | "cos" | "tan" | "asin" | "acos" | "atan" | "sinh"
            | "cosh" | "tanh" | "asinh" | "acosh" | "atanh" | "exp" | "exp2" | "exp_m1"
            | "ln" | "log2" | "log10" | "ln_1p" | "ceil" | "floor" | "round" | "trunc"
            | "round_ties_even" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", format!("`.{m}()` no lleva argumentos"), x));
                }
                if let Some(e) = esp {
                    if e != &t {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da `{}`", self.muestra(e), self.muestra(&t)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let suf = if t == CType::F32 { "f" } else { "" };
                Ok(ExVal::puro(format!("(({cn}{suf}(({c}))))"), t))
            }
            "powf" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`.powf()` lleva un exponente".into(), x));
                }
                if let Some(e) = esp {
                    if e != &t {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `{}`", self.muestra(e), self.muestra(&t)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let v = self.baja_expr(&args[0], Some(&t))?;
                let ce = self.consume(v, Some(&t))?;
                let f = if t == CType::F32 { "powf" } else { "pow" };
                Ok(ExVal::puro(format!("(({f}(({c}), ({ce}))))"), t))
            }
            "powi" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`.powi()` lleva un exponente `i32`".into(), x));
                }
                if let Some(e) = esp {
                    if e != &t {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `{}`", self.muestra(e), self.muestra(&t)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let v = self.baja_expr(&args[0], Some(&CType::I32))?;
                let ce = self.consume(v, Some(&CType::I32))?;
                let f = if t == CType::F32 { "powf" } else { "pow" };
                Ok(ExVal::puro(format!("(({f}(({c}), (double)({ce}))))"), t))
            }
            "atan2" | "hypot" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", format!("`.{m}()` lleva otro valor"), x));
                }
                if let Some(e) = esp {
                    if e != &t {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `{}`", self.muestra(e), self.muestra(&t)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let v = self.baja_expr(&args[0], Some(&t))?;
                let co = self.consume(v, Some(&t))?;
                let suf = if t == CType::F32 { "f" } else { "" };
                Ok(ExVal::puro(format!("(({m}{suf}(({c}), ({co}))))"), t))
            }
            "log" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`.log()` lleva una base".into(), x));
                }
                if let Some(e) = esp {
                    if e != &t {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `{}`", self.muestra(e), self.muestra(&t)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let v = self.baja_expr(&args[0], Some(&t))?;
                let cb = self.consume(v, Some(&t))?;
                let suf = if t == CType::F32 { "f" } else { "" };
                let ta = self.temp(t.clone())?;
                self.emite(format!("{ta} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let tb2 = self.temp(t.clone())?;
                self.emite(format!("{tb2} = ({cb});"));
                self.marca_movida_si_temp_pub(&cb);
                Ok(ExVal::puro(format!("((log{suf}(({ta})) / log{suf}(({tb2}))))"), t))
            }
            "mul_add" => {
                if args.len() != 2 {
                    return Err(self.err_en("C0005", "`.mul_add()` lleva dos valores".into(), x));
                }
                if let Some(e) = esp {
                    if e != &t {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `{}`", self.muestra(e), self.muestra(&t)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let v0 = self.baja_expr(&args[0], Some(&t))?;
                let c0 = self.consume(v0, Some(&t))?;
                let v1 = self.baja_expr(&args[1], Some(&t))?;
                let c1 = self.consume(v1, Some(&t))?;
                let f = if t == CType::F32 { "fmaf" } else { "fma" };
                Ok(ExVal::puro(format!("(({f}(({c}), ({c0}), ({c1}))))"), t))
            }
            "recip" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.recip()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if e != &t {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `{}`", self.muestra(e), self.muestra(&t)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let uno = if t == CType::F32 { "(1.0f)" } else { "(1.0)" };
                Ok(ExVal::puro(format!("(({uno} / ({c})))"), t))
            }
            "fract" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.fract()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if e != &t {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `{}`", self.muestra(e), self.muestra(&t)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let tt = self.temp(t.clone())?;
                self.emite(format!("{tt} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let f = if t == CType::F32 { "truncf" } else { "trunc" };
                Ok(ExVal::puro(format!("((({tt}) - ({f}({tt}))))"), t))
            }
            "to_degrees" | "to_radians" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", format!("`.{m}()` no lleva argumentos"), x));
                }
                if let Some(e) = esp {
                    if e != &t {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `{}`", self.muestra(e), self.muestra(&t)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let (num, den) = if t == CType::F32 {
                    ("180.0f", "3.141592653589793f")
                } else {
                    ("180.0", "3.141592653589793")
                };
                if m == "to_degrees" {
                    Ok(ExVal::puro(format!("((({c}) * ({num} / {den})))"), t))
                } else {
                    Ok(ExVal::puro(format!("((({c}) * ({den} / {num})))"), t))
                }
            }
            "copysign" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`.copysign()` lleva otro valor".into(), x));
                }
                if let Some(e) = esp {
                    if e != &t {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `{}`", self.muestra(e), self.muestra(&t)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let v = self.baja_expr(&args[0], Some(&t))?;
                let co = self.consume(v, Some(&t))?;
                let f = if t == CType::F32 { "copysignf" } else { "copysign" };
                Ok(ExVal::puro(format!("(({f}(({c}), ({co}))))"), t))
            }
            "is_nan" | "is_infinite" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", format!("`.{m}()` no lleva argumentos"), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Bool {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da `bool`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let f = if m == "is_nan" { "isnan" } else { "isinf" };
                Ok(ExVal::puro(format!("((({f}({c})) != 0))"), CType::Bool))
            }
            "is_finite" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.is_finite()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Bool {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `bool`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let tt = self.temp(t.clone())?;
                self.emite(format!("{tt} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                Ok(ExVal::puro(format!("(((isnan({tt}) == 0) && (isinf({tt}) == 0)))"), CType::Bool))
            }
            "to_bits" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.to_bits()` no lleva argumentos".into(), x));
                }
                let tu = if t == CType::F32 { CType::U32 } else { CType::U64 };
                if let Some(e) = esp {
                    if e != &tu {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.to_bits()` da `{}`", self.muestra(e), self.muestra(&tu)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let tt = self.temp(t.clone())?;
                self.emite(format!("{tt} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let tu2 = self.temp(tu.clone())?;
                let nb = if t == CType::F32 { 4 } else { 8 };
                self.emite(format!("memcpy(&({tu2}), &({tt}), {nb});"));
                Ok(ExVal::puro(tu2, tu))
            }
            _ => Err(self.err_en("C0002", format!("flotante `{m}` no cableado (bug interno)"), x)),
        }
    }

    /// Métodos de `char` (ASCII exactos; los unicode dan C0003).
    fn ex_caracter(
        &mut self,
        b: ExVal,
        base: &CType,
        d: usize,
        m: &str,
        _tb: &[CType],
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        esp: Option<&CType>,
        x: &syn::ExprMethodCall,
    ) -> Result<Option<ExVal>, CError> {
        if *base != CType::Char {
            return Ok(None);
        }
        let bool_esp = |s: &Self, esp: Option<&CType>| -> Result<(), CError> {
            if let Some(e) = esp {
                if *e != CType::Bool {
                    return Err(s.err_en(
                        "C0005",
                        format!("se esperaba `{}`, da `bool`", s.muestra(e)),
                        x,
                    ));
                }
            }
            Ok(())
        };
        match m {
            "len_utf8" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.len_utf8()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Usize {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.len_utf8()` da `usize`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let tt = self.temp(CType::U32)?;
                self.emite(format!("{tt} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                Ok(Some(ExVal::puro(
                    format!("(((({tt}) < 0x80) ? 1 : ((({tt}) < 0x800) ? 2 : ((({tt}) < 0x10000) ? 3 : 4))))"),
                    CType::Usize,
                )))
            }
            "is_ascii" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.is_ascii()` no lleva argumentos".into(), x));
                }
                bool_esp(self, esp)?;
                let c = self.recv_num(b, base, d)?;
                Ok(Some(ExVal::puro(format!("((({c}) < 0x80))"), CType::Bool)))
            }
            "is_ascii_alphabetic" | "is_ascii_alphanumeric" | "is_ascii_digit"
            | "is_ascii_hexdigit" | "is_ascii_lowercase" | "is_ascii_uppercase"
            | "is_ascii_whitespace" | "is_ascii_graphic" | "is_ascii_punctuation"
            | "is_ascii_control" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", format!("`.{m}()` no lleva argumentos"), x));
                }
                bool_esp(self, esp)?;
                let c = self.recv_num(b, base, d)?;
                let tt = self.temp(CType::U32)?;
                self.emite(format!("{tt} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let l = format!("((({tt}) >= 'a') && (({tt}) <= 'z'))");
                let u = format!("((({tt}) >= 'A') && (({tt}) <= 'Z'))");
                let dg = format!("((({tt}) >= '0') && (({tt}) <= '9'))");
                let e = match m {
                    "is_ascii_alphabetic" => format!("(({l} || {u}))"),
                    "is_ascii_alphanumeric" => format!("(({l} || {u} || {dg}))"),
                    "is_ascii_digit" => dg,
                    "is_ascii_hexdigit" => format!(
                        "(({dg} || ((({tt}) >= 'a') && (({tt}) <= 'f')) || ((({tt}) >= 'A') && (({tt}) <= 'F'))))"
                    ),
                    "is_ascii_lowercase" => l,
                    "is_ascii_uppercase" => u,
                    "is_ascii_whitespace" => format!(
                        "(((({tt}) == ' ') || (({tt}) == '\\t') || (({tt}) == '\\n') || (({tt}) == 0x0C) || (({tt}) == '\\r')))"
                    ),
                    "is_ascii_graphic" => format!("((({tt}) >= 0x21) && (({tt}) <= 0x7E))"),
                    "is_ascii_punctuation" => {
                        format!("((((({tt}) >= 0x21) && (({tt}) <= 0x7E)) && !({l} || {u} || {dg})))")
                    }
                    _ => format!("((({tt}) < 0x20) || (({tt}) == 0x7F))"),
                };
                Ok(Some(ExVal::puro(e, CType::Bool)))
            }
            "is_digit" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`.is_digit()` lleva la base".into(), x));
                }
                bool_esp(self, esp)?;
                let c = self.recv_num(b, base, d)?;
                let v = self.baja_expr(&args[0], Some(&CType::U32))?;
                let cr = self.consume(v, Some(&CType::U32))?;
                let tt = self.temp(CType::U32)?;
                self.emite(format!("{tt} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let tr2 = self.temp(CType::U32)?;
                self.emite(format!("{tr2} = ({cr});"));
                self.marca_movida_si_temp_pub(&cr);
                // Valor del dígito (u32::MAX si no es dígito) y compara con la base.
                let tv = self.temp(CType::U32)?;
                self.emite(format!("{tv} = 0xFFFFFFFFu;"));
                self.emite(format!("if ((({tt}) >= '0') && (({tt}) <= '9')) {{ ({tv}) = (({tt}) - '0'); }}"));
                self.emite(format!(
                    "else if ((({tt}) >= 'a') && (({tt}) <= 'z')) {{ ({tv}) = (({tt}) - 'a' + 10); }}"
                ));
                self.emite(format!(
                    "else if ((({tt}) >= 'A') && (({tt}) <= 'Z')) {{ ({tv}) = (({tt}) - 'A' + 10); }}"
                ));
                Ok(Some(ExVal::puro(
                    format!("((((({tr2}) >= 2) && (({tr2}) <= 36)) && (({tv}) < ({tr2}))))"),
                    CType::Bool,
                )))
            }
            "to_digit" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`.to_digit()` lleva la base".into(), x));
                }
                let t_r = CType::Opcion(Box::new(CType::U32));
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.to_digit()` da `Option<u32>`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let c = self.recv_num(b, base, d)?;
                let v = self.baja_expr(&args[0], Some(&CType::U32))?;
                let cr = self.consume(v, Some(&CType::U32))?;
                let tt = self.temp(CType::U32)?;
                self.emite(format!("{tt} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let tr2 = self.temp(CType::U32)?;
                self.emite(format!("{tr2} = ({cr});"));
                self.marca_movida_si_temp_pub(&cr);
                let tv = self.temp(CType::U32)?;
                self.emite(format!("{tv} = 0xFFFFFFFFu;"));
                self.emite(format!("if ((({tt}) >= '0') && (({tt}) <= '9')) {{ ({tv}) = (({tt}) - '0'); }}"));
                self.emite(format!(
                    "else if ((({tt}) >= 'a') && (({tt}) <= 'z')) {{ ({tv}) = (({tt}) - 'a' + 10); }}"
                ));
                self.emite(format!(
                    "else if ((({tt}) >= 'A') && (({tt}) <= 'Z')) {{ ({tv}) = (({tt}) - 'A' + 10); }}"
                ));
                let to = self.temp(t_r.clone())?;
                self.emite(format!("memset(&({to}), 0, sizeof({to}));"));
                self.emite(format!(
                    "if ((((({tr2}) >= 2) && (({tr2}) <= 36)) && (({tv}) < ({tr2})))) {{ ({to}).tiene = true; ({to}).valor = ({tv}); }}"
                ));
                Ok(Some(ExVal::movible(to.clone(), t_r, to)))
            }
            "to_ascii_lowercase" | "to_ascii_uppercase" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", format!("`.{m}()` no lleva argumentos"), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Char {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `char`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let c = self.recv_num(b, base, d)?;
                let tt = self.temp(CType::U32)?;
                self.emite(format!("{tt} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let e = if m == "to_ascii_lowercase" {
                    format!("((((( {tt}) >= 'A') && (({tt}) <= 'Z')) ? (({tt}) + 32) : ({tt})))")
                } else {
                    format!("((((( {tt}) >= 'a') && (({tt}) <= 'z')) ? (({tt}) - 32) : ({tt})))")
                };
                Ok(Some(ExVal::puro(e, CType::Char)))
            }
            "is_alphabetic" | "is_alphanumeric" | "is_uppercase" | "is_lowercase"
            | "is_whitespace" | "is_control" | "is_numeric" | "to_lowercase" | "to_uppercase"
            | "escape_debug" | "escape_unicode" | "encode_utf8" => {
                let mut e = self.err_en(
                    "C0003",
                    format!("`char.{m}()` necesita tablas unicode o iteradores"),
                    x,
                );
                e.ayuda = Some("usa la familia `.is_ascii_*()` / `.to_ascii_*()`".into());
                Err(e)
            }
            _ => Ok(None),
        }
    }

    /// Métodos de `bool` (`then_some`; `then` pide clausura).
    fn ex_bool_met(
        &mut self,
        b: ExVal,
        base: &CType,
        d: usize,
        m: &str,
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        esp: Option<&CType>,
        x: &syn::ExprMethodCall,
    ) -> Result<Option<ExVal>, CError> {
        if *base != CType::Bool {
            return Ok(None);
        }
        match m {
            "then_some" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`.then_some()` lleva un valor".into(), x));
                }
                let guia_in: Option<CType> = match esp {
                    Some(CType::Opcion(inner)) => Some(inner.as_ref().clone()),
                    Some(e) => {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.then_some()` da `Option`", self.muestra(e)),
                            x,
                        ));
                    }
                    None => None,
                };
                let c = self.recv_num(b, base, d)?;
                let tc = self.temp(CType::Bool)?;
                self.emite(format!("{tc} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let v = self.baja_expr(&args[0], guia_in.as_ref())?;
                let t_in = guia_in.clone().unwrap_or_else(|| v.tipo.clone());
                let t_r = CType::Opcion(Box::new(t_in.clone()));
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                // then_some evalúa el valor siempre: a temp, luego if/else.
                let cv = self.consume(v, Some(&t_in))?;
                let tv = self.temp(t_in.clone())?;
                self.emite(format!("{tv} = ({cv});"));
                let to = self.temp(t_r.clone())?;
                self.emite(format!("memset(&({to}), 0, sizeof({to}));"));
                self.emite(format!("if (({tc})) {{"));
                self.mueve_valor_pub(&tv, &format!("({to}).valor"), &t_in)?;
                self.emite(format!("    ({to}).tiene = true;"));
                self.emite("} else {".to_string());
                self.libera_lugar_pub(&tv, &t_in)?;
                self.emite("}".to_string());
                Ok(Some(ExVal::movible(to.clone(), t_r, to)))
            }
            "then" => {
                let mut e = self.err_en("C0003", "`bool.then()` pide una clausura".into(), x);
                e.ayuda = Some("usa `if` o `.then_some()`".into());
                Err(e)
            }
            _ => Ok(None),
        }
    }

    /// ¿Se puede prestar `&mut` por toda la cadena? (dueño o cadena de `&mut`/`Box`).
    fn presta_mut_ok(t: &CType) -> bool {
        let mut a = t;
        loop {
            match a {
                CType::Ref(m, i) => {
                    if !*m {
                        return false;
                    }
                    a = &**i;
                }
                CType::Caja(i) => {
                    a = &**i;
                }
                _ => return true,
            }
        }
    }

    /// `Vec::new()` / `Vec::with_capacity(n)` (el `inner` sale del tipo esperado).
    fn ex_vec_new(
        &mut self,
        cap: Option<String>,
        esp: Option<&CType>,
        x: &syn::ExprCall,
    ) -> Result<ExVal, CError> {
        let inner = match esp {
            Some(CType::Vec(i)) => i.as_ref().clone(),
            Some(e) => {
                return Err(self.err_en(
                    "C0005",
                    format!("se esperaba `{}`, `Vec::new()` da `Vec`", self.muestra(e)),
                    x,
                ))
            }
            None => {
                return Err(self.err_en(
                    "C0005",
                    "`Vec::new()` necesita tipo esperado (anota el `let`)".into(),
                    x,
                ))
            }
        };
        let t = CType::Vec(Box::new(inner.clone()));
        self.registra_mono(&t).map_err(|e| self.con_archivo(e))?;
        let tr = self.temp(t.clone())?;
        match cap {
            None => {
                self.emite(format!("memset(&({tr}), 0, sizeof({tr}));"));
            }
            Some(c) => {
                let tc = self.temp(CType::Usize)?;
                self.emite(format!("{tc} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                let se = inner.deletrea().map_err(|e| self.con_archivo(e))?;
                self.emite(format!("({tr}).largo = 0;"));
                self.emite(format!("({tr}).capacidad = ({tc});"));
                self.emite(format!(
                    "({tr}).datos = (({tc}) ? malloc((({tc}) * sizeof({se}))) : NULL);"
                ));
                self.emite(format!(
                    "if ((({tc}) != 0) && (({tr}).datos == NULL)) {{ KAMI_PANICO(\"sin memoria\"); }}"
                ));
            }
        }
        Ok(ExVal::movible(tr.clone(), t, tr))
    }

    /// Lugar mutable del vector (`&mut` en la cadena; `d` pasos de autoderef).
    fn recv_vec_mut(
        &mut self,
        b: ExVal,
        base: &CType,
        d: usize,
        m: &str,
        x: &syn::ExprMethodCall,
    ) -> Result<String, CError> {
        if !Self::presta_mut_ok(&b.tipo) {
            return Err(self.err_en(
                "C0003",
                format!("`.{m}()` pide `&mut` en toda la cadena"),
                &x.receiver,
            ));
        }
        self.recv_lugar(b, base, d)
    }

    /// Crece el vector (`nueva` nombres distintos por llamada).
    fn vec_crece(&mut self, pl: &str, inner: &CType) -> Result<(), CError> {
        let se = inner.deletrea().map_err(|e| self.con_archivo(e))?;
        let tn = self.temp(CType::Usize)?;
        self.emite(format!(
            "{tn} = (({pl}).capacidad ? (({pl}).capacidad * 2) : 4);"
        ));
        self.emite(format!(
            "({pl}).datos = realloc(({pl}).datos, (({tn}) * sizeof({se})));"
        ));
        self.emite(format!(
            "if ((({pl}).datos == NULL)) {{ KAMI_PANICO(\"sin memoria\"); }}"
        ));
        self.emite(format!("({pl}).capacidad = ({tn});"));
        Ok(())
    }

    /// Métodos de `Vec` (núcleo: largos, `push`/`pop`, huecos, orden).
    fn ex_vec(
        &mut self,
        b: ExVal,
        base: &CType,
        d: usize,
        _mut_ext: Option<bool>,
        m: &str,
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        esp: Option<&CType>,
        x: &syn::ExprMethodCall,
    ) -> Result<Option<ExVal>, CError> {
        let inner = match base {
            CType::Vec(i) => i.as_ref().clone(),
            _ => return Ok(None),
        };
        match m {
            "len" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.len()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Usize {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.len()` da `usize`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let pl = self.recv_lugar(b, base, d)?;
                Ok(Some(ExVal::puro(format!("((({pl}).largo))"), CType::Usize)))
            }
            "is_empty" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.is_empty()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Bool {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `bool`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let pl = self.recv_lugar(b, base, d)?;
                Ok(Some(ExVal::puro(format!("((({pl}).largo == 0))"), CType::Bool)))
            }
            "capacity" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.capacity()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Usize {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.capacity()` da `usize`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let pl = self.recv_lugar(b, base, d)?;
                Ok(Some(ExVal::puro(format!("((({pl}).capacidad))"), CType::Usize)))
            }
            "push" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`.push()` lleva un valor".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Vacio {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.push()` da `()`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let pl = self.recv_vec_mut(b, base, d, m, x)?;
                let v = self.baja_expr(&args[0], Some(&inner))?;
                let cv = self.consume(v, Some(&inner))?;
                let tv = self.temp(inner.clone())?;
                self.emite(format!("{tv} = ({cv});"));
                self.emite(format!("if ((({pl}).largo >= ({pl}).capacidad)) {{"));
                self.vec_crece(&pl, &inner)?;
                self.emite("}".to_string());
                self.emite(format!("({pl}).datos[({pl}).largo] = ({tv});"));
                self.emite(format!("({pl}).largo = (({pl}).largo + 1);"));
                self.marca_movida_si_temp_pub(&tv);
                Ok(Some(ExVal::puro("0".into(), CType::Vacio)))
            }
            "pop" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.pop()` no lleva argumentos".into(), x));
                }
                let t_r = CType::Opcion(Box::new(inner.clone()));
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.pop()` da `Option`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let pl = self.recv_vec_mut(b, base, d, m, x)?;
                let to = self.temp(t_r.clone())?;
                self.emite(format!("memset(&({to}), 0, sizeof({to}));"));
                self.emite(format!("if ((({pl}).largo > 0)) {{"));
                self.emite(format!("    ({pl}).largo = (({pl}).largo - 1);"));
                self.emite(format!("    ({to}).valor = (({pl}).datos[({pl}).largo]);"));
                self.emite(format!("    ({to}).tiene = true;"));
                self.emite("}".to_string());
                Ok(Some(ExVal::movible(to.clone(), t_r, to)))
            }
            "clear" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.clear()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Vacio {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.clear()` da `()`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let pl = self.recv_vec_mut(b, base, d, m, x)?;
                if self.necesita_drop(&inner) {
                    let ti = self.temp(CType::Usize)?;
                    self.emite(format!("for (({ti}) = 0; ({ti}) < (({pl}).largo); ({ti})++) {{"));
                    self.libera_lugar_pub(&format!("({pl}).datos[{ti}]"), &inner)?;
                    self.emite("}".to_string());
                }
                self.emite(format!("({pl}).largo = 0;"));
                Ok(Some(ExVal::puro("0".into(), CType::Vacio)))
            }
            "truncate" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`.truncate()` lleva un largo".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Vacio {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.truncate()` da `()`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let pl = self.recv_vec_mut(b, base, d, m, x)?;
                let v = self.baja_expr(&args[0], Some(&CType::Usize))?;
                let cn = self.consume(v, Some(&CType::Usize))?;
                let tn = self.temp(CType::Usize)?;
                self.emite(format!("{tn} = ({cn});"));
                self.marca_movida_si_temp_pub(&cn);
                self.emite(format!("if ((({tn}) < ({pl}).largo)) {{"));
                if self.necesita_drop(&inner) {
                    let ti = self.temp(CType::Usize)?;
                    self.emite(format!("    for (({ti}) = ({tn}); ({ti}) < (({pl}).largo); ({ti})++) {{"));
                    self.libera_lugar_pub(&format!("({pl}).datos[{ti}]"), &inner)?;
                    self.emite("    }".to_string());
                }
                self.emite(format!("    ({pl}).largo = ({tn});"));
                self.emite("}".to_string());
                Ok(Some(ExVal::puro("0".into(), CType::Vacio)))
            }
            "insert" => {
                if args.len() != 2 {
                    return Err(self.err_en("C0005", "`.insert()` lleva índice y valor".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Vacio {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.insert()` da `()`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let pl = self.recv_vec_mut(b, base, d, m, x)?;
                let vi = self.baja_expr(&args[0], Some(&CType::Usize))?;
                let ci = self.consume(vi, Some(&CType::Usize))?;
                let ti = self.temp(CType::Usize)?;
                self.emite(format!("{ti} = ({ci});"));
                self.marca_movida_si_temp_pub(&ci);
                let v = self.baja_expr(&args[1], Some(&inner))?;
                let cv = self.consume(v, Some(&inner))?;
                let tv = self.temp(inner.clone())?;
                self.emite(format!("{tv} = ({cv});"));
                self.emite(format!(
                    "if ((({ti}) > ({pl}).largo)) {{ KAMI_PANICO(\"índice fuera de rango\"); }}"
                ));
                self.emite(format!("if ((({pl}).largo >= ({pl}).capacidad)) {{"));
                self.vec_crece(&pl, &inner)?;
                self.emite("}".to_string());
                let se = inner.deletrea().map_err(|e| self.con_archivo(e))?;
                self.emite(format!(
                    "memmove(&(({pl}).datos[({ti}) + 1]), &(({pl}).datos[({ti})]), ((({pl}).largo - ({ti})) * sizeof({se})));"
                ));
                self.emite(format!("({pl}).datos[({ti})] = ({tv});"));
                self.emite(format!("({pl}).largo = (({pl}).largo + 1);"));
                self.marca_movida_si_temp_pub(&tv);
                Ok(Some(ExVal::puro("0".into(), CType::Vacio)))
            }
            "remove" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`.remove()` lleva un índice".into(), x));
                }
                if let Some(e) = esp {
                    if e != &inner {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.remove()` da `{}`", self.muestra(e), self.muestra(&inner)),
                            x,
                        ));
                    }
                }
                let pl = self.recv_vec_mut(b, base, d, m, x)?;
                let vi = self.baja_expr(&args[0], Some(&CType::Usize))?;
                let ci = self.consume(vi, Some(&CType::Usize))?;
                let ti = self.temp(CType::Usize)?;
                self.emite(format!("{ti} = ({ci});"));
                self.marca_movida_si_temp_pub(&ci);
                self.emite(format!(
                    "if ((({ti}) >= ({pl}).largo)) {{ KAMI_PANICO(\"índice fuera de rango\"); }}"
                ));
                let tr = self.temp(inner.clone())?;
                self.mueve_valor_pub(&format!("({pl}).datos[{ti}]"), &tr, &inner)?;
                let se = inner.deletrea().map_err(|e| self.con_archivo(e))?;
                self.emite(format!(
                    "memmove(&(({pl}).datos[({ti})]), &(({pl}).datos[({ti}) + 1]), ((({pl}).largo - ({ti}) - 1) * sizeof({se})));"
                ));
                self.emite(format!("({pl}).largo = (({pl}).largo - 1);"));
                Ok(Some(ExVal::movible(tr.clone(), inner.clone(), tr)))
            }
            "swap_remove" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`.swap_remove()` lleva un índice".into(), x));
                }
                if let Some(e) = esp {
                    if e != &inner {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `{}`", self.muestra(e), self.muestra(&inner)),
                            x,
                        ));
                    }
                }
                let pl = self.recv_vec_mut(b, base, d, m, x)?;
                let vi = self.baja_expr(&args[0], Some(&CType::Usize))?;
                let ci = self.consume(vi, Some(&CType::Usize))?;
                let ti = self.temp(CType::Usize)?;
                self.emite(format!("{ti} = ({ci});"));
                self.marca_movida_si_temp_pub(&ci);
                self.emite(format!(
                    "if ((({ti}) >= ({pl}).largo)) {{ KAMI_PANICO(\"índice fuera de rango\"); }}"
                ));
                let tr = self.temp(inner.clone())?;
                self.mueve_valor_pub(&format!("({pl}).datos[{ti}]"), &tr, &inner)?;
                self.emite(format!("if ((({ti}) + 1 < ({pl}).largo)) {{"));
                self.emite(format!("    ({pl}).datos[({ti})] = (({pl}).datos[({pl}).largo - 1]);"));
                self.emite("}".to_string());
                self.emite(format!("({pl}).largo = (({pl}).largo - 1);"));
                Ok(Some(ExVal::movible(tr.clone(), inner.clone(), tr)))
            }
            "swap" => {
                if args.len() != 2 {
                    return Err(self.err_en("C0005", "`.swap()` lleva dos índices".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Vacio {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.swap()` da `()`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let pl = self.recv_vec_mut(b, base, d, m, x)?;
                let v0 = self.baja_expr(&args[0], Some(&CType::Usize))?;
                let c0 = self.consume(v0, Some(&CType::Usize))?;
                let v1 = self.baja_expr(&args[1], Some(&CType::Usize))?;
                let c1 = self.consume(v1, Some(&CType::Usize))?;
                let t0 = self.temp(CType::Usize)?;
                self.emite(format!("{t0} = ({c0});"));
                self.marca_movida_si_temp_pub(&c0);
                let t1 = self.temp(CType::Usize)?;
                self.emite(format!("{t1} = ({c1});"));
                self.marca_movida_si_temp_pub(&c1);
                self.emite(format!(
                    "if ((({t0}) >= ({pl}).largo) || (({t1}) >= ({pl}).largo)) {{ KAMI_PANICO(\"índice fuera de rango\"); }}"
                ));
                let te = self.temp(inner.clone())?;
                self.emite(format!("{te} = (({pl}).datos[({t0})]);"));
                self.emite(format!("({pl}).datos[({t0})] = (({pl}).datos[({t1})]);"));
                self.emite(format!("({pl}).datos[({t1})] = ({te});"));
                Ok(Some(ExVal::puro("0".into(), CType::Vacio)))
            }
            "reverse" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.reverse()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Vacio {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.reverse()` da `()`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let pl = self.recv_vec_mut(b, base, d, m, x)?;
                let ti = self.temp(CType::Usize)?;
                let te = self.temp(inner.clone())?;
                self.emite(format!("for (({ti}) = 0; ({ti}) < (({pl}).largo / 2); ({ti})++) {{"));
                self.emite(format!("    ({te}) = (({pl}).datos[({ti})]);"));
                self.emite(format!("    ({pl}).datos[({ti})] = (({pl}).datos[({pl}).largo - 1 - ({ti})]);"));
                self.emite(format!("    ({pl}).datos[({pl}).largo - 1 - ({ti})] = ({te});"));
                self.emite("}".to_string());
                Ok(Some(ExVal::puro("0".into(), CType::Vacio)))
            }
            "reserve" | "reserve_exact" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", format!("`.{m}()` lleva un extra"), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Vacio {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da `()`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let pl = self.recv_vec_mut(b, base, d, m, x)?;
                let v = self.baja_expr(&args[0], Some(&CType::Usize))?;
                let cn = self.consume(v, Some(&CType::Usize))?;
                let tn = self.temp(CType::Usize)?;
                self.emite(format!("{tn} = (({cn}) + ({pl}).largo);"));
                self.marca_movida_si_temp_pub(&cn);
                self.emite(format!("if ((({tn}) > ({pl}).capacidad)) {{"));
                let se = inner.deletrea().map_err(|e| self.con_archivo(e))?;
                self.emite(format!(
                    "    ({pl}).datos = realloc(({pl}).datos, (({tn}) * sizeof({se})));"
                ));
                self.emite("    if ((({pl}).datos == NULL)) {{ KAMI_PANICO(\"sin memoria\"); }}".replace("{pl}", &pl));
                self.emite(format!("    ({pl}).capacidad = ({tn});"));
                self.emite("}".to_string());
                Ok(Some(ExVal::puro("0".into(), CType::Vacio)))
            }
            "shrink_to_fit" | "shrink_to" => {
                if m == "shrink_to" && args.len() != 1 {
                    return Err(self.err_en("C0005", "`.shrink_to()` lleva un mínimo".into(), x));
                }
                if m != "shrink_to" && !args.is_empty() {
                    return Err(self.err_en("C0005", "`.shrink_to_fit()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Vacio {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da `()`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let pl = self.recv_vec_mut(b, base, d, m, x)?;
                let tmin = if m == "shrink_to" {
                    let v = self.baja_expr(&args[0], Some(&CType::Usize))?;
                    let cn = self.consume(v, Some(&CType::Usize))?;
                    let t = self.temp(CType::Usize)?;
                    self.emite(format!("{t} = ({cn});"));
                    self.marca_movida_si_temp_pub(&cn);
                    t
                } else {
                    String::new()
                };
                let se = inner.deletrea().map_err(|e| self.con_archivo(e))?;
                let tn = self.temp(CType::Usize)?;
                if m == "shrink_to" {
                    self.emite(format!("{tn} = ((({tmin}) > ({pl}).largo) ? ({tmin}) : ({pl}).largo);"));
                } else {
                    self.emite(format!("{tn} = ({pl}).largo;"));
                }
                self.emite("{".to_string());
                self.emite(format!("if ((({tn}) < ({pl}).capacidad)) {{"));
                self.emite(format!("    if ((({tn}) == 0)) {{"));
                self.emite(format!("        free(({pl}).datos);"));
                self.emite(format!("        ({pl}).datos = NULL;"));
                self.emite(format!("        ({pl}).capacidad = 0;"));
                self.emite("    } else {".to_string());
                self.emite(format!(
                    "        void *__enc = realloc(({pl}).datos, (({tn}) * sizeof({se})));"
                ));
                self.emite("        if ((__enc != NULL)) {".to_string());
                self.emite(format!("            ({pl}).datos = __enc;"));
                self.emite(format!("            ({pl}).capacidad = ({tn});"));
                self.emite("        }".to_string());
                self.emite("    }".to_string());
                self.emite("}".to_string());
                self.emite("}".to_string());
                Ok(Some(ExVal::puro("0".into(), CType::Vacio)))
            }
            "as_ptr" | "as_mut_ptr" | "spare_capacity_mut" => {
                let mut e = self.err_en("C0003", format!("`Vec.{m}()` da un puntero crudo"), x);
                e.ayuda = Some("los punteros crudos no tienen soporte; usa índices o rebanadas".into());
                Err(e)
            }
            "first" | "last" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", format!("`.{m}()` no lleva argumentos"), x));
                }
                let t_r = CType::Opcion(Box::new(CType::Ref(false, Box::new(inner.clone()))));
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da `Option<&T>`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let pl = self.recv_lugar(b, base, d)?;
                let to = self.temp(t_r.clone())?;
                self.emite(format!("memset(&({to}), 0, sizeof({to}));"));
                let idx = if m == "first" { "0".to_string() } else { format!("(({pl}).largo - 1)") };
                self.emite(format!("if ((({pl}).largo > 0)) {{"));
                self.emite(format!("    ({to}).tiene = true;"));
                self.emite(format!("    ({to}).valor = &(({pl}).datos[{idx}]);"));
                self.emite("}".to_string());
                Ok(Some(ExVal::movible(to.clone(), t_r, to)))
            }
            "first_mut" | "last_mut" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", format!("`.{m}()` no lleva argumentos"), x));
                }
                if !Self::presta_mut_ok(&b.tipo) {
                    return Err(self.err_en(
                        "C0003",
                        format!("`.{m}()` pide `&mut` en toda la cadena"),
                        &x.receiver,
                    ));
                }
                let t_r = CType::Opcion(Box::new(CType::Ref(true, Box::new(inner.clone()))));
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da `Option<&mut T>`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let pl = self.recv_lugar(b, base, d)?;
                let to = self.temp(t_r.clone())?;
                self.emite(format!("memset(&({to}), 0, sizeof({to}));"));
                let idx = if m == "first_mut" { "0".to_string() } else { format!("(({pl}).largo - 1)") };
                self.emite(format!("if ((({pl}).largo > 0)) {{"));
                self.emite(format!("    ({to}).tiene = true;"));
                self.emite(format!("    ({to}).valor = &(({pl}).datos[{idx}]);"));
                self.emite("}".to_string());
                Ok(Some(ExVal::movible(to.clone(), t_r, to)))
            }
            "get" | "get_mut" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", format!("`.{m}()` lleva un índice"), x));
                }
                let es_mut = m == "get_mut";
                if es_mut && !Self::presta_mut_ok(&b.tipo) {
                    return Err(self.err_en(
                        "C0003",
                        "`.get_mut()` pide `&mut` en toda la cadena".into(),
                        &x.receiver,
                    ));
                }
                let t_r = CType::Opcion(Box::new(CType::Ref(es_mut, Box::new(inner.clone()))));
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da `Option`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let pl = self.recv_lugar(b, base, d)?;
                let v = self.baja_expr(&args[0], Some(&CType::Usize))?;
                let ci = self.consume(v, Some(&CType::Usize))?;
                let ti = self.temp(CType::Usize)?;
                self.emite(format!("{ti} = ({ci});"));
                self.marca_movida_si_temp_pub(&ci);
                let to = self.temp(t_r.clone())?;
                self.emite(format!("memset(&({to}), 0, sizeof({to}));"));
                self.emite(format!("if ((({ti}) < ({pl}).largo)) {{"));
                self.emite(format!("    ({to}).tiene = true;"));
                self.emite(format!("    ({to}).valor = &(({pl}).datos[({ti})]);"));
                self.emite("}".to_string());
                Ok(Some(ExVal::movible(to.clone(), t_r, to)))
            }
            "as_slice" | "as_mut_slice" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", format!("`.{m}()` no lleva argumentos"), x));
                }
                let es_mut = m == "as_mut_slice";
                if es_mut && !Self::presta_mut_ok(&b.tipo) {
                    return Err(self.err_en(
                        "C0003",
                        "`.as_mut_slice()` pide `&mut` en toda la cadena".into(),
                        &x.receiver,
                    ));
                }
                let t_r = CType::Rebana(Box::new(inner.clone()), es_mut);
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da rebanada", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let n_r = self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let pl = self.recv_lugar(b, base, d)?;
                Ok(Some(ExVal::puro(
                    format!("(({n_r}){{({pl}).datos, ({pl}).largo}})"),
                    t_r,
                )))
            }
            "split_at" | "split_at_mut" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", format!("`.{m}()` lleva un índice"), x));
                }
                let es_mut = m == "split_at_mut";
                if es_mut && !Self::presta_mut_ok(&b.tipo) {
                    return Err(self.err_en(
                        "C0003",
                        "`.split_at_mut()` pide `&mut` en toda la cadena".into(),
                        &x.receiver,
                    ));
                }
                let t_s = CType::Rebana(Box::new(inner.clone()), es_mut);
                let t_r = CType::Tupla(vec![t_s.clone(), t_s.clone()]);
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da par", self.muestra(e)),
                            x,
                        ));
                    }
                }
                self.registra_mono(&t_s).map_err(|e| self.con_archivo(e))?;
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let pl = self.recv_lugar(b, base, d)?;
                let v = self.baja_expr(&args[0], Some(&CType::Usize))?;
                let ci = self.consume(v, Some(&CType::Usize))?;
                let ti = self.temp(CType::Usize)?;
                self.emite(format!("{ti} = ({ci});"));
                self.marca_movida_si_temp_pub(&ci);
                self.emite(format!(
                    "if ((({ti}) > ({pl}).largo)) {{ KAMI_PANICO(\"índice fuera de rango\"); }}"
                ));
                let tr = self.temp(t_r.clone())?;
                self.emite(format!("({tr})._0.datos = (({pl}).datos);"));
                self.emite(format!("({tr})._0.largo = ({ti});"));
                self.emite(format!("({tr})._1.datos = (&(({pl}).datos[({ti})]));"));
                self.emite(format!("({tr})._1.largo = ((({pl}).largo - ({ti})));"));
                Ok(Some(ExVal::movible(tr.clone(), t_r, tr)))
            }
            "split_at_checked" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`.split_at_checked()` lleva un índice".into(), x));
                }
                let t_s = CType::Rebana(Box::new(inner.clone()), false);
                let t_p = CType::Tupla(vec![t_s.clone(), t_s.clone()]);
                let t_r = CType::Opcion(Box::new(t_p.clone()));
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `Option`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                self.registra_mono(&t_s).map_err(|e| self.con_archivo(e))?;
                self.registra_mono(&t_p).map_err(|e| self.con_archivo(e))?;
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let pl = self.recv_lugar(b, base, d)?;
                let v = self.baja_expr(&args[0], Some(&CType::Usize))?;
                let ci = self.consume(v, Some(&CType::Usize))?;
                let ti = self.temp(CType::Usize)?;
                self.emite(format!("{ti} = ({ci});"));
                self.marca_movida_si_temp_pub(&ci);
                let to = self.temp(t_r.clone())?;
                self.emite(format!("memset(&({to}), 0, sizeof({to}));"));
                self.emite(format!("if ((({ti}) <= ({pl}).largo)) {{"));
                self.emite(format!("    ({to}).tiene = true;"));
                self.emite(format!("    ({to}).valor._0.datos = (({pl}).datos);"));
                self.emite(format!("    ({to}).valor._0.largo = ({ti});"));
                self.emite(format!("    ({to}).valor._1.datos = (&(({pl}).datos[({ti})]));"));
                self.emite(format!("    ({to}).valor._1.largo = ((({pl}).largo - ({ti})));"));
                self.emite("}".to_string());
                Ok(Some(ExVal::movible(to.clone(), t_r, to)))
            }
            "split_first" | "split_last" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", format!("`.{m}()` no lleva argumentos"), x));
                }
                let t_s = CType::Rebana(Box::new(inner.clone()), false);
                let t_p = CType::Tupla(vec![
                    CType::Ref(false, Box::new(inner.clone())),
                    t_s.clone(),
                ]);
                let t_r = CType::Opcion(Box::new(t_p.clone()));
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da `Option`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                self.registra_mono(&t_s).map_err(|e| self.con_archivo(e))?;
                self.registra_mono(&t_p).map_err(|e| self.con_archivo(e))?;
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let pl = self.recv_lugar(b, base, d)?;
                let to = self.temp(t_r.clone())?;
                self.emite(format!("memset(&({to}), 0, sizeof({to}));"));
                self.emite(format!("if ((({pl}).largo > 0)) {{"));
                self.emite(format!("    ({to}).tiene = true;"));
                if m == "split_first" {
                    self.emite(format!("    ({to}).valor._0 = &(({pl}).datos[0]);"));
                    self.emite(format!("    ({to}).valor._1.datos = (&(({pl}).datos[1]));"));
                    self.emite(format!("    ({to}).valor._1.largo = ((({pl}).largo - 1));"));
                } else {
                    self.emite(format!("    ({to}).valor._0 = &(({pl}).datos[({pl}).largo - 1]);"));
                    self.emite(format!("    ({to}).valor._1.datos = (({pl}).datos);"));
                    self.emite(format!("    ({to}).valor._1.largo = ((({pl}).largo - 1));"));
                }
                self.emite("}".to_string());
                Ok(Some(ExVal::movible(to.clone(), t_r, to)))
            }
            "get_unchecked" | "get_unchecked_mut" | "first_chunk" | "last_chunk"
            | "first_chunk_mut" | "last_chunk_mut" => {
                let mut e = self.err_en("C0003", format!("`Vec.{m}()` no tiene soporte"), x);
                e.ayuda = Some("usa `.get()` / `.first()` / `.split_at()`".into());
                Err(e)
            }
            "contains" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`.contains()` lleva una referencia".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Bool {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `bool`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                if !(inner.es_numero() || matches!(inner, CType::Bool | CType::Char)) {
                    let mut e = self.err_en(
                        "C0003",
                        format!("`.contains()` sobre `{}` necesita `PartialEq`", self.muestra(&inner)),
                        x,
                    );
                    e.ayuda = Some("compara a mano con un `for`".into());
                    return Err(e);
                }
                let pl = self.recv_lugar(b, base, d)?;
                let t_pr = CType::Ref(false, Box::new(inner.clone()));
                let v = self.baja_expr(&args[0], Some(&t_pr))?;
                let cp = self.consume(v, Some(&t_pr))?;
                let tp = self.temp(t_pr.clone())?;
                self.emite(format!("{tp} = ({cp});"));
                self.marca_movida_si_temp_pub(&cp);
                let tr = self.temp(CType::Bool)?;
                self.emite(format!("{tr} = false;"));
                let ti = self.temp(CType::Usize)?;
                self.emite(format!("for (({ti}) = 0; ({ti}) < (({pl}).largo); ({ti})++) {{"));
                self.emite(format!("    if (((({pl}).datos[({ti})]) == ((*({tp}))))) {{ ({tr}) = true; break; }}"));
                self.emite("}".to_string());
                Ok(Some(ExVal::puro(tr, CType::Bool)))
            }
            "starts_with" | "ends_with" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", format!("`.{m}()` lleva una rebanada"), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Bool {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `bool`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                if !(inner.es_numero() || matches!(inner, CType::Bool | CType::Char)) {
                    let mut e = self.err_en(
                        "C0003",
                        format!("`.{m}()` sobre `{}` necesita `PartialEq`", self.muestra(&inner)),
                        x,
                    );
                    e.ayuda = Some("compara a mano con un `for`".into());
                    return Err(e);
                }
                let pl = self.recv_lugar(b, base, d)?;
                let v = self.baja_expr(&args[0], None)?;
                let t_ag = CType::Rebana(Box::new(inner.clone()), false);
                // Acepta `&[T]` o `&Vec<T>` (vista sobre sus campos).
                let es_reb = matches!(&v.tipo, CType::Rebana(i, _) if i.as_ref() == &inner);
                let es_vref = matches!(&v.tipo, CType::Ref(_, vv) if vv.as_ref() == base);
                let t_v = v.tipo.clone();
                let (nb, nl) = if es_reb {
                    let c = self.consume(v, Some(&t_ag))?;
                    let tn = self.temp(t_ag.clone())?;
                    self.emite(format!("{tn} = ({c});"));
                    (format!("({tn}).datos"), format!("({tn}).largo"))
                } else if es_vref {
                    let c = self.consume(v, None)?;
                    let tp = self.temp(t_v)?;
                    self.emite(format!("{tp} = ({c});"));
                    self.marca_movida_si_temp_pub(&c);
                    (format!("((*({tp}))).datos"), format!("((*({tp}))).largo"))
                } else {
                    return Err(self.err_en(
                        "C0005",
                        format!("`.{m}()` pide `&[T]`, no `{}`", self.muestra(&t_v)),
                        &args[0],
                    ));
                };
                let tr = self.temp(CType::Bool)?;
                self.emite(format!("{tr} = false;"));
                self.emite(format!("if ((({nl}) <= ({pl}).largo)) {{"));
                let ti = self.temp(CType::Usize)?;
                let tq = self.temp(CType::Bool)?;
                self.emite(format!("    ({tq}) = true;"));
                self.emite(format!("    for (({ti}) = 0; ({ti}) < ({nl}); ({ti})++) {{"));
                let lado = if m == "starts_with" {
                    format!("(({pl}).datos[({ti})])")
                } else {
                    format!("(({pl}).datos[({pl}).largo - ({nl}) + ({ti})])")
                };
                self.emite(format!("        if ((({lado}) != (({nb}[({ti})]))) {{ ({tq}) = false; break; }}"));
                self.emite("    }".to_string());
                self.emite(format!("    ({tr}) = ({tq});"));
                self.emite("}".to_string());
                Ok(Some(ExVal::puro(tr, CType::Bool)))
            }
            "strip_prefix" | "strip_suffix" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", format!("`.{m}()` lleva una rebanada"), x));
                }
                if !(inner.es_numero() || matches!(inner, CType::Bool | CType::Char)) {
                    let mut e = self.err_en(
                        "C0003",
                        format!("`.{m}()` sobre `{}` necesita `PartialEq`", self.muestra(&inner)),
                        x,
                    );
                    e.ayuda = Some("compara a mano con un `for`".into());
                    return Err(e);
                }
                let t_s = CType::Rebana(Box::new(inner.clone()), false);
                let t_r = CType::Opcion(Box::new(t_s.clone()));
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da `Option`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                self.registra_mono(&t_s).map_err(|e| self.con_archivo(e))?;
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let pl = self.recv_lugar(b, base, d)?;
                let v = self.baja_expr(&args[0], None)?;
                let t_ag = CType::Rebana(Box::new(inner.clone()), false);
                let es_reb = matches!(&v.tipo, CType::Rebana(i, _) if i.as_ref() == &inner);
                let es_vref = matches!(&v.tipo, CType::Ref(_, vv) if vv.as_ref() == base);
                let t_v = v.tipo.clone();
                let (nb, nl) = if es_reb {
                    let c = self.consume(v, Some(&t_ag))?;
                    let tn = self.temp(t_ag.clone())?;
                    self.emite(format!("{tn} = ({c});"));
                    (format!("({tn}).datos"), format!("({tn}).largo"))
                } else if es_vref {
                    let c = self.consume(v, None)?;
                    let tp = self.temp(t_v)?;
                    self.emite(format!("{tp} = ({c});"));
                    self.marca_movida_si_temp_pub(&c);
                    (format!("((*({tp}))).datos"), format!("((*({tp}))).largo"))
                } else {
                    return Err(self.err_en(
                        "C0005",
                        format!("`.{m}()` pide `&[T]`, no `{}`", self.muestra(&t_v)),
                        &args[0],
                    ));
                };
                let to = self.temp(t_r.clone())?;
                self.emite(format!("memset(&({to}), 0, sizeof({to}));"));
                let ti = self.temp(CType::Usize)?;
                let tq = self.temp(CType::Bool)?;
                self.emite(format!("if ((({nl}) <= ({pl}).largo)) {{"));
                self.emite(format!("    ({tq}) = true;"));
                self.emite(format!("    for (({ti}) = 0; ({ti}) < ({nl}); ({ti})++) {{"));
                let lado = if m == "strip_prefix" {
                    format!("(({pl}).datos[({ti})])")
                } else {
                    format!("(({pl}).datos[({pl}).largo - ({nl}) + ({ti})])")
                };
                self.emite(format!("        if ((({lado}) != (({nb}[({ti})]))) {{ ({tq}) = false; break; }}"));
                self.emite("    }".to_string());
                self.emite(format!("    if (({tq})) {{"));
                self.emite(format!("        ({to}).tiene = true;"));
                if m == "strip_prefix" {
                    self.emite(format!("        ({to}).valor.datos = (&(({pl}).datos[({nl})]));"));
                } else {
                    self.emite(format!("        ({to}).valor.datos = (({pl}).datos);"));
                }
                self.emite(format!("        ({to}).valor.largo = ((({pl}).largo - ({nl})));"));
                self.emite("    }".to_string());
                self.emite("}".to_string());
                Ok(Some(ExVal::movible(to.clone(), t_r, to)))
            }
            "binary_search" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`.binary_search()` lleva una referencia".into(), x));
                }
                let t_r = CType::Resultado(Box::new(CType::Usize), Box::new(CType::Usize));
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `Result`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                if !(inner.es_numero() || matches!(inner, CType::Bool | CType::Char)) {
                    let mut e = self.err_en(
                        "C0003",
                        format!("`.binary_search()` sobre `{}` necesita `Ord`", self.muestra(&inner)),
                        x,
                    );
                    e.ayuda = Some("ordena y busca a mano con un `while`".into());
                    return Err(e);
                }
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let pl = self.recv_lugar(b, base, d)?;
                let t_pr = CType::Ref(false, Box::new(inner.clone()));
                let v = self.baja_expr(&args[0], Some(&t_pr))?;
                let cp = self.consume(v, Some(&t_pr))?;
                let tp = self.temp(t_pr.clone())?;
                self.emite(format!("{tp} = ({cp});"));
                self.marca_movida_si_temp_pub(&cp);
                let tlo = self.temp(CType::Usize)?;
                let thi = self.temp(CType::Usize)?;
                let tmi = self.temp(CType::Usize)?;
                let tok = self.temp(CType::Bool)?;
                self.emite(format!("{tlo} = 0;"));
                self.emite(format!("{thi} = ({pl}).largo;"));
                self.emite(format!("{tok} = false;"));
                self.emite(format!("while ((({tlo}) < ({thi}))) {{"));
                self.emite(format!("    ({tmi}) = ((({tlo}) + ({thi})) / 2);"));
                self.emite(format!("    if (((({pl}).datos[({tmi})]) < ((*({tp}))))) {{ ({tlo}) = (({tmi}) + 1); }}"));
                self.emite(format!("    else if (((({pl}).datos[({tmi})]) == ((*({tp}))))) {{ ({tok}) = true; ({tlo}) = ({tmi}); break; }}"));
                self.emite(format!("    else {{ ({thi}) = ({tmi}); }}"));
                self.emite("}".to_string());
                let to = self.temp(t_r.clone())?;
                self.emite(format!("if (({tok})) {{"));
                self.emite(format!("    ({to}).es_ok = true;"));
                self.emite(format!("    ({to}).datos.ok = ({tlo});"));
                self.emite("} else {".to_string());
                self.emite(format!("    ({to}).es_ok = false;"));
                self.emite(format!("    ({to}).datos.err = ({tlo});"));
                self.emite("}".to_string());
                Ok(Some(ExVal::movible(to.clone(), t_r, to)))
            }
            "is_sorted" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.is_sorted()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Bool {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `bool`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                if !(inner.es_numero() || matches!(inner, CType::Bool | CType::Char)) {
                    let mut e = self.err_en(
                        "C0003",
                        format!("`.is_sorted()` sobre `{}` necesita `Ord`", self.muestra(&inner)),
                        x,
                    );
                    e.ayuda = Some("compara a mano con un `for`".into());
                    return Err(e);
                }
                let pl = self.recv_lugar(b, base, d)?;
                let tr = self.temp(CType::Bool)?;
                self.emite(format!("{tr} = true;"));
                let ti = self.temp(CType::Usize)?;
                self.emite(format!("for (({ti}) = 1; ({ti}) < (({pl}).largo); ({ti})++) {{"));
                self.emite(format!("    if (((({pl}).datos[({ti}) - 1]) > (({pl}).datos[({ti})]))) {{ ({tr}) = false; break; }}"));
                self.emite("}".to_string());
                Ok(Some(ExVal::puro(tr, CType::Bool)))
            }
            "sort" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.sort()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Vacio {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.sort()` da `()`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                if !(inner.es_numero() || matches!(inner, CType::Bool | CType::Char)) {
                    let mut e = self.err_en(
                        "C0003",
                        format!("`.sort()` sobre `{}` necesita `Ord`", self.muestra(&inner)),
                        x,
                    );
                    e.ayuda = Some("ordena a mano con un `for`".into());
                    return Err(e);
                }
                let pl = self.recv_vec_mut(b, base, d, m, x)?;
                // Inserción (O(n²) pero sin `qsort` ni comparadores externos).
                let ti = self.temp(CType::Usize)?;
                let tj = self.temp(CType::Usize)?;
                let te = self.temp(inner.clone())?;
                self.emite(format!("for (({ti}) = 1; ({ti}) < (({pl}).largo); ({ti})++) {{"));
                self.emite(format!("    ({te}) = (({pl}).datos[({ti})]);"));
                self.emite(format!("    ({tj}) = ({ti});"));
                self.emite(format!("    while ((({tj}) > 0) && ((({pl}).datos[({tj}) - 1]) > ({te}))) {{"));
                self.emite(format!("        ({pl}).datos[({tj})] = (({pl}).datos[({tj}) - 1]);"));
                self.emite(format!("        ({tj}) = (({tj}) - 1);"));
                self.emite("    }".to_string());
                self.emite(format!("    ({pl}).datos[({tj})] = ({te});"));
                self.emite("}".to_string());
                Ok(Some(ExVal::puro("0".into(), CType::Vacio)))
            }
            "dedup" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.dedup()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Vacio {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.dedup()` da `()`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                if !(inner.es_numero() || matches!(inner, CType::Bool | CType::Char)) {
                    let mut e = self.err_en(
                        "C0003",
                        format!("`.dedup()` sobre `{}` necesita `PartialEq`", self.muestra(&inner)),
                        x,
                    );
                    e.ayuda = Some("filtra a mano con un `for`".into());
                    return Err(e);
                }
                let pl = self.recv_vec_mut(b, base, d, m, x)?;
                let tr2 = self.temp(CType::Usize)?;
                let ti = self.temp(CType::Usize)?;
                self.emite(format!("{tr2} = 0;"));
                self.emite(format!("for (({ti}) = 0; ({ti}) < (({pl}).largo); ({ti})++) {{"));
                self.emite(format!("    if ((({tr2}) == 0) || (((({pl}).datos[({tr2}) - 1]) != (({pl}).datos[({ti})])))) {{"));
                self.emite(format!("        ({pl}).datos[({tr2})] = (({pl}).datos[({ti})]);"));
                self.emite(format!("        ({tr2}) = (({tr2}) + 1);"));
                self.emite("    }".to_string());
                self.emite("}".to_string());
                if self.necesita_drop(&inner) {
                    self.emite(format!("for (({ti}) = ({tr2}); ({ti}) < (({pl}).largo); ({ti})++) {{"));
                    self.libera_lugar_pub(&format!("({pl}).datos[{ti}]"), &inner)?;
                    self.emite("}".to_string());
                }
                self.emite(format!("({pl}).largo = ({tr2});"));
                Ok(Some(ExVal::puro("0".into(), CType::Vacio)))
            }
            "resize" => {
                if args.len() != 2 {
                    return Err(self.err_en("C0005", "`.resize()` lleva largo y valor".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Vacio {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.resize()` da `()`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                if self.necesita_drop(&inner) {
                    let mut e = self.err_en(
                        "C0003",
                        format!("`.resize()` sobre `{}` necesita `Clone`", self.muestra(&inner)),
                        x,
                    );
                    e.ayuda = Some("llena a mano con `push` en un `for`".into());
                    return Err(e);
                }
                let pl = self.recv_vec_mut(b, base, d, m, x)?;
                let vn = self.baja_expr(&args[0], Some(&CType::Usize))?;
                let cn = self.consume(vn, Some(&CType::Usize))?;
                let tn = self.temp(CType::Usize)?;
                self.emite(format!("{tn} = ({cn});"));
                self.marca_movida_si_temp_pub(&cn);
                let v = self.baja_expr(&args[1], Some(&inner))?;
                let cv = self.consume(v, Some(&inner))?;
                let tv = self.temp(inner.clone())?;
                self.emite(format!("{tv} = ({cv});"));
                self.emite(format!("if ((({tn}) > ({pl}).largo)) {{"));
                let se = inner.deletrea().map_err(|e| self.con_archivo(e))?;
                self.emite(format!("    if ((({tn}) > ({pl}).capacidad)) {{"));
                self.emite(format!("        ({pl}).datos = realloc(({pl}).datos, (({tn}) * sizeof({se})));"));
                self.emite(format!("        if ((({pl}).datos == NULL)) {{ KAMI_PANICO(\"sin memoria\"); }}"));
                self.emite(format!("        ({pl}).capacidad = ({tn});"));
                self.emite("    }".to_string());
                let ti = self.temp(CType::Usize)?;
                self.emite(format!("    for (({ti}) = ({pl}).largo; ({ti}) < ({tn}); ({ti})++) {{"));
                self.emite(format!("        ({pl}).datos[({ti})] = ({tv});"));
                self.emite("    }".to_string());
                self.emite("}".to_string());
                self.emite(format!("({pl}).largo = ({tn});"));
                Ok(Some(ExVal::puro("0".into(), CType::Vacio)))
            }
            "append" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`.append()` lleva otro vector".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Vacio {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.append()` da `()`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let pl = self.recv_vec_mut(b, base, d, m, x)?;
                let v = self.baja_expr(&args[0], None)?;
                match &v.tipo {
                    CType::Ref(_, vv) if vv.as_ref() == base => {}
                    _ => {
                        return Err(self.err_en(
                            "C0005",
                            format!("`.append()` pide `&mut Vec`, no `{}`", self.muestra(&v.tipo)),
                            &args[0],
                        ))
                    }
                }
                let co = self.consume(v, None)?;
                let tp = self.temp(CType::Ref(true, Box::new(base.clone())))?;
                self.emite(format!("{tp} = ({co});"));
                self.marca_movida_si_temp_pub(&co);
                let se = inner.deletrea().map_err(|e| self.con_archivo(e))?;
                let tn = self.temp(CType::Usize)?;
                self.emite(format!("{tn} = ((({pl}).largo + ((*({tp}))).largo));"));
                self.emite(format!("if ((({tn}) > ({pl}).capacidad)) {{"));
                self.emite(format!("    ({pl}).datos = realloc(({pl}).datos, (({tn}) * sizeof({se})));"));
                self.emite(format!("    if ((({pl}).datos == NULL)) {{ KAMI_PANICO(\"sin memoria\"); }}"));
                self.emite(format!("    ({pl}).capacidad = ({tn});"));
                self.emite("}".to_string());
                self.emite(format!(
                    "memcpy(&(({pl}).datos[({pl}).largo]), ((*({tp}))).datos, (((*({tp}))).largo * sizeof({se})));"
                ));
                self.emite(format!("({pl}).largo = ({tn});"));
                self.emite(format!("((*({tp}))).largo = 0;"));
                Ok(Some(ExVal::puro("0".into(), CType::Vacio)))
            }
            "extend_from_slice" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`.extend_from_slice()` lleva una rebanada".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Vacio {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `()`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                if self.necesita_drop(&inner) {
                    let mut e = self.err_en(
                        "C0003",
                        format!("`.extend_from_slice()` sobre `{}` necesita `Clone`", self.muestra(&inner)),
                        x,
                    );
                    e.ayuda = Some("copia a mano con `push` en un `for`".into());
                    return Err(e);
                }
                let pl = self.recv_vec_mut(b, base, d, m, x)?;
                let v = self.baja_expr(&args[0], None)?;
                let t_ag = CType::Rebana(Box::new(inner.clone()), false);
                let es_reb = matches!(&v.tipo, CType::Rebana(i, _) if i.as_ref() == &inner);
                let es_vref = matches!(&v.tipo, CType::Ref(_, vv) if vv.as_ref() == base);
                let t_v = v.tipo.clone();
                let (nb, nl) = if es_reb {
                    let c = self.consume(v, Some(&t_ag))?;
                    let tn2 = self.temp(t_ag.clone())?;
                    self.emite(format!("{tn2} = ({c});"));
                    (format!("({tn2}).datos"), format!("({tn2}).largo"))
                } else if es_vref {
                    let c = self.consume(v, None)?;
                    let tp = self.temp(t_v)?;
                    self.emite(format!("{tp} = ({c});"));
                    self.marca_movida_si_temp_pub(&c);
                    (format!("((*({tp}))).datos"), format!("((*({tp}))).largo"))
                } else {
                    return Err(self.err_en(
                        "C0005",
                        format!("`.extend_from_slice()` pide `&[T]`, no `{}`", self.muestra(&t_v)),
                        &args[0],
                    ));
                };
                let se = inner.deletrea().map_err(|e| self.con_archivo(e))?;
                let tn = self.temp(CType::Usize)?;
                self.emite(format!("{tn} = ((({pl}).largo + ({nl})));"));
                self.emite(format!("if ((({tn}) > ({pl}).capacidad)) {{"));
                self.emite(format!("    ({pl}).datos = realloc(({pl}).datos, (({tn}) * sizeof({se})));"));
                self.emite(format!("    if ((({pl}).datos == NULL)) {{ KAMI_PANICO(\"sin memoria\"); }}"));
                self.emite(format!("    ({pl}).capacidad = ({tn});"));
                self.emite("}".to_string());
                self.emite(format!(
                    "memcpy(&(({pl}).datos[({pl}).largo]), ({nb}), (({nl}) * sizeof({se})));"
                ));
                self.emite(format!("({pl}).largo = ({tn});"));
                Ok(Some(ExVal::puro("0".into(), CType::Vacio)))
            }
            "split_off" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`.split_off()` lleva un índice".into(), x));
                }
                if let Some(e) = esp {
                    if e != base {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.split_off()` da `Vec`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                self.registra_mono(base).map_err(|e| self.con_archivo(e))?;
                let pl = self.recv_vec_mut(b, base, d, m, x)?;
                let v = self.baja_expr(&args[0], Some(&CType::Usize))?;
                let ci = self.consume(v, Some(&CType::Usize))?;
                let ti = self.temp(CType::Usize)?;
                self.emite(format!("{ti} = ({ci});"));
                self.marca_movida_si_temp_pub(&ci);
                self.emite(format!(
                    "if ((({ti}) > ({pl}).largo)) {{ KAMI_PANICO(\"índice fuera de rango\"); }}"
                ));
                let se = inner.deletrea().map_err(|e| self.con_archivo(e))?;
                let tn = self.temp(CType::Usize)?;
                self.emite(format!("{tn} = ((({pl}).largo - ({ti})));"));
                let tr = self.temp(base.clone())?;
                self.emite(format!("({tr}).largo = ({tn});"));
                self.emite(format!("({tr}).capacidad = ({tn});"));
                self.emite(format!(
                    "({tr}).datos = (({tn}) ? malloc((({tn}) * sizeof({se}))) : NULL);"
                ));
                self.emite(format!(
                    "if ((({tn}) != 0) && (({tr}).datos == NULL)) {{ KAMI_PANICO(\"sin memoria\"); }}"
                ));
                self.emite(format!(
                    "memcpy(({tr}).datos, &(({pl}).datos[({ti})]), (({tn}) * sizeof({se})));"
                ));
                self.emite(format!("({pl}).largo = ({ti});"));
                Ok(Some(ExVal::movible(tr.clone(), base.clone(), tr)))
            }
            "join" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`.join()` lleva un separador".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Texto {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.join()` da `String`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                if !matches!(inner, CType::Texto | CType::VistaTexto | CType::Char) {
                    let mut e = self.err_en(
                        "C0003",
                        format!("`.join()` sobre `Vec<{}>` no tiene soporte", self.muestra(&inner)),
                        x,
                    );
                    e.ayuda = Some("une a mano con `push_str` en un `for`".into());
                    return Err(e);
                }
                let pl = self.recv_lugar(b, base, d)?;
                let v = self.baja_expr(&args[0], Some(&CType::VistaTexto))?;
                let cs = self.consume(v, Some(&CType::VistaTexto))?;
                let ts = self.temp(CType::VistaTexto)?;
                self.emite(format!("{ts} = ({cs});"));
                self.marca_movida_si_temp_pub(&cs);
                let tr = self.temp(CType::Texto)?;
                self.emite(format!("{tr} = kami_texto_nuevo();"));
                let ti = self.temp(CType::Usize)?;
                self.emite(format!("for (({ti}) = 0; ({ti}) < (({pl}).largo); ({ti})++) {{"));
                self.emite(format!("    if ((({ti}) > 0)) {{ kami_texto_empuja(&({tr}), ({ts})); }}"));
                match inner {
                    CType::Texto => {
                        self.emite(format!(
                            "    kami_texto_empuja_n(&({tr}), (({pl}).datos[({ti})]).datos, (({pl}).datos[({ti})]).largo);"
                        ));
                    }
                    CType::VistaTexto => {
                        self.emite(format!("    kami_texto_empuja(&({tr}), (({pl}).datos[({ti})]));"));
                    }
                    _ => {
                        self.emite(format!("    kami_texto_empuja_char(&({tr}), (({pl}).datos[({ti})]));"));
                    }
                }
                self.emite("}".to_string());
                Ok(Some(ExVal::movible(tr.clone(), CType::Texto, tr)))
            }
            "concat" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.concat()` no lleva argumentos".into(), x));
                }
                let es_cadena = matches!(inner, CType::Texto | CType::VistaTexto);
                let aplana = match &inner {
                    CType::Vec(u) => !self.necesita_drop(u),
                    _ => false,
                };
                if !es_cadena && !aplana {
                    let mut e = self.err_en(
                        "C0003",
                        format!("`.concat()` sobre `Vec<{}>` no tiene soporte", self.muestra(&inner)),
                        x,
                    );
                    e.ayuda = Some("concatena a mano con `extend`/`push_str` en un `for`".into());
                    return Err(e);
                }
                if es_cadena {
                    if let Some(e) = esp {
                        if *e != CType::Texto {
                            return Err(self.err_en(
                                "C0005",
                                format!("se esperaba `{}`, `.concat()` da `String`", self.muestra(e)),
                                x,
                            ));
                        }
                    }
                    let pl = self.recv_lugar(b, base, d)?;
                    let tr = self.temp(CType::Texto)?;
                    self.emite(format!("{tr} = kami_texto_nuevo();"));
                    let ti = self.temp(CType::Usize)?;
                    self.emite(format!("for (({ti}) = 0; ({ti}) < (({pl}).largo); ({ti})++) {{"));
                    if inner == CType::Texto {
                        self.emite(format!(
                            "    kami_texto_empuja_n(&({tr}), (({pl}).datos[({ti})]).datos, (({pl}).datos[({ti})]).largo);"
                        ));
                    } else {
                        self.emite(format!("    kami_texto_empuja(&({tr}), (({pl}).datos[({ti})]));"));
                    }
                    self.emite("}".to_string());
                    return Ok(Some(ExVal::movible(tr.clone(), CType::Texto, tr)));
                }
                // Aplana un nivel (`Vec<Vec<T>>` con `T: Copy`).
                if let Some(e) = esp {
                    if e != base {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.concat()` da `Vec`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let u = match &inner {
                    CType::Vec(uu) => uu.as_ref().clone(),
                    _ => unreachable!(),
                };
                self.registra_mono(base).map_err(|e| self.con_archivo(e))?;
                let pl = self.recv_lugar(b, base, d)?;
                let se = u.deletrea().map_err(|e| self.con_archivo(e))?;
                let tr = self.temp(base.clone())?;
                self.emite(format!("memset(&({tr}), 0, sizeof({tr}));"));
                let ti = self.temp(CType::Usize)?;
                let tn = self.temp(CType::Usize)?;
                self.emite(format!("for (({ti}) = 0; ({ti}) < (({pl}).largo); ({ti})++) {{"));
                self.emite(format!("    ({tn}) = ((({tr}).largo + (({pl}).datos[({ti})]).largo));"));
                self.emite(format!("    if ((({tn}) > ({tr}).capacidad)) {{"));
                self.emite(format!("        ({tr}).datos = realloc(({tr}).datos, (({tn}) * sizeof({se})));"));
                self.emite(format!("        if ((({tr}).datos == NULL)) {{ KAMI_PANICO(\"sin memoria\"); }}"));
                self.emite(format!("        ({tr}).capacidad = ({tn});"));
                self.emite("    }".to_string());
                self.emite(format!(
                    "    memcpy(&(({tr}).datos[({tr}).largo]), (({pl}).datos[({ti})]).datos, ((({pl}).datos[({ti})]).largo * sizeof({se})));"
                ));
                self.emite(format!("    ({tr}).largo = ({tn});"));
                self.emite("}".to_string());
                Ok(Some(ExVal::movible(tr.clone(), base.clone(), tr)))
            }
            "retain" | "retain_mut" | "dedup_by" | "dedup_by_key" | "partition"
            | "is_partitioned" | "resize_with" | "sort_by" | "sort_by_key"
            | "sort_by_cached_key" | "binary_search_by" | "binary_search_by_key" | "partition_point" => {
                let mut e = self.err_en("C0003", format!("`Vec.{m}()` pide una clausura"), x);
                e.ayuda = Some("las clausuras no tienen soporte; usa un `for`".into());
                Err(e)
            }
            "extend" | "splice" | "drain" | "extract_if" | "leak" | "into_boxed_slice"
            | "push_within_capacity" => {
                let mut e = self.err_en("C0003", format!("`Vec.{m}()` no tiene soporte"), x);
                e.ayuda = Some("usa `push` / `truncate` / `split_off`".into());
                Err(e)
            }
            "iter" | "iter_mut" | "windows" | "chunks" | "chunks_mut" | "chunks_exact"
            | "rchunks" | "split" | "split_mut" | "rsplit" | "splitn" | "split_inclusive" => {
                let mut e = self.err_en("C0003", format!("`Vec.{m}()` da un iterador"), x);
                e.ayuda = Some("los iteradores no tienen soporte; usa índices o `while`".into());
                Err(e)
            }
            _ => Ok(None),
        }
    }

    /// Lugar mutable de rebanada (elementos `&mut` + `&mut` en la cadena).
    fn recv_reb_mut(
        &mut self,
        b: ExVal,
        base: &CType,
        d: usize,
        m: &str,
        x: &syn::ExprMethodCall,
    ) -> Result<String, CError> {
        if !matches!(base, CType::Rebana(_, true)) {
            return Err(self.err_en(
                "C0003",
                format!("`.{m}()` pide rebanada `&mut`"),
                &x.receiver,
            ));
        }
        if !Self::presta_mut_ok(&b.tipo) {
            return Err(self.err_en(
                "C0003",
                format!("`.{m}()` pide `&mut` en toda la cadena"),
                &x.receiver,
            ));
        }
        self.recv_lugar(b, base, d)
    }

    /// Vista `(base, largo)` desde `&[T]` o `&Vec<T>`.
    fn vista_origen(
        &mut self,
        v: ExVal,
        inner: &CType,
        m: &str,
        xe: &syn::Expr,
    ) -> Result<(String, String), CError> {
        let t_ag = CType::Rebana(Box::new(inner.clone()), false);
        let es_reb = matches!(&v.tipo, CType::Rebana(i, _) if i.as_ref() == inner);
        let es_vref = matches!(&v.tipo, CType::Ref(_, vv) if matches!(vv.as_ref(), CType::Vec(i) if i.as_ref() == inner));
        let t_v = v.tipo.clone();
        if es_reb {
            let c = self.consume(v, Some(&t_ag))?;
            let tn = self.temp(t_ag.clone())?;
            self.emite(format!("{tn} = ({c});"));
            Ok((format!("({tn}).datos"), format!("({tn}).largo")))
        } else if es_vref {
            let c = self.consume(v, None)?;
            let tp = self.temp(t_v)?;
            self.emite(format!("{tp} = ({c});"));
            self.marca_movida_si_temp_pub(&c);
            Ok((format!("((*({tp}))).datos"), format!("((*({tp}))).largo")))
        } else {
            Err(self.err_en(
                "C0005",
                format!("`.{m}()` pide `&[T]`, no `{}`", self.muestra(&t_v)),
                xe,
            ))
        }
    }

    /// Métodos de rebanada (`&[T]`: accesores, copias, orden).
    fn ex_rebana(
        &mut self,
        b: ExVal,
        base: &CType,
        d: usize,
        m: &str,
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        esp: Option<&CType>,
        x: &syn::ExprMethodCall,
    ) -> Result<Option<ExVal>, CError> {
        let inner = match base {
            CType::Rebana(i, _) => i.as_ref().clone(),
            _ => return Ok(None),
        };
        match m {
            "len" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.len()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Usize {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.len()` da `usize`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let pl = self.recv_lugar(b, base, d)?;
                Ok(Some(ExVal::puro(format!("((({pl}).largo))"), CType::Usize)))
            }
            "is_empty" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.is_empty()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Bool {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `bool`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let pl = self.recv_lugar(b, base, d)?;
                Ok(Some(ExVal::puro(format!("((({pl}).largo == 0))"), CType::Bool)))
            }
            "first" | "last" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", format!("`.{m}()` no lleva argumentos"), x));
                }
                let t_r = CType::Opcion(Box::new(CType::Ref(false, Box::new(inner.clone()))));
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da `Option<&T>`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let pl = self.recv_lugar(b, base, d)?;
                let to = self.temp(t_r.clone())?;
                self.emite(format!("memset(&({to}), 0, sizeof({to}));"));
                let idx = if m == "first" { "0".to_string() } else { format!("(({pl}).largo - 1)") };
                self.emite(format!("if ((({pl}).largo > 0)) {{"));
                self.emite(format!("    ({to}).tiene = true;"));
                self.emite(format!("    ({to}).valor = &(({pl}).datos[{idx}]);"));
                self.emite("}".to_string());
                Ok(Some(ExVal::movible(to.clone(), t_r, to)))
            }
            "first_mut" | "last_mut" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", format!("`.{m}()` no lleva argumentos"), x));
                }
                let t_r = CType::Opcion(Box::new(CType::Ref(true, Box::new(inner.clone()))));
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da `Option<&mut T>`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let pl = self.recv_reb_mut(b, base, d, m, x)?;
                let to = self.temp(t_r.clone())?;
                self.emite(format!("memset(&({to}), 0, sizeof({to}));"));
                let idx = if m == "first_mut" { "0".to_string() } else { format!("(({pl}).largo - 1)") };
                self.emite(format!("if ((({pl}).largo > 0)) {{"));
                self.emite(format!("    ({to}).tiene = true;"));
                self.emite(format!("    ({to}).valor = &(({pl}).datos[{idx}]);"));
                self.emite("}".to_string());
                Ok(Some(ExVal::movible(to.clone(), t_r, to)))
            }
            "get" | "get_mut" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", format!("`.{m}()` lleva un índice"), x));
                }
                let es_mut = m == "get_mut";
                let t_r = CType::Opcion(Box::new(CType::Ref(es_mut, Box::new(inner.clone()))));
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da `Option`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let pl = if es_mut {
                    self.recv_reb_mut(b, base, d, m, x)?
                } else {
                    self.recv_lugar(b, base, d)?
                };
                let v = self.baja_expr(&args[0], Some(&CType::Usize))?;
                let ci = self.consume(v, Some(&CType::Usize))?;
                let ti = self.temp(CType::Usize)?;
                self.emite(format!("{ti} = ({ci});"));
                self.marca_movida_si_temp_pub(&ci);
                let to = self.temp(t_r.clone())?;
                self.emite(format!("memset(&({to}), 0, sizeof({to}));"));
                self.emite(format!("if ((({ti}) < ({pl}).largo)) {{"));
                self.emite(format!("    ({to}).tiene = true;"));
                self.emite(format!("    ({to}).valor = &(({pl}).datos[({ti})]);"));
                self.emite("}".to_string());
                Ok(Some(ExVal::movible(to.clone(), t_r, to)))
            }
            "split_at" | "split_at_mut" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", format!("`.{m}()` lleva un índice"), x));
                }
                let es_mut = m == "split_at_mut";
                let t_s = CType::Rebana(Box::new(inner.clone()), es_mut);
                let t_r = CType::Tupla(vec![t_s.clone(), t_s.clone()]);
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da par", self.muestra(e)),
                            x,
                        ));
                    }
                }
                self.registra_mono(&t_s).map_err(|e| self.con_archivo(e))?;
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let pl = if es_mut {
                    self.recv_reb_mut(b, base, d, m, x)?
                } else {
                    self.recv_lugar(b, base, d)?
                };
                let v = self.baja_expr(&args[0], Some(&CType::Usize))?;
                let ci = self.consume(v, Some(&CType::Usize))?;
                let ti = self.temp(CType::Usize)?;
                self.emite(format!("{ti} = ({ci});"));
                self.marca_movida_si_temp_pub(&ci);
                self.emite(format!(
                    "if ((({ti}) > ({pl}).largo)) {{ KAMI_PANICO(\"índice fuera de rango\"); }}"
                ));
                let tr = self.temp(t_r.clone())?;
                self.emite(format!("({tr})._0.datos = (({pl}).datos);"));
                self.emite(format!("({tr})._0.largo = ({ti});"));
                self.emite(format!("    ({tr})._1.datos = (&(({pl}).datos[({ti})]));"));
                self.emite(format!("    ({tr})._1.largo = ((({pl}).largo - ({ti})));"));
                Ok(Some(ExVal::movible(tr.clone(), t_r, tr)))
            }
            "split_at_checked" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`.split_at_checked()` lleva un índice".into(), x));
                }
                let t_s = CType::Rebana(Box::new(inner.clone()), false);
                let t_p = CType::Tupla(vec![t_s.clone(), t_s.clone()]);
                let t_r = CType::Opcion(Box::new(t_p.clone()));
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `Option`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                self.registra_mono(&t_s).map_err(|e| self.con_archivo(e))?;
                self.registra_mono(&t_p).map_err(|e| self.con_archivo(e))?;
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let pl = self.recv_lugar(b, base, d)?;
                let v = self.baja_expr(&args[0], Some(&CType::Usize))?;
                let ci = self.consume(v, Some(&CType::Usize))?;
                let ti = self.temp(CType::Usize)?;
                self.emite(format!("{ti} = ({ci});"));
                self.marca_movida_si_temp_pub(&ci);
                let to = self.temp(t_r.clone())?;
                self.emite(format!("memset(&({to}), 0, sizeof({to}));"));
                self.emite(format!("if ((({ti}) <= ({pl}).largo)) {{"));
                self.emite(format!("    ({to}).tiene = true;"));
                self.emite(format!("    ({to}).valor._0.datos = (({pl}).datos);"));
                self.emite(format!("    ({to}).valor._0.largo = ({ti});"));
                self.emite(format!("    ({to}).valor._1.datos = (&(({pl}).datos[({ti})]));"));
                self.emite(format!("    ({to}).valor._1.largo = ((({pl}).largo - ({ti})));"));
                self.emite("}".to_string());
                Ok(Some(ExVal::movible(to.clone(), t_r, to)))
            }
            "split_first" | "split_last" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", format!("`.{m}()` no lleva argumentos"), x));
                }
                let t_s = CType::Rebana(Box::new(inner.clone()), false);
                let t_p = CType::Tupla(vec![
                    CType::Ref(false, Box::new(inner.clone())),
                    t_s.clone(),
                ]);
                let t_r = CType::Opcion(Box::new(t_p.clone()));
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da `Option`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                self.registra_mono(&t_s).map_err(|e| self.con_archivo(e))?;
                self.registra_mono(&t_p).map_err(|e| self.con_archivo(e))?;
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let pl = self.recv_lugar(b, base, d)?;
                let to = self.temp(t_r.clone())?;
                self.emite(format!("memset(&({to}), 0, sizeof({to}));"));
                self.emite(format!("if ((({pl}).largo > 0)) {{"));
                self.emite(format!("    ({to}).tiene = true;"));
                if m == "split_first" {
                    self.emite(format!("    ({to}).valor._0 = &(({pl}).datos[0]);"));
                    self.emite(format!("    ({to}).valor._1.datos = (&(({pl}).datos[1]));"));
                    self.emite(format!("    ({to}).valor._1.largo = ((({pl}).largo - 1));"));
                } else {
                    self.emite(format!("    ({to}).valor._0 = &(({pl}).datos[({pl}).largo - 1]);"));
                    self.emite(format!("    ({to}).valor._1.datos = (({pl}).datos);"));
                    self.emite(format!("    ({to}).valor._1.largo = ((({pl}).largo - 1));"));
                }
                self.emite("}".to_string());
                Ok(Some(ExVal::movible(to.clone(), t_r, to)))
            }
            "to_vec" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.to_vec()` no lleva argumentos".into(), x));
                }
                let t_r = CType::Vec(Box::new(inner.clone()));
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.to_vec()` da `Vec`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                if self.necesita_drop(&inner) {
                    let mut e = self.err_en(
                        "C0003",
                        format!("`.to_vec()` sobre `[{}]` necesita `Clone`", self.muestra(&inner)),
                        x,
                    );
                    e.ayuda = Some("copia a mano con `push` en un `for`".into());
                    return Err(e);
                }
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let pl = self.recv_lugar(b, base, d)?;
                let se = inner.deletrea().map_err(|e| self.con_archivo(e))?;
                let tr = self.temp(t_r.clone())?;
                self.emite(format!("({tr}).largo = (({pl}).largo);"));
                self.emite(format!("({tr}).capacidad = (({pl}).largo);"));
                self.emite(format!(
                    "({tr}).datos = ((({pl}).largo) ? malloc(((({pl}).largo) * sizeof({se}))) : NULL);"
                ));
                self.emite(format!(
                    "if (((({pl}).largo != 0) && (({tr}).datos == NULL))) {{ KAMI_PANICO(\"sin memoria\"); }}"
                ));
                self.emite(format!(
                    "memcpy(({tr}).datos, ({pl}).datos, ((({pl}).largo * sizeof({se}))));"
                ));
                Ok(Some(ExVal::movible(tr.clone(), t_r, tr)))
            }
            "copy_from_slice" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`.copy_from_slice()` lleva otra rebanada".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Vacio {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `()`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                if self.necesita_drop(&inner) {
                    let mut e = self.err_en(
                        "C0003",
                        format!("`.copy_from_slice()` sobre `[{}]` necesita `Copy`", self.muestra(&inner)),
                        x,
                    );
                    e.ayuda = Some("copia a mano con un `for`".into());
                    return Err(e);
                }
                let pl = self.recv_reb_mut(b, base, d, m, x)?;
                let v = self.baja_expr(&args[0], None)?;
                let (nb, nl) = self.vista_origen(v, &inner, m, &args[0])?;
                self.emite(format!(
                    "if ((({nl}) != ({pl}).largo)) {{ KAMI_PANICO(\"largos distintos\"); }}"
                ));
                let se = inner.deletrea().map_err(|e| self.con_archivo(e))?;
                self.emite(format!("memcpy(({pl}).datos, ({nb}), ((({pl}).largo * sizeof({se}))));"));
                Ok(Some(ExVal::puro("0".into(), CType::Vacio)))
            }
            "contains" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`.contains()` lleva una referencia".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Bool {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `bool`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                if !(inner.es_numero() || matches!(inner, CType::Bool | CType::Char)) {
                    let mut e = self.err_en(
                        "C0003",
                        format!("`.contains()` sobre `[{}]` necesita `PartialEq`", self.muestra(&inner)),
                        x,
                    );
                    e.ayuda = Some("compara a mano con un `for`".into());
                    return Err(e);
                }
                let pl = self.recv_lugar(b, base, d)?;
                let t_pr = CType::Ref(false, Box::new(inner.clone()));
                let v = self.baja_expr(&args[0], Some(&t_pr))?;
                let cp = self.consume(v, Some(&t_pr))?;
                let tp = self.temp(t_pr.clone())?;
                self.emite(format!("{tp} = ({cp});"));
                self.marca_movida_si_temp_pub(&cp);
                let tr = self.temp(CType::Bool)?;
                self.emite(format!("{tr} = false;"));
                let ti = self.temp(CType::Usize)?;
                self.emite(format!("for (({ti}) = 0; ({ti}) < (({pl}).largo); ({ti})++) {{"));
                self.emite(format!("    if (((({pl}).datos[({ti})]) == ((*({tp}))))) {{ ({tr}) = true; break; }}"));
                self.emite("}".to_string());
                Ok(Some(ExVal::puro(tr, CType::Bool)))
            }
            "starts_with" | "ends_with" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", format!("`.{m}()` lleva una rebanada"), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Bool {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `bool`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                if !(inner.es_numero() || matches!(inner, CType::Bool | CType::Char)) {
                    let mut e = self.err_en(
                        "C0003",
                        format!("`.{m}()` sobre `[{}]` necesita `PartialEq`", self.muestra(&inner)),
                        x,
                    );
                    e.ayuda = Some("compara a mano con un `for`".into());
                    return Err(e);
                }
                let pl = self.recv_lugar(b, base, d)?;
                let v = self.baja_expr(&args[0], None)?;
                let (nb, nl) = self.vista_origen(v, &inner, m, &args[0])?;
                let tr = self.temp(CType::Bool)?;
                self.emite(format!("{tr} = false;"));
                self.emite(format!("if ((({nl}) <= ({pl}).largo)) {{"));
                let ti = self.temp(CType::Usize)?;
                let tq = self.temp(CType::Bool)?;
                self.emite(format!("    ({tq}) = true;"));
                self.emite(format!("    for (({ti}) = 0; ({ti}) < ({nl}); ({ti})++) {{"));
                let lado = if m == "starts_with" {
                    format!("(({pl}).datos[({ti})])")
                } else {
                    format!("(({pl}).datos[({pl}).largo - ({nl}) + ({ti})])")
                };
                self.emite(format!("        if ((({lado}) != (({nb}[({ti})]))) {{ ({tq}) = false; break; }}"));
                self.emite("    }".to_string());
                self.emite(format!("    ({tr}) = ({tq});"));
                self.emite("}".to_string());
                Ok(Some(ExVal::puro(tr, CType::Bool)))
            }
            "strip_prefix" | "strip_suffix" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", format!("`.{m}()` lleva una rebanada"), x));
                }
                if !(inner.es_numero() || matches!(inner, CType::Bool | CType::Char)) {
                    let mut e = self.err_en(
                        "C0003",
                        format!("`.{m}()` sobre `[{}]` necesita `PartialEq`", self.muestra(&inner)),
                        x,
                    );
                    e.ayuda = Some("compara a mano con un `for`".into());
                    return Err(e);
                }
                let t_s = CType::Rebana(Box::new(inner.clone()), false);
                let t_r = CType::Opcion(Box::new(t_s.clone()));
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da `Option`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                self.registra_mono(&t_s).map_err(|e| self.con_archivo(e))?;
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let pl = self.recv_lugar(b, base, d)?;
                let v = self.baja_expr(&args[0], None)?;
                let (nb, nl) = self.vista_origen(v, &inner, m, &args[0])?;
                let to = self.temp(t_r.clone())?;
                self.emite(format!("memset(&({to}), 0, sizeof({to}));"));
                let ti = self.temp(CType::Usize)?;
                let tq = self.temp(CType::Bool)?;
                self.emite(format!("if ((({nl}) <= ({pl}).largo)) {{"));
                self.emite(format!("    ({tq}) = true;"));
                self.emite(format!("    for (({ti}) = 0; ({ti}) < ({nl}); ({ti})++) {{"));
                let lado = if m == "strip_prefix" {
                    format!("(({pl}).datos[({ti})])")
                } else {
                    format!("(({pl}).datos[({pl}).largo - ({nl}) + ({ti})])")
                };
                self.emite(format!("        if ((({lado}) != (({nb}[({ti})]))) {{ ({tq}) = false; break; }}"));
                self.emite("    }".to_string());
                self.emite(format!("    if (({tq})) {{"));
                self.emite(format!("        ({to}).tiene = true;"));
                if m == "strip_prefix" {
                    self.emite(format!("        ({to}).valor.datos = (&(({pl}).datos[({nl})]));"));
                } else {
                    self.emite(format!("        ({to}).valor.datos = (({pl}).datos);"));
                }
                self.emite(format!("        ({to}).valor.largo = ((({pl}).largo - ({nl})));"));
                self.emite("    }".to_string());
                self.emite("}".to_string());
                Ok(Some(ExVal::movible(to.clone(), t_r, to)))
            }
            "binary_search" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`.binary_search()` lleva una referencia".into(), x));
                }
                let t_r = CType::Resultado(Box::new(CType::Usize), Box::new(CType::Usize));
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `Result`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                if !(inner.es_numero() || matches!(inner, CType::Bool | CType::Char)) {
                    let mut e = self.err_en(
                        "C0003",
                        format!("`.binary_search()` sobre `[{}]` necesita `Ord`", self.muestra(&inner)),
                        x,
                    );
                    e.ayuda = Some("ordena y busca a mano con un `while`".into());
                    return Err(e);
                }
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let pl = self.recv_lugar(b, base, d)?;
                let t_pr = CType::Ref(false, Box::new(inner.clone()));
                let v = self.baja_expr(&args[0], Some(&t_pr))?;
                let cp = self.consume(v, Some(&t_pr))?;
                let tp = self.temp(t_pr.clone())?;
                self.emite(format!("{tp} = ({cp});"));
                self.marca_movida_si_temp_pub(&cp);
                let tlo = self.temp(CType::Usize)?;
                let thi = self.temp(CType::Usize)?;
                let tmi = self.temp(CType::Usize)?;
                let tok = self.temp(CType::Bool)?;
                self.emite(format!("{tlo} = 0;"));
                self.emite(format!("{thi} = ({pl}).largo;"));
                self.emite(format!("{tok} = false;"));
                self.emite(format!("while ((({tlo}) < ({thi}))) {{"));
                self.emite(format!("    ({tmi}) = ((({tlo}) + ({thi})) / 2);"));
                self.emite(format!("    if (((({pl}).datos[({tmi})]) < ((*({tp}))))) {{ ({tlo}) = (({tmi}) + 1); }}"));
                self.emite(format!("    else if (((({pl}).datos[({tmi})]) == ((*({tp}))))) {{ ({tok}) = true; ({tlo}) = ({tmi}); break; }}"));
                self.emite(format!("    else {{ ({thi}) = ({tmi}); }}"));
                self.emite("}".to_string());
                let to = self.temp(t_r.clone())?;
                self.emite(format!("if (({tok})) {{"));
                self.emite(format!("    ({to}).es_ok = true;"));
                self.emite(format!("    ({to}).datos.ok = ({tlo});"));
                self.emite("} else {".to_string());
                self.emite(format!("    ({to}).es_ok = false;"));
                self.emite(format!("    ({to}).datos.err = ({tlo});"));
                self.emite("}".to_string());
                Ok(Some(ExVal::movible(to.clone(), t_r, to)))
            }
            "is_sorted" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.is_sorted()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Bool {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `bool`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                if !(inner.es_numero() || matches!(inner, CType::Bool | CType::Char)) {
                    let mut e = self.err_en(
                        "C0003",
                        format!("`.is_sorted()` sobre `[{}]` necesita `Ord`", self.muestra(&inner)),
                        x,
                    );
                    e.ayuda = Some("compara a mano con un `for`".into());
                    return Err(e);
                }
                let pl = self.recv_lugar(b, base, d)?;
                let tr = self.temp(CType::Bool)?;
                self.emite(format!("{tr} = true;"));
                let ti = self.temp(CType::Usize)?;
                self.emite(format!("for (({ti}) = 1; ({ti}) < (({pl}).largo); ({ti})++) {{"));
                self.emite(format!("    if (((({pl}).datos[({ti}) - 1]) > (({pl}).datos[({ti})]))) {{ ({tr}) = false; break; }}"));
                self.emite("}".to_string());
                Ok(Some(ExVal::puro(tr, CType::Bool)))
            }
            "sort" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.sort()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Vacio {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.sort()` da `()`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                if !(inner.es_numero() || matches!(inner, CType::Bool | CType::Char)) {
                    let mut e = self.err_en(
                        "C0003",
                        format!("`.sort()` sobre `[{}]` necesita `Ord`", self.muestra(&inner)),
                        x,
                    );
                    e.ayuda = Some("ordena a mano con un `for`".into());
                    return Err(e);
                }
                let pl = self.recv_reb_mut(b, base, d, m, x)?;
                let ti = self.temp(CType::Usize)?;
                let tj = self.temp(CType::Usize)?;
                let te = self.temp(inner.clone())?;
                self.emite(format!("for (({ti}) = 1; ({ti}) < (({pl}).largo); ({ti})++) {{"));
                self.emite(format!("    ({te}) = (({pl}).datos[({ti})]);"));
                self.emite(format!("    ({tj}) = ({ti});"));
                self.emite(format!("    while ((({tj}) > 0) && ((({pl}).datos[({tj}) - 1]) > ({te}))) {{"));
                self.emite(format!("        ({pl}).datos[({tj})] = (({pl}).datos[({tj}) - 1]);"));
                self.emite(format!("        ({tj}) = (({tj}) - 1);"));
                self.emite("    }".to_string());
                self.emite(format!("    ({pl}).datos[({tj})] = ({te});"));
                self.emite("}".to_string());
                Ok(Some(ExVal::puro("0".into(), CType::Vacio)))
            }
            "reverse" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.reverse()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Vacio {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.reverse()` da `()`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let pl = self.recv_reb_mut(b, base, d, m, x)?;
                let ti = self.temp(CType::Usize)?;
                let te = self.temp(inner.clone())?;
                self.emite(format!("for (({ti}) = 0; ({ti}) < (({pl}).largo / 2); ({ti})++) {{"));
                self.emite(format!("    ({te}) = (({pl}).datos[({ti})]);"));
                self.emite(format!("    ({pl}).datos[({ti})] = (({pl}).datos[({pl}).largo - 1 - ({ti})]);"));
                self.emite(format!("    ({pl}).datos[({pl}).largo - 1 - ({ti})] = ({te});"));
                self.emite("}".to_string());
                Ok(Some(ExVal::puro("0".into(), CType::Vacio)))
            }
            "swap" => {
                if args.len() != 2 {
                    return Err(self.err_en("C0005", "`.swap()` lleva dos índices".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Vacio {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.swap()` da `()`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let pl = self.recv_reb_mut(b, base, d, m, x)?;
                let v0 = self.baja_expr(&args[0], Some(&CType::Usize))?;
                let c0 = self.consume(v0, Some(&CType::Usize))?;
                let v1 = self.baja_expr(&args[1], Some(&CType::Usize))?;
                let c1 = self.consume(v1, Some(&CType::Usize))?;
                let t0 = self.temp(CType::Usize)?;
                self.emite(format!("{t0} = ({c0});"));
                self.marca_movida_si_temp_pub(&c0);
                let t1 = self.temp(CType::Usize)?;
                self.emite(format!("{t1} = ({c1});"));
                self.marca_movida_si_temp_pub(&c1);
                self.emite(format!(
                    "if ((({t0}) >= ({pl}).largo) || (({t1}) >= ({pl}).largo)) {{ KAMI_PANICO(\"índice fuera de rango\"); }}"
                ));
                let te = self.temp(inner.clone())?;
                self.emite(format!("{te} = (({pl}).datos[({t0})]);"));
                self.emite(format!("({pl}).datos[({t0})] = (({pl}).datos[({t1})]);"));
                self.emite(format!("({pl}).datos[({t1})] = ({te});"));
                Ok(Some(ExVal::puro("0".into(), CType::Vacio)))
            }
            "fill" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`.fill()` lleva un valor".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Vacio {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.fill()` da `()`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                if self.necesita_drop(&inner) {
                    let mut e = self.err_en(
                        "C0003",
                        format!("`.fill()` sobre `[{}]` necesita `Clone`", self.muestra(&inner)),
                        x,
                    );
                    e.ayuda = Some("llena a mano con un `for`".into());
                    return Err(e);
                }
                let pl = self.recv_reb_mut(b, base, d, m, x)?;
                let v = self.baja_expr(&args[0], Some(&inner))?;
                let cv = self.consume(v, Some(&inner))?;
                let tv = self.temp(inner.clone())?;
                self.emite(format!("{tv} = ({cv});"));
                let ti = self.temp(CType::Usize)?;
                self.emite(format!("for (({ti}) = 0; ({ti}) < (({pl}).largo); ({ti})++) {{"));
                self.emite(format!("    ({pl}).datos[({ti})] = ({tv});"));
                self.emite("}".to_string());
                Ok(Some(ExVal::puro("0".into(), CType::Vacio)))
            }
            "copy_within" => {
                if args.len() != 2 {
                    return Err(self.err_en("C0005", "`.copy_within()` lleva rango y destino".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Vacio {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.copy_within()` da `()`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let (ini_e, fin_e) = match &args[0] {
                    syn::Expr::Range(r) => match (&r.start, &r.end) {
                        (Some(s), Some(e)) => (s.as_ref(), e.as_ref()),
                        _ => {
                            return Err(self.err_en(
                                "C0003",
                                "`.copy_within()` pide rango con inicio y fin".into(),
                                &args[0],
                            ))
                        }
                    },
                    _ => {
                        return Err(self.err_en(
                            "C0005",
                            "`.copy_within()` pide un rango `a..b`".into(),
                            &args[0],
                        ))
                    }
                };
                let pl = self.recv_reb_mut(b, base, d, m, x)?;
                let v0 = self.baja_expr(ini_e, Some(&CType::Usize))?;
                let c0 = self.consume(v0, Some(&CType::Usize))?;
                let v1 = self.baja_expr(fin_e, Some(&CType::Usize))?;
                let c1 = self.consume(v1, Some(&CType::Usize))?;
                let v2 = self.baja_expr(&args[1], Some(&CType::Usize))?;
                let c2 = self.consume(v2, Some(&CType::Usize))?;
                let t0 = self.temp(CType::Usize)?;
                self.emite(format!("{t0} = ({c0});"));
                self.marca_movida_si_temp_pub(&c0);
                let t1 = self.temp(CType::Usize)?;
                self.emite(format!("{t1} = ({c1});"));
                self.marca_movida_si_temp_pub(&c1);
                let t2 = self.temp(CType::Usize)?;
                self.emite(format!("{t2} = ({c2});"));
                self.marca_movida_si_temp_pub(&c2);
                self.emite(format!(
                    "if ((({t0}) > ({t1})) || (({t1}) > ({pl}).largo) || ((({t2}) + (({t1}) - ({t0}))) > ({pl}).largo)) {{ KAMI_PANICO(\"índice fuera de rango\"); }}"
                ));
                let se = inner.deletrea().map_err(|e| self.con_archivo(e))?;
                self.emite(format!(
                    "memmove(&(({pl}).datos[({t2})]), &(({pl}).datos[({t0})]), (((({t1}) - ({t0})) * sizeof({se})));"
                ));
                Ok(Some(ExVal::puro("0".into(), CType::Vacio)))
            }
            "rotate_left" | "rotate_right" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", format!("`.{m}()` lleva un monto"), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Vacio {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da `()`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let pl = self.recv_reb_mut(b, base, d, m, x)?;
                let v = self.baja_expr(&args[0], Some(&CType::Usize))?;
                let cn = self.consume(v, Some(&CType::Usize))?;
                let se = inner.deletrea().map_err(|e| self.con_archivo(e))?;
                let tn = self.temp(CType::Usize)?;
                self.emite(format!("{tn} = ({cn});"));
                self.marca_movida_si_temp_pub(&cn);
                self.emite("{".to_string());
                self.emite(format!("    size_t {tn}_k = (({pl}).largo ? (({tn}) % ({pl}).largo) : 0);"));
                if m == "rotate_right" {
                    self.emite(format!("    {tn}_k = (({tn}_k ? (({pl}).largo - {tn}_k) : 0));"));
                }
                self.emite(format!("    if (({tn}_k > 0)) {{"));
                self.emite(format!("        void *{tn}_tmp = malloc((({tn}_k * sizeof({se}))));"));
                self.emite(format!("        if (({tn}_tmp == NULL)) {{ KAMI_PANICO(\"sin memoria\"); }}"));
                self.emite(format!("        memcpy({tn}_tmp, ({pl}).datos, (({tn}_k * sizeof({se}))));"));
                self.emite(format!(
                    "        memmove(({pl}).datos, &(({pl}).datos[{tn}_k]), (((({pl}).largo - {tn}_k) * sizeof({se}))));"
                ));
                self.emite(format!(
                    "        memcpy(&(({pl}).datos[({pl}).largo - {tn}_k]), {tn}_tmp, (({tn}_k * sizeof({se}))));"
                ));
                self.emite(format!("        free({tn}_tmp);"));
                self.emite("    }".to_string());
                self.emite("}".to_string());
                Ok(Some(ExVal::puro("0".into(), CType::Vacio)))
            }
            "join" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`.join()` lleva un separador".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Texto {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.join()` da `String`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                if !matches!(inner, CType::Texto | CType::VistaTexto | CType::Char) {
                    let mut e = self.err_en(
                        "C0003",
                        format!("`.join()` sobre `[{}]` no tiene soporte", self.muestra(&inner)),
                        x,
                    );
                    e.ayuda = Some("une a mano con `push_str` en un `for`".into());
                    return Err(e);
                }
                let pl = self.recv_lugar(b, base, d)?;
                let v = self.baja_expr(&args[0], Some(&CType::VistaTexto))?;
                let cs = self.consume(v, Some(&CType::VistaTexto))?;
                let ts = self.temp(CType::VistaTexto)?;
                self.emite(format!("{ts} = ({cs});"));
                self.marca_movida_si_temp_pub(&cs);
                let tr = self.temp(CType::Texto)?;
                self.emite(format!("{tr} = kami_texto_nuevo();"));
                let ti = self.temp(CType::Usize)?;
                self.emite(format!("for (({ti}) = 0; ({ti}) < (({pl}).largo); ({ti})++) {{"));
                self.emite(format!("    if ((({ti}) > 0)) {{ kami_texto_empuja(&({tr}), ({ts})); }}"));
                match inner {
                    CType::Texto => {
                        self.emite(format!(
                            "    kami_texto_empuja_n(&({tr}), (({pl}).datos[({ti})]).datos, (({pl}).datos[({ti})]).largo);"
                        ));
                    }
                    CType::VistaTexto => {
                        self.emite(format!("    kami_texto_empuja(&({tr}), (({pl}).datos[({ti})]));"));
                    }
                    _ => {
                        self.emite(format!("    kami_texto_empuja_char(&({tr}), (({pl}).datos[({ti})]));"));
                    }
                }
                self.emite("}".to_string());
                Ok(Some(ExVal::movible(tr.clone(), CType::Texto, tr)))
            }
            "concat" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.concat()` no lleva argumentos".into(), x));
                }
                let es_cadena = matches!(inner, CType::Texto | CType::VistaTexto);
                let u_ap = match &inner {
                    CType::Vec(u) => {
                        if self.necesita_drop(u) {
                            None
                        } else {
                            Some(u.as_ref().clone())
                        }
                    }
                    _ => None,
                };
                if !es_cadena && u_ap.is_none() {
                    let mut e = self.err_en(
                        "C0003",
                        format!("`.concat()` sobre `[{}]` no tiene soporte", self.muestra(&inner)),
                        x,
                    );
                    e.ayuda = Some("concatena a mano en un `for`".into());
                    return Err(e);
                }
                if es_cadena {
                    if let Some(e) = esp {
                        if *e != CType::Texto {
                            return Err(self.err_en(
                                "C0005",
                                format!("se esperaba `{}`, `.concat()` da `String`", self.muestra(e)),
                                x,
                            ));
                        }
                    }
                    let pl = self.recv_lugar(b, base, d)?;
                    let tr = self.temp(CType::Texto)?;
                    self.emite(format!("{tr} = kami_texto_nuevo();"));
                    let ti = self.temp(CType::Usize)?;
                    self.emite(format!("for (({ti}) = 0; ({ti}) < (({pl}).largo); ({ti})++) {{"));
                    if inner == CType::Texto {
                        self.emite(format!(
                            "    kami_texto_empuja_n(&({tr}), (({pl}).datos[({ti})]).datos, (({pl}).datos[({ti})]).largo);"
                        ));
                    } else {
                        self.emite(format!("    kami_texto_empuja(&({tr}), (({pl}).datos[({ti})]));"));
                    }
                    self.emite("}".to_string());
                    return Ok(Some(ExVal::movible(tr.clone(), CType::Texto, tr)));
                }
                // Aplana un nivel (`[Vec<T>]` con `T: Copy`).
                let u = u_ap.unwrap();
                let t_r = CType::Vec(Box::new(u.clone()));
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.concat()` da `Vec`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let pl = self.recv_lugar(b, base, d)?;
                let se = u.deletrea().map_err(|e| self.con_archivo(e))?;
                let tr = self.temp(t_r.clone())?;
                self.emite(format!("memset(&({tr}), 0, sizeof({tr}));"));
                let ti = self.temp(CType::Usize)?;
                let tn = self.temp(CType::Usize)?;
                self.emite(format!("for (({ti}) = 0; ({ti}) < (({pl}).largo); ({ti})++) {{"));
                self.emite(format!("    ({tn}) = ((({tr}).largo + (({pl}).datos[({ti})]).largo));"));
                self.emite(format!("    if ((({tn}) > ({tr}).capacidad)) {{"));
                self.emite(format!("        ({tr}).datos = realloc(({tr}).datos, (({tn}) * sizeof({se})));"));
                self.emite(format!("        if ((({tr}).datos == NULL)) {{ KAMI_PANICO(\"sin memoria\"); }}"));
                self.emite(format!("        ({tr}).capacidad = ({tn});"));
                self.emite("    }".to_string());
                self.emite(format!(
                    "    memcpy(&(({tr}).datos[({tr}).largo]), (({pl}).datos[({ti})]).datos, ((({pl}).datos[({ti})]).largo * sizeof({se})));"
                ));
                self.emite(format!("    ({tr}).largo = ({tn});"));
                self.emite("}".to_string());
                Ok(Some(ExVal::movible(tr.clone(), t_r, tr)))
            }
            "is_ascii" => {
                if inner != CType::U8 {
                    return Ok(None);
                }
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.is_ascii()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Bool {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `bool`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let pl = self.recv_lugar(b, base, d)?;
                let tr = self.temp(CType::Bool)?;
                self.emite(format!("{tr} = true;"));
                let ti = self.temp(CType::Usize)?;
                self.emite(format!("for (({ti}) = 0; ({ti}) < (({pl}).largo); ({ti})++) {{"));
                self.emite(format!("    if (((({pl}).datos[({ti})]) >= 0x80)) {{ ({tr}) = false; break; }}"));
                self.emite("}".to_string());
                Ok(Some(ExVal::puro(tr, CType::Bool)))
            }
            "to_ascii_uppercase" | "to_ascii_lowercase" => {
                if inner != CType::U8 {
                    return Ok(None);
                }
                if !args.is_empty() {
                    return Err(self.err_en("C0005", format!("`.{m}()` no lleva argumentos"), x));
                }
                let t_r = CType::Vec(Box::new(CType::U8));
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da `Vec<u8>`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let pl = self.recv_lugar(b, base, d)?;
                let tr = self.temp(t_r.clone())?;
                self.emite(format!("({tr}).largo = (({pl}).largo);"));
                self.emite(format!("({tr}).capacidad = (({pl}).largo);"));
                self.emite(format!(
                    "({tr}).datos = ((({pl}).largo) ? malloc(({pl}).largo) : NULL);"
                ));
                self.emite(format!(
                    "if (((({pl}).largo != 0) && (({tr}).datos == NULL))) {{ KAMI_PANICO(\"sin memoria\"); }}"
                ));
                let ti = self.temp(CType::Usize)?;
                let tb2 = self.temp(CType::U8)?;
                self.emite(format!("for (({ti}) = 0; ({ti}) < (({pl}).largo); ({ti})++) {{"));
                self.emite(format!("    ({tb2}) = (({pl}).datos[({ti})]);"));
                if m == "to_ascii_uppercase" {
                    self.emite(format!("    if (((({tb2}) >= 'a') && (({tb2}) <= 'z'))) {{ ({tb2}) = ((({tb2}) - 32)); }}"));
                } else {
                    self.emite(format!("    if (((({tb2}) >= 'A') && (({tb2}) <= 'Z'))) {{ ({tb2}) = ((({tb2}) + 32)); }}"));
                }
                self.emite(format!("    ({tr}).datos[({ti})] = ({tb2});"));
                self.emite("}".to_string());
                Ok(Some(ExVal::movible(tr.clone(), t_r, tr)))
            }
            "fill_with" | "sort_by" | "sort_by_key" | "sort_by_cached_key" | "binary_search_by"
            | "binary_search_by_key" | "partition_point" | "is_partitioned" => {
                let mut e = self.err_en("C0003", format!("`rebanada.{m}()` pide una clausura"), x);
                e.ayuda = Some("las clausuras no tienen soporte; usa un `for`".into());
                Err(e)
            }
            "iter" | "iter_mut" | "windows" | "chunks" | "chunks_mut" | "chunks_exact"
            | "rchunks" | "rchunks_mut" | "rchunks_exact" | "chunks_exact_mut" | "as_chunks"
            | "as_chunks_mut" | "as_rchunks" | "array_chunks" | "array_windows" | "split"
            | "split_mut" | "rsplit" | "splitn" | "split_inclusive" | "split_off" | "utf8_chunks"
            | "split_off_mut" | "split_off_first" => {
                let mut e = self.err_en("C0003", format!("`rebanada.{m}()` da un iterador"), x);
                e.ayuda = Some("los iteradores no tienen soporte; usa índices o `while`".into());
                Err(e)
            }
            "get_unchecked" | "get_unchecked_mut" | "first_chunk" | "last_chunk"
            | "first_chunk_mut" | "last_chunk_mut" | "as_ptr" | "as_mut_ptr" | "as_flattened"
            | "as_flattened_mut" | "to_vec_in" | "repeat" => {
                let mut e = self.err_en("C0003", format!("`rebanada.{m}()` no tiene soporte"), x);
                e.ayuda = Some("usa `.get()` / `.first()` / `.split_at()` / `.to_vec()`".into());
                Err(e)
            }
            _ => Ok(None),
        }
    }

    /// Métodos de arreglo (`[T; N]`: propios + delegación a rebanada).
    fn ex_arreglo(
        &mut self,
        b: ExVal,
        base: &CType,
        d: usize,
        m: &str,
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        esp: Option<&CType>,
        x: &syn::ExprMethodCall,
    ) -> Result<Option<ExVal>, CError> {
        let (inner, n_c) = match base {
            CType::Arreglo(i, l) => (i.as_ref().clone(), l.deletrea()),
            _ => return Ok(None),
        };
        match m {
            "len" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.len()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Usize {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.len()` da `usize`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let _ = self.recv_lugar(b, base, d)?;
                Ok(Some(ExVal::puro(format!("({n_c})"), CType::Usize)))
            }
            "is_empty" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.is_empty()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Bool {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `bool`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let _ = self.recv_lugar(b, base, d)?;
                Ok(Some(ExVal::puro(format!("((({n_c}) == 0))"), CType::Bool)))
            }
            "as_slice" | "as_mut_slice" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", format!("`.{m}()` no lleva argumentos"), x));
                }
                let es_mut = m == "as_mut_slice";
                if es_mut && !Self::presta_mut_ok(&b.tipo) {
                    return Err(self.err_en(
                        "C0003",
                        "`.as_mut_slice()` pide `&mut` en toda la cadena".into(),
                        &x.receiver,
                    ));
                }
                let t_r = CType::Rebana(Box::new(inner.clone()), es_mut);
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da rebanada", self.muestra(e)),
                            x,
                        ));
                    }
                }
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let pl = self.recv_lugar(b, base, d)?;
                let tv = self.temp(t_r.clone())?;
                self.emite(format!("({tv}).datos = ((({n_c})) ? (&(({pl})[0])) : NULL);"));
                self.emite(format!("({tv}).largo = ({n_c});"));
                Ok(Some(ExVal::movible(tv.clone(), t_r, tv)))
            }
            "each_ref" | "each_mut" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", format!("`.{m}()` no lleva argumentos"), x));
                }
                let es_mut = m == "each_mut";
                if es_mut && !Self::presta_mut_ok(&b.tipo) {
                    return Err(self.err_en(
                        "C0003",
                        "`.each_mut()` pide `&mut` en toda la cadena".into(),
                        &x.receiver,
                    ));
                }
                let l = match base {
                    CType::Arreglo(_, l) => l.clone(),
                    _ => unreachable!(),
                };
                let t_r = CType::Arreglo(
                    Box::new(CType::Ref(es_mut, Box::new(inner.clone()))),
                    l,
                );
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da arreglo", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let pl = self.recv_lugar(b, base, d)?;
                let tr = self.temp(t_r.clone())?;
                let ti = self.temp(CType::Usize)?;
                self.emite(format!("for (({ti}) = 0; ({ti}) < ({n_c}); ({ti})++) {{"));
                self.emite(format!("    ({tr})[({ti})] = (&(({pl})[({ti})]));"));
                self.emite("}".to_string());
                Ok(Some(ExVal::movible(tr.clone(), t_r, tr)))
            }
            "as_flattened" | "as_flattened_mut" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", format!("`.{m}()` no lleva argumentos"), x));
                }
                let es_mut = m == "as_flattened_mut";
                if es_mut && !Self::presta_mut_ok(&b.tipo) {
                    return Err(self.err_en(
                        "C0003",
                        "`.as_flattened_mut()` pide `&mut` en toda la cadena".into(),
                        &x.receiver,
                    ));
                }
                let (u, m_c) = match &inner {
                    CType::Arreglo(u, l) => (u.as_ref().clone(), l.deletrea()),
                    _ => return Ok(None),
                };
                let t_r = CType::Rebana(Box::new(u), es_mut);
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da rebanada", self.muestra(e)),
                            x,
                        ));
                    }
                }
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let pl = self.recv_lugar(b, base, d)?;
                let tv = self.temp(t_r.clone())?;
                self.emite(format!(
                    "({tv}).datos = (((( {n_c}) * ({m_c})) != 0) ? (&(({pl})[0][0])) : NULL);"
                ));
                self.emite(format!("({tv}).largo = ((({n_c}) * ({m_c})));"));
                Ok(Some(ExVal::movible(tv.clone(), t_r, tv)))
            }
            "map" | "try_map" => {
                let mut e = self.err_en("C0003", format!("`arreglo.{m}()` pide una clausura"), x);
                e.ayuda = Some("las clausuras no tienen soporte; usa un `for`".into());
                Err(e)
            }
            "into_iter" => {
                let mut e = self.err_en("C0003", "`arreglo.into_iter()` da un iterador".into(), x);
                e.ayuda = Some("los iteradores no tienen soporte; usa índices o `while`".into());
                Err(e)
            }
            "transpose" => {
                let mut e = self.err_en("C0003", "`arreglo.transpose()` no tiene soporte".into(), x);
                e.ayuda = Some("traspone a mano con un `for` doble".into());
                Err(e)
            }
            _ => {
                // Todo lo demás delega a la rebanada con una vista temporal.
                const MUT: &[&str] = &[
                    "first_mut",
                    "last_mut",
                    "get_mut",
                    "split_at_mut",
                    "sort",
                    "reverse",
                    "swap",
                    "fill",
                    "copy_from_slice",
                    "copy_within",
                    "rotate_left",
                    "rotate_right",
                ];
                if MUT.contains(&m) && !Self::presta_mut_ok(&b.tipo) {
                    return Err(self.err_en(
                        "C0003",
                        format!("`.{m}()` pide `&mut` en toda la cadena"),
                        &x.receiver,
                    ));
                }
                let pl = self.recv_lugar(b, base, d)?;
                let t_v = CType::Rebana(Box::new(inner.clone()), true);
                self.registra_mono(&t_v).map_err(|e| self.con_archivo(e))?;
                let tv = self.temp(t_v.clone())?;
                self.emite(format!("({tv}).datos = ((({n_c})) ? (&(({pl})[0])) : NULL);"));
                self.emite(format!("({tv}).largo = ({n_c});"));
                let vb = ExVal::puro(tv.clone(), t_v.clone());
                self.ex_rebana(vb, &t_v, 0, m, args, esp, x)
            }
        }
    }

    /// Métodos de punteros crudos (`*const T` / `*mut T`: solo finos).
    fn ex_puntero(
        &mut self,
        b: ExVal,
        base: &CType,
        d: usize,
        m: &str,
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        esp: Option<&CType>,
        x: &syn::ExprMethodCall,
    ) -> Result<Option<ExVal>, CError> {
        let (m0, inner) = match base {
            CType::Ptr(mt, i) => (*mt, i.as_ref().clone()),
            _ => return Ok(None),
        };
        if matches!(inner, CType::Rebana(_, _) | CType::Dyn(_)) {
            let mut e = self.err_en("C0003", format!("`{m}` sobre puntero gordo no tiene soporte"), x);
            e.ayuda = Some("usa rebanadas o referencias".into());
            return Err(e);
        }
        match m {
            "is_null" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.is_null()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Bool {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `bool`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let p = self.recv_num(b, base, d)?;
                Ok(Some(ExVal::puro(format!("((({p}) == NULL))"), CType::Bool)))
            }
            "addr" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.addr()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Usize {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.addr()` da `usize`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let p = self.recv_num(b, base, d)?;
                Ok(Some(ExVal::puro(format!("((size_t)({p}))"), CType::Usize)))
            }
            "with_addr" | "mask" | "map_addr" => {
                if m == "map_addr" {
                    let mut e = self.err_en("C0003", "`.map_addr()` pide una clausura".into(), x);
                    e.ayuda = Some("usa `.addr()` y `.with_addr()`".into());
                    return Err(e);
                }
                if args.len() != 1 {
                    return Err(self.err_en("C0005", format!("`.{m}()` lleva una dirección"), x));
                }
                if let Some(e) = esp {
                    if e != base {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da puntero", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let p = self.recv_num(b, base, d)?;
                let v = self.baja_expr(&args[0], Some(&CType::Usize))?;
                let cn = self.consume(v, Some(&CType::Usize))?;
                let spell = base.deletrea().map_err(|e| self.con_archivo(e))?;
                if m == "with_addr" {
                    Ok(Some(ExVal::puro(format!("(({spell})({cn}))"), base.clone())))
                } else {
                    Ok(Some(ExVal::puro(
                        format!("(({spell})(((uintptr_t)({p})) & ({cn})))"),
                        base.clone(),
                    )))
                }
            }
            "cast" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.cast()` no lleva argumentos".into(), x));
                }
                let t_r = match esp {
                    Some(CType::Ptr(mt, u)) => CType::Ptr(*mt, u.clone()),
                    Some(e) => {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.cast()` da puntero", self.muestra(e)),
                            x,
                        ))
                    }
                    None => {
                        return Err(self.err_en(
                            "C0005",
                            "`.cast()` necesita tipo esperado (anota el `let`)".into(),
                            x,
                        ))
                    }
                };
                let p = self.recv_num(b, base, d)?;
                let spell = t_r.deletrea().map_err(|e| self.con_archivo(e))?;
                Ok(Some(ExVal::puro(format!("(({spell})({p}))"), t_r)))
            }
            "cast_mut" | "cast_const" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", format!("`.{m}()` no lleva argumentos"), x));
                }
                let t_r = CType::Ptr(m == "cast_mut", Box::new(inner.clone()));
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da puntero", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let p = self.recv_num(b, base, d)?;
                let spell = t_r.deletrea().map_err(|e| self.con_archivo(e))?;
                Ok(Some(ExVal::puro(format!("(({spell})({p}))"), t_r)))
            }
            "wrapping_add" | "wrapping_sub" | "add" | "sub" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", format!("`.{m}()` lleva una cuenta"), x));
                }
                if let Some(e) = esp {
                    if e != base {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da puntero", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let p = self.recv_num(b, base, d)?;
                let v = self.baja_expr(&args[0], Some(&CType::Usize))?;
                let cn = self.consume(v, Some(&CType::Usize))?;
                let op = if m == "wrapping_add" || m == "add" { "+" } else { "-" };
                Ok(Some(ExVal::puro(format!("((({p}) {op} ({cn})))"), base.clone())))
            }
            "wrapping_offset" | "offset" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", format!("`.{m}()` lleva una cuenta"), x));
                }
                if let Some(e) = esp {
                    if e != base {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da puntero", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let p = self.recv_num(b, base, d)?;
                let v = self.baja_expr(&args[0], Some(&CType::Isize))?;
                let cn = self.consume(v, Some(&CType::Isize))?;
                Ok(Some(ExVal::puro(format!("((({p}) + ({cn})))"), base.clone())))
            }
            "wrapping_byte_add" | "wrapping_byte_sub" | "byte_add" | "byte_sub" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", format!("`.{m}()` lleva una cuenta"), x));
                }
                if let Some(e) = esp {
                    if e != base {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da puntero", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let p = self.recv_num(b, base, d)?;
                let v = self.baja_expr(&args[0], Some(&CType::Usize))?;
                let cn = self.consume(v, Some(&CType::Usize))?;
                let op = if m.ends_with("add") { "+" } else { "-" };
                let spell = base.deletrea().map_err(|e| self.con_archivo(e))?;
                Ok(Some(ExVal::puro(
                    format!("(({spell})(((char *)({p})) {op} ({cn})))"),
                    base.clone(),
                )))
            }
            "read" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.read()` no lleva argumentos".into(), x));
                }
                if inner == CType::Vacio {
                    return Err(self.err_en("C0003", "`.read()` sobre `void` no tiene soporte".into(), x));
                }
                if let Some(e) = esp {
                    if e != &inner {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `{}`", self.muestra(e), self.muestra(&inner)),
                            x,
                        ));
                    }
                }
                let p = self.recv_num(b, base, d)?;
                Ok(Some(ExVal::puro(format!("((*({p})))"), inner)))
            }
            "read_unaligned" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.read_unaligned()` no lleva argumentos".into(), x));
                }
                if inner == CType::Vacio {
                    return Err(self.err_en("C0003", "`.read_unaligned()` sobre `void` no tiene soporte".into(), x));
                }
                if let Some(e) = esp {
                    if e != &inner {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `{}`", self.muestra(e), self.muestra(&inner)),
                            x,
                        ));
                    }
                }
                let p = self.recv_num(b, base, d)?;
                let se = inner.deletrea().map_err(|e| self.con_archivo(e))?;
                let tr = self.temp(inner.clone())?;
                self.emite(format!("memcpy(&({tr}), ({p}), sizeof({se}));"));
                Ok(Some(ExVal::puro(tr, inner)))
            }
            "read_volatile" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.read_volatile()` no lleva argumentos".into(), x));
                }
                if inner == CType::Vacio {
                    return Err(self.err_en("C0003", "`.read_volatile()` sobre `void` no tiene soporte".into(), x));
                }
                if let Some(e) = esp {
                    if e != &inner {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `{}`", self.muestra(e), self.muestra(&inner)),
                            x,
                        ));
                    }
                }
                let p = self.recv_num(b, base, d)?;
                let se = inner.deletrea().map_err(|e| self.con_archivo(e))?;
                Ok(Some(ExVal::puro(format!("((*((volatile {se} *)({p}))))"), inner)))
            }
            "write" | "write_unaligned" | "write_volatile" | "copy_from"
            | "copy_from_nonoverlapping" => {
                if !m0 {
                    return Ok(None);
                }
                if m == "write" || m == "write_unaligned" || m == "write_volatile" {
                    if args.len() != 1 {
                        return Err(self.err_en("C0005", format!("`.{m}()` lleva un valor"), x));
                    }
                    if let Some(e) = esp {
                        if *e != CType::Vacio {
                            return Err(self.err_en(
                                "C0005",
                                format!("se esperaba `{}`, `.{m}()` da `()`", self.muestra(e)),
                                x,
                            ));
                        }
                    }
                    let p = self.recv_num(b, base, d)?;
                    let v = self.baja_expr(&args[0], Some(&inner))?;
                    let cv = self.consume(v, Some(&inner))?;
                    if m == "write_unaligned" {
                        let se = inner.deletrea().map_err(|e| self.con_archivo(e))?;
                        let tv = self.temp(inner.clone())?;
                        self.emite(format!("{tv} = ({cv});"));
                        self.emite(format!("memcpy(({p}), &({tv}), sizeof({se}));"));
                    } else if m == "write_volatile" {
                        let se = inner.deletrea().map_err(|e| self.con_archivo(e))?;
                        self.emite(format!("(*((volatile {se} *)({p}))) = ({cv});"));
                    } else {
                        self.emite(format!("(*({p})) = ({cv});"));
                    }
                    return Ok(Some(ExVal::puro("0".into(), CType::Vacio)));
                }
                if args.len() != 2 {
                    return Err(self.err_en("C0005", format!("`.{m}()` lleva origen y cuenta"), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Vacio {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da `()`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let p = self.recv_num(b, base, d)?;
                let t_src = CType::Ptr(false, Box::new(inner.clone()));
                let v0 = self.baja_expr(&args[0], Some(&t_src))?;
                let cs = self.consume(v0, Some(&t_src))?;
                let v1 = self.baja_expr(&args[1], Some(&CType::Usize))?;
                let cn = self.consume(v1, Some(&CType::Usize))?;
                let se = inner.deletrea().map_err(|e| self.con_archivo(e))?;
                if m == "copy_from" {
                    self.emite(format!("memmove(({p}), ({cs}), (({cn}) * sizeof({se})));"));
                } else {
                    self.emite(format!("memcpy(({p}), ({cs}), (({cn}) * sizeof({se})));"));
                }
                Ok(Some(ExVal::puro("0".into(), CType::Vacio)))
            }
            "copy_to" | "copy_to_nonoverlapping" => {
                if args.len() != 2 {
                    return Err(self.err_en("C0005", format!("`.{m}()` lleva destino y cuenta"), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Vacio {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.{m}()` da `()`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let p = self.recv_num(b, base, d)?;
                let t_dst = CType::Ptr(true, Box::new(inner.clone()));
                let v0 = self.baja_expr(&args[0], Some(&t_dst))?;
                let cd = self.consume(v0, Some(&t_dst))?;
                let v1 = self.baja_expr(&args[1], Some(&CType::Usize))?;
                let cn = self.consume(v1, Some(&CType::Usize))?;
                let se = inner.deletrea().map_err(|e| self.con_archivo(e))?;
                if m == "copy_to" {
                    self.emite(format!("memmove(({cd}), ({p}), (({cn}) * sizeof({se})));"));
                } else {
                    self.emite(format!("memcpy(({cd}), ({p}), (({cn}) * sizeof({se})));"));
                }
                Ok(Some(ExVal::puro("0".into(), CType::Vacio)))
            }
            "is_aligned" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.is_aligned()` no lleva argumentos".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Bool {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `bool`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let p = self.recv_num(b, base, d)?;
                let se = inner.deletrea().map_err(|e| self.con_archivo(e))?;
                Ok(Some(ExVal::puro(
                    format!("(((((uintptr_t)({p})) % _Alignof({se})) == 0))"),
                    CType::Bool,
                )))
            }
            "is_aligned_to" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`.is_aligned_to()` lleva un alineado".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Bool {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `bool`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let p = self.recv_num(b, base, d)?;
                let v = self.baja_expr(&args[0], Some(&CType::Usize))?;
                let cn = self.consume(v, Some(&CType::Usize))?;
                Ok(Some(ExVal::puro(
                    format!("(((((uintptr_t)({p})) % ({cn})) == 0))"),
                    CType::Bool,
                )))
            }
            "as_ref" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.as_ref()` no lleva argumentos".into(), x));
                }
                let t_r = CType::Opcion(Box::new(CType::Ref(false, Box::new(inner.clone()))));
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.as_ref()` da `Option`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let p = self.recv_num(b, base, d)?;
                let tp = self.temp(base.clone())?;
                self.emite(format!("{tp} = ({p});"));
                self.marca_movida_si_temp_pub(&p);
                let to = self.temp(t_r.clone())?;
                self.emite(format!("memset(&({to}), 0, sizeof({to}));"));
                self.emite(format!("if ((({tp}) != NULL)) {{"));
                self.emite(format!("    ({to}).tiene = true;"));
                self.emite(format!("    ({to}).valor = ({tp});"));
                self.emite("}".to_string());
                Ok(Some(ExVal::movible(to.clone(), t_r, to)))
            }
            "as_mut" => {
                if !m0 {
                    return Ok(None);
                }
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`.as_mut()` no lleva argumentos".into(), x));
                }
                let t_r = CType::Opcion(Box::new(CType::Ref(true, Box::new(inner.clone()))));
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `.as_mut()` da `Option`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                let p = self.recv_num(b, base, d)?;
                let tp = self.temp(base.clone())?;
                self.emite(format!("{tp} = ({p});"));
                self.marca_movida_si_temp_pub(&p);
                let to = self.temp(t_r.clone())?;
                self.emite(format!("memset(&({to}), 0, sizeof({to}));"));
                self.emite(format!("if ((({tp}) != NULL)) {{"));
                self.emite(format!("    ({to}).tiene = true;"));
                self.emite(format!("    ({to}).valor = ({tp});"));
                self.emite("}".to_string());
                Ok(Some(ExVal::movible(to.clone(), t_r, to)))
            }
            "as_uninit_ref" | "as_uninit_mut" | "guarantee_init" | "guarantee_init_mut"
            | "without_provenance" | "with_metadata_of" | "to_bits" => {
                let mut e = self.err_en("C0003", format!("`puntero.{m}()` no tiene soporte"), x);
                e.ayuda = Some("usa `.addr()` / `.cast()` / aritmética".into());
                Err(e)
            }
            _ => Ok(None),
        }
    }

    /// Macros en posición de expresión (`vec!`, `format!`, `panic!`...).
    fn ex_macro(&mut self, mac: &syn::Macro, esp: Option<&CType>) -> Result<ExVal, CError> {
        let nombre = mac
            .path
            .segments
            .last()
            .map(|s| s.ident.to_string())
            .unwrap_or_default();
        match nombre.as_str() {
            "vec" => self.ex_macro_vec(mac, esp),
            "format" => self.ex_macro_format(mac, esp),
            "panic" | "todo" | "unimplemented" | "unreachable" => {
                self.ex_macro_panico(&nombre, mac, esp)
            }
            "concat" | "stringify" | "include_str" | "line" | "file" | "column" => {
                self.ex_macro_const(&nombre, mac, esp)
            }
            "env" | "option_env" => self.ex_macro_env(&nombre, mac, esp),
            "cfg" => self.ex_macro_cfg(mac, esp),
            "matches" => Err(self.err_en(
                "C0003",
                "`matches!` necesita patrones (tramo pendiente)".into(),
                mac,
            )),
            "println" | "print" | "eprintln" | "eprint" => {
                self.ex_macro_print(&nombre, mac)?;
                if let Some(e) = esp {
                    if *e != CType::Vacio {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `{nombre}!` vale `()`", self.muestra(e)),
                            mac,
                        ));
                    }
                }
                Ok(ExVal::puro("0".into(), CType::Vacio))
            }
            "assert" | "assert_eq" | "assert_ne" | "debug_assert" | "debug_assert_eq"
            | "debug_assert_ne" => {
                self.ex_macro_assert(&nombre, mac)?;
                if let Some(e) = esp {
                    if *e != CType::Vacio {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `{nombre}!` vale `()`", self.muestra(e)),
                            mac,
                        ));
                    }
                }
                Ok(ExVal::puro("0".into(), CType::Vacio))
            }
            _ => Err(self.err_en("C0004", format!("macro `{nombre}!` desconocida"), mac)),
        }
    }

    /// `vec![a, b]` / `vec![x; n]`.
    fn ex_macro_vec(&mut self, mac: &syn::Macro, esp: Option<&CType>) -> Result<ExVal, CError> {
        struct Repite {
            elem: syn::Expr,
            _p: syn::Token![;],
            n: syn::Expr,
        }
        impl syn::parse::Parse for Repite {
            fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
                Ok(Repite {
                    elem: input.parse()?,
                    _p: input.parse()?,
                    n: input.parse()?,
                })
            }
        }
        let guia_in: Option<CType> = match esp {
            Some(CType::Vec(i)) => Some(i.as_ref().clone()),
            Some(e) => {
                return Err(self.err_en(
                    "C0005",
                    format!("se esperaba `{}`, `vec!` da `Vec`", self.muestra(e)),
                    mac,
                ))
            }
            None => None,
        };
        if let Ok(r) = syn::parse2::<Repite>(mac.tokens.clone()) {
            // `vec![x; n]`.
            let v_elem = self.baja_expr(&r.elem, guia_in.as_ref())?;
            let inner = guia_in.clone().unwrap_or_else(|| v_elem.tipo.clone());
            if self.necesita_drop(&inner) {
                let mut e = self.err_en(
                    "C0003",
                    format!("`vec![x; n]` sobre `{}` necesita `Clone`", self.muestra(&inner)),
                    &r.elem,
                );
                e.ayuda = Some("llena con `push` en un `for`".into());
                return Err(e);
            }
            let ce = self.consume(v_elem, Some(&inner))?;
            let te = self.temp(inner.clone())?;
            self.emite(format!("{te} = ({ce});"));
            let v_n = self.baja_expr(&r.n, Some(&CType::Usize))?;
            let cn = self.consume(v_n, Some(&CType::Usize))?;
            let tn = self.temp(CType::Usize)?;
            self.emite(format!("{tn} = ({cn});"));
            self.marca_movida_si_temp_pub(&cn);
            let t = CType::Vec(Box::new(inner.clone()));
            self.registra_mono(&t).map_err(|e| self.con_archivo(e))?;
            let se = inner.deletrea().map_err(|e| self.con_archivo(e))?;
            let tr = self.temp(t.clone())?;
            self.emite(format!("({tr}).largo = ({tn});"));
            self.emite(format!("({tr}).capacidad = ({tn});"));
            self.emite(format!("({tr}).datos = ((({tn})) ? malloc((({tn}) * sizeof({se}))) : NULL);"));
            self.emite(format!(
                "if (((({tn}) != 0) && (({tr}).datos == NULL))) {{ KAMI_PANICO(\"sin memoria\"); }}"
            ));
            let ti = self.temp(CType::Usize)?;
            self.emite(format!("for (({ti}) = 0; ({ti}) < ({tn}); ({ti})++) {{"));
            self.emite(format!("    ({tr}).datos[({ti})] = ({te});"));
            self.emite("}".to_string());
            return Ok(ExVal::movible(tr.clone(), t, tr));
        }
        // `vec![a, b, c]`.
        let elems: syn::punctuated::Punctuated<syn::Expr, syn::token::Comma> = mac
            .parse_body_with(syn::punctuated::Punctuated::<syn::Expr, syn::token::Comma>::parse_terminated)
            .map_err(|e| self.err_en("C0005", format!("`vec!` mal formado: {e}"), mac))?;
        if elems.is_empty() && guia_in.is_none() {
            return Err(self.err_en("C0005", "`vec![]` necesita tipo esperado (anota el `let`)".into(), mac));
        }
        let mut inner = guia_in.clone();
        let mut cs: Vec<(String, CType)> = Vec::new();
        for (i, e) in elems.iter().enumerate() {
            let g = if i == 0 { guia_in.as_ref() } else { inner.as_ref() };
            let v = self.baja_expr(e, g)?;
            if i == 0 && inner.is_none() {
                inner = Some(v.tipo.clone());
            }
            let inn = inner.clone().unwrap();
            let c = self.consume(v, Some(&inn))?;
            cs.push((c, inn));
        }
        let inner = inner.unwrap();
        let t = CType::Vec(Box::new(inner.clone()));
        self.registra_mono(&t).map_err(|e| self.con_archivo(e))?;
        let tr = self.temp(t.clone())?;
        if cs.is_empty() {
            self.emite(format!("memset(&({tr}), 0, sizeof({tr}));"));
            return Ok(ExVal::movible(tr.clone(), t, tr));
        }
        let se = inner.deletrea().map_err(|e| self.con_archivo(e))?;
        let n = cs.len();
        self.emite(format!("({tr}).largo = {n};"));
        self.emite(format!("({tr}).capacidad = {n};"));
        self.emite(format!("({tr}).datos = malloc(({n} * sizeof({se})));"));
        self.emite(format!("if ((({tr}).datos == NULL)) {{ KAMI_PANICO(\"sin memoria\"); }}"));
        for (i, (c, _)) in cs.iter().enumerate() {
            self.emite(format!("({tr}).datos[{i}] = ({c});"));
            self.marca_movida_si_temp_pub(c);
        }
        Ok(ExVal::movible(tr.clone(), t, tr))
    }

    /// `panic!` / `todo!` / `unimplemented!` / `unreachable!` (divergen).
    fn ex_macro_panico(
        &mut self,
        nombre: &str,
        mac: &syn::Macro,
        esp: Option<&CType>,
    ) -> Result<ExVal, CError> {
        let args: syn::punctuated::Punctuated<syn::Expr, syn::token::Comma> = mac
            .parse_body_with(syn::punctuated::Punctuated::<syn::Expr, syn::token::Comma>::parse_terminated)
            .map_err(|e| self.err_en("C0005", format!("`{nombre}!` mal formado: {e}"), mac))?;
        if args.is_empty() {
            let msg = match nombre {
                "todo" => "todavía no implementado",
                "unimplemented" => "no implementado",
                "unreachable" => "rama inalcanzable",
                _ => "panico explícito",
            };
            self.emite(format!("KAMI_PANICO(\"{msg}\");"));
        } else if let syn::Expr::Lit(lit) = &args[0] {
            if let syn::Lit::Str(s) = &lit.lit {
                let resto: Vec<syn::Expr> = args.iter().skip(1).cloned().collect();
                let (f, cs) = self.fmt_traduce(&s.value(), &resto, mac)?;
                let extra = if cs.is_empty() { String::new() } else { format!(", {}", cs.join(", ")) };
                self.emite(format!("KAMI_PANICO({f}{extra});"));
            } else {
                return Err(self.err_en("C0005", format!("`{nombre}!` pide literal de cadena"), &args[0]));
            }
        } else if args.len() == 1 {
            let resto = vec![args[0].clone()];
            let (f, cs) = self.fmt_traduce("{}", &resto, mac)?;
            self.emite(format!("KAMI_PANICO({f}, {});", cs.join(", ")));
        } else {
            return Err(self.err_en("C0005", format!("`{nombre}!` pide literal de formato"), &args[0]));
        }
        match esp {
            None | Some(CType::Vacio) => Ok(ExVal::puro("0".into(), CType::Vacio)),
            Some(t) => {
                let tr = self.temp(t.clone())?;
                Ok(ExVal::movible(tr.clone(), t.clone(), tr))
            }
        }
    }

    /// Escapa un tramo literal para cadena C (`%` se duplica para `printf`).
    fn esc_fmt_c(s: &str) -> String {
        let mut o = String::new();
        for ch in s.chars() {
            match ch {
                '\\' => o.push_str("\\\\"),
                '"' => o.push_str("\\\""),
                '\n' => o.push_str("\\n"),
                '\r' => o.push_str("\\r"),
                '\t' => o.push_str("\\t"),
                '%' => o.push_str("%%"),
                c => o.push(c),
            }
        }
        o
    }

    /// Traduce formato Rust a `printf`: (expr-formato-C, exprs-args). Presta, no mueve.
    pub(crate) fn fmt_traduce(
        &mut self,
        fmt: &str,
        args: &[syn::Expr],
        mac: &syn::Macro,
    ) -> Result<(String, Vec<String>), CError> {
        let mut pos: Vec<&syn::Expr> = Vec::new();
        let mut nom: std::collections::HashMap<String, &syn::Expr> = std::collections::HashMap::new();
        for a in args {
            if let syn::Expr::Assign(x) = a {
                if let syn::Expr::Path(p) = x.left.as_ref() {
                    if p.qself.is_none() && p.path.segments.len() == 1 {
                        nom.insert(p.path.segments[0].ident.to_string(), x.right.as_ref());
                        continue;
                    }
                }
                return Err(self.err_en("C0005", "`nombre = expr` mal formado".into(), a));
            }
            if !nom.is_empty() {
                return Err(self.err_en("C0005", "posicional después de nombrado no vale".into(), a));
            }
            pos.push(a);
        }
        let mut usados_pos = vec![false; pos.len()];
        let mut usados_nom: std::collections::HashSet<String> = std::collections::HashSet::new();
        let mut auto_i = 0usize;
        // Piezas del formato: literales (ya escapados) y especs sueltos.
        enum Pieza {
            Lit(String),
            Esp(String),
        }
        let mut piezas: Vec<Pieza> = Vec::new();
        let mut lit = String::new();
        let mut argv: Vec<String> = Vec::new();
        let chs: Vec<char> = fmt.chars().collect();
        let mut i = 0;
        while i < chs.len() {
            let c = chs[i];
            if c == '{' {
                if i + 1 < chs.len() && chs[i + 1] == '{' {
                    lit.push('{');
                    i += 2;
                    continue;
                }
                let mut j = i + 1;
                while j < chs.len() && chs[j] != '}' {
                    j += 1;
                }
                if j >= chs.len() {
                    return Err(self.err_en("C0005", "`{` sin cerrar".into(), mac));
                }
                let dentro: String = chs[i + 1..j].iter().collect();
                i = j + 1;
                let (cab, cola) = match dentro.find(':') {
                    Some(k) => (&dentro[..k], &dentro[k + 1..]),
                    None => (dentro.as_str(), ""),
                };
                let es_debug = match cola {
                    "" => false,
                    "?" | "#?" => true,
                    _ => {
                        let mut e = self.err_en(
                            "C0003",
                            format!("formato `{{:{cola}}}` sin soporte"),
                            mac,
                        );
                        e.ayuda = Some("solo `{}` y `{:?}`".into());
                        return Err(e);
                    }
                };
                // ¿Qué argumento?
                let e: &syn::Expr = if cab.is_empty() {
                    if auto_i >= pos.len() {
                        return Err(self.err_en("C0005", "faltan argumentos para el formato".into(), mac));
                    }
                    usados_pos[auto_i] = true;
                    auto_i += 1;
                    pos[auto_i - 1]
                } else if let Ok(n) = cab.parse::<usize>() {
                    if n >= pos.len() {
                        return Err(self.err_en("C0005", format!("no hay argumento `{n}`"), mac));
                    }
                    usados_pos[n] = true;
                    pos[n]
                } else if nom.contains_key(cab) {
                    usados_nom.insert(cab.to_string());
                    nom[cab]
                } else {
                    // Captura (`"{x}"`): debe ser identificador; se baja como variable.
                    if !cab.chars().next().is_some_and(|c0| c0.is_alphabetic() || c0 == '_')
                        || !cab.chars().all(|c0| c0.is_alphanumeric() || c0 == '_')
                    {
                        return Err(self.err_en("C0005", format!("hueco `{{{cab}}}` mal formado"), mac));
                    }
                    let ee: syn::Expr = syn::parse_str(cab)
                        .map_err(|e| self.err_en("C0005", format!("identificador malo: {e}"), mac))?;
                    let v = self.baja_expr(&ee, None)?;
                    if v.arreglo.is_some() {
                        return Err(self.err_en(
                            "C0003",
                            "arreglo como argumento de formato sin soporte".into(),
                            mac,
                        ));
                    }
                    let (spec, a) = self.fmt_uno(v, es_debug, mac)?;
                    piezas.push(Pieza::Lit(std::mem::take(&mut lit)));
                    piezas.push(Pieza::Esp(spec));
                    argv.push(a);
                    continue;
                };
                let v = self.baja_expr(e, None)?;
                if v.arreglo.is_some() {
                    return Err(self.err_en(
                        "C0003",
                        "arreglo como argumento de formato sin soporte".into(),
                        mac,
                    ));
                }
                let (spec, a) = self.fmt_uno(v, es_debug, mac)?;
                piezas.push(Pieza::Lit(std::mem::take(&mut lit)));
                piezas.push(Pieza::Esp(spec));
                argv.push(a);
                continue;
            }
            if c == '}' {
                if i + 1 < chs.len() && chs[i + 1] == '}' {
                    lit.push('}');
                    i += 2;
                    continue;
                }
                return Err(self.err_en("C0005", "`}` suelta".into(), mac));
            }
            lit.push(c);
            i += 1;
        }
        for (k, u) in usados_pos.iter().enumerate() {
            if !u {
                let _ = k;
                return Err(self.err_en("C0005", "sobran argumentos para el formato".into(), mac));
            }
        }
        for k in nom.keys() {
            if !usados_nom.contains(k) {
                return Err(self.err_en("C0005", format!("argumento `{k}` sin hueco"), mac));
            }
        }
        piezas.push(Pieza::Lit(lit));
        let mut f = String::new();
        for p in piezas {
            match p {
                Pieza::Lit(s) => {
                    f.push('"');
                    f.push_str(&Self::esc_fmt_c(&s));
                    f.push('"');
                }
                Pieza::Esp(s) => f.push_str(&s),
            }
        }
        Ok((f, argv))
    }

    /// Un argumento de formato: (pieza-espec, expr-C). `v` se presta.
    fn fmt_uno(
        &mut self,
        v: ExVal,
        es_debug: bool,
        mac: &syn::Macro,
    ) -> Result<(String, String), CError> {
        if es_debug {
            let td = self.depurar_a_temp(v.c.clone(), v.lugar.clone(), &v.tipo)?;
            return Ok(("\"%s\"".into(), format!("kami_texto_cstr(&({td}))")));
        }
        // Display: pela `&`/`Box` (solo lectura).
        let mut t = v.tipo.clone();
        let mut c = v.c.clone();
        loop {
            match &t {
                CType::Ref(_, i) | CType::Caja(i) => {
                    t = i.as_ref().clone();
                    c = format!("(*({c}))");
                }
                _ => break,
            }
        }
        match &t {
            CType::I8 => Ok(("\"%\" PRId8".into(), c)),
            CType::I16 => Ok(("\"%\" PRId16".into(), c)),
            CType::I32 => Ok(("\"%\" PRId32".into(), c)),
            CType::I64 => Ok(("\"%\" PRId64".into(), c)),
            CType::Isize => Ok(("\"%zd\"".into(), c)),
            CType::U8 => Ok(("\"%\" PRIu8".into(), c)),
            CType::U16 => Ok(("\"%\" PRIu16".into(), c)),
            CType::U32 => Ok(("\"%\" PRIu32".into(), c)),
            CType::U64 => Ok(("\"%\" PRIu64".into(), c)),
            CType::Usize => Ok(("\"%zu\"".into(), c)),
            CType::F32 | CType::F64 => Ok(("\"%g\"".into(), c)),
            CType::Bool => Ok(("\"%s\"".into(), format!("((({c}) ? \"true\" : \"false\"))"))),
            CType::Char => {
                let te = self.temp(CType::U32)?;
                self.emite(format!("{te} = ({c});"));
                self.emite(format!("char {te}_buf[5];"));
                self.emite(format!("if ((({te}) < 0x80)) {{ ({te}_buf[0]) = ({te}); ({te}_buf[1]) = 0; }}"));
                self.emite(format!("else if ((({te}) < 0x800)) {{ ({te}_buf[0]) = (0xC0 | (({te}) >> 6)); ({te}_buf[1]) = (0x80 | (({te}) & 0x3F)); ({te}_buf[2]) = 0; }}"));
                self.emite(format!("else if ((({te}) < 0x10000)) {{ ({te}_buf[0]) = (0xE0 | (({te}) >> 12)); ({te}_buf[1]) = (0x80 | ((({te}) >> 6) & 0x3F)); ({te}_buf[2]) = (0x80 | (({te}) & 0x3F)); ({te}_buf[3]) = 0; }}"));
                self.emite(format!("else {{ ({te}_buf[0]) = (0xF0 | (({te}) >> 18)); ({te}_buf[1]) = (0x80 | ((({te}) >> 12) & 0x3F)); ({te}_buf[2]) = (0x80 | ((({te}) >> 6) & 0x3F)); ({te}_buf[3]) = (0x80 | (({te}) & 0x3F)); ({te}_buf[4]) = 0; }}"));
                Ok(("\"%s\"".into(), format!("{te}_buf")))
            }
            CType::VistaTexto => Ok(("\"%s\"".into(), c)),
            CType::Texto => {
                if v.movible.is_some() || v.lugar.is_some() {
                    Ok(("\"%s\"".into(), format!("kami_texto_cstr(&({c}))")))
                } else {
                    let tt = self.temp(CType::Texto)?;
                    self.emite(format!("{tt} = ({c});"));
                    Ok(("\"%s\"".into(), format!("kami_texto_cstr(&({tt}))")))
                }
            }
            _ => {
                let mut e = self.err_en(
                    "C0003",
                    format!("`{{}}` sobre `{}` sin soporte", self.muestra(&t)),
                    mac,
                );
                e.ayuda = Some("usa `{:?}`".into());
                Err(e)
            }
        }
    }

    /// `format!("...", args...)` → `String`.
    fn ex_macro_format(&mut self, mac: &syn::Macro, esp: Option<&CType>) -> Result<ExVal, CError> {
        let args: syn::punctuated::Punctuated<syn::Expr, syn::token::Comma> = mac
            .parse_body_with(syn::punctuated::Punctuated::<syn::Expr, syn::token::Comma>::parse_terminated)
            .map_err(|e| self.err_en("C0005", format!("`format!` mal formado: {e}"), mac))?;
        if args.is_empty() {
            return Err(self.err_en("C0005", "`format!` pide un formato".into(), mac));
        }
        let fmt = match &args[0] {
            syn::Expr::Lit(l) => match &l.lit {
                syn::Lit::Str(s) => s.value(),
                _ => return Err(self.err_en("C0005", "`format!` pide literal de cadena".into(), &args[0])),
            },
            _ => return Err(self.err_en("C0005", "`format!` pide literal de cadena".into(), &args[0])),
        };
        if let Some(e) = esp {
            if *e != CType::Texto {
                return Err(self.err_en(
                    "C0005",
                    format!("se esperaba `{}`, `format!` da `String`", self.muestra(e)),
                    mac,
                ));
            }
        }
        let resto: Vec<syn::Expr> = args.iter().skip(1).cloned().collect();
        let (f, cs) = self.fmt_traduce(&fmt, &resto, mac)?;
        let tr = self.temp(CType::Texto)?;
        self.emite(format!("{tr} = kami_texto_nuevo();"));
        let extra = if cs.is_empty() { String::new() } else { format!(", {}", cs.join(", ")) };
        self.emite(format!("kami_texto_empuja_fmt(&({tr}), {f}{extra});"));
        Ok(ExVal::movible(tr.clone(), CType::Texto, tr))
    }

    /// Literal de cadena C desde `&str` (rechaza NUL: truncaría el `char*`).
    fn lit_cstr(&self, s: &str, mac: &syn::Macro) -> Result<String, CError> {
        if s.contains('\0') {
            return Err(self.err_en("C0003", "NUL dentro de cadena no cabe en `char*`".into(), mac));
        }
        let mut o = String::from("\"");
        for ch in s.chars() {
            match ch {
                '\\' => o.push_str("\\\\"),
                '"' => o.push_str("\\\""),
                '\n' => o.push_str("\\n"),
                '\r' => o.push_str("\\r"),
                '\t' => o.push_str("\\t"),
                c => o.push(c),
            }
        }
        o.push('"');
        Ok(o)
    }

    /// `concat!` / `stringify!` / `include_str!` / `file!` (`line!`/`column!` no).
    fn ex_macro_const(
        &mut self,
        nombre: &str,
        mac: &syn::Macro,
        esp: Option<&CType>,
    ) -> Result<ExVal, CError> {
        if let Some(e) = esp {
            if *e != CType::VistaTexto {
                return Err(self.err_en(
                    "C0005",
                    format!("se esperaba `{}`, `{nombre}!` da `&str`", self.muestra(e)),
                    mac,
                ));
            }
        }
        match nombre {
            "concat" => {
                let args: syn::punctuated::Punctuated<syn::Expr, syn::token::Comma> = mac
                    .parse_body_with(syn::punctuated::Punctuated::<syn::Expr, syn::token::Comma>::parse_terminated)
                    .map_err(|e| self.err_en("C0005", format!("`concat!` mal formado: {e}"), mac))?;
                let mut o = String::new();
                for a in &args {
                    let lit = match a {
                        syn::Expr::Lit(l) => &l.lit,
                        _ => return Err(self.err_en("C0005", "`concat!` pide literales".into(), a)),
                    };
                    match lit {
                        syn::Lit::Str(s) => o.push_str(&s.value()),
                        syn::Lit::Char(s) => o.push(s.value()),
                        syn::Lit::Bool(s) => o.push_str(if s.value { "true" } else { "false" }),
                        syn::Lit::Int(s) => o.push_str(s.base10_digits()),
                        syn::Lit::Float(s) => o.push_str(s.base10_digits()),
                        _ => return Err(self.err_en("C0005", "`concat!` pide literales básicos".into(), a)),
                    }
                }
                let c = self.lit_cstr(&o, mac)?;
                Ok(ExVal::puro(c, CType::VistaTexto))
            }
            "stringify" => {
                let c = self.lit_cstr(&mac.tokens.to_string(), mac)?;
                Ok(ExVal::puro(c, CType::VistaTexto))
            }
            "include_str" => {
                let args: syn::punctuated::Punctuated<syn::Expr, syn::token::Comma> = mac
                    .parse_body_with(syn::punctuated::Punctuated::<syn::Expr, syn::token::Comma>::parse_terminated)
                    .map_err(|e| self.err_en("C0005", format!("`include_str!` mal formado: {e}"), mac))?;
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`include_str!` lleva una ruta".into(), mac));
                }
                let ruta = match &args[0] {
                    syn::Expr::Lit(l) => match &l.lit {
                        syn::Lit::Str(s) => s.value(),
                        _ => return Err(self.err_en("C0005", "`include_str!` pide literal de cadena".into(), &args[0])),
                    },
                    _ => return Err(self.err_en("C0005", "`include_str!` pide literal de cadena".into(), &args[0])),
                };
                let base = std::path::Path::new(&self.archivo)
                    .parent()
                    .map(|p| p.to_path_buf())
                    .unwrap_or_else(|| std::path::PathBuf::from("."));
                let contenido = std::fs::read_to_string(base.join(&ruta)).map_err(|_| {
                    self.err_en("C0004", format!("`include_str!`: `{ruta}` no se pudo leer"), &args[0])
                })?;
                let c = self.lit_cstr(&contenido, mac)?;
                Ok(ExVal::puro(c, CType::VistaTexto))
            }
            "file" => {
                let c = self.lit_cstr(&self.archivo.clone(), mac)?;
                Ok(ExVal::puro(c, CType::VistaTexto))
            }
            "line" | "column" => {
                let mut e = self.err_en("C0003", format!("`{nombre}!` necesita ubicaciones de span"), mac);
                e.ayuda = Some("pasa el número a mano".into());
                Err(e)
            }
            other => Err(self.err_en("C0002", format!("macro const `{other}` sin cablear (bug interno)"), mac)),
        }
    }

    /// `env!` / `option_env!` (leen el entorno al transpirar).
    fn ex_macro_env(
        &mut self,
        nombre: &str,
        mac: &syn::Macro,
        esp: Option<&CType>,
    ) -> Result<ExVal, CError> {
        let args: syn::punctuated::Punctuated<syn::Expr, syn::token::Comma> = mac
            .parse_body_with(syn::punctuated::Punctuated::<syn::Expr, syn::token::Comma>::parse_terminated)
            .map_err(|e| self.err_en("C0005", format!("`{nombre}!` mal formado: {e}"), mac))?;
        let n_args = if nombre == "env" { (1, 2) } else { (1, 1) };
        if args.len() < n_args.0 || args.len() > n_args.1 {
            return Err(self.err_en("C0005", format!("`{nombre}!` lleva otro número de argumentos"), mac));
        }
        let var = match &args[0] {
            syn::Expr::Lit(l) => match &l.lit {
                syn::Lit::Str(s) => s.value(),
                _ => return Err(self.err_en("C0005", format!("`{nombre}!` pide literal de cadena"), &args[0])),
            },
            _ => return Err(self.err_en("C0005", format!("`{nombre}!` pide literal de cadena"), &args[0])),
        };
        match std::env::var(&var) {
            Ok(val) => {
                if nombre == "env" {
                    if let Some(e) = esp {
                        if *e != CType::VistaTexto {
                            return Err(self.err_en(
                                "C0005",
                                format!("se esperaba `{}`, `env!` da `&str`", self.muestra(e)),
                                mac,
                            ));
                        }
                    }
                    let c = self.lit_cstr(&val, mac)?;
                    Ok(ExVal::puro(c, CType::VistaTexto))
                } else {
                    let t_r = CType::Opcion(Box::new(CType::VistaTexto));
                    if let Some(e) = esp {
                        if e != &t_r {
                            return Err(self.err_en(
                                "C0005",
                                format!("se esperaba `{}`, `option_env!` da `Option`", self.muestra(e)),
                                mac,
                            ));
                        }
                    }
                    self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                    let c = self.lit_cstr(&val, mac)?;
                    let to = self.temp(t_r.clone())?;
                    self.emite(format!("memset(&({to}), 0, sizeof({to}));"));
                    self.emite(format!("({to}).tiene = true;"));
                    self.emite(format!("({to}).valor = {c};"));
                    Ok(ExVal::movible(to.clone(), t_r, to))
                }
            }
            Err(_) => {
                if nombre == "option_env" {
                    let t_r = CType::Opcion(Box::new(CType::VistaTexto));
                    if let Some(e) = esp {
                        if e != &t_r {
                            return Err(self.err_en(
                                "C0005",
                                format!("se esperaba `{}`, `option_env!` da `Option`", self.muestra(e)),
                                mac,
                            ));
                        }
                    }
                    self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
                    let to = self.temp(t_r.clone())?;
                    self.emite(format!("memset(&({to}), 0, sizeof({to}));"));
                    return Ok(ExVal::movible(to.clone(), t_r, to));
                }
                if args.len() == 2 {
                    if let syn::Expr::Lit(l) = &args[1] {
                        if let syn::Lit::Str(s) = &l.lit {
                            return Err(self.err_en("C0004", s.value(), &args[1]));
                        }
                    }
                    return Err(self.err_en("C0005", "`env!` pide mensaje literal".into(), &args[1]));
                }
                Err(self.err_en("C0004", format!("variable de entorno `{var}` no existe"), &args[0]))
            }
        }
    }

    /// `cfg!(...)` (solo lo evaluable sin plataforma).
    fn ex_macro_cfg(&mut self, mac: &syn::Macro, esp: Option<&CType>) -> Result<ExVal, CError> {
        if let Some(e) = esp {
            if *e != CType::Bool {
                return Err(self.err_en(
                    "C0005",
                    format!("se esperaba `{}`, `cfg!` da `bool`", self.muestra(e)),
                    mac,
                ));
            }
        }
        let meta: syn::Meta = syn::parse2(mac.tokens.clone())
            .map_err(|e| self.err_en("C0005", format!("`cfg!` mal formado: {e}"), mac))?;
        match Self::eval_cfg(&meta) {
            Some(v) => Ok(ExVal::puro(if v { "1" } else { "0" }.into(), CType::Bool)),
            None => {
                let mut e = self.err_en("C0003", "`cfg!` con predicado no evaluable".into(), mac);
                e.ayuda = Some("solo `unix`, `windows`, `target_family`, `feature`, `all`/`any`/`not`".into());
                Err(e)
            }
        }
    }

    /// Evalúa un predicado `cfg` conocido.
    fn eval_cfg(m: &syn::Meta) -> Option<bool> {
        match m {
            syn::Meta::Path(p) => match p.get_ident().map(|i| i.to_string()).as_deref() {
                Some("unix") => Some(true),
                Some("windows") => Some(false),
                _ => None,
            },
            syn::Meta::List(l) => {
                let op = l.path.get_ident().map(|i| i.to_string())?;
                let items: syn::punctuated::Punctuated<syn::Meta, syn::token::Comma> =
                    l.parse_args_with(syn::punctuated::Punctuated::<syn::Meta, syn::token::Comma>::parse_terminated).ok()?;
                match op.as_str() {
                    "all" => {
                        let mut r = true;
                        for it in &items {
                            r = r && Self::eval_cfg(it)?;
                        }
                        Some(r)
                    }
                    "any" => {
                        let mut r = false;
                        for it in &items {
                            r = r || Self::eval_cfg(it)?;
                        }
                        Some(r)
                    }
                    "not" => {
                        if items.len() != 1 {
                            return None;
                        }
                        Self::eval_cfg(&items[0]).map(|v| !v)
                    }
                    _ => None,
                }
            }
            syn::Meta::NameValue(nv) => {
                let k = nv.path.get_ident().map(|i| i.to_string())?;
                let lit = match &nv.value {
                    syn::Expr::Lit(l) => match &l.lit {
                        syn::Lit::Str(s) => s.value(),
                        _ => return None,
                    },
                    _ => return None,
                };
                match k.as_str() {
                    "target_family" => match lit.as_str() {
                        "unix" => Some(true),
                        "windows" => Some(false),
                        _ => None,
                    },
                    "feature" => Some(false),
                    _ => None,
                }
            }
        }
    }

    /// Constructores llamados (`Some(x)`, `Ok(x)`, `Err(x)`).
    fn ex_ctor_llamada(
        &mut self,
        n: &str,
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        esp: Option<&CType>,
        x: &syn::ExprCall,
    ) -> Result<ExVal, CError> {
        if args.len() != 1 {
            return Err(self.err_en("C0005", format!("`{n}()` lleva un valor"), x));
        }
        if n == "Some" {
            let guia_in: Option<CType> = match esp {
                Some(CType::Opcion(i)) => Some(i.as_ref().clone()),
                Some(e) => {
                    return Err(self.err_en(
                        "C0005",
                        format!("se esperaba `{}`, `Some()` da `Option`", self.muestra(e)),
                        x,
                    ))
                }
                None => None,
            };
            let v = self.baja_expr(&args[0], guia_in.as_ref())?;
            let inner = guia_in.clone().unwrap_or_else(|| v.tipo.clone());
            let t_r = CType::Opcion(Box::new(inner.clone()));
            self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
            let c = self.consume(v, Some(&inner))?;
            let to = self.temp(t_r.clone())?;
            self.emite(format!("memset(&({to}), 0, sizeof({to}));"));
            self.emite(format!("({to}).tiene = true;"));
            self.emite(format!("({to}).valor = ({c});"));
            self.marca_movida_si_temp_pub(&c);
            return Ok(ExVal::movible(to.clone(), t_r, to));
        }
        // `Ok` / `Err`: el otro lado sale del tipo esperado.
        let (ta, tb2) = match esp {
            Some(CType::Resultado(a, b)) => (a.as_ref().clone(), b.as_ref().clone()),
            Some(e) => {
                return Err(self.err_en(
                    "C0005",
                    format!("se esperaba `{}`, `{n}()` da `Result`", self.muestra(e)),
                    x,
                ))
            }
            None => {
                return Err(self.err_en(
                    "C0005",
                    format!("`{n}()` necesita tipo esperado (anota el `let`)"),
                    x,
                ))
            }
        };
        let t_r = CType::Resultado(Box::new(ta.clone()), Box::new(tb2.clone()));
        self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
        let guia = if n == "Ok" { &ta } else { &tb2 };
        let v = self.baja_expr(&args[0], Some(guia))?;
        let c = self.consume(v, Some(guia))?;
        let to = self.temp(t_r.clone())?;
        self.emite(format!("memset(&({to}), 0, sizeof({to}));"));
        if n == "Ok" {
            self.emite(format!("({to}).es_ok = true;"));
            self.emite(format!("({to}).datos.ok = ({c});"));
        } else {
            self.emite(format!("({to}).es_ok = false;"));
            self.emite(format!("({to}).datos.err = ({c});"));
        }
        self.marca_movida_si_temp_pub(&c);
        Ok(ExVal::movible(to.clone(), t_r, to))
    }

    /// `i32::from(x)` y familia (conversiones que conservan el valor).
    fn ex_from_num(
        &mut self,
        t0: &str,
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        esp: Option<&CType>,
        x: &syn::ExprCall,
    ) -> Result<ExVal, CError> {
        let Some(tgt) = Self::num_por_nombre(t0) else {
            return Err(self.err_en("C0002", format!("`{t0}::from` sin cablear (bug interno)"), x));
        };
        if args.len() != 1 {
            return Err(self.err_en("C0005", format!("`{t0}::from` lleva un valor"), x));
        }
        if let Some(e) = esp {
            if e != &tgt {
                return Err(self.err_en(
                    "C0005",
                    format!("se esperaba `{}`, `{t0}::from` da `{t0}`", self.muestra(e)),
                    x,
                ));
            }
        }
        let v = self.baja_expr(&args[0], None)?;
        let ok = match (&tgt, &v.tipo) {
            (a, b) if a == b => true,
            (CType::I16, CType::I8 | CType::U8) => true,
            (CType::I32, CType::I16 | CType::I8 | CType::U16 | CType::U8) => true,
            (CType::I64, CType::I32 | CType::I16 | CType::I8 | CType::U32 | CType::U16 | CType::U8) => true,
            (CType::Isize, CType::I32 | CType::I16 | CType::I8 | CType::U32 | CType::U16 | CType::U8) => true,
            (CType::U16, CType::U8) => true,
            (CType::U32, CType::U16 | CType::U8) => true,
            (CType::U64, CType::U32 | CType::U16 | CType::U8 | CType::Usize) => true,
            (CType::Usize, CType::U32 | CType::U16 | CType::U8) => true,
            (CType::F32, CType::I16 | CType::I8 | CType::U16 | CType::U8) => true,
            (CType::F64, CType::F32 | CType::I32 | CType::I16 | CType::I8 | CType::U32 | CType::U16 | CType::U8) => true,
            (CType::U8, CType::Bool) => true,
            (CType::U32, CType::Char) => true,
            (CType::U64, CType::Char) => true,
            _ => false,
        };
        if !ok {
            let mut e = self.err_en(
                "C0005",
                format!("`{t0}::from` no recibe `{}`", self.muestra(&v.tipo)),
                &args[0],
            );
            e.ayuda = Some("usa `as` para conversiones con pérdida".into());
            return Err(e);
        }
        let c = self.consume(v, None)?;
        let spell = tgt.deletrea().map_err(|e| self.con_archivo(e))?;
        Ok(ExVal::puro(format!("(({spell})({c}))"), tgt))
    }

    /// Nombre de tipo numérico → `CType`.
    fn num_por_nombre(t0: &str) -> Option<CType> {
        match t0 {
            "i8" => Some(CType::I8),
            "i16" => Some(CType::I16),
            "i32" => Some(CType::I32),
            "i64" => Some(CType::I64),
            "isize" => Some(CType::Isize),
            "u8" => Some(CType::U8),
            "u16" => Some(CType::U16),
            "u32" => Some(CType::U32),
            "u64" => Some(CType::U64),
            "usize" => Some(CType::Usize),
            "f32" => Some(CType::F32),
            "f64" => Some(CType::F64),
            _ => None,
        }
    }

    /// `slice::from_ref` / `slice::from_mut` (vista de un elemento).
    fn ex_slice_from(
        &mut self,
        m: &str,
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        esp: Option<&CType>,
        x: &syn::ExprCall,
    ) -> Result<ExVal, CError> {
        if args.len() != 1 {
            return Err(self.err_en("C0005", format!("`slice::{m}` lleva una referencia"), x));
        }
        let es_mut = m == "from_mut";
        let v = self.baja_expr(&args[0], None)?;
        let inner = match &v.tipo {
            CType::Ref(mt, i) if *mt || !es_mut => i.as_ref().clone(),
            _ => {
                return Err(self.err_en(
                    "C0005",
                    format!("`slice::{m}` pide `&T`, no `{}`", self.muestra(&v.tipo)),
                    &args[0],
                ))
            }
        };
        let c = self.consume(v, None)?;
        let t_r = CType::Rebana(Box::new(inner), es_mut);
        if let Some(e) = esp {
            if e != &t_r {
                return Err(self.err_en(
                    "C0005",
                    format!("se esperaba `{}`, `slice::{m}` da rebanada", self.muestra(e)),
                    x,
                ));
            }
        }
        self.registra_mono(&t_r).map_err(|e| self.con_archivo(e))?;
        let tv = self.temp(t_r.clone())?;
        self.emite(format!("({tv}).datos = ({c});"));
        self.emite(format!("({tv}).largo = 1;"));
        Ok(ExVal::movible(tv.clone(), t_r, tv))
    }

    /// `mem::forget` / `replace` / `swap` / `zeroed` / `transmute` / `drop`...
    fn ex_mem_fn(
        &mut self,
        m: &str,
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        esp: Option<&CType>,
        x: &syn::ExprCall,
    ) -> Result<ExVal, CError> {
        match m {
            "forget" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`mem::forget` lleva un valor".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Vacio {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `mem::forget` vale `()`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let v = self.baja_expr(&args[0], None)?;
                if v.movible.is_none() && v.lugar.is_some() && self.necesita_drop(&v.tipo) {
                    return Err(self.err_en(
                        "C0008",
                        "`mem::forget` mueve; ese lugar no se puede mover".into(),
                        &args[0],
                    ));
                }
                let tt = v.tipo.clone();
                let c = self.consume(v, None)?;
                self.marca_movida_si_temp_pub(&c);
                if self.necesita_drop(&tt) {
                    // Se evalúa (efectos) pero no se libera: gotea a propósito.
                    self.emite(format!("(void)({c});"));
                }
                Ok(ExVal::puro("0".into(), CType::Vacio))
            }
            "replace" => {
                if args.len() != 2 {
                    return Err(self.err_en("C0005", "`mem::replace` lleva destino y valor".into(), x));
                }
                let v0 = self.baja_expr(&args[0], None)?;
                let t = match &v0.tipo {
                    CType::Ref(true, i) => i.as_ref().clone(),
                    _ => {
                        return Err(self.err_en(
                            "C0005",
                            format!("`mem::replace` pide `&mut T`, no `{}`", self.muestra(&v0.tipo)),
                            &args[0],
                        ))
                    }
                };
                if let Some(e) = esp {
                    if e != &t {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `{}`", self.muestra(e), self.muestra(&t)),
                            x,
                        ));
                    }
                }
                let c0 = self.consume(v0, None)?;
                let v1 = self.baja_expr(&args[1], Some(&t))?;
                let c1 = self.consume(v1, Some(&t))?;
                let tp = self.temp(CType::Ref(true, Box::new(t.clone())))?;
                self.emite(format!("{tp} = ({c0});"));
                self.marca_movida_si_temp_pub(&c0);
                let to = self.temp(t.clone())?;
                self.emite(format!("{to} = ((*({tp})));"));
                self.emite(format!("(*({tp})) = ({c1});"));
                self.marca_movida_si_temp_pub(&c1);
                Ok(ExVal::movible(to.clone(), t, to))
            }
            "swap" => {
                if args.len() != 2 {
                    return Err(self.err_en("C0005", "`mem::swap` lleva dos destinos".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Vacio {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `mem::swap` vale `()`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let v0 = self.baja_expr(&args[0], None)?;
                let t = match &v0.tipo {
                    CType::Ref(true, i) => i.as_ref().clone(),
                    _ => {
                        return Err(self.err_en(
                            "C0005",
                            format!("`mem::swap` pide `&mut T`, no `{}`", self.muestra(&v0.tipo)),
                            &args[0],
                        ))
                    }
                };
                let t_pr = CType::Ref(true, Box::new(t.clone()));
                let v1 = self.baja_expr(&args[1], Some(&t_pr))?;
                let c0 = self.consume(v0, None)?;
                let c1 = self.consume(v1, Some(&t_pr))?;
                let t0 = self.temp(t_pr.clone())?;
                self.emite(format!("{t0} = ({c0});"));
                self.marca_movida_si_temp_pub(&c0);
                let t1 = self.temp(t_pr)?;
                self.emite(format!("{t1} = ({c1});"));
                self.marca_movida_si_temp_pub(&c1);
                let te = self.temp(t.clone())?;
                self.emite(format!("{te} = ((*({t0})));"));
                self.emite(format!("(*({t0})) = ((*({t1})));"));
                self.emite(format!("(*({t1})) = ({te});"));
                Ok(ExVal::puro("0".into(), CType::Vacio))
            }
            "take" => {
                let mut e = self.err_en("C0003", "`mem::take` necesita `Default`".into(), x);
                e.ayuda = Some("usa `mem::replace` con un valor a mano".into());
                Err(e)
            }
            "zeroed" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`mem::zeroed` no lleva argumentos".into(), x));
                }
                let t = match esp {
                    Some(t) => t.clone(),
                    None => {
                        return Err(self.err_en(
                            "C0005",
                            "`mem::zeroed` necesita tipo esperado (anota el `let`)".into(),
                            x,
                        ))
                    }
                };
                let tr = self.temp(t.clone())?;
                self.emite(format!("memset(&({tr}), 0, sizeof({tr}));"));
                Ok(ExVal::movible(tr.clone(), t, tr))
            }
            "transmute" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`mem::transmute` lleva un valor".into(), x));
                }
                let t = match esp {
                    Some(t) => t.clone(),
                    None => {
                        return Err(self.err_en(
                            "C0005",
                            "`mem::transmute` necesita tipo esperado (anota el `let`)".into(),
                            x,
                        ))
                    }
                };
                let v = self.baja_expr(&args[0], None)?;
                let es_dir = v.movible.is_some() || v.lugar.is_some();
                let tt = v.tipo.clone();
                let c = self.consume(v, None)?;
                let tr = self.temp(t.clone())?;
                if es_dir {
                    self.emite(format!("memcpy(&({tr}), &({c}), sizeof({tr}));"));
                } else {
                    let th = self.temp(tt)?;
                    self.emite(format!("{th} = ({c});"));
                    self.emite(format!("memcpy(&({tr}), &({th}), sizeof({tr}));"));
                    self.marca_movida_si_temp_pub(&th);
                }
                self.marca_movida_si_temp_pub(&c);
                Ok(ExVal::movible(tr.clone(), t, tr))
            }
            "drop" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`mem::drop` lleva un valor".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Vacio {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `mem::drop` vale `()`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let v = self.baja_expr(&args[0], None)?;
                if v.movible.is_none() && v.lugar.is_some() && self.necesita_drop(&v.tipo) {
                    return Err(self.err_en(
                        "C0008",
                        "`mem::drop` mueve; ese lugar no se puede mover".into(),
                        &args[0],
                    ));
                }
                let es_lugar = v.movible.is_some() || v.lugar.is_some();
                let tt = v.tipo.clone();
                let c = self.consume(v, None)?;
                if es_lugar {
                    for l in self.libera_lugar(&c, &tt) {
                        self.emite(l);
                    }
                } else if self.necesita_drop(&tt) {
                    let t = self.temp(tt.clone())?;
                    self.emite(format!("{t} = ({c});"));
                    self.marca_movida_si_temp_pub(&c);
                    for l in self.libera_lugar(&t, &tt) {
                        self.emite(l);
                    }
                }
                Ok(ExVal::puro("0".into(), CType::Vacio))
            }
            "needs_drop" | "uninitialized" | "discriminant" | "offset_of" => {
                let mut e = self.err_en("C0003", format!("`mem::{m}` no tiene soporte"), x);
                e.ayuda = Some("ese reflejo no cabe en C".into());
                Err(e)
            }
            _ => Err(self.err_en("C0004", format!("`mem::{m}` no existe"), x)),
        }
    }

    /// `ptr::null` / `read` / `write` / `copy` / `eq` / `drop_in_place`...
    fn ex_ptr_fn(
        &mut self,
        m: &str,
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        esp: Option<&CType>,
        x: &syn::ExprCall,
    ) -> Result<ExVal, CError> {
        match m {
            "null" | "null_mut" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", format!("`ptr::{m}` no lleva argumentos"), x));
                }
                let t = match esp {
                    Some(CType::Ptr(mt, i)) if *mt == (m == "null_mut") => {
                        CType::Ptr(*mt, i.clone())
                    }
                    Some(e) => {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `ptr::{m}` da puntero", self.muestra(e)),
                            x,
                        ))
                    }
                    None => {
                        return Err(self.err_en(
                            "C0005",
                            format!("`ptr::{m}` necesita tipo esperado (anota el `let`)"),
                            x,
                        ))
                    }
                };
                Ok(ExVal::puro("NULL".into(), t))
            }
            "dangling" => {
                if !args.is_empty() {
                    return Err(self.err_en("C0005", "`ptr::dangling` no lleva argumentos".into(), x));
                }
                let t = match esp {
                    Some(CType::Ptr(mt, i)) => CType::Ptr(*mt, i.clone()),
                    Some(e) => {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `ptr::dangling` da puntero", self.muestra(e)),
                            x,
                        ))
                    }
                    None => {
                        return Err(self.err_en(
                            "C0005",
                            "`ptr::dangling` necesita tipo esperado (anota el `let`)".into(),
                            x,
                        ))
                    }
                };
                let se = match &t {
                    CType::Ptr(_, i) => i.deletrea().map_err(|e| self.con_archivo(e))?,
                    _ => unreachable!(),
                };
                let spell = t.deletrea().map_err(|e| self.con_archivo(e))?;
                Ok(ExVal::puro(format!("(({spell})(_Alignof({se})))"), t))
            }
            "without_provenance" | "with_exposed_provenance" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", format!("`ptr::{m}` lleva una dirección"), x));
                }
                let t = match esp {
                    Some(CType::Ptr(mt, i)) => CType::Ptr(*mt, i.clone()),
                    Some(e) => {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `ptr::{m}` da puntero", self.muestra(e)),
                            x,
                        ))
                    }
                    None => {
                        return Err(self.err_en(
                            "C0005",
                            format!("`ptr::{m}` necesita tipo esperado (anota el `let`)"),
                            x,
                        ))
                    }
                };
                let v = self.baja_expr(&args[0], Some(&CType::Usize))?;
                let cn = self.consume(v, Some(&CType::Usize))?;
                let spell = t.deletrea().map_err(|e| self.con_archivo(e))?;
                Ok(ExVal::puro(format!("(({spell})({cn}))"), t))
            }
            "read" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`ptr::read` lleva un puntero".into(), x));
                }
                let v = self.baja_expr(&args[0], None)?;
                let t = match &v.tipo {
                    CType::Ptr(_, i) => i.as_ref().clone(),
                    _ => {
                        return Err(self.err_en(
                            "C0005",
                            format!("`ptr::read` pide puntero, no `{}`", self.muestra(&v.tipo)),
                            &args[0],
                        ))
                    }
                };
                if t == CType::Vacio {
                    return Err(self.err_en("C0003", "`ptr::read` sobre `void` no tiene soporte".into(), x));
                }
                if let Some(e) = esp {
                    if e != &t {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `{}`", self.muestra(e), self.muestra(&t)),
                            x,
                        ));
                    }
                }
                let c = self.consume(v, None)?;
                Ok(ExVal::puro(format!("((*({c})))"), t))
            }
            "write" => {
                if args.len() != 2 {
                    return Err(self.err_en("C0005", "`ptr::write` lleva puntero y valor".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Vacio {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `ptr::write` vale `()`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let v0 = self.baja_expr(&args[0], None)?;
                let t = match &v0.tipo {
                    CType::Ptr(true, i) => i.as_ref().clone(),
                    _ => {
                        return Err(self.err_en(
                            "C0005",
                            format!("`ptr::write` pide `*mut T`, no `{}`", self.muestra(&v0.tipo)),
                            &args[0],
                        ))
                    }
                };
                let c0 = self.consume(v0, None)?;
                let v1 = self.baja_expr(&args[1], Some(&t))?;
                let c1 = self.consume(v1, Some(&t))?;
                self.emite(format!("(*({c0})) = ({c1});"));
                self.marca_movida_si_temp_pub(&c1);
                Ok(ExVal::puro("0".into(), CType::Vacio))
            }
            "swap" => {
                if args.len() != 2 {
                    return Err(self.err_en("C0005", "`ptr::swap` lleva dos punteros".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Vacio {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `ptr::swap` vale `()`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let v0 = self.baja_expr(&args[0], None)?;
                let t = match &v0.tipo {
                    CType::Ptr(true, i) => i.as_ref().clone(),
                    _ => {
                        return Err(self.err_en(
                            "C0005",
                            format!("`ptr::swap` pide `*mut T`, no `{}`", self.muestra(&v0.tipo)),
                            &args[0],
                        ))
                    }
                };
                let t_p = CType::Ptr(true, Box::new(t.clone()));
                let v1 = self.baja_expr(&args[1], Some(&t_p))?;
                let c0 = self.consume(v0, None)?;
                let c1 = self.consume(v1, Some(&t_p))?;
                let t0 = self.temp(t_p.clone())?;
                self.emite(format!("{t0} = ({c0});"));
                self.marca_movida_si_temp_pub(&c0);
                let t1 = self.temp(t_p)?;
                self.emite(format!("{t1} = ({c1});"));
                self.marca_movida_si_temp_pub(&c1);
                let te = self.temp(t.clone())?;
                self.emite(format!("{te} = ((*({t0})));"));
                self.emite(format!("(*({t0})) = ((*({t1})));"));
                self.emite(format!("(*({t1})) = ({te});"));
                Ok(ExVal::puro("0".into(), CType::Vacio))
            }
            "replace" => {
                if args.len() != 2 {
                    return Err(self.err_en("C0005", "`ptr::replace` lleva puntero y valor".into(), x));
                }
                let v0 = self.baja_expr(&args[0], None)?;
                let t = match &v0.tipo {
                    CType::Ptr(true, i) => i.as_ref().clone(),
                    _ => {
                        return Err(self.err_en(
                            "C0005",
                            format!("`ptr::replace` pide `*mut T`, no `{}`", self.muestra(&v0.tipo)),
                            &args[0],
                        ))
                    }
                };
                if let Some(e) = esp {
                    if e != &t {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `{}`", self.muestra(e), self.muestra(&t)),
                            x,
                        ));
                    }
                }
                let c0 = self.consume(v0, None)?;
                let v1 = self.baja_expr(&args[1], Some(&t))?;
                let c1 = self.consume(v1, Some(&t))?;
                let tp = self.temp(CType::Ptr(true, Box::new(t.clone())))?;
                self.emite(format!("{tp} = ({c0});"));
                self.marca_movida_si_temp_pub(&c0);
                let to = self.temp(t.clone())?;
                self.emite(format!("{to} = ((*({tp})));"));
                self.emite(format!("(*({tp})) = ({c1});"));
                self.marca_movida_si_temp_pub(&c1);
                Ok(ExVal::movible(to.clone(), t, to))
            }
            "copy" | "copy_nonoverlapping" => {
                if args.len() != 3 {
                    return Err(self.err_en("C0005", format!("`ptr::{m}` lleva origen, destino y cuenta"), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Vacio {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `ptr::{m}` vale `()`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let v0 = self.baja_expr(&args[0], None)?;
                let t = match &v0.tipo {
                    CType::Ptr(_, i) => i.as_ref().clone(),
                    _ => {
                        return Err(self.err_en(
                            "C0005",
                            format!("`ptr::{m}` pide puntero, no `{}`", self.muestra(&v0.tipo)),
                            &args[0],
                        ))
                    }
                };
                let t_d = CType::Ptr(true, Box::new(t.clone()));
                let v1 = self.baja_expr(&args[1], Some(&t_d))?;
                let v2 = self.baja_expr(&args[2], Some(&CType::Usize))?;
                let c0 = self.consume(v0, None)?;
                let c1 = self.consume(v1, Some(&t_d))?;
                let cn = self.consume(v2, Some(&CType::Usize))?;
                let se = t.deletrea().map_err(|e| self.con_archivo(e))?;
                if m == "copy" {
                    self.emite(format!("memmove(({c1}), ({c0}), (({cn}) * sizeof({se})));"));
                } else {
                    self.emite(format!("memcpy(({c1}), ({c0}), (({cn}) * sizeof({se})));"));
                }
                Ok(ExVal::puro("0".into(), CType::Vacio))
            }
            "eq" | "addr_eq" => {
                if args.len() != 2 {
                    return Err(self.err_en("C0005", format!("`ptr::{m}` lleva dos punteros"), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Bool {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, da `bool`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let v0 = self.baja_expr(&args[0], None)?;
                let v1 = self.baja_expr(&args[1], None)?;
                for (i, vt) in [&v0.tipo, &v1.tipo].iter().enumerate() {
                    match vt {
                        CType::Ptr(_, inner) if !matches!(inner.as_ref(), CType::Rebana(_, _) | CType::Dyn(_)) => {}
                        _ => {
                            return Err(self.err_en(
                                "C0005",
                                format!("`ptr::{m}` pide puntero fino (arg {})", i + 1),
                                x,
                            ))
                        }
                    }
                }
                let c0 = self.consume(v0, None)?;
                let c1 = self.consume(v1, None)?;
                Ok(ExVal::puro(format!("((({c0}) == ({c1})))"), CType::Bool))
            }
            "drop_in_place" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", "`ptr::drop_in_place` lleva un puntero".into(), x));
                }
                if let Some(e) = esp {
                    if *e != CType::Vacio {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, vale `()`", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let v = self.baja_expr(&args[0], None)?;
                let t = match &v.tipo {
                    CType::Ptr(true, i) => i.as_ref().clone(),
                    _ => {
                        return Err(self.err_en(
                            "C0005",
                            format!("`ptr::drop_in_place` pide `*mut T`, no `{}`", self.muestra(&v.tipo)),
                            &args[0],
                        ))
                    }
                };
                let c = self.consume(v, None)?;
                self.libera_lugar_pub(&format!("(*({c}))"), &t)?;
                Ok(ExVal::puro("0".into(), CType::Vacio))
            }
            "from_ref" | "from_mut" => {
                if args.len() != 1 {
                    return Err(self.err_en("C0005", format!("`ptr::{m}` lleva una referencia"), x));
                }
                let es_mut = m == "from_mut";
                let v = self.baja_expr(&args[0], None)?;
                let t = match &v.tipo {
                    CType::Ref(mt, i) if *mt || !es_mut => i.as_ref().clone(),
                    _ => {
                        return Err(self.err_en(
                            "C0005",
                            format!("`ptr::{m}` pide referencia, no `{}`", self.muestra(&v.tipo)),
                            &args[0],
                        ))
                    }
                };
                let t_r = CType::Ptr(es_mut, Box::new(t));
                if let Some(e) = esp {
                    if e != &t_r {
                        return Err(self.err_en(
                            "C0005",
                            format!("se esperaba `{}`, `ptr::{m}` da puntero", self.muestra(e)),
                            x,
                        ));
                    }
                }
                let c = self.consume(v, None)?;
                Ok(ExVal::puro(c, t_r))
            }
            "slice_from_raw_parts" | "slice_from_raw_parts_mut" | "from_raw_parts" => {
                let mut e = self.err_en("C0003", format!("`ptr::{m}` da puntero gordo"), x);
                e.ayuda = Some("usa rebanadas".into());
                Err(e)
            }
            _ => Err(self.err_en("C0004", format!("`ptr::{m}` no existe"), x)),
        }
    }

    /// Campo de struct/tupla: (nombre C, tipo). `sp` es el nodo campo.
    fn campo_info(
        &self,
        base: &CType,
        m: &syn::Member,
        sp: &syn::ExprField,
    ) -> Result<(String, CType), CError> {
        match base {
            CType::Usuario(n) => {
                if self.enums.contains_key(n) {
                    return Err(self.err_en(
                        "C0005",
                        "los enums no dejan leer campos directo".into(),
                        sp,
                    )
                    .ayuda("usa `match` o `si sea` para abrir la variante"));
                }
                let s = self.structs.get(n).ok_or_else(|| {
                    CError::nuevo("C0002", format!("tipo `{n}` sin registrar (bug interno)"))
                        .con_archivo(self.archivo.clone())
                })?;
                match m {
                    syn::Member::Named(id) => {
                        let q = tipos::desnuda(id);
                        match s.campos.iter().find(|c| c.nombre == q) {
                            Some(c) => Ok((tipos::higieniza(&c.nombre), c.tipo.clone())),
                            None => Err(self.err_en(
                                "C0004",
                                format!("`{n}` no tiene campo `{q}`"),
                                sp,
                            )),
                        }
                    }
                    syn::Member::Unnamed(idx) => {
                        let q = format!("_{}", idx.index);
                        match s.campos.iter().find(|c| c.nombre == q) {
                            Some(c) => Ok((q, c.tipo.clone())),
                            None => Err(self.err_en(
                                "C0004",
                                format!("`{n}` no tiene campo `.{}`", idx.index),
                                sp,
                            )),
                        }
                    }
                }
            }
            CType::Tupla(v) => match m {
                syn::Member::Unnamed(idx) => {
                    let i: usize = idx.index as usize;
                    match v.get(i) {
                        Some(t) => Ok((format!("_{i}"), t.clone())),
                        None => Err(self.err_en(
                            "C0004",
                            format!("la tupla trae {} campos, pediste `.{i}`", v.len()),
                            sp,
                        )),
                    }
                }
                syn::Member::Named(id) => Err(self.err_en(
                    "C0004",
                    format!("las tuplas usan `.{}` con número, no `{}`", 0, tipos::desnuda(id)),
                    sp,
                )),
            },
            CType::Opcion(_) | CType::Resultado(_, _) => Err(self.err_en(
                "C0005",
                format!("`{}` no deja leer campos directo", self.muestra(base)),
                sp,
            )
            .ayuda("usa `match`, `si sea` o `?`")),
            CType::Vec(_) | CType::Rebana(_, _) | CType::Arreglo(_, _) => Err(self.err_en(
                "C0005",
                format!("`{}` no tiene campos", self.muestra(base)),
                sp,
            )
            .ayuda("usa `.len()` / índices")),
            _ => Err(self.err_en(
                "C0005",
                format!("`{}` no tiene campos", self.muestra(base)),
                sp,
            )),
        }
    }

    /// `base[idx]`: baja base e índice, chequea rango, da (lugar C, tipo elem).
    fn indice_lugar(
        &mut self,
        base: &syn::Expr,
        idx: &syn::Expr,
    ) -> Result<(String, CType), CError> {
        let b = self.baja_expr(base, None)?;
        let bt = b.tipo.clone();
        let (bc, bt) = if b.arreglo.is_some() {
            let (tmp, t) = self.aloja_arreglo(b)?;
            (tmp, t)
        } else if b.movible.is_some() || b.lugar.is_some() {
            (b.c.clone(), bt)
        } else {
            let t = self.temp(bt.clone())?;
            self.emite(format!("{t} = ({});", b.c));
            (t, bt)
        };
        let mut bc = bc;
        let mut bt = bt;
        loop {
            match bt {
                CType::Ref(_, i) | CType::Caja(i) => {
                    bc = format!("(*({bc}))");
                    bt = (*i).clone();
                }
                _ => break,
            }
        }
        let vi = self.baja_expr(idx, Some(&CType::Usize))?;
        let ci = self.consume(vi, Some(&CType::Usize))?;
        let ti = self.temp(CType::Usize)?;
        self.emite(format!("{ti} = ({ci});"));
        match bt {
            CType::Vec(e) | CType::Rebana(e, _) => {
                self.emite(format!(
                    "if ((({ti}) >= (({bc}).largo))) {{ KAMI_PANICO(\"índice fuera de rango\"); }}"
                ));
                Ok((format!("(({bc}).datos[({ti})])"), (*e).clone()))
            }
            CType::Arreglo(e, l) => {
                self.emite(format!(
                    "if ((({ti}) >= (size_t)({}))) {{ KAMI_PANICO(\"índice fuera de rango\"); }}",
                    l.deletrea()
                ));
                Ok((format!("(({bc})[({ti})])"), (*e).clone()))
            }
            CType::Ptr(_, e) => Ok((format!("(({bc})[({ti})])"), (*e).clone())),
            CType::Texto | CType::VistaTexto => Err(self.err_en(
                "C0005",
                "no se puede indexar texto".into(),
                idx,
            )
            .ayuda("usa `.as_bytes()[i]` o `.chars()`")),
            CType::Usuario(n) if self.enums.contains_key(&n) => {
                Err(self.err_en("C0005", "los enums no se indexan".into(), idx))
            }
            CType::Usuario(_) => Err(self.err_en(
                "C0003",
                "indexar ese tipo pide `Index`".into(),
                idx,
            )
            .ayuda("expón un método que devuelva el elemento")),
            otro => Err(self.err_en(
                "C0005",
                format!("no se puede indexar `{}`", self.muestra(&otro)),
                idx,
            )),
        }
    }

    /// Materializa un arreglo diferido en un temporal. Da (nombre, tipo).
    pub(crate) fn aloja_arreglo(&mut self, v: ExVal) -> Result<(String, CType), CError> {
        let CType::Arreglo(elem, largo) = v.tipo.clone() else {
            return Err(CError::nuevo("C0002", "se esperaba arreglo (bug interno)".to_string())
                .con_archivo(self.archivo.clone()));
        };
        let t_arr = CType::Arreglo(elem.clone(), largo.clone());
        self.registra_uso_pub(&t_arr)?;
        let tmp = self.temp(t_arr.clone())?;
        self.materializa_arreglo(&tmp, v, &elem, &largo, false)?;
        Ok((tmp, t_arr))
    }

    /// Arreglo diferido como argumento: materializa y ajusta al parámetro.
    fn conv_arreglo(&mut self, v: ExVal, pt: &CType) -> Result<String, CError> {
        let t_arr = v.tipo.clone();
        let (elem, _largo) = match &t_arr {
            CType::Arreglo(e, l) => (e.as_ref().clone(), l.clone()),
            _ => {
                return Err(CError::nuevo("C0002", "se esperaba arreglo (bug interno)".to_string())
                    .con_archivo(self.archivo.clone()))
            }
        };
        let (tmp, t_arr) = self.aloja_arreglo(v)?;
        match pt {
            CType::Ref(_, inner) if inner.as_ref() == &t_arr => Ok(format!("&({tmp})")),
            CType::Rebana(y, _) if y.as_ref() == &elem => {
                let m = self.registra_mono(pt)?;
                let es = elem.deletrea()?;
                let l = match &t_arr {
                    CType::Arreglo(_, l) => l.deletrea(),
                    _ => unreachable!(),
                };
                Ok(format!("({m}){{ ({es}*)({tmp}), {l} }}"))
            }
            _ => Err(CError::nuevo(
                "C0005",
                format!("ese arreglo no entra donde va `{}` (¿falta `&`?)", self.muestra(pt)),
            )
            .con_archivo(self.archivo.clone())),
        }
    }
    /// `[a, b, c]`: literal diferido (se materializa al consumir).
    fn ex_array(&mut self, x: &syn::ExprArray, esp: Option<&CType>) -> Result<ExVal, CError> {
        let (guia_elem, esp_largo) = match esp {
            Some(CType::Arreglo(e, l)) => (Some(e.as_ref().clone()), Some(l.clone())),
            Some(e) => {
                return Err(self.err_en(
                    "C0005",
                    format!("se esperaba `{}`, llegó arreglo", self.muestra(e)),
                    x,
                ))
            }
            None => (None, None),
        };
        if let Some(LargoArreglo::Lit(k)) = &esp_largo {
            if *k != x.elems.len() {
                return Err(self.err_en(
                    "C0005",
                    format!("el arreglo trae {} elementos, se esperaban {k}", x.elems.len()),
                    x,
                ));
            }
        }
        let mut outs = Vec::new();
        let mut t_elem = guia_elem;
        for e in &x.elems {
            let v = self.baja_expr(e, t_elem.as_ref())?;
            if t_elem.is_none() {
                t_elem = Some(v.tipo.clone());
            }
            let t0 = t_elem.clone().unwrap();
            let c = self.consume(v, Some(&t0))?;
            outs.push(ElemLista { c, tipo: t0 });
        }
        let elem = match t_elem {
            Some(t) => t,
            None => {
                return Err(self.err_en(
                    "C0005",
                    "`[]` vacío necesita tipo (anota el `let`)".into(),
                    x,
                ))
            }
        };
        if matches!(elem, CType::Vacio | CType::Infer | CType::Dyn(_)) {
            return Err(self.err_en("C0003", "elemento de arreglo inválido".into(), x));
        }
        let t = CType::Arreglo(Box::new(elem), LargoArreglo::Lit(x.elems.len()));
        Ok(ExVal {
            c: String::new(),
            tipo: t,
            movible: None,
            lugar: None,
            arreglo: Some(ArrayInit::Lista(outs)),
        })
    }

    /// `[v; n]`: repetición diferida (`n` debe ser constante).
    fn ex_repite(&mut self, x: &syn::ExprRepeat, esp: Option<&CType>) -> Result<ExVal, CError> {
        let (guia_elem, esp_largo) = match esp {
            Some(CType::Arreglo(e, l)) => (Some(e.as_ref().clone()), Some(l.clone())),
            Some(e) => {
                return Err(self.err_en(
                    "C0005",
                    format!("se esperaba `{}`, llegó arreglo", self.muestra(e)),
                    x,
                ))
            }
            None => (None, None),
        };
        let (c_n, v_n) = self.baja_expr_const(&x.len)?;
        let k_lit: Option<usize> = match v_n {
            Some(k) => match usize::try_from(k) {
                Ok(k) => Some(k),
                Err(_) => return Err(self.err_en("C0003", "arreglo gigante".into(), &x.len)),
            },
            None => None,
        };
        if let (Some(k), Some(LargoArreglo::Lit(j))) = (k_lit, &esp_largo) {
            if k != *j {
                return Err(self.err_en(
                    "C0005",
                    format!("el arreglo trae {k} elementos, se esperaban {j}"),
                    x,
                ));
            }
        }
        let largo = match k_lit {
            Some(k) => LargoArreglo::Lit(k),
            None => LargoArreglo::Expr(c_n),
        };
        let v = self.baja_expr(&x.expr, guia_elem.as_ref())?;
        let elem = match guia_elem {
            Some(t) => t,
            None => v.tipo.clone(),
        };
        // Dueños + largo no literal = VLA sin `={0}` (ilegal en C): se exige literal.
        if k_lit.is_none() && self.necesita_drop(&elem) {
            return Err(self.err_en(
                "C0003",
                "el largo con dueños debe ser literal".into(),
                &x.len,
            )
            .ayuda("usa un número o una const entera"));
        }
        if matches!(elem, CType::Vacio | CType::Infer | CType::Dyn(_)) {
            return Err(self.err_en("C0003", "elemento de arreglo inválido".into(), &x.expr));
        }
        // OJO: se clona por vuelta (Rust pediría `Copy`); leniencia documentada.
        let c = self.consume(v, Some(&elem))?;
        let t = CType::Arreglo(Box::new(elem.clone()), largo.clone());
        Ok(ExVal {
            c: String::new(),
            tipo: t,
            movible: None,
            lugar: None,
            arreglo: Some(ArrayInit::Repite { c, tipo: elem, n: largo.deletrea() }),
        })
    }

    /// `S { x }` / `E::V { x }` / `S { x, ..base }`: literal de struct o
    /// variante con campos.
    fn ex_struct(&mut self, x: &syn::ExprStruct, esp: Option<&CType>) -> Result<ExVal, CError> {
        if x.qself.is_some() {
            return Err(self.err_en("C0003", "literal con `Self` calificado no cabe".into(), x));
        }
        if x.path.segments.iter().any(|s| {
            matches!(&s.arguments, syn::PathArguments::AngleBracketed(ab) if !ab.args.is_empty())
        }) {
            return Err(self.err_en("C0003", "structs sin genéricos".into(), x)
                .ayuda("quita el `::<...>`"));
        }
        let segs: Vec<String> =
            x.path.segments.iter().map(|s| tipos::desnuda(&s.ident)).collect();
        let raiz = x.path.leading_colon.is_some();
        // Destino: struct o variante con campos.
        enum Dest {
            Struct { nc: String, campos: Vec<super::baja::CampoInfo>, es_union: bool },
            Variante { nc: String, var: String, campos: Vec<super::baja::CampoInfo> },
        }
        let dest = if segs == ["Self"] {
            match &self.tipo_self {
                Some(n) => match self.structs.get(n) {
                    Some(s) => Dest::Struct {
                        nc: n.clone(),
                        campos: s.campos.clone(),
                        es_union: s.es_union,
                    },
                    None => return Err(self.err_en("C0004", "`Self` no es struct".into(), x)),
                },
                None => return Err(self.err_en("C0003", "`Self` fuera de `impl`".into(), x)),
            }
        } else if let Some(nc) = self.resuelve_tipo_pub(&segs, raiz) {
            if let Some(s) = self.structs.get(&nc) {
                Dest::Struct { nc, campos: s.campos.clone(), es_union: s.es_union }
            } else if self.enums.contains_key(&nc) {
                return Err(self.err_en(
                    "C0004",
                    format!("`{}` es enum: falta la variante", segs.join("::")),
                    x,
                ));
            } else {
                return Err(self.err_en(
                    "C0002",
                    format!("tipo `{nc}` sin registrar (bug interno)"),
                    x,
                ));
            }
        } else if segs.len() >= 2 {
            let nt = segs.len() - 1;
            let nc = match self.resuelve_tipo_pub(&segs[..nt], raiz) {
                Some(nc) => nc,
                None => {
                    return Err(self.err_en(
                        "C0004",
                        format!("tipo `{}` desconocido", segs[..nt].join("::")),
                        x,
                    ))
                }
            };
            let e = match self.enums.get(&nc) {
                Some(e) => e.clone(),
                None => {
                    return Err(self.err_en(
                        "C0004",
                        format!("`{}` no es enum ni struct", segs.join("::")),
                        x,
                    ))
                }
            };
            let var = segs[nt].clone();
            let v = match e.variantes.iter().find(|v| v.nombre == var) {
                Some(v) => v.clone(),
                None => {
                    return Err(self.err_en(
                        "C0004",
                        format!("`{nc}` no tiene variante `{var}`"),
                        x,
                    ))
                }
            };
            match &v.campos {
                super::baja::CamposVariante::Struct(cs) => {
                    Dest::Variante { nc, var, campos: cs.clone() }
                }
                super::baja::CamposVariante::Unit => {
                    return Err(self.err_en(
                        "C0003",
                        format!("`{var}` es un valor (sin llaves)"),
                        x,
                    ))
                }
                super::baja::CamposVariante::Tuple(_) => {
                    return Err(self.err_en(
                        "C0003",
                        format!("`{var}` se construye con paréntesis"),
                        x,
                    ))
                }
            }
        } else {
            return Err(self.err_en(
                "C0004",
                format!("tipo `{}` desconocido", segs.join("::")),
                x,
            ));
        };
        let (nc, campos, var, es_union) = match &dest {
            Dest::Struct { nc, campos, es_union } => (nc.clone(), campos.clone(), None, *es_union),
            Dest::Variante { nc, var, campos } => (nc.clone(), campos.clone(), Some(var.clone()), false),
        };
        if var.is_none() && !es_union && campos.iter().all(|c| c.nombre.starts_with('_')) && !campos.is_empty() {
            return Err(self.err_en(
                "C0003",
                format!("`{nc}` se construye con paréntesis"),
                x,
            ));
        }
        if es_union && x.fields.len() > 1 {
            return Err(self.err_en("C0003", "la unión se inicia con un campo".into(), x));
        }
        if es_union && x.rest.is_some() {
            return Err(self.err_en("C0003", "`..base` no vale en uniones".into(), x));
        }
        let t = CType::Usuario(nc.clone());
        if let Some(e) = esp {
            if e != &t {
                return Err(self.err_en(
                    "C0005",
                    format!("se esperaba `{}`, da `{nc}`", self.muestra(e)),
                    x,
                ));
            }
        }
        let mut vistos = std::collections::HashSet::new();
        let mut asigs: Vec<(String, CType, String)> = Vec::new();
        for f in &x.fields {
            let q = match &f.member {
                syn::Member::Named(id) => tipos::desnuda(id),
                syn::Member::Unnamed(idx) => format!("_{}", idx.index),
            };
            if !vistos.insert(q.clone()) {
                return Err(self.err_en("C0005", format!("campo `{q}` repetido"), &f.member));
            }
            let fc = match campos.iter().find(|c| c.nombre == q) {
                Some(fc) => fc.clone(),
                None => {
                    return Err(self.err_en(
                        "C0004",
                        format!("`{nc}` no tiene campo `{q}`"),
                        &f.member,
                    ))
                }
            };
            let v = self.baja_expr(&f.expr, Some(&fc.tipo))?;
            let c = self.consume(v, Some(&fc.tipo))?;
            asigs.push((tipos::higieniza(&q), fc.tipo, c));
        }
        // `..base` (o chequeo de faltantes).
        let base_c: Option<(String, bool)> = match &x.rest {
            Some(r) => {
                let vb = self.baja_expr(r, Some(&t))?;
                let es_dir = vb.movible.is_some() || vb.lugar.is_some();
                let cb = self.consume(vb, Some(&t))?;
                Some((cb, es_dir))
            }
            None => {
                let falt: Vec<String> =
                    campos.iter().filter(|c| !vistos.contains(&c.nombre)).map(|c| c.nombre.clone()).collect();
                if !falt.is_empty() {
                    return Err(self.err_en(
                        "C0005",
                        format!("faltan campos: {}", falt.join(", ")),
                        x,
                    ));
                }
                None
            }
        };
        let tr = self.temp(t.clone())?;
        self.emite(format!("memset(&({tr}), 0, sizeof({tr}));"));
        if let Some((cb, es_dir)) = base_c {
            if es_dir {
                self.emite(format!("memcpy(&({tr}), &({cb}), sizeof({tr}));"));
            } else {
                let th = self.temp(t.clone())?;
                self.emite(format!("{th} = ({cb});"));
                self.emite(format!("memcpy(&({tr}), &({th}), sizeof({tr}));"));
                self.marca_movida_si_temp_pub(&th);
            }
            self.marca_movida_si_temp_pub(&cb);
        }
        for (nc_f, ft, c) in &asigs {
            let d = match &var {
                Some(vv) => format!("({tr}).datos.{vv}.{nc_f}"),
                None => format!("({tr}).{nc_f}"),
            };
            if matches!(ft, CType::Arreglo(_, _)) {
                self.emite(format!("memcpy({d}, ({c}), sizeof({d}));"));
            } else {
                self.emite(format!("{d} = ({c});"));
            }
            self.marca_movida_si_temp_pub(c);
        }
        if let Some(vv) = &var {
            self.emite(format!("({tr}).etiqueta = {nc}_{vv};"));
        }
        Ok(ExVal::movible(tr.clone(), t, tr))
    }

    /// `(a, b)`: tupla (mono `KamiTupla_...`). `()` vale `()`.
    fn ex_tuple(&mut self, x: &syn::ExprTuple, esp: Option<&CType>) -> Result<ExVal, CError> {
        if x.elems.is_empty() {
            if let Some(e) = esp {
                if *e != CType::Vacio {
                    return Err(self.err_en(
                        "C0005",
                        format!("se esperaba `{}`, `()` vale `()`", self.muestra(e)),
                        x,
                    ));
                }
            }
            return Ok(ExVal::puro("0".into(), CType::Vacio));
        }
        let guias: Option<&Vec<CType>> = match esp {
            Some(CType::Tupla(v)) => {
                if v.len() != x.elems.len() {
                    return Err(self.err_en(
                        "C0005",
                        format!("la tupla trae {} campos, se esperaban {}", x.elems.len(), v.len()),
                        x,
                    ));
                }
                Some(v)
            }
            Some(e) => {
                return Err(self.err_en(
                    "C0005",
                    format!("se esperaba `{}`, llegó tupla", self.muestra(e)),
                    x,
                ))
            }
            None => None,
        };
        let mut ts = Vec::new();
        let mut cs = Vec::new();
        for (i, e) in x.elems.iter().enumerate() {
            let g = guias.map(|v| &v[i]);
            let v = self.baja_expr(e, g)?;
            let t = match g {
                Some(t) => (*t).clone(),
                None => v.tipo.clone(),
            };
            let c = self.consume(v, Some(&t))?;
            ts.push(t);
            cs.push(c);
        }
        if ts.iter().any(|t| matches!(t, CType::Vacio | CType::Infer | CType::Dyn(_))) {
            return Err(self.err_en("C0003", "elemento de tupla inválido".into(), x));
        }
        let t = CType::Tupla(ts.clone());
        self.registra_mono(&t).map_err(|e| self.con_archivo(e))?;
        let tr = self.temp(t.clone())?;
        for (i, (c, ft)) in cs.iter().zip(ts.iter()).enumerate() {
            let d = format!("({tr})._{i}");
            if matches!(ft, CType::Arreglo(_, _)) {
                self.emite(format!("memcpy({d}, ({c}), sizeof({d}));"));
            } else {
                self.emite(format!("{d} = ({c});"));
            }
            self.marca_movida_si_temp_pub(c);
        }
        Ok(ExVal::movible(tr.clone(), t, tr))
    }

    /// `base.campo`: acceso con autoderef (`&S`/`Box`/`*` se pelan).
    fn ex_field(&mut self, x: &syn::ExprField, esp: Option<&CType>) -> Result<ExVal, CError> {
        let b = self.baja_expr(&x.base, None)?;
        let b = if b.arreglo.is_some() {
            let (tmp, t) = self.aloja_arreglo(b)?;
            ExVal::lugar(tmp.clone(), t, tmp)
        } else {
            b
        };
        let bc = if b.movible.is_some() || b.lugar.is_some() {
            b.c.clone()
        } else {
            let t = self.temp(b.tipo.clone())?;
            self.emite(format!("{t} = ({});", b.c));
            t
        };
        let mut acc = bc;
        let mut bt = b.tipo.clone();
        loop {
            match bt {
                CType::Ref(_, i) | CType::Caja(i) | CType::Ptr(_, i) => {
                    acc = format!("(*({acc}))");
                    bt = (*i).clone();
                }
                _ => break,
            }
        }
        let (nc, tf) = self.campo_info(&bt, &x.member, x)?;
        if let Some(e) = esp {
            if e != &tf {
                return Err(self.err_en(
                    "C0005",
                    format!("se esperaba `{}`, el campo da `{}`", self.muestra(e), self.muestra(&tf)),
                    x,
                ));
            }
        }
        let pl = format!("(({acc}).{nc})");
        Ok(ExVal::lugar(pl.clone(), tf, pl))
    }

    /// `base[idx]`: delega en `indice_lugar` (chequea rango).
    fn ex_index(&mut self, x: &syn::ExprIndex, esp: Option<&CType>) -> Result<ExVal, CError> {
        let (pl, te) = self.indice_lugar(&x.expr, &x.index)?;
        if let Some(e) = esp {
            if e != &te {
                return Err(self.err_en(
                    "C0005",
                    format!("se esperaba `{}`, el elemento da `{}`", self.muestra(e), self.muestra(&te)),
                    x,
                ));
            }
        }
        Ok(ExVal::lugar(pl.clone(), te, pl))
    }

    /// `&x` / `&mut x`: referencia a lugar (o temporal direccionado).
    fn ex_ref(&mut self, x: &syn::ExprReference, esp: Option<&CType>) -> Result<ExVal, CError> {
        let es_mut = x.mutability.is_some();
        let b = self.baja_expr(&x.expr, None)?;
        let b = if b.arreglo.is_some() {
            let (tmp, t) = self.aloja_arreglo(b)?;
            ExVal::lugar(tmp.clone(), t, tmp)
        } else {
            b
        };
        let t = CType::Ref(es_mut, Box::new(b.tipo.clone()));
        if let Some(e) = esp {
            if e != &t {
                return Err(self.err_en(
                    "C0005",
                    format!("se esperaba `{}`, da `{}`", self.muestra(e), self.muestra(&t)),
                    x,
                ));
            }
        }
        if es_mut && matches!(b.tipo, CType::Ref(false, _)) {
            return Err(self.err_en("C0005", "no se puede pedir `&mut` desde `&`".into(), x));
        }
        // OJO: sin rastro de `mut` en variables, `&mut x` siempre pasa; leniencia.
        let c = if let Some(l) = b.lugar.clone() {
            format!("&({l})")
        } else if let Some(m) = b.movible.clone() {
            format!("&({m})")
        } else {
            self.direcciona_pub(b.c.clone(), None, &b.tipo)?
        };
        Ok(ExVal::puro(c, t))
    }

    /// `&raw const/mut x`: puntero crudo a lugar (sin chequeos).
    fn ex_rawaddr(&mut self, x: &syn::ExprRawAddr) -> Result<ExVal, CError> {
        let es_mut = matches!(x.mutability, syn::PointerMutability::Mut(_));
        let b = self.baja_expr(&x.expr, None)?;
        let b = if b.arreglo.is_some() {
            let (tmp, t) = self.aloja_arreglo(b)?;
            ExVal::lugar(tmp.clone(), t, tmp)
        } else {
            b
        };
        let t = CType::Ptr(es_mut, Box::new(b.tipo.clone()));
        let c = if let Some(l) = b.lugar.clone() {
            format!("&({l})")
        } else if let Some(m) = b.movible.clone() {
            format!("&({m})")
        } else {
            self.direcciona_pub(b.c.clone(), None, &b.tipo)?
        };
        Ok(ExVal::puro(c, t))
    }

    /// `x as T`: conversiones numéricas (saturando flotante→entero), `u8 as
    /// char`, casts de punteros y `&T as *const T`.
    fn ex_cast(&mut self, x: &syn::ExprCast, esp: Option<&CType>) -> Result<ExVal, CError> {
        let t = self.baja_tipo(&x.ty)?;
        let b = self.baja_expr(&x.expr, None)?;
        let b = if b.arreglo.is_some() {
            let (tmp, ta) = self.aloja_arreglo(b)?;
            ExVal::lugar(tmp.clone(), ta, tmp)
        } else {
            b
        };
        let f = b.tipo.clone();
        if let Some(e) = esp {
            if e != &t {
                return Err(self.err_en(
                    "C0005",
                    format!("se esperaba `{}`, el `as` da `{}`", self.muestra(e), self.muestra(&t)),
                    x,
                ));
            }
        }
        if f == t {
            let c = self.consume(b, Some(&t))?;
            return Ok(ExVal::puro(c, t));
        }
        let es_int = |t: &CType| {
            matches!(
                t,
                CType::I8
                    | CType::I16
                    | CType::I32
                    | CType::I64
                    | CType::Isize
                    | CType::U8
                    | CType::U16
                    | CType::U32
                    | CType::U64
                    | CType::Usize
            )
        };
        let es_float = |t: &CType| matches!(t, CType::F32 | CType::F64);
        // Flotante → entero: saturación (Rust; C sería indefinido).
        if es_float(&f) && es_int(&t) {
            let c = self.consume(b, None)?;
            let tf = self.temp(f.clone())?;
            self.emite(format!("{tf} = ({c});"));
            self.marca_movida_si_temp_pub(&c);
            let (lo_f, hi_f, lo_c, hi_c) = match t {
                CType::I8 => ("-128.0", "127.0", "INT8_MIN", "INT8_MAX"),
                CType::I16 => ("-32768.0", "32767.0", "INT16_MIN", "INT16_MAX"),
                CType::I32 => ("-2147483648.0", "2147483647.0", "INT32_MIN", "INT32_MAX"),
                CType::I64 => (
                    "-9223372036854775808.0",
                    "9223372036854775807.0",
                    "INT64_MIN",
                    "INT64_MAX",
                ),
                CType::Isize => (
                    "-9223372036854775808.0",
                    "9223372036854775807.0",
                    "INTPTR_MIN",
                    "INTPTR_MAX",
                ),
                CType::U8 => ("-1.0", "255.0", "0", "UINT8_MAX"),
                CType::U16 => ("-1.0", "65535.0", "0", "UINT16_MAX"),
                CType::U32 => ("-1.0", "4294967295.0", "0", "UINT32_MAX"),
                CType::U64 => ("-1.0", "18446744073709551615.0", "0", "UINT64_MAX"),
                CType::Usize => ("-1.0", "18446744073709551615.0", "0", "SIZE_MAX"),
                _ => unreachable!(),
            };
            let spell = t.deletrea().map_err(|e| self.con_archivo(e))?;
            let v = format!(
                "((({tf}) != ({tf})) ? (0) : ((({tf}) >= ({hi_f})) ? ({hi_c}) : ((({tf}) <= ({lo_f})) ? ({lo_c}) : (({spell})({tf})))))"
            );
            return Ok(ExVal::puro(v, t));
        }
        // `u8 as char` (único entero→char válido).
        if f == CType::U8 && t == CType::Char {
            let c = self.consume(b, None)?;
            return Ok(ExVal::puro(format!("((KamiChar)({c}))"), t));
        }
        if es_int(&f) && t == CType::Char {
            return Err(self.err_en(
                "C0005",
                format!("solo `u8 as char`; llegó `{}`", self.muestra(&f)),
                x,
            )
            .ayuda("usa `char::from_u32`"));
        }
        // Bool/char/float/entero → número o flotante.
        if (f == CType::Bool || f == CType::Char || es_int(&f) || es_float(&f))
            && (es_int(&t) || es_float(&t))
        {
            if f == CType::Bool && es_float(&t) {
                return Err(self.err_en("C0005", "`bool as` flotante no vale".into(), x)
                    .ayuda("usa `si x { 1.0 } si_no { 0.0 }`"));
            }
            if f == CType::Char && es_float(&t) {
                return Err(self.err_en("C0005", "`char as` flotante no vale".into(), x)
                    .ayuda("pasa por `u32` primero"));
            }
            let c = self.consume(b, None)?;
            let spell = t.deletrea().map_err(|e| self.con_archivo(e))?;
            return Ok(ExVal::puro(format!("(({spell})({c}))"), t));
        }
        // Punteros entre sí, a/desde `usize`/`isize`, y `&T as *`.
        match (&f, &t) {
            (CType::Ptr(_, _), CType::Ptr(_, _)) => {
                let c = self.consume(b, None)?;
                let spell = t.deletrea().map_err(|e| self.con_archivo(e))?;
                return Ok(ExVal::puro(format!("(({spell})({c}))"), t));
            }
            (CType::Ptr(_, _), CType::Usize) | (CType::Ptr(_, _), CType::Isize) => {
                let c = self.consume(b, None)?;
                let spell = t.deletrea().map_err(|e| self.con_archivo(e))?;
                return Ok(ExVal::puro(format!("(({spell})({c}))"), t));
            }
            (CType::Usize, CType::Ptr(_, _)) => {
                let c = self.consume(b, None)?;
                let spell = t.deletrea().map_err(|e| self.con_archivo(e))?;
                return Ok(ExVal::puro(format!("(({spell})({c}))"), t));
            }
            (CType::Ref(mf, i), CType::Ptr(mt, j)) if i.as_ref() == j.as_ref() && (*mf || !*mt) => {
                let c = self.consume(b, None)?;
                let spell = t.deletrea().map_err(|e| self.con_archivo(e))?;
                return Ok(ExVal::puro(format!("(({spell})({c}))"), t));
            }
            (CType::FnPtr { .. }, CType::Ptr(_, _))
            | (CType::FnPtr { .. }, CType::Usize)
            | (CType::FnPtr { .. }, CType::Isize) => {
                let c = self.consume(b, None)?;
                let spell = t.deletrea().map_err(|e| self.con_archivo(e))?;
                return Ok(ExVal::puro(format!("(({spell})({c}))"), t));
            }
            _ => {}
        }
        if matches!(f, CType::Ptr(_, _)) && es_int(&t) {
            return Err(self.err_en(
                "C0005",
                "puntero `as` entero: solo `usize`/`isize`".into(),
                x,
            )
            .ayuda("pasa por `usize` primero"));
        }
        if es_int(&f) && matches!(t, CType::Ptr(_, _)) {
            return Err(self.err_en(
                "C0005",
                "entero `as` puntero: solo desde `usize`".into(),
                x,
            )
            .ayuda("usa `ptr::without_provenance`"));
        }
        Err(self.err_en(
            "C0005",
            format!("no se puede convertir `{}` as `{}`", self.muestra(&f), self.muestra(&t)),
            x,
        ))
    }

    /// `expr?`: desenvuelve `Option`/`Result` o retorna temprano (sin `From`:
    /// el error debe coincidir con el de la función).
    fn ex_try(&mut self, x: &syn::ExprTry, esp: Option<&CType>) -> Result<ExVal, CError> {
        let b = self.baja_expr(&x.expr, None)?;
        let b = if b.arreglo.is_some() {
            let (tmp, ta) = self.aloja_arreglo(b)?;
            ExVal::lugar(tmp.clone(), ta, tmp)
        } else {
            b
        };
        let bc = if b.movible.is_some() || b.lugar.is_some() {
            b.c.clone()
        } else {
            let t = self.temp(b.tipo.clone())?;
            self.emite(format!("{t} = ({});", b.c));
            t
        };
        // Pela `&` (ergonomía del `match`).
        let mut acc = bc;
        let mut bt = b.tipo.clone();
        loop {
            match bt {
                CType::Ref(_, i) => {
                    acc = format!("(*({acc}))");
                    bt = (*i).clone();
                }
                _ => break,
            }
        }
        let (inner_t, err_t) = match &bt {
            CType::Opcion(i) => (i.as_ref().clone(), None),
            CType::Resultado(o, e) => (o.as_ref().clone(), Some(e.as_ref().clone())),
            _ => {
                return Err(self.err_en(
                    "C0005",
                    format!("`?` pide `Option` o `Result`, llegó `{}`", self.muestra(&bt)),
                    x,
                ))
            }
        };
        if let Some(e) = esp {
            if e != &inner_t {
                return Err(self.err_en(
                    "C0005",
                    format!("se esperaba `{}`, `?` da `{}`", self.muestra(e), self.muestra(&inner_t)),
                    x,
                ));
            }
        }
        let ret = self.fn_ret.clone();
        match (&ret, &bt) {
            (CType::Opcion(_), CType::Opcion(_)) => {}
            (CType::Resultado(_, fe), CType::Resultado(_, e)) if fe.as_ref() == e.as_ref() => {}
            _ => {
                return Err(self.err_en(
                    "C0005",
                    format!(
                        "`?` no convierte: la función da `{}`, llegó `{}`",
                        self.muestra(&ret),
                        self.muestra(&bt)
                    ),
                    x,
                )
                .ayuda("usa `match` o `map_err` a mano"))
            }
        }
        if err_t.is_none() {
            self.emite(format!("if ((!(({acc}).tiene))) {{"));
            let m = self.registra_mono(&ret).map_err(|e| self.con_archivo(e))?;
            let limp = self.limpieza_hasta(0);
            self.emite_todas(limp);
            self.emite(format!("return ({m}){{0}};"));
            self.emite("}".to_string());
            let pl = format!("(({acc}).valor)");
            Ok(ExVal::lugar(pl.clone(), inner_t, pl))
        } else {
            self.emite(format!("if ((!(({acc}).es_ok))) {{"));
            let e_t = err_t.unwrap();
            let ev = ExVal::lugar(format!("(({acc}).datos.err)"), e_t.clone(), format!("(({acc}).datos.err)"));
            let ce = self.consume(ev, Some(&e_t))?;
            let tr = self.temp(ret.clone())?;
            self.emite(format!("({tr}).es_ok = false;"));
            self.emite(format!("({tr}).datos.err = ({ce});"));
            self.marca_movida_si_temp_pub(&ce);
            let limp = self.limpieza_hasta(0);
            self.emite_todas(limp);
            self.emite(format!("return ({tr});"));
            self.emite("}".to_string());
            let pl = format!("(({acc}).datos.ok)");
            Ok(ExVal::lugar(pl.clone(), inner_t, pl))
        }
    }

    /// Libera un lugar y emite las líneas (envoltorio de `libera_lugar`).
    fn libera_lugar_pub(&mut self, lugar: &str, tipo: &CType) -> Result<(), CError> {
        for l in self.libera_lugar(lugar, tipo) {
            self.emite(l);
        }
        Ok(())
    }

    /// Mueve un valor (`dst = src` bit a bit + marca origen si es temporal).
    /// El destino debe estar fresco (sin contenido viejo que liberar).
    fn mueve_valor_pub(
        &mut self,
        origen: &str,
        destino: &str,
        tipo: &CType,
    ) -> Result<(), CError> {
        if matches!(tipo, CType::Arreglo(_, _)) {
            self.emite(format!("memcpy(({destino}), ({origen}), sizeof({destino}));"));
        } else {
            self.emite(format!("{destino} = ({origen});"));
        }
        self.marca_movida_si_temp_pub(origen);
        Ok(())
    }

    /// Macro en posición de valor (delega en `ex_macro`).
    fn baja_macro_valor(
        &mut self,
        mac: &syn::Macro,
        esp: Option<&CType>,
    ) -> Result<ExVal, CError> {
        self.ex_macro(mac, esp)
    }

    /// `Punto(a, b)` / `E::Var(a, b)` / `S()`: tupla-struct, variante tupla o
    /// struct unitario. `None` = no es constructor (sigue otra resolución).
    fn ex_ctor_tupla(
        &mut self,
        ruta: &[String],
        raiz: bool,
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        esp: Option<&CType>,
        x: &syn::ExprCall,
    ) -> Result<Option<ExVal>, CError> {
        if ruta.len() >= 2 {
            let nt = ruta.len() - 1;
            let nc = match self.resuelve_tipo_pub(&ruta[..nt], raiz) {
                Some(nc) => nc,
                None => return Ok(None),
            };
            let e = match self.enums.get(&nc) {
                Some(e) => e.clone(),
                None => return Ok(None),
            };
            let var = ruta[nt].clone();
            let v = match e.variantes.iter().find(|v| v.nombre == var) {
                Some(v) => v.clone(),
                None => return Ok(None),
            };
            let ts = match &v.campos {
                super::baja::CamposVariante::Tuple(ts) => ts.clone(),
                super::baja::CamposVariante::Unit => {
                    return Err(self.err_en(
                        "C0003",
                        format!("`{}::{var}` es un valor (sin paréntesis)", ruta[..nt].join("::")),
                        x,
                    ))
                }
                super::baja::CamposVariante::Struct(_) => {
                    return Err(self.err_en(
                        "C0003",
                        format!("`{}::{var}` se construye con llaves", ruta[..nt].join("::")),
                        x,
                    ))
                }
            };
            if ts.len() != args.len() {
                return Err(self.err_en(
                    "C0005",
                    format!("`{var}` pide {} campos, llegaron {}", ts.len(), args.len()),
                    x,
                ));
            }
            let t = CType::Usuario(nc.clone());
            if let Some(e2) = esp {
                if e2 != &t {
                    return Err(self.err_en(
                        "C0005",
                        format!("se esperaba `{}`, da `{nc}`", self.muestra(e2)),
                        x,
                    ));
                }
            }
            let tr = self.temp(t.clone())?;
            self.emite(format!("memset(&({tr}), 0, sizeof({tr}));"));
            self.emite(format!("({tr}).etiqueta = {nc}_{var};"));
            for (i, (a, ft)) in args.iter().zip(ts.iter()).enumerate() {
                let v = self.baja_expr(a, Some(ft))?;
                let c = self.consume(v, Some(ft))?;
                let d = format!("({tr}).datos.{var}._{i}");
                if matches!(ft, CType::Arreglo(_, _)) {
                    self.emite(format!("memcpy({d}, ({c}), sizeof({d}));"));
                } else {
                    self.emite(format!("{d} = ({c});"));
                }
                self.marca_movida_si_temp_pub(&c);
            }
            return Ok(Some(ExVal::movible(tr.clone(), t, tr)));
        }
        let nc = match self.resuelve_tipo_pub(ruta, raiz) {
            Some(nc) => nc,
            None => return Ok(None),
        };
        let s = match self.structs.get(&nc) {
            Some(s) => s.clone(),
            None => return Ok(None),
        };
        if !s.campos.iter().all(|c| c.nombre.starts_with('_')) {
            return Err(self.err_en(
                "C0003",
                format!("`{}` se construye con llaves", ruta.join("::")),
                x,
            ));
        }
        if s.campos.len() != args.len() {
            return Err(self.err_en(
                "C0005",
                format!(
                    "`{}` pide {} campos, llegaron {}",
                    ruta.join("::"),
                    s.campos.len(),
                    args.len()
                ),
                x,
            ));
        }
        let t = CType::Usuario(nc.clone());
        if let Some(e2) = esp {
            if e2 != &t {
                return Err(self.err_en(
                    "C0005",
                    format!("se esperaba `{}`, da `{nc}`", self.muestra(e2)),
                    x,
                ));
            }
        }
        let tr = self.temp(t.clone())?;
        self.emite(format!("memset(&({tr}), 0, sizeof({tr}));"));
        for (i, (a, fc)) in args.iter().zip(s.campos.iter()).enumerate() {
            let v = self.baja_expr(a, Some(&fc.tipo))?;
            let c = self.consume(v, Some(&fc.tipo))?;
            let d = format!("({tr})._{i}");
            if matches!(fc.tipo, CType::Arreglo(_, _)) {
                self.emite(format!("memcpy({d}, ({c}), sizeof({d}));"));
            } else {
                self.emite(format!("{d} = ({c});"));
            }
            self.marca_movida_si_temp_pub(&c);
        }
        Ok(Some(ExVal::movible(tr.clone(), t, tr)))
    }

    /// Constante por ruta (busca de adentro hacia afuera, como fns/tipos).
    fn const_por_ruta(&self, ruta: &[String], raiz: bool) -> Option<super::baja::InfoConst> {
        if raiz {
            let c = ruta.iter().map(|p| tipos::higieniza(p)).collect::<Vec<_>>().join("__");
            return self.consts.get(&c).cloned();
        }
        for k in (0..=self.mods.len()).rev() {
            let mut v: Vec<String> = self.mods[..k].to_vec();
            v.extend(ruta.iter().cloned());
            let c = v.iter().map(|p| tipos::higieniza(p)).collect::<Vec<_>>().join("__");
            if let Some(i) = self.consts.get(&c) {
                return Some(i.clone());
            }
        }
        None
    }

    /// Evalúa una expresión constante: (código C, valor si se conoce).
    /// Los booleanos valen 0/1; el desborde da `None` (C envuelve igual).
    pub(crate) fn baja_expr_const(&self, e: &syn::Expr) -> Result<(String, Option<i128>), CError> {
        let mut vis = std::collections::HashSet::new();
        self.const_expr(e, &mut vis)
    }

    pub(crate) fn const_expr(
        &self,
        e: &syn::Expr,
        vis: &mut std::collections::HashSet<String>,
    ) -> Result<(String, Option<i128>), CError> {
        match e {
            syn::Expr::Lit(x) => self.const_lit(&x.lit),
            syn::Expr::Paren(x) => self.const_expr(&x.expr, vis),
            syn::Expr::Group(x) => self.const_expr(&x.expr, vis),
            syn::Expr::Unary(x) => {
                let (c, v) = self.const_expr(&x.expr, vis)?;
                match x.op {
                    syn::UnOp::Neg(_) => Ok((format!("-({c})"), v.and_then(|k| k.checked_neg()))),
                    syn::UnOp::Not(_) => {
                        let nv = match (c.as_str(), v) {
                            ("true", _) => Some(0),
                            ("false", _) => Some(1),
                            (_, Some(k)) => Some(!k),
                            _ => None,
                        };
                        Ok((format!("!({c})"), nv))
                    }
                    _ => Err(self.err_en("C0003", "ese operador no es constante".into(), x)),
                }
            }
            syn::Expr::Binary(x) => {
                let (cl, vl) = self.const_expr(&x.left, vis)?;
                let (cr, vr) = self.const_expr(&x.right, vis)?;
                let op = match x.op {
                    syn::BinOp::Add(_) => "+",
                    syn::BinOp::Sub(_) => "-",
                    syn::BinOp::Mul(_) => "*",
                    syn::BinOp::Div(_) => "/",
                    syn::BinOp::Rem(_) => "%",
                    syn::BinOp::And(_) => "&&",
                    syn::BinOp::Or(_) => "||",
                    syn::BinOp::BitAnd(_) => "&",
                    syn::BinOp::BitOr(_) => "|",
                    syn::BinOp::BitXor(_) => "^",
                    syn::BinOp::Shl(_) => "<<",
                    syn::BinOp::Shr(_) => ">>",
                    syn::BinOp::Eq(_) => "==",
                    syn::BinOp::Ne(_) => "!=",
                    syn::BinOp::Lt(_) => "<",
                    syn::BinOp::Le(_) => "<=",
                    syn::BinOp::Gt(_) => ">",
                    syn::BinOp::Ge(_) => ">=",
                    _ => return Err(self.err_en("C0003", "ese operador no es constante".into(), x)),
                };
                let v = match (vl, vr) {
                    (Some(a), Some(b)) => self.const_bin(&x.op, a, b, x)?,
                    _ => None,
                };
                Ok((format!("(({cl}) {op} ({cr}))"), v))
            }
            syn::Expr::Cast(x) => {
                let (c, v) = self.const_expr(&x.expr, vis)?;
                let (spell, envuelve): (Option<&str>, fn(i128) -> i128) = match &*x.ty {
                    syn::Type::Path(tp) if tp.qself.is_none() && tp.path.segments.len() == 1 => {
                        let id = tp.path.segments[0].ident.to_string();
                        if !matches!(
                            tp.path.segments[0].arguments,
                            syn::PathArguments::None
                        ) {
                            return Err(self.err_en("C0003", "ese `as` no es constante".into(), x));
                        }
                        match id.as_str() {
                            "bool" => return Err(self.err_en(
                                "C0005",
                                "`as bool` no vale".into(),
                                x,
                            )),
                            "char" => (Some("KamiChar"), |k| k),
                            "i8" => (Some("int8_t"), |k| (k as i8) as i128),
                            "i16" => (Some("int16_t"), |k| (k as i16) as i128),
                            "i32" => (Some("int32_t"), |k| (k as i32) as i128),
                            "i64" => (Some("int64_t"), |k| (k as i64) as i128),
                            "isize" => (None, |k| (k as i64) as i128),
                            "u8" => (Some("uint8_t"), |k| (k as u8) as i128),
                            "u16" => (Some("uint16_t"), |k| (k as u16) as i128),
                            "u32" => (Some("uint32_t"), |k| (k as u32) as i128),
                            "u64" => (Some("uint64_t"), |k| (k as u64) as i128),
                            "usize" => (None, |k| (k as u64) as i128),
                            "f32" | "f64" => (None, |_| 0),
                            _ => return Err(self.err_en("C0003", "ese `as` no es constante".into(), x)),
                        }
                    }
                    _ => return Err(self.err_en("C0003", "ese `as` no es constante".into(), x)),
                };
                let nv = match (&*x.ty, v) {
                    (syn::Type::Path(tp), Some(k))
                        if matches!(tp.path.segments[0].ident.to_string().as_str(), "f32" | "f64") =>
                    {
                        let _ = k;
                        None
                    }
                    (_, Some(k)) => Some(envuelve(k)),
                    _ => None,
                };
                match spell {
                    Some(s) => Ok((format!("(({s})({c}))"), nv)),
                    None => Ok((format!("({c})"), nv)),
                }
            }
            syn::Expr::Path(x) => {
                if x.qself.is_some() {
                    return Err(self.err_en("C0003", "eso no es constante".into(), x));
                }
                if x.path.segments.iter().any(|s| !matches!(s.arguments, syn::PathArguments::None)) {
                    return Err(self.err_en("C0003", "eso no es constante".into(), x));
                }
                let mut segs: Vec<String> =
                    x.path.segments.iter().map(|s| tipos::desnuda(&s.ident)).collect();
                let raiz = x.path.leading_colon.is_some();
                if raiz && segs.first().is_some_and(|s| s == "crate") {
                    segs.remove(0);
                }
                if !raiz && segs.first().is_some_and(|s| s == "crate") {
                    segs.remove(0);
                    let info = self.const_por_ruta(&segs, true);
                    return self.const_de_info(info, &segs.join("::"), x, vis);
                }
                let clave_m = segs.join("::");
                let info = self.const_por_ruta(&segs, raiz);
                self.const_de_info(info, &clave_m, x, vis)
            }
            syn::Expr::Block(x) => {
                if x.label.is_some() || !x.attrs.is_empty() {
                    return Err(self.err_en("C0003", "eso no es constante".into(), x));
                }
                if x.block.stmts.len() == 1 {
                    if let syn::Stmt::Expr(e, None) = &x.block.stmts[0] {
                        return self.const_expr(e, vis);
                    }
                }
                Err(self.err_en("C0003", "eso no es constante".into(), x))
            }
            _ => Err(self.err_en("C0003", "eso no es constante".into(), e)
                .ayuda("usa literales, operadores o consts")),
        }
    }

    /// Valor de una const ya resuelta (con chequeo de ciclos).
    fn const_de_info(
        &self,
        info: Option<super::baja::InfoConst>,
        clave_m: &str,
        sp: &syn::ExprPath,
        vis: &mut std::collections::HashSet<String>,
    ) -> Result<(String, Option<i128>), CError> {
        let info = match info {
            Some(i) => i,
            None => {
                return Err(self.err_en(
                    "C0004",
                    format!("const `{clave_m}` desconocida"),
                    sp,
                ))
            }
        };
        if let Some(d) = &info.define {
            let v = d
                .trim()
                .trim_start_matches('(')
                .trim_end_matches(')')
                .trim()
                .parse::<i128>()
                .ok();
            return Ok((d.clone(), v));
        }
        match &info.expr {
            Some(ex) => {
                if !vis.insert(info.nombre_c.clone()) {
                    return Err(self.err_en(
                        "C0005",
                        format!("const `{clave_m}` cíclica"),
                        sp,
                    ));
                }
                let r = self.const_expr(ex, vis);
                vis.remove(&info.nombre_c);
                r
            }
            None => Ok((info.nombre_c.clone(), None)),
        }
    }

    /// Aritmética/comparación constante (`/0` es error; desborde → `None`).
    fn const_bin(
        &self,
        op: &syn::BinOp,
        a: i128,
        b: i128,
        sp: &syn::ExprBinary,
    ) -> Result<Option<i128>, CError> {
        match op {
            syn::BinOp::Add(_) => Ok(a.checked_add(b)),
            syn::BinOp::Sub(_) => Ok(a.checked_sub(b)),
            syn::BinOp::Mul(_) => Ok(a.checked_mul(b)),
            syn::BinOp::Div(_) => {
                if b == 0 {
                    return Err(self.err_en("C0005", "división entre cero".into(), sp));
                }
                Ok(a.checked_div(b))
            }
            syn::BinOp::Rem(_) => {
                if b == 0 {
                    return Err(self.err_en("C0005", "división entre cero".into(), sp));
                }
                Ok(a.checked_rem(b))
            }
            syn::BinOp::BitAnd(_) => Ok(Some(a & b)),
            syn::BinOp::BitOr(_) => Ok(Some(a | b)),
            syn::BinOp::BitXor(_) => Ok(Some(a ^ b)),
            syn::BinOp::Shl(_) => {
                Ok(u32::try_from(b).ok().and_then(|s| a.checked_shl(s)))
            }
            syn::BinOp::Shr(_) => {
                Ok(u32::try_from(b).ok().and_then(|s| a.checked_shr(s)))
            }
            syn::BinOp::Eq(_) => Ok(Some((a == b) as i128)),
            syn::BinOp::Ne(_) => Ok(Some((a != b) as i128)),
            syn::BinOp::Lt(_) => Ok(Some((a < b) as i128)),
            syn::BinOp::Le(_) => Ok(Some((a <= b) as i128)),
            syn::BinOp::Gt(_) => Ok(Some((a > b) as i128)),
            syn::BinOp::Ge(_) => Ok(Some((a >= b) as i128)),
            syn::BinOp::And(_) => Ok(Some(((a != 0) && (b != 0)) as i128)),
            syn::BinOp::Or(_) => Ok(Some(((a != 0) || (b != 0)) as i128)),
            _ => Ok(None),
        }
    }

    /// Literal constante: (código C, valor).
    fn const_lit(&self, lit: &syn::Lit) -> Result<(String, Option<i128>), CError> {
        match lit {
            syn::Lit::Int(i) => {
                let tok = i.to_string();
                let suf = i.suffix();
                let d = tok.strip_suffix(suf).unwrap_or(&tok);
                let (radix, digs) = if let Some(h) = d.strip_prefix("0x").or_else(|| d.strip_prefix("0X")) {
                    (16, h)
                } else if let Some(h) = d.strip_prefix("0o").or_else(|| d.strip_prefix("0O")) {
                    (8, h)
                } else if let Some(h) = d.strip_prefix("0b").or_else(|| d.strip_prefix("0B")) {
                    (2, h)
                } else {
                    (10, d)
                };
                let digs: String = digs.chars().filter(|c| *c != '_').collect();
                let v = i128::from_str_radix(&digs, radix).ok();
                match v {
                    Some(k) => Ok((k.to_string(), Some(k))),
                    None => Ok((tok, None)),
                }
            }
            syn::Lit::Bool(b) => Ok((
                (if b.value { "true" } else { "false" }).into(),
                Some(if b.value { 1 } else { 0 }),
            )),
            syn::Lit::Char(c) => {
                let n = c.value() as u32 as i128;
                Ok((n.to_string(), Some(n)))
            }
            syn::Lit::Byte(b) => {
                let n = b.value() as i128;
                Ok((n.to_string(), Some(n)))
            }
            syn::Lit::Str(s) => Ok((Self::esc_cstr(&s.value()), None)),
            syn::Lit::ByteStr(s) => Ok((Self::esc_cstr_bytes(&s.value()), None)),
            syn::Lit::CStr(s) => Ok((Self::esc_cstr(&s.value().to_string_lossy()), None)),
            syn::Lit::Float(f) => {
                let tok = f.to_string();
                let suf = f.suffix();
                let d = tok.strip_suffix(suf).unwrap_or(&tok);
                let c = match suf {
                    "f32" => format!("{d}f"),
                    _ => d.to_string(),
                };
                Ok((c, None))
            }
            _ => Err(CError::nuevo("C0003", "ese literal no es constante".to_string())
                .con_archivo(self.archivo.clone())),
        }
    }

    /// `"..."` C escapado desde un `&str`.
    pub(crate) fn esc_cstr(s: &str) -> String {
        let mut o = String::with_capacity(s.len() + 2);
        o.push('"');
        for c in s.chars() {
            match c {
                '"' => o.push_str("\\\""),
                '\\' => o.push_str("\\\\"),
                '\n' => o.push_str("\\n"),
                '\r' => o.push_str("\\r"),
                '\t' => o.push_str("\\t"),
                '\0' => o.push_str("\\0"),
                c => o.push(c),
            }
        }
        o.push('"');
        o
    }

    /// `"..."` C escapado desde bytes.
    pub(crate) fn esc_cstr_bytes(b: &[u8]) -> String {
        let mut o = String::with_capacity(b.len() + 2);
        o.push('"');
        for &c in b {
            match c {
                b'"' => o.push_str("\\\""),
                b'\\' => o.push_str("\\\\"),
                b'\n' => o.push_str("\\n"),
                b'\r' => o.push_str("\\r"),
                b'\t' => o.push_str("\\t"),
                0 => o.push_str("\\0"),
                32..=126 => o.push(c as char),
                _ => o.push_str(&format!("\\x{c:02x}")),
            }
        }
        o.push('"');
        o
    }

    /// `{ ... }` como expresión (también `const { }` y `unsafe { }`).
    fn ex_bloque(&mut self, b: &syn::Block, esp: Option<&CType>) -> Result<ExVal, CError> {
        match esp {
            None | Some(CType::Vacio) => {
                self.entra();
                self.baja_bloque_contenido(b, &Destino::Descarta)?;
                let buf = self.sale();
                self.emite("{".to_string());
                for l in buf {
                    self.emite(format!("    {l}"));
                }
                self.emite("}".to_string());
                Ok(ExVal::puro("0".into(), CType::Vacio))
            }
            Some(t) => {
                let tiene_cola = b.stmts.last().is_some_and(|s| match s {
                    syn::Stmt::Expr(_, None) => true,
                    syn::Stmt::Macro(m) => m.semi_token.is_none(),
                    _ => false,
                });
                if !tiene_cola {
                    return Err(self.err_en(
                        "C0005",
                        format!("se esperaba `{}`, el bloque no da valor", self.muestra(t)),
                        b,
                    ));
                }
                let tr = self.temp(t.clone())?;
                self.entra();
                self.baja_bloque_contenido(b, &Destino::Temp(tr.clone()))?;
                let buf = self.sale();
                self.emite("{".to_string());
                for l in buf {
                    self.emite(format!("    {l}"));
                }
                self.emite("}".to_string());
                Ok(ExVal::movible(tr.clone(), t.clone(), tr))
            }
        }
    }

    /// `si cond { } si_no { }` como expresión (con o sin valor).
    fn ex_if(&mut self, x: &syn::ExprIf, esp: Option<&CType>) -> Result<ExVal, CError> {
        match esp {
            None | Some(CType::Vacio) => {
                self.ex_if_en(x, &Destino::Descarta)?;
                Ok(ExVal::puro("0".into(), CType::Vacio))
            }
            Some(t) => {
                if x.else_branch.is_none() {
                    return Err(self.err_en(
                        "C0005",
                        format!("se esperaba `{}`, falta `si_no`", self.muestra(t)),
                        x,
                    ));
                }
                let tr = self.temp(t.clone())?;
                self.ex_if_en(x, &Destino::Temp(tr.clone()))?;
                Ok(ExVal::movible(tr.clone(), t.clone(), tr))
            }
        }
    }

    /// `si` (y `si_no si` en cadena) hacia un destino.
    fn ex_if_en(&mut self, x: &syn::ExprIf, dest: &Destino) -> Result<(), CError> {
        match &*x.cond {
            syn::Expr::Let(l) => {
                let v = self.baja_expr(&l.expr, None)?;
                let t_scrut = v.tipo.clone();
                let ts = if v.movible.is_some() || v.lugar.is_some() {
                    v.c.clone()
                } else if let CType::Arreglo(elem, largo) = t_scrut.clone() {
                    let tmp = self.temp(t_scrut.clone())?;
                    self.materializa_arreglo(&tmp, v, &elem, &largo, false)?;
                    tmp
                } else {
                    let tmp = self.temp(t_scrut.clone())?;
                    let c = self.consume(v, Some(&t_scrut))?;
                    self.emite(format!("{tmp} = ({c});"));
                    self.marca_movida_si_temp_pub(&c);
                    tmp
                };
                let (cond, enlaces) = self.analiza_patron(&l.pat, &t_scrut, &ts)?;
                self.emite(format!("if (({cond})) {{"));
                self.entra();
                self.emite_enlaces(&enlaces)?;
                self.baja_bloque_contenido(&x.then_branch, dest)?;
                let buf = self.sale();
                for l in buf {
                    self.emite(format!("    {l}"));
                }
                self.emite("}".to_string());
            }
            _ => {
                let v = self.baja_expr(&x.cond, Some(&CType::Bool))?;
                let c = self.consume(v, Some(&CType::Bool))?;
                self.emite(format!("if (({c})) {{"));
                self.entra();
                self.baja_bloque_contenido(&x.then_branch, dest)?;
                let buf = self.sale();
                for l in buf {
                    self.emite(format!("    {l}"));
                }
                self.emite("}".to_string());
            }
        }
        if let Some((_, e)) = &x.else_branch {
            match &**e {
                syn::Expr::If(x2) => {
                    // Con bloque: la condición trae preámbulo (temps/chequeos).
                    self.emite("else {".to_string());
                    self.entra();
                    self.ex_if_en(x2, dest)?;
                    let buf = self.sale();
                    for l in buf {
                        self.emite(format!("    {l}"));
                    }
                    self.emite("}".to_string());
                }
                syn::Expr::Block(b) => {
                    self.emite("else {".to_string());
                    self.entra();
                    self.baja_bloque_contenido(&b.block, dest)?;
                    let buf = self.sale();
                    for l in buf {
                        self.emite(format!("    {l}"));
                    }
                    self.emite("}".to_string());
                }
                _ => {
                    return Err(CError::nuevo("C0002", "si_no inválido (bug interno)".to_string())
                        .con_archivo(self.archivo.clone()))
                }
            }
        }
        Ok(())
    }

    /// `match scrut { brazos }` (parte `A | B` en brazos gemelos).
    fn ex_match(&mut self, x: &syn::ExprMatch, esp: Option<&CType>) -> Result<ExVal, CError> {
        let v = self.baja_expr(&x.expr, None)?;
        let t_scrut = v.tipo.clone();
        if t_scrut == CType::Infer {
            return Err(self.err_en("C0005", "`match` sin tipo (anota)".into(), &x.expr));
        }
        let ts = if v.movible.is_some() || v.lugar.is_some() {
            v.c.clone()
        } else if let CType::Arreglo(elem, largo) = t_scrut.clone() {
            let tmp = self.temp(t_scrut.clone())?;
            self.materializa_arreglo(&tmp, v, &elem, &largo, false)?;
            tmp
        } else {
            let tmp = self.temp(t_scrut.clone())?;
            let c = self.consume(v, Some(&t_scrut))?;
            self.emite(format!("{tmp} = ({c});"));
            self.marca_movida_si_temp_pub(&c);
            tmp
        };
        let (dest, ret) = match esp {
            None | Some(CType::Vacio) => (Destino::Descarta, None),
            Some(t) => {
                let tr = self.temp(t.clone())?;
                (Destino::Temp(tr.clone()), Some(tr))
            }
        };
        // Brazos (`A | B` se parte; los enlaces deben coincidir).
        struct Brazo {
            pat: syn::Pat,
            guardia: Option<syn::Expr>,
            cuerpo: syn::Expr,
        }
        let mut brazos: Vec<Brazo> = Vec::new();
        for a in &x.arms {
            let cuerpo = (*a.body).clone();
            let guardia = a.guard.as_ref().map(|(_, g)| (**g).clone());
            match &a.pat {
                syn::Pat::Or(o) => {
                    for caso in &o.cases {
                        brazos.push(Brazo {
                            pat: caso.clone(),
                            guardia: guardia.clone(),
                            cuerpo: cuerpo.clone(),
                        });
                    }
                }
                otro => brazos.push(Brazo {
                    pat: otro.clone(),
                    guardia: guardia.clone(),
                    cuerpo,
                }),
            }
        }
        if brazos.is_empty() {
            return Err(self.err_en("C0005", "`match` sin brazos".into(), x));
        }
        let mut primero = true;
        let mut ultimo_abierto = false;
        for br in &brazos {
            let (cond, enlaces) = self.analiza_patron(&br.pat, &t_scrut, &ts)?;
            ultimo_abierto = cond == "true" && br.guardia.is_none();
            self.emite(format!("{} (({cond})) {{", if primero { "if" } else { "} else if" }));
            primero = false;
            self.entra();
            self.emite_enlaces(&enlaces)?;
            if let Some(g) = &br.guardia {
                let gv = self.baja_expr(g, Some(&CType::Bool))?;
                let gc = self.consume(gv, Some(&CType::Bool))?;
                self.emite(format!("if (({gc})) {{"));
                self.entra();
                self.baja_cola(&br.cuerpo, &dest)?;
                let buf2 = self.sale();
                for l in buf2 {
                    self.emite(format!("    {l}"));
                }
                self.emite("}".to_string());
            } else {
                self.baja_cola(&br.cuerpo, &dest)?;
            }
            let buf = self.sale();
            for l in buf {
                self.emite(format!("    {l}"));
            }
        }
        if ultimo_abierto {
            self.emite("}".to_string());
        } else {
            // Sin red (no exhaustivo a ojos del backend): pánico, no basura.
            self.emite("} else { KAMI_PANICO(\"match sin brazo\"); }".to_string());
        }
        match ret {
            Some(tr) => {
                let t = esp.unwrap().clone();
                Ok(ExVal::movible(tr.clone(), t, tr))
            }
            None => Ok(ExVal::puro("0".into(), CType::Vacio)),
        }
    }

    /// `para pat en iter { }`: rangos, `Option`, listas y arreglos.
    /// Forma `goto` (el `continuar` cae en el incremento).
    fn ex_for(&mut self, x: &syn::ExprForLoop) -> Result<ExVal, CError> {
        let etiqueta = x.label.as_ref().map(|l| tipos::desnuda(&l.name.ident));
        let id = self.etiqueta_n;
        self.etiqueta_n += 1;
        // Rango: se baja aparte (inicio/fin/límites).
        if let syn::Expr::Range(r) = &*x.expr {
            return self.ex_for_rango(x, r, etiqueta, id);
        }
        let v = self.baja_expr(&x.expr, None)?;
        let t_iter = v.tipo.clone();
        // `Option`: una sola vuelta si hay valor.
        if let CType::Opcion(inner) = &t_iter {
            let inner = inner.as_ref().clone();
            let ts = if v.movible.is_some() || v.lugar.is_some() {
                v.c.clone()
            } else {
                let tmp = self.temp(t_iter.clone())?;
                let c = self.consume(v, Some(&t_iter))?;
                self.emite(format!("{tmp} = ({c});"));
                self.marca_movida_si_temp_pub(&c);
                tmp
            };
            self.emite(format!("if ((!(({ts}).tiene))) goto fin{id};"));
            self.entra();
            let prof = self.pila.len() - 1;
            self.bucles.push(InfoBucle { etiqueta, id, prof_cuerpo: prof, valor: None });
            let (cond, enlaces) = self.analiza_patron(&x.pat, &inner, &format!("(({ts}).valor)"))?;
            if cond != "true" {
                self.bucles.pop();
                return Err(self.err_en(
                    "C0003",
                    "ese patrón puede fallar en el `para`".into(),
                    &x.pat,
                ));
            }
            self.emite_enlaces(&enlaces)?;
            self.baja_bloque_contenido(&x.body, &Destino::Descarta)?;
            self.bucles.pop();
            let buf = self.sale();
            self.emite("{".to_string());
            for l in buf {
                self.emite(format!("    {l}"));
            }
            self.emite("}".to_string());
            self.emite(format!("sig{id}: ;"));
            self.emite(format!("fin{id}: ;"));
            return Ok(ExVal::puro("0".into(), CType::Vacio));
        }
        // Listas: (elem, sitio(i), largo); `&` pela y presta.
        let (base_c, elem, largo_c, presta): (String, CType, String, bool) = match &t_iter {
            CType::Vec(e) | CType::Rebana(e, _) => {
                let bc = Self::sitio_base_for(&v)?;
                (bc, e.as_ref().clone(), String::new(), false)
            }
            CType::Arreglo(e, l) => {
                let bc = Self::sitio_base_for(&v)?;
                (bc, e.as_ref().clone(), l.deletrea(), false)
            }
            CType::Ref(m, i) => match i.as_ref() {
                CType::Vec(e) | CType::Rebana(e, _) => {
                    let bc = Self::sitio_base_for(&v)?;
                    (bc, e.as_ref().clone(), String::new(), *m)
                }
                CType::Arreglo(e, l) => {
                    let bc = Self::sitio_base_for(&v)?;
                    (bc, e.as_ref().clone(), l.deletrea(), *m)
                }
                _ => {
                    return Err(self.err_en(
                        "C0005",
                        format!("no se itera `{}`", self.muestra(&t_iter)),
                        &x.expr,
                    ))
                }
            },
            _ => {
                return Err(self.err_en(
                    "C0003",
                    format!("no se itera `{}`", self.muestra(&t_iter)),
                    &x.expr,
                )
                .ayuda("itera por índice o usa rangos y rebanadas"))
            }
        };
        // Base estable (sin consumir: se presta).
        let bc = if v.movible.is_some() || v.lugar.is_some() {
            base_c
        } else if v.arreglo.is_some() {
            let (tmp, _) = self.aloja_arreglo(v)?;
            tmp
        } else {
            let tmp = self.temp(t_iter.clone())?;
            let c = self.consume(v, Some(&t_iter))?;
            self.emite(format!("{tmp} = ({c});"));
            self.marca_movida_si_temp_pub(&c);
            tmp
        };
        // OJO: `para x en vec` no marca (clona elementos; leniencia).
        let es_arr = match &t_iter {
            CType::Arreglo(_, _) => true,
            CType::Ref(_, b) => matches!(b.as_ref(), CType::Arreglo(_, _)),
            _ => false,
        };
        let largo_rt = if es_arr { largo_c } else { String::new() };
        let ti = self.temp(CType::Usize)?;
        self.emite(format!("{ti} = 0;"));
        self.emite(format!("goto cond{id};"));
        self.emite(format!("sig{id}: ++({ti});"));
        self.emite(format!("cond{id}: ;"));
        if es_arr {
            self.emite(format!("if (!((({ti}) < (size_t)({largo_rt})))) goto fin{id};"));
        } else {
            self.emite(format!("if (!((({ti}) < (({bc}).largo)))) goto fin{id};"));
        }
        self.entra();
        let prof = self.pila.len() - 1;
        self.bucles.push(InfoBucle { etiqueta, id, prof_cuerpo: prof, valor: None });
        // Sitio del elemento (pela `&` si presta).
        let (t_pat, sitio) = if matches!(&t_iter, CType::Ref(_, _)) {
            let s = if es_arr {
                format!("(((*({bc})))[({ti})])")
            } else {
                format!("(((*({bc}))).datos[({ti})])")
            };
            (CType::Ref(presta, Box::new(elem)), format!("&({s})"))
        } else if es_arr {
            (elem, format!("(({bc})[({ti})])"))
        } else {
            (elem, format!("(({bc}).datos[({ti})])"))
        };
        let (cond, enlaces) = self.analiza_patron(&x.pat, &t_pat, &sitio)?;
        if cond != "true" {
            self.bucles.pop();
            return Err(self.err_en(
                "C0003",
                "ese patrón puede fallar en el `para`".into(),
                &x.pat,
            ));
        }
        self.emite_enlaces(&enlaces)?;
        self.baja_bloque_contenido(&x.body, &Destino::Descarta)?;
        self.bucles.pop();
        let buf = self.sale();
        self.emite("{".to_string());
        for l in buf {
            self.emite(format!("    {l}"));
        }
        self.emite("}".to_string());
        self.emite(format!("goto sig{id};"));
        self.emite(format!("fin{id}: ;"));
        Ok(ExVal::puro("0".into(), CType::Vacio))
    }

    /// Base de `para` cuando ya es lugar (el llamador hoistea el resto).
    fn sitio_base_for(v: &ExVal) -> Result<String, CError> {
        if v.movible.is_some() || v.lugar.is_some() {
            Ok(v.c.clone())
        } else {
            Ok(String::new())
        }
    }

    /// `para i en a..b` (contador ancho; `..=` con pre/post-chequeo).
    fn ex_for_rango(
        &mut self,
        x: &syn::ExprForLoop,
        r: &syn::ExprRange,
        etiqueta: Option<String>,
        id: usize,
    ) -> Result<ExVal, CError> {
        let cerrado = matches!(r.limits, syn::RangeLimits::Closed(_));
        let inicio_e = match &r.start {
            Some(e) => e,
            None => {
                return Err(self.err_en("C0005", "el rango necesita inicio".into(), r)
                    .ayuda("escribe `0..n`"))
            }
        };
        let vi = self.baja_expr(inicio_e, None)?;
        let t_elem = vi.tipo.clone();
        // Contador ancho (sin vueltas infinitas por desborde).
        let t_cont = match &t_elem {
            CType::I8 | CType::I16 | CType::I32 | CType::I64 | CType::Isize => CType::I64,
            CType::U8 | CType::U16 | CType::U32 | CType::U64 | CType::Usize => CType::U64,
            CType::Char => CType::U32,
            _ => {
                return Err(self.err_en(
                    "C0005",
                    format!("rangos solo en números, llegó `{}`", self.muestra(&t_elem)),
                    inicio_e,
                ))
            }
        };
        let ci = self.consume(vi, None)?;
        let spell_c = t_cont.deletrea().map_err(|e| self.con_archivo(e))?;
        let tc = self.temp(t_cont.clone())?;
        self.emite(format!("{tc} = (({spell_c})({ci}));"));
        self.marca_movida_si_temp_pub(&ci);
        // Fin (una sola vez; `a..` infinito).
        let tfinal: Option<String> = match &r.end {
            Some(e) => {
                let ve = self.baja_expr(e, Some(&t_elem))?;
                let ce = self.consume(ve, Some(&t_elem))?;
                let th = self.temp(t_cont.clone())?;
                self.emite(format!("{th} = (({spell_c})({ce}));"));
                self.marca_movida_si_temp_pub(&ce);
                Some(th)
            }
            None => None,
        };
        if cerrado && tfinal.is_none() {
            return Err(self.err_en("C0005", "`..=` necesita fin".into(), r));
        }
        if tfinal.is_none() && !cerrado {
            // `a..` infinito: el cuerpo decide (`romper`).
        }
        if cerrado {
            let th = tfinal.clone().unwrap();
            self.emite(format!("if ((({tc}) > ({th}))) goto fin{id};"));
        }
        self.emite(format!("goto cond{id};"));
        self.emite(format!("sig{id}: ++({tc});"));
        self.emite(format!("cond{id}: ;"));
        match (&tfinal, cerrado) {
            (Some(th), false) => {
                self.emite(format!("if (!((({tc}) < ({th})))) goto fin{id};"));
            }
            (None, false) => {}
            (Some(_), true) => {}
            (None, true) => unreachable!(),
        }
        self.entra();
        let prof = self.pila.len() - 1;
        self.bucles.push(InfoBucle { etiqueta, id, prof_cuerpo: prof, valor: None });
        // Enlace por vuelta (conversión si el contador es más ancho).
        let sitio = if t_cont == t_elem {
            format!("({tc})")
        } else {
            let spell_e = t_elem.deletrea().map_err(|e| self.con_archivo(e))?;
            let te = self.temp(t_elem.clone())?;
            self.emite(format!("{te} = (({spell_e})({tc}));"));
            te
        };
        let (cond, enlaces) = self.analiza_patron(&x.pat, &t_elem, &sitio)?;
        if cond != "true" {
            self.bucles.pop();
            return Err(self.err_en(
                "C0003",
                "ese patrón puede fallar en el `para`".into(),
                &x.pat,
            ));
        }
        self.emite_enlaces(&enlaces)?;
        self.baja_bloque_contenido(&x.body, &Destino::Descarta)?;
        self.bucles.pop();
        let buf = self.sale();
        self.emite("{".to_string());
        for l in buf {
            self.emite(format!("    {l}"));
        }
        self.emite("}".to_string());
        if cerrado {
            let th = tfinal.unwrap();
            self.emite(format!("if ((({tc}) == ({th}))) goto fin{id};"));
        }
        self.emite(format!("goto sig{id};"));
        self.emite(format!("fin{id}: ;"));
        Ok(ExVal::puro("0".into(), CType::Vacio))
    }

    /// `mientras cond { }` / `mientras sea` (condición reevaluada por vuelta).
    fn ex_while(&mut self, x: &syn::ExprWhile) -> Result<ExVal, CError> {
        let etiqueta = x.label.as_ref().map(|l| tipos::desnuda(&l.name.ident));
        let id = self.etiqueta_n;
        self.etiqueta_n += 1;
        self.emite(format!("goto cond{id};"));
        self.emite(format!("sig{id}: ;"));
        self.emite(format!("cond{id}: ;"));
        self.entra();
        let prof = self.pila.len() - 1;
        self.bucles.push(InfoBucle { etiqueta, id, prof_cuerpo: prof, valor: None });
        match &*x.cond {
            syn::Expr::Let(l) => {
                let v = self.baja_expr(&l.expr, None)?;
                let t_scrut = v.tipo.clone();
                let ts = if v.movible.is_some() || v.lugar.is_some() {
                    v.c.clone()
                } else if let CType::Arreglo(elem, largo) = t_scrut.clone() {
                    let tmp = self.temp(t_scrut.clone())?;
                    self.materializa_arreglo(&tmp, v, &elem, &largo, false)?;
                    tmp
                } else {
                    let tmp = self.temp(t_scrut.clone())?;
                    let c = self.consume(v, Some(&t_scrut))?;
                    self.emite(format!("{tmp} = ({c});"));
                    self.marca_movida_si_temp_pub(&c);
                    tmp
                };
                let (cond, enlaces) = self.analiza_patron(&l.pat, &t_scrut, &ts)?;
                if cond == "true" {
                    // Irrefutable: aviso y vuelta infinita (el cuerpo decide).
                    self.emite("/* `mientras sea` irrefutable */".to_string());
                } else {
                    self.emite(format!("if (!({cond})) {{"));
                    let limp = self.limpieza_hasta(prof);
                    for l in limp {
                        self.emite(format!("    {l}"));
                    }
                    self.emite(format!("    goto fin{id};"));
                    self.emite("}".to_string());
                }
                self.emite_enlaces(&enlaces)?;
            }
            _ => {
                let v = self.baja_expr(&x.cond, Some(&CType::Bool))?;
                let c = self.consume(v, Some(&CType::Bool))?;
                self.emite(format!("if (!(({c}))) {{"));
                let limp = self.limpieza_hasta(prof);
                for l in limp {
                    self.emite(format!("    {l}"));
                }
                self.emite(format!("    goto fin{id};"));
                self.emite("}".to_string());
            }
        }
        self.baja_bloque_contenido(&x.body, &Destino::Descarta)?;
        self.bucles.pop();
        let buf = self.sale();
        self.emite("{".to_string());
        for l in buf {
            self.emite(format!("    {l}"));
        }
        self.emite("}".to_string());
        self.emite(format!("goto sig{id};"));
        self.emite(format!("fin{id}: ;"));
        Ok(ExVal::puro("0".into(), CType::Vacio))
    }

    /// `ciclo { }` (con valor si `esp` lo pide, vía `romper valor`).
    fn ex_loop(&mut self, x: &syn::ExprLoop, esp: Option<&CType>) -> Result<ExVal, CError> {
        let etiqueta = x.label.as_ref().map(|l| tipos::desnuda(&l.name.ident));
        let id = self.etiqueta_n;
        self.etiqueta_n += 1;
        let (dest, valor, ret) = match esp {
            None | Some(CType::Vacio) => (Destino::Descarta, None, None),
            Some(t) => {
                let tr = self.temp(t.clone())?;
                (Destino::Temp(tr.clone()), Some((tr.clone(), t.clone())), Some(tr))
            }
        };
        self.emite(format!("sig{id}: ;"));
        self.entra();
        let prof = self.pila.len() - 1;
        self.bucles.push(InfoBucle { etiqueta, id, prof_cuerpo: prof, valor });
        self.baja_bloque_contenido(&x.body, &dest)?;
        self.bucles.pop();
        let buf = self.sale();
        self.emite("{".to_string());
        for l in buf {
            self.emite(format!("    {l}"));
        }
        self.emite("}".to_string());
        self.emite(format!("goto sig{id};"));
        self.emite(format!("fin{id}: ;"));
        match ret {
            Some(tr) => {
                let t = esp.unwrap().clone();
                Ok(ExVal::movible(tr.clone(), t, tr))
            }
            None => Ok(ExVal::puro("0".into(), CType::Vacio)),
        }
    }
}
