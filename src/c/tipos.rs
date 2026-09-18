//! Tipos: de `syn::Type` (Rust) a `CType` (C).
//!
//! Mapeo:
//! - `i32` → `int32_t`, … `usize` → `size_t`, `f64` → `double`.
//! - `bool` → `bool`, `char` → `KamiChar` (escalar unicode, `uint32_t`).
//! - `String` → `KamiTexto` (dueño, hay que liberar).
//! - `&str` → `const char *` (prestado, siempre NUL-terminado).
//! - `Vec<T>` → `KamiVec_T` (monomorfizado, dueño).
//! - `Option<T>` / `Result<T, E>` → struct con bandera (+ `union`).
//! - `[T; N]` → arreglo C de verdad (`T x[N]`).
//! - `&T` / `&mut T` → `const T*` / `T*`.
//! - `&[T]` → vista `KamiVista_T` (puntero + largo).
//! - structs/enums de usuario → `typedef` con el mismo nombre.
//! - `dyn Rasgo` → `Rasgo__dyn` (puntero gordo: vtabla + objeto).

use std::collections::{HashMap, HashSet};

use syn::spanned::Spanned;

use super::errores::CError;

/// Tipo ya entendido por el backend C.
#[derive(Clone, Debug, PartialEq)]
pub enum CType {
    Vacio,
    Bool,
    Char,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    Isize,
    Usize,
    F32,
    F64,
    /// `String`: dueño.
    Texto,
    /// `&str`: `const char *` con NUL.
    VistaTexto,
    Vec(Box<CType>),
    Opcion(Box<CType>),
    Resultado(Box<CType>, Box<CType>),
    Tupla(Vec<CType>),
    Arreglo(Box<CType>, LargoArreglo),
    /// `&[T]` / `&mut [T]`: vista prestada. El bool es mutabilidad.
    Rebana(Box<CType>, bool),
    /// `&T` (false) / `&mut T` (true).
    Ref(bool, Box<CType>),
    /// `*const T` (false) / `*mut T` (true).
    Ptr(bool, Box<CType>),
    /// `Box<T>`: `T*` dueño.
    Caja(Box<CType>),
    /// Struct / enum / unión de usuario (nombre C final).
    Usuario(String),
    /// `dyn Rasgo`: nombre del rasgo (se deletrea `Rasgo__dyn`).
    Dyn(String),
    /// `fn(A, B) -> R`.
    FnPtr {
        params: Vec<CType>,
        ret: Box<CType>,
    },
    /// `_`: se resuelve con el tipo esperado o se pide anotación.
    Infer,
}

/// Longitud de `[T; N]`.
#[derive(Clone, Debug, PartialEq)]
pub enum LargoArreglo {
    /// Número literal.
    Lit(usize),
    /// Constante (`N`): se deletrea tal cual (la const se emite como `#define`).
    Expr(String),
}

impl LargoArreglo {
    pub fn deletrea(&self) -> String {
        match self {
            LargoArreglo::Lit(n) => n.to_string(),
            LargoArreglo::Expr(s) => s.clone(),
        }
    }
}

/// Contexto para resolver tipos.
pub struct CtxTipos<'a> {
    /// Pila de módulos actual (`["a", "b"]` dentro de `mod a::b`).
    pub mods: &'a [String],
    /// Nombres C de tipos de usuario + alias conocidos.
    pub propios: &'a HashSet<String>,
    /// Alias resueltos.
    pub alias: &'a HashMap<String, CType>,
    /// Tipo del `Self` actual (dentro de un `impl`), si hay.
    pub tipo_self: Option<&'a str>,
}

/// Quita `r#` / `r##` de identificadores crudos.
pub fn desnuda(id: &syn::Ident) -> String {
    let s = id.to_string();
    s.strip_prefix("r##")
        .or_else(|| s.strip_prefix("r#"))
        .unwrap_or(&s)
        .to_string()
}

/// Palabras reservadas de C (+ POSIX común). Si un ident choca, se le pega `_`.
pub fn es_palabra_c(s: &str) -> bool {
    matches!(
        s,
        "auto" | "break" | "case" | "char" | "const" | "continue" | "default" | "do"
            | "double" | "else" | "enum" | "extern" | "float" | "for" | "goto" | "if"
            | "inline" | "int" | "long" | "register" | "restrict" | "return" | "short"
            | "signed" | "sizeof" | "static" | "struct" | "switch" | "typedef" | "union"
            | "unsigned" | "void" | "volatile" | "while" | "_Alignas" | "_Alignof"
            | "_Atomic" | "_Bool" | "_Complex" | "_Generic" | "_Imaginary" | "_Noreturn"
            | "_Static_assert" | "_Thread_local" | "asm" | "typeof" | "true" | "false"
            | "NULL" | "main" | "stdin" | "stdout" | "stderr" | "errno"
    )
}

/// Identificador seguro para C.
pub fn higieniza(s: &str) -> String {
    if es_palabra_c(s) || s.starts_with("kami_") || s.starts_with("Kami") || s.starts_with("__") {
        format!("{s}_")
    } else {
        s.to_string()
    }
}

/// Baja un `syn::Type` a `CType`.
pub fn baja_tipo(
    ty: &syn::Type,
    ctx: &CtxTipos<'_>,
    lineas: &[String],
) -> Result<CType, CError> {
    let err = |c: &'static str, m: String| CError::nuevo(c, m).con_span(ty, lineas);
    match ty {
        syn::Type::Path(p) => baja_path(&p.path, p.qself.is_some(), ctx, lineas),
        syn::Type::Reference(r) => {
            // `&[T]` / `&mut [T]`: vista.
            if let syn::Type::Slice(s) = &*r.elem {
                let elem = baja_tipo(&s.elem, ctx, lineas)?;
                return Ok(CType::Rebana(elem.into(), r.mutability.is_some()));
            }
            // `&str`: vista de texto (`str` pelado no tiene tamaño).
            if let syn::Type::Path(p) = &*r.elem {
                if p.qself.is_none() && p.path.is_ident("str") {
                    if r.mutability.is_some() {
                        return Err(err("C0003", "&mut str no cabe en C (usa Texto)".into())
                            .ayuda("cambia &mut str por Texto y trabaja con métodos que devuelven dueño"));
                    }
                    return Ok(CType::VistaTexto);
                }
            }
            let interna = baja_tipo(&r.elem, ctx, lineas)?;
            if matches!(interna, CType::FnPtr { .. }) {
                return Err(err("C0003", "`&fn` no cabe; `fn` pelado ya es puntero".into()));
            }
            if r.mutability.is_some() {
                match interna {
                    CType::VistaTexto => {
                        return Err(err(
                            "C0003",
                            "&mut str no cabe en C (usa Texto)".into(),
                        )
                        .ayuda("cambia &mut str por Texto y trabaja con métodos que devuelven dueño"));
                    }
                    _ => Ok(CType::Ref(true, Box::new(interna))),
                }
            } else {
                match interna {
                    CType::VistaTexto => Ok(CType::VistaTexto),
                    _ => Ok(CType::Ref(false, Box::new(interna))),
                }
            }
        }
        syn::Type::Ptr(p) => {
            if matches!(&*p.elem, syn::Type::Slice(_)) {
                return Err(err("C0003", "puntero crudo a [T] no cabe; usa &[...]".into()));
            }
            let interna = baja_tipo(&p.elem, ctx, lineas)?;
            match interna {
                CType::VistaTexto => Err(err("C0003", "puntero crudo a str no cabe".into())),
                CType::FnPtr { .. } => Err(err(
                    "C0003",
                    "puntero a `fn` no cabe; `fn` pelado ya es puntero".into(),
                )),
                _ => Ok(CType::Ptr(
                    p.mutability.is_some(),
                    Box::new(interna),
                )),
            }
        }
        syn::Type::Array(a) => {
            let elem = baja_tipo(&a.elem, ctx, lineas)?;
            let largo = baja_largo(&a.len, lineas)?;
            Ok(CType::Arreglo(Box::new(elem), largo))
        }
        syn::Type::Slice(_) => Err(err(
            "C0003",
            "[T] pelado no cabe; usa &[...] (vista) o Lista<T> (dueña)".into(),
        )),
        syn::Type::Tuple(t) if t.elems.is_empty() => Ok(CType::Vacio),
        syn::Type::Tuple(t) => {
            let mut v = Vec::new();
            for e in &t.elems {
                v.push(baja_tipo(e, ctx, lineas)?);
            }
            Ok(CType::Tupla(v))
        }
        syn::Type::Paren(p) => baja_tipo(&p.elem, ctx, lineas),
        syn::Type::Group(g) => baja_tipo(&g.elem, ctx, lineas),
        syn::Type::Never(_) => Ok(CType::Vacio),
        syn::Type::Infer(_) => Ok(CType::Infer),
        syn::Type::TraitObject(o) => baja_dyn(o, lineas),
        syn::Type::ImplTrait(i) => Err(err(
            "C0003",
            "impl Rasgo no cabe en C (es genérico)".into(),
        )
        .con_span(&i.impl_token, lineas)
        .ayuda("usa un tipo concreto, una caja `Box<T>` o un objeto `&din Rasgo`")),
        syn::Type::BareFn(f) => {
            if f.unsafety.is_some() {
                return Err(err("C0003", "fn inseguro como tipo no se soporta".into()));
            }
            let mut params = Vec::new();
            for a in &f.inputs {
                params.push(baja_tipo(&a.ty, ctx, lineas)?);
            }
            let ret = match &f.output {
                syn::ReturnType::Default => CType::Vacio,
                syn::ReturnType::Type(_, t) => baja_tipo(t, ctx, lineas)?,
            };
            Ok(CType::FnPtr {
                params,
                ret: Box::new(ret),
            })
        }
        syn::Type::Macro(m) => Err(err(
            "C0003",
            "tipo con macro no soportado".into(),
        )
        .con_span(&m.mac.path, lineas)),
        other => Err(err("C0003", "tipo no soportado por el backend C".into())
            .con_span(other, lineas)),
    }
}

fn baja_largo(e: &syn::Expr, lineas: &[String]) -> Result<LargoArreglo, CError> {
    match e {
        syn::Expr::Lit(l) => match &l.lit {
            syn::Lit::Int(i) => {
                let n: usize = i
                    .base10_parse()
                    .map_err(|_| {
                        CError::nuevo("C0003", "longitud de arreglo inválida".to_string())
                            .con_span(i, lineas)
                    })?;
                Ok(LargoArreglo::Lit(n))
            }
            _ => Err(CError::nuevo(
                "C0003",
                "la longitud del arreglo debe ser un número o una constante".to_string(),
            )
            .con_span(l, lineas)),
        },
        syn::Expr::Path(p) if p.qself.is_none() => Ok(LargoArreglo::Expr(
            p.path.segments.iter().map(|s| s.ident.to_string()).collect::<Vec<_>>().join("__"),
        )),
        _ => Err(CError::nuevo(
            "C0003",
            "la longitud del arreglo debe ser un número o una `constante`".to_string(),
        )
        .con_span(e, lineas)
        .ayuda("en C el tamaño es fijo: usa `[T; 10]` o `[T; N]` con `constante N: usize = 10;`")),
    }
}

fn baja_dyn(o: &syn::TypeTraitObject, lineas: &[String]) -> Result<CType, CError> {
    let mut rasgos = Vec::new();
    for b in &o.bounds {
        match b {
            syn::TypeParamBound::Trait(t) if t.lifetimes.is_none() => {
                rasgos.push(t.path.clone());
            }
            syn::TypeParamBound::Trait(_) => {
                return Err(CError::nuevo(
                    "C0003",
                    "dyn con lifetimes en bounds no soportado".to_string(),
                )
                .con_span(b, lineas))
            }
            syn::TypeParamBound::Lifetime(_) => {}
            _ => {
                return Err(CError::nuevo(
                    "C0003",
                    "bound no soportado en dyn".to_string(),
                )
                .con_span(b, lineas))
            }
        }
    }
    if rasgos.len() != 1 {
        return Err(CError::nuevo(
            "C0003",
            "usa un solo rasgo por objeto `din` (p.ej. `&din Dibuja`)".to_string(),
        )
        .con_span(o, lineas));
    }
    let p = &rasgos[0];
    if p.segments.len() != 1 || !matches!(p.segments[0].arguments, syn::PathArguments::None) {
        return Err(CError::nuevo(
            "C0003",
            "el rasgo del objeto `din` debe ser simple (sin genéricos ni rutas)".to_string(),
        )
        .con_span(p, lineas));
    }
    Ok(CType::Dyn(desnuda(&p.segments[0].ident)))
}

/// Resuelve un path de tipo a nombre C + chequea genéricos.
fn baja_path(
    path: &syn::Path,
    hay_qself: bool,
    ctx: &CtxTipos<'_>,
    lineas: &[String],
) -> Result<CType, CError> {
    if hay_qself {
        return Err(CError::nuevo(
            "C0003",
            "tipos calificados `<T como R>::X` no soportados".to_string(),
        )
        .con_span(path, lineas)
        .ayuda("nombra el tipo concreto directamente"));
    }
    let segs: Vec<String> = path.segments.iter().map(|s| desnuda(&s.ident)).collect();
    if segs.is_empty() {
        return Err(CError::nuevo("C0002", "tipo vacío".to_string()).con_span(path, lineas));
    }

    // Rutas crate:: / self:: / super:: / ::externa
    let ruta_mangleada = if segs[0] == "crate" || segs[0] == "self" || segs[0] == "super" {
        let mut base: Vec<String> = ctx.mods.to_vec();
        let mut resto = &segs[..];
        while !resto.is_empty() {
            match resto[0].as_str() {
                "crate" => {
                    base.clear();
                    resto = &resto[1..];
                    break;
                }
                "self" => {
                    resto = &resto[1..];
                    break;
                }
                "super" => {
                    base.pop();
                    resto = &resto[1..];
                }
                _ => break,
            }
        }
        let mut todo = base;
        todo.extend(resto.iter().cloned());
        todo.join("__")
    } else if path.leading_colon.is_some() {
        segs.join("__")
    } else if segs.len() == 1 {
        // Un solo nombre: prueba módulo actual hacia afuera.
        let mut cands = Vec::new();
        for k in (0..=ctx.mods.len()).rev() {
            let mut v: Vec<String> = ctx.mods[..k].to_vec();
            v.push(segs[0].clone());
            cands.push(v.join("__"));
        }
        cands
            .into_iter()
            .find(|c| ctx.propios.contains(c))
            .unwrap_or_else(|| segs[0].clone())
    } else {
        // `a::B`: prueba con prefijo del módulo actual y luego pelado.
        let mut cands = Vec::new();
        for k in (0..=ctx.mods.len()).rev() {
            let mut v: Vec<String> = ctx.mods[..k].to_vec();
            v.extend(segs.clone());
            cands.push(v.join("__"));
        }
        cands
            .into_iter()
            .find(|c| ctx.propios.contains(c))
            .unwrap_or_else(|| segs.join("__"))
    };

    let ultimo = path.segments.last().unwrap();
    let args_tipos = match &ultimo.arguments {
        syn::PathArguments::None => Vec::new(),
        syn::PathArguments::AngleBracketed(a) => {
            let mut v = Vec::new();
            for g in &a.args {
                match g {
                    syn::GenericArgument::Type(t) => v.push(baja_tipo(t, ctx, lineas)?),
                    syn::GenericArgument::Lifetime(_) => {}
                    _ => {
                        return Err(CError::nuevo(
                            "C0003",
                            "argumentos const en tipos no soportados".to_string(),
                        )
                        .con_span(g, lineas))
                    }
                }
            }
            v
        }
        syn::PathArguments::Parenthesized(_) => {
            return Err(CError::nuevo(
                "C0003",
                "tipos `Fn(...)` con paréntesis no soportados (usa `fn(A) -> B`)".to_string(),
            )
            .con_span(path, lineas))
        }
    };

    let nombre = desnuda(&ultimo.ident);
    // Primitivos y bendecidos de std (por último segmento).
    let primitivo: Option<CType> = match nombre.as_str() {
        "bool" => Some(CType::Bool),
        "char" => Some(CType::Char),
        "i8" => Some(CType::I8),
        "i16" => Some(CType::I16),
        "i32" => Some(CType::I32),
        "i64" => Some(CType::I64),
        "u8" => Some(CType::U8),
        "u16" => Some(CType::U16),
        "u32" => Some(CType::U32),
        "u64" => Some(CType::U64),
        "isize" => Some(CType::Isize),
        "usize" => Some(CType::Usize),
        "f32" => Some(CType::F32),
        "f64" => Some(CType::F64),
        "str" => {
            return Err(CError::nuevo(
                "C0003",
                "str sin tamaño no cabe; usa &str (prestado) o Texto (dueño)".to_string(),
            )
            .con_span(path, lineas))
        }
        "String" => {
            exige_args(&nombre, &args_tipos, 0, path, lineas)?;
            // ¿El usuario definió su propio `String`? Gana el usuario.
            if ctx.propios.contains(&ruta_mangleada) && ruta_mangleada != "String" {
                None
            } else {
                Some(CType::Texto)
            }
        }
        "Vec" => {
            exige_args(&nombre, &args_tipos, 1, path, lineas)?;
            if ctx.propios.contains(&ruta_mangleada) && ruta_mangleada != "Vec" {
                None
            } else {
                Some(CType::Vec(Box::new(args_tipos[0].clone())))
            }
        }
        "Option" => {
            exige_args(&nombre, &args_tipos, 1, path, lineas)?;
            Some(CType::Opcion(Box::new(args_tipos[0].clone())))
        }
        "Result" => {
            exige_args(&nombre, &args_tipos, 2, path, lineas)?;
            Some(CType::Resultado(
                Box::new(args_tipos[0].clone()),
                Box::new(args_tipos[1].clone()),
            ))
        }
        "Box" => {
            exige_args(&nombre, &args_tipos, 1, path, lineas)?;
            if ctx.propios.contains(&ruta_mangleada) && ruta_mangleada != "Box" {
                None
            } else {
                Some(CType::Caja(Box::new(args_tipos[0].clone())))
            }
        }
        "Self" => match ctx.tipo_self {
            Some(s) => Some(CType::Usuario(s.to_string())),
            None => {
                return Err(CError::nuevo(
                    "C0002",
                    "Yo fuera de un `implementa`".to_string(),
                )
                .con_span(path, lineas))
            }
        },
        "PhantomData" => Some(CType::U8),
        _ => None,
    };
    if let Some(t) = primitivo {
        return Ok(t);
    }

    // Colecciones no soportadas → error con alternativa.
    if let Some(alt) = alternativa_std(&nombre) {
        return Err(CError::nuevo(
            "C0003",
            format!("`{nombre}` no tiene equivalente directo en el backend C"),
        )
        .con_span(path, lineas)
        .ayuda(alt));
    }

    // Genéricos en tipos de usuario: no.
    if !args_tipos.is_empty() {
        return Err(CError::nuevo(
            "C0003",
            format!("`{nombre}` con genéricos no cabe en C (sin monomorfización general)"),
        )
        .con_span(path, lineas)
        .ayuda("usa tipos concretos, `Lista<T>`/`Opcion<T>` (sí soportados) o `&din Rasgo`"));
    }

    // Alias.
    if let Some(t) = ctx.alias.get(&ruta_mangleada) {
        return Ok(t.clone());
    }
    if ctx.propios.contains(&ruta_mangleada) {
        return Ok(CType::Usuario(ruta_mangleada));
    }
    Err(CError::nuevo(
        "C0002",
        format!("tipo desconocido `{ruta_mangleada}`"),
    )
    .con_span(path, lineas)
    .ayuda("revísalo con `kamisolari correr`; si es un rasgo como tipo, escribe `&din Rasgo`"))
}

fn exige_args(
    nombre: &str,
    args: &[CType],
    n: usize,
    path: &syn::Path,
    lineas: &[String],
) -> Result<(), CError> {
    if args.len() != n {
        return Err(CError::nuevo(
            "C0002",
            format!("`{nombre}` lleva {n} parámetros de tipo"),
        )
        .con_span(path, lineas));
    }
    Ok(())
}

fn alternativa_std(nombre: &str) -> Option<String> {
    match nombre {
        "HashMap" | "HashSet" | "BTreeMap" | "BTreeSet" => Some(
            "el backend C no trae tablas hash: usa una `Lista` de tuplas + búsqueda lineal".to_string(),
        ),
        "VecDeque" | "LinkedList" | "BinaryHeap" => Some(
            "usa `Lista<T>` (Vec) con empuja/saca, que sí está soportada".to_string(),
        ),
        "Rc" | "Arc" | "RefCell" | "Cell" | "Mutex" | "RwLock" => Some(
            "punteros compartidos no soportados: reestructura con préstamos `&T`/`&mut T` o duplica con `.clona()`".to_string(),
        ),
        "String" | "str" => None,
        "Path" | "PathBuf" | "File" | "Instant" | "Duration" | "SystemTime" => Some(
            "usa tipos simples (`Texto`, números) o declara la función C con `externo`".to_string(),
        ),
        "From" | "Into" | "FromStr" | "Display" | "Debug" | "Clone" | "Copy" | "Default"
        | "PartialEq" | "Eq" | "PartialOrd" | "Ord" | "Hash" | "Iterator" | "FromIterator"
        | "AsRef" | "AsMut" | "Borrow" | "ToOwned" | "Deref" | "Drop" | "Sized" => Some(
            "ese rasgo de std no se usa como tipo; el backend genera `Clone`/`PartialEq`/`Default`/`Debug` bajo demanda y soporta `Display` con `escribe!`".to_string(),
        ),
        _ => None,
    }
}

/// Deletreo de `&T`, `*T`, `Box<T>` manejando arreglos (`E (*)[N]`).
fn deletrea_puntero(t: &CType, es_const: bool) -> Result<String, CError> {
    if let CType::Arreglo(e, l) = t {
        let c = if es_const { "const " } else { "" };
        return Ok(format!("{c}{} (*)[{}]", e.deletrea()?, l.deletrea()));
    }
    if matches!(t, CType::FnPtr { .. }) {
        return Err(CError::nuevo(
            "C0003",
            "puntero a `fn` no cabe; `fn` pelado ya es puntero".to_string(),
        ));
    }
    if es_const {
        Ok(format!("const {} *", t.deletrea()?))
    } else {
        Ok(format!("{} *", t.deletrea()?))
    }
}

impl CType {
    /// ¿Es `()`?
    pub fn es_vacio(&self) -> bool {
        matches!(self, CType::Vacio)
    }

    /// Nombre C completo para monos y tipos con nombre.
    /// (Para primitivos devuelve el deletreo corto usado como sufijo.)
    pub fn mangle(&self) -> Result<String, String> {
        match self {
            CType::Vacio => Err("`()` no tiene nombre C".into()),
            CType::Bool => Ok("bool".into()),
            CType::Char => Ok("KamiChar".into()),
            CType::I8 => Ok("i8".into()),
            CType::I16 => Ok("i16".into()),
            CType::I32 => Ok("i32".into()),
            CType::I64 => Ok("i64".into()),
            CType::U8 => Ok("u8".into()),
            CType::U16 => Ok("u16".into()),
            CType::U32 => Ok("u32".into()),
            CType::U64 => Ok("u64".into()),
            CType::Isize => Ok("isize".into()),
            CType::Usize => Ok("usize".into()),
            CType::F32 => Ok("f32".into()),
            CType::F64 => Ok("f64".into()),
            CType::Texto => Ok("KamiTexto".into()),
            CType::VistaTexto => Ok("cstr".into()),
            CType::Vec(t) => Ok(format!("KamiVec_{}", t.mangle()?)),
            CType::Opcion(t) => Ok(format!("KamiOpcion_{}", t.mangle()?)),
            CType::Resultado(a, b) => Ok(format!("KamiResultado_{}_{}", a.mangle()?, b.mangle()?)),
            CType::Tupla(v) => {
                if v.is_empty() {
                    return Err("tupla vacía sin nombre".into());
                }
                let partes: Vec<String> =
                    v.iter().map(|t| t.mangle()).collect::<Result<_, _>>()?;
                Ok(format!("KamiTupla_{}", partes.join("_")))
            }
            CType::Arreglo(t, l) => {
                let ls = l.deletrea();
                let limpio: String =
                    ls.chars().map(|c| if c.is_alphanumeric() { c } else { '_' }).collect();
                Ok(format!("KamiArreglo_{}_{}", t.mangle()?, limpio))
            }
            CType::Rebana(t, m) => Ok(format!(
                "KamiVista{}_{}",
                if *m { "Mut" } else { "" },
                t.mangle()?
            )),
            CType::Ref(false, t) => Ok(format!("ref_{}", t.mangle()?)),
            CType::Ref(true, t) => Ok(format!("mut_{}", t.mangle()?)),
            CType::Ptr(false, t) => Ok(format!("ptr_{}", t.mangle()?)),
            CType::Ptr(true, t) => Ok(format!("ptrmut_{}", t.mangle()?)),
            CType::Caja(t) => Ok(format!("caja_{}", t.mangle()?)),
            CType::Usuario(n) => Ok(n.clone()),
            CType::Dyn(r) => Ok(format!("{r}__dyn")),
            CType::FnPtr { params, ret } => {
                let mut s = String::from("KamiFn");
                for p in params {
                    s.push('_');
                    s.push_str(&p.mangle()?);
                }
                s.push_str("__ret_");
                s.push_str(&if ret.es_vacio() { "vacio".into() } else { ret.mangle()? });
                Ok(s)
            }
            CType::Infer => Err("tipo `_` sin resolver (anota el tipo)".into()),
        }
    }

    /// Deletreo C sin declarador. Falla en arreglos y fn-ptrs (necesitan nombre).
    pub fn deletrea(&self) -> Result<String, CError> {
        match self {
            CType::Vacio => Ok("void".into()),
            CType::Bool => Ok("bool".into()),
            CType::Char => Ok("KamiChar".into()),
            CType::I8 => Ok("int8_t".into()),
            CType::I16 => Ok("int16_t".into()),
            CType::I32 => Ok("int32_t".into()),
            CType::I64 => Ok("int64_t".into()),
            CType::U8 => Ok("uint8_t".into()),
            CType::U16 => Ok("uint16_t".into()),
            CType::U32 => Ok("uint32_t".into()),
            CType::U64 => Ok("uint64_t".into()),
            CType::Isize => Ok("ptrdiff_t".into()),
            CType::Usize => Ok("size_t".into()),
            CType::F32 => Ok("float".into()),
            CType::F64 => Ok("double".into()),
            CType::Texto => Ok("KamiTexto".into()),
            CType::VistaTexto => Ok("const char *".into()),
            CType::Vec(_) | CType::Opcion(_) | CType::Resultado(_, _) | CType::Tupla(_) => {
                self.mangle().map_err(|m| CError::nuevo("C0002", m))
            }
            CType::Arreglo(_, _) => Err(CError::nuevo(
                "C0002",
                "arreglo sin declarador (bug interno)".to_string(),
            )),
            CType::Rebana(_, _) => self.mangle().map_err(|m| CError::nuevo("C0002", m)),
            CType::Ref(false, t) => deletrea_puntero(t, true),
            CType::Ref(true, t) => deletrea_puntero(t, false),
            CType::Ptr(false, t) => deletrea_puntero(t, true),
            CType::Ptr(true, t) => deletrea_puntero(t, false),
            CType::Caja(t) => deletrea_puntero(t, false),
            CType::Usuario(n) => Ok(n.clone()),
            CType::Dyn(r) => Ok(format!("{r}__dyn")),
            CType::FnPtr { .. } => Err(CError::nuevo(
                "C0002",
                "puntero a función sin declarador (bug interno)".to_string(),
            )),
            CType::Infer => Err(CError::nuevo(
                "C0005",
                "no pude inferir el tipo; anótalo (`sea x: Tipo = ...`)".to_string(),
            )),
        }
    }

    /// Declaración completa: `declara(i32, "x")` → `"int32_t x"`.
    /// Maneja arreglos (`int32_t x[3]`) y fn-ptrs (`int (*f)(double)`).
    pub fn declara(&self, nombre: &str) -> Result<String, CError> {
        match self {
            CType::Arreglo(t, l) => Ok(format!("{} {}[{}]", t.deletrea()?, nombre, l.deletrea())),
            CType::FnPtr { params, ret } => {
                let ps: Vec<String> =
                    params.iter().map(|p| p.deletrea()).collect::<Result<_, _>>()?;
                let ps = if ps.is_empty() { "void".to_string() } else { ps.join(", ") };
                Ok(format!("{} (*{})({})", ret.deletrea()?, nombre, ps))
            }
            _ => {
                let b = self.deletrea()?;
                if b.ends_with('*') {
                    Ok(format!("{b}{nombre}"))
                } else {
                    Ok(format!("{b} {nombre}"))
                }
            }
        }
    }

    /// Cómo se escribe este tipo en Rust (para comentarios; aproximado).
    pub fn a_rust(&self) -> String {
        match self {
            CType::Vacio => "()".into(),
            CType::Bool => "bool".into(),
            CType::Char => "char".into(),
            CType::I8 => "i8".into(),
            CType::I16 => "i16".into(),
            CType::I32 => "i32".into(),
            CType::I64 => "i64".into(),
            CType::U8 => "u8".into(),
            CType::U16 => "u16".into(),
            CType::U32 => "u32".into(),
            CType::U64 => "u64".into(),
            CType::Isize => "isize".into(),
            CType::Usize => "usize".into(),
            CType::F32 => "f32".into(),
            CType::F64 => "f64".into(),
            CType::Texto => "String".into(),
            CType::VistaTexto => "&str".into(),
            CType::Vec(t) => format!("Vec<{}>", t.a_rust()),
            CType::Opcion(t) => format!("Option<{}>", t.a_rust()),
            CType::Resultado(a, b) => format!("Result<{}, {}>", a.a_rust(), b.a_rust()),
            CType::Tupla(v) => {
                let xs: Vec<String> = v.iter().map(|t| t.a_rust()).collect();
                format!("({})", xs.join(", "))
            }
            CType::Arreglo(t, l) => format!("[{}; {}]", t.a_rust(), l.deletrea()),
            CType::Rebana(t, m) => {
                if *m {
                    format!("&mut [{}]", t.a_rust())
                } else {
                    format!("&[{}]", t.a_rust())
                }
            }
            CType::Ref(m, t) => {
                if *m {
                    format!("&mut {}", t.a_rust())
                } else {
                    format!("&{}", t.a_rust())
                }
            }
            CType::Ptr(m, t) => {
                if *m {
                    format!("*mut {}", t.a_rust())
                } else {
                    format!("*const {}", t.a_rust())
                }
            }
            CType::Caja(t) => format!("Box<{}>", t.a_rust()),
            CType::Usuario(n) => n.clone(),
            CType::Dyn(r) => format!("dyn {r}"),
            CType::FnPtr { params, ret } => {
                let ps: Vec<String> = params.iter().map(|t| t.a_rust()).collect();
                format!("fn({}) -> {}", ps.join(", "), ret.a_rust())
            }
            CType::Infer => "_".into(),
        }
    }

    /// Base descriptiva para temporales (`String` → `texto`, `Persona` → `persona`).
    pub fn pista_temp(&self) -> String {
        match self {
            CType::Texto => "texto".into(),
            CType::VistaTexto => "vista".into(),
            CType::Char => "car".into(),
            CType::Bool => "cond".into(),
            CType::I8
            | CType::I16
            | CType::I32
            | CType::I64
            | CType::U8
            | CType::U16
            | CType::U32
            | CType::U64
            | CType::Isize
            | CType::Usize
            | CType::F32
            | CType::F64 => "num".into(),
            CType::Vec(_) => "vec".into(),
            CType::Opcion(_) => "opc".into(),
            CType::Resultado(_, _) => "res".into(),
            CType::Tupla(_) => "tup".into(),
            CType::Arreglo(_, _) => "arr".into(),
            CType::Rebana(_, _) => "reb".into(),
            CType::Ref(_, _) => "ref".into(),
            CType::Ptr(_, _) => "ptr".into(),
            CType::Caja(_) => "caja".into(),
            CType::Usuario(n) => n.to_lowercase(),
            CType::Dyn(_) => "dyn".into(),
            CType::FnPtr { .. } => "f".into(),
            CType::Vacio | CType::Infer => "t".into(),
        }
    }

    /// Valor cero para inicializar / ramas inalcanzables.
    pub fn cero(&self) -> Result<String, CError> {
        match self {
            CType::Vacio => Ok(String::new()),
            CType::Bool => Ok("false".into()),
            CType::Char
            | CType::I8
            | CType::I16
            | CType::I32
            | CType::I64
            | CType::U8
            | CType::U16
            | CType::U32
            | CType::U64
            | CType::Isize
            | CType::Usize => Ok("0".into()),
            CType::F32 => Ok("0.0f".into()),
            CType::F64 => Ok("0.0".into()),
            CType::VistaTexto | CType::Ref(_, _) | CType::Ptr(_, _) | CType::Caja(_) => {
                Ok("NULL".into())
            }
            CType::Texto
            | CType::Vec(_)
            | CType::Opcion(_)
            | CType::Resultado(_, _)
            | CType::Tupla(_)
            | CType::Arreglo(_, _)
            | CType::Rebana(_, _)
            | CType::Usuario(_)
            | CType::Dyn(_) => Ok("{0}".into()),
            CType::FnPtr { .. } => Ok("NULL".into()),
            CType::Infer => Err(CError::nuevo(
                "C0005",
                "no pude inferir el tipo; anótalo".to_string(),
            )),
        }
    }

    /// Cero para `return` (literal compuesto con tipo cuando hace falta).
    pub fn cero_ret(&self) -> Result<String, CError> {
        let c = self.cero()?;
        if c.starts_with('{') {
            Ok(format!("({}){}", self.deletrea()?, c))
        } else {
            Ok(c)
        }
    }

    /// ¿El valor es dueño de memoria que hay que liberar?
    /// `usuario` responde para tipos de usuario (con guardia anti-ciclos).
    pub fn necesita_drop(&self, usuario: &dyn Fn(&str) -> bool) -> bool {
        match self {
            CType::Texto | CType::Vec(_) | CType::Caja(_) => true,
            CType::Opcion(t) => t.necesita_drop(usuario),
            // `&[T]` es una vista prestada (puntero + largo): no es dueña.
            CType::Rebana(_, _) => false,
            CType::Resultado(a, b) => a.necesita_drop(usuario) || b.necesita_drop(usuario),
            CType::Tupla(v) => v.iter().any(|t| t.necesita_drop(usuario)),
            CType::Arreglo(t, _) => t.necesita_drop(usuario),
            CType::Usuario(n) => usuario(n),
            _ => false,
        }
    }

    /// ¿Es numérico entero?
    pub fn es_entero(&self) -> bool {
        matches!(
            self,
            CType::I8
                | CType::I16
                | CType::I32
                | CType::I64
                | CType::U8
                | CType::U16
                | CType::U32
                | CType::U64
                | CType::Isize
                | CType::Usize
                | CType::Char
        )
    }

    pub fn es_flotante(&self) -> bool {
        matches!(self, CType::F32 | CType::F64)
    }

    pub fn es_numero(&self) -> bool {
        self.es_entero() || self.es_flotante()
    }

    /// ¿Entero sin signo?
    pub fn es_sin_signo(&self) -> bool {
        matches!(
            self,
            CType::U8 | CType::U16 | CType::U32 | CType::U64 | CType::Usize
        )
    }
}

/// Escapa contenido para dentro de un literal C `"..."`.
/// Parte el literal (`"..." "..."`) cuando un `\xNN` quedaría pegado a un
/// dígito hexadecimal, para no cambiar su valor.
pub fn escapa_cadena_c(s: &str) -> String {
    let mut out = String::new();
    let chars: Vec<char> = s.chars().collect();
    for (i, c) in chars.iter().enumerate() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\t' => out.push_str("\\t"),
            '\r' => out.push_str("\\r"),
            c if (*c as u32) < 0x20 || (*c as u32) == 0x7f => {
                out.push_str(&format!("\\x{:02x}", *c as u32));
                if let Some(sig) = chars.get(i + 1) {
                    if sig.is_ascii_hexdigit() {
                        out.push_str("\" \"");
                    }
                }
            }
            c => out.push(*c),
        }
    }
    out
}

/// Escapa un `char` de Rust para un literal C `U'...'`.
pub fn escapa_char_c(c: char) -> String {
    match c {
        '\'' => "\\'".into(),
        '\\' => "\\\\".into(),
        '\n' => "\\n".into(),
        '\t' => "\\t".into(),
        '\r' => "\\r".into(),
        '\0' => "\\0".into(),
        c if (c as u32) < 0x20 || (c as u32) == 0x7f => format!("\\U{:08X}", c as u32),
        c => c.to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ctx<'a>(
        propios: &'a HashSet<String>,
        alias: &'a HashMap<String, CType>,
    ) -> CtxTipos<'a> {
        CtxTipos {
            mods: &[],
            propios,
            alias,
            tipo_self: None,
        }
    }

    fn parse_ty(s: &str) -> syn::Type {
        syn::parse_str(s).unwrap()
    }

    #[test]
    fn primitivos() {
        let p = HashSet::new();
        let a = HashMap::new();
        let c = ctx(&p, &a);
        let l = vec![];
        assert_eq!(baja_tipo(&parse_ty("i32"), &c, &l).unwrap(), CType::I32);
        assert_eq!(baja_tipo(&parse_ty("&str"), &c, &l).unwrap(), CType::VistaTexto);
        assert_eq!(baja_tipo(&parse_ty("String"), &c, &l).unwrap(), CType::Texto);
        assert_eq!(
            baja_tipo(&parse_ty("&mut i32"), &c, &l).unwrap(),
            CType::Ref(true, Box::new(CType::I32))
        );
        assert_eq!(
            baja_tipo(&parse_ty("Vec<String>"), &c, &l).unwrap(),
            CType::Vec(Box::new(CType::Texto))
        );
        assert_eq!(baja_tipo(&parse_ty("()"), &c, &l).unwrap(), CType::Vacio);
        assert_eq!(
            baja_tipo(&parse_ty("&[i32]"), &c, &l).unwrap(),
            CType::Rebana(Box::new(CType::I32), false)
        );
    }

    #[test]
    fn rust_y_pistas() {
        assert_eq!(CType::Texto.a_rust(), "String");
        assert_eq!(CType::VistaTexto.a_rust(), "&str");
        assert_eq!(
            CType::Ref(false, Box::new(CType::Usuario("Persona".into()))).a_rust(),
            "&Persona"
        );
        assert_eq!(CType::Vec(Box::new(CType::I32)).a_rust(), "Vec<i32>");
        assert_eq!(CType::Texto.pista_temp(), "texto");
        assert_eq!(CType::I32.pista_temp(), "num");
        assert_eq!(CType::Bool.pista_temp(), "cond");
        assert_eq!(CType::Usuario("Persona".into()).pista_temp(), "persona");
        assert_eq!(CType::VistaTexto.pista_temp(), "vista");
    }

    #[test]
    fn deletreos() {
        assert_eq!(CType::I32.deletrea().unwrap(), "int32_t");
        assert_eq!(
            CType::Ref(false, Box::new(CType::I32)).deletrea().unwrap(),
            "const int32_t *"
        );
        assert_eq!(
            CType::Arreglo(Box::new(CType::I32), LargoArreglo::Lit(3))
                .declara("a")
                .unwrap(),
            "int32_t a[3]"
        );
        assert_eq!(
            CType::Vec(Box::new(CType::I32)).deletrea().unwrap(),
            "KamiVec_i32"
        );
        assert_eq!(
            CType::Ref(false, Box::new(CType::Arreglo(
                Box::new(CType::I32),
                LargoArreglo::Lit(3)
            )))
            .deletrea()
            .unwrap(),
            "const int32_t (*)[3]"
        );
    }

    #[test]
    fn escapa_hex_pegado() {
        // \x01 seguido de '2' debe partir el literal.
        let e = escapa_cadena_c("\u{1}2");
        assert_eq!(e, "\\x01\" \"2");
    }

    #[test]
    fn drop_basico() {
        let f = |_: &str| false;
        assert!(!CType::I32.necesita_drop(&f));
        assert!(CType::Texto.necesita_drop(&f));
        assert!(!CType::Opcion(Box::new(CType::I32)).necesita_drop(&f));
        assert!(CType::Opcion(Box::new(CType::Texto)).necesita_drop(&f));
        assert!(!CType::VistaTexto.necesita_drop(&f));
    }
}
