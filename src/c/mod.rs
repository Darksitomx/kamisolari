//! Backend C: kamisolari (o Rust) → C11 legible.
//!
//! ```text
//! .kami --traduce--> Rust --syn--> AST --baja--> C (+ kami.h)
//! ```
//!
//! El backend confía en que el programa es válido: verifícalo primero con
//! `kamisolari correr`. Sobre esa base genera C con la misma semántica:
//! propiedad por ámbito (drops automáticos), movimientos con banderas y
//! chequeo de índices en arreglos/vectores.

mod ayuda;
mod baja;
pub mod errores;
mod expr;
mod formato;
mod pase1;
mod patron;
mod pulir;
mod stmt;
mod tipos;

pub use errores::CError;

use baja::Bajador;

/// Runtime C (`runtime/kami.h`) embebido para `--autonomo`.
pub const RUNTIME_KAMI_H: &str = include_str!("../../runtime/kami.h");

/// Opciones del backend C.
#[derive(Clone, Debug, Default)]
pub struct COpciones {
    /// Incluye el runtime dentro del .c (un solo archivo).
    pub autonomo: bool,
}

/// kamisolari → C. `nombre` es el archivo origen (para errores).
pub fn kami_a_c(fuente: &str, nombre: &str, opt: &COpciones) -> Result<String, CError> {
    use crate::translate::{es_archivo_espanol, traducir_con, Direccion, Opciones};
    // La traducción es token por token y conserva los saltos de línea,
    // así que los números de línea del error valen para el .kami.
    let rust = if es_archivo_espanol(fuente) {
        traducir_con(fuente, Direccion::ARust, Opciones::default())
    } else {
        fuente.to_string()
    };
    rust_a_c(&rust, nombre, opt)
}

/// Rust → C. `nombre` es el archivo origen (para errores).
pub fn rust_a_c(fuente: &str, nombre: &str, opt: &COpciones) -> Result<String, CError> {
    let archivo: syn::File =
        syn::parse_file(fuente).map_err(|e| errores::sintaxis(nombre, e))?;
    let mut b = Bajador::nuevo(nombre.to_string(), fuente);
    b.pase1(&archivo)?;
    b.pase2(&archivo)?;
    b.emite_ayudas()?;
    b.ensambla(opt.autonomo)
}
