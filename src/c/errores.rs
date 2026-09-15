//! Errores del backend C: mensajes en español, con ubicación y ayuda.
//!
//! Filosofía: si algo no se puede llevar a C, el error debe decir QUÉ,
//! DÓNDE y CÓMO reescribirlo en kamisolari. Nada de "internal error".

use std::fmt;

use syn::spanned::Spanned;

/// Error del backend C.
#[derive(Debug, Clone)]
pub struct CError {
    /// Código estable, p.ej. `C0003`.
    pub codigo: &'static str,
    /// Qué pasó.
    pub mensaje: String,
    /// Cómo arreglarlo (opcional pero preferida).
    pub ayuda: Option<String>,
    /// Archivo (el .kami original de ser posible).
    pub archivo: Option<String>,
    /// Línea 1-based dentro del fuente parseado.
    pub linea: Option<usize>,
    /// Columna 1-based.
    pub columna: Option<usize>,
    /// Fragmento de la línea donde ocurrió.
    pub fragmento: Option<String>,
}

impl CError {
    pub fn nuevo(codigo: &'static str, mensaje: impl Into<String>) -> Self {
        Self {
            codigo,
            mensaje: mensaje.into(),
            ayuda: None,
            archivo: None,
            linea: None,
            columna: None,
            fragmento: None,
        }
    }

    pub fn no_soportado(mensaje: impl Into<String>) -> Self {
        Self::nuevo("C0003", mensaje)
    }

    pub fn ayuda(mut self, ayuda: impl Into<String>) -> Self {
        self.ayuda = Some(ayuda.into());
        self
    }

    pub fn con_archivo(mut self, archivo: impl Into<String>) -> Self {
        self.archivo = Some(archivo.into());
        self
    }

    /// Pega ubicación a partir de un nodo de `syn` y las líneas del fuente.
    pub fn con_span<T: Spanned>(mut self, nodo: &T, lineas: &[String]) -> Self {
        let span = nodo.span();
        let inicio = span.start();
        if inicio.line > 0 {
            self.linea = Some(inicio.line);
            self.columna = Some(inicio.column + 1);
            if let Some(lin) = lineas.get(inicio.line - 1) {
                self.fragmento = Some(recorta(lin));
            }
        }
        self
    }
}

fn recorta(linea: &str) -> String {
    const MAX: usize = 100;
    let t = linea.trim();
    if t.chars().count() > MAX {
        format!("{}…", t.chars().take(MAX).collect::<String>())
    } else {
        t.to_string()
    }
}

impl fmt::Display for CError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "error[{}]: {}", self.codigo, self.mensaje)?;
        if let Some(a) = &self.archivo {
            write!(f, "\n  --> {a}")?;
            if let Some(l) = self.linea {
                write!(f, ":{l}")?;
                if let Some(c) = self.columna {
                    write!(f, ":{c}")?;
                }
            }
        } else if let Some(l) = self.linea {
            write!(f, "  (línea {l}")?;
            if let Some(c) = self.columna {
                write!(f, ", col {c}")?;
            }
            write!(f, ")")?;
        }
        if let Some(fr) = &self.fragmento {
            write!(f, "\n   | {fr}")?;
        }
        if let Some(a) = &self.ayuda {
            write!(f, "\n   ayuda: {a}")?;
        }
        Ok(())
    }
}

impl std::error::Error for CError {}

/// Códigos:
/// - C0001: no se pudo parsear (sintaxis).
/// - C0002: tipo desconocido.
/// - C0003: construcción no soportada por el backend C.
/// - C0004: uso de un valor movido.
/// - C0005: falta anotación de tipo (el backend C no puede inferir).
/// - C0006: formato de `imprimir`/`formato` inválido o no soportado.
/// - C0007: método o función asociada desconocida.
/// - C0008: patrón no soportado.
pub fn sintaxis(origen: &str, e: syn::Error) -> CError {
    CError::nuevo(
        "C0001",
        format!("no pude parsear el programa: {e}"),
    )
    .con_archivo(origen)
    .ayuda("verifica el programa con `kamisolari correr archivo.kami` primero; el backend C parte de código que rustc acepta")
}
