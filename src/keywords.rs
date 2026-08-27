//! Keyword map Rust <-> espanol.
//!
//! corta=true: English is already the short Spanish (pub, mod) or the same word
//! (super, mut). Default leaves them. --todas may lengthen them.
//! Otherwise even 2-letter grammar words translate: if->si, use->usa, dyn->din.

#[derive(Clone, Copy)]
pub struct Par {
    pub en: &'static str,
    pub es: &'static str,
    pub corta: bool,
}

pub const PARES: &[Par] = &[
    Par { en: "as", es: "como", corta: false },
    Par { en: "async", es: "asinc", corta: false },
    Par { en: "await", es: "esperar", corta: false },
    Par { en: "break", es: "romper", corta: false },
    Par { en: "const", es: "constante", corta: false },
    Par { en: "continue", es: "continuar", corta: false },
    Par { en: "crate", es: "paquete", corta: true },
    Par { en: "dyn", es: "din", corta: false },
    Par { en: "else", es: "sino", corta: false },
    Par { en: "enum", es: "enumeracion", corta: false },
    Par { en: "extern", es: "externo", corta: false },
    Par { en: "false", es: "falso", corta: false },
    Par { en: "fn", es: "funcion", corta: false },
    Par { en: "for", es: "para", corta: false },
    Par { en: "if", es: "si", corta: false },
    Par { en: "impl", es: "implementa", corta: false },
    Par { en: "in", es: "en", corta: false },
    Par { en: "let", es: "sea", corta: false },
    Par { en: "loop", es: "ciclo", corta: false },
    Par { en: "match", es: "segun", corta: false },
    Par { en: "mod", es: "modulo", corta: true },
    Par { en: "move", es: "mover", corta: false },
    Par { en: "mut", es: "mut", corta: true },
    Par { en: "pub", es: "publico", corta: true },
    Par { en: "ref", es: "ref", corta: true },
    Par { en: "return", es: "retornar", corta: false },
    Par { en: "self", es: "yo", corta: false },
    Par { en: "Self", es: "Yo", corta: false },
    Par { en: "static", es: "estatico", corta: false },
    Par { en: "struct", es: "estructura", corta: false },
    Par { en: "super", es: "super", corta: true },
    Par { en: "trait", es: "rasgo", corta: false },
    Par { en: "true", es: "verdadero", corta: false },
    Par { en: "type", es: "tipo", corta: false },
    Par { en: "unsafe", es: "inseguro", corta: false },
    Par { en: "use", es: "usa", corta: false },
    Par { en: "where", es: "donde", corta: false },
    Par { en: "while", es: "mientras", corta: false },
    Par { en: "union", es: "union", corta: true },
    // reserved
    Par { en: "abstract", es: "abstracto", corta: false },
    Par { en: "become", es: "devenir", corta: false },
    Par { en: "box", es: "caja", corta: false },
    Par { en: "do", es: "hacer", corta: false },
    Par { en: "final", es: "final", corta: true },
    Par { en: "gen", es: "gen", corta: true },
    Par { en: "macro", es: "macro", corta: true },
    Par { en: "override", es: "sobrescribe", corta: false },
    Par { en: "priv", es: "privado", corta: true },
    Par { en: "try", es: "intenta", corta: false },
    Par { en: "typeof", es: "tipode", corta: false },
    Par { en: "unsized", es: "sintamano", corta: false },
    Par { en: "virtual", es: "virtual", corta: true },
    Par { en: "yield", es: "ceder", corta: false },
];

pub fn en_a_es(w: &str, todas: bool) -> Option<&'static str> {
    for p in PARES {
        if p.en == w {
            if p.corta && !todas {
                return None;
            }
            return Some(p.es);
        }
    }
    None
}

pub fn es_a_en(w: &str) -> Option<&'static str> {
    for p in PARES {
        if p.es == w {
            return Some(p.en);
        }
    }
    match w {
        "usar" => Some("use"),
        _ => None,
    }
}

pub fn es_keyword(w: &str) -> bool {
    es_a_en(w).is_some()
}
