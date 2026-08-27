// Token-level translate. Comments/strings/lifetimes copied as-is.
use rustc_lexer::{tokenize, TokenKind};

use crate::keywords::{en_a_es, es_a_en, es_keyword, PARES};
use crate::prelude::{prelude_en_a_es, prelude_es_a_en, SIN_BANG};

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Direccion {
    AEs,
    ARust,
}

#[derive(Clone, Copy)]
pub struct Opciones {
    pub todas: bool,
}

impl Default for Opciones {
    fn default() -> Self {
        Self { todas: false }
    }
}

struct Tok {
    kind: TokenKind,
    start: usize,
    len: usize,
}

pub fn traducir(src: &str, dir: Direccion) -> String {
    traducir_con(src, dir, Opciones::default())
}

pub fn traducir_con(src: &str, dir: Direccion, opt: Opciones) -> String {
    let mut toks = Vec::new();
    let mut pos = 0usize;
    for t in tokenize(src) {
        toks.push(Tok {
            kind: t.kind,
            start: pos,
            len: t.len,
        });
        pos += t.len;
    }

    let mut skip = vec![false; toks.len()];
    let mut repl = vec![None; toks.len()];

    for i in 0..toks.len() {
        if toks[i].kind != TokenKind::Ident {
            continue;
        }
        let ident = &src[toks[i].start..toks[i].start + toks[i].len];
        match dir {
            Direccion::AEs => map_a_es(src, &toks, i, ident, opt, &mut skip, &mut repl),
            Direccion::ARust => map_a_rust(src, &toks, i, ident, &mut skip, &mut repl),
        }
    }

    let mut out = String::with_capacity(src.len() + 16);
    for (i, t) in toks.iter().enumerate() {
        if skip[i] {
            continue;
        }
        if let Some(r) = &repl[i] {
            out.push_str(r);
        } else {
            out.push_str(&src[t.start..t.start + t.len]);
        }
    }
    out
}

fn sin_bang_en(w: &str) -> Option<&'static str> {
    SIN_BANG.iter().find(|(en, _)| *en == w).map(|(_, es)| *es)
}

fn sin_bang_es(w: &str) -> Option<&'static str> {
    SIN_BANG.iter().find(|(_, es)| *es == w).map(|(en, _)| *en)
}

fn next_sig(toks: &[Tok], i: usize) -> Option<usize> {
    let mut j = i + 1;
    while j < toks.len() {
        if toks[j].kind != TokenKind::Whitespace {
            return Some(j);
        }
        j += 1;
    }
    None
}

fn map_a_es(
    _src: &str,
    toks: &[Tok],
    i: usize,
    ident: &str,
    opt: Opciones,
    skip: &mut [bool],
    repl: &mut [Option<String>],
) {
    if let Some(es) = sin_bang_en(ident) {
        if let Some(j) = next_sig(toks, i) {
            if toks[j].kind == TokenKind::Not {
                skip[j] = true;
            }
        }
        repl[i] = Some(es.into());
        return;
    }
    if ident == "main" {
        repl[i] = Some("principal".into());
        return;
    }
    if ident == "Box" {
        repl[i] = Some("Caja".into());
        return;
    }
    if ident == "principal" || ident == "Caja" || sin_bang_es(ident).is_some() {
        repl[i] = Some(format!("r#{ident}"));
        return;
    }
    if let Some(es) = prelude_en_a_es(ident) {
        repl[i] = Some(es.into());
        return;
    }
    if prelude_es_a_en(ident).is_some() {
        repl[i] = Some(format!("r#{ident}"));
        return;
    }
    if let Some(es) = en_a_es(ident, opt.todas) {
        repl[i] = Some(es.into());
    } else if es_keyword(ident) && PARES.iter().all(|p| p.en != ident) {
        repl[i] = Some(format!("r#{ident}"));
    }
}

fn map_a_rust(
    _src: &str,
    toks: &[Tok],
    i: usize,
    ident: &str,
    _skip: &mut [bool],
    repl: &mut [Option<String>],
) {
    if let Some(en) = sin_bang_es(ident) {
        if let Some(j) = next_sig(toks, i) {
            if toks[j].kind == TokenKind::Not {
                repl[i] = Some(en.into());
                return;
            }
            if toks[j].kind == TokenKind::OpenParen {
                repl[i] = Some(format!("{en}!"));
                return;
            }
        }
        repl[i] = Some(en.into());
        return;
    }
    if ident == "principal" {
        repl[i] = Some("main".into());
        return;
    }
    if ident == "Caja" {
        repl[i] = Some("Box".into());
        return;
    }
    if let Some(en) = prelude_es_a_en(ident) {
        repl[i] = Some(en.into());
        return;
    }
    if let Some(en) = es_a_en(ident) {
        repl[i] = Some(en.into());
    }
}

const FUERTES: &[&str] = &[
    "funcion",
    "sea",
    "mientras",
    "estructura",
    "implementa",
    "rasgo",
    "segun",
    "retornar",
    "ciclo",
    "enumeracion",
    "principal",
    "imprimir",
];

pub fn es_archivo_espanol(src: &str) -> bool {
    let mut pos = 0usize;
    for tok in tokenize(src) {
        let end = pos + tok.len;
        let slice = &src[pos..end];
        pos = end;
        if tok.kind == TokenKind::Ident && FUERTES.contains(&slice) {
            return true;
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ida_y_vuelta_basica() {
        let rs = "fn main() { let mut x = 1; if x > 0 { return; } }\n";
        let es = traducir(rs, Direccion::AEs);
        assert!(es.contains("funcion principal"), "{es}");
        assert!(es.contains("sea mut x"), "{es}");
        assert!(es.contains("si x"), "{es}");
        assert!(!es.contains("if x"), "{es}");
        assert!(es.contains("retornar"), "{es}");
        let back = traducir(&es, Direccion::ARust);
        assert_eq!(back, rs);
    }

    #[test]
    fn imprimir_sin_bang() {
        let rs = "fn main() { println!(\"hola mundo\"); }\n";
        let es = traducir(rs, Direccion::AEs);
        assert!(es.contains("funcion principal"), "{es}");
        assert!(es.contains("imprimir(\"hola mundo\")"), "{es}");
        assert!(!es.contains("println"), "{es}");
        assert!(!es.contains("imprimir!"), "{es}");
        let back = traducir(&es, Direccion::ARust);
        assert_eq!(back, rs, "es era: {es}");
    }

    #[test]
    fn no_toca_strings_ni_comentarios() {
        let rs = r#"fn f() { let _ = "if let fn"; /* if */ }"#;
        let es = traducir(rs, Direccion::AEs);
        assert!(es.contains("\"if let fn\""));
        assert!(es.contains("/* if */"));
    }

    #[test]
    fn todas_si_traduce_if() {
        let rs = "fn f() { if true { let x = 1; } }\n";
        let es = traducir_con(rs, Direccion::AEs, Opciones { todas: true });
        assert!(es.contains("si verdadero"), "{es}");
        assert!(es.contains("sea x"), "{es}");
    }

    #[test]
    fn box_a_caja() {
        let rs = "fn f(x: Box<i32>) { let _ = box 1; }\n";
        let es = traducir(rs, Direccion::AEs);
        assert!(es.contains("Caja<i32>"), "{es}");
        assert!(es.contains("caja 1"), "{es}");
        let back = traducir(&es, Direccion::ARust);
        assert_eq!(back, rs, "{es}");
    }

    #[test]
    fn new_a_nuevo() {
        let rs = "fn f() { let x = Box::new(1); }\n";
        let es = traducir(rs, Direccion::AEs);
        assert!(es.contains("Caja::nuevo(1)"), "{es}");
        assert!(!es.contains("new"), "{es}");
        let back = traducir(&es, Direccion::ARust);
        assert_eq!(back, rs, "{es}");
    }
}
