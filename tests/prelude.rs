use kamisolari::keywords::PARES;
use kamisolari::prelude::PRELUDE;
use kamisolari::translate::{traducir, Direccion};

fn check(en: &str, es: &str) {
    let src = format!("/*L*/ {en} /*R*/");
    let got = traducir(&src, Direccion::AEs);
    let exp = format!("/*L*/ {es} /*R*/");
    assert_eq!(got, exp, "`{en}` -> `{es}`, obtuvo `{got}`");
    assert_eq!(traducir(&got, Direccion::ARust), src, "vuelta `{en}`");
}

#[test]
fn cada_metodo_del_prelude() {
    for p in PRELUDE {
        check(p.en, p.es);
    }
}

#[test]
fn nombres_unicos() {
    let mut en = std::collections::HashSet::new();
    let mut es = std::collections::HashSet::new();
    for p in PARES {
        assert!(en.insert(p.en), "en duplicado {}", p.en);
        if p.es != p.en {
            assert!(es.insert(p.es), "es duplicado keyword {}", p.es);
        }
    }
    for p in PRELUDE {
        assert!(en.insert(p.en), "en duplicado prelude {}", p.en);
        assert!(
            es.insert(p.es),
            "es duplicado prelude {} (choca con keyword o metodo)",
            p.es
        );
        assert!(!en.contains(p.es) || p.es == p.en, "es {} es un en", p.es);
    }
}

#[test]
fn into_hacia() {
    let rs = "fn f(x: i32) -> u32 { x.into() }\n";
    let es = traducir(rs, Direccion::AEs);
    assert!(es.contains(".hacia()"), "{es}");
    assert!(!es.contains(".into()"), "{es}");
    assert_eq!(traducir(&es, Direccion::ARust), rs);
}

#[test]
fn from_desde() {
    let rs = "fn f() { let _ = u32::from(1); }\n";
    let es = traducir(rs, Direccion::AEs);
    assert!(es.contains("u32::desde(1)"), "{es}");
    assert_eq!(traducir(&es, Direccion::ARust), rs);
}

#[test]
fn clone_clona() {
    let rs = "fn f(x: String) { let _ = x.clone(); }\n";
    let es = traducir(rs, Direccion::AEs);
    assert!(es.contains(".clona()"), "{es}");
    assert_eq!(traducir(&es, Direccion::ARust), rs);
}

#[test]
fn unwrap_desenvuelve() {
    let rs = "fn f(x: Option<i32>) { x.unwrap(); }\n";
    let es = traducir(rs, Direccion::AEs);
    assert!(es.contains(".desenvuelve()"), "{es}");
    assert_eq!(traducir(&es, Direccion::ARust), rs);
}

#[test]
fn len_push_collect() {
    let rs = "fn f(v: Vec<i32>) { v.len(); v.push(1); v.into_iter().collect() }\n";
    let es = traducir(rs, Direccion::AEs);
    assert!(es.contains(".largo()"), "{es}");
    assert!(es.contains(".empuja(1)"), "{es}");
    assert!(es.contains(".hacia_iter().recolecta()"), "{es}");
    assert_eq!(traducir(&es, Direccion::ARust), rs);
}

#[test]
fn some_none() {
    let rs = "fn f() { let _ = Some(1); let _ = None; }\n";
    let es = traducir(rs, Direccion::AEs);
    assert!(es.contains("Alguno(1)"), "{es}");
    assert!(es.contains("Ninguno"), "{es}");
    assert_eq!(traducir(&es, Direccion::ARust), rs);
}

#[test]
fn map_filter() {
    let rs = "fn f() { let _ = xs.iter().filter(|x| true).map(|x| x).collect(); }\n";
    let es = traducir(rs, Direccion::AEs);
    assert!(es.contains(".filtra("), "{es}");
    assert!(es.contains(".mapea("), "{es}");
    assert!(es.contains(".recolecta()"), "{es}");
    assert_eq!(traducir(&es, Direccion::ARust), rs);
}

#[test]
fn vec_string_option() {
    let rs = "fn f(v: Vec<String>) -> Option<i32> { let s: String = String::new(); None }\n";
    let es = traducir(rs, Direccion::AEs);
    assert!(es.contains("Lista<Texto>"), "{es}");
    assert!(es.contains("Opcion<i32>"), "{es}");
    assert!(es.contains("Texto::nuevo"), "{es}");
    assert!(es.contains("Ninguno"), "{es}");
    assert_eq!(traducir(&es, Direccion::ARust), rs);
}

#[test]
fn snippet_metodos() {
    let rs = r#"
fn g(v: Vec<String>) -> String {
    v.into_iter()
        .filter(|s| !s.is_empty())
        .map(|s| s.to_string())
        .collect()
}
"#;
    let es = traducir(rs, Direccion::AEs);
    assert!(es.contains("hacia_iter"), "{es}");
    assert!(es.contains("filtra"), "{es}");
    assert!(es.contains("esta_vacio"), "{es}");
    assert!(es.contains("a_texto"), "{es}");
    assert!(es.contains("recolecta"), "{es}");
    assert_eq!(traducir(&es, Direccion::ARust), rs);
}
