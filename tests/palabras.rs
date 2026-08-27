use kamisolari::keywords::PARES;
use kamisolari::translate::{traducir, traducir_con, Direccion, Opciones};

fn check(en: &str, es: &str, corta: bool) {
    let src = format!("/*L*/ {en} /*R*/");
    let got = traducir(&src, Direccion::AEs);
    if corta {
        assert_eq!(got, src, "debe quedarse `{en}`, obtuvo `{got}`");
        let todas = traducir_con(&src, Direccion::AEs, Opciones { todas: true });
        let exp = format!("/*L*/ {es} /*R*/");
        assert_eq!(todas, exp, "--todas `{en}` -> `{es}`, obtuvo `{todas}`");
        assert_eq!(
            traducir(&todas, Direccion::ARust),
            src,
            "vuelta --todas `{en}`"
        );
    } else {
        let exp = format!("/*L*/ {es} /*R*/");
        assert_eq!(got, exp, "`{en}` -> `{es}`, obtuvo `{got}`");
        assert_eq!(traducir(&got, Direccion::ARust), src, "vuelta `{en}`");
    }
}

macro_rules! kw {
    ($name:ident, $en:literal, $es:literal, $corta:expr) => {
        #[test]
        fn $name() {
            check($en, $es, $corta);
        }
    };
}

// --- strict keywords ---
kw!(as_como, "as", "como", false);
kw!(async_asinc, "async", "asinc", false);
kw!(await_esperar, "await", "esperar", false);
kw!(break_romper, "break", "romper", false);
kw!(const_constante, "const", "constante", false);
kw!(continue_continuar, "continue", "continuar", false);
kw!(crate_paquete, "crate", "paquete", true);
kw!(dyn_din, "dyn", "din", false);
kw!(else_sino, "else", "sino", false);
kw!(enum_enumeracion, "enum", "enumeracion", false);
kw!(extern_externo, "extern", "externo", false);
kw!(false_falso, "false", "falso", false);
kw!(fn_funcion, "fn", "funcion", false);
kw!(for_para, "for", "para", false);
kw!(if_si, "if", "si", false);
kw!(impl_implementa, "impl", "implementa", false);
kw!(in_en, "in", "en", false);
kw!(let_sea, "let", "sea", false);
kw!(loop_ciclo, "loop", "ciclo", false);
kw!(match_segun, "match", "segun", false);
kw!(mod_modulo, "mod", "modulo", true);
kw!(move_mover, "move", "mover", false);
kw!(mut_mut, "mut", "mut", true);
kw!(pub_publico, "pub", "publico", true);
kw!(ref_ref, "ref", "ref", true);
kw!(return_retornar, "return", "retornar", false);
kw!(self_yo, "self", "yo", false);
kw!(self_type_yo, "Self", "Yo", false);
kw!(static_estatico, "static", "estatico", false);
kw!(struct_estructura, "struct", "estructura", false);
kw!(super_super, "super", "super", true);
kw!(trait_rasgo, "trait", "rasgo", false);
kw!(true_verdadero, "true", "verdadero", false);
kw!(type_tipo, "type", "tipo", false);
kw!(unsafe_inseguro, "unsafe", "inseguro", false);
kw!(use_usa, "use", "usa", false);
kw!(where_donde, "where", "donde", false);
kw!(while_mientras, "while", "mientras", false);
kw!(union_union, "union", "union", true);

// --- reserved ---
kw!(abstract_abstracto, "abstract", "abstracto", false);
kw!(become_devenir, "become", "devenir", false);
kw!(box_caja, "box", "caja", false);
kw!(do_hacer, "do", "hacer", false);
kw!(final_final, "final", "final", true);
kw!(gen_gen, "gen", "gen", true);
kw!(macro_macro, "macro", "macro", true);
kw!(override_sobrescribe, "override", "sobrescribe", false);
kw!(priv_privado, "priv", "privado", true);
kw!(try_intenta, "try", "intenta", false);
kw!(typeof_tipode, "typeof", "tipode", false);
kw!(unsized_sintamano, "unsized", "sintamano", false);
kw!(virtual_virtual, "virtual", "virtual", true);
kw!(yield_ceder, "yield", "ceder", false);

#[test]
fn pares_cubre_cada_test() {
    for p in PARES {
        let src = format!("/*L*/ {} /*R*/", p.en);
        let got = traducir(&src, Direccion::AEs);
        if p.corta {
            assert_eq!(got, src, "PARES corta {}", p.en);
        } else {
            assert_eq!(
                got,
                format!("/*L*/ {} /*R*/", p.es),
                "PARES {} -> {}",
                p.en,
                p.es
            );
        }
        let todas = traducir_con(&src, Direccion::AEs, Opciones { todas: true });
        assert_eq!(
            todas,
            format!("/*L*/ {} /*R*/", p.es),
            "PARES --todas {} -> {}",
            p.en,
            p.es
        );
        assert_eq!(
            traducir(&todas, Direccion::ARust),
            src,
            "PARES vuelta {}",
            p.en
        );
    }
}

#[test]
fn snippet_realista() {
    let rs = r#"
pub mod demo {
    use std::io;
    pub struct Punto { pub x: i32 }
    pub trait Dibuja { fn trazo(&self); }
    impl Dibuja for Punto {
        fn trazo(&self) {
            if true {
                let mut n = 0;
                while n < 1 {
                    n += 1;
                    continue;
                }
                for _i in 0..1 {
                    break;
                }
            } else {
                return;
            }
        }
    }
    pub fn crea<T>(x: Box<T>) -> Box<T> where T: 'static {
        let y = Box::new(x);
        y
    }
    pub enum Color { Rojo, Azul }
    pub const N: i32 = 1;
    pub static S: i32 = 2;
    pub type Num = i32;
    pub async fn va() { true.await; }
    unsafe fn u() {}
    pub fn dyn_obj(x: &dyn Dibuja) { let _ = x; }
}
"#;
    let es = traducir(rs, Direccion::AEs);
    assert!(es.contains("usa std::io"), "{es}");
    assert!(es.contains("pub estructura Punto"), "{es}");
    assert!(es.contains("pub rasgo Dibuja"), "{es}");
    assert!(es.contains("funcion trazo(&yo)"), "{es}");
    assert!(es.contains("implementa Dibuja para Punto"), "{es}");
    assert!(es.contains("si verdadero"), "{es}");
    assert!(es.contains("sea mut n"), "{es}");
    assert!(es.contains("mientras n"), "{es}");
    assert!(es.contains("continuar"), "{es}");
    assert!(es.contains("para _i en 0..1"), "{es}");
    assert!(es.contains("romper"), "{es}");
    assert!(es.contains("sino"), "{es}");
    assert!(es.contains("retornar"), "{es}");
    assert!(es.contains("Caja<T>"), "{es}");
    assert!(es.contains("donde T:"), "{es}");
    assert!(es.contains("Caja::nuevo"), "{es}");
    assert!(es.contains("pub enumeracion Color"), "{es}");
    assert!(es.contains("pub constante N"), "{es}");
    assert!(es.contains("pub estatico S"), "{es}");
    assert!(es.contains("pub tipo Num"), "{es}");
    assert!(es.contains("pub asinc funcion va"), "{es}");
    assert!(es.contains("verdadero.esperar"), "{es}");
    assert!(es.contains("inseguro funcion u"), "{es}");
    assert!(es.contains("&din Dibuja"), "{es}");
    assert!(es.contains("pub mod demo"), "{es}");
    let back = traducir(&es, Direccion::ARust);
    assert_eq!(back, rs);
}

#[test]
fn prelude_new_box_main_imprimir() {
    let rs = "fn main() { let x = Box::new(1); println!(\"{}\", x); }\n";
    let es = traducir(rs, Direccion::AEs);
    assert!(es.contains("funcion principal"), "{es}");
    assert!(es.contains("Caja::nuevo"), "{es}");
    assert!(es.contains("imprimir(\"{}\", x)"), "{es}");
    assert_eq!(traducir(&es, Direccion::ARust), rs);
}
