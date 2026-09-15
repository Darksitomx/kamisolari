//! E2E del backend C: `.kami` → `.c` → `gcc` → correr → comparar.
//!
//! Cada prueba baja un ejemplo con `kami_a_c`, lo compila con `gcc`
//! (C11) y verifica que el binario imprime lo mismo que
//! `kamisolari correr` sobre el mismo ejemplo.

use std::fs;
use std::path::PathBuf;
use std::process::Command;

use kamisolari::c::{kami_a_c, COpciones};

fn repo(path: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(path)
}

fn tmpdir() -> PathBuf {
    let d = std::env::temp_dir().join(format!("kami-c-{}", std::process::id()));
    fs::create_dir_all(&d).unwrap();
    d
}

/// Baja `ejemplo`, lo compila con gcc y devuelve su stdout.
fn salida_c(ejemplo: &str) -> String {
    let fuente = fs::read_to_string(repo(ejemplo)).unwrap();
    let opt = COpciones { autonomo: true };
    let c = kami_a_c(&fuente, ejemplo, &opt).unwrap_or_else(|e| panic!("kami_a_c falló: {e}"));
    let d = tmpdir();
    let base = PathBuf::from(ejemplo).file_stem().unwrap().to_string_lossy().into_owned();
    let fc = d.join(format!("{base}.c"));
    let bin = d.join(&base);
    fs::write(&fc, &c).unwrap();
    let gcc = Command::new("gcc")
        .args(["-std=c11", "-Wall", "-o"])
        .arg(&bin)
        .arg(&fc)
        .output()
        .expect("lanzar gcc");
    assert!(
        gcc.status.success(),
        "gcc falló para {ejemplo}:\n{}\n--- C generado ---\n{c}",
        String::from_utf8_lossy(&gcc.stderr),
    );
    let run = Command::new(&bin).output().expect("correr binario C");
    assert!(
        run.status.success(),
        "el binario C falló para {ejemplo}: {}",
        String::from_utf8_lossy(&run.stderr)
    );
    String::from_utf8_lossy(&run.stdout).into_owned()
}

/// Salida de referencia vía `kamisolari correr` (rustc).
fn salida_ref(ejemplo: &str) -> String {
    let out = Command::new(env!("CARGO_BIN_EXE_kamisolari"))
        .arg("correr")
        .arg(repo(ejemplo))
        .output()
        .expect("kamisolari correr");
    assert!(
        out.status.success(),
        "referencia falló para {ejemplo}: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    String::from_utf8_lossy(&out.stdout).into_owned()
}

fn compara(ejemplo: &str) {
    assert_eq!(salida_c(ejemplo), salida_ref(ejemplo), "difiere {ejemplo}");
}

#[test]
fn c_helloworld() {
    compara("examples/helloworld.kami");
}

#[test]
fn c_hola() {
    compara("examples/hola.kami");
}

#[test]
fn c_cuenta() {
    compara("examples/cuenta.kami");
}

#[test]
fn c_rasgo() {
    compara("examples/rasgo.kami");
}

#[test]
fn c_pila() {
    compara("examples/pila.kami");
}

#[test]
fn c_fizzbuzz() {
    compara("examples/fizzbuzz.kami");
}

#[test]
fn c_bucle() {
    compara("examples/bucle.kami");
}

fn kami() -> Command {
    Command::new(env!("CARGO_BIN_EXE_kamisolari"))
}

/// Subdirectorio temporal único por prueba (el común se comparte entre hilos).
fn tmpdir_unico(nombre: &str) -> PathBuf {
    let d = tmpdir().join(nombre);
    fs::create_dir_all(&d).unwrap();
    d
}

#[test]
fn cli_c_stdout() {
    let out = kami()
        .arg("c")
        .arg(repo("examples/hola.kami"))
        .output()
        .expect("kamisolari c");
    assert!(out.status.success(), "stderr: {}", String::from_utf8_lossy(&out.stderr));
    let c = String::from_utf8_lossy(&out.stdout);
    assert!(c.contains("int main(void)"), "el C trae main");
    assert!(c.contains("kami_texto"), "autónomo trae el runtime");
}

#[test]
fn cli_c_archivo_y_gcc() {
    let d = tmpdir();
    let fc = d.join("hola_cli.c");
    let bin = d.join("hola_cli");
    let out = kami()
        .arg("c")
        .arg(repo("examples/hola.kami"))
        .arg("-o")
        .arg(&fc)
        .output()
        .expect("kamisolari c -o");
    assert!(out.status.success(), "stderr: {}", String::from_utf8_lossy(&out.stderr));
    let gcc = Command::new("gcc")
        .args(["-std=c11", "-o"])
        .arg(&bin)
        .arg(&fc)
        .arg("-lm")
        .output()
        .expect("gcc");
    assert!(gcc.status.success(), "gcc: {}", String::from_utf8_lossy(&gcc.stderr));
    let run = Command::new(&bin).output().expect("binario");
    assert_eq!(String::from_utf8_lossy(&run.stdout), salida_ref("examples/hola.kami"));
}

#[test]
fn cli_c_correr() {
    let out = kami()
        .arg("c")
        .arg(repo("examples/cuenta.kami"))
        .arg("--correr-c")
        .output()
        .expect("kamisolari c --correr-c");
    assert!(out.status.success(), "stderr: {}", String::from_utf8_lossy(&out.stderr));
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        salida_ref("examples/cuenta.kami")
    );
}

#[test]
fn cli_c_no_autonomo() {
    let d = tmpdir();
    let fc = d.join("hola_aparte.c");
    let out = kami()
        .arg("c")
        .arg(repo("examples/hola.kami"))
        .arg("-o")
        .arg(&fc)
        .arg("--no-autonomo")
        .output()
        .expect("kamisolari c --no-autonomo");
    assert!(out.status.success(), "stderr: {}", String::from_utf8_lossy(&out.stderr));
    let c = fs::read_to_string(&fc).unwrap();
    assert!(c.contains("#include \"kami.h\""), "pide kami.h");
    assert!(d.join("kami.h").exists(), "escribe kami.h al lado");
}

#[test]
fn cli_c_default_separado() {
    // Sin flags: .c corto que pide kami.h, y kami.h al lado. Compila y corre.
    let d = tmpdir_unico("default_separado");
    let fc = d.join("hola.c");
    let bin = d.join("hola");
    let out = kami()
        .arg("c")
        .arg(repo("examples/hola.kami"))
        .arg("-o")
        .arg(&fc)
        .output()
        .expect("kamisolari c -o");
    assert!(out.status.success(), "stderr: {}", String::from_utf8_lossy(&out.stderr));
    let c = fs::read_to_string(&fc).unwrap();
    assert!(c.contains("#include \"kami.h\""), "pide kami.h");
    assert!(!c.contains("KAMISOLARI_KAMI_H"), "sin runtime embebido");
    assert!(c.lines().count() < 150, "corto: {} líneas", c.lines().count());
    assert!(d.join("kami.h").exists(), "escribe kami.h al lado");
    let gcc = Command::new("gcc")
        .args(["-std=c11", "-Wall", "-o"])
        .arg(&bin)
        .arg(&fc)
        .arg("-lm")
        .output()
        .expect("gcc");
    assert!(gcc.status.success(), "gcc: {}", String::from_utf8_lossy(&gcc.stderr));
    let run = Command::new(&bin).output().expect("binario");
    assert_eq!(String::from_utf8_lossy(&run.stdout), salida_ref("examples/hola.kami"));
}

#[test]
fn cli_c_autonomo_opt_in() {
    // --autonomo: un solo .c autocontenido, sin kami.h al lado.
    let d = tmpdir_unico("autonomo_opt_in");
    let fc = d.join("hola.c");
    let bin = d.join("hola");
    let out = kami()
        .arg("c")
        .arg(repo("examples/hola.kami"))
        .arg("-o")
        .arg(&fc)
        .arg("--autonomo")
        .output()
        .expect("kamisolari c --autonomo");
    assert!(out.status.success(), "stderr: {}", String::from_utf8_lossy(&out.stderr));
    let c = fs::read_to_string(&fc).unwrap();
    assert!(c.contains("KAMISOLARI_KAMI_H"), "trae el runtime");
    assert!(!d.join("kami.h").exists(), "no escribe kami.h");
    let gcc = Command::new("gcc")
        .args(["-std=c11", "-o"])
        .arg(&bin)
        .arg(&fc)
        .arg("-lm")
        .output()
        .expect("gcc");
    assert!(gcc.status.success(), "gcc: {}", String::from_utf8_lossy(&gcc.stderr));
    let run = Command::new(&bin).output().expect("binario");
    assert_eq!(String::from_utf8_lossy(&run.stdout), salida_ref("examples/hola.kami"));
}

/// Ningún temporal `t` + dígitos como palabra (`t0`, `t12`...) en lo generado.
fn assert_sin_t(c: &str, ejemplo: &str) {
    let gen = c.split("/* === defines === */").nth(1).unwrap_or(c);
    let gen = gen.split("#include \"kami.h\"").nth(1).unwrap_or(gen);
    let b = gen.as_bytes();
    for i in 0..b.len() {
        if b[i] == b't' && i + 1 < b.len() && b[i + 1].is_ascii_digit() {
            let prev = if i == 0 { 0 } else { b[i - 1] };
            if !(prev.is_ascii_alphanumeric() || prev == b'_' || prev == b'$') {
                let ini = i.saturating_sub(30);
                let fin = (i + 10).min(b.len());
                panic!("{ejemplo} trae temporal con nombre t: ...{}...", &gen[ini..fin]);
            }
        }
    }
}

fn genera(ejemplo: &str) -> String {
    let fuente = fs::read_to_string(repo(ejemplo)).unwrap();
    kami_a_c(&fuente, ejemplo, &COpciones { autonomo: true })
        .unwrap_or_else(|e| panic!("kami_a_c falló: {e}"))
}

#[test]
fn c_rasgo_documentado() {
    let c = genera("examples/rasgo.kami");
    assert!(c.contains("Saludar::saludo para Persona"), "contexto del método");
    assert!(c.contains("fn saludo(&self) -> String */"), "firma Rust:\n{c}");
    assert!(c.contains("/* mueve */"), "anota el movimiento");
    assert!(c.contains("nombre; /* String */"), "tipo Rust del campo");
    assert!(c.contains("&self->nombre"), "deref normalizado");
    assert!(c.contains("persona"), "temporal descriptivo persona");
    assert!(c.contains("texto"), "temporal descriptivo texto");
    assert!(c.contains("vista"), "temporal descriptivo vista");
    assert_sin_t(&c, "rasgo");
}

#[test]
fn c_sin_temporales_t() {
    for ej in ["helloworld", "hola", "cuenta", "rasgo", "pila", "fizzbuzz", "bucle"] {
        let f = format!("examples/{ej}.kami");
        assert_sin_t(&genera(&f), &f);
    }
}

#[test]
fn c_docs_fuente_viajan() {
    // Los `///` de structs y funciones aparecen en el C.
    let fuente = "/// Una persona con nombre.\nestructura Persona {\n    nombre: Texto,\n}\n\n/// Arma el saludo.\nfuncion saludo(p: &Persona) -> Texto {\n    formato!(\"hola, {}\", p.nombre)\n}\n\nfuncion principal() {\n    sea p = Persona { nombre: \"Ana\".into() };\n    imprimir(\"{}\", saludo(&p));\n}\n";
    let c = kami_a_c(fuente, "docs", &COpciones { autonomo: true }).unwrap();
    assert!(c.contains("// Una persona con nombre."), "docs del struct");
    assert!(c.contains("// Arma el saludo."), "docs de la función");
    assert!(c.contains("/* fn saludo(p: &Persona) -> String */"), "firma:\n{c}");
}
