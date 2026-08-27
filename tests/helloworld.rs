use std::process::Command;

fn kami() -> Command {
    Command::new(env!("CARGO_BIN_EXE_kamisolari"))
}

fn repo(path: &str) -> std::path::PathBuf {
    std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(path)
}

#[test]
fn hola_mundo() {
    let out = kami()
        .arg("correr")
        .arg(repo("examples/helloworld.kami"))
        .output()
        .expect("kamisolari run");
    assert!(
        out.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&out.stdout).trim(), "hola mundo");
}

#[test]
fn hola_aritmetica() {
    let out = kami()
        .args(["run", "-O"])
        .arg(repo("examples/hola.kami"))
        .output()
        .expect("run hola");
    assert!(out.status.success());
    assert_eq!(
        String::from_utf8_lossy(&out.stdout).trim(),
        "1 + 2 * 3 = 7"
    );
}

#[test]
fn version_flag() {
    let out = kami().arg("-V").output().unwrap();
    assert!(out.status.success());
    let s = String::from_utf8_lossy(&out.stdout);
    assert!(s.contains("kamisolari"), "{s}");
}

#[test]
fn es_cambia_if_a_si() {
    let mut child = kami()
        .args(["es", "-"])
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .spawn()
        .unwrap();
    use std::io::Write;
    child
        .stdin
        .as_mut()
        .unwrap()
        .write_all(b"fn f() { if true { let x = 1; } }\n")
        .unwrap();
    let out = child.wait_with_output().unwrap();
    let s = String::from_utf8_lossy(&out.stdout);
    assert!(s.contains("funcion f"), "{s}");
    assert!(s.contains("si "), "{s}");
    assert!(s.contains("sea x"), "{s}");
    assert!(!s.contains("if "), "{s}");
}

#[test]
fn flag_o_escribe_binario() {
    let dir = std::env::temp_dir().join("kami-hello-bin");
    let _ = std::fs::create_dir_all(&dir);
    let bin = dir.join("hola");
    let out = kami()
        .args(["run", "-o"])
        .arg(&bin)
        .arg(repo("examples/helloworld.kami"))
        .output()
        .unwrap();
    assert!(
        out.status.success(),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(bin.exists(), "no escribio {}", bin.display());
    let run = Command::new(&bin).output().unwrap();
    assert_eq!(String::from_utf8_lossy(&run.stdout).trim(), "hola mundo");
}
