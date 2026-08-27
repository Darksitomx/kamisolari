mod cli;

use std::env;
use std::fs;
use std::io::{self, Read, Write};
use std::path::Path;
use std::process::{self, Command};

use cli::Flags;
use kamisolari::translate::{es_archivo_espanol, traducir_con, Direccion, Opciones};

fn main() {
    let raw: Vec<String> = env::args().skip(1).collect();
    if raw.is_empty() {
        ayuda();
        process::exit(1);
    }
    let flags = match cli::parse(&raw) {
        Ok(f) => f,
        Err(e) => {
            eprintln!("{e}");
            process::exit(1);
        }
    };
    if let Err(e) = despachar(flags) {
        eprintln!("{e}");
        process::exit(1);
    }
}

fn ayuda() {
    eprintln!(
        "\
Kamisolari - Rust con palabras clave en espanol.

  kamisolari es      <archivo|dir> [--salida ruta] [--todas]
  kamisolari rust    <archivo|dir> [--salida ruta]
  kamisolari correr  <archivo> [flags]
  kamisolari construir [dir] [--final] [--verboso]

Flags (en español; las inglesas también valen):
  --salida, -o <ruta>     binario o archivo traducido
  --optimizar, -O         optimizar
  --final                 cargo --release
  --depurar, -g           info de debug
  --edicion <año>         2015|2018|2021|2024 (default 2021)
  --tipo-crate <t>        bin|lib|rlib|dylib|cdylib|staticlib
  --nombre-crate <n>
  --libs, -L <path>
  --codegen, -C <opt>
  --cfg <flag>
  --todas                 traduce también if/as/in/pub/mod/use/dyn
  --verboso, -v
  --callado, -q
  --ver, -V               versión
  --ayuda, -h
  --                      el resto se pasa a rustc

No cambian: pub mod super mut ref crate (ya son la forma corta).
Sí cambian: if→si  use→usa  dyn→din  fn→funcion  let→sea
            main→principal  println!→imprimir()
Lista completa: README.md

  funcion principal() {{
      sea mut n = 0;
      mientras n < 3 {{
          n += 1;
      }}
      imprimir(\"hola mundo\");
  }}
"
    );
}

fn despachar(f: Flags) -> Result<(), String> {
    match f.cmd.as_str() {
        "ayuda" | "" => {
            ayuda();
            Ok(())
        }
        "version" => {
            println!("kamisolari {}", env!("CARGO_PKG_VERSION"));
            Ok(())
        }
        "es" => cmd_traducir(f, Direccion::AEs),
        "rust" => cmd_traducir(f, Direccion::ARust),
        "run" | "correr" => cmd_run(f),
        "build" | "construir" => cmd_build(f),
        otro => Err(format!("comando desconocido '{otro}'. kamisolari --ayuda")),
    }
}

fn vlog(f: &Flags, msg: impl std::fmt::Display) {
    if f.verbose && !f.quiet {
        eprintln!("{msg}");
    }
}

fn cmd_traducir(f: Flags, dir: Direccion) -> Result<(), String> {
    let input = f.input.clone().ok_or("falta archivo o directorio")?;
    let in_path = Path::new(&input);
    let opt = f.opciones();

    if in_path.is_dir() {
        let out = f.output.clone().ok_or("para un directorio usa --salida")?;
        traducir_arbol(in_path, Path::new(&out), dir, opt)?;
        if !f.quiet {
            eprintln!("listo: {out}");
        }
        return Ok(());
    }

    let src = leer(&input)?;
    let out = traducir_con(&src, dir, opt);
    match &f.output {
        Some(p) => {
            fs::write(p, &out).map_err(|e| format!("no pude escribir {p}: {e}"))?;
            vlog(&f, format!("escribio {p}"));
        }
        None => {
            io::stdout()
                .write_all(out.as_bytes())
                .map_err(|e| e.to_string())?;
        }
    }
    Ok(())
}

fn cmd_run(f: Flags) -> Result<(), String> {
    let path = f.input.clone().ok_or("falta archivo")?;
    let src = leer(&path)?;
    let rust = if es_archivo_espanol(&src) {
        traducir_con(&src, Direccion::ARust, f.opciones())
    } else {
        src
    };
    let tmp = std::env::temp_dir();
    let stem = Path::new(&path)
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("k");
    let rs = tmp.join(format!("kamisolari-{stem}.rs"));
    let bin = f
        .output
        .as_ref()
        .map(Path::new)
        .map(Path::to_path_buf)
        .unwrap_or_else(|| tmp.join(format!("kamisolari-{stem}")));
    fs::write(&rs, &rust).map_err(|e| format!("tmp: {e}"))?;
    vlog(&f, format!("rustc {} -> {}", rs.display(), bin.display()));

    let args = cli::rustc_args(&f, &rs, &bin);
    if f.verbose {
        eprintln!("rustc {}", args.join(" "));
    }
    let status = Command::new("rustc")
        .args(&args)
        .status()
        .map_err(|e| format!("no pude lanzar rustc: {e}"))?;
    if !status.success() {
        return Err("rustc fallo".into());
    }

    if matches!(
        f.crate_type.as_deref(),
        Some("lib" | "rlib" | "staticlib" | "dylib" | "cdylib")
    ) {
        if !f.quiet {
            eprintln!("compilo {}", bin.display());
        }
        return Ok(());
    }

    let status = Command::new(&bin)
        .status()
        .map_err(|e| format!("no pude ejecutar: {e}"))?;
    if !status.success() {
        process::exit(status.code().unwrap_or(1));
    }
    Ok(())
}

fn cmd_build(f: Flags) -> Result<(), String> {
    let dir = f.input.clone().unwrap_or_else(|| ".".into());
    let root = Path::new(&dir);
    if !root.join("Cargo.toml").exists() {
        return Err(format!("no hay Cargo.toml en {dir}"));
    }
    let tmp = std::env::temp_dir().join(format!(
        "kamisolari-build-{}",
        root.file_name()
            .and_then(|s| s.to_str())
            .unwrap_or("crate")
    ));
    let _ = fs::remove_dir_all(&tmp);
    traducir_arbol(root, &tmp, Direccion::ARust, f.opciones())?;
    vlog(&f, format!("cargo build en {}", tmp.display()));

    let mut cmd = Command::new("cargo");
    cmd.arg("build").current_dir(&tmp);
    if f.release {
        cmd.arg("--release");
    }
    if f.verbose {
        cmd.arg("-v");
    }
    if f.quiet {
        cmd.arg("-q");
    }
    let status = cmd.status().map_err(|e| format!("cargo: {e}"))?;
    if !status.success() {
        return Err("cargo build fallo".into());
    }
    if !f.quiet {
        eprintln!("compilo en {}", tmp.display());
    }
    Ok(())
}

fn traducir_arbol(src: &Path, dst: &Path, dir: Direccion, opt: Opciones) -> Result<(), String> {
    fs::create_dir_all(dst).map_err(|e| format!("{}: {e}", dst.display()))?;
    for ent in fs::read_dir(src).map_err(|e| format!("{}: {e}", src.display()))? {
        let ent = ent.map_err(|e| e.to_string())?;
        let name = ent.file_name();
        let name_s = name.to_string_lossy();
        if name_s == "target" || name_s == ".git" || name_s == ".kamisolari" {
            continue;
        }
        let from = ent.path();
        let to = dst.join(&name);
        if from.is_dir() {
            traducir_arbol(&from, &to, dir, opt)?;
            continue;
        }
        let ext = from.extension().and_then(|e| e.to_str());
        let is_rs = ext == Some("rs") || ext == Some("kami");
        if is_rs {
            let src = fs::read_to_string(&from).map_err(|e| format!("{}: {e}", from.display()))?;
            let body = if dir == Direccion::ARust && !es_archivo_espanol(&src) {
                src
            } else {
                traducir_con(&src, dir, opt)
            };
            let dest = if ext == Some("kami") {
                to.with_extension("rs")
            } else {
                to
            };
            if let Some(p) = dest.parent() {
                fs::create_dir_all(p).map_err(|e| e.to_string())?;
            }
            fs::write(&dest, body).map_err(|e| format!("{}: {e}", dest.display()))?;
        } else {
            if let Some(p) = to.parent() {
                fs::create_dir_all(p).map_err(|e| e.to_string())?;
            }
            fs::copy(&from, &to).map_err(|e| format!("copiar {}: {e}", from.display()))?;
        }
    }
    Ok(())
}

fn leer(path: &str) -> Result<String, String> {
    if path == "-" {
        let mut s = String::new();
        io::stdin()
            .read_to_string(&mut s)
            .map_err(|e| e.to_string())?;
        return Ok(s);
    }
    fs::read_to_string(path).map_err(|e| format!("no pude leer {path}: {e}"))
}
