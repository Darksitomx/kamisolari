//! Flags. Nombres en espanol; los ingleses siguen valiendo.

use kamisolari::translate::Opciones;

#[derive(Clone, Debug)]
pub struct Flags {
    pub cmd: String,
    pub input: Option<String>,
    pub output: Option<String>,
    pub edition: String,
    pub optimize: bool,
    pub debug: bool,
    pub verbose: bool,
    pub quiet: bool,
    pub todas: bool,
    pub release: bool,
    pub crate_type: Option<String>,
    pub crate_name: Option<String>,
    pub lib_paths: Vec<String>,
    pub codegen: Vec<String>,
    pub cfg: Vec<String>,
    pub rustc_extra: Vec<String>,
}

impl Flags {
    pub fn opciones(&self) -> Opciones {
        Opciones { todas: self.todas }
    }
}

pub fn parse(args: &[String]) -> Result<Flags, String> {
    let mut f = Flags {
        cmd: String::new(),
        input: None,
        output: None,
        edition: "2021".into(),
        optimize: false,
        debug: false,
        verbose: false,
        quiet: false,
        todas: false,
        release: false,
        crate_type: None,
        crate_name: None,
        lib_paths: Vec::new(),
        codegen: Vec::new(),
        cfg: Vec::new(),
        rustc_extra: Vec::new(),
    };

    let mut i = 0usize;
    let mut after_dashdash = false;
    while i < args.len() {
        let a = args[i].as_str();
        if after_dashdash {
            f.rustc_extra.push(args[i].clone());
            i += 1;
            continue;
        }
        match a {
            "--" => {
                after_dashdash = true;
                i += 1;
            }
            "-" => {
                if f.input.is_none() {
                    f.input = Some("-".into());
                } else {
                    return Err("argumento de mas: -".into());
                }
                i += 1;
            }
            "-h" | "--help" | "--ayuda" | "ayuda" => {
                f.cmd = "ayuda".into();
                return Ok(f);
            }
            "-V" | "--version" | "--ver" => {
                f.cmd = "version".into();
                return Ok(f);
            }
            "-o" | "--out" | "--output" | "--salida" => {
                f.output = Some(need(args, i, a)?);
                i += 2;
            }
            "-O" | "--optimizar" => {
                f.optimize = true;
                i += 1;
            }
            "-g" | "--depurar" => {
                f.debug = true;
                i += 1;
            }
            "-v" | "--verbose" | "--verboso" => {
                f.verbose = true;
                i += 1;
            }
            "-q" | "--quiet" | "--callado" => {
                f.quiet = true;
                i += 1;
            }
            "--release" | "--final" => {
                f.release = true;
                f.optimize = true;
                i += 1;
            }
            "--todas" => {
                f.todas = true;
                i += 1;
            }
            "--edition" | "--edicion" => {
                f.edition = need(args, i, a)?;
                i += 2;
            }
            "--crate-type" | "--tipo-crate" => {
                f.crate_type = Some(need(args, i, a)?);
                i += 2;
            }
            "--crate-name" | "--nombre-crate" => {
                f.crate_name = Some(need(args, i, a)?);
                i += 2;
            }
            "-L" | "--libs" => {
                f.lib_paths.push(need(args, i, a)?);
                i += 2;
            }
            "-C" | "--codegen" => {
                f.codegen.push(need(args, i, a)?);
                i += 2;
            }
            "--cfg" => {
                f.cfg.push(need(args, i, a)?);
                i += 2;
            }
            "es" | "rust" | "run" | "correr" | "build" | "construir" => {
                if f.cmd.is_empty() {
                    f.cmd = a.to_string();
                    i += 1;
                } else if f.input.is_none() {
                    f.input = Some(args[i].clone());
                    i += 1;
                } else {
                    return Err(format!("argumento de mas: {a}"));
                }
            }
            _ if a.starts_with('-') => {
                return Err(format!(
                    "flag desconocida '{a}'. kamisolari --ayuda  (o pasala despues de --)"
                ));
            }
            _ => {
                if f.cmd.is_empty() {
                    f.cmd = a.to_string();
                } else if f.input.is_none() {
                    f.input = Some(args[i].clone());
                } else {
                    return Err(format!("argumento de mas: {a}"));
                }
                i += 1;
            }
        }
    }
    Ok(f)
}

fn need(args: &[String], i: usize, flag: &str) -> Result<String, String> {
    args.get(i + 1)
        .cloned()
        .ok_or_else(|| format!("falta valor despues de {flag}"))
}

pub fn rustc_args(f: &Flags, input_rs: &std::path::Path, bin: &std::path::Path) -> Vec<String> {
    let mut a = vec![
        "--edition".into(),
        f.edition.clone(),
        "-o".into(),
        bin.display().to_string(),
    ];
    if f.optimize || f.release {
        a.push("-O".into());
    }
    if f.debug {
        a.push("-g".into());
    }
    if let Some(ct) = &f.crate_type {
        a.push("--crate-type".into());
        a.push(ct.clone());
    }
    if let Some(cn) = &f.crate_name {
        a.push("--crate-name".into());
        a.push(cn.clone());
    }
    for p in &f.lib_paths {
        a.push("-L".into());
        a.push(p.clone());
    }
    for c in &f.codegen {
        a.push("-C".into());
        a.push(c.clone());
    }
    for c in &f.cfg {
        a.push("--cfg".into());
        a.push(c.clone());
    }
    a.extend(f.rustc_extra.iter().cloned());
    a.push(input_rs.display().to_string());
    a
}
