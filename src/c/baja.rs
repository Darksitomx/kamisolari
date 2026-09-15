//! Bajador Rust → C: contexto, scopes y propiedad.
//!
//! - Pase 1a: registra nombres (tipos, funciones, consts).
//! - Pase 1b: resuelve tipos de campos, firmas y alias.
//! - Pase 2: baja cuerpos a C.
//! - Ensamblado: ordena tipos (topológico), prototipos y definiciones.
//!
//! Pase 1 en `pase1.rs`, sentencias en `stmt.rs`, expresiones en `expr.rs`,
//! patrones en `patron.rs` y ayudantes/tipos en `ayuda.rs`.

use std::collections::{HashMap, HashSet};

use super::errores::CError;
use super::tipos::{self, CType, CtxTipos};

// ---------------------------------------------------------------------------
// Tablas del pase 1
// ---------------------------------------------------------------------------

#[derive(Clone)]
pub(crate) struct CampoInfo {
    pub nombre: String,
    pub tipo: CType,
}

#[derive(Clone)]
pub(crate) struct InfoStruct {
    pub nombre: String,
    pub campos: Vec<CampoInfo>,
    pub es_union: bool,
    pub orden: usize,
    pub docs: Vec<String>,
}

#[derive(Clone)]
pub(crate) enum CamposVariante {
    Unit,
    Tuple(Vec<CType>),
    Struct(Vec<CampoInfo>),
}

#[derive(Clone)]
pub(crate) struct InfoVariante {
    pub nombre: String,
    pub campos: CamposVariante,
    /// Discriminante (`Rojo = 3`), si se escribió.
    pub disc: Option<syn::Expr>,
    /// ¿Trae `#[default]`? (para `Default` en enums)
    pub es_default: bool,
}

#[derive(Clone)]
pub(crate) struct InfoEnum {
    pub nombre: String,
    pub variantes: Vec<InfoVariante>,
    pub orden: usize,
    pub docs: Vec<String>,
}

#[derive(Clone, Copy, PartialEq)]
pub(crate) enum Receptor {
    Ninguno,
    Valor,
    Ref,
    Mut,
}

#[derive(Clone)]
pub(crate) struct MetodoRasgo {
    pub nombre: String,
    pub params: Vec<(String, CType)>,
    pub ret: CType,
    pub receptor: Receptor,
    pub defecto: Option<syn::Block>,
}

#[derive(Clone)]
pub(crate) struct InfoRasgo {
    pub nombre: String,
    pub metodos: Vec<MetodoRasgo>,
    pub tiene_estaticos: bool,
}

#[derive(Clone)]
pub(crate) struct Firma {
    pub params: Vec<(String, CType)>,
    pub ret: CType,
    pub variadic: bool,
}

#[derive(Clone)]
pub(crate) struct SigMetodo {
    pub nombre_c: String,
    pub params: Vec<(String, CType)>,
    pub ret: CType,
    pub receptor: Receptor,
}

#[derive(Clone)]
pub(crate) struct InfoConst {
    pub nombre_c: String,
    pub tipo: CType,
    /// Si es entero literal → `#define`.
    pub define: Option<String>,
    pub expr: Option<syn::Expr>,
}

// ---------------------------------------------------------------------------
// Scopes y variables
// ---------------------------------------------------------------------------

#[derive(Clone)]
pub(crate) struct VarInfo {
    pub nombre_c: String,
    pub tipo: CType,
    pub movida: bool,
    pub bandera: Option<String>,
    /// ¿Hay que liberarla al salir del scope? (dueña + droppable, no prestada)
    pub duena: bool,
}

#[derive(Clone)]
pub(crate) struct Alcance {
    pub vars: HashMap<String, VarInfo>,
    pub orden: Vec<String>,
    pub buf: Vec<String>,
}

#[derive(Clone)]
pub(crate) struct InfoBucle {
    pub etiqueta: Option<String>,
    pub id: usize,
    /// Índice del scope del cuerpo en `pila`.
    pub prof_cuerpo: usize,
    /// Temp para `loop` como expresión.
    pub valor: Option<(String, CType)>,
}

/// Destino del valor de un bloque / if / match.
#[derive(Clone)]
pub(crate) enum Destino {
    Descarta,
    /// Asignar a una variable ya declarada.
    Temp(String),
    /// Retornar de la función actual.
    Retorna,
}

// ---------------------------------------------------------------------------
// Bajador
// ---------------------------------------------------------------------------

pub(crate) struct Bajador {
    pub archivo: String,
    pub lineas: Vec<String>,
    pub mods: Vec<String>,
    pub structs: HashMap<String, InfoStruct>,
    pub enums: HashMap<String, InfoEnum>,
    pub rasgos: HashMap<String, InfoRasgo>,
    pub alias: HashMap<String, CType>,
    pub propio_set: HashSet<String>,
    pub fns: HashMap<String, Firma>,
    pub fn_externa: HashSet<String>,
    pub metodos: HashMap<(String, String), SigMetodo>,
    /// (rasgo, tipo) → método → firma.
    pub impl_rasgos: HashMap<(String, String), HashMap<String, SigMetodo>>,
    pub impl_drop: HashMap<String, syn::Block>,
    pub display: HashSet<String>,
    pub consts: HashMap<String, InfoConst>,
    pub consts_orden: Vec<String>,
    pub dyn_usados: HashSet<String>,
    pub monos: HashMap<String, CType>,
    pub monos_orden: Vec<String>,
    /// Ayudantes bajo demanda: (operación, tipo-mangle) → tipo.
    /// Operaciones: liberar/clona/iguales/defecto/depurar.
    pub ayuda: HashMap<(String, String), CType>,
    pub globales: HashSet<String>,
    pub anidados: Vec<(Vec<String>, syn::Item)>,
    // Buffers de salida.
    pub fwd: Vec<String>,
    pub consts_out: Vec<String>,
    pub protos: Vec<String>,
    pub defs: Vec<String>,
    pub estaticas: Vec<String>,
    // Estado de la función actual.
    pub pila: Vec<Alcance>,
    pub bucles: Vec<InfoBucle>,
    pub cond: usize,
    pub temp_base: HashMap<String, usize>,
    pub etiqueta_n: usize,
    pub fn_ret: CType,
    pub fn_nombre: String,
    pub tipo_self: Option<String>,
    pub en_display_fmt: bool,
    pub en_const: bool,
    pub usa_math: bool,
}

impl Bajador {
    pub fn nuevo(archivo: String, fuente: &str) -> Self {
        Self {
            archivo,
            lineas: fuente.lines().map(|s| s.to_string()).collect(),
            mods: Vec::new(),
            structs: HashMap::new(),
            enums: HashMap::new(),
            rasgos: HashMap::new(),
            alias: HashMap::new(),
            propio_set: HashSet::new(),
            fns: HashMap::new(),
            fn_externa: HashSet::new(),
            metodos: HashMap::new(),
            impl_rasgos: HashMap::new(),
            impl_drop: HashMap::new(),
            display: HashSet::new(),
            consts: HashMap::new(),
            consts_orden: Vec::new(),
            dyn_usados: HashSet::new(),
            monos: HashMap::new(),
            monos_orden: Vec::new(),
            ayuda: HashMap::new(),
            globales: HashSet::new(),
            anidados: Vec::new(),
            fwd: Vec::new(),
            consts_out: Vec::new(),
            protos: Vec::new(),
            defs: Vec::new(),
            estaticas: Vec::new(),
            pila: Vec::new(),
            bucles: Vec::new(),
            cond: 0,
            temp_base: HashMap::new(),
            etiqueta_n: 0,
            fn_ret: CType::Vacio,
            fn_nombre: String::new(),
            tipo_self: None,
            en_display_fmt: false,
            en_const: false,
            usa_math: false,
        }
    }

    pub fn ctx_tipos(&self) -> CtxTipos<'_> {
        CtxTipos {
            mods: &self.mods,
            propios: &self.propio_set,
            alias: &self.alias,
            tipo_self: self.tipo_self.as_deref(),
        }
    }

    /// Baja un tipo y registra monos / dyn usados.
    pub fn baja_tipo(&mut self, ty: &syn::Type) -> Result<CType, CError> {
        let mut t = tipos::baja_tipo(ty, &self.ctx_tipos(), &self.lineas)
            .map_err(|e| self.con_archivo(e))?;
        self.resuelve_dyn_en(&mut t)?;
        self.registra_uso(&t)?;
        Ok(t)
    }

    /// Resuelve nombres de rasgos en `Dyn` contra el módulo actual.
    fn resuelve_dyn_en(&self, t: &mut CType) -> Result<(), CError> {
        match t {
            CType::Dyn(r) => {
                *r = self.resuelve_nombre_rasgo(r);
                Ok(())
            }
            CType::Vec(x)
            | CType::Opcion(x)
            | CType::Caja(x)
            | CType::Rebana(x, _)
            | CType::Ref(_, x)
            | CType::Ptr(_, x) => self.resuelve_dyn_en(x),
            CType::Resultado(a, b) => {
                self.resuelve_dyn_en(a)?;
                self.resuelve_dyn_en(b)
            }
            CType::Tupla(v) => v.iter_mut().try_for_each(|x| self.resuelve_dyn_en(x)),
            CType::Arreglo(x, _) => self.resuelve_dyn_en(x),
            CType::FnPtr { params, ret } => {
                params.iter_mut().try_for_each(|x| self.resuelve_dyn_en(x))?;
                self.resuelve_dyn_en(ret)
            }
            _ => Ok(()),
        }
    }

    fn resuelve_nombre_rasgo(&self, r: &str) -> String {
        for k in (0..=self.mods.len()).rev() {
            let mut v: Vec<String> = self.mods[..k].to_vec();
            v.push(r.to_string());
            let c = v.join("__");
            if self.rasgos.contains_key(&c) || self.propio_set.contains(&c) {
                return c;
            }
        }
        r.to_string()
    }

    fn registra_uso(&mut self, t: &CType) -> Result<(), CError> {
        match t {
            CType::Vec(_)
            | CType::Opcion(_)
            | CType::Resultado(_, _)
            | CType::Tupla(_)
            | CType::Rebana(_, _) => {
                if matches!(t, CType::Tupla(v) if v.is_empty()) {
                    return Ok(());
                }
                let m = t.mangle().map_err(|m| CError::nuevo("C0002", m))?;
                if !self.monos.contains_key(&m) {
                    self.monos.insert(m.clone(), t.clone());
                    self.monos_orden.push(m);
                }
                for h in hijos_de(t) {
                    self.registra_uso(&h)?;
                }
                Ok(())
            }
            CType::Dyn(r) => {
                self.valida_dyn_objeto(r)?;
                self.dyn_usados.insert(r.clone());
                Ok(())
            }
            _ => {
                for h in hijos_de(t) {
                    self.registra_uso(&h)?;
                }
                Ok(())
            }
        }
    }

    /// Registra un tipo construido a mano (monos/vistas). Idempotente.
    /// Los tipos que pasan por `baja_tipo` ya vienen registrados; esto es
    /// para los que arma el pase 2 (`vec!`, `Ninguno`, tuplas, `?`, ...).
    pub(crate) fn registra_uso_pub(&mut self, t: &CType) -> Result<(), CError> {
        self.registra_uso(t)
    }

    /// Un rasgo usado como `din` debe ser objeto: sin estáticos ni `yo` por valor.
    fn valida_dyn_objeto(&self, r: &str) -> Result<(), CError> {
        let Some(info) = self.rasgos.get(r) else {
            return Ok(());
        };
        if info.tiene_estaticos {
            return Err(CError::nuevo(
                "C0003",
                format!("`{r}` tiene métodos sin `yo`: no sirve como objeto `din`"),
            )
            .con_archivo(self.archivo.clone())
            .ayuda("los objetos `din` solo usan métodos con receptor (`&yo`/`&mut yo`)"));
        }
        for m in &info.metodos {
            if m.receptor == Receptor::Valor {
                return Err(CError::nuevo(
                    "C0003",
                    format!(
                        "`{r}::{}` toma `yo` por valor: no sirve como objeto `din`",
                        m.nombre
                    ),
                )
                .con_archivo(self.archivo.clone())
                .ayuda("cambia el receptor a `&yo` o no uses ese rasgo como `din`"));
            }
        }
        Ok(())
    }

    pub fn con_archivo(&self, e: CError) -> CError {
        if e.archivo.is_some() {
            e
        } else {
            e.con_archivo(self.archivo.clone())
        }
    }

    pub fn falla<T>(&self, codigo: &'static str, mensaje: String) -> Result<T, CError> {
        Err(CError::nuevo(codigo, mensaje).con_archivo(self.archivo.clone()))
    }

    // ---- nombres globales ----

    /// Nombre C para un item en el módulo actual.
    pub fn nombre_global(&self, base: &str) -> String {
        let mut partes: Vec<String> = self.mods.clone();
        partes.push(base.to_string());
        partes
            .iter()
            .map(|p| tipos::higieniza(p))
            .collect::<Vec<_>>()
            .join("__")
    }

    /// Reserva un nombre global único (evita choques en el namespace único de C).
    pub fn reserva_global(&mut self, base: &str) -> String {
        let mut cand = base.to_string();
        let mut n = 1;
        while self.globales.contains(&cand) {
            n += 1;
            cand = format!("{base}_{n}");
        }
        self.globales.insert(cand.clone());
        cand
    }

    // ---- scopes ----

    pub fn entra(&mut self) {
        self.pila.push(Alcance {
            vars: HashMap::new(),
            orden: Vec::new(),
            buf: Vec::new(),
        });
    }

    /// Cierra el scope actual: devuelve sus líneas + limpieza al final.
    pub fn sale(&mut self) -> Vec<String> {
        let mut alc = self.pila.pop().expect("pila de scopes vacía (bug)");
        let mut out = std::mem::take(&mut alc.buf);
        out.extend(self.limpieza_de(&alc));
        out
    }

    /// Limpieza de un scope (orden inverso).
    fn limpieza_de(&mut self, alc: &Alcance) -> Vec<String> {
        let vars: Vec<(String, CType, bool, Option<String>, bool)> = alc
            .orden
            .iter()
            .rev()
            .map(|n| {
                let v = &alc.vars[n];
                (
                    v.nombre_c.clone(),
                    v.tipo.clone(),
                    v.movida,
                    v.bandera.clone(),
                    v.duena,
                )
            })
            .collect();
        self.limpieza_vars(&vars)
    }

    fn limpieza_vars(
        &mut self,
        vars: &[(String, CType, bool, Option<String>, bool)],
    ) -> Vec<String> {
        let mut out = Vec::new();
        for (nc, tipo, movida, bandera, duena) in vars {
            if !duena || *movida {
                continue;
            }
            let st = self.libera_lugar(nc, tipo);
            if st.is_empty() {
                continue;
            }
            if let Some(b) = bandera {
                out.push(format!("if (!{b}) {{"));
                for s in st {
                    out.push(format!("    {s}"));
                }
                out.push("}".into());
            } else {
                out.extend(st);
            }
        }
        out
    }

    /// Limpieza de los scopes con índice >= `desde` (para return/break/continue).
    pub fn limpieza_hasta(&mut self, desde: usize) -> Vec<String> {
        let mut out = Vec::new();
        for i in (desde..self.pila.len()).rev() {
            let vars: Vec<(String, CType, bool, Option<String>, bool)> = self.pila[i]
                .orden
                .iter()
                .rev()
                .map(|n| {
                    let v = &self.pila[i].vars[n];
                    (
                        v.nombre_c.clone(),
                        v.tipo.clone(),
                        v.movida,
                        v.bandera.clone(),
                        v.duena,
                    )
                })
                .collect();
            out.extend(self.limpieza_vars(&vars));
        }
        out
    }

    pub fn emite(&mut self, linea: impl Into<String>) {
        let top = self.pila.len() - 1;
        self.pila[top].buf.push(linea.into());
    }

    pub fn emite_todas(&mut self, lineas: Vec<String>) {
        let top = self.pila.len() - 1;
        self.pila[top].buf.extend(lineas);
    }

    /// Halla una variable por nombre Rust o nombre C (los temporales se
    /// conocen por su nombre C: `__t0` vive como `__t0_`).
    pub fn clave_de(&self, nombre: &str) -> Option<(usize, String)> {
        for (i, alc) in self.pila.iter().enumerate().rev() {
            if alc.vars.contains_key(nombre) {
                return Some((i, nombre.to_string()));
            }
            if let Some((k, _)) = alc.vars.iter().find(|(_, v)| v.nombre_c == nombre) {
                return Some((i, k.clone()));
            }
        }
        None
    }

    /// ¿`c` nombra una variable/temporal dueña? Un uso por valor así MUEVE
    /// (misma búsqueda que `marca_movida_si_temp`, sin marcar).
    pub fn es_dueno_nombrado(&self, c: &str) -> bool {
        self.clave_de(c)
            .and_then(|(i, k)| self.pila[i].vars.get(&k).map(|v| v.duena))
            .unwrap_or(false)
    }

    pub fn busca(&self, nombre: &str) -> Option<(usize, &VarInfo)> {
        for (i, alc) in self.pila.iter().enumerate().rev() {
            if let Some(v) = alc.vars.get(nombre) {
                return Some((i, v));
            }
        }
        None
    }

    /// Declara una variable en el scope actual (renombra si hay sombra en el MISMO scope).
    pub fn declara(&mut self, nombre: &str, tipo: CType, duena: bool) -> String {
        let top = self.pila.len() - 1;
        let mut cand = tipos::higieniza(nombre);
        if self.pila[top].vars.contains_key(&cand)
            || self.pila[top].vars.values().any(|v| v.nombre_c == cand)
        {
            let mut n = 1;
            while self.pila[top].vars.values().any(|v| v.nombre_c == format!("{cand}_{n}"))
                || self.pila[top].vars.contains_key(&format!("{nombre}_{n}"))
            {
                n += 1;
            }
            cand = format!("{cand}_{n}");
        }
        self.pila[top].vars.insert(
            nombre.to_string(),
            VarInfo {
                nombre_c: cand.clone(),
                tipo,
                movida: false,
                bandera: None,
                duena,
            },
        );
        self.pila[top].orden.push(nombre.to_string());
        cand
    }

    /// Marca una variable como movida. Si el movimiento es condicional, crea bandera.
    pub fn marca_movida(&mut self, nombre: &str) {
        let Some((i, clave)) = self.clave_de(nombre) else { return };
        let cond = self.cond;
        let v = self.pila[i].vars.get_mut(&clave).unwrap();
        v.movida = true;
        if cond > 0 && v.duena && v.bandera.is_none() {
            let b = format!("movido_{}_{}", tipos::higieniza(&clave), i);
            v.bandera = Some(b.clone());
            // La bandera vive en el scope dueño. Como el contenido del
            // bloque anidado aún no se ha vaciado en el dueño, agregarla
            // al final del buf del dueño la deja antes del bloque. Correcto.
            self.pila[i].buf.push(format!("bool {b} = false;"));
        }
    }

    /// Activa la bandera de una variable movida condicionalmente.
    pub fn activa_bandera(&mut self, nombre: &str) {
        let b = self
            .clave_de(nombre)
            .and_then(|(i, k)| self.pila[i].vars.get(&k).and_then(|v| v.bandera.clone()));
        if let Some(b) = b {
            self.emite(format!("{b} = true;"));
        }
    }

    /// Temporal nuevo en el scope actual, declarado con cero si es droppable.
    /// El nombre describe el contenido (`texto0`, `persona1`) con contador
    /// por base: único en todo el archivo.
    pub fn temp(&mut self, tipo: CType) -> Result<String, CError> {
        let base = tipo.pista_temp();
        let n = self.temp_base.get(&base).copied().unwrap_or(0);
        self.temp_base.insert(base.clone(), n + 1);
        let nombre = format!("{base}{n}");
        let duena = self.necesita_drop(&tipo);
        let nc = self.declara(&nombre, tipo.clone(), duena);
        let decl = tipo.declara(&nc)?;
        if duena {
            self.emite(format!("{decl} = {0};", tipo.cero()?));
        } else {
            self.emite(format!("{decl};"));
        }
        Ok(nc)
    }

    pub fn necesita_drop(&self, t: &CType) -> bool {
        let structs = &self.structs;
        let enums = &self.enums;
        let visitados = std::cell::RefCell::new(HashSet::new());
        fn usuario(
            n: &str,
            structs: &HashMap<String, InfoStruct>,
            enums: &HashMap<String, InfoEnum>,
            vis: &std::cell::RefCell<HashSet<String>>,
        ) -> bool {
            if !vis.borrow_mut().insert(n.to_string()) {
                return false;
            }
            let r = if let Some(s) = structs.get(n) {
                s.campos
                    .iter()
                    .any(|c| c.tipo.necesita_drop(&|m| usuario(m, structs, enums, vis)))
            } else if let Some(e) = enums.get(n) {
                e.variantes.iter().any(|v| match &v.campos {
                    super::baja::CamposVariante::Unit => false,
                    super::baja::CamposVariante::Tuple(ts) => {
                        ts.iter().any(|t| t.necesita_drop(&|m| usuario(m, structs, enums, vis)))
                    }
                    super::baja::CamposVariante::Struct(cs) => cs
                        .iter()
                        .any(|c| c.tipo.necesita_drop(&|m| usuario(m, structs, enums, vis))),
                })
            } else {
                false
            };
            vis.borrow_mut().remove(n);
            r
        }
        t.necesita_drop(&|n| usuario(n, structs, enums, &visitados))
    }
}

fn hijos_de(t: &CType) -> Vec<CType> {
    match t {
        CType::Vec(x)
        | CType::Opcion(x)
        | CType::Caja(x)
        | CType::Rebana(x, _)
        | CType::Ref(_, x)
        | CType::Ptr(_, x) => vec![(**x).clone()],
        CType::Resultado(a, b) => vec![(**a).clone(), (**b).clone()],
        CType::Tupla(v) => v.clone(),
        CType::Arreglo(x, _) => vec![(**x).clone()],
        CType::FnPtr { params, ret } => {
            let mut v = params.clone();
            v.push((**ret).clone());
            v
        }
        _ => vec![],
    }
}

/// Sustituye `Usuario(rasgo)` (= `Self` en firmas de rasgo) por el tipo concreto.
pub(crate) fn sustituye_self(t: &CType, rasgo: &str, concreto: &str) -> CType {
    match t {
        CType::Usuario(n) if n == rasgo => CType::Usuario(concreto.to_string()),
        CType::Vec(x) => CType::Vec(sustituye_self(x, rasgo, concreto).into()),
        CType::Opcion(x) => CType::Opcion(sustituye_self(x, rasgo, concreto).into()),
        CType::Resultado(a, b) => CType::Resultado(
            sustituye_self(a, rasgo, concreto).into(),
            sustituye_self(b, rasgo, concreto).into(),
        ),
        CType::Tupla(v) => {
            CType::Tupla(v.iter().map(|x| sustituye_self(x, rasgo, concreto)).collect())
        }
        CType::Arreglo(x, l) => CType::Arreglo(sustituye_self(x, rasgo, concreto).into(), l.clone()),
        CType::Rebana(x, m) => CType::Rebana(sustituye_self(x, rasgo, concreto).into(), *m),
        CType::Ref(m, x) => CType::Ref(*m, sustituye_self(x, rasgo, concreto).into()),
        CType::Ptr(m, x) => CType::Ptr(*m, sustituye_self(x, rasgo, concreto).into()),
        CType::Caja(x) => CType::Caja(sustituye_self(x, rasgo, concreto).into()),
        CType::FnPtr { params, ret } => CType::FnPtr {
            params: params.iter().map(|x| sustituye_self(x, rasgo, concreto)).collect(),
            ret: sustituye_self(ret, rasgo, concreto).into(),
        },
        _ => t.clone(),
    }
}

/// Firma Rust para comentarios (`fn saludo(&self) -> String`).
/// `receptor` arma la parte `self`; `None` = función libre.
pub(crate) fn firma_rust(
    nombre: &str,
    params: &[(String, CType)],
    ret: &CType,
    receptor: Option<&Receptor>,
) -> String {
    let mut ps = Vec::new();
    if let Some(r) = receptor {
        match r {
            Receptor::Ninguno => {}
            Receptor::Valor => ps.push("self".to_string()),
            Receptor::Ref => ps.push("&self".to_string()),
            Receptor::Mut => ps.push("&mut self".to_string()),
        }
    }
    for (n, t) in params {
        ps.push(format!("{n}: {}", t.a_rust()));
    }
    let mut s = format!("fn {}({})", nombre, ps.join(", "));
    if !matches!(ret, CType::Vacio) {
        s.push_str(&format!(" -> {}", ret.a_rust()));
    }
    s
}
