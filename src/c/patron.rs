//! Pase 2: patrones (`sea`, `si sea`, `match`, `para`, `mientras sea`).
//!
//! - `analiza_patron`: patrón → (condición C, enlaces). No emite.
//! - `emite_enlaces`: declara cada enlace (mueve de temporales pelados,
//!   clona hondo de lugares con dueño).
//! - `enlaza`: `sea PAT = valor` completo (arreglos directos incluidos).

use super::baja::{Bajador, CampoInfo};
use std::collections::HashSet;
use super::errores::CError;
use super::expr::ExVal;
use super::tipos::{self, CType};

/// Enlace `patrón → variable C` pendiente de emitir.
#[derive(Debug, Clone)]
pub struct Enlace {
    /// Nombre kamisolari de la variable.
    pub clave: String,
    /// Tipo de la variable.
    pub tipo: CType,
    /// Plantilla de inicio (`{nc}` = nombre C final; casi siempre ausente).
    pub plantilla: String,
    /// Lugar analizado (para clones con dueño).
    pub lugar: String,
}

/// Constructor resuelto en un patrón.
enum CtorPat {
    Some,
    Ok,
    Err,
    Variante { nc: String, var: String },
    Struct { nc: String, campos: Vec<CampoInfo>, es_union: bool },
}

impl Bajador {
    /// Analiza un patrón contra un lugar ya evaluado: (condición, enlaces).
    /// La condición es `"true"` si el patrón es irrefutable.
    pub(crate) fn analiza_patron(
        &mut self,
        pat: &syn::Pat,
        tipo: &CType,
        lugar: &str,
    ) -> Result<(String, Vec<Enlace>), CError> {
        match pat {
            syn::Pat::Wild(_) | syn::Pat::Rest(_) => Ok(("true".into(), Vec::new())),
            syn::Pat::Paren(x) => self.analiza_patron(&x.pat, tipo, lugar),
            syn::Pat::Ident(x) => self.pat_ident(x, tipo, lugar),
            syn::Pat::Tuple(x) => self.pat_tupla(x, tipo, lugar),
            syn::Pat::TupleStruct(x) => self.pat_ctor_tupla(x, tipo, lugar),
            syn::Pat::Struct(x) => self.pat_struct(x, tipo, lugar),
            syn::Pat::Path(x) => self.pat_solo(x, tipo, lugar),
            syn::Pat::Lit(x) => {
                let e = syn::Expr::Lit(syn::ExprLit { attrs: Vec::new(), lit: x.lit.clone() });
                self.pat_lit_expr(&e, tipo, lugar, pat)
            }
            syn::Pat::Range(x) => self.pat_rango(x, tipo, lugar),
            syn::Pat::Reference(x) => {
                let inner = match tipo {
                    CType::Ref(_, i) => i.as_ref().clone(),
                    _ => {
                        return Err(self.err_en(
                            "C0005",
                            format!("ese patrón pide referencia, llegó `{}`", self.muestra(tipo)),
                            pat,
                        ))
                    }
                };
                self.analiza_patron(&x.pat, &inner, &format!("(*({lugar}))"))
            }
            syn::Pat::Slice(x) => self.pat_rebana(x, tipo, lugar),
            syn::Pat::Type(x) => {
                let t = self.baja_tipo(&x.ty)?;
                if t != *tipo {
                    return Err(self.err_en(
                        "C0005",
                        format!(
                            "el patrón anota `{}`, llegó `{}`",
                            self.muestra(&t),
                            self.muestra(tipo)
                        ),
                        pat,
                    ));
                }
                self.analiza_patron(&x.pat, tipo, lugar)
            }
            syn::Pat::Or(_) => Err(self.err_en(
                "C0003",
                "`A | B` solo vale directo en brazos de `match`".into(),
                pat,
            )
            .ayuda("separa los brazos o usa guardias")),
            syn::Pat::Macro(_) | syn::Pat::Verbatim(_) | _ => {
                Err(self.err_en("C0003", "ese patrón no cabe".into(), pat))
            }
        }
    }

    /// `x` / `mut x` / `ref x` / `x @ sub`.
    fn pat_ident(
        &mut self,
        x: &syn::PatIdent,
        tipo: &CType,
        lugar: &str,
    ) -> Result<(String, Vec<Enlace>), CError> {
        if matches!(tipo, CType::Infer) {
            return Err(self.err_en("C0005", "el patrón necesita tipo (anota)".into(), x));
        }
        // `mut` se ignora (sin rastro de mutabilidad: leniencia).
        let nombre = tipos::desnuda(&x.ident);
        // Identificadores que resuelven a valores (el prelude los trae).
        if x.subpat.is_none() && x.by_ref.is_none() {
            if nombre == "None" {
                if !matches!(tipo, CType::Opcion(_)) {
                    return Err(self.err_en(
                        "C0005",
                        format!("ese patrón pide `Option`, llegó `{}`", self.muestra(tipo)),
                        x,
                    ));
                }
                return Ok((format!("(!(({lugar}).tiene))"), Vec::new()));
            }
            if nombre == "true" || nombre == "false" {
                if *tipo != CType::Bool {
                    return Err(self.err_en(
                        "C0005",
                        format!("ese patrón pide `bool`, llegó `{}`", self.muestra(tipo)),
                        x,
                    ));
                }
                if nombre == "true" {
                    return Ok((format!("({lugar})"), Vec::new()));
                }
                return Ok((format!("(!({lugar}))"), Vec::new()));
            }
        }
        let mut enlaces = Vec::new();
        if x.by_ref.is_some() {
            let es_mut = x.mutability.is_some();
            enlaces.push(Enlace {
                clave: nombre,
                tipo: CType::Ref(es_mut, Box::new(tipo.clone())),
                plantilla: format!("&({lugar})"),
                lugar: lugar.to_string(),
            });
        } else {
            enlaces.push(Enlace {
                clave: nombre,
                tipo: tipo.clone(),
                plantilla: lugar.to_string(),
                lugar: lugar.to_string(),
            });
        }
        if let Some((_, sub)) = &x.subpat {
            let (c2, mut e2) = self.analiza_patron(sub, tipo, lugar)?;
            enlaces.append(&mut e2);
            return Ok((c2, enlaces));
        }
        Ok(("true".into(), enlaces))
    }

    /// Parte `a, .., z`: (delante, detrás, hay `..`). Un solo `..` vale.
    fn parte_puntos<S: syn::spanned::Spanned>(
        &self,
        elems: &syn::punctuated::Punctuated<syn::Pat, syn::token::Comma>,
        sp: S,
    ) -> Result<(Vec<syn::Pat>, Vec<syn::Pat>, bool), CError> {
        let mut delante = Vec::new();
        let mut detras = Vec::new();
        let mut visto = false;
        let mut doble = false;
        for e in elems {
            if matches!(e, syn::Pat::Rest(_)) {
                if visto {
                    doble = true;
                }
                visto = true;
                continue;
            }
            if visto {
                detras.push(e.clone());
            } else {
                delante.push(e.clone());
            }
        }
        if doble {
            return Err(self.err_en("C0005", "un solo `..` por patrón".into(), sp));
        }
        Ok((delante, detras, visto))
    }

    /// `(a, b)`: tupla exacta (con `..` vale prefijo+sufijo).
    fn pat_tupla(
        &mut self,
        x: &syn::PatTuple,
        tipo: &CType,
        lugar: &str,
    ) -> Result<(String, Vec<Enlace>), CError> {
        let ts = match tipo {
            CType::Tupla(v) => v.clone(),
            _ => {
                return Err(self.err_en(
                    "C0005",
                    format!("ese patrón pide tupla, llegó `{}`", self.muestra(tipo)),
                    x,
                ))
            }
        };
        let (del, det, puntos) = self.parte_puntos(&x.elems, x)?;
        if !puntos && del.len() != ts.len() {
            return Err(self.err_en(
                "C0005",
                format!("la tupla trae {} campos, el patrón {}", ts.len(), del.len()),
                x,
            ));
        }
        if puntos && del.len() + det.len() > ts.len() {
            return Err(self.err_en(
                "C0005",
                format!("la tupla trae {} campos, sobran patrones", ts.len()),
                x,
            ));
        }
        let mut conds = Vec::new();
        let mut enlaces = Vec::new();
        for (i, p) in del.iter().enumerate() {
            let (c, mut e) = self.analiza_patron(p, &ts[i], &format!("(({lugar})._{i})"))?;
            if c != "true" {
                conds.push(format!("({c})"));
            }
            enlaces.append(&mut e);
        }
        for (j, p) in det.iter().enumerate() {
            let i = ts.len() - det.len() + j;
            let (c, mut e) = self.analiza_patron(p, &ts[i], &format!("(({lugar})._{i})"))?;
            if c != "true" {
                conds.push(format!("({c})"));
            }
            enlaces.append(&mut e);
        }
        Ok((Self::y_conds(conds), enlaces))
    }

    /// Une condiciones con `&&` (`"true"` si vacías).
    fn y_conds(conds: Vec<String>) -> String {
        if conds.is_empty() {
            "true".into()
        } else {
            conds.join(" && ")
        }
    }

    /// Resuelve `Some` / `E::Var` / `S` en un patrón (sin turbofish).
    fn resuelve_ctor(&self, path: &syn::Path, sp: &syn::Pat) -> Result<CtorPat, CError> {
        if path.segments.iter().any(|s| !matches!(s.arguments, syn::PathArguments::None)) {
            return Err(self.err_en("C0003", "patrones sin genéricos".into(), sp)
                .ayuda("quita el `::<...>`"));
        }
        let segs: Vec<String> =
            path.segments.iter().map(|s| tipos::desnuda(&s.ident)).collect();
        if segs == ["Some"] || segs == ["Option", "Some"] {
            return Ok(CtorPat::Some);
        }
        if segs == ["Ok"] || segs == ["Result", "Ok"] {
            return Ok(CtorPat::Ok);
        }
        if segs == ["Err"] || segs == ["Result", "Err"] {
            return Ok(CtorPat::Err);
        }
        let raiz = path.leading_colon.is_some();
        if let Some(nc) = self.resuelve_tipo_pub(&segs, raiz) {
            if let Some(s) = self.structs.get(&nc) {
                return Ok(CtorPat::Struct {
                    nc,
                    campos: s.campos.clone(),
                    es_union: s.es_union,
                });
            }
            return Err(self.err_en(
                "C0004",
                format!("`{}` es enum: falta la variante", segs.join("::")),
                sp,
            ));
        }
        if segs.len() >= 2 {
            let nt = segs.len() - 1;
            if let Some(nc) = self.resuelve_tipo_pub(&segs[..nt], raiz) {
                if self.enums.contains_key(&nc) {
                    return Ok(CtorPat::Variante { nc, var: segs[nt].clone() });
                }
            }
        }
        // Variante pelada (`use E::*`).
        if segs.len() == 1 {
            let cands = self.busca_variante_pelada(&segs[0]);
            if cands.len() == 1 {
                return Ok(CtorPat::Variante { nc: cands[0].0.clone(), var: segs[0].clone() });
            }
            if cands.len() > 1 {
                return Err(self.err_en(
                    "C0003",
                    format!("`{}` es variante de varios", segs[0]),
                    sp,
                )
                .ayuda("califica con el enum"));
            }
        }
        Err(self.err_en("C0004", format!("patrón `{}` desconocido", segs.join("::")), sp))
    }
}

impl Bajador {
    /// `Some(a)` / `E::Var(a, b)` / `Punto(a, b)`: ctor posicional.
    fn pat_ctor_tupla(
        &mut self,
        x: &syn::PatTupleStruct,
        tipo: &CType,
        lugar: &str,
    ) -> Result<(String, Vec<Enlace>), CError> {
        let ctor = self.resuelve_ctor(&x.path, &syn::Pat::TupleStruct(x.clone()))?;
        // `Some`/`Ok`/`Err` guardan el valor directo (sin `._0`).
        let directo = matches!(ctor, CtorPat::Some | CtorPat::Ok | CtorPat::Err);
        let (cond_base, n_campos, tipos_sub, sitio): (String, usize, Vec<CType>, String) = match ctor {
            CtorPat::Some => {
                let i = match tipo {
                    CType::Opcion(v) => v.as_ref().clone(),
                    _ => {
                        return Err(self.err_en(
                            "C0005",
                            format!("ese patrón pide `Option`, llegó `{}`", self.muestra(tipo)),
                            x,
                        ))
                    }
                };
                (format!("(({lugar}).tiene)"), 1, vec![i], format!("(({lugar}).valor)"))
            }
            CtorPat::Ok => {
                let o = match tipo {
                    CType::Resultado(v, _) => v.as_ref().clone(),
                    _ => {
                        return Err(self.err_en(
                            "C0005",
                            format!("ese patrón pide `Result`, llegó `{}`", self.muestra(tipo)),
                            x,
                        ))
                    }
                };
                (format!("(({lugar}).es_ok)"), 1, vec![o], format!("(({lugar}).datos.ok)"))
            }
            CtorPat::Err => {
                let e = match tipo {
                    CType::Resultado(_, v) => v.as_ref().clone(),
                    _ => {
                        return Err(self.err_en(
                            "C0005",
                            format!("ese patrón pide `Result`, llegó `{}`", self.muestra(tipo)),
                            x,
                        ))
                    }
                };
                (format!("(!(({lugar}).es_ok))"), 1, vec![e], format!("(({lugar}).datos.err)"))
            }
            CtorPat::Variante { nc, var } => {
                if tipo != &CType::Usuario(nc.clone()) {
                    return Err(self.err_en(
                        "C0005",
                        format!("ese patrón pide `{nc}`, llegó `{}`", self.muestra(tipo)),
                        x,
                    ));
                }
                let e = self.enums.get(&nc).cloned().unwrap();
                let v = e.variantes.iter().find(|v| v.nombre == var).cloned().unwrap();
                let ts = match &v.campos {
                    super::baja::CamposVariante::Tuple(ts) => ts.clone(),
                    super::baja::CamposVariante::Unit => {
                        return Err(self.err_en("C0003", format!("`{var}` es un valor (sin paréntesis)"), x))
                    }
                    super::baja::CamposVariante::Struct(_) => {
                        return Err(self.err_en("C0003", format!("`{var}` se abre con llaves"), x))
                    }
                };
                let n = ts.len();
                (format!("((({lugar}).etiqueta) == ({nc}_{var}))"), n, ts, format!("(({lugar}).datos.{var})"))
            }
            CtorPat::Struct { nc, campos, es_union } => {
                if tipo != &CType::Usuario(nc.clone()) {
                    return Err(self.err_en(
                        "C0005",
                        format!("ese patrón pide `{nc}`, llegó `{}`", self.muestra(tipo)),
                        x,
                    ));
                }
                if es_union {
                    return Err(self.err_en("C0003", "las uniones se abren con llaves".into(), x));
                }
                if !campos.iter().all(|c| c.nombre.starts_with('_')) {
                    return Err(self.err_en("C0003", format!("`{nc}` se abre con llaves"), x));
                }
                let ts: Vec<CType> = campos.iter().map(|c| c.tipo.clone()).collect();
                let n = ts.len();
                ("true".into(), n, ts, format!("({lugar})"))
            }
        };
        let (del, det, puntos) = self.parte_puntos(&x.elems, x)?;
        if !puntos && del.len() != n_campos {
            return Err(self.err_en(
                "C0005",
                format!("el ctor trae {n_campos} campos, el patrón {}", del.len()),
                x,
            ));
        }
        if puntos && del.len() + det.len() > n_campos {
            return Err(self.err_en("C0005", "sobran patrones para los campos".into(), x));
        }
        let mut conds = Vec::new();
        if cond_base != "true" {
            conds.push(format!("({cond_base})"));
        }
        let mut enlaces = Vec::new();
        for (i, p) in del.iter().enumerate() {
            let sp = if directo { sitio.clone() } else { format!("{sitio}._{i}") };
            let (c, mut e) = self.analiza_patron(p, &tipos_sub[i], &sp)?;
            if c != "true" {
                conds.push(format!("({c})"));
            }
            enlaces.append(&mut e);
        }
        for (j, p) in det.iter().enumerate() {
            let i = n_campos - det.len() + j;
            let sp = if directo { sitio.clone() } else { format!("{sitio}._{i}") };
            let (c, mut e) = self.analiza_patron(p, &tipos_sub[i], &sp)?;
            if c != "true" {
                conds.push(format!("({c})"));
            }
            enlaces.append(&mut e);
        }
        Ok((Self::y_conds(conds), enlaces))
    }

    /// `S { x, y }` / `E::V { x }`: struct o variante con campos.
    fn pat_struct(
        &mut self,
        x: &syn::PatStruct,
        tipo: &CType,
        lugar: &str,
    ) -> Result<(String, Vec<Enlace>), CError> {
        let ctor = self.resuelve_ctor(&x.path, &syn::Pat::Struct(x.clone()))?;
        let (cond_base, nc, campos, var): (String, String, Vec<CampoInfo>, Option<String>) = match ctor {
            CtorPat::Struct { nc, campos, .. } => {
                if tipo != &CType::Usuario(nc.clone()) {
                    return Err(self.err_en(
                        "C0005",
                        format!("ese patrón pide `{nc}`, llegó `{}`", self.muestra(tipo)),
                        x,
                    ));
                }
                if !campos.is_empty() && campos.iter().all(|c| c.nombre.starts_with('_')) {
                    return Err(self.err_en("C0003", format!("`{nc}` se abre con paréntesis"), x));
                }
                ("true".into(), nc, campos, None)
            }
            CtorPat::Variante { nc, var } => {
                if tipo != &CType::Usuario(nc.clone()) {
                    return Err(self.err_en(
                        "C0005",
                        format!("ese patrón pide `{nc}`, llegó `{}`", self.muestra(tipo)),
                        x,
                    ));
                }
                let e = self.enums.get(&nc).cloned().unwrap();
                let v = e.variantes.iter().find(|v| v.nombre == var).cloned().unwrap();
                let cs = match &v.campos {
                    super::baja::CamposVariante::Struct(cs) => cs.clone(),
                    super::baja::CamposVariante::Unit => {
                        return Err(self.err_en("C0003", format!("`{var}` es un valor (sin llaves)"), x))
                    }
                    super::baja::CamposVariante::Tuple(_) => {
                        return Err(self.err_en("C0003", format!("`{var}` se abre con paréntesis"), x))
                    }
                };
                (format!("((({lugar}).etiqueta) == ({nc}_{var}))"), nc, cs, Some(var))
            }
            _ => {
                return Err(self.err_en("C0003", "ese ctor no usa llaves".into(), x));
            }
        };
        let mut vistos = HashSet::new();
        let mut conds = Vec::new();
        if cond_base != "true" {
            conds.push(format!("({cond_base})"));
        }
        let mut enlaces = Vec::new();
        for f in &x.fields {
            let q = match &f.member {
                syn::Member::Named(id) => tipos::desnuda(id),
                syn::Member::Unnamed(idx) => format!("_{}", idx.index),
            };
            if !vistos.insert(q.clone()) {
                return Err(self.err_en("C0005", format!("campo `{q}` repetido"), &f.member));
            }
            let fc = match campos.iter().find(|c| c.nombre == q) {
                Some(fc) => fc.clone(),
                None => {
                    return Err(self.err_en("C0004", format!("`{nc}` no tiene campo `{q}`"), &f.member))
                }
            };
            let sub: syn::Pat = (*f.pat).clone();
            let sitio = match &var {
                Some(vv) => format!("(({lugar}).datos.{vv}.{})", tipos::higieniza(&q)),
                None => format!("(({lugar}).{})", tipos::higieniza(&q)),
            };
            let (c, mut e) = self.analiza_patron(&sub, &fc.tipo, &sitio)?;
            if c != "true" {
                conds.push(format!("({c})"));
            }
            enlaces.append(&mut e);
        }
        if x.rest.is_none() {
            let falt: Vec<String> =
                campos.iter().filter(|c| !vistos.contains(&c.nombre)).map(|c| c.nombre.clone()).collect();
            if !falt.is_empty() {
                return Err(self.err_en("C0005", format!("faltan campos: {}", falt.join(", ")), x)
                    .ayuda("nómbralos todos o agrega `..`"));
            }
        }
        Ok((Self::y_conds(conds), enlaces))
    }

    /// `None` / `true` / `E::V` / `S` / `N`: patrón de un solo camino.
    fn pat_solo(
        &mut self,
        x: &syn::PatPath,
        tipo: &CType,
        lugar: &str,
    ) -> Result<(String, Vec<Enlace>), CError> {
        if x.qself.is_some() {
            return Err(self.err_en("C0003", "ese patrón no cabe".into(), x));
        }
        if x.path.segments.iter().any(|s| !matches!(s.arguments, syn::PathArguments::None)) {
            return Err(self.err_en("C0003", "patrones sin genéricos".into(), x));
        }
        let segs: Vec<String> =
            x.path.segments.iter().map(|s| tipos::desnuda(&s.ident)).collect();
        let raiz = x.path.leading_colon.is_some();
        if segs == ["None"] || segs == ["Option", "None"] {
            if !matches!(tipo, CType::Opcion(_)) {
                return Err(self.err_en(
                    "C0005",
                    format!("ese patrón pide `Option`, llegó `{}`", self.muestra(tipo)),
                    x,
                ));
            }
            return Ok((format!("(!(({lugar}).tiene))"), Vec::new()));
        }
        if segs == ["true"] || segs == ["false"] {
            if *tipo != CType::Bool {
                return Err(self.err_en(
                    "C0005",
                    format!("ese patrón pide `bool`, llegó `{}`", self.muestra(tipo)),
                    x,
                ));
            }
            if segs == ["true"] {
                return Ok((format!("({lugar})"), Vec::new()));
            }
            return Ok((format!("(!({lugar}))"), Vec::new()));
        }
        // Variante unitaria (calificada o pelada).
        let mut var_unit: Option<(String, String)> = None;
        if segs.len() >= 2 {
            let nt = segs.len() - 1;
            if let Some(nc) = self.resuelve_tipo_pub(&segs[..nt], raiz) {
                if let Some(e) = self.enums.get(&nc) {
                    if e.variantes.iter().any(|v| v.nombre == segs[nt]) {
                        var_unit = Some((nc, segs[nt].clone()));
                    }
                }
            }
        } else if segs.len() == 1 {
            let cands = self.busca_variante_pelada(&segs[0]);
            if cands.len() == 1 {
                var_unit = Some((cands[0].0.clone(), segs[0].clone()));
            } else if cands.len() > 1 {
                return Err(self.err_en(
                    "C0003",
                    format!("`{}` es variante de varios", segs[0]),
                    x,
                )
                .ayuda("califica con el enum"));
            }
        }
        if let Some((nc, var)) = var_unit {
            if tipo != &CType::Usuario(nc.clone()) {
                return Err(self.err_en(
                    "C0005",
                    format!("ese patrón pide `{nc}`, llegó `{}`", self.muestra(tipo)),
                    x,
                ));
            }
            let e = self.enums.get(&nc).cloned().unwrap();
            let v = e.variantes.iter().find(|v| v.nombre == var).cloned().unwrap();
            if !matches!(v.campos, super::baja::CamposVariante::Unit) {
                return Err(self.err_en("C0003", format!("`{var}` lleva campos"), x));
            }
            let con_datos = e.variantes.iter().any(|vv| !matches!(vv.campos, super::baja::CamposVariante::Unit));
            if con_datos {
                return Ok((format!("((({lugar}).etiqueta) == ({nc}_{var}))"), Vec::new()));
            }
            return Ok((format!("((({lugar}) == ({nc}_{var})))"), Vec::new()));
        }
        // Struct unitario pelado o calificado.
        if let Some(nc) = self.resuelve_tipo_pub(&segs, raiz) {
            if let Some(s) = self.structs.get(&nc) {
                if tipo != &CType::Usuario(nc.clone()) {
                    return Err(self.err_en(
                        "C0005",
                        format!("ese patrón pide `{nc}`, llegó `{}`", self.muestra(tipo)),
                        x,
                    ));
                }
                if !s.campos.is_empty() {
                    return Err(self.err_en("C0003", format!("`{nc}` lleva campos"), x));
                }
                return Ok(("true".into(), Vec::new()));
            }
            if self.enums.contains_key(&nc) {
                return Err(self.err_en(
                    "C0004",
                    format!("`{}` es enum: falta la variante", segs.join("::")),
                    x,
                ));
            }
        }
        // Constante.
        if let Some(info) = self.busca_const_pub(&segs, raiz) {
            return self.pat_const(&info, tipo, lugar, x);
        }
        Err(self.err_en("C0004", format!("patrón `{}` desconocido", segs.join("::")), x))
    }

    /// Patrón de constante: compara según el tipo (`==`, `strcmp`).
    fn pat_const(
        &mut self,
        info: &super::baja::InfoConst,
        tipo: &CType,
        lugar: &str,
        sp: &syn::PatPath,
    ) -> Result<(String, Vec<Enlace>), CError> {
        if info.tipo != *tipo {
            return Err(self.err_en(
                "C0005",
                format!(
                    "la const da `{}`, llegó `{}`",
                    self.muestra(&info.tipo),
                    self.muestra(tipo)
                ),
                sp,
            ));
        }
        if matches!(info.tipo, CType::F32 | CType::F64) {
            return Err(self.err_en("C0005", "flotantes no valen en patrones".into(), sp));
        }
        let c = info.define.clone().unwrap_or_else(|| info.nombre_c.clone());
        if info.tipo == CType::VistaTexto {
            return Ok((format!("((strcmp(({lugar}), ({c})) == 0))"), Vec::new()));
        }
        if matches!(
            info.tipo,
            CType::Bool
                | CType::Char
                | CType::I8
                | CType::I16
                | CType::I32
                | CType::I64
                | CType::Isize
                | CType::U8
                | CType::U16
                | CType::U32
                | CType::U64
                | CType::Usize
        ) {
            return Ok((format!("((({lugar}) == ({c})))"), Vec::new()));
        }
        Err(self.err_en("C0003", "esa const no vale en patrones".into(), sp))
    }

    /// `42` / `-1` / `'a'` / `"hola"` / `b"ab"`: literal (vía `const_expr`).
    fn pat_lit_expr(
        &mut self,
        e: &syn::Expr,
        tipo: &CType,
        lugar: &str,
        sp: &syn::Pat,
    ) -> Result<(String, Vec<Enlace>), CError> {
        let (lugar_o, tipo_o) = match tipo {
            CType::Ref(_, i) => (format!("(*({lugar}))"), i.as_ref().clone()),
            _ => (lugar.to_string(), tipo.clone()),
        };
        let lugar = lugar_o.as_str();
        let tipo = &tipo_o;
        // Cadenas de bytes contra rebanadas/arreglos.
        if let syn::Expr::Lit(x) = e {
            if let syn::Lit::ByteStr(b) = &x.lit {
                if *tipo == CType::Rebana(Box::new(CType::U8), false)
                    || *tipo == CType::Rebana(Box::new(CType::U8), true)
                {
                    let n = b.value().len();
                    let c = Self::esc_cstr_bytes(&b.value());
                    return Ok((
                        format!("((((({lugar}).largo) == {n}) && ((memcmp((({lugar}).datos), ({c}), {n})) == 0)))"),
                        Vec::new(),
                    ));
                }
                if let CType::Arreglo(elem, largo) = tipo {
                    if elem.as_ref() == &CType::U8 {
                        let n = b.value().len();
                        if let super::tipos::LargoArreglo::Lit(k) = largo {
                            if *k != n {
                                return Err(self.err_en(
                                    "C0005",
                                    format!("el patrón trae {n} bytes, el arreglo {k}"),
                                    sp,
                                ));
                            }
                        }
                        let c = Self::esc_cstr_bytes(&b.value());
                        return Ok((format!("((memcmp(({lugar}), ({c}), {n}) == 0))"), Vec::new()));
                    }
                }
            }
            if let syn::Lit::Str(s) = &x.lit {
                if *tipo == CType::VistaTexto {
                    let c = Self::esc_cstr(&s.value());
                    return Ok((format!("((strcmp(({lugar}), ({c})) == 0))"), Vec::new()));
                }
                if *tipo == CType::Texto {
                    return Err(self.err_en(
                        "C0005",
                        "ese patrón pide `&str`, llegó `String`".into(),
                        sp,
                    )
                    .ayuda("compara con `.as_str()`"));
                }
            }
        }
        if matches!(tipo, CType::F32 | CType::F64) {
            return Err(self.err_en("C0005", "flotantes no valen en patrones".into(), sp));
        }
        if !matches!(
            tipo,
            CType::Bool
                | CType::Char
                | CType::I8
                | CType::I16
                | CType::I32
                | CType::I64
                | CType::Isize
                | CType::U8
                | CType::U16
                | CType::U32
                | CType::U64
                | CType::Usize
        ) {
            return Err(self.err_en(
                "C0005",
                format!("ese literal no casa con `{}`", self.muestra(tipo)),
                sp,
            ));
        }
        let (c, _) = self.const_expr(e, &mut HashSet::new())?;
        if *tipo == CType::Bool && c != "true" && c != "false" {
            return Err(self.err_en("C0005", "ese literal no es `bool`".into(), sp));
        }
        Ok((format!("((({lugar}) == ({c})))"), Vec::new()))
    }

    /// `0..10` / `'a'..='z'`: rango en patrones (solo enteros/chars).
    fn pat_rango(
        &mut self,
        x: &syn::PatRange,
        tipo: &CType,
        lugar: &str,
    ) -> Result<(String, Vec<Enlace>), CError> {
        let es_nc = matches!(
            tipo,
            CType::Char
                | CType::I8
                | CType::I16
                | CType::I32
                | CType::I64
                | CType::Isize
                | CType::U8
                | CType::U16
                | CType::U32
                | CType::U64
                | CType::Usize
        );
        if !es_nc {
            return Err(self.err_en(
                "C0005",
                format!("rangos solo en números, llegó `{}`", self.muestra(tipo)),
                x,
            ));
        }
        let mut conds = Vec::new();
        if let Some(lo) = x.start.as_deref() {
            let (c, _) = self.const_expr(lo, &mut HashSet::new())?;
            conds.push(format!("((({lugar}) >= ({c})))"));
        }
        if let Some(hi) = x.end.as_deref() {
            let (c, _) = self.const_expr(hi, &mut HashSet::new())?;
            let op = match x.limits {
                syn::RangeLimits::HalfOpen(_) => "<",
                syn::RangeLimits::Closed(_) => "<=",
            };
            conds.push(format!("((({lugar}) {op} ({c})))"));
        }
        Ok((Self::y_conds(conds), Vec::new()))
    }

    /// `[a, b]` / `[primero, .., ultimo]`: patrones de rebanada.
    fn pat_rebana(
        &mut self,
        x: &syn::PatSlice,
        tipo: &CType,
        lugar: &str,
    ) -> Result<(String, Vec<Enlace>), CError> {
        let (lugar_o, tipo_o) = match tipo {
            CType::Ref(_, i) => (format!("(*({lugar}))"), i.as_ref().clone()),
            _ => (lugar.to_string(), tipo.clone()),
        };
        let lugar = lugar_o.as_str();
        // (largo C o None si estático, tipo elem, sitio elem)
        let (largo_c, elem, estático): (Option<String>, CType, bool) = match &tipo_o {
            CType::Vec(e) => (Some(format!("(({lugar}).largo)")), e.as_ref().clone(), false),
            CType::Rebana(e, _) => (Some(format!("(({lugar}).largo)")), e.as_ref().clone(), false),
            CType::Arreglo(e, l) => match l {
                super::tipos::LargoArreglo::Lit(k) => {
                    (Some(k.to_string()), e.as_ref().clone(), true)
                }
                _ => {
                    return Err(self.err_en(
                        "C0003",
                        "ese patrón pide largo literal".into(),
                        x,
                    ))
                }
            },
            _ => {
                return Err(self.err_en(
                    "C0005",
                    format!("ese patrón pide lista, llegó `{}`", self.muestra(&tipo_o)),
                    x,
                ))
            }
        };
        let sitio = |i: &str| match &tipo_o {
            CType::Vec(_) | CType::Rebana(_, _) => format!("(({lugar}).datos[{i}])"),
            _ => format!("(({lugar})[{i}])"),
        };
        let (del, det, puntos) = self.parte_puntos(&x.elems, x)?;
        let n = del.len() + det.len();
        let largo = largo_c.unwrap();
        if estático {
            let k: usize = largo.parse().unwrap();
            if (!puntos && n != k) || (puntos && n > k) {
                return Err(self.err_en(
                    "C0005",
                    format!("el arreglo trae {k}, el patrón {n}"),
                    x,
                ));
            }
        }
        let mut conds = Vec::new();
        if !estático {
            if puntos {
                conds.push(format!("((({largo}) >= {n}))"));
            } else {
                conds.push(format!("((({largo}) == {n}))"));
            }
        }
        let mut enlaces = Vec::new();
        for (i, p) in del.iter().enumerate() {
            let (c, mut e) = self.analiza_patron(p, &elem, &sitio(&i.to_string()))?;
            if c != "true" {
                conds.push(format!("({c})"));
            }
            enlaces.append(&mut e);
        }
        for (j, p) in det.iter().enumerate() {
            let idx = if estático {
                (largo.parse::<usize>().unwrap() - det.len() + j).to_string()
            } else {
                format!("(({largo}) - {} + {j})", det.len())
            };
            let (c, mut e) = self.analiza_patron(p, &elem, &sitio(&idx))?;
            if c != "true" {
                conds.push(format!("({c})"));
            }
            enlaces.append(&mut e);
        }
        Ok((Self::y_conds(conds), enlaces))
    }

    /// Declara los enlaces: mueve de temporales pelados, clona hondo de
    /// lugares con dueño, `memcpy` de arreglos Copy.
    pub(crate) fn emite_enlaces(&mut self, enlaces: &[Enlace]) -> Result<(), CError> {
        for e in enlaces {
            let init = e.plantilla.replace("{nc}", "");
            // Temporal pelado (identificador): se mueve.
            let pelado = !init.is_empty()
                && init.chars().all(|c| c.is_alphanumeric() || c == '_');
            let duena = self.necesita_drop(&e.tipo);
            let nc = self.declara(&e.clave, e.tipo.clone(), duena);
            if let CType::Arreglo(elem, largo) = &e.tipo {
                let decl = e.tipo.declara(&nc)?;
                self.emite(format!("{decl};"));
                if self.necesita_drop(elem) {
                    let n_c = largo.deletrea();
                    let idx = format!("__i{}", self.etiqueta_n);
                    self.etiqueta_n += 1;
                    self.emite(format!(
                        "for (size_t {idx} = 0; {idx} < (size_t)({n_c}); ++{idx}) {{"
                    ));
                    let te = self.clona_a_temp(format!("(({init})[{idx}])"), None, elem)?;
                    if matches!(elem.as_ref(), CType::Arreglo(_, _)) {
                        self.emite(format!("memcpy((({nc})[{idx}]), ({te}), sizeof((({nc})[{idx}])));"));
                    } else {
                        self.emite(format!("(({nc})[{idx}]) = ({te});"));
                    }
                    self.marca_movida_si_temp_pub(&te);
                    self.emite("}".to_string());
                } else {
                    self.emite(format!("memcpy(({nc}), ({init}), sizeof({nc}));"));
                }
                continue;
            }
            let decl = e.tipo.declara(&nc)?;
            if duena && !pelado {
                let tc = self.clona_a_temp(init, None, &e.tipo)?;
                self.emite(format!("{decl} = ({tc});"));
                self.marca_movida_si_temp_pub(&tc);
            } else {
                self.emite(format!("{decl} = ({init});"));
                self.marca_movida_si_temp_pub(&init);
            }
        }
        Ok(())
    }

    /// `sea PAT = valor`: arreglos directos, identificadores simples o
    /// desestructuración via `analiza_patron` (irrefutable).
    pub(crate) fn enlaza(
        &mut self,
        pat: &syn::Pat,
        val: ExVal,
        anotado: Option<&CType>,
    ) -> Result<(), CError> {
        // 1. Arreglo diferido + `sea nombre`: directo con su nombre.
        if val.arreglo.is_some() {
            if let syn::Pat::Ident(p) = pat {
                if p.subpat.is_none() && p.by_ref.is_none() {
                    let t_arr = val.tipo.clone();
                    if let Some(a) = anotado {
                        if a != &t_arr {
                            return Err(self.err_en(
                                "C0005",
                                format!(
                                    "se esperaba `{}`, llegó `{}`",
                                    self.muestra(a),
                                    self.muestra(&t_arr)
                                ),
                                pat,
                            ));
                        }
                    }
                    let (elem, largo) = match &t_arr {
                        CType::Arreglo(e, l) => (e.as_ref().clone(), l.clone()),
                        _ => {
                            return Err(CError::nuevo(
                                "C0002",
                                "se esperaba arreglo (bug interno)".to_string(),
                            )
                            .con_archivo(self.archivo.clone()))
                        }
                    };
                    if matches!(elem, CType::Vacio | CType::Infer | CType::Dyn(_)) {
                        return Err(self.err_en("C0003", "elemento de arreglo inválido".into(), pat));
                    }
                    self.registra_uso_pub(&t_arr)?;
                    let duena = self.necesita_drop(&t_arr);
                    let nc = self.declara(&tipos::desnuda(&p.ident), t_arr.clone(), duena);
                    let decl = t_arr.declara(&nc)?;
                    if duena {
                        self.emite(format!("{decl} = {{0}};"));
                    } else {
                        self.emite(format!("{decl};"));
                    }
                    return self.materializa_arreglo(&nc, val, &elem, &largo, false);
                }
            }
            let (tmp, t) = self.aloja_arreglo(val)?;
            let val = ExVal::lugar(tmp.clone(), t, tmp);
            return self.enlaza(pat, val, anotado);
        }
        // 2. Tipo guía (anotado gana; divergente adopta).
        let t = match anotado {
            Some(a) => {
                if a != &val.tipo && val.tipo != CType::Infer {
                    return Err(self.err_en(
                        "C0005",
                        format!(
                            "se esperaba `{}`, llegó `{}`",
                            self.muestra(a),
                            self.muestra(&val.tipo)
                        ),
                        pat,
                    ));
                }
                a.clone()
            }
            None => val.tipo.clone(),
        };
        if t == CType::Infer {
            return Err(self.err_en("C0005", "`sea` sin tipo (diverge o anota)".into(), pat));
        }
        // 3. Comodín: evalúa y tira.
        if let syn::Pat::Wild(_) = pat {
            let c = self.consume(val, Some(&t))?;
            self.emite(format!("(void)({c});"));
            return Ok(());
        }
        // 4. Identificador simple: declara y asigna (arreglo vía `memcpy`).
        if let syn::Pat::Ident(p) = pat {
            if p.subpat.is_none() && p.by_ref.is_none() {
                let nombre = tipos::desnuda(&p.ident);
                if t == CType::Vacio {
                    // `()` no tiene nombre C: testigo de un byte (vale 0).
                    let c = self.consume(val, Some(&t))?;
                    let nc = self.declara(&nombre, CType::Vacio, false);
                    self.emite(format!("char {nc} = 0; /* () */"));
                    self.emite(format!("(void)({c});"));
                    return Ok(());
                }
                if let CType::Arreglo(elem, largo) = &t {
                    self.registra_uso_pub(&t)?;
                    let duena = self.necesita_drop(&t);
                    let nc = self.declara(&nombre, t.clone(), duena);
                    let decl = t.declara(&nc)?;
                    if duena {
                        self.emite(format!("{decl} = {{0}};"));
                    } else {
                        self.emite(format!("{decl};"));
                    }
                    return self.materializa_arreglo(&nc, val, elem, largo, false);
                }
                let c = self.consume(val, Some(&t))?;
                let duena = self.necesita_drop(&t);
                let nc = self.declara(&nombre, t.clone(), duena);
                let decl = t.declara(&nc)?;
                if self.es_dueno_nombrado(&c) {
                    self.emite(format!("{decl} = ({c}); /* mueve */"));
                } else {
                    self.emite(format!("{decl} = ({c});"));
                }
                self.marca_movida_si_temp_pub(&c);
                return Ok(());
            }
        }
        // 5. Desestructuración sobre un lugar (clona dueños al enlazar).
        let (base_c, base_t) = if val.movible.is_some() || val.lugar.is_some() {
            (val.c.clone(), t.clone())
        } else {
            let tt = self.temp(t.clone())?;
            let c = self.consume(val, Some(&t))?;
            self.emite(format!("{tt} = ({c});"));
            self.marca_movida_si_temp_pub(&c);
            (tt, t.clone())
        };
        let (cond, enlaces) = self.analiza_patron(pat, &base_t, &base_c)?;
        if cond != "true" {
            return Err(self.err_en("C0003", "ese patrón puede fallar".into(), pat)
                .ayuda("usa `sea ... else` o `si sea`"));
        }
        self.emite_enlaces(&enlaces)
    }
}
