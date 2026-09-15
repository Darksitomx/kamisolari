//! Ayudantes bajo demanda, monos, vtables y ensamblado.
//!
//! - `libera_lugar`: sentencias que sueltan un lugar de un tipo dado.
//! - `pide_ayuda`: registra `Tipo__op` (liberar/clona/iguales/defecto/depurar).
//! - Monos (`KamiVec_i32`, …) y tipos de usuario en orden topológico.
//! - Vtables para rasgos + instancias por conversión `&T → &din R`.
//! - `ensambla`: pega cabecera, tipos, prototipos y definiciones.

use std::collections::{HashMap, HashSet};

use syn::spanned::Spanned;

use super::baja::{sustituye_self, Bajador, CamposVariante, Destino, Receptor};
use super::errores::CError;
use super::tipos::{self, CType};

impl Bajador {
    // ------------------------------------------------------------------
    // Liberar
    // ------------------------------------------------------------------

    /// Sentencias que liberan `lugar` (expresión C direccionable) de tipo `tipo`.
    /// Vacío si el tipo no es dueño de nada.
    pub fn libera_lugar(&mut self, lugar: &str, tipo: &CType) -> Vec<String> {
        match tipo {
            CType::Texto => vec![format!("kami_texto_liberar(&({lugar}));")],
            CType::Vec(_) | CType::Opcion(_) | CType::Resultado(_, _) | CType::Tupla(_) => {
                let m = tipo.mangle().unwrap();
                self.pide_ayuda_reg("liberar", &m, tipo);
                vec![format!("{m}__liberar(&({lugar}));")]
            }
            CType::Arreglo(elem, largo) => {
                if !self.necesita_drop(elem) {
                    return vec![];
                }
                let m = tipo.mangle().unwrap();
                self.pide_ayuda_reg("liberar", &m, tipo);
                vec![format!("{m}__liberar({lugar});")]
            }
            CType::Usuario(n) => {
                if !self.necesita_drop(tipo) {
                    return vec![];
                }
                self.pide_ayuda_reg("liberar", n, tipo);
                vec![format!("{n}__liberar(&({lugar}));")]
            }
            CType::Caja(inner) => {
                let mut out = vec![format!("if ({lugar}) {{")];
                if self.necesita_drop(inner) {
                    match &**inner {
                        CType::Arreglo(_, _) => {
                            let m = inner.mangle().unwrap();
                            self.pide_ayuda_reg("liberar", &m, inner);
                            out.push(format!("    {m}__liberar(*({lugar}));"));
                        }
                        _ => {
                            let f = self.nombre_libera(inner);
                            out.push(format!("    {f}({lugar});"));
                        }
                    }
                }
                out.push(format!("    free((void*)({lugar}));"));
                out.push("}".into());
                out
            }
            _ => vec![],
        }
    }

    /// Nombre de función liberadora para un tipo (registra ayudante si hace falta).
    /// El llamador pasa puntero al valor.
    fn nombre_libera(&mut self, tipo: &CType) -> String {
        match tipo {
            CType::Texto => "kami_texto_liberar".into(),
            _ => {
                let m = tipo.mangle().unwrap();
                self.pide_ayuda_reg("liberar", &m, tipo);
                format!("{m}__liberar")
            }
        }
    }

    /// Nombre de función clonadora. El llamador pasa puntero y recibe dueño.
    /// `None` si es copia bit a bit (no droppable).
    fn nombre_clona(&mut self, tipo: &CType) -> Option<String> {
        if !self.necesita_drop(tipo) {
            return None;
        }
        match tipo {
            CType::Texto => Some("kami_texto_clona".into()),
            CType::Caja(_) => None, // inline en clona_a_temp
            _ => {
                let m = tipo.mangle().unwrap();
                self.pide_ayuda_reg("clona", &m, tipo);
                Some(format!("{m}__clona"))
            }
        }
    }

    /// Clona `c` (lugar direccionable en `lugar`, si lo es) a un temporal dueño.
    /// Si no es lugar (prvalue), el valor ya es dueño y se mueve directo.
    pub fn clona_a_temp(
        &mut self,
        c: String,
        lugar: Option<String>,
        tipo: &CType,
    ) -> Result<String, CError> {
        let t = self.temp(tipo.clone())?;
        if !self.necesita_drop(tipo) {
            self.emite(format!("{t} = ({c});"));
            self.marca_movida_si_temp(&c);
            return Ok(t);
        }
        let Some(l) = lugar else {
            // Prvalue dueño: mover directo.
            self.emite(format!("{t} = ({c});"));
            self.marca_movida_si_temp(&c);
            return Ok(t);
        };
        match tipo {
            CType::Arreglo(_, _) => {
                let m = tipo.mangle().map_err(|m| CError::nuevo("C0002", m))?;
                self.pide_ayuda_reg("clona", &m, tipo);
                self.emite(format!("{m}__clona(({l}), {t});"));
                Ok(t)
            }
            CType::Caja(inner) => {
                let spell = inner.deletrea()?;
                self.emite(format!("{t} = ({spell}*)malloc(sizeof({spell}));"));
                self.emite(format!("if (!{t}) {{ KAMI_SIN_MEMORIA(); }}"));
                if self.necesita_drop(inner) {
                    match &**inner {
                        CType::Arreglo(_, _) => {
                            let m = inner.mangle().map_err(|m| CError::nuevo("C0002", m))?;
                            self.pide_ayuda_reg("clona", &m, inner);
                            self.emite(format!("{m}__clona((*({l})), (*({t})));"));
                        }
                        _ => {
                            let f = self.nombre_clona(inner).unwrap();
                            self.emite(format!("*({t}) = {f}(&(*({l})));"));
                        }
                    }
                } else {
                    self.emite(format!("*({t}) = (*({l}));"));
                }
                Ok(t)
            }
            _ => {
                let f = self.nombre_clona(tipo).unwrap();
                self.emite(format!("{t} = {f}(&({l}));"));
                Ok(t)
            }
        }
    }

    /// Si `c` nombra un temporal droppable, márcalo movido (se consumió).
    fn marca_movida_si_temp(&mut self, c: &str) {
        let es_temp = self
            .clave_de(c)
            .and_then(|(i, k)| self.pila[i].vars.get(&k).map(|v| v.duena))
            .unwrap_or(false);
        if es_temp {
            let n = c.to_string();
            self.marca_movida(&n);
            self.activa_bandera(&n);
        }
    }

    // ------------------------------------------------------------------
    // Igualdad
    // ------------------------------------------------------------------

    /// Expresión C bool que compara `a`/`b` (con lugares opcionales).
    pub fn iguales_val(
        &mut self,
        ca: String,
        la: Option<String>,
        cb: String,
        lb: Option<String>,
        tipo: &CType,
    ) -> Result<String, CError> {
        match tipo {
            CType::Bool
            | CType::Char
            | CType::I8
            | CType::I16
            | CType::I32
            | CType::I64
            | CType::U8
            | CType::U16
            | CType::U32
            | CType::U64
            | CType::Isize
            | CType::Usize
            | CType::F32
            | CType::F64 => Ok(format!("(({ca}) == ({cb}))")),
            CType::VistaTexto => Ok(format!("(strcmp(({ca}), ({cb})) == 0)")),
            CType::Texto => {
                let pa = self.direcciona(ca, la, &CType::Texto)?;
                let pb = self.direcciona(cb, lb, &CType::Texto)?;
                Ok(format!("kami_texto_iguales({pa}, {pb})"))
            }
            CType::Ref(_, x) | CType::Ptr(_, x) | CType::Caja(x) => {
                // `&a == &b` compara objetivos en Rust.
                self.iguales_val(
                    format!("(*({ca}))"),
                    Some(format!("(*({}))", la.unwrap_or(ca.clone()))),
                    format!("(*({cb}))"),
                    Some(format!("(*({}))", lb.unwrap_or(cb.clone()))),
                    x,
                )
            }
            CType::FnPtr { .. } => Ok(format!("(({ca}) == ({cb}))")),
            CType::Vec(_) | CType::Opcion(_) | CType::Resultado(_, _) | CType::Tupla(_)
            | CType::Arreglo(_, _) | CType::Usuario(_) => {
                let m = tipo.mangle().map_err(|m| CError::nuevo("C0002", m))?;
                if let CType::Usuario(n) = tipo {
                    if let Some(manual) = self.igual_manual(n) {
                        let pa = self.direcciona(ca, la, tipo)?;
                        let pb = self.direcciona(cb, lb, tipo)?;
                        return Ok(format!("{manual}({pa}, {pb})"));
                    }
                }
                self.pide_ayuda_reg("iguales", &m, tipo);
                if matches!(tipo, CType::Arreglo(_, _)) {
                    Ok(format!("{m}__iguales(({ca}), ({cb}))"))
                } else {
                    let pa = self.direcciona(ca, la, tipo)?;
                    let pb = self.direcciona(cb, lb, tipo)?;
                    Ok(format!("{m}__iguales({pa}, {pb})"))
                }
            }
            CType::Dyn(_) => self.falla("C0003", "objetos `din` no comparables con ==".into()),
            CType::Rebana(_, _) => self.falla(
                "C0003",
                "vistas &[...] no comparables (compara largo y elementos)".into(),
            ),
            _ => self.falla("C0003", "esos valores no comparables con ==".into()),
        }
    }

    /// `&lugar` (o temporal si no hay lugar).
    fn direcciona(
        &mut self,
        c: String,
        lugar: Option<String>,
        tipo: &CType,
    ) -> Result<String, CError> {
        if let Some(l) = lugar {
            Ok(format!("&({l})"))
        } else {
            let t = self.temp(tipo.clone())?;
            self.emite(format!("{t} = ({c});"));
            self.marca_movida_si_temp(&c);
            Ok(format!("&{t}"))
        }
    }

    /// ¿Hay `impl PartialEq` manual? Devuelve el nombre C de `eq`.
    fn igual_manual(&self, tipo: &str) -> Option<String> {
        for ((r, t), mapa) in &self.impl_rasgos {
            if t == tipo && (r == "PartialEq" || r.ends_with("__PartialEq")) {
                if let Some(s) = mapa.get("eq") {
                    return Some(s.nombre_c.clone());
                }
            }
        }
        None
    }

    /// ¿Hay `impl Clone` manual? Devuelve el nombre C de `clone`.
    fn clona_manual(&self, tipo: &str) -> Option<String> {
        for ((r, t), mapa) in &self.impl_rasgos {
            if t == tipo && (r == "Clone" || r.ends_with("__Clone")) {
                if let Some(s) = mapa.get("clone") {
                    return Some(s.nombre_c.clone());
                }
            }
        }
        None
    }

    // ------------------------------------------------------------------
    // Defecto
    // ------------------------------------------------------------------

    /// Llena `lugar` con el valor por defecto del tipo.
    pub fn defecto_a_lugar(&mut self, lugar: &str, tipo: &CType) -> Result<(), CError> {
        match tipo {
            CType::Bool
            | CType::Char
            | CType::I8
            | CType::I16
            | CType::I32
            | CType::I64
            | CType::U8
            | CType::U16
            | CType::U32
            | CType::U64
            | CType::Isize
            | CType::Usize
            | CType::F32
            | CType::F64 => {
                let c = tipo.cero()?;
                self.emite(format!("{lugar} = {c};"));
                Ok(())
            }
            CType::Texto => {
                self.emite(format!("{lugar} = kami_texto_nuevo();"));
                Ok(())
            }
            CType::VistaTexto => {
                self.emite(format!("{lugar} = \"\";"));
                Ok(())
            }
            CType::Vec(_) | CType::Opcion(_) => {
                let m = tipo.mangle().map_err(|m| CError::nuevo("C0002", m))?;
                self.emite(format!("{lugar} = ({m}){{0}};"));
                Ok(())
            }
            CType::Tupla(_) | CType::Usuario(_) => {
                let m = tipo.mangle().map_err(|m| CError::nuevo("C0002", m))?;
                self.pide_ayuda_reg("defecto", &m, tipo);
                self.emite(format!("{lugar} = {m}__defecto();"));
                Ok(())
            }
            CType::Arreglo(_, _) => {
                let m = tipo.mangle().map_err(|m| CError::nuevo("C0002", m))?;
                self.pide_ayuda_reg("defecto", &m, tipo);
                self.emite(format!("{m}__defecto({lugar});"));
                Ok(())
            }
            CType::Caja(inner) => {
                let spell = inner.deletrea()?;
                self.emite(format!("{lugar} = ({spell}*)malloc(sizeof({spell}));"));
                self.emite(format!("if (!({lugar})) {{ KAMI_SIN_MEMORIA(); }}"));
                self.defecto_a_lugar(&format!("(*({lugar}))"), inner)
            }
            CType::Resultado(_, _) => self.falla(
                "C0003",
                "Resultado no tiene valor por defecto (¿qué variante sería?)".into(),
            ),
            _ => self.falla("C0003", "ese tipo no tiene valor por defecto".into()),
        }
    }

    // ------------------------------------------------------------------
    // Depurar ({:?})
    // ------------------------------------------------------------------

    /// Renderiza `c`/`lugar` con Debug a un temporal `KamiTexto` dueño.
    pub fn depurar_a_temp(
        &mut self,
        c: String,
        lugar: Option<String>,
        tipo: &CType,
    ) -> Result<String, CError> {
        let t = self.temp(CType::Texto)?;
        self.emite(format!("{t} = kami_texto_nuevo();"));
        self.depurar_en(&c, lugar, tipo, &t)?;
        Ok(t)
    }

    /// Agrega el Debug de `c` a la variable `dest` (KamiTexto ya declarada).
    fn depurar_en(
        &mut self,
        c: &str,
        lugar: Option<String>,
        tipo: &CType,
        dest: &str,
    ) -> Result<(), CError> {
        match tipo {
            CType::Bool => {
                self.emite(format!("kami_texto_empuja({dest}, ({c}) ? \"true\" : \"false\");"));
                Ok(())
            }
            CType::I8 | CType::I16 | CType::I32 | CType::I64 | CType::Isize => {
                self.emite(format!(
                    "kami_texto_empuja_fmt({dest}, \"%lld\", (long long)({c}));"
                ));
                Ok(())
            }
            CType::U8 | CType::U16 | CType::U32 | CType::U64 | CType::Usize => {
                self.emite(format!(
                    "kami_texto_empuja_fmt({dest}, \"%llu\", (unsigned long long)({c}));"
                ));
                Ok(())
            }
            CType::F32 => {
                self.emite(format!("kami_texto_empuja_fmt({dest}, \"%.9g\", (double)({c}));"));
                Ok(())
            }
            CType::F64 => {
                self.emite(format!("kami_texto_empuja_fmt({dest}, \"%.16g\", (double)({c}));"));
                Ok(())
            }
            CType::Char => {
                self.emite(format!("kami_texto_empuja_n({dest}, \"'\", 1);"));
                self.emite(format!("kami_texto_empuja_char({dest}, ({c}));"));
                self.emite(format!("kami_texto_empuja_n({dest}, \"'\", 1);"));
                Ok(())
            }
            CType::VistaTexto => {
                self.emite(format!("kami_texto_empuja_depurado({dest}, ({c}));"));
                Ok(())
            }
            CType::Texto => {
                let p = self.direcciona(c.to_string(), lugar, tipo)?;
                self.emite(format!("{{ KamiTexto __d = kami_texto_depurar({p}); kami_texto_empuja({dest}, kami_texto_cstr(&__d)); kami_texto_liberar(&__d); }}"));
                Ok(())
            }
            CType::Ref(_, x) | CType::Ptr(_, x) | CType::Caja(x) => {
                let inner = format!("(*({c}))");
                self.depurar_en(&inner, Some(inner.clone()), x, dest)
            }
            CType::FnPtr { .. } => {
                self.emite(format!("kami_texto_empuja({dest}, \"<fn>\");"));
                Ok(())
            }
            CType::Dyn(r) => {
                self.emite(format!("kami_texto_empuja({dest}, \"<din {r}>\");"));
                Ok(())
            }
            CType::Rebana(_, _) => {
                self.emite(format!("kami_texto_empuja({dest}, \"<vista>\");"));
                Ok(())
            }
            CType::Vec(_) | CType::Opcion(_) | CType::Resultado(_, _) | CType::Tupla(_)
            | CType::Arreglo(_, _) | CType::Usuario(_) => {
                let m = tipo.mangle().map_err(|m| CError::nuevo("C0002", m))?;
                self.pide_ayuda_reg("depurar", &m, tipo);
                if matches!(tipo, CType::Arreglo(_, _)) {
                    self.emite(format!("{{ KamiTexto __d = {m}__depurar(({c})); kami_texto_empuja({dest}, kami_texto_cstr(&__d)); kami_texto_liberar(&__d); }}"));
                } else {
                    let p = self.direcciona(c.to_string(), lugar, tipo)?;
                    self.emite(format!("{{ KamiTexto __d = {m}__depurar({p}); kami_texto_empuja({dest}, kami_texto_cstr(&__d)); kami_texto_liberar(&__d); }}"));
                }
                Ok(())
            }
            _ => self.falla("C0003", "ese valor no se puede depurar con {:?}".into()),
        }
    }

    // ------------------------------------------------------------------
    // Registro y emisión de ayudantes
    // ------------------------------------------------------------------

    fn pide_ayuda_reg(&mut self, op: &str, mangle: &str, tipo: &CType) {
        // Clone manual gana sobre el generado.
        if op == "clona" {
            if let CType::Usuario(n) = tipo {
                if self.clona_manual(n).is_some() {
                    return;
                }
            }
        }
        self.ayuda
            .entry((op.to_string(), mangle.to_string()))
            .or_insert_with(|| tipo.clone());
    }

    /// Nombre C de la clonadora (manual si hay `impl Clone`).
    pub fn nombre_clona_pub(&mut self, tipo: &CType) -> Option<String> {
        if !self.necesita_drop(tipo) {
            return None;
        }
        if let CType::Usuario(n) = tipo {
            if let Some(m) = self.clona_manual(n) {
                return Some(m);
            }
        }
        self.nombre_clona(tipo)
    }

    pub fn emite_ayudas(&mut self) -> Result<(), CError> {
        let mut hechas = HashSet::new();
        loop {
            let pendientes: Vec<((String, String), CType)> = self
                .ayuda
                .iter()
                .filter(|(k, _)| !hechas.contains(*k))
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect();
            if pendientes.is_empty() {
                break;
            }
            for ((op, mangle), tipo) in pendientes {
                hechas.insert((op.clone(), mangle.clone()));
                self.emite_una_ayuda(&op, &mangle, &tipo)?;
            }
        }
        Ok(())
    }

    fn emite_una_ayuda(&mut self, op: &str, mangle: &str, tipo: &CType) -> Result<(), CError> {
        match op {
            "liberar" => self.emite_liberar(mangle, tipo),
            "clona" => self.emite_clona(mangle, tipo),
            "iguales" => self.emite_iguales(mangle, tipo),
            "defecto" => self.emite_defecto(mangle, tipo),
            "depurar" => self.emite_depurar(mangle, tipo),
            _ => Ok(()),
        }
    }

    fn emite_liberar(&mut self, m: &str, tipo: &CType) -> Result<(), CError> {
        match tipo.clone() {
            CType::Usuario(n) => {
                if let Some(s) = self.structs.get(&n).cloned() {
                    return self.emite_liberar_struct(m, &s);
                }
                if let Some(e) = self.enums.get(&n).cloned() {
                    return self.emite_liberar_enum(m, &e);
                }
                self.falla("C0002", format!("tipo desconocido `{n}`"))
            }
            CType::Vec(e) => {
                let edrop = self.necesita_drop(&e);
                self.protos.push(format!("void {m}__liberar({m} *self);"));
                let mut d = vec![format!("/* Libera {m} (como el drop de Rust). */"), format!("void {m}__liberar({m} *self) {{")];
                d.push("    if (!self || !self->datos) {".into());
                d.push("        if (self) { self->largo = 0; self->capacidad = 0; }".into());
                d.push("        return;".into());
                d.push("    }".into());
                if edrop {
                    d.push("    for (size_t __i = 0; __i < self->largo; ++__i) {".into());
                    for s in self.libera_lugar("self->datos[__i]", &e) {
                        d.push(format!("        {s}"));
                    }
                    d.push("    }".into());
                }
                d.push("    free(self->datos);".into());
                d.push("    self->datos = NULL; self->largo = 0; self->capacidad = 0;".into());
                d.push("}".into());
                self.defs.extend(d);
                Ok(())
            }
            CType::Opcion(e) => {
                self.protos.push(format!("void {m}__liberar({m} *self);"));
                let mut d = vec![format!("/* Libera {m} (como el drop de Rust). */"), format!("void {m}__liberar({m} *self) {{")];
                d.push("    if (!self || !self->tiene) return;".into());
                for s in self.libera_lugar("self->valor", &e) {
                    d.push(format!("    {s}"));
                }
                d.push("    self->tiene = false;".into());
                d.push("}".into());
                self.defs.extend(d);
                Ok(())
            }
            CType::Resultado(a, b) => {
                self.protos.push(format!("void {m}__liberar({m} *self);"));
                let mut d = vec![format!("/* Libera {m} (como el drop de Rust). */"), format!("void {m}__liberar({m} *self) {{")];
                d.push("    if (!self) return;".into());
                d.push("    if (self->es_ok) {".into());
                for s in self.libera_lugar("self->datos.ok", &a) {
                    d.push(format!("        {s}"));
                }
                d.push("    } else {".into());
                for s in self.libera_lugar("self->datos.err", &b) {
                    d.push(format!("        {s}"));
                }
                d.push("    }".into());
                d.push("}".into());
                self.defs.extend(d);
                Ok(())
            }
            CType::Tupla(v) => {
                self.protos.push(format!("void {m}__liberar({m} *self);"));
                let mut d = vec![format!("/* Libera {m} (como el drop de Rust). */"), format!("void {m}__liberar({m} *self) {{")];
                d.push("    if (!self) return;".into());
                for (i, t) in v.iter().enumerate() {
                    for s in self.libera_lugar(&format!("self->_{i}"), t) {
                        d.push(format!("    {s}"));
                    }
                }
                d.push("}".into());
                self.defs.extend(d);
                Ok(())
            }
            CType::Arreglo(e, l) => {
                let es = e.deletrea()?;
                self.protos.push(format!("void {m}__liberar({es} *a);"));
                let mut d = vec![format!("/* Libera {m} (como el drop de Rust). */"), format!("void {m}__liberar({es} *a) {{")];
                d.push(format!("    for (size_t __i = 0; __i < (size_t)({}); ++__i) {{", l.deletrea()));
                for s in self.libera_lugar("a[__i]", &e) {
                    d.push(format!("        {s}"));
                }
                d.push("    }".into());
                d.push("}".into());
                self.defs.extend(d);
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn emite_liberar_struct(
        &mut self,
        m: &str,
        s: &super::baja::InfoStruct,
    ) -> Result<(), CError> {
        self.protos.push(format!("void {m}__liberar({m} *self);"));
        let mut d = vec![format!("/* Libera {m} (como el drop de Rust). */"), format!("void {m}__liberar({m} *self) {{")];
        d.push("    if (!self) return;".into());
        // Primero el cuerpo de `drop`, luego los campos (como Rust).
        if let Some(cuerpo) = self.impl_drop.get(&s.nombre).cloned() {
            self.entra();
            self.pila[0].vars.insert(
                "self".into(),
                super::baja::VarInfo {
                    nombre_c: "self".into(),
                    tipo: CType::Ref(true, Box::new(CType::Usuario(s.nombre.clone()))),
                    movida: false,
                    bandera: None,
                    duena: false,
                },
            );
            self.pila[0].orden.push("self".into());
            let ret = std::mem::replace(&mut self.fn_ret, CType::Vacio);
            let guarda = self.tipo_self.replace(s.nombre.clone());
            self.baja_bloque_contenido(&cuerpo, &Destino::Descarta)?;
            let mut lineas = self.sale();
            // Quita el `return` implícito si lo hubiera (drop es void).
            lineas.retain(|l| l.trim() != "return;");
            for l in lineas {
                d.push(format!("    {l}"));
            }
            self.fn_ret = ret;
            self.tipo_self = guarda;
        }
        if !s.es_union {
            for c in &s.campos {
                for l in self.libera_lugar(&format!("self->{}", c.nombre), &c.tipo) {
                    d.push(format!("    {l}"));
                }
            }
        }
        d.push("}".into());
        self.defs.extend(d);
        Ok(())
    }

    fn emite_liberar_enum(&mut self, m: &str, e: &super::baja::InfoEnum) -> Result<(), CError> {
        self.protos.push(format!("void {m}__liberar({m} *self);"));
        let mut d = vec![format!("/* Libera {m} (como el drop de Rust). */"), format!("void {m}__liberar({m} *self) {{")];
        d.push("    if (!self) return;".into());
        d.push("    switch (self->etiqueta) {".into());
        for v in &e.variantes {
            let tag = format!("{m}_{}", v.nombre);
            match &v.campos {
                CamposVariante::Unit => {}
                CamposVariante::Tuple(ts) => {
                    if ts.iter().any(|t| self.necesita_drop(t)) {
                        d.push(format!("    case {tag}: {{"));
                        for (i, t) in ts.iter().enumerate() {
                            for l in self.libera_lugar(&format!("self->datos.{}._{i}", v.nombre), t) {
                                d.push(format!("        {l}"));
                            }
                        }
                        d.push("        break;".into());
                        d.push("    }".into());
                    }
                }
                CamposVariante::Struct(cs) => {
                    if cs.iter().any(|c| self.necesita_drop(&c.tipo)) {
                        d.push(format!("    case {tag}: {{"));
                        for c in cs {
                            for l in self.libera_lugar(&format!("self->datos.{}.{}", v.nombre, c.nombre), &c.tipo) {
                                d.push(format!("        {l}"));
                            }
                        }
                        d.push("        break;".into());
                        d.push("    }".into());
                    }
                }
            }
        }
        d.push("    default: break;".into());
        d.push("    }".into());
        d.push("}".into());
        self.defs.extend(d);
        Ok(())
    }

    fn emite_clona(&mut self, m: &str, tipo: &CType) -> Result<(), CError> {
        match tipo.clone() {
            CType::Usuario(n) => {
                if let Some(s) = self.structs.get(&n).cloned() {
                    return self.emite_clona_struct(m, &s);
                }
                if let Some(e) = self.enums.get(&n).cloned() {
                    return self.emite_clona_enum(m, &e);
                }
                self.falla("C0002", format!("tipo desconocido `{n}`"))
            }
            CType::Vec(e) => {
                let es = e.deletrea()?;
                self.protos.push(format!("{m} {m}__clona(const {m} *self);"));
                let mut d = vec![format!("/* Clona {m} (copia profunda). */"), format!("{m} {m}__clona(const {m} *self) {{")];
                d.push(format!("    {m} r = {{0}};"));
                d.push("    if (!self || self->largo == 0) return r;".into());
                d.push(format!("    r.datos = ({es}*)malloc(sizeof(*r.datos) * self->largo);"));
                d.push("    if (!r.datos) { KAMI_SIN_MEMORIA(); }".into());
                d.push("    r.largo = self->largo; r.capacidad = self->largo;".into());
                if matches!(&*e, CType::Arreglo(_, _)) {
                    let me = e.mangle().unwrap();
                    self.pide_ayuda_reg("clona", &me, &e);
                    d.push("    for (size_t __i = 0; __i < self->largo; ++__i) {".into());
                    d.push(format!("        {me}__clona(self->datos[__i], r.datos[__i]);"));
                    d.push("    }".into());
                } else if self.necesita_drop(&e) {
                    let f = self.nombre_clona(&e).unwrap();
                    d.push("    for (size_t __i = 0; __i < self->largo; ++__i) {".into());
                    d.push(format!("        r.datos[__i] = {f}(&self->datos[__i]);"));
                    d.push("    }".into());
                } else {
                    d.push("    memcpy(r.datos, self->datos, sizeof(*r.datos) * self->largo);".into());
                }
                d.push("    return r;".into());
                d.push("}".into());
                self.defs.extend(d);
                Ok(())
            }
            CType::Opcion(e) => {
                self.protos.push(format!("{m} {m}__clona(const {m} *self);"));
                let mut d = vec![format!("/* Clona {m} (copia profunda). */"), format!("{m} {m}__clona(const {m} *self) {{")];
                d.push(format!("    {m} r = {{0}};"));
                d.push("    if (!self || !self->tiene) return r;".into());
                d.push("    r.tiene = true;".into());
                d.extend(self.stmts_clona_campo("self->valor", "r.valor", &e, "    "));
                d.push("    return r;".into());
                d.push("}".into());
                self.defs.extend(d);
                Ok(())
            }
            CType::Resultado(a, b) => {
                self.protos.push(format!("{m} {m}__clona(const {m} *self);"));
                let mut d = vec![format!("/* Clona {m} (copia profunda). */"), format!("{m} {m}__clona(const {m} *self) {{")];
                d.push(format!("    {m} r = {{0}};"));
                d.push("    if (!self) return r;".into());
                d.push("    r.es_ok = self->es_ok;".into());
                d.push("    if (self->es_ok) {".into());
                d.extend(self.stmts_clona_campo("self->datos.ok", "r.datos.ok", &a, "        "));
                d.push("    } else {".into());
                d.extend(self.stmts_clona_campo("self->datos.err", "r.datos.err", &b, "        "));
                d.push("    }".into());
                d.push("    return r;".into());
                d.push("}".into());
                self.defs.extend(d);
                Ok(())
            }
            CType::Tupla(v) => {
                self.protos.push(format!("{m} {m}__clona(const {m} *self);"));
                let mut d = vec![format!("/* Clona {m} (copia profunda). */"), format!("{m} {m}__clona(const {m} *self) {{")];
                d.push(format!("    {m} r = {{0}};"));
                for (i, t) in v.iter().enumerate() {
                    d.extend(self.stmts_clona_campo(
                        &format!("self->_{i}"),
                        &format!("r._{i}"),
                        t,
                        "    ",
                    ));
                }
                d.push("    return r;".into());
                d.push("}".into());
                self.defs.extend(d);
                Ok(())
            }
            CType::Arreglo(e, l) => {
                let es = e.deletrea()?;
                self.protos.push(format!("void {m}__clona(const {es} *src, {es} *dst);"));
                let mut d = vec![format!("/* Clona {m} (copia profunda). */"), format!("void {m}__clona(const {es} *src, {es} *dst) {{")];
                d.push(format!("    for (size_t __i = 0; __i < (size_t)({}); ++__i) {{", l.deletrea()));
                if matches!(&*e, CType::Arreglo(_, _)) {
                    let me = e.mangle().unwrap();
                    self.pide_ayuda_reg("clona", &me, &e);
                    d.push(format!("        {me}__clona(src[__i], dst[__i]);"));
                } else if self.necesita_drop(&e) {
                    let f = self.nombre_clona(&e).unwrap();
                    d.push(format!("        dst[__i] = {f}(&src[__i]);"));
                } else {
                    d.push("        dst[__i] = src[__i];".into());
                }
                d.push("    }".into());
                d.push("}".into());
                self.defs.extend(d);
                Ok(())
            }
            _ => Ok(()),
        }
    }

    /// `dst = clona(src)` (maneja arreglos con llamada).
    fn stmts_clona_campo(&mut self, src: &str, dst: &str, t: &CType, ind: &str) -> Vec<String> {
        if matches!(t, CType::Arreglo(_, _)) {
            let me = t.mangle().unwrap();
            self.pide_ayuda_reg("clona", &me, t);
            vec![format!("{ind}{me}__clona({src}, {dst});")]
        } else if self.necesita_drop(t) {
            let f = self.nombre_clona(t).unwrap();
            vec![format!("{ind}{dst} = {f}(&({src}));")]
        } else {
            vec![format!("{ind}{dst} = ({src});")]
        }
    }

    fn emite_clona_struct(&mut self, m: &str, s: &super::baja::InfoStruct) -> Result<(), CError> {
        self.protos.push(format!("{m} {m}__clona(const {m} *self);"));
        let mut d = vec![format!("/* Clona {m} (copia profunda). */"), format!("{m} {m}__clona(const {m} *self) {{")];
        if s.es_union {
            d.push("    return *self;".into());
            d.push("}".into());
            self.defs.extend(d);
            return Ok(());
        }
        d.push(format!("    {m} r = {{0}};"));
        if s.campos.is_empty() {
            d.push("    (void)self;".into());
        }
        for c in &s.campos {
            d.extend(self.stmts_clona_campo(
                &format!("self->{}", c.nombre),
                &format!("r.{}", c.nombre),
                &c.tipo,
                "    ",
            ));
        }
        d.push("    return r;".into());
        d.push("}".into());
        self.defs.extend(d);
        Ok(())
    }

    fn emite_clona_enum(&mut self, m: &str, e: &super::baja::InfoEnum) -> Result<(), CError> {
        self.protos.push(format!("{m} {m}__clona(const {m} *self);"));
        let mut d = vec![format!("/* Clona {m} (copia profunda). */"), format!("{m} {m}__clona(const {m} *self) {{")];
        d.push(format!("    {m} r = {{0}};"));
        d.push("    r.etiqueta = self->etiqueta;".into());
        d.push("    switch (self->etiqueta) {".into());
        for v in &e.variantes {
            let tag = format!("{m}_{}", v.nombre);
            match &v.campos {
                CamposVariante::Unit => {}
                CamposVariante::Tuple(ts) => {
                    d.push(format!("    case {tag}: {{"));
                    for (i, t) in ts.iter().enumerate() {
                        d.extend(self.stmts_clona_campo(
                            &format!("self->datos.{}._{i}", v.nombre),
                            &format!("r.datos.{}._{i}", v.nombre),
                            t,
                            "        ",
                        ));
                    }
                    d.push("        break;".into());
                    d.push("    }".into());
                }
                CamposVariante::Struct(cs) => {
                    d.push(format!("    case {tag}: {{"));
                    for c in cs {
                        d.extend(self.stmts_clona_campo(
                            &format!("self->datos.{}.{}", v.nombre, c.nombre),
                            &format!("r.datos.{}.{}", v.nombre, c.nombre),
                            &c.tipo,
                            "        ",
                        ));
                    }
                    d.push("        break;".into());
                    d.push("    }".into());
                }
            }
        }
        d.push("    default: break;".into());
        d.push("    }".into());
        d.push("    return r;".into());
        d.push("}".into());
        self.defs.extend(d);
        Ok(())
    }

    fn emite_iguales(&mut self, m: &str, tipo: &CType) -> Result<(), CError> {
        match tipo.clone() {
            CType::Usuario(n) => {
                if let Some(s) = self.structs.get(&n).cloned() {
                    return self.emite_iguales_struct(m, &s);
                }
                if let Some(e) = self.enums.get(&n).cloned() {
                    return self.emite_iguales_enum(m, &e);
                }
                self.falla("C0002", format!("tipo desconocido `{n}`"))
            }
            CType::Vec(e) => {
                self.protos.push(format!("bool {m}__iguales(const {m} *a, const {m} *b);"));
                let mut d = vec![format!("bool {m}__iguales(const {m} *a, const {m} *b) {{")];
                d.push("    if (a->largo != b->largo) return false;".into());
                d.push("    for (size_t __i = 0; __i < a->largo; ++__i) {".into());
                let c = self.expr_igual_elem("a->datos[__i]", "b->datos[__i]", &e)?;
                d.push(format!("        if (!({c})) return false;"));
                d.push("    }".into());
                d.push("    return true;".into());
                d.push("}".into());
                self.defs.extend(d);
                Ok(())
            }
            CType::Opcion(e) => {
                self.protos.push(format!("bool {m}__iguales(const {m} *a, const {m} *b);"));
                let mut d = vec![format!("bool {m}__iguales(const {m} *a, const {m} *b) {{")];
                d.push("    if (a->tiene != b->tiene) return false;".into());
                d.push("    if (!a->tiene) return true;".into());
                let c = self.expr_igual_elem("a->valor", "b->valor", &e)?;
                d.push(format!("    return ({c});"));
                d.push("}".into());
                self.defs.extend(d);
                Ok(())
            }
            CType::Resultado(a, b) => {
                self.protos.push(format!("bool {m}__iguales(const {m} *a, const {m} *b);"));
                let mut d = vec![format!("bool {m}__iguales(const {m} *a, const {m} *b) {{")];
                d.push("    if (a->es_ok != b->es_ok) return false;".into());
                let ca = self.expr_igual_elem("a->datos.ok", "b->datos.ok", &a)?;
                let cb = self.expr_igual_elem("a->datos.err", "b->datos.err", &b)?;
                d.push(format!("    if (a->es_ok) return ({ca});"));
                d.push(format!("    return ({cb});"));
                d.push("}".into());
                self.defs.extend(d);
                Ok(())
            }
            CType::Tupla(v) => {
                self.protos.push(format!("bool {m}__iguales(const {m} *a, const {m} *b);"));
                let mut d = vec![format!("bool {m}__iguales(const {m} *a, const {m} *b) {{")];
                if v.is_empty() {
                    d.push("    (void)a; (void)b; return true;".into());
                } else {
                    let mut parts = Vec::new();
                    for (i, t) in v.iter().enumerate() {
                        parts.push(self.expr_igual_elem(&format!("a->_{i}"), &format!("b->_{i}"), t)?);
                    }
                    d.push(format!("    return ({});", parts.join(" && ")));
                }
                d.push("}".into());
                self.defs.extend(d);
                Ok(())
            }
            CType::Arreglo(e, l) => {
                let es = e.deletrea()?;
                self.protos
                    .push(format!("bool {m}__iguales(const {es} *a, const {es} *b);"));
                let mut d = vec![format!("bool {m}__iguales(const {es} *a, const {es} *b) {{")];
                d.push(format!("    for (size_t __i = 0; __i < (size_t)({}); ++__i) {{", l.deletrea()));
                let c = self.expr_igual_elem("a[__i]", "b[__i]", &e)?;
                d.push(format!("        if (!({c})) return false;"));
                d.push("    }".into());
                d.push("    return true;".into());
                d.push("}".into());
                self.defs.extend(d);
                Ok(())
            }
            _ => Ok(()),
        }
    }

    /// Expresión bool que compara dos lugares direccionables del mismo tipo.
    fn expr_igual_elem(&mut self, a: &str, b: &str, t: &CType) -> Result<String, CError> {
        match t {
            CType::Bool
            | CType::Char
            | CType::I8
            | CType::I16
            | CType::I32
            | CType::I64
            | CType::U8
            | CType::U16
            | CType::U32
            | CType::U64
            | CType::Isize
            | CType::Usize
            | CType::F32
            | CType::F64 => Ok(format!("(({a}) == ({b}))")),
            CType::VistaTexto => Ok(format!("(strcmp(({a}), ({b})) == 0)")),
            CType::Texto => Ok(format!("kami_texto_iguales(&({a}), &({b}))")),
            CType::Ref(_, x) | CType::Ptr(_, x) | CType::Caja(x) => {
                self.expr_igual_elem(&format!("(*({a}))"), &format!("(*({b}))"), x)
            }
            CType::FnPtr { .. } => Ok(format!("(({a}) == ({b}))")),
            CType::Vec(_) | CType::Opcion(_) | CType::Resultado(_, _) | CType::Tupla(_)
            | CType::Usuario(_) => {
                if let CType::Usuario(n) = t {
                    if let Some(manual) = self.igual_manual(n) {
                        return Ok(format!("{manual}(&({a}), &({b}))"));
                    }
                }
                let m = t.mangle().map_err(|m| CError::nuevo("C0002", m))?;
                self.pide_ayuda_reg("iguales", &m, t);
                Ok(format!("{m}__iguales(&({a}), &({b}))"))
            }
            CType::Arreglo(_, _) => {
                let m = t.mangle().map_err(|m| CError::nuevo("C0002", m))?;
                self.pide_ayuda_reg("iguales", &m, t);
                Ok(format!("{m}__iguales(({a}), ({b}))"))
            }
            _ => self.falla("C0003", "ese campo no comparable con ==".into()),
        }
    }

    fn emite_iguales_struct(
        &mut self,
        m: &str,
        s: &super::baja::InfoStruct,
    ) -> Result<(), CError> {
        self.protos.push(format!("bool {m}__iguales(const {m} *a, const {m} *b);"));
        let mut d = vec![format!("bool {m}__iguales(const {m} *a, const {m} *b) {{")];
        if s.es_union {
            d.push("    (void)a; (void)b; return true; /* unión: sin comparar */".into());
        } else if s.campos.is_empty() {
            d.push("    (void)a; (void)b; return true;".into());
        } else {
            let mut parts = Vec::new();
            for c in &s.campos {
                parts.push(self.expr_igual_elem(
                    &format!("a->{}", c.nombre),
                    &format!("b->{}", c.nombre),
                    &c.tipo,
                )?);
            }
            d.push(format!("    return ({});", parts.join(" && ")));
        }
        d.push("}".into());
        self.defs.extend(d);
        Ok(())
    }

    fn emite_iguales_enum(&mut self, m: &str, e: &super::baja::InfoEnum) -> Result<(), CError> {
        self.protos.push(format!("bool {m}__iguales(const {m} *a, const {m} *b);"));
        let mut d = vec![format!("bool {m}__iguales(const {m} *a, const {m} *b) {{")];
        d.push("    if (a->etiqueta != b->etiqueta) return false;".into());
        d.push("    switch (a->etiqueta) {".into());
        for v in &e.variantes {
            let tag = format!("{m}_{}", v.nombre);
            match &v.campos {
                CamposVariante::Unit => {}
                CamposVariante::Tuple(ts) => {
                    d.push(format!("    case {tag}: {{"));
                    let mut parts = Vec::new();
                    for (i, t) in ts.iter().enumerate() {
                        parts.push(self.expr_igual_elem(
                            &format!("a->datos.{}._{i}", v.nombre),
                            &format!("b->datos.{}._{i}", v.nombre),
                            t,
                        )?);
                    }
                    d.push(format!("        return ({});", parts.join(" && ")));
                    d.push("    }".into());
                }
                CamposVariante::Struct(cs) => {
                    d.push(format!("    case {tag}: {{"));
                    let mut parts = Vec::new();
                    for c in cs {
                        parts.push(self.expr_igual_elem(
                            &format!("a->datos.{}.{}", v.nombre, c.nombre),
                            &format!("b->datos.{}.{}", v.nombre, c.nombre),
                            &c.tipo,
                        )?);
                    }
                    d.push(format!("        return ({});", parts.join(" && ")));
                    d.push("    }".into());
                }
            }
        }
        d.push("    default: break;".into());
        d.push("    }".into());
        d.push("    return true;".into());
        d.push("}".into());
        self.defs.extend(d);
        Ok(())
    }

    fn emite_defecto(&mut self, m: &str, tipo: &CType) -> Result<(), CError> {
        match tipo.clone() {
            CType::Usuario(n) => {
                if let Some(s) = self.structs.get(&n).cloned() {
                    if s.es_union {
                        return self.falla(
                            "C0003",
                            format!("la unión `{n}` no tiene valor por defecto"),
                        );
                    }
                    self.protos.push(format!("{m} {m}__defecto(void);"));
                    let mut d = vec![format!("{m} {m}__defecto(void) {{")];
                    d.push(format!("    {m} r = {{0}};"));
                    for c in &s.campos {
                        self.stmts_defecto_campo(&format!("r.{}", c.nombre), &c.tipo, &mut d)?;
                    }
                    d.push("    return r;".into());
                    d.push("}".into());
                    self.defs.extend(d);
                    return Ok(());
                }
                if let Some(e) = self.enums.get(&n).cloned() {
                    let def = e.variantes.iter().find(|v| v.es_default);
                    let Some(v) = def else {
                        return self.falla(
                            "C0003",
                            format!("la enumeración `{n}` no trae variante `#[default]`"),
                        ).map_err(|e| e.ayuda(
                            "marca una variante unitaria con #[default] (requiere Rust reciente) o construye el valor a mano",
                        ));
                    };
                    if !matches!(v.campos, CamposVariante::Unit) {
                        return self.falla(
                            "C0003",
                            "la variante #[default] debe ser unitaria".into(),
                        );
                    }
                    self.protos.push(format!("{m} {m}__defecto(void);"));
                    let mut d = vec![format!("{m} {m}__defecto(void) {{")];
                    d.push(format!("    {m} r = {{0}};"));
                    d.push(format!("    r.etiqueta = {m}_{};", v.nombre));
                    d.push("    return r;".into());
                    d.push("}".into());
                    self.defs.extend(d);
                    return Ok(());
                }
                self.falla("C0002", format!("tipo desconocido `{n}`"))
            }
            CType::Tupla(v) => {
                self.protos.push(format!("{m} {m}__defecto(void);"));
                let mut d = vec![format!("{m} {m}__defecto(void) {{")];
                d.push(format!("    {m} r = {{0}};"));
                for (i, t) in v.iter().enumerate() {
                    self.stmts_defecto_campo(&format!("r._{i}"), t, &mut d)?;
                }
                d.push("    return r;".into());
                d.push("}".into());
                self.defs.extend(d);
                Ok(())
            }
            CType::Arreglo(e, l) => {
                let es = e.deletrea()?;
                self.protos.push(format!("void {m}__defecto({es} *dst);"));
                let mut d = vec![format!("void {m}__defecto({es} *dst) {{")];
                d.push(format!("    for (size_t __i = 0; __i < (size_t)({}); ++__i) {{", l.deletrea()));
                self.stmts_defecto_campo("dst[__i]", &e, &mut d)?;
                // Indenta el cuerpo del ciclo.
                let n = d.len();
                let mut cuerpo = d.split_off(n - 1);
                // (solo hay una línea o pocas; reindentar todo lo agregado)
                let _ = &mut cuerpo;
                d.push("    }".into());
                d.push("}".into());
                self.defs.extend(d);
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn stmts_defecto_campo(
        &mut self,
        dst: &str,
        t: &CType,
        d: &mut Vec<String>,
    ) -> Result<(), CError> {
        match t {
            CType::Bool
            | CType::Char
            | CType::I8
            | CType::I16
            | CType::I32
            | CType::I64
            | CType::U8
            | CType::U16
            | CType::U32
            | CType::U64
            | CType::Isize
            | CType::Usize
            | CType::F32
            | CType::F64 => {
                d.push(format!("    {dst} = {};", t.cero()?));
                Ok(())
            }
            CType::Texto => {
                d.push(format!("    {dst} = kami_texto_nuevo();"));
                Ok(())
            }
            CType::VistaTexto => {
                d.push(format!("    {dst} = \"\";"));
                Ok(())
            }
            CType::Vec(_) | CType::Opcion(_) => {
                let mm = t.mangle().map_err(|m| CError::nuevo("C0002", m))?;
                d.push(format!("    {dst} = ({mm}){{0}};"));
                Ok(())
            }
            CType::Tupla(_) | CType::Usuario(_) => {
                let mm = t.mangle().map_err(|m| CError::nuevo("C0002", m))?;
                self.pide_ayuda_reg("defecto", &mm, t);
                d.push(format!("    {dst} = {mm}__defecto();"));
                Ok(())
            }
            CType::Arreglo(_, _) => {
                let mm = t.mangle().map_err(|m| CError::nuevo("C0002", m))?;
                self.pide_ayuda_reg("defecto", &mm, t);
                d.push(format!("    {mm}__defecto({dst});"));
                Ok(())
            }
            CType::Caja(inner) => {
                let spell = inner.deletrea()?;
                d.push(format!("    {dst} = ({spell}*)malloc(sizeof({spell}));"));
                d.push(format!("    if (!({dst})) {{ KAMI_SIN_MEMORIA(); }}"));
                self.stmts_defecto_campo(&format!("(*({dst}))"), inner, d)
            }
            _ => self.falla("C0003", "ese campo no tiene valor por defecto".into()),
        }
    }

    fn emite_depurar(&mut self, m: &str, tipo: &CType) -> Result<(), CError> {
        match tipo.clone() {
            CType::Usuario(n) => {
                if let Some(s) = self.structs.get(&n).cloned() {
                    return self.emite_depurar_struct(m, &s);
                }
                if let Some(e) = self.enums.get(&n).cloned() {
                    return self.emite_depurar_enum(m, &e);
                }
                self.falla("C0002", format!("tipo desconocido `{n}`"))
            }
            CType::Vec(e) => {
                self.protos.push(format!("KamiTexto {m}__depurar(const {m} *self);"));
                let mut d = vec![format!("/* Depura {m} (como {{:?}}). */"), format!("KamiTexto {m}__depurar(const {m} *self) {{")];
                d.push("    KamiTexto r = kami_texto_desde(\"[\");".into());
                d.push("    for (size_t __i = 0; self && __i < self->largo; ++__i) {".into());
                d.push("        if (__i) kami_texto_empuja_n(&r, \", \", 2);".into());
                d.extend(self.stmts_depurar_campo("self->datos[__i]", &e, "r", "        ")?);
                d.push("    }".into());
                d.push("    kami_texto_empuja_n(&r, \"]\", 1);".into());
                d.push("    return r;".into());
                d.push("}".into());
                self.defs.extend(d);
                Ok(())
            }
            CType::Opcion(e) => {
                self.protos.push(format!("KamiTexto {m}__depurar(const {m} *self);"));
                let mut d = vec![format!("/* Depura {m} (como {{:?}}). */"), format!("KamiTexto {m}__depurar(const {m} *self) {{")];
                d.push("    KamiTexto r = kami_texto_nuevo();".into());
                d.push("    if (!self || !self->tiene) { kami_texto_empuja(&r, \"None\"); return r; }".into());
                d.push("    kami_texto_empuja(&r, \"Some(\");".into());
                d.extend(self.stmts_depurar_campo("self->valor", &e, "r", "    ")?);
                d.push("    kami_texto_empuja_n(&r, \")\", 1);".into());
                d.push("    return r;".into());
                d.push("}".into());
                self.defs.extend(d);
                Ok(())
            }
            CType::Resultado(a, b) => {
                self.protos.push(format!("KamiTexto {m}__depurar(const {m} *self);"));
                let mut d = vec![format!("/* Depura {m} (como {{:?}}). */"), format!("KamiTexto {m}__depurar(const {m} *self) {{")];
                d.push("    KamiTexto r = kami_texto_nuevo();".into());
                d.push("    if (self && self->es_ok) {".into());
                d.push("        kami_texto_empuja(&r, \"Ok(\");".into());
                d.extend(self.stmts_depurar_campo("self->datos.ok", &a, "r", "        ")?);
                d.push("        kami_texto_empuja_n(&r, \")\", 1);".into());
                d.push("    } else {".into());
                d.push("        kami_texto_empuja(&r, \"Err(\");".into());
                d.extend(self.stmts_depurar_campo("self->datos.err", &b, "r", "        ")?);
                d.push("        kami_texto_empuja_n(&r, \")\", 1);".into());
                d.push("    }".into());
                d.push("    return r;".into());
                d.push("}".into());
                self.defs.extend(d);
                Ok(())
            }
            CType::Tupla(v) => {
                self.protos.push(format!("KamiTexto {m}__depurar(const {m} *self);"));
                let mut d = vec![format!("/* Depura {m} (como {{:?}}). */"), format!("KamiTexto {m}__depurar(const {m} *self) {{")];
                d.push("    KamiTexto r = kami_texto_desde(\"(\");".into());
                for (i, t) in v.iter().enumerate() {
                    if i > 0 {
                        d.push("    kami_texto_empuja_n(&r, \", \", 2);".into());
                    }
                    d.extend(self.stmts_depurar_campo(&format!("self->_{i}"), t, "r", "    ")?);
                    if v.len() == 1 {
                        d.push("    kami_texto_empuja_n(&r, \",\", 1);".into());
                    }
                }
                d.push("    kami_texto_empuja_n(&r, \")\", 1);".into());
                d.push("    return r;".into());
                d.push("}".into());
                self.defs.extend(d);
                Ok(())
            }
            CType::Arreglo(e, l) => {
                let es = e.deletrea()?;
                self.protos
                    .push(format!("KamiTexto {m}__depurar(const {es} *a);"));
                let mut d = vec![format!("KamiTexto {m}__depurar(const {es} *a) {{")];
                d.push("    KamiTexto r = kami_texto_desde(\"[\");".into());
                d.push(format!("    for (size_t __i = 0; __i < (size_t)({}); ++__i) {{", l.deletrea()));
                d.push("        if (__i) kami_texto_empuja_n(&r, \", \", 2);".into());
                d.extend(self.stmts_depurar_campo("a[__i]", &e, "r", "        ")?);
                d.push("    }".into());
                d.push("    kami_texto_empuja_n(&r, \"]\", 1);".into());
                d.push("    return r;".into());
                d.push("}".into());
                self.defs.extend(d);
                Ok(())
            }
            _ => Ok(()),
        }
    }

    /// Sentencias que agregan el Debug de `lugar` a `dest`.
    fn stmts_depurar_campo(
        &mut self,
        lugar: &str,
        t: &CType,
        dest: &str,
        ind: &str,
    ) -> Result<Vec<String>, CError> {
        let d = dest.to_string();
        let push = |s: String| vec![format!("{ind}{s}")];
        match t {
            CType::Bool => Ok(push(format!(
                "kami_texto_empuja(&{d}, ({lugar}) ? \"true\" : \"false\");"
            ))),
            CType::I8 | CType::I16 | CType::I32 | CType::I64 | CType::Isize => Ok(push(format!(
                "kami_texto_empuja_fmt(&{d}, \"%lld\", (long long)({lugar}));"
            ))),
            CType::U8 | CType::U16 | CType::U32 | CType::U64 | CType::Usize => Ok(push(format!(
                "kami_texto_empuja_fmt(&{d}, \"%llu\", (unsigned long long)({lugar}));"
            ))),
            CType::F32 => Ok(push(format!(
                "kami_texto_empuja_fmt(&{d}, \"%.9g\", (double)({lugar}));"
            ))),
            CType::F64 => Ok(push(format!(
                "kami_texto_empuja_fmt(&{d}, \"%.16g\", (double)({lugar}));"
            ))),
            CType::Char => Ok(vec![
                format!("{ind}kami_texto_empuja_n(&{d}, \"'\", 1);"),
                format!("{ind}kami_texto_empuja_char(&{d}, ({lugar}));"),
                format!("{ind}kami_texto_empuja_n(&{d}, \"'\", 1);"),
            ]),
            CType::VistaTexto => Ok(push(format!(
                "kami_texto_empuja_depurado(&{d}, ({lugar}));"
            ))),
            CType::Texto => Ok(push(format!(
                "{{ KamiTexto __d = kami_texto_depurar(&({lugar})); kami_texto_empuja(&{d}, kami_texto_cstr(&__d)); kami_texto_liberar(&__d); }}"
            ))),
            CType::Ref(_, x) | CType::Ptr(_, x) | CType::Caja(x) => {
                self.stmts_depurar_campo(&format!("(*({lugar}))"), x, dest, ind)
            }
            CType::FnPtr { .. } => Ok(push(format!("kami_texto_empuja(&{d}, \"<fn>\");"))),
            CType::Dyn(r) => Ok(push(format!("kami_texto_empuja(&{d}, \"<din {r}>\");"))),
            CType::Rebana(_, _) => Ok(push(format!("kami_texto_empuja(&{d}, \"<vista>\");"))),
            CType::Vec(_) | CType::Opcion(_) | CType::Resultado(_, _) | CType::Tupla(_)
            | CType::Usuario(_) => {
                let mm = t.mangle().map_err(|m| CError::nuevo("C0002", m))?;
                self.pide_ayuda_reg("depurar", &mm, t);
                Ok(push(format!(
                    "{{ KamiTexto __d = {mm}__depurar(&({lugar})); kami_texto_empuja(&{d}, kami_texto_cstr(&__d)); kami_texto_liberar(&__d); }}"
                )))
            }
            CType::Arreglo(_, _) => {
                let mm = t.mangle().map_err(|m| CError::nuevo("C0002", m))?;
                self.pide_ayuda_reg("depurar", &mm, t);
                Ok(push(format!(
                    "{{ KamiTexto __d = {mm}__depurar(({lugar})); kami_texto_empuja(&{d}, kami_texto_cstr(&__d)); kami_texto_liberar(&__d); }}"
                )))
            }
            _ => self.falla("C0003", "ese campo no depurable con {:?}".into()),
        }
    }

    fn emite_depurar_struct(
        &mut self,
        m: &str,
        s: &super::baja::InfoStruct,
    ) -> Result<(), CError> {
        self.protos.push(format!("KamiTexto {m}__depurar(const {m} *self);"));
        let mut d = vec![format!("/* Depura {m} (como {{:?}}). */"), format!("KamiTexto {m}__depurar(const {m} *self) {{")];
        if s.es_union {
            d.push(format!("    return kami_texto_desde(\"{m} {{ .. }}\");"));
            d.push("}".into());
            self.defs.extend(d);
            return Ok(());
        }
        d.push(format!("    KamiTexto r = kami_texto_desde(\"{m} {{{{ \");"));
        for (i, c) in s.campos.iter().enumerate() {
            if i > 0 {
                d.push("    kami_texto_empuja_n(&r, \", \", 2);".into());
            }
            d.push(format!("    kami_texto_empuja(&r, \"{}: \");", c.nombre));
            d.extend(self.stmts_depurar_campo(&format!("self->{}", c.nombre), &c.tipo, "r", "    ")?);
        }
        d.push("    kami_texto_empuja_n(&r, \" }\", 2);".into());
        d.push("    return r;".into());
        d.push("}".into());
        self.defs.extend(d);
        Ok(())
    }

    fn emite_depurar_enum(&mut self, m: &str, e: &super::baja::InfoEnum) -> Result<(), CError> {
        self.protos.push(format!("KamiTexto {m}__depurar(const {m} *self);"));
        let mut d = vec![format!("/* Depura {m} (como {{:?}}). */"), format!("KamiTexto {m}__depurar(const {m} *self) {{")];
        d.push("    KamiTexto r = kami_texto_nuevo();".into());
        d.push("    switch (self->etiqueta) {".into());
        for v in &e.variantes {
            let tag = format!("{m}_{}", v.nombre);
            match &v.campos {
                CamposVariante::Unit => {
                    d.push(format!("    case {tag}: kami_texto_empuja(&r, \"{}\"); break;", v.nombre));
                }
                CamposVariante::Tuple(ts) => {
                    d.push(format!("    case {tag}: {{"));
                    d.push(format!("        kami_texto_empuja(&r, \"{}(\");", v.nombre));
                    for (i, t) in ts.iter().enumerate() {
                        if i > 0 {
                            d.push("        kami_texto_empuja_n(&r, \", \", 2);".into());
                        }
                        d.extend(self.stmts_depurar_campo(
                            &format!("self->datos.{}._{i}", v.nombre),
                            t,
                            "r",
                            "        ",
                        )?);
                    }
                    d.push("        kami_texto_empuja_n(&r, \")\", 1);".into());
                    d.push("        break;".into());
                    d.push("    }".into());
                }
                CamposVariante::Struct(cs) => {
                    d.push(format!("    case {tag}: {{"));
                    d.push(format!("        kami_texto_empuja(&r, \"{} {{{{ \");", v.nombre));
                    for (i, c) in cs.iter().enumerate() {
                        if i > 0 {
                            d.push("        kami_texto_empuja_n(&r, \", \", 2);".into());
                        }
                        d.push(format!("        kami_texto_empuja(&r, \"{}: \");", c.nombre));
                        d.extend(self.stmts_depurar_campo(
                            &format!("self->datos.{}.{}", v.nombre, c.nombre),
                            &c.tipo,
                            "r",
                            "        ",
                        )?);
                    }
                    d.push("        kami_texto_empuja_n(&r, \" }\", 2);".into());
                    d.push("        break;".into());
                    d.push("    }".into());
                }
            }
        }
        d.push("    default: kami_texto_empuja(&r, \"<?>\"); break;".into());
        d.push("    }".into());
        d.push("    return r;".into());
        d.push("}".into());
        self.defs.extend(d);
        Ok(())
    }

    // ------------------------------------------------------------------
    // Tipos: orden topológico y emisión
    // ------------------------------------------------------------------

    /// Registra conversión `&T → &din R` (para emitir la instancia de vtabla).
    pub fn registra_dyn(&mut self, rasgo: &str, tipo: &str) {
        let _ = (rasgo, tipo);
        // Las instancias se emiten para todo impl conocido del rasgo usado;
        // esta función queda como punto de extensión (conteo de usos).
    }

    pub fn emite_tipos(&mut self) -> Result<Vec<String>, CError> {
        // Nodos: structs, enums, monos, alias.
        #[derive(Clone, PartialEq, Eq, Hash)]
        enum Nodo {
            Struct(String),
            Enum(String),
            Mono(String),
            Alias(String),
        }
        let mut nodos: Vec<Nodo> = Vec::new();
        let mut es_struct: HashSet<String> = HashSet::new();
        let mut es_enum: HashSet<String> = HashSet::new();
        for n in self.structs.keys() {
            nodos.push(Nodo::Struct(n.clone()));
            es_struct.insert(n.clone());
        }
        for n in self.enums.keys() {
            nodos.push(Nodo::Enum(n.clone()));
            es_enum.insert(n.clone());
        }
        for n in self.monos_orden.clone() {
            nodos.push(Nodo::Mono(n));
        }
        // Deps: nombre → nombres que deben ir antes.
        let mut deps: HashMap<Nodo, HashSet<Nodo>> = HashMap::new();
        let mut clave_usuario = |n: &str| -> Option<Nodo> {
            if es_struct.contains(n) {
                Some(Nodo::Struct(n.to_string()))
            } else if es_enum.contains(n) {
                Some(Nodo::Enum(n.to_string()))
            } else {
                None
            }
        };
        // Recolecta menciones (valor) y menciones a enums (siempre ordenadas).
        fn menciones(t: &CType, out_valor: &mut Vec<String>, out_enum: &mut Vec<String>) {
            match t {
                CType::Usuario(n) => {
                    out_valor.push(n.clone());
                    out_enum.push(n.clone());
                }
                CType::Vec(x) | CType::Opcion(x) | CType::Rebana(x, _) => {
                    menciones(x, out_valor, out_enum)
                }
                CType::Resultado(a, b) => {
                    menciones(a, out_valor, out_enum);
                    menciones(b, out_valor, out_enum);
                }
                CType::Tupla(v) => {
                    for x in v {
                        menciones(x, out_valor, out_enum);
                    }
                }
                CType::Arreglo(x, _) => menciones(x, out_valor, out_enum),
                // Detrás de puntero no hay dependencia de orden (fwd sirve),
                // SALVO enums (C no admite fwd de enum).
                CType::Ref(_, x) | CType::Ptr(_, x) | CType::Caja(x) => {
                    let mut vv = Vec::new();
                    let mut ve = Vec::new();
                    menciones(x, &mut vv, &mut ve);
                    out_enum.extend(ve);
                }
                CType::FnPtr { params, ret } => {
                    for x in params {
                        let mut vv = Vec::new();
                        let mut ve = Vec::new();
                        menciones(x, &mut vv, &mut ve);
                        out_enum.extend(ve);
                    }
                    let mut vv = Vec::new();
                    let mut ve = Vec::new();
                    menciones(ret, &mut vv, &mut ve);
                    out_enum.extend(ve);
                }
                _ => {}
            }
        }
        // Aristas.
        for (nombre, s) in &self.structs {
            let mut dep = HashSet::new();
            for c in &s.campos {
                let mut vv = Vec::new();
                let mut ve = Vec::new();
                menciones(&c.tipo, &mut vv, &mut ve);
                for u in vv {
                    if u != *nombre {
                        if let Some(k) = clave_usuario(&u) {
                            dep.insert(k);
                        }
                    }
                }
                for u in ve {
                    if u != *nombre && es_enum.contains(&u) {
                        dep.insert(Nodo::Enum(u));
                    }
                }
                // Monos mencionados.
                for mm in monos_mencionados(&c.tipo) {
                    dep.insert(Nodo::Mono(mm));
                }
            }
            deps.insert(Nodo::Struct(nombre.clone()), dep);
        }
        for (nombre, e) in &self.enums {
            let mut dep = HashSet::new();
            let mut tipos = Vec::new();
            for v in &e.variantes {
                match &v.campos {
                    CamposVariante::Unit => {}
                    CamposVariante::Tuple(ts) => tipos.extend(ts.clone()),
                    CamposVariante::Struct(cs) => tipos.extend(cs.iter().map(|c| c.tipo.clone())),
                }
            }
            for t in tipos {
                let mut vv = Vec::new();
                let mut ve = Vec::new();
                menciones(&t, &mut vv, &mut ve);
                for u in vv {
                    if u != *nombre {
                        if let Some(k) = clave_usuario(&u) {
                            dep.insert(k);
                        }
                    }
                }
                for u in ve {
                    if u != *nombre && es_enum.contains(&u) {
                        dep.insert(Nodo::Enum(u));
                    }
                }
                for mm in monos_mencionados(&t) {
                    dep.insert(Nodo::Mono(mm));
                }
            }
            deps.insert(Nodo::Enum(nombre.clone()), dep);
        }
        for m in self.monos_orden.clone() {
            let t = self.monos[&m].clone();
            let mut dep = HashSet::new();
            let mut vv = Vec::new();
            let mut ve = Vec::new();
            // El contenido del mono debe estar completo (incluso Vec: `E(*)[N]`).
            menciones_contenido(&t, &mut vv, &mut ve);
            for u in vv {
                if let Some(k) = clave_usuario(&u) {
                    dep.insert(k);
                }
            }
            for u in ve {
                if es_enum.contains(&u) {
                    dep.insert(Nodo::Enum(u));
                }
            }
            for mm in monos_mencionados(&t) {
                if mm != m {
                    dep.insert(Nodo::Mono(mm));
                }
            }
            deps.insert(Nodo::Mono(m), dep);
        }
        // Orden topológico (Kahn) con desempate por orden fuente.
        let mut orden_idx: HashMap<Nodo, usize> = HashMap::new();
        for (n, s) in &self.structs {
            orden_idx.insert(Nodo::Struct(n.clone()), s.orden);
        }
        for (n, e) in &self.enums {
            orden_idx.insert(Nodo::Enum(n.clone()), 100000 + e.orden);
        }
        for (i, m) in self.monos_orden.iter().enumerate() {
            orden_idx.insert(Nodo::Mono(m.clone()), 200000 + i);
        }
        let mut emitidos: HashSet<Nodo> = HashSet::new();
        let mut resultado: Vec<Nodo> = Vec::new();
        let total = nodos.len();
        while resultado.len() < total {
            let mut cands: Vec<Nodo> = nodos
                .iter()
                .filter(|n| {
                    !emitidos.contains(*n)
                        && deps
                            .get(*n)
                            .map(|d| d.iter().all(|x| emitidos.contains(x)))
                            .unwrap_or(true)
                })
                .cloned()
                .collect();
            if cands.is_empty() {
                let resto: Vec<String> = nodos
                    .iter()
                    .filter(|n| !emitidos.contains(*n))
                    .map(|n| match n {
                        Nodo::Struct(s) | Nodo::Enum(s) | Nodo::Mono(s) | Nodo::Alias(s) => {
                            s.clone()
                        }
                    })
                    .collect();
                return Err(CError::nuevo(
                    "C0003",
                    format!("ciclo entre tipos: {}", resto.join(", ")),
                )
                .con_archivo(self.archivo.clone())
                .ayuda("C no admite ciclos struct↔enum ni enums recursivos por valor; rompe el ciclo (p.ej. con un índice o Box + fwd)"));
            }
            cands.sort_by_key(|n| orden_idx.get(n).cloned().unwrap_or(usize::MAX));
            let elegido = cands.into_iter().next().unwrap();
            emitidos.insert(elegido.clone());
            resultado.push(elegido);
        }

        let mut out = Vec::new();
        for nodo in resultado {
            match nodo {
                Nodo::Struct(n) => out.extend(self.emite_struct(&n)?),
                Nodo::Enum(n) => out.extend(self.emite_enum(&n)?),
                Nodo::Mono(m) => out.extend(self.emite_mono(&m)?),
                Nodo::Alias(_) => {}
            }
        }
        // Alias al final (ya resueltos, solo typedef).
        let mut alias_nombres: Vec<String> = self.alias.keys().cloned().collect();
        alias_nombres.sort();
        for a in alias_nombres {
            let t = self.alias[&a].clone();
            match &t {
                CType::Arreglo(e, l) => {
                    out.push(format!("typedef {} {a}[{}];", e.deletrea()?, l.deletrea()));
                }
                CType::FnPtr { params, ret } => {
                    let ps: Vec<String> = params
                        .iter()
                        .map(|p| p.deletrea())
                        .collect::<Result<_, _>>()?;
                    let ps = if ps.is_empty() { "void".into() } else { ps.join(", ") };
                    out.push(format!("typedef {} (*{a})({ps});", ret.deletrea()?));
                }
                _ => out.push(format!("typedef {} {a};", t.deletrea()?)),
            }
        }
        // Vtables + dyns de rasgos (solo los usados como `din`: si nadie
        // nombra el objeto, sus tipos sobran y confunden).
        let mut rasgos: Vec<String> = self.rasgos.keys().cloned().collect();
        rasgos.sort();
        for r in rasgos {
            let usado = self.dyn_usados.iter().any(|d| {
                d == &r || d.ends_with(&format!("__{r}")) || r.ends_with(&format!("__{d}"))
            });
            if usado {
                out.extend(self.emite_rasgo_tipos(&r)?);
            }
        }
        if self.dyn_usados.iter().any(|r| r == "Display" || r.ends_with("__Display")) {
            out.extend(self.emite_display_tipos());
        }
        if self.dyn_usados.iter().any(|r| r == "Debug" || r.ends_with("__Debug")) {
            out.extend(self.emite_debug_tipos());
        }
        Ok(out)
    }

    fn emite_struct(&self, n: &str) -> Result<Vec<String>, CError> {
        let s = &self.structs[n];
        let palabra = if s.es_union { "union" } else { "struct" };
        let mut out = s.docs.clone();
        out.push(format!("typedef {palabra} {n} {{"));
        if s.campos.is_empty() {
            out.push("    char _sin_campos; /* C no admite structs vacíos */".into());
        }
        for c in &s.campos {
            let d = c.tipo.declara(&tipos::higieniza(&c.nombre))?;
            out.push(format!("    {d}; /* {} */", c.tipo.a_rust()));
        }
        out.push(format!("}} {n};"));
        Ok(out)
    }

    fn emite_enum(&mut self, n: &str) -> Result<Vec<String>, CError> {
        let e = self.enums[n].clone();
        let con_datos = e
            .variantes
            .iter()
            .any(|v| !matches!(v.campos, CamposVariante::Unit));
        let mut out = e.docs.clone();
        if !con_datos {
            out.push("typedef enum {".into());
            for (i, v) in e.variantes.iter().enumerate() {
                let tag = format!("{n}_{}", v.nombre);
                if let Some(d) = &v.disc {
                    let (c, _) = self.baja_expr_const(d)?;
                    out.push(format!("    {tag} = ({c}){comma}", comma = if i + 1 < e.variantes.len() { "," } else { "" }));
                } else {
                    out.push(format!("    {tag}{comma}", comma = if i + 1 < e.variantes.len() { "," } else { "" }));
                }
            }
            out.push(format!("}} {n};"));
            return Ok(out);
        }
        out.push(format!("typedef enum {{"));
        for (i, v) in e.variantes.iter().enumerate() {
            let tag = format!("{n}_{}", v.nombre);
            // Los discriminantes explícitos en enums con datos se ignoran en la
            // etiqueta (C no los necesita); el orden es el de declaración.
            let _ = i;
            out.push(format!("    {tag},"));
        }
        out.push(format!("}} {n}__etiqueta;"));
        out.push("typedef struct {".into());
        out.push(format!("    {n}__etiqueta etiqueta;"));
        out.push("    union {".into());
        for v in &e.variantes {
            match &v.campos {
                CamposVariante::Unit => {}
                CamposVariante::Tuple(ts) => {
                    out.push(format!("        struct {{"));
                    for (i, t) in ts.iter().enumerate() {
                        out.push(format!("            {};", t.declara(&format!("_{i}"))?));
                    }
                    out.push(format!("        }} {};", v.nombre));
                }
                CamposVariante::Struct(cs) => {
                    out.push(format!("        struct {{"));
                    for c in cs {
                        out.push(format!("            {};", c.tipo.declara(&tipos::higieniza(&c.nombre))?));
                    }
                    out.push(format!("        }} {};", v.nombre));
                }
            }
        }
        out.push("    } datos;".into());
        out.push(format!("}} {n};"));
        Ok(out)
    }

    /// Campo `datos` de Vec/vista para un elemento (maneja arreglos).
    fn campo_datos(elem: &CType, cnst: bool) -> Result<String, CError> {
        if let CType::Arreglo(e, l) = elem {
            let c = if cnst { "const " } else { "" };
            return Ok(format!("{c}{} (*datos)[{}]", e.deletrea()?, l.deletrea()));
        }
        if matches!(elem, CType::FnPtr { .. }) {
            return Err(CError::nuevo(
                "C0003",
                "listas de funciones no soportadas".to_string(),
            ));
        }
        let c = if cnst { "const " } else { "" };
        Ok(format!("{c}{}* datos", elem.deletrea()?))
    }

    fn emite_mono(&mut self, m: &str) -> Result<Vec<String>, CError> {
        let t = self.monos[m].clone();
        match t {
            CType::Vec(e) => Ok(vec![
                "typedef struct {".into(),
                format!("    {};", Self::campo_datos(&e, false)?),
                "    size_t largo;".into(),
                "    size_t capacidad;".into(),
                format!("}} {m};"),
            ]),
            CType::Opcion(e) => Ok(vec![
                "typedef struct {".into(),
                "    bool tiene;".into(),
                format!("    {};", e.declara("valor")?),
                format!("}} {m};"),
            ]),
            CType::Resultado(a, b) => Ok(vec![
                "typedef struct {".into(),
                "    bool es_ok;".into(),
                "    union {".into(),
                format!("        {};", a.declara("ok")?),
                format!("        {};", b.declara("err")?),
                "    } datos;".into(),
                format!("}} {m};"),
            ]),
            CType::Tupla(v) => {
                let mut out = vec!["typedef struct {".into()];
                for (i, t) in v.iter().enumerate() {
                    out.push(format!("    {};", t.declara(&format!("_{i}"))?));
                }
                out.push(format!("}} {m};"));
                Ok(out)
            }
            CType::Rebana(e, mutable) => Ok(vec![
                "typedef struct {".into(),
                format!("    {};", Self::campo_datos(&e, !mutable)?),
                "    size_t largo;".into(),
                format!("}} {m};"),
            ]),
            _ => Ok(vec![]),
        }
    }

    fn firma_metodo_vtabla(
        &self,
        rasgo: &str,
        nombre: &str,
        receptor: Receptor,
        params: &[(String, CType)],
        ret: &CType,
    ) -> Result<(String, String), CError> {
        // Devuelve (campo de vtabla, params de despacho).
        let recv = match receptor {
            Receptor::Ref => "const void *yo".to_string(),
            Receptor::Mut => "void *yo".to_string(),
            _ => {
                return Err(CError::nuevo(
                    "C0003",
                    format!("`{rasgo}::{nombre}` no sirve para `din` (sin receptor válido)"),
                )
                .con_archivo(self.archivo.clone()))
            }
        };
        for (_, t) in params {
            if contiene_self(t, rasgo) {
                return Err(CError::nuevo(
                    "C0003",
                    format!("`{rasgo}::{nombre}` usa `Yo` en la firma: no sirve para `din`"),
                )
                .con_archivo(self.archivo.clone()));
            }
        }
        if contiene_self(ret, rasgo) {
            return Err(CError::nuevo(
                "C0003",
                format!("`{rasgo}::{nombre}` devuelve `Yo`: no sirve para `din`"),
            )
            .con_archivo(self.archivo.clone()));
        }
        let mut ps = vec![recv];
        for (n, t) in params {
            ps.push(t.declara(&tipos::higieniza(n))?);
        }
        let campo = format!("{} (*{nombre})({});", ret.deletrea()?, ps.join(", "));
        Ok((campo, ps.join(", ")))
    }

    fn emite_rasgo_tipos(&mut self, r: &str) -> Result<Vec<String>, CError> {
        let info = self.rasgos[r].clone();
        let mut out = vec![format!("typedef struct {r}__vtabla {r}__vtabla;")];
        out.push(format!("struct {r}__vtabla {{"));
        for m in &info.metodos {
            let (campo, _) =
                self.firma_metodo_vtabla(r, &m.nombre, m.receptor, &m.params, &m.ret)?;
            out.push(format!("    {campo}"));
        }
        out.push("    void (*suelta)(void *objeto);".into());
        out.push("};".into());
        out.push("typedef struct {".into());
        out.push(format!("    const {r}__vtabla *vtabla;"));
        out.push("    void *objeto;".into());
        out.push(format!("}} {r}__dyn;"));
        // Despacho inline.
        for m in &info.metodos {
            let mut ps = vec![format!("const {r}__dyn *yo")];
            let mut args = vec![match m.receptor {
                Receptor::Ref => "(const void *)yo->objeto".to_string(),
                _ => "yo->objeto".to_string(),
            }];
            for (n, t) in &m.params {
                let nn = tipos::higieniza(n);
                ps.push(t.declara(&nn)?);
                args.push(nn);
            }
            let ret = m.ret.deletrea()?;
            if m.ret.es_vacio() {
                out.push(format!(
                    "static inline void {r}__{mn}({ps}) {{ yo->vtabla->{mn}({args}); }}",
                    mn = m.nombre,
                    ps = ps.join(", "),
                    args = args.join(", ")
                ));
            } else {
                out.push(format!(
                    "static inline {ret} {r}__{mn}({ps}) {{ return yo->vtabla->{mn}({args}); }}",
                    mn = m.nombre,
                    ps = ps.join(", "),
                    args = args.join(", ")
                ));
            }
        }
        Ok(out)
    }

    fn emite_display_tipos(&self) -> Vec<String> {
        vec![
            "typedef struct Display__vtabla Display__vtabla;".into(),
            "struct Display__vtabla {".into(),
            "    KamiTexto (*fmt)(const void *yo);".into(),
            "    void (*suelta)(void *objeto);".into(),
            "};".into(),
            "typedef struct {".into(),
            "    const Display__vtabla *vtabla;".into(),
            "    void *objeto;".into(),
            "} Display__dyn;".into(),
            "static inline KamiTexto Display__fmt(const Display__dyn *yo) { return yo->vtabla->fmt((const void *)yo->objeto); }".into(),
        ]
    }

    fn emite_debug_tipos(&self) -> Vec<String> {
        vec![
            "typedef struct Debug__vtabla Debug__vtabla;".into(),
            "struct Debug__vtabla {".into(),
            "    KamiTexto (*depurar)(const void *yo);".into(),
            "    void (*suelta)(void *objeto);".into(),
            "};".into(),
            "typedef struct {".into(),
            "    const Debug__vtabla *vtabla;".into(),
            "    void *objeto;".into(),
            "} Debug__dyn;".into(),
            "static inline KamiTexto Debug__depurar(const Debug__dyn *yo) { return yo->vtabla->depurar((const void *)yo->objeto); }".into(),
        ]
    }

    /// Instancias de vtabla para cada (`din R`, `T` impl) + drop-glue.
    pub fn emite_vtablas(&mut self) -> Result<(), CError> {
        // Agrupa impls por rasgo.
        let mut por_rasgo: HashMap<String, Vec<String>> = HashMap::new();
        for (r, t) in self.impl_rasgos.keys() {
            por_rasgo.entry(r.clone()).or_default().push(t.clone());
        }
        let mut rasgos: Vec<String> = self.dyn_usados.iter().cloned().collect();
        rasgos.sort();
        for r in rasgos {
            if r == "Display" || r.ends_with("__Display") {
                let mut tipos: Vec<String> = self.display.iter().cloned().collect();
                tipos.sort();
                for t in tipos {
                    self.estaticas.push(format!(
                        "static const Display__vtabla {t}__como__Display__vtabla = {{"
                    ));
                    self.estaticas
                        .push(format!("    .fmt = {t}__Display__fmt,"));
                    self.estaticas.push(format!("    .suelta = {t}__Display__suelta,"));
                    self.estaticas.push("};".into());
                    let dropit = self.necesita_drop(&CType::Usuario(t.clone()));
                    self.defs.push(format!("static void {t}__Display__suelta(void *p) {{"));
                    if dropit {
                        self.pide_ayuda_reg(
                            "liberar",
                            &t,
                            &CType::Usuario(t.clone()),
                        );
                        self.defs.push(format!("    {t}__liberar(({t} *)p);"));
                    } else {
                        self.defs.push("    (void)p;".into());
                    }
                    self.defs.push("}".into());
                }
                continue;
            }
            if r == "Debug" || r.ends_with("__Debug") {
                // Las instancias de Debug se emiten al convertir (ver expr.rs).
                continue;
            }
            let Some(info) = self.rasgos.get(&r).cloned() else {
                continue;
            };
            // Valida objeto.
            for m in &info.metodos {
                if m.receptor == Receptor::Ninguno || m.receptor == Receptor::Valor {
                    return Err(CError::nuevo(
                        "C0003",
                        format!("`{r}` no sirve como objeto `din` (revisa `{}`)", m.nombre),
                    )
                    .con_archivo(self.archivo.clone()));
                }
            }
            let mut tipos = por_rasgo.get(&r).cloned().unwrap_or_default();
            tipos.sort();
            for t in tipos {
                let mapa = self.impl_rasgos.get(&(r.clone(), t.clone())).cloned().unwrap_or_default();
                self.estaticas
                    .push(format!("/* Vtabla: {t} como dyn {r}. */"));
                self.estaticas
                    .push(format!("static const {r}__vtabla {t}__como__{r}__vtabla = {{"));
                for m in &info.metodos {
                    let sig = mapa.get(&m.nombre).ok_or_else(|| {
                        CError::nuevo(
                            "C0003",
                            format!("`implementa {r} para {t}` no define `{}`", m.nombre),
                        )
                        .con_archivo(self.archivo.clone())
                    })?;
                    self.estaticas.push(format!("    .{mn} = {f},", mn = m.nombre, f = sig.nombre_c));
                }
                self.estaticas.push(format!("    .suelta = {t}__{r}__suelta,"));
                self.estaticas.push("};".into());
                let dropit = self.necesita_drop(&CType::Usuario(t.clone()));
                self.defs.push(format!("/* Libera {t} guardado como dyn {r}. */"));
                self.defs.push(format!("static void {t}__{r}__suelta(void *p) {{"));
                if dropit {
                    self.pide_ayuda_reg("liberar", &t, &CType::Usuario(t.clone()));
                    self.defs.push(format!("    {t}__liberar(({t} *)p);"));
                } else {
                    self.defs.push("    (void)p;".into());
                }
                self.defs.push("}".into());
            }
        }
        Ok(())
    }

    // ------------------------------------------------------------------
    // Ensamblado
    // ------------------------------------------------------------------

    pub fn ensambla(&mut self, autonomo: bool) -> Result<String, CError> {
        self.emite_vtablas()?;
        let mut out = String::new();
        out.push_str(&format!(
            "/* Generado por kamisolari c {} desde {}. No editar a mano: edita el .kami. */\n",
            env!("CARGO_PKG_VERSION"),
            self.archivo
        ));
        if autonomo {
            out.push_str("/* Runtime inline (--autonomo). */\n");
            out.push_str(super::RUNTIME_KAMI_H);
            out.push_str("\n");
        } else {
            out.push_str("#include \"kami.h\" /* va al lado; compila: cc -std=c11 -O2 prog.c -lm */\n");
        }
        if self.usa_math {
            out.push_str("#include <math.h>\n");
        }
        let mut defines = String::new();
        for c in &self.consts_orden.clone() {
            let info = &self.consts[c];
            if let Some(d) = &info.define {
                defines.push_str(&format!("#define {} {}\n", info.nombre_c, d));
            }
        }
        if !defines.is_empty() {
            out.push_str("\n/* === defines === */\n");
            out.push_str(&defines);
        }
        // fwd de structs/uniones.
        let mut fwd: Vec<String> = self
            .structs
            .values()
            .map(|s| {
                let palabra = if s.es_union { "union" } else { "struct" };
                format!("typedef {palabra} {n} {n};", n = s.nombre)
            })
            .collect();
        fwd.sort();
        if !fwd.is_empty() {
            out.push_str("\n/* === fwd === */\n");
            for f in fwd {
                out.push_str(&f);
                out.push('\n');
            }
        }
        let tipos = self.emite_tipos()?;
        if !tipos.is_empty() {
            out.push_str("\n/* === tipos === */\n");
            for t in &tipos {
                out.push_str(t);
                out.push('\n');
            }
        }
        if !self.protos.is_empty() {
            out.push_str("\n/* === prototipos === */\n");
            for p in &self.protos.clone() {
                out.push_str(p);
                out.push('\n');
            }
        }
        if !self.estaticas.is_empty() {
            out.push_str("\n/* === vtables === */\n");
            for s in &self.estaticas.clone() {
                out.push_str(s);
                out.push('\n');
            }
        }
        if !self.consts_out.is_empty() {
            out.push_str("\n/* === consts === */\n");
            for s in &self.consts_out.clone() {
                out.push_str(s);
                out.push('\n');
            }
        }
        if !self.defs.is_empty() {
            out.push_str("\n/* === definiciones === */\n");
            for s in &self.defs.clone() {
                out.push_str(s);
                out.push('\n');
            }
        }
        Ok(super::pulir::pulir(&out))
    }
}

/// ¿Menciona `Self` (como `Usuario(rasgo)`)?
fn contiene_self(t: &CType, rasgo: &str) -> bool {
    match t {
        CType::Usuario(n) => n == rasgo,
        CType::Vec(x) | CType::Opcion(x) | CType::Caja(x) | CType::Rebana(x, _)
        | CType::Ref(_, x) | CType::Ptr(_, x) => contiene_self(x, rasgo),
        CType::Resultado(a, b) => contiene_self(a, rasgo) || contiene_self(b, rasgo),
        CType::Tupla(v) => v.iter().any(|x| contiene_self(x, rasgo)),
        CType::Arreglo(x, _) => contiene_self(x, rasgo),
        CType::FnPtr { params, ret } => {
            params.iter().any(|x| contiene_self(x, rasgo)) || contiene_self(ret, rasgo)
        }
        _ => false,
    }
}

/// Monos mencionados dentro de un tipo.
fn monos_mencionados(t: &CType) -> Vec<String> {
    let mut out = Vec::new();
    fn rec(t: &CType, out: &mut Vec<String>) {
        match t {
            CType::Vec(_) | CType::Opcion(_) | CType::Resultado(_, _) | CType::Rebana(_, _) => {
                if let Ok(m) = t.mangle() {
                    out.push(m);
                }
            }
            CType::Tupla(v) if !v.is_empty() => {
                if let Ok(m) = t.mangle() {
                    out.push(m);
                }
            }
            _ => {}
        }
        match t {
            CType::Vec(x) | CType::Opcion(x) | CType::Caja(x) | CType::Rebana(x, _)
            | CType::Ref(_, x) | CType::Ptr(_, x) | CType::Arreglo(x, _) => rec(x, out),
            CType::Resultado(a, b) => {
                rec(a, out);
                rec(b, out);
            }
            CType::Tupla(v) => {
                for x in v {
                    rec(x, out);
                }
            }
            CType::FnPtr { params, ret } => {
                for x in params {
                    rec(x, out);
                }
                rec(ret, out);
            }
            _ => {}
        }
    }
    rec(t, &mut out);
    out
}

/// Menciones de contenido (para ordenar monos: el elemento debe estar completo).
fn menciones_contenido(t: &CType, out_valor: &mut Vec<String>, out_enum: &mut Vec<String>) {
    match t {
        CType::Vec(x) | CType::Opcion(x) | CType::Rebana(x, _) => {
            contenido(x, out_valor, out_enum)
        }
        CType::Resultado(a, b) => {
            contenido(a, out_valor, out_enum);
            contenido(b, out_valor, out_enum);
        }
        CType::Tupla(v) => {
            for x in v {
                contenido(x, out_valor, out_enum);
            }
        }
        _ => {}
    }
}

fn contenido(t: &CType, out_valor: &mut Vec<String>, out_enum: &mut Vec<String>) {
    match t {
        CType::Usuario(n) => {
            out_valor.push(n.clone());
            out_enum.push(n.clone());
        }
        CType::Vec(x) | CType::Opcion(x) | CType::Rebana(x, _) => contenido(x, out_valor, out_enum),
        CType::Resultado(a, b) => {
            contenido(a, out_valor, out_enum);
            contenido(b, out_valor, out_enum);
        }
        CType::Tupla(v) => {
            for x in v {
                contenido(x, out_valor, out_enum);
            }
        }
        CType::Arreglo(x, _) => contenido(x, out_valor, out_enum),
        CType::Ref(_, x) | CType::Ptr(_, x) | CType::Caja(x) => {
            let mut vv = Vec::new();
            let mut ve = Vec::new();
            contenido(x, &mut vv, &mut ve);
            out_enum.extend(ve);
        }
        _ => {}
    }
}

/// Firma C: `ret nombre(p1, p2)` (prototipo o encabezado).
pub(crate) fn firma_c(nombre: &str, params: &[(String, CType)], ret: &CType) -> Result<String, CError> {
    let ps: Vec<String> = params
        .iter()
        .map(|(n, t)| t.declara(&tipos::higieniza(n)))
        .collect::<Result<_, _>>()?;
    let ps = if ps.is_empty() { "void".to_string() } else { ps.join(", ") };
    Ok(format!("{} {nombre}({ps})", ret.deletrea()?))
}
