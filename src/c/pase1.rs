//! Pase 1: registra nombres (1a) y resuelve tipos (1b).
//!
//! Todo lo que el pase 2 necesita saber de antemano — campos de structs,
//! firmas, variantes, vtables — se junta aquí. Los items anidados dentro
//! de funciones se elevan a globales con nombre `padre__hijo` (en Rust no
//! capturan nada, así que elevar es correcto).

use syn::spanned::Spanned;

use super::baja::{
    Bajador, CampoInfo, CamposVariante, Firma, InfoConst, InfoEnum, InfoRasgo, InfoStruct,
    InfoVariante, MetodoRasgo, Receptor, SigMetodo,
};
use super::errores::CError;
use super::tipos::{self, CType};

impl Bajador {
    pub fn pase1(&mut self, archivo: &syn::File) -> Result<(), CError> {
        self.pase1a_items(&archivo.items)?;
        // Cascada de anidados (cada uno puede traer más adentro).
        let mut i = 0;
        while i < self.anidados.len() {
            let (mods, item) = self.anidados[i].clone();
            let guarda = std::mem::replace(&mut self.mods, mods);
            self.pase1a_item(&item)?;
            self.recoge_cuerpos(&item)?;
            self.mods = guarda;
            i += 1;
        }
        self.pase1b_items(&archivo.items)?;
        let anidados = self.anidados.clone();
        for (mods, item) in &anidados {
            let guarda = std::mem::replace(&mut self.mods, mods.clone());
            self.pase1b_item(item)?;
            self.mods = guarda;
        }
        Ok(())
    }

    // ------------------------------------------------------------------
    // Pase 1a: nombres
    // ------------------------------------------------------------------

    fn pase1a_items(&mut self, items: &[syn::Item]) -> Result<(), CError> {
        for it in items {
            self.pase1a_item(it)?;
            self.recoge_cuerpos(it)?;
        }
        Ok(())
    }

    fn pase1a_item(&mut self, item: &syn::Item) -> Result<(), CError> {
        if !cfg_activo(item_attrs(item)) {
            return Ok(());
        }
        match item {
            syn::Item::Fn(f) => {
                let n = self.nombre_global(&tipos::desnuda(&f.sig.ident));
                self.reserva_global(&n);
                Ok(())
            }
            syn::Item::Struct(s) => self.registra_tipo(&tipos::desnuda(&s.ident)),
            syn::Item::Enum(e) => self.registra_tipo(&tipos::desnuda(&e.ident)),
            syn::Item::Union(u) => self.registra_tipo(&tipos::desnuda(&u.ident)),
            syn::Item::Trait(t) => self.registra_tipo(&tipos::desnuda(&t.ident)),
            syn::Item::Type(t) => self.registra_tipo(&tipos::desnuda(&t.ident)),
            syn::Item::Const(c) => {
                let n = self.nombre_global(&tipos::desnuda(&c.ident));
                self.reserva_global(&n);
                Ok(())
            }
            syn::Item::Static(s) => {
                let n = self.nombre_global(&tipos::desnuda(&s.ident));
                self.reserva_global(&n);
                Ok(())
            }
            syn::Item::Mod(m) => {
                let nombre = tipos::desnuda(&m.ident);
                match &m.content {
                    Some((_, items)) => {
                        self.mods.push(nombre);
                        let r = self.pase1a_items(items);
                        self.mods.pop();
                        r
                    }
                    None => Err(CError::nuevo(
                        "C0003",
                        format!("`mod {nombre};` en otro archivo no soportado"),
                    )
                    .con_span(&m.mod_token, &self.lineas)
                    .con_archivo(self.archivo.clone())
                    .ayuda("pega el módulo inline (`mod nombre { ... }`) o usa un solo archivo")),
                }
            }
            syn::Item::ForeignMod(fm) => {
                for it in &fm.items {
                    if let syn::ForeignItem::Fn(g) = it {
                        let n = self.nombre_global(&tipos::desnuda(&g.sig.ident));
                        self.reserva_global(&n);
                    }
                    if let syn::ForeignItem::Static(s) = it {
                        let n = self.nombre_global(&tipos::desnuda(&s.ident));
                        self.reserva_global(&n);
                    }
                }
                Ok(())
            }
            syn::Item::Impl(_) | syn::Item::Use(_) | syn::Item::ExternCrate(_) => Ok(()),
            syn::Item::Macro(m) => {
                if m.ident.is_some() {
                    return Err(CError::no_soportado(
                        "definir macros con `macro_rules!` no soportado",
                    )
                    .con_span(&m.mac.path, &self.lineas)
                    .con_archivo(self.archivo.clone())
                    .ayuda("usa funciones; el backend C solo trae las macros de std comunes"));
                }
                Err(CError::no_soportado("macro a nivel de item no soportada")
                    .con_span(&m.mac.path, &self.lineas)
                    .con_archivo(self.archivo.clone()))
            }
            syn::Item::Verbatim(v) => Err(CError::no_soportado("item no reconocido")
                .con_span(v, &self.lineas)
                .con_archivo(self.archivo.clone())),
            _ => Err(CError::no_soportado("item no soportado por el backend C")
                .con_span(item, &self.lineas)
                .con_archivo(self.archivo.clone())),
        }
    }

    fn registra_tipo(&mut self, base: &str) -> Result<(), CError> {
        let n = self.nombre_global(base);
        self.reserva_global(&n);
        self.propio_set.insert(n);
        Ok(())
    }

    /// Busca items anidados en los cuerpos de funciones/métodos.
    fn recoge_cuerpos(&mut self, item: &syn::Item) -> Result<(), CError> {
        match item {
            syn::Item::Fn(f) => {
                let mut pref = self.mods.clone();
                pref.push(tipos::desnuda(&f.sig.ident));
                self.recoge_bloque(&f.block, &pref);
                Ok(())
            }
            syn::Item::Impl(im) => {
                for it in &im.items {
                    if let syn::ImplItem::Fn(m) = it {
                        let mut pref = self.mods.clone();
                        pref.push(format!("impl_{}", self.anidados.len()));
                        self.recoge_bloque(&m.block, &pref);
                    }
                }
                Ok(())
            }
            syn::Item::Mod(m) => {
                if let Some((_, items)) = &m.content {
                    let mut pref = self.mods.clone();
                    pref.push(tipos::desnuda(&m.ident));
                    self.recoge_items_anidados(items, &pref);
                }
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn recoge_bloque(&mut self, bloque: &syn::Block, pref: &[String]) {
        recoge_items_bloque(bloque, pref, &mut self.anidados);
    }

    fn recoge_items_anidados(&mut self, items: &[syn::Item], pref: &[String]) {
        for it in items {
            match it {
                syn::Item::Fn(f) => {
                    let mut p2 = pref.to_vec();
                    p2.push(tipos::desnuda(&f.sig.ident));
                    self.recoge_bloque(&f.block, &p2);
                }
                syn::Item::Mod(m) => {
                    if let Some((_, inner)) = &m.content {
                        let mut p2 = pref.to_vec();
                        p2.push(tipos::desnuda(&m.ident));
                        self.recoge_items_anidados(inner, &p2);
                    }
                }
                syn::Item::Impl(im) => {
                    for it2 in &im.items {
                        if let syn::ImplItem::Fn(m) = it2 {
                            let mut p2 = pref.to_vec();
                            p2.push(format!("impl_{}", self.anidados.len()));
                            self.recoge_bloque(&m.block, &p2);
                        }
                    }
                }
                _ => {}
            }
        }
    }

    // ------------------------------------------------------------------
    // Pase 1b: tipos
    // ------------------------------------------------------------------

    fn pase1b_items(&mut self, items: &[syn::Item]) -> Result<(), CError> {
        for it in items {
            self.pase1b_item(it)?;
        }
        Ok(())
    }

    fn pase1b_item(&mut self, item: &syn::Item) -> Result<(), CError> {
        if !cfg_activo(item_attrs(item)) {
            return Ok(());
        }
        match item {
            syn::Item::Fn(f) => {
                self.rechaza_genericos(&f.sig.generics, "funciones con genéricos")?;
                let firma = self.baja_firma_fn(&f.sig)?;
                let n = self.nombre_global(&tipos::desnuda(&f.sig.ident));
                self.fns.insert(n, firma);
                Ok(())
            }
            syn::Item::Struct(s) => {
                self.rechaza_genericos(&s.generics, "estructuras con genéricos")?;
                let n = self.nombre_global(&tipos::desnuda(&s.ident));
                let campos = self.baja_campos(&s.fields)?;
                let orden = self.structs.len();
                self.structs.insert(
                    n.clone(),
                    InfoStruct {
                        nombre: n,
                        campos,
                        es_union: false,
                        orden,
                        docs: docs_de(&s.attrs),
                    },
                );
                Ok(())
            }
            syn::Item::Union(u) => {
                self.rechaza_genericos(&u.generics, "uniones con genéricos")?;
                let n = self.nombre_global(&tipos::desnuda(&u.ident));
                let campos = self.baja_campos(&syn::Fields::Named(u.fields.clone()))?;
                let orden = self.structs.len();
                self.structs.insert(
                    n.clone(),
                    InfoStruct {
                        nombre: n,
                        campos,
                        es_union: true,
                        orden,
                        docs: docs_de(&u.attrs),
                    },
                );
                Ok(())
            }
            syn::Item::Enum(e) => {
                self.rechaza_genericos(&e.generics, "enumeraciones con genéricos")?;
                let n = self.nombre_global(&tipos::desnuda(&e.ident));
                let mut variantes = Vec::new();
                for v in &e.variants {
                    variantes.push(self.baja_variante(v)?);
                }
                let orden = self.enums.len();
                self.enums.insert(
                    n.clone(),
                    InfoEnum {
                        nombre: n,
                        variantes,
                        orden,
                        docs: docs_de(&e.attrs),
                    },
                );
                Ok(())
            }
            syn::Item::Trait(t) => self.pase1b_rasgo(t),
            syn::Item::Type(t) => {
                self.rechaza_genericos(&t.generics, "alias con genéricos")?;
                let n = self.nombre_global(&tipos::desnuda(&t.ident));
                let res = self.baja_tipo(&t.ty)?;
                self.alias.insert(n, res);
                Ok(())
            }
            syn::Item::Const(c) => {
                let t = self.baja_tipo(&c.ty)?;
                let n = self.nombre_global(&tipos::desnuda(&c.ident));
                let define = match (&t, &*c.expr) {
                    (tt, syn::Expr::Lit(l)) if es_int_c(tt) => {
                        if let syn::Lit::Int(i) = &l.lit {
                            Some(format!("{}({})", deletrea_define(tt), i.base10_digits()))
                        } else {
                            None
                        }
                    }
                    _ => None,
                };
                self.consts.insert(
                    n.clone(),
                    InfoConst {
                        nombre_c: n.clone(),
                        tipo: t,
                        define,
                        expr: Some((*c.expr).clone()),
                    },
                );
                self.consts_orden.push(n);
                Ok(())
            }
            syn::Item::Static(s) => {
                let t = self.baja_tipo(&s.ty)?;
                let n = self.nombre_global(&tipos::desnuda(&s.ident));
                self.consts.insert(
                    n.clone(),
                    InfoConst {
                        nombre_c: n.clone(),
                        tipo: t,
                        define: None,
                        expr: Some((*s.expr).clone()),
                    },
                );
                self.consts_orden.push(n);
                Ok(())
            }
            syn::Item::Mod(m) => {
                if let Some((_, items)) = &m.content {
                    self.mods.push(tipos::desnuda(&m.ident));
                    let r = self.pase1b_items(items);
                    self.mods.pop();
                    r
                } else {
                    Ok(())
                }
            }
            syn::Item::Impl(im) => self.pase1b_impl(im),
            syn::Item::ForeignMod(fm) => {
                for it in &fm.items {
                    match it {
                        syn::ForeignItem::Fn(g) => {
                            self.rechaza_genericos(&g.sig.generics, "externas con genéricos")?;
                            let firma = self.baja_firma_fn(&g.sig)?;
                            let n = self.nombre_global(&tipos::desnuda(&g.sig.ident));
                            self.fns.insert(n.clone(), firma);
                            self.fn_externa.insert(n);
                        }
                        syn::ForeignItem::Static(s) => {
                            let t = self.baja_tipo(&s.ty)?;
                            let n = self.nombre_global(&tipos::desnuda(&s.ident));
                            self.consts.insert(
                                n.clone(),
                                InfoConst {
                                    nombre_c: n.clone(),
                                    tipo: t,
                                    define: None,
                                    expr: None,
                                },
                            );
                            self.consts_orden.push(n);
                        }
                        _ => {
                            return Err(CError::no_soportado(
                                "ese item `externo` no soportado (usa funciones o estáticas)",
                            )
                            .con_span(it, &self.lineas)
                            .con_archivo(self.archivo.clone()))
                        }
                    }
                }
                Ok(())
            }
            syn::Item::Use(_) | syn::Item::ExternCrate(_) => Ok(()),
            _ => Ok(()),
        }
    }

    fn rechaza_genericos(&self, g: &syn::Generics, que: &str) -> Result<(), CError> {
        for p in &g.params {
            match p {
                syn::GenericParam::Lifetime(_) => {}
                _ => {
                    return Err(CError::nuevo(
                        "C0003",
                        format!("{que} no caben en C (sin monomorfización general)"),
                    )
                    .con_span(p, &self.lineas)
                    .con_archivo(self.archivo.clone())
                    .ayuda(
                        "usa tipos concretos; `Lista<T>`, `Opcion<T>` y `Resultado<T, E>` sí aceptan parámetros",
                    ))
                }
            }
        }
        if let Some(w) = &g.where_clause {
            if !w.predicates.is_empty() {
                return Err(CError::nuevo("C0003", "cláusulas `donde` no soportadas".to_string())
                    .con_span(w, &self.lineas)
                    .con_archivo(self.archivo.clone()));
            }
        }
        Ok(())
    }

    fn baja_campos(&mut self, fields: &syn::Fields) -> Result<Vec<CampoInfo>, CError> {
        let mut out = Vec::new();
        match fields {
            syn::Fields::Named(f) => {
                for c in &f.named {
                    let id = c.ident.as_ref().unwrap();
                    out.push(CampoInfo {
                        nombre: tipos::desnuda(id),
                        tipo: self.baja_tipo(&c.ty)?,
                    });
                }
            }
            syn::Fields::Unnamed(f) => {
                for (i, c) in f.unnamed.iter().enumerate() {
                    out.push(CampoInfo {
                        nombre: format!("_{i}"),
                        tipo: self.baja_tipo(&c.ty)?,
                    });
                }
            }
            syn::Fields::Unit => {}
        }
        Ok(out)
    }

    fn baja_variante(&mut self, v: &syn::Variant) -> Result<InfoVariante, CError> {
        let disc = v.discriminant.as_ref().map(|(_, d)| d.clone());
        let es_default = v.attrs.iter().any(|a| a.path().is_ident("default"));
        let campos = match &v.fields {
            syn::Fields::Named(f) => {
                let mut cs = Vec::new();
                for c in &f.named {
                    cs.push(CampoInfo {
                        nombre: tipos::desnuda(c.ident.as_ref().unwrap()),
                        tipo: self.baja_tipo(&c.ty)?,
                    });
                }
                CamposVariante::Struct(cs)
            }
            syn::Fields::Unnamed(f) => {
                let mut ts = Vec::new();
                for c in &f.unnamed {
                    ts.push(self.baja_tipo(&c.ty)?);
                }
                CamposVariante::Tuple(ts)
            }
            syn::Fields::Unit => CamposVariante::Unit,
        };
        Ok(InfoVariante {
            nombre: tipos::desnuda(&v.ident),
            campos,
            disc,
            es_default,
        })
    }

    /// Firma de función libre o externa.
    fn baja_firma_fn(&mut self, sig: &syn::Signature) -> Result<Firma, CError> {
        if sig.asyncness.is_some() {
            return Err(CError::no_soportado("funciones `asinc` no caben en C")
                .con_span(&sig.fn_token, &self.lineas)
                .con_archivo(self.archivo.clone())
                .ayuda("usa funciones normales; el backend C no trae un executor"));
        }
        let mut params = Vec::new();
        let mut n = 0;
        for arg in &sig.inputs {
            match arg {
                syn::FnArg::Receiver(r) => {
                    return Err(CError::no_soportado("`yo` fuera de un `implementa`")
                        .con_span(r, &self.lineas)
                        .con_archivo(self.archivo.clone()))
                }
                syn::FnArg::Typed(t) => {
                    let tipo = self.baja_tipo(&t.ty)?;
                    let nombre = match &*t.pat {
                        syn::Pat::Ident(p) => tipos::desnuda(&p.ident),
                        syn::Pat::Wild(_) => format!("arg{n}"),
                        _ => format!("p{n}"),
                    };
                    params.push((nombre, tipo));
                    n += 1;
                }
            }
        }
        let ret = match &sig.output {
            syn::ReturnType::Default => CType::Vacio,
            syn::ReturnType::Type(_, t) => self.baja_tipo(t)?,
        };
        Ok(Firma {
            params,
            ret,
            variadic: sig.variadic.is_some(),
        })
    }

    fn pase1b_rasgo(&mut self, t: &syn::ItemTrait) -> Result<(), CError> {
        self.rechaza_genericos(&t.generics, "rasgos con genéricos")?;
        if !t.supertraits.is_empty() {
            return Err(CError::no_soportado("rasgos con superrasgos (`rasgo A: B`)")
                .con_span(&t.ident, &self.lineas)
                .con_archivo(self.archivo.clone())
                .ayuda("aplana la jerarquía: repite los métodos en cada rasgo"));
        }
        let n = self.nombre_global(&tipos::desnuda(&t.ident));
        let mut metodos = Vec::new();
        let mut tiene_estaticos = false;
        for it in &t.items {
            match it {
                syn::TraitItem::Fn(m) => {
                    self.rechaza_genericos(&m.sig.generics, "métodos con genéricos")?;
                    let (receptor, params, ret) = self.baja_firma_metodo(&m.sig, Some(&n))?;
                    if receptor == Receptor::Ninguno {
                        tiene_estaticos = true;
                    }
                    metodos.push(MetodoRasgo {
                        nombre: tipos::desnuda(&m.sig.ident),
                        params,
                        ret,
                        receptor,
                        defecto: m.default.clone(),
                    });
                }
                _ => {
                    return Err(CError::no_soportado(
                        "el rasgo solo puede traer métodos (sin tipos ni consts asociadas)",
                    )
                    .con_span(it, &self.lineas)
                    .con_archivo(self.archivo.clone()))
                }
            }
        }
        self.rasgos.insert(
            n.clone(),
            InfoRasgo {
                nombre: n,
                metodos,
                tiene_estaticos,
            },
        );
        Ok(())
    }

    /// Firma de método: (receptor, params-sin-receptor, ret).
    pub fn baja_firma_metodo(
        &mut self,
        sig: &syn::Signature,
        tipo_self: Option<&str>,
    ) -> Result<(Receptor, Vec<(String, CType)>, CType), CError> {
        if sig.asyncness.is_some() {
            return Err(CError::no_soportado("métodos `asinc` no caben en C")
                .con_span(&sig.fn_token, &self.lineas)
                .con_archivo(self.archivo.clone()));
        }
        if sig.variadic.is_some() {
            return Err(CError::no_soportado("métodos variádicos no soportados")
                .con_span(&sig.ident, &self.lineas)
                .con_archivo(self.archivo.clone()));
        }
        self.tipo_self = tipo_self.map(|s| s.to_string());
        let mut receptor = Receptor::Ninguno;
        let mut params = Vec::new();
        let mut n = 0;
        for arg in &sig.inputs {
            match arg {
                syn::FnArg::Receiver(r) => {
                    receptor = receptor_de(r, &self.lineas, &self.archivo)?;
                }
                syn::FnArg::Typed(t) => {
                    let tipo = self.baja_tipo(&t.ty)?;
                    let nombre = match &*t.pat {
                        syn::Pat::Ident(p) => tipos::desnuda(&p.ident),
                        syn::Pat::Wild(_) => format!("arg{n}"),
                        _ => format!("p{n}"),
                    };
                    params.push((nombre, tipo));
                    n += 1;
                }
            }
        }
        let ret = match &sig.output {
            syn::ReturnType::Default => CType::Vacio,
            syn::ReturnType::Type(_, t) => self.baja_tipo(t)?,
        };
        self.tipo_self = None;
        Ok((receptor, params, ret))
    }

    fn pase1b_impl(&mut self, im: &syn::ItemImpl) -> Result<(), CError> {
        self.rechaza_genericos(&im.generics, "implementas con genéricos")?;
        // Tipo propio: debe ser de usuario.
        let tipo = match &*im.self_ty {
            syn::Type::Path(p) if p.qself.is_none() => {
                let t = self.baja_tipo(&im.self_ty)?;
                match t {
                    CType::Usuario(n) => n,
                    _ => {
                        return Err(CError::no_soportado(
                            "`implementa` sobre tipos de std no soportado",
                        )
                        .con_span(&*im.self_ty, &self.lineas)
                        .con_archivo(self.archivo.clone()))
                    }
                }
            }
            _ => {
                return Err(CError::no_soportado(
                    "`implementa` sobre tipos compuestos no soportado",
                )
                .con_span(&*im.self_ty, &self.lineas)
                .con_archivo(self.archivo.clone()))
            }
        };

        if let Some((_, trait_path, _)) = &im.trait_ {
            let rnombre = ultimo_ident(trait_path);
            let resuelto = self.resuelve_rasgo(trait_path)?;
            match rnombre.as_str() {
                "Drop" => return self.pase1b_drop(&tipo, im),
                "Display" => {
                    self.display.insert(tipo.clone());
                }
                _ => {}
            }
            for it in &im.items {
                match it {
                    syn::ImplItem::Fn(m) => {
                        self.rechaza_genericos(&m.sig.generics, "métodos con genéricos")?;
                        let (receptor, params, ret) =
                            self.baja_firma_metodo(&m.sig, Some(&tipo))?;
                        let nombre = tipos::desnuda(&m.sig.ident);
                        let nombre_c = format!("{tipo}__{resuelto}__{nombre}");
                        self.reserva_global(&nombre_c.clone());
                        self.impl_rasgos
                            .entry((resuelto.clone(), tipo.clone()))
                            .or_default()
                            .insert(
                                nombre,
                                SigMetodo {
                                    nombre_c,
                                    params,
                                    ret,
                                    receptor,
                                },
                            );
                    }
                    syn::ImplItem::Const(c) => {
                        let t = self.baja_tipo(&c.ty)?;
                        let n = format!("{tipo}__{resuelto}__{}", tipos::desnuda(&c.ident));
                        self.reserva_global(&n.clone());
                        self.consts.insert(
                            n.clone(),
                            InfoConst {
                                nombre_c: n.clone(),
                                tipo: t,
                                define: None,
                                expr: Some(c.expr.clone()),
                            },
                        );
                        self.consts_orden.push(n);
                    }
                    _ => {
                        return Err(CError::no_soportado(
                            "en `implementa Rasgo para T` solo van métodos",
                        )
                        .con_span(it, &self.lineas)
                        .con_archivo(self.archivo.clone()))
                    }
                }
            }
            return Ok(());
        }

        // Inherente.
        for it in &im.items {
            match it {
                syn::ImplItem::Fn(m) => {
                    self.rechaza_genericos(&m.sig.generics, "métodos con genéricos")?;
                    let (receptor, params, ret) = self.baja_firma_metodo(&m.sig, Some(&tipo))?;
                    let nombre = tipos::desnuda(&m.sig.ident);
                    let base = format!("{tipo}__{nombre}");
                    let nombre_c = self.reserva_global(&base);
                    self.metodos.insert(
                        (tipo.clone(), nombre),
                        SigMetodo {
                            nombre_c,
                            params,
                            ret,
                            receptor,
                        },
                    );
                }
                syn::ImplItem::Const(c) => {
                    let t = self.baja_tipo(&c.ty)?;
                    let n = format!("{tipo}__{}", tipos::desnuda(&c.ident));
                    let n = self.reserva_global(&n);
                    let define = match (&t, &c.expr) {
                        (tt, syn::Expr::Lit(l)) if es_int_c(tt) => {
                            if let syn::Lit::Int(i) = &l.lit {
                                Some(format!("{}({})", deletrea_define(tt), i.base10_digits()))
                            } else {
                                None
                            }
                        }
                        _ => None,
                    };
                    self.consts.insert(
                        n.clone(),
                        InfoConst {
                            nombre_c: n.clone(),
                            tipo: t,
                            define,
                            expr: Some(c.expr.clone()),
                        },
                    );
                    self.consts_orden.push(n);
                }
                _ => {
                    return Err(CError::no_soportado(
                        "en `implementa T` solo van métodos y consts asociadas",
                    )
                    .con_span(it, &self.lineas)
                    .con_archivo(self.archivo.clone()))
                }
            }
        }
        Ok(())
    }

    fn pase1b_drop(&mut self, tipo: &str, im: &syn::ItemImpl) -> Result<(), CError> {
        for it in &im.items {
            match it {
                syn::ImplItem::Fn(m) => {
                    let nombre = tipos::desnuda(&m.sig.ident);
                    if nombre != "drop" {
                        return Err(CError::no_soportado("en `Drop` solo va `drop`")
                            .con_span(&m.sig.ident, &self.lineas)
                            .con_archivo(self.archivo.clone()));
                    }
                    if m.sig.inputs.len() != 1 {
                        return Err(CError::no_soportado("`drop` lleva solo `&mut yo`")
                            .con_span(&m.sig.ident, &self.lineas)
                            .con_archivo(self.archivo.clone()));
                    }
                    self.impl_drop.insert(tipo.to_string(), m.block.clone());
                }
                _ => {
                    return Err(CError::no_soportado("en `Drop` solo va `drop`")
                        .con_span(it, &self.lineas)
                        .con_archivo(self.archivo.clone()))
                }
            }
        }
        Ok(())
    }

    fn resuelve_rasgo(&self, path: &syn::Path) -> Result<String, CError> {
        let segs: Vec<String> = path.segments.iter().map(|s| tipos::desnuda(&s.ident)).collect();
        if segs.len() == 1 {
            for k in (0..=self.mods.len()).rev() {
                let mut v: Vec<String> = self.mods[..k].to_vec();
                v.push(segs[0].clone());
                let c = v.join("__");
                if self.rasgos.contains_key(&c) || self.propio_set.contains(&c) {
                    return Ok(c);
                }
            }
            return Ok(segs[0].clone());
        }
        Ok(segs.join("__"))
    }
}

// ---------------------------------------------------------------------------
// Utilidades
// ---------------------------------------------------------------------------

fn item_attrs(item: &syn::Item) -> &[syn::Attribute] {
    match item {
        syn::Item::Fn(i) => &i.attrs,
        syn::Item::Struct(i) => &i.attrs,
        syn::Item::Enum(i) => &i.attrs,
        syn::Item::Union(i) => &i.attrs,
        syn::Item::Trait(i) => &i.attrs,
        syn::Item::Type(i) => &i.attrs,
        syn::Item::Const(i) => &i.attrs,
        syn::Item::Static(i) => &i.attrs,
        syn::Item::Mod(i) => &i.attrs,
        syn::Item::Impl(i) => &i.attrs,
        syn::Item::ForeignMod(i) => &i.attrs,
        syn::Item::Use(i) => &i.attrs,
        syn::Item::ExternCrate(i) => &i.attrs,
        syn::Item::Macro(i) => &i.attrs,
        syn::Item::Verbatim(_) => &[],
        _ => &[],
    }
}

/// ¿El item sobrevive al `#[cfg]`? (evaluador mínimo, documentado)
pub fn cfg_activo(attrs: &[syn::Attribute]) -> bool {
    for a in attrs {
        if a.path().is_ident("cfg") {
            if let syn::Meta::List(list) = &a.meta {
                match list.parse_args::<CfgPred>() {
                    Ok(p) => {
                        if !p.evalua() {
                            return false;
                        }
                    }
                    Err(_) => {} // no se entiende: conserva el item
                }
            }
        }
    }
    true
}

enum CfgPred {
    Falso,
    Verdadero,
    Os(String),
    Not(Box<CfgPred>),
    All(Vec<CfgPred>),
    Any(Vec<CfgPred>),
}

impl CfgPred {
    fn evalua(&self) -> bool {
        match self {
            CfgPred::Falso => false,
            CfgPred::Verdadero => true,
            CfgPred::Os(s) => s == "linux",
            CfgPred::Not(p) => !p.evalua(),
            CfgPred::All(v) => v.iter().all(|p| p.evalua()),
            CfgPred::Any(v) => v.iter().any(|p| p.evalua()),
        }
    }
}

impl syn::parse::Parse for CfgPred {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let ident: syn::Ident = input.parse()?;
        let n = ident.to_string();
        if input.peek(syn::token::Paren) {
            let content;
            syn::parenthesized!(content in input);
            let internos = content.parse_terminated(CfgPred::parse, syn::token::Comma)?;
            let v: Vec<CfgPred> = internos.into_iter().collect();
            return Ok(match n.as_str() {
                "not" if v.len() == 1 => CfgPred::Not(Box::new(v.into_iter().next().unwrap())),
                "all" => CfgPred::All(v),
                "any" => CfgPred::Any(v),
                _ => CfgPred::Verdadero,
            });
        }
        if input.peek(syn::Token![=]) {
            let _: syn::Token![=] = input.parse()?;
            let lit: syn::LitStr = input.parse()?;
            return Ok(match n.as_str() {
                "target_os" => CfgPred::Os(lit.value()),
                _ => CfgPred::Verdadero,
            });
        }
        Ok(match n.as_str() {
            "test" | "windows" | "macos" => CfgPred::Falso,
            "unix" | "debug_assertions" => CfgPred::Verdadero,
            _ => CfgPred::Verdadero,
        })
    }
}

/// Líneas de documentación `///` como comentarios C.
pub fn docs_de(attrs: &[syn::Attribute]) -> Vec<String> {
    let mut out = Vec::new();
    for a in attrs {
        if a.path().is_ident("doc") {
            if let syn::Meta::NameValue(nv) = &a.meta {
                if let syn::Expr::Lit(l) = &nv.value {
                    if let syn::Lit::Str(s) = &l.lit {
                        out.push(format!("//{}", s.value()));
                    }
                }
            }
        }
    }
    out
}

fn ultimo_ident(path: &syn::Path) -> String {
    path.segments.last().map(|s| tipos::desnuda(&s.ident)).unwrap_or_default()
}

fn receptor_de(
    r: &syn::Receiver,
    lineas: &[String],
    archivo: &str,
) -> Result<Receptor, CError> {
    if r.reference.is_some() {
        // En syn 2 `reference` lleva `&` + lifetime; el `mut` va aparte.
        return Ok(if r.mutability.is_some() { Receptor::Mut } else { Receptor::Ref });
    }
    match &*r.ty {
        syn::Type::Path(p) if p.qself.is_none() && p.path.is_ident("Self") => Ok(Receptor::Valor),
        syn::Type::Reference(rf) => match &*rf.elem {
            syn::Type::Path(p) if p.qself.is_none() && p.path.is_ident("Self") => {
                Ok(if rf.mutability.is_some() {
                    Receptor::Mut
                } else {
                    Receptor::Ref
                })
            }
            _ => Err(receptor_malo(r, lineas, archivo)),
        },
        _ => Err(receptor_malo(r, lineas, archivo)),
    }
}

fn receptor_malo(r: &syn::Receiver, lineas: &[String], archivo: &str) -> CError {
    CError::no_soportado("receptor raro (usa `yo`, `&yo` o `&mut yo`)")
        .con_span(r, lineas)
        .con_archivo(archivo.to_string())
}

fn es_int_c(t: &CType) -> bool {
    matches!(
        t,
        CType::I8
            | CType::I16
            | CType::I32
            | CType::I64
            | CType::U8
            | CType::U16
            | CType::U32
            | CType::U64
            | CType::Isize
            | CType::Usize
    )
}

fn deletrea_define(t: &CType) -> &'static str {
    match t {
        CType::I8 => "INT8_C",
        CType::I16 => "INT16_C",
        CType::I32 => "INT32_C",
        CType::U8 => "UINT8_C",
        CType::U16 => "UINT16_C",
        CType::U32 => "UINT32_C",
        CType::U64 => "UINT64_C",
        CType::I64 | CType::Isize => "INT64_C",
        CType::Usize => "UINT64_C",
        _ => "",
    }
}

/// Recolecta items dentro de un bloque (cualquier profundidad).
fn recoge_items_bloque(
    bloque: &syn::Block,
    pref: &[String],
    out: &mut Vec<(Vec<String>, syn::Item)>,
) {
    for st in &bloque.stmts {
        if let syn::Stmt::Item(it) = st {
            out.push((pref.to_vec(), it.clone()));
        }
    }
    struct V<'a> {
        pref: &'a [String],
        out: &'a mut Vec<(Vec<String>, syn::Item)>,
    }
    impl<'ast> syn::visit::Visit<'ast> for V<'_> {
        fn visit_block(&mut self, b: &'ast syn::Block) {
            for st in &b.stmts {
                if let syn::Stmt::Item(it) = st {
                    self.out.push((self.pref.to_vec(), it.clone()));
                }
            }
            syn::visit::visit_block(self, b);
        }
        fn visit_item_fn(&mut self, _: &'ast syn::ItemFn) {
            // No entrar a fns anidadas: se procesan en cascada.
        }
    }
    for st in &bloque.stmts {
        let mut v = V { pref, out };
        use syn::visit::Visit;
        v.visit_stmt(st);
    }
}
