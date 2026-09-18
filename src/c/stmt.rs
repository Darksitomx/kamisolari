//! Pase 2: items, funciones, sentencias y control.
//!
//! Las funciones y métodos se emiten con prototipo + definición.
//! `si`/`segun` como expresión usan un temporal; `para` sobre rangos,
//! listas y arreglos se vuelve `for` C; `mientras sea` se desazucara.
//!
//! Etiquetas de ciclo: `fin{id}` (salida, para `romper 'e`) y `sig{id}`
//! (siguiente vuelta, para `continuar 'e`), solo si el ciclo tiene etiqueta.

use syn::spanned::Spanned;

use super::ayuda::firma_c;
use super::baja::{Bajador, Destino, InfoBucle, Receptor, SigMetodo, VarInfo};
use super::errores::CError;
use super::expr::{ArrayInit, ExVal};
use super::pase1::{cfg_activo, docs_de};
use super::tipos::{self, CType, LargoArreglo};

impl Bajador {
    pub fn pase2(&mut self, archivo: &syn::File) -> Result<(), CError> {
        self.completa_defaults();
        self.pase2_items(&archivo.items)?;
        let anidados = self.anidados.clone();
        for (mods, item) in &anidados {
            let guarda = std::mem::replace(&mut self.mods, mods.clone());
            self.pase2_item(item)?;
            self.mods = guarda;
        }
        Ok(())
    }

    /// Registra métodos default omitidos en los impls (`T__R__m`).
    fn completa_defaults(&mut self) {
        let claves: Vec<(String, String)> = self.impl_rasgos.keys().cloned().collect();
        for (r, t) in claves {
            let info = match self.rasgos.get(&r) {
                Some(i) => i,
                None => continue,
            };
            let mut nuevos: Vec<(String, Vec<(String, CType)>, CType, Receptor)> = Vec::new();
            for m in &info.metodos {
                let mapa = self.impl_rasgos.get(&(r.clone(), t.clone())).unwrap();
                if !mapa.contains_key(&m.nombre) && m.defecto.is_some() {
                    nuevos.push((m.nombre.clone(), m.params.clone(), m.ret.clone(), m.receptor));
                }
            }
            for (nombre, params, ret, receptor) in nuevos {
                let nombre_c = self.reserva_global(&format!("{t}__{r}__{nombre}"));
                self.impl_rasgos
                    .get_mut(&(r.clone(), t.clone()))
                    .unwrap()
                    .insert(nombre, SigMetodo { nombre_c, params, ret, receptor });
            }
        }
    }

    fn pase2_items(&mut self, items: &[syn::Item]) -> Result<(), CError> {
        for it in items {
            self.pase2_item(it)?;
        }
        Ok(())
    }

    fn pase2_item(&mut self, item: &syn::Item) -> Result<(), CError> {
        match item {
            syn::Item::Fn(f) => self.baja_fn(f),
            syn::Item::Mod(m) => {
                if let Some((_, items)) = &m.content {
                    self.mods.push(tipos::desnuda(&m.ident));
                    let r = self.pase2_items(items);
                    self.mods.pop();
                    r
                } else {
                    Ok(())
                }
            }
            syn::Item::Impl(im) => self.baja_impl(im),
            syn::Item::Const(c) => {
                let n = self.nombre_global(&tipos::desnuda(&c.ident));
                let info = self.consts.get(&n).cloned().unwrap();
                self.baja_const_info(&info, false)
            }
            syn::Item::Static(s) => {
                let n = self.nombre_global(&tipos::desnuda(&s.ident));
                let info = self.consts.get(&n).cloned().unwrap();
                self.baja_const_info_estatica(&info, matches!(s.mutability, syn::StaticMutability::Mut(_)))
            }
            syn::Item::ForeignMod(fm) => {
                for it in &fm.items {
                    match it {
                        syn::ForeignItem::Fn(g) => {
                            let n = self.nombre_global(&tipos::desnuda(&g.sig.ident));
                            let f = self.fns.get(&n).cloned().unwrap();
                            let mut p = firma_c(&n, &f.params, &f.ret)?;
                            p.pop();
                            if f.variadic {
                                if f.params.is_empty() {
                                    p.push_str("...");
                                } else {
                                    p.push_str(", ...");
                                }
                            }
                            p.push(')');
                            self.protos.push(format!("{p};"));
                        }
                        syn::ForeignItem::Static(s) => {
                            let n = self.nombre_global(&tipos::desnuda(&s.ident));
                            let info = self.consts.get(&n).cloned().unwrap();
                            let d = info.tipo.declara(&info.nombre_c)?;
                            self.protos.push(format!("extern {d};"));
                        }
                        _ => {}
                    }
                }
                Ok(())
            }
            _ => Ok(()),
        }
    }

    // ------------------------------------------------------------------
    // Funciones
    // ------------------------------------------------------------------

    fn baja_fn(&mut self, f: &syn::ItemFn) -> Result<(), CError> {
        let ident = tipos::desnuda(&f.sig.ident);
        if !cfg_activo(&f.attrs) {
            return Ok(());
        }
        let nombre = self.nombre_global(&ident);
        let firma = self.fns.get(&nombre).cloned().unwrap();
        if matches!(firma.ret, CType::Arreglo(_, _)) {
            return Err(CError::no_soportado(
                "devolver arreglos por valor no cabe en C".to_string(),
            )
            .con_span(&f.sig.output, &self.lineas)
            .con_archivo(self.archivo.clone())
            .ayuda("devuelve una `Lista<T>` o recibe el destino como `&mut [T]`"));
        }
        for (i, (_, pt)) in firma.params.iter().enumerate() {
            if matches!(pt, CType::Arreglo(_, _)) {
                return Err(CError::no_soportado(
                    "arreglos por valor como parámetro no caben (C los degrada a puntero)".to_string(),
                )
                .con_span(&f.sig.inputs[i], &self.lineas)
                .con_archivo(self.archivo.clone())
                .ayuda("pasa `&[T; N]` o `&[T]` en su lugar"));
            }
        }
        let es_main = self.mods.is_empty() && ident == "main";
        if es_main && !firma.params.is_empty() {
            return Err(CError::no_soportado("`principal` con parámetros no soportado".to_string())
                .con_span(&f.sig.inputs, &self.lineas)
                .con_archivo(self.archivo.clone())
                .ayuda("lee la entrada con `std::io::stdin().read_line(&mut linea)`"));
        }
        if es_main && !matches!(firma.ret, CType::Vacio | CType::I32) {
            return Err(CError::no_soportado("`principal` solo puede devolver `()` o `i32`".to_string())
                .con_span(&f.sig.output, &self.lineas)
                .con_archivo(self.archivo.clone()));
        }

        self.fn_nombre = nombre.clone();
        self.fn_ret = firma.ret.clone();
        self.tipo_self = None;
        self.en_display_fmt = false;
        self.entra();
        let mut i = 0;
        for arg in &f.sig.inputs {
            if let syn::FnArg::Typed(t) = arg {
                let (pname, ptipo) = firma.params[i].clone();
                self.declara_param(&t.pat, &pname, &ptipo)?;
                i += 1;
            }
        }
        self.baja_bloque_contenido(&f.block, &Destino::Retorna)?;
        let mut cuerpo = self.sale();
        // Epílogo inalcanzable (la limpieza ya la puso `sale`).
        if firma.ret.es_vacio() {
            // nada: cae al cierre
        } else if !es_main {
            let cero = firma.ret.cero_ret()?;
            cuerpo.push(format!("return {cero}; /* inalcanzable */"));
        } else {
            cuerpo.push("return 0;".into());
        }

        let mut firma_s = firma_c(&nombre, &firma.params, &firma.ret)?;
        if firma.variadic {
            firma_s.pop();
            if firma.params.is_empty() {
                firma_s.push_str("...");
            } else {
                firma_s.push_str(", ...");
            }
            firma_s.push(')');
        }
        let docs = docs_de(&f.attrs);
        if es_main {
            let mut d = vec!["int main(void) {".into()];
            d.extend(cuerpo);
            d.push("}".into());
            self.defs.extend(docs);
            self.defs.extend(d);
        } else {
            self.protos.push(format!("{firma_s};"));
            let mut d = vec![format!("{firma_s} {{")];
            d.extend(cuerpo);
            d.push("}".into());
            let fr = super::baja::firma_rust(&ident, &firma.params, &firma.ret, None);
            self.defs.push(format!("/* {fr} */"));
            self.defs.extend(docs);
            self.defs.extend(d);
        }
        Ok(())
    }

    /// Declara un parámetro (con desestructuración si el patrón es complejo).
    /// El valor vive en el parámetro C de la firma; la variable lo alia.
    fn declara_param(
        &mut self,
        pat: &syn::Pat,
        pname: &str,
        ptipo: &CType,
    ) -> Result<(), CError> {
        let nc_firma = tipos::higieniza(pname);
        match pat {
            syn::Pat::Ident(p) => {
                if p.subpat.is_some() {
                    return Err(CError::no_soportado("subpatrón `@` en parámetros".to_string())
                        .con_span(pat, &self.lineas)
                        .con_archivo(self.archivo.clone()));
                }
                let nombre = tipos::desnuda(&p.ident);
                let duena = self.necesita_drop(ptipo) && !matches!(ptipo, CType::Ref(_, _));
                self.declara_exacta(&nombre, &nc_firma, ptipo.clone(), duena);
                Ok(())
            }
            syn::Pat::Wild(_) => {
                let duena = self.necesita_drop(ptipo);
                self.declara_exacta(&format!("__ign_{nc_firma}"), &nc_firma, ptipo.clone(), duena);
                Ok(())
            }
            _ => {
                // Desestructura: el parámetro C trae el valor, el patrón lo abre.
                // Los enlaces CLONAN; el original sigue intacto y se limpia al salir.
                let duena = self.necesita_drop(ptipo);
                let clave = format!("__prm_{nc_firma}");
                self.declara_exacta(&clave, &nc_firma, ptipo.clone(), duena);
                let val = ExVal {
                    c: nc_firma.clone(),
                    tipo: ptipo.clone(),
                    movible: None,
                    lugar: Some(nc_firma),
                    arreglo: None,
                };
                self.enlaza(pat, val, None)?;
                Ok(())
            }
        }
    }

    /// Declara con nombre C exacto (para aliar parámetros de la firma).
    pub(crate) fn declara_exacta(&mut self, clave: &str, nombre_c: &str, tipo: CType, duena: bool) {
        let top = self.pila.len() - 1;
        self.pila[top].vars.insert(
            clave.to_string(),
            VarInfo { nombre_c: nombre_c.to_string(), tipo, movida: false, bandera: None, duena },
        );
        self.pila[top].orden.push(clave.to_string());
    }

    // ------------------------------------------------------------------
    // Métodos
    // ------------------------------------------------------------------

    fn baja_impl(&mut self, im: &syn::ItemImpl) -> Result<(), CError> {
        let tipo = match &*im.self_ty {
            syn::Type::Path(p) if p.qself.is_none() => self.baja_tipo(&im.self_ty)?,
            _ => {
                return Err(CError::no_soportado("implementa raro".to_string())
                    .con_span(&*im.self_ty, &self.lineas)
                    .con_archivo(self.archivo.clone()))
            }
        };
        let CType::Usuario(tipo_n) = tipo else {
            return Ok(());
        };
        if let Some((_, trait_path, _)) = &im.trait_ {
            let rnombre = trait_path
                .segments
                .last()
                .map(|s| tipos::desnuda(&s.ident))
                .unwrap_or_default();
            if rnombre == "Drop" {
                return Ok(()); // el cuerpo vive en `__liberar`
            }
            let resuelto = self.resuelve_rasgo_pub(trait_path);
            if let Some(info) = self.rasgos.get(&resuelto).cloned() {
                for it in &im.items {
                    if let syn::ImplItem::Fn(m) = it {
                        let n = tipos::desnuda(&m.sig.ident);
                        if !info.metodos.iter().any(|x| x.nombre == n) {
                            return Err(CError::nuevo(
                                "C0007",
                                format!("`{resuelto}` no tiene método `{n}`"),
                            )
                            .con_span(&m.sig.ident, &self.lineas)
                            .con_archivo(self.archivo.clone())
                            .ayuda("revisa el nombre o declara el método en el rasgo"));
                        }
                    }
                }
                // Defaults omitidos: bajar el default por impl.
                let mapa = self
                    .impl_rasgos
                    .get(&(resuelto.clone(), tipo_n.clone()))
                    .cloned()
                    .unwrap_or_default();
                for m in &info.metodos {
                    let provisto = im.items.iter().any(|it| match it {
                        syn::ImplItem::Fn(f) => tipos::desnuda(&f.sig.ident) == m.nombre,
                        _ => false,
                    });
                    if !provisto {
                        if let Some(cuerpo) = m.defecto.clone() {
                            let sig = mapa.get(&m.nombre).cloned().unwrap();
                            self.baja_metodo_default(&tipo_n, &resuelto, &sig, &m.nombre, &cuerpo)?;
                        } else {
                            return Err(CError::nuevo(
                                "C0007",
                                format!(
                                    "`implementa {resuelto} para {tipo_n}` no define `{}`",
                                    m.nombre
                                ),
                            )
                            .con_span(&*im.self_ty, &self.lineas)
                            .con_archivo(self.archivo.clone()));
                        }
                    }
                }
            }
            let es_display = resuelto == "Display" || resuelto.ends_with("__Display");
            for it in &im.items {
                match it {
                    syn::ImplItem::Fn(m) => {
                        let n = tipos::desnuda(&m.sig.ident);
                        let sig = self
                            .impl_rasgos
                            .get(&(resuelto.clone(), tipo_n.clone()))
                            .and_then(|mm| mm.get(&n))
                            .cloned();
                        let Some(sig) = sig else { continue };
                        if es_display {
                            self.baja_display_fmt(&tipo_n, &sig, m)?;
                        } else {
                            self.baja_metodo(&tipo_n, m, &sig, Some(&resuelto))?;
                        }
                    }
                    syn::ImplItem::Const(c) => {
                        let n = format!("{tipo_n}__{resuelto}__{}", tipos::desnuda(&c.ident));
                        if let Some(info) = self.consts.get(&n).cloned() {
                            self.baja_const_info(&info, false)?;
                        }
                    }
                    _ => {}
                }
            }
            return Ok(());
        }
        for it in &im.items {
            match it {
                syn::ImplItem::Fn(m) => {
                    let n = tipos::desnuda(&m.sig.ident);
                    let sig = self.metodos.get(&(tipo_n.clone(), n)).cloned();
                    let Some(sig) = sig else { continue };
                    self.baja_metodo(&tipo_n, m, &sig, None)?;
                }
                syn::ImplItem::Const(c) => {
                    let n = format!("{tipo_n}__{}", tipos::desnuda(&c.ident));
                    let clave = self
                        .consts
                        .keys()
                        .find(|k| *k == &n || k.starts_with(&format!("{n}_")))
                        .cloned();
                    if let Some(k) = clave {
                        let info = self.consts.get(&k).cloned().unwrap();
                        self.baja_const_info(&info, false)?;
                    }
                }
                _ => {}
            }
        }
        Ok(())
    }

    fn resuelve_rasgo_pub(&self, path: &syn::Path) -> String {
        let segs: Vec<String> =
            path.segments.iter().map(|s| tipos::desnuda(&s.ident)).collect();
        if segs.len() == 1 {
            for k in (0..=self.mods.len()).rev() {
                let mut v: Vec<String> = self.mods[..k].to_vec();
                v.push(segs[0].clone());
                let c = v.join("__");
                if self.rasgos.contains_key(&c) || self.propio_set.contains(&c) {
                    return c;
                }
            }
            return segs[0].clone();
        }
        segs.join("__")
    }

    /// Método inherente o de rasgo (no Display).
    fn baja_metodo(
        &mut self,
        tipo_n: &str,
        m: &syn::ImplItemFn,
        sig: &SigMetodo,
        rasgo: Option<&str>,
    ) -> Result<(), CError> {
        if !cfg_activo(&m.attrs) {
            return Ok(());
        }
        if matches!(sig.ret, CType::Arreglo(_, _)) {
            return Err(CError::no_soportado("devolver arreglos por valor no cabe en C".to_string())
                .con_span(&m.sig.output, &self.lineas)
                .con_archivo(self.archivo.clone()));
        }
        for (i, (_, pt)) in sig.params.iter().enumerate() {
            if matches!(pt, CType::Arreglo(_, _)) {
                return Err(CError::no_soportado(
                    "arreglos por valor como parámetro no caben".to_string(),
                )
                .con_span(&m.sig.inputs[i + 1.min(m.sig.inputs.len())], &self.lineas)
                .con_archivo(self.archivo.clone())
                .ayuda("pasa `&[T; N]` o `&[T]` en su lugar"));
            }
        }
        let mut params: Vec<(String, CType)> = Vec::new();
        let recv_tipo = match sig.receptor {
            Receptor::Valor => Some(CType::Usuario(tipo_n.to_string())),
            Receptor::Ref => Some(CType::Ref(false, Box::new(CType::Usuario(tipo_n.to_string())))),
            Receptor::Mut => Some(CType::Ref(true, Box::new(CType::Usuario(tipo_n.to_string())))),
            Receptor::Ninguno => None,
        };
        if let Some(rt) = &recv_tipo {
            params.push(("self".into(), rt.clone()));
        }
        params.extend(sig.params.clone());

        self.fn_nombre = sig.nombre_c.clone();
        self.fn_ret = sig.ret.clone();
        self.tipo_self = Some(tipo_n.to_string());
        self.en_display_fmt = false;
        self.entra();
        if let Some(rt) = recv_tipo {
            let duena = matches!(sig.receptor, Receptor::Valor) && self.necesita_drop(&rt);
            self.declara_exacta("self", "self", rt, duena);
        }
        let mut i = 0;
        for arg in &m.sig.inputs {
            if let syn::FnArg::Typed(t) = arg {
                let (pname, ptipo) = sig.params[i].clone();
                self.declara_param(&t.pat, &pname, &ptipo)?;
                i += 1;
            }
        }
        self.baja_bloque_contenido(&m.block, &Destino::Retorna)?;
        let mut cuerpo = self.sale();
        if !sig.ret.es_vacio() {
            let cero = sig.ret.cero_ret()?;
            cuerpo.push(format!("return {cero}; /* inalcanzable */"));
        }

        let firma = firma_c(&sig.nombre_c, &params, &sig.ret)?;
        self.protos.push(format!("{firma};"));
        let nm = tipos::desnuda(&m.sig.ident);
        let fr = super::baja::firma_rust(&nm, &sig.params, &sig.ret, Some(&sig.receptor));
        let donde = match rasgo {
            Some(r) => format!("{r}::{nm} para {tipo_n}"),
            None => format!("{tipo_n}::{nm}"),
        };
        self.defs.push(format!("/* {donde}: {fr} */"));
        let docs = docs_de(&m.attrs);
        let mut d = vec![format!("{firma} {{")];
        d.extend(cuerpo);
        d.push("}".into());
        self.defs.extend(docs);
        self.defs.extend(d);
        self.tipo_self = None;
        Ok(())
    }

    /// Método default de rasgo bajado para un impl concreto.
    fn baja_metodo_default(
        &mut self,
        tipo_n: &str,
        rasgo: &str,
        sig: &SigMetodo,
        nombre: &str,
        cuerpo: &syn::Block,
    ) -> Result<(), CError> {
        let mut params: Vec<(String, CType)> = Vec::new();
        let recv_tipo = match sig.receptor {
            Receptor::Valor => Some(CType::Usuario(tipo_n.to_string())),
            Receptor::Ref => Some(CType::Ref(false, Box::new(CType::Usuario(tipo_n.to_string())))),
            Receptor::Mut => Some(CType::Ref(true, Box::new(CType::Usuario(tipo_n.to_string())))),
            Receptor::Ninguno => None,
        };
        if let Some(rt) = &recv_tipo {
            params.push(("self".into(), rt.clone()));
        }
        let params_c: Vec<(String, CType)> = params
            .iter()
            .map(|(n, t)| (n.clone(), super::baja::sustituye_self(t, rasgo, tipo_n)))
            .collect();
        let mut sig_params_c = params_c.clone();
        if recv_tipo.is_some() {
            sig_params_c.remove(0);
        }
        let ret_c = super::baja::sustituye_self(&sig.ret, rasgo, tipo_n);

        self.fn_nombre = sig.nombre_c.clone();
        self.fn_ret = ret_c.clone();
        self.tipo_self = Some(tipo_n.to_string());
        self.en_display_fmt = false;
        self.entra();
        if let Some(rt) = recv_tipo {
            let duena = matches!(sig.receptor, Receptor::Valor) && self.necesita_drop(&rt);
            self.declara_exacta("self", "self", rt, duena);
        }
        for (n, t) in &sig_params_c {
            let duena = self.necesita_drop(t) && !matches!(t, CType::Ref(_, _));
            self.declara_exacta(n, &tipos::higieniza(n), t.clone(), duena);
        }
        self.baja_bloque_contenido(cuerpo, &Destino::Retorna)?;
        let mut lineas = self.sale();
        if !ret_c.es_vacio() {
            let cero = ret_c.cero_ret()?;
            lineas.push(format!("return {cero}; /* inalcanzable */"));
        }
        let firma = firma_c(&sig.nombre_c, &params_c, &ret_c)?;
        self.protos.push(format!("{firma};"));
        let fr = super::baja::firma_rust(nombre, &sig.params, &sig.ret, Some(&sig.receptor));
        self.defs
            .push(format!("/* {rasgo}::{nombre} para {tipo_n} (default): {fr} */"));
        let mut d = vec![format!("{firma} {{")];
        d.extend(lineas);
        d.push("}".into());
        self.defs.extend(d);
        self.tipo_self = None;
        Ok(())
    }

    /// `impl Display`: `fn fmt(&self, f: &mut Formatter) -> fmt::Result`
    /// se vuelve `KamiTexto NOMBRE(const void *crudo)`.
    ///
    /// El cuerpo escribe al búfer `__kamif` (`write!`/`writeln!` van ahí) y
    /// todo retorno devuelve el búfer. El parámetro `f` del original no
    /// existe en C: `write!(f, ...)` se detecta por nombre desconocido.
    fn baja_display_fmt(
        &mut self,
        tipo_n: &str,
        sig: &SigMetodo,
        m: &syn::ImplItemFn,
    ) -> Result<(), CError> {
        if !cfg_activo(&m.attrs) {
            return Ok(());
        }
        if m.sig.inputs.len() != 2 {
            return Err(CError::nuevo("C0007", "`fmt` lleva `(&yo, f)`".to_string())
                .con_span(&m.sig.inputs, &self.lineas)
                .con_archivo(self.archivo.clone()));
        }
        self.fn_nombre = sig.nombre_c.clone();
        self.fn_ret = CType::Texto;
        self.tipo_self = Some(tipo_n.to_string());
        self.en_display_fmt = true;
        self.entra();
        let yo = CType::Ref(false, Box::new(CType::Usuario(tipo_n.to_string())));
        self.emite(format!("const {tipo_n} *self = (const {tipo_n} *)crudo;"));
        self.declara_exacta("self", "self", yo, false);
        self.emite("KamiTexto __kamif = kami_texto_nuevo();".to_string());
        self.baja_bloque_contenido(&m.block, &Destino::Retorna)?;
        let mut lineas = self.sale();
        lineas.push("return __kamif; /* inalcanzable */".into());
        let firma = format!("KamiTexto {}(const void *crudo)", sig.nombre_c);
        self.protos.push(format!("{firma};"));
        self.defs.push(format!(
            "/* Display para {tipo_n} (fmt devuelve el texto en vez de escribirlo) */"
        ));
        let mut d = vec![format!("{firma} {{")];
        d.extend(lineas);
        d.push("}".into());
        self.defs.extend(docs_de(&m.attrs));
        self.defs.extend(d);
        self.tipo_self = None;
        self.en_display_fmt = false;
        Ok(())
    }

    // ------------------------------------------------------------------
    // Consts y estáticos
    // ------------------------------------------------------------------

    /// Constante (`const` o asociada): `#define` si hay — o se puede
    /// calcular — un literal, si no `static const`.
    fn baja_const_info(
        &mut self,
        info: &super::baja::InfoConst,
        _reservado: bool,
    ) -> Result<(), CError> {
        self.baja_const_inner(info, false, false)
    }

    /// Estático: nunca `#define` (es direccionable).
    fn baja_const_info_estatica(
        &mut self,
        info: &super::baja::InfoConst,
        mutable: bool,
    ) -> Result<(), CError> {
        self.baja_const_inner(info, true, mutable)
    }

    fn baja_const_inner(
        &mut self,
        info: &super::baja::InfoConst,
        estatica: bool,
        mutable: bool,
    ) -> Result<(), CError> {
        // Localiza la clave (nombre_c puede venir con sufijo).
        let clave = self
            .consts
            .iter()
            .find(|(_, v)| v.nombre_c == info.nombre_c)
            .map(|(k, _)| k.to_string());
        if estatica {
            // Los estáticos son direccionables: jamás `#define`.
            if let Some(k) = &clave {
                if let Some(i) = self.consts.get_mut(k) {
                    i.define = None;
                }
            }
        }
        if info.define.is_some() {
            return Ok(()); // lo emite `ensambla` al inicio
        }
        let Some(expr) = info.expr.clone() else {
            return Ok(()); // `extern`: declarado en el ForeignMod
        };
        // Intento de `#define` para consts evaluables a literal.
        // (Los `#[define]` del pase 1 solo cubren literales directos;
        // aquí entran `N + 1`, `-3`, rutas a otras consts, etc.)
        if !estatica {
            if info.tipo.es_entero() {
                let (c, _) = self.baja_expr_const(&expr)?;
                if es_literal_seguro(&c) {
                    let mac = deletrea_macro(&info.tipo);
                    let def = if mac.is_empty() {
                        format!("({c})")
                    } else {
                        format!("{mac}({c})")
                    };
                    if let Some(k) = &clave {
                        self.consts.get_mut(k).unwrap().define = Some(def);
                    }
                    return Ok(());
                }
            } else if matches!(info.tipo, CType::Bool) {
                let (c, _) = self.baja_expr_const(&expr)?;
                if c == "true" || c == "false" {
                    if let Some(k) = &clave {
                        self.consts.get_mut(k).unwrap().define = Some(format!("(bool)({c})"));
                    }
                    return Ok(());
                }
            } else if matches!(info.tipo, CType::Char) {
                let (c, _) = self.baja_expr_const(&expr)?;
                if es_literal_seguro(&c) {
                    if let Some(k) = &clave {
                        self.consts
                            .get_mut(k)
                            .unwrap()
                            .define = Some(format!("(KamiChar)({c})"));
                    }
                    return Ok(());
                }
            }
        }
        let (c, _) = self.baja_expr_const(&expr)?;
        let decl = info.tipo.declara(&info.nombre_c)?;
        if mutable {
            self.consts_out.push(format!("static {decl} = ({c});"));
        } else {
            self.consts_out.push(format!("static const {decl} = ({c});"));
        }
        Ok(())
    }

    // ------------------------------------------------------------------
    // Bloques y sentencias
    // ------------------------------------------------------------------

    /// Contenido de un bloque (sentencias + cola) hacia un destino.
    /// El llamador abre/cierra el scope (`entra`/`sale`).
    pub(crate) fn baja_bloque_contenido(
        &mut self,
        b: &syn::Block,
        dest: &Destino,
    ) -> Result<(), CError> {
        let n = b.stmts.len();
        for (i, s) in b.stmts.iter().enumerate() {
            let ultimo = i + 1 == n;
            match s {
                syn::Stmt::Expr(e, None) if ultimo => self.baja_cola(e, dest)?,
                syn::Stmt::Macro(m) if ultimo && m.semi_token.is_none() => {
                    self.baja_cola_macro(&m.mac, dest)?
                }
                _ => self.baja_stmt(s)?,
            }
        }
        Ok(())
    }

    /// Expresión-cola hacia un destino.
    pub(crate) fn baja_cola(&mut self, e: &syn::Expr, dest: &Destino) -> Result<(), CError> {
        match dest {
            Destino::Descarta => {
                let v = self.baja_expr(e, None)?;
                if matches!(v.tipo, CType::Arreglo(_, _) if v.arreglo.is_none()) {
                    // Arreglo descartado: sus temporales se limpian solos.
                }
                let c = self.consume(v, None)?;
                self.emite(format!("(void)({c});"));
                Ok(())
            }
            Destino::Temp(t) => {
                let tipo = self.tipo_de_temp(t);
                let v = self.baja_expr(e, Some(&tipo))?;
                if let CType::Arreglo(elem, largo) = tipo.clone() {
                    // `if c { [1,2] } else { [3,4] }`: materializa directo.
                    return self.materializa_arreglo(t, v, &elem, &largo, false);
                }
                let c = self.consume(v, Some(&tipo))?;
                self.fija(t, c, &tipo)?;
                Ok(())
            }
            Destino::Retorna => {
                if self.en_display_fmt {
                    return self.baja_cola_fmt(e);
                }
                let ret = self.fn_ret.clone();
                if ret.es_vacio() {
                    let v = self.baja_expr(e, None)?;
                    let c = self.consume(v, None)?;
                    self.emite(format!("(void)({c});"));
                    Ok(())
                } else {
                    let v = self.baja_expr(e, Some(&ret))?;
                    let c = self.consume(v, Some(&ret))?;
                    let limp = self.limpieza_hasta(0);
                    self.emite_todas(limp);
                    self.emite(format!("return ({c});"));
                    Ok(())
                }
            }
        }
    }

    /// Cola dentro de `fmt`: se evalúa por efectos y se retorna el búfer.
    /// `Ok(x)` no construye nada (el `Ok` es nominal aquí).
    fn baja_cola_fmt(&mut self, e: &syn::Expr) -> Result<(), CError> {
        if let Some(inner) = es_ok(e) {
            let v = self.baja_expr(inner, None)?;
            let c = self.consume(v, None)?;
            self.emite(format!("(void)({c});"));
        } else if es_err(e).is_some() {
            return Err(CError::nuevo("C0003", "`fmt` no puede fallar aquí".to_string())
                .con_span(e, &self.lineas)
                .con_archivo(self.archivo.clone())
                .ayuda("nuestro `write!` nunca falla: quita el `Err`"));
        } else {
            let v = self.baja_expr(e, None)?;
            let c = self.consume(v, None)?;
            self.emite(format!("(void)({c});"));
        }
        let limp = self.limpieza_hasta(0);
        self.emite_todas(limp);
        self.emite("return __kamif;");
        Ok(())
    }

    /// Separa `patrón [: Tipo]` de un `sea`.
    fn desanota<'a>(&mut self, pat: &'a syn::Pat) -> Result<(&'a syn::Pat, Option<CType>), CError> {
        match pat {
            syn::Pat::Type(pt) => {
                let t = self.baja_tipo(&pt.ty)?;
                if matches!(t, CType::Infer) {
                    Ok((&pt.pat, None))
                } else {
                    Ok((&pt.pat, Some(t)))
                }
            }
            _ => Ok((pat, None)),
        }
    }

    fn baja_stmt(&mut self, s: &syn::Stmt) -> Result<(), CError> {
        match s {
            syn::Stmt::Local(l) => self.baja_local(l),
            syn::Stmt::Item(_) => Ok(()), // anidados: se emiten a nivel superior
            syn::Stmt::Expr(e, _) => self.baja_expr_stmt(e),
            syn::Stmt::Macro(m) => {
                if m.semi_token.is_some() {
                    self.baja_macro_stmt(&m.mac)
                } else {
                    self.baja_macro_expr_stmt(&m.mac)
                }
            }
        }
    }

    fn baja_expr_stmt(&mut self, e: &syn::Expr) -> Result<(), CError> {
        match e {
            syn::Expr::Return(x) => self.baja_return(x),
            syn::Expr::Break(x) => self.baja_break(x),
            syn::Expr::Continue(x) => self.baja_continue(x),
            syn::Expr::Macro(m) => self.baja_macro_expr_stmt(&m.mac),
            _ => {
                let v = self.baja_expr(e, None)?;
                let c = self.consume(v, None)?;
                self.emite(format!("(void)({c});"));
                Ok(())
            }
        }
    }

    fn baja_local(&mut self, l: &syn::Local) -> Result<(), CError> {
        let Some(init) = &l.init else {
            return Err(CError::nuevo("C0003", "`sea x;` sin inicializar no cabe".to_string())
                .con_span(&l.pat, &self.lineas)
                .con_archivo(self.archivo.clone())
                .ayuda("inicializa al declarar: `sea x: Tipo = valor;`"));
        };
        if init.diverge.is_some() {
            let sino = init.diverge.as_ref().map(|(_, b)| &**b).unwrap();
            return self.baja_let_else(l, &init.expr, sino);
        }
        let (pat_real, anotado) = self.desanota(&l.pat)?;
        // Vía rápida: `sea nombre = [..]` declara el arreglo con su nombre.
        if matches!(&*init.expr, syn::Expr::Array(_)) {
            if let syn::Pat::Ident(p) = pat_real {
                if p.subpat.is_none() && p.by_ref.is_none() {
                    return self.sea_arreglo(&tipos::desnuda(&p.ident), &init.expr, anotado.as_ref());
                }
            }
        }
        let v = self.baja_expr(&init.expr, anotado.as_ref())?;
        // `sea x = return;` (divergente): maniquí para que compile.
        if matches!(v.tipo, CType::Infer) && anotado.is_none() {
            if let syn::Pat::Ident(p) = pat_real {
                if p.subpat.is_none() {
                    let nc = self.declara(&tipos::desnuda(&p.ident), CType::I32, false);
                    let decl = CType::I32.declara(&nc)?;
                    self.emite(format!("{decl} = 0; /* divergente */"));
                    return Ok(());
                }
            }
            if matches!(pat_real, syn::Pat::Wild(_)) {
                return Ok(());
            }
        }
        self.enlaza(pat_real, v, anotado.as_ref())
    }

    /// `sea PAT = expr else { diverge };` (`else` también acepta
    /// `return`, `continue`, `panic!(...)` pelados).
    fn baja_let_else(
        &mut self,
        l: &syn::Local,
        plaza: &syn::Expr,
        sino: &syn::Expr,
    ) -> Result<(), CError> {
        let (pat_real, anotado) = self.desanota(&l.pat)?;
        let v = self.baja_expr(plaza, anotado.as_ref())?;
        let t_scrut = anotado.clone().unwrap_or_else(|| v.tipo.clone());
        let tmp = self.temp(t_scrut.clone())?;
        if let CType::Arreglo(elem, largo) = t_scrut.clone() {
            self.materializa_arreglo(&tmp, v, &elem, &largo, false)?;
        } else {
            let c = self.consume(v, Some(&t_scrut))?;
            self.fija(&tmp, c, &t_scrut)?;
        }
        let (cond, enlaces) = self.analiza_patron(pat_real, &t_scrut, &tmp)?;
        if cond != "true" {
            self.entra();
            match sino {
                syn::Expr::Block(b) => self.baja_bloque_contenido(&b.block, &Destino::Descarta)?,
                otro => {
                    let v = self.baja_expr(otro, None)?;
                    let c = self.consume(v, None)?;
                    self.emite(format!("(void)({c});"));
                }
            }
            let rama = self.sale();
            self.emite(format!("if (!({cond})) {{"));
            for l in rama {
                self.emite(format!("    {l}"));
            }
            self.emite("}");
        }
        self.emite_enlaces(&enlaces)?;
        Ok(())
    }

    /// `sea nombre [(: [T; N])] = [..]`.
    fn sea_arreglo(
        &mut self,
        nombre: &str,
        expr: &syn::Expr,
        anotado: Option<&CType>,
    ) -> Result<(), CError> {
        let v = self.baja_expr(expr, anotado)?;
        let CType::Arreglo(elem, largo) = v.tipo.clone() else {
            return Err(CError::nuevo("C0002", "se esperaba arreglo (bug interno)".to_string())
                .con_span(expr, &self.lineas)
                .con_archivo(self.archivo.clone()));
        };
        if matches!(&*elem, CType::Vacio | CType::Infer | CType::Dyn(_)) {
            return Err(CError::nuevo("C0003", "elemento de arreglo inválido".to_string())
                .con_span(expr, &self.lineas)
                .con_archivo(self.archivo.clone()));
        }
        let tipo_arr = CType::Arreglo(elem.clone(), largo.clone());
        self.registra_uso_pub(&tipo_arr)?;
        let duena = self.necesita_drop(&tipo_arr);
        let nc = self.declara(nombre, tipo_arr.clone(), duena);
        let decl = tipo_arr.declara(&nc)?;
        if duena {
            self.emite(format!("{decl} = {{0}};"));
        } else {
            self.emite(format!("{decl};"));
        }
        self.materializa_arreglo(&nc, v, &elem, &largo, false)
    }

    /// Materializa un arreglo (literal diferido o valor) en `dst` ya
    /// declarado. Con `libera_dst` libera el contenido viejo primero.
    pub(crate) fn materializa_arreglo(
        &mut self,
        dst: &str,
        val: ExVal,
        elem: &CType,
        largo: &LargoArreglo,
        libera_dst: bool,
    ) -> Result<(), CError> {
        let tipo_arr = CType::Arreglo(Box::new(elem.clone()), largo.clone());
        if libera_dst && self.necesita_drop(elem) {
            for s in self.libera_lugar(dst, &tipo_arr) {
                self.emite(s);
            }
        }
        match val.arreglo {
            Some(ArrayInit::Lista(elems)) => {
                if let LargoArreglo::Lit(k) = largo {
                    if elems.len() != *k {
                        return Err(CError::nuevo(
                            "C0003",
                            format!("el arreglo trae {} elementos, se esperaban {k}", elems.len()),
                        )
                        .con_archivo(self.archivo.clone()));
                    }
                }
                for (i, e) in elems.iter().enumerate() {
                    let d = format!("({dst})[{i}]");
                    if matches!(elem, CType::Arreglo(_, _)) {
                        // Elemento arreglo (temp o lugar): memcpy.
                        self.emite(format!("memcpy({d}, ({}), sizeof({d}));", e.c));
                        self.marca_movida_si_temp_pub(&e.c);
                    } else {
                        self.emite(format!("{d} = ({});", e.c));
                        self.marca_movida_si_temp_pub(&e.c);
                    }
                }
                Ok(())
            }
            Some(ArrayInit::Repite { c, tipo: _, n: _ }) => {
                // La fuente se evalúa UNA vez (como Rust) en un temporal;
                // cada vuelta copia (Copy) o clona (dueños).
                let len = largo.deletrea();
                let tsrc = self.temp(elem.clone())?;
                if matches!(elem, CType::Arreglo(_, _)) {
                    self.emite(format!("memcpy({tsrc}, ({c}), sizeof({tsrc}));"));
                } else {
                    self.emite(format!("{tsrc} = ({c});"));
                }
                self.marca_movida_si_temp_pub(&c);
                let idx = format!("__i{}", self.etiqueta_n);
                self.etiqueta_n += 1;
                self.emite(format!("for (size_t {idx} = 0; {idx} < (size_t)({len}); ++{idx}) {{"));
                self.clona_elem_en(&format!("({dst})[{idx}]"), &tsrc, elem)?;
                self.emite("}");
                Ok(())
            }
            None => {
                if !matches!(val.tipo, CType::Arreglo(_, _)) {
                    return Err(CError::nuevo("C0002", "se esperaba arreglo (bug interno)".to_string())
                        .con_archivo(self.archivo.clone()));
                }
                // Movimiento bit a bit + marca (vale para Copy y dueños).
                self.emite(format!("memcpy(({dst}), ({}), sizeof({dst}));", val.c));
                if let Some(m) = val.movible {
                    self.marca_movida(&m);
                    self.activa_bandera(&m);
                }
                self.marca_movida_si_temp_pub(&val.c);
                Ok(())
            }
        }
    }

    /// `dst = copia-o-clon(src)` para UN elemento (repite).
    /// `src` es un lugar direccionable (temporal).
    fn clona_elem_en(&mut self, dst: &str, src: &str, elem: &CType) -> Result<(), CError> {
        if !self.necesita_drop(elem) {
            if matches!(elem, CType::Arreglo(_, _)) {
                self.emite(format!("memcpy(({dst}), ({src}), sizeof({dst}));"));
            } else {
                self.emite(format!("{dst} = ({src});"));
            }
            return Ok(());
        }
        if let Some(f) = self.nombre_clona_pub(elem) {
            self.emite(format!("{dst} = {f}({src});"));
            return Ok(());
        }
        if let CType::Caja(inner) = elem {
            if matches!(&**inner, CType::Arreglo(_, _) | CType::FnPtr { .. }) {
                return Err(CError::nuevo(
                    "C0003",
                    "caja con arreglo o función en `[x; n]` no cabe".to_string(),
                )
                .con_archivo(self.archivo.clone())
                .ayuda("arma el arreglo con un `para` o usa `Lista`"));
            }
            let spell = inner.deletrea()?;
            let nb = format!("__nb{}", self.etiqueta_n);
            self.etiqueta_n += 1;
            self.emite(format!("{spell} *{nb} = ({spell}*)malloc(sizeof({spell}));"));
            self.emite(format!("if (!{nb}) {{ KAMI_SIN_MEMORIA(); }}"));
            if self.necesita_drop(inner) {
                if let Some(f) = self.nombre_clona_pub(inner) {
                    self.emite(format!("*{nb} = {f}({src});"));
                } else {
                    return Err(CError::nuevo("C0003", "caja anidada en `[x; n]` no cabe".to_string())
                        .con_archivo(self.archivo.clone()));
                }
            } else {
                self.emite(format!("*{nb} = (*({src}));"));
            }
            self.emite(format!("{dst} = {nb};"));
            return Ok(());
        }
        Err(CError::nuevo(
            "C0003",
            "arreglos anidados con dueños en `[x; n]` no caben".to_string(),
        )
        .con_archivo(self.archivo.clone())
        .ayuda("arma el arreglo con un `para` o usa `Lista`"))
    }

    /// Marca movido si `c` nombra un temporal dueño (`__t3`).
    /// Todo lo demás (llamadas, lugares) se ignora sin ruido.
    pub(crate) fn marca_movida_si_temp_pub(&mut self, c: &str) {
        let es_temp = self
            .pila
            .iter()
            .rev()
            .find_map(|a| a.vars.get(c))
            .map(|v| v.duena)
            .unwrap_or(false);
        if es_temp {
            let n = c.to_string();
            self.marca_movida(&n);
            self.activa_bandera(&n);
        }
    }

    /// Tipo de un temporal por su nombre C.
    fn tipo_de_temp(&self, t: &str) -> CType {
        self.pila
            .iter()
            .rev()
            .find_map(|a| a.vars.values().find(|v| v.nombre_c == t).map(|v| v.tipo.clone()))
            .expect("temp sin declarar (bug)")
    }

    /// Asigna un valor consumido a un temporal ya declarado.
    fn fija(&mut self, t: &str, c: String, tipo: &CType) -> Result<(), CError> {
        if matches!(tipo, CType::Arreglo(_, _)) {
            return Err(CError::nuevo("C0002", "arreglo directo en temporal (bug interno)".to_string())
                .con_archivo(self.archivo.clone()));
        }
        self.emite(format!("{t} = ({c});"));
        self.marca_movida_si_temp_pub(&c);
        Ok(())
    }

    /// `return [expr];`
    pub(crate) fn baja_return(&mut self, x: &syn::ExprReturn) -> Result<(), CError> {
        if self.en_display_fmt {
            if let Some(e) = &x.expr {
                if es_err(e).is_some() {
                    return Err(CError::nuevo("C0003", "`fmt` no puede fallar aquí".to_string())
                        .con_span(x, &self.lineas)
                        .con_archivo(self.archivo.clone())
                        .ayuda("nuestro `write!` nunca falla: quita el `Err`"));
                }
                if let Some(inner) = es_ok(e) {
                    let v = self.baja_expr(inner, None)?;
                    let c = self.consume(v, None)?;
                    self.emite(format!("(void)({c});"));
                } else {
                    let v = self.baja_expr(e, None)?;
                    let c = self.consume(v, None)?;
                    self.emite(format!("(void)({c});"));
                }
            }
            let limp = self.limpieza_hasta(0);
            self.emite_todas(limp);
            self.emite("return __kamif;");
            return Ok(());
        }
        let ret = self.fn_ret.clone();
        match &x.expr {
            None => {
                let limp = self.limpieza_hasta(0);
                self.emite_todas(limp);
                self.emite("return;");
                Ok(())
            }
            Some(e) => {
                if ret.es_vacio() {
                    let v = self.baja_expr(e, None)?;
                    let c = self.consume(v, None)?;
                    self.emite(format!("(void)({c});"));
                    let limp = self.limpieza_hasta(0);
                    self.emite_todas(limp);
                    self.emite("return;");
                    Ok(())
                } else {
                    let v = self.baja_expr(e, Some(&ret))?;
                    let c = self.consume(v, Some(&ret))?;
                    let limp = self.limpieza_hasta(0);
                    self.emite_todas(limp);
                    self.emite(format!("return ({c});"));
                    Ok(())
                }
            }
        }
    }

    /// `break ['e] [valor];`
    pub(crate) fn baja_break(&mut self, x: &syn::ExprBreak) -> Result<(), CError> {
        let (prof, id, valor) = self.objetivo_ruptura(x.label.as_ref())?;
        if let Some(e) = &x.expr {
            let Some((t, tipo)) = valor else {
                return Err(CError::nuevo(
                    "C0003",
                    "`romper` con valor solo vale en `ciclo` usado como valor".to_string(),
                )
                .con_span(x, &self.lineas)
                .con_archivo(self.archivo.clone()));
            };
            if matches!(tipo, CType::Arreglo(_, _)) {
                let v = self.baja_expr(e, Some(&tipo))?;
                let CType::Arreglo(elem, largo) = tipo else { unreachable!() };
                self.materializa_arreglo(&t, v, &elem, &largo, false)?;
            } else {
                let v = self.baja_expr(e, Some(&tipo))?;
                let c = self.consume(v, Some(&tipo))?;
                self.fija(&t, c, &tipo)?;
            }
        } else if let Some((_, tipo)) = &valor {
            if !tipo.es_vacio() {
                return Err(CError::nuevo(
                    "C0003",
                    "`romper` sin valor en un ciclo que da valor".to_string(),
                )
                .con_span(x, &self.lineas)
                .con_archivo(self.archivo.clone()));
            }
        }
        let limp = self.limpieza_hasta(prof);
        self.emite_todas(limp);
        self.emite(format!("goto fin{id};"));
        Ok(())
    }

    /// `continue ['e];`
    pub(crate) fn baja_continue(&mut self, x: &syn::ExprContinue) -> Result<(), CError> {
        let (prof, id, _) = self.objetivo_ruptura(x.label.as_ref())?;
        let limp = self.limpieza_hasta(prof);
        self.emite_todas(limp);
        self.emite(format!("goto sig{id};"));
        Ok(())
    }

    /// Ciclo objetivo de `romper`/`continuar`: (prof_cuerpo, id, valor).
    fn objetivo_ruptura(
        &self,
        etiqueta: Option<&syn::Lifetime>,
    ) -> Result<(usize, usize, Option<(String, CType)>), CError> {
        if let Some(lt) = etiqueta {
            let nombre = tipos::desnuda(&lt.ident);
            for b in self.bucles.iter().rev() {
                if b.etiqueta.as_deref() == Some(nombre.as_str()) {
                    return Ok((b.prof_cuerpo, b.id, b.valor.clone()));
                }
            }
            return Err(CError::nuevo("C0003", format!("etiqueta desconocida '{nombre}"))
                .con_span(lt, &self.lineas)
                .con_archivo(self.archivo.clone()));
        }
        match self.bucles.last() {
            Some(b) => Ok((b.prof_cuerpo, b.id, b.valor.clone())),
            None => Err(CError::nuevo("C0003", "romper/continuar fuera de ciclo".to_string())
                .con_archivo(self.archivo.clone())),
        }
    }
}

/// `INT32_C` y familia para los `#define` de consts enteras.
fn deletrea_macro(t: &CType) -> &'static str {
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

/// ¿El C evaluado es seguro como cuerpo de `#define`?
/// (Dígitos, operadores, paréntesis, comillas, sufijos; sin `;` ni llaves.)
fn es_literal_seguro(c: &str) -> bool {
    !c.is_empty()
        && c.chars().all(|ch| {
            ch.is_ascii_alphanumeric() || " \t_+-*/%()&|~^<>=!.,'\"".contains(ch)
        })
}

/// `Ok(x)` como llamada (con o sin turbofish).
fn es_ok(e: &syn::Expr) -> Option<&syn::Expr> {
    if let syn::Expr::Call(c) = e {
        if let syn::Expr::Path(p) = &*c.func {
            if p.path.segments.last().map(|s| tipos::desnuda(&s.ident)).as_deref() == Some("Ok") {
                if c.args.len() == 1 {
                    return Some(&c.args[0]);
                }
            }
        }
    }
    None
}

/// `Err(x)` como llamada (con o sin turbofish).
fn es_err(e: &syn::Expr) -> Option<&syn::Expr> {
    if let syn::Expr::Call(c) = e {
        if let syn::Expr::Path(p) = &*c.func {
            if p.path.segments.last().map(|s| tipos::desnuda(&s.ident)).as_deref() == Some("Err") {
                if c.args.len() == 1 {
                    return Some(&c.args[0]);
                }
            }
        }
    }
    None
}

impl Bajador {
    /// Macro como sentencia con `;` (`println!`, `assert!`...).
    pub(crate) fn baja_macro_stmt(&mut self, mac: &syn::Macro) -> Result<(), CError> {
        let nombre = mac
            .path
            .segments
            .last()
            .map(|s| s.ident.to_string())
            .unwrap_or_default();
        match nombre.as_str() {
            "println" | "print" | "eprintln" | "eprint" => self.ex_macro_print(&nombre, mac),
            "assert" | "assert_eq" | "assert_ne" | "debug_assert" | "debug_assert_eq"
            | "debug_assert_ne" => self.ex_macro_assert(&nombre, mac),
            _ => self.baja_macro_expr_stmt(mac),
        }
    }

    /// Macro como sentencia-expresión (sin `;`, no final): se baja y descarta.
    pub(crate) fn baja_macro_expr_stmt(&mut self, mac: &syn::Macro) -> Result<(), CError> {
        let nombre = mac
            .path
            .segments
            .last()
            .map(|s| s.ident.to_string())
            .unwrap_or_default();
        match nombre.as_str() {
            "println" | "print" | "eprintln" | "eprint" => self.ex_macro_print(&nombre, mac),
            "assert" | "assert_eq" | "assert_ne" | "debug_assert" | "debug_assert_eq"
            | "debug_assert_ne" => self.ex_macro_assert(&nombre, mac),
            _ => {
                let e = syn::Expr::Macro(syn::ExprMacro {
                    attrs: Vec::new(),
                    mac: mac.clone(),
                });
                let v = self.baja_expr(&e, None)?;
                let c = self.consume(v, None)?;
                self.emite(format!("(void)({c});"));
                Ok(())
            }
        }
    }

    /// Macro en cola: se baja como expresión hacia el destino.
    pub(crate) fn baja_cola_macro(&mut self, mac: &syn::Macro, dest: &Destino) -> Result<(), CError> {
        let nombre = mac
            .path
            .segments
            .last()
            .map(|s| s.ident.to_string())
            .unwrap_or_default();
        match nombre.as_str() {
            "println" | "print" | "eprintln" | "eprint" | "assert" | "assert_eq" | "assert_ne"
            | "debug_assert" | "debug_assert_eq" | "debug_assert_ne" => {
                // Valen `()`: se emiten y luego unidad al destino.
                if matches!(nombre.as_str(), "println" | "print" | "eprintln" | "eprint") {
                    self.ex_macro_print(&nombre, mac)?;
                } else {
                    self.ex_macro_assert(&nombre, mac)?;
                }
                let u: syn::Expr = syn::parse_str("()").map_err(|e: syn::Error| {
                    CError::nuevo("C0002", format!("no pude fabricar `()`: {e}"))
                        .con_archivo(self.archivo.clone())
                })?;
                self.baja_cola(&u, dest)
            }
            _ => {
                let e = syn::Expr::Macro(syn::ExprMacro {
                    attrs: Vec::new(),
                    mac: mac.clone(),
                });
                self.baja_cola(&e, dest)
            }
        }
    }

    /// `println!` / `print!` / `eprintln!` / `eprint!`.
    pub(crate) fn ex_macro_print(&mut self, nombre: &str, mac: &syn::Macro) -> Result<(), CError> {
        let args: syn::punctuated::Punctuated<syn::Expr, syn::token::Comma> = mac
            .parse_body_with(syn::punctuated::Punctuated::<syn::Expr, syn::token::Comma>::parse_terminated)
            .map_err(|e| self.err_en("C0005", format!("`{nombre}!` mal formado: {e}"), mac))?;
        let (fmt_src, resto) = if args.is_empty() {
            (String::new(), Vec::new())
        } else {
            let f = match &args[0] {
                syn::Expr::Lit(l) => match &l.lit {
                    syn::Lit::Str(s) => s.value(),
                    _ => return Err(self.err_en("C0005", format!("`{nombre}!` pide literal de cadena"), &args[0])),
                },
                _ => return Err(self.err_en("C0005", format!("`{nombre}!` pide literal de cadena"), &args[0])),
            };
            (f, args.iter().skip(1).cloned().collect())
        };
        let (f, cs) = self.fmt_traduce(&fmt_src, &resto, mac)?;
        let extra = if cs.is_empty() { String::new() } else { format!(", {}", cs.join(", ")) };
        let nl = if matches!(nombre, "println" | "eprintln") { "\"\\n\"" } else { "" };
        if matches!(nombre, "println" | "print") {
            self.emite(format!("printf({f}{nl}{extra});"));
        } else {
            self.emite(format!("fprintf(stderr, {f}{nl}{extra});"));
        }
        Ok(())
    }

    /// `assert!` / `assert_eq!` / `assert_ne!` (+ `debug_*`, que sí se revisan).
    pub(crate) fn ex_macro_assert(&mut self, nombre: &str, mac: &syn::Macro) -> Result<(), CError> {
        let args: syn::punctuated::Punctuated<syn::Expr, syn::token::Comma> = mac
            .parse_body_with(syn::punctuated::Punctuated::<syn::Expr, syn::token::Comma>::parse_terminated)
            .map_err(|e| self.err_en("C0005", format!("`{nombre}!` mal formado: {e}"), mac))?;
        if args.is_empty() {
            return Err(self.err_en("C0005", format!("`{nombre}!` pide condición"), mac));
        }
        let base = nombre.strip_prefix("debug_").unwrap_or(nombre);
        if base == "assert" {
            let v = self.baja_expr(&args[0], Some(&CType::Bool))?;
            let c = self.consume(v, Some(&CType::Bool))?;
            if args.len() == 1 {
                self.emite(format!("if (!(({c}))) {{ KAMI_PANICO(\"assert falló\"); }}"));
                return Ok(());
            }
            let f = match &args[1] {
                syn::Expr::Lit(l) => match &l.lit {
                    syn::Lit::Str(s) => s.value(),
                    _ => return Err(self.err_en("C0005", format!("`{nombre}!` pide literal de formato"), &args[1])),
                },
                _ => return Err(self.err_en("C0005", format!("`{nombre}!` pide literal de formato"), &args[1])),
            };
            let resto: Vec<syn::Expr> = args.iter().skip(2).cloned().collect();
            let (ff, cs) = self.fmt_traduce(&f, &resto, mac)?;
            let extra = if cs.is_empty() { String::new() } else { format!(", {}", cs.join(", ")) };
            self.emite(format!("if (!(({c}))) {{ KAMI_PANICO({ff}{extra}); }}"));
            return Ok(());
        }
        // `assert_eq!` / `assert_ne!`: dos valores (+ formato opcional).
        if args.len() < 2 {
            return Err(self.err_en("C0005", format!("`{nombre}!` pide dos valores"), mac));
        }
        let va = self.baja_expr(&args[0], None)?;
        let vb = self.baja_expr(&args[1], None)?;
        if va.tipo != vb.tipo {
            return Err(self.err_en(
                "C0005",
                format!(
                    "`{nombre}!` pide el mismo tipo, no `{}` y `{}`",
                    self.muestra(&va.tipo),
                    self.muestra(&vb.tipo)
                ),
                mac,
            ));
        }
        if va.arreglo.is_some() || vb.arreglo.is_some() {
            return Err(self.err_en("C0003", "arreglo en `assert` sin soporte; guárdalo en variable".into(), mac));
        }
        let t = va.tipo.clone();
        // Mensaje del usuario (si hay).
        let usr: Option<(String, Vec<String>)> = if args.len() > 2 {
            let f = match &args[2] {
                syn::Expr::Lit(l) => match &l.lit {
                    syn::Lit::Str(s) => s.value(),
                    _ => return Err(self.err_en("C0005", format!("`{nombre}!` pide literal de formato"), &args[2])),
                },
                _ => return Err(self.err_en("C0005", format!("`{nombre}!` pide literal de formato"), &args[2])),
            };
            let resto: Vec<syn::Expr> = args.iter().skip(3).cloned().collect();
            Some(self.fmt_traduce(&f, &resto, mac)?)
        } else {
            None
        };
        let es_eq = base == "assert_eq";
        if t.es_flotante() {
            // Flotantes con `==` (NaN != NaN, como Rust).
            let ca = self.consume(va, Some(&t))?;
            let cb = self.consume(vb, Some(&t))?;
            let ta = self.temp(t.clone())?;
            self.emite(format!("{ta} = ({ca});"));
            let tb2 = self.temp(t.clone())?;
            self.emite(format!("{tb2} = ({cb});"));
            let op = if es_eq { "==" } else { "!=" };
            let cmp = if es_eq { "!=" } else { "==" };
            let vals = format!("\"%g {op} %g\", ({ta}), ({tb2})");
            let (ff, extra) = match &usr {
                Some((f, cs)) => {
                    let mut all = cs.clone();
                    all.push(format!("({ta})"));
                    all.push(format!("({tb2})"));
                    (format!("{f} \": %s {op} %s\""), format!(", {}", all.join(", ")))
                }
                None => (format!("\"assert falló: {vals}\""), String::new()),
            };
            // Ojo: en la rama con formato los valores van con %s de depurado abajo.
            let _ = (ff, extra);
            if usr.is_none() {
                self.emite(format!("if ((({ta}) {cmp} ({tb2}))) {{ KAMI_PANICO(\"assert falló: {vals}\"); }}"));
            } else {
                let (f, cs) = usr.unwrap();
                let mut all = cs.clone();
                all.push(format!("({ta})"));
                all.push(format!("({tb2})"));
                self.emite(format!("if ((({ta}) {cmp} ({tb2}))) {{ KAMI_PANICO({f} \", %g {op} %g\", {}); }}", all.join(", ")));
            }
            return Ok(());
        }
        // Resto: Debug de ambos + `strcmp`.
        let amarra = |s: &mut Self, v: ExVal| -> Result<(String, Option<String>), CError> {
            if v.movible.is_some() || v.lugar.is_some() {
                Ok((v.c, v.lugar))
            } else {
                let tt = v.tipo.clone();
                let c = s.consume(v, Some(&tt))?;
                let t = s.temp(tt)?;
                s.emite(format!("{t} = ({c});"));
                Ok((t, None))
            }
        };
        let (ca, la) = amarra(self, va)?;
        let (cb, lb) = amarra(self, vb)?;
        let ta = self.depurar_a_temp(ca, la, &t)?;
        let tb2 = self.depurar_a_temp(cb, lb, &t)?;
        let sa = format!("kami_texto_cstr(&({ta}))");
        let sb = format!("kami_texto_cstr(&({tb2}))");
        let op = if es_eq { "!=" } else { "==" };
        let cmp = if es_eq { "!=" } else { "==" };
        match usr {
            None => {
                self.emite(format!("if (((strcmp({sa}, {sb})) {cmp} 0)) {{ KAMI_PANICO(\"assert falló: %s {op} %s\", {sa}, {sb}); }}"));
            }
            Some((f, cs)) => {
                let mut all = cs.clone();
                all.push(sa.clone());
                all.push(sb.clone());
                self.emite(format!("if (((strcmp({sa}, {sb})) {cmp} 0)) {{ KAMI_PANICO({f} \", %s {op} %s\", {}); }}", all.join(", ")));
            }
        }
        Ok(())
    }
}
