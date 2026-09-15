//! Pule el C generado: quita ruido de máquina sin cambiar su sentido.
//!
//! El emisor escribe a lo seguro (paréntesis de más, temporales sueltos,
//! líneas planas). Este pase deja el `.c` presentable para un humano:
//!
//! - colapsa paréntesis redundantes (`((x))` → `(x)`, `(f())` → `f()`),
//!   sin tocar casts, llamadas ni precedencia;
//! - pega literales adyacentes (`"a" "b"` → `"ab"`, lo mismo en C);
//! - junta declara+asigna (`T v; v = e;` → `T v = e;`);
//! - quita muertos (`(void)(0);`, `memset` tras `= {0}`);
//! - indenta y separa definiciones con blancos.
//!
//! Todo es conservador: ante la duda, conserva. La suite E2E (gcc `-Wall`
//! + correr + comparar) vigila que nada cambie de sentido.

/// Pule un `.c` completo (runtime incluido).
pub fn pulir(c: &str) -> String {
    let s = sin_paren_ruido(c);
    let s = normaliza_deref(&s);
    let s = pega_literales(&s);
    let lineas: Vec<String> = s.lines().map(|l| l.to_string()).collect();
    let lineas = sin_muertos(lineas);
    let lineas = junta_declara(lineas);
    let lineas = indenta(lineas);
    let lineas = sin_inalcanzable(lineas);
    let lineas = separa_defs(lineas);
    let mut o = lineas.join("\n");
    o.push('\n');
    o
}

fn es_ident(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_' || b == b'$'
}

/// Palabras que nunca se desparentizan (`(int)` es un cast, no ruido).
const GUARDA_TIPOS: &[&str] = &[
    "void", "char", "short", "int", "long", "float", "double", "signed", "unsigned", "_Bool",
    "const", "volatile", "static", "extern", "auto", "register", "sizeof",
];

// ---------------------------------------------------------------------------
// Paréntesis
// ---------------------------------------------------------------------------

/// Quita paréntesis redundantes en una pasada (los interiores primero).
fn sin_paren_ruido(c: &str) -> String {
    #[derive(PartialEq)]
    enum St {
        Normal,
        Cadena,
        Caracter,
        Linea,
        Bloque,
    }
    let e = c.as_bytes();
    let n = e.len();
    let mut out: Vec<u8> = Vec::with_capacity(n);
    // Pila de `(`: (posición en `out`, posición en la entrada).
    let mut pila: Vec<(usize, usize)> = Vec::new();
    let mut st = St::Normal;
    let mut i = 0;
    while i < n {
        let b = e[i];
        match st {
            St::Cadena => {
                out.push(b);
                if b == b'\\' && i + 1 < n {
                    i += 1;
                    out.push(e[i]);
                } else if b == b'"' {
                    st = St::Normal;
                }
                i += 1;
            }
            St::Caracter => {
                out.push(b);
                if b == b'\\' && i + 1 < n {
                    i += 1;
                    out.push(e[i]);
                } else if b == b'\'' {
                    st = St::Normal;
                }
                i += 1;
            }
            St::Linea => {
                out.push(b);
                if b == b'\n' {
                    st = St::Normal;
                }
                i += 1;
            }
            St::Bloque => {
                out.push(b);
                if b == b'*' && i + 1 < n && e[i + 1] == b'/' {
                    i += 1;
                    out.push(e[i]);
                    st = St::Normal;
                }
                i += 1;
            }
            St::Normal => {
                if b == b'"' {
                    st = St::Cadena;
                    out.push(b);
                    i += 1;
                } else if b == b'\'' {
                    st = St::Caracter;
                    out.push(b);
                    i += 1;
                } else if b == b'/' && i + 1 < n && e[i + 1] == b'/' {
                    st = St::Linea;
                    out.push(b);
                    i += 1;
                    out.push(e[i]);
                    i += 1;
                } else if b == b'/' && i + 1 < n && e[i + 1] == b'*' {
                    st = St::Bloque;
                    out.push(b);
                    i += 1;
                    out.push(e[i]);
                    i += 1;
                } else if b == b'(' {
                    pila.push((out.len(), i));
                    out.push(b);
                    i += 1;
                } else if b == b')' {
                    if let Some((p, abre)) = pila.pop() {
                        // Seguidor: primer no-blanco tras `)`.
                        let mut j = i + 1;
                        while j < n && (e[j] == b' ' || e[j] == b'\t') {
                            j += 1;
                        }
                        let seg = if j < n { Some(e[j]) } else { None };
                        let seg2 = if j + 1 < n { Some(e[j + 1]) } else { None };
                        if quita_par(&out, p, e, abre, seg, seg2) {
                            let mut ini = p + 1;
                            let mut fin = out.len();
                            while ini < fin && (out[ini] == b' ' || out[ini] == b'\t') {
                                ini += 1;
                            }
                            while fin > ini && (out[fin - 1] == b' ' || out[fin - 1] == b'\t') {
                                fin -= 1;
                            }
                            let dentro = out[ini..fin].to_vec();
                            out.truncate(p);
                            out.extend_from_slice(&dentro);
                        } else {
                            out.push(b);
                        }
                    } else {
                        out.push(b); // `)` huérfano: conserva
                    }
                    i += 1;
                } else {
                    out.push(b);
                    i += 1;
                }
            }
        }
    }
    String::from_utf8(out).unwrap_or_else(|_| c.to_string())
}

/// ¿Se quitan los paréntesis del par que cierra aquí?
/// `p` = posición del `(` en `out`; `abre` = su posición en la entrada.
fn quita_par(
    out: &[u8],
    p: usize,
    entrada: &[u8],
    abre: usize,
    seg: Option<u8>,
    seg2: Option<u8>,
) -> bool {
    let mut ini = p + 1;
    let mut fin = out.len();
    while ini < fin && (out[ini] == b' ' || out[ini] == b'\t') {
        ini += 1;
    }
    while fin > ini && (out[fin - 1] == b' ' || out[fin - 1] == b'\t') {
        fin -= 1;
    }
    if ini >= fin {
        return false; // `()`
    }
    let d = &out[ini..fin];
    // a) Doble: `((X))` → `(X)` (siempre vale).
    if d[0] == b'(' && cierra_al_final(d) {
        return true;
    }
    // b) Literal o llamada: quita si el líder deja (nunca es tipo).
    if es_literal(d) || es_llamada(d) {
        return lider_deja(entrada, abre);
    }
    // c) Identificador suelto: quita si líder y seguidor dejan (no cast).
    if es_ident_solo(d) {
        if let Ok(s) = std::str::from_utf8(d) {
            if GUARDA_TIPOS.contains(&s) {
                return false;
            }
        }
        return lider_deja(entrada, abre) && seguidor_deja(seg, seg2);
    }
    // d) Acceso puro (`a.b`, `p->f`, `x[i]`, `(e).f`): nunca es tipo;
    // quita si el líder deja (el seguidor solo veta `{`, literal compuesto).
    if es_acceso(d) {
        if seg == Some(b'{') {
            return false;
        }
        return lider_deja(entrada, abre);
    }
    false
}

/// ¿Cadena postfija pura con algún acceso (`.`, `->`, `[...]`)?
/// `a.b[i]->c`, `(e).f`, `f(x).y` valen; `a+b`, `(int)`, `f(x)` no.
fn es_acceso(d: &[u8]) -> bool {
    let n = d.len();
    // Salta un grupo balanceado desde `d[i]` (`(`/`[`); respeta strings/chars.
    fn salta_grupo(d: &[u8], mut i: usize) -> Option<usize> {
        let abre = d[i];
        let cierra = if abre == b'(' { b')' } else { b']' };
        let mut prof = 0i32;
        while i < d.len() {
            let b = d[i];
            if b == b'"' {
                i += 1;
                while i < d.len() && d[i] != b'"' {
                    if d[i] == b'\\' {
                        i += 1;
                    }
                    i += 1;
                }
                i += 1;
                continue;
            }
            if b == b'\'' {
                i += 1;
                while i < d.len() && d[i] != b'\'' {
                    if d[i] == b'\\' {
                        i += 1;
                    }
                    i += 1;
                }
                i += 1;
                continue;
            }
            if b == b'(' || b == b'[' {
                prof += 1;
            } else if b == b')' || b == b']' {
                prof -= 1;
                if prof == 0 {
                    return if b == cierra { Some(i + 1) } else { None };
                }
                if prof < 0 {
                    return None;
                }
            }
            i += 1;
        }
        None
    }
    fn blancos(d: &[u8], mut i: usize) -> usize {
        while i < d.len() && (d[i] == b' ' || d[i] == b'\t') {
            i += 1;
        }
        i
    }
    fn ident(d: &[u8], mut i: usize) -> Option<usize> {
        if i < d.len() && (d[i].is_ascii_alphabetic() || d[i] == b'_' || d[i] == b'$') {
            i += 1;
            while i < d.len() && es_ident(d[i]) {
                i += 1;
            }
            Some(i)
        } else {
            None
        }
    }
    // Átomo inicial: identificador o grupo.
    let mut i = blancos(d, 0);
    if i < n && (d[i] == b'(' || d[i] == b'[') {
        if d[i] == b'[' {
            return false;
        }
        let Some(j) = salta_grupo(d, i) else {
            return false;
        };
        i = j;
    } else if let Some(j) = ident(d, i) {
        i = j;
    } else {
        return false;
    }
    let mut vio_miembro = false;
    loop {
        i = blancos(d, i);
        if i >= n {
            break;
        }
        if d[i] == b'.' {
            vio_miembro = true;
            let Some(j) = ident(d, blancos(d, i + 1)) else {
                return false;
            };
            i = j;
        } else if d[i] == b'-' && i + 1 < n && d[i + 1] == b'>' {
            vio_miembro = true;
            let Some(j) = ident(d, blancos(d, i + 2)) else {
                return false;
            };
            i = j;
        } else if d[i] == b'[' {
            vio_miembro = true;
            let Some(j) = salta_grupo(d, i) else {
                return false;
            };
            i = j;
        } else if d[i] == b'(' {
            let Some(j) = salta_grupo(d, i) else {
                return false;
            };
            i = j;
        } else {
            return false;
        }
    }
    vio_miembro
}

/// El `(` de `d[0]` ¿cierra justo al final? (respeta strings/chars).
fn cierra_al_final(d: &[u8]) -> bool {
    let mut prof = 0i32;
    let mut i = 0;
    let n = d.len();
    while i < n {
        let b = d[i];
        if b == b'"' {
            i += 1;
            while i < n && d[i] != b'"' {
                if d[i] == b'\\' {
                    i += 1;
                }
                i += 1;
            }
            i += 1;
            continue;
        }
        if b == b'\'' {
            i += 1;
            while i < n && d[i] != b'\'' {
                if d[i] == b'\\' {
                    i += 1;
                }
                i += 1;
            }
            i += 1;
            continue;
        }
        if b == b'(' {
            prof += 1;
        } else if b == b')' {
            prof -= 1;
            if prof == 0 {
                return i + 1 == n;
            }
        }
        i += 1;
    }
    false
}

/// Un literal completo: `"..."`, número o `'c'`.
fn es_literal(d: &[u8]) -> bool {
    if d.is_empty() {
        return false;
    }
    if d[0] == b'"' {
        let mut i = 1;
        while i < d.len() && d[i] != b'"' {
            if d[i] == b'\\' {
                i += 1;
            }
            i += 1;
        }
        return i + 1 == d.len();
    }
    if d[0] == b'\'' {
        let mut i = 1;
        while i < d.len() && d[i] != b'\'' {
            if d[i] == b'\\' {
                i += 1;
            }
            i += 1;
        }
        return i + 1 == d.len() && i > 1;
    }
    // Número pp: empieza con dígito (o `.` + dígito); `e-`/`p+` valen.
    let mut i = 0;
    if d[0] == b'.' {
        if d.len() < 2 || !d[1].is_ascii_digit() {
            return false;
        }
        i = 2;
    } else {
        if !d[0].is_ascii_digit() {
            return false;
        }
        i = 1;
    }
    while i < d.len() {
        let b = d[i];
        if b.is_ascii_alphanumeric() || b == b'_' || b == b'.' || b == b'\'' {
            i += 1;
        } else if (b == b'-' || b == b'+')
            && i > 0
            && matches!(d[i - 1], b'e' | b'E' | b'p' | b'P')
        {
            i += 1;
        } else {
            return false;
        }
    }
    true
}

/// `f(...)` completo (sin `*`: no es declarador).
fn es_llamada(d: &[u8]) -> bool {
    if d.contains(&b'*') {
        return false;
    }
    let mut i = 0;
    if !d.is_empty() && (d[0].is_ascii_alphabetic() || d[0] == b'_' || d[0] == b'$') {
        i = 1;
        while i < d.len() && es_ident(d[i]) {
            i += 1;
        }
    } else {
        return false;
    }
    while i < d.len() && (d[i] == b' ' || d[i] == b'\t') {
        i += 1;
    }
    if i >= d.len() || d[i] != b'(' {
        return false;
    }
    cierra_al_final(&d[i..])
}

fn es_ident_solo(d: &[u8]) -> bool {
    if d.is_empty() || !(d[0].is_ascii_alphabetic() || d[0] == b'_' || d[0] == b'$') {
        return false;
    }
    d.iter().all(|&b| es_ident(b))
}

/// El líder (lo que precede al `(`) ¿deja quitar?
/// No cuando es lista de argumentos (`f(`, `if (`, `)(`, `](`).
fn lider_deja(entrada: &[u8], abre: usize) -> bool {
    let mut k = abre;
    while k > 0 && (entrada[k - 1] == b' ' || entrada[k - 1] == b'\t') {
        k -= 1;
    }
    let hubo_espacio = k != abre;
    if k == 0 {
        return true;
    }
    let a = entrada[k - 1];
    if a == b')' || a == b']' {
        return false;
    }
    if es_ident(a) {
        let mut w = k - 1;
        while w > 0 && es_ident(entrada[w - 1]) {
            w -= 1;
        }
        // `return (x);` → `return x;` (con espacio de por medio).
        return hubo_espacio && &entrada[w..k - 1 + 1] == b"return";
    }
    true
}

/// El seguidor (tras el `)`) ¿deja quitar un identificador?
/// Solo contextos donde un cast sería imposible (`(T).`, `(T);`... no vale).
fn seguidor_deja(seg: Option<u8>, seg2: Option<u8>) -> bool {
    match seg {
        None => true,
        Some(b'\n') | Some(b'\r') => true,
        Some(b'.') => true,
        Some(b'-') => seg2 == Some(b'>'),
        Some(b'[') | Some(b']') | Some(b')') | Some(b',') | Some(b';') | Some(b'}')
        | Some(b'?') | Some(b':') => true,
        // `=` solo (no `==`): `(x) = y` → `x = y`.
        Some(b'=') => seg2 != Some(b'='),
        _ => false,
    }
}

// ---------------------------------------------------------------------------
// Dereferencia (`&(*p).f` → `&p->f`, `&(*p)` suelto → `p`)
// ---------------------------------------------------------------------------

/// `&(*p).campo` → `&p->campo`; `&(*p)` suelto → `p` (el estándar ampara
/// la cancelación `&*`). No toca `&(*p)->`, `&(*p)[` ni `&(*p)(` (llamada).
fn normaliza_deref(c: &str) -> String {
    let e = c.as_bytes();
    let n = e.len();
    let mut out: Vec<u8> = Vec::with_capacity(n);
    let mut i = 0;
    // Copia un literal/char/comentario entero sin mirar dentro.
    fn copia_crudo(e: &[u8], o: &mut Vec<u8>, mut i: usize) -> usize {
        let n = e.len();
        if e[i] == b'/' && i + 1 < n && e[i + 1] == b'/' {
            while i < n && e[i] != b'\n' {
                o.push(e[i]);
                i += 1;
            }
            return i;
        }
        if e[i] == b'/' && i + 1 < n && e[i + 1] == b'*' {
            o.push(e[i]);
            o.push(e[i + 1]);
            i += 2;
            while i < n && !(e[i - 1] == b'*' && e[i] == b'/') {
                o.push(e[i]);
                i += 1;
            }
            if i < n {
                o.push(e[i]);
                i += 1;
            }
            return i;
        }
        let cierra = e[i];
        o.push(cierra);
        i += 1;
        while i < n && e[i] != cierra && e[i] != b'\n' {
            if e[i] == b'\\' && i + 1 < n {
                o.push(e[i]);
                i += 1;
            }
            o.push(e[i]);
            i += 1;
        }
        if i < n && e[i] == cierra {
            o.push(e[i]);
            i += 1;
        }
        i
    }
    fn es_ini(b: u8) -> bool {
        b.is_ascii_alphabetic() || b == b'_' || b == b'$'
    }
    while i < n {
        let b = e[i];
        if b == b'"' || b == b'\'' || (b == b'/' && i + 1 < n && (e[i + 1] == b'/' || e[i + 1] == b'*')) {
            i = copia_crudo(e, &mut out, i);
            continue;
        }
        // ¿`&(*IDENT)`?
        if b == b'&' && i + 3 < n && e[i + 1] == b'(' && e[i + 2] == b'*' && es_ini(e[i + 3]) {
            let mut j = i + 4;
            while j < n && es_ident(e[j]) {
                j += 1;
            }
            if j < n && e[j] == b')' {
                let id = &e[i + 3..j];
                let sig = if j + 1 < n { Some(e[j + 1]) } else { None };
                if sig == Some(b'.') {
                    out.push(b'&');
                    out.extend_from_slice(id);
                    out.push(b'-');
                    out.push(b'>');
                    i = j + 2; // salta `)` y `.`
                    continue;
                }
                if !matches!(sig, Some(b'[') | Some(b'-') | Some(b'(')) {
                    out.extend_from_slice(id);
                    i = j + 1; // salta `)`
                    continue;
                }
            }
        }
        out.push(b);
        i += 1;
    }
    String::from_utf8_lossy(&out).into_owned()
}

// ---------------------------------------------------------------------------
// Literales
// ---------------------------------------------------------------------------

/// Pega literales adyacentes (`"a" "b"` → `"ab"`).
fn pega_literales(c: &str) -> String {
    let e = c.as_bytes();
    let n = e.len();
    let mut out: Vec<u8> = Vec::with_capacity(n);
    let mut i = 0;
    let mut en_linea = false;
    let mut en_bloque = false;
    // Lee un literal desde el `"` en `i`; devuelve (contenido, fin tras el cierre).
    fn lee_lit(e: &[u8], i: usize) -> Option<(Vec<u8>, usize)> {
        let mut v = Vec::new();
        let mut j = i + 1;
        while j < e.len() && e[j] != b'"' {
            if e[j] == b'\n' {
                return None;
            }
            if e[j] == b'\\' && j + 1 < e.len() {
                v.push(e[j]);
                j += 1;
                v.push(e[j]);
                j += 1;
            } else {
                v.push(e[j]);
                j += 1;
            }
        }
        if j >= e.len() {
            return None;
        }
        Some((v, j + 1))
    }
    while i < n {
        let b = e[i];
        if en_linea {
            out.push(b);
            if b == b'\n' {
                en_linea = false;
            }
            i += 1;
            continue;
        }
        if en_bloque {
            out.push(b);
            if b == b'*' && i + 1 < n && e[i + 1] == b'/' {
                i += 1;
                out.push(e[i]);
                en_bloque = false;
            }
            i += 1;
            continue;
        }
        if b == b'/' && i + 1 < n && e[i + 1] == b'/' {
            en_linea = true;
            out.push(b);
            i += 1;
            out.push(e[i]);
            i += 1;
            continue;
        }
        if b == b'/' && i + 1 < n && e[i + 1] == b'*' {
            en_bloque = true;
            out.push(b);
            i += 1;
            out.push(e[i]);
            i += 1;
            continue;
        }
        if b == b'\'' {
            // Char: copia tal cual.
            out.push(b);
            i += 1;
            while i < n && e[i] != b'\'' && e[i] != b'\n' {
                if e[i] == b'\\' && i + 1 < n {
                    out.push(e[i]);
                    i += 1;
                }
                out.push(e[i]);
                i += 1;
            }
            if i < n && e[i] == b'\'' {
                out.push(b'\'');
                i += 1;
            }
            continue;
        }
        if b == b'"' {
            if i > 0 && es_ident(e[i - 1]) {
                let mut w = i - 1;
                while w > 0 && es_ident(e[w - 1]) {
                    w -= 1;
                }
                // Prefijo de codificación: copia el literal entero sin pegarlo.
                if matches!(&e[w..i], [b'L'] | [b'u'] | [b'U'] | [b'u', b'8']) {
                    if let Some((lit, j)) = lee_lit(e, i) {
                        out.push(b'"');
                        out.extend_from_slice(&lit);
                        out.push(b'"');
                        i = j;
                        continue;
                    }
                    out.push(b);
                    i += 1;
                    continue;
                }
                // Si no: macro (`PRId32"..."`); cae abajo y pega adyacentes.
            }
            let Some((mut acc, mut j)) = lee_lit(e, i) else {
                out.push(b);
                i += 1;
                continue;
            };
            // Cadena de adyacentes.
            loop {
                let mut k = j;
                while k < n && (e[k] == b' ' || e[k] == b'\t') {
                    k += 1;
                }
                if k < n && e[k] == b'"' {
                    if let Some((mas, j2)) = lee_lit(e, k) {
                        acc.extend_from_slice(&mas);
                        j = j2;
                        continue;
                    }
                }
                break;
            }
            out.push(b'"');
            out.extend_from_slice(&acc);
            out.push(b'"');
            i = j;
            continue;
        }
        out.push(b);
        i += 1;
    }
    String::from_utf8(out).unwrap_or_else(|_| c.to_string())
}

// ---------------------------------------------------------------------------
// Muertos y juntas (por líneas)
// ---------------------------------------------------------------------------

/// Quita `(void)(0);` y `memset` redundante tras `= {0}`.
fn sin_muertos(lineas: Vec<String>) -> Vec<String> {
    let mut out: Vec<String> = Vec::with_capacity(lineas.len());
    for l in lineas {
        let t: String = l.chars().filter(|c| !c.is_whitespace()).collect();
        if t == "(void)(0);" {
            continue;
        }
        // `memset(&V,0,sizeof(V));` justo tras una línea que deja V en ceros.
        if let Some(v) = memset_de(&t) {
            if let Some(prev) = out.last() {
                if deja_en_ceros(prev, &v) {
                    continue;
                }
            }
        }
        out.push(l);
    }
    out
}

/// `memset(&V,0,sizeof(V));` (sin blancos) → `Some(V)`.
fn memset_de(t: &str) -> Option<String> {
    let r = t.strip_prefix("memset(&")?;
    let coma = r.find(",0,sizeof(")?;
    let v = &r[..coma];
    if v.is_empty() || !v.bytes().all(es_ident) || v.bytes().next().is_some_and(|b| b.is_ascii_digit()) {
        return None;
    }
    let r2 = &r[coma + ",0,sizeof(".len()..];
    if r2 == format!("{v}));") {
        Some(v.to_string())
    } else {
        None
    }
}

/// ¿La línea deja `V` en ceros? (`T V = {0};`, con blancos tolerados).
fn deja_en_ceros(prev: &str, v: &str) -> bool {
    let t = prev.trim();
    let t = match t.strip_suffix(';') {
        Some(x) => x,
        None => return false,
    };
    let ig = match t.rfind('=') {
        Some(i) => i,
        None => return false,
    };
    let der: String = t[ig + 1..].chars().filter(|c| !c.is_whitespace()).collect();
    if der != "{0}" {
        return false;
    }
    let izq = t[..ig].trim_end();
    let pre = match izq.strip_suffix(v) {
        Some(x) => x,
        None => return false,
    };
    if !pre.is_empty() && es_ident(pre.as_bytes()[pre.len() - 1]) {
        return false;
    }
    !izq.bytes().any(|b| {
        matches!(
            b,
            b'(' | b')' | b'[' | b']' | b'{' | b'}' | b',' | b':' | b'='
        )
    })
}

/// Suelta líneas inalcanzables tras `return`/`goto` rasos. Corre TRAS
/// `indenta` (usa indents canónicos): suelta lo que sigue al mismo o mayor
/// indent hasta `}`, etiqueta, `case`/`default` o menor indent. `} else {` e
/// `if (c) return;` no ciegan: solo líneas que EMPIEZAN con la palabra.
fn sin_inalcanzable(lineas: Vec<String>) -> Vec<String> {
    fn indent_de(l: &str) -> usize {
        l.len() - l.trim_start_matches(' ').len()
    }
    /// ¿Etiqueta (`fin: ;`, `case 1:`, `default:`)? `a ? b : c;` no vale.
    fn es_etiqueta(t: &str) -> bool {
        if t.starts_with("case ") || t.starts_with("default:") || t.starts_with("default :") {
            return true;
        }
        let antes = t.split(':').next().unwrap_or("");
        !antes.is_empty()
            && t.contains(':')
            && antes.bytes().all(|b| b.is_ascii_alphanumeric() || b == b'_' || b == b'$')
    }
    // OJO: `}` NO frena por sí solo: si va al mismo o mayor indent es
    // el cierre de un bloque muerto interno y cae junto con su `{`
    // (balanceado: su `{` también cayó por ir al mismo o mayor indent).
    // Solo despiertan el menor indent y las etiquetas (destinos de salto).
    let mut out: Vec<String> = Vec::with_capacity(lineas.len());
    let mut ciega: Option<usize> = None; // indent del return/goto vigente
    for l in lineas {
        let t = l.trim();
        if t.is_empty() {
            if ciega.is_none() {
                out.push(l);
            }
            continue;
        }
        let ind = indent_de(&l);
        if let Some(bloq) = ciega {
            if ind < bloq || es_etiqueta(t) {
                ciega = None;
            } else {
                continue;
            }
        }
        let raso = t.starts_with("return;")
            || t.starts_with("return ")
            || t.starts_with("return(")
            || t.starts_with("goto ")
            || t == "return"
            || t == "goto";
        out.push(l);
        if raso {
            ciega = Some(ind);
        }
    }
    out
}

/// Junta `T V;` + `V = E;` (o `T V = {0};` + `V = E;`) en una línea.
fn junta_declara(lineas: Vec<String>) -> Vec<String> {
    let mut out: Vec<String> = Vec::with_capacity(lineas.len());
    for l in lineas {
        let t = l.trim();
        if let Some((v, e)) = parte_asigna(t) {
            if let Some(prev) = out.last() {
                if let Some(tipo) = declara_de(prev.trim(), &v) {
                    if !menciona(&e, &v) {
                        out.pop();
                        if tipo.is_empty() {
                            out.push(format!("{v} = {e};"));
                        } else if tipo.ends_with('*') {
                            let base = tipo.trim_end_matches('*').trim_end();
                            if base.is_empty() {
                                out.push(format!("{tipo}{v} = {e};"));
                            } else {
                                let asts: String =
                                    tipo[base.len()..].chars().filter(|&c| c == '*').collect();
                                out.push(format!("{base} {asts}{v} = {e};"));
                            }
                        } else {
                            out.push(format!("{tipo} {v} = {e};"));
                        }
                        continue;
                    }
                }
            }
        }
        out.push(l);
    }
    out
}

/// `V = E;` → `(V, E)` (E sin `;` interior).
fn parte_asigna(t: &str) -> Option<(String, String)> {
    let ig = t.find('=')?;
    // No `==` (ni `<=`/`>=`/`!=`: esos no casan `^V =` de todos modos).
    if t.as_bytes().get(ig + 1) == Some(&b'=') {
        return None;
    }
    let v = t[..ig].trim();
    if v.is_empty() || !v.bytes().all(es_ident) || v.bytes().next().is_some_and(|b| b.is_ascii_digit()) {
        return None;
    }
    if GUARDA_TIPOS.contains(&v) {
        return None;
    }
    let mut e = t[ig + 1..].trim();
    e = e.strip_suffix(';')?;
    if e.is_empty() || e.contains(';') {
        return None;
    }
    Some((v.to_string(), e.trim().to_string()))
}

/// `T V;` → `Some(T)`; `T V = {0};` → `Some(T)`; si no, `None`.
fn declara_de(prev: &str, v: &str) -> Option<String> {
    if prev.starts_with('#') || prev.starts_with('}') {
        return None;
    }
    let t = prev.trim();
    let t = match t.strip_suffix(';') {
        Some(x) => x.trim_end(),
        None => return None,
    };
    // ¿Trae `= {0}`?
    if let Some(ig) = t.rfind('=') {
        let der: String = t[ig + 1..].chars().filter(|c| !c.is_whitespace()).collect();
        if der == "{0}" {
            let izq = t[..ig].trim_end();
            let pre = izq.strip_suffix(v)?;
            if !pre.is_empty() && es_ident(pre.as_bytes()[pre.len() - 1]) {
                return None;
            }
            if izq.bytes().any(|b| {
                matches!(
                    b,
                    b'(' | b')' | b'[' | b']' | b'{' | b'}' | b',' | b':' | b'='
                )
            }) {
                return None;
            }
            return Some(pre.trim().to_string());
        }
        return None; // otro `=` (ya trae valor)
    }
    // Variante pelada `T V`.
    if t.bytes().any(|b| matches!(b, b'(' | b')' | b'[' | b']' | b'{' | b'}' | b',' | b':')) {
        return None;
    }
    let pre = t.strip_suffix(v)?;
    if pre.is_empty() || es_ident(pre.as_bytes()[pre.len() - 1]) {
        return None;
    }
    let tipo = pre.trim();
    if tipo.is_empty() {
        return None;
    }
    Some(tipo.to_string())
}

/// ¿El identificador `v` aparece en `e` como palabra?
fn menciona(e: &str, v: &str) -> bool {
    let b = e.as_bytes();
    let n = b.len();
    let m = v.len();
    let mut i = 0;
    while i + m <= n {
        if &e[i..i + m] == v {
            let antes = i == 0 || !es_ident(b[i - 1]);
            let despues = i + m == n || !es_ident(b[i + m]);
            if antes && despues {
                return true;
            }
            i += m;
        } else {
            i += 1;
        }
    }
    false
}

// ---------------------------------------------------------------------------
// Indentación y blancos
// ---------------------------------------------------------------------------

/// Indenta con 4 espacios (contando llaves fuera de strings/comentarios).
/// Las líneas `#...` quedan en columna 0; los comentarios de bloque y las
/// continuaciones `\` se preservan tal cual.
fn indenta(lineas: Vec<String>) -> Vec<String> {
    let mut out = Vec::with_capacity(lineas.len());
    let mut prof: i32 = 0;
    let mut en_bloque = false;
    let mut continua = false;
    for l in lineas {
        let (delta, cierres_ini, en_bloque_fin) = analiza_linea(&l, en_bloque);
        let preservar = continua || en_bloque;
        let crudo_sigue = l.as_bytes().last() == Some(&b'\\');
        if l.trim().is_empty() {
            out.push(String::new());
        } else if preservar {
            out.push(l.clone());
        } else {
            let t = l.trim_start();
            if t.starts_with('#') {
                out.push(t.to_string());
            } else {
                let mut nivel = (prof - cierres_ini).max(0);
                if es_etiqueta(t) {
                    nivel = (nivel - 1).max(0);
                }
                out.push(format!("{}{}", "    ".repeat(nivel as usize), t.trim_start()));
            }
        }
        prof = (prof + delta).max(0);
        en_bloque = en_bloque_fin;
        continua = crudo_sigue;
    }
    out
}

/// Etiqueta (`fin0:`), `case` o `default`: van un nivel menos.
fn es_etiqueta(t: &str) -> bool {
    let b = t.as_bytes();
    if t.starts_with("case") {
        return b.get(4).is_some_and(|&c| !es_ident(c));
    }
    if t.starts_with("default") {
        return b.get(7).is_some_and(|&c| !es_ident(c));
    }
    let mut i = 0;
    if !b.is_empty() && (b[0].is_ascii_alphabetic() || b[0] == b'_' || b[0] == b'$') {
        i = 1;
        while i < b.len() && es_ident(b[i]) {
            i += 1;
        }
    } else {
        return false;
    }
    if i < b.len() && b[i] == b':' {
        return b.get(i + 1).is_none_or(|&c| c == b' ' || c == b'\t' || c == b';');
    }
    false
}

/// `(delta_llaves, cierres_iniciales, en_bloque_al_salir)`.
fn analiza_linea(l: &str, en_bloque_ini: bool) -> (i32, i32, bool) {
    let b = l.as_bytes();
    let n = b.len();
    let mut delta = 0i32;
    let mut cierres_ini = 0i32;
    let mut inicio = true; // aún no hay token real
    let mut en_bloque = en_bloque_ini;
    let mut i = 0;
    while i < n {
        let c = b[i];
        if en_bloque {
            if c == b'*' && i + 1 < n && b[i + 1] == b'/' {
                en_bloque = false;
                i += 2;
            } else {
                i += 1;
            }
            continue;
        }
        if c == b'"' {
            inicio = false;
            i += 1;
            while i < n && b[i] != b'"' {
                if b[i] == b'\\' {
                    i += 1;
                }
                i += 1;
            }
            i += 1;
            continue;
        }
        if c == b'\'' {
            inicio = false;
            i += 1;
            while i < n && b[i] != b'\'' {
                if b[i] == b'\\' {
                    i += 1;
                }
                i += 1;
            }
            i += 1;
            continue;
        }
        if c == b'/' && i + 1 < n && b[i + 1] == b'/' {
            break; // el resto es comentario
        }
        if c == b'/' && i + 1 < n && b[i + 1] == b'*' {
            i += 2;
            // ¿Cierra en la misma línea?
            let mut j = i;
            let mut cerro = false;
            while j + 1 < n {
                if b[j] == b'*' && b[j + 1] == b'/' {
                    cerro = true;
                    j += 2;
                    break;
                }
                j += 1;
            }
            if cerro {
                i = j;
            } else {
                en_bloque = true;
                break;
            }
            continue;
        }
        if c == b' ' || c == b'\t' || c == b'\r' {
            i += 1;
            continue;
        }
        if c == b'{' {
            inicio = false;
            delta += 1;
        } else if c == b'}' {
            if inicio {
                cierres_ini += 1;
            }
            inicio = false;
            delta -= 1;
        } else {
            inicio = false;
        }
        i += 1;
    }
    (delta, cierres_ini, en_bloque)
}

/// Blanco tras cada `}` de cierre (pero no antes de `}`, `else`, `while`, `#`).
fn separa_defs(lineas: Vec<String>) -> Vec<String> {
    let mut out = Vec::with_capacity(lineas.len());
    let n = lineas.len();
    for (k, l) in lineas.iter().enumerate() {
        out.push(l.clone());
        let t = l.trim();
        let cierra = t.starts_with('}') && (t == "}" || t.ends_with(';'));
        if !cierra || k + 1 >= n {
            continue;
        }
        let sig = lineas[k + 1].trim();
        if sig.is_empty() || sig.starts_with('}') || sig.starts_with('#') {
            continue;
        }
        if palabra_ini(sig, "else") || palabra_ini(sig, "while") {
            continue;
        }
        out.push(String::new());
    }
    out
}

fn palabra_ini(s: &str, w: &str) -> bool {
    if let Some(r) = s.strip_prefix(w) {
        r.is_empty() || !es_ident(r.as_bytes()[0])
    } else {
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parentesis() {
        assert_eq!(sin_paren_ruido("x = ((y));"), "x = y;");
        assert_eq!(sin_paren_ruido("x = (y);"), "x = y;");
        assert_eq!(sin_paren_ruido("return (t0);"), "return t0;");
        assert_eq!(sin_paren_ruido("f((15));"), "f(15);");
        assert_eq!(sin_paren_ruido("if (((a % b)) == (0))"), "if ((a % b) == 0)");
        // Llamadas y casts se respetan.
        assert_eq!(sin_paren_ruido("f(x);"), "f(x);");
        assert_eq!(sin_paren_ruido("if (x) {}"), "if (x) {}");
        assert_eq!(sin_paren_ruido("x = (int32_t)(t0);"), "x = (int32_t)(t0);");
        assert_eq!(sin_paren_ruido("x = (int)t0;"), "x = (int)t0;");
        assert_eq!(sin_paren_ruido("return (KamiTexto){0};"), "return (KamiTexto){0};");
        assert_eq!(sin_paren_ruido("(t2).nombre = f((t1));"), "t2.nombre = f(t1);");
        // Accesos sí se limpian.
        assert_eq!(sin_paren_ruido("(t2).nombre = v;"), "t2.nombre = v;");
        assert_eq!(sin_paren_ruido("f(&(p));"), "f(&p);");
        assert_eq!(sin_paren_ruido("(*(s)).c = 1;"), "(*s).c = 1;");
        // Accesos puros sí se limpian.
        assert_eq!(sin_paren_ruido("&((*s).nombre);"), "&(*s).nombre;");
        assert_eq!(sin_paren_ruido("x = (a.b);"), "x = a.b;");
        assert_eq!(sin_paren_ruido("f(&(p->f));"), "f(&p->f);");
        assert_eq!(sin_paren_ruido("x = (a[i]);"), "x = a[i];");
        assert_eq!(sin_paren_ruido("x = t3[(t4)];"), "x = t3[t4];");
        // ...pero no binarios ni literales compuestos.
        assert_eq!(sin_paren_ruido("x = (a + b);"), "x = (a + b);");
        assert_eq!(sin_paren_ruido("x = (int[3]){0};"), "x = (int[3]){0};");
        assert_eq!(sin_paren_ruido("f((a.b));"), "f(a.b);");
        // Strings intactos.
        assert_eq!(sin_paren_ruido("f(\"((\");"), "f(\"((\");");
    }

    #[test]
    fn literales() {
        assert_eq!(pega_literales("f(\"a\" \"b\");"), "f(\"ab\");");
        assert_eq!(pega_literales("f(\"\"\"%s\"\"\"\"\\n\");"), "f(\"%s\\n\");");
        assert_eq!(pega_literales("f(\"a\", \"b\");"), "f(\"a\", \"b\");");
        assert_eq!(
            pega_literales("printf(\"%\" PRId32\"\"\"\\n\", n);"),
            "printf(\"%\" PRId32\"\\n\", n);"
        );
        assert_eq!(
            pega_literales("f(PRId32\"a\" \"b\");"),
            "f(PRId32\"ab\");"
        );
        assert_eq!(pega_literales("f(L\"x\" \"y\");"), "f(L\"x\" \"y\");");
    }

    #[test]
    fn muertos_y_juntas() {
        let l = vec![
            "(void)(0);".to_string(),
            "Persona t2 = {0};".to_string(),
            "memset(&t2, 0, sizeof(t2));".to_string(),
            "int32_t t5;".to_string(),
            "t5 = (i);".to_string(),
            "KamiTexto t0 = {0};".to_string(),
            "t0 = kami_texto_nuevo();".to_string(),
            "t1 = f(&t1);".to_string(),
            "int32_t *t0;".to_string(),
            "t0 = &n;".to_string(),
            "const char* t1;".to_string(),
            "t1 = \"Mila\";".to_string(),
        ];
        let l = sin_muertos(l);
        assert_eq!(l.len(), 10);
        let l = junta_declara(l);
        assert_eq!(
            l,
            vec![
                "Persona t2 = {0};",
                "int32_t t5 = (i);",
                "KamiTexto t0 = kami_texto_nuevo();",
                "t1 = f(&t1);",
                "int32_t *t0 = &n;",
                "const char *t1 = \"Mila\";",
            ]
        );
    }

    #[test]
    fn deref() {
        assert_eq!(normaliza_deref("x = &(*p).campo;"), "x = &p->campo;");
        assert_eq!(normaliza_deref("f(&(*t));"), "f(t);");
        assert_eq!(normaliza_deref("x = &(*p)->f;"), "x = &(*p)->f;");
        assert_eq!(normaliza_deref("x = &(*p)[i];"), "x = &(*p)[i];");
        assert_eq!(normaliza_deref("x = &(*f)(a);"), "x = &(*f)(a);");
        assert_eq!(normaliza_deref("s = \"&(*p).f\";"), "s = \"&(*p).f\";");
        assert_eq!(normaliza_deref("/* &(*p).f */ x;"), "/* &(*p).f */ x;");
        assert_eq!(normaliza_deref("// &(*p)\nx;"), "// &(*p)\nx;");
    }

    #[test]
    fn inalcanzable() {
        let v = |ls: &[&str]| ls.iter().map(|l| l.to_string()).collect::<Vec<_>>();
        // return tras return se suelta (con blancos muertos entremedio).
        assert_eq!(
            sin_inalcanzable(v(&["    return t0;", "", "    return (KamiTexto){0};", "}"])),
            v(&["    return t0;", "}"])
        );
        // `if (c) return;` no ciega.
        assert_eq!(
            sin_inalcanzable(v(&["    if (!self) return;", "    kami_x(&a);", "}"])),
            v(&["    if (!self) return;", "    kami_x(&a);", "}"])
        );
        // goto ciega hasta la etiqueta (pero no se la come).
        assert_eq!(
            sin_inalcanzable(v(&["    goto fin;", "    x = 1;", "fin: ;", "    y = 2;"])),
            v(&["    goto fin;", "fin: ;", "    y = 2;"])
        );
        // Bloque entero tras return cae completo (llaves incluidas).
        assert_eq!(
            sin_inalcanzable(v(&["    return;", "    {", "        z();", "    }", "    w();", "}"])),
            v(&["    return;", "}"])
        );
        // `} else {` frena (la rama else vive).
        assert_eq!(
            sin_inalcanzable(v(&[
                "    if (c) {",
                "        return x;",
                "    } else {",
                "        y();",
                "    }",
                "}",
            ])),
            v(&[
                "    if (c) {",
                "        return x;",
                "    } else {",
                "        y();",
                "    }",
                "}",
            ])
        );
        // Ternario con `:` no es etiqueta: tras goto cae.
        assert_eq!(
            sin_inalcanzable(v(&["    goto fin;", "    v = a ? b : c;", "fin: ;"])),
            v(&["    goto fin;", "fin: ;"])
        );
    }

    #[test]
    fn indentacion() {
        let l = vec![
            "int f(void) {".to_string(),
            "int x;".to_string(),
            "if (x) {".to_string(),
            "x = 1;".to_string(),
            "}".to_string(),
            "fin0: ;".to_string(),
            "}".to_string(),
        ];
        let l = indenta(l);
        assert_eq!(
            l,
            vec![
                "int f(void) {",
                "    int x;",
                "    if (x) {",
                "        x = 1;",
                "    }",
                "fin0: ;",
                "}",
            ]
        );
    }
}
