
/* =====================================================================
 * kami.h — runtime del backend C de kamisolari.
 *
 * Acompaña al .c generado (o va inline con --autonomo). Todo es
 * `static inline`: no hay nada que compilar por separado.
 *
 * Modelo:
 * - `KamiTexto` = String (dueño; se libera con kami_texto_liberar).
 * - `&str`     = const char* (prestado, siempre NUL-terminado).
 * - `char`     = KamiChar (escalar unicode, uint32_t).
 * - Los vectores/monos los genera el .c según se usen.
 * ===================================================================== */
#ifndef KAMISOLARI_KAMI_H
#define KAMISOLARI_KAMI_H

/* getline/ssize_t son POSIX: pedirlos explícitamente (compila con -std=c11). */
#if !defined(_WIN32) && !defined(_WIN64)
#ifndef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 200809L
#endif
#include <sys/types.h>
#endif

#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef uint32_t KamiChar;

/* ---------------- fallos ---------------- */

#define KAMI_PANICO(...) \
    (fprintf(stderr, __VA_ARGS__), fputc('\n', stderr), abort(), (void)0)

#define KAMI_SIN_MEMORIA() \
    (fprintf(stderr, "kami: sin memoria\n"), abort(), (void)0)

/* Índice con chequeo (como Rust: fuera de rango = panics, no UB). */
static inline size_t kami_indice(long long i, size_t n) {
    if (i < 0 || (unsigned long long)i >= (unsigned long long)n) {
        fprintf(stderr, "kami: índice %lld fuera de rango (largo %zu)\n", i, n);
        abort();
    }
    return (size_t)i;
}

/* ---------------- Texto (String) ---------------- */

typedef struct {
    char *datos;
    size_t largo;
    size_t capacidad;
} KamiTexto;

static inline KamiTexto kami_texto_nuevo(void) {
    KamiTexto t;
    t.datos = NULL;
    t.largo = 0;
    t.capacidad = 0;
    return t;
}

static inline void kami_texto_reserva(KamiTexto *t, size_t extra) {
    size_t necesito = t->largo + extra + 1;
    if (necesito <= t->capacidad) {
        return;
    }
    size_t cap = t->capacidad ? t->capacidad : 16;
    while (cap < necesito) {
        cap *= 2;
    }
    char *nuevo = (char *)realloc(t->datos, cap);
    if (!nuevo) {
        KAMI_SIN_MEMORIA();
    }
    t->datos = nuevo;
    t->capacidad = cap;
}

static inline KamiTexto kami_texto_desde_n(const char *s, size_t n) {
    KamiTexto t = kami_texto_nuevo();
    if (n == 0) {
        return t;
    }
    kami_texto_reserva(&t, n);
    memcpy(t.datos, s, n);
    t.largo = n;
    t.datos[n] = '\0';
    return t;
}

static inline KamiTexto kami_texto_desde(const char *s) {
    if (!s) {
        return kami_texto_nuevo();
    }
    return kami_texto_desde_n(s, strlen(s));
}

static inline void kami_texto_liberar(KamiTexto *t) {
    if (t && t->datos) {
        free(t->datos);
        t->datos = NULL;
    }
    if (t) {
        t->largo = 0;
        t->capacidad = 0;
    }
}

static inline KamiTexto kami_texto_clona(const KamiTexto *t) {
    if (!t || !t->datos) {
        return kami_texto_nuevo();
    }
    return kami_texto_desde_n(t->datos, t->largo);
}

/* Siempre devuelve algo imprimible con %s (nunca NULL). */
static inline const char *kami_texto_cstr(const KamiTexto *t) {
    if (!t || !t->datos) {
        return "";
    }
    return t->datos;
}

static inline size_t kami_texto_largo(const KamiTexto *t) {
    return t ? t->largo : 0;
}

static inline bool kami_texto_vacio(const KamiTexto *t) {
    return !t || t->largo == 0;
}

static inline void kami_texto_empuja_n(KamiTexto *t, const char *s, size_t n) {
    if (n == 0) {
        return;
    }
    kami_texto_reserva(t, n);
    memcpy(t->datos + t->largo, s, n);
    t->largo += n;
    t->datos[t->largo] = '\0';
}

static inline void kami_texto_empuja(KamiTexto *t, const char *s) {
    if (s) {
        kami_texto_empuja_n(t, s, strlen(s));
    }
}

/* Agrega un escalar unicode codificado en UTF-8. */
static inline void kami_texto_empuja_char(KamiTexto *t, KamiChar c) {
    char b[4];
    size_t n = 0;
    if (c < 0x80) {
        b[0] = (char)c;
        n = 1;
    } else if (c < 0x800) {
        b[0] = (char)(0xC0 | (c >> 6));
        b[1] = (char)(0x80 | (c & 0x3F));
        n = 2;
    } else if (c < 0x10000) {
        b[0] = (char)(0xE0 | (c >> 12));
        b[1] = (char)(0x80 | ((c >> 6) & 0x3F));
        b[2] = (char)(0x80 | (c & 0x3F));
        n = 3;
    } else {
        b[0] = (char)(0xF0 | (c >> 18));
        b[1] = (char)(0x80 | ((c >> 12) & 0x3F));
        b[2] = (char)(0x80 | ((c >> 6) & 0x3F));
        b[3] = (char)(0x80 | (c & 0x3F));
        n = 4;
    }
    kami_texto_empuja_n(t, b, n);
}

static inline void kami_texto_empuja_fmt(KamiTexto *t, const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    va_list aq;
    va_copy(aq, ap);
    int n = vsnprintf(NULL, 0, fmt, aq);
    va_end(aq);
    if (n < 0) {
        va_end(ap);
        return;
    }
    kami_texto_reserva(t, (size_t)n);
    vsnprintf(t->datos + t->largo, (size_t)n + 1, fmt, ap);
    va_end(ap);
    t->largo += (size_t)n;
}

/* Agrega entre comillas con escapes (formato {:?}). */
static inline void kami_texto_empuja_depurado(KamiTexto *t, const char *s) {
    kami_texto_empuja_n(t, "\"", 1);
    if (s) {
        for (const unsigned char *p = (const unsigned char *)s; *p; p++) {
            switch (*p) {
            case '"':
                kami_texto_empuja_n(t, "\\\"", 2);
                break;
            case '\\':
                kami_texto_empuja_n(t, "\\\\", 2);
                break;
            case '\n':
                kami_texto_empuja_n(t, "\\n", 2);
                break;
            case '\t':
                kami_texto_empuja_n(t, "\\t", 2);
                break;
            case '\r':
                kami_texto_empuja_n(t, "\\r", 2);
                break;
            default:
                if (*p < 0x20 || *p == 0x7F) {
                    char b[8];
                    snprintf(b, sizeof b, "\\x%02x", *p);
                    kami_texto_empuja(t, b);
                } else {
                    kami_texto_empuja_n(t, (const char *)p, 1);
                }
                break;
            }
        }
    }
    kami_texto_empuja_n(t, "\"", 1);
}

static inline bool kami_texto_iguales(const KamiTexto *a, const KamiTexto *b) {
    const char *x = kami_texto_cstr(a);
    const char *y = kami_texto_cstr(b);
    return strcmp(x, y) == 0;
}

static inline int kami_texto_compara(const KamiTexto *a, const KamiTexto *b) {
    return strcmp(kami_texto_cstr(a), kami_texto_cstr(b));
}

/* `s + resto`: toma `a` por valor (el llamador ya la movió) y la extiende. */
static inline KamiTexto kami_texto_mas(KamiTexto a, const char *b) {
    kami_texto_empuja(&a, b);
    return a;
}

/* ---------------- impresión ---------------- */

static inline void kami_imprime_bool(bool v) {
    fputs(v ? "true" : "false", stdout);
}

/* Codifica el char en `b` (5 bytes) y devuelve `b` para usar con %s. */
static inline const char *kami_char_a_cstr(KamiChar c, char b[5]) {
    size_t n = 0;
    if (c < 0x80) {
        b[0] = (char)c;
        n = 1;
    } else if (c < 0x800) {
        b[0] = (char)(0xC0 | (c >> 6));
        b[1] = (char)(0x80 | (c & 0x3F));
        n = 2;
    } else if (c < 0x10000) {
        b[0] = (char)(0xE0 | (c >> 12));
        b[1] = (char)(0x80 | ((c >> 6) & 0x3F));
        b[2] = (char)(0x80 | (c & 0x3F));
        n = 3;
    } else {
        b[0] = (char)(0xF0 | (c >> 18));
        b[1] = (char)(0x80 | ((c >> 12) & 0x3F));
        b[2] = (char)(0x80 | ((c >> 6) & 0x3F));
        b[3] = (char)(0x80 | (c & 0x3F));
        n = 4;
    }
    b[n] = '\0';
    return b;
}

/* ---------------- entrada ---------------- */

/* Lee una línea de stdin (conserva el \n, como `read_line`). false en EOF. */
static inline bool kami_leer_linea(KamiTexto *out) {
    char *linea = NULL;
    size_t cap = 0;
#if defined(_WIN32) || defined(_WIN64)
    /* getline no existe en MSVC: bucle con fgets. */
    char tmp[256];
    bool leyo = false;
    while (fgets(tmp, sizeof tmp, stdin)) {
        leyo = true;
        kami_texto_empuja(out, tmp);
        if (strchr(tmp, '\n')) {
            break;
        }
    }
    return leyo;
#else
    ssize_t n = getline(&linea, &cap, stdin);
    if (n < 0) {
        free(linea);
        return false;
    }
    kami_texto_empuja_n(out, linea, (size_t)n);
    free(linea);
    return true;
#endif
}

/* ---------------- parseo (`.parse()`) ---------------- */

static inline bool kami_parsea_i64(const char *s, long long *out, bool con_signo) {
    if (!s || !*s) {
        return false;
    }
    char *fin = NULL;
    long long v = strtoll(s, &fin, 10);
    if (!fin || *fin != '\0') {
        return false;
    }
    if (!con_signo && v < 0) {
        return false;
    }
    *out = v;
    return true;
}

static inline bool kami_parsea_u64(const char *s, unsigned long long *out) {
    if (!s || !*s || *s == '-') {
        return false;
    }
    if (*s == '+') {
        s++;
    }
    char *fin = NULL;
    unsigned long long v = strtoull(s, &fin, 10);
    if (!fin || *fin != '\0') {
        return false;
    }
    *out = v;
    return true;
}

static inline bool kami_parsea_f64(const char *s, double *out) {
    if (!s || !*s) {
        return false;
    }
    char *fin = NULL;
    double v = strtod(s, &fin);
    if (!fin || *fin != '\0') {
        return false;
    }
    *out = v;
    return true;
}

#endif /* KAMISOLARI_KAMI_H */
