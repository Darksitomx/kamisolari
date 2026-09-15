# Kamisolari

Rust, con la gramática en español. El compilador es **el mismo rustc**.

```
funcion principal() {
    imprimir("hola mundo");
}
```

`principal` es `main`. `imprimir(...)` es `println!(...)` (sin `!`).

## Uso

```bash
cargo test
cargo run -- correr examples/helloworld.kami
cargo run -- correr --optimizar --salida /tmp/hola examples/hola.kami

cargo run -- es src/main.rs
cargo run -- es src/ --salida /tmp/src-es
cargo run -- es --todas src/main.rs
cargo run -- rust examples/rasgo.kami
cargo run -- construir --final .
cargo run -- c examples/pila.kami -o /tmp/pila.c
cc -std=c11 -O2 -o /tmp/pila /tmp/pila.c -lm && /tmp/pila
cargo run -- c examples/pila.kami --correr-c
```

## Flags

Los nombres van en español. Los equivalentes en inglés siguen valiendo.

| Español | Inglés | Qué hace |
|---|---|---|
| `--salida`, `-o` | `--out` | binario o archivo traducido |
| `--optimizar`, `-O` | `-O` | optimizar |
| `--final` | `--release` | cargo release |
| `--depurar`, `-g` | `-g` | info de debug |
| `--edicion <año>` | `--edition` | 2015 / 2018 / 2021 / 2024 |
| `--tipo-crate` | `--crate-type` | bin, lib, … |
| `--nombre-crate` | `--crate-name` | nombre del crate |
| `--libs`, `-L` | `-L` | buscar librerías |
| `--codegen`, `-C` | `-C` | opciones de codegen |
| `--cfg` | `--cfg` | cfg |
| `--todas` | | alarga pub/mod/crate |
| `--verboso`, `-v` | `--verbose` | verbose |
| `--callado`, `-q` | `--quiet` | callado |
| `--ver`, `-V` | `--version` | versión |
| `--ayuda`, `-h` | `--help` | esta ayuda |
| `--` | `--` | el resto se pasa a rustc |
| `correr` | `run` | traduce, rustc, ejecuta |
| `construir` | `build` | cargo build |
| `c` | | genera C11 desde .kami o .rs |
| `--autonomo` | | un solo .c con el runtime dentro |
| `--no-autonomo` | | el .c pide kami.h (default con -o) |
| `--correr-c`, `--ejecutar` | | compila el C con cc y lo ejecuta |
| `--cc <compilador>` | | compilador C (default: cc) |

`kamisolari --ayuda` enseña lo mismo.

## Backend C

`kamisolari c` baja tu programa a C11 legible (una pasada lo entiende,
otra lo emite). Si tu clase pide entregas en C, este es el camino:
escribe kamisolari, saca el `.c`.

```bash
kamisolari c programa.kami -o programa.c   # programa.c + kami.h al lado
cc -std=c11 -O2 -o programa programa.c -lm
kamisolari c programa.kami --correr-c      # atajo: genera, compila y corre
kamisolari c programa.kami -o todo.c --autonomo  # un solo .c con runtime
```

El .c generado pide `kami.h` al lado (se escribe junto a `-o`); con
`--autonomo` trae el runtime dentro (`KamiTexto`, chequeo de índices,
tablas de `match`) en un solo archivo. El backend confía en que el
programa vale: primero revísalo con `kamisolari correr`.

Cómo cae lo orientado a objetos:

| Rust/kamisolari | C |
|---|---|
| `estructura` | `typedef struct` + `__liberar` (drops por ámbito) |
| `implementa` (inherente) | funciones `Tipo__metodo(Tipo *yo, …)` |
| `rasgo` + `implementa Rasgo para T` | funciones `Tipo__Rasgo__metodo` |
| `&yo` / `&mut yo` | `const T*` / `T*` |
| `String`, `Vec<T>`, `Option<T>` | `KamiTexto`, `KamiVec_T`, `KamiOpcion_T` |

Cubre: structs, enums, tuplas, arreglos, vectores, `String`/`&str`,
`Option`/`Result`, `match` (con guardias), `if`/`sino`, `para`,
`mientras`, `ciclo` + `romper`/`continuar`, métodos, rasgos, `impl Drop`,
`impl Display`, `format!`/`println!` y mucha std
(`Vec`, `Option`, `Result`, numéricos, `char`, texto).

No cabe (error claro con código C00xx): genéricos, cierres, `async`, hilos,
`dyn` fuera de `Display`/`Debug`, macros de usuario ni crates externos. Los `unsafe`
crudos se bajan tal cual, bajo tu responsabilidad.

## Palabras que se quedan (no cambian)

Inglés que **ya es** la forma corta en español (`pub` = público, `mod` = módulo) o que significa lo mismo (`super`).

| Rust | Se queda | Por qué |
|---|---|---|
| `pub` | `pub` | corto de público |
| `mod` | `mod` | corto de módulo |
| `super` | `super` | igual en español |
| `mut` | `mut` | igual |
| `ref` | `ref` | igual |
| `crate` | `crate` | nombre del ecosistema |

Se quedan acrónimos y notación: `Ok`, `Err`, `Arc`, `Rc`, `i32`, `u8`, `usize`, `f64`, `bool`, `char`, `serde`, `tokio`, operadores.

Los tipos con nombre en inglés **sí** se traducen (rustc no los ve: ida y vuelta): `Vec` → `Lista`, `String` → `Texto`, `Option` → `Opcion`.

Con `--todas` también se alargan `pub` → `publico`, `mod` → `modulo`, `crate` → `paquete`.

## Palabras que cambian

| Rust | Kamisolari |
|---|---|
| `fn` | `funcion` |
| `let` | `sea` |
| `if` | `si` |
| `else` | `sino` |
| `while` | `mientras` |
| `for` | `para` |
| `in` | `en` |
| `as` | `como` |
| `use` | `usa` |
| `dyn` | `din` |
| `type` | `tipo` |
| `loop` | `ciclo` |
| `match` | `segun` |
| `return` | `retornar` |
| `break` | `romper` |
| `continue` | `continuar` |
| `struct` | `estructura` |
| `enum` | `enumeracion` |
| `impl` | `implementa` |
| `trait` | `rasgo` |
| `self` | `yo` |
| `Self` | `Yo` |
| `true` | `verdadero` |
| `false` | `falso` |
| `where` | `donde` |
| `const` | `constante` |
| `static` | `estatico` |
| `extern` | `externo` |
| `move` | `mover` |
| `async` | `asinc` |
| `await` | `esperar` |
| `unsafe` | `inseguro` |
| `box` | `caja` |
| `Box` | `Caja` |
| `new` | `nuevo` |
| `try` | `intenta` |
| `do` | `hacer` |
| `override` | `sobrescribe` |
| `abstract` | `abstracto` |
| `become` | `devenir` |
| `yield` | `ceder` |
| `typeof` | `tipode` |
| `unsized` | `sintamano` |
| `fn main()` | `funcion principal()` |
| `println!(...)` | `imprimir(...)` |

Ejemplos de frase:

- `if let` → `si sea`
- `for x in xs` → `para x en xs`
- `impl Foo for Bar` → `implementa Foo para Bar`
- `use std::io` → `usa std::io`
- `dyn Trait` → `din rasgo` (el nombre `Trait` no se toca; `trait` sí)
- `x.into()` → `x.hacia()`
- `Box::new` → `Caja::nuevo`
- `Some(1)` → `Alguno(1)`

## Métodos de std

Verbos al estilo `usa` (tercera persona). `into` no puede ser `en` (choca con `in`).

| Rust | Kamisolari |
|---|---|
| `.into()` | `.hacia()` |
| `from` / `From` | `desde` / `Desde` |
| `.clone()` | `.clona()` |
| `.unwrap()` | `.desenvuelve()` |
| `.expect()` | `.exige()` |
| `.len()` | `.largo()` |
| `.push()` | `.empuja()` |
| `.is_empty()` | `.esta_vacio()` |
| `.collect()` | `.recolecta()` |
| `.map()` / `.filter()` | `.mapea()` / `.filtra()` |
| `.to_string()` | `.a_texto()` |
| `.into_iter()` | `.hacia_iter()` |
| `Some` / `None` | `Alguno` / `Ninguno` |
| `format!` | `formato!` |

Lista completa: `src/prelude.rs`. Cada entrada tiene test en `tests/prelude.rs`.
