# Kamisolari — diseño

Frontend de Rust. No hay backend propio. rustc es el compilador.

## Decisión

Un dialecto con GC no puede comerse un crate de Tokio. Un mapeo de keywords sí.

Ida y vuelta:

```
.rs  --es-->  español  --rust-->  .rs  --rustc-->  binario
```

Los tokens que no son keyword no se tocan. Eso permite proyectos grandes.

## Prelude

`main` ↔ `principal`  
`println!(...)` ↔ `imprimir(...)`  (sin bang al escribir)

## Colisiones

`if` pasa a `si`. Un ident que se llame `si` se emite `r#si`.
`pub` / `mod` / `super` se quedan: ya son la forma corta de público / módulo / super.

## Lo que no vamos a hacer

- Traducir la std.
- Reimplementar el borrow checker.
- LLVM propio.
