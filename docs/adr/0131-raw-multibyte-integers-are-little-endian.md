# Raw multibyte integers are little-endian

`load16`, `load32`, `load64`, and their matching stores interpret bytes in x86-64 little-endian order:

```text
memory bytes: 01 02 03 04
load32:       0x04030201
```

This is both Casa's explicit behavior and the native behavior of its sole compilation target. `load8` and `store8` have no byte-order distinction.

## Consequences

- Binary parsing has deterministic byte order rather than an unspecified "native" format.
- Little-endian formats may use the raw operations directly after their ordinary bounds and unsafe validity checks.
- Big-endian protocols explicitly reverse or assemble bytes in library code.
- `ptr::read[T]` and `ptr::write[T]` continue to use the compiler's ordinary in-memory layout; for fixed-width integers on x86-64 that layout is also little-endian.
- A future target would need to preserve the specified operation semantics or introduce a separate target decision.
