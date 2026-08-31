# Extern functions use the platform C ABI

The initial foreign-function surface is a bodyless `extern fn` declaration using the target platform's C ABI. Calling an extern function requires an unsafe context.

```casa
extern fn puts text:$cstr -> i32
```

An extern function returns zero or one ABI value; it cannot directly produce Casa's multiple stack outputs. The initial surface excludes variadics, aggregate-by-value ABI, callbacks, symbol aliases, and alternative ABI strings. Safe Casa functions validate foreign preconditions and expose ordinary safe contracts.

Compiler paths use the conventional native-toolchain split:

```text
-L path / --library-path path  Casa module lookup
-l name / --link-library name  native library to link
```

Small extern structs that classify into one or two System V eightbytes can pass
and return by value. Parameters require `Copy`. The classifier assigns
`INTEGER` and `SSE` classes before lowering, and assembly emission consumes that
classification. Register exhaustion moves the complete aggregate to the native
stack. Memory-class aggregates remain excluded.

The existing Casa `-L` / `--library-path` module option does not change.
Repeated native libraries are passed to the linker in command-line order.
Casa adds no `-I` alias, native link search option, or source-level link
attribute initially.
