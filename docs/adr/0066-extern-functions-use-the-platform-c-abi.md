# Extern functions use the platform C ABI

The initial foreign-function surface is a bodyless `extern fn` declaration using the target platform's C ABI. Calling an extern function requires an unsafe context.

```casa
extern fn puts text:$cstr -> i32
```

An extern function returns zero or one ABI value; it cannot directly produce Casa's multiple stack outputs. The initial surface excludes variadics, aggregate-by-value ABI, callbacks, symbol aliases, and alternative ABI strings. Safe Casa functions validate foreign preconditions and expose ordinary safe contracts.

Compiler paths use the conventional native-toolchain split:

```text
-I path / --module-path path   Casa module lookup
-L path / --link-search path   native-library lookup
-l name / --link-library name  native library to link
```

The current Casa `-L` / `--library-path` module option migrates to `-I` / `--module-path`. Repeated native search paths and libraries are passed to the linker in command-line order; Casa adds no source-level link attributes initially.
