# Extern functions use the platform C ABI

The initial foreign-function surface is a bodyless `extern fn` declaration using the target platform's C ABI. Calling an extern function requires an unsafe context.

```casa
extern fn puts text:$cstr -> i32
```

An extern function returns zero or one ABI value. It cannot directly produce Casa's multiple stack outputs. The surface excludes variadics, callbacks, symbol aliases, and alternative ABI strings. Safe Casa functions validate foreign preconditions and expose ordinary safe contracts.

Compiler paths use the conventional native-toolchain split:

```text
-L path / --library-path path  Casa module lookup
-l name / --link-library name  native library to link
```

Extern structs can pass and return by value. Parameters require `Copy`. The
classifier assigns `INTEGER`, `SSE`, or `MEMORY` before lowering, and assembly
emission consumes that classification. Register exhaustion and memory-class
parameters move the complete aggregate to the native stack. Memory-class returns
use caller-owned hidden return storage.

The existing Casa `-L` / `--library-path` module option does not change.
Repeated native libraries are passed to the linker in command-line order.
Casa adds no `-I` alias, native link search option, or source-level link
attribute initially.
