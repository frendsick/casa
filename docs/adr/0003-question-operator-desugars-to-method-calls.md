# ? operator desugars to method calls instead of special-casing Result/Option

The `?` operator desugars at parse time to `dup .is_ok if ! then (FnReturnType) return fi .unwrap` — the same pattern as `for` loops, which desugar to `.next`/`.is_some`/`.unwrap` calls. The compiler has zero knowledge of `Result`, `Option`, or any `Try` trait; it emits method calls and lets the existing typechecker validate them. Any user-defined type with `is_ok` and `unwrap` methods gets `?` support automatically.

**Considered Options**

- Hardcoded type names (`"Result"`, `"Option"`) in the typechecker and bytecode emitter: simplest to implement, but couples the compiler to stdlib types that are not built-in. No precedent — the compiler hardcodes trait names (`"Eq"`, `"Ord"`, `"Display"`) for operator lowering, but never type names beyond the built-in `"array"`.
- Trait-based validation (compiler checks for a `Try` trait): cleaner than type names, but adds coupling that `for` loops don't need. The `for` loop precedent shows pure desugaring works without any trait knowledge in the compiler.
- Pure desugaring with strict type matching (no cast on error path): simpler, but rejects `Result[A E]?` inside a fn returning `Result[B E]`, which is the dominant use case for error propagation.

**Consequences**

- `Option` gains an `is_ok` method (alias for `is_some`) so both Result and Option satisfy the same duck-type contract.
- Error messages for misuse are indirect ("no method `is_ok` on type `int`" rather than "`?` requires Result or Option") — consistent with `for` loop diagnostics.
- The type cast on the error path is a runtime no-op (same pointer, different type annotation) and is safe because all Casa enums share the same memory layout (tag@0, payload@8).
