# Static emission for array literals

Array literals whose elements are all compile-time constants (int, str, bool, char literals — including values folded from `const` declarations and `const fn` calls) are emitted as static data in the `.data` section rather than heap-allocated at runtime. The full array struct (header + elements) lives in `.data` with the linker resolving internal pointers. This makes patterns like `["a", "b", "c"].contains` zero-cost, unblocking #217.

**Considered Options**

- `.rodata` (read-only): eliminates accidental mutation but segfaults at runtime if anyone writes to a static array, with no compile-time guard. Rejected because Casa has no `const`/`mut` distinction for variables, so the failure mode is a silent crash.
- `.data` with copy-on-use: emit the template statically but `memcpy` into a fresh heap allocation when the array is bound to a mutable context. Preserves per-use isolation but adds complexity and only eliminates allocation for read-only uses — the common case already, so the complexity buys little.
- `.data` shared (chosen): all references to the same literal get the same static address. Mutating a literal array is an exotic edge case (writing past bounds is already undefined), and the motivating uses are read-only. Consistent with how string literals already work in `.data`.

**Consequences**

- Two calls to a function containing `["a", "b"]` receive the same array pointer. Mutation through one reference is visible to others. This is a semantic change from heap-allocated arrays where each call got a fresh copy.
- A new `InstValue::PushStaticArray` variant carries element data from bytecode compilation to the emitter, which writes the array into `.data` as `static_array_N` labels and pushes the label address at runtime.
- Arrays containing any non-literal element (variables, function call results from non-const fns) remain heap-allocated as before.
