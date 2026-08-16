# Copy is methodless

`Copy` is Casa's methodless standard marker for implicit duplication: copying always duplicates representation bits and never invokes user code. It enables implicit reuse of copied bindings plus `dup`, the copied operand of `over`, and ADR-0136's `copy` intrinsic for producing owned `T` from `$T`. Built-in scalars, shared borrows, and named function references automatically implement Copy; user-defined structs and enums opt in with `derives Copy` or an equivalent validated empty implementation.

The compiler accepts a Copy implementation only when every field is `Copy`, the type has no custom destruction, and raw duplication cannot create multiple owners or exclusive borrows. Copy has no method because there is no behavior to customize. An eligible user type may omit the implementation to preserve move-only semantic identity.

Casa initially deferred a generic deep-duplication capability. ADR-0074 and ADR-0075 later introduce explicit `Clone` while preserving this decision's boundary: `Copy` remains methodless, and `dup` and `over` remain `Copy`-only.

## Considered options

- Automatically making every representation-eligible user type `Copy` minimizes annotations, but prevents an all-scalar type from representing a deliberately unique capability or state token.
- Providing both `Copy` and `Clone` was initially deferred until owned arrays established a concrete need for explicit allocating duplication.
- Giving `Copy` a method suggests customizable behavior even though safe implicit copying must always be trivial bit duplication.
- Opt-in methodless `Copy` preserves semantic choice and keeps explicit deep duplication outside fundamental stack operations.

## Consequences

- `struct Point derives Copy` may be used with `dup`, `over`, `copy` through `$Point`, and ordinary implicit copying when its fields qualify.
- A representation-eligible struct that does not implement Copy remains move-only.
- `str`, `Bytes`, lists, maps, owned closures, and resource owners are not `Copy`; suitable types may separately implement `Clone`.
- Generic trivial duplication uses `[T: Copy]`; generic explicit duplication uses `[T: Clone]`.
- `Copy` never allocates under ADR-0072. Any relationship to Clone comes from the active trait declaration under ADR-0080.
