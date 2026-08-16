# Safe code has no general casts

Casa removes the general `(Type)` cast syntax. Safe type changes are either inferred, constrained by an annotation or explicit generic argument, or performed by a named conversion that validates or constructs its result. Representation reinterpretation remains available only through named unsafe pointer and typed-memory operations.

## Considered options

- Keeping `(Type)` as a trust-based reinterpretation would let safe code forge owners and invalidate borrowing and destruction guarantees.
- Making `(Type)` a checked numeric conversion would reuse syntax, but retain an overloaded operation whose meaning changes between numeric, generic, pointer, and representation types.
- Restricting `(Type)` to `unsafe` would preserve a concise escape hatch, but hide which low-level operation is being requested.
- Removing the syntax gives annotations, conversions, and unsafe representation operations distinct and auditable roles.

## Consequences

- Empty and otherwise unresolved generic values use expected types, typed bindings, or explicit generic arguments instead of casts. Associated items use `Type[Arguments]::member`, and free generic functions use `function[Arguments]`; dummy type-value arguments are removed.
- Lossless numeric conversions use `Target::from`; exactly checked conversions use `Target::try_from -> Option[Target]`; deliberately rounded floating-point conversions use `Target::round_from`; float-to-integer truncation uses terminating `Target::trunc_from`; integer truncation uses `Target::wrapping_from`.
- Character conversion uses safe `character.codepoint` and `char::from_codepoint` APIs, plus the narrow unsafe `char::from_codepoint_unchecked` primitive, rather than integer reinterpretation.
- Floating-point representation uses the width-matched safe `from_bits` and `to_bits` operations rather than casts or raw pointers.
- Raw pointer reinterpretation, typed loads, and typed stores are named operations requiring `unsafe`; foreign declarations and safe wrappers carry their concrete boundary types.
- Casts between unrelated safe types are impossible, and Casa adds no `as` syntax.
- `?` returns failures through the source type's statically checked `propagate` method instead of retyping the source value as the enclosing function's return type.
- The compiler may trust that every safe owned value was constructed according to its declared type.
