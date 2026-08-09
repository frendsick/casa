# ? uses a safe structural propagation protocol

The `?` operator remains independent of `Option`, `Result`, and other nominal type identities. A type supports `?` structurally when it provides compatible `is_ok`, `unwrap`, and `propagate` methods. `is_ok` borrows the value, `unwrap` consumes a successful value and produces the operation's output, and `propagate` consumes a failed value and produces the enclosing function's declared return type.

Conceptually, `value ?` binds `value` to a hidden local, borrows it for `is_ok`, and then consumes it exactly once: the failure branch calls `propagate` and returns, while the success branch calls `unwrap`. The compiler validates both branch stack effects without reinterpreting the value's representation. The initial protocol supports functions with one declared return value.

## Considered options

- Continuing the current `dup` plus return-type cast desugaring preserves the smallest parser implementation, but duplicates an owned value and depends on an unchecked representation cast. It is incompatible with affine ownership and the removal of general casts.
- Recognizing the standard-library `Option` and `Result` declarations would make validation direct, but promote otherwise ordinary library enums into compiler-known types and exclude useful custom propagation types.
- A residual conversion protocol separates failure extraction from return-type reconstruction, but adds abstractions that the current propagation cases do not require.
- Adding `propagate` to the existing structural `is_ok` and `unwrap` contract makes the failure conversion explicit and statically checked while retaining custom-type support.

## Consequences

- `Option[T]` can propagate failure into `Option[U]`, and `Result[T E]` can propagate failure into `Result[U E]`, through ordinary generic `propagate` methods.
- A different `Result` error type is rejected unless the source type deliberately provides a compatible propagation conversion; callers otherwise convert it explicitly before `?`.
- `?` becomes a resolved operation whose ownership-aware control-flow meaning belongs to shared operation semantics rather than an unchecked parser-only rewrite.
- A malformed custom implementation may violate the behavioral agreement between `is_ok`, `unwrap`, and `propagate`, just as a structural comparison implementation may violate trait laws, but it cannot use `?` to forge a value of an unrelated return type.
- `Option` and `Result` remain ordinary standard-library enums, and user-defined types may support `?` without registration or compiler changes.
