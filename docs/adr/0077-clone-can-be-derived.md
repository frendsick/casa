# Clone can be derived
status: amended by [ADR-0150](0150-shared-borrow-duplication-is-not-copy-conformance.md) and [ADR-0163](0163-standard-trait-derivation-is-a-complete-implementation.md)
related issue: #314

Structs and enums may request `Clone` in the existing inline `derives` clause:

```casa
struct Pair[T] derives Clone {
    first: T
    second: T
}
```

The generated implementation is conditional on every owned field or variant payload implementing `Clone`. It clones fields in declaration order and reconstructs the same struct or enum variant. Under the standard `Copy: Clone` declaration, Copy fields satisfy this bound through ordinary supertrait satisfaction. Shared-borrow Clone implementations preserve their safe aliasing semantics, while an exclusive-borrow field makes derivation invalid.

Derivation generates ordinary explicit Clone behavior and may therefore allocate through field clones. It does not make the type `Copy`, enable `dup`, or permit implicit duplication.

## Consequences

- Generic `Pair[T] derives Clone` implements `Clone` when `T: Clone` without placing bounds on constructing or moving `Pair[T]`.
- A handwritten Clone method overrides the generated fallback under ADR-0090; two handwritten Clone implementations remain a conflict.
- Clone derivation reuses the established `derives` syntax and adds no general attribute or metaprogramming mechanism.
- Types choose whether to expose duplication; Clone is not inferred merely because all fields support it.
- Recursive Clone obligations are resolved as dependency cycles under ADR-0093 rather than rejected or expanded indefinitely.
