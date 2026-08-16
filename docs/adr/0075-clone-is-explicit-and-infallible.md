# Clone is explicit and infallible

`Clone` is the language-wide capability for explicitly producing another valid equivalent value when duplication may allocate or run type-specific code:

```casa
trait Clone {
    fn clone $self -> self
}
```

Calling `clone` returns the cloned value directly. Allocation failure terminates the process under ADR-0013 and is not represented with `Option` or `Result`. A domain operation that can fail for another recoverable reason uses a separately named method with an explicit result type rather than implementing `Clone`.

Clone is never implicit. Assignment, argument passing, field access, pattern binding, `dup`, and `over` do not fall back to `Clone`; source code must call `.clone` where the additional owner is wanted.

## Consequences

- Clone implementations may allocate and call other Clone implementations.
- A Clone implementation must leave the borrowed source valid. Types with uniquely owned mutable backing storage return a distinct owner; Clone implementations for non-owning values such as shared borrows and raw pointers retain their ordinary aliasing semantics.
- `Clone` does not imply that duplication is cheap.
- ADR-0077 later adds `derives Clone` through the existing narrow derivation mechanism.
