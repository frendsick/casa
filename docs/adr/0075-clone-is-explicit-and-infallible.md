# Clone is explicit and infallible

`Clone` is the language-wide capability for explicitly producing another valid equivalent value when duplication may allocate or run type-specific code. It is an ordinary trait declared in `std`:

```casa
trait Clone {
    fn clone $self -> self
}
```

Calling `clone` returns the cloned value directly. Allocation failure terminates the process under ADR-0013 and is not represented with `Option` or `Result`. A domain operation that can fail for another recoverable reason uses a separately named method with an explicit result type rather than implementing `Clone`.

Code imports `std` through the ordinary module system to name `std::Clone`.
Bounds, dispatch, and explicit implementations use ordinary trait machinery.
The compiler recognizes the canonical standard-library trait identity for
derivation under
[ADR-0163](0163-standard-trait-derivation-is-a-complete-implementation.md).
An unrelated user trait named Clone gains no derivation behavior. Injecting a
Clone declaration or a prelude solely for Clone would add a second trait-definition
mechanism and is rejected.

Clone is never implicit. Assignment, argument passing, field access, pattern binding, `dup`, and `over` do not fall back to `Clone`; source code must call `.clone` where the additional owner is wanted.

## Consequences

- Clone implementations may allocate and call other Clone implementations.
- A Clone implementation must leave the borrowed source valid. Types with uniquely owned mutable backing storage return a distinct owner. Non-owning raw pointers retain their ordinary aliasing semantics. Borrows do not implement Clone, and cloning a borrowed value returns an owner under [ADR-0150](0150-shared-borrow-duplication-is-not-copy-conformance.md).
- `Clone` does not imply that duplication is cheap.
- [ADR-0163](0163-standard-trait-derivation-is-a-complete-implementation.md) defines `derives Clone` through the existing narrow derivation mechanism.
