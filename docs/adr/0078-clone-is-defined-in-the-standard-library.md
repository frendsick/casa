# Clone is defined in the standard library
status: amended by [ADR-0084](0084-standard-copy-extends-clone.md), [ADR-0092](0092-copy-provided-clone-composes-field-clone.md), and [ADR-0163](0163-standard-trait-derivation-is-a-complete-implementation.md)

`Clone` is declared as an ordinary trait in `std` rather than injected by the compiler:

```casa
trait Clone {
    fn clone $self -> self
}
```

Code imports it through the ordinary module system when it needs to name the trait. Clone implementations, bounds, method dispatch, trait implementation rules, and derivation otherwise use the same machinery as other standard-library traits. The compiler does not special-case `Option`, `Result`, arrays, or any other Clone implementation.

The compiler recognizes the canonical standard-library trait identity when expanding `derives Clone` and when supplying missing fieldwise behavior required by the standard Copy implementation. An unrelated user trait also named `Clone` does not acquire derivation behavior.

## Considered options

- Injecting Clone as a built-in trait would remove an import but introduce a trait-definition mechanism that Casa does not otherwise have.
- A standard-library declaration with one narrow derivation identity keeps generated behavior visible in Casa source without injecting the trait declaration.

## Consequences

- `import "std" { Clone }` makes `[T: Clone]`, `derives Clone`, and explicit Clone implementations available.
- Clone can evolve through its visible standard-library declaration, subject to its language-level contract.
- The compiler generates Clone method bodies in two documented cases only: explicit `derives Clone`, and the fieldwise fallback that the standard `Copy: Clone` declaration requires under ADR-0084 and ADR-0092. Every other body comes from a standard or handwritten implementation, and an explicit implementation always takes precedence over a generated one.
- Casa adds no general implicit prelude solely for Clone.
