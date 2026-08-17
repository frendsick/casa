# Clone is defined in the standard library

`Clone` is declared as an ordinary trait in `std` rather than injected by the compiler:

```casa
trait Clone {
    fn clone $self -> self
}
```

Code imports it through the ordinary module system when it needs to name the trait. Clone implementations, bounds, method dispatch, coherence, and derivation otherwise use the same machinery as other standard-library traits. The compiler does not special-case `Option`, `Result`, arrays, or any other Clone implementation.

The compiler recognizes the canonical standard-library trait identity when expanding `derives Clone` and when supplying missing fieldwise behavior required by the standard Copy implementation. An unrelated user trait also named `Clone` does not acquire derivation behavior.

## Considered options

- Injecting Clone as a built-in trait would remove an import but introduce a trait-definition mechanism that Casa does not otherwise have.
- A standard-library declaration with one narrow derivation identity keeps generated behavior visible in Casa source without injecting the trait declaration.

## Consequences

- `import "std" { Clone }` makes `[T: Clone]`, `derives Clone`, and explicit Clone implementations available.
- Clone can evolve through its visible standard-library declaration, subject to its language-level contract.
- The compiler owns no implicit Clone method bodies; bodies come from standard implementations, handwritten implementations, or explicit `derives Clone` generation.
- Casa adds no general implicit prelude solely for Clone.
