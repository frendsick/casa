# IEEE floats with partial comparison

Casa's `f32` and `f64` types are IEEE-754 values and may contain NaN or infinity. Floating-point comparison uses partial comparison semantics: both types satisfy `PartialEq` and `PartialOrd`, but not `Eq`, `Ord`, or `Hashable`.

## Considered options

- Prohibiting NaN would require checking every float-producing operation and foreign-function result, then either trapping or making ordinary arithmetic fallible.
- Giving NaN a language-specific total equality and ordering would permit `Eq` and `Ord`, but would make familiar floating-point operators behave unexpectedly.
- IEEE behavior with partial traits preserves hardware semantics while keeping total-order requirements honest.

## Consequences

- PartialEq provides the shared `eq` and `ne` operator hooks, while Eq is the explicit total-equality refinement. PartialOrd provides `partial_cmp` plus the four boolean ordering hooks; Ord adds `cmp` and total-order semantics.
- `f32` and `f64` cannot be `Map` or `Set` keys directly.
- A validated finite-number wrapper may provide lawful `Eq`, `Ord`, and `Hashable` as a library type.
- Integer-to-float and float-to-integer conversions remain explicit.
- Same-width arithmetic and strict execution behavior follow ADR-0144 and ADR-0145.
