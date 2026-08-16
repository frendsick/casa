# Ordering is a standard enum

Three-way comparison uses an ordinary standard-library enum:

```casa
enum Ordering {
    Less
    Equal
    Greater
}
```

The enum is not required for primitive comparison operators, which remain intrinsic. The compiler validates the three variants and their order only when compiler-generated Ord behavior must construct an Ordering value. A freestanding library using `derives Ord` may supply the same contract without importing the standard library.

Option remains entirely ordinary. PartialOrd's `Option[Ordering]` return and Ord's wrapping default are expressed in library source rather than synthesized as a compiler case.

## Consequences

- `cmp` returns Ordering and `partial_cmp` returns `Option[Ordering]`.
- Ordering may receive ordinary methods and trait conformances without changing its three language-relevant variants.
- Programs using only primitive boolean comparison need neither Ordering nor Option declarations.
- A malformed or differently ordered enum cannot serve as the Ordering contract used by `derives Ord`.
- ADR-0089 gives the standard enum Eq and Copy conformance initially; Ord and Hashable remain deferred.
