# Recursive types may derive comparison and hashing

Finite recursive owned structs and enums may derive Eq, Ord, and Hashable as well as Clone:

```casa
enum Tree derives Eq Ord Hashable Clone {
    Leaf(i64)
    Branch(Tree Tree)
}
```

Generated equality, ordering, and hashing recursively visit payloads using their ordinary field or variant order. Safe affine ownership prevents owned cycles, so each operation traverses a finite value. Compiler-managed recursive indirection remains an ownership detail and makes the type non-Copy.

The conformance checker resolves mutually recursive derivations as strongly connected components. Self-recursive obligations are provisionally accepted within the component, while every external field dependency must satisfy the requested capability.

## Consequences

- Recursive equality and ordering remain subject to the same semantic laws and explicit-customization rules as non-recursive derivation.
- Runtime work and call depth may be proportional to the complete recursive structure.
- Tests must cover direct and mutual recursion, unequal deep leaves, ordering across variants and payloads, stable equal hashes, and an external field lacking the requested capability.
- Iterative generation remains a later optimization if deep-structure benchmarks justify it.
