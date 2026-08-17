# Recursive owned types may derive Clone

A finite recursive owned struct or enum may derive Clone:

```casa
enum Node derives Clone {
    End
    Next(i64 Node)
}
```

Generated Clone recursively clones each payload and allocates any compiler-managed recursive indirection required by the destination. Safe affine ownership prevents cycles, so every constructible source value has a finite traversal. Clone remains explicit and may be proportional to the complete recursive structure.

The trait implementation checker treats a recursive derivation dependency as one strongly connected component. It provisionally validates self-recursive Clone requirements, then requires every non-recursive field dependency to satisfy Clone. It must not recursively instantiate the same obligation without termination.

## Consequences

- Compiler-managed indirection does not prevent Clone derivation, although it makes the recursive type non-Copy.
- A deep recursive clone may exhaust call-stack space or allocation capacity; allocation failure terminates under the established model.
- A measured deep-structure workload may justify an iterative handwritten Clone implementation or later compiler optimization.
- Tests must cover direct recursion, mutual recursion, a non-Clone external field, and a long finite chain.
