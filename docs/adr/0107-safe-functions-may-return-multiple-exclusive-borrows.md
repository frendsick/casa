# Safe functions may return multiple exclusive borrows

A safe function may return multiple exclusive borrows when its body proves that their places do not overlap:

```casa
struct Pair {
    left: Buffer
    right: Buffer
}

fn split pair:mut$Pair -> mut$Buffer mut$Buffer {
    pair.left
    pair.right
}
```

The compiler verifies the disjoint named fields while checking `split`. Callers may then rely on the two returned `mut$Buffer` values being non-aliasing without repeating that proof.

## Consequences

- Every simultaneously live exclusive borrow remains non-aliasing, including borrows returned from a call.
- A safe function cannot return duplicate or overlapping exclusive projections.
- Returning multiple borrows requires no tuple type or special split syntax because Casa stack effects already support multiple outputs.
- Collection operations such as `split_at_mut` may provide the same safe contract after validating runtime boundaries with unsafe internals.
- An unsafe foreign or raw-memory implementation is responsible for upholding the declared exclusive-borrow contract.
