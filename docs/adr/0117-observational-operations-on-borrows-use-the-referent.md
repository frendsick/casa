# Observational operations on borrows use the referent

Equality, ordering, hashing, and display applied to `$T` or a shared reborrow of `mut$T` use `T`'s corresponding trait implementation. They observe the referent rather than the borrow address:

```casa
fn same left:$Point right:$Point -> bool {
    left right ==
}
```

The comparison above invokes `Point`'s equality hooks exactly as comparison of two owned `Point` values does through automatic borrowing. This follows the ordinary `$self` receiver-capability rule in ADR-0120; equality, ordering, hashing, and display receive no separate borrow-specific method handling.

## Consequences

- Borrowed and owned views of the same value have consistent equality, ordering, hashing, and display behavior.
- Borrow types do not acquire separate user-overridable observational conformances merely because they are borrows; ordinary method lookup reaches the referent type.
- `mut$T` weakens to `$T` for observation and is not mutated by these operations.
- Ordinary safe code has no borrow-identity comparison.
- Equality on existing raw `ptr` values compares addresses and remains distinct from referent equality.
