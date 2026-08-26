# Cloning a shared borrow clones the borrow
status: superseded by [ADR-0150](0150-shared-borrow-duplication-is-not-copy-conformance.md)
related issue: #314

Under Casa's standard `Copy: Clone` declaration, calling `.clone` on a value whose type is `$T` returns another `$T`. Clone always returns the same `Self` type as its selected implementation, and shared borrows are `Copy` values satisfying that ordinary supertrait relationship:

```casa
view.clone # $T -> $T
```

Cloning the borrowed value into a new owned `T` explicitly selects `T`'s Clone method:

```casa
view str::clone # $str -> str
```

## Consequences

- `view dup` and `view.clone` both duplicate a shared borrow; neither clones the borrowed value.
- Every copied or cloned shared borrow keeps the source owner loaned until all copies reach their last use.
- `Type::clone` makes ownership-producing borrowed-value duplication visible and may allocate or run user code.
- Method lookup does not use an expected return type to choose between borrow Clone and borrowed-value Clone.
- Generic Clone continues to mean `Self -> Self` for owned values and non-owning values alike.
- A freestanding Copy declaration that does not extend Clone supplies no Clone method for `$T`; ordinary receiver lookup may then reach `T.clone` when `T` implements Clone.
