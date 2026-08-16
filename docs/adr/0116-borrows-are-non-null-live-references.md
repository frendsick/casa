# Borrows are non-null live references

Every `$T` and `mut$T` value refers to a valid live `T` for the complete inferred borrow duration. Borrow types have no null value or invalid sentinel.

Absence is represented explicitly by an ordinary enum such as the standard-library `Option`:

```casa
fn find items:$List[T] -> Option[$T]
```

The compiler does not recognize `Option` specially. Any library or application enum may represent an optional borrow under the ordinary aggregate-origin rules.

## Consequences

- Safe use of a borrow requires no null check.
- `ptr` remains the nullable, non-owning raw-address type.
- `ptr::as_ref[T]` and `ptr::as_mut[T]` require a non-null address; violating that unsafe precondition is undefined behavior.
- Borrow fields and returned borrows retain the same non-null guarantee.
- Casa has no `null` literal or null coercion for borrowed types.
