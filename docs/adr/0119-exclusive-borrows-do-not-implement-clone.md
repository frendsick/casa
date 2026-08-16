# Exclusive borrows do not implement Clone

`mut$T` does not automatically implement `Clone` and cannot be duplicated with `dup` or `over`:

```casa
exclusive dup   # error
```

Duplicating an exclusive borrow would create simultaneous mutable aliases and violate its defining invariant. An exclusive borrow remains affine and may only be moved between bindings, arguments, and returns.

When `T` implements Clone, ordinary receiver lookup may call that referent method through a shared reborrow and return an owned `T`:

```casa
exclusive.clone         # mut$Buffer -> Buffer
exclusive Buffer::clone # mut$Buffer -> Buffer
```

Both calls weaken `exclusive` to a temporary shared reborrow. The exclusive borrow becomes usable again after that call ends. ADR-0120 supersedes the earlier decision to reject unqualified `exclusive.clone`; no Clone-specific lookup exception is needed.

## Consequences

- Standard `Copy: Clone` does not apply to `mut$T` because exclusive borrows are not Copy.
- Method lookup may reach any applicable `$self` method on `T`, including Clone, under the ordinary receiver-capability rule.
- Aggregates containing an exclusive borrow cannot derive Clone.
- Moving an exclusive borrow transfers the one capability and leaves the source unavailable.
