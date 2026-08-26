# Shared-borrow duplication is not Copy conformance
related issue: #422

A shared borrow `$T` may be duplicated with `dup` and as the copied operand of
`over`. This duplicates the borrow and preserves its loan origin. It does not
make `$T` implement or satisfy `Copy`, even when `T` implements `Copy`.

Shared borrows therefore do not gain `Clone` through the standard
`Copy: Clone` declaration. When `T` implements `Clone`, `.clone` on `$T` calls
`T`'s implementation and returns an owned `T`. The same call on `mut$T` uses a
temporary shared reborrow and also returns an owned `T`.

Copy eligibility for a non-borrow aggregate remains representation-based. A
stored shared-borrow field is safe to duplicate even though its type does not
satisfy a `Copy` bound. Copy-generated Clone duplicates such a field as a
borrow, preserves its origin, and calls `.clone` for other fields and payloads.

This supersedes ADR-0118 and the borrow Clone consequences of ADR-0120. It also
refines earlier statements in ADR-0013, ADR-0033, ADR-0035, ADR-0077, and
ADR-0079 that described shared-borrow duplication as a Copy implementation.

## Consequences

- Generic `[T: Copy]` and `[T: Clone]` bounds reject `$U` and `mut$U`.
- `dup` and `over` accept `$T` without a `Copy` implementation and keep every
  duplicated borrow tied to the same owner.
- `mut$T` remains affine and cannot be duplicated.
- `.clone` is the normal way to clone a borrowed value into a new owner.
- Casa does not add `clone_ref`. `dup` and `over` already duplicate shared
  references.
