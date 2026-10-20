# Standard library provides a shared immutable owner
related issue: #844

The standard library provides `Shared[T]`, an explicit reference-counted owner
of one immutable value. `Shared::new` moves the value into a heap allocation.
`clone` adds a handle to the same value, `get` borrows it, and the last handle
destroys it.

The compiler previously implemented reference counting three times: a private
`Shared[T]`, the file path of each `Location`, and `GenericType` trees. Each
used `ptr::into_raw`, manual count updates, and `ptr::from_raw` behind safety
comments to share immutable values that compiler passes clone frequently. User
programs had no safe equivalent. This compiler use is the concrete program
required by [#367](https://github.com/frendsick/casa/issues/367).

[ADR-0013](0013-affine-ownership-with-automatic-storage.md) rejects automatic
reference counting, which adds hidden retain and release work to ordinary
values. `Shared[T]` is a library type that a program selects explicitly. Counts
change only when a program clones or destroys a handle, and other values keep
affine ownership.

A reference cycle needs a value that refers to its own handle. The value is
complete before `Shared::new` moves it, and
[ADR-0110](0110-shared-borrows-do-not-permit-safe-interior-mutation.md) forbids
mutation through the borrow that `get` returns.
[ADR-0115](0115-safe-code-cannot-construct-self-referential-owners.md) also
forbids self-referential owners. Safe code therefore cannot form a cycle, and
the last handle always reclaims the value.

Equality, ordering, hashing, and display use the shared value as in
[ADR-0117](0117-observational-operations-on-borrows-use-the-borrowed-value.md).
Each operator method calls the matching method of `T`, so the overrides of `T`
also apply to its handles. `Clone` does not require `T: Clone` because cloning
a handle shares the value. `Shared[T]` exposes no handle identity comparison.

## Considered options

- Keeping private compiler implementations repeats the unsafe count logic and leaves user programs without a safe way to share values.
- One owner plus IDs or indices suits graphs and mutable shared state. Type trees and locations cloned across compiler passes would need a long-lived arena and index plumbing.
- Mutable access through a handle or weak references has no current use. It would need an exception to ADR-0110 and would make cycles possible.

## Consequences

- Reference counts are not atomic because Casa programs run on one thread.
- `Shared[T]` stores its value in raw storage, like `List` and `Map`. It does not retain the borrow origins of a `T` that contains borrows, so such values are unsupported until origin tracking covers raw-storage owners.
- Conditional `Eq`, `Ord`, and `Hashable` implementations for `Shared[T]` reuse its separate `PartialEq` and `PartialOrd` implementations under [ADR-0098](0098-subtrait-implementation-satisfies-supertraits.md).
- Moving a value out of its last handle and weak handles remain deferred until a concrete program needs them.
- Multiply referenced immutable values may use `Shared[T]`. Cyclic or mutable graphs still use one owner plus IDs, indices, or checked non-owning references.
