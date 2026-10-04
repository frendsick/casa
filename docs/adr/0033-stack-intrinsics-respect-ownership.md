# Stack intrinsics respect ownership
status: amended by [ADR-0150](0150-shared-borrow-duplication-is-not-copy-conformance.md)
related issue: #314

Stack manipulation operates on typed values, not untracked machine words. `dup` and `over` require Copy for owned values they duplicate. Shared borrows duplicate under ADR-0150 without making `$T` conform to Copy. ADR-0136 adds `copy` with `[T: Copy] $T -> T` for materializing an owned Copy value through a borrow. `swap` and `rot` only reorder values and accept non-`Copy` owners without cloning them. `drop` consumes its top value and runs the same deterministic destruction used at scope exit.

Dropping a shared or exclusive borrow ends that loan without destroying the borrowed value. Shared `$T` may be duplicated. Exclusive `mut$T` may not. Reordering an owner is valid only when it does not move a borrowed value or otherwise invalidate a live loan. Lowering must preserve stable storage for the borrowed value or reject that reorder.

## Considered options

- Restricting every stack operation to `Copy` values is simple, but prevents useful ownership-preserving reordering of resources.
- Letting `dup` and `over` copy any machine word preserves current behavior, but creates multiple apparent owners of one allocation.
- Making duplication invoke type-specific deep-copy code hides allocations and user behavior behind fundamental stack operations.

## Consequences

- Declared generic wrappers that duplicate an owned value expose `[T: Copy]`. A shared borrow does not satisfy that bound.
- `copy` carries the same bound on its borrowed value and never invokes Clone.
- `swap` and `rot` transfer ownership between stack positions without calling `drop` or other custom code.
- `drop` on an owner may run custom cleanup followed by field destruction. It is no longer always a single stack-pointer adjustment.
- An owner cannot be dropped or physically relocated while a borrow requiring its current storage remains live.
- Compiler-synthesized stack shuffles obey the same rules as source-written intrinsics.
- A `str` view is `Copy`, so stack intrinsics may duplicate its view. Owned `String` values remain non-`Copy` and use explicit `clone` when duplication is required.
