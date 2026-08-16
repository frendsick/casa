# Stack intrinsics respect ownership

Stack manipulation operates on typed values, not untracked machine words. `dup` requires its top value to be `Copy`; `over` requires the deeper value it copies to be `Copy`. ADR-0136 adds `copy` with `[T: Copy] $T -> T` for materializing an owned Copy value through a borrow. `swap` and `rot` only reorder values and accept non-`Copy` owners without cloning them. `drop` consumes its top value and runs the same deterministic destruction used at scope exit.

Dropping a shared or exclusive borrow ends that loan without destroying its referent. Shared `$T` is `Copy`, so it may be duplicated; exclusive `mut$T` is not. Reordering an owner is valid only when it does not move a borrowed referent or otherwise invalidate a live loan. Lowering must preserve the referent's stable storage or reject that reorder.

## Considered options

- Restricting every stack operation to `Copy` values is simple, but prevents useful ownership-preserving reordering of resources.
- Letting `dup` and `over` copy any machine word preserves current behavior, but creates multiple apparent owners of one allocation.
- Making duplication invoke type-specific deep-copy code hides allocations and user behavior behind fundamental stack operations.
- Distinguishing duplication from reordering follows the semantic operation each intrinsic actually performs.

## Consequences

- The stack effects of `dup` and `over` carry a compiler-known `Copy` constraint on the value being duplicated. Declared generic wrappers expose that bound explicitly.
- `copy` carries the same bound on its referent and never invokes Clone.
- `swap` and `rot` transfer ownership between stack positions without calling `drop` or other custom code.
- `drop` on an owner may run custom cleanup followed by field destruction; it is no longer always a single stack-pointer adjustment.
- An owner cannot be dropped or physically relocated while a borrow requiring its current storage remains live.
- Compiler-synthesized stack shuffles obey the same rules as source-written intrinsics.
- Existing raw-word lowering and documentation examples using `str dup` must migrate because owned `str` is non-`Copy`.
