# Owned bindings may lend mutable borrows
status: amended by [ADR-0165](0165-runtime-state-is-owned-by-the-root-body.md)

Every owned local binding and owned parameter may be reassigned, mutated, or lent as `mut$T`. Casa adds no `mut` binding declaration. Shared `$T` parameters cannot mutate the borrowed value; exclusive `mut$T` parameters can. Borrow exclusivity, rather than a declaration modifier, is the safety boundary.

An owned parameter still receives its value by move, so mutating that local owner does not mutate a caller binding. A caller-visible mutation requires a `mut$T` parameter. Reassigning an owned binding destroys its previous value exactly once after the compiler verifies that no borrow remains live.

## Considered options

- Requiring `mut` on bindings advertises intended mutation, but adds syntax without strengthening ownership or aliasing guarantees.
- Making bindings immutable by default encourages a functional style, but conflicts with Casa's existing assignment-based locals, stateful iterators, and composition through mutable buffers.
- Allowing mutation only through explicit `mut$` variables would require users to create references for routine updates to uniquely owned values.
- Letting any owner lend an exclusive borrow keeps declarations minimal while preserving the meaningful safety rule.

## Consequences

- Methods requiring `mut$self` may auto-borrow an available owned receiver according to the callee-directed borrowing rules.
- An owned field may be mutated through its available owner. Active whole-value or overlapping field borrows prevent the mutation.
- Closure mutation of a captured binding requires the inferred exclusive capture already defined by the ownership model.
- A moved binding is unavailable until control flow proves it has been reinitialized; general mutability does not permit use after move.
- Constants remain immutable. Global mutation and its visibility follow the separate module/global rules rather than this local-binding decision.
- The compiler tracks no additional per-binding mutability flag, so this decision adds no compile-time analysis beyond ownership and borrowing.
