# Immutable globals are borrowed places
status: superseded by [ADR-0165](0165-runtime-state-is-owned-by-the-root-body.md)

An immutable global denotes a program-lifetime storage place rather than a reusable owned value. Ordinary observation produces `$T`; a function may return that borrow without a borrowed input because the compiler knows its global origin remains live for the complete program.

When a consumer specifically requires owned `T`, the compiler may read a global into a value only when `T: Copy`. This is the same callee-directed convenience as automatic borrowing: passing the global to `$T` borrows it, while passing it to a consuming `T` parameter copies only if the type permits copying. A non-`Copy` global never moves, clones, or manufactures an owner implicitly.

```casa
fn keywords -> $Map[str Keyword] {
    KEYWORDS
}

fn add_limit value:i64 -> i64 {
    LIMIT value + # LIMIT is copied because arithmetic requires i64
}
```

## Consequences

- Casa adds no explicit global-dereference syntax and does not change arithmetic to operate on borrowed integers.
- A use with no owned-value requirement remains a shared borrow, including `dup`, observation, and a declared `$T` return.
- Existing non-`Copy` compiler globals used as owned templates become constructors or explicit owned values; they receive no special static-owner or implicit-clone rule.
- Immutable globals live until process termination and are not destroyed at shutdown. State or resources requiring deterministic cleanup belong to an entry-point owner.
- Resolving a global use requires only local expected-type checking plus the existing `Copy` test; it introduces no lifetime solver or whole-program analysis.
