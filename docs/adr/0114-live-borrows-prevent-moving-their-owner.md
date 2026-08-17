# Live borrows prevent moving their owner

An owner may not be moved, replaced, or destroyed while any borrow derived from it remains live:

```casa
owner.field = view
owner take          # error
view.inspect
```

The owner becomes movable again after the last derived borrow expires:

```casa
owner.field = view
view.inspect
owner take
```

This is a source-level ownership rule independent of whether the current representation happens to use stable heap storage.

## Consequences

- Consuming calls, assignment to another binding, return, explicit destruction, and replacement are moves or invalidations subject to this rule.
- Reordering an owner with stack intrinsics while its value is borrowed is rejected.
- Field mutation remains possible only when it does not overlap the live loan under the field-sensitive rules.
- The compiler need not pin borrowed owners, allocate them implicitly, or transfer loan identity between owner bindings.
- Internal lowering may relocate representations only when it preserves all observable addresses and borrow guarantees; that does not relax source semantics.
