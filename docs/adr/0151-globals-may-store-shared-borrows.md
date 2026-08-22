# Globals may store shared borrows

A global initializer may produce a shared borrow, so one global can name another global's storage:

```casa
global OPERATORS { build_operators }

global OPERATOR_VIEW { OPERATORS }
```

Reading a global keeps the ADR-0048 rule unchanged: ordinary observation borrows the stored value, and an owned context materializes a value only when the stored type is `Copy`. A borrow of a borrow is that same borrow, so observing `OPERATOR_VIEW` produces `$Map[str OperatorKind]` rather than a nested borrow type, and the `Copy` test never applies to a borrow-typed global.

An exclusive borrow cannot be stored. `mut$T` is affine, and ADR-0047 leaves safe code no mutable global state to lend.

This needs no new machinery. ADR-0048 already gives every global storage program lifetime, so a stored borrow's origin outlives every use of it. Initialization order, forward-reference rejection, and cycle rejection follow ADR-0053 unchanged, because an initializer that reads another global depends on it like any other read.

## Consequences

- A global's type may be `T` or `$T`. The declaration does not show which; an optional type annotation for globals remains a separate decision.
- Borrow-typed globals are an alias, not a copy. They duplicate no data and keep the origin of the global they name.
- A public borrow-typed global exposes exactly what a public function returning `$T` already exposes under ADR-0056.
- A global holding an aggregate with borrowed fields was already permitted; it stores an owner whose origins propagate under ADR-0052.
