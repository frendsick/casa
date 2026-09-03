# Immutable globals initialize eagerly once
status: superseded by [ADR-0165](0165-runtime-state-is-owned-by-the-root-body.md)

Each immutable global has exactly one successful initializer. Modules initialize eagerly once, dependency modules before their importers and globals within a module in source order. Reading a not-yet-initialized global or forming an initialization cycle is a compile-time error.

Complex values are built locally and then moved into their global exactly once:

```casa
fn build_operators -> Map[str OperatorKind] {
    Map[str OperatorKind]::new = operators
    OperatorKind::Plus "+" operators.set
    OperatorKind::Minus "-" operators.set
    operators
}

global OPERATORS {
    build_operators
}
```

Every runtime global uses the explicit `global NAME` declaration of ADR-0058, so a top-level `build_operators = OPERATORS` creates a root local instead. Casa has no repeated global assignment, lazy-global guards, or user-visible partially initialized state. A selectively imported declaration automatically carries the private immutable globals in its dependency closure and initializes them without exposing their names.

Eager source order reuses ordinary module loading and avoids synchronization or hidden first-use branches. Existing top-level tables assembled through repeated assignment migrate to builder functions.
