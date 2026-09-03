# `global` declares an immutable runtime value
status: superseded by [ADR-0165](0165-runtime-state-is-owned-by-the-root-body.md)

Casa reuses `global` exclusively as an explicit top-level declaration for an exactly-once, runtime-initialized, program-lifetime value. A direct initializer follows the name; a block supplies a more complex stack expression:

```casa
global LIMIT 100

global OPERATORS {
    build_operators
}

pub global PUBLIC_OPERATORS {
    build_public_operators
}
```

The initializer must produce exactly one value. A global name cannot be redeclared, reassigned, or borrowed mutably after initialization. Reads follow ADR-0048: ordinary use borrows the stored value, while an owned context may copy it only when its type is `Copy`.

This changes both previous global forms. Top-level `value = NAME` now creates a local in the root entry-point scope rather than an implicit global. The old in-function `global NAME` declaration is invalid; functions cannot mutate global state. Existing runtime tables migrate to explicit declarations, while application state moves into root locals and explicit parameters.

`const` remains distinct: it is evaluated at compile time and materialized or inlined at each use, whereas `global` names one runtime-initialized value with program lifetime. Reusing the keyword adds no new vocabulary and makes global storage visible at its declaration.
