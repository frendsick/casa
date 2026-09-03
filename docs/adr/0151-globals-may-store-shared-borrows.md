# Globals may store shared borrows
status: superseded by [ADR-0165](0165-runtime-state-is-owned-by-the-root-body.md)

A global initializer may produce a shared borrow, so a global can name a place inside another global:

```casa
global CONFIG { load_config }

global DEFAULT_KIND { CONFIG.default_kind }
```

It may also settle a choice between globals once at initialization instead of at every call:

```casa
global ASCII_TABLE { build_ascii_table }
global UNICODE_TABLE { build_unicode_table }

global ACTIVE_TABLE {
    if unicode_enabled then
        UNICODE_TABLE
    else
        ASCII_TABLE
    fi
}
```

Naming a complete global, as in `global VIEW { OPERATORS }`, is permitted but pointless, because ordinary use of `OPERATORS` already produces that same borrow.

Reading a global keeps the ADR-0048 rule unchanged: ordinary observation borrows the stored value, and an owned context materializes a value only when the stored type is `Copy`. A borrow of a borrow is that same borrow, so observing `DEFAULT_KIND` produces `$OperatorKind` rather than a nested borrow type, and the `Copy` test never applies to a borrow-typed global.

An exclusive borrow cannot be stored. `mut$T` is affine, and ADR-0047 leaves safe code no mutable global state to lend.

This needs no new machinery. ADR-0048 already gives every global storage program lifetime, so a stored borrow's origin outlives every use of it. A projection borrows one field under ADR-0105 rather than the complete parent. Initialization order, forward-reference rejection, and cycle rejection follow ADR-0053 unchanged, because an initializer that reads another global depends on it like any other read.

Rejecting borrow-typed globals would cost a type condition on ADR-0058's initializer rule and its diagnostic, while allowing them costs the one borrow-collapse sentence that projections need in any case.

## Consequences

- A global's type may be `T` or `$T`. The declaration does not show which; an optional type annotation for globals remains a separate decision.
- Borrow-typed globals are an alias, not a copy. They duplicate no data and keep the origin of the global they name.
- A public borrow-typed global exposes exactly what a public function returning `$T` already exposes under ADR-0056.
- A global holding an aggregate with borrowed fields was already permitted; it stores an owner whose origins propagate under ADR-0052.
