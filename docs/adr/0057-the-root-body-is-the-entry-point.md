# The root body is the entry point
status: amended by [ADR-0165](0165-runtime-state-is-owned-by-the-root-body.md)

The root source file remains directly executable: its top-level operations form the program's implicit entry-point scope. Casa does not require, reserve, or automatically invoke a function named `main`.

```casa
"hello" println
```

Authors may still define and explicitly call `main` as an ordinary function when that organization is useful. Root locals own runtime state and are destroyed when the root body completes. Imported modules contribute declarations and immutable-global initialization under ADR-0055 but do not execute their own top-level program bodies.

This preserves Casa's script-like composition and avoids wrapping every example, test, and small program in ceremonial entry-point syntax.
