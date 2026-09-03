# Named functions do not capture root locals
status: amended by [ADR-0165](0165-runtime-state-is-owned-by-the-root-body.md)

Named functions cannot capture local bindings from the executable root body. They may read constants and immutable globals; runtime state owned by the root is passed through ordinary owned, shared, or exclusive parameters.

```casa
fn tick state:mut$State {
    state.update
}

state tick
```

Lexical closures in the root body retain the ordinary capture rules. Keeping capture exclusive to closures prevents root locals from becoming hidden mutable globals and leaves every named function's runtime dependencies visible in its declaration.
