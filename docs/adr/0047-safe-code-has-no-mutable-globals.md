# Safe code has no mutable globals
status: amended by [ADR-0165](0165-runtime-state-is-owned-by-the-root-body.md)

Top-level global bindings may be constructed during module initialization, but safe code cannot reassign them or borrow them mutably afterward. The in-function `global NAME` mutation declaration is removed; ADR-0058 later reuses `global` exclusively for explicit immutable top-level declarations. Runtime state is owned by an entry point and passed to functions as an owner, shared borrow, or exclusive borrow. Immutable top-level values and `const` declarations remain available.

This supersedes ADR-0001. Casa's affine ownership model cannot safely grant an unscoped mutable loan to globally aliased state, and explicit state makes dependencies, tests, and future concurrency easier to reason about. No unsafe mutable-global abstraction is exposed initially; one may be added later only for a demonstrated systems-programming need.

## Consequences

- The current production code has four `global` declarations across `lsp.casa`, `lib/log.casa`, and `lib/timer.casa`; these become explicit parameters or locally owned state.
- Existing read-only lookup tables and type values remain globals. Their construction finishes before ordinary function execution, after which safe code observes them only through shared access.
- Test helpers return or accept their `SymbolStore` instead of publishing the last store through mutable test globals.
