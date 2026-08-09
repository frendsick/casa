# Control-flow joins require definite availability

An owned binding is usable at a control-flow join only when every continuing predecessor proves that the binding still owns a value. Moving from a binding makes it unavailable; assigning a new value makes it available again. A path that returns or otherwise cannot reach the join does not participate.

The initial analysis tracks complete bindings rather than independently movable fields. This matches Casa's decision to reject partial moves while retaining the important safety property: no path reaching a use may have already transferred or destroyed its owner.

## Considered options

- Rejecting every conditional move would simplify analysis, but needlessly forbids branches that terminate or reinitialize the binding.
- Trusting the branch selected at runtime would require unchecked use or unconditional runtime ownership flags.
- Rust-style definite initialization at field granularity is expressive, but partial moves add state and diagnostics Casa has already chosen to defer.
- Whole-binding definite availability provides the required safety with a small finite-state dataflow analysis.

## Consequences

- After `if`, `match`, and similar joins, a binding is available only if every incoming continuing path owns or reinitialized it.
- Returning and other non-continuing paths are excluded from the join.
- Every loop back-edge and `continue` must restore the ownership state required at the loop header. Loop exits merge the zero-iteration path with every reachable `break` and normal exit.
- A loan that may still be live on any incoming path is treated as live after the join; function-local last-use analysis may end it before then.
- The compiler destroys each value exactly once. It may emit a hidden drop flag only when conditional initialization makes the value's existence runtime-dependent.
- The implementation tracks a small state per binding and must be checked against the compiler self-compilation benchmark so ownership safety does not introduce an unexpectedly large compile-time cost.
