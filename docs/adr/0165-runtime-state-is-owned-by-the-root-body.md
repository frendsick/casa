# Runtime state is owned by the root body
related issue: #662

Casa removes immutable runtime `global` declarations. Compile-time values remain
`const`. Runtime state is constructed by ordinary root-body execution, held by
ordinary owners, and passed to named functions through owned, shared, or
exclusive parameters. Root closures may capture it. Libraries expose
constructors or loaders instead of public runtime globals. This keeps effects,
failure, ownership, and cleanup in visible ordinary control flow.

Imports contribute declarations and never execute module code. There is no
module initializer schedule, hidden exactly-once guard, initializer dependency
graph, forward-global reference, or program-lifetime storage category. Root
bindings initialize in execution order when control reaches them. Initialization
may perform any operation allowed in ordinary root code.

Root-owned state follows the normal affine contract. It moves and borrows like
any other owner and is destroyed in LIFO order when the root scope completes.
Process termination still does not unwind. Named functions do not capture root
locals, so their state dependencies remain visible in their parameters.

## Consequences

- Literal globals migrate to `const` when their initializer is valid under the
  active constant contract. Runtime-built and borrow-valued globals migrate to
  root bindings, optional application context structs, and explicit parameters.
  Public globals become constructors or loaders.
- The removed `global` keyword gets a focused diagnostic that suggests `const`
  or root-owned state. It is not silently reinterpreted.
- Tests keep constant behavior, root execution order, explicit state passing,
  root cleanup, import non-execution, public constructors, and the removal
  diagnostic. Global-initializer scheduling and dependency cycles,
  selected-initializer closure, initializer alias deduplication, global origins,
  and immortal storage tests are removed.
- The language reference, examples, and tests change with the compiler
  migration. Until then, they continue to describe and test implemented global
  behavior.
