# Explicit `global` declarations for in-function global writes

Variables assigned inside functions silently target an existing global of the same name instead of creating a local (#171). This happens because `resolve_identifiers_fn` skips local registration when `parser.store.variables` already contains the name — and since resolution is lazy (deferred to first call via `ensure_typechecked`), globals registered by *other files* suppress locals in library code parsed earlier.

We chose to add a `global X` keyword: inside a named function, `= X` / `+= X` / `-= X` always creates or writes a local unless the function declares `global X` at the top of its body. This makes intent explicit and eliminates cross-file leaks.

## Considered options

| Option | Why rejected |
|---|---|
| **Per-file global namespacing** | More principled long-term, but requires tracking file-of-origin per global and per function — a significant internal refactor for a scoping fix. Could be layered on later. |
| **Case-convention enforcement** (SCREAMING_SNAKE = global, lowercase = local) | Codifies the style guide, but couples scoping semantics to naming. Forbids mutable lowercase globals forever. Brittle. |
| **Parse-time local declaration** with explicit write operator (`:=`) | Similar to `global` but introduces a new *operator* instead of a declaration. Adds syntax at every write site rather than one declaration per function. |

## Consequences

- New reserved keyword `global`.
- Migration: every in-function global write (`lib/log.casa`, `lib/timer.casa`, `compiler/lexer.casa`, `compiler/bytecode.casa`) needs a `global X` declaration added.
- Library prefixed names (`iter_value`, `iter_elem`, etc.) can be renamed back to plain names (`value`, `elem`) since locals no longer leak.
- Lambdas cannot use `global` — only named functions.
- `global X` is a compile error if X is not an existing top-level variable.
