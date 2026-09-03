# Shared semantics below parsing and typechecking
status: amended by [ADR-0165](0165-runtime-state-is-owned-by-the-root-body.md)

Selective-import dependency discovery and typechecking both need the typed meaning of resolved operations. We will place that meaning in a phase-independent `compiler/semantics.casa` module below both callers, return data-only facts, and keep import closure policy and typechecking policy in their current phases.

## Current boundaries and ordering

Compiler analysis currently runs in this order:

1. Lex the root source.
2. Enter the **Parse-and-resolve boundary** with tokens, diagnostics, source text, and library paths.
3. At each selective import, lex and parse-and-resolve the imported file against a cloned base store, compute the complete ordered selective-import closure, validate every declaration against the destination, and only then merge the closure. Top-level imported operations are not merged.
4. Finish expansion and identifier resolution for the root source. Any failed import makes the parse-and-resolve output unusable.
5. Typecheck the returned operations and symbols. The **Typecheck result** contains the updated store, checked global **Stack effect**, resolved operations, and diagnostics.
6. Compile bytecode only from an error-free **Typecheck result**.

Selective imports must remain inside step 3: their declarations are needed to resolve the rest of the importing source. They therefore cannot consume a completed **Typecheck result**. The closure and conflict checks from #272 and #273 also remain all-or-nothing; semantic analysis must not mutate the importing store before the existing transactional merge succeeds.

## Decision

Add one deep semantics module whose interface accepts resolved operations, optional function context, and a read-only `SymbolStore`. It returns semantic facts containing:

- canonical resolved operations, including comparison, printing, f-string, method, and trait dispatch rewrites;
- the inferred **Stack effect**;
- ordered semantic dependencies for functions, function references, types, traits, concrete implementations, default methods, and globals; and
- operation-validation diagnostics.

The module owns the transient typed stack, branch and scope state, generic bindings, callable effects, and dispatch rules. None of that mutable builder state is part of its interface. The deletion test is that changing an operation's meaning changes this module once, not a typechecker handler and a selective-import simulator.

Phase ownership remains explicit:

- Parsing owns syntax, import loading, import encounter order, and identifier resolution.
- Selective-import discovery owns requested roots, visited and cycle handling, closure order, forbidden-global diagnostics, declaration conflicts, and transactional merge.
- Semantics owns typed stack transitions, inference, callable application, dispatch selection, resolved operation forms, and the dependencies those decisions expose.
- Typechecking owns function scheduling, committing resolved operations and inferred effects, publishing operation-validation diagnostics, and constructing the **Typecheck result**.

Selective-import discovery consumes ordered dependency facts but does not publish typecheck-only diagnostics early. Typechecking consumes the same semantic result and preserves the current diagnostic and mutation order. Neither caller receives a `Parser`, `TypeChecker`, stack, branch frame, or mutable callback interface.

## Considered options

| Seam | Depth | Locality | Leverage | Test surface | Decision |
|---|---|---|---|---|---|
| Phase-independent semantics below both callers | One data interface hides stack, inference, control flow, and dispatch | Operation meaning changes in one module; phase policy stays with each caller | Both current callers reuse the same facts | One semantic interface plus boundary tests for typechecking and selective imports | Chosen |
| TypeChecker as the selective-import query seam | Deep for typechecking, but its interface includes typecheck lifecycle and mutation rules that closure discovery does not need | Centralizes meaning, but couples import expansion to later-phase state | Reuses current handlers | Requires tests for a second TypeChecker mode and mutation/diagnostic suppression | Rejected: `syntax` already imports selective-import code while typechecking imports `syntax`, so this points the dependency upward or creates a cycle; it also risks changing phase ordering and store mutation |
| Parser annotates operations with all semantic dependencies | Shallow because later inference and dispatch still need another interpreter | Spreads type knowledge into parsing and duplicates late semantic decisions | Parsing and closure reuse annotations, but typechecking cannot rely on incomplete early facts | Parser annotation tests plus the existing typechecker tests | Rejected: receiver types, inferred **Stack effects**, trait dispatch, branches, and patterns are not all known during parsing, and parser-owned mutable state would cross its boundary |

## Incremental migration

Issue #301 introduces the result and dependency-fact vocabulary, then moves fixed operation effects, callable effects, comparison and printing dispatch, memory operations, syscalls, methods, trait bounds, and default methods behind the shared seam. The existing selective-import stack may remain only as a fallback for operation families not migrated in that slice; migrated families must not retain parallel rules.

Issue #302 moves control flow, assignments, variables, arrays, structs, enums, and patterns to the same semantic implementation, switches closure discovery to the returned ordered facts, and deletes the selective-import-only stack, snapshots, branches, bindings, and operation dispatcher.

Tests should concentrate operation meaning at the new interface. Typechecker tests retain diagnostic and **Typecheck result** behavior; selective-import tests retain closure order, global rejection, cycles, exact dependencies, and transactional failure. Mirrored per-operation simulation tests should disappear with the simulator.
