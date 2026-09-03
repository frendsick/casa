# Semantic-analysis complexity audit

Status: complete audit for issue
[#640](https://github.com/frendsick/casa/issues/640).

Source revision:
[`bb6ffa7`](https://github.com/frendsick/casa/commit/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1),
2026-09-02.

Run date: 2026-09-03.

## Finding

Casa's language rules require substantial semantic analysis. Typed stacks,
inferred stack effects, affine ownership, static trait dispatch, generic
specialization, and control-flow joins are inherent complexity. The main
accidental complexity is how the implementation represents those rules.

One mutable operation tree and one mutable `SymbolStore` represent unresolved,
checked, and specialized programs. Phase state is carried by optional fields,
booleans, side tables, synthetic functions, and operation variants. The
implementation must clone and synchronize that state while several recursive
walkers interpret the same operation for dependencies, stack effects, type
changes, ownership, dispatch, and specialization.

Later architecture work should first deepen the operation-semantics seam. It
should define a small phase result that owns checked operations, inferred stack
effects, semantic dependencies, ownership facts, and diagnostics. This is a
required responsibility boundary, not a choice of target architecture. A
later design can decide whether that result uses a new operation type, tagged
phase state, or another representation.

After accounting for overlap, the combined opportunity is about 700-1,200
production lines and 400-700 test lines. Closing visibility can also remove up
to 252 names from the declared `semantics.casa` interface without removing
source lines. No dependency deletion is available in this scope.

## Audit boundary

The audit follows semantic data from parse and resolution through typechecking
and into bytecode generation. It covers:

- `compiler/semantics.casa`, `compiler/semantic_rules.casa`, and
  `compiler/typechecker.casa`.
- The shared operation and symbol types in `compiler/common.casa`.
- Semantic callers in `compiler/analysis.casa`, `compiler/syntax.casa`,
  `compiler/selective_import.casa`, and `compiler/bytecode.casa`.
- Focused semantic, trait, ownership, and typechecker tests.
- The language decisions that make semantic work necessary.

The baseline is `origin/main` at `bb6ffa7`. It excludes unmerged work on
`feat/592-os-byte-round-trips`, `refactor/547-private-bytecode-pass`, and the
parallel audits for issues #639, #641, and #642. None of those branches had a
change that invalidated this audit's semantic baseline when work started.

## Measured baseline

| Source | Lines | Functions | Declared public functions |
|---|---:|---:|---:|
| `compiler/semantics.casa` | 12,689 | 384 | 273 |
| `compiler/semantic_rules.casa` | 1,942 | 63 | 63 |
| `compiler/typechecker.casa` | 159 | 11 | 9 |
| All compiler sources | 40,192 | | |

The principal mutable records have these dimensions:

| Record | Size indicator | Role |
|---|---:|---|
| `TypeChecker` | 41 fields | Active stacks, branch frames, loans, substitutions, callable bindings, dependencies, diagnostics, and store access |
| `TypedValue` | 12 fields | Type, literal, place, origin, loan, cleanup, binding, and callable state |
| `SymbolStore` | 21 fields | Declarations, generated functions, operation ownership, inferred effects, and phase indexes |
| `OpValue` | 128 variants | Source operations, resolved calls, lowered forms, deferred forms, and control markers |
| `SemanticDependency` | 6 variants | Selective-import dependency closure |

Public declarations overstate the usable module interface. Only 24 distinct
`semantics::` member names have direct cross-file references, including focused
tests. Three are types and 21 are functions. This leaves 252 declared public
functions without a direct cross-file reference. `semantic_rules.casa` has 38
distinct externally referenced member names for 63 public functions. Many
public `TypeChecker` methods cannot be called through the private type. The
source still presents them as interface knowledge to a reader and to tools.

Six focused test files contain 5,040 lines and 287 tests:

| Tests | Lines | Test count |
|---|---:|---:|
| Semantic values and operations | 2,514 | 127 |
| Typechecker boundary | 224 | 10 |
| Traits | 1,516 | 102 |
| Closure and owned contexts | 786 | 48 |

Twenty-eight compiler test files directly import at least one audited module.
This count includes tests that use a stable typechecker boundary and does not
mean that all 28 files depend on internals.

### Self-compilation reference

The installed stable Casa v1.50.0 compiler compiled `casa.casa` three times
with `./casac -L lib casa.casa`. GNU `time` measured wall time and peak RSS.
The environment was Linux 6.18.33.2 under WSL2 on an AMD Ryzen 7 3700X with 16
logical CPUs. There was no separate warm-up.

| Run | Wall time | Peak RSS |
|---:|---:|---:|
| 1 | 32.11 s | 760,564 KiB |
| 2 | 30.64 s | 760,436 KiB |
| 3 | 30.60 s | 760,308 KiB |
| Median | 30.64 s | 760,436 KiB |

The output executable was 4,777,120 bytes. This is a full-compiler reference,
not a measurement of semantic analysis alone. It gives the parent architecture
work a reproducible baseline, but it cannot assign time or memory to a finding
in this document.

## Current responsibility and dependency flow

`analysis::analyze` calls lexing, `syntax::parse_and_resolve`, and then
`typechecker::type_check`. The typechecker creates a semantic session, checks
root operations, schedules reachable functions, commits the semantic store,
validates trait implementations and generic cycles, and monomorphizes checked
generics. It returns a `TypecheckResult` consumed by bytecode generation.

Selective imports use the semantic engine earlier. Syntax resolution creates
an isolated `SemanticSession` and analyzes selected function bodies and global
initializers to discover dependencies and inferred effects before the complete
program reaches `type_check`.

The effective dependency direction is:

```text
analysis -> syntax -> selective_import -> semantics -> semantic_rules
    |          |               |              |
    +----------+---------------+----------> common
    |
    +-> typechecker -> semantics -> semantic_rules
             |
             +-> TypecheckResult -> bytecode -> semantic_rules
```

`semantics.casa` is not one cohesive module. Its 12,689 lines contain these
responsibilities:

| Approximate lines | Responsibility |
|---:|---|
| 1-989 | Typed values, encoded origins, callable metadata, and place helpers |
| 990-1,233 | If, loop, and match flow state |
| 1,234-1,767 | Typechecker state and numeric literal resolution |
| 1,768-3,117 | Stack transitions, ownership, loans, and cleanup |
| 3,118-4,783 | Compatibility, inference, calls, and borrow flow |
| 4,784-6,765 | Operators, memory, assignment, variables, and control flow |
| 6,766-8,430 | Traits, functions, callables, and dependency summaries |
| 8,431-10,521 | Literals, patterns, match, structs, and method dispatch |
| 10,522-11,630 | Recursive checking, validation, and operation dispatch |
| 11,631-12,110 | Analysis entry points, scheduling, cloning, and sessions |
| 12,111-12,689 | Trait validation, generic-cycle validation, and monomorphization |

`semantic_rules.casa` contains type rewriting and unification, operation facts,
fixed stack effects, trait satisfaction, and method resolution. Its rules are
used during syntax work, selective-import analysis, complete typechecking, and
bytecode generation. This is useful leverage. The problem is that callers can
select individual rules and reconstruct larger semantic decisions.

[ADR-0008](../adr/0008-shared-semantics.md) already places shared operation
semantics below syntax and typechecking. It calls for resolved operations,
inferred effects, ordered semantic dependencies, and diagnostics while keeping
transient stacks, branches, generics, callables, and dispatch private. The
current modules reuse one engine, but the 273-function surface and shared
mutable store do not provide the intended deep seam.

## Data ownership and phase state

`common::Op` starts with operation ID zero and optional type and deferred-return
hints. Its `OpValue` can be an unresolved identifier, several resolved call
forms, a source-level convenience operation, a lowered operation, or a nested
operation list. `common::Function` adds `is_resolved` and `is_typechecked`
booleans to mutable operations, stack effects, captures, and generic state.
`SymbolStore.op_ownership` stores ownership facts separately by the assigned
operation ID.

These values permit invalid combinations. A checked function can contain an
unresolved call. A specialized operation can retain stale bindings or
ownership. A nonzero operation ID can lack a side-table entry. A phase flag can
disagree with the contents of the operation tree. The code prevents these
states with scheduling order, guards, cloning rules, and convention. The type
model does not prevent them.

`SemanticSession` has two ownership modes. `from_store` takes the live store for
complete typechecking. `new` prepares a semantic clone for selective-import
dependency discovery. `clone_for_semantics` manually copies store fields and
function bodies. `commit_semantic_store` manually copies selected fields back
because heap-backed maps share mutation after shallow copies. The
`clone_bodies` and `infer_parameters` flags then select behavior inside common
analysis functions.

This is a shallow seam. Callers ask for semantic work, but they must know
whether it owns a store, clones bodies, infers parameters, or commits changes.
Adding a field to `SymbolStore` can require coordinated changes in clone and
commit code. An explicit isolated analysis product or owned store operation is
the smallest deepening opportunity. It would hide aliasing and commit policy
behind one interface.

## Repeated interpretation and traversal

Every checked operation first passes through `collect_operation_facts`.
That function calls `semantic_rules::analyze_operation` to collect dependencies
and discards the remaining facts. Operation-specific handlers then call
`analyze_operation` again for comparisons, memory operations, fixed operations,
trait calls and references, function execution, I/O, method calls, and format
strings. One operation can therefore repeat dispatch, lookup, and effect
classification before it changes the typed stack.

Nested operations have no common child traversal. At least eight walkers in
`semantics.casa` independently match arrays, expression groups, match arms,
and global initializers:

- Variable-use collection.
- Numeric-literal detection and default resolution.
- Default-method call qualification.
- Semantic cloning.
- Generic-cycle validation.
- Operation metadata specialization.
- Monomorphization.

`common::Type::substitute_t` already implements recursive type substitution.
`semantics::resolve_return_type_t` repeats the same recursion for two callers.
This duplicate has no separate language responsibility.

Callable return summaries can recursively analyze function bodies for a set
of callable argument bindings. Default trait methods use a synthetic function
and the same semantic analyzer. `ensure_typechecked` can also start recursive
function analysis before the outer scheduler reaches that function. An active
call set prevents recursion, but there is no summary cache keyed by function
and callable bindings. Context-sensitive callable effects are inherent. The
repeated construction and analysis of equivalent contexts is not.

## Control flow, traits, validation, and specialization

`IfContext`, `WhileContext`, and `MatchContext` repeat before and after stack
state, flow facts, and path flags. `merge_if_branch_stack` and
`unify_arm_stacks` implement similar stack-width and type joins.
`apply_match_result_stack` contains another related merge. `BranchFrame`
conversion methods terminate the process if a caller requests the wrong frame
kind. A common internal join operation and typed frame transitions can reduce
this repeated invariant logic without changing Casa's flow rules.

Trait and method dispatch has valid staged work. Syntax validates trait
declarations, inheritance, implementation compatibility, coherence, and
language trait contracts. Semantic analysis checks method calls and function
bodies. Typechecking later validates implementation stack effects and generic
cycles. Generic monomorphization must resolve layout-sensitive copy and drop
operations after a generic body has been checked.

The accidental part is the representation transfer. Default bodies are parsed
and stored, selectively analyzed through synthetic functions, cloned into
generated functions, checked, and resolved again during specialization.
`instantiate_default_trait_method` alone is about 194 lines. A later design
should place default-method instantiation, callable substitution, operation-ID
assignment, ownership transfer, and specialization metadata behind one private
module interface. This audit does not decide when that module runs or what IR
it owns.

Validation is not broadly duplicated. Syntax owns declaration-graph and
contract checks. Semantic analysis owns body and type obligations. The
avoidable complexity is that orchestration functions live among operation
handlers and mutate the same store. Moving validators behind phase results is
an interface and locality improvement, not a deletion of required checks.

## Origin and ownership representation

`TypedValue` can carry an optional place, a list of origin strings, a separate
cleanup-origin list, loan spans, a binding, callable values, and capability
state at the same time. Origin strings encode meaning with prefixes:

- `!` means exclusive access.
- `%:` means a callable-origin dependency.
- `@:` means a cleanup-origin dependency.
- Dot-separated text encodes field paths.

Helpers strip and parse those strings throughout ownership checks, callable
dependency collection, cleanup propagation, and returned-borrow analysis.
Typed place and origin variants would make impossible states harder to create
and remove repeated parsing. This is a representation finding, not a proposal
to change Casa's ownership behavior. A new type is useful only if it replaces
the strings and their parallel flags. Layering it over the current fields
would add complexity.

## Test coupling

`test_semantics_operations.casa` has 117 tests and 2,135 lines. Its helper
constructs operation lists and calls `analyze_function_semantics` through the
typechecker test boundary. This is mostly the correct deep seam. Some fixed
operation tests and later pipeline tests repeat the same stack-effect cases
with different setup.

The typechecker exports thin wrappers for `check_ops`, trait satisfaction,
method receiver type, literal defaulting, monomorphized names, and receiver
bindings. Most exist for tests or bytecode callers. These wrappers make
`typechecker.casa` look like a stable facade while its implementation simply
forwards to `semantics.casa`. Tests should use a stable semantic-analysis result
for behavior and keep direct rule tests only for rule-specific edge cases.

Do not add a second test layer for a new internal representation. When a stable
seam replaces the current one, remove tests that only mirror private handler
dispatch. Keep focused behavior tests for ownership diagnostics, trait
selection, stack joins, generic specialization, and selective dependencies.

## Inherent and accidental complexity

| Area | Inherent language complexity | Accidental implementation complexity |
|---|---|---|
| Typed stack | Reverse Polish argument order, type compatibility, inferred effects | Repeated effect lookup and handler dispatch for one operation |
| Type inference | Context-sensitive literals, parameters, returns, and callable effects | Optional hints and phase flags on shared mutable operations |
| Control flow | Definite availability and compatible branch stacks | Three parallel context records and repeated join algorithms |
| Ownership | Affine moves, loans, nested places, cleanup, and returned borrows | Encoded origin strings, parallel lists, and an ID-keyed side table |
| Traits | Static inheritance, qualified calls, default methods, and no runtime trait objects | Repeated synthetic function construction and dispatch resolution |
| Callables | Context-sensitive stack effects and returned callable bindings | Recursive equivalent body analysis without a keyed summary product |
| Validation | Declaration contracts, body rules, and generic-cycle checks | Validators mixed into a 12,689-line mutable checker module |
| Generics | One body check followed by layout-sensitive specialization | Cloned operation trees and manual metadata and ownership transfer |
| Monomorphization | Concrete code for concrete type bindings | Rewalking all nested operations with another local dispatcher |

The inherent column follows the accepted behavior in
[ADR-0037](../adr/0037-control-flow-joins-require-definite-availability.md),
[ADR-0069](../adr/0069-generics-are-monomorphized-after-one-body-check.md),
[ADR-0070](../adr/0070-traits-have-no-runtime-object-form.md),
[ADR-0096](../adr/0096-trait-inheritance-is-acyclic.md),
[ADR-0101](../adr/0101-types-may-implement-multiple-generic-trait-instantiations.md),
[ADR-0105](../adr/0105-borrows-distinguish-statically-disjoint-struct-fields.md),
[ADR-0108](../adr/0108-opaque-returned-borrows-keep-the-complete-input-loaned.md),
and
[ADR-0161](../adr/0161-borrow-errors-show-contracts-and-related-loan-locations.md).
A later architecture must preserve those decisions unless a separate language
decision changes them.

## Ranked simplification findings

The estimates count direct deletion or relocation after replacement. They are
not additive unless the ranges do not overlap.

No `stdlib:` or `native:` replacement was verified. The complexity is in
semantic state and control flow, not in a local utility implementation.

1. `shrink:` Replace shared cross-phase `Op`, `Function`, and side-table state
   with one owned checked semantic product at the existing analysis seam.
   Remove phase flags, stale-state guards, clone coordination, and metadata
   synchronization. Estimated production reduction or relocation: 250-500
   lines.
2. `shrink:` Compute `SemanticFacts` once per operation and pass it through the
   stack and ownership transition. Remove the second `analyze_operation` calls
   and parallel dispatch glue. Estimated production reduction: 80-150 lines.
3. `shrink:` Replace encoded origin and place strings plus parallel flags with
   one tagged internal representation. Remove parsing and normalization at
   callers. Estimated net production reduction: 120-250 lines.
4. `shrink:` Put default-method instantiation, callable substitution, and
   generic specialization behind one private interface. Remove repeated
   synthetic-function and metadata-transfer machinery. Estimated production
   reduction or relocation: 150-300 lines.
5. `shrink:` Give `Op` one private child traversal used by literal resolution,
   validation, cloning, specialization, and monomorphization. Estimated
   production reduction: 70-120 lines.
6. `shrink:` Produce or cache callable return summaries by function and
   callable binding context. Remove equivalent recursive body analysis and
   synthetic summary setup. Estimated production reduction or relocation:
   90-180 lines.
7. `shrink:` Use one stack-join operation and typed branch-frame transition for
   if, loop, and match analysis. Estimated production reduction: 50-100 lines.
8. `delete:` Replace `resolve_return_type_t` with
   `common::Type::substitute_t`. Estimated production reduction: 57 lines.
9. `yagni:` Remove forwarding helpers in `typechecker.casa` and
   `semantics.casa` when their callers can use the owning module or the checked
   result. Estimated production reduction: 45-60 lines.
10. `yagni:` Make semantic implementation functions private. The 252
    `semantics.casa` public functions with no direct cross-file reference are
    candidates, subject to a dead-declaration check.
    Expected line reduction: zero. Expected interface reduction: up to 252
    names after dead declarations are checked separately.
11. `shrink:` Replace handler-coupled tests with checked-result behavior tests
    when the semantic seam changes. Keep distinct rule and diagnostic cases.
    Estimated test reduction: 400-700 lines.

`net: -700 to -1,200 production lines, -400 to -700 test lines, -0 dependencies possible.`

The net discounts overlap between the ranked ranges. It does not count
visibility changes or assume that a new phase representation is shorter on
its first revision.

## Priority for later architecture work

1. Define the semantic pass boundary and ownership contract. State which input
   is consumed, which checked data is returned, and which data remains private.
2. Define valid operation phases and ownership of operation IDs, specialization
   bindings, and ownership facts. Reject designs that retain the current flags
   beside a second representation.
3. Make one operation interpretation feed dependencies, stack effects, type
   changes, ownership, and diagnostics. Measure how much repeated lookup this
   removes.
4. Unify typed origins and child traversal inside the semantic module. Keep
   callers unaware of encoding and recursion details.
5. Isolate control-flow joins, callable summaries, default methods, and generic
   specialization behind the pass boundary.
6. Close source visibility and consolidate tests only after the replacement
   interface exists. Replace old surfaces instead of adapting both indefinitely.

The parent architecture work should compare designs by interface size, invalid
state count, data ownership, dependency direction, and measured compile cost.
Source-line reduction is supporting evidence, not the only acceptance measure.

## Evidence index

- The analysis pipeline and result handoff are in
  [`compiler/analysis.casa` lines 49-75](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/analysis.casa#L49-L75)
  and
  [`compiler/typechecker.casa` lines 28-82](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/typechecker.casa#L28-L82).
- `TypedValue`, origin encodings, branch state, `TypeChecker`, repeated
  operation analysis, semantic sessions, validation, and monomorphization are
  in
  [`compiler/semantics.casa` lines 101-1,368](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/semantics.casa#L101-L1368),
  [`compiler/semantics.casa` lines 10,777-11,370](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/semantics.casa#L10777-L11370),
  and
  [`compiler/semantics.casa` lines 11,846-12,689](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/semantics.casa#L11846-L12689).
- Type rewriting, operation facts, fixed effects, and trait resolution are in
  [`compiler/semantic_rules.casa` lines 267-1,942](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/semantic_rules.casa#L267-L1942).
- `Op`, `OpValue`, `Function`, `SymbolStore`, and common type substitution are
  in
  [`compiler/common.casa` lines 479-1,550](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/common.casa#L479-L1550),
  [`compiler/common.casa` lines 2,130-2,188](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/common.casa#L2130-L2188),
  and
  [`compiler/common.casa` lines 3,420-3,470](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/common.casa#L3420-L3470).
- Early semantic dependency discovery is in
  [`compiler/selective_import.casa` lines 195-214](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/selective_import.casa#L195-L214),
  [`compiler/selective_import.casa` lines 580-625](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/selective_import.casa#L580-L625),
  and
  [`compiler/syntax.casa` lines 9,100-9,145](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/syntax.casa#L9100-L9145).
- Downstream semantic consumers are in
  [`compiler/bytecode.casa` lines 3,690-3,721](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/bytecode.casa#L3690-L3721).
- The largest focused operation and trait suites are
  [`tests/compiler/test_semantics_operations.casa`](../../tests/compiler/test_semantics_operations.casa)
  and [`tests/compiler/test_traits.casa`](../../tests/compiler/test_traits.casa).
