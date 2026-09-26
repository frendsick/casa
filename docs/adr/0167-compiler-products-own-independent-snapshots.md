# Compiler products own independent snapshots

related issue: [Choose compiler representations and state ownership](https://github.com/frendsick/casa/issues/646).

The maintainer accepted independent compilation snapshots and the representation
contract below on 2026-09-26. Typed operations and private semantic construction remove
caller-managed phase protocols. Production migration remains pending. The separate
language, tooling, and performance decisions remain open. The backend contract
is recorded in [ADR-0169](0169-backend-plans-and-renders-one-function-at-a-time.md).

Evidence was checked against `c8b392bcba0648fa759290f25eb91d4a90faa32a`, which matched
`origin/main` on 2026-09-26. ADR-0166 supplies the accepted Compiler Capsule
constraints. Historical audit counts remain historical.

## Selected model

Use three typed operations for syntax, analysis, and assembly. Each request owns an
independent snapshot. Inside the compiler, retain a source representation and one
semantic body representation. Private builders establish validity before publishing
immutable products. The backend receives a sealed, target-neutral `CheckedProgram`
containing concrete semantic bodies and their required declarations.

Represent structured control flow directly. Put cleanup actions on the operations and
exits that perform them. Keep temporary loan analysis and specialization work private.
Editor queries consume source-oriented facts, without retaining compiler bodies.

This replaces the current public operation/store pairing. It does not require a distinct
copy of the complete operation vocabulary for every phase.

## External interface

The following is contract notation, not executable Casa syntax. Names are illustrative.

```text
syntax(SourceUnit) -> Result[SyntaxResult CompilerFailure]
analyze(CompilationInput) -> Result[AnalysisSnapshot CompilerFailure]
assembly(CompilationInput, Target) -> Result[AssemblyResult CompilerFailure]

hover(&AnalysisSnapshot, Position) -> Option[Hover]
definition(&AnalysisSnapshot, Position) -> Option[SourceRange]
completion(&AnalysisSnapshot, Position, Trigger) -> CompletionResult
references(&AnalysisSnapshot, Position, IncludeDeclaration) -> ReferenceResult
semantic_tokens(&AnalysisSnapshot, File) -> TokenResult
```

Every result has access to one immutable report containing ordered diagnostics and the
exact source text used by that request. Report access supplies diagnostics and source
lookup without a separate query/answer protocol. Syntax additionally exposes lossless
tokens, usable structural facts when available, and syntax-equivalence comparison for
the formatter.

- `SyntaxResult` distinguishes usable syntax from rejection. Rejection retains
  diagnostics and recovered tokens without promising valid structural facts.
- `AnalysisSnapshot` owns its report and `EditorIndex`. Source errors can coexist with
  partial editor facts. No operation extracts codegen input from this product.
- `AssemblyResult` is either `Produced(report, AssemblySource)` or `Rejected(report)`.
  Produced assembly carries its implemented target. Rejection includes source errors and
  target restrictions.
- `CompilerFailure` owns the report accumulated before an internal invariant failed,
  plus phase, message, and available source attribution. It contains no codegen input.
  It describes reportable compiler failures, not recovery from process termination or
  memory exhaustion.
- Assembler, linker, and executable launch failures remain outside these operations.

Query methods borrow the snapshot for the duration of the call and return owned
presentation values. Results contain no declaration, operation, or function-body
objects. Their positions refer to that snapshot's source text. The caller retains the
snapshot until source-based conversion is complete, or retains the already converted
result. Results must not be applied to a newer document revision without checking the
caller's document version.

The tooling decision must define which facts survive each source failure and how
unavailable or incomplete answers are represented. `CompletionResult`,
`ReferenceResult`, and `TokenResult` deliberately leave that contract open. The
representation decision requires that incomplete facts cannot masquerade as checked
codegen input. It does not promise a recovery level or silently replace an unavailable
answer with a complete empty list.

### Caller knowledge comparison

| Surface | Broad prototype protocol | Typed operations |
| --- | --- | --- |
| Compiler intent | 3 request variants and 4 product variants. Only 4 of the 12 request/product combinations are meaningful. | 3 operations whose return types encode the requested product. |
| Tool query | 6 request variants and 6 answer variants. Only 6 of the 36 combinations are meaningful. | 5 editor operations plus report access. Each query has its own answer type. |
| Failure | Request-specific rejection plus outer internal failure. The prototype's internal error omits the report. | Same necessary distinction between source rejection and internal failure. Both preserve sources and diagnostics. |
| Ordering | Caller chooses a request, matches a product, then performs matching queries. | No phase ordering or product-tag agreement. Formatter equivalence checking remains a real two-input operation. |
| Ownership | Caller still needs report, source, index, and query lifetime rules. | The same necessary snapshot lifetime rule, with no retained compiler session. |

The typed interface adds callable names but removes tag-pairing rules. It does not claim
that ownership and failure rules disappear. Current LSP code uses five distinct editor
queries. References also supplies rename, so rename needs no second symbol-analysis
operation.

## Independent snapshots

`CompilationInput` owns the root source, root identity, ordered library paths, and
source overrides. Relative path resolution uses an explicit request base directory
captured by the caller. It must preserve Casa's distinct path-style and module-style
import rules.

The request-local source owner resolves an import, reads its selected contents once, and
retains those exact bytes. Matching overrides take precedence over disk contents.
Tokens, declarations, and diagnostics refer to that retained source. Repeated loads of
the same resolved file reuse its entry within the request. Snapshot identity does not
imply an atomic filesystem snapshot across multiple reads.

Unused overrides can be released after discovery. A completed product retains the root
and sources actually used, including any source required by diagnostics or editor facts.
It does not retain every unrelated open document merely because it was supplied as an
override.

Analysis followed by assembly repeats compiler work. Repeated editor requests also
construct new snapshots. There is no public prepare/advance/reset protocol, shared
mutable session, cross-request symbol identity, or required cache. Within one request,
memoization of module loads, semantic instances, layouts, and ABI plans remains
appropriate.

Replacing an editor snapshot releases its source and index storage when no query still
borrows it. Producing a replacement while retaining the old snapshot can temporarily
retain both. Required edit latency, workspace invalidation, and numerical memory limits
remain with the tooling and performance decisions. If those requirements cannot be met,
revisit this choice using measurements.

## Private representations

### Source input

`SourceProgram` owns parsed modules, declarations, lexical scopes, and structured source
bodies. Tokens retain original spelling and source ranges. Declaration and binding
identities are separate values, scoped to the compilation. A private module builder owns
discovery, visibility, duplicate checks, import relationships, and lookup tables.

Source bodies contain structured branches, loops, match arms and guards, expression
groups, and ordinary source leaves. A name occurrence remains source syntax until the
shared semantic traversal resolves it in its recorded scope. This product is parsed
input, not a falsely named fully resolved program. No backend or editor consumer reads
it.

Declaration signatures remain available while recursive bodies are being checked. A body
work item owns the body being transformed. Pending, active, completed, and failed work
are private alternatives, rather than public flags beside a replaceable body. Failure
discards that request's unfinished work. There is no public take/restore protocol.

Exact import-selection policy and constant elaboration remain separate decisions. They
may change the module builder's work, but cannot publish unresolved code as checked
input.

### Semantic bodies

Use one private semantic node vocabulary for checked generic recipes and concrete
bodies. It contains resolved actions such as scalar operations, calls, literal
materialization, value movement, copies, projections, assignments, borrow formation,
cleanup, and structured control flow. Each action carries the facts needed by its
consumer. It does not carry a general bag of optional hints.

Stable leaf concepts include structural types, declaration identity, binding identity,
source origin, field path, and operation kind. Source spelling is never overwritten with
an internal name. A source node vocabulary describes grammar, while the semantic
vocabulary describes established behavior. There is no separate
parsed/resolved/typed/specialized/backend copy of the current 128-variant enum.

A generic recipe may contain type parameters and already-checked constrained dispatch.
Specialization substitutes those terms and resolves concrete dispatch within the same
node vocabulary. `CheckedProgram` has a private constructor that verifies every body
passed to the backend is concrete and that no deferred dispatch remains. The
representation admits symbolic terms internally. The constructor, not a claim about the
underlying enum, establishes concreteness.

The checked-once generic recipe follows the current generic ADR. The separate generic
contract decision can revise admissible generic programs or their diagnostics. It cannot
weaken the already accepted requirement that specialization and trait dispatch finish
before backend entry.

### Concrete examples

These sketches show representation responsibilities, not a proposed Casa syntax.

| Case | Representation and established fact |
| --- | --- |
| Branch | `If(condition, then_body, else_body, join)`. Each continuing arm contributes an ordered typed stack and ownership state. An absent `else` contributes the unchanged incoming path. Terminating arms contribute no join state. |
| Loop | `Loop(loop_id, condition, body, exit_join)`. The checker validates header stack and ownership on every back-edge and `continue`. False-condition and reachable `break` edges contribute to the exit. |
| Match | `Match(subject, arms, join)`. Subject capability and place determine binding behavior. Arms own patterns, guards, and bodies. The checker establishes exhaustiveness and the join of continuing arms. |
| Early return | `Return(result_transfers, cleanup)`. Result owners move first. Remaining initialized locals and temporaries are destroyed in the accepted LIFO order. The path ends here. |
| Borrowed result | The function's checked result summary records the compatible input-origin sets, exclusivity, and cleanup-observed dependencies. Call checking substitutes actual origins. One result can depend on several inputs. No local-owner origin escapes. |
| Specialized call | `Call(concrete_function_id, argument_actions, result_types)`. The referenced body exists in the same checked product. Argument actions already state moves and reborrows. The backend performs calling-convention lowering without trait lookup or type inference. |

Checked bodies are structured rather than a flat stream of start/end markers. Shared
primitive kinds do not force another structured tree for each phase. A backend can
privately lower these bodies to blocks or instructions if its decision demonstrates a
need.

### Joins and cleanup

The checker owns stack joining, whole-binding availability, loan propagation, and
termination. It uses local dataflow state, including precise field paths where
supported. Availability requires ownership on every continuing predecessor. Possible
live loans are merged conservatively. Loop back-edges must restore the state required by
the header.

Cleanup actions are attached directly to assignments, scope fallthrough, returns,
breaks, and continues. They name the owner and concrete or parameterized type, rather
than an operation ID used to retrieve a separate ownership record. Ordinary borrow
origins and cleanup-observed origins remain distinct while checking.

When conditional initialization makes an owner's existence depend on the executed path,
the checker specifies the required conditional cleanup and its state updates. The
backend allocates physical storage for such state. It does not decide whether a value
was moved or whether cleanup is required. Panic and process exit carry no unwinding
cleanup.

Only facts needed after checking survive in the checked body. Temporary loan maps and
diagnostic provenance used only by the checker can be released after final checks and
editor-fact projection. This avoids retaining the entire proof history as backend input.

### Specialization and backend input

A private worklist keys instances by declaration identity and canonical type/constant
bindings. A reserved instance identity can support recursive references while its body
is being built. The worklist owns cycle detection, substitution, and deferred trait
dispatch. It commits `CheckedProgram` only after every required reachable instance is
complete and all source-level checks have passed.

`CheckedProgram` owns concrete bodies and the semantic declaration/type facts they need.
The backend receives this single product, never separate operations and a
caller-supplied store. There is no public mutable access or public constructor. Neither
partial analysis nor a report can yield this product.

The backend owns target layouts, field storage plans, ABI plans, labels, pools, runtime
selection, and assembly construction. `size_of` remains a typed symbolic query until
target planning supplies its value. Constant evaluation must not require a physical
target value to claim that a target-neutral product is complete. Whether such use is
allowed must be reconciled in the constant-evaluation decision.

[ADR-0169](0169-backend-plans-and-renders-one-function-at-a-time.md) retains a private
instruction buffer for one function at a time. Its constructors belong to the backend,
which validates instruction families, labels, and pools before publishing assembly.

## Ownership and reclamation

| Owner or product | Constructor and consumers | Transfer, borrows, and release |
| --- | --- | --- |
| Request source/report builder | Compiler entry operation. Parser, checker, and backend borrow source context and append diagnostics through private interfaces. | Owns request input and loaded source bytes. Becomes one report on success, rejection, or reportable internal failure. No phase takes the last copy of error context away from it. |
| `SourceProgram` | Module/parser builder. Shared semantic checker consumes it. | Owns module visibility, declarations, scopes, and source bodies. Body work items move out internally. Released when semantic construction no longer needs it, or on failure. |
| Semantic work | Checker and specialization worklist. Private builders only. | Owns local stack/loan state, recipes, pending instances, and checked facts. Moves concrete bodies into `CheckedProgram`. Releases temporary state on every return path. |
| `CheckedProgram` | Successful semantic commit. Analysis orchestration may discard it, assembly transfers it to the backend. | Owns all paired body/declaration facts. Backend borrows read-only during lowering, then releases the product. No public retained borrow. |
| `EditorIndex` | Shared source and semantic traversal. Analysis queries consume projected facts only. | Owned by `AnalysisSnapshot`. Assembly releases its editor facts. Storage and formatting costs must be measured. This decision does not claim they are free. |
| `SyntaxResult` | Syntax entry operation. Formatter and syntax tests. | Owns report, tokens, and any usable structural facts. Borrowed views cannot outlive it. Dropping it releases all retained storage. |
| `AnalysisSnapshot` | Analysis entry operation on accepted or rejected source. LSP and query tests. | Owns one report and index, with no compiler bodies. Queries borrow temporarily and return owned values. Replacement releases the old snapshot when borrows end. |
| Backend work | Backend receives `CheckedProgram` and target. Target planner and lowering use private state. | Owns layout/ABI caches and any machine form. Releases them on success, target rejection, or internal failure. |
| `AssemblyResult` | Assembly entry operation after target processing. CLI/native build adapter. | Owns report and, only on success, target-tagged assembly. Source and compiler state need not remain borrowed during native process execution. |
| `CompilerFailure` | Entry operation packages private failure with the still-owned report. CLI/LSP/formatter inspect it. | Owns all retained error context. No dangling phase borrow or partially valid backend product. |

## Eight invalid-state families

| Baseline family | What prevents or contains it |
| --- | --- |
| Source spelling versus internal name | Immutable source tokens and separate declaration identities. No reverse mapping compensates for token rewriting. |
| Parser/module collections and flags | Private request-local builder. Commit validates discovery, visibility, and required declaration relationships. These are private consistency checks, not invariants claimed to be enforced entirely by types. |
| Phase-polymorphic `Op` | Source forms cannot enter `CheckedProgram`. Semantic recipe terms remain possible privately, but the concrete commit rejects unresolved terms and dispatch. |
| Function body versus phase booleans | Work state owns transformation. A completed body is published atomically, without independent public booleans or caller-managed restore. |
| Aliased or mismatched `SymbolStore` | One checked product owns bodies and referenced declaration facts. External callers cannot supply a mismatched pair. Internal ID references are validated when committing. |
| `Op.id` versus ownership side table | Cleanup and transfer actions travel with their semantic nodes and exits. Specialization substitutes those payloads with the body. No ownership table repair follows ID reassignment. |
| Error-bearing typecheck output accepted by backend | Partial `AnalysisSnapshot` and private `CheckedProgram` are different products. Only successful semantic commit creates backend input. Target rejection remains valid afterward. |
| Mismatched machine program fields or instruction family | Machine state is backend-private. Its builder validates labels, pools, and family inputs. The backend decision chooses the representation, without exposing unchecked construction to callers. |

## Validation and remaining decisions

Tests cross the typed compiler and editor interfaces. Keep source-to-behavior coverage
for branch joins, loops, matches, returned borrows, cleanup order, generic dispatch, and
native calls. Formatter tests check safe rejection and token/comment/structure
equivalence. Report tests check exact overridden/imported text and accumulated
diagnostics after source rejection and reportable internal failure.

Private tests may exercise semantic commits and backend plans through their actual
constructing interfaces. Keep focused checks that reject unfinished instances,
unresolved dispatch, invalid references, and mismatched machine state. Replace tests
whose sole purpose is to mirror public fields, clone lists, take/restore ordering, or
operation-ID transfer. Implementation and executable validation remain with the
blueprint and production migration.

The existing executable-blueprint decision must demonstrate all six representation cases
above, including conditional cleanup and a borrowed generic result. It must measure
retained memory and repeated-request cost, and show that editor queries no longer retain
or interpret compiler bodies. No source reduction or performance gain is established by
this decision.

The front-end decision owns exact parsing, resolution, and constant elaboration seams.
The semantic decision owns checking and specialization algorithms. The tooling decision
owns partial-fact guarantees, target diagnostics in editor analysis,
version/invalidation policy, and latency. ADR-0169 records machine form and
platform scope. The constant-evaluation decision must reconcile layout-dependent
constants with target-neutral checking. These are existing open decisions, not new
workstreams.

## Evidence

- [Current analysis result and entry
  operation](https://github.com/frendsick/casa/blob/c8b392bcba0648fa759290f25eb91d4a90faa32a/compiler/analysis.casa#L33)
  expose diagnostics, sources, and optional typechecked data.
- [Current typecheck result and
  orchestration](https://github.com/frendsick/casa/blob/c8b392bcba0648fa759290f25eb91d4a90faa32a/compiler/typechecker.casa#L7)
  expose mutable operations and store. [Backend
  entry](https://github.com/frendsick/casa/blob/c8b392bcba0648fa759290f25eb91d4a90faa32a/compiler/bytecode.casa#L3713)
  checks diagnostics at runtime.
- [Operation ownership
  records](https://github.com/frendsick/casa/blob/c8b392bcba0648fa759290f25eb91d4a90faa32a/compiler/common.casa#L479)
  and [function
  flags](https://github.com/frendsick/casa/blob/c8b392bcba0648fa759290f25eb91d4a90faa32a/compiler/common.casa#L1479)
  demonstrate the current identity and lifecycle protocols.
- [Current semantic value
  state](https://github.com/frendsick/casa/blob/c8b392bcba0648fa759290f25eb91d4a90faa32a/compiler/semantics.casa#L101)
  distinguishes ordinary and cleanup-observed origins.
- [Editor
  state](https://github.com/frendsick/casa/blob/c8b392bcba0648fa759290f25eb91d4a90faa32a/compiler/document.casa#L22)
  retains operations and declaration maps. [Document
  construction](https://github.com/frendsick/casa/blob/c8b392bcba0648fa759290f25eb91d4a90faa32a/compiler/document.casa#L706)
  consumes the analysis result.
- [Syntax
  analysis](https://github.com/frendsick/casa/blob/c8b392bcba0648fa759290f25eb91d4a90faa32a/compiler/syntax.casa#L8359)
  supports the formatter's independent syntax request.
- [Pinned architecture
  comparison](https://github.com/frendsick/casa/blob/bd3516658e11b4a1544562a552ae47e37329c073/compiler/compiler_architecture_prototype.html)
  supplies the broad intent and query protocols used in the comparison.
- [Pinned invalid-state
  baseline](https://github.com/frendsick/casa/blob/539d13953c189d2d12be1e8daf3cbf9947fc977d/docs/benchmarks/compiler-complexity-baseline.md#L158)
  identifies the eight families.
