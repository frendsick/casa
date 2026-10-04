# Compiler products own independent snapshots

status: amended by [ADR-0172](0172-editor-products-retain-verified-source-facts.md)

related issue: [Choose compiler representations and state ownership](https://github.com/frendsick/casa/issues/646).

Typed operations and private semantic construction remove caller-managed phase
protocols under [ADR-0166](0166-compiler-capsule-owns-phase-state.md).
ADR-0172 refines the editor answers below, and
[ADR-0169](0169-backend-plans-and-renders-one-function-at-a-time.md) defines the backend.

## Selected model

Use three typed operations for syntax, analysis, and assembly. Each request owns an
independent snapshot. Inside the compiler, retain a source representation and one
semantic body representation. Private builders establish validity before publishing
immutable products. The backend receives a sealed `CheckedProgram`
containing concrete semantic bodies and their required declarations.

Represent structured control flow directly. Put cleanup actions on the operations and
exits that perform them. Keep temporary loan analysis and specialization work private.
Editor queries consume source-oriented facts, without retaining compiler bodies.

The checked product replaces public operation/store pairing without duplicating
the complete operation vocabulary for every phase.

## External interface

The following is contract notation, not executable Casa syntax. Names are illustrative.

```text
syntax(SourceUnit) -> Result[SyntaxResult CompilerFailure]
analyze(CompilationInput) -> Result[AnalysisSnapshot CompilerFailure]
assembly(CompilationInput, Target) -> Result[AssemblyResult CompilerFailure]

hover(&AnalysisSnapshot, Position) -> HoverResult
definition(&AnalysisSnapshot, Position) -> DefinitionResult
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

[ADR-0172](0172-editor-products-retain-verified-source-facts.md) defines which facts
survive each source failure and distinguishes absent, unavailable, incomplete, and
complete answers. It also defines workspace reference aggregation and rename through
these same queries. Incomplete facts cannot be used as checked codegen input.

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
retain both.

## Private representations

### Source input

`SourceProgram` owns parsed modules, declarations, lexical scopes, and structured source
bodies. Tokens retain original spelling and source ranges. Declaration and binding
identities are separate values, scoped to the compilation. A private module builder owns
discovery, visibility, duplicate checks, import relationships, and lookup tables.

Source bodies contain structured branches, loops, match arms and guards, expression
groups, and ordinary source leaves. A name occurrence remains source syntax until the
shared semantic traversal resolves it in its recorded scope. This product is parsed
input that has not been fully resolved. No backend or editor consumer reads
it.

Declaration signatures remain available while recursive bodies are being checked. A body
work item owns the body being transformed. Pending, active, completed, and failed work
are private alternatives, rather than public flags beside a replaceable body. Failure
discards that request's unfinished work. There is no public take/restore protocol.

Import policy follows the qualified-only contract in
[Imports expose qualified names only](0168-imports-expose-qualified-names-only.md).
Constant elaboration applies
[Constants use bounded target-independent expressions](0171-constants-use-bounded-target-independent-expressions.md).
Neither permits publishing unresolved code as checked input.

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
parsed/resolved/typed/specialized/backend copy of the operation vocabulary.

A generic recipe may contain type parameters and already-checked constrained dispatch.
Specialization substitutes those terms and resolves concrete dispatch within the same
node vocabulary. `CheckedProgram` has a private constructor that verifies every body
passed to the backend is concrete and that no deferred dispatch remains. The
representation admits symbolic terms internally. The constructor, not a claim about the
underlying enum, establishes concreteness.

The checked-once generic recipe follows
[ADR-0170](0170-generics-specialize-after-symbolic-checking.md). Specialization and
trait dispatch finish before backend entry.

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
selection, and assembly construction. The backend lowers typed `size_of` queries
using its layout plan. Source checking also consults the current physical layout
to validate concrete `size_of` queries.
[ADR-0171](0171-constants-use-bounded-target-independent-expressions.md) excludes
layout queries from constant initializers and constant type arguments.

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
| `EditorIndex` | Shared source and semantic traversal. Analysis queries consume projected facts only. | Owned by `AnalysisSnapshot`. Assembly releases its editor facts. |
| `SyntaxResult` | Syntax entry operation. Formatter and syntax tests. | Owns report, tokens, and any usable structural facts. Borrowed views cannot outlive it. Dropping it releases all retained storage. |
| `AnalysisSnapshot` | Analysis entry operation on accepted or rejected source. LSP and query tests. | Owns one report and index, with no compiler bodies. Queries borrow temporarily and return owned values. Replacement releases the old snapshot when borrows end. |
| Backend work | Backend receives `CheckedProgram` and target. Target planner and lowering use private state. | Owns layout/ABI caches and any machine form. Releases them on success, target rejection, or internal failure. |
| `AssemblyResult` | Assembly entry operation after target processing. CLI/native build adapter. | Owns report and, only on success, target-tagged assembly. Source and compiler state need not remain borrowed during native process execution. |
| `CompilerFailure` | Entry operation packages private failure with the still-owned report. CLI/LSP/formatter inspect it. | Owns all retained error context. No dangling phase borrow or partially valid backend product. |

## Product boundaries

| Invalid state | What prevents or contains it |
| --- | --- |
| Source spelling versus internal name | Immutable source tokens and separate declaration identities. No reverse mapping compensates for token rewriting. |
| Parser/module collections and flags | Private request-local builder. Commit validates discovery, visibility, and required declaration relationships. These are private consistency checks, not invariants claimed to be enforced entirely by types. |
| Phase-polymorphic `Op` | Source forms cannot enter `CheckedProgram`. Semantic recipe terms remain possible privately, but the concrete commit rejects unresolved terms and dispatch. |
| Function body versus phase booleans | Work state owns transformation. A completed body is published atomically, without independent public booleans or caller-managed restore. |
| Aliased or mismatched `SymbolStore` | One checked product owns bodies and referenced declaration facts. External callers cannot supply a mismatched pair. Internal ID references are validated when committing. |
| `Op.id` versus ownership side table | Cleanup and transfer actions travel with their semantic nodes and exits. Specialization substitutes those payloads with the body. No ownership table repair follows ID reassignment. |
| Error-bearing typecheck output accepted by backend | Partial `AnalysisSnapshot` and private `CheckedProgram` are different products. Only successful semantic commit creates backend input. Target rejection remains valid afterward. |
| Mismatched machine program fields or instruction family | Machine state is backend-private. Its builder validates labels, pools, and family inputs. The backend decision chooses the representation, without exposing unchecked construction to callers. |

## Validation

Tests cross the typed compiler and editor interfaces. Keep source-to-behavior coverage
for branch joins, loops, matches, returned borrows, cleanup order, generic dispatch, and
native calls. Formatter tests check safe rejection and token/comment/structure
equivalence. Report tests check exact overridden/imported text and accumulated
diagnostics after source rejection and reportable internal failure.

Private tests may exercise semantic commits and backend plans through their actual
constructing interfaces. Keep focused checks that reject unfinished instances,
unresolved dispatch, invalid references, and mismatched machine state. Replace tests
whose sole purpose is to mirror public fields, clone lists, take/restore ordering, or
operation-ID transfer.
