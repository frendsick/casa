# Semantic checking owns source obligations

related issue: [Choose the semantic-analysis and specialization seams](https://github.com/frendsick/casa/issues/648).

The maintainer accepted this contract on 2026-09-26. Compiler Capsule uses one
request-owned semantic builder. It checks source bodies,
proves ownership and control flow, and specializes reachable concrete bodies
before it commits a target-neutral checked program. The target backend decides
physical layout and machine behavior. This places each source-level decision
with the state that can prove it, while keeping temporary checking state out of
compiler products. This refines [ADR-0167](0167-compiler-products-own-independent-snapshots.md)
and [ADR-0170](0170-generics-specialize-after-symbolic-checking.md). Production
migration remains pending.

Compiler-source evidence was inspected at
`18e23726e93924294313979eac4023b7536696d7` on 2026-09-26. Later
documentation-only merges changed the worktree base without changing those
sources. The
[semantic audit](../benchmarks/semantic-analysis-complexity.md) is a historical
baseline. Current code still uses a mutable `SymbolStore`, phase flags, encoded
origins, and operation-ID ownership records. The contracts below are a design,
not a claim about implemented behavior.

## Private semantic seam

These are interface sketches, not Casa declarations. The module names and
internal record names are illustrative. A caller supplies parsed source facts
with explicit recovery coverage and a request-owned report builder. It does
not supply a mutable
semantic store or select clone, inference, or commit modes.

| Operation | Input and output | Mutation, failure, and ownership |
| --- | --- | --- |
| `check(SourceProgram, ReportBuilder)` | Private checked generic recipes, checked concrete declarations, source dependencies, and editor facts. | Consumes source bodies through private work items. Owns typed stacks, scopes, loans, summaries, and diagnostics. A source error retains the report and usable editor facts but produces no checked program. |
| `specialize(CheckedRecipes, ReportBuilder)` | Complete reachable concrete semantic instances and their declarations. | Owns the worklist, instance cache, and concrete obligation resolution. A failed binding keeps source context and cannot yield backend input. |
| `commit(ConcreteInstances)` | One immutable `CheckedProgram`, or a reportable internal failure. | A private constructor verifies concrete types, resolved targets, valid references, complete bodies, and attached cleanup. It transfers bodies into the program and releases temporary proof state. |

These operations are private to the assembly path. Analysis can project an
`EditorIndex` from established source facts even when source errors prevent commit.
The public syntax, analysis, assembly, and editor interfaces remain those of
ADR-0167. No tool or backend caller observes the semantic builder.

Declaration identity, binding identity, source spelling, and source ranges are
different values. The parsed program owns declarations and name lookup. The
semantic builder owns checked bodies and source-level facts. The backend owns
target layouts, field offsets, storage, ABI placement, machine operations, and
emission. The report builder keeps the exact source snapshot on success,
rejection, and reportable internal failure. No retained borrow crosses a
completed request. Pending semantic work and caches are freed on every exit.

## One operation decision

The checker consumes a structured source node with its lexical scope, source
range, and current typed stack. For a leaf, it resolves the name or operation,
finishes local input-driven type inference and numeric literal defaulting,
adjusts the receiver for its actual borrow capability, and ensures any
compiler-provided Clone candidate before method lookup. One resolved decision
then provides the stack effect, effective declaration and trait target,
ordered semantic dependencies, ownership action, and diagnostic context.
The checker applies that decision once to the stack and ownership state and
emits one semantic node. A failed decision emits a source diagnostic and no
apparently checked node.

For editor recovery, each established declaration, occurrence, type, and
scope fact is committed with its source region and prerequisites. If an
operation fails, facts that depend on its stack or receiver state remain
unavailable. Independent later regions and modules can still contribute
facts when the shared parser and checker establish their scope and inputs.
The index records missing coverage for affected queries. It never presents
an unresolved binding or inferred type as verified. This follows the
[tooling recovery contract](0172-editor-products-retain-verified-source-facts.md).

Dispatch and dependencies use the same final receiver and implementation
context. A preliminary dependency scan may discover names, but its candidate
method is never an authoritative target. This matters today because the
dependency prepass uses a defaulted stack type while the method handler may
change the receiver capability and install Clone before resolution. The
semantic decision is the single source of truth for both outputs. Later
private traversals may consume its established target for editor facts,
reachability, or code emission without resolving it again.

The semantic body has structured `If`, `Loop`, `Match`, and expression nodes,
plus leaves for resolved actions. A leaf carries only the facts its consumer
needs, such as an established target, typed operands, transfer, or cleanup.
Source spelling and ranges remain attached for diagnostics. Generic recipes
may contain symbolic terms and constrained dispatch. Concrete instances use
the same node vocabulary with those terms substituted and targets resolved.
No general bag of optional phase hints or second complete operation enum is
required.

## Ownership, returns, and control flow

The checker represents a place as a binding identity plus a field path where
the accepted field-sensitivity rule applies. An origin is a typed reference
to a source input, local place, or callable input, with access capability.
Ordinary origins and origins observed by cleanup are distinct typed sets.
Loans record shared or exclusive capability, live place, and the source ranges
needed for the conflict and later-use diagnostic. A typed value combines its
type, capability, optional place, source range, and those origin sets. This
replaces prefixes such as `!`, `%:`, and `@:` and their parsing helpers.
It also replaces parallel binding-origin and cleanup-origin maps rather than
copying them into another layer.

Each body has a local typed stack and ownership state. Stack transitions own
literal resolution, input-driven inference through a trait implementation,
bound checks, moves, Copy eligibility, reborrows, and unsafe obligations.
Whole-binding moves invalidate the owner. Statically disjoint struct-field
loans can coexist. A returned borrow summary refers to compatible source
inputs, with the accepted conservative whole-input rule for opaque returns.
It cannot name a local owner. Closure checking records capture mode and
repeatability in the checked closure body. Callable summaries describe
possible returned function targets, returned origins, and cleanup-observed
origins. A call substitutes its actual argument facts into that summary.

One private completion operation handles every continuing `if` and `match`
arm in this order: transfer branch-result ownership out of branch-local
bindings, attach and validate LIFO cleanup for owners left in the scope,
exit the scope, then contribute the resulting typed stack and ownership
state to the join. A return, break, or continue transfers its outputs and
attaches cleanup to that exit. A non-continuing path contributes no state to
the ordinary join. An absent `else` contributes the incoming state. A match
also validates subject and pattern behavior and exhaustiveness. The join
requires compatible stack widths and types, requires an owner to be available
on every continuing predecessor, and unions possible live loans. It
preserves source locations for conflicting paths. Loop headers and every
back-edge or continue require compatible stack and ownership state. False
conditions and reachable breaks contribute to the loop exit. Cleanup is a
semantic action attached to the operation or exit, including conditional
cleanup state for path-dependent initialization. The backend allocates that
state but does not decide whether cleanup is due. Panic and process exit do
not unwind.

## Checked generic recipes and callable summaries

Every admitted generic declaration is checked once with symbolic type and
constant parameters against its declared bounds, including unused bodies.
Direct calls infer bindings from consumed inputs, receivers, and their trait
implementations. Explicit function references supply complete arguments.
The same binding validation checks concrete integer widths and symbolic
forwarding at direct calls and references. Source checking records constrained
trait operations, source call edges, returned-origin rules, and ownership
actions in a checked recipe. Specialization substitutes a recipe and
resolves concrete obligations. It never reruns source body checking.

Callable effect and return-summary computation uses constraints recorded
during the single source-body traversal. Calls whose recursive return facts
are not yet known retain summary dependencies and ownership obligations.
After the summaries stabilize, the checker discharges those obligations
against the final facts before accepting the recipe. It does not parse or
typecheck the body again. The summary key includes declaration identity,
relevant trait
implementation identity, complete type and constant bindings, and the
callable-target sets bound to input parameters. The summary has a finite
domain: source callable declarations and input placeholders, plus bounded
place paths and source-input origin dependencies. A recursive dependency
starts with no established targets, then unions facts until the strongly
connected group reaches a fixed point. Dependents are revisited when a
summary grows. The builder never treats an in-progress empty summary as a
proof that no callable or borrow can escape. It rejects a context it cannot
represent soundly, with source attribution, rather than silently dropping an
edge. The finite domain and monotone union guarantee termination, subject to
ordinary resource limits. Stable summaries can be reused by the same request
only for the same immutable declaration, binding, and callable context.
Dependency changes invalidate in-progress summaries and dependent obligations.
This analysis consumes established operation decisions and cannot weaken the
one-time symbolic body check. Cache entries are request-local, immutable once
stable, and released with the request.

Generic recursion validation traverses the entire source dependency chain,
including non-generic functions and function references. It composes complete
type and constant bindings along each edge. An active revisit of the same
declaration with changed bindings is rejected, even if the cycle is finite.
The diagnostic shows the source cycle and changed bindings. A revisit with
identical bindings terminates that search path. The validation runs for
unused admitted generic declarations as well as reachable ones. Independent
non-recursive instantiations remain valid.

## Concrete specialization

A request-local worklist starts with ordinary root execution and follows
calls, references, closure targets, concrete Clone and drop obligations,
derived structural operations, and trait defaults. An instance key is the
canonical declaration identity, relevant implementation identity, and full
type and constant binding tuple. The identity does not depend on a formatted
name or hash. Reserving an instance ID before filling its body permits valid
same-binding recursion. A pending instance is never published as complete.
Repeated use of one key shares its completed instance.

A trait default is a checked generic body owned by its declaring trait. Its
key also identifies the selected implementation and bindings. Unqualified
calls to that trait's methods resolve within that implementation. An explicit
override supplies its own body when accepted by the trait contract. No
synthetic source function or per-receiver body recheck is needed.

Each derive request expands into its complete effective trait closure before
coherence checking. The accepted target trait graph gives these contributions:

| Request | Effective traits | Structural operation |
| --- | --- | --- |
| `Clone` | `Clone` | Fieldwise or payload `clone` |
| `Copy` | `Copy`, `Clone` | Raw-value Copy eligibility and the same structural `clone` |
| `Eq` | `Eq`, `PartialEq` | Structural `eq` |
| `Ord` | `Ord`, `PartialOrd`, `Eq`, `PartialEq` | Lexicographic `cmp` and the same structural `eq` |
| `Hashable` | `Hashable`, `Eq`, `PartialEq` | Structural `hash` and the same structural `eq` |

Expand transitive supertraits from the resolved trait declarations, so a
future standard trait hierarchy change updates the effective closure. The
current `std.casa` still gives `Hashable` a `Word` supertrait, which
[ADR-0022](0022-word-is-not-a-public-trait.md) removes in the target. Standard
default methods, including `ne` and `partial_cmp`, remain trait-owned bodies.
They do not need duplicate structural operations. Reject a repeated source
derive name. For distinct requests on the same receiver pattern, merge an
effective trait only when each shared required method has the same structural
operation kind and field or payload order. The five current requests meet
that test for shared `clone` or `eq`. Allocate one conformance identity per
effective trait and one semantic operation per shared method. Its availability
is the logical OR of the contributors' eligibility conditions. Each request
keeps its own required field bounds.
For example, separate `Eq` and `Ord` requests allow Eq when the fields meet
Eq's requirements, while Ord still requires Ord-capable fields. If a future
pair prescribes different behavior for one effective method, reject it at the
derive items instead of choosing by source order.

An explicit implementation conflicts when its receiver pattern intersects a
derived receiver pattern and its effective trait closure or method set
intersects the derived closure. Do not exempt a concrete explicit
implementation because a conditional generic derive might be unavailable
for that concrete binding. A derived conformance is complete and cannot
accept a partial explicit override. Conditional requirements are checked when
a concrete use needs them, with the first ineligible field or payload path in
the diagnostic. Distinct applicable trait defaults remain ambiguous at the
call unless an accepted explicit override resolves them. The implementation
records identity, not synthetic method text.

Specialization settles concrete bounds, Copy and Clone behavior, moves,
destruction, selected trait and method targets, and all reachable semantic
instances. Each cleanup action names its selected drop behavior before
commit. A structural derive operation names its semantic fields or payload
and chosen member operations, while the backend later supplies their offsets
and machine form. The backend cannot select a different drop hook or repeat
trait resolution. `size_of` remains a typed query for target planning.

The private checked-program constructor verifies that every reachable body
and referenced declaration exists, every type and constant binding is
concrete, every constrained dispatch and cleanup target is settled, and every
structured exit has its checked transfers and cleanup. The constructor
publishes no partial result. Target-specific layout and ABI rejection keeps
the originating call or reference and binding chain for diagnostics.

## Diagnostics and test seam

Definition-time body, ownership, missing-bound, and cycle errors cite source
definitions. Concrete bound, width, derive, or target-selection errors cite
the call or reference and show the declaration and binding chain as related
context. Borrow errors preserve origin and later-use ranges. Internal
invariant failures retain accumulated diagnostics and exact source text.
Generated names and operation IDs are never the primary attribution.

Focused tests enter through the production syntax, analysis, and assembly
operations. They cover one final receiver driving both dispatch and
dependencies, branch-result transfer before cleanup, non-continuing joins,
loop exits, disjoint field loans, opaque returned origins, repeatable closure
captures, recursive callable summaries, input trait inference, forwarded
constant widths, unused generic errors, recursion through non-generic
intermediates, same-key reuse, `Eq`/`Ord`/`Hashable` and `Copy`/`Clone`
deduplication, conditional derive requirements, effective explicit overlap,
defaults,
concrete drop selection, and source attribution. Internal tests may exercise
the private summary fixed point and checked-program constructor directly.
They do not preserve wrappers solely to mirror handler dispatch. The
blueprint's executable slice and performance measures remain with
[Blueprint validation](https://github.com/frendsick/casa/issues/651).

Migration removes `SemanticSession` clone/commit modes and their store copies,
function phase flags, synthetic default and derive functions, cloned generic
declarations, operation-ID ownership transfer, encoded origin strings and
parallel metadata, per-handler duplicate operation interpretation, and
typechecker forwarding wrappers whose only role is to expose those internals.
The source-level checks, target planning, and any necessary private traversal
remain. Relocated code is not counted as deleted code.
