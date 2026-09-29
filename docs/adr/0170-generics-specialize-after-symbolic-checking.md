# Generics specialize after symbolic checking

related issue: [Choose the generic checking and specialization contract](https://github.com/frendsick/casa/issues/661).

Casa checks each generic
body once against its declared bounds, then caches
reachable concrete specializations. This retains early definition errors and
direct trait calls without hidden runtime type descriptors or dictionaries.
Distinct bindings can increase compilation cost and emitted size.

This record extends [ADR-0069](0069-generics-are-monomorphized-after-one-body-check.md)
for Compiler Capsule. Checked recipes are implemented, while the full
target-neutral boundary and consumer cutover remain incomplete. Algorithms follow
[Choose the semantic-analysis and specialization seams](https://github.com/frendsick/casa/issues/648).

## Checking and inference

Every generic declaration admitted to source checking receives its symbolic
body check, even when unused. Imports follow ADR-0168. The check uses symbolic type and constant parameters, the
declared stack effect, and declared bounds. It validates control flow,
ownership, borrowing, returned values, and required operations. A missing bound
is a definition error. Specialization must not repeat this body check.

Direct calls infer bindings locally from consumed inputs, receivers, and their
declared trait constraints. Repeated occurrences of a parameter must agree.
Explicit arguments constrain or supply bindings. The compiler does not infer
missing bindings from later assignments, return contexts, or subsequent calls.
A parameter determined by an input's trait implementation is input-driven even
when it does not occur directly in that input's type. A parameter available
only from output context requires an explicit argument.

Named generic function references supply every type and constant argument.
`&id[i64]` produces one monomorphic function value. An enclosing generic can
forward its symbolic parameters, as in `&id[T]`. Each enclosing specialization
then has a concrete reference. Later indirect calls do not determine its type.
This retains [ADR-0042](0042-generic-function-references-are-explicitly-specialized.md).

Check trait bounds and constant-argument types at the call or reference.
Concrete integer constants must fit the declared width. Forwarded constant
parameters remain symbolic until substitution and must satisfy the receiving
parameter's constraints. The accepted expression surface and layout exclusions follow
[ADR-0171](0171-constants-use-bounded-target-independent-expressions.md).

## Reachability and recursion

Reachability starts at ordinary root execution. Follow calls, function
references, closure behavior, and required concrete clone, drop, derived, and
default-method behavior. Runtime globals are absent under
[ADR-0165](0165-runtime-state-is-owned-by-the-root-body.md). Checked but
unreachable generic declarations emit no machine code.

Reuse one semantic specialization for each canonical declaration identity,
relevant trait implementation identity, and complete type and constant binding
set. Different implementation contexts must not collide. Repeated uses of the
same key reuse the same instance. This is a semantic identity contract, not a
prescription for cache storage or generated symbol names.

Ordinary generic recursion is valid. Within an active dependency chain, every
revisit of the same generic declaration must retain its complete bindings.
Validate composed bindings across mutual recursion, constant parameters,
function references, and intervening non-generic functions. Apply the rule to
unused declarations as part of declaration validation too.

Reject binding-changing recursion even when its specialization family is
finite. For example, `f[A, B]` referring to `f[B, A]` violates the rule. The
reason is a predictable recursion contract, not a claim that every changing
cycle grows without bound. Independent calls to `f[i64]` and `f[str]` remain
valid. The restriction applies to recursive revisits, not all uses of a
declaration across the program.

## Semantic specialization and target planning

Concrete specialization resolves bound satisfaction, Copy eligibility,
ownership transfers, destruction, structural derived behavior, and static
trait targets from the checked body. Trait defaults use this same path under
[ADR-0164](0164-trait-default-methods-are-trait-owned-generic-bodies.md).
Conditional concrete behavior cannot justify an operation missing its symbolic
bound.

Complete all required reachable semantic instances and dispatch before
constructing the target-neutral checked program. The backend owns physical
sizes, field offsets, storage, ABI placement, and machine operations under
[ADR-0167](0167-compiler-products-own-independent-snapshots.md). It may reject a
target-specific layout or ABI obligation. It must not rerun source-level
checking or weaken ownership rules.

| Failure | Diagnostic timing and attribution |
| --- | --- |
| Invalid symbolic body or missing bound | At the generic definition, including unused definitions. |
| Unsatisfied concrete bound or constant width | At the call or reference, with the generic declaration and binding chain as related context. |
| Binding-changing recursion | During generic cycle validation, naming the source cycle and changed type or constant bindings. |
| Target-specific layout or ABI restriction | During backend target planning, retaining the source call or reference, declaration, and binding context. |

Diagnostics use source names and bindings. Generated hashes and operation IDs
must not replace source attribution.

## Cost, migration, and validation

Make no exact binary-size guarantee. Measure generic-heavy compilation time,
peak memory, and emitted size on common source workloads. Keep self-compilation
and fixed-point validation separate from comparisons using a common workload.
The [checked-generics benchmark](../benchmarks/checked-generics.md) is historical
evidence of workload-dependent cost, not a prediction for the redesign.

Preserve generic syntax, accepted behavior, and diagnostic phase except for
separately accepted language changes and the validation gaps listed below.
Update reference documentation, examples, and executable tests with production
migration. Retain focused coverage for unused invalid bodies, missing bounds,
input and trait-driven inference, explicit references, same-binding recursion,
mutual and constant-changing recursion, symbolic constant forwarding and width
checks, concrete copying and destruction, and specialization reuse.

[ADR-0173](0173-semantic-checking-owns-source-obligations.md) owns cycle
traversal, recipes, identities, cache lifetime, and concrete obligations.

## Current implementation

[Checked-recipe measurements](../benchmarks/checked-recipes/README.md) document
the implemented recipe path. Cycle validation traverses non-generic callees and
reports the source cycle and changed bindings. These requirements remain part
of the accepted contract, not outstanding migration gaps.

The full target-neutral semantic boundary remains incomplete, with consumer
cutover in #715. The historical code-inspection findings remain in
[the original decision](https://github.com/frendsick/casa/blob/e9a258837d40185376c97e4fd03fccb98d238d82/docs/adr/0170-generics-specialize-after-symbolic-checking.md#current-evidence-and-migration-gaps).
