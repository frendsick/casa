# Generics specialize after symbolic checking

related issue: [Choose the generic checking and specialization contract](https://github.com/frendsick/casa/issues/661).

The maintainer accepted this contract on 2026-09-26. Casa checks each generic
body once against its declared bounds, then caches
reachable concrete specializations. This retains early definition errors and
direct trait calls without hidden runtime type descriptors or dictionaries.
Distinct bindings can increase compilation cost and emitted size.

This record extends [ADR-0069](0069-generics-are-monomorphized-after-one-body-check.md)
for the Compiler Capsule blueprint. Production migration remains pending.
The semantic representation and specialization algorithms belong to
[Choose the semantic-analysis and specialization seams](https://github.com/frendsick/casa/issues/648).

## Checking and inference

Every generic declaration admitted to source checking receives its symbolic
body check, even when unused. Import selection remains a separate contract.
The check uses symbolic type and constant parameters, the
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
parameter's constraints. This preserves constant forwarding without deciding
which additional constant expressions the language accepts. That surface and
layout-dependent constants remain with
[Choose the compile-time evaluation surface](https://github.com/frendsick/casa/issues/657).

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

The semantic seam owns cycle traversal, checked recipes, identity keys, cache
lifetime, and concrete obligation resolution. The existing blueprint validation
ticket owns executable evidence. No new implementation workstream is created.

## Current evidence and migration gaps

Evidence is pinned to `62a46a68929ef20860f8a35f49b40ab5c0093949`, the worktree's
`origin/main` base on 2026-09-26. These are source and test inspections, not new
execution results.

- [Recursion tests](https://github.com/frendsick/casa/blob/62a46a68929ef20860f8a35f49b40ab5c0093949/tests/compiler/test_traits.casa#L529) already accept
  unchanged bindings and reject a finite type swap through a function reference.
- [Generic binding and cycle checking](https://github.com/frendsick/casa/blob/62a46a68929ef20860f8a35f49b40ab5c0093949/compiler/semantics.casa#L12169) compose
  type and constant bindings. Current cycle traversal stops at non-generic
  callees, and its diagnostic omits the complete cycle and changed bindings.
  The target contract requires both gaps to be addressed.
- [Input trait inference](https://github.com/frendsick/casa/blob/62a46a68929ef20860f8a35f49b40ab5c0093949/tests/compiler/test_traits.casa#L1190) includes
  `I: Carrier[T]`, where the input's implementation supplies `T`. Parser type
  hints used by generic calls include explicit associated arguments such as
  `List[i64]::new`. They do not establish inference from later output context.
- [Constant argument matching](https://github.com/frendsick/casa/blob/62a46a68929ef20860f8a35f49b40ab5c0093949/compiler/semantics.casa#L3543)
  checks concrete widths but returns early for symbolic values.
  [Explicit reference validation](https://github.com/frendsick/casa/blob/62a46a68929ef20860f8a35f49b40ab5c0093949/compiler/semantics.casa#L7513)
  checks that a binding is a constant without checking its width there.
  Uniform checks for references and forwarded constants need focused migration
  coverage. These missing checks are code-inspection findings, not demonstrated
  acceptance of invalid programs.
- [Specialization naming](https://github.com/frendsick/casa/blob/62a46a68929ef20860f8a35f49b40ab5c0093949/compiler/semantics.casa#L12434)
  currently hashes formatted bindings into function names. The backend still
  [selects drop hooks](https://github.com/frendsick/casa/blob/62a46a68929ef20860f8a35f49b40ab5c0093949/compiler/bytecode.casa#L679).
  Canonical semantic identity, complete source attribution, and semantic target
  resolution before backend entry remain migration requirements.
- The [semantic audit](../benchmarks/semantic-analysis-complexity.md) identifies
  cloned operation trees and ownership-metadata transfer as implementation
  costs. This decision retains generic semantics without requiring that
  machinery.
