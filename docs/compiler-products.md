# Compiler request products

`compiler/products.casa` provides three request operations for the Compiler
Capsule migration in #700:

| Operation | Input | Successful return |
| --- | --- | --- |
| `syntax` | One `SourceUnit` | Tokens, optional structural facts, and a report |
| `analyze` | `CompilationInput` | An independent analysis snapshot with a report and semantic tokens |
| `assembly` | `CompilationInput` and `Target` | `Produced(report, source)` or `Rejected(report)` |

Each operation returns `std::Result` with `CompilerFailure` as its error type. Source errors
return ordinary products. Rejected syntax has no structural facts, and rejected
assembly has no assembly text. `CompilerFailure` retains the accumulated report,
phase, message, and optional location. Current failures cover bytecode errors
and invalid request bases. Process termination and allocation failure are excluded.

`CompilationInput` owns root text, its file identity, an absolute base directory,
ordered library paths, and source overrides. Relative file identities, library
paths, and override keys resolve against that base without changing the process
directory. Root text takes precedence over overrides for the root file. Import overrides take
precedence over disk text. Casa's path-style and module-style search rules apply.

Reports expose shared borrows through `get_diagnostics` and `get_sources`, with
diagnostics in front-end order and exact root and loaded import text, including
rejected sources. `SourceStore::get_bytes` returns an owned byte copy even for
imports rejected as invalid UTF-8, for which text lookup returns no value.
Completed analysis and assembly reports omit unused overrides. Requests own source stores and
compiler state, so retained results cannot affect other requests.

`AssemblySource` exposes a shared borrow of its text and identifies its target,
`Target::LinuxX86_64`. The caller writes and builds assembly and launches binaries.
Native failures do not consume or change the report.

Scalar checking uses one request-owned function state to distinguish active,
accepted, and rejected bodies. Operation handlers record dependencies from the
same selected target and receiver that they use to check the stack. The editor
projection includes only operation identities verified by that request, after
literal checks and recursive call obligations have settled.
A failed operation withholds facts that depend on its recovered stack. An
independent sibling branch or function can still contribute verified facts.
A return-signature error rejects assembly without erasing established calls.

Ownership checking keeps typed places with a resolved binding name and field
components. Origins distinguish access to a place from dependencies on an
input's ordinary or cleanup-observed origins. Each binding keeps both origin
sets and its diagnostic location in one record. Continuing branch completion
transfers results before checking LIFO cleanup and leaving the scope. Returns
attach cleanup to the exit and do not contribute to the continuing join.

Checked operations own their assignment, move, and cleanup actions. Ownership
actions are optional on operations that need none. Specialization substitutes
cleanup types within those operations, and bytecode lowering reads the actions
directly. Pending moves from stack values remain private to the body checker
until it attaches them to their source operations. There is no ownership table
in `SymbolStore`.

The parser, checker, document projector, and emitter remain temporary adapters.
Analysis releases their operations and declarations after projecting semantic
tokens and cannot produce codegen input. Existing consumers retain their current
interfaces. Shared root syntax and formatter migration belong to #701, full
editor queries and verified partial facts to #713 and #714, and final consumer
cutover to #715. Language behavior is unchanged. The [measurements](benchmarks/compiler-products/README.md)
do not establish final redesign performance.

`tests/compiler/test_products.casa` checks independent overrides, release order,
source rejection, syntax products, retained failure context, and native output
from requested assembly. The private-field fixture checks the product boundary.
