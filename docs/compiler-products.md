# Compiler request products

`compiler/products.casa` introduces the request boundary for the Compiler
Capsule migration in #700. Existing CLI, formatter, and language-server callers
retain their current interfaces until their migration slices.

The public operations are:

| Operation | Input | Successful return |
| --- | --- | --- |
| `syntax` | One `SourceUnit` | Tokens, optional structural facts, and a report |
| `analyze` | `CompilationInput` | An independent analysis snapshot with a report and semantic tokens |
| `assembly` | `CompilationInput` and `Target` | `Produced(report, source)` or `Rejected(report)` |

Each operation returns `std::Result` with `CompilerFailure` as its error type.
Source errors are ordinary products. Syntax rejection has no structural facts.
Assembly rejection cannot contain assembly. A compiler failure retains the
report accumulated before failure, its phase, message, and optional location.
The current adapters report bytecode failures and invalid request bases.
Process termination and allocation failure are outside this error contract.

`CompilationInput` owns root text, its file identity, an absolute base directory,
ordered library paths, and source overrides. Relative file identities, library
paths, and override keys resolve against that base. The compiler does not change
the process directory. The root text takes precedence over an override for the
root. Import overrides take precedence over disk text. Imports retain Casa's
existing path-style and module-style search rules.

Reports expose shared borrows through `get_diagnostics` and `get_sources`.
They retain the root and loaded import text, including imported source that
caused rejection. `SourceStore::get_bytes` returns an owned copy of exact bytes,
including imported files rejected for invalid UTF-8. Text lookup returns no
value for undecodable files. Unused overrides are removed after analysis. Diagnostic order
comes from the existing front end. Each request has its own source store and
compiler state. Keeping one result alive cannot affect another request.

Assembly currently supports `Target::LinuxX86_64`. `AssemblySource` exposes its
target and text through shared borrows. The caller writes the text and runs
native tools. Assembler, linker, and launch failures do not consume or change
the report.

The legacy parser, checker, document projector, and emitter are temporary
implementation adapters. Analysis releases their operations and declarations
after projecting semantic tokens. Its snapshot cannot produce codegen input.
Full editor queries and verified partial-fact guarantees belong to #713 and
#714. Shared root syntax and formatter migration belong to #701. Final consumer
cutover belongs to #715. This slice does not change language behavior or claim
the completed redesign's performance gains.

`tests/compiler/test_products.casa` checks independent overrides, release order,
source rejection, syntax products, retained failure context, and native output
from requested assembly. The private-field fixture checks the product boundary.
