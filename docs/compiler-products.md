# Compiler products

`compiler/products.casa` provides three operations. Each request owns its
source text, diagnostics, and result. The CLI requests assembly. The language
server and workspace queries request editor analysis. The formatter requests
syntax without loading imports or checking types.

| Operation | Input | Result |
| --- | --- | --- |
| `syntax` | One `SourceUnit` | Tokens, usable syntax facts when available, and a report |
| `analyze` | `CompilationInput` | A report and source-oriented editor index |
| `assembly` | `CompilationInput` and `Target` | Complete assembly with a report, or source rejection with a report |

`CompilationInput` supplies the root text and identity, an absolute base
directory, ordered library paths, and source overrides. Relative paths resolve
against that directory. Root text takes precedence over an override with the
same identity. Reports retain the exact sources used, including rejected bytes.
Unused overrides are released when the request finishes.

Source errors remain in the report. An analysis snapshot can retain partial
facts. Query answers distinguish known facts, absence, incomplete coverage,
and unavailable coverage. Source rejection cannot produce backend input.
A reportable internal failure returns `CompilerFailure` with the accumulated
report, phase, message, and any available source location.

Editor queries borrow a snapshot and return owned presentation values. Keep the
snapshot until source positions have been converted, and check document versions
before applying results to newer text. Replacing a snapshot releases its index
and sources. Editor products retain no compiler bodies or declaration store.

The checker commits an owned `CheckedProgram` before target planning. Its
fields and direct constructor are private. The public commitment function still
accepts mutable legacy checking state and verifies selected invariants. Scoped
locals can retain unknown word-slot metadata, and specialized lambdas can retain
contextual return variables. The backend still reads declaration metadata for
layout and ABI planning. This transitional seal does not establish the accepted
fully concrete, target-neutral checked representation. The backend borrows this
product and publishes complete assembly. Machine instructions, pools, lowering builders, and renderers stay
private to `compiler/backend.casa`. Assembly and native build failures are
separate. `compiler/build.casa` invokes the native assembler and linker.

The [cutover evidence](benchmarks/compiler-cutover.md) records measurements,
retained coverage, and remaining design work. Consumer migration alone does not
establish acceptance of the full Compiler Capsule redesign.
