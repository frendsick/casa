# Initial compiler consumer cutover

Historical evidence from 2026-09-29 at `5e9eb25`.

This is a partial implementation of [#715](https://github.com/frendsick/casa/issues/715).
It moves production consumers onto typed products and hides machine construction.
It does not complete the accepted Compiler Capsule representations or waive their
performance gates. The issue and pull request remain open while the native
prerequisites and acceptance gates remain unmet.

The CLI now requests assembly and passes its target-tagged result to the native
build adapter. The language server stores an `AnalysisSnapshot` and reads its
report and owned query answers. It no longer retains `AnalyzedDocument` or a
compiler body. The formatter already used the syntax product. Tests now request
assembly instead of constructing public `Program` or `InstValue` values. Direct
parser and semantic algorithm tests remain until their underlying seams move.

`analysis.casa` and the document wrapper are removed. `legacy_parser.casa` becomes
`source_builder.casa`. Machine types, lowering, and rendering are private in
`backend.casa`. That consolidation relocates implementation. It does not delete
the aggregate pipeline or make its repeated decisions disappear.

## Checked boundary and remaining design

Commitment owns the checked operation/store pair behind private fields and a
private direct constructor. It rejects source errors, unresolved operation
forms, symbolic operation facts, absent required hints, missing explicit and
implicit callees, unchecked destructor bodies, and selected invalid local,
capture, field, and variant references. Planning borrows the owned product.
Corruption tests use the commitment function directly for the new seal checks.

The public commitment function still accepts mutable legacy checking state.
Scoped bindings can retain unknown word-slot types. Specialized lambdas can
retain contextual return variables. Selected invariant checks do not establish
a fully concrete, target-neutral semantic product. Backend layout and ABI
planning still read declaration metadata.

The following capabilities remain separately scoped work:

- [Phase-valid source and semantic bodies](https://github.com/frendsick/casa/issues/732):
  structured bodies, concrete binding and callable metadata, private work states,
  and retirement of public operation/store and take/restore protocols.
- [Structured aggregate backend planning](https://github.com/frendsick/casa/issues/733):
  completed storage and dispatch plans, a reachable-function worklist, validated
  machine references, and one completed function buffer at a time.
- [Target-neutral checking and report presentation](https://github.com/frendsick/casa/issues/734):
  physical ABI policy outside source checking and shared diagnostic presentation
  including codes and ordered notes.

## Maintained source and caller obligations

Physical line counts exclude generated assembly and executables. The comparison
checkpoint is `47ed353`. Its compiler sources equal the original `50c6475`
preflight checkpoint because subsequent changes were documentation only.

| Source group | Checkpoint | Cutover | Difference |
| --- | ---: | ---: | ---: |
| `compiler/*.casa` | 43,313 | 43,753 | +440 |
| CLI, LSP, formatter | 4,357 | 4,365 | +8 |
| Compiler tests and fixtures | 27,096 | 24,250 | -2,846 |

Counts use physical lines in tracked `.casa` files, including new files and
excluding deleted files. Compiler code grows at this checkpoint because the
transitional commitment checks coexist with the legacy semantic representation.
No runtime source moves in this cutover. The historical compiler audit reported
40,192 lines at `1e89fb5`. The current compiler is 3,561 lines larger. That total
includes language and behavior migrations since the historical audit and cannot
be attributed to this architecture change alone.

| Consumer | Checkpoint knowledge | Cutover knowledge |
| --- | --- | --- |
| CLI | Analysis input/result, diagnostic gate, operation/store pair, bytecode product, renderer, native build | Compilation input, assembly rejection or production, retained report, outer failure, native build |
| LSP | Analysis input/result and document-body wrapper | Snapshot, report, owned query answers, version and source-position conversion |
| Formatter | Syntax product and equivalence check | Same syntax product and equivalence check |
| Assembly tests | Public machine constructors and instruction variants | Source input and complete assembly, or the private commitment constructing interface |

There are three typed request operations. Their distinct return types remove
request/product tag agreement and public phase ordering. Source rejection and
outer compiler failure remain distinct. Snapshots still need an owner until
position conversion ends, and query results still need a document version check.
These are retained obligations, not deletion claims.

Backend trait satisfaction, numeric defaulting, type-name interpretation,
destructor specialization, flat control-flow validation, and repeated instruction
family dispatch remain in the aggregate path. Source declaration elaboration can
still call semantic work. This cutover does not claim fewer repeated semantic
decisions in those paths.

## Validation and measurements

Focused source-product tests preserve program-specific allocation and field
stores, disjoint array/scalar frame slots, inline copies without allocation,
typed indirect copies, reachable-only and once-per-request function emission,
repeatable assembly from the same owned checked input, SSE2 rounding, the
32/33-word frame-initialization boundary, and forwarding-frame preservation.
Assembly-region helpers exclude embedded runtime text from those assertions.
Behavior, ownership, ABI, runtime, formatter, and bootstrap suites remain.

The final `tests/test_all.sh` run passed all 14 shards. The earlier runs passed
7/14 and 13/14 shards. Their failures exposed missing inferred reordering hints,
a stale test call, and discarded contextual method-return bindings. Focused
checks and the complete suite passed after the corrections. The parser example
and installed native build also passed. Both control and candidate bootstrapped
from v1.53.0 and reached matching stage-two/stage-three assembly.

The frontend lifetime fixture also exposed an inverted completion range for a
generated cleanup binding. Projection now skips that range before unsigned
subtraction. A source-product test preserves successful analysis and complete
editor facts for the affected generic destructor input.

### Serial compiler measurements

Measurements ran on 2026-09-29 UTC on the reference Ryzen 7 3700X, Linux x86-64
under WSL2, with GCC 11.4.0 and GNU assembler 2.47.20260726. Each configuration
used one unmeasured warm-up and three measured runs. Configuration order reversed
between cycles. No builds or tests ran concurrently with timing. Raw-clock wall
time includes native assembly and linking.

Common source is the complete compiler and library corpus archived at `47ed353`.
It includes the compiler's generic-heavy collection and semantic workloads.
Both fixed-point compilers accepted every input in that corpus. Candidate own
source is this cutover. Control own source equals the common-source corpus.
Common-source and changed-source results remain separate.

| Compiler and source | Raw wall samples, seconds | Median | Range | Peak RSS samples, MiB |
| --- | --- | ---: | --- | --- |
| Control, common/own | 12.495, 12.440, 12.707 | 12.495 | 12.440–12.707 | 349.281, 349.281, 349.156 |
| Candidate, common | 12.581, 13.106, 12.758 | 12.758 | 12.581–13.106 | 350.559, 350.434, 350.559 |
| Candidate, own | 12.612, 13.134, 12.995 | 12.995 | 12.612–13.134 | 358.934, 358.934, 358.934 |

The own-source median misses the 10-second gate. Every run stays below the
1 GiB RSS ceiling. Common-source median time increases by 0.263 seconds, or
2.1%. Paired candidate differences are +0.086, +0.666, and +0.051 seconds.
The observed ranges overlap, and the second candidate run varies more than the
control. The candidate is slower in all three pairs. These results establish no
speed gain or accepted regression tolerance. Explicit evidence review and a new
accepted decision remain necessary before completing a missed final gate.

Common-source assembly is byte-identical across compilers, at 29,170,407 bytes.
Its SHA-256 is `13873903e0a57f8558cf424c2f02e97272c6a86a3f2cd47f28afb10329ceaaac`.
Executables are 6,312,592 bytes, with matching `.text`, `.data`, and build ID.
Whole executable hashes differ because GCC records a random temporary object
filename in the non-loaded symbol string table. Candidate own-source assembly
is 29,653,424 bytes and its executable is 6,411,272 bytes. That increase includes
the additional transitional commitment checks and changed compiler source.

### Repeated request lifetimes

Each workload sampled 31 stopped-process boundaries. After initialization and
five warm-up batches, all 25 remaining samples matched. Each batch releases its
request products. The editor workload retains an owned hover answer while
replacing its snapshot. The workspace workload retains reference ranges while
replacing snapshots and rejecting incomplete renames.

| Workload | Live blocks/payload after warm-up | Reusable payload, bytes | Mapped heap, bytes | Plateau RSS, KiB |
| --- | --- | ---: | ---: | ---: |
| Frontend success and rejection | 0 / 0 | 130,712 | 67,108,864 | 4,192 |
| Scalar, aggregate extern, and rejected assembly | 0 / 0 | 276,072 | 67,108,864 | 4,940 |
| Repeated editor edits and queries | 0 / 0 | 55,944 | 67,108,864 | 4,088 |
| Workspace references and rename | 0 / 0 | 54,232 | 67,108,864 | 4,236 |

The allocator retains reusable blocks and a 64 MiB mapped chunk. Its live set and
RSS plateau. Mapped capacity does not equal resident memory or retained products.

[Raw evidence](evidence.json) contains host and toolchain
identity, source counts and patch hash, exact commands, bootstrap and compiler
hashes, clock readings, phase logs, every timing/RSS sample, output hashes and
sizes, all lifetime samples, and local validation logs. It also retains the
superseded timing pass and the failed lifetime attempt with their dispositions.
The superseded pass preceded the binding-scope and method-return fixes. Final
gate results use the final-source samples above. These measurements do not
complete the accepted representations or waive a gate.

The non-blocking Standards finding ST006 remains separate work. A few scalar
instruction assertions search complete assembly and can match runtime mnemonics.
Native scalar fixtures cover behavior. A generated-region helper would make
those specific selection assertions stronger.

The historical [control-flow target benchmark](../control-flow-targets.md) is pinned
to its original source revision. Its old bytecode-only figures cannot be compared
with the current `.casa` workloads, which measure complete assembly requests.
