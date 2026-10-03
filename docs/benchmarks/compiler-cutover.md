# Compiler consumer cutover

Evidence for [#715](https://github.com/frendsick/casa/issues/715). The candidate
integrates the completed source-body, backend, extern-checking, and diagnostic
prerequisites with the production consumer cutover.

The CLI requests `products::assembly` and passes the target-tagged assembly to
the native driver. The LSP retains `AnalysisSnapshot` and uses its report,
source positions, and owned query answers. The formatter requests syntax.
Workspace queries already used snapshots. None of these consumers coordinates
parser, checking, commitment, or backend phases.

`analysis.casa` and `AnalyzedDocument` are removed. `source_builder.casa` owns
the former parser implementation. `backend.casa` owns structured planning,
private machine instructions, selection, rendering, and spelling helpers.
The backend accepts a borrowed `CheckedProgram`. The wrapper that accepted a
mutable checking result is removed. Direct parser, checker, and sealed-backend
tests remain at their algorithm boundaries. Private spelling and invalid-machine
checks run inside the backend fixture.

The [initial cutover](compiler-cutover/initial.md) records the earlier draft and
its then-open prerequisites. Its measurements describe `5e9eb25`, before the
prerequisites merged. They do not describe the current candidate.

## Interfaces and ownership

Three typed operations publish independent products. There is no request/result
tag agreement or caller-managed compiler phase ordering. `CompilationInput`
owns source text, root identity, an absolute base directory, library paths, and
overrides. Source rejection returns a report. Internal failure retains the
report, phase, and message. Assembly and native build failures remain separate.

The LSP keeps snapshots until location conversion finishes and checks document
versions before applying query results. Owned answers survive snapshot release.
These obligations remain necessary. The deleted document wrapper added a second
owner protocol without adding query information.

Structured source and semantic bodies, private checking registries, concrete
commitment, and one completed function plan at a time come from the merged
prerequisites. Checking selects copy behavior, drop hooks, and call targets.
Backend planning selects storage and physical ABI behavior. Selection consumes
those plans. This consumer cutover adds no new semantic decision pass and
relocates no runtime assets.

## Maintained source

Counts are physical lines in tracked Casa files. Compiler tests include their
Casa fixtures. The private backend `.inc` fixture is shown separately.

| Source group | Historical `1e89fb5` | Initial base `47ed353` | Control `6732f0f` | Candidate `f18cc64` |
| --- | ---: | ---: | ---: | ---: |
| Compiler | 40,192 | 43,313 | 46,299 | 46,262 |
| CLI, LSP, formatter | 3,920 | 4,357 | 4,405 | 4,403 |
| Compiler tests | 25,853 | 27,096 | 24,757 | 24,791 |
| Private backend fixture | 0 | 0 | 42 | 67 |

The final consumer cutover removes 39 production lines against its merged
control. Compiler tests grow by 34 lines, and 25 spelling-test lines move into
the private fixture. Combined production source remains 6,553 lines above the
historical audit and 2,995 above the initial cutover base. These totals include
the intervening source, semantic, backend, and language changes. There is no
adjusted checkpoint that isolates the entire architecture migration.

## Compiler measurements

Measured on 2026-10-03 on the reference Ryzen 7 3700X under WSL2 Linux x86-64,
with GCC 11.4.0 and GNU binutils 2.47.20260726. Control `6732f0f` and candidate
`f18cc64` were built from stable v1.53.0. Each reached byte-identical stage-two
and stage-three assembly. Measurements use the stage-three binaries.

The common source is the complete control compiler and its library, including
its generic collections and semantic workloads. Both compilers accept the same
corpus without exclusions. Control own source equals common source. Candidate
own-source results include its changed input and remain separate.

Each configuration has one warm-up and three measured runs. Configuration order
reverses between cycles. No builds or tests ran concurrently with timing.
CLOCK_MONOTONIC_RAW measures the complete command, including assembly and linking.
GNU time records peak RSS. Raw compiler progress logs are retained, but the
candidate's single assembly request no longer exposes the control's separate
analysis and backend intervals. Per-phase allocation traffic was not measured.

| Compiler and source | Wall samples, seconds | Median | Range | Peak RSS samples, MiB |
| --- | --- | ---: | --- | --- |
| Control, common/own | 16.957, 17.019, 17.120 | 17.019 | 16.957 to 17.120 | 397.426, 397.484, 397.484 |
| Candidate, common | 17.457, 17.562, 17.197 | 17.457 | 17.197 to 17.562 | 397.500, 397.500, 397.625 |
| Candidate, own | 17.112, 17.129, 17.170 | 17.129 | 17.112 to 17.170 | 412.500, 412.375, 412.375 |

Self-compilation misses the 10-second gate. Common-source median time rises by
0.438 seconds, or 2.6%. Every paired candidate run is slower, and the ranges do
not overlap. This comparison establishes a regression beyond observed variation.
Every run stays below the 1 GiB ceiling. Common-source memory is nearly unchanged.
Own-source memory is higher, with no speed gain, although that comparison includes
changed compiler input. On 2026-10-03, the maintainer accepted these measured costs for #715 under the
[accepted protocol](compiler-simplification-measurements.md). This decision covers
the 17.129-second self-build median, the common-source regression, and measured
memory use. The self-compilation target below 10 seconds remains required before
#699 closes. This acceptance does not change that parent gate.

Common-source assembly is identical across compilers and all samples:
34,012,442 bytes, SHA-256
`2d9807a92c5b7169ba55e8e084cd3273cda92220714e507345480d6a0f9e5966`.
Executables are 7,357,672 bytes with matching `.text` and `.data`. Whole ELF hashes
can differ because the linker records temporary object names. Candidate own-source
assembly is 34,113,918 bytes and its executable is 7,381,408 bytes.

## Request lifetimes

The retained frontend, backend, editor, and workspace workloads each record
31 stopped-process boundaries. Each batch releases its products. Editor queries
retain an owned hover answer across snapshot replacement. Workspace queries
retain reference ranges while replacing snapshots and rejecting incomplete
renames. All 25 samples after five warm-up batches match their workload's plateau.

| Workload | Live blocks / payload bytes | Reusable payload, bytes | Mapped heap, bytes | RSS, KiB |
| --- | --- | ---: | ---: | ---: |
| Frontend success and rejection | 0 / 0 | 137,448 | 67,108,864 | 4,844 |
| Scalar, aggregate extern, and rejected assembly | 0 / 0 | 244,120 | 67,108,864 | 5,640 |
| Editor edits and queries | 0 / 0 | 53,360 | 67,108,864 | 4,628 |
| Workspace references and rename | 0 / 0 | 52,536 | 67,108,864 | 4,684 |

The allocator retains reusable storage and one mapped 64 MiB chunk. Zero live
payload establishes release between requests. These samples do not measure peak
memory while snapshots overlap or per-function backend allocation traffic.

## Validation

The complete local suite passed 12 of 14 shards. The integration and generics
shards each found one stale call to the removed backend wrapper. After correction,
all 13 focused extern, generic, ABI, and related error checks passed. The analysis,
LSP, and private-constructor filters also passed. Both affected shard reruns passed with the fixed-point candidate: 6 generics
checks and 17 integration checks. All 14 shard paths have now passed locally.

The retained suites cover behavior, ownership, native ABI, runtime, examples,
formatter idempotency and safety, installed compilation, stable bootstrap, and
assembly fixed points. The generated-binding regression test exposed a scope
end before its declaration offset. Editor projection now skips that generated
range before unsigned subtraction.

The follow-up Standards and Spec reviews reported no unresolved findings. Scalar
instruction assertions now exclude embedded runtime text, resolving ST006.
The migrated control-flow timing driver overflowed at 1,000 nested blocks.
Its complete-request workload now uses 64, 128, 256, and 512 blocks, resolving
SP001. Both control-flow drivers fail on rejected source. All four timing
cases and 101 repeated 500-block requests passed. Seven focused compiler
filters and all 14 shards in the follow-up full local suite also passed.

[Raw evidence](compiler-cutover/final-evidence.json) records source revisions and
hashes, bootstrap commands and hashes, every warm-up and timing sample, retained
assembly and executable sizes, code/data hashes, all lifetime samples, and local
validation commands and logs. The existing measurement and lifetime scripts in
`docs/benchmarks` reproduce the individual commands. The initial failed test
calls are retained in the suite log and superseded by the focused checks.
