# Compiler Capsule final acceptance

The final change for [#699](https://github.com/frendsick/casa/issues/699) reaches
a 9.658-second self-compilation median on the reference host. Every candidate
run stays below 308 MiB peak RSS. The common-source median falls from 15.583 to
9.751 seconds, a 37.4% reduction, with lower memory use in every measured pair.
The broader constant findings remain separate work in
[#744](https://github.com/frendsick/casa/issues/744),
[#745](https://github.com/frendsick/casa/issues/745), and
[#746](https://github.com/frendsick/casa/issues/746).

## Scope and retained contracts

The 16 implementation children are merged. The final change removes surviving
review leftovers and reduces allocation, cloning, repeated lookup, and emitted
instruction costs. Empty Lists and Maps allocate backing storage on first
insertion. String construction and byte copying use bounded direct operations.
Checking borrows existing declarations and origin sets, and uses registered
member-accessor and receiver-indexed trait metadata.

The backend still plans and validates one complete function before rendering.
It renders that physical plan into private assembly without a second buffer of
rendered lines. A request-owned registry maps complete generated identities to
short assembler labels. Bounded leaf expansion, register permutation, folded
field loads, and direct comparison branches reduce native work. Removed frames
retain their capacity requirements. Standalone wrappers with nested calls keep
physical storage so indirect calls preserve return-stack overflow behavior.

The public boundary remains three typed operations: syntax, analysis, and
assembly. This change adds no caller-visible concept, result variant, phase
ordering rule, or ownership obligation. Source errors remain reports, internal
failures retain phase and diagnostics, and native build failures remain separate.
The CLI and editor consumers depend on products without coordinating internal
compiler phases. Checking still selects semantic behavior. Backend planning
consumes those decisions and fixes physical storage. This final change moves no
runtime assets. The earlier redesign extracted the 613-line `compiler/runtime.casa`
module, which remains included in the compiler totals.

## Maintained source

Counts are physical lines. Generated output is excluded. The private backend
fixture is separate from Casa tests.

| Source group | Historical `1e89fb5` | Control `2e7b231` | Candidate |
| --- | ---: | ---: | ---: |
| Compiler | 40,192 | 46,262 | 46,384 |
| CLI, LSP, formatter | 3,920 | 4,403 | 4,395 |
| Workspace consumer | 0 | 616 | 616 |
| Standard library | 5,668 | 5,703 | 5,732 |
| Compiler tests | 25,853 | 24,792 | 24,916 |
| Private backend fixture | 0 | 67 | 79 |

Production source grows by 143 lines against the merged control and by 7,347
against the historical audit when the library is included. This final change
adds native optimizations and focused behavior coverage while removing obsolete
paths. The historical difference includes intervening language and behavior
changes. There is no adjusted checkpoint that isolates architecture-only savings.
The blueprint sets no deletion quota. Raw evidence retains added and deleted
line counts separately.

## Compiler measurements

Measured on 2026-10-03 on the Ryzen 7 3700X reference host under WSL2 Linux
x86-64, with GCC 11.4.0 and GNU binutils 2.47.20260726. Both compilers were built
from stable v1.54.0 and reached byte-identical stage-two and stage-three assembly.
The measurements use stage-three binaries. Candidate code is `00d8be8`. The evidence also records exact input hashes.

The common source is frozen control `2e7b231`, including its complete compiler
and library. It exercises generic collections and semantic checking. Both
compilers accept the full corpus without exclusions. Control own source equals
common source. Candidate own source includes the changed compiler and library.

Each configuration has one warm-up and three measured runs. Order reverses in
the middle cycle. No builds or tests ran concurrently with timing.
CLOCK_MONOTONIC_RAW covers the complete native command, including assembly and
linking. GNU time records peak RSS. The final series has no failed runs or reruns.

| Compiler and source | Wall samples, seconds | Median | Range | Peak RSS samples, MiB |
| --- | --- | ---: | --- | --- |
| Control, common/own | 15.547, 15.583, 15.654 | 15.583 | 15.547 to 15.654 | 408.875, 408.500, 408.750 |
| Candidate, common | 9.751, 9.683, 9.771 | 9.751 | 9.683 to 9.771 | 296.219, 296.219, 296.219 |
| Candidate, own | 9.658, 9.687, 9.607 | 9.658 | 9.607 to 9.687 | 307.219, 307.219, 307.344 |

The self-build median passes the required threshold below 10.0 seconds. Every
measured run, including the control, stays below 1 GiB. Common-source ranges do
not overlap. The speed gain repeats in every pair while peak RSS falls by about
112 MiB. This comparison measures the complete retained change. It does not
attribute a separate gain to each optimization.

| Compiler and source | Assembly bytes | Executable bytes | `.text` bytes | `.data` bytes |
| --- | ---: | ---: | ---: | ---: |
| Control, common/own | 34,113,958 | 7,381,408 | 5,393,437 | 177,926 |
| Candidate, common | 24,044,496 | 5,528,992 | 4,458,786 | 177,926 |
| Candidate, own | 23,948,390 | 5,512,480 | 4,441,183 | 177,550 |

Assembly is deterministic across all four runs of each configuration. Evidence
retains assembly, executable, and code/data hashes. Whole ELF hashes can differ
between stages because temporary object names reach the symbol table.

Compiler progress logs expose the complete assembly request and native-driver
boundary. They do not separate source building, checking, planning, and rendering.
Per-phase allocation traffic was not measured. The updated wall sampler identifies
native call return addresses and completed a self-build with 1,004 samples.
Sampling stops add overhead, so that run is not a timing-gate sample.
[Exploratory samples](compiler-capsule-acceptance/exploratory.json) retain the
single-run development measurements, including timing misses. Their differing
inputs and order do not establish isolated optimization gains.

## Request lifetimes

Five workloads each record 31 stopped-process boundaries. All 25 samples after
initialization and five warm-up batches match their workload's plateau exactly.
Every plateau has zero live blocks and zero live payload bytes.

| Workload | Reusable payload, bytes | Mapped heap, bytes | RSS, KiB |
| --- | ---: | ---: | ---: |
| Frontend success and rejection | 104,488 | 67,108,864 | 3,976 |
| Scalar, aggregate extern, and rejected assembly | 193,664 | 67,108,864 | 4,664 |
| Editor edits and queries | 35,248 | 67,108,864 | 3,840 |
| Workspace references and rename | 34,088 | 67,108,864 | 3,936 |
| Native build success and tool failures | 1,232 | 67,108,864 | 60 |

The editor workload keeps an owned hover answer across snapshot replacement.
The workspace workload keeps references across replacement and rejects incomplete
renames. The allocator retains reusable storage and one mapped 64 MiB chunk.
These observations establish release between requests, not peak memory while
products overlap or per-function allocation traffic.

## Bootstrap and validation

Stable v1.53.0 rejects valid payload-free borrowed Option returns and branch-local
borrow lifetimes used by this change. The rejection was reproduced after formatting.
The required stable [v1.54.0 release](https://github.com/frendsick/casa/releases/tag/v1.54.0)
was built from merged control `2e7b231`. Its release workflow passed. Both downloaded
assets were verified, and `casa-release.env` now selects that release.

The complete local suite ran all 14 shards. Twelve passed. The integration and
language-value shards each stopped at a stale test expectation. The product test
now uses the current internal module prefix. Enum code-generation tests now check
direct branches, the print call and cleanup, and the current scalar registers.
All three affected focused checks pass after these test-only corrections. The
full run includes passing self-hosting, fixed-point, ownership, ABI, runtime,
example, formatter, CLI, and installed-compiler checks. The compiler and library
inputs are unchanged from the measured builds.

The focused return-stack fixture first reproduced a missing overflow in indirect
wrapper calls. The corrected path passes both the last valid depth and first
overflowing depth. Destruction, emitter, `typeof`, and existing stack-boundary
checks pass. Fresh Standards and Spec reviews found no remaining findings after
the correction and cleanup. The Standards review includes the required Casa
argument-order inventory.

## Child review decisions

The maintainer selected fixing the surviving small cleanups and assessing broader
constant work separately. The final dispositions are:

| Origin | Disposition |
| --- | --- |
| #717, #718 | Removed unreachable parser fallback, redundant initialization, and owned resolved-name clones. The old cloned variable-key path was already gone. |
| #721 | Corrected the stale `global` diagnostic and replaced constant-map key copies with direct lengths. Deferred independent-error recovery to #744, declaration classification to #745, and parsed-constant evaluation with cursor snapshots to #746. |
| #723 | Removed obsolete editor helpers and unused completion setup. |
| #724, #725 | Removed redundant aliases and callable-set clones. Source-body revisits were already replaced by owned work states and stored return summaries. |
| #726, #727 | Removed the forced references Option wrapper and corrected the equality-test label. |
| #728 | The scalar fallback clone path was already removed. Function planning now borrows the checked declaration. |
| #742 | Removed unused backend parameters and the unused `typeof` string-table helper. |

The later editor and workspace lifetime workloads cover the edits and queries
missing from the earlier #721 workload. The three constant follow-ups retain
the bounded constant language, source order, visibility, and typed-value rules.
They do not add user-function compile-time evaluation or layout queries.

[Raw acceptance evidence](compiler-capsule-acceptance/evidence.json) contains
bootstrap provenance, source hashes, all warm-up and measured samples, output
sizes and hashes, lifetime samples, validation logs, and reproduction commands.
