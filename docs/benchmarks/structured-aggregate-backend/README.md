# Structured aggregate backend

Evidence for [#733](https://github.com/frendsick/casa/issues/733), measured on
2026-10-03 under the [accepted protocol](../compiler-simplification-measurements.md).

The common-source median changes from 16.990 to 17.190 seconds. The ranges
overlap, and one of three candidate runs is faster than its paired control.
These samples do not establish a repeatable timing regression beyond observed
variation. Median peak RSS falls by 12.3%. Own-source median time falls by 2.9%
and median peak RSS by 10.0%. Both compilers remain above the final ten-second
target. The protocol permits this timing miss for intermediate #699 subissues.
Every measured peak is below the 1 GiB limit.

## Inputs and method

Control: `dc4b3500df4eb2607411a13e9dbb434fed96a1d6`.
Candidate: `be5d6bb767d0ef0cf95901fbf34e65a9275e6a83`.
Both compilers were bootstrapped with stable v1.53.0. Their stage 2 and stage 3
assembly is identical. Measurements use the stage 3 binaries.

The common-source workload compiles the complete control compiler and its
imports, including generic standard-library collections and compiler products.
Both variants use the same control source and library. The own-source workload
compiles each compiler's source with its corresponding library. No source was
excluded. Own-source results include the changed input and do not isolate
backend architecture costs.

Host: Ryzen 7 3700X, Linux 6.18.40.1-microsoft-standard-WSL2, x86-64, GCC 11.4.0,
GNU assembler and linker 2.47.20260726. Each workload has one warm-up per compiler
followed by three alternating control/candidate pairs. Builds run serially,
with fresh outputs and assembly retention. Timing includes assembly and linking.
The existing [measurement helper](../fixed-point-compilation/measure.py) reads
Linux `CLOCK_MONOTONIC_RAW` through a direct syscall. GNU time records peak RSS.

[measurements.json](measurements.json) retains every warm-up and sample, exact
commands, compiler hashes, source revisions, output sizes, clock readings, and
verbose phase logs. Phase timestamps use the compiler clock and cannot be added
to the raw-clock totals. Allocator traffic and peak live payload were not
instrumented. Request-boundary live allocations are measured below.

## Compilation results

| Workload | Control median (range), s | Candidate median (range), s | Control RSS median (range), MiB | Candidate RSS median (range), MiB |
| --- | ---: | ---: | ---: | ---: |
| Common source | 16.990 (16.964–17.243) | 17.190 (17.135–17.278) | 436.9 (436.9–437.1) | 383.0 (383.0–383.3) |
| Own source | 17.133 (16.637–17.187) | 16.638 (15.842–17.018) | 437.0 (436.9–437.0) | 393.3 (393.1–393.4) |

| Workload | Control assembly bytes | Candidate assembly bytes | Control executable bytes | Candidate executable bytes |
| --- | ---: | ---: | ---: | ---: |
| Common source | 34,608,029 | 34,621,160 | 7,501,336 | 7,501,336 |
| Own source | 34,608,029 | 33,893,373 | 7,501,336 | 7,330,624 |

All 16 compilation runs succeeded. Assembly hashes, executable sizes, and
`.text` and `.data` hashes are stable across each configuration's four runs.
Full ELF hashes differ because GCC assigns a fresh temporary object name to a
local `FILE` symbol. The control compiler built by the candidate also reproduces
the control's fixed-point assembly when it compiles the control source.

## Request lifetime and retained state

The existing [backend lifetime workload](../compiler-reductions/backend-lifetime.casa)
repeats successful scalar and aggregate/extern requests plus a rejected request
for 30 rounds. The [allocator inspector](../compiler-reductions/lifetime.py)
samples the process after initialization and each completed round.

| Plateau after warm-up | Control | Candidate |
| --- | ---: | ---: |
| Live blocks / payload bytes | 0 / 0 | 0 / 0 |
| Reusable blocks | 4,625 | 4,649 |
| Reusable payload bytes | 239,288 | 245,648 |
| Mapped heap bytes | 67,108,864 | 67,108,864 |
| RSS, KiB | 5,720 | 5,616 |

Every sample from round 6 through round 30 matches its variant's plateau.
Reusable storage belongs to the allocator. No backend allocation remains live
between requests. Function plans and selected text buffers are released after
each function, except bounded leaf templates retained for later call expansion.
That function lifetime follows the ownership path reviewed in the implementation.
It was not measured with a separate per-function allocator probe.

## Source and interface change

Production source adds 4,163 lines and removes 5,457, a net reduction of 1,294.
Tests add 279 lines and remove 2,728, a net reduction of 2,449. These counts include
moving physical instructions and selection into the backend module. No runtime
asset is relocated and no generated source is added.

| Maintained source | Historical baseline | Control | Candidate |
| --- | ---: | ---: | ---: |
| Compiler and direct consumers | 44,112 | 51,964 | 50,670 |
| Tests, shell runners, and private invariant fixture | 27,523 | 28,958 | 26,509 |

The historical baseline is `1e89fb524248ab4cb7bfc0e750e1213638a900b2`.
Direct consumers are the CLI, LSP, and formatter. Library source is outside this
count. The candidate remains 6,558 production lines above that historical
baseline and 1,014 test lines below it. Intervening behavior changes prevent
attributing those historical totals to this backend migration.

The backend exposes checked compilation and its failure result. Public machine
instructions, pools, and whole-program construction are removed. A request owns
function identities, three literal pools, native-call plans, and bounded leaf
templates. Three work-item variants cover checked functions, value destructors,
and closure destructors. Functions are scheduled deterministically without
recursive function construction. Each completed function validates its references,
selects instructions through one exhaustive dispatch, renders, and releases its
buffers. Publication checks cross-function and pool references. Failure discards
private output without trying another backend.

Semantic commitment owns copy behavior, drop hooks, and numeric wrapper targets.
Target planning owns storage and ABI decisions. Selection consumes completed
physical operands. Rendering consumes only instruction/directive text and does
not inspect semantic types or layouts.

## Validation

The complete local suite passed 12 shards initially. CLI and formatter dispatch
failed on the same stale assertion for the removed scalar label format. Scalar
execution output was already correct. After changing the assertion to the concrete
`factorial` call, the complete CLI and formatter dispatch checks passed. The other
103 formatter checks, including idempotency and safety, passed in the full run.
No production source changed for that correction.

Focused tests cover structured control flow, aggregate storage, cleanup, native
ABI, forwarding and leaf expansion, SSE2 rounding, oversized frames, and private
reference rejection. Self-hosting and fixed-point checks pass. The final Standards
and Spec reviews have no blocking findings. Non-blocking cleanup remains for an
unused test helper and unused parameters in four backend helpers.
