# Phase-valid source and semantic bodies

Evidence for [#732](https://github.com/frendsick/casa/issues/732).
Run date: 2026-10-03.

## Protocol

The control source is `29ddfa1b9b61fde744bc0da4101e5c1136a76580`.
Its compiler and library sources are unchanged through the branch base
`d8dbb8f155c691a24c30db8a614ddf994be11b17`. The candidate contains this
implementation. [measurements.json](measurements.json) records source-tree
hashes, compiler hashes, commands, warm-ups, every sample, phase logs and output
hashes. Both compilers are fixed points bootstrapped with stable v1.53.0.
A further self-compilation produced identical assembly for each compiler.

The host is an AMD Ryzen 7 3700X running Linux 6.18.40.1 under WSL2, with
GCC 11.4.0. Each workload uses one warm-up per compiler and three alternating
measured pairs. The second pair runs the candidate first. Builds run serially,
with warm filesystem caches and fresh outputs. Timing uses CLOCK_MONOTONIC_RAW
through the existing [measurement helper](../fixed-point-compilation/measure.py)
and includes assembly and linking. GNU time records peak RSS.

The common-source workload builds the complete control compiler and its library
with both compilers. It includes the compiler's generic collections, closures,
traits and derived implementations. No source is excluded. The separate
self-compilation workload builds each compiler's own source. That comparison
includes the larger candidate source and cannot isolate implementation cost.

## Compilation results

| Workload | Control median (range), s | Candidate median (range), s | Time change | Control RSS median, MiB | Candidate RSS median, MiB |
| --- | ---: | ---: | ---: | ---: | ---: |
| Same control source | 12.542 (12.414 to 12.667) | 13.321 (13.047 to 13.426) | +6.2% | 357.3 | 361.2 |
| Each compiler’s source | 12.641 (12.504 to 12.708) | 15.831 (15.659 to 16.243) | +25.2% | 357.2 | 438.9 |

| Workload | RSS range, KiB |
| --- | ---: |
| Same control source, control | 365,792 to 365,920 |
| Same control source, candidate | 369,916 to 370,044 |
| Each compiler’s source, control | 365,792 to 365,920 |
| Each compiler’s source, candidate | 449,404 to 449,532 |

| Workload | Control assembly bytes | Candidate assembly bytes | Control executable bytes | Candidate executable bytes |
| --- | ---: | ---: | ---: | ---: |
| Same control source | 29,190,622 | 29,183,520 | 6,315,568 | 6,323,760 |
| Each compiler’s source | 29,190,622 | 34,635,425 | 6,315,568 | 7,505,880 |

All measured compilation runs succeeded. The common-source timing ranges do
not overlap. The candidate uses more memory without a speed gain, so it does
not meet the general memory tradeoff requirement. Every run stays below the
1 GiB ceiling. Self-compilation exceeds 10 seconds, which the protocol permits
temporarily for intermediate #699 work. The common-source regression and memory
increase require an explicit decision under the
[measurement protocol](../compiler-simplification-measurements.md).
On 2026-10-03, the maintainer accepted the measured time and memory costs for
#732 after reviewing this evidence. This exception applies to #732. The general
performance gates remain unchanged.

The [native checkpoint](../compiler-self-compilation-target.md#current-evidence)
recorded 9.710 seconds and 283.2 MiB. The candidate's own-source result is
+63.0% in time and +55.0% in peak RSS against that checkpoint.
Intervening behavior changes, source growth and host updates prevent attributing
this historical difference to #732 alone. No adjusted architecture-only checkpoint
is available.

## Cost investigation

The [first measurements](before-storage-reuse.json) found a common-source median
of 13.515 seconds against 12.414 seconds, with 371,964 KiB against 365,920 KiB
median RSS. Backend tracing found duplicate storage collection, an extra copy
of lowered operations, and lambda-body lowering during destructor generation.
The implementation now reuses storage declarations finalized by CheckedProgram,
rewrites its freshly lowered stream in place, and constructs closure destruction
from capture metadata. The [next measurements](after-storage-reuse.json) retain
that comparison. The final measurements above include the subsequent naming
corrections. These runs have separate controls and do not isolate each deletion.

The compiler's coarse progress boundaries give these median intervals using
CLOCK_MONOTONIC. The native-build interval uses the sampler's matching clock.
These readings differ from CLOCK_MONOTONIC_RAW on this host, so the intervals
must not be subtracted from the acceptance totals above.

| Workload | Analysis, s | Checked commitment, planning and emission, s | Native build and process overhead, s |
| --- | ---: | ---: | ---: |
| Same control source, control | 8.792 | 1.569 | 1.900 |
| Same control source, candidate | 8.655 | 2.607 | 1.716 |
| Each compiler’s source, control | 8.862 | 1.530 | 1.965 |
| Each compiler’s source, candidate | 10.331 | 3.081 | 2.075 |

The second interval includes the new concrete-body gate. These boundaries do
not isolate individual passes. Allocation traffic during compilation was not
instrumented. Peak RSS and the request-lifetime samples below measure different
parts of memory use.

## Request lifetime

The existing [editor snapshot workload](../editor-snapshots/lifetime.casa)
performs 630 analyses in 30 batches. It alternates valid imports and rejected
function bodies, replaces owned snapshots and retains a hover answer across
replacement. The [allocator sampler](../compiler-reductions/lifetime.py)
stops after initialization and each batch.

The initial candidate [leaked 32 closure records per batch](lifetime-leak.json),
adding 1,536 live bytes each time. These were temporary borrowed visitors in
editor projection. Naming the visitors establishes owners. An intermediate
[explicit-drop attempt crashed](lifetime-explicit-drop.json) because the stable
compiler emitted both explicit destruction and scope cleanup. The final visitors
use automatic scope cleanup.

The [control samples](lifetime-control.json) and
[final candidate samples](lifetime.json) both have zero live allocations at
all 31 checkpoints. After five warm-up batches, each has a stable plateau:

| Measurement | Control | Candidate |
| --- | ---: | ---: |
| Live allocations and payload bytes | 0 | 0 |
| Reusable allocations | 1,135 | 1,049 |
| Reusable payload bytes | 57,720 | 54,080 |
| Mapped heap bytes | 67,108,864 | 67,108,864 |
| RSS, KiB | 4,108 | 4,788 |

The allocator retains reusable storage in one mapped chunk. Zero live payload
establishes release between requests. This workload does not measure peak
memory while snapshots overlap or compiler-source query latency.

## Maintained source and interfaces

Counts use physical lines in casa.casa and compiler/*.casa, with compiler tests
counted separately. [source-counts.json](source-counts.json) retains the totals.

| Source | Production lines | Compiler test lines |
| --- | ---: | ---: |
| Historical source | 40,346 | 25,853 |
| Control | 43,565 | 27,212 |
| Candidate | 47,827 | 27,200 |

This change adds 6,064 production lines and removes 1,802. Tests add 1,293 lines
and remove 1,305. No runtime assets or generated source are relocated. The
historical counts use revision 1e89fb5 and include intervening language changes.
The implementation increases maintained source while removing caller-managed
phase transitions.

Function loses its body and two phase flags, leaving 12 declaration fields.
The source registry has three private states. The semantic registry has four.
Active resolution, checking and specialization frames own bodies until they
publish their results. Callers no longer pair body extraction with restoration
or manufacture checked flags.

Source bodies have eight structural node variants and 19 leaf categories.
Semantic bodies have eight structural variants and 33 checked action variants.
The editor occurrence vocabulary has 19 variants and contains no executable
ownership actions. Explicit structure adds traversals, but removes delimiter
matching from semantic body traversal. Private body fields prevent consumers from
changing work state through declaration metadata.

Specialization substitutes checked recipes. CheckedProgram validates reachable
signatures, storage, captures, fields, patterns and call targets once before
backend selection. Both backends reuse the finalized storage and lower one body
at a time. Editor projection depends on source occurrences and presentation
facts, and releases compiler bodies before returning the index. Rejected source
retains diagnostics and editor facts but cannot produce backend input.
Unreachable semantic nodes retain source facts without executable operands.

## Reproduction

Build each compiler with stable v1.53.0, then self-compile twice with --keep-asm
and compare the two assemblies. Use the fixed-point binaries with the commands
recorded in measurements.json. The shared runner accepts a label, compiler,
source checkout and output directory:

```sh
python3 docs/benchmarks/fixed-point-compilation/measure.py label "$compiler" "$source_tree" "$output_directory"
```

Run one warm-up per compiler and three alternating pairs for each source
workload. Preserve all generated JSON records. For lifetime sampling:

```sh
./casac_stage2 -L lib docs/benchmarks/editor-snapshots/lifetime.casa -o "$output_directory/lifetime"
python3 docs/benchmarks/compiler-reductions/lifetime.py "$output_directory/lifetime" "$output_directory/lifetime.json"
```
