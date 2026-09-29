# Checked generic recipes

Evidence for [#708](https://github.com/frendsick/casa/issues/708).
Run date: 2026-09-29.

## Protocol

Control source: `27a3b9f8bd4eeba73fdbba636b64aa0b1c4aa01b`. Candidate source is the
production diff in this commit. The raw evidence records its SHA-256 and both
compiler hashes. Both compilers are fixed points bootstrapped with stable
v1.53.0. Their next self-compilation produces identical assembly.

Host: AMD Ryzen 7 3700X 8-Core Processor. OS: `Linux-6.18.33.2-microsoft-standard-WSL2-x86_64-with-glibc2.39`.
GNU assembler and linker 2.47.20260726, GCC 11.4.0. Each workload has one warm-up
followed by three alternating control/candidate pairs. Builds run serially.
Wall time uses Linux `CLOCK_MONOTONIC_RAW` through the existing
[measurement helper](../fixed-point-compilation/measure.py). GNU time records
peak RSS. Timing includes assembly and linking.

Self-compilation uses each compiler's own sources. The common-source workloads
use the control tree's `tests/compiler/test_traits.casa` and
`tests/compiler/test_iterator_combinators.casa`, with the same control library.
No source from either common workload was excluded. Commands, warm-ups, all
samples, output sizes, and assembly hashes are in [measurements.json](measurements.json).
These results include validation and correctness changes. They do not isolate
summary caching from the other changes.

## Results

| Workload | Control median (range), s | Candidate median (range), s | Change | Control RSS median, MiB | Candidate RSS median, MiB |
| --- | ---: | ---: | ---: | ---: | ---: |
| self | 9.029 (8.941–9.098) | 9.366 (9.316–9.394) | +3.7% | 315.2 | 329.7 |
| traits | 8.278 (8.225–8.743) | 8.510 (8.468–8.538) | +2.8% | 292.0 | 304.9 |
| iterators | 0.723 (0.712–0.760) | 0.744 (0.723–0.747) | +2.9% | 24.1 | 24.6 |

| Workload | Control assembly bytes | Candidate assembly bytes | Control executable bytes | Candidate executable bytes |
| --- | ---: | ---: | ---: | ---: |
| self | 26,199,762 | 28,331,167 | 5,527,296 | 6,112,896 |
| traits | 22,510,913 | 24,391,584 | 4,702,512 | 5,209,840 |
| iterators | 688,464 | 788,131 | 170,952 | 208,392 |

All runs succeeded. Output sizes and assembly hashes are stable across the
four runs of each configuration and workload. Exact specialization names
retain complete bindings instead of a hash, which increases symbol text.

Self-compilation remains below the final 10-second target. The highest measured
self-compilation RSS is 329.8 MiB, below the 1 GiB limit. The branch does not
show a speed or memory improvement. Its self-compilation timing range is
above the control range. The common-source timing ranges overlap, so these
three pairs do not establish a repeatable regression beyond observed variation.
Their higher memory and emitted sizes remain measurable costs of this change.
Summary caching, stronger validation, and exact symbol identities are bundled
here, so this experiment cannot attribute those costs to one mechanism.
There is no blanket percentage allowance for a common-source regression.

| Workload | Control RSS range, KiB | Candidate RSS range, KiB |
| --- | ---: | ---: |
| self | 322,808–322,936 | 337,604–337,732 |
| traits | 299,000–299,000 | 312,260–312,388 |
| iterators | 24,440–24,696 | 25,156–25,156 |

The [native checkpoint](../compiler-self-compilation-target.md#current-evidence)
recorded 9.710 seconds and 283.2 MiB on source `5c7ff5b`. This candidate records
9.366 seconds and 329.7 MiB, a total difference of -3.5% time and +16.4% RSS.
Intervening compiler changes and different source workloads prevent attributing
that historical difference to #708. The paired control above isolates the
current change more closely, while self-compilation still builds different trees.

The [accepted memory gate](../compiler-simplification-measurements.md) requires
memory increases to buy substantial, repeatable speed gains. This change does
not meet that requirement. On 2026-09-29, the maintainer accepted the measured
time and memory increases for #708 after reviewing this evidence. This decision
permits this change to proceed. It does not change the general memory gate.

## Lifetime and complexity

[requests.casa](requests.casa) creates and destroys 500 independent analysis
snapshots. Each request checks a borrowed generic closure passed through a
generic identity function. It samples `/proc/self/statm` every 50 requests.
All ten samples are identical: 17,937 virtual pages and 1,033 resident pages.
With 4,096-byte pages, RSS plateaus at 4.04 MiB. This checks bounded process
retention across requests. It does not measure live allocations or distinguish
allocator reuse from unreachable allocations. Per-phase timing and allocator
traffic were not instrumented for this change.

The summary map belongs to `SemanticChecks` and ends with its semantic
session. Returned summaries own their cloned results. No process-global cache
or new product API is introduced. The bytecode/emitter contract gains one
`StaticArrayElement::Function` variant so all static callable values use the
same label encoder. Binding keys sort names and length-prefix each field.
Generated semantic names use whitespace, which source identifiers cannot use.
The emitter maps those identities into a separate assembly-label namespace.

Production source adds 191 lines and removes 94 lines, for a net increase
of 97 lines. Tests add 92 lines and remove one line, plus the 41-line request
fixture. No runtime assets or generated source were relocated. Direct-call
summary lookup replaces repeated callable-body analysis. Trait defaults and
derived behavior remain separate work under #709.

## Reproduce the request workload

```sh
/tmp/casa-708-candidate -L lib docs/benchmarks/checked-recipes/requests.casa -o /tmp/casa-708-requests
/tmp/casa-708-requests
```

Use fixed-point control and candidate executables for the paired compilation
commands recorded in `measurements.json`. Repeat the recorded warm-up and
three measured pairs with the same source and output paths. Timing uses
`clocks()["monotonic_raw"]` from the linked helper around each command.

Validation: `tests/test_all.sh` passed all 14 shards, including bootstrap and
formatter checks. The focused product tests passed with the candidate compiler.
Both control and candidate passed the assembly fixed-point comparison.
The final Standards and Spec code reviews found no blocking defects. The
maintainer accepted the performance cost as recorded above. One extra clone
of saved callable sets remains a non-blocking review finding.
