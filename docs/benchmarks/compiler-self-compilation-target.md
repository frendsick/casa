# Self-compilation target and measurements

On 2026-09-28 the maintainer accepted a **10.0-second maximum median** for
self-compilation on the reference Ryzen 7 3700X Linux x86-64 host. Every measured
run must use at most **1 GiB peak RSS**. Memory increases must buy a substantial,
repeatable reduction in end-to-end compile time. Small speed gains do not justify
materially higher memory use.

This makes the earlier roughly ten-second goal explicit. The previous proposals
to allow 20% more time and 25% more memory were not accepted. Common-source
regressions beyond observed variation require explicit review of the evidence,
without a fixed percentage allowance. The maintainer accepted the blueprint on
2026-09-28 in [Validate the compiler simplification blueprint](https://github.com/frendsick/casa/issues/651).

Aggressive simplification and feature reductions may be investigated for large
compilation gains. Each behavior change needs an explicit decision about its
benefit and user cost. Production implementation remains outside the completed
compiler-planning map and must meet the accepted contracts and gates.

The measurement workload is a fresh compiler process building its complete
source into a native executable, including assembly and linking. Use the
reference Linux x86-64 machine, warm filesystem caches and fresh output, without
a compiled-module cache. Final acceptance requires the fixed-point compiler and
the [paired measurement protocol](compiler-simplification-measurements.md).

## Current evidence

The [completed native investigation](https://github.com/frendsick/casa/issues/683#issuecomment-5867480612)
reached the speed target without reducing language features. The implementation
merged in [Bring self-compilation below ten seconds](https://github.com/frendsick/casa/pull/696).

| Pair | Control seconds | Candidate seconds |
| --- | ---: | ---: |
| 1 | 13.095 | 9.721 |
| 2, candidate first | 12.740 | 9.578 |
| 3 | 12.928 | 9.710 |
| Median | 12.928 | 9.710 |

Both fixed-point compilers built the same committed source, `5c7ff5b`. The control
was `dfb77b4`. One warm-up per compiler preceded three alternating measured pairs.
Times use `CLOCK_MONOTONIC_RAW` and include assembling and linking. Median peak
RSS fell from 294.2 to 283.2 MiB. The measurements apply to this host and workload.

The [evidence report](https://github.com/frendsick/casa/blob/43c405c750fa2d230fcc4ea2cf1f5ad0b7fb85d1/docs/benchmarks/under-ten-native/README.md)
and [raw samples](https://github.com/frendsick/casa/blob/43c405c750fa2d230fcc4ea2cf1f5ad0b7fb85d1/docs/benchmarks/under-ten-native/timings.csv)
retain provenance, native probes, and correctness results. This is evidence for
the current compiler. The redesigned compiler must independently meet its gates.

## Historical investigation, 2026-09-26

The branch compiler was built from production revision
`29fe1866a6ed6e408bdf127b19bf3734b839e376` by stable Casa v1.50.0.
It then compiled the same source on Linux under WSL2, using an AMD Ryzen 7 3700X.
One warm-up preceded three serial measured runs with `-L lib`, `--keep-asm`,
`--verbose` and the same output path. All compiler invocations succeeded.

| Run | Monotonic seconds | GNU time wall seconds | Peak RSS MiB |
| --- | ---: | ---: | ---: |
| 1 | 54.933 | 57.40 | 571.50 |
| 2 | 54.421 | 55.76 | 571.50 |
| 3 | 54.469 | 56.88 | 571.50 |
| Median | 54.469 | 56.88 | 571.50 |

The clocks disagree beyond rounding. The cause is unverified, so both readings
are retained as historical evidence. Later fixed-point and native measurements
use `CLOCK_MONOTONIC_RAW` for acceptance comparisons.

These are branch-compiler measurements, not established fixed-point timings.
The historical [33.11-second baseline](compiler-complexity-baseline.md#measured-build-cost)
used stable v1.50.0 to build the repository source. A single fresh stable build
took 29.84 seconds by GNU time and 28.623 monotonic seconds, at 742.75 MiB RSS.
That single run is not a paired median and does not establish why the compilers
have different costs.

A separate GDB run timestamped function entries and completed in 55.065
monotonic seconds. Its inclusive intervals were:

| Interval | Seconds |
| --- | ---: |
| Parse and resolve to typecheck entry | 12.067 |
| Typecheck entry to function scheduler | 29.410 |
| Scheduler to trait validation | 1.818 |
| Trait validation to generic-cycle validation | 0.097 |
| Generic-cycle validation to specialization | 0.045 |
| Specialization to bytecode entry | 4.515 |
| Bytecode to emitter entry | 2.547 |

The 29.410-second interval includes extern validation, session setup and root
semantic analysis of reachable calls. The specialization interval includes
subsequent diagnostic and source cleanup. These boundaries do not identify the
internal causes. Generic-cycle validation was not material in this workload.

The complete [raw evidence and reproduction scripts](https://github.com/frendsick/casa/blob/53f7b8cc0a3813ccfd309ae49612dcebcc37e581/compiler/blueprint_prototype/performance-evidence.json)
and [investigation notes](https://github.com/frendsick/casa/blob/53f7b8cc0a3813ccfd309ae49612dcebcc37e581/compiler/blueprint_prototype/PERFORMANCE.md)
remain pinned on the evidence branch. They include compiler hashes, tool
versions, commands and logs. The
[integrated prototype](https://github.com/frendsick/casa/blob/53f7b8cc0a3813ccfd309ae49612dcebcc37e581/compiler/blueprint_prototype/README.md)
demonstrates its limited contracts but does not predict native compilation speed.
No speed improvement or blueprint acceptance is claimed by this checkpoint.

## Blueprint acceptance

[Establish fixed-point compilation costs](https://github.com/frendsick/casa/issues/682)
and [Choose reductions toward ten-second self-compilation](https://github.com/frendsick/casa/issues/683)
are complete. The [blueprint acceptance record](compiler-simplification-blueprint.md)
reconciles their evidence with the earlier executable slice and implementation
plan. Acceptance of the blueprint remains separate from eventual production
validation.
