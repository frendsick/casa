# Self-compilation target and measurements

On 2026-09-26 the maintainer set a goal of roughly **10 seconds for
self-compilation** and rejected half-minute compilation as acceptable.
Memory is secondary unless self-compilation needs multiple gigabytes. The
previous proposals to allow 20% more time and 25% more memory were not accepted.
The exact timing acceptance band and numeric RSS ceiling remain open.
The decision is recorded in [Validate the compiler simplification blueprint](https://github.com/frendsick/casa/issues/651#issuecomment-5849035021).

Aggressive simplification and feature reductions may be investigated for large
compilation gains. Each behavior change needs an explicit decision about its
benefit and user cost. Blueprint acceptance remains open, and production
implementation remains outside the compiler-planning map.

The measurement workload is a fresh compiler process building its complete
source into a native executable, including assembly and linking. Use the
reference Linux x86-64 machine, warm filesystem caches and fresh output, without
a compiled-module cache. Final acceptance requires the fixed-point compiler and
the [paired measurement protocol](compiler-simplification-measurements.md).

## Current evidence

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
are retained. Resolve this discrepancy before using the results for a precise
acceptance comparison.

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

## Next decisions

[Establish fixed-point compilation costs](https://github.com/frendsick/casa/issues/682)
first establishes the comparable baseline and attributes the dominant costs.
[Choose reductions toward ten-second self-compilation](https://github.com/frendsick/casa/issues/683)
then uses native experiments to measure gains and select tradeoffs with the
maintainer. Final blueprint validation follows those results.
