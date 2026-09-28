# Qualified import measurements

Measured on 2026-09-28 on the AMD Ryzen 7 3700X Linux x86-64 reference host,
running Linux 6.18.33.2-microsoft-standard-WSL2. Baseline: `a1ccb4f`.
Candidate: `6a88317`. Both compilers were built for two generations from
the v1.52.0 release compiler. Sources were archived into immutable directories.

Each workload had one warm-up and three alternating baseline/candidate pairs.
Wall time uses Python `time.perf_counter`. Peak RSS uses `/usr/bin/time -f %M`.
No builds or test suites ran concurrently. Both compilers used the command

```sh
<compiler> -L lib --keep-asm casa.casa -o <temporary-output>
```

Self-compilation builds each compiler’s own archived source. The common-source
workload makes both compilers build the complete baseline source tree. That
tree exercises the compiler’s generic collections, traits, and module graph.
No sources were excluded. [Raw evidence](evidence.json) includes every sample
and compiler SHA-256 hashes.

## Timings and peak memory

### Self-compilation

| Run | Baseline seconds | Candidate seconds | Baseline RSS KiB | Candidate RSS KiB |
| --- | ---: | ---: | ---: | ---: |
| Warm-up | 10.877 | 10.302 | 302840 | 292316 |
| 1 | 10.701 | 10.370 | 302840 | 292444 |
| 2 | 10.513 | 10.264 | 302840 | 292316 |
| 3 | 10.423 | 10.083 | 302840 | 292572 |
| Median | 10.513 | 10.264 | 302840 | 292444 |

Observed time ranges: baseline 10.423–10.701 seconds,
candidate 10.083–10.370 seconds.

### Common source

| Run | Baseline seconds | Candidate seconds | Baseline RSS KiB | Candidate RSS KiB |
| --- | ---: | ---: | ---: | ---: |
| Warm-up | 10.376 | 10.509 | 302840 | 307420 |
| 1 | 10.576 | 10.060 | 302968 | 307676 |
| 2 | 9.963 | 10.064 | 302968 | 307804 |
| 3 | 10.400 | 10.661 | 303096 | 307804 |
| Median | 10.400 | 10.064 | 302968 | 307804 |

Observed time ranges: baseline 9.963–10.576 seconds,
candidate 10.060–10.661 seconds.

The candidate self-compilation median exceeds the accepted 10-second gate by
0.264 seconds. Every final sample is below the 1 GiB peak-RSS ceiling.
Median self-compilation RSS falls by 10.15 MiB. On common source, median RSS
changes by +4.72 MiB and median wall time changes by
-0.336 seconds. Baseline timing varied across the final and earlier complete runs. The final revision does not meet the timing gate.
The timing result and common-source memory cost require an evidence-based decision
under the accepted measurement protocol. They are not recorded as accepted here.

## Generated output

| Workload and compiler | Executable bytes | Assembly bytes |
| --- | ---: | ---: |
| self, baseline | 5604880 | 26598034 |
| self, changed | 5566744 | 26397654 |
| common, baseline | 5604880 | 26598034 |
| common, changed | 5588016 | 26552602 |

## Maintained code and interfaces

Against `a1ccb4f`, compiler sources add 1,065 lines and delete 2,033 lines.
Tests add 332 lines and delete 1,032 lines. No runtime assets were relocated.
The removed tests exercise retired selection and pruning contracts. Qualified
visibility, repeated aliases, independent failures, root identity collisions,
syntax facts, and formatter rejection have replacement coverage.

The production path removes import operation variants, token namespace rewriting,
import-token prepasses, and the selective-import closure module. Parsed import
and declaration facts feed a request-local module cache and source binding maps.
Bindings distinguish aliases from declarations. Failed module entries prevent
reloading or repeating diagnostics. Original tokens retain their spelling.

Imported global initialization and the legacy semantic representation remain for
their later slices. This measurement does not isolate those future migrations.
Per-phase timing, allocator live bytes, and allocator reusable bytes were not
instrumented. RSS alone does not identify every allocation cost.

## Repeated requests

The bounded workload analyzes and releases 500 requests, printing resident and
virtual page counts every 50 requests:

```sh
./casac_new -L lib docs/benchmarks/compiler-products/requests.casa -o /tmp/casa702-requests
/tmp/casa702-requests
```

All ten checkpoints reported 958 resident pages (3.742 MiB).

```text
17887 958 940 924 0 16954 0
17887 958 940 924 0 16954 0
17887 958 940 924 0 16954 0
17887 958 940 924 0 16954 0
17887 958 940 924 0 16954 0
17887 958 940 924 0 16954 0
17887 958 940 924 0 16954 0
17887 958 940 924 0 16954 0
17887 958 940 924 0 16954 0
17887 958 940 924 0 16954 0
```

The plateau covers request construction, imported overrides, analysis products,
and release. It does not cover repeated workspace edits and queries.

## Earlier samples

The raw evidence also retains complete runs at `9b52076` and `a0f7fea`, plus an
exploratory development run. The latter stopped after sources changed during
measurement,
causing its final candidate compilation to fail. It is excluded from comparisons.
The final runs above use immutable source archives and include all successful
warm-up and measured samples. Baseline timings also varied across runs,
so these results do not establish a general speed improvement.
