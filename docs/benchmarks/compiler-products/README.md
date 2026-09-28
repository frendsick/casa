# Request product measurements

Measured on 2026-09-28 on the reference AMD Ryzen 7 3700X Linux x86-64 host.
Baseline: `b9ddb3b`. Changed source: the #700 request-product implementation.
Both compilers were built for two generations using the v1.52.0 release tools.

## Repeated requests

Run from the repository root:

```sh
./casac -L lib docs/benchmarks/compiler-products/requests.casa -o /tmp/casa-product-requests
/tmp/casa-product-requests
```

The workload analyzes and releases 500 requests. Each request uses an imported
source override and retains a report and semantic tokens until release. It
prints `/proc/self/statm` every 50 requests. All ten samples were identical:

```text
17895 973 955 932 0 16954 0
```

With 4,096-byte pages, resident memory was 3.801 MiB and virtual size was
69.902 MiB at every checkpoint. This bounded workload shows stable process
memory after warm-up. It does not instrument allocator live bytes or cover
workspace editing workloads, which remain part of later editor acceptance.

## Self-compilation

Each measured compiler compiled its own checkout's `casa.casa`, including
assembly and linking. The commands used `-L lib casa.casa -o <temporary binary>`.
Each configuration had one warm-up followed by three alternating measured
pairs. Wall time came from Python's monotonic `time.perf_counter`. Peak RSS
came from `/usr/bin/time -f %M`. No test suite ran concurrently.

| Run | Baseline seconds | Changed seconds | Baseline peak RSS KiB | Changed peak RSS KiB |
| --- | ---: | ---: | ---: | ---: |
| Warm-up | 9.314 | 9.564 | 285116 | 303316 |
| 1 | 9.251 | 9.326 | 284988 | 303572 |
| 2 | 9.193 | 9.270 | 284988 | 303572 |
| 3 | 9.358 | 9.212 | 284988 | 303316 |
| Median, measured | 9.251 | 9.270 | 284988 | 303572 |

The median time difference was 0.019 seconds. The observed ranges overlap.
Median peak RSS increased by 18.148 MiB, from 278.309 to 296.457 MiB.
These measurements cover the shared source-store change through the existing
CLI pipeline. They do not establish final redesign acceptance or performance
for the new assembly entrypoint.
