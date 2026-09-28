# Bounded constant measurements

Measured on 2026-09-28 on the AMD Ryzen 7 3700X Linux x86-64 reference host,
running Linux 6.18.33.2-microsoft-standard-WSL2. Baseline source is `9e9b40c`
and candidate source is `9ea03e2`. Both compilers were built for at least two
generations from the v1.52.0 release compiler. Their SHA-256 hashes are:

| Compiler | SHA-256 |
| --- | --- |
| Baseline | `da414243b587ab09e55420d325bf265424966891c46363369aaa0d1f9414d6c0` |
| Candidate | `24538cf1a4151587a94c2b89754db61d29ae2396bb43e92f4899a0d4c7bddd41` |

Each configuration had one warm-up. Three alternating pairs compiled the full
baseline source with both compilers. Three more alternating pairs compiled each
compiler's own source. The baseline source is the same in both workloads. No
source was excluded. No builds or tests ran concurrently. The command was:

```sh
<compiler> -L <source>/lib <source>/casa.casa -o /tmp/casa703-bench/out
```

Wall time used Python `time.clock_gettime(time.CLOCK_MONOTONIC_RAW)` around the
command. Peak RSS came from `/usr/bin/time -f %M` and is in KiB.

## Samples

| Workload | Run | Baseline seconds | Candidate seconds | Baseline RSS | Candidate RSS |
| --- | --- | ---: | ---: | ---: | ---: |
| Common source | Warm-up | 9.557 | 9.108 | 288476 | 289236 |
| Common source | 1 | 9.249 | 9.217 | 288732 | 289236 |
| Common source | 2 | 9.237 | 9.228 | 288604 | 289236 |
| Common source | 3 | 9.192 | 9.303 | 288604 | 289108 |
| Common source | Median | 9.237 | 9.228 | 288604 | 289236 |
| Self-compilation | Warm-up | 9.557 | 9.310 | 288476 | 303572 |
| Self-compilation | 1 | 9.184 | 9.405 | 288604 | 303316 |
| Self-compilation | 2 | 9.269 | 9.309 | 288476 | 303572 |
| Self-compilation | 3 | 9.190 | 9.322 | 288476 | 303316 |
| Self-compilation | Median | 9.190 | 9.322 | 288476 | 303316 |

Common-source times range from 9.192 to 9.249 seconds for the baseline and
9.217 to 9.303 seconds for the candidate. The median change is -0.009 seconds,
within observed variation. Median RSS increases by 632 KiB. Self-compilation
times range from 9.184 to 9.269 seconds for the baseline and 9.309 to 9.405
seconds for the candidate. The candidate remains below the 10-second timing
gate, and every measured RSS value remains below 1 GiB. The larger source
changes the self-compilation workload, so that result does not isolate a compiler
performance change.

The pinned historical self-compilation baseline is 9.710 seconds and 283.2 MiB.
The current candidate measures 9.322 seconds and about 296.2 MiB. Several
compiler changes intervene, so this is a total change, not an attribution to
bounded constants. The direct common-source comparison above uses the same
source input, but combines implementation and behavior changes since `9e9b40c`.

## Generated output and maintained code

The same sources were compiled again with `--keep-asm` to measure output size.

| Workload and compiler | Executable bytes | Assembly bytes |
| --- | ---: | ---: |
| Baseline source, baseline compiler | 5566720 | 26397654 |
| Baseline source, candidate compiler | 5566728 | 26397654 |
| Candidate source, candidate compiler | 5614144 | 26619682 |

Against `9e9b40c`, compiler sources add 981 lines and delete 659 lines. Tests
add 203 lines and delete 64 lines. No runtime assets were relocated. The
removed tests depend on retired `const fn` evaluation. Replacement tests cover
constant values, numeric failures, imports, type arguments, formatter syntax,
and executable output.

The new evaluator owns one typed value stack and reports failed declarations
without a substitute. The parser still supplies tokens and resolves names from
its constant store. Its constant map is copied in three parse phases. These
interface and allocation costs remain until the parsed-expression migration.
Per-phase time and allocator detail were not instrumented. RSS alone does not
identify their individual costs.

The repeated-request workload in
[`compiler-products/requests.casa`](../compiler-products/requests.casa) reported
974 resident pages at each of ten checkpoints over 500 requests. This covers
request construction and release, not repeated workspace edits and queries.
