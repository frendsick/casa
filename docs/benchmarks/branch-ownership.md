# Branch ownership measurements

Measured on 2026-09-29 on the AMD Ryzen 7 3700X Linux x86-64 WSL2 host.
The baseline is `b4b7d3375c32589dc0c8fa03dfa13628b796a51b`. The candidate is the
#706 implementation in this commit. Both compilers were built for two
generations using the v1.53.0 release compiler.

The five changed compiler files have combined SHA-256
`972f5cc6c3137de2391f7bbff4d9d7796e4316de353a77c5be3c6b6a99bda5da`.
The digest concatenates each filename, a NUL byte, and its bytes in this order:
`bytecode.casa`, `common.casa`, `legacy_parser.casa`, `semantics.casa`,
`typechecker.casa`, with the `compiler/` prefix on each filename.

Compiler binary SHA-256 values:

- Baseline: `d988e9b4c188211f0a10a810ef4e067488dee503d1faa1f28e43bbaf5363496d`
- Candidate: `f4748d607fb89e9175a2849e54dcb58c981623f663f74fa6f7111242dd12f245`

## Compilation

Each configuration had one warm-up and three alternating measured runs.
Wall time uses `CLOCK_MONOTONIC_RAW`. Peak RSS uses `/usr/bin/time -f %M`.
No test suite ran concurrently. Every compile succeeded and included assembly
and linking with `--keep-asm`.

For self-compilation, each compiler compiled its own checkout. The shared
source was the baseline checkout's `tests/compiler/test_copy_clone.casa`,
including its compiler and library imports. This exercises generic ownership
checking on identical input files. The command forms were:

```sh
"$compiler" -L "$source_root/lib" "$source_root/casa.casa" -o "$output" --keep-asm
"$compiler" -L "$baseline_root/lib" "$baseline_root/tests/compiler/test_copy_clone.casa" -o "$output" --keep-asm
```

| Workload / run | Baseline seconds | Candidate seconds | Baseline RSS KiB | Candidate RSS KiB |
| --- | ---: | ---: | ---: | ---: |
| Self / warm-up | 9.005 | 8.869 | 293732 | 301424 |
| Self / 1 | 8.950 | 8.981 | 293748 | 301424 |
| Self / 2 | 8.920 | 9.011 | 293876 | 301424 |
| Self / 3 | 8.934 | 8.817 | 294004 | 301296 |
| Self / median | 8.934 | 8.981 | 293876 | 301424 |
| Shared source / warm-up | 7.899 | 7.911 | 269300 | 275056 |
| Shared source / 1 | 7.926 | 8.030 | 269556 | 274928 |
| Shared source / 2 | 8.045 | 7.869 | 269428 | 275056 |
| Shared source / 3 | 7.986 | 7.868 | 269428 | 275056 |
| Shared source / median | 7.986 | 7.869 | 269428 | 275056 |

Timing ranges overlap for both workloads. These samples do not establish a
speed improvement. Candidate self-compilation stays below ten seconds. Its
maximum measured RSS is 294.36 MiB, below the 1 GiB ceiling. Median RSS rises
by 7.37 MiB for self-compilation and 5.50 MiB for the shared source. Typed places
own field lists, including backing storage for empty lists. The change does
not claim a memory reduction.

| Output / bytes | Baseline | Candidate |
| --- | ---: | ---: |
| self assembly | 26068456 | 26053924 |
| self executable | 5496096 | 5499648 |
| common assembly | 21998638 | 21998638 |
| common executable | 4592784 | 4592792 |

## Repeated requests

[The request workload](branch-ownership-requests.casa) analyzes and releases
500 requests with owned branch results, an early return, a borrowed input
result, and implicit cleanup. Each request asserts that analysis succeeded.
It samples `/proc/self/statm` every 50 requests.

```sh
./casac_new -L lib docs/benchmarks/branch-ownership-requests.casa -o /tmp/casa-706-requests
/tmp/casa-706-requests
```

All ten resident-memory samples are 975 pages, or 3.809 MiB with 4,096-byte
pages. This bounded workload plateaus after warm-up. It does not measure
allocator live bytes, reusable bytes, or a full workspace editing workload.

## Scope and validation

The production compiler diff adds 685 lines and removes 745 lines relative
to the pinned baseline. No runtime code is relocated. Typed origin records
replace encoded strings and parallel binding maps. Operations own optional
cleanup actions, replacing the ownership table in `SymbolStore`.

All 14 local CI shards passed. Seven focused ownership, destruction, and
scope checks passed after the final typed-query change. The shared-source
executables produced identical test output. Final self-compilation also
preserved fixed-point assembly.
