# Parsed constant evaluation

Issue [#746](https://github.com/frendsick/casa/issues/746) removes constant-table
snapshots from token cursors. The shared grammar records each constant's
annotation, bounded terms, and source locations. Evaluation consumes those terms
without advancing or inspecting a token cursor. The cursor supplies only the
source-name bindings visible at the declaration.

`SymbolStore` owns completed and failed constants. The evaluator publishes each
successful declaration once. Type parsing borrows the store for the duration of
each call, including recursive type arguments. No shared lookup remains live
while constant evaluation updates the store. Header and body parsing can see all
elaborated module constants, while initializers retain source-order visibility.

The expression language is unchanged. Unsupported compound expressions remain
in syntax for diagnostics and formatting. They cannot execute user functions or
query target layout. Existing tests cover numeric widths, operand order,
qualified visibility, failed dependencies, and symbolic parameters. Added cases
cover parsed terms, unsupported compound expressions, recovery, and named
constant arguments in headers preceding their declarations.

## Repeated requests

Run the bounded workload with a compiler built from this branch:

```sh
./casac -L lib casa.casa -o /tmp/casac-parsed-constants
/tmp/casac-parsed-constants -L lib \
  docs/benchmarks/parsed-constants/requests.casa -o /tmp/constant-requests
/tmp/constant-requests
```

The workload releases 500 analysis snapshots, alternating successful constants
and failed declarations with dependent uses. Each request imports a public
constant computed from a private constant and uses it in a struct field type.
It prints `/proc/self/statm` after every 50 requests.

Measured on 2026-10-03 with 4,096-byte pages. Baseline source `6ad372c` reported
848 resident pages at all ten checkpoints. The candidate reported 849 pages at
all ten checkpoints. Both reached a resident-memory plateau before the first
checkpoint. These samples do not measure live allocations, allocator reuse, or
heap high-water separately.


## Compiler cost

Measured on the same date on an AMD Ryzen 7 3700X, Linux
6.18.40.1-microsoft-standard-WSL2. Baseline and candidate compilers were each
built once with the pinned v1.54.0 release compiler. No tests or other builds
ran concurrently with the timed samples. Each workload had one warm-up pair,
then three alternating baseline/candidate pairs.

Self-compilation uses each compiler's own source. Common-source compilation
uses baseline source `6ad372c` for both compilers. Each run used:

```sh
<compiler> -L <source>/lib <source>/casa.casa -o /tmp/measured-compiler
```

Wall time uses `CLOCK_MONOTONIC_RAW` around the process. Peak RSS comes from
`/usr/bin/time -f %M` and is in KiB.

| Workload | Run | Baseline seconds | Candidate seconds | Baseline RSS | Candidate RSS |
| --- | --- | ---: | ---: | ---: | ---: |
| Self-compilation | Warm-up | 10.824 | 11.561 | 306616 | 306172 |
| Self-compilation | 1 | 11.077 | 11.083 | 306616 | 306172 |
| Self-compilation | 2 | 11.306 | 10.959 | 306744 | 306172 |
| Self-compilation | 3 | 11.404 | 10.976 | 306616 | 306300 |
| Self-compilation | Median | 11.306 | 10.976 | 306616 | 306172 |
| Common source | Warm-up | 11.189 | 11.106 | 306488 | 304124 |
| Common source | 1 | 11.415 | 11.320 | 306744 | 304124 |
| Common source | 2 | 11.297 | 11.166 | 306616 | 304124 |
| Common source | 3 | 11.096 | 11.360 | 306616 | 304124 |
| Common source | Median | 11.297 | 11.320 | 306616 | 304124 |

Common-source median wall time increased by 0.023 seconds, with overlapping
sample ranges. Median peak RSS decreased by 2,492 KiB. The self-compilation
comparison also includes the changed source, so it does not isolate the cost
of the lookup and expression changes.

Compiler SHA-256 hashes:

| Compiler | SHA-256 |
| --- | --- |
| Baseline | `12de3a4b8c8ec927d03d47cfc4fc91ed5fa4112631857f636ad26dceb1beb1c8` |
| Candidate | `cc31c2177c8292f5643b0e62c4dd2233cb7f6d4dd768757251d15ce7017a7621` |
