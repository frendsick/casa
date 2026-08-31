# Trait implementation index benchmark

This benchmark compares `origin/main` at `a006f4668254` with the trait
implementation index. Each generated program has one used trait implementation
and 0 to 4,000 unrelated receiver-trait pairs. This isolates lookup and overlap
work as unrelated implementations are added.

## Environment

- Linux 6.18.33.2-microsoft-standard-WSL2 x86_64
- AMD Ryzen 7 3700X 8-Core Processor
- Stable v1.42.0 compiler used to build both compilers
- Fixed-point stage 3 compiler used for each measurement
- One warm-up followed by three measured runs per compiler and corpus
- GNU `/usr/bin/time` wall time in seconds

## Commands

Build each compiler to a fixed point and verify the stage 2 and stage 3 assembly:

```sh
bootstrap=/path/to/v1.42.0/casac
main_dir=/path/to/origin-main-worktree
branch_dir=/path/to/issue-447-worktree
benchmark_dir=$(mktemp -d /tmp/casa-447-benchmark.XXXXXX)

"$bootstrap" -L "$main_dir/lib" "$main_dir/casa.casa" \
    -o "$benchmark_dir/main-stage1" --keep-asm
"$benchmark_dir/main-stage1" -L "$main_dir/lib" "$main_dir/casa.casa" \
    -o "$benchmark_dir/main-stage2" --keep-asm
"$benchmark_dir/main-stage2" -L "$main_dir/lib" "$main_dir/casa.casa" \
    -o "$benchmark_dir/main-stage3" --keep-asm
diff -q "$benchmark_dir/main-stage2.s" "$benchmark_dir/main-stage3.s"

"$bootstrap" -L "$branch_dir/lib" "$branch_dir/casa.casa" \
    -o "$benchmark_dir/branch-stage1" --keep-asm
"$benchmark_dir/branch-stage1" -L "$branch_dir/lib" "$branch_dir/casa.casa" \
    -o "$benchmark_dir/branch-stage2" --keep-asm
"$benchmark_dir/branch-stage2" -L "$branch_dir/lib" "$branch_dir/casa.casa" \
    -o "$benchmark_dir/branch-stage3" --keep-asm
diff -q "$benchmark_dir/branch-stage2.s" "$benchmark_dir/branch-stage3.s"
```

Run the generated trait-heavy corpus with both compilers:

```sh
BENCHMARK_RUNS=3 "$branch_dir/docs/benchmarks/trait-implementation-index.sh" \
    "$benchmark_dir/main-stage3"
BENCHMARK_RUNS=3 "$branch_dir/docs/benchmarks/trait-implementation-index.sh" \
    "$benchmark_dir/branch-stage3"

for iteration in 1 2 3; do
    /usr/bin/time -f "main $iteration %e %M" \
        "$benchmark_dir/main-stage3" -L "$main_dir/lib" "$main_dir/casa.casa" \
        -o "$benchmark_dir/measured-main"
    /usr/bin/time -f "branch $iteration %e %M" \
        "$benchmark_dir/branch-stage3" -L "$branch_dir/lib" "$branch_dir/casa.casa" \
        -o "$benchmark_dir/measured-branch"
done
```

## Results

| Unrelated implementations | Main median | Branch median | Change |
|---:|---:|---:|---:|
| 0 | 0.01 s | 0.01 s | 0.00 s |
| 250 | 0.11 s | 0.13 s | +0.02 s, +18% |
| 500 | 0.23 s | 0.22 s | -0.01 s, -4% |
| 1,000 | 0.51 s | 0.46 s | -0.05 s, -10% |
| 2,000 | 1.25 s | 0.92 s | -0.33 s, -26% |
| 4,000 | 3.44 s | 1.89 s | -1.55 s, -45% |

The branch scales close to the source size because unrelated implementations
do not enter receiver or receiver-trait candidate lists. The remaining growth
includes lexing, parsing, validation, code generation, assembly, and linking.

## Compiler-wide check

Three alternating self-compilation runs used the same fixed-point compilers and
GNU `time -f '%e %M'`:

| Compiler | Wall-time median | Peak RSS median |
|---|---:|---:|
| Main | 26.14 s | 746,344 KiB |
| Branch | 25.27 s | 748,268 KiB |
| Change | -0.87 s, -3.3% | +1,924 KiB, +0.3% |

The branch improved self-compilation time. Peak RSS rose by 0.3%. This benchmark
did not collect allocator data, so it does not attribute the increase to a
specific allocation.
