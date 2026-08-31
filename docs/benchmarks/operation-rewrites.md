# Operation rewrite benchmark

Run date: 2026-08-31

This benchmark compares `origin/main` at `aa088b4` with the append-only
operation rewrite in this document's commit. The workload keeps one bound
`Display` value and repeats `tag print`. Each group contains two source
operations. Typechecking lowers `print` to a display call followed by a typed
print operation.

## Environment

- Linux 6.18.33.2-microsoft-standard-WSL2 x86_64
- AMD Ryzen 7 3700X 8-Core Processor
- Stable v1.49.0 compiler used to build both compilers
- Fixed-point stage 3 compiler used for each measurement
- Stage 2 and stage 3 assembly matched for both compilers
- One warm-up followed by three measured runs per compiler and corpus
- Analysis time from the compiler's `--verbose` progress timestamps
- Wall time from GNU `/usr/bin/time`

## Commands

Build each compiler to a fixed point:

```sh
stable=/path/to/v1.49.0/casac
main_dir=/path/to/origin-main-worktree
branch_dir=/path/to/issue-444-worktree
benchmark_dir=$(mktemp -d /tmp/casa-444-benchmark.XXXXXX)

"$stable" -L "$main_dir/lib" "$main_dir/casa.casa" \
    -o "$benchmark_dir/main-stage1" --keep-asm
"$benchmark_dir/main-stage1" -L "$main_dir/lib" \
    "$main_dir/casa.casa" -o "$benchmark_dir/main-stage2" --keep-asm
"$benchmark_dir/main-stage2" -L "$main_dir/lib" \
    "$main_dir/casa.casa" -o "$benchmark_dir/main-stage3" --keep-asm
diff -q "$benchmark_dir/main-stage2.s" "$benchmark_dir/main-stage3.s"

"$stable" -L "$branch_dir/lib" "$branch_dir/casa.casa" \
    -o "$benchmark_dir/branch-stage1" --keep-asm
"$benchmark_dir/branch-stage1" -L "$branch_dir/lib" \
    "$branch_dir/casa.casa" -o "$benchmark_dir/branch-stage2" --keep-asm
"$benchmark_dir/branch-stage2" -L "$branch_dir/lib" \
    "$branch_dir/casa.casa" -o "$benchmark_dir/branch-stage3" --keep-asm
diff -q "$benchmark_dir/branch-stage2.s" \
    "$benchmark_dir/branch-stage3.s"
```

Run the generated corpus with both fixed-point compilers:

```sh
BENCHMARK_RUNS=3 "$branch_dir/docs/benchmarks/operation-rewrites.sh" \
    "$benchmark_dir/main-stage3" 1000 2000 4000 8000
BENCHMARK_RUNS=3 "$branch_dir/docs/benchmarks/operation-rewrites.sh" \
    "$benchmark_dir/branch-stage3" 1000 2000 4000 8000
```

## Results

| Rewrite groups | Source operations | Main analysis | Branch analysis | Branch vs main | Main doubling | Branch doubling |
|---:|---:|---:|---:|---:|---:|---:|
| 1,000 | 2,003 | 0.751 s | 0.639 s | -15% | n/a | n/a |
| 2,000 | 4,003 | 1.652 s | 1.332 s | -19% | 2.20x | 2.08x |
| 4,000 | 8,003 | 3.758 s | 2.599 s | -31% | 2.27x | 1.95x |
| 8,000 | 16,003 | 9.806 s | 5.258 s | -46% | 2.61x | 2.02x |

The branch remains near-linear. Analysis costs 0.639 to 0.666 seconds per
1,000 rewrite groups at every size. Main costs rise from 0.751 to 1.226
seconds per 1,000 groups because each middle insertion shifts more remaining
operations.

Wall-time medians include bytecode generation, assembly emission, and linking:

| Rewrite groups | Main wall time | Branch wall time |
|---:|---:|---:|
| 1,000 | 0.77 s | 0.66 s |
| 2,000 | 1.68 s | 1.37 s |
| 4,000 | 3.81 s | 2.65 s |
| 8,000 | 9.90 s | 5.36 s |

Raw analysis samples, in execution order:

| Revision | 1,000 | 2,000 | 4,000 | 8,000 |
|---|---|---|---|---|
| Main | 0.747, 0.772, 0.751 | 1.652, 1.567, 1.717 | 3.758, 3.859, 3.665 | 9.356, 10.027, 9.806 |
| Branch | 0.635, 0.639, 0.651 | 1.473, 1.329, 1.332 | 2.540, 2.599, 2.827 | 5.043, 5.264, 5.258 |
