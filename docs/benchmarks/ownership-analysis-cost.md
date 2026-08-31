# Ownership analysis cost benchmark

Run date: 2026-08-31

This benchmark compares `origin/main` at `3a26a31` with the #588 ownership
analysis cost changes in this document's commit. Both are fixed-point compiler
builds bootstrapped with stable Casa v1.46.0. Stage 2 and
stage 3 assembly matched for each compiler.

The pre-migration values come from the #362 ownership model benchmark at
`17eede0`. They provide historical context. The paired main and branch results
identify the effect of #588 without mixing in the 327 commits between the
pre-migration compiler and current main.

## Environment

- Linux 6.18.33.2 under WSL2
- AMD Ryzen 7 3700X, 8 cores and 16 logical CPUs
- One warm-up followed by three alternating self-compilation measurements
- One warm-up followed by ten alternating focused-workload measurements
- Wall time and peak resident set size (RSS) from GNU `time`
- Phase times from the compiler's `--verbose` progress timestamps

## Fixed-point build

Run these commands from the #588 branch. `main_dir` must not exist before the
setup. `branch_dir` names the #588 worktree.

```sh
main_dir=/tmp/casa-588-main
branch_dir=$PWD
benchmark_dir=$(mktemp -d /tmp/casa-588.XXXXXX)

git worktree add --detach "$main_dir" \
    3a26a318da6d8f58c5352df32b3200970f446792
./install.sh
stable="$branch_dir/casac"

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

## Measurement

The self-compilation measurements used each compiler with its matching source
tree. The focused measurements used the branch library and
`ownership-model-after.casa` for both compilers.

```sh
"$benchmark_dir/main-stage3" -L "$main_dir/lib" "$main_dir/casa.casa" \
    -o "$benchmark_dir/warm-main"
"$benchmark_dir/branch-stage3" -L "$branch_dir/lib" \
    "$branch_dir/casa.casa" -o "$benchmark_dir/warm-branch"

: > "$benchmark_dir/main-self.times"
: > "$benchmark_dir/branch-self.times"
for iteration in 1 2 3; do
    /usr/bin/time -f "$iteration %e %M" \
        -o "$benchmark_dir/main-self.times" -a \
        "$benchmark_dir/main-stage3" --verbose -L "$main_dir/lib" \
        "$main_dir/casa.casa" -o "$benchmark_dir/measured-main" \
        2> "$benchmark_dir/main-self-$iteration.stderr"
    /usr/bin/time -f "$iteration %e %M" \
        -o "$benchmark_dir/branch-self.times" -a \
        "$benchmark_dir/branch-stage3" --verbose -L "$branch_dir/lib" \
        "$branch_dir/casa.casa" -o "$benchmark_dir/measured-branch" \
        2> "$benchmark_dir/branch-self-$iteration.stderr"
done

focus="$branch_dir/docs/benchmarks/ownership-model-after.casa"
"$benchmark_dir/main-stage3" -L "$branch_dir/lib" "$focus" \
    -o "$benchmark_dir/warm-focus-main"
"$benchmark_dir/branch-stage3" -L "$branch_dir/lib" "$focus" \
    -o "$benchmark_dir/warm-focus-branch"

: > "$benchmark_dir/main-focus.times"
: > "$benchmark_dir/branch-focus.times"
for iteration in $(seq 1 10); do
    /usr/bin/time -f "$iteration %e %M" \
        -o "$benchmark_dir/main-focus.times" -a \
        "$benchmark_dir/main-stage3" --verbose -L "$branch_dir/lib" \
        "$focus" -o "$benchmark_dir/measured-focus-main" \
        2> "$benchmark_dir/main-focus-$iteration.stderr"
    /usr/bin/time -f "$iteration %e %M" \
        -o "$benchmark_dir/branch-focus.times" -a \
        "$benchmark_dir/branch-stage3" --verbose -L "$branch_dir/lib" \
        "$focus" -o "$benchmark_dir/measured-focus-branch" \
        2> "$benchmark_dir/branch-focus-$iteration.stderr"
done
```

Analysis time is the `Compiling bytecode` timestamp. Bytecode generation time
is the interval from `Compiling bytecode` through `Emitting assembly`.

## Profile

Temporary `timer::Timer` instrumentation first isolated the analysis paths at
`d759c20`. One instrumented run measured these two complete intervals. The
instrumentation added function-level timers, so these values identify expensive
paths but are not used as benchmark results.

| Analysis path | Time |
|---|---:|
| Generic monomorphization within typecheck | 5.884 s |
| Bytecode generation | 2.649 s |

The later #595 Clone refactor replaced manual clones with derived clones but
retained both full-copy paths.

Two deep-copy paths were avoidable:

- `monomorphize_ops` called `monomorphize_named_operation` for every operation.
  Each named call or function reference cloned the complete target function
  before the code knew whether it needed a generic specialization.
- `BytecodeCompiler.compile_ops` cloned the complete checked operation tree.
  Bytecode ownership queries then cloned complete `OpOwnership` records when
  they needed only assignments, cleanups, or the `moves_owner` flag.

The branch restricts named-operation work to calls and function references. It
defers function and binding clones until specialization needs them. Bytecode
compilation reads the checked operation tree and copies only the ownership event
list that each lowering path consumes.

The executable size increase is generated code, not ownership metadata stored
in the binary. From the pre-migration compiler to current main, the ELF `.text`
section increased from 827,114 to 3,266,883 bytes. It accounts for 79% of the
file-size increase. Defined Casa function symbols increased from 1,762 to 5,167.
The symbol and string tables account for another 19% of the increase. The
compiler and library source set grew from 24,033 to 43,610 lines, or 81.46%, and
from 886,855 to 1,641,095 bytes, or 85.05%. This growth and checked generic
monomorphization explain the larger function set. The #588 changes do not remove
those required specializations.

## Results

The branch improves self-compilation while leaving the focused workload and
output sizes unchanged within measurement precision.

| Self-compilation metric | Pre-migration reference | Current main | Branch | Branch vs main |
|---|---:|---:|---:|---:|
| Wall-time median | 4.31 s | 27.27 s | 25.47 s | -1.80 s, -6.60% |
| Analysis | 2.551 s | 20.796 s | 19.451 s | -1.345 s, -6.47% |
| Bytecode | 0.308 s | 2.571 s | 2.258 s | -0.313 s, -12.17% |
| Peak RSS median | 977,500 KiB | 630,776 KiB | 574,836 KiB | -55,940 KiB, -8.87% |
| Compiler binary | 1,121,288 B | 4,220,520 B | 4,216,960 B | -3,560 B, -0.08% |

| Focused metric | Pre-migration reference | Current main | Branch | Branch vs main |
|---|---:|---:|---:|---:|
| Compile median | 0.090 s | 0.740 s | 0.760 s | +0.020 s, +2.70% |
| Analysis | 0.081 s | 0.714 s | 0.730 s | +0.017 s, +2.31% |
| Peak RSS median | 27,484 KiB | 26,616 KiB | 26,612 KiB | -4 KiB, -0.02% |
| Binary size | 11,328 B | 16,856 B | 16,856 B | unchanged |

Current-main and branch time entries are medians. The pre-migration wall and
focused compile entries are medians from #362. Its analysis and bytecode entries
are single verbose-run reference values.

Raw self-compilation samples are in execution order:

| Revision | Wall time, seconds | Peak RSS, KiB | Analysis, seconds | Bytecode, seconds |
|---|---|---|---|---|
| Current main | 27.17, 27.27, 27.94 | 630776, 630904, 630648 | 20.525, 20.796, 21.688 | 2.744, 2.571, 2.526 |
| Branch | 23.87, 25.96, 25.47 | 574964, 574836, 574708 | 18.967, 19.782, 19.451 | 2.258, 2.359, 2.254 |

Raw focused-workload samples are in execution order:

| Revision | Wall time, seconds | Peak RSS, KiB | Analysis, seconds |
|---|---|---|---|
| Current main | 0.79, 0.71, 0.71, 0.71, 0.81, 0.78, 0.75, 0.73, 0.71, 0.79 | 26616, 26616, 26616, 26488, 26616, 26616, 26488, 26616, 26616, 26488 | 0.759, 0.680, 0.684, 0.679, 0.775, 0.746, 0.721, 0.706, 0.680, 0.762 |
| Branch | 0.76, 0.71, 0.76, 0.71, 0.78, 0.76, 0.72, 0.70, 0.80, 0.78 | 26612, 26612, 26612, 26484, 26612, 26612, 26612, 26612, 26612, 26612 | 0.731, 0.682, 0.729, 0.678, 0.749, 0.735, 0.691, 0.673, 0.764, 0.752 |

## Accepted budget

The current ownership model is accepted with these limits on the measured
environment and workloads:

| Metric | Budget |
|---|---:|
| Self-compilation wall-time median | 26.0 s |
| Self-compilation analysis median | 20.0 s |
| Self-compilation bytecode median | 2.5 s |
| Self-compilation peak RSS median | 600,000 KiB |
| Compiler binary | 4,225,000 B |
| Focused compile median | 0.85 s |
| Focused analysis median | 0.80 s |
| Focused peak RSS median | 27,000 KiB |
| Focused binary | 17,000 B |

Use medians after one warm-up and alternating runs. Measurements on a different
machine must compare against a fixed-point `origin/main` compiler on the same
machine instead of applying these absolute time and RSS limits.

The budget accepts the remaining cost because the ownership model provides
move diagnostics, borrow validation, and deterministic destruction. The #362
runtime workload also runs faster and keeps memory bounded because it releases
each batch. Compiler and library growth plus checked generic monomorphization
account for most of the binary increase. Revisit the implementation when a
same-environment median or produced binary exceeds a listed limit. Preserve the
ownership diagnostics and fixed-point requirement when changing the budget.
