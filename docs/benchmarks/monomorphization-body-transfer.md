# Monomorphization body-transfer benchmark

Run date: 2026-08-30

This benchmark compares `origin/main` at `ba9e563` with the implementation for
#551. Both compilers were self-compiled fixed points bootstrapped with stable
Casa v1.41.0. Stage 2 and stage 3 assembly matched for each compiler.

## Environment

- Linux 6.18.33.2 under WSL2
- AMD Ryzen 7 3700X, 8 cores and 16 logical CPUs
- One warm-up followed by three alternating main and branch measurements
- Self-compilation of each compiler's own `casa.casa` and source tree
- Wall time and peak RSS from GNU `time`

## Setup

The main source worktree was pinned at `ba9e563`. `branch_dir` names a worktree
at the #551 commit.

```sh
benchmark_dir=/tmp/casa-551-benchmark
main_dir=/tmp/casa-551-main
branch_dir=/path/to/casa-551
main_phase_dir=/tmp/casa-551-phase-main
branch_phase_dir=/tmp/casa-551-phase-branch
stable=/path/to/casac-v1.41.0
branch_commit=$(git -C "$branch_dir" rev-parse HEAD)

mkdir -p "$benchmark_dir"
git worktree add --detach "$main_dir" ba9e563
git worktree add --detach "$main_phase_dir" ba9e563
git worktree add --detach "$branch_phase_dir" "$branch_commit"

"$stable" -L "$main_dir/lib" "$main_dir/casa.casa" \
    -o "$benchmark_dir/main-stage1" --keep-asm
"$benchmark_dir/main-stage1" -L "$main_dir/lib" "$main_dir/casa.casa" \
    -o "$benchmark_dir/main-stage2" --keep-asm
"$benchmark_dir/main-stage2" -L "$main_dir/lib" "$main_dir/casa.casa" \
    -o "$benchmark_dir/main-stage3" --keep-asm
diff -q "$benchmark_dir/main-stage2.s" "$benchmark_dir/main-stage3.s"

"$stable" -L "$branch_dir/lib" "$branch_dir/casa.casa" \
    -o "$benchmark_dir/branch-stage1" --keep-asm
"$benchmark_dir/branch-stage1" -L "$branch_dir/lib" "$branch_dir/casa.casa" \
    -o "$benchmark_dir/branch-stage2" --keep-asm
"$benchmark_dir/branch-stage2" -L "$branch_dir/lib" "$branch_dir/casa.casa" \
    -o "$benchmark_dir/branch-stage3" --keep-asm
diff -q "$benchmark_dir/branch-stage2.s" "$benchmark_dir/branch-stage3.s"
```

The phase builds added `timer::Timer` immediately around
`monomorphize_checked_generics` and printed `elapsed_ns`. The fixed-point main
and branch compilers built their corresponding instrumented source trees.
The instrumentation was not included in the measured source or final change.
Apply this temporary patch to `compiler/typechecker.casa` in copies of both
source trees:

```diff
@@
 import "pattern" as pattern
+import "timer" as timer
@@
     if diagnostics.has_errors ! then
+        timer::Timer::new = monomorphization_timer
         diagnostics store ops monomorphize_checked_generics
+        f"monomorphization {monomorphization_timer.elapsed_ns}" std::eprintln_string
     fi
```

Build each instrumented tree with its corresponding stage 3 compiler:

```sh
"$benchmark_dir/main-stage3" -L "$main_phase_dir/lib" \
    "$main_phase_dir/casa.casa" -o "$benchmark_dir/main-phase"
"$benchmark_dir/branch-stage3" -L "$branch_phase_dir/lib" \
    "$branch_phase_dir/casa.casa" -o "$benchmark_dir/branch-phase"
```

## Measurement

After one warm-up per compiler, wall time and RSS used these commands for three
alternating pairs:

```sh
"$benchmark_dir/main-stage3" -L "$main_dir/lib" "$main_dir/casa.casa" \
    -o "$benchmark_dir/warm-main"
"$benchmark_dir/branch-stage3" -L "$branch_dir/lib" "$branch_dir/casa.casa" \
    -o "$benchmark_dir/warm-branch"

for iteration in 1 2 3; do
    /usr/bin/time -f "main $iteration %e %M" \
        "$benchmark_dir/main-stage3" -L "$main_dir/lib" \
        "$main_dir/casa.casa" -o "$benchmark_dir/measured-main"
    /usr/bin/time -f "branch $iteration %e %M" \
        "$benchmark_dir/branch-stage3" -L "$branch_dir/lib" \
        "$branch_dir/casa.casa" -o "$benchmark_dir/measured-branch"
done
```

The phase samples used the same alternating order and summed the reported
intervals. Self-compilation runs one root monomorphization interval.

```sh
"$benchmark_dir/main-phase" -L "$main_dir/lib" "$main_dir/casa.casa" \
    -o "$benchmark_dir/warm-main-phase" >/dev/null 2>&1
"$benchmark_dir/branch-phase" -L "$branch_dir/lib" "$branch_dir/casa.casa" \
    -o "$benchmark_dir/warm-branch-phase" >/dev/null 2>&1

for iteration in 1 2 3; do
    set -o pipefail
    "$benchmark_dir/main-phase" -L "$main_dir/lib" "$main_dir/casa.casa" \
        -o "$benchmark_dir/measured-main-phase" 2>&1 |
        awk -v run="$iteration" '/^monomorphization / { sum += $2 } \
            END { printf "main %d %.6f\n", run, sum / 1000000000 }'
    "$benchmark_dir/branch-phase" -L "$branch_dir/lib" \
        "$branch_dir/casa.casa" -o "$benchmark_dir/measured-branch-phase" 2>&1 |
        awk -v run="$iteration" '/^monomorphization / { sum += $2 } \
            END { printf "branch %d %.6f\n", run, sum / 1000000000 }'
done
```

## Results

| Measurement | Main median | Branch median | Difference | Main samples | Branch samples |
|---|---:|---:|---:|---|---|
| Monomorphization | 7.126 s | 6.878 s | -0.248 s, -3.48% | 9.600, 7.126, 6.717 s | 7.351, 6.878, 5.719 s |
| Wall time | 28.52 s | 28.78 s | +0.26 s, +0.91% | 28.52, 28.48, 28.67 s | 28.78, 28.31, 28.95 s |
| Peak RSS | 761,564 KiB | 761,692 KiB | +128 KiB, +0.02% | 761,564, 761,436, 761,564 KiB | 761,564, 761,692, 761,692 KiB |

The body transfer reduced measured monomorphization time. Whole-process wall
time and peak RSS had no material regression.
