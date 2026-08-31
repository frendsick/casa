# Ownership model benchmark

Run date: 2026-08-31

This benchmark compares the compiler before affine ownership at `17eede0` with
the completed ownership model at `82720ba`. The earlier revision is the first
parent of the #337 merge. The current revision includes affine values, borrow
tracking, deterministic destruction, closure ownership, ownership-sensitive
patterns, and recursive owners.

Both measured compilers are self-compiled fixed points. Stage 2 and stage 3
produce identical assembly. The benchmark records wall time and peak resident
set size (RSS) with GNU `/usr/bin/time -f '%e %M'`.

## Environment

- Linux 6.18.33.2 under WSL2
- AMD Ryzen 7 3700X, 8 cores and 16 logical CPUs
- 8 MiB process stack
- Stable v1.18.0 compiler starts the pre-migration chain
- Stable v1.45.0 compiler starts the completed-model chain
- One warm-up before five self-compilation runs
- One warm-up before ten focused compile and runtime runs
- Alternating pre-migration and completed-model measurements

## Workloads

Self-compilation uses each revision's `casa.casa`, compiler modules, and library.
The repository compiler and library source footprint grew from 24,033 lines and
886,855 bytes to 44,083 lines and 1,666,974 bytes. This is an 83% line increase
and an 88% byte increase.

The version-specific `ownership-model-before.casa` and
`ownership-model-after.casa` programs perform the same work. Each program builds
20,000 batches of 64 integers, stores each list in an enum, captures the enum in
a closure, matches and traverses it, and produces checksum `40320000`. The
completed-model version uses a shared borrow for traversal and a moving closure
capture. Its list, enum, and closure storage is destroyed after each iteration.
The pre-migration version has no ownership-sensitive destruction.

The diagnostics workload compiles 11 existing ownership error fixtures. It
covers moves, borrow escape, borrow-to-owner conversion, closure capture,
aggregate origins, repeatable closure restoration, and collection loans. The
pre-migration compiler has no equivalent ownership diagnostics, so this workload
records only the completed model's rejection cost.

The recursive-owner workload builds an `Option[Node]` chain in a loop, then
destroys it. The source depth is changed mechanically to find the practical
call-stack limit of generated recursive destruction. An internal monotonic
timer starts after construction and stops after destruction.

The large inline `Copy` workload copies a 2,048-byte nested fixed array five
million times. A mechanically generated control binary runs the same loop with
the copy disabled. This separates 10.24 GB of copying from loop overhead.

## Fixed-point build

Run these commands from the benchmark branch:

```sh
git worktree add --detach /tmp/casa-362-before \
    17eede0d6ed09e605eace93f1c05d1727328c6af
mkdir -p /tmp/casa-362-benchmark/release-v1.18.0
gh release download v1.18.0 --pattern casac \
    --dir /tmp/casa-362-benchmark/release-v1.18.0
chmod u+x /tmp/casa-362-benchmark/release-v1.18.0/casac

before=/tmp/casa-362-before
branch=$PWD
benchmark=/tmp/casa-362-benchmark

"$benchmark/release-v1.18.0/casac" -L "$before/lib" \
    "$before/casa.casa" -o "$benchmark/before-stage1" --keep-asm
"$benchmark/before-stage1" -L "$before/lib" "$before/casa.casa" \
    -o "$benchmark/before-stage2" --keep-asm
"$benchmark/before-stage2" -L "$before/lib" "$before/casa.casa" \
    -o "$benchmark/before-stage3" --keep-asm
diff -q "$benchmark/before-stage2.s" "$benchmark/before-stage3.s"

./install.sh
./casac -L lib casa.casa -o "$benchmark/after-stage1" --keep-asm
"$benchmark/after-stage1" -L lib casa.casa \
    -o "$benchmark/after-stage2" --keep-asm
"$benchmark/after-stage2" -L lib casa.casa \
    -o "$benchmark/after-stage3" --keep-asm
diff -q "$benchmark/after-stage2.s" "$benchmark/after-stage3.s"
```

## Self-compilation

The median is the middle of five runs. Spread is the minimum through maximum.

| Metric | Pre-migration | Completed model | Change |
|---|---:|---:|---:|
| Wall-time median | 4.31 s | 28.89 s | +24.58 s, +570% |
| Wall-time spread | 4.12-4.60 s | 27.74-31.46 s | n/a |
| Peak RSS median | 977,500 KiB | 756,072 KiB | -221,428 KiB, -22.7% |
| Compiler binary | 1,121,288 B | 4,198,192 B | +3,076,904 B, +274% |

Raw samples are in execution order:

| Revision | Wall time, seconds | Peak RSS, KiB |
|---|---|---|
| Pre-migration | 4.20, 4.31, 4.12, 4.31, 4.60 | 977500, 977628, 977628, 977500, 977500 |
| Completed model | 27.74, 28.89, 28.42, 31.46, 31.17 | 756072, 756072, 755944, 756200, 756072 |

One verbose run separated the cumulative progress timestamps into phases:

| Phase | Pre-migration | Completed model | Change |
|---|---:|---:|---:|
| Analysis | 2.551 s | 24.624 s | +22.073 s, +865% |
| Bytecode generation | 0.308 s | 2.960 s | +2.652 s, +861% |
| Assembly emission and native build | 1.238 s | 1.753 s | +0.515 s, +42% |

The completed compiler uses less peak memory, but its compile time and binary
size are material regressions. Source growth explains part of the result, but
the focused workload below also shows a large front-end increase.

## Ownership-heavy compile and runtime cost

Compile both version-specific programs with their matching fixed-point compiler
and library:

```sh
"$benchmark/before-stage3" -L "$before/lib" \
    "$branch/docs/benchmarks/ownership-model-before.casa" \
    -o "$benchmark/ownership-before"
"$benchmark/after-stage3" -L "$branch/lib" \
    "$branch/docs/benchmarks/ownership-model-after.casa" \
    -o "$benchmark/ownership-after"
```

| Metric | Pre-migration | Completed model | Change |
|---|---:|---:|---:|
| Compile median | 0.090 s | 0.860 s | +0.770 s, +856% |
| Compile spread | 0.09-0.12 s | 0.83-0.89 s | n/a |
| Compile peak RSS median | 27,484 KiB | 31,720 KiB | +4,236 KiB, +15.4% |
| Binary size | 11,328 B | 16,816 B | +5,488 B, +48.4% |
| Runtime median | 0.100 s | 0.085 s | -0.015 s, -15.0% |
| Runtime spread | 0.09-0.11 s | 0.07-0.11 s | n/a |
| Runtime peak RSS median | 19,840 KiB | 136 KiB | -19,704 KiB, -99.3% |

The current model completes the same work 15% faster and keeps runtime RSS
bounded by reclaiming each batch. The pre-migration program retains every list
allocation. The current compile and binary costs remain material.

Raw compile samples:

| Revision | Wall time, seconds | Peak RSS, KiB |
|---|---|---|
| Pre-migration | 0.09, 0.09, 0.09, 0.09, 0.10, 0.10, 0.10, 0.09, 0.12, 0.09 | 27484 for every run |
| Completed model | 0.89, 0.88, 0.84, 0.85, 0.83, 0.87, 0.87, 0.85, 0.83, 0.87 | 31592-31848 |

Raw runtime samples:

| Revision | Wall time, seconds | Peak RSS, KiB |
|---|---|---|
| Pre-migration | 0.09, 0.10, 0.10, 0.09, 0.10, 0.09, 0.11, 0.10, 0.09, 0.10 | 19712-19840 |
| Completed model | 0.10, 0.07, 0.07, 0.08, 0.09, 0.10, 0.10, 0.08, 0.07, 0.11 | 136 for every run |

## Large inline Copy cost

The 2,048-byte copy loop has a 0.23-second median. The control loop has a
0.01-second median. Subtracting loop overhead gives 0.22 seconds for 10.24 GB,
or 46.5 GB/s. Peak RSS is 136 KiB in every run for both binaries.

| Workload | Wall-time median | Spread | Heap allocation calls |
|---|---:|---:|---:|
| Copy disabled | 0.01 s | 0.01-0.01 s | 2 |
| 5,000,000 copies | 0.23 s | 0.22-0.25 s | 2 |

The two allocation calls construct the nested array before the measured loop.
GDB breaks on `heap_alloc` for the complete process. The unchanged call count
confirms that five million `copy` operations introduce no allocation.

```sh
sed 's/^const COPY_VALUES true/const COPY_VALUES false/' \
    docs/benchmarks/ownership-copy.casa \
    > "$benchmark/ownership-copy-empty.casa"
"$benchmark/after-stage3" -L lib docs/benchmarks/ownership-copy.casa \
    -o "$benchmark/ownership-copy" --keep-asm
"$benchmark/after-stage3" -L lib "$benchmark/ownership-copy-empty.casa" \
    -o "$benchmark/ownership-copy-empty" --keep-asm

for run in 1 2 3 4 5 6 7 8 9 10; do
    /usr/bin/time -f '%e %M' "$benchmark/ownership-copy-empty"
    /usr/bin/time -f '%e %M' "$benchmark/ownership-copy"
done

gdb -q -batch -x docs/benchmarks/ownership-copy.gdb \
    --args "$benchmark/ownership-copy-empty"
gdb -q -batch -x docs/benchmarks/ownership-copy.gdb \
    --args "$benchmark/ownership-copy"
```

## Diagnostics

The three complete diagnostic workload runs took 8.28, 8.51, and 8.61 seconds.
The median is 8.51 seconds and peak RSS is 31,336 KiB. Every compile produced
the expected ownership error. Ten fixtures import the standard library and take
0.78-0.93 seconds each. The self-contained borrow-to-owner fixture takes less
than 0.01 seconds.

```sh
diagnostics='array_literal_moves_element borrowed_temporary_escape
borrowed_value_to_owned_parameter closure_capture_after_move
closure_capture_conflicting_borrow closure_capture_keeps_owner_loaned
fstring_capture_after_move moving_closure_preserves_nested_borrow
option_result_owner_reuse repeatable_closure_consumes_capture
slice_keeps_list_borrowed'

for run in 1 2 3; do
    for name in $diagnostics; do
        /usr/bin/time -f "$run $name %e %M" \
            "$benchmark/after-stage3" -L lib \
            "tests/compiler/errors/$name.casa" -o "$benchmark/error-output"
        test "$?" -ne 0
    done
done
```

## Recursive destruction limit

With the default 8 MiB stack, depth 86,250 completes and depth 87,500 terminates
with `SIGSEGV`. The limit is environment-dependent because generated destruction
uses one native call frame per recursive owner level. The accepted call-stack
implementation is practical for the tested 128-level repository cases. An
iterative implementation is required if valid programs need chains near 86,000
levels on this stack configuration.

The source prints nanoseconds measured only around `next drop`. Construction and
allocation finish before the timer starts. Each successful result is the median
of three runs.

| Chain depth | Destruction median |
|---:|---:|
| 1,000 | 25,372 ns |
| 10,000 | 367,344 ns |
| 40,000 | 1,412,579 ns |
| 80,000 | 2,553,463 ns |
| 86,250 | 2,982,542 ns |
| 87,500 | `SIGSEGV` |

```sh
ulimit -c 0
for depth in 1000 10000 40000 80000 86250 87500; do
    sed "s/^const DEPTH .*/const DEPTH $depth/" \
        docs/benchmarks/ownership-recursion.casa \
        > "$benchmark/ownership-recursion-$depth.casa"
    "$benchmark/after-stage3" -L lib \
        "$benchmark/ownership-recursion-$depth.casa" \
        -o "$benchmark/ownership-recursion-$depth"
    for run in 1 2 3; do
        "$benchmark/ownership-recursion-$depth"
    done
done
```

## Design decision

The memory and runtime results support deterministic destruction and reusable
storage. The compile-time and binary-size regressions are not accepted by this
report. Issue #588 records the measured costs and requests an explicit budget or
a reduction. It is a native blocker of #362 so the regression cannot be closed
without a decision.

The comparison spans 319 commits and includes source growth and language work
beyond ownership. It does not attribute the complete regression to ownership.
The equivalent focused workload still increases analysis from 0.081 to 0.828
seconds, so source growth in `casa.casa` does not explain the full front-end cost.
