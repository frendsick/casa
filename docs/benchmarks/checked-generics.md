# Checked generic body monomorphization benchmark

Run date: 2026-08-28

This benchmark compares `origin/main` at `6ca6d69` with the checked generic
body monomorphization implementation in this document's commit. Both measured
compilers are self-compiled fixed points bootstrapped with stable Casa v1.40.0.

The trait and iterator measurements use the same branch source files and
library path for both compilers. The self-compilation measurement uses each
compiler's own source tree.

## Environment

- Linux 6.18.33.2 under WSL2
- AMD Ryzen 7 3700X, 8 cores and 16 logical CPUs
- Ten measured runs after one warm-up run per compiler and corpus
- Main then branch, repeated for each corpus
- Wall time from `/usr/bin/time -f %e`

## Setup

The main worktree was pinned before the benchmark:

```sh
git worktree add --detach /tmp/casa-336-main-benchmark-v140 6ca6d69
```

Each compiler used a three-stage build. The stage 2 and stage 3 assembly files
matched before measurement:

```sh
/tmp/casa-336-bootstrap-v1.40.0/casac \
    -L /tmp/casa-336-main-benchmark-v140/lib \
    /tmp/casa-336-main-benchmark-v140/casa.casa \
    -o /tmp/casac-336-v140-main-stage1 --keep-asm
/tmp/casac-336-v140-main-stage1 \
    -L /tmp/casa-336-main-benchmark-v140/lib \
    /tmp/casa-336-main-benchmark-v140/casa.casa \
    -o /tmp/casac-336-v140-main-stage2 --keep-asm
/tmp/casac-336-v140-main-stage2 \
    -L /tmp/casa-336-main-benchmark-v140/lib \
    /tmp/casa-336-main-benchmark-v140/casa.casa \
    -o /tmp/casac-336-v140-main-stage3 --keep-asm
diff -q /tmp/casac-336-v140-main-stage2.s \
    /tmp/casac-336-v140-main-stage3.s

/tmp/casa-336-bootstrap-v1.40.0/casac \
    -L lib casa.casa \
    -o /tmp/casac-336-v140-branch-stage1 --keep-asm
/tmp/casac-336-v140-branch-stage1 \
    -L lib casa.casa \
    -o /tmp/casac-336-v140-branch-stage2 --keep-asm
/tmp/casac-336-v140-branch-stage2 \
    -L lib casa.casa \
    -o /tmp/casac-336-v140-branch-stage3 --keep-asm
diff -q /tmp/casac-336-v140-branch-stage2.s \
    /tmp/casac-336-v140-branch-stage3.s
```

## Measurement

The measured corpora were `casa.casa`,
`tests/compiler/test_traits.casa`, and
`tests/compiler/test_iterator_combinators.casa`. Each command wrote to a fixed
path under `/tmp`. The following loop shows the trait measurement. The other
corpora used the same alternating structure.

```sh
for iteration in $(seq 1 10); do
    /usr/bin/time -f '%e' -o traits-main.times -a \
        /tmp/casac-336-v140-main-stage3 \
        -L lib tests/compiler/test_traits.casa \
        -o /tmp/casa-336-measured-main-traits
    /usr/bin/time -f '%e' -o traits-branch.times -a \
        /tmp/casac-336-v140-branch-stage3 \
        -L lib tests/compiler/test_traits.casa \
        -o /tmp/casa-336-measured-branch-traits
done
```

## Results

Spread is the maximum time minus the minimum time.

| Corpus | Main median | Branch median | Branch difference | Main spread | Branch spread |
|---|---:|---:|---:|---:|---:|
| Self-compilation | 15.255 s | 19.980 s | +4.725 s, +30.97% | 0.770 s | 0.740 s |
| Trait-heavy test | 14.750 s | 18.855 s | +4.105 s, +27.83% | 0.370 s | 0.490 s |
| Iterator-heavy test | 1.310 s | 1.450 s | +0.140 s, +10.69% | 0.090 s | 0.190 s |

Produced executable sizes:

| Corpus | Main | Branch | Branch difference |
|---|---:|---:|---:|
| Self-compilation | 3,139,488 bytes | 3,526,608 bytes | +387,120 bytes, +12.33% |
| Trait-heavy test | 2,625,720 bytes | 2,983,632 bytes | +357,912 bytes, +13.63% |
| Iterator-heavy test | 163,728 bytes | 140,336 bytes | -23,392 bytes, -14.29% |

Raw samples in execution order:

| Corpus | Main samples, seconds | Branch samples, seconds |
|---|---|---|
| Self-compilation | 15.63, 15.15, 15.21, 14.86, 15.41, 15.18, 15.39, 15.40, 15.30, 14.91 | 20.06, 19.90, 20.45, 19.89, 20.26, 19.76, 19.81, 20.26, 20.10, 19.71 |
| Trait-heavy test | 14.82, 14.74, 14.76, 14.66, 14.80, 14.58, 14.83, 14.79, 14.46, 14.50 | 18.55, 18.67, 18.99, 18.95, 18.86, 19.04, 18.85, 18.59, 18.77, 19.02 |
| Iterator-heavy test | 1.39, 1.30, 1.31, 1.30, 1.31, 1.30, 1.31, 1.33, 1.30, 1.38 | 1.59, 1.40, 1.40, 1.41, 1.40, 1.51, 1.50, 1.49, 1.52, 1.40 |

The branch regresses compilation time on all three corpora. It increases the
self-compiled compiler and trait-heavy executable sizes. It reduces the
iterator-heavy executable size.
