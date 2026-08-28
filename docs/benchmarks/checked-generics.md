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
| Self-compilation | 15.200 s | 20.130 s | +4.930 s, +32.43% | 0.700 s | 0.850 s |
| Trait-heavy test | 14.850 s | 19.065 s | +4.215 s, +28.38% | 0.430 s | 0.780 s |
| Iterator-heavy test | 1.330 s | 1.465 s | +0.135 s, +10.15% | 0.130 s | 0.200 s |

Produced executable sizes:

| Corpus | Main | Branch | Branch difference |
|---|---:|---:|---:|
| Self-compilation | 3,139,480 bytes | 3,526,608 bytes | +387,128 bytes, +12.33% |
| Trait-heavy test | 2,625,712 bytes | 2,983,624 bytes | +357,912 bytes, +13.63% |
| Iterator-heavy test | 163,728 bytes | 140,336 bytes | -23,392 bytes, -14.29% |

Raw samples in execution order:

| Corpus | Main samples, seconds | Branch samples, seconds |
|---|---|---|
| Self-compilation | 15.29, 15.05, 15.09, 15.40, 15.39, 14.94, 14.86, 15.14, 15.56, 15.26 | 19.74, 19.90, 20.12, 20.47, 19.83, 20.45, 19.62, 20.14, 20.15, 20.17 |
| Trait-heavy test | 14.86, 14.61, 14.85, 15.00, 14.75, 14.92, 14.57, 14.85, 14.96, 14.82 | 19.05, 19.08, 19.02, 19.45, 19.16, 18.94, 19.11, 18.70, 18.67, 19.39 |
| Iterator-heavy test | 1.43, 1.32, 1.31, 1.33, 1.32, 1.33, 1.37, 1.34, 1.30, 1.33 | 1.60, 1.41, 1.48, 1.43, 1.45, 1.43, 1.49, 1.40, 1.51, 1.50 |

The branch regresses compilation time on all three corpora. It increases the
self-compiled compiler and trait-heavy executable sizes. It reduces the
iterator-heavy executable size.
