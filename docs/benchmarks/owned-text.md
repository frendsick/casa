# Owned text compiler benchmark

Run date: 2026-08-27

This benchmark compares `origin/main` at `204f4fd` with the owned `String`
implementation in this document's commit. Both measured binaries are
self-compiled fixed points. The commands use the same options and write outputs
under `/tmp`.

No stable compiler accepts both source contracts. v1.38.0 requires
`Display.to_str -> str`, while v1.39.0 requires `Display.to_str -> String`.
v1.38.0 starts the main chain and v1.39.0 starts the branch chain. Each chain
reaches a fixed point before measurement, so the timed compilers contain only
their own source implementation.

## Environment

- Linux 6.18.33.2 under WSL2
- AMD Ryzen 7 3700X, 8 cores and 16 logical CPUs
- Ten measured runs after one warm-up run per compiler and corpus
- Main then branch, repeated for each corpus
- Wall time from `/usr/bin/time -f %e`

## Setup and build commands

Run these commands from the branch worktree with unused `/tmp` paths:

```sh
git worktree add --detach /tmp/casa-514-benchmark-main 204f4fd
mkdir /tmp/casa-release-v1.38.0 /tmp/casa-release-v1.39.0
gh release download v1.38.0 --pattern casac --dir /tmp/casa-release-v1.38.0
gh release download v1.39.0 --pattern casac --dir /tmp/casa-release-v1.39.0
chmod u+x /tmp/casa-release-v1.38.0/casac /tmp/casa-release-v1.39.0/casac

/tmp/casa-release-v1.38.0/casac \
    -L /tmp/casa-514-benchmark-main/lib \
    /tmp/casa-514-benchmark-main/casa.casa \
    -o /tmp/casac-514-benchmark-main-stage1 --keep-asm
/tmp/casac-514-benchmark-main-stage1 \
    -L /tmp/casa-514-benchmark-main/lib \
    /tmp/casa-514-benchmark-main/casa.casa \
    -o /tmp/casac-514-benchmark-main --keep-asm
/tmp/casac-514-benchmark-main \
    -L /tmp/casa-514-benchmark-main/lib \
    /tmp/casa-514-benchmark-main/casa.casa \
    -o /tmp/casac-514-benchmark-main-stage3 --keep-asm
diff -q /tmp/casac-514-benchmark-main.s \
    /tmp/casac-514-benchmark-main-stage3.s

/tmp/casa-release-v1.39.0/casac \
    -L lib casa.casa \
    -o /tmp/casac-514-benchmark-branch-stage1 --keep-asm
/tmp/casac-514-benchmark-branch-stage1 \
    -L lib casa.casa \
    -o /tmp/casac-514-benchmark-branch --keep-asm
/tmp/casac-514-benchmark-branch \
    -L lib casa.casa \
    -o /tmp/casac-514-benchmark-branch-stage3 --keep-asm
diff -q /tmp/casac-514-benchmark-branch.s \
    /tmp/casac-514-benchmark-branch-stage3.s
```

## Measurement commands

The self-compilation corpus was `casa.casa`. The representative multi-file
corpus was `lsp.casa` with its compiler and library imports.

```sh
/tmp/casac-514-benchmark-main-stage3 \
    -L /tmp/casa-514-benchmark-main/lib \
    /tmp/casa-514-benchmark-main/casa.casa \
    -o /tmp/casa-514-warm-main-self
/tmp/casac-514-benchmark-branch-stage3 \
    -L lib casa.casa -o /tmp/casa-514-warm-branch-self
/tmp/casac-514-benchmark-main-stage3 \
    -L /tmp/casa-514-benchmark-main/lib \
    /tmp/casa-514-benchmark-main/lsp.casa \
    -o /tmp/casa-514-warm-main-lsp
/tmp/casac-514-benchmark-branch-stage3 \
    -L lib lsp.casa -o /tmp/casa-514-warm-branch-lsp

: > self-main.times
: > self-branch.times
: > lsp-main.times
: > lsp-branch.times

for iteration in $(seq 1 10); do
    /usr/bin/time -f '%e' -o self-main.times -a \
        /tmp/casac-514-benchmark-main-stage3 \
        -L /tmp/casa-514-benchmark-main/lib \
        /tmp/casa-514-benchmark-main/casa.casa \
        -o /tmp/casa-514-bench-main-self
    /usr/bin/time -f '%e' -o self-branch.times -a \
        /tmp/casac-514-benchmark-branch-stage3 \
        -L lib casa.casa \
        -o /tmp/casa-514-bench-branch-self
done

for iteration in $(seq 1 10); do
    /usr/bin/time -f '%e' -o lsp-main.times -a \
        /tmp/casac-514-benchmark-main-stage3 \
        -L /tmp/casa-514-benchmark-main/lib \
        /tmp/casa-514-benchmark-main/lsp.casa \
        -o /tmp/casa-514-bench-main-lsp
    /usr/bin/time -f '%e' -o lsp-branch.times -a \
        /tmp/casac-514-benchmark-branch-stage3 \
        -L lib lsp.casa \
        -o /tmp/casa-514-bench-branch-lsp
done

for corpus in self lsp; do
    for side in main branch; do
        sort -n "$corpus-$side.times" | awk -v corpus="$corpus" -v side="$side" \
            'NR==1 {min=$1} NR==5 {lower=$1} NR==6 {upper=$1} {max=$1}
             END {printf "%s %s median %.3f spread %.3f\n",
                         corpus, side, (lower+upper)/2, max-min}'
    done
done
```

## Results

Spread is the maximum time minus the minimum time.

| Corpus | Main median | Branch median | Branch difference | Main spread | Branch spread |
|---|---:|---:|---:|---:|---:|
| Self-compilation | 14.780 s | 14.370 s | -0.410 s, -2.77% | 0.600 s | 0.310 s |
| Multi-file LSP | 13.370 s | 13.190 s | -0.180 s, -1.35% | 0.810 s | 0.590 s |

The branch change is smaller than the observed run-to-run spread. It does not
show a compile-time regression on either corpus.

Raw samples in execution order:

| Corpus | Main samples, seconds | Branch samples, seconds |
|---|---|---|
| Self-compilation | 14.76, 14.68, 14.71, 15.27, 15.08, 14.78, 14.78, 15.03, 15.15, 14.67 | 14.44, 14.25, 14.39, 14.56, 14.35, 14.30, 14.26, 14.31, 14.42, 14.48 |
| Multi-file LSP | 13.34, 13.44, 13.05, 13.32, 13.24, 13.40, 13.48, 13.19, 13.86, 13.51 | 12.96, 13.16, 13.13, 13.24, 12.93, 13.29, 12.70, 13.24, 13.22, 13.27 |

## Allocation and source effects

`String.drop` now returns dynamic text storage to the reusable allocator.
Focused tests confirm that a released 1024-byte string buffer is reused and
that 100 post-warm-up iterations of clone, substring, numeric conversion,
formatting, consuming concatenation, and file reading do not increase the heap
high-water marker. Literal `str` views stay outside the free lists. Converting a
literal before mutation creates independent owned storage.

The migration changes 85 Casa source files with 6,136 added and 5,242 removed
lines, for a net increase of 894 lines.
