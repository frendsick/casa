# Owned text compiler benchmark

Run date: 2026-08-27

This benchmark compares `origin/main` at `204f4fd` with the owned `String`
implementation in this document's commit. Both measured binaries were
self-compiled fixed-point candidates. They used the same compiler options and
wrote outputs under `/tmp`.

The v1.38.0 compiler built the main compiler. It cannot build the branch because
its language-trait check requires the old `Display.to_str -> str` contract. The
branch compiler was built through the new self-hosted compiler chain. This is
the source-compatibility reason for the v1.39.0 bootstrap release.

## Environment

- Linux 6.18.33.2 under WSL2
- AMD Ryzen 7 3700X, 8 cores and 16 logical CPUs
- Ten measured runs after one warm-up run per compiler and corpus
- Main then branch, repeated for each corpus
- Wall time from `/usr/bin/time -f %e`

## Commands

The self-compilation corpus was `casa.casa`. The representative multi-file
corpus was `lsp.casa` with its compiler and library imports.

```sh
for iteration in $(seq 1 10); do
    /usr/bin/time -f '%e' -o self-main.times -a \
        /tmp/casac-514-baseline-self \
        -L /tmp/casa-514-baseline/lib \
        /tmp/casa-514-baseline/casa.casa \
        -o /tmp/casa-514-bench-main-self
    /usr/bin/time -f '%e' -o self-branch.times -a \
        /tmp/casac-514-stage11 \
        -L lib casa.casa \
        -o /tmp/casa-514-bench-branch-self
done

for iteration in $(seq 1 10); do
    /usr/bin/time -f '%e' -o lsp-main.times -a \
        /tmp/casac-514-baseline-self \
        -L /tmp/casa-514-baseline/lib \
        /tmp/casa-514-baseline/lsp.casa \
        -o /tmp/casa-514-bench-main-lsp
    /usr/bin/time -f '%e' -o lsp-branch.times -a \
        /tmp/casac-514-stage11 \
        -L lib lsp.casa \
        -o /tmp/casa-514-bench-branch-lsp
done
```

## Results

Spread is the maximum time minus the minimum time.

| Corpus | Main median | Branch median | Branch difference | Main spread | Branch spread |
|---|---:|---:|---:|---:|---:|
| Self-compilation | 14.640 s | 14.460 s | -0.180 s, -1.23% | 0.640 s | 0.590 s |
| Multi-file LSP | 13.300 s | 13.005 s | -0.295 s, -2.22% | 0.860 s | 0.680 s |

The branch change is smaller than the observed run-to-run spread. It does not
show a compile-time regression on either corpus.

## Allocation and source effects

`String.drop` now returns dynamic text storage to the reusable allocator.
Focused tests confirm that a released 1024-byte string buffer is reused and
that 100 post-warm-up iterations of clone, substring, numeric conversion,
formatting, consuming concatenation, and file reading do not increase the heap
high-water marker. Literal `str` views stay outside the free lists. Converting a
literal before mutation creates independent owned storage.

The migration changes 73 Casa source files with 5,986 added and 5,192 removed
lines, for a net increase of 794 lines.
