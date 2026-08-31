# Indirect typed copy benchmark

Run date: 2026-08-31

This benchmark compares `origin/main` at `aa088b4` with the implementation for
#616. Both compilers are self-compiled fixed points bootstrapped with stable
Casa v1.49.0. Stage 2 and stage 3 assembly matched for each compiler.

## Environment

- Linux 6.18.33.2 under WSL2
- AMD Ryzen 7 3700X, 8 cores and 16 logical CPUs
- One warm-up followed by three measured runs per compiler
- Self-compilation of each compiler's own `casa.casa` and source tree
- Wall time from GNU `time`

## Measurement

Each compiler used a three-stage build. Each measured command wrote to a fixed
path under `/tmp`:

```sh
/usr/bin/time -f '%e' -o times -a \
    ./stage3 -L lib casa.casa -o /tmp/casa-616-benchmark/measured
```

## Results

| Measurement | Main | Changed | Difference |
|---|---:|---:|---:|
| Generated assembly | 23,662,439 bytes | 23,213,582 bytes | -448,857 bytes, -1.90% |
| Median self-compilation | 38.09 s | 38.37 s | +0.28 s, +0.74% |

Main samples were 40.72, 38.09, and 37.31 seconds. Changed samples were 38.52,
38.15, and 38.37 seconds. The timing difference is smaller than the 3.41-second
baseline spread and does not show a material runtime regression.
