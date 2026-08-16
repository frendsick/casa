# Checked integer arithmetic benchmark

Measured on 2026-08-16 under WSL2 Linux 6.18.33.2 on an AMD Ryzen 7 3700X
(8 cores, 16 logical CPUs). Timings use GNU `time` wall-clock seconds. Each
case was warmed once, then run seven times; the table reports medians.

The legacy self-compilation executable was the first compiler stage built by
the v1.16.0 release compiler, so its own arithmetic contains the legacy
unchecked instructions while it can still compile the current source. The
checked executable was rebuilt by a compiler whose own arithmetic was already
checked. Both compiled the same `casa.casa` with the same library path and
output-directory class:

```sh
/usr/bin/time -f '%e' legacy-casac -L lib casa.casa -o /tmp/casa-legacy-N
/usr/bin/time -f '%e' checked-casac -L lib casa.casa -o /tmp/casa-checked-N
```

For the runtime cases, v1.16.0 and the checked compiler compiled the committed
benchmark sources, then each resulting binary was measured with the same
warm-up and seven-run procedure:

```sh
casac -L lib benchmarks/checked_arithmetic/add_sub.casa -o /tmp/add-sub
casac -L lib benchmarks/checked_arithmetic/multiply.casa -o /tmp/multiply
/usr/bin/time -f '%e' /tmp/add-sub >/dev/null
/usr/bin/time -f '%e' /tmp/multiply >/dev/null
```

| Case | Legacy median | Checked median | Difference | Change |
| --- | ---: | ---: | ---: | ---: |
| Self-compilation | 3.46 s | 3.33 s | -0.13 s | -3.8% |
| Add/subtract loop | 0.51 s | 0.47 s | -0.04 s | -7.8% |
| Multiply-heavy loop | 0.11 s | 0.10 s | -0.01 s | -9.1% |

Raw self-compilation samples were `3.42 3.41 3.41 3.52 3.46 3.53 3.69`
seconds for legacy and `3.33 3.31 3.40 3.39 3.36 3.19 3.32` seconds for
checked. Add/subtract samples were `0.50 0.53 0.51 0.51 0.53 0.54 0.50` and
`0.44 0.44 0.44 0.51 0.49 0.51 0.47`; multiply samples were
`0.13 0.13 0.13 0.12 0.11 0.11 0.11` and
`0.11 0.11 0.10 0.10 0.10 0.10 0.10`.

On this machine the accepted compile-time and runtime costs hold: checked
arithmetic introduced no measured slowdown in these cases. The small apparent
speedups are within the noise expected from short wall-clock measurements and
are not evidence that checks improve performance.
