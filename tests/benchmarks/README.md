# Measurement tools

These tools exercise the current compiler. Keep results outside the repository
or under `tests/benchmarks/results/`, which git ignores. Never commit results,
raw samples, logs, profiles, or historical performance reports.

The tools require Linux x86-64, Python 3, and GNU binutils. The lifetime sampler
reads its child process through `/proc`. The profiler also requires permission
to trace its child with `ptrace`.

## Allocation lifetimes

`lifetime.py` samples the Casa allocator and RSS while a workload stops itself.
It checks that all 31 samples are present and that measurements remain unchanged
after five warm-up requests. The workloads cover distinct ownership paths:

| Workload | Paths exercised |
| --- | --- |
| `analysis-lifetime.casa` | Accepted and rejected generic analysis |
| `backend-lifetime.casa` | Scalar and aggregate assembly requests, plus rejection |
| `editor-lifetime.casa` | Snapshot replacement while retaining owned hover answers |
| `workspace-lifetime.casa` | References and rename validation across replacements |
| `native-build-lifetime.casa` | Successful builds and native-tool launch and exit failures |

Build the branch compiler, then run each check from the repository root:

```sh
benchmark_output=$(mktemp -d /tmp/casa-benchmarks.XXXXXX)
./casac -L lib casa.casa -o "$benchmark_output/casac" --keep-asm
for workload in analysis backend editor workspace native-build; do
    "$benchmark_output/casac" -L lib "tests/benchmarks/$workload-lifetime.casa" \
        -o "$benchmark_output/$workload-lifetime"
    python3 -B tests/benchmarks/lifetime.py \
        "$benchmark_output/$workload-lifetime" \
        "$benchmark_output/$workload-lifetime.json"
done
```

Run these workloads serially. The workspace and native-build checks use fixed
temporary paths. The sampler depends on the allocator's current block layout
and symbols, so update it when that representation changes.

## Native stack sampling

`sample.py` reconstructs Casa's separate return stack and attributes samples to
native function symbols. Use the compiler and assembly built above:

```sh
python3 -B tests/benchmarks/sample.py "$benchmark_output/casac.s" \
    "$benchmark_output/profile.json" "$benchmark_output/casac" \
    -L lib casa.casa -o "$benchmark_output/profiled-compiler"
```

Sampling stops the child and adds overhead. Samples include waiting time, and
generic function symbols may not expose source names. Use the profile to find
hot paths. Use ordinary unsampled runs for timing comparisons.

For elapsed time and peak RSS, use the system tool directly:

```sh
/usr/bin/time -f '%e seconds, %M KiB peak RSS' "$benchmark_output/casac" \
    -L lib casa.casa -o "$benchmark_output/timed-compiler"
```

Fixed-point correctness remains covered by `tests/test_bootstrap.sh`.
