# Native compiler optimization measurements

The production source is commit `5c7ff5b` on `perf/683-under-ten`. The control is
merged main commit `dfb77b4`. Both use the pinned v1.50.0 compiler to establish
a native fixed point. Measurements include analysis, bytecode generation,
assembly emission, assembling, and linking on a Linux x86-64 guest with an
AMD Ryzen 7 3700X host CPU.

`timings.csv` retains each warm-up and measured run, clock readings, RSS,
compiler hashes, and output assembly hashes. Each study used one warm-up
followed by three measured pairs with alternating order. No other build or
profile ran concurrently with these measurements. Absolute paths in the
original run records were temporary locations, not runtime dependencies.

## Final committed comparison

| Pair | Main | Candidate |
|---|---:|---:|
| pair1 | 13.095 s | 9.721 s |
| pair2 | 12.740 s | 9.578 s |
| pair3 | 12.928 s | 9.710 s |
| Median | **12.928 s** | **9.710 s** |

Median peak RSS: 301308 → 289980 KiB.
All three pairs improve and all three candidate runs finish below ten seconds.

## Native probes

The library probe starts from `dfb77b4`. Apply `library-probe.patch`, build the
compiler to a fixed point with `--keep-asm`, and retain its assembly. This
snapshot reads ASCII hash bytes directly and copies file output through
`Bytes.to_raw_buffer`. Its Unicode path and hash recurrence remain unchanged.

The scripts change only the executing compiler's machine code. They do not
change its source or emitted output:

```sh
python3 call-probe.py control.s calls transfer
python3 frame-probe.py calls.s 32 calls-frames
```

`call-probe.py` without `transfer` preserves the original return-address writes
and discards the CPU's duplicate address at entry. The measured `transfer`
variant moves the CPU return address into Casa's reserved slot instead. Both
use native `call` and `ret`. Frame probes preserve the initialized bytes,
return-stack checks, and final register and flag state.

All twelve warm-up and measured outputs in `calls-comparison` have identical
assembly hashes. Its measured medians were 12.141 seconds for the library
control, 10.647 seconds for native calls, and 10.455 seconds with 32-word direct
frame stores. These adjacent results are separate from the final comparison.

The byte-fill probe replaces remaining frame `rep stosq` instructions with
`rep stosb`, multiplying their counts by eight. It was rejected: its paired
median was 10.131 seconds versus 10.061 seconds for its control.

## Production implementation

The retained implementation uses native calls, direct stores through 32 local
words, cached values across no-op conversions and conditional branches, direct
ASCII hash reads, a separate Unicode hash helper, and bulk file-output copies.
The recurrence and Unicode hash values remain unchanged. Language features,
checked return-stack capacity, and external C calls retain their behavior.

The private hash parameter was renamed after the first final comparison. This
changes source offsets used in generated lambda labels. `committed-comparison`
therefore measures the committed source and its rebuilt fixed-point compiler.
Use that study for the final result.

The source fingerprints in `provenance.json` identify the measured inputs.
Timing variation is host and workload specific. The ten-second target is an
observed self-compilation result, not a universal compiler time limit.

## Reproduce a comparison

From two clean source checkouts, install the pinned tools and build each
compiler through stages 1, 2, and 3 with `--keep-asm`. Compare its stage 2 and 3
assembly. Supply the two stage 2 compiler paths to the existing
`docs/benchmarks/compiler-reductions/compare.py`, with both entries pointing to
the same production source checkout. The control and candidate intentionally
emit different machine code. Each variant must emit deterministic assembly.

Correctness checks use the production regression tests and full
`tests/test_all.sh` suite, including the bootstrap fixed-point shard. The
production PR contains no raw measurement artifacts.
