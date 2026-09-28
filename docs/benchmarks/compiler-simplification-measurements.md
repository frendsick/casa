# Compiler simplification measurement protocol

This protocol supports [Choose compiler simplification targets](https://github.com/frendsick/casa/issues/663)
and [Validate the compiler simplification blueprint](https://github.com/frendsick/casa/issues/651).
The accepted gates and any later performance tolerances are recorded in those
issues. The [historical complexity baseline](compiler-complexity-baseline.md)
remains pinned to its original compiler and source revision.

On 2026-09-28 the maintainer required a self-compilation median below
10.0 seconds before #699 closes and a peak-RSS ceiling of 1 GiB for every measured
run on the reference Ryzen 7 3700X Linux x86-64 host. Intermediate subissues of
#699 may temporarily exceed the timing target. Timing includes assembly and linking.
Use the warm-up and three alternating measured pairs below. Record every sample.
Memory increases must buy a substantial, repeatable end-to-end speed gain.
The ceiling does not justify spending memory for small improvements.
The [target and measurement summary](compiler-self-compilation-target.md)
records the native evidence. The
[blueprint acceptance record](compiler-simplification-blueprint.md) records the
accepted scope and production validation obligations.

Common-source comparisons have no fixed percentage regression allowance.
Regressions beyond observed variation require explicit review of the evidence.
The self-compilation gates and memory tradeoff requirement still apply.

## Comparison inputs

Record the control and candidate compiler binary hashes, source commits,
bootstrap provenance, and supported source corpus. Use a corpus that both
compilers accept for the direct comparison. Record any excluded source and the
reason. Also compile each compiler's own source as a separate workload. That
self-compilation result includes the effect of changing the source being built.

Keep the machine, operating system, native toolchain, library inputs, compiler
flags, output mode, and output location equivalent for each pair. Pin the
command and input files in the report. Do not infer architecture-only speed or
memory gains from the historical self-compilation median.

## Runs and reporting

For each compiler and workload, run one unmeasured warm-up. Then alternate
control and candidate for three measured pairs. Save each wall-time and peak
RSS sample, their medians, and their observed ranges. Include failed runs and
explain any rerun. Compare the paired results with the workload and variation
visible, before proposing a time or memory tolerance.

Report these measures alongside the performance samples:

| Measure | Required distinction |
| --- | --- |
| Maintained production and test source | Count deleted lines separately from relocated runtime assets and generated output. Preserve distinct behavior, diagnostic, ownership, ABI, and formatter safety coverage. |
| Interface complexity | Count caller-visible concepts, variants, matching and ordering rules, ownership, failure states, and knowledge of other modules. Record dependency direction and repeated semantic decisions. |
| Generated assembly and executable size | Use the same corpus and output mode. Include a generic-heavy source where relevant. |
| Compiler phase and allocation cost | Record per-phase time, allocation detail, and peak RSS where available. |
| Retained editor state | Measure memory after repeated edits and queries, including whether the live set plateaus. |

Show total change against the pinned historical source baseline. Where an
implemented behavior migration provides a measured adjusted checkpoint, also
show architecture-only change from that checkpoint. Otherwise report the
combined gain and its attribution limit. Do not subtract estimated behavior
costs from measured totals.

The executable slice must calibrate production estimates and test its own contracts. Full
compiler correctness, self-hosting, fixed-point behavior, and performance gates
require final implementation evidence. The native compiler's measured result
does not establish the redesigned compiler's performance. For gate misses outside the
temporary timing allowance for #699 subissues, record a new evidence-based decision.
