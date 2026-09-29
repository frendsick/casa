# Aggregate storage and native-call plans

Issue #711 moves Linux x86-64 native-call placement from the assembly emitter
into `compiler/abi.casa`. Lowering builds one plan per reachable concrete extern
function and keeps it in the request-owned program. Call instructions contain
only that function's identity. Repeated calls reuse the plan, including calls
from different Casa functions.

The plan contains selected instruction lines plus allocation and conditional
argument-release actions. It fixes integer and SSE registers, whole-aggregate
spill rollback, stack argument order and alignment, hidden result storage,
return normalization, aggregate result reconstruction, and temporary cleanup.
The emitter assigns unique runtime continuation labels and expands the selected
allocation and release actions. It never sees source types or ABI classes.

Native calls preserve the existing register contract. `%r13` retains the source
arguments, `%r12` retains the final Casa stack position, and `%r15` retains hidden
result storage across the C call. Results are saved before aggregate arguments
are released. Native calls use the C stack directly. Runtime allocation and
release helpers retain Casa's checked return-stack protocol. The conditional
free guard remains necessary for borrowed inline copies.

Aggregate storage keeps the existing shared `FieldStoragePlan` for ordinary and
extern fields. Lowering resolves field offsets and inline placement before
creating explicit loads, stores, `ReadIndirect`, `WriteIndirect`, or `MoveBytes`
operations. These operations contain selected widths or byte counts. Rendering
does not inspect aggregate types, field families, traits, or layout. No layout,
value carrier, ownership, or destruction policy changes in this slice.

The scalar-only sealed path remains as introduced in #710. Aggregate and extern
programs still enter through the existing checked lowering adapter. This slice
removes late native-call classification from that shared path without adding a
second aggregate representation. The final consumer cutover remains in #715.

Validation retains the native scalar, bool, integer/SSE exhaustion, mixed-class
aggregate spill, hidden-result, padding, and aggregate ownership fixtures. The
extern unit test checks that repeated calls share one request-owned plan and
that selected assembly retains argument placement and return extension.

## Measurements

Measured on 2026-09-29 on the Ryzen 7 3700X Linux x86-64 reference host under
WSL2. Both compilers were bootstrapped from v1.53.0 and verified at a fixed
point. Each configuration had one warm-up and three alternating measured runs
using `CLOCK_MONOTONIC_RAW`, including assembly and linking. No other build or
suite ran concurrently.

`main` is pinned at `069ad803b9d9cacc9da7f727cdfb435e9dac65ba`. The baseline
compiles its own source. `candidate-common` compiles that same source, while
`candidate-own` compiles this branch. The baseline common-source and own-source
workloads are identical, so their samples are shared.

| Configuration | Median (range), seconds | Median RSS, KiB | Assembly / executable bytes |
| --- | --- | --- | --- |
| main | 10.7223 (10.5703–10.7704) | 346,688 | 29,162,468 / 6,300,712 |
| candidate-common | 10.6986 (10.6630–10.8149) | 346,728 | 29,162,468 / 6,300,728 |
| candidate-own | 10.7339 (10.6584–10.7767) | 349,032 | 29,291,597 / 6,332,920 |

Common-source timing differs by 0.2%, within the overlapping observed ranges.
This establishes no speed gain or timing regression. Candidate self-compilation
has a 10.7339-second median, above the final ten-second target and within the
accepted temporary allowance for intermediate #699 issues. Every measured run
remains below the 1 GiB RSS ceiling. Common-source median RSS changes by 40 KiB.

The request-lifetime workload includes scalar compilation, repeated aggregate
extern calls, and rejected source. All 30 completed iterations return to zero
live allocations. After warm-up, the final candidate retains 4,904 reusable
blocks with 269,824 payload bytes, 64 MiB of mapped heap, and 4,876 KiB RSS.
The baseline fails the same plateau check with 64 live bytes added per request.
The removed result-register arrays account for that growth. No runtime
allocator change is included.

Production source adds 342 lines and deletes 244 against the pinned baseline.
Tests add 105 lines and delete 11, mainly to supply the plan table in existing
bytecode-construction tests. The existing lifetime workload adds one line.
No runtime source is relocated. Classification and placement now have one owner
in `abi.casa`, and the renderer depends on three physical action variants instead
of classified source signatures. Plans live until the request ends. Their action
lists are neither cloned nor rebuilt for repeated calls.

The [raw evidence](aggregate-native-evidence.json) records every timing/RSS
sample, exact commands, source and compiler hashes, fixed-point provenance,
output sizes, and baseline and candidate allocator samples. All 14 local CI
shards passed. Final native and emitter checks passed after review fixes.
