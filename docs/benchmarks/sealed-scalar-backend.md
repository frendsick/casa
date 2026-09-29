# Sealed scalar backend

Issue #710 introduces a private scalar path in `compiler/emitter.casa`.
Both `products::assembly` and the CLI select it through
`bytecode::compile_assembly`.

Commitment consumes checked source facts into private scalar bodies. It assigns
function and local identities, retains string literals, and turns conditional
markers into nested condition, consequent, and alternative bodies. Missing
functions, unchecked callees, missing locals, rejected operations, and incomplete
conditionals fail before target selection. Source diagnostics cannot enter this
path. Scalar values require no destruction, so ownership-bearing operations
select the existing backend.

The supported path includes builtin scalar values, arithmetic, comparisons,
stack operations, numeric conversions, local assignment, direct calls,
conditionals, and returns. Aggregate, closure, extern, loop, match, typed-memory,
and remaining intrinsic operations use the existing backend for the complete
program. There is no per-function fallback and no retry after a commit failure.
The existing public bytecode representation remains for that migration work.

The sealed product owns complete reachable bodies and literals. Target planning
records bounded leaf expansion ranges. Function selection assigns word slots,
checks frame capacity, restores the frame at every return, and selects concrete
x86-64 instructions and labels. Selection retains the register stack cache and
native `call`/`ret` protocol. Calls use identities assigned before selection, so
recursive calls do not recursively create function buffers.
Expanded leaves check the original call-slot and local-frame capacity. Checked
arithmetic retains its physical frame because the error writer can reserve a
return slot. This preserves return-stack overflow precedence at the boundary.

Each function owns a private list of selected instruction and directive lines.
The renderer only appends those completed lines with their assembly indentation.
It performs no source, storage, or calling-convention classification. It releases
both buffer elements and capacity after each function. Literal pools and checked
bodies remain request-owned. Fixed runtime selection is shared with the existing
backend until the runtime extraction in #712. Complete assembly is published only
after every function succeeds, with the existing Linux x86-64 product tag.

Validation covers the production commit interface, invalid references and
incomplete bodies, recursive calls, conditional early returns, local frames,
integer widths, float arithmetic, register spills, and complete retained assembly.
The existing aggregate and extern tests continue to exercise their original path.

## Migration cost

Against main at `5d0ed60147236ecde664bbee0b4c91e721c12822`, production source
adds 1,172 lines and deletes 92. Tests add 207 lines and delete one. The existing
lifetime workload adds one line and deletes two. No runtime asset is relocated.
These counts exclude documentation and generated measurement output. The scalar
path adds code while the complete legacy path remains available for later work.

Callers request assembly through one `compile_assembly` operation. They receive
complete text or a failure and cannot access checked bodies, target plans, or
function buffers. The private scalar entry distinguishes unsupported capability
from invalid state. Only unsupported capability selects the legacy path. Existing
public bytecode interfaces remain available to legacy callers and tests.

## Measurements

Measured on 2026-09-29 with fixed-point compilers bootstrapped from v1.53.0, on
the Ryzen 7 3700X Linux x86-64 host under WSL2. Commands include assembly and
linking. Each configuration had one warm-up followed by three alternating
control/candidate pairs using `CLOCK_MONOTONIC_RAW`. No suite or other build ran
concurrently. The first timing attempt used the standard monotonic clock and was
discarded before restarting with the required raw clock.

| Workload | Main median (range), seconds | Candidate median (range), seconds | Main / candidate median RSS, KiB |
| --- | --- | --- | --- |
| Common compiler source from main | 10.6323 (10.5313–10.7926) | 10.9263 (10.7753–10.9444) | 338,512 / 338,624 |
| Each compiler's own source | 10.6953 (10.6542–10.7751) | 11.0191 (10.8515–11.1776) | 338,640 / 347,712 |
| 512 scalar functions with calls and branches | 0.1930 (0.1873–0.1934) | 0.1819 (0.1818–0.1831) | 14,800 / 15,196 |

The common-source median increased by 2.8%. All three candidate samples were
slower than their paired control, although the overall ranges overlap. Common
source produces the same assembly and executable byte counts. This result does
not establish a compiler-wide speed gain. Own-source timing includes the added
migration code. Its median remains above the final 10-second target, within the
accepted temporary allowance for intermediate #699 work. Every measured
self-compilation run remained below the 1 GiB RSS ceiling.

The scalar workload's median decreased by 5.8%. Its assembly changed from 457,465
to 452,675 bytes and its executable from 134,696 to 88,328 bytes. Median RSS rose
by 396 KiB. These small-workload results do not predict full migration results.

The repeated backend workload completed 30 successful and rejected request pairs.
After each request pair, live allocations returned to zero. After warm-up, the
allocator retained 980 reusable blocks with 83,872 payload bytes, 64 MiB of mapped
heap, and 4,656 KiB RSS. All remaining samples matched that plateau. Function
buffers release their elements and capacity after rendering, while the allocator
can retain free blocks for reuse.

The [raw evidence](sealed-scalar-evidence.json) records every retained timing and
RSS sample, exact commands, compiler and workload hashes, fixed-point provenance,
output sizes, and allocator samples. The complete local suite passed all 14
shards after correcting leaf expansion's return-stack boundary behavior.
