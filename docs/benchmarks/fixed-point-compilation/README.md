# Fixed-point self-compilation costs

Evidence for [Establish fixed-point compilation costs](https://github.com/frendsick/casa/issues/682).
Run date: 2026-09-26. Production compiler and library sources are unchanged.
This baseline informs the next experiment. It does not select an optimization
or change an accepted language contract.

## Baseline

The verified fixed-point compiler takes **62.126 seconds** to compile itself
and uses **327.4 MiB** peak RSS, as medians of three runs after one warm-up.
Reaching 10 seconds would require approximately an 84% elapsed-time reduction
on this workload. This is a measured starting point, not an acceptance band.

| Run | Raw monotonic seconds | Adjusted monotonic seconds | GNU time seconds | Peak RSS KiB |
| --- | ---: | ---: | ---: | ---: |
| Warm-up, excluded | 62.988 | 60.479 | 63.15 | 335,228 |
| 1 | 61.657 | 59.201 | 61.87 | 335,228 |
| 2 | 62.302 | 59.806 | 61.14 | 335,356 |
| 3 | 62.126 | 59.659 | 62.33 | 335,100 |
| Median | 62.126 | 59.659 | 61.87 | 335,228 |

[Raw evidence](evidence.json) retains every sample, commands, clock readings,
logs, tool versions, source identities, and compiler hashes. The source revision
is `6a2c8451d9f6a58562d6e1273e4dba3c5da6b76b`. Its compiler and library sources
match the earlier investigation at `29fe1866a6ed6e408bdf127b19bf3734b839e376`.
The machine is the reference AMD Ryzen 7 3700X under Linux
6.18.33.2-microsoft-standard-WSL2, with 16 logical CPUs and 15 GiB RAM.
The native build uses GNU assembler and linker 2.38 and GCC 11.4.0.

`tests/test_bootstrap.sh` passed both checks. The captured stage 2 and stage 3
assembly hashes match, as does every warm-up and measured output's assembly.
Stable v1.50.0 builds stage 1. Stage 1 builds stage 2. Stage 2 builds stage 3.
The measurements use stage 2. The earlier branch-compiler and stable-bootstrap
measurements describe different executables and must remain separate.

Use direct `clock_gettime(CLOCK_MONOTONIC_RAW)` for subsequent paired comparisons
on this machine. The raw range is 0.644 seconds, or 1.04% of the median.
Raw time exceeds summed user and system CPU time by only 0.027 to 0.042 seconds
in the measured runs. Python's monotonic clock and direct kernel
`CLOCK_MONOTONIC` agree, but both advance approximately 4% less than the raw
clock. GNU time tracks the realtime interval and includes a shorter interval
in run 2, even shorter than CPU accounting. This isolates the discrepancy to
the clock domains, rather than Python output collection. The host clock
adjustment mechanism was not diagnosed. Do not mix historical GNU or adjusted
monotonic values with this baseline when judging small changes.

## Measured costs and experiment order

The successful sampler run collected 5,944 observations in 65.833 raw seconds.
Its generated output matches the fixed-point assembly. Its duration includes
profiling overhead and is excluded from the baseline. The following phase
categories are disjoint. Time estimates multiply the baseline median by each
category's share of samples, so they are approximate attribution, not separately
timed passes.

| Phase | Samples | Share | Estimated baseline seconds |
| --- | ---: | ---: | ---: |
| Parse and resolve, including imports | 1,261 | 21.2% | 13.18 |
| Typechecking and specialization | 3,927 | 66.1% | 41.04 |
| Bytecode | 281 | 4.7% | 2.94 |
| Assembly emission | 209 | 3.5% | 2.18 |
| Native build | 243 | 4.1% | 2.54 |
| Other CLI and cleanup work | 23 | 0.4% | 0.24 |

The front end's 1,261 samples split into global-initializer validation (578),
imported lexing (113), namespacing (89), parsing (85), identifier rewriting
(66), and remaining import/declaration bookkeeping (330). Global-initializer
validation is 45.8% of this interval. The semantic interval includes checking
reachable callees from the root. It must not be described as root-only work.

Start with the following experiments, changing one cause per paired comparison.
The percentages below are **inclusive and overlap**. They are not predicted
savings and must not be added.

| Priority | Measured target | Experiment and contract to retain |
| --- | --- | --- |
| 1 | `format_name_for_source`: 1,328 samples, 22.3% of the build. `set_parameter_context` runs 46,266 times. | Defer diagnostic-context text until needed, or index source-name lookup. Preserve diagnostic names and attribution. |
| 2 | `str_hash`: 1,246 samples, 21.0%. String cloning adds 700 inclusive samples. | Reduce repeated string-key hashing through stable identities or cached hashes. Preserve name resolution and the runtime-local hashing contract. |
| 3 | Global-initializer validation: 578 samples, 9.7%. It creates 22 semantic sessions and checks zero initializer bodies. Store cloning alone accounts for 458 samples. | Avoid session construction when no initializer exists. This can preserve current behavior. The already accepted runtime-global removal is a separate migration. |
| 4 | `analyze_operation`: 484 samples, 8.1%. There are 114,362 operation dispatches and 114,362 fact-collection calls. | Reuse established operation facts across dependency collection and checking. Measure avoided resolution before crediting a gain. |
| 5 | Concrete drop-type traversal: 398 samples, 6.7%, with 151,209 recursive visits through 11,807 outer requests. | Test request-owned reuse of concrete cleanup dependencies. Preserve reachability and destruction behavior. |

The first target is supported by the current call path:
[`apply_stack_effect`](../../../compiler/semantics.casa#L4673) eagerly calls
[`set_parameter_context`](../../../compiler/semantics.casa#L4453), which calls
[`format_name_for_source`](../../../compiler/common.casa#L3544). That helper
linearly searches source-name mappings twice. The generated text is consumed by
diagnostic paths. The profile measures its cost on a successful compilation.

For the third target,
[`validate_global_initializer_order`](../../../compiler/legacy_parser.casa#L9127)
creates a `SemanticSession` before inspecting operations. Its
[`constructor`](../../../compiler/semantics.casa#L1310) calls
[`clone_for_semantics`](../../../compiler/semantics.casa#L11897), which copies
functions and semantic tables. These are remaining validation snapshots,
distinct from the importer-store copies removed in earlier work.

Cloning spans 1,653 samples, or 27.8% of the build. Exact counters record
3,135,590 `Op` clones, 8,589,984 `Type` clones, and 26,901,745 `String` clones.
The `Op` count is 27.4 times the operation-dispatch count. This compares calls,
not unique source nodes, and includes work outside semantic checking.

The allocator is entered 610,348,357 times and receives 14,375,898,966 requested
bytes, or 13.39 GiB of cumulative traffic. Allocator and free helpers occupy
386 leaf samples, or 6.5%. Only 249,634 allocations enter the large-block path,
with 1,923,751 search-loop visits and six heap mappings. These counts do not
identify the large free-list search as the dominant cause. RSS remains much
smaller than cumulative traffic because allocations are reused. Clone,
hashing, and allocation costs overlap.

## Hypotheses ruled down

The GDB trace records **2,817 body-analysis calls for 2,817 distinct function
names**, including the root. No recorded input context repeats. The separate
counter agrees on 2,817 calls to `analyze_ops`. The 296 calls through
`analyze_function_semantics_in_store` also have distinct names and contexts.
There are 295 scheduled continuations. Although
`collect_function_return_summary` is called 30,583 times, it occupies only six
samples, or 0.1%. Repeated body checking and callable-summary caching are not
supported as first targets by this corpus. Other programs can behave differently.

Generic-cycle validation occupies five samples, or 0.08%. The GDB phase trace
measures approximately 0.049 seconds for that interval. It is not a useful
first target for the 10-second direction. No source-language feature removal
has been measured or selected here.

The next decision remains
[Choose reductions toward ten-second self-compilation](https://github.com/frendsick/casa/issues/683).
It must compare native experiments against this fixed-point control, with one
warm-up and three alternating measured pairs, and retain correctness evidence.
This profile establishes where to experiment. It does not demonstrate a route
all the way to 10 seconds or set numerical acceptance limits.

## Evidence files

- [evidence.json](evidence.json) contains provenance, all timing samples, exact
  entry counters, GDB phase events, and complete body-analysis contexts.
- [profile-samples.json](profile-samples.json) retains all samples. Each sample
  is `[elapsed_raw_ns, pc, stack_index]`. `stacks` contains indices into
  `functions`. `leaf` and `inclusive` contain `[function_index, count]` pairs.
  Names and stacks are interned only to reduce artifact size.
- The four scripts below reproduce measurement, sampling, counters, and contexts.
  Instrumentation is confined to generated profiling executables and debugger
  probes. Compiler and library sources remain unchanged.

## Reproduce

Use the pinned source revision and the reference Linux x86-64 machine. Install
the release named by `casa-release.env`, then run the repository check:

```sh
./install.sh
tests/test_bootstrap.sh
```

The recorded binaries and assembly were copied from the check's temporary
directory before its normal cleanup. To retain the same build stages without
intercepting cleanup, use the commands below from the source worktree. Output
names can change ELF file symbols and binary hashes even when assembly matches.
Use assembly equality for the repository's fixed-point criterion.
Absolute source paths are embedded in assembly, so use the recorded worktree
path when comparing hashes with this capture.

```sh
root=$PWD
out=$(mktemp -d /tmp/casa-fixed-point.XXXXXX)
evidence=$root/docs/benchmarks/fixed-point-compilation
./casac -L "$root/lib" "$root/casa.casa" -o "$out/fp_stage1"
"$out/fp_stage1" -L "$root/lib" "$root/casa.casa" \
  -o "$out/fp_stage2" --keep-asm
"$out/fp_stage2" -L "$root/lib" "$root/casa.casa" \
  -o "$out/fp_stage3" --keep-asm
cmp "$out/fp_stage2.s" "$out/fp_stage3.s"
for label in warmup run1 run2 run3; do
  python3 "$evidence/measure.py" "$label" "$out/fp_stage2" "$root" "$out"
done
```

Each invocation uses a fresh process and output name, warm filesystem caches,
`--keep-asm --verbose`, and no compiled-module cache. Time includes analysis,
code generation, assembly, linking, and process cleanup. Run benchmarks serially,
without concurrent builds or profiles. The warm-up is retained but excluded
from the three-run median.

The scripts use Python's standard library, GNU binutils, and GDB. `sample.py`
uses Linux x86-64 `ptrace` and Casa's emitted return-stack layout. Reassemble
with local labels retained, and verify that the machine instructions match:

```sh
/usr/bin/as -L -o "$out/sample.o" "$out/fp_stage2.s"
/usr/bin/cc -nostdlib -no-pie -Wl,-e,_start -Wl,-z,noexecstack \
  -o "$out/sample" "$out/sample.o"
objcopy -O binary --only-section=.text "$out/fp_stage2" "$out/fixed.text"
objcopy -O binary --only-section=.text "$out/sample" "$out/sample.text"
cmp "$out/fixed.text" "$out/sample.text"
python3 "$evidence/sample.py" "$out/fp_stage2.s" "$out/profile.json" \
  "$out/sample" -L "$root/lib" "$root/casa.casa" \
  -o "$out/profile-output" --keep-asm --verbose > "$out/profile.log"
cmp "$out/fp_stage2.s" "$out/profile-output.s"
```

Sampling requests a stop every 10 milliseconds. Exact call-return labels
identify active frames among local values in Casa's separate return stack.
Inclusive counts count each function at most once per sample. They overlap.
Leaf counts identify the sampled instruction's function. These are wall
samples, including pauses and native-build waits, not hardware CPU events.
The profiler does not follow assembler or linker children. Sampling overhead
and coarse intervals prevent treating profile duration as a benchmark.

Collect exact call counts and allocator traffic in a separate executable:

```sh
python3 "$evidence/count.py" "$out/fp_stage2.s" "$out/count.s" "$out/count-labels.json"
/usr/bin/as -o "$out/count.o" "$out/count.s"
/usr/bin/cc -nostdlib -no-pie -Wl,-e,_start -Wl,-z,noexecstack \
  -o "$out/count" "$out/count.o"
/usr/bin/time -f '%e %M %U %S' -o "$out/count.time" \
  "$out/count" -L "$root/lib" "$root/casa.casa" \
  -o "$out/count-output" --keep-asm --verbose 3> "$out/counts.bin"
cmp "$out/fp_stage2.s" "$out/count-output.s"
python3 - "$out" <<'PY'
import json, struct, sys
from pathlib import Path
out = Path(sys.argv[1])
labels = json.loads((out / 'count-labels.json').read_text())
counts = [value for (value,) in struct.iter_unpack('<Q', (out / 'counts.bin').read_bytes())]
assert len(labels) == len(counts)
print(json.dumps(dict(zip(labels, counts)), indent=2))
PY
```

The counters cover parser and semantic functions, generated and explicit clone
functions, allocator entries, large-block searches, and requested bytes.
Requested bytes are cumulative traffic, not live memory or RSS. Counter
instrumentation changes execution cost, so its duration is not a speed result.

Record actual body-analysis inputs and phase entries with GDB:

```sh
CASA_PROFILE_OUTPUT="$out/contexts.json" gdb --batch \
  -ex 'set pagination off' -ex 'set confirm off' \
  -ex 'set startup-with-shell off' -ex 'set disable-randomization off' \
  -ex "source $evidence/contexts.py" --args \
  "$out/fp_stage2" -L "$root/lib" "$root/casa.casa" \
  -o "$out/context-output" --keep-asm --verbose > "$out/contexts.log" 2>&1
cmp "$out/fp_stage2.s" "$out/context-output.s"
```

`contexts.py` uses the pinned compiler's symbols and verified aggregate offsets.
Rediscover both before applying it to a different compiler. Contexts include
function name, callable bindings, active recursion guard, clone mode, and
parameter-inference mode. Repeated inputs do not prove cache equivalence:
mutable symbol-store state and the current checked body also matter.
