# Self-compilation speed target

On 2026-09-26 the maintainer rejected a half-minute self-compilation time and
the proposed 20% time and 25% memory regression ceilings. The design target is
roughly **10 seconds for self-compilation**. Memory is secondary unless it
reaches multiple gigabytes. Neither an exact timing acceptance band nor a
numeric RSS ceiling has been selected.

Aggressive implementation simplification and feature reductions may be
investigated when they can produce large compilation gains. No specific feature
removal has been accepted. A measured feature tradeoff must identify the user
cost and the existing decision it would supersede. The compiler map still
covers decisions and experiments, with production implementation outside scope.

For this investigation, self-compilation means a fresh compiler process building
its own complete source into a native executable, including assembly and linking.
Use the reference Linux x86-64 machine, warm filesystem caches and fresh output,
without a compiled-module cache. Final implementation acceptance also requires
the fixed-point compiler. The measurements below use the branch compiler built
by stable v1.50.0 and do not establish a fixed point.

## Fresh baseline

The [raw evidence](performance-evidence.json) preserves compiler hashes, source
revision, machine/tool versions, commands, samples, logs and reproduction scripts.
Production sources remain at `29fe1866a6ed6e408bdf127b19bf3734b839e376`.

One warm-up preceded three serial measured builds with
`/tmp/casa-651-control -L lib casa.casa --keep-asm --verbose` and the same output
path. All compiler invocations succeeded. The investigation harness rejects
elapsed time above 10 seconds. Every build failed that performance check.

| Run | Python monotonic seconds | GNU time wall seconds | Peak RSS MiB |
| --- | ---: | ---: | ---: |
| 1 | 54.933 | 57.40 | 571.50 |
| 2 | 54.421 | 55.76 | 571.50 |
| 3 | 54.469 | 56.88 | 571.50 |
| Median | 54.469 | 56.88 | 571.50 |

The clocks disagree beyond ordinary rounding. The cause was not established.
Keep both readings and resolve this discrepancy before making a tight acceptance
claim. The historical 33.11-second baseline used GNU time and stable v1.50.0
compiling the repository source. It did not measure this branch compiler.
A single fresh stable comparison took 29.84 seconds by GNU time and 28.623
seconds by the monotonic clock, at 742.75 MiB RSS. That single run is not a paired
median and does not explain the cost difference between compilers.

The current branch result requires roughly an 82% reduction to reach 10 seconds.
The Python integrated slice supplies no evidence that this reduction is achieved
or feasible. It remains an executable contract experiment.

## Analysis profile

A separate GDB run timestamped function entries without changing the compiler.
It completed in 55.065 monotonic seconds. These are inclusive intervals from one
instrumented run, not sampled CPU attribution or benchmark medians.

| Interval | Seconds | Interpretation |
| --- | ---: | --- |
| Parse and resolve to typecheck entry | 12.067 | Includes imported-source work |
| Typecheck entry to function scheduler | 29.410 | Extern validation, session setup and root semantic analysis, including reachable calls |
| Scheduler to trait validation | 1.818 | Remaining scheduled function checking |
| Trait validation to generic-cycle validation | 0.097 | Trait validation interval |
| Generic-cycle validation to specialization | 0.045 | Not a material cost in this workload |
| Specialization to bytecode entry | 4.515 | Includes subsequent diagnostic/source cleanup |
| Bytecode to emitter entry | 2.547 | Bytecode interval |

The verbose log puts assembly emission near 1.9 seconds and native build plus
remaining cleanup near 2.7 seconds. The largest measured opportunity is in the
front end and semantic checking. Making emission faster alone cannot save the
required time. Improving generated code could also accelerate the compiler
itself, which is a separate effect that these phase totals do not isolate.

## Experiments before blueprint acceptance

First split the 12.1-second front-end interval and the 29.4-second root-checking
interval into enough detail to distinguish useful checking from repeated work.
Record calls, distinct contexts, checked operations, cloned nodes and allocation
cost. Then change one dominant cause in a throwaway experiment and run paired
whole-build measurements. Judge gains against observed variation and confirm
unchanged results for the retained contracts.

| Candidate | Current evidence and falsifiable question |
| --- | --- |
| One grammar and stable declaration identities | The 12.1-second front-end interval is material. Measure token rewriting, declaration expansion and resolution separately before crediting ADR-0174's replacement with a gain. |
| Consume semantic results once | [Semantic result construction](../semantics.casa#L11939) and the [scheduler](../typechecker.casa#L94) clone bodies/results. How many cloned operation nodes exist per checked source node, and how much time can ownership transfer remove? |
| Reuse each operation decision | [Dependency collection](../semantics.casa#L11280) and operation handlers call shared resolution separately. Does implementing ADR-0173's single operation decision remove substantial repeated work? |
| Cache stabilized callable summaries | [Returned-callable analysis](../semantics.casa#L10777) reanalyzes bodies with an active recursion guard. How many equivalent declaration/callable-binding contexts repeat? Use request-local cache ownership consistent with independent snapshots. |
| Cache concrete cleanup dependencies | [Drop-type traversal](../semantics.casa#L12565) revisits concrete types. Measure duplicate visits and cost within the 4.5-second specialization interval. |

Generic-cycle traversal was an initial source-based suspect. Its measured
0.045-second interval rules it out as a meaningful route to the self-compilation
target on this corpus. Do not spend the performance budget optimizing it first.

Some tempting savings are already present. The completed module-store-copy
removal, bytecode whole-tree clone removal and named-function clone restriction
cannot be counted again. See [module analysis](../../docs/benchmarks/module-analysis.md#post-implementation-measurements-564)
and [ownership cost](../../docs/benchmarks/ownership-analysis-cost.md#profile).
The historical 4.31-second compiler was much smaller and preceded many changes.
Its comparison with a later compiler does not isolate ownership cost.

Current compiler/library sources use neither selective-import clauses nor
`const fn`. Their accepted removal may reduce the compiler source and generated
compiler, but does not directly eliminate a used source-language feature from
this self-compilation workload. Measure the effect instead of assigning a saving.

If implementation simplifications do not provide a credible route to the target,
bring measured feature tradeoffs back to the maintainer. Checked generics,
recursive callable behavior, inference and ownership guarantees are candidates
for investigation, not predetermined cuts. Removing or restricting them can
conflict with ADR-0170, ADR-0173 and the accepted ownership decision. Record each
accepted behavior change explicitly and update its retained coverage.

The blueprint remains unaccepted until the performance plan has sufficient
evidence and the maintainer accepts the resulting tradeoffs. There is no measured
speed improvement in this update.

## Reproduce the measurements

Run from `/home/ai-agent/git/casa-651` with the recorded stable v1.50.0 `casac`.
The archived scripts retain the original paths and compiler-specific GDB symbol
names. Adjust paths and rediscover symbols before using a different checkout or
compiler revision.

```sh
mkdir -p /tmp/casa-651-speed
python3 - <<'PY'
import json
from pathlib import Path
evidence = json.loads(Path('compiler/blueprint_prototype/performance-evidence.json').read_text())
for name, source in evidence['reproduction_scripts'].items():
    (Path('/tmp/casa-651-speed') / name).write_text(source)
PY
./casac -L lib casa.casa -o /tmp/casa-651-control
python3 /tmp/casa-651-speed/measure.py replay
```

The last command exits 1 when the build exceeds 10 seconds. Inspect its JSON
`exit_code` to distinguish a compiler failure from the timing rejection. Repeat
with distinct labels after warm-up to collect samples. The separate profile was
launched with this exact Bash command from the same worktree:

```sh
set -o pipefail
timeout 75s gdb --batch -x /tmp/casa-651-speed/phases.gdb --args \
  /tmp/casa-651-control -L lib casa.casa \
  -o /tmp/casa-651-speed/profile-build --keep-asm --verbose \
  2>&1 | tee /tmp/casa-651-speed/phases.log
```
