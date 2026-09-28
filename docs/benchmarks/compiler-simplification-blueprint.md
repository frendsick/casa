# Compiler simplification blueprint acceptance

Status: pending maintainer acceptance in
[Validate the compiler simplification blueprint](https://github.com/frendsick/casa/issues/651).
Reconciled against `origin/main` at `f08a6e1` on 2026-09-28.

## Design and implementation plan

The [integrated blueprint](https://github.com/frendsick/casa/blob/53f7b8cc0a3813ccfd309ae49612dcebcc37e581/compiler/blueprint_prototype/README.md)
defines the accepted Compiler Capsule contracts, module dependencies, data flow,
failures, lifetimes, invalid-state dispositions, and eight implementation units.
Each unit identifies its dependencies, observable outcome, removed structures,
and retained coverage. This record updates its historical baseline and gates.
Production implementation remains outside the planning map.

The accepted design remains in ADR-0166 through ADR-0174, alongside the linked
derivation, trait-default, runtime-state, and ownership decisions. The later
incremental identity, body-ownership, and operation-fact experiments did not
establish sufficient gains to adopt those implementations. They do not reverse
the accepted design contracts or establish performance gains for a rewrite.

## Reconciliation with current production

- The [native compiler result](compiler-self-compilation-target.md#current-evidence)
  reaches a 9.710-second median at 283.2 MiB median peak RSS. The historical
  slice's statement that ten seconds still needs an 82% reduction no longer
  describes production. The slice itself remains a contract experiment.
- The original 40,192-line compiler audit remains pinned historical evidence.
  Current production differs from that source. Compare the final implementation
  with both the historical audit and a pinned current checkpoint. Separate
  runtime relocation from deletion and identify inseparable behavior changes.
- The bootstrap route uses the stable compiler named by `casa-release.env`,
  currently v1.52.0. The slice's v1.50.0 instructions reproduce its historical
  experiment. Create the next stable release and update `casa-release.env` when
  the newest stable compiler cannot compile valid repository syntax.
- Implementation units 5 and 6 must preserve the current native `call`/`ret`
  convention across callers, callees, closures, destruction, and runtime helpers.
  Callees transfer the CPU return address to Casa's checked return stack before
  reading values. The slice's manual return labels and jumps are historical.
  Runtime extraction must use one consistent protocol throughout.
- Retain the measured frame-initialization, scalar-cache, hashing, String
  construction, and bulk-output gains through the rewrite unless a measured
  replacement satisfies the accepted contracts and performance gates.

## Evidence and limits

The [executable slice measurements](https://github.com/frendsick/casa/blob/25d369c9659bd8b5bcfd3ea5389139811d50218d/compiler/blueprint_prototype/MEASUREMENTS.md)
record 28 passing check groups and three matching native fixture outputs.
They exercise structured control flow, direct and borrowed generic calls,
conditional cleanup, aggregate copy, integer-class extern calls, source rejection,
private checked-program construction, independent editor facts, and packaged
runtime availability outside the checkout. The Python archive requires Python.

The slice omits full recursive generic checking, trait dispatch, loan analysis,
closure ownership, complete ABI/storage combinations, workspace queries, and
malformed-module recovery. Its editor workload retains 21,344 bytes of reachable
snapshot data, while traced allocations still grow. It does not prove a process
memory plateau. The generic-heavy workload is compiled but not executed.

The proposal is to accept this bounded evidence for the blueprint and enforce
the omitted contracts during production implementation. The full compiler must
pass retained behavior suites, all 14 CI shards, standalone installed-runtime
checks, self-hosting, fixed-point assembly equality, native ABI/runtime checks,
and representative repeated-editor workloads. Those obligations are not waived.

The historical estimate remains 820 to 850 production lines and 350 to 500 test
lines removable, with about 500 runtime lines relocatable. These are estimates,
not measured redesign savings. There is no source-deletion or test-deletion quota.
Measure caller knowledge, ownership obligations, representable invalid states,
dependency direction, repeated semantic decisions, and maintained source. The
prototype does not justify scaling its line count to the full compiler.

## Accepted performance gates

The maintainer accepted these gates on 2026-09-28:

- Self-compilation median at most 10.0 seconds on the reference Ryzen 7 3700X
  Linux x86-64 host, including assembly and linking.
- Peak RSS at most 1 GiB in every measured self-compilation run.
- Trade memory for speed only when the measured end-to-end gain is substantial
  and repeatable. Small speed gains do not justify materially higher memory use.
- Common-source regressions beyond observed variation require explicit review
  of the evidence. There is no fixed percentage regression allowance.

Use a fixed-point compiler, one warm-up per configuration, three alternating
measured pairs, raw-clock wall times, all samples, and the
[measurement protocol](compiler-simplification-measurements.md). Self-compilation
and common-source results remain separate. Missing an accepted gate requires
a new evidence-based decision.

## Remaining maintainer decision

Final acceptance must confirm whether the documented slice limits are sufficient
for planning and whether the linked implementation plan can proceed with its
full production gates. Until that confirmation, the blueprint remains open.
