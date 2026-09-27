# Canonical identity experiments

Evidence for [Prototype canonical identities in semantic checking](https://github.com/frendsick/casa/issues/687).

The combined prototype gives a small, workload-dependent gain. Compiling each
variant's own source gives a 0.4% median difference with mixed paired results.
A follow-up using identical input sources improves the median from 19.636 to
19.027 seconds, or 3.1%, with improvement in all three pairs.

Keep these changes experimental. The next experiment should remove repeated
ownership of checked function bodies between analysis and scheduling. No language
feature reduction is justified by these results.

## Changes tested

The control is merged main at `549c68c`, containing the corrected compiler from
the [native code study](../compiler-reductions/native-code.md). Its executable
matches that study's final compiler hash. A fresh baseline has a 19.304-second
median, with a 19.227–19.324-second range.

| Variant | Representation change |
| --- | --- |
| Borrowed scope types | Return borrowed types from scope lookup. Clone only where a caller needs an independent value. |
| Per-function type IDs | Store structural type IDs in scope bindings, backed by a pool owned by each checker. Scope snapshots clone the pool. |
| Trait implementation IDs | Store each implementation once. Candidate indexes contain numeric IDs and return borrowed declarations. |
| Request type IDs | Move the type pool into `SymbolStore`. Checkers and guard scopes use that store's immutable type nodes. |
| Combined | Request type IDs and trait implementation IDs together. |

The scope variants remove the unconditional clone on successful lookup. The ID
variants also avoid cloning a type on every binding update. Request type IDs
avoid copying types when a guard snapshots its bindings. Presence queries read
only a binding ID. Scope names still use string keys.

Trait candidate iteration removes the copied implementation name and the name
map lookup for each candidate. A query still formats and looks up its receiver
or receiver/trait key. A separate group ID lets the iterator borrow its index
without retaining a borrow of that temporary key.

Borrowed scope types still require copies at ownership-analysis boundaries.
The shared pool needs two additional copies at loop-loan checks because those
checks mutate a cache in the same store. The implementation keeps these copies
explicit.

## Identity and ownership boundaries

Type IDs index immutable structural `Type` values. Interning hashes the complete
structure and compares full `Type.eq` within a hash bucket. Type and constant
arguments remain distinct. A focused test uses the colliding names `Ab` and `BA`
to check that equal hashes do not produce equal IDs.

IDs belong to one store. A scope snapshot shares its store's pool while owning
independent binding maps. A new semantic session creates an independent store
and pool. The migrated IDs do not replace the owned types and functions in
query results or compiler products.

Type-variable names retain their surrounding declaration context. These IDs
alone are not specialization keys. The prototype does not change specialization
identity or authorize a change to [ADR-0170](../../adr/0170-generics-specialize-after-symbolic-checking.md).

Trait declarations remain owned by the store. Construction and replacement use
exclusive access. Resolution borrows declarations. Replacement keeps the same
ID for the same complete declaration key. Pruning releases the declaration and
rebuilds its indexes. Removed slots are not reused before the store is released.

Changing function bodies retain their existing owners and copies. Trait
candidate iteration does not mutate a body. Semantic sessions still clone the
declarations they need for independent analysis, as required by
[ADR-0167](../../adr/0167-compiler-products-own-independent-snapshots.md).

## Measurements

Every variant reaches identical stage 2 and stage 3 assembly. Each study uses
one warm-up and three measured rounds, reversing the order in round two. Each
compiler compiles its own source snapshot. Added prototype code is included in
that workload. No compilation, tests, or profiling run concurrently with a timed
comparison. Times use `CLOCK_MONOTONIC_RAW`.
Compare within each study because the control shifts between studies.

| Study | Variant | Median seconds | Range | Median peak RSS, KiB |
| --- | --- | ---: | ---: | ---: |
| Borrowing | Control | 18.821 | 18.735–18.882 | 304,252 |
| Borrowing | Borrowed scope types | 18.866 | 18.821–18.910 | 307,324 |
| Per-function pool | Control | 20.245 | 20.089–20.392 | 304,124 |
| Per-function pool | Type IDs | 20.202 | 20.110–20.296 | 305,276 |
| Per-function pool | Trait IDs | 20.096 | 19.919–20.202 | 304,508 |
| Per-function pool | Both | 20.125 | 19.985–20.201 | 305,660 |
| Shared pool | Control | 19.034 | 18.987–19.098 | 304,252 |
| Shared pool | Trait IDs | 18.853 | 18.842–18.932 | 304,572 |
| Shared pool | Type IDs | 19.076 | 18.828–19.587 | 307,836 |
| Shared pool | Both | 18.955 | 18.745–19.035 | 307,452 |

Borrowing alone is 0.2% slower by the paired medians. All gains in the
per-function study are below 0.8%. In the shared-pool study, trait IDs improve
the median by 1.0%, with improvement in all three pairs. That result supports at
most a small gain. The earlier trait-ID study has one slower pair.

Shared type IDs are 0.2% slower. Their combined result improves in two pairs and
regresses in one. Peak RSS increases by 3,200 KiB for the combined variant.
The benefit depends on the input and comparison. These results do not establish
that a broad migration of semantic values to IDs will provide a large gain.

### Common-source check

The retained, formatted prototype adds 197 net lines across three compiler files
and two test files. To separate compiler behavior from this added source, both
compilers also compile the retained prototype's source at `c57c7de`:

| Compiler | Median seconds | Range | Median peak RSS, KiB |
| --- | ---: | ---: | ---: |
| Control | 19.636 | 19.136–19.816 | 306,940 |
| Combined prototype | 19.027 | 18.496–19.132 | 307,452 |

All eight outputs are identical to the prototype's fixed-point assembly.
Measured pair improvements range from 0.6% to 5.8%. The median improves by 3.1%,
while the ranges cover about 3.4% of each median. This supports a small gain on
this shared input. It does not establish a consistent 3.1% improvement in normal
self-compilation, where changing the compiler also changes its input source.

## Correctness and reclamation

Six focused fixtures pass: block scope, scope behavior, traits, constant
parameters, analysis products, and document products. They cover hash collisions,
distinct constants and function results, independent scope snapshots, implementation
replacement, pruning and reinsertion, diagnostics, and retained products.
All 14 shards pass in the final `tests/test_all.sh` run on the reviewed prototype.

`lifetime.casa` repeats a successful generic trait call and a diagnostic-producing
request 30 times in one process. The successful request includes checking,
bytecode generation, and assembly emission. The existing
[lifetime sampler](../compiler-reductions/lifetime.py) records live allocations,
reusable allocator storage, mapped heap, and RSS at 31 stop points.

Both workloads return to zero live blocks and payload bytes after every request.
All 25 samples after warm-up are identical within each workload:

| At each checkpoint | Control | Combined prototype |
| --- | ---: | ---: |
| Reusable blocks | 2,077 | 2,091 |
| Reusable payload bytes | 151,992 | 152,904 |
| Mapped heap high-water, bytes | 67,108,864 | 67,108,864 |
| RSS and peak RSS, KiB | 5,236 | 5,248 |

The allocator retains its mapped chunk for reuse. This workload demonstrates
reclamation between these requests. It does not establish a memory bound for
arbitrary inputs.

## Remaining cost and next experiment

The control profile contains 1,828 samples and the combined prototype contains
1,821, sampled at 10 ms intervals. Each profile executable has the same `.text`
bytes as its corresponding fixed-point compiler. Profiled output matches that
compiler's fixed-point assembly. Sampling adds overhead, so these counts explain
cost rather than establish latency.

| Inclusive operation | Control samples | Prototype samples |
| --- | ---: | ---: |
| Any clone | 473 (25.9%) | 440 (24.2%) |
| Type clone | 130 (7.1%) | 112 (6.2%) |
| String hash | 166 (9.1%) | 179 (9.8%) |
| Scope lookup | 40 (2.2%) | 5 (0.3%) |
| Type interning | 0 | 7 (0.4%) |
| Trait candidate iteration | 7 (0.4%) | 1 (0.1%) |

These shares overlap and must not be added. The migrated lookups account for
little of the remaining cost. Cloning still appears in about one quarter of
samples. Method resolution continues to copy type and stack-effect structures,
and typed stack values still own their types.

The code still copies checked bodies across the analysis/scheduling boundary:

- `analyze_function_semantics_in_store` clones checked operations into the
  function, then clones the function back into the store.
- `analyze_function_semantics_in_session_mode` clones the analysis result's
  operations, checked function, facts, effects, and diagnostics into its return.
- `schedule_functions` clones the scheduled function before checking and the
  checked function before committing it.

These copies are outside the migrated scope and trait-index paths. The next
experiment should give a checked body one owner, transfer that result into the
scheduler, and let queries borrow immutable signatures. Keep an owned copy only
where an independent retained product requires it. Measure that change before
combining it with a broader identity migration.

The ten-second target still requires roughly half the current compilation time.
This experiment does not establish that the remaining copies can supply that
reduction. It also does not show that generics, traits, ownership, or deterministic
destruction require language restrictions.

## Evidence and reproduction

The [evidence file](evidence.json) retains raw timing rows, peak RSS, compiler
and assembly hashes, profile counts, and reclamation results. Full logs, raw
sample stacks, and superseded experiment patches are omitted.

The prototype is retained on
[`perf/687-canonical-identities`](https://github.com/frendsick/casa/tree/perf/687-canonical-identities).
The trait-ID change is [`b711e11`](https://github.com/frendsick/casa/commit/b711e11047f259a28e2b0b131e325e52819fe73a)
and the combined implementation is [`c57c7de`](https://github.com/frendsick/casa/commit/c57c7de726678468e773ab14a5dc8deafe41e34b). Retained source revisions
are checked against the measured source hashes. Superseded per-function and
unformatted prototypes retain timing summaries and hashes only. Production
adoption is a separate decision.

The existing native measurement tools accept a JSON array of variant records
with `name`, `compiler`, and `source` paths:

```sh
python3 docs/benchmarks/compiler-reductions/bootstrap.py \
  /path/to/bootstrap /path/to/source /path/to/build
python3 docs/benchmarks/compiler-reductions/compare.py variants.json output
/path/to/build/stage3 -L /path/to/source/lib \
  /path/to/source/docs/benchmarks/canonical-identities/lifetime.casa \
  -o /path/to/lifetime-binary
python3 docs/benchmarks/compiler-reductions/lifetime.py \
  /path/to/lifetime-binary lifetime.json
```

Copy `lifetime.casa` into the same relative path in each source checkout before
building it. For the common-source comparison, both variant records use the
prototype's source path, with their respective compiler executables.
