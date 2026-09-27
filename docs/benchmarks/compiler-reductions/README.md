# Compiler reduction experiments

Native evidence for [Choose reductions toward ten-second self-compilation](https://github.com/frendsick/casa/issues/683).
This is a throwaway evidence branch. The maintainer decision and production
rollout remain separate.

The [native code follow-up](native-code.md) reaches a corrected median of
**19.568 seconds**, a **26.8% improvement** over its 26.731-second paired
word-copy control. It tests word copying, leaf expansion, and register lowering
after the five changes below. It records rejected prototypes and the corrected
final implementation.

## First five reductions

Five implementation changes bring median end-to-end self-compilation from
**59.472 to 25.658 seconds** across two incremental studies. That is a **56.9%
reduction between the study endpoints**. Peak RSS falls from **333.5 to
305.7 MiB**. Every adjacent comparison improves all three measured pairs.
These results apply to this self-compilation workload on the reference machine.
They do not establish the ten-second target.

| Cumulative variant | Median seconds | Measured range, seconds | Median peak RSS, MiB | Reduction from preceding variant |
| --- | ---: | ---: | ---: | ---: |
| Control | 59.472 | 59.412–60.069 | 333.5 | |
| Deferred diagnostics | 45.809 | 45.672–45.895 | 332.7 | 23.0% |
| Skip empty global validation | 39.786 | 39.750–39.997 | 306.5 | 13.1% |
| ASCII hash fast path | 30.655 | 30.581–31.165 | 302.9 | 22.9% |
| Shared cleanup discovery | 27.756 | 27.447–27.852 | 301.7 | 9.5% |
| Shared cleanup, repeated control | 28.047 | 27.783–28.107 | 301.7 | |
| Narrow trait method query | 25.658 | 25.433–25.840 | 305.7 | 8.5% |

The largest within-variant range is 1.9% of its median. The percentage gains
are incremental and must not be added. The first five rows form one study.
The final two rows form a separate alternating pair study. Its repeated control
is 1.0% slower than the first study's cleanup median. The first control and final
candidate were not measured as a direct pair.

All six compilers reach identical stage 2 and stage 3 assembly. All 28 warm-up
and measured outputs match their variant's fixed-point assembly.
[evidence.json](evidence.json) records raw timings, peak RSS, source revisions,
compiler hashes, profile counts, and validation summaries.

## Question

Which implementation changes can reduce self-compilation time while preserving
Casa's features? Investigate feature restrictions only when measured costs and
alternative implementations support that tradeoff.

The starting point is the [fixed-point cost profile](../fixed-point-compilation/README.md).
Its 62.126-second median and 327.4 MiB peak RSS describe the pinned control.
The current experiment measures an incremental sequence of candidates. Historical
profile percentages identify experiments, but do not predict their savings.

## Diagnostic context

The control's `set_parameter_context` resolves source-facing operation names and
constructs a message for every checked argument. Only stack underflow and
borrowed-to-owned diagnostics consume that text. Source-name formatting occupies
22.3% of the control's successful self-compilation profile.

The candidate retains owned operation and parameter names, the parameter index,
and the source file in a private `ExpectationContext`. It formats them when a
diagnostic consumes the context. Plain-text builtin contexts retain their
existing behavior. The existing context resets still end its lifetime.

The source-name table is established before semantic checking. Checking and
specialization do not add source-name mappings. Capturing the file at context
creation preserves the lookup input even if other checker work intervenes.

## Other native variants

The global-initializer guard scans the same top-level operations as validation.
When none is an initializer, it returns before constructing a semantic session.
When an initializer exists, the existing validation runs unchanged. This does
not implement the accepted removal of runtime globals.

String hashing now traverses ASCII bytes directly. On a non-ASCII byte, it
restarts with the original decoded-character loop. This preserves every
existing hash value while removing the iterator closure from ASCII hashing.

An earlier byte-only variant was rejected during review. Although
[ADR-0159](../../adr/0159-hashing-is-runtime-local-and-unordered.md) permits
changed hashes, the compiler also uses a binding hash as its complete
specialization identity. Byte hashing introduced new collisions between Unicode
type names. The fallback avoids that regression. Using a hash without comparing
the complete binding key is a pre-existing correctness weakness. Future
identity changes must address it.

The byte-only variant's native reproducer uses structs `Aáab` and `AĀbA`, with sizes 1 and 8.
Their binding keys collide under byte hashing. The rejected compiler reports
generic sizes `1, 1`, while the control and corrected compiler report `1, 8`.
The retained `test_size_of` assertions cover this regression without depending
on specific hash values.

Cleanup specialization now retains one set of discovered concrete types for
the whole `monomorphize_checked_generics` call. The existing guard already
retained every visited type within one cleanup request. Extending that lifetime
avoids repeated discovery across requests. Aggregate declarations and drop-hook
selection remain stable during the pass. The pass can add specialized function
bodies, whose names do not replace source drop hooks. Marking types before
following dependencies still terminates recursive graphs. The set is released
when the pass returns. Runtime cleanup operations remain at every site.

The remaining profile exposed a fifth experiment. `trait_type_has_method`
builds fully bound candidates and clones method definitions to return a boolean.
The candidate searches method names and recursively follows substituted
supertraits. It creates no method candidates and binds no method signatures.
Actual method selection, default precedence, and ambiguity checks retain their
existing paths. The public presence-check signature is unchanged.

An initial nominal-only traversal was rejected. Current substitution rules
allow a trait parameter to shadow a supertrait name. For example,
`trait C[A]: A` instantiated as `C[B]` follows `B`. Ignoring that substitution
rejects a previously accepted qualified method call. The corrected traversal
preserves it, and a focused source fixture protects the accepted call.

## Cost profile after five reductions

The final compiler was sampled at 10 ms intervals during self-compilation.
The sampler uses the same instructions as the measured compiler. Its `.text`
section matches byte for byte, and its output matches the fixed-point assembly.
Sampling adds overhead, so these shares explain cost and do not replace the
uninstrumented timings above.

| Phase | Final samples | Share |
| --- | ---: | ---: |
| Parse and resolve | 472 | 18.4% |
| Typecheck and specialize | 1,453 | 56.5% |
| Bytecode | 179 | 7.0% |
| Assembly emission | 209 | 8.1% |
| Assemble and link | 239 | 9.3% |
| Other | 20 | 0.8% |

These phase shares partition all 2,572 samples. The following inclusive shares
overlap and must not be added.

| Operation | After four changes | After five changes |
| --- | ---: | ---: |
| Any clone operation | 30.8% | 30.2% |
| String clone | 16.4% | 17.2% |
| Type clone | 7.8% | 6.8% |
| String hash | 8.1% | 7.7% |
| Operation analysis | 12.9% | 7.5% |
| Operation fact collection | 8.6% | 6.6% |
| Trait method presence | 8.9% | 0.3% |
| Trait candidate construction | 8.4% | 1.2% |
| Conflicting-loan query | 4.2% | 4.2% |

Cloning remains spread across method resolution, type formatting, scheduling,
bytecode emission, stack effects, and borrow checking. The largest individual
compiler caller before the outermost clone accounts for only 31 final samples.
This supports investigating representation and ownership across shared paths.
It does not support assigning the whole cloning share to one body-copy removal.

The four-change profile has 2,819 samples. The final profile has 2,572.
Aggregate counts are recorded in `evidence.json`.

## Correctness and allocation lifetime

Focused checks cover exact underflow messages and source locations, imported
source names, Unicode map operations and specialization, generic cleanup at
multiple runtime sites, trait diamonds, missing methods, and substituted
supertraits. The corresponding control characterizations pass. The rejected
variants' failures are described above and covered by the retained tests.

The final `tests/test_all.sh` run passes all 14 shards, including compiler tests,
CLI tests, examples, bootstrap, and formatter checks.

The [lifetime workload](lifetime.casa) repeats one successful generic cleanup
request and one diagnostic-producing request 30 times in the same process.
Each result is destroyed before a checkpoint. The [inspector](lifetime.py)
reads allocator block headers and free lists while the child is stopped.
After five warm-up requests, all 25 remaining checkpoints have identical values:

| At each checkpoint | Control | Final candidate |
| --- | ---: | ---: |
| Live blocks / payload bytes | 0 / 0 | 0 / 0 |
| Reusable blocks | 2,124 | 1,980 |
| Reusable payload bytes | 105,208 | 100,664 |
| Mapped heap high-water, bytes | 67,108,864 | 67,108,864 |
| RSS and peak RSS, KiB | 4,244 | 4,236 |

The allocator retains one 64 MiB mapping for reuse. Only touched pages contribute
to RSS. Stable reusable storage with zero live blocks supports reclamation
between these requests. It does not establish a memory bound for arbitrary
programs or long editor sessions.

Copy `lifetime.casa` into the same relative report directory in each source
worktree. Build it with that variant and its library, then run:

```sh
python3 docs/benchmarks/compiler-reductions/lifetime.py /path/to/lifetime-binary lifetime.json
```

The inspector is specific to the retained Linux x86-64 allocator layout.

## Other feature implementation costs

These observations come from the retained control profile and current code.
Inclusive shares overlap.

| Feature or operation | Observed implementation cost | Better implementation to test |
| --- | --- | --- |
| Runtime-global validation | 22 semantic sessions are copied while zero initializer bodies are checked. Validation occupies 9.7% of samples. | Construct validation state only when an initializer exists. This is independent of the already accepted runtime-global removal. |
| String-keyed lookup | String hashing occupies 21.0% of samples. Each hash uses a closure iterator that decodes Unicode characters. | Reduce work inside hashing, then assess stable declaration and type identities if repeated string lookup still dominates. |
| Generic cleanup specialization | 11,807 outer cleanup requests cause 151,209 recursive visits, occupying 6.7% of samples. Each request creates a new type visited set. | Share concrete cleanup discovery across the compilation request while retaining every runtime cleanup operation. |
| Semantic snapshots and results | Cloning appears in 27.8% of samples. Results and the symbol store can own duplicate copies of the same checked body. | Move bodies and result fields into their final owner. Preserve isolation for callers that borrow an unchanged source store. |
| Operation interpretation | Resolution occupies 8.1% of samples. Analysis and fact collection each run 114,362 times. | Carry established operation facts into checking and dependency discovery. |

The profile does not support body-result caching as the first experiment:
2,817 recorded body checks have distinct function names and no repeated recorded
contexts. The remaining-function scheduler occupies 189 of 5,944 samples,
about 3.2%, including its bookkeeping. This does not isolate unused generic
checking, but it gives no support for weakening that diagnostic guarantee as
the first reduction. Generic-cycle validation occupies about 0.08%. These
findings do not justify restrictions on generics, recursion, or ownership.

Some work follows from the feature contract. Monomorphization must create each
distinct reachable instance. Ownership must validate moves, loans, and cleanup
obligations. Deterministic destruction must retain cleanup at every relevant
runtime site. Repeatedly discovering the same type graph, cloning a checked body
into several intermediate results, and formatting unused diagnostics are
implementation choices.

## Broader design alternatives

All three alternatives use in-memory compiler state. None needs a transport
adapter or a persistent cache.

| Alternative | Interface and ownership | Tradeoff |
| --- | --- | --- |
| Minimal semantic interface | `check(SourceProgram, report)` produces checked recipes and editor facts. `specialize(recipes, report)` produces the checked program. Canonical declaration and instance identities remain private. | Hides scheduling and cloning protocols, but a complete identity migration is substantial. Its speed benefit is unmeasured. |
| Immutable declarations with owned semantic work | Written declarations remain readable while one work item owns a changing body. Checked effects, recipes, and editor facts are separate products. | Supports independent compiler products without whole-store snapshots. Keeping old and new representations together would add cost, so migration must replace ownership paths. |
| Optimize the ordinary request | Keep the public `type_check` interface. A private traversal owns both processed functions and visited concrete cleanup types for the request. | Smallest change for repeated cleanup discovery. It retains current key representations and dependency rules, so string costs remain. |

## Recommendation and decision still needed

Use the five measured changes as inputs to the compiler redesign. Preserve
generics, trait defaults, ownership, and deterministic destruction. The evidence
does not show that any of those features is fundamentally too expensive.
No additional language restriction or contract supersession is recommended.

The [native code follow-up](native-code.md) records the current endpoint and
remaining profile. It recommends canonical declaration and type identities with
immutable declarations and one owner for changing semantic bodies. Cloning and
string lookup remain distributed costs. Removing a whole-body copy alone cannot
be assumed to recover the full inclusive cloning share. Specialization identities
must compare complete binding keys so hash collisions cannot alias instances.

Within that representation, carry resolved operation facts into checking.
`collect_operation_facts` currently calls `analyze_operation`, retains symbol
dependencies, and drops its method result. The checker then resolves operations
again. Measure reuse within one operation before selecting a broad cache.

ADRs 0167, 0170, 0172, and 0173 permit these private implementation choices.
Their combined timing benefit is still unmeasured. The native code follow-up
records the current endpoint and remaining gap to ten seconds. The route remains
a hypothesis, and this evidence does not justify declaring the blueprint complete.

The maintainer authorized further native experiments. The acceptable timing
band around ten seconds and a numeric memory guardrail remain open. This issue remains open
for that discussion. Production rollout remains outside this planning map.

## Measurement protocol

The reference machine is a Ryzen 7 3700X under WSL2, with 16 logical processors
and 15.6 GiB available physical memory. The emitted compiler is single-threaded.
Native builds use `/usr/bin/as` and `/usr/bin/ld` from GNU binutils 2.38, through
`/usr/bin/cc` 11.4.0. Machine and toolchain versions are recorded in `evidence.json`.

The evidence branch retains each implementation step as a separate commit:

| Source variant | Commit |
| --- | --- |
| Control | `848d28b` |
| Deferred diagnostics | `c9be994` |
| Empty-global guard | `8642602` |
| ASCII hash fast path | `42968e8` |
| Shared cleanup discovery | `a19f00e` |
| Narrow trait method query | `5d1b077` |

Create a detached worktree at the chosen commit for each source directory.
Use the benchmark scripts from this report's revision for all variants.

Build each variant from stable v1.50.0 before measurement:

```sh
python3 docs/benchmarks/compiler-reductions/bootstrap.py \
  ./casac /path/to/variant-source /path/to/variant-build
```

This builds stages 1 through 3, retains commands and hashes, and requires
identical stage 2 and stage 3 assembly. Measurements use stage 3.

`compare.py` calls the retained raw-clock measurement helper. All builds run
serially, without concurrent compilation or profiling. The variants form an
incremental sequence: control, deferred diagnostics, empty-global guard,
ASCII hashing, and shared cleanup discovery. A second study compares shared
cleanup discovery with the narrow trait method query.

1. Warm up each variant once, in sequence.
2. Measure each variant in sequence.
3. Measure each variant in reverse sequence.
4. Measure each variant in sequence.

Each adjacent pair therefore has one warm-up and three consecutive measured
pairs in alternating order. Intermediate variants serve as both the candidate
for one comparison and the control for the next. These shared samples are
correlated observations, not independent repetitions. Each adjacent comparison
changes one cause.

Each compiler compiles its own source revision through assembly and linking.
All use the same CLI options, including `--keep-asm --verbose`. The report
records source revisions, compiler and fixed-point assembly hashes, raw elapsed
times, CPU accounting, and peak RSS. Warm-ups are excluded from medians.
Every measured output must match the other outputs for that same variant.

Create a JSON array in comparison order. Each entry contains `name`, `compiler`,
and `source`, for example:

```json
[
  {"name": "control", "compiler": "/path/to/control/stage3", "source": "/path/to/control-source"},
  {"name": "lazy", "compiler": "/path/to/lazy/stage3", "source": "/path/to/lazy-source"}
]
```

```sh
python3 docs/benchmarks/compiler-reductions/compare.py variants.json output
```

Use direct `CLOCK_MONOTONIC_RAW` readings for comparisons on the reference
machine. The prior investigation found materially different clock rates between
raw and adjusted monotonic clocks. The comparison uses raw-clock elapsed times.
