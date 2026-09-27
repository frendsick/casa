# Native code experiments

Further evidence for [Choose reductions toward ten-second self-compilation](https://github.com/frendsick/casa/issues/683).
These experiments continue the [first five reductions](README.md). They test
whether generated code makes ordinary compiler operations unnecessarily costly.
They do not complete the backend migration in ADR-0169 or authorize a language
restriction.

## Question and alternatives

The previous final compiler takes 25.658 seconds in its retained study. Its
profile places 9.6% of leaf samples in the byte-at-a-time `memcpy` loop. Another
17.3% fall in `str.length`, `str.at`, `String.as_str`, and `read_pointer`.
These are locations where time was sampled, not predicted savings.

The generated `str.length`, `String.as_str`, and `read_pointer` functions each
reserve and zero a local slot, store their argument, reload it, perform a load,
release the slot, and return. This provides a concrete native experiment before
changing semantic representations.

Three backend designs were considered:

| Design | Private responsibility | Main cost or limit |
| --- | --- | --- |
| Remove redundant parameter storage | Plan the required frame after lowering the function body. | A narrow pattern improves only some functions. |
| Expand bounded leaf bodies | Select reusable primitive instructions for direct calls, preserving callable symbols. | More expansion can increase native code size and compilation work. |
| Keep scalar values in registers | Plan value locations within straight-line code, then flush at control-flow and call boundaries. | Register bookkeeping and larger instructions can offset reduced stack traffic. |

All three can fit behind the backend's existing entry. The eventual private
per-function machine plan can own these decisions. This experiment works on the
current lowered instructions to avoid combining native instruction selection
with the pending checked-program migration.

## Implementations tested

| Variant | Parent | Change |
| --- | --- | --- |
| Word copy | Narrow trait query from the first study | Copy eight bytes per iteration, followed by the byte tail. |
| Forwarding frame | Word copy | Remove a sole parameter's adjacent store/reload and its physical frame when no remaining instruction observes it. |
| Leaf expansion | Forwarding frame | Expand direct calls to eligible leaf bodies with at most eight instructions. |
| Scalar cache | Leaf expansion | Keep up to four scalar values in registers. Flush when an input is outside the cached suffix. |
| Scalar refill | Leaf expansion | Use a circular register order to load missing operands without moving existing cached values. Use shorter moves for small constants. |
| Framed expansion | Leaf expansion | Admit up to sixteen instructions and four local slots, preserving the local frame and allowing primitive stores. |

The register experiments retain distinct registers for duplicated values.
Local writes always update memory. There is no local-value forwarding across
mutation. Calls, branches, closures, foreign calls, and unsupported instructions
receive the original physical value stack. Narrow checked arithmetic and numeric
conversions retain their existing lowering.

Leaf templates take ownership of eligible function instruction lists. They do
not clone whole bodies. Their owner is one assembly request. All function symbols
and static closure records remain available. Ineligible functions retain the
ordinary call path. Templates exclude nested calls, captures, local addresses,
branches, and runtime helper operations.

The selected implementation excludes checked arithmetic from leaf eligibility.
Its error writer needs a physical return-stack slot, so capacity-only checks
cannot preserve its failure behavior near the stack limit. Checked arithmetic
still uses scalar registers inside ordinary physical frames.

Removed return-stack storage retains its original capacity requirement. A direct
expansion checks both the original call slot and callee frame. A forwarding
function still checks its removed local slot. Arithmetic instructions retain
their original order and checks. There is no source-name special case.

The byte-copy contract is unchanged. Source and destination ranges must be
initialized/readable and writable as appropriate, and must not overlap. The
loop reads a full word only when at least eight bytes remain. ADR-0130 permits
unaligned raw integer accesses on this target, and ADR-0135 permits a measured
optimized implementation.

## Measurements

The copy study uses one warm-up and three alternating measured pairs:

| Variant | Median seconds | Range | Median peak RSS, KiB |
| --- | ---: | ---: | ---: |
| Narrow trait query | 25.627 | 25.325–25.920 | 312,908 |
| Word copy | 24.420 | 23.971–24.423 | 309,244 |

Word copying improves the paired median by 4.7%. All three measured pairs improve.
All eight warm-up and measured outputs match their variant's fixed-point
assembly.

The initial six-variant study also uses one warm-up and three alternating rounds:

| Prototype | Median seconds | Range | Median peak RSS, KiB |
| --- | ---: | ---: | ---: |
| Word copy | 27.177 | 26.802–27.187 | 309,244 |
| Forwarding frame | 24.096 | 21.603–24.514 | 309,336 |
| Leaf expansion | 21.387 | 19.359–22.224 | 308,592 |
| Scalar cache | 20.244 | 17.910–20.339 | 308,852 |
| Scalar refill | 20.042 | 17.780–20.132 | 307,964 |
| Framed expansion | 21.529 | 19.292–21.905 | 312,764 |

All 24 outputs match their variant's fixed-point assembly. Scalar refill improves
over word copy in all three rounds. Its median is 26.2% lower. Its gain over the
simpler scalar cache is only 1.0%, so this study gives limited evidence for the
extra refill bookkeeping. Broader framed expansion gives no clear gain over
leaf expansion and is not selected.

Ranges reach 12–14% of the backend medians. The third round is faster across the
backend variants, while the copy control is steadier. These observations do not
establish a stable 20-second latency. CPU time is close to elapsed time, and no
other compilation, tests, or profiling ran during these measurements.

### Rejected failure behavior

Review reproduced a correctness defect in the initial scalar-refill design.
A checked arithmetic failure at the exact return-stack boundary reports
`error: integer arithmetic failed` where the control reports
`error: return stack overflow`. The original leaf call and local frame fill the
stack. The error writer then cannot reserve its own call slot. Eliding those
physical slots changes the failure even when the leaf checks their capacity.

The corrected design excludes checked arithmetic from both forwarding-frame
removal and direct leaf expansion. The ordinary register lowering remains.
Direct and function-reference fixtures protect the combined failure case.
A separate nontrapping leaf verifies the exact successful/overflow boundary.
The backend prototype timings above therefore describe superseded code. The
corrected comparison below establishes the final gain. An interrupted follow-up study was discarded when
review found this defect.

### Corrected final comparison

A fresh study compares the corrected final compiler with the word-copy control:

| Variant | Median seconds | Range | Median peak RSS, KiB |
| --- | ---: | ---: | ---: |
| Word-copy control | 26.731 | 26.717–26.817 | 308,988 |
| Corrected final compiler | **19.568** | **19.120–19.678** | **305,340** |

The final median improves by **26.8%**, with improvement in all three pairs.
All eight warm-up and measured outputs match their variant's fixed-point
assembly. Final peak RSS is 298.2 MiB. The candidate range is 2.9% of its median.
The copy control is 9.5% slower than in the earlier copy-only study, so gains
must be assessed within each paired study.

The selected changes are retained separately:

| Change | Commit |
| --- | --- |
| Word copying | `404e308` |
| Forwarding parameter storage | `b0ee6e1` |
| Nontrapping leaf expansion | `c0c559f` |
| Scalar register lowering | `ea0ec52` |

The intermediate forwarding and leaf commits pass their emitter checks. Their
correctness corrections differ from the initial measured prototypes. Only the
corrected final compiler receives the final performance claim.

## Remaining cost and route toward ten seconds

The final profile contains 1,957 samples at 10 ms intervals. Its executable has
exactly the same `.text` bytes as the measured compiler, and its generated
assembly matches the fixed point. Sampling adds overhead, so the profile
explains cost rather than establishing another latency result.

| Phase | Samples | Share |
| --- | ---: | ---: |
| Parse and resolve | 338 | 17.3% |
| Typecheck and specialize | 1,087 | 55.5% |
| Bytecode | 148 | 7.6% |
| Assembly emission | 161 | 8.2% |
| Assemble and link | 205 | 10.5% |
| Other | 18 | 0.9% |

The following inclusive shares overlap and must not be added:

| Operation | Samples | Share |
| --- | ---: | ---: |
| Any clone operation | 506 | 25.9% |
| String clone | 237 | 12.1% |
| Type clone | 140 | 7.2% |
| String hash | 176 | 9.0% |
| Operation analysis | 138 | 7.1% |
| Operation fact collection | 116 | 5.9% |

Cloning remains distributed. The largest individual compiler caller before the
outermost clone accounts for 29 samples. Leaf samples in `memcpy` fall from 9.6%
in the prior profile to 4.2%. `str.at` still accounts for 6.0%. Expanded helpers
are attributed to their callers, so their absence as leaf symbols is not itself
a savings measurement. Full samples are retained in [native-profile.json](native-profile.json).

The compiler's generated code has fewer instructions and stack operations, but
its machine-code section grows:

| Static output measure | Word-copy control | Final candidate |
| --- | ---: | ---: |
| Assembly instruction lines | 1,311,091 | 1,189,632 |
| `pushq` / `popq` instructions | 206,864 / 168,469 | 115,585 / 79,842 |
| `.text` bytes | 4,873,095 | 5,112,014 |

These are static counts, not executed instruction counts. The 4.9% machine-code
size increase is a cost of this implementation even though end-to-end time
improves.

Ten seconds requires a further **48.9% reduction** from the corrected median.
That remains ambitious. These measurements support a broader representation
experiment, but do not prove that it can supply the missing reduction.

The next substantial experiment should give declarations and types canonical
identities owned by one compilation request. Immutable declarations should be
shared through those identities, while each changing semantic body has one
owner. Start across method resolution, block-scope lookup, scheduling, and stack
effects, where the profile shows distributed cloning. The current
[`SymbolStore`](../../../compiler/common.casa#L3420) keys declarations by owned
strings, and [`BlockScope.lookup`](../../../compiler/block_scope.casa#L52) clones
the type on each successful lookup. These are concrete boundaries for the
identity experiment. Replace existing copies
and string-keyed paths instead of retaining parallel representations. Binding
identities must compare complete keys so hash collisions cannot alias generic
instances.

Within that design, retain resolved operation facts until their checker consumer
has used them. Fact collection currently discards resolution work that checking
can repeat. Keep the validity boundary explicit when types, literal hints, or
method context change. Measure each representation step and the combined
end-to-end result with the same memory-lifetime checks.

No feature restriction is justified yet. Generics, ownership, trait defaults,
and deterministic destruction have implementation alternatives left to test.
The current evidence supports further redesign, with ten seconds as a target
that still needs measurement. Production adoption and the maintainer's final
tradeoff decision remain separate.

## Correctness and lifetime checks

Every candidate reaches identical stage 2 and stage 3 assembly. The copy test
covers lengths zero through 65 and every source/destination alignment within an
eight-byte word. It checks destination bytes outside the requested range and the
zero-length null-pointer case.

The call tests cover direct calls, function references, a captured caller, and
reassigned parameters. An assembly check verifies call expansion, retained
callable symbols, the original capacity check, and fallback for a local address.
The register experiments also exercise spills, independent duplicates, stack
order, and reads retained across local and memory writes.

The initial 78 executions compare six signed and unsigned 64-bit overflow cases
at top level and inside leaves, plus the return-stack boundary, across all six
prototypes. They agree with the control, but did not combine arithmetic and
return-stack failure. The corrected comparison adds that missing case for both
direct and function-reference calls. All 30 executions agree with the control.
The final `tests/test_all.sh` run passes all 14 shards, including 89 formatter
checks and bootstrap. Seven focused fixtures pass. Inspection of the nontrapping leaf assembly
confirms expansion, a retained standalone symbol, and its capacity check.

`backend-lifetime.casa` repeats a successful complete compilation and a semantic
error in one process. The existing `lifetime.py` sampler inspects live blocks,
reusable allocator storage, mapped heap, and RSS at 31 stop points. The successful
source includes a nontrapping leaf and an ordinary caller so the selected
implementation creates and releases a populated template map.

Both final workloads return to zero live blocks and payload bytes after every
request. Their 25 post-warm-up samples are unchanged:

| At each checkpoint | Word-copy control | Final candidate |
| --- | ---: | ---: |
| Reusable blocks | 972 | 972 |
| Reusable payload bytes | 80,368 | 80,368 |
| Mapped heap high-water, bytes | 67,108,864 | 67,108,864 |
| RSS and peak RSS, KiB | 4,912 | 5,152 |

The mapped chunk is retained for reuse. These results support reclamation between
these requests. They do not establish a bound for arbitrary compiler inputs.

## Reproduction

The [evidence file](native-code.json) retains source and compiler hashes, native
builds, complete timing samples, checks, and memory checkpoints. The patches in
[native-patches](native-patches) reconstruct the original prototypes and final
compiler from commit `eb69995f8a0c16b7414d7c7440610e611ba7e88e`. Each patch applies
independently with `git apply`. Reconstruction was checked against every source
hash in the corresponding snapshot. Rejected patches preserve experimental
behavior and must not be used as the selected implementation.

Use the same native toolchain and serial measurement protocol as the first
study. Each compiler measures its own source snapshot. No compilation, test run,
or profiling runs concurrently with the timed comparison.

```sh
python3 docs/benchmarks/compiler-reductions/bootstrap.py \
  ./casac /path/to/variant-source /path/to/variant-build
python3 docs/benchmarks/compiler-reductions/compare.py variants.json output
python3 docs/benchmarks/compiler-reductions/check-native.py variants.json checks
python3 docs/benchmarks/compiler-reductions/lifetime.py \
  /path/to/backend-lifetime-binary lifetime.json
```
