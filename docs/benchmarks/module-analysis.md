# Module analysis efficiency investigation

Status: complete investigation.

Source revision: [`0694ddd`](https://github.com/frendsick/casa/commit/0694ddd71d941f5d188bde0543142cd8089cefeb), 2026-08-29.

Run date: 2026-08-30.

## Finding

The main avoidable cost is not one clone call. Module analysis uses the
importer's cumulative `SymbolStore` as the input, work area, cache source, and
merge result. Each cache miss therefore copies and scans declarations that do
not belong to the imported source.

Keep the existing `parse_and_resolve` interface and deepen its implementation.
One private module analyzer should own a single working store, one canonical
record per resolved source, alias bindings, cycle state, and selective-retention
sets. It should move the successful working store into `ParseResolveResult`
after one final prune. No module cache or adapter needs to escape the
parse-and-resolve seam.

The current revision reproduced the same shape at a larger source size.
Self-compilation had a 27.68-second median wall time and 747,708 KiB median
peak RSS. The phase build measured 28.71 seconds in analysis, split between
9.35 seconds in parse and resolution and 19.28 seconds in typechecking. A
separate clone build observed 42 deep `SymbolStore` copies. The largest copy
contained 3,412 functions, 115 structs, 32 enums, 10 traits, and 149 trait
implementations.

Store preparation, cache copying, and symbol merging consumed 3.83 seconds,
or 41% of the measured parse-and-resolve time. The qualified graph spent 0.99
seconds of 2.15 seconds of resolution on the same work. The selective graph
spent 0.87 seconds on store, cache, and merge work, plus 0.22 seconds on
selective dependency discovery. These measurements support removing
cumulative stores before optimizing the closure algorithm.

## Current flow

`analysis::analyze` creates a new `SourceStore`, loads all overrides, adds the
root source, and calls `parse_and_resolve`. Typechecking starts only when that
pass returns usable operations. The module cache therefore lasts for one
analysis call. It is not shared between CLI or LSP compilations
([`compiler/analysis.casa` lines 49-75](https://github.com/frendsick/casa/blob/0694ddd71d941f5d188bde0543142cd8089cefeb/compiler/analysis.casa#L49-L75)).

Before normal parsing, `namespace_module_tokens` scans top-level imports and
declarations, then rewrites names to an internal namespace. Path imports use
the alias in that namespace. Module imports use the module specifier. A
path-style alias or module specifier is therefore part of analyzed symbol
identity today
([`compiler/syntax.casa` lines 5414-5639](https://github.com/frendsick/casa/blob/0694ddd71d941f5d188bde0543142cd8089cefeb/compiler/syntax.casa#L5414-L5639),
[`compiler/syntax.casa` lines 5759-5803](https://github.com/frendsick/casa/blob/0694ddd71d941f5d188bde0543142cd8089cefeb/compiler/syntax.casa#L5759-L5803)).

### Qualified imports

On a cache miss, `handle_full_import` does this work:

1. Reject a cycle by resolved path.
2. Build a cache key as `namespace|path`.
3. Deep-clone the importing store.
4. Lex, namespace, parse, recursively expand imports, and resolve the imported
   file against that clone.
5. Deep-clone the cumulative result for the cache, then scan it again to retain
   declarations owned by the imported file.
6. Merge the cumulative result back into the importer.

On a cache hit, it still deep-clones the cached source declarations before the
merge. Imported root operations are omitted. Immutable-global initializers are
kept
([`compiler/syntax.casa` lines 6027-6092](https://github.com/frendsick/casa/blob/0694ddd71d941f5d188bde0543142cd8089cefeb/compiler/syntax.casa#L6027-L6092),
[`compiler/syntax.casa` lines 7601-7651](https://github.com/frendsick/casa/blob/0694ddd71d941f5d188bde0543142cd8089cefeb/compiler/syntax.casa#L7601-L7651)).

### Selective imports

A selective cache miss takes three deep store copies: the importing store and
two copies of the cumulative analyzed result. One result feeds dependency
discovery. The other is pruned for the cache. A hit still clones the cached
declarations. The handler validates that each requested declaration is public,
computes its dependency closure, checks all conflicts, then commits the closure
transactionally. Private dependencies remain private, and only retained
global initializers are appended
([`compiler/syntax.casa` lines 5843-6025](https://github.com/frendsick/casa/blob/0694ddd71d941f5d188bde0543142cd8089cefeb/compiler/syntax.casa#L5843-L6025),
[`compiler/selective_import.casa` lines 868-968](https://github.com/frendsick/casa/blob/0694ddd71d941f5d188bde0543142cd8089cefeb/compiler/selective_import.casa#L868-L968)).

The closure builder visits each discovered symbol once, but some visits scan
the full function map for methods. Finalization scans all trait
implementations. Global dependency collection scans initializers
([`compiler/selective_import.casa` lines 384-432](https://github.com/frendsick/casa/blob/0694ddd71d941f5d188bde0543142cd8089cefeb/compiler/selective_import.casa#L384-L432),
[`compiler/selective_import.casa` lines 662-709](https://github.com/frendsick/casa/blob/0694ddd71d941f5d188bde0543142cd8089cefeb/compiler/selective_import.casa#L662-L709),
[`compiler/selective_import.casa` lines 812-865](https://github.com/frendsick/casa/blob/0694ddd71d941f5d188bde0543142cd8089cefeb/compiler/selective_import.casa#L812-L865)).

### Clone, cache, and merge behavior

Both `SymbolStore` clone forms walk all function declarations and clone all
other declaration maps through `merge_import_metadata`. `merge_import` walks
the imported function map, then walks the remaining declaration maps again.
`retain_file_declarations` scans those maps after the cache clone
([`compiler/syntax.casa` lines 472-640](https://github.com/frendsick/casa/blob/0694ddd71d941f5d188bde0543142cd8089cefeb/compiler/syntax.casa#L472-L640),
[`compiler/syntax.casa` lines 643-785](https://github.com/frendsick/casa/blob/0694ddd71d941f5d188bde0543142cd8089cefeb/compiler/syntax.casa#L643-L785)).

The cache stores declarations owned by one source file, not its dependencies.
That is the correct ownership shape. Its key is not canonical because it also
contains the rewritten namespace. The same path-style source imported through
two aliases gets two analyses and two cache entries. Repeated imports with the
same alias reuse one entry, including selective-then-qualified use
([`tests/compiler/test_parser.casa` lines 1091-1132](https://github.com/frendsick/casa/blob/0694ddd71d941f5d188bde0543142cd8089cefeb/tests/compiler/test_parser.casa#L1091-L1132)).

This cache shape came from two valid changes. Commit
[`2b162421`](https://github.com/frendsick/casa/commit/2b162421600401e24bc7dfb1b68ced35eaa48bc0)
introduced shared full/selective analysis with source-owned cache entries.
Commit [`bee333de`](https://github.com/frendsick/casa/commit/bee333de954de4f84784bf0fee066254f8ccc22f)
added private namespaced modules and made namespace part of the key because
namespace rewriting happens before parsing.

## Static complexity

Let:

- `M` be canonical imported sources.
- `A` be import aliases and repeated import encounters.
- `D` be all declarations in the compilation.
- `C_i` be declarations in the importing store at cache miss `i`.
- `R` be declarations reached by one selective import.
- `F` and `I` be functions and trait implementations visible to its closure.

The clone and merge work is `Theta(sum(C_i))`. With declarations spread across
modules, this is `Theta(M * D)` in the worst case and quadratic when declaration
count grows with module count. A deep import chain can keep several cumulative
stores alive at once, so transient declaration storage can also approach
`Theta(sum(C_i))`.

Selective discovery adds its own work. Visited symbols prevent repeated
recursive expansion, but method lookup can cost `Theta(R * F)` and final trait
implementation selection costs `Theta(I)`. Phase measurements show 216 ms in
selective discovery against 806 ms in cache work for the selective graph.
Recheck the closure scans after cumulative copies are removed. Do not optimize
them first.

With the recommended ownership model, module cache lookup and final assembly
cost `Theta(M + A + D)`, plus source parsing, dependency edges, and selective
closure work. Declarations have one working copy and one final linear prune.
Alias registration does not copy declarations. Peak declaration storage is
`Theta(D)` rather than the sum of cumulative stores.

## Required behavior

### Resolution and source identity

Path imports resolve relative to the importer or use the absolute path as
given. Module imports probe the importer directory, then each `-L` path in
order. The first source override or disk file wins. Disk hits return absolute
normalized paths. Virtual hits retain their normalized `SourceStore` key
([`compiler/syntax.casa` lines 5657-5721](https://github.com/frendsick/casa/blob/0694ddd71d941f5d188bde0543142cd8089cefeb/compiler/syntax.casa#L5657-L5721),
[`compiler/lexer.casa` lines 180-205](https://github.com/frendsick/casa/blob/0694ddd71d941f5d188bde0543142cd8089cefeb/compiler/lexer.casa#L180-L205)).

The canonical source identity must be the exact key returned by this resolver.
Do not use the module specifier, alias, basename, or filesystem real path.
Library-path order must run before cache lookup because it selects the source
identity.

### Cycles

Cycle tracking uses resolved paths, not cache keys, and runs before reuse. It
reports the complete active import chain. Keep a source-identity stack separate
from the successful-result cache
([`compiler/syntax.casa` lines 5811-5830](https://github.com/frendsick/casa/blob/0694ddd71d941f5d188bde0543142cd8089cefeb/compiler/syntax.casa#L5811-L5830),
[`tests/compiler/test_parser.casa` lines 949-976](https://github.com/frendsick/casa/blob/0694ddd71d941f5d188bde0543142cd8089cefeb/tests/compiler/test_parser.casa#L949-L976),
[ADR-0068](https://github.com/frendsick/casa/blob/0694ddd71d941f5d188bde0543142cd8089cefeb/docs/adr/0068-module-import-cycles-are-rejected.md)).

### Visibility and selective closure

Canonical facts must retain their defining source and public/private state.
Aliases only bind source names to canonical identities. A qualified import
exposes public names through that binding. A selective import first rejects a
private or absent root, then retains private dependencies without exposing
them. Imported dependencies are not re-exported. This follows ADR-0010 and the
documented module contract
([ADR-0010](https://github.com/frendsick/casa/blob/0694ddd71d941f5d188bde0543142cd8089cefeb/docs/adr/0010-namespaced-private-modules.md),
[`docs/modules.md` lines 34-95](https://github.com/frendsick/casa/blob/0694ddd71d941f5d188bde0543142cd8089cefeb/docs/modules.md#L34-L95)).

The selective-retention graph must exclude declarations inherited from the
importer and include declarations in the imported module's dependency graph.
Source ownership should replace the current `inherited_symbols` and
`inherited_impls` snapshots.

### Diagnostics

Imported diagnostics merge at the import encounter. A failed import makes the
parse-and-resolve output unusable, commits no result store, and stops later
import expansion and identifier resolution. Diagnostics are not sorted by file
or severity. A successful cache entry stores no diagnostics, so a cache hit
must not replay them
([`compiler/error.casa` lines 176-207](https://github.com/frendsick/casa/blob/0694ddd71d941f5d188bde0543142cd8089cefeb/compiler/error.casa#L176-L207),
[`tests/compiler/test_parser.casa` lines 1134-1328](https://github.com/frendsick/casa/blob/0694ddd71d941f5d188bde0543142cd8089cefeb/tests/compiler/test_parser.casa#L1134-L1328)).

The analyzer must keep depth-first source-order traversal and merge diagnostics
when the import is first encountered. Cycle errors remain encounter-local.

### Immutable globals

The same source imported through multiple aliases must initialize each physical
global once. Current storage identity is absolute normalized source path plus
source offset. Initializer ordering deduplicates that identity
([`compiler/syntax.casa` lines 6380-6413](https://github.com/frendsick/casa/blob/0694ddd71d941f5d188bde0543142cd8089cefeb/compiler/syntax.casa#L6380-L6413),
[`compiler/syntax.casa` lines 7681-7716](https://github.com/frendsick/casa/blob/0694ddd71d941f5d188bde0543142cd8089cefeb/compiler/syntax.casa#L7681-L7716),
[`tests/compiler/test_parser.casa` lines 978-1025](https://github.com/frendsick/casa/blob/0694ddd71d941f5d188bde0543142cd8089cefeb/tests/compiler/test_parser.casa#L978-L1025)).

### LSP sources and invalidation

The LSP builds a fresh `AnalysisInput` for each document compile and copies all
other open document sources into `source_overrides`. `lex_file` reads a stored
source before disk. A changed unsaved document therefore changes the next
analysis without a persistent cache invalidation protocol
([`lsp.casa` lines 298-313](https://github.com/frendsick/casa/blob/0694ddd71d941f5d188bde0543142cd8089cefeb/lsp.casa#L298-L313),
[`compiler/lexer.casa` lines 1123-1141](https://github.com/frendsick/casa/blob/0694ddd71d941f5d188bde0543142cd8089cefeb/compiler/lexer.casa#L1123-L1141),
[`tests/compiler/test_analysis.casa` lines 45-69](https://github.com/frendsick/casa/blob/0694ddd71d941f5d188bde0543142cd8089cefeb/tests/compiler/test_analysis.casa#L45-L69)).

Keep the canonical module cache per analysis call. Its invalidation inputs are
the root source, exact source overrides, reachable disk contents, ordered
library paths, and import specs. A new `AnalysisInput` creates a new cache.
Persistent cross-analysis caching is not needed for this issue.

## Deep module recommendation

Keep this interface:

```casa
pub fn parse_and_resolve
    tokens:std::List[common::Token]
    search_paths:std::List[std::String]
    diagnostics:error::Diagnostics
    sources:mut$lexer::SourceStore
-> ParseResolveResult
```

Callers already provide every varying input, and the result already contains
the usable operations, final store, and ordered diagnostics
([`compiler/syntax.casa` lines 7355-7375](https://github.com/frendsick/casa/blob/0694ddd71d941f5d188bde0543142cd8089cefeb/compiler/syntax.casa#L7355-L7375)).
Adding a public cache, resolver adapter, or per-phase symbol view would make the
interface shallower.

Its private implementation should own:

- One working `SymbolStore` for the complete analysis.
- A successful module record keyed only by resolved source identity. The record
  contains declaration keys, public keys, dependency source identities, and
  initializer operation indexes. It does not own another store.
- A source-identity stack for cycle diagnostics.
- Alias bindings from source spelling to the canonical module prefix.
- Full-retention markers and selective requests, including encounter locations,
  by source identity.

Use a canonical internal prefix derived from source identity, not an import
alias. Both aliases then rewrite to the same internal symbols. Parse each
source once into the working store. Cache hits add alias and retention facts but
do not clone or merge declarations.

After import discovery succeeds, compute selective closures in import encounter
order and union their retained keys. Keep each request location for diagnostics.
Repeated requests can reuse the closure for the same canonical roots. Then
consume or remove unretained facts once. The returned
`SymbolStore` is the only mutable declaration store that crosses into
typechecking. The private cache contains keys only and is dropped. This avoids
shared mutable maps between a cache and `TypecheckResult`, which matters because
`SymbolStore` maps share backing storage when the struct is passed by value
([`compiler/common.casa` lines 3954-3986](https://github.com/frendsick/casa/blob/0694ddd71d941f5d188bde0543142cd8089cefeb/compiler/common.casa#L3954-L3986)).

A failed import can discard the complete working store because the current
pass contract already returns no usable output. Keep selective conflict checks
before the final prune so a conflict cannot partially change a successful
result.

## Migration slices

The measurements in this report complete the investigation slice. Implement
the recommendation in these separate changes:

1. **Use `SymbolStore` ownership.** Land #550 and #546 first. Add the private
   module analyzer through `SymbolStore` behavior, not direct map edits or a new
   store adapter.
2. **Separate identity from aliases.** Resolve imports before namespace
   rewriting, assign one canonical source prefix, key module records by source
   identity, and keep cycle state path-based. Preserve the existing
   `parse_and_resolve` interface.
3. **Remove cumulative copies.** Parse canonical modules into one working store.
   Change selective discovery to return retained declaration keys. Preserve
   encounter locations while unioning those keys. Replace
   `ImportedModule.store`, both store clone forms used by imports,
   `retain_file_declarations`, and `merge_import` with one final prune.
4. **Validate behavior and measure again.** Run focused import, diagnostic,
   source-override, LSP, compiler, example, bootstrap, and fixed-point checks.
   Repeat the same benchmark commands and report medians, spread, clone counts,
   declaration counts, wall time, and peak RSS.

Do not fold #545's repeated identifier-resolution change or #551's
monomorphization body transfer into these slices. The module analyzer should
consume resolved function bodies when #545 lands and should use the function
lifecycle from #546.

## Measurements

### Environment and build

- Linux 6.18.33.2 under WSL2.
- AMD Ryzen 7 3700X, 8 cores and 16 logical CPUs.
- 15 GiB memory and 4 GiB swap. Swap stayed unused.
- Stable Casa v1.41.0 built the first compiler.
- The measured compiler was a three-stage fixed point. Stage 2 and stage 3
  assembly matched.

Run the build from revision `0694ddd`:

```sh
benchmark_dir=$(mktemp -d /tmp/casa-553.XXXXXX)
release_dir=$(mktemp -d /tmp/casa-553-release.XXXXXX)
gh release download v1.41.0 --pattern casac --dir "$release_dir"
chmod u+x "$release_dir/casac"

"$release_dir/casac" -L lib casa.casa \
    -o "$benchmark_dir/casac-stage1" --keep-asm
"$benchmark_dir/casac-stage1" -L lib casa.casa \
    -o "$benchmark_dir/casac-stage2" --keep-asm
"$benchmark_dir/casac-stage2" -L lib casa.casa \
    -o "$benchmark_dir/casac-stage3" --keep-asm
diff -q "$benchmark_dir/casac-stage2.s" "$benchmark_dir/casac-stage3.s"
```

### Representative import graphs

Both generated graphs use one 1,204-function module. It has four public roots.
Each root reaches a separate chain of 300 private functions. Two wrapper
modules import the large module under the same `shared` alias, which creates a
diamond. Each root also imports the large module through three direct aliases.

The qualified root repeats one exact full import. The selective root requests
two closures from one alias, then requests one closure from each of two more
aliases. This covers a cache hit and namespace-keyed cache misses without
changing visibility or requesting the same selective source name twice.

Generate the corpus:

```sh
corpus_dir="$benchmark_dir/corpus"
mkdir "$corpus_dir"
module="$corpus_dir/module.casa"
: > "$module"
for prefix in a b c d; do
    printf 'pub fn value_%s -> i64 { private_%s_0 }\n' \
        "$prefix" "$prefix" >> "$module"
    for index in $(seq 0 298); do
        next=$((index + 1))
        printf 'fn private_%s_%s -> i64 { private_%s_%s }\n' \
            "$prefix" "$index" "$prefix" "$next" >> "$module"
    done
    printf 'fn private_%s_299 -> i64 { 1 }\n' "$prefix" >> "$module"
done

cat > "$corpus_dir/left.casa" <<'EOF'
import "module.casa" as shared
pub fn left_value -> i64 { shared::value_a }
EOF

cat > "$corpus_dir/right.casa" <<'EOF'
import "module.casa" as shared
pub fn right_value -> i64 { shared::value_b }
EOF

cat > "$corpus_dir/qualified.casa" <<'EOF'
import "left.casa" as left
import "right.casa" as right
import "module.casa" as module_a
import "module.casa" as module_b
import "module.casa" as module_a
import "module.casa" as module_c
left::left_value drop
right::right_value drop
module_a::value_a drop
module_a::value_b drop
module_b::value_b drop
module_c::value_d drop
EOF

cat > "$corpus_dir/selective.casa" <<'EOF'
import "left.casa" as left { left_value }
import "right.casa" as right { right_value }
import "module.casa" as module_a { value_a }
import "module.casa" as module_a { value_b }
import "module.casa" as module_b { value_c }
import "module.casa" as module_c { value_d }
left::left_value drop
right::right_value drop
module_a::value_a drop
module_a::value_b drop
module_b::value_c drop
module_c::value_d drop
EOF
```

### Wall time and peak RSS

Each corpus had one warm-up. Self-compilation used three measured runs. The
shorter graph corpora used five alternating measured runs. GNU `time` reported
wall time and maximum resident set size:

```sh
compiler="$benchmark_dir/casac-stage3"
"$compiler" -L lib casa.casa -o "$benchmark_dir/warm-self"
"$compiler" -L lib "$corpus_dir/qualified.casa" \
    -o "$benchmark_dir/warm-qualified"
"$compiler" -L lib "$corpus_dir/selective.casa" \
    -o "$benchmark_dir/warm-selective"

: > "$benchmark_dir/self.times"
for iteration in 1 2 3; do
    /usr/bin/time -f '%e %M' -a -o "$benchmark_dir/self.times" \
        "$compiler" -L lib casa.casa -o "$benchmark_dir/measured-self"
done

: > "$benchmark_dir/qualified.times"
: > "$benchmark_dir/selective.times"
for iteration in 1 2 3 4 5; do
    /usr/bin/time -f '%e %M' -a -o "$benchmark_dir/qualified.times" \
        "$compiler" -L lib "$corpus_dir/qualified.casa" \
        -o "$benchmark_dir/measured-qualified"
    /usr/bin/time -f '%e %M' -a -o "$benchmark_dir/selective.times" \
        "$compiler" -L lib "$corpus_dir/selective.casa" \
        -o "$benchmark_dir/measured-selective"
done
```

RSS is the process maximum in KiB. Spread is maximum wall time minus minimum
wall time.

| Corpus | Median wall | Spread | Median peak RSS | Raw wall samples | Raw RSS samples |
|---|---:|---:|---:|---|---|
| Self-compilation | 27.68 s | 0.96 s | 747,708 KiB, 730.18 MiB | 28.54, 27.68, 27.58 | 747,836, 747,580, 747,708 |
| Qualified graph | 2.58 s | 0.19 s | 86,972 KiB, 84.93 MiB | 2.63, 2.44, 2.58, 2.55, 2.61 | 86,972, 86,972, 86,972, 86,972, 86,972 |
| Selective graph | 2.72 s | 0.19 s | 78,908 KiB, 77.06 MiB | 2.70, 2.69, 2.74, 2.88, 2.72 | 78,908, 78,908, 78,908, 78,908, 78,908 |

The allocator exposes no live-allocation or reusable-byte counters. Peak RSS
therefore measures the process high-water mark, not live declaration bytes.
The clone profile below supplies logical declaration sizes. No claim about
retained allocator bytes is possible from this instrumentation.

### Phase profile

A temporary phase build used `timer::Timer.elapsed_ns` around these existing
operations:

- Root `lex_source`, namespace and `parse_ops`, `finish_resolution`,
  `type_check`, and complete `analysis::analyze`.
- Imported `lex_file`, namespace and `parse_ops`, and `finish_resolution`.
- Importer-store clone, cache-hit clone, cache-store clone, and `merge_import`.
- `compute_selective_import_closure_with_inherited` and
  `merge_selective_import_result`.

Each point printed one `PROFILE <label> <nanoseconds>` line. The instrumentation
was removed after building `casac-profile`. Three clean-source runs supplied
each median. Imported resolution timers are inclusive of nested import
resolution, so that row is not additive. Root resolution is the inclusive
parse-and-resolve value. Cache work is the sum of importer-store cloning and
cache-hit and cache-store work. Symbol merging includes full and selective
merges.

| Phase, median milliseconds | Self | Qualified | Selective |
|---|---:|---:|---:|
| Complete analysis | 28,712.429 | 2,665.608 | 2,716.689 |
| Root lexing | 2.942 | 0.274 | 0.313 |
| Root parsing | 4.116 | 0.464 | 0.507 |
| Root resolution, inclusive | 9,354.070 | 2,148.754 | 2,482.482 |
| Imported lexing, 19 / 6 / 6 files | 1,171.297 | 180.803 | 179.349 |
| Imported parsing, 19 / 6 / 6 files | 1,907.925 | 485.829 | 442.482 |
| Imported resolution, inclusive | 4,076.875 | 667.157 | 668.898 |
| Typechecking | 19,282.439 | 454.604 | 218.602 |
| Cache work | 3,231.698 | 781.000 | 805.932 |
| Selective dependency discovery | 0 | 0 | 216.358 |
| Symbol merging | 595.291 | 209.617 | 62.410 |

The complete-analysis range was 27,919 to 29,055 ms for self-compilation,
2,348 to 2,683 ms for the qualified graph, and 2,710 to 2,818 ms for the
selective graph. The logging build is slower than the uninstrumented wall-time
build, so phase values show attribution rather than replacement wall times.

### Clone counts and sizes

A second temporary build added these lines at the start of `SymbolStore.clone`
and `SymbolStore.clone_for_selective_import`, respectively:

```casa
f"CLONE declaration {self.functions.length} {self.structs.length} {self.enums.length} {self.traits.length} {self.trait_impls.length} {self.constants.length} {self.variables.length}" std::eprintln_string
f"CLONE import {self.functions.length} {self.structs.length} {self.enums.length} {self.traits.length} {self.trait_impls.length} {self.constants.length} {self.variables.length}" std::eprintln_string
```

`declaration` is the importer-store clone. `import` is the cache and selective
import clone. Counts are logical declarations at the clone call, not allocated
bytes.

| Corpus | Total clones | Importer clones | Import/cache clones | Largest clone: functions | Structs | Enums | Traits | Implementations |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| Self-compilation | 42 | 19 | 23 | 3,412 | 115 | 32 | 10 | 149 |
| Qualified graph | 13 | 6 | 7 | 4,818 | 0 | 0 | 0 | 0 |
| Selective graph | 19 | 6 | 13 | 2,711 | 0 | 0 | 0 | 0 |

The qualified graph's importer clones grew through 0, 1, 1,205, 1,206, 2,410,
and 3,614 functions. Its cache clones reached 4,818 functions. The selective
graph made two cache copies on each selective miss and one on its hit. Its
importer clones grew through 0, 1, 302, 604, 1,206, and 1,507 functions. This
growth matches `Theta(sum(C_i))` and confirms that cache identity by namespace
and path repeats source analysis for aliases.

## Related open issues

- [#545](https://github.com/frendsick/casa/issues/545#issuecomment-5470180715) owns single identifier
  resolution for source function bodies. Module records must keep those resolved
  bodies and must not introduce another resolver lifecycle.
- [#546](https://github.com/frendsick/casa/issues/546#issuecomment-5470180809) owns function declaration
  lookup and lifecycle. The module analyzer should consume that interface. It
  must not add a competing store view or body-transfer protocol.
- [#549](https://github.com/frendsick/casa/issues/549#issuecomment-5470180910) keeps behavior-specific
  clone operations explicit while replacing mechanical clones. The import store
  clones remain behavior-specific until this work removes them.
- [#550](https://github.com/frendsick/casa/issues/550#issuecomment-5470180985) removes inert function
  metadata. It is independent and reduces the clone and lifecycle surface before
  #546.
- [#551](https://github.com/frendsick/casa/issues/551#issuecomment-5470181068) removes a separate
  monomorphization body clone after #546. Its measurements must not be credited
  to the module-analysis change.

The linked comments record these implications on each issue.
