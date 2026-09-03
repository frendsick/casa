# Compiler complexity baseline

Status: complete synthesis for
[Synthesize the compiler complexity baseline](https://github.com/frendsick/casa/issues/643).

Source revision:
[`1e89fb5`](https://github.com/frendsick/casa/commit/1e89fb524248ab4cb7bfc0e750e1213638a900b2),
2026-09-03.

Run date: 2026-09-03.

## Result

Casa's main compiler maintenance cost comes from two shared implementation
choices. One mutable `Op` tree represents parsed, resolved, checked, and
backend-ready programs. One mutable `SymbolStore` owns declarations together
with facts produced by later phases. Phase and identity invariants therefore
live in scheduling, cloning, side tables, flags, and caller knowledge instead
of phase-valid types.

That model spreads one language decision across several modules. Import and
literal syntax are interpreted more than once. Semantic operation facts are
collected and then queried again by handlers. The backend repeats semantic,
storage, and ABI decisions after typechecking. Document queries classify the
same operation variants again. The persistent bytecode list then feeds another
wide dispatch in the emitter.

The outer compiler interfaces are smaller than their implementations.
`analysis::analyze`, `syntax::parse_and_resolve`, and
`syntax::analyze_syntax` are useful deep modules. Their result types and the
interfaces below them expose mutable phase state. Tests make much of that
state a practical interface.

The four audits identify a non-overlapping, behavior-preserving deletion floor
of about 820 to 850 production lines and 350 to 500 test lines. Another 500
compiler lines can move to a fixed runtime asset. Larger estimates overlap
because they all replace the same operation, store, and phase protocols. They
must not be added into one headline target before an architecture prototype
shows the replacement.

No target architecture is selected here. This baseline defines the present
flow, costs, constraints, and comparison measures for the architecture work.

## Evidence and freshness

This synthesis reconciles these completed audits:

- [Front-end complexity audit](../audits/front-end-complexity.md)
- [Semantic-analysis complexity audit](semantic-analysis-complexity.md)
- [Backend and runtime complexity audit](backend-runtime-complexity-audit.md)
- [Shared compiler state and tooling coupling audit](../audits/shared-compiler-state-and-tooling.md)

All four audits inspected source revision `bb6ffa7`. The commits from that
revision through `1e89fb5` add only the four audit documents. A path-limited
diff has no changes in `casa.casa`, `compiler/`, `lsp.casa`, `formatter/`,
`tests/`, or `lib/`. Their static findings therefore describe the source used
for this synthesis.

The only open pull request at the run date is
[Add practical raylib demo](https://github.com/frendsick/casa/pull/634). It
changes the release environment, example files, and example-test support. It
does not change a compiler, LSP, formatter, or compiler-test path.

Line counts use physical source lines. Interface and reference counts use the
methods from the linked audits. Each audit checked production callers as well
as declarations, because `pub` alone does not prove that a name is part of a
useful interface.

## Present compiler flow

```text
CLI or LSP source
    |
    v
analysis::analyze
    |
    +-> SourceStore and lexer -> tokens and lexical diagnostics
    |
    +-> syntax::parse_and_resolve
    |       |
    |       +-> Parser, module graph, declaration elaboration
    |       +-> selective-import semantic sessions
    |       +-> resolved Op list and SymbolStore
    |
    +-> typechecker::type_check
            |
            +-> SemanticSession and mutable Op rewrites
            +-> checked functions, ownership side table, specialization
            +-> TypecheckResult
                    |
                    +-> LSP AnalyzedDocument and document queries
                    |
                    +-> bytecode::compile_typechecked
                            |
                            +-> Program and InstValue list
                                    |
                                    +-> emitter::emit -> assembly text
                                            |
                                            +-> build::compile_binary -> executable

Formatter source
    |
    +-> syntax::analyze_syntax -> tokens, comments, and syntax spans
            |
            +-> format -> analyze candidate again -> compare syntax facts
```

The main function contracts show where caller knowledge leaks through otherwise
small interfaces:

| Interface | Explicit contract | Additional caller knowledge |
|---|---|---|
| [`analysis::analyze`](../../compiler/analysis.casa#L49-L75) | Takes root source, root identity, search paths, and overrides. Returns sources, diagnostics, and optional typechecked output. | Callers must inspect diagnostics before treating `typechecked` as successful. The nested result also retains a diagnostic copy. |
| [`syntax::parse_and_resolve`](../../compiler/syntax.casa#L8495-L8518) | Loads imports into the supplied `SourceStore`, then returns resolved operations, a symbol store, and diagnostics. | A missing operation list means the store is unusable. The type permits other combinations, and callers must retain the mutated source store. |
| [`typechecker::type_check`](../../compiler/typechecker.casa#L53-L75) | Consumes operations and a store. Returns checked operations, updated symbols, a root stack effect, and diagnostics. | The result may contain errors for editor use. Code generation requires the same result to contain only backend-valid operations. |
| [`bytecode::compile_typechecked`](../../compiler/bytecode.casa#L3713-L3721) | Returns a complete `Program` or an internal failure. | It rechecks diagnostics, then depends on the exact pairing between operations and store-owned side tables. |
| [`emitter::emit`](../../compiler/emitter.casa#L2647-L2654) | Converts a `Program` to GNU assembly text. | Public `Program` fields and instruction-family helpers allow callers and tests to construct states outside this contract. |
| [`build::compile_binary`](../../compiler/build.casa#L8-L70) | Writes assembly, assembles, links, and applies the keep-assembly option. | The CLI sequences this separately from emission. The function exits the process after write, assembly, or link failures. |

## Source and interface baseline

The compiler contains 40,192 lines. The CLI, LSP, and formatter add 3,920
lines that consume compiler interfaces.

| Source cluster | Files | Lines | Share of compiler source |
|---|---|---:|---:|
| Shared models, diagnostics, analysis, and document queries | `common`, `error`, `analysis`, `document` | 6,554 | 16.3% |
| Front end | `lexer`, `syntax`, `selective_import` | 12,003 | 29.9% |
| Semantic analysis | `block_scope`, `pattern`, `semantic_rules`, `semantics`, `typechecker` | 15,034 | 37.4% |
| Backend and native build | `abi`, `bytecode`, `emitter`, `build` | 6,601 | 16.4% |
| **Compiler total** | | **40,192** | **100%** |
| CLI, LSP, and formatter | `casa.casa`, `lsp.casa`, `formatter/format.casa` | 3,920 | |
| **Compiler and direct consumers** | | **44,112** | |

The grouping describes current responsibility. It does not propose future
file ownership. For example, `common.casa` contains data from every phase, and
`pattern.casa` is used by semantic analysis and bytecode lowering.

The current interface surface is much wider than production use:

| Module or type | Declared or exposed surface | Verified external use |
|---|---:|---:|
| `common.casa` | 292 public declarations | Imported by 16 production files across every phase |
| Front-end files | 346 public functions | 13 of 221 top-level functions have production callers outside their file |
| `semantics.casa` | 273 public functions | 21 functions have direct cross-file references. Up to 252 names can become private after a dead-declaration check |
| `semantic_rules.casa` | 63 public functions | 38 distinct member names are referenced outside the file |
| `emitter.casa` | 61 public functions | Production outside the file calls only `emit`. At least 52 functions have no outside caller |
| `AnalyzedDocument` | 8 public fields and 44 public methods | The LSP calls 6 methods and directly needs 3 fields. At least 31 methods have no outside caller |
| `Parser` | 13 public fields | Production enters through `parse_and_resolve` or `analyze_syntax`. Tests construct the parser directly |
| `SymbolStore` | 21 fields, 16 public, with 33 public methods across files | At least 343 direct production field accesses and 66 test accesses |
| `Program` | 7 public fields | One production producer and one production consumer |

Closing visibility removes little source by itself. It reduces the interface a
maintainer and a test must understand. The replacement architecture should
measure interface names, public fields, required ordering rules, and failure
modes, not only lines.

## State ownership and representation transitions

| Representation | Current ownership and transition | Invalid state or manual protocol |
|---|---|---|
| `Token` | The lexer creates source spellings. The namespace prepass rewrites identifier values into internal names. | One field represents two name domains. `SymbolStore.source_names` must preserve the reverse mapping. |
| `Parser` and `ModuleContext` | Parsing owns the working store, import cache, source prefixes, module records, initializer indexes, diagnostics, and formatter mode. | Parallel collections and flags can disagree. A declaration error after store mutation makes all output unusable. |
| `Op` and `OpValue` | Parsing creates operations. Resolution and semantic analysis replace values, add hints and bindings, and assign IDs. | One 128-variant enum admits unresolved, resolved, checked, and lowered forms in every phase. Flags and contents can disagree. |
| `Function` | Syntax stores bodies and phase booleans. Semantic analysis takes, clones, rewrites, specializes, and restores bodies. | `is_resolved` and `is_typechecked` can disagree with the body. Six production paths rely on paired take and restore calls. |
| `SymbolStore` | Syntax creates declarations. Semantic analysis clones or owns the store and later commits selected fields. Bytecode reads declarations and operation metadata. | Heap-backed values alias on shallow copy. Clone and commit field lists differ. An operation list and an unrelated store have valid types but are not a valid program. |
| `Op.id` plus `op_ownership` | Semantic analysis allocates an ID and stores ownership events under it. Specialization can allocate another ID and copy events. | A nonzero ID can lack facts. A cloned ID can refer to stale facts. The backend depends on exact identity preservation. |
| `TypecheckResult` | Typechecking returns checked data and diagnostics for both code generation and editor use. | A result with errors is useful to the editor but invalid for bytecode. The type does not distinguish those products. |
| `Program` and `InstValue` | Bytecode creates a public program with a 136-variant instruction list. The emitter consumes it. | Tests can build mismatched fields. Family helpers accept the full enum and terminate when given another family. |

These are eight verified invalid-state families. They are not a count of every
constructible invalid value. Public structs make that number combinatorial.
Architecture prototypes should show which families become unrepresentable and
which remain private assertions.

## Dependency direction

The effective production dependency direction is:

```text
analysis -> lexer + syntax + typechecker
syntax -> lexer + selective_import + semantics + semantic_rules
selective_import -> semantics + semantic_rules
typechecker -> semantics
semantics -> semantic_rules + abi + block_scope + pattern
bytecode -> typechecker + semantic_rules + abi + pattern
emitter -> common

common <- lexer, syntax, selective_import, semantics, semantic_rules,
          typechecker, bytecode, abi, emitter, document, CLI, LSP, formatter

LSP -> analysis + document + lexer + common
document -> analysis + lexer + common
formatter -> lexer + syntax + common
```

Four dependency facts drive maintenance cost:

1. Syntax depends on semantic analysis for selective dependency discovery and
   global-initializer validation before the complete semantic pass.
2. Bytecode depends on `semantic_rules` and `typechecker` to recover decisions
   that successful typechecking did not carry forward.
3. `common` owns lexical, semantic, declaration, layout, ABI, and machine data.
   Its fan-out is necessary, but its mixed ownership is not.
4. Document queries depend on raw checked operations and declarations. They
   classify 96 of 128 `OpValue` variants and deep-clone function bodies during
   queries.

A target design should make facts flow with the phase product that established
them. It should not add wrapper interfaces around one shared mutable store.
One adapter with one implementation would add a shallow module.

## Repeated interpretation

| Decision or traversal | Current repeats | Maintenance effect |
|---|---|---|
| Imports and declaration names | Prefix collection, namespace discovery, and normal parsing each read overlapping grammar. The namespace pass mutates tokens. | Grammar changes can require coordinated edits and source-name synchronization. |
| Literals | Five syntax paths classify or convert bool, char, integer, float, and string spelling. | A new literal rule must remain consistent across expressions, constants, patterns, and final resolution. |
| Operation semantics | `collect_operation_facts` calls `analyze_operation`, then operation handlers call it again for several families. | Dependencies, effects, dispatch, and stack transitions can diverge or repeat lookup work. |
| Nested operations | At least eight semantic walkers recurse through arrays, expression groups, match arms, and global initializers independently. | Each new nested form must update several local dispatchers. |
| Trait defaults and specialization | Default bodies become synthetic functions, are qualified, cloned, checked, and specialized. | Identity, bindings, ownership facts, and operation IDs need manual transfer. |
| Storage and ABI | Semantics, bytecode, and the emitter each query or complete type layout and native-call placement decisions. | Later phases can reject or reinterpret facts that an earlier phase already checked. |
| Machine instruction selection | The emitter routes 123 of 136 `InstValue` variants into helpers that match the same enum again. | One producer and one consumer still share a large two-stage dispatch protocol. |
| Editor meaning | Semantic-token and hover queries independently match 94 and 64 operation variants. | Tooling must change when compiler-private operation representation changes. |

The target is not one universal traversal abstraction. Each owning module can
keep private traversals. The external interface should carry final facts so
callers do not reconstruct the decision.

## Test seams

Casa has 26,321 lines of Casa test source and 1,202 lines of shell test code.
The tests provide broad behavior coverage. A material part also fixes current
representation details:

| Coupling | Current evidence |
|---|---:|
| Test files that name `OpValue` | 17 |
| Test files that name `InstValue` | 8 |
| Direct `common::Op::new` calls | 186 |
| Direct parser constructions in `test_parser.casa` | 11 |
| Positional `Program` occurrences in `test_emitter.casa` | about 80 |
| `test_selective_import_closure.casa` | 718 lines |
| `test_semantics_operations.casa` | 2,135 lines and 117 tests |
| `test_bytecode.casa` plus `test_emitter.casa` | 2,852 lines and 129 tests |

The source-to-binary tests, error fixtures, formatter command tests, and
production `analyze` or `parse_and_resolve` tests are stable behavior seams.
Exact operation lists, exact instruction positions, public parser fields, and
field-wise store transfers are implementation seams. When a deeper interface
replaces one of them, tests should move to that interface and the replaced
tests should be deleted. A second test layer for the new internal
representation would retain both costs.

## Measured build cost

The stable Casa v1.50.0 compiler built `casa.casa` at source revision
`1e89fb5` on Linux 6.18.33.2 under WSL2. The system has an AMD Ryzen 7 3700X,
8 cores, and 16 logical CPUs. One unmeasured warm-up preceded three measured
builds. Each build kept the generated assembly so its size could be measured.

| Run | Wall time | Peak RSS |
|---:|---:|---:|
| 1 | 33.67 s | 760,308 KiB |
| 2 | 33.11 s | 760,308 KiB |
| 3 | 32.95 s | 760,436 KiB |
| **Median** | **33.11 s** | **760,308 KiB** |

The generated assembly has 1,265,225 lines and 26,529,317 bytes. The linked
compiler has 4,777,120 bytes.

The separate backend audit recorded one verbose 32.390-second build at the
same source state. Analysis completed at 24.819 seconds. Bytecode took another
2.985 seconds, assembly emission 2.007 seconds, and native assembly plus linking
2.579 seconds. That sample places about 77% of wall time before bytecode and
23% after it. It does not attribute cost within analysis.

The front-end, semantic, and backend audits used slightly different warm-up
and `--keep-asm` protocols. Their medians range from 30.64 to 31.90 seconds, while
peak RSS remains between 760,308 and 760,564 KiB. This synthesis uses the fresh
three-run protocol above as the comparison baseline. Later measurements must
use the same compiler, command, warm-up, output mode, and machine before they
claim a gain.

## Required and accidental complexity

The current architecture must preserve accepted language and tooling behavior
unless one of the linked behavior decisions changes it.

| Area | Required behavior at this baseline | Accidental implementation cost |
|---|---|---|
| Source and syntax | UTF-8 locations, Reverse Polish syntax, recovery, ordered diagnostics, modules, visibility, and formatter facts | Repeated grammar scans, token identity mutation, parser-wide mode flags, and shared declaration mutation |
| Semantic analysis | Typed stacks, contextual inference, control-flow joins, static traits, generics, ownership, and diagnostics | Phase-polymorphic operations, encoded origins, side tables, repeated facts, cloned bodies, and mixed orchestration |
| Backend | Direct x86-64 output, deterministic storage and destruction, System V extern calls, and runtime failures | Backend semantic queries, incomplete storage and call plans, frontend control-flow validation, and duplicate instruction dispatch |
| Tooling | Partial editor results, stable diagnostic data, lossless formatter input, and syntax-preserving output | Raw operation scans, deep declaration clones, public compiler fields, and three adapters decoding variant conventions |
| Tests | Observable diagnostics, execution, ABI behavior, formatting safety, and fixed-point compilation | Direct construction and assertion of parser, operation, store, program, and instruction internals |

## Reconciled simplification ledger

The audit estimates use different boundaries. This ledger counts a line once.
It separates confirmed local work from changes that depend on the selected
architecture.

### Behavior-preserving floor

| Change | Production lines | Test lines | Evidence |
|---|---:|---:|---|
| Delete obsolete selective-import result and merge machinery | 350 to 380 | 200 to 350 | Production drops the public-surface result and never calls the old merge interface |
| Use one exhaustive instruction-selection path, delete dead static-struct emission, share emitter test fixtures, use one compiler-driver command, and consolidate small machine routines | 350 | 150 | Backend audit's non-overlapping model-independent ledger |
| Replace `resolve_return_type_t` with `Type::substitute_t` | 57 | 0 | The common operation already implements the required recursion |
| Remove typechecker forwarding helpers | 40 | 0 | They delegate to the semantic implementation for tests and bytecode callers |
| Remove `Op.deferred_return_hint` | 17 | 0 | No production construction path seeds a value. Only clone coverage keeps it live |
| Remove six unused CLI and LSP imports | 6 | 0 | No qualified use exists |
| **Non-overlapping floor** | **820 to 850** | **350 to 500** | No behavior or dependency change |

Moving the fixed 500-line runtime emitter block to a checked-in assembly asset
improves locality but does not reduce repository lines. No audit found a major
dependency to remove.

### Architecture-dependent opportunity

| Area | Supported opportunity | Why it cannot be added to the floor |
|---|---|---|
| Front-end internals | 680 to 910 production lines in the full audit, plus 2,050 to 2,300 lines relocatable behind deeper interfaces | The full range includes the selective-import floor and overlapping name, literal, and formatter changes |
| Semantic analysis | 700 to 1,200 production lines and 400 to 700 test lines | Checked products, typed origins, common traversals, callable summaries, control joins, and specialization replace the same shared state |
| Backend checked input and plans | About 350 to 450 additional production lines, plus about 150 relocatable ABI lines | The result depends on structured checked input, storage plans, call plans, and whether bytecode remains |
| Document queries | Up to 1,232 lines can move behind a compiler-produced editor product | The product and scans to delete remain undecided. This is not a credible net deletion estimate |
| Diagnostics and formatter facts | About 100 diagnostic lines relocatable and 80 to 140 formatter-mode lines exposed to deletion | The tooling product decision determines the final interfaces |

The ranked `ponytail-audit` result is:

1. `shrink:` replace the cross-phase operation and store protocol with a
   phase-valid checked product. This has the largest leverage across semantic,
   backend, and tooling work.
2. `shrink:` deepen front-end ownership behind the existing parse-and-resolve
   and syntax-analysis interfaces. Remove token identity rewriting and keep
   declaration elaboration private.
3. `shrink:` compute operation semantics once and keep nested traversal,
   origins, control joins, callable summaries, and specialization inside the
   semantic module.
4. `shrink:` make storage, ownership, control flow, and native-call placement
   complete before assembly rendering. Keep or delete bytecode based on the
   prototype, not its current name.
5. `shrink:` produce explicit successful-codegen, partial-editor,
   presentation-neutral diagnostic, and lossless-formatting products instead
   of exposing phase-private state.
6. `delete:` take the 820 to 850 production-line and 350 to 500 test-line floor
   before preserving those paths in a target design.
7. `yagni:` close hundreds of unused public names and replace
   representation-coupled tests when the owning interfaces exist.
8. `native:` place the fixed runtime behind a small assembly or object
   interface and let the compiler driver assemble and link.

`net: -820 to -850 production lines, -350 to -500 test lines, -0 dependencies possible before target-architecture gains.`

## Separate behavior decisions

The audits expose seven behavior changes with enough evidence to state as
decisions. None is assumed by the deletion floor or architecture comparison.
Each ticket is a child of the architecture map and blocks the final target
choice, but it does not block the three architecture prototypes.

| Decision | Implicated current surface | Baseline rule |
|---|---|---|
| [Choose the selective-import contract](https://github.com/frendsick/casa/issues/656) | 929-line module, early semantic discovery, import cache, initializer selection, and 718-line focused test | Preserve selective imports until decided |
| [Choose the compile-time evaluation surface](https://github.com/frendsick/casa/issues/657) | About 540 syntax-owned evaluator and validation lines, plus constant type parameters | Preserve `const fn` and constant blocks until decided |
| [Choose generated trait implementation behavior](https://github.com/frendsick/casa/issues/658) | About 1,200 syntax lines plus semantic and specialization work | Preserve all current `derives` behavior until decided |
| [Choose trait default method behavior](https://github.com/frendsick/casa/issues/659) | Synthetic default functions, call qualification, generic substitution, inheritance, and specialization | Preserve direct and inherited defaults until decided |
| [Choose Casa's ownership guarantees](https://github.com/frendsick/casa/issues/660) | 41-field checker and at least 1,350 core ownership, loan, and cleanup lines | Preserve all current safety and destruction guarantees until decided |
| [Choose the generic checking and specialization contract](https://github.com/frendsick/casa/issues/661) | Generic validation, operation cloning, metadata transfer, and 579-line validation and monomorphization tail | Preserve one body check followed by specialization until decided |
| [Choose the immutable-global initialization model](https://github.com/frendsick/casa/issues/662) | About 480 module registration, selection, ordering, and validation lines plus later-phase handling | Preserve eager, exactly-once runtime initialization until decided |

These tickets revisit accepted documentation and ADRs. The parent map permits
that review. Closing an implementation-detail ticket cannot supersede a
language decision.

[Choose compiler products for editor and formatter tooling](https://github.com/frendsick/casa/issues/650)
already owns partial editor results, diagnostic products, and lossless
formatter facts. A second behavior ticket would duplicate that question.

[Choose compiler simplification targets](https://github.com/frendsick/casa/issues/663)
now owns the quantified target decision. It waits for the architecture
prototypes so its thresholds can be credible, then blocks the target
architecture choice.

## Requirements for architecture comparison

Each architecture prototype must use the same representative slice and report:

1. Compiler and test lines required for one scalar operation, aggregate copy,
   branch, direct function call, generic call, and extern call.
2. Public module names, public representation fields, ordering rules, error
   modes, and cross-module types at each seam.
3. Which of the eight invalid-state families remain constructible at an
   external interface.
4. The number of operation classifications and tree traversals needed to
   produce dependencies, stack effects, types, ownership, storage, ABI
   placement, machine operations, and editor facts.
5. Dependency direction, including every semantic, symbol-store, trait,
   type-formatting, and layout query after successful typechecking.
6. Measured cost for the representative slice where the prototype can run,
   with explicit limits and extrapolation risks where it cannot. Full
   self-compilation median wall time, peak RSS, generated assembly size, and
   executable size are later implementation gates for the selected design.
7. Behavior tests retained, representation tests retained, replacement tests,
   and tests deleted.
8. How every unresolved behavior ticket is isolated. A prototype must preserve
   current behavior or state exactly which result depends on a later decision.

A coherent architecture must keep direct parsing, self-hosting, direct x86-64
emission, and no major compiler dependency. It should keep the existing deep
outer interfaces when possible. It must replace old interfaces instead of
layering new representations and adapters over them.

## Reproduction

Check evidence freshness and source distribution:

```sh
git diff --name-status bb6ffa7..1e89fb5 -- \
    casa.casa compiler lsp.casa formatter tests lib
wc -l compiler/*.casa
wc -l casa.casa lsp.casa formatter/format.casa
find tests -type f -name '*.casa' -print0 | xargs -0 wc -l
wc -l tests/*.sh
```

Check interface and test coupling:

```sh
rg -c '^\s*pub (fn|struct|enum|const)\b' compiler/*.casa
rg -l 'OpValue::' tests/compiler --glob '*.casa' | wc -l
rg -l 'InstValue::' tests/compiler --glob '*.casa' | wc -l
rg -o 'common::Op::new' tests/compiler --glob '*.casa' | wc -l
rg -o '\bsyntax_only\b' compiler/syntax.casa | wc -l
rg -o 'clone_for_import' compiler/document.casa | wc -l
```

Measure self-compilation:

```sh
measure_dir=$(mktemp -d)
./casac --keep-asm -L lib casa.casa -o "$measure_dir/casac-warm"
for run in 1 2 3
do
    /usr/bin/time -f "$run %e %M" -a \
        -o "$measure_dir/measurements.txt" \
        ./casac --keep-asm -L lib casa.casa -o "$measure_dir/casac-$run"
done
wc -lc "$measure_dir/casac-3.s"
stat -c '%s %n' "$measure_dir/casac-3"
```
