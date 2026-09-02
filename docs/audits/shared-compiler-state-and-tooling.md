# Shared compiler state and tooling coupling audit

Status: complete audit.

Source revision: [`bb6ffa7`](https://github.com/frendsick/casa/commit/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1), 2026-09-02.

Run date: 2026-09-03.

## Finding

The largest shared-state risk is the combination of one phase-polymorphic
operation type and one broadly mutable declaration store. `Op` represents
parsed, resolved, checked, and backend-ready operations. `SymbolStore` holds
declarations plus analysis metadata that is keyed by `Op.id`. Parser and
semantic analysis rewrite the operations, semantic analysis allocates and
copies IDs, and bytecode lowering must read the matching side-table entries.
The types do not make those phase and identity invariants explicit.

Tooling then depends on these compiler representations. Document queries keep
checked operations and declarations, classify 96 of the 128 `OpValue`
variants, and deep-clone function bodies during queries. The formatter has a
better top-level seam in `analyze_syntax`, but that seam is implemented by 25
`syntax_only` references spread through the compiler parser. Diagnostics have
one data schema but three adapters decode it separately.

The existing `analysis::analyze` entry point is a useful deep module. It owns
source setup, lexing, parsing, resolution, and typechecking behind one call.
Its result remains shallow for consumers because it exposes partial
`TypecheckResult`, mutable source state, and raw diagnostics. Later design work
should keep the single analysis seam while choosing phase-valid products and
state ownership.

This audit does not choose a target architecture or implement any cleanup.

## Scope and method

The primary scope contains 6,708 lines in `compiler/common.casa`,
`compiler/error.casa`, `compiler/analysis.casa`, `compiler/document.casa`, and
`casa.casa`. The audit also traced their consumers through `compiler/syntax.casa`,
`compiler/semantics.casa`, `compiler/typechecker.casa`,
`compiler/bytecode.casa`, `compiler/emitter.casa`, `lsp.casa`,
`formatter/format.casa`, and focused compiler tests.

Counts below are static source counts at the recorded revision. “Production
file” means compiler, CLI, LSP, or formatter source. Benchmark programs and
tests are counted separately when relevant.

## Lifecycle and consumers

| Artifact | Created or changed by | Later consumers | Verified coupling |
| --- | --- | --- | --- |
| `Token` and syntax facts | Lexer and the parser's syntax-only path | Parser and formatter | Formatter reads token spelling, kind, location, and eight `SyntaxSpanKind` variants. |
| `Op` and `OpValue` | Parser creates and resolves them. Semantic analysis assigns IDs, rewrites values, and adds type and call metadata. | Semantic rules, bytecode lowering, document queries, and tests | `OpValue` has 128 variants. Seven production modules name variants. Parser, common helpers, and semantics contain 118 `Op::set_value` calls. |
| `SymbolStore` | Parser creates declarations. Semantic analysis updates checked and specialized declarations plus operation metadata. | Parser, semantic rules, ABI planning, bytecode lowering, document extraction, and tests | The store has 21 fields, of which 16 are public. Its implementation is spread across `common`, `syntax`, and `semantics`. |
| `AnalysisResult` | `analysis::analyze` merges lexer, parser, and typechecker outputs. | CLI diagnostic reporting, LSP diagnostic conversion, and `AnalyzedDocument` construction | The result exposes diagnostics, sources, and optional partial `TypecheckResult`. Typechecking diagnostics are cloned into the outer diagnostics while remaining in the nested result. |
| `Diagnostics` | Lexer, parser, and semantic analysis | CLI, formatter, LSP, import ordering, and tests | CLI, formatter, and LSP each destructure `Diagnostic`. Error rendering is implemented on `lexer::SourceStore`. |
| `Program` and `InstValue` | Bytecode lowering | Assembly emitter and tests | `Program` has seven public fields. `InstValue` has 136 variants, all named by the emitter dispatch. |
| `AnalyzedDocument` | Moves root operations and four declaration maps out of `TypecheckResult` | Six LSP query operations and tests | The type exposes eight fields and 44 public methods. The production LSP calls six methods and directly needs three fields. |

## Static baseline

### Shared module surface

[`compiler/common.casa`](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/common.casa)
is 4,276 lines and contains 292 public type, constant, and function
declarations. It combines these change domains:

- lexical categories and source locations.
- the 128-variant high-level operation representation.
- the 136-variant backend instruction representation and `Program`.
- stack effects, declarations, the type model, and type utilities.
- the declaration and analysis store.
- ABI-independent type and aggregate layout.

Sixteen production files import `common`. Two benchmark programs also import
it. The import fan-out reflects real shared types, but the file makes all of
those types appear to belong to one module. A file split alone would only move
complexity. Representation ownership must be decided first.

### Operation fan-out

| Consumer | `OpValue` references | Distinct variants |
| --- | ---: | ---: |
| Semantic analysis | 334 | 108 |
| Parser and resolver | 297 | 82 |
| Bytecode lowering | 207 | 92 |
| Document queries | 203 | 96 |
| Common helpers | 171 | 90 |
| Semantic rules | 142 | 85 |

The same [`Op`](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/common.casa#L479-L486)
holds `id`, source location, two optional type facts, call bindings, and
`OpValue`. Parser and common helpers contain 65 value or type-hint setter calls.
Semantic analysis contains another 96. `Identifier`, unresolved method calls,
checked calls, ownership cleanup, structured control markers, and concrete
numeric operations can therefore inhabit the same type.

Ownership facts do not live in the operation. Semantic analysis assigns
`Op.id` from `SymbolStore.next_op_id`, writes ownership facts into
`SymbolStore.op_ownership`, and bytecode lowering looks them up again by ID
([assignment](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/semantics.casa#L1444-L1447),
[backend lookup](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/bytecode.casa#L1116-L1127)).
Specialization must allocate new IDs and copy each ownership event to the new
key
([`specialize_op_metadata`](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/semantics.casa#L12294-L12334)).
Derived `Op` cloning copies the numeric ID, so each clone path must know whether
to preserve or replace that identity.

This is a cross-phase manual protocol. The backend requires a checked
operation list and the exact store that owns its side-table facts. A list and a
different store are valid parameter types but not a valid compilation input.

### Symbol store surface and protocols

[`SymbolStore`](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/common.casa#L3420-L3442)
has 21 fields. Sixteen fields are public. Its main implementation exposes 23
public methods. A parser-side extension exposes 10 more public methods, five
of which have no caller outside `compiler/syntax.casa`. Semantic analysis adds
the private `clone_for_semantics` implementation in a third file.

A conservative receiver-qualified scan found 229 direct accesses to public
store fields through identifiers named `store` or `*_store` outside `common`.
The parser's `SymbolStore` extension contains 50 more through `self`,
`imported`, and `cloned`. The manual semantic clone and commit functions each
contain another 32 source and target field references. These non-overlapping
sets give a lower bound of 343 production references. They do not include
direct accesses through other receiver names or implicit access inside store
methods. Tests add at least 66 accesses through `store` or `*_store`.

Two field-wise transfer protocols must track store growth:

- [`clone_for_semantics`](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/semantics.casa#L11896-L11926)
  creates a fresh store, copies each public field, rebuilds functions and trait
  implementations through methods, and separately copies source names.
- [`commit_semantic_store`](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/typechecker.casa#L28-L51)
  copies the fields back for the test-facing `check_functions` path. Its field
  list is different because it omits trait implementations and source names.

Function mutation is partly deepened after
[compiler: make function lifecycle a SymbolStore responsibility](https://github.com/frendsick/casa/issues/546),
and module analysis no longer copies cumulative stores after
[compiler: analyze canonical modules without cumulative SymbolStore copies](https://github.com/frendsick/casa/issues/564).
The current interface still exposes paired take and restore operations. Six
production call paths take a function body and later restore it. The store
keeps a declaration placeholder, but the type system does not require the
caller to return the body.

Later design should deepen one store or replace it with owned phase products.
It should not recreate the parser, resolver, and codegen wrapper adapters from
[RFC: extract SymbolRegistry via ports & adapters](https://github.com/frendsick/casa/issues/79).
Those wrappers had one backing implementation and would add interfaces without
removing the state protocol.

### Analysis and CLI seam

[`analysis::analyze`](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/analysis.casa#L49-L76)
is the only production entry point for full front-end analysis. CLI and LSP
both use it. This is a deep module with an explicit source, override, and
library-path input.

The output contract requires more caller knowledge than its three fields show:

- `typechecked: Some` can contain diagnostics and represents partial output.
- CLI must report diagnostics before unwrapping `typechecked`.
- `report_diagnostics` exits the process on errors, so the unwrap is safe only
  because of an effect that is not present in the function result.
- `bytecode::compile_typechecked` accepts the whole mutable `TypecheckResult`,
  checks its diagnostics again, then reads only `store` and `ops`
  ([source](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/bytecode.casa#L3713-L3721)).
- LSP first borrows the outer result for diagnostics, then consumes it to make
  an `AnalyzedDocument`. The order is correct but caller-controlled.

`compiler/typechecker.casa` also contains six thin forwarding functions to
semantic-analysis functions. Three exist only for tests. Three exist so
bytecode can call semantic helpers through the typechecker module. Together
with the test-facing clone-and-commit path, this is about 40 to 67 lines of
shallow production interface that can move behind the chosen compiler seam.

The CLI is 154 lines and imports 14 modules. It directly sequences source I/O,
analysis, diagnostic policy, typecheck-result extraction, bytecode lowering,
assembly emission, native build, and execution. The orchestration is small,
but its knowledge of partial analysis and backend representations makes it a
consumer of compiler internals rather than only a CLI adapter.

### Document query input

[`AnalyzedDocument`](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/document.casa#L22-L30)
owns root operations, four declaration maps, the compiler source store, source
text, and file identity. Its implementation has 44 public methods. Thirty-one
have no caller outside `compiler/document.casa`. Production LSP code uses six
query methods and directly reads only `source`, `file_path`, and `sources`.

The implementation is coupled to operation and declaration internals:

- `compiler/document.casa` contains 203 `OpValue` references covering 96
  distinct variants.
- The semantic-token table classifies 94 variants
  ([source](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/document.casa#L414-L512)).
- The hover table interprets 64 variants and embeds stack-effect text
  ([source](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/document.casa#L634-L704)).
- Document queries call `Function::clone_for_import` 10 times. That operation
  deep-clones the complete operation body
  ([clone](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/common.casa#L1513-L1533)).
- `DocumentElement` and `DocumentSymbol` own optional `Function` values.
  Queries therefore copy compiler entities to represent identity and context.

The LSP-facing seam is already narrower than the implementation. A candidate
minimum interface contains its six query operations, private compiler fields,
and stable editor facts or indexes produced once per analysis. The 1,232-line
`AnalyzedDocument` implementation is the maximum relocation surface, not a
credible deletion estimate. The chosen product must determine which scans can
be removed.

### Diagnostics

[`Diagnostics`](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/error.casa#L52-L188)
exposes its entry list. CLI, formatter, and LSP all destructure `Diagnostic`.
CLI and formatter render errors through `SourceStore::format_error`, while LSP
rebuilds ranges and messages from raw `CasaError` and `CasaWarning` fields.
Warning text formatting lives in `casa.casa`, not in the diagnostics module.
Source-context error formatting lives on `SourceStore` in `compiler/lexer.casa`.

This is not duplicate transport code. CLI text and LSP JSON need different
adapters. The accidental part is that each adapter must know diagnostic
variants, optional-field conventions, source fallbacks, and warning kinds.
A presentation-neutral diagnostic view could own those rules while each
adapter keeps only text or LSP conversion. About 100 lines currently perform
diagnostic projection across CLI, LSP, formatter, and `SourceStore`. Net
deletion depends on the selected interface.

### Formatter coupling

The formatter calls the compact `syntax::analyze_syntax(source, file)` seam.
That part is deep and should be preserved. Its implementation constructs the
normal parser and a full `SymbolStore`, then enables `Parser.syntax_only`.
`compiler/syntax.casa` contains 25 references to this mode across parsing and
fact construction.

The formatter itself names 12 `TokenKind` variants in 64 occurrences, eight
`SyntaxSpanKind` variants in 17 occurrences, and directly reads token value,
kind, and location throughout its 2,605 lines. It also re-runs syntax analysis
after formatting to compare syntax facts. Later architecture work must decide
whether formatting consumes a lossless syntax product or whether normal
parsing always produces the required spans. The current bool-selected parser
mode is the complexity to remove. A second parser is not justified by this
evidence.

### Backend representation and test coupling

[`InstValue`](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/common.casa#L241-L397)
has 136 variants. The emitter dispatch names all 136. Common owns a 54-case
direct `OpValue` to `InstValue` mapping even though only bytecode lowering uses
it
([source](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/common.casa#L3310-L3372)).
`Program` exposes seven fields, and the emitter adds `Program::split_bytecode`
from another module.

The representation may earn its place as a testable backend seam. Its location
in `common` does not. The target architecture should compare keeping it inside
a deep backend module against direct assembly emission. The decision must use
backend measurements from the separate backend audit.

Tests show the cost of the current public representation:

- 17 compiler test files name `OpValue` and eight name `InstValue`.
- Tests contain 186 direct `Op::new` calls and 290 operation-value field
  references.
- `tests/compiler/test_emitter.casa` has 80 occurrences of positional
  `Program` construction.
- The dead `Op.deferred_return_hint` field is kept alive only by a clone test.

Focused phase tests remain useful. Later work should preserve them behind the
chosen phase interfaces and remove tests whose only purpose is to preserve a
field layout or manual transfer protocol.

## Ranked architecture priorities

1. [Choose compiler representations and state ownership](https://github.com/frendsick/casa/issues/646)
   should make operation phase and identity valid by construction. It must
   decide whether ownership facts travel with checked operations or live in an
   owned checked-program product.
2. The same decision should make declaration transitions atomic and privatize
   store fields. It should deepen the existing store rather than add per-phase
   wrapper adapters.
3. [Choose compiler products for editor and formatter tooling](https://github.com/frendsick/casa/issues/650)
   should define consumer-specific outputs for error-free code generation,
   partial editor analysis, diagnostics, and lossless formatting.
4. [Choose the backend and runtime seams](https://github.com/frendsick/casa/issues/649)
   should decide whether `InstValue` remains. If it remains, `InstValue`,
   `Program`, lowering, emission, and their focused tests should form one deep
   backend module.
5. Only after those decisions should `common.casa` be split. The move should
   follow ownership of lexical, typed-operation, declaration, layout, and
   backend representations. File movement without interface reduction has no
   maintenance gain.

## Ponytail findings

The ranking uses the size of the removable or relocatable surface. Estimates
exclude the larger redesign where the net line change is not yet supported by
evidence.

1. `shrink:` evaluate replacing repeated raw-IR document scans and deep
   function clones with an analysis-produced document product. The six
   production LSP queries define a candidate minimum interface. Up to 1,232
   lines can move behind the seam, but the net deletion is not yet known.
   [`compiler/document.casa`](../../compiler/document.casa)
2. `shrink:` require the chosen representation to eliminate phase-polymorphic
   `Op` mutation plus the store-keyed ownership protocol. Net deletion depends
   on the chosen representation.
   [`compiler/common.casa`](../../compiler/common.casa)
3. `shrink:` privatize `SymbolStore` fields and make body transformation,
   specialization, and semantic-store ownership atomic. This removes two
   field-wise transfer lists and six caller-managed take/restore sequences.
   Net deletion depends on whether the store survives the architecture choice.
   [`compiler/common.casa`](../../compiler/common.casa)
4. `yagni:` remove six typechecker forwarding functions. Call the selected
   semantic or checked-program seam directly. About 40 lines can be removed
   conservatively. The separate test-facing clone-and-commit path has an
   unproven net deletion and is excluded from this estimate.
   [`compiler/typechecker.casa`](../../compiler/typechecker.casa)
5. `shrink:` centralize diagnostic variant and source-location projection in a
   presentation-neutral view. Keep CLI text, formatter text, and LSP JSON as
   adapters. About 100 lines can be relocated, with net deletion unproven.
   [`compiler/error.casa`](../../compiler/error.casa)
6. `shrink:` remove the bool-selected formatter path from the compiler parser.
   Compare unconditional syntax facts with an explicit lossless syntax product.
   Do not add a second parser without supporting evidence. This removes 25 mode
   references. Net deletion depends on product design.
   [`compiler/syntax.casa`](../../compiler/syntax.casa)
7. `delete:` remove `Op.deferred_return_hint`, its two propagation branches,
   its one semantic read, and its clone-only test setup. No production
   construction path seeds a non-empty value. About 17 lines and one field
   disappear.
   [`compiler/common.casa`](../../compiler/common.casa)
8. `delete:` remove six imports with no qualified use: `parser`, `syntax`, and
   `typechecker` from the CLI, plus `os`, `syntax`, and `typechecker` from the
   LSP. Six lines disappear. [`casa.casa`](../../casa.casa)

net: -63 lines, -0 dependencies possible from conservative direct cuts. The
larger representation and product changes are excluded from this net.

## Reproduction

```sh
wc -l compiler/common.casa compiler/error.casa compiler/analysis.casa \
  compiler/document.casa casa.casa

rg -c '^\s*pub (fn|struct|enum|const)\b' compiler/common.casa \
  compiler/error.casa compiler/analysis.casa compiler/document.casa \
  compiler/bytecode.casa casa.casa lsp.casa formatter/format.casa

for file in $(rg -l 'OpValue::' --glob '*.casa'); do
  occurrences=$(rg -o 'OpValue::[A-Za-z0-9_]+' "$file" | wc -l)
  variants=$(rg -o 'OpValue::[A-Za-z0-9_]+' "$file" | sort -u | wc -l)
  printf '%4d %4d %s\n' "$occurrences" "$variants" "$file"
done

rg -o '\bsyntax_only\b' compiler/syntax.casa | wc -l
rg -o 'clone_for_import' compiler/document.casa | wc -l
rg -l 'OpValue::' tests/compiler --glob '*.casa' | wc -l
rg -l 'InstValue::' tests/compiler --glob '*.casa' | wc -l
rg -o 'common::Op::new' tests/compiler --glob '*.casa' | wc -l

fields='next_op_id|variables|constants|enums|structs|traits|builtins|copy_types|borrow_shapes|op_ownership|immutable_globals|immutable_global_files|immutable_global_slots|public_symbols|private_symbols|module_private_symbols'
production=$(rg --files -g '*.casa' compiler formatter | rg -v '^compiler/common\.casa$'; printf '%s\n' casa.casa lsp.casa)
printf '%s\n' "$production" | xargs rg -U -o --no-filename -r x \
  "\\b[A-Za-z0-9_]*store[[:space:]]*\\.($fields)\\b" | wc -l
sed -n '429,618p' compiler/syntax.casa | rg -U -o --no-filename -r x \
  "\\b(self|imported|cloned)[[:space:]]*\\.($fields)\\b" | wc -l
sed -n '11896,11926p' compiler/semantics.casa | rg -U -o --no-filename -r x \
  "\\b(source|cloned)[[:space:]]*\\.($fields)\\b" | wc -l
sed -n '28,51p' compiler/typechecker.casa | rg -U -o --no-filename -r x \
  "\\b(source|target)[[:space:]]*\\.($fields)\\b" | wc -l
rg -U -o --no-filename -r x \
  "\\b[A-Za-z0-9_]*store[[:space:]]*\\.($fields)\\b" \
  tests/compiler --glob '*.casa' | wc -l
```
