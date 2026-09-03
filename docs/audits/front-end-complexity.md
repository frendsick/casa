# Front-end complexity audit

Status: complete

Source revision: [`bb6ffa7`](https://github.com/frendsick/casa/commit/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1), 2026-09-02.

Run date: 2026-09-03.

## Result

Casa's compiler has a good outer front-end seam. [`analysis::analyze`](../../compiler/analysis.casa#L49-L75) owns source setup and calls [`parse_and_resolve`](../../compiler/syntax.casa#L8495-L8518) before typechecking. The formatter has a second small seam through [`analyze_syntax`](../../compiler/syntax.casa#L8359-L8388). These interfaces hide substantial implementation detail.

The implementation behind those seams has poor locality. `syntax.casa` is 9,918 lines and owns parsing, module discovery, import resolution, declaration elaboration, constant execution, generated declarations, trait validation, identifier resolution, and formatter facts. Most of that code is public. Names and literals are interpreted more than once, and several phase states exist only as conventions across mutable collections.

The first later change should delete obsolete selective-import machinery. Production computes one closure and prunes one shared store. It does not consume the closure's `public_surface_types` and does not call the old transactional merge interface. This is a verified deletion of about 350 to 380 production lines.

The next priorities are the namespace prepass and the broad `syntax.casa` interface. These have more design risk, but they cause repeated grammar interpretation, token mutation, source-name side tables, and direct test dependence on parser state.

This audit identifies deletion and deepening opportunities. It does not select a token model, pass graph, symbol-store model, module layout, or target architecture.

## Scope and method

The audit traced the two production entry paths, every production caller of top-level public functions in the three target files, shared front-end state in `common.casa`, and focused tests. It also checked the module-analysis history so that removed clone-and-merge costs are not reported as current complexity.

At the run date, the only open pull request was [`feat(examples): add practical raylib demo`](https://github.com/frendsick/casa/pull/634). Its changed paths are the release environment, one example, example documentation, generated example output, and example tests. It does not change the audited front-end paths or their callers.

Simple code lines below exclude blank lines and full-line `#` comments. Public-function counts include methods for the total column. The external entry count includes only top-level functions called from production Casa files outside the defining file.

| Module | Lines | Simple code lines | Public functions | Private functions | External production entries |
|---|---:|---:|---:|---:|---:|
| [`compiler/lexer.casa`](../../compiler/lexer.casa) | 1,156 | 1,017 | 46 | 1 | 8 of 12 top-level functions |
| [`compiler/syntax.casa`](../../compiler/syntax.casa) | 9,918 | 9,051 | 270 | 13 | 4 of 205 top-level functions |
| [`compiler/selective_import.casa`](../../compiler/selective_import.casa) | 929 | 890 | 30 | 0 | 1 of 4 top-level functions |
| Total | 12,003 | 10,958 | 346 | 14 | 13 of 221 top-level functions |

The focused implementation-facing test surface is also large:

| Test area | Lines | Test functions |
|---|---:|---:|
| [`test_lexer.casa`](../../tests/compiler/test_lexer.casa) | 1,664 | 113 |
| [`test_parser.casa`](../../tests/compiler/test_parser.casa) | 1,410 | 101 |
| [`test_selective_import_closure.casa`](../../tests/compiler/test_selective_import_closure.casa) | 718 | 26 |
| [`test_const.casa`](../../tests/compiler/test_const.casa) and [`test_const_param.casa`](../../tests/compiler/test_const_param.casa) | 228 | 20 |
| Formatter implementation and golden inputs or outputs | 3,013 | Not function-based |

Current self-compilation provides an integrated baseline, not front-end attribution. `casac v1.50.0` compiled this revision three times on Linux under WSL2 on an AMD Ryzen 7 3700X. Median wall time was 30.89 seconds, spread was 0.66 seconds, and median peak RSS was 760,564 KiB. The samples were 31.49, 30.83, and 30.89 seconds. The existing [module-analysis investigation](../benchmarks/module-analysis.md#post-implementation-measurements-564) remains the authoritative evidence that canonical module analysis removed the former store clone-and-merge cost.

## Current responsibility and ownership map

| Area | Current owner and flow | State and dependency facts |
|---|---|---|
| Source loading and lexing | `analysis::analyze` creates a [`SourceStore`](../../compiler/lexer.casa#L167-L316). Root text uses `lex_source`. Imports use `lex_file`. Formatter text uses `lex_source_fmt`. | `lexer.casa` owns UTF-8 decoding, path normalization, source storage, line indexes, diagnostic rendering, file loading, and tokenization. `SourceStore` is also returned to CLI and LSP consumers. |
| Parsing | [`parse_ops`](../../compiler/syntax.casa#L8211-L8249) repeatedly calls the token-to-operation parser and controls error recovery. | [`Parser`](../../compiler/syntax.casa#L201-L215) owns a `SymbolStore`, sources, diagnostics, import graph state, initializer indexes, two mode booleans, and mutable caches. [`ParseState`](../../compiler/syntax.casa#L47-L164) uses one-element `List[bool]` values as mutable cells. |
| Module discovery | [`namespace_module_tokens_with_prefixes`](../../compiler/syntax.casa#L6521-L6552) scans top-level import and declaration syntax before normal parsing. [`collect_resolved_import_prefixes`](../../compiler/syntax.casa#L6674-L6721) scans imports before that pass. | The prepass rewrites `Token.value` from source spelling to internal identity. [`SymbolStore::record_source_names`](../../compiler/common.casa#L3530-L3564) keeps a reverse side table for diagnostics and formatting. |
| Imports | `Parser` resolves paths, assigns canonical prefixes, parses imports, analyzes each source once, tracks dependencies, retains initializers, and prunes declarations. | [`ImportedModule`](../../compiler/syntax.casa#L166-L172) stores declaration keys and dependency facts. [`ModuleContext`](../../compiler/syntax.casa#L174-L199) stores five parallel collections whose relationships are maintained by parser methods. The cache and cycle stack live for one analysis call. |
| Selective imports | [`handle_selective_import`](../../compiler/syntax.casa#L7050-L7111) asks the closure builder for declaration keys, drops the returned public-surface set, and retains keys in the shared store. | The builder depends on `common`, `error`, `semantic_rules`, and `semantics`. It reuses `SemanticSession`, so operation semantics are not independently simulated here. |
| Constants | Parsing dispatches to literal conversion and a syntax-owned interpreter for constant functions and blocks. | [`parse_literal_to_constant_value`](../../compiler/syntax.casa#L1982-L1998), [`eval_const_fn`](../../compiler/syntax.casa#L2309-L2450), and [`eval_const_block`](../../compiler/syntax.casa#L2452-L2522) read and write shared declaration facts. |
| Declaration elaboration and generated declarations | Declaration parsing writes functions, constants, globals, aggregates, traits, and implementations directly into the shared store. Resolution later derives traits, creates member accessors, finalizes trait methods, and synthesizes Copy or Clone fallbacks. | Generated functions use the same `Function`, visibility sets, names, and resolution path as source functions. The generation policy is spread across roughly 1,500 lines in `syntax.casa`. |
| Identifier resolution | [`finish_resolution`](../../compiler/syntax.casa#L8429-L8493) expands imports, prunes the store, validates and generates declarations, then resolves root operations, function bodies, trait defaults, and global initializers. | The shared [`SymbolStore`](../../compiler/common.casa#L3420-L3441) contains declarations, visibility sets, source-name mappings, ownership facts, and later-phase facts. Its heap-backed maps alias when the struct is passed by value. |
| Formatter facts | `analyze_syntax` lexes lossless tokens, removes comments and newlines for parsing, runs the normal parser in `syntax_only` mode, and returns token, comment, and structural facts. The formatter analyzes its output again and compares the facts. | `syntax.casa` has 25 references to `syntax_only`. Parse state owns span stacks and accessor state. The formatter depends on the parser's span kinds and `fallback_token` in addition to the two intended fact functions. |

## Repeated interpretation and invalid intermediate states

### Names and imports

The namespace prepass interprets import clauses, declaration headers, enum bodies, sigils, and qualified names in [`compiler/syntax.casa` lines 6216 to 6552](../../compiler/syntax.casa#L6216-L6552). `collect_resolved_import_prefixes` separately interprets import clauses. Normal parsing then interprets the same import and declaration grammar again.

The prepass mutates identifier token values. Later code sometimes needs source spelling, so `SymbolStore` keeps a linear `source_names` list and copies it into later stores. A token can therefore be in source-name or internal-name form. Correctness depends on pass order and on keeping the side table synchronized.

The language requires source identity, aliases, visibility, selective roots, dependency closure, and cycle diagnostics. It does not require grammar to be scanned before it is parsed or require one token field to represent two name domains.

### Literals and constants

The lexer emits one `Literal` token kind. Syntax code classifies or converts that spelling in [`get_op_literal`](../../compiler/syntax.casa#L1011-L1047), `parse_literal_to_constant_value`, constant evaluation conversion helpers, [`parse_literal_match_pattern`](../../compiler/syntax.casa#L6032-L6062), and final identifier resolution. The repeated switches cover bool, char, integer, float, and string forms.

Casa requires literal spelling, constant values, pattern literals, and constant execution. It does not require every consumer to classify the raw spelling again.

### Parser and store state

`Parser` has 13 public fields. Its module records, initializer indexes, source prefixes, analyzed-source set, import stack, and mutable store can represent combinations that no valid front-end pass should create. `ParseResolveResult` and `SyntaxResult` also expose public fields that permit callers to construct mismatched success values and diagnostics.

Declaration parsing mutates the final shared store before a complete construct succeeds. [`parse_ops`](../../compiler/syntax.casa#L8211-L8249) can roll back emitted operations after recovery, but a failed construct that changed the store makes all output unusable because the store has no matching rollback. This fail-closed behavior is correct. The need for it shows that syntax recognition and declaration elaboration share one mutation boundary.

`SymbolStore` mixes source declarations, generated declarations, visibility, module-private names, source spelling, ownership results, and backend-consumed facts. Passing the struct by value aliases all heap-backed maps by design. The type does not encode which phase owns mutation or which collections must agree.

These states are implementation complexity. Error recovery, partial diagnostics, recursive declarations, and cross-module resolution are required language and tooling behavior.

### Formatter mode

The formatter must preserve meaningful token spelling, comments, and structural layout facts. Re-analyzing formatted output is a useful semantic guard and is not duplicate work to delete without equivalent proof.

The accidental part is the cross-cutting mode. Normal parser functions contain `syntax_only` branches and write formatter-only span stacks. This lowers locality because grammar changes can require edits in parser logic, fact collection, formatter layout, and golden tests.

## Interface depth and test coupling

The outer analysis interface is deep. One input produces diagnostics, sources, and an optional typechecked result. `parse_and_resolve` is also a useful seam because callers do not need its module graph or resolution lifecycle.

The file-level interfaces are shallow despite their size. Of 221 top-level public functions in the audited files, 208 have no production caller outside their defining file. `syntax.casa` exposes 205 top-level functions while production callers use only `parse_and_resolve`, `analyze_syntax`, `syntax_facts_match`, and `fallback_token`.

Tests explain part of this surface. `test_parser.casa` constructs `Parser` directly at 11 sites. It changes `search_paths` and `prune_before_resolution`, calls `namespace_tokens`, `parse_ops`, and `resolve_identifiers_global`, then inspects the cache, store, and parse state. The 718-line selective-closure test imports the declaration enum and calls the legacy merge entry that production does not use.

The tests provide valuable behavior coverage. Their current seam makes implementation details costly to change. Production-level parser and import tests already show that most behavior can be checked through `parse_and_resolve` and `AnalysisInput`.

One ownership anomaly also crosses module boundaries. [`syntax.casa` implements methods on `common::SymbolStore`](../../compiler/syntax.casa#L429-L621), including ownership accessors and `clone_for_typecheck`. Later phases use some of these methods even though their implementation is in the syntax module. This creates hidden compile-order and import coupling and weakens locality.

## Required complexity

Later work must preserve these verified rules unless the architecture map explicitly changes language behavior:

- UTF-8 validation, escape handling, exact source locations, and source retention for diagnostics.
- Reverse Polish operation syntax, declarations, patterns, control forms, recovery, and ordered diagnostics.
- Both documented [import forms](../modules.md#import-paths), ordered library-path lookup, source overrides, aliases, visibility, private dependency retention, cycle rejection, and one initialization per physical immutable global.
- Constant declarations, constant functions and blocks, and integer, bool, or char constant type parameters as recorded by [ADR-0153](../adr/0153-constant-type-parameters-accept-integers-bool-and-char.md).
- Inline trait derivation from [ADR-0015](../adr/0015-inline-trait-derivation.md), generated field accessors, trait defaults, and validated Copy or Clone behavior.
- Formatter preservation of token spelling and comments, structural layout ownership, and rejection of output whose syntax facts differ.

Canonical module analysis is current required context, not a remaining simplification claim. Each resolved source is analyzed once into one working store. The old cumulative store clones and merge path were removed by [`aa879c0`](https://github.com/frendsick/casa/commit/aa879c0a7b37b8c7fa6457cbb07fc94c02c277b4).

## Ranked findings

1. **`delete:` Obsolete selective-import result and merge surface.** Production calls only `compute_selective_import_closure_with_inherited`, then drops `public_surface_types`. The public-surface traversal at lines 386 to 558, `find_conflict_kind`, `add_to`, the test-only compute wrapper, and both merge functions have no production effect. Delete about 350 to 380 production lines and rewrite implementation-coupled tests around the retained-key behavior.
2. **`shrink:` Declaration elaboration, generated-declaration policy, and constant evaluation share the parser implementation.** About 1,900 to 2,100 lines for constant execution, derivation, accessors, method finalization, and Copy or Clone fallback generation can move behind narrow elaboration interfaces while `parse_and_resolve` stays the public seam. This is mainly relocation. Do not count it as net deletion until repeated naming, visibility, and conversion logic is measured after separation.
3. **`shrink:` Namespace discovery parses grammar before the parser and mutates token identity.** The 409-line discovery and prefix region plus the source-name side table contain an estimated 170 to 260 removable lines of repeated scanning and synchronization. Keep import and visibility rules. Remove the need for two interpretations of declarations and imports.
4. **`shrink:` The audited files have about 25 times as many public functions as private functions.** Make the 208 top-level functions with no external production caller private or hide them behind deeper modules. This removes little code by itself, but it reduces the supported top-level interface by about 94 percent and permits later implementation changes without coordinated test edits.
5. **`shrink:` Formatter fact collection is a parser-wide mode.** Preserve the two-pass semantic check, token facts, comment facts, and structural spans. Consolidating the 25 `syntax_only` sites and formatter-only parse state exposes about 80 to 140 lines to deletion and improves locality between grammar recognition and fact emission.
6. **`shrink:` Literal classification and value conversion are repeated across five syntax paths.** One owned interpretation can remove an estimated 70 to 120 lines of switches and make invalid literal-kind combinations harder to construct. The raw spelling still has to remain available for diagnostics and formatter facts.
7. **`shrink:` `lexer.casa` owns source storage and diagnostic presentation as well as lexing.** The 150-line `SourceStore` and source-position region has high leverage across CLI, LSP, parser, and diagnostics but low locality with tokenization. About 150 to 200 lines are relocatable behind a source repository interface. This is not a dependency or net-line reduction by itself.
8. **`yagni:` Tests preserve private parser and retired merge protocols.** Eleven direct `Parser` constructions and the selective-closure declaration assertions make cache fields, mode switches, and merge types part of the practical interface. Retain focused semantic cases, but prefer the production seams when the related implementation changes. This can remove about 200 to 350 test lines after the production deletion.
9. **`stdlib:` `dir_of_path` repeats standard path splitting.** Its 15-line scan differs from `os::path::dirname` only in its trailing-separator and empty-directory contract. A small adapter using the standard path utility can remove about 8 to 12 lines after the root, relative, and empty-path tests define the exact contract.

No `native:` replacement was verified. The audited modules add no third-party dependency, so no dependency removal is available.

Net production deletion frontier: about 680 to 910 lines. The range combines the verified selective-import deletion with conservative namespace, formatter, literal, and path estimates. The estimates overlap in `syntax.casa`, so later work must measure each landed change instead of adding independent headline numbers. A further 2,050 to 2,300 lines are candidates for relocation behind deeper module interfaces. Net dependencies removed: 0.

## Priority constraints for later architecture work

1. Delete the selective-import surface before designing around it. Its merge model is not part of the current production flow.
2. Preserve the deep `analysis::analyze` and `parse_and_resolve` seams unless evidence shows a caller needs more control.
3. Give source spelling and internal symbol identity explicit ownership. Do not keep the current token-rewrite and reverse-table protocol by accident.
4. Keep one semantic meaning for operations. Selective dependency discovery already uses the shared semantic session.
5. Treat generated declarations as normal downstream declarations after elaboration, but keep generation policy out of token parsing where a narrow seam is sufficient.
6. Preserve formatter equivalence checks while improving the locality of syntax fact production.
7. Move tests with behavior. Do not retain public parser fields only to keep implementation-facing tests unchanged.
