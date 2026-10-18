# Analysis reuses a verified import analysis

related issue: [lsp: reuse unchanged module analysis between document edits](https://github.com/frendsick/casa/issues/850).

This amends the repeated-request rules of
[ADR-0167](0167-compiler-products-own-independent-snapshots.md) and the
invalidation rules of
[ADR-0172](0172-editor-products-retain-verified-source-facts.md).

Analysis runs in two stages. The import stage parses and checks the modules a
root imports, without the root's declarations, and projects editor facts for
the imported files. The root stage parses and checks the root over a copy of
that import analysis. It projects root-stage facts and adds the imported-file
facts to the snapshot's index. Root-stage facts come first, so they win ties
between equal occurrences.

`analyze_cached` takes a caller-owned `AnalysisCache` that keeps one import
analysis. A later request reuses it only when all of these still match:

- the root identity, base directory, and ordered library paths
- the text of every imported file, read from its override or from disk
- the file that each import inside an imported module resolves to
- the order of the root's imports reported by the root stage

Any mismatch analyzes the imports again. The root stage always works on a copy,
including the request that creates the import analysis. Copies of maps iterate
in a different order than the originals, so this keeps a reused import analysis
and a new one producing the same snapshot. `analyze` uses an empty cache.

An import analysis is kept only when the imported modules report no
diagnostics. Otherwise analysis checks the root and its imports together, as
assembly does, and the cache stays empty. Assembly does not use two stages.

The language server keeps one cache per open document and drops it when the
document closes. Workspace reference and rename analyses use `analyze`.

## Considered options

- Keeping one combined analysis repeats the work for every imported module on
  each edit. After the projection indexes from #882, about 3.7 s of the 4.1 s
  that `compiler/abi.casa` took per edit was import parsing, checking, and
  projection.
- Caching each imported module separately needs a symbol store that can be
  assembled from independent module results. Imports share one store, their
  module prefixes depend on discovery order, and checked bodies refer to each
  other's declarations.
- Sharing the import analysis without copying it would need a persistent
  symbol store and checker state. The root stage adds declarations, bindings,
  and specializations to both.
- File-change notifications would avoid reading imported files on each request,
  but correctness would then depend on the client's watchers. Rereading and
  comparing the imported text takes milliseconds.

## Consequences

- With a reused import analysis, analysis of `compiler/abi.casa` takes about
  0.6 s instead of 4.1 s. A request that builds the import analysis takes about
  4.8 s.
- Each cache holds the checked import state, the imported-file facts, and the
  imported text. Six cached analyses of `compiler/abi.casa` peaked at 476 MB,
  compared with 243 MB for one combined analysis.
- Imported-file facts depend only on the imported modules. Completion in an
  imported file no longer offers the root's declarations, which that file
  cannot reference. Imported files keep their checked facts even when the
  root's syntax cannot be used.
- Diagnostics keep their codes and locations, but their order can differ from
  assembly's because the stages check functions in a different order.
- Snapshots stay independent. Each owns copies of the imported-file facts and
  text, and the cache never changes a published snapshot.
- Imports with diagnostics get no reuse. Editing an imported module into an
  invalid state makes each analysis of its importers as slow as before.
