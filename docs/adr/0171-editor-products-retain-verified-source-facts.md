# Editor products retain verified source facts

related issue: [Choose compiler products for editor and formatter tooling](https://github.com/frendsick/casa/issues/650).

This completes the tooling contract for the independent snapshots in
[ADR-0167](0167-compiler-products-own-independent-snapshots.md). The maintainer
accepted target-neutral analysis, debounced full reanalysis, useful partial
results, and workspace-wide references and rename on demand on 2026-09-26.
Workspace scope is the editor's opened folders with explicit exclusions.
Production migration and executable validation remain pending.

## Verified starting point

Evidence was checked against `62a46a6`, current `origin/main` when this worktree
was created. The source is unchanged at `719aa09`. Its qualified-only import,
constant-evaluation, generic, and backend decisions were incorporated before publication.
Historical audit counts are not current measurements.

- [Analysis](../../compiler/analysis.casa) can retain typechecked output after
  type and ownership errors. Unusable parse/resolve output instead gives
  [editor documents](../../compiler/document.casa) empty operations and maps.
  Failed imports can cause this loss across the root compilation.
- Editor documents retain operations and complete function, struct, enum, and
  variable maps. Queries scan operations and clone function bodies. There is
  no indication that an answer or list is incomplete.
- [The LSP](../../lsp.casa) recompiles the changed document with open-document
  overrides. It ignores document versions and does not invalidate dependents
  or handle document close. Completion always reports a complete list.
- References and rename inspect one compilation. If two roots import the same
  public function, renaming from one root can miss uses in the other root.
  The existing multi-file rename test proves root/import grouping only.
- [Syntax analysis](../../compiler/syntax.casa) supports formatting without
  imports or name resolution. It retains rejected tokens, but requires clean
  syntax before exposing formatter facts. Its equivalence check compares
  meaningful tokens and comment facts, not its structural spans.
- [Extern checking](../../compiler/typechecker.casa) currently invokes
  System V ABI classification during analysis. Target-neutral analysis must
  separate language admissibility from physical target restrictions.

These are code-derived facts. Existing tests do not establish the recovery
matrix below, version safety, or editor latency.

## Products and query answers

Keep the three typed operations from ADR-0167:

| Operation | Product and consumer |
| --- | --- |
| `syntax(SourceUnit)` | Root-only tokens, structural facts when usable, equivalence checking, and report. Formatter and syntax tests consume it. It does not load imports. |
| `analyze(CompilationInput)` | Report and source-oriented `EditorIndex`, including partial facts. LSP and query tests consume it. It has no target argument or codegen extraction. |
| `assembly(CompilationInput, Target)` | Target-tagged assembly or source/target rejection, with report. CLI and native build tests consume it. Native tool execution remains outside the compiler. |

All three preserve ADR-0167's outer `CompilerFailure`: reportable internal
failure owns accumulated diagnostics and exact source context. It does not
publish an unfinished index or checked program.

Use one report-owned diagnostic view with severity, code, message, optional
primary source range, and ordered notes with optional related ranges. Expected
and actual values belong in this shared presentation, not in each adapter.
Diagnostics retain deterministic compiler encounter order. Adapters only
render text or convert source ranges and presentation values to their protocol.
Never move an imported diagnostic to an unrelated root location. A diagnostic
without a source range remains an explicit request-level diagnostic.

A source range identifies a retained file and a half-open byte interval in
that snapshot. Queries take positions in the same coordinate system. Protocol
position conversion uses retained source text, including non-ASCII text, and
never rereads disk. Invalid positions and files outside the snapshot return
unavailable answers rather than fabricated empty results.

There are still five typed queries. The following is contract notation, not
Casa syntax. It refines ADR-0167's provisional answer shapes, including the
`Option` results for hover and definition.

| Query | Owned payload and availability |
| --- | --- |
| `hover(snapshot, position)` | `Known(Hover)` with occurrence range and source-level presentation, `Absent`, or `Unavailable(reason)`. Presentation can include a declaration, established type, stack effect, and documentation. A declared type is not a claim that the body passed checking. |
| `definition(snapshot, position)` | `Known(SourceRange)`, `Absent`, or `Unavailable(reason)`. Resolve the selected source name segment, including qualified names. Builtins without a source declaration have no source definition. |
| `completion(snapshot, position, trigger)` | `Complete(items)`, `Incomplete(items, reason)`, or `Unavailable(reason)`. Items carry label, kind, insertion text, replacement range, and optional established detail. Scope visibility and receiver capability filter candidates through shared semantic facts. |
| `references(snapshot, position, include_declaration)` | Complete or incomplete exact source ranges with the declaration source range, explicit compilation coverage, and rename constraints, or unavailable identity. Deduplicate and order ranges by source identity and offset. Declaration inclusion is explicit. |
| `semantic_tokens(snapshot, file)` | Complete or incomplete sorted, non-overlapping source ranges with token kind/modifiers, or unavailable file. Lexical categories remain possible without semantic categories. Only requested-file ranges are returned. |

`Absent` means the position was understood and has no supported answer.
`Unavailable` means the necessary fact could not be established. An incomplete
list can be empty. A complete empty list is a verified result within the stated
scope. Reasons distinguish rejected syntax, unresolved dependencies, missing
semantic facts, and invalid query input. Callers do not infer availability from
the presence or absence of diagnostics.

Every returned item is established for the current snapshot. Incompleteness
describes missing coverage, not a license to return speculative types or name
bindings. Hover may show a written declaration while omitting an unavailable
inferred type. A list's coverage concerns that query, not whether the whole
program can compile.

The LSP sets completion's incomplete flag when appropriate. Unavailable point
answers become no result. It may present verified partial highlighting.
The [LSP reference response](https://github.com/microsoft/language-server-protocol/blob/gh-pages/_specifications/lsp/3.17/language/references.md)
has no completeness field. Return verified partial references with a visible
request/status message explaining the missing coverage. Streaming partial
results alone does not convey this limitation. Rename never consumes incomplete
coverage.

## Recovery requirements

Parsing and semantic analysis own recovery and fact validity. The editor does
not run a second parser, resolver, typechecker, or ownership checker. Commit
facts only when their prerequisites are established. Track which regions or
queries lost coverage without making the whole file unavailable by default.

| Failure | Diagnostics and usable facts | Withheld or incomplete facts |
| --- | --- | --- |
| Lexical | Keep diagnostics, exact bytes, and recognized tokens. Keep declarations and semantic facts in independently recoverable regions and modules. | No semantic claim inside invalid tokens or where token boundaries are uncertain. An unterminated literal can make the rest of that region unavailable. |
| Parser | Keep sound declarations and scopes outside the damaged construct. Continue at structural synchronization points established by the shared parser. | An incomplete declaration is not a valid symbol. Facts that depend on ambiguous scope or delimiter ownership are unavailable. |
| Import | Keep the import-site failure, loaded-source diagnostics, and independent local or successfully imported facts. | Failed-module members and dependent bindings/types are unavailable. A missing import does not erase unrelated declarations. |
| Type | Keep established declarations, resolved occurrences, and types that do not depend on the failure. | Do not invent receiver types, inferred stack effects, or downstream facts that require a failed stack transition. |
| Ownership | Keep sound name/type facts and diagnostics. A borrow error does not erase an established declaration or definition. | Do not claim ownership validity or dependent receiver capability where checking failed. No checked codegen product can be extracted. |

This is a minimum recovery contract, not a promise to recover every malformed
construct. The front-end and semantic seam decisions must specify the
synchronization and fact-commit algorithms. Unaffected means independent of
the failed fact, not merely on another line. A malformed delimiter can prevent
that independence from being established.

Report every discovered diagnostic with its actual source attribution. The LSP
can publish per-file diagnostics from current snapshots, including imported
files. When multiple roots contribute diagnostics to one file, retain their
root provenance internally, deduplicate identical diagnostics, and remove only
the invalidated contribution. Request-level failures remain visible through the
adapter's request/status error path. Do not silently filter out imported errors.

## Workspace references and rename

Workspace-wide references and rename are required on demand. Discover all Casa
files beneath the editor's opened workspace folders, including unopened files
and unsaved Casa documents within those folders. Apply explicit exclusions.
Do not require a project manifest. Avoid directory traversal cycles. Imported
files outside those folders remain searchable but read-only for rename.
Excluded and unknown downstream source files are outside the guarantee. With
no workspace folder, report only the explicit document/import scope and do not
claim workspace completeness.

The LSP owns discovery and aggregation. A compiler reference query still covers
one compilation's root and transitively discovered imports. Analyze discovered
files as roots under the same captured library paths and overrides. Reuse only
current snapshots. Query each relevant snapshot at the target declaration's
source position, which can be in an imported file. A complete compilation that
does not load that file contributes no references. A failed compilation that
could hide an import or occurrence makes coverage incomplete.

Group results by declaration source range and identical declaration-source
revision, never by compiler IDs from different snapshots. Deduplicate source
occurrences. If root contexts resolve one occurrence differently, report the
ambiguity and do not rename it. This uses source locations to correlate owned
answers, not a persistent cross-request compiler symbol identity. Convert each
answer using its retained source before releasing temporary snapshots. Keep
only the presentation results and source revision information needed by the
workspace request. The full collection need not retain every compiler snapshot.

Workspace completeness requires completed discovery and sufficient query
coverage from every potentially relevant root at one current workspace
generation. A broken unrelated function need not prevent local rename if the
shared checker establishes complete coverage of that binding and its captures.
A broken file that could hide a use of an exported symbol prevents complete
workspace rename. Cancellation, unreadable directories, or incomplete discovery
must not become a complete empty answer.

Validate that replacement text is one complete identifier through the shared
syntax product. Before returning edits, analyze the proposed edited sources
through the same compiler interface and verify that target occurrences still bind to the renamed
declaration and other previously resolved occurrences keep their bindings.
Compare source occurrences through the proposed edit mapping. Reject scope
collisions, capture, changed dispatch, new diagnostics, or unavailable facts
needed to establish rename safety. This can repeat workspace analysis. It
avoids implementing a second name resolver in the LSP.

Use source token ranges and the existing definition/reference queries for these
comparisons. Map declaration ranges through the edits and compare established
source targets, not IDs from different snapshots. No sixth editor query or
public scope/declaration map is required.

Return a workspace edit only after discovery, coverage, and rename validation
complete. A declaration outside the writable workspace, or a rename that needs
to edit a read-only imported file, is unavailable. Public functions can be
renamed within the declared workspace. This does not promise to update unknown
external clients of a library.

Use versioned edits for open documents. For closed documents, recheck the exact
disk contents used by analysis immediately before returning edits and cancel
if they changed. The [LSP optional document version](https://github.com/microsoft/language-server-protocol/blob/gh-pages/_specifications/lsp/3.17/types/versionedTextDocumentIdentifier.md)
permits `null` for unopened files whose disk content is authoritative. This
does not provide an atomic filesystem transaction or protect against unrelated
external writes after the final check. Require client support for document
edits with versions for open buffers. Do not fall back to unversioned edits for
those buffers.

## Source identity and repeated edits

Use the same file identity rules for roots, imports, and overrides: normalized
absolute paths, resolved against the request's captured base directory.
Preserve case and normalize `.` and `..` after combining relative paths with
that base. Identity does not require filesystem existence or `realpath`, so
unsaved files work. Symlink spellings remain distinct identities. The LSP owns
URI conversion, including escaping, outside the compiler.

Normalize override keys before lookup. Root input is authoritative for its
identity. Identical non-root aliases may coalesce. Reject conflicting override
texts for one identity rather than choosing by map iteration order. The report
must identify both supplied aliases. Ordered import search still selects the
first available candidate under Casa's existing path/module rules.

Each request reads a selected source once and retains those exact bytes. An
override wins over disk for the same identity. The finished snapshot retains
only sources used by its report or facts, not unrelated open documents. This
is not an atomic filesystem snapshot across all reads.

The LSP owns document versions and a workspace generation. Keep current open
text separately from completed compiler snapshots. Opening, changing, or
closing a document, a relevant filesystem change, or a workspace-folder,
exclusion, library-path, or base configuration change advances the generation.
Initial invalidation is coarse:
invalidate all cached analyses in that workspace. This also catches newly
created import candidates, deleted files, and changed search precedence
without a separate public dependency graph.

Debounce and coalesce edits, then construct independent snapshots from the
latest inputs. Use one analysis worker initially, retaining at most one pending
replacement per root. Reanalyze open roots for diagnostics and local queries.
Discover and analyze remaining workspace roots when references or rename needs
them. Do not rescan and compile the entire workspace on every keystroke.
Before publishing, compare the captured generation and document versions with
current inputs. Discard superseded results. Queries never apply old ranges to
new source, even when the root text stayed unchanged and only an import changed.

Closing a document removes its override, releases its cached product when no
query borrows it, and invalidates dependents through the workspace generation.
Saving does not replace newer open text with older disk text. Observe relevant
file create, delete, rename, and content changes, including library search
directories. If reliable change observation is unavailable, revalidate inputs
before reusing a cached result. Workspace configuration and missing-import
changes require the same freshness checks as ordinary edits.

Install a current replacement atomically. Release the previous report and index
when temporary query borrows end. Never append new facts into an old index.
Owned query values still refer to their originating source: retain the snapshot
until source-based conversion finishes, or retain already converted results.
Conversion does not waive the version check before publication or edit use.

## Construction cost and target diagnostics

Shared parsing and semantic traversal project source identities, occurrences,
scopes, declarations, established type displays, and coverage into the private
index. Queries read those facts. They do not scan semantic bodies, reverse
rewritten names, clone functions, or repeat semantic decisions. Declarations
and calls introduced only by specialization or derives do not create duplicate
source occurrences. Attribute their diagnostics to the originating source.

Retain the index only for analysis. Under the accepted construction model,
assembly can still incur fact-projection and temporary-storage costs before
discarding them. This is not a claim that assembly builds the index for free.
Any later omission of unused projections must keep the shared semantic owner.
Syntax requests do no semantic analysis or editor-index construction.

Measure full request time, index construction, retained source/index bytes,
peak replacement memory, and release after repeated edits. Include a small
file, imported unsaved files, the compiler source, and errors from each recovery
class. Measure end-to-end edit latency, already-built index queries, workspace
discovery, and cold/warm reference and rename requests separately. Include two
roots importing one declaration, unopened files, and rejected workspace inputs.
Report debounce time separately from computation. Set numerical
latency and memory gates from executable evidence through the existing
simplification-target and blueprint decisions. No numerical gate or performance
gain is established here.

Analysis validates target-neutral source obligations, including language-level
extern admissibility. It does not validate physical layouts, ABI placement,
runtime availability, or native tool success. Assembly repeats source checking
for its independent inputs and adds target diagnostics. With identical inputs,
source diagnostic codes and attribution must agree across analysis and
assembly. A clean analysis report does not promise a successful build. Target
diagnostics and late compiler failures retain the complete report. The backend
decision owns their concrete checks.

## Formatter safety

Syntax retains exact source bytes, original token spelling, comments, and
structural facts for definitions, delimited forms, aggregate bodies, control
forms, matches and arms, and accessor chains. The shared grammar produces these
facts without constructing semantic declarations or loading imports. Internal
representations and private formatter helpers need not become public compiler
interfaces.

Format only syntactically accepted input. Missing imports, unresolved names,
type errors, and ownership errors do not prevent formatting. Lexical or syntax
rejection returns the original input with failure status, even when editor
analysis could recover useful facts. Invalid UTF-8 preserves original bytes.

Reparse the candidate through the same syntax operation. Require equivalence
of meaningful token kinds and exact spelling, logical comment text/order and
attachment, and normalized grammatical structure. Compare structure by token
relationships, not byte offsets that formatting changes. Permit only existing
grammar-authorized whitespace, newline, comma, and comment-prefix spacing
normalization. A mismatch, candidate rejection, or reportable internal failure
returns the original input and failure status. Keep idempotence and paired
layout convergence checks. The current token/comment comparison alone is not
evidence that the new structure check exists.

## Migration and validation

| Retired coupling | Replacement and retained behavior |
| --- | --- |
| `AnalyzedDocument` operations and function/struct/enum/variable maps, `DocumentElement` and `DocumentSymbol` owning functions | Private source index and owned query payloads. Keep exact hover, qualified-name ranges, navigation, and visibility behavior. |
| Document `OpValue` classifiers, query-time `clone_for_import`, scans of function bodies | One projection from shared semantic facts. Keep intrinsic stack-effect and contextual-literal presentation. |
| `TypecheckResult.into_document_parts`, `SymbolStore.into_document_facts`, discarded document diagnostics | Analysis directly owns its report and index. No adapter extracts phase-private compiler state. |
| Analysis's cloned diagnostic list, three adapters interpreting diagnostic variants, formatter's temporary source store for reporting | One report and diagnostic presentation. Keep distinct text and LSP transport adapters. |
| Formatter `syntax_only` flag paths and semantic `SymbolStore` construction | Shared syntax recognition and facts, followed by separate semantic analysis when requested. Keep formatter safety. |
| Reverse source-name maps and test-facing typechecker clone/commit/forwarding protocols | Source spelling separate from identity, and tests through typed products. Removal is shared with the front-end and semantic migrations. |

Remove assertions whose sole purpose is preserving `.ops`, declaration-map
layout, take/restore ordering, cloned metadata fields, or source-name clone
survival. Examples occur in `test_lsp.casa`'s partial-operation checks and
`test_common.casa`'s lifecycle/clone checks. Preserve their distinct observable
behaviors through the new interfaces. Do not remove semantic or ownership
coverage merely because its old fixture inspected internal state.

Required migration checks cover every query against each recovery class, with
an unaffected declaration and a dependent damaged region. Distinguish absent,
unavailable, incomplete-empty, and complete-empty answers. Check a missing
import, an imported-file diagnostic, shadowed locals, captures, qualified-name
segments, source spelling after specialization, and requested-file token ranges.

Check unsaved imported overrides, normalized aliases, conflicts, Unicode range
conversion, replacement during queries, stale worker completion, imported-file
changes with unchanged root text, close/save, file creation after a failed
import, changed library search order, and storage release after repeated edits.
References must report compilation coverage internally and workspace coverage
after aggregation. Test public rename across two roots and an unopened importer,
explicit exclusions, external read-only libraries, incomplete discovery,
different root contexts, and edits during a workspace request. Rename must
reject incomplete required coverage, stale inputs, invalid names, scope
collisions, and changed bindings or dispatch after candidate reanalysis.

Keep source-to-assembly rejection and report-lifetime checks, formatter golden
outputs, safe refusal, token/comment/structure equivalence, idempotence, and
paired layouts. These are requirements for the executable slice and production
migration, not tests supplied by this documentation change. Source reduction is
unmeasured. Do not count moved presentation code or shared removals twice.
