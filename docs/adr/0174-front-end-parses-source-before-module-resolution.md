# The front end parses source before module resolution

related issue: [Choose the front-end and module-analysis seams](https://github.com/frendsick/casa/issues/647).

Shared syntax recognition preserves lossless tokens and structured constructs,
including both meanings of `Name {}`.
The source builder collects declaration headers and parses source bodies. Resolve source
names through compilation-local identities without changing token spelling.
This completes the front-end responsibilities of ADR-0167 under the
import, constant, and tooling contracts in ADR-0168, ADR-0171, and ADR-0172.
The semantic construction consumes these facts under
[ADR-0173](0173-semantic-checking-owns-source-obligations.md).
Shared syntax recognition feeds `source_builder.casa`. Production consumers
use typed requests in `products.casa`.

## Alternatives

Keeping token rewriting would preserve the repeated grammar and reverse-name
protocol. A complete parsed/resolved/typed product ladder would add ownership
transfers without resolving those responsibilities. The source
representation stays inside Compiler Capsule and passes directly to the shared
semantic construction chosen in ADR-0167. No new public phase controls are added.

## Private interfaces and ownership

The table describes responsibilities, not a requirement for one file or public
function per row. All constructors and mutable builders remain private.

| Seam | Inputs and outputs | Mutation, effects, and failure |
| --- | --- | --- |
| Source acquisition | Captured base directory, root bytes and identity, ordered library paths, overrides, and parsed import specifiers produce retained source entries. | The request source owner normalizes identities, reads a selected file once, and owns exact bytes before decoding. It records lookup, read, and input-conflict failures in the report. This is the front end's filesystem effect. |
| Syntax recognition | One retained source produces lossless tokens, structured source constructs, original name-segment ranges, grammar relationships, and rejected regions. | A cursor skips trivia without copying a second token list. Construct-local builders own unfinished syntax. This work reads no files, resolves no names, and creates no semantic declarations. Source errors retain diagnostics and safely recognized regions. |
| Module and declaration construction | Retained tokens and grammar facts produce the private `SourceProgram` and module-local lookup tables. | A request-local builder collects headers and parses declarations and bodies. It owns discovery state, alias bindings, declaration identities, visibility, and duplicate diagnostics. Failed prerequisites remain explicit. |
| Constant elaboration | Complete parsed initializers, visible constant identities, and established dependency values produce typed or contextual constant values. | A private evaluator commits a value once or records failure. It uses shared primitive typing and numeric rules, with no function bodies, runtime state, or target layout. A failed declaration has no substitute value. |
| Name and semantic construction | Source bodies and their scopes, declaration facts, and elaborated constants feed the shared semantic traversal. | The traversal establishes name targets, types, ownership, and generated behavior. It projects verified editor facts and builds private semantic bodies. Source errors never create a checked program. Detailed checking and specialization algorithms belong to the semantic-seam decision. |

The request report owns source bytes and accumulated diagnostics throughout
these operations. Source ranges refer to its retained entries. Source syntax
and builder state are released when checking no longer needs them, or on
failure. Analysis retains the report and editor index. Syntax retains its
root-only syntax product. Assembly follows the checked-program and backend
ownership in ADR-0167 and ADR-0169. A reportable internal failure preserves the
report and releases unfinished construction, as required by ADR-0172.

## Grammar without declaration lookup

Tokens retain their original spelling. Decode literal contents once into the
appropriate lexical value form while retaining their source ranges. Numeric
literals keep exact information until their typing context selects a width.
Recognition does not turn an integer literal into an `i64` prematurely or round
every float through `f64`. Name segments retain separate ranges for editor
navigation through qualified references.

Import nodes contain the decoded specifier, optional written alias, and source
ranges. Declaration nodes contain parsed headers, source-order positions,
visibility, type syntax, and structured bodies. The source builder collects
headers before parsing declarations and bodies. A type name
remains an unresolved source name until its owning lookup establishes identity.

Preserve the existing valid name-plus-brace forms. For example, `N {}` can
construct an empty struct or call a function and then create an empty closure,
depending on the declaration of `N`. Record the name and braced contents in a
neutral source form. Resolution selects the meaning from the established name
target. Source construction parses the children according to that meaning. If the target is unavailable, the dependent semantic fact is unavailable.
The formatter uses the neutral structure without imports or declaration lookup.

Recognize field labels from their grammatical position and `name:` syntax,
including unknown field names. A nested construct owns its own delimiters and
labels. An assignment's type annotation is part of the assignment, not a field
separator. Record each field expression in source order. Field existence,
visibility, duplicates, and construction validity are checked against the
resolved struct. In ordinary expression context, a non-struct name followed by
an ordinary braced body retains call-then-closure behavior. Array items and match
patterns retain their own grammar rules. This decision adds no further syntax
removals. Any proposed tightening of accepted punctuation must identify the
affected forms and obtain a separate language decision.

Reserving every `Name {...}` for construction would simplify this one ambiguous
case but change valid call-then-closure source. `move {...}` is not an equivalent
replacement because it changes capture behavior. The selected design retains one
neutral source form instead of introducing that language migration.

## Module discovery and declaration identity

Apply the source identity and override conflict rules of ADR-0172 at request
entry. Combine relative inputs with the captured base before normalizing `.`
and `..`. Use normalized absolute paths without resolving symlinks or changing
case. Root text is authoritative for its identity. Coalesce identical override
aliases and diagnose conflicting texts with both supplied aliases.

Resolve a path-style import relative to its importing file, without search.
For a module-style import, search that directory and then library directories
in their supplied order. A matching override makes a candidate available even
without a disk file. Preserve the same-directory self-match exception. Once
the first available candidate is selected, its read or source error is an
error for that candidate, not permission to try a later library.

Keep one module entry per identity. An active entry owns its position in the
discovery path. Reaching it again diagnoses the complete cycle. A completed
entry owns parsed facts and their availability. An unreadable or rejected
entry retains its failure attribution. These are private construction states,
not caller flags. Visit parsed imports depth first in source order, reuse
completed entries, and never reload a failed module under a different alias.

Bind each import alias to that module identity in the importing scope. A
repeated alias for the same module is allowed. A different alias gives another
route to the same declarations and types. Diagnose alias/declaration collisions
and aliases for different modules. Visibility belongs to declaration identity
and defining module, not a generated prefix. Imports do not re-export their
dependencies. Apply public access checks to declarations, methods, and fields
while retaining private implementation dependencies inside their defining module.

Collect complete declaration headers before checking ordinary bodies so
recursive references and later declarations resolve through the same module tables. Keep lookup
tables scoped to modules and lexical scopes. A source occurrence retains its
spelling and scope until shared resolution establishes its declaration or
binding identity. Bind parameters and locals at their lexical positions.
Use canonical identities for dependency and specialization keys, never aliases
or rewritten source names.

All imported sources and declarations receive the validation required by
ADR-0168. Imported root statements still receive syntax and name resolution,
but no imported root body is executed or added to the root stack/typechecking
flow. The shared resolver applies the same lookup rules to those statements.
No initializer selection, global-origin state, or initialization schedule exists.
Standard trait recognition must use resolved declaration facts in the semantic
owner, not basename-dependent namespace rewriting in the parser.

## Constants and generated behavior

After discovery establishes module and visibility facts, elaborate dependency
constants before importer constants. Within each module, visit constants in
source order. Initializer lookup sees only earlier local constants and public
constants reached through preceding imports. Ordinary body and type lookup
uses all successfully elaborated constants under ADR-0171.

The evaluator receives parsed primitive expressions and resolved constant
references. It checks the closed operation set, contextual widths, operand
order, range failures, and exact final stack shape. Commit either an established
value or a failed constant identity with diagnostic attribution. Dependent
uses cannot read a fabricated zero. Canonical constant values identify type
arguments. Generic constant parameters remain symbolic in semantic checking
and never enter named-initializer evaluation.

The source representation records derives, fields, trait requirements,
default bodies, and explicit implementations. Semantic construction checks
their contracts and creates structural conformances, projection/assignment
actions, and trait-owned generic recipes. Preserve callable field accessors and
their visibility where the language exposes them. If a callable target needs a
body, create a semantic body with field attribution, not a synthetic source
function passed through parsing and source-name resolution.

Derives and defaults follow ADR-0163 and ADR-0164. They do not inject source
functions, merge overlapping explicit implementations into derives, or clone
default source bodies per implementation. Their source occurrences appear once
in the editor index. Concrete generated targets remain the semantic engine's
responsibility before checked-program construction.

## Recovery and fact publication

The parser tracks the owner and nesting of each delimiter. Each declaration
header and structured region is built locally before its facts become usable.
On failure, recover at a closer owned by the damaged construct or at a new
declaration start whose enclosing scope is established. A declaration keyword
inside an unclosed function or literal is not a trustworthy top-level restart.
Discard the damaged subtree, retain its source range and diagnostic, and
continue only where delimiter ownership is known.

An incomplete header or uncertain declaration envelope produces a rejected
region, not a valid symbol. A complete header and established closing delimiter
can retain the written declaration and signature when an inner body region is
rejected. That source fact does not assert a checked body or inferred result.
Never publish nested declarations, closure functions, or accessors left over
from an abandoned builder.

| Failure | Retained product | Unavailable facts |
| --- | --- | --- |
| Read failure or conflicting request inputs | Report and available source context. Unread bytes have no fabricated source entry. | The affected source and facts depending on it. |
| Lexical damage | Exact bytes, diagnostics, recognized tokens, and independently delimited regions. | Invalid token contents and regions whose token boundaries cannot be established. |
| Malformed declaration or ambiguous delimiters | Earlier sound constructs and later constructs reached through a proven structural restart. | The incomplete declaration, uncertain scope, and dependent bindings. |
| Closed declaration with damaged inner body | Written declaration/signature and independent regions whose prerequisites are established. | Checked body, inferred facts crossing the damaged region, and dependent results. |
| Failed import | Import-site failure, retained loaded bytes and diagnostics, independent local facts and other successful imports. | Failed-module members and facts that require that dependency. |
| Failed constant, resolution, type, or ownership check | Established source declarations, name targets, and independent semantic facts. | The failed value or dependent semantic fact. No checked codegen product. |

Continue discovery through sound independent imports after an import failure.
Do not expand or resolve a dependent damaged construct merely because its text
was recognized. Track unavailable prerequisites with the affected facts so
editor queries distinguish incomplete coverage from complete empty answers.
ADR-0173 supplies the corresponding body-checking recovery.

If no structural restart can be established, the remainder of that source is
unavailable. A request can still own bytes, diagnostics, and facts from other
independent regions or modules. No usable root syntax product is promised when
lexical or syntax rejection occurred. The formatter returns original input
with failure status even when analysis can retain partial editor facts.

## Diagnostic encounter order and formatter facts

Keep diagnostics in deterministic compiler encounter order. When attaching new
diagnostics at a parsed import, partition existing diagnostics into those
attributed later in that same importing file and all remaining diagnostics.
Keep unlocated diagnostics and diagnostics from other files in the remaining partition.
Emit the remaining partition, the first-discovery child diagnostic stream,
then the deferred later-file partition. Preserve relative order within each
partition and child stream. The original parent stream need not follow source
order because lexing can report a later error before parsing finds an earlier
one. Apply the same rule recursively for nested imports. Imported diagnostic
ranges continue to identify the imported file.

Do not globally sort by path, severity, or source offset. Semantic diagnostics
follow deterministic source and worklist traversal, independent of hash-map
iteration.

Report a reused module's source diagnostics at its first discovery. A later
alias reuses its failure state instead of reparsing or replaying its diagnostic
stream. Reusing a module does not repeat partitioning. A new import-site failure
uses the encounter-insertion rule. A cycle diagnostic belongs to the edge that
closes the active path.
Analysis and assembly use the same source diagnostic construction.

The syntax operation invokes only source retention and shared grammar
recognition for its root. It loads no imports and constructs no semantic store.
Accepted syntax exposes lossless tokens, logical comment attachment, and
normalized grammar relationships for the formatter. Relationships use token
positions and ownership, not byte offsets that formatting changes. The neutral
name-plus-brace form is compared in that same representation.

Reparse formatter output through this operation and apply ADR-0172's token,
comment, and structure equivalence guard. Missing imports, unresolved names,
and semantic errors do not block syntax-only formatting. Delete the parser's
`syntax_only` mode rather than reproducing it as flags across the new parser.
