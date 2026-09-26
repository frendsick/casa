# Imports expose qualified names only

related issue: [Choose the selective-import contract](https://github.com/frendsick/casa/issues/656).

The maintainer selected qualified-only imports and no source-level code-size
guarantee on 2026-09-26. Importing a module exposes its public declarations through
a local namespace. Remove selective import clauses and their dependency-retention
contract. This gives imports one name-access rule and removes dependency discovery
from import selection. Production migration remains pending. Current behavior was
checked against `62a46a6`, the `origin/main` revision used for this decision.

## Source contract

Keep these forms:

```casa
import "std"
import "parser" as syntax_parser
import "../lib/tool.casa" as tool
import "/absolute/path/tool.casa" as other_tool

std::List[i64]::new = values
tool::parse_message
```

A module-style import uses its specifier as the namespace unless `as` supplies an
alias. A path-style import requires an explicit alias. Imported functions,
constants, types, constructors, enum variants, and explicit associated references
use that namespace, including in type annotations and function references. Normal
receiver method and field syntax remains available. An import never adds bare
declaration names to the importing scope. This rule also applies to `std`.

Reject every `{ ... }` selection clause, including empty and multiline clauses,
with a diagnostic at the clause that gives a replacement. For example, replace
`import "std" { List }` with `import "std"` and use `std::List`. After an explicit
alias, use that alias in the suggested qualified name. There is no compatibility
mode that accepts the retired syntax.

Declarations remain private by default. `pub` controls cross-module access to
declarations, methods, and fields. Enum variants inherit their enum's visibility.
Private helpers and transitive dependencies remain available inside their defining
modules but cannot be named by an importer. Imports do not re-export dependencies.
Public structs with private fields still require a public construction interface.

Two modules may export the same declaration name because their namespaces differ.
An alias cannot collide with a declaration in the importing scope or name two
different modules. Diagnose the conflicting source name at the offending import
or declaration. Repeating the same alias for the same resolved module is allowed.
Different aliases for one module refer to the same declarations and types, not
copies of that module.

## Resolution and validation

Preserve ordered lookup. A specifier containing `/` or ending in `.casa` is a path.
Relative paths resolve from the importing file's directory. Absolute paths are used
directly. Paths are not searched. Bare module names search the importing directory,
then `-L` / `--library-path` directories in CLI order. The first existing candidate
wins. Preserve the same-directory self-match exception and missing-module
diagnostics that list the searched directories.

Canonical module identity follows the resolved, normalized absolute path within
one compilation, independently of the alias or path spelling. Relative and
absolute spellings that normalize to that path share one module. This does not add
filesystem identity through symlink or inode resolution. Matching in-memory source
overrides take precedence over disk contents and can supply modules absent on disk.
Retain the exact selected source text for diagnostics and editor facts, following
[ADR-0167](0167-compiler-products-own-independent-snapshots.md).

Keep full-import validation. Every imported source must lex, parse, and resolve,
including its root statements and unused declarations. All imported declarations
undergo the ordinary semantic checks that apply to them, even when the importer
does not use them. Generic declarations follow the generic contract. This does not
require instantiating every possible type argument. Imported root statements are
resolved but are not part of the importing program's executable or typechecked
root body.

Reject declaration-only cycles as well as other import cycles, with the complete
resolved path that closes the cycle. Discover dependencies in source order and
reuse a resolved module within the request. Preserve diagnostic encounter order
and source locations. Failed import loading, lexing, parsing, or resolution stops
dependent expansion and resolution. Source errors prevent assembly publication.
The tooling decision retains responsibility for partial editor facts after errors.

Imports execute no module code, as required by
[ADR-0165](0165-runtime-state-is-owned-by-the-root-body.md). They do not initialize
runtime globals or establish an initializer schedule. Runtime state is constructed
by root execution and passed explicitly.

Visibility does not specify emitted reachability or binary size. The compiler or
linker may remove unused declarations as an optimization. That optimization must
preserve the language's validation and runtime behavior, and does not justify
restoring selected-name syntax.

## Alternatives and cost

Visibility-only selection would preserve short unqualified names without retaining
dependency pruning. It still needs selection grammar, selected-name bindings,
visibility checks, and collision diagnostics. Qualified-only imports give up that
convenience. Current selective imports additionally require requested roots,
dependency closure, retention state, and selection-specific failure handling.

The [front-end audit](../audits/front-end-complexity.md) identified 350 to 380 lines
of obsolete result and merge machinery removable without a language change. Remove
that floor from the 929-line selective-import module before comparing options. The
remaining module accounts for approximately 549 to 579 lines, plus related syntax
and state machinery. Those figures are a scope estimate, not a measured migration
saving. Some work belongs to runtime-global removal or the module-seam redesign.
The 718-line closure test also overlaps the audit's existing test-deletion estimate.
Do not count either whole file again as an additional architecture gain.

Compiler and library sources already use ordinary qualified imports. This reduces
their source-migration cost, but the choice sacrifices selected-name convenience
for public users regardless of repository usage. No compile-time, memory, or binary
size improvement has been measured for this decision.

## Atomic migration

Deliver the source, reference-documentation, example, and test changes together:

1. Remove selection parsing and binding, selective import operations, closure
   discovery, retention state, and obsolete merge interfaces. Keep the shared
   module loader, privacy checks, source owner, cycle detection, and diagnostics.
   The front-end seam may replace their current representation. Normalize source
   identities and override keys together against the request base directory.
   Current overrides can retain relative keys. Keep operation semantics still
   needed by semantic analysis.
2. Replace each selection clause with an ordinary import and qualify every name
   that depended on it, including nested imports, types, function references,
   constants, constructors, and explicit method or trait references. Keep normal
   receiver calls. Coordinate runtime-global sources and fixtures with ADR-0165
   so the completed migration has no imported initialization path.
3. Update `docs/modules.md`, affected language examples, formatter fixtures, and
   executable sources in the same change. Until that migration, the reference
   docs and tests continue to describe implemented selective-import behavior.
4. Delete retired closure and merge protocol tests. Convert useful behavior cases
   from selective-import tests to qualified imports through production entry
   points. Remove initializer selection and scheduling tests with runtime globals.
   Do not replace them with tests that merely assert deleted internals are absent.
5. Retain focused production-entry checks for public qualified access, aliases,
   unqualified-name rejection, privacy and private transitive helpers, collisions,
   module validation, complete cycle paths, ordered lookup, canonical identity,
   repeated imports, in-memory overrides, diagnostics, and import non-execution.
   Cover the removed syntax diagnostic through compiler analysis and syntax-only
   formatter validation. Formatting valid imports must not require dependencies
   to exist on disk.

The current affected paths are `compiler/syntax.casa`,
`compiler/selective_import.casa`, import operation definitions in
`compiler/common.casa`, and their consumers. Existing behavior checks live in
`tests/compiler/test_modules.casa`, `test_parser.casa`, `test_selective_import*.casa`,
compiler error fixtures, and formatter tests. Use the surviving analysis and syntax
entry points rather than exposing new closure or cache protocols to tests.

The completed compiler migration must pass focused import and formatter checks,
the full CI suite, and bootstrap fixed-point validation. This decision adds no new
source syntax that requires a compiler release today.

This amends the selective-import provisions of ADR-0008 and ADR-0010. The single
authoritative operation-semantics principle, namespace privacy, and the separate
runtime-global removal decision remain in force. Exact module representation and
semantic algorithms remain with the existing front-end and semantic-seam tickets.
