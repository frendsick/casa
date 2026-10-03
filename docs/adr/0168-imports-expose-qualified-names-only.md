# Imports expose qualified names only

related issue: [Choose the selective-import contract](https://github.com/frendsick/casa/issues/656).

Importing a module exposes its public declarations through a local namespace.
Qualified-only imports remove selective clauses and their dependency-retention
contract. Visibility gives no source-level code-size guarantee.

[Compiler products](../compiler-products.md) describe the delivered interfaces.

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

Casa adds no separate `module` declaration. An import alias cannot change the
resolved module identity.

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

Trait implementations follow the either-owner orphan rule in
[ADR-0041](0041-trait-implementation-obeys-an-orphan-rule.md).

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

Flat import merging creates name collisions and hides module boundaries as a
program grows. Public-by-default namespaces expose helpers accidentally and
make APIs harder to identify. Private-by-default declarations with explicit
public access keep the module's intended interface visible.

Visibility-only selection would preserve short unqualified names without retaining
dependency pruning. It still needs selection grammar, selected-name bindings,
visibility checks, and collision diagnostics. Qualified-only imports give up that
convenience without promising compile-time, memory, or binary-size improvements.

## Validation

Production analysis and syntax checks retain public qualified access, aliases,
unqualified-name rejection, privacy and private dependencies, collisions,
full-import validation, complete cycle paths, ordered lookup, canonical identity,
repeated imports, overrides, diagnostic order, and import non-execution.
Check the removed-clause diagnostic in both analysis and syntax-only formatting.
Formatting valid imports does not require dependencies to exist on disk.
