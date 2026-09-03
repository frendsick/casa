# Namespaced modules with private declarations by default
status: amended by [ADR-0165](0165-runtime-state-is-owned-by-the-root-body.md)

Casa modules will define namespaces instead of merging every imported declaration into one global namespace. Ordinary imports provide qualified access, declarations are private by default, and a declaration must be explicitly public to cross a module boundary. Selective imports remain the explicit way to bring public names directly into the importing scope.

## Considered options

- Flat import merging is syntactically small, but creates name collisions and makes module boundaries invisible as a program grows.
- Public-by-default namespaces avoid public modifiers, but expose helpers accidentally and make APIs harder to identify.
- Selective imports alone reduce some collisions, but do not provide a stable qualified identity for a declaration.

## Consequences

- Existing imports and cross-file declarations will require a source migration.
- A module-style import derives its namespace from the module specifier: `import "std"` binds `std`.
- A path-style import requires an explicit local alias: `import "../lib/parser.casa" as parser`.
- Casa does not add a separate `module` declaration; the importer cannot disagree with the resolved module identity.
- Private helpers cannot be selectively imported.
- `pub` may prefix types, functions, constants, methods, and individual struct fields. Enum variants inherit the enum's visibility, and generated field accessors inherit their field's visibility.
- Mutable globals no longer exist in safe code. Immutable globals follow ordinary declaration visibility and may be public; public constants remain allowed.
- Casa initially has no public re-export syntax.
- Inherent `impl` blocks may appear only in the defining module. Multiple blocks remain allowed there, and compiler-owned built-in types may be extended only by the core library.
- Qualified calls remain ordinary Casa composition; module isolation adds no runtime behavior.
