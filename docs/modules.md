# Modules

Casa source files can load declarations from other files with the `import` directive.

## `import` Directive

`import` loads public declarations from another Casa source file. Declarations are private unless they use `pub`.

There are two forms:

### Path-style

```casa
import "relative/path/to/file.casa" as file
import "/absolute/path/to/file.casa" as system_file
```

A specifier is treated as a path when it contains `/` or ends with `.casa`. A path-style import requires an `as` alias. Relative paths resolve from the directory of the importing file. Absolute paths are used as-is. No search is performed.

### Module-style

```casa
import "std"
import "parser" as syntax_parser
```

A specifier without `/` and without a `.casa` suffix is treated as a module name. The resolver looks for `<module>.casa` in:

1. the directory of the importing file, then
2. each directory passed via `-L` / `--library-path`, in CLI order.

The first existing match wins. Without `as`, the module specifier is also its namespace. A same-directory candidate that resolves to the importing file itself is skipped, so an example file `examples/argparse.casa` can `import "argparse"` and reach the library copy via `-L`. If no candidate exists, the compiler reports an error listing every directory searched.

### Qualified access

An ordinary import exposes public declarations through its namespace:

```casa
import "std"
import "../lib/parser.casa" as parser

std::List[i64]::new = values
parser::Cursor::new = cursor
```

`std` follows the same rule. A full `std` import does not add unqualified names.

Aliases and declarations cannot use the same source name. Importing two modules with one alias is also an error.

### Qualified names and module identity

Every imported declaration requires its namespace, including types, constants,
constructors, enum variants, and function references. Receiver calls such as
`value.method` keep their normal syntax. Imports never add bare names or
re-export another module's imports.

Selection clauses are no longer supported. Replace:

```casa
import "std" { List }
```

with:

```casa
import "std"

std::List[i64]::new = values
```

The compiler identifies a module by its normalized absolute path. Repeated
imports and different aliases for that path share declarations and types.
Repeating an alias for the same module is allowed. Symlinks are not resolved
when comparing module identities.

### Public declarations

`pub` can prefix functions, constants, structs, enums, traits, methods, and individual struct fields. Enum variants inherit the enum visibility. Generated field accessors inherit the field visibility.

```casa
pub const DEFAULT_LIMIT 10

pub struct Counter {
    pub value: i64
    secret:    i64
}

impl Counter {
    pub fn total self:$Counter -> i64 { self.secret self.value + }
}
```

Private declarations remain available to code in the same module. Imports do not re-export their dependencies.

Code outside the defining module can construct a struct only when every field is public. A public struct with private fields must provide a public factory function or method.

### Import failures

An imported file must lex, parse, and resolve successfully before its declarations are available to the importer. This includes unused declarations and root statements. A failed import reports the imported file's diagnostics at the import position and stops dependent resolution. Discovery continues through later independent imports. Failed modules are not loaded again under another alias. Declaration headers are collected before ordinary bodies are resolved.

Module imports must be acyclic. A cycle is a compile-time error that lists the complete resolved path from the first repeated module back to itself. The compiler visits dependencies in source order before their importers and processes repeated imports only once.

Imports do not run or typecheck the imported root body. They initialize the module's immutable globals. Each global initializes once, even when the file is imported through more than one alias.

### `-L` / `--library-path`

Repeatable. Adds a directory to the module search path:

```sh
casac -L lib program.casa
casac -L lib -L vendor program.casa
```

This option does not add a native linker search path. Use `-l` /
`--link-library` to name a native library.
