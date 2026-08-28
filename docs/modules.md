# Modules

Casa source files can load declarations from other files with the `import` directive.

## `import` Directive

`import` loads public declarations from another Casa source file. Declarations are private unless they use `pub`.

There are three forms:

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

Aliases and declarations cannot use the same source name. Importing two modules with one alias is also an error.

### Selective imports

```casa
import "path/to/tool.casa" as tool { parse_message DispatchState }
import "std" {
    List
    Map
}
```

Selective imports use the same resolution rules, then make the requested public declarations available without a qualifier. Private declarations cannot be selected.

- Function imports include referenced functions, constants, structs, enums, traits, and methods needed by the imported function body and function declaration.
- Struct and enum imports include generated accessors plus functions in their `impl` blocks.
- Constants can be imported directly.
- Public immutable globals can be imported directly. A selected declaration also includes private immutable globals and initializer helpers in its dependency closure.
- Top-level expressions, assignments, and bare calls in the imported file are skipped.
- Importing a function that depends on skipped top-level state is a compile error.
- Names referenced directly by the importing file must be listed explicitly.

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

### Import failures

An imported file must lex, parse, and resolve successfully before its declarations are added to the importing file. A failed full or selective import reports the imported file's diagnostics at the import position and stops further import expansion and identifier resolution.

Module imports must be acyclic. A cycle is a compile-time error that lists the complete resolved path from the first repeated module back to itself. The compiler visits dependencies in source order before their importers and processes repeated imports only once.

Imports do not run the imported root body. Full imports initialize the module's immutable globals. Selective imports initialize only globals in the selected declarations' dependency closure. Each physical global initializes once, even when the file is imported through more than one alias.

### `-L` / `--library-path`

Repeatable. Adds a directory to the module search path:

```sh
casac -L lib program.casa
casac -L lib -L vendor program.casa
```
