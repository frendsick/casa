# Modules

Casa source files can load declarations from other files with the `import` directive.

## `import` Directive

`import` loads another Casa source file. Each file is imported at most once, regardless of how many times it appears, and the deduplication uses the canonicalized resolved path.

There are three forms:

### Path-style

```casa
import "relative/path/to/file.casa"
import "/absolute/path/to/file.casa"
```

A specifier is treated as a path when it contains `/` or ends with `.casa`. Relative paths resolve from the directory of the importing file. Absolute paths are used as-is. No search is performed.

### Module-style

```casa
import "std"
```

A specifier without `/` and without a `.casa` suffix is treated as a module name. The resolver looks for `<module>.casa` in:

1. the directory of the importing file, then
2. each directory passed via `-L` / `--library-path`, in CLI order.

The first existing match wins. A same-directory candidate that resolves to the importing file itself is skipped, so an example file `examples/argparse.casa` can `import "argparse"` and reach the library copy via `-L`. If no candidate exists, the compiler reports an error listing every directory searched.

### Selective imports

```casa
import "path/to/tool.casa" { parse_message DispatchState }
import "std" {
    List
    Map
}
```

Selective imports use the same path-style and module-style resolution rules as bare imports, then extract only the named declarations and the transitive dependencies needed by those declarations.

- Function imports include referenced functions, constants, structs, enums, traits, and methods needed by the imported function body and signature.
- Struct and enum imports include generated accessors plus functions in their `impl` blocks.
- Constants can be imported directly.
- Top-level expressions and bare calls in the imported file are skipped.
- Top-level global assignments are skipped and cannot be imported.
- Importing a function that depends on a skipped global variable is a compile error.
- Names referenced directly by the importing file must be listed explicitly.

### `-L` / `--library-path`

Repeatable. Adds a directory to the module search path:

```sh
casac -L lib program.casa
casac -L lib -L vendor program.casa
```
