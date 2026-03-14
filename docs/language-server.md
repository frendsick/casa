# Language Server

Casa includes an LSP language server that provides real-time compiler diagnostics in any editor that supports the [Language Server Protocol](https://microsoft.github.io/language-server-protocol/).

## Setup

Install the `pygls` LSP framework in the project virtualenv:

```sh
python3.13 -m venv .venv
.venv/bin/pip install pygls
```

## Running

The server communicates over stdio:

```sh
.venv/bin/python casa_ls.py
```

## Features

### Diagnostics

The server runs the full compiler pipeline (lex, parse, resolve, type check) on the open file and reports any errors or warnings as LSP diagnostics.

| Event | Action |
|-------|--------|
| File opened | Diagnostics published |
| File changed | Diagnostics refreshed from buffer content |
| File saved | Diagnostics refreshed from disk |

Errors appear with severity **Error** and warnings with severity **Warning**. All diagnostics include the source location (line, column, span) so the editor can underline the problematic code.

When a file uses `include`, only diagnostics originating from the open file are shown. Errors in included files are filtered out.

### Error details

Diagnostics carry the error message from the compiler. When the compiler provides expected/got information (e.g. type mismatches), it is appended to the message:

```
type mismatch
Expected: int
Got: str
```

See [Errors](errors.md) for the full list of error kinds and their descriptions.

### Go to Definition

Jump to the definition of a symbol under the cursor. Supported symbols:

| Symbol | Target |
|--------|--------|
| Function call or `&reference` | Function declaration |
| Variable | Most recent assignment before usage (local first, then global) |
| Struct constructor | Struct definition |
| Enum variant `Enum::Variant` (cursor on enum) | Enum definition |
| Enum variant `Enum::Variant` (cursor on variant) | Variant definition in enum body |
| Type cast `(TypeName)` | Struct or enum definition |
| Qualified name `Type::method` (cursor on type) | Struct or enum definition |
| Qualified name `Type::method` (cursor on method) | Method definition |

Works across files. If a function or struct is defined in an included file, the editor will open that file.

For qualified names like `Point::get_x`, the server checks where the cursor falls relative to the `::` separator. Clicking on `Point` navigates to the struct definition, while clicking on `get_x` navigates to the method definition.

### Hover

Hover over a symbol to see type and signature information in a code block.

| Symbol | Displayed info |
|--------|---------------|
| Function call or `&reference` | `fn name param:type -> return_type` |
| Variable | `name: type` |
| Struct constructor | `struct Name { field: type, ... }` |
| Enum variant | `EnumName::VariantName` |
| Integer, string, bool, or char literal | Type and value (e.g. `(int) 42`) |
| Assignment | `= name: type` (with inferred type) |
| Operator | Name and stack effect (e.g. `+: int int -> int`) |
| Intrinsic | Name and stack effect (e.g. `drop: any -> None`) |

All operators and intrinsics show their stack effects on hover, including arithmetic, comparison, boolean, bitwise, stack manipulation, memory, syscall, and IO operations.

### Completion

Trigger completion to get context-aware suggestions. The server provides the following completion items:

| Kind | Items |
|------|-------|
| Functions | All user-defined functions (excluding internal lambdas) with signatures |
| Structs | Struct names with member details |
| Enum variants | All `EnumName::Variant` entries |
| Local variables | Variables from the function the cursor is inside, with types |
| Global variables | All global variables with types |
| Keywords | All Casa keywords (`fn`, `if`, `while`, `struct`, `enum`, etc.) |
| Intrinsics | All intrinsics (`drop`, `dup`, `swap`, `alloc`, `print`, etc.), shown as keywords |

#### Dot-triggered method completion

Typing `.` after a variable triggers method completion. The server looks up the variable's type and offers all methods defined for that type.

For example, after `p.` where `p` is a `Point`, the server offers `get_x`, `set_x`, etc. from the `Point` impl block. Method completions use the short name (e.g. `get_x` not `Point::get_x`) and appear with `Method` kind.

This works for generic types too. If `items` is a `List[int]`, typing `items.` shows methods from `List::*`.

Method chaining is supported. For example, `test.hash.` resolves `test` to its type, looks up the return type of `hash`, and offers methods for that return type.

#### Qualified name completion

Typing `::` after a type name triggers qualified name completion. The server suggests enum variants and methods for that type.

For example, after `Color::` the server offers `Red`, `Green`, `Blue` (enum variants) and any methods from `Color`'s impl block. After `str::` it offers string methods like `length`, `at`, `concat`, etc.

### Find References

Find all references to a symbol across the codebase. Supported symbols:

| Symbol | Found references |
|--------|-----------------|
| Function | All call sites and `&references`, plus the definition |
| Variable | All usages and assignments (local variables search only the containing function) |
| Struct | All constructor usages and type casts |
| Enum | All variant usages |

The `include_declaration` context flag controls whether the definition itself is included in the results.

### Rename

Rename a symbol and all its references across the file. Supported symbols:

| Symbol | Renamed locations |
|--------|-----------------|
| Function | Definition and all call sites |
| Variable | Assignment and all usage sites |

For assignment operators (`=`, `+=`, `-=`), the rename correctly targets only the variable name portion of the span, not the operator prefix.

### Semantic Tokens

The server provides full semantic token highlighting. Each token is classified into one of the following types:

| Token type | Colored elements |
|------------|-----------------|
| `function` | Function calls, `&references`, function definition names |
| `variable` | Variable reads, captures, assignments |
| `string` | String literals |
| `number` | Integer and character literals |
| `keyword` | `fn`, `if`, `while`, `match`, `true`, `false`, `none`, `some`, `ok`, `error`, `exec`, `return`, `break`, `continue` |
| `operator` | Arithmetic, comparison, boolean, and bitwise operators |
| `type` | Type casts `(TypeName)` |
| `enumMember` | Enum variant references |
| `struct` | Struct constructors |
| `macro` | Intrinsics (`drop`, `dup`, `swap`, `alloc`, `print`, `load8`, `store64`, `syscall3`, `typeof`, etc.) |

Intrinsics are highlighted as `macro` to visually distinguish them from user-defined functions. The `fn` keyword before function definitions is highlighted as a keyword.

## Editor Configuration

### Neovim (nvim-lspconfig)

```lua
require('lspconfig.configs').casa = {
  default_config = {
    cmd = { '.venv/bin/python', 'casa_ls.py' },
    filetypes = { 'casa' },
    root_dir = function(fname)
      return vim.fs.dirname(fname)
    end,
  },
}
require('lspconfig').casa.setup({})
```

### VS Code

Add a `.vscode/settings.json` entry or use a generic LSP client extension (e.g. `vscode-lsp-client`) with:

```json
{
  "command": ".venv/bin/python",
  "args": ["casa_ls.py"],
  "filetypes": ["casa"]
}
```

### Helix

Add to `~/.config/helix/languages.toml`:

```toml
[[language]]
name = "casa"
scope = "source.casa"
file-types = ["casa"]
language-servers = ["casa-ls"]

[language-server.casa-ls]
command = ".venv/bin/python"
args = ["casa_ls.py"]
```

## Limitations

- Diagnostics update on open, change, and save
- The server resets all compiler state between runs, so each diagnostics pass is a full recompilation
- Completion does not filter by prefix or context (the editor handles filtering)
- Dot-triggered method completion works for variables, literals, and method chains but not for arbitrary expressions
- No code actions
