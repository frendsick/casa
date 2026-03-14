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
| File saved | Diagnostics refreshed |

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
| Variable | First assignment of that variable (local first, then global) |
| Struct constructor | Struct definition |
| Enum variant | Enum definition |
| Type cast `(TypeName)` | Struct or enum definition |

Works across files. If a function or struct is defined in an included file, the editor will open that file.

### Hover

Hover over a symbol to see type and signature information in a code block.

| Symbol | Displayed info |
|--------|---------------|
| Function call or `&reference` | `fn name param:type -> return_type` |
| Variable | `name: type` |
| Struct constructor | `struct Name { field: type, ... }` |
| Enum variant | `EnumName::VariantName` |
| Integer, string, bool, or char literal | Type and value (e.g. `(int) 42`) |
| Assignment | `= name` |
| Operator or intrinsic | Name and stack effect (e.g. `+: int int -> int`) |

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
| Intrinsics | All intrinsics (`drop`, `dup`, `swap`, `alloc`, `print`, etc.) |

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

- Diagnostics update on open and save, not on every keystroke
- The server resets all compiler state between runs, so each diagnostics pass is a full recompilation
- Completion does not filter by prefix or context (the editor handles filtering)
- No rename, find references, or code actions
