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

- No completion, hover, or go-to-definition (diagnostics only)
- Diagnostics update on open and save, not on every keystroke
- The server resets all compiler state between runs, so each diagnostics pass is a full recompilation
