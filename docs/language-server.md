# Language Server

Casa includes a Language Server Protocol (LSP) server for editor diagnostics
and navigation.

## Build

Build a current compiler first. This avoids a mismatch between the installed
bootstrap compiler and the source tree:

```sh
./casac casa.casa -o casac-next -L lib
./casac-next lsp.casa -o casa_lsp -L lib
```

Use an absolute path to `casa_lsp` in editor configuration. The server uses
standard input and output. Do not start it in a terminal for normal use.

## Neovim

Add this to `init.lua`. Replace both absolute paths:

```lua
vim.filetype.add({ extension = { casa = "casa" } })

vim.api.nvim_create_autocmd("FileType", {
  pattern = "casa",
  callback = function()
    vim.lsp.start({
      name = "casa",
      cmd = { "/absolute/path/to/casa_lsp" },
      root_dir = vim.fs.root(0, { ".git" }) or vim.fn.getcwd(),
      init_options = {
        libraryPaths = { "/absolute/path/to/casa/lib" },
      },
    })
  end,
})
```

## Helix

Add this to `~/.config/helix/languages.toml`. Replace both absolute paths:

```toml
[language-server.casa]
command = "/absolute/path/to/casa_lsp"
config = { libraryPaths = ["/absolute/path/to/casa/lib"] }

[[language]]
name = "casa"
scope = "source.casa"
file-types = ["casa"]
language-servers = ["casa"]
```

## VS Code

This repository does not include a VS Code extension. VS Code needs an
extension to start and connect to a language server, so workspace settings
alone are not sufficient.

## Library paths

Clients can send this initialization option:

```json
{
  "libraryPaths": ["/absolute/path/to/casa/lib"]
}
```

Each entry acts like one `casac -L` path for module-style imports. Relative
paths depend on the editor's server working directory, so absolute paths are
safer.

## Features

| Feature | Current behavior |
|---|---|
| Diagnostics | Compile on open, full-document change, and save |
| Definition | Functions, bindings, structs, enum variants, and qualified methods |
| Hover | Types and stack effects for symbols, literals, operators, and intrinsics |
| Completion | Names, keywords, intrinsics, dot methods, and qualified names |
| References | Functions, bindings, structs, and enums |
| Rename | Functions and bindings across analyzed source locations |
| Semantic tokens | Full-document token classification |

Definitions and references can resolve imported declarations. Unsaved content
from other open Casa documents is included in analysis.

## Limitations

- Diagnostics from imported files are not published. Open the imported file to
  see its diagnostics.
- Changes use full-document synchronization, not incremental edits.
- Completion is broad. The editor performs prefix filtering.
- Dot completion does not support every arbitrary expression.
- There are no code actions or formatting requests.
- The server recompiles the document and does not free all session allocations,
  so memory use can grow during a long session.

See [Compiler Diagnostics](errors.md) to interpret errors and [Casa Format
Guide](FORMAT.md) to format source files.
