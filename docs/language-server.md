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

| Feature | Behavior |
|---|---|
| Completion | Names, keywords, intrinsics, dot methods, and qualified names |
| Definition | Functions, bindings, structs, enum variants, and qualified methods |
| Diagnostics | Compile on open, full-document change, and save |
| Hover | Types and stack effects for symbols, literals, operators, and intrinsics |
| References | Verified uses across discovered workspace roots and imports |
| Rename | Validated workspace edits for functions and bindings |
| Semantic tokens | Full-document token classification |

Diagnostics expose the same error and warning codes as the compiler CLI.
Their messages include expected and actual values and all notes in compiler
encounter order. Notes with retained source text also appear as LSP
`relatedInformation`, including locations in imported files. Notes without
available source text remain in the message.
Diagnostics without a primary source location appear as `window/showMessage`
notifications instead of file diagnostics.
The notification text includes any available note locations.

Definitions and references can resolve imported declarations. Unsaved content
from other open Casa documents is included in analysis. Queries use a source
index built during analysis. Replacing a document releases its old snapshot.
Query results own their presentation text and source ranges.

Source errors can leave independent hover, definition, completion, reference,
and token facts available. Partial completion lists set `isIncomplete`.
Completion edits replace the selected identifier range.
References discover Casa files beneath `workspaceFolders` or `rootUri` on demand,
including unopened files and unsaved documents. No manifest is required. Directory
symlinks are not traversed. Add absolute files or directories to the initialization
option `excludePaths` to exclude them from discovery. Imported sources still
participate when required by an included root.

Requests analyze current open text and disk sources, correlate declaration ranges
and exact source revisions, and deduplicate uses. Incomplete discovery, failed
imports, and missing semantic facts produce a visible partial-reference notice.
Conflicting bindings across roots are excluded. Local binding queries use their
containing compilation without workspace discovery.

Rename requires complete coverage, a valid identifier, and candidate reanalysis
that preserves established bindings. It rejects collisions, unavailable validation
facts, and edits outside the included writable workspace. Open documents receive
versioned `documentChanges`. Closed documents receive a null version after an
exact disk-content check. The client must support versioned document changes.
Open text remains authoritative on save. Closing a document removes its override.

The server processes requests synchronously and uses fresh analyses for workspace
queries. It refuses publication when newer client input is pending. This avoids
applying old ranges while a queued edit waits. There is no background workspace
scan or debounce delay. File changes after the final disk check remain subject
to the client's edit application behavior.

## Limitations

- Diagnostics from imported files are not published. Open the imported file to
  see its diagnostics.
- Changes use full-document synchronization, not incremental edits.
- Completion is broad. The editor performs prefix filtering.
- Dot completion does not support every arbitrary expression.
- There are no code actions or formatting requests.
- Type rename remains unavailable until signature and field type references have
  complete coverage.
- Rename validation is conservative. Syntax failures or diagnostics in a required
  root can prevent rename even when some references remain queryable.
- Workspace requests reanalyze roots on each request. Large workspaces can be slow.
  Exclude generated files and intentionally invalid fixtures with `excludePaths`.

See [Compiler Diagnostics](errors.md) to interpret errors and [Casa Format
Guide](FORMAT.md) to format source files.
