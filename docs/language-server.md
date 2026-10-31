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

Each entry acts like one `casac -L` path for module-style imports. Document
analysis resolves relative paths against the server's working directory.
Workspace analysis uses `/` as its request base. Use absolute paths so both
request types find the same libraries.

## Features

| Feature | Behavior |
|---|---|
| Completion | Names, keywords, intrinsics, dot methods, and qualified names |
| Definition | Functions, bindings, structs, enum variants, traits, and qualified methods |
| Diagnostics | Analyze on open and full-document change, and on save or local editor queries after inputs change |
| Formatting | Whole-document `casafmt` result for the open text |
| Hover | Types and stack effects for symbols, literals, operators, and intrinsics |
| References | Verified uses across discovered workspace roots and imports |
| Rename | Validated workspace edits for functions, bindings, structs, enums, enum variants, and traits |
| Semantic tokens | Full-document token classification |

Diagnostics expose the same error and warning codes as the compiler CLI.
Their messages include expected and actual values and all notes in compiler
encounter order. Notes with retained source text also appear as LSP
`relatedInformation`, including locations in imported files. Notes without
available source text remain in the message.
Diagnostics without a primary source location appear as `window/showMessage`
notifications instead of file diagnostics.
The notification text includes any available note locations.

Diagnostics located in an imported file appear under that file's URI. If
several open documents import the file, each distinct diagnostic appears once.
These diagnostics update when an importing document is analyzed again. They
clear when the error is fixed or when no open document imports the file. While
a file is open, only its own analysis publishes its diagnostics. That analysis
follows each edit of the file, while an importer's analysis can lag until the
importer is analyzed again. Closing a document hands its diagnostics to the
open documents that import it, or clears them. Before that, those documents
check that their analysis is current, because it may have used the closed
document's unsaved text.

`textDocument/formatting` formats the open text of the document with the same
pipeline as `casafmt`. The server returns one edit that replaces the whole
document. It returns no edit when the text is already formatted, when `casafmt`
would reject the text or its own output, or when the document is not open.
The formatter has no options, so the server ignores the request `options`.

Definitions and references can resolve imported declarations. Unsaved content
from other open Casa documents is included in analysis. Queries use a source
index built during analysis. Replacing a document releases its old snapshot.
Query results own their presentation text and source ranges.

Hover, definition, completion, and semantic-token requests first check that
the document's analysis is current. The server reads the imported files again,
resolves each import again, and compares the result with the analysis. If an
input changed, the server analyzes the document again and publishes diagnostics
before it returns the result. Otherwise the request uses the existing analysis.
Save notifications use the same check.

Each open document keeps the analysis of its imports between edits. The server
reads the imported files again on each analysis and reuses the earlier import
analysis only when their text, import resolution, library paths, and the
document's imports are unchanged. Closing the document releases it.

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

Malformed messages do not end the session. A request with missing or mistyped
parameters receives a JSON-RPC `-32602` error. A message body that is not valid
JSON receives a `-32700` error with a null id. Malformed notifications are
logged to standard error and skipped. Negative positions return no result.
A missing or negative `Content-Length` and the end of input end the session
because the server cannot find the next message.

An internal compiler failure during analysis appears as a `window/showMessage`
error. The server discards that document's previous analysis, so its queries
return no result until it analyzes again.

## Limitations

- Changes use full-document synchronization, not incremental edits.
- Completion is broad. The editor performs prefix filtering.
- Dot completion does not support every arbitrary expression.
- There are no code actions, range formatting, or on-type formatting requests.
- Rename validation is conservative. Syntax failures or diagnostics in a required
  root can prevent rename even when some references remain queryable.
- An open document whose imports report diagnostics gets no import reuse. Each
  edit then analyzes all of its imports again.
- Workspace requests reanalyze roots on each request. Large workspaces can be slow.
  Exclude generated files and intentionally invalid fixtures with `excludePaths`.

See [Compiler Diagnostics](errors.md) to interpret errors and [Casa Format
Guide](FORMAT.md) to format source files.
