# Casa

**A statically typed, stack-based programming language with functional influences.**

Casa compiles to x86-64 Linux executables via GNU assembly. It features static type inference, first-class functions, closures, structs with methods, and a stack-based execution model.

`"Hello, world!" print`

## Features

- **Stack-based** — values live on the stack; operators consume and produce stack entries
- **Statically typed** — types are checked at compile time with automatic inference and generics
- **First-class functions** — lambdas and function references (`&name`) are values on the stack
- **Structs and methods** — user-defined types with auto-generated accessors and `impl` blocks
- **String interpolation** — f-strings with embedded expressions (`f"hello {name}"`)
- **Compiles to native code** — generates x86-64 assembly with `ld` and `as`
- **Enums and match** — enum types with exhaustive pattern matching
- **Traits** — structural trait system with bounded polymorphism (`trait`, `Hashable`, `Display`)
- **Standard library** — generic `List[T]`, `Map[K V]`, `Set[K]`, arrays with `map`/`filter`/`reduce`, type conversions, and memory utilities

## Requirements

- **Linux x86-64**
- **GNU assembler** (`as`) and **linker** (`ld`) — typically from the `binutils` package

## Installation

Clone the repository and run the install script to download the latest compiler binary:

```sh
git clone https://github.com/frendsick/casa.git
cd casa
./install.sh
```

Or download and run the install script directly:

```sh
curl -sSL https://raw.githubusercontent.com/frendsick/casa/main/install.sh | sh
```

## Getting Started

Compile and run a program:

```sh
./casac examples/hello_world.casa -r
# Hello world!
```

Compile with a custom output name:

```sh
./casac examples/fibonacci.casa -o fib
./fib
# 6765
```

### Building the Compiler from Source

The compiler is self-hosted. From the repo root, build it with the existing `casac`:

```sh
./casac casa.casa -o casac -L lib
```

The `-L lib` flag adds `lib/` to the module import search path so `compiler/` sources can resolve `import "std"` and similar module-style imports.

## CLI Usage

```
./casac <file> [-o name] [-L path]... [--keep-asm] [-r] [-v]
```

| Flag | Description |
|------|-------------|
| `-o`, `--output` | Output binary name (default: input file stem) |
| `-L`, `--library-path` | Add a directory to the module import search path (repeatable, e.g. `-L lib`) |
| `--keep-asm` | Keep the generated `.s` assembly file |
| `-r`, `--run` | Execute the binary after compilation |
| `-v`, `--verbose` | Enable verbose logging |

## Compilation Pipeline

```
source → lex → parse → resolve → type check → bytecode → emit asm → assemble → link → executable
```

## Documentation

Start with the core language docs (1-8), then explore the library and tooling docs as needed.

### Core Language

| # | Document | Topics |
|---|----------|--------|
| 1 | [Types and Literals](docs/types-and-literals.md) | Primitive types, composite types, literals, type casting, comments |
| 2 | [Operators](docs/operators.md) | Stack-based evaluation, arithmetic, comparison, boolean, assignment |
| 3 | [Functions and Lambdas](docs/functions-and-lambdas.md) | Functions, lambdas, closures, variables |
| 4 | [Control Flow](docs/control-flow.md) | Conditionals, loops, `for` iterators, `match` pattern matching |
| 5 | [Structs and Methods](docs/structs-and-methods.md) | Struct definition, accessors, `impl` blocks, dot/arrow syntax |
| 6 | [Enums](docs/enums.md) | Enum types, variant constructors, `is` destructuring |
| 7 | [Traits](docs/traits.md) | Trait definitions, structural satisfaction, trait bounds, `Hashable` |
| 8 | [Built-in Intrinsics](docs/intrinsics.md) | Stack manipulation, IO, memory, and syscall intrinsics |

### Library

| Document | Topics |
|----------|--------|
| [Standard Library](docs/standard-library.md) | `import`, `memcpy`, arrays (`map`, `filter`, `reduce`), `Option[T]`, `Result[T E]` |
| [Collections](docs/collections.md) | `List[T]`, `Map[K V]`, `Set[K]`, `StringBuilder` |
| [Strings and IO](docs/strings-and-io.md) | String methods, file I/O, character classification, type conversions |
| [Utilities](docs/utilities.md) | Logging, timer, argument parsing, process execution |
| [Parser Library](docs/parser.md) | `Cursor`, `ParseError`, scanning methods, string/int/identifier parsers |

### Tooling

| Document | Topics |
|----------|--------|
| [Errors](docs/errors.md) | Error kinds, diagnostics format, multi-error collection |
| [Language Server](docs/language-server.md) | LSP setup, editor configuration, diagnostics |

## Examples

| File | Description |
|------|-------------|
| [hello_world.casa](examples/hello_world.casa) | Print a string |
| [fizzbuzz.casa](examples/fizzbuzz.casa) | FizzBuzz with functions, variables, and conditionals |
| [hash_map.casa](examples/hash_map.casa) | `Map[K V]` and `Set[K]` with the `Hashable` trait |
| [enum.casa](examples/enum.casa) | Enum types with `match` pattern matching |
| [parser.casa](examples/parser.casa) | Cursor-based text scanning and parsing |
| [game_of_life.casa](examples/game_of_life.casa) | Conway's Game of Life with terminal rendering and resize handling |

More examples: [examples](./examples/)

## Editor Integration

Casa ships with an LSP language server written in Casa. It provides diagnostics, go-to-definition, hover, completion, references, rename, and semantic tokens.

### Building the Language Server

Compile the LSP server with the Casa compiler from the repo root:

```sh
./casac lsp.casa -o casa_lsp -L lib
```

### Editor Configuration

The server communicates over stdio. Point your editor's LSP client at the compiled binary. For example, in Neovim with `nvim-lspconfig`:

```lua
require('lspconfig.configs').casa = {
  default_config = {
    cmd = { '/path/to/casa/casa_lsp' },
    filetypes = { 'casa' },
    root_dir = function(fname)
      return vim.fs.dirname(fname)
    end,
  },
}
require('lspconfig').casa.setup({})
```

### Supported Features

| Feature | Trigger |
|---------|---------|
| Error/warning diagnostics | File open, file save |
| Go-to-definition | `gd` or equivalent |
| Hover | Hover over identifier |
| Completion | Typing |
| Find references | `gr` or equivalent |
| Rename | `<leader>rn` or equivalent |
| Semantic tokens | Automatic |

## Formatter

Casa ships with `casafmt`, a self-hosted autoformatter. It reads Casa source from stdin and writes formatted output to stdout. On parse error it echoes the original source unchanged and exits with code 1.

### Building the Formatter

From the repo root:

```sh
./casac formatter/format.casa -o casafmt -L lib
```

### Manual Usage

```sh
./casafmt < file.casa > tmp && mv tmp file.casa
```

A temporary file is used because shell redirection (`> file.casa`) truncates the file before the formatter reads it. Writing to a temp file first keeps the original intact if the formatter fails.

### Neovim Integration

Any formatter plugin that supports stdin-based formatters will work. For example, with [conform.nvim](https://github.com/stevearc/conform.nvim):

```lua
require('conform').setup({
  formatters_by_ft = {
    casa = { 'casafmt' },
  },
  formatters = {
    casafmt = {
      command = '/path/to/casa/casafmt',
      stdin = true,
    },
  },
})
```

See [FORMAT.md](docs/FORMAT.md) for the full list of formatting rules.

## Testing

Run the example and compiler test suites from the root folder of the repository:

```sh
./tests/test_examples.sh
./tests/test_compiler.sh
```

Both scripts default to the `./casac` binary downloaded by `install.sh`. Pass an alternate compiler path as the first argument to use a different binary.
