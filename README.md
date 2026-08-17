# Casa

Casa is a statically typed, stack-based programming language that compiles to
x86-64 Linux executables. Programs compose small functions through a typed value
stack.

```casa
"Hello, world!\n" print
```

## Requirements

- Linux on x86-64
- GNU assembler (`as`) and linker (`ld`)
  - For example, on Ubuntu, install the `binutils` package.

## Install

Clone the repository and download the compiler configured by
`casa-release.env`:

```sh
git clone https://github.com/frendsick/casa.git
cd casa
./install.sh
```

You can also run the installer directly:

```sh
curl -sSL https://raw.githubusercontent.com/frendsick/casa/main/install.sh | sh
```

## Run a program

Compile and run the hello-world example:

```sh
./casac examples/hello_world.casa -r
```

Compile without running:

```sh
./casac examples/fibonacci.casa -o fib
./fib
```

Common compiler options:

| Option | Purpose |
|---|---|
| `-o`, `--output` | Set the output binary name |
| `-L`, `--library-path` | Add a module search directory |
| `-r`, `--run` | Run the program after compilation |
| `--keep-asm` | Keep the generated assembly file |
| `-v`, `--verbose` | Print compiler progress |

## Learn Casa

Start with the [Casa guide](docs/guide.md). It builds one program while
introducing the value stack, operand order, bindings, functions, control flow,
imports, and collections.

Use the topic references when you need exact behavior:

| Topic | Reference |
|---|---|
| Values and types | [Types and Literals](docs/types-and-literals.md) |
| Stack evaluation and operators | [Operators](docs/operators.md) |
| Bindings, functions, and lambdas | [Functions and Lambdas](docs/functions-and-lambdas.md) |
| Branches, loops, and matching | [Control Flow](docs/control-flow.md) |
| Structs and methods | [Structs and Methods](docs/structs-and-methods.md) |
| Enums and patterns | [Enums](docs/enums.md) |
| Generics and traits | [Traits](docs/traits.md) |
| Imports | [Modules](docs/modules.md) |
| Compiler operations | [Built-in Intrinsics](docs/intrinsics.md) |

Library references:

| Topic | Reference |
|---|---|
| Arrays, iterators, `Option`, and `Result` | [Standard Library](docs/standard-library.md) |
| Lists, maps, and sets | [Collections](docs/collections.md) |
| Text and operating-system I/O | [Strings and IO](docs/strings-and-io.md) |
| Logging, timing, arguments, and processes | [Utilities](docs/utilities.md) |
| Parser building blocks | [Parser Library](docs/parser.md) |

Tooling:

- [Compiler diagnostics](docs/errors.md)
- [Language server](docs/language-server.md)
- [Formatter rules](docs/FORMAT.md)
- [Casa style](docs/STYLE.md)

See [`examples/`](examples/) for runnable programs.

## Build from source

Casa is self-hosted. Build the compiler with an existing `casac`:

```sh
./casac casa.casa -o casac -L lib
```

Build the language server and formatter in the same way:

```sh
./casac lsp.casa -o casa_lsp -L lib
./casac formatter/format.casa -o casafmt -L lib
```

Contributor conventions are in [FORMAT.md](docs/FORMAT.md) and
[STYLE.md](docs/STYLE.md).
