# Casa

Casa is a statically typed, stack-based programming language for Linux.

```casa
"Hello, world!\n" print
```

## Requirements

- Linux on x86-64
- GNU assembler (`as`) and linker (`ld`)
  - For example, on Ubuntu, install the `binutils` package.

## Install

Clone the repository and download the compiler:

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

Start with the [Casa guide](docs/guide.md).

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
| Optional values and errors | [Optional Values and Errors](docs/optional-values-and-errors.md) |
| Collections and iterators | [Collections](docs/collections.md) |
| Text, characters, and output | [Text and Characters](docs/strings-and-io.md) |
| Files, directories, environment, and processes | [Operating-System APIs](docs/os.md) |
| Logging, timing, arguments, JSON, and parsing | [Specialist Libraries](docs/utilities.md) |
| Parser building blocks | [Parser Library](docs/parser.md) |

Tooling:

- [Compiler diagnostics](docs/errors.md)
- [Language server](docs/language-server.md)
- [Formatter usage and rules](docs/FORMAT.md)
- [Casa style](docs/STYLE.md)

See the [curated examples](examples/README.md) for runnable programs ordered
from introductory to advanced.

## Build from source

Casa is self-hosted. Build the compiler with an existing `casac`:

```sh
./casac casa.casa -o casac -L lib
```
