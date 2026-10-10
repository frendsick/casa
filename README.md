# Casa

Casa is a statically typed, stack-based programming language for Linux.

```casa
"Hello, world!\n" print
```

## Requirements

- Linux on x86-64
- A C compiler driver at `/usr/bin/cc`, with GNU assembler and linker tools
  - For example, on Ubuntu, install the `build-essential` package.

## Install

Clone the repository and download the compiler and formatter:

```sh
git clone https://github.com/frendsick/casa.git
cd casa
./install.sh
```

To download only `casac` and `casafmt` into the current directory, run the
installer directly. This does not download the standard library or examples:

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
| `--keep-asm` | Keep the generated assembly file |
| `-L`, `--library-path` | Add a module search directory |
| `-l`, `--link-library` | Link a native library |
| `-o`, `--output` | Set the output binary name |
| `-r`, `--run` | Run the program after compilation |
| `-v`, `--verbose` | Print compiler stages and elapsed time to stderr |
| `--version` | Print the compiler version |

Verbose output reports source reading, lexing, parsing and import resolution,
type and ownership checks, specialization, assembly generation, and native
assembly and linking. Each timestamp is elapsed time since compilation started.

## Learn Casa

Start with the [Casa guide](docs/guide.md).
The [examples](examples/README.md) contain runnable programs ordered from
introductory to advanced.

### Language

- [Control flow](docs/control-flow.md)
- [Enums and patterns](docs/enums.md)
- [Functions and lambdas](docs/functions-and-lambdas.md)
- [Intrinsics](docs/intrinsics.md)
- [Modules](docs/modules.md)
- [Operators](docs/operators.md)
- [Ownership and borrows](docs/ownership.md)
- [Reference notation](docs/notation.md)
- [Structs and methods](docs/structs-and-methods.md)
- [Traits](docs/traits.md)
- [Types and literals](docs/types-and-literals.md)

### Libraries

- [Collections and iterators](docs/collections.md)
- [JSON](docs/json.md)
- [List](docs/lists.md)
- [Operating-system APIs](docs/os.md)
- [Optional values and errors](docs/optional-values-and-errors.md)
- [Parser library](docs/parser.md)
- [Specialist libraries](docs/utilities.md): logging, timing, arguments, JSON,
  and parsing
- [Text and characters](docs/strings-and-io.md)

### Tooling

- [Casa style](docs/STYLE.md)
- [Compiler diagnostics](docs/errors.md)
- [Compiler request products](docs/compiler-products.md)
- [Formatter usage and rules](docs/FORMAT.md)
- [Language server](docs/language-server.md)
- [Line counts with cloc](cloc-lang-def.txt)

## Build from source

Casa is self-hosted. Build the compiler with an existing `casac`:

```sh
./casac casa.casa -o casac -L lib
```
