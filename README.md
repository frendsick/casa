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
- **Standard library** — dynamic `List`, arrays, type conversions, and memory utilities

## Requirements

- **Linux x86-64**
- **Python 3.11+**
- **GNU assembler** (`as`) and **linker** (`ld`)

## Getting Started

Compile and run a program:

```sh
python3 casa.py examples/hello_world.casa -r
# Hello world!
```

Compile with a custom output name:

```sh
python3 casa.py examples/fibonacci.casa -o fib
./fib
# 6765
```

## CLI Usage

```
python3 casa.py <file> [-o name] [--keep-asm] [-r] [-v]
```

| Flag | Description |
|------|-------------|
| `-o`, `--output` | Output binary name (default: input file stem) |
| `--keep-asm` | Keep the generated `.s` assembly file |
| `-r`, `--run` | Execute the binary after compilation |
| `-v`, `--verbose` | Enable verbose logging |

## Compilation Pipeline

```
source → lex → parse → resolve → type check → bytecode → emit asm → assemble → link → executable
```

## Documentation

| Document | Topics |
|----------|--------|
| [Types and Literals](docs/types-and-literals.md) | Primitive types, composite types, literals, type casting, comments |
| [Operators](docs/operators.md) | Arithmetic, bitshift, bitwise, comparison, boolean, assignment |
| [Control Flow](docs/control-flow.md) | Conditionals (`if`/`elif`/`else`/`fi`), loops (`while`/`do`/`done`) |
| [Functions and Lambdas](docs/functions-and-lambdas.md) | Functions, lambdas, closures, variables, stack intrinsics, IO, memory |
| [Structs and Methods](docs/structs-and-methods.md) | Struct definition, accessors, `impl` blocks, dot/arrow syntax |
| [Standard Library](docs/standard-library.md) | `include`, `memcpy`, arrays (`map`, `filter`, `reduce`), `option[T]`, `List`, string methods, C string methods, character classification, type conversions |
| [Errors](docs/errors.md) | Error kinds, diagnostics format, multi-error collection |

## Examples

| File | Description |
|------|-------------|
| [hello_world.casa](examples/hello_world.casa) | Print a string |
| [fibonacci.casa](examples/fibonacci.casa) | Recursive Fibonacci function |
| [fizzbuzz.casa](examples/fizzbuzz.casa) | FizzBuzz with functions, variables, and conditionals |
| [struct.casa](examples/struct.casa) | Structs, getters, setters, and methods |
| [euler01.casa](examples/euler01.casa) | Project Euler problem 1 — sum of multiples |
| [string_operations.casa](examples/string_operations.casa) | String methods, char type, and cstr conversion |
| [bitwise_operations.casa](examples/bitwise_operations.casa) | Bitwise AND, OR, XOR, and NOT operations |

More examples: [examples](./examples/)

## Testing

Run the following command from the root folder of the repository:

```sh
pytest tests
```
