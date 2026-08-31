# Casa Examples

Run an example from the repository root:

```sh
./casac examples/fizzbuzz.casa -L lib -r
```

The examples are ordered from introductory programs to low-level system code.

| Order | Program | What it demonstrates |
|---:|---|---|
| 1 | [`hello_world.casa`](hello_world.casa) | The smallest complete Casa program |
| 2 | [`fibonacci.casa`](fibonacci.casa) | Functions, recursion, and early return |
| 3 | [`fizzbuzz.casa`](fizzbuzz.casa) | Bindings, loops, and conditional branches |
| 4 | [`euler01.casa`](euler01.casa) | Constants and an arithmetic algorithm |
| 5 | [`struct.casa`](struct.casa) | A struct literal, method, field update, and destructuring |
| 6 | [`destruction.casa`](destruction.casa) | Deterministic cleanup in reverse acquisition order |
| 7 | [`enum.casa`](enum.casa) | Payload variants, recursive ownership, exhaustive matching, and guards |
| 8 | [`generics.casa`](generics.casa) | A trait-bound generic function with two implementations |
| 9 | [`for_loop.casa`](for_loop.casa) | A custom iterator used by a `for` loop |
| 10 | [`iterator_combinators.casa`](iterator_combinators.casa) | A lazy filter and map pipeline with terminal operations |
| 11 | [`hash_map.casa`](hash_map.casa) | Counting values with `Map` |
| 12 | [`sorting.casa`](sorting.casa) | List sorting with default and custom order |
| 13 | [`propagate_result.casa`](propagate_result.casa) | Structural `?` propagation for `Result` and a custom enum |
| 14 | [`argparse.casa`](argparse.casa) | A command-line interface with options and help |
| 15 | [`parser.casa`](parser.casa) | A complete parser built from `Cursor` operations |
| 16 | [`os_interaction.casa`](os_interaction.casa) | Files, directories, paths, environment, and processes |
| 17 | [`log.casa`](log.casa) | Configurable logging |
| 18 | [`timer.casa`](timer.casa) | Local and global timers |
| 19 | [`freestanding_primitives.casa`](freestanding_primitives.casa) | Primitive operations without the standard library |
| 20 | [`sized_memory.casa`](sized_memory.casa) | Unsafe allocation and sized memory access |
| 21 | [`unicode.casa`](unicode.casa) | Direct Unicode, Unicode escapes, and code-point conversion |
| 22 | [`owned_string.casa`](owned_string.casa) | Owned string growth, borrowing, and cloning |
| 23 | [`bytes.casa`](bytes.casa) | Compact binary storage, iteration, and validated text conversion |
| 24 | [`game_of_life.casa`](game_of_life.casa) | An interactive terminal program with raw Linux calls |
| 25 | [`immutable_global.casa`](immutable_global.casa) | Eager immutable globals with `Copy` and borrowed reads |
| 26 | [`foreign_function.casa`](foreign_function.casa) | A C ABI declaration, borrowed C string, and native library link |

`game_of_life.casa` needs an interactive terminal. Stop it with Ctrl+C.

The foreign-function example links libc:

```sh
./casac -L lib -l c examples/foreign_function.casa -r
```
