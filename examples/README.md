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
| 6 | [`enum.casa`](enum.casa) | Payload variants, exhaustive matching, and guards |
| 7 | [`generics.casa`](generics.casa) | A trait-bound generic function with two implementations |
| 8 | [`for_loop.casa`](for_loop.casa) | A custom iterator used by a `for` loop |
| 9 | [`iterator_combinators.casa`](iterator_combinators.casa) | A lazy filter and map pipeline with terminal operations |
| 10 | [`hash_map.casa`](hash_map.casa) | Counting values with `Map` |
| 11 | [`sorting.casa`](sorting.casa) | List sorting with default and custom order |
| 12 | [`propagate_result.casa`](propagate_result.casa) | File errors and `?` propagation |
| 13 | [`argparse.casa`](argparse.casa) | A command-line interface with options and help |
| 14 | [`parser.casa`](parser.casa) | A complete parser built from `Cursor` operations |
| 15 | [`os_interaction.casa`](os_interaction.casa) | Files, directories, paths, environment, and processes |
| 16 | [`log.casa`](log.casa) | Configurable logging |
| 17 | [`timer.casa`](timer.casa) | Local and global timers |
| 18 | [`freestanding_primitives.casa`](freestanding_primitives.casa) | Primitive operations without the standard library |
| 19 | [`sized_memory.casa`](sized_memory.casa) | Unsafe allocation and sized memory access |
| 20 | [`game_of_life.casa`](game_of_life.casa) | An interactive terminal program with raw Linux calls |

`game_of_life.casa` needs an interactive terminal. Stop it with Ctrl+C.
