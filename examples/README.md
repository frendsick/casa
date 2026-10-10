# Casa Examples

Run an example from the repository root:

```sh
./casac examples/fizzbuzz.casa -L lib -r
```

The examples are ordered from introductory programs to low-level system code.

| Order | Program | Purpose |
|---:|---|---|
| 1 | [hello_world.casa](hello_world.casa) | The smallest complete Casa program |
| 2 | [fibonacci.casa](fibonacci.casa) | Functions, recursion, and early return |
| 3 | [fizzbuzz.casa](fizzbuzz.casa) | Bindings, loops, and conditional branches |
| 4 | [euler01.casa](euler01.casa) | A typed constant expression and an arithmetic algorithm |
| 5 | [struct.casa](struct.casa) | A struct literal, method, field update, and destructuring |
| 6 | [destruction.casa](destruction.casa) | Deterministic cleanup in reverse acquisition order |
| 7 | [enum.casa](enum.casa) | Payload variants, recursive ownership, exhaustive matching, and guards |
| 8 | [generics.casa](generics.casa) | A trait-bound generic function with two implementations |
| 9 | [for_loop.casa](for_loop.casa) | A custom iterator used by a `for` loop |
| 10 | [iterator_combinators.casa](iterator_combinators.casa) | A lazy filter and map pipeline with terminal operations |
| 11 | [hash_map.casa](hash_map.casa) | Counting values with `Map` |
| 12 | [sorting.casa](sorting.casa) | List sorting with default and custom order |
| 13 | [propagate_result.casa](propagate_result.casa) | Structural `?` propagation for `Result` and a custom enum |
| 14 | [argparse.casa](argparse.casa) | A command-line interface with options and help |
| 15 | [parser.casa](parser.casa) | A complete parser built from `Cursor` operations |
| 16 | [json.casa](json.casa) | JSON conversion traits for a struct and an enum |
| 17 | [os_interaction.casa](os_interaction.casa) | Files, directories, paths, environment, and processes |
| 18 | [log.casa](log.casa) | Logging with explicit root-owned level state |
| 19 | [timer.casa](timer.casa) | A root-owned monotonic timer |
| 20 | [freestanding_primitives.casa](freestanding_primitives.casa) | Primitive operations without the standard library |
| 21 | [sized_memory.casa](sized_memory.casa) | [unsafe](../docs/functions-and-lambdas.md#unsafe-boundaries) allocation and sized memory access |
| 22 | [unicode.casa](unicode.casa) | Direct Unicode, Unicode escapes, and code-point conversion |
| 23 | [owned_string.casa](owned_string.casa) | Owned string growth, borrowing, and cloning |
| 24 | [bytes.casa](bytes.casa) | Compact binary storage, iteration, and validated text conversion |
| 25 | [game_of_life.casa](game_of_life.casa) | Resizable raylib graphics and toroidal Conway simulation |
| 26 | [root_owned_state.casa](root_owned_state.casa) | Root-owned runtime state, explicit parameters, and cleanup |
| 27 | [foreign_function.casa](foreign_function.casa) | C ABI scalars, an aggregate return, and native library linking |

The [borrow lookup example](borrow_lookup.casa) reuses map and JSON lookup keys
while the returned values remain borrowed from their collections.

The foreign-function example links libc:

```sh
./casac -L lib -l c examples/foreign_function.casa -r
```

## Raylib example

[game_of_life.casa](game_of_life.casa) runs Conway's Game of Life in a resizable
800 by 600 window. Cells wrap across both edges. The simulation runs at five
frames per second with eight-pixel cells.

Hold the left mouse button to create live cells. Hold the right button to erase
them. The right button wins when both are held. Close the window or press Escape
to exit. Resizing preserves the top-left overlap and randomizes new cells.
Minimizing the window or making it smaller than one cell pauses the simulation
and keeps the last valid grid and graphics resources. Unused edge pixels remain
dark gray.

The example uses the reusable
[raylib module](../lib/raylib.casa), which owns the native resources and releases
them on normal scope exit and early return.

Install raylib as a system library on Linux x86-64, following the
[GNU/Linux installation guide](https://github.com/raysan5/raylib/wiki/Working-on-GNU-Linux).
Run the example in an X11 graphical session with OpenGL support. The declarations
and example were tested with [raylib 6.0](https://github.com/raysan5/raylib/releases/tag/6.0).
This is a tested version, not a minimum version requirement.

Build the compiler from this checkout, then link raylib and its Linux dependencies:

```sh
./casac -L lib casa.casa -o /tmp/casac-raylib
/tmp/casac-raylib -L lib \
    -l raylib -l GL -l m -l pthread -l dl -l rt -l X11 -l c \
    examples/game_of_life.casa -r
```

Casa does not vendor raylib. Default CI compiles the example against a small C
fixture that checks argument values, drawing order, failure handling, and cleanup.
It compares uploaded pixels with a deterministic reference simulation through
resizes and mouse edits. The previous mouse-following texture program remains a
[test fixture](../tests/compiler/fixtures/raylib_mouse.casa). Default CI does not
require raylib, a display server, GPU drivers, or network access.

### Resource ownership

The module exposes the operations needed by this example. Native functions and
resource handles are private. `Color` and `Vector2` are public `Copy` extern
structs with public fields.

| Operation | Contract |
|---|---|
| `Window::new` | Returns `Option[Window]`. Rejects nonpositive dimensions, an existing window, or failed initialization. An empty title becomes `Casa`. |
| `Window.split` | Lends a shared `Context` and an exclusive `Drawing` capability. Both keep the window loaned. |
| `Context.is_minimized`, `.mouse_button_down`, `.mouse_position`, `.should_close` | Read mouse input, minimization state, or the close condition while the window is open. |
| `Context.random_byte` | Returns a random byte for cell initialization. |
| `Context.screen_width`, `.screen_height`, `.set_resizable`, `.set_target_fps` | Read current dimensions, enable resizing, or set the frame limit. |
| `Image::new` | Returns `Option[Image]` for a solid-color image. Rejects nonpositive dimensions and RGBA byte counts above the signed C integer range. |
| `Image.clear`, `.set_pixel` | Clear the image or edit a pixel. `set_pixel` returns `false` for out-of-bounds coordinates. |
| `Texture::new` | Takes an owned image and a shared context. Returns the image, then `Option[Texture]`, with the upload result on top of the stack. The caller retains the image for editing or drops it. |
| `Texture.update_from` | Borrows the texture exclusively and the image shared. Returns `false` without uploading if dimensions or formats differ. |
| `Drawing.begin` | Borrows the drawing capability exclusively and retains a shared texture borrow in a `Frame`. |
| `Frame.clear`, `.draw_texture`, `.draw_texture_scaled` | Clear the background and draw the retained texture at a position with a tint and optional scale. |

`Image::new` fills a temporary Casa buffer and calls raylib's `ImageCopy`, which
checks its native allocation. Raylib 6.0's `GenImageColor` does not check allocation
failure before writing pixels. The temporary buffer is released after the copy.

`Texture::new` returns the CPU image on success and failure. This replaces the
previous consuming constructor contract:

```casa
context image raylib::Texture::new = upload = image
upload ? = texture
```

Returning ownership avoids an opaque shared image loan lasting as long as the
texture. Callers can edit the image and upload it again. End the previous frame
before updating or replacing a texture.

Constructors return `None` for invalid native results. A frame ends drawing when
it is destroyed. A texture cannot be released until its frame ends, and the
window cannot close while either capability or a texture is live. Image, texture,
and window owners release their native resources exactly once. As with other Casa
owners, process termination through `panic` or `process::exit` does not run cleanup.
The example returns from `run -> bool` before calling `process::exit`, so graphics
cleanup completes on both success and resource failure.

A frame retains one texture. Extend that contract if a later example needs several
textures in one frame. The module does not wrap audio, fonts, models, or callbacks.
