# Character conversion uses code-point APIs

Casa exposes the safe lossless operation `character.codepoint` with effect `$char -> u32` and the unsafe primitive `char::from_codepoint_unchecked` with effect `u32 -> char`. The standard library builds `char::from_codepoint` with effect `u32 -> Option[char]` by validating the integer before calling the unsafe primitive.

## Consequences

- `char::from_codepoint` accepts Unicode scalar values and returns `Option::None` for values above `0x10FFFF` or in the surrogate range.
- `character.codepoint` is always safe and lossless.
- Calling `char::from_codepoint_unchecked` with a non-scalar value is undefined behavior and therefore requires an explicit `unsafe` block.
- Code that has already validated a scalar, such as a UTF-8 decoder, can construct the `char` without repeating validation.
- `Option` remains an ordinary standard-library enum. The compiler neither recognizes it nor supplies fallible-conversion behavior.
- Casa does not restore integer-to-character cast syntax.
