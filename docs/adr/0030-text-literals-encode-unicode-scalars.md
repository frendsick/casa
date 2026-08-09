# Text literals encode Unicode scalars

Casa source files and text literals contain valid UTF-8. A `char` literal denotes exactly one Unicode scalar value, and a `str` literal denotes a sequence of Unicode scalars. Source may contain Unicode directly or use `\u{scalar}` with hexadecimal scalar notation. Surrogates, values above `U+10FFFF`, malformed source UTF-8, and character literals containing zero or multiple scalars are compile errors.

The `\xHH` escape remains available only for values `00` through `7F`, where one byte is also one Unicode scalar encoded identically in UTF-8. It is useful for ASCII control values such as `\x1b`; it never injects an arbitrary byte into text. `\0` denotes Unicode NUL and remains valid text, although a `str` containing it cannot be borrowed as `$cstr`.

## Considered options

- Keeping unrestricted `\xHH` preserves current byte-oriented literals, but permits invalid UTF-8 to inhabit `char` and `str`.
- Removing hexadecimal escapes entirely guarantees scalar text, but makes ASCII control sequences unnecessarily awkward.
- Interpreting adjacent `\xHH` escapes as UTF-8 encoding bytes makes one character depend on surrounding escapes and duplicates source decoding rules.
- Adding `\u{scalar}` gives every Unicode scalar an unambiguous spelling while retaining safe ASCII byte escapes.

## Consequences

- The lexer decodes escapes to scalar values and encodes them as UTF-8 in string contents. Literal validation is linear in source length.
- Direct Unicode and escaped Unicode have the same value. Casa performs no implicit Unicode normalization.
- Existing uses that assemble one glyph from multiple high-byte `\xHH` escapes migrate to the direct character or one `\u{...}` escape.
- Arbitrary byte sequences use `Bytes`. Casa initially adds no dedicated bytes-literal syntax because contextual `u8` literals already construct byte buffers without ambiguity.
- F-string brace escapes remain syntactic escapes independent of Unicode decoding.
