# UTF-8 text is separate from byte data

Casa will distinguish text from arbitrary bytes. `str` contains validated UTF-8 text, `char` represents one Unicode scalar value, and `u8` represents one byte. Binary data uses a byte collection rather than pretending to be text.

## Considered options

- Keeping `str` and `char` byte-oriented preserves the current implementation, but makes common text operations ASCII-only and lets invalid text flow through text APIs.
- Making every string operation character-indexed hides decoding costs and makes constant-time byte-oriented operations impossible to express clearly.
- Using `i64` for bytes avoids another primitive, but does not constrain values to the range or representation required by binary formats and foreign interfaces.

## Consequences

- `str.byte_length` returns the encoded byte length in constant time; `str.length` counts Unicode scalar values.
- Strings do not provide ambiguous integer indexing. `.bytes` and `.chars` expose explicit iterators, and slicing APIs name whether their units are bytes or Unicode scalars.
- Text literals accept direct Unicode and `\u{scalar}`. `\xHH` is restricted to ASCII values so it cannot inject invalid UTF-8 into `char` or `str`.
- Existing byte-oriented `char` and `str` code requires migration.
- Compact binary storage is a stdlib `Bytes` type rather than another compiler-owned collection. Raw file, standard-input, and captured-process data enters safe code as `Bytes` and requires explicit UTF-8 validation before becoming `str`.
- Foreign NUL-terminated bytes are exposed as `$cstr`; converting them to `str` validates UTF-8 and returns `Result[str Utf8Error]`.
