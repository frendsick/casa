# UTF-8 text is separate from byte data

Casa distinguishes text from arbitrary bytes. `str` contains validated UTF-8 text, `char` represents one Unicode scalar value, and `u8` represents one byte. Binary data uses a byte collection rather than pretending to be text.

## Considered options

- Keeping `str` and `char` byte-oriented makes common text operations ASCII-only and lets invalid text flow through text APIs.
- Making every string operation character-indexed hides decoding costs and makes constant-time byte-oriented operations impossible to express clearly.
- Using `i64` for bytes avoids another primitive, but does not constrain values to the range or representation required by binary formats and foreign interfaces.

## Consequences

- `str.length` returns the encoded byte length in constant time. `str.iter` decodes Unicode scalar values as `char`.
- `str::substring` uses byte ranges and validates UTF-8 boundaries.
- Text literals accept direct Unicode and `\u{scalar}`. `\xHH` is restricted to ASCII values so it cannot inject invalid UTF-8 into `char` or `str`.
- Binary storage uses the ordinary stdlib owner `Bytes`. Raw file, standard-input, and captured-process data enters safe code as `Bytes` and requires explicit UTF-8 validation before becoming `String`.
- Foreign NUL-terminated bytes are exposed as `$cstr`; converting them to owned text validates UTF-8 and returns `Result[String Utf8Error]`.
