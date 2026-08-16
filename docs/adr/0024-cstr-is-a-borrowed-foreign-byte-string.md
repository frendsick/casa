# cstr is a borrowed foreign byte string

Casa represents a NUL-terminated C byte string as `$cstr`: an immutable, lifetime-bound view that neither owns its storage nor claims that its bytes are valid UTF-8. The referent `cstr` is opaque to safe code. A safe `str.as_cstr -> Option[$cstr]` borrows the source `str`, rejecting interior NUL, and `cstr.to_str $self -> Result[str Utf8Error]` validates and copies foreign bytes into owned Casa text.

Foreign declarations use the same pointer-level meaning. A C parameter of type `const char *` is declared as `$cstr`, for example `extern fn puts text:$cstr -> i32`. The ABI lowers that borrow to one pointer; it does not add another pointer level. A raw C pointer is not implicitly a valid `$cstr`: constructing the view requires `unsafe` code to establish accessibility and NUL termination. Safe wrappers either tie the view to an existing borrowed owner or copy it into `str`.

## Considered options

- Keeping `cstr` as a freely copyable raw pointer preserves current casts and direct printing, but bypasses provenance, lifetime, and UTF-8 validation.
- Treating `cstr` as owned would leave destruction and allocator choice undefined across foreign boundaries.
- Treating every C string as `str` would falsely promise UTF-8 and require a length header that the C ABI does not provide.
- A borrowed opaque byte-string view represents the actual foreign contract while keeping conversion and ownership explicit.

## Consequences

- The borrow returned by `str.as_cstr` prevents mutation, reallocation, or destruction of the source `str` while the foreign view is live.
- `str` storage maintains a trailing NUL, but valid UTF-8 text may contain an interior NUL, so `as_cstr` remains fallible.
- `cstr` has no direct `Display` implementation or specialized safe `print`; callers validate with `to_str` or use an explicitly byte-oriented unsafe boundary.
- Direct construction from `ptr`, current `(cstr)` casts, and unbounded `str::from_cstr` are removed from safe code.
- Foreign APIs returning pointers require an unsafe wrapper to establish a lifetime or copy the bytes into an owned value. Casa initially adds no source syntax for foreign static lifetimes.
- Mutable C buffers use bounded byte storage or explicit unsafe pointers rather than `mut$cstr`.
