# Text views and owned strings are separate

Casa uses `str` for immutable UTF-8 views and `String` for owned growable UTF-8 text. A `str` is `Copy` and never frees its storage. Literals use static `str` storage. A `String` is non-`Copy`, moves by default, and mutates only through `mut$String`. Casa removes `StringBuilder` and has no final `build` conversion or third text type.

`String.as_str self:$String -> $str` borrows the owner's current text without allocation. `str.to_str self:$str -> String` allocates an independent owner. Operations that produce text return `String`. Operations that only inspect text accept `$str`. Collection helpers for `String` accept `$str` where ownership is not required.

Both representations preserve valid UTF-8 and a trailing NUL. Safe mutation appends either `$str` or `char`, so it cannot insert invalid bytes. Dynamic `String` storage is released exactly once when the owner is destroyed. Static `str` storage is never released.

## Considered options

- Keeping `StringBuilder` retains a construction-only type and a final conversion.
- Making `str` growable makes literals appear to own static storage and prevents cheap copied views.

## Consequences

- Copying `str` copies only its view. Moving `String` transfers ownership of its storage.
- A live `$str` borrowed from a `String` prevents mutation or destruction of that owner. A live `mut$String` is exclusive.
- Safe code cannot mutate arbitrary bytes. Appending `$str`, pushing `char`, clearing, and any future insertion or truncation operations must preserve UTF-8 and maintain the trailing NUL required at foreign boundaries.
- A literal must be converted to `String` before mutation. The conversion copies static bytes into owned storage.
- Collections own `String` keys and values. Borrowed lookup helpers accept `$str` without allocating temporary owners.
- Allocation failure follows Casa's process-termination policy.
- The completed migration and its comparison requirements remain in
  [#418](https://github.com/frendsick/casa/issues/418).

The design adds no implicit coercion. Callers use `as_str` and `to_str` at the ownership boundary. `append` and other mutations use the general `mut$T` rules. Capacity growth and UTF-8 maintenance remain standard-library operations.
