# Text views and owned strings are separate

Casa uses `str` for immutable UTF-8 views and `String` for owned growable UTF-8 text. A `str` is `Copy` and never frees its storage. Literals use static `str` storage. A `String` is non-`Copy`, moves by default, and mutates only through `mut$String`. Casa removes `StringBuilder` and has no final `build` conversion or third text type.

`String.as_str self:$String -> $str` borrows the owner's current text without allocation. `str.to_str self:$str -> String` allocates an independent owner. Operations that produce text return `String`. Operations that only inspect text accept `$str`. Collection helpers for `String` accept `$str` where ownership is not required.

Both representations preserve valid UTF-8 and a trailing NUL. Safe mutation appends either `$str` or `char`, so it cannot insert invalid bytes. Dynamic `String` storage is released exactly once when the owner is destroyed. Static `str` storage is never released.

## Considered options

- Keeping `StringBuilder` retains a construction-only type and a final conversion.
- Making `str` growable makes literals appear to own static storage and prevents cheap copied views.
- Using `String` as the owner and `str` as the view gives each type one storage role.

## Consequences

- Copying `str` copies only its view. Moving `String` transfers ownership of its storage.
- A live `$str` borrowed from a `String` prevents mutation or destruction of that owner. A live `mut$String` is exclusive.
- Safe code cannot mutate arbitrary bytes. Appending `$str`, pushing `char`, clearing, and any future insertion or truncation operations must preserve UTF-8 and maintain the trailing NUL required at foreign boundaries.
- A literal must be converted to `String` before mutation. The conversion copies static bytes into owned storage.
- Collections own `String` keys and values. Borrowed lookup helpers accept `$str` without allocating temporary owners.
- Allocation failure follows Casa's process-termination policy.
- The implementation is incomplete until compile-time performance is compared with the current implementation:
  1. Build baseline and branch compilers with the same bootstrap compiler and optimization settings.
  2. Warm both compilers, then alternate at least ten timed self-compilations on the same machine with equivalent source trees, command-line options, library paths, and output location class.
  3. Report median wall-clock time, run-to-run spread, absolute difference, and percentage difference. Also report the source and command differences required by the `StringBuilder` migration.
  4. Run a representative multi-file compiler/examples corpus accepted by both versions to separate general compiler throughput from changed self-hosted source.
  5. If the branch compiler is slower by more than observed benchmark noise, pause the decision and report the measured slowdown before retaining it.

The design adds no implicit coercion. Callers use `as_str` and `to_str` at the ownership boundary. `append` and other mutations use the general `mut$T` rules. Capacity growth and UTF-8 maintenance remain standard-library operations.
