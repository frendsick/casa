# One growable text type replaces StringBuilder

Casa uses `str` as its single owned text value. It is non-`Copy`, contains valid UTF-8, moves by default, and is observed through `$str`. Methods such as `append`, `push`, and `clear` mutate it only through `mut$str` and preserve UTF-8. Casa removes `StringBuilder` and does not add a separate `String` type or a final `build` conversion.

The runtime representation may remain one pointer on the value stack. Its allocation header carries byte length, capacity, and a storage flag. Literals and the empty value may point to shared read-only static storage; the first mutation copies static bytes into uniquely owned growable storage. Dynamic storage is deterministically freed when its owner is dropped. This static-storage promotion requires no reference counting because static bytes are never reclaimed.

## Considered options

- Keeping immutable owned `str` plus `StringBuilder` makes immutable sharing simple, but retains two construction states and a final conversion throughout string-heavy code.
- Renaming `StringBuilder` to `String` while still building a separate `str` changes vocabulary without removing a concept.
- Using an owned mutable `String` plus a distinct borrowed `str` follows a familiar model, but adds an ownership/view distinction and coercion rules that Casa does not otherwise need.
- Making `str` itself growable removes a type and conversion while reusing the general affine ownership and exclusive-borrow rules.

## Consequences

- Moving `str` transfers only its one-word handle. It is never duplicated implicitly; ADR-0076 later gives it an explicit allocating `Clone` implementation.
- A live `$str`, including a foreign string view derived from it, prevents mutation or destruction of its owner. A live `mut$str` is exclusive.
- Safe code cannot mutate arbitrary bytes. Appending `$str`, pushing `char`, clearing, and any future insertion or truncation operations must preserve UTF-8 and maintain the trailing NUL required at foreign boundaries.
- Mutating a literal is safe and cannot affect another equal literal because the first mutation promotes its static backing to uniquely owned storage.
- A `str` moved into a collection cannot be mutated through an old alias. Collections must not expose mutable access to keys whose equality or hash determines placement.
- Allocation failure follows Casa's process-termination policy.
- The implementation is incomplete until compile-time performance is compared with a baseline that already includes the same ownership checker:
  1. Build baseline and growable-`str` branch compilers with the same bootstrap compiler and optimization settings.
  2. Warm both compilers, then alternate at least ten timed self-compilations on the same machine with equivalent source trees, command-line options, library paths, and output location class.
  3. Report median wall-clock time, run-to-run spread, absolute difference, and percentage difference. Also report the source and command differences required by the `StringBuilder` migration.
  4. Run a representative multi-file compiler/examples corpus accepted by both versions to separate general compiler throughput from changed self-hosted source.
  5. If the growable-`str` compiler is slower by more than observed benchmark noise, pause the design decision and ask again with the measured slowdown, the assumed frequency of string mutation, and the expected effect on ordinary programs and self-compilation.

The design itself adds no string-specific type-checking rule: `append` and other mutations use the same `mut$T` checking and callee-directed auto-borrowing required for every mutable type. Capacity growth, static-storage promotion, and UTF-8 maintenance are runtime library operations. Compile-time regression is therefore not expected, but the benchmark gate remains authoritative.
