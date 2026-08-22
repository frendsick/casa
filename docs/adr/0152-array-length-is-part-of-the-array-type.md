# Array length is part of the array type

`array[T N]` is an owned sequence of exactly `N` elements, where `N` is a
compile-time constant. Arrays of different lengths are different types. The
value is its element storage. It carries no length word and no separate header,
so its size is `N * size_of[T]` and the compiler may place it inline in a struct
field or on the stack instead of allocating it.

`N` is a constant type parameter that is also usable as a value inside the
declaration that binds it. Array length therefore stays ordinary library code:

```casa
fn length [T N] self:$array[T N] -> u64 { N }
```

An index that is a compile-time constant is checked against `N` during
typechecking. A runtime index keeps the terminating bounds check.

A view over a runtime-length range can no longer be an array, because its length
is not known when the type is written. Casa needs a separate runtime-length
sequence view, and `List` slicing returns that view instead of an array.

This supersedes ADR-0073 and ends the deferral it recorded for type-level array
lengths and for a distinct slice type. ADR-0020 is unaffected: each evaluation of
an array literal still produces an independent owned value.

## Considered options

- Keeping the runtime length, as ADR-0073 chose, preserves one sequence type and
  one array spelling. It also forces every array to carry a header and an
  indirection, and leaves inline arrays, stack arrays, and fixed-size foreign
  fields inexpressible at any cost.
- Tracking the length as a compile-time fact attached to the value, the way
  contextual numeric literals already work, needs no new type syntax. The fact
  cannot survive a function boundary or a branch join, so anything passed around
  falls back to a stored length and the header returns.
- Putting the length in the type expresses the fixed-length case exactly and
  gives arrays a layout. It costs a constant type parameter and a second
  sequence concept.

## Consequences

- `[1, 2, 3]` has type `array[i64 3]`, not `array[i64]`.
- Arrays of different lengths no longer unify. `if condition then [1, 2] else
  [1, 2, 3] fi` becomes a type error, and existing sources that rely on the
  joined type must change.
- `.length` resolves to a constant. No array operation loads a stored length.
- Indexing an `array[T N]` with an out-of-range constant is a compile-time
  error. Indexing with a runtime value still terminates the program.
- `[]` is `array[T 0]` and has size zero. ADR-0132's one-byte minimum describes
  inhabited types, so a zero-length array needs an explicit rule rather than an
  implicit exception.
- Casa gains a third sequence concept. `array[T N]` is fixed and statically
  sized, the runtime-length view is borrowed, and `List[T]` stays growable.
  `List::slice` and `List::to_array` produce the view.
- A function that accepts arrays of any length takes a constant length
  parameter, such as `fn total [T N] values:$array[T N] -> T`. Under ADR-0069
  each distinct `N` monomorphizes separately.
- Converting an owned array into a list still transfers its allocation, but the
  list must record the length that the array no longer stores.
