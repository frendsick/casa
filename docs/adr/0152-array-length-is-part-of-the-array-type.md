# Array length is part of the array type

`array[T N]` is an owned sequence of exactly `N` elements, where `N` is a
compile-time constant. Arrays of different lengths are different types. The
value is its element storage. It carries no length word and no separate header,
so its size is `N * size_of[T]` and the compiler may place it inline in a struct
field or on the stack instead of allocating it.

`N` is a constant type parameter that is also usable as a value inside the
declaration that binds it. The binding site marks it with `const` and states
its type, as in `[T const N:u64]`. The marker is needed because a bare name in
the bracket list already binds a type variable. After `const`, the `:`
introduces the constant's type rather than a trait bound, because a constant
parameter cannot carry one. A use site writes the constant directly, as in
`array[i64 3]`. An array length is a `u64`, so it is non-negative and
`array[T 0]` is a legal type. Array length therefore stays ordinary library
code:

```casa
fn length [T const N:u64] self:$array[T N] -> u64 { N }
```

An index is rejected during typechecking only when both the index and array
length are concrete at that checking site. For example, this definition is
rejected because `3` is outside the concrete length `3`:

```casa
fn fourth values:$array[i64 3] -> i64 { 3 values.nth.clone }
```

A constant parameter remains symbolic while its generic body is checked under
ADR-0069. The same index is therefore accepted here because `N` is not concrete:

```casa
fn fourth [const N:u64] values:$array[i64 N] -> i64 {
    3 values.nth.clone
}

[1, 2, 3] fourth drop # Specializes N to 3 and terminates at runtime.
```

Each generated specialization retains the ordinary terminating runtime bounds
check. This applies even when its array length later becomes concrete. Casa does
not need a generic constraint solver or a new body check for each
specialization. An index that is not a compile-time constant also keeps the
runtime bounds check.

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
- Indexing an array with an out-of-range constant is a compile-time error when
  the array length is also concrete at that checking site. A symbolic generic
  length or runtime index keeps the terminating runtime bounds check.
- `[]` is `array[T 0]`. ADR-0155 settles its size: a zero-length array is
  inhabited and keeps ADR-0132's one-byte minimum, so `N * size_of[T]`
  describes its element storage rather than the whole value.
- Casa gains a third sequence concept. `array[T N]` is fixed and statically
  sized, `Slice[T]` is borrowed and has a runtime length, and `List[T]` stays
  growable. `List::slice` and `List::to_array` return `Slice[T]`.
- A function that accepts arrays of any length takes a constant length
  parameter, such as `fn total [T const N:u64] values:$array[T N] -> T`. Under
  ADR-0069 each distinct `N` monomorphizes separately.
- Converting an owned array into a list still transfers its allocation, but the
  list must record the length that the array no longer stores.
- A constant parameter states its own type, so array length needs `u64` only.
  Constant parameters of other types are a later extension rather than a syntax
  change.
