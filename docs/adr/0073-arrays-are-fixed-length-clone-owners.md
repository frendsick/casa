# Arrays are fixed-length Clone owners

`array[T]` is an owned fixed-length sequence. Its length is chosen when the value is constructed and cannot subsequently change, but the length is runtime metadata rather than part of the source type. Arrays of different lengths therefore share the same `array[T]` type. `List[T]` remains the growable sequence.

An owned array controls indirect element storage and is never `Copy`, even when `T: Copy`. It conditionally implements `Clone` when `T: Clone`; cloning produces an independent owned array and may allocate. An implementation may bulk-copy elements when `T: Copy`, but that remains an explicit `clone` operation.

Borrowed `$array[T]` values may provide fixed-length views without taking ownership. Existing zero-copy list slicing continues to return a borrow tied to the source list rather than introducing a separate slice type.

## Considered options

- Encoding the length in `array[T N]` would permit inline, statically sized arrays, but requires dedicated constant type arguments and a separate runtime-length slice abstraction.
- Keeping runtime length preserves the existing type and collection boundary while still distinguishing fixed-length arrays from growable lists.

## Consequences

- `[1, 2, 3]` has type `array[i64]`, not `array[i64 3]`.
- Array operations may replace elements but cannot append, insert, remove, or otherwise change length.
- Converting an owned array into a list may transfer its allocation; viewing a list as an array borrows the list.
- Type-level array lengths and a distinct `Slice[T]` remain deferred until a concrete static-layout or interoperability need justifies them.
