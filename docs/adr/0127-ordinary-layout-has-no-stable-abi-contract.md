# Ordinary layout has no stable ABI contract

`size_of[T]` reports the current compiler's x86-64 inline layout for `T`. It does not promise that an ordinary struct or enum retains the same size, padding, tag representation, or field offsets across compiler versions.

Code compiled together may use `size_of[T]`, `ptr::read[T]`, and `ptr::write[T]` consistently because they share one compiler layout. Persisted data and foreign interfaces may not treat that layout as a stable format.

## Consequences

- Ordinary Casa aggregates remain free to receive layout improvements without source annotations or migration guarantees.
- `size_of[T]` is valid for allocation and addressing within the current compiled program, not for protocol constants or file formats.
- Initial foreign declarations continue to exclude aggregates; callers pass scalar values or `ptr` through explicit wrappers.
- Serialization writes fields through an explicit format rather than copying aggregate representation bytes.
- Casa adds no `repr(C)`, packed-layout, explicit-alignment, or stable-layout feature until a concrete foreign aggregate requires one.
