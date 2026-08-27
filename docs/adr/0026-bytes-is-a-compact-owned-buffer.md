# Bytes is a compact owned buffer

`Bytes` is a distinct standard-library type for arbitrary binary data. It owns a compact growable buffer with one `u8` per byte, is non-`Copy`, moves by default, and is observed through `$Bytes`. Safe mutation requires `mut$Bytes`; ADR-0076 later gives it an explicit allocating `Clone` implementation.

`Bytes` is implemented as an ordinary stdlib type with private representation and unsafe allocation internals. It does not require new syntax or another compiler-owned collection. Its initial safe surface includes empty construction, length and capacity queries, `push`, buffer append, copied indexed access returning `Option[u8]`, and iteration yielding copied `u8` values. Raw-input wrappers may reserve and initialize storage through private unsafe operations before publishing the initialized length.

## Considered options

- Aliasing `Bytes` to `array[u8]` reuses an existing type, but current arrays store every element in one eight-byte word and have different fixed-value semantics.
- Aliasing `Bytes` to `List[u8]` reuses growth operations, but current lists also store one eight-byte word per element and expose a general collection representation.
- Making bytes another compiler primitive could guarantee representation, but adds language machinery for an abstraction the standard library can safely encapsulate.
- A dedicated stdlib buffer provides compact storage and byte-specific operations behind the existing unsafe boundary.

## Consequences

- Every initialized element is a valid `u8`; unlike `str`, `Bytes` imposes no UTF-8 or interior-NUL invariant.
- Indexed reads copy `u8`. Any mutable indexed access must remain bounded and require an exclusive borrow.
- Moving `Bytes` transfers its buffer handle; deterministic destruction frees dynamic storage exactly once.
- `Bytes.to_str $self -> Result[String Utf8Error]` validates and copies without consuming the source.
- A consuming `into_str` conversion remains deferred until measured copying justifies its failure-ownership and representation complexity.
- Conversion to `$cstr` is not implicit: arbitrary bytes may lack a trailing NUL or contain interior NUL.
- Allocation failure follows Casa's process-termination policy.
