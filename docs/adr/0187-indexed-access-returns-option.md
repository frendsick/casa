# Indexed access returns Option

Indexed operations on arrays, `List`, `Slice`, and `Bytes` return `Option` when
they do not consume an item. Missing elements, invalid ranges, empty list pops,
and missing process arguments return `None`. Failure leaves the source unchanged.
Callers can handle absence without a separate length check, and the result
does not depend on the collection type. Operations that consume an item return
`Result` and give the rejected item back,
as specified by [ADR-0186](0186-list-indexed-mutations-return-rejected-items.md).

`get` is the indexed read name across these collections. Arrays and `Slice`
replace `nth` with `get`. `List.get_ref` is removed because it duplicated `get`.
`List.get` returns `Option[$T]`, and `List.get_mut` returns `Option[mut$T]`.
`Bytes.get` keeps returning `Option[u8]`. Borrowed elements and views retain their
source loans through the last use, including when stored inside `Option`.

`slice` returns `Option[Slice[T]]` for `start <= stop <= length`, including empty
ranges at the end. `as_slice` keeps returning a complete `Slice[T]` because its
range is always valid. Private unchecked helpers construct views only after a
public range check or from the complete source extent.

`List.swap_at` returns `Option[Unit]`. Both indices must be valid, including
when they are equal. `List.pop` returns `Option[T]` and transfers the removed
owner only on success. `std::get_arg` returns `Option[Bytes]`.

There is no terminating indexed-access variant. Callers use `.unwrap` when the
index, range, or element must be valid. `Option.unwrap` and `Result.unwrap` write
failure messages to standard error and retain exit status 60. A constant array
index outside the length in its type remains a compile-time error. Runtime
indices return `None` when they are out of range.

Standard-library iterators use private unchecked element helpers after checking
their cursor against the source length. This avoids constructing and unwrapping
an intermediate `Option` for each element. The yielded result remains optional.

This implements [#1004](https://github.com/frendsick/casa/issues/1004) and amends
[ADR-0152](0152-array-length-is-part-of-the-array-type.md) and
[ADR-0157](0157-collection-ownership-uses-two-explicit-mode-triads.md).
