# Extern functions use an ABI-safe type whitelist

The `extern fn` surface accepts fixed-width integers, `f32`, `f64`, `bool`, and `ptr`. On Casa's x86-64 target, C `size_t` is written `u64` and `ssize_t` is written `i64`. Casa `bool` represents C `_Bool`; parameters contain 0 or 1, and returns are normalized before they enter Casa. `$cstr` represents a C `const char *` parameter. `$T` and `mut$T` parameters are permitted when `T` is an ABI-safe scalar or an extern struct, lowering to one pointer without changing ownership.

An `extern struct` is the explicit C layout boundary for aggregates. It is non-generic and non-empty. Its fields can contain ABI-safe scalars, nested extern structs, and non-empty fixed arrays composed from those types. The x86-64 System V C ABI determines field order, alignment, padding, array stride, and tail padding. Ordinary structs and enums remain compiler-owned representations and stay rejected in foreign declarations.

Casa `str`, `String`, `Bytes`, C string views, collections, function values, borrowed fields, ordinary structs, and enums are rejected as extern struct fields. Foreign pointer and string results use `ptr`. Borrowed return types are rejected because foreign code supplies no compiler-verifiable origin.

An extern struct can cross a native call by value. One- or two-eightbyte values
use `INTEGER`, `SSE`, or mixed register classes. `MEMORY` parameters use aligned
native stack arguments, and `MEMORY` returns use caller-owned hidden storage. A
by-value parameter must implement `Copy`. A return becomes an owned Casa value.
