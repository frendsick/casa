# Extern functions use an ABI-safe type whitelist

The `extern fn` surface accepts fixed-width integers, `f32`, `f64`, `bool`, and `ptr`. On Casa's x86-64 target, C `size_t` is written `u64` and `ssize_t` is written `i64`. Casa `bool` represents C `_Bool`; parameters contain 0 or 1, and returns are normalized before they enter Casa. `$cstr` represents a C `const char *` parameter. `$T` and `mut$T` parameters are permitted only when `T` is an ABI-safe scalar, lowering to one pointer without changing ownership.

Casa `str`, `Bytes`, collections, Unicode `char`, structs, and enums are rejected in foreign declarations because their representations are not C contracts. Casa initially adds no `repr(C)` or other user-controlled layout feature; aggregates cross the boundary through `ptr` and safe wrappers.

Foreign pointer and string results use `ptr`. Borrowed return types are rejected because foreign code supplies no compiler-verifiable origin; an unsafe wrapper establishes a lifetime tied to an existing owner or copies validated data into an owned value. This whitelist prevents ABI mismatches without making ordinary safe layouts part of Casa's public ABI.
