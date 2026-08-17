# Raw pointers are opaque non-owning addresses

`ptr` is one nullable, non-owning, `Copy` raw-address type. It carries no element type, ownership, or source lifetime. Safe code may store, copy, compare, and test raw pointers for null, but cannot dereference them, form safe borrows from them, or perform pointer arithmetic.

Casa keeps the existing standalone `load8`, `load16`, `load32`, `load64`, `store8`, `store16`, `store32`, and `store64` intrinsics rather than duplicating them as `ptr` methods. Under ADR-0129 they operate only on their exact unsigned integer widths. Existing pointer `+` and `-` operations also remain and take `u64` byte offsets under ADR-0128. Loads, stores, pointer arithmetic, and the new `ptr::as_ref[T]` and `ptr::as_mut[T]` operations require an `unsafe` context. ADR-0122 later adds `ptr::read[T]` and `ptr::write[T]` as unsafe ownership-moving operations for initialized generic storage.

`ptr::as_ref[T]` produces `$T`; `ptr::as_mut[T]` produces `mut$T`. The caller must establish that the address is non-null, correctly aligned, points to a valid live `T` for the complete inferred borrow, and, for `as_mut`, is exclusively accessible. These operations do not create ownership or copy the borrowed value. ADR-0121 adds the safe reverse operation `ptr::from_ref`, which obtains the raw address of either borrow kind without preserving its lifetime.

## Considered options

- A typed `ptr[T]` catches some mismatched foreign pointers, but still cannot prove validity, lifetime, alignment, or exclusivity and adds another generic pointer family before concrete FFI needs justify it.
- A general cast inside `unsafe` provides the same low-level power, but obscures whether code is converting an address, forming a borrow, or reinterpreting representation.
- Namespacing every memory intrinsic under `ptr` makes related operations discoverable, but duplicates an existing concise surface without improving safety.
- Reusing existing intrinsics and adding only the missing typed-borrow operations is the smallest auditable design.

## Consequences

- `(ptr)` and all other general casts remain removed.
- `ptr::null` is the canonical null value. Pointer equality is safe; null testing needs no separate intrinsic.
- Existing loads, stores, and pointer arithmetic are retained but become unsafe operations.
- Pointer-pointer subtraction is not provided; raw `+` and `-` use `u64` byte offsets and remain within one allocation or its one-past address.
- Raw pointers never free storage and cannot be implicitly converted to or from `$T`, `mut$T`, `$cstr`, or an owned value. `ptr::from_ref`, `ptr::as_ref[T]`, and `ptr::as_mut[T]` are explicit named operations.
- Generic raw `read[T]` and `write[T]` move valid initialized values under explicit unsafe preconditions; they never bless arbitrary bits or copy an owning representation.
- Integer-address conversion and typed raw pointers remain deferred until a concrete syscall or foreign interface requires them.
- Safe wrappers validate external conditions once and expose borrows tied to an existing owner or copy validated data into an owned Casa value.
