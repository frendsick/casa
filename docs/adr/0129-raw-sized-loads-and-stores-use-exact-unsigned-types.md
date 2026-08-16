# Raw sized loads and stores use exact unsigned types

Raw sized memory operations use the unsigned integer type matching their width:

| Operation | Stack effect |
|---|---|
| `load8` | `ptr -> u8` |
| `load16` | `ptr -> u16` |
| `load32` | `ptr -> u32` |
| `load64` | `ptr -> u64` |
| `store8` | `ptr u8 -> None` |
| `store16` | `ptr u16 -> None` |
| `store32` | `ptr u32 -> None` |
| `store64` | `ptr u64 -> None` |

The store effects follow Casa's topmost-input-first notation: source writes place the value below the address, such as `byte address store8`.

These unsafe operations read or write raw numeric bits. Signed integers, floating-point values, pointers, and owned values use `ptr::read[T]` and `ptr::write[T]` with their explicit type and ownership contracts.

## Consequences

- `load8` no longer zero-extends into an unrelated `i64`; it produces `u8` directly.
- Raw stores accept no generic `Word` capability and cannot write arbitrary one-word representations.
- Integer-width conversion remains explicit and separate from memory access.
- The compiler lowers each operation to one exact-width load or store without runtime type metadata.
- The caller must provide enough accessible bytes. ADR-0130 permits unaligned addresses for these raw integer operations on x86-64; typed raw operations retain natural alignment.
