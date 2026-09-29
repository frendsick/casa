# Linux OS strings are Bytes
status: amended by [ADR-0160](0160-os-byte-round-trips-use-bytes-and-cstr.md)

Linux process arguments, environment values, and directory entry names have no
UTF-8 guarantee. Safe APIs return owned `Bytes`, preserving the exact input
without exposing borrowed process/syscall buffer lifetimes. `to_str` validates
and copies into an owned `String` when the application requires text.

Returning text would reject, replace, or misrepresent valid Linux values.
`OsString` and `OsStr` would add no invariant on this byte-native target. A
native-string abstraction waits for a platform with a different representation.

NUL-terminated interfaces exclude the terminator from logical byte length.
These byte values do not gain Display by assuming an encoding. Programs may
inspect, compare, hash, or deliberately render them.

[ADR-0160](0160-os-byte-round-trips-use-bytes-and-cstr.md) replaces the initial
text-only path restriction. Filesystem operations accept one Linux path argument,
`$cstr`, which NUL-free text or bytes can lend. There is no `Path` type, duplicate
raw-path API, or implicit text/byte coercion. Environment keys remain `$str`.
