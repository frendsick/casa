# Linux OS strings are Bytes

On Casa's Linux target, process arguments, environment values, directory entry names, and similar OS-provided strings without a UTF-8 guarantee enter safe code as owned `Bytes`. Callers use `to_str` to validate and create an owned `String` when their domain expects text. Casa does not add `OsString` or `OsStr`; on Linux those types would wrap the same arbitrary byte sequences without adding a new invariant.

The initial APIs include `process::args -> List[Bytes]`, `env::get key:$str -> Option[Bytes]`, and `dir::list path:$str -> Result[List[Bytes] IoError]`. Filesystem paths and environment keys initially accept only `$str`. Casa deliberately defers raw `Bytes` path inputs, a `Path` type, duplicate raw-path functions, and implicit `str`/`Bytes` coercions.

## Considered options

- Returning `str` keeps common programs concise, but either admits invalid UTF-8 or silently rejects/replaces valid Linux values.
- Adding `OsString` anticipates non-byte-native platforms, but Casa currently targets Linux x86-64 and has no second representation to abstract.
- Returning borrowed views into process or syscall buffers avoids copies, but exposes storage lifetimes throughout otherwise simple APIs.
- Returning owned `Bytes` preserves every Linux value and follows the existing raw-input boundary.

## Consequences

- Text-oriented programs validate explicitly through `to_str`.
- These APIs do not implement `Display` by assuming an encoding. Programs may inspect, compare, hash, or deliberately render their bytes.
- Values obtained from NUL-terminated OS interfaces exclude the terminator from their logical length.
- Environment keys and filesystem path inputs remain text-only. Consequently, safe code initially cannot pass a non-UTF-8 argument or directory entry back to a filesystem API.
- Raw path support requires a concrete use to settle interior-NUL validation and how path-validation failures compose with `IoError`; it is not added speculatively.
- Cross-platform support may introduce a native-string abstraction only when a target cannot represent its OS strings as bytes.
