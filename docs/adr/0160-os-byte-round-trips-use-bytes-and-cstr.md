# OS byte round trips use Bytes and cstr
related issue: #592

Casa uses `Bytes` for owned Linux data without a UTF-8 guarantee and `$cstr` for
borrowed NUL-terminated system-call inputs. Filesystem operations accept `$cstr`
as their only path representation. `str.as_cstr` and `Bytes.as_cstr` reject an
interior NUL and lend the same path type without an implicit text conversion.
`Bytes` keeps a trailing NUL outside its logical length so this borrow does not
allocate.

Process arguments and file contents use `Bytes` in both directions.
`process::args` and `run_command` use `List[Bytes]`, and byte-oriented file
reads and writes use `Bytes`. `Bytes::from_str` provides an explicit,
infallible text-to-bytes copy, and `List[Bytes].push_str` covers common text
arguments. `Bytes` converts to text only through `to_str`. It validates UTF-8,
copies valid text into `String`, and preserves the source.

`Bytes` implements `Eq` and `Hashable` so OS values support identity checks and
map or set keys. It does not implement `Display` because arbitrary bytes have no
text encoding. Byte-oriented output remains explicit.

Casa does not add `OsString`, `OsStr`, `Path`, duplicate raw-path functions, or
implicit `str` and `Bytes` coercions. The text path utilities remain text-only.
Raw path construction uses `Bytes::from_str`, `clone`, `push`, and `append`.
Environment variable names remain `$str` until an API produces raw names that
need lossless round trips.

## Considered options

- Text-only path inputs leave valid results from `dir::list` and `dir::current`
  unusable as inputs to other safe filesystem operations.
- Separate raw-path functions duplicate every filesystem operation and let the
  text and byte surfaces drift.
- A `Path` or `OsString` wrapper adds no invariant on Casa's byte-native Linux
  target.
- Accepting `$cstr` reuses the existing NUL validation and borrow lifetime for
  both text and byte storage.

## Consequences

- A path or process argument containing an interior NUL fails before the system
  call.
- `Bytes` storage uses one hidden trailing byte without changing `length` or
  `capacity` semantics. Interior NUL bytes remain valid byte data.
- Existing text path, process, and file-write call sites require explicit
  conversion during implementation.
- A consuming byte-to-text conversion and byte-specific path utilities remain
  deferred until measured copying or repeated path code justifies them.
