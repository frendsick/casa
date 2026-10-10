# Operating-System APIs

Import the Linux operating-system module with a library path that contains
`os.casa`:

```casa
import "std"
import "os"
```

For example, compile from this repository with `casac -L lib program.casa`.

Reference tables abbreviate library type names and list inputs in consumption
order. Source examples use qualified names. See [reference notation](notation.md)
for signatures, fragments, and commands for running complete examples.

## Errors

High-level file and directory operations return
[Result[T IoError]](optional-values-and-errors.md#result).

Operations without a success payload return `Ok(std::Unit::Value)`. See
[Unit success values](optional-values-and-errors.md#unit-success-values) for
propagation, matching, and migration from `Result[bool IoError]`.

| Variant | Meaning |
|---|---|
| `IoError::AlreadyExists` | Target already exists |
| `IoError::BadFd` | Invalid file descriptor |
| `IoError::IsDirectory` | A file operation received a directory |
| `IoError::NotDirectory` | A directory operation received another type |
| `IoError::NotEmpty` | Directory is not empty |
| `IoError::NotFound` | Path does not exist |
| `IoError::Other(errno)` | Other Linux error number |
| `IoError::PermissionDenied` | Operation is not permitted |

`IoError` implements `Display`. Its `to_str` and `format` methods return a short
message.

## Files

Prefer the high-level functions:

| Function | Result |
|---|---|
| `os::file::exists path:$cstr -> bool` | Whether `stat` can find the path |
| `os::file::read_all path:$cstr -> Result[Bytes IoError]` | Read bytes until EOF |
| `os::file::remove path:$cstr -> Result[Unit IoError]` | Remove a file |
| `os::file::stat path:$cstr -> Result[FileStat IoError]` | File metadata |
| `os::file::write_all path:$cstr content:$Bytes -> Result[Unit IoError]` | Create or replace a file |

Handle the operation result directly. A separate existence check can become
stale before the next file operation:

```casa
import "std"
import "os"

"notes.txt".as_cstr.unwrap os::file::read_all match
    std::Result::Ok(bytes) => bytes.to_str.unwrap print
    std::Result::Error(error) => f"read failed: {error}" std::eprintln_string
end
```

### os::file::read_all

```text
fn read_all path:$cstr -> Result[Bytes IoError]
```

Opens `path` for reading and collects bytes until EOF. It accepts non-seekable
files and virtual files whose metadata reports zero size. Open and read errors
return `Error`. The function closes its file descriptor on success or error.

Reads can block until data or EOF is available. The result must fit in memory.
Concurrent file changes can affect the returned bytes, so this is not a snapshot.

### FileStat

`FileStat` has `size`, `mode`, `mtime`, `atime`, and `ctime` fields. It also
provides these checks. `os::file::stat` follows symbolic links, so its result describes
the target rather than the link. Permission helpers inspect owner mode bits.
They do not check effective access for the current process:

| Method | Signature | Description |
|---|---|---|
| [is_dir](#filestatis_dir) | `fn is_dir self:$FileStat -> bool` | Directory |
| [is_executable](#filestatis_executable) | `fn is_executable self:$FileStat -> bool` | Owner-executable mode bit |
| [is_file](#filestatis_file) | `fn is_file self:$FileStat -> bool` | Regular file |
| [is_readable](#filestatis_readable) | `fn is_readable self:$FileStat -> bool` | Owner-readable mode bit |
| [is_symlink](#filestatis_symlink) | `fn is_symlink self:$FileStat -> bool` | Symbolic link |
| [is_writable](#filestatis_writable) | `fn is_writable self:$FileStat -> bool` | Owner-writable mode bit |

### FileStat::is_dir

```text
fn is_dir self:$FileStat -> bool
```

Returns whether the entry is a directory.

### FileStat::is_executable

```text
fn is_executable self:$FileStat -> bool
```

Returns whether the owner-executable mode bit is set.

### FileStat::is_file

```text
fn is_file self:$FileStat -> bool
```

Returns whether the entry is a regular file.

### FileStat::is_readable

```text
fn is_readable self:$FileStat -> bool
```

Returns whether the owner-readable mode bit is set.

### FileStat::is_symlink

```text
fn is_symlink self:$FileStat -> bool
```

Returns whether the entry is a symbolic link.

### FileStat::is_writable

```text
fn is_writable self:$FileStat -> bool
```

Returns whether the owner-writable mode bit is set.


The complete [OS example](../examples/os_interaction.casa) creates, inspects,
and removes a file and directory.

## Directories

| Function | Result |
|---|---|
| `os::dir::change path:$cstr -> Result[Unit IoError]` | Change working directory |
| `os::dir::create path:$cstr mode:i64 -> Result[Unit IoError]` | Create a directory |
| `os::dir::current -> Result[Bytes IoError]` | Current working directory |
| `os::dir::exists path:$cstr -> bool` | Whether the path is a directory |
| `os::dir::list path:$cstr -> Result[List[Bytes] IoError]` | Entry names without `.` or `..` |
| `os::dir::remove path:$cstr -> Result[Unit IoError]` | Remove an empty directory |

The mode is a Linux permission value. For example, `493` is octal `0755`.

## Environment and paths

`os::env::get name:$str -> Option[Bytes]` returns one environment variable.
Environment values and directory names can contain any non-NUL byte. Convert
them with `Bytes.to_str` only when the caller requires UTF-8 text. Filesystem
operations accept `$cstr`, so both text paths and byte paths use `as_cstr`.
Environment variable names and text path utilities remain `$str`.

| Path function | Result |
|---|---|
| `os::path::basename path:$str -> String` | Final component |
| `os::path::dirname path:$str -> String` | Parent portion |
| `os::path::extension path:$str -> String` | Final extension without `.` |
| `os::path::join child:$str parent:$str -> String` | Join with one `/` |

```casa
import "std"
import "os"

"HOME" os::env::get
    .unwrap
    .to_str
    .unwrap print
"tmp" "report.txt" os::path::join print # tmp/report.txt
"src/main.casa" os::path::extension print # casa
```

See the [OS example](../examples/os_interaction.casa) for files, directories,
environment variables, paths, and a child process.

## Arguments and processes

`std::process::args -> List[Bytes]` copies all process arguments. `argc` is the
argument count and `std::get_arg index:u64 -> Bytes` copies one argument. Index `0`
is the program name. An invalid index terminates the program.

`std::process::exit status:u8` terminates immediately with the supplied status. It
does not unwind or run cleanup. Normal root completion exits with status zero.

```casa
import "std"

2 std::process::exit
```

`std::run_command arguments:List[Bytes] -> i64` starts a process and waits for it. The
first list element is the executable path:

```casa
import "std"

std::List[std::Bytes]::new = command
"/bin/echo" command.push_str
"hello" command.push_str
command std::run_command = exit_code
```

The executable path is passed directly to `execve`. There is no `PATH` search.
An argument with an interior NUL returns `-22` before launch. The return value
extracts bits 8–15 of the wait status. It does not distinguish signal termination
or report launch and wait errors through `Result`. A fork failure terminates the
calling process, and an exec failure exits the child with status `1`.

See the [argument parser example](../examples/argparse.casa) for a command-line
interface and the [OS example](../examples/os_interaction.casa) for
`run_command`.

## Standard input

Import `io` for a buffered standard-input reader:

```casa
import "std"
import "io"

io::StdinReader::new = reader
reader io::stdin_read_line = line
line.to_str.unwrap print
```

| Function | Result |
|---|---|
| `io::stdin_read_byte reader:mut$StdinReader -> i64` | Byte value, or `-1` at end of input |
| `io::stdin_read_exact reader:mut$StdinReader count:u64 -> Bytes` | Up to `count` bytes |
| `io::stdin_read_line reader:mut$StdinReader -> Bytes` | Bytes before a newline or end of input |

The reader's buffer and counters are private. `reader.has_buffered_input`
reports whether an unread byte is buffered. `reader.buffer` returns a shared
RawBuffer borrow for low-level access, with no storage replacement or counter
setters. Raw memory access still requires `unsafe`.

Read errors are treated as end of input. `stdin_read_exact` can return fewer
bytes than requested. Check the returned length when the protocol requires an
exact count. `stdin_read_line` consumes the newline without including it in the
result.

## Advanced file descriptors

The module also exposes direct Linux file-descriptor operations:

| Function | Result |
|---|---|
| `os::errno_to_io_error result:i64 -> IoError` | Convert a negative result |
| `os::file::close fd:i64 -> i64` | Zero or negative error |
| `os::file::open path:$cstr flags:i64 mode:i64 -> i64` | File descriptor or negative error |
| `os::file::read fd:i64 buffer:ptr size:u64 -> i64` | Unsafe. Bytes read or negative error |
| `os::file::write fd:i64 data:$Bytes -> i64` | Bytes written or negative error |

Open flags are `os::O_RDONLY`, `os::O_WRONLY`, `os::O_CREAT`, and `os::O_TRUNC`. Combine flags
with `|`. Prefer the high-level `Result` functions unless direct descriptors
are required.

`os::file::read` requires an `unsafe` block. The caller must provide `size` writable
bytes at `buffer` with exclusive access for the call. Prefer `read_all` for
checked reads into owned `Bytes`.
