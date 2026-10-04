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

High-level file and directory operations return `Result[T IoError]`.

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
| `file::exists path:$cstr -> bool` | Whether `stat` can find the path |
| `file::read_all path:$cstr -> Result[Bytes IoError]` | Entire file contents |
| `file::remove path:$cstr -> Result[bool IoError]` | Remove a file |
| `file::stat path:$cstr -> Result[FileStat IoError]` | File metadata |
| `file::write_all path:$cstr content:$Bytes -> Result[bool IoError]` | Create or replace a file |

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

`FileStat` has `size`, `mode`, `mtime`, `atime`, and `ctime` fields. It also
provides these checks:

| Method | Signature | Behavior |
|---|---|---|
| [is_dir](#filestatis_dir) | `fn is_dir self:$FileStat -> bool` | Directory |
| [is_executable](#filestatis_executable) | `fn is_executable self:$FileStat -> bool` | Owner-executable mode bit |
| [is_file](#filestatis_file) | `fn is_file self:$FileStat -> bool` | Regular file |
| [is_readable](#filestatis_readable) | `fn is_readable self:$FileStat -> bool` | Owner-readable mode bit |
| [is_symlink](#filestatis_symlink) | `fn is_symlink self:$FileStat -> bool` | Symbolic link |
| [is_writable](#filestatis_writable) | `fn is_writable self:$FileStat -> bool` | Owner-writable mode bit |

<a id="filestatis_dir"></a>

### is_dir

```text
fn is_dir self:$FileStat -> bool
```

Returns whether the entry is a directory.

<a id="filestatis_executable"></a>

### is_executable

```text
fn is_executable self:$FileStat -> bool
```

Returns whether the owner-executable mode bit is set.

<a id="filestatis_file"></a>

### is_file

```text
fn is_file self:$FileStat -> bool
```

Returns whether the entry is a regular file.

<a id="filestatis_readable"></a>

### is_readable

```text
fn is_readable self:$FileStat -> bool
```

Returns whether the owner-readable mode bit is set.

<a id="filestatis_symlink"></a>

### is_symlink

```text
fn is_symlink self:$FileStat -> bool
```

Returns whether the entry is a symbolic link.

<a id="filestatis_writable"></a>

### is_writable

```text
fn is_writable self:$FileStat -> bool
```

Returns whether the owner-writable mode bit is set.


The complete [OS example](../examples/os_interaction.casa) creates, inspects,
and removes a file and directory.

## Directories

| Function | Result |
|---|---|
| `dir::change path:$cstr -> Result[bool IoError]` | Change working directory |
| `dir::create path:$cstr mode:i64 -> Result[bool IoError]` | Create a directory |
| `dir::current -> Result[Bytes IoError]` | Current working directory |
| `dir::exists path:$cstr -> bool` | Whether the path is a directory |
| `dir::list path:$cstr -> Result[List[Bytes] IoError]` | Entry names without `.` or `..` |
| `dir::remove path:$cstr -> Result[bool IoError]` | Remove an empty directory |

The mode is a Linux permission value. For example, `493` is octal `0755`.

## Environment and paths

`env::get name:$str -> Option[Bytes]` returns one environment variable.
Environment values and directory names can contain any non-NUL byte. Convert
them with `Bytes.to_str` only when the caller requires UTF-8 text. Filesystem
operations accept `$cstr`, so both text paths and byte paths use `as_cstr`.
Environment variable names and text path utilities remain `$str`.

| Path function | Result |
|---|---|
| `path::basename path:$str -> String` | Final component |
| `path::dirname path:$str -> String` | Parent portion |
| `path::extension path:$str -> String` | Final extension without `.` |
| `path::join child:$str parent:$str -> String` | Join with one `/` |

```casa
import "std"
import "os"

"HOME" os::env::get .unwrap.to_str.unwrap print
"tmp" "report.txt" os::path::join print    # tmp/report.txt
"src/main.casa" os::path::extension print  # casa
```

See the [OS example](../examples/os_interaction.casa) for files, directories,
environment variables, paths, and a child process.

## Arguments and processes

`process::args -> List[Bytes]` copies all process arguments. `argc` is the
argument count and `get_arg index:u64 -> Bytes` copies one argument. Index `0`
is the program name. An invalid index terminates the program.

`process::exit status:u8` terminates immediately with the supplied status. It
does not unwind or run cleanup. Normal root completion exits with status zero.

```casa
import "std"

2 process::exit
```

`run_command arguments:List[Bytes] -> i64` starts a process and waits for it. The
first list element is the executable path:

```casa
import "std"

std::List[std::Bytes]::new = command
"/bin/echo" command.push_str
"hello" command.push_str
command std::run_command = exit_code
```

See the [argument parser example](../examples/argparse.casa) for a command-line
interface and the [OS example](../examples/os_interaction.casa) for
`run_command`.

## Advanced file descriptors

The module also exposes direct Linux file-descriptor operations:

| Function | Result |
|---|---|
| `errno_to_io_error result:i64 -> IoError` | Convert a negative result |
| `file::close fd:i64 -> i64` | Zero or negative error |
| `file::open path:$cstr flags:i64 mode:i64 -> i64` | File descriptor or negative error |
| `file::read fd:i64 buffer:ptr size:u64 -> i64` | Bytes read or negative error |
| `file::write fd:i64 data:$Bytes -> i64` | Bytes written or negative error |

Open flags are `O_RDONLY`, `O_WRONLY`, `O_CREAT`, and `O_TRUNC`. Combine flags
with `|`. Prefer the high-level `Result` functions unless direct descriptors
are required.
