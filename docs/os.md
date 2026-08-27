# Operating-System APIs

Import the Linux operating-system module with a library path that contains
`os.casa`:

```casa
import "os"
```

For example, compile from this repository with `casac -L lib program.casa`.

## Errors

High-level file and directory operations return `Result[T IoError]`.

| Variant | Meaning |
|---|---|
| `IoError::NotFound` | Path does not exist |
| `IoError::PermissionDenied` | Operation is not permitted |
| `IoError::AlreadyExists` | Target already exists |
| `IoError::IsDirectory` | A file operation received a directory |
| `IoError::NotDirectory` | A directory operation received another type |
| `IoError::NotEmpty` | Directory is not empty |
| `IoError::BadFd` | Invalid file descriptor |
| `IoError::Other(errno)` | Other Linux error number |

`IoError` implements `Display`. Its `to_str` and `format` methods return a short
message.

## Files

Prefer the high-level functions:

| Function | Result |
|---|---|
| `file::read_all path:$str -> Result[String IoError]` | Entire file contents |
| `file::write_all path:$str content:$str -> Result[bool IoError]` | Create or replace a file |
| `file::remove path:$str -> Result[bool IoError]` | Remove a file |
| `file::exists path:$str -> bool` | Whether `stat` can find the path |
| `file::stat path:$str -> Result[FileStat IoError]` | File metadata |

Handle the operation result directly. A separate existence check can become
stale before the next file operation:

```casa
"notes.txt" file::read_all match
    Result::Ok(text) => text print
    Result::Error(error) => f"read failed: {error}\n" eprint
end
```

`FileStat` has `size`, `mode`, `mtime`, `atime`, and `ctime` fields. It also
provides these checks:

| Method | Meaning |
|---|---|
| `is_dir` | Directory |
| `is_file` | Regular file |
| `is_symlink` | Symbolic link |
| `is_readable` | Owner-readable mode bit |
| `is_writable` | Owner-writable mode bit |
| `is_executable` | Owner-executable mode bit |

The complete [OS example](../examples/os_interaction.casa) creates, inspects,
and removes a file and directory.

## Directories

| Function | Result |
|---|---|
| `dir::list path:$str -> Result[List[String] IoError]` | Entry names without `.` or `..` |
| `dir::create path:$str mode:i64 -> Result[bool IoError]` | Create a directory |
| `dir::remove path:$str -> Result[bool IoError]` | Remove an empty directory |
| `dir::exists path:$str -> bool` | Whether the path is a directory |
| `dir::current -> Result[String IoError]` | Current working directory |
| `dir::change path:$str -> Result[bool IoError]` | Change working directory |

The mode is a Linux permission value. For example, `493` is octal `0755`.

## Environment and paths

`env::get name:$str -> Option[String]` returns one environment variable.

| Path function | Result |
|---|---|
| `path::join child:$str parent:$str -> String` | Join with one `/` |
| `path::dirname path:$str -> String` | Parent portion |
| `path::basename path:$str -> String` | Final component |
| `path::extension path:$str -> String` | Final extension without `.` |

```casa
"HOME" env::get .unwrap print
"tmp" "report.txt" path::join print    # tmp/report.txt
"src/main.casa" path::extension print  # casa
```

See the [OS example](../examples/os_interaction.casa) for files, directories,
environment variables, paths, and a child process.

## Arguments and processes

`argc` is the argument count and `get_arg index:u64 -> String` returns an argument.
Index `0` is the program name. An invalid index terminates the program.

`run_command arguments:List[String] -> i64` starts a process and waits for it. The
first list element is the executable path:

```casa
List[String]::new = command
"/bin/echo" command.push_str
"hello" command.push_str
command run_command = exit_code
```

See the [argument parser example](../examples/argparse.casa) for a command-line
interface and the [OS example](../examples/os_interaction.casa) for
`run_command`.

## Advanced file descriptors

The module also exposes direct Linux file-descriptor operations:

| Function | Result |
|---|---|
| `file::open path:str flags:i64 mode:i64 -> i64` | File descriptor or negative error |
| `file::read fd:i64 buffer:ptr size:u64 -> i64` | Bytes read or negative error |
| `file::write fd:i64 data:str -> i64` | Bytes written or negative error |
| `file::close fd:i64 -> i64` | Zero or negative error |
| `errno_to_io_error result:i64 -> IoError` | Convert a negative result |

Open flags are `O_RDONLY`, `O_WRONLY`, `O_CREAT`, and `O_TRUNC`. Combine flags
with `|`. Prefer the high-level `Result` functions unless direct descriptors
are required.
