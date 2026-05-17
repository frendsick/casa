# Strings and IO

String operations, character classification, file I/O, output functions, and type conversions. All functions are defined in `lib/std.casa` and available via `import`.

## String Methods

Methods for working with `str` values. Method calls on any `str` receiver are resolved to `str::method`.

### `str::length`

Returns the length of a string in bytes.

**Stack effect:** `str -> int`

```casa
"hello".length print    # 5
"".length print         # 0
```

### `str::at`

Returns the character at the given index.

**Stack effect:** `str int -> char`

```casa
0 "hello".at print      # h
4 "hello".at print      # o
```

### `str::set`

Writes a character at the given index in a string (mutates in place).

**Stack effect:** `str int char -> None`

```casa
14 alloc (str) = buf
5 buf (ptr) store64
'H' 0 buf.set
'i' 1 buf.set
'\0' 2 buf.set
buf print    # Hi
```

### `str::as_cstr`

Returns a `cstr` pointing to the string's byte data (skipping the 8-byte length prefix). The returned `cstr` is null-terminated.

**Stack effect:** `str -> cstr`

```casa
"hello" .as_cstr print    # hello
```

### `str::eq`

Compares two strings by content. Returns `true` if they have the same length and identical bytes.

**Stack effect:** `str str -> bool`

```casa
"hello" "hello" str::eq print    # true
"hello" "world" str::eq print    # false
```

### `str::substring`

Extracts a substring starting at `start` with the given `len`.

**Stack effect:** `int int str -> str`

```casa
3 1 "hello".substring print    # ell
```

### `str::find`

Finds the first occurrence of `needle` in the string. Returns the index, or -1 if not found.

**Stack effect:** `str str -> int`

```casa
"lo" "hello".find print     # 3
"xyz" "hello".find print    # -1
```

### `str::starts_with`

Returns `true` if the string starts with the given prefix.

**Stack effect:** `str str -> bool`

```casa
"hel" "hello".starts_with print    # true
"xyz" "hello".starts_with print    # false
```

### `str::ends_with`

Returns `true` if the string ends with the given suffix.

**Stack effect:** `str str -> bool`

```casa
"llo" "hello".ends_with print    # true
"xyz" "hello".ends_with print    # false
```

### `str::concat`

Concatenates two strings, returning a new string.

**Stack effect:** `str str -> str`

```casa
"world" "hello " str::concat print    # hello world
```

### `str::contains`

Returns `true` if the string contains the given substring.

**Stack effect:** `str str -> bool`

```casa
"ell" "hello".contains print    # true
"xyz" "hello".contains print    # false
```

### `str::split`

Splits a string by a delimiter, returning a `List[str]` of the parts.

**Stack effect:** `str str -> List[str]`

```casa
"," "a,b,c".split = parts
parts.length print    # 3
0 parts.get print     # a
```

### `str::trim`

Removes leading and trailing whitespace (spaces, tabs, newlines, carriage returns).

**Stack effect:** `str -> str`

```casa
"  hello  ".trim print    # hello
```

### `str::replace`

Replaces all occurrences of `old` with `new_str`, returning a new string.

**Stack effect:** `str str str -> str`

```casa
"world" "there" "hello there".replace print    # hello world
```

## C String Methods

Methods for working with `cstr` values. Method calls on any `cstr` receiver are resolved to `cstr::method`.

### `cstr::to_str`

Converts a null-terminated C string to a `str`. Scans for the null byte to determine length, then allocates a new string with a length prefix and copies the bytes.

**Stack effect:** `cstr -> str`

```casa
"hello" .as_cstr .to_str print    # hello
```

## File I/O

Functions for reading, writing, and managing files. All file operations use Linux system calls internally.

### File I/O Constants

The standard library provides constants for common file open flags:

| Constant | Value | Description |
|----------|-------|-------------|
| `O_RDONLY` | 0 | Open for reading only |
| `O_WRONLY` | 1 | Open for writing only |
| `O_CREAT` | 64 | Create the file if it does not exist |
| `O_TRUNC` | 512 | Truncate the file to zero length |

Flags can be combined with the bitwise OR operator (`|`):

```casa
O_WRONLY O_CREAT | O_TRUNC |    # open for writing, create if needed, truncate
```

### `file::open`

Opens a file and returns a file descriptor. Returns a negative value on error.

**Stack effect:** `str int int -> int`

```casa
0 O_RDONLY "input.txt" file::open = fd
```

The `mode` parameter sets file permissions when creating a new file (e.g., 420 for `rw-r--r--`). It is ignored when opening an existing file.

### `file::read`

Reads up to `size` bytes from a file descriptor into a buffer. Returns the number of bytes read, or a negative value on error.

**Stack effect:** `int ptr int -> int`

```casa
1024 alloc = buf
1024 buf fd file::read = bytes_read
```

### `file::write`

Writes a string to a file descriptor. Returns the number of bytes written, or a negative value on error.

**Stack effect:** `int str -> int`

```casa
"Hello, file!\n" fd file::write drop
```

### `file::close`

Closes a file descriptor. Returns 0 on success, or a negative value on error.

**Stack effect:** `int -> int`

```casa
fd file::close drop
```

### `FileError`

The high-level file operations (`file::read_all`, `file::write_all`, `file::remove`) report failures through a `FileError` enum wrapped in `Result`:

```casa
enum FileError {
    NotFound
    PermissionDenied
    AlreadyExists
    IsDirectory
    NotDirectory
    BadFd
    Other (int)    # raw errno for cases not covered above
}
```

`FileError` implements `Display`, so it can be printed with `to_str` or interpolated in f-strings. The free function `errno_to_file_error` converts a negative syscall return value to the appropriate `FileError`.

### `file::read_all`

Reads the entire contents of a file into a string. Returns `Result::Ok(content)` on success or `Result::Error(FileError)` if the file cannot be opened or read.

**Stack effect:** `str -> Result[str FileError]`

```casa
"input.txt" file::read_all match
    Result::Ok(content)  => content print
    Result::Error(read_err) => f"read failed: {read_err.to_str}\n" eprint
end
```

Match directly on the `Result` rather than probing with `file::exists` first; the latter introduces a TOCTOU race because the file can disappear between the two calls.

### `file::write_all`

Writes a string to a file, creating or truncating it. Returns `Result::Ok(true)` on success or `Result::Error(FileError)` if the file cannot be opened.

**Stack effect:** `str str -> Result[bool FileError]`

```casa
"Hello, world!\n" "output.txt" file::write_all drop
```

### `file::remove`

Deletes a file. Returns `Result::Ok(true)` on success or `Result::Error(FileError)` if the syscall fails (e.g. the file is missing).

**Stack effect:** `str -> Result[bool FileError]`

```casa
"temp.txt" file::remove drop
```

### `file::exists`

Returns `true` when the path can be opened for reading, `false` otherwise (missing file, permission denied). Safe to use as a probe when the existence check is needed for control flow that does not later read the file (to read the file, match on `file::read_all` directly to avoid a TOCTOU race).

**Stack effect:** `str -> bool`

```casa
"/tmp/casa.lock" file::exists.to_str print
```

See [`examples/file_io.casa`](../examples/file_io.casa) for a full program using file I/O.

## Character Classification

Methods on `char` for classifying ASCII characters.

### `char::is_digit`

Returns `true` if the character is an ASCII digit (`'0'`-`'9'`).

**Stack effect:** `char -> bool`

```casa
'0'.is_digit print    # true
'A'.is_digit print    # false
```

### `char::is_upper`

Returns `true` if the character is an uppercase ASCII letter (`'A'`-`'Z'`).

**Stack effect:** `char -> bool`

```casa
'A'.is_upper print    # true
'a'.is_upper print    # false
```

### `char::is_lower`

Returns `true` if the character is a lowercase ASCII letter (`'a'`-`'z'`).

**Stack effect:** `char -> bool`

```casa
'a'.is_lower print    # true
'A'.is_lower print    # false
```

### `char::is_alpha`

Returns `true` if the character is an ASCII letter (uppercase or lowercase).

**Stack effect:** `char -> bool`

```casa
'A'.is_alpha print    # true
'0'.is_alpha print    # false
```

### `char::is_space`

Returns `true` if the character is ASCII whitespace (space, tab, newline, or carriage return).

**Stack effect:** `char -> bool`

```casa
' '.is_space print    # true
'A'.is_space print    # false
```

## Stdout Output

### `println`

Writes a string followed by a newline to stdout.

**Stack effect:** `str -> None`

```casa
"Hello world!" println
```

## Stderr Output

### `eprint`

Writes a string to stderr (file descriptor 2).

**Stack effect:** `str -> None`

```casa
"warning: something happened\n" eprint
```

### `eprintln`

Writes a string followed by a newline to stderr.

**Stack effect:** `str -> None`

```casa
"error: file not found" eprintln
```

## Type Conversions

Convert values to their string representation. All `to_str` methods can be called with dot syntax (e.g., `42.to_str`).

### `digit_to_str`

Converts a single digit (0-9) to its string representation. This is a helper used internally by `int::to_str`.

**Stack effect:** `int -> str`

```casa
5 digit_to_str print    # 5
```

### `int::to_str`

Converts an integer to its string representation. Handles negative numbers and zero.

**Stack effect:** `int -> str`

```casa
42.to_str print         # 42
0.to_str print          # 0
-123.to_str print       # -123
```

### `bool::to_str`

Converts a boolean to `"true"` or `"false"`.

**Stack effect:** `bool -> str`

```casa
true.to_str print       # true
false.to_str print      # false
```

### `str::to_str`

Identity function. Returns the string unchanged.

**Stack effect:** `str -> str`

```casa
"hello".to_str print    # hello
```

### `ptr::to_str`

Converts a pointer to a string by casting its address to an integer and converting that.

**Stack effect:** `ptr -> str`

```casa
10 alloc = buf
buf.to_str print        # prints the address as a decimal number
```

### `char::to_str`

Wraps a single character in a one-character string.

**Stack effect:** `char -> str`

```casa
'A'.to_str print    # A
```

### `array[T]::to_str`

Formats an array as `[elem1, elem2, ...]`. Requires `T` to satisfy the `Display` trait.

**Stack effect:** `[T: Display] array[T] -> str`

```casa
[1, 2, 3] (array[int]) .to_str print    # [1, 2, 3]
```

### `List[T]::to_str`

Formats a `List` the same way as an array. Requires `T` to satisfy the `Display` trait.

**Stack effect:** `[T: Display] List[T] -> str`

### `Option[T]::to_str`

Formats an `Option` as `Some(value)` or `None`. Requires `T` to satisfy the `Display` trait.

**Stack effect:** `[T: Display] Option[T] -> str`

```casa
5 Option::Some .to_str print                # Some(5)
Option::None (Option[int]) .to_str print    # None
```

### `Result[T E]::to_str`

Formats a `Result` as `Ok(value)` or `Error(err)`. Requires both `T` and `E` to satisfy the `Display` trait.

**Stack effect:** `[T: Display, E: Display] Result[T E] -> str`

```casa
99 Result::Ok (Result[int str]) .to_str print       # Ok(99)
"oops" Result::Error (Result[int str]) .to_str print # Error(oops)
```

## Hash Helpers

Standalone hash functions used by the built-in `Hashable` trait implementations.

### `str_hash`

Computes a hash for a string using the djb2 algorithm. Returns a non-negative integer.

**Stack effect:** `str -> int`

```casa
"hello" str_hash print    # prints hash value
```

### `int_hash`

Returns the absolute value of an integer, for use as a hash.

**Stack effect:** `int -> int`

```casa
-42 int_hash print    # 42
42 int_hash print     # 42
```

## See Also

- [Standard Library](standard-library.md) -- arrays, Option, and Result
- [Modules](modules.md) -- import directives and module resolution
- [Collections](collections.md) -- List, Map, Set, and StringBuilder
- [Traits](traits.md) -- the `Display` trait used by type conversions
- [Types and Literals](types-and-literals.md) -- primitive type definitions
