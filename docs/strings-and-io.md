# Strings and IO

String operations, character classification, file I/O, output functions, and type conversions. All functions are defined in `lib/std.casa` and available via `import`.

## String Methods

Methods for working with `str` values. Method calls on any `str` receiver are resolved to `str::method`.

### `str::length`

Returns the length of a string in bytes.

**Signature:** `str::length s:str -> int`

**Stack effect:** `str -> int`

```casa
"hello".length print    # 5
"".length print         # 0
```

### `str::at`

Returns the character at the given index.

**Signature:** `str::at s:str index:int -> char`

**Stack effect:** `str int -> char`

```casa
0 "hello".at print      # h
4 "hello".at print      # o
```

### `str::set`

Writes a character at the given index in a string (mutates in place).

**Signature:** `str::set s:str index:int c:char`

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

**Signature:** `str::as_cstr s:str -> cstr`

**Stack effect:** `str -> cstr`

```casa
"hello" .as_cstr print    # hello
```

### `str::eq`

Compares two strings by content. Returns `true` if they have the same length and identical bytes.

**Signature:** `str::eq b:str a:str -> bool`

**Stack effect:** `str str -> bool`

```casa
"hello" "hello" str::eq print    # 1
"hello" "world" str::eq print    # 0
```

### `str::substring`

Extracts a substring starting at `start` with the given `len`.

**Signature:** `str::substring len:int start:int s:str -> str`

**Stack effect:** `int int str -> str`

```casa
3 1 "hello".substring print    # ell
```

### `str::find`

Finds the first occurrence of `needle` in the string. Returns the index, or -1 if not found.

**Signature:** `str::find needle:str s:str -> int`

**Stack effect:** `str str -> int`

```casa
"lo" "hello".find print     # 3
"xyz" "hello".find print    # -1
```

### `str::starts_with`

Returns `true` if the string starts with the given prefix.

**Signature:** `str::starts_with prefix:str s:str -> bool`

**Stack effect:** `str str -> bool`

```casa
"hel" "hello".starts_with print    # 1
"xyz" "hello".starts_with print    # 0
```

### `str::ends_with`

Returns `true` if the string ends with the given suffix.

**Signature:** `str::ends_with suffix:str s:str -> bool`

**Stack effect:** `str str -> bool`

```casa
"llo" "hello".ends_with print    # 1
"xyz" "hello".ends_with print    # 0
```

### `str::concat`

Concatenates two strings, returning a new string.

**Signature:** `str::concat b:str a:str -> str`

**Stack effect:** `str str -> str`

```casa
"world" "hello " str::concat print    # hello world
```

## C String Methods

Methods for working with `cstr` values. Method calls on any `cstr` receiver are resolved to `cstr::method`.

### `cstr::to_str`

Converts a null-terminated C string to a `str`. Scans for the null byte to determine length, then allocates a new string with a length prefix and copies the bytes.

**Signature:** `cstr::to_str s:cstr -> str`

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

**Signature:** `file::open path:str flags:int mode:int -> int`

**Stack effect:** `str int int -> int`

```casa
0 O_RDONLY "input.txt" file::open = fd
```

The `mode` parameter sets file permissions when creating a new file (e.g., 420 for `rw-r--r--`). It is ignored when opening an existing file.

### `file::read`

Reads up to `size` bytes from a file descriptor into a buffer. Returns the number of bytes read, or a negative value on error.

**Signature:** `file::read fd:int buf:ptr size:int -> int`

**Stack effect:** `int ptr int -> int`

```casa
1024 alloc = buf
1024 buf fd file::read = bytes_read
```

### `file::write`

Writes a string to a file descriptor. Returns the number of bytes written, or a negative value on error.

**Signature:** `file::write fd:int data:str -> int`

**Stack effect:** `int str -> int`

```casa
"Hello, file!\n" fd file::write drop
```

### `file::close`

Closes a file descriptor. Returns 0 on success, or a negative value on error.

**Signature:** `file::close fd:int -> int`

**Stack effect:** `int -> int`

```casa
fd file::close drop
```

### `file::read_all`

Reads the entire contents of a file into a string. Prints an error and exits if the file cannot be opened.

**Signature:** `file::read_all path:str -> str`

**Stack effect:** `str -> str`

```casa
"input.txt" file::read_all = content
content print
```

### `file::write_all`

Writes a string to a file, creating or truncating it. Returns `true` on success, `false` if the file cannot be opened.

**Signature:** `file::write_all path:str content:str -> bool`

**Stack effect:** `str str -> bool`

```casa
"Hello, world!\n" "output.txt" file::write_all drop
```

### `file::remove`

Deletes a file. Returns 0 on success, or a negative value on error.

**Signature:** `file::remove path:str -> int`

**Stack effect:** `str -> int`

```casa
"temp.txt" file::remove drop
```

See [`examples/file_io.casa`](../examples/file_io.casa) for a full program using file I/O.

## Character Classification

Methods on `char` for classifying ASCII characters.

### `char::is_digit`

Returns `true` if the character is an ASCII digit (`'0'`-`'9'`).

**Signature:** `char::is_digit c:char -> bool`

**Stack effect:** `char -> bool`

```casa
'0'.is_digit print    # 1
'A'.is_digit print    # 0
```

### `char::is_upper`

Returns `true` if the character is an uppercase ASCII letter (`'A'`-`'Z'`).

**Signature:** `char::is_upper c:char -> bool`

**Stack effect:** `char -> bool`

```casa
'A'.is_upper print    # 1
'a'.is_upper print    # 0
```

### `char::is_lower`

Returns `true` if the character is a lowercase ASCII letter (`'a'`-`'z'`).

**Signature:** `char::is_lower c:char -> bool`

**Stack effect:** `char -> bool`

```casa
'a'.is_lower print    # 1
'A'.is_lower print    # 0
```

### `char::is_alpha`

Returns `true` if the character is an ASCII letter (uppercase or lowercase).

**Signature:** `char::is_alpha c:char -> bool`

**Stack effect:** `char -> bool`

```casa
'A'.is_alpha print    # 1
'0'.is_alpha print    # 0
```

### `char::is_space`

Returns `true` if the character is ASCII whitespace (space, tab, newline, or carriage return).

**Signature:** `char::is_space c:char -> bool`

**Stack effect:** `char -> bool`

```casa
' '.is_space print    # 1
'A'.is_space print    # 0
```

## Stdout Output

### `println`

Writes a string followed by a newline to stdout.

**Signature:** `println msg:str`

**Stack effect:** `str -> None`

```casa
"Hello world!" println
```

## Stderr Output

### `eprint`

Writes a string to stderr (file descriptor 2).

**Signature:** `eprint msg:str`

**Stack effect:** `str -> None`

```casa
"warning: something happened\n" eprint
```

### `eprintln`

Writes a string followed by a newline to stderr.

**Signature:** `eprintln msg:str`

**Stack effect:** `str -> None`

```casa
"error: file not found" eprintln
```

## Type Conversions

Convert values to their string representation. All `to_str` methods can be called with dot syntax (e.g., `42.to_str`).

### `digit_to_str`

Converts a single digit (0-9) to its string representation. This is a helper used internally by `int::to_str`.

**Signature:** `digit_to_str d:int -> str`

**Stack effect:** `int -> str`

```casa
5 digit_to_str print    # 5
```

### `int::to_str`

Converts an integer to its string representation. Handles negative numbers and zero.

**Signature:** `int::to_str n:int -> str`

**Stack effect:** `int -> str`

```casa
42.to_str print         # 42
0.to_str print          # 0
-123.to_str print       # -123
```

### `bool::to_str`

Converts a boolean to `"true"` or `"false"`.

**Signature:** `bool::to_str b:bool -> str`

**Stack effect:** `bool -> str`

```casa
true.to_str print       # true
false.to_str print      # false
```

### `str::to_str`

Identity function. Returns the string unchanged.

**Signature:** `str::to_str s:str -> str`

**Stack effect:** `str -> str`

```casa
"hello".to_str print    # hello
```

### `ptr::to_str`

Converts a pointer to a string by casting its address to an integer and converting that.

**Signature:** `ptr::to_str p:ptr -> str`

**Stack effect:** `ptr -> str`

```casa
10 alloc = buf
buf.to_str print        # prints the address as a decimal number
```

### `char::to_str`

Wraps a single character in a one-character string.

**Signature:** `char::to_str c:char -> str`

**Stack effect:** `char -> str`

```casa
'A'.to_str print    # A
```

### `array[T]::to_str`

Formats an array as `[elem1, elem2, ...]`. Requires `T` to satisfy the `Display` trait.

**Signature:** `array::to_str self:array[T] -> str` (with `T: Display`)

```casa
[1, 2, 3] (array[int]) .to_str print    # [1, 2, 3]
```

### `List[T]::to_str`

Formats a `List` the same way as an array. Requires `T` to satisfy the `Display` trait.

**Signature:** `List::to_str self:List[T] -> str` (with `T: Display`)

### `Option[T]::to_str`

Formats an `Option` as `Some(value)` or `None`. Requires `T` to satisfy the `Display` trait.

**Signature:** `Option::to_str self:Option[T] -> str` (with `T: Display`)

```casa
5 Option::Some .to_str print                # Some(5)
Option::None (Option[int]) .to_str print    # None
```

### `Result[T E]::to_str`

Formats a `Result` as `Ok(value)` or `Error(err)`. Requires both `T` and `E` to satisfy the `Display` trait.

**Signature:** `Result::to_str self:Result[T E] -> str` (with `T: Display, E: Display`)

```casa
99 Result::Ok (Result[int str]) .to_str print       # Ok(99)
"oops" Result::Error (Result[int str]) .to_str print # Error(oops)
```

## Hash Helpers

Standalone hash functions used by the built-in `Hashable` trait implementations.

### `str_hash`

Computes a hash for a string using the djb2 algorithm. Returns a non-negative integer.

**Signature:** `str_hash s:str -> int`

**Stack effect:** `str -> int`

```casa
"hello" str_hash print    # prints hash value
```

### `int_hash`

Returns the absolute value of an integer, for use as a hash.

**Signature:** `int_hash n:int -> int`

**Stack effect:** `int -> int`

```casa
-42 int_hash print    # 42
42 int_hash print     # 42
```

## See Also

- [Standard Library](standard-library.md) -- arrays, Option, Result, and the `import` directive
- [Collections](collections.md) -- List, Map, Set, and StringBuilder
- [Traits](traits.md) -- the `Display` trait used by type conversions
- [Types and Literals](types-and-literals.md) -- primitive type definitions
