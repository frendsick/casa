# Standard Library

The standard library is in `lib/std.casa`. Include it with:

```casa
include "../lib/std.casa"
```

## `include` Directive

`include` loads another Casa source file. Each file is included at most once, regardless of how many times it appears.

```casa
include "relative/path/to/file.casa"
```

Paths are resolved relative to the file containing the `include` directive. All functions, structs, and global variables defined in the included file become available.

## `memcpy`

Copies `n` bytes from one pointer to another.

**Signature:** `memcpy dst:ptr src:ptr n:int`

**Stack effect:** `dst src n -> None`

```casa
24 alloc = src
10 src (ptr) store64
20 src (ptr) 8 + store64
30 src (ptr) 16 + store64

24 alloc = dst
24 src dst memcpy

dst (ptr) load64 print        # 10
dst (ptr) 8 + load64 print    # 20
dst (ptr) 16 + load64 print   # 30
```

## Arrays

Arrays are fixed-size heap-allocated sequences created with bracket syntax. Each array has a 16-byte header: the data pointer at offset 0 and the length at offset 8. Elements are stored contiguously after the header, each taking 8 bytes.

### `array::length`

Returns the length of an array.

**Signature:** `array::length array -> int`

**Stack effect:** `array -> int`

```casa
[1, 2, 3] = arr
arr.length print    # 3
```

### `array::nth`

Returns the nth element of an array (zero-indexed). This is a generic function that returns the element type of the array.

**Signature:** `array::nth[T] array:array[T] n:int -> T`

**Stack effect:** `array[T] n -> T`

```casa
[10, 20, 30] = arr
1 arr array::nth print    # 20
# or with dot syntax:
1 arr.nth print           # 20
```

The return type matches the array's element type. For example, calling `nth` on an `array[int]` returns `int`, not `any`.

### `array::map`

Applies a function to each element, returning a new array with the results.

**Signature:** `array::map[T1 T2] arr:array[T1] f:fn[T1 -> T2] -> array[T2]`

**Stack effect:** `array[T1] fn[T1 -> T2] -> array[T2]`

```casa
{ 2 * } [1, 2, 3].map
# result: [2, 4, 6]
```

The return type is determined by the function's return type. For example, mapping `fn[int -> str]` over an `array[int]` produces `array[str]`.

### `array::filter`

Returns a new array containing only elements for which the function returns `true`.

**Signature:** `array::filter[T] arr:array[T] f:fn[T -> bool] -> array[T]`

**Stack effect:** `array[T] fn[T -> bool] -> array[T]`

```casa
{ 2 % 0 == } [1, 2, 3, 4].filter
# result: [2, 4]
```

### `array::reduce`

Reduces an array to a single value by applying a function to an accumulator and each element.

**Signature:** `array::reduce[T1 T2] arr:array[T1] acc:T2 f:fn[T2 T1 -> T2] -> T2`

**Stack effect:** `array[T1] T2 fn[T2 T1 -> T2] -> T2`

The accumulator is the initial value. The function receives the current accumulator and the current element, and returns the new accumulator.

```casa
{ + } 0 [1, 2, 3].reduce print    # 6
```

The accumulator type and the array element type can differ.

## Option

Methods for working with `option[T]` values. Method calls on any `option[T]` receiver are resolved to `option::method`.

### `option::is_some`

Returns `true` if the option contains a value.

**Signature:** `option::is_some self:option -> bool`

**Stack effect:** `option -> bool`

```casa
42 some .is_some print    # 1
none .is_some print       # 0
```

### `option::is_none`

Returns `true` if the option is empty.

**Signature:** `option::is_none self:option -> bool`

**Stack effect:** `option -> bool`

```casa
42 some .is_none print    # 0
none .is_none print       # 1
```

### `option::unwrap`

Extracts the contained value. Prints an error and exits with code 60 if called on `none`.

**Signature:** `option::unwrap[T] self:option[T] -> T`

**Stack effect:** `option[T] -> T`

```casa
42 some .unwrap print     # 42
none .unwrap              # error: called unwrap on None
```

### `option::unwrap_or`

Returns the contained value, or a default if the option is empty.

**Signature:** `option::unwrap_or[T] self:option[T] default:T -> T`

**Stack effect:** `option[T] T -> T`

```casa
0 42 some .unwrap_or print    # 42
0 none .unwrap_or print       # 0
```

## `List[T]`

A generic dynamic list that grows automatically when items are pushed. The type parameter `T` tracks the element type at compile time.

### Definition

```casa
struct List {
    data:     ptr
    size:     int
    capacity: int
}
```

### `List::new`

Creates an empty `List` with an initial capacity of 8.

**Signature:** `List::new -> List`

**Stack effect:** `-> List`

```casa
List::new = v
```

### `List::from_array`

Creates a `List[T]` from a fixed-size array. The list's size and capacity are both set to the array's length. The data pointer points directly into the array's data.

**Signature:** `List::from_array[T] arr:array[T] -> List[T]`

**Stack effect:** `array[T] -> List[T]`

```casa
[1, 2, 3] List::from_array = list
```

### `List::length`

Returns the number of elements in the list.

**Signature:** `List::length self:List -> int`

**Stack effect:** `List -> int`

```casa
list.length print    # 3
```

### `List::get`

Returns the element at index `n` (zero-indexed). Prints an error and exits if the index is out of bounds.

**Signature:** `List::get[T] self:List[T] n:int -> T`

**Stack effect:** `List[T] int -> T`

```casa
0 list.get print    # 1
2 list.get print    # 3
```

The return type matches the list's element type. For example, calling `get` on a `List[int]` returns `int`.

### `List::set`

Sets the element at index `n`. Prints an error and exits if the index is out of bounds.

**Signature:** `List::set[T] self:List[T] n:int item:T`

**Stack effect:** `List[T] int T -> None`

```casa
99 1 list.set
1 list.get print    # 99
```

### `List::push`

Appends an item to the list. If the list is at capacity, it allocates a new buffer with double the capacity and copies the existing elements.

**Signature:** `List::push[T] self:List[T] item:T`

**Stack effect:** `List[T] T -> None`

```casa
4 list.push
list.length print    # 4
3 list.get print     # 4
```

### `List::pop`

Removes and returns the last element. Prints an error and exits if the list is empty.

**Signature:** `List::pop[T] self:List[T] -> T`

**Stack effect:** `List[T] -> T`

```casa
list.pop print          # 4
list.length print       # 3
```

### `List::slice`

Returns an `array[T]` view into the list's data from index `start` (inclusive) to `end` (exclusive). This is a zero-copy operation. Prints an error and exits if the range is out of bounds.

**Signature:** `List::slice[T] self:List[T] start:int end:int -> array[T]`

**Stack effect:** `List[T] int int -> array[T]`

```casa
3 1 list.slice = sliced
sliced.length print          # 2
0 sliced array::nth print    # 200
```

### `List::to_array`

Returns an `array[T]` view of the entire list. This is a zero-copy operation equivalent to `0 self.size self.slice`.

**Signature:** `List::to_array[T] self:List[T] -> array[T]`

**Stack effect:** `List[T] -> array[T]`

```casa
list.to_array = arr
arr.length print    # 3
```

### Complete Example

```casa
include "../lib/std.casa"

List::new = v
10 v.push
20 v.push
30 v.push
v.length print          # 3
0 v.get print           # 10
2 v.get print           # 30

99 1 v.set
1 v.get print           # 99

v.pop print             # 30
v.length print          # 2

[100, 200, 300] List::from_array = v2
3 1 v2.slice = sliced
sliced.length print     # 2
```

See [`examples/vec.casa`](../examples/vec.casa) for a full program demonstrating all List methods.

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
0 O_RDONLY 0 "input.txt" file::open = fd
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

**Signature:** `fn is_digit c:char -> bool`

**Stack effect:** `char -> bool`

```casa
'0'.is_digit print    # 1
'A'.is_digit print    # 0
```

### `char::is_upper`

Returns `true` if the character is an uppercase ASCII letter (`'A'`-`'Z'`).

**Signature:** `fn is_upper c:char -> bool`

**Stack effect:** `char -> bool`

```casa
'A'.is_upper print    # 1
'a'.is_upper print    # 0
```

### `char::is_lower`

Returns `true` if the character is a lowercase ASCII letter (`'a'`-`'z'`).

**Signature:** `fn is_lower c:char -> bool`

**Stack effect:** `char -> bool`

```casa
'a'.is_lower print    # 1
'A'.is_lower print    # 0
```

### `char::is_alpha`

Returns `true` if the character is an ASCII letter (uppercase or lowercase).

**Signature:** `fn is_alpha c:char -> bool`

**Stack effect:** `char -> bool`

```casa
'A'.is_alpha print    # 1
'0'.is_alpha print    # 0
```

### `char::is_space`

Returns `true` if the character is ASCII whitespace (space, tab, newline, or carriage return).

**Signature:** `fn is_space c:char -> bool`

**Stack effect:** `char -> bool`

```casa
' '.is_space print    # 1
'A'.is_space print    # 0
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
