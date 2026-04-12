# Standard Library

The standard library provides core types and functions used by most Casa programs. It is in `lib/std.casa`. Include it with:

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

`Option[T]` is an enum representing a value that may or may not exist. It is defined as:

```casa
enum Option[T] { None Some(T) }
```

Methods are resolved via `Option::method`.

### `Option::is_some`

Returns `true` if the option contains a value.

**Signature:** `Option::is_some self:Option -> bool`

**Stack effect:** `Option -> bool`

```casa
42 Option::Some .is_some print       # 1
Option::None .is_some print          # 0
```

### `Option::is_none`

Returns `true` if the option is empty.

**Signature:** `Option::is_none self:Option -> bool`

**Stack effect:** `Option -> bool`

```casa
42 Option::Some .is_none print       # 0
Option::None .is_none print          # 1
```

### `Option::unwrap`

Extracts the contained value. Prints an error and exits with code 60 if called on `None`.

**Signature:** `Option::unwrap[T] self:Option[T] -> T`

**Stack effect:** `Option[T] -> T`

```casa
42 Option::Some .unwrap print        # 42
Option::None .unwrap                 # error: called unwrap on None
```

### `Option::unwrap_or`

Returns the contained value, or a default if the option is empty.

**Signature:** `Option::unwrap_or[T] self:Option[T] default:T -> T`

**Stack effect:** `Option[T] T -> T`

```casa
0 42 Option::Some .unwrap_or print   # 42
0 Option::None .unwrap_or print      # 0
```

### Match on Option

Use `match` with destructuring to handle Option values:

```casa
42 Option::Some match
    Option::Some(value) => value print
    Option::None => "nothing" print
end
```

## Result

`Result[T E]` is an enum representing success or failure. It is defined as:

```casa
enum Result[T E] { Error(E) Ok(T) }
```

Methods are resolved via `Result::method`.

### `Result::is_ok`

Returns `true` if the result contains a success value.

**Signature:** `Result::is_ok self:Result -> bool`

**Stack effect:** `Result -> bool`

```casa
42 Result::Ok .is_ok print                  # 1
"error" Result::Error .is_ok print          # 0
```

### `Result::is_error`

Returns `true` if the result contains an error value.

**Signature:** `Result::is_error self:Result -> bool`

**Stack effect:** `Result -> bool`

```casa
42 Result::Ok .is_error print               # 0
"error" Result::Error .is_error print       # 1
```

### `Result::unwrap`

Extracts the success value. Prints an error and exits with code 60 if called on an `Error` result.

**Signature:** `Result::unwrap[T E] self:Result[T E] -> T`

**Stack effect:** `Result[T E] -> T`

```casa
42 Result::Ok .unwrap print                 # 42
"error" Result::Error .unwrap               # error: called unwrap on error
```

### `Result::unwrap_error`

Extracts the error value. Prints an error and exits with code 60 if called on an `Ok` result.

**Signature:** `Result::unwrap_error[T E] self:Result[T E] -> E`

**Stack effect:** `Result[T E] -> E`

```casa
"error" Result::Error .unwrap_error print   # error
42 Result::Ok .unwrap_error                 # error: called unwrap_error on ok
```

### `Result::unwrap_or`

Returns the success value, or a default if the result is an error.

**Signature:** `Result::unwrap_or[T E] self:Result[T E] default:T -> T`

**Stack effect:** `Result[T E] T -> T`

```casa
0 42 Result::Ok .unwrap_or print            # 42
0 "error" Result::Error .unwrap_or print    # 0
```

### Match on Result

Use `match` with destructuring to handle Result values:

```casa
42 Result::Ok match
    Result::Ok(value) => value print
    Result::Error(err) => err print
end
```

## See Also

- [Collections](collections.md) -- List, Map, Set, and StringBuilder
- [Strings and IO](strings-and-io.md) -- string methods, file I/O, type conversions, and output functions
- [Utilities](utilities.md) -- logging, timer, argument parsing, and process execution
- [Types and Literals](types-and-literals.md) -- primitive and composite type definitions
