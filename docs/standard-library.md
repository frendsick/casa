# Standard Library

The standard library provides core types and functions used by most Casa programs. It is in `lib/std.casa`. Import it as a module:

```casa
import "std"
```

…and pass `-L path/to/lib` to `casac`. Or import by path:

```casa
import "path/to/lib/std.casa"
```

For full import resolution rules, selective imports, and `-L` search paths, see [Modules](modules.md).

## `memcpy`

Copies `n` bytes from one pointer to another.

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

**Stack effect:** `array -> int`

```casa
[1, 2, 3] = arr
arr.length print    # 3
```

### `array::nth`

Returns the nth element of an array (zero-indexed). This is a generic function that returns the element type of the array.

**Stack effect:** `array[T] n -> T`

```casa
[10, 20, 30] = arr
1 arr array::nth print    # 20
# or with dot syntax:
1 arr.nth print           # 20
```

The return type matches the array's element type. For example, calling `nth` on an `array[int]` returns `int`, not `any`.

## Iteration

All standard collections provide a `.iter` method that returns an `Iter[T]` value. `Iter[T]` satisfies the `Iterable[T]` trait (see [Traits](traits.md#built-in-trait-iterablet)), so the default methods `collect`, `map`, `filter`, `fold`, `count`, `any`, `all`, and `find` are available.

### `Iter[T]`

A generic iterator wrapper returned by `.iter` on `array[T]`, `List[T]`, and `str`.

**Definition:**

```casa
struct Iter {
    get:    ptr
    length: int
    index:  int
}
```

### `.iter` on Collections

| Collection | Method | Returns |
|------------|--------|---------|
| `array[T]` | `array::iter` | `Iter[T]` |
| `List[T]` | `List::iter` | `Iter[T]` |
| `str` | `str::iter` | `Iter[char]` |

```casa
[1 2 3].iter           # Iter[int]
my_list.iter           # Iter[T]
"hello".iter           # Iter[char]
```

### `Iterable[T]` Default Methods

Default methods are available on any type satisfying `Iterable[T]`, including `Iter[T]` (the return of `.iter`) and custom iterators.

#### `collect`

Collects all elements into a `List[T]`.

**Stack effect:** `Iter[T] -> List[T]`

```casa
[1 2 3].iter.collect    # List[int] with elements 1, 2, 3
```

#### `map`

Applies a function to each element, returning a lazy `Iter[U]`. Use `.collect` to materialize a `List[U]`.

**Stack effect:** `Iter[T] fn[T -> U] -> Iter[U]`

```casa
{ 2 * } [1 2 3].iter.map.collect    # List[int] with elements 2, 4, 6
```

#### `filter`

Returns a lazy `Iter[T]` of elements for which the function returns `true`. Use `.collect` to materialize a `List[T]`.

**Stack effect:** `Iter[T] fn[T -> bool] -> Iter[T]`

```casa
{ 2 % 0 == } [1 2 3 4].iter.filter.collect    # List[int] with elements 2, 4
```

#### `fold`

Reduces to a single value using an accumulator and a function.

**Stack effect:** `Iter[T] U fn[U T -> U] -> U`

```casa
{ + } 0 [1 2 3].iter.fold print    # 6
```

#### `count`

Returns the number of elements.

**Stack effect:** `Iter[T] -> int`

```casa
[1 2 3].iter.count print    # 3
```

#### `any`

Returns `true` if any element satisfies the predicate.

**Stack effect:** `Iter[T] fn[T -> bool] -> bool`

```casa
{ 3 == } [1 2 3].iter.any print    # true
```

#### `all`

Returns `true` if all elements satisfy the predicate.

**Stack effect:** `Iter[T] fn[T -> bool] -> bool`

```casa
{ 0 < } [1 2 3].iter.all print    # true
```

#### `find`

Returns the first element satisfying the predicate, or `Option::None`.

**Stack effect:** `Iter[T] fn[T -> bool] -> Option[T]`

```casa
{ 2 < } [1 2 3].iter.find .unwrap print    # 2
```

## Option

`Option[T]` is an enum representing a value that may or may not exist. It is defined as:

```casa
enum Option[T] { None Some(T) }
```

Methods are resolved via `Option::method`.

### `Option::is_some`

Returns `true` if the option contains a value.

**Stack effect:** `Option -> bool`

```casa
42 Option::Some .is_some print       # true
Option::None .is_some print          # false
```

### `Option::is_none`

Returns `true` if the option is empty.

**Stack effect:** `Option -> bool`

```casa
42 Option::Some .is_none print       # false
Option::None .is_none print          # true
```

### `Option::unwrap`

Extracts the contained value. Prints an error and exits with code 60 if called on `None`.

**Stack effect:** `Option[T] -> T`

```casa
42 Option::Some .unwrap print        # 42
Option::None .unwrap                 # error: called unwrap on None
```

### `Option::unwrap_or`

Returns the contained value, or a default if the option is empty.

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

**Stack effect:** `Result -> bool`

```casa
42 Result::Ok .is_ok print                  # true
"error" Result::Error .is_ok print          # false
```

### `Result::is_error`

Returns `true` if the result contains an error value.

**Stack effect:** `Result -> bool`

```casa
42 Result::Ok .is_error print               # false
"error" Result::Error .is_error print       # true
```

### `Result::unwrap`

Extracts the success value. Prints an error and exits with code 60 if called on an `Error` result.

**Stack effect:** `Result[T E] -> T`

```casa
42 Result::Ok .unwrap print                 # 42
"error" Result::Error .unwrap               # error: called unwrap on error
```

### `Result::unwrap_error`

Extracts the error value. Prints an error and exits with code 60 if called on an `Ok` result.

**Stack effect:** `Result[T E] -> E`

```casa
"error" Result::Error .unwrap_error print   # error
42 Result::Ok .unwrap_error                 # error: called unwrap_error on ok
```

### `Result::unwrap_or`

Returns the success value, or a default if the result is an error.

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

## Built-in Traits

The standard library declares traits with primitive implementations. See [Traits](traits.md) for details.

| Trait | Required | Defaults | Built-in impls |
|-------|----------|----------|----------------|
| `Eq` | `eq self other -> bool` | `ne` | `int`, `bool`, `char`, `str`, `cstr`, `ptr` |
| `Ord` | `lt self other -> bool` | `le`, `gt`, `ge` | `int`, `char` |
| `Display: Word` | `to_str self -> str` (extends `Word`) | -- | `int`, `bool`, `char`, `str`, `cstr`, `ptr`, `array[T]`, `List[T]`, `Option[T]`, `Result[T E]` |
| `Word` | (marker) | -- | every single-slot type |
| `Hashable: Eq + Word` | `hash self -> int` (extends `Eq`, `Word`) | -- | `int`, `str`, payload-free enums (auto-derived) |
| `Iterable[T]` | `next self -> Option[T]` | `collect`, `map`, `filter`, `fold`, `count`, `any`, `all`, `find` | `Iter[T]` |

## See Also

- [Collections](collections.md) -- List, Map, Set, and StringBuilder
- [Strings and IO](strings-and-io.md) -- string methods, file I/O, type conversions, and output functions
- [Utilities](utilities.md) -- logging, timer, argument parsing, and process execution
- [Types and Literals](types-and-literals.md) -- primitive and composite type definitions
