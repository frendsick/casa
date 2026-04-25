# Standard Library

The standard library provides core types and functions used by most Casa programs. It is in `lib/std.casa`. Import it as a module:

```casa
import "std"
```

…and pass `-L path/to/lib` to `casac`. Or import by path:

```casa
import "path/to/lib/std.casa"
```

## `import` Directive

`import` loads another Casa source file. Each file is imported at most once, regardless of how many times it appears, and the deduplication uses the canonicalized resolved path.

There are two forms:

### Path-style

```casa
import "relative/path/to/file.casa"
import "/absolute/path/to/file.casa"
```

A specifier is treated as a path when it contains `/` or ends with `.casa`. Relative paths resolve from the directory of the importing file. Absolute paths are used as-is. No search is performed.

### Module-style

```casa
import "std"
```

A specifier without `/` and without a `.casa` suffix is treated as a module name. The resolver looks for `<module>.casa` in:

1. the directory of the importing file, then
2. each directory passed via `-L` / `--library-path`, in CLI order.

The first existing match wins. A same-directory candidate that resolves to the importing file itself is skipped, so an example file `examples/argparse.casa` can `import "argparse"` and reach the library copy via `-L`. If no candidate exists, the compiler reports an error listing every directory searched.

### `-L` / `--library-path`

Repeatable. Adds a directory to the module search path:

```sh
casac -L lib program.casa
casac -L lib -L vendor program.casa
```

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

## Iteration

All standard collections provide a `.iter` method that returns an `Iter[T]` value. `Iter[T]` satisfies the `Iterable[T]` trait (see [Traits](traits.md#built-in-trait-iterablet)), so it inherits all default methods: `collect`, `map`, `filter`, `fold`, `count`, `any`, `all`, and `find`.

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

**Signature:** `Iter::collect self:Iter[T] -> List[T]`

```casa
[1 2 3].iter.collect    # List[int] with elements 1, 2, 3
```

#### `map`

Applies a function to each element, returning a `List[U]`.

**Signature:** `Iter::map[U] self:Iter[T] f:fn[T -> U] -> List[U]`

```casa
{ 2 * } [1 2 3].iter.map    # List[int] with elements 2, 4, 6
```

#### `filter`

Returns a `List[T]` of elements for which the function returns `true`.

**Signature:** `Iter::filter self:Iter[T] f:fn[T -> bool] -> List[T]`

```casa
{ 2 % 0 == } [1 2 3 4].iter.filter    # List[int] with elements 2, 4
```

#### `fold`

Reduces to a single value using an accumulator and a function.

**Signature:** `Iter::fold[U] self:Iter[T] acc:U f:fn[U T -> U] -> U`

```casa
{ + } 0 [1 2 3].iter.fold print    # 6
```

#### `count`

Returns the number of elements.

**Signature:** `Iter::count self:Iter[T] -> int`

```casa
[1 2 3].iter.count print    # 3
```

#### `any`

Returns `true` if any element satisfies the predicate.

**Signature:** `Iter::any self:Iter[T] f:fn[T -> bool] -> bool`

```casa
{ 3 == } [1 2 3].iter.any print    # 1
```

#### `all`

Returns `true` if all elements satisfy the predicate.

**Signature:** `Iter::all self:Iter[T] f:fn[T -> bool] -> bool`

```casa
{ 0 < } [1 2 3].iter.all print    # 1
```

#### `find`

Returns the first element satisfying the predicate, or `Option::None`.

**Signature:** `Iter::find self:Iter[T] f:fn[T -> bool] -> Option[T]`

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
