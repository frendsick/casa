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

Copies heap slots from one pointer to another.

**Signature:** `memcpy dst:ptr src:ptr n:int`

**Stack effect:** `dst src n -> None`

```casa
3 alloc = src
10 src store
20 src 1 + store
30 src 2 + store

3 alloc = dst
3 src dst memcpy

dst load print        # 10
dst 1 + load print    # 20
dst 2 + load print    # 30
```

## Arrays

Arrays are fixed-size heap-allocated sequences created with bracket syntax. The length is stored in the first heap slot, and elements start at offset 1.

### `array::length`

Returns the length of an array.

**Signature:** `array::length array -> int`

**Stack effect:** `array -> int`

```casa
[1, 2, 3] = arr
arr.length print    # 3
```

### `array::nth`

Returns the nth element of an array (zero-indexed).

**Signature:** `array::nth array:array n:int -> any`

**Stack effect:** `array n -> any`

```casa
[10, 20, 30] = arr
1 arr array::nth print    # 20
# or with dot syntax:
1 arr.nth print           # 20
```

> **Note:** The return type is `any`. Use a [type cast](types-and-literals.md#type-casting) if you need a specific type.

## `List`

A dynamic list that grows automatically when items are pushed.

### Definition

```casa
struct List {
    size:     int
    capacity: int
    array:    array
}
```

### `List::from_array`

Creates a `List` from a fixed-size array. The list's size and capacity are both set to the array's length.

**Signature:** `List::from_array array:array -> List`

**Stack effect:** `array -> List`

```casa
[1, 2, 3] List::from_array = list
```

### `List::nth`

Returns the nth element (zero-indexed).

**Signature:** `List::nth List int -> any`

**Stack effect:** `List int -> any`

```casa
0 list.nth print    # 1
2 list.nth print    # 3
```

### `List::push`

Appends an item to the list. If the list is at capacity, it allocates a new array with double the capacity and copies the existing elements.

**Signature:** `List::push self:List item:any`

**Stack effect:** `List any -> None`

```casa
4 list.push
list.size print       # 4
3 list.nth print      # 4
```

### Complete Example

```casa
include "../lib/std.casa"

[1, 2, 3] List::from_array = list
list.size print         # 3
list.capacity print     # 3

4 list.push
list.size print         # 4
list.capacity print     # 6

0 list.nth print        # 1
3 list.nth print        # 4
```

See [`examples/dynamic_list.casa`](../examples/dynamic_list.casa) for a full program using the List.

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
