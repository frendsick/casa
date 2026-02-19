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

**Stack effect:** `( dst src n -- )`

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

**Stack effect:** `( array -- int )`

```casa
[1, 2, 3] = arr
arr.length print    # 3
```

### `array::nth`

Returns the nth element of an array (zero-indexed).

**Signature:** `array::nth array:array n:int -> any`

**Stack effect:** `( array n -- any )`

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

**Stack effect:** `( array -- List )`

```casa
[1, 2, 3] List::from_array = list
```

### `List::nth`

Returns the nth element (zero-indexed).

**Signature:** `List::nth List int -> any`

**Stack effect:** `( List int -- any )`

```casa
0 list.nth print    # 1
2 list.nth print    # 3
```

### `List::push`

Appends an item to the list. If the list is at capacity, it allocates a new array with double the capacity and copies the existing elements.

**Signature:** `List::push self:List item:any`

**Stack effect:** `( List any -- )`

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
