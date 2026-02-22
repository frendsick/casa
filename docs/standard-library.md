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

## `Arena`

A bump allocator that hands out pointers from a single pre-allocated buffer. Allocations are fast (just a pointer bump) and the entire arena can be reset at once instead of freeing individual allocations.

### Definition

```casa
struct Arena {
    buffer: ptr
    size: int
    offset: int
}
```

### `Arena::new`

Creates a new arena with the given number of heap slots.

**Signature:** `Arena::new _arena_size:int -> Arena`

**Stack effect:** `int -> Arena`

```casa
10 Arena::new = arena
```

### `Arena::alloc`

Allocates `n` contiguous slots from the arena and returns a pointer to the first slot.

**Signature:** `Arena::alloc self:Arena n:int -> ptr`

**Stack effect:** `Arena int -> ptr`

```casa
3 arena.alloc = ptr1
42 ptr1 store
99 ptr1 1 + store
7 ptr1 2 + store
```

### `Arena::reset`

Resets the arena offset to zero, making the entire buffer available for reuse. Previously returned pointers should not be used after a reset.

**Signature:** `Arena::reset self:Arena`

**Stack effect:** `Arena -> None`

```casa
arena.reset
```

### `Arena::used`

Returns the number of slots currently allocated.

**Signature:** `Arena::used self:Arena -> int`

**Stack effect:** `Arena -> int`

```casa
arena.used print    # 0 on a fresh arena
```

### `Arena::available`

Returns the number of slots remaining in the arena.

**Signature:** `Arena::available self:Arena -> int`

**Stack effect:** `Arena -> int`

```casa
arena.available print    # equals size on a fresh arena
```

### Complete Example

```casa
include "../lib/std.casa"

10 Arena::new = arena
arena.used print          # 0
arena.available print     # 10

3 arena.alloc = ptr1
42 ptr1 store
99 ptr1 1 + store
7 ptr1 2 + store

arena.used print          # 3
arena.available print     # 7

arena.reset
arena.used print          # 0
arena.available print     # 10

4 arena.alloc = ptr2
1 ptr2 store
2 ptr2 1 + store
3 ptr2 2 + store
4 ptr2 3 + store

arena.used print          # 4
arena.available print     # 6
```

See [`examples/arena.casa`](../examples/arena.casa) for a full program using the Arena.

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
