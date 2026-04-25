# Collections

Casa provides generic container types for dynamic data. All collections are defined in `lib/std.casa` and available via `import`.

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

Returns an `array[T]` view into the list's data from index `start` (inclusive) to `stop` (exclusive). This is a zero-copy operation. Prints an error and exits if the range is out of bounds.

**Signature:** `List::slice[T] self:List[T] start:int stop:int -> array[T]`

**Stack effect:** `List[T] int int -> array[T]`

```casa
3 1 list.slice = sliced
sliced.length print          # 2
0 sliced array::nth print    # 2
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
import "path/to/lib/std.casa"

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

## `Map[K V]`

A generic hash map using separate chaining. Keys must satisfy the `Hashable` trait (see [Traits](traits.md)). The type parameters `K` and `V` track key and value types at compile time.

### Definition

```casa
struct Map {
    buckets:  ptr
    size:     int
    capacity: int
}

impl[K: Hashable, V] Map[K V] { ... }
```

All Map methods are defined in an `impl` block with `K: Hashable` and `V` type parameters. Individual methods inherit these bounds.

### `Map::new`

Creates an empty map with an initial capacity of 16.

**Signature:** `Map::new -> Map[K V]`

**Stack effect:** `-> Map[K V]`

```casa
Map::new (Map[str int]) = m
```

The type cast `(Map[str int])` tells the compiler the concrete types for `K` and `V`. The compiler verifies that `str` satisfies `Hashable`.

### `Map::length`

Returns the number of key-value pairs in the map.

**Signature:** `Map::length self:Map[K V] -> int`

**Stack effect:** `Map[K V] -> int`

```casa
m.length print    # 0
```

### `Map::get`

Looks up a key and returns `Option[V]`. Returns `Option::Some` with the value if found, `Option::None` otherwise.

**Signature:** `Map::get self:Map[K V] key:K -> Option[V]`

**Stack effect:** `Map[K V] K -> Option[V]`

```casa
"hello" m.get .unwrap print    # prints the value for "hello"
"missing" m.get .is_none print # 1
```

### `Map::has`

Returns `true` if the key exists in the map.

**Signature:** `Map::has self:Map[K V] key:K -> bool`

**Stack effect:** `Map[K V] K -> bool`

```casa
"hello" m.has print    # true or false
```

### `Map::set`

Inserts or updates a key-value pair. Returns the updated map. Automatically resizes at 75% load factor.

**Signature:** `Map::set self:Map[K V] value:V key:K -> Map[K V]`

**Stack effect:** `Map[K V] V K -> Map[K V]`

```casa
"one" 1 m.set = m
```

Note: the key is on top of the stack, the value is below it, and the map is below the value.

### `Map::delete`

Removes a key from the map. Returns the updated map. If the key does not exist, the map is returned unchanged.

**Signature:** `Map::delete self:Map[K V] key:K -> Map[K V]`

**Stack effect:** `Map[K V] K -> Map[K V]`

```casa
"one" m.delete = m
```

### `Map::keys`

Returns a `List[K]` of all keys in the map.

**Signature:** `Map::keys self:Map[K V] -> List[K]`

**Stack effect:** `Map[K V] -> List[K]`

```casa
m.keys = key_list
```

### `Map::values`

Returns a `List[V]` of all values in the map.

**Signature:** `Map::values self:Map[K V] -> List[V]`

**Stack effect:** `Map[K V] -> List[V]`

```casa
m.values = val_list
```

### Complete Example

```casa
import "path/to/lib/std.casa"

# Create a map from strings to ints
Map::new (Map[str int]) = m

# Insert key-value pairs
"one" 1 m.set = m
"two" 2 m.set = m
"three" 3 m.set = m

# Look up values
"one" m.get .unwrap print      # 1
"two" m.get .unwrap print      # 2

# Check membership
"one" m.has print              # true
"four" m.has print             # false

# Update a value
"one" 42 m.set = m
"one" m.get .unwrap print      # 42

# Delete a key
"two" m.delete = m
m.length print                 # 2

# Integer keys work too
Map::new (Map[int str]) = m2
1 "hello" m2.set = m2
1 m2.get .unwrap print         # hello
```

See [`examples/hash_map.casa`](../examples/hash_map.casa) for a full program.

## `Set[K]`

A generic hash set backed by a `Map[K int]`. Keys must satisfy the `Hashable` trait (see [Traits](traits.md)). The type parameter `K` tracks the element type at compile time.

### Definition

```casa
struct Set[K] {
    map: Map[K int]
}

impl[K: Hashable] Set[K] { ... }
```

Set uses a struct-level type parameter `K` with a typed `Map[K int]` field. All methods are in an `impl` block with the `K: Hashable` bound.

### `Set::new`

Creates an empty set.

**Signature:** `Set::new -> Set[K]`

**Stack effect:** `-> Set[K]`

```casa
Set::new (Set[str]) = s
```

### `Set::length`

Returns the number of elements in the set.

**Signature:** `Set::length self:Set[K] -> int`

**Stack effect:** `Set[K] -> int`

```casa
s.length print    # 0
```

### `Set::has`

Returns `true` if the element is in the set.

**Signature:** `Set::has self:Set[K] key:K -> bool`

**Stack effect:** `Set[K] K -> bool`

```casa
"apple" s.has print    # true or false
```

### `Set::add`

Adds an element to the set. Returns the updated set. Adding a duplicate has no effect.

**Signature:** `Set::add self:Set[K] key:K -> Set[K]`

**Stack effect:** `Set[K] K -> Set[K]`

```casa
"apple" s.add = s
```

### `Set::remove`

Removes an element from the set. Returns the updated set. If the element does not exist, the set is returned unchanged.

**Signature:** `Set::remove self:Set[K] key:K -> Set[K]`

**Stack effect:** `Set[K] K -> Set[K]`

```casa
"apple" s.remove = s
```

### `Set::to_list`

Returns a `List[K]` of all elements in the set.

**Signature:** `Set::to_list self:Set[K] -> List[K]`

**Stack effect:** `Set[K] -> List[K]`

```casa
s.to_list = elements
```

### Complete Example

```casa
import "path/to/lib/std.casa"

# Create a set of strings
Set::new (Set[str]) = s

# Add elements
"apple" s.add = s
"banana" s.add = s
"cherry" s.add = s

s.length print         # 3
"apple" s.has print    # true
"grape" s.has print    # false

# Remove an element
"banana" s.remove = s
s.length print         # 2
"banana" s.has print   # false
```

See [`examples/hash_map.casa`](../examples/hash_map.casa) for a full program using both Map and Set.

## `StringBuilder`

A mutable string builder backed by `List[char]`. Useful for efficiently constructing strings from many parts.

### Definition

```casa
struct StringBuilder {
    chars: List[char]
}
```

### `StringBuilder::new`

Creates an empty `StringBuilder`.

**Signature:** `StringBuilder::new -> StringBuilder`

**Stack effect:** `-> StringBuilder`

```casa
StringBuilder::new = sb
```

### `StringBuilder::append`

Appends a string to the builder.

**Signature:** `StringBuilder::append self:StringBuilder s:str`

**Stack effect:** `StringBuilder str -> None`

```casa
"hello " sb.append
"world" sb.append
```

### `StringBuilder::append_char`

Appends a single character to the builder.

**Signature:** `StringBuilder::append_char self:StringBuilder c:char`

**Stack effect:** `StringBuilder char -> None`

```casa
'!' sb.append_char
```

### `StringBuilder::build`

Converts the builder's contents into a string.

**Signature:** `StringBuilder::build self:StringBuilder -> str`

**Stack effect:** `StringBuilder -> str`

```casa
sb.build print    # hello world!
```

### `StringBuilder::length`

Returns the number of characters in the builder.

**Signature:** `StringBuilder::length self:StringBuilder -> int`

**Stack effect:** `StringBuilder -> int`

## See Also

- [Standard Library](standard-library.md) -- arrays, Option, Result, and the `import` directive
- [Strings and IO](strings-and-io.md) -- string methods, file I/O, and type conversions
- [Traits](traits.md) -- the `Hashable` trait required by Map and Set keys
