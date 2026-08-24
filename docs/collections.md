# Collections and Iterators

Import `std` to use the collection methods on this page:

```casa
import "std"
```

## Arrays

`array[T N]` is a sequence of exactly `N` elements, created with bracket syntax.
The length is part of the type, so `[10, 20, 30]` has type `array[i64 3]` and
arrays of different lengths are different types:

```casa
[10, 20, 30] = numbers:array[i64 3]
1 numbers.nth print    # 20
```

| Method | Result or action |
|---|---|
| `length self:$array[T N] -> u64` | Number of elements, which is `N` |
| `nth self:$array[T N] index:u64 -> $T` | Borrow of the element at a zero-based index |
| `clone self:$array[T N] -> array[T N]` | Independent array when `T: Clone` |
| `iter self:$array[T N] -> Iter[$T]` | Iterator over borrows of the elements |
| `contains self:$array[str N] needle:$str -> bool` | Whether a string array contains `needle` |

An array value is its element storage: it carries no length word, and `.length`
resolves to the constant in its type. Array length cannot change. Use `List[T]`
when values must be added or removed.

A function that accepts arrays of any length takes a constant length parameter:

```casa
fn total [const N:u64] values:$array[i64 N] -> i64 {
    0 (i64) = sum
    for value in values.iter do
        value += sum
    done
    sum
}
```

Each evaluation of an array literal produces an independent owned array. The
literal takes ownership of its elements, so an element binding cannot be used
again afterwards:

```casa
Resource { id: 1 } = resource
[resource] = owned:array[Resource 1]
resource drop    # error: owner `resource` was already moved
```

The array destroys its elements when it goes out of scope. `clone` produces an
independent array when `T: Clone`, and `array[T N]` is never `Copy`, even when
`T` is. Copying an owning value would create two owners of the same storage.
Indexing with a constant past the last element is a compile-time error. Indexing
past it with a runtime value terminates the program.

`nth` and `iter` read through a borrowed array, so they hand back `$T` rather
than an owned element. The array stays the only owner: an element type with a
reserved `drop` method runs its hook once, when the array is destroyed. Use
`.clone` on the result when an owned value is needed.

## Slices

`Slice[T]` is a borrowed runtime-length range over a `List[T]`:

```casa
[10, 20, 30, 40] List::from_array = numbers
4 1 numbers.slice = middle
0 middle.nth print    # 20
```

| Method | Result or action |
|---|---|
| `length self:$Slice[T] -> u64` | Number of elements in the view |
| `nth self:$Slice[T] index:u64 -> $T` | Borrow of the element at a zero-based index |
| `iter self:$Slice[T] -> Iter[$T]` | Iterator over borrows of the elements |

A slice contains a borrow of its source list. It does not own or destroy the
elements. The list stays loaned until the slice's last use. `List::to_array`
returns a slice over the complete list for compatibility with existing code.

## Lists

`List[T]` is a growable sequence:

```casa
[3, 1, 2] List::from_array = numbers
numbers.sort
0 numbers.get print    # 1
```

| Method | Result or action |
|---|---|
| `List[T]::new -> List[T]` | Empty list |
| `from_array values:array[T N] -> List[T]` | List containing the array values |
| `length self:List[T] -> u64` | Number of elements |
| `get self:$List[T] index:u64 -> T` | Element at a zero-based index |
| `get_ref self:$List[T] index:u64 -> $T` | Borrow of the element at a zero-based index |
| `slice self:$List[T] start:u64 stop:u64 -> Slice[T]` | Borrowed half-open range `[start, stop)` |
| `to_array self:$List[T] -> Slice[T]` | Borrowed view of the complete list |
| `set self:List[T] index:u64 value:T` | Replace an element |
| `push self:List[T] value:T` | Add at the end |
| `pop self:List[T] -> T` | Remove and return the last element |
| `insert self:List[T] value:T index:u64` | Insert before `index` |
| `swap_at self:List[T] first:u64 second:u64` | Exchange two elements |
| `reverse self:List[T]` | Reverse in place |
| `append self:mut$List[T] other:List[T]` | Move every element of `other` onto the end |
| `clone self:List[T] -> List[T]` | Independent list when `T: Clone` |
| `iter self:$List[T] -> Iter[$T]` | Iterator over borrows of the elements |
| `sort self:List[T]` | Sort in place when `T` implements `Ord` |
| `sort_by self:List[T] compare:fn[T T -> bool]` | Sort in place with a callback |
| `sort_by_range self:List[T] low:u64 high:u64 compare:fn[T T -> bool]` | Sort an inclusive index range |
| `join self:List[str] separator:str -> str` | Join a string list |
| `contains self:List[str] needle:str -> bool` | Whether a string list contains `needle` |

`get_ref` and `iter` borrow the element in place, so the list stays its owner.
`get` still returns an owned handle to storage the list also references; prefer
`get_ref` for reads and use `.clone` when an owned value is needed.

Out-of-range indexing, slicing, insertion, and popping an empty list terminate
the program.

See [`examples/sorting.casa`](../examples/sorting.casa) for sorting and
reversing.

## Maps

`Map[K V]` associates unique keys with values. `K` must implement `Hashable`:

```casa
Map[str i64]::new = scores
10 "Ada" scores.set = scores

"Ada" scores.get match
    Option::Some(score) => score print
    Option::None => "missing" print
end
```

| Method | Result or action |
|---|---|
| `Map[K V]::new -> Map[K V]` | Empty map |
| `length self:Map[K V] -> u64` | Number of entries |
| `get self:Map[K V] key:K -> Option[V]` | Value for a key, if present |
| `has self:Map[K V] key:K -> bool` | Whether a key exists |
| `set self:Map[K V] key:K value:V -> Map[K V]` | Insert or replace an entry |
| `delete self:Map[K V] key:K -> Map[K V]` | Remove an entry if present |
| `iter self:Map[K V] -> Iter[Pair[K V]]` | Iterator over key-value pairs |
| `keys self:Map[K V] -> List[K]` | List of keys |
| `values self:Map[K V] -> List[V]` | List of values |
| `clone self:Map[K V] -> Map[K V]` | Independent map when `K: Clone` and `V: Clone` |

Rebind the result of `set` and `delete`, as shown above. Iteration order is not
specified.

See [`examples/hash_map.casa`](../examples/hash_map.casa) for a runnable map
example.

## Sets

`Set[K]` stores unique `Hashable` values:

```casa
Set[str]::new = names
"Ada" names.add = names
"Grace" names.add = names
"Ada" names.has print    # true
```

| Method | Result or action |
|---|---|
| `Set[K]::new -> Set[K]` | Empty set |
| `length self:Set[K] -> u64` | Number of values |
| `has self:Set[K] key:K -> bool` | Whether a value exists |
| `add self:Set[K] key:K -> Set[K]` | Add a value |
| `remove self:Set[K] key:K -> Set[K]` | Remove a value if present |
| `iter self:Set[K] -> Iter[K]` | Iterator over the values |
| `to_list self:Set[K] -> List[K]` | Values in unspecified order |
| `clone self:Set[K] -> Set[K]` | Independent set when `K: Clone` |

Rebind the result of `add` and `remove`.

## String builders

`StringBuilder` avoids repeated string concatenation when text is assembled in
steps:

```casa
StringBuilder::new = builder
"Hello" builder.append
", " builder.append
"Casa" builder.append
builder.build print
```

| Method | Result or action |
|---|---|
| `StringBuilder::new -> StringBuilder` | Empty builder |
| `append self:StringBuilder text:str` | Add a string |
| `append_char self:StringBuilder character:char` | Add one character |
| `build self:StringBuilder -> str` | Build the current text |
| `length self:StringBuilder -> u64` | Current character count |

## Iterator sources

`.iter` creates a stateful, single-pass iterator:

| Source | Iterator |
|---|---|
| `array[T N]` | `Iter[$T]` |
| `List[T]` | `Iter[$T]` |
| `Slice[T]` | `Iter[$T]` |
| `str` | `Iter[char]` |
| `Map[K V]` | `Iter[Pair[K V]]` |
| `Set[K]` | `Iter[K]` |

A `for` loop consumes the iterator. Create another iterator to traverse the
source again.

`array[T N]`, `List[T]`, and `Slice[T]` yield borrows of their elements, because
the source keeps owning them. A loop variable bound from one of these iterators
is a `$T`; `.clone` it when an owned value is needed.

## Lazy iterator operations

Lazy operations return `Iter` and do no work until the result is consumed.

| Method | Result |
|---|---|
| `map self:I transform:fn[T -> U] -> Iter[U]` | Transform each value |
| `filter self:I predicate:fn[T -> bool] -> Iter[T]` | Keep matching values |
| `take self:I count:u64 -> Iter[T]` | Yield at most `count` values |
| `skip self:I count:u64 -> Iter[T]` | Omit the first `count` values |
| `take_while self:I predicate:fn[T -> bool] -> Iter[T]` | Yield while the predicate is true |
| `skip_while self:I predicate:fn[T -> bool] -> Iter[T]` | Omit values while the predicate is true |
| `enumerate self:I -> Iter[Pair[i64 T]]` | Pair each value with its zero-based index |
| `zip self:I other:Iter[U] -> Iter[Pair[T U]]` | Pair values until either iterator ends |
| `chain self:I other:Iter[T] -> Iter[T]` | Yield from `self`, then `other` |
| `flat_map self:I transform:fn[T -> Iter[U]] -> Iter[U]` | Transform and flatten one level |

`I` is any type that implements `Iterable[T]`.

## Terminal iterator operations

Terminal operations advance or consume the iterator and return a non-iterator
value.

| Method | Result |
|---|---|
| `next self:I -> Option[T]` | Next value, if present |
| `collect self:I -> List[T]` | All remaining values |
| `fold self:I initial:U combine:fn[U T -> U] -> U` | Reduce from an initial value |
| `count self:I -> u64` | Number of remaining values |
| `any self:I predicate:fn[T -> bool] -> bool` | Whether any value matches |
| `all self:I predicate:fn[T -> bool] -> bool` | Whether every value matches |
| `find self:I predicate:fn[T -> bool] -> Option[T]` | First matching value |
| `reduce self:I combine:fn[T T -> T] -> Option[T]` | Reduce from the first value |
| `min_by self:I compare:fn[T T -> bool] -> Option[T]` | Minimum selected by a callback |
| `max_by self:I compare:fn[T T -> bool] -> Option[T]` | Maximum selected by a callback |
| `partition self:I predicate:fn[T -> bool] -> Pair[List[T] List[T]]` | Matching and non-matching lists |
| `sum self:I -> i64` | Interpret values as `i64` and add them |
| `min self:Iter[T] -> Option[T]` | Minimum value when `T` implements `Ord` |
| `max self:Iter[T] -> Option[T]` | Maximum value when `T` implements `Ord` |

This pipeline skips two values, takes four, keeps even values, and doubles
them. Only `collect` runs the pipeline:

```casa
2 [1, 2, 3, 4, 5, 6, 7] (array[i64 7]).iter.skip = rest
4 rest.take = window
{ 2 % 0 == } window.filter = even
{ 2 * } even.map.collect = doubled
```

See [`examples/iterator_combinators.casa`](../examples/iterator_combinators.casa)
for every lazy and terminal operation.
