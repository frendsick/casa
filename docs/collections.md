# Collections and Iterators

Import `std` to use the collection methods on this page:

```casa
import "std" { Bytes Iter List Map Option Pair Set Slice String }
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
| `is_empty self:$array[T N] -> bool` | Whether `N` is zero |
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
    0 = sum:i64
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
independent array when `T: Clone`. An array is `Copy` when `T: Copy`, including
when `N` is zero. Arrays with non-`Copy` elements remain affine. Indexing with a
constant past the last element is a compile-time error. Indexing past it with a
runtime value terminates the program.

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
| `is_empty self:$Slice[T] -> bool` | Whether the view has no elements |
| `nth self:$Slice[T] index:u64 -> $T` | Borrow of the element at a zero-based index |
| `iter self:$Slice[T] -> Iter[$T]` | Iterator over borrows of the elements |

A slice contains a borrow of its source list. It does not own or destroy the
elements. The list stays loaned until the slice's last use. `List::as_slice`
returns a slice over the complete list.

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
| `length self:$List[T] -> u64` | Number of elements |
| `is_empty self:$List[T] -> bool` | Whether the list has no elements |
| `get self:$List[T] index:u64 -> $T` | Borrow of the element at a zero-based index |
| `get_ref self:$List[T] index:u64 -> $T` | Borrow of the element at a zero-based index |
| `get_mut self:mut$List[T] index:u64 -> mut$T` | Exclusive borrow of an element |
| `slice self:$List[T] start:u64 stop:u64 -> Slice[T]` | Borrowed half-open range `[start, stop)` |
| `as_slice self:$List[T] -> Slice[T]` | Borrowed view of the complete list |
| `set self:mut$List[T] index:u64 value:T` | Replace and destroy an element |
| `replace self:mut$List[T] index:u64 value:T -> T` | Replace and return an element |
| `push self:mut$List[T] value:T` | Add at the end |
| `pop self:mut$List[T] -> T` | Remove and return the last element |
| `insert self:mut$List[T] value:T index:u64` | Insert before `index` |
| `remove self:mut$List[T] index:u64 -> Option[T]` | Remove and return an element if present |
| `swap_at self:mut$List[T] first:u64 second:u64` | Exchange two elements |
| `reverse self:mut$List[T]` | Reverse in place |
| `append self:mut$List[T] other:List[T]` | Move every element of `other` onto the end |
| `clone self:$List[T] -> List[T]` | Independent list when `T: Clone` |
| `iter self:$List[T] -> Iter[$T]` | Iterator over borrows of the elements |
| `sort self:mut$List[T]` | Sort in place when `T` implements `Ord` |
| `sort_by self:mut$List[T] compare:fn[$T $T -> bool]` | Sort in place with a callback |
| `sort_by_range self:mut$List[T] low:u64 high:u64 compare:fn[$T $T -> bool]` | Sort an inclusive index range |
| `join self:$List[str] separator:$str -> String` | Join a string-view list |
| `contains self:$List[str] needle:$str -> bool` | Whether a string list contains `needle` |
| `join_strings self:$List[String] separator:$str -> String` | Join owned strings |
| `push_str self:mut$List[String] value:$str` | Copy and append one text view |

`get`, `get_ref`, and `iter` borrow elements in place, so the list stays their
owner. Use `get_mut` for exclusive access. Use `.clone` when an owned value is
needed. `set` destroys the replaced value. `replace`, `remove`, and `pop` move
values out instead.

Out-of-range indexing, slicing, insertion, and popping an empty list terminate
the program.

See [`examples/sorting.casa`](../examples/sorting.casa) for sorting and
reversing.

## Bytes

`Bytes` is a non-`Copy` owned growable buffer for binary data. It stores one
`u8` per byte. Mutation requires an exclusive borrow.

| Method | Result or action |
|---|---|
| `Bytes::new -> Bytes` | Empty byte buffer |
| `length self:$Bytes -> u64` | Number of initialized bytes |
| `capacity self:$Bytes -> u64` | Number of bytes available before growth |
| `push self:mut$Bytes byte:u8` | Add one byte |
| `append self:mut$Bytes source:$Bytes` | Copy the source bytes onto the end |
| `get self:$Bytes index:u64 -> Option[u8]` | Copy one byte if the index is in range |
| `iter self:$Bytes -> Iter[u8]` | Iterator that copies each byte |
| `clone self:$Bytes -> Bytes` | Independent byte buffer |
| `to_str self:$Bytes -> Result[String Utf8Error]` | Validate and copy UTF-8 text |

`to_str` borrows the buffer and keeps it unchanged. Invalid UTF-8 returns
`Utf8Error`. There is no consuming `into_str` conversion.

See [`examples/bytes.casa`](../examples/bytes.casa) for a runnable example.

## Maps

`Map[K V]` associates unique keys with values. `K` must implement `Hashable`:

```casa
Map[str i64]::new = scores
10 "Ada" scores.set

"Ada" scores.get match
    Option::Some(score) => score print
    Option::None => "missing" print
end
```

| Method | Result or action |
|---|---|
| `Map[K V]::new -> Map[K V]` | Empty map |
| `length self:$Map[K V] -> u64` | Number of entries |
| `is_empty self:$Map[K V] -> bool` | Whether the map has no entries |
| `get self:$Map[K V] key:$K -> Option[$V]` | Borrow of a value, if present |
| `get_mut self:mut$Map[K V] key:$K -> Option[mut$V]` | Exclusive borrow of a value, if present |
| `get_copy self:$Map[K V] key:$K -> Option[V]` | Owned value when `V: Copy` |
| `get_cloned self:$Map[K V] key:$K -> Option[V]` | Owned value when `V: Clone` |
| `has self:$Map[K V] key:$K -> bool` | Whether a key exists |
| `set self:mut$Map[K V] key:K value:V` | Insert or replace an entry |
| `delete self:mut$Map[K V] key:$K` | Remove and destroy a value, if present |
| `remove self:mut$Map[K V] key:$K -> Option[V]` | Remove and return a value, if present |
| `iter self:$Map[K V] -> Iter[Pair[$K $V]]` | Iterator over borrowed key-value pairs |
| `keys self:$Map[K V] -> List[K]` | Cloned keys when `K: Clone` |
| `values self:$Map[K V] -> List[V]` | Cloned values when `V: Clone` |
| `clone self:$Map[K V] -> Map[K V]` | Independent map when `K: Clone` and `V: Clone` |

`get` and `iter` keep the map as owner. `set` destroys a replaced value.
`delete` destroys a removed value. `remove` moves it out.

Hash collisions do not merge unequal keys. `Map` compares same-hash keys with
`Eq`, so lookup, replacement, and removal remain correct. Heavy collisions can
make an operation linear in the number of entries.

Iteration order is not specified. It can change after insertion, removal, or
resizing, and between processes, builds, releases, and targets. `keys` and
`values` also have unspecified order.

Standard hashes are unkeyed. `Map` does not defend against adversarial
collisions. Bound or validate untrusted key sets, or use a specialized
collection.

`Map[String V]` also accepts borrowed text keys without allocating a temporary
owner:

| Method | Result or action |
|---|---|
| `set_str self:mut$Map[String V] key:$str value:V` | Copy and insert a text key |
| `get_str self:$Map[String V] key:$str -> Option[$V]` | Borrow a value |
| `get_mut_str self:mut$Map[String V] key:$str -> Option[mut$V]` | Exclusively borrow a value |
| `has_str self:$Map[String V] key:$str -> bool` | Whether the text key exists |
| `delete_str self:mut$Map[String V] key:$str` | Remove and destroy a value |
| `remove_str self:mut$Map[String V] key:$str -> Option[V]` | Remove a value |

See [`examples/hash_map.casa`](../examples/hash_map.casa) for a runnable map
example.

## Sets

`Set[K]` stores unique `Hashable` values:

```casa
Set[str]::new = names
"Ada" names.add
"Grace" names.add
"Ada" names.has print    # true
```

| Method | Result or action |
|---|---|
| `Set[K]::new -> Set[K]` | Empty set |
| `length self:$Set[K] -> u64` | Number of values |
| `is_empty self:$Set[K] -> bool` | Whether the set has no values |
| `has self:$Set[K] key:$K -> bool` | Whether a value exists |
| `add self:mut$Set[K] key:K` | Add a value |
| `remove self:mut$Set[K] key:$K` | Remove a value if present |
| `iter self:$Set[K] -> Iter[$K]` | Iterator over borrowed values |
| `to_list self:$Set[K] -> List[K]` | Cloned values in unspecified order when `K: Clone` |
| `clone self:$Set[K] -> Set[K]` | Independent set when `K: Clone` |

`iter` keeps the set as owner. `to_list` and `clone` require `K: Clone`.
Collisions, traversal order, and untrusted-key behavior match `Map`.

`Set[String]` provides `add_str`, `has_str`, and `remove_str` for borrowed
`$str` values. `add_str` copies the key. Lookup and removal do not allocate.

## Owned strings

`String` owns growable UTF-8 text. It avoids repeated concatenation when text
is assembled in steps:

```casa
String::new = text
"Hello" text.append
", " text.append
"Casa" text.append
text.as_str print
```

| Method | Result or action |
|---|---|
| `String::new -> String` | Empty owned text |
| `append self:mut$String text:$str` | Append borrowed text |
| `append_string self:mut$String text:String` | Append and consume owned text |
| `push self:mut$String character:char` | Append one Unicode scalar value |
| `as_str self:$String -> $str` | Borrow the current text without allocation |
| `length self:$String -> u64` | Current byte length |

## Iterator sources

`.iter` creates a stateful, single-pass iterator:

| Source | Iterator |
|---|---|
| `array[T N]` | `Iter[$T]` |
| `List[T]` | `Iter[$T]` |
| `Slice[T]` | `Iter[$T]` |
| `Bytes` | `Iter[u8]` |
| `str` | `Iter[char]` |
| `Map[K V]` | `Iter[Pair[$K $V]]` |
| `Set[K]` | `Iter[$K]` |

A `for` loop consumes the iterator. Create another iterator to traverse the
source again.

Arrays, lists, slices, maps, and sets yield borrows because the source keeps
owning its elements. Clone a yielded value when an owned value is needed.

## Lazy iterator operations

Lazy operations return `Iter` and do no work until the result is consumed.

| Method | Result |
|---|---|
| `map self:I transform:fn[T -> U] -> Iter[U]` | Transform each value |
| `filter self:I predicate:fn[$T -> bool] -> Iter[T]` | Keep matching values |
| `take self:I count:u64 -> Iter[T]` | Yield at most `count` values |
| `skip self:I count:u64 -> Iter[T]` | Omit the first `count` values |
| `take_while self:I predicate:fn[$T -> bool] -> Iter[T]` | Yield while the predicate is true |
| `skip_while self:I predicate:fn[$T -> bool] -> Iter[T]` | Omit values while the predicate is true |
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
| `any self:I predicate:fn[$T -> bool] -> bool` | Whether any value matches |
| `all self:I predicate:fn[$T -> bool] -> bool` | Whether every value matches |
| `find self:I predicate:fn[$T -> bool] -> Option[T]` | First matching value |
| `reduce self:I combine:fn[T T -> T] -> Option[T]` | Reduce from the first value |
| `min_by self:I compare:fn[$T $T -> bool] -> Option[T]` | Minimum selected by a callback |
| `max_by self:I compare:fn[$T $T -> bool] -> Option[T]` | Maximum selected by a callback |
| `partition self:I predicate:fn[$T -> bool] -> Pair[List[T] List[T]]` | Matching and non-matching lists |
| `sum self:Iter[i64] -> i64` | Add owned `i64` values |
| `min self:Iter[T] -> Option[T]` | Minimum value when `T` implements `Ord` |
| `max self:Iter[T] -> Option[T]` | Maximum value when `T` implements `Ord` |

This pipeline skips two values, takes four, keeps even values, and doubles
them. Only `collect` runs the pipeline:

```casa
[1, 2, 3, 4, 5, 6, 7] = values:array[i64 7]
2 values.iter.skip = rest
4 rest.take = window
{ copy 2 % 0 == } window.filter = even
{ copy 2 * } even.map.collect = doubled
```

See [`examples/iterator_combinators.casa`](../examples/iterator_combinators.casa)
for every lazy and terminal operation.
