# Collections and Iterators

Import `std` to use the collection methods on this page:

```casa
import "std"
```

Run complete examples from the repository root with
`./casac sample.casa -L lib -r`. Tables abbreviate standard-library names as
`List`, `Option`, and similar local names. Source code uses `std::List`,
`std::Option`, and the other qualified names. See [reference notation](notation.md).

| Need | Type or operation |
|---|---|
| Fixed number of elements | [Array](#arrays) |
| Growable sequence | [List](lists.md) |
| Borrowed list range | [Slice](#slices) |
| Binary data | [Bytes](#bytes) |
| Key-value lookup | [Map](#maps) |
| Unique values | [Set](#sets) |
| Growable UTF-8 text | [String](strings-and-io.md#owned-strings) |
| Process a sequence | [Iterators](#iterator-sources) |

## Arrays

`array[T N]` is a sequence of exactly `N` elements, created with bracket syntax.
The length is part of the type, so `[10, 20, 30]` has type `array[i64 3]` and
arrays of different lengths are different types:

```casa
import "std"

[10, 20, 30] = numbers:array[i64 3]
1 numbers.nth print    # 20
```

| Method | Signature | Behavior |
|---|---|---|
| [array[T N]::length](#arrayt-nlength) | `fn length [T const N:u64] self:$array[T N] -> u64` | Number of elements, which is `N` |
| [array[T N]::is_empty](#arrayt-nis_empty) | `fn is_empty [T const N:u64] self:$array[T N] -> bool` | Whether `N` is zero |
| [array[T N]::nth](#arrayt-nnth) | `fn nth [T const N:u64] self:$array[T N] index:u64 -> $T` | Borrow of the element at a zero-based index |
| [array[T N]::clone](#arrayt-nclone) | `fn clone self:$array[T N] -> array[T N]` | Independent array when `T: Clone` |
| [array[T N]::iter](#arrayt-niter) | `fn iter [T const N:u64] self:$array[T N] -> Iter[$T]` | Iterator over borrows of the elements |
| [array[str N]::contains](#arraystr-ncontains) | `fn contains [const N:u64] self:$array[str N] needle:$str -> bool` | Whether a string array contains `needle` |

### array[T N]::length

Returns the number of elements, which is `N`.

### array[T N]::is_empty

Returns whether `N` is zero.

### array[T N]::nth

Returns a shared borrow of the element at a zero-based index. An out-of-range index
terminates the program. The source keeps ownership of the element.

### array[T N]::clone

Returns an independent array when `T: Clone`.

### array[T N]::iter

Returns an iterator over borrows of the elements.

### array[str N]::contains

Returns whether a string array contains `needle`.


An array value is its element storage: it carries no length word, and `.length`
resolves to the constant in its type. Array length cannot change. Use `List[T]`
when values must be added or removed.

A function that accepts arrays of any length takes a constant length parameter:

```casa
import "std"

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

The following invalid example uses an owning struct:

```casa
struct Resource { id: i64 }

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
import "std"

[10, 20, 30, 40] std::List::from_array = numbers
4 1 numbers.slice = middle
0 middle.nth print    # 20
```

| Method | Signature | Behavior |
|---|---|---|
| [Slice[T]::length](#slicetlength) | `fn length self:$Slice[T] -> u64` | Number of elements in the view |
| [Slice[T]::is_empty](#slicetis_empty) | `fn is_empty self:$Slice[T] -> bool` | Whether the view has no elements |
| [Slice[T]::nth](#slicetnth) | `fn nth self:$Slice[T] index:u64 -> $T` | Borrow of the element at a zero-based index |
| [Slice[T]::iter](#slicetiter) | `fn iter self:$Slice[T] -> Iter[$T]` | Iterator over borrows of the elements |

### Slice[T]::length

Returns the number of elements in the view.

### Slice[T]::is_empty

Returns whether the view has no elements.

### Slice[T]::nth

Returns a shared borrow of the element at a zero-based index. An out-of-range index
terminates the program. The source keeps ownership of the element.

### Slice[T]::iter

Returns an iterator over borrows of the elements.


A slice contains a borrow of its source list. It does not own or destroy the
elements. The list stays loaned until the slice's last use. `List::as_slice`
returns a slice over the complete list.

## Lists

`std::List[T]` owns a growable sequence. Use it when elements must be added or
removed. The [List reference](lists.md) covers list operations, including
[reading an element](lists.md#listtget),
[appending](lists.md#listtpush), and [removal](lists.md#listtpop).


## Bytes

`Bytes` is a non-`Copy` owned growable buffer for binary data. It stores one
`u8` per byte. Mutation requires an exclusive borrow.

| Method | Signature | Behavior |
|---|---|---|
| [Bytes::new](#bytesnew) | `fn new -> Bytes` | Empty byte buffer |
| [Bytes::from_str](#bytesfrom_str) | `fn from_str source:$str -> Bytes` | Copy the text's UTF-8 bytes |
| [Bytes::length](#byteslength) | `fn length self:$Bytes -> u64` | Number of initialized bytes |
| [Bytes::capacity](#bytescapacity) | `fn capacity self:$Bytes -> u64` | Number of bytes available before growth |
| [Bytes::to_raw_buffer](#bytesto_raw_buffer) | `fn to_raw_buffer self:$Bytes -> RawBuffer` | Independent allocation containing exactly `length` initialized bytes |
| [Bytes::push](#bytespush) | `fn push self:mut$Bytes byte:u8` | Add one byte |
| [Bytes::append](#bytesappend) | `fn append self:mut$Bytes source:$Bytes` | Copy the source bytes onto the end |
| [Bytes::get](#bytesget) | `fn get self:$Bytes index:u64 -> Option[u8]` | Copy one byte if the index is in range |
| [Bytes::iter](#bytesiter) | `fn iter self:$Bytes -> Iter[u8]` | Iterator that copies each byte |
| [Bytes::as_cstr](#bytesas_cstr) | `fn as_cstr self:$Bytes -> Option[$cstr]` | Borrow a NUL-terminated view if no byte is NUL |
| [Bytes::clone](#bytesclone) | `fn clone self:$Bytes -> Bytes` | Independent byte buffer |
| [Bytes::to_str](#bytesto_str) | `fn to_str self:$Bytes -> Result[String Utf8Error]` | Validate and copy UTF-8 text |

### Bytes::new

Creates an empty byte buffer.

### Bytes::from_str

Copies the text's UTF-8 bytes.

### Bytes::length

Returns the number of initialized bytes.

### Bytes::capacity

Returns the number of bytes available before growth.

### Bytes::to_raw_buffer

Returns an independent allocation containing exactly `length` initialized bytes.

### Bytes::push

Adds one byte.

### Bytes::append

Copies the source bytes onto the end.

### Bytes::get

Returns a copied byte wrapped in `Option::Some`. An out-of-range index returns
`Option::None`. The byte buffer remains available.

### Bytes::iter

Returns an iterator that copies each byte.

### Bytes::as_cstr

Borrows a NUL-terminated view if no byte is NUL.

### Bytes::clone

Returns an independent byte buffer.

### Bytes::to_str

Validates and copies UTF-8 text.


`to_str` borrows the buffer and keeps it unchanged. Invalid UTF-8 returns
`Utf8Error`. There is no consuming `into_str` conversion. `Bytes` implements
`Eq` and `Hashable`, but not `Display`. Its storage keeps a trailing NUL outside
the logical length so `as_cstr` does not allocate.

See [`examples/bytes.casa`](../examples/bytes.casa) for a runnable example.

## Maps

`Map[K V]` associates unique keys with values. `K` must implement `Hashable`.
A new map allocates its buckets when the first entry is inserted:

```casa
import "std"

std::Map[str i64]::new = scores
10 "Ada" scores.set

"Ada" scores.get match
    std::Option::Some(score) => score print
    std::Option::None => "missing" print
end
```

| Method | Signature | Behavior |
|---|---|---|
| [Map[K V]::new](#mapk-vnew) | `fn new -> Map[K V]` | Empty map |
| [Map[K V]::length](#mapk-vlength) | `fn length self:$Map[K V] -> u64` | Number of entries |
| [Map[K V]::is_empty](#mapk-vis_empty) | `fn is_empty self:$Map[K V] -> bool` | Whether the map has no entries |
| [Map[K V]::get](#mapk-vget) | `fn get self:$Map[K V] key:$K -> Option[$V]` | Borrow of a value, if present |
| [Map[K V]::get_mut](#mapk-vget_mut) | `fn get_mut self:mut$Map[K V] key:$K -> Option[mut$V]` | Exclusive borrow of a value, if present |
| [Map[K V]::get_copy](#mapk-vget_copy) | `fn get_copy self:$Map[K V] key:$K -> Option[V]` | Owned value when `V: Copy` |
| [Map[K V]::get_cloned](#mapk-vget_cloned) | `fn get_cloned self:$Map[K V] key:$K -> Option[V]` | Owned value when `V: Clone` |
| [Map[K V]::has](#mapk-vhas) | `fn has self:$Map[K V] key:$K -> bool` | Whether a key exists |
| [Map[K V]::set](#mapk-vset) | `fn set self:mut$Map[K V] key:K value:V` | Insert or replace an entry |
| [Map[K V]::delete](#mapk-vdelete) | `fn delete self:mut$Map[K V] key:$K` | Remove and destroy a value, if present |
| [Map[K V]::remove](#mapk-vremove) | `fn remove self:mut$Map[K V] key:$K -> Option[V]` | Remove and return a value, if present |
| [Map[K V]::iter](#mapk-viter) | `fn iter self:$Map[K V] -> Iter[Pair[$K $V]]` | Iterator over borrowed key-value pairs |
| [Map[K V]::keys](#mapk-vkeys) | `fn keys self:$Map[K V] -> List[K]` | Cloned keys when `K: Clone` |
| [Map[K V]::values](#mapk-vvalues) | `fn values self:$Map[K V] -> List[V]` | Cloned values when `V: Clone` |
| [Map[K V]::clone](#mapk-vclone) | `fn clone self:$Map[K V] -> Map[K V]` | Independent map when `K: Clone` and `V: Clone` |

### Map[K V]::new

Creates an empty map.

### Map[K V]::length

Returns the number of entries.

### Map[K V]::is_empty

Returns whether the map has no entries.

### Map[K V]::get

Returns a borrow of a value, if present.

### Map[K V]::get_mut

Returns an exclusive borrow of a value, if present.

### Map[K V]::get_copy

Returns a copied value, if present. `V` must implement `Copy`.

### Map[K V]::get_cloned

Returns a cloned value, if present. `V` must implement `Clone`.

### Map[K V]::has

Returns whether a key exists.

### Map[K V]::set

Inserts or replaces an entry.

### Map[K V]::delete

Removes and destroys a value, if present.

### Map[K V]::remove

Removes and returns a value, if present.

### Map[K V]::iter

Returns an iterator over borrowed key-value pairs.

### Map[K V]::keys

Returns a list of cloned keys when `K` implements `Clone`.

### Map[K V]::values

Returns a list of cloned values when `V` implements `Clone`.

### Map[K V]::clone

Returns an independent map when `K: Clone` and `V: Clone`.


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

| Method | Signature | Behavior |
|---|---|---|
| [Map[String V]::set_str](#mapstring-vset_str) | `fn set_str self:mut$Map[String V] key:$str value:V` | Copy and insert a text key |
| [Map[String V]::get_str](#mapstring-vget_str) | `fn get_str self:$Map[String V] key:$str -> Option[$V]` | Borrow a value |
| [Map[String V]::get_mut_str](#mapstring-vget_mut_str) | `fn get_mut_str self:mut$Map[String V] key:$str -> Option[mut$V]` | Exclusively borrow a value |
| [Map[String V]::has_str](#mapstring-vhas_str) | `fn has_str self:$Map[String V] key:$str -> bool` | Whether the text key exists |
| [Map[String V]::delete_str](#mapstring-vdelete_str) | `fn delete_str self:mut$Map[String V] key:$str` | Remove and destroy a value |
| [Map[String V]::remove_str](#mapstring-vremove_str) | `fn remove_str self:mut$Map[String V] key:$str -> Option[V]` | Remove a value |

### Map[String V]::set_str

Copies and inserts a text key.

### Map[String V]::get_str

Borrows a value.

### Map[String V]::get_mut_str

Exclusively borrows a value.

### Map[String V]::has_str

Returns whether the text key exists.

### Map[String V]::delete_str

Removes and destroys a value.

### Map[String V]::remove_str

Removes a value.


See [`examples/hash_map.casa`](../examples/hash_map.casa) for a runnable map
example.

## Sets

`Set[K]` stores unique `Hashable` values:

```casa
import "std"

std::Set[str]::new = names
"Ada" names.add
"Grace" names.add
"Ada" names.has print    # true
```

| Method | Signature | Behavior |
|---|---|---|
| [Set[K]::new](#setknew) | `fn new -> Set[K]` | Empty set |
| [Set[K]::length](#setklength) | `fn length self:$Set[K] -> u64` | Number of values |
| [Set[K]::is_empty](#setkis_empty) | `fn is_empty self:$Set[K] -> bool` | Whether the set has no values |
| [Set[K]::has](#setkhas) | `fn has self:$Set[K] key:$K -> bool` | Whether a value exists |
| [Set[K]::add](#setkadd) | `fn add self:mut$Set[K] key:K` | Add a value |
| [Set[K]::remove](#setkremove) | `fn remove self:mut$Set[K] key:$K` | Remove a value if present |
| [Set[K]::iter](#setkiter) | `fn iter self:$Set[K] -> Iter[$K]` | Iterator over borrowed values |
| [Set[K]::to_list](#setkto_list) | `fn to_list self:$Set[K] -> List[K]` | Cloned values in unspecified order when `K: Clone` |
| [Set[K]::clone](#setkclone) | `fn clone self:$Set[K] -> Set[K]` | Independent set when `K: Clone` |

### Set[K]::new

Creates an empty set.

### Set[K]::length

Returns the number of values.

### Set[K]::is_empty

Returns whether the set has no values.

### Set[K]::has

Returns whether a value exists.

### Set[K]::add

Adds a value.

### Set[K]::remove

Removes a value if present.

### Set[K]::iter

Returns an iterator over borrowed values.

### Set[K]::to_list

Returns a list of cloned values in unspecified order when `K` implements `Clone`.

### Set[K]::clone

Returns an independent set when `K: Clone`.


`iter` keeps the set as owner. `to_list` and `clone` require `K: Clone`.
Collisions, traversal order, and untrusted-key behavior match `Map`.

`Set[String]` accepts borrowed text keys:

| Method | Signature | Behavior |
|---|---|---|
| [Set[String]::add_str](#setstringadd_str) | `fn add_str self:mut$Set[String] key:$str` | Copy and add a text key |
| [Set[String]::has_str](#setstringhas_str) | `fn has_str self:$Set[String] key:$str -> bool` | Whether the text key exists |
| [Set[String]::remove_str](#setstringremove_str) | `fn remove_str self:mut$Set[String] key:$str` | Remove a text key if present |

### Set[String]::add_str

Copies the borrowed text key into an owned String and adds it to the set.

### Set[String]::has_str

Returns whether the borrowed text key exists. Lookup does not allocate.

### Set[String]::remove_str

Removes and destroys the matching key, if present. Removal does not allocate.

## Owned strings

`std::String` owns growable UTF-8 text. Use it to assemble text in steps.
The [text reference](strings-and-io.md#owned-strings) owns its method contracts
and examples. Use [Bytes](#bytes) for binary data.

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

| Method | Signature | Behavior |
|---|---|---|
| [Iterable[T]::map](#iterabletmap) | `fn map [U] self:self f:fn[T -> U] -> Iter[U]` | Transform each value |
| [Iterable[T]::filter](#iterabletfilter) | `fn filter self:self f:fn[$T -> bool] -> Iter[T]` | Keep matching values |
| [Iterable[T]::take](#iterablettake) | `fn take self:self n:u64 -> Iter[T]` | Yield at most `n` values |
| [Iterable[T]::skip](#iterabletskip) | `fn skip self:self n:u64 -> Iter[T]` | Omit the first `n` values |
| [Iterable[T]::take_while](#iterablettake_while) | `fn take_while self:self f:fn[$T -> bool] -> Iter[T]` | Yield while the predicate is true |
| [Iterable[T]::skip_while](#iterabletskip_while) | `fn skip_while self:self f:fn[$T -> bool] -> Iter[T]` | Omit values while the predicate is true |
| [Iterable[T]::enumerate](#iterabletenumerate) | `fn enumerate self:self -> Iter[Pair[i64 T]]` | Pair each value with its zero-based index |
| [Iterable[T]::zip](#iterabletzip) | `fn zip [U] self:self other:Iter[U] -> Iter[Pair[T U]]` | Pair values until either iterator ends |
| [Iterable[T]::chain](#iterabletchain) | `fn chain self:self other:Iter[T] -> Iter[T]` | Yield from `self`, then `other` |
| [Iterable[T]::flat_map](#iterabletflat_map) | `fn flat_map [U] self:self f:fn[T -> Iter[U]] -> Iter[U]` | Transform and flatten one level |

### Iterable[T]::map

Transforms each value.

### Iterable[T]::filter

Keeps matching values.

### Iterable[T]::take

Yields at most `n` values.

### Iterable[T]::skip

Omits the first `n` values.

### Iterable[T]::take_while

Yields while the predicate is true.

### Iterable[T]::skip_while

Omits values while the predicate is true.

### Iterable[T]::enumerate

Pairs each value with its zero-based index.

### Iterable[T]::zip

Pairs values until either iterator ends.

### Iterable[T]::chain

Yields from `self`, then `other`.

### Iterable[T]::flat_map

Transforms and flattens one level.


In trait signatures, the type `self` means the type that implements `Iterable[T]`.

## Terminal iterator operations

Terminal operations borrow the iterator exclusively, advance its state, and
return a non-iterator value. A named iterator remains available after the call.
Operations such as `any` and `find` can stop before it is exhausted.

| Method | Signature | Behavior |
|---|---|---|
| [Iterable[T]::next](#iterabletnext) | `fn next self:mut$self -> Option[T]` | Next value, if present |
| [Iterable[T]::collect](#iterabletcollect) | `fn collect self:mut$self -> List[T]` | All remaining values |
| [Iterable[T]::fold](#iterabletfold) | `fn fold [U] self:mut$self acc:U f:fn[U T -> U] -> U` | Reduce from an initial value |
| [Iterable[T]::count](#iterabletcount) | `fn count self:mut$self -> u64` | Number of remaining values |
| [Iterable[T]::any](#iterabletany) | `fn any self:mut$self f:fn[$T -> bool] -> bool` | Whether any value matches |
| [Iterable[T]::all](#iterabletall) | `fn all self:mut$self f:fn[$T -> bool] -> bool` | Whether every value matches |
| [Iterable[T]::find](#iterabletfind) | `fn find self:mut$self f:fn[$T -> bool] -> Option[T]` | First matching value |
| [Iterable[T]::reduce](#iterabletreduce) | `fn reduce self:mut$self f:fn[T T -> T] -> Option[T]` | Reduce from the first value |
| [Iterable[T]::min_by](#iterabletmin_by) | `fn min_by self:mut$self f:fn[$T $T -> bool] -> Option[T]` | Minimum selected by a callback |
| [Iterable[T]::max_by](#iterabletmax_by) | `fn max_by self:mut$self f:fn[$T $T -> bool] -> Option[T]` | Maximum selected by a callback |
| [Iterable[T]::partition](#iterabletpartition) | `fn partition self:mut$self f:fn[$T -> bool] -> Pair[List[T] List[T]]` | Matching and non-matching lists |
| [Iter[i64]::sum](#iteri64sum) | `fn sum self:mut$Iter[i64] -> i64` | Add owned `i64` values |
| [Iter[T]::min](#itertmin) | `fn min self:mut$Iter[T] -> Option[T]` | Minimum value when `T` implements `Ord` |
| [Iter[T]::max](#itertmax) | `fn max self:mut$Iter[T] -> Option[T]` | Maximum value when `T` implements `Ord` |

### Iterable[T]::next

Advances the iterator and returns the next value, if present.

### Iterable[T]::collect

Collects all remaining values into a list.

### Iterable[T]::fold

Reduces from an initial value.

### Iterable[T]::count

Returns the number of remaining values.

### Iterable[T]::any

Returns whether any value matches.

### Iterable[T]::all

Returns whether every value matches.

### Iterable[T]::find

Returns the first matching value, if present.

### Iterable[T]::reduce

Reduces from the first value.

### Iterable[T]::min_by

Returns the minimum value selected by the comparison callback, if present.

### Iterable[T]::max_by

Returns the maximum value selected by the comparison callback, if present.

### Iterable[T]::partition

Separates the remaining values into matching and non-matching lists.

### Iter[i64]::sum

Adds owned `i64` values.

### Iter[T]::min

Returns the minimum value, if present. `T` must implement `Ord`.

### Iter[T]::max

Returns the maximum value, if present. `T` must implement `Ord`.


This pipeline skips two values, takes four, keeps even values, and doubles
them. Only `collect` runs the pipeline:

```casa
import "std"

[1, 2, 3, 4, 5, 6, 7] = values:array[i64 7]
2 values.iter.skip = rest
4 rest.take = window
{ copy 2 % 0 == } window.filter = even
{ copy 2 * } even.map.collect = doubled
```

See [`examples/iterator_combinators.casa`](../examples/iterator_combinators.casa)
for every lazy and terminal operation.
