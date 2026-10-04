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
| Binary data | [Bytes](#bytes) |
| Borrowed list range | [Slice](#slices) |
| Fixed number of elements | [Array](#arrays) |
| Growable sequence | [List](lists.md) |
| Growable UTF-8 text | [String](strings-and-io.md#owned-strings) |
| Key-value lookup | [Map](#maps) |
| Process a sequence | [Iterators](#iterator-sources) |
| Unique values | [Set](#sets) |

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
| [clone](#arrayt-nclone) | `fn clone self:$array[T N] -> array[T N]` | Independent array when `T: Clone` |
| [contains](#arraystr-ncontains) | `fn contains [const N:u64] self:$array[str N] needle:$str -> bool` | Whether a string array contains `needle` |
| [is_empty](#arrayt-nis_empty) | `fn is_empty [T const N:u64] self:$array[T N] -> bool` | Whether `N` is zero |
| [iter](#arrayt-niter) | `fn iter [T const N:u64] self:$array[T N] -> Iter[$T]` | Iterator over borrows of the elements |
| [length](#arrayt-nlength) | `fn length [T const N:u64] self:$array[T N] -> u64` | Number of elements, which is `N` |
| [nth](#arrayt-nnth) | `fn nth [T const N:u64] self:$array[T N] index:u64 -> $T` | Borrow of the element at a zero-based index |

<a id="arrayt-nclone"></a>

### clone

```text
fn clone self:$array[T N] -> array[T N]
```

Returns an independent array when `T: Clone`.

<a id="arraystr-ncontains"></a>

### contains

```text
fn contains [const N:u64] self:$array[str N] needle:$str -> bool
```

Returns whether a string array contains `needle`.

<a id="arrayt-nis_empty"></a>

### is_empty

```text
fn is_empty [T const N:u64] self:$array[T N] -> bool
```

Returns whether `N` is zero.

<a id="arrayt-niter"></a>

### iter

```text
fn iter [T const N:u64] self:$array[T N] -> Iter[$T]
```

Returns an iterator over borrows of the elements.

<a id="arrayt-nlength"></a>

### length

```text
fn length [T const N:u64] self:$array[T N] -> u64
```

Returns the number of elements, which is `N`.

<a id="arrayt-nnth"></a>

### nth

```text
fn nth [T const N:u64] self:$array[T N] index:u64 -> $T
```

Returns a shared borrow of the element at a zero-based index. An out-of-range index
terminates the program. The source keeps ownership of the element.

### Array storage and ownership

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
| [is_empty](#slicetis_empty) | `fn is_empty self:$Slice[T] -> bool` | Whether the view has no elements |
| [iter](#slicetiter) | `fn iter self:$Slice[T] -> Iter[$T]` | Iterator over borrows of the elements |
| [length](#slicetlength) | `fn length self:$Slice[T] -> u64` | Number of elements in the view |
| [nth](#slicetnth) | `fn nth self:$Slice[T] index:u64 -> $T` | Borrow of the element at a zero-based index |

<a id="slicetis_empty"></a>

### is_empty

```text
fn is_empty self:$Slice[T] -> bool
```

Returns whether the view has no elements.

<a id="slicetiter"></a>

### iter

```text
fn iter self:$Slice[T] -> Iter[$T]
```

Returns an iterator over borrows of the elements.

<a id="slicetlength"></a>

### length

```text
fn length self:$Slice[T] -> u64
```

Returns the number of elements in the view.

<a id="slicetnth"></a>

### nth

```text
fn nth self:$Slice[T] index:u64 -> $T
```

Returns a shared borrow of the element at a zero-based index. An out-of-range index
terminates the program. The source keeps ownership of the element.

### Slice ownership

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
| [append](#bytesappend) | `fn append self:mut$Bytes source:$Bytes` | Copy the source bytes onto the end |
| [as_cstr](#bytesas_cstr) | `fn as_cstr self:$Bytes -> Option[$cstr]` | Borrow a NUL-terminated view if no byte is NUL |
| [capacity](#bytescapacity) | `fn capacity self:$Bytes -> u64` | Number of bytes available before growth |
| [clone](#bytesclone) | `fn clone self:$Bytes -> Bytes` | Independent byte buffer |
| [from_str](#bytesfrom_str) | `fn from_str source:$str -> Bytes` | Copy the text's UTF-8 bytes |
| [get](#bytesget) | `fn get self:$Bytes index:u64 -> Option[u8]` | Copy one byte if the index is in range |
| [iter](#bytesiter) | `fn iter self:$Bytes -> Iter[u8]` | Iterator that copies each byte |
| [length](#byteslength) | `fn length self:$Bytes -> u64` | Number of initialized bytes |
| [new](#bytesnew) | `fn new -> Bytes` | Empty byte buffer |
| [push](#bytespush) | `fn push self:mut$Bytes byte:u8` | Add one byte |
| [to_raw_buffer](#bytesto_raw_buffer) | `fn to_raw_buffer self:$Bytes -> RawBuffer` | Independent allocation containing exactly `length` initialized bytes |
| [to_str](#bytesto_str) | `fn to_str self:$Bytes -> Result[String Utf8Error]` | Validate and copy UTF-8 text |

<a id="bytesappend"></a>

### append

```text
fn append self:mut$Bytes source:$Bytes
```

Copies the source bytes onto the end.

<a id="bytesas_cstr"></a>

### as_cstr

```text
fn as_cstr self:$Bytes -> Option[$cstr]
```

Borrows a NUL-terminated view if no byte is NUL.

<a id="bytescapacity"></a>

### capacity

```text
fn capacity self:$Bytes -> u64
```

Returns the number of bytes available before growth.

<a id="bytesclone"></a>

### clone

```text
fn clone self:$Bytes -> Bytes
```

Returns an independent byte buffer.

<a id="bytesfrom_str"></a>

### from_str

```text
fn from_str source:$str -> Bytes
```

Copies the text's UTF-8 bytes.

<a id="bytesget"></a>

### get

```text
fn get self:$Bytes index:u64 -> Option[u8]
```

Returns a copied byte wrapped in `Option::Some`. An out-of-range index returns
`Option::None`. The byte buffer remains available.

<a id="bytesiter"></a>

### iter

```text
fn iter self:$Bytes -> Iter[u8]
```

Returns an iterator that copies each byte.

<a id="byteslength"></a>

### length

```text
fn length self:$Bytes -> u64
```

Returns the number of initialized bytes.

<a id="bytesnew"></a>

### new

```text
fn new -> Bytes
```

Creates an empty byte buffer.

<a id="bytespush"></a>

### push

```text
fn push self:mut$Bytes byte:u8
```

Adds one byte.

<a id="bytesto_raw_buffer"></a>

### to_raw_buffer

```text
fn to_raw_buffer self:$Bytes -> RawBuffer
```

Returns an independent allocation containing exactly `length` initialized bytes.

<a id="bytesto_str"></a>

### to_str

```text
fn to_str self:$Bytes -> Result[String Utf8Error]
```

Validates and copies UTF-8 text.

### Byte storage and conversions

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
| [clone](#mapk-vclone) | `fn clone self:$Map[K V] -> Map[K V]` | Independent map when `K: Clone` and `V: Clone` |
| [delete](#mapk-vdelete) | `fn delete self:mut$Map[K V] key:$K` | Remove and destroy a value, if present |
| [get](#mapk-vget) | `fn get self:$Map[K V] key:$K -> Option[$V]` | Borrow of a value, if present |
| [get_cloned](#mapk-vget_cloned) | `fn get_cloned self:$Map[K V] key:$K -> Option[V]` | Owned value when `V: Clone` |
| [get_copy](#mapk-vget_copy) | `fn get_copy self:$Map[K V] key:$K -> Option[V]` | Owned value when `V: Copy` |
| [get_mut](#mapk-vget_mut) | `fn get_mut self:mut$Map[K V] key:$K -> Option[mut$V]` | Exclusive borrow of a value, if present |
| [has](#mapk-vhas) | `fn has self:$Map[K V] key:$K -> bool` | Whether a key exists |
| [is_empty](#mapk-vis_empty) | `fn is_empty self:$Map[K V] -> bool` | Whether the map has no entries |
| [iter](#mapk-viter) | `fn iter self:$Map[K V] -> Iter[Pair[$K $V]]` | Iterator over borrowed key-value pairs |
| [keys](#mapk-vkeys) | `fn keys self:$Map[K V] -> List[K]` | Cloned keys when `K: Clone` |
| [length](#mapk-vlength) | `fn length self:$Map[K V] -> u64` | Number of entries |
| [new](#mapk-vnew) | `fn new -> Map[K V]` | Empty map |
| [remove](#mapk-vremove) | `fn remove self:mut$Map[K V] key:$K -> Option[V]` | Remove and return a value, if present |
| [set](#mapk-vset) | `fn set self:mut$Map[K V] key:K value:V` | Insert or replace an entry |
| [values](#mapk-vvalues) | `fn values self:$Map[K V] -> List[V]` | Cloned values when `V: Clone` |

<a id="mapk-vclone"></a>

### clone

```text
fn clone self:$Map[K V] -> Map[K V]
```

Returns an independent map when `K: Clone` and `V: Clone`.

<a id="mapk-vdelete"></a>

### delete

```text
fn delete self:mut$Map[K V] key:$K
```

Removes and destroys a value, if present.

<a id="mapk-vget"></a>

### get

```text
fn get self:$Map[K V] key:$K -> Option[$V]
```

Returns a borrow of a value, if present.

<a id="mapk-vget_cloned"></a>

### get_cloned

```text
fn get_cloned self:$Map[K V] key:$K -> Option[V]
```

Returns a cloned value, if present. `V` must implement `Clone`.

<a id="mapk-vget_copy"></a>

### get_copy

```text
fn get_copy self:$Map[K V] key:$K -> Option[V]
```

Returns a copied value, if present. `V` must implement `Copy`.

<a id="mapk-vget_mut"></a>

### get_mut

```text
fn get_mut self:mut$Map[K V] key:$K -> Option[mut$V]
```

Returns an exclusive borrow of a value, if present.

<a id="mapk-vhas"></a>

### has

```text
fn has self:$Map[K V] key:$K -> bool
```

Returns whether a key exists.

<a id="mapk-vis_empty"></a>

### is_empty

```text
fn is_empty self:$Map[K V] -> bool
```

Returns whether the map has no entries.

<a id="mapk-viter"></a>

### iter

```text
fn iter self:$Map[K V] -> Iter[Pair[$K $V]]
```

Returns an iterator over borrowed key-value pairs.

<a id="mapk-vkeys"></a>

### keys

```text
fn keys self:$Map[K V] -> List[K]
```

Returns a list of cloned keys when `K` implements `Clone`.

<a id="mapk-vlength"></a>

### length

```text
fn length self:$Map[K V] -> u64
```

Returns the number of entries.

<a id="mapk-vnew"></a>

### new

```text
fn new -> Map[K V]
```

Creates an empty map.

<a id="mapk-vremove"></a>

### remove

```text
fn remove self:mut$Map[K V] key:$K -> Option[V]
```

Removes and returns a value, if present.

<a id="mapk-vset"></a>

### set

```text
fn set self:mut$Map[K V] key:K value:V
```

Inserts or replaces an entry.

<a id="mapk-vvalues"></a>

### values

```text
fn values self:$Map[K V] -> List[V]
```

Returns a list of cloned values when `V` implements `Clone`.

### Map ownership and hashing

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
| [delete_str](#mapstring-vdelete_str) | `fn delete_str self:mut$Map[String V] key:$str` | Remove and destroy a value |
| [get_mut_str](#mapstring-vget_mut_str) | `fn get_mut_str self:mut$Map[String V] key:$str -> Option[mut$V]` | Exclusively borrow a value |
| [get_str](#mapstring-vget_str) | `fn get_str self:$Map[String V] key:$str -> Option[$V]` | Borrow a value |
| [has_str](#mapstring-vhas_str) | `fn has_str self:$Map[String V] key:$str -> bool` | Whether the text key exists |
| [remove_str](#mapstring-vremove_str) | `fn remove_str self:mut$Map[String V] key:$str -> Option[V]` | Remove a value |
| [set_str](#mapstring-vset_str) | `fn set_str self:mut$Map[String V] key:$str value:V` | Copy and insert a text key |

<a id="mapstring-vdelete_str"></a>

### delete_str

```text
fn delete_str self:mut$Map[String V] key:$str
```

Removes and destroys a value.

<a id="mapstring-vget_mut_str"></a>

### get_mut_str

```text
fn get_mut_str self:mut$Map[String V] key:$str -> Option[mut$V]
```

Exclusively borrows a value.

<a id="mapstring-vget_str"></a>

### get_str

```text
fn get_str self:$Map[String V] key:$str -> Option[$V]
```

Borrows a value.

<a id="mapstring-vhas_str"></a>

### has_str

```text
fn has_str self:$Map[String V] key:$str -> bool
```

Returns whether the text key exists.

<a id="mapstring-vremove_str"></a>

### remove_str

```text
fn remove_str self:mut$Map[String V] key:$str -> Option[V]
```

Removes a value.

<a id="mapstring-vset_str"></a>

### set_str

```text
fn set_str self:mut$Map[String V] key:$str value:V
```

Copies and inserts a text key.

### Map example

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
| [add](#setkadd) | `fn add self:mut$Set[K] key:K` | Add a value |
| [clone](#setkclone) | `fn clone self:$Set[K] -> Set[K]` | Independent set when `K: Clone` |
| [has](#setkhas) | `fn has self:$Set[K] key:$K -> bool` | Whether a value exists |
| [is_empty](#setkis_empty) | `fn is_empty self:$Set[K] -> bool` | Whether the set has no values |
| [iter](#setkiter) | `fn iter self:$Set[K] -> Iter[$K]` | Iterator over borrowed values |
| [length](#setklength) | `fn length self:$Set[K] -> u64` | Number of values |
| [new](#setknew) | `fn new -> Set[K]` | Empty set |
| [remove](#setkremove) | `fn remove self:mut$Set[K] key:$K` | Remove a value if present |
| [to_list](#setkto_list) | `fn to_list self:$Set[K] -> List[K]` | Cloned values in unspecified order when `K: Clone` |

<a id="setkadd"></a>

### add

```text
fn add self:mut$Set[K] key:K
```

Adds a value.

<a id="setkclone"></a>

### clone

```text
fn clone self:$Set[K] -> Set[K]
```

Returns an independent set when `K: Clone`.

<a id="setkhas"></a>

### has

```text
fn has self:$Set[K] key:$K -> bool
```

Returns whether a value exists.

<a id="setkis_empty"></a>

### is_empty

```text
fn is_empty self:$Set[K] -> bool
```

Returns whether the set has no values.

<a id="setkiter"></a>

### iter

```text
fn iter self:$Set[K] -> Iter[$K]
```

Returns an iterator over borrowed values.

<a id="setklength"></a>

### length

```text
fn length self:$Set[K] -> u64
```

Returns the number of values.

<a id="setknew"></a>

### new

```text
fn new -> Set[K]
```

Creates an empty set.

<a id="setkremove"></a>

### remove

```text
fn remove self:mut$Set[K] key:$K
```

Removes a value if present.

<a id="setkto_list"></a>

### to_list

```text
fn to_list self:$Set[K] -> List[K]
```

Returns a list of cloned values in unspecified order when `K` implements `Clone`.

### Set ownership and hashing

`iter` keeps the set as owner. `to_list` and `clone` require `K: Clone`.
Collisions, traversal order, and untrusted-key behavior match `Map`.

`Set[String]` accepts borrowed text keys:

| Method | Signature | Behavior |
|---|---|---|
| [add_str](#setstringadd_str) | `fn add_str self:mut$Set[String] key:$str` | Copy and add a text key |
| [has_str](#setstringhas_str) | `fn has_str self:$Set[String] key:$str -> bool` | Whether the text key exists |
| [remove_str](#setstringremove_str) | `fn remove_str self:mut$Set[String] key:$str` | Remove a text key if present |

<a id="setstringadd_str"></a>

### add_str

```text
fn add_str self:mut$Set[String] key:$str
```

Copies the borrowed text key into an owned String and adds it to the set.

<a id="setstringhas_str"></a>

### has_str

```text
fn has_str self:$Set[String] key:$str -> bool
```

Returns whether the borrowed text key exists. Lookup does not allocate.

<a id="setstringremove_str"></a>

### remove_str

```text
fn remove_str self:mut$Set[String] key:$str
```

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
| `Bytes` | `Iter[u8]` |
| `List[T]` | `Iter[$T]` |
| `Map[K V]` | `Iter[Pair[$K $V]]` |
| `Set[K]` | `Iter[$K]` |
| `Slice[T]` | `Iter[$T]` |
| `str` | `Iter[char]` |

A `for` loop consumes the iterator. Create another iterator to traverse the
source again.

Arrays, lists, slices, maps, and sets yield borrows because the source keeps
owning its elements. Clone a yielded value when an owned value is needed.

## Lazy iterator operations

Lazy operations return `Iter` and do no work until the result is consumed.

In trait signatures, the type `self` means the type that implements `Iterable[T]`.

| Method | Signature | Behavior |
|---|---|---|
| [chain](#iterabletchain) | `fn chain self:self other:Iter[T] -> Iter[T]` | Yield from `self`, then `other` |
| [enumerate](#iterabletenumerate) | `fn enumerate self:self -> Iter[Pair[i64 T]]` | Pair each value with its zero-based index |
| [filter](#iterabletfilter) | `fn filter self:self f:fn[$T -> bool] -> Iter[T]` | Keep matching values |
| [flat_map](#iterabletflat_map) | `fn flat_map [U] self:self f:fn[T -> Iter[U]] -> Iter[U]` | Transform and flatten one level |
| [map](#iterabletmap) | `fn map [U] self:self f:fn[T -> U] -> Iter[U]` | Transform each value |
| [skip](#iterabletskip) | `fn skip self:self n:u64 -> Iter[T]` | Omit the first `n` values |
| [skip_while](#iterabletskip_while) | `fn skip_while self:self f:fn[$T -> bool] -> Iter[T]` | Omit values while the predicate is true |
| [take](#iterablettake) | `fn take self:self n:u64 -> Iter[T]` | Yield at most `n` values |
| [take_while](#iterablettake_while) | `fn take_while self:self f:fn[$T -> bool] -> Iter[T]` | Yield while the predicate is true |
| [zip](#iterabletzip) | `fn zip [U] self:self other:Iter[U] -> Iter[Pair[T U]]` | Pair values until either iterator ends |

<a id="iterabletchain"></a>

### chain

```text
fn chain self:self other:Iter[T] -> Iter[T]
```

Yields from `self`, then `other`.

<a id="iterabletenumerate"></a>

### enumerate

```text
fn enumerate self:self -> Iter[Pair[i64 T]]
```

Pairs each value with its zero-based index.

<a id="iterabletfilter"></a>

### filter

```text
fn filter self:self f:fn[$T -> bool] -> Iter[T]
```

Keeps matching values.

<a id="iterabletflat_map"></a>

### flat_map

```text
fn flat_map [U] self:self f:fn[T -> Iter[U]] -> Iter[U]
```

Transforms and flattens one level.

<a id="iterabletmap"></a>

### map

```text
fn map [U] self:self f:fn[T -> U] -> Iter[U]
```

Transforms each value.

<a id="iterabletskip"></a>

### skip

```text
fn skip self:self n:u64 -> Iter[T]
```

Omits the first `n` values.

<a id="iterabletskip_while"></a>

### skip_while

```text
fn skip_while self:self f:fn[$T -> bool] -> Iter[T]
```

Omits values while the predicate is true.

<a id="iterablettake"></a>

### take

```text
fn take self:self n:u64 -> Iter[T]
```

Yields at most `n` values.

<a id="iterablettake_while"></a>

### take_while

```text
fn take_while self:self f:fn[$T -> bool] -> Iter[T]
```

Yields while the predicate is true.

<a id="iterabletzip"></a>

### zip

```text
fn zip [U] self:self other:Iter[U] -> Iter[Pair[T U]]
```

Pairs values until either iterator ends.


## Terminal iterator operations

Terminal operations borrow the iterator exclusively, advance its state, and
return a non-iterator value. A named iterator remains available after the call.
Operations such as `any` and `find` can stop before it is exhausted.

| Method | Signature | Behavior |
|---|---|---|
| [all](#iterabletall) | `fn all self:mut$self f:fn[$T -> bool] -> bool` | Whether every value matches |
| [any](#iterabletany) | `fn any self:mut$self f:fn[$T -> bool] -> bool` | Whether any value matches |
| [collect](#iterabletcollect) | `fn collect self:mut$self -> List[T]` | All remaining values |
| [count](#iterabletcount) | `fn count self:mut$self -> u64` | Number of remaining values |
| [find](#iterabletfind) | `fn find self:mut$self f:fn[$T -> bool] -> Option[T]` | First matching value |
| [fold](#iterabletfold) | `fn fold [U] self:mut$self acc:U f:fn[U T -> U] -> U` | Reduce from an initial value |
| [max](#itertmax) | `fn max self:mut$Iter[T] -> Option[T]` | Maximum value when `T` implements `Ord` |
| [max_by](#iterabletmax_by) | `fn max_by self:mut$self f:fn[$T $T -> bool] -> Option[T]` | Maximum selected by a callback |
| [min](#itertmin) | `fn min self:mut$Iter[T] -> Option[T]` | Minimum value when `T` implements `Ord` |
| [min_by](#iterabletmin_by) | `fn min_by self:mut$self f:fn[$T $T -> bool] -> Option[T]` | Minimum selected by a callback |
| [next](#iterabletnext) | `fn next self:mut$self -> Option[T]` | Next value, if present |
| [partition](#iterabletpartition) | `fn partition self:mut$self f:fn[$T -> bool] -> Pair[List[T] List[T]]` | Matching and non-matching lists |
| [reduce](#iterabletreduce) | `fn reduce self:mut$self f:fn[T T -> T] -> Option[T]` | Reduce from the first value |
| [sum](#iteri64sum) | `fn sum self:mut$Iter[i64] -> i64` | Add owned `i64` values |

<a id="iterabletall"></a>

### all

```text
fn all self:mut$self f:fn[$T -> bool] -> bool
```

Returns whether every value matches.

<a id="iterabletany"></a>

### any

```text
fn any self:mut$self f:fn[$T -> bool] -> bool
```

Returns whether any value matches.

<a id="iterabletcollect"></a>

### collect

```text
fn collect self:mut$self -> List[T]
```

Collects all remaining values into a list.

<a id="iterabletcount"></a>

### count

```text
fn count self:mut$self -> u64
```

Returns the number of remaining values.

<a id="iterabletfind"></a>

### find

```text
fn find self:mut$self f:fn[$T -> bool] -> Option[T]
```

Returns the first matching value, if present.

<a id="iterabletfold"></a>

### fold

```text
fn fold [U] self:mut$self acc:U f:fn[U T -> U] -> U
```

Reduces from an initial value.

<a id="itertmax"></a>

### max

```text
fn max self:mut$Iter[T] -> Option[T]
```

Returns the maximum value, if present. `T` must implement `Ord`.

<a id="iterabletmax_by"></a>

### max_by

```text
fn max_by self:mut$self f:fn[$T $T -> bool] -> Option[T]
```

Returns the maximum value selected by the comparison callback, if present.

<a id="itertmin"></a>

### min

```text
fn min self:mut$Iter[T] -> Option[T]
```

Returns the minimum value, if present. `T` must implement `Ord`.

<a id="iterabletmin_by"></a>

### min_by

```text
fn min_by self:mut$self f:fn[$T $T -> bool] -> Option[T]
```

Returns the minimum value selected by the comparison callback, if present.

<a id="iterabletnext"></a>

### next

```text
fn next self:mut$self -> Option[T]
```

Advances the iterator and returns the next value, if present.

<a id="iterabletpartition"></a>

### partition

```text
fn partition self:mut$self f:fn[$T -> bool] -> Pair[List[T] List[T]]
```

Separates the remaining values into matching and non-matching lists.

<a id="iterabletreduce"></a>

### reduce

```text
fn reduce self:mut$self f:fn[T T -> T] -> Option[T]
```

Reduces from the first value.

<a id="iteri64sum"></a>

### sum

```text
fn sum self:mut$Iter[i64] -> i64
```

Adds owned `i64` values.

### Iterator example

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
