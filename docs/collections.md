# Collections and Iterators

Import `std` to use the collection methods on this page:

```casa
import "std"
```

Run complete examples from the repository root with
`./casac sample.casa -L lib -r`. Tables abbreviate standard-library names as
`List`, `Option`, and similar local names. Source code uses `std::List`,
`std::Option`, and the other qualified names. See [reference notation](notation.md).

| Type | Need |
|---|---|
| [Array](#arrays) | Fixed number of elements |
| [Bytes](#bytes) | Binary data |
| [Iterators](#iterator-sources) | Process a sequence |
| [List](lists.md) | Growable sequence |
| [Map](#maps) | Key-value lookup |
| [RawBuffer](#raw-buffers) | Untyped allocation for low-level code |
| [Set](#sets) | Unique values |
| [Slice](#slices) | Borrowed sequence range |
| [String](strings-and-io.md#owned-strings) | Growable UTF-8 text |

## Arrays

`array[T N]` is a sequence of exactly `N` elements, created with bracket syntax.
The length is part of the type, so `[10, 20, 30]` has type `array[i64 3]` and
arrays of different lengths are different types:

```casa
import "std"

[10, 20, 30] = numbers: array[i64 3]
1 numbers.nth print # 20
```

| Method | Signature | Description |
|---|---|---|
| [as_slice](#arrayt-nas_slice) | `fn as_slice [T const N:u64] self:$array[T N] -> Slice[T]` | Borrowed view of the complete array |
| [clone](#arrayt-nclone) | `fn clone self:$array[T N] -> array[T N]` | Independent array when `T: Clone` |
| [contains](#arraystr-ncontains) | `fn contains [const N:u64] self:$array[str N] needle:$str -> bool` | Whether a string array contains `needle` |
| [is_empty](#arrayt-nis_empty) | `fn is_empty [T const N:u64] self:$array[T N] -> bool` | Whether `N` is zero |
| [iter](#arrayt-niter) | `fn iter [T const N:u64] self:$array[T N] -> Iter[$T]` | Iterator over borrows of the elements |
| [length](#arrayt-nlength) | `fn length [T const N:u64] self:$array[T N] -> u64` | Number of elements, which is `N` |
| [nth](#arrayt-nnth) | `fn nth [T const N:u64] self:$array[T N] index:u64 -> $T` | Borrow of the element at a zero-based index |
| [slice](#arrayt-nslice) | `fn slice [T const N:u64] self:$array[T N] start:u64 stop:u64 -> Slice[T]` | Borrowed half-open range `[start, stop)` |

### array[T N]::as_slice

```text
fn as_slice [T const N:u64] self:$array[T N] -> Slice[T]
```

Borrows the complete array as a [runtime-length view](#slices).

### array[T N]::clone

```text
fn clone self:$array[T N] -> array[T N]
```

Returns an independent array when `T: Clone`.

### array[str N]::contains

```text
fn contains [const N:u64] self:$array[str N] needle:$str -> bool
```

Returns whether a string array contains `needle`.

### array[T N]::is_empty

```text
fn is_empty [T const N:u64] self:$array[T N] -> bool
```

Returns whether `N` is zero.

### array[T N]::iter

```text
fn iter [T const N:u64] self:$array[T N] -> Iter[$T]
```

Returns an iterator over borrows of the elements.

### array[T N]::length

```text
fn length [T const N:u64] self:$array[T N] -> u64
```

Returns the number of elements, which is `N`.

### array[T N]::nth

```text
fn nth [T const N:u64] self:$array[T N] index:u64 -> $T
```

Returns a [shared borrow](ownership.md#borrow-for-a-call) of the element at a zero-based index. An out-of-range index
terminates the program. The source keeps ownership of the element.

### array[T N]::slice

```text
fn slice [T const N:u64] self:$array[T N] start:u64 stop:u64 -> Slice[T]
```

Borrows `[start, stop)`. Requires `start <= stop <= N`. An invalid range terminates
the program. Call: `stop start values.slice`.

### Array storage and ownership

An array value is its element storage: it carries no length word, and `.length`
resolves to the constant in its type. Array length cannot change. Use `List[T]`
when values must be added or removed.

A function that accepts arrays of any length takes a constant length parameter:

```casa
import "std"

fn total [const N:u64] values:$array[i64 N] -> i64 {
    0 = sum: i64
    for value in values.iter do
        value += sum
    done
    sum
}
```

Each evaluation of an array literal produces an independent owned array. The
literal takes [ownership](ownership.md#move-a-value) of non-Copy elements, so
those element bindings cannot be used again afterwards:

The following invalid example uses an owning struct:

```casa
struct Resource {
    id: i64
}

Resource { id: 1 } = resource
[resource] = owned: array[Resource 1]
resource drop # error: owner `resource` was already moved
```

The array destroys its elements when it goes out of scope. `clone` produces an
independent array when `T: Clone`. An array is `Copy` when `T: Copy`, including
when `N` is zero. Arrays with non-`Copy` elements remain affine. Indexing with a
constant past the last element is a compile-time error. Indexing past it with a
runtime value terminates the program.

`nth` and `iter` read through a borrowed array, so they return `$T` rather
than an owned element. The array stays the only owner: an element type with a
reserved `drop` method runs its hook once, when the array is destroyed. Use
`.clone` on the result when an owned value is needed.

## Slices

`Slice[T]` is a borrowed runtime-length view over contiguous elements. Arrays and
lists supply `Slice[T]`, and `Bytes` supplies `Slice[u8]`. One function can process
all sources of the same element type without copying elements or changing owners:

```casa
import "std"

fn total values:$std::Slice[i64] -> i64 {
    0 = sum
    for value in values.iter do
        value copy += sum
    done
    sum
}

[10, 20, 30, 40] = numbers
4 1 numbers.slice = middle
middle total print # 90
[10, 20, 30] std::List::from_array = list
list.as_slice total print # 60
```

| Method | Signature | Description |
|---|---|---|
| [is_empty](#slicetis_empty) | `fn is_empty self:$Slice[T] -> bool` | Whether the view has no elements |
| [iter](#slicetiter) | `fn iter self:$Slice[T] -> Iter[$T]` | Iterator over borrows of the elements |
| [length](#slicetlength) | `fn length self:$Slice[T] -> u64` | Number of elements in the view |
| [nth](#slicetnth) | `fn nth self:$Slice[T] index:u64 -> $T` | Borrow of the element at a zero-based index |
| [slice](#slicetslice) | `fn slice self:$Slice[T] start:u64 stop:u64 -> Slice[T]` | Borrowed subrange `[start, stop)` |

### Slice[T]::is_empty

```text
fn is_empty self:$Slice[T] -> bool
```

Returns whether the view has no elements.

### Slice[T]::iter

```text
fn iter self:$Slice[T] -> Iter[$T]
```

Returns an iterator over borrows of the elements.

### Slice[T]::length

```text
fn length self:$Slice[T] -> u64
```

Returns the number of elements in the view.

### Slice[T]::nth

```text
fn nth self:$Slice[T] index:u64 -> $T
```

Returns a shared borrow of the element at a zero-based index. An out-of-range index
terminates the program. The source keeps ownership of the element.

### Slice[T]::slice

```text
fn slice self:$Slice[T] start:u64 stop:u64 -> Slice[T]
```

Borrows a subrange relative to this view. Requires `start <= stop <= self.length`.
An invalid range terminates the program. Call: `stop start view.slice`. Equal
bounds produce an empty view, including at the end of the source. Empty views
have length zero and yield no elements. Indexing an empty view terminates.

### Slice ownership

Construct views with `as_slice` or `slice` on an array, a list, or `Bytes`.
Construction checks the range and retains a shared borrow of the source owner.
A slice does not own, copy, or destroy elements. A source remains loaned through
subranges, element borrows, iterators, and aggregates containing a returned view.
This also applies to empty views. Safe code cannot mutate, move, or destroy a
loaned source. The loan ends after the last use of its dependent values.

`Slice[T]` is `Copy`, including when `T` is not `Copy`. Copies retain the source
borrow and do not copy elements. View construction and subranges use automatic
descriptor storage without allocation. A subrange borrows its parent view, so
that view must also remain live. Moving a view into an owned aggregate field or closure capture
can allocate storage for the descriptor, without copying the elements.

Views expose shared access only. `Bytes.iter` still yields copied `u8` values,
while `Bytes.as_slice.iter` yields `$u8`. Mutable and consuming traversal are
not part of this API.

### Migration from list-specific slices

Existing `List.slice`, `List.as_slice`, `Slice.length`, `Slice.is_empty`,
`Slice.nth`, and `Slice.iter` calls keep their behavior. The former public
`source`, `start`, and `size` fields are removed. Use `length` instead of `size`,
`view.slice` for relative subranges, and `nth` or `iter` for element access.
Construct a view with `stop start owner.slice` instead of a `Slice` struct literal
or positional constructor. Keep the original owner separately when it is needed
after the view's last use.

## Lists

`std::List[T]` owns a growable sequence. Use it when elements must be added or
removed. The [List reference](lists.md) covers list operations, including
[reading an element](lists.md#listtget),
[appending](lists.md#listtpush), and [removal](lists.md#listtpop).


## Bytes

`Bytes` is a non-`Copy` owned growable buffer for binary data. It stores one
`u8` per byte. Mutation requires an [exclusive borrow](ownership.md#borrow-for-a-call).

| Method | Signature | Description |
|---|---|---|
| [append](#bytesappend) | `fn append self:mut$Bytes source:$Bytes` | Copy the source bytes onto the end |
| [as_cstr](#bytesas_cstr) | `fn as_cstr self:$Bytes -> Option[$cstr]` | Borrow a NUL-terminated view if no byte is NUL |
| [as_slice](#bytesas_slice) | `fn as_slice self:$Bytes -> Slice[u8]` | Borrowed view of all initialized bytes |
| [capacity](#bytescapacity) | `fn capacity self:$Bytes -> u64` | Number of bytes available before growth |
| [clone](#bytesclone) | `fn clone self:$Bytes -> Bytes` | Independent byte buffer |
| [from_str](#bytesfrom_str) | `fn from_str source:$str -> Bytes` | Copy the text's UTF-8 bytes |
| [get](#bytesget) | `fn get self:$Bytes index:u64 -> Option[u8]` | Copy one byte if the index is in range |
| [iter](#bytesiter) | `fn iter self:$Bytes -> Iter[u8]` | Iterator that copies each byte |
| [length](#byteslength) | `fn length self:$Bytes -> u64` | Number of initialized bytes |
| [new](#bytesnew) | `fn new -> Bytes` | Empty byte buffer |
| [push](#bytespush) | `fn push self:mut$Bytes byte:u8` | Add one byte |
| [slice](#bytesslice) | `fn slice self:$Bytes start:u64 stop:u64 -> Slice[u8]` | Borrowed byte range `[start, stop)` |
| [to_raw_buffer](#bytesto_raw_buffer) | `fn to_raw_buffer self:$Bytes -> RawBuffer` | Independent allocation containing exactly `length` initialized bytes |
| [to_str](#bytesto_str) | `fn to_str self:$Bytes -> Result[String Utf8Error]` | Validate and copy UTF-8 text |

### Bytes::append

```text
fn append self:mut$Bytes source:$Bytes
```

Copies the source bytes onto the end.

### Bytes::as_cstr

```text
fn as_cstr self:$Bytes -> Option[$cstr]
```

Borrows a NUL-terminated view if no byte is NUL.

### Bytes::as_slice

```text
fn as_slice self:$Bytes -> Slice[u8]
```

Borrows all initialized bytes as a [sequence view](#slices). The trailing NUL
slot is outside the view.

### Bytes::capacity

```text
fn capacity self:$Bytes -> u64
```

Returns the number of bytes available before growth.

### Bytes::clone

```text
fn clone self:$Bytes -> Bytes
```

Returns an independent byte buffer.

### Bytes::from_str

```text
fn from_str source:$str -> Bytes
```

Copies the text's UTF-8 bytes.

### Bytes::get

```text
fn get self:$Bytes index:u64 -> Option[u8]
```

Returns a copied byte wrapped in `Option::Some`. An out-of-range index returns
`Option::None`. The byte buffer remains available.

### Bytes::iter

```text
fn iter self:$Bytes -> Iter[u8]
```

Returns an iterator that copies each byte.

### Bytes::length

```text
fn length self:$Bytes -> u64
```

Returns the number of initialized bytes.

### Bytes::new

```text
fn new -> Bytes
```

Creates an empty byte buffer.

### Bytes::push

```text
fn push self:mut$Bytes byte:u8
```

Adds one byte.

### Bytes::slice

```text
fn slice self:$Bytes start:u64 stop:u64 -> Slice[u8]
```

Borrows `[start, stop)`. Requires `start <= stop <= self.length`. An invalid range
terminates the program. Call: `stop start bytes.slice`.

### Bytes::to_raw_buffer

```text
fn to_raw_buffer self:$Bytes -> RawBuffer
```

Returns an independent allocation containing exactly `length` initialized bytes.

### Bytes::to_str

```text
fn to_str self:$Bytes -> Result[String Utf8Error]
```

Validates and copies UTF-8 text.

### Byte storage and conversions

`to_str` borrows the buffer and keeps it unchanged. Invalid UTF-8 returns
`Utf8Error`. There is no consuming `into_str` conversion. `Bytes` implements
`Eq` and `Hashable`, but not `Display`. Its storage keeps a trailing NUL outside
the logical length so `as_cstr` does not allocate.

See [examples/bytes.casa](../examples/bytes.casa) for a runnable example.

## Maps

Map storage fields are private. Construct a map with `new`, inspect its count
with `length`, and use the checked lookup and mutation methods below. Callers
cannot construct a map from bucket pointers or assign its size or capacity.

Low-level `entry_key` and `entry_value` require `unsafe`. They interpret the
supplied address as `$K` or `$V` without checking it. The address must point to an
initialized value that remains live and immutable for the returned borrow,
which is bounded by the shared Map borrow. `entry_value` expects the value
address, not the start of an entry. Prefer `get`, `get_mut`, or `iter`.

`Map[K V]` associates unique keys with values. `K` must implement [Hashable](traits.md#hashable-contract).
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

| Method | Signature | Description |
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

### Map[K V]::clone

```text
fn clone self:$Map[K V] -> Map[K V]
```

Returns an independent map when `K: Clone` and `V: Clone`.

### Map[K V]::delete

```text
fn delete self:mut$Map[K V] key:$K
```

Removes and destroys a value, if present.

### Map[K V]::get

```text
fn get self:$Map[K V] key:$K -> Option[$V]
```

Returns a borrow of a value, if present.

### Map[K V]::get_cloned

```text
fn get_cloned self:$Map[K V] key:$K -> Option[V]
```

Returns a cloned value, if present. `V` must implement `Clone`.

### Map[K V]::get_copy

```text
fn get_copy self:$Map[K V] key:$K -> Option[V]
```

Returns a copied value, if present. `V` must implement `Copy`.

### Map[K V]::get_mut

```text
fn get_mut self:mut$Map[K V] key:$K -> Option[mut$V]
```

Returns an exclusive borrow of a value, if present.

### Map[K V]::has

```text
fn has self:$Map[K V] key:$K -> bool
```

Returns whether a key exists.

### Map[K V]::is_empty

```text
fn is_empty self:$Map[K V] -> bool
```

Returns whether the map has no entries.

### Map[K V]::iter

```text
fn iter self:$Map[K V] -> Iter[Pair[$K $V]]
```

Returns an iterator over borrowed key-value pairs.

### Map[K V]::keys

```text
fn keys self:$Map[K V] -> List[K]
```

Returns a list of cloned keys when `K` implements `Clone`.

### Map[K V]::length

```text
fn length self:$Map[K V] -> u64
```

Returns the number of entries.

### Map[K V]::new

```text
fn new -> Map[K V]
```

Creates an empty map.

### Map[K V]::remove

```text
fn remove self:mut$Map[K V] key:$K -> Option[V]
```

Removes and returns a value, if present.

### Map[K V]::set

```text
fn set self:mut$Map[K V] key:K value:V
```

Inserts or replaces an entry.

### Map[K V]::values

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

| Method | Signature | Description |
|---|---|---|
| [delete_str](#mapstring-vdelete_str) | `fn delete_str self:mut$Map[String V] key:$str` | Remove and destroy a value |
| [get_mut_str](#mapstring-vget_mut_str) | `fn get_mut_str self:mut$Map[String V] key:$str -> Option[mut$V]` | Exclusively borrow a value |
| [get_str](#mapstring-vget_str) | `fn get_str self:$Map[String V] key:$str -> Option[$V]` | Borrow a value |
| [has_str](#mapstring-vhas_str) | `fn has_str self:$Map[String V] key:$str -> bool` | Whether the text key exists |
| [remove_str](#mapstring-vremove_str) | `fn remove_str self:mut$Map[String V] key:$str -> Option[V]` | Remove a value |
| [set_str](#mapstring-vset_str) | `fn set_str self:mut$Map[String V] key:$str value:V` | Copy and insert a text key |

### Map[String V]::delete_str

```text
fn delete_str self:mut$Map[String V] key:$str
```

Removes and destroys a value.

### Map[String V]::get_mut_str

```text
fn get_mut_str self:mut$Map[String V] key:$str -> Option[mut$V]
```

Exclusively borrows a value.

### Map[String V]::get_str

```text
fn get_str self:$Map[String V] key:$str -> Option[$V]
```

Borrows a value.

### Map[String V]::has_str

```text
fn has_str self:$Map[String V] key:$str -> bool
```

Returns whether the text key exists.

### Map[String V]::remove_str

```text
fn remove_str self:mut$Map[String V] key:$str -> Option[V]
```

Removes a value.

### Map[String V]::set_str

```text
fn set_str self:mut$Map[String V] key:$str value:V
```

Copies and inserts a text key.

### Map example

See [examples/hash_map.casa](../examples/hash_map.casa) for a runnable map
example.

## Sets

`Set[K]` stores unique [Hashable](traits.md#hashable-contract) values:

```casa
import "std"

std::Set[str]::new = names
"Ada" names.add
"Grace" names.add
"Ada" names.has print # true
```

| Method | Signature | Description |
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

### Set[K]::add

```text
fn add self:mut$Set[K] key:K
```

Adds a value.

### Set[K]::clone

```text
fn clone self:$Set[K] -> Set[K]
```

Returns an independent set when `K: Clone`.

### Set[K]::has

```text
fn has self:$Set[K] key:$K -> bool
```

Returns whether a value exists.

### Set[K]::is_empty

```text
fn is_empty self:$Set[K] -> bool
```

Returns whether the set has no values.

### Set[K]::iter

```text
fn iter self:$Set[K] -> Iter[$K]
```

Returns an iterator over borrowed values.

### Set[K]::length

```text
fn length self:$Set[K] -> u64
```

Returns the number of values.

### Set[K]::new

```text
fn new -> Set[K]
```

Creates an empty set.

### Set[K]::remove

```text
fn remove self:mut$Set[K] key:$K
```

Removes a value if present.

### Set[K]::to_list

```text
fn to_list self:$Set[K] -> List[K]
```

Returns a list of cloned values in unspecified order when `K` implements `Clone`.

### Set ownership and hashing

`iter` keeps the set as owner. `to_list` and `clone` require `K: Clone`.
Collisions, traversal order, and untrusted-key behavior match `Map`.

`Set[String]` accepts borrowed text keys:

| Method | Signature | Description |
|---|---|---|
| [add_str](#setstringadd_str) | `fn add_str self:mut$Set[String] key:$str` | Copy and add a text key |
| [has_str](#setstringhas_str) | `fn has_str self:$Set[String] key:$str -> bool` | Whether the text key exists |
| [remove_str](#setstringremove_str) | `fn remove_str self:mut$Set[String] key:$str` | Remove a text key if present |

### Set[String]::add_str

```text
fn add_str self:mut$Set[String] key:$str
```

Copies the borrowed text key into an owned String and adds it to the set.

### Set[String]::has_str

```text
fn has_str self:$Set[String] key:$str -> bool
```

Returns whether the borrowed text key exists. Lookup does not allocate.

### Set[String]::remove_str

```text
fn remove_str self:mut$Set[String] key:$str
```

Removes and destroys the matching key, if present. Removal does not allocate.

## Owned strings

`std::String` owns growable UTF-8 text. Use it to assemble text in steps.
The [text reference](strings-and-io.md#owned-strings) documents its methods
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

A [for loop](control-flow.md#for-loops) consumes the iterator. Create another iterator to traverse the
source again. A borrowed iterator remains available after its loop ends.

Lists also provide [`iter_mut`](lists.md#listtiter_mut), which lends one mutable
element at a time, and [`into_iter`](lists.md#listtinto_iter), which consumes
the list and yields owned elements. `ListIterMut[T]` supports `for` and its own
`all`, `any`, `count`, `find`, and `next` methods. It does not implement
`Iterable`. Consuming list iteration returns `Iter[T]` and supports the lazy
and terminal operations below.

`iter` on arrays, lists, slices, maps, and sets yields borrows because the source keeps
owning its elements. Clone a yielded value when an owned value is needed.

## Lazy iterator operations

Lazy operations return `Iter` and do no work until the result is consumed.

In trait signatures, the type `self` means the type that implements `Iterable[T]`.

| Method | Signature | Description |
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

### Iterable[T]::chain

```text
fn chain self:self other:Iter[T] -> Iter[T]
```

Yields from `self`, then `other`.

### Iterable[T]::enumerate

```text
fn enumerate self:self -> Iter[Pair[i64 T]]
```

Pairs each value with its zero-based index.

### Iterable[T]::filter

```text
fn filter self:self f:fn[$T -> bool] -> Iter[T]
```

Keeps matching values.

### Iterable[T]::flat_map

```text
fn flat_map [U] self:self f:fn[T -> Iter[U]] -> Iter[U]
```

Transforms and flattens one level.

### Iterable[T]::map

```text
fn map [U] self:self f:fn[T -> U] -> Iter[U]
```

Transforms each value.

### Iterable[T]::skip

```text
fn skip self:self n:u64 -> Iter[T]
```

Omits the first `n` values.

### Iterable[T]::skip_while

```text
fn skip_while self:self f:fn[$T -> bool] -> Iter[T]
```

Omits values while the predicate is true.

### Iterable[T]::take

```text
fn take self:self n:u64 -> Iter[T]
```

Yields at most `n` values.

### Iterable[T]::take_while

```text
fn take_while self:self f:fn[$T -> bool] -> Iter[T]
```

Yields while the predicate is true.

### Iterable[T]::zip

```text
fn zip [U] self:self other:Iter[U] -> Iter[Pair[T U]]
```

Pairs values until either iterator ends.


## Terminal iterator operations

Terminal operations borrow the iterator exclusively, advance its state, and
return a non-iterator value. A named iterator remains available after the call.
Operations such as `any` and `find` can stop before it is exhausted.

| Method | Signature | Description |
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
| [try_fold](#iterablettry_fold) | `fn try_fold [U E] self:mut$self acc:U f:fn[U T -> Result[U E]] -> Result[U E]` | Fallible reduction |
| [try_for_each](#iterablettry_for_each) | `fn try_for_each [E] self:mut$self f:fn[T -> Result[Unit E]] -> Result[Unit E]` | Fallible action for each value |

### Iterable[T]::all

```text
fn all self:mut$self f:fn[$T -> bool] -> bool
```

Returns whether every value matches.

### Iterable[T]::any

```text
fn any self:mut$self f:fn[$T -> bool] -> bool
```

Returns whether any value matches.

### Iterable[T]::collect

```text
fn collect self:mut$self -> List[T]
```

Collects all remaining values into a list.

### Iterable[T]::count

```text
fn count self:mut$self -> u64
```

Returns the number of remaining values.

### Iterable[T]::find

```text
fn find self:mut$self f:fn[$T -> bool] -> Option[T]
```

Returns the first matching value, if present.

### Iterable[T]::fold

```text
fn fold [U] self:mut$self acc:U f:fn[U T -> U] -> U
```

Reduces from an initial value.

### Iter[T]::max

```text
fn max self:mut$Iter[T] -> Option[T]
```

Returns the maximum value, if present. `T` must implement `Ord`.

### Iterable[T]::max_by

```text
fn max_by self:mut$self f:fn[$T $T -> bool] -> Option[T]
```

Returns the maximum value selected by the comparison callback, if present.

### Iter[T]::min

```text
fn min self:mut$Iter[T] -> Option[T]
```

Returns the minimum value, if present. `T` must implement `Ord`.

### Iterable[T]::min_by

```text
fn min_by self:mut$self f:fn[$T $T -> bool] -> Option[T]
```

Returns the minimum value selected by the comparison callback, if present.

### Iterable[T]::next

```text
fn next self:mut$self -> Option[T]
```

Advances the iterator and returns the next value, if present.

### Iterable[T]::partition

```text
fn partition self:mut$self f:fn[$T -> bool] -> Pair[List[T] List[T]]
```

Separates the remaining values into matching and non-matching lists.

### Iterable[T]::reduce

```text
fn reduce self:mut$self f:fn[T T -> T] -> Option[T]
```

Reduces from the first value.

### Iter[i64]::sum

```text
fn sum self:mut$Iter[i64] -> i64
```

Adds owned `i64` values.

### Iterable[T]::try_fold

```text
fn try_fold [U E] self:mut$self acc:U f:fn[U T -> Result[U E]] -> Result[U E]
```

Calls `f` with the current accumulator and item until the iterator ends or `f`
returns `Error`. An empty iterator returns `Ok(acc)`. On success, the final
accumulator is returned. On failure, the original error is returned, and the
next unvisited item remains available from the iterator.

Owned accumulators and items move into the callback. It returns the next
accumulator on success. On failure, it must consume, destroy, or return the
owners it received. `try_fold` does not duplicate them. Borrowed items remain
borrows, with their source loans in force.

### Iterable[T]::try_for_each

```text
fn try_for_each [E] self:mut$self f:fn[T -> Result[Unit E]] -> Result[Unit E]
```

Calls `f` for each item until the iterator ends or `f` returns `Error`. On
success, returns `Ok(Unit::Value)`. On failure, returns the original error and
leaves unvisited items available from the iterator. Owned items move into the
callback, and `try_for_each` discards each successful Unit value. Borrowed
items retain their source loans.

### Iterator example

This pipeline skips two values, takes four, keeps even values, and doubles
them. Only `collect` runs the pipeline:

```casa
import "std"

[1, 2, 3, 4, 5, 6, 7] = values: array[i64 7]
2 values.iter.skip = rest
4 rest.take = window
{ copy 2 % 0 == } window.filter = even
{ copy 2 * } even.map.collect = doubled
```

See [examples/iterator_combinators.casa](../examples/iterator_combinators.casa)
for every lazy and terminal operation.

## Raw buffers

`RawBuffer` owns an untyped allocation. Its storage field is private, and it
releases that allocation when dropped. `Bytes` provides checked byte access
when raw memory is not needed.

| Method | Signature | Behavior |
|---|---|---|
| `data` | `fn data self:$RawBuffer -> ptr` | Non-owning pointer, with no setter |
| `from_raw` | `unsafe fn from_raw data:ptr -> RawBuffer` | Adopt sole ownership of an allocation |
| `into_raw` | `fn into_raw self:RawBuffer -> ptr` | Consume the buffer and transfer responsibility for freeing its allocation |
| `new` | `fn new size:u64 -> RawBuffer` | Own `size` uninitialized bytes |
| `swap_with` | `fn swap_with self:mut$RawBuffer other:mut$RawBuffer` | Exchange the allocations of two exclusively borrowed buffers |

`data` does not transfer ownership or extend the allocation's lifetime. Raw
reads, writes, and deallocation require `unsafe`. Initialize bytes before
reading them, stay within the allocation, and respect active typed borrows.

`from_raw` requires null or the start of a complete, live allocation from
`alloc`. Ownership transfers to the returned buffer. The caller must not free
the pointer afterwards or retain another owner. A pointer returned by
`into_raw` must eventually be freed or adopted by one owner.

```casa
import "std"

8 std::RawBuffer::new = buffer
# SAFETY: buffer owns eight writable bytes.
unsafe { 42 buffer.data store64 }
buffer.into_raw = address
# SAFETY: into_raw transferred sole ownership of this alloc allocation.
unsafe { address std::RawBuffer::from_raw } = adopted
# SAFETY: adopted still owns the eight initialized bytes.
unsafe { adopted.data load64 } print
```
