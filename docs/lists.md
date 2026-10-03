# List

`std::List[T]` owns a growable sequence of `T` values. Use it when elements must be
added or removed. Import `std` and qualify its constructors.

## Create and use a list

Save this complete program as `sample.casa` and run `./casac sample.casa -L lib -r` from
the repository root:

```casa
import "std"

[10, 20] std::List::from_array = numbers
30 numbers.push
0 numbers.get print "\n" print
numbers.pop print "\n" print
```

Output:

```text
10
30
```

`push` appends `30`. `get` borrows the first element, and `print` finishes using that
borrow before `pop` changes the list. `pop` removes and returns `30`. The list then
contains `[10, 20]`.

## Operations

The table summarizes operations from [lib/std.casa](../lib/std.casa). Signatures use
local type names. Bounds inherited from an `impl` block are stated with the relevant
behavior. See [reference notation](notation.md) for consumption order and qualified
names. Call forms below assume a list binding named `numbers`.

| Method | Signature | Behavior |
|---|---|---|
| [List[T]::new](#listtnew) | `fn new [T] -> List[T]` | Empty list |
| [List[T]::from_array](#listtfrom_array) | `fn from_array[T const N:u64] array:array[T N] -> List[T]` | List containing the array values |
| [List[T]::length](#listtlength) | `fn length self:$List -> u64` | Number of elements |
| [List[T]::is_empty](#listtis_empty) | `fn is_empty self:$List -> bool` | Whether the list has no elements |
| [List[T]::get](#listtget) | `fn get [T] self:$List[T] n:u64 -> $T` | Borrow of the element at a zero-based index |
| [List[T]::get_ref](#listtget_ref) | `fn get_ref [T] self:$List[T] n:u64 -> $T` | Borrow of the element at a zero-based index |
| [List[T]::get_mut](#listtget_mut) | `fn get_mut [T] self:mut$List[T] n:u64 -> mut$T` | Exclusive borrow of an element |
| [List[T]::slice](#listtslice) | `fn slice [T] self:$List[T] start:u64 stop:u64 -> Slice[T]` | Borrowed half-open range `[start, stop)` |
| [List[T]::as_slice](#listtas_slice) | `fn as_slice [T] self:$List[T] -> Slice[T]` | Borrowed view of the complete list |
| [List[T]::set](#listtset) | `fn set [T] self:mut$List[T] n:u64 item:T` | Replace and destroy an element |
| [List[T]::replace](#listtreplace) | `fn replace [T] self:mut$List[T] n:u64 item:T -> T` | Replace and return an element |
| [List[T]::push](#listtpush) | `fn push [T] self:mut$List[T] item:T` | Add at the end |
| [List[T]::pop](#listtpop) | `fn pop [T] self:mut$List[T] -> T` | Remove and return the last element |
| [List[T]::insert](#listtinsert) | `fn insert [T] self:mut$List[T] item:T index:u64` | Insert before `index` |
| [List[T]::remove](#listtremove) | `fn remove [T] self:mut$List[T] index:u64 -> Option[T]` | Remove and return an element if present |
| [List[T]::swap_at](#listtswap_at) | `fn swap_at [T] self:mut$List[T] i:u64 j:u64` | Exchange two elements |
| [List[T]::reverse](#listtreverse) | `fn reverse [T] self:mut$List[T]` | Reverse in place |
| [List[T]::append](#listtappend) | `fn append [T] self:mut$List[T] other:List[T]` | Move every element of `other` onto the end |
| [List[T]::clone](#listtclone) | `fn clone self:$List[T] -> List[T]` | Independent list when `T: Clone` |
| [List[T]::iter](#listtiter) | `fn iter self:$List[T] -> Iter[$T]` | Iterator over borrows of the elements |
| [List[T]::sort](#listtsort) | `fn sort self:mut$List[T]` | Sort in place when `T` implements `Ord` |
| [List[T]::sort_by](#listtsort_by) | `fn sort_by self:mut$List[T] f:fn[$T $T -> bool]` | Sort in place with a callback |
| [List[T]::sort_by_range](#listtsort_by_range) | `fn sort_by_range self:mut$List[T] low:u64 high:u64 f:$fn[$T $T -> bool]` | Sort an inclusive index range |
| [List[str]::join](#liststrjoin) | `fn join self:$List[str] separator:$str -> String` | Join a string-view list |
| [List[str]::contains](#liststrcontains) | `fn contains self:$List[str] needle:$str -> bool` | Whether a string list contains `needle` |
| [List[String]::join_strings](#liststringjoin_strings) | `fn join_strings self:$List[String] separator:$str -> String` | Join owned strings |
| [List[String]::push_str](#liststringpush_str) | `fn push_str self:mut$List[String] value:$str` | Copy and append one text view |
| [List[Bytes]::push_str](#listbytespush_str) | `fn push_str self:mut$List[Bytes] value:$str` | Copy text bytes and append one buffer |

### List[T]::new

Creates an empty list without element storage. The first `push`, `insert`, or nonempty
`append` allocates that storage.

Call: `std::List[i64]::new = numbers`.

### List[T]::from_array

Takes ownership of the array and its elements and returns a growable list.

Call: `[10, 20] std::List::from_array = numbers`.

### List[T]::length

Returns the number of elements.

### List[T]::is_empty

Returns whether the list has no elements.

### List[T]::get

Returns a shared borrow of the element at zero-based index `n`. It does not remove or
clone the element. The list keeps ownership and cannot be mutated until the returned
borrow's last use.

The index must be less than the list's length. An out-of-range index terminates the
program. This method does not return an `Option`.

Call: `index numbers.get`.

### List[T]::get_ref

Returns the same shared element borrow as [get](#listtget), with the same bounds and
ownership rules. An out-of-range index terminates the program.

Call: `index numbers.get_ref`.

### List[T]::get_mut

Returns an exclusive borrow of the indexed element. The exclusive borrow prevents other
access to the list until its last use. An out-of-range index terminates the program.

Call: `index numbers.get_mut`.

### List[T]::slice

Borrows the half-open range `[start, stop)`. It requires `start <= stop <= length`. An
invalid range terminates the program. The slice keeps the list loaned until its last
use.

Call: `stop start numbers.slice`. See [Slices](collections.md#slices).

### List[T]::as_slice

Borrows the complete list as a slice. The slice keeps the list loaned until its last
use.

Call: `numbers.as_slice`.

### List[T]::set

Borrows the list exclusively, destroys the previous element, and moves a non-`Copy` item
into its place. An out-of-range index terminates the program. Use
[replace](#listtreplace) when the previous value must be returned.

Call: `item index numbers.set`.

### List[T]::replace

Borrows the list exclusively, moves a non-`Copy` item into the indexed position, and
returns the previous element to the caller. An out-of-range index terminates the
program.

Call: `item index numbers.replace`.

### List[T]::push

Appends `item` and produces no output. The list is borrowed exclusively and remains
owned by the caller. A non-`Copy` item moves into the list. No element borrow may remain
in use across the call.

Call: `item numbers.push`.

### List[T]::pop

Borrows the list exclusively, removes its final element, and returns that element by
value. The caller receives ownership when `T` is an owning type. The list binding
remains available afterward.

The list must be nonempty. Popping an empty list terminates the program.

Call: `numbers.pop`.

### List[T]::insert

Inserts before `index`. Insertion at the list's length appends. A greater index
terminates the program. The list is borrowed exclusively, and a non-`Copy` item moves
into it. The item is consumed before the index, unlike [set](#listtset).

Call: `index item numbers.insert`.

### List[T]::remove

Borrows the list exclusively, removes the indexed element, and shifts later elements
toward the start. It returns `Option::Some` containing the removed value, transferring
its ownership to the caller. An invalid index returns `Option::None` without changing
the list.

Call: `index numbers.remove`.

### List[T]::swap_at

Exchanges two elements through an exclusive list borrow. When the two indices differ,
either index outside the list terminates the program. Equal indices leave the list
unchanged.

Call: `second first numbers.swap_at`.

### List[T]::reverse

Reverses elements in place through an exclusive list borrow.

Call: `numbers.reverse`.

### List[T]::append

Borrows the receiver exclusively and consumes the other list, moving all its elements
onto the receiver's end.

Call: `other numbers.append`.

### List[T]::clone

Returns an independent list when `T` implements `Clone`. Cloning each element can
allocate or run user code. The source list remains available.

Call: `numbers.clone`.

### List[T]::iter

Returns an iterator over shared element borrows. The list keeps ownership and remains
loaned while those borrows are in use.

Call: `numbers.iter`.

### List[T]::sort

Sorts elements in place in ascending order when `T` implements `Ord`.

Call: `numbers.sort`. See [the sorting example](../examples/sorting.casa).

### List[T]::sort_by

Sorts elements in place with a comparison callback. The list is borrowed exclusively.

Call: `compare numbers.sort_by`. See [the sorting example](../examples/sorting.casa).

### List[T]::sort_by_range

Sorts the inclusive index range with a borrowed comparison callback. The list is
borrowed exclusively.

Call: `compare high low numbers.sort_by_range`.

### List[str]::join

Joins a string-view list.

### List[str]::contains

Returns whether a string list contains `needle`.

### List[String]::join_strings

Joins owned strings.

### List[String]::push_str

Copies and appends one text view.

### List[Bytes]::push_str

Copies text bytes and appends one buffer.

 ## Element ownership

`get`, `get_ref`, and `iter` keep the list as the element owner. Use `copy` for a `Copy`
element or `.clone` for a `Clone` element when an independent value is needed. `set`
destroys the replaced element. `replace`, `remove`, and `pop` transfer the removed value
to the caller. See [Ownership and Borrows](ownership.md).