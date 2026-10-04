# List

`std::List[T]` [owns](ownership.md#move-a-value) a growable sequence of `T` values. Use it when elements must be
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
| [append](#listtappend) | `fn append [T] self:mut$List[T] other:List[T]` | Move every element of `other` onto the end |
| [as_slice](#listtas_slice) | `fn as_slice [T] self:$List[T] -> Slice[T]` | Borrowed view of the complete list |
| [clone](#listtclone) | `fn clone self:$List[T] -> List[T]` | Independent list when `T: Clone` |
| [contains](#liststrcontains) | `fn contains self:$List[str] needle:$str -> bool` | Whether a string list contains `needle` |
| [from_array](#listtfrom_array) | `fn from_array[T const N:u64] array:array[T N] -> List[T]` | List containing the array values |
| [get](#listtget) | `fn get [T] self:$List[T] n:u64 -> $T` | Borrow of the element at a zero-based index |
| [get_mut](#listtget_mut) | `fn get_mut [T] self:mut$List[T] n:u64 -> mut$T` | Exclusive borrow of an element |
| [get_ref](#listtget_ref) | `fn get_ref [T] self:$List[T] n:u64 -> $T` | Borrow of the element at a zero-based index |
| [insert](#listtinsert) | `fn insert [T] self:mut$List[T] item:T index:u64` | Insert before `index` |
| [is_empty](#listtis_empty) | `fn is_empty self:$List -> bool` | Whether the list has no elements |
| [iter](#listtiter) | `fn iter self:$List[T] -> Iter[$T]` | Iterator over borrows of the elements |
| [join](#liststrjoin) | `fn join self:$List[str] separator:$str -> String` | Join a string-view list |
| [join_strings](#liststringjoin_strings) | `fn join_strings self:$List[String] separator:$str -> String` | Join owned strings |
| [length](#listtlength) | `fn length self:$List -> u64` | Number of elements |
| [new](#listtnew) | `fn new [T] -> List[T]` | Empty list |
| [pop](#listtpop) | `fn pop [T] self:mut$List[T] -> T` | Remove and return the last element |
| [push](#listtpush) | `fn push [T] self:mut$List[T] item:T` | Add at the end |
| [push_str](#listbytespush_str) | `fn push_str self:mut$List[Bytes] value:$str` | Copy text bytes and append one buffer |
| [push_str](#liststringpush_str) | `fn push_str self:mut$List[String] value:$str` | Copy and append one text view |
| [remove](#listtremove) | `fn remove [T] self:mut$List[T] index:u64 -> Option[T]` | Remove and return an element if present |
| [replace](#listtreplace) | `fn replace [T] self:mut$List[T] n:u64 item:T -> T` | Replace and return an element |
| [reverse](#listtreverse) | `fn reverse [T] self:mut$List[T]` | Reverse in place |
| [set](#listtset) | `fn set [T] self:mut$List[T] n:u64 item:T` | Replace and destroy an element |
| [slice](#listtslice) | `fn slice [T] self:$List[T] start:u64 stop:u64 -> Slice[T]` | Borrowed half-open range `[start, stop)` |
| [sort](#listtsort) | `fn sort self:mut$List[T]` | Sort in place when `T` implements `Ord` |
| [sort_by](#listtsort_by) | `fn sort_by self:mut$List[T] f:fn[$T $T -> bool]` | Sort in place with a callback |
| [sort_by_range](#listtsort_by_range) | `fn sort_by_range self:mut$List[T] low:u64 high:u64 f:$fn[$T $T -> bool]` | Sort an inclusive index range |
| [swap_at](#listtswap_at) | `fn swap_at [T] self:mut$List[T] i:u64 j:u64` | Exchange two elements |

### List[T]::append

```text
fn append [T] self:mut$List[T] other:List[T]
```

Borrows the receiver exclusively and consumes the other list, moving all its elements
onto the receiver's end.

Call: `other numbers.append`.

### List[T]::as_slice

```text
fn as_slice [T] self:$List[T] -> Slice[T]
```

Borrows the complete list as a slice. The slice keeps the list loaned until its last
use.

Call: `numbers.as_slice`.

### List[T]::clone

```text
fn clone self:$List[T] -> List[T]
```

Returns an independent list when `T` implements [`Clone`](traits.md#built-in-traits). Cloning each element can
allocate or run user code. The source list remains available.

Call: `numbers.clone`.

### List[str]::contains

```text
fn contains self:$List[str] needle:$str -> bool
```

Returns whether a string list contains `needle`.

### List[T]::from_array

```text
fn from_array[T const N:u64] array:array[T N] -> List[T]
```

Takes ownership of the array and its elements and returns a growable list.

Call: `[10, 20] std::List::from_array = numbers`.

### List[T]::get

```text
fn get [T] self:$List[T] n:u64 -> $T
```

Returns a [shared borrow](ownership.md#borrow-for-a-call) of the element at zero-based index `n`. It does not remove or
clone the element. The list keeps ownership and cannot be mutated until the returned
borrow's last use.

The index must be less than the list's length. An out-of-range index terminates the
program. This method does not return an `Option`.

Call: `index numbers.get`.

### List[T]::get_mut

```text
fn get_mut [T] self:mut$List[T] n:u64 -> mut$T
```

Returns an [exclusive borrow](ownership.md#borrow-for-a-call) of the indexed element. The exclusive borrow prevents other
access to the list until its last use. An out-of-range index terminates the program.

Call: `index numbers.get_mut`.

### List[T]::get_ref

```text
fn get_ref [T] self:$List[T] n:u64 -> $T
```

Returns the same shared element borrow as [get](#listtget), with the same bounds and
ownership rules. An out-of-range index terminates the program.

Call: `index numbers.get_ref`.

### List[T]::insert

```text
fn insert [T] self:mut$List[T] item:T index:u64
```

Inserts before `index`. Insertion at the list's length appends. A greater index
terminates the program. The list is borrowed exclusively, and a non-`Copy` item moves
into it. The item is consumed before the index, unlike [set](#listtset).

Call: `index item numbers.insert`.

### List[T]::is_empty

```text
fn is_empty self:$List -> bool
```

Returns whether the list has no elements.

### List[T]::iter

```text
fn iter self:$List[T] -> Iter[$T]
```

Returns an iterator over shared element borrows. The list keeps ownership and remains
loaned while those borrows are in use.

Call: `numbers.iter`.

### List[str]::join

```text
fn join self:$List[str] separator:$str -> String
```

Joins a string-view list.

### List[String]::join_strings

```text
fn join_strings self:$List[String] separator:$str -> String
```

Joins owned strings.

### List[T]::length

```text
fn length self:$List -> u64
```

Returns the number of elements.

### List[T]::new

```text
fn new [T] -> List[T]
```

Creates an empty list without element storage. The first `push`, `insert`, or nonempty
`append` allocates that storage.

Call: `std::List[i64]::new = numbers`.

### List[T]::pop

```text
fn pop [T] self:mut$List[T] -> T
```

Borrows the list exclusively, removes its final element, and returns that element by
value. The caller receives ownership when `T` is an owning type. The list binding
remains available afterward.

The list must be nonempty. Popping an empty list terminates the program.

Call: `numbers.pop`.

### List[T]::push

```text
fn push [T] self:mut$List[T] item:T
```

Appends `item` and produces no output. The list is borrowed exclusively and remains
owned by the caller. A non-`Copy` item moves into the list. No element borrow may remain
in use across the call.

Call: `item numbers.push`.

### List[Bytes]::push_str

```text
fn push_str self:mut$List[Bytes] value:$str
```

Copies text bytes and appends one buffer.

### List[String]::push_str

```text
fn push_str self:mut$List[String] value:$str
```

Copies and appends one text view.

### List[T]::remove

```text
fn remove [T] self:mut$List[T] index:u64 -> Option[T]
```

Borrows the list exclusively, removes the indexed element, and shifts later elements
toward the start. It returns `Option::Some` containing the removed value, transferring
its ownership to the caller. An invalid index returns `Option::None` without changing
the list.

Call: `index numbers.remove`.

### List[T]::replace

```text
fn replace [T] self:mut$List[T] n:u64 item:T -> T
```

Borrows the list exclusively, moves a non-`Copy` item into the indexed position, and
returns the previous element to the caller. An out-of-range index terminates the
program.

Call: `item index numbers.replace`.

### List[T]::reverse

```text
fn reverse [T] self:mut$List[T]
```

Reverses elements in place through an exclusive list borrow.

Call: `numbers.reverse`.

### List[T]::set

```text
fn set [T] self:mut$List[T] n:u64 item:T
```

Borrows the list exclusively, destroys the previous element, and moves a non-`Copy` item
into its place. An out-of-range index terminates the program. Use
[replace](#listtreplace) when the previous value must be returned.

Call: `item index numbers.set`.

### List[T]::slice

```text
fn slice [T] self:$List[T] start:u64 stop:u64 -> Slice[T]
```

Borrows the half-open range `[start, stop)`. It requires `start <= stop <= length`. An
invalid range terminates the program. The slice keeps the list loaned until its last
use.

Call: `stop start numbers.slice`. See [Slices](collections.md#slices).

### List[T]::sort

```text
fn sort self:mut$List[T]
```

Sorts elements in place in ascending order when `T` implements [`Ord`](traits.md#built-in-traits).

Call: `numbers.sort`. See [the sorting example](../examples/sorting.casa).

### List[T]::sort_by

```text
fn sort_by self:mut$List[T] f:fn[$T $T -> bool]
```

Sorts elements in place with a [comparison callback](functions-and-lambdas.md#function-values). The list is borrowed exclusively.

Call: `compare numbers.sort_by`. See [the sorting example](../examples/sorting.casa).

### List[T]::sort_by_range

```text
fn sort_by_range self:mut$List[T] low:u64 high:u64 f:$fn[$T $T -> bool]
```

Sorts the inclusive index range with a borrowed comparison callback. The list is
borrowed exclusively.

Call: `compare high low numbers.sort_by_range`.

### List[T]::swap_at

```text
fn swap_at [T] self:mut$List[T] i:u64 j:u64
```

Exchanges two elements through an exclusive list borrow. When the two indices differ,
either index outside the list terminates the program. Equal indices leave the list
unchanged.

Call: `second first numbers.swap_at`.

 ## Element ownership

`get`, `get_ref`, and `iter` keep the list as the element owner. Use `copy` for a `Copy`
element or `.clone` for a `Clone` element when an independent value is needed. `set`
destroys the replaced element. `replace`, `remove`, and `pop` transfer the removed value
to the caller. See [Ownership and Borrows](ownership.md).