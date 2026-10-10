# List

`std::List[T]` [owns](ownership.md#move-a-value) a growable sequence of `T` values. Use it when elements must be
added or removed. Import `std` and qualify its constructors.

Storage fields are private. Use `new` or `from_array` to construct a list and
`length` to inspect its element count. Element access and mutation go through
checked methods such as `get`, `get_mut`, `push`, and `remove`.

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

| Method | Signature | Description |
|---|---|---|
| [append](#listtappend) | `fn append [T] self:mut$List[T] other:List[T]` | Move every element of `other` onto the end |
| [as_ptr](#listtas_ptr) | `fn as_ptr self:$List -> ptr` | Non-owning pointer to element storage |
| [as_slice](#listtas_slice) | `fn as_slice [T] self:$List[T] -> Slice[T]` | Borrowed view of the complete list |
| [clone](#listtclone) | `fn clone self:$List[T] -> List[T]` | Independent list when `T: Clone` |
| [contains](#liststrcontains) | `fn contains self:$List[str] needle:$str -> bool` | Whether a string list contains `needle` |
| [from_array](#listtfrom_array) | `fn from_array[T const N:u64] array:array[T N] -> List[T]` | List containing the array values |
| [get](#listtget) | `fn get [T] self:$List[T] n:u64 -> $T` | Borrow of the element at a zero-based index |
| [get_mut](#listtget_mut) | `fn get_mut [T] self:mut$List[T] n:u64 -> mut$T` | Exclusive borrow of an element |
| [get_ref](#listtget_ref) | `fn get_ref [T] self:$List[T] n:u64 -> $T` | Borrow of the element at a zero-based index |
| [insert](#listtinsert) | `fn insert [T] self:mut$List[T] item:T index:u64` | Insert before `index` |
| [into_iter](#listtinto_iter) | `fn into_iter self:List[T] -> Iter[T]` | Transfer elements in index order |
| [is_empty](#listtis_empty) | `fn is_empty self:$List -> bool` | Whether the list has no elements |
| [iter](#listtiter) | `fn iter self:$List[T] -> Iter[$T]` | Iterator over borrows of the elements |
| [iter_mut](#listtiter_mut) | `fn iter_mut self:mut$List[T] -> ListIterMut[T]` | Lend one mutable element at a time |
| [join](#liststrjoin) | `fn join self:$List[str] separator:$str -> String` | Join a string-view list |
| [join_strings](#liststringjoin_strings) | `fn join_strings self:$List[String] separator:$str -> String` | Join owned strings |
| [length](#listtlength) | `fn length self:$List -> u64` | Number of elements |
| [new](#listtnew) | `fn new [T] -> List[T]` | Empty list |
| [pop](#listtpop) | `fn pop [T] self:mut$List[T] -> T` | Remove and return the last element |
| [push](#listtpush) | `fn push [T] self:mut$List[T] item:T` | Add at the end |
| [push_str](#listbytespush_str) | `fn push_str self:mut$List[Bytes] value:$str` | Copy text into an owned byte buffer and append it |
| [push_str](#liststringpush_str) | `fn push_str self:mut$List[String] value:$str` | Copy a text view into an owned string and append it |
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

`append` borrows the receiver exclusively and consumes the other list, moving
all its elements onto the receiver's end.

Call `other numbers.append`.

### List[T]::as_ptr

```text
fn as_ptr self:$List -> ptr
```

`as_ptr` returns a non-owning pointer for low-level interoperation. An empty
list can return null. Storage growth or destruction invalidates the pointer.
Dereferencing it requires `unsafe`. The caller must preserve element
initialization, ownership, and borrow exclusivity. Prefer `get` or `get_mut` for
checked access.

### List[T]::as_slice

```text
fn as_slice [T] self:$List[T] -> Slice[T]
```

`as_slice` borrows the complete list as a slice. The slice keeps the list loaned
until its last use.

Call `numbers.as_slice`.

### List[T]::clone

```text
fn clone self:$List[T] -> List[T]
```

`clone` returns an independent list when `T` implements `Clone`. Cloning each
element can allocate or run user code. The source list remains available.

Call `numbers.clone`.

### List[str]::contains

```text
fn contains self:$List[str] needle:$str -> bool
```

`contains` returns whether a string list contains `needle`.

### List[T]::from_array

```text
fn from_array[T const N:u64] array:array[T N] -> List[T]
```

`from_array` takes ownership of the array and its elements and returns a
growable list.

Call `[10, 20] std::List::from_array = numbers`.

### List[T]::get

```text
fn get [T] self:$List[T] n:u64 -> $T
```

`get` returns a [shared borrow](ownership.md#borrow-for-a-call) of the element
at zero-based index `n`. It does not remove or clone the element. The list keeps
ownership and cannot be mutated until the returned borrow's last use.

The index must be less than the list's length. An out-of-range index terminates the
program. This method does not return an `Option`.

Call `index numbers.get`.

### List[T]::get_mut

```text
fn get_mut [T] self:mut$List[T] n:u64 -> mut$T
```

`get_mut` returns an [exclusive borrow](ownership.md#borrow-for-a-call) of the
indexed element. The exclusive borrow prevents other access to the list until
its last use. An out-of-range index terminates the program.

Call `index numbers.get_mut`.

### List[T]::get_ref

```text
fn get_ref [T] self:$List[T] n:u64 -> $T
```

`get_ref` returns the same shared element borrow as [get](#listtget), with the
same bounds and ownership rules. An out-of-range index terminates the program.

Call `index numbers.get_ref`.

### List[T]::insert

```text
fn insert [T] self:mut$List[T] item:T index:u64
```

`insert` inserts before `index`. Insertion at the list's length appends. A
greater index terminates the program. The list is borrowed exclusively, and a
non-`Copy` item moves into it. The item is consumed before the index, unlike
[set](#listtset).

Call `index item numbers.insert`.

### List[T]::into_iter

```text
fn into_iter self:List[T] -> Iter[T]
```

`into_iter` consumes the list and yields each element exactly once in original
index order. The iterator owns the unconsumed elements. Dropping it destroys
those elements. A yielded owner can outlive the iterator. Neither `Copy` nor
`Clone` is required. Empty and exhausted iterators keep returning `None`.

Construction reverses the owned list in linear time. Each `next` removes the last
element in constant time. This reuses the list's element allocation and creates
an ordinary closure. The returned `Iter[T]` supports all existing iterator
operations. Terminals that borrow a named iterator leave unvisited elements
available for subsequent calls.

Call `numbers.into_iter`.

### List[T]::is_empty

```text
fn is_empty self:$List -> bool
```

`is_empty` returns whether the list has no elements.

### List[T]::iter

```text
fn iter self:$List[T] -> Iter[$T]
```

`iter` returns an iterator over shared element borrows. The list keeps ownership
and remains loaned while those borrows are in use.

Call `numbers.iter`.

### List[T]::iter_mut

```text
fn iter_mut self:mut$List[T] -> ListIterMut[T]
```

`iter_mut` returns a mutable cursor in index order. The list remains the element
owner and is borrowed exclusively while the cursor or a yielded borrow is live.
The cursor neither removes elements nor reallocates storage. It requires neither
`Copy` nor `Clone`.

`next` lends one element. Finish using that borrow before advancing, moving, or
dropping the cursor. Empty and exhausted cursors keep returning `None`. A `for`
loop follows the same rule: each element borrow must end before the next
iteration.

| Method | Signature | Behavior |
|---|---|---|
| `all` | `fn all self:mut$ListIterMut[T] predicate:fn[$T -> bool] -> bool` | Stop at the first false result, or return true on exhaustion |
| `any` | `fn any self:mut$ListIterMut[T] predicate:fn[$T -> bool] -> bool` | Stop at the first true result, or return false on exhaustion |
| `count` | `fn count self:mut$ListIterMut[T] -> u64` | Return the remaining count and exhaust the cursor |
| `find` | `fn find self:mut$ListIterMut[T] predicate:fn[$T -> bool] -> Option[mut$T]` | Lend the first match, or return `None` on exhaustion |
| `next` | `fn next self:mut$ListIterMut[T] -> Option[mut$T]` | Lend the next element |

Predicates receive shared element borrows. `all`, `any`, and `find` leave the
cursor positioned after the last element examined. A live `find` result keeps
the cursor exclusively borrowed. `count` uses constant time and does not destroy
elements.

`ListIterMut[T]` has its own methods and supports `for`. It does not implement
`Iterable[mut$T]`, whose defaults can retain earlier yields while advancing.

Call `numbers.iter_mut`. See [the list iteration example](../examples/list_iteration.casa).

### List[str]::join

```text
fn join self:$List[str] separator:$str -> String
```

`join` joins a string-view list.

### List[String]::join_strings

```text
fn join_strings self:$List[String] separator:$str -> String
```

`join_strings` joins owned strings.

### List[T]::length

```text
fn length self:$List -> u64
```

`length` returns the number of elements.

### List[T]::new

```text
fn new [T] -> List[T]
```

`new` creates an empty list without element storage. The first `push`, `insert`,
or nonempty `append` allocates that storage.

Call `std::List[i64]::new = numbers`.

### List[T]::pop

```text
fn pop [T] self:mut$List[T] -> T
```

`pop` borrows the list exclusively, removes its final element, and returns that
element by value. The caller receives ownership when `T` is an owning type. The
list binding remains available afterward.

The list must be nonempty. Popping an empty list terminates the program.

Call `numbers.pop`.

### List[T]::push

```text
fn push [T] self:mut$List[T] item:T
```

`push` appends `item` and produces no output. The list is borrowed exclusively
and remains owned by the caller. A non-`Copy` item moves into the list. No
element borrow may remain in use across the call.

Call `item numbers.push`.

### List[Bytes]::push_str

```text
fn push_str self:mut$List[Bytes] value:$str
```

`push_str` copies the text bytes into an owned `Bytes` buffer and appends it.

### List[String]::push_str

```text
fn push_str self:mut$List[String] value:$str
```

`push_str` copies the text view into an owned `String` and appends it.

### List[T]::remove

```text
fn remove [T] self:mut$List[T] index:u64 -> Option[T]
```

`remove` borrows the list exclusively, removes the indexed element, and shifts
later elements toward the start. It returns `Option::Some` containing the
removed value, transferring its ownership to the caller. An invalid index
returns `Option::None` without changing the list.

Call `index numbers.remove`.

### List[T]::replace

```text
fn replace [T] self:mut$List[T] n:u64 item:T -> T
```

`replace` borrows the list exclusively, moves a non-`Copy` item into the indexed
position, and returns the previous element to the caller. An out-of-range index
terminates the program.

Call `item index numbers.replace`.

### List[T]::reverse

```text
fn reverse [T] self:mut$List[T]
```

`reverse` reverses elements in place through an exclusive list borrow.

Call `numbers.reverse`.

### List[T]::set

```text
fn set [T] self:mut$List[T] n:u64 item:T
```

`set` borrows the list exclusively, destroys the previous element, and moves a
non-`Copy` item into its place. An out-of-range index terminates the program.
Use [replace](#listtreplace) when the previous value must be returned.

Call `item index numbers.set`.

### List[T]::slice

```text
fn slice [T] self:$List[T] start:u64 stop:u64 -> Slice[T]
```

`slice` borrows the half-open range `[start, stop)`. It requires
`start <= stop <= length`. An invalid range terminates the program. The slice
keeps the list loaned until its last use.

Call `stop start numbers.slice`. See [Slices](collections.md#slices).

### List[T]::sort

```text
fn sort self:mut$List[T]
```

`sort` sorts elements in place in ascending order when `T` implements `Ord`.

Call `numbers.sort`. See [the sorting example](../examples/sorting.casa).

### List[T]::sort_by

```text
fn sort_by self:mut$List[T] f:fn[$T $T -> bool]
```

`sort_by` sorts elements in place through an exclusive list borrow. The
[comparison callback](functions-and-lambdas.md#function-values) must return
true when its first parameter precedes its second. A named `&T::lt` gives
ascending order. For an unnamed stack lambda, `{ > }` gives ascending order
and `{ < }` gives descending order because symbolic operators read source
order while callback parameters use consumption order. Sorting is not stable.

Call `compare numbers.sort_by`. See [the sorting example](../examples/sorting.casa).

### List[T]::sort_by_range

```text
fn sort_by_range self:mut$List[T] low:u64 high:u64 f:$fn[$T $T -> bool]
```

`sort_by_range` sorts the inclusive index range with a borrowed callback using
the [sort_by](#listtsort_by) contract. If `low >= high`, the method leaves the
list unchanged. Otherwise, both indices must be in range or the program
terminates.

Call `compare high low numbers.sort_by_range`.

### List[T]::swap_at

```text
fn swap_at [T] self:mut$List[T] i:u64 j:u64
```

`swap_at` exchanges two elements through an exclusive list borrow. When the two
indices differ, either index outside the list terminates the program. Equal
indices leave the list unchanged.

Call `second first numbers.swap_at`.

## Element ownership

`get`, `get_ref`, `get_mut`, `iter`, and `iter_mut` keep the list as the element owner. Use `copy` for a `Copy`
element or `.clone` for a `Clone` element when an independent value is needed. `set`
destroys the replaced element. `replace`, `remove`, and `pop` transfer the removed value
to the caller. `into_iter` transfers each yielded element to its caller and owns
the remainder. See [Ownership and Borrows](ownership.md).
