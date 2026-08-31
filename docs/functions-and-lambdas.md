# Functions and Lambdas

Casa functions consume stack values and can push results. Functions are
declared at global scope and called by name.

## Declare and call a function

```casa
fn subtract left:i64 right:i64 -> i64 {
    left right -
}

3 12 subtract print    # 9
```

Parameters are listed in consumption order. The first parameter receives the
topmost value, so `left` receives `12` and `right` receives `3` in this
call.

Use unnamed inputs when a local name adds no clarity:

```casa
fn square i64 -> i64 { dup * }

6 square print    # 36
```

Omit `->` when a function pushes no result:

```casa
fn greet name:str {
    f"Hello, {name}!\n" print
}
```

A function can push multiple results by listing each output type after `->`.
Calls can appear before the declaration. Use `return` to leave a function
early. Every exit path must produce the declared outputs.

See [Generics and Traits](traits.md) for type parameters and trait bounds.

## Root and termination contracts

Operations outside declarations form the executable root body. Casa does not
reserve or automatically call a function named `main`. The root must leave an
empty value stack when it completes. `return` is not valid in the root body.
Root owners are destroyed on normal completion.

`panic` and `process::exit` terminate without unwinding or cleanup. A direct
call to a named function is also non-returning when every reachable path in its
implementation terminates. These paths do not participate in branch stack or
ownership joins. Function values called with `exec` are assumed to return, so
`fn[...]` types do not expose a termination effect.

```casa
fn require_positive value:i64 -> i64 {
    if 0 value > then
        value
    else
        "value must be positive" panic
    fi
}

5 require_positive print
```

## Function values

`&name` pushes a named function as a value. `exec` calls the function value on
top of the stack:

```casa
fn increment value:i64 -> i64 { value 1 + }

&increment = operation
41 operation exec print    # 42
```

Its type records the stack effect. For example, `&increment` has type
`fn[i64 -> i64]`. A function can accept that type as a parameter:

```casa
fn apply operation:fn[i64 -> i64] value:i64 -> i64 {
    value operation exec
}

40 { 2 + } apply print    # 42
```

The function value must be on top when `exec` runs. Its arguments stay below
it.

A generic function reference must name its concrete arguments:

```casa
fn identity[T] value:T -> T { value }

&identity[i64] = integer_identity
```

## Unsafe boundaries

Raw memory operations, pointer arithmetic and conversion, and Linux system
calls must be inside an `unsafe` block:

```casa
fn read_byte address:ptr -> u8 {
    unsafe { address load8 }
}
```

Raw storage uses `u64 alloc -> ptr` and `ptr free -> None`. Allocating zero
bytes returns the null pointer. Freeing null does nothing. A positive allocation
returns aligned storage or terminates the process if storage is exhausted.
`free` releases only a complete live allocation returned by `alloc`. Double
free, use after free, and freeing foreign or interior pointers are undefined
behavior.

Pointer `+` and `-` use `u64` byte offsets and do not scale by an element type.
Raw loads and stores use the matching unsigned integer width:

| Operation | Stack effect |
|---|---|
| `load8` | `ptr -> u8` |
| `load16` | `ptr -> u16` |
| `load32` | `ptr -> u32` |
| `load64` | `ptr -> u64` |
| `store8` | `ptr u8 -> None` |
| `store16` | `ptr u16 -> None` |
| `store32` | `ptr u32 -> None` |
| `store64` | `ptr u64 -> None` |

The x86-64 target permits unaligned access. Multibyte operations use
little-endian byte order. Every accessed byte must still belong to valid live
storage.

The block permits only designated unsafe operations. Type, ownership, borrow,
control-flow, and stack-effect checks still apply.

### Safety comments

Every `unsafe` block and `unsafe fn` in maintained non-test source must have a
`# SAFETY:` comment immediately before it. The comment for a block states the
concrete invariants that make every unchecked operation in the block valid. Do
not only describe the operation. State the facts that prove its safety, such as
live allocation bounds, initialization, alignment, ownership, aliasing, a
preceding check, or a syscall or FFI contract.

Keep the proof brief and information-dense. One comment can cover a complete
block only when it proves every unsafe operation in that block. Split a block
when separate proofs would otherwise be vague or long.

Use `unsafe fn` when callers must uphold a contract that the function cannot
check. Calls to an unsafe function also require an `unsafe` block. An unsafe
function body does not become an implicit unsafe block:

```casa
# SAFETY: the caller provides `count` readable source bytes and writable,
# non-overlapping destination bytes.
unsafe fn copy_bytes destination:ptr source:ptr count:u64 {
    # SAFETY: the caller contract proves both byte accesses are valid.
    unsafe { count source destination memcpy }
}
```

The comment before an `unsafe fn` states the caller contract. Comments inside
its body prove the implementation's individual unsafe operations. A safe
function must establish its own proof and cannot rely on its caller to do so.

Unsafe functions cannot be used as function values. Put the unsafe call in a
safe wrapper when the wrapper can validate and preserve a safe contract.

## Extern functions

A bodyless `extern fn` declaration names a function provided by a native
library. Casa calls it with the x86-64 System V C ABI:

```casa
extern fn strlen text:$cstr -> u64

"Casa".as_cstr.unwrap = text
# SAFETY: `text` is a live NUL-terminated string for the duration of `strlen`.
unsafe { text strlen } print
```

An `extern struct` declaration gives an aggregate the C field layout for this
ABI:

```casa
extern struct Point derives Copy {
    x: f32
    y: f32
}

extern struct Shape {
    point:  Point
    colors: array[u8 4]
    active: bool
}

extern fn move_shape shape:mut$Shape

extern fn offset_point point:Point offset:Point -> Point
```

Extern structs are non-generic and contain at least one field. A field can be a
fixed-width integer, `f32`, `f64`, `bool`, `ptr`, another extern struct, or a
non-empty fixed array composed from these types. Field order, alignment,
padding, array stride, and tail padding match C. Ordinary structs and enums keep
their compiler-owned layout and are not C ABI types.

Extern structs use normal construction, field access, field assignment,
visibility, imports, and methods. An extern function can take `$T` or `mut$T`
when `T` is an extern struct. The borrow lowers to one native pointer and keeps
the normal Casa lifetime and exclusivity rules.

An extern struct that implements `Copy` can be passed by value when its C layout
fits in one or two System V eightbytes. The call copies its representation, so
Casa keeps the original value. The supported register classes are `INTEGER`,
`SSE`, and mixtures of both. If the complete aggregate cannot use the remaining
argument registers, it moves to one aligned native stack argument. A small extern
struct return becomes an owned Casa value. Larger memory-class aggregates are
not supported yet.

An extern call is always unsafe. The caller must meet the native function's
contract. An extern function cannot be used as a function value. A safe Casa
wrapper can check the inputs and contain the unsafe call.

Extern parameters can use these C ABI types:

- Fixed-width integers: `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, and
  `u64`.
- Floating-point values: `f32` and `f64`.
- C boolean values: `bool`, represented as C `_Bool`.
- Raw pointers: `ptr`.
- Shared or exclusive borrows of an ABI scalar, such as `$i32` or `mut$f64`.
- Shared or exclusive borrows of an extern struct.
- A `Copy` extern struct that fits in one or two System V eightbytes.
- `$cstr` for a borrowed NUL-terminated byte string.

An extern declaration has zero or one return value. A return can be a
fixed-width integer, `f32`, `f64`, `bool`, `ptr`, or an extern struct that fits
in one or two System V eightbytes. Casa sends `bool` parameters as 0 or 1 and
normalizes each C `_Bool` return before use. Borrowed returns, `char`, `str`,
owned `cstr`, memory-class aggregates, variadic arguments, callbacks, generic
type parameters, and symbol aliases are not supported.

Casa keeps ownership of every argument. A by-value extern struct copies its C
representation. A borrowed scalar, extern struct, or `$cstr` is passed as a
native pointer. The pointer is valid only while the Casa borrow is live. The
native function must not retain it after the call unless a separate API
contract keeps the storage alive.

Use `-l` / `--link-library` to link each required native library. The option is
repeatable and preserves command-line order:

```sh
casac -L lib -l c program.casa
```

`-L` / `--library-path` remains the Casa module search path. Casa does not use
`-I` and does not provide a native library search-path option.

## Bindings

`= name` pops the top value and binds it. The first assignment fixes the
binding's type:

```casa
42 = count
1 += count
count print
```

A binding inside a function is local. A top-level binding belongs to the root
entry-point scope. Named functions cannot capture root locals. Pass mutable
state as an exclusive parameter:

```casa
struct Counter {
    value: i64
}

fn increment_counter counter:mut$Counter { 1 += counter.value }

0 Counter = counter
counter increment_counter
```

Lexical closures in the root body can capture root locals.

`global` is valid only at the top level and always requires an initializer. It
does not declare access inside a function.

Declare an immutable runtime global at the top level with an initializer:

```casa
global LIMIT 100

global OPERATORS {
    build_operators
}

pub global PUBLIC_LIMIT 200
```

A direct initializer is one operation. Use a block for a multi-operation
initializer. The initializer must produce one value. Dependency modules
initialize before their importers, and globals in one module initialize in
source order. A global cannot read a later global during initialization.

An immutable global is private unless it has `pub`. A `Copy` value is copied
when an owned value is required. A non-`Copy` value is borrowed and cannot move
out of its global place. Local and pattern bindings cannot shadow a visible
immutable global.

Use `= name:Type` when the value needs an explicit type:

```casa
Option::None = result:Option[i64]
```

See [Operators](operators.md#assignment) for assignment forms.

## Ownership and borrows

A plain `T` parameter consumes an owned value. Non-Copy owners can be consumed
only once:

```casa
fn consume text:String { text drop }

"one owner".to_str = text
text consume
# text consume    # Error: text was already moved.
```

Use `$T` for shared access and `mut$T` for exclusive mutable access. Calls
borrow an available owner automatically:

```casa
fn length text:$str -> u64 { text.length }
fn clear text:mut$String { text.clear }

"Casa".to_str = text
text.as_str length print
text clear
text.as_str length print
```

Shared borrows can be duplicated with `dup` and `over`, but they do not satisfy
`Copy` bounds. Exclusive borrows and non-Copy owners are affine. For owned
values, `dup` and the copied value of `over` require `Copy`. `swap` and `rot`
only move values, so they also work with non-Copy owners.

An owner or exclusive borrow can be reborrowed for a call. When the call does
not return a borrow, the reborrow ends when the call returns. One call cannot
borrow the same binding exclusively more than once or combine shared and
exclusive borrows of that binding:

```casa
fn replace_both left:mut$Person right:mut$Person { }

# person person replace_both  # Error: the exclusive arguments alias.
```

A returned borrow keeps each compatible borrowed input loaned until its last
use. The caller cannot know which input supplied an opaque result:

```casa
fn select first:$Person second:$Person choose_first:bool -> $Person {
    if choose_first then first else second fi
}

true other person select = selected
# person drop       # Error: selected can borrow person.
selected.description print
person drop
other drop
```

A function cannot return a borrow of a local owner. The diagnostic identifies
the local owner that would escape.

A function can return multiple exclusive field borrows when their named paths
do not overlap:

```casa
fn split pair:mut$Pair -> mut$Item mut$Item {
    pair.left
    pair.right
}
```

The compiler rejects duplicate or nested overlapping outputs. After a call,
the returned borrows keep the complete borrowed input loaned because field
paths are not part of a public function type.

## Lambdas and closures

Braces create an anonymous function value:

```casa
{ 1 + } = increment
41 increment exec print    # 42
```

The compiler infers a lambda's stack effect from its body and the context in
which it is used. Here, `increment` has type `fn[i64 -> i64]`.

When a call has an expected function type, the compiler uses its resolved input
types for unannotated lambda parameters. The body determines the output types:

```casa
[1, 2, 3] List::from_array = values
0 = initial_total:i64
{ = total copy total + } initial_total values.iter.fold print
```

An explicit parameter annotation takes priority and must match the expected
function type. A lambda that is bound before use has no expected function type,
so its body alone determines its stack effect.

A lambda can capture bindings from its enclosing scope. Copy values are copied.
An ordinary lambda borrows a non-Copy owner for the lifetime of the closure:

```casa
[1, 2, 3] List::from_array = values
{ values.length } = count_values
count_values exec print    # 3
```

Use `move` when the closure must own its captures and outlive their original
scope:

```casa
fn counter values:List[i64] -> fn[-> u64] {
    move { values.length }
}
```

Both forms create repeatable closures. A moving closure can inspect or mutate
an owned capture. If one invocation moves or destroys that capture, every
continuing path must restore it before returning. Destroying the closure
destroys its owned captures once.

Use a lambda for a short callback. Use a named function when the operation is
shared or needs its own documentation.
