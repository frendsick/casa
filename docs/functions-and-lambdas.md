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

## Unsafe boundaries

Raw memory operations, pointer arithmetic and conversion, and Linux system
calls must be inside an `unsafe` block:

```casa
fn read_byte address:ptr -> i64 {
    unsafe { address load8 }
}
```

Raw storage uses `u64 alloc -> ptr` and `ptr free -> None`. Allocating zero
bytes returns the null pointer. Freeing null does nothing. A positive allocation
returns aligned storage or terminates the process if storage is exhausted.
`free` releases only a complete live allocation returned by `alloc`. Double
free, use after free, and freeing foreign or interior pointers are undefined
behavior.

The block permits only designated unsafe operations. Type, ownership, borrow,
control-flow, and stack-effect checks still apply.

Use `unsafe fn` when callers must uphold a contract that the function cannot
check. Calls to an unsafe function also require an `unsafe` block. An unsafe
function body does not become an implicit unsafe block:

```casa
unsafe fn copy_bytes destination:ptr source:ptr count:u64 {
    unsafe { count source destination memcpy }
}
```

Unsafe functions cannot be used as function values. Put the unsafe call in a
safe wrapper when the wrapper can validate and preserve a safe contract.

## Bindings

`= name` pops the top value and binds it. The first assignment fixes the
binding's type:

```casa
42 = count
1 += count
count print
```

A binding inside a function is local, even when a global has the same name.
Declare a global at the start of a function before you assign it:

```casa
0 = COUNTER

fn increment_counter {
    global COUNTER
    1 += COUNTER
}
```

`global NAME` must appear before other statements. The global must already
exist. Lambdas cannot declare globals.

Use `= name:Type` when the value needs an explicit type:

```casa
Option::None = result:Option[i64]
```

See [Operators](operators.md#assignment) for assignment forms.

## Ownership and borrows

A plain `T` parameter consumes an owned value. Non-Copy owners can be consumed
only once:

```casa
fn consume text:str { text drop }

"one owner" = text
text consume
# text consume    # Error: text was already moved.
```

Use `$T` for shared access and `mut$T` for exclusive mutable access. Calls
borrow an available owner automatically:

```casa
fn length text:$str -> u64 { text.length }
fn clear text:mut$str { text.clear }

"Casa" = text
text length print
text clear
text length print
```

Shared borrows are Copy. Exclusive borrows and non-Copy owners are affine.
`dup` and the copied value of `over` require `Copy`. `swap` and `rot` only move
values, so they also work with non-Copy owners.

An owner or exclusive borrow can be reborrowed for a call. When the call does
not return a borrow, the reborrow ends when the call returns. One call cannot
borrow the same binding exclusively more than once or combine shared and
exclusive borrows of that binding:

```casa
fn replace_both left:mut$Person right:mut$Person { }

# person person replace_both  # Error: the exclusive arguments alias.
```

## Lambdas and closures

Braces create an anonymous function value:

```casa
{ 1 + } = increment
41 increment exec print    # 42
```

The compiler infers a lambda's stack effect from its body and the context in
which it is used. Here, `increment` has type `fn[i64 -> i64]`.

A lambda can capture bindings from its enclosing scope. Copy values are copied.
A non-Copy owner moves into the lambda:

```casa
10 = offset
{ offset + } = add_offset
32 add_offset exec print    # 42
```

Use a lambda for a short callback. Use a named function when the operation is
shared or needs its own documentation.
