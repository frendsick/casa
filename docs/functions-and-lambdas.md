# Functions and Lambdas

## Functions

Functions are defined with `fn` at global scope. They can be called before their definition (forward references are allowed).

### Syntax

```
fn name[TypeVar ...] param:type ... -> return_type ... {
    body
}
```

The `[TypeVar ...]` part is optional — see [Generic Functions](#generic-functions) below.

Parameters are consumed from the top of the stack in declaration order: the first parameter is on top, and the last parameter is deepest. Return types describe what the function leaves on the stack after it returns.

### Basic Example

```casa
fn local_add a:int b:int -> int {
    a b +
}

34 35 local_add print   # 69
```

When `34 35 local_add` is called, `35` is assigned to `a` (first param, popped from top) and `34` is assigned to `b` (second param, popped next). The result is the same since addition is commutative.

### No Parameters

Functions can have no explicit parameters. They can still consume values from the stack if the body does so, and the type checker will infer the signature.

```casa
fn global_add -> int {
    global_a global_b +
}
```

### No Return Type

If a function returns nothing, omit the `-> type` part:

```casa
fn greet name:str {
    name print
}
```

### Signature Inference

Type annotations are optional. The type checker can infer the full signature by replaying the function's operations on a symbolic stack.

```casa
fn double {
    2 *
}
# Inferred signature: int -> int
```

When both an explicit signature and inference are available, the type checker verifies that they match.

### Calling Functions

Push the arguments onto the stack, then write the function name:

```casa
20 fib print
```

### Function References

The `&name` syntax pushes a named function onto the stack as a value instead of calling it.

**Stack effect:** `-> fn[sig]`

```casa
fn add a:int b:int -> int { a b + }

&add                # pushes fn[int int -> int]
3 5 &add exec       # calls add, result: 8
&add = my_fn        # store in variable
3 5 my_fn exec      # call via variable, result: 8
```

This works for struct accessors and methods too:

```casa
struct Point { x: int y: int }

&Point::x           # pushes fn[Point -> int]

impl Point {
    fn sum self:Point -> int { self.x self.y + }
}

&Point::sum          # pushes fn[Point -> int]
```

Use `exec` to call the function reference, just like with lambdas. See [`exec`](#exec) for details.

### Early Return

Use `return` to exit a function early. The stack at the `return` point must match the function's return type.

```casa
fn fib number:int -> int {
    if number 1 >= then
        number return
    elif number 0 == then
        number return
    fi

    number 1 - fib
    number 2 - fib +
}
```

See [`examples/fibonacci.casa`](../examples/fibonacci.casa).

### Generic Functions

Functions can declare type variables in square brackets after the name. Type variables are resolved to concrete types at each call site, enabling type-safe polymorphism.

**Stack effect:** `args... -> returns...` (type variables resolve to the actual types at the call site)

```casa
fn id[T] T -> T { }       # T -> T
42 id print                # int -> int, prints 42
"hello" id print           # str -> str, prints hello
```

Multiple type variables:

```casa
fn swap_t[T1 T2] T1 T2 -> T1 T2 { swap }   # T1 T2 -> T1 T2
5 "hi" swap_t              # int str -> str int
```

Generic functions can mix type variables with concrete types and named parameters:

```casa
fn first[T1 T2] a:T1 b:T2 -> T1 { a }
fn wrap[T] T -> T int { 42 }
```

The type checker enforces consistency — if the same type variable appears multiple times in the parameters, all occurrences must bind to the same type:

```casa
fn pair[T] T T -> T T { }
42 42 pair        # OK: both T=int
42 "hi" pair      # ERROR: T bound to int and str
```

Generic type parameters also work in `impl` block methods:

```casa
impl Box {
    fn apply[T] self:Box T -> T { }
}
```

Every type variable must appear in at least one parameter (return-only type variables are not allowed).

Type variable names must not collide with built-in types (`int`, `bool`, `str`, `ptr`, `array`, `any`) or user-defined struct names:

```casa
fn bad[int] int -> int { }     # ERROR: shadows built-in type
fn bad[MyStruct] MyStruct -> MyStruct { }   # ERROR: shadows struct type
fn good[T] T -> T { }         # OK
```

### Restrictions

- Functions must be defined at **global scope** — no nested function definitions (use lambdas instead).
- A function's signature mismatch is detected when the function is **called**, not at its definition.

## Variables

### Global Variables

Declared at the top level, visible everywhere (including inside functions):

```casa
1 = global_a
2 = global_b

fn global_add -> int {
    global_a global_b +
}
```

### Local Variables

Declared inside a function, scoped to that function:

```casa
fn fizzbuzz number:int {
    number 3 % 0 == = fizz    # fizz is local
    number 5 % 0 == = buzz    # buzz is local
    # ...
}
```

### Assignment Operators

| Operator | Stack Effect | Description |
|----------|-------------|-------------|
| `= name` | `a -> None` | Assign top of stack to `name` |
| `+= name` | `int -> None` | Add to `name` |
| `-= name` | `int -> None` | Subtract from `name` |

A variable's type is set on first assignment and cannot change:

```casa
42 = x       # x is int
"hi" = x     # ERROR: cannot assign str to int variable
```

## Lambdas

Lambdas are anonymous functions created with braces `{ body }`. They push a function value onto the stack.

**Stack effect:** `-> fn[sig]`

### Basic Example

```casa
{ 2 * }              # type: fn[int -> int]
21 swap exec print   # 42
```

### Storing and Calling

```casa
{ 1 + } = increment
41 increment exec print   # 42
```

### Closures

Lambdas capture variables from their enclosing scope:

```casa
10 = offset
{ offset + } = add_offset
32 add_offset exec print   # 42
```

Captured variables are copied at the time the lambda is created.

### Passing Lambdas

Lambdas are first-class values and can be passed to functions:

```casa
fn apply_twice f:fn[int -> int] x:int -> int {
    x f exec f exec
}

40 { 1 + } apply_twice print   # 42
```

### `exec`

Calls the function value on top of the stack.

**Stack effect:** `args... fn[sig] -> results...`

The function value must be on top of the stack, with its arguments below. For `fn[int -> int]`, exec pops the function and one `int`, then pushes one `int`.

## Stack Intrinsics

Built-in operations for manipulating the stack directly.

| Intrinsic | Stack Effect | Description |
|-----------|-------------|-------------|
| `drop` | `a ->` | Discard top of stack |
| `dup` | `a -> a a` | Duplicate top of stack |
| `swap` | `a b -> b a` | Swap top two values |
| `over` | `a b -> a b a` | Copy second value to top |
| `rot` | `a b c -> b c a` | Rotate top three values |

### Examples

```casa
1 2 drop print       # 1

3 dup print print    # 3 3

9 10 swap
print print          # 9 10

4 5 over
print print print    # 4 5 4

6 7 8 rot
print print print    # 6 8 7
```

See [`examples/stack_operations.casa`](../examples/stack_operations.casa).

## IO

### `print`

Prints the top of the stack to stdout.

**Stack effect:** `a -> None`

Integers and booleans are printed as decimal numbers. Strings are printed as text.

```casa
42 print                    # 42
true print                  # 1
"Hello" print               # Hello
"Hello" print "\n" print    # Hello followed by a newline
```

## Memory Intrinsics

Low-level byte-addressed heap access for building data structures.

| Intrinsic | Stack Effect | Description |
|-----------|-------------|-------------|
| `alloc` | `int -> ptr` | Allocate N bytes of heap memory, return pointer |
| `load8` | `ptr -> int` | Load 8-bit value from address (zero-extended) |
| `load16` | `ptr -> int` | Load 16-bit value from address (zero-extended) |
| `load32` | `ptr -> int` | Load 32-bit value from address (zero-extended) |
| `load64` | `ptr -> int` | Load 64-bit value from address |
| `store8` | `any ptr -> None` | Store 8-bit value to address |
| `store16` | `any ptr -> None` | Store 16-bit value to address |
| `store32` | `any ptr -> None` | Store 32-bit value to address |
| `store64` | `any ptr -> None` | Store 64-bit value to address |

### Example

```casa
32 alloc = buf                # allocate 32 bytes
42 buf (ptr) store64          # store 64-bit value at buf
buf (ptr) load64 print        # 42

255 buf (ptr) 8 + store8      # store 8-bit value at byte offset 8
buf (ptr) 8 + load8 print     # 255

100000 buf (ptr) 16 + store32 # store 32-bit value at byte offset 16
buf (ptr) 16 + load32 print   # 100000
```

Values are addressed by byte offset. Use pointer arithmetic (`+`) to access different offsets within an allocated block. Choose the load/store size that matches your data width.

See [Standard Library](standard-library.md) for higher-level abstractions built on these primitives.

## Syscall Intrinsics

Direct Linux system call access. Each intrinsic pops N+1 values from the stack (the syscall number on top, then arguments in order) and pushes the kernel return value as `int`. The syscall number must be `int`. Arguments have no type constraints.

| Intrinsic | Stack Effect | Description |
|-----------|-------------|-------------|
| `syscall0` | `nr -> int` | Syscall with 0 args |
| `syscall1` | `a1 nr -> int` | Syscall with 1 arg |
| `syscall2` | `a2 a1 nr -> int` | Syscall with 2 args |
| `syscall3` | `a3 a2 a1 nr -> int` | Syscall with 3 args |
| `syscall4` | `a4 a3 a2 a1 nr -> int` | Syscall with 4 args |
| `syscall5` | `a5 a4 a3 a2 a1 nr -> int` | Syscall with 5 args |
| `syscall6` | `a6 a5 a4 a3 a2 a1 nr -> int` | Syscall with 6 args |

### Register Mapping

Arguments are placed in registers following the Linux x86-64 syscall convention:

| Argument | Register |
|----------|----------|
| Syscall number | `%rax` |
| arg1 | `%rdi` |
| arg2 | `%rsi` |
| arg3 | `%rdx` |
| arg4 | `%r10` |
| arg5 | `%r8` |
| arg6 | `%r9` |

### Examples

```casa
# exit(0)
0 60 syscall1 drop

# write(1, buf, len) where buf is a string pointer and len is its length
len buf 1 1 syscall3 drop
```
