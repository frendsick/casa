# Built-in Intrinsics

Intrinsics are compiler-provided operations. They need no import.

## Stack operations

| Intrinsic | Stack effect | Action |
|---|---|---|
| `drop` | `T -> None` | Destroy the top owned value |
| `dup` | `[T: Copy] T -> T T` | Duplicate the top value |
| `copy` | `[T: Copy] T -> T` | Produce an owned Copy value |
| `swap` | `T1 T2 -> T2 T1` | Swap the top two values |
| `over` | `[T2: Copy] T1 T2 -> T2 T1 T2` | Copy the second value to the top |
| `rot` | `T1 T2 T3 -> T3 T1 T2` | Rotate the top three values |

```casa
1 2 drop print       # 1
3 dup + print        # 6
```

`swap` and `rot` only move values. They accept non-Copy values. `dup` and `over`
also duplicate shared borrows without making `$T` satisfy Copy. `dup`, `over`,
and `copy` never call Clone and never allocate. `drop` runs the same custom
cleanup and recursive field destruction as a scope exit.

## Output and inspection

| Intrinsic | Stack effect | Action |
|---|---|---|
| `print` | `[T: Display] T -> None` | Write a value to standard output |
| `size_of[T]` | `None -> u64` | Return the inline storage size of `T` |
| `typeof` | `T -> str` | Return the compile-time type name |
| `exec` | `fn[...] -> ...` | Call a function value on the top of the stack |

```casa
42 print
"hello" typeof print    # str
```

Primitive display types print directly. User-defined types must implement
[`Display`](traits.md#built-in-traits). See
[Functions and Lambdas](functions-and-lambdas.md#function-values) for `exec`.

## Process values

| Intrinsic | Stack effect | Value |
|---|---|---|
| `argc` | `None -> u64` | Command-line argument count |
| `argv` | `None -> ptr` | Command-line argument vector |
| `envp` | `None -> ptr` | Environment vector |

Prefer the standard-library process helpers unless raw startup data is needed.

## Advanced memory access

These operations expose raw byte-addressed memory. Prefer standard-library
collections and strings for application code. Each operation must be inside
an `unsafe` block.

| Intrinsic | Stack effect | Action |
|---|---|---|
| `alloc` | `u64 -> ptr` | Allocate bytes or terminate on failure |
| `free` | `ptr -> None` | Release a complete live allocation |
| `load8` | `ptr -> u8` | Load an unsigned 8-bit value |
| `load16` | `ptr -> u16` | Load an unsigned 16-bit value |
| `load32` | `ptr -> u32` | Load an unsigned 32-bit value |
| `load64` | `ptr -> u64` | Load an unsigned 64-bit value |
| `store8` | `ptr u8 -> None` | Store an 8-bit value |
| `store16` | `ptr u16 -> None` | Store a 16-bit value |
| `store32` | `ptr u32 -> None` | Store a 32-bit value |
| `store64` | `ptr u64 -> None` | Store a 64-bit value |

Inputs in a stack effect are listed from the top downward. The value is pushed
before the destination pointer at a store call:

```casa
unsafe {
    16 alloc = buffer
    42 buffer (ptr) store64
    buffer (ptr) load64 print    # 42
}
```

Pointer `+` and `-` take `u64` byte offsets. Multibyte loads and stores permit
unaligned addresses and use little-endian byte order.

`0 alloc` returns null, and `free` does nothing when given null. A positive
allocation is non-null. Double free, use after free, and freeing an interior or
foreign pointer are undefined behavior.

### Forming a borrow from a raw address

Casting a loaded word to a borrow type inside `unsafe` produces a typed borrow.
A raw address carries no lifetime, so the result is anchored conservatively to
every compatible borrowed input of the enclosing function: a `$T` accepts any
borrowed input, a `mut$T` only exclusive ones. A function with no compatible
input cannot return the borrow and is rejected with `Borrowed return has no live
input origin`.

```casa
const ARRAY_ELEMENT_WORD_SIZE 8

fn nth [T const N:u64] array:$array[T N] index:u64 -> $T {
    unsafe { array (ptr) index ARRAY_ELEMENT_WORD_SIZE * + load64 ($T) }
}
```

The unsafe body promises that the address stays valid for as long as the
anchored input. See ADR-0112 and ADR-0113.

`unsafe fn memcpy destination:ptr source:ptr count:u64` is a `std` function,
not an intrinsic. It becomes available after `import "std"` and copies raw
bytes. Its call must also be inside an `unsafe` block. Prefer typed collections
and text operations unless raw memory is required.

## Advanced Linux system calls

`syscall0` through `syscall6` invoke Linux x86-64 system calls directly. Push
the arguments in reverse register order, then push the syscall number. The
number is the topmost value when the intrinsic runs. Each call must be inside
an `unsafe` block.

| Intrinsic | Stack effect |
|---|---|
| `syscall0` | `i64 -> i64` |
| `syscall1` | `i64 A1 -> i64` |
| `syscall2` | `i64 A1 A2 -> i64` |
| `syscall3` | `i64 A1 A2 A3 -> i64` |
| `syscall4` | `i64 A1 A2 A3 A4 -> i64` |
| `syscall5` | `i64 A1 A2 A3 A4 A5 -> i64` |
| `syscall6` | `i64 A1 A2 A3 A4 A5 A6 -> i64` |

Each argument must fit one machine word. The kernel return value is `i64`.

| Value | Register |
|---|---|
| Syscall number | `%rax` |
| Argument 1 | `%rdi` |
| Argument 2 | `%rsi` |
| Argument 3 | `%rdx` |
| Argument 4 | `%r10` |
| Argument 5 | `%r8` |
| Argument 6 | `%r9` |

```casa
# exit(0)
unsafe { 0 60 syscall1 drop }
```

The [operating-system APIs](os.md) provide safer operations for normal
programs.
