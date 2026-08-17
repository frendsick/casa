# Built-in Intrinsics

Intrinsics are compiler-provided operations. They need no import.

## Stack operations

| Intrinsic | Stack effect | Action |
|---|---|---|
| `drop` | `T -> None` | Discard the top value |
| `dup` | `T -> T T` | Duplicate the top value |
| `swap` | `T1 T2 -> T2 T1` | Swap the top two values |
| `over` | `T1 T2 -> T2 T1 T2` | Copy the second value to the top |
| `rot` | `T1 T2 T3 -> T3 T1 T2` | Rotate the top three values |

```casa
1 2 drop print       # 1
3 dup + print        # 6
```

## Output and inspection

| Intrinsic | Stack effect | Action |
|---|---|---|
| `print` | `[T: Display] T -> None` | Write a value to standard output |
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
collections and strings for application code.

| Intrinsic | Stack effect | Action |
|---|---|---|
| `alloc` | `u64 -> ptr` | Allocate bytes on the heap |
| `load8` | `ptr -> i64` | Load 8 bits and zero-extend them |
| `load16` | `ptr -> i64` | Load 16 bits and zero-extend them |
| `load32` | `ptr -> i64` | Load 32 bits and zero-extend them |
| `load64` | `ptr -> i64` | Load 64 bits |
| `store8` | `[T: Word] ptr T -> None` | Store the low 8 bits |
| `store16` | `[T: Word] ptr T -> None` | Store the low 16 bits |
| `store32` | `[T: Word] ptr T -> None` | Store the low 32 bits |
| `store64` | `[T: Word] ptr T -> None` | Store 64 bits |

Inputs in a stack effect are listed from the top downward. The value is pushed
before the destination pointer at a store call:

```casa
16 alloc = buffer
42 buffer (ptr) store64
buffer (ptr) load64 print    # 42
```

Pointer offsets are measured in bytes.

`memcpy destination:ptr source:ptr count:u64` is a `std` function, not an
intrinsic. It becomes available after `import "std"` and copies raw bytes.
Prefer typed collections and text operations unless raw memory is required.

## Advanced Linux system calls

`syscall0` through `syscall6` invoke Linux x86-64 system calls directly. Push
the arguments in reverse register order, then push the syscall number. The
number is the topmost value when the intrinsic runs.

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
0 60 syscall1 drop
```

The [operating-system APIs](os.md) provide safer operations for normal
programs.
