# Built-in Intrinsics

Intrinsics are operations built into the compiler. They are available in every Casa program without any `import` directive.

## Stack Intrinsics

Operations for manipulating the stack directly.

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

**Signature:** `print a:any`

**Stack effect:** `a -> None`

Integers print as decimal numbers. Booleans print as `true` or `false`. Strings, characters, and C strings print as text.

```casa
42 print                    # 42
true print                  # true
"Hello" print               # Hello
'A' print                   # A
"Hello" .as_cstr print      # Hello
"Hello" print "\n" print    # Hello followed by a newline
```

### `typeof`

Consumes the top of the stack and prints its type name to stdout.

**Signature:** `typeof a:any`

**Stack effect:** `a -> None`

Works with all types: `int`, `bool`, `str`, `char`, `ptr`, `array`, `fn`, structs, and enums.

```casa
42 typeof                   # int
true typeof                 # bool
"hello" typeof              # str
```

## Memory Intrinsics

Low-level byte-addressed memory access for building data structures. All load/store intrinsics use absolute addressing.

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

### Examples

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

## See Also

- [Types and Literals](types-and-literals.md) -- primitive types used by intrinsics
- [Operators](operators.md) -- arithmetic and comparison operators
- [Standard Library](standard-library.md) -- higher-level abstractions built on these primitives
