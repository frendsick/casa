# Ownership and Memory Management

Casa uses a scope-based ownership system to manage heap-allocated values automatically. When a variable holding an owned type goes out of scope, the compiler inserts code to free its memory. This prevents memory leaks without requiring a garbage collector or manual memory management in most cases.

## Copy Types vs Owned Types

Types in Casa fall into two categories:

| Category | Types | Behavior |
|----------|-------|----------|
| **Copy** | `int`, `bool`, `str`, `ptr`, `fn[sig]`, `any` | Values are freely duplicated. No cleanup needed. |
| **Owned** | `array[T]`, user-defined structs | Heap-allocated. Freed automatically when the owning scope ends. |

Copy types can be used with `dup`, `over`, and captured in closures without restriction. Owned types have additional rules to prevent use-after-free and double-free bugs.

## Automatic Scope-Based Freeing

When a local variable holding an owned type goes out of scope (the function returns), the compiler automatically frees the memory. You do not need to call `free` manually in most cases.

```casa
fn example {
    [1, 2, 3] = arr       # arr is owned (array[int])
    arr.length print       # use arr normally
}                          # arr is automatically freed here
```

Global variables holding owned types are freed when the program exits.

## `clone`

Deep-copies an owned value, producing two independent copies on the stack.

**Stack effect:** `owned -> owned owned`

```casa
[1, 2, 3] clone    # two independent array[int] values on the stack
= copy
= original
```

The `clone` intrinsic works on both arrays and structs. Each copy has its own heap allocation, so modifying one does not affect the other.

`clone` only works on owned types. Using it on a copy type is a compile-time error:

```casa
42 clone    # ERROR: Cannot clone copy type `int`, `clone` is only for owned types
```

## `free`

Explicitly frees an owned value from the stack.

**Stack effect:** `owned ->`

```casa
[1, 2, 3] = arr
arr free    # manually free the array
```

Use `free` when you need to release memory before a scope ends, or when replacing a value in a data structure. For example, `List::push` uses `free` to release the old backing array when it grows:

```casa
# Inside List::push (from lib/std.casa)
self.array free        # free old array before replacing it
new_array self->array  # assign new, larger array
```

In most code, automatic scope-based freeing is sufficient and `free` is not needed.

## Ownership Rules

### `dup` and `over` are restricted on owned types

Since `dup` and `over` create shallow copies, using them on owned types would create two references to the same heap memory. This would lead to double-free bugs, so the compiler rejects it:

```casa
[1, 2, 3] dup     # ERROR: Cannot duplicate owned type `array[int]`, use `clone` instead
```

```casa
42 [1, 2] over    # ERROR: Cannot duplicate owned type `array[int]` via `over`, use `clone` instead
```

Use `clone` instead when you need two copies of an owned value.

`swap` and `rot` are allowed on owned types because they move values without duplicating them.

### Closures cannot capture owned types

Lambdas capture variables by copying them into the closure. Since owned types cannot be implicitly copied, capturing an owned variable is a compile-time error:

```casa
[1, 2, 3] = arr
{ arr.length }    # ERROR: Cannot capture owned type `array[int]` in closure
```

### `drop` frees owned types

When `drop` is used on an owned type, the compiler emits the appropriate free instruction. Dropping a copy type simply discards the value.

```casa
[1, 2, 3] drop    # array is freed
42 drop            # integer is simply discarded
```

## Examples

### Basic ownership

```casa
fn make_numbers -> array[int] {
    [10, 20, 30]    # returned to caller, not freed
}

fn process {
    make_numbers = nums
    nums.length print       # 3
}                           # nums is freed here
```

### Cloning for independent copies

```casa
struct Point {
    x: int
    y: int
}

fn example {
    10 20 Point = p1
    p1 clone = p2 = p1      # p1 and p2 are independent copies
    99 p2->x                 # modifying p2 does not affect p1
    p1.x print               # 10
}                            # both p1 and p2 are freed
```

### Early free

```casa
fn process_data {
    [1, 2, 3, 4, 5] = data
    data.length = len        # extract what we need
    data free                # free early, we only need len from here
    len print
}
```
