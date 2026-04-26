# Operators

Casa uses postfix (reverse Polish) notation. Operands are pushed onto the stack first, then the operator consumes them and pushes the result. There is no operator precedence.

## Stack-Based Evaluation

In most languages you write `(3 + 4) * 2`. In Casa, you write `3 4 + 2 *`. Here is how it evaluates step by step:

```
Step       Operation    Stack (top on right)
────       ─────────    ────────────────────
           start        [ ]
3          push 3       [ 3 ]
4          push 4       [ 3, 4 ]
+          add          [ 7 ]
2          push 2       [ 7, 2 ]
*          multiply     [ 14 ]
```

The stack replaces parentheses and precedence rules. Values are consumed left to right, and every operator immediately uses the top values on the stack.

For binary operators, the **top of the stack is the first argument**. This means `1 0 >` asks "is `0` greater than `1`?", not "is `1` greater than `0`?". To check if 1 > 0, write `0 1 >`.

## Arithmetic

All arithmetic operators consume two values and produce one.

| Operator | Stack Effect | Description |
|----------|-------------|-------------|
| `+` | `a int -> a` | Addition. The second operand must be `int`; result type matches the first operand. Works on `ptr` for pointer arithmetic. |
| `-` | `a int -> a` | Subtraction. Same typing rules as `+`. |
| `*` | `int int -> int` | Multiplication |
| `/` | `int int -> int` | Integer division (truncates toward zero) |
| `%` | `int int -> int` | Modulo (remainder) |

```casa
34 35 + print    # 69
1357 20 - print  # 1337
7 6 * print      # 42
14 3 / print     # 4
14 3 % print     # 2
```

### Pointer Arithmetic

`+` and `-` support `ptr` as the first operand, allowing offset-based heap access:

```casa
32 alloc = buf
42 buf (ptr) 8 + store64    # store 42 at byte offset 8
buf (ptr) 8 + load64 print  # 42
```

## Bitshift

| Operator | Stack Effect | Description |
|----------|-------------|-------------|
| `<<` | `a int -> a` | Left shift. Result type matches the first operand. |
| `>>` | `a int -> a` | Right shift. Result type matches the first operand. |

```casa
1 4 << print    # 16
16 4 >> print   # 1
```

## Bitwise

| Operator | Stack Effect | Description |
|----------|-------------|-------------|
| `&` | `int int -> int` | Bitwise AND |
| `\|` | `int int -> int` | Bitwise OR |
| `^` | `int int -> int` | Bitwise XOR |
| `~` | `int -> int` | Bitwise NOT (one's complement) |

```casa
12 10 & print   # 8 (1100 AND 1010 = 1000)
12 10 | print   # 14 (1100 OR 1010 = 1110)
12 10 ^ print   # 6 (1100 XOR 1010 = 0110)
12 ~ print      # -13 (inverts all bits)
```

> **Note:** `&` is also used as a function reference prefix (`&name`). When followed by an identifier, it creates a function reference. When used after two values on the stack, it performs bitwise AND.

## Comparison

All comparison operators consume two values of the same type and push a `bool`. Equality operators require the operand type to satisfy the `Eq` trait; ordering operators require `Ord`.

| Operator | Stack Effect | Description |
|----------|-------------|-------------|
| `==` | `[T: Eq] T T -> bool` | Equal |
| `!=` | `[T: Eq] T T -> bool` | Not equal |
| `<`  | `[T: Ord] T T -> bool` | Less than |
| `<=` | `[T: Ord] T T -> bool` | Less than or equal |
| `>`  | `[T: Ord] T T -> bool` | Greater than |
| `>=` | `[T: Ord] T T -> bool` | Greater than or equal |

Built-in primitives (`int`, `bool`, `char`, `cstr`, `ptr`) and enums get direct bytecode comparison. User-defined types must provide `impl T { fn eq ... }` (and `fn lt ...` for ordering); the operator then lowers to the corresponding trait method call. See [traits.md](traits.md) for `Eq` and `Ord`.

```casa
1 1 == print    # 1 (true)
1 0 != print    # 1 (true)
1 0 > print     # 0 (false: top=0 is not greater than second=1)
```

> **Note on stack order:** In `1 0 >`, the value `0` is on top of the stack and `1` is below. The comparison checks whether the top (`0`) is greater than the second (`1`), which is false. To check if 1 > 0, write `0 1 >` or equivalently `1 0 <`.

### String comparison

String `==` and `!=` compare by content (byte-by-byte), not by pointer identity. Other comparison operators (`<`, `<=`, `>`, `>=`) are not supported for strings.

```casa
"hello" "hello" == print    # true
"hello" "world" != print    # true
```

## Boolean

| Operator | Stack Effect | Description |
|----------|-------------|-------------|
| `&&` | `any any -> bool` | Logical AND |
| `\|\|` | `any any -> bool` | Logical OR |
| `!`  | `any -> bool` | Logical NOT |

```casa
true true && print    # 1
true false || print   # 1
true ! print          # 0
```

## Assignment

Assignment operators pop a value from the stack and store it in a named variable.

| Operator | Stack Effect | Description |
|----------|-------------|-------------|
| `= name` | `a -> None` | Assign top of stack to variable `name` |
| `= name:type` | `a -> None` | Assign with type annotation, verifies and narrows the type |
| `+= name` | `int -> None` | Add top of stack to variable `name` |
| `-= name` | `int -> None` | Subtract top of stack from variable `name` |

```casa
42 = count        # count is now 42
1 += count        # count is now 43
10 -= count       # count is now 33
```

### Type annotations

The `= name:type` form lets you annotate the type of a variable at assignment time. The type checker verifies the stack value is compatible and uses the annotated type for the variable.

```casa
42 = x:int                  # explicit int annotation
Option::None = empty:Option[int]    # narrow bare Option to Option[int]
```

Variables are created on first assignment. See [Functions and Lambdas -- Variables](functions-and-lambdas.md#variables) for scoping rules.

## See Also

- [Types and Literals](types-and-literals.md) -- primitive types and type casting
- [Functions and Lambdas](functions-and-lambdas.md) -- variables and scoping rules
- [Control Flow](control-flow.md) -- using conditions in `if` and `while`
