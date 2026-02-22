# Operators

Casa uses postfix (reverse Polish) notation. Operands are pushed onto the stack first, then the operator consumes them and pushes the result.

```casa
3 4 +     # push 3, push 4, add → 7
```

There is no operator precedence — evaluation order is determined entirely by the order values appear on the stack.

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

## Comparison

All comparison operators consume two values and push a `bool`.

| Operator | Stack Effect | Description |
|----------|-------------|-------------|
| `==` | `any any -> bool` | Equal |
| `!=` | `any any -> bool` | Not equal |
| `<`  | `any any -> bool` | Less than |
| `<=` | `any any -> bool` | Less than or equal |
| `>`  | `any any -> bool` | Greater than |
| `>=` | `any any -> bool` | Greater than or equal |

```casa
1 1 == print    # 1 (true)
1 0 != print    # 1 (true)
1 0 > print     # 0 (false: top=0 is not greater than second=1)
```

> **Note on stack order:** In `1 0 >`, the value `0` is on top of the stack and `1` is below. The comparison checks whether the top (`0`) is greater than the second (`1`), which is false. To check if 1 > 0, write `0 1 >` or equivalently `1 0 <`.

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
| `+= name` | `int -> None` | Add top of stack to variable `name` |
| `-= name` | `int -> None` | Subtract top of stack from variable `name` |

```casa
42 = count        # count is now 42
1 += count        # count is now 43
10 -= count       # count is now 33
```

Variables are created on first assignment. See [Functions and Lambdas — Variables](functions-and-lambdas.md#variables) for scoping rules.
