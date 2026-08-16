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

### Operand order

Arithmetic and comparison operators use different stack conventions:

- **Arithmetic** (`+ - * / % << >> & | ^`): `a b op` = `a op b`. The value pushed first is the left operand. This means `10 3 -` is `10 - 3 = 7` — natural left-to-right reading.
- **Comparison** (`== != < <= > >=`): `a b op` = `b op a`. The top of the stack is the left operand, matching function call convention. This means `0 1 >` is `1 > 0 = true`.

```
10 3 -     # 10 - 3 = 7   (arithmetic: left-to-right)
10 3 >     # 3 > 10 = false (comparison: top is left operand)
```

## Arithmetic

All arithmetic operators consume two values and produce one.

| Operator | Stack Effect | Description |
|----------|-------------|-------------|
| `+` | `T T -> T` | Same-width numeric addition; also `ptr i64 -> ptr` |
| `-` | `T T -> T` | Same-width numeric subtraction; also `ptr i64 -> ptr` |
| `*` | `T T -> T` | Same-width numeric multiplication |
| `/` | `T T -> T` | Same-width numeric division; integer division truncates toward zero |
| `%` | `T T -> T` | Same-width integer remainder |

```casa
34 35 + print    # 69
1357 20 - print  # 1337
7 6 * print      # 42
14 3 / print     # 4
14 3 % print     # 2
```

Integer `+`, `-`, `*`, `/`, `%`, `<<`, and `>>` terminate the program when the
result is not representable, the divisor is zero, or a shift count is outside
the operand width. Use `try_add`, `try_sub`, `try_mul`, `try_div`, or `try_mod`
for an `Option[T]` result. Deliberate modulo arithmetic uses `wrapping_add`,
`wrapping_sub`, and `wrapping_mul`.

Floating-point `+`, `-`, `*`, and `/` use strict IEEE round-to-nearest,
ties-to-even execution. They preserve subnormals, signed zero, infinities, and
NaNs and never promote `f32` to `f64` implicitly.

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
| `<<` | `T u64 -> T` | Integer left shift; result keeps the operand width |
| `>>` | `T u64 -> T` | Integer right shift; signed types preserve the sign |

```casa
1 4 << print    # 16
16 4 >> print   # 1
```

## Bitwise

| Operator | Stack Effect | Description |
|----------|-------------|-------------|
| `&` | `T T -> T` | Same-width integer bitwise AND |
| `\|` | `T T -> T` | Same-width integer bitwise OR |
| `^` | `T T -> T` | Same-width integer bitwise XOR |
| `~` | `T -> T` | Integer bitwise NOT (one's complement) |

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

Built-in integer and floating-point types, `bool`, `char`, `cstr`, `ptr`, and
enums get direct bytecode comparison. Numeric operands must have the same
width. Floating-point comparison is partial: every ordered comparison with NaN
is false, equality with NaN is false, and inequality with NaN is true.
User-defined types must provide `impl T { fn eq ... }` (and `fn lt ...` for
ordering); the operator then lowers to the corresponding trait method call. See
[traits.md](traits.md) for `Eq` and `Ord`.

```casa
1 1 == print    # true
1 0 != print    # true
0 1 > print     # true (1 > 0, top is left operand)
1 0 > print     # false (0 > 1)
```

Comparison operators use the top of the stack as the left operand (`a b op` = `b op a`). See [Operand order](#operand-order) above.

### String comparison

String `==` and `!=` compare by content (byte-by-byte), not by pointer identity. Other comparison operators (`<`, `<=`, `>`, `>=`) are not supported for strings.

```casa
"hello" "hello" == print    # true
"hello" "world" != print    # true
```

## Boolean

| Operator | Stack Effect | Description |
|----------|-------------|-------------|
| `&&` | `bool bool -> bool` | Logical AND |
| `\|\|` | `bool bool -> bool` | Logical OR |
| `!`  | `bool -> bool` | Logical NOT |

```casa
true true && print    # true
true false || print   # true
true ! print          # false
```

## Assignment

Assignment operators pop a value from the stack and store it in a variable or a
variable-rooted field path.

| Operator | Stack Effect | Description |
|----------|-------------|-------------|
| `= target` | `T -> None` | Assign top of stack to variable or field path `target` |
| `= name:type` | `T -> None` | Assign with type annotation, verifies and narrows the type |
| `+= target` | `T -> None` | Add to a same-width integer variable or field path |
| `-= target` | `T -> None` | Subtract from a same-width integer variable or field path |

```casa
42 = count        # count is now 42
1 += count        # count is now 43
10 -= count       # count is now 33
"Jane" = person.name
1 += person.age
"Helsinki" = person.address.city
```

A field-path target has the form `identifier(.field)*`. Its root must be a named
variable; arbitrary expression receivers are not assignable.

### Type annotations

The `= name:type` form lets you annotate the type of a variable at assignment time. The type checker verifies the stack value is compatible and uses the annotated type for the variable.

```casa
42 = x:i64                  # explicit i64 annotation
Option::None = empty:Option[i64]    # narrow bare Option to Option[i64]
```

Variables are created on first assignment. See [Functions and Lambdas -- Variables](functions-and-lambdas.md#variables) for scoping rules.

## See Also

- [Types and Literals](types-and-literals.md) -- primitive types and type casting
- [Functions and Lambdas](functions-and-lambdas.md) -- variables and scoping rules
- [Control Flow](control-flow.md) -- using conditions in `if` and `while`
