# Operators

Casa operators use postfix notation. Push the operands first, then write the
operator:

```casa
3 4 + 2 * print    # 14
```

There is no operator precedence. Each operator immediately consumes its
operands and pushes its result.

## Operand order

Arithmetic reads from left to right:

```casa
10 3 - print    # 7, because this means 10 - 3
```

Functions and comparisons use the topmost value as the first operand:

```casa
0 1 > print     # true, because this means 1 > 0
```

This comparison rule can be surprising. For example, `90 score >=` means
`score >= 90`.

## Arithmetic

| Operator | Stack effect | Meaning |
|---|---|---|
| `+` | `T T -> T` | Addition |
| `-` | `T T -> T` | Subtraction |
| `*` | `T T -> T` | Multiplication |
| `/` | `T T -> T` | Division |
| `%` | `T T -> T` | Integer remainder |

Operands must have the same numeric type. Integer division truncates toward
zero. Floating-point arithmetic preserves the operand width.

Integer arithmetic terminates the program on overflow, division by zero, or
an invalid shift. The standard library provides `try_add`, `try_sub`,
`try_mul`, `try_div`, and `try_mod` when failure must produce an `Option`.
It also provides `wrapping_add`, `wrapping_sub`, and `wrapping_mul` for
deliberate modulo arithmetic.

`+` and `-` also apply byte offsets to pointers:

```casa
16 alloc = buffer
42 buffer (ptr) 8 + store64
```

## Bit operations

| Operator | Stack effect | Meaning |
|---|---|---|
| `<<` | `u64 T -> T` | Left shift |
| `>>` | `u64 T -> T` | Right shift |
| `&` | `T T -> T` | Bitwise AND |
| `\|` | `T T -> T` | Bitwise OR |
| `^` | `T T -> T` | Bitwise XOR |
| `~` | `T -> T` | Bitwise NOT |

Shifts preserve the integer width. A signed right shift preserves the sign.
`&name` is a function reference when `&` appears before an identifier.

## Comparisons

| Operator | Stack effect | Meaning |
|---|---|---|
| `==` | `[T: Eq] T T -> bool` | Equal |
| `!=` | `[T: Eq] T T -> bool` | Not equal |
| `<` | `[T: Ord] T T -> bool` | Less than |
| `<=` | `[T: Ord] T T -> bool` | Less than or equal |
| `>` | `[T: Ord] T T -> bool` | Greater than |
| `>=` | `[T: Ord] T T -> bool` | Greater than or equal |

The operands must have the same type. Strings support `==` and `!=`, which
compare their contents. Floating-point comparisons follow IEEE behavior. In
particular, ordered comparisons with NaN are false.

See [Traits](traits.md) for comparisons on user-defined types.

## Boolean operators

| Operator | Stack effect | Meaning |
|---|---|---|
| `&&` | `bool bool -> bool` | Logical AND |
| `\|\|` | `bool bool -> bool` | Logical OR |
| `!` | `bool -> bool` | Logical NOT |

Both operands of `&&` and `||` are evaluated before the operator runs.

## Assignment

| Form | Meaning |
|---|---|
| `value = name` | Create or replace a binding |
| `value = name:Type` | Bind with an explicit type |
| `value += name` | Add to an integer binding |
| `value -= name` | Subtract from an integer binding |

```casa
42 = count
1 += count
10 -= count
"Helsinki" = person.address.city
```

A field assignment must start from a named binding. See
[Functions and Lambdas](functions-and-lambdas.md#bindings) for scope and
[Structs and Methods](structs-and-methods.md) for fields.
