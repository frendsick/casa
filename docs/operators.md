# Operators

Casa operators use postfix notation. Push the operands first, then write the
operator:

```casa
3 4 + 2 * print # 14
```

There is no operator precedence. Each operator immediately consumes its
operands and pushes its result.

Stack-effect inputs use consumption order. Outputs use push order. See
[reference notation](notation.md) for examples and the meaning of `None`.

## Operand order

All binary symbolic operators read operands from left to right:

```casa
10 3 - print # 7, because this means 10 - 3
```

Comparisons use the same order:

```casa
0 1 < print # true, because this means 0 < 1
```

For example, `score 90 >=` means `score >= 90`. The topmost consumed value
is the right operand. This applies to arithmetic, shifts, bitwise operations,
comparisons, and eager boolean operations, including constant blocks.

Named functions and receiver methods keep their topmost-first parameter order.
Both operand expressions run from left to right before the operator runs.
See [migrating existing source](operator-migration.md) for code written with
the former comparison rule.

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

After `import "std"`, these `i64` helpers are available:

| Method | Signature | Description |
|---|---|---|
| [abs](#i64abs) | `fn abs self:$i64 -> i64` | Absolute value |
| [clamp](#i64clamp) | `fn clamp self:$i64 lo:i64 hi:i64 -> i64` | Value limited to the inclusive range |
| [max](#i64max) | `fn max self:$i64 other:i64 -> i64` | Larger value |
| [min](#i64min) | `fn min self:$i64 other:i64 -> i64` | Smaller value |
| [pow](#i64pow) | `fn pow self:$i64 exp:i64 -> i64` | Integer exponentiation |

### i64::abs

```text
fn abs self:$i64 -> i64
```

Returns the absolute value.

### i64::clamp

```text
fn clamp self:$i64 lo:i64 hi:i64 -> i64
```

Returns the value limited to the inclusive range.

### i64::max

```text
fn max self:$i64 other:i64 -> i64
```

Returns the larger value.

### i64::min

```text
fn min self:$i64 other:i64 -> i64
```

Returns the smaller value.

### i64::pow

```text
fn pow self:$i64 exp:i64 -> i64
```

Raises the integer to the given exponent.

### Arithmetic examples

`f32` and `f64` also provide `abs`.

`+` and `-` also apply `u64` byte offsets to pointers without element scaling:

```casa
unsafe {
    16 alloc = buffer
    42 buffer 8 + store64
}
```

Pointer arithmetic requires an [unsafe](functions-and-lambdas.md#unsafe-boundaries) block.

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
| `==` | `[T: PartialEq] T T -> bool` | Equal |
| `!=` | `[T: PartialEq] T T -> bool` | Not equal |
| `<` | `[T: PartialOrd] T T -> bool` | Less than |
| `<=` | `[T: PartialOrd] T T -> bool` | Less than or equal |
| `>` | `[T: PartialOrd] T T -> bool` | Greater than |
| `>=` | `[T: PartialOrd] T T -> bool` | Greater than or equal |

The operands must have the same type. Strings support `==` and `!=`, which
compare their contents. Floating-point values implement only the partial
comparison traits. Their operators follow IEEE behavior, so ordered
comparisons with NaN are false.

For user-defined types, the left operand is the receiver of `eq`, `ne`, `lt`,
`le`, `gt`, or `ge`. For already evaluated values, `a b <` corresponds to
`b a.lt`. The operator does not reorder evaluation of the expressions that
produce `a` and `b`. `!=` calls `ne` directly.

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
