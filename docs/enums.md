# Enums

Enums define a type with a fixed set of named variants. Variants can carry inner values, making enums suitable for modeling alternatives and tagged unions.

## Defining an Enum

Use the `enum` keyword followed by the type name and a list of variants in braces.

```casa
enum Color { Red Green Blue }
enum Direction { North South East West }
```

Each variant is a distinct value of the enum type. An enum must have at least one variant. Variant names must be unique within the enum.

## Variants with Inner Values

Variants can carry inner values by specifying types in parentheses.

```casa
enum Shape {
    Circle(int)
    Rectangle(int int)
    Point
}
```

An enum can mix variants with and without inner values. When any variant has inner values, all variants are heap-allocated at runtime.

## Generic Enums

Enums can have type parameters, written in brackets after the name.

```casa
enum Option[T] { None Some(T) }
enum Result[T E] { Error(E) Ok(T) }
```

The type parameters are resolved when constructing or matching on the enum.

## Variant Constructors

Access variants using the `EnumName::VariantName` syntax.

**Stack effect (plain):** `-> EnumName`

```casa
Color::Red        # pushes a Color value
Direction::North  # pushes a Direction value
```

For variants with inner values, push the inner values onto the stack first.

**Stack effect (with inner values):** `inner_values -> EnumName`

```casa
10 Shape::Circle           # pushes Shape with radius 10
3 4 Shape::Rectangle       # pushes Shape with width 3, height 4
Shape::Point               # pushes Shape with no inner values
```

For generic enums, the type parameters are inferred from the inner values.

```casa
42 Option::Some            # pushes Option[int]
Option::None               # pushes Option[T] (generic)
"hello" Option::Some       # pushes Option[str]
```

Variants can be assigned to variables:

```casa
Color::Blue = my_color
42 Option::Some = maybe_int
```

## Comparison

Enum values of the same type can be compared with all comparison operators (`==`, `!=`, `<`, `<=`, `>`, `>=`). Ordering is based on the declaration order of variants (0-based ordinal). For data-carrying enums, comparison checks the variant tag only (not inner values).

**Stack effect:** `EnumName EnumName -> bool`

```casa
Color::Red Color::Red == print     # true
Color::Red Color::Blue != print    # true
Color::Blue Color::Red < print     # true (Red < Blue, ordinal 0 < 2)
Color::Red Color::Blue > print     # true (Blue > Red, ordinal 2 > 0)
```

Comparing values of different enum types is a compile-time error:

```casa
# ERROR: cannot compare Color and Direction
Color::Red Direction::North ==
```

Comparing an enum value with a non-enum type (e.g. `int`) is also a compile-time error.

## Variant Checking with `is`

The `is` keyword checks whether an enum value is a specific variant. It consumes the enum value and pushes a `bool`.

**Stack effect:** `EnumName -> bool`

```casa
Color::Red = color
color Color::Red is print     # true
color Color::Blue is print    # false
```

### Destructuring with `is`

Inside `if`/`elif` conditions, `is` can destructure inner values into bindings. The bindings are available in the corresponding `then`-block.

```casa
enum Shape {
    Circle(int)
    Rectangle(int int)
    Point
}

10 Shape::Circle = shape

if shape Shape::Circle(radius) is then
    radius print
fi
```

Use `elif` to check multiple variants:

```casa
if shape Shape::Circle(r) is then
    r print
elif shape Shape::Rectangle(w h) is then
    w h * print
fi
```

For generic enums, binding types are inferred from the concrete enum type:

```casa
42 Option::Some = maybe
if maybe Option::Some(value) is then
    value print    # value has type int
fi
```

Using `is` with bindings outside of `if`/`elif` conditions is a compile-time error:

```casa
# ERROR: `is` with bindings is only allowed in `if`/`elif` conditions
shape Shape::Circle(r) is
```

## Printing

Printing an enum value outputs its ordinal (0-based index in the declaration order).

```casa
Color::Red print     # 0
Color::Green print   # 1
Color::Blue print    # 2
```

## Match

Enums support exhaustive pattern matching with `match`/`end`. Variants with inner values can be destructured into bindings. All variants must be covered, or a wildcard `_` arm must be present.

```casa
enum Color { Red Green Blue }

Color::Green match
    Color::Red => "red" print
    Color::Green => "green" print
    Color::Blue => "blue" print
end
```

See [Control Flow -- Match](control-flow.md#match) for full syntax, destructuring, wildcard arms, match as expression, and exhaustiveness rules.

## Enums in Functions

Enum types work as function parameters and return types.

```casa
enum Direction { North South East West }

fn is_vertical d:Direction -> bool {
    d match
        Direction::North => true
        Direction::South => true
        Direction::East => false
        Direction::West => false
    end
}

Direction::North is_vertical print   # true
```

## Enums in Conditionals

Enum values can be used in `if` conditions with comparison operators:

```casa
Color::Red = my_color

if my_color Color::Red == then
    "it is red" print
fi
```

## Complete Example

```casa
enum Color { Red Green Blue }

# Variant constructor
Color::Green = color

# Comparison
color Color::Green == print    # true

# Match as expression
color match
    Color::Red => "red"
    Color::Green => "green"
    Color::Blue => "blue"
end
print    # green

# Print ordinal
Color::Blue print    # 2
```

## Variant Count

Using the enum name as a value pushes the number of variants as an `int`.

**Stack effect:** `-> int`

```casa
enum Color { Red Green Blue }

Color print    # 3
```

This is resolved at compile time.

See [`examples/enum.casa`](../examples/enum.casa).

## See Also

- [Control Flow -- Match](control-flow.md#match) -- full match syntax, destructuring, and exhaustiveness rules
- [Traits](traits.md) -- defining and implementing traits for enum types
- [Types and Literals](types-and-literals.md) -- `Option[T]` and `Result[T E]` as built-in enums
