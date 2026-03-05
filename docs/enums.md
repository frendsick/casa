# Enums

Enums define a type with a fixed set of named variants.

## Defining an Enum

Use the `enum` keyword followed by the type name and a list of variants in braces.

```casa
enum Color { Red Green Blue }
enum Direction { North South East West }
```

Each variant is a distinct value of the enum type. An enum must have at least one variant. Variant names must be unique within the enum.

## Variant Constructors

Access variants using the `EnumName::VariantName` syntax.

**Stack effect:** `-> EnumName`

```casa
Color::Red        # pushes a Color value
Direction::North  # pushes a Direction value
```

Variants can be assigned to variables:

```casa
Color::Blue = my_color
```

## Comparison

Enum values of the same type can be compared with `==` and `!=`.

**Stack effect:** `EnumName EnumName -> bool`

```casa
Color::Red Color::Red == print     # true
Color::Red Color::Blue != print    # true
```

Comparing values of different enum types is a compile-time error:

```casa
# ERROR: cannot compare Color and Direction
Color::Red Direction::North ==
```

Comparing an enum value with a non-enum type (e.g. `int`) is also a compile-time error.

## Printing

Printing an enum value outputs its ordinal (0-based index in the declaration order).

```casa
Color::Red print     # 0
Color::Green print   # 1
Color::Blue print    # 2
```

## Match Statement

The `match` keyword provides exhaustive pattern matching on enum values. It consumes the enum value from the stack and executes the matching arm.

### Syntax

```
<enum_value> match
    EnumName::Variant1 => <body>
    EnumName::Variant2 => <body>
    ...
end
```

The `match` keyword pops the enum value from the stack. Each arm specifies a variant pattern followed by `=>` and a body. The block is closed with `end`.

### Basic Example

```casa
enum Color { Red Green Blue }

Color::Green match
    Color::Red => "red" print
    Color::Green => "green" print
    Color::Blue => "blue" print
end
# prints: green
```

### Exhaustiveness

All variants must be covered. Missing a variant is a compile-time error:

```casa
# ERROR: Non-exhaustive match, missing arms: `Color::Blue`
Color::Red match
    Color::Red => "red" print
    Color::Green => "green" print
end
```

Duplicate arms are also compile-time errors.

### Wildcard Arm

The `_ =>` wildcard arm matches any remaining variants. When used, it must be the last arm. No further arms are allowed after it.

```casa
enum Direction { North South East West }

Direction::North match
    Direction::North => "up" print
    _ => "not up" print
end
```

The wildcard satisfies exhaustiveness on its own, so you do not need to list every variant:

```casa
# Only handle one variant explicitly, catch the rest
color match
    Color::Red => "red" print
    _ => "other" print
end
```

Arms after a wildcard are unreachable and produce a compile-time error:

```casa
# ERROR: unreachable match arm after wildcard `_`
color match
    _ => "any" print
    Color::Red => "red" print
end
```

### Match as Expression

Match arms can leave values on the stack. All arms must produce the same stack effect (same types).

```casa
Color::Green match
    Color::Red => 10
    Color::Green => 20
    Color::Blue => 30
end
print    # 20
```

This is useful for mapping enum values to other types:

```casa
fn color_name c:Color -> str {
    c match
        Color::Red => "red"
        Color::Green => "green"
        Color::Blue => "blue"
    end
}
```

### Stack Consistency

All arms must leave the stack in the same state. The following is a compile-time error:

```casa
# ERROR: arms produce different types
Color::Red match
    Color::Red => 1
    Color::Green => "two"
    Color::Blue => 3
end
```

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
