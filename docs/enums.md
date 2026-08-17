# Enums

An enum defines a closed set of variants. A variant can carry values.

## Define an enum

```casa
enum Shape {
    Circle(i64)
    Rectangle(i64 i64)
    Point
}
```

An enum must have at least one variant. Variant names are unique within the
enum.

Enums can have type parameters:

```casa
enum Option[T] { None Some(T) }
enum Result[T E] { Error(E) Ok(T) }
```

See [Traits](traits.md) for the general rules for type parameters.

## Construct a variant

Use `Enum::Variant`. Push carried values before the constructor:

```casa
Shape::Point
10 Shape::Circle
3 4 Shape::Rectangle
```

The carried values determine generic type parameters when possible:

```casa
42 Option::Some          # Option[i64]
"not found" Result::Error
```

An empty generic variant often needs context:

```casa
Option::None = result:Option[i64]
```

## Process an enum

Use `is` for one conditional variant check. Use `match` when each variant needs
its own behavior:

```casa
fn area shape:Shape -> i64 {
    shape match
        Shape::Circle(radius) => radius radius *
        Shape::Rectangle(width height) => width height *
        Shape::Point => 0
    end
}
```

See [Control Flow and Patterns](control-flow.md#test-and-bind-an-enum-variant)
for `is`, pattern bindings, guards, and exhaustive `match`.

## Other enum operations

Enums can be compared when their types match. Variant order follows declaration
order. Printing an enum writes its zero-based variant number. Using the enum
name as a value produces its number of variants:

```casa
enum Color { Red Green Blue }

Color::Red Color::Blue > print    # true, because Blue follows Red
Color::Blue print                 # 2
Color print                       # 3
```

Payload-free enums automatically implement `Hashable`. See
[Traits](traits.md#auto-derived-hashable-for-payload-free-enums) for use as map
keys or set elements.

See [`examples/enum.casa`](../examples/enum.casa) for more runnable examples.
