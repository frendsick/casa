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

Enums can derive comparison traits. Variant order follows declaration order,
and variants with the same tag compare their payloads from left to right.
Printing an enum writes its zero-based variant number. Using the enum name as a
value produces its number of variants:

```casa
enum Color derives Ord { Red Green Blue }

Color::Red Color::Blue > print    # true, because Blue follows Red
Color::Blue print                 # 2
Color print                       # 3
```

Use `derives Eq`, `derives Ord`, or `derives Hashable` to opt in. Plain enums
have no implicit comparison or hashing implementation. See the
[derivation rules](traits.md#derive-standard-traits).

Only payload-free enums can currently derive `Copy`. Payload enums use managed
indirection and can derive `Clone` for explicit independent duplication. Clone
can be customized. See
[Copy and Clone](traits.md#copy-and-clone).

See [`examples/enum.casa`](../examples/enum.casa) for more runnable examples.
