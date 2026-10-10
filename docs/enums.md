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
enum Option[T] {
    None
    Some(T)
}

enum Result[T E] {
    Error(E)
    Ok(T)
}
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
42 Option::Some # Option[i64]
"not found" Result::Error
```

An empty generic variant often needs context:

```casa
Option::None = result: Option[i64]
```

A directly bound payload enum can use function-local storage without implementing
Copy when its uses prove that the storage cannot escape. Tag tests and local
destruction qualify. Payload bindings and uncertain uses retain heap storage.
Structs and enums share the [function-local storage budget](structs-and-methods.md).

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
import "std"

enum Color derives std::Ord {
    Red
    Green
    Blue
}

Color::Red Color::Blue < print # true, because Red precedes Blue
Color::Blue print # 2
Color print # 3
```

Use `derives Eq`, `derives Ord`, or `derives Hashable` to opt in. Plain enums
have no implicit comparison or hashing implementation. See the
[derivation rules](traits.md#derive-standard-traits).

Enums can derive `Copy` when every owned payload field is Copy and all stored
borrows are shared. Payload enums then copy the complete tag and payload into
independent storage. Generic conformance follows the payload bounds. Enums with
owned non-Copy payloads can derive `Clone` for explicit duplication. For custom
Clone behavior, omit the derive and write an explicit implementation. See
[Copy and Clone](traits.md#copy-and-clone).

See [examples/enum.casa](../examples/enum.casa) for more runnable examples.
