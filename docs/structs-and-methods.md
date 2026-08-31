# Structs and Methods

A struct groups named fields into one type.

## Define and construct a struct

```casa
struct Person {
    name: str
    age: i64
}

Person { name: "Ada" age: 36 } = person
```

A named struct literal must provide every field. Fields can appear in any
order, and each value can be an expression:

```casa
Person { age: 35 1 + name: "Ada" } = person
```

## Read and assign fields

Use dot syntax to read a field or assign through a named binding:

```casa
person.name print
37 = person.age
1 += person.age
```

Nested assignment is valid when each field exists:

```casa
"Helsinki" = employee.address.city
```

Loans to different named fields can be used together. A loan to the complete
struct overlaps every field loan. Nested fields overlap when one path contains
the other.

Every field also has generated `Type::field` and `Type::set_field` functions.
Dot syntax is the usual form:

```casa
person Person::name print
"Grace" person Person::set_name
```

## Add methods

An `impl` block adds methods to a type:

```casa
impl Person {
    fn birthday self:mut$Person {
        1 += self.age
    }

    fn description self:$Person -> String {
        f"{self.name}, age {self.age}"
    }
}

person.birthday
person.description print
```

The first parameter is normally the receiver and is named `self`. A method can
also be called by its qualified name:

```casa
person Person::description print
```

A type can have more than one `impl` block. Built-in types can also have
methods.

An `impl` block can target one concrete generic type. Concrete impls can use
the same method name because each full receiver type has a separate method:

```casa
impl Box[i64] {
    fn label self:$Box[i64] -> str { "integer" }
}

impl Box[bool] {
    fn label self:$Box[bool] -> str { "boolean" }
}
```

Casa checks the concrete receiver before a generic impl such as `impl[T]
Box[T]`. Use the full receiver in a qualified call or function reference, such
as `box Box[i64]::label` or `&Box[i64]::label`. Method visibility is unchanged.
A private concrete method is available only in its defining module.

## C-layout extern structs

Use `extern struct` only when a struct field layout is part of a C ABI contract.
Extern structs keep normal Casa construction, fields, imports, visibility, and
methods. Their allowed field types and native pointer use are documented under
[Extern functions](./functions-and-lambdas.md#extern-functions).

Ordinary `struct` declarations keep a compiler-owned layout that can change
between compiler versions.

The declared receiver controls which values can call a method:

| Receiver | Owned `T` | Shared `$T` | Exclusive `mut$T` |
|---|---|---|---|
| `self:T` | Yes, and consumes it | No | No |
| `self:$T` | Yes | Yes | Yes, through a shared reborrow |
| `self:mut$T` | Yes | No | Yes |

Method lookup checks the exact value type before it checks the borrowed type.
Shared borrows do not implement Clone. When `Person` implements Clone, `.clone`
on `$Person` or `mut$Person` calls that implementation and produces a new
owner:

```casa
person.clone
```

See [Traits](traits.md) for generic structs, generic `impl` blocks, and trait
implementations.

A struct or enum can contain a borrow, including through a generic field. The
aggregate keeps the borrowed owner loaned until the aggregate's last use. A
function that returns such an aggregate preserves the same origin.

## Copy and Clone

Ordinary struct values use heap-indirect storage, so they cannot implement
`Copy`. Derive `Clone` when fieldwise independent duplication is suitable:

```casa
struct Point derives Clone {
    x: i64
    y: i64
}
```

Define the method when the generated behavior is not suitable:

```casa
impl Point: Clone {
    fn clone self:$Point -> Point {
        self.y self.x Point
    }
}
```

Structs can also derive `Eq`, `Ord`, and `Hashable`. Generated methods process
fields in declaration order and add the required bounds for generic fields.
See [Derive standard traits](traits.md#derive-standard-traits).

An extern struct can implement `Copy` when each field implements `Copy`. Its
fixed C-layout body uses the same direct-copy storage model as a fixed array.
Payload-free enums use a raw representation and can derive `Copy`. Payload
enums and ordinary structs remain non-Copy until their value representation can
be duplicated without allocation or aliasing. Fixed arrays are `Copy` when
their elements are `Copy`. See [Copy and Clone](traits.md#copy-and-clone).

## Custom destruction

Define the reserved inherent `drop` method when a type needs custom cleanup:

```casa
impl Person {
    fn drop self:mut$Person {
        self.age print
    }
}
```

The method must have the exact stack effect `self:mut$Person -> None`. It cannot
be called or referenced directly. The compiler calls it when an owner is
destroyed. A type with this method cannot implement `Copy`.

Casa destroys owners in reverse acquisition order on normal scope exits,
returns, and loop exits. It calls a custom `drop` method first, then destroys
the fields in reverse declaration order. The `drop` intrinsic starts the same
process immediately.

## Alternative stack constructor

For compact stack-oriented code, push fields in reverse declaration order and
call the struct name:

```casa
36 "Ada" Person = person
```

The named literal is easier to read when a struct has several fields.

## Struct patterns

Use a struct pattern to bind selected fields in `match`:

```casa
person match
    Person { name: name age: age } => f"{name} is {age}" print
end
```

Partial patterns are allowed. See
[Control Flow and Patterns](control-flow.md#match-a-value) for binding scope,
stack consistency, and exhaustiveness.

See [`examples/struct.casa`](../examples/struct.casa) and
[`examples/destruction.casa`](../examples/destruction.casa) for runnable
examples.
