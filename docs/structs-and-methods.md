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
    fn birthday self:Person {
        1 += self.age
    }

    fn description self:Person -> str {
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

See [Traits](traits.md) for generic structs, generic `impl` blocks, and trait
implementations.

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

See [`examples/struct.casa`](../examples/struct.casa) for a runnable example.
