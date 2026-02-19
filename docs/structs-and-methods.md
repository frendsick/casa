# Structs and Methods

## Defining a Struct

Structs group named, typed fields into a single type.

```casa
struct Person {
    name: str
    age:  int
}
```

## Instantiation

Push fields onto the stack in reverse declaration order (last declared field deepest, first declared field on top), then call the struct name. Fields are popped from the top in declaration order.

**Stack effect:** `fieldN ... field2 field1 -> StructName`

```casa
18 "John Doe" Person = person
```

Here `18` (`age`, last declared) is pushed first and sits deepest. `"John Doe"` (`name`, first declared) is pushed second and sits on top. The struct constructor pops fields in declaration order: `name` first (from top), then `age`.

## Auto-Generated Accessors

Every struct automatically gets getter and setter functions for each field.

### Getters

`Type::field` — pops a struct instance, pushes the field value.

**Stack effect:** `StructName -> field_type`

```casa
person Person::name print    # John Doe
person Person::age print     # 18
```

### Setters

`Type::set_field` — pops a new value and a struct instance, updates the field in place.

**Stack effect:** `value StructName -> None`

```casa
"Jane Doe" person Person::set_name
person Person::name print    # Jane Doe
```

## Dot and Arrow Syntax

Shorthand syntax makes struct access more readable.

### Dot Syntax (Getter)

`instance.field` is equivalent to `instance Type::field`.

```casa
person.name print    # same as: person Person::name print
person.age print     # same as: person Person::age print
```

The type checker resolves the receiver type automatically — you do not need to write the struct name.

### Arrow Syntax (Setter)

`value instance->field` is equivalent to `value instance Type::set_field`.

```casa
"Jane Doe" person->name    # same as: "Jane Doe" person Person::set_name
```

## `impl` Blocks

Add methods to a type with `impl`:

```casa
impl Person {
    fn celebrate_birthday self:Person {
        self.age 1 + self->age
    }
}
```

Methods are namespaced as `TypeName::method`. The first parameter is typically the instance (named `self` by convention, but any name works).

### Calling Methods

Full syntax:

```casa
person Person::celebrate_birthday
```

Dot syntax (preferred):

```casa
person.celebrate_birthday
```

### Example

```casa
person.celebrate_birthday
person.age print    # 19
```

### Multiple `impl` Blocks

A type can have multiple `impl` blocks, even across different files:

```casa
impl Person {
    fn greet self:Person {
        self.name print
    }
}

impl Person {
    fn is_adult self:Person -> bool {
        self.age 18 <= !
    }
}
```

## `impl` on Built-In Types

`impl` blocks work on any type, including built-in types like `int`, `str`, `bool`, and `ptr`:

```casa
impl int {
    fn double  int -> int { 2 * }
    fn add_one int -> int { 1 + }
}

20.add_one.double print    # 42
```

See [`examples/method_chain.casa`](../examples/method_chain.casa).

## Complete Example

```casa
struct Person {
    name: str
    age:  int
}

impl Person {
    fn celebrate_birthday self:Person {
        self.age 1 + self->age
    }
}

# Instantiate
18 "John Doe" Person = person

# Getters
person.name print       # John Doe
person.age print        # 18

# Setters
"Jane Doe" person->name
person.name print       # Jane Doe

# Methods
person.celebrate_birthday
person.age print        # 19
```

See [`examples/struct.casa`](../examples/struct.casa).
