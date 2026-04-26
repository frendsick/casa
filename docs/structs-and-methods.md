# Structs and Methods

Structs group named, typed fields into a single type. Methods are added via `impl` blocks, and dot/arrow syntax provides convenient access.

## Defining a Struct

```casa
struct Person {
    name: str
    age:  int
}
```

## Struct Literals

Construct a struct with named fields inside braces. Fields can appear in any order. All fields must be specified.

```casa
Person { name: "John Doe" age: 18 } = person
```

Field order does not matter:

```casa
Person { age: 18 name: "John Doe" } = person
```

Each field value is an arbitrary expression:

```casa
Person { name: first_name " " str::concat last_name str::concat age: birth_year current_year swap - } = person
```

## Stack-Based Instantiation

Alternatively, push fields onto the stack in reverse declaration order (last declared field deepest, first declared field on top), then call the struct name.

**Stack effect:** `fieldN ... field2 field1 -> StructName`

```casa
18 "John Doe" Person = person
```

Here `18` (`age`, last declared) is pushed first and sits deepest. `"John Doe"` (`name`, first declared) is pushed second and sits on top.

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

The type checker resolves the receiver type automatically from whatever value is on top of the stack — you do not need to write the struct name. This means dot syntax works on any expression, not just variables:

```casa
18 "John Doe" Person .name print   # John Doe (dot on constructor result)
person.name print                  # John Doe (dot on variable)
```

### Arrow Syntax (Setter)

`value instance->field` is equivalent to `value instance Type::set_field`.

```casa
"Jane Doe" person->name    # same as: "Jane Doe" person Person::set_name
```

## Struct-Typed Fields

A struct field can be declared with another struct type. Reads and writes are type-checked — no manual cast at the use site.

```casa
struct Node {
    value: int
    next:  Node
}

0 (Node) 42 Node = first      # next=null, value=42
0 (Node) 99 Node = second
second first->next            # first.next now points at second
```

Self-referential and mutually recursive struct types are supported. The null sentinel for "no value" is `0 (StructName)`. For optional references, prefer `Option[StructName]`, which makes absence explicit:

```casa
struct BytecodeCompiler {
    ops:      List[Op]
    function: Option[Function]
}

if compiler BytecodeCompiler::function Option::Some(function) is then
    function Function::variables.length print
fi
```

## Generic Structs

Structs can have type parameters. These are declared in square brackets after the struct name.

```casa
struct Box[T] {
    value: T
}
```

Generic structs infer their type parameters from the values on the stack:

```casa
42 Box          # inferred as Box[int]
"hello" Box     # inferred as Box[str]
```

Auto-generated getters and setters are parameterized automatically:

```casa
42 Box .value print    # 42 — getter returns int
```

Struct-level type parameters can be used in field types:

```casa
struct Pair[A B] {
    first: A
    second: B
}

true 42 Pair    # Pair[int bool] — first=42 (top), second=true
```

Trait bounds are not allowed on struct definitions. Use `impl`-level bounds instead (see below).

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

### `impl` with Type Parameters

`impl` blocks can declare type parameters with trait bounds. These are inherited by all methods in the block, so individual methods do not need to redeclare them.

```casa
struct Set[K] {
    map: Map[K int]
}

impl[K: Hashable] Set[K] {
    fn new -> Set[K] { ... }
    fn has self:Set[K] key:K -> bool { ... }
    fn add self:Set[K] key:K -> Set[K] { ... }
}
```

All methods in this block inherit the `K: Hashable` bound. Methods can still declare additional type parameters of their own:

```casa
impl[K: Hashable] Set[K] {
    fn convert[V] self:Set[K] -> List[V] { ... }
}
```

Here `K: Hashable` comes from the `impl` block and `V` is the method's own type parameter.

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
Person { name: "John Doe" age: 18 } = person

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

## Struct Destructuring in Match

Structs can be destructured in `match` expressions. Each arm binds struct fields to local variables.

```casa
person match
    Person { name: n age: a } => n print a print
end
```

Partial destructuring is allowed — you do not need to bind every field:

```casa
person match
    Person { name: n } => n print
end
```

A single struct pattern arm is exhaustive (structs have one shape). A wildcard `_` arm is also valid:

```casa
person match
    _ => "any person" print
end
```

## Traits

Types can satisfy traits by implementing the required methods in their `impl` blocks. Trait satisfaction is structural: no special declaration is needed. See [Traits](traits.md) for details.

```casa
trait Hashable {
    fn hash self:self -> int
    fn eq self:self other:self -> bool
}

struct Point {
    x: int
    y: int
}

impl Point {
    fn hash self:Point -> int { self.x 31 * self.y + }
    fn eq self:Point other:Point -> bool {
        self.x other.x == self.y other.y == &&
    }
}

# Point now satisfies Hashable and can be used as a Map key
```

## See Also

- [Traits](traits.md) -- defining and satisfying traits in `impl` blocks
- [Enums](enums.md) -- enum types as an alternative to structs
- [Control Flow -- Match](control-flow.md#match) -- struct destructuring in match expressions
- [Collections](collections.md) -- List, Map, and Set built with structs
