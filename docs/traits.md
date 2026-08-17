# Traits

Traits define a set of required methods that a type must implement. Generic
functions can require that a type variable satisfies a trait, and the compiler
verifies this at each call site.

## Defining a Trait

Use the `trait` keyword to declare a trait with one or more method declarations. Required methods have no body; default methods optionally include a body (see [Default Methods](#default-methods)). Use `self` as a placeholder for the implementing type.

```casa
trait Eq {
    fn eq self:self other:self -> bool
    fn ne self:self other:self -> bool { other self.eq ! }
}

trait Word { }

trait Hashable: Eq + Word {
    fn hash self:self -> i64
}
```

This declares the minimum equality and hashing methods used by the language and
standard library.

## Implementing a Trait

Implement a trait after the receiver type with `impl Type: Trait`. The compiler checks that the block provides all required methods with matching stack effects.

```casa
impl str: Hashable {
    fn hash self:str -> i64 { self str_hash }
    # str::eq is already defined in the standard library
}

impl i64: Hashable {
    fn hash self:i64 -> i64 { self int_hash }
    fn eq self:i64 other:i64 -> bool { self other == }
}
```

The compiler checks that `str::hash` and `str::eq` have the stack effects that `Hashable` requires, with `self` replaced by the implementing type. If a method is missing or has the wrong stack effect, the declaration is rejected. An ordinary `impl str { ... }` block defines inherent methods but does not implement a trait.

A block can implement multiple traits with `+`:

```casa
impl Item: Eq + Display { ... }
```

Methodless marker traits and traits with only default methods still require an explicit declaration. Use an empty block when no method body is required:

```casa
impl Item: Marker { }
```

## Custom Types

User-defined structs can implement traits by declaring the trait and its required methods:

```casa
struct Point {
    x: i64
    y: i64
}

impl Point: Hashable {
    fn hash self:Point -> i64 {
        self.x 31 * self.y +
    }
    fn eq self:Point other:Point -> bool {
        self.x other.x == self.y other.y == &&
    }
}
```

`Point` now satisfies `Hashable` and can be used as a `Map` key or `Set` element.

## Auto-derived `Hashable` for Payload-Free Enums

Enums whose variants carry no inner values automatically satisfy `Hashable` — no manual `impl` block is needed. The compiler synthesizes `hash` from the variant discriminant and `eq` from `==`:

```casa
enum Color { Red Green Blue }

# Works without writing impl Color: Hashable { fn hash ... fn eq ... }
Map[Color i64]::new = scores
10 Color::Red scores.set = scores
```

Enums with payload-bearing variants (`Some(T)`, `Circle(i64)`, etc.) are not auto-derived; for those, write an explicit `impl` if needed.

A user-written `impl` always wins. If you define `Color::hash` or `Color::eq` manually, the synthesized version is suppressed and your implementation is used.

## Trait Bounds

Functions and `impl` blocks declare trait bounds on type variables using the `K: TraitName` syntax inside square brackets. Separate multiple bounds on one type variable with `+`. Separate type variables with commas.

### On Functions

```casa
fn example[K: Hashable] key:K -> i64 {
    key K::hash
}
```

Every bound must be satisfied:

```casa
fn show_twice[T: Copy + Display] value:T {
    value dup print print
}
```

Equivalent duplicate bounds are normalized. If two bounds declare the same method with incompatible stack effects, the generic declaration is rejected.

Here `K` must satisfy `Hashable`. The compiler verifies this at every call site by checking that the concrete type bound to `K` has the required methods.

Type variables without bounds have no restrictions:

```casa
fn identity[T] x:T -> T { x }
```

### On `impl` Blocks

`impl` blocks can declare trait bounds that are available to all methods. This avoids repeating the same bounds on every method. See [Structs and Methods](structs-and-methods.md) for details.

```casa
impl[K: Hashable, V] Map[K V] {
    fn get self:Map[K V] key:K -> Option[V] {
        key K::hash self.capacity % = idx
        ...
    }
}
```

Trait bounds belong on `impl` blocks, not on struct definitions. Structs only declare bare type parameters:

```casa
struct Set[K] { map: Map[K i64] }       # correct
# struct Set[K: Hashable] { ... }       # error
impl[K: Hashable] Set[K] { ... }        # bounds go here
```

## Trait Implementation Rules

A trait implementation must be declared in the module that defines either the
type or the trait. A third module cannot implement an imported trait for an
imported type. This restriction is the orphan rule.

Each receiver and fully instantiated trait pair can have only one implementation.
Distinct instantiations such as `Marker[i64]` and `Marker[str]` can coexist.
Overlapping generic implementations and `impl[T] T: Trait` implementations for
every type are rejected.

## Calling Trait Methods

Inside a trait-bounded function, there are two ways to call a trait method.

### Namespace syntax

Use `K::method` to call a trait method on a value:

```casa
fn example[K: Hashable] key:K -> i64 {
    key K::hash
}
```

### Dot syntax

Dot syntax also works when the receiver is a trait-bounded type variable:

```casa
fn example[K: Hashable] key:K -> i64 {
    key.hash
}
```

Both forms are equivalent. The compiler resolves them to the correct method for the concrete type at each call site.

An inherent method wins over trait defaults. If more than one trait implementation
supplies a compatible candidate, qualify the call with the trait:

```casa
value First::render
value Second::render
```

The compiler rejects an ambiguous unqualified call and lists the available trait qualifiers.

## Trait Method References

Use `&K::method` to push a trait method as a function pointer without calling it:

```casa
fn get_hasher[K: Hashable] -> fn[K -> i64] {
    &K::hash
}
```

This pushes the function pointer for the concrete type's method.

Use the receiver and trait names when more than one trait method pointer is available:

```casa
&Token::Convert[i64]::convert
```

## Auto-Injection at Call Sites

When calling a function with trait bounds, the compiler automatically injects the correct function pointers. You do not need to pass them manually.

```casa
Map[str i64]::new = m
```

The explicit `Map[str i64]` arguments bind `K=str` and `V=i64`. The compiler
then verifies that `str` satisfies `Hashable` and injects `&str::hash` and
`&str::eq` behind the scenes.

## Built-in Trait: `Eq`

Equality comparison. The required method is `eq`; the trait provides a default `ne` implemented as `!eq`.

```casa
trait Eq {
    fn eq self:self other:self -> bool
    fn ne self:self other:self -> bool { other self.eq ! }
}
```

Built-in implementations: `i64`, `u64`, `bool`, `char`, `str`, `cstr`, `ptr`.

A type satisfies `Eq` by declaring `impl Type: Eq` and providing `Type::eq self:Type other:Type -> bool`. The `ne` default is available for any type that implements `Eq`, so `x.ne y` works without writing it.

The `==` and `!=` operators are bounded by `Eq`. Built-in primitives and enums get direct bytecode comparison, but a user-defined struct used with `==` must declare `impl T: Eq { fn eq ... }`. The operator then lowers to `T::eq`. Comparing values whose type does not satisfy `Eq` is a compile-time error.

## Built-in Trait: `Ord`

Total ordering. The required method is `lt`; the defaults `le`, `gt`, and `ge` are derived from it.

```casa
trait Ord {
    fn lt self:self other:self -> bool
    fn le self:self other:self -> bool { self other.lt ! }
    fn gt self:self other:self -> bool { self other.lt }
    fn ge self:self other:self -> bool { other self.lt ! }
}
```

Built-in implementations: `i64`, `char`. Lexicographic ordering for `str` is intentionally out of scope.

The `<`, `<=`, `>`, and `>=` operators are bounded by `Ord`. Built-in primitives (excluding `str`) and enums use direct bytecode ordering. User-defined types must declare `impl T: Ord { fn lt ... }`, and the operator lowers to the corresponding trait method (`lt`, `le`, `gt`, `ge`). A type that implements `Eq` but does not satisfy `Ord` is rejected at compile time when used with an ordering operator.

## Built-in Trait: `Word`

Marker trait for register-sized values that fit in one stack slot. It declares no methods:

```casa
trait Word { }
```

It is used as a bound on builtins that require single-slot operands, such as syscall and `store*` arguments. Standard single-slot types implement `Word`. User-defined types must also implement it when needed. Multi-slot value types cannot validly implement it.

`Hashable` and `Display` both extend `Word` as supertraits, so any type that satisfies one of them automatically satisfies `Word`.

## Built-in Trait: `Hashable`

The standard library defines the `Hashable` trait as an extension of `Eq` and `Word`. Any `Hashable` type therefore also satisfies both `Eq` and `Word` (see [Supertraits](#supertraits)). The trait declares only `hash`; equality is reused from `Eq`:

```casa
trait Hashable: Eq + Word {
    fn hash self:self -> i64
}
```

Built-in implementations:
- `str::hash` uses the djb2 hash algorithm (via `str_hash`)
- `i64::hash` returns the absolute value (via `int_hash`)
- `Eq::eq` for `str` and `i64` is provided by their respective `impl` blocks

## Built-in Trait: `Display`

The standard library defines a `Display` trait used by f-string interpolation to convert values to strings. `Display` extends `Word`, so any displayable type also satisfies `Word`:

```casa
trait Display: Word {
    fn to_str self:self -> str
}
```

Any type that implements `Display` must provide a `to_str self:T -> str` method. The standard library provides implementations for `i64`, `u64`, `f32`, `f64`, `bool`, `str`, `char`, `cstr`, `ptr`, and generic containers `array[T]`, `List[T]`, `Option[T]`, and `Result[T E]`. The parameter types must themselves satisfy `Display`.

When an expression appears inside an f-string (`f"value: {x}"`), the compiler verifies that its type satisfies `Display` and automatically calls the corresponding `to_str` method. Custom structs and enums become interpolatable by implementing `Display` and providing `to_str`:

```casa
struct Point { x: i64 y: i64 }

impl Point: Display {
    fn to_str self:Point -> str {
        f"Point({self.x}, {self.y})"
    }
}

1 2 Point = origin
f"origin = {origin}\n" print    # origin = Point(1, 2)
```

## Supertraits

A trait can require its implementors to also satisfy one or more *supertraits*. Declare supertraits with `:` after the trait name (and optional type parameters), separating multiple supertraits with `+`:

```casa
trait Eq {
    fn eq self:self other:self -> bool
}

trait Word { }

trait Hashable: Eq + Word {
    fn hash self:self -> i64
}
```

Multiple supertraits are listed with `+`. Implementing `Hashable` also satisfies its implied `Eq` and `Word` supertraits. The type must provide `eq` from `Eq` and `hash` from `Hashable`. `Word` is a marker with no method requirement.

Trait-bounded code may call methods declared by any supertrait directly. For example, inside a function bounded by `K: Hashable`, both `K::hash` and `K::eq` resolve correctly.

Supertrait names can refer to traits declared later in the same module. The compiler rejects undefined supertraits, inheritance cycles, generic arity errors, and inherited methods with incompatible stack effects. A diamond inherits one shared declaration or default only once.

## Default Methods

Traits can provide default method implementations. A default method has a body in the trait definition and is available to any type that implements the trait and provides the required methods. Default methods can call the required methods using `self`.

```casa
trait Iterable[T] {
    fn next self:self -> Option[T]

    fn collect self:self -> List[T] {
        List[T]::new = iter_result
        for iter_elem in self do
            iter_elem iter_result.push
        done
        iter_result
    }

    fn count self:self -> u64 {
        0 = iter_count
        for iter_elem in self do
            1 += iter_count
        done
        iter_count
    }
}
```

Here `next` is the only required method. `collect` and `count` are default methods: any type that implements `next` returning `Option[T]` automatically gets `collect` and `count` without writing them.

One compatible inherited default satisfies matching requirements. Two unrelated defaults with the same name are ambiguous. Add an inherent method or use a trait-qualified call to select the intended behavior.

## Language Trait Contracts

The compiler recognizes `Eq`, `Ord`, `Hashable`, `Display`, and `Iterable` only
when their effective declarations contain the required methods with the correct
stack effects. These methods can be inherited. A malformed declaration is
rejected at its declaration and identifies the missing or incompatible method.

These traits can add supertraits and default methods. They cannot add other
bodyless requirements because compiler-provided primitive behavior cannot
implement unknown methods. Primitive comparison, printing, and formatting remain
available without importing the standard library.

### Built-in Trait: `Iterable[T]`

The standard library defines the `Iterable[T]` trait for iteration. A type that implements `Iterable[T]` and provides `next self:self -> Option[T]` gains all default methods.

**Required method:**

| Method | Stack effect | Description |
|--------|-------------|-------------|
| `next` | `self -> Option[T]` | Return the next element, or `Option::None` when exhausted |

**Default methods:**

| Method | Stack effect | Description |
|--------|-------------|-------------|
| `collect` | `self -> List[T]` | Collect all elements into a `List[T]` |
| `map` | `self fn[T -> U] -> Iter[U]` | Lazily apply a function to each element |
| `filter` | `self fn[T -> bool] -> Iter[T]` | Lazily keep elements for which the function returns `true` |
| `fold` | `self U fn[U T -> U] -> U` | Reduce to a single value using an accumulator |
| `count` | `self -> u64` | Count the number of elements |
| `any` | `self fn[T -> bool] -> bool` | Return `true` if any element satisfies the predicate |
| `all` | `self fn[T -> bool] -> bool` | Return `true` if all elements satisfy the predicate |
| `find` | `self fn[T -> bool] -> Option[T]` | Return the first element satisfying the predicate |

The standard library `Iter[T]` struct (returned by `.iter` on `array[T]`, `List[T]`, and `str`) satisfies `Iterable[T]`. See [Standard Library](standard-library.md) for details on `Iter` and the default methods.

## Errors

### `MISSING_TRAIT_METHOD`

Reported when a trait-dependent operation is used on a type that does not implement the required trait.

```
error[MISSING_TRAIT_METHOD]: Type `Foo` does not satisfy trait `Hashable`
```

### `TRAIT_SIGNATURE_MISMATCH`

Reported when a type has a method with the right name but the wrong stack effect.

```
error[TRAIT_SIGNATURE_MISMATCH]: Method signature does not match trait requirement
```

## See Also

- [Structs and Methods](structs-and-methods.md) - `impl` blocks where trait methods are defined
- [Functions and Lambdas](functions-and-lambdas.md) - generic functions and trait bounds
- [Collections](collections.md) - Map and Set require the `Hashable` trait
- [Strings and IO](strings-and-io.md) - type conversions using the `Display` trait
