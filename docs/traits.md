# Traits

Traits define a set of required methods that a type must implement. They enable bounded polymorphism: functions can require that a type variable satisfies a trait, and the compiler verifies this at each call site.

## Defining a Trait

Use the `trait` keyword to declare a trait with one or more method signatures. Required methods have no body; default methods optionally include a body (see [Default Methods](#default-methods)). Use `self` as a placeholder for the implementing type.

```casa
trait Hashable {
    fn hash self:self -> int
    fn eq self:self other:self -> bool
}
```

This declares that any type satisfying `Hashable` must have a `hash` method (taking self, returning `int`) and an `eq` method (taking self and another value of the same type, returning `bool`).

## Implementing a Trait

Traits use structural satisfaction. A type satisfies a trait when its `impl` block contains all required methods with matching signatures. There is no `impl Trait for Type` syntax.

```casa
impl str {
    fn hash self:str -> int { self str_hash }
    # str::eq is already defined in the standard library
}

impl int {
    fn hash self:int -> int { self int_hash }
    fn eq self:int other:int -> bool { self other == }
}
```

The compiler checks that `str::hash` and `str::eq` exist with signatures matching what `Hashable` requires (with `self` replaced by `str`). If any method is missing or has a wrong signature, a compile-time error is reported.

## Custom Types

User-defined structs can satisfy traits by implementing the required methods:

```casa
struct Point {
    x: int
    y: int
}

impl Point {
    fn hash self:Point -> int {
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

# Works without writing impl Color { fn hash ... fn eq ... }
Map::new(Map[Color int]) = scores
Color::Red 10 scores.set = scores
```

Enums with payload-bearing variants (`Some(T)`, `Circle(int)`, etc.) are not auto-derived; for those, write an explicit `impl` if needed.

A user-written `impl` always wins. If you define `Color::hash` or `Color::eq` manually, the synthesized version is suppressed and your implementation is used.

## Trait Bounds

Functions and `impl` blocks declare trait bounds on type variables using the `K: TraitName` syntax inside square brackets. Multiple type variables are separated by commas.

### On Functions

```casa
fn example[K: Hashable] key:K -> int {
    key K::hash
}
```

Here `K` must satisfy `Hashable`. The compiler verifies this at every call site by checking that the concrete type bound to `K` has the required methods.

Type variables without bounds have no restrictions:

```casa
fn identity[T] x:T -> T { x }
```

### On `impl` Blocks

`impl` blocks can declare trait bounds that are inherited by all methods. This avoids repeating the same bounds on every method. See [Structs and Methods](structs-and-methods.md) for details.

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
struct Set[K] { map: Map[K int] }       # correct
# struct Set[K: Hashable] { ... }       # error
impl[K: Hashable] Set[K] { ... }        # bounds go here
```

## Calling Trait Methods

Inside a trait-bounded function, there are two ways to call a trait method.

### Namespace syntax

Use `K::method` to call a trait method on a value:

```casa
fn example[K: Hashable] key:K -> int {
    key K::hash
}
```

### Dot syntax

Dot syntax also works when the receiver is a trait-bounded type variable:

```casa
fn example[K: Hashable] key:K -> int {
    key.hash
}
```

Both forms are equivalent. The compiler resolves them to the correct method for the concrete type at each call site.

## Trait Method References

Use `&K::method` to push a trait method as a function pointer without calling it:

```casa
fn get_hasher[K: Hashable] -> fn[K -> int] {
    &K::hash
}
```

This pushes the function pointer for the concrete type's method.

## Auto-Injection at Call Sites

When calling a function with trait bounds, the compiler automatically injects the correct function pointers. You do not need to pass them manually.

```casa
Map::new (Map[str int]) = m
```

The compiler sees that `Map::new` requires `[K: Hashable, V]`, determines `K=str` from the type cast `(Map[str int])`, verifies that `str` satisfies `Hashable`, and injects `&str::hash` and `&str::eq` behind the scenes.

## Built-in Trait: `Eq`

Equality comparison. The required method is `eq`; the trait provides a default `ne` implemented as `!eq`.

```casa
trait Eq {
    fn eq self:self other:self -> bool
    fn ne self:self other:self -> bool { other self.eq ! }
}
```

Built-in implementations: `int`, `bool`, `char`, `str`, `cstr`, `ptr`.

A type satisfies `Eq` by providing `Type::eq self:Type other:Type -> bool`. The `ne` default is auto-instantiated for any satisfying type, so `x.ne y` works without writing it.

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

Built-in implementations: `int`, `char`. Lexicographic ordering for `str` is intentionally out of scope.

## Built-in Trait: `Word`

Marker trait for register-sized values that fit in one stack slot. It declares no methods:

```casa
trait Word { }
```

It is used as a bound on builtins that require single-slot operands (for example, syscall and `store*` arguments). Primitive types, enums, struct refs, and array refs satisfy `Word`; multi-slot value types do not.

## Built-in Trait: `Hashable`

The standard library defines the `Hashable` trait and provides implementations for `str` and `int`:

```casa
trait Hashable {
    fn hash self:self -> int
    fn eq self:self other:self -> bool
}
```

Built-in implementations:
- `str::hash` uses the djb2 hash algorithm (via `str_hash`)
- `str::eq` compares strings by content
- `int::hash` returns the absolute value (via `int_hash`)
- `int::eq` compares integers with `==`

## Built-in Trait: `Display`

The standard library defines a `Display` trait used by f-string interpolation to convert values to strings:

```casa
trait Display {
    fn to_str self:self -> str
}
```

Any type with a `to_str self:T -> str` method structurally satisfies `Display`. The standard library provides implementations for `int`, `bool`, `str`, `char`, `cstr`, `ptr`, and generic containers `array[T]`, `List[T]`, `Option[T]`, and `Result[T E]` (the parameter types must themselves satisfy `Display`).

When an expression appears inside an f-string (`f"value: {x}"`), the compiler verifies that its type satisfies `Display` and automatically calls the corresponding `to_str` method. Custom structs and enums become interpolatable simply by providing a `to_str` method:

```casa
struct Point { x: int y: int }

impl Point {
    fn to_str self:Point -> str {
        f"Point({self.x}, {self.y})"
    }
}

1 2 Point = origin
f"origin = {origin}\n" print    # origin = Point(1, 2)
```

## Default Methods

Traits can provide default method implementations. A default method has a body in the trait definition and is automatically available to any type that satisfies the trait (i.e. implements the required methods). Default methods can call the required methods using `self`.

```casa
trait Iterable[T] {
    fn next self:self -> Option[T]

    fn collect self:self -> List[T] {
        List::new (List[T]) = iter_result
        for iter_elem in self do
            iter_elem iter_result.push
        done
        iter_result
    }

    fn count self:self -> int {
        0 = iter_count
        for iter_elem in self do
            1 += iter_count
        done
        iter_count
    }
}
```

Here `next` is the only required method. `collect` and `count` are default methods: any type that implements `next` returning `Option[T]` automatically gets `collect` and `count` without writing them.

### Built-in Trait: `Iterable[T]`

The standard library defines the `Iterable[T]` trait for iteration. Any type with a `next self:self -> Option[T]` method structurally satisfies `Iterable[T]` and gains all default methods.

**Required method:**

| Method | Signature | Description |
|--------|-----------|-------------|
| `next` | `self:self -> Option[T]` | Return the next element, or `Option::None` when exhausted |

**Default methods:**

| Method | Signature | Description |
|--------|-----------|-------------|
| `collect` | `self:self -> List[T]` | Collect all elements into a `List[T]` |
| `map` | `self:self f:fn[T -> U] -> List[U]` | Apply a function to each element, returning a new list |
| `filter` | `self:self f:fn[T -> bool] -> List[T]` | Return a list of elements for which the function returns `true` |
| `fold` | `self:self acc:U f:fn[U T -> U] -> U` | Reduce to a single value using an accumulator |
| `count` | `self:self -> int` | Count the number of elements |
| `any` | `self:self f:fn[T -> bool] -> bool` | Return `true` if any element satisfies the predicate |
| `all` | `self:self f:fn[T -> bool] -> bool` | Return `true` if all elements satisfy the predicate |
| `find` | `self:self f:fn[T -> bool] -> Option[T]` | Return the first element satisfying the predicate |

The standard library `Iter[T]` struct (returned by `.iter` on `array[T]`, `List[T]`, and `str`) satisfies `Iterable[T]`. See [Standard Library](standard-library.md) for details on `Iter` and the default methods.

## Errors

### `MISSING_TRAIT_METHOD`

Reported when a type is used with a trait bound but does not implement all required methods.

```
error[MISSING_TRAIT_METHOD]: Type `Foo` does not satisfy trait `Hashable`
```

### `TRAIT_SIGNATURE_MISMATCH`

Reported when a type has a method with the right name but the wrong signature.

```
error[TRAIT_SIGNATURE_MISMATCH]: Method signature does not match trait requirement
```

## See Also

- [Structs and Methods](structs-and-methods.md) -- `impl` blocks where trait methods are defined
- [Functions and Lambdas](functions-and-lambdas.md) -- generic functions and trait bounds
- [Collections](collections.md) -- Map and Set require the `Hashable` trait
- [Strings and IO](strings-and-io.md) -- type conversions using the `Display` trait
