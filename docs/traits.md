# Traits

Traits define a set of required methods that a type must implement. They enable bounded polymorphism: functions can require that a type variable satisfies a trait, and the compiler verifies this at each call site.

## Defining a Trait

Use the `trait` keyword to declare a trait with one or more method signatures. Method bodies are not allowed in trait definitions. Use `self` as a placeholder for the implementing type.

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

## Trait Bounds

Functions declare trait bounds on type variables using the `K: TraitName` syntax inside square brackets. Multiple type variables are separated by commas.

```casa
fn get[K: Hashable, V] self:Map[K V] key:K -> option[V] {
    key K::hash self.capacity % = idx
    ...
}
```

Here `K` must satisfy `Hashable`. The compiler verifies this at every call site by checking that the concrete type bound to `K` has the required methods.

Type variables without bounds have no restrictions:

```casa
fn keys[K V] self:Map[K V] -> List[K] { ... }
```

Here `K` and `V` can be any type.

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
