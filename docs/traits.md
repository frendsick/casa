# Generics and Traits

Generics let one declaration work with several types. Traits state which
operations a generic type must provide.

## A generic function

Declare type parameters in brackets after a function name:

```casa
fn identity[T] value:T -> T { value }

42 identity print
"hello" identity print
```

`T` becomes `i64` at the first call and `str` at the second. Repeated uses of
the same type parameter must resolve to the same type.

Casa checks the generic body once with its declared types and trait bounds.
The compiler then creates one implementation for each reachable concrete type
combination. Each implementation uses direct layout, destruction, and trait
method operations. Generic calls do not pass runtime type or trait metadata.

Recursive generic calls must keep the same type arguments. A call cycle that
changes them is rejected as polymorphic recursion.

Use more than one type parameter when the types are independent:

```casa
fn keep_first[A, B] first:A second:B -> A {
    second drop
    first
}
```

A type parameter must occur in an input. Casa cannot choose a type that appears
only in the outputs.

## Generic data

Structs and enums can also declare type parameters:

```casa
struct Box[T] { value: T }
enum Maybe[T] { None Some(T) }

42 Box = box:Box[i64]
"hello" Maybe::Some = maybe:Maybe[str]
```

Put requirements on an `impl` block, not on a struct definition. See
[Advanced generic implementations](#advanced-generic-implementations).

## Define and implement a trait

A trait declares methods that a type promises to provide. `self` stands for
the implementing type:

```casa
trait Describe {
    fn describe self:self -> String
}

struct User { name: str }

impl User: Describe {
    fn describe self:User -> String { self.name.to_str }
}
```

The `impl User: Describe` declaration is explicit. An inherent method with the
same name does not implement the trait.

## Require a trait

Add a trait after a type parameter to restrict accepted types:

```casa
fn print_description[T: Describe] value:T {
    value.describe print
}

User { name: "Ada" } print_description
```

The call is valid only when the concrete type implements `Describe`. Use `+`
when one type parameter requires several traits:

```casa
trait Stored { }
impl User: Stored { }

fn save_description[T: Describe + Stored] value:T {
    value.describe print
}
```

## Built-in traits

The standard library defines these traits. This table is the public catalog for
their relationships and language uses.

| Trait | Requirement | Extends | Used by |
|---|---|---|---|
| `Clone` | `clone self:self -> self` | None | Explicit value duplication |
| `Copy` | No methods | `Clone` | `dup`, `over`, and `copy` |
| `PartialEq` | `eq self:$self other:$self -> bool` | None | `==` and `!=` |
| `Eq` | No new methods | `PartialEq` | Total equality |
| `PartialOrd` | `partial_cmp self:$self other:$self -> Option[Ordering]` | `PartialEq` | `<`, `<=`, `>`, and `>=` |
| `Ord` | `cmp self:$self other:$self -> Ordering` | `PartialOrd + Eq` | Total ordering |
| `Word` | No methods | None | Raw memory stores and system calls |
| `Hashable` | `hash self:self -> i64` | `Eq + Word` | `Map` keys and `Set` elements |
| `Display` | `to_str self:$self -> String` | `Word` | `print` and string interpolation |
| `Iterable[T]` | `next self:self -> Option[T]` | None | `for` loops and iterator methods |

`PartialEq` supplies `ne` from `eq`. The `!=` operator calls `ne`, so an
implementation can replace that default. `PartialOrd` supplies the four
ordering operator methods from `partial_cmp`. `Ord` supplies `partial_cmp`
from `cmp`.

`Ordering` has the variants `Less`, `Equal`, and `Greater`. `partial_cmp`
returns `None` when two values are unordered. The `f32` and `f64` types
implement `PartialEq` and `PartialOrd`, but not `Eq` or `Ord`. Integer types
and `char` implement the total traits.

`Iterable[T]` supplies the standard lazy iterator operations. See
[Collections](collections.md) for those operations.

Payload-free enums implement `Hashable` automatically. Enums with carried
values need an explicit implementation.

Primitive comparisons and printing remain available without importing `std`.

## Copy and Clone

`Copy` marks values whose representation can be duplicated without allocation
or user code. Scalars, `str` views, raw pointers, C string pointers, and named
function references are Copy. `String`, arrays, and collections are not.

Shared borrows can be duplicated with `dup` and `over`, but `$T` does not
implement or satisfy `Copy`. An exclusive `mut$T` borrow cannot be duplicated.

Payload-free enums can opt in with `derives Copy` or an empty implementation:

```casa
enum Direction derives Copy {
    North
    South
}

impl Direction: Copy { }
```

Structs and payload enums currently use heap-indirect value storage. They cannot
implement `Copy`, even when every field is Copy, because duplicating their
handle would alias one allocation. Fixed arrays store their elements directly,
but the current array rule also excludes them from `Copy`. Use `Clone` for
explicit independent duplication. When `T` implements Clone, calling `.clone`
on `$T` or `mut$T` calls the borrowed value's implementation and returns an
owned `T`.

Clone is always explicit and can allocate or run user code:

```casa
impl Document: Clone {
    fn clone self:$Document -> Document {
        self.title.clone Document
    }
}
```

`String`, `array[T N]`, `List[T]`, `Option[T]`, `Result[T E]`, `Map[K V]`, and
`Set[T]` implement Clone when their owned contents implement Clone. Cloning a
`String` allocates independent storage. Cloning a `str` copies its view.

## Advanced trait implementations

An implementation must provide every required method with the declared stack
effect. One block can implement several traits:

```casa
struct Item { id: i64 name: str }

impl Item: Describe + Stored {
    fn describe self:Item -> String { self.name.to_str }
}
```

A methodless marker trait still needs an explicit empty block unless it uses a
supported `derives` clause. A trait
implementation must be in the module that defines the type or the trait. The
same type and fully specified trait can have only one implementation.

## Advanced generic implementations

An `impl` block can declare type parameters and requirements that apply to all
its methods:

```casa
struct Box[T] { value: T }

impl[T] Box[T] {
    fn unwrap self:Box[T] -> T { self.value }
}

impl[T: Describe] Box[T]: Describe {
    fn describe self:Box[T] -> String { self.value.describe }
}
```

Struct declarations accept plain type parameters such as `Box[T]`. Put
requirements such as `T: Describe` on functions or `impl` blocks.

## Advanced supertraits and defaults

A trait can extend other traits and provide method bodies:

```casa
trait Labeled {
    fn label self:self -> String
}

trait Greet: Labeled {
    fn greeting self:self -> String {
        f"Hello, {self.label}!"
    }
}
```

A type that implements `Greet` must also provide the requirements inherited
from `Labeled`. It receives `greeting` unless an applicable inherent method
replaces that default.

Casa rejects supertrait cycles and inherited methods with incompatible stack
effects. Two unrelated applicable defaults with the same name are ambiguous.

## Advanced method selection

Inside generic code, dot syntax and the type-parameter namespace are
equivalent:

```casa
fn description[T: Describe] value:T -> String {
    value T::describe
}
```

Qualify a method when more than one trait provides the same name:

```text
value First::render
value Second::render
```

Use `&T::method` to get a trait method as a function value. When a type
implements several versions of a generic trait, include the receiver and full
trait type, for example `&Token::Convert[i64]::convert`.
