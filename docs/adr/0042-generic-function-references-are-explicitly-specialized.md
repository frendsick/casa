# Generic function references are explicitly specialized

Every first-class function value has one monomorphic `fn[...]` type. Taking a reference to a generic named function requires explicit type arguments:

```casa
fn id[T] value:T -> T { value }

&id[i64]  # fn[i64 -> i64]
&id[str]  # fn[str -> str]
```

`&id` is rejected because it would require a first-class polymorphic function value. Direct calls to generic functions continue to infer type arguments from their operands.

## Considered options

- A first-class universally quantified function type preserves `&id`, but requires `fn[...]` values, assignments, indirect calls, and returned callbacks to carry quantified variables and bounds.
- Inferring specialization from a later expected function type reduces annotations, but adds backward constraint propagation to Casa's left-to-right stack checking.
- Capturing the first indirect call's types makes behavior depend on use order and permits surprising later failures.
- Explicit specialization keeps function values concrete and makes their complete stack effect visible at the reference site.

## Consequences

- Free generic functions use `&function[Arguments]`; every declared type argument is supplied.
- Trait and `Copy` bounds are checked when the reference is formed.
- A generic body may specialize using its own in-scope type variables, such as `&id[T]`; the resulting function type is monomorphic for each instantiation of the enclosing function.
- `FunctionType` need not represent universally quantified variables or delayed bounds.
- Casa initially performs no expected-type specialization for omitted reference arguments. It may be added later as local syntactic inference without changing function-value semantics.
- Named function references remain `Copy`; captured closures are repeatable and follow the ordinary ownership and borrowing rules.
