# Standard Copy extends Clone

The standard library declares Copy as a Clone subtrait:

```casa
trait Copy: Clone { }
```

Copy provides allocation-free implicit and stack duplication through `dup` and `over`. Extending Clone also guarantees that every standard Copy type works in `[T: Clone]` code and has an explicit `.clone` operation, so programmers never need to ask whether a copyable value is clonable.

The relationship comes from the visible standard declaration rather than an unconditional compiler rule. A freestanding library may instead declare `trait Copy { }`, in which case its Copy types have no Clone guarantee.

For Casa's standard Copy declaration, `derives Copy` and the equivalent validated `impl Type: Copy { }` supply missing fieldwise Clone behavior automatically. An existing or later explicit Clone implementation takes precedence and remains in use. The compiler does not synthesize behavior for unrelated Copy supertraits.

## Consequences

- `[T: Copy]` may call Clone methods through ordinary supertrait lookup.
- `[T: Clone]` accepts every type that implements the standard Copy declaration.
- Standard scalar Copy types receive trivial Clone implementations.
- `derives Copy` and `impl Type: Copy { }` remain equivalent and provide a fieldwise Clone fallback only when no explicit implementation exists.
