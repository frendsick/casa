# Trait conformance is explicit

A type implements a user-defined trait only through an explicit conformance clause on an `impl` block. Casa extends its type-first `impl` syntax with `:` and uses `+` for multiple traits:

```casa
impl Item: Eq + Display {
    fn eq $self other:$self -> bool { ... }
    fn to_str $self -> str { ... }
}
```

The compiler validates every required method and inherited supertrait requirement at the declaration. Merely defining methods with matching names and stack effects does not make a type implement a trait. Methods in the block remain callable on the concrete type when unambiguous, while retaining their conformance identity so distinct generic-trait instantiations can coexist. One definition may satisfy compatible requirements from multiple declared traits.

An empty conformance block explicitly adopts a trait whose requirements are all defaults or whose purpose is marking a capability:

```casa
impl Item: Described { }
```

## Considered options

- Structural conformance avoids declarations, but makes empty traits match every type and installs default-only traits globally.
- Making only empty or default-only traits nominal creates two conformance models whose behavior depends on trait contents.
- Rust-style `impl Trait for Type` is explicit, but reverses Casa's existing type-first `impl Type` form.
- `impl Type: Trait` extends existing syntax with the same constraint separator used elsewhere.

## Consequences

- Empty marker traits and default-only traits are meaningful because only explicitly adopting types implement them.
- Generic bounds are satisfied by declared conformance rather than a global scan for matching methods.
- `impl[T: Eq] Pair[T]: Eq { ... }` declares conditional generic conformance.
- `derives Eq`, `derives Ord`, `derives Hashable`, `derives Clone`, and `derives Copy` declare conformance and generate any required methods. Copy conformance is additionally subject to compiler representation validation.
- Default methods are considered only from traits the receiver explicitly implements, including their supertraits. Ambiguity follows the separate default-method coherence rule.
- Ordinary `impl Type { ... }` blocks continue to define inherent methods without declaring trait conformance.
- Rules governing where a conformance may be declared and whether it may be repeated require a separate coherence decision.
