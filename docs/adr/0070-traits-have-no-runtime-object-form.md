# Traits have no runtime object form

Traits remain compile-time constraints with explicit conformance. Casa initially has no `dyn Trait`, existential trait value, vtable, object-safety rules, or dynamic trait-based destruction.

Generic functions handle statically known variation, enums represent closed heterogeneous sets, and function values carry behavior without exposing a complete object interface. A runtime trait-object form remains deferred until a concrete API cannot reasonably use one of those existing mechanisms.
