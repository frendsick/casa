# Strict generic type matching

Generic types must be fully parameterized at all boundaries: function parameters, variable assignments, and stack effect declarations. Bare generic types like `self:Option` (without type parameters) are no longer accepted; write `[T] self:Option[T]` instead. The type unifier rejects incompatible concrete types even when one side is an enum, closing a hole where `Option[int]` could silently unify with `str` or `Option[str]` during variable assignment.

**Considered Options**

- Keep the zero-param matching rule and only fix `is_enum_type_var_t`: rejected because bare generic types erase type parameters, silently bypassing the type checker for any code that omits them. Two separate permissiveness holes are easier to reason about as one policy.
- Require full parameterization only in function signatures, not variable types: rejected because the unification bug is in variable assignment (`unify_type_t`), and allowing bare generics anywhere keeps the ambiguity between `Type::Generic("Option", [])` meaning "Option with erased params" vs "Option with unknown params."

**Consequences**

- `is_enum_type_var_t` is replaced by a check for unresolved `Type::TypeVar` nodes in the type tree. `Option[T]` (unresolved) unifies flexibly; `Option[int]` (resolved) unifies structurally.
- `is_type_variable_t` is deleted. `type_var_name` only matches `Type::TypeVar`.
- `types_match_t` is deleted. `expect_type_t` uses `unify_type_t`; `type_satisfies_trait` and `stack_effect_matches` call `stack_effect_type_matches` directly.
- The zero-param-matches-any-param rule in `stack_effect_type_matches` is removed.
- Stdlib methods `is_ok`, `is_some`, `is_none`, `is_error` gain explicit type parameters.
