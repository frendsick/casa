# Strict generic type matching

Generic types must be fully parameterized in function parameters, assignments,
and stack effects. Write `[T] self:Option[T]`, never bare `self:Option`.

Matching is flexible only where the type tree contains unresolved type variables.
`Option[T]` can unify through an unresolved `T`. Resolved types match
structurally, so `Option[i64]` cannot unify with `str` or `Option[str]`.

Allowing bare generics would erase parameters and confuse erased types with
unknown parameters. Restricting this rule to function headers would leave the
same hole in assignments. Enum or struct identity alone never makes a type
flexible.
