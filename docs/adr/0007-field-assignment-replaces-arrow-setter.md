# Field assignment replaces arrow setter syntax

Casa has no `->` setter syntax (`value instance->field`). The assignment
operators `=`, `+=`, and `-=` accept variable-rooted dot-chain targets such as
`= a.field`, `= a.b.c`, and `1 += self.depth`. One assignment form works for
both local variables and struct fields, and compound assignment extends to
fields without a separate operator family.

## Considered options

- Keeping `->` as an alias for `=` on paths gives two equivalent ways to set a
  field. It splits code style permanently and doubles the language surface.
- Adding compound-assignment-only operators such as `+->` or `->+=` without
  changing `=` adds another operator family instead of generalizing the
  existing one.

## Consequences

- `=`, `+=`, and `-=` accept a variable-rooted dot-chain target:
  `identifier(.field)*`. The root must be a named variable. Arbitrary expression
  receivers are not assignment targets.
- `+=` and `-=` on field paths accept only integers, as they do on local
  variables.
- `->` appears only in signatures and function types such as `fn[i64 -> str]`.
  Assignment parsing does not use it.
