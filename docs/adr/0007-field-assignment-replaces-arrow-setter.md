# Field assignment replaces arrow setter syntax

The `->` setter syntax (`value instance->field`) is removed in favour of extending the existing assignment operators (`=`, `+=`, `-=`) to accept variable-rooted dot-chain lvalues (`= a.field`, `= a.b.c`, `1 += self.depth`). The motivation is twofold: a single assignment operator works uniformly for both local variables and struct fields, and compound assignment (`+=`, `-=`) extends naturally to fields without introducing a separate operator family.

**Considered Options**

- Keep `->` alongside `=`-on-paths as an alias: rejected because two syntactically equivalent ways to set a field permanently split the codebase style and double the language surface area.
- Add compound-assignment-only operators (`+->`, `->+=`) without touching `=`: rejected because it introduces yet another operator family instead of generalising the one that already exists.

**Consequences**

- `=`, `+=`, `-=` accept a variable-rooted dot-chain lvalue as their target: `identifier(.field)*`. The root must be a named variable; arbitrary expression receivers are not lvalues.
- `+=` and `-=` on field paths remain `int`-only, consistent with their behaviour on local variables.
- `->` is removed from the delimiter handler entirely. Its use in type annotations (`fn[int -> str]`) is unaffected — that token is parsed in a separate code path.
- All existing call sites using `->` must be migrated to `= receiver.field`.
