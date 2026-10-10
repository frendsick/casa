# Migrating comparison operand order

Binary symbolic operators now use source operand order. `10 3 <` means
`10 < 3`. Earlier Casa versions interpreted it as `3 < 10`. Existing comparison
expressions can still typecheck while producing a different result.

Run the migration tool once on each source file written with the old rule,
including your imported modules. Build the tool with the compiler and libraries
from this checkout:

```sh
./casac -L lib tools/migrate_operators.casa -o /tmp/casa-migrate-operators
/tmp/casa-migrate-operators < program.casa > program.casa.migrated &&
    mv program.casa.migrated program.casa
```

The tool reads standard input and writes the migrated source to standard output.
It inserts `swap` before each `==`, `!=`, `<`, `<=`, `>`, and `>=` token:

```casa
# Before migration, with the old rule:
left_expression right_expression <

# After migration, with the new rule:
left_expression right_expression swap <
```

Both expressions still run in their original order. The comparison still calls
the same method with the same receiver and other argument. This also applies
to custom `eq` and `ne` implementations. Replacing `<` with `>` would select
a different user-defined method and would not preserve that behavior.

The tool handles comparison tokens in constant blocks and nested f-string
expressions. It preserves comments, string and character contents, whitespace,
and line endings. Named calls, arithmetic, and boolean operators keep their
existing behavior. Lexical errors or invalid UTF-8 cause a nonzero exit status,
an error on standard error, and unchanged input on standard output.

Use the tool only on source that uses the old rule. It does not detect a file's
language version or whether migration has already run. A second run inserts
another `swap` and changes the behavior again. Migrate embedded Casa source in
test strings or generated files separately. The tool leaves ordinary string
contents untouched and does not follow imports.

After migration, review the diff and run your tests. Comparisons of known
primitive values can be simplified. For example, `index length swap >` can
become `index length <`. Keep operand expressions in their original evaluation
order when they have effects. A formatter pass does not migrate semantics.
