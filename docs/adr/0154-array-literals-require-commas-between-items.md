# Array literals require commas between items
related issue: #437

An array literal MUST separate its items with commas: `[1, 2, 3]`. A missing
comma between two items is a syntax error. A single trailing comma before the
closing `]` is allowed, so `[1, 2, 3,]` is also valid.

```casa
[1, 2, 3] = numbers: array[i64 3] # required commas
[1, 2, 3,] = trailing: array[i64 3] # optional trailing comma allowed
[1 2 3]                          # syntax error: missing commas
```

Whitespace-only separation would leave two spellings for one array. A single
optional trailing comma instead makes multiline literals easy to edit and reorder.

## Consequences

- Generic type-argument lists (`Map[str i64]`) are unaffected. They use a
  different parser and keep their whitespace-separated form.
- A trailing comma carries no meaning: `casafmt` omits it in the compact form
  and adds it in the expanded form, and the syntax-fact safety net excludes
  commas from its token comparison so this normalization is allowed.
