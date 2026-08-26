# Array literals require commas between items
related issue: #437

An array literal MUST separate its items with commas: `[1, 2, 3]`. A missing
comma between two items is a syntax error. A single trailing comma before the
closing `]` is allowed, so `[1, 2, 3,]` is also valid.

Previously the comma was optional. The parser consumed it with
`expect_delimiter drop`, so items were separated by whitespace alone and a comma
was tolerated decoration. That left two spellings for the same array (`[1 2 3]`
and `[1, 2, 3]`), which is exactly the optional syntax this decision removes.

```casa
[1, 2, 3] = numbers:array[i64 3]   # required commas
[1, 2, 3,] = trailing:array[i64 3] # optional trailing comma allowed
[1 2 3]                          # syntax error: missing commas
```

## Considered options

- Keep the comma optional. Matches the whitespace-separated RPN heritage, but
  every array literal in the repository already uses commas, and two accepted
  spellings for one value invite drift.
- Require commas and forbid the trailing comma. Fully strict, but the trailing
  comma keeps expanded multiline literals easy to edit and reorder, so it earns
  its keep for developer experience.
- Require commas between items and allow a single optional trailing comma
  (chosen). Removes the redundant separator spelling while keeping the
  ergonomic trailing comma.

## Consequences

- `get_op_array` requires a comma between consecutive items and reports
  `Expected \`,\` between array items` otherwise.
- Generic type-argument lists (`Map[str i64]`) are unaffected; they use a
  different parser and keep their whitespace-separated form.
- A trailing comma carries no meaning: `casafmt` omits it in the compact form
  and adds it in the expanded form, and the syntax-fact safety net excludes
  commas from its token comparison so this normalization is allowed.
- No existing repository source changes, since all array literals already use
  commas.
