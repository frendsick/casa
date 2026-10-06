# Casa review requirements

Select the validation tier in [testing.md](./testing.md). Documentation-only
and CI-only changes do not run Casa tests.

- Reuse the task's `function-design` analysis for changed non-trivial functions.
- List every changed Casa comparison and non-commutative call, translate each
  expression to conventional notation, and confirm its argument order against
  a focused test.
