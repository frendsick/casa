# Casa review requirements

Select the validation tier in [testing.md](./testing.md). Documentation-only
and CI-only changes do not run Casa tests.

- Reuse the task's `function-design` analysis for changed non-trivial functions.
- List every changed Casa comparison and non-commutative call, translate each
  expression to conventional notation, and confirm its argument order against
  a focused test.

## Prerequisites

Keep trivial or non-independent prerequisites in the current work. For a
non-trivial prerequisite that can merge independently, ensure it has an issue,
split it from the current work, and keep the pull request in draft until the
blocker is resolved.

Add a native GitHub blocker relationship to the originating issue. If there is
no originating issue, add it to the pull request instead. See
[issue-tracker.md](./issue-tracker.md#blocker-relationships).
