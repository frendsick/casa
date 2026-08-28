# Review

Rules for the pre-PR review loop.

## Finding classes

Assign each finding one class:

- **Must fix**: A defect within the requested scope.
- **Prerequisite**: Work required by the requested behavior, specification, or
  required checks.
- **Non-blocking**: Any other improvement.

## Review loop

Before opening a pull request:

Do not run focused behavior tests for documentation-only changes. The complete
pre-PR suite required by [testing.md](./testing.md) still applies.

1. Run the `ponytail-review` skill on the diff.
2. Review non-trivial functions added or changed with `function-design` as part
   of the Standards axis.
3. Review the Standards and Spec axes.
4. Classify each finding.
5. **MUST** fix every Must fix finding without pausing. Run focused tests and
   review only the affected axis again.
6. Implement an untracked Prerequisite in the current work. For a tracked
   Prerequisite, keep the pull request in draft and add a native GitHub blocker
   relationship to the originating issue. If there is no originating issue, add
   the relationship to the pull request instead. See
   [issue-tracker.md](./issue-tracker.md#blocker-relationships).
7. Report Non-blocking findings to the user and in the pull request. Do not
   implement them.
8. Repeat until no Must fix or untracked Prerequisite findings remain, then open
   or update the pull request. See [git.md](./git.md).

## Why no pausing

Stopping mid-loop wastes a round-trip and frustrates the user. The review is part
of finishing the change, not a separate sign-off.
