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

1. Review the Standards and Spec axes in parallel. The first pass for each axis
   must report all findings from the complete diff.
2. Include the `ponytail-review` checks in the Standards axis. Review
   non-trivial functions added or changed with `function-design` in the same
   axis. Do not run separate passes for these checks.
3. Give each finding a stable identifier and class. Keep the finding and its
   disposition in the review context for later passes.
4. **MUST** fix every Must fix finding without pausing. Batch compatible fixes,
   then run focused tests.
5. Implement an untracked Prerequisite in the current work. For a tracked
   Prerequisite, keep the pull request in draft and add a native GitHub blocker
   relationship to the originating issue. If there is no originating issue, add
   the relationship to the pull request instead. See
   [issue-tracker.md](./issue-tracker.md#blocker-relationships).
6. Report Non-blocking findings to the user and in the pull request. Do not
   implement them.
7. Re-review only an axis affected by the fixes. Give the same reviewer its
   prior findings and the correction diff. The reviewer must verify each
   disposition and inspect the changed paths and their callers for regressions.
   It must not review unchanged parts of the original diff again or repeat an
   existing Non-blocking finding.
8. Use a fresh reviewer context to review the complete diff when a fix changes
   an interface or the design, reaches outside the previously affected flow, or
   two incremental passes still produce Must fix or untracked Prerequisite
   findings.
9. Repeat until no Must fix or untracked Prerequisite findings remain, then run
   the complete pre-PR suite once and open or update the pull request. See
   [testing.md](./testing.md) and [git.md](./git.md).

## Why no pausing

Stopping mid-loop wastes a round-trip and frustrates the user. The review is part
of finishing the change, not a separate sign-off.
