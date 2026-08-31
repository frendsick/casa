# Review

Rules for the pre-PR review loop.

## Finding classes

Assign each finding one class:

- **Must fix**: A defect in the current implementation within the requested
  scope.
- **Prerequisite**: A separately scoped capability that must exist before the
  requested behavior or required checks can succeed.
- **Non-blocking**: Any other improvement.

Keep a trivial prerequisite in the current work. Track and split a non-trivial
prerequisite when it can merge independently.

## Review loop

Before opening a pull request, select the validation tier in
[testing.md](./testing.md). Documentation-only and CI-only changes do not run
Casa tests.

1. Review the Standards and Spec axes in parallel. The first pass for each axis
   must report all findings from the complete diff.
2. Include the `ponytail-review` checks in the Standards axis. Apply the task's
   existing `function-design` analysis to changed non-trivial functions. Do not
   run separate passes for these checks.
3. The Standards reviewer must list every changed comparison and
   non-commutative call, translate each Casa expression to conventional
   notation, and confirm its argument order against a focused test.
4. Give each finding a stable identifier and class. Keep the finding and its
   disposition in the review context for later passes.
5. **MUST** fix every Must fix finding without pausing. Batch compatible fixes,
   then run focused tests.
6. Implement a trivial or non-independent Prerequisite in the current work. For
   a non-trivial Prerequisite that can merge independently, ensure it has an
   issue, split it from the current work, keep the pull request in draft, and
   add a native GitHub blocker relationship to the originating issue. If there
   is no originating issue, add the relationship to the pull request instead. See
   [issue-tracker.md](./issue-tracker.md#blocker-relationships).
7. Report Non-blocking findings to the user and in the pull request. Do not
   implement them.
8. Re-review only an axis affected by the fixes. Give the same reviewer its
   prior findings and the correction diff. The reviewer must verify each
   disposition and inspect the changed paths and their callers for regressions.
   It must not review unchanged parts of the original diff again or repeat an
   existing Non-blocking finding.
9. Use a fresh reviewer context to review the complete diff when a fix changes
   an interface or the design, reaches outside the previously affected flow, or
   two incremental passes still produce Must fix or untracked Prerequisite
   findings.
10. Record each round's separate Standards and Spec results in the task
    conversation. Record elapsed time from the first axis dispatch until both
    axis results arrive. Mark repeated findings, fresh-review reasons, and user
    decisions. A combined self-review or an unrecorded axis does not complete a
    round.
11. Repeat until no Must fix or Prerequisite assigned to the current work
    remains, then run the selected tier's final validation once and open or
    update the pull request. See [testing.md](./testing.md) and [git.md](./git.md).

## Why no pausing

Stopping mid-loop wastes a round-trip and frustrates the user. The review is part
of finishing the change, not a separate sign-off.
