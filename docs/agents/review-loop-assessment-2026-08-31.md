# Review loop assessment, 2026-08-31

## Scope and method

The sample contains the first ten code pull requests whose task conversations
record use of the review policy from PR #566: #568, #569, #570, #571, #573,
#574, #575, #576, #577, and #578. PR #567 opened after #566 merged, but its
review had already used the previous process.

A round is one paired Standards and Spec pass, including an incremental or
fresh pass. An axis-only disposition belongs to its current round. Round elapsed
time runs from the first axis dispatch until both axis results arrive. Total
review elapsed runs from the first round's dispatch through the last recorded
round result. It excludes final validation and CI. A user intervention is a
user message needed to select or unblock the review process. Scope, validation,
and pull request metadata changes after review are excluded.

A repeated finding is the same defect left open after a claimed correction.
Tracked prerequisites carried forward with the same identifier are not repeats.
A post-review defect is a defect first reported by final validation, pull
request CI, pull request discussion, or a later repository issue.

## Results

| Pull request | Rounds | Review elapsed | Repeated findings | User interventions | Post-review defects |
|---|---:|---:|---|---:|---:|
| #568 | 2 | 10 min | None | 1 adoption prompt | 0 |
| #569 | 2 | 4 min | None. One cross-axis overlap | 0 | 0 |
| #570 | 3 | 11 min | None. One proposed prerequisite was withdrawn | 0 | 0 |
| #571 | 2 | 23 min | None | 0 | 0 |
| #573 | 0 completed; 1 combined self-review, then an unreviewed change | Not measurable | None reported | 0 | 0 known |
| #574 | 8 recorded, final Spec result missing | 2 h 20 min | 2 partial corrections | 2 | 0 known |
| #575 | 1 | 4 min | None | 0 | 0 |
| #576 | 1 | 6 min | None. Two proposed prerequisites were withdrawn | 0 | 0 |
| #577 | 4 | 35 min | None. One cross-axis overlap | 0 | 0 |
| #578 | 0 completed; 1 combined self-review | Not measurable | None reported | 0 | 0 known |

The median was two rounds. Eight pull requests have independent axis timestamps.
For their initial rounds, parallel execution avoided an estimated median 41% of
the wall time that the same recorded axis durations would have taken in series.

No user had to choose how to fix a finding. Two interventions told work that
started before PR #566 merged to adopt the new process. A third intervention on
#574 told the task to open the pull request after a later main merge. The task
published without the planned fresh review or a final Spec result.

Incremental review preserved finding identifiers and avoided repeated
Non-blocking findings. The two repeated findings were both on #574. Clone order
was only partly corrected after its first report, and qualified equality still
accepted an unrelated inherent method after its first correction. Both were
fixed before publication.

Fresh review was used on #568, #570, #571, and #574 when corrections changed an
interface, design, or affected flow. The fresh passes on #574 found additional
correctness defects after earlier incremental passes had converged. This is
evidence for keeping the escalation rule.

#574 later merged another compiler-interface change after its last recorded
Spec result still had a Must fix finding. Its replacement fresh review did not
finish before publication. #577 used another incremental pass after two
incremental passes still found defects. These were two escalation deviations.
#577's final correction was incrementally verified. #574's final correction was
not review-verified. Neither produced a known escaped defect.

All final pull request CI runs passed. No pull request discussion or later
repository issue reported a defect in the changed behavior before this
assessment. #573 removed 114 lines after its only combined review and did not
repeat review. Its final revision and #574's final corrections cannot support a
claim that review prevented defects. The observation period is also short, so
the sample does not establish a long-term defect rate.

#573 and #578 used combined self-review without parallel reviewer dispatch.
This repeated compliance problem justifies requiring separate, recorded axis
results and treating a combined or unrecorded axis as an incomplete round.

## Decision

Keep parallel Standards and Spec review, incremental re-review, and fresh-review
escalation. The sample supports lower initial review latency and no finding-level
user intervention. It does not justify weakening the fresh-review safeguard.
The only workflow change is the evidence-recording requirement.
