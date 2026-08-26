# Architecture decision records

An ADR without `status` metadata is accepted. It records the current decision
even when work to implement it is still open.

Use `status: amended by [ADR-NNNN](NNNN-slug.md)` when one or more successors
change part of the decision. Use `status: superseded by [ADR-NNNN](NNNN-slug.md)`
when a successor replaces the decision. Retain amended and superseded ADRs as
history.

Use `related issue: #NNN` when the related implementation issue is known. The
issue tracks delivery status. An open related issue does not change an accepted
ADR's status.
