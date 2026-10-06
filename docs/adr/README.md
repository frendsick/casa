# Architecture decision records

ADRs describe decisions implemented in the current repository, their rationale,
and consequences. Verify behavior against code and tests before updating a record.
Plans and delivery status belong in issues. Previous decisions remain in git history.

## Admission and retention

Create a record only for a durable decision that is costly to reverse,
surprising without context, and based on a real tradeoff. Keep the decision,
its non-obvious reason, and consequences outside the immediate implementation.
API usage belongs in reference docs.

Keep only implemented decisions. Trim a mixed record to implemented behavior
when the boundary is clear. Ask the maintainer when implementation status or
retention is unclear. Remove obsolete progress, migration plans, saved
measurements, and implementation details already available in code or reference docs.
Keep alternatives only when their tradeoff explains the current choice.

Keep benchmark workloads, measurement tools, and generated results outside the
repository. Never commit benchmark files or performance reports.

Preserve stable identifiers and filenames for retained records. Use
`status: amended by [ADR-NNNN](NNNN-slug.md)` when a retained successor changes
part of an implemented decision. Remove a record when it no longer describes
current behavior. Check maintained links and status references after cleanup.
