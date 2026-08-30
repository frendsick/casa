# Git conventions

Rules for commits, branches, and pull requests in this repo.

## Commits

- MUST use conventional commit format.
- MUST sign every commit. Never bypass signing with `-c commit.gpgsign=false` or
  `--no-gpg-sign`. If signing fails, stop and ask the user to `ssh-add` their SSH key.
- MUST NOT append `Co-Authored-By` lines, mentions of Claude or AI usage, or any
  other tooling references to commit messages, PR descriptions, or branch names.
- MUST NOT commit Claude-related files (`.claude/`, agent memory, transcripts).
- MUST NOT commit binary files unless the user explicitly asks for it.
- MUST keep one commit per functionality. Don't mix unrelated changes.
- MUST keep commit messages brief.

## Branches

- MUST NOT modify `main` directly. All work happens on a feature branch.
- MUST use conventional prefixes: `feat/`, `refactor/`, `fix/`, etc. This applies
  to worktree branches too — rename away from the `worktree-` default before pushing.

## Issues

- Always set `frendsick` as assignee when working on an issue that does not have an assignee

## Pull requests

- Always set `frendsick` as assignee
- Always set relevant labels for the PR
- The PR body MUST have a `Validation` section that lists each exact local
  command that ran.
- A pull request is complete only after all required CI checks pass.
- Retry a failed CI job only when its log identifies an infrastructure or
  transient failure.

## Releases

- Every release **MUST** contain assets named `casac` and `casafmt`, with no
  version suffixes. CI and new worktrees download those exact names.
- Build the release `casafmt` with the `casac` uploaded to the same release.
- Release descriptions must summarize changes without mentioning the compiler's
  bootstrap role.
- When using `gh release create`, pass binaries whose filenames are exactly
  `casac` and `casafmt`. The `#label` syntax changes only the display label, not
  the download filename.

## Workflow

- MUST make a commit once a planned change is finished.
- MUST open a pull request for every implemented issue and other completed
  planned change after resolving all review findings that can be completed in
  the current work. See [review.md](./review.md).
