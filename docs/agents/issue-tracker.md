# Issue tracker: GitHub

Issues and PRDs for this repo live as GitHub issues. Use the `gh` CLI for all operations.

## Before implementation

Search open and closed issues for matching work. Read the relevant issue body,
comments, labels, and dependency state. Inspect the current code before
accepting a suggested design. Record newly discovered blockers with native
relationships and add material findings to the affected existing issue. Then
follow the preflight in [git.md](./git.md#worktrees).

## Conventions

- **Create an issue**: `gh issue create --title "..." --body "..."`. Use a heredoc for multi-line bodies.
- **Read an issue**: `gh issue view <number> --comments`, filtering comments by `jq` and also fetching labels.
- **List issues**: `gh issue list --state open --json number,title,body,labels,comments --jq '[.[] | {number, title, body, labels: [.labels[].name], comments: [.comments[].body]}]'` with appropriate `--label` and `--state` filters.
- **Comment on an issue**: `gh issue comment <number> --body "..."`
- **Apply / remove labels**: `gh issue edit <number> --add-label "..."` / `--remove-label "..."`
- **Close**: `gh issue close <number> --comment "..."`

## Blocker relationships

Use GitHub's native `blocked by` relationship for tracked prerequisites. Do not
copy the relationship into issue or pull request text.

```sh
blocker_id=$(gh api repos/{owner}/{repo}/issues/<blocker> --jq .id)
gh api --method POST repos/{owner}/{repo}/issues/<blocked>/dependencies/blocked_by \
  -F issue_id="$blocker_id"
```

Keep the relationship after the blocker closes so it remains in the issue
history. Only open blockers prevent completion.

Inspect current dependencies and related issues before creating a blocker or a
new issue. Update existing relationships and issue context when they already
represent the work.

Infer the repo from `git remote -v`. `gh` does this automatically inside a clone.

## When a skill says "publish to the issue tracker"

Create a GitHub issue.

## When a skill says "fetch the relevant ticket"

Run `gh issue view <number> --comments`.
