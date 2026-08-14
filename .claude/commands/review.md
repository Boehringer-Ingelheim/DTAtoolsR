---
description: Review the diff against this package's R/S7/CRAN conventions via the r-review subagent (Sonnet). For bug-hunting instead, use /code-review.
argument-hint: "[working | branch] (default: working tree)"
---

Use the `r-review` subagent to review the current changes before I commit them.
Target: $ARGUMENTS — `working` (or empty) means the working-tree diff, `branch`
means `git diff master...HEAD`.

This is the conventions review described in the project workflow: S7 property
contracts, roxygen/`@examples` coverage, `Collate:` ordering, `cli_abort()` over
`stop()`, namespaced calls declared in `Imports:`, and `CHANGELOG.md` /
`Version:` hygiene. It is read-only.

Wait for the findings, then decide on the main thread which to act on — the
subagent reports, it does not design. Tell me which findings you accept, which
you are rejecting and why, and fix the accepted ones yourself.
