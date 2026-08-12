---
description: Run tests / roxygen / R CMD check via the r-verify subagent (Haiku) and report only failures.
argument-hint: "[test filter, or 'docs' | 'check' | 'all'] (default: full test suite)"
---

Use the `r-verify` subagent to verify the current working tree. Scope: $ARGUMENTS

Interpret the scope as follows — if it is empty, run the full test suite.

- a bare word (e.g. `DTAFile`) → `devtools::test(filter='<word>')`
- `docs` → `roxygen2::roxygenise()`
- `check` → `rcmdcheck::rcmdcheck(args='--no-manual')`
- `all` → tests, then roxygen, then the full check

Do not run `Rscript` from the main thread; the point of this command is to keep
the test transcript out of the main context. Wait for the subagent's report,
then tell me only what failed and what you intend to do about it.

Remember the local environment: `Rscript` is not on `PATH` here, and
`pre-commit` is not installed locally — do not claim the hooks passed.
