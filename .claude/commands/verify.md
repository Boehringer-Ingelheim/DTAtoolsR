---
description: Run tests / style / roxygen / R CMD check via the r-verify subagent (Haiku) and report only failures.
argument-hint: "[test filter, or 'style' | 'docs' | 'check' | 'all'] (default: full test suite)"
---

Use the `r-verify` subagent to verify the current working tree. Scope: $ARGUMENTS

Interpret the scope as follows — if it is empty, run the full test suite.

- a bare word (e.g. `DTAFile`) → `devtools::test(filter='<word>')`
- `style` → `styler::style_pkg(dry = "fail")` — checks without writing, and
  errors if any file is not already styled
- `docs` → `roxygen2::roxygenise()`, then report whether `man/` or `NAMESPACE`
  came out dirty (`git status --porcelain man NAMESPACE`)
- `check` → `rcmdcheck::rcmdcheck(args='--no-manual')`
- `all` → tests, then `style`, then `docs`, then `check` — this mirrors what CI
  enforces. Run every stage even if an earlier one fails, and report all
  failures together; do not stop at the first.

Do not run `Rscript` from the main thread; the point of this command is to keep
the transcript out of the main context. Wait for the subagent's report, then
tell me only what failed and what you intend to do about it.

**Applying style fixes is a main-thread job.** `r-verify` only ever checks with
`dry = "fail"`. If it reports unstyled files, run
`Rscript -e "styler::style_pkg()"` yourself and show me the resulting diff —
never ask the subagent to rewrite R sources.

Remember the local environment: `Rscript` is not on `PATH` here, and
`pre-commit` is not installed locally — do not claim the hooks passed. Note also
that `pre-commit` runs only the fast language-agnostic hooks; it does not style
or roxygenise, so those stages are not covered by running the hooks.
