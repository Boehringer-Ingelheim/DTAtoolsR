# DTAtools

R package (S7 classes) that validates tabular data against Data Transfer
Specifications written in YAML. Domain overview: `README.md`. Feature walkthrough:
`vignettes/DTAtools.Rmd`.

## Commands

Run everything from the repo root; `renv` activates via `.Rprofile`.

| Task | Command |
| --- | --- |
| Load package | `Rscript -e "pkgload::load_all()"` |
| Tests | `Rscript -e "devtools::test()"` (one file: `devtools::test(filter='DTAFile')`) |
| Regenerate docs | `Rscript -e "roxygen2::roxygenise()"` |
| Full check (= CI) | `Rscript -e "rcmdcheck::rcmdcheck(args='--no-manual')"` |
| Style + hooks | `pre-commit run --all-files` (also runs in CI) |

In PowerShell, `R` is an alias for `Invoke-History` — always use `Rscript`, and
quote `-e` with double quotes so the argument survives. If `Rscript` is not on
`PATH`, `CLAUDE.local.md` holds the absolute path for this machine.

## Workflow

Plan on the main thread; delegate the token-heavy work to subagents.

1. **Plan before editing.** Any change touching more than one file, or any S7
   class, starts in plan mode with a plan written on the main thread. No edits
   until the plan is approved.
2. **Delegate searching** to `Explore` (Haiku) — "where is X used", "which files
   define Y". Do not grep the codebase from the main thread.
3. **Delegate verification** to `r-verify` (Haiku) after edits; it runs tests and
   returns failures only.
4. **Delegate review** to `r-review` (Sonnet) before proposing a commit.
5. Subagents gather and verify; the main thread decides. Never let a subagent
   design a class hierarchy or pick an S7 property contract.

## Tools

- `gh` (on `PATH`) for all GitHub work — PRs, issues, `gh run view` for CI logs.
  Never scrape the web UI.
- Graphify is installed but produces nothing here: its extractor has no `.R`
  parser, so a build reports "No code files found". Use `Explore`, not a graph.
  `.graphifyignore` exists only to keep an accidental build from spending
  minutes indexing the vendored JavaScript under `renv/`.
- Do **not** run `air format` (Posit Air, bundled with the VS Code extension).
  Its style disagrees with the `styler` tidyverse config in
  `.pre-commit-config.yaml`, and running it reformats files the hooks will then
  revert. Air is for the editor's format-on-save only.

## Conventions

- New class → S7 `new_class()`, one per `R/<Name>-class.R`, added to `Collate:`
  in `DESCRIPTION` in dependency order (parents before children).
- Reuse the `class_*_or_null` unions in `R/00_helpers.R`; do not redefine them.
- User-facing errors/warnings use `cli::cli_abort()` / `cli::cli_warn()`, never
  `stop()` or `warning()`.
- Call dependencies namespaced (`dplyr::filter`) and declare them in
  `DESCRIPTION` `Imports:` — the `deps-in-desc` hook fails otherwise.
- Every exported function: roxygen block with `@export` and a runnable
  `@examples`. Examples are executed by `R CMD check`, **not** by the test
  suite — `tests/testthat/test-examples.R` exercises the bundled example
  *data* under `inst/extdata`, despite its name. An example wrapped in
  `\dontrun{}` is therefore never executed anywhere, so use it only when the
  code genuinely cannot run unattended (it writes outside `tempdir()`, needs
  a network, or launches the Shiny app).
- Every behaviour change: a test in `tests/testthat/test-<Topic>.R`.
- Tests must assert behaviour, not existence. `expect_error()` needs a
  `regexp` or `class`; a file-producing function is checked by reading the
  file back, not by `file.exists()`. Note `expect_s3_class(x, c("A","B"))` is
  an ANY-match, not ALL. Some tests deliberately pin known defects — search
  `KNOWN DEFECT, pinned rather than endorsed`; when the fix lands they are
  *meant* to fail, and the comment names the assertion to switch to.
- Never assert on translated text. R renders base errors and `%B` month names
  in the system language (German on the primary dev machine), so match
  condition classes (`subscriptOutOfBoundsError`), package-authored `cli`
  strings, or force `LC_TIME = "C"`.
- tidyverse style, applied by `styler` in pre-commit — do not hand-format.

## Guardrails

- Never hand-edit `man/`, `NAMESPACE`, or `renv.lock` — they are generated.
- Never commit `.rds`, `.RData`, `.Rhistory` (pre-commit rejects them).
- Work on `dev`; PRs target `dev`. `master` is release-only.
- User-facing changes: bump `Version:` in `DESCRIPTION` and add an entry under
  `## [Unreleased]` in `CHANGELOG.md` (Keep a Changelog format).
