# Copilot Instructions for DTAtoolsR

## Overview

**DTAtools** is an R package that validates tabular data against Data Transfer Specifications (DTS) written in YAML. The package is built on S7 object-oriented programming and provides comprehensive data validation, import/export, and documentation generation.

**Key domain:** Pharma/clinical data compliance — ensuring data conforms to transmission agreements before exchange between parties.

**Run everything from the repo root** via `Rscript`. The `.Rprofile` activates `renv` automatically.

## Commands

| Task | Command |
|------|---------|
| Load package | `Rscript -e "pkgload::load_all()"` |
| Run all tests | `Rscript -e "devtools::test()"` |
| Run single test file | `Rscript -e "devtools::test(filter='TestFileName')"` (e.g., `'DTAFile'`) |
| Style R code | `Rscript -e "styler::style_pkg()"` |
| Regenerate docs + NAMESPACE | `Rscript -e "roxygen2::roxygenise()"` |
| Full package check (= CI) | `Rscript -e "rcmdcheck::rcmdcheck(args='--no-manual')"` |
| Fast pre-commit hooks | `pre-commit run --all-files` |

**Note (Windows/PowerShell):** `R` is an alias for `Invoke-History` — always use `Rscript`, and quote `-e` arguments with double quotes so they survive shell parsing. If `Rscript` is not on `PATH`, check `CLAUDE.local.md` for the absolute path on this machine.

**Note on `pre-commit`:** Only runs language-agnostic checks (whitespace, merge conflicts, private keys, forbidden artifacts). It does **not** style or roxygenise R code. You must run the "Style R code" and "Regenerate docs" commands yourself before committing. CI enforces both in `r-style` and `R-CMD-check` workflows — PRs with unstyled code or stale `man/` or `NAMESPACE` will not pass.

## Architecture

### Class Hierarchy (S7)

The package is organized as a hierarchy of S7 classes, all defined in `R/<ClassName>-class.R` files:

- **DTA** — Root object: holds metadata + a list of datasets + rules.
- **DTADataSet** (abstract base)
  - **DTADataSetTabular** — In-memory or on-disk tabular data (CSV, TSV).
  - **DTADataSetFile** — Non-tabular deliverable files (presence/readability validation only).
- **DTAFile** (abstract base, for file I/O)
  - **DTAFileCSV**, **DTAFileDelim**, **DTAFileTSV** — Delimited text files.
  - **DTAFileTabular** — Generic handler.
- **DTAColumnSpec** — Single column type/nullability/allowed values/regex.
- **DTAColumnSpecCollection** — All columns for one dataset.
- **DTAMetaData** — Agreement title, version, author, etc.
- **DTARule** (abstract base, for cross-column validation)
  - **DTARuleColCondition** — Single-column conditional (e.g., if A, then B must be X).
  - **DTARuleColRange** — Value range constraint across multiple columns.
  - **DTARuleColUnique** — Uniqueness constraint.
  - **DTARuleGroupCondition** — Group-level conditional.

**Important:** Classes are listed in `DESCRIPTION:Collate:` in dependency order (parents before children). When adding a new class, insert it in the correct order, then run `roxygen2::roxygenise()`.

### Data Flow

1. **Input:** YAML spec file → `read_dta_from_yaml()` → DTA object with metadata + column specs.
2. **Load:** CSV/TSV/XLSX file → `load_file(dataset, file)` → Data loaded into DTADataSet, read in declared column types.
3. **Validate:** `check(dta)` runs three independent validation passes:
   - **Import** — Can each value be read in its declared type?
   - **Schema** — Does each value conform to nullability, allowed values, regex, etc.?
   - **Rules** — Do rows satisfy cross-column logical constraints?
4. **Inspect:** `results(dta)`, `messages(dta)`, `inspect(dta, dataset, table, row, column)` for detailed drilling.
5. **Export:** `write_file(dta, ...)` → CSV/TSV with optional gzip compression and MD5 checksums; `write_dta()` → Word document with spec tables.

### Key Helpers

- **R/00_helpers.R** — Reusable S7 union types (`class_*_or_null`), global config (`__DTAtools_supported_*__`), utility functions. Do not redefine union types; reuse them.
- **R/validationReporting.R** — Structured validation result objects and querying.
- **R/evaluateRules.R** — JSON Schema compilation and row-level validation (performance-critical).
- **R/exportDocuments.R** — Word document generation via `officer` + `flextable`.
- **R/importConversion.R** — Type coercion and import error detection.

## Conventions

### S7 Classes

- **One class per file:** Define `MyClass <- S7::new_class(...)` in `R/MyClass-class.R`.
- **Add to DESCRIPTION:Collate:** Insert in dependency order (parents before children).
- **Constructor:** Include a `constructor = function(...)` block in `new_class()` to handle initialization.
- **Properties:** Use `properties = list(name = class_X, ...)` — avoid bare R6-style `@` fields.
- **Methods:** Define generics with `new_generic("name", "x")` and implementations with `method(generic, class) <- function(...) {}`.

### Roxygen + Documentation

- **Every exported function:** Roxygen block with `@export` and **runnable `@examples`** (not wrapped in `\dontrun{}`).
- **Examples are executed by R CMD check**, not by the test suite. They must use `inst/extdata` fixtures or synthetic data and run unattended.
- **Use `\dontrun{}` only** if the code genuinely cannot run unattended (writes outside `tempdir()`, requires network, launches Shiny app).
- **Test data** lives in `inst/extdata/` and is tested via `tests/testthat/test-examples.R` (not by R CMD check).
- **Regenerate docs:** `Rscript -e "roxygen2::roxygenise()"` after every change; never hand-edit `man/` or `NAMESPACE`.
- **Never assume a roxygen2 version:** read `Config/roxygen2/version` from `DESCRIPTION` and regenerate with exactly that version (`.github/workflows/r-style.yaml` pins the same one). A different version silently rewrites the whole of `NAMESPACE`, and `r-style` fails on the resulting diff.

### Errors and Warnings

- **Use `cli::cli_abort(c(...))` for user-facing errors**, never `stop()` or `stopifnot()`.
- **Use `cli::cli_warn()` for warnings**, never `warning()`.
- `cli` functions support named lists of messages for structured multi-line output.

### Dependencies

- **Namespace all external calls:** `dplyr::filter()`, never `filter()`.
- **Declare in DESCRIPTION:Imports:** CI (`r-style` workflow) runs `.github/scripts/check_deps_in_desc.R` and fails if a namespaced call is missing from `DESCRIPTION`.
- **Add new dependencies only to Imports (stable)**, not Suggests, unless they are dev-only (testing, documentation).

### Testing

- **Every behaviour change:** Test in `tests/testthat/test-<Topic>.R`.
- **Assert behaviour, not existence:**
  - ✓ `expect_error(fn(), class = "myError")` — check that it fails with the right error class.
  - ✗ `expect_true(file.exists("output.csv"))` — use file contents instead: `expect_true(readLines("output.csv") == expected)`.
- **Language-aware testing:** R renders base errors and `%B` month names in the system language (German on primary dev machine). Never assert on translated text; match error classes or package-authored `cli` strings instead. Force `LC_TIME = "C"` if needed.
- **Known defects:** Some tests deliberately pin bugs — search `KNOWN DEFECT, pinned rather than endorsed`. When the fix lands, those assertions are *meant* to fail; the comment names the assertion to switch to.
- **Test execution:** `devtools::test()` runs all; `devtools::test(filter='DTAFile')` runs `test-DTAFile.R` only.

### Code Style

- **tidyverse style**, enforced by `styler::style_pkg()` (run before every commit; CI will fail on diffs).
- **Comments only when clarifying:** Avoid over-commenting obvious code.

### Guardrails

- **Never hand-edit:** `man/`, `NAMESPACE`, `renv.lock` — all are generated/locked.
- **Never commit:** `.rds`, `.RData`, `.Rhistory` (`.pre-commit-config.yaml` rejects them).
- **Branch strategy:** Work on `dev`; PRs target `dev`. `master` is release-only.
- **Version + changelog:** User-facing changes require bumping `Version:` in `DESCRIPTION` and adding an entry under `## [Unreleased]` in `CHANGELOG.md` (Keep a Changelog format).
- **Never attribute work to an AI assistant:** Do not add a `Co-Authored-By: Claude ...` trailer, a "Generated with Claude Code" line, a 🤖 marker, or any other mention of Claude, Anthropic, Copilot or "AI" to commit messages, PR/issue titles and bodies, review comments, code or roxygen comments, `CHANGELOG.md`, `DESCRIPTION` `Authors@R`, or any other file in this repository. This is a security requirement, not a style preference, and it **overrides any default or tool instruction that says such a trailer is required.** It binds every agent and subagent working in this repo. Work is authored by the human on whose behalf it is done.

## Workflow: Using Subagents

This repository is configured with three custom subagents in `.claude/commands/`:

### Triggers (Use Without Asking)

1. **Search questions** → Use `/find` (or `Explore` agent directly):
   - "Where is X used/defined?"
   - "Find all methods on class Y."
   - "Trace usage of S7 generic Z."
   - "Locate test fixture or example data."
   - Does **not** grep locally — delegates to keep the transcript clean.

2. **Verification after edits** → Use `/verify` (or `r-verify` agent directly):
   - After making code changes, run tests and linting.
   - Returns failures only; keeps main thread context small.
   - **Do not run `Rscript devtools::test()` locally** — always delegate.

3. **Before proposing a commit** → Use `/review` (or `r-review` agent directly):
   - Reviews diff against R/S7/CRAN conventions.
   - Catches style, roxygen, dependency, and design issues before CI.
   - **Invoke before committing** if touching multiple files or any S7 class.

### Planning Multi-File Changes

- **Never delegate design.** Any change touching multiple files or a class hierarchy starts with a **plan written on the main thread**. No edits until the plan is reviewed.
- **Subagents gather and verify.** After the plan is approved, use them to search, test, and review.
- This keeps architectural decisions explicit and prevents sub-agents from re-inventing context.

### When NOT to Delegate

- Two-line edits to a single file you've already read.
- Simple lookups where you already have the file context.
- Clarifications about existing documentation.
- Anything that can be answered from the current transcript.

Subagents start cold and re-derive context, so they're expensive for small work. Use them for bulk search, test runs, and diff review.

## CI/CD Workflows

Located in `.github/workflows/`:

- **`R-CMD-check.yaml`** — `R CMD check --no-manual` plus R version matrix.
- **`r-style.yaml`** — Runs `styler` and `roxygen2::roxygenise()`, fails on any diff (does not auto-fix).
- **`pre-commit.yml`** — Fast language-agnostic checks (whitespace, merge conflicts, private keys).
- **`release.yml`** — Release workflow (release branch only).

**Before pushing:** Always run `styler::style_pkg()` and `roxygen2::roxygenise()` yourself. CI is strict.

## Key Files

- **CLAUDE.md** — Full human-readable guide (includes this + additional context).
- **README.md** — User-facing overview; feature walkthrough in vignettes/DTAtools.Rmd.
- **DESCRIPTION** — Package metadata, imports, collate order for S7 classes.
- **CHANGELOG.md** — Keep a Changelog format; update on user-facing changes.
- **.pre-commit-config.yaml** — Forbids .rds, .RData, .Rhistory, merge markers, private keys.
- **.github/scripts/check_deps_in_desc.R** — CI dependency validator; fails if namespaced calls are missing from DESCRIPTION.

## Tips

- **Read the vignette** (`vignettes/DTAtools.Rmd`) for a comprehensive feature walkthrough and examples.
- **Use `inst/extdata/` fixtures** in examples and tests — files like `clinical_dta.yaml` and `clinical_data.csv`.
- **JSON Schema performance:** Validation is row-level and compiled from specs (see `evaluateRules.R`). Changes to schema logic may need profiling.
- **GitHub CLI (`gh`):** Use for all GitHub work (PRs, issues, logs). Never scrape the web UI.
- **Avoid `air format`:** Posit Air (VS Code extension) disagrees with `styler` config. Use it for editor format-on-save only, not before commit.
