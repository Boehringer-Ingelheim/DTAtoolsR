# Plan: scaffolding a private template repository, and closing two lint false-negatives

## What the request asked for, and what the code already has

The ask was four things: a function that creates a private template repo, tests
for it, a linting function that says whether a template repository is valid, and
documentation that is up to date.

**The linter already exists, and it is thorough.** `validate_template(path,
strict = FALSE, kinds = NULL)` (`R/validateTemplate.R:1037`) already accepts a
**directory**, not just a file. Given one it builds a cross-file index, resolves
`extends:` chains, resolves `datasets[].template` imports, party-slot profile
allow-lists and `values_from:` vocabulary bindings across files, dry-run
instantiates every non-abstract creation template, and reports duplicate
`kind+id+version` across files. It emits **25 distinct issue codes** at two
severities, returns a tidy `file/kind/id/version/severity/code/message` frame,
and `strict = TRUE` aborts with a `cli` summary — the one line a CI job needs.
There is even a ready-to-copy GitHub Actions workflow shipped at
`inst/extdata/templates/validate-templates.yml`, with Bitbucket Pipelines
translation notes in its header.

Writing a second repository linter would duplicate all of that. **This plan does
not add one.** What it does instead is close two false-negatives I verified by
running the function.

**The creation function genuinely does not exist.** ✓ VERIFIED: nothing in `R/`
or `inst/shiny/dta_app/R/` creates, scaffolds or initialises a template, a
template file or a template directory. The package's template API is read-only —
`validate_template`, `export_with_template`, `dta_template_placeholders` are the
only template-related exports (`NAMESPACE`).

**The documentation gap is narrower than it looks.** `vignettes/private-templates.Rmd`
is 749 lines and covers authoring every kind, all three source schemes, the git
credential, versioning, parties, vocabularies, provenance/rebase, and CI. What it
does **not** contain is a "start a new repository from scratch" path: the
directory layout, the filename conventions as a rule rather than as inferable
examples, and a minimal set a reader can copy and run.

## The two verified lint false-negatives

Both were reproduced by running `validate_template()` directly, not inferred.

### 1. An empty directory passes `strict = TRUE` silently

```
empty dir rows: 0
strict on empty dir: NO ERROR - an empty repository passes silently
```

Point the shipped CI workflow at the wrong path — a typo, a wrong working
directory, a checkout that placed the templates one level down — and the job
goes **green while validating nothing**. That is the precise failure mode a
repository linter exists to prevent, and it is the default outcome today.

### 2. The scan is non-recursive, silently

```
nested-only dir rows: 0 (0 means the scan is non-recursive and missed it)
```

✓ VERIFIED that this is *consistent*, not a bug: the app's own loader
(`inst/shiny/dta_app/R/template_index.R:215-219`) is also `recursive = FALSE`.
So a template repository **must be flat**, and a file in a subdirectory is
invisible to both the linter and the app. That rule is currently written down
nowhere, and an author who organises by study or supplier into subfolders gets
silence from both.

## Technical choices

- **Ship the starter set as package data, not as strings built in R.** A new
  `inst/extdata/template-repo-skeleton/` holds one valid file of each of the four
  kinds plus a `README.md`. `create_template_repo()` copies it. This keeps the
  starter templates testable in place (a test validates the skeleton directory
  itself) and means the scaffold cannot drift from what the validator accepts.

- **Write the hidden paths programmatically, do not ship them.** `.github/` and
  `.gitignore` inside `inst/` are not reliably carried through `R CMD build`.
  The CI workflow is copied from the already-installed
  `inst/extdata/templates/validate-templates.yml`, and `.gitignore` is written
  from a short character vector in the function.

- **The scaffold's contract is a round trip.** The directory
  `create_template_repo()` produces must satisfy
  `validate_template(path, strict = TRUE)` with zero issues. That is the single
  most valuable test in this plan: it ties the two halves of the request
  together and it will fail loudly if either side drifts.

- **`no_templates` is severity `error`, not `warning`.** A warning would leave
  the CI false-negative in place, since `strict = TRUE` only aborts on errors.
  ✓ VERIFIED safe: every existing test that calls `validate_template()` on an
  empty directory (`test-validateTemplate.R:70-79`) is an argument-validation
  test that aborts before the scan, so none of them changes behaviour.

## Tasks

### Task 1 — `inst/extdata/template-repo-skeleton/` (new package data)

Five files, self-consistent, cross-referencing, and collectively valid.

- [ ] `starter_terms.dta-vocabulary.yaml` — `kind: dta_vocabulary`, a handful of
  code/label terms.
- [ ] `starter_dataset.dta-dataset-template.yaml` — `kind: dta_dataset_template`,
  one tabular dataset, at least one column bound to the vocabulary with
  `values_from: starter_terms@1.0` so the starter exercises a cross-file
  reference rather than four unrelated files.
- [ ] `starter_supplier.dta-party.yaml` — `kind: dta_party_profile`, `role: supplier`.
- [ ] `starter.dta-template.yaml` — `kind: dta_creation_template`, a `base:` with
  minimal metadata, one `datasets:` entry importing the dataset template, and a
  `party_slots:` entry offering the supplier profile.
- [ ] `README.md` — what each file is, the flat-layout rule, how to point the app
  at this directory, and how to validate.

Every `version:` must be **quoted** — the validator raises `version_unquoted` at
severity `error` otherwise.

Ids are prefixed `starter_` so they cannot collide with the shipped
`inst/extdata/templates/` family if a user configures both roots.

### Task 2 — `create_template_repo()` in a new `R/createTemplateRepo.R`

```r
create_template_repo(path, examples = TRUE, ci = TRUE, overwrite = FALSE)
```

- [ ] `path` — a single non-empty string. Created recursively when missing.
- [ ] `examples` — `TRUE` copies the four starter template files and the
  skeleton `README.md`; `FALSE` writes only the README, `.gitignore` and CI.
- [ ] `ci` — `TRUE` writes `.github/workflows/validate-templates.yml`, copied
  from `system.file("extdata", "templates", "validate-templates.yml")`.
- [ ] `overwrite` — `FALSE` (default) aborts via `cli::cli_abort()` naming every
  file that already exists, and writes **nothing**; the directory must come out
  of a failed call exactly as it went in. `TRUE` replaces them.
- [ ] Returns `invisible(normalizePath(path, winslash = "/"))`.
- [ ] Reports what it wrote with `cli::cli_inform()`, and closes with the exact
  next command to run — `validate_template("<path>", strict = TRUE)`.
- [ ] Roxygen with `@export` and a **runnable** `@examples` writing under
  `tempdir()` (examples are executed by `R CMD check`; never `\dontrun{}` here).
- [ ] Added to `Collate:` in `DESCRIPTION` if that field lists R files.

Dependencies: `cli`, `tools`, `utils` — all already in `Imports:`. No new dependency.

### Task 3 — Two new checks in `validate_template()`

- [ ] **`no_templates`**, severity `error`, one row, emitted only when `path` is a
  **directory** and the scan found zero files of any kind. `file` is the
  directory, `kind`/`id`/`version` are `NA`. Message names the four filename
  suffixes and states that the scan is not recursive, because a wrong path and a
  nested layout are the two ways to arrive here.
- [ ] **`template_in_subdirectory`**, severity `warning`, one row per offending
  file, emitted when `path` is a directory and a recursive scan finds a
  template-named file that the non-recursive scan did not. Message must say that
  the app's own loader will not see it either, so an author knows this is not a
  linter quirk.
- [ ] Both documented in the roxygen block's issue-code list, if it has one;
  otherwise in `@details`.
- [ ] The recursive probe must not walk into `.git/` or `renv/` — cheap
  `list.files(recursive = TRUE)` filtered by the same kind patterns, then
  `setdiff()` against the non-recursive result.

### Task 4 — Tests

New `tests/testthat/test-createTemplateRepo.R`:

- [ ] **The round trip**: `create_template_repo(tmp)` then
  `validate_template(tmp, strict = TRUE)` raises nothing, and the non-strict call
  returns zero rows. This is the anchor test.
- [ ] The scaffold writes one file of each of the four kinds — asserted by
  reading each back with `yaml::read_yaml()` and checking its `kind`, not by
  `file.exists()`.
- [ ] `ci = TRUE` writes `.github/workflows/validate-templates.yml` and its
  content contains `validate_template`; `ci = FALSE` does not create `.github`.
- [ ] `examples = FALSE` produces a directory with no template files — and
  therefore one that `validate_template()` now flags `no_templates` (this ties
  Tasks 2 and 3 together).
- [ ] `overwrite = FALSE` against a populated directory aborts with a
  `cli`-authored message (`expect_error(..., class = "rlang_error")`) and leaves
  every existing file byte-for-byte unchanged.
- [ ] `overwrite = TRUE` replaces them.
- [ ] The function creates a missing parent directory, and errors on a `path`
  that is an existing *file*.
- [ ] The skeleton shipped in `inst/extdata/template-repo-skeleton/` is itself
  clean: `validate_template()` on it returns zero rows. Guards the assets
  directly, independent of the copy logic.

Added to `tests/testthat/test-validateTemplate.R`:

- [ ] `no_templates` fires for an empty directory, at severity `error`, and
  `strict = TRUE` now aborts there — the regression guard for the verified CI
  false-negative.
- [ ] `no_templates` does **not** fire for a single-file `path`.
- [ ] `template_in_subdirectory` fires for a template one level down, names that
  file, and is a `warning` — so it does not turn CI red on its own.
- [ ] A directory with both a top-level template and a nested one reports the
  nested one and still validates the top-level one normally.

### Task 5 — Documentation

- [ ] `vignettes/private-templates.Rmd` — a new section **"Starting a new
  template repository"**, placed before the existing "Authoring a creation
  template" (line ~165), since it is the step that comes first. It covers:
  `create_template_repo()`, the flat-layout rule and why (both the linter and the
  app loader are non-recursive), the four filename suffixes as a table, and the
  `validate_template(".", strict = TRUE)` loop. Ends by pointing at the existing
  CI section rather than repeating it.
- [ ] Extend the existing CI section (line ~717) with the `no_templates` check,
  framed as what it protects against: a workflow pointed at the wrong path used
  to pass.
- [ ] `README.md` — the "Private templates" section (line ~617) gains one
  sentence naming `create_template_repo()` as the starting point.
- [ ] `man/` — regenerate with `roxygen2::roxygenise()`, reading the pinned
  version from `DESCRIPTION`'s `Config/roxygen2/version`. Never hand-edit.
- [ ] `CHANGELOG.md` — one entry under `## [Unreleased]` / `### Added` for the
  scaffold, and one under `### Fixed` for the two lint false-negatives.
- [ ] No `DESCRIPTION` version bump — #85 shipped a comparable feature without
  one, and a bump has not been requested.
- [ ] No `_pkgdown.yml` change — ✓ VERIFIED the file does not exist.

## Success criteria

### Automated

- [ ] `testthat::test_local(filter = "createTemplateRepo")` — all pass.
- [ ] `testthat::test_local(filter = "validateTemplate")` — all pass, including
  the pre-existing 822 lines.
- [ ] Full suite — no new failures.
- [ ] `styler::style_pkg()` leaves no diff.
- [ ] `roxygen2::roxygenise()` leaves no diff after being run once.
- [ ] `R CMD check` — the new `@examples` block executes cleanly.
- [ ] `pre-commit run --all-files` passes.
- [ ] `bump_version.R --check` and `check_manifest.R` stay OK.

### Manual

- [ ] `create_template_repo(tempfile())` then open the result: the README reads
  as instructions to a newcomer, not as a file listing.
- [ ] Point `DTATOOLS_TEMPLATE_SOURCES` at the scaffolded directory and start the
  app: the starter template appears in "Create new from template" and builds a
  document.

## Risks

- **`no_templates` at severity `error` is a behaviour change to an exported
  function.** Someone calling `validate_template()` on an empty directory in
  `strict` mode gets an abort where they previously got silence. That is the
  entire point, but it belongs in the changelog under `### Fixed` with the
  reasoning, not buried.
- **The starter set must stay valid as the validator evolves.** Mitigated by the
  test that validates the shipped skeleton directly, so a future validator change
  that would reject it fails a test rather than shipping a broken scaffold.
- **`system.file()` under `pkgload::load_all()` vs an installed package.** The
  scaffold resolves its assets with `system.file()`; the test harness note in
  `tests/testthat/helper-shinyapp.R:20-31` documents the trap. The function must
  fail loudly (`cli::cli_abort`) if the skeleton cannot be located, never
  silently produce an empty repository.

## Out of scope

- A second, separate repository linter — `validate_template()` already is one.
- Making the scan recursive. Both the linter and the app agree it is flat;
  changing that is a template-engine decision, not a documentation fix. The new
  warning surfaces the mismatch instead.
- Exporting `create_dta_from_template()` — noted as an API gap by the docs audit,
  but it is Shiny-app internal and out of this request's scope.
- A `_pkgdown.yml` site.
