# PLAN: Fix the ten open known defects (DEV register)

Status: draft for review, 2026-11-04. No code has been changed.

## 1. Goal

Close every `status: open` entry in `inst/qualification/deviations.yaml`, so the
qualification register describes a package with no known open defects:

| ID | Severity | One line | Work package |
| --- | --- | --- | --- |
| DEV-001 | minor | `BESTw.` format infers `Int` instead of `Num` | WP1 |
| DEV-004 | major | Declared Int typed differently by the in-memory and streaming engines | WP1 |
| DEV-006 | major | Declared Int column silently accepts 5000000000 | WP1 |
| DEV-005 | major | `messages()` aborts on a dataset whose spec has no columns | WP2 |
| DEV-012 | major | One metadata error strips inspect detail from every report message | WP2 |
| DEV-010 | major | A metadata import issue is lost after one write and read | WP3 |
| DEV-003 | minor | `export_column_value_table()` leaks a base R error for a bad argument | WP4 |
| DEV-007 | minor | `write_table_to_file()` returns a list where it documents a checksum | WP4 |
| DEV-014 | minor | The Word specification round trip cannot read its own output | WP4 |
| DEV-015 | minor | A specification written to JSON cannot be read back | WP4 |

Out of scope: DEV-002 and DEV-016 (`status: pending`, no binding), every
`LIM-nnn` limitation, and any version bump.

The per-package briefs are in `thoughts/shared/plans/known-defects/`:

- `WP1-declared-int-typing.md` (DEV-001, DEV-004, DEV-006)
- `WP2-messages-and-report.md` (DEV-005, DEV-012)
- `WP3-metadata-roundtrip.md` (DEV-010)
- `WP4-export-and-spec-io.md` (DEV-003, DEV-007, DEV-014, DEV-015)

Each brief is self-contained: root cause with file:line, the fix, files owned,
tests to flip and add, qualification-suite changes, verification.

## 2. Decisions

Root causes come from three read-only investigations of this worktree, and the
key lines were re-read on the main thread. Line numbers are anchors, not
contracts: locate code by function name.

**Decided here (implementers follow, do not redesign):**

- Every package follows `inst/qualification/docs/maintaining.md` → "You fixed a
  defect that the register records": close the entry, replace the
  `qa_known_deviation()` binding with a test asserting the *fix*, rewrite any
  requirement whose `notes:` say "Pinned, not endorsed", add a CHANGELOG entry
  under `## [Unreleased]` → `### Fixed`.
- No package bumps the version. No package touches `man/`, `NAMESPACE` or
  `renv.lock` by hand.
- `OQ-CORE-006` currently uses DEV-001 as its live example of a reproduced
  deviation. Once every defect is closed there is no open entry left to bind,
  so WP1 rewrites it to test the mechanism against a fixture register.

**Needs the owner's confirmation before the package starts.** Each brief
implements the recommended option; the alternative is stated so it can be
switched without re-planning.

| # | Package | Recommended | Alternative |
| --- | --- | --- | --- |
| D1 | WP1 | For a declared Int, a value that is fractional, non-finite or outside int32 becomes an **import error** (value NA, raw text kept); storage is **always** R integer, decided by the declaration, never by the data. Fixes DEV-004 and DEV-006 together. Changes today's behaviour, where a fractional value stays double and is reported on the column-spec axis. | Only out-of-range values become import errors; fractional handling unchanged. Fixes DEV-006 only; DEV-004 stays open. |
| D2 | WP4 | `write_table_to_file()` returns `md5sum` as the bare checksum (or `NA`), plus new `n_rows` and `n_cols` elements. Only tests read the nested form today. | Change the documentation to describe the list. Leaves the `NA`-vs-list type mismatch. |
| D3 | WP4 | Add an exported `import_specs_from_json()` reader, and stop the JSON writer emitting `{}` for unset properties. | Document JSON as write-only and close DEV-015 as a limitation. |
| D4 | WP4 | The Word "Format" cell carries the backend prefix (`SAS $10.`); the reader accepts empty cells and, for older documents, an unprefixed format by taking the backend from the Type cell. | Reader-only fix: infer the prefix from Type, leave the writer as is. |
| D5 | WP3 | The DEV-010 fix also covers the top-level free-text date left over from DEV-009 (`not_convertible`, stored as NA): the raw text is written back so the issue survives a round trip. | Fix `trailing_residue` only, as the register entry is worded. |

## 3. Work packages and file ownership

All four can run in parallel; no two packages own the same function.

| WP | Owns (production code) | Owns (tests and suite) | Shared with |
| --- | --- | --- | --- |
| WP1 | `R/DTAColumnSpecStructureSAS-class.R`, `R/importConversion.R`, `R/columnSpecChecks.R`; `dta_import_error_messages()` in `R/validationReporting.R` | `test-DTAColumnSpecStructureSAS.R`, `test-importConversion.R`, `test-streaming-parity.R`; `test-OQ-TYPE-declared.R`, `test-OQ-CORE-runner.R`, `helper-parity.R`; `REQ-TYPE.yaml`, `REQ-CORE.yaml` | WP2 also edits `R/validationReporting.R`, in different functions |
| WP2 | `dta_collect_messages_for_dataset()` and `inspect()` for DTA in `R/validationReporting.R`; `R/exportValidationReport.R`; `R/reportRendering.R` | `test-DTADataSetTabular-validation.R`, `test-validationReporting.R`; `test-OQ-VAL-check.R`, `test-OQ-REPORT-html.R`; `REQ-VAL.yaml`, `REQ-REPORT.yaml` | WP1, as above |
| WP3 | `R/DTAMetaData-helpers.R` | `test-DTAMetaData.R`; `test-OQ-META-metadata.R`; `REQ-META.yaml` | none |
| WP4 | `R/exportFunctions.R`, `R/DTADataSetTabular-class.R` (`write_table_to_file()` only), `R/DTAColumnSpecCollection-class.R` | `test-exportFunctions.R`, `test-writeColumnsToJson.R`, new round-trip tests; `test-OQ-EXPORT-documents.R`, `test-OQ-EXPORT-tables.R`, `test-OQ-FILE-writing.R`, `test-OQ-ROBUST-isolation.R`, `test-PQ-PERF-workflows.R`; `REQ-EXPORT.yaml`, `REQ-FILE.yaml`, `REQ-SPEC.yaml`, `_api.yaml`, `_index.yaml`; vignette | none |

Every package also edits these shared files, each in its own lines:

- `inst/qualification/deviations.yaml`: its own entries only.
- `CHANGELOG.md`: its own bullets under `### Fixed`, and WP4 also adds one
  under `### Added`.
- `inst/qualification/baseline/inst-files.sha256`: regenerated, never hand-edited.

**Branching.** One branch per package, cut from `dev`, one PR each into `dev`.
Each PR must be green on its own, so each package closes its own register
entries and regenerates the manifest. After a PR merges, the next one merges
`dev` and resolves:

- `CHANGELOG.md` by keeping both sides;
- `deviations.yaml` normally needs nothing, since the hunks are distinct;
- the manifest by re-running `Rscript .github/scripts/sync_qualification_manifest.R`,
  never by hand-merging hashes.

**Suggested merge order:** WP2 → WP3 → WP4 → WP1. The small, independent
packages go first. WP1 goes last because it is the largest behaviour change
and depends on decision D1.

## 4. Standard agent brief (paste into every package prompt)

> Work ONLY in the git worktree you were given; never read or edit
> `C:\Users\Tom\workspace\DTAtoolsR` itself (a stale checkout). First put a
> file `.Renviron` containing `RENV_CONFIG_AUTOLOADER_ENABLED=FALSE` in the
> worktree root (untracked, gitignored); never run `renv::restore()`. Rscript is
> `"C:\Program Files\R\R-4.5.1\bin\Rscript.exe"` if not on PATH. Read
> `thoughts/shared/plans/PLAN-known-defects.md` and your own brief in
> `thoughts/shared/plans/known-defects/`, then
> `inst/qualification/docs/maintaining.md` sections "You changed what an existing
> function does", "You fixed a defect that the register records" and "Writing a
> test case", then the cited source lines.
>
> Edit only the files your brief lists as owned. Rules:
> - Conditions use `cli::cli_abort()` / `cli::cli_warn()`, and calls are namespaced.
> - Never use `expect_error()` without `class =` or a package-authored `cli`
>   string, and never assert translated base-R text.
> - In the qualification suite, use `qa_step()` for every expected-versus-observed
>   comparison. Expected values come from the specification, never pasted from
>   output, and there are no snapshots.
> - A file-producing function is checked by reading the file back.
>
> Before reporting, run:
> 1. `Rscript .github/scripts/style.R`;
> 2. `Rscript -e "roxygen2::roxygenise()"`, roxygen2 8.1.0 per `DESCRIPTION`;
> 3. `Rscript .github/scripts/sync_qualification_manifest.R`;
> 4. the verification block in your brief.
>
> Do not commit, push or open a PR: leave the changes in the working tree. Report:
> - what changed, per defect;
> - the CHANGELOG text you added;
> - test counts before and after;
> - **any deviation from the brief, instead of approximating**;
> - any new surprising behaviour, as a candidate DEV or LIM entry.
>
> Never bump the version. Never add AI attribution anywhere, including code
> comments and the CHANGELOG.

## 5. After each package (main thread)

1. `r-verify` on the worktree: `devtools::test()` in full, plus the brief's
   qualification filter.
2. `r-review` on the diff.
3. The owner approves the commit; then PR into `dev`. Wait for every workflow
   (all `R-CMD-check` OS legs, `r-style`, `pre-commit`, `manifest-sync`) before
   merging, polling `gh run list --branch`.

## 6. Done when

- `deviations.yaml` has no `status: open` entry.
- A `standard` qualification run returns `PASS` (or `PASS WITH NOT-EXECUTED
  TESTS` only for optional tooling) with meta checks M1–M12 green.
- `devtools::test()` is green with no new skips, and `R CMD check` (`NOT_CRAN=true`,
  `--no-manual`) is `Status: OK`.
- The validation deck's defect slide is updated (10 open → 0).

## 7. Risks

- **WP1 changes user-visible validation results** (D1). A delivery with
  `8.5` in an Int column moves from a column-spec error to an import error, and
  the value becomes NA in the typed table. The CHANGELOG entry must say so
  plainly.
- **Arrow fast path.** It narrows only values of nine digits or fewer to int32.
  WP1 must prove the Arrow route and the R route still agree, which the existing
  parity tests in `test-importConversion.R` are for.
- **Parallel merges.** Expect conflicts only in `CHANGELOG.md` and the manifest;
  anything else conflicting means a package edited outside its ownership.
- **A closed defect leaves no live example.** Handled by the OQ-CORE-006
  rewrite in WP1. If that cannot be done against a fixture register, WP1 stops
  and reports rather than binding another real defect.
