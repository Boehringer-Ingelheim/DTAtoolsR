# WP2: messages() and the HTML report (DEV-005, DEV-012)

Part of `thoughts/shared/plans/PLAN-known-defects.md`. Paste the standard brief
from §4 of that file into the agent prompt along with this file.

## DEV-005: `messages()` aborts on an "unspecified" dataset

**Root cause.**
- `check()` on a tabular dataset whose spec has no columns
  (`R/DTADataSetTabular-class.R` about 1371–1384) records
  `validation_index[[table]] <- list(status = "unspecified")`, with no
  artifact and no `validation_store` entry.
- `validation_status()` special-cases that (about line 1033), which is why
  `results()` works.
- `dta_collect_messages_for_dataset()` (`R/validationReporting.R` about 359–394)
  returns the empty frame only when the entry is `NULL` (about 365). For
  "unspecified" it falls through to `validation_errors()`, which finds nothing
  and aborts (`R/DTADataSetTabular-class.R` about 1095–1099).
- Every `messages()` method routes through this helper, including
  `messages(DTA)`.

**Fix.** Extend the early return:

```r
if (is.null(entry) || identical(entry$status, "unspecified")) {
  return(dta_empty_messages())
}
```

- `dta_empty_messages()` (about 193–206) is the ten-column zero-row schema:
  `id` integer; `dataset`, `target`, `severity`, `source`, `rule_id` character;
  `row` numeric; `column`, `keyword`, `message` character.
- `rbind` across datasets already tolerates zero-row frames.

## DEV-012: one metadata message removes all inspect detail

**Root cause.**
- `write_validation_report()` (`R/exportValidationReport.R` about 88–97) fetches
  all inspect detail in a single batched
  `DTAtools::inspect(x, id = msgs$id, ...)` inside
  `tryCatch(..., error = function(e) NULL)`. The batching is deliberate: it
  avoids O(n²) message re-collection.
- `messages(DTA)` prepends metadata rows with `dataset = NA_character_` and
  `target = "metadata"` (`R/validationReporting.R` about 480–482, built by
  `dta_metadata_messages_to_df()` in `R/DTAMetaData-helpers.R`).
- `inspect()` for a DTA (`R/validationReporting.R` about 1487–1533) looks up
  `x@datasets[["NA"]]` and aborts at about line 1502.
- The whole batch is therefore discarded. Every message falls back to the bare
  row, and `.report_inspect_panel_html()` (`R/reportRendering.R` about
  1081–1139) renders the generic "unknown" panel. Nothing says detail is missing.

**Fix, three parts.**

1. **Root cause: teach `inspect(DTA)` the metadata message.**
   - In the per-row loop, before the dataset lookup, handle
     `is.na(msg_row$dataset) && identical(msg_row$target, "metadata")` by
     building a record from the metadata object instead of aborting.
   - Read `.report_inspect_panel_html()` and the import-text helpers in
     `R/reportRendering.R` (`.report_import_expected_text` /
     `.report_import_actual_text`) first. Produce the record shape they render
     for `type = "import"`, sourcing `raw`, `reason` and the field from
     `x@metadata@import_issues`, matched by the message's column/field.
   - If a metadata message cannot be matched reliably to its issue, emit
     `type = "metadata"` with message and field only, and add a renderer branch
     that shows a plain panel for it.
   - **Report which of the two you did.**
2. **Isolation: a failure loses only its own detail.**
   - Keep the single batch as the fast path.
   - If it errors, retry once per dataset group (split `msgs$id` by
     `msgs$dataset`, with the NA group as its own group), each in its own
     `tryCatch`, and assemble whatever succeeds.
   - Do not loop per message id: that reintroduces the O(n²) cost the batch exists
     to avoid.
3. **Say so when detail is missing.** Where a message still falls back to its
   bare row, have `.report_inspect_panel_html()` state, in package-authored text,
   that detail could not be generated for this message. Keep the
   `inspect-badge-unknown` class for that case.

## Files owned

- `R/validationReporting.R`: functions `dta_collect_messages_for_dataset()`
  and the DTA `inspect()` method only. WP1 edits `dta_import_error_messages()`
  in the same file; do not touch it.
- `R/exportValidationReport.R`, `R/reportRendering.R`.
- `tests/testthat/test-DTADataSetTabular-validation.R`,
  `tests/testthat/test-validationReporting.R` (or the file where existing
  `inspect()` / report tests live; name it in your report).
- `inst/qualification/tests/test-OQ-VAL-check.R`,
  `inst/qualification/tests/test-OQ-REPORT-html.R`.
- `inst/qualification/requirements/REQ-VAL.yaml`, `REQ-REPORT.yaml`.
- Your entries in `deviations.yaml` (DEV-005, DEV-012), your CHANGELOG bullets,
  and the regenerated manifest.

## Tests: flip, add

**Developer suite.**
- New tests near the existing "unspecified" tests in
  `test-DTADataSetTabular-validation.R` (about 372–399):
  - `messages()` on an unspecified dataset returns zero rows with exactly the
    ten columns and types above, both with `as_tibble = FALSE` and with
    `as_tibble = TRUE`;
  - on a DTA mixing one specified and one unspecified dataset, `messages()`
    returns only the specified dataset's rows.
- Re-run `test-validation-summary.R` (about 206) and `test-DTA.R` (about 673),
  which already exercise unspecified datasets.
- `inspect(dta, id = <metadata message id>)` returns a record and does not
  abort. Mixing metadata and dataset ids in one call returns a record per id.
- Report built from a DTA with a metadata import error **and** a column-spec
  error:
  - the column-spec message's panel carries its real type badge
    (`inspect-badge-columnspec`), not `unknown`;
  - the metadata message has its own detail.

  Parse the HTML; do not snapshot it.
- Isolation: force `inspect()` to fail for one dataset's ids only. Use
  `testthat::local_mocked_bindings()` if no faithful real failure can be built,
  and say which you used. Assert the other dataset's messages keep their detail
  and the failed ones show the "could not be generated" text.

**Qualification suite.**
- `test-OQ-VAL-check.R`, OQ-VAL-026 (about 221–240): remove
  `qa_known_deviation("DEV-005", ...)`. Add `qa_step`s that `messages(ds)`
  returns 0 rows and that its column names equal the schema above.
- `test-OQ-REPORT-html.R`, OQ-REPORT-006 (about 226–268):
  - the step at about 259–262 asserting `inspect-badge-unknown` flips to
    `inspect-badge-columnspec`;
  - remove `qa_known_deviation("DEV-012", ...)`;
  - add a step that the metadata message's panel has detail;
  - retitle if the title describes the defect.
- `REQ-VAL.yaml` (REQ-VAL-020, -023) and `REQ-REPORT.yaml` (REQ-REPORT-006,
  notes at about line 84): rewrite any "Pinned, not endorsed" text to the
  correct behaviour.

**Register.** DEV-005, DEV-012: set `status: closed` and `closed: <date>`.

**CHANGELOG** (`### Fixed`), one bullet each.
- DEV-005: `messages()` no longer aborts for a dataset with an empty
  specification, which used to break any loop collecting messages across
  datasets.
- DEV-012: the HTML report keeps its click-to-inspect detail when the transfer
  has a metadata issue.

## Verification

```bash
Rscript --no-init-file -e "devtools::test(filter = 'validation|Reporting|report|DTA')"
Rscript --no-init-file -e "devtools::test()"
Rscript --no-init-file -e "pkgload::load_all(quiet = TRUE); r <- DTAtools::run_qualification(tempfile('q'), stages = 'OQ', scale = 'quick', formats = 'md', filter = 'VAL|REPORT', quiet = TRUE); print(summary(r))"
Rscript --no-init-file -e "devtools::test(filter = 'qualification')"
Rscript .github/scripts/sync_qualification_manifest.R --check
```
