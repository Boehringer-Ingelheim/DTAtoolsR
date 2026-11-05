# WP3: Metadata import issues survive a round trip (DEV-010)

Part of `thoughts/shared/plans/PLAN-known-defects.md`. Paste the standard brief
from §4 of that file into the agent prompt along with this file. Implements
decision **D5** (recommended: also cover the top-level free-text date).

## Root cause

- The `DTAMetaData` constructor (`R/DTAMetaData-class.R` about 104–117)
  re-parses character dates on every construction. `.parse_metadata_date()`
  (about 307–341) returns the resolved `Date` plus an import issue built by
  `.metadata_import_issue()` (about 275). Issues live in the `import_issues`
  property (about 151).
- `as.list()` for `DTAMetaData` (`R/DTAMetaData-helpers.R` about 106–136) writes
  `date = .date_to_iso(x@date)` and `transmission = lapply(x@transmission,
  .date_to_iso)`. The raw string, held only in `import_issues`, is never written.
- The reader (`R/DTA-class.R` about 1189) is `do.call(DTAMetaData, x$metadata)`.
  It re-runs the constructor, so **any raw string written back reproduces its
  own issue**. The fix belongs entirely in the writer.
- The DEV-009 case (top-level `date: "after approval"`, `reason =
  "not_convertible"`, stored as NA) has the same loss: `as.list()` drops the NA
  date through `.dta_metadata_field_is_absent()` (about 130–135).

## Fix: write the raw text back (option a)

In `as.list()` for `DTAMetaData`, after the list is built:

1. For each record in `x@import_issues` with `reason` in
   `c("trailing_residue", "not_convertible")` and `column` in `"date"`,
   `"transmission$date_first_transfer"` or `"transmission$date_last_transfer"`:
   - **Staleness guard.** Substitute only if the field's current value is still
     what that raw string resolves to. For `trailing_residue`, the current Date
     equals `.parse_metadata_date(raw)`'s value; for `not_convertible`, the
     current value is NA or absent. Otherwise the user changed the field after
     load: write the current value, and do not resurrect the old issue.
   - Write `raw` into that field in the output list.
2. Make sure the `not_convertible` top-level `date` survives the absent-field
   filter: substitute before filtering, or exempt a substituted field.
3. Keep `import_issues` itself out of the output. The existing contract says it
   is derived, not written (`tests/testthat/test-DTAMetaData.R` about 328–330;
   the comment at `R/DTAMetaData-helpers.R` about 123). Machine-owned
   provenance fields are untouched.

The constructor then recreates the same issue with the same `raw` on read. No
reader change, no YAML schema change.

**Why not serialise `import_issues` (option b).** It needs a new YAML key and
reader handling, breaks the "as.list omits import_issues" contract, and lets a
hand-edited file claim an issue that disagrees with its own data.

**Check before finishing.** Grep `inst/shiny/dta_app` for consumers of
`as.list(<metadata>)` or of the written date fields: template, rebase and
versioning code, and the metadata edit form. A raw string such as
`"2026-12-31 at the earliest"` can now appear where only ISO dates appeared
before. Report every consumer and whether it tolerates that. Do not edit the
app; if one breaks, stop and report.

## Files owned

- `R/DTAMetaData-helpers.R` (the `as.list()` method and any small private
  helper it needs). Leave `R/DTAMetaData-class.R` untouched unless a helper
  must be exported from it; report it if so.
- `tests/testthat/test-DTAMetaData.R` (and `test-metadata-contract.R` only if
  an assertion there genuinely changes; report it).
- `inst/qualification/tests/test-OQ-META-metadata.R`.
- `inst/qualification/requirements/REQ-META.yaml`.
- Your entry in `deviations.yaml` (DEV-010), your CHANGELOG bullet, and the
  regenerated manifest.

## Tests: flip, add

**Developer suite (`test-DTAMetaData.R`).** Add round-trip tests through
`write_yaml`/`read_yaml` of a DTA, reading the file back, not through
`as.list` alone:
- `transmission$date_last_transfer = "2026-12-31 at the earliest"` round-trips:
  1 `trailing_residue` issue with the same `raw`, and the Date 2026-12-31.
- Top-level `date = "after approval"` (D5) round-trips: date NA and 1
  `not_convertible` issue with the same `raw`.
- Staleness: load, then set `date_last_transfer` to a new Date, then write and
  read. The new value is written, and no issue comes back.
- A clean ISO date still writes as exactly the ISO string. The existing
  assertion at about 312–331 must hold unchanged, including "omits import_issues".

**Qualification suite (`test-OQ-META-metadata.R`, OQ-META-018, about 424–458).**
- The step at about 433–436 expects `as_list$transmission$date_last_transfer ==
  "2026-12-31"`. It flips to the raw string.
- The step at about 445–448 expects 0 issues after restore. It flips to 1, with
  matching `reason` and `raw`.
- Keep the step that the date still resolves to 2026-12-31.
- Remove `qa_known_deviation("DEV-010", ...)`.
- OQ-META-007 (about 192–210) asserts `md@date` is NA for "after approval".
  That stays true; re-run it.
- `REQ-META.yaml`: REQ-META-017 and REQ-META-018 (notes at about 219). Rewrite
  any "Pinned, not endorsed" text to: an import issue on a metadata date
  survives a write and a read.

**Register.** DEV-010: set `status: closed` and `closed: <date>`. If D5 is
implemented, say in the closing note that the DEV-009 residual is covered too.

**CHANGELOG** (`### Fixed`): a questionable metadata date no longer turns into
a clean one after a single save and reload; the original text is written back,
so the issue is still reported.

## Verification

```bash
Rscript --no-init-file -e "devtools::test(filter = 'DTAMetaData|metadata')"
Rscript --no-init-file -e "devtools::test()"
Rscript --no-init-file -e "pkgload::load_all(quiet = TRUE); r <- DTAtools::run_qualification(tempfile('q'), stages = 'OQ', scale = 'quick', formats = 'md', filter = 'META|TMPL|APP', quiet = TRUE); print(summary(r))"
Rscript --no-init-file -e "devtools::test(filter = 'qualification')"
Rscript .github/scripts/sync_qualification_manifest.R --check
```

`TMPL` and `APP` are in the filter on purpose: template and app code read
written metadata.
