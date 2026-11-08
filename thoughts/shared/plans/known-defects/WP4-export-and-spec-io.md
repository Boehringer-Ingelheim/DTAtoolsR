# WP4: Export and specification I/O (DEV-003, DEV-007, DEV-014, DEV-015)

Part of `thoughts/shared/plans/PLAN-known-defects.md`. Paste the standard brief
from §4 of that file into the agent prompt along with this file. Implements
decisions **D2**, **D3** and **D4** (recommended options). Do the four defects
in the order below; each is independently testable.

## DEV-003: `export_column_value_table()` has no argument check

**Root cause.** `R/exportFunctions.R`, `export_column_value_table()` (about
229–238). Its first statement is `DTAColumnSpecCollection@columns[[id]]`, so a
plain list raises a base R error from `@`.

**Fix.** Copy the guard that `export_specs_table()` uses (same file, about
45–47) to the top of the function: an `inherits(..., "DTAtools::DTAColumnSpecCollection")`
test and a `cli::cli_abort()` naming the expected class. Leave the existing
aborts for an unknown `id` and for no values alone (OQ-EXPORT-019,
`tests/testthat/test-exportFunctions.R` about 146–165).

**Tests.**
- `tests/testthat/test-exportFunctions.R` about 127–134: replace
  `expect_false(inherits(err, "rlang_error"))` with
  `expect_error(..., class = "rlang_error")` plus the package `cli` text, as the
  comment there says.
- `inst/qualification/tests/test-OQ-EXPORT-documents.R`, OQ-EXPORT-001 (about
  104–119): remove `qa_known_deviation("DEV-003", ...)` and assert a classed cli
  condition.
- `REQ-EXPORT.yaml`, REQ-EXPORT-001 (about 12–27): it is worded "observed, not
  endorsed". Rewrite it to the correct behaviour.

## DEV-007: `md5sum` holds a list (D2)

**Root cause.** In `write_table_to_file()` (`R/DTADataSetTabular-class.R` about
600–610), the result of `write_metadata()` is assigned to `md5sum`.
`write_metadata()` (`R/exportFunctions.R` about 283–321) returns
`list(md5sum, <rows>, <cols>)`. The roxygen `@return` (about 450–453) documents
a bare checksum or `NA`.

**Fix (D2).**
- Assign the result to a local.
- Return `md5sum` as the bare checksum string, or `NA_character_` when
  `get_md5sum = FALSE`.
- Add new `n_rows` and `n_cols` elements. Populate them in both branches: from
  `write_metadata()`'s result when it runs, otherwise from the table written.
  Check that the two sources agree.
- Update the `@return` roxygen and run `roxygenise()`.

**Who reads it.** No caller in `R/` or `inst/shiny/dta_app`; examples, README
and vignette ignore the value. Only qualification tests read it. Update each to
the new shape:
- `test-OQ-FILE-writing.R` about 117–160: these read `$md5sum$md5sum`,
  `$n_rows` and `$n_cols` on the nested list, and OQ-FILE-043 at about 137–156
  pins `class == "list"`.
- `test-OQ-EXPORT-tables.R` about 198–221: reads `[[2]]` / `[[3]]` at about 203.
- `test-OQ-ROBUST-isolation.R` about 98.
- `test-PQ-PERF-workflows.R` about 229–234: reads `[[2]]` / `[[3]]` at about 234.

`tests/testthat/test-exportFunctions.R` about 184–187 tests `write_metadata()`
directly and stays as is.

**Tests to add.**
- `tests/testthat`: `md5sum` is a single string equal to an **independent**
  digest of the written file (`tools::md5sum()` on it), and `n_rows` / `n_cols`
  match the table.
- With `get_md5sum = FALSE`, `md5sum` is `NA`.
- The documented idiom `if (!is.na(res$md5sum))` evaluates without error in
  both branches.

**Qualification.**
- OQ-FILE-043: remove `qa_known_deviation("DEV-007", ...)` and assert the
  documented shape.
- REQ-FILE-025 and REQ-FILE-027 (`REQ-FILE.yaml` about 333–340): rewrite.
- The DEV-007 workaround text becomes history once the entry is closed.

## DEV-014: Word specification round trip (D4)

**Root cause.**
- The writer is in `R/exportFunctions.R` (helpers inside `export_specs_table()`,
  about 60–80). `get_spec_type()` writes `paste(backend, type)`, e.g. `SAS Char`.
  `get_spec_format()` writes the format **without** its backend prefix, and NA
  when the format is unset.
- The reader, `columns_specs_from_word()` (`R/DTAColumnSpecCollection-class.R`
  about 498–583), passes the cell to `DTAColumnSpec()`. That reaches
  `DTAColumnSpecStructureFactory` (`R/00_helpers.R` about 518), whose
  `__extract_prefix_and_rest__()` (about 43–54) aborts on `""` and on an
  unprefixed format.

**Fix (D4).**
1. Writer: `get_spec_format()` returns `paste(structure@backend, structure@format)`
   when a format is set, mirroring `get_spec_type()`. When unset, keep today's
   output and verify what the reader receives.
2. Reader, per row, before building the spec:
   - map a format cell of `NA`, `""` or `"NA"` to `NULL`;
   - if a non-empty format has no recognised backend prefix, prepend the prefix
     of that row's Type cell. That keeps documents written by older versions
     readable.

The only consumer of this table is `columns_specs_from_word()`. The docx
builder in `R/documentBuilders.R` (about 655–720) is separate and has no Format
column; confirm it is unaffected.

**Tests.**
- `tests/testthat` has no `columns_specs_from_word()` test today. Add a round
  trip, `export_specs_table()` then `columns_specs_from_word()`, for a
  collection with:
  - a column with no format;
  - a column with an explicit format;
  - Char, Int and Num types.

  Assert the ids, types, formats and lengths read back equal the original.
- Add an old-style document: a hand-built table with an **unprefixed** format,
  built the way OQ-EXPORT-025 builds its fixture
  (`test-OQ-EXPORT-documents.R` about 552ff). It must read back correctly.
- Qualification OQ-EXPORT-026 (about 593–615):
  - the "aborts" check (about 607–610) and the `grepl("prefix")` step (about
    611–612) flip to a successful round trip with equal fields;
  - remove `qa_known_deviation("DEV-014", ...)`.
- Confirm OQ-EXPORT-025 still passes.
- REQ-EXPORT-019 notes (`REQ-EXPORT.yaml` about 233–242): rewrite.

## DEV-015: JSON specification round trip (D3)

**Root cause.**
- `write_columns_to_json()` (`R/DTAColumnSpecCollection-class.R` about 725–735)
  calls `jsonlite::write_json(as.list(columns), auto_unbox = TRUE)`.
- `as.list()` for `DTAColumnSpec` (`R/DTAColumnSpec-class.R` about 375–388)
  keeps unset properties as `NULL`, and jsonlite writes `NULL` as `{}`. That
  reads back as an empty list, which the `class_*_or_null` properties reject.
- There is no JSON reader. `import_specs_from_json` is commented out as a TODO
  (about 463–483), and it contains two bugs: a bare `!is.null` and an
  unnamespaced `cli_abort`.

**Fix (D3).**
1. Writer: before `write_json`, drop `NULL` entries from each column's list. Do
   not change `as.list()` itself, so YAML output is untouched. Check whether
   the structure-level list already omits unset values; it should.
2. Reader: a new exported `import_specs_from_json(file)`, mirroring
   `import_specs_from_yaml()` (about line 299). Read with
   `jsonlite::fromJSON(file, simplifyVector = FALSE)`, then call
   `specs_from_list(columns, rules)` (about line 343).
   - Verify what top-level keys `as.list(<collection>)` writes, and that
     **rules** round-trip, including an empty rules array.
   - If rules cannot round-trip without changing the rule classes, stop and
     report.
   - Delete the commented-out TODO block.
3. Write a roxygen block with `@export` and a runnable `@examples` that writes
   to and reads from `tempfile()`. Then run `roxygenise()`; NAMESPACE changes
   only through it.
4. Vignette: `vignettes/DTAtools.Rmd` about 371–380 describes JSON export. Add
   the reader.

**A new export:** follow `maintaining.md` → "You added, renamed or removed an
exported function", all four steps:
- add it to `requirements/_api.yaml`, keeping the list sorted;
- add a requirement covering it, in `REQ-SPEC.yaml` next to the YAML reader's
  requirement, or `REQ-EXPORT.yaml`; say which;
- write at least one test case for it;
- raise that area's `min_tests` in `_index.yaml`.

**Tests.**
- `tests/testthat/test-writeColumnsToJson.R`: its comment says there is no JSON
  reader. Update the comment and add a full round trip: write, read, compare
  every column property and the rules. Cover a column with every optional
  property unset, and one with all of them set.
- Qualification OQ-EXPORT-039 (`test-OQ-EXPORT-tables.R` about 369–401):
  - the "fails" check (about 393) and the `<list>` message step (about 397)
    flip to a successful round trip;
  - remove `qa_known_deviation("DEV-015", ...)`.
- OQ-SPEC-021 (`test-OQ-SPEC-yaml.R` about 253–266) only checks that the file
  parses; re-run it.
- Rewrite REQ-EXPORT-029 (about 356–363) and REQ-SPEC-015 (`REQ-SPEC.yaml`
  about 158–165).

**If the owner chose the D3 alternative (write-only)**, instead:
- document JSON as write-only in the `write_columns_to_json()` roxygen and the
  vignette;
- move DEV-015 to a `LIM-nnn` entry (no binding);
- pin it with a plain `qa_step`, as LIM-004 is pinned.

## Files owned

- `R/exportFunctions.R`, `R/DTADataSetTabular-class.R` (`write_table_to_file()`
  only), `R/DTAColumnSpecCollection-class.R`, `vignettes/DTAtools.Rmd` (the JSON
  section only).
- `tests/testthat/test-exportFunctions.R`, `test-writeColumnsToJson.R`, and a
  new `tests/testthat/test-columnsSpecsFromWord.R`.
- `inst/qualification/tests/test-OQ-EXPORT-documents.R`, `test-OQ-EXPORT-tables.R`,
  `test-OQ-FILE-writing.R`, `test-OQ-ROBUST-isolation.R`, `test-PQ-PERF-workflows.R`.
- `inst/qualification/requirements/REQ-EXPORT.yaml`, `REQ-FILE.yaml`,
  `REQ-SPEC.yaml`, `_api.yaml`, `_index.yaml`.
- Your entries in `deviations.yaml` (DEV-003, DEV-007, DEV-014, DEV-015), your
  CHANGELOG bullets, and the regenerated manifest.

**CHANGELOG.**
- `### Fixed`: one bullet per defect, saying why it mattered. DEV-007: the
  integrity check a caller wrote is the thing that broke. DEV-014: the documented
  Word exchange workflow did not complete.
- `### Added`: `import_specs_from_json()`.

## Verification

```bash
Rscript --no-init-file -e "devtools::test(filter = 'export|Json|Word|DTADataSetTabular')"
Rscript --no-init-file -e "devtools::test()"
Rscript --no-init-file -e "pkgload::load_all(quiet = TRUE); r <- DTAtools::run_qualification(tempfile('q'), stages = c('IQ','OQ','PQ'), scale = 'quick', formats = 'md', filter = 'EXPORT|FILE|SPEC|ROBUST|PERF|ENV', quiet = TRUE); print(summary(r))"
Rscript --no-init-file -e "devtools::test(filter = 'qualification')"
Rscript --no-init-file .github/scripts/check_deps_in_desc.R
Rscript .github/scripts/sync_qualification_manifest.R --check
```

The new export means IQ (`ENV`, IQ-ENV-014 compares exports against `_api.yaml`)
must run here too. Verify the example in the new roxygen block actually runs:
`R CMD check` executes it, the test suite does not.
