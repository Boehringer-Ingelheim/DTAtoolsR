# Findings — q3-qual-tests-b (qualification suite tests, part B)

Slice: `scratchpad/slices/q3-tests-b.txt` — 41 files under `inst/qualification/tests`
(OQ-MATRIX, OQ-META, OQ-REPORT, OQ-ROBUST, OQ-RULE, OQ-SPEC, OQ-STREAM, OQ-TMPL,
OQ-TYPE, OQ-VAL, PQ-PERF). All paths relative to `$SNAP`.

Read first, completely: `inst/qualification/docs/maintaining.md`,
`inst/qualification/tests/helper-00-qa.R`, `helper-generators.R`, `helper-parity.R`,
`helper-matrix.R`, `helper-parse.R`, `setup-qualification.R`.

Status: review complete. All 41 slice files are fully read; findings F-q3-qual-tests-b-01 … -74. The list of what was read, skimmed and not read is the "Coverage" section at the end; each area section also names the code it traced.


---

## Area MATRIX

### F-q3-qual-tests-b-01  [major] [test-weak]
- Where: inst/qualification/tests/test-OQ-MATRIX-findings.R:115-129 (requirement: inst/qualification/requirements/REQ-MATRIX.yaml:700-709; code: R/reportRendering.R:890-908, R/DTADataSetMatrix-class.R:491-496)
- What: OQ-MATRIX-054 is the only case for REQ-MATRIX-056, which names four kinds of row-less finding ("a header label, a row label's header text, a column count or the row count"); the body exercises one (a non-empty data label failing the pattern) — and the unexercised branch of the code produces exactly the wording the requirement forbids.
- Evidence:
  ```
  ds <- mx_ds(columns = list(pattern = "^S[0-9]$"))
  path <- mx_path("	S1	bad\nr1	1	2\n")
  ... qa_step(..., TRUE, grepl("bad", out$actual[[1]], fixed = TRUE))
  ... FALSE, grepl("missing|not present|absent", out$actual[[1]], ignore.case = TRUE)
  ```
  `.report_columnspec_actual_text()` returns `"(missing / not present)"` whenever `columnspec_data` is NA or `""` and the keyword is neither `additionalProperties` nor `rowLabelHeader`; an empty data label is filed as `propertyNames` with `data = ""` (DTADataSetMatrix-class.R:492-496), and an empty header field above a row label yields `"(an empty field, or no field)"`.
- Failure scenario: a header `"\tS1\t\tS3"` (empty data label) → `explain_messages()$actual` is "(missing / not present)", which the test's own regex would reject; nothing in the suite feeds that input to `explain_messages()`. Mutation not detected: drop `data = fmt(n)` from `dta_matrix_rowcount_finding()` (R/validationFunctions.R:782) or `as.character(n_data)` from the min/maxProperties findings — every row-count/column-count finding is then explained as "(missing / not present)" and OQ-MATRIX-054 still passes, REQ-MATRIX-056 still reports verified.
- Confidence: verified-by-reading (test, requirement, `.report_columnspec_actual_text`, the three `add()` calls; not traced: that `inspect()` hands an empty `data` through as `""` rather than NA — both land on the same branch)
- Suggested direction: one fixture per kind named in the requirement (wrong row-label header text, empty data label, min/maxProperties, min/maxItems), each asserting the offending text or the observed count in `actual`.

### F-q3-qual-tests-b-02  [major] [test-weak]
- Where: inst/qualification/tests/test-OQ-MATRIX-findings.R:62-113 (REQ-MATRIX-043; also REQ-MATRIX-054 "in the same retained order under max_errors", whose only case, test-OQ-MATRIX-parity.R:39-76, never passes `max_errors`)
- What: "the retained findings are the same, in the same order, on both holdings" is compared on the `keyword` column alone, and the three cell findings of the fixture all carry the keyword `minimum`, so neither which cell was retained nor the cell order can be told apart.
- Evidence:
  ```
  path <- mx_path("\tS1\tbad\nr1\t-1\t-2\nr2\t5\t-3\n")
  complete <- c("propertyNames", "minItems", "minimum", "minimum", "minimum")
  kept <- as.data.frame(messages(d))$keyword
  heads[[paste(stream, cap)]] <- kept
  qa_step(..., heads[[paste("never", cap)]], heads[[paste("always", cap)]])
  qa_step(..., complete, as.character(full$keyword))
  ```
  At `max_errors = 3` both holdings give `c("propertyNames","minItems","minimum")` whichever of the three cells was kept; the "cells in file order (row 1: S1, bad; row 2: bad)" comment is not something the comparison can see. With no cap the in-memory order is not checked at all (`sort(complete)` vs `sort(kept)`).
- Failure scenario: mutation — the streaming engine retains cell findings column-major (or keeps the last cell of a batch rather than the first) while the whole-column engine retains row-major: at cap 3 one holding keeps (row 1, S1), the other (row 1, bad) or (row 2, bad); `collect_full_errors()` returns the cells in a different order. Every step of OQ-MATRIX-041 and OQ-MATRIX-052 still passes.
- Confidence: verified-by-reading
- Suggested direction: compare `(row, column, keyword)` triples (e.g. `mx_found()` without its re-sort) for `kept` and for `collect_full_errors()`, and give the three cells distinguishable keywords or assert the triples.

### F-q3-qual-tests-b-03  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-MATRIX-cells.R:346-365 (REQ-MATRIX-040, REQ-MATRIX.yaml:505-513)
- What: REQ-MATRIX-040 states that the cell "values, pattern, length and format settings" are evaluated together with the range; its only case never declares `format` on a cell (it is an accepted `matrix.cells` key, R/DTAMatrixSpec-class.R:183,265).
- Evidence: the two fixtures are `cells = list(type = "SAS Int", nullable = FALSE, values = list(0, 1, 2), range = list(min = 0))` and `cells = list(type = "SAS Char", nullable = FALSE, pattern = "^[ACGT]+$", length = 4)`; `grep format` over the six MATRIX test files finds no cell `format`.
- Failure scenario: mutation — `dta_matrix_spec_from_list()` stops passing `format = cl$format` to the cell spec (the setting is silently ignored): no MATRIX case changes its result.
- Confidence: verified-by-reading
- Suggested direction: add a `format` cell to OQ-MATRIX-038, or strike "format" from the requirement text.

### F-q3-qual-tests-b-04  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-MATRIX-cells.R:170-175 (REQ-MATRIX-032)
- What: "1.7976931348623157e308 is a finite value in a SAS Num cell" is asserted as "nothing reported" on a cell spec that is `nullable = TRUE` with no range — the one shape under which a value read as missing is also silent; the file's own method (header lines 6-9: not nullable, range [v, v]) is not applied to this token.
- Evidence: `mc_expect("the largest double in a Num cell", raw, list(type = "SAS Num", nullable = TRUE), list(character(0)))`
- Failure scenario: mutation — the token is classified as missing (or parsed to any other finite number, e.g. after a lossy re-read): no `finite`, no `type`, no import finding, the step passes.
- Confidence: verified-by-reading
- Suggested direction: `nullable = FALSE` plus `range = list(min = 1.7976931348623157e308)` (written as a number), as `mc_value()` does for every other token.

### F-q3-qual-tests-b-05  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-MATRIX-yaml.R:267-270 (REQ-MATRIX-059, last sentence)
- What: "a current document / current matrix dataset is not refused" is asserted as `FALSE == inherits(error, "dta_matrix_format_outdated")`, which also holds when `upgrade_dta()` aborts with any other condition.
- Evidence:
  ```
  refused <- function(expr) inherits(mx_error(expr), "dta_matrix_format_outdated")
  qa_step("a current document is upgraded without a refusal", FALSE, refused(upgrade_dta(doc)))
  qa_step("and a current matrix dataset too", FALSE, refused(upgrade_dta(doc@datasets[["gene_counts"]])))
  ```
- Failure scenario: mutation — `upgrade_dta()` on a current matrix document aborts with an unclassed error (or class `dta_structure_invalid`): `refused()` is FALSE, both steps record "upgraded without a refusal".
- Confidence: verified-by-reading
- Suggested direction: `qa_step(..., NULL, mx_error(upgrade_dta(doc)))`, the form the rest of the file uses for "accepted".

### F-q3-qual-tests-b-06  [nit] [test-weak]
- Where: inst/qualification/tests/test-OQ-MATRIX-yaml.R:60
- What: half of the expectation compares a literal with the same literal.
- Evidence: `qa_step("with the keys they were written with", c(12, NA), c(ds@specs@columns[["ID"]]@structure@length, NA))` — the second element is `NA` on both sides by construction; only ID's length is checked, although the step is worded for "the keys" of both row labels (DESC was written `nullable = TRUE`, no length).
- Failure scenario: mutation — `dta_dataset_from_list()` drops or defaults the keys of every row label after the first (e.g. DESC built not nullable, or with a length): not detected here.
- Confidence: verified-by-reading
- Suggested direction: read DESC's `@structure@length` and `@nullable` into the observed side.

## Area META

(files fully read: requirements/REQ-META.yaml, tests/test-OQ-META-agreement.R, tests/test-OQ-META-metadata.R; code traced: R/DTAMetaData-helpers.R)

### F-q3-qual-tests-b-07  [major] [test-weak]
- Where: inst/qualification/tests/test-OQ-META-metadata.R:449-481 (REQ-META-018, REQ-META.yaml:220-237; code: R/DTAMetaData-helpers.R:136-159)
- What: OQ-META-018 is the only case for REQ-META-018, which names three issue kinds (trailing_residue on the top-level date, trailing_residue on a transmission date, not_convertible top-level date) and a conditional clause ("only while the field still holds the value that text resolved to; a value changed after loading is written as itself and carries no issue"); the body exercises one kind (`transmission$date_last_transfer`, trailing_residue) and never the clause.
- Evidence:
  ```
  original <- DTAMetaData(title = "t", transmission = list(date_last_transfer = "2026-12-31 at the earliest"))
  ...
  qa_step(..., "2026-12-31 at the earliest", as_list$transmission$date_last_transfer)
  ```
  `as.list(DTAMetaData)` has three separate branches (`"date" = x@date`, the two transmission paths), a separate `unchanged` rule for `not_convertible` (`length(current) == 0 || is.na(current)`) and a separate write (`result$date <- issue$raw`); only the last-transfer/trailing_residue path is ever run.
- Failure scenario: mutations not detected — (a) delete `"date" = x@date` from the `switch` (a qualified top-level date is written as the bare ISO date and the issue vanishes on reload: the DEV-010 defect, reopened for the top-level field); (b) change the `not_convertible` test so the raw phrase is never written (the NA date is dropped by `.date_to_iso()` and the document reloads clean); (c) replace `if (!unchanged) next` by nothing (a date edited after loading is overwritten by its stale original text on save — silent data loss of the edit).
- Confidence: verified-by-reading
- Suggested direction: three more round trips (top-level `date = "2026-05-01 pending signature"`, `date = "after approval"`, `transmission$date_first_transfer`) and one object whose date is reassigned after construction, asserting the new value is written and no issue comes back.

### F-q3-qual-tests-b-08  [minor] [test-cheat]
- Where: inst/qualification/tests/test-OQ-META-metadata.R:246-267 (REQ-META-010)
- What: three expected message strings are transcribed from the function body and labelled "the documented fixed message", but no requirement, roxygen block, README or vignette states them (REQ-META-010 says only "a message naming the problem"; `@return` says "messages (character vector)").
- Evidence:
  ```
  "date_first_transfer cannot be after date_last_transfer", result_bad$messages
  list(is_valid = TRUE, messages = "All transmission dates are valid"),
  list(is_valid = TRUE, messages = "No transmission data to validate"),
  ```
  The same three literals occur only at R/DTAMetaData-helpers.R:390, 408, 414 (grep over `$SNAP`, renv excluded).
- Failure scenario: breaks the suite's governing rule ("an expected value never comes from the software"): the wording cannot be traced to a requirement sentence, so a wrong or misleading message already in the code when the test was written is pinned as correct, and a harmless rewording fails the qualification.
- Confidence: verified-by-reading
- Suggested direction: either put the three messages into REQ-META-010's text, or assert `is_valid` and a `grepl()` on the field names the requirement mentions.

### F-q3-qual-tests-b-09  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-META-metadata.R:239-268 (REQ-META-010; code R/DTAMetaData-helpers.R:406-411)
- What: the boundary of "a first-transfer date no later than the last-transfer date … shall report is_valid = TRUE" is never tested: the cases are first > last by five months and first < last by five months, never first == last.
- Evidence: fixtures `2026-06-01`/`2026-01-01` and `2026-01-01`/`2026-06-01` only; code is `if (first > last)`.
- Failure scenario: mutation `first > last` → `first >= last`: a single-day transfer (first == last) is reported invalid; OQ-META-008 still passes. Also unasserted: the two extra failure branches the function has ("date_first_transfer cannot be empty character", lines 396-404), which no requirement describes.
- Confidence: verified-by-reading
- Suggested direction: add an equal-dates case expecting `is_valid = TRUE`.

### F-q3-qual-tests-b-10  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-META-agreement.R:1-14 (REQ-META-019), :16-39 (REQ-META-020), :41-57 (REQ-META-021)
- What: 44 rejections are asserted as "some error was raised" (`inherits(outcome, "error")`) with no class and no message, and two clauses of REQ-META-019 are not exercised at all: an invalid `protocol_id` (every invalid shape uses `id` or an unknown key) and "Study metadata shall not change … a dataset's STUDYID constraints" (no dataset appears in the test).
- Evidence:
  ```
  outcome <- tryCatch(DTAMetaData(study = invalid[[i]]), error = function(e) e)
  qa_check(paste("invalid study shape rejected", i), inherits(outcome, "error"))
  ```
  The validator loops `for (field in intersect(names(study), c("id", "protocol_id")))` (R/DTAMetaData-helpers.R:27-31).
- Failure scenario: mutation — restrict that loop to `"id"`: `study = list(id = "S", protocol_id = NA_character_)` is accepted and nothing fails. Wrong-reason passes: any abort (an S7 property-type error, an internal `[[` error on the unnamed list) satisfies every one of the 44 steps, so a validator rule that stopped firing is masked whenever something else throws first.
- Confidence: verified-by-reading
- Suggested direction: match the package-authored message fragment each rule emits (`"study$protocol_id must be"`, `"sections identifier"`, `"transmission$scope must be"`), add invalid `protocol_id` shapes, and build one dataset with a STUDYID column beside study metadata.

### F-q3-qual-tests-b-11  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-META-metadata.R:186-215 and :358-372 (REQ-META-007, REQ-META-015)
- What: REQ-META-007 requires the import issue to carry "the field path, the original string verbatim, the declared type and the reason"; the declared type is asserted nowhere (OQ-META-015 checks only that a column of that name exists).
- Evidence: `list(column = issues$column[[1]], raw = issues$raw[[1]], reason = issues$reason[[1]])` — no `declared_type`.
- Failure scenario: mutation — `.parse_metadata_date()` records `declared_type = NA` (or `"character"`): the message built from it (`"only '%s' fits the declared type '%s'"`) then reads "declared type 'NA'" in every report; no META case fails.
- Confidence: verified-by-reading (test and message builder; `.parse_metadata_date()` itself not opened)
- Suggested direction: add `declared_type = "Date"` to the expected list (after stating the value in REQ-META-007).

### F-q3-qual-tests-b-12  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-META-metadata.R:400-447 (REQ-META-017) and test-OQ-META-agreement.R:59-66 (REQ-META-022)
- What: the "full surface" round trip compares a subset: `transmission$type` and `date_last_transfer`, the second version_history record, every record's `version`/`changes`, and the section title are written but never compared; and "legacy constructor arguments retain their positional meanings" is asserted for the 11th argument (`template`) only.
- Evidence: `qa_step("transmission's own dates survive …", format(original@transmission$date_first_transfer, …), format(restored@transmission$date_first_transfer, …))` is the only transmission comparison; `qa_step("existing positional provenance retains its meaning", list(id = "legacy", version = "1.0"), legacy@template)` is the only positional one.
- Failure scenario: mutations not detected — `as.list()` drops `transmission$type` or truncates `version_history` to its first record; the constructor's positional order swaps `header` (4th) and `error_handling` (9th), both character.
- Confidence: verified-by-reading
- Suggested direction: compare `restored@transmission` and `restored@version_history` whole (with the documented Date→character difference stated), and read back `legacy@title`, `@version`, `@header`, `@error_handling`, `@authorized_for_corrections`.

## Area REPORT

(files fully read: requirements/REQ-REPORT.yaml, tests/test-OQ-REPORT-html.R; helper-parse.R `qa_html_report()`; code traced: R/reportRendering.R `.report_messages_table_html()` head)

### F-q3-qual-tests-b-13  [major] [test-weak]
- Where: inst/qualification/tests/test-OQ-REPORT-html.R:184-203 (OQ-REPORT-004), :345-366 (OQ-REPORT-010); helper inst/qualification/tests/helper-parse.R:51-66
- What: the area header says the report "has to say what check() actually found", but no case compares what a message row says with `messages()`: every assertion on the message table is a row count (or the `data-dataset` attribute). `qa_html_report()` builds `info$messages` (the cell text of every `tr.msg-row`) for exactly this purpose and no test reads it.
- Evidence:
  ```
  qa_step("the document carries exactly as many message rows as messages() does", nrow(messages(dta)), info$n_messages)
  qa_step("two of the seven are beyond the default cap of five, hidden rather than dropped", 2L, length(extra))
  qa_step("their messages are combined in one table: two plus three", 5L, info$n_messages)
  ```
  `grep -n 'info\$messages' test-OQ-REPORT-html.R` has no hit.
- Failure scenario: mutation in `.report_messages_table_html()` — every row is rendered from the first message of its group (same Row number seven times), the Row and Column cells are swapped, or the Message cell carries the neighbouring message's text: seven `tr.msg-row` elements still exist, all of OQ-REPORT-004/005/007/008/010 pass, and the signed report tells the reader the wrong rows failed.
- Confidence: verified-by-reading
- Suggested direction: for the 7-row fixture assert the Row cells are 1..7, the Column cell is `SEX`, the Source cell `columnspec` (hand-derived from the fixture), read from `info$messages`.

### F-q3-qual-tests-b-14  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-REPORT-html.R:415-442 (REQ-REPORT-011)
- What: "overwrite = TRUE replaces it" and "quiet = TRUE … the file is written all the same" are existence / no-error assertions, and the refused second write is not checked for leaving the first file's bytes alone.
- Evidence:
  ```
  replaced <- tryCatch(write_validation_report(dta, out, overwrite = TRUE, quiet = TRUE), error = function(e) e)
  qa_check("overwrite = TRUE replaces it without error", !inherits(replaced, "error"))
  qa_check("but the file is written all the same - quiet silences reporting, not the write", file.exists(out3))
  ```
  The same `dta` is written each time, so nothing distinguishes the replaced file from the original.
- Failure scenario: mutations — `overwrite = TRUE` returns early without writing (the stale report stays); `quiet = TRUE` creates a zero-byte file; the refusal truncates the existing report before aborting (REQ-ROBUST-004 lists `write_validation_report`, and OQ-ROBUST-004 only exercises `write_table_to_file`). None is detected.
- Confidence: verified-by-reading
- Suggested direction: overwrite with a DTA of a different title and read the title back; hash the file before/after the refused call; parse `out3` with `qa_html_report()`.

### F-q3-qual-tests-b-15  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-REPORT-html.R:551-560 (REQ-REPORT-015)
- What: three of the input-provenance checks are weaker than they read: the file size is matched as a bare two-digit substring of a section that also holds timestamps and a hash; the content-hash stamp's expected value is produced by calling the package's own internal `dta_table_hash_stamp()`; and "for every input, its dataset and table" is not asserted at all.
- Evidence:
  ```
  qa_check("with that file's size in bytes", grepl(as.character(file.size(streamed_path)), inputs, fixed = TRUE))
  stamp <- dta_table_hash_stamp(in_memory@tables[[1]])
  qa_check("the in-memory table carries a content-hash stamp", is.character(stamp) && nzchar(stamp))
  qa_check("and the report shows it", grepl(stamp, inputs, fixed = TRUE))
  ```
  The fixture file is ~33-36 bytes; `inputs` contains an ISO mtime and a hash string, either of which can contain those two digits. This is the only reference to the stamp function in the qualification tests.
- Failure scenario: mutations — the size is dropped from the inputs table (passes whenever "33"/"36" occurs in the mtime or the stamp); `dta_table_hash_stamp()` returns a constant or a value that ignores the table's content (both sides call the same function, so they agree); the dataset id / table name columns are emptied. None reliably detected.
- Confidence: verified-by-reading (test and helper; the stamp function body not opened)
- Suggested direction: use a fixture whose size is a distinctive 4+ digit number and match it with word boundaries; assert the stamp differs between two tables that differ in one cell; assert `mem`/`big` and their table names appear.

### F-q3-qual-tests-b-16  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-REPORT-html.R:283-300 (REQ-REPORT-007) and :106-133, :368-396 (REQ-REPORT-002)
- What: the repeat cap is only ever applied to a table holding one group of identical messages, so "identical repeated messages (same dataset/target/source/rule/column/keyword/message, differing only by row)" is not distinguished from "the first N rows of the table"; and of the three statuses REQ-REPORT-002 maps to pending (skipped, not_validated, unspecified) only a never-checked dataset is rendered.
- Evidence: `qrpt_dataset(dir, "rep", n_bad = 7L)` — seven rows failing the same SEX check — is the fixture of OQ-REPORT-004, -005, -007, -008.
- Failure scenario: mutation — build the grouping key from `dataset` only (or cap on the running row index): with a second, different message in the table it would be hidden under another message's "Show N more like this" toggle; with the single-group fixture nothing changes. A `skipped` target rendered as passed is likewise not exercised.
- Confidence: verified-by-reading
- Suggested direction: a fixture with two distinct message groups of different sizes (e.g. 4 x SEX enum, 2 x ID maxLength) and `max_repeats = 3`: expect 3 + 2 visible, 1 hidden, one toggle.

## Area ROBUST

(files fully read: requirements/REQ-ROBUST.yaml, tests/test-OQ-ROBUST-inputs.R, tests/test-OQ-ROBUST-isolation.R; code traced: R/DTADataSetTabular-class.R `write_table_to_file()` 485-670 and `check()` 1472-1563, R/exportFunctions.R `write_metadata()`, R/DTADataSet-class.R:616-624)

### F-q3-qual-tests-b-17  [blocker] [test-cheat]
- Where: inst/qualification/tests/test-OQ-ROBUST-inputs.R:194-208 (OQ-ROBUST-012, the only case for REQ-ROBUST-012, risk high / integrity)
- What: the assertion is a tautology — the accepted set is exactly the range of the expression that produces `outcome` — so the case passes whatever `load_file()`/`check()` do with invalid UTF-8, including the outcome the requirement forbids (silently reinterpreting the bytes and validating clean).
- Evidence:
  ```
  outcome <- tryCatch({
      ds <- check(ri_load(path), persist = FALSE, quiet = TRUE)
      status <- validation_status(ds)
      if (nrow(status) == 0) "no table" else "validated"
    }, error = function(e) "condition")
  qa_step("invalid bytes are either refused or reported, never silently accepted as valid text",
    TRUE, outcome %in% c("condition", "no table", "validated"))
  qa_check("the outcome is recorded for review", nzchar(outcome), detail = outcome)
  ```
  `outcome` can only be one of those three strings; `nzchar(outcome)` is likewise always TRUE. The comment "The outcome is pinned so a change is visible" is not true: nothing is pinned.
- Failure scenario: the reader maps the bytes `ff fe` to U+FFFD (or drops them), the ID becomes a different value, `check()` reports `ok = TRUE` and no message → `outcome == "validated"` → step passes; REQ-ROBUST-012 ("shall be reported rather than silently reinterpreted") is recorded as verified in the signed evidence.
- Confidence: verified-by-reading
- Suggested direction: decide the contract (condition class, or an import/encoding finding at row 1 column ID) and assert it: on the "validated" branch require `ok == FALSE` and a message naming row 1 / ID, and assert the stored ID is not a silently altered string.

### F-q3-qual-tests-b-18  [major] [test-weak]
- Where: inst/qualification/tests/test-OQ-ROBUST-inputs.R:110-129 (OQ-ROBUST-003, only case for REQ-ROBUST-003)
- What: the requirement is "shall raise a condition rather than validate as an empty or partial table"; the case also passes when the binary file IS read and validated as a table, as long as the verdict is not literally TRUE.
- Evidence:
  ```
  if (nrow(status) == 0) "no table" else as.character(status$ok)
  ...
  TRUE, outcome %in% c("condition", "no table", "FALSE", "NA")
  ```
  With `ri_specs()` declaring ID and AGE, six bytes of binary read as a header-only or one-column table can hardly produce `ok = TRUE`, so the accepted outcomes cover essentially every behaviour except a clean pass.
- Failure scenario: `load_file()` accepts `00 ff 00 fe 01 02` as a CSV, binds a table of one garbage column and zero rows, and `check()` reports it not ok (or `NA`, "unspecified") for the missing/undeclared columns — the delivery was "validated as an empty table", the case records a pass. Mutation: remove whatever guard makes the reader abort on unparseable content; not detected.
- Confidence: verified-by-reading (test and requirement; which of the four outcomes the software produces today was not determined — R may not be run)
- Suggested direction: assert the one outcome the requirement names (`"condition"`), with the condition's class; if another outcome is intended, the requirement text has to change first.

### F-q3-qual-tests-b-19  [major] [test-weak]
- Where: inst/qualification/tests/test-OQ-ROBUST-isolation.R:67-86 (OQ-ROBUST-005, only case for REQ-ROBUST-005; code R/DTADataSetTabular-class.R:589-638)
- What: "a write that cannot complete leaves nothing that looks complete" is tested with a target whose parent directory does not exist, i.e. a write that cannot START: no file can be left there by any implementation, so the two assertions are met by construction, and the mechanism that actually implements the requirement (write to `*.part`, rename when complete) is never exercised.
- Evidence:
  ```
  target <- file.path(dir, "no-such-directory", "export.tsv")
  err <- tryCatch(write_table_to_file(ds, table = table_name, filename = target, quiet = TRUE), error = function(e) e)
  qa_check("the write raises a condition", inherits(err, "condition"))
  qa_step("and no file is left behind", FALSE, file.exists(target))
  qa_step("nor is a checksum sidecar left claiming one exists", FALSE, file.exists(paste0(target, ".md5")))
  ```
- Failure scenario: mutation — delete the temp-file-and-rename block and let `write.table()` write straight to `filename` (the defect the code comment at lines 592-597 describes: a conversion failing partway leaves a header-only file that "validates perfectly clean"): OQ-ROBUST-005 passes unchanged. Side observation: the condition raised here is base R's connection error from `write.table()` (no parent-directory guard in `write_table_to_file()`), i.e. an untranslated-message, class-less error; `inherits(err, "condition")` hides that.
- Confidence: verified-by-reading
- Suggested direction: make the write fail after it has begun (e.g. a table with a list column, which `write.table()` rejects after emitting the header) into an existing directory, with and without a pre-existing target, and assert the target is absent / byte-identical and no `.part`/`.md5` remains.

### F-q3-qual-tests-b-20  [major] [test-weak]
- Where: inst/qualification/tests/test-OQ-ROBUST-isolation.R:150-166 (OQ-ROBUST-008, only case for REQ-ROBUST-008, category security)
- What: the case cannot fail: it runs `check(persist = FALSE)` — the one setting under which check() persists nothing — and then looks only at a freshly created temp directory the code was never told about; that directory is itself inside the temporary directory, where the requirement allows writes. `write_table_to_file`, the second symbol in `covers:`, is not called.
- Evidence:
  ```
  watched <- qa_tempdir()
  writeLines("sentinel", file.path(watched, "sentinel.txt"))
  before <- list.files(watched, recursive = TRUE)
  invisible(rs_checked(dir, "scoped.csv"))        # -> check(ds, persist = FALSE, quiet = TRUE)
  qa_step("an unrelated directory is untouched", before, list.files(watched, recursive = TRUE))
  ```
  `check()` defaults to `persist = TRUE`, which `dir.create()`s `tempdir()/DTAtools_validation/<id>` (R/DTADataSetTabular-class.R:1553-1563, R/DTADataSet-class.R:616-619).
- Failure scenario: mutation — the default artifact directory becomes `file.path(getwd(), "DTAtools_validation")`, or `~/.DTAtools`, or a sidecar is dropped beside the delivery: the working directory, the home directory and the delivery's own directory are not inspected, `persist` is off, the case passes.
- Confidence: verified-by-reading
- Suggested direction: run `check()` with its defaults and a `write_table_to_file()` into a named output directory, snapshot `getwd()`, the delivery directory and `tools::R_user_dir("DTAtools", "cache")`/`path.expand("~")` listing before and after, and assert the only new paths are under `tempdir()` or the named output directory.

### F-q3-qual-tests-b-21  [major] [test-weak]
- Where: inst/qualification/tests/test-OQ-ROBUST-inputs.R:269-298 (OQ-ROBUST-015, only case for REQ-ROBUST-015, `covers: [check, load_file, read_dta_from_yaml]`)
- What: none of the three functions the requirement covers is exercised — the case calls `validate_file_stream()`, `rule_check_range()` and `get_table()` — and for two of the three failures the only class asserted is `rlang_error`, which every `cli_abort()` carries and no caller can use to "tell one failure from another".
- Evidence:
  ```
  missing_file = tryCatch(validate_file_stream(ri_specs(), file.path(dir, "nope.csv"), verbose = FALSE), ...)
  bad_rule_column = tryCatch(rule_check_range(DTARuleColRange(...), data.frame(A = 1)), ...)
  unknown_table = tryCatch(get_table(ri_dataset(file.path(dir, "x.csv")), "no_such_table"), ...)
  ... vapply(conditions, function(e) inherits(e, "rlang_error"), logical(1), USE.NAMES = FALSE)
  ```
- Failure scenario: `read_dta_from_yaml()` on a missing or malformed file surfaces the base `file()`/`yaml` error (German text on the primary dev machine), or `load_file()` lets an Arrow `IOError` through: the traceability matrix still shows REQ-ROBUST-015 verified for those functions (M4/M5 are satisfied by the title alone). OQ-ROBUST-002 and -005 accept `inherits(x, "condition")`, so they do not catch it either.
- Confidence: verified-by-reading
- Suggested direction: provoke one user-facing failure from each covered function and assert a package class (not `rlang_error`) on each.

### F-q3-qual-tests-b-22  [major] [test-weak]
- Where: inst/qualification/tests/test-OQ-ROBUST-isolation.R:88-115 (OQ-ROBUST-006, only case for REQ-ROBUST-006; code R/exportFunctions.R:305-345)
- What: the "shall change when those bytes change" half asserts a property of MD5, not of the package, and the checksum actually recorded on disk (the `.md5` sidecar that travels with the delivery) is never read.
- Evidence:
  ```
  recorded <- written$md5sum
  ...
  altered[[2]] <- sub("A0000001", "B0000001", altered[[2]]); writeLines(altered, target)
  qa_step("and changing one value in the file changes it", FALSE, identical(recorded, unname(tools::md5sum(target))))
  ```
  After `recorded` is taken the package is not called again: the step compares a fixed string with `tools::md5sum()` of an edited file and can only fail if base R's MD5 collides.
- Failure scenario: mutations — `write_metadata()` writes the sidecar before the final rename, or writes a checksum of the previous export, or `writeLines(metadata_lines, …)` emits `md5sum: NA`; or a second export of changed data returns the first export's checksum (cached). The return value of the first call still equals `md5sum(target)`, the tautological step still passes.
- Confidence: verified-by-reading
- Suggested direction: read `paste0(target, ".md5")` and compare its `md5sum:` line with `tools::md5sum(target)`; export a second table differing in one value and assert the recorded checksum differs and again equals the file's.

### F-q3-qual-tests-b-23  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-ROBUST-isolation.R:117-148 (OQ-ROBUST-007; REQ-ROBUST-007 `covers: [check, validate_file_stream]`) and :168-186 (OQ-ROBUST-009)
- What: environment variables are checked for additions only (options are checked for additions and alterations), and the isolation cases only run the in-memory path (`load_file(stream = "never")` + `check(persist = FALSE)`); `validate_file_stream()` and the batch scanner — where thread counts, Arrow options and temp spill stores are handled — are never run under the before/after comparison.
- Evidence: `character(0), setdiff(names(Sys.getenv()), names(env_before))` is the only environment assertion; `rs_checked()` is the only workload.
- Failure scenario: mutations — the scanner calls `Sys.setenv(OMP_NUM_THREADS = "1")`/`Sys.setenv(TZ = "UTC")` on a variable that already exists, unsets one, or `validate_file_stream()` leaves `options(arrow.use_threads = FALSE)`/`arrow::set_cpu_count()` behind, or the streaming path draws from the RNG to name a spill file: none detected.
- Confidence: verified-by-reading (tests; the streaming code was not searched for such calls — the point is the uncovered surface, not an observed defect)
- Suggested direction: compare `Sys.getenv()` whole (values, names), and repeat 007/009 around `validate_file_stream()` and a `stream = "always"` check with default `persist`.

### F-q3-qual-tests-b-24  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-ROBUST-inputs.R:29-88 (OQ-ROBUST-001), :90-108 (OQ-ROBUST-002)
- What: (a) every refusal is "any condition" (`inherits(tryCatch(expr, error = function(e) e), "condition")`), which wrong-typed input satisfies by accident in almost any R function (an S7 dispatch failure, `$ operator is invalid for atomic vectors`); (b) the "writes nothing while refusing" step passes a correctly typed but unknown table name, discards the result, and never asserts that anything was refused; (c) `write_validation_report` (REQ-ROBUST-001 `covers:`) and `load_file` (REQ-ROBUST-002 `covers:`) are not called, and only `validate_file_stream()`'s message is checked for naming the path.
- Evidence:
  ```
  invisible(tryCatch(
    write_table_to_file(ri_dataset(path), table = "no_such_table", filename = file.path(dir, "out.tsv")),
    error = function(e) e))
  qa_step("and writes nothing while refusing", before, list.files(dir))
  ```
- Failure scenario: mutations — `write_table_to_file()` with an unknown table returns `invisible(NULL)` silently (step still passes: nothing written, nothing asserted); `read_dta_from_yaml()` on a missing file raises a message without the path; `load_file()` on a missing path returns the dataset unchanged.
- Confidence: verified-by-reading
- Suggested direction: assert a package class or message fragment per refusal, assert the write call errored, and add the missing covered functions.

### F-q3-qual-tests-b-25  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-ROBUST-inputs.R:300-365 (OQ-ROBUST-016, only case for REQ-ROBUST-016)
- What: the requirement lists five carriers of specification/delivery text ("a rule id, a rule or constraint message, a column name, a template file name or path") and four call sites including "summary() of a qualification result"; the case injects braces through a rule id and a template file name only, and never calls `summary()` on a qualification result.
- Evidence: the only payloads are `payload_id <- "range_rule_{1+1}"` and the file name `bad_{1+1}_version.dta-template.yaml`.
- Failure scenario: mutation — a `cli_abort()` in `validate_table()` that pastes a delivered column name (e.g. a CSV header `{Sys.getenv('USERNAME')}`) or a rule's `message:` into the format vector: evaluated on the validating machine, no case fails.
- Confidence: verified-by-reading (test and requirement; `summary.dta_qualification` coverage in the CORE area, another slice, not checked)
- Suggested direction: add a column named `{1+1}` that fails a column check, a rule whose message holds braces, and a `summary()` of a result with a brace-carrying failure message.

### F-q3-qual-tests-b-26  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-ROBUST-inputs.R:150-180 (OQ-ROBUST-011), :211-236 (OQ-ROBUST-013), :238-252 (OQ-ROBUST-014)
- What: three loose expectations: an inequality where the count is known (one 200,000-character ID against `length = 8` is exactly one finding); a "very wide delivery" checked only for `status == "validated"` although the comment says the 500 undeclared columns are "reported as such"; the truncated-gzip case run only with `stream = "never"` although the requirement's source is `R/streamingScale.R`; and a determinism comparison whose two sides are never shown to be non-empty.
- Evidence:
  ```
  TRUE, as.integer(big$n_columnspec_errors) >= 1
  qa_step("a very wide delivery is validated", "validated", wide_status$status)
  first <- qa_messages_norm(check(ri_load(path), ...)); second <- qa_messages_norm(check(ri_load(path), ...))
  qa_step("two validations report the same messages", first, second)
  ```
- Failure scenario: mutations — the long value is reported twice or the row is dropped and reported as a missing ID; the reader silently keeps only the first N columns of a wide file; the batch scanner validates the readable prefix of a truncated `.csv.gz`; `check()` reports no message at all for the fixture (two empty frames are identical). None detected.
- Confidence: verified-by-reading
- Suggested direction: expect exactly 1; assert the count of undeclared-column findings (500) or that `ncol()` of the bound table is 502; run 013 with `stream = "always"` too; pin `nrow(first)` to the two findings the fixture injects.

## Area RULE

(files fully read: requirements/REQ-RULE.yaml, tests/test-OQ-RULE-group.R, tests/test-OQ-RULE-operators.R, tests/test-OQ-RULE-types.R; code traced: R/evaluateRules.R 740-870, 1104-1300, 1364-1703, `dta_equality_bound()`; R/DTARuleGroupCondition-class.R 96-274; R/datePatternTokens.R 31-61; R/validationFunctions.R 46-98, 269-285)

### F-q3-qual-tests-b-27  [blocker] [test-cheat]
- Where: inst/qualification/tests/test-OQ-RULE-operators.R:168-182 (OQ-RULE-011, only case for REQ-RULE-007, risk high); code R/evaluateRules.R:851
- What: the step recorded as "an empty string and a missing value do not match a pattern" runs the pattern `^$` — the one pattern that matches nothing but an empty string — against a column that holds no empty string; the implementation has no empty-string rule at all, so with the fixture the step's own description calls for it would report 1, not 0.
- Evidence:
  ```
  qa_step(
    "an empty string and a missing value do not match a pattern",
    0L, ro_selected("pattern", "^$", c("x", NA))
  )
  ```
  ```
  pattern = grepl(dta_expand_date_pattern_tokens(value), as.character(x), perl = TRUE),
  ```
  `dta_expand_date_pattern_tokens("^$")` returns its input unchanged (no `<<`), and `grepl("^$", "", perl = TRUE)` is `TRUE` in R; only `NA` is non-matching by `grepl()` itself. The first step's `""` proves nothing either: `""` does not match `^[A-Z]{3}[0-9]{3}$` under any implementation.
- Failure scenario: REQ-RULE-007 says the pattern operator "shall treat a missing value and an empty string as not matching". A rule `IF CODE pattern ^[A-Z]*$ THEN …` (or `^$`, `.*`) selects rows whose CODE is `""`; the signed evidence says it does not. No deviation or limitation in `deviations.yaml` records this (grep for REQ-RULE-007 / "empty string": none). The suite reports the requirement verified while the software contradicts it for every pattern that can match the empty string.
- Confidence: verified-by-reading (test, requirement, the `pattern` branch, the token expander; R not run — the claim rests on `grepl()`'s documented behaviour for `""`)
- Suggested direction: put `""` in the fixture (`c("", NA)` with `^$` and with `.*`); then either make the operator treat `""` as non-matching or rewrite REQ-RULE-007 / register the behaviour.

### F-q3-qual-tests-b-28  [major] [test-cheat]
- Where: inst/qualification/tests/test-OQ-RULE-group.R:272-297 (OQ-RULE-049, tagged REQ-RULE-022)
- What: "the groups come back in C-collation order" cannot fail: the label extracted from each violation is the first `[AaBb]` character of the pasted detail, which is the `a` of the generated constraint id `constraint_1` for every group, so the comparison is `sort(c("a","a","a","a"))` against itself.
- Evidence:
  ```
  labels <- vapply(result$details,
    function(d) sub("^.*?([AaBb]).*$", "\\1", paste(unlist(d), collapse = " ")), character(1))
  qa_step("and the groups come back in C-collation order, upper case first",
    sort(labels, method = "radix"), labels)
  ```
  Each detail is `list(constraint_id = "constraint_1", group = "SUBJ=<x>", message = …, rows = …, rows_truncated = FALSE)` (R/evaluateRules.R:1637-1644; the id defaults to `paste0("constraint_", i)`, R/DTARuleGroupCondition-class.R:186). The pasted text therefore starts `constraint_1 SUBJ=…`; were the lazy quantifier ignored, the last match would be the `A` of `FALSE` — constant either way. The expected value is also derived from the observed one instead of being written down (`c("A","B","a","b")`).
- Failure scenario: mutation — replace `sort(unique(split_key), method = "radix")` (R/evaluateRules.R:1470) by a plain locale `sort()`: on the German dev machine the groups come back `a A b B`; every label is still `"a"`, the step passes. (On a C-locale runner the mutation is invisible to any test, so this case is the only place it could have been caught.)
- Confidence: verified-by-reading
- Suggested direction: `qa_step(…, c("SUBJ=A", "SUBJ=B", "SUBJ=a", "SUBJ=b"), vapply(result$details, function(d) d$group, character(1)))`.

### F-q3-qual-tests-b-29  [major] [test-weak]
- Where: inst/qualification/tests/test-OQ-RULE-group.R:241-270 (OQ-RULE-048, only case for REQ-RULE-028)
- What: "the violation names the group it belongs to" is `grepl("A", <whole detail pasted>)`, which is already satisfied by the `FALSE` of `rows_truncated`; "carries the rows that produced it" is `length(...) > 0`, although the rows are known to be 1 and 2.
- Evidence:
  ```
  qa_step("the violation names the group it belongs to",
    TRUE, grepl("A", paste(unlist(detail), collapse = " "), fixed = TRUE))
  qa_check("and carries the rows that produced it",
    length(detail$rows %||% detail[["rows"]] %||% NULL) > 0)
  ```
- Failure scenario: mutations — `group = NA_character_` / the label of the wrong group (subject B) in the detail and in the message; `rows` holding the rows of another group or only the left condition's row. `unlist(detail)` still contains "FALSE" (and "failed"/"reported"), `rows` is still non-empty: all three steps pass.
- Confidence: verified-by-reading
- Suggested direction: `qa_step(…, "SUBJ=A", detail$group)` and `qa_step(…, c(1L, 2L), as.integer(detail$rows))`.

### F-q3-qual-tests-b-30  [major] [test-weak]
- Where: inst/qualification/tests/test-OQ-RULE-operators.R:202-218 (OQ-RULE-013, only case for REQ-RULE-009, risk high); code R/evaluateRules.R:353-398 (`dta_equality_bound`)
- What: the requirement is about a bound "that parses as a number" being compared numerically so that double and integer storage agree; the case passes a bound that already IS a number against a double column, a comparison base R gets right with no package logic, and never uses an integer column.
- Evidence:
  ```
  frame <- data.frame(V = c(1e6, 999999), FLAG = c("no", "no"), stringsAsFactors = FALSE)
  result <- rule_check_col_condition(ro_rule("equals", 1000000), frame)
  ```
  The function's own documentation names the failing shape: "`1e6 == "1000000"` renders the left side via `as.character()` first and is `FALSE`, while `1000000L == "1000000"` is `TRUE`".
- Failure scenario: mutation — delete `value <- dta_equality_bound(x, value)` (R/evaluateRules.R:835-837) for numeric columns: `c(1e6, 999999) == 1000000` is still `TRUE, FALSE`, OQ-RULE-013 passes, and a YAML rule `equals: "1000000"` now selects the row on the engine that narrowed the column to integer and not on the one that kept it double — the divergence REQ-RULE-009 exists to prevent.
- Confidence: verified-by-reading
- Suggested direction: use the text bound `"1000000"` (and `"1e6"`) against `V = c(1e6, 999999)` and against `V = c(1000000L, 999999L)`, expecting 1 both times.

### F-q3-qual-tests-b-31  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-RULE-operators.R:123-131 (OQ-RULE-008)
- What: for two of the four operators the assertion "does not select the missing value" holds whether or not the missing value is selected.
- Evidence:
  ```
  values <- c(10, 20, NA)
  for (operator in c("greater", "less", "greater_equal", "less_equal")) {
    qa_step(sprintf("%s does not select the missing value", operator), TRUE, ro_selected(operator, -1e6, values) <= 2L)
  ```
  With the bound -1e6, `less`/`less_equal` select 0 rows, or 1 if the NA row were selected; both satisfy `<= 2L`.
- Failure scenario: mutation — `less = is.na(x) | x < value`: the two `less*` steps of this case still pass (the exact counts in OQ-RULE-006/007 would catch it, which is why this is minor; the evidence line for these two steps is nonetheless unfalsifiable).
- Confidence: verified-by-reading
- Suggested direction: state the exact count per operator (`c(greater = 2L, less = 0L, greater_equal = 2L, less_equal = 0L)`).

### F-q3-qual-tests-b-32  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-RULE-group.R:165-188 (OQ-RULE-045, REQ-RULE-025), :211-221 (OQ-RULE-046); test-OQ-RULE-types.R:107-119 (OQ-RULE-024)
- What: (a) the alias `implies` is only shown to construct an object of the right class, never to behave as `requires`; (b) two expectations are computed by the software itself (`not_both` vs `mutually_exclusive`, `range` vs `min`/`max`) although the value is known (FALSE; 2); (c) the unknown-condition rejection is "any condition".
- Evidence:
  ```
  qa_check("implies is accepted as a constraint type", inherits(implies, "DTAtools::DTARuleGroupCondition"))
  qa_step("not_both reaches the same verdict as mutually_exclusive",
    rule_check_group_condition(rg_exclusive(), frame)$valid, rule_check_group_condition(alias, frame)$valid)
  qa_step("a range and a min/max pair naming the same band agree", rt_violations(by_range), rt_violations(by_min_max))
  ```
- Failure scenario: mutation — the alias map in `dta_normalize_group_constraints()` sends `implies` to `"mutually_exclusive"` (or stores `type = "implies"` unnormalised): the rule builds, and at evaluation an unnormalised type falls into the `else` branch `constraint_viol[[ci]] <- logical(n_groups)` (R/evaluateRules.R:1597-1599) — a rule that silently checks nothing, the outcome REQ-RULE-026's note warns about; OQ-RULE-045 passes. Cross-slice note: that `else` branch means any rule object not built through the constructor (a restored session, `S7::prop(..., check = FALSE)`) with an unknown constraint type is "ignored when it is evaluated", contrary to REQ-RULE-026's wording; the roxygen even documents it ("Constraints of any other type are ignored").
- Confidence: verified-by-reading
- Suggested direction: evaluate the `implies` rule on a violating and a satisfying frame with literal expectations (FALSE / TRUE); write FALSE and 2L as the expected values.

### F-q3-qual-tests-b-33  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-RULE-operators.R:46-71 (OQ-RULE-001), :92-101 (OQ-RULE-005), :184-200 (OQ-RULE-012), :236-253 (OQ-RULE-050); test-OQ-RULE-types.R:133-144 (OQ-RULE-026)
- What: requirement clauses with no assertion behind them: REQ-RULE-001 "reject any other operator with a condition naming the supported set" (only `inherits(err, "condition")`); REQ-RULE-008 "a blank string as empty in character and factor columns" (no factor column); REQ-RULE-016 "raise a condition when given none or several" (only several); REQ-RULE-033 "in its condition or consequent" and `covers: DTARuleGroupCondition` (arity is only tried in the IF clause of a `DTARuleColCondition`). Also a step description that contradicts its own number: "the two non-members and the missing value are selected" with expected `2L` (one non-member, C, plus the missing value).
- Evidence: `qa_check("an unsupported operator raises a condition", inherits(err, "condition"))`; `rejects <- function(operator, value) { err <- tryCatch(ro_rule(operator, value), …)` where `ro_rule()` always puts the operator under `condition`.
- Failure scenario: mutations — the arity check is skipped for `then` clauses (`equals` with two values in a consequent recycles row-wise again, the defect REQ-RULE-033's note describes); the `is.factor(x)` branch of `empty` is removed; a range rule with `columns = character(0)` evaluates to "valid". None detected.
- Confidence: verified-by-reading
- Suggested direction: one assertion per clause; correct the OQ-RULE-005 description.

## Area SPEC

(files fully read: requirements/REQ-SPEC.yaml, tests/test-OQ-SPEC-accessors.R, -classes.R, -ids.R, -lint.R, -yaml.R; code traced: R/DTA-class.R 1509-1599, R/DTAColumnSpecCollection-class.R 352-485, R/lintDTA.R 640-742)

### F-q3-qual-tests-b-34  [major] [test-cheat]
- Where: inst/qualification/tests/test-OQ-SPEC-yaml.R:103-125 (OQ-SPEC-004, only case for REQ-SPEC-004, risk high); code R/DTA-class.R:1509-1545
- What: the "parsed form builds the same object" comparison has the same call on both sides, so it cannot fail; and three of the four list constructors the requirement names are not called.
- Evidence:
  ```
  from_file <- read_dta_from_yaml(path)
  from_list <- dta_from_list(yaml::read_yaml(path))
  qa_step("the same datasets are built either way", names(datasets(from_file)), names(datasets(from_list)))
  ```
  `read_dta_from_yaml()` is `dta_lint_enforce(lint_dta(file)); yaml_data <- yaml::read_yaml(file); dta_from_list_build(yaml_data)`, and `dta_from_list(x)` is `dta_lint_enforce(lint_dta(x)); dta_from_list_build(x)`: each of the three steps compares `dta_from_list_build(yaml::read_yaml(path))` with a second evaluation of the same expression, on names only. `dta_dataset_from_list()`, `dta_file_handlers_from_list()` and `specs_from_list()` are in `covers:`; `specs_from_list` is not called directly by any qualification test (grep over `inst/qualification/tests`).
- Failure scenario: mutation — `dta_dataset_from_list()` drops every column after the first, or every rule of type `col_condition`: both sides lose the same parts, all three steps pass. Nothing else in the SPEC area compares what was read with the file (see F-35).
- Confidence: verified-by-reading
- Suggested direction: compare the object with the file's own content read independently (`yaml::read_yaml(path)$datasets[[1]]$columns` ids, types, lengths; rule ids; handler file names), and exercise the other three list constructors against literal expectations.

### F-q3-qual-tests-b-35  [major] [test-weak]
- Where: inst/qualification/tests/test-OQ-SPEC-yaml.R:17-49 (OQ-SPEC-001, -002; REQ-SPEC-001/-002, risk high); test-OQ-SPEC-accessors.R:25-114 (OQ-SPEC-040, -041, -042; REQ-SPEC-018..020)
- What: what `read_dta_from_yaml()` and the accessors return for the shipped specification is asserted by existence, class or by comparing two views of the same slot — never against the content of the file.
- Evidence:
  ```
  qa_check("it declares at least one file handler", length(files(ds)) >= 1)
  qa_check("it declares column specifications", length(columns(ds)) > 0)
  qa_check("it declares rules", length(rules(specs(ds))) > 0)
  qa_step("the collection reports the same identifiers as the dataset", from_dataset, names(collection))
  qa_step("and its labels cover the same columns", length(from_dataset), length(labels(collection)))
  qa_step("and by position, giving the same table", dim(as.data.frame(by_name)), dim(as.data.frame(by_position)))
  ```
  `columns(ds)` is `x@specs@columns` and `names(collection)` reads the same list; `get_table(ds, name)` and `get_table(ds, 1)` are the same function on a dataset holding one table.
- Failure scenario: mutations — the reader keeps only the first column / first rule / first handler of a dataset; `labels()` returns the ids instead of the labels; `get_table()` returns the table with zero rows; `metadata(dta)` returns an empty `DTAMetaData`. Every step above still passes; REQ-SPEC-001 ("carrying its datasets, their column specifications, their rules, their file handlers and the transfer metadata") stays verified.
- Confidence: verified-by-reading
- Suggested direction: derive the expected ids/labels/types/row count from the bundled files with `yaml::read_yaml()` / `utils::read.csv()` and compare.

### F-q3-qual-tests-b-36  [major] [test-cheat]
- Where: inst/qualification/tests/test-OQ-SPEC-yaml.R:138-149 (OQ-SPEC-005, only case for REQ-SPEC-005, risk high); code R/lintDTA.R:690-698, R/DTA-class.R:1548-1558
- What: the step for "a file that parses but describes nothing the package can use" accepts both possible outcomes, so it cannot fail — and the outcome the software actually produces is the one the requirement forbids (an object, no condition).
- Evidence:
  ```
  writeLines("something_else: true", empty)
  result <- tryCatch(read_dta_from_yaml(empty), error = function(e) e)
  qa_check("a specification naming no datasets does not yield a usable transfer object",
    inherits(result, "condition") || length(datasets(result)) == 0)
  ```
  An unknown top-level key is a lint finding of severity `warning` (`document_field_unknown`), which `dta_lint_enforce()` does not raise; `dta_from_list_build()` then only prints `cli_alert_warning("No 'datasets' section found in list")` and returns `DTA(datasets = list(), metadata = DTAMetaData())`. A file without `datasets:` can never produce a DTA with datasets, so the disjunction is always TRUE.
- Failure scenario: a specification whose top-level key is misspelt (`dataset:` for `datasets:`) is "read" as an empty transfer with no condition; REQ-SPEC-005 ("shall raise a condition naming what is wrong with it, rather than yielding a partial object") is recorded verified. The other two steps of the case assert only "some condition", not that it names what is wrong.
- Confidence: verified-by-reading (lint and build paths read; R not run)
- Suggested direction: decide whether an unrecognised document is an error; assert exactly one outcome (class `dta_lint_error` with code `document_field_unknown`, or the documented empty-document contract written into the requirement).

### F-q3-qual-tests-b-37  [major] [test-cheat]
- Where: inst/qualification/tests/test-OQ-SPEC-yaml.R:158-187 (OQ-SPEC-006, only case for REQ-SPEC-006, risk high / integrity); code R/DTAColumnSpecCollection-class.R:352-373, 454-485
- What: the comment states the hazard ("YAML would read a bare 007 as the number seven") and the fixture then writes the values quoted, which every YAML parser keeps as text; the bare form — the only one that can lose its text — is never read, and neither is the warning the package issues for it.
- Evidence:
  ```
  "    values:",
  '      - "007"',
  '      - "010"'
  ...
  qa_step("the permitted values keep the text they were written with", c("007", "010"), as.character(colspec(collection, "CODE")@values))
  ```
  `import_specs_from_yaml()` parses with plain `yaml::read_yaml(file)` (no scalar handlers); for unquoted values on a text column the only reaction is `dta_warn_numeric_values_on_text_column()` — a `cli_warn()`, after which the collection is built with the numbers. No qualification test matches that warning (grep "read as numbers": none).
- Failure scenario: `values: [007, 010]` unquoted on a `SAS Char` column builds a collection that permits "7" and "8" and rejects "007" (YAML 1.1 reads both as numbers, `010` as octal) — the exact outcome the test's comment describes — while OQ-SPEC-006 passes. Mutation: delete the `lapply(dta_columns, dta_warn_numeric_values_on_text_column)` call; nothing in the suite fails, and the conversion becomes fully silent.
- Confidence: verified-by-reading (reader and warning code; the YAML-1.1 reading of `007`/`010` by the `yaml` package is from knowledge, not from a run)
- Suggested direction: add the unquoted fixture and assert what the requirement is meant to guarantee for it (a refusal, or at least the classed warning plus the stored values), and state that contract in REQ-SPEC-006.

### F-q3-qual-tests-b-38  [major] [test-weak]
- Where: inst/qualification/tests/test-OQ-SPEC-yaml.R:189-242 (OQ-SPEC-020, only case for REQ-SPEC-014, risk high / integrity)
- What: REQ-SPEC-014 requires the round trip to keep "the same types, lengths, nullability, permitted values and patterns"; the comparison covers nullability, values and pattern only — type and length are written into the fixture (`SAS Char` 8/1/3, `SAS Num`) and never compared.
- Evidence:
  ```
  list(nullable = before@nullable,
       values = as.character(before@values %||% character(0)),
       pattern = as.character(before@pattern %||% NA_character_))
  ```
- Failure scenario: mutation — `write_columns_to_yaml()` omits `length:` (or writes every type as `SAS Char`): the restored AGE is a text column, SUBJID has no length limit, every step passes.
- Confidence: verified-by-reading
- Suggested direction: add `type = before@structure@type`, `backend`, `length = before@structure@length` (and `format`) to both lists.

### F-q3-qual-tests-b-39  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-SPEC-classes.R:13-30 (OQ-SPEC-030), :242-257 (OQ-SPEC-036); test-OQ-SPEC-yaml.R:244-266 (OQ-SPEC-021)
- What: (a) "every declared property is carried" compares id, label, nullable and pattern; the declared type, length and description are not read back (REQ-SPEC-007 lists all of them). (b) REQ-SPEC-013 "each factory shall reject a type it does not recognise, naming what it accepts" is asserted as "any condition", for `DTADataSetFactory`, `DTAFileFactory` and `DTARuleFactory` — `DTAColumnSpecStructureFactory`, the third symbol in `covers:`, is not given an unknown type. (c) REQ-SPEC-015's `import_specs_from_json()` half is not exercised under this requirement, and "names the same columns" is a substring search over all JSON values.
- Evidence: `list(id = spec@id, label = spec@label, nullable = spec@nullable, pattern = as.character(spec@pattern))`; `inherits(tryCatch(DTADataSetFactory(id = "x", type = "nonsense"), error = function(e) e), "condition")`; `any(grepl(id, paste(unlist(parsed), collapse = " "), fixed = TRUE))`.
- Failure scenario: mutations — `DTAColumnSpec()` drops `description`; `DTAColumnSpecStructureFactory(type = "SAS Nonsense")` silently builds a Char structure; a factory's error stops listing the accepted types. None detected by these cases.
- Confidence: verified-by-reading
- Suggested direction: read every declared property back; assert a message fragment listing the accepted types for each of the three covered factories.

### F-q3-qual-tests-b-40  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-SPEC-accessors.R:145-191 (OQ-SPEC-044, REQ-SPEC-022)
- What: "describing an object does not change it" compares three name vectors of `dta`/`ds` before and after `print()`; R passes the objects by value, so no print method could change the caller's copies — the comparison holds by the language, not by the package. The dataset is also unbound, so the only reference-semantics state a print method could disturb (Arrow tables, a lazily held scan) is absent.
- Evidence: `before <- list(datasets = names(datasets(dta)), columns = names(columns(ds)), rules = …)` … `describe()` … the same list recomputed from the same local variables.
- Failure scenario: mutation — `print(DTADataSetTabular)` collects (materialises) a lazily held table, or consumes a `RecordBatchReader`, so a later `check()` sees no rows: not detectable here.
- Confidence: verified-by-reading
- Suggested direction: print a dataset holding a `stream = "always"` table and a batch reader, then show `check()` still reports the injected findings.

### F-q3-qual-tests-b-41  [nit] [inconsistency]
- Where: inst/qualification/tests/test-OQ-SPEC-accessors.R:11-13, test-OQ-SPEC-yaml.R:11-13, test-OQ-SPEC-lint.R:296, :32 and :446, test-OQ-SPEC-ids.R:46-47
- What: three house-rule / hygiene slips: bare `testthat::skip()` calls where maintaining.md says "Skip with a reason, through the helpers only" (a helper for exactly this, `qa_example_path()`, exists in helper-parse.R); the name `sl_codes` is bound to a character vector at line 32 and re-bound to a function at line 446 of the same file (correct only because `test_that()` blocks run in file order); and a comment lists `" #"` among "ids YAML would misread unquoted" although that id is not in the fixture and would be refused for its leading space.
- Evidence: `if (!nzchar(path)) { testthat::skip("the bundled example specification is not installed") }`; `sl_codes <- c("rule_id_duplicate", …)` / `sl_codes <- function(body, severity = "error") {`.
- Failure scenario: a missing bundled file turns five SPEC cases into skips rather than failures (IQ-ENV-016 would normally catch the missing file first); moving OQ-SPEC-058 above OQ-SPEC-049 silently breaks `sl_codes`.
- Confidence: verified-by-reading
- Suggested direction: use `qa_example_path()`; rename the second `sl_codes`.

## Area STREAM

(files fully read: requirements/REQ-STREAM.yaml, tests/test-OQ-STREAM-arguments.R, -filecount.R, -parity.R, helper-parity.R; code traced: R/arrowCompute.R (whole file), R/streamingValidation.R 2530-2625 `dta_structure_findings()`, R/evaluateRules.R:593-597)

### F-q3-qual-tests-b-42  [major] [test-weak]
- Where: inst/qualification/tests/test-OQ-STREAM-arguments.R:559-582 (OQ-STREAM-019, only case for REQ-STREAM-017, risk high); code R/arrowCompute.R:67-70, 106-131
- What: the Arrow-vs-R parity case cannot tell "the Arrow engine agrees with R" from "the Arrow engine never ran": the Arrow branch of `dta_count_duplicates()` is wrapped in `tryCatch(…, error = function(e) NULL)` and falls back to the R implementation silently, and the test asserts nothing that depends on which branch executed.
- Evidence:
  ```
  old <- options(DTAtools.arrow_min_rows = 1L, DTAtools.use_arrow_compute = TRUE)
  arrow_checked <- check(ds, quiet = TRUE, persist = FALSE)
  options(DTAtools.use_arrow_compute = FALSE)
  r_checked <- check(ds, quiet = TRUE, persist = FALSE)
  qa_step("the Arrow-accelerated duplicate count and the plain R one agree exactly", qa_messages_norm(r_checked), qa_messages_norm(arrow_checked))
  ```
  ```
  arrow_count <- tryCatch({ … dplyr::collect(dplyr::summarise(dplyr::group_by(tbl, dplyr::across(dplyr::all_of(cols))), .groups = "drop")) … },
    error = function(e) NULL)
  if (!is.null(arrow_count)) return(arrow_count)
  ```
- Failure scenario: mutations — `dta_use_arrow_compute()` reads a misspelt option name; `dta_arrow_compute_available()` is FALSE on the target build; the Arrow pipeline throws on this arrow version: in all three both `check()` calls run the R code, the two message frames are identical by construction, and the requirement whose notes call this "the one place the promise is actually exercised end to end" reports verified. A wrong Arrow count is only detectable if the branch runs.
- Confidence: verified-by-reading (whether the Arrow branch succeeds on the installed arrow was not determined — R may not be run; the point is that the test cannot know either)
- Suggested direction: prove the branch ran (e.g. `testthat::local_mocked_bindings` is not available to an installed-package suite, so trace through an observable: compare against a build of the frame for which the two engines must differ in an internal counter, or expose/record which engine counted), and assert `dta_use_arrow_compute()` is TRUE before comparing.

### F-q3-qual-tests-b-43  [minor] [inconsistency]
- Where: inst/qualification/requirements/REQ-STREAM.yaml:284-291 (REQ-STREAM-021, first sentence); inst/qualification/tests/test-OQ-STREAM-parity.R:182-212 (OQ-STREAM-027, its only case), :27-47 (OQ-STREAM-021), :119-146 (OQ-STREAM-025); code R/streamingValidation.R:2581-2618
- What: "A column no specification declares and no rule reads shall not change the verdict on either engine" is exercised by no case (OQ-STREAM-027's file holds only ID and the rule-read XKEY) and contradicts the implemented and separately qualified behaviour: such a column is `unexpected` and is reported as an `additionalProperties` column-specification error (REQ-SPEC-034 / OQ-SPEC-060 count it), so its presence turns a valid table invalid. A side effect: every generated campaign file carries the unread, undeclared XPAD, so every case has a column-specification finding before any cell is looked at, and the coverage guards "the columnspec axis appeared at least once" are met by that structural finding alone.
- Evidence: `unexpected <- … if (enumerable) setdiff(column_names, described) else character(0)` with `described` = declared ids ∪ rule columns; `columns$XPAD <- if (n == 0) character(0) else sprintf("p%03d", seq_len(n))` (helper-parity.R:168); `seen[["columnspec"]] <- seen[["columnspec"]] || any(sources == "columnspec")`.
- Failure scenario: a reader of the traceability matrix takes REQ-STREAM-021 as verified evidence that extra columns are harmless; and a campaign in which no per-cell column check ever fires (mutation: the generator's injected SEX/ID defects removed) still passes OQ-STREAM-021/-025, the guards written to prevent exactly that.
- Confidence: verified-by-reading (`dta_structure_findings()`; that the unexpected-column rows carry `source == "columnspec"` in `validation_errors()` is inferred from OQ-SPEC-060's expected counts, not traced)
- Suggested direction: reword the sentence to what is meant (both engines report the same finding for it), and make the guards require a per-cell keyword (`enum`, `maxLength`, `type`) rather than any columnspec row.

### F-q3-qual-tests-b-44  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-STREAM-parity.R:386-407 (OQ-STREAM-031, only case for REQ-STREAM-024)
- What: the inspect() comparison is restricted to the context/failing columns that exist on BOTH sides, so a streamed inspect() that reports fewer columns than the in-memory one still "matches"; and the requirement's last clause ("when the held table cannot be safely re-read, inspect() shall say so rather than report an empty or a clean result") has no case.
- Evidence:
  ```
  compare_cols <- intersect(
    grep("^(context_|failing_)", names(eager_info), value = TRUE),
    grep("^(context_|failing_)", names(lazy_info), value = TRUE))
  qa_check(paste("row detail is reported on both engines for", k), length(compare_cols) > 0)
  ```
- Failure scenario: mutation — the lazy branch of `dta_inspect_table_frame()` returns only `context_.row` (or only the offending column): one common column remains, its values agree, both steps pass. Deleting the file between `check()` and `inspect()` is never tried.
- Confidence: verified-by-reading
- Suggested direction: compare `sort(names(...))` of the two detail frames first, then all of them; add a case that removes or rewrites the file before `inspect()`.

### F-q3-qual-tests-b-45  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-STREAM-parity.R:49-65 (OQ-STREAM-022), :67-99 and :157-178 (OQ-STREAM-023, -026); test-OQ-STREAM-arguments.R:81-139 (OQ-STREAM-002, -003), :213-235 (OQ-STREAM-006), :383-408 (OQ-STREAM-012)
- What: a group of assertions that are weaker than their wording. (a) "the campaign varies its shape": four bounds that are invariants of the generator itself (3..6 columns, 1..3 rules) and hold if every case is identical. (b) the zero-/one-row boundary cases draw their rule set with an unseeded `sample()` (`qp_rules()`), so which rules are tested at the boundary depends on the RNG state left by whatever ran before (it differs between scale tiers) and is not recorded. (c) `quote = ""` / the wrong quote character "raises a condition" accepts any error, including Arrow's own column-count error. (d) REQ-STREAM-007 says the reassembled frame carries "neither attribute"; only `truncated` is checked, and the reassembled rows are counted, not read. (e) "verbose = FALSE prints nothing" captures the message stream only.
- Evidence: `qa_check("the number of declared columns never drops below the generator's minimum of three", min(n_columns) >= 3L)`; `specs = qp_collection(qp_column_pool()[c("ID", "AGE")], qp_rules(c("ID", "AGE")))`; `inherits(err, "condition")`; `is.null(attr(full, "truncated", exact = TRUE))`; `capture.output(…, type = "message")`.
- Failure scenario: mutations — the generator always draws the same shape; `collect_full_errors()` returns the retained head three times, or keeps `spilled_rows`; `verbose = FALSE` prints the summary with `cat()`. None detected.
- Confidence: verified-by-reading
- Suggested direction: assert `length(unique(n_columns)) > 1`; fix the boundary rule set explicitly; assert rows 1:3 and both attributes; capture stdout as well.

### F-q3-qual-tests-b-46  [minor] [design]
- Where: inst/qualification/tests/test-OQ-STREAM-arguments.R:432-434 (OQ-STREAM-013), :542-556 (OQ-STREAM-018), :568-572 (OQ-STREAM-019)
- What: three cases change session-global state (an option, Arrow's CPU count) and restore it with a plain statement after the call under test, with no `on.exit()`/`withr::defer()`; if the call in between errors, the state stays changed for every later case of the stage.
- Evidence:
  ```
  old <- options(DTAtools.benchmark = TRUE)
  via_option <- validate_file_stream(specs, path, verbose = FALSE)
  options(old)
  ```
  ```
  previous <- set_dta_compute_threads(target) … during <- validate_file_stream(…) … set_dta_compute_threads(previous)
  ```
- Failure scenario: `check()` aborts inside OQ-STREAM-019: `DTAtools.use_arrow_compute = TRUE` and `DTAtools.arrow_min_rows = 1L` remain set, so every later uniqueness check in the run (the parity campaign, VAL, PQ-PERF) silently runs on the opt-in Arrow engine and the run's evidence describes a configuration nobody chose; a failure in OQ-STREAM-018 leaves the PQ-PERF measurements on 3 or 4 threads.
- Confidence: verified-by-reading
- Suggested direction: `withr::local_options()` / `withr::defer(set_dta_compute_threads(previous))` (withr is already used by `qa_tempdir()`).

## Area TMPL

(files fully read: requirements/REQ-TMPL.yaml, tests/test-OQ-TMPL-agreement.R, -guide.R, -inherit.R, -library.R, -lint.R, -matrix.R, -paths.R, -repo.R, -validate.R; helper-app.R `qapp_*` loader read; fixtures opened: fx/t/ca/*, fx/t/c/{vocab_slot_invalid,values_from_invalid,vocabulary_extends_failed,sealed_violation,sealed_violation_option}, fx/t/inh/{four-states,vocab-incl-excl}. The other ~50 fixture folders under fx/t were NOT opened: where a case asserts "exactly one row" for a fixture I did not open, the fixture's content is taken on trust.)

Overall: test-OQ-TMPL-paths.R, -matrix.R, -library.R, -repo.R and most of -validate.R are the strongest files of the slice — literal expectations, negative controls, fixtures damaged on purpose to prove a folder is read. The findings below are the exceptions.

### F-q3-qual-tests-b-47  [major] [test-weak]
- Where: inst/qualification/tests/test-OQ-TMPL-inherit.R:238-246 (OQ-TMPL-046, OQ-TMPL-047; REQ-TMPL-018, risk high); fixture inst/qualification/fx/t/inh/vocab-incl-excl/
- What: "include and exclude each select a clean subset of a vocabulary's terms" asserts only that validate_template() reports no row; which terms the binding expands to is never read — here or anywhere else in the suite (the only call of `expand_column_vocabularies` in `inst/qualification/tests` is OQ-TMPL-118, a whole vocabulary on a matrix row label with neither include nor exclude).
- Evidence:
  ```
  result <- validate_template(tmpl_fixture("inh", "vocab-incl-excl"))
  qa_step("zero issues across both the include-based and the exclude-based binding", 0L, nrow(result))
  ```
  The fixture's two bindings are `include: [A, B]` and `exclude: [C]`.
- Failure scenario: mutations — the expansion ignores `include`/`exclude` and permits every term of the vocabulary; `include` is applied as `exclude`; `exclude` removes nothing. The built column then permits values the template author excluded, deliveries carrying them validate clean, and both cases still report "zero issues". REQ-TMPL-018's "expanding into the column's permitted values" stays verified.
- Confidence: verified-by-reading
- Suggested direction: expand the two fixture bindings through the engine (as OQ-TMPL-118 does) and assert `c("A","B")` for both, in the vocabulary's order.

### F-q3-qual-tests-b-48  [major] [test-weak]
- Where: inst/qualification/tests/test-OQ-TMPL-inherit.R:153-189 (OQ-TMPL-039, -040, -041; REQ-TMPL-022, risk high); fixture inst/qualification/fx/t/inh/four-states/
- What: REQ-TMPL-022 claims the four value states for "every mergeable section … (base, options, datasets, party_slots, vocabulary_slots)". The cases exercise `datasets` (where absent / wiped / new value are told apart) and `base`, whose two assertions cannot distinguish any state from any other; `options`, `party_slots` and `vocabulary_slots` are never exercised.
- Evidence:
  ```
  base_drop_rows <- tmpl_rows_for(result, "child_base_drop.dta-template.yaml")   # base: null
  base_empty_rows <- tmpl_rows_for(result, "child_base_empty.dta-template.yaml") # base: {}
  qa_step("base: null and base: {} both leave zero issues too", c(0L, 0L), c(nrow(base_drop_rows), nrow(base_empty_rows)))
  ```
  A child that inherits the parent's `base` unchanged (the "absent" state) also has zero issues (`child_absent`), so "0 rows" is the outcome of all three states.
- Failure scenario: mutation — `dta_template_merge_collection_section()` treats an explicit `base: null` (or `options: null`, `party_slots: []`) as "key not written" and keeps the parent's content: a child that meant to wipe inherited metadata, options or party slots silently keeps them; every step of OQ-TMPL-039..041 passes.
- Confidence: verified-by-reading (tests and the six fixture files; the merge code itself not opened)
- Suggested direction: observe the merged definition (the engine's resolver is already reachable through `.dta_template_engine`) and assert, per section, inherited / dropped / emptied / merged content.

### F-q3-qual-tests-b-49  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-TMPL-guide.R:81-91 (OQ-TMPL-088, REQ-TMPL-040)
- What: two of the five header/footer checks are satisfied by text that is always there: the template version is "1.0" and every header/footer XML part begins `<?xml version="1.0" …?>`; and `PAGE` is a substring of `NUMPAGES`.
- Evidence:
  ```
  all <- paste(guide_hf_xml(res$paths[["biomarker_gf"]]), collapse = " ")   # raw XML of word/header*.xml, footer*.xml
  qa_check("a PAGE field is present", grepl("PAGE", all, fixed = TRUE))
  qa_check("a NUMPAGES field is present", grepl("NUMPAGES", all, fixed = TRUE))
  qa_check("the template version is printed", grepl(as.character(parent[["version"]]), all, fixed = TRUE))
  ```
  `inst/extdata/library/templates/biomarker_gf.dta-template.yaml:4` is `version: "1.0"`.
- Failure scenario: mutations — the guide stops printing the template version in the running header; the footer keeps "of Y" but loses the current-page field. Both checks still pass.
- Confidence: verified-by-reading (that officer writes the XML declaration with `version="1.0"` into header/footer parts is from knowledge of the format, not from a run)
- Suggested direction: match the instruction text (`instrText` holding exactly ` PAGE ` / ` NUMPAGES `), and use a template whose version cannot occur by accident (e.g. write a copy with version "7.31").

### F-q3-qual-tests-b-50  [minor] [test-cheat]
- Where: inst/qualification/tests/test-OQ-TMPL-matrix.R:97-99 (OQ-TMPL-116, REQ-TMPL-048)
- What: "and leaves the other row label as it was" reads a freshly built fixture, not the result of the call under test, so it compares a constant with itself.
- Evidence:
  ```
  by_path <- set_path(tmx_body(), "dataset.row_labels.GENE_NAME.nullable", FALSE)
  qa_step("a path dataset.row_labels.<id>.<field> sets that field of the row label it names", FALSE, by_path$row_labels[[2]]$nullable)
  qa_step("and leaves the other row label as it was", FALSE, tmx_body()$row_labels[[1]]$nullable)
  ```
  `tmx_body()` builds GENE_ID with `nullable = FALSE` every time; `by_path$row_labels[[1]]` is never inspected. (Even the intended expression could not fail: the value written, FALSE, equals what GENE_ID already holds.)
- Failure scenario: mutation — `apply_dataset_template_path()` writes the field into every row label of the dataset: not detected by this step (OQ-TMPL-124's GENE / GENE.ID pair would catch it).
- Confidence: verified-by-reading
- Suggested direction: write a field GENE_ID does not have (`dataset.row_labels.GENE_NAME.length`, 20L) and assert `is.null(by_path$row_labels[[1]]$length)` on the result.

### F-q3-qual-tests-b-51  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-TMPL-validate.R:245-257 (OQ-TMPL-013, REQ-TMPL-010), :297-318 (OQ-TMPL-017, REQ-TMPL-011); fixture inst/qualification/fx/t/ca/kinds_filter/
- What: two requirements about cross-file resolution are tested on a fixture that contains no cross-file reference: `kinds` "shall never restrict which files are available to resolve a cross-file reference", and validate_template() "shall not consult" the seven DTATOOLS_TEMPLATE_* variables.
- Evidence: the fixture is one abstract creation template with `options: [{id: bad, type: text, target: metadata.no_such_field}]` and one vocabulary with no terms — no `extends:`, no `datasets[].template`, no party slot, no `values_from`. OQ-TMPL-017 compares `baseline[, c("kind","severity","code")]` with the same call under the env vars.
- Failure scenario: mutations — `kinds = "dta_creation_template"` also drops vocabularies / party profiles / dataset templates from the resolution set (every template using them would report `*_unresolved`); the extends resolver falls back to the app's index built from `DTATOOLS_TEMPLATE_SOURCES` / the built-in library when a reference is not found locally. Neither changes anything for a fixture without references.
- Confidence: verified-by-reading
- Suggested direction: use a library whose creation template extends a parent, imports a dataset template and names a party profile and a vocabulary; run it with `kinds = "dta_creation_template"` (expect zero rows) and, for REQ-TMPL-011, with a reference that only a built-in or env-configured source could satisfy (expect `extends_unresolved` either way).

### F-q3-qual-tests-b-52  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-TMPL-validate.R:136-152 (REQ-TMPL-004), :370-373 (REQ-TMPL-014), :768-778 (REQ-TMPL-036), :782-889 (REQ-TMPL-037); test-OQ-TMPL-inherit.R:115-149 (REQ-TMPL-021); test-OQ-TMPL-agreement.R:1-73 (REQ-TMPL-032); test-OQ-TMPL-lint.R:177-186 (REQ-TMPL-047)
- What: enumerated requirement clauses with no case behind them (each requirement has cases, so M4 is satisfied): REQ-TMPL-004 "each independently of the other two" (the three fixtures each carry one defect); REQ-TMPL-014 "a default outside its own offered terms" (only an invalid target grammar); REQ-TMPL-036 six of its eight malformations (not a mapping, no version, not a version number, repeated version, date not YYYY-MM-DD, unknown key); REQ-TMPL-037 a link that is not http(s)/mailto, a missing logo or Word template file, **a path that leaves the library folder**, a name/abbreviation count mismatch, a label judged "once its whitespace is collapsed", a wrong kind, YAML that does not parse; REQ-TMPL-021 a `modify:` verb, a `remove:`, an explicit null, a whole-section replacement, a `party_slots:` entry, `effects:`/`effects_all:`/`set:` maps, and "an entry inherited unchanged … is not itself a violation"; REQ-TMPL-032 every rebase sentence (sections compared and applied as one collection for additions, removals and order-only changes; a conflict requiring an explicit choice) — the case only shows that the metadata leaves hold `sections` as one entry; REQ-TMPL-047 "one line per finding, naming the file, the code and the message" (only the code is matched).
- Evidence: e.g. `qa_step("exactly one row", 1L, nrow(result)); qa_step("severity is warning", "warning", tmpl_severity(result, "id_missing"))` for each of the three REQ-TMPL-004 fixtures; `fx/t/c/vocab_slot_invalid` holds `target: not.a.valid.target`; `grep REQ-TMPL-032` finds test-OQ-TMPL-agreement.R:1 only.
- Failure scenario: mutations — the three REQ-TMPL-004 checks become an `if / else if` chain (a file missing id and version reports one row); the dta-app.yaml check accepts `logo: ../../etc/passwd`-style paths; an option added by a child with `effects: {set: {metadata.title: x}}` on a sealed title is not reported. None detected.
- Confidence: verified-by-reading (for REQ-TMPL-037 the four appconfig fixtures used were identified from the test text; `appconfig/valid` and `data_types_*` not opened)
- Suggested direction: one fixture per enumerated item, or narrow the requirement sentences to what is checked.

### F-q3-qual-tests-b-53  [nit] [inconsistency]
- Where: inst/qualification/tests/test-OQ-TMPL-validate.R:6-11; test-OQ-TMPL-repo.R:197-200; test-OQ-TMPL-library.R:167-170
- What: (a) the header of test-OQ-TMPL-validate.R states that every fixture's result was "verified by running validate_template() over it and reading back what actually came out, not by reasoning about the source alone" — the method maintaining.md forbids for expected values ("Never run the code, look at the output and paste it in as the expectation"); the codes and severities are in the requirement texts, but the "exactly one row" expectations of ~20 cases rest on that tuning. (b) OQ-TMPL-055 expects the `.gitignore` to be exactly `c(".Rproj.user", ".Rhistory", "*.Rproj")`, content no requirement states. (c) OQ-TMPL-097 skips with a bare `testthat::skip()` (maintaining.md: "through the helpers only"); on an unprivileged Windows account — the owner's primary machine — REQ-TMPL-044 (category security) is therefore never verified locally.
- Evidence: the three quoted locations.
- Failure scenario: a reviewer cannot tell which "exactly one row" expectations are specified and which were observed; a harmless change to the scaffolded `.gitignore` fails the qualification.
- Confidence: verified-by-reading
- Suggested direction: reword the header to say what is specified (code, severity) and what is fixture hygiene; assert only that the placeholder line is gone in OQ-TMPL-055.

## Area TYPE

(files fully read: requirements/REQ-TYPE.yaml, tests/test-OQ-TYPE-declared.R, -numeric.R, -rbackend.R; code traced: R/evaluateRules.R:10-90 `dta_as_numeric_strict()`)

Overall: test-OQ-TYPE-rbackend.R is exemplary — every fit/no-fit value is written from the requirement, the calendar rule is cross-checked against base R (`as.Date()`/`format()` round trip over a seeded sample, with a guard that both outcomes occur), findings are compared as exact (source, row, column, keyword) frames on both holdings and four batch sizes, and DEV-018 is bound with a narrow `qa_known_deviation()`. Only the following are worth recording.

### F-q3-qual-tests-b-54  [minor] [test-cheat]
- Where: inst/qualification/tests/test-OQ-TYPE-numeric.R:11-16, 20-44, 63-73 (OQ-TYPE-030; REQ-TYPE-012, REQ-TYPE.yaml:177-185); code R/evaluateRules.R:73-75
- What: the 13-row expected table (value, missing flag, unconvertible flag) has no source in the specification: REQ-TYPE-012 says each input is classified "into its documented value, missing flag and unconvertible flag" without stating any of them; the test says the table is "copied verbatim from … tests/testthat/helper-validation-corpus.R (`vc_numeric_edges()`)"; and the implementation cites the tests as its authority ("the oracle matrix (vc_numeric_edges) and the qualification matrix OQ-TYPE-numeric pin them as valid numbers"). The chain is circular, and the cases call the internal `dta_as_numeric_strict()` only — `apply_rules` and `check`, the `covers:` of REQ-TYPE-012..015, are never called in this file (and the cases are not tagged `white-box`, unlike OQ-TYPE-042/-049).
- Evidence:
  ```
  # The 13-case matrix is copied verbatim from the value/missing/unconvertible
  # table this package's own unit suite carries at
  # tests/testthat/helper-validation-corpus.R:304-347 (`vc_numeric_edges()`).
  ```
- Failure scenario: whether `"0x10"` is 16, `"Inf"` a value, `" 42 "` a number, `"1,5"` and `"TRUE"` unconvertible are specification decisions (LIM-025 exists for the hexadecimal one). With no requirement sentence stating them, a classification that was wrong when the developer corpus was recorded is pinned as correct, and a reviewer of the evidence cannot say "which sentence of which requirement the expected value comes from" (maintaining.md).
- Confidence: verified-by-reading
- Suggested direction: move the table into REQ-TYPE-012's text (or a referenced specification section) and add one end-to-end case through a range rule so the covered exports are exercised.

### F-q3-qual-tests-b-55  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-TYPE-declared.R:370-410 (OQ-TYPE-024, REQ-TYPE-009), :412-455 (OQ-TYPE-025, REQ-TYPE-002), :259-281 (OQ-TYPE-021, REQ-TYPE-010)
- What: three under-assertions. (a) REQ-TYPE-009 "stored as NA with its raw text kept", for a value that is "outside ±2^31-1, fractional or non-finite": the cases count import errors only — neither the recorded raw text nor the reason (`out_of_range` / `not_whole`) is read for a SAS Int column, and a non-finite value is not presented. (b) REQ-TYPE-002 (`covers: [load_file, check]`) is shown by calling the internal `dta_coerce_table_to_specs()` on a data frame split by hand, not by a batched read (untagged white-box). (c) the Date half of REQ-TYPE-010 compares only a missing value in a `SAS Date` column with no format, so no rendered date is ever compared with its text equivalent.
- Evidence: `qa_step("...and costs exactly one import error", c(ok = FALSE, columnspec = 0L, rule = 0L, import = 1L), td_axis(ds_wide))`; `whole_result <- dta_coerce_table_to_specs(whole, specs)`; `df_r$DT <- as.Date(c("2020-01-01", NA))` with `DT = DTAColumnSpec(id = "DT", type = "SAS Date", nullable = FALSE)`.
- Failure scenario: mutations — the import issue of an out-of-range Int records `raw = NA` or the reason `not_convertible`; the streaming reader bypasses `dta_coerce_table_to_specs()` for one batch shape; a typed Date is rendered as its day count before the `DATE9.` check. (b) is covered end to end for the R spelling by OQ-TYPE-050 and for `0.01` by the STREAM campaign, which is why this is minor.
- Confidence: verified-by-reading
- Suggested direction: assert `messages()` keyword and the inspect raw value in OQ-TYPE-024, add `Inf`, and give OQ-TYPE-021 a `DATE9.` column with one real date on both routes.

## Area VAL

(files fully read: requirements/REQ-VAL.yaml, tests/test-OQ-VAL-accessors.R, -check.R, -columnspec.R, -filecount.R; code traced: R/DTADataSetTabular-class.R 1472-1840 `check()`, R/streamingValidation.R 2362-2425 `dta_validate_any_table()`, R/sasTemporalText.R:1-10)

Overall: test-OQ-VAL-columnspec.R and test-OQ-VAL-filecount.R are strong — one defect per fixture, exact (source, row, column, keyword) frames, hand-computed SAS display widths, both engines, boundary values on both sides. Two cases in test-OQ-VAL-check.R are not.

### F-q3-qual-tests-b-56  [major] [test-cheat]
- Where: inst/qualification/tests/test-OQ-VAL-check.R:183-219 (OQ-VAL-025, only case for REQ-VAL-019, risk high); code R/streamingValidation.R:2415-2425, R/DTADataSetTabular-class.R:1450-1455
- What: the "threads on vs threads off" comparison is run on a table held in memory, the one holding for which `use_threads` is documented and coded to have no effect — both sides execute the identical call, so the step compares a result with a recomputation of itself.
- Evidence:
  ```
  load_file(ds, file = path, handler_index = 1, stream = "never")
  threaded <- check(load_once(), persist = FALSE, quiet = TRUE, use_threads = TRUE)
  serial <- check(load_once(), persist = FALSE, quiet = TRUE, use_threads = FALSE)
  qa_step("the same file reports the same messages with and without threads", qa_messages_norm(serial), qa_messages_norm(threaded))
  ```
  ```
  if (!dta_table_is_lazy(table)) {
    return(validate_table_detailed(specs, as.data.frame(table), verbose = verbose, max_errors = max_errors, structure = structure))
  }
  ```
  `use_threads` is not among the arguments passed on; the roxygen says "Ignored for a table held in memory."
- Failure scenario: mutation — the threaded scanner delivers batches out of order and the row offsets of findings shift (rows misnumbered when `use_threads = TRUE`), or drops the last batch: `check()` on a streamed table reports different messages with and without threads; OQ-VAL-025 passes unchanged. (OQ-STREAM-011 exercises the argument on `validate_file_stream()`, so the property is not wholly untested — but REQ-VAL-019, which covers `check`, is verified by a tautology.)
- Confidence: verified-by-reading
- Suggested direction: load with `stream = "always"` and a `batch_rows` small enough for several batches, assert the table is lazy (`dta_table_is_lazy`), and state the expected findings literally (rows 2, 3, 4) as well as comparing the two runs.

### F-q3-qual-tests-b-57  [major] [test-weak]
- Where: inst/qualification/tests/test-OQ-VAL-check.R:94-122 (OQ-VAL-022, REQ-VAL-016, risk high), :372-414 (OQ-VAL-038)
- What: REQ-VAL-016 says "a change to either the table or the specification shall cause revalidation without force"; the case titled "so does a change to either input" changes only the specification. No case replaces the contents of an in-memory table and checks again. The only thing standing between a table-insensitive skip gate and a green run is the DEV-002 binding of OQ-VAL-038, which would then fail with the misleading message "DEV-002 no longer reproduces; close it".
- Evidence:
  ```
  widened <- ds
  widened@specs <- vc_specs(list(… ID …, … EXTRA …))
  qa_step("a changed specification forces revalidation on its own", "validated", vc_call_status(check(widened, persist = FALSE, quiet = TRUE)))
  ```
  The gate is `identical(previous$table_hash, table_hash) && identical(previous$specs_hash, specs_hash)` (R/DTADataSetTabular-class.R:1624-1628).
- Failure scenario: mutation — drop the `table_hash` comparison (or make `dta_table_change_signal()` return a constant for Arrow tables): a dataset whose table was replaced by a corrected or a corrupted delivery under the same name is reported "skipped" with the old verdict — the "stale verdict presented as a current one" the requirement's note calls worse than none. OQ-VAL-021/-022 pass; OQ-VAL-038's `qa_known_deviation("DEV-002", identical(unnamed$call, "validated"))` fails, telling the maintainer to close DEV-002; once DEV-002 is closed nothing detects it. (For a lazily held matrix file the change path is covered indirectly by OQ-MATRIX-044.)
- Confidence: verified-by-reading
- Suggested direction: after the first `check()`, assign a different table under the same name (one value changed so that it violates a constraint) and assert the call status "validated" and the new verdict `ok = FALSE`; do the same for a `stream = "always"` tabular file rewritten in place.

### F-q3-qual-tests-b-58  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-VAL-check.R:39-67 (OQ-VAL-020), :124-157 (OQ-VAL-023); test-OQ-VAL-accessors.R:149-169 (OQ-VAL-033), :189-192 (OQ-VAL-034); test-OQ-VAL-columnspec.R:724-803 (OQ-VAL-043)
- What: smaller gaps. (a) REQ-VAL-014 names the `tables` and the `tab` argument; only `tables` is used. (b) REQ-VAL-017: the artifact verdict is compared with the in-memory one without either being stated (both `ok = FALSE`, 1 error by construction of the fixture), and "an artifact file was written" is `length(list.files(...)) > 0`. (c) REQ-VAL-025 "including the offending value and the surrounding row": only the offending value is looked for, in any column; the unknown-id refusal is "any condition". (d) `result_version` is expected to be `2L`, a number no requirement states. (e) REQ-VAL-031 "The month names shall not depend on the system language": the only month that occurs in a value expected to FIT is AUG, which is spelled the same in German (the primary dev locale); JAN and FEB occur only in rows that fail for their day.
- Evidence: `status <- validation_status(check(ds, tables = "one", persist = FALSE, quiet = TRUE))`; `list(ok = from_memory$ok, n = as.integer(from_memory$n_columnspec_errors))` vs the same from `from_artifact`; `any(vapply(detail, function(col) any(as.character(col) %in% "X"), logical(1)))`; `D = c("15AUG2025", "2025-08-15", NA, "15aug2025", "15AGU2025")`.
- Failure scenario: mutations — `tab = "one"` validates every table; `validation_errors(source = "artifact")` returns the in-memory store whenever a file exists; `inspect()` drops the `context_*` columns; the month list is taken from the session locale (`format(…, "%b")`), so `15MAY2025`/`15OCT2025`/`15DEC2025` are rejected on a German system — none detected. (The implementation hard-codes the English names, R/sasTemporalText.R:6-10; this is a test gap, not a defect.)
- Confidence: verified-by-reading
- Suggested direction: add `tab = "one"`; state `ok = FALSE, n = 1L` for both sources and read the `.rds`; assert `context_ID == "A002"`; use MAR/MAY/OCT/DEC as fitting dates.


## Area PQ-PERF

(files fully read: requirements/REQ-PERF.yaml, tests/test-PQ-PERF-workflows.R, -scale.R, -repeatability.R; helper-00-qa.R, helper-generators.R, helper-parse.R re-read; code traced: R/benchmarking.R, R/arrowCompute.R, R/run_dta_app.R (all whole), R/streamingValidation.R 30-460 and 463-560, R/evaluateRules.R 1452-1470 and 1725-1775, R/columnSpecChecks.R 640-719 and 955-997, R/DTADataSetTabular-class.R 1380-1710, R/DTA-class.R 1008-1067, R/qualification.R 395-460, 680-972, 1356-1370; fixtures: every inst/extdata/clinical_data_error_*.csv diffed against clinical_data.csv, clinical_dta.yaml whole, clinical_data2.csv and its .gz compared)

Overall: the workflow counts hold up. I re-derived the totals of all four deliberately-broken fixtures by diffing each against the clean delivery and applying clinical_dta.yaml by hand (columnspec fixture: HEIGHT absent x500 + "Alien" + "VISIT_CODE_TOO_LONG" x2 + "heavy" + "9999-9999" = 505/0/1; rules fixture: 7 of the 8 rules; combined: 10/6/4; import: 5/0/4) and they match the constants in the tests, so those expectations are not read back from a run, as the file header claims. The problems are the other three quarters of the area: parity cases whose two sides run the same code, cases whose only failure path is a skip, scale checks that compare counts only, and resource bounds applied to a figure that cannot show the failure.

### F-q3-qual-tests-b-59  [major] [test-cheat]
- Where: inst/qualification/tests/test-PQ-PERF-repeatability.R:75-87 (PQ-PERF-033, the only case for the third clause of REQ-PERF-013, risk high); code R/arrowCompute.R:67-70, 106-110; R/evaluateRules.R:597, 1740-1745; R/validationFunctions.R:127, 282; R/streamingValidation.R:283-340, 1062; R/streamingScale.R:167-169
- What: the "Arrow compute path on vs off" comparison is run with `stream = "always"`, but the option it toggles is read by exactly one function, `dta_count_duplicates()`, which only the in-memory rule evaluator reaches (`validate_table_detailed()` -> `apply_rules()` -> `rule_check_unique()`); the streaming engine evaluates uniqueness in `dta_rule_stream_update()` / `dta_stream_unique_precompute()` under a different switch, so both sides of the step execute identical code.
- Evidence:
  ```
  off <- withr::with_options(list(DTAtools.use_arrow_compute = FALSE), pr_messages(path, stream = "always"))
  on <- withr::with_options(list(DTAtools.use_arrow_compute = TRUE, DTAtools.arrow_min_rows = 1L), pr_messages(path, stream = "always"))
  ```
  ```
  if (dta_use_arrow_compute() && all_safe && nrow(df) >= getOption("DTAtools.arrow_min_rows", 100000L)) {   # dta_count_duplicates(): the only reader of either option
  dta_stream_unique_arrow_enabled <- function() isTRUE(getOption("DTAtools.stream_arrow_unique", TRUE)) && dta_arrow_compute_available()
  ```
  A search of R/ finds no other consumer of `dta_use_arrow_compute()` or `DTAtools.arrow_min_rows`; `apply_rules()` is called only from `validate_table_detailed()` and `validate_rules()`; the streaming driver calls `dta_rule_stream_update()` per batch.
- Failure scenario: mutation — the Arrow branch of `dta_count_duplicates()` miscounts (NA keys grouped differently by Acero, say): `check()` on an in-memory table reports a different duplicate count for a user who sets `options(DTAtools.use_arrow_compute = TRUE)`. PQ-PERF-033 compares two streaming runs that never enter that function and passes. The two switches that do change the streaming path (`DTAtools.stream_arrow_unique`, `DTAtools.stream_arrow_numeric`) are toggled by no case in the suite; they appear only as defaults in test-IQ-ENV.R:322-324. Together with F-42 (OQ-STREAM-019 cannot tell whether the Arrow branch ran) the requirement clause has no effective case.
- Confidence: verified-by-reading
- Suggested direction: run the pair with `stream = "never"`, assert `DTAtools:::dta_use_arrow_compute()` is TRUE on the "on" side, and add streaming pairs for `stream_arrow_unique` and `stream_arrow_numeric` TRUE/FALSE.

### F-q3-qual-tests-b-60  [major] [test-cheat]
- Where: inst/qualification/tests/test-PQ-PERF-repeatability.R:178-242 (PQ-PERF-037, the only case for REQ-PERF-016); R/run_dta_app.R:31-58; inst/shiny/dta_app/app.R:7-9; R/qualification.R:421-429, 725-735, 1358-1368
- What: the launch check cannot fail for an application that does not start: every way of not serving a page ends in `testthat::skip()`, and the function the requirement covers, `run_dta_app()`, is never called — the script the case writes calls `shiny::runApp()` on the directory.
- Evidence:
  ```
  sprintf("shiny::runApp(app, port = %d, launch.browser = FALSE, host = \"127.0.0.1\")", port)
  page <- tryCatch(suppressWarnings(readLines(url(sprintf("http://127.0.0.1:%d", port)), warn = FALSE)), error = function(e) NULL)
  if (!is.null(page) && length(page) > 0) { reachable <- TRUE; qa_check(...); qa_step(...) }     # the only assertions
  if (!reachable) { testthat::skip("the application did not become reachable within 60 seconds") }
  ```
  A case holding a skip is recorded `"skip"`, its requirement `"not_verified"`, and the run verdict becomes "PASS WITH NOT-EXECUTED TESTS".
- Failure scenario: mutations — a syntax error in app.R; `library(shinyjs)` failing (the case requires shiny, bslib and DT but not shinyjs, which app.R attaches unconditionally and `run_dta_app()` checks for); a UI that errors, so the server answers HTTP 500 and `readLines(url())` throws; `run_dta_app()` resolving the wrong directory or mis-passing `port`. Each ends in the skip, and the run still "passes". In addition the child is started `--vanilla` with no library paths handed over (PQ-PERF-034, line 119, passes `R_LIBS_USER = paste(.libPaths(), ...)`; this case does not), so wherever the suite runs from a library the child's default start-up does not reproduce, the case skips on every run.
- Confidence: verified-by-reading (that a particular environment makes it skip every time is inferred)
- Suggested direction: start `DTAtools::run_dta_app(launch.browser = FALSE, port = )` in the child with the parent's `.libPaths()`, capture its stderr, and FAIL with that output when no page is served; skip only for the absent optional packages.

### F-q3-qual-tests-b-61  [minor] [bug]
- Where: inst/qualification/tests/test-PQ-PERF-repeatability.R:186, 200-220 (PQ-PERF-037); tests/setup-qualification.R:19; tests/helper-generators.R:179
- What: the server the case starts is not reliably stopped and its port is not random: the kill step looks for a window titled `launch.R` on Windows and for any command line containing `launch.R` elsewhere, and the port is drawn from the suite's seeded RNG.
- Evidence:
  ```
  port <- sample(20000:60000, 1)
  system2(rscript, c("--vanilla", shQuote(normalizePath(script, winslash = "/"))), wait = FALSE, stdout = FALSE, stderr = FALSE)
  c("/F", "/FI", shQuote(sprintf("WINDOWTITLE eq %s", basename(script))))     # windows: taskkill
  c("-f", basename(script))                                                  # elsewhere: pkill
  ```
  `set.seed(qa_seed())` runs at set-up and `qa_frame()` calls `set.seed(seed + offset)` for every generated file — the last time in PQ-PERF-036, immediately before this case.
- Failure scenario: on Windows a detached `Rscript` owns no window titled `launch.R`, so `taskkill` matches nothing and the Shiny process keeps listening after the run (the comment above it says "killed in every exit path"). The next run draws the same port; its own app cannot bind; the poll is answered by the server left over from the previous run — possibly of an older installed version — and PQ-PERF-037 passes on a page the package under qualification never served. On Unix `pkill -f launch.R` also kills any other process of the user whose command line contains that string, e.g. a second qualification run.
- Confidence: inferred (Windows window-title matching and the RNG state at line 186 are reasoned from the code; nothing was executed)
- Suggested direction: have the child write `Sys.getpid()` to a file and kill that PID; let the OS choose the port (`httpuv::randomPort()`), and look in the page for something only this run knows.

### F-q3-qual-tests-b-62  [major] [test-weak]
- Where: inst/qualification/tests/test-PQ-PERF-repeatability.R:89-133 (PQ-PERF-034, the only case for REQ-PERF-014, risk high); tests/helper-generators.R:18-57, 176-218; tests/helper-00-qa.R:358-371; code R/evaluateRules.R:1463-1470
- What: the locale case can report a locale-dependent failure only as a skip, and the delivery it validates contains nothing whose outcome could depend on collation: ASCII, upper-case-only values, a numeric range, an exact-match key and no grouped rule.
- Evidence:
  ```
  in_c_locale <- tryCatch(qa_subprocess({ ... qa_messages_norm(check(ds, persist = FALSE, quiet = TRUE)) }, envvars = c(LC_ALL = "C", LANGUAGE = "C", ...)), error = function(e) e)
  if (inherits(in_c_locale, "condition")) { testthat::skip(paste0("the C-locale subprocess could not run: ", conditionMessage(in_c_locale))) }
  ```
  `qa_specs()`: SUBJID `^S[0-9]{7}$`, SEX in M/F, DOMAIN "GF", `DTARuleColRange` on AGE, `DTARuleColUnique`, one `DTARuleColCondition`. The package's own note on the collation defect it once had: "group order decides the assembled violation message order, and locale collation made the same file report groups in a different order on a de_DE dev machine than under CI's C collation" — a `group_condition` rule, which this delivery does not carry.
- Failure scenario: mutations — `sort(unique(split_key), method = "radix")` loses its `method` (the regression that comment describes); `order(out$row, out$column, method = "radix")` likewise; a text bound compared with `<` in the session collation; `check()` aborting in a C session on a non-ASCII value. The first three change nothing for this delivery. The fourth leaves no result file, `qa_subprocess()` stops, and the case is skipped: REQ-PERF-014 "not verified", verdict "PASS WITH NOT-EXECUTED TESTS". (`qa_messages_norm()` also orders rows with `order()` on character keys in each process's own collation; the two frames are comparable only because no key here sorts differently.)
- Confidence: verified-by-reading
- Suggested direction: give the case its own delivery — mixed-case and non-ASCII group keys under a grouped rule, two findings in one row in columns whose names differ in case, a text bound — sort the normalised frames with `method = "radix"`, and treat a child that errors after the package has loaded as a failure.

### F-q3-qual-tests-b-63  [major] [test-weak]
- Where: inst/qualification/tests/test-PQ-PERF-scale.R:48-123 (PQ-PERF-020, -021, -022; REQ-PERF-006, -007, -008, all risk high); test-PQ-PERF-workflows.R:148-179 (PQ-PERF-006, -007); tests/helper-generators.R:81-100, 111-136, 140-167, 252-267; tests/helper-00-qa.R:373-392
- What: "exactly those defects, and the same ones" and "identical messages" are asserted on message counts per (source, target, keyword) and on three axis totals. No row, column, rule id or message text is compared at any tier, the injection plan the generator returns is thrown away, and nothing asserts how many rows were read.
- Evidence:
  ```
  qa_write_large_csv(rows, path)                                   # returns list(path, rows, plan, expected); the value is dropped
  qa_step(..., qa_oracle_counts(run$table), run$counts)            # target taken from the run; the three rules collapse to keyword "" n = 3
  qa_step(sprintf("at %d rows the two engines report the same errors", rows), eager$counts, streamed$counts)
  ps_measure(plain, "always", "PQ-PERF-022", "plain", rows)$axes, ps_measure(gzipped, "always", "PQ-PERF-022", "gzip", rows)$axes
  ```
  `slots <- unique(round(seq(2, n_rows - 1, length.out = total)))` gives the last three of the 19 slots to `visit_v00`, and a rule contributes one message however many rows break it. `$plan` and `by_rule` are used by no test in the suite.
- Failure scenario: mutations — (a) the streaming engine numbers rows per batch, or shifts them by one: every count is unchanged; (b) the scan stops short — the final read block, or the tail of a `.gz` — losing every row after slot 17, the last ~11 % of the file: only two redundant `V00` rows are lost, `adult_visit` is still violated, every count and axis is unchanged; the rule text would say 1 row instead of 3, but text is not compared, and the streaming `n_rows_scanned` attribute is never held against `rows`; (c) a finding attributed to the wrong column, or two rules reported under one id. For REQ-PERF-008 the shipped twin is weaker still: `clinical_data2.csv` is a clean 12-row delivery (checked by hand against clinical_dta.yaml), so PQ-PERF-007 compares `c(ok = TRUE, 0, 0, 0)` with itself and a gzip reader that returns the header and any prefix of the rows passes.
- Confidence: verified-by-reading (the block layout at each tier is inferred from the documented 8 MiB read block and the 84-byte generated row)
- Suggested direction: keep the plan; assert the (row, column, keyword) each per-cell defect predicts, the violating-row count of each rule (4; 3 duplicates — the two `BADPAT1` ids duplicate each other; 3), and `n_rows_scanned == rows`; compare `qa_messages_norm()` between engines and between plain and gzip; put a per-cell defect in the last row.

### F-q3-qual-tests-b-64  [major] [test-weak]
- Where: inst/qualification/tests/test-PQ-PERF-scale.R:200-258 (PQ-PERF-024, -025; REQ-PERF-009 risk medium, REQ-PERF-010 risk high); code R/benchmarking.R:73, 127-133; R/DTADataSetTabular-class.R:1403-1411, 1450-1455
- What: the memory bound is applied to a number that cannot show the failure the requirement names. `r_peak_mb` is the absolute R-heap high-water mark — the whole session footprint, nothing subtracted — while a streamed scan's buffers live in Arrow's pool, which the package itself documents as "outside the R heap and invisible to gc()"; the Arrow and RSS figures are recorded and never judged. Both growth cases also stop at 1e6 rows even at the full tier, and turn a zero baseline into "ratio 1".
- Evidence:
  ```
  tiers <- utils::head(sort(qa_tier_rows()), 3)
  peak <- c(peak, run$metrics$r_peak_mb)
  ratio <- if (peak[[i]] > 0) peak[[i + 1]] / peak[[i]] else 1
  qa_check(sprintf("... peak R memory grew x%.2f, within the bound of x3", ...), ratio <= 3, detail = c(peak[[i]], peak[[i + 1]]))
  ```
  `gc(reset = TRUE, full = TRUE)` at the start and `r_peak_mb <- if (length(idx) == 1L) sum(g[, idx + 1L]) else NA_real_` at the end (benchmarking.R). The roxygen of `check()`: "a batch is one Arrow read block of about ... 8 MiB", so the 1e4-row (0.84 MB) and 1e5-row (8.4 MB) deliveries are each read as about one batch: the first step compares two whole-file reads.
- Failure scenario: mutation — the streaming path collects the Dataset into an Arrow Table before iterating (memory proportional to the file, in Arrow's pool): `r_peak_mb` does not move, `arrow_pool_peak_mb` and `rss_end_mb` grow tenfold per tier and are not looked at; PQ-PERF-025 passes. A variant that materialises in R adds its growth on top of the heap a session that has just run IQ and OQ already holds, so whether the absolute peak triples depends on that baseline and not on the engine. For time, the x20-per-x10 bound is a sound detector of a quadratic term for the 1e5 -> 1e6 step only: a cost quadratic in the number of batches is invisible at ten batches, and the 1e7 tier that would show it is dropped by `head(..., 3)`. `r_peak_mb == 0` (a broken measurement) gives ratio 1 and a pass.
- Confidence: verified-by-reading for what is measured and judged; inferred for the magnitudes (nothing was run)
- Suggested direction: judge the increase over the value at the start of the window; judge `arrow_call_mb` and RSS as well; compare tiers that are both many blocks long (lower `DTAtools.stream_block_size`, or 1e6 -> 1e7 at the full tier); fail when a baseline is not positive.

### F-q3-qual-tests-b-65  [major] [test-cheat]
- Where: inst/qualification/tests/test-PQ-PERF-scale.R:125-198 (PQ-PERF-023, the only case for REQ-PERF-011); tests/helper-00-qa.R:260-294; code R/benchmarking.R:40-45, 176-180; R/DTA-class.R:1366; inst/qualification/deviations.yaml:574-586 (LIM-009)
- What: of the seven quantities REQ-PERF-011 says every measured run shall record, one is asserted on its value (elapsed, as `>= 0` under the description "finite positive"), three are checked by column name only (processor time, peak R memory, resident memory), one by a check that is also true for a missing value (Arrow pool peak), and the last two (row count, throughput) are pinned as ABSENT — while the case passes and the requirement reads verified.
- Evidence:
  ```
  qa_check("and the Arrow pool peak was observed",
    is.finite(run$metrics$arrow_pool_peak_mb) || is.na(run$metrics$arrow_pool_peak_mb))
  qa_step("the engine reports no row count at transfer level, and so no rate", c(rows = TRUE, rows_per_sec = TRUE),
    c(rows = is.na(run$metrics$rows), rows_per_sec = is.na(run$metrics$rows_per_sec)))
  qa_check("the recorded evidence nonetheless carries a finite, positive throughput", nrow(perf) > 0 && all(is.finite(perf$rows_per_sec)) && all(perf$rows_per_sec > 0))
  ```
  The throughput in the last check is `engine_rate <- rows / elapsed`, computed by `qa_perf()` in the helper from the row count the test itself passed in. `dta_arrow_pool_max_bytes()` returns `NA_real_` on any error.
- Failure scenario: mutations — `arrow::default_memory_pool()$max_memory` is renamed and the guarded read returns NA on every run; `cpu_user_sec`/`cpu_sys_sec` are always 0; `r_peak_mb` is NA because `gc()`'s "max used" column is not found: the case passes and its evidence reads "the Arrow pool peak was observed". For rows and throughput the requirement's own clause ("together with the row count and the throughput derived from them") is false for every measurement the suite takes (LIM-009; limitations do not change a requirement's status, only open deviations do), and the engine's `rows_per_sec` arithmetic (benchmarking.R:176-180) is asserted nowhere in the suite — OQ-STREAM-013 checks `rows` only.
- Confidence: verified-by-reading
- Suggested direction: assert each cost field on its value (finite, and positive where it must be; RSS only when `ps` is installed); take one measurement through `validate_file_stream(benchmark = TRUE)` and assert `rows == n` and `rows_per_sec == rows / elapsed_sec`; narrow the requirement text to what a transfer-level check records.

### F-q3-qual-tests-b-66  [major] [test-weak]
- Where: inst/qualification/tests/test-PQ-PERF-repeatability.R:135-158 (PQ-PERF-035; REQ-PERF-015 "shall not accumulate memory or leave temporary files behind", risk medium); code R/streamingValidation.R:497-510; R/DTAFileTabular-class.R:553-566; R/00_helpers.R:1026-1029
- What: the case cannot fail, and half of the requirement is not measured at all. The delivery it validates never makes the package create a temporary file (11 per-cell findings (9 column-specification, 2 import) against a retention cap of 10,000; ASCII, so no transcoded copy), the threshold tolerates one leaked file per validation, directories are not counted, and nothing measures memory (the `gc()` call discards its result). Where the package does create temporary files it keeps them for the session by design — the opposite of what the case's own comment calls legitimate — and no limitation is registered against REQ-PERF-015.
- Evidence:
  ```
  before <- length(list.files(temp_root, recursive = TRUE))
  for (i in seq_len(5)) { invisible(check(qa_dta(path, stream = "always"), persist = FALSE, quiet = TRUE)) }
  gc(verbose = FALSE)
  qa_check(sprintf("five validations added no more than a handful of files ...", before, after), after - before <= 5, ...)
  ```
  ```
  # One overflowing frame to disk. tempfile() scopes the spill to the R session: ... recoverable for as long as the session's tempdir lives
  sink$spill_dir <- tempfile("dta_error_spill_", tmpdir = dta_temp_dir()); dir.create(sink$spill_dir, recursive = TRUE)
  ```
  No `unlink()` of a spill directory exists anywhere in R/.
- Failure scenario: (a) mutation — each `check()` leaves one scratch file: five new files, `5 <= 5`, pass. (b) as shipped — a long-lived process that streams (the case's own "server checking deliveries all day") validates a delivery with one wrongly formatted column: every bad cell past the first 10,000 is written to `dta_error_spill_*/part-*.rds` and stays until the process ends; ten such checks leave ten directories, and REQ-PERF-015 reads verified. (c) memory that grows by a constant per validation is looked at by no case.
- Confidence: verified-by-reading
- Suggested direction: validate a delivery that spills (`max_errors = 10`) and a non-UTF-8 one; compare the file and directory listing exactly (`setdiff`); measure `gc()`'s used memory across twenty repeats; and either remove spill parts when their result is dropped or register the retention as a limitation and reword the requirement.

### F-q3-qual-tests-b-67  [minor] [test-weak]
- Where: inst/qualification/tests/test-PQ-PERF-repeatability.R:160-176 (PQ-PERF-036, REQ-PERF-015)
- What: "leaves the session as it found it" compares option NAMES only, and takes its "before" picture in a session in which `check()` has already run on the same kind of delivery in the six cases above it (and throughout OQ), so any change `check()` makes the same way on every call is already part of the baseline.
- Evidence:
  ```
  options_before <- options(); locale_before <- Sys.getlocale(); wd_before <- getwd()
  invisible(check(qa_dta(path, stream = "always"), persist = FALSE, quiet = TRUE))
  changed <- setdiff(names(options()), names(options_before))
  qa_step("no new global options are left set", character(0), changed)
  ```
- Failure scenario: mutations — `check()` sets `options(warn = -1)` or `options(arrow.use_threads = FALSE)` without restoring it (an existing name, invisible to `setdiff(names())`); it sets a new option, calls `Sys.setlocale("LC_COLLATE", "C")`, or `setwd()`s to a fixed directory on every call (the first call was PQ-PERF-030's, so before equals after). Arrow's thread count, the RNG state, open connections and environment variables are not looked at here (F-23 covers the OQ-ROBUST counterpart).
- Confidence: verified-by-reading
- Suggested direction: compare the common options by value, and take the baseline in a fresh `qa_subprocess()` that has never called the package.

### F-q3-qual-tests-b-68  [major] [test-weak]
- Where: inst/qualification/tests/test-PQ-PERF-workflows.R:181-236 (PQ-PERF-008, the only case for REQ-PERF-003, risk high); tests/helper-parse.R:51-117
- What: "the standalone HTML report, the Word document and the exported data file shall each be produced from the same verdict and shall agree with it" is checked by the NUMBER of message rows in the report, by the presence of the specification's title in the Word text, and by the export function's own return value. Neither the report's summary counts and target status, nor anything else in the Word document, nor the contents of the exported file is compared with the verdict (ok = FALSE, 10/6/4) or with the data.
- Evidence:
  ```
  qa_step("and lists exactly the messages the engine reported", nrow(as.data.frame(messages(ds))), parsed$n_messages)
  qa_step("and names the transfer it describes", TRUE, grepl(dta@metadata@title %||% "", text, fixed = TRUE))
  qa_step("the recorded checksum is the checksum of the file on disk", unname(tools::md5sum(out)), written$md5sum)
  c(rows = as.integer(written$n_rows), columns = as.integer(written$n_cols))
  ```
  `qa_html_report()` already returns `counts`, `target_statuses` and the `messages` frame; this case uses none of them.
- Failure scenario: mutations — the report's overview shows 0 errors and the target as passed above a table of 20 rows (the shape of the closed DEV-011); the 20 rows are the right number of wrong messages; `write_dta()` writes the title page and an empty specification; `write_table_to_file()` leaves a header-only or truncated file and returns `n_rows = nrow(table)` (the shape of the closed DEV-013) — the checksum step still passes, because it hashes whatever is on disk. `%||% ""` would also turn a missing title into `grepl("", ...)`, which is always TRUE.
- Confidence: verified-by-reading
- Suggested direction: assert `parsed$counts` and `parsed$target_statuses` against 10/6/4 and the failed status, compare the report's message cells with `messages(ds)`, assert the Word tables list the 14 declared columns (`qa_docx_tables()`), and read the exported file back (501 lines, 14 fields, first and last row equal to the fixture's).

### F-q3-qual-tests-b-69  [major] [test-weak]
- Where: inst/qualification/tests/test-PQ-PERF-workflows.R:238-303 (PQ-PERF-009, -010: REQ-PERF-004; PQ-PERF-011: REQ-PERF-005); inst/extdata/clinical_dta_with_file_dataset.yaml:274-285
- What: three end-to-end cases titled "loads and validates" accept any verdict, and one of them validates nothing. PQ-PERF-010 reads the YAML and looks at class names: no file is bound and `check()` is never called, although the specification's file dataset expects `clinical_data2.csv.gz`, which ships next to it. PQ-PERF-009 and -011 require only that the status is "validated" and `ok` is not NA. `results`, listed in REQ-PERF-004's `covers`, is not called.
- Evidence:
  ```
  qa_step("both deliveries are validated", 2L, sum(status$status == "validated"))
  qa_step("and each reaches its own verdict", 2L, sum(!is.na(status$ok)))
  qa_check("it declares a file-presence dataset alongside the tabular one", any(grepl("DTADataSetFile", kinds, fixed = TRUE)))   # PQ-PERF-010 ends here
  qa_step("the delivery is validated", "validated", status$status); qa_check("and reaches a verdict", !is.na(status$ok))
  ```
- Failure scenario: mutations — with two files bound to one handler the second table is validated against the first table's rows, or its findings are filed under the first; `check()` on a `DTADataSetFile` aborts, or reports every expected file present; every value check of the second-domain specification stops firing, or fires on every row: all three cases pass. Independent expectations were available: `clinical_data2.csv` is clean by hand-check and `clinical_data.csv` is the delivery PQ-PERF-001 states is clean, so `ok = c(TRUE, TRUE)` with zero counts per table could be stated.
- Confidence: verified-by-reading
- Suggested direction: state the verdicts per table; bind the shipped `.gz` to the file dataset and assert present -> ok and absent -> the missing-file finding; pin the second-domain delivery's counts from its fixture.

### F-q3-qual-tests-b-70  [minor] [test-weak]
- Where: inst/qualification/tests/test-PQ-PERF-workflows.R:55-146 (PQ-PERF-002 ... -005; REQ-PERF-002, risk high); code R/columnSpecChecks.R:697-711
- What: the four broken-fixture cases state totals that can be re-derived from the fixtures, but mostly not WHICH findings. PQ-PERF-003 counts distinct rule ids (7) without naming them; PQ-PERF-004 and -005 assert no keyword, column or row for their 10 and 5 column-specification findings; "each names the row it came from" is `sum(!is.na(import$row)) == 4`; and PQ-PERF-002's "in the columns it was built to break them in" omits HEIGHT — the column the fixture breaks 500 times — because the engine leaves `column` NA for an absent column, so the expected set follows the engine's representation rather than the fixture.
- Evidence:
  ```
  qa_step("and seven distinct rules are named", 7L, length(unique(as.character(msgs$rule_id))))
  qa_step("the recorded counts", c(ok = FALSE, columnspec = 5L, rule = 0L, import = 4L), pw_axes(ds))
  qa_step("and each names the row it came from", 4L, sum(!is.na(import$row)))
  c("BMI", "GENDER", "STUDYID", "VISIT"), sort(unique(as.character(msgs$column[msgs$source == "columnspec" & !is.na(msgs$column)])))
  ```
  From the fixtures: the rules fixture breaks every rule except `group_condition_pass_example`; the import fixture has unparseable values in rows 5 (HEIGHT), 6 (AGE), 9 (BMI), 18 (WEIGHT) and an NA BMI in row 2.
- Failure scenario: mutations — import findings numbered from 0 or per batch (rows 4, 5, 8, 17); `group_condition_pass_example` reported violated and `rule_range_example` not; the 500 `required` findings naming WEIGHT instead of HEIGHT; five column-specification findings in the wrong rows of the import fixture: every stated total still holds.
- Confidence: verified-by-reading (fixtures diffed against clinical_data.csv, the specification applied by hand)
- Suggested direction: state the identities the fixtures give for free — the seven rule ids, the (row, column, keyword) of each value-level finding, the import rows `c(5L, 6L, 9L, 18L)`, and that the `required` messages name HEIGHT.

### F-q3-qual-tests-b-71  [minor] [test-weak]
- Where: inst/qualification/tests/test-PQ-PERF-repeatability.R:9-49 (pr_delivery; PQ-PERF-030, -031; REQ-PERF-012, first clause of REQ-PERF-013); code R/00_helpers.R:239, R/DTADataSetTabular-class.R:1403-1411
- What: the repeatability and thread cases run on a delivery that is a single batch. `pr_delivery()` writes `min(qa_tier_rows())` = 10,000 rows (0.84 MB) at every scale tier, below the 8 MiB read block and the 131,072-row cap, so there is nothing for a second thread or a second run to interleave; only PQ-PERF-032 forces several batches. All of these cases use the streaming engine only, and only PQ-PERF-030 checks that anything was reported.
- Evidence:
  ```
  pr_delivery <- function(dir, name = "repeat.csv", rows = NULL) { rows <- rows %||% min(qa_tier_rows()) ...
  original <- set_dta_compute_threads(1L); ...; single <- pr_messages(path, stream = "always")
  set_dta_compute_threads(2L); double <- pr_messages(path, stream = "always")
  ```
- Failure scenario: mutation — with `use_threads = TRUE` the scanner hands batches to R out of file order and row numbers (or the first-seen side of a duplicate) follow arrival order: results differ between one and two threads, and between runs, on any delivery longer than one block. PQ-PERF-030 and -031 see one batch and pass. This is the mutation F-56 names for OQ-VAL-025; REQ-PERF-013's thread clause does not close it. A repeated in-memory validation is not compared at all.
- Confidence: verified-by-reading (the single-batch reading is inferred from the documented block size and the 84-byte generated row)
- Suggested direction: run the thread and repeat cases under `withr::local_options(DTAtools.stream_block_size = 65536)` (or `batch_rows = 64`), with one against four threads, and add an in-memory repeat.

### F-q3-qual-tests-b-72  [minor] [inconsistency]
- Where: R/qualification.R:421-429, 725-735 (runner; reported as c4-tmpl-export F-01); in this slice test-PQ-PERF-scale.R:51-54, 84-86 (PQ-PERF-020, -021), test-PQ-PERF-workflows.R:148-163 (PQ-PERF-006), test-OQ-TYPE-declared.R:283-284 (OQ-TYPE-022), test-OQ-SPEC-lint.R:295-297 (OQ-SPEC-053); test-OQ-CORE-runner.R:564-613 (outside the slice, read for this cross-check only)
- What: cross-check of the "one passing case makes a requirement verified" rule against this slice. The rule is confirmed in the code. No case in this slice asserts anything about the traceability matrix — test-OQ-META-*.R qualify `DTAMetaData` (titles, version history, transmission dates), not the suite's meta level — and the one case that does, OQ-CORE-011, builds four requirements with one case each (pass, pass, pass, fail), never a requirement whose cases are pass + skip. In this slice the rule turns three requirements "verified" while the case carrying a distinct part of them is skipped.
- Evidence:
  ```
  } else if (any(types == "skip")) { "skip"                    # qual_tests_df(): a case with successes and one skip is "skip"
  } else if (any(mine$status == "pass")) { "verified"          # qual_traceability()
  if (rows >= 1e6) { qa_skip_unless_memory_gb(8) }             # PQ-PERF-021, inside the tier loop
  qa_requires("bit64")                                         # OQ-TYPE-022
  ```
- Failure scenario: on a target with less than 8 GB free, PQ-PERF-021 skips at the 1e6 tier (discarding the steps already passed for 1e4 and 1e5), PQ-PERF-006 — five 500-row shipped files compared on four numbers — passes, and REQ-PERF-007 ("identical messages ... at every scale tier", risk high) is reported verified. Likewise REQ-TYPE-010 without bit64 (its integer64 clause is OQ-TYPE-022 alone; it shows as verified_with_deviation through DEV-018) and REQ-SPEC-028 without the bundled example. The opposite also occurs: PQ-PERF-020 skipping at 1e7 turns REQ-PERF-006 "not verified" although three tiers passed.
- Confidence: verified-by-reading
- Suggested direction: with the runner fix, record each tier as its own case (or let a skip inside a loop keep the steps already recorded), require "no case skipped" for verified, and add a pass + skip requirement to OQ-CORE-011.

### F-q3-qual-tests-b-73  [minor] [design]
- Where: inst/qualification/tests/test-PQ-PERF-scale.R:22-46, 91-92 (ps_measure, PQ-PERF-021); tests/helper-00-qa.R:260-294; R/qualification.R:909-971 (M14)
- What: the performance evidence does not say what it measured. PQ-PERF-021 records the in-memory and the streamed run under the same tc_id, tier, row count and file name, with no engine column, and the two windows are not the same work: the in-memory run's file read happens in `load_file()` before `check(dta, benchmark = TRUE)` starts the clock, the streamed run's read happens inside it. `qa_perf()` also writes nothing when no bundle directory is configured, so PQ-PERF-023 fails when the file is run by hand.
- Evidence:
  ```
  ds <- qa_dta(path, id = "qual", stream = stream)             # the load — and, for "never", the read — is outside the window
  dta <- check(dta, benchmark = TRUE, persist = FALSE, quiet = TRUE)
  eager <- ps_measure(path, "never", "PQ-PERF-021", as.character(rows), rows)
  streamed <- ps_measure(path, "always", "PQ-PERF-021", as.character(rows), rows)
  if (is.null(bundle) || is.null(metrics) || nrow(metrics) == 0) { return(invisible(NULL)) }
  ```
- Failure scenario: a reviewer — or M14's `perf_floor`, which takes every finite `rows_per_sec` in performance.csv — compares the "rows per second" of two PQ-PERF-021 rows that differ only in position; the in-memory figure excludes parsing and looks several times faster than the engine actually used at scale, and a floor chosen from one fails the other.
- Confidence: verified-by-reading (that an in-memory `load_file()` parses the file at load time is taken from `read_file_execution()` and was not traced line by line)
- Suggested direction: add an `engine` field to the record and time load + check together for both engines, or record the load separately.

### F-q3-qual-tests-b-74  [minor] [doc-wrong]
- Where: inst/qualification/tests/helper-generators.R:77-80, 119-126, 176-218 (qa_defect_plan, qa_oracle, qa_frame)
- What: three statements the suite's "closed-form oracle" rests on are not true of the code. (a) `qa_frame()` takes `seed` and calls `set.seed(seed + offset)` but draws nothing — the frame is a pure function of `n_rows`, `offset` and the plan — so the argument is dead (OQ-STREAM passes 7L and 11L and gets the same data) and its only effect is to reset the session RNG for whatever runs next. (b) "one bad cell is never counted on two axes" holds only because AGE is nullable here; in the shipped fixtures every unparseable value in a non-nullable column is counted on both (that is PQ-PERF-005's 5 = 1 + 4). (c) "One row carries at most one injected defect, which is what keeps the oracle additive": the two `subjid_pattern` rows receive the same id and duplicate each other, so the uniqueness rule is violated even with `subjid_dup = 0`, where `qa_oracle()` says it is not; and "at any batch size the defects fall in different batches" — at 1e4 and 1e5 rows the delivery is one or two read blocks.
- Evidence:
  ```
  set.seed(seed + offset)                         # nothing below draws: sprintf(), rep(), %%
  frame$SUBJID[at$subjid_pattern] <- "BADPAT1"
  subjid_unique = as.integer(defects[["subjid_dup"]] > 0),
  # ... so one bad cell is never counted on two axes.
  ```
- Failure scenario: a maintainer varies the defect vector (`subjid_dup = 0L`) or relies on `seed` for a second, different delivery: the oracle is wrong by one rule in the first case and the "different" delivery is byte-identical in the second. Every `sample()` that follows a generated file (the port in PQ-PERF-037, `qp_rules()` in OQ-STREAM, see F-45) is silently made deterministic.
- Confidence: verified-by-reading
- Suggested direction: drop `seed` and the `set.seed()` call (or really randomise the filler), give the pattern defects distinct ids, and correct the three comments.

## Coverage

All paths relative to `$SNAP`. Nothing was executed: no `R`/`Rscript`, no server, no browser. Every statement about behaviour comes from reading test and package source and, for the shipped example deliveries, from diffing the files and applying the specification by hand.

### Slice files — all 41 fully read (every line)

- OQ-MATRIX (6): test-OQ-MATRIX-cells.R, -findings.R, -header.R, -parity.R, -rows.R, -yaml.R
- OQ-META (2): test-OQ-META-agreement.R, -metadata.R
- OQ-REPORT (1): test-OQ-REPORT-html.R
- OQ-ROBUST (2): test-OQ-ROBUST-inputs.R, -isolation.R
- OQ-RULE (3): test-OQ-RULE-group.R, -operators.R, -types.R
- OQ-SPEC (5): test-OQ-SPEC-accessors.R, -classes.R, -ids.R, -lint.R, -yaml.R
- OQ-STREAM (3): test-OQ-STREAM-arguments.R, -filecount.R, -parity.R
- OQ-TMPL (9): test-OQ-TMPL-agreement.R, -guide.R, -inherit.R, -library.R, -lint.R, -matrix.R, -paths.R, -repo.R, -validate.R
- OQ-TYPE (3): test-OQ-TYPE-declared.R, -numeric.R, -rbackend.R
- OQ-VAL (4): test-OQ-VAL-accessors.R, -check.R, -columnspec.R, -filecount.R
- PQ-PERF (3): test-PQ-PERF-repeatability.R, -scale.R, -workflows.R

### Supporting files fully read

- scratchpad/REVIEW-BRIEF.md, scratchpad/slices/q3-tests-b.txt
- inst/qualification/docs/maintaining.md
- inst/qualification/tests: helper-00-qa.R, helper-generators.R, helper-parity.R, helper-matrix.R, helper-parse.R, setup-qualification.R
- inst/qualification/requirements: REQ-MATRIX, REQ-META, REQ-REPORT, REQ-ROBUST, REQ-RULE, REQ-SPEC, REQ-STREAM, REQ-TMPL, REQ-TYPE, REQ-VAL, REQ-PERF (.yaml)
- R/: benchmarking.R, arrowCompute.R, run_dta_app.R, DTADataSetMatrix-class.R, DTAColumnSpecCell-class.R, DTAMetaData-helpers.R
- inst/extdata: clinical_dta.yaml, clinical_data2.csv; the four clinical_data_error_*.csv as diffs against clinical_data.csv

### Read in part / skimmed (line ranges are in each area's header note)

- inst/qualification/tests/helper-app.R — only the `qapp_*` loader and its skip paths
- inst/qualification/deviations.yaml — the id/title/affects index of every entry; full text of DEV-001, DEV-002, LIM-002, LIM-003, LIM-004, LIM-009 only
- inst/qualification/tests/test-OQ-CORE-runner.R — lines 536-613 and the list of case titles only; outside the slice, read for the traceability cross-check (F-72)
- R/qualification.R — 395-460 (case status), 680-972 (traceability, meta-checks, M14), 1356-1370 (verdict); the rest not read
- R/evaluateRules.R, R/validationFunctions.R, R/validationReporting.R, R/reportRendering.R, R/columnSpecChecks.R, R/streamingValidation.R, R/streamingScale.R, R/DTADataSetTabular-class.R, R/DTA-class.R, R/DTADataSet-class.R, R/DTAColumnSpecCollection-class.R, R/DTARuleGroupCondition-class.R, R/lintDTA.R, R/exportFunctions.R, R/datePatternTokens.R, R/sasTemporalText.R — only the functions a candidate finding depended on
- R/DTAFileTabular-class.R, R/DTAFile-class.R, R/00_helpers.R, R/importConversion.R — search hits only (the lines quoted in F-63, F-66, F-71, F-73)
- inst/qualification/fx/t — the fixture folders named in the TMPL header note; roughly 50 others not opened
- inst/extdata: clinical_data.csv (first rows and as the diff base; not all 500 rows checked by hand), clinical_dta_multiple_files.yaml and clinical_dta_with_file_dataset.yaml (the `files:` sections only), gf_dataset.yaml (the `files:` section only)
- inst/shiny/dta_app/app.R — a search for how it loads its packages, nothing else

### Not read

- inst/qualification/tests/helper-container.R; inst/qualification/docs/validation-plan.md, glossary.md; inst/qualification/README.md; risk-assessment.yaml; baseline/
- every test file of the other slices (IQ-*, OQ-APP, OQ-CONT, OQ-CORE apart from the lines above, OQ-EXPORT, OQ-FILE, ...); tests/testthat (developer suite) apart from one search in test-examples.R
- inst/extdata/gf_data_small_smirna.tsv (PQ-PERF-011's delivery: no expected verdict was derived for it)

### Limits of what the findings claim

- Findings marked `inferred` say what was not checked. The recurring ones: byte and batch layouts at each scale tier (derived from the documented 8 MiB read block and the generated row width), memory magnitudes in F-64, Windows process handling and RNG state in F-61.
- Where a case asserts "exactly one finding" for a template fixture I did not open, the fixture's content is taken on trust (TMPL header note).
- Cross-check asked for by the coordinator (traceability "verified if any case passed"): done, see F-72. The OQ-META files of this slice do not test traceability; the only case that does is OQ-CORE-011, outside the slice.

### Tally

74 findings: blocker 2 (F-17, F-27), major 31, minor 38, nit 3. By category: test-weak 53, test-cheat 13, inconsistency 4, design 2, bug 1, doc-wrong 1.
