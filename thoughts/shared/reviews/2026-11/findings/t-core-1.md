# Findings — t-core-1 (test-integrity review, non-Shiny package tests)

Reviewer key: t-core-1. Tree: `$SNAP` (68408c44). Read-only; no R executed.

Slice (35 files, tests/testthat): test-streaming-validation.R, test-rulePhrasing.R,
test-importConversion.R, test-DTADataSetMatrix.R, test-DTADataSetTabular-validation.R,
test-exportMarkdown.R, test-DTAFileSAS7BDAT.R, test-DTA.R, test-DTADataSet.R,
test-exportMarkdownDocument.R, test-streaming-parity.R, test-qualification.R,
test-documentData.R, test-DTAMatrixSpec.R, test-DTADataSetTabularFileCount.R,
test-validationReporting.R, test-metadata-contract.R, test-spaced-headers.R,
test-validation-summary.R, test-exportFunctions.R, test-writeTableToFile.R,
test-benchmark.R, test-fileNameConflicts.R, test-DTARule.R, test-container-reporting.R,
test-clinical-error-fixtures.R, test-arrowCompute.R, test-explainMessages.R,
test-DTAColumnSpecCollection.R, test-docx-unzip-member-names.R,
test-qualificationPrintSummary.R, test-writeColumnsToJson.R, test-accessor-contracts.R,
test-metadata-scaffold.R, test-asJsonSchemaLength.R

Files fully read so far: (updated at the end in "Coverage")

## Findings

### F-t-core-1-01  [minor] [test-weak]
- Where: tests/testthat/test-arrowCompute.R:19-28
- What: The test titled "dta_use_arrow_compute() only enables when capability check also passes" compares `dta_use_arrow_compute()` to the very capability function it calls, on machines where that capability is always TRUE, so the gating it names is never exercised.
- Evidence:
  ```r
  options(DTAtools.use_arrow_compute = TRUE)
  expect_identical(dta_use_arrow_compute(), dta_arrow_compute_available())
  ```
  `dta_use_arrow_compute()` is `isTRUE(getOption(...)) && dta_arrow_compute_available()` (R/arrowCompute.R:67-70). arrow is a hard Import, so on every test machine both sides are TRUE. `dta_arrow_reset_cache()`'s own docs say tests "need to simulate arrow being available/unavailable across cases" (R/arrowCompute.R:10-12), but no test ever seeds the cache with FALSE.
- Failure scenario: mutate `dta_use_arrow_compute()` to `isTRUE(getOption("DTAtools.use_arrow_compute", FALSE))` (drop the capability conjunct): both sides are TRUE, the test passes.
- Confidence: verified-by-reading
- Suggested direction: `assign("available", FALSE, envir = DTAtools:::\`__dta_arrow_cache__\`)` and assert `dta_use_arrow_compute()` is FALSE with the option TRUE.

### F-t-core-1-02  [minor] [test-weak]
- Where: tests/testthat/test-arrowCompute.R:127-156 (claim at R/arrowCompute.R:169-172)
- What: The roxygen of `set_dta_compute_threads()` promises that "the R and Arrow paths are tested to agree", but the Arrow branch of `dta_count_duplicates()` is exercised by exactly one input (integer+character key, no NA, no factor/logical).
- Evidence:
  ```r
  # R/arrowCompute.R:169-172
  #' validation verdict does not depend on either option (the R and Arrow paths
  #' are tested to agree), which is what makes Arrow compute safe to enable
  ```
  Every other `dta_count_duplicates()` test runs with the option unset and < 100000 rows, i.e. on the R path only. The repeated-NA, factor and logical cases (lines 49-91) never reach Arrow's `group_by`/`summarise`.
- Failure scenario: an arrow release that stops grouping nulls together (or a refactor that drops NA keys before grouping) changes the Arrow count for `ID = c(1L, NA, NA)`; no test runs that input through the Arrow branch, so it passes.
- Confidence: verified-by-reading (tests and code); whether arrow currently agrees on NA keys was not executed.
- Suggested direction: loop the existing R-path cases through the forced-Arrow harness (same `dta_unique_key` stop-mock), or soften the doc claim.

### F-t-core-1-03  [nit] [doc-wrong]
- Where: tests/testthat/test-arrowCompute.R:169
- What: Title says `set_dta_compute_threads()` "sets and restores" the count, but the function only sets it; the restore is the test's own `on.exit(arrow::set_cpu_count(original))`.
- Evidence: `previous <- set_dta_compute_threads(target)` then only `expect_identical(previous, original)` and `expect_identical(arrow::cpu_count(), as.integer(target))`.
- Failure scenario: n/a (title overclaims; nothing about restoring via the function is asserted).
- Confidence: verified-by-reading
- Suggested direction: either call `set_dta_compute_threads(previous)` and assert the restore, or retitle.

### F-t-core-1-04  [minor] [test-weak]
- Where: tests/testthat/test-qualificationPrintSummary.R:61-67, 20-47
- What: The title claims print() "reports success rather than a warning when every meta-check passes", but print() reports nothing at all for meta-checks in that case and the test only asserts absence; the mock also never exercises the `"file_error"` status or the `type != "success"` expectation filter that summary()/print() rely on.
- Evidence:
  ```r
  # R/qualification.R:1645, 1648-1651, 1666
  sum(rows$status %in% c("error", "file_error")), ...
  failed_meta <- sum(!x$meta_checks$ok)
  if (failed_meta > 0) { cli::cli_alert_warning(...) }
  object$expectations$type != "success", ,
  ```
  The mock's `tests$status` has no `"file_error"` and `expectations` has only one `"failure"` row.
- Failure scenario: (a) drop `"file_error"` from the errored count in print() — undetected; (b) drop the `& object$expectations$type != "success"` filter in summary() — undetected, although a real run records a success row before a failure for the same tc_id and would then print the success message.
- Confidence: verified-by-reading
- Suggested direction: add a `file_error` row and a leading `"success"` expectation row for OQ-002 to the mock; retitle the meta-check test ("prints no meta-check line when all pass").

### F-t-core-1-05  [minor] [test-weak]
- Where: tests/testthat/test-docx-unzip-member-names.R:36-50, 82-88
- What: The extraction watcher (a mock of `utils::unzip`) has no positive control, so every `expect_false(seen$extracted)` would also pass if the code under test extracted through anything other than `utils::unzip` (e.g. `zip::unzip`, already used in R/containerFiles.R:352); for the app's `template_has_placeholders()` that is the only extraction assertion.
- Evidence:
  ```r
  expect_false(app_fn("template_has_placeholders")(zp))
  expect_false(seen$extracted)
  ```
  The one "ordinary archive is still extracted" test (lines 124-133) does not use `.watch_extraction()`, so nothing proves the watcher can ever turn TRUE.
- Failure scenario: refactor the app's `template_has_placeholders()` to `zip::unzip(path, exdir = temp_dir)` and drop its member-name check: the dummy archive's empty document.xml still yields FALSE and `seen$extracted` stays FALSE — the test passes while the guard is gone.
- Confidence: verified-by-reading (test + R/exportTemplateDocx.R:784-802 + inst/shiny/dta_app/R/utils_export.R:242-254 call sites)
- Suggested direction: in the ordinary-archive test, call through `.watch_extraction()` and `expect_true(seen$extracted)`; for the app case also assert nothing was written outside a known exdir.

### F-t-core-1-06  [nit] [test-weak]
- Where: tests/testthat/test-writeColumnsToJson.R:38-50 and 95-103
- What: "writes an empty rules array when there are none" is checked with `expect_length(back$rules, 0)`, which also passes when the `rules` key is absent (NULL has length 0); line 1-6 also carries a stray duplicated comment fragment ("# than trusting file.exists().").
- Evidence:
  ```r
  back <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  expect_length(back$rules, 0)
  ```
- Failure scenario: change `drop_unset()` in `write_columns_to_json()` (R/DTAColumnSpecCollection-class.R:903-908) to also drop zero-length lists: the key disappears, the test still passes.
- Confidence: verified-by-reading
- Suggested direction: `expect_true("rules" %in% names(back)); expect_identical(back$rules, list())`; delete the orphan comment line 6.

### F-t-core-1-07  [nit] [test-weak]
- Where: tests/testthat/test-DTAColumnSpecCollection.R:11-14, 155-161, 113-121
- What: Three identical assertions on `rules[[1]]` (copy-paste; `rules[[2]]` never checked); "selects by value, not by position" checks example 2 only with `expect_s3_class(..., "S7_object")`; the columnless round-trip test asserts `!grepl("columns", yaml_text)` on a YAML it built itself, not on the app serializer it says it mirrors.
- Evidence:
  ```r
  expect_equal(class(specs@rules[[1]]), c("DTAtools::DTARuleColCondition", ...))   # x3
  expect_s3_class(create_example_DTAColumnSpecCollection(2), "S7_object")
  expect_false(grepl("columns", yaml_text, fixed = TRUE))   # yaml_text from a hand-built list
  ```
- Failure scenario: example 2 returning example 1's columns (AGE instead of AVAL) passes; the app's `dta_dataset_to_list()` starting to emit `columns: []` is not noticed here.
- Confidence: verified-by-reading
- Suggested direction: assert `names(create_example_DTAColumnSpecCollection(2))` exactly; check rules[[2]]; build the YAML via the app serializer (`app_fn("dta_dataset_to_list")`) or drop the tautological expect_false.

### F-t-core-1-08  [nit] [test-weak]
- Where: tests/testthat/test-asJsonSchemaLength.R:7-19
- What: The only fixture declares `length = 10` together with `format = "SAS $10."`, so the accessor test cannot tell the declared `length` from the format width, and the documented NULL return (no structure / no length) is never exercised.
- Evidence: `DTAColumnSpec(id = "NAME", type = "SAS Char", format = "SAS $10.", length = 10, ...)`; `@return ... or NULL when the specification declares none` (R/DTAColumnSpec-class.R:640-643).
- Failure scenario: `as_json_schema_length()` reading the `$w.` width instead of `@length` returns 10 and passes; returning `NA` instead of `NULL` for `DTAColumnSpec(id = "X")` is untested.
- Confidence: verified-by-reading
- Suggested direction: use different numbers (format `$20.`, length 10) and add `expect_null(as_json_schema_length(DTAColumnSpec(id = "X")))`.

### F-t-core-1-09  [nit] [test-weak]
- Where: tests/testthat/test-clinical-error-fixtures.R:208-212
- What: The comment and the "missing is not unconvertible" assertion point at data row 3, but SUBJ0002's missing BMI cell is data row 2; row 3 (SUBJ0003) holds no defect at all, so `expect_false(3 %in% import_errors$row)` can never fail.
- Evidence:
  ```r
  # Rows 5, 6, 9 and 18 are the unconvertible cells; row 3 (SUBJ0002's blank
  # BMI cell) is a genuinely missing value and must NOT appear here -- proving
  expect_false(3 %in% import_errors$row)
  ```
  inst/extdata/clinical_data_error_import.csv: data row 2 = `"1234-5678","V02",57,...,70.6,NA,...,"SUBJ0002"`; data row 3 = SUBJ0003, BMI 28.2. The same file's rows 5/6/9/18 (SUBJ0005/6/9/18) confirm `row` is the 1-based data row. (The sibling "all" test, line 161, correctly uses row 11 = SUBJ0011.)
- Failure scenario: none for the current code — the exact `c(5, 6, 9, 18)` on line 211 already excludes row 2 — but the separate guard the comment advertises is dead and its explanation is wrong.
- Confidence: verified-by-reading (awk over the fixture: only data row 2 has `NA` in field 8)
- Suggested direction: `expect_false(2 %in% import_errors$row)` and fix the comment ("row 2"); also "blank" is a literal `NA` token here.

### F-t-core-1-10  [nit] [test-weak]
- Where: tests/testthat/test-clinical-error-fixtures.R:36-37
- What: The `required` violation (HEIGHT is absent from clinical_data_error_columnspec.csv) is never tied to HEIGHT: its rows carry `column = NA` and `sort()` silently drops NA, so the column assertion ignores it.
- Evidence: `expect_equal(sort(unique(columnspec_full$column)), c("BMI", "GENDER", "STUDYID", "VISIT"))`; R/columnSpecChecks.R:702-708 builds the `required` rows with `column = NA_character_, columnspec = column_name`.
- Failure scenario: the presence check reporting the wrong missing column in `columnspec` (e.g. `columnspec = names(specs@columns)[1]`) still yields keyword `required` and passes this test.
- Confidence: verified-by-reading (fixture header has no HEIGHT; code lines read)
- Suggested direction: assert `unique(columnspec_full$columnspec[columnspec_full$keyword == "required"]) == "HEIGHT"`.

### F-t-core-1-11  [minor] [code-cheat]
- Where: R/DTARule-class.R:116-121 (pinned deliberately by tests/testthat/test-lintDTA.R:1011-1015; cross-slice)
- What: `DTARuleFactory()` — the entry point for every YAML/JSON rule — silently rewrites whitespace in a rule id to underscores, while the `DTARule` validator documents whitespace as an error; the author is never told the id they wrote is not the id that will appear in every report.
- Evidence:
  ```r
  id <- if (!is.null(id)) {
    gsub("[[:space:]]+", "_", as.character(id))
  } else { id }
  ```
  `DTARule` validator: `cli_abort(dta_id_problem(self@id), class = "dta_rule_invalid_id")` ("cannot have whitespaces"); lint_dta() has no rule-id whitespace check, and `@param id` of the factory says nothing about the rewrite.
- Failure scenario: a YAML rule `id: "age range"` loads as `age_range`; reports, `messages()`, `explain_messages(id=...)` lookups and template paths then use an id that appears nowhere in the specification, with no warning.
- Confidence: verified-by-reading
- Suggested direction: drop the rewrite and let the validator's `dta_rule_invalid_id` surface (or at least `cli_warn()`), and document the behaviour.

### F-t-core-1-12  [minor] [test-weak]
- Where: tests/testthat/test-fileNameConflicts.R:248-255
- What: Both nested-repeat patterns can only match strings longer than the 255-character cap (2500 and 1000 characters), so `dta_pattern_examples()` returns `character(0)` and the two content assertions run over an empty vector (`all(logical(0))` is TRUE).
- Evidence:
  ```r
  for (p in c("^((x|y){50}){50}$", "^(((x|y){10}){10}){10}$")) {
    elapsed <- system.time(ex <- dta_pattern_examples(p))[["elapsed"]]
    expect_true(all(stringr::str_detect(ex, p)))
    expect_true(all(nchar(ex) <= 255L))
  ```
  R/fileNameConflicts.R:43-46 drops every product longer than 255 and `repeat_set()` breaks on an empty set, so no example survives either pattern.
- Failure scenario: a regression in `product()`/`repeat_set()` that emits non-matching or over-long examples for nested repeats is not caught, because no nested pattern here has any example at all; only the timing assertion is live.
- Confidence: verified-by-reading (traced the 255 cap; minimum match lengths 50*50 and 10*10*10)
- Suggested direction: add a nested pattern with a feasible length (e.g. `^((x|y){3}){4}$`) and assert `length(ex) > 0` before the content checks.

### F-t-core-1-13  [minor] [test-weak]
- Where: tests/testthat/test-writeTableToFile.R:1-4, 92-216 (claim also in CHANGELOG.md:1858-1861)
- What: The header says the file pins "the crash-safety guarantee the CHANGELOG documents for both branches", but every failure-path test uses the default `compression = "none"`; the gzip branch's install step (`R.utils::gzip(temp_file, destname = filename, overwrite = overwrite)`) is never failed.
- Evidence:
  ```r
  # R/DTADataSetTabular-class.R:605-618
  if (compression == "gzip") {
    write.table(table_data, file = temp_file, ...)
    R.utils::gzip(temp_file, destname = filename, overwrite = overwrite)
  ```
  The only gzip test (lines 63-90) checks magic bytes and content of a successful write.
- Failure scenario: revert the gzip branch to `write.table(table_data, gzfile(filename), ...)` (truncating the destination before conversion) — every test in the file still passes, while the CHANGELOG still promises the old file survives.
- Confidence: verified-by-reading for test coverage; inferred for whether `R.utils::gzip()` itself writes `destname` in place or via its own temp file (installed package code not readable here).
- Suggested direction: run the non-empty-directory install-failure test with `compression = "gzip"` too, asserting the previous bytes survive.

### F-t-core-1-14  [minor] [test-cheat]
- Where: tests/testthat/test-metadata-contract.R:268-276
- What: "study metadata does not modify dataset constraints or previous validation" assigns `dta@metadata` and then compares `dta@datasets` with a copy taken before — a property assignment with no custom setter cannot change another property, so the assertion cannot fail; the "previous validation" (the `last_validation_*` attributes `check()` left on the DTA) is never looked at.
- Evidence:
  ```r
  dta <- app_fixture_dta_with_data(checked = TRUE)
  datasets <- dta@datasets
  ...
  dta@metadata <- md
  expect_identical(dta@datasets, datasets)
  ```
  R/DTA-class.R has no setter for `metadata` (only `containers` uses `new_property`).
- Failure scenario: a change that makes metadata edits drop or invalidate the DTA's validation attributes (`attr(dta, "last_validation_ok")`, results) passes this test unchanged.
- Confidence: verified-by-reading
- Suggested direction: compare `attributes()` of the validation state (e.g. `results(dta)`, `attr(dta, "last_validation_ok")`) before/after, or delete the tautology.

### F-t-core-1-15  [nit] [dead-code]
- Where: tests/testthat/test-metadata-contract.R:27-57, 116-141
- What: Two tests exist only to preserve backwards compatibility ("legacy serialization and positional API unchanged", "without tightening legacy extensions"): they pin the positional order of `DTAMetaData()`'s formals for old callers, which the owner's pre-1.0 no-compat rule makes dead weight that constrains future signature changes.
- Evidence: `old_args <- list("Legacy", "1.0", ...)`; `expect_identical(names(formals(DTAMetaData)), c(old_names, "study", "sections"))`; `positional <- do.call(DTAMetaData, old_args)`.
- Failure scenario: n/a (compat pin, not a defect detector).
- Confidence: verified-by-reading
- Suggested direction: keep the behavioural parts (empty fields omitted from `as.list()`), drop the positional-API and "legacy" framing.

### F-t-core-1-16  [nit] [doc-wrong]
- Where: R/benchmarking.R:88-92
- What: The roxygen of `dta_benchmark_end()` says its `on.exit()` reset means "a measurement that errors still leaves the nesting guard usable", but that `on.exit` only runs if `dta_benchmark_end()` is reached; an error in the measured body never reaches it — the call sites (R/DTA-class.R:1046-1055, R/streamingValidation.R:2984-2993) say exactly that and carry the real reset.
- Evidence: `on.exit(dta_benchmark_env$active <- FALSE, add = TRUE)` (benchmarking.R:108) vs DTA-class.R:1046 "The reset cannot live only inside dta_benchmark_end()".
- Failure scenario: a new caller trusting the doc omits its own `on.exit`; an abort between begin and end leaves `active = TRUE` and every later `benchmark = TRUE` silently returns no metrics.
- Confidence: verified-by-reading
- Suggested direction: reword to "callers must register their own on.exit reset; the one here only covers errors inside end()".

### F-t-core-1-17  [minor] [code-cheat]
- Where: R/validationReporting.R:1052-1105 (pinned as desired by tests/testthat/test-validationReporting.R:206-211)
- What: `dta_rule_failure_row_indices()` answers "no failing rows" (`integer(0)`) for any rule class it has no branch for, instead of refusing; this exact silent default already hid real failures once (group_condition, see the test comment at test-validationReporting.R:326-327), and the suite enshrines it.
- Evidence:
  ```r
  if (inherits(rule, "DTAtools::DTARuleGroupCondition")) { ... }
  integer(0)
  }
  ```
  test: `expect_identical(dta_rule_failure_row_indices(list(id = "not_a_rule"), data.frame(age = 1:3)), integer(0))`; comment at line 326: "Reporting no rows here made inspect() show failing_row_count = 0 for a rule that unambiguously failed, because the lookup had no branch for this class."
- Failure scenario: a fifth rule class is added; `inspect()` shows `failing_row_count = 0` for every failure of it, with no error, and the test above keeps passing.
- Confidence: verified-by-reading
- Suggested direction: end the function with `cli::cli_abort(..., class = "dta_rule_unsupported")` for a `DTARule` it does not know (keep `integer(0)` only for the documented missing-column cases) and flip the test.

### F-t-core-1-18  [minor] [dead-code]
- Where: R/DTARuleColRange-class.R:15-17, 41, 53-60 (exercised as "legacy" at tests/testthat/test-validationReporting.R:152-154; cross-slice)
- What: The `range =` constructor argument is documented as a "Legacy argument" kept beside `min`/`max`; it is silently ignored whenever `min` or `max` is also supplied, and it is still the form the roxygen example teaches.
- Evidence:
  ```r
  #' @param range ... Legacy argument for range checks; it is used only when both \code{min} and
  #'   \code{max} are \code{NULL}, and is ignored otherwise.
  if (!is.null(range) && is.null(min) && is.null(max)) {
  ```
  `as.list()` writes only `min`/`max`, so nothing the package writes ever needs `range`.
- Failure scenario: `DTARuleColRange(id = "r", columns = "AGE", range = c(18, 65), min = 0, max = 100)` silently checks 0..100.
- Confidence: verified-by-reading
- Suggested direction: remove `range` (pre-1.0, no compat owed) and update examples/tests/the corpus helper that still use it.

### F-t-core-1-19  [minor] [bug]
- Where: R/DTADataSetTabular-class.R:716-757 (cross-slice)
- What: `create_example_DTADataSetTabular()` switches on the numeric `index` positionally, the defect `create_example_DTAColumnSpecCollection()` was fixed for ("selects by value, not by position", test-DTAColumnSpecCollection.R:155): any index past the default arm returns `NULL` invisibly instead of aborting.
- Evidence:
  ```r
  switch(index,
    `1` = {...}, `2` = {...}, `3` = {...},
    cli::cli_abort("No example available for the provided index.")
  )
  ```
  A numeric EXPR selects the n-th argument, so `index = 4` hits the abort but `index = 5` (or 0) matches nothing and yields `NULL`.
- Failure scenario: `create_example_DTADataSetTabular(5)` returns `NULL`; the caller fails later with an unrelated `@` error.
- Confidence: verified-by-reading
- Suggested direction: `switch(as.character(index), ...)` plus a length-1 guard, as in the collection example.

### F-t-core-1-20  [nit] [test-weak]
- Where: tests/testthat/test-validationReporting.R:361-377
- What: The collation regression test asks for the Windows-only locale name `"German_Germany.utf8"` and skips when it cannot be set, so on Linux/macOS runners (C collation, no such locale) it always skips; it only ever runs on Windows.
- Evidence:
  ```r
  withr::local_collate("German_Germany.utf8")
  skip_if(identical(sort(c("B", "a")), c("B", "a")), "a non-C collation is not available here")
  ```
- Failure scenario: a bytewise-ordering regression that only shows under glibc/ICU collation (e.g. `de_DE.UTF-8`/`en_US.UTF-8`) is never exercised on the two POSIX CI legs.
- Confidence: inferred (CI runner locale availability not checked; the code path is read)
- Suggested direction: try a list of candidate locales (`"de_DE.UTF-8"`, `"en_US.UTF-8"`, `"German_Germany.utf8"`) and skip only if none sets.

### F-t-core-1-21  [minor] [doc-wrong]
- Where: R/documentData.R:405-407 (pinned by tests/testthat/test-documentData.R:416; cross-slice)
- What: The exported Word/Markdown column table labels `colclass` — documented as "the semantic category of the column" (`patient_info`, `visit_related`, ...) — as "R class:", which in a package that now has an `R` type backend (`R numeric`, `R character`) reads as the column's R storage class.
- Evidence:
  ```r
  if (!is.null(spec@colclass) && nzchar(spec@colclass)) {
    type_str <- paste0(type_str, "\nR class: ", spec@colclass)
  ```
  test: `expect_identical(df$Type[1], "SAS Char (8)\nFormat: $8.\nR class: visit_related")`; `@param colclass` (R/DTAColumnSpec-class.R:148-152) calls it a semantic category.
- Failure scenario: a delivered DTA document tells the supplier a `SAS Char` column has "R class: visit_related", next to columns typed `R character`.
- Confidence: verified-by-reading
- Suggested direction: label it "Category:" (or "Column class:") in documentData.R and update the pinned string.

### F-t-core-1-22  [minor] [bug]
- Where: R/DTA-class.R:316-321 and 380-384 (pinned without the sanctioned marker at tests/testthat/test-DTA.R:39-41)
- What: `datasets(dta, 1.9)` and `dta[1.5]` still truncate a fractional index silently to dataset 1 (the defect `get_table()` and `colspec()` were fixed for), and the test pins that behaviour with "Pinned, not endorsed." instead of the repo's searchable "KNOWN DEFECT, pinned rather than endorsed" marker and without naming the assertion to switch to.
- Evidence:
  ```r
  if (is.numeric(name)) {
    if (any(name < 1) || any(name > length(all_datasets))) { cli_abort(...) }
    return(all_datasets[[name]])
  ```
  test: `# ... quietly receives dataset 1. Pinned, not endorsed.` / `expect_identical(datasets(dta, 1.9)@id, "demographics")`. Contrast R/DTADataSetTabular-class.R:382 (`id != trunc(id)` refused).
- Failure scenario: `check(dta, datasets = 1.9)` / `datasets(dta, 1.9)` act on the first dataset without a word; `dta[NA_real_]` reaches `if (NA)` and raises a base "missing value where TRUE/FALSE needed" instead of a cli error.
- Confidence: verified-by-reading
- Suggested direction: reject non-whole and NA indices (as `get_table()` does) in `datasets()`, `[` and `check(DTA)`; until then mark the test with the sanctioned phrase and the `expect_error(..., "whole number")` to switch to.

### F-t-core-1-23  [nit] [doc-wrong]
- Where: tests/testthat/test-DTA.R:194-198
- What: The "Deferred (implementation gap)" comment says `DTA(metadata = DTAMetaData(title = "t"))` without `datasets` fails because "the constructor never coerces the default to list()", but the constructor now does exactly that, so the deferred assertion was never added and the comment is stale.
- Evidence: R/DTA-class.R:52-54 `if (is.null(datasets)) { datasets <- list() }`; the same file later asserts `DTA(title = "Empty")` builds (test-DTA.R:873-877).
- Failure scenario: n/a (stale note; the metadata-only call stays unasserted).
- Confidence: verified-by-reading
- Suggested direction: replace the comment with `expect_length(DTA(metadata = DTAMetaData(title = "t"))@datasets, 0)`.

### F-t-core-1-24  [minor] [test-weak]
- Where: tests/testthat/test-DTA.R:802-817 (code: R/DTA-class.R:1031; also R/streamingValidation.R:2982)
- What: `expect_error(..., class = "simpleError")` is satisfied by ANY base-R error (a typo'd object, a wrong argument), the exact weakness test-qualification.R:321-326 documents when it replaced `match.arg()` in `run_qualification()`; `check(DTA)` (and `validate_file_stream()`) still validate the user-facing `on_missing_column` with base `match.arg()`, i.e. a translated, non-cli error.
- Evidence:
  ```r
  on_missing_column <- match.arg(on_missing_column, c("scan", "stop"))
  expect_error(check(dta, on_missing_column = "maybe", quiet = TRUE), class = "simpleError")
  ```
- Failure scenario: a refactor that breaks `check(DTA)` with `object 'on_missing' not found` (also a simpleError) still passes this test.
- Confidence: verified-by-reading
- Suggested direction: validate with `rlang::arg_match()`/the package's `qual_match_arg`-style cli helper and assert the package-authored message + `rlang_error` class.

### F-t-core-1-25  [nit] [test-weak]
- Where: tests/testthat/test-DTA.R:340
- What: `expect_true(result$status %in% c("validated", "failed"))` accepts both verdicts for the bundled clean fixture, so a verdict flip of `clinical_data.csv` is not detected by the test titled "check() method validates all datasets in DTA".
- Evidence: the only verdict assertion in the test is the either-or above.
- Failure scenario: a regression that fails every row of clinical_data.csv still passes here.
- Confidence: verified-by-reading (which verdict is correct was not executed)
- Suggested direction: pin the fixture's actual, hand-derived verdict (and `n_columnspec_errors`).

### F-t-core-1-26  [blocker] [test-weak]
- Where: tests/testthat/test-qualification.R:418-422 (helpers: inst/qualification/tests/helper-generators.R:140-167, helper-00-qa.R:373-392; same comparison carries the audit claim in inst/qualification/tests/test-PQ-PERF-scale.R:48-103, cross-slice)
- What: The PR-time guard that "the generated fixture's expected result is arithmetic" compares counts keyed only by (source, target, keyword): `qa_oracle_counts()` computes each error's column and then aggregates it away, `qa_counts()` never keys on column or row, and `ignore_attr = TRUE` additionally drops the names. PQ-PERF-020 ("the injected defects are reported exactly, at every tier") and PQ-PERF-021 ("the two engines report the same errors") use exactly this count comparison as their evidence, although `qa_defect_plan()` knows every injected row position. The requirements they trace to promise more than counts: REQ-PERF-006 (risk high) "shall report exactly those defects, and the same ones, at every scale tier"; REQ-PERF-007 (risk high) "shall report identical messages for the same delivery at every scale tier" (inst/qualification/requirements/REQ-PERF.yaml:68-89) — so the traceability matrix records two high-risk requirements as verified on evidence that never checks the property they state.
- Evidence:
  ```r
  cs$column <- vapply(parts, `[[`, character(1), 1)
  cs <- stats::aggregate(list(n = cs$n), by = cs[, c("source", "target", "keyword")], FUN = sum)
  expect_equal(env$qa_counts(ds), env$qa_oracle_counts(status$table[[1]]), ignore_attr = TRUE)
  ```
- Failure scenario: the engine reporting SUBJID's maxLength error under column SEX, or — the scale-specific defect PQ exists to catch — a batch row-offset error that shifts every reported row at 10M rows, leaves every count unchanged; the PR guard and both PQ test cases still record PASS as audit evidence of "exact" reporting.
- Confidence: verified-by-reading (test-PQ-PERF-scale.R:48-103 and both helpers read)
- Suggested direction: key the oracle by column and by the injected row positions from `qa_defect_plan()` (compare `qa_messages_norm()` rows, not counts), and drop `ignore_attr`.

### F-t-core-1-27  [nit] [doc-wrong]
- Where: tests/testthat/test-streaming-parity.R:1-4
- What: The header is copied from test-validationReporting.R ("tests how the engine copes with rules that contradict their columns (stale, restored or legacy specs)"), but this is the randomised parity suite; the shadowing is really needed because its central case, a uniqueness rule on the undeclared column XKEY, is a specification the class refuses (`rule_column_undeclared` is an error, R/lintDTA.R:297-304) — so the motivating divergence is only reachable through objects built behind the validator.
- Evidence: `DTAColumnSpecCollection <- unchecked_collection`; `DTARuleColUnique(id = "xkey_unique", columns = "XKEY")` with ID/AGE declared.
- Failure scenario: a reader of the file is told it tests legacy specs and misses that half its rule draws are publicly unconstructible.
- Confidence: verified-by-reading
- Suggested direction: rewrite the header to say why the bypass is needed (undeclared XKEY), or declare XKEY untyped so the case is a real, constructible one.

### F-t-core-1-28  [nit] [test-weak]
- Where: tests/testthat/test-exportMarkdownDocument.R:506-513, 618-620, 728-730; tests/testthat/test-DTAFileSAS7BDAT.R:841-842
- What: The rendered-HTML assertions of the rules test sit in a bare `if (rmarkdown::pandoc_available()) { ... }`, so they vanish without any skip being reported, and three tests turn a missing bundled asset into a skip (`skip_if(!nzchar(yaml_path), "gf_dataset.yaml missing")`, `skip_if(!nzchar(csv), "bundled clinical_data.csv not found")`) although the suite elsewhere — the same SAS file's own `sas_fx()` included — treats a missing bundled fixture as a failure (test-DTAFileSAS7BDAT.R:18-19, test-clinical-error-fixtures.R:4-6, helper-shinyapp.R:124).
- Evidence: `if (rmarkdown::pandoc_available()) { html <- .mdd_html(path); expect_length(xml2::xml_find_all(html, "//code|//pre"), 0L) ... }`
- Failure scenario: an install whose tarball lost inst/extdata/gf_dataset.yaml (cf. the `.Rbuildignore` incident) reports these as skips, not failures; on a pandoc-less runner the "no <code>/<pre> in the rendered output" check silently never runs.
- Confidence: verified-by-reading
- Suggested direction: `skip_if_not(pandoc_available())` in a separate test for the HTML half; `expect_true(nzchar(yaml_path))` instead of skipping.

<!-- progress marker (superseded: all 35 files are now read, see Coverage at the end). At this point (28/35): asJsonSchemaLength, metadata-scaffold, accessor-contracts,
writeColumnsToJson, qualificationPrintSummary, docx-unzip-member-names, DTAColumnSpecCollection, explainMessages,
arrowCompute, clinical-error-fixtures, container-reporting, DTARule, fileNameConflicts, benchmark, writeTableToFile,
exportFunctions, validation-summary, spaced-headers, metadata-contract, validationReporting,
DTADataSetTabularFileCount, DTAMatrixSpec, documentData, qualification, streaming-parity, exportMarkdownDocument,
DTADataSet, DTA. Remaining: DTAFileSAS7BDAT, exportMarkdown, DTADataSetTabular-validation, DTADataSetMatrix,
importConversion, rulePhrasing, streaming-validation. -->

### F-t-core-1-29  [nit] [dead-code]
- Where: tests/testthat/test-exportMarkdown.R:659-663
- What: The byte expectation is assigned twice; the first assignment is dead (immediately overwritten) and is also wrong — `charToRaw("caf\u00c3\u00a9...")` is the UTF-8 encoding of "cafÃ©", not of "café".
- Evidence:
  ```r
  expected <- charToRaw("caf\u00c3\u00a9\nx\n\n\nend \u00c2\u00a0\n")
  expected <- c(charToRaw("caf"), as.raw(c(0xc3, 0xa9)), ...)
  ```
- Failure scenario: none at run time; a reader trusting the first line is misled about the expected bytes.
- Confidence: verified-by-reading
- Suggested direction: delete line 659.

### F-t-core-1-30  [nit] [test-weak]
- Where: tests/testthat/test-DTADataSetTabular-validation.R:1-4 (same pattern: test-validationReporting.R:1-4, test-streaming-parity.R:1-4)
- What: The rule-unchecked constructor is swapped in for the WHOLE file although only one test here (the text-column range rule at lines 646-660) needs a specification the class refuses; every other fixture in the file now silently skips the rule/column consistency check, so an accidentally inconsistent fixture is accepted instead of failing loudly.
- Evidence: `DTAColumnSpecCollection <- unchecked_collection` at file level; tv_id_specs(), tv_stamp_specs(), the max_errors and unspecified-dataset fixtures are all built through it.
- Failure scenario: a fixture whose rule names a misspelt column (e.g. `DTARuleColUnique(id = "uid", columns = "Id")` in `tv_stamp_specs()`) is no longer refused at construction; whether a later assertion happens to notice depends on each test, so the mistake can surface far from its cause or not at all.
- Confidence: verified-by-reading (what the engine reports for a rule on an absent column was not traced per test)
- Suggested direction: call `vc_specs_unchecked()`/`unchecked_collection()` only in the tests that need a refused specification.

### F-t-core-1-31  [minor] [test-weak]
- Where: tests/testthat/test-DTADataSetMatrix.R:546-553
- What: "a composite row key is tuple-unique" uses data in which the repeated TUPLE (a, x) also makes the first key column repeat, so the single generated rule fails whether the key is the tuple or just `ID`; tuple semantics are never distinguished.
- Evidence:
  ```r
  # (a, x) repeats; (a, y) and (b, x) are distinct although each id repeats.
  path <- mxd_file("ID\tDESC\tS1\na\tx\t1\na\ty\t2\nb\tx\t3\na\tx\t4\n")
  expect_identical(validation_status(d)$n_rule_errors, 1L, info = stream)
  ```
  `n_rule_errors` counts failing rules (one generated rule, R/DTADataSetMatrix-class.R:657-663), not duplicate rows.
- Failure scenario: `dta_matrix_row_key()` keeping only the first key column (`key[[1]]`) still yields one failing `.matrix:row_key` rule and passes.
- Confidence: verified-by-reading
- Suggested direction: add a file where each column repeats but no tuple does (`a x / a y / b x`) and expect 0 rule errors.

### F-t-core-1-32  [minor] [test-weak]
- Where: tests/testthat/test-DTADataSetMatrix.R:518-524 and 1117-1130
- What: "an untypable cell is an import finding on the cell's column" (and "a fraction or text is an import finding") assert only the import COUNT; neither the column nor the row of the import finding is checked, although the titles promise it.
- Evidence: `expect_identical(validation_status(d)$n_import_errors, 1L, info = stream)` is the whole body.
- Failure scenario: the matrix reader attributing an import failure to the wrong data column (e.g. the reading-name offset by the k row labels) passes both tests.
- Confidence: verified-by-reading
- Suggested direction: read `messages(d)` with `source == "import"` and assert `column == "S2"`, `row == 1`.

### F-t-core-1-33  [minor] [test-weak]
- Where: tests/testthat/test-DTADataSetMatrix.R:924-945
- What: "printing a tabular dataset is unchanged by the shared print body" first compares `print(d)` with `dta_print_tabular(d)` — the method IS that one call (R/DTADataSetTabular-class.R:763), a tautology — and the only meaningful comparison, against the pre-refactoring method read from `git show f6189bf:...`, skips wherever that commit is absent, which includes every CI leg: the workflow's `actions/checkout` sets no `fetch-depth` (R-CMD-check.yaml:113), so the clone is shallow and holds only the head commit.
- Evidence:
  ```r
  expect_identical(capture.output(print(d), type = "message"), capture.output(dta_print_tabular(d), type = "message"))
  base <- tryCatch(suppressWarnings(system2("git", c("show", "f6189bf:R/DTADataSetTabular-class.R"), ...)), ...)
  skip_if(length(base) == 0L, "the base commit is not available")
  ```
- Failure scenario: a change to `dta_print_tabular()` that alters every printed line passes on every CI leg (first assertion compares the function with itself, second skips).
- Confidence: verified-by-reading for the shallow CI checkout and the tautology; inferred for other environments (whether a given dev clone or an `R CMD check` working directory can resolve f6189bf was not checked)
- Suggested direction: pin the expected lines by hand (as test-DTADataSetMatrix.R:907-922 does for the matrix) and drop the git-history evaluation.

### F-t-core-1-34  [minor] [dead-code]
- Where: R/containerAssignments.R:112-200 (`dta_is_stale()`, `dta_is_old_matrix()`, `dta_upgrade()`), called from every entry point (e.g. R/DTA-class.R:1033-1034, R/fileNameConflicts.R:488); pinned by tests/testthat/test-DTADataSetMatrix.R:947-970
- What: An upgrade layer exists solely to re-stamp objects saved by older package versions ("A restored 0.27.0 object always qualifies"), plus an abort for the retired `corner_mode` matrix format — compatibility machinery the owner's pre-1.0 rule makes dead weight, run on every `check()`/`load_file()`.
- Evidence:
  ```r
  # An object restored from an older version lacks properties its class now has.
  if (dta_is_stale(x)) x <- dta_upgrade(x)
  ```
  and the comment at containerAssignments.R:117-120 admitting a validator-only change is invisible to it.
- Failure scenario: n/a (compat leftover); its own comment notes the mechanism silently keeps an old validator on restored objects when a change adds no property.
- Confidence: verified-by-reading (call sites sampled, not all enumerated)
- Suggested direction: remove the upgrade path and its test; a restored object from another version can simply be refused.

### F-t-core-1-35  [minor] [test-weak]
- Where: tests/testthat/test-importConversion.R:743-785 and 788-815
- What: "a float in a declared Int column is an import error, not a read abort" never asserts that an import error was recorded, and its own comments contradict each other (lines 749-752 say the fraction "is left as a double and reported as a *schema* error, not an import error"; lines 780-781 say the opposite); the streaming twin asserts only `ok == FALSE`, which the non-nullable `DOSE` column's column-spec `type` failure produces on its own.
- Evidence:
  ```r
  # dta_coerce_column() intentionally does NOT flag 0.01 as an import error --
  # a fractional value in an Int column is left as a double and reported as a
  # *schema* (column spec) error, not an import error.
  ...
  expect_type(typed$DOSE, "integer")
  expect_true(is.na(typed$DOSE[[101]]))
  ```
  streaming: `DTAColumnSpec(id = "DOSE", type = "SAS Int", nullable = FALSE)` ... `expect_false(details$ok)`.
- Failure scenario: the reader turning `0.01` into NA without recording the `not_whole` issue (silent data loss on the import axis) passes both tests: the eager one checks only the NA, the streaming one fails anyway on the column-spec axis.
- Confidence: verified-by-reading
- Suggested direction: assert `ds@import_issues[["float_in_int"]]` has one row (row 101, raw "0.01", reason "not_whole") and `details$n_import_errors == 1`; delete the stale comment.

### F-t-core-1-36  [minor] [code-cheat]
- Where: R/DTARuleGroupCondition-class.R:187 and R/evaluateRules.R:1695, 244-249 (pinned by tests/testthat/test-rulePhrasing.R:1078-1079, 1089-1090, 1098, 1138-1140, 1201)
- What: Two rule defects that the phrasing module itself detects and words as "Problem: ... the validation stops with an error" — a constraint `message` that is not a single text, and a non-date text bound on a date column — are accepted by the constructor (and not refused by lint), and then abort validation with a raw, translated base-R error (`vapply()` length / `Ops.Date` format) instead of a classed `cli_abort()`; the tests can only assert `class = "simpleError"`, which any base error (a typo, `object not found`) also satisfies.
- Evidence:
  ```r
  msg <- cst$message %||% NULL                      # DTARuleGroupCondition-class.R:187, never type-checked
  details <- vapply(violations, function(v) v$message, character(1))   # evaluateRules.R:1695
  if (inherits(x, "Date") || inherits(x, "POSIXt")) { return(list(x = x, value = value)) }  # raw text bound reaches Ops.Date
  expect_error(rule_check_group_condition(vec, df), class = "simpleError")
  ```
- Failure scenario: a YAML group rule with `message: [a, b]` builds, lints clean and then kills `check(dta)` for every dataset with "values must be length 1" in the session language; a regression replacing that crash with an unrelated `simpleError` still passes the tests.
- Confidence: verified-by-reading (constructor, evaluator lines and tests read; lint checked for `message` handling by search only)
- Suggested direction: refuse a non-scalar `message` and a non-number/non-date bound in the constructor/lint (the phrasing module already has the predicate), and assert a package class.

### F-t-core-1-37  [minor] [dead-code]
- Where: R/DTARuleGroupCondition-class.R:180-184, 260; inst/shiny/dta_app/app.R:6819-6820; vignettes/DTAtools.Rmd:2918-2919 (cross-slice)
- What: The constraint-type aliases `not_both` and `implies` are kept, by the vignette's own account, "For compatibility with earlier drafts" — a compat leftover under the owner's pre-1.0 rule, duplicated in the app's normaliser and enshrined as a qualification requirement (REQ-RULE.yaml:260-261).
- Evidence: `ctype <- switch(as.character(ctype), not_both = "mutually_exclusive", implies = "requires", as.character(ctype))`; vignette: "For compatibility with earlier drafts, `not_both` is accepted as an alias".
- Failure scenario: n/a (two spellings for one thing in documents, tests, app and requirements).
- Confidence: verified-by-reading
- Suggested direction: drop the aliases and their docs/requirement.

### F-t-core-1-38  [nit] [test-weak]
- Where: tests/testthat/test-rulePhrasing.R:1415-1444
- What: The mutation test swallows every rebuild error (`tryCatch(rebuild(...), error = function(e) NULL)` then `next`) and never counts how many mutations it actually exercised, so if `rebuild()`/`DTARuleFactory()` stopped accepting a rule class's `as.list()` shape, every mutation of that class would be skipped and the test would still pass.
- Evidence: `changed <- tryCatch(rebuild(mutate_leaf(lst, leaf$path, fn)), error = function(e) NULL); if (is.null(changed)) next`
- Failure scenario: `as.list(DTARuleGroupCondition)` gaining a field the factory refuses ("Unknown argument") makes all group-rule mutations no-ops; the wording could then ignore any group leaf undetected.
- Confidence: inferred (how many mutations currently fail to rebuild was not executed)
- Suggested direction: count successful rebuilds per rule type and `expect_gt()` a floor.

### F-t-core-1-39  [nit] [doc-wrong]
- Where: tests/testthat/test-streaming-validation.R:2897-2901
- What: The comment justifying base `options()` says "the package does not depend on withr, and `R CMD check` fails a `::` call to a package DESCRIPTION does not declare" — but withr IS declared (DESCRIPTION `Suggests: withr`) and the same file calls `withr::local_options()`/`withr::with_options()` (lines 916, 3170).
- Evidence: DESCRIPTION Suggests lists `withr`; `withr::local_options(DTAtools.stream_arrow_unique = FALSE)` at line 916.
- Failure scenario: n/a (stale reasoning that misleads the next editor).
- Confidence: verified-by-reading
- Suggested direction: drop the comment (or use `withr::local_options()` like the rest of the file).

## Tally

39 findings. Severity: 1 major, 21 minor, 17 nit (no blocker).
Category: 22 test-weak, 1 test-cheat, 3 code-cheat, 6 doc-wrong, 5 dead-code, 2 bug.
KNOWN-DEFECT pins checked: test-DTADataSetTabular-validation.R:946-996 (tzone = "") is marked and names its
switch; test-DTAFileSAS7BDAT.R:862-903 (LIM-011/012) reference registered limitations and name their switch;
test-DTA.R:39-41 pins a still-real defect WITHOUT the marker (F-22).

## Coverage

Fully read (35/35 slice files): test-asJsonSchemaLength.R, test-metadata-scaffold.R, test-accessor-contracts.R,
test-writeColumnsToJson.R, test-qualificationPrintSummary.R, test-docx-unzip-member-names.R,
test-DTAColumnSpecCollection.R, test-explainMessages.R, test-arrowCompute.R, test-clinical-error-fixtures.R,
test-container-reporting.R, test-DTARule.R, test-fileNameConflicts.R, test-benchmark.R, test-writeTableToFile.R,
test-exportFunctions.R, test-validation-summary.R, test-spaced-headers.R, test-metadata-contract.R,
test-validationReporting.R, test-DTADataSetTabularFileCount.R, test-DTAMatrixSpec.R, test-documentData.R,
test-qualification.R, test-streaming-parity.R, test-exportMarkdownDocument.R, test-DTADataSet.R, test-DTA.R,
test-DTAFileSAS7BDAT.R, test-exportMarkdown.R, test-DTADataSetTabular-validation.R, test-DTADataSetMatrix.R,
test-importConversion.R, test-rulePhrasing.R, test-streaming-validation.R.

Helpers fully read: helper-validation-corpus.R, helper-unchecked-collection.R, helper-source-tree.R,
helper-containers.R, helper-docx.R. Only skimmed: helper-shinyapp.R (app_fn and the DTA fixture helpers).
Fixture data opened where a test's claim depended on it: inst/extdata/clinical_data_error_*.csv (awk over the
numeric fields), clinical_dta.yaml (column list). Not opened: tests/testthat/fixtures/matrix/*,
tests/testthat/fixtures/write_dta-furniture.txt, tests/testthat/_snaps/validation-oracle.md (belongs to
test-validation-oracle.R, outside this slice).

R/ code read in part to verify claims (not reviewed as a slice): arrowCompute.R (all), benchmarking.R (all),
fileNameConflicts.R (all), DTARule-class.R (all), DTARuleColRange-class.R (all), DTAFileSAS7BDAT-class.R (all),
DTAColumnSpec-class.R:1-780, DTAColumnSpecCollection-class.R:1-915, DTA-class.R:1-400 and 1000-1080,
DTADataSetTabular-class.R:330-760, validationReporting.R:1000-1160, qualification.R:420-500 and 1600-1680,
exportTemplateDocx.R:770-850, streamingValidation.R:610-670, evaluateRules.R:225-285 and 915-960,
containerAssignments.R:95-200, DTARuleGroupCondition-class.R:90-220, documentData.R:360-420,
lintDTA.R:255-315; inst/qualification/tests/helper-generators.R (all), helper-00-qa.R:340-392,
test-PQ-PERF-scale.R:1-110; .github/workflows/R-CMD-check.yaml:100-168; DESCRIPTION.
