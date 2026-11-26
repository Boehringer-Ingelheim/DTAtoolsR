# Findings — t-core-3 (package tests, non-Shiny)

Reviewer key: t-core-3. Tree: `$SNAP` = scratchpad/snapshot (68408c44).
Slice: the 35 files in `slices/t-core-3.txt` (tests/testthat/test-evaluateRules.R,
test-loadDelivery.R, test-DTAFile.R, test-templateGuide-collect.R,
test-exportTemplateDocx-blocks.R, test-exportTemplateDocx.R, test-DTADataSetFile.R,
test-templateGuide.R, test-DTAMetaData.R, test-importAxis.R, test-rTemporalFormat.R,
test-DTAColumnSpecStructureR.R, test-logical-r.R, test-lintDTA-matrix.R,
test-DTAFileXPT.R, test-matrix-reading.R, test-export-matrix.R,
test-columnspec-format.R, test-template-matrix.R, test-DTAColumnSpecCell.R,
test-bundled-templates.R, test-lintTemplateRepo.R, test-DTAContainer.R,
test-DTAFile-container.R, test-docxLayout.R, test-matrix-review-fixes.R,
test-generic-guards.R, test-run-dta-app.R, test-upgrade.R,
test-datePatternTokens.R, test-local-template-library.R, test-DTAFileAny.R,
test-provenance.R, test-dataset-runtime-keys.R, test-source-portability.R)
plus all tests/testthat/helper-*.R.

Files fully read so far: (updated as the review proceeds — see Coverage at end)

## Helpers, generic-guards, source-portability, provenance, dataset-runtime-keys, templateGuide-collect, docxLayout

### F-t-core-3-01  [minor] [doc-wrong]
- Where: [assistant-tool].md:24-28; .github/workflows/R-CMD-check.yaml:97-107; tests/testthat/test-generic-guards.R:180-197
- What: Three places claim that setting NOT_CRAN under `R CMD check` is what makes the two load-failure subprocess tests in test-generic-guards.R run, but both tests also call `skip_if_no_source_tree()`, which always skips under `R CMD check`, so NOT_CRAN is irrelevant to them there.
- Evidence: test-generic-guards.R:27-33 `skip_on_cran()` ... `skip_if_no_source_tree("not running from a package source tree (R CMD check and covr run from the install)")`; R-CMD-check.yaml:152-153 itself says "The source-tree tests skip themselves under R CMD check (the tarball has no tests/ tree to find)". The tests only run in the separate step at R-CMD-check.yaml:157-162, which sets `NOT_CRAN: "true"` at step level anyway. [assistant-tool].md:24-27 nevertheless says "without it every skip_on_cran() test skips — including the two in test-generic-guards.R that are the only way to observe a package-load failure". The yaml comment also counts "Six tests ... the two subprocess tests ... and three that run the qualification runner" (2+3=5), while the suite holds 7 `skip_on_cran()` in test-qualification.R alone, plus test-streaming-validation.R:2967, test-shinyapp-export.R:565/1561, test-shinyapp-modal-reattach.R:83 and a file-level one in test-shinyapp-modal-browser.R:31.
- Failure scenario: A maintainer who reads [assistant-tool].md believes the full check covers package-load regressions. It does not: deleting the "Run source-tree tests" step (or dropping `generic-guards` from its filter) silences both tests with every check still green.
- Confidence: verified-by-reading (the helper, both tests and the workflow). Inferred: the claim that `DTAtools.Rcheck/tests/testthat/../../R` does not exist under R CMD check. The workflow comment at :152 states the same.
- Suggested direction: Say plainly that these two tests run only in the source-tree step, and fix the stale "six"/"three" counts.

### F-t-core-3-02  [nit] [doc-wrong]
- Where: tests/testthat/helper-source-tree.R:17-21; tests/testthat/test-generic-guards.R:198-201
- What: Both comments give the CI filter as `generic-guards|source-portability|templateGuide-collect`, but the workflow runs `...|templateGuide-collect|rulePhrasing`, and test-rulePhrasing.R:1717 is a fourth caller of `skip_if_no_source_tree()`.
- Evidence: R-CMD-check.yaml:162 `devtools::test(filter='generic-guards|source-portability|templateGuide-collect|rulePhrasing', ...)`.
- Failure scenario: Someone who adds a source-tree test keeps the comment's list current and drops `rulePhrasing` from the workflow, or never adds the new file. The comment is the documented contract, and it is already out of date.
- Confidence: verified-by-reading
- Suggested direction: Name the filter in one place (the workflow) and point to it from the comments.

### F-t-core-3-03  [minor] [test-weak]
- Where: tests/testthat/test-generic-guards.R:180-220
- What: The test is titled "the skip_on_cran() tests above are not silently skipped on CI", but it only checks the NOT_CRAN spelling. It cannot detect the subprocess tests being skipped through their other gate (`skip_if_no_source_tree`), which is the gate that actually skips them in CI's R CMD check. Its off-CI branch is close to a tautology.
- Evidence: Only `if (identical(Sys.getenv("CI"), "true")) expect_identical(Sys.getenv("NOT_CRAN"), "true", ...)` and `else expect_true(Sys.getenv("NOT_CRAN") %in% c("", "true"))`. The DTATOOLS_REQUIRE_SOURCE_TREE branch fires only inside the step that already sets it.
- Failure scenario: Mutation: remove the "Run source-tree tests (must not skip)" step from R-CMD-check.yaml. Both subprocess tests then never run anywhere in CI, and this guard still passes because NOT_CRAN is still set at job level.
- Confidence: verified-by-reading
- Suggested direction: Retitle it to what it checks, and accept that the step itself is the only guarantee, or check it from a workflow-lint step.

### F-t-core-3-04  [minor] [dead-code]
- Where: R/DTADataSet-class.R:108-127; tests/testthat/test-provenance.R:43-47
- What: `dta_provenance()` / `dta_set_provenance()` carry a backwards-compatibility branch for objects "restored from before the property existed", and a test pins it. The owner wants no compat before 1.0.0. The branch is also implemented as a blanket error swallow.
- Evidence: `dta_provenance <- function(ds) { tryCatch(ds@provenance, error = function(e) list()) }` and `if (!"provenance" %in% S7::prop_names(ds)) { return(ds) }`. The test builds an `Old` class to exercise it: "An object without the slot comes back unchanged."
- Failure scenario: Any error reading `@provenance` (wrong object type, NULL, a non-S7 list) silently becomes "no records" ("unrecorded"), instead of an error that names the wrong argument.
- Confidence: verified-by-reading
- Suggested direction: Drop the compat branch and its test, and let `ds@provenance` error.

### F-t-core-3-05  [nit] [test-weak]
- Where: tests/testthat/test-provenance.R:56-60
- What: `expect_error()` has no `class` or `regexp`, against the repo rule, so any error at all satisfies the test "other errors propagate".
- Evidence: `expect_error(dta_set_provenance(ds, character(0), list(file = "f", ...)))`
- Failure scenario: Mutation: an unrelated bug that makes `dta_set_provenance()` error early (for example a typo in `S7::prop_names(ds)`) still passes.
- Confidence: verified-by-reading
- Suggested direction: Assert the error comes from the subscript, for example with `class = "error"` plus a check that `S7::prop_names` was reached, or test the propagation through a mocked failing setter.

### F-t-core-3-06  [minor] [test-weak]
- Where: tests/testthat/test-templateGuide-collect.R:1253-1270 (and R/templateGuide-collect.R:1404-1436, 1455-1469)
- What: The test is titled "a study option's default reaches both sides of the guide's comparison", but it only asserts that no column is marked "Changed". That is also the result when the dataset-template side is never built, because then no comparison happens at all.
- Evidence: Test: `expect_identical(cols$Change, c("", ""))`, with no `source_version` or `source` check. The sibling test at :1245 does check `ds$source_version == "1.0"`. Code: `.tgc_build_dataset_template()` returns NULL on any error (`tryCatch(dta_dataset_from_list(plain), error = function(e) NULL)`). In `.tgc_built_datasets()`, a NULL `tpl` leaves `cmark <- rep("", ...)`.
- Failure scenario: Mutation: break `resolve_dataset_tokens()` or `dta_dataset_from_list()` for a token_env that carries a default, so the template side errors, `tpl` is NULL, every mark is "" and the test passes. The test also never checks that the default value (`1001_0190`) appears in either side's "Allowed values".
- Confidence: verified-by-reading
- Suggested direction: Also assert `ds$source_version == "1.0"`, and that STUDYID's "Allowed values" equals the default.

### F-t-core-3-07  [minor] [code-cheat] (cross-slice, R/)
- Where: R/templateGuide-collect.R:787-793, 1433, 1435, 1436
- What: The guide's dataset-template comparison swallows every error and falls back to "not compared". The guide then prints a dataset with no change marks, which is the same rendering as "identical to its template", and tells the reader nothing.
- Evidence: `plain <- tryCatch(g("resolve_dataset_tokens")(plain, env), error = function(e) plain)`; `tryCatch(dta_dataset_from_list(plain), error = function(e) NULL)`; `overrides <- tryCatch(..., error = function(e) list())`.
- Failure scenario: Given a dataset template that no longer builds, or a token resolver that throws, the guide shows the creation template's dataset with every column unmarked. A reviewer reads that as "unchanged from the dataset template".
- Confidence: verified-by-reading
- Suggested direction: Surface a failed comparison as a problem row or a "could not compare" note, not as an unmarked table.

### F-t-core-3-08  [minor] [test-weak]
- Where: tests/testthat/test-docxLayout.R:88-93
- What: The test is titled "export_with_template keeps its tables at their built widths", but it never calls `export_with_template()` or any template block. It only checks that `.build_column_specs_table(width = NULL)` differs from the default.
- Evidence: `natural <- sum(.build_column_specs_table(specs, width = NULL)$body$colwidths)`; `expect_equal(sum(.build_column_specs_table(specs)$body$colwidths), TEXT_WIDTH$landscape)`. The `width = NULL` callers are in R/exportTemplateBlocks.R:355, 396, 438, 471, 476, 553.
- Failure scenario: Mutation: drop `width = NULL` at R/exportTemplateBlocks.R:396 or :476, so template exports get stretched to 9.7in. This test still passes. (Whether test-exportTemplateDocx-blocks.R catches it is checked below.)
- Confidence: verified-by-reading
- Suggested direction: Export through `export_with_template()` and read the table widths back from the produced document.

### F-t-core-3-09  [nit] [test-weak]
- Where: tests/testthat/test-docxLayout.R:168-201; tests/testthat/fixtures/write_dta-furniture.txt
- What: The golden fixture is serialized XML taken from `write_dta()`'s own output ("captured before the furniture builder was generalised"). It cannot catch a defect that existed when it was captured, and its re-capture instructions ("Regenerate only for a deliberate layout change") mean pasting new observed output. It also pins officer's serialization of every child element (rFonts, `w:dirty`, `w:firstLineChars`), so an officer upgrade can fail it with no product change.
- Evidence: Comment at :169-174; the fixture lines are verbatim officer XML.
- Failure scenario: An officer release that reorders run attributes fails the test spuriously. A wrong but stable layout passes.
- Confidence: verified-by-reading
- Suggested direction: Keep the independent content assertions (lines 25-64). If the golden stays, compare parsed structure (texts, tab positions, field codes), not the raw serialization.

### F-t-core-3-10  [nit] [test-weak]
- Where: tests/testthat/test-templateGuide-collect.R:882 and :1035-1041
- What: Two titles overclaim. "every answer-type branch mirrors the app's input" checks the guide against hand-written labels only, and never compares it with the app's widget mapping. "the collector aborts with cli conditions, never stop()" is a text grep for `\bstop\(` that misses `rlang::abort()`, `warning()` and `stopifnot()`.
- Evidence: `expect_false(any(grepl("\\bstop\\(", code)))`
- Failure scenario: Mutation: the app maps `type: select` with no choices to a select widget, and the guide still says "Free text". The test passes.
- Confidence: verified-by-reading
- Suggested direction: Retitle, or source the app's mapping in the test.

## test-evaluateRules.R

### F-t-core-3-11  [minor] [test-weak]
- Where: tests/testthat/test-evaluateRules.R:317-354
- What: "apply_rules handles canonical and legacy rule types": its "legacy" half sets each rule's `@type` to the canonical value the constructor already set. `apply_rules()` has no legacy type path, so the second half re-runs the first and the title claims coverage of a path that does not exist.
- Evidence: `legacy_rule@type <- "col_range"` (the constructor sets `type = "col_range"`, R/DTARuleColRange-class.R:66); `grouped_legacy@type <- "group_condition"` (R/DTARuleGroupCondition-class.R:109). The dispatch table in R/evaluateRules.R:1740-1745 holds only `col_range`, `col_unique`, `col_condition`, `group_condition`.
- Failure scenario: No mutation is caught by lines 336-353 that lines 320-348 do not already catch. The test reads as legacy coverage while testing nothing extra.
- Confidence: verified-by-reading
- Suggested direction: Delete the "legacy" half and drop "legacy" from the title.

### F-t-core-3-12  [minor] [test-weak]
- Where: tests/testthat/test-evaluateRules.R:1551-1572 (guard under test: R/evaluateRules.R:225)
- What: The "(FIX 1)" regression test for "a cache built for a larger frame is not recycled against a subset" uses a fixture on which the stale cache and a fresh conversion give the same message. Removing the length guard therefore goes unnoticed.
- Evidence: `df_subset <- df_full[1:3, ]`, i.e. AGE "20","70","ninety" from "20","70","ninety","40","55". With the stale cache, the mask over 5 values is c(F,T,T,F,F), sum 2. Fresh, it is c(F,T,T), sum 2. `rule_check_range()` reports only the count (`dta_range_violation_message(rule@id, sum(violated), ...)`), so both produce "violated: 2 rows ...".
- Failure scenario: Mutation: delete `&& length(cached$values) == nrow(df)` at R/evaluateRules.R:225. The stale cache is reused, and `with_stale_cache` still equals `without_cache`, so the test passes.
- Confidence: verified-by-reading
- Suggested direction: Pick a subset where the two counts differ (for example rows 3:5, or rows 4:5, where the stale mask counts 2 and the truth is 0).

### F-t-core-3-13  [minor] [test-weak]
- Where: tests/testthat/test-evaluateRules.R:1600-1637, 1639-1692 (code: R/evaluateRules.R:1463-1470)
- What: Both tests claim group order is "sorted", but neither can detect a change in ordering. (a) In the first test the ordering assertion is a tautology, and its comment contradicts its own assertion. (b) In the second, the data already lists the violating groups in ascending order, so first-appearance order equals sorted order. Nothing in this file pins the radix (C-locale) order the code comment insists on.
- Evidence: (a) `RESULT = rep(c(NA, NA), n_groups)` and `df$RESULT[df$SUBJ == last] <- NA` is a no-op. The comment says "so the rest violate", yet the test asserts `expect_true(res$valid)`. Then only S150 violates, so `regexpr("SUBJ=S150", m) < regexpr("SUBJ=S299", m) || !grepl("SUBJ=S299", m)` is always TRUE. (b) violators "S0007","S0500","S0999" appear in that order in rows sorted by SUBJ.
- Failure scenario: Mutation: `kf <- factor(split_key, levels = unique(split_key))` (first-appearance order) at R/evaluateRules.R:1470, or a switch to locale `sort()`. Both tests still pass. Elsewhere, test-streaming-validation.R:2175-2262 compares the two paths only differentially (inferred: a coordinated change to both paths passes there too).
- Confidence: verified-by-reading (both tests and the code). Inferred for the cross-file remark.
- Suggested direction: Use groups that appear out of order and whose case differs ("b", "B", "a"), and assert the exact radix order of `details$group`.

### F-t-core-3-14  [minor] [test-weak]
- Where: tests/testthat/test-evaluateRules.R:1786-1810
- What: The test is titled "group_condition mutually_exclusive/requires 'all' scope on a multi-row group". It contains no `requires` constraint, and its `mutually_exclusive` fixture gives the same verdict under "any" and under "all", so it cannot tell the scopes apart.
- Evidence: Every row is STATUS "FAILED" and the rule sets `left_scope = "all", right_scope = "any"`, so left is TRUE under either scope. The only assertion is `expect_false(res$valid)`.
- Failure scenario: Mutation: ignore `constraint$left_scope` (always "any") at R/evaluateRules.R:1590. The test passes.
- Confidence: verified-by-reading
- Suggested direction: Add a non-FAILED row so "all" and "any" differ, assert both outcomes, and either add the `requires` case or drop it from the title.

### F-t-core-3-15  [nit] [test-weak]
- Where: tests/testthat/test-evaluateRules.R:1739-1764, 1766-1784, 615-636
- What: Three comments or titles claim more than the assertions check. (1) "Exactly ten row numbers shown ... for each side": `shown` uses the regex `[0-9,]+`, which stops at the first ", ", so only "two sides" is checked. (2) The title "scope 'all' is false for an empty condition set, not vacuously true" names the `n_seen > 0` guard (R/evaluateRules.R:1548), which can never be FALSE because every factor level comes from `unique(split_key)`; the guard is dead code. (3) "The source is the column itself, not a copy of it" is asserted with `expect_identical()`, which compares values, not identity.
- Evidence: `expect_true(length(shown) == 2)`; `cond_all_true <- lapply(cond_n_true, function(n_true) n_seen > 0 & n_true == n_seen)`; `expect_identical(entry$source, column)`.
- Failure scenario: Mutation: cap only one side of the row evidence. Mutation: store `source = column + 0` (a copy). Both pass.
- Confidence: verified-by-reading
- Suggested direction: Assert the exact rendered row lists, drop the dead guard or its claim, and drop the identity claim (or check it with `lobstr::obj_addr`).

### F-t-core-3-16  [nit] [inconsistency] (UI string, cross-slice R/)
- Where: R/evaluateRules.R:552-555 (pinned by tests/testthat/test-evaluateRules.R:18, 45, 648, 727, 946, ...)
- What: Rule messages read "violated: 1 rows", ungrammatical for a count of one, and the tests pin the ungrammatical form.
- Evidence: `"Rule '%s' violated: %s rows where %s not in range [%s, %s]"`; `expect_match(result$message, "violated: 1 rows")`.
- Failure scenario: Every one-row failure in a validation report reads "1 rows".
- Confidence: verified-by-reading
- Suggested direction: Pluralise (cli `{?s}` or an explicit branch), and update the pins.

## test-loadDelivery.R, test-upgrade.R

### F-t-core-3-17  [major] [dead-code]
- Where: R/containerAssignments.R:112-253 (`dta_is_stale()`, `dta_is_old_matrix()`, `dta_upgrade()`, exported `upgrade_dta()`), called at R/loadDelivery.R:160, R/DTA-class.R:519/1034, R/DTADataSetTabular-class.R:892/1491, R/DTADataSetFile-class.R:613/787, R/DTADataSetMatrix-class.R:761, R/containerFindings.R:53, R/containerAssignments.R:51, R/documentData.R:57, R/lintDTA.R:1391/1423, R/fileNameConflicts.R:488. Tests: tests/testthat/test-upgrade.R (whole file), tests/testthat/test-loadDelivery.R:1766-1813.
- What: This is a whole backwards-compatibility subsystem for objects `saveRDS()`d by an older DTAtools version. It includes an exported function, a detector for the retired matrix `corner_mode` format, and a stale-check at every entry point. The owner states no compat is needed before 1.0.0.
- Evidence: Roxygen: "A DTA or dataset saved with saveRDS() by an older version of DTAtools keeps the class definition it was saved with ... upgrade_dta() re-creates every DTAtools object". `dta_is_old_matrix <- function(obj) { ... !is.null(attr(m, "corner_mode", exact = TRUE)) }`. The comment at :114 reads "A restored 0.27.0 object always qualifies". test-upgrade.R builds "old-shaped" classes (`old_shape_class`, `age_object`) just to keep this alive, and test-loadDelivery.R:1766-1800 duplicates that machinery as `ld_old_class` / `ld_age`.
- Failure scenario: There is no wrong verdict. The cost is a class-walk at every entry point, an exported API to keep documented and qualified, and two copies of an "age an object" harness in the tests (`old_shape_class`/`age_object` vs `ld_old_class`/`ld_age`, near-identical).
- Confidence: verified-by-reading
- Suggested direction: Remove `upgrade_dta()`/`dta_upgrade()`/`dta_is_stale()`/`dta_is_old_matrix()` and their call sites and tests. An old RDS then fails with S7's own error. If any of it stays, at least deduplicate the two aging helpers into a helper file.

### F-t-core-3-18  [minor] [test-weak]
- Where: tests/testthat/test-loadDelivery.R:1672-1691
- What: The test is titled "a refused file is not reported as missing". Its key assertion is vacuous whenever no summary message is emitted, because `msg` stays NULL and `"ae.sas7bdat" %in% NULL$missing` is FALSE.
- Evidence: `msg <- NULL; d <- withCallingHandlers(suppressWarnings(load_delivery(...)), message = function(m) { if (inherits(m, "dta_delivery_summary")) msg <<- m; invokeRestart("muffleMessage") })` followed by `expect_false("ae.sas7bdat" %in% msg$missing)`, with no check that `msg` was captured.
- Failure scenario: Mutation: `load_delivery()` stops emitting `dta_delivery_summary` in skip mode, or emits it under another class. `msg` is NULL, and the "not reported as missing" assertion passes without testing anything.
- Confidence: verified-by-reading
- Suggested direction: Add `expect_s3_class(msg, "dta_delivery_summary")` before the `missing` assertion, or use `expect_message(..., class =)` as the rest of the file does.

## test-DTAFile.R, test-DTAFileAny.R

### F-t-core-3-19  [minor] [test-cheat]
- Where: tests/testthat/test-DTAFile.R:1051-1072 (code: R/DTAFileTabular-class.R:566, 579-583)
- What: The test "a half-written copy does not survive the failure that produced it" checks for leftovers with a file-name pattern the implementation never uses, so its "no copy left behind" assertion can never fail.
- Evidence: Test: `before <- list.files(tempdir(), pattern = "^file.*\.csv$")` ... `expect_identical(list.files(tempdir(), pattern = "^file.*\.csv$"), before)`. Code: `destination <- tempfile("dta_transcode_", tmpdir = dta_temp_dir(), fileext = ".csv")`, so the copy is named `dta_transcode_<hex>.csv` and `^file` never matches it. The comment's "and nothing cached that a later call could return" is not asserted at all.
- Failure scenario: Mutation: delete `on.exit(if (!finished) unlink(destination), add = TRUE)` (R/DTAFileTabular-class.R:583). The half-written `dta_transcode_*.csv` stays in tempdir(), and the test still passes. The skip at :1061 also turns the test off wherever iconv accepts the bytes (the comment names Windows), so on the owner's machine it may not run at all (inferred; not executed).
- Confidence: verified-by-reading (pattern vs `tempfile()` prefix). Inferred: whether the skip fires on Windows.
- Suggested direction: Snapshot `list.files(dta_temp_dir(), "^dta_transcode_")`, and also assert that the transcode cache holds no entry for the key.

### F-t-core-3-20  [minor] [test-weak]
- Where: tests/testthat/test-DTAFile.R:1222-1342
- What: Twelve DTAFileAny `test_that()` blocks (1246-1339) are nested, indented, inside the unrelated test "a pattern description survives the factory and a YAML round trip". They split that test's setup (1230-1239) from its only assertion (1341, about 100 lines later). Several of them duplicate tests in test-DTAFileAny.R.
- Evidence: Line 1239 closes the `dta_dataset_from_list(...)` call, then `# DTAFileAny class` and `  test_that("DTAFileAny constructs and carries its properties", {`, ... and `  expect_equal(ds@files[[1]]@pattern_description, "one file per batch")` before the outer `})` at 1342. Duplicates: 1302-1309 vs test-DTAFileAny.R:21-26 (report.pdf.gz); 1311-1323 vs test-DTAFileAny.R:60-70; 1336-1339 repeats 1251-1252.
- Failure scenario: testthat 3.3 runs them as sub-tests of the pattern-description test (installed NEWS: "Tests ... can now be arbitrarily nested"). A DTAFileAny failure is therefore reported under the pattern-description test, and an error inside a nested block (for example a constructor regression) aborts the outer test before its own assertion at 1341, hiding whether pattern descriptions survive. This looks like a merge accident.
- Confidence: verified-by-reading. Inferred: the testthat nesting semantics come from the installed NEWS.md (3.3.2), not from running the suite.
- Suggested direction: Move the DTAFileAny blocks to top level in test-DTAFileAny.R, drop the duplicates, and close the pattern-description test after its assertion.

## test-templateGuide.R

### F-t-core-3-21  [minor] [test-weak]
- Where: tests/testthat/test-templateGuide.R:109, 154, 177, 187, 203, 236, 262, 276, 296, 312, 321, 372, 408, 426, 441, 453, 471, 485, 503, 520, 541, 562, 588, 625, 633, 644, 663, 742, 777, 795 (30 occurrences)
- What: Thirty tests skip when the bundled template library is missing. That library (`inst/extdata/library`) is a guaranteed package asset, so a packaging regression would silently switch off nearly the whole render-side guide suite instead of failing it. The sibling collect-side file treats the same absence as a failure, and the repo's own helpers say "a missing fixture is a failure, not a skip".
- Evidence: `skip_if(!nzchar(tg_lib()), "bundled templates not installed")` with `tg_lib <- function() system.file("extdata", "library", package = "DTAtools")`. Compare test-templateGuide-collect.R:5-7, which uses `bundled_library()` with no skip, and helper-shinyapp.R:30-31 ("The app is a guaranteed package asset — a missing app directory is a failure, not a skip").
- Failure scenario: Mutation: an `.Rbuildignore` line that drops `inst/extdata/library` from the tarball (the kind of slip the repo has had before with an unanchored `.lock` line). Under R CMD check, 30 tests in this file skip and the run stays green. The collect-side file would fail, but only on its own tests.
- Confidence: verified-by-reading (all 30 skip lines counted with grep, and the helper read). Inferred: the R CMD check consequence, not executed. Similar skips on bundled assets exist in other slices (test-DTAFileSAS7BDAT.R:842, test-exportMarkdownDocument.R:620/730).
- Suggested direction: Replace the skips with `expect_true(nzchar(tg_lib()))`, or a helper that fails on a missing guaranteed asset.

### F-t-core-3-22  [nit] [test-weak]
- Where: tests/testthat/test-templateGuide.R:769-773
- What: The test claims "The cell carries exactly the shared rule text, for every rule", but its expectation is computed by `.rule_text()`, the same helper the renderer uses. Rule-text lines beyond the hand-written ones at :755-767 are therefore compared with themselves.
- Evidence: `for (i in seq_along(specs@rules)) expect_identical(tbls[[1]]$description[[i]], .rule_text(specs@rules[[i]]))`
- Failure scenario: Mutation: a wrong "Missing values:" line produced by `.rule_text()` for r_range. Cell and expectation agree, and the test passes. Only lines 1-2 of r_range and r_cond and line 1 of r_unique are hand-written.
- Confidence: verified-by-reading
- Suggested direction: Write out every expected line by hand (the specification exists in R/rulePhrasing.R comments), or label this loop as a rendering-consistency check only.

## test-exportTemplateDocx-blocks.R

### F-t-core-3-23  [minor] [doc-wrong] (resolves the open question in F-t-core-3-08)
- Where: tests/testthat/test-docxLayout.R:88-93 (title); R/formattingHelpers.R:151-155 (roxygen of `.fit_table_width`); R/exportTemplateBlocks.R:312-349 (`.tv_block_fit`), plus the `width = NULL` call sites at :355, 396, 438, 471, 476, 553
- What: The docxLayout test title ("export_with_template keeps its tables at their built widths") and the `.fit_table_width()` roxygen say template exports leave tables at their built widths. They do not. `.tv_block_fit()` rescales every block table, in proportion, to the text width of the template section it lands in. test-exportTemplateDocx-blocks.R:236-389 pins exactly that. Since both rescales are proportional, the `width = NULL` arguments at the call sites no longer change the fixed-layout result: they are dead plumbing kept alive by an inaccurate comment.
- Evidence: formattingHelpers.R: "`width = NULL` leaves the table as built: user templates passed to `export_with_template()` have page geometry the package cannot assume". exportTemplateBlocks.R:335-337: `w <- dim(ft)$widths; ft <- flextable::width(ft, width = w * text_w / sum(w)); flextable::set_table_properties(ft, layout = "fixed")`. blocks test :225-234 `.expect_block_tables_fill_section`: "Every table's grid must sum to its section's text width".
- Failure scenario: Mutation: drop `width = NULL` at exportTemplateBlocks.R:396. `.fit_table_width()` scales to 9.7in, then `.tv_block_fit()` scales proportionally to the section width, so the output on the fixed path is identical (only the autofit fallback's grid hints change; inferred). Neither the docxLayout test nor the blocks tests can fail, because nothing observable depends on the argument.
- Confidence: verified-by-reading (both scaling functions and the call path `.tv_block_table` -> `.tv_block_fit`). Inferred: autofit-path grid hints, not inspected in an exported file.
- Suggested direction: Remove the `width = NULL` plumbing (or the pre-scale), fix the roxygen, and delete or retitle the docxLayout test.

### F-t-core-3-24  [minor] [test-weak]
- Where: tests/testthat/test-exportTemplateDocx-blocks.R:793-820
- What: The test claims the five blocks "each render identifiable fixture content", but two of its five markers are not unique to the block they are attributed to. Emily Turner and Alice Smith each appear in two of the rendered blocks, so either block can render nothing and the test still passes.
- Evidence: Fixture `create_example_DTAMetaData(2)` (R/DTAMetaData-class.R:668-719): Emily Turner is a supplier contact with `signature = TRUE`, so she is in SIGNATURES_TABLE and in SUPPLIER_CONTACTS_TABLE. Alice Smith is a receiver contact with `signature = TRUE` and is also in `authorized_for_corrections`, so she is in SIGNATURES_TABLE and in AUTHORIZED_CORRECTIONS_LIST. Test: `expect_match(txt, "Emily Turner") # SIGNATURES_TABLE`, `expect_match(txt, "Alice Smith") # AUTHORIZED_CORRECTIONS_LIST`, both run on the concatenated text of all five blocks.
- Failure scenario: Mutation: `{AUTHORIZED_CORRECTIONS_LIST}` expands to an empty paragraph. "Alice Smith" still comes from the signatures table and the test passes. grep finds no other test anywhere in tests/ that renders `{AUTHORIZED_CORRECTIONS_LIST}`, so the regression would ship. (SIGNATURES_TABLE alone is covered by test-exportTemplateDocx-regions.R:508-523.)
- Confidence: verified-by-reading
- Suggested direction: Render each block into its own template, or assert on the paragraph or table each block produces (for example the exact list paragraphs "Alice Smith", "Bob Johnson" in order).

### F-t-core-3-25  [nit] [test-weak]
- Where: tests/testthat/test-exportTemplateDocx-blocks.R:391-412, 414-444, 501-548, 1081-1095
- What: These are the same pattern as F-t-core-3-22. The rule cells are compared with `.rule_text()`, the helper the renderer itself calls. Only a few lines are written out by hand: the grouped rule at :538-547, and none at all for `age_range_check` at :411.
- Evidence: `expect_identical(unname(descriptions[["age_range_check"]]), .rule_text(range_rule))`
- Failure scenario: Mutation: `.rule_text()` drops the "inclusive" bound wording. Renderer and expectation change together, so every one of these tests passes. Only test-rulePhrasing.R, in another slice, could catch it.
- Confidence: verified-by-reading
- Suggested direction: Keep one consistency check, and spell out at least one full rule cell per rule type by hand.

## test-exportTemplateDocx.R

### F-t-core-3-26  [minor] [code-cheat] (R/ cross-slice; pinned by this slice's tests)
- Where: R/exportTemplateDocx.R:102-107, 142, 180-206; tests/testthat/test-exportTemplateDocx.R:649-652, 759-765
- What: With `export_with_template()`'s default `fallback = TRUE`, any error during template processing is caught: a broken template, but equally an internal bug in a block renderer. The function then writes the built-in `write_dta()` layout to `output` instead. With `quiet = TRUE` it does this without any condition at all, and two tests pin that silent substitution as intended.
- Evidence: `tryCatch({ .replace_docx_placeholders(...) ... }, error = function(e) { if (isTRUE(fallback)) { if (!isTRUE(quiet)) { cli::cli_warn(...) }; write_dta(dta, output, format = "docx", overwrite = TRUE, quiet = quiet) ...`. Test: `expect_no_warning(export_with_template(dta, bad_template, out, quiet = TRUE, fallback = TRUE))` and "# quiet = TRUE keeps its meaning: the fallback still happens, silently." All the same, test :724 is titled "the template fallback is detectable programmatically".
- Failure scenario: A script calls `export_with_template(dta, "sponsor_template.docx", out, quiet = TRUE)` to stay quiet. A regression in `.tv_render_blocks()` makes every template fail. Every output silently becomes the generic layout, with exit status OK and no warning, and the documents ship in the wrong format. `quiet` is documented as "suppresses console output", but here it also suppresses the only failure signal. (`write_dta()` and the app both pass `fallback = FALSE`, so only direct API callers are affected.)
- Confidence: verified-by-reading (function and both call sites: R/exportDocuments.R:168-178, inst/shiny/dta_app/app.R:10168-10174)
- Suggested direction: Default `fallback = FALSE`, or always signal the substitution (a classed warning that `quiet` does not suppress), and catch only template-format conditions, not every error.

## test-DTADataSetFile.R

### F-t-core-3-27  [minor] [test-weak]
- Where: tests/testthat/test-DTADataSetFile.R:22-46 (code: R/DTADataSetFile-class.R:412-434)
- What: The test is titled "DTADataSetFile reports missing or unreadable files", but it only covers a missing file. The two "unreadable" outcomes of `validate_file_dataset_entry()` (the `file.access()` refusal and the failed read probe) are not exercised by any test in tests/ (grep for "not readable", "could not be read", `Sys.chmod`, `file.access`).
- Evidence: Code: `if (file.access(path, 4) != 0) { ... "File '%s' is not readable." }` and `readable <- tryCatch({ con <- file(path, open = "rb"); ...; readBin(con, ...); TRUE }, error = function(e) FALSE)` followed by "could not be read". Test: the only fixture is `tempfile()` with the file deleted, and the only assertion is `expect_match(msgs$message, "not found")`.
- Failure scenario: Mutation: delete the `file.access()` branch and the read probe (or make the probe return TRUE always). An unreadable deliverable then validates as present and readable, and no test fails. The probe also swallows the OS error, so the message never says why the read failed.
- Confidence: verified-by-reading (code and grep across tests/). Inferred: no other slice uses a different spelling to test unreadability.
- Suggested direction: On non-Windows, `Sys.chmod(path, "000")` with `skip_on_os("windows")` plus `skip_if(running as root)`; at least retitle the test, and keep `conditionMessage(e)` in the "could not be read" message.

### F-t-core-3-28  [nit] [test-weak]
- Where: tests/testthat/test-DTADataSetFile.R:382-388
- What: "check() on a DTA with an empty DTADataSetFile does not error" only asserts `expect_no_error()` and nothing about the verdict. The interesting property, that an undelivered dataset is not reported as a pass, is pinned only in other files (test-DTA.R:706, test-validation-summary.R).
- Evidence: `expect_no_error(check(dta, quiet = TRUE))`
- Failure scenario: Mutation: `n_undelivered <- 0L` in R/DTA-class.R:1180. This test passes. Other slices catch it.
- Confidence: verified-by-reading
- Suggested direction: Also assert `attr(result, "last_validation_ok")` is FALSE (or `n_undelivered == 1`).

## test-DTAMetaData.R

### F-t-core-3-29  [minor] [test-weak] (+ cross-slice code-cheat)
- Where: tests/testthat/test-DTAMetaData.R:432-451; R/DTAMetaData-helpers.R:224-263 (and :214-221)
- What: The test pins a real defect: `get_receiver_reviewers()` returns `list()` instead of `character(0)` when no contact is a reviewer. It is a known-defect pin, but it does not carry the repo's marker `KNOWN DEFECT, pinned rather than endorsed`, so the documented "search for the marker" process ([assistant-tool].md, Conventions) cannot find it. The function also rewrites every internal error into a misleading "x must be a DTAMetaData object" message.
- Evidence: Test comment: "Deferred: ... Pinned, not asserted as correct: the desired result is character(0) for name_only = TRUE", then `expect_identical(get_receiver_reviewers(md, name_only = TRUE), list())`. Code: `sapply(reviewers, function(c) c$name %||% NA_character_)`. On an empty list, `sapply()` returns `list()`. The roxygen `@return` says "A character vector of reviewer names". The function is wrapped in `tryCatch({...}, error = function(e) cli::cli_abort("x must be a DTAMetaData object with receiver and contacts"))`, so a malformed contact (for example a non-list `contacts`) is blamed on `x`'s class. `get_authorized_for_corrections()` does the same at :215-220.
- Failure scenario: A maintainer looking for open known defects with the documented search never sees this one. A caller doing `if (length(r) && is.character(r))`, or `paste(r, collapse = ", ")` on the "no reviewers" path, gets a different type than the docs promise.
- Confidence: verified-by-reading
- Suggested direction: Fix it (`vapply(..., character(1))`), or at least add the sanctioned marker and name the switch-to assertion `expect_identical(..., character(0))`. Narrow the `tryCatch()` to the property access only, as was already done for `get_version_history_df()` (test :548-556).

### F-t-core-3-30  [nit] [test-weak]
- Where: tests/testthat/test-DTAMetaData.R:453-465, 315-322
- What: Two tests are existence-only. `get_transmission_dates()` is checked only for the class of the two values, never their values or which is first. The YAML read test checks only `nchar(md@title) > 0`.
- Evidence: `expect_s3_class(dates2$first_transfer, "Date"); expect_s3_class(dates2$last_transfer, "Date")`; `expect_true(nchar(md@title) > 0)`.
- Failure scenario: Mutation: `get_transmission_dates()` returns first and last swapped. Every testthat test passes, because the round-trip test at :378-381 compares the function with itself. Inferred: inst/qualification OQ-META-014 may catch it, but that suite is run separately.
- Confidence: verified-by-reading (testthat). Inferred for the qualification test.
- Suggested direction: `expect_identical(dates2$first_transfer, as.Date("2026-02-01"))` etc., and `expect_identical(md@title, "Clinical Data Specification")`.

## test-importAxis.R

### F-t-core-3-31  [minor] [test-weak]
- Where: tests/testthat/test-importAxis.R:1-4, 516-645 (class contract: R/DTAColumnSpecCollection-class.R:97-109; R/lintDTA.R:307-315, 362-375)
- What: The file replaces the `DTAColumnSpecCollection` constructor with the rule-unchecked variant for every test in it. The "stage 1" import-axis tests then isolate the import axis with specs the package refuses to build: a `col_range` rule and a `greater_equal` comparison on a column declared `SAS Char`. The behaviour they pin ("fails the run on the import axis alone") is therefore reachable only through objects that bypassed validation (legacy, restored, or `check = FALSE`), never from a user-built spec.
- Evidence: `DTAColumnSpecCollection <- unchecked_collection` (file level). `range_import_specs()`: AGE `type = "SAS Char"` + `DTARuleColRange(... columns = "AGE" ...)`. `import_only_specs()`: AGE `SAS Char` + `condition = list(AGE = list(greater_equal = 18))`. The real validator rejects both: `rule_range_non_numeric` (error) and `rule_condition_numeric_on_text` (error, `is_char`). The test asserts `expect_equal(details$import_errors$declared_type, "SAS Char")`, i.e. an "unrepresentable value" error against a text type in which "ninety" is perfectly representable.
- Failure scenario: The titles read as general import-axis behaviour ("an unconvertible value fails the run on the import axis alone", "import errors reach messages() ..."). A refactor that changes how a buildable spec (SAS Num column, numeric rule) reports import errors is not exercised here, and the "SAS Char" import error these tests protect is a state no valid spec can reach.
- Confidence: verified-by-reading (shadowing, the two spec builders, validator, lint codes)
- Suggested direction: Scope `unchecked_collection` to the tests that are about contradictory specs. Rebuild stage 1 on a buildable spec (a SAS Num column with a numeric rule, accepting that the column-spec axis also fails, or an R-type backend column), or delete it with the compat path.

### F-t-core-3-32  [minor] [dead-code]
- Where: R/validationFunctions.R:554-593 (`dta_migrate_validation_details()`), R/validationReporting.R:350-357, R/DTADataSetTabular-class.R:1291/1310, R/DTADataSetFile-class.R:1007/1028; tests/testthat/test-importAxis.R:31-44, 88-162, 199-203, 261-272, 287-300, 413-430
- What: This is backwards-compatibility code for validation artifacts and index entries written "by an older version of the package" (before the import axis, `result_version` 1), plus handling of non-DTADataSet entries in `results(DTA)` that the class validator now makes unreachable. Tests keep it alive with hand-built "legacy" shapes. The owner states no compat is needed before 1.0.0.
- Evidence: roxygen "Brings a `validate_table_detailed()` result recorded by an older version of the package up to the current shape"; reporting branch "validation artifact predates import checking (result_version 1); re-run check(force = TRUE)"; test `legacy_details()` "Exactly the shape validate_table_detailed() returned before the import axis existed"; test :291-294 "The @datasets validator now rejects a non-DTADataSet entry, so the stub can only arise from state that bypassed it; reproduce that with check = FALSE".
- Failure scenario: None functional; it is dead weight on every artifact read and in the reporting code, and the NOTE at validationFunctions.R:584-593 already documents a second compat gap (`columnspec_checks`) that the migration does not cover.
- Confidence: verified-by-reading
- Suggested direction: Delete the migration, the result_version-1 reporting branch, the non-DTADataSet stub handling, and their tests. An artifact from an older build is then simply re-validated (or refused).

## test-rTemporalFormat.R, test-DTAColumnSpecStructureR.R, test-logical-r.R, test-lintDTA-matrix.R, test-DTAFileXPT.R, test-matrix-reading.R, test-export-matrix.R

(test-rTemporalFormat.R, test-DTAColumnSpecStructureR.R, test-logical-r.R and test-lintDTA-matrix.R were read in full and yielded no finding. Their expected values are hand-derived, and the base-R differential oracle in test-rTemporalFormat.R:373-445 has power checks.)

### F-t-core-3-33  [minor] [design] (data loss pinned as a "limitation")
- Where: tests/testthat/test-DTAFileXPT.R:336-343; R/DTAFileXPT-class.R:20-25, 184-199
- What: For a V5 transport file whose rows are shorter than 80 bytes, trailing all-blank rows are silently dropped, because the row count cannot be read from the file. `read_file()` then returns the shorter table with no condition at all, and the test pins that silent outcome. Those dropped rows are exactly the rows a `nullable: false` check exists to catch, so `check()` reports PASS on a delivery that holds blank records.
- Evidence: Roxygen: "a V5 file whose rows are shorter than 80 bytes does not say how many it holds, and its trailing blank rows are lost". Code: `expected <- dta_xpt_expected_rows(...); if (!is.na(expected) && nrow(df) != expected) {...}`, so when `expected` is NA nothing is signalled. Test: `expect_true(is.na(dta_xpt_expected_rows(...))); expect_identical(nrow(read_file(DTAFileXPT("t.xpt"), p)), 2L)` ("Pinned as the documented limitation").
- Failure scenario: A V5 AE.xpt with 4-byte rows ends in two blank records (S1, S2, "", ""). The package reads 2 rows, raises no import or column-spec finding, and the agreement validates clean. The same data as CSV fails `nullable: false` (compare test :311-333).
- Confidence: verified-by-reading
- Suggested direction: When `expected` is NA and the last 80-byte record's padding could hold at least one row, raise a classed warning ("row count cannot be verified; trailing blank rows may be lost"), or a validation finding, instead of trusting haven's count silently. Assert that condition in the test.

### F-t-core-3-34  [minor] [test-weak]
- Where: tests/testthat/test-export-matrix.R:270-279
- What: The test is titled "{COLUMN_SPECS} without a heading still renders the matrix's row-label table", but its only content assertion ("GENE_ID" appears anywhere) is also met by the "Matrix layout" block that the same call renders. The table itself is never checked.
- Evidence: `expect_true("GENE_ID" %in% text || any(grepl("GENE_ID", text)))`. The first disjunct is subsumed by the second. The layout rows contain "Each line starts with the row label columns GENE_ID, GENE_NAME" and "Row key: GENE_ID (unique, no missing values)" (pinned at :35, :48), and the test asserts the layout is present (`"Matrix layout" %in% text`).
- Failure scenario: Mutation: `.tv_block_render_column_specs(..., heading = FALSE)` skips the row-label table and renders only the layout block. "GENE_ID" still matches through the layout text, and the test passes.
- Confidence: verified-by-reading (test and the pinned layout wording). Inferred: that `.docx_paragraphs()` also returns layout-table cells, since officer's `docx_summary()` includes table cells.
- Suggested direction: Assert the row-label table's cells (for example via `docx_summary()` with `content_type == "table cell"` holding both "GENE_ID" and "GENE_NAME" in its first column).

### F-t-core-3-35  [nit] [test-weak]
- Where: tests/testthat/test-matrix-reading.R:251-254, 343-348; tests/testthat/test-DTAFileXPT.R:410
- What: Two problems. First, linear-time guarantees are asserted with wall-clock bounds (`expect_lt(elapsed, 5)` / `expect_lt(elapsed, 10)`), which flake on slow runners and catch only a gross regression. test-evaluateRules.R:2388-2425 deliberately asserts "Cost shape ... structurally rather than by wall clock". Second, test-DTAFileXPT.R:410 matches the bare digit "3" in an error message.
- Evidence: `elapsed <- system.time(h <- dta_head_records(path, 2L))[["elapsed"]]; expect_lt(elapsed, 5)`; `expect_match(xpt_flat(e), "3", fixed = TRUE)`.
- Failure scenario: A quadratic scan of a 3 MiB file that still finishes in under 5 s on the dev machine passes. A loaded macOS runner fails it spuriously. The bare "3" matches any message containing a 3 (a path, "announces 2 rows, but 3").
- Confidence: verified-by-reading
- Suggested direction: Count reads or bytes structurally (as the evaluateRules probe does), and match "3 could be read".

## test-columnspec-format.R, test-template-matrix.R, test-DTAColumnSpecCell.R, test-bundled-templates.R, test-lintTemplateRepo.R, test-DTAContainer.R, test-DTAFile-container.R, test-matrix-review-fixes.R

(test-template-matrix.R, test-DTAColumnSpecCell.R, test-bundled-templates.R and test-DTAContainer.R were read in full with no finding of their own. `ignore_attr = TRUE` in test-DTAColumnSpecCell.R:167/307 only drops row names of a frame the test builds itself, so it hides nothing.)

### F-t-core-3-36  [minor] [dead-code] (more compat leftovers, same family as F-t-core-3-17)
- Where: R/DTAFile-class.R:437-439 (`dta_handler_container`); tests/testthat/test-DTAFile-container.R:189-194; tests/testthat/test-matrix-review-fixes.R:22-40, 157-176; tests/testthat/test-DTADataSetFile.R:887-892
- What: More code and tests that exist only for objects saved by older versions: a handler "restored from an autosave written before `container` existed", matrices "saved in the earlier format" (`corner_mode`), and "a legacy object" whose `file_handlers` is out of step. `dta_handler_container()` implements its compat path as a blanket error swallow.
- Evidence: `dta_handler_container <- function(h) tryCatch(h@container, error = function(e) NULL)` with the comment "A handler restored from an autosave written before `container` existed has no such slot". Test: `Old <- S7::new_class("Old", ...); expect_null(dta_handler_container(Old(filename = "a.csv")))`. test-matrix-review-fixes.R ages a matrix with `attr(settings, "corner_mode") <- "blank"` to drive `dta_is_old_matrix()`.
- Failure scenario: Passing a non-handler (a list, NULL, a dataset) to anything that reads the container yields "no container" instead of an error, so a wrongly wired call silently drops a packaging requirement.
- Confidence: verified-by-reading
- Suggested direction: Delete with F-t-core-3-17. Read `h@container` directly.

### F-t-core-3-37  [major] [security] (cross-slice: R/lintDTA.R; pinned by this slice)
- Where: R/lintDTA.R:1323-1349 (column specs), R/lintDTA.R:936-945 (the guarded metadata path, for contrast), R/DTA-class.R:1570-1578; tests/testthat/test-matrix-review-fixes.R:128-143
- What: A column (or row-label) mapping from a DTA YAML goes straight into `do.call(DTAColumnSpec, raw)` with no unknown-key check. Two consequences follow. (1) R's partial argument matching silently accepts any unique prefix of a field, so `example:`, `desc:`, `null:` or `pat:` take effect without a word, and the test pins this as intended ("`example:` reaches `examples` by R's partial matching, as it always has"). (2) Any unknown key makes R build its "unused argument" error by deparsing the key's whole value. The package's own comments call that "exponential for a YAML alias bomb" and guard exactly that case for `metadata` (and for file entries and containers), but not for columns.
- Evidence: Column path: `# (Other keys are left to do.call(): it matches them partially, and \`example:\` for \`examples:\` is in files that exist.)` then `do.call(DTAColumnSpec, raw)`. Metadata path: "An unknown key makes R print the whole offending argument into its 'unused argument' error, which is exponential for a YAML alias bomb, so unknown keys are reported by name here, before anything is expanded." followed by `setdiff(names(md), names(formals(DTAMetaData)))`. Templates do check column keys (R/validateTemplate.R:1462 `col_keys`), but DTA documents do not.
- Failure scenario: (a) An uploaded DTA YAML with `columns: [{id: X, type: SAS Char, zz: *bomb}]`, where `*bomb` is a billion-laughs alias chain, makes `lint_dta()` / `read_dta_from_yaml()` (and the app's upload) spend exponential time deparsing it, the very DoS the metadata path was fixed for. (b) An unknown key's error text is R's own translated "unused argument" message (German on the owner's machine), which the lint report then carries. (c) An abbreviation is accepted in a document that `validate_template()` would reject as a template.
- Confidence: verified-by-reading (the missing guard and the asymmetry with metadata, file entries and containers). Inferred: the exponential deparse, taken from the repo's own comments at lintDTA.R:936-938 and DTA-class.R:1570-1571; not executed here (no R runs allowed).
- Suggested direction: Check `setdiff(names(raw), c(names(formals(DTAColumnSpec)), "values_from", "header"))` before `do.call()`, as metadata and file entries already do, and call a key unknown unless it matches exactly. Then drop the partial-matching pin.

### F-t-core-3-38  [minor] [test-weak]
- Where: tests/testthat/test-lintTemplateRepo.R:157-185
- What: The non-ASCII tests are titled "is linted, not aborted on", but they only assert `expect_no_error()` and the column set. Their fixture template is also written in a format the package no longer reads (`dta_template: 1`, `kind: creation`), so nothing shows the non-ASCII file was linted at all.
- Evidence: `writeLines(c("dta_template: 1", paste0("id: ", id), "version: '1.0'", "kind: creation", ...))`; then `res <- expect_no_error(lint_quiet(root, fail_on = "none")); expect_identical(names(res), lint_columns)`, with no assertion on any row for `café.dta-template.yaml`. The repository-folder variant (:182-185) asserts only `expect_no_error()`. (The export-template variant at :187-195 does count findings, so it is fine.)
- Failure scenario: Mutation: the linter drops every file whose name is not ASCII before scanning, which is a plausible "fix" for the radix-sort abort. No error, same columns, and both tests pass while non-ASCII templates go unchecked.
- Confidence: verified-by-reading. Inferred: that the outdated `kind: creation` is reported as a finding (it may simply be ignored, which would make the test weaker still).
- Suggested direction: Write a current-format template with a known defect and assert its finding (file and code) appears for the non-ASCII path.

### F-t-core-3-39  [nit] [test-weak]
- Where: tests/testthat/test-columnspec-format.R:33-37 (used throughout, notably :337-347)
- What: `cf_expect()` compares findings with `expect_setequal()`, which ignores duplicates. A test titled "a fraction in an integer-only column is reported once, as a type" therefore cannot detect the same finding being reported twice.
- Evidence: `expect_setequal(found$eager, expected); expect_setequal(found$streamed, expected)` with `expected = c("2 type", "3 format")`.
- Failure scenario: Mutation: one engine reports "2 type" twice (a double-report regression, the class of bug the "double reports" section header names). `setequal` still holds.
- Confidence: verified-by-reading
- Suggested direction: Compare sorted vectors with `expect_identical(sort(found$eager), sort(expected))`.

## test-run-dta-app.R, test-datePatternTokens.R, test-local-template-library.R

(test-datePatternTokens.R was read in full with no finding. Its expected regex fragments and boundary values are hand-written, and the PCRE and ICU engines are cross-checked.)

### F-t-core-3-40  [minor] [doc-wrong]
- Where: R/run_dta_app.R:15-21 (roxygen `@details`); tests/testthat/test-run-dta-app.R:17, 27-31
- What: The roxygen says only "shiny, bslib or DT" trigger the informative error, but the code checks four packages, shinyjs included. The test's comments still say "exactly these three (R/run_dta_app.R:28)" and cite line 41 for the `system.file()` call. Both line references are stale (the code is at :32 and :45), and the test's own `required` vector omits shinyjs.
- Evidence: Code: `required <- c("shiny", "bslib", "DT", "shinyjs")`. Roxygen: "If any of **shiny**, **bslib** or **DT** is missing, an informative error explains what to install." Test: `required <- c("shiny", "bslib", "DT")` with "# three (R/run_dta_app.R:28)".
- Failure scenario: A reader of `?run_dta_app` is told that a missing shinyjs produces no informative error, when it does. The DESCRIPTION check in this test would not notice shinyjs being dropped from Suggests. That gap is covered only because app.R happens to use `shinyjs::` (11 hits) and test :140-162 scans for it.
- Confidence: verified-by-reading
- Suggested direction: Say "shiny, bslib, DT or shinyjs" in `@details`. Derive the test's list from the function (or add shinyjs) and drop the line numbers from the comments.

### F-t-core-3-41  [nit] [test-weak]
- Where: tests/testthat/test-local-template-library.R:13-17, 32-37 (both tests)
- What: Both tests skip unless the developer variable `DTATOOLS_LOCAL_LIBRARY` names an existing directory, which never happens in CI. They provide no assurance in any automated run, yet they sit in the suite as if they were coverage.
- Evidence: "Inert unless DTATOOLS_LOCAL_LIBRARY names an existing directory, so CI and every other developer skip it." `skip_if(!nzchar(local_template_library()), ...)`.
- Failure scenario: Any regression in `build_template_index()` / `template_picker_entries()` that only a real library exposes ships green. The file reports two skips per run, which teaches readers to ignore skips.
- Confidence: verified-by-reading
- Suggested direction: Move it to a developer script under `.github/scripts/`, or `tools/`, and out of tests/testthat, or document it in CONTRIBUTING as an explicit pre-release step.

## helper-validation-corpus.R (shared helper; consumers are in other slices)

### F-t-core-3-42  [nit] [doc-wrong]
- Where: tests/testthat/helper-validation-corpus.R:82-95
- What: The comment says the fixture is '"aeiou" with combining marks'. The bytes are precomposed code points: "äöüßé" is C3 A4 / C3 B6 / C3 BC / C3 9F / C3 A9 (checked with `od -c`), with no combining marks and an "ß" that is no vowel. Worse, a combining-mark spelling would be 10 characters to R's `nchar()`, which would invert the test's premise.
- Evidence: `# Pins the "maxLength counts characters, not bytes" decision. "aeiou" with combining marks is 5 characters but more than 5 bytes`, while the fixture is `NAME = c("abcde", "äöüßé", "äöüßéx")`.
- Failure scenario: A maintainer who "fixes" the fixture to match the comment (a + U+0308 ...) makes row 2 ten characters long. The case then fails for a reason unrelated to bytes versus characters, or gets its expectation re-pinned to the wrong verdict.
- Confidence: verified-by-reading (byte dump of the line)
- Suggested direction: "precomposed accented letters (2 bytes each in UTF-8)".

### F-t-core-3-43  [minor] [logic-error] (circular justification; cross-slice R/)
- Where: R/evaluateRules.R:71-76; tests/testthat/helper-validation-corpus.R:314-357 (`vc_numeric_edges`)
- What: Numeric parsing accepts hexadecimal text ("0x10" becomes 16) and "Inf"/"NaN" spellings because base `as.numeric()` does. The code defends this as "deliberate" by pointing at the pins: the oracle matrix and a qualification matrix. But the oracle matrix's own header says it records the then-CURRENT behaviour, so the justification is circular, and nobody decided that a SAS Num delivery may hold hex literals.
- Evidence: Code: "as.numeric() also reads hex ("0x1A" is 26) and "Inf" spellings. That is deliberate: the oracle matrix (vc_numeric_edges) and the qualification matrix OQ-TYPE-numeric pin them as valid numbers." Helper: "pin the behaviour of the CURRENT validation engine" ... "Each row states the CURRENT behaviour; where that behaviour is arguably wrong the comment says so". It flags "NaN" as questionable but says nothing about `"0x10" -> 16` or `"Inf"`.
- Failure scenario: A SAS Num column whose export garbled a value into "0x1F", or that carries a hex-coded flag, validates as the number 31 and passes `range` rules and the type check (inferred for the type-check path, which the comment says OQ-TYPE-numeric pins the same way). SAS itself would not read it as 31 with a standard numeric informat.
- Confidence: verified-by-reading (code comment, helper header). Inferred: the column-type path (not traced), and whether OQ-TYPE-numeric's expectation was derived from observed output, which would breach the qualification rule "an expected value never comes from the software". The qualification files were not read.
- Suggested direction: Decide explicitly. Either refuse `0x`/`Inf`/`NaN` text for SAS Num (a regex gate before `as.numeric()`), or document the acceptance in the user docs as a decision rather than as a consequence of a pin.

## Cross-slice notes (pointers only, not traced in full)

- tests/testthat/test-validation-oracle.R:1-16, 95-106 (other slice): the "Golden oracle" asserts its "behavioural contract" axis facts through `expect_snapshot_value()`, i.e. snapshots recorded from the engine's own first run (`_snaps/validation-oracle.md`). It is a characterization pin, not an independent oracle. Whoever owns that slice should check whether any snapshot value was ever verified by hand.
- R/lintDTA.R has no unknown-key guard for dataset-level keys either (only `container`, runtime keys and matrix keys are named). See F-t-core-3-37 for the column-level consequence. I did not trace whether `dta_dataset_from_list()` passes dataset keys through `do.call()` partial matching, as R/validateTemplate.R:1397 says it does for `file:`.

## Coverage

Fully read (every line):
- Helpers: helper-validation-corpus.R, helper-unchecked-collection.R, helper-source-tree.R, helper-containers.R, helper-docx.R, helper-shinyapp.R.
- Slice files (all 35): test-evaluateRules.R, test-loadDelivery.R, test-DTAFile.R, test-templateGuide-collect.R, test-exportTemplateDocx-blocks.R, test-exportTemplateDocx.R, test-DTADataSetFile.R, test-templateGuide.R, test-DTAMetaData.R, test-importAxis.R, test-rTemporalFormat.R, test-DTAColumnSpecStructureR.R, test-logical-r.R, test-lintDTA-matrix.R, test-DTAFileXPT.R, test-matrix-reading.R, test-export-matrix.R, test-columnspec-format.R, test-template-matrix.R, test-DTAColumnSpecCell.R, test-bundled-templates.R, test-lintTemplateRepo.R, test-DTAContainer.R, test-DTAFile-container.R, test-docxLayout.R, test-matrix-review-fixes.R, test-generic-guards.R, test-run-dta-app.R, test-upgrade.R, test-datePatternTokens.R, test-local-template-library.R, test-DTAFileAny.R, test-provenance.R, test-dataset-runtime-keys.R, test-source-portability.R.
- Fixtures: tests/testthat/fixtures/matrix/* (all three), fixtures/write_dta-furniture.txt (all lines).

Read in part, to verify specific claims (R/ and other slices): R/evaluateRules.R (numeric cache, range, group-condition, apply_rules, dta_as_numeric_strict), R/DTADataSet-class.R (provenance), R/containerAssignments.R (upgrade machinery), R/DTAFileTabular-class.R (transcode), R/templateGuide-collect.R (dry builds, template comparison), R/exportTemplateBlocks.R (block fit), R/formattingHelpers.R (fit width, signatories), R/exportTemplateDocx.R (fallback), R/exportDocuments.R (template route), R/DTADataSetFile-class.R (file checks), R/DTAMetaData-helpers.R, R/DTAMetaData-class.R (example metadata), R/validationFunctions.R (migration), R/lintDTA.R (column, metadata and range lint), R/DTAColumnSpecCollection-class.R (validator), R/DTAFileXPT-class.R, R/DTAFile-class.R (handler container), R/run_dta_app.R, R/datePatternTokens.R, .github/workflows/R-CMD-check.yaml, [assistant-tool].md, the first 110 lines of tests/testthat/test-validation-oracle.R, and lines 2170-2280 of tests/testthat/test-streaming-validation.R.

Not read: inst/qualification/** (OQ-TYPE-numeric and OQ-META-014 are mentioned only as inferred cross-references), tests/testthat/_snaps/validation-oracle.md.

No `KNOWN DEFECT, pinned rather than endorsed` marker occurs in any slice file (grep). The one unmarked known-defect pin is F-t-core-3-29.

Counts: 43 findings. blocker 0, major 2, minor 27, nit 14. test-weak 27, test-cheat 1, doc-wrong 5, dead-code 4, code-cheat 2, security 1, design 1, inconsistency 1, logic-error 1.
