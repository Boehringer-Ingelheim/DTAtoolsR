# Findings — t-core-2 (package tests under tests/testthat, non-Shiny)

Reviewer key: t-core-2. Tree: $SNAP (pinned 68408c44).

Slice (36 files): test-validateTemplate.R, test-exportDocuments.R, test-lintDTA.R,
test-load-file-streaming.R, test-columnspec-checks.R, test-exportValidationReport.R,
test-containerFiles.R, test-streaming-scale.R, test-createTemplateRepo.R,
test-validation-oracle.R, test-exportTemplateDocx-regions.R, test-container-findings.R,
test-columnspec-format-r.R, test-columnsSpecsFromWord.R, test-validationFunctions.R,
test-DTAColumnSpec.R, test-inspect.R, test-matrix-structure-engine.R,
test-formattingHelpers.R, test-export-containers.R, test-exportAgreementMetadata.R,
test-DTAColumnSpecStructureSAS.R, test-DTAColumnSpecStructure.R,
test-sas-temporal-text.R, test-gzip-input.R, test-DTARuleGroupCondition.R,
test-validateTemplate-app-config.R, test-final-review-fixes.R,
test-DTAMetaData-template.R, test-clinical-multiple-files.R,
test-clinical-file-dataset-example.R, test-examples.R, test-regex-limits.R,
test-rulePreview.R, test-DTAFileTSV.R, test-namespaceImports.R
(+ helpers: helper-containers.R, helper-docx.R, helper-source-tree.R,
helper-unchecked-collection.R, helper-validation-corpus.R; fixtures/)

Files fully read: all 36 slice files plus the 5 helpers and _snaps/validation-oracle.md -- see the Coverage section at the end.

---

## Helpers + small files (namespaceImports, DTAFileTSV, rulePreview, regex-limits, examples, clinical-file-dataset-example, clinical-multiple-files, validation-oracle, validationFunctions)

### F-t-core-2-01  [minor] [doc-wrong]
- Where: tests/testthat/test-validationFunctions.R:382-400 (also tests/testthat/test-examples.R:87-88)
- What: The test's title and rationale claim it exercises the 5000-row chunk-offset arithmetic of validate_table_detailed(), which no longer exists, so it is now a plain row-number check duplicating the test at :107-123.
- Evidence: test: `# validate_table_detailed() validates in chunks of 5000 rows and offsets the reported row by chunk_size * (chunk_index - 1). Only a violation in a later chunk exercises that arithmetic.` vs R/validationFunctions.R:184-187: `# The chunking and progress bar this replaces existed to keep the JSON payload handed to the validator bounded. Nothing is serialised now, so neither is needed`. test-examples.R:87-88 repeats the stale claim ("the 5000-row validation chunk boundary has its own synthetic tests").
- Failure scenario: a reader (or the qualification traceability) believes a chunk boundary is covered; there is no chunk boundary in the in-memory engine. Nothing is wrong with the assertion itself, only with what it is claimed to prove.
- Confidence: verified-by-reading (grep for `chunk|5000` in R/validationFunctions.R and R/columnSpecChecks.R finds only the two "this replaces" comments).
- Suggested direction: retitle/re-comment as a large-row-index check, or delete as a duplicate; fix the test-examples.R comment.

### F-t-core-2-02  [minor] [test-weak]
- Where: tests/testthat/test-validationFunctions.R:162-174 and :200-214
- What: Two titles claim more than (or the opposite of) what the body asserts.
- Evidence: :162 `"A column-spec-only failure still records that the rules were checked"` asserts only `result$rules_valid` TRUE and `rule_errors` empty — exactly the defaults validate_table_detailed() keeps when no rule runs (R/validationFunctions.R:269-271 `rule_errors <- list(); rules_valid <- TRUE`); nothing in the result records that rules ran. :200 `"validate_table does not abort when a rule names an absent column"` then asserts `expect_error(validate_table(...), "Rule violations")`, i.e. that it DOES abort (the intended claim is "reports a rule violation instead of an internal R error").
- Failure scenario: mutation `if (length(rules_list) > 0 && !has_columnspec_errors)` (skip rules when the column spec axis failed) passes the :162 test (it is caught only by the separate one-pass test at :125-160).
- Confidence: verified-by-reading
- Suggested direction: give :162 a rule that the table violates in its non-length aspect (e.g. a duplicate key alongside the length error) so a skipped rule is visible — validate_table()'s returned list carries no `rule_results`; or test it through validate_table_detailed()'s `rule_results` — and retitle :200 ("…reports the stale rule as a violation instead of erroring internally").

### F-t-core-2-03  [nit] [doc-wrong]
- Where: tests/testthat/helper-validation-corpus.R:82-84
- What: The comment says the unicode maxLength case uses vowels "with combining marks", but the fixture uses precomposed code points ("äöüßé"); with real combining marks (e.g. "a\u0308") nchar() counts 2 characters per letter and the case would flip.
- Evidence: `# ... "aeiou" with combining marks is 5 characters but more than 5 bytes` vs `NAME = c("abcde", "äöüßé", "äöüßéx")`.
- Failure scenario: a maintainer "fixing" the fixture to match the comment (combining marks) turns row 2 into a 9-code-point value (ß has no decomposition) and breaks the snapshot for a reason unrelated to byte counting.
- Confidence: verified-by-reading
- Suggested direction: say "precomposed umlauts / ß / é (one code point, two UTF-8 bytes each)".

### F-t-core-2-04  [nit] [doc-wrong]
- Where: tests/testthat/test-validation-oracle.R:600-607
- What: The comment says every consumer of vc_corpus() writes it to CSV first, so the corpus "cannot say anything about a table that never passes a reader" — but this very file's direct-path tests (vc_details(), :47-53) hand the corpus to validate_table_detailed() without any reader, and the first consumer it names, test-columnspec-checks.R (:312-318), converts it with `arrow::as_arrow_table()` without any CSV either.
- Evidence: `every consumer of that corpus -- here, test-columnspec-checks.R and test-streaming-validation.R -- writes it to a CSV before checking it.` vs `vc_details <- function(case) validate_table_detailed(specs = case$specs, table = case$table, ...)` and test-columnspec-checks.R `dta_as_batch_reader(arrow::as_arrow_table(case$table), batch_rows = 1L)`.
- Failure scenario: misleading rationale only.
- Confidence: verified-by-reading
- Suggested direction: drop "every consumer ... writes it to a CSV"; say which consumers use a reader and which do not.

### F-t-core-2-05  [minor] [test-weak]
- Where: tests/testthat/test-regex-limits.R:40-46 (code: R/DTAFile-class.R:431)
- What: "more than three wildcards is refused" is tested with 5 stars (refused) and 3 stars (accepted); the boundary value 4 is never exercised.
- Evidence: test: `DTAFileCSV("x.csv", container = "*a*a*a*a*.zip")` (5 `*`) / `"*a*a*.zip"` (3 `*`); code: `if (nchar(gsub("[^*]", "", value)) > 3L) {`.
- Failure scenario: mutation `> 3L` -> `> 4L` (four wildcards accepted, contradicting the error text "more than three") passes both assertions.
- Confidence: verified-by-reading
- Suggested direction: add a 4-star glob that must be refused (class `dta_container_invalid`).

### F-t-core-2-06  [minor] [test-weak]
- Where: tests/testthat/test-validation-oracle.R:133-141, :494-533 (snapshot tests/testthat/_snaps/validation-oracle.md:181-249)
- What: The section header says the hand-derived companions "assert the same axis facts and error attribution against values worked out by hand", but on the read path 12 of the 17 corpus verdict rows are pinned only by a captured snapshot, and the "already-typed" companion checks only the three validity flags, never the counts.
- Evidence: `# The tests below assert the same axis facts and error attribution against values worked out by hand` / companion `expected <- list(clean = …, rule_range = …, columnspec_maxlength = …)` compares `columnspec_valid`, `rules_valid`, `import_valid` only; counts appear only in the snapshot (`"value": [0, 2, 1, 1, 1, …]`). Direct-path cases columnspec_maxlength_unicode, columnspec_const, rule_unique_na and all_axes likewise have no hand-derived companion in this file (const/unique-NA/unicode are pinned elsewhere: test-clinical-error-fixtures.R:36, test-evaluateRules.R:1010, OQ-VAL-005).
- Failure scenario: a reader change that, say, reports a missing required column twice per row on the read path, or drops the duplicate-NA uniqueness hit after CSV round trip (NA written as "" and read back as ""), changes only the snapshot; `snapshot_accept()` blesses it — the file's own header names exactly this risk. (I hand-checked the current snapshot values; they are correct today.)
- Confidence: verified-by-reading
- Suggested direction: extend the read-path companion to counts for all 17 cases (they are derivable by hand, as the direct-path comments show), or state explicitly which cases remain snapshot-only.

## test-DTAColumnSpec.R, test-DTAColumnSpecStructure.R, test-DTAColumnSpecStructureSAS.R

### F-t-core-2-07  [minor] [test-cheat]
- Where: tests/testthat/test-DTAColumnSpec.R:237-247 (code: R/DTAColumnSpec-class.R:721-730)
- What: "a typeless spec's enum matches both character and numeric data" asserts with `%in%`, which coerces both sides to a common type, so it passes whether the enum is kept un-coerced, coerced to character, or coerced to numeric — it cannot detect the very regression its comment describes.
- Evidence: `expect_true(all(c("1", "2") %in% schema$enum))` / `expect_true(all(c(1, 2) %in% schema$enum))`; in R `c(1, 2) %in% c("1", "2")` and `c("1", "2") %in% c(1, 2)` are both TRUE (match() coerces to character). The code comment at R/DTAColumnSpec-class.R:724-729 itself relies on that same coercion.
- Failure scenario: mutation at R/DTAColumnSpec-class.R:730 `values_flat` -> `as.character(values_flat)` (or `as.numeric(...)`) — "coercing the declared values to one R type", which the test comment says it guards — passes both assertions.
- Confidence: verified-by-reading
- Suggested direction: assert the representation (`expect_identical(schema$enum, c(1, 2))`) and/or validate a character column c("1","2") and a numeric column c(1,2) end to end against the typeless spec, with a "3" that must fail.

### F-t-core-2-08  [minor] [test-weak]
- Where: tests/testthat/test-DTAColumnSpec.R:24-30
- What: Copy-paste slip: the VISIT block asserts `spec[[1]]`'s structure again instead of `spec[[2]]`'s, so example 2's type/backend are never checked (examples 3 and 5 are checked only for class).
- Evidence: `expect_equal(spec[[2]]@label, "Visit")` / `expect_equal(spec[[1]]@structure@type, "Char")` / `expect_equal(spec[[1]]@structure@backend, "SAS")` / `expect_equal(spec[[2]]@description, "Visit code")`.
- Failure scenario: changing create_example_DTAColumnSpec(2) (R/DTAColumnSpec-class.R:407-416) to `type = "SAS Num"` passes this test.
- Confidence: verified-by-reading
- Suggested direction: `spec[[2]]` on lines 26-27.

### F-t-core-2-09  [nit] [doc-wrong]
- Where: tests/testthat/test-DTAColumnSpec.R:160-164
- What: The comment says the id line renders only when `label` is set; the print method prints the id unconditionally, and the test at :378-381 asserts exactly that.
- Evidence: comment `the id/label lines both render only when \`label\` is set (see the guard on the "id" alert ...)` vs R/DTAColumnSpec-class.R:460-462 `if (!is.null(x@id)) { cli_alert("id         : {.field {x@id}}") }`.
- Failure scenario: misleading comment only.
- Confidence: verified-by-reading
- Suggested direction: delete the stale sentence.

### F-t-core-2-10  [nit] [inconsistency]
- Where: tests/testthat/test-DTAColumnSpec.R:106-129 vs R/DTAColumnSpec-class.R:338-340
- What: The test calls get_arrow_type()'s NA for SAS Date/Time/DateTime a "documented gap" with "desired values" date32/time32/timestamp ("Pinned, not asserted"), while the function's roxygen documents NA as the contract ("The temporal types of either backend (they are validated as text) have no Arrow type"). It is a pinned defect without the repo's `KNOWN DEFECT, pinned rather than endorsed` marker, or it is not a defect — the two texts cannot both be right.
- Evidence: test `# Pinned, not asserted: the desired values ("date32"/"time32"/"timestamp") require adding the branches` vs roxygen `The temporal types of either backend ... have no Arrow type and give NA_character_`.
- Failure scenario: a reader searching for pinned defects ([assistant-tool].md's documented search string) never finds this one; or a "fix" is made against the documented contract.
- Confidence: verified-by-reading
- Suggested direction: decide which it is; either mark it with the sanctioned KNOWN DEFECT comment or drop "gap/desired values" from the test.

### F-t-core-2-11  [nit] [test-weak]
- Where: tests/testthat/test-DTAColumnSpecStructure.R:69-74
- What: The length assertion is vacuous: "12" also occurs in the format line "$12.", so dropping the length line from print_info() still passes.
- Evidence: `x <- DTAColumnSpecStructure(type = "Char", format = "$12.", length = 12, ...)` / `expect_true(any(grepl("12", info, fixed = TRUE)))`; R/DTAColumnSpecStructure-class.R:139 prints `format     : {x@format}`.
- Failure scenario: deleting R/DTAColumnSpecStructure-class.R:141-143 (the length alert) passes the test.
- Confidence: verified-by-reading
- Suggested direction: grep for "length     : 12" or use a length that does not occur in the format.

### F-t-core-2-12  [minor] [test-weak]
- Where: tests/testthat/test-DTAColumnSpecStructure.R:187-215
- What: The bundled-example YAML round trip compares only id, label, nullable, structure and values; description, pattern and examples are never compared, and the stated reason for not comparing whole objects is stale (values are normalised to an atomic vector on construction now).
- Evidence: `# ... @values is stored as a list but read back from YAML as a character vector, so the two collections are not identical() as whole objects.` vs test-DTAColumnSpec.R:19-22 (`@values is normalised to an atomic vector on construction`) and R/DTAColumnSpec-class.R:175 `values <- dta_normalise_spec_values(values)`. The example columns carry `description` (all four) and `pattern` (AGE).
- Failure scenario: write_columns_to_yaml()/as.list() dropping `description` or `pattern` passes this test (the sibling test at :157-185 uses columns that have neither).
- Confidence: verified-by-reading for the omissions; inferred that a whole-object `expect_equal(back, original)` would now pass (not run).
- Suggested direction: compare the whole collection with `expect_equal(back, original)` and drop the stale comment.

### F-t-core-2-13  [minor] [test-weak]
- Where: tests/testthat/test-DTAColumnSpecStructureSAS.R:197-222 and :248-266 (also :224-229 comment)
- What: "preserves mixed typed values during conversion" and "chunked path preserves values without mutation" assert `expect_identical(validate_table(...), original)`, which is true by construction whenever validation passes: validate_table() returns its own unmodified argument (R/validationFunctions.R:97 `table`) and does no conversion; there is no chunked path any more (6001 rows was chosen to cross a 5000-row chunk that was removed). The vctrs_unspecified test's comment still describes jsonlite::toJSON()/asJSON serialisation that no longer happens.
- Evidence: `n <- 6001` / `expect_identical(validated, original)`; R/validationFunctions.R:184-187 ("The chunking ... Nothing is serialised now"); test comment `jsonlite::toJSON() has no asJSON method for that class ... must instead be serialised as JSON null`.
- Failure scenario: the only thing these can catch is "validation now fails"; any "conversion"/"mutation" claim in the titles is untestable here because R's copy-on-modify makes the caller's frame immune. Not wrong, just mis-titled and stale.
- Confidence: verified-by-reading
- Suggested direction: retitle to "… validates clean" and assert `validate_table_detailed()`'s verdict; drop the chunk and JSON rationale.

## test-DTARuleGroupCondition.R (no finding), test-DTAMetaData-template.R

### F-t-core-2-14  [nit] [doc-wrong]
- Where: tests/testthat/test-DTAMetaData-template.R:111-115
- What: The "verified by reading" comment cites a line range of the app helper that no longer holds it.
- Evidence: `# Read inst/shiny/dta_app/R/utils_dta.R:1577-1584: the function checks inherits(x, "Date") first ...` — `.dta_stringify_dates` is defined at inst/shiny/dta_app/R/utils_dta.R:2974-2982.
- Failure scenario: a reviewer following the citation reads unrelated code.
- Confidence: verified-by-reading
- Suggested direction: cite the function by name, not by line numbers.

## test-final-review-fixes.R, test-validateTemplate-app-config.R (no finding in the latter)

### F-t-core-2-15  [minor] [test-weak]
- Where: tests/testthat/test-final-review-fixes.R:138-144
- What: "a fail_fast scan of a real dataset reports the unsettled import axis in its own words" asserts only an absence (no "predates import checking"); it never asserts that the scan stopped early or that the "scan stopped early" wording is present.
- Evidence: `d <- check(d, quiet = TRUE, persist = FALSE, fail_fast = TRUE)` / `expect_false(any(grepl("predates import checking", m$message, fixed = TRUE)))` — nothing else.
- Failure scenario: (a) dta_import_messages_to_df() emitting no row at all for a partial scan, or (b) fail_fast not stopping early on this 3-row file (so the axis is settled and the plain import finding is shown) — both pass. The positive wording is checked only on a hand-built `details` list in the previous test (:125-136), never on a real fail_fast scan.
- Confidence: verified-by-reading (test body); did not trace whether this input actually yields `partial_scan` today.
- Suggested direction: assert `attr(details, "partial_scan")` (or the import-axis NA) and `expect_match(m$message, "scan stopped early")` on the real scan.

### F-t-core-2-16  [nit] [test-weak]
- Where: tests/testthat/test-final-review-fixes.R:99-102 and :126-127
- What: (a) The HTML-report check is negative-only: it proves "(missing / not present)" is absent but not that the label finding's actual value ("bad") is rendered; (b) the partial-scan unit test writes two fixture files it never uses.
- Evidence: `expect_false(grepl("(missing / not present)", html, fixed = TRUE))`; `tab <- fr_write(c("ID,X", "r1,2", "r2,3"), "t.csv")` / `mat <- fr_write(...)` unused in :125-136.
- Failure scenario: a report that drops the actual-value column (or every finding) passes (a).
- Confidence: verified-by-reading
- Suggested direction: assert the rendered cell holds "bad"; delete the unused writes.

## test-gzip-input.R (no finding), test-sas-temporal-text.R (no finding: every expected value re-derived by hand, all correct), test-formattingHelpers.R

### F-t-core-2-17  [minor] [bug]
- Where: R/formattingHelpers.R:351-363 (pinned by tests/testthat/test-formattingHelpers.R:21-33, :50-66)
- What: .format_scalar_value() breaks its own documented contract ("Missing values ... NA of any type ... all render as MISSING_VALUE_DISPLAY"; the test: "The bare text 'NA' must never reach a rendered document") for anything that is not a scalar, and the tests only probe scalars.
- Evidence: `if (length(val) == 1 && is.na(val)) return(MISSING_VALUE_DISPLAY)` (scalar only) / `if (is.logical(val)) return(if (isTRUE(val)) "Yes" else "No")` / `if (length(val) > 1) val <- paste(as.character(val), collapse = ", ")` / `if (is.list(val)) { ... if (is.null(nms) || !any(nzchar(nms))) return(MISSING_VALUE_DISPLAY); return(paste(nms, collapse = ", ")) }`.
- Failure scenario: `c("A", NA)` renders "A, NA"; `c(TRUE, TRUE)` renders "No" (isTRUE of a length-2 vector is FALSE); an unflattened `list("A", "B")` renders "(not specified)" — present data shown as absent; `list(a = 1, b = 2)` renders "a, b" (values discarded; the test pins this as expected).
- Confidence: verified-by-reading for the code paths; inferred (not traced) which callers can hand it a vector/list today — the code comment itself says lists "should rarely be hit".
- Suggested direction: handle NA/"" element-wise in the vector branch, apply the logical mapping element-wise, abort (cli) on an unflattened list instead of rendering it as missing or as its names; add vector/list cases to the test.

### F-t-core-2-18  [minor] [test-weak]
- Where: tests/testthat/test-formattingHelpers.R:27-33 (code R/formattingHelpers.R:363)
- What: The test pins scientific notation as the rendering contract and frames it as affecting only "large numerics" (1e15), but `as.character()` already switches at 1e5 and for small fractions, so ordinary agreement values print as "1e+05" or "1e-04"; this is a document-correctness defect pinned as intended behaviour, without the repo's KNOWN DEFECT marker.
- Evidence: `# as.character() decides the numeric rendering, so a value that a DTA author typed as 1000000000000000 is shown as "1e+15"` / `expect_equal(.format_scalar_value(1e15), "1e+15")`; code `val <- as.character(val)`.
- Failure scenario: a metadata or rule value of 100000 is exported to Word/Markdown as "1e+05" (R's as.character(100000) is "1e+05"); no test notices because only 1000, 1e14 and 1e15 are probed.
- Confidence: verified-by-reading for the code; R's as.character() switch-over point inferred from documented R behaviour (not run).
- Suggested direction: render numbers with `format(x, scientific = FALSE, trim = TRUE, drop0trailing = TRUE)` (or the package's own number formatter used by .rule_text, which already prints "0.123456789"/"99999.999"), and pin 1e5/1e-4.

### F-t-core-2-19  [nit] [doc-wrong]
- Where: tests/testthat/test-formattingHelpers.R:22-23 and :36
- What: Stale line citations: "R/formattingHelpers.R:251-255" (list branch) and ":266-268" (truncation) point at unrelated code; the branches are at R/formattingHelpers.R:340-348 and :367-369.
- Evidence: `(callers are expected to flatten first, R/formattingHelpers.R:251-255)`; `# R/formattingHelpers.R:266-268: >80 chars becomes the first 77 plus "..."`.
- Failure scenario: misleading pointers only.
- Confidence: verified-by-reading
- Suggested direction: cite the function, not line numbers.

## test-inspect.R

### F-t-core-2-20  [minor] [test-weak]
- Where: tests/testthat/test-inspect.R:218-277
- What: The eager-vs-streamed inspect() comparison only compares the `context_*`/`failing_*` columns BOTH sides happen to have (`intersect()`), and skips silently when that intersection is empty, so a streamed path that stops returning row context passes; row context (`context_*`) has no positive assertion anywhere in tests/testthat.
- Evidence: `context_cols <- intersect(grep("^context_", names(eager_info), value = TRUE), grep("^context_", names(lazy_info), value = TRUE))` / `if (length(context_cols) > 0) { expect_equal(...) }`; grep for `context_`/`row_context` in tests/testthat finds only this test.
- Failure scenario: mutation in R/validationReporting.R:1363 `table_df <- if (has_row && !dta_table_is_lazy(raw_table)) ... else NULL` (lazy tables never get row context — the very regression the section header at :150-154 guards against) → lazy_info has no `context_*` columns → intersection empty → both columnspec and import comparisons are skipped → test passes. Making dta_build_inspect_row_context() return NULL on both paths also passes the whole file.
- Confidence: verified-by-reading
- Suggested direction: assert `expect_gt(length(context_cols), 0)` for columnspec/import messages and `length(preview_cols) > 1` for rule messages, and add one positive row-context assertion (e.g. `context_STATUS == "WRONG"` for the enum message).

### F-t-core-2-21  [nit] [test-weak]
- Where: tests/testthat/test-inspect.R:66-83 (and :92)
- What: "inspect() gives detailed rule context with failing rows" cannot see a failed rule reported with zero failing rows: `expect_gte(count, 0)` is true for any count, and `any(grepl("^failing_", names(info)))` is satisfied by the `failing_row_count` column itself. (The equality with messages() is pinned in test-validationReporting.R:289-348, so this is redundancy, not a coverage hole.) Separately, :92 calls `check(ds, quiet = TRUE)` with the default `persist = TRUE`, writing .rds artifacts into tempdir() — the undeclared state test-examples.R:17-18 says tests must avoid.
- Evidence: `expect_gte(info$failing_row_count[[1]], 0)` / `expect_true(any(grepl("^failing_", names(info))))`; R/DTADataSetFile-class.R:769 `persist = TRUE,`.
- Failure scenario: dta_rule_failure_row_indices() returning integer(0) for every rule passes this test.
- Confidence: verified-by-reading
- Suggested direction: `expect_gt(..., 0)` and require a `failing_` column other than the count; pass `persist = FALSE`.

### F-t-core-2-22  [minor] [design]
- Where: R/validationReporting.R:1113, :1361, :1381, :1452, :1457 (cross-slice; the fixtures in tests/testthat/test-inspect.R:166-181 are shaped around it)
- What: inspect()'s row context and failing-row preview hard-code the bundled clinical example's identifier columns `SUBJECT_ID` and `VISIT` as the identity of every row of every dataset.
- Evidence: `useful_cols <- c("SUBJECT_ID", "VISIT", msg_row$column)`; `row_cols <- unique(c("SUBJECT_ID", "VISIT", as.character(msg_row$column)))`; `preview_cols <- rule_def@group_by %||% c("SUBJECT_ID", "VISIT")`; `preview_cols <- c("SUBJECT_ID", "VISIT")`. inst/extdata/clinical_data.csv's header is exactly `"STUDYID","VISIT",...,"SUBJECT_ID"`, and test-inspect.R's stream specs declare exactly SUBJECT_ID and VISIT.
- Failure scenario: a CDISC delivery keyed by USUBJID/VISITNUM gets a row context showing only the offending column (no subject identifier) — the context the feature exists to provide — while every test, built on the example's column names, shows it working.
- Confidence: verified-by-reading
- Suggested direction: derive the identity columns from the specification (e.g. a declared key / the columns of a uniqueness rule / a spec-level `identifiers:`), not from the example data set.

## test-matrix-structure-engine.R (no finding), test-export-containers.R (no finding), test-exportAgreementMetadata.R (no finding), test-columnsSpecsFromWord.R

### F-t-core-2-23  [minor] [code-cheat]
- Where: R/DTAColumnSpecCollection-class.R:736-737 (cross-slice; tested only by tests/testthat/test-columnsSpecsFromWord.R:81-95)
- What: columns_specs_from_word() turns every non-blank Nullable cell it does not recognise as "yes" into `nullable = FALSE` without a word, so a German "Ja", "Optional", "N/A" or "1" silently makes the column required.
- Evidence: `nullable = if (is.null(nullable_cell)) NULL else grepl("^(yes|y|true)([^a-z]|$)", tolower(nullable_cell)),` — no "no/n/false" branch and no error; the test probes only "YES", "y", "Yes (if consented)", "No" and "".
- Failure scenario: a Word specification written with "Ja"/"Nein" (or "Optional") imports every column as non-nullable; every empty cell in the delivery then fails the column spec axis — a wrong specification produced from valid input, with no diagnostic. Mutation-wise: replacing the regex by `!grepl("^(no|n|false)", ...)` (unknown → TRUE) also passes the test.
- Confidence: verified-by-reading
- Suggested direction: accept an explicit yes-set and no-set and abort (class dta_word_import_error) on anything else; add an unrecognised-value test.

### F-t-core-2-24  [minor] [bug]
- Where: R/exportFunctions.R:88-92 and R/DTAColumnSpecCollection-class.R:696-701 (round-trip tests tests/testthat/test-columnsSpecsFromWord.R:219-256, :408-431)
- What: export_specs_table() writes permitted values joined by "; " without quoting, and columns_specs_from_word() splits on ";" before unquoting, so any value that contains the separator is silently split into several values on the documented export -> import round trip; the round-trip tests use only separator-free values and their comparison (`wd_facts()`) leaves out `description` entirely.
- Evidence: export `paste0("\n#@values: ", paste(as.vector(unlist(values)), collapse = "; "))`; import `unquote(strsplit(values_line, value_sep, fixed = TRUE)[[1]])`; test helper `wd_facts <- function(spec) list(id =, label =, backend =, type =, format =, length =, nullable =, values =, pattern =)`.
- Failure scenario: `values = c("Yes; confirmed", "No")` exports as `#@values: Yes; confirmed; No` and reads back as three values `c("Yes", "confirmed", "No")`; a delivery holding "Yes; confirmed" then fails the enum check. Separately, export dropping the free-text description (keeping only the marker lines) would pass both round-trip tests.
- Confidence: verified-by-reading
- Suggested direction: quote values on export and split only outside quotes on import (or refuse a value containing `value_sep` on export); include `description` in `wd_facts()` and a separator-bearing value in the round trip.

### F-t-core-2-25  [nit] [doc-wrong]
- Where: R/DTAColumnSpecCollection-class.R:580-582 (roxygen of columns_specs_from_word)
- What: The roxygen says the markers "and everything after them" are removed from the stored description, but the code removes only the rest of each marker's own line; text on later lines is kept (that is what test-columnsSpecsFromWord.R:58-64's fixture "Arm\n#@pattern: ^[A-Z]+$\nsecond line of text" relies on, though the test never asserts the stored description).
- Evidence: `Both markers, and everything after them, are removed from the stored description.` vs `description <- trimws(gsub("[\r\n]*#@(values|pattern):[^\r\n]*", "", description))`.
- Failure scenario: a reader expecting trailing notes after a marker to be dropped finds them in the imported description.
- Confidence: verified-by-reading
- Suggested direction: "the marker lines are removed"; assert `@description == "Arm\nsecond line of text"` in the :58 test.

## test-columnspec-format-r.R (KNOWN DEFECT at :197-199 verified still real: R/columnSpecChecks.R:276-277 skips Date/POSIXt but not difftime in the SAS branch, while :329 skips all three in the R branch)

### F-t-core-2-26  [nit] [test-weak]
- Where: tests/testthat/test-columnspec-format-r.R:37-41
- What: rf_expect() compares each engine's findings with `expect_setequal()`, which ignores multiplicity, so an engine that reports the same cell twice (e.g. at the batch_rows = 2 boundary it deliberately uses) still passes; only the R Date case at :406-430 compares whole frames.
- Evidence: `expect_setequal(rf_pairs(found$eager), expected)` / `expect_setequal(rf_pairs(found$streamed), expected)` with `rf_pairs()` = `paste(err$row, err$keyword)`.
- Failure scenario: the streamed path emitting row 3's maxLength finding twice (once per overlapping batch) passes "R character is checked for its length"; the n_columnspec_errors count would be off by one unnoticed here.
- Confidence: verified-by-reading
- Suggested direction: compare sorted vectors with `expect_identical(sort(...), sort(expected))`.

## test-container-findings.R

### F-t-core-2-27  [minor] [dead-code]
- Where: tests/testthat/test-container-findings.R:287-334 and :495-509 (they pin R/containerAssignments.R:112-253 `dta_is_stale()`/`dta_upgrade()`/`upgrade_dta()`/`dta_is_old_matrix()`, R/DTAFile-class.R:437-439 and R/DTAFileTabular-class.R:384-390 — cross-slice)
- What: A whole compatibility layer for objects saved by older versions ("A restored 0.27.0 object always qualifies (no `containers`, no `provenance`)") is kept and tested — the owner's policy is no backwards compatibility before 1.0.0, so this is dead weight; two of its accessors also turn ANY error into a default instead of surfacing it.
- Evidence: test `# An object as an older version saved it: no \`containers\`, no \`provenance\`.` + `cf_age()` that strips properties to fake an old object; code `dta_handler_container <- function(h) tryCatch(h@container, error = function(e) NULL)` ("A handler restored from an autosave written before `container` existed ... reads as 'no container'"); `value <- tryCatch(S7::prop(handler, name), error = function(e) NULL)` ("a handler predating these properties").
- Failure scenario: besides the maintenance cost (every entry point pays `dta_is_stale()`), the two tryCatch accessors convert an unexpected error on a current object into "no container" (packaging requirement silently dropped from container_findings()) or into the UTF-8 default encoding (a latin1 delivery decoded wrongly) — the brief's "silent fallback" pattern, justified only by the compat case.
- Confidence: verified-by-reading (code and tests); the policy judgement is the owner's stated rule.
- Suggested direction: delete the stale-object upgrade path, its tests and the tryCatch accessors (read `h@container` / `S7::prop()` directly), or ask the owner to exempt it explicitly.

## test-exportTemplateDocx-regions.R (no finding), test-createTemplateRepo.R

### F-t-core-2-28  [nit] [test-weak]
- Where: tests/testthat/test-createTemplateRepo.R:115-128
- What: The title pins "the starter dataset template's STATUS column is bound via values_from:", but the body only checks that SOME column has a non-NULL `values_from`; neither the column nor the vocabulary it points at is checked.
- Evidence: `bindings <- Filter(function(col) !is.null(col$values_from), columns)` / `expect_gt(length(bindings), 0)`.
- Failure scenario: the skeleton moving `values_from` from STATUS to another column, or pointing it at a vocabulary id that is not the starter one, passes this test (the latter only if validate_template() tolerates it).
- Confidence: verified-by-reading
- Suggested direction: select the STATUS column by id and assert its `values_from` equals the starter vocabulary's id.

## test-validateTemplate.R

### F-t-core-2-29  [minor] [inconsistency]
- Where: tests/testthat/test-validateTemplate.R:3028-3059 (code: R/validateTemplate.R:739-757 via inst/shiny/dta_app/R/party_profiles.R:173 and vocabulary.R:693)
- What: A party or vocabulary slot written without an id is silently dropped by the engine, and validate_template() reports it only as an R warning — the test pins that the RESULT holds zero rows — so `validate_template(strict = TRUE)` and the scaffolded CI linter pass a template whose slots the app will ignore.
- Evidence: test `expect_equal(nrow(result), 0, ...)` / `expect_match(warned, "Dropping 1 party slot with a missing or empty id", ...)`; the same file's own design rule at :1513-1524: "Both helpers ... are documented as reporting failure through their return value ... so a validator built to return findings also emitted one [a warning] ... where nothing could match on it."
- Failure scenario: an author writes `party_slots: [{target: metadata.supplier}]` (id forgotten); CI runs `validate_template(dir, strict = TRUE)` or the scaffold's `lint_template_repo(".")`, which returns 0 rows and exits 0 (the warning scrolls past) — even with `fail_on = "warning"`, since lint_template_repo() counts rows only (R/lintTemplateRepo.R:59-80); the created DTA has no supplier slot at all.
- Confidence: verified-by-reading (test, validator, engine warn sites, and lint_template_repo(), which calls validate_template() without any condition handler and fails only on finding rows)
- Suggested direction: turn the dropped-slot condition into a `party_slot_invalid`/`vocab_slot_invalid` row (error, since the slot is lost) and change the test to expect that row.

### F-t-core-2-30  [minor] [design]
- Where: tests/testthat/test-validateTemplate.R:2514-2529 (code: R/00_helpers.R:718-725)
- What: The test enshrines that a dataset may declare `file:` instead of `files:` because "DTADataSetFactory() takes it by partial argument matching" — an undocumented key that works only through R's partial matching of a named argument placed before `...`.
- Evidence: test `# \`file:\` still builds a dataset with one file (DTADataSetFactory() takes it by partial argument matching), so reporting it would be a false alarm.`; code `DTADataSetFactory <- function(type, columns = NULL, rules = NULL, files = NULL, matrix = NULL, row_labels = NULL, ...)`.
- Failure scenario: adding any other argument whose name starts with "file" before `...` (e.g. `file_count`) makes `file:` ambiguous and every such template breaks; meanwhile the validator cannot tell an author that the documented key is `files:`. (Also nearby in the same function, cross-slice: R/00_helpers.R:727-730 says "check that type is not NULL" but does not, so `type: ~` dies on `if (logical(0))`, and the message reads `'{type}'must` with a missing space.)
- Confidence: verified-by-reading
- Suggested direction: reject (or normalise explicitly, with a lint warning) `file:`; never rely on partial matching for a YAML key.

## test-exportDocuments.R

### F-t-core-2-31  [nit] [test-weak]
- Where: tests/testthat/test-exportDocuments.R:424-427, :1218-1223, :2079-2088, :1170-1180 (same pattern test-formattingHelpers.R:103-107)
- What: Four small weaknesses in an otherwise strong file: (a) `length(idx) == 1` after `which(...)[1]` is always TRUE (a missing heading gives NA of length 1; only the following `<` comparison catches it); (b) the comment "Each authorized signatory of the fixture gets exactly one row" is checked with `any(grepl(nm, ...))` — presence, not exactly one — and the expected names come from the exporter's own `.extract_signatories()`; (c) the file redefines the shared helper `.docx_text()` (helper-docx.R:69, signature `(path)`) as `.docx_text(dta)` with a different meaning; (d) the "any LC_TIME" tests switch to a non-English locale only if one of four names is installed and silently run under the current locale otherwise, so on a CI image without de/fr locales the "Januar|janvier" negative check proves nothing.
- Evidence: `idx_process <- heading1$doc_index[which(heading1$text == "Process Information")[1]]` / `expect_true(length(idx_process) == 1 && length(idx_datasets) == 1)`; `sig <- .extract_signatories(dta@metadata)` / `expect_true(any(grepl(nm, summary$text, fixed = TRUE)))`; `.docx_text <- function(dta) {`; `for (loc in c("de_DE.UTF-8", ...)) { if (nzchar(suppressWarnings(Sys.setlocale("LC_TIME", loc)))) break }`.
- Failure scenario: (b) a signatory rendered twice passes; (d) a regression to `format(date, "%d %B %Y")` passes on any runner lacking those locales.
- Confidence: verified-by-reading
- Suggested direction: (a) drop the vacuous check; (b) count rows; (c) rename the local helper; (d) record whether the switch succeeded and `skip()` with a reason (or fail on CI) when it did not.

## test-lintDTA.R

### F-t-core-2-32  [minor] [code-cheat]
- Where: R/DTARule-class.R:117-118 (pinned by tests/testthat/test-lintDTA.R:1011-1015)
- What: DTARuleFactory() — the constructor every YAML/list reader goes through — silently rewrites whitespace in a rule id to underscores, and lint_dta() reports nothing, so a specification's rule `my rule` is validated, reported and exported as `my_rule` without the author ever being told; the class itself (DTARule) refuses the same id.
- Evidence: `id <- if (!is.null(id)) { gsub("[[:space:]]+", "_", as.character(id)) } ...`; test `expect_identical(rule@id, "my_rule")` / `expect_identical(nrow(lint_rules(list(list(id = "my rule", ...)))), 0L)`; test-DTAColumnSpec.R:351-356 `DTARule(id = "my rule", ...)` -> error class `dta_rule_invalid_id`.
- Failure scenario: an author searching validation messages or the exported Word rules table for the id written in the YAML ("my rule") finds nothing; two rules "a b" and "a_b" in one specification collide only after the rewrite. The same file (R/DTARule-class.R:133-135) refuses partial-match abbreviations precisely so that nothing is "let through unnoticed" — the id rewrite is the unnoticed case.
- Confidence: verified-by-reading
- Suggested direction: make it a lint finding (warning `rule_id_rewritten`, or an error matching DTARule) instead of a silent gsub.

## test-load-file-streaming.R

### F-t-core-2-33  [minor] [test-weak]
- Where: tests/testthat/test-load-file-streaming.R:157-186 and :250-276 (fixture builder :16-22)
- What: The eager-vs-streamed equivalence tests claim the fixture "exercises all three axes at once", but the specs they validate with (`create_example_DTAColumnSpecCollection(1)`) declare no rules at all, so the rule axis is compared as 0 == 0; the column spec axis is mostly the structural mismatch between that 4-column example spec and the 13-column clinical file.
- Evidence: `specs = create_example_DTAColumnSpecCollection(1)`; R/DTAColumnSpecCollection-class.R:830-838 `` `1` = { example_rules <- list(); DTAColumnSpecCollection(columns = ..., rules = example_rules) } ``; test `# The all-errors fixture exercises all three axes at once`.
- Failure scenario: a streaming path that never evaluates rules (or evaluates them per batch wrongly) passes both tests, including "batch size changes how the file is scanned, not what is found". Rule parity is covered elsewhere (e.g. test-streaming-validation.R:371-375, the latin1 test in this file at :775-848 with a uniqueness rule), so this is a mis-stated test, not an uncovered axis.
- Confidence: verified-by-reading
- Suggested direction: load with `read_dta_from_yaml(clinical_dta.yaml)` specs (which carry rules and match the file), and assert `n_rule_errors > 0` as the non-vacuity guard.

### F-t-core-2-34  [minor] [test-weak]
- Where: tests/testthat/test-load-file-streaming.R:278-300
- What: "max_errors caps retained detail without changing the verdict" never checks that anything was capped: the only retention assertion is `capped <= uncapped`, which an ignored `max_errors` satisfies with equality.
- Evidence: `capped <- details_after_check(make(), table_name, max_errors = 3L)` / `expect_lte(nrow(capped_detail), nrow(uncapped_detail))`; check() accepts `max_errors` (R/DTADataSetTabular-class.R:1482).
- Failure scenario: check(DTADataSetTabular) dropping `max_errors` on its way to the streaming scan (always using the 10000 default) passes; so does a cap that keeps 4 or 400 rows.
- Confidence: verified-by-reading
- Suggested direction: assert `nrow(capped_detail) <= 3` (or the exact head plus a `spilled_rows`/`truncated` attribute) and that the uncapped frame is larger.

### F-t-core-2-35  [nit] [doc-wrong]
- Where: tests/testthat/test-load-file-streaming.R:1136-1140
- What: The "KNOWN LIMITATION" note names the wrong function: the classless refusal now lives in `dta_refuse_wide_encoding()` (R/DTAFileTabular-class.R:432-441), not `dta_transcode_to_utf8()`; the limitation itself (no condition class, against the repo's class-matching convention) is still real.
- Evidence: test `# KNOWN LIMITATION: the refusal in dta_transcode_to_utf8() is a plain cli_abort() with no condition class`; code `dta_refuse_wide_encoding <- function(encoding, path) { if (dta_encoding_is_wide(encoding)) { cli::cli_abort(c("A file declaring {.val {encoding}} cannot be converted block by block.", ...)) } }`.
- Failure scenario: a maintainer adding the class looks in the wrong function.
- Confidence: verified-by-reading
- Suggested direction: give the abort a class (e.g. `dta_encoding_unsupported`) and switch the test to it.

## test-columnspec-checks.R

### F-t-core-2-36  [major] [code-cheat]
- Where: R/columnSpecChecks.R:617-631 (pinned by tests/testthat/test-columnspec-checks.R:928-950)
- What: When `as_json_schema()` fails for a column, dta_compile_columnspec_schemas() catches the error, warns, and returns a NULL schema; every value check of that column (type, length, values, pattern) is then skipped, and because the verdict is computed only from the findings that were produced, the table can come back `ok = TRUE` for data that was never examined. The test pins the warning and the NULL schema and never looks at the verdict.
- Evidence: code `schema <- tryCatch(as_json_schema(columns[[i]]), error = function(e) { cli::cli_warn("Column {.val {column_name}} could not be compiled into a schema; its value checks are skipped.", class = "dta_columnspec_schema_failed"); NULL })` — its own comment: "worth a warning rather than a table that quietly reports 'ok' on data it never actually examined" — yet R/validationFunctions.R:235/357 derive `columnspec_valid` solely from `nrow(full_error) > 0`. Test: `expect_length(warnings, 1)` / `expect_null(schemas[[1]]$schema)` only.
- Failure scenario: any future defect in a schema method (today reachable only by assigning a bare `DTAColumnSpecStructure`, as the test does) turns into "Validation PASSED" plus one warning that a CI log or the Shiny app easily drops; the column's values are not checked at all. Mutation-wise: deleting the `cli_warn()` call passes everything except this test, and nothing anywhere asserts the verdict for this case.
- Confidence: verified-by-reading for the catch/skip and the verdict computation; inferred (not traced in full) that dta_columnspec_check_summary() then omits the column rather than marking a check `not_checked`.
- Suggested direction: abort (a spec that cannot be compiled cannot be validated against), or at least force the column's checks to `not_checked` and `ok = NA/FALSE`; assert the verdict in the test.

### F-t-core-2-37  [minor] [dead-code]
- Where: tests/testthat/helper-unchecked-collection.R:1-14, tests/testthat/test-validationFunctions.R:1-4, tests/testthat/test-columnspec-checks.R:1-4; also `vc_specs_unchecked()` in tests/testthat/test-streaming-scale.R:287-378 and :519-548 ("rules may read undeclared columns") (code kept alive: R/streamingValidation.R:2558-2617, R/columnSpecChecks.R:594-613, R/validationFunctions.R:272)
- What: Both files shadow `DTAColumnSpecCollection` with a validator-bypassing constructor "to test how the engine copes with rules that contradict their columns (stale, restored or legacy specs)", which keeps alive engine branches the code itself labels "reachable only by validator bypass" — compatibility defence the owner's policy calls dead weight; in test-columnspec-checks.R the header is copied verbatim although that file is about per-check reporting and only one test (:664-693) needs a contradicting collection.
- Evidence: test header `# This file tests how the engine copes with rules that contradict their columns (stale, restored or legacy specs). The class refuses to build them, so the constructor is shadowed by the rule-unchecked variant for the whole file.`; code `# Per-column fallback ... a PARTIALLY named collection (reachable only by validator bypass)` (R/columnSpecChecks.R:599-601, R/streamingValidation.R:2562-2565); `described <- union(described, as.character(cols))` (R/streamingValidation.R:2615) only changes a result when a rule names an undeclared column next to declared ones, which the class validator refuses (`rule_column_undeclared` is an error, R/DTAColumnSpecCollection-class.R:100-109); `rules_list <- tryCatch(specs@rules, error = function(e) NULL)` (R/validationFunctions.R:272) runs after line 139 has already guaranteed a DTAColumnSpecCollection.
- Failure scenario: the defensive `tryCatch(..., error = function(e) NULL/NA)` accessors also swallow genuine errors on valid objects (a renamed property would read as "no rules"/"no columns" and validate clean); and every file in the slice that uses the shadow exercises the real constructor in none of its other tests.
- Confidence: verified-by-reading (all cited lines); the "dead weight" judgement is the owner's stated policy.
- Suggested direction: drop the bypass states and their branches, read `specs@rules`/`@columns` directly, and use the real constructor in both files.

## test-exportValidationReport.R

### F-t-core-2-38  [minor] [test-cheat]
- Where: tests/testthat/test-exportValidationReport.R:224-240 (code R/exportValidationReport.R:69-90, 206-238)
- What: The test for the `title = NA_character_` fallback asserts that the `<h1>` is non-empty, but the `<h1>` holds the document name ("Data Transfer Agreement"), not the title, so it is non-empty whatever the fallback does; the title goes to `<title>` and `.report-subtitle`, which the test never reads. The code comment repeats the stale claim that the title feeds the `<h1>`.
- Evidence: test `h1_text <- xml2::xml_text(xml2::xml_find_first(doc, "//h1"))` / `expect_true(nzchar(h1_text))`; code comment `... a title = NA_character_ caller would skip the fallback entirely and end up with an empty <title>/<h1>` vs `<title>{escaped_title}</title>` and `<div class="report-subtitle">{escaped_title}</div>` (the h1 is the document name, as the test at :41-44 itself shows).
- Failure scenario: mutation deleting `is.na(title) ||` at R/exportValidationReport.R:74 — the exact bug the test is named for — leaves `title` NA, `.report_html_escape(NA)` gives "" (pinned at :146-149), so `<title>` and the subtitle are empty; the test still passes.
- Confidence: verified-by-reading
- Suggested direction: assert `//title` and `.report-subtitle` equal "Example DTA"; fix the comment.

### F-t-core-2-39  [minor] [code-cheat]
- Where: R/exportValidationReport.R:116-132 (pinned by tests/testthat/test-exportValidationReport.R:838-870)
- What: write_validation_report() swallows every error inspect() raises — first for all ids, then per dataset group with `error = function(e2) NULL` — without a warning or the error text; the affected panels just read "Detail could not be generated", and the test (which fakes the failure because "No faithful real failure can be built") pins this silent degradation.
- Evidence: `inspect_all <- tryCatch(DTAtools::inspect(x, id = msgs$id, as_tibble = FALSE), error = function(e) { ... parts <- lapply(groups, function(ids) { tryCatch(DTAtools::inspect(x, id = ids, as_tibble = FALSE), error = function(e2) NULL) }) ... })`; neither `e` nor `e2` is reported anywhere.
- Failure scenario: a real defect in inspect() (e.g. a renamed column in one dataset's error frame) produces a report that looks complete — verdicts and messages present — while every panel of that dataset says only "Detail could not be generated"; the user, CI and the developer never see the underlying error. The brief's "tryCatch returning NULL that swallows a real failure" pattern.
- Confidence: verified-by-reading (the catch at :116-132; grep for `cli_warn`/`warning(` in R/exportValidationReport.R and R/reportRendering.R finds none, so the error is not surfaced later either)
- Suggested direction: keep the isolation but `cli::cli_warn()` (classed) with the dataset and `conditionMessage(e2)`, and assert that warning in the test.

### F-t-core-2-40  [nit] [test-weak]
- Where: tests/testthat/test-exportValidationReport.R:309-329 (code R/reportRendering.R:849-881)
- What: The test is titled ".report_columnspec_expected_text covers every columnspec keyword the validator emits" but probes only required, type, maxLength, const and pattern; the `propertyNames`, `minProperties` and `maxItems` branches are asserted nowhere in tests/testthat or the qualification suite (grep for their texts finds nothing).
- Evidence: `expect_match(.report_columnspec_expected_text(row_for("required", ...` ... `row_for("pattern", ...))` only; code `propertyNames = "Every data-column label must satisfy the rule declared under matrix.columns."`, `minProperties = paste0("The matrix has fewer data columns ...")`, `maxItems = paste0("The matrix has more data rows ...")`.
- Failure scenario: deleting (or swapping the min/max wording of) those three branches falls back to the validator's raw message or inverts "fewer"/"more" in the report; no test fails.
- Confidence: verified-by-reading
- Suggested direction: loop over `dta_columnspec_check_kinds()$keyword` and assert each has its own non-fallback text.

## test-containerFiles.R (expected values hand-derived from fixture bytes and offsets throughout; one nit)

### F-t-core-2-41  [nit] [dead-code]
- Where: R/containerFiles.R:265-271 (pinned by tests/testthat/test-containerFiles.R:314-321)
- What: dta_zip_inspect() checks at run time whether the installed zip package is older than 3.0.0, but DESCRIPTION already requires `zip (>= 3.0.0)`, so the branch is unreachable; the test reaches it only by mocking `zip::zip_list()` to return an old-shaped frame.
- Evidence: DESCRIPTION:41 `zip (>= 3.0.0),`; code `if (!all(c("filename", "uncompressed_size", "type", "encryption") %in% names(l))) { reason <- "The installed zip package is too old to inspect archives safely; zip 3.0.0 or later is required." ...`; test `testthat::local_mocked_bindings(zip_list = function(...) data.frame(filename = "ae.csv", uncompressed_size = 53), .package = "zip")`.
- Failure scenario: none at run time; if a future zip changed its column names the message would wrongly blame an "old" package that is new.
- Confidence: verified-by-reading
- Suggested direction: drop the branch and its test, or reword it as an unexpected-shape abort without the version claim.

## test-streaming-scale.R (no new finding; its `vc_specs_unchecked()` uses are folded into F-t-core-2-37; the randomised Arrow/per-batch/eager differential at :168-250 is guarded against never taking the Arrow path)

---

## Summary

41 findings: 1 major, 24 minor, 16 nit.
By category: test-weak 18, doc-wrong 8, code-cheat 4, dead-code 3, test-cheat 2, bug 2, design 2, inconsistency 2.
Cross-slice (the defect is in R/ or inst/, found through this slice's tests -- dedupe against the code reviewers): F-17, F-22, F-23, F-24, F-27, F-30, F-32, F-36, F-37, F-39, F-41.

Verified spot-checks that produced NO finding (recorded so they are not re-done):
- `_snaps/validation-oracle.md`: every row of all four snapshots re-derived by hand from the corpus (incl. the unicode, const, NA-uniqueness and all-axes cases and the 12 read-path rows); all correct today (the residual risk is F-06).
- KNOWN DEFECT at test-columnspec-format-r.R:197-199: still real (R/columnSpecChecks.R:276-277 vs :329), and the comment names the assertion to switch.
- test-sas-temporal-text.R: every SAS day/second number re-derived by hand (epoch, leap days, 1582-10-14, 9999-12-31, month starts); all correct.
- test-regex-limits.R: the trip-cap and per-name isolation expectations follow R/00_helpers.R:973-1015 exactly.
- test-containerFiles.R: byte offsets (409-416, 1120/2000/3120, 1280/2160/3280) consistent with the fixture layout described; expectations are literal fixture data.
- Severity split `file_name_conflict` warning (lint_dta) vs error (validate_template) is documented in the skeleton README, so not reported.

## Coverage

Fully read (every line): test-validateTemplate.R, test-exportDocuments.R, test-lintDTA.R,
test-load-file-streaming.R, test-columnspec-checks.R, test-exportValidationReport.R,
test-containerFiles.R, test-streaming-scale.R, test-createTemplateRepo.R,
test-validation-oracle.R (+ tests/testthat/_snaps/validation-oracle.md),
test-exportTemplateDocx-regions.R, test-container-findings.R, test-columnspec-format-r.R,
test-columnsSpecsFromWord.R, test-validationFunctions.R, test-DTAColumnSpec.R, test-inspect.R,
test-matrix-structure-engine.R, test-formattingHelpers.R, test-export-containers.R,
test-exportAgreementMetadata.R, test-DTAColumnSpecStructureSAS.R, test-DTAColumnSpecStructure.R,
test-sas-temporal-text.R, test-gzip-input.R, test-DTARuleGroupCondition.R,
test-validateTemplate-app-config.R, test-final-review-fixes.R, test-DTAMetaData-template.R,
test-clinical-multiple-files.R, test-clinical-file-dataset-example.R, test-examples.R,
test-regex-limits.R, test-rulePreview.R, test-DTAFileTSV.R, test-namespaceImports.R;
helpers helper-validation-corpus.R, helper-unchecked-collection.R, helper-source-tree.R,
helper-containers.R, helper-docx.R.

Read in part (only the functions the slice's tests target): R/validationFunctions.R (1-420),
R/columnSpecChecks.R (format branch, schema compile, check summary), R/validationReporting.R
(results/inspect paths), R/streamingValidation.R (dta_structure_findings), R/DTAColumnSpec-class.R,
R/DTAColumnSpecCollection-class.R (class, previews, columns_specs_from_word), R/DTARule*-class.R,
R/formattingHelpers.R (.format_scalar_value), R/exportValidationReport.R (1-163), R/reportRendering.R
(.report_columnspec_expected_text), R/containerAssignments.R (stale/upgrade), R/containerFiles.R
(dta_zip_inspect head), R/exportFunctions.R (export_specs_table description), R/00_helpers.R
(regex limits, DTADataSetFactory), R/validateTemplate.R (slot checks), app helpers named in findings.

Not read: tests/testthat/helper-shinyapp.R (used by two slice files via app_fn()/app_fixture_dta*,
out of slice); tests/testthat/fixtures/ (listed, not opened -- no slice file reads them).
