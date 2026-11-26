# Findings — c4-tmpl-export

Slice (paths relative to $SNAP):
- R/createTemplateRepo.R
- R/documentBuilders.R
- R/documentData.R
- R/exportDocuments.R
- R/exportFunctions.R
- R/exportMarkdown.R
- R/exportTemplateBlocks.R
- R/exportTemplateDocx.R
- R/exportTemplateRegions.R
- R/exportValidationReport.R
- R/lintTemplateRepo.R
- R/qualification-report.R
- R/qualification.R
- R/run_dta_app.R
- R/templateGuide-collect.R
- R/templateGuide.R
- R/validateTemplate.R
- plus inst/extdata/{template-repo-skeleton,agent-skills,library} as they interact with the code

Files fully read: all 17 R files of the slice (see Coverage at the end for the
supporting files read in full or in part, and what was not read).

## Area 1 — qualification runner (R/qualification.R, R/qualification-report.R)

Also read for context (not in any slice): inst/qualification/tests/helper-00-qa.R,
inst/qualification/tests/setup-qualification.R, requirements/_index.yaml,
.github/scripts/qualification_verdict.R, the run step of .github/workflows/qualification.yml.

### F-c4-tmpl-export-01  [blocker] [logic-error]
- Where: R/qualification.R:722-739 (qual_traceability); contradicted by R/qualification-report.R:157-159, inst/qualification/README.md:71-73, inst/qualification/docs/maintaining.md:268-270, docs/validation-plan.md:220-221
- What: A requirement is reported `verified` as soon as ANY one of its test cases passed, even when its other test cases were skipped or never executed, while every document that describes the matrix promises the opposite.
- Evidence:
  `} else if (any(mine$status %in% c("fail", "error", "file_error"))) { "failed"`
  `} else if (any(mine$status == "pass")) { "verified"`
  README: "the requirements it would have verified are marked not verified"; report: "`verified_with_deviation` when every test case tracing to it passed".
  A skipped TC only makes its requirement "not verified" when ALL TCs of that requirement skipped (155 of 577 requirements have >1 TC).
- Failure scenario: REQ-EXPORT-040 (byte-identical output also under another LC_CTYPE) traces to OQ-EXPORT-015/059/060/063/064/065. On a host without a UTF-8 LC_CTYPE, OQ-EXPORT-064 skips (its LC_CTYPE half is never exercised); without pandoc OQ-EXPORT-059 skips too. Section 7 and SUMMARY.txt still report REQ-EXPORT-040 `verified`, the "not verified" count stays 0, so acceptance criterion 5 of the validation plan ("any requirement left unverified ... is listed, and the reviewer accepts each one") is satisfied vacuously. Same for REQ-EXPORT-030 (OQ-EXPORT-041 skips when the bundled example is not installed).
- Confidence: verified-by-reading (traceability code, report text, README/maintaining/validation-plan wording; TC-to-REQ mapping extracted from the test titles)
- Mitigation that exists (why blocker and not worse): the overall verdict still reads "PASS WITH NOT-EXECUTED TESTS" and section 9 lists the skipped TC with its requirement ids; what is false is the per-requirement status and the "not verified" count the reviewer is told to judge.
- Suggested direction: `verified` only when every TC of the requirement passed; any skip/not_executed among them -> `not_verified` (or a new `partially_verified`), and align the four documents.

### F-c4-tmpl-export-02  [major] [bug]
- Where: R/qualification.R:1991-1997 with R/qualification-report.R:214-225; masked by tests/testthat/test-qualification.R:425-446
- What: The report is rendered before `x$run$hash_manifest` is set, so every real report's section 12 names no manifest and prints an incomplete verification command.
- Evidence:
  `report_paths <- qual_write_report(x, bundle_dir, formats)`   # line 1991
  `x$run$hash_manifest <- if (identical(qual_hash_algorithm(), "sha256")) {`   # line 1993, AFTER
  report: `sprintf("Verify with \`%ssum -c %s\` from the bundle directory.", x$run$hash_algorithm, x$run$hash_manifest %||% "")`
- Failure scenario: any run_qualification() -> report/qualification-report.md section 12 reads "`the hash manifest` lists every file ..." and "Verify with `sha256sum -c ` from the bundle directory." -- the signed document gives the auditor a command with no file argument. The developer test passes only because it hand-builds `run = list(hash_manifest = "SHA256SUMS", ...)`, a state run_qualification() never produces (mutation: moving line 1993 above 1991 or deleting it is undetected by both tests).
- Confidence: verified-by-reading
- Suggested direction: set `hash_manifest` (it is known up front from qual_hash_algorithm()) when `x$run` is built, and test the report produced by a real run_qualification() call.

### F-c4-tmpl-export-03  [major] [logic-error]
- Where: R/qualification.R:1358-1370, 1892-1901, 1925
- What: A run that executes no test at all (`stages = character()`, documented as a supported mode) gets the verdict "PASS (PARTIAL)", which print() shows as a green success and the CI gate accepts.
- Evidence:
  `verdict <- if (failed > 0 || meta_failed > 0 || stale > 0) { "FAIL" } else if (any(tests_df$status == "skip")) {...} else { "PASS" }`
  with tests_df empty every term is 0/FALSE -> "PASS"; `partial` adds "(PARTIAL)". The runner itself says (1890-1891) a run that selects nothing "must not read as a pass" but only guards the filter case.
- Failure scenario: `run_qualification(dir, stages = character())` -> SUMMARY.txt "Verdict : PASS (PARTIAL)", "Test cases : 0 executed"; `.github/scripts/qualification_verdict.R` only fails on a verdict starting with "FAIL", so such a bundle would pass that gate. Today no CI leg feeds a zero-stage run to the gate (qualification.yml runs all stages; the zero-stage callers are tests/testthat/test-qualification.R and OQ-CORE-001/002/0xx, which do not assert the verdict -- OQ-CORE-004's "PASS (PARTIAL)" case uses two passing tests), so the impact is on anyone scripting against the verdict string; no test pins the zero-test verdict either way.
- Confidence: verified-by-reading
- Suggested direction: a distinct verdict (e.g. "NOT EXECUTED") whenever nrow(tests_df) == 0, never one starting with "PASS".

### F-c4-tmpl-export-04  [minor] [bug]
- Where: R/qualification-report.R:535-537, 552-568; roxygen R/qualification.R:1761-1764
- What: Requesting PDF without DOCX never uses the documented PDF backend (`dta_pdf_backend()`), only xelatex/lualatex.
- Evidence: `rendered$pdf <- qual_render_pdf(md_path, rendered$docx)` — `rendered$docx` is NA unless "docx" was also requested; `usable_docx` is then FALSE and the Word/LibreOffice route is skipped. Roxygen: "PDF when a PDF backend is (see [dta_pdf_backend()])".
- Failure scenario: Windows host with Word but no TeX, `formats = c("md", "pdf")` -> no PDF; with `formats = c("md","docx","pdf")` the same host produces one.
- Confidence: verified-by-reading
- Suggested direction: render the docx as an intermediate whenever pdf is requested (or document the dependency on "docx").

### F-c4-tmpl-export-05  [minor] [dead-code]
- Where: R/qualification.R:521 (`api` of qual_requirements_df), 690 (`stages_run` of qual_traceability), 1706-1707
- What: Two parameters are never read; `qualification_requirements()` reads `_api.yaml` only to pass it into the ignored argument.
- Evidence: the bodies at 521-590 and 690-758 contain no reference to `api` / `stages_run`; callers at 1707, 1866, 1924 and in the tests pass them anyway.
- Failure scenario: readers assume the requirement schema is validated against the export list (it is not; M6 does that) and that traceability is scoped to the stages run (it is not; it uses all discovered TCs).
- Confidence: verified-by-reading
- Suggested direction: drop both parameters and the dead `qual_read_api()` call in qualification_requirements().

### F-c4-tmpl-export-06  [minor] [design]
- Where: R/qualification.R:421-429, 1358-1368; R/qualification-report.R:330-333
- What: A test case that raised uncaught warnings is a plain "pass"; the warnings reach neither the verdict nor the report's "Steps requiring attention".
- Evidence: status considers only error/failure/skip types; `n_warning` is counted but unused; stage detail expands only `fail/error/file_error`.
- Failure scenario: the software under test emits an unexpected warning (e.g. a coercion warning while validating) inside a qualification step -> report shows "Checks 5/6" with no explanation; the reviewer never sees the warning text unless they open expectations.csv.
- Confidence: verified-by-reading
- Suggested direction: list warning expectations in the stage detail (or make them a status of their own).

### F-c4-tmpl-export-07  [minor] [inconsistency]
- Where: R/qualification.R:1585-1591
- What: SUMMARY.txt labels the number of recorded rows as "executed", although that number includes skipped tests and stage-abort rows.
- Evidence: `"Test cases : %d executed, %d passed, %d failed, %d errored, %d not executed", nrow(x$tests), ..., sum(x$tests$status == "skip")`
- Failure scenario: 812 cases with 10 skipped -> "812 executed, 802 passed, 0 failed, 0 errored, 10 not executed" -- the line contradicts itself.
- Confidence: verified-by-reading
- Suggested direction: label it "recorded" or report nrow minus skips.

### F-c4-tmpl-export-08  [minor] [doc-wrong]
- Where: R/qualification-report.R:169-173 vs inst/qualification/tests/helper-00-qa.R:127-141 and R/qualification.R:1361
- What: The report says a `pending` deviation's run result "carries no verdict either way", but `qa_known_deviation()` accepts pending ids and `qual_verdict()` fails the run on any `not_reproduced` deviation, pending included.
- Evidence: helper: `if ((d$status %||% "open") %in% c("open", "pending"))`; runner: `stale <- sum(dev_status$status_run == "not_reproduced")` (no status filter).
- Failure scenario: a pending entry bound by a test whose defect no longer reproduces -> verdict FAIL, while section 8.1 tells the reviewer pending results carry no verdict.
- Confidence: verified-by-reading
- Suggested direction: make the text match (pending bindings do count) or filter `stale` to open entries.

### F-c4-tmpl-export-09  [minor] [test-weak]
- Where: R/qualification.R:1023-1039 (qual_discover_deviation_refs, feeds meta check M7)
- What: The "binding" id is the first string literal anywhere after the call in the file, not the call's first argument, so M7 can be satisfied by an unrelated string.
- Evidence: `following <- strings[strings$line1 > calls$line1[[i]] | (...same line, later column)]` — no bound on the call's closing parenthesis.
- Failure scenario: `qa_known_deviation(id_var, cond)` followed later in the file by any literal "DEV-018" (e.g. in a qa_step description) marks DEV-018 as bound; M7 passes although no call binds it. Not exploited today (all four calls use literals).
- Confidence: verified-by-reading (no current misuse found by grep)
- Suggested direction: take the STR_CONST whose parent is the call's argument list (getParseData parent ids), or require a literal first argument.

### F-c4-tmpl-export-10  [minor] [security]
- Where: R/qualification.R:528, 536-538, 563, 576-579
- What: Text taken from requirement YAML (ids, `area`, yaml parser messages) is passed to `cli::cli_abort()` as format strings, so braces in it are evaluated/parsed by cli.
- Evidence: `problems <- c(problems, sprintf("%s [%s]: %s", basename(f), id, paste(bad, collapse = "; ")))` ... `cli::cli_abort(c("...", stats::setNames(problems, rep("x", length(problems)))))`; `.cli_escape()` exists (R/exportDocuments.R:1332) but is not used here.
- Failure scenario: `qualification_requirements(dir = <suite>)` on a requirement with id `"{Sys.getenv('X')}"` evaluates the expression while building the error; a yaml message containing "{" replaces the schema error with a cli glue error.
- Confidence: verified-by-reading (input comes from a suite directory; the shipped files are trusted, `dir=` makes it caller-supplied)
- Suggested direction: wrap each problem in `.cli_escape()`.

### F-c4-tmpl-export-11  [minor] [inconsistency]
- Where: R/qualification.R:1277-1303 (child script of qual_unit_tests_run)
- What: The developer suite child never sets `NOT_CRAN`, so every `skip_on_cran()` test (18 calls in 6 files of tests/testthat) skips in the "supplementary evidence" and section 11 does not say why.
- Evidence: the script sets only `Sys.setenv(TESTTHAT_PARALLEL = "FALSE")`; Rscript is non-interactive, so `skip_on_cran()` skips unless NOT_CRAN is inherited.
- Failure scenario: report section 11 shows "Skipped: N" for checks (including the package-load guards in test-generic-guards.R that [assistant-tool].md calls the only way to observe a load failure) that never had a chance to run.
- Confidence: inferred (testthat's on_cran() semantics not re-read in this snapshot; counts verified by grep)
- Suggested direction: set NOT_CRAN=true in the child, or state it in section 11.

### F-c4-tmpl-export-12  [minor] [bug]
- Where: R/qualification.R:1820-1822, 297, 1842-1846
- What: `filter` is only type-checked; an invalid regular expression aborts the run with a base R (translated, unclassed) grepl error after the bundle directory and run.log were created, leaving a stub bundle.
- Evidence: validation `!is.character(filter) || length(filter) != 1 || is.na(filter)`; first use `nm[grepl(filter, nm)]` inside qual_run_stage, outside its tryCatch.
- Failure scenario: `run_qualification(out, filter = "OQ-(VAL")` -> "invalid regular expression" in the system language, `out/QUAL-.../run.log` left behind.
- Confidence: verified-by-reading
- Suggested direction: validate the regex up front with tryCatch(grepl(filter, "")) and cli_abort.

### F-c4-tmpl-export-13  [nit] [doc-missing]
- Where: R/qualification.R:1771-1773 (roxygen of include_unit_tests) vs 1216-1218
- What: The roxygen does not say that `"auto"` skips the developer suite at `scale = "quick"`.
- Evidence: `if (identical(mode, "auto") && identical(scale, "quick")) return(list(run = FALSE, reason = "skipped at scale = \"quick\""...`
- Failure scenario: a user running the documented example or a quick CI run expects supplementary evidence and gets none.
- Confidence: verified-by-reading
- Suggested direction: document the auto rule.

### F-c4-tmpl-export-14  [nit] [inconsistency]
- Where: R/qualification-report.R:41-47, 72-94
- What: `qual_md_kv()` formats NA as the literal "NA", so the signed report's header shows "Filter | NA" and "Source revision | NA" (while table cells render NA as empty).
- Evidence: `Value = vapply(pairs, function(x) paste(format(x), collapse = ", "), character(1))`; `x$run$filter` is `filter %||% NA_character_`.
- Failure scenario: every unfiltered run's report reads "Filter: NA".
- Confidence: verified-by-reading
- Suggested direction: map NA to "none"/"" before formatting.

### F-c4-tmpl-export-15  [nit] [inconsistency]
- Where: R/qualification.R:1505-1510 vs R/qualification-report.R:457-460
- What: Section 11 says nothing about the developer suite "contributes to the verdict", yet a failure to write `unit-tests.csv` marks the bundle incomplete and turns the verdict into FAIL.
- Evidence: `track(x$unit_tests$results, file.path(results_dir, "unit-tests.csv"))` then `qual_mark_incomplete(x, failed)`.
- Failure scenario: the flattened testthat frame (list column of condition objects) fails to serialise -> verdict FAIL for a supplementary file.
- Confidence: verified-by-reading
- Suggested direction: either keep it out of `failed` or adjust the sentence.

## Area 2 — R/exportDocuments.R (fully read), R/documentBuilders.R (fully read)

### F-c4-tmpl-export-16  [major] [code-cheat]
- Where: R/exportDocuments.R:1384-1432 (.convert_docx_to_pdf); documented trade-offs at R/exportDocuments.R:926-944, 980-993
- What: PDF export silently falls back from the backend it advertises (Word, then LibreOffice) to TinyTeX/pandoc, which by the package's own documentation drop the running page header and footer, and the caller is never told which backend produced the file or that the preferred ones failed.
- Evidence:
  `err <- tryCatch({ .dta_write_staged(pdf_file, write = ...) ; NULL }, error = function(e) conditionMessage(e))`
  `if (is.null(err)) { return(invisible(pdf_file)) }`  -- the collected `failures` are only shown when ALL backends fail.
  dta_pdf_backend() doc: "TinyTeX -- ... Page headers and footers are lost." / "pandoc -- ... Page headers and footers are lost."
- Failure scenario: Windows host where `dta_pdf_backend()$name == "word"`; Word's COM call fails (activation dialog, 300 s timeout, locked profile). `write_dta(dta, "dta.pdf")` still reports "Document saved", but the PDF came from TinyTeX: no "Page X of Y", no title/study header, no version/date footer on any page -- a regulated agreement document missing its identifying furniture, with no warning. Same path renders the qualification report PDF (R/qualification-report.R:556-565).
- Confidence: verified-by-reading
- Suggested direction: `cli::cli_warn()` (classed) whenever the backend used is not the first available one, naming the failures, or make the fallback opt-in.

### F-c4-tmpl-export-17  [minor] [code-cheat]
- Where: R/exportDocuments.R:1063-1082 (.soffice_docx_to_pdf)
- What: LibreOffice's stdout/stderr and exit status are captured and thrown away, so a failed conversion is reported only as "LibreOffice did not write a PDF" -- the reason is lost (the Word route at 1164-1169 keeps it).
- Evidence: `suppressWarnings(system2(soffice, c(...), stdout = TRUE, stderr = TRUE, timeout = 180))` -- return value not assigned; then `if (!file.exists(produced)) cli::cli_abort("LibreOffice did not write a PDF for {.file {docx_file}}.")`.
- Failure scenario: soffice dies with "javaldx: Could not find a Java Runtime" or a profile lock -> the user (and the aggregated failure list of F-16) sees only the generic sentence.
- Confidence: verified-by-reading
- Suggested direction: keep `out` and include `.cli_escape(out)` in the abort, as the Word route does.

### F-c4-tmpl-export-18  [nit] [inconsistency]
- Where: R/exportDocuments.R:134-146, 1509-1522 (write_dta / write_dataset_metadata)
- What: An explicit `format` is never reconciled with the extension of `file`, so `write_dta(x, "a.docx", format = "md")` writes Markdown into a `.docx` file (and `format = "pdf"` into `.docx` a PDF).
- Evidence: the extension is consulted only `if (is.null(format))`; no check otherwise.
- Failure scenario: Word refuses to open the resulting "a.docx"; nothing warned.
- Confidence: verified-by-reading
- Suggested direction: abort (or warn) when `file`'s extension contradicts `format`.

### F-c4-tmpl-export-19  [nit] [dead-code]
- Where: R/exportDocuments.R:1385-1389
- What: `.pdf_conversion_available()` re-runs every backend probe (registry reads, `Sys.which`, TinyTeX lookup) that `backends` already holds; the two conditions are the same test.
- Evidence: `backends <- .pdf_backends_available()` / `if (!.pdf_conversion_available() || length(backends) == 0)` where `.pdf_conversion_available <- function() length(.pdf_backends_available()) > 0`.
- Failure scenario: none functional; double probing per export.
- Confidence: verified-by-reading
- Suggested direction: keep only `length(backends) == 0`.

### F-c4-tmpl-export-20  [nit] [inconsistency]
- Where: R/exportDocuments.R:627-636 vs R/documentBuilders.R:480-491
- What: The Markdown writer's comment says a clause without text is shown "as in Word ... like every other missing value", but Word renders an empty paragraph while Markdown prints MISSING_VALUE_DISPLAY; and the "Untitled section" branch is unreachable because the metadata validator requires a nonblank title (R/DTAMetaData-helpers.R:51-53).
- Evidence: md `if (length(text) > 0) text else .md_escape(MISSING_VALUE_DISPLAY)`; docx `.add_text_paragraphs(doc, section$text)` (no placeholder).
- Failure scenario: a clause with `text: ""` reads "(not specified)" in the .md and blank in the .docx/.pdf of the same DTA.
- Confidence: verified-by-reading
- Suggested direction: pick one rendering for blank clause text and delete the unreachable title fallback.

### F-c4-tmpl-export-21  [nit] [code-cheat]
- Where: R/documentBuilders.R:122-156 (.xml_safe_deep)
- What: Failures to read or rewrite a property are swallowed, and nesting deeper than 50 returns the object unsanitised, so control characters can survive into the Word part and surface later as an unexplained libxml abort.
- Evidence: `v <- tryCatch(S7::prop(x, p), error = function(e) NULL)`; `x <- tryCatch({ S7::prop(x, p, check = FALSE) <- nv; x }, error = function(e) x)`; `if (depth > 50L) return(x)`.
- Failure scenario: a property whose setter refuses the cleaned value keeps its raw text -> officer/xml2 abort "PCDATA invalid Char value" with no pointer to the field.
- Confidence: verified-by-reading (no current read-only text property was identified)
- Suggested direction: let the errors propagate (or abort naming the property) instead of returning the original.

## Area 3 — R/documentData.R (fully read)

### F-c4-tmpl-export-22  [major] [bug]
- Where: R/documentData.R:294-314 (.file_specs_df) and 481-494 (.file_count_sentence)
- What: An open-ended file entry (`max_number_of_files` NULL, which the engine and print methods treat as "unbounded") is exported as a maximum of 0, so the Word/PDF/Markdown agreement states an impossible count such as "2 to 0".
- Evidence:
  `max_n <- f@max_number_of_files %||% 0` ... `Expected Count = paste0(min_n, if (min_n != max_n) paste0(" to ", max_n) else "")`
  `total_max <- sum(sapply(files, function(f) { tryCatch(f@max_number_of_files %||% 0, error = function(e) 0) }))`
  Engine (R/DTADataSetFile-class.R:337-351): a NULL `hi` is never exceeded and is worded "at least <lo>"; DTAFile's own example (R/DTAFile-class.R:1031-1035) builds exactly such an entry.
- Failure scenario: `DTAFileCSV(filename = "part.*", pattern = TRUE, min_number_of_files = 2)` in a dataset -> write_dta()/write_dataset_metadata() print "Expected Count: 2 to 0" and "2 to 0 files are expected for this dataset in total." in the signed specification, contradicting what check() accepts (any number >= 2). No test covers it: tests/testthat/test-documentData.R:240-359 use only bounded counts ("1 to 3", "2 to 4").
- Confidence: verified-by-reading
- Suggested direction: render a NULL maximum as "at least n"/"n or more" (and an unbounded total likewise), mirroring dta_print_file_count().

### F-c4-tmpl-export-23  [minor] [dead-code]
- Where: R/documentData.R:57, 482-486
- What: The export path still upgrades "stale" objects saved by older versions (compat layer, cf. F-t-core-2-27), and the file-count sentence turns any error reading a count property into 0.
- Evidence: `if (dta_is_stale(dta)) dta <- dta_upgrade(dta)`; `tryCatch(f@min_number_of_files %||% 0, error = function(e) 0)`.
- Failure scenario: an unexpected error reading the property on a current object silently produces "0 files are expected" in the document instead of an error; the stale-upgrade costs a check per export for a case the owner's no-compat policy removes.
- Confidence: verified-by-reading
- Suggested direction: drop the upgrade call and the tryCatch; read the properties directly.

## Area 4 — R/exportMarkdown.R (fully read)

### F-c4-tmpl-export-24  [minor] [bug]
- Where: R/exportMarkdown.R:117-131 (.md_escape_plain); consumer R/qualification-report.R:17-22, 517-533 (pandoc rendering of the report)
- What: The escaper does not neutralise pandoc-Markdown syntax that is on by default when the qualification report is rendered through rmarkdown/pandoc -- `^...^` superscript and the `smart` extension (`--`, `---`, `...`, straight quotes) -- so "evidence verbatim" is not true for the HTML/Word/PDF reports.
- Evidence: escaped set is `([\`*_{}\[\]#!~|$])` plus `<`, `>`, `&`, `\`; `^`, `"`, `'` and `-` (outside line start) pass through. qual_md_escape's comment: "pandoc must not read it as raw HTML, TeX, maths or emphasis".
- Failure scenario: a qa_step message "expected: --no-manual | actual: \"x^2^\"" renders in qualification-report.html/.docx as "expected: –no-manual | actual: “x²”" (en dash, curly quotes, superscript) -- the rendered evidence differs from results.json.
- Confidence: inferred (pandoc's default `markdown` extensions `superscript` and `smart` not re-checked against the pandoc version in use; escaping code verified by reading)
- Suggested direction: escape `^` (and `"`, `'`, `-`, `.` sequences) or render with `from = "markdown-smart-superscript"` / `"gfm"`.

## Area 5 — R/exportFunctions.R (fully read)

### F-c4-tmpl-export-25  [minor] [bug]  (confirms the export half of F-t-core-2-24)
- Where: R/exportFunctions.R:83-102 (get_spec_description inside export_specs_table)
- What: Permitted values are written as one `#@values:` line joined by "; " with no quoting or escaping, so a value containing ";" (or a line break) cannot survive the documented export -> columns_specs_from_word() round trip.
- Evidence: `value_line <- paste0("\n#@values: ", paste(as.vector(unlist(values)), collapse = "; "))`
- Failure scenario: `values = c("Yes; confirmed", "No")` -> cell text "#@values: Yes; confirmed; No" -> re-imported as three values; a value with "\n" ends the marker line early.
- Confidence: verified-by-reading (export side; the import side is in R/DTAColumnSpecCollection-class.R, outside this slice)
- Suggested direction: quote/escape values on export (or abort on a value containing the separator).

### F-c4-tmpl-export-26  [minor] [inconsistency]
- Where: R/exportFunctions.R:112-114 vs R/documentData.R:409-415
- What: An unset `nullable` is exported by export_specs_table() as an empty cell, while write_dta()/write_dataset_metadata() print "No (default)" for the same column, because the engine checks an unset `nullable` as FALSE.
- Evidence: `Nullable = sapply(specs, function(x) { if (is.null(x@nullable)) NA else ifelse(x@nullable, "Yes", "No") })` vs `nullable_str <- if (!is.null(spec@nullable)) ... else "No (default)"`.
- Failure scenario: the spec table handed to a supplier shows a blank Nullable for a column that rejects missing values; the agreement document for the same spec says "No (default)".
- Confidence: verified-by-reading
- Suggested direction: state the effective value ("No (default)") in both, or document why the table differs.

### F-c4-tmpl-export-27  [nit] [doc-wrong]
- Where: R/exportFunctions.R:148, 183 (and 143-147, 177-182)
- What: Column widths convert centimetres to inches with 2.54 everywhere except the last column of each layout, which divides by 2.5.
- Evidence: `flextable::width(j = 6, width = 9.75 / 2.5)` / `flextable::width(j = 7, width = 9.5 / 2.5)` next to `width = 2.21 / 2.54`.
- Failure scenario: the description column is 1.6 % wider than the centimetre value written in the code says.
- Confidence: verified-by-reading
- Suggested direction: use 2.54 consistently (or one named constant).

### F-c4-tmpl-export-28  [nit] [design]
- Where: R/exportFunctions.R:295-345 (write_metadata)
- What: The sidecar is named `<file>.md5` but its content ("md5sum: <hash>", "Number of Columns: n", ...) is not the `<hash>  <name>` format `md5sum -c` reads, so the conventional verification command fails on it.
- Evidence: `metadata_lines <- c(paste0("md5sum: ", checksum), paste0("Number of Columns: ", n_cols), paste0("Number of Rows: ", n_rows))`; `metadata_file <- paste0(file, ".md5")`.
- Failure scenario: a receiver runs `md5sum -c data.csv.md5` -> "no properly formatted MD5 checksum lines found".
- Confidence: verified-by-reading
- Suggested direction: name it e.g. `.meta.txt`, or write a standard md5 line plus a separate info file.

## Area 6 — R/exportTemplateBlocks.R, R/exportTemplateDocx.R, R/exportTemplateRegions.R (all fully read)

### F-c4-tmpl-export-29  [major] [code-cheat]
- Where: R/exportTemplateBlocks.R:653-665 (.tv_render_blocks), R/exportTemplateDocx.R:1235-1238 (.substitute_placeholder_text), R/exportDocuments.R:168-178 (write_dta), inst/shiny/dta_app/app.R:10168-10176
- What: Unrendered block placeholders and malformed `{#DATASETS}`/`{/DATASETS}` markers are reported ONLY when `quiet = FALSE`, inline substitution deliberately does not count them as unresolved, and both production callers pass `quiet = TRUE` unconditionally -- so through write_dta(template=) and the Shiny app these template errors are never reported.
- Evidence:
  `leftover <- .tv_scan_block_tokens(docx_path); if (length(leftover) > 0 && !isTRUE(quiet)) { ... cli::cli_warn(...) }`
  `} else if (.tv_is_block_token(token) || .tv_is_region_marker(token)) { out <- c(out, token) }  # "Not unresolved"`
  write_dta: `export_with_template(x, template = template, ..., quiet = TRUE, fallback = FALSE, overwrite = TRUE)` (the user's own `quiet = FALSE` is ignored); app: `quiet = TRUE, fallback = FALSE`.
  Roxygen of export_with_template: "Any placeholder without a corresponding value is left untouched and reported via a warning."
- Failure scenario: a template paragraph `{COLUMN_SPECS:ADLS}` (typo), `{DATASETS}` inside a table cell, or a `{#DATASETS}` with no closing marker -> `write_dta(dta, "dta.docx", template = "t.docx")` (quiet = FALSE) or the app's Word export produces a document containing the literal token / an unexpanded region, with no warning, while an unknown inline token in the same template IS warned about.
- Confidence: verified-by-reading
- Suggested direction: report leftovers regardless of `quiet` (it is a content warning, not console chatter), or have write_dta() forward its own `quiet`.

### F-c4-tmpl-export-30  [minor] [code-cheat]
- Where: R/exportTemplateDocx.R:180-206 (export_with_template, `fallback = TRUE` default)
- What: With the default `fallback = TRUE` and `quiet = TRUE`, a template that fails to process is silently replaced by the built-in layout: the only signal of the substitution is the warning that `quiet` suppresses.
- Evidence: `if (isTRUE(fallback)) { if (!isTRUE(quiet)) { cli::cli_warn(c("Template processing failed: ...", i = "Falling back to the standard document format.")) }; write_dta(dta, output, format = "docx", overwrite = TRUE, quiet = quiet) }`
- Failure scenario: `export_with_template(dta, "corporate.docx", out, quiet = TRUE)` on a template with a zip-slip-flagged member or an unreadable part returns `out` holding the package's built-in layout instead of the corporate template, invisibly and without any condition.
- Confidence: verified-by-reading (behaviour is documented in @param fallback, but documentation does not make a silent substitution acceptable under the brief)
- Suggested direction: always signal the fallback (classed warning or message) irrespective of `quiet`, or default `fallback` to FALSE.

### F-c4-tmpl-export-31  [minor] [inconsistency]
- Where: R/exportTemplateDocx.R:416 vs R/documentData.R:159 / R/documentBuilders.R:249-260
- What: `{DTA_DATE}` is only `metadata@date`, while the built-in layout's date (cover, Document Information, footer) falls back to the version-history date of the current version; the same DTA therefore shows a date in write_dta() and an empty `{DTA_DATE}` in a template export.
- Evidence: `"{DTA_DATE}" = .tv_scalar(meta@date)` vs `Date = .format_document_date(.current_version_date(meta))`.
- Failure scenario: a DTA with `version: "2.0"` and a version_history entry dated 2026-11-05 but no `date:` -> built-in document "Date 2026-11-05"; template document "Date: " blank.
- Confidence: verified-by-reading
- Suggested direction: use `.current_version_date(meta)` for `{DTA_DATE}` too (and say so in the catalogue).

### F-c4-tmpl-export-32  [minor] [dead-code]
- Where: R/exportTemplateBlocks.R:30-37 (.tv_block_aliases), R/exportTemplateRegions.R:122, inst/extdata/word-templates/clinical_dta_template.docx
- What: `{DATASETS_DETAIL}` is kept as a "legacy block token spelling" mapped to `{DATASETS}` purely for compatibility with an older bundled template and the app's former markdown string -- the no-compat policy makes it dead weight; the bundled template should use the canonical token.
- Evidence: `#' Legacy block token spellings, mapped to their canonical name` / `c("{DATASETS_DETAIL}" = "{DATASETS}")`.
- Failure scenario: two spellings of one block in the grammar, the catalogue (dta_template_placeholders()) does not list the alias, so a template author cannot discover why `{DATASETS_DETAIL}` works.
- Confidence: verified-by-reading
- Suggested direction: rename the token in clinical_dta_template.docx and delete the alias table.

### F-c4-tmpl-export-33  [nit] [dead-code]
- Where: R/exportTemplateDocx.R:575-583 (.tv_signatories_block)
- What: The Department/Email/Phone branches can never run: `.extract_signatories()` returns only Organization, Name and Role.
- Evidence: `if ("Department" %in% names(sig) && ...)`, `"Email" %in% names(sig)`, `"Phone" %in% names(sig)`; R/formattingHelpers.R:453-458 builds `data.frame(Organization =, Name =, Role =)` and .normalize_signatories() returns `signatories[, c("Organization", "Name", "Role")]`.
- Failure scenario: none at run time; the `{SIGNATORIES}` block reads as if it showed contact details that it never shows.
- Confidence: verified-by-reading
- Suggested direction: delete the three branches (or extend .extract_signatories() if the details are wanted).

### F-c4-tmpl-export-34  [minor] [bug]
- Where: R/exportTemplateRegions.R:178-204, 308-315 (.tv_renumber_drawings / region copy)
- What: Bookmarks inside a repeating region are copied verbatim (same `w:id` and `w:name`) in every repetition; the comment defers this "for the day a template needs it", but Word adds hidden `_Toc…`/`_Ref…` bookmarks to any heading that a table of contents or cross-reference targets, so an ordinary template already needs it.
- Evidence: `#' A bookmark (w:bookmarkStart / w:bookmarkEnd) ... inside a region is copied verbatim, id included, in every repetition`.
- Failure scenario: a template whose `{#DATASETS}` region holds a "Dataset {DATASET_NAME}" heading listed in the template's TOC -> N copies share one `_Toc` bookmark; the TOC entries all jump to the first dataset, and Word may report duplicate-id content on open.
- Confidence: inferred (Word's handling of duplicate bookmark ids/names not reproduced here; the verbatim copy is verified by reading)
- Suggested direction: renumber `w:id` pairs and suffix `w:name` per repetition, as is already done for docPr ids.

### F-c4-tmpl-export-35  [nit] [design]
- Where: R/exportTemplateRegions.R:11-61, R/exportTemplateDocx.R:883-885
- What: Brace protection uses U+E000/U+E001 and then rewrites EVERY `w:t` of document.xml that contains them back to `{`/`}`, on the claim that "no real document text uses" these code points -- but icon/symbol fonts store glyphs in the Private Use Area, so a template's own U+E000/U+E001 characters (or a DTA value containing them) are silently turned into braces.
- Evidence: `nodes <- xml2::xml_find_all(doc, ".//*[local-name()='t']")` ... `xml2::xml_text(nodes[[k]]) <- .tv_restore_braces(texts[[k]])` over the whole part.
- Failure scenario: a corporate template with an icon-font glyph U+E001 in its title page loses the glyph ("}" printed instead).
- Confidence: verified-by-reading (code path); inferred (prevalence of PUA glyphs in templates)
- Suggested direction: restore only within the nodes pass 0 wrote (track them), or use a nonce-based ASCII stand-in.

### F-c4-tmpl-export-36  [nit] [code-cheat]
- Where: R/exportTemplateBlocks.R:142-146, 187-199; R/exportTemplateRegions.R:249-259
- What: Pass 0/1 helpers and the leftover scan turn an unreadable part or a failed unzip into "nothing found" (`character(0)`), so the leftover report can pass vacuously.
- Evidence: `doc <- tryCatch(xml2::read_xml(xml_path), error = function(e) NULL); if (is.null(doc)) return(character(0))`; `extracted <- tryCatch(utils::unzip(docx_path, exdir = temp_dir), warning = function(w) character(0), error = function(e) character(0))`.
- Failure scenario: in practice later steps (officer::read_docx check, pass 1's unguarded read_xml) fail loudly, so the effect is limited to a lost report; but the scan itself cannot distinguish "no leftovers" from "could not look".
- Confidence: verified-by-reading
- Suggested direction: let these errors propagate.

## Area 7 — R/exportValidationReport.R (fully read)

### F-c4-tmpl-export-37  [minor] [code-cheat]  (confirms F-t-core-2-39 in the code)
- Where: R/exportValidationReport.R:116-132, 152-157
- What: Every error from inspect() is swallowed -- first for all ids, then per dataset group with `error = function(e2) NULL` -- with no warning and no error text; the affected messages silently fall back to the bare message row.
- Evidence: `inspect_all <- tryCatch(DTAtools::inspect(x, id = msgs$id, as_tibble = FALSE), error = function(e) { ... tryCatch(DTAtools::inspect(x, id = ids, as_tibble = FALSE), error = function(e2) NULL) ... })`; `inspect_rows <- if (!is.null(rows_for_id)) ... else msg_row`.
- Failure scenario: a defect in inspect() for one dataset produces a complete-looking report whose panels for that dataset carry no row context; neither `e` nor `e2` reaches the user, CI or a log.
- Confidence: verified-by-reading
- Suggested direction: keep the isolation but cli_warn() (classed) naming the dataset and the error.

### F-c4-tmpl-export-38  [minor] [code-cheat]
- Where: R/exportValidationReport.R:171-195, 246-248
- What: A missing or unreadable bundled `report.css`/`report.js` silently becomes "", producing a report whose inspect panels stay `hidden` forever and whose table cannot be sorted or filtered -- although the package treats a missing bundled asset as a broken installation elsewhere (R/documentBuilders.R:421-437).
- Evidence: `css_content <- tryCatch({ ...; if (nzchar(css_path) && file.exists(css_path)) paste0(readLines(...)) else "" }, error = function(e) "")` (same for JS); `<div id="inspect-panels" hidden>`.
- Failure scenario: a partial or damaged installation writes a report that looks normal at first glance but cannot show any row detail, with "Report saved" and no warning; the documented "sortable, filterable table ... with click-to-inspect detail" is absent.
- Confidence: verified-by-reading
- Suggested direction: cli_abort() when either asset is missing, as .new_numbered_docx() does.

## Area 8 — R/validateTemplate.R, R/lintTemplateRepo.R, R/createTemplateRepo.R, R/run_dta_app.R (all fully read; inst/extdata/ci/* read)

### F-c4-tmpl-export-39  [major] [code-cheat]
- Where: R/validateTemplate.R:973 and 1475
- What: Both places that run lint_dta() over a template (every built variant of a creation template; every dataset template body) turn ANY error from lint_dta() into "no findings", so the whole linter category -- including the D16 `file_name_conflict` that is escalated to an error -- passes vacuously when the linter fails.
- Evidence: `lint <- tryCatch(lint_dta(build_res$value), error = function(e) NULL)` / `lint <- tryCatch(lint_dta(lint_input), error = function(e) NULL)` followed by `for (i in seq_len(if (is.null(lint)) 0L else nrow(lint)))`. The @return promises "Zero rows means every checked file passed every check."
- Failure scenario: a dataset template whose `rules:` entry has a shape lint_dta() does not anticipate (or any defect in lint_dta()) makes lint_dta() abort; validate_template() returns zero rows for the file, lint_template_repo() prints "No findings" and the template repository's CI goes green, although no rule or column was judged.
- Confidence: verified-by-reading
- Suggested direction: report a caught error as its own error row (e.g. `lint_failed` with the message) instead of NULL.

### F-c4-tmpl-export-40  [minor] [code-cheat]
- Where: R/validateTemplate.R:620-632 (.dta_template_check_party); inst/shiny/dta_app/R/party_profiles.R:52-74
- What: The party-profile check suppresses the reader's own "dropping N malformed contact(s)" warning and reports nothing, so a profile whose contacts the app silently drops passes validation clean.
- Evidence: `read <- suppressWarnings(.dta_template_engine_get("read_party_profile")(path))` with the comment "Not reported here: the reader drops malformed contacts with a warning, which is suppressed above."; reader: `cli::cli_warn("Party profile {.val {profile_id}}: dropping {n} malformed contact{?s} (not a named list).")`.
- Failure scenario: `contacts: [ "Jane Doe" ]` (a bare string) -> lint_template_repo() reports no finding; in the app the supplier appears with no contact and no signatory, and every DTA created from the profile lacks the signature line.
- Confidence: verified-by-reading
- Suggested direction: capture the warning (withCallingHandlers) and emit it as a `party_profile_contact_dropped` row (warning or error).

### F-c4-tmpl-export-41  [minor] [logic-error]
- Where: R/validateTemplate.R:495-499, 521-528, 553-555
- What: A file whose `kind:` is unknown (e.g. a typo) or that has no `id`/`version` is only a WARNING, and an unknown kind skips every further check -- although the app's reader refuses such a file outright (`stop("Template 'kind' must be 'dta_creation_template'.")`) and the index drops id-/version-less files, so the template can never be offered.
- Evidence: `add(kind, id, version, "warning", "kind_unknown", ...)` ... `if (!kind_known) { return(.dta_template_bind_rows(rows)) }`; inst/shiny/dta_app/R/template_core.R:88-91.
- Failure scenario: `templates/study_x.dta-template.yaml` with `kind: dta_creaton_template` -> one warning, no dry-run build; `lint_template_repo(".")` (fail_on = "error") exits 0, CI is green, and the template is missing from the app's picker.
- Confidence: verified-by-reading
- Suggested direction: make `kind_unknown` (for a file whose suffix names a known kind) and `id_missing`/`version_missing` errors, consistent with `kind_suffix_mismatch` and `dataset_id_missing`.

### F-c4-tmpl-export-42  [minor] [doc-wrong]
- Where: R/validateTemplate.R:1643-1648 and roxygen 1926-1933
- What: Both say the packaged `validate-templates.yml` workflow's "entire CI job is `validate_template(".", strict = TRUE)`", but the workflow (and the Bitbucket/Jenkins assets) run `DTAtools::lint_template_repo(".")`.
- Evidence: inst/extdata/ci/validate-templates.yml: `run: Rscript -e 'DTAtools::lint_template_repo(".")'`.
- Failure scenario: a reader reasons about CI behaviour (strict abort on errors only, no repository-format checks) from the wrong function.
- Confidence: verified-by-reading
- Suggested direction: update both texts to lint_template_repo().

### F-c4-tmpl-export-43  [nit] [doc-wrong]
- Where: R/validateTemplate.R:54-55, 60-68
- What: The comments say the engine is "~150 KB of helper code" and list six engine files, but `.dta_template_engine()` sources every `.R` file in inst/shiny/dta_app/R -- 18 files, about 850 KB, including UI-only files (theme.R 95 KB, ui_components.R, template_ui.R) and utils_dta.R (232 KB).
- Evidence: `files <- sort(enc2utf8(list.files(r_dir, pattern = "[.][Rr]$", full.names = TRUE)), method = "radix")` then `sys.source(f, envir = env, ...)` for all.
- Failure scenario: misleading cost/scope statement for the first validate_template() call of a session.
- Confidence: verified-by-reading (sizes from `ls`)
- Suggested direction: correct the figure and the file list, or source only the engine files.

### F-c4-tmpl-export-44  [nit] [doc-wrong]
- Where: R/run_dta_app.R:16-19 vs 32
- What: The details say an informative error appears "If any of shiny, bslib or DT is missing", but the code also requires and checks shinyjs.
- Evidence: `required <- c("shiny", "bslib", "DT", "shinyjs")`.
- Failure scenario: none functional; the documented list is incomplete.
- Confidence: verified-by-reading
- Suggested direction: name shinyjs in that sentence.

No finding in R/createTemplateRepo.R and R/lintTemplateRepo.R beyond the above (checked: overwrite pre-check, CI asset mapping, radix ordering, `.cli_escape` of finding text, fail_on semantics -- lint_dta() and validate_template() emit only "error"/"warning").

## Area 9 — R/templateGuide.R, R/templateGuide-collect.R (both fully read)

### F-c4-tmpl-export-45  [major] [bug]
- Where: R/templateGuide-collect.R:769-795 (.tgc_dry_builds) vs R/validateTemplate.R:944-953 and inst/shiny/dta_app/R/party_profiles.R:310-321
- What: The guide's sample build calls create_dta_from_template() without `party_selections`, so every template with a `required: true` party slot fails to build in the guide (the engine aborts on a required slot with no selection) -- while validate_template() fills such slots with `.dta_template_party_dry_selections()` and passes the same template.
- Evidence: guide `b <- g("create_dta_from_template")(sdef, path, sel, index = ctx$index, vocab_selections = vsel, study_fallback = fallback)`; validator adds `party_selections = .dta_template_party_dry_selections(selected_def, index_df)`; engine `if (isTRUE(slot$required)) { cli::cli_abort(c("Party slot {.val {slot$id}} is required, and no profile was selected for it.", ...)) }`.
- Failure scenario: a library template with `party_slots: [{id: supplier, required: true, ...}]` -> write_template_guide() warns "Could not describe ... completely", the guide shows "A sample DTA could not be built from this template: Party slot "supplier" is required ..." and contains NO "Datasets it creates" chapter, although lint_template_repo() reports the template clean. No test covers a required slot in the guide (tests/testthat/test-templateGuide-collect.R uses only optional slots).
- Confidence: verified-by-reading (not executed)
- Suggested direction: pass `.dta_template_party_dry_selections(sdef, ctx$index)` exactly as the validator does (one shared helper for both dry runs).

### F-c4-tmpl-export-46  [minor] [doc-wrong]
- Where: R/templateGuide.R:25-28, 111-122 (write_template_guide)
- What: The promise that a warning says when a PDF route loses the running header/footer only checks the FIRST available backend; when Word/LibreOffice is first but fails, .convert_docx_to_pdf() silently falls back to TinyTeX/pandoc (F-16) and no warning is given.
- Evidence: `backend <- dta_pdf_backend(); if (!is.null(backend) && backend$name %in% c("tinytex", "pandoc")) cli::cli_warn(...)`; conversion later via `.convert_docx_to_pdf(temp_docx, targets[[k]])`.
- Failure scenario: Windows host, Word COM broken, TinyTeX installed -> guides written as PDFs without "Page X of Y" headers/footers, no warning, contrary to @param format.
- Confidence: verified-by-reading
- Suggested direction: let .convert_docx_to_pdf() report the backend it used and warn from that.

### F-c4-tmpl-export-47  [minor] [doc-wrong]
- Where: R/templateGuide.R:1-3 -> man/write_template_guide.Rd \details
- What: The roxygen block has both a leading title paragraph and an explicit `@title`, so the title sentence is emitted a second time as the Rd's entire \details section.
- Evidence: Rd: `\details{ Write a readable guide for each template of a library }`.
- Failure scenario: ?write_template_guide shows a "Details" section that only repeats the title.
- Confidence: verified-by-reading (generated Rd inspected)
- Suggested direction: delete the duplicate first line (keep `@title`), regenerate docs.

### F-c4-tmpl-export-48  [minor] [code-cheat]
- Where: R/templateGuide-collect.R:1420-1437 (.tgc_build_dataset_template), 1395-1413
- What: Building the dataset-template side of the "Added/Changed" comparison swallows three different errors (term-list narrowing, token resolution, dataset construction); the first two silently compare against an un-narrowed/unresolved template and the third silently drops all change marks and the "Based on dataset template" line -- no problem row is recorded on this path.
- Evidence: `plain <- tryCatch(g("apply_vocabulary_slot_overrides")(...), error = function(e) b$value$dataset)`; `plain <- tryCatch(g("resolve_dataset_tokens")(plain, env), error = function(e) plain)`; `tryCatch(dta_dataset_from_list(plain), error = function(e) NULL)`; caller `if (is.null(built)) NULL`.
- Failure scenario: a narrowing that fails -> every narrowed column is marked "Changed by this template" in the guide although the template did not change it; a construction failure -> a heavily patched dataset is shown with no "Added/Changed" marks and no source, i.e. as if unmodified.
- Confidence: verified-by-reading
- Suggested direction: record each caught error through ctx$problem (and say "comparison unavailable") instead of degrading silently.

### F-c4-tmpl-export-49  [minor] [inconsistency]
- Where: R/templateGuide-collect.R:1324-1332 (.tgc_col_rows) vs R/documentData.R:417-426
- What: The guide's "Allowed values" shows the value list OR the pattern, never both, while the agreement document deliberately shows both ("a specification that hides ... is wrong").
- Evidence: `allowed <- if (length(vals) > 0) paste(vals, collapse = "\n") else if (nzchar(pat)) paste("Must match pattern", pat) else "Any"`.
- Failure scenario: a column with `values: [A, B]` and `pattern: "^[A-Z]$"` -> the guide omits the pattern constraint the data is checked against.
- Confidence: verified-by-reading (DTAColumnSpec accepts both; the docx builder shows both)
- Suggested direction: append the pattern line when both are present.

### F-c4-tmpl-export-50  [minor] [logic-error]
- Where: R/templateGuide.R:858-870 (.tg_technical)
- What: Problems of referenced files are matched by the prefix "<id>." of the file name, so a file named "<id>_<qualifier>.<suffix>" -- a naming lint_template_repo() explicitly accepts -- never has its problems listed in the guide that uses it.
- Evidence: `vapply(pr[["File"]], function(f) any(startsWith(f, paste0(involved, "."))), logical(1))`; R/lintTemplateRepo.R:132 accepts `paste0(rec$id, c("_", "-", ".", "@"))`.
- Failure scenario: `templates/gf_smrnaseq_v2.dta-dataset-template.yaml` (id gf_smrnaseq) cannot be built -> its problem is collected but omitted from the "Problems found" table of every DTA template using it.
- Confidence: verified-by-reading
- Suggested direction: match problems by path (collect the file paths of the involved index rows) rather than by name prefix.

### F-c4-tmpl-export-51  [nit] [doc-wrong]
- Where: R/templateGuide-collect.R:14; R/templateGuide.R:29-31, 124-132
- What: (a) The header says "`%||%` here is base R's (NULL only)" but the namespace imports rlang's (R/00_helpers.R:8; same NULL-only semantics). (b) `organisation_unit = NULL` is documented as "uses the one set in the library's dta-app.yaml", but when `organisation` is supplied the unit silently becomes "" instead.
- Evidence: `unit <- if (!is.null(organisation_unit)) ... else if (is.null(organisation)) .tg_chr(lib[["organisation_unit"]]) else ""`.
- Failure scenario: `write_template_guide(lib, out, organisation = "ACME")` drops the library's configured unit from the cover/header without saying so.
- Confidence: verified-by-reading
- Suggested direction: fix the header comment; document (or remove) the coupling.

### F-c4-tmpl-export-52  [nit] [design]
- Where: R/templateGuide.R:262-266 (.tg_props)
- What: Writes into flextable's internal style store (`ft$body$styles$pars$keep_with_next$data[...]`) instead of a public API; a flextable release that reshapes that structure either errors or silently stops un-chaining the last row.
- Evidence: `ft$body$styles$pars$keep_with_next$data[nrow(ft$body$styles$pars$keep_with_next$data), ] <- FALSE`.
- Failure scenario: after a flextable update, every kept table chains to the following paragraph, producing large blank gaps.
- Confidence: inferred (no flextable version check in the code)
- Suggested direction: use flextable's paragraph-property setter on the last row, if available, or guard the internal access.

## Area 10 — inst/extdata (template-repo-skeleton, agent-skills, ci, library) as far as they touch the slice

### F-c4-tmpl-export-53  [minor] [doc-wrong]
- Where: R/exportTemplateDocx.R:838-851 (parts substituted), R/exportTemplateBlocks.R:201-211 (parts scanned for leftovers); inst/extdata/template-repo-skeleton/README.md:1562
- What: Only `word/document.xml` and `header*.xml`/`footer*.xml` are substituted and scanned, so a marker in `footnotes.xml`, `endnotes.xml` or `comments.xml` is neither filled nor reported -- while the skeleton README tells template authors "Markers are replaced wherever they appear".
- Evidence: both functions use `list.files(word_dir, pattern = "^(header|footer)[0-9]*\\.xml$", ...)` plus document.xml; the unresolved-inline warning is built only from those parts.
- Failure scenario: a corporate template with "{STUDY_ID}" in a footnote -> the exported DTA keeps the literal token in the footnote and no warning is raised.
- Confidence: verified-by-reading (export_with_template()'s own roxygen correctly says "body, headers, footers and table cells"; the README overclaims)
- Suggested direction: include footnote/endnote parts in substitution and leftover scan (or name the exclusion in the README and warn on markers found there).

### F-c4-tmpl-export-54  [minor] [code-cheat]
- Where: R/qualification.R:1046-1048 (qual_safe); R/qualification-report.R:518-531, 556-566, 571-578
- What: `qual_safe()` unwinds on the first WARNING as well as on errors, so a render that wrote its file but warned is recorded as not produced, and in qual_render_pdf() a warning on the Word/LibreOffice route discards that PDF and falls through to xelatex, which renders the Markdown into the same path -- a different document -- with no record of why.
- Evidence: `qual_safe <- function(expr, default = NA) { tryCatch(expr, error = function(e) default, warning = function(w) default) }`; `via_docx <- qual_safe({ .convert_docx_to_pdf(docx_path, pdf); pdf }, NA_character_)`.
- Failure scenario: a TinyTeX conversion that succeeds with a warning -> the docx-faithful PDF is overwritten by the xelatex rendering of the md; `formats_rendered$html` is NA while qualification-report.html sits (hashed) in the bundle.
- Confidence: inferred (whether these renderers raise R-level warnings on success was not reproduced); the unwinding itself is verified by reading
- Suggested direction: catch errors only; route warnings to the run log via withCallingHandlers().

### F-c4-tmpl-export-55  [minor] [logic-error]
- Where: R/qualification.R:649-680, 1358-1370; inst/qualification/docs/validation-plan.md:216
- What: In a complete, unfiltered run an open deviation whose binding never executed (`not_executed`) has no effect on the verdict, so acceptance criterion 4 ("Every open deviation still reproduces") can be met without any reproduction.
- Evidence: `stale <- sum(dev_status$status_run == "not_reproduced")` -- `not_executed` is never counted; M7 only checks statically that some qa_known_deviation() call names the id.
- Failure scenario: the binding sits after an early `return()` or inside a condition that is false on this host, in a test that otherwise passes -> DEV-xxx reads "not_executed" in section 8.1 and the verdict is PASS. Latent today: both open deviations are bound unconditionally (test-OQ-VAL-check.R:405, test-OQ-TYPE-rbackend.R:449).
- Confidence: verified-by-reading
- Suggested direction: in a non-partial run treat an open deviation with status_run "not_executed" as a failure (or at least as NOT-EXECUTED in the verdict).

### F-c4-tmpl-export-56  [minor] [test-weak]
- Where: R/qualification.R:254-285 (qual_discover_tests), 846-857 (M10)
- What: Discovery sees only top-level calls spelled exactly `test_that`, and nothing checks that every EXECUTED test was discovered, so a test registered under a condition, inside a helper, or as `testthat::test_that(` escapes M1/M2/M4/M10 and the traceability matrix.
- Evidence: `if (!is.call(call) || !identical(as.character(call[[1]])[[1]], "test_that")) next`; M10 is `setdiff(expected_tcs, tests_df$tc_id)` (discovered minus executed only).
- Failure scenario: `if (.Platform$OS.type == "windows") test_that("OQ-X-001 | ...", ...)` never runs on Linux and no meta check notices; a namespaced call runs but its requirement ids never reach the matrix. Latent: a grep finds no indented or namespaced test_that in inst/qualification/tests today.
- Confidence: verified-by-reading
- Suggested direction: add the reverse check (executed tc_ids subset of discovered) and reject non-top-level test_that calls via getParseData().

### F-c4-tmpl-export-57  [minor] [test-weak]
- Where: R/qualification.R:1423-1424 (qual_hash_bundle); oracle inst/qualification/tests/test-OQ-CORE-runner.R:79-89; claim R/qualification-report.R:219-222
- What: The manifest skips hidden files and directories (`all.files = FALSE`) and every file named SHA256SUMS/MD5SUMS at any depth, although the report says it "lists every file in this evidence bundle"; OQ-CORE-001's "covers every other file" oracle lists the bundle with the same default, so it cannot detect the gap.
- Evidence: `rel <- sort(list.files(dir, recursive = TRUE, all.files = FALSE)); rel <- rel[!basename(rel) %in% c("SHA256SUMS", "MD5SUMS")]` vs `on_disk <- setdiff(list.files(bundle$bundle_dir, recursive = TRUE), basename(manifest))`.
- Failure scenario: a `.note` file or `artifacts/x/SHA256SUMS` in the bundle is neither hashed nor reported; mutation: dropping hidden files from the manifest is undetectable by the test by construction. No current producer writes such a file (qa_artifact_dir() is defined but used by no test), so the impact is the completeness claim and tamper evidence.
- Confidence: verified-by-reading
- Suggested direction: hash with `all.files = TRUE` (excluding only the top-level manifest) and make the oracle list with `all.files = TRUE`.

Checked without a finding: inst/extdata/ci/{validate-templates.yml, bitbucket-pipelines.yml, Jenkinsfile} (all run `lint_template_repo(".")` after a deliberate failing install placeholder); template-repo-skeleton/README.md "Validating"/"Linting"/"CI"/validator-code tables (severities match the code, incl. `kind_unknown` = warning -- see F-41 for why that severity is itself the problem); export-templates/README.md (its "PDF export ... goes through Markdown and pandoc" is true of the app, utils_export.R:99-110); library/templates party_slots (no `required: true`, so the bundled examples do not hit F-45).

---

## Summary

57 findings: 1 blocker, 7 major, 33 minor, 16 nit.
By category: code-cheat 12, bug 8, inconsistency 9, doc-wrong 9, dead-code 5, logic-error 5, design 4, test-weak 3, doc-missing 1, security 1.

Withdrawn during the final pass: a draft finding on agent-skills/.../linting.md ("DTAtools 0.28.0 and earlier
... required party slot") -- accurate for the released 0.28.0; a dev tree reporting the last released version is
expected under the repo's no-stray-bump rule.

Focus items with no finding in this slice:
- `<<...>>` tokens: not used by any file of the slice; they are the date-pattern masks of R/datePatternTokens.R
  (and inst/shiny/dta_app/R/template_core.R), outside this slice -- not reviewed. The exporters print authored
  file-name patterns verbatim (Markdown escapes `<`/`>` as entities).
- `${...}` tokens: validated by `.dta_template_unknown_tokens()` (R/validateTemplate.R:607-616) against the
  engine's list; resolution itself is in the app engine (outside the slice).
- pandoc `from=`: the slice's own pandoc calls read DOCX (.tinytex_docx_to_pdf, .pandoc_docx_to_pdf: input
  inferred from the .docx extension) or the qualification Markdown written through `.md_escape()` (F-24 covers what
  that escaping misses); the app's `pandoc_convert(from = pandoc_markdown_reader())` (utils_export.R:101-110) is
  outside the slice.
- XML escaping in templates: values are set through xml2 (`xml_add_sibling(..., "w:t", value)`), which escapes
  `&`, `<`, `>`; C0 controls are stripped (`.tv_split_word_chunks`, `.xml_safe_text`); no injection path found.

Cross-slice confirmations requested by the coordinator (t-core-2):
- F-t-core-2-24 (export half, R/exportFunctions.R:88-92): CONFIRMED -> F-c4-tmpl-export-25. The import half lives in R/DTAColumnSpecCollection-class.R (not in this slice; not re-checked).
- F-t-core-2-39 (R/exportValidationReport.R:116-132): CONFIRMED -> F-c4-tmpl-export-37; also F-38 (assets silently empty) in the same function.
- F-t-core-2-17 (.format_scalar_value): not in this slice's files; the slice's only direct caller, .md_info_lines() (R/exportDocuments.R:747-765), calls it element by element on scalars, so the vector/list defect is not reachable through that caller (other callers not traced).
- F-t-core-2-22, -23, -27, -30, -32, -36, -37, -41: in files outside this slice; not re-checked. (F-c4-tmpl-export-23 notes the slice's own call into the stale-object upgrade layer of F-t-core-2-27.)

## Coverage

Fully read (every line): R/qualification.R, R/qualification-report.R, R/exportDocuments.R,
R/documentBuilders.R, R/documentData.R, R/exportMarkdown.R, R/exportFunctions.R,
R/exportTemplateBlocks.R, R/exportTemplateDocx.R, R/exportTemplateRegions.R,
R/exportValidationReport.R, R/createTemplateRepo.R, R/lintTemplateRepo.R, R/run_dta_app.R,
R/validateTemplate.R, R/templateGuide.R, R/templateGuide-collect.R;
inst/qualification/tests/helper-00-qa.R, inst/qualification/tests/setup-qualification.R,
.github/scripts/qualification_verdict.R, inst/extdata/ci/* (3 files),
inst/extdata/template-repo-skeleton/export-templates/README.md.

Read in part (only what the slice's code depends on): inst/qualification/requirements/_index.yaml,
inst/qualification/deviations.yaml (statuses), inst/qualification/README.md (verdict/review sections),
inst/qualification/docs/validation-plan.md (acceptance criteria), inst/qualification/docs/maintaining.md
(skip rule), .github/workflows/qualification.yml (run + gate steps), tests/testthat/test-qualification.R
(inventory tests), inst/qualification/tests/test-OQ-EXPORT-regions.R (fallback usage),
tests/testthat/test-templateGuide-collect.R (party-slot test), inst/shiny/dta_app/R/party_profiles.R
(reader warnings, apply_party_selections), inst/shiny/dta_app/R/template_core.R (reader kind check,
party step), inst/shiny/dta_app/R/utils_export.R (app PDF route), inst/shiny/dta_app/app.R:10140-10190,
R/formattingHelpers.R (.extract_signatories, .format_document_date), R/DTAFile-class.R (count printing),
R/DTADataSetFile-class.R:325-354, R/DTAMatrixSpec-class.R:170-347, R/DTAColumnSpec-class.R (constructor,
nullable), R/DTAMetaData-helpers.R (sections validator), man/write_template_guide.Rd, DESCRIPTION, CHANGELOG [Unreleased],
inst/extdata/template-repo-skeleton/README.md (sections 1585-1660, 1885-1905 and grep hits),
inst/extdata/agent-skills/dta-template-authoring/references/linting.md (15-140, 205-240),
inst/extdata/agent-skills/.../references/repository-architecture.md (205-215).

Not read: inst/extdata/agent-skills/dta-template-authoring/SKILL.md and references/composition.md in full,
the rest of template-repo-skeleton/README.md (~1,700 lines), the skeleton/library YAML bodies beyond
their party_slots, inst/extdata/word-templates/*.docx (binary).
