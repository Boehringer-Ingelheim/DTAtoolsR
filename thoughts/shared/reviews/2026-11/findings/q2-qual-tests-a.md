# Findings — q2-qual-tests-a (qualification suite tests, part A)

Reviewer key: `q2-qual-tests-a`. Tree: `$SNAP` (pinned snapshot, plain files).
Slice: `scratchpad/slices/q2-tests-a.txt` — 26 test files under
`inst/qualification/tests` (IQ-ENV, OQ-APP-*, OQ-CONT-*, OQ-CORE, OQ-EXPORT-*, OQ-FILE-*).

Read first (context, all completely): `inst/qualification/docs/maintaining.md`,
`docs/validation-plan.md`, `tests/helper-00-qa.R`, `helper-app.R`, `helper-container.R`,
`helper-generators.R`, `helper-matrix.R`, `helper-parity.R`, `helper-parse.R`,
`setup-qualification.R`, `requirements/_index.yaml`, `_api.yaml`, `REQ-ENV.yaml`,
`REQ-CORE.yaml`, and the runner parts of `R/qualification.R` the CORE/ENV cases call
(lines 1-1110, 1350-1575, 1700-2031), `.github/workflows/qualification.yml`,
`.github/scripts/qualification_verdict.R`.

All 26 files of the slice were read completely (see the Coverage section at the end).
Nothing was executed: no `Rscript`, no server, no browser (brief, rules of engagement), so
every statement about what a test "would" do is from reading the test and the code it calls.
Severity: `blocker` is used once (F-17), for a high-risk requirement whose central claim no
step can fail on; `major` for a high-risk requirement most of whose scope is unasserted, or
a case whose named purpose is unverifiable; the rest `minor` / `nit`. No finding shows wrong
product behaviour: these are findings about what the suite can and cannot detect.

Runner facts every finding below leans on (verified by reading `R/qualification.R`):
- a test case's status is `skip` when it has a skip and no failure/error (l.421-429);
  the verdict is `FAIL` only for fail/error/file_error, a failed meta check or a
  `not_reproduced` deviation; any skip gives `PASS WITH NOT-EXECUTED TESTS` (l.1358-1370);
- CI fails the job only when the verdict starts with `FAIL`
  (`.github/scripts/qualification_verdict.R`), so **a skip is green in CI**;
- a requirement is `verified` as soon as ANY one of its test cases passed and none
  failed (l.725-735) — a skipped sibling case does not show.

---

## IQ — test-IQ-ENV.R

### F-q2-qual-tests-a-01  [major] [test-cheat]
- Where: inst/qualification/tests/test-IQ-ENV.R:299-306
- What: IQ-ENV-019 skips exactly when REQ-ENV-019 is violated, and the one assertion left behind it is a tautology, so the case can never fail.
- Evidence:
  ```r
  n <- if (is.null(info$results)) 0L else nrow(info$results)
  if (n == 0L) {
    testthat::skip("no vignettes are installed with this copy of the package")
  }
  qa_check(sprintf("%d vignette(s) are installed", n), n > 0)
  ```
  REQ-ENV-019 says "At least one vignette **shall** be installed". `n == 0` is the violation; it is turned into a skip, and `n > 0` after that guard is always TRUE.
- Failure scenario: the vignettes are dropped from the build (e.g. `vignettes/` added to `.Rbuildignore`, or the knitr step broken) -> IQ-ENV-019 = skip -> verdict `PASS WITH NOT-EXECUTED TESTS` -> CI green. It already fires on every CI run: `qualification.yml` installs with `R CMD INSTALL --install-tests --build .` from the source directory, the tree has no `inst/doc`, and `R CMD INSTALL` does not build vignettes, so the installed copy has none (inferred from R's install semantics; not run).
- Confidence: verified-by-reading (test, REQ text, verdict logic, workflow); the "always skips in CI" part is inferred from how `R CMD INSTALL` treats a source directory.
- Suggested direction: assert `n > 0` unconditionally (fail), or reword REQ-ENV-019 as "recorded, not required" like REQ-ENV-023 and stop calling it a `shall`.

### F-q2-qual-tests-a-02  [major] [test-weak]
- Where: inst/qualification/tests/test-IQ-ENV.R:245-278
- What: IQ-ENV-016 (risk: high, integrity) passes vacuously on a manifest with no entries and degrades to a skip when the manifest is missing or its header line is damaged, so the tamper check can be switched off by touching the manifest itself.
- Evidence:
  ```r
  if (!file.exists(manifest_path)) { testthat::skip("no baseline/inst-files.sha256 manifest shipped ...") }
  if (is.na(algo) || !algo %in% c("sha256", "md5")) { testthat::skip("the baseline manifest header does not name ...") }
  lines <- lines[!startsWith(lines, "#") & nzchar(lines)]
  ...
  qa_step(sprintf("all %d manifest-listed files under inst/ match their recorded hash", sum(present)), character(0), mismatched)
  ```
  There is no floor on `length(lines)`; with zero entries both `missing` and `mismatched` are `character(0)` and the step reads "all 0 manifest-listed files ... match".
- Failure scenario: an installed `inst/shiny/dta_app/app.R` is altered and `baseline/inst-files.sha256` is truncated to its header (or one entry is deleted, or the file is removed) -> IQ-ENV-016 passes ("all 0 ...") or skips; nothing fails. The manifest is deliberately not covered by itself (maintaining.md l.160-161), so nothing else notices. A removed entry for the altered file is likewise invisible: the case checks listed files only, never that the installed tree holds no file the manifest does not list.
- Confidence: verified-by-reading
- Suggested direction: fail (not skip) on a missing/unreadable manifest, require a minimum entry count, and compare the listed set against `list.files(system.file(package=), recursive=TRUE)` minus `qualification/baseline/`.

### F-q2-qual-tests-a-03  [minor] [test-cheat]
- Where: inst/qualification/tests/test-IQ-ENV.R:442-450 (helper: R/qualification.R:1046-1048)
- What: IQ-ENV-024 wraps the function under test in `qual_safe()`, which turns any error or warning into `NULL`, and `NULL` is one of the two accepted answers.
- Evidence:
  ```r
  backend <- qual_safe(dta_pdf_backend(), NULL)
  qa_check("dta_pdf_backend() reports NULL or a list naming a usable backend",
    is.null(backend) || (is.list(backend) && is.character(backend$name) && nzchar(backend$name)), ...
  qual_safe <- function(expr, default = NA) tryCatch(expr, error = function(e) default, warning = function(w) default)
  ```
- Failure scenario: mutation `dta_pdf_backend <- function() stop("boom")` (or one that warns on a half-installed TinyTeX) -> `backend` is `NULL` -> recorded as "no PDF backend available", case passes, REQ-ENV-024 "verified". "usable" is not checked either: any list with a non-empty `name` passes.
- Confidence: verified-by-reading
- Suggested direction: call `dta_pdf_backend()` unguarded; an error must fail the case.

### F-q2-qual-tests-a-04  [minor] [test-weak]
- Where: inst/qualification/tests/test-IQ-ENV.R:45-63, 132-136, 405-416, 418-440 (also 79-97, 139-169, 191-196)
- What: Four requirements (REQ-ENV-002, -007, -022, -023) are "verified" by `qa_check(<text>, TRUE, ...)` — a literal TRUE — and several more by conditions that cannot be false in a running R (`nzchar(R.version$platform)`, `isNamespaceLoaded("DTAtools")` from inside the package's own runner).
- Evidence:
  ```r
  qa_check("the Built field is recorded when present", TRUE, detail = desc$Built %||% "not present ...")
  qa_check("free disk space at the write location is recorded", TRUE, detail = if (is.na(free_bytes)) "could not be determined on this platform" else ...)
  qa_check("pandoc availability is recorded", TRUE, ...)
  ```
  maintaining.md l.263-265: "A test that cannot fail is worse than no test, because it reports coverage it does not have."
- Failure scenario: REQ-ENV-022 ("free disk space ... shall be recorded"): the Windows branch shells out to `wmic`, which is absent on this machine (`where wmic` finds nothing on Windows 11 26200); `system2()` errors, the `tryCatch` yields `NA`, the step records "could not be determined on this platform" and the requirement is reported verified with nothing recorded. No mutation of the recording code can fail any of these steps.
- Confidence: verified-by-reading (`where wmic` was checked on this machine; behaviour on other hosts not checked)
- Suggested direction: either make them real (`!is.na(free_bytes)` with a documented platform exemption, replace `wmic` by `Get-CimInstance`/`fsutil`) or record them as environment evidence outside the pass/fail traceability.

### F-q2-qual-tests-a-05  [minor] [test-weak]
- Where: inst/qualification/tests/test-IQ-ENV.R:308-379; inst/qualification/requirements/REQ-ENV.yaml:207-234
- What: IQ-ENV-020 checks only the 17 hand-listed option names, so an option added to the package later is outside the check although REQ-ENV-020 says "Every package option read via getOption("DTAtools....")"; the requirement's `source:` line numbers are stale.
- Evidence: `for (name in names(defaults)) { ... sites <- site_defaults(name) ...}` — the walker is only ever asked for names the test already knows. The walker also matches only `identical(e[[1]], quote(getOption))` with `length(e) >= 3` and a literal name.
  REQ source says `R/00_helpers.R:159 (DTAtools.stream_threshold ...)`, `:175`, `:277`, `:364`, `R/DTA-class.R:782/783/791`, `R/importConversion.R:870/884`, `R/streamingValidation.R:895`; the sites are at 00_helpers.R:467/305/359/428, DTA-class.R:1018/1019/1027, importConversion.R:976/990, streamingValidation.R:931.
- Failure scenario: mutation: add `getOption("DTAtools.new_limit", 5L)` (or change behaviour through `base::getOption("DTAtools.max_errors", 1L)`, which the walker does not see) -> IQ-ENV-020 still passes and the new/changed default is unpinned. `DTAtools.temp_dir` (R/00_helpers.R:1027) is already read by the package and appears in neither the test nor the requirement.
- Confidence: verified-by-reading
- Suggested direction: let the walker collect every `DTAtools.*` name it meets and compare that set with `names(defaults)`; refresh the `source:` references (or drop line numbers).

### F-q2-qual-tests-a-06  [nit] [test-weak]
- Where: inst/qualification/tests/test-IQ-ENV.R:65-121 (helper R/qualification.R:1054-1096)
- What: IQ-ENV-003/005/006 take the whole verdict (`satisfied`) from the package's own `qual_dependency_check()`, and no case in the suite ever shows that helper reporting an unsatisfied dependency.
- Evidence: `unsatisfied <- rows$package[!rows$satisfied]; qa_step(..., character(0), unsatisfied)`; the helper strips the operator with `sub("^[><=!]+", "", required)` and always compares with `>=`.
- Failure scenario: mutation in the helper: `satisfied <- TRUE` for a non-NA `installed` (or `compareVersion(required_version, installed)` reversed with an old minimum) -> all three cases still pass; the requirement "satisfies its declared minimum" is decided by code no test has seen fail.
- Confidence: verified-by-reading
- Suggested direction: compute the comparison in the test from `packageDescription()` + `utils::packageVersion()` (base R), or add a CORE case feeding the helper a known-unsatisfied DESCRIPTION.

---

## OQ-CORE — test-OQ-CORE-runner.R

### F-q2-qual-tests-a-07  [major] [test-weak]
- Where: inst/qualification/tests/test-OQ-CORE-runner.R:91-97
- What: OQ-CORE-001 recomputes one hash — the first line of the manifest — and that line is never `run.log`, the one file whose hash the suite's own documentation says was once wrong.
- Evidence:
  ```r
  first <- listed[[1]]
  qa_step(sprintf("the recorded hash of %s matches the file", first),
    qa_hash_file(file.path(bundle$bundle_dir, first)), sub("  .*$", "", readLines(manifest)[[1]]))
  ```
  The manifest is written in `sort(list.files(dir, recursive = TRUE))` order (R/qualification.R:1423), so line 1 is `SUMMARY.txt` (C collation) or `report/qualification-report.md` (de_DE). maintaining.md l.421-424: "A guard that checks one item out of many checks nothing. The bundle verification originally recomputed a single hash from the manifest and passed while the log's own entry was wrong. If you are verifying a set, verify the set."
- Failure scenario: mutation: call `qual_hash_bundle(bundle_dir)` before `qual_log("run_end", verdict)` / before the log is closed (R/qualification.R:2014-2018) -> `run.log`'s recorded hash no longer matches the file, `sha256sum -c` fails for the auditor, OQ-CORE-001 passes. Same for any wrong hash on lines 2..n.
- Confidence: verified-by-reading
- Suggested direction: recompute every listed hash and compare the two vectors.

### F-q2-qual-tests-a-08  [major] [test-weak]
- Where: inst/qualification/tests/test-OQ-CORE-runner.R:165-217 (code: R/qualification.R:1358-1370)
- What: OQ-CORE-004 claims REQ-CORE-004 ("FAIL whenever any executed test case fails **or errors**") but only ever hands `qual_verdict()` the status `fail`; `error` and `file_error` are never tried.
- Evidence: the only failing-status input is `data.frame(status = c("pass", "fail"))` (l.186); the code under test is `failed <- sum(tests_df$status %in% c("fail", "error", "file_error"))`.
- Failure scenario: mutation: `failed <- sum(tests_df$status == "fail")` -> a test case that throws (status `error`) or a stage that could not run (`file_error`, `qual_stage_abort_row`) yields verdict `PASS`; every step of OQ-CORE-004 still passes, and no other case in the suite calls `qual_verdict()` (grep over `inst/qualification`). OQ-CORE-005 only shows that M10 turns false, not that the verdict follows.
- Confidence: verified-by-reading
- Suggested direction: add `c("pass","error")` and `c("pass","file_error")` steps, and one end-to-end step on the fixture with the broken file asserting the verdict.

### F-q2-qual-tests-a-09  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-CORE-runner.R:10-22, 205-216
- What: The step "a failing case does not stop the ones after it" cannot fail, because in the fixture the failing case is the last one; the comment describes the opposite order.
- Evidence:
  ```r
  # A failing step must not stop the file: the second case in the fixture runs
  # after the first one fails ...
  qa_step("a failing case does not stop the ones after it", c("OQ-XX-001", "OQ-XX-002"), sort(tests$tc_id))
  ```
  Fixture order (l.13-19): `OQ-XX-001` passes (`qa_step("two is two", 2L, 2L)`), `OQ-XX-002` fails. Nothing runs after the failure. Each fixture case also has exactly one step, so "a failing step shall be recorded and the run shall continue" (within a case) is not exercised either.
- Failure scenario: mutation: make `qa_step()` call `testthat::fail()` and then `stop()` (abort the case), or run the stage with a stop-on-first-failure reporter -> both ids are still reported, the step passes.
- Confidence: verified-by-reading
- Suggested direction: put the failing case first and give it a second step after the failing one; assert that step was recorded.

### F-q2-qual-tests-a-10  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-CORE-runner.R:256-311 (code: R/qualification.R:649-680)
- What: REQ-CORE-006 ("reported as reproduced ... shall fail the run when it is not") is traced to a case that never calls `qual_deviation_status()`, the function that turns the recorded expectations into `reproduced` / `not_reproduced` for the verdict and the report.
- Evidence: OQ-CORE-006 asserts only the expectation type/message of `qa_known_deviation()`; OQ-CORE-004 feeds `qual_verdict()` a hand-made `data.frame(status_run = "not_reproduced")`. `qual_deviation_status` appears in no file under `inst/qualification/tests` (grep).
- Failure scenario: mutation: change the marker to `paste0("^", id, " reproduced$")` (or reword the helper's message to "Deviation DEV-nnn reproduced") -> every deviation is `not_executed`, the report's deviation section and `n_reproduced` are wrong, acceptance criterion 4 ("every open deviation still reproduces") is no longer evaluated; all CORE cases pass.
- Confidence: verified-by-reading
- Suggested direction: run `qual_deviation_status()` on the fixture's `exps` and assert `c(DEV-901 = "reproduced", DEV-902 = "not_reproduced")`.

### F-q2-qual-tests-a-11  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-CORE-runner.R:117-131, 309-310, 351-352
- What: Three refusals are asserted as "some condition was raised" with no class or message, and the hash comparison in OQ-CORE-002 guards a directory the second run was never going to write to.
- Evidence:
  ```r
  qa_check("a run whose bundle directory already exists aborts", inherits(err, "condition"))
  qa_step("the first run's results are untouched", before, qa_hash_file(marker))
  qa_check("an unregistered deviation id aborts", inherits(err, "condition"))
  qa_check("a requirement missing its risk rating aborts the read", inherits(err, "condition"))
  ```
  The second run's bundle directory is one of the freshly created empty `claimed` directories, not `first$bundle_dir`, so `marker` cannot change whatever the runner does.
- Failure scenario: mutation: the second `run_qualification()` call errors for any unrelated reason (a later step failing on the pre-created empty directory, an argument check) -> "aborts" passes although the documented refusal (`cli_abort("An evidence bundle already exists at ...")`, R/qualification.R:1839-1841) never ran; the "untouched" step passes under every mutation because nothing addresses `first$bundle_dir`. For l.351: `qual_requirements_df(broken)` erroring because of anything passes; the exported `qualification_requirements(dir = broken)` named by REQ-CORE-007 is not the function called.
- Confidence: verified-by-reading
- Suggested direction: match the package-authored cli text ("An evidence bundle already exists", "do not satisfy their schema", "is not an open deviation") and, for OQ-CORE-002, pre-seed the claimed directory with a file and assert it is unchanged.

### F-q2-qual-tests-a-12  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-CORE-runner.R:321-336
- What: "returns every requirement shipped with the package" (REQ-CORE-007) is compared against an independent reading for the CORE file only; the other 16 requirement files are never counted.
- Evidence: `core_file <- file.path(qa_suite_root(), "requirements", "REQ-CORE.yaml")` ... `qa_step(..., sort(declared), sort(qualification_requirements(area = "CORE")$id))`.
- Failure scenario: mutation: `qual_yaml_files()` pattern narrowed (e.g. `^REQ-[A-Z]{4,}\\.ya?ml$`, dropping REQ-VAL/REQ-ENV/REQ-APP...) or a `next` that drops the last entry of a long file -> `nrow(reqs) > 0` and the CORE comparison still pass. (M3 would catch a wholly dropped file in a full run, but this case, which carries the requirement, would not.)
- Confidence: verified-by-reading
- Suggested direction: run the same regex over every `REQ-*.yaml` and compare the union with `qualification_requirements()$id`.

---

## OQ-APP — test-OQ-APP-workflow.R, -agreement.R, -variants.R, -server.R

(REQ-APP.yaml read completely; app code opened where a finding depends on it:
`inst/shiny/dta_app/app.R` l.1722-1760, 1904-1922, 2800-2951, 3184-3310, 3780-4010,
5800-5855, 6095-6353; `R/utils_dta.R` l.580-690; `R/template_core.R` l.396-456;
`R/run_dta_app.R` whole file.)

### F-q2-qual-tests-a-13  [major] [test-cheat]
- Where: inst/qualification/tests/test-OQ-APP-workflow.R:100-126 (code: inst/shiny/dta_app/app.R:6156-6192)
- What: The only case traced to REQ-APP-017 sets an input the application does not have, selects no term and asserts only that nothing threw, so it never reaches the vocabulary resolver and checks none of the requirement.
- Evidence:
  ```r
  session$setInputs(col_vocab_id = "no_such_vocabulary")           # test, l.112
  outcome <- tryCatch({ session$setInputs(col_vocab_apply = 1); "reported" }, error = function(e) "raised")
  sel <- as.character(input$col_vocab_terms_sel %||% character(0)) # app, l.6157
  if (length(sel) == 0) { rv$col_msg <- list(ok = FALSE, error = "Select at least one term, ..."); return() }
  vocabulary_resolver(dta_template_index_cached())(input$col_vocab_ref)   # app, l.6164 -- never reached
  ```
  `col_vocab_id` occurs nowhere under `inst/shiny` (grep); the app reads `input$col_vocab_ref`. With no `col_vocab_terms_sel` the observer returns at l.6159-6162, before any reference is resolved. Nothing asserts a message (`rv$col_msg`) either.
- Failure scenario: mutations that all pass: drop the `tryCatch(..., error = function(e) NULL)` at app.R:6163-6166 so an unresolvable reference raises; delete the whole `col_vocab_apply` observer; stop clearing `pf$pattern`; write the terms straight into `rv$dta` instead of the pending prefill. "replace the pending allowed values", "clear any pending pattern", "without changing the saved specification until the column is saved" and "report a message" are all unasserted.
- Confidence: verified-by-reading
- Suggested direction: open the column editor on a fixture library with one vocabulary, set `col_vocab_ref` + `col_vocab_terms_sel`, assert `rv$col_prefill$values`/`$pattern` and an unchanged `rv$dta`; for the negative path set `col_vocab_ref` to an unknown id with a term selected and assert `rv$col_msg$error`.

### F-q2-qual-tests-a-14  [major] [test-weak]
- Where: inst/qualification/tests/test-OQ-APP-server.R:46-67 (code: R/run_dta_app.R:31-43)
- What: OQ-APP-003's "names every missing package" step is satisfied by the product name in the same message, and `shiny::runApp()` is not intercepted, so neither half of REQ-APP-003 can fail the case.
- Evidence:
  ```r
  c(bslib = grepl("bslib", text, fixed = TRUE), DT = grepl("DT", text, fixed = TRUE))      # test
  "The DTAtools app needs additional package{?s}: {.pkg {missing}}."                      # code
  ```
  "DTAtools" contains "DT", so the `DT` element is TRUE whatever `missing` holds.
- Failure scenario: mutation `missing <- missing[1]` (report one package at a time — exactly what the test's comment says must not happen) -> message "The DTAtools app needs additional package: bslib." -> `bslib` TRUE, `DT` TRUE -> pass. Mutation: remove the guard altogether -> `run_dta_app(launch.browser = FALSE)` calls the real `shiny::runApp()` and blocks; the qualification run hangs instead of recording a failure ("shall never call shiny::runApp()" has no assertion).
- Confidence: verified-by-reading
- Suggested direction: mock `shiny::runApp` (helper-app.R l.214-217 already says this works) to record the call, assert it was not called, and match `\\bDT\\b`/the `{.pkg}` list rather than a substring.

### F-q2-qual-tests-a-15  [major] [test-cheat]
- Where: inst/qualification/tests/test-OQ-APP-server.R:32-44 (helper: inst/qualification/tests/helper-app.R:27-45)
- What: OQ-APP-002 never calls `run_dta_app()`; its three steps restate what its own skip guard already established.
- Evidence:
  ```r
  dir <- qapp_skip_unless_app_installed()
  qa_check("the application directory is found", nzchar(dir))
  qa_step("and it holds the application's entry point", TRUE, file.exists(file.path(dir, "app.R")))
  # helper: if (nzchar(dir) && file.exists(file.path(dir, "app.R"))) dir else ""  ... skip when !nzchar(dir)
  ```
  REQ-APP-002: "run_dta_app() shall resolve the application directory ... and pass it, together with launch.browser and any supplied port, to shiny::runApp()". No step observes an argument reaching `runApp`.
- Failure scenario: mutation `args <- list(appDir = getwd(), launch.browser = TRUE)` (wrong directory, caller's `launch.browser` and `port` ignored, `...` dropped) -> passes. When the app is absent the case skips, so the two first steps can only ever be TRUE.
- Confidence: verified-by-reading
- Suggested direction: `local_mocked_bindings(runApp = function(...) captured <<- list(...), .package = "shiny")`, call `run_dta_app(launch.browser = FALSE, port = 4321L, quiet = TRUE)` and compare `captured` with a literal list.

### F-q2-qual-tests-a-16  [major] [test-weak]
- Where: inst/qualification/tests/test-OQ-APP-workflow.R:59-98, 188-230
- What: OQ-APP-021 and OQ-APP-025 create a document from whichever template the machine's ambient library lists first and assert only "a document exists and has some title or some supplier", so REQ-APP-015 and REQ-APP-016 are traced to cases that check almost none of their text.
- Evidence:
  ```r
  creation <- index[index$kind == "dta_creation_template", , drop = FALSE]   # no qapp_with_library(): ambient sources
  session$setInputs(template_select_name = creation$id[[1]], ...)
  qa_check("carrying metadata from the template rather than nothing",
    nzchar(meta@title %||% "") || length(meta@supplier %||% list()) > 0)
  ```
  REQ-APP-015 also requires (a) a chosen party profile to overwrite the slot's affiliation and contacts, and (b) a `datasets[]` reference that is absolute or climbs out of the template directory to "resolve to nothing". No party input is set, and `resolve_template_dataset_path` is referenced by no file under `inst/qualification/tests` (grep; only `tests/testthat/test-shinyapp-template.R` has it). REQ-APP-016 requires parent options carried forward and "the child's own dataset patch" applied; OQ-APP-025 asserts `nzchar(title) || length(supplier) > 0 || length(receiver) > 0`.
- Failure scenario: mutations that pass: `.dta_path_under()` returns TRUE always (path traversal in a template reference); `collect_party_selections()` ignored; the child's `patch:` (the bundled `biomarker_gf_acme` adds column `ACMEID`) never applied; base metadata replaced by a constant title. Both cases also skip when the ambient library has no (extending) creation template, and `DTATOOLS_TEMPLATE_SOURCES` set on the host changes which template is under test.
- Confidence: verified-by-reading
- Suggested direction: pin a fixture library with `qapp_with_library()` (as the backends cases do), drive one creation template + party profile, and compare title/supplier/columns with literals read off the fixture; add direct steps for an absolute and a `../` dataset reference.

### F-q2-qual-tests-a-17  [blocker] [test-weak]
- Where: inst/qualification/tests/test-OQ-APP-server.R:322-346
- What: OQ-APP-014 is meant to show that an import-axis violation alone fails a dataset (REQ-APP-011), but its fixture also carries column-specification errors, so the status would be `fail` without the import axis.
- Evidence: `session$setInputs(up_1_1 = qapp_file_input("clinical_data_error_import.csv"))` ... `qa_step("a delivery whose only fault is unparseable values is reported as failing", "fail", ...)`. The suite's own PQ-PERF-005 states the counts for that file: `c(ok = FALSE, columnspec = 5L, rule = 0L, import = 4L)` (test-PQ-PERF-workflows.R:132); AGE, HEIGHT, WEIGHT and BMI are all `nullable: false` in `clinical_dta.yaml`, so every unparseable cell (and the injected `NA` BMI) is also a column-specification finding. The second step only checks `"import" %in% msgs$source`, which the engine reports regardless of how the app maps it.
- Failure scenario: mutation in the app's status mapping: ignore `n_import_errors` (fail iff columnspec or rule errors) -> status is still `fail` from the 5 columnspec errors, messages still contain `import` -> OQ-APP-014 passes while a delivery whose only fault is an unparseable value in a nullable column would be shown green. The app's own comment names this exact failure as the reason the import count is weighed (`inst/shiny/dta_app/R/utils_dta.R:1345-1349`: "reading only those two axes paints it green while ok is FALSE"), so the case that carries REQ-APP-011 (risk high) cannot detect the regression it was written for.
- Confidence: verified-by-reading (fixture diffed against clinical_data.csv; counts taken from PQ-PERF-005, not from a run)
- Suggested direction: use a specification whose numeric column is nullable (or a purpose-built two-column fixture) so that `columnspec = 0`, and assert that the import axis is the only source reported.

### F-q2-qual-tests-a-18  [major] [test-weak]
- Where: inst/qualification/tests/test-OQ-APP-server.R:943-1043 (code: inst/shiny/dta_app/R/utils_dta.R:630-660)
- What: OQ-APP-060 (REQ-APP-050, risk high, security) probes only "not a data frame", "outside the temporary directory altogether", "a directory" and "does not exist"; the three layout rules the requirement states are never probed with a real file inside `tempdir()`.
- Evidence: the refusing inputs are `list(...)`, `datapath = spec_path` (the installed fixture), a directory, and an unlinked file. The code has three further checks no input reaches: `identical(dirname(dirname(np)), nt)`, `grepl("^[0-9a-f]{24}$", basename(dirname(np)))`, `grepl("^[0-9]+([.][[:alnum:]]+)?$", basename(np))`.
- Failure scenario: mutation: delete the 24-hex and file-name checks, or relax the first to `startsWith(np, nt)` (any existing file anywhere under `tempdir()`, e.g. another session's working copy under `tempdir()/qa-.../x.yaml`) -> every step of OQ-APP-060 still passes. The requirement text `<tempdir>/<24 lowercase hexadecimal characters>/<digits>[.<ext>]` is therefore unverified in this suite.
- Confidence: verified-by-reading
- Suggested direction: add three negative controls built from real files: `tempdir()/<24hex>/name.yaml` (bad file name), `tempdir()/qa-x/0.yaml` (bad folder), `tempdir()/<24hex>/sub/0.yaml` (too deep) and one upper-case hex folder.

### F-q2-qual-tests-a-19  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-APP-server.R:109-136
- What: OQ-APP-006 replaces a document by one whose dataset ids differ, so "no status from the previous document" is met by construction, and the active dataset is not checked at all.
- Evidence: first document `clinical_dta.yaml` (dataset `clinical_data`), second `gf_dataset.yaml`; `qa_step(..., character(0), intersect(first_status, names(rv$status)))`. REQ-APP-006: "the uploads, the per-dataset status map and the active dataset ... shall carry none of their prior values forward"; its note names the risk as a stale verdict "read against the wrong specification".
- Failure scenario: mutation in `apply_loaded()` (app.R:1917-1922): keep the previous status for dataset ids present in both documents (`pending` only for new ids) -> a revised specification of the same study loads with `clinical_data = "pass"` carried over; OQ-APP-006 passes. `rv$active` left pointing at the old dataset also passes.
- Confidence: verified-by-reading
- Suggested direction: reload a second specification that reuses the id `clinical_data` after a passing check and assert `pending` and the new `rv$active`.

### F-q2-qual-tests-a-20  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-APP-server.R:220-255
- What: OQ-APP-010 "replaces" a bound delivery with a byte-identical file, so it cannot tell whether the bound table was replaced before the confirmation, after it, or never.
- Evidence: both uploads are `qapp_file_input("clinical_data.csv")`; the steps assert only `!is.null(rv$pending_upload)`, its indices, `is.null(rv$pending_upload)` after `confirm_overwrite`, and `length(rv$uploads) == 1L`.
- Failure scenario: mutation: `handle_upload()` binds the new table immediately and also sets `rv$pending_upload` (the silent replacement REQ-APP-009's note describes) -> passes; mutation: `confirm_overwrite` clears the pending record without loading anything -> passes.
- Confidence: verified-by-reading
- Suggested direction: stage a different file under the same name (`qapp_upload_value(<error fixture>, "clinical_data.csv")`) and compare a cell of the bound table before and after the confirmation.

### F-q2-qual-tests-a-21  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-APP-server.R:138-195
- What: OQ-APP-007 and OQ-APP-008 claim the document and the status map but read only `names(rv$status)`; OQ-APP-008 never shows that nothing is removed before the confirmation.
- Evidence: titles "drops it from the document and the status map"; every step is of the form `"second_ds" %in% names(rv$status)` / `before, names(rv$status)`. `rv$dta` is not read in either case.
- Failure scenario: mutation in `add_ds_save` outside edit mode: `rv$dta <- r$value` executed before `req(editing())` (document changed, status untouched) -> OQ-APP-007 passes. Mutation: `remove_dataset` (the request) already removes the dataset, or `remove_dataset_confirm` forgets `rv$dta <- r$value` -> OQ-APP-008 passes.
- Confidence: verified-by-reading
- Suggested direction: assert `names(DTAtools::datasets(rv$dta))` alongside the status names, and add a step between request and confirmation.

### F-q2-qual-tests-a-22  [minor] [test-cheat]
- Where: inst/qualification/tests/test-OQ-APP-server.R:426-446
- What: In OQ-APP-026 the step "both columns remain, unchanged" compares the input object with itself, and "naming the id that already exists" never looks for the id.
- Evidence:
  ```r
  cols_before <- DTAtools::datasets(dta, "clinical_data")@specs@columns
  res <- set_column(dta, "clinical_data", id = "STUDYID", old_id = "VISIT")
  cols_after <- DTAtools::datasets(dta, "clinical_data")@specs@columns      # same `dta`, never reassigned
  qa_step("naming the id that already exists", TRUE, grepl("already exists", res$error, fixed = TRUE))
  ```
  `dta` is an R value; `dta_set_column()` cannot change the caller's copy, so `cols_after` is `cols_before` under every implementation (and only `@label` is compared).
- Failure scenario: mutation: the refusal returns `list(ok = FALSE, error = "... already exists", value = <document with STUDYID overwritten>)`, or the message drops the id -> all steps pass. The control ("renaming onto a free id succeeds") asserts only `isTRUE(ok$ok)`, not that `VISIT_CODE` exists and `VISIT` does not.
- Confidence: verified-by-reading
- Suggested direction: assert `is.null(res$value)` (or drive `col_save` in `testServer` and compare `rv$dta`), `grepl("STUDYID", res$error)`, and the column ids of `ok$value`.

### F-q2-qual-tests-a-23  [minor] [doc-wrong]
- Where: inst/qualification/requirements/REQ-APP.yaml:286-300; inst/qualification/tests/test-OQ-APP-server.R:453-488 (code: inst/shiny/dta_app/app.R:5838-5840, 6328-6334)
- What: REQ-APP-020 and OQ-APP-027 say the column form "exposes no field" for `examples`, but the form has an Examples field and a save through it writes what the field holds.
- Evidence: requirement: "shall carry that column's `examples` and `colclass` forward unchanged, because the form exposes no field for either". App: `textAreaInput("col_examples", "Examples (one per line)", value = g("examples"), ...)` and, in `col_save`, `examples = if (is.null(input$col_examples)) NULL else strsplit(input$col_examples, "\n", fixed = TRUE)[[1]]` with the comment "otherwise exactly what the field holds, so a stale example can be cleared here". OQ-APP-027 calls `dta_set_column()` with `examples` omitted, a call the rendered form never makes.
- Failure scenario: the requirement is false for the shipped app (clearing the field clears the examples, by design); the test passes because it exercises the helper's `examples = NULL` branch, not "saving a column through the editor form". A regression in the handler's `examples` wiring (e.g. `strsplit("", "\n")[[1]]` turning into a kept stale value) is outside the case.
- Confidence: verified-by-reading
- Suggested direction: rewrite REQ-APP-020 (colclass is carried, examples are what the field holds) and drive the save through `col_save` with and without `col_examples`.

### F-q2-qual-tests-a-24  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-APP-workflow.R:17-57
- What: OQ-APP-020's "well-formed HTML naming the dataset that was checked" is a substring search for "clinical" over the raw file, "refused" is any error, and the findings step is `n_messages > 0`.
- Evidence: `grepl("clinical", paste(readLines(written, warn = FALSE), collapse = " "), fixed = TRUE)`; the uploaded file is `clinical_data_error_all.csv` and the document title is "Clinical Data Specification", so the dataset id `clinical_data` need not appear. `unchecked <- tryCatch({ output$dl_msgs_html; "produced" }, error = function(e) "refused")`. Well-formedness is never tested (`xml2::read_html` repairs anything).
- Failure scenario: mutation: the report omits the dataset column/heading but prints the file name; or lists 1 of the N findings; or the pre-check download fails with an unrelated error -> all pass.
- Confidence: verified-by-reading
- Suggested direction: assert `parsed$targets`/the dataset id from `qa_html_report()`, a hand-counted `n_messages`, and a `shiny.silent.error`/validation class for the refusal.

### F-q2-qual-tests-a-25  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-APP-server.R:18-30, 87-107, 197-218, 566-648; test-OQ-APP-workflow.R:155-186
- What: Several requirement clauses in this file's traceability have no assertion behind them.
- Evidence (clause -> what the cases do):
  - REQ-APP-026 "removing a column ... shall, for a dataset that has data bound, re-validate" and "Reordering columns shall not re-validate": no case sets `col_up_click`/`col_down_click` (grep), and the only tabular `col_del_click` (OQ-APP-037) has no data bound.
  - REQ-APP-018 "leaving the document and its edit lock unchanged": OQ-APP-024 checks version, history and `rv$new_version_msg`, never the lock.
  - REQ-APP-001 "further arguments passed through to shiny::runApp()": OQ-APP-001 only reads `names(formals(run_dta_app))`.
  - REQ-APP-005 "select its first dataset ... every declared dataset's status": the fixture declares one dataset.
  - REQ-APP-009 "keyed by dataset id and handler index": `grepl("clinical_data", names(rv$uploads)[[1]])`, the index is not read.
- Failure scenario: mutations that pass: `col_del_click` leaves the status at `pass` after removing a column the data needs; `col_up_click` triggers `recheck_dataset()`; a refused blank version still calls the unlock; the launcher drops `...`; `apply_loaded()` activates the last dataset; uploads keyed by dataset only.
- Confidence: verified-by-reading
- Suggested direction: one step per clause; use `clinical_dta_multiple_files.yaml`/a two-dataset fixture for "first" and "every".

### F-q2-qual-tests-a-26  [minor] [inconsistency]
- Where: inst/qualification/requirements/REQ-APP.yaml:21-25; inst/qualification/tests/helper-app.R:22; inst/qualification/tests/test-OQ-APP-server.R:49 (code: R/run_dta_app.R:32, inst/shiny/dta_app/app.R:9)
- What: The requirement, the test comment and the skip helper say the application needs three packages; the launcher requires four and the app attaches the fourth unconditionally.
- Evidence: REQ-APP-001 "shall require the shiny, bslib and DT packages"; test "# The application needs three packages."; `qapp_required_packages <- function() c("shiny", "bslib", "DT")`; code `required <- c("shiny", "bslib", "DT", "shinyjs")` and `library(shinyjs)`.
- Failure scenario: on a host without `shinyjs`, `qapp_skip_unless_app_installed()` does not skip, every case that starts the server with `shiny::testServer(app, ...)` fails at app.R's `library(shinyjs)` and is reported as an error rather than as "dependency absent: shinyjs"; the requirement text is wrong either way.
- Confidence: verified-by-reading (the failure mode on such a host is inferred, not run)
- Suggested direction: add `shinyjs` to REQ-APP-001/-003 and to `qapp_required_packages()`.

## OQ-APP — test-OQ-APP-backends.R, test-OQ-APP-matrix.R

Both files read completely. They are the strongest of the APP files: expectations are
literals written from the requirement text or parsed out of the YAML with `yaml::yaml.load`,
negative controls are present (OQ-APP-043, -049), and refusals are checked against an
unchanged document. Only the following survived tracing (app code opened: app.R l.7690-7855).

### F-q2-qual-tests-a-27  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-APP-matrix.R:508-539; inst/qualification/tests/test-OQ-APP-backends.R:688-706 (code: inst/shiny/dta_app/app.R:7836-7840)
- What: The "saved unchanged leaves the layout as it was" steps of OQ-APP-052 and OQ-APP-070 cannot tell a save that changed nothing from a save that was refused, because a refused save also leaves the document unchanged and neither case reads `rv$mx_msg`.
- Evidence:
  ```r
  before <- as.list(datasets(rv$dta, "gene_counts")@matrix)
  session$setInputs(mx_save = 2)
  qa_check("an unchanged save leaves the matrix settings as they were", identical(before, as.list(datasets(rv$dta, "gene_counts")@matrix)))
  # app: r <- dta_set_matrix(...); if (!isTRUE(r$ok)) { rv$mx_msg <- list(ok = FALSE, error = r$error); return() }
  ```
  OQ-APP-052's second save is the only place in the suite where the layout form is submitted with every field reported the way a browser reports it (`mx_n_rows_max = NA_real_`, `mx_range_max = NA_real_`, empty texts as `""`); its acceptance is never asserted.
- Failure scenario: mutation in `dta_set_matrix()`: refuse a blank numeric that arrives as `NA` ("at most must be a number") -> in a browser no matrix layout without a maximum can be saved any more; OQ-APP-052 step passes (document unchanged), and so do the three "saved unchanged" steps of OQ-APP-070 for the same reason.
- Confidence: verified-by-reading
- Suggested direction: assert `is.null(rv$mx_msg)` (and/or the "Matrix layout saved." notification) before comparing.

### F-q2-qual-tests-a-28  [nit] [test-cheat]
- Where: inst/qualification/tests/test-OQ-APP-matrix.R:724-731
- What: Second instance of F-22's pattern: after two refused moves OQ-APP-056 asserts "the order of the row labels is unchanged" on the input object the helper could not have modified.
- Evidence: `down <- move(dta, ...); up <- move(dta, ...); qa_step("the order of the row labels is unchanged", c("GENE_ID", "GENE_NAME", "NOTE", "BIOTYPE"), names(datasets(dta, "gene_counts")@specs@columns))` — `dta` is never reassigned; the step re-reads the fixture's own YAML order. (The first refusal in the same case does it properly: `is.null(r$value)`.)
- Failure scenario: mutation: `dta_move_column()` reports `ok = FALSE` but returns a reordered document in `value` -> the step passes, because it never looks at `down$value` or `up$value`; under every mutation of the helper the step compares the fixture's YAML order with itself.
- Confidence: verified-by-reading
- Suggested direction: assert `is.null(down$value)` and `is.null(up$value)`.

### F-q2-qual-tests-a-29  [nit] [test-weak]
- Where: inst/qualification/tests/test-OQ-APP-matrix.R:669-700, 289-297; inst/qualification/tests/test-OQ-APP-server.R:383-418
- What: Three steps rest on a premise the case states but does not check.
- Evidence: (a) OQ-APP-055 says "the table was loaded lazily through the second entry" and forces it with `withr::local_options(DTAtools.stream_threshold = 1)`, but never asserts that the bound table is lazy; (b) OQ-APP-048 reads `dta_supported_types()`/`dta_handler_types()` and not what the dropdown is built from; (c) OQ-APP-016 looks for the literal source fragments `addEventListener('click', function(ev){` and `}, true);` anywhere in the script.
- Failure scenario: (a) the app starts passing `stream = "never"` to `load_file()` -> the reorder step silently tests an in-memory table, for which REQ-APP-046's "fail as changed" cannot arise; (b) a dropdown with hard-coded choices; (c) a guard whose click listener is registered without capture while another `}, true);` exists in the script. All pass. (b) and (c) are declared "pinned structurally" in the requirement notes, so this is recorded for completeness, not as a defect of intent.
- Confidence: verified-by-reading for the test side; inferred for (a) (the app's loading call was not traced beyond the absence of an explicit `stream =` in `inst/shiny/dta_app`)
- Suggested direction: (a) assert the table's class / `dta_is_lazy`-style property before the reorder.

---

## OQ-CONT — test-OQ-CONT-app.R, -documents.R, -findings.R, -model.R, -routing.R, -safety.R, -xpt.R, -zip.R

All eight files and `REQ-CONT.yaml` read completely; code opened: `R/containerFiles.R` l.98-170.
This is the best-built area of the slice. Expectations are literals from the fixture data
(`CONT_AE_CSV`, `CONT_DM_CSV`), byte offsets of the library, or hand arithmetic (53 + 30 = 83
bytes, 21 + 17 x 200 = 3421, SAS day 23968 = 2025-08-15, checked by hand here); refusals are
asserted by condition class plus "nothing is written"; parity cases are anchored by an absolute
count before two runs are compared (OQ-CONT-002). Mocks touch only sinks
(`showNotification`, `showModal`, `nav_select`). What remains:

### F-q2-qual-tests-a-30  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-CONT-safety.R:68-132, 406-448 (code: R/containerFiles.R:98-170; requirement: REQ-CONT.yaml:137-166)
- What: REQ-CONT-007 (risk high, security) enumerates the entry names that must refuse an archive or never be extracted; about half of the enumerated conditions have no fixture and no case.
- Evidence: exercised: `..` (three fixtures), a leading `/`, a drive letter, a symlink, a colon, a reserved device name, a trailing dot, a control character, 511+ bytes. Never exercised (grep over `test-OQ-CONT-*.R` finds no such name): an empty name, a name that is not valid text, a UNC / backslash-leading name, an empty path segment (`a//b`), a lone `.` segment, the characters `< > " | ? *`, a segment ending in a space, a segment longer than 255 bytes. Each has its own line in `dta_zip_entry_problem()` / `dta_zip_entry_escape()`.
- Failure scenario: mutations that pass every CONT case: delete `if (grepl("[<>\"|?*]", name))`; delete the 255-byte rule; change `"[. ]$"` to `"[.]$"`; change `segs %in% c(".", "..")` to `segs == ".."`; drop `validUTF8(name)`; change `"^[/\\]"` to `"^/"` (a `\server\share\x` entry then falls to the empty-segment rule, which is itself untested).
- Confidence: verified-by-reading
- Suggested direction: build the missing names at run time with `cont_swap()` / `cont_zip_add_empty()` (both already exist for exactly this) and loop over them like the `hostile` and `phrases` tables.

### F-q2-qual-tests-a-31  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-CONT-model.R:464-507 (requirement: REQ-CONT.yaml:613-626)
- What: OQ-CONT-047 makes only the outer DTA "old"; every object inside it is a current object, so "re-create every DTAtools object inside a DTA or a dataset saved by an older version" is not exercised, nor is upgrading a dataset on its own.
- Evidence: `old <- cont_doc(cont_csv_ds("ae"), cont_csv_ds("dm")); attr(old, "containers") <- NULL; attr(old, "S7_class") <- old_class` — the two datasets and their file handlers are built by the current constructors (they already have `provenance`, `container`). The steps assert `fixed@containers` and the two file names.
- Failure scenario: mutation: `upgrade_dta()` rebuilds the top-level object only and returns nested datasets untouched -> a document saved by 0.27.0 (whose datasets have no `provenance` property) still breaks at the first `@provenance` access; OQ-CONT-047 passes. `upgrade_dta(<old dataset>)` returning its input unchanged also passes.
- Confidence: verified-by-reading
- Suggested direction: strip a property from a nested dataset and from a file handler the same way, and add one step for a dataset passed on its own.

### F-q2-qual-tests-a-32  [minor] [doc-missing]
- Where: inst/qualification/requirements/REQ-CONT.yaml (21 `source:` lines, e.g. l.33, 57, 131, 212, 299); inst/qualification/tests/test-OQ-CONT-model.R:6-9, test-OQ-CONT-findings.R:4-5, test-OQ-CONT-documents.R:5-8, test-OQ-CONT-app.R:12-14; REQ-APP.yaml:505-506 and following
- What: The CONT requirements and test headers name `PLAN-container-manager.md` as the source of their expected values, and that document is not in the tree; the matrix plans REQ-APP cites exist only under `thoughts/`, which is not built into the package.
- Evidence: `grep -rl PLAN-container-manager` over the snapshot returns only `REQ-CONT.yaml` (21 hits), `deviations.yaml` and one developer test; `thoughts/shared/plans/` holds 18 plans, none of that name; `.Rbuildignore` has `^thoughts$`. Headers: "written down from the requirement text and from the container-manager plan (docs of the sections named in each requirement's source), never captured from a run".
- Failure scenario: a reviewer cannot apply the suite's own test ("which sentence of which requirement the expected value comes from", maintaining.md l.28-31) to the many exact strings no requirement sentence contains, e.g. "File entry 1 of dataset 'ae' refers to container 'sdtm', which this document does not define. (A dataset on its own ...)" (model.R:132-135), "Validation FAILED: 1 file not inside the required container" (findings.R:171), `ZP_KEPT` (zip.R:43-46). They are indistinguishable from text copied out of the software.
- Confidence: verified-by-reading (absence checked by name and by listing the plans directory)
- Suggested direction: ship the cited sections under `inst/qualification/docs/` or move the governing sentences into the requirement `text:`/`notes:`.

### F-q2-qual-tests-a-33  [nit] [test-cheat]
- Where: inst/qualification/tests/test-OQ-CONT-routing.R:229; test-OQ-CONT-xpt.R:351; test-OQ-CONT-model.R:448
- What: Three steps cannot fail for a reason unrelated to the code under test: one reads the input object after an aborted call, two search a message for a directory in one spelling only.
- Evidence: (a) `a <- cont_run(dta0, z, exdir = ex)` aborts, then `qa_step("no table is bound", 0L, length(tables(datasets(dta0, "dm"))))` — `dta0` is the argument, never reassigned (OQ-CONT-009 l.180 does it properly: `NULL, a$result`). (b) `grepl(dirname(p), msg, fixed = TRUE)` / `!grepl(dirname(zip), msg, fixed = TRUE)` test the forward-slash spelling only, while OQ-CONT-015 l.238-243 checks three spellings (`tempdir()`, `winslash = "/"`, `winslash = "\\"`) for the same claim.
- Failure scenario: (a) any mutation; (b) a message that prints `normalizePath(path)` on Windows (backslashes) discloses the server directory and both steps pass.
- Confidence: verified-by-reading
- Suggested direction: (a) assert `is.null(a$result)`; (b) reuse the three-spelling vector.

---

## OQ-EXPORT — test-OQ-EXPORT-agreement.R, -documents.R, -placeholders.R, -regions.R, -tables.R

All five files and `REQ-EXPORT.yaml` read completely; code opened: `R/DTADataSetTabular-class.R`
l.560-669 (the writer), `R/exportFunctions.R` (format cell), grep over `R/exportDocuments.R`
for the staging helper. The rule-wording cases (OQ-EXPORT-051, -055..-058, -073) and the
Markdown structure cases (OQ-EXPORT-060, -063, -064, -066) are exemplary: every sentence is a
literal from the notes of REQ-EXPORT-036, and pandoc / xml2 / `read.table` / jsonlite are
used as independent readers. What remains:

### F-q2-qual-tests-a-34  [major] [test-weak]
- Where: inst/qualification/tests/test-OQ-EXPORT-documents.R:1119-1153 (requirement: REQ-EXPORT.yaml:488-509)
- What: REQ-EXPORT-032 (risk high) covers three exported functions and three formats; its only case exercises one combination, `write_dta()` at the DOCX-to-PDF step.
- Evidence: `grep REQ-EXPORT-032` over the tests finds OQ-EXPORT-044 alone. It mocks `.pandoc_docx_to_pdf` to write "not a pdf" and calls `write_dta(et_minimal_dta(), file = out, format = "pdf", overwrite = TRUE, quiet = TRUE)`. Requirement: "write_dta(), write_dataset_metadata() and export_with_template() shall not leave a file ... a document export that fails, at any format (docx, md, pdf) or at DOCX-to-PDF conversion, shall leave a file already at the destination byte-for-byte unchanged and shall leave no staging file behind."
- Failure scenario: mutations that pass: `export_with_template()` zips the filled document straight onto `output` instead of a staging file; the Markdown writer calls `writeLines(lines, file)` on the destination; `write_dataset_metadata()` prints the Word document directly to `file`. A failure half-way then leaves a truncated agreement where a good one was (the exact defect DEV-013 recorded for tables), and no case notices.
- Confidence: verified-by-reading
- Suggested direction: loop the same "pre-existing bytes + injected failure" step over the three functions and the docx and md formats (a mocked writer that throws after the staging file is opened is enough).

### F-q2-qual-tests-a-35  [major] [test-weak]
- Where: inst/qualification/tests/test-OQ-FILE-writing.R:157-180; inst/qualification/tests/test-OQ-EXPORT-tables.R:166-178; inst/qualification/tests/test-OQ-EXPORT-documents.R:618-639
- What: "overwrite = TRUE shall replace it" — the positive half of REQ-FILE-028, REQ-EXPORT-023 and REQ-EXPORT-009 (all risk high) — is asserted nowhere in a way that could fail: the one case that tries writes the same table over itself and checks that the file exists.
- Evidence:
  ```r
  write_table_to_file(ds, table = "tab1", filename = p, ...)                       # first write
  res_ow <- write_table_to_file(ds, table = "tab1", filename = p, ..., overwrite = TRUE, quiet = TRUE)
  qa_check("overwrite = TRUE replaces the file without error", file.exists(p) && nrow(res_ow$tables) == 3L)
  ```
  `p` existed before the call and holds the same three rows either way. OQ-EXPORT-031, -012 and -013 test the refusal only (`inherits(cond, "condition")`), and every other `overwrite = TRUE` in the slice targets a fresh path or a write that is made to fail.
- Failure scenario: mutation: with `overwrite = TRUE` and an existing file the function returns after computing the checksum without moving the new file into place (e.g. `R.utils::gzip(..., skip = TRUE)` on the gzip branch, or an early return on the plain branch) -> the stale delivery stays on disk, its `.md5` sidecar agrees with it, every case passes. The same for `write_dta()` / `write_dataset_metadata()`.
- Confidence: verified-by-reading
- Suggested direction: overwrite a file holding different, known content and read the new content back (as OQ-EXPORT-072 already does for `export_with_template()`).

### F-q2-qual-tests-a-36  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-EXPORT-documents.R:253-296
- What: Three "is named in the document" steps of OQ-EXPORT-002/-003 (REQ-EXPORT-002, risk high) are substring searches that another value in the same document satisfies.
- Evidence: `for (expected in c("Clinical Data Specification", "Test Company 2", "Test Company")) qa_step(..., TRUE, grepl(expected, text, fixed = TRUE))` — the receiver's organisation "Test Company" is a prefix of the supplier's "Test Company 2". `grepl("clinical_data", text)` is met by the file pattern `clinical_data.*.csv$`, and of the fourteen column ids `CONSENT` is inside `CONSENT_DATE` and `AE_TERM` inside `SAE_TERM`.
- Failure scenario: mutation: the Word Parties chapter prints the supplier's affiliation under both headings (`receiver` built from the supplier) -> OQ-EXPORT-002 passes; no other case reads the receiver's organisation from the default Word layout (OQ-EXPORT-010 asserts it exactly, but for Markdown: `"Organization = Test Company" %in% pairs`). A Word column table that drops the `AE_TERM` row and repeats another still has 15 rows (OQ-EXPORT-004) and still "contains" every id.
- Confidence: verified-by-reading
- Suggested direction: compare cells, not text: the organisation cell under each party heading, and `tbl$col1[-1]` against the fourteen declared ids (as OQ-EXPORT-077 does for its own fixture).

### F-q2-qual-tests-a-37  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-EXPORT-tables.R:148-162; inst/qualification/tests/test-OQ-FILE-writing.R:80-106
- What: Neither gzip case looks at the bytes, so "shall write a gzip-compressed / valid gzip file" (REQ-EXPORT-022 risk high, REQ-FILE-026) is not independently verified.
- Evidence: OQ-EXPORT-030 reads back with `read.table(gzfile(out), ...)`; base R's `gzfile()` opens an uncompressed file for reading just as well ("it can also open for reading uncompressed files", ?connections). OQ-FILE-041 reads back through the package's own `read_file()`, parts of which also go through `gzfile(path, open = "rb")` (R/DTAFileTabular-class.R:570, 1169).
- Failure scenario: mutation: `compression = "gzip"` writes the plain table under the `.gz` name -> OQ-EXPORT-030 passes for certain; OQ-FILE-041 passes unless Arrow's extension-based decompression rejects the file on that path (not traced).
- Confidence: verified-by-reading for the two cases and for `gzfile()`'s documented behaviour; inferred for whether the package reader would fail on such a file
- Suggested direction: assert the two magic bytes `1f 8b` with `readBin(out, "raw", 2L)`.

### F-q2-qual-tests-a-38  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-EXPORT-regions.R:70-164; test-OQ-EXPORT-agreement.R:97-135; test-OQ-EXPORT-placeholders.R:1-90
- What: Three placeholder claims are traced but not checked: the three count placeholders of a region, the `{STUDY_TABLE}` block, and "the documented set and the substituted set are the same set".
- Evidence: (a) REQ-EXPORT-031 names `{DATASET_FILE_COUNT}`, `{DATASET_COLUMN_COUNT}`, `{DATASET_RULE_COUNT}`; they occur in the tests only inside the list of expected names (placeholders.R:55). (b) OQ-EXPORT-046's template holds `{STUDY_ID}`, `{PROTOCOL_ID}` and `{STUDY_TABLE}`; the only study assertions are `grepl("DEMO-001", text)` and `grepl("P-02", text)`, which the two inline tokens satisfy, and the absence of a literal `{STUDY_TABLE}` is not asserted. (c) the header of placeholders.R says the vocabulary "is only useful if the documented set and the substituted set are the same set", but the step compares `names(dta_template_placeholders())` with `names(dta_template_placeholders(dta))` — the function with itself; and OQ-EXPORT-041 takes its expected title and version from `metadata(dta)@title` / `@version`, i.e. from the package's own reader, although the literals ("Clinical Data Specification", "0.1") are used elsewhere in the suite.
- Failure scenario: mutations that pass: `{DATASET_COLUMN_COUNT}` resolves to the first dataset's count in every repetition; `{STUDY_TABLE}` renders nothing or stays as literal text; an inline placeholder (say `{DTA_DATE}`) is listed by `dta_template_placeholders()` but no longer substituted by `export_with_template()`.
- Confidence: verified-by-reading
- Suggested direction: one region case with two datasets of different file/column/rule counts; a `{STUDY_TABLE}`-only template; a template holding every inline name the function reports, asserting none survives.

### F-q2-qual-tests-a-39  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-EXPORT-documents.R:1716-1792
- What: OQ-EXPORT-059 ("... identical when written again, under another LC_TIME") silently repeats under the same locale when no second locale is installed, and records that as a passing step instead of a skip.
- Evidence:
  ```r
  other <- et_other_time_locale()        # NULL when none of six de/fr names can be set
  qa_check("the LC_TIME locale of the repeat is recorded", TRUE,
    detail = if (is.null(other)) "no second locale installed: the repeat is written under the current LC_TIME" else other)
  et_with_time_locale(other, writers[[what]](second))   # NULL leaves the locale as it is
  ```
  maintaining.md l.268-270: "Skip with a reason, through the helpers only. Every skip ... marks its requirements unverified".
- Failure scenario: mutation: the Markdown footer or a date cell is formatted with `%B` (a locale-dependent month name) -> on a host with only C/en_US locales both runs print the same English month, the files are byte-identical, the case passes and REQ-EXPORT-040's "also under a different LC_TIME" is reported verified. Likely the state of the Linux CI leg (inferred: the runner's locale list was not checked).
- Confidence: verified-by-reading for the fall-through; inferred for which hosts take it
- Suggested direction: skip (or fail) the locale part when `other` is NULL, as OQ-EXPORT-064 does for LC_CTYPE.

### F-q2-qual-tests-a-40  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-EXPORT-tables.R:290-308; inst/qualification/tests/test-OQ-FILE-writing.R:184-209
- What: Both "the written file reaches the same validation verdict" cases (REQ-EXPORT-026, REQ-FILE-029, the latter risk high) use a table with no violation, so the round trip only has to preserve "valid".
- Evidence: OQ-EXPORT-035 compares `validation_status(ds_checked)$ok` with `validation_status(ds2)$ok` (TRUE with TRUE, never anchored); OQ-FILE-045 compares the four counts and anchors `before$ok` to TRUE. Neither fixture holds a missing value in a required column, an over-long value, a non-number or a rule violation.
- Failure scenario: mutation: the writer drops every data row, or writes `NA` cells as the text "NA" that the reader then takes for a value -> a table with no rows, or with the gaps filled, still validates clean; both cases pass. A table whose verdict is `FALSE` with known counts would expose either.
- Confidence: verified-by-reading
- Suggested direction: round-trip a table with one violation per axis and compare the counts with literals.

### F-q2-qual-tests-a-41  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-EXPORT-documents.R:235-249, 393-424, 743-770, 796-810, 827-844, 912-941; inst/qualification/tests/test-OQ-FILE-execution.R:35-45, 61-79
- What: A group of steps assert less than the sentence they are traced to.
- Evidence (requirement text -> assertion):
  - REQ-EXPORT-001 / -012 "a classed cli condition naming the expected/required class" -> `inherits(cond, "rlang_error")`; the message is never read.
  - REQ-EXPORT-006 "embed the given text verbatim ... warn and embed nothing when yaml_text is NULL or empty" -> one word of the text (`marker-for-embed-test`), NULL only, any warning.
  - REQ-EXPORT-010 "an alias that forwards every argument unchanged" -> OQ-EXPORT-015 calls both functions with defaults only.
  - REQ-EXPORT-017 "warn naming the failure (unless quiet = TRUE) and fall back to the standard write_dta() layout" -> any warning, and `grepl("Data Transfer Agreement", text)`; `quiet = TRUE` is not tried.
  - REQ-FILE-030 "perform ... the lazy open" -> `!is.data.frame(opened)` (an in-memory Arrow Table passes); "raise a condition when called on the abstract tabular class" -> `inherits(..., "condition")`, any error.
- Failure scenario: mutations that pass: the bad-argument abort becomes `cli_abort("Invalid input.")`; `write_file_specification()` drops `...`; the fallback writes a one-line document containing the default document name; `open_file_execution()` reads the whole file into a Table.
- Confidence: verified-by-reading
- Suggested direction: match the package-authored message text, call the alias with a non-default argument, compare the fallback's chapter list with OQ-EXPORT-010's, and assert `inherits(opened, "Dataset")` as the CONT cases do.

### F-q2-qual-tests-a-42  [nit] [doc-wrong]
- Where: inst/qualification/tests/test-OQ-EXPORT-documents.R:1, 37-66, 1046-1050; inst/qualification/tests/test-OQ-APP-workflow.R:9-15; 15 call sites of `testthat::skip(`
- What: Stale comments, a duplicated helper, a dead helper, and a documented rule the files do not follow.
- Evidence: (a) documents.R l.37-48 says helper-parse.R's `qa_docx_tables()` "groups ... by `doc_index`" and that this "local, corrected copy is used in this file in its place"; helper-parse.R l.27-32 now groups by `table_index`, so `et_docx_tables()` is a line-for-line duplicate (and l.2821 of the same file calls `qa_docx_tables()` anyway). (b) l.1048-1050: "a column with a format -- which export_specs_table() writes without its "SAS " prefix"; the code writes `paste(structure@backend, structure@format)` (R/exportFunctions.R:70) and REQ-EXPORT-019's note says so. (c) l.1 lists "REQ-EXPORT-001 .. 019, 032 and 036 .. 042"; the file also holds the cases for 043-046. (d) `qw_bundled()` in workflow.R is defined and never called. (e) maintaining.md l.268: "Skip with a reason, through the helpers only"; `testthat::skip()` is called directly in test-IQ-ENV.R (6), test-OQ-APP-workflow.R (5), test-OQ-EXPORT-documents.R (3) and test-OQ-EXPORT-placeholders.R (1). (f) OQ-EXPORT-034's "no partial file is left" filters on the implementation's own suffix (`endsWith(f, ".part")`) instead of asserting the directory listing, as OQ-EXPORT-044 does.
- Failure scenario: none functional; (f) passes if the staging suffix is renamed and a staging file is left behind.
- Confidence: verified-by-reading
- Suggested direction: delete the duplicate and the dead helper, correct the three comments, route skips through a `qa_skip()` helper (or drop the sentence), assert `list.files(dir)` exactly.

---

## OQ-FILE — test-OQ-FILE-execution.R, -handlers.R, -reading.R, -sas7bdat.R, -writing.R

All five files and `REQ-FILE.yaml` read completely. The readers are the second strongest area
after CONT: fixtures are written as bytes by the case itself (`writeChar(..., useBytes = TRUE)`,
BOM and Latin-1 bytes by `writeBin`), expectations are literal data frames, the SAS cases use
`datasets::iris` and the bytes of the fixture as oracles, and the SAS epoch arithmetic was
re-derived by hand here and is right (23968, 14669, 2070887400, 1267444800, 315619200).
The overwrite, gzip, round-trip and "any condition" items of this area are filed above as
F-35, F-37, F-40 and F-41. What remains:

### F-q2-qual-tests-a-43  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-FILE-reading.R:84-113; inst/qualification/tests/test-OQ-FILE-sas7bdat.R:564-566
- What: OQ-FILE-021 (REQ-FILE-011, risk high: "readable end to end, reaching the identical validation verdict") compares two verdicts of a violation-free file with each other and anchors neither, so a compressed file that reads to nothing reaches "the same verdict".
- Evidence:
  ```r
  frame <- data.frame(ID = c("A0000001", "A0000002"), AGE = c("30", "40"), ...)   # clean under fr_specs()
  plain_verdict <- fr_verdict_fields(fr_verdict(p_plain))
  comp_verdict  <- fr_verdict_fields(fr_verdict(p_comp, filename = basename(p_comp)))
  qa_step(sprintf("a '.%s'-compressed file reaches the same verdict ...", ext), plain_verdict, comp_verdict)
  ```
  `fr_verdict_fields()` is `ok`, `columnspec`, `import` — no row count, no value. The neighbouring OQ-FILE-035 shows the right pattern ("gives the three modes something non-trivial to agree on: an import error, not just a clean pass", with a literal expected list). OQ-FILE-064 likewise calls the gzip SAS verdict "the same, clean, verdict" without comparing it with `fs_clean`.
- Failure scenario: mutation: the gzip path stops at the first block / returns a header-only table -> zero rows validate clean, `plain_verdict == comp_verdict`, the case passes. (That a gzip file's values are read is asserted elsewhere — OQ-FILE-041, OQ-CONT-003 — but not by the case that carries REQ-FILE-011.)
- Confidence: verified-by-reading
- Suggested direction: put one unparseable AGE in the frame and compare both verdicts with a literal `list(ok = FALSE, columnspec = 0L, import = 1L)`.

### F-q2-qual-tests-a-44  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-FILE-reading.R:203-225, 374-394; inst/qualification/tests/test-OQ-FILE-handlers.R:102-116, 329-358; inst/qualification/tests/test-OQ-EXPORT-regions.R:17-21, 102-111
- What: Requirement clauses of the FILE area with no step, and one expectation that is read from a package constructor although the file says it is a literal.
- Evidence (clause -> state):
  - REQ-FILE-033 "declared as UTF-16 or UTF-32": only `UTF-16LE` is written and declared.
  - REQ-FILE-017 (risk high) "fewer or more fields ... whether the file is read eagerly or only opened lazily": the too-many case (OQ-FILE-029) calls `read_file()` only.
  - REQ-FILE-031 "A handler declaring several names with pattern = FALSE is unaffected: those names are alternates": OQ-FILE-054 has the `pattern = TRUE` handler only.
  - REQ-FILE-003 "read as an ordinary data row": for `has_header = FALSE` only `names(t)` (`f0`, `f1`) and `nrow(t)` are asserted, not the first line's values.
  - regions.R l.17-21 says the fixture is built "so that every expected id below (\"ADSL\", \"ADAE\", their column ids) is this test's own literal"; the column ids are in fact `vapply(create_example_DTAColumnSpecCollection(1)@columns, function(s) s@id, ...)` (l.102-105), read from the package at run time.
- Failure scenario: mutations that pass: the wide-encoding refusal tests `grepl("UTF-16", enc)` only; `open_file()` pads a row with a surplus field; two literal alternates are reported as two missing targets; `has_header = FALSE` drops the first line but keeps two rows by repeating the last; `create_example_DTAColumnSpecCollection()` returning no columns makes both sides of the id comparison empty.
- Confidence: verified-by-reading
- Suggested direction: one step per clause; write the column ids of the region fixture out as literals.

---

## Cross-cutting

### F-q2-qual-tests-a-45  [minor] [design]
- Where: .github/scripts/qualification_verdict.R:17; R/qualification.R:851-857, 1362-1368; inst/qualification/tests/helper-app.R:38-45; every `qapp_skip_unless_app_installed()` / `qa_requires()` call in the slice
- What: Nothing in the run or in CI is sensitive to an unexpected skip, so a whole area can stop executing and stay green.
- Evidence: `if (startsWith(verdict, "FAIL")) stop(...)` is the only CI gate; the verdict for any number of skips is `PASS WITH NOT-EXECUTED TESTS`; M10 ("Every test case this run selected was executed") is `setdiff(expected_tcs, tests_df$tc_id)`, and a skipped case has a row in `tests_df`, so it counts as executed; M11 counts discovered titles, not executed cases. The verdict is already never a plain `PASS` in CI (IQ-ENV-019 and OQ-EXPORT-011 skip there, see F-01), so one more skip changes nothing visible.
- Failure scenario: `shiny`, `bslib` or `DT` fails to install on a CI leg (or `shinyjs` is added to `qapp_required_packages()` and is absent): 69 of the 70 cases of the six APP files (all but OQ-APP-001, which has no guard) and the 9 of test-OQ-CONT-app.R skip at their first line ("dependency absent: ..."), REQ-APP-002..060 turn `not_verified`, and the job is green. IQ-ENV-007 records the absence with a literal TRUE (F-04).
- Confidence: verified-by-reading
- Suggested direction: give CI an expected-skip list (or a maximum skip count per leg) and fail on anything outside it; keep the tolerant verdict for target systems.

### F-q2-qual-tests-a-46  [nit] [inconsistency]
- Where: inst/qualification/docs/maintaining.md:221-222; test titles across the slice
- What: The tag `white-box` is permitted by M13 but defined nowhere (not in maintaining.md, glossary.md, the Validation Plan or the report code) and is applied unevenly.
- Evidence: 21 of the 307 cases in this slice carry it. OQ-CORE-010/-011/-012 are tagged; OQ-CORE-003..-009 call the same kind of runner internals (`qual_expectations_df`, `qual_verdict`, `qual_tests_df`, `qual_meta_checks`, `qual_requirements_df`, `qual_find_snapshot_calls`, `qual_report_unit`) untagged. OQ-APP-056/-057 (calling `qapp_fn("dta_set_column")` without a server) are tagged; OQ-APP-026/-027, which do exactly that, are not. OQ-FILE-021 calls the internal `dta_compression_extensions()` untagged. `grep "white-box\|tags" R/qualification-report.R` finds nothing: the tag is a column of `results/tests.csv` only and is not printed in the report a reviewer signs.
- Failure scenario: a reviewer cannot use the tag to tell evidence gathered through the public interface from evidence that depends on internals.
- Confidence: verified-by-reading
- Suggested direction: define the tag in maintaining.md, print it in the report, and apply it by rule (any `:::`, `qual_*` or `qapp_fn()` call).

---

## Found on the final pass (EXPORT-agreement and CORE)

### F-q2-qual-tests-a-47  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-EXPORT-agreement.R:204-215
- What: The last two steps of OQ-EXPORT-050 are an `all()` and a `!any()` over a vector that is empty when the document has no header or footer part at all, so they pass exactly when the furniture is missing.
- Evidence:
  ```r
  write_dta(dta, file = bare, format = "docx")           # no template, no study
  qa_check("without a template the footer names the built-in layout",
    all(grepl("Built-in layout \u00b7 DTAtools", qagreement_furniture_texts(bare, "footer"), fixed = TRUE)))
  qa_check("without a study the header names none",
    !any(grepl("Study", qagreement_furniture_texts(bare, "header"), fixed = TRUE)))
  ```
  `qagreement_furniture_texts()` returns `character(0)` when the archive holds no `footerN.xml` / `headerN.xml`; `all(logical(0))` is TRUE. The guards `length(headers) > 0L` / `length(footers) > 0L` earlier in the case belong to the first document, not to `bare`. REQ-EXPORT-035 also asks for "the built-in layout and package version"; only the word "DTAtools" is searched, not the version.
- Failure scenario: mutation: the running footer (or all page furniture) is added only when `metadata@template` is set -> the bare document has no footer part, both steps pass, and "or the built-in layout and package version when the document records no template" is reported verified.
- Confidence: verified-by-reading
- Suggested direction: assert `length(...) > 0L` for the bare document too, and match `paste("DTAtools", utils::packageVersion("DTAtools"))`.

### F-q2-qual-tests-a-48  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-CORE-runner.R:53-73 (code: R/qualification.R:127-149, 1846)
- What: OQ-CORE-001 asserts the thirteen evidence files of REQ-CORE-001 (risk high) by `file.exists()` only; `run.log`, `environment.json` and `sessioninfo.txt` are read by no case in the suite.
- Evidence:
  ```r
  present <- file.exists(file.path(bundle$bundle_dir, expected))
  qa_step("every required bundle file is written", expected, expected[present])
  ```
  `run.log` exists as soon as `qual_log_open()` runs `file(path, open = "at")`, before anything is logged. A grep for `run.log`, `environment.json` and `sessioninfo` over `inst/qualification/tests` finds only this list; `SUMMARY.txt` and the report are opened only by OQ-CORE-010, for the "incomplete" marker. maintaining.md l.263-265: "No existence-only assertions. Reading a file back beats `file.exists()`".
- Failure scenario: mutation: `qual_log()` returns early for every event (the `isOpen(con)` guard inverted), or `qual_write_report()` writes a zero-length Markdown file -> the "contemporaneous run log" is empty and the "Validation Summary Report" blank; every file still exists, the manifest hashes the empty files correctly, OQ-CORE-001 passes.
- Confidence: verified-by-reading
- Suggested direction: read each file back: `run_start` and `run_end` lines in `run.log`, the verdict in `SUMMARY.txt` and `results.json`, the R version in `environment.json`.

---

## Coverage

**Slice files fully read (26 of 26, every line; 307 test cases):**
`inst/qualification/tests/` test-IQ-ENV.R (23 cases), test-OQ-APP-agreement.R (2),
test-OQ-APP-backends.R (13), test-OQ-APP-matrix.R (18), test-OQ-APP-server.R (29),
test-OQ-APP-variants.R (1), test-OQ-APP-workflow.R (7), test-OQ-CONT-app.R (9),
test-OQ-CONT-documents.R (3), test-OQ-CONT-findings.R (3), test-OQ-CONT-model.R (10),
test-OQ-CONT-routing.R (15), test-OQ-CONT-safety.R (7), test-OQ-CONT-xpt.R (11),
test-OQ-CONT-zip.R (5), test-OQ-CORE-runner.R (13), test-OQ-EXPORT-agreement.R (3),
test-OQ-EXPORT-documents.R (54), test-OQ-EXPORT-placeholders.R (2), test-OQ-EXPORT-regions.R (4),
test-OQ-EXPORT-tables.R (14), test-OQ-FILE-execution.R (3), test-OQ-FILE-handlers.R (19),
test-OQ-FILE-reading.R (23), test-OQ-FILE-sas7bdat.R (10), test-OQ-FILE-writing.R (6).

**Context fully read:** `docs/maintaining.md`, `docs/validation-plan.md`; all eight helper /
setup files (`helper-00-qa.R`, `helper-app.R`, `helper-container.R`, `helper-generators.R`,
`helper-matrix.R`, `helper-parity.R`, `helper-parse.R`, `setup-qualification.R`);
`requirements/_index.yaml`, `_api.yaml`, `REQ-ENV.yaml`, `REQ-CORE.yaml`, `REQ-APP.yaml`,
`REQ-CONT.yaml`, `REQ-EXPORT.yaml`, `REQ-FILE.yaml`; `R/run_dta_app.R`;
`.github/workflows/qualification.yml`, `.github/scripts/qualification_verdict.R`;
`inst/extdata/library/templates/biomarker_gf_acme.dta-template.yaml`.

**Read in part (only the lines a finding depends on):** `R/qualification.R` (l.1-1110,
1350-1575, 1700-2031); `R/containerFiles.R` (l.98-170); `R/DTADataSetTabular-class.R`
(l.560-669); `R/00_helpers.R` (l.300-312, 356-366, 464-472, 1020-1035);
`inst/shiny/dta_app/app.R` (l.1722-1760, 2800-3030, 3184-3310, 3780-4010, 5800-5855,
6095-6204, 6270-6390, 7690-7880); `inst/shiny/dta_app/R/utils_dta.R` (l.580-690, 1214-1396);
`inst/shiny/dta_app/R/template_core.R` (l.396-456); `inst/extdata/clinical_dta.yaml`
(l.1-230); `inst/extdata/library/templates/biomarker_gf.dta-template.yaml` (l.1-120);
`inst/qualification/deviations.yaml` (ids, statuses and the titles of LIM-010..022 and
LIM-033..035, which match the cases that pin them; DEV-002 and DEV-018, the two open
deviations, are bound outside this slice).

**Searched, not read:** `R/qualification-report.R`, `R/exportFunctions.R`,
`R/DTAFileTabular-class.R`, `R/exportDocuments.R`, `docs/glossary.md`,
`tests/testthat/` (only to confirm that a helper is or is not exercised there).

**Not read:** `inst/qualification/README.md`, `docs/references.md`, `risk-assessment.yaml`,
the other eleven `REQ-*.yaml` files and the 41 test files of the other qualification slice;
the binary fixtures under `inst/qualification/fx/` (their documented contents were taken from
the test headers, not inspected byte by byte) and the scripts that build them
(`.github/scripts/build_container_fixtures.R`, `build_sas7bdat_fixtures.R`).

**Not done, by instruction:** nothing was run. Whether the suite currently passes on this
tree, and the "always skips in CI" parts of F-01, F-39 and F-45, are therefore inferences from
the code and the workflow, marked as such in each finding.

**What was looked for and not found in this slice:** no `expect_snapshot*`; no mock of a
function under test (the seven `local_mocked_bindings()` sites replace sinks —
`showNotification`, `showModal`, `nav_select` — or inject a write/convert failure; the
`requireNamespace` substitute of OQ-APP-003 and the `session$send*` recorders of OQ-APP-040
stand in for the environment, not for the code under test); no
`qa_step(ok = ...)` override; no assertion on translated base-R text or locale-dependent month
names (the SAS display dates in OQ-FILE-060 would in fact expose a `%b` dependence under the
German dev locale: `31DEC1959`); no loop over a possibly empty vector other than the one in
F-47; the limitations pinned in the slice all exist in the register under the titles the cases
give.

## Tally

48 findings: 1 blocker, 11 major, 30 minor, 6 nit.
By category: test-weak 35, test-cheat 7 (F-01, F-03, F-13, F-15, F-22, F-28, F-33),
doc-wrong 2 (F-23, F-42), doc-missing 1 (F-32), inconsistency 2 (F-26, F-46), design 1 (F-45).
Blocker: F-17. Major: F-01, F-02, F-07, F-08, F-13, F-14, F-15, F-16, F-18, F-34, F-35.
Nit: F-06, F-28, F-29, F-33, F-42, F-46. Everything else is minor.

By area the picture is uneven. IQ-ENV has the weakest steps (literal `TRUE`, a skip in place
of a failure). The launcher, template-creation and vocabulary cases of APP are close to
vacuous, while the APP backends/matrix cases and all of CONT and FILE are careful, literal
and hard to fool. EXPORT sits between: excellent on rule wording and Markdown structure,
thin on overwrite, atomicity and the default Word layout.
