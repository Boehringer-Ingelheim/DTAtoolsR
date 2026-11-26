# Findings — q1-qual-infra (qualification suite infrastructure)

Reviewer key: `q1-qual-infra`. Pinned tree: `$SNAP` (68408c44). Read-only review; no R was run.

## Slice

- `inst/qualification/README.md`, `inst/qualification/docs/*`
- `inst/qualification/requirements/*` (17 `REQ-*.yaml`, `_api.yaml`, `_index.yaml`)
- `inst/qualification/deviations.yaml`, `risk-assessment.yaml`, `baseline/*`
- `inst/qualification/fx/` (fixtures) and their generators (`.github/scripts/build_container_fixtures.R`, `build_sas7bdat_fixtures.R`)
- `inst/qualification/tests/helper-*.R`, `setup-qualification.R`
- `.github/scripts/qualification_verdict.R`, `.github/scripts/sync_qualification_manifest.R`, `.github/workflows/qualification.yml`
- Opened to understand the above: `R/qualification.R`, `R/qualification-report.R` (runner, meta-checks, verdict), `NAMESPACE`, selected tests.

## Files fully read (updated as the review proceeds)

(see "Coverage" at the end)

---

## Area A — runner, verdict, traceability, meta-checks (`R/qualification.R`, `R/qualification-report.R`, CI verdict)

### F-q1-qual-infra-01  [major] [logic-error]
- Where: R/qualification.R:725-735 (`qual_traceability`); contradicted by inst/qualification/README.md:71-73, inst/qualification/docs/maintaining.md:268-270, R/qualification-report.R:157-159
- What: A requirement is reported `verified` as soon as ANY one test case citing it passed, even when the other test cases citing it were skipped or never executed, although the docs and the report say a skipped test marks its requirements not verified and that `verified*` means "every test case tracing to it passed".
- Evidence:
  ```r
  } else if (any(mine$status %in% c("fail", "error", "file_error"))) {
    "failed"
  } else if (any(mine$status == "pass")) {
    "verified"
  } else if (all(mine$status == "not_executed")) {
  ```
  README.md:71-73: "A test that cannot run is never silently dropped ... and the requirements it would have verified are marked not verified." maintaining.md:268-270: "Every skip ... marks its requirements unverified". Report text (qualification-report.R:157): "when every test case tracing to it passed". `any(pass)` wins over any number of `skip`/`not_executed` rows.
- Failure scenario (counted on the pinned tree: 11 requirements are cited both by a case that can skip and by one that cannot — REQ-CONT-032, REQ-EXPORT-007/-030/-040/-042/-046, REQ-PERF-008, REQ-SPEC-028, REQ-TMPL-014/-056, REQ-TYPE-010; e.g. REQ-TMPL-056 by four app-dependent cases (OQ-TMPL-124/-126/-128/-131) and four plain ones, REQ-EXPORT-040 by four pandoc-dependent cases (OQ-EXPORT-059/-060/-063/-064) and two plain ones): REQ-X is cited by OQ-A (a light check, passes) and OQ-B (the substantive check; skipped because `shiny`/pandoc/a browser/the scale tier/memory is absent). Section 7 of the signed report shows REQ-X `verified`; the reviewer following README step 4 ("check section 7 for requirements that are not verified") is never pointed at it. Mutation not detected by the suite's own evidence: break the behaviour only OQ-B checks on a machine where OQ-B skips -> requirement still `verified`.
- Confidence: verified-by-reading
- Suggested direction: derive `verified` only when no citing case is skip/not_executed (introduce `partially_verified`), or correct the three documents.

### F-q1-qual-infra-02  [major] [logic-error]
- Where: R/qualification.R:1358-1370 (`qual_verdict`), 664-670 (`qual_deviation_status`); inst/qualification/docs/validation-plan.md:219, 136-141
- What: Acceptance criterion 4 ("Every open deviation still reproduces") is not what the verdict computes: only `not_reproduced` fails the run; an open deviation whose binding emitted no expectation (`not_executed`) leaves the verdict at PASS / PASS WITH NOT-EXECUTED TESTS.
- Evidence:
  ```r
  stale <- sum(dev_status$status_run == "not_reproduced")
  verdict <- if (failed > 0 || meta_failed > 0 || stale > 0) {
    "FAIL"
  } else if (any(tests_df$status == "skip")) {
  ```
  `qual_deviation_status()` returns `"not_executed"` when no expectation message matches `^<id> (reproduced|no longer reproduces)`; that state is never read by `qual_verdict()`. M7 is static (it parses for a `qa_known_deviation("DEV-nnn"` call) so it passes whether or not the call ran.
- Failure scenario: the test holding the binding of DEV-002 or DEV-018 is skipped, or a later edit puts the `qa_known_deviation()` call behind an `if`/after an early `return()`: M7 passes (the call exists), the deviation is `not_executed`, and an unfiltered three-stage run still reports `PASS` (or `PASS WITH NOT-EXECUTED TESTS`) although nothing established that the registered defect still describes the software. The requirement the deviation affects is shown `verified_with_deviation` on the strength of a defect nobody observed.
- Confidence: verified-by-reading (both current bindings, test-OQ-VAL-check.R:405 and test-OQ-TYPE-rbackend.R:449, are unconditional today — see Area D; the hole is in the verdict logic)
- Suggested direction: in an unfiltered full run treat an `open` deviation with `status_run != "reproduced"` as a failing meta-check (or FAIL), and state the rule in the plan.

### F-q1-qual-infra-03  [major] [test-cheat]
- Where: inst/qualification/tests/test-IQ-ENV.R:246-261 (IQ-ENV-016), R/qualification.R:1358-1370, .github/scripts/qualification_verdict.R:17; contradicts inst/qualification/docs/validation-plan.md:213 and maintaining.md:150-151
- What: The installed-file integrity check skips — it does not fail — exactly when its evidence base is broken (manifest missing, header unreadable, or R < 4.5), and a skipped IQ case does not fail the run, although acceptance criterion 1 says "Every installation-qualification case passes".
- Evidence:
  ```r
  if (!file.exists(manifest_path)) {
    testthat::skip("no baseline/inst-files.sha256 manifest shipped with this installation")
  }
  ...
  if (identical(algo, "sha256") && !qual_has_sha256()) {
    testthat::skip("the baseline manifest is sha256 and this R (< 4.5) has no tools::sha256sum")
  ```
  `qual_verdict()` maps any skip to `PASS WITH NOT-EXECUTED TESTS` regardless of stage; the CI gate only stops on `startsWith(verdict, "FAIL")`.
- Failure scenario: (a) an installation that lost or had removed `qualification/baseline/inst-files.sha256` (the one file the manifest cannot cover) reports no integrity failure; (b) on R 4.1–4.4 — permitted by `DESCRIPTION` `Depends: R (>= 4.1.0)` — IQ-ENV-016 can never run because the shipped manifest is sha256, so with a plain source install (IQ-ENV-015 skips too, test-IQ-ENV.R:235) no installed file is integrity-checked at all, and the run is still a PASS-class verdict. README's "What it needs" table (README.md:60-69) does not list R >= 4.5 as the precondition of this check.
- Confidence: verified-by-reading
- Suggested direction: make a missing/unparseable manifest a failure, ship an md5 column (or require R >= 4.5 for qualification), and make IQ skips that are not explicitly optional fail criterion 1.

### F-q1-qual-infra-04  [minor] [doc-wrong]
- Where: inst/qualification/docs/maintaining.md:218-219 and :305; R/qualification.R:791-793
- What: The documented half of meta-check M2 ("the file name must carry the same stage and area") does not exist; M2 checks id uniqueness only.
- Evidence:
  ```r
  ids <- disc_df$tc_id[disc_df$ok]
  dup <- unique(ids[duplicated(ids)])
  add("M2", "Test case ids are unique", length(dup) == 0, paste(dup, collapse = ", "))
  ```
  maintaining.md:305: "M2 | Duplicate test-case id, or id disagrees with the file name | Renumber, or move the test to the right file". No code compares `disc_df$file` with `stage`/`area` (the only use of `disc_df$file` is the M10 filter context, :848). The agreement is asserted only by the developer suite (tests/testthat/test-qualification.R:90-95), which is not installed and does not run on a target system, so it is not a meta-check of a qualification run.
- Failure scenario: `test_that("IQ-ENV-030 | ... | REQ-ENV-001", ...)` added to `test-OQ-VAL-check.R` passes M1 and M2; it runs in the OQ stage and is recorded with stage `OQ` in `tests.csv` (stage comes from the file, :408/:436) but `IQ` in discovery (from the title), so per-stage counts and the report's stage sections disagree with the ids, and a `stages = "IQ"` run reports it under M10 as selected-but-not-executed. (Mechanically checked: none of the 812 current titles disagrees with its file.)
- Confidence: verified-by-reading
- Suggested direction: implement the comparison in M2 or delete the claim from both places in maintaining.md.

### F-q1-qual-infra-05  [minor] [logic-error]
- Where: R/qualification.R:1358-1370, 1753-1755 (`stages = character()`), 1892-1901; .github/scripts/qualification_verdict.R:17
- What: A run that executed no test case at all gets a PASS-class verdict (`PASS (PARTIAL)`), printed by `print.dta_qualification()` as a green success, and the CI gate accepts every verdict that does not start with `FAIL`.
- Evidence:
  ```r
  failed <- sum(tests_df$status %in% c("fail", "error", "file_error"))
  ...
  } else if (any(tests_df$status == "skip")) {
    "PASS WITH NOT-EXECUTED TESTS"
  } else {
    "PASS"
  ```
  With `stages = character()` `tests_df` has zero rows; M10's expected set is empty, M4/M11 are static, so nothing fails. The empty-run guard (:1892) only fires when a `filter` was given.
- Failure scenario: `run_qualification(out, stages = character())` writes `Verdict : PASS (PARTIAL)` into SUMMARY.txt and the report header with "0 executed"; `qualification_verdict.R` would exit 0 on it. "PASS" is asserted about zero evidence; `(PARTIAL)` is the only hint.
- Confidence: verified-by-reading
- Suggested direction: give a zero-test run its own verdict (e.g. `NOT EXECUTED`) and make the CI script accept only an explicit allow-list of verdicts.

### F-q1-qual-infra-06  [minor] [test-weak]
- Where: .github/scripts/qualification_verdict.R:9-17; .github/workflows/qualification.yml:122-125
- What: The CI gate passes on `PASS WITH NOT-EXECUTED TESTS` with no bound on how many or which tests were skipped, and reads only the first `SUMMARY.txt` it finds.
- Evidence:
  ```r
  text <- readLines(summaries[[1]])
  verdict <- sub("^Verdict[[:space:]]*:[[:space:]]*", "", grep("^Verdict", text, value = TRUE)[[1]])
  if (startsWith(verdict, "FAIL")) stop("The qualification run failed.")
  ```
- Failure scenario: a change makes a skip condition permanently true (a typo in a package name passed to `qa_requires()`, a renamed bundled example behind `qa_example_path()`, Chrome missing from a runner image): the affected cases turn from pass to skip on every platform, the job stays green, and — because of F-01 — most of their requirements stay `verified`. Nothing in CI compares the skip set with an expected one.
- Confidence: verified-by-reading
- Suggested direction: fail CI when the set of skipped test ids on a runner differs from a committed allow-list (per OS/tier).

### F-q1-qual-infra-07  [minor] [test-weak]
- Where: R/qualification.R:978-995 (`qual_find_snapshot_calls`, M8); claims at inst/qualification/README.md:177-179, docs/validation-plan.md:130-134, docs/maintaining.md:27-28
- What: M8 detects only call tokens whose name starts with `expect_snapshot`; it is presented as the enforcement of "no expected value comes from the software" but cannot see any other way of doing that.
- Evidence:
  ```r
  calls <- pd[pd$token == "SYMBOL_FUNCTION_CALL" &
    grepl("^expect_snapshot", pd$text), , drop = FALSE]
  ```
  Not matched: `expect_known_output()/expect_known_value()/expect_known_hash()`, `expect_equal_to_reference()`, `verify_output()`, `do.call("expect_snapshot", ...)`, `f <- testthat::expect_snapshot; f(x)`, a `readRDS()` of a stored result, or a literal pasted from observed output. (Grep of the suite: none of the named testthat functions is used today.)
- Failure scenario: a test added as `expect_known_value(check(ds), "ref.rds")` or `qa_step("...", readRDS(ref), observed)` stores the software's own output as the expected value and M8 stays green while validation-plan.md:132-134 keeps asserting that "a meta-consistency check fails the run if a snapshot expectation appears anywhere".
- Confidence: verified-by-reading
- Suggested direction: extend the token list to every reference-style expectation and word the docs as "no testthat snapshot calls", not as enforcement of the rule itself.

## Area B — helpers, generators, oracle (`inst/qualification/tests/helper-*.R`, `setup-qualification.R`)

### F-q1-qual-infra-08  [major] [bug]
- Where: inst/qualification/tests/helper-generators.R:183 (with the spec at :20-23, the oracle at :111-136, tiers at helper-00-qa.R:55-62); consumer inst/qualification/tests/test-PQ-PERF-scale.R:48-78; inst/qualification/README.md:24-25, :92; docs/glossary.md:87-90
- What: The scale generator writes a subject id that violates its own specification on row 10,000,000, so at the `full` tier — the documented default of `run_qualification()` — the "closed-form" oracle is wrong by two column-specification errors.
- Evidence:
  ```r
  SUBJID = DTAColumnSpec(id = "SUBJID", type = "SAS Char", length = 8, nullable = FALSE,
    pattern = "^S[0-9]{7}$")                       # helper-generators.R:20-23
  SUBJID = sprintf("S%07d", index),                # :183, index runs to n_rows
  full = c(1e4L, 1e5L, 1e6L, 1e7L),                # helper-00-qa.R:59
  ```
  `sprintf("S%07d", 1e7)` is `"S10000000"`: 9 characters and 8 digits, so the last, un-injected row breaks both `length = 8` and the pattern. `qa_defect_plan()` places its last slot at `n_rows - 1`, so that row is not in the plan and `qa_oracle()` still says 9 column-specification errors.
- Failure scenario: `run_qualification("out")` (scale `full`, README.md:24) on a machine with >= 12 GB free (or without `ps`, where `qa_skip_unless_memory_gb()` returns TRUE): PQ-PERF-020 "at 10000000 rows, the three axis counts are the injected ones" expects `columnspec = 9` and a correct engine reports 11 -> the qualification of record is `FAIL` for a fixture defect. With < 12 GB the whole case turns into a skip instead, so the defect stays hidden. CI never reaches it: qualification.yml runs `quick`/`standard` only (`full` is a manual-dispatch option). The glossary's claim that "the same defects are injected at the same relative positions" at every tier is false for the largest one.
- Confidence: verified-by-reading (not executed; the arithmetic of `sprintf` and of `qa_defect_plan()` was traced by hand)
- Suggested direction: widen the id (`%08d`/length 9) or cap the index, and run the `full` tier once in CI so it cannot rot.

### F-q1-qual-infra-09  [minor] [test-cheat]
- Where: inst/qualification/tests/test-PQ-PERF-scale.R:72-75 with inst/qualification/tests/helper-generators.R:140-149
- What: The "closed-form" expected count table takes its `target` column from the software's own output, breaking the suite's one rule for that column.
- Evidence:
  ```r
  table = status$table[[1]],                       # ps_measure(), from validation_status()
  ...
  qa_step(sprintf("at %d rows, each constraint is broken exactly as often as planned", rows),
    qa_oracle_counts(run$table), run$counts)
  ```
  `qa_oracle_counts(target, ...)` writes `target = target` into every expected row; the caller passes the table name the engine reported, and `run$counts$target` comes from the same engine's `messages()`.
- Failure scenario: mutation — the engine labels every message and the status row with a wrong or empty table name (consistently): expected and observed still agree, the step passes. maintaining.md:19 ("An expected value never comes from the software") is violated in the PQ evidence for REQ-PERF-006.
- Confidence: verified-by-reading
- Suggested direction: pass the literal the specification implies (the file name the test wrote) instead of `run$table`.

### F-q1-qual-infra-10  [minor] [dead-code]
- Where: inst/qualification/tests/helper-generators.R:176-179 (`qa_frame`), :252-263; callers inst/qualification/tests/test-OQ-STREAM-arguments.R:177, :361
- What: `qa_frame()` takes a `seed` and an `n_total` that have no effect on the data, while its `set.seed()` call silently resets the session's random stream.
- Evidence:
  ```r
  qa_frame <- function(n_rows, seed = 1L, n_filler = 7L,
                       plan = qa_defect_plan(n_rows), offset = 0L,
                       n_total = n_rows) {
    set.seed(seed + offset)
    index <- seq_len(n_rows) + offset
  ```
  Nothing after `set.seed()` draws a random number (the frame is built with `rep()`/`sprintf()` only) and `n_total` is never read. `qa_frame(300L, seed = 7L)` and `qa_frame(300L, seed = 11L)` are byte-identical.
- Failure scenario: a reader of test-OQ-STREAM-arguments.R believes two different random deliveries are exercised (seeds 7 and 11); they are the same file. Side effect: every call re-seeds the global RNG to `1 + offset`, so the per-run stream that setup-qualification.R:19 establishes "so that a randomised case which fails can be reproduced" is discarded at each call, and the `runif()`/`sample.int()` draws tests use for unique file names repeat after it.
- Confidence: verified-by-reading
- Suggested direction: drop both parameters and the `set.seed()` call.

### F-q1-qual-infra-11  [minor] [doc-wrong]
- Where: R/qualification.R:1777-1778 (`@param seed`), inst/qualification/tests/setup-qualification.R:17-19; inst/qualification/tests/helper-parity.R:146-147, :281-283, :385, :449-451
- What: The run's `seed` does not seed "the randomised differential cases": the parity campaign always uses the fixed seeds `1..n` and `1001..1000+n`, and the seed is not printed in the report it is said to make a failure reproducible from.
- Evidence:
  ```r
  #' @param seed Integer. Seed for the randomised differential cases, so a run is reproducible.
  qp_case <- function(seed) { set.seed(seed) ...          # helper-parity.R:146-147
  lapply(seq_len(n), function(seed) { case <- qp_case(seed)   # :282-283
  set.seed(1000L + seed)                                   # :385
  ```
  `qa_seed()` is read in exactly two places: setup-qualification.R:19 and test-OQ-TYPE-rbackend.R:242. setup-qualification.R:17-18 says a failing randomised case "can be reproduced from the report alone", but `qual_report_md()` (R/qualification-report.R:72-94) prints no seed; it exists only in `results/results.json`/`run.json`.
- Failure scenario: an operator re-runs with `seed = 1L` expecting a different draw of the differential campaign, or expects `seed` from the report to reproduce a parity failure: the campaign is identical for every `seed`, and the report does not carry the value.
- Confidence: verified-by-reading
- Suggested direction: either derive the campaign seeds from `qa_seed()` and print the seed in the report, or document the campaign as fixed.

### F-q1-qual-infra-12  [minor] [test-cheat]
- Where: inst/qualification/tests/helper-parity.R:100-121 (`qp_legacy_collection`, `qp_collection`), :131, :163-167
- What: The parity campaign's declared "core axis" is exercised only by bypassing S7 validation to build a specification the package itself refuses, justified by a backwards-compatibility scenario the project has disowned.
- Evidence:
  ```r
  # ... it can still reach the engines as a legacy object
  # restored from a saved session, which is deserialised without validation. So
  # the rules are attached with the check bypassed
  x <- DTAColumnSpecCollection(columns = columns)
  S7::prop(x, "rules", check = FALSE) <- rules
  ```
  REQ-SPEC-027 (cited in the same comment) makes a rule naming an undeclared column a constructor error; `xkey_unique` over the undeclared `XKEY` is "in every generated case" by the file header (:22-23) and is attached this way whenever it is drawn.
- Failure scenario: the evidence for the engine-parity requirements (OQ-STREAM parity cases) is taken on objects no supported API call can produce. A divergence between the engines that exists only for validated objects is diluted, and a divergence that exists only for the bypassed object fails the run for a state users cannot reach. With "no backward compatibility until 1.0.0" the legacy-session path is dead weight.
- Confidence: verified-by-reading (I did not read how often `xkey_unique` is drawn per tier; `sample(2:4, sample(0:2, 1))` makes it roughly half of the cases)
- Suggested direction: declare `XKEY` as an untyped/any column or drop the rule; keep one explicitly named case if the legacy path must stay.

### F-q1-qual-infra-13  [minor] [doc-wrong]
- Where: inst/qualification/README.md:66; inst/qualification/tests/helper-00-qa.R:204-211; R/qualification.R:81-83, 1121-1130; docs/maintaining.md:221-222; requirements/REQ-ENV.yaml:259-263
- What: The suite documents, tags, probes for and records a browser for "browser tests of the application", but contains none: `qa_skip_unless_browser()` is never called, no title carries the `browser` (or `scale-full`) tag, and no test uses `shinytest2`/`chromote`.
- Evidence:
  ```
  | `shinytest2`, `chromote` + Chrome | browser tests of the application | reported as not executed |   (README.md:66)
  qa_skip_unless_browser <- function() {            (helper-00-qa.R:204, 0 callers)
  ```
  Tags actually used across the 812 titles: `white-box` 42, `subprocess` 3, `slow` 2, `slow,scale-standard` 2. `grep -i "shinytest2|chromote|AppDriver"` over `tests/test-*.R` hits only IQ-ENV-023, which merely records whether Chrome exists.
- Failure scenario: a reviewer reads README.md:66 and section 3 of the report ("Browser for app tests: <path>") as evidence that the application was driven in a browser on this system; nothing was. With Chrome absent nothing is "reported as not executed" either, because there is nothing to skip.
- Confidence: verified-by-reading
- Suggested direction: delete the row, the helper, the two unused tags and the browser probe, or add the browser cases they describe.

### F-q1-qual-infra-14  [nit] [dead-code]
- Where: inst/qualification/tests/helper-parse.R:125-132 (`qa_example_path`), helper-matrix.R:129-130 (`mx_table`), helper-00-qa.R:229-237 (`qa_artifact_dir`)
- What: Three helpers have no caller; `qa_artifact_dir()` being unused means the bundle's `artifacts/` directory, documented as holding "files individual test cases chose to keep", is always empty.
- Evidence: counting `name(` over `tests/test-*.R`, `helper-*.R` and `setup-qualification.R` gives 0 for `qa_example_path`, `mx_table` and `qa_skip_unless_browser`, and for `qa_artifact_dir` only the mention in a comment (setup-qualification.R:6). README.md:107 and maintaining.md:255 still describe artifacts as a live mechanism.
- Failure scenario: none at run time; maintainers are told to write artefacts through a path nothing exercises, and test files re-implement `qa_example_path()` inline (e.g. test-OQ-SPEC-yaml.R:12, test-PQ-PERF-workflows.R:17) instead of sharing it.
- Confidence: verified-by-reading
- Suggested direction: remove the dead helpers or route the duplicated inline copies through them.

### F-q1-qual-infra-15  [minor] [doc-wrong]
- Where: inst/qualification/docs/maintaining.md:253-270 ("Rules that are enforced"); R/qualification.R:774-903
- What: Of the five "rules that are enforced" only the snapshot ban has an enforcing check; in particular "Skip with a reason, through the helpers only" is contradicted by 23 direct `testthat::skip()` calls in the test files.
- Evidence: maintaining.md:268: "**Skip with a reason, through the helpers only.**" The meta-checks M1-M14 contain nothing about skips, writes outside `qa_tempdir()`, existence-only assertions or translated text. Direct calls: test-IQ-ENV.R:236, 248, 257, 260, 283, 303; test-OQ-APP-workflow.R:12, 71, 75, 198, 206; test-OQ-EXPORT-documents.R:602, 1883, 2247; test-OQ-EXPORT-placeholders.R:64; test-OQ-SPEC-accessors.R:12; test-OQ-SPEC-lint.R:296; test-OQ-SPEC-yaml.R:12; test-OQ-TMPL-library.R:169; test-PQ-PERF-repeatability.R:127, 183, 240; test-PQ-PERF-workflows.R:17.
- Failure scenario: a maintainer trusts the heading and assumes an ad-hoc `skip()` or an existence-only assertion would be rejected by the run; it is not. Several of the direct skips fire exactly when the thing under test is broken (a bundled example or the app missing from the installation; the application not starting within 60 s, test-PQ-PERF-repeatability.R:239-241; a C-locale subprocess that errors, :126-128), turning a failure into `PASS WITH NOT-EXECUTED TESTS`.
- Confidence: verified-by-reading (call sites listed by grep; the three contexts named in the scenario were read)
- Suggested direction: rename the section to "Rules" or add a parse-based meta-check that only the `qa_*` helpers may call `skip()`, and make "missing shipped file"/"did not start" conditions failures.

## Area C — evidence that the runner and its meta-checks work (REQ-CORE vs `test-OQ-CORE-runner.R`, cross-read)

### F-q1-qual-infra-16  [major] [test-weak]
- Where: R/qualification.R:774-903 (`qual_meta_checks`); only caller in any test: inst/qualification/tests/test-OQ-CORE-runner.R:242-253; claim at inst/qualification/docs/validation-plan.md:199-202 and :143-146
- What: Thirteen of the fourteen meta-checks have no test, in either suite, showing they can fail; the plan nevertheless excludes the runner from coverage measurement on the grounds that "its correct operation is established by the installation qualification and the OQ-CORE cases".
- Evidence:
  ```r
  meta <- qual_meta_checks(req_df = qualification_requirements()[0, ], ... stages_run = "OQ")
  qa_step("the unreached case fails the completeness check", FALSE, meta$ok[meta$check_id == "M10"])
  ```
  This is the only call of `qual_meta_checks()` outside the runner (grep over `inst/qualification/tests` and `tests/testthat`: no other `qual_meta_checks`/`check_id`). OQ-CORE-008 tests the helper behind M8, not the M8 row. tests/testthat/test-qualification.R re-derives M1/M2/M3/M6/M7/M9 on the shipped files with its own code (:61-114) but never asserts on `qual_meta_checks()`, and has no counterpart at all for M4 (every requirement referenced), M5 (every export covered), M11, M12, M13, M14. validation-plan.md:145: "A test suite that passes proves nothing until someone has shown it is capable of failing."
- Failure scenario: mutation `uncovered <- character(0)` (M4), `missing_cover <- character(0)` (M5) or `add("M7", ..., TRUE, "")` in `qual_meta_checks()`: every qualification case and every developer test still passes, the report keeps printing "M4 ... TRUE", and a requirement with no test / an export with no requirement / an unbound open deviation is reported as checked. These are the checks README.md:177-182 and validation-plan.md:215-218 name as acceptance criterion 3.
- Confidence: verified-by-reading
- Suggested direction: one OQ-CORE case per meta-check that feeds `qual_meta_checks()` a synthetic violating input and expects that row `FALSE` (and a clean input `TRUE`).

### F-q1-qual-infra-17  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-CORE-runner.R:91-97 (evidence for REQ-CORE-001); inst/qualification/docs/maintaining.md:421-424
- What: The qualification case for the hash manifest recomputes exactly one hash — the first entry — which is the very mistake maintaining.md lists as a trap that "actually caught us".
- Evidence:
  ```r
  first <- listed[[1]]
  qa_step(sprintf("the recorded hash of %s matches the file", first),
    qa_hash_file(file.path(bundle$bundle_dir, first)), sub("  .*$", "", readLines(manifest)[[1]]))
  ```
  maintaining.md:421-424: "A guard that checks one item out of many checks nothing. The bundle verification originally recomputed a single hash ... and passed while the log's own entry was wrong." The developer suite was fixed (tests/testthat/test-qualification.R:143-153, "Every file, not a sample"); the qualification case, which is the evidence a target-system report cites for REQ-CORE-001 ("a hash manifest covering every other file"), was not.
- Failure scenario: mutation — take the manifest before the last `qual_log()` lines are written (swap R/qualification.R:2014-2016 with :2018): `run.log`'s recorded hash is wrong, `sha256sum -c` fails for the auditor, and OQ-CORE-001 passes because `listed[[1]]` is never `run.log`.
- Confidence: verified-by-reading
- Suggested direction: recompute every listed hash in OQ-CORE-001, as the developer test already does.

### F-q1-qual-infra-18  [minor] [test-weak]
- Where: inst/qualification/tests/test-OQ-CORE-runner.R:165-217 (OQ-CORE-004), :256-311 (OQ-CORE-006), :564-613 (OQ-CORE-011); requirements/REQ-CORE.yaml:43-48, :64-67, :127-135
- What: Three high/medium-risk CORE requirements are cited by cases that exercise only part of their text: the "errors" half of the verdict rule, the reporting of a deviation's run status, and the word "every" in REQ-CORE-011.
- Evidence: OQ-CORE-004 calls `qual_verdict()` with statuses `pass`, `skip`, `fail` only — never `error`, `file_error` or a stage abort, although REQ-CORE-004 says "FAIL whenever any executed test case fails or errors". OQ-CORE-006 asserts the two messages `qa_known_deviation()` emits but never calls `qual_deviation_status()`, the function that turns them into `reproduced`/`not_reproduced` for the verdict and section 8.1. OQ-CORE-011 gives each requirement exactly one test case, so "when every test case tracing to it passed" (REQ-CORE-011) is indistinguishable from "any" — and the code implements "any" (F-01).
- Failure scenario: mutations not detected by any qualification case: `failed <- sum(tests_df$status == "fail")` in `qual_verdict()` (an erroring or file-level-broken case no longer fails the run); a typo in the marker regex at R/qualification.R:662 (every deviation becomes `not_executed`, which by F-02 fails nothing).
- Confidence: verified-by-reading
- Suggested direction: add `error`/`file_error` rows to OQ-CORE-004, drive `qual_deviation_status()` in OQ-CORE-006, and give OQ-CORE-011 a requirement with one passed and one skipped case.

## Area D — deviation register, limitations, installation checks (`deviations.yaml`, `REQ-ENV.yaml`, `baseline/`, `sync_qualification_manifest.R`; `test-IQ-ENV.R` cross-read)

### F-q1-qual-infra-19  [major] [logic-error]
- Where: R/qualification.R:690-697 and :1924 (`qual_traceability` reads only the deviations); inst/qualification/deviations.yaml:551-562 (LIM-003); inst/qualification/requirements/REQ-VAL.yaml:317-325 (REQ-VAL-026); inst/qualification/tests/test-OQ-VAL-accessors.R:171-213
- What: A limitation's `affects:` list has no effect on the traceability matrix, so a requirement whose text the software does not meet is reported `verified` whenever the gap was filed as a `LIM` instead of a `DEV` — as REQ-VAL-026 is, by a case that pins the opposite of its text "as observed".
- Evidence:
  ```r
  live <- if (is.null(dev_df)) list() else dev_df[dev_df$status != "closed", , drop = FALSE]   # only DEV entries
  trace <- qual_traceability(req_df, disc_df, tests_df, stages, devs$deviations)               # :1924, no limitations
  ```
  REQ-VAL-026: "validation_errors() shall return the full validation detail, carrying its result version and the attributes recording how many rows were scanned, whether the scan was partial and whether it was structural only." LIM-003: "the whole-column engine sets no such attribute. partial_scan and structural_only are likewise absent". OQ-VAL-034 (:209-213): `# ... Pinned as observed; see LIM-003.` / `qa_step("the whole-column engine records no scanned-row count", TRUE, is.null(eager_rows))`.
  Second instance: REQ-PERF-011 (requirements/REQ-PERF.yaml:131-139, "Every measured run shall record ... the row count and the throughput derived from them") against LIM-009/LIM-004 (deviations.yaml:539-549, :574-586: "Both rows and rows_per_sec are therefore empty in the metrics of every measurement this suite takes"); PQ-PERF-023 (tests/test-PQ-PERF-scale.R:176-189) expects `is.na(rows)` and `is.na(rows_per_sec)` and then asserts a "finite, positive throughput" that the suite's own helper computed (`qa_perf()`, helper-00-qa.R:268-272), i.e. it checks the test harness, not the software.
- Failure scenario: section 7 of every report lists REQ-VAL-026 as `verified` with an empty Deviations column although the in-memory engine does not carry the attributes the requirement demands and the "verifying" test asserts their absence. By glossary.md:51-55 ("A defect: the software does something other than what its requirement says") this is a deviation, which would make the row `verified_with_deviation`. The expected value `is.null(...)` is taken from the software's behaviour, not from the requirement (maintaining.md:19). Mutation not detected: none needed — the requirement is already unmet and reported met.
- Confidence: verified-by-reading
- Suggested direction: either reword REQ-VAL-026 to the engine-specific contract, or register it as a DEV; and show limitations against the requirements they affect in the matrix.

### F-q1-qual-infra-20  [minor] [test-weak]
- Where: inst/qualification/tests/test-IQ-ENV.R:45-63 (IQ-ENV-002), :132-136 (IQ-ENV-007), :405-416 (IQ-ENV-022), :418-440 (IQ-ENV-023); requirements/REQ-ENV.yaml:25-34, :247-267; inst/qualification/README.md:94-96
- What: Three IQ cases consist only of `qa_check(<text>, TRUE, detail = ...)` and so cannot fail; they "verify" requirements of the form "shall be recorded" even when nothing could be recorded, and README says the stage records free memory, which no case does.
- Evidence:
  ```r
  qa_check("free disk space at the write location is recorded", TRUE,
    detail = if (is.na(free_bytes)) "could not be determined on this platform" else sprintf(...))
  ```
  Same shape at :48-62 (Built, Packaged, source revision) and :420-439 (pandoc, browser). README.md:94: "The installation stage records the free disk and memory it found" — `grep -i memory test-IQ-ENV.R` has no hit; memory is only probed inside `qa_skip_unless_memory_gb()` and never recorded.
- Failure scenario: on a host where `wmic` is absent (the tool IQ-ENV-022 shells out to on Windows, test-IQ-ENV.R:20; it is no longer installed by default on current Windows 11 — inferred, not executed) the step message reads "could not be determined", the case passes and REQ-ENV-022 ("shall be recorded as installation evidence") is `verified`. Mutation not detected: delete the `sha`/`built`/`packaged` fields from `qual_environment()` (R/qualification.R:1139-1142) — IQ-ENV-002 never looks at the environment record the report prints.
- Confidence: verified-by-reading (the availability of `wmic` on a given Windows build is inferred)
- Suggested direction: assert on the recorded value (non-NA where the platform can supply it, and present in `environment.json`), mark what is unavailable as a skip with a reason, and drop "and memory" from README or record it.

### F-q1-qual-infra-21  [minor] [test-weak]
- Where: inst/qualification/tests/test-IQ-ENV.R:308-379 (IQ-ENV-020); inst/qualification/docs/maintaining.md:176-180; inst/qualification/requirements/REQ-ENV.yaml:207-234
- What: IQ-ENV-020 checks a hand-written list of 17 options and has no completeness step, so the claim that it "pins the default of every option read via getOption("DTAtools....")" is false: `DTAtools.temp_dir` (R/00_helpers.R:1027) is read by the package and absent from the list; ten of the seventeen `file:line` sources the requirement cites are stale.
- Evidence:
  ```r
  d <- getOption("DTAtools.temp_dir")              # R/00_helpers.R:1027
  for (name in names(defaults)) { ... sites <- site_defaults(name)   # test-IQ-ENV.R:366-368, only the listed names
  ```
  `grep -o '"DTAtools\.[A-Za-z0-9_.]+"' R/*.R` yields 18 option names besides the run config; the test lists 17. The walker also only recognises calls with an explicit default (`length(e) >= 3`, :341) so it could not see this site. REQ-ENV-020 `source:` cites e.g. `R/00_helpers.R:159` for `DTAtools.stream_threshold` (actual :419/:468), `R/DTA-class.R:782/783/791` (actual :879/:890/:918), `R/importConversion.R:870/884` (actual :976/:990), `R/streamingValidation.R:895` (actual :811/:931).
- Failure scenario: a new option `getOption("DTAtools.foo", 5)` is added and its default later changed to 50: IQ-ENV-020 passes unchanged because nothing compares the set of option names found in the namespace with `names(defaults)`; maintaining.md:178-180 relies on the maintainer remembering.
- Confidence: verified-by-reading
- Suggested direction: derive the set of option names from the same namespace walk and fail on any name not in the pinned list; replace line numbers in `source:` with function names.

### F-q1-qual-infra-22  [minor] [security]
- Where: inst/qualification/tests/test-IQ-ENV.R:262-277 (IQ-ENV-016); .github/scripts/sync_qualification_manifest.R:30-34; requirements/REQ-ENV.yaml:174-184
- What: The installed-file integrity check is one-directional: it hashes the files the manifest lists and never asks whether the installed tree holds files the manifest does not list (and the manifest itself omits dot-files).
- Evidence:
  ```r
  resolved <- vapply(rel_path, function(p) system.file(p, package = "DTAtools"), character(1))
  ...
  mismatched <- rel_path[present][expected_hash[present] != actual_hash[present]]
  ```
  `list.files("inst", recursive = TRUE, all.files = FALSE)` (sync script :30) builds the list; nothing enumerates the installed directories.
- Failure scenario: a file added after release under the installed `shiny/dta_app/R/` (Shiny sources every `.R` file there at start-up; the suite's own `qapp_helper_files()` does the same, helper-app.R:48-50) or an extra template under `extdata/` changes what the application executes or offers, and IQ-ENV-015/016 both pass: "a file quietly altered after release would first become visible" (test-IQ-ENV.R:8-9) holds for altered bytes only.
- Confidence: verified-by-reading
- Suggested direction: also compare `list.files()` of each manifest-covered installed directory with the manifest and report extras.

### F-q1-qual-infra-23  [minor] [doc-wrong]
- Where: inst/qualification/requirements/REQ-TMPL.yaml:24, :78, :308, :326 (and 30 more lines), REQ-ENV.yaml:128, :217-229, :277, REQ-TYPE.yaml:77-82; inst/qualification/deviations.yaml:479, :515, :567, :577
- What: Most `file:line` references in `source:`/`reference:`/`notes:` fields no longer point at the code or test they name.
- Evidence (referenced line -> what is there in the pinned tree): `R/validateTemplate.R:846-941 (.dta_template_check_bindings)` -> the function is defined at :1253; `R/validateTemplate.R:273-286 (.dta_template_version_plain_is_exact)` -> defined at :299; `R/arrowCompute.R:191 set_dta_compute_threads()` -> :198; `R/exportDocuments.R:579 dta_pdf_backend()` -> :1016; REQ-TYPE-005 notes "a documented gap already pinned at tests/testthat/test-DTAColumnSpec.R:98" -> line 98 is the `SAS Num`/`"double"` expectation, nothing about Date/Time; LIM-009 `R/DTA-class.R:952` -> a roxygen line about `last_validation_ok`; LIM-006 `R/DTAFileTabular-class.R:1066` -> a blank line; LIM-001 `tests/testthat/test-validation-oracle.R:296` -> `expect_equal(rule_row$row, NA_integer_)`; LIM-002 `tests/testthat/helper-validation-corpus.R:338` -> `),`.
- Failure scenario: a reviewer tracing a requirement or a limitation to its stated source lands on unrelated code; IQ-ENV-020's own comment sends the reader to "REQ-ENV-020's source field for file:line" (test-IQ-ENV.R:309-311), ten of whose seventeen entries are wrong (F-21).
- Confidence: verified-by-reading (every reference in REQ-ENV, REQ-FILE, REQ-RULE, REQ-TYPE, the register and a sample of nine in REQ-TMPL was printed against the file; the remaining REQ-TMPL line ranges were not individually judged)
- Suggested direction: cite function names (stable) instead of line numbers, or add a check that each cited line still contains the cited symbol.

### F-q1-qual-infra-24  [minor] [inconsistency]
- Where: inst/qualification/deviations.yaml:10-20, :27, :394-398, :429-433; inst/qualification/docs/maintaining.md:113-140; R/qualification.R:592-628, :822-827, R/qualification-report.R:171-173; inst/qualification/tests/helper-00-qa.R:123-126
- What: The register's rules are stated inconsistently and are not enforced by the loader: `pending` has three different definitions, "exactly one binding" and "the full schema" are checked nowhere, an unknown `status` silently escapes M7, the `resolution:` field is dropped, and DEV-001 lists a requirement it has nothing to do with.
- Evidence: deviations.yaml:17-19 "`pending` means it is registered but the area work package that binds it has not landed yet"; maintaining.md:123-125 "for a defect you have documented but this suite does not itself reproduce, usually because it was found elsewhere"; helper-00-qa.R:123-125 accepts `pending` so "an area's tests and the promotion ... land in the same change". Loader: `status = d$status %||% "open"` with no validation, then M7 `open_devs <- dev_df$id[dev_df$status == "open"]`. `qual_deviations_df()` copies `id,title,status,severity,affects,observed,expected,impact,workaround,reference,opened,closed` only, so `resolution:` of DEV-016/DEV-017 never reaches `deviations.csv`/`results.json`, while DEV-016's exported `impact:` still reads "this half is not [fixed]". DEV-001 (BEST12. inferred as integer) has `affects: [REQ-CORE-006, REQ-TYPE-001]`; REQ-CORE-006 is the runner's deviation-reporting requirement. `qual_discover_deviation_refs()` returns `unique(ids)`, so two bindings of one id pass although deviations.yaml:10-11 demands "exactly one".
- Failure scenario: an entry written `status: Open` (or `status: fixed`, or any other misspelling) needs no binding for M7, still marks its requirements `verified_with_deviation` (`status != "closed"`), and its run status is `not_executed` for ever — which by F-02 fails nothing. The developer suite does reject unknown statuses (tests/testthat/test-qualification.R:109) but that check is not part of a qualification run.
- Confidence: verified-by-reading
- Suggested direction: validate the register's schema and status vocabulary in `qual_deviations_df()` (abort like the requirement loader does), count bindings per id in M7, carry `resolution`, and settle one definition of `pending`.

### F-q1-qual-infra-25  [nit] [inconsistency]
- Where: .github/scripts/sync_qualification_manifest.R:83-87
- What: The script that exists to guarantee LF output passes a raw line break inside a string literal as the separator and registers an `on.exit()` at top level, where it does nothing.
- Evidence:
  ```r
  con <- file(manifest_path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeLines(lines, con, sep = "
  ")
  close(con)
  ```
  The separator is whatever line ending the script file itself was checked out with (LF only because `.gitattributes` forces it); `on.exit()` outside a function is ignored by `Rscript`, so the explicit `close(con)` is the only one. The same raw-newline literal appears three times in inst/qualification/tests/test-OQ-CORE-runner.R:406-407, :434-435, :444-445.
- Failure scenario: a checkout or an editor that rewrites the script with CRLF makes the written manifest CRLF — the exact failure the comment above the call says the binary connection prevents (a working copy that differs from the index and a `mixed-line-ending` hook rejection). `--check` cannot notice, because `readLines()` strips either line ending.
- Confidence: verified-by-reading
- Suggested direction: write `sep = "\n"` and drop the `on.exit()`.

## Area E — requirements versus tests (`requirements/REQ-*.yaml`, `_api.yaml`, `_index.yaml`; test files cross-read)

Mechanical facts established for this area (bash/awk over the pinned tree, no R): 581 requirements, 812 test cases; `_api.yaml` equals the 128 `export()` lines of `NAMESPACE`; every requirement id is cited by a title and every cited id exists (M3/M4 hold); `covers` and `_api.yaml` are the same set (M5/M6 hold); every `min_tests` equals today's count; no title disagrees with its file; all `affects:` ids exist; the baseline manifest lists exactly the 363 files under `inst/` and all 363 sha256 values match.

### F-q1-qual-infra-26  [major] [test-weak]
- Where: R/qualification.R:809-820 (M5/M6), :558-561 (`covers` must be non-empty); inst/qualification/README.md:10-16; requirements/REQ-APP.yaml (48 of 60 requirements `covers: [run_dta_app]` only); requirements/REQ-SPEC.yaml:40-53; inst/qualification/tests/test-OQ-SPEC-yaml.R:103-123; tests/test-OQ-APP-server.R:46-55; tests/test-PQ-PERF-repeatability.R:175-242
- What: "Covers every exported function" is established by listing: M5 only checks that each export appears in some `covers:` list, nothing checks that a test citing that requirement ever calls the symbol, and several exports are in fact never exercised the way their requirement says.
- Evidence:
  ```r
  covered_symbols <- unique(unlist(req_df$covers))
  missing_cover <- setdiff(api, covered_symbols)
  add("M5", "Every exported symbol is covered by at least one requirement", ...)
  ```
  Textual cross-check of each `(requirement, covers symbol)` pair against the bodies of the cases citing that requirement: in 468 of 1012 pairs the symbol is not called in any citing case (315 not in that file's own helpers either; shared helpers account for most `check`/`messages`/`load_file` pairs). Concrete cases read in full: (a) `specs_from_list` is called nowhere in the suite; its only requirement, REQ-SPEC-004 (four list constructors "shall build the same objects ... as the YAML readers"), is cited by OQ-SPEC-004 alone, which calls `dta_from_list()` only and compares dataset names, column names and rule ids with `read_dta_from_yaml()` — which, by the requirement's own note, "parse[s] and then delegate[s] to these", so both sides run the same code. (b) `run_dta_app` is the sole `covers` entry of 48 APP requirements (72 pairs in all) whose cases drive `shiny::testServer()`; `run_dta_app()` itself is called once in the suite, with `bslib`/`DT` forced unavailable (OQ-APP-003), so its success path never runs — PQ-PERF-037, the "launch check" for REQ-PERF-016 (`covers: [run_dta_app]`), starts the app with `shiny::runApp(app, port = ...)` in its own script. (c) REQ-TYPE-012..015 `covers: [apply_rules, check]`; their cases call the internal `dta_as_numeric_strict()` only.
- Failure scenario: mutations no qualification case detects — `specs_from_list()` drops every `values:` list (both sides of OQ-SPEC-004 drop it identically); `run_dta_app()` passes the wrong directory or ignores `port`/`...` to `shiny::runApp()`; M5 stays `TRUE` and README.md:10 keeps saying every exported function is covered.
- Confidence: verified-by-reading for (a)-(c) and for the M5 code; the 468/315 counts are a textual heuristic (a symbol reached only through internal delegation is counted as not called)
- Suggested direction: make `covers` checkable (e.g. require that at least one citing case contains a call of each covered symbol, with an explicit `indirect:` escape that names the path), and give `run_dta_app()`'s success path and `specs_from_list()` a case of their own.

### F-q1-qual-infra-27  [major] [doc-wrong]
- Where: inst/qualification/requirements/REQ-STREAM.yaml:284-291 (REQ-STREAM-021, risk high); inst/qualification/tests/test-OQ-STREAM-parity.R:183-211; inst/qualification/tests/helper-parity.R:17-24
- What: The requirement states the opposite of the behaviour its own test asserts: it says text such as "1.5" and "1.50" in an undeclared key column is treated "as the SAME key on both" engines, while the case expects three distinct keys and no duplicate.
- Evidence:
  ```yaml
  uniqueness rule DOES read shall be keyed identically by both engines,
  in particular treating text differing only in numeric formatting (such
  as "1.5" and "1.50") as the SAME key on both, because neither engine has
  been told the column is a number.
  ```
  ```r
  qa_step("three textually distinct values are keyed as three distinct keys, so the table is valid on both",
    list(ok = TRUE, n_rule_errors = 0L), ...)
  ```
  The test comment (:184-187) says "both engines are required to key it as text, and the answer -- no duplicate"; the rationale clause of the requirement ("because neither engine has been told the column is a number") also only supports the distinct-keys reading.
- Failure scenario: the requirement text is printed in section 13 of every report and is the specification a reviewer signs; read literally, a delivery with ids `1.5` and `1.50` in an undeclared key column should fail a uniqueness rule, and it passes. The expected value `n_rule_errors = 0` cannot be traced to "a sentence of the requirement" (maintaining.md:28-31) — it contradicts the sentence.
- Confidence: verified-by-reading
- Suggested direction: reword to "as two distinct text keys on both engines".

## Cross-check requested by the coordinator — c4-tmpl-export F-01, F-02, F-03

All three are CONFIRMED by an independent reading of `R/qualification.R`, `R/qualification-report.R`, the three docs and `.github/scripts/qualification_verdict.R`.

- **c4 F-01 (traceability `verified` on any pass) — confirmed; same defect as F-q1-qual-infra-01.** The logic is in `qual_traceability()` (R/qualification.R:725-735), the false sentence is in the report (R/qualification-report.R:157-159) and in README.md:71-73, maintaining.md:268-270, validation-plan.md:220-221 and REQ-CORE-011 (requirements/REQ-CORE.yaml:129-131, "when every test case tracing to it passed"). Counts on the pinned tree (mine, by awk over titles): 581 requirements (c4 wrote 577), 158 cited by more than one case (c4: 155), and 11 that today mix a skip-capable case with one that cannot skip (list in F-01; c4's examples REQ-EXPORT-040 and REQ-EXPORT-030 are among them). What the safeguards say: no meta-check looks at it — M4 is static (a title mentions the id), M10 counts a skipped case as executed (`setdiff(expected_tcs, tests_df$tc_id)`, :851-852), M1-M14 contain nothing per requirement; `qual_verdict()` does turn any skip into `PASS WITH NOT-EXECUTED TESTS` (:1364-1365) and section 9 lists the skipped case with its requirement ids, so the run-level flag exists, but SUMMARY.txt's "not verified" count (:1598) and section 7 stay wrong; `qualification_verdict.R:17` passes that verdict. No test pins the any-vs-every semantics (OQ-CORE-011 and tests/testthat/test-qualification.R:40-59 give each requirement exactly one case — F-18). Severity: I rated it major because the run-level verdict and section 9 still expose the skip; blocker is defensible since the per-requirement matrix is the thing README step 4 tells the reviewer to sign from.
- **c4 F-03 (zero tests -> `PASS (PARTIAL)`) — confirmed; same defect as F-q1-qual-infra-05.** With `stages = character()` every meta-check passes vacuously or statically: M10's expected set is empty, M4/M5/M6/M11/M13 read only the files, M7 is static, M14 returns TRUE ("no floor was given"/"the performance stage was not part of this run", :910-923), and the deviation statuses are all `not_executed`, which `qual_verdict()` ignores (F-02). `qualification_verdict.R` exits 0 on it. The suite itself depends on this mode: OQ-CORE-001, -002 and -010 and tests/testthat/test-qualification.R:116-159 all call `run_qualification(stages = character(0))`, and none asserts the verdict (OQ-CORE-010 only that it starts with FAIL after an injected write failure), so nothing pins the zero-test verdict either way.
- **c4 F-02 (report section 12 names no manifest and prints `sha256sum -c ` with no file) — confirmed; not previously in this file, recorded below as F-28.**

### F-q1-qual-infra-28  [major] [bug]
- Where: R/qualification.R:1991-1997 with R/qualification-report.R:214-225; inst/qualification/README.md:132-138; tests/testthat/test-qualification.R:425-446
- What: (Confirms c4-tmpl-export F-02.) The report is rendered before `x$run$hash_manifest` is assigned, so section 12 of every real report names no manifest and prints a verification command without its file argument, contradicting the README and SUMMARY.txt.
- Evidence:
  ```r
  report_paths <- qual_write_report(x, bundle_dir, formats)          # :1991
  x$run$hash_manifest <- if (identical(qual_hash_algorithm(), "sha256")) {   # :1993, after the report exists
  sprintf("Verify with `%ssum -c %s` from the bundle directory.", x$run$hash_algorithm, x$run$hash_manifest %||% "")
  ```
  `qual_report_inventory()` falls back to the literal "the hash manifest" and to `""`; SUMMARY.txt is written at :1998, after the assignment, so it is correct. Neither suite reads section 12 of a report produced by `run_qualification()`: the developer tests call `qual_report_inventory()` with a hand-built `run` list (one with, one without `hash_manifest`), and no OQ-CORE case mentions the section.
- Failure scenario: any run -> `report/qualification-report.md` (and its HTML/Word/PDF renderings, the document that is signed) reads "`the hash manifest` lists every file in this evidence bundle ..." and "Verify with `sha256sum -c ` from the bundle directory." Mutation not detected by any test: deleting :1993-1997 or moving them above :1991.
- Confidence: verified-by-reading
- Suggested direction: set `hash_manifest` when `x$run` is first built (it depends only on `qual_hash_algorithm()`), and assert section 12 of a real run's report in OQ-CORE-001.

## Area E (continued) — requirement texts, their sources, and the register's blind spots

### F-q1-qual-infra-29  [major] [test-cheat]
- Where: inst/qualification/requirements/REQ-VAL.yaml:37-50 (REQ-VAL-003), REQ-RULE.yaml:199-210 (REQ-RULE-019), REQ-FILE.yaml:76-92 (REQ-FILE-006), :175-188 (REQ-FILE-014), REQ-TYPE.yaml:177-185 (REQ-TYPE-012), :207-219 (REQ-TYPE-015); inst/qualification/tests/test-OQ-VAL-columnspec.R:73-93, tests/test-OQ-TYPE-numeric.R:11-16; claims at docs/validation-plan.md:130-134, docs/maintaining.md:19-31
- What: A number of requirements were written from the software's observed behaviour (and say so), so the expected values derived from them come from the software after all; three quarters of all `source:` fields name nothing but implementation files.
- Evidence:
  ```yaml
  Pinned: the absence is reported once per ROW rather than once per table ... Recorded as observed behaviour, not endorsed as a good one.   # REQ-VAL-003 notes
  Pinned as observed. It follows base R's duplicated() ...                                                                          # REQ-RULE-019 notes
  Pinned from dta_matches_filename_base(): the non-pattern branch tests candidate %in% declared_names ...                           # REQ-FILE-006 notes
  ```
  REQ-TYPE-012 states no values at all ("each into its documented value, missing flag and unconvertible flag"); its case says "The 13-case matrix is copied verbatim from ... tests/testthat/helper-validation-corpus.R:304-347". OQ-VAL-001: "the behaviour on record rather than the behaviour one would design" with expected `n` errors for `n` rows. Classifying the 581 `source:` fields: 442 cite only `R/...` or `inst/shiny/...` files (223 of them `risk: high`), 25 cite code plus a document, 26 a document only. The per-row "required" behaviour and the NaN/duplicate-NA readings that the notes call "not endorsed" are neither DEV nor LIM entries (only REQ-FILE-014 and REQ-TYPE-015 have one), so section 8 of a report never shows them.
- Failure scenario: a behaviour that was already wrong when the requirement was transcribed cannot be detected — exactly the failure maintaining.md:23-25 says the one rule exists to prevent. Concretely: a missing declared column costs one error per row (10 million errors at the full tier); a specification-first requirement would have failed the suite, whereas the requirement was bent to the code and the report shows REQ-VAL-003 `verified` with no limitation attached. validation-plan.md:132-133 ("Every expected value in this suite is written from the specification") is not true for these.
- Confidence: verified-by-reading for the six requirements and two cases quoted; the 442/25/26 split is a regex classification of `source:` text
- Suggested direction: register every "pinned, not endorsed" behaviour as a LIM/DEV, give REQ-TYPE-012 its table in the requirement text, and state in the plan that the requirements are a reverse-engineered functional specification where that is the case.

### F-q1-qual-infra-30  [minor] [doc-wrong]
- Where: inst/qualification/requirements/REQ-CONT.yaml:33, :57, :131, :212, :299, :348, :371, :430, :453, :474, :489, :508, :523, :552, :578, :599, :611, :626, :656, :679, :707; inst/qualification/deviations.yaml:775; REQ-MATRIX.yaml (59 lines), REQ-APP.yaml (17), REQ-TMPL.yaml (8); REQ-PERF.yaml:109, :124, :150, :172, :184; REQ-ROBUST.yaml:83, :95, :104, :157
- What: Many requirements name as their source a document that does not exist in the tree, is not shipped with the package, or does not contain the statement.
- Evidence: `plan PLAN-container-manager.md ...` is cited 22 times; `thoughts/shared/plans/` holds no such file (it holds PLAN-matrix-dataset.md, PLAN-matrix-format-review.md, PLAN-matrix-shiny.md and 15 others). The 98 references to the three matrix plans point into `thoughts/`, which `.Rbuildignore` removes (`^thoughts$`), so an installed copy — the thing being qualified — and the evidence bundle carry none of the cited sources. REQ-PERF-009/-010/-012/-014/-015 and REQ-ROBUST-007/-008/-009/-014 give `source: inst/qualification/docs/validation-plan.md`; that file contains none of "twenty times", "three times", "locale", "random", "working directory" or "temporary".
- Failure scenario: a reviewer asked to judge REQ-CONT-020 or REQ-PERF-009 against its stated source cannot find the source (container plan), cannot open it on the target system (matrix plans), or finds nothing about the statement (validation plan); the claim that each expected value can be traced to a specification sentence rests on documents nobody can read.
- Confidence: verified-by-reading (file listing of `thoughts/`, `.Rbuildignore`, grep of the plan)
- Suggested direction: ship the design documents the requirements cite under `inst/qualification/docs/`, or cite only shipped material, and fix the validation-plan citations.

### F-q1-qual-infra-31  [minor] [doc-wrong]
- Where: inst/qualification/README.md:148, docs/maintaining.md:272-294; e.g. requirements/REQ-TMPL.yaml (REQ-TMPL-056), REQ-EXPORT.yaml:84-122 (REQ-EXPORT-007), :711-747 (REQ-EXPORT-040), REQ-CONT.yaml:137-160 (REQ-CONT-007), REQ-FILE.yaml:259-267 (REQ-FILE-021), REQ-ROBUST.yaml:117-124 (REQ-ROBUST-011), REQ-SPEC.yaml:193-200 (REQ-SPEC-018)
- What: "Each requirement is one testable statement" and "write `text:` so it can be falsified" do not describe the requirement set: most requirements bundle several obligations, some dozens, and a few are not falsifiable as written.
- Evidence: counting "shall" per `text:` — 227 of 581 have one, 199 have three or more, 73 have five or more (up to 12, REQ-CONT-030); 55 texts exceed 150 words (REQ-TMPL-056: 576). 423 requirements are cited by exactly one case, among them 29 of the 73 with five or more obligations. Unfalsifiable wording: REQ-FILE-021 "an unusually large number of declared columns or an unusually large single field shall be read correctly and complete in a small, bounded time"; REQ-ROBUST-011 "a very large single value, or very many columns"; REQ-SPEC-018 "shall each return the corresponding part"; REQ-STREAM-018 "over enough generated cases".
- Failure scenario: section 7 reports such a requirement `verified` when its single case passed, but nothing ties the case's steps to the individual obligations; a clause no step checks (e.g. the `specs_from_list()` clause of REQ-SPEC-004, F-26) is invisible in the matrix. maintaining.md:29-31 asks the author to name "which sentence of which requirement" a value comes from; with twelve obligations per sentence that is not reviewable.
- Confidence: verified-by-reading (counts by awk over the YAML texts)
- Suggested direction: split the long requirements, or record per step which clause it verifies, and replace the vague texts with numbers.

### F-q1-qual-infra-32  [minor] [dead-code]
- Where: inst/qualification/requirements/REQ-CONT.yaml:613-626 (REQ-CONT-029), REQ-MATRIX.yaml:734-747 (REQ-MATRIX-059), REQ-META.yaml:264-276 (REQ-META-021), :278-288 (REQ-META-022), REQ-EXPORT.yaml:304-312 (REQ-EXPORT-019 notes), :511-526 (REQ-EXPORT-033), :528-545 (REQ-EXPORT-034), REQ-STREAM.yaml:296-305 (REQ-STREAM-021 notes)
- What: The suite specifies and pins backward-compatibility behaviour that the project no longer wants before 1.0.0.
- Evidence: "upgrade_dta() shall re-create every DTAtools object inside a DTA or a dataset saved by an older version" (REQ-CONT-029); "A matrix dataset saved by a version in which ... (the settings of such an object carry corner_mode) shall not be read on" (REQ-MATRIX-059, i.e. code kept to recognise the old format); "Previously supported transmission fields and unknown extensions shall remain supported" (REQ-META-021); "Legacy constructor arguments shall retain their positional meanings" (REQ-META-022); "for a document written by an older version, whose Format cell carried no prefix, takes the backend from the same row's Type cell" (REQ-EXPORT-019 notes); "unknown legacy extension fields, shall be retained" (REQ-EXPORT-033/-034); "a legacy, unvalidated collection (one restored from a saved session)" (REQ-STREAM-021, see F-12).
- Failure scenario: removing any of these compat paths, as the owner's policy invites, fails the qualification run; conversely the requirement set keeps an exported function (`upgrade_dta`) and several legacy branches alive as "verified" obligations.
- Confidence: verified-by-reading
- Suggested direction: drop or rewrite these requirements together with the compat code they pin.

### F-q1-qual-infra-33  [minor] [inconsistency]
- Where: inst/qualification/requirements/REQ-ENV.yaml:140-150 vs REQ-SPEC.yaml:242-263; REQ-ENV.yaml:119-128 vs REQ-STREAM.yaml:205-214; REQ-FILE.yaml:313-358 vs REQ-EXPORT.yaml:337-401 vs REQ-ROBUST.yaml:48-72; REQ-SPEC.yaml:2 vs requirements/_index.yaml:21-22; REQ-EXPORT.yaml:762-765, REQ-FILE.yaml:501-503
- What: The same statement is specified two or three times in different areas, one area carries two titles, and limitations are recorded in requirement notes instead of the register.
- Evidence: REQ-ENV-013 and REQ-SPEC-023 both say every `create_example_*()` constructor builds a valid object of the class it names; REQ-ENV-011 and REQ-STREAM-016 both specify `set_dta_compute_threads()` report/change/restore; `write_table_to_file()` gzip equality is REQ-FILE-026 and REQ-EXPORT-022, the overwrite refusal REQ-FILE-028, REQ-EXPORT-023 and REQ-ROBUST-004, the md5 sidecar REQ-FILE-027, REQ-EXPORT-024 and REQ-ROBUST-006, the round-trip verdict REQ-FILE-029 and REQ-EXPORT-026, the incomplete write REQ-EXPORT-025 and REQ-ROBUST-005. `REQ-SPEC.yaml` is titled "Specifications, their construction and their accessors" while `_index.yaml` and `risk-assessment.yaml` call the area "Specifications, YAML and factories" (the other 16 agree). "Known limitation: for an emoji in a heading, pandoc 3.13 writes the emoji's name into the anchor ..." (REQ-EXPORT-040 notes) and "Assumptions, not verified against SAS: fractional seconds are truncated ..." (REQ-FILE-037 notes) have no LIM entry, so report section 8.3 omits them.
- Failure scenario: a behaviour change updates one copy of a duplicated requirement and its case while the twin keeps asserting the old text (both stay `verified` if their separate cases are edited independently, or one fails with no hint that a duplicate exists); the "581 requirements" figure overstates distinct obligations.
- Confidence: verified-by-reading
- Suggested direction: keep one requirement per statement and cross-reference it from the other areas; move the two note-only limitations into `deviations.yaml`.

### F-q1-qual-infra-34  [minor] [doc-wrong]
- Where: inst/qualification/risk-assessment.yaml:198-206, :208-216, :229-241; inst/qualification/README.md:144-155; docs/references.md:54-57
- What: The risk assessment is an orphan document — nothing reads it, the README's directory table does not list it and it never reaches a bundle — and several of its statements about test depth are untrue.
- Evidence: `grep -rn "risk-assessment"` over `R/`, the suite, its docs, `tests/testthat` and `.github` returns nothing. APP: "Browser scenarios run where a browser is available and are reported as not executed otherwise" (no browser case exists, F-13) and "severity: minor ... a usability problem rather than a data-integrity one" although 31 of the 60 REQ-APP requirements are `risk: high` and REQ-APP-050 is `category: security`. ROBUST: "Bad input per exported function" — OQ-ROBUST-001 (tests/test-OQ-ROBUST-inputs.R:29-90) probes four functions with a wrong type. ENV: "Full inventory with file-level checksums against a released manifest" — the manifest covers `inst/` only (F-22; the package's R code is checked only where an MD5 file was installed).
- Failure scenario: validation-plan.md:20-27 says testing depth follows risk and that "the report says which is which through each requirement's risk rating"; the area-level assessment a quality unit would file next to the plan contradicts both the requirement-level ratings and the tests, and no check keeps the two in step.
- Confidence: verified-by-reading (the per-function claim was checked against OQ-ROBUST-001 only; bad-input cases in other areas were not enumerated)
- Suggested direction: either generate the area table from the requirement files and print it in the report, or delete the file; correct the three depth statements.

### F-q1-qual-infra-35  [minor] [inconsistency]
- Where: R/run_dta_app.R:32; inst/shiny/dta_app/app.R:7-9; inst/qualification/requirements/REQ-APP.yaml:21-29 (REQ-APP-001), :41-49 (REQ-APP-003); inst/qualification/tests/helper-app.R:22, tests/test-OQ-APP-server.R:18-31, :46-67; inst/qualification/README.md:65
- What: The launcher requires four packages and the app attaches `shinyjs`, but the requirement, the suite's own gate and the README all say three (`shiny`, `bslib`, `DT`).
- Evidence:
  ```r
  required <- c("shiny", "bslib", "DT", "shinyjs")        # R/run_dta_app.R:32
  library(shinyjs)                                         # inst/shiny/dta_app/app.R:9
  qapp_required_packages <- function() c("shiny", "bslib", "DT")   # helper-app.R:22
  ```
  REQ-APP-001: "shall require the shiny, bslib and DT packages"; OQ-APP-003's comment: "The application needs three packages." OQ-APP-001 checks only `formals(run_dta_app)`, so the required set is verified by no case.
- Failure scenario: on a system with shiny, bslib and DT but without shinyjs, `qapp_skip_unless_app_installed()` does not skip, the app's `library(shinyjs)` fails when `shiny::testServer()` loads it, and the ~87 application cases error — a `FAIL` verdict where README.md:57-65 promises that a missing optional package is "recorded rather than fatal ... reported as not executed". Mutation not detected: dropping `shinyjs` (or any package) from `required` in `run_dta_app()`.
- Confidence: verified-by-reading for the three lists; that `testServer()` executes `library(shinyjs)` is inferred (not run)
- Suggested direction: add `shinyjs` to REQ-APP-001, `qapp_required_packages()` and the README row, and assert the required set in OQ-APP-001/-003.

### F-q1-qual-infra-36  [minor] [doc-wrong]
- Where: inst/qualification/README.md:57, :98-109, :132-140, :154; docs/references.md:66-68; docs/glossary.md:87-90; docs/maintaining.md (no section); .github/scripts/build_container_fixtures.R:1-21, build_sas7bdat_fixtures.R:1-15
- What: Several smaller statements in the suite's documentation do not match the runner and the tree.
- Evidence: (a) README.md:57 / references.md:66-67: "Only R, the package and its dependencies, plus `testthat`" — `run_qualification()` also aborts without `withr` (R/qualification.R:1823-1829), which is only in `Suggests`. (b) README.md:105-106 lists the `results/` CSVs as "tests, expectations, traceability, requirements, coverage, deviations, meta checks, environment": the environment is `environment.json`, and `limitations.csv`, `performance.csv`, `unit-tests.csv`, `sessioninfo.txt` and `run.json` are written too (R/qualification.R:1490-1519, :2004-2007). (c) README.md:108, :136-137 name `SHA256SUMS` and `sha256sum -c` unconditionally; on R < 4.5, which `DESCRIPTION` allows, the manifest is `MD5SUMS` (R/qualification.R:1389-1415). (d) README.md:140 "covers every file except itself": `list.files(all.files = FALSE)` (:1423) leaves out any dot-file a case wrote to the bundle. (e) README.md:154 calls `fx/` "small hand-built inputs"; the 34 container fixtures and two of the three SAS fixtures are generated by `.github/scripts/build_container_fixtures.R` and `build_sas7bdat_fixtures.R`, which neither README nor maintaining.md mentions, and no workflow rebuilds and compares them (`deflated.zip` and `zip64_one.zip` are not used by any qualification case, only by `tests/testthat`). (f) glossary.md:87-90 "the same defects are injected at the same relative positions" at every tier — not at 10 million rows (F-08). (g) the `white-box` tag is advisory only: OQ-TYPE-030 to -035 call the internal `dta_as_numeric_strict()` and helper-parity.R:259 reads `@validation_store` without it, and M13 checks tag spelling only.
- Failure scenario: a maintainer changing a container fixture edits the binary by hand because the docs never mention the generator; an auditor on R 4.4 looks for `SHA256SUMS`; a reader of README trusts a file inventory that is incomplete.
- Confidence: verified-by-reading
- Suggested direction: correct the seven statements; add a "you changed a fixture" section to maintaining.md and a CI step that rebuilds the fixtures into a temp dir and compares hashes.

### F-q1-qual-infra-37  [minor] [design]
- Where: R/qualification.R:566-571 (`qual_requirements_df`), R/qualification-report.R:204-206; inst/qualification/docs/maintaining.md:286-288; e.g. requirements/REQ-APP.yaml:798-805 (REQ-APP-050), :189-199 (REQ-APP-013), REQ-TMPL.yaml:427-441 (REQ-TMPL-022), REQ-FILE.yaml:493-505 (REQ-FILE-037)
- What: The `notes:` of a requirement — the place the suite records that a behaviour is "pinned, not endorsed", verified only structurally, or not verified by this suite at all — are dropped when the requirements are loaded, so they reach neither the report nor any evidence file.
- Evidence:
  ```r
  rows[[length(rows) + 1]] <- data.frame(
    id = id, area = area, area_title = doc$title %||% NA_character_,
    text = req$text, risk = req$risk, category = req$category,
    covers = I(list(covers)), source = req$source %||% NA_character_, file = basename(f), ...)
  ```
  215 of the 581 requirements carry `notes:`. REQ-APP-050 (`risk: high`, `category: security`): "The application additionally refuses any value that arrived as a client input message. shiny::testServer cannot deliver one, so that binding rests on the developer suite's wiring ... and on a browser run, not on this suite. This requirement is verified here for the value itself." The report's appendix prints `id, risk, category, text` only, and `requirements.csv`/`results.json` are written from the same frame.
- Failure scenario: section 7 shows REQ-APP-050, REQ-APP-013, REQ-TMPL-022 and the other annotated requirements as plain `verified`; the reviewer who signs cannot see that the client-message guard of the upload path is outside this suite (and no browser run exists in it, F-13), that a clause is pinned against the author's own judgement, or that an expected value rests on an assumption "not verified against SAS".
- Confidence: verified-by-reading
- Suggested direction: carry `notes` (and `source`) into `requirements.csv`, `results.json` and the appendix, and mark requirements whose notes declare a verification limit in the matrix.

---

## Coverage

**Fully read (every line):**
- `inst/qualification/README.md`; `inst/qualification/docs/maintaining.md`, `validation-plan.md`, `glossary.md`, `references.md`
- `inst/qualification/requirements/_api.yaml`, `_index.yaml` and all 17 area files: `REQ-APP`, `REQ-CONT`, `REQ-CORE`, `REQ-ENV`, `REQ-EXPORT`, `REQ-FILE`, `REQ-MATRIX`, `REQ-META`, `REQ-PERF`, `REQ-REPORT`, `REQ-ROBUST`, `REQ-RULE`, `REQ-SPEC`, `REQ-STREAM`, `REQ-TMPL`, `REQ-TYPE`, `REQ-VAL`
- `inst/qualification/deviations.yaml`, `inst/qualification/risk-assessment.yaml`
- `inst/qualification/tests/helper-00-qa.R`, `helper-app.R`, `helper-container.R`, `helper-generators.R`, `helper-matrix.R`, `helper-parity.R`, `helper-parse.R`, `setup-qualification.R`
- `.github/scripts/qualification_verdict.R`, `sync_qualification_manifest.R`, `build_container_fixtures.R`, `build_sas7bdat_fixtures.R`; `.github/workflows/qualification.yml`, `ci-tier.yml`, `test-coverage.yaml`
- Opened in full to understand the slice: `R/qualification.R`, `R/qualification-report.R`, `inst/qualification/tests/test-IQ-ENV.R`, `test-OQ-CORE-runner.R`, `test-PQ-PERF-scale.R`; `NAMESPACE`, `DESCRIPTION`, `.Rbuildignore`, `.gitattributes`

**Checked mechanically rather than by eye (bash/awk/sha256sum, no R):**
- `inst/qualification/baseline/inst-files.sha256`: header and format read; the 363 listed paths equal the files under `inst/` (minus `baseline/`) and all 363 sha256 values match the pinned tree.
- `inst/qualification/fx/`: directory listing, binary fixtures not opened byte by byte; verified against their generators by reading the generators, and checked which fixtures any qualification case references. The ~200 small YAML fixtures under `fx/t/` were listed but NOT read individually (they are inputs of the TMPL cases in the q3 slice).
- All 812 test titles: grammar, file/stage/area agreement, duplicates, per-area counts vs `min_tests`, tags, cited requirement ids vs the 581 declared; `covers` vs `_api.yaml` vs `NAMESPACE`; `affects:` ids; DEV/LIM references to test ids; `file:line` references in `source:`/`reference:` (all of REQ-ENV/FILE/RULE/TYPE and the register, a sample of nine in REQ-TMPL).

**Only partly read (cross-slice, opened for a specific question):** `inst/qualification/tests/test-OQ-VAL-accessors.R` (OQ-VAL-034), `test-OQ-VAL-check.R` (OQ-VAL-038), `test-OQ-VAL-columnspec.R` (OQ-VAL-001), `test-OQ-TYPE-rbackend.R` (OQ-TYPE-042), `test-OQ-TYPE-numeric.R` (head), `test-OQ-SPEC-yaml.R` (OQ-SPEC-001/-004), `test-OQ-SPEC-accessors.R` (OQ-SPEC-041), `test-OQ-STREAM-parity.R` (lines 1-232), `test-OQ-ROBUST-inputs.R` (lines 1-100), `test-OQ-APP-server.R` (lines 1-70), `test-PQ-PERF-repeatability.R` (three excerpts), `tests/testthat/test-qualification.R` (lines 1-185, 423-446), `R/run_dta_app.R` and `inst/shiny/dta_app/app.R` (grep and the first 30 lines).

**Not read:** the remaining test files of the q2/q3 slices; the other `.github/workflows` and scripts beyond their trigger blocks; `thoughts/` (only its file listing).

**Not verifiable by reading (stated as such in the findings):** anything that needs execution — in particular that the `full` tier actually fails (F-08, arithmetic traced by hand), `wmic` availability (F-20), `testServer()` sourcing `library(shinyjs)` (F-35), and the historical mutation table in validation-plan.md section 4a (only the existence and titles of the named cases were checked).
