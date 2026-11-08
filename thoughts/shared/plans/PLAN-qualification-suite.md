# PLAN: DTAtools Qualification Suite (IQ/OQ/PQ, audit-grade)

**Status:** for approval
**Target branch:** `dev` (feature branch `ai/pharma-validation-tests-cdc777`)
**Version bump:** none (CHANGELOG entry under `[Unreleased]` only)
**Implementation model:** main thread writes/holds the contracts in §4 and reviews; sonnet-class agents implement the disjoint work packages of §6; a full-strength reviewer runs WP16. The brief in §6.1 is the prompt skeleton for every agent.
**User decisions taken (2026-09-09):** suite ships inside the package; default scale tier is `full`; app scope per §12.

---

## 1. Context

DTAtools validates clinical and biomarker data transfers against YAML Data Transfer Specifications and is deployed (package + Shiny app) on Posit Connect instances inside a pharmaceutical company. Software that gates clinical data needs documented evidence that the *installed* software works as specified. In GAMP 5 terms DTAtools is a **Category 5 (custom) application**, so the expected evidence set is: requirements, a risk assessment, Installation / Operational / Performance Qualification (IQ/OQ/PQ) protocols with recorded expected-vs-actual results, a traceability matrix, a deviations register and a signed Validation Summary Report.

What exists today (verified in this worktree):

- A strong developer unit suite: `tests/testthat`, 75 files, ~1,820 `test_that()` blocks, incl. a 17-case validation corpus (`tests/testthat/helper-validation-corpus.R`), a snapshot oracle (`test-validation-oracle.R`), a seeded randomised eager-vs-streaming differential test (`test-streaming-parity.R`, 25 seeds, file + R-typed generators) and 29 Shiny `shiny::testServer()` files (`helper-shinyapp.R`).
- Benchmark scripts under `benchmarks/` (build-ignored) and in-package instrumentation: `check(<DTA>, benchmark = TRUE)` / `validate_file_stream(benchmark = TRUE)` / `options(DTAtools.benchmark = TRUE)` → `validation_benchmark()` returns 13 metrics incl. Arrow pool peak (`R/benchmarking.R`). Note `check()` for `DTADataSetTabular` has **no** `benchmark` argument (`R/DTADataSetTabular-class.R:1240`); the DTA method has (`R/DTA-class.R:728`) — PQ uses the option.
- CI: `R-CMD-check` (5-leg matrix), `r-style` (styler 1.11.0, roxygen2 8.1.0, dependency/version/manifest checks), `pre-commit`, `manifest-sync`.
- DTAtools is **not installed** in the user library of the dev machine (`requireNamespace("DTAtools")` is FALSE); everything runs through `pkgload::load_all()` today.

What is missing for an audit: the unit suite is not installed with the package (`tests/` is not shipped), is not requirement-traced, records no per-step expected/actual evidence, uses snapshots (actual = expected by definition), and says nothing about the environment the package is deployed into. Nothing in the repo mentions GAMP, 21 CFR Part 11, Annex 11, IQ/OQ/PQ or traceability (grep verified, whole repo minus `renv/`).

**Outcome:** one command on the target system —

```r
DTAtools::run_qualification("~/dta-qualification")
```

— produces a self-contained, tamper-evident evidence bundle and a Validation Summary Report (Markdown always; HTML + DOCX when pandoc exists; PDF when a PDF backend exists) with full traceability, so a QA reviewer can sign it and an inspector can re-verify every hash and every file:line pointer.

---

## 2. Regulatory basis (verified 2026-09-09; cited in `docs/references.md`)

| Reference | Status | Used for |
| --- | --- | --- |
| ISPE GAMP 5, 2nd ed. (July 2022) | current | lifecycle model, Category 5, IQ/OQ/PQ, traceability, leverage of supplier (developer) evidence |
| FDA 21 CFR Part 11 | current | electronic-record integrity; attributable, contemporaneous, tamper-evident evidence |
| FDA *Computer Software Assurance for Production and Quality System Software* | final 24 Sep 2025, updated 3 Feb 2026 (supersedes GPSV §6) | risk-based assurance: scripted testing for high-risk functions; recorded unscripted testing acceptable for low-risk |
| FDA *General Principles of Software Validation* (2002) | current except §6 | independence of review, validation principles |
| EU GMP Annex 11 (2011); revision draft 7 Jul 2025 (consultation closed 7 Oct 2025; final expected 2026) | current + upcoming | computerised-system validation, audit trail, data integrity |
| ICH E6(R3) GCP | adopted 6 Jan 2025; EU effective 23 Jul 2025; FDA published Sep 2025 | computerised systems in clinical trials; validation proportionate to risk |
| PIC/S PI 041-1 | current | ALCOA+ properties of the evidence bundle |
| R Validation Hub white paper *A Risk-based Approach for Assessing R package Accuracy within a Validated Infrastructure* (pharmaR); `riskmetric` 0.2.5 dimensions | current | package-level risk assessment (documentation, testing, maintenance, community) |
| PHUSE `valtools` 0.4.1 (GitHub only, not CRAN) | reference only | artifact set (requirements / test cases / test code / report) reproduced natively; **not** a dependency |

---

## 3. Decisions

| Decision | Choice | Rationale |
| --- | --- | --- |
| Location | `inst/qualification/` (installed) + `R/qualification.R`, `R/qualification-report.R` | must run on the deployed system against the **installed** package; `tests/` is not installed. **User-confirmed.** |
| Entry point | exported `run_qualification(output_dir, ...)`; `Rscript -e "DTAtools::run_qualification('<dir>')"` | one command; no devtools on the target |
| Default scale | `scale = "full"` (1e7 rows, 1000-seed parity, 50-run stability); `standard` and `quick` as arguments | runtime is not a concern for the manual run. **User-confirmed.** |
| Vocabulary | "qualification" for the software (IQ/OQ/PQ); "validation" stays the package's domain word; formal document titles keep the regulatory names (Validation Plan, Validation Summary Report) | avoids `validate_*`/`validation_*` name clashes |
| Test framework | testthat 3 (already Suggests); per-expectation evidence via `testthat::succeed()`/`fail()` messages; contemporaneous event log via a small R6 reporter | verified in §4.9: `test_dir()` always returns every expectation with the call-site `srcref`, so every step has a file:line pointer |
| Test directory | **flat** `inst/qualification/tests/` with `test-IQ-*.R`, `test-OQ-*.R`, `test-PQ-*.R`; stage selected with `filter = "^IQ-"` etc. | testthat sources `helper-*.R`/`setup-*.R` only from the directory of the test files (verified) |
| Expected results | written **before** execution (requirement text + fixture files); **no `expect_snapshot*` anywhere** (meta-check M8); no `expect_true(TRUE)`-class assertions | a snapshot cannot fail against an independent expectation; snapshots would also write into the read-only library |
| Black box | OQ/PQ use the exported API; a test that touches an internal is tagged `white-box` and listed in the report | the delivered interface is what is qualified |
| Reports | Markdown assembled in R → HTML (self-contained) + DOCX via `rmarkdown::render()` on the `.md` when pandoc exists → PDF via the existing internal `.convert_docx_to_pdf()` (`R/exportDocuments.R:791`) when `dta_pdf_backend()` finds a backend; JSON + CSV always | verified in §4.9; robust on bare servers; signable documents where possible |
| Hashing | `tools::sha256sum()` (R ≥ 4.5.0) else `tools::md5sum()`; manifest named `SHA256SUMS` or `MD5SUMS`, algorithm recorded in report and JSON | tamper evidence with zero new dependencies (Depends is R ≥ 4.1.0) |
| Dependencies | **no new Imports**; Suggests += `R6` only (a hard dependency of testthat and shiny, so present wherever tests can run). No `digest`, `callr`, `shinytest2`, `chromote`, `valtools`, `riskmetric` | subprocess tests use `system2(Rscript)` (`qa_subprocess`); the browser tier skips when its packages are absent (§12) |
| App coverage | `shiny::testServer()` OQ + PQ launch smoke always; browser tier optional, never blocking (§12). **User-confirmed.** | Chrome stays a property of the qualification host, not of the package |
| Known defects | `inst/qualification/deviations.yaml` register (DEV-nnn) + limitations (LIM-nnn); a test binds a defect with `qa_known_deviation(id, expr)`; the run FAILS when an open deviation no longer reproduces or is never reproduced | never hides defects; forces the register to track fixes |
| Unit suite | executed as *developer test evidence* when available (installed with `--install-tests`, or a `pkgload` dev tree); else the report points to the CI run of the package SHA | supplementary evidence, never a substitute |
| Performance acceptance | correctness at scale (exact counts) and scaling/memory ratios are hard criteria with wide tolerances; absolute throughput is recorded and compared with `baseline/reference-performance.json`, failing only when `perf_floor` is set | slow hardware must not fail correctness-grade qualification |
| CI | `.github/workflows/qualification.yml` (manual + weekly, `scale = "quick"`, 3 OS, bundle uploaded); PR gating only via `tests/testthat/test-qualification.R` (static consistency + IQ in tempdir) | prevents rot without slowing PRs |
| Integrity manifest | `inst/qualification/baseline/inst-files.sha256` regenerated by `.github/scripts/sync_qualification_manifest.R`, `--check` in the `r-style` workflow | same pattern as the app manifest; IQ verifies installed `inst/` files byte-for-byte |

---

## 4. Architecture and contracts (binding for WP0; other WPs consume them)

### 4.1 Files to create / modify

```
R/qualification.R                         # exported run_qualification(), qualification_requirements(); runner internals; R6 reporter; S3 methods
R/qualification-report.R                  # Markdown report builder + format renderers (internal)
inst/qualification/
  README.md                               # run / review / sign / verify procedures, tiers, resources, offline notes
  requirements/
    _index.yaml                           # area list (order, title, min_tests), suite_revision, package_version, risk model
    _api.yaml                             # frozen export list (107 symbols at 0.25.0)
    REQ-CORE.yaml REQ-ENV.yaml REQ-SPEC.yaml REQ-TYPE.yaml REQ-VAL.yaml REQ-RULE.yaml REQ-FILE.yaml
    REQ-STREAM.yaml REQ-META.yaml REQ-TMPL.yaml REQ-EXPORT.yaml REQ-REPORT.yaml REQ-APP.yaml
    REQ-ROBUST.yaml REQ-PERF.yaml
  deviations.yaml                         # DEV-nnn and LIM-nnn
  risk-assessment.yaml                    # per area: severity × probability × detectability → test depth
  docs/validation-plan.md docs/glossary.md docs/references.md
  baseline/reference-performance.json     # measured on the named reference machine (WP14)
  baseline/inst-files.sha256              # generated manifest of inst/ (excludes qualification/baseline/)
  fixtures/oracle/*.yaml                  # expected results for bundled example data, derived by inspection
  fixtures/yaml/ fixtures/files/ fixtures/templates/ fixtures/docx/   # tiny negative/edge fixtures
  tests/                                  # FLAT
    setup-qualification.R helper-qa.R helper-generators.R helper-parse.R helper-parity.R helper-app.R
    test-IQ-ENV.R
    test-OQ-<AREA>-<topic>.R              # one or more files per area
    test-PQ-<topic>.R
tests/testthat/test-qualification.R       # PR guard
.github/workflows/qualification.yml
.github/scripts/sync_qualification_manifest.R
.github/workflows/r-style.yaml            # + step running the manifest script with --check
DESCRIPTION                               # Suggests += R6; Collate += qualification.R, qualification-report.R (after validateTemplate.R, before validationFunctions.R)
CHANGELOG.md README.md vignettes/DTAtools.Rmd (short eval=FALSE "Qualification" section)
man/ NAMESPACE                            # regenerated with roxygen2 8.1.0 — never hand-edited
```

### 4.2 Identifier scheme and test-title grammar

- Areas: `CORE ENV SPEC TYPE VAL RULE FILE STREAM META TMPL EXPORT REPORT APP ROBUST PERF`.
- `REQ-<AREA>-<nnn>`, `<STAGE>-<AREA>-<nnn>` (`STAGE ∈ IQ|OQ|PQ`), `DEV-<nnn>`, `LIM-<nnn>`, `RISK-<AREA>-<nn>`.
- Title grammar (regex applied by `qual_parse_test_name()`; meta-check M1):

  ```
  ^(IQ|OQ|PQ)-([A-Z]{2,8})-([0-9]{3}) \| ([^|]+?) \| (REQ-[A-Z]{2,8}-[0-9]{3}(?: REQ-[A-Z]{2,8}-[0-9]{3})*)(?: \| tags: ([a-z-]+(?:,[a-z-]+)*))?$
  ```

  `tc_id = "\1-\2-\3"`; the file must be `test-\1-\2-*.R` (M2). Tags allowed: `white-box`, `subprocess`, `browser`, `slow`, `scale-standard`, `scale-full`. Example: `test_that("OQ-VAL-007 | codelist violation reported per row | REQ-VAL-001 REQ-VAL-009", { ... })`.

### 4.3 YAML schemas

```yaml
# requirements/REQ-VAL.yaml
area: VAL                    # ^[A-Z]{2,8}$ and equal to the file's area
title: Data validation engine
requirements:
  - id: REQ-VAL-001          # unique across all files
    text: "check() shall report a table as valid (ok = TRUE) if and only if it has zero column-specification, zero rule and zero import errors."
    risk: high               # high | medium | low  — impact on the integrity of a data transfer
    category: functional     # functional | interface | installation | performance | integrity | security | documentation
    covers: [check, validation_status, results]   # exported symbols; each must be in _api.yaml (M6)
    source: "README §The Three Validation Axes; R/DTADataSetTabular-class.R:1240"
    notes: optional — derivation of expected values, code lines, pinned-behaviour remarks
```

```yaml
# deviations.yaml
deviations:
  - id: DEV-001
    title: "SAS format BEST12. is inferred as Int"
    status: open             # open | closed (closed entries stay for history; M7 ignores them)
    severity: minor          # critical | major | minor
    affects: [REQ-TYPE-004]  # must exist (M3)
    observed: "<what the software does>"
    expected: "<what the requirement says>"
    impact: "<consequence for users>"
    workaround: "<text>"
    reference: "tests/testthat/test-DTAColumnSpecStructureSAS.R:24"
    opened: 2026-09-09
    closed: null
limitations:
  - id: LIM-001
    title: "Rows whose every field is empty are dropped by the CSV reader without warning"
    affects: [REQ-FILE-0xx]
    reference: "tests/testthat/test-validation-oracle.R:296"
    consequence: "a file containing all-empty rows validates fewer rows than it contains"
```

Seed entries (WP0 transcribes from the actual comments, not from this summary): `test-DTAColumnSpecStructureSAS.R:24` (BEST12.), `test-DTADataSetTabular-validation.R:942` (POSIXct `tzone = ""` handling), `test-exportFunctions.R:127` (export function behaviour on bad input), the Int-narrowing defect referenced in `test-streaming-parity.R:340-346`; `test-shinyapp-dataset-template.R:824` is fixed → `status: closed`. Candidate entries to confirm in the area WPs: unknown group-constraint type silently passes (`R/evaluateRules.R:1341`), duplicate rule ids unchecked, `"NaN"` classified unconvertible, all-empty rows dropped (LIM-001).

### 4.4 Exported functions

```r
run_qualification(
  output_dir,                                    # required; bundle is created inside it
  stages = c("IQ", "OQ", "PQ"),                  # order preserved; character(0) = static mode (no tests)
  scale = c("full", "standard", "quick"),
  formats = c("md", "html", "docx", "pdf"),      # "md" always; others best-effort
  filter = NULL,                                 # regex on test file names; a filtered run is marked PARTIAL
  tester = Sys.info()[["user"]],
  reviewer = NULL,                               # printed in the approval block
  include_unit_tests = c("auto", "never", "always"),
  perf_floor = NULL,                             # rows/sec; NULL = informational baseline comparison
  seed = 20260101L,
  quiet = FALSE
) -> invisible(<dta_qualification>)
qualification_requirements(area = NULL, dir = NULL) -> data.frame(id, area, area_title, text, risk, category, covers<list>, source, file)
```

Semantics: validates arguments with `cli::cli_abort()`; requires testthat ≥ 3.0.0 and R6 (abort naming the package); creates `bundle_dir <- file.path(output_dir, run_id)` (abort if it exists); opens `run.log`; sets `options(DTAtools.qualification = list(run_id, bundle_dir, scale, seed, log, tester))` for the run (restored on exit); writes `environment.json`/`sessioninfo.txt` **first**; discovers tests statically; runs the requested stages in order, never stopping early; runs the unit suite when applicable; builds data frames, meta-checks, verdict; writes bundle files; builds the Markdown report; renders other formats (each renderer failure is logged, never fatal); writes the hash manifest last; closes the log; prints the summary unless `quiet`. Never writes outside `bundle_dir` and `tempdir()`. `@examples`: `run_qualification(tempfile("qual"), stages = "IQ", scale = "quick", formats = "md")` (seconds; no `\dontrun`).

### 4.5 Internal functions (`R/qualification.R`, prefix `qual_`)

| Function | Contract |
| --- | --- |
| `qual_root(dir = NULL)` | `dir %||% system.file("qualification", package = "DTAtools")`; abort if `""` |
| `qual_config()` | `getOption("DTAtools.qualification")`; NULL outside a run |
| `qual_run_id(version, time)` | `sprintf("QUAL-%s-%s", version, format(time, "%Y%m%d-%H%M%S", tz = "UTC"))` |
| `qual_log_open(path)`, `qual_log(event, ...)` | appends one tab-separated line `"<%Y-%m-%dT%H:%M:%OS3%z>\t<event>\t<fields…>"` and `flush()`es; events `run_start env stage_start file_start test_start expectation test_end file_end stage_end unit_tests meta_check artifact render hash run_end`; fields sanitised with `gsub("[\t\r\n]", " ", x)` |
| `QualReporter` | R6 class `inherit = testthat::Reporter`; `start_file/start_test/add_result/end_test/end_file` call `qual_log`; `add_result` logs type, test (or `"(file level)"`), `conditionMessage()` truncated to 500 chars |
| `qual_discover_tests(tests_dir)` | `parse(keep.source = TRUE)` every `test-*.R`; top-level `test_that()` calls with a string-literal first argument → data.frame(file, line, name, ok, tc_id, stage, area, seq, title, req_ids<list>, tags<list>); non-literal names → `ok = FALSE` |
| `qual_parse_test_name(name)` | applies §4.2 grammar |
| `qual_stage_filter(stage, filter)` | `"^<STAGE>-"`, or `sprintf("^%s-.*(%s)", stage, filter)` |
| `qual_run_stage(stage, tests_dir, filter, reporter)` | `withr::local_envvar(TESTTHAT_PARALLEL = "FALSE", TESTTHAT_EDITION = "3")`; `testthat::test_dir(tests_dir, filter = qual_stage_filter(stage, filter), reporter = reporter, env = NULL, load_helpers = TRUE, stop_on_failure = FALSE, stop_on_warning = FALSE, package = "DTAtools", load_package = "none")`; an abort from `test_dir` itself (e.g. no files match) becomes a `stage_error` log line and zero tests |
| `qual_tests_df(results, stage)` | one row per `testthat_results` element: `stage file tc_id name title req_ids tags n_expectations n_pass n_fail n_error n_skip n_warning status skip_reason real_sec`; `status ∈ pass fail error skip expected_failure file_error`; file-level entries (`test` NULL) → `tc_id = basename(file)`, `status = "file_error"` |
| `qual_expectations_df(results, stage)` | one row per expectation: `stage tc_id seq type message file line` (from `srcref`) |
| `qual_requirements_df(root)`, `qual_deviations_df(root)` | YAML → data.frames; schema validation aborts listing every violation |
| `qual_traceability(req_df, disc_df, tests_df, stages_run)` | long table `req_id tc_id stage status` (+ `not_executed` for discovered-but-not-run) and per-REQ status: `verified` (≥ 1 pass, no fail/error), `failed`, `not_verified` (all executed TCs skipped), `not_executed`, `uncovered` |
| `qual_meta_checks(...)` | data.frame(check_id, description, ok, detail): **M1** every `test_that` name parses; **M2** tc_ids unique and consistent with file names; **M3** every referenced REQ/DEV id exists; **M4** every REQ has ≥ 1 TC; **M5** every `_api.yaml` symbol appears in ≥ 1 `covers`; **M6** every `covers` symbol is an export; **M7** every open DEV bound by exactly one `qa_known_deviation()` (static grep of the sources; at run time exactly one `"DEV-nnn reproduced"` expectation); **M8** no `expect_snapshot` in the sources; **M9** DEV/LIM ids unique; **M10** every discovered TC of a run stage was executed; **M11** per-area TC count ≥ `min_tests` from `_index.yaml`; **M12** `_index.yaml$package_version` equals `packageVersion("DTAtools")` |
| `qual_deviation_status(dev_df, exp_df)` | `id status(reproduced/not_reproduced/not_executed) tc_id message` |
| `qual_environment(tester)` | list: run (id, tester, started_at), package (version, sha = `RemoteSha %||% GithubSHA1`, built, packaged, library_path, loaded_via = pkgload/installed, md5_check = `tools::checkMD5sums("DTAtools")` or NA), r (version, platform, arch), locale, tz, hostname, user, pid, cwd, encoding (`l10n_info()`), arrow (`arrow::arrow_info()` subset: version, codecs, dataset/acero/compute flags), pandoc (available, version), pdf_backend (`dta_pdf_backend()`), chrome (path found via `CHROMOTE_CHROME`/`DTATOOLS_CHROME`/chromote default, or NA), memory (via `ps` when installed), dependencies = `qual_dependency_check()`, options (every `DTAtools.*` option and its value) |
| `qual_dependency_check()` | own regex parser over `read.dcf` Depends/Imports/Suggests → `package field required_version installed_version installed satisfied` (R itself is a row) |
| `qual_unit_tests(mode)` | runs `testthat::test_dir()` on `system.file("tests/testthat", package = "DTAtools")` (installed with `--install-tests`) or `<pkgload::pkg_path()>/tests/testthat` when `pkgload::is_dev_package("DTAtools")`; "auto" = run when found and `scale != "quick"`; returns summary df or NULL + reason |
| `qual_verdict(tests_df, meta_df, dev_status, partial)` | `FAIL` if any status `fail/error/file_error`, any meta-check not ok, any open DEV `not_reproduced`; else `PASS WITH NOT-EXECUTED TESTS` if any skip; else `PASS`; suffix ` (PARTIAL)` when `filter` was used or a stage omitted |
| `qual_write_bundle(x, dir)` | writes everything in §4.6 (`jsonlite::write_json(auto_unbox = TRUE, pretty = TRUE, digits = NA, na = "null", null = "null")`; `utils::write.csv(row.names = FALSE, fileEncoding = "UTF-8")`) |
| `qual_hash_bundle(dir)` | hashes every file except the manifest; writes `SHA256SUMS`/`MD5SUMS` as `<hash>  <relative/path>` (LF, `sha256sum -c` compatible); returns list(algorithm, files); called last; the report embeds the hash of `results/results.json` computed before rendering |
| `new_dta_qualification()`, `print.dta_qualification()`, `summary.dta_qualification()` | `print`: verdict, run id, bundle dir, per-stage pass/fail/error/skip, failed meta-checks, report paths (via `cli`); `summary`: per-stage table + every non-pass TC with its first failing message; returns the table invisibly |

`R/qualification-report.R`: `qual_report_md(x)` (character vector, pipe tables via `qual_md_table()`, escapes `|`), `qual_render_formats(md_path, formats, log)` (HTML: `rmarkdown::render(md, html_document(self_contained = TRUE, toc = TRUE, toc_depth = 2), quiet = TRUE)`; DOCX: `word_document(toc = TRUE)`; PDF: `.convert_docx_to_pdf()` when `dta_pdf_backend()` non-NULL), report text constants.

### 4.6 Evidence bundle and `results.json`

```
<output_dir>/QUAL-<version>-<YYYYMMDD-HHMMSS>/
  SUMMARY.txt                     verdict, counts, results.json hash, verify instructions
  run.log                         contemporaneous event log
  report/qualification-report.md [.html] [.docx] [.pdf]
  results/results.json tests.csv expectations.csv traceability.csv requirements.csv deviations.csv
          meta_checks.csv performance.csv environment.json sessioninfo.txt unit-tests.csv (when run)
  artifacts/<tc_id>/...           files kept by tests via qa_artifact_dir(); each hashed
  SHA256SUMS | MD5SUMS            written last; excludes itself
```

`results.json` (schema_version 1): `generator {package, version, r_version}`, `run {id, tester, reviewer, hostname, started_at, finished_at, stages_requested, stages_run, scale, filter, partial, formats_requested, formats_rendered{html,docx,pdf}, hash_algorithm, results_json_sha}`, `package {version, sha, built, packaged, library_path, loaded_via, md5_check}`, `summary {verdict, per_stage[{stage,n_tests,pass,fail,error,skip,expected_failure,file_error,warnings,sec}], n_requirements, n_verified, n_failed, n_not_verified, n_not_executed, n_uncovered, n_deviations_open, n_reproduced, n_not_reproduced}`, `tests[]`, `expectations[]`, `requirements[{id,area,text,risk,category,covers[],tc_ids[],status}]`, `traceability[]`, `deviations[]`, `limitations[]`, `meta_checks[]`, `performance[]` (`tc_id tier rows path elapsed_sec cpu_user_sec cpu_sys_sec r_peak_mb rss_start_mb rss_end_mb arrow_pool_peak_mb rows_per_sec metrics_version baseline_rows_per_sec delta_pct`), `unit_tests`, `environment`, `artifacts[{tc_id,path,bytes,hash}]`. `tests.csv`/`expectations.csv`/`traceability.csv` carry **no timestamps** so two runs diff cleanly.

### 4.7 Validation Summary Report outline (H1 "DTAtools Software Qualification Report")

1. Identity: run id, package version + SHA, suite revision, tester, host, start/finish (UTC + local), **verdict in bold**, `results.json` hash + algorithm.
2. Approval block: Performed by / Reviewed by / Approved by — name, role, date, signature (blank rows).
3. Scope and approach: stages, tier and row counts, filter, GAMP 5 Cat. 5 / IQ-OQ-PQ / ALCOA+ mapping (static text embedded from `docs/validation-plan.md` §1).
4. Environment: R, platform, locale, TZ, arrow capabilities, pandoc/PDF/Chrome availability, dependency table, `DTAtools.*` options.
5. IQ results, 6. OQ results per area, 7. PQ results (+ performance tables and scaling/memory criteria with measured ratios and baseline deltas). TC tables: id, title, reqs, status, expectations pass/fail, seconds, file:line; failing/expected-failing steps expanded with expected/actual text.
8. Traceability matrix (REQ → TCs → status; coverage per area) and reverse index.
9. Deviations: known register with run status; new failures/errors/file errors; limitations.
10. Tests not executed (reason: dependency absent / scale tier / platform / browser absent).
11. Meta-consistency checks.
12. Developer test evidence (unit suite summary or CI reference).
13. Evidence inventory (every file + hash; verification command).
14. Appendices: sessionInfo; full requirement texts; risk assessment; glossary; references.

### 4.8 Helper API (`inst/qualification/tests/helper-*.R`)

`helper-qa.R` (evidence):

| Function | Contract |
| --- | --- |
| `qa_config()`, `qa_scale()`, `qa_seed()`, `qa_bundle_dir()` | read the runner options; outside a run: `scale = "quick"`, bundle NULL |
| `qa_tier_rows(scale = qa_scale())` | quick `1e4`; standard `c(1e4, 1e5, 1e6)`; full `c(1e4, 1e5, 1e6, 1e7)` |
| `qa_parity_seeds(scale)` | quick 25; standard 200; full 1000 |
| `qa_step(desc, expected, actual, ok = identical(expected, actual), tolerance = NULL)` | one expectation; message `"<desc> \| expected: <fmt> \| actual: <fmt>"`, `fmt()` = `paste(format(x), collapse = ", ")` truncated to 300 chars, data.frames as `"<n>x<m> data.frame"`; `tolerance` → `all.equal`; `succeed()`/`fail()`; returns `invisible(ok)`; never throws |
| `qa_check(desc, ok, detail = NULL)` | boolean step |
| `qa_known_deviation(id, expr)` | `expr` TRUE iff the defect is still observable (errors inside count as not reproduced and are quoted); reproduced → `succeed("<id> reproduced: <deparse>")`; else `fail("<id> no longer reproduces; close it in deviations.yaml and promote the test")`; aborts if `id` is not an open entry |
| `qa_requires(...)` | `skip("dependency absent: <pkg>")` for the first missing package |
| `qa_skip_unless_scale(tier)` | `skip("scale tier: <tier> not selected")` |
| `qa_skip_unless_memory_gb(gb)` | `skip("insufficient memory: <gb> GB free required")` using `ps::ps_system_memory()` when `ps` is installed; otherwise no skip |
| `qa_tempdir(env = parent.frame())` | fresh dir under `tempdir()`, `withr::defer(unlink)`; asserts at teardown nothing was written elsewhere |
| `qa_artifact_dir(tc_id)` | `<bundle>/artifacts/<tc_id>/` (created) or `qa_tempdir()` outside a run |
| `qa_perf(tc_id, tier, rows, path, metrics)` | appends `validation_benchmark()` row + keys to `results/performance.csv`; no-op outside a run |
| `qa_hash_file(path)` | runner's algorithm |
| `qa_subprocess(code, envvars = character(), timeout_sec = 1800)` | writes `code` (quoted expr) to a temp `.R` that `saveRDS()`es its value; runs `file.path(R.home("bin"), "Rscript")` `--vanilla --no-init-file` with `system2()` under `withr::with_envvar(envvars)`; paths `shQuote(normalizePath(winslash = "/"))`; returns the value or `stop()`s with captured stderr |
| `qa_messages_norm(x)` | `messages(x)` with `id` removed, fixed column order, sorted, row names dropped → `identical()`-comparable |
| `qa_counts(x)` | `data.frame(source, target, keyword, n)` aggregated from `qa_messages_norm()`; comparable with `qa_oracle()` |

`helper-generators.R` (fixtures with closed-form expectations):

| Function | Contract |
| --- | --- |
| `qa_specs(n_filler = 7L)` | `DTAColumnSpecCollection`: `SUBJID` Char 8 pattern `^S[0-9]{7}$` non-null; `SEX` Char 1 values M/F; `DOMAIN` Char 2 const "GF"; `AGE` Num nullable; `VISIT` Char 3 non-null; `WEIGHT` Num nullable; `FILLnn` Char 12 nullable; rules `age_range` [18,70], `weight_range` [30,200], `subjid_unique`, `adult_visit` (AGE ≥ 18 → VISIT ≠ "V00"), `grp_visit` group_condition on SUBJID (`requires`: any V01 → any V02; WP0 documents the exact clause) |
| `qa_defects()` | named counts, constant across tiers: `sex_enum 3, subjid_pattern 2, subjid_long 1, subjid_na 1, subjid_dup 2, age_text 2, age_range 4, visit_v00 3, weight_range 2, grp_violation 1` |
| `qa_frame(n_rows, seed = 1L, n_filler = 7L, defects = qa_defects(), offset = 0L)` | deterministic (`set.seed(seed + offset)`), all columns character; defects at **known, mutually disjoint** rows computed from `n_rows` (`round(seq(2, n - 1, length.out = k))` shifted per kind); returns `list(frame, positions<list>, defects)` |
| `qa_oracle(frame, positions)` | **independent base-R** count of expected messages: `data.frame(source, target, keyword, n)` — columnspec rows per (column, keyword) using the keyword strings from `R/columnSpecChecks.R` (`required additionalProperties type maxLength enum const pattern`), rule rows per `rule_id`, import rows per column; documents whether `n_rule_errors` counts violated rules or rows (WP0 pins from `R/DTADataSetTabular-class.R` and `tests/testthat/test-clinical-error-fixtures.R:76`, where `n_rule_errors = 7` equals the number of violated rule ids) |
| `qa_dta(frame, specs, dir, name = "ds", file = "data.csv", gzip = FALSE)` | writes the CSV, returns an unloaded `DTA` with one `DTADataSetTabular` + `DTAFileCSV` |
| `qa_write_csv(frame, path, gzip = FALSE, header_style = c("plain","quoted","padded"), eol = c("\n","\r\n"), na = "", quote_all = FALSE, bom = FALSE)` | returns the final path |
| `qa_write_large_csv(n_rows, path, seed, chunk_rows = 1e6, gzip = FALSE)` | streams chunks through `qa_frame(offset = …)` so 1e7 rows never sit in memory; returns `list(path, expected = summed oracle)` |

`helper-parse.R`: `qa_docx_text(path)` (`officer::docx_summary(officer::read_docx(path))$text`), `qa_docx_tables(path)`, `qa_html_report(path)` (→ list(title, n_message_rows, statuses, counts); selectors pinned by WP10 from `R/exportValidationReport.R` + `inst/report/report.js`), `qa_read_yaml(path)`.
`helper-parity.R`: port of `tests/testthat/test-streaming-parity.R` generators (`parity_case`, `parity_case_memory`, `parity_run*`, `parity_sort`) parameterised by seed count.
`helper-app.R`: port of `tests/testthat/helper-shinyapp.R` (`.shiny_app_dir()` → `system.file("shiny/dta_app")`, `app_env()`, `app_fn()`, fixtures, `unlock_editing()` …) — owned by WP11.

### 4.9 Verified testthat 3.3.2 / R 4.5.1 facts implementers rely on (do not re-derive)

- `test_dir()` returns `testthat_results` from its internal ListReporter **regardless of the reporter passed**; each element has `file context test user system real results`; each expectation has `message srcref trace test` and class `expectation_<type>`; `srcref` points at the **call site in the test file** even when raised inside a helper (so `qa_step()` gets a file:line).
- `testthat::succeed("msg")` → `expectation_success` with `conditionMessage() == "msg"`, retained and counted; `fail("msg")` likewise.
- `test_dir(path, filter, reporter, env, ..., load_helpers = TRUE, stop_on_failure = TRUE, stop_on_warning = FALSE, package = NULL, load_package = c("none","installed","source"), shuffle = FALSE)`; `filter` is a regex on the file name minus `test-`/`.R`; `package = "DTAtools", load_package = "none"` runs tests in a clone of the namespace (internals reachable unqualified) without calling `library()`; helper/setup/teardown files are sourced from the **test directory itself**; edition from `TESTTHAT_EDITION` env var; edition 3 sets `LANGUAGE = C` inside tests.
- A **top-level error in a test file does not abort `test_dir()`**: the rest of that file is skipped and one entry with `test = NULL` and an `expectation_error` is recorded → mapped to `file_error`; M10 catches hidden TCs.
- A custom R6 reporter (`inherit = testthat::Reporter`) receives every event in order incl. file-level errors while `test_dir()` still returns the full results.
- `tools::sha256sum()` exists in R ≥ 4.5.0; `rmarkdown::render()` on a `.md` produces self-contained HTML, DOCX and (with TinyTeX) PDF without knitting; `rmarkdown::pandoc_available()` is TRUE on the dev machine without extra env vars.
- testthat `setwd()`s into the test directory, which is **inside the read-only library** on a target system: every write goes through `qa_tempdir()`/`qa_artifact_dir()`.

---

## 5. Stage content — requirement checklists per area

Each bullet becomes ≥ 1 requirement and ≥ 1 test case. Read the cited source before writing the requirement; state expected values explicitly. **pin** = record the actual behaviour as the requirement; if it is unsafe or surprising, ALSO register a `LIM-`/`DEV-` entry. Where the exploration reports disagreed (marked ⚠), the WP verifies from source and records the file:line in `notes`.

### 5.1 CORE (runner self-qualification) — `test-OQ-CORE-*.R`, 6 REQ / 8 TC (WP0)

`qa_step()` failure captured with expected/actual text; `qa_known_deviation()` both branches; a skip is reported with its reason; a file-level error becomes `file_error` and fails the verdict (synthetic test dir under `tempdir()` run via `qual_run_stage()`); grammar parsing (6 cases); hash manifest verifies (`sha256sum -c` semantics reproduced in R); static mode produces a bundle without tests.

### 5.2 ENV (IQ) — `test-IQ-ENV.R`, ≥ 18 REQ / ≥ 18 TC (WP13)

- Identity: `packageDescription("DTAtools")` Version equals `_index.yaml$package_version`; Built/Packaged/RemoteSha or GithubSHA1 recorded (Connect installs carry `GithubSHA1`).
- R ≥ 4.1.0; platform/OS/arch/locale/TZ/encoding recorded; `l10n_info()$`UTF-8` recorded.
- Depends/Imports present and ≥ minimum; Suggests inventory with the TCs each absence disables (from `qa_requires` calls, statically grepped).
- Arrow: version; gzip codec; dataset + Acero/compute availability (`arrow::arrow_info()`); `set_dta_compute_threads()` round trip.
- Namespace loads; each S7 class with a `create_example_*()` constructs and `inherits()` the expected class string; the 15 classes are `S7::S7_class` objects.
- `getNamespaceExports("DTAtools")` equals `_api.yaml` exactly (107 symbols at 0.25.0).
- `tools::checkMD5sums("DTAtools")` TRUE, or skip with reason "no MD5 file (source install)".
- Installed `inst/` file hashes equal `baseline/inst-files.sha256` byte-for-byte (extdata, report css/js, shiny app, templates, qualification requirements/docs/fixtures).
- `inst/shiny/dta_app/manifest.json` file list + checksums match the installed app files (adapt only the file/checksum part of `.github/scripts/check_manifest.R`).
- Help pages for every export (`tools::Rd_db("DTAtools")` aliases ⊇ exports); vignettes installed.
- `DTAtools.*` option defaults ⚠ (exploration reports differ on `use_arrow_compute` default TRUE vs FALSE and on the full list: `stream`, `stream_threshold` 512 MB, `stream_block_size` 8 MiB, `stream_batch_rows` 131072, `transcode_block_bytes` 4 MiB, `max_errors` 10000, `use_arrow_compute`, `arrow_min_rows` 100000, `benchmark` FALSE, `progress_seconds`) — enumerate by `grep -n "getOption(" R/ inst/shiny` and pin each default with file:line.
- Write access to bundle dir and `tempdir()`; free disk ≥ tier requirement (full: 25 GB); pandoc/PDF backend/Chrome recorded (informational).

### 5.3 SPEC — YAML specifications and factories, ≥ 20 REQ / ≥ 35 TC (WP1)

Sources: `R/DTA-class.R:1079` (`read_dta_from_yaml`), `R/DTADataSet-class.R:254/311/154/143`, `R/DTAColumnSpecCollection-class.R:299/693/718`, `R/DTAColumnSpec-class.R:425-495` (JSON schema generics), factories (`DTARuleFactory`, `DTAFileFactory:279`, `DTADataSetFactory:118`, `DTAColumnSpecStructureFactory`, `specs_from_list:98`).

- Bundled specs read with expected dataset names, column ids, rule ids, handler classes, metadata keys written in `fixtures/oracle/spec-*.yaml`: `clinical_dta.yaml`, `clinical_dta_multiple_files.yaml`, `clinical_dta_with_file_dataset.yaml`, `gf_dataset.yaml`.
- Round trips: `write_columns_to_yaml()` → `import_specs_from_yaml()` property-wise equal; `write_columns_to_json()` → `jsonlite` parse → same ids/types; `specs_from_list()`/`dta_from_list()`/`dta_dataset_from_list()`/`dta_file_handlers_from_list()` ≡ YAML path.
- JSON schema fragments per feature (type, nullable → `"null"` in type, length → `maxLength`, values → `enum`/`const`, pattern) stated explicitly.
- YAML scalar fidelity: `007`, `01`, `0x1F`, `1e3`, `Y`, `N`, `yes`, `no` in `values:`/`examples:` survive as text (pin per case; project memory: exact scalar handlers exist for some, not all).
- Negative: missing `datasets`, unknown rule `type`, duplicate column ids, invalid regex, non-list `columns`, unreadable path, range rule without bounds (`R/evaluateRules.R:357/370`), condition shape errors (`:508/516/554/562/636/891`) — each with the `cli` message/class as written.

### 5.4 TYPE — declared types and typing at read, ≥ 15 REQ / ≥ 30 TC (WP2)

Sources: `R/importConversion.R`, `R/DTAColumnSpecStructure-class.R:73`, `R/DTAColumnSpecStructureSAS-class.R`, `R/DTAColumnSpec-class.R:196`, `R/evaluateRules.R:51-99`.

- Accepted `type` strings (pin exhaustively: "SAS Char", "SAS Num", "SAS Int", "date", "datetime"/"POSIXct", "boolean") and SAS formats (DATE9, DATE11, DDMMYY10, YYMMDD10, TIME8, TIME12.2, DATETIME19/21, numeric `8.2`) → `as_r_type()` and `get_arrow_type()` mapping tables, one TC row each.
- `"007"` in a Char column stays `"007"` after read (README headline); in a Num column parses to 7.
- Strict numeric matrix (13 inputs from `tests/testthat/helper-validation-corpus.R:312-347`, copied not sourced): expected value/missing/unconvertible; the three classes partition; `"NaN"` unconvertible → pin + LIM.
- Int narrowing: outside ±(2^31−1) → import error; fractional in a declared Int; batch-dependent narrowing → DEV (evidence `test-streaming-parity.R:340-346`).
- BEST12. → DEV-001. Reader schema widens to string only (a bad cell yields one import error, never an unreadable file).
- Typed-in-R table vs the same values from a file reach the same verdict (factor, Date, POSIXct, logical, integer64 when bit64 present).

### 5.5 VAL — validation engine and accessors, ≥ 25 REQ / ≥ 40 TC (WP3)

Sources: `R/columnSpecChecks.R` (keywords; `dta_columnspec_check_kinds()` :503-512; error frame :70-79), `R/DTADataSetTabular-class.R:1240-1649` (`check`), `:969-1024` (`validation_status`), `R/validationReporting.R:29/190/529`, `R/validationFunctions.R:39/41`.

- Keywords, one TC each (two-row table, one violating row): `required` (**one error per row when a column is absent** — pin + note), `additionalProperties`, `type`, `maxLength` (characters not bytes: `"äöüßé"` passes at 5), `enum`, `const` (enum precedence when both), `pattern` (PCRE), nullable NA handling; error-frame columns `row column keyword message columnspec data`; `columnspec_checks` labels `presence extra format length values pattern` × statuses `passed failed not_applicable not_checked`.
- Verdict algebra: `ok` ⇔ three axes valid; counts per axis; `import_valid` NA for pre-0.13 artifacts (pin); status ∈ `validated skipped not_validated unspecified`; zero-column spec → `unspecified`, `ok = NA` (warning).
- `check()` arguments: `tables`/`tab`; `force`; revalidation skipped when table hash and specs hash unchanged and re-run when either changes; `persist`/`artifact_dir` (artifact readable via `validation_errors(source = "artifact")`; missing artifact errors `:1066/1072`); `validation_run` propagation; `max_errors` retains a prefix but **counts are never truncated**; `fail_fast` → partial, never skipped; `on_missing_column = "stop"` → structural-only; `use_threads` FALSE/TRUE identical; consumed `RecordBatchReader` → error `:1369`; `quiet`.
- Accessors: exact column sets of `validation_status()`, `results()` (`n_targets n_validated n_valid n_invalid n_skipped n_not_validated`), `messages()` (`source ∈ columnspec rule import`, sequential ids, `severity`), `inspect()` (`R/validationReporting.R:533/553` errors), `validation_errors()` (`result_version = 2L`; attributes `n_rows_scanned partial_scan structural_only`), `clear_validation()`, `get_table()` errors (`:343/347/360/362/368`), `results(datasets=)` errors (`:104/117/124/129/134`).
- `validate_table()` and `validate_rules()` standalone; `validate_rules()` aborts with the bullet list (`:1586`); DTA-level `check()` with mixed tabular + file datasets.

### 5.6 RULE — rule types and operators, ≥ 30 REQ / ≥ 60 TC (WP4)

Sources: `R/evaluateRules.R` (operator dispatch `:611-653`, range `:310-326`, unique `:450-477`, col_condition `:1025-1072`, group `:1181-1447`, constraint dispatch `:1333-1343`), `R/DTARule*-class.R`.

- Operators, one TC each with NA and type edge rows: `equals/equal`, `not_equals/not_equal`, `in`, `not_in`, `greater`, `less`, `greater_equal`, `less_equal`, `min`+`max` (one inclusive band), `range` (length 2), `pattern` (PCRE; NA/"" → FALSE), `empty` (TRUE/FALSE; "" empty only for char/factor); numeric coercion of bounds only against numeric columns; Date/POSIXct compared as-is; unsupported operator → `:636`.
- `col_condition`: IF conjunction; IF-NA split (missing out of scope; unconvertible in scope and fails); multiple operators ANDed; empty IF = all rows; message `:1070`.
- `col_range`: inclusive; NA ignored; unconvertible fails **and** appears on the import axis; exactly one target (`:357`); `range` or `min`/`max` (`:370`); missing column → class `dta_rule_not_applicable` (`:395`).
- `col_unique`: composite keys; repeated NAs are duplicates; case-sensitive; missing columns `:456`.
- `group_condition`: `group_by` one or several columns; `mutually_exclusive` (left/right, scopes any/all) and `requires` (if/then, scopes any/all); **unknown constraint type silently yields no violation** (`:1341`) → pin + DEV; unknown condition name `:1300`; missing grouping column `:1197`; groups ordered in C locale; `details` (group, constraint_id, message, rows, rows_truncated).
- Duplicate rule ids not rejected (pin + LIM); no `enabled` switch (document); `rule_preview()`; `DTARuleFactory()` YAML→constructor per type; undeclared column read by a rule keyed textually (XKEY).

### 5.7 FILE — handlers and readers, ≥ 20 REQ / ≥ 35 TC (WP5)

Sources: `R/DTAFile-class.R` (`open_file :407`, `read_file :599`, `matches_filename :689`, `max/min_number_of_files :697/737`), `R/DTAFileTabular-class.R:602/1457`, `R/DTAFileCSV/TSV/Delim/Any-class.R`, `R/streamingScale.R` (`dta_compression_extensions()`), `R/DTADataSetTabular-class.R:457` (`write_table_to_file`).

- Handler properties applied at read: delimiter (CSV `,`, TSV `\t`, Delim `;`/`|`), `quote`, `header` TRUE/FALSE, `na_strings` default `c("", "NA")` and custom, `encoding` (UTF-8; latin1 transcoded; BOM), `filename`/`pattern` semantics (pin regex vs literal), `min/max_number_of_files` on `DTADataSetFile`.
- Compression ⚠: verify the real `dta_compression_extensions()` list (one report says gz/bz2/xz/zip; project memory says only gzip works and `.zip` fails inside Arrow) — one end-to-end TC per listed extension; a failing extension becomes LIM/DEV.
- Edge files (pin each): empty; header-only; blank lines skipped (LIM-001); CRLF; leading/trailing empty fields; mismatched field count; duplicate header names; header styles plain/quoted/padded; path with spaces and non-ASCII; 2,000 columns; a 1 MB field; embedded newlines in quoted fields.
- `load_file(stream = "never"|"auto"|"always")` holding type; `DTAtools.stream_threshold` drives `auto`; `handler_index` routing; `open_file()` returns schema without rows; `read_file_execution()`/`open_file_execution()`.
- `write_table_to_file()`: signature (`arrange_by`, `sep`, `na`, `quote`, `compression = c("none","gzip")`, `get_md5sum`, `write_md5sum_to_file`, `overwrite`); sidecar `<filename>.md5` content `md5sum: <hex>` / `Number of Columns` / `Number of Rows` equals `tools::md5sum()` and dims; overwrite abort; written file re-reads to the same verdict.
- `DTAFileAny` multi-part extensions (`tar.gz`, `nii.gz`).

### 5.8 STREAM — streaming and parity, ≥ 20 REQ / ≥ 40 TC (WP6)

Sources: `R/streamingValidation.R:2751-2893`, `:697`, `:2603`, `R/arrowCompute.R:190`, `tests/testthat/test-streaming-parity.R`.

- `validate_file_stream()` every argument: `delim quote has_header batch_rows` (3/64/131072 identical verdicts), `max_errors` (spill; `collect_full_errors()` axes `columnspec import import_typing`; `truncated`/`spilled_rows` attributes; warning when spill files are gone `:731`), `fail_fast`, `on_missing_column`, `use_threads`, `verbose`, `benchmark` → 13 metrics, `metrics_version = 1L`; missing file `:2764`.
- Batch-boundary correctness: defects at rows `batch_rows`, `batch_rows + 1`, last row; duplicate keys split across batches → exact counts.
- Randomised parity campaign (`helper-parity.R`, seeds from `qa_parity_seeds()`; file and R-typed generators; `expect_identical` on status, sorted errors, `n_import_errors`; coverage assertion for all three axes and both compressions; boundary cases 0 and 1 rows).
- `DTAtools.use_arrow_compute` TRUE vs FALSE identical verdicts; `DTAtools.arrow_min_rows` threshold crossing identical.
- `cache_as_parquet()` round trip equals CSV verdict; Arrow pool metrics finite for every streamed run.

### 5.9 META — metadata, ≥ 12 REQ / ≥ 18 TC (WP7)

Sources: `R/DTAMetaData-class.R:66-110, 258-287`, `R/DTAMetaData-helpers.R:12-17, 101-344`.

- Properties/validators: `title`/`version` non-empty when set; `version_history` records need `version`, `date` (Date), non-empty `changes`; `transmission` first ≤ last when both Dates; `template` needs `id` + `version`; `import_issues` read-only.
- Date parsing three cases: bare ISO → Date; ISO + trailing text → Date + import issue `trailing_residue`; phrase → kept verbatim, no issue; `.date_to_iso()` round trip through YAML.
- Accessors' return shapes: `get_authorized_for_corrections()`, `get_receiver_reviewers(name_only)`, `get_transmission_dates()` (`first_transfer`, `last_transfer`), `get_version_history_df()` (`version date changes`), `validate_transmission_dates()` (`is_valid`, `messages`), `metadata_import_errors()` (`row column raw declared_type reason`); `messages(<DTAMetaData>)` surfaces import issues.
- Accepted YAML keys (title, version, date, header, version_history[], receiver/supplier{affiliation, contacts[name role email department phone signature reviewer backup]}, transmission{type frequency notification test_upload blinded_transfer date_first_transfer date_last_transfer}, error_handling, authorized_for_corrections, template{id version}) each round-tripped.

### 5.10 TMPL — templates, inheritance, vocabulary, ≥ 25 REQ / ≥ 40 TC (WP8)

Sources: `R/validateTemplate.R:1255-1311` (`validate_template(path, strict = FALSE, kinds = NULL)` → df `file kind id version severity code message`), check codes at `:465-1167`, `R/createTemplateRepo.R:201-265`, `inst/shiny/dta_app/R/{template_inherit,vocabulary,template_sources}.R`, `inst/extdata/templates`, `inst/extdata/template-repo-skeleton`.

- One TC per `validate_template()` code, each with a fixture directory under `fixtures/templates/` that triggers exactly that code (31 codes): `parse_failed kind_unknown id_missing version_missing version_unquoted version_unparseable target_machine_owned target_invalid party_slot_invalid vocab_slot_invalid vocab_slot_unresolved dataset_template_unresolved patch_incoherent values_and_values_from values_from_pattern values_from_invalid values_from_unresolved values_from_terms_invalid dataset_missing extends_unresolved extends_cycle sealed_violation sealed_path_unknown instantiate_failed vocabulary_invalid vocabulary_unresolved vocabulary_extends_failed duplicate_id_version no_templates template_in_subdirectory`; severities as coded; `strict` and `kinds` arguments.
- Bundled templates all validate with zero errors; `create_template_repo(path, examples, ci = github|bitbucket|jenkins|FALSE, overwrite)` creates exactly the documented files, returns the normalised path, refuses to overwrite.
- Inheritance: `extends:` resolution across the four file kinds (`*.dta-template.yaml`, `*.dta-dataset-template.yaml`, `*.dta-party.yaml`, `*.dta-vocabulary.yaml`), four value states (absent / explicit null / empty collection / value), `sealed` paths, depth/cycle limit, latest-version by `numeric_version`, `version_history` rebase.
- Vocabulary: `values_from` include/exclude expansion into `values:` for every consumer kind; unknown code → error; cyclic `extends` → error.
- `dta_template_placeholders()` ⚠ (exported per NAMESPACE, reported at `R/validateTemplate.R:1255` by one report and "app-side only" by another — verify) and `export_with_template()` placeholder set.
- Env vars ⚠ (verify by `grep -n "Sys.getenv(" R/ inst/shiny`; expected `DTATOOLS_TEMPLATE_SOURCES`, `_CACHE_DIR`, `_GIT_TOKEN` (never logged/persisted), `_GIT_USER`, `_GIT_AUTH`, `_INCLUDE_BUILTIN`, `_REFRESH_SECONDS`): behaviour with `dir:` and `pkg:` sources set via `withr::local_envvar`; `git:` documented as not executed offline.

### 5.11 EXPORT — documents and data export, ≥ 20 REQ / ≥ 30 TC (WP9)

Sources: `R/exportDocuments.R:60-154` (`write_dta(x, file, format = NULL, overwrite = FALSE, include_signatures = TRUE, signature_list = NULL, quiet = FALSE, include_yaml = FALSE, yaml_text = NULL, template = NULL, template_variables = NULL)`), `:571` (`dta_pdf_backend`: LibreOffice → TinyTeX → pandoc engines), `:791`, `:867`, `:1023`, `R/exportFunctions.R:23/229`, `R/exportTemplateDocx.R:80`, `R/importConversion.R:73`.

- `write_dta()` docx: the ten sections (title, approvals when `include_signatures`, document info, version history, supplier, receiver, process info, datasets with file/column/rule tables, embedded YAML when `include_yaml`, footer) verified by `qa_docx_text()`/`qa_docx_tables()` content; `md` format; `pdf` when a backend exists (else not executed); overwrite abort; `template`/`template_variables` path.
- `write_dataset_metadata()`, `write_file_specification()`, `export_specs_table()` (docx; DEV per `test-exportFunctions.R:127`), `export_column_value_table()`, `export_with_template()` placeholder patching, `columns_specs_from_word()` round trip (specs → docx → specs equal).
- Atomicity (pin): most writers write in place; PDF conversion writes to a temp file then renames; MD5 computed after write → LIM if a failed write can leave a partial file.
- Output determinism modulo the timestamp/date lines.

### 5.12 REPORT — HTML validation report, ≥ 8 REQ / ≥ 12 TC (WP10)

Sources: `R/exportValidationReport.R:34`, `inst/report/report.css`, `inst/report/report.js`.

- Self-contained (no `http(s)://` asset references; css/js inlined); counts in the HTML equal `results()`; every `messages()` row present; inspect-panel payload present; generation timestamp line; report for a DTA with mixed dataset types and for an unvalidated dataset; overwrite guard; title default.

### 5.13 APP — Shiny application, ≥ 15 REQ / ≥ 25 TC (WP11; scope per §12)

Sources: `R/run_dta_app.R:26` (`run_dta_app(launch.browser = TRUE, port = NULL, ...)`, requires shiny/bslib/DT, sets `shiny.maxRequestSize` 1 GB), `inst/shiny/dta_app/app.R:397` (`server`), `tests/testthat/helper-shinyapp.R`, and the 29 real `tests/testthat/test-shinyapp-*.R` files (⚠ one report invented file names; use the real listing: click-guard, create-empty, dataset-add-remove, dataset-meta, dataset-template, dta-ops, edit-mode, export, file-dataset(-server), file-handlers, harness, import-axis, modal-outputs, server, template-create/index/inherit/latest/parties/rebase/sources/ui, template, theme, ui-components, utils, versioning, vocabulary).

- `run_dta_app()` resolves the bundled app; dependency message when shiny/bslib/DT absent.
- Server logic via `shiny::testServer()` (all skipped with reason when shiny/bslib/DT/shinyjs/shinyAce absent): load YAML → add dataset → bind data → check → status mapping (`pass/fail/pending/nodata`) → export report/docx; template create / inherit / new version / edit mode / rebase; party profiles; vocabulary; dataset add/remove; import-axis display; click guard.
- Browser tier (tag `browser`): runs only when `shinytest2` + `chromote` are installed **and** a Chrome/Chromium/Edge binary is found (`CHROMOTE_CHROME`, `DTATOOLS_CHROME`, or chromote's default search); otherwise `skip("browser absent")` → "not executed (browser absent)". Scenarios: open app, upload bundled YAML, upload `clinical_data.csv`, run check, download HTML report, screenshot kept as artifact.

### 5.14 ROBUST — error handling, integrity, isolation, ≥ 15 REQ / ≥ 30 TC (WP12)

- Wrong argument types on every exported function with arguments (≥ 1 TC per function): `cli` error, no side effects.
- Non-existent, unreadable, corrupted gzip, truncated mid-row, invalid UTF-8, 10k-column, 1 MB-field, zero-byte inputs → pinned behaviour, never a silent wrong verdict.
- Path traversal (`../`) in `create_template_repo()`/`write_*()`; read-only output dir → error and no partial file; nothing written outside output dir and `tempdir()` (directory snapshots); no temp-file accumulation after 20 checks.
- Determinism: two runs → identical `qa_messages_norm()`; exports identical modulo timestamp.
- Isolation: `options()`, locale, env vars, RNG state unchanged after every exported call.
- Locale independence (`subprocess`): verdicts identical under `LC_ALL=C` and the system locale; `LC_TIME` independence of date parsing.

### 5.15 PERF (PQ) — `test-PQ-*.R`, ≥ 20 REQ / ≥ 30 TC (WP14)

- End-to-end on bundled data with hand-derived oracles (`fixtures/oracle/data-*.yaml`; every count traced to cells; starting points from `tests/testthat/test-clinical-error-fixtures.R`: `error_columnspec` → import 1, keywords const/enum/maxLength/required/type, columns BMI/GENDER/STUDYID/VISIT; `error_rules` → 7 rule ids; `error_all` → import 4, six rule ids (no range); `error_import` → import 4 at rows 5/6/9/18 raw `>190 ninety unknown >300`, columnspec 5; `clinical_data.csv` clean; `gf_data_small_smrnaseq.tsv` ⚠ column count reported as 6 and as 33 — verify): check → results/messages/inspect → `write_validation_report()` (parsed counts equal) → `write_dta()` docx → `write_table_to_file()` gz + MD5; multiple-files and file-dataset YAML flows.
- Correctness at scale: for each row count in `qa_tier_rows()` generate with `qa_write_large_csv()`; validate streaming (all) and in-memory (all; the 1e7 eager path guarded by `qa_skip_unless_memory_gb(16)`); assert `qa_counts() == qa_oracle()` exactly; gz and plain; `qa_perf()` rows for every run.
- Scaling (hard, wide): elapsed(10n)/elapsed(n) ≤ 20; streaming `r_peak_mb(10n)/r_peak_mb(n)` ≤ 3; `arrow_pool_peak_mb` ≤ 8 × file MB; baseline deltas reported; `perf_floor` applied when set.
- Repeatability: 5 repeats identical; threads 1/2/all identical; `batch_rows` 3/64/default identical; `use_arrow_compute` on/off identical; locale C vs system (`subprocess`).
- Stability: 50 consecutive checks of the 1e5 file (full; 5 in quick/standard) — RSS mean of last 5 ≤ 1.5 × mean of first 5 (skip without `ps`), no temp-file accumulation, Arrow pool peak ≤ 1.5 × first-run peak.
- App launch smoke (`subprocess`): background `Rscript` running `shiny::runApp(system.file("shiny/dta_app"), port)`, HTTP 200 on `/` within 60 s (`base::url()`), page contains the app title, killed with `tools::pskill()`; skip without shiny.
- Developer evidence: unit suite executed when available (`include_unit_tests`).

---

## 6. Work packages

### 6.1 Standard agent brief (paste into every WP prompt)

> Work ONLY in `C:\Users\Tom\workspace\DTAtoolsR\.claude\worktrees\pharma-validation-tests-cdc777` (never the main checkout). Edit ONLY the files listed under "Files owned". Read `thoughts/shared/plans/PLAN-qualification-suite.md` §4 (contracts) and your §5 checklist first, then the cited source lines. Rules: `cli::cli_abort()`/`cli_warn()` for conditions; namespaced calls; every `test_that()` title matches the §4.2 grammar; every requirement has explicit expected values in its text or a fixture; no `expect_snapshot*`, no `expect_true(TRUE)`, no bare `expect_error()` (use `class =` or a package `cli` string); never assert translated base-R text; all writes via `qa_tempdir()`/`qa_artifact_dir()`; skip via `qa_requires()`/`qa_skip_unless_scale()` only; use `qa_step()` for every expected/actual comparison a reviewer should see; every file starts with a comment naming the area and its REQ range. Run `Rscript --no-init-file .github/scripts/style.R` on your files and the verification command below before reporting. **Report deviations from the spec instead of approximating**, and report every surprising behaviour you pinned (candidate DEV/LIM). Never add AI attribution anywhere.

### 6.2 Packages

| WP | Files owned | Depends | Deliverables (minimums) | Verification |
| --- | --- | --- | --- | --- |
| **WP0 Core** (first; main thread writes contracts, one sonnet agent implements, main thread reviews) | `R/qualification.R`, `R/qualification-report.R`, `inst/qualification/tests/{setup-qualification.R,helper-qa.R,helper-generators.R,helper-parse.R}`, `requirements/{_index.yaml,_api.yaml,REQ-CORE.yaml}`, `deviations.yaml` (seeded), `tests/test-OQ-CORE-*.R`, `tests/testthat/test-qualification.R`, `.github/workflows/qualification.yml`, `.github/scripts/sync_qualification_manifest.R` + `r-style.yaml` step, `DESCRIPTION`, `CHANGELOG.md`, `README.md` section, `inst/qualification/README.md` (stub) | — | all §4 contracts; roxygen for the two exports with runnable examples; PR guard: grammar (6 cases), static mode, IQ-less run producing a bundle whose `results.json` hash verifies, `qa_step` failure text captured, file-level error → `file_error`; workflow manual + weekly | `pkgload::load_all(); run_qualification(tempfile("q"), scale = "quick", formats = "md")` writes every §4.6 file; `devtools::test(filter = "qualification")` green; style, roxygen 8.1.0, `rcmdcheck(args = "--no-manual")` clean; example < 20 s |
| WP1 SPEC | `REQ-SPEC.yaml`, `tests/test-OQ-SPEC-*.R`, `fixtures/oracle/spec-*.yaml`, `fixtures/yaml/*` | WP0 | ≥ 20 REQ / ≥ 35 TC | `VERIFY SPEC` (below) |
| WP2 TYPE | `REQ-TYPE.yaml`, `tests/test-OQ-TYPE-*.R` | WP0 | ≥ 15 / ≥ 30; DEV-001 + Int-narrowing DEV bound | `VERIFY TYPE` |
| WP3 VAL | `REQ-VAL.yaml`, `tests/test-OQ-VAL-*.R` | WP0 | ≥ 25 / ≥ 40 | `VERIFY VAL` |
| WP4 RULE | `REQ-RULE.yaml`, `tests/test-OQ-RULE-*.R` | WP0 | ≥ 30 / ≥ 60; unknown-constraint-type finding registered | `VERIFY RULE` |
| WP5 FILE | `REQ-FILE.yaml`, `tests/test-OQ-FILE-*.R`, `fixtures/files/*` | WP0 | ≥ 20 / ≥ 35; compression list verified end-to-end | `VERIFY FILE` |
| WP6 STREAM | `REQ-STREAM.yaml`, `tests/test-OQ-STREAM-*.R`, `tests/helper-parity.R` | WP0 | ≥ 20 / ≥ 40; parity campaign parameterised by tier | `VERIFY STREAM` |
| WP7 META | `REQ-META.yaml`, `tests/test-OQ-META-*.R` | WP0 | ≥ 12 / ≥ 18 | `VERIFY META` |
| WP8 TMPL | `REQ-TMPL.yaml`, `tests/test-OQ-TMPL-*.R`, `fixtures/templates/*` | WP0 | ≥ 25 / ≥ 40; one fixture per `validate_template()` code | `VERIFY TMPL` |
| WP9 EXPORT | `REQ-EXPORT.yaml`, `tests/test-OQ-EXPORT-*.R`, `fixtures/docx/*` | WP0 | ≥ 20 / ≥ 30 | `VERIFY EXPORT` |
| WP10 REPORT | `REQ-REPORT.yaml`, `tests/test-OQ-REPORT-*.R`, `qa_html_report()` body in `helper-parse.R` (WP0 leaves a documented stub) | WP0 | ≥ 8 / ≥ 12 | `VERIFY REPORT` |
| WP11 APP | `REQ-APP.yaml`, `tests/test-OQ-APP-*.R`, `tests/helper-app.R` | WP0 | ≥ 15 / ≥ 25 (+ browser scenarios per §12) | `VERIFY APP` |
| WP12 ROBUST | `REQ-ROBUST.yaml`, `tests/test-OQ-ROBUST-*.R` | WP0 | ≥ 15 / ≥ 30 | `VERIFY ROBUST` |
| WP13 IQ | `REQ-ENV.yaml`, `tests/test-IQ-ENV.R`, `baseline/inst-files.sha256` (via the WP0 script) | WP0 | ≥ 18 / ≥ 18 | `stages = "IQ"` |
| WP14 PQ | `REQ-PERF.yaml`, `tests/test-PQ-*.R`, `fixtures/oracle/data-*.yaml`, `baseline/reference-performance.json` | WP0 (+ reads `helper-parity.R` from WP6) | ≥ 20 / ≥ 30; oracle derivations at cell level; full tier completes on the dev machine with runtimes recorded | `stages = "PQ", scale = "quick"`, then `"standard"`, then once `"full"` |
| WP15 DOCS | `inst/qualification/README.md` (full), `docs/validation-plan.md`, `docs/glossary.md`, `docs/references.md`, `risk-assessment.yaml`, vignette section | WP0 | Validation Plan (scope, GAMP category rationale, roles, stages, acceptance criteria, deviation handling, re-qualification triggers: package version, R minor, arrow major, OS/Connect change), risk assessment (area × severity × probability × detectability → depth, mapped to TC counts), README (run on a target incl. offline Suggests install, review, sign, verify hashes, tiers/resources) | renders with `rmarkdown::render()`; reviewed by main thread against §2 |
| WP16 REVIEW (full-strength) | read-only + fix list | all | run all tiers locally (Windows) and `quick` on CI (Linux/macOS); **mutation negative controls**: 5 seeded source mutations (e.g. `>=`→`>` in `rule_check_range`, drop `enum` check, swap `n_import_errors` for 0, break `maxLength` unicode counting, disable spill) must each make ≥ 1 TC fail with the right TC named; parity ≥ 1000 seeds; every REQ text checked for falsifiability against the code it cites; 10 random TCs traced to code; measured runtimes per tier and tarball size (< 10 MB) recorded in README; style/roxygen/rcmdcheck/pre-commit; no AI attribution; no snapshots; no existence-only assertions | fix-first findings resolved on the main thread |

`VERIFY <AREA>` = `Rscript --no-init-file -e "pkgload::load_all(quiet = TRUE); r <- run_qualification(tempfile('q'), stages = 'OQ', filter = '<AREA>', scale = 'quick', formats = 'md'); print(summary(r)); stopifnot(!grepl('^FAIL', r$verdict))"` — plus a manual read of `report/qualification-report.md` for the area.

Execution order: WP0 → {WP1…WP15 in parallel; disjoint files; `helper-qa.R`/`helper-generators.R` are read-only for them — needed additions are reported to the main thread} → WP16 → PR to `dev`. Numbering: each area owns its `nnn` space from 001.

---

## 7. Verification (end-to-end)

1. Dev loop in the worktree (`.Renviron` holds `RENV_CONFIG_AUTOLOADER_ENABLED=FALSE`): `Rscript --no-init-file -e "pkgload::load_all(); run_qualification(tempfile('q'), scale = 'quick')"`; read `SUMMARY.txt`, the Markdown report, and verify `SHA256SUMS` with `sha256sum -c` in Git Bash.
2. Installed-package run (what the target does): `R CMD build .` → `R CMD INSTALL --install-tests DTAtools_0.25.0.tar.gz -l <templib>` → `Rscript -e ".libPaths('<templib>'); DTAtools::run_qualification('<dir>', scale = 'standard')"` → verdict PASS, unit suite executed, HTML/DOCX/PDF present on this machine, `md5_check` TRUE.
3. Full tier once on the dev machine (`scale = "full"`, default); runtimes and resource peaks recorded into `reference-performance.json`, README and the CHANGELOG entry.
4. `devtools::test()` (whole suite incl. `test-qualification.R`), `Rscript .github/scripts/style.R`, `Rscript -e "roxygen2::roxygenise()"` (8.1.0 per `Config/roxygen2/version`), `rcmdcheck::rcmdcheck(args = "--no-manual")` clean, `pre-commit run --all-files`.
5. CI: dispatch `qualification.yml`; bundle artifact present on all three OS legs; `r-style` fails when `inst/` changes without a manifest resync (break once deliberately, then resync).
6. Negative controls (WP16): each seeded mutation makes the suite FAIL naming the right TC.

---

## 8. Risks (pre-mortem)

| Risk | Sev | Mitigation |
| --- | --- | --- |
| Tests that cannot fail (the 0.13.0 audit found 21 such) | HIGH | no snapshots; explicit expected values; closed-form oracle independent of the engine; mutation negative controls; reviewer rejects existence-only assertions |
| Writes into the read-only library (testthat `setwd()`s into the test dir on a target) | HIGH | `qa_tempdir()`/`qa_artifact_dir()` for every path; M8 bans snapshots; WP16 runs from a read-only temp library |
| Agents read the stale main checkout | HIGH | absolute worktree path in every brief; WP16 checks that every cited file:line resolves in the worktree |
| `pkgload::load_all()` hides a broken `@export` / S7 identity differs installed vs dev | MEDIUM | verification step 2 installs from the tarball; IQ compares exports with `_api.yaml`; class checks via `inherits()` strings |
| Full tier resources (1e7 rows: ~25 GB disk incl. gz copy; eager path ≥ 16 GB RAM) | MEDIUM | `qa_skip_unless_memory_gb()`, IQ disk check, README states requirements; `standard`/`quick` available |
| Hard throughput thresholds on slow servers | MEDIUM | correctness/scaling hard; throughput informational unless `perf_floor` |
| pandoc / PDF backend / Chrome absent on the target | LOW | Markdown + JSON/CSV always; formats and browser tests logged as not produced / not executed |
| Helper discovery / nested `test_dir()` inside the PR guard on CI | MEDIUM | flat test dir (verified); guard falls back to a `qa_subprocess`-style `Rscript` run if the in-process call misbehaves |
| Exploration reports disagree on details (⚠ items) | MEDIUM | each WP verifies from source and records file:line in `notes` |
| Manifest chore blocks unrelated PRs | LOW | same pattern as the app manifest; `--check` prints the one-line fix |
| Line-ending drift changes `inst/` hashes on some build hosts | LOW | `.gitattributes` forces LF; IQ manifest verified on Linux + Windows CI |
| Tarball growth | LOW | fixtures tiny; data generated; WP16 measures (< 10 MB) |
| Rate limits / agent cut-offs | LOW | WPs are file-scoped and restartable |

---

## 9. Out of scope

Changing data-validation behaviour (findings are registered as DEV/LIM and fixed in separate PRs); version bump / release; Bioconductor items; electronic-signature implementation (the report is signed outside the tool); `valtools`/`riskmetric` dependencies; `docs/index.html` landing page.

## 10. First steps after approval

1. Copy this plan to `thoughts/shared/plans/PLAN-qualification-suite.md` (house convention) — the agent briefs cite it.
2. WP0 on the main thread + one sonnet implementer; review; then dispatch WP1–WP15 in parallel with the §6.1 brief; then WP16.

## 11. Facts the area agents must not trust from summaries (verify from source)

Env-var names and their file:lines; `dta_template_placeholders()` location; `DTAtools.use_arrow_compute` default and the full option list; `dta_compression_extensions()` list; `gf_data_small_smrnaseq.tsv` column count; the exact wording of the four unit-test "KNOWN DEFECT" comments; whether `n_rule_errors` counts rules or rows.

## 12. App scope (user decision, taken)

**Server logic via `shiny::testServer()` + PQ launch smoke always; browser scenarios are an optional tier.**

Chrome and the `shinytest2`/`chromote` packages are **never** package dependencies. They are not added to `DESCRIPTION` at all: `qa_requires("shinytest2", "chromote")` skips the browser tier when they are absent, exactly as the existing `tests/testthat/test-exportDocuments.R` and `test-shinyapp-export.R` already do for Chrome via `DTATOOLS_CHROME`. They matter only on the system where a qualification run happens, and installing them there is optional.

Browser scenarios run when both packages are installed **and** a Chrome/Chromium/Edge binary is found (`CHROMOTE_CHROME`, `DTATOOLS_CHROME`, or chromote's default search). Otherwise every browser TC is recorded as `skip("browser absent")`, listed in report §10 as "not executed (browser absent)", and its requirements are marked `not_verified`. The verdict then reads `PASS WITH NOT-EXECUTED TESTS`, never `FAIL`. IQ records the browser availability either way, so the report always states whether that tier ran.
