# d2-meta-ci findings: package metadata, build tooling and CI

Reviewer key: d2-meta-ci. Tree: `$SNAP` = scratchpad/snapshot (pinned tree 68408c44...). Read-only; R was not run.

Slice (relative to `$SNAP`): DESCRIPTION, NAMESPACE, manifest.json, app.R, [assistant-tool].md, .github/** (9 workflows, 11 scripts,
agents/DTAtoolsKeeper.agent.md, [assistant-tool]-instructions.md, .github/.gitignore), .pre-commit-config.yaml, .Rbuildignore,
.gitignore, .gitattributes, benchmarks/, inst/shiny/dta_app/VERSION (inst/ci/ does not exist; there are no PR/issue templates).
Also opened to check claims: .Rprofile, .renvignore, .graphifyignore, .vscode/settings.json, .[assistant-tool]/agents + commands,
docs/index.html (version sites), CHANGELOG.md (headings/link defs), inst/qualification/requirements/_index.yaml,
R/qualification.R (verdict + hashing), R/run_dta_app.R, R/zzz.R, R/columnSpecChecks.R:1, tests/testthat/helper-source-tree.R.

Method notes: GitHub action SHAs were checked against public tag data (`gh api repos/<repo>/tags`): all 11 pinned SHAs exist and
equal the tag in their comment (checkout v5 = v5.1.0, checkout v4 = v4.4.0, r-lib/actions v2.14.0, upload-artifact v4.6.2,
setup-python v5.6.0, cache v4.3.0, git-auto-commit v5.2.0, create-pull-request v7.0.11, configure-pages v5.0.0,
upload-pages-artifact v3.0.1, deploy-pages v4.0.5), so there is no SHA/tag mismatch finding.

Verified OK (no finding): Collate lists all 70 R/*.R files exactly once (no missing, extra or duplicate); all 128 NAMESPACE `export()`
entries have a roxygen `@export` at a definition site (generics sit inside `if (!exists(...))` guards under the tag; the one
guarded duplicate in R/DTARule-class.R:79 has no tag but R/00_helpers.R:552 has it); `.onLoad` calls `S7::methods_register()`;
every `pkg::` used in R/ is declared (Imports, or Suggests behind a requireNamespace guard: shiny via run_dta_app's loop,
rmarkdown/tinytex/ps/R6 guarded, pkgload/chromote/withr wrapped in `qual_safe`); version 0.28.0 agrees in DESCRIPTION, VERSION,
_index.yaml, docs/index.html badge (l.540) and footer (l.1922), CHANGELOG heading (l.161) and `[Unreleased]` link (l.5493);
`Config/roxygen2/version: 8.1.0` equals the pin in r-style.yaml:67; the app.R md5 in manifest.json equals the file on disk;
the S7 pin `cran::S7@0.2.2` is present in every `setup-r-dependencies` step (R-CMD-check, qualification, r-style,
test-coverage x2, release; bump-version/pages/pre-commit install no R packages); no untrusted `${{ github.event.* }}` value is
interpolated into a `run:` block (ci-tier passes values through `env:`; bump-version passes `inputs.version` through `env:`).

---

### F-d2-meta-ci-01  [major] [design]
- Where: DESCRIPTION:45; .github/workflows/R-CMD-check.yaml:135, qualification.yml:94, r-style.yaml:67, test-coverage.yaml:79,166, release.yml:33; R/DTADataSetFile-class.R:89 (and 9 more `.parent =` sites)
- What: CI pins S7 to 0.2.2 in all six dependency steps without saying why, while DESCRIPTION only declares a lower bound, so every install outside those workflows takes the newest S7, which the 10 `new_object(.parent = ...)` constructors do not support.
- Evidence: `DESCRIPTION:45  S7 (>= 0.2.2)`; `R-CMD-check.yaml:135  extra-packages: any::rcmdcheck,any::remotes,any::devtools,cran::S7@0.2.2`; `R/DTAFileXPT-class.R:86  .parent = DTAFileSAS7BDAT(`; the same `.parent =` call exists in DTADataSetFile, DTAColumnSpecStructureSAS/R, DTAFileAny, DTAFileTabular, DTAFileSAS7BDAT, DTADataSetTabular, DTARuleColRange, DTARuleColUnique. No comment next to any of the six pins, in README.md or in DESCRIPTION records the reason. The CI assets that `create_template_repo()` writes for downstream repositories go the other way: `inst/extdata/ci/bitbucket-pipelines.yml:34-37` and `Jenkinsfile:51-54` set the `latest` package mirror precisely so that `S7 >= 0.2.2` resolves to the newest S7.
- Failure scenario: `remotes::install_github("Boehringer-Ingelheim/DTAtoolsR")` (README.md:66, vignettes/DTAtools.Rmd:63) on a machine whose newest S7 renamed/removed `new_object(.parent=)` installs, loads, and then fails in the first constructor call; CI never shows it because the pin hides it. The Connect deploy only survives because manifest.json freezes S7 0.2.2 (manifest.json:205 `"Version": "0.2.2"`; renv.lock also pins 0.2.2); regenerating the manifest from a current library silently ships the newer S7.
- Confidence: inferred (S7 1.0.0 is not in the snapshot; the incompatibility is taken from the brief's "S7 pitfalls: new_object(.parent=)" and from the existence of the unexplained pin; what I did verify: the 10 call sites, the six pins, the lower-bound-only declaration)
- Suggested direction: adapt the 10 constructors to the current `new_object()` signature and drop the pin, or state the incompatibility (comment at each pin, README install note, an upper bound or install-time check) until then.

### F-d2-meta-ci-02  [minor] [inconsistency]
- Where: .Rbuildignore:12-15,17-18
- What: six patterns have no leading `^` (five of them no trailing `$` either), so they match anywhere in a path; three of them duplicate anchored entries that already exist.
- Evidence: `.Rbuildignore:12 .github` / `:13 .vscode` / `:14 img` / `:15 .pre-commit-config.yaml` / `:17 CHANGELOG.md` / `:18 .lock$`; the anchored twins are `:16 ^\.github$`, `:40 ^\.vscode$`, `:42 ^\.pre-commit-config\.yaml$`, `:43 ^\.github`. R CMD build applies each line as a case-insensitive Perl regex to every path relative to the package root.
- Failure scenario: a future file such as `R/imgHelpers.R`, `inst/shiny/dta_app/www/img/logo.png` (path contains "img"), `inst/extdata/unlock` (ends in "lock") or `R/github_helpers.R` (".github" matches "/github") is silently dropped from the built tarball. `pkgload::load_all()` and `devtools::test()` still pass, so only an installed or built package breaks (the trap .gitignore:11-14 already documents for helper.R); the qualification workflow installs from the source directory (`R CMD INSTALL .`, qualification.yml:103) and `inst-files.sha256` is generated from the working tree, so the IQ file check would not see the missing file either; only an IQ run on the released tarball (release.yml builds with `R CMD build .`) would. Today nothing is hit: I emulated the six patterns with `grep -P -i` over all 1,352 non-renv/non-thoughts paths and only the intended files (.vscode/*, img/*, .pre-commit-config.yaml, CHANGELOG.md, renv.lock) match.
- Confidence: verified-by-reading (patterns + emulation over the snapshot path list); the case-insensitive Perl matching rule of R CMD build is documented behaviour that I did not run
- Suggested direction: anchor every entry (`^img$`, `^CHANGELOG\.md$`, `^renv\.lock$`) and delete the unanchored duplicates.

### F-d2-meta-ci-03  [minor] [code-cheat]
- Where: .github/workflows/test-coverage.yaml:110-113, 212-215
- What: the coverage gate is silently disabled whenever `COVERAGE_FLOOR` is unset, empty or not a plain number.
- Evidence: `floor <- suppressWarnings(as.numeric(Sys.getenv("COVERAGE_FLOOR")))` then `if (!is.na(floor) && total < floor) { stop(...) }` (same two lines in the qualification-coverage job).
- Failure scenario: someone edits the env block to `COVERAGE_FLOOR: ""` or `"85%"`, or deletes the line: `as.numeric()` gives NA, the warning is suppressed, the `!is.na(floor)` guard is FALSE, and the job passes at 0% coverage while still printing a plausible table. A gate that depends on a typo-able string must fail closed.
- Confidence: verified-by-reading
- Suggested direction: `if (is.na(floor)) stop("COVERAGE_FLOOR is not a number")` before the comparison, in both jobs.

### F-d2-meta-ci-04  [minor] [doc-wrong]
- Where: [assistant-tool].md:181-187; .github/[assistant-tool]-instructions.md:129; .[assistant-tool]/agents/r-review.md:33-35 (its list of "unrequested bump" sites also omits `_index.yaml`); .github/scripts/bump_version.R:28-30,97-162,367
- What: the docs list the version sites wrongly: [assistant-tool].md says the CHANGELOG heading is checked by check_version_sync.R (it is not) and omits `_index.yaml`; [assistant-tool]-instructions.md names the app's manifest.json as a version site (it carries none).
- Evidence: `[assistant-tool].md:185-187  CHANGELOG.md heading — all rewritten by bump_version.R ... and checked by check_version_sync.R in CI`; `bump_version.R:367  for (site in sites[-1L]) {` where `sites` (l.97-162) = DESCRIPTION, VERSION file, `inst/qualification/requirements/_index.yaml`, docs badge, docs footer, so no CHANGELOG site; `[assistant-tool]-instructions.md:129  moves together with inst/shiny/dta_app/VERSION, the app's manifest.json, docs/index.html, and the CHANGELOG.md heading`; `bump_version.R:28-29  The manifest carries no DTAtools entry and no version`.
- Failure scenario: a hand bump that leaves `## [0.28.0]` in CHANGELOG.md while DESCRIPTION says 0.29.0 passes `check_version_sync.R` and r-style although [assistant-tool].md promises that CI checks it; a contributor following [assistant-tool]-instructions.md looks for a version in manifest.json and misses the `package_version:` line in `_index.yaml` (rewritten by bump_version.R:117-130 and checked by it).
- Confidence: verified-by-reading
- Suggested direction: correct both lists (DESCRIPTION, VERSION, `_index.yaml`, badge, footer are checked; CHANGELOG heading and the qualification manifest are written but only the manifest has its own check), or add a CHANGELOG site to `check()`.

### F-d2-meta-ci-05  [minor] [doc-wrong]
- Where: .github/agents/DTAtoolsKeeper.agent.md:73-74; .github/[assistant-tool]-instructions.md:73,199; README.md:1598 (cross-slice); R/zzz.R:3
- What: three documents say validation is JSON-Schema-backed / compiled from JSON Schema in evaluateRules.R, but the code validates "without a JSON Schema validator" and `jsonvalidate` is used nowhere.
- Evidence: `DTAtoolsKeeper.agent.md:73  Validation is JSON-Schema-backed (`jsonvalidate`) for fast row-level schema`; `[assistant-tool]-instructions.md:73  R/evaluateRules.R — JSON Schema compilation and row-level validation (performance-critical)`; `R/columnSpecChecks.R:1  # Column-spec-axis validation without a JSON Schema validator.`; grep -i for `json.?schema` in R/evaluateRules.R finds nothing; `jsonvalidate` occurs in no R, test, vignette or DESCRIPTION line (only manifest.json/renv.lock). `R/zzz.R:3` still declares the global `"instancePath"` (a jsonvalidate/ajv error column) that nothing uses any more.
- Failure scenario: an agent or contributor told to "keep jsonvalidate-based checks consistent" or to profile "JSON Schema compilation in evaluateRules.R" looks for code that does not exist; a stale `globalVariables()` name would also mask a real unbound `instancePath` typo.
- Confidence: verified-by-reading
- Suggested direction: rewrite the three passages to say constraints are derived by `as_json_schema()` and evaluated natively in columnSpecChecks.R; drop `instancePath` from zzz.R.

### F-d2-meta-ci-06  [minor] [dead-code]
- Where: manifest.json:239,1653-1666; .github/scripts/check_manifest.R:90-151
- What: the Connect manifest still lists `jsonvalidate` and its dependency `V8`, which nothing in the repo uses, and check_manifest.R can only detect missing packages, never superfluous ones.
- Evidence: `manifest.json:1653  "jsonvalidate": {` ... `:1666  "Imports": "R6, V8",` and `:239  "V8": {`; DESCRIPTION Imports/Suggests, R/, inst/ and the app contain no `jsonvalidate`/`V8`; check_manifest.R only does `setdiff(deps, listed)` and `setdiff(unique(loaded), c(listed, "DTAtools"))`.
- Failure scenario: every Connect (re)build restores V8 (a compiled package that needs a V8/libv8 toolchain or a static-libv8 download) for no purpose, so a deploy can fail or slow down because of a package nobody calls; further stale entries accumulate unseen because the `packages` block is a frozen snapshot (bump_version.R:34-37).
- Confidence: verified-by-reading for "unused" (grep over R/, inst/, tests/, vignettes/, benchmarks/, .github/); inferred for the V8 install cost on Connect
- Suggested direction: drop the two entries and have check_manifest.R report packages that are neither needed by DESCRIPTION/app nor a dependency of one that is.

### F-d2-meta-ci-07  [minor] [design]
- Where: .github/scripts/qualification_verdict.R:17; R/qualification.R:1364-1369; .github/workflows/qualification.yml:79-125
- What: the CI gate fails only on a verdict starting with "FAIL", so "PASS WITH NOT-EXECUTED TESTS" and "PASS (PARTIAL)" are green, and this workflow, unlike R-CMD-check.yaml:125,142-150, installs no TinyTeX and has no guard that the cases it expects to run did run.
- Evidence: `qualification_verdict.R:17  if (startsWith(verdict, "FAIL")) stop("The qualification run failed.")`; `R/qualification.R:1364  } else if (any(tests_df$status == "skip")) {` ... `"PASS WITH NOT-EXECUTED TESTS"`; `:1369  if (isTRUE(partial)) paste(verdict, "(PARTIAL)") else verdict`.
- Failure scenario: a precondition helper regresses (renamed Suggests package, `qa_requires()` now FALSE) so a whole group of OQ/PQ cases becomes "not executed": the runner reports PASS WITH NOT-EXECUTED TESTS, qualification_verdict.R prints it and exits 0, and the nightly "keeps the suite from rotting" job stays green while testing less every day.
- Confidence: verified-by-reading (gate and verdict logic); inferred (how many cases are skipped on the runner today)
- Suggested direction: require `identical(verdict, "PASS")` (or a pinned maximum of not-executed cases) in the CI step and install the optional tools the OQ/PQ cases use.

### F-d2-meta-ci-08  [minor] [design]
- Where: .github/scripts/check_r_sanity.R:21; .github/scripts/check_deps_in_desc.R:29; .github/scripts/style.R:125-129
- What: the parse/debug-call check and the undeclared-dependency check only scan R/, although style.R's own header says inst/ holds "a large body of R code, comparable in size to R/ itself" (94 .R files under inst/ against 70 in R/), and style.R explicitly does not count an unparseable file as a violation.
- Evidence: `check_r_sanity.R:21  r_dir <- "R"`; `check_deps_in_desc.R:29  r_dir <- "R"`; `style.R:125-129  # NA means styler could not process the file at all ... Not treated as a style violation ... unstyled <- visited$file[which(visited$changed)]`.
- Failure scenario: a syntax error, a forgotten `browser()` or a `pkg::fn()` of an undeclared package in `inst/shiny/dta_app/R/*.R`, `inst/qualification/**` or the template engine passes r-style (styler returns NA for the unparseable file and the script ignores NA); only a test that happens to source that file would notice. check_manifest.R:119-123 covers parsing for the app directory only.
- Confidence: verified-by-reading (scripts); inferred that styler reports `changed = NA` for unparseable files (as the script's own comment states)
- Suggested direction: point check_r_sanity.R and check_deps_in_desc.R at c("R", "inst") and make style.R fail on NA.

### F-d2-meta-ci-09  [minor] [design]
- Where: .github/workflows/pre-commit.yml:56-66
- What: the autofix commit step runs even when the hook step failed, pushes with the default token (which starts no workflows, so the merged head commit never gets CI), and installs an unpinned pre-commit on a floating Python.
- Evidence: `pre-commit.yml:63-64  - uses: stefanzweifel/git-auto-commit-action@b863ae19... # v5   if: ${{ !cancelled() && github.event.pull_request.head.repo.full_name == github.repository }}`; `:57  run: python -m pip install pre-commit` with `python-version: "3.x"` (l.37); `bump-version.yml:73-74  # NOTE: a PR opened with the default GITHUB_TOKEN does not trigger other workflows`.
- Failure scenario: a PR with an unfixable hook failure (private key) and a fixable whitespace problem gets a bot commit on top; that commit starts no r-style/R-CMD-check/qualification run, so the SHA that is merged was never tested, and a new pre-commit major release can change hook behaviour between two runs of the same commit.
- Confidence: verified-by-reading (conditions, unpinned install); inferred (GitHub's rule that GITHUB_TOKEN pushes trigger no workflows, which this repo's own note at bump-version.yml:73 states)
- Suggested direction: commit autofixes only when the hook step failed solely because it fixed files, push with a token that triggers CI (or drop the autofix and let the contributor re-run the hooks), and pin pre-commit.

### F-d2-meta-ci-10  [minor] [security]
- Where: .github/workflows/release.yml (no `permissions:`); R-CMD-check.yaml:48; r-style.yaml:30; test-coverage.yaml:37
- What: release.yml, which uploads release assets, declares no `permissions:` block (it inherits the repository default), and three other workflows grant `read-all` where `contents: read` (plus `pull-requests: read` for ci-tier) would do, unlike qualification.yml:42-45.
- Evidence: release.yml has no `permissions` key but runs `gh release upload "${TAG}" "./${FILENAME}" --clobber ...` with `GITHUB_TOKEN`; `R-CMD-check.yaml:48  permissions: read-all`; `qualification.yml:42-44  permissions: contents: read / pull-requests: read` is the least-privilege form the others should copy.
- Failure scenario: if the repository's default token permission is read-write, the release job (and any step added later) holds write access to everything; if it is read-only the upload fails only at release time, after the tarball was built.
- Confidence: verified-by-reading; inferred (the repository-level default token setting)
- Suggested direction: add `permissions: contents: write` to release.yml's job and `contents: read` / `pull-requests: read` to the three read-all workflows.

### F-d2-meta-ci-11  [nit] [inconsistency]
- Where: .github/workflows/qualification.yml:75
- What: qualification.yml pins actions/checkout v4 while the other eight checkout steps in six workflows pin v5.
- Evidence: `qualification.yml:75  - uses: actions/checkout@11d5960a326750d5838078e36cf38b85af677262 # v4` against `R-CMD-check.yaml:113`, `r-style.yaml:43`, `pre-commit.yml:27`, `test-coverage.yaml:63,152`, `release.yml:19`, `bump-version.yml:53`, `pages.yml:40`: `fbc6f3992d24b796d5a048ff273f7fcc4a7b6c09 # v5` (both SHAs verified: v4.4.0 and v5.1.0).
- Failure scenario: the one workflow that runs on three operating systems keeps the older Node runtime and behaviour while everything else moves on; a later bulk bump misses it.
- Confidence: verified-by-reading
- Suggested direction: use the v5 SHA everywhere (or none).

### F-d2-meta-ci-12  [nit] [doc-wrong]
- Where: .github/workflows/R-CMD-check.yaml:97-107; tests/testthat/helper-source-tree.R:19-21
- What: the comment says "Six tests are gated" by skip_on_cran (two in generic-guards plus three qualification tests, which is five, not six); the repo has 14 `skip_on_cran()` calls in 6 files, and helper-source-tree.R lists three test files for the source-tree step that now runs four.
- Evidence: `R-CMD-check.yaml:98-100  Six tests are gated that way -- the two subprocess tests in test-generic-guards.R ... and three that run the qualification runner end to end`; `grep '^\s*skip_on_cran()'`: generic-guards 2, qualification 7, shinyapp-export 2, shinyapp-modal-browser 1 (file level), shinyapp-modal-reattach 1, streaming-validation 1; `R-CMD-check.yaml:162  filter='generic-guards|source-portability|templateGuide-collect|rulePhrasing'` vs `helper-source-tree.R:20  filter = "generic-guards|source-portability| templateGuide-collect"`.
- Failure scenario: a maintainer reasoning from the comment believes only six tests depend on NOT_CRAN and under-estimates what a missing variable silently disables (14 gated blocks, incl. the browser tests).
- Confidence: verified-by-reading
- Suggested direction: state the real count (or drop the number) and add rulePhrasing to the helper comment.

### F-d2-meta-ci-13  [nit] [dead-code]
- Where: .pre-commit-config.yaml:47,73,81-82
- What: the `check-ast` hook (Python syntax check) has no Python file to check; the `forbid-to-commit` pattern is case-sensitive and its `exclude` is an unanchored search; one comment has a typo.
- Evidence: `:47  - id: check-ast` (the snapshot contains no `*.py` outside renv/thoughts); `:81  files: '\.(Rhistory|RData|Rds|rds)$'` and `:82  exclude: "(inst/extdata|data)/.*"`; `:73  # Only reuiqred when https://pre-commit.ci is used`.
- Failure scenario: `fit.Rdata`, `x.RDS` or `.rData` is committed without a hook failure, and `tests/testthat/fixtures/metadata/x.rds` is exempt because pre-commit evaluates `exclude` with `re.search`, so any path containing `data/` matches; `check-ast` costs a hook run per commit and checks nothing.
- Confidence: verified-by-reading; inferred (pre-commit's `re.search` semantics for `files`/`exclude`)
- Suggested direction: drop `check-ast`, anchor the exclude (`^(inst/extdata|data)/`), make the pattern `(?i)`, fix "required".

### F-d2-meta-ci-14  [minor] [logic-error]
- Where: benchmarks/bench_block_size.R:412-418 (data: benchmarks/block_size.csv)
- What: the "batch_rows becomes binding at N MiB" line uses `duplicated()`, which flags the second block size with the capped batch size, so it reports one sweep step too late.
- Evidence: `capped <- by_block[duplicated(by_block$max_rows), , drop = FALSE]` ... `min(capped$block_mib)`; block_size.csv: `32,1,13,76923,25671,131072,...` and `64,1,10,100000,51343,131072,...` with the default `batch_rows` 131072 (l.60), i.e. the cap is already reached at 32 MiB (max_rows == batch_rows) but the script prints 64 MiB; R/00_helpers.R:291-292 (and man/dta_stream_block_size.Rd) say "32 MiB yields the same thirteen batches as 16 MiB, the batch_rows cap of 131,072 having begun to bind", so the script contradicts the documentation derived from the same CSV (every other figure quoted there that the CSV can confirm matches it: 230/277 MB pool, 4.4 MB spread, the marginal MB per MiB, 4,897/39,185/19,594 rows, 19.5/14.7/12.9 s medians and ranges, 69 and 487 MB).
- Failure scenario: running the sweep on the committed fixture prints "becomes binding at 64 MiB: past there the block buys read-ahead and no rows", so a reader of the script's output concludes that 32 MiB still buys rows, when the 32 MiB row already costs 488 MB of Arrow pool (346 MB at 16 MiB) for the same thirteen batches.
- Confidence: verified-by-reading (code and committed CSV)
- Suggested direction: test `by_block$max_rows >= batch_rows` and report the first such block.

### F-d2-meta-ci-15  [minor] [doc-wrong]
- Where: .[assistant-tool]/commands/review.md:8; .[assistant-tool]/agents/r-review.md:14
- What: the branch review diffs against `master...HEAD`, but [assistant-tool].md says all work happens on `dev` and PRs target `dev`.
- Evidence: `review.md:8  means `git diff master...HEAD`.`; `r-review.md:14  `git diff master...HEAD` (branch)`; `[assistant-tool].md:177  Work on `dev`; PRs target `dev`. `master` is release-only.`
- Failure scenario: a feature branch cut from `dev` is reviewed against the merge base with master, so the "diff" contains every unreleased dev commit and the reviewer reports findings on code the branch never touched (or gives up on the volume).
- Confidence: verified-by-reading
- Suggested direction: use `dev...HEAD` (or the PR base) in both files.

### F-d2-meta-ci-16  [nit] [doc-wrong]
- Where: [assistant-tool].md:35,38; .github/[assistant-tool]-instructions.md:9; .[assistant-tool]/agents/r-verify.md:29-30; .github/agents/DTAtoolsKeeper.agent.md:224
- What: "~105 packages in renv.lock" (renv.lock has 114 `"Package"` entries) and ".Rprofile holds nothing but the autoloader" (it also holds the Connect `shiny.autoload.r` branch).
- Evidence: `[assistant-tool].md:38  `.Rprofile` holds nothing but the autoloader, so`; `.Rprofile:4-9  if ("CONNECT" %in% Sys.getenv(c("POSIT_PRODUCT", "RSTUDIO_PRODUCT"))) { ... options(shiny.autoload.r = FALSE) }`; Grep count of `"Package":` in renv.lock = 114.
- Failure scenario: advice to skip `.Rprofile` "at no cost" is right locally but wrong for anyone testing the Connect branch; the package count is stale in four files.
- Confidence: verified-by-reading
- Suggested direction: say "~110" or drop the number; mention the Connect branch.

### F-d2-meta-ci-17  [nit] [doc-wrong]
- Where: DESCRIPTION:2,10-11
- What: Title expands DTS as "Data Transmission Specifications", Description as "data transfer specifications" ([assistant-tool].md and the R help use "Transfer" and "Transmission" in comparable numbers: 15 vs 19 files).
- Evidence: `DESCRIPTION:2  ... Data Transfer Agreements (DTAs) and Data Transmission Specifications (DTS)`; `:10-11  ... data transfer agreements (DTAs) and data transfer specifications (DTS)`.
- Failure scenario: the package's one-line description and its title name two different things for the same acronym.
- Confidence: verified-by-reading
- Suggested direction: pick one expansion and use it in DESCRIPTION, README and the help pages.

### F-d2-meta-ci-18  [minor] [design]
- Where: .gitignore:7,10,20,25,64
- What: `testdata`, `example/`, `*docx`, `.tmp*` and `*.tar.gz` are unanchored (match at any depth), so a new test fixture directory or file with such a name is silently untracked.
- Evidence: `.gitignore:7  testdata`, `:10  example/`, `:20  *docx` (with two `!inst/extdata/word-templates/...docx` exceptions), `:25  .tmp*`, `:64  *.tar.gz`; no tracked directory is called testdata/example today (find), no tracked `*.tar.gz` exists.
- Failure scenario: `tests/testthat/testdata/bad.csv`, `inst/extdata/example/spec.yaml` or a `.tar.gz` template-library fixture is created, tests pass locally, `git add` ignores it, CI fails on the missing fixture (the exact failure the comment above the `!inst/extdata/library/parties/*.dta-party.yaml` exceptions recounts for party files). Also `:26 ..Rcheck/00check.log` is dead (the comment above `*.Rcheck/` says it matches neither the directory nor the log).
- Confidence: verified-by-reading
- Suggested direction: anchor to the intended directories (`/testdata/`, `/example/`) or whitelist the fixture folders.

### F-d2-meta-ci-19  [minor] [design]
- Where: .github/scripts/bump_version.R:339-346,393-403
- What: promoting `## [Unreleased]` replaces the heading and never re-creates an empty one, and `bump()` writes the sites one by one so a failing later site leaves a half-bumped tree.
- Evidence: `bump_version.R:341  lines[unreleased] <- heading` (heading = `## [<v>] - <date>`) while `promote_unreleased_link()` moves `[Unreleased]:` to `v<v>...HEAD`; `bump()` loops `site$write(v)` (l.394-402) before `update_changelog(v)` and the manifest sync, with `fail()` quitting mid-way.
- Failure scenario: right after a bump CHANGELOG.md has an `[Unreleased]:` link but no `## [Unreleased]` heading, while [assistant-tool].md:178 tells every change to go under it (the heading in this snapshot, l.7, was re-added by hand); a docs/index.html whose badge line changed shape makes `sole_match()` quit after DESCRIPTION and VERSION were already rewritten.
- Confidence: verified-by-reading
- Suggested direction: re-insert `## [Unreleased]` above the promoted heading and validate every site's shape before writing any.

### F-d2-meta-ci-20  [nit] [dead-code]
- Where: .Rbuildignore:19,39 (duplicate), 12/16/43 (.github), 13/40 (.vscode), 15/42 (pre-commit config); .Rbuildignore:63-68
- What: the file lists `^\.Rprofile$` twice, `^\.github$`/`^\.github` and `.github`, `^\.vscode$` and `.vscode`, and the comment "Planning directory" sits above five unrelated entries (app.R, manifest.json, .dtatools-lib, dts_metadata_coverage_overview).
- Evidence: `.Rbuildignore:19  ^\.Rprofile$` and `:39  ^\.Rprofile$`; `:63  # Planning directory — never part of the shipped package` followed by `^thoughts$`, `^app\.R$`, `^manifest\.json$`, `^\.dtatools-lib$`, `^dts_metadata_coverage_overview_.*\.md$`.
- Failure scenario: none functional; it makes the unanchored entries of F-02 look intentional and the file hard to audit.
- Confidence: verified-by-reading
- Suggested direction: de-duplicate and group by reason.

### F-d2-meta-ci-21  [nit] [doc-wrong]
- Where: CHANGELOG.md:3613,3764 (headings) vs link definitions at :5493-5500
- What: `## [0.21.0]` and `## [0.20.0]` have no `[0.21.0]:` / `[0.20.0]:` link definitions, and `[0.23.0]:` compares `v0.20.1...v0.23.0` (there is no 0.22.0 heading), so the Keep-a-Changelog footer is incomplete.
- Evidence: `CHANGELOG.md:5499  [0.23.0]: .../compare/v0.20.1...v0.23.0`; Grep for `^\[0\.2[0-4]\.[0-9]+\]: ` returns only 0.24.0, 0.23.0 and 0.20.1.
- Failure scenario: the two headings render as plain bracketed text instead of compare links.
- Confidence: verified-by-reading
- Suggested direction: add the two link definitions.

### F-d2-meta-ci-22  [minor] [design]
- Where: .github/workflows/R-CMD-check.yaml:18-32,77,83; ci-tier.yml:164-169
- What: the comment claims that a job skipped by `if:` "report[s] its status as 'Success'" and therefore satisfies the five required matrix contexts, but the skipped job is the matrix job itself, whose contexts only exist after the matrix is expanded.
- Evidence: `R-CMD-check.yaml:77  if: ${{ !cancelled() && needs.changes.outputs.skip != 'true' }}` on the job whose `name:` (l.83) is `${{ matrix.config.os }} (${{ matrix.config.r }})`; the comment at :29-32 relies on GitHub's "skipped by a conditional reports Success" rule.
- Failure scenario: a docs-only PR into master (all paths inert) skips the job before the matrix is expanded; as far as I know GitHub then reports one skipped check under the unexpanded name, so the five required contexts named in the master ruleset stay "Expected" and the PR cannot merge until a commit that touches a non-inert path is pushed.
- Confidence: inferred (GitHub's reporting of a job-level-skipped matrix job is not verifiable from the snapshot, and the master ruleset is not in the repo)
- Suggested direction: split the gate into a non-matrix aggregate job that is the required check, or run the one-leg tier for inert diffs instead of skipping the matrix.

### F-d2-meta-ci-23  [nit] [inconsistency]
- Where: .[assistant-tool]/agents/Explore.md:7-11, r-review.md:7-11, r-verify.md:7-11 (tracked files); [assistant-tool].md:99
- What: three tracked agent files hard-code machine-specific paths ("MACHINE-SPECIFIC", `C:\BITrusted\GitHubCLI\bin\gh.exe`, `C:\Program Files\R\R-4.5.1\bin\Rscript.exe`), while [assistant-tool].md says `gh` is on PATH and keeps machine specifics in the untracked [assistant-tool].local.md.
- Evidence: Explore.md:10 reads "**gh** (full path): C:\BITrusted\GitHubCLI\bin\gh.exe" (same line in the other two); [assistant-tool].md:99 reads "gh (on PATH) for all GitHub work"; on the review machine `gh` resolves to C:\Program Files\GitHub CLI\gh.exe and the BITrusted path does not exist.
- Failure scenario: any other contributor or CI machine gets wrong instructions in a shared file; on this machine the quoted gh path is already stale.
- Confidence: verified-by-reading for the tracked paths; inferred for "stale" (depends on which machine the file describes)
- Suggested direction: move the paths to [assistant-tool].local.md and have the agents refer to it.

### F-d2-meta-ci-24  [major] [logic-error]
- Where: benchmarks/bench_streaming.R:211-225 (columns stream_peak_mb / stream_rss_mb / stream_rss_after_gc_mb in benchmarks/streaming.csv)
- What: the eager run's `tbl` and `typed` objects are created inside the promise passed to `peak_during()`, so they live in the calling frame and are still referenced while the streamed run and the after-gc RSS reading are measured; the streaming figures therefore include a full eager copy of the file.
- Evidence: `bench_streaming.R:211  eager <- peak_during({ tbl <- dta_read_delim_normalized(...); typed <- dta_coerce_table_to_specs(as.data.frame(tbl), specs); validate_table_detailed(...) })` then `:223  streamed <- peak_during(validate_file_stream(...))` and `:231-233  invisible(gc(full = TRUE)) x2; stream_rss_after_gc_mb <- get_rss_mb()`; `tbl` is only re-assigned at :239 and `typed` is never removed. `peak_during()` forces its argument with `force(expr)` (l.190), i.e. in the caller's environment.
- Failure scenario: in the committed streaming.csv the streamed RSS after gc rises 354 -> 496 -> 693 MB while the input grows 50k -> 200k -> 800k rows, and the eager object that stayed alive grows 49.8 -> 134 -> 312 MB (eager_hold_mb) plus its Arrow table; the script's own text (l.304-309) reads RSS that "does NOT return to baseline after full gc" as "the signature of symbol-table interning - a permanent leak". The benchmark cannot tell a streaming leak from the leftover eager table, so the central claim it exists to test ("peak memory is governed by the batch size, not the input", l.5-12) is neither confirmed nor refuted by these numbers. A change that makes streaming retain data proportional to the input would be absorbed into the already-growing figures.
- Confidence: verified-by-reading (R evaluates a promise in the caller's frame; no `rm()` between the runs); the exact share of the growth due to the leftover objects is inferred from the CSV sizes, not measured
- Suggested direction: run each path in its own function or fresh R process (as bench_block_size.R does), or `rm(tbl, typed); gc()` before the streamed measurement.

### F-d2-meta-ci-25  [minor] [design]
- Where: benchmarks/bench_streaming.R:239-249,285-287
- What: the "what each path must HOLD" figure for streaming is the `object.size` of a slice of `batch_rows` rows, which is constant by construction and cannot grow with the input, whatever the streaming implementation retains.
- Evidence: `one_batch <- as.data.frame(tbl$Slice(0, min(batch_rows, tbl$num_rows)))` / `stream_hold_mb <- as.numeric(utils::object.size(one_batch)) / 1024^2`; the report line `stream grew %sx (%s -> %s MB)` prints 1.05x (18.4 -> 19.3 MB in streaming.csv).
- Failure scenario: an accumulator that keeps every offending row or every group key (the leaks the header says went undetected, l.14-22) leaves `stream_hold_mb` unchanged, so the benchmark's headline "retention" metric stays at ~1.0x while the real retention grows.
- Confidence: verified-by-reading
- Suggested direction: measure what the scan actually holds (Arrow pool high-water mark, `sink`/accumulator sizes, RSS in a fresh process) and drop or relabel the slice size.

### F-d2-meta-ci-26  [minor] [logic-error]
- Where: benchmarks/bench_validation.R:161-177
- What: the stage chain does not chain: the typed result of the "coerce" stage is discarded and the "materialise", "columnspec" and "rules" stages run on the untyped read, so the per-stage costs (and the 80 GB extrapolation) are not those of the production path they claim to decompose.
- Evidence: `tbl <- read$value`; `coerce <- timed("coerce", dta_coerce_table_to_specs(tbl, specs))` (no later use of `coerce$value`); `materialise <- timed("materialise", as.data.frame(tbl))`; `df <- materialise$value`; `schema <- timed("columnspec", dta_columnspec_errors(specs, df))`. In production `validate_table_detailed(specs, table)` (R/validationFunctions.R:127-195) receives the table already typed at import and passes it to `dta_columnspec_errors()`.
- Failure scenario: text columns (AGE as character while the spec says SAS Num) take whatever path the checker uses for untyped input, so baseline.csv's "columnspec 10.45 s at 1M rows" can be higher or lower than the cost on a typed table, and an optimisation of the typed path would not show up.
- Confidence: verified-by-reading (the dropped value); inferred (how different the two paths' costs are)
- Suggested direction: feed `as.data.frame(coerce$value$table)` to the later stages.

### F-d2-meta-ci-27  [minor] [logic-error]
- Where: benchmarks/bench_rules.R:67-72
- What: the AGE fixture is documented as "~1% unconvertible" but contains about five bad values in total, because five strings are appended to the pool and the pool is then resampled with replacement.
- Evidence: `age_vals <- sample(c(as.character(sample(18:75, floor(0.99 * n_rows), replace = TRUE)), c("ninety", "N/A", ">65", "unknown", "<18")), n_rows, replace = TRUE)` under the comment `# AGE: character holding mostly numbers but ~1% unconvertible (these are expensive to coerce)`. The pool has floor(0.99 n) + 5 entries and n draws are taken from it, so each bad value is drawn about once: ~5 rows in 50,000 or 200,000.
- Failure scenario: the rules benchmark (rules.csv, CHANGELOG l.4323-4369 cite it) is meant to expose the cost of coercing unconvertible text, but it exercises that path 200-400 times less than documented, so a slow-failure path in the AGE range rules goes unmeasured.
- Confidence: verified-by-reading
- Suggested direction: build the vector as `c(numbers, rep(bad, length.out = n_rows - length(numbers)))` and shuffle it, or sample bad values with `prob`.

### F-d2-meta-ci-28  [minor] [design]
- Where: benchmarks/bench_validation.R:234; bench_block_size.R:427; bench_streaming.R:317 (bench_rules.R:37 has `--out`)
- What: three scripts always overwrite their committed result CSV in `benchmarks/`, so any run with non-default arguments replaces the baseline, and baseline.csv cannot be reproduced with the script's defaults.
- Evidence: `bench_validation.R:234  out_path <- file.path("benchmarks", "baseline.csv"); utils::write.csv(results, out_path, row.names = FALSE)`; its default rows are `"10000,50000,200000"` (l.31) but the committed baseline.csv holds n_rows 200000 and 1000000 only; the usage text itself suggests `--rows 1000000`; rules.csv also stores `2e+05` for 200000 rows because `row_counts` is numeric (bench_rules.R:35).
- Failure scenario: running any usage example from the script header (`--rows 10000,50000,200000 --cols 20`, or `--rows 1000000`) replaces the 200k/1M baseline that bench_rules.R:5-6 compares against with a different set of rows; nobody can tell which arguments produced the committed rows.
- Confidence: verified-by-reading
- Suggested direction: add an `--out` argument to all scripts (default a scratch path), record the generating command in the CSV header or a README, and format row counts as integers.

### F-d2-meta-ci-29  [minor] [dead-code]
- Where: NAMESPACE:136-221 (generated); source tags R/validationFunctions.R:5-7, R/exportFunctions.R:4-8,221-224, R/DTAColumnSpecCollection-class.R:199,545-547, R/DTADataSetTabular-class.R:436-441, R/formattingHelpers.R:4, R/exportDocuments.R:22-25, R/DTAFile-class.R:307
- What: about 45 imported symbols are never called unqualified anywhere in R/, so they are dead imports left over from removed code (the JSON-Schema era), and they contradict the repo rule "call dependencies namespaced".
- Evidence: NAMESPACE imports `dplyr` across/any_of/arrange/distinct/everything/filter/group_by/mutate/n/select/summarise/where, `flextable` align/bg/body_add_flextable/bold/border_inner/border_outer/flextable/font/italic/padding/save_as_docx/valign/width, `officer` body_add_break/body_add_par/fp_border/fp_par/fp_text/read_docx, `jsonlite` fromJSON/toJSON/write_json, `stringr` str_c/str_detect, `yaml` read_yaml/write_yaml, `glue::glue`, `tools` file_ext/md5sum, `R.utils::gzip`, `docxtractr::docx_extract_all_tbls`, `methods::is`, `purrr::set_names`; a scan of the non-comment lines of R/*.R for `(^|[^A-Za-z0-9._:$@])symbol\(` finds 0 unqualified calls for each of them (the symbols that are used: cli_*, str_flatten_comma 6, str_glue 9, setNames 3, write.table 2, arrow_table 2, map 2, `%>%`).
- Failure scenario: a new unqualified `filter(x, ...)`, `select(...)`, `n()`, `width(...)`, `font(...)` or `where(...)` in R/ silently binds to dplyr/flextable (masking stats::filter etc.) and still passes check_deps_in_desc.R and R CMD check, because the symbol is already imported; meanwhile `purrr` and `magrittr` stay in Imports for two `map()` calls and the `%>%` pipes.
- Confidence: verified-by-reading (regex scan of R/; a symbol used only as a function value, e.g. `lapply(x, filter)`, would be missed, and I saw none)
- Suggested direction: delete the stale `@importFrom` tags, regenerate NAMESPACE, and consider dropping purrr (two `lapply` equivalents) from Imports.

### F-d2-meta-ci-30  [nit] [doc-wrong]
- Where: .pre-commit-config.yaml:19-26
- What: the comment says a vendored `renv/staging_excluded/` is tracked in git and that `renv/.gitignore` does not list it, but neither is true of the pinned tree.
- Evidence: `.pre-commit-config.yaml:19-22  renv/staging_excluded/ is a vendored renv package-library snapshot that ended up tracked in git (renv/.gitignore only lists `staging/`, not the distinct `staging_excluded/` directory ...`; the tree's `renv/` holds only `.gitignore` and `activate.R`, and `renv/.gitignore` lists `staging/` and `staging_excluded/`.
- Failure scenario: the `exclude:` on l.26 and its justification outlive the cause, and a reader believes tracked vendored code still exists.
- Confidence: verified-by-reading
- Suggested direction: delete the comment and shrink the exclude to what exists.

### F-d2-meta-ci-31  [minor] [doc-wrong]
- Where: [assistant-tool].md:17,24-28; .github/[assistant-tool]-instructions.md:20; .github/workflows/R-CMD-check.yaml:164-167
- What: the "Full check (= CI)" command omits `--as-cran`, which the CI step uses by default, so the documented local check is weaker than the one CI runs.
- Evidence: `[assistant-tool].md:17  | Full check (= CI) | Rscript -e "rcmdcheck::rcmdcheck(args='--no-manual', env=c(NOT_CRAN='true'))" |`; `R-CMD-check.yaml:164-167  - uses: r-lib/actions/check-r-package@... with: upload-snapshots: true  build_args: 'c("--no-manual","--compact-vignettes=gs+qpdf")'` (no `args:` input, so the action's default `c("--no-manual", "--as-cran")` applies).
- Failure scenario: a change that only `--as-cran` flags (an extra WARNING class: e.g. a non-portable file name, a too-long path, a hidden file, an undeclared import in examples) passes the documented local run and fails CI with `error-on: warning`; [assistant-tool].md:24-28 argues the same point for NOT_CRAN ("checking something CI does not") and then leaves the second difference in.
- Confidence: verified-by-reading for the missing flag; inferred for the action default (action.yml is not in the snapshot)
- Suggested direction: use `args = c('--no-manual', '--as-cran')` in both documents, or pass `args:` explicitly in the workflow so the two stay visibly in step.

### F-d2-meta-ci-32  [minor] [doc-wrong]
- Where: [assistant-tool].md:103-107
- What: the merge procedure says to wait for "all CI workflows" but lists only R-CMD-check, r-style and pre-commit, while qualification.yml and test-coverage.yaml also run on every PR into master.
- Evidence: `[assistant-tool].md:103-107  ... until **all** CI workflows (`R-CMD-check` on all OS matrix targets, `r-style`, `pre-commit`) have finished and reported passing status`; qualification.yml:25-29 and test-coverage.yaml:27-31 trigger on `pull_request` to master/dev, and the master ruleset (per R-CMD-check.yaml:81-83) requires only the five R-CMD-check legs.
- Failure scenario: a release PR whose qualification run (the audit-evidence suite) or coverage floor fails is merged because neither the ruleset nor the documented procedure waits for it.
- Confidence: verified-by-reading (the two lists); inferred (the ruleset contents, which are not in the repo)
- Suggested direction: name all five workflows in the procedure (or state that qualification and coverage are advisory), and decide whether they belong in the ruleset.

### F-d2-meta-ci-33  [nit] [bug]
- Where: .github/scripts/sync_qualification_manifest.R:30,40,85-86
- What: the manifest writer has weaker guards than its sibling in bump_version.R: an unreadable file is hashed as `NA` and written as a manifest line, the newline separator is a literal line break inside a string, hidden files under inst/ are silently left out, and on R < 4.5 it silently switches the committed baseline from sha256 to md5.
- Evidence: `:40  hashes <- unname(vapply(file.path("inst", files), hash_fun, character(1)))` (no NA check; bump_version.R:215-216 does `if (is.na(md5)) fail("could not compute an md5 for %s.", f)` for the same reason); `:85-86  writeLines(lines, con, sep = "` newline `")`; `:30  files <- list.files("inst", recursive = TRUE, all.files = FALSE)`; `:21-28  exists("sha256sum", asNamespace("tools"))` else `tools::md5sum`, while DESCRIPTION:44 declares `R (>= 4.1.0)`.
- Failure scenario: an inst/ file that md5sum/sha256sum cannot read yields `NA  inst/...` in inst-files.sha256, and `--check` on the same machine reproduces it and passes; a future `inst/.something` file would ship but never be pinned by the IQ; a contributor on R 4.4 running bump_version.R rewrites the sha256 baseline as md5; if the script is ever checked out with CRLF the literal-newline separator becomes CRLF and the "LF on every platform" claim (l.78-82) fails.
- Confidence: verified-by-reading (no hidden files exist under inst/ today; the `NA` and R 4.4 paths are inferred from R's documented md5sum/sha256sum behaviour)
- Suggested direction: abort on NA, use "\n", pass `all.files = TRUE`, and refuse (not switch) when the committed manifest's algorithm is unavailable.

### F-d2-meta-ci-34  [minor] [inconsistency]
- Where: DESCRIPTION:19-42,46-65 (Imports/Suggests versions); tests/testthat (39 files); R/DTAColumnSpecCollection-class.R:7; R/DTAFileTabular-class.R:1498,1826 (open_delim_dataset); R/importConversion.R:894,907 (call_function)
- What: the declared minimum versions are lower than what the code and tests need, and `check_deps_in_desc.R` only checks that a package is named, never its version.
- Evidence: `DESCRIPTION:49  testthat (>= 3.0.0),` while 39 test files call `local_mocked_bindings()` (new in testthat 3.2.0; `expect_s7_class()` in test-DTAColumnSpecCollection.R:166 is newer still) and 47 call `expect_no_error()` (3.1.5); Imports `arrow`, `stringr`, `dplyr`, `cli`, `rlang` have no bound although R/ calls `arrow::open_delim_dataset()` (2 sites), `arrow::call_function()`, `Scanner$create(batch_size = )`, and NAMESPACE imports `stringr::str_flatten_comma` (added in stringr 1.5.0, 2022).
- Failure scenario: an environment with a frozen CRAN snapshot (typical for the clinical systems this package targets) satisfies every declared bound, installs the package, and then fails at the first call into the newer API ("... is not an exported object from namespace:arrow" / "could not find function local_mocked_bindings"); `devtools::test()` errors in 39 files instead of declining to run. CI never sees it because setup-r-dependencies installs the newest releases and the only older leg is oldrel-1 of R, not of the packages.
- Confidence: verified-by-reading for the testthat mismatch (function names and counts); inferred for the exact release that introduced `open_delim_dataset` and `str_flatten_comma` (not checked against NEWS)
- Suggested direction: raise testthat to the release that has `local_mocked_bindings()`/`expect_s7_class()`, add lower bounds for arrow and stringr, and have a script compare declared bounds with a list of required functions or install the declared minima in one CI leg.

### F-d2-meta-ci-35  [nit] [doc-wrong]
- Where: .github/[assistant-tool]-instructions.md:35,39,46,49
- What: DTADataSet, DTAFile, DTARule and DTAColumnSpecStructure are described as "abstract", but none is declared `abstract = TRUE` and each is instantiated directly in its own documented example.
- Evidence: `[assistant-tool]-instructions.md:39  **DTAFile** (abstract base, for file I/O)`; `R/DTAFile-class.R:57-58  file_info <- DTAFile("file.txt")`; `R/DTAColumnSpecStructure-class.R:15  DTAColumnSpecStructure(type = "Char", format = "$12.", length = 12, backend = "SAS")`; `grep abstract` in R/DTADataSet-class.R, DTAFile-class.R, DTARule-class.R, DTAColumnSpecStructure-class.R finds only a roxygen sentence at DTADataSet-class.R:515.
- Failure scenario: a contributor or agent trusts "abstract" and assumes `DTAFile()`/`DTARule()` cannot be built (or adds code relying on S7's abstract-class error), when they can and the examples do.
- Confidence: verified-by-reading
- Suggested direction: say "base class (not meant to be used directly)" or declare them `abstract = TRUE` and change the examples.

### F-d2-meta-ci-36  [nit] [dead-code]
- Where: dts_metadata_coverage_overview_2026-07-28.md:1-3 (repo root); .Rbuildignore:68
- What: a tracked root-level analysis document describes itself as historical and "partly superseded", cites a source archive that is "not included in the repository", and needs its own `.Rbuildignore` line.
- Evidence: `dts_metadata_coverage_overview_2026-07-28.md:3  > Status: historical analysis of 2026-07-28, partly superseded by later changes to DTAMetaData.`; `:7  - (source archive not included in the repository)`; `.Rbuildignore:68  ^dts_metadata_coverage_overview_.*\.md$`.
- Failure scenario: readers (and the d1 documentation checks) treat a self-declared stale design note as current documentation of the metadata model; it cannot be re-verified because its inputs are missing.
- Confidence: verified-by-reading
- Suggested direction: move it under thoughts/ (already excluded from the build and from CI) or delete it, so the repository root carries only current documents.

### F-d2-meta-ci-37  [minor] [inconsistency]
- Where: [assistant-tool].md:191-201; .github/[assistant-tool]-instructions.md:130; .[assistant-tool]/agents/r-review.md:36-39 (the rule); CHANGELOG.md:12,1717-1718,4864-4865,4974; .Rbuildignore:55
- What: the "never mention [assistant-tool], [assistant-tool], [assistant-tool] or [assistant] in any file" rule is stated without exception and the reviewer agent is told to "report any occurrence as a finding, however small", yet the pinned tree breaks it literally in the very file the rule names.
- Evidence: `[assistant-tool].md:193-196  ... or any other mention of [assistant-tool], [assistant-tool], [assistant-tool] or "[assistant]" to commit messages, PR/issue titles and bodies, review comments, code or roxygen comments, `CHANGELOG.md`, ... or any other file in this repository`; `CHANGELOG.md:4864-4865  `.github/[assistant-tool]-instructions.md`: repository-level instructions for GitHub [assistant-tool] covering commands ...`; `CHANGELOG.md:12  `.[assistant-tool]/skills/dta-template-authoring/` (new argument `agent_skill` ...`; `.Rbuildignore:55  # [assistant] assistant configuration — never part of the shipped package`; R/createTemplateRepo.R:181 writes `.[assistant-tool]/skills`, and the files [assistant-tool].md, .[assistant-tool]/ and [assistant-tool]-instructions.md themselves carry the names.
- Failure scenario: `/review` (r-review.md) applied literally flags any diff that touches those lines, and an agent following [assistant-tool].md "exactly" would have to remove feature documentation (`.[assistant-tool]/skills` is a shipped feature of `create_template_repo()`) or rename its own instruction files; the rule can be neither satisfied nor audited as written.
- Confidence: verified-by-reading
- Suggested direction: narrow the rule to what it is for (no authorship or "generated by" attribution to an assistant), list the permitted exceptions (tool-named config files, the `.[assistant-tool]/skills` feature), or scrub the four CHANGELOG hits and the `.Rbuildignore` comment.

## Coverage

Fully read (every line):
- DESCRIPTION, NAMESPACE, app.R, [assistant-tool].md, .Rbuildignore, .gitignore, .gitattributes, .pre-commit-config.yaml, .Rprofile, .renvignore, .graphifyignore, .github/.gitignore, vignettes/.gitignore, .vscode/settings.json
- .github/workflows/: R-CMD-check.yaml, ci-tier.yml, qualification.yml, r-style.yaml, pre-commit.yml, test-coverage.yaml, release.yml, bump-version.yml, pages.yml
- .github/scripts/: bump_version.R, check_version_sync.R, check_manifest.R, check_deps_in_desc.R, check_r_sanity.R, style.R, sync_qualification_manifest.R, qualification_verdict.R, build_clinical_template.R, build_container_fixtures.R, build_sas7bdat_fixtures.R
- .github/[assistant-tool]-instructions.md, .github/agents/DTAtoolsKeeper.agent.md
- benchmarks/: bench_validation.R, bench_parquet.R, bench_block_size.R, bench_rules.R, bench_streaming.R and the four CSVs
- .[assistant-tool]/agents/*.md and .[assistant-tool]/commands/{find,review,verify}.md (to check [assistant-tool].md's claims; .[assistant-tool]/commands/derive-agreement-options.md was not opened)
- inst/shiny/dta_app/VERSION

Checked by script/grep rather than line-by-line reading:
- docs/index.html (only the version badge and footer lines and the `DTAtools v` / `v0.` hits), inst/qualification/requirements/_index.yaml (the `package_version:` line), CHANGELOG.md (the `[Unreleased]` head, release headings, link definitions, and the [assistant-tool]/[assistant-tool] hits), README.md (install, S7 and JSON Schema lines only), renv.lock (package count and the S7 entry).
- manifest.json (3,382 lines): structure, top-level keys, `files` block (md5 of app.R equals the file), the 97-entry package list, and the S7 / V8 / jsonvalidate entries; the per-package `description` blocks of the frozen renv snapshot were not read.
- Collate vs R/ (set equality, no duplicates, parent-before-child for every `parent =` pair); NAMESPACE exports vs roxygen `@export` (all 128 have a tag at a definition site) and vs man/*.Rd (every export has an Rd page with an `\examples` section; the only `\dontrun` is run_dta_app, which [assistant-tool].md allows); `pkg::` use in R/ and inst/ vs DESCRIPTION; guards for Suggests packages in R/; unqualified use of every imported symbol; skip_on_cran() call sites; R-version-dependent syntax (none found beyond guarded sha256sum); GitHub action SHAs vs public tags.
- R/qualification.R verdict and hashing code, R/run_dta_app.R, R/zzz.R, R/00_helpers.R:228-330 (cross-checked the figures quoted in dta_stream_block_size() that benchmarks/block_size.csv can confirm: pool peaks, spread, marginal costs, rows per batch, medians and ranges all match; the 204 MB / 214 bytes-a-row / eight-thread figures describe the fixture and machine and cannot be checked from the CSV), signatures of the internals the benchmarks call.

Not covered / open questions:
- Whether the renv autoloader (`.Rprofile` -> `renv/activate.R`, enabled by default, no `RENV_*` variable in any workflow) leaves CI's `.libPaths()` alone for the steps that start R in the checkout root (style.R, roxygenise, the source-tree test step, qualification_verdict.R). [assistant-tool].md:30-39 documents that in a fresh checkout it hides every installed package; I could not trace renv's own `load()` from the snapshot, so I did not file it. A one-line `RENV_CONFIG_AUTOLOADER_ENABLED: FALSE` in the workflows would remove the question.
- S7 1.0.0 behaviour (F-01) and GitHub's reporting of a job-level-skipped matrix job (F-22) are inferred, not observed.
- The master branch ruleset and repository default token permissions are not in the repository (F-10, F-22, F-32).
- inst/qualification, inst/shiny, tests/ and the rest of R/ belong to other slices; I only opened the parts named above.
