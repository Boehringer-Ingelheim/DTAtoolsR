# Plan: Transform DTAtools into a valid Bioconductor package

Date: 2026-08-26
Status: draft, awaiting user review
Target: Bioconductor **devel (3.24)** — current release is 3.23; submissions are
reviewed against devel. Next release cut is ~late October 2026, so a September
submission could land in 3.24; otherwise 3.25 (April 2027).

## Goal

Make DTAtools pass `R CMD build` + `R CMD check` + `BiocCheck` cleanly against
Bioconductor devel, satisfy the Bioconductor submission checklist (versioning,
DESCRIPTION metadata, vignette conventions, NEWS, tests, size/time budgets,
maintainer registration), and prepare the repository and logistics for a
`Bioconductor/Contributions` submission.

## The threshold question: scope (read first)

Bioconductor accepts software "related to the analysis and comprehension of
high-throughput genomic/biological data". DTAtools is a domain-general
DTA/DTS validation tool whose primary framing today is clinical/regulated data
transfer. This is the **single biggest risk** of the whole effort — everything
else in this plan is mechanical by comparison.

What we have going for us: the vignette already names "clinical research,
biomarker science, genomics" as the domain; `inst/extdata` already contains
small-RNA example data (`gf_data_small_smirna.tsv`); omics data transfer
between pharma and CROs/vendors is a genuine Bioconductor-adjacent workflow.

Decisions taken in this plan (see Task 1): position the package as
infrastructure for **specification-driven validation of biological/omics data
deliveries**, lead the vignette with an omics use case, and optionally ask on
bioc-devel before submitting whether the scope is acceptable. Fallback if
rejected: CRAN (the package is closer to CRAN's scope; nothing in this plan is
wasted there except biocViews/BiocStyle).

## Technical choices

- **Keep S7.** A rewrite to S4 would be regressive and out of scope. S7 is
  CRAN-published, maintained by the R Consortium OOP working group, and not
  prohibited by BiocCheck. Prepare a written rationale for the review thread;
  expect reviewer questions (see Risks).
- **Keep the Shiny app in-package** (`inst/shiny`, 757K, `run_dta_app()`
  guarded and `\dontrun`). Bioconductor allows bundled apps; splitting it out
  is out of scope.
- **Keep `styler` tidyverse formatting** (2-space indent). BiocCheck's 4-space
  preference is a NOTE, not an ERROR; swapping formatter would fight the
  existing `r-style` CI. Accept and justify the NOTE.
- **Convert CHANGELOG.md → NEWS.md** (single news file, shipped). Bioconductor
  scrapes NEWS for release announcements; the current CHANGELOG.md is
  build-ignored and invisible. Retire CHANGELOG.md rather than maintain both.
- **Bioconductor versioning replaces semver.** Submission requires `0.99.0`;
  Bioconductor then owns the version lanes (1.0.0 at first release, y even =
  release / odd = devel). The Keep-a-Changelog "adheres to semver" claim goes.
- **CI: add a Bioc-devel + BiocCheck job** (container
  `bioconductor/bioconductor_docker:devel`) alongside the existing 5-leg
  matrix, rather than replacing it.

## Current state analysis (verified in this worktree, v0.21.1)

**DESCRIPTION** ([DESCRIPTION](DESCRIPTION)):
- No `biocViews:`, no `BugReports:`. Version `0.21.1`. Redundant standalone
  `Maintainer:` line alongside `Authors@R`. `R (>= 4.1.0)`.
- 23 direct dependencies (21 Imports + `R`/`S7` Depends). BiocCheck flags
  high dependency counts (threshold ~20 — verify at first BiocCheck run).
  Diet candidates to audit: `magrittr` (native `|>`; R >= 4.1 already
  required), `glue` (cli embeds glue interpolation), `R.utils`, `fastmap`.
- License `LGPL (>= 3)` — acceptable to Bioconductor as-is.

**Vignette** ([vignettes/DTAtools.Rmd](vignettes/DTAtools.Rmd)):
- `rmarkdown::html_vignette`, not BiocStyle. 59 chunks, 11 `eval=FALSE`
  (~19% — already under the ≤50% unevaluated bar). No `sessionInfo()` at the
  end. Installation chunk uses `remotes::install_github()`.
- Assets: `dtatools_logo.png` is 479K (compressible), architecture SVG 11K,
  classes PNG 73K.

**Code** (audited via Explore, counts approximate):
- 25 `sapply()` calls (e.g. [R/DTAColumnSpecCollection-class.R:29](R/DTAColumnSpecCollection-class.R:29), [R/DTA-class.R:64](R/DTA-class.R:64), [R/exportFunctions.R:93](R/exportFunctions.R:93)).
- 4 `1:length()`/`1:n` ranges ([R/DTAColumnSpecCollection-class.R:124](R/DTAColumnSpecCollection-class.R:124), :553; [R/DTARuleColCondition-class.R:128](R/DTARuleColCondition-class.R:128); [R/formattingHelpers.R:212](R/formattingHelpers.R:212)).
- 3 `<<-` in R/ (cache init: [R/evaluateRules.R:1113](R/evaluateRules.R:1113); [R/columnSpecChecks.R:121](R/columnSpecChecks.R:121), :133).
- 4 `print(doc, target=…)` calls in [R/exportDocuments.R](R/exportDocuments.R) — that is officer's documented save API; keep, expect a BiocCheck NOTE.
- Clean otherwise: no bare `T`/`F`, no `library()`/`require()` in R/, no
  `set.seed`, no unrestored `options()`/`par()`, no network access, all
  conditions via cli, `Sys.setenv` restored via `on.exit`.
- Examples: 124 roxygen `@examples` blocks; only 3 `\dontrun` (`write_dta()`,
  `write_validation_report()`, `run_dta_app()` — all justifiable); 2
  commented-out example blocks ([R/DTAColumnSpecCollection-class.R:439](R/DTAColumnSpecCollection-class.R:439), :461).
- NAMESPACE: 109 exports. `zzz.R` `.onLoad` only registers S7 methods — clean.

**Repo/infra**:
- Tarball-relevant content ≈ 4.8M — under the 10MB Bioconductor cap; largest
  single files well under the 5MB per-file cap. No `data/`, no `.rda`/`.rds`.
- No NEWS in any accepted form. No `inst/CITATION`.
- Tracked renv infrastructure: `renv/activate.R`, `renv.lock`, `.Rprofile`
  (build-ignored, but present in the repo the Single Package Builder clones).
- Stray tracked file `dts_metadata_coverage_overview_2026-07-28.md` at repo
  root, **not** build-ignored → currently ships in the tarball.
- `.Rbuildignore` has ~10 stale entries (bla.md, test_comprehensive.*, …,
  none of which exist) and unanchored regexes (`.lock` matches any path
  containing "Xlock"; `img`, `.github`, `.vscode` unanchored duplicates).
- Tests: `tests/testthat.R` + 48 test files (969K); heaviest:
  test-streaming-validation.R (88K), test-evaluateRules.R (63K),
  test-shinyapp-server.R (54K). Check runtime on the Bioc builder is the
  unknown — budget is **< 10 min for `R CMD check --no-build-vignettes`**.
- CI: 5-leg R CMD check matrix (incl. R-devel) exists
  ([.github/workflows/R-CMD-check.yaml](.github/workflows/R-CMD-check.yaml));
  no BiocCheck job.
- Name availability: no `DTAtools` on CRAN (404) or Bioconductor (redirects to
  removed-packages) — verified 2026-08-26. GitHub repo is `DTAtoolsR` while
  the package is `DTAtools` — Bioconductor expects repo name == package name
  (VERIFY exact enforcement before Task 15).
- Branch model: work on `dev`, `master` release-only. The SPB builds the
  **default branch** of the submitted GitHub repo.
- Local toolchain: R 4.5.1 + BiocManager 3.22 (one release behind devel);
  BiocCheck and BiocStyle not installed.

## Tasks

### Task 1: Scope positioning and go/no-go framing
Produce the narrative that survives review, and give the user a clean
decision point before mechanical work starts.
- [ ] Rewrite `Description:` in DESCRIPTION to lead with biological/omics data
      transfer (biomarker, sequencing, assay deliveries between labs, CROs and
      sponsors), keeping the general mechanism secondary.
- [ ] Choose `biocViews` (proposal: `Software, Infrastructure, DataImport,
      QualityControl, ReportWriting`; validate the terms with
      `BiocCheck` / `biocViews::guessPackageType()`).
- [ ] Add a short "Why Bioconductor" paragraph to the vignette introduction
      (omics data exchange, reproducible validated deliveries).
- [ ] Optional but recommended: post a pre-submission scope inquiry to
      bioc-devel — a "yes in principle" costs a week and de-risks everything.
- [ ] Decision recorded: proceed to Bioconductor / fall back to CRAN.

**Files:** `DESCRIPTION`, `vignettes/DTAtools.Rmd`

### Task 2: DESCRIPTION overhaul
- [ ] Add `biocViews:` (from Task 1) and `BugReports:
      https://github.com/Boehringer-Ingelheim/DTAtoolsR/issues`.
- [ ] Set `Version: 0.99.0`.
- [ ] Remove the standalone `Maintainer:` line (derived from `Authors@R`).
- [ ] Decide the `R (>=)` floor: BiocCheck will NOTE anything older than the
      devel R (4.6); bumping narrows non-Bioc installability — recommend
      bumping to the Bioc-devel R and noting it in NEWS.
- [ ] Add `BiocStyle` to `Suggests` (used by the vignette after Task 5).
- [ ] Keep `S7` in `Depends` deliberately; write the one-line justification
      into the submission notes (users need S7 attached for `@`/generic
      ergonomics on R < 4.3).

**Files:** `DESCRIPTION`

### Task 3: Dependency diet (aim ≤ 20 direct)
For each candidate: find usages, replace, remove from `DESCRIPTION`, re-run
tests. Do not chase removals that create churn — reaching exactly 20 is not a
hard requirement, it silences a NOTE.
- [ ] `magrittr` → native `|>` (R floor already ≥ 4.1; after Task 2 it is 4.6).
- [ ] `glue` → cli's inline interpolation where that is the only use.
- [ ] `R.utils` → audit what it is used for (likely gzip helpers); replace
      with base/arrow equivalents if trivial, else keep.
- [ ] `fastmap` → audit; if only used in the Shiny app, move to `Suggests`.
- [ ] Re-count direct dependencies; record the final number in the plan notes.

**Files:** `DESCRIPTION`, `R/*.R` touched by replacements

### Task 4: NEWS and versioning machinery
- [ ] Convert `CHANGELOG.md` → `NEWS.md` at repo root (Bioc/`utils::news()`
      compatible headings: `# DTAtools 0.99.0` sections; fold the Unreleased
      section into 0.99.0 with a "switched to Bioconductor versioning" entry).
      Retire `CHANGELOG.md` (git history preserves it).
- [ ] Remove the `CHANGELOG.md` line from `.Rbuildignore`; ensure `NEWS.md`
      ships (it must NOT be build-ignored).
- [ ] Update `.github/workflows/bump-version.yml` and the release/manifest
      workflows for the 0.99.z scheme (they template the version into the
      Shiny app VERSION file and manifest.json).
- [ ] Update `CLAUDE.md` guardrails section (CHANGELOG → NEWS.md).

**Files:** `CHANGELOG.md` → `NEWS.md`, `.Rbuildignore`,
`.github/workflows/bump-version.yml`, `CLAUDE.md`

### Task 5: Vignette Bioc-ification
- [ ] Switch output to `BiocStyle::html_document` (keep toc); keep
      `%\VignetteEngine{knitr::rmarkdown}`.
- [ ] Installation section: `BiocManager::install("DTAtools")` in an
      `eval=FALSE` chunk, with the GitHub-devel install as the secondary path.
- [ ] Append a `## Session info` section calling `sessionInfo()`.
- [ ] Review the 11 `eval=FALSE` chunks — each needs a reason (install
      instructions and app-launching chunks qualify; convert any that could
      simply run).
- [ ] Lead with / add an omics-flavoured worked example using the existing
      `gf_data_small_smirna.tsv` extdata (supports Task 1).
- [ ] Recompress `vignettes/dtatools_logo.png` (479K → target < 100K).

**Files:** `vignettes/DTAtools.Rmd`, `vignettes/dtatools_logo.png`,
`DESCRIPTION` (Suggests)

### Task 6: BiocCheck-driven code fixes
- [ ] Replace 25 `sapply()` with `vapply()` (or `lapply()` where the result is
      consumed as a list); the intentional one documented at
      [R/reportRendering.R:134](R/reportRendering.R:134) gets `vapply` or an
      explicit justification comment.
- [ ] Replace the 4 `1:…` ranges with `seq_len()`/`seq_along()`.
- [ ] Refactor the 3 `<<-` cache initialisations in R/ to an environment
      idiom (`cache <- new.env(); assign into it`) — BiocCheck flags `<<-`.
- [ ] Delete the 2 commented-out example blocks in
      `R/DTAColumnSpecCollection-class.R`.
- [ ] Leave `print(doc, target=…)` (officer API) — prepare the justification
      for the review thread.
- [ ] Run `styler::style_pkg()` + `roxygen2::roxygenise()`; full test suite.

**Files:** ~8 files in `R/` per the audit locations above

### Task 7: Examples coverage audit
BiocCheck requires runnable examples on ≥ 80% of man pages for exported
objects. 124 example blocks vs 109 exports looks healthy, but the mapping
export→Rd→example must be verified, not inferred.
- [ ] Enumerate exported objects without a runnable example (script it:
      parse NAMESPACE vs man/*.Rd `\examples` minus `\dontrun`).
- [ ] Add examples for any gaps (extdata-based, tempdir-only writes).
- [ ] Keep the 3 `\dontrun` cases; confirm each has a stated reason in its
      roxygen (app launch, non-tempdir writes).

**Files:** roxygen blocks in `R/*.R` as gaps dictate

### Task 8: Repository hygiene
- [ ] Remove (or relocate out of the tree) the stray tracked file
      `dts_metadata_coverage_overview_2026-07-28.md` — it ships in the tarball
      today. **Confirm with the user before deleting.**
- [ ] Rewrite `.Rbuildignore`: drop the ~10 stale entries, anchor every
      pattern (`^…$`), remove duplicates, add `^thoughts$` (this plan's
      directory) and any other newly tracked dev files.
- [ ] Decide renv's fate for the submission branch: recommended — keep renv
      for local dev on `dev`, but remove `renv/`, `renv.lock`, `.Rprofile`,
      `.renvignore` from the branch submitted to Bioconductor (reviewers
      routinely ask; the SPB clones the whole repo). Alternative: leave them
      and respond if asked. **User decision.**
- [ ] Verify `R CMD build` output contains no surprises
      (`tar -tzf` the tarball and eyeball it); confirm < 10MB.

**Files:** `.Rbuildignore`, root-level strays, possibly renv files

### Task 9: Check-time budget
- [ ] Time `R CMD check --no-build-vignettes` locally; the Bioc budget is
      10 min on the builders (slower than this machine — leave headroom,
      target ≤ 5–6 min locally).
- [ ] If over: gate the heaviest tests (streaming validation, Shiny
      testServer suites) behind `testthat::skip_on_bioc()`, keeping a
      representative fast case of each in the normal run; the full suite
      still runs in the existing GitHub CI. `longtests/` is the heavier
      alternative if we want Bioconductor to still run them weekly.
- [ ] Verify no test/vignette needs > 8GB memory (streaming/arrow paths).

**Files:** `tests/testthat/test-*.R` as needed

### Task 10: Bioconductor check infrastructure
- [ ] Add `.github/workflows/bioc-check.yaml`: runs on the
      `bioconductor/bioconductor_docker:devel` container — `R CMD build`,
      `R CMD check` on the tarball, `BiocCheck::BiocCheck("--new-package")`.
      (biocthis's `use_bioc_github_action()` is a reasonable template.)
- [ ] Locally (optional, CI suffices): install R 4.6 + BiocManager (devel) +
      BiocCheck + BiocStyle for fast iteration; current machine is R 4.5.1 /
      BiocManager 3.22.
- [ ] Iterate until: BiocCheck **0 ERROR**; each remaining
      WARNING/NOTE either fixed or written down with a justification (this
      list becomes the submission-issue comment).

**Files:** `.github/workflows/bioc-check.yaml`

### Task 11: Submission logistics (mostly user actions, not code)
- [ ] Subscribe the maintainer email (the one in `Authors@R`) to the
      bioc-devel mailing list. **Required; checked at submission.**
- [ ] Register the same email on support.bioconductor.org.
- [ ] Resolve repo-name mismatch: package `DTAtools` vs repo `DTAtoolsR`
      (VERIFY current enforcement in the Contributions README; if required,
      rename the GitHub repo or create a `DTAtools` repo — renaming keeps
      stars/issues and GitHub redirects old URLs).
- [ ] Make the default branch carry the submissible 0.99.0 package (either
      release to `master` and point default there, or a dedicated
      `bioc-submission` branch as default for the review period — align with
      the existing dev/master + Connect deployment model. **User decision.**)
- [ ] Open the issue at `Bioconductor/Contributions` (template: repo link,
      confirmation checkboxes); add the Bioconductor webhook when
      instructed; set up BiocCredentials + SSH key.
- [ ] During review: respond to reviewer; every push must bump `0.99.z` to
      trigger a rebuild.
- [ ] Post-acceptance: add `git.bioconductor.org` as a second remote, set up
      the GitHub↔Bioc-git sync workflow, update README install instructions
      and badges (BiocManager install becomes primary).

**Files:** none (GitHub/registry actions), then `README.md` post-acceptance

## Success criteria

### Automated
- [ ] `R CMD build` tarball < 10MB, no unexpected contents, every file ≤ 5MB.
- [ ] `rcmdcheck::rcmdcheck(args = "--no-manual")`: 0 errors, 0 warnings on
      Bioc devel (container job) **and** the existing 5-leg matrix stays green.
- [ ] `BiocCheck::BiocCheck()` with new-package flag: **0 ERROR**; remaining
      NOTEs enumerated and justified in writing.
- [ ] `R CMD check --no-build-vignettes` wall time ≤ ~6 min locally.
- [ ] `devtools::test()` green; `pre-commit run --all-files` and the `r-style`
      workflow checks pass (styler + roxygen + deps-in-DESCRIPTION).

### Manual
- [ ] Vignette renders with BiocStyle, opens with the omics use case, installs
      via BiocManager, ends with `sessionInfo()`.
- [ ] NEWS.md readable by `utils::news()`.
- [ ] bioc-devel subscription + support-site account confirmed by the
      maintainer.
- [ ] Scope decision (Task 1) explicitly made by the user before Task 11.

## Out of scope
- Rewriting S7 classes as S4, or adopting SummarizedExperiment-style
  containers (no natural fit for spec-validation objects).
- Splitting the Shiny app or the Word/PDF export machinery into separate
  packages (a reviewer may ask; decide then).
- CRAN submission (fallback path only).
- Renaming the *package* (repo rename may be needed; package name stays).

## Risks (Pre-Mortem)

### Tigers
- **Scope rejection by Bioconductor** (HIGH). Domain-general validation tools
  sit at the edge of Bioconductor's remit. Mitigation: Task 1 positioning +
  pre-submission bioc-devel inquiry; fallback CRAN. Do Task 1 first and
  cheaply before investing in Tasks 2–10.
- **S7 pushback** (MEDIUM-HIGH). Reviewers steer new packages toward S4 and
  Bioc core classes. Mitigation: prepared rationale (S7 = successor OO system,
  package predates submission, no Bioc container fits DTS validation); this is
  a judgement call by the assigned reviewer — an explicit rejection on these
  grounds is possible but unlikely.
- **Check-time overrun on the SPB** (MEDIUM). 48 test files incl. streaming
  and Shiny testServer suites; builder hardware is slow. Mitigation: Task 9
  measurement + `skip_on_bioc()` gating.
- **Dependency-weight objections** (MEDIUM). 23 direct deps incl. heavy
  `arrow` plus office-document stack (`officer`, `flextable`, `docxtractr`,
  `xml2`). All CRAN-available on all Bioc platforms, so this is friction,
  not a blocker. Mitigation: Task 3 diet + justification list.

### Elephants
- **Ongoing dual-registry maintenance.** Bioconductor imposes a 6-month
  release cadence, its own git server, and devel/release branch discipline —
  on top of this repo's dev/master + Connect deployment machinery
  (manifest.json, bump-version, release workflows) and a corporate-owned
  GitHub org. Someone must own that overhead indefinitely; acceptance also
  means the version jumps 0.21.1 → 0.99.0 → 1.0.0, which every downstream
  consumer (Connect deployments, manifest pins) must absorb.
- **Maintainer continuity.** The maintainer address is a corporate email;
  Bioconductor deprecates packages whose maintainers stop responding.
- **Is Bioconductor actually the right registry?** CRAN fits a
  domain-general tool with fewer constraints and no scope argument. The user
  should make this call consciously at the end of Task 1, not by default.
