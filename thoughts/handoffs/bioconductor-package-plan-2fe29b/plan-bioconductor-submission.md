---
date: 2026-08-26T00:00:00+02:00
type: plan
status: complete
plan_file: thoughts/shared/plans/PLAN-bioconductor-submission.md
---

# Plan Handoff: Bioconductor submission for DTAtools

## Summary
Planned the full transformation of DTAtools (v0.21.1, S7-based DTA/DTS
validation package) into a Bioconductor-submissible package targeting Bioc
devel 3.24, with scope positioning identified as the dominant risk and gating
decision.

## Plan Created
`thoughts/shared/plans/PLAN-bioconductor-submission.md`

## Key Technical Decisions
- **Scope-first ordering**: Task 1 (biological positioning + optional
  bioc-devel pre-inquiry + explicit go/no-go vs CRAN) happens before any
  mechanical work — scope rejection is the highest risk.
- **Keep S7**: no S4 rewrite; prepared rationale for reviewers instead.
- **Keep styler 2-space formatting**: accept the BiocCheck formatting NOTE.
- **CHANGELOG.md → NEWS.md**: single shipped news file; semver claim dropped
  in favour of Bioconductor 0.99.z versioning.
- **CI additive**: new bioc-devel container job with BiocCheck alongside the
  existing 5-leg R-CMD-check matrix.

## Task Overview
1. Scope positioning + go/no-go (user decision gate)
2. DESCRIPTION overhaul (biocViews, BugReports, 0.99.0, drop Maintainer line)
3. Dependency diet (23 direct deps → aim ≤20: magrittr, glue, R.utils, fastmap)
4. NEWS.md conversion + version machinery (bump-version.yml, manifest)
5. Vignette Bioc-ification (BiocStyle, BiocManager install, sessionInfo, omics lead example)
6. BiocCheck code fixes (25 sapply, 4 `1:n`, 3 `<<-`, dead example blocks)
7. Examples coverage audit (≥80% runnable-example rule)
8. Repo hygiene (stray tracked md file, .Rbuildignore rewrite, renv removal decision)
9. Check-time budget (<10 min SPB limit; skip_on_bioc gating if needed)
10. Bioc check infrastructure (bioc-check.yaml workflow, local R 4.6 toolchain)
11. Submission logistics (bioc-devel subscription, repo-name mismatch, default branch, Contributions issue)

## Research Findings (verified in the worktree, not the stale main checkout)
- DESCRIPTION: no biocViews/BugReports; Version 0.21.1; redundant Maintainer
  line; 23 direct deps; License LGPL(>=3) is Bioc-acceptable.
- Vignette: rmarkdown::html_vignette (no BiocStyle), 59 chunks / 11 eval=FALSE,
  no sessionInfo(), installs via remotes::install_github (vignettes/DTAtools.Rmd:61).
- Code audit: 25 sapply, 4 `1:length`-style, 3 `<<-` (R/evaluateRules.R:1113,
  R/columnSpecChecks.R:121,:133), officer `print(doc, target=)` x4 in
  R/exportDocuments.R (keep); otherwise clean — cli conditions everywhere, no
  network/seed/options issues; only 3 justified \dontrun examples.
- Size: ~4.8M ships (cap 10MB); no data/, no binary R objects; 326 Rd files.
- Hygiene: stray tracked `dts_metadata_coverage_overview_2026-07-28.md` ships
  in the tarball today; ~10 stale + unanchored .Rbuildignore entries; renv
  files (renv/activate.R, renv.lock, .Rprofile) are git-tracked.
- Name `DTAtools` free on CRAN (404) and Bioconductor (removed-packages
  redirect) as of 2026-08-26; repo name `DTAtoolsR` mismatches package name.
- Bioconductor: current release 3.23, devel 3.24; limits confirmed from
  contributions.bioconductor.org: <10MB tarball, ≤5MB/file, <10 min
  `R CMD check --no-build-vignettes`, ≤8GB memory.
- Local toolchain: R 4.5.1 + BiocManager 3.22; BiocCheck/BiocStyle absent.
- Existing CI: 5-leg R-CMD-check matrix incl. R-devel; r-style; pre-commit;
  release/manifest/bump-version workflows tied to the semver scheme.

## Assumptions Made
- BiocCheck's direct-dependency NOTE threshold (~20) — verify at first
  BiocCheck run (Task 10).
- Repo-name==package-name enforcement by the Contributions bot — VERIFY in
  the Contributions README before Task 11.
- The 3 `<<-` sites are plain cache initialisation refactorable to an
  environment idiom — confirm when editing (Task 6).
- Streaming/Shiny test suites are the check-time drivers — measure before
  gating (Task 9).
- BiocManager::version() locally reports 3.22; devel targeting was inferred
  from bioconductor.org (release 3.23 / devel 3.24) — re-verify at submission.

## For Next Steps
- User reviews `thoughts/shared/plans/PLAN-bioconductor-submission.md`.
- The two explicit user decisions: (a) Task 1 go/no-go Bioconductor vs CRAN,
  (b) Task 8 renv removal from the submission branch + deletion of the stray
  root markdown file.
- After approval, implement in task order (Task 1 gates the rest); Tasks 2–8
  are ordinary dev-branch PR work under the existing styler/roxygen/CHANGELOG
  → NEWS conventions.
