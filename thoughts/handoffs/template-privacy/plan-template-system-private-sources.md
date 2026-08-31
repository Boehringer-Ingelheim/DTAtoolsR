---
date: 2026-08-29
type: plan
status: complete
plan_file: thoughts/shared/plans/PLAN-template-system-private-sources.md
branch: ai/planning-agent-template-privacy-d05774
---

# Plan Handoff: Stronger DTA Creation Templates

## Summary

Plans the expansion of "Create new from template" into a template *system*:
runtime-loaded private template sources configured by a single environment
variable, versioned templates with inheritance, first-class reusable dataset
templates with declarative deviations, four party-reuse mechanisms, and rebase
onto a newer template version.

## Plan Created

`thoughts/shared/plans/PLAN-template-system-private-sources.md` — 15 tasks.

## Key Technical Decisions

- **One env var, scheme-tagged sources.** `DTATOOLS_TEMPLATE_SOURCES` with
  `dir:` / `pkg:` / `git:` entries. Chosen over a private-package-only or
  mounted-dir-only design because it is the only option that needs no change to
  the committed `manifest.json`, leaks no internal names into the public repo,
  and is a single Posit Connect content variable.
- **Cache-first failure mode.** A failed source serves its last-good cache with
  a staleness banner; only a cold start with no cache hard-fails, and it then
  offers *no* templates rather than falling back to the packaged demo.
- **`GIT_ASKPASS`, not a URL credential.** Keeps the Bitbucket token out of
  `.git/config` and out of the process table.
- **`id@version` with the version inside the file.** Filenames stay free-form,
  several versions can coexist in one directory, and `extends:` resolves through
  the same index.
- **Dataset-level provenance reuses existing S7 properties.** `DTADataSet`
  already has `template_source` / `template_version` / `template_date`; only the
  DTA-level provenance needs a new property.
- **Exactly one S7 class change:** `DTAMetaData@template` (`class_list`), machine-owned.
- **`validate_template()` becomes a package export** so the private template repo's CI can call it without reaching into the app's auto-sourced helpers.

## Task Overview

1. Template source providers (`template_sources.R`) — grammar, resolution, cache
2. Git source provider — askpass auth, TTL refresh, redaction
3. Versioned template index (`template_index.R`) — `id@version` resolution
4. Template inheritance (`template_inherit.R`) — `extends:` merge contract
5. Dataset templates with options and patch deviations (`dataset_template.R`)
6. `metadata.template` provenance property — the S7 change
7. Party profiles (`party_profiles.R`)
8. Metadata carry-over at creation (upload / open document)
9. Post-creation swap-party action
10. Diff engine and rebase (`template_diff.R`) — three-way merge
11. App UI — grouped picker, diagnostics, refresh, conflict review
12. Split the packaged template to exercise the new machinery
13. Tests — 8 new/extended test files
14. Documentation, `.gitignore` guard, version bump, manifest sync
15. Public `validate_template()` export + copy-ready CI workflow for the private repo

## Research Findings

- **`DTADataSet` already carries template provenance** —
  `R/DTADataSet-class.R:55-57` defines `template_source`, `template_version`,
  `template_date`; emitted at `inst/shiny/dta_app/R/utils_dta.R:1909-1914`, read
  back via `do.call(DTADataSetFactory, x)` at `R/DTADataSet-class.R:326`, printed
  at `R/DTADataSet-class.R:184-191`, consumed by the export footer at
  `R/exportDocuments.R:922`. Currently hand-typed prose in the packaged template.
  This removes an entire S7 change from the plan.
- **`as.list(DTAMetaData)` is hand-written** (`R/DTAMetaData-helpers.R:45-89`),
  not property-driven — a new property does not round-trip unless it is edited.
- **The round-trip test would not catch that** —
  `tests/testthat/test-shinyapp-utils.R:838` asserts only dataset names, column
  ids and rule ids. Metadata round-trip is currently untested.
- **`dta_template_metadata_fields()` derives from the S7 class**
  (`inst/shiny/dta_app/R/template_core.R:117`), so a new property becomes
  author-writable — i.e. forgeable provenance — unless excluded like
  `import_issues`.
- **Exports name metadata fields explicitly** (`{VERSION_HISTORY}`,
  `meta@version_history`), so a new property will not alter generated documents.
- Template wiring: `app.R:769` (picker), `app.R:800` (load), `app.R:818`
  (create), `app.R:5803` (landing button), dynamic option UI `app.R:548-634`.
- App helper files are auto-sourced by `shiny::runApp()`; the only unit-test
  harness is `app_fn()` / `app_env()` in `tests/testthat/helper-shinyapp.R`.
- No new package dependency required: `rlang::hash()` and `yaml` are already in
  `Imports:`; git is invoked via `system2()`.

## Assumptions Made

- **VERIFY BEFORE IMPLEMENTING:** that `.dta_stringify_dates()` in `utils_dta.R`
  recurses into a nested list, so `metadata.template.created` serialises. It is
  applied to the whole metadata list in `dta_to_list()` so it is expected to, but
  this was inferred from call position, not read.
- **Confirmed Bitbucket Server / Data Center**, with GitHub also required for
  testing. HTTP basic auth with a fixed username and the token as password is
  accepted by both, so one code path serves both; `DTATOOLS_TEMPLATE_GIT_USER`
  defaults to `git` and `DTATOOLS_TEMPLATE_GIT_AUTH=bearer` is available for a
  DC instance configured to require a Bearer header.
- The Connect content account can execute `git` and reach Bitbucket. If egress is
  blocked, the `dir:` scheme with an ops-side sync is the documented fallback and
  needs no code change.
- **Metadata schema is free to change** and machine-owned fields no author can
  set are acceptable — so `metadata.template` is machine-owned, and
  `dta_metadata_machine_fields()` generalises the existing `import_issues`
  exception into a category.
- `numeric_version()` orders every template version anyone will author; the plan
  handles unparseable versions but treats them as a warning case, not normal.

## Open Concern (stated, not acted on)

Fifteen tasks in one release is a large, long-lived branch. One release was the
explicit decision and the plan delivers it in full; the task order given is a
valid landing order if it needs to be cut later (Tasks 1–3 are independently
reviewable; Tasks 7–9 do not depend on Task 10).

## For Next Steps

- Review `thoughts/shared/plans/PLAN-template-system-private-sources.md`
- Both prior open questions are now settled: Bitbucket **Server / Data Center**
  (with GitHub for testing), and the private repo **does** get its own validation
  CI — hence Task 15, which exports `validate_template()` and ships a copy-ready
  workflow file.
- After approval, implement in task order. Task 13's round-trip test extension
  should land **before** Task 6, so the provenance-loss failure mode is caught by
  a test that exists first.
