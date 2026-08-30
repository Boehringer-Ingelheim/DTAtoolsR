---
date: 2026-08-30
type: plan
status: complete
plan_file: thoughts/shared/plans/PLAN-dta-document-versioning.md
branch: ai/planning-agent-dta-versioning-ce4b3c
base_branch: ai/planning-agent-template-privacy-d05774
---

# Plan Handoff: "Create new version" gate for loaded DTA documents

## Summary

A DTA loaded from an existing file (upload, bundled example, restored session)
opens read-only with a **"Create new version"** button where the Edit-mode
switch normally sits. Pressing it bumps `metadata.version`, opens a new
`version_history` entry, and unlocks editing. On export, that entry's `changes`
field is filled in with a summary of every difference between the document as
loaded and the document as it now stands.

## Plan created

`thoughts/shared/plans/PLAN-dta-document-versioning.md` — 10 tasks.

## Branch

The requested base was `ai/planning-agent-template-privacy-d05774`. The
worktree's current branch `ai/planning-agent-dta-versioning-ce4b3c` already
descends from it (`git merge-base --is-ancestor` confirms), so no new branch is
needed. Working tree is clean at `46d8f02`.

## Requirements settled with the user

| Question | Answer |
| --- | --- |
| Gated load paths | Uploaded file, bundled example, restored session. **Not** template-created documents. |
| Raw YAML "Apply" | Not gated; and it must **not** overwrite the live `version` / `version_history`. |
| Version number | Modal prefilled with next minor, editable, plus an optional note. |
| When `changes` is computed | Only on export/download. |
| Changes text | Grouped counts followed by full per-key detail. |

## Key technical decisions

- **Reuse `dta_diff()`** (`inst/shiny/dta_app/R/template_diff.R:280`) rather
  than writing a differ. It already flattens to dotted paths, keys columns by
  their own `id` instead of position, and sorts with `method = "radix"` so row
  order does not depend on locale collation.
- **Enforce the lock inside `editing()`** (`app.R:409`), not by hiding a
  control. Every editing surface already calls `req(editing())`, so one
  expression change gates all of them — and it closes the trap that Shiny does
  not reset an input's value when its control leaves the DOM.
- **Store the baseline as YAML text**, not as a second DTA object. Text
  `saveRDS`es for free; a live DTA needs `dta_dump_session()` because of its
  Arrow tables.
- **The generated summary is single-line and sanitised.** Forced by the
  Markdown exporter, which puts `changes` straight into a pipe-delimited table
  cell (`R/exportDocuments.R:333-338`).
- **Export finalisation is a pure read** (`export_dta()`), not a reactive write
  inside a `downloadHandler`.

## Task overview

1. New helper file `inst/shiny/dta_app/R/versioning.R` — pure functions
   (next-version suggestion, summary builder, sanitiser, entry writers).
2. Brandbar gate — `uiOutput("edit_gate")` replaces the static switch; new
   button; CSS.
3. Server state (`version_locked`, `version_baseline_yaml`,
   `version_entry_index`, `version_note`) and the `editing()` change.
4. Arm the lock on the three gated load paths; clear it on reset and on
   template creation.
5. Persist and restore the new state through the session `.rds`.
6. The "Create new version" modal and its confirm handler.
7. Preserve `version` / `version_history` across a Raw YAML apply.
8. `export_dta()` and routing all five document-export paths through it.
9. Tests — new `test-shinyapp-versioning.R`, plus a Markdown-table guard in
   `test-exportDocuments.R`.
10. Changelog, `DESCRIPTION` bump, `--sync-manifest`, `styler::style_pkg()`.

## Research findings

- ✓ VERIFIED `dta_diff(a, b)` returns `list(metadata = <df>, datasets = <df>)`,
  each `key | change | from | to` — `template_diff.R:280-291`.
- ✓ VERIFIED `rebase_apply()` already appends a `version_history` entry —
  `template_diff.R:764-775`. That is the shape to copy, and the one existing
  writer of history in the codebase.
- ✓ VERIFIED the DTAMetaData validator rejects an empty `changes` string —
  `R/DTAMetaData-class.R:139`. A newly opened entry therefore needs a
  non-empty placeholder.
- ✓ VERIFIED `dta_dataset_to_list()` is specification-only (no bound tables, no
  validation results), so uploading a data file cannot pollute the diff.
- ✓ VERIFIED the Markdown exporter writes `changes` into a markdown table cell
  via `.df_to_md_table()` — `R/exportDocuments.R:333-338`. A newline or a pipe
  in the summary would break the table. This shaped the output format.
- ✓ VERIFIED there is **no** existing document-version bump anywhere in `R/` or
  `inst/`; `.github/scripts/bump_version.R` bumps the package, not a document.
- ✓ VERIFIED there is **no** version-history UI in the app (`grep -n "history"
  inst/shiny/dta_app/app.R` returns nothing).
- ✓ VERIFIED five independent document-export paths, all reading `rv$dta`
  directly: `dl_yaml` (app.R:5452), `dl_docx` (app.R:5481), and the export
  modal's Markdown / PDF / built-in-Word / custom-template-Word branches
  (app.R:5501-5748). No single choke point exists.
- ✓ VERIFIED `rv$yaml_text` is never used for an export — every export
  re-serialises from `rv$dta`.
- ✓ VERIFIED bslib 0.12.0 is installed and exports `update_switch` and
  `toggle_switch`.
- ✓ VERIFIED `tests/testthat/helper-shinyapp.R:56-60` sources every `.R` file
  under the app's `R/` directory, so a new helper file is picked up
  automatically by the app and the harness alike.

## Assumptions made

- **The optional note field** in the modal is included on the strength of the
  chosen option's wording ("prefilled with next minor, editable, plus an
  optional note"). If unwanted, drop it and the `version_note` state with it.
- **`metadata.date` is not bumped** when a version is created. VERIFY BEFORE
  IMPLEMENTING if the author expects the document date to move too.
- **No seed history entry** is written for the pre-existing version. A document
  at 1.0 with no history ends up with one entry, for the new version only.
- **One new version per loaded document per session.** Once unlocked the button
  is gone; a second version requires reloading the document.
- **Restoring a pre-change session file** re-locks the document (fields absent
  means "treat as a fresh gated load"). Conservative, but it means an
  in-progress session saved before this ships reopens read-only.
- **The Raw YAML apply now re-serialises** rather than keeping the author's
  pasted text in `rv$yaml_text`, because the preserved version would otherwise
  not match what the Raw tab shows. This is a visible behaviour change to that
  tab.

## For next steps

- Review the plan at `thoughts/shared/plans/PLAN-dta-document-versioning.md`,
  in particular the five assumptions above.
- After approval, implement. Per `CLAUDE.md`, anything touching more than one
  file is planned on the main thread first — that is done; implementation can
  be delegated.
- Verification runs through `r-verify`; the pre-commit review through
  `r-review`.
- Do not forget `Rscript .github/scripts/bump_version.R --sync-manifest` —
  `inst/shiny/dta_app/` is changing and `manifest-sync` CI fails without it.
