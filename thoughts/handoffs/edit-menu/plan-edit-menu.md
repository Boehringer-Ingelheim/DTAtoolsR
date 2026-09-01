---
date: 2026-09-01
type: plan
status: complete
issue: https://github.com/Boehringer-Ingelheim/DTAtoolsR/issues/106
plan_file: thoughts/shared/plans/PLAN-edit-menu.md
branch: ai/github-issue-106-plan-d0af53
---

# Plan Handoff: the brandbar "Edit" menu (issue #106)

## Summary

The Shiny app's "Create new version" button becomes an **Edit** dropdown with
three ways into editing (create new version / edit current version / create new
from current), and the "Edit mode" switch is **removed entirely** — replaced by
a read-only status tag plus a "Stop editing" menu row.

## Plan created

`thoughts/shared/plans/PLAN-edit-menu.md` — 9 tasks.

## Key technical decisions

- **`rv$editing` replaces `input$edit_mode`.** Server-owned state cannot go
  stale when its control leaves the DOM, which is the trap the previous feature
  spent four `bslib::update_switch()` calls working around. All four are deleted.
- **Reuse `ds_edit_menu()` / `ds_edit_menu_item()`** (`ui_components.R:99-196`)
  rather than building a new control. Its rows are `actionLink`s rendering
  `<a class="action-button">`, so Shiny's click binding, the app's double-click
  guard (`GUARD_SELECTOR` at `ui_components.R:371`) and `testServer` all work
  unchanged.
- **The `create_new_version` input id is kept**, so its observer, modal, confirm
  handler and 24 test references survive the move from button to menu row.
- **"Create new from current"** writes version `0.1` and **one seeded** history
  entry ("Created from &lt;title&gt; version &lt;X&gt;."), left closed so an
  export does not overwrite the seed with a diff. Title, date and template
  provenance untouched.
- **"Edit current version" records nothing** — no open entry, no baseline, so
  `export_dta()` short-circuits. The menu row says so in its description.
- **Second bump in one session finalises the first entry.** The summary
  computation is extracted from `export_dta()` into `dta_version_finalise()` so
  both callers share one definition.
- **Status tag wording:** `Editing v1.0` / `Editing v1.1 (new version)` /
  `Editing v0.1 (new document)` — one shape, always naming the version.

## Research findings

- `output$edit_gate` (`app.R:473-482`) is the two-state slot: NULL on the
  landing page, `create_new_version_button()` while `rv$version_locked`, else
  `edit_mode_switch()`. Its `rv$doc_token` / `isolate(rv$structure)` dependency
  design and the two follow-up bugfixes behind it (051220a, 1563a65) must be
  preserved.
- `editing <- reactive(isTRUE(input$edit_mode) && !isTRUE(rv$version_locked))`
  at `app.R:427` is the single gate; every editing surface `req()`s it, and each
  observer re-checks it server-side (two-layer enforcement, VERIFIED).
- VERIFIED: `as.list(DTAMetaData)` guards `version_history` with
  `if (length(...) > 0)` (`R/DTAMetaData-helpers.R:61`) and the validator accepts
  a zero-length list (`R/DTAMetaData-class.R:122`) — so an emptied history is
  safe in YAML. We still seed one entry, by decision, not by necessity.
- VERIFIED: `.app-brandbar` sets no `overflow` (`theme.R:69-73`), so a dropdown
  will not be clipped by it.
- VERIFIED counts driving the effort estimate: `edit_mode` appears **111 times
  across 6 test files**; `unlock_editing()` at **65 call sites**. `helper-shinyapp.R:169-173`
  is the seam that absorbs most of it.
- `test-shinyapp-theme.R:153-154` pins the `.app-actions > .shiny-html-output`
  rules **and their source order** — leave both strings alone.
- `test-shinyapp-ui-components.R:16-136` is the `render_html()` + `ds_edit_menu`
  pattern the new `edit_menu()` tests copy.

## Decisions the user made (interview, this session)

1. Edit menu stays visible at all times; **the toggle switch is dropped**; a tag
   appears while editing. Tom noted the tag's wording is open — the plan proposes
   `Editing v<version> (<qualifier>)` and flags the alternative.
2. "Create new from current" → empty history **+ seeded 0.1 entry**.
3. "Edit current version" → **record nothing**.

## Assumptions made — VERIFY BEFORE IMPLEMENTING

- **"Stop editing" is an addition, not in the issue text.** Dropping the switch
  removes the only way back to a read-only view, so the plan adds a menu row.
  Confirm Tom wants it.
- **Template-created documents now enter edit mode immediately.** With no switch
  to flip, the alternative strands the author in a read-only view of a document
  they just created. This is a behaviour change beyond the issue's wording.
- **"Edit current version" is hidden once a version entry is open** this session,
  to stop it silently abandoning a part-written summary. Not stated in the issue.
- The status tag reads the version under `isolate()` gated on `rv$md_token`;
  confirm during implementation that `md_token` is in fact bumped by every path
  that changes `metadata@version` (it is bumped by the new-version confirm at
  `app.R:544` — check the Metadata tab's own version field too).

## For next steps

- Review `thoughts/shared/plans/PLAN-edit-menu.md`, especially the three
  assumptions above and the status-tag wording.
- After approval, implement in task order; **Task 7 (test migration) should be
  its own commit** before Task 8 adds new tests.
- Do **not** bump `Version:` (CLAUDE.md guardrail). Do run
  `Rscript .github/scripts/bump_version.R --sync-manifest` and
  `Rscript .github/scripts/style.R`.
- PR targets `dev`. Poll `gh run list --branch` rather than `gh pr checks` —
  manifest-sync's bot push moves the head and re-gates every workflow.
