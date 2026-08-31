# Plan: "Create new version" gate for loaded DTA documents

Branch: `ai/planning-agent-dta-versioning-ce4b3c` (descends from
`ai/planning-agent-template-privacy-d05774`, as requested — verified with
`git merge-base --is-ancestor`).

## Goal

When a DTA document is **loaded from an existing one** (uploaded YAML, bundled
example, restored session), the app must not offer the Edit-mode switch
straight away. It shows a **"Create new version"** button instead. Pressing it:

1. opens a small modal prefilled with the next minor version,
2. bumps `metadata.version` to the chosen value,
3. appends a new `version_history` entry for that version, and
4. unlocks editing — the Edit-mode switch replaces the button and is turned on.

From then on the app tracks what the author changes. When the document is
**exported or downloaded**, that new entry's `changes` field is filled in with
a summary of every difference between the document as loaded and the document
as it now stands. The version history therefore reads, entry by entry, as "what
changed between this version and the one before it".

## Decisions taken (from the requirements interview)

| Question | Decision |
| --- | --- |
| Which load paths are gated | Uploaded file, bundled example, **and** restored sessions. **Not** template-created documents — those are new, not loaded, so they stay directly editable. |
| Raw YAML "Apply" | Not a gated load. Additionally, applying pasted YAML **must not overwrite** the document's current `version` or `version_history` — both are preserved from the live document. |
| Version number | Modal prefilled with the next minor version, editable. Optional free-text note. |
| When `changes` is computed | **Only on export/download.** |
| What `changes` says | Grouped counts followed by the full per-key detail. |

## Technical choices

- **Reuse `dta_diff()`, do not write a new differ.**
  `inst/shiny/dta_app/R/template_diff.R:280` already produces exactly the
  comparison needed: `list(metadata = <df>, datasets = <df>)`, each a frame of
  `key | change | from | to` with `change` one of added/removed/changed. It
  flattens to dotted paths, keys columns by their own `id` rather than by
  position (so inserting a column does not report every later column as
  changed — `template_diff.R:150-162`), and sorts with `method = "radix"` so
  row order does not depend on the machine's collation.

- **The baseline is stored as YAML text, not as a second DTA object.**
  `rv$version_baseline_yaml` holds the document exactly as loaded. Text costs
  nothing to `saveRDS` (a live DTA needs `dta_dump_session()` to survive a
  round trip because of its Arrow tables — `utils_dta.R:2969`), it survives
  session restore unchanged, and `dta_read_yaml_text()` re-parses it on demand.
  The diff runs once per export, so re-parsing is not on any hot path.

- **Uploading data files cannot perturb the diff.**
  VERIFIED: `dta_dataset_to_list()` (`utils_dta.R`) emits only specification
  fields — name, type, description, template provenance, `files`, `columns`,
  `rules`. No bound tables and no validation results. So a diff between the
  loaded document and the current one reports specification edits only, which
  is what a version history should record.

- **The lock is enforced in `editing()`, not by hiding a control.**
  `editing <- reactive(isTRUE(input$edit_mode))` (`app.R:409`) is the single
  gate every editing surface already calls `req()` on. Adding the version lock
  to that one expression propagates the gate to every surface for free, with
  no per-observer changes. This also closes a trap that a purely cosmetic swap
  would open: Shiny does **not** clear an input's value when its control is
  removed from the DOM, so a switch that had been turned on and then
  un-rendered would leave `input$edit_mode == TRUE` behind it.

- **The generated `changes` string is single-line and sanitised.**
  VERIFIED constraint: the Markdown exporter writes `changes` into a markdown
  table cell (`R/exportDocuments.R:333-338`, via `.df_to_md_table()`). A
  markdown table cell cannot contain a newline, and a raw pipe character would
  split the cell. The summary is therefore built as one line, with `;` between
  detail items, and any newline or pipe inside a value replaced by a space.
  See "Risks" for the alternative that was rejected.

- **Export finalisation is a pure read, not a reactive write.**
  A helper `export_dta()` returns the document with the open entry's `changes`
  filled in. It does not mutate `rv$dta`. Writing reactive state from inside a
  `downloadHandler` content function would re-render the Metadata tab and the
  Raw YAML editor mid-download for no benefit.

## Current state analysis

### Key files

- `inst/shiny/dta_app/app.R` — the whole UI and server. `edit_mode_switch()` is
  placed in the static `brandbar` at **app.R:26**; `editing()` at **app.R:409**;
  `rv` at **app.R:342-388**; `apply_loaded()` at **app.R:514-541**;
  `autosave()` at **app.R:496-513**; the Raw YAML apply observer ending with
  `rv$dta <- new_dta` / `rv$yaml_text <- txt`; `restore_session` at
  **app.R:6242-6294**; `confirm_reset` at **app.R:5826**.
- `inst/shiny/dta_app/R/ui_components.R:20-22` — `edit_mode_switch()`, a pure
  function returning `bslib::input_switch("edit_mode", "Edit mode", value = FALSE)`.
- `inst/shiny/dta_app/R/template_diff.R:280-291` — `dta_diff()`, the differ to reuse.
- `inst/shiny/dta_app/R/template_diff.R:696-780` — `rebase_apply()`, which already
  appends a `version_history` entry; the shape to copy (`list(version, date, changes)`).
- `inst/shiny/dta_app/R/theme.R:70-128` — the `.app-brandbar` / `.app-actions` CSS,
  including the block that makes `input_switch()` sit inline among the pills.
- `R/DTAMetaData-class.R:49-143` — `version` (`class_character_or_null`),
  `version_history` (`class_list`), and the validator that requires every entry
  to be a list carrying `version`, `date` and `changes`, with **`changes` not an
  empty string** (line 139). This is why a newly created entry needs a
  placeholder rather than an empty string.
- `R/DTAMetaData-helpers.R:53-103` — `as.list(DTAMetaData)`, which ISO-formats
  each history entry's date on the way to YAML.
- `R/exportDocuments.R:191-219` (DOCX) and `:333-338` (Markdown) — where the
  history table is rendered.
- `tests/testthat/test-shinyapp-edit-mode.R` — the idiom every new test follows:
  `shiny::testServer(app_server_dir(), ...)`, `session$setInputs(edit_mode = ...)`,
  `load_fixture(session)`, and `app_fn("name")` to reach an app helper directly.
- `tests/testthat/helper-shinyapp.R:56-60` — sources **every** `.R` file under
  the app's `R/` directory, so a new helper file is picked up automatically by
  both the app and the test harness.

### What does not exist today

- VERIFIED: no function anywhere in `R/` or `inst/` bumps a DTA document's
  version. `.github/scripts/bump_version.R` bumps the *package*, not a document.
- VERIFIED: no version-history UI in the app — `grep -n "history" app.R`
  returns nothing. The Metadata tab exposes `version` (**app.R:4724**) and
  `date`, but not the history.

## Tasks

### Task 1 — New helper file `inst/shiny/dta_app/R/versioning.R`

Pure, server-free functions, so they are unit-testable through `app_fn()`
without `testServer`. Follows the commenting convention of the neighbouring
template files: say *why*, not *what*.

- [ ] `dta_next_version(v)` — suggest the next minor version.
  - `"1.0"` to `"1.1"`, `"2.3.4"` to `"2.3.5"` (increment the **last** dotted
    component when every component is an integer).
  - `"1"` to `"1.1"` (a single component gains a minor rather than becoming
    `"2"`, which would read as a major revision the author did not ask for).
  - Anything that does not parse (`"Draft A"`, empty, `NULL`) yields `""`,
    leaving the modal field empty so the author must type a version
    deliberately. Never guess at a non-numeric scheme.
- [ ] `dta_version_ignored_diff_keys()` — the predicate identifying the
  versioning machinery's own writes: the top-level `version` key and anything
  matching `^version_history(\.|$)`. These must never appear in a change
  summary; the summary would otherwise always lead with "changed version 1.0 to
  1.1", which is the header of the entry it is being written into.
  - Do **not** add these to `dta_metadata_machine_fields()`
    (`template_core.R:221`). That set is also what
    `dta_template_metadata_fields()` subtracts from, and a creation template
    legitimately sets `version`; widening it would silently make `version`
    un-settable from a template.
- [ ] `dta_version_change_line(row)` — render one diff row as
  `changed <key> ('<from>' -> '<to>')`, `added <key> ('<to>')`,
  `removed <key> ('<from>')`. Values come from the diff frame, which
  `dta_diff_display_value()` has already collapsed to a string.
- [ ] `dta_version_sanitise(x)` — collapse any CR, LF and pipe character in a
  value to a single space, then squash runs of whitespace. Called on every
  value that reaches the summary. This is what keeps the Markdown export's
  table intact (`R/exportDocuments.R:333-338`).
- [ ] `dta_version_change_summary(diff, note = "", max_items = 50)` — the whole
  string:
  - Leading counts: `"Metadata: 2 changed, 1 added. Datasets: 1 changed, 1 removed."`
    Sections with no changes are omitted; if nothing at all changed, return
    `"No changes recorded."` (non-empty — the DTAMetaData validator rejects an
    empty `changes`, `DTAMetaData-class.R:139`).
  - Then `" Details: "` and the per-key lines joined with `"; "`, metadata rows
    first, dataset rows after, each already in the differ's radix order.
  - If `note` is non-empty, it leads the string, followed by a dash separator.
  - If there are more than `max_items` detail rows, emit the first `max_items`
    and append `"; ... and <K> further change(s) not listed."` — the count is
    stated, never silently truncated.
- [ ] `dta_version_placeholder()` — the string a freshly opened entry carries
  until an export fills it in. Something explicit, e.g.
  `"(changes are summarised when the document is exported)"`. Must be non-empty.
- [ ] `dta_append_version_entry(dta, version, date, changes)` — returns
  `dta_try()`; appends `list(version = , date = , changes = )` to
  `metadata@version_history` and sets `metadata@version` to `version`. Mirrors
  the write in `rebase_apply()` (`template_diff.R:764-775`).
- [ ] `dta_set_version_entry_changes(dta, index, changes, version = NULL)` —
  returns `dta_try()`; rewrites entry `index`'s `changes`, and when `version`
  is supplied also re-syncs that entry's `version` to it. The re-sync matters
  because the author can edit `md_version` in the Metadata tab after creating
  the version, and the history entry must not drift from the document's own
  version field. A no-op (returns `dta` unchanged) when `index` is `NULL` or
  out of range.

**Files to create:** `inst/shiny/dta_app/R/versioning.R`

### Task 2 — The brandbar gate (UI)

- [ ] `inst/shiny/dta_app/R/ui_components.R`: add
  `create_new_version_button()` returning
  `actionButton("create_new_version", "Create new version", class = "brand-link brand-action")`
  — reusing the existing `.brand-link` pill styling so it sits on the same
  baseline as the switch it replaces.
- [ ] `inst/shiny/dta_app/R/ui_components.R`: give `edit_mode_switch()` a
  `value = FALSE` parameter so the server can re-render it in the on state
  after a version is created. Default unchanged, so the existing
  "off by default" test at `test-shinyapp-edit-mode.R` still passes.
- [ ] `inst/shiny/dta_app/app.R:26`: replace the direct `edit_mode_switch()`
  call in `brandbar` with `uiOutput("edit_gate", inline = TRUE)`.
- [ ] `inst/shiny/dta_app/R/theme.R`: add `.app-actions .brand-action`
  (an `actionButton` renders a `<button>`, not an `<a>`, so it needs
  background/border/font re-stated to match `.brand-link`) and give
  `.app-actions > .shiny-html-output` `display: flex; align-items: center;`
  so the wrapper `uiOutput` does not break the row's alignment.

**Files to modify:** `inst/shiny/dta_app/R/ui_components.R`,
`inst/shiny/dta_app/R/theme.R`, `inst/shiny/dta_app/app.R`

### Task 3 — Server state and the lock

- [ ] `app.R` `rv` (around **app.R:342-388**), new fields, each commented in the
  style of its neighbours:
  - `version_locked = FALSE` — TRUE while a loaded document has not yet had a
    new version created.
  - `version_baseline_yaml = NULL` — the document exactly as loaded; the left
    side of every change summary.
  - `version_entry_index = NULL` — index into `metadata@version_history` of the
    entry this session opened. Held as an index rather than looked up by
    version string, because the author may edit the version afterwards.
  - `version_note = ""` — the optional note typed in the modal.
  - `new_version_msg = NULL` — inline modal result, matching the
    `rv$add_ds_msg` convention.
- [ ] `app.R:409`: change the gate to
  `editing <- reactive(isTRUE(input$edit_mode) && !isTRUE(rv$version_locked))`.
  Add a comment explaining that this is what makes the lock real rather than
  cosmetic, and referencing the removed-control-keeps-its-value trap.
- [ ] `output$edit_gate` — renders `create_new_version_button()` when
  `rv$version_locked`, otherwise `edit_mode_switch(value = isTRUE(input$edit_mode))`.
  It must depend on `rv$version_locked` and **isolate** `input$edit_mode`, or
  flipping the switch re-renders the switch under the user's cursor.

**Files to modify:** `inst/shiny/dta_app/app.R`

### Task 4 — Arm the lock on the gated load paths

- [ ] `apply_loaded()` (**app.R:514-541**) gains a `versioned` argument
  (default `FALSE`). When `TRUE`: set `rv$version_locked <- TRUE`,
  `rv$version_baseline_yaml <- yaml_text`, `rv$version_entry_index <- NULL`,
  `rv$version_note <- ""`. When `FALSE`: clear all four.
- [ ] Pass `versioned = TRUE` from the file-upload observer (**app.R:831-857**)
  and the bundled-example loader (**app.R:859-904**).
- [ ] Leave the template-creation call (**app.R:1176**) at the default — a
  template-created document is new, not loaded, and stays directly editable.
- [ ] `confirm_reset` (**app.R:5826**) clears all four, like any other new
  document.
- [ ] Whenever the lock is armed, also call
  `bslib::update_switch("edit_mode", value = FALSE)`. Loading a second document
  after editing a first must not carry the previous `input$edit_mode == TRUE`
  forward. `bslib::update_switch()` is available — VERIFIED, bslib 0.12.0 is
  what is installed and it exports both `update_switch` and `toggle_switch`.

**Files to modify:** `inst/shiny/dta_app/app.R`

### Task 5 — Session persistence (restored sessions are gated)

- [ ] `autosave()` (**app.R:496-513**) writes the new fields into the saved list.
- [ ] `restore_session` (**app.R:6242-6294**) reads them back. Fallback for a
  session file written before this change (no such fields): treat the restored
  document as a gated load — `version_locked = TRUE`,
  `version_baseline_yaml = saved$yaml_text`, `version_entry_index = NULL`. The
  conservative default: an old session reopens read-only, which is the app's
  standing posture, rather than silently editable.
- [ ] Restoring also calls `bslib::update_switch("edit_mode", value = FALSE)`
  when the restored state is locked.

**Files to modify:** `inst/shiny/dta_app/app.R`

### Task 6 — The "Create new version" modal

- [ ] `observeEvent(input$create_new_version, ...)`: `req(rv$dta)`,
  `req(rv$version_locked)`, then `showModal()` with
  - the current version rendered as static text ("Current version: 1.0", or
    "This document has no version yet." when unset),
  - `textInput("new_version_value", "New version", value = dta_next_version(current))`,
  - `textAreaInput("new_version_note", "Note (optional)", ...)` — free text
    prepended to the generated summary,
  - Cancel / "Create version" buttons, and an inline message area fed by
    `rv$new_version_msg` for a rejected value.
- [ ] The modal body is built by a pure function in `ui_components.R`
  (`new_version_modal_body(current_version, suggested)`) so it can be asserted
  on without a server, exactly as `ds_edit_menu_item()` and friends are.
- [ ] `observeEvent(input$new_version_confirm, ...)`:
  - `req(rv$dta)`, `req(rv$version_locked)` — the guard holds even if the input
    is driven directly over the websocket after the modal is gone.
  - Reject a blank/whitespace version, and reject a version identical to the
    current one, with an inline message; the modal stays open and keeps what
    the author typed (the `rv$add_ds_token` lesson at
    `test-shinyapp-edit-mode.R` — do not bump a token that re-renders the body).
  - On success: `dta_append_version_entry(rv$dta, version, Sys.Date(), dta_version_placeholder())`,
    then `rv$dta <- res$value`,
    `rv$version_entry_index <- length(version_history)`,
    `rv$version_locked <- FALSE`, `rv$version_note <- note`,
    `rv$md_token <- rv$md_token + 1`, `sync_yaml_text()`,
    `bslib::update_switch("edit_mode", value = TRUE)`, `removeModal()`.
  - `metadata.date` is **not** touched — see Out of Scope.

**Files to modify:** `inst/shiny/dta_app/app.R`,
`inst/shiny/dta_app/R/ui_components.R`

### Task 7 — Preserve version and version history across a Raw YAML apply

- [ ] In the `apply_yaml` observer, before `rv$dta <- new_dta`: copy
  `version` and `version_history` from the live document's metadata onto
  `new_dta`'s metadata, so pasted YAML cannot overwrite either.
- [ ] Because the applied text then no longer matches the document, replace
  `rv$yaml_text <- txt` with a `sync_yaml_text()` call so the Raw tab shows the
  document that is actually loaded. Every other mutation in the app already
  re-serialises this way; leaving the author's text in place would show a
  version the document does not have.
- [ ] Comment the *why* at the call site: the version record is now owned by
  the versioning flow, and a paste is an edit to the specification, not a
  replacement of the document's identity.

**Files to modify:** `inst/shiny/dta_app/app.R`

### Task 8 — Fill in `changes` on export

- [ ] `export_dta()` in `app.R`: returns `isolate(rv$dta)` unchanged when
  `rv$version_entry_index` is `NULL`. Otherwise: re-parse
  `rv$version_baseline_yaml` with `dta_read_yaml_text()`, run
  `dta_diff(baseline, current)`, drop the ignored keys, build the summary with
  `dta_version_change_summary(diff, note = rv$version_note)`, and return
  `dta_set_version_entry_changes(dta, index, summary, version = <current metadata version>)$value`.
  Any failure (unparseable baseline, diff error) falls back to returning
  `rv$dta` untouched — an export must never be blocked by the summary.
- [ ] Route every **document** export through it. VERIFIED there are five
  independent sites, all reading `rv$dta` directly:
  - `output$dl_yaml` (**app.R:5452**) — full-DTA YAML.
  - `output$dl_docx` (**app.R:5481**).
  - the export modal's Markdown branch, its PDF branch (Markdown to PDF), its
    built-in Word branch and its custom-template Word branch
    (**app.R:5501-5748**), including each `dta_to_yaml_text()` call that embeds
    the YAML into the output.
- [ ] Leave alone: `output$dl_ds_yaml` (**app.R:5464**) — a single dataset
  carries no metadata, so it has no version history; and every validation
  export (**app.R:4253/4260/4270/4297/5794**) — those describe data, not the
  specification.

**Files to modify:** `inst/shiny/dta_app/app.R`

### Task 9 — Tests

New file `tests/testthat/test-shinyapp-versioning.R`, following the idiom of
`test-shinyapp-edit-mode.R`. Every test asserts behaviour, never existence.

Pure helpers, via `app_fn()` — no server:

- [ ] `dta_next_version()`: `"1.0"` to `"1.1"`, `"2.3.4"` to `"2.3.5"`, `"1"` to
  `"1.1"`, `"Draft A"` to `""`, `NULL` to `""`, empty to `""`.
- [ ] `dta_version_change_summary()` on a hand-built diff: counts line correct;
  detail present; `version`/`version_history.*` keys absent from the output;
  a value containing a newline or a pipe comes out with neither; `max_items`
  truncation states the remaining count; an empty diff yields a non-empty string.
- [ ] `dta_append_version_entry()` then re-reading the metadata succeeds — i.e.
  the placeholder satisfies the `changes` validator at
  `R/DTAMetaData-class.R:139`.
- [ ] `new_version_modal_body()` renders the current version and prefills the
  suggestion.

Server, via `testServer`:

- [ ] Loading the fixture leaves `rv$version_locked` TRUE, and
  `session$setInputs(edit_mode = TRUE)` does **not** make an edit land — e.g.
  `md_header` stays unchanged. This is the core guarantee.
- [ ] `output$edit_gate` renders `id="create_new_version"` and no
  `id="edit_mode"` while locked; after creating a version it renders
  `id="edit_mode"` and no button.
- [ ] Creating a version sets `metadata@version` to the typed value and appends
  exactly one `version_history` entry whose `changes` is the placeholder.
- [ ] After creating a version, `edit_mode = TRUE` makes an edit land.
- [ ] A blank version is refused: the document is untouched and
  `rv$new_version_msg` carries the error.
- [ ] Loading a **second** document re-arms the lock even though
  `input$edit_mode` is still TRUE from before — the regression guard for the
  removed-control-keeps-its-value trap.
- [ ] A template-created document is **not** locked (drive
  `input$template_create_confirm`, matching how
  `test-shinyapp-template-create.R` reaches this flow).
- [ ] Applying Raw YAML that names a different `version` and a different
  `version_history` leaves both as they were on the live document.
- [ ] `export_dta()`: after creating a version and changing `md_title`, the
  returned document's open entry's `changes` names `title` and its old and new
  values, and `rv$dta`'s own entry is still the placeholder (the finalisation
  is a pure read).
- [ ] `export_dta()` with no open version returns the document unchanged.
- [ ] Uploading a data file does **not** add anything to the summary — the
  guard for "the diff is specification-only".
- [ ] Round trip: `dta_to_yaml_text()` of the finalised document re-parses via
  `dta_read_yaml_text()` with the version history intact, and the summary
  string survives the YAML round trip byte for byte.

Export-side:

- [ ] Extend `tests/testthat/test-exportDocuments.R` — write a Markdown export
  of a document whose history entry carries a realistic generated summary and
  assert the version-history table still has one row per entry, i.e. the
  summary did not break `.df_to_md_table()`.

**Files to create:** `tests/testthat/test-shinyapp-versioning.R`
**Files to modify:** `tests/testthat/test-exportDocuments.R`

### Task 10 — Docs, changelog, manifest

- [ ] `CHANGELOG.md`: entry under `## [Unreleased]` / `### Added`, in the
  house style (prose that explains the why, not a bullet of file names).
- [ ] `DESCRIPTION`: bump `Version:` (user-facing change; 0.25.0 to 0.26.0).
- [ ] `vignettes/DTAtools.Rmd` / `README.md`: mention the gate only if the app
  walkthrough already describes Edit mode — check before editing.
- [ ] **`Rscript .github/scripts/bump_version.R --sync-manifest`** — mandatory,
  `inst/shiny/dta_app/` is changing and `manifest.json` carries per-file
  checksums. The `manifest-sync` CI workflow fails otherwise.
- [ ] `Rscript -e "styler::style_pkg()"` and, if any roxygen block changed,
  `Rscript -e "roxygen2::roxygenise()"` — the `r-style` workflow fails on a
  diff rather than fixing it. (No new exported R function is planned, so
  roxygen should be a no-op.)

**Files to modify:** `CHANGELOG.md`, `DESCRIPTION`,
`inst/shiny/dta_app/manifest.json` (generated — do not hand-edit)

## Success criteria

### Automated

- [ ] `Rscript -e "devtools::test(filter='shinyapp-versioning')"` — all pass.
- [ ] `Rscript -e "devtools::test(filter='shinyapp-edit-mode')"` — unchanged,
  still all pass (the `editing()` change touches every surface these cover).
- [ ] `Rscript -e "devtools::test(filter='shinyapp-template')"` — unchanged
  (`template_diff.R` is being reused, not modified).
- [ ] `Rscript -e "devtools::test()"` — no new failures anywhere.
- [ ] `Rscript -e "rcmdcheck::rcmdcheck(args='--no-manual')"` — 0 errors,
  0 warnings, and no new notes.
- [ ] `Rscript -e "styler::style_pkg()"` leaves no diff.
- [ ] `pre-commit run --all-files` passes (with R on `PATH` in the same
  command — see `CLAUDE.local.md`).
- [ ] `git diff --exit-code inst/shiny/dta_app/manifest.json` is clean after
  running `--sync-manifest`.

### Manual (in a running app)

- [ ] Upload `inst/extdata/clinical_dta.yaml`: the brandbar shows
  "Create new version" and no Edit-mode switch. Every editing surface is
  unreachable.
- [ ] Press it: the modal shows "Current version: ..." and prefills the next
  minor. Cancel leaves the document untouched and the button still showing.
- [ ] Confirm: the switch appears **on**, the Metadata tab shows the new
  version, and the Raw YAML tab shows the new `version_history` entry with the
  placeholder text.
- [ ] Edit the title and a column type, then export as YAML: the entry's
  `changes` names both, with counts first and the detail after.
- [ ] Export the same document as Markdown: the Version History table renders
  as a table, one row per entry — nothing spilled out of the cell. Repeat for
  DOCX.
- [ ] Load a second document after all of the above: the button is back and the
  switch is off.
- [ ] Create from a template: the switch is present immediately, no button.
- [ ] Paste YAML with a different `version:` and a different `version_history:`
  into the Raw tab and apply: the document keeps the version and history it
  had, and the rest of the pasted content lands.

## Risks (pre-mortem)

### Tigers

- **A removed control keeps its input value.** (HIGH)
  Swapping the switch out for a button does not reset `input$edit_mode`. If the
  lock were only cosmetic, loading a second document after editing a first
  would leave the app editable with no switch on screen.
  *Mitigation:* the lock lives inside `editing()` (Task 3), so it is real
  regardless of what is rendered; plus an explicit
  `bslib::update_switch(value = FALSE)` on every arm; plus the dedicated
  regression test in Task 9.

- **A multi-line `changes` breaks the Markdown export's table.** (HIGH)
  `.df_to_md_table()` (`R/exportDocuments.R:333-338`) puts the string straight
  into a pipe-delimited cell.
  *Mitigation:* `dta_version_sanitise()` (Task 1) plus the export-side test
  (Task 9). The alternative — teaching `.df_to_md_table()` to escape newlines
  and pipes — was rejected for now because it changes every table the exporter
  writes, which is a much wider blast radius than this feature needs; it is
  recorded as a follow-up.

- **The five independent export paths.** (MEDIUM)
  There is no single choke point; missing one means an export that ships the
  placeholder text. VERIFIED the full list is the five sites named in Task 8.
  *Mitigation:* Task 8 enumerates them explicitly, and the manual checklist
  exercises YAML, Markdown and DOCX separately.

- **A very large diff produces an unusable table cell.** (MEDIUM)
  A document whose datasets were substantially rewritten can produce hundreds
  of rows.
  *Mitigation:* the `max_items` cap in `dta_version_change_summary()`, which
  states the number of omitted changes rather than silently truncating.

### Elephants

- **`changes` is only correct at export time.** (MEDIUM)
  This is the chosen behaviour, but it means the app's own Raw YAML tab and the
  autosaved session both carry the placeholder while the author works. Someone
  reading the in-app YAML may reasonably think the feature is not working.
  *Mitigation:* make the placeholder say so in as many words
  (`dta_version_placeholder()`, Task 1). Worth revisiting if it confuses
  anyone in practice — moving to the debounced-live option later is a
  one-function change, since `export_dta()` already isolates the computation.

- **One new version per loaded document per session.** (LOW)
  Once unlocked, the button is gone, so an author cannot cut a second version
  without reloading. Nothing in the request asks for more; recorded in Out of
  Scope so it is a decision rather than an oversight.

- **A rebase also appends a version-history entry.** (LOW)
  `rebase_apply()` (`template_diff.R:764-775`) writes its own entry using the
  *template's* version number. A document that is both rebased and versioned in
  one session gets two entries whose `version` fields mean different things.
  Pre-existing behaviour, untouched here, but worth a look if the two flows are
  ever used together.

## Out of scope

- **`metadata.date` is not bumped.** The request names the version and the
  version history only, and the date field can legitimately be a
  transfer-agreement date rather than a "last edited" stamp.
- **No seed entry for the baseline version.** A document at 1.0 with an empty
  history gets one entry (for the new version). Inventing a "1.0 — initial
  version" record would be asserting something the app does not know.
- **No version-history editing UI.** The history is still not editable in the
  Metadata tab; it is written by this flow and by `rebase_apply()`, and read
  by the exporters.
- **Dataset-level three-way rebase**, unchanged from `template_diff.R`'s own
  scope note.
- **Escaping newlines in `.df_to_md_table()`** — recorded above as a follow-up.
- **Nothing new is exported from the package.** All of this lives in the Shiny
  app's helper environment, like `dta_diff()` itself.
