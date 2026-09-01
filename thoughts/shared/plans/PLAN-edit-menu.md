# Plan: replace "Create new version" with an "Edit" menu

Issue: https://github.com/Boehringer-Ingelheim/DTAtoolsR/issues/106
Branch: `ai/github-issue-106-plan-d0af53` (off `dev`, which is at `075da23` —
the same commit as this worktree's HEAD, verified). PR targets `dev`.

## Goal

The brandbar's "Create new version" button becomes an **Edit** menu offering
three ways into editing:

1. **Create new version** — the existing dialogue, unchanged.
2. **Edit current version** — unlock editing in place, recording nothing.
3. **Create new from current** — restart the specification at version `0.1`,
   discarding the version history.

The **"Edit mode" switch is removed entirely.** Editing is no longer a toggle
the user flips; it is a mode entered by choosing one of the menu items. While
it is active a **status tag** in the brandbar names what is being edited, and
the menu gains a **Stop editing** item to leave it again.

## Decisions taken (from the requirements interview)

| Question | Decision |
| --- | --- |
| What replaces the switch | Nothing interactive. The Edit menu stays visible at all times once a document is open; a read-only status tag appears beside it while editing. |
| "Create new from current" reset | `version` becomes `0.1`; the old history is discarded and replaced by **one seeded entry** recording where it came from. Title, date and template provenance untouched. |
| "Edit current version" record | Nothing. No history entry is opened, so an export writes no change summary. The menu item says so. |

## Technical choices

- **`rv$editing` replaces `input$edit_mode`.** Editing state moves from a
  client input to server-owned reactive state. This is not just tidiness: the
  prior feature's central trap was that *Shiny does not clear an input's value
  when its control leaves the DOM* (`app.R:461-466`, with a regression test for
  it in `test-shinyapp-versioning.R`). Four separate
  `bslib::update_switch("edit_mode", value = FALSE)` calls exist purely to work
  around that. A server-owned flag cannot go stale, so all four go away with it.

- **Reuse the `ds_edit_menu()` idiom rather than inventing a control.**
  `ui_components.R:99-196` already builds a Bootstrap dropdown whose rows are
  `actionLink(class = "dropdown-item ds-edit-item")` — an
  `<a class="action-button">`, which is exactly what Shiny's click binding and
  the app's own double-click guard
  (`GUARD_SELECTOR = '.action-button, .shiny-download-link'`,
  `ui_components.R:371`) both look for. Menu rows are therefore guarded and
  `testServer`-drivable for free, with no new bindings and no new JavaScript.

- **The "Create new version" input id does not change.** The menu row keeps
  `id = "create_new_version"`, exactly as `ds_edit_menu()` kept `edit_cols` /
  `edit_rules` when three buttons became one menu. Its observer
  (`app.R:490-504`), its modal, its confirm handler (`app.R:514-562`) and the
  24 test references to it are untouched by the move.

- **An empty `version_history` round-trips cleanly — but is not what we write.**
  VERIFIED: `as.list(DTAMetaData)` guards the key with
  `if (length(x@version_history) > 0)` (`R/DTAMetaData-helpers.R:61`), so an
  emptied history omits the key from the YAML rather than emitting
  `version_history: []`, and the class validator accepts a zero-length list
  (`R/DTAMetaData-class.R:122`). Both shapes are safe; per the interview we
  write a single seeded entry, so the new document's history reads truthfully
  from its first line instead of starting blank.

- **The seeded entry is closed, not open.** `rv$version_entry_index` stays
  `NULL` after "Create new from current", so an export does not overwrite the
  seed text ("Created from …") with a diff. This matches how a
  template-created document behaves: the template seeds its first entry and the
  app does not hold it open.

- **Bumping the version twice in one session finalises the first entry.**
  Because the menu is now reachable while already editing, "Create new version"
  can be chosen a second time. The change-summary computation that today lives
  inline in `export_dta()` (`app.R:5878-5895`) is extracted into a helper so the
  second bump can close the first entry with a real summary and re-baseline,
  instead of leaving entry #1 on the placeholder forever.

- **Status tag wording.** One shape for all three states, always naming the
  version being edited: `Editing v1.0`, `Editing v1.1 (new version)`,
  `Editing v0.1 (new document)`. Rendered as a non-interactive `<span>` with its
  own `.brand-status` class (deliberately *not* `.brand-link`, which would read
  as clickable) and `role="status"`. Alternative considered and rejected: three
  unrelated phrases ("Edit mode" / "New version" / "New document"), which makes
  the reader parse a different sentence per state.

## Current state analysis

### Key files

- `inst/shiny/dta_app/app.R:26` — the `uiOutput("edit_gate", inline = TRUE)`
  slot in the static `brandbar`.
- `app.R:388-391` — `rv$version_locked`, `version_baseline_yaml`,
  `version_entry_index`, `version_note`.
- `app.R:427` — `editing <- reactive(isTRUE(input$edit_mode) && !isTRUE(rv$version_locked))`,
  the single gate every editing surface `req()`s on.
- `app.R:437-445` — `observeEvent(editing(), ...)`, which closes open modals
  when editing stops.
- `app.R:447-482` — the `output$edit_gate` renderUI and its WHY comments.
- `app.R:490-504` / `506-512` / `514-562` — the "Create new version" observer,
  its inline message output, and its confirm handler.
- `app.R:651-654` — `autosave()` persisting the four version fields.
- `app.R:683-696` — `apply_loaded()` arming and clearing them.
- `app.R:5878-5895` — `export_dta()`, where the change summary is built.
- `app.R:6279-6284` — `confirm_reset`.
- `app.R:6451-6458` — the `suspendWhenHidden = FALSE` list, which already names
  `edit_gate`.
- `app.R:6762-6770` — `restore_session`.
- `inst/shiny/dta_app/R/ui_components.R:29-31` — `edit_mode_switch()`;
  `:41-47` — `create_new_version_button()`; `:64-81` — `new_version_modal_body()`;
  `:99-196` — `ds_edit_menu_item()` and `ds_edit_menu()`, the idiom to copy.
- `inst/shiny/dta_app/R/versioning.R:256-325` — `dta_version_placeholder()`,
  `dta_append_version_entry()`, `dta_set_version_entry_changes()`. The new
  helper mirrors these.
- `inst/shiny/dta_app/R/theme.R:80-160` — `.app-actions`, `.brand-link`,
  `.brand-action`, the `> .shiny-html-output` and `:empty` rules, and the
  `input_switch` block that goes away.
- `tests/testthat/helper-shinyapp.R:169-173` — `unlock_editing()`.
- `tests/testthat/test-shinyapp-ui-components.R:16-136` — the `render_html()` +
  `ds_edit_menu` assertions to copy for `edit_menu()`.

### What this changes about an existing guarantee

VERIFIED: today a document loaded from YAML **cannot be edited at all** until a
new version is created — `editing()` ANDs in `!rv$version_locked`, and that was
the entire point of the feature shipped in PR #102. "Edit current version"
deliberately relaxes that: a loaded document can now be edited with nothing
written to its history. This is the requested behaviour, but it is a real change
in posture and the changelog entry must say so plainly.

## Tasks

### Task 1 — Versioning helpers for the new document

`inst/shiny/dta_app/R/versioning.R`, following the commenting style of its
neighbours (say *why*, not *what*).

- [ ] `dta_new_document_seed(title, version)` — the seed entry's `changes` text.
  `"Created from <title> version <version>."` when both are present; degrade to
  `"Created from version <version>."`, `"Created from <title>."`, and finally
  `"Created as a new document."` when they are not. Every interpolated value
  passes through `dta_version_sanitise()` — the string lands in a Markdown table
  cell on export (`R/exportDocuments.R:333-338`), where a raw pipe splits the
  cell.
- [ ] `dta_restart_version_history(dta, version = "0.1", date = Sys.Date(), changes = NULL)`
  — returns `dta_try()`. Sets `metadata@version` to `version` and **replaces**
  `metadata@version_history` with a single entry
  `list(version =, date =, changes =)`. Rejects a blank version with the same
  message shape `dta_append_version_entry()` uses. When `changes` is `NULL` it
  builds the seed from the document's *current* title and version — read before
  either is overwritten.
- [ ] Extract the summary computation now inlined in `export_dta()` into
  `dta_version_finalise(dta, index, baseline_yaml, note)` — re-parse the
  baseline, `dta_diff()`, drop ignored keys, `dta_version_change_summary()`,
  `dta_set_version_entry_changes()`. Returns the document unchanged on any
  failure, exactly as `export_dta()` does today. Both `export_dta()` and the
  second-bump path in Task 5 call it, so there is one definition of "close this
  entry".

**Files to modify:** `inst/shiny/dta_app/R/versioning.R`

### Task 2 — The Edit menu and status tag (UI)

`inst/shiny/dta_app/R/ui_components.R`.

- [ ] `edit_menu(locked, editing, entry_open)` — a `div(class = "dropdown app-edit")`
  holding a `tags$button(id = "app_edit_toggle", class = "brand-action dropdown-toggle", 'data-bs-toggle' = "dropdown", ...)`
  labelled `✏️ Edit`, and a
  `tags$ul(class = "dropdown-menu dropdown-menu-end ds-edit-menu")`.
  `dropdown-menu-end` is required: the menu hangs off a control at the
  right-hand end of the brandbar and would otherwise open off the right edge of
  the viewport.
  Rows, all via the existing `ds_edit_menu_item()`:
  - `create_new_version` — "Create new version" / "Bump the version and record
    what you change." (id unchanged, see Technical choices).
  - `edit_current_version` — "Edit current version" / "Change this document in
    place. Not recorded in the version history." Offered only when `entry_open`
    is FALSE; with an entry already open this session it would silently abandon
    a summary the author is part-way through.
  - divider, then `create_new_document` — "Create new from current" / "Start a
    new specification at version 0.1, discarding this history.", with
    `class = "ds-edit-item-danger"` — the same treatment "Remove dataset"
    already carries, because this one throws away the history.
  - divider, then `stop_editing` — "Stop editing" / "Return to the read-only
    view." Rendered only when `editing` is TRUE.
- [ ] `edit_status_tag(version, kind)` — `span(class = "brand-status", role = "status", ...)`
  rendering `Editing v<version>`, plus `" (new version)"` for
  `kind == "new_version"` and `" (new document)"` for `"new_document"`. When the
  document has no version, render `Editing (no version set)` rather than an
  empty `v`.
- [ ] **Delete** `edit_mode_switch()` and `create_new_version_button()`. Both are
  dead once `output$edit_gate` renders the menu.
- [ ] `new_document_modal_body(current_title, current_version)` — the
  confirmation body: what will be discarded stated in prose, a
  `textInput("new_document_version", "Version", value = "0.1")`, and
  `uiOutput("new_document_msg")` for an inline rejection. A pure function, like
  `new_version_modal_body()` beside it, so it is assertable without a server.

**Files to modify:** `inst/shiny/dta_app/R/ui_components.R`

### Task 3 — CSS

`inst/shiny/dta_app/R/theme.R`.

- [ ] **Remove** the `input_switch` block (the `.form-group` /
  `.bslib-input-switch` rules that made the switch sit inline among the pills).
  Nothing renders that control any more.
- [ ] Keep `.app-actions .brand-action` — the menu toggle is still a `<button>`
  needing the pill look re-stated over Bootstrap's `.btn`. Add
  `.app-actions .dropdown-toggle::after` colour so the caret is visible on the
  dark bar.
- [ ] Add `.app-actions .brand-status`: the same pill geometry as `.brand-link`,
  but a filled, higher-contrast background and **no hover transition** — it is
  not a control and must not invite a click.
- [ ] Add `.app-actions .dropdown-menu { ... }` — the menu body renders on the
  light page below the dark bar, so it keeps Bootstrap's default light surface;
  only `z-index` (above the brandbar's `box-shadow` stacking) and a small
  `margin-top` are needed. VERIFIED `.app-brandbar` sets no `overflow`, so the
  menu is not clipped by it.
- [ ] Leave `.app-actions > .shiny-html-output { display: flex; align-items: center; }`
  and the `:empty { display: none; }` rule that follows it **exactly as they
  are, in that order** — `test-shinyapp-theme.R:153-154` asserts both strings
  and their relative source order, and the slot now holds two children (menu +
  tag), so it still needs to be a flex row.

**Files to modify:** `inst/shiny/dta_app/R/theme.R`

### Task 4 — Server state: retire `input$edit_mode`

`inst/shiny/dta_app/app.R`.

- [ ] `rv` (near **app.R:388**): add `editing = FALSE` (is the author in edit
  mode) and `edit_kind = NULL` (`"current"`, `"new_version"` or
  `"new_document"` — the status tag's wording only). Comment both in the style
  of their neighbours.
- [ ] **app.R:427**: `editing <- reactive(isTRUE(rv$editing))`. Replace the WHY
  comment above it: the removed-control-keeps-its-value trap it describes no
  longer exists, because the state is no longer an input. Say that, and say what
  replaced the hard version lock — `rv$version_locked` survives but now only
  decides which menu rows are offered, not whether editing is possible.
- [ ] **app.R:473-482** `output$edit_gate`: when `rv$structure` is NULL render
  NULL (unchanged — the landing page has nothing to edit); otherwise render
  `tagList(edit_menu(...), if (isTRUE(rv$editing)) edit_status_tag(...))`. It
  depends on `rv$doc_token`, `rv$version_locked`, `rv$editing`, `rv$edit_kind`
  and the document's current version. The `isolate()` on `input$edit_mode` goes
  away with the input; keep the `isolate()` on `rv$structure` and its comment.
  **Trap to avoid:** the version shown in the tag must be read in a way that
  does not make this output rebuild on every metadata keystroke. Read it from
  `rv$dta` under `isolate()`, gated on the existing `rv$md_token`, which is
  already bumped by the paths that change the version.
- [ ] Delete all four `bslib::update_switch("edit_mode", ...)` calls
  (**app.R:561, 696, 6284, 6770**) and the comments explaining them.

**Files to modify:** `inst/shiny/dta_app/app.R`

### Task 5 — The three actions

`inst/shiny/dta_app/app.R`.

- [ ] `observeEvent(input$create_new_version, ...)` (**app.R:490**): drop
  `req(rv$version_locked)` — the menu now offers this while already editing.
- [ ] `observeEvent(input$new_version_confirm, ...)` (**app.R:514**): drop
  `req(rv$version_locked)`; replace `bslib::update_switch("edit_mode", value = TRUE)`
  with `rv$editing <- TRUE; rv$edit_kind <- "new_version"`. Before appending, if
  `rv$version_entry_index` is already set, close the open entry first with
  `dta_version_finalise()` and reset `rv$version_baseline_yaml` to
  `dta_to_yaml_text()` of the document as it now stands, so the new entry's
  summary spans from this bump rather than from the original load.
- [ ] New `observeEvent(input$edit_current_version, ...)`: `req(rv$dta)`, and
  `req(is.null(rv$version_entry_index))` — the guard holds even if the input is
  driven over the websocket while the row is not rendered. Sets
  `rv$version_locked <- FALSE`, `rv$editing <- TRUE`, `rv$edit_kind <- "current"`,
  and clears `version_baseline_yaml` / `version_entry_index` / `version_note`.
  Nothing is written to the document.
- [ ] New `observeEvent(input$create_new_document, ...)`: `req(rv$dta)`,
  `showModal()` with `new_document_modal_body()`, Cancel and a
  `create_new_document_confirm` primary button. Inline errors via
  `rv$new_document_msg` and `output$new_document_msg`, mirroring
  `rv$new_version_msg` (**app.R:506-512**).
- [ ] New `observeEvent(input$create_new_document_confirm, ...)`: reject a blank
  version inline (the modal stays open, keeping what was typed). On success call
  `dta_restart_version_history()`, then `rv$dta <- res$value`,
  `rv$version_locked <- FALSE`, `rv$version_entry_index <- NULL`,
  `rv$version_baseline_yaml <- NULL`, `rv$version_note <- ""`,
  `rv$editing <- TRUE`, `rv$edit_kind <- "new_document"`,
  `rv$md_token <- rv$md_token + 1`, `sync_yaml_text()`, `removeModal()`.
- [ ] New `observeEvent(input$stop_editing, ...)`: `rv$editing <- FALSE`,
  `rv$edit_kind <- NULL`. Leaves every version field alone, so re-entering edit
  mode resumes the same open entry. The existing `observeEvent(editing(), ...)`
  (**app.R:437-445**) already closes open modals when this flips.

**Files to modify:** `inst/shiny/dta_app/app.R`

### Task 6 — Load, reset, template, autosave, restore

`inst/shiny/dta_app/app.R`.

- [ ] `apply_loaded()` (**app.R:683-696**): set `rv$editing <- FALSE`,
  `rv$edit_kind <- NULL` where the `update_switch` call was. A newly loaded
  document is never mid-edit.
- [ ] Template creation (the `template_create_confirm` observer): set
  `rv$editing <- TRUE`, `rv$edit_kind <- "current"`. **Deliberate behaviour
  change** — today a template-created document is unlocked but still needs the
  switch flipped. With no switch to flip, leaving it non-editing would strand
  the author in a read-only view of a document they just created.
- [ ] `confirm_reset` (**app.R:6279-6284**): clear `editing` and `edit_kind`
  alongside the version fields.
- [ ] `autosave()` (**app.R:651-654**): persist `editing` and `edit_kind`.
- [ ] `restore_session` (**app.R:6762-6770**): read them back. A session file
  written before this change carries neither — restore as **not editing**, which
  is the app's standing conservative posture and matches how the same function
  already defaults `version_locked` to TRUE.

**Files to modify:** `inst/shiny/dta_app/app.R`

### Task 7 — Test migration

The bulk of the work. VERIFIED counts: `edit_mode` appears **111 times across 6
files**; `unlock_editing()` at **65 call sites**.

- [ ] `tests/testthat/helper-shinyapp.R:169-173`: `unlock_editing(session, version)`
  keeps its name and signature but now drives `create_new_version` →
  `new_version_value` → `new_version_confirm` and no longer needs a preceding
  `edit_mode = TRUE`. Add siblings `enter_edit_mode(session)`
  (`session$setInputs(edit_current_version = 1)`) and `leave_edit_mode(session)`
  (`stop_editing = 1`).
- [ ] Sweep every `session$setInputs(edit_mode = TRUE)`: delete it where
  `unlock_editing()` follows (redundant), otherwise replace with
  `enter_edit_mode(session)`. Replace `edit_mode = FALSE` with
  `leave_edit_mode(session)`. Files: `test-shinyapp-server.R` (40),
  `test-shinyapp-edit-mode.R` (33), `test-shinyapp-versioning.R` (24),
  `test-shinyapp-file-dataset-server.R` (9),
  `test-shinyapp-dataset-add-remove.R` (4).
  These fail **loudly** rather than silently if missed — `testServer` accepts an
  unknown input, but `editing()` then stays FALSE and every downstream assertion
  fails.
- [ ] `test-shinyapp-versioning.R`: the assertions on `output$edit_gate`
  currently look for `id="create_new_version"` / `id="edit_mode"`. Rewrite to
  assert the menu's rows and the status tag.
- [ ] `test-shinyapp-ui-components.R`: delete the `edit_mode_switch()` /
  `create_new_version_button()` assertions if present; add `edit_menu()` and
  `edit_status_tag()` assertions in the style of the `ds_edit_menu` block at
  lines 16-136, using the same `render_html()` helper.
- [ ] `test-shinyapp-theme.R`: drop any assertion on the removed `input_switch`
  CSS; add one for `.brand-status`. **Do not touch** the `:empty`-ordering test
  at lines 153-154.

**Files to modify:** `tests/testthat/helper-shinyapp.R`,
`test-shinyapp-server.R`, `test-shinyapp-edit-mode.R`,
`test-shinyapp-versioning.R`, `test-shinyapp-file-dataset-server.R`,
`test-shinyapp-dataset-add-remove.R`, `test-shinyapp-ui-components.R`,
`test-shinyapp-theme.R`

### Task 8 — New tests

- [ ] Pure, via `app_fn()`: `dta_restart_version_history()` sets the version and
  leaves **exactly one** history entry; the entry satisfies the `DTAMetaData`
  validator (re-read the metadata); a blank version is refused; a title
  containing a pipe or a newline comes out sanitised.
  `dta_new_document_seed()` degrades correctly through all four shapes.
- [ ] Pure: `edit_menu()` renders `create_new_version`, `edit_current_version`,
  `create_new_document`; omits `stop_editing` unless `editing`; omits
  `edit_current_version` when `entry_open`; carries `dropdown-menu-end`.
  `edit_status_tag()` renders each of the three wordings and the no-version
  fallback.
- [ ] Server: loading a fixture leaves `editing()` FALSE and an edit does not
  land; `edit_current_version` makes it land **without** adding a history entry
  and **without** changing `metadata@version` — the core new guarantee.
- [ ] Server: `create_new_document_confirm` sets `version` to `0.1`, leaves one
  history entry naming the old title and version, and leaves the datasets
  untouched (assert a dataset name survives).
- [ ] Server: after "Create new from current", `export_dta()` returns the
  document with its seeded `changes` **intact** — the entry is closed, not open.
- [ ] Server: two "Create new version" bumps in one session leave two entries,
  the first carrying a real summary rather than the placeholder.
- [ ] Server: `stop_editing` makes `editing()` FALSE and an edit stop landing;
  re-entering resumes the same `version_entry_index`.
- [ ] Server: a restored session written without the new fields comes back **not
  editing**.
- [ ] Round trip: `dta_to_yaml_text()` of a restarted document re-parses with one
  history entry and version `0.1`.

**Files to modify:** `tests/testthat/test-shinyapp-versioning.R`,
`tests/testthat/test-shinyapp-ui-components.R`

### Task 9 — Changelog, manifest, style

- [ ] `CHANGELOG.md`: **rewrite the existing `## [Unreleased]` → `### Fixed`
  entry** about the Edit-mode switch on the landing page. It describes a control
  this change deletes ("a loaded document still offers 'Create new version'
  until it is versioned, and the switch after that"), and it has not been
  released, so leaving it would ship a paragraph about a switch that does not
  exist. Fold it into one new `### Changed` entry covering the Edit menu, and
  say plainly that a loaded document can now be edited without creating a
  version.
- [ ] `Rscript .github/scripts/bump_version.R --sync-manifest` — **mandatory**,
  `inst/shiny/dta_app/` changes and `manifest.json` carries per-file checksums.
  The `manifest-sync` workflow rewrites it and pushes otherwise, which moves the
  PR head and re-gates every check as `action_required`.
- [ ] `Rscript .github/scripts/style.R` — not `styler::style_pkg()`; the script
  also covers `inst/` and repairs the CRLF styler leaves on Windows.
- [ ] **Do not bump `Version:`.** No roxygen blocks change (nothing here is
  exported), so `roxygenise()` should be a no-op — run it only if that turns out
  to be false.

**Files to modify:** `CHANGELOG.md`, `inst/shiny/dta_app/manifest.json`
(generated — do not hand-edit)

## Success criteria

### Automated

- [ ] `Rscript -e "devtools::test()"` — full suite green, 0 failures.
- [ ] `Rscript -e "devtools::test(filter='shinyapp')"` — the migrated files in
  particular.
- [ ] `Rscript .github/scripts/style.R --check` — clean.
- [ ] `Rscript -e "rcmdcheck::rcmdcheck(args='--no-manual')"` — 0 errors,
  0 warnings, and **0 skips** (this machine has TinyTeX and pandoc; a skip means
  the environment regressed, not the code).
- [ ] `grep -rn "edit_mode" inst/ tests/` returns nothing but comments.
- [ ] `git diff --stat` shows `manifest.json` regenerated by the script, not by
  hand.

### Manual (browser)

- [ ] Landing page: the brandbar shows **no** Edit control and **no gap** before
  "Report issues" — the `:empty` rule still applies with the new two-child slot.
- [ ] Load a bundled example: the Edit menu appears, opens **downward and
  right-aligned**, is not clipped by the brandbar, and sits above the page below
  it. This is the one thing `testServer` cannot check — the menu has only ever
  been used in the light content area, never on the dark bar.
- [ ] Each of the three items enters edit mode and shows the right status tag.
- [ ] "Stop editing" returns the read-only view and the tag disappears.
- [ ] "Create new from current" shows the confirmation before discarding
  anything; cancelling leaves the history intact.
- [ ] Export after "Edit current version" writes no change summary; export after
  "Create new version" does.
- [ ] Double-clicking a menu row fires the action once (the existing guard covers
  `.action-button`, which is what these rows are).

## Out of scope

- Any change to how the change summary itself is computed
  (`dta_version_change_summary()` and the differ are untouched).
- Restoring the "loaded documents cannot be edited unversioned" guarantee —
  relaxing it is the point of this issue.
- Resetting `metadata.title` or `metadata.date` in "Create new from current"
  (decided in the interview: the author renames it themselves).
- Surfacing the version history anywhere in the UI — still no history view.
- A version bump or release.

## Risks (Pre-Mortem)

### Tigers

- **The test migration is the change** (HIGH). 111 `edit_mode` references and 65
  `unlock_editing()` sites across six files dwarf the app change itself. A missed
  site fails loudly rather than silently, but the volume invites a careless
  sweep.
  - Mitigation: do Task 7 as its own commit, before Task 8's new tests, and
    verify with `grep -rn "edit_mode" tests/` returning nothing.

- **A Bootstrap dropdown has never been rendered in the brandbar** (MEDIUM).
  `ds_edit_menu()` proves the idiom works, but only in the light content area.
  The brandbar is a `display: flex` row with `margin-left: auto`, a `box-shadow`,
  and a wrapper the CSS forces to `display: flex`. Popper positioning, right-edge
  overflow and stacking above the page are all plausible failure points, and none
  of them is visible to `testServer`.
  - Mitigation: `dropdown-menu-end` from the start; the browser check is a named
    success criterion, not an afterthought.

- **Removing `input$edit_mode` removes the app's only client-side editing gate**
  (MEDIUM). Every editing surface already re-checks `req(editing())`
  server-side (two-layer enforcement, VERIFIED), so the guarantee survives — but
  the audit that confirms every surface calls `editing()` and not
  `input$edit_mode` directly must actually be run, not assumed.
  - Mitigation: the `grep -rn "edit_mode" inst/` success criterion is exactly
    that audit.

### Elephants

- **"Edit current version" quietly weakens the version discipline this app was
  given last month** (MEDIUM). PR #102 existed to stop a loaded specification
  being edited without a version record; this issue adds a one-click route around
  it. That is Tom's call and the interview confirmed it, but for a
  clinical-data-transfer specification the absence of a record is the kind of
  thing an auditor asks about later. The menu row says so, the changelog should
  too, and if the answer is ever "we should have logged that", the seam is
  `rv$edit_kind == "current"`.

- **The status tag replaces a control with a label** (LOW). The switch made edit
  mode feel reversible at a glance; a tag does not. "Stop editing" in the menu is
  the replacement, and it is one click deeper than the switch was. Worth watching
  whether users find it.
