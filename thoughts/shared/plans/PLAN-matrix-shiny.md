# PLAN — Matrix datasets in the Shiny app

Branch `ai/matrix-shiny-a71aa9` (worktree `sas7bdat-file-reading-c193cb`), cut
from `ai/matrix-dataset` @ `890a2cf` (PR #205 → `dev`). Copied to
`thoughts/shared/plans/PLAN-matrix-shiny.md` once approved. Line numbers are on
`890a2cf`; `W` = `inst/shiny/dta_app`.

Status: **r2**: reviewed twice before implementation (a design review, and an
empirical review that rebuilt the plan's assumptions in R), user decisions
D1–D4 (§10). §12 records what the review of the implementation found.

## 0. Context

`type: matrix` (PR #205) works everywhere in the package and in the app's data
paths, but its plan scoped the **app editors** out of v1, and nobody told the
user. The user found that **Add dataset cannot create a matrix**. The audit
shows this is one symptom: a matrix opened in the app has **no editor at all**.
The Columns and Rules editors are hidden for it, and there is no editor for the
`matrix:` block, so anything beyond ID/name/description/files can be changed
only in Raw YAML. Goal: a matrix can be created, edited, loaded, validated,
inspected and exported from the app as a tabular dataset can. The editors keep
every matrix invariant, and every default fails loudly instead of passing
hollow. Load failures become readable, stay on screen and are recorded in the
dataset's error log (user decision D3).

## 1. Audit — every app feature that touches a dataset, judged for a matrix

Method:
- Five parallel read-only audits covered:
  - editors and the dataset model
  - upload and preview
  - results and export
  - templates, Raw YAML, session and help
  - a function-by-function sweep of `utils_dta.R`
- I then re-read every **broken**/**missing** claim myself: ✔ means verified
  by reading the code, ⚗ means probed by running it.
- The design reviewer spot-checked #1, #3–#5 and #18 independently.
- Rows marked neither ✔ nor ⚗ are the audit's readings of the code they cite.

| # | Area | Feature | Site | Verdict | Reason |
|---|---|---|---|---|---|
| 1 | Add | Type choice in the dialog | `W/app.R:2919-2926` | **missing** ✔ | Offers Tabular and Files only; the hint describes two types |
| 2 | Add | `dta_add_dataset()` | `W/R/utils_dta.R:3117-3154` | **broken** ✔ | `type %in% c("tabular","file")` refuses `"matrix"`; there is no `DTADataSetMatrix` branch |
| 3 | Editor | Edit menu | `W/R/ui_components.R:269-316` (gate `:288`) | **missing** ✔ | Columns and Rules appear only for `"tabular"`; there is no matrix-settings item |
| 4 | Editor | Editor guards | `W/app.R:5252, 5813, 6370, 6863` | **missing** ✔ | `req(identical(type, "tabular"))` blocks both editors |
| 5 | Editor | Row labels: add/edit/rename/delete/reorder | `W/R/utils_dta.R:3261-3393` | **broken if exposed** ✔ | Assigning `ds@specs` keeps the subclass, but: <br>• renaming the key aborts (`row_key` is always explicit, constructor `R/DTADataSetMatrix-class.R:118`) <br>• adding or removing a label with a list corner aborts (length ≠ k, `:165`) <br>• removing the key aborts with a raw validator message <br>• reordering passes but leaves list-corner entries on the wrong labels |
| 6 | Editor | Matrix settings (header, rows, cells) | whole app | **missing** ✔ | No UI for `ds@matrix`; Raw YAML only |
| 7 | Editor | Rules on a matrix | `W/R/utils_dta.R:3478-3536` | **missing** (helpers would work) | Column choices come from `@specs`, which holds the row labels — correct. A `.matrix:` id is refused only by the raw validator (`:191-198`) |
| 8 | Editor | Generated `.matrix:row_key` rule shown | whole app | **missing** | Built at check time, so no list shows it |
| 9 | Editor | Details (id/name/description/template) | `W/R/utils_dta.R:3054-3089` | works | `@<-` on the object keeps the class, `@matrix` and `@heads` |
| 10 | Editor | Files: type choices / save | `W/R/utils_dta.R:3558-3566, 3695-3822` | works ✔ | csv/tsv only, re-validated on save (REQ-APP-039) |
| 11 | Editor | Files: remove/move a handler | `W/R/utils_dta.R:3829-3881` | **broken** ✔ | `@heads[[t]]$handler` indexes `@files` (`R/DTADataSetMatrix-class.R:632`) and is not remapped. A lazily bound table re-reads its head through the wrong handler (`:570`) and fails falsely with "the file changed" |
| 12 | Editor | Type change tabular ↔ matrix | `W/R/utils_dta.R:3099-3105` | n.a. by design (D4) | The type is fixed once created; Raw YAML can change it under REQ-APP-036 |
| 13 | Editor | Remove dataset | `W/R/utils_dta.R:3164` | works | By name |
| 14 | Sidebar | Type chip, dataset status line | `W/app.R:9583, 9600` | works | Prints `s$type`, so it reads "matrix" |
| 15 | Sidebar | Structure / counts | `W/app.R:1616-1656` | works | `build_structure()` holds label, type and handlers, with no type branch |
| 16 | Upload | Loose file into a slot, overwrite, reload | `W/app.R:3085-3215` | works | Goes through `load_file()`, which refreshes `@heads` |
| 17 | Upload | Container drop, delivery routing, container manager | `W/app.R:3304-3345, 3943+`; `R/loadDelivery.R:225`; `W/R/containers.R` | works | `inherits(ds, DTADataSetTabular)`; the container code has no type test |
| 18 | Upload | Gate failure message (wrong corner, delimiter, preamble) | `W/app.R:3194-3197, 3270-3285`; deliveries `3316, 3378` | **broken (readability)** ✔ | The full cli message, with its bullets joined by `\n`, goes into a 10-second `showNotification()`. The breaks collapse, the notice vanishes and nothing is recorded. This affects all types; the matrix hints are the multi-line ones |
| 19 | Upload | Delivery-plan reason | `R/loadDelivery.R:271, 572` | works | The full text is in the Note column of the Delivery view |
| 20 | Upload | Unload one / all tables | `W/R/utils_dta.R:1243-1324` | works | `@heads` dropped (REQ-APP-038) |
| 21 | Upload | Transfer on Raw-YAML apply | `W/R/utils_dta.R:4071-4100`; `W/app.R:8881` | **broken** ⚗ | Heads are carried and a type change drops data (REQ-APP-036), but `@heads$handler` is copied **without remapping**. Reordering `files:` makes a lazily bound table fail falsely with "the file changed" (probed) |
| 22 | Preview | Data grid for 20 k columns | whole app | n.a. | The app has no data preview for any type. `DT` renders only spec tables, messages (`pageLength = 8`, `scrollX`) and deliveries |
| 23 | Preview | Inspect modal width | `W/app.R:7454, 7525` | works ⚗ | A cell finding's `inspect()` has 17 columns, bounded by the finding, not the table |
| 24 | Results | Messages dock | `W/app.R:7134, 7292-7351` | works ✔ | Generic columns; NA row and column render as empty; nothing groups by column |
| 25 | Results | Status chips / counts | `W/R/utils_dta.R:1002-1184` | works | Read from `results()` and `validation_status()` |
| 26 | Results | Inspect: `minimum`/`maximum` | `W/app.R:7510-7511` | works ✔ | REQ-APP-040 |
| 27 | Results | Inspect: `corner`, `propertyNames`, `uniqueNames`, `min/maxProperties`, `min/maxItems`, `finite` | `W/app.R:7484-7521, 7713-7730` | **broken (wording)** ⚗ | Count findings carry `columnspec_data` = actual and `columnspec_columnspec` = bound, with no column. The panel shows a bare "3" with no unit or location, and "Should be" falls back to the raw message |
| 28 | Results | Inspect: `.matrix:row_key` | `W/app.R:7568-7600` | **broken (wording)** ⚗ | Looked up in `ds@specs@rules`, so it shows no type or wording. The package's `dta_find_rule_def()` (`R/validationReporting.R:575`) resolves it |
| 29 | Results | Per-check column-spec summary | whole app | n.a. | Not rendered for any type; it lives in the HTML report |
| 30 | Results | Stale/missing head ("load it again") | `R/DTADataSetMatrix-class.R:554-564` | works | A table failure in `messages()` (REQ-APP-037) |
| 31 | Results | HTML validation report | `W/app.R:7413-7431` | works | `write_validation_report()` |
| 32 | Export | md/docx/pdf, whole document and single dataset | `W/R/utils_dta.R:1397-1416, 2049-2092` | works | All go through `write_dta()`, which writes "Matrix layout" and "Row label columns" (confirmed in the browser, §6) |
| 33 | Templates | Create from template, slots, `values_from` in `row_labels` | `W/R/vocabulary.R:673-711, 895-1048`; `W/R/dataset_template.R:227-370` | works | `dta_dataset_spec_key()` |
| 34 | Templates | Inheritance merge | `W/R/template_inherit.R:43-80, 442-500` | works | `row_labels` is a sequence like `columns`; `matrix:` merges as a map |
| 35 | Templates | Diff / history | `W/R/template_diff.R:185`; `W/R/template_history.R:173-195` | works | Done in PR #205 |
| 36 | YAML | Raw YAML apply, spec-check panel | `W/R/utils_dta.R:229-264`; `W/app.R:1697, 8774, 10203` | works | Package readers; lint display does not depend on the finding kind |
| 37 | Session | Save / restore, streamed tables included | `W/R/utils_dta.R:4125-4254` | works ⚗ | `@heads`/`@matrix` travel on the object (REQ-APP-034); `write_ipc_file()` accepts an Arrow `Dataset` (probed), so a streamed table is kept |
| 38 | Onboarding | Example loader | `W/R/utils_dta.R:517-575` | works, poorly placed ✔ | `matrix_dta.yaml` is listed last, outside the teaching order; its data files are in the slot example picker |
| 39 | Help | Hints, stale comments | `W/app.R:2925, 9979`; `W/R/ui_components.R:247-256`; `W/R/dataset_template.R:35` | **missing** | No hint mentions matrix; the comments name two types |
| 40 | Editor | Column form fields, vocabulary picker for a row label | `W/app.R:5396-5850` | works | Row labels are ordinary column specs; the picker writes plain `values` |
| 41 | Shiny | Click guard on new buttons | `W/R/ui_components.R:491` | works | Matches `.action-button` by class: new buttons are covered without registration |

**Summary**
- 1 broken function: #2
- 3 missing editors: #3/#4, #6 and #8
- 1 latent invariant hazard the editors would expose: #5
- 1 real data bug, reachable two ways: #11, #21
- 3 wording or readability defects: #18, #27 and #28
- help text: #1, #38 and #39
- No crash and no hollow pass today

## 2. Design

### 2.1 Add dataset → Matrix (`#1`, `#2`; D2: no file entry)

- **Type choice.** The radio offers **Tabular / Matrix / Files**. The hint gains:
  "A **Matrix** dataset is a table whose data columns are not listed, such as
  gene counts: you describe the row-label columns, the header and what every
  cell may hold."
- **Required fields.** A `conditionalPanel("input.add_ds_type == 'matrix'")`
  holds three of them, each `radioButtons(selected = character(0))`, so nothing
  is preselected and there is no hollow default:
  - `add_ds_mx_label`: "First row-label column ID".
  - `add_ds_mx_corner`: one of
    - `named`: the row-label ID
    - `blank`: Empty — R `write.csv`, pandas
    - `none`: No field — R `write.table`; written as `[null]`
    - `text`: Other text, with `add_ds_mx_corner_text`
  - `add_ds_mx_cells`: SAS Num / SAS Int / SAS Char.
- **Save.** `add_ds_save` passes the matrix arguments **only when
  `type == "matrix"`**. Hidden conditional inputs still report their values, so
  a user who switched back to Tabular must not get them.
- **`dta_add_dataset(dta, id, type, name, description, row_label = NULL,
  corner = NULL, corner_text = NULL, cell_type = NULL)`**:
  - accepts `dta_supported_types()` and ignores the matrix arguments for other
    types;
  - for a matrix, refuses each of these with a plain sentence:
    - a missing or whitespace ID
    - a missing corner or cell type
    - empty Other text ("choose Empty")
    - the text `named`/`blank` (YAML reads it as the keyword; "choose The
      row-label ID")
  - builds `DTAtools::DTADataSetMatrix(id, specs =
    DTAColumnSpecCollection(columns = list(<id> = DTAColumnSpec(id, type =
    "SAS Char", nullable = FALSE))), matrix = list(header = list(corner = …),
    cells = list(type = …, nullable = FALSE)), name, description)`.
- **Defaults** are the package's: data columns ≥ 1, rows ≥ 1, Inf rejected for
  Num, key unique and non-missing. Cell `nullable = FALSE` is passed
  explicitly. The constructor would leave it unset, which is not written to the
  YAML; probed: it means the same, but the specification should say so.
- **Modal pattern.** As today, with the token bumped on open. A failed save does
  not re-render, so the typed values stay. The body render is tested after a
  cancel and reopen (§4).

### 2.2 Row labels = the Columns editor, invariant-safe (`#3`–`#5`; D1: auto-align)

- **Menu.** `ds_edit_menu("matrix")`:
  - **Row labels** (`edit_cols`): "Columns that identify and describe each
    row, by position"
  - **Rules** (`edit_rules`)
  - **Matrix layout** (`edit_matrix`, new): "Header, dimensions and cell values"
  - Files, Details and Remove, as before
- **Gates.** The four guards use `type %in% c("tabular", "matrix")`.
- **Wording.** For a matrix, every user-facing string in the column-modal path
  says "row label":
  - title, add button, remove confirm and save/remove notifications
  - the hint "Row labels are matched to the file's leading columns by position,
    so order matters. The row key is set under Matrix layout." The key is
    always explicit (constructor `:118`), so it does not follow position.
  - It covers every string in the path, `W/app.R:5240-5850`.
- **Invariants in the helpers** (`utils_dta.R`), which every caller goes
  through. Each sets `specs` and `matrix` in one `S7::set_props()`, so the
  validator sees one consistent object:

  | Operation | Matrix rule |
  |---|---|
  | rename (`dta_set_column`, old ≠ new) | an id in `row_key` is renamed **in place** (a composite key keeps its order), like the rule carry-over |
  | add (`dta_set_column`, new id), list corner | appends `""` (strict: the file must have a blank there). The app shows a **warning** that persists: "Its header text above the column is empty; set it under Matrix layout." |
  | remove (`dta_remove_column`) | refused when the id is in `row_key` ("choose another key under Matrix layout first") or is the last label. With a list corner, the entry is dropped, **refused** at position 1 with a `null` entry |
  | move (`dta_move_column`), list corner | the two entries swap; **refused** if either is `null` ("the first row label has no header field above it; it must stay first"). `[null, a, b]` moving `b` up is legal |
  | key column `nullable: Yes` | pre-checked with a plain sentence |

  `named`/`blank` corners follow the ids and the count by definition. A move
  never changes the key, because it is held by id.

  - **Named corner.** With a `named` corner, a rename changes the expected
    header text, so a table that is already loaded will ask to be loaded again.
    The save notice says so.
  - **Keyword trap.** A list corner left with **one** entry whose text is
    `named` or `blank` would read back from YAML as that keyword: probed, the
    text `"blank"` becomes an empty header after a session restore. Any
    operation that would leave it is refused: a remove, the Matrix layout save,
    or the Add dialog. The refusal says "change the corner under Matrix layout
    first". One helper does the check.

### 2.3 Rules (`#7`, `#8`)

- The gates are opened (§2.2). `dta_set_rule` pre-checks a `.matrix:` id on a
  matrix with a plain sentence.
- Under the rules table, a read-only line for a matrix: "Also checked: the row
  key (`GENE_ID`) must be unique — rule `.matrix:row_key`, generated from Matrix
  layout."

### 2.4 Matrix layout editor (`#6`) — a new modal, same pattern as Details

**Open.** `observeEvent(input$edit_matrix)`:
- guards on `editing()`, `rv$active` and type `matrix`;
- prefills `dta_matrix_fields(dta, ds)`;
- shows `modalDialog("Matrix layout — X", uiOutput("mx_modal_body"))`;
- the body renders **from the prefill only**, on a token, as
  `meta_modal_body` does (`W/app.R:7001`).

**Stale inputs** (the modal-cache trap: a cached copy binds first and reports
its old inputs). Three defences:
- `outputOptions(output, "mx_modal_body", suspendWhenHidden = FALSE)`.
- No observer reacts to any `mx_*` input except `mx_save`, so a stale report in
  that window changes nothing.
- The save reads only `mx_corner_1..k` for the current k; a leftover
  `mx_corner_3` from a 3-label matrix is ignored.

Verified in the browser by opening the layouts of the two bundled matrices in
turn and saving the second unchanged.

**Header**
- Corner: the row-label IDs / Empty / Exact text per row label.
  - In list mode: one text input per label ("Above `GENE_ID`").
  - Also in list mode: "The header has no field above the first row label
    (R `write.table`)", which makes entry 1 `null`.
- Label, description.
- Data labels must (**at most one** of the two; neither is allowed):
  - match a pattern;
  - be one of a list, one label per line (always kept as **text**, so `007`
    stays `"007"`; both filled is refused with the app's own sentence, because
    the package's error has no class).
- Length.
- Data columns: min (≥ 1, default 1), max (blank = no limit).

**Rows**
- Key: multi-select of row-label ids; zero ids are refused. If the selection
  is `setequal` to the prefill, the prefill order is kept, because selectize
  reports DOM order.
- Count: min (≥ 0, default 1), max.

**Cells**
- Type.
- "Cells may be missing".
- Range min/max: blank = open; Num/Int only.
- "Allow Inf and -Inf": Num only.
- Allowed values, one per line: coerced to numbers for Num/Int (a non-number is
  refused), text for Char.
- Pattern, length, format, label, description.

**Save.** `dta_set_matrix(dta, ds, form)` **patches the original**. A form
that writes every field would drift on an unchanged save; probed cases:
- an unset `nullable` becomes `false`;
- YAML `0:1` becomes `c("0","1")`;
- a composite key is reordered.

The steps:
1. `orig <- as.list(ds@matrix)`, `pf <- dta_matrix_fields(dta, ds)`.
2. For each form field that **differs** from `pf` (`setequal` for the key),
   write it into `orig` at its path, or delete the key when the field is blank
   or `NA`. A blank `numericInput` is `NA` and must never reach the parser.
3. Field conversions:
   - format: shown without and saved with the `"SAS "` prefix, as
     `dta_column_fields()` / `dta_set_column()` do (probed: a bare `"8.2"`
     aborts);
   - Num/Int cell values: coerced to numbers, refusing a non-number;
   - min == max: written as the scalar count;
   - an empty key: refused (probed: it would silently fall back to the first
     label and drop `rows.key`).
4. Gate by the resulting cell type, **on the server**: `conditionalPanel` only
   hides, so the last values are still sent. Probed: even `infinite: false`
   aborts on an Int cell.
   - not Num: `cells$infinite` is deleted;
   - Char: `cells$range` is deleted;
   - the save notification names a dropped field only if it held a value (a
     range bound, or Inf allowed).
5. Parse with the package's own `dta_matrix_spec_from_list()` through an
   accessor (the pattern at `utils_dta.R:2369`; all accessors together).
6. Call `S7::set_props(ds, matrix =)`.

Validator classes are confirmed by probe:
- `dta_cell_range_type` / `dta_cell_finite_type` / `dta_cell_range_invalid`
- `dta_matrix_count_invalid` / `dta_matrix_key_invalid`

**After save.**
- On error, the modal stays open showing the cli message.
- On success, as `col_save` does: `invalidate_dataset()`, `sync_yaml_text()`,
  a notification and `recheck_dataset()`.
- The modal hints that a loaded file whose header no longer fits will ask to
  be loaded again (REQ-APP-037).
- **Round trip:** saving unchanged leaves `as.list(ds@matrix)` equal (§4
  fixture).

### 2.5 Inspect panel (`#27`, `#28`)

**`columnspec_expected_text()`** gains, with N taken from `columnspec_columnspec`:

| Keyword | Expected text |
|---|---|
| `corner` | "header text: …" ("(empty)" for `""`) |
| `propertyNames` | "a label matching …" |
| `uniqueNames` | "a label no other column has" |
| `minProperties` / `maxProperties` | "at least / at most N data columns" |
| `minItems` / `maxItems` | "at least / at most N data rows" |
| `finite` | "a finite number (not Inf or -Inf)" |

**Actual value.** For the four count keywords the panel shows "3 data
columns" or "2 data rows".

**Rule branch.** If the rule is not in `ds@specs@rules`, fall back to
`dta_find_rule_def()` (accessor) for the type label and wording. A `.matrix:`
id adds "Generated from the matrix layout."

### 2.6 Load failures: readable, sticky, logged (`#18`; D3)

**Notices**
- Every load-failure notice — the slot rejections at `W/app.R:3270-3285` and
  the delivery failures at `3316, 3378` — goes through one helper,
  `load_failure_notice(text)` in `ui_components.R`.
- The helper returns `div(class = "notice-multiline", text)`. `theme.R` styles
  it `white-space: pre-line`.
- These notices are shown with `duration = NULL` and a fixed `id` per dataset,
  so a repeat replaces the notice instead of stacking.
- Success notices are unchanged.

**Message text.** Probed: cli soft-wraps `conditionMessage()` at about 80
columns, splitting the message around a 123-character temp path. With
`pre-line` those soft wraps would become hard breaks. So:
- `dta_load_file()` (`utils_dta.R:585`) runs `load_file()` under
  `options(cli.width = Inf)` (restored on exit), leaving only the bullet breaks;
- the staged temp path in the error is replaced by the uploaded file's name.

**Error log**
- `rv$load_errors` is a data.frame with columns `dataset`, `table`, `file` and
  `message`.
- Pure helpers in `utils_dta.R`:
  - `dta_load_errors_add()`: replaces the row for the same dataset and table.
  - `dta_load_errors_clear(dataset, table = NULL)`.
  - `dta_load_errors_rename()`.
  - `dta_load_error_rows(le, dataset)`: rows shaped like `messages()`, with
    `source = "load"` and `id`/`row`/`column`/`rule_id` all NA.
- When a row is **recorded**:
  - a rejected slot upload;
  - a delivery plan row with `status == "refused"` and a dataset (both
    `commit_delivery()` and the failure branch of `handle_delivery()`).
- When a row is **cleared**:
  - a successful load of the same dataset and table (slot or delivery);
  - "unload all" of that dataset;
  - dataset removal;
  - every document reset (`W/app.R:1725, 9545`).
- When a row is **re-keyed**: a dataset rename (`rename_dataset_state()`).
- On Raw-YAML rebuild, only the rows of datasets that still exist are kept.
- It is **not** saved in the autosave (transient, like the notice).

**Messages dock**
- `msgs_r()` depends on `rv$load_errors`. It binds the load rows first, then
  `dta_dataset_messages()`, filling missing columns with NA.
- The count badge counts the load rows.
- Clicking a `load` row opens a small modal with the message (pre-line) instead
  of `inspect()`.
- A recorded failure for the **active** dataset opens the dock
  (`dta_msgs_dock` → `open`).
- The message downloads include the load rows; they stay gated on a check, as
  today.

### 2.7 Head index on handler remove/move and Raw-YAML apply (`#11`, `#21`)

There is one remap, and both paths call it.
- **Editor paths.** `dta_remap_file_handlers()` (`utils_dta.R:3829`) already
  serves remove and move, but returns early for non-file datasets. It gains a
  matrix branch that rewrites `@heads[[t]]$handler` through the same index map:
  - a removed index becomes `NA`;
  - later indices shift down;
  - a move swaps them.
- **Raw-YAML apply.** `dta_transfer_bound_data()` (`:4071-4100`) remaps the
  copied heads with `dta_match_handlers()`. Probed: it gives `2 1` for a swap
  and `NA 1` for a dropped entry.

An `NA` index barely occurs in the editor, because `do_remove_handler`
(`W/app.R:4575-4586`) unloads that handler's tables first. Through Raw YAML it
can occur. A lazily read table then reports the package's "the file changed;
load it again". The wording is imprecise, but the outcome is safe: it fails
and never passes hollow. Listed in §9.

### 2.8 Help and onboarding (`#38`, `#39`)

- `matrix_dta.yaml` is added last to `dta_example_yaml_preferred_order`.
- The stale comments at `W/app.R:9979` and `W/R/ui_components.R:247-256` are
  fixed.
- `W/R/dataset_template.R:35` gets one line naming the `row_labels` variant.

## 3. Files (one writer each)

| File | Change |
|---|---|
| `W/R/utils_dta.R` | <br>• `dta_add_dataset` matrix branch <br>• invariants in `dta_set_column` / `dta_remove_column` / `dta_move_column` (incl. the corner keyword trap) <br>• `.matrix:` pre-check in `dta_set_rule` <br>• matrix branch in `dta_remap_file_handlers` <br>• head remap in `dta_transfer_bound_data` <br>• `cli.width` and the file name in `dta_load_file` <br>• new: `dta_matrix_fields`, `dta_set_matrix` (patches the original), `dta_matrix_key_ids` <br>• new: `dta_load_errors_*`, `dta_load_error_rows` <br>• accessors `dta_pkg_matrix_spec_from_list`, `dta_pkg_find_rule_def` <br>• example order |
| `W/R/ui_components.R` | `ds_edit_menu()` matrix items; `load_failure_notice()`; comment |
| `W/R/theme.R` | `.notice-multiline` |
| `W/R/dataset_template.R` | header comment only |
| `W/app.R` | <br>• add-dialog fields and save <br>• the four gates <br>• column-modal wording <br>• rules-list key line <br>• Matrix layout modal and save <br>• inspect cases, rule fallback and the load-row click <br>• notices and the load-error log wiring <br>• comment at `:9979` |
| `tests/testthat/test-shinyapp-matrix.R` | §4 |
| `inst/qualification/requirements/REQ-APP.yaml`, `_index.yaml`, `inst/qualification/tests/test-OQ-APP-matrix.R` | §5 |
| `inst/qualification/baseline/inst-files.sha256` | from `sync_qualification_manifest.R` |
| `CHANGELOG.md`, `vignettes/DTAtools.Rmd`, `README.md` | §7 |

**Execution, in four steps**
1. **A:** the helpers: `utils_dta.R`, `ui_components.R`, `theme.R`,
   `dataset_template.R`.
2. **B:** `app.R`, once A's helper signatures are fixed, from an exact line map
   of the sites.
3. **C:** tests, after A and B.
4. **D:** qualification and docs.

A and B run one after the other, because B calls A's helpers. No file has two
writers.

## 4. Developer tests (`tests/testthat/test-shinyapp-matrix.R`)

**Unit tests (`app_fn()`), behaviour only**

`dta_add_dataset` for a matrix:
- the row label becomes a non-nullable SAS Char key;
- all four corner forms are stored, `[null]` included, and each round-trips
  through `dta_dataset_to_list`;
- the cell type is stored;
- refused: a missing corner, a missing cell type, an empty label, empty Other
  text, the text `named`;
- `type = "tabular"` with `corner = "named"` yields a plain tabular dataset.

Row-label invariants:
- renaming the key renames `row_key`, keeping its position in a composite key;
- adding a label with a list corner appends `""`;
- removing the key is refused and the DTA is unchanged;
- removing a non-key label drops its entry;
- removing position 1 with a `null` entry is refused;
- a move swaps the entries;
- a move involving a `null` is refused;
- `[null, a, b]` moving `b` up works;
- a move never changes the key.

Rules:
- `.matrix:` id refused (`r$ok` FALSE, plain message);
- the rule column picker's ids are the row-label ids.

`dta_matrix_fields` / `dta_set_matrix`:
- an unchanged save leaves `as.list(ds@matrix)` identical for both bundled
  datasets **and** for a synthetic fixture with:
  - `header.values: ["007", "S1"]`
  - Int `cells.values: [0, 1, 2]`
  - `length`, `format` and `infinite: true`
  - a composite key
  - `rows.count: 0`
  - `header.count: 96` (the scalar form)
  - a `[null, x]` corner
- edits land;
- refused, with the DTA unchanged:
  - a pattern and values together
  - min > max
  - a non-whole count
  - `header.count.min` 0
  - zero key ids
  - a non-numeric Num value
- range is dropped for Char, and the dropped-field report names it;
- Num → Int drops `infinite: false` without an error and without a report;
- a format edit saves as `SAS 8.2`;
- a blank max removes the bound;
- a key given as `c("B","A")` against a prefill of `c("A","B")` is kept as
  `A, B`;
- unset `nullable` stays unset on an unchanged save.

Heads remap (through `dta_remap_file_handlers` and `dta_transfer_bound_data`):
- removing handler 1 shifts a head bound to handler 2 down to 1;
- its own removal gives `NA`;
- a move swaps;
- a Raw-YAML reorder of `files:` remaps the head;
- end to end: two TSV entries, `b.tsv` loaded **lazily** through entry 2,
  entry 1 removed, then `check()` passes with no "changed" failure.

Corner keyword trap:
- removing the label after `"blank"` from a `["blank", x]` corner is refused;
- the Add dialog and the Matrix layout save refuse a single text `named`.

Load-error text:
- the gate message recorded for an upload names the uploaded file, not the
  staging path;
- it has no line break inside a sentence (`cli.width = Inf`).

Load-error log helpers:
- add replaces the row for the same table;
- clear by dataset, and by table;
- rename re-keys;
- the rows have `source == "load"`.

**`testServer`, reading the outputs** (per the blind-spot memory)

Add dialog:
- `add_ds_body` HTML contains Matrix, the three fields and **no `checked`**
  attribute on them;
- after a cancel and reopen, the body has no typed values;
- adding via inputs lands a `DTADataSetMatrix` with `rv$structure` type
  `matrix`.

Edit menu:
- `ds_edit_menu("matrix")` HTML has Row labels, Rules and Matrix layout;
- `"file"` has neither of the first two.

Row labels and rules:
- `edit_cols` on `gene_counts` renders a `col_modal_body` saying "row label";
- `col_save` renaming `GENE_ID` changes the YAML text's `rows.key`;
- the rule list shows the generated-key line.

Matrix layout:
- `edit_matrix` renders `mx_modal_body` with the prefilled pattern;
- `mx_save` with a changed count changes the YAML text and re-validates.

Inspect bodies:
- a `minItems` finding shows "at least 5 data rows" and "2 data rows";
- a `.matrix:row_key` finding shows the col_unique label.

Load failures:
- uploading a wrong-corner TSV binds nothing;
- `output$msgs_dock_meta` counts 1;
- the `msgs` data has a `load` row with the gate hint;
- a following good upload clears it.
- `load_failure_notice()` keeps its `\n` and its class. The notification
  itself is not observable in `testServer`.

**Mutation check** once green: revert, one at a time,
- the key rename
- the corner append
- the null-move refusal
- the remove-key refusal
- the heads remap
- the load-row clear

Each must fail a test.

## 5. Qualification

`inst/qualification/docs/maintaining.md` has been read.
- **Requirements.** New REQ-APP-041 to 046, each `covers: [run_dta_app]`.
  They are written as specification sentences before the tests.
- **Tests.** OQ-APP-050 to 055, in `test-OQ-APP-matrix.R`, with its header
  comment updated. Expected values come from the sentences, never from output.
- **Floor.** The APP `min_tests` floor in `_index.yaml` moves from 46 to 52.
- **Manifest.** Run `sync_qualification_manifest.R`.
- **Run.** IQ+OQ, quick tier, from a temporary library.
- **No version change** (`package_version` stays).

| Requirement | Sentence (abridged) | Test |
|---|---|---|
| REQ-APP-041 | Adding a matrix dataset requires a row-label ID, a corner and a cell type. The result has a single non-nullable SAS Char row label as key, the declared corner and cell type, and ≥ 1 data column and ≥ 1 row | OQ-APP-050 |
| REQ-APP-042 | Row-label edits keep the matrix valid: <br>• renaming the key renames the row key <br>• adding to a listed corner extends it with an empty entry <br>• removing the key is refused and changes nothing | OQ-APP-051 |
| REQ-APP-043 | The matrix-layout editor saves header, row and cell settings as entered and re-validates. It refuses minimum > maximum without changing the dataset | OQ-APP-052 |
| REQ-APP-044 | Rules of a matrix are editable on row-label columns; an id starting `.matrix:` is refused | OQ-APP-053 |
| REQ-APP-045 | Inspecting a matrix row-count finding states the declared bound and the actual number of rows | OQ-APP-054 |
| REQ-APP-046 | A file the application refuses to load is recorded in that dataset's validation messages with the reason, until a file loads in its place. Neither removing a file entry in the editor nor reordering file entries in raw YAML makes a correctly loaded matrix table fail as changed | OQ-APP-055 |

## 6. Verification

1. The full check runs:
   - `Rscript .github/scripts/style.R --check`
   - roxygen (no diff expected; no export changes)
   - `pre-commit run --all-files`
   - the full `devtools::test()`
   - the IQ+OQ quick run
2. The real app in a browser (`pkgload::load_all(); run_dta_app()`), or in the
   headless-Edge harness if screenshots fail. Steps:
   - Add dataset → Matrix, and reopen once to see that no stale values remain.
   - Edit the row labels: add `GENE_NAME`, rename the key.
   - Change the Matrix layout.
   - Add a TSV entry.
   - Upload `inst/extdata/matrix_gene_counts.tsv`, validate and inspect a
     finding.
   - Upload a wrong-corner file, then read the sticky notice and the dock row.
   - Export md and docx, and check that "Matrix layout" and "Row label columns"
     appear.
   - Load the bundled `matrix_dta.yaml` from the example loader.
3. Phase 4 review:
   - A review of the diff, plus a review that drives the app.
   - Fix what they find, then verify again.

## 7. Docs

- **CHANGELOG** `[Unreleased]`:
  - Extend the existing matrix bullet under Added: "…and in the app, where a
    matrix dataset can be added, its row labels edited like columns and its
    header, dimensions and cells edited under Matrix layout".
  - Add one Changed bullet: a file the app refuses to load now shows its reason
    with line breaks, stays until closed, and is listed in that dataset's
    validation messages until a file loads in its place.
  - No new heading.
- **Vignette** (`vignettes/DTAtools.Rmd:2033`): drop "the app's dataset editor"
  from "What is not supported", and add one sentence on the app to the matrix
  section.
- **README** (`README.md:925-930`): one sentence saying tabular, matrix and
  file datasets can be added and edited in the app.

## 8. Working rules

- One writer per file, from an exact spec, reporting any deviation.
- The app helpers use `stop()` inside `dta_try()`, as their neighbours do.
- Never `styler::style_pkg()`, `air format`, or a hand-edit of
  `man/`/`NAMESPACE`.
- Never edit with a PowerShell string (backtick trap).

## 9. Out of scope (stated in the PR)

- **Changing a dataset's type** in the editor (D4). The type is fixed once
  created; Raw YAML is guarded by REQ-APP-036.
- **A data preview grid.** The app has none for any type.
- **The per-check column-spec summary table in the app.** It is shown for no
  type; it lives in the HTML report.
- **A corner-text field in the row-label form.** D1 chose auto-alignment, with
  the texts kept in Matrix layout.
- **Load errors in the autosaved session.** They are transient.
- **The wording when a file entry is dropped in Raw YAML.** A table loaded
  lazily through that entry fails with the package's "the file changed; load it
  again". That fails safe; only the reason is imprecise.
- An orphan `@heads` entry when a table fails to restore (cosmetic).
- A dataset-template patch op for `matrix:` (`set:` paths work).
- Template-guide support for matrix (listed in the vignette).
- A `has_header` control in the Files editor (exposed for no type).
- Template lint display (the app runs none).
- Verified working and left unchanged:
  - containers
  - delivery routing
  - Raw YAML
  - session restore
  - template diff/history
  - vocabulary and patch paths
  - export

## 10. User decisions (2026-11-14)

| # | Question | Decision |
|---|---|---|
| D1 | List corner when row labels change | Auto-align: add → `""`, remove → drop, move → swap; texts edited in Matrix layout |
| D2 | File entry in the Add dialog | No; added in the Files editor, as for Tabular |
| D3 | Refused-upload notice | Readable **and** sticky **and** recorded in the dataset's error log (the validation messages) |
| D4 | Type change in the editor | Keep it fixed |

## 11. Revision history

- r0: draft after the five-way audit.
- r1:
  - Design review findings:
    - no preselection; matrix arguments only for a matrix;
    - a `null` corner entry is refused on remove and on move;
    - a composite key keeps its order on rename;
    - selectize keeps the prefill order;
    - count bounds and the scalar count form;
    - pattern XOR values; dropped hidden fields are reported;
    - typed values; a full round-trip fixture;
    - REQ for #11;
    - sticky notices replace each other by id;
    - the delivery notices are included;
    - the row-label wording grep;
    - the rule-picker test.
  - User decisions D1–D4, with the D3 error log (§2.6).
  - The audit row for streamed tables restored from autosave was probed:
    they work. Rows were added for the container manager, the column form and
    the click guard.
- r2: findings from the empirical review (R 4.5.1, against the worktree):
  - Add writes `nullable: false` explicitly.
  - A single list-corner text `named`/`blank` changes meaning after a YAML
    round trip, so it is refused everywhere.
  - The Matrix layout save **patches the original** instead of writing every
    field.
  - Format prefix, blank `numericInput`, and the empty-key refusal.
  - Cell-type gating on the server.
  - Row #21 was wrong: the Raw-YAML transfer copies the head index without
    remapping. Fixed in the same remap.
  - `cli.width = Inf` and the file name instead of the staging path.
  - Stale-input defences for the layout modal.
  - Hint wording: the key is explicit, and renaming under a named corner
    makes the table stale.

## 12. Implementation review (2026-11-14)

Two reviews of the finished diff: one against the package conventions, and one
that drove the running app. Neither found a blocker, data loss or a hollow
pass. What they found, and what was done:

| Finding | Resolution |
|---|---|
| A refusal row could outlive the file that replaced it. Gate and bind refusals were keyed by table only, and removing or moving a file entry left `slot` stale | Every refusal into a single-file entry records the entry; rows are remapped when entries are removed or moved; a delivery that loads clears by entry too; a row can be removed from the list in its dialog |
| Sticky notices outlived their problem; a partial delivery's notice had no `id`, so notices stacked | A notice is removed once its datasets have no rows left in the log (this also covers Start over and a new document); every delivery notice uses `load_fail_delivery` |
| A refused delivery's reason was soft-wrapped into lines inside sentences | `dta_load_delivery()` runs under `cli.width = Inf`, like `dta_load_file()` |
| Some Matrix layout refusals named YAML paths | The app's own sentences check counts, range order and nullable key columns before the package parser runs |
| An unchanged save rewrote a range bound beyond 15 significant digits | Numbers are compared at 15 significant digits, the precision `numericInput` returns |
| A cell type change kept unedited allowed values in the old type | Converted to the new type; a value that is not a number for Num/Int is refused |
| The vocabulary view of a row label said "column"; so did a rename collision | Matrix wording |
| Inspecting a row or column count showed "(column)" | "(whole matrix)" |
| `numericInput(value = NA)` logged a browser warning per blank field | Blank fields render empty without the warning |
| The package's "more fields" gate hint said "drop the `null`" when no `null` was declared | Conditional hint (`R/DTADataSetMatrix-class.R`) |
| OQ-APP-055 did not cover the Raw-YAML reorder half of REQ-APP-046; OQ-APP-052's unchanged save sent almost no inputs | Both completed |
| **Found while driving the app:** DT's column filters ship selectize 0.12 under the dependency name `selectize`, and the browser keeps whichever copy arrives first. Once the messages dock had rendered, every later `selectizeInput` failed to bind ("Unable to find selectize-plugin-a11y"), and the rest of its modal failed with it. This affected the Files editor, the rule pickers and the metadata picker, for every dataset type | Shiny's selectize is loaded with the page head. The row key in Matrix layout is a checkbox group: a matrix has one to a few row labels |

**Further out of scope, found during the review and left as they were:**
- The Delivery view's Note column collapses the whitespace of a reason (package
  `dta_delivery_reason()`), so a matrix gate hint there is one line. The
  messages dock and the notice show it in full.
- Not caused by this work (tabular too):
  - changing a column's type after a file is loaded makes every cell fail
    "must be string" with no prompt to load the file again;
  - inspecting a `type` finding shows an empty "Should be: type:".
