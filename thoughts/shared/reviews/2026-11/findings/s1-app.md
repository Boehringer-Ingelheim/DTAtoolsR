# Findings — slice s1-app

Slice: `inst/shiny/dta_app/app.R` (11 206 lines) — the Shiny app's UI and server.
Pinned tree: `$SNAP` = scratchpad/snapshot (68408c44).

Files fully read: `inst/shiny/dta_app/app.R`, lines 1-11206 (complete). 29 findings, F-s1-app-01 … F-s1-app-29; cross-checks of t-shiny-1 F-10/F-14/F-34 and t-shiny-2 F-10 are in their own section; "Coverage" is at the end.

Method: read-only; no R run, no server/browser started. Every finding below is
marked `verified-by-reading` or `inferred`.

---

## Area 1 — rule editor (app.R 6355-7473)

### F-s1-app-01  [blocker] [bug]
- Where: inst/shiny/dta_app/app.R:6399-6427 (`cond_to_rows`), 6374-6377 (operator select), 6465-6477 (`collect_cond`); inst/shiny/dta_app/R/utils_dta.R:2936-2950
- What: Opening and saving a rule whose clause uses an engine operator the editor does not offer (`range`, `not_equal`, `equal`) silently rewrites it to `equals` — `not_equal` is inverted, `range: [lo, hi]` becomes `equals: "lo, hi"`.
- Evidence:
  `op <- keys[1]` / `val <- ... paste(as.character(unlist(raw)), collapse = ", ")` (6413-6418) hands ANY key through;
  `selectInput(oid, "Operator", choices = dta_condition_operators(), selected = pf$op %||% "equals", ...)` (6374) — the choices are only equals, not_equals, greater, greater_equal, less, less_equal, min_max, in, not_in, pattern, empty (utils_dta.R:2937-2949), so a `selected` of "range"/"not_equal" marks no option and a single <select> then holds its first option, "equals";
  `op <- input[[..._op_i]] %||% "equals"` / `inner[[op]] <- parse_cond_value(op, txt)` (6465, 6477) writes that back.
  The engine accepts all three (R/evaluateRules.R:875-881) and DTA_YAML_GUIDE.md documents them (l.663 `range | [lo, hi]`, l.669 `equal`/`not_equal`) and uses `LBORRES: { range: [0.5, 40] }` in its own worked example (l.1079, 1088).
- Failure scenario: load the guide's `glucose_unit` rule, Edit ▸ Rules ▸ pencil, change only the description, Save rule. THEN clause `LBORRES: {range: [0.5, 40]}` is saved as `LBORRES: {equals: "0.5, 40"}`: every glucose row now fails. A hand-written `VISIT: {not_equal: SCREENING}` is saved as `VISIT: {equals: SCREENING}` — the opposite test — with the toast "Rule saved." and no warning. The same `cond_to_rows()` feeds the grouped-condition rows (6555).
- Confidence: verified-by-reading (R side traced end to end; that a <select> with no `selected` option reports its first option is browser behaviour, not run here)
- Suggested direction: map `range` to the UI's `min_max`, `equal`/`not_equal` to `equals`/`not_equals`, and refuse to open (or round-trip verbatim) any operator the form cannot show.

### F-s1-app-02  [blocker] [bug]
- Where: inst/shiny/dta_app/app.R:6405-6420 (`cond_to_rows`), 6458-6482 (`collect_cond`, `out[[col]] <- inner` at 6479)
- What: A clause with several operators on one column — the form the guide prescribes — cannot survive the editor: on open only the first operator (or only min/max) is shown, and on save two rows for one column overwrite each other, so operators are dropped silently.
- Evidence:
  `if ("min" %in% keys || "max" %in% keys) { op <- "min_max" ... } else { op <- keys[1]; raw <- spec[[1]] ...}` — every other key of `spec` is discarded (one row per COLUMN: `lapply(names(cond), ...)`).
  `out[[col]] <- inner` (6479) replaces what an earlier row put under the same column, where `collect_group_conditions()` merges instead (`out[[nm]][[col]][[op]] <- ...`, 6601).
  DTA_YAML_GUIDE.md l.655/667: "Operators (several on one column are ANDed)… Each column may appear only once per clause; merge its operators."; the engine aborts on a column given twice (R/evaluateRules.R:692-697).
- Failure scenario: (a) YAML `then: {AGE: {greater_equal: 18, less: 65}}` -> open in the editor, Save -> `then: {AGE: {greater_equal: 18}}`; the upper bound is gone and rows with AGE 80 pass. (b) In the form add IF rows `AGE greater_or_equal 18` and `AGE less 65` -> saved rule holds only `AGE: {less: 65}`. (c) A grouped condition built from two rows `c1 / AGE / between 18,` and `c1 / AGE / not equals 99` is stored merged (`{min: 18, not_equals: 99}`), but the next open shows only "between 18" and the next Save drops `not_equals`.
- Confidence: verified-by-reading
- Suggested direction: emit one row per (column, operator) in `cond_to_rows()` and merge same-column rows in `collect_cond()` (refuse a repeated operator), mirroring the grouped collector.

### F-s1-app-03  [major] [bug]
- Where: inst/shiny/dta_app/app.R:6428-6457 (`parse_cond_value`), 6418 (join with ", ")
- What: Every rule value passes through `as.numeric()` on save and `in`/`not_in` lists are joined and re-split on commas, so text codes are turned into numbers and values containing a comma are split — on a save that changed nothing.
- Evidence:
  `nums <- suppressWarnings(as.numeric(parts)); if (all(!is.na(nums))) return(nums)` (6443-6446) and `num <- suppressWarnings(as.numeric(txt)); if (!is.na(num)) return(num)` (6452-6455);
  `paste(as.character(unlist(raw)), collapse = ", ")` (6418) then `strsplit(txt, ",")` (6438).
  The engine compares a text column as text: "non-numeric columns (Char ids with leading zeros above all) are never touched" (R/evaluateRules.R:826-828), then `x == value` / `x %in% value` (841-845), where R renders a numeric bound with as.character().
- Failure scenario: rule `condition: {SITEID: {in: ["001", "002"]}}` on a Char column. Open, Save. Stored value is now `c(1, 2)`; `"001" %in% c(1, 2)` is FALSE, so the IF clause selects no row and the rule can never fail again. `equals: "007"` -> 7, `"1.50"` -> 1.5, `"1e5"` -> 1e+05, `"0x10"` -> 16 likewise. `in: ["Smith, John", "Doe"]` is shown as `Smith, John, Doe` and saved as three values.
- Confidence: verified-by-reading
- Suggested direction: keep what the rule held (remember each value's type from the prefill, or coerce by the column's declared type), and use a list control or an escaping rule instead of a bare comma join.

### F-s1-app-04  [major] [code-cheat]
- Where: inst/shiny/dta_app/app.R:7416-7423 (range rule), 6468-6475 (`collect_cond`), 6594-6599 (`collect_group_conditions`)
- What: A bound that does not parse as a number is discarded under `suppressWarnings(as.numeric())` instead of being reported, so a rule is saved weaker than what the form shows.
- Evidence:
  `mn <- suppressWarnings(as.numeric(input$rule_min))` … `args$min <- if (!is.na(mn)) mn else NULL` (7416-7419): only when BOTH are unusable is there an error (7420);
  `mn <- if (...) suppressWarnings(as.numeric(parts[1])) else NA_real_` … `if (length(inner) == 0) next` (6471-6475): the whole condition row is skipped.
- Failure scenario: range rule, Minimum `0,5` (decimal comma — the owner's locale is German), Maximum `10` -> saved with `max: 10` only, toast "Rule saved."; values below 0.5 pass. IF row `AGE between "18 - 65"` plus a second IF row -> the AGE row vanishes from the saved rule; with it as the only row the message is the unrelated "A conditional rule needs at least one IF and one THEN condition."
- Confidence: verified-by-reading
- Suggested direction: treat non-empty text that is not a number as an inline error (`rv$rule_msg`), never as "absent".

### F-s1-app-05  [major] [bug]
- Where: inst/shiny/dta_app/app.R:7294-7326 (selector refresh observer), 6825-6833 (`collect_group_constraints`)
- What: Renaming a named grouped condition silently unselects it in every constraint that used it, and a constraint row with an empty Left/Right is then silently left out of the saved rule.
- Evidence:
  `updateSelectInput(session, left_id, choices = c("(select)" = "", cond_names), selected = if (...) left_selected else "")` (7313-7318) — after a rename `left_selected` (the old name) is no longer among `cond_names`;
  `if (!nzchar(ctype) || !nzchar(left) || !nzchar(right)) next` (6833).
- Failure scenario: grouped rule with conditions `c1`, `c2` and constraints K1 (c1 requires c2), K2 (c2 mutually exclusive c1). Correct a typo in the name `c1` -> `c1_failed`: both constraints drop to "(select)" on that side. Fix K1 by hand, overlook K2, Save: the rule is stored with K1 only and "Rule saved."; K2 is gone. (With no complete row left the user at least gets "Define at least one grouped constraint.")
- Confidence: verified-by-reading for the server logic; inferred that selectize falls back to "" when `selected` is not among the new choices (not run in a browser)
- Suggested direction: carry the rename into the selectors, and make a partially filled constraint row an inline error rather than something to skip.

---

**Progress note (for a resumed run):** `inst/shiny/dta_app/app.R` has been READ COMPLETELY, lines 1-11206.
Helper code opened to verify findings: `R/containers.R` (all), parts of `R/utils_dta.R`
(1-130, 176-265, 262-347, 480-860, 1195-1430, 2925-2965, 3335-3410, 4380-4640, 5180-5789),
`R/utils_export.R` 290-472, `R/ui_components.R` 1-180, `R/theme.R` 1570-1628, `R/app_config.R` (grep),
package `R/evaluateRules.R` 320-390 and 630-950, `R/containerFindings.R` (all), `R/containerAssignments.R` (all),
`R/loadDelivery.R` 520-580, `DTA_YAML_GUIDE.md` 612-672 and 1066-1095. What remains is writing up, not reading.

## Area 2 — uploads, delivery, the file-drop visibility change (app.R 2222-2285, 3328-4056, 10581-10668, 10894-10924)

### F-s1-app-06  [major] [bug]
- Where: inst/shiny/dta_app/app.R:2222-2235, 2242-2256, 10879-10881; inst/shiny/dta_app/R/utils_dta.R:114-128, 283-288
- What: The landing drop target has no size or type guard: whatever is dropped (up to the app-wide 1 GB cap) is slurped with `readLines()`, parsed as YAML, and on failure its ENTIRE text is sent back to the browser inside a modal textarea.
- Evidence:
  `txt <- tryCatch(paste(readLines(f$datapath, warn = FALSE), collapse = "\n"), error = function(e) NULL)` then `finish_file_load(dta_read_yaml(f$datapath), txt, f$name)` (2231-2234) — `f$size` and the extension are never looked at; `accept = c(".yaml", ".yml")` (10880) only filters the file-picker dialog, not a drop;
  `rv$load_failure <- list(name = name, text = txt, ...)`; `showModal(dta_load_failure_modal(rv$load_failure))` (2250-2254) with `textAreaInput(..., value = lf$text, rows = 16)` (utils_dta.R:119);
  the only size check, `dta_check_yaml_size(parsed)` (utils_dta.R:285-286), runs AFTER `yaml::read_yaml()` has parsed the whole file and counts parsed leaves, not bytes.
- Failure scenario: the user drags the 400 MB data file instead of the specification onto "Drop a .yaml / .yml file here". The single R worker slurps 400 MB with readLines() and then hands the file to the YAML parser, blocking every other session on it meanwhile; when the load is refused, a `showModal` message carrying the 400 MB text is pushed to the tab, which hangs. utils_dta.R:218-222 shows the authors already treat "holds every other session for many minutes" as a defect for much smaller inputs.
- Confidence: verified-by-reading (server side); that Shiny does not enforce `accept` on a drop is its documented behaviour, not run here
- Suggested direction: refuse by `f$size` (a few MB) and extension before reading anything, and cap the text echoed into the failure dialog.

### F-s1-app-07  [major] [code-cheat]
- Where: inst/shiny/dta_app/app.R:684-691 (`dv_findings`), 10586 (`findings_now`), 983-993, 2974-3004, 10247-10277, 10297-10304
- What: `dv_findings()` turns ANY error of `DTAtools::container_findings()` into "no findings", and every reader of `findings_now()` then reports the packaging as clean — including the green "Packaging conforms" chip and the "summary certifying this successful validation".
- Evidence:
  `# The packaging findings of the live document; never throws.` / `f <- tryCatch(DTAtools::container_findings(dta), error = function(e) NULL)` / `if (is.data.frame(f)) f else dv_empty_findings()` (684-690);
  `complete <- length(validated) == length(st) && nrow(fnd) == 0` -> `"btn btn-success w-100"`, title "Download a summary certifying this successful validation" (10249-10277); `dta_build_validation_report(..., findings = findings_now())` (10299-10304).
  The app's own linter wrapper states the opposite rule: "Never throws, and never reports a crashed linter as a clean document: a failure becomes one `lint_failed` error finding" (utils_dta.R:62-64).
- Failure scenario: `container_findings()` aborts (it aborts on a non-DTA, re-stamps and validates a stale object first — R/containerFindings.R:50-53, R/containerAssignments.R:161-220 — and walks every dataset's provenance and file entries). All datasets passed content validation, one file was delivered loose instead of inside the required archive. With the error swallowed: nav rows green "Passed all checks", Delivery view "Packaging conforms", sidebar button green, downloaded summary certifies a pass — the exact verdict the feature exists to prevent.
- Confidence: verified-by-reading (the swallow and every consumer); which concrete input makes `container_findings()` throw was not established
- Suggested direction: return an explicit "packaging could not be judged" state (one error finding, like `lint_failed`) and let no consumer read it as conforming.

### F-s1-app-08  [minor] [logic-error]
- Where: inst/shiny/dta_app/app.R:983-993 (`dv_view_ui` chip), 710-717 (`dv_anything_bound`)
- What: The Delivery view's headline verdict reads "Packaging conforms" as soon as ANY dataset holds data, even when nothing that must arrive inside a container has been delivered.
- Evidence:
  `} else if (!dv_anything_bound(dta)) { list("status-pending", "Nothing delivered yet") } else { list("status-pass", "Packaging conforms") }` — `dv_anything_bound()` is `any(...has_data)` over ALL datasets; `container_findings()` only ever reports BOUND items of datasets that need a container (R/containerFindings.R:72, 86).
- Failure scenario: document with container `sdtm_*.zip` required for dataset `ae` only, and a second dataset `notes` without a container. Drop `notes.csv` on its slot, open the Delivery view: green chip "Packaging conforms" directly above a container whose own line says "sdtm_*.zip: not delivered yet".
- Confidence: verified-by-reading
- Suggested direction: derive the chip from the container states (every container with entries has delivered files and no finding), not from "something is bound somewhere".

### F-s1-app-09  [minor] [bug]
- Where: inst/shiny/dta_app/app.R:10598-10606 (`delivery_zone_on`, `delivery_block_on`), 10907-10924, 806-832 (`dv_panel_ui`), 3804-3839 (`commit_delivery`), 4050-4053
- What: (file-drop change) The Delivery block's flag ignores `rv$last_delivery`: in a document that names no containers the block — and with it the sidebar's "Last delivery … Details" line — is hidden in view mode, so the routing table of an archive dropped on a dataset's own slot cannot be opened; switching Edit mode on makes the same line appear.
- Evidence:
  `delivery_block_on <- reactive({ delivery_has_dataset() && (delivery_zone_on() || editing()) })`; `conditionalPanel("output.delivery_block", div(..., conditionalPanel("output.delivery_zone", ...fileInput("delivery_files"...)), uiOutput("delivery_panel")))` — `delivery_panel` holds `last_line` (816-827) and is inside the block;
  a slot drop of an archive still commits a delivery: `handle_delivery(fileinfo, datasets = dsname, origin = "slot")` (3366) -> `rv$last_delivery <- list(plan = d$plan, ...)` (3834), and only a sidebar-origin delivery switches to the Delivery view (3835-3839).
- Failure scenario: view mode, document without containers, drop `bundle.zip` on dataset `dm`'s slot; two of its files are refused. The toast is the only trace; "Last delivery, 14:02: 1 file loaded, 2 unmatched · Details" exists server-side but its panel is `display:none`. Enable edit mode: the line and its Details link appear although nothing about the delivery changed.
- Confidence: verified-by-reading
- Suggested direction: let the block show whenever there is something in it (`zone || editing || !is.null(rv$last_delivery)`), or keep the last-delivery line outside the gated block.
- Note on the rest of the new logic (checked, no defect found): the two flags follow `rv$dta`, so adding the first container (manager, Files editor's typed archive name, raw YAML), removing/renaming one, a default container, an implied container (`dta_container_rows()`, containers.R:145-185), Start over and Restore all re-evaluate them; `output$dataset_hint` (10660-10668) and the slot text (10731-10737) only mention the sidebar drop area when it exists; no other code shows or hides the zone. A document with containers but no datasets shows no block at all, so in edit mode the sidebar's "Containers…" link is missing there (the Metadata tab's "Manage containers…" still works).

### F-s1-app-10  [minor] [bug]
- Where: inst/shiny/dta_app/app.R:3459-3484, 4559-4578, 1716-1720 vs 1757-1778; inst/shiny/dta_app/R/utils_dta.R:5576-5615
- What: The staged copy of a loose upload is never deleted while the session lives — not when the load is refused, not when the file is replaced or removed — and it is outside the "delivery budget"; after the session it lingers for the retention window although nothing can use it.
- Evidence:
  `staged <- dta_stage_upload(dp, nm)` … `own("upload", dirname(staged))` … `if (!res$ok) { rejected <- ...; next }` / `"could not be bound" ...; next` (3459-3484) — no `unlink()` on either refusal;
  `dta_delivery_cleanup()` and `dta_session_delivery_bytes()` both start with `dirs[startsWith(basename(dirs), "dta_upload_delivery_")]` (utils_dta.R:5582, 5602), i.e. they skip `dta_upload_<random>` staging directories;
  the two comments disagree about why they stay: "the autosave, its tables and staged uploads stay for 'Restore previous session'" (1719-1720) vs "Restore re-reads file datasets from their files; tables come back from the autosave's own Arrow copies" (1762-1763).
- Failure scenario: a user iterating on a 900 MB extract (upload, see errors, fix, upload again) ten times leaves ~9 GB of superseded copies in tempdir() until Start over, and for 24 h after the tab closes; none of it counts against `DTATOOLS_SESSION_DELIVERY_BYTES`.
- Confidence: verified-by-reading
- Suggested direction: unlink the staging directory on every refusal and whenever its table is unloaded or replaced; drop tabular staged copies at session end.

### F-s1-app-11  [minor] [bug]
- Where: inst/shiny/dta_app/app.R:896-902 (`dv_last_table`), 3834; assignments of `rv$last_delivery` only at 1920, 3834, 10337, 11103
- What: `rv$last_delivery` keeps the handler INDEX and dataset id of delivery time, but is not reset or re-keyed when file entries are moved or removed, a dataset is renamed or removed, or raw YAML is applied — so the Delivery view's "Matched" column then names the wrong entry.
- Evidence: `e <- ca$expected[ca$dataset == plan$dataset[[i]] & ca$handler == plan$handler[[i]]]` joins the stored index against the CURRENT `container_assignments()`; `after_handler_change()` (4943-4950), `rename_dataset_state()` (4890-4918), `remove_dataset_confirm` (3272-3310) and `apply_yaml` (9739-9774) re-key uploads, file ids and load errors but never touch `rv$last_delivery`.
- Failure scenario: delivery loads `dm.csv` through entry 2 of `dm`. In Edit ▸ Files move entry 2 up. Delivery view now shows `dm.csv | … | Matched: <expected name of the entry that is now #2>`. After renaming the dataset the row shows the old id and "—".
- Confidence: verified-by-reading
- Suggested direction: store the expected name in the committed plan instead of resolving it later, or clear/re-key `rv$last_delivery` in the same places the uploads are re-keyed.

### F-s1-app-12  [minor] [bug]
- Where: inst/shiny/dta_app/app.R:5478-5491 with 5494-5499 and 5018-5031; 3395-3405 with 3969-3974
- What: Two confirmation dialogs are `easyClose = TRUE` although only their Cancel BUTTON does the clean-up: Esc or a backdrop click leaves the pending state set and skips what Cancel does.
- Evidence:
  remove-entry dialog: `easyClose = TRUE` (5490); only `observeEvent(input$cancel_remove_handler, { rv$pending_handler_removal <- NULL; removeModal(); reopen_file_editor() })` brings the Files editor back, and the comment on `reopen_file_editor()` says that is the point ("rather than leaving the user back at the dataset view mid-edit");
  overwrite dialog: `easyClose = TRUE` (3404); `rv$pending_upload` is cleared and the file input reset only via `cancel_overwrite` / `handle_upload()`'s tail (3589).
- Failure scenario: Edit ▸ Files, bin on an entry with loaded files, press Esc: the editor is gone and `rv$pending_handler_removal` still names the entry. Drop a file that would overwrite, press Esc: the slot's file input still shows the file as uploaded, and choosing the same file again through "Browse" fires no change event.
- Confidence: verified-by-reading (R side); the browser not firing `change` for an unchanged file selection is standard behaviour, not run here
- Suggested direction: `easyClose = FALSE` on both (as the Replace-delivery dialog already has, 3756), or clean up from the `dta_modal_state` report.

## Area 3 — swallowed failures (autosave, YAML sync, raw-YAML apply, metadata saves)

### F-s1-app-13  [major] [code-cheat]
- Where: inst/shiny/dta_app/app.R:1850-1881 (`autosave`)
- What: A failed autosave is swallowed whole — the table dump, the `saveRDS()` and the reason — so "Restore previous session" silently restores an older state (or nothing) with no warning at the time or later.
- Evidence:
  `saved <- try(saveRDS(list(user = session$user, dump = store$dump(isolate(rv$dta)), ...), target), silent = TRUE)` / `if (!inherits(saved, "try-error")) store$commit()` — nothing else looks at `saved`; `store$dump()` (the Arrow copy of every bound table, utils_dta.R:5273-5299, which re-throws write errors) is evaluated inside the same `try`. `saveRDS()` also writes straight onto the previous snapshot, so a write that dies half-way destroys the last good one.
- Failure scenario: tempdir() fills up (each loose upload is held three times: Shiny's copy, the staged copy — F-s1-app-10 — and the Arrow copy). From then on every `autosave()` fails silently. The user edits for an hour, reloads the page, clicks "Restore previous session": the document comes back as it was before the disk filled, or the truncated .rds yields "Could not restore the previous session."
- Confidence: verified-by-reading
- Suggested direction: write to a temp file and rename; on failure tell the user once (sticky notification) that the session is no longer being saved.

### F-s1-app-14  [minor] [code-cheat]
- Where: inst/shiny/dta_app/app.R:4781-4792 (`sync_yaml_text`); same pattern at 1525-1526, 1649-1650, 2435-2436, 2934-2935, 10160-10165, 10182-10184
- What: When the document cannot be serialised after an edit, nothing is reported: the Raw YAML tab keeps showing the pre-edit text, a new document starts with an empty YAML text, a version baseline silently stays stale, and "Embed YAML" silently embeds nothing.
- Evidence: `if (isTRUE(res$ok)) { rv$yaml_text <- res$value; set_yaml_editor(res$value) }` then `autosave()` — no `else`; `yaml_text <- if (isTRUE(yres$ok)) yres$value else ""` (2436, 2935); `if (isTRUE(yres$ok)) rv$version_baseline_yaml <- yres$value` (1526); `if (isTRUE(res_yaml$ok)) variables[["{YAML_EMBEDDED}"]] <- res_yaml$value` (10162-10164) followed by "Document exported successfully".
- Failure scenario: an edit produces a document `dta_to_yaml_text()` refuses. The toast says "Column saved."; the Raw YAML tab still shows the old text, and "Apply changes" there now REVERTS the edit, the opposite of what the comment in `save_md()` (8866-8867) says the sync is for. With "Embed YAML specification at end of document" ticked, the Word file is delivered without it.
- Confidence: verified-by-reading (the YAML download itself re-serialises and does report, 9833-9837)
- Suggested direction: surface `res$error` (notification + a "Raw YAML is out of date" marker) instead of dropping it.

### F-s1-app-15  [minor] [code-cheat]
- Where: inst/shiny/dta_app/app.R:9690-9716, 9725-9738, 9775-9783 (`apply_yaml`); help text 9605-9608
- What: "Apply changes" reports "✔ Valid — document applied." while silently discarding parts of what was applied: an edited `version` / `version_history`, the loaded data of any dataset whose transfer fails, and any table whose unload fails.
- Evidence:
  `tr <- dta_transfer_bound_data(...)`; `if (!isTRUE(tr$ok)) next` (9690-9694) — `tr$error` is dropped and the dataset arrives empty;
  `uv <- dta_unload_table(new_dta, nm, rec$table); if (isTRUE(uv$ok)) new_dta <- uv$value` (9714-9715) — on failure the table stays bound with no upload record, the state the comment above it calls "bound to the dataset and unreachable";
  `S7::prop(new_md, "version") <- S7::prop(keep_md, "version")`; same for `version_history` (9734-9735), then `rv$yaml_msg <- list(ok = TRUE, error = NULL)`.
  The hint lists what is kept and cleared but says nothing about the version fields, which ARE editable two tabs away (`md_version`, 8648 / 8919-8926).
- Failure scenario: change `version: "1.0"` to `"1.1"` in the editor, Apply: green "Valid — document applied.", toast "DTA YAML applied…", version still 1.0 (the editor is quietly re-filled with the old value). Or: a dataset's loaded file disappears after Apply with the toast still saying "loaded files kept where a slot remains for them".
- Confidence: verified-by-reading
- Suggested direction: say what was not applied (version fields) and name every dataset whose data could not be carried over.

### F-s1-app-16  [minor] [code-cheat]
- Where: inst/shiny/dta_app/app.R:8910-8926, 8839-8842
- What: An emptied Title or Version field is silently not saved while the form states "Changes are saved automatically … as you type".
- Evidence: `# title and version are REQUIRED (non-nullable); block saves of empty/blank values` / `if (!is_blank) save_md("title", v)` — no message in the other branch.
- Failure scenario: clear the Title to retype it later and export: the document still carries the old title although the field on screen is empty; nothing said so.
- Confidence: verified-by-reading
- Suggested direction: show an inline "required — not saved" note (or restore the stored value) when a required field is blank.

## Area 4 — modal handling

### F-s1-app-17  [minor] [bug]
- Where: inst/shiny/dta_app/app.R:5009-5016 + 5143, 5628-5655 + 5657 + 5857-5895, 6867-6897 + 6899 + 7085-7122, 7491-7514 (file / column / rule / details editors); contrast 2497-2520 + 2841-2843, 4145-4149 + 4320, 7646-7652 + 7804
- What: Four editor dialogs keep their body in a `uiOutput` without the "render NULL while closed / never suspended" treatment the picker, the container manager and the matrix dialog have, so each re-open first shows the PREVIOUS body (possibly another dataset's) for the length of the fade — and the column/rule list's stale table re-reports its old search term after the server reset it.
- Evidence:
  the file's own description of the mechanism: "Shiny's client keeps the LAST value of every output and re-inserts it the moment the output binds again … and then reports the inputs inside that stale copy as if the user had just set them … Seen happen, not theorised." (2498-2506); the fresh render waits for `shown.bs.modal` (modal_unsuspend_js, 263-328);
  `rv$col_search <- ""` "A fresh editor starts unfiltered" (5644-5646), but the re-inserted `col_tbl` runs `reorder_table_callback()`'s `sync();` -> `Shiny.setInputValue('col_tbl_search', table.search(), {priority: 'event'})` (5111-5121) -> `observeEvent(input$col_tbl_search, { rv$col_search <- input$col_tbl_search %||% "" })` (5893-5895), and the fresh render then uses `search = list(search = isolate(rv$col_search))` (5884). Same for rules (6889, 7111, 7120-7122).
  The stale list is live: its row buttons call `Shiny.setInputValue('col_del_click', i, …)` and `col_del_click` resolves `i` against the NEW `rv$editor_dataset` (6227-6236).
- Failure scenario: in dataset A search the column list for "date", edit a column, Close. Open Edit ▸ Columns on dataset B: A's filtered list flashes, then B's list arrives already filtered by "date" (maybe "No matching records") with reordering switched off. A click on a bin in the flashed list deletes column #i of B.
- Confidence: inferred — the R side is traced; the client-side re-insert and its timing are taken from the file's own comments and Shiny's `bindOutput`, not reproduced in a browser
- Suggested direction: give these four bodies the same closed-state NULL + `suspendWhenHidden = FALSE` handling (the matrix dialog's comment, 7648-7652, already states the rule).

### F-s1-app-18  [minor] [bug]
- Where: inst/shiny/dta_app/R/template_ui.R:337-343 (consumed by inst/shiny/dta_app/app.R:2508-2513, 2562-2567); cf. app.R:373-381 and tests/testthat/test-shinyapp-modal-scroll-lock.R:54-64
- What: The template picker's "modal closed" report is a NATIVE `document.addEventListener('hidden.bs.modal', …)` — the one kind of listener app.R itself documents (and a test pins) as never reached for a Shiny modal — so `input$template_picker_closed` is never sent and the picker's "render NOTHING while closed" rule does not operate in a browser.
- Evidence:
  template_ui.R:337 `"    document.addEventListener('hidden.bs.modal', function(ev){"` … `Shiny.setInputValue('template_picker_closed', Date.now(), {priority: 'event'});`
  app.R:378-381 "A jQuery handler, not addEventListener …: Shiny removes the dialog from the page inside its own jQuery 'hidden' handler, and Bootstrap sends the native event only after that, from a node that is no longer in the document, so it never reaches `document`."; the scroll-lock test is literally named "it listens through jQuery, because the native 'hidden' never reaches document" and asserts `expect_no_match(js, "addEventListener('hidden.bs.modal'")`.
  app.R:2567 `observeEvent(input$template_picker_closed, tmpl_picker_open(FALSE))` is the only place `tmpl_picker_open` becomes FALSE; the server tests set the input by hand (test-shinyapp-template-ui.R:449, 529).
- Failure scenario: open the picker once, Cancel. `tmpl_picker_open()` stays TRUE for the rest of the session, the three never-suspended outputs keep their full HTML, and every re-open re-inserts the cached frame and list (old checked radio, old library filter) before the tick-driven render replaces them — the "seen happen" behaviour of 2498-2506, now hidden only by the fallback the comment calls "a close whose report never arrived". The comments at 2508-2513 / 2562-2565 describe a mechanism that does not run, and two tests assert it by simulating the report.
- Confidence: inferred — both scripts and their consumers are read; that the native event does not reach `document` is taken from this repository's own documented finding and test, not re-observed
- Suggested direction: send the report from the existing jQuery `dta_modal_state` channel (stamp the picker with `data-dta-modal`), as the container manager does (4387-4397).

## Area 5 — user-visible text that is wrong

### F-s1-app-19  [minor] [doc-wrong]
- Where: inst/shiny/dta_app/app.R:10875 (vs 10939-10943, 2266-2268; inst/shiny/dta_app/R/utils_dta.R:269-282)
- What: The landing page states "A dataset-only file opens without a Metadata section." — it opens WITH the Metadata tab, wrapped in a new document, and says so in its own toast.
- Evidence: `"A full DTA with metadata or a standalone dataset specification is accepted. A dataset-only file opens without a Metadata section."`; `# The Metadata tab is always available: a loaded dataset YAML is wrapped into a full (empty-metadata) DTA that the user can complete.` / `nav_panel("Metadata", uiOutput("metadata_editor"))`; toast "Dataset loaded into a new DTA — add metadata to complete it." The sentence also hard-codes "DTA" where the heading above it honours `cfg$document$abbr`.
- Failure scenario: a user loading `gf_dataset.yaml` reads on the landing page that there will be no metadata, then gets a Metadata tab and a toast asking to complete it.
- Confidence: verified-by-reading
- Suggested direction: reword ("…a dataset-only file is opened inside a new, empty document") and use the configured abbreviation.

### F-s1-app-20  [minor] [doc-wrong]
- Where: inst/shiny/dta_app/app.R:9600-9603, 4316, 4174, 10684, 10702, 4666, 1896
- What: Several hints send the user to a control that is not on screen in the state the hint appears in — edit-only controls in view mode or in a deployment with `features.editing: false`, and the Raw YAML tab when `features.raw_yaml_tab` is off.
- Evidence:
  9602 `"This document is <b>read-only</b>. Turn on <b>Edit mode</b> (top right) to change it."` and 4316 `"View only — switch on Edit mode to change containers."` are also shown when `!cfg$features$editing`, where `output$edit_gate` renders nothing (1428);
  10684 "Use + Add dataset in the sidebar to create the first one" and 4666 "add one with + Add dataset before validating" — the button exists only while `editing()` (3074-3077);
  10702 "Use Edit > Files to add the files you expect to receive." and 4174 "Add the expected files under Edit ▸ Files first." — the dataset Edit menu is rendered only `if (editing())` (10787);
  1896 `"%s: %d error(s), %d warning(s) — see the Raw YAML tab."` — the tab, and with it the only place the findings are listed (`output$spec_check`, 10962), exists only `if (cfg$features$raw_yaml_tab)` (10944).
  The file knows the rule: "Read-only has no "Add person" button to point at, so the empty state must not tell the user to click one." (9164-9165).
- Failure scenario: open a specification whose dataset has no file entries and stay in view mode: the Datasets tab says "Use Edit > Files", and there is no Edit menu on the page. With `raw_yaml_tab: false` a loaded document with lint warnings points at a tab that does not exist and its findings cannot be read anywhere.
- Confidence: verified-by-reading
- Suggested direction: branch these texts on `editing()` / the feature flags as `render_contacts()` does, and show the spec-check list somewhere that is always present.

### F-s1-app-21  [minor] [doc-wrong]
- Where: inst/shiny/dta_app/app.R:8020-8026 (vs 7930-7933)
- What: For a dataset that has never been checked the messages dock states "No validation messages." (badge "0") — the very reading the file calls misleading when it disables the exports.
- Evidence: `return(DT::datatable(data.frame(Message = "No validation messages."), …))` whenever `msgs_r()` is empty; 7930-7933: "Exporting anyway would hand back a file that reads as a clean result ("No validation messages for this dataset.") when in truth nothing was ever looked at".
- Failure scenario: load data, do not press Check, unfold the dock: "No validation messages." under a dataset whose nav row says "Not validated yet".
- Confidence: verified-by-reading
- Suggested direction: say "Not checked yet" (or "No data loaded") until `msgs_active_checked()` is TRUE.

### F-s1-app-22  [nit] [doc-wrong]
- Where: inst/shiny/dta_app/app.R:10438-10460 (vs inst/shiny/dta_app/R/utils_dta.R:823-848, 1351-1357)
- What: The tooltip of the "unknown" file state tells the user to "Re-run validation (force)", an R argument the app has no control for; the state itself only exists for artifacts "written before the import axis existed".
- Evidence: `"Import status unknown — validated before import checking."`, `"Re-run validation (force) to complete the check."`; `dta_check()` calls `DTAtools::check(dta, quiet = TRUE, persist = FALSE)` — never `force = TRUE`.
- Failure scenario: none in normal use (the state needs a pre-import-axis artifact, and the app runs with `persist = FALSE`); if it ever shows, the advice cannot be followed in the app.
- Confidence: verified-by-reading
- Suggested direction: remove the compat state (no backward compatibility is needed), or make Check force a re-run for such tables.

### F-s1-app-23  [nit] [inconsistency]
- Where: inst/shiny/dta_app/app.R:7493-7497 vs 7580, 7643; 3764-3780; 822, 1028; 474-497 vs 554
- What: Four small text/comment slips. (a) The "Edit details" dialog — whose comment insists the title must read like the menu label, "not the internal name" — has a button "Save metadata" and a toast "Metadata updated.", colliding with the document-level Metadata tab. (b) `slot_delivery_note()` prints "Loaded only X's files from  (dm.csv). Its other 3 files were not loaded." when the files loaded from a slot drop were the loose ones (`archives` is empty). (c) "Last delivery, 14:02" is `format(last$at %||% Sys.time(), "%H:%M")`: server time zone, and "now" if the time were ever missing. (d) The 23-line comment explaining `yaml_ace_resize_js` (474-496) sits directly above `copy_yaml_js`; the script it describes (554) has none.
- Evidence: 7580 `actionButton("meta_save", "Save metadata", …)`, 7643 `showNotification("Metadata updated.", …)`; 3769-3773 `archives <- unique(loaded$delivered[dta_is_container_name(loaded$delivered)])` / `sprintf("Loaded only %s's files from %s (%s).", …)`; 822.
- Failure scenario: (b) drop `bundle.zip` + `dm.csv` on the `dm` slot when the archive holds nothing for `dm`.
- Confidence: verified-by-reading
- Suggested direction: "Save details" / "Details updated."; name the loose files as such; format the time in the browser; move the comment.

## Area 6 — dead code and compatibility leftovers (owner's priority 4)

### F-s1-app-24  [minor] [dead-code]
- Where: inst/shiny/dta_app/app.R:11054-11063, 11068-11090 with 11018-11035 and 11193-11194, 11111-11116, 11141-11148, 11153-11177, 2968-2970, 10438-10460; why they are unreachable: 1696-1699 and inst/shiny/dta_app/R/utils_dta.R:5331-5333
- What: `restore_session` and its helpers carry a whole layer of "session file written by an older version" compatibility that cannot be exercised by a deployed app: the autosave lives in `tempdir()`, which belongs to one R process, and one process never runs two package versions.
- Evidence: `file.path(tempdir(), paste0("dtatools_app_session_", …, ".rds"))` (1696-1699); "Everything this app writes that carries a user's data … lives in tempdir()" (utils_dta.R:5331-5333).
  The branches: "fall back to a legacy `dta` field" (11055-11063); `upgrade_dta()` failing -> `rebuild_from_yaml()` -> toast "Previous session restored from its YAML text: it was saved by an older version of the app." and the `dta_matrix_format_outdated` text (11068-11090, 11193-11194); "Legacy session files stored a bare character vector of paths" (11111-11116); "a session file written before this fix can already CARRY the NA" (11141-11148); "A session file written before the versioning feature existed has none of these fields" (11153-11177); "A structure restored from a session saved before labels existed" (2968-2970).
- Failure scenario: n/a — dead weight (≈110 lines plus the rebuild path) that every reader must reason about; the rebuild path also restores `saved$status` (11127) for data whose validation it just dropped, so if it ever did run it would show stale "pass".
- Confidence: verified-by-reading (reachable only by reloading newer code into the SAME R process, i.e. a developer's `load_all()`)
- Suggested direction: delete the legacy readers; refuse a snapshot that does not match the current shape.

### F-s1-app-25  [minor] [dead-code]
- Where: inst/shiny/dta_app/app.R:1250, 1869, 1904, 1924, 2261, 2330, 4782-4784, 9756, 10499-10516, 11151 (`dataset_only`); 1306, 1506, 1570, 1639, 1871, 1937, 10345, 11158 (`version_locked`); 1966-1969, 1987; 5423-5424, 5902-5904, 7129-7130; 9858-9869; 2994; 3655, 3684; 10795-10799; inst/shiny/dta_app/R/template_core.R:48, 65
- What: Further leftovers that no path can reach or that nothing reads.
- Evidence:
  `dataset_only = FALSE, # legacy flag; … (never set TRUE)` — `dta_read_yaml()` documents it as "always FALSE now … retained for back-compat with callers/old sessions" (utils_dta.R:278-279, 301, 314), yet it still selects a branch in `sync_yaml_text()` (4782-4784, a branch that can no longer be taken) and three in `output$workspace_header` ("Dataset — no metadata" pill);
  `rv$version_locked` is assigned in six places and read only by `autosave()`: "it no longer gates editing and no longer drives any UI" (1361-1363);
  `identical(as.character(val %||% ""), "yes")` "covers a restored autosave" (1966-1969) — the autosave stores no input values (1857-1876);
  `if (isTRUE(m$ok)) div(…, " File saved.")` / " Column saved." / " Rule saved." — `rv$file_msg`, `rv$col_msg`, `rv$rule_msg` are only ever NULL or `list(ok = FALSE, …)`;
  `output$dl_docx`: "No button points here";
  `ic_cls <- paste0("nav-ic-", if (st2 == "pending") "pending" else st2)` (both arms are `st2`);
  `copies <- staged[staged != fileinfo$datapath]` — `dta_stage_upload()` "never returns the client's datapath" (utils_dta.R:517-518), so the filter removes nothing;
  commented-out UI (10795-10799); `list_dta_creation_templates()` / `get_dta_creation_template_path()` "still exist … but have no remaining callers in the app" (2455-2458; grep confirms only their definitions).
- Failure scenario: n/a.
- Confidence: verified-by-reading (each symbol grepped across app.R and the app's R/)
- Suggested direction: remove them; `rv$tmpl_carry_upload` and `rv$template_variant_selections` (2060-2127, 2897-2904), by contrast, are used but missing from the `reactiveValues()` declaration at 1241-1328.

## Area 7 — accessibility basics and smaller inconsistencies

### F-s1-app-26  [minor] [design]
- Where: inst/shiny/dta_app/app.R:5871-5874 and 7098-7101 (vs 5040-5046, 5398-5402); 3025-3030; 10474-10477; 10464; 655-661 with 10220-10229
- What: Accessible names are missing where the file's own comment says they are needed, and an invisible download link is in the tab order.
- Evidence:
  `row_action_buttons("col_edit_click", "col_del_click", nrow(ov), "col_up_click", "col_down_click")` and the rule equivalent pass no `labels`, so every row offers the same bare ✎ / 🗑 / ▲ / ▼ — "a screen reader hears "Edit sdtm_*.zip", not a row of identical pencils" (5040-5041) holds for the Files and Containers tables only;
  `actionButton(paste0("checkds_", i), label = HTML("&#x25B6;"), …, title = sprintf("Check '%s'", label))` and `actionButton(paste0("rmfile_", fid), label = HTML("&#x1F5D1;&#xFE0F;"), …, title = "Remove this file")`: glyph as the name, `title` only; the per-file status glyph (10464) has a `title` but neither `role` nor `aria-label`, unlike the nav icon (3015);
  `div(style = "position: absolute; left: -9999px; …", downloadButton("export_trigger_download", ""))`: an enabled, focusable, unlabelled link. `export_state` is never cleared (not in `confirm_reset`, 10329-10363, nor `apply_loaded`), so Enter on it downloads the last export of a document that may no longer be open, or a 404 named "export.txt".
- Failure scenario: keyboard/screen-reader use of Edit ▸ Columns: a table of indistinguishable buttons, one of which deletes a column without confirmation (6227-6252).
- Confidence: verified-by-reading
- Suggested direction: pass `labels = ov$id` to both tables, add `aria-label` to the glyph buttons, and give the hidden link `tabindex="-1"` + `aria-hidden="true"`.

### F-s1-app-27  [nit] [inconsistency]
- Where: inst/shiny/dta_app/app.R:1922 vs 3217 and 9749-9754; 6254-6299 and 7207-7247 vs 6251, 6352, 7204, 7472; 4609-4644 vs 3233-3240; 4547-4556 vs 2311-2314; 2779-2782, 2851-2885 vs 1234-1237; 7987-7993 with 8114-8127; 3291-3293, 4585-4588, 3536
- What: A handful of behaviours that contradict a sibling a few lines away.
- Evidence:
  status of a dataset without data: "pending" after a load (1922), "nodata" after "+ Add dataset" (3217), an applied YAML (9749-9754) or any Check (`dta_status_map()`);
  moving a column or rule clears the validation and leaves "pending", while saving/removing one re-checks at once ("so the status never reads "pending" for data whose result the new spec already determines", 4805-4808);
  "Discard all" acts on `rv$active` at confirm time although the remove-dataset dialog stashes its target for exactly that reason (3233-3240);
  "Load file" with nothing chosen closes the example dialog silently; its landing twin warns "Choose an example specification first.";
  `tmpl_refresh_templates`, `template_options_back`, `template_variant_back/confirm` lack the `cfg$features$create_from_template` gate that "every feature gate below is enforced here on the server" (1234-1237) promises — with the feature off, a websocket message still opens the picker and lists the library;
  the XLSX button is offered whenever the config lists it, and only the click finds out that `writexl` (a Suggests) is missing;
  upload keys are `paste0(<dataset id>, "||", <slot>)`, matched with `startsWith()` / split on "||", and notice ids are `gsub("[^A-Za-z0-9_-]", "_", id)` — while a dataset id may be any non-empty string (R/DTADataSet-class.R:87-90), so ids such as `a` and `a||b`, or `a b` and `a_b`, collide.
- Failure scenario: e.g. remove dataset `a` in a document that also has `a||b`: the upload records of `a||b` are dropped with it (3291-3293).
- Confidence: verified-by-reading
- Suggested direction: one status rule (`dta_status_map()`), re-check (or do not invalidate) on reorder, stash the dataset for Discard, gate the four template observers, hide XLSX without writexl, and key uploads by dataset index or a list instead of a pasted string.

## Cross-checks asked for by the coordinator

- **F-t-shiny-1-14 (emptied vocabulary slot yields the full vocabulary) — CONFIRMED from app.R.** `collect_vocab_selections()` does record the emptied control as `character(0)` (app.R:2026-2034; `out[[id]] <- val[…]` keeps a zero-length element) and `template_create_confirm` passes it on (2906, 2921-2926). But `vocabulary_slot_values()` returns `NULL` for it (`if (length(selection) == 0) return(NULL)`, vocabulary.R:809-811), the same value as "slot left alone", and `resolve_vocabulary_slot_overrides()` skips NULL (860-863). So the app.R comment at 2013-2019 ("`vocabulary_slot_values()` reads character(0) as "no terms"… the "None" link above it would be a lie if clearing the chips silently put the default back") is false as written, and the dialog's own counter (2160-2175) shows "0 of n selected" for a slot that will be created with every term. No app.R-side guard catches it.
- **F-t-shiny-1-34 (upload guard exempts MockShinySession) — CONFIRMED.** app.R installs the guard once per session (1737-1739) and `accept_upload()` (1740-1747) is its only consumer; all four file inputs go through it (`tmpl_carry_file` 2058, `dta_file` 2226, `up_<n>_<n>` 3929, `delivery_files` 3982 — `fileInput(` occurs nowhere else in the app). With no `onInputReceived` on a mock session, `upload_guard$take()` can only be TRUE when a test calls `note()` by hand, so the session binding is never exercised under `testServer()`; only the shape check `dta_upload_value_problem()` runs. In production the class-name branch is unreachable (a real session has the callback) — it is test scaffolding inside app code.
- **F-t-shiny-1-10 (XSS escaping of the editor tables) — app code is CORRECT in the pinned tree; the test weakness stands.** At all three call sites the generated column is appended last and is the only unescaped one: `ov$Actions <- …` then `escape = -ncol(ov)` (file_tbl 5397-5408, col_tbl 5870-5880, rule_tbl 7097-7107). The only user text inside that column is the aria-label, escaped with `htmltools::htmlEscape(labels[[i]], attribute = TRUE)` (5045); the container list's `HTML(actions[[i]])` (1224) receives the same escaped strings (4290-4292). `output$msgs` uses DT's default `escape = TRUE` (8047-8055). No app-side XSS found; nothing in app.R enforces "Actions is last", so the reviewer's mutation would indeed go unnoticed.
- **F-t-shiny-2-10 (suspendWhenHidden test cannot fail) — app side checked:** `delivery_flag()` does call `outputOptions(output, id, suspendWhenHidden = FALSE)` (10614) and the sidebar loop exists (10647-10653).


## Area 8 — reactive correctness: status map and metadata saves

### F-s1-app-28  [blocker] [logic-error]
- Where: inst/shiny/dta_app/app.R:4697-4705 (`run_check`), with 3514-3516, 3825-3827, 3485-3491; root cause inst/shiny/dta_app/R/utils_dta.R:1268-1277 (already reported there as F-s2-shiny-r-03 — this entry adds how app.R reaches it)
- What: `run_check()` replaces the WHOLE status map with `dta_status_map()` after checking any subset, which silently turns a dataset the app itself had marked "pending" (a file added or replaced since its last check) into "pass" although that file was never validated.
- Evidence:
  app.R:3514-3516 after an upload: `st[[dsname]] <- "pending"`; 4700-4704: `res <- dta_check(rv$dta, dataset = ready)` … `rv$status <- dta_status_map(rv$dta)` — every dataset, not only `ready`;
  utils_dta.R:1271-1274: `if (!is.na(inv) && inv > 0) "fail" else if (done > 0 || val > 0) "pass"` — `n_not_validated` is never read; `load_file()` keeps the other tables' verdicts when a NEW table is bound (R/DTADataSetTabular-class.R:969-972) and `validation_status()` reports the new one as `not_validated`, `ok = NA` (1202-1223).
- Failure scenario: dataset `lab` (pattern entry, several files) passed with `lab_1.csv`. Upload `lab_2.csv` -> nav row "Not validated yet". Press ▶ on dataset `dm`, or just save a column or rule of `dm` (`recheck_dataset()` -> `run_check(ed, quiet = TRUE)`, 4809-4813): `lab` turns green "Passed all checks", the sidebar offers the green "Validation summary" ("Download a summary certifying this successful validation", 10276-10277) and its banner is built from this map (utils_dta.R:2452-2470) — while "Loaded files" still shows `lab_2.csv` as "Not validated yet". Same after an overwrite (3487-3491) or a delivery (3825-3827) into a dataset with other validated tables.
- Confidence: verified-by-reading
- Suggested direction: derive "pass" only when no bound table is `not_validated` (fix in `dta_status_map()`), and in `run_check()` only overwrite the statuses of the datasets that were checked.

### F-s1-app-29  [minor] [bug]
- Where: inst/shiny/dta_app/app.R:8860-8990, 9371-9394 (debounced field observers), 4781-4792 (`sync_yaml_text`), 9574-9580 (`set_yaml_editor`); contrast 8929-8947
- What: Every text/flag field of the Metadata form echoes its freshly rendered value back as a "change" (~25 fields per render), and each echo runs the full save path — re-serialise the document, autosave, and overwrite the Raw YAML editor — so merely opening the Metadata tab in edit mode discards unapplied text in the Raw YAML editor.
- Evidence:
  `observeEvent(title_d(), { … if (!is_blank) save_md("title", v) }, ignoreInit = TRUE)` and its siblings fire when the input goes from NULL (or the previous document's value) to the rendered one; `save_md()`/`save_tr()`/`save_study()`/`save_affiliation()` all end in `sync_yaml_text()` unconditionally, which calls `set_yaml_editor(res$value)` -> `shinyAce::updateAceEditor(session, "raw_yaml_editor", value = …)` and `autosave()`;
  only one field is protected: "The guard makes a re-render (document load, edit toggle) a no-op instead of a spurious edit" (`md_authorized`, 8929-8944).
- Failure scenario: create a document from a template (edit mode, Datasets tab showing), type changes into the Raw YAML tab without pressing Apply, then click the Metadata tab to look something up: 0.7 s later the Raw YAML editor is reset to the server's text and the typed changes are gone; meanwhile the document was serialised and autosaved once per field although nothing was edited.
- Confidence: verified-by-reading (server side; the editor's content being replaced by `updateAceEditor(value=)` is shinyAce's documented behaviour, not run)
- Suggested direction: skip the save when the incoming value equals what the document holds (as `md_authorized` does), and do not push text into an editor that holds unapplied changes.

---

## Coverage

- **Fully read:** `inst/shiny/dta_app/app.R`, lines 1-11206 (the whole slice), top to bottom.
- **Opened to verify findings (not reviewed as a slice):**
  - app helpers: `inst/shiny/dta_app/R/containers.R` (all 418 lines); `R/utils_dta.R` 1-130, 176-347, 480-860, 1195-1430, 2395-2535, 2925-2965, 3335-3410, 3640-3700, 4380-4640, 5180-5789; `R/utils_export.R` 290-472; `R/ui_components.R` 1-180; `R/theme.R` 1570-1628; `R/vocabulary.R` 150-185, 500-575, 770-870; `R/template_ui.R` 300-345; `R/app_config.R` (grep + 660-678).
  - package: `R/evaluateRules.R` 320-390, 630-950; `R/containerFindings.R` (all); `R/containerAssignments.R` (all); `R/loadDelivery.R` 520-580 (+ grep); `R/validationReporting.R` 1-175; `R/DTADataSetTabular-class.R` 955-976, 1176-1235; `R/DTADataSet-class.R` (id validator, grep); `DESCRIPTION`; `NAMESPACE` (grep); `DTA_YAML_GUIDE.md` 612-672, 1066-1095; `inst/extdata/*.yaml` (grep for rule operators).
  - other reviewers' findings: `findings/t-shiny-1.md` (F-10, F-14, F-18, F-26, F-34), `findings/t-shiny-2.md` (F-10), `findings/s2-shiny-r.md` (headings + F-03).
- **Only skimmed / not read:** the rest of the app's `R/` helpers (template engine, theme CSS, versioning, most of utils_dta.R), `www/`, and all tests except the greps noted above — they belong to other slices.
- **Not done (by the rules of engagement):** nothing was executed. Every statement about browser-side behaviour (a `<select>` with no selected option, Shiny's output cache on re-bind, `updateAceEditor`, the native `hidden.bs.modal` event) is marked as such in the finding; F-s1-app-17 and F-s1-app-18 are `inferred` for that reason and are worth one run in a painting browser.
- **Looked at and found sound (no finding):** the upload guard's coverage of all four `fileInput`s; whitelisting of client-supplied names (`dta_example_yaml_path`, `dta_example_data_path`, `get_template_path`); the client-id validation and session-file naming; escaping in every `HTML()` call fed by document text (`row_action_buttons`, `create_new_body`); the Backend-select echo guard (`col_shown_backend`); the remove-dataset / rename state migration (apart from `rv$last_delivery`, F-11); the engine phrases `dv_reason()` / `dv_result_word()` / `dta_app_reason()` match by text (they agree with R/loadDelivery.R and R/containerFiles.R at this tree, though nothing pins that).
