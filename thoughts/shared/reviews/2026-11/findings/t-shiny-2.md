# Findings — slice t-shiny-2

Slice: Shiny-app tests, second half (`tests/testthat/test-shinyapp-*.R`, 29 files,
~20k lines) — see `scratchpad/slices/t-shiny-2.txt`. Shared helpers read for context:
`tests/testthat/helper-*.R`.
Pinned tree: `$SNAP` = scratchpad/snapshot (68408c44).

Method: read-only; no R run, no server/browser started. Every finding is marked
`verified-by-reading` or `inferred`. Paths are relative to `$SNAP`.

Files fully read: all 29 files of the slice plus helper-shinyapp.R,
helper-containers.R, helper-source-tree.R, helper-unchecked-collection.R — see
"Coverage" at the end. Findings F-t-shiny-2-01 … F-t-shiny-2-49.

Cross-slice note (for t-shiny-1 and anyone reading testServer tests): in shiny
1.14.0 (pinned in renv.lock) `MockShinySession` implements these session
methods as no-ops returning `invisible()` (`makeWarnNoops(...)` list read from the
installed `shiny/R/shiny.rdb`): allowReconnect, decrementBusyCount, doBookmark,
exportTestValues, flushOutput, getTestSnapshotUrl, incrementBusyCount,
manageHiddenOutputs, manageInputs, onBookmark(ed), onInputReceived, onRestore(d),
**outputOptions**, reactlog, registerDataObj, reload, resetBrush,
sendBinaryMessage, sendChangeTabVisibility, **sendCustomMessage**,
**sendInputMessage** (every `update*Input()`), sendInsertTab, **sendInsertUI**,
**sendModal**, setCurrentTheme/getCurrentTheme, **sendNotification**,
sendProgress, sendRemoveTab, **sendRemoveUI**, setShowcase, showProgress,
updateQueryString. Any testServer assertion that needs one of these to have an
observable effect (an input changed by `update*Input()`, a custom JS message, an
`outputOptions()` read-back) without mocking the shiny function is vacuous. See
F-t-shiny-2-10 for the instance in this slice; click-guard, modal-scroll-lock,
modal-outputs and upload-guard tests (t-shiny-1) are the likely other places.

---

## tests/testthat/test-shinyapp-containers.R (fully read)

Visibility rule (file-drop fix): the gate test (L97-134) DOES pin the new rule in
both directions — `two` (2 datasets, no containers) must give `delivery_zone`/
`delivery_block` FALSE (catches a revert to "has datasets" or "> 1 dataset"),
`one_container` (explicit default) and `implied` (file-entry reference only) must
give TRUE; L246-294 pin the edit-mode block and the add-first/remove-last
transitions through the real container manager. Verified against
`inst/shiny/dta_app/app.R:10598-10617`. Remaining gaps below.

### F-t-shiny-2-01  [minor] [test-weak]
- Where: tests/testthat/test-shinyapp-containers.R:136-154 (vs inst/shiny/dta_app/app.R:10660-10668)
- What: the hint test checks the "drop it on Delivery in the sidebar" pointer only for an EXPLICIT container (`ct_spec(ct_default)`), never for a document whose only container is implied by a file entry.
- Evidence: `named <- ct_spec(ct_default)` / `plain <- ct_spec()` are the only two inputs; app: `if (delivery_zone_on()) " To load a whole delivery ..."`.
- Failure scenario: change the hint's condition to `length(rv$dta@containers) > 0` (explicit only) — both servers still pass, yet a document with `container: sdtm_csv.zip` on an entry shows a drop zone (gate test L128-133) but a hint that never mentions it; the CHANGELOG promise "its hint names the sidebar zone only when there is one" is broken for implied containers without a red test.
- Confidence: verified-by-reading
- Suggested direction: add the `implied` spec (already built in the gate test) as a third server in the hint test.

### F-t-shiny-2-02  [minor] [test-weak]
- Where: tests/testthat/test-shinyapp-containers.R:97-134 (vs inst/shiny/dta_app/app.R:10598-10603)
- What: no test pins the `delivery_has_dataset()` half of the gate for a document that names containers but has no datasets.
- Evidence: every server in the gate test loads a spec with >= 1 dataset; `delivery_zone_on <- reactive({ delivery_has_dataset() && length(dta_container_rows(rv$dta)) > 0 })`.
- Failure scenario: drop `delivery_has_dataset() &&` from `delivery_zone_on`/`delivery_block_on` — a containers-only document (e.g. after "Remove dataset" of the last one) shows a drop zone that can bind nothing; the whole file stays green.
- Confidence: verified-by-reading (searched this file and test-shinyapp-delivery.R / -dataset-add-remove.R for a zero-dataset + containers case: none)
- Suggested direction: one server: containers section + remove the last dataset -> `expect_false(output$delivery_zone)`.

### F-t-shiny-2-03  [nit] [test-weak]
- Where: tests/testthat/test-shinyapp-containers.R:1030-1033
- What: the "works as a current document does" assertion reads back the value it has just assigned to a local copy, so it can only fail by erroring.
- Evidence: `h <- rv$dta@datasets$ae@files[[1]]; h@container <- "sdtm.zip"; expect_identical(h@container, "sdtm.zip")`
- Failure scenario: the expectation is a tautology; whatever the upgrade did, if `@<-` does not throw the expectation passes (the real guard is `expect_false(dta_is_stale(rv$dta))` two lines up).
- Confidence: verified-by-reading
- Suggested direction: drop the tautology, or assert something the app does with the reference (e.g. `container_assignments()` / `dta_container_rows()` on a document rebuilt with it).

### F-t-shiny-2-04  [minor] [dead-code]
- Where: tests/testthat/test-shinyapp-containers.R:903-1036 (and inst/shiny/dta_app/app.R:11054-11090, 11106-11120, 11153-11188)
- What: three tests pin backward-compatibility of the session restore (an RDS "saved by an older version": `upgrade_dta()` re-stamping, YAML-text rebuild fallback, a hand-aged pre-containers class tree via `ct_age()`/`ct_old_shape_class()`), i.e. compat machinery the owner says is not needed before 1.0.0.
- Evidence: `test_that("restore_session() upgrades a document saved by an older version", ...` ; `saved$dump$dta <- ct_age(saved$dump$dta)` ; app: `# A document saved by an older version has the classes it was saved with: re-stamp them` / `# Legacy session files stored a bare character vector of paths` / `# A session file written before the versioning feature existed`.
- Failure scenario: n/a (dead weight) — the tests keep ~130 lines of fixture-aging code and the app's legacy branches alive.
- Confidence: verified-by-reading (app branches read at app.R:11037-11203)
- Suggested direction: decide pre-1.0 policy; if no compat, drop the legacy restore branches and these tests (keep the "neither restorable" refusal).

### F-t-shiny-2-05  [nit] [inconsistency]
- Where: tests/testthat/test-shinyapp-containers.R:6-9 vs :946, :1013
- What: the file's header rule says files are written BEFORE `testServer()` starts because a `withr::local_*()` inside the block defers to an environment that never exits, but two tests call `cont_write()` (which does `withr::local_tempdir(.local_envir = parent.frame())`) inside the block.
- Evidence: L946 `session$setInputs(up_1_1 = ct_fi(cont_write("ae.csv", CONT_AE_CSV), "ae.csv"))`; L1013 same.
- Failure scenario: the temp dir is never cleaned by the test (or, depending on withr's handling of a non-frame env, deferred to an unrelated env) — the file contradicts its own stated invariant.
- Confidence: verified-by-reading (the rule violation); inferred (exact withr behaviour on the data-mask env not checked)
- Suggested direction: create the two files before `testServer()` like every other test in the file.

### F-t-shiny-2-06  [nit] [doc-wrong]
- Where: tests/testthat/test-shinyapp-containers.R:92-93
- What: the comment above `ct_set_input()` describes something else entirely.
- Evidence: `# A delivery directory's staged copies: not the extraction itself.` / `ct_set_input <- function(session, id, value) do.call(session$setInputs, ...)`
- Failure scenario: misleads a reader about a helper that just sets an input by a computed id.
- Confidence: verified-by-reading
- Suggested direction: "Set one input whose id is computed (e.g. amb_<nonce>_1)."

## tests/testthat/test-shinyapp-delivery.R (fully read)

Visibility rule: this file does not test the gate at all (only the container
hint, L444-474, for a document WITH a container) — it would pass with the rule
reverted; the gate is pinned in test-shinyapp-containers.R only (see above).

### F-t-shiny-2-07  [minor] [inconsistency]
- Where: tests/testthat/test-shinyapp-delivery.R:306-333, 335-397, 691-728, 730-755, 815-833, 977-1014, 1040-1068, 1095-1134, 1265-1292, 1544-1587; tests/testthat/test-shinyapp-containers.R:395-419, 1598-1623, 1774-1797 (vs inst/shiny/dta_app/app.R:3977-3991, 10601-10606, 10907-10924)
- What: since the file-drop fix the sidebar drop zone is hidden for any document without containers, yet these tests still drive `session$setInputs(delivery_files = ...)` on container-less specs (`dv_spec()`, `ct_spec()`), and the server observer has no matching gate — so the suite pins, as the main delivery behaviour, a flow no user can reach, and nothing pins what the server does with a `delivery_files` value while the zone is off.
- Evidence: test `dv_spec <- function(env = parent.frame()) dv_yaml(c(dv_header, dv_ae(), dv_dm), env)` (no container anywhere) then `session$setInputs(delivery_files = fi(zip1, "sdtm_csv.zip"))`; app observer `observeEvent(input$delivery_files, { ... req(rv$dta); handle_delivery(v) ...})` never consults `delivery_zone_on()`.
- Failure scenario: the gate is client-side only (`conditionalPanel("output.delivery_zone", ...)`); a crafted client (or a later UI change that re-exposes the input) still gets a whole-document delivery on a document "that has no delivery to drop", and the tests — which are the only executable description of that path — treat it as the normal case. Test names such as "a zip dropped on the Delivery zone loads every dataset it feeds" describe a UI that this fixture no longer has.
- Confidence: verified-by-reading
- Suggested direction: either gate the observer (`req(delivery_zone_on())`) and add a refusal test, or move these tests onto container-bearing specs so they describe a reachable flow.

### F-t-shiny-2-08  [minor] [test-weak]
- Where: tests/testthat/test-shinyapp-delivery.R:1265-1292 and 1544-1569 (helper second_tab() L1249-1263)
- What: the two "second tab" tests never assert that tab B actually restored the session (and so owns the delivery directory); if B's restore fails silently, B owns nothing, its close/Start over deletes nothing, and both tests pass without exercising the cross-tab ownership registry they are named for.
- Evidence: `tab_b <- second_tab(id, session); tab_b$close(); expect_true(file.exists(ae_path))` and `tab_b$setInputs(confirm_reset = 1); expect_true(file.exists(ae_path))`; `second_tab()` ends with `s$setInputs(restore_session = 1); s` and nothing checks the outcome (the app reports a failed restore only through `showNotification("Could not restore the previous session.")`, app.R:11044/11084, which `mock_notifications()` captures into `shown` but no assertion reads).
- Failure scenario: a change that makes B's restore return early (e.g. `session_file()` keyed differently for a bare `MockShinySession`, a `saved$user` mismatch, or `dta_restore_session()` erroring into the `tryCatch(..., error = function(e) NULL)` at app.R:11057-11060) leaves both tests green even with the registry fix reverted. (The file-dataset variant L1294-1324 is NOT vacuous: there tab A's own removal must be stopped by B's ownership.)
- Confidence: verified-by-reading (traced restore_session app.R:11037-11203 and onSessionEnded app.R:1757-1778)
- Suggested direction: assert B restored, e.g. `expect_true(any(grepl("Previous session restored", shown, fixed = TRUE)))` after `second_tab()`.

### F-t-shiny-2-09  [nit] [doc-wrong]
- Where: tests/testthat/test-shinyapp-delivery.R:703-706
- What: the comment explains a direct `up_2_1` drop, but the code below drops on `delivery_files`.
- Evidence: `# up_2_1 is dm's slot; in a browser it exists only while dm is the active` / `# dataset, testServer() accepts it regardless.` / `session$setInputs(delivery_files = fi(b1, "b.zip"))`
- Failure scenario: stale comment left from an earlier version of the test; misleads about which input is exercised.
- Confidence: verified-by-reading
- Suggested direction: delete the comment (or say it is a sidebar drop).

## tests/testthat/test-shinyapp-dataset-add-remove.R (fully read)

(Re-checked for F-t-shiny-2-02: no server test here removes the last dataset of a
document that names containers; the zero-dataset case L250-259 is a unit test
of dta_remove_dataset() without containers.)

### F-t-shiny-2-10  [blocker] [test-cheat]
- Where: tests/testthat/test-shinyapp-dataset-add-remove.R:283-301 (vs inst/shiny/dta_app/app.R:10609-10617, 10647-10653)
- What: the only test of the `suspendWhenHidden = FALSE` contract cannot fail: under `testServer()` `outputOptions(output, id)` is a no-op that returns NULL, and `isTRUE(NULL$suspendWhenHidden)` is FALSE, so `expect_false()` passes whatever the app sets — including for `delivery_zone` / `delivery_block`, the two flags the file-drop fix depends on.
- Evidence: `expect_false(isTRUE(outputOptions(output, id)$suspendWhenHidden), info = ...)`. In the installed shiny 1.14.0 (the version renv.lock pins) `MockShinySession`'s `outputOptions` is generated by `makeWarnNoops("allowReconnect", ..., "outputOptions", ...)`, whose method body is `private$noopWarn(name, msg); invisible()` (read from shiny's lazy-load DB: `R/shiny.rdb`, the `makeWarnNoops` call and closure). `outputOptions()` dispatches to `.subset2(output, "impl")$outputOptions(name)`, i.e. that no-op.
- Failure scenario: delete `outputOptions(output, id, suspendWhenHidden = FALSE)` from `delivery_flag()` (app.R:10614) — per the app's own comment (app.R:10588-10591) the hidden conditionalPanel then never receives TRUE, so the Delivery drop zone never appears; or delete the whole `for (sidebar_output in ...)` block (app.R:10647-10653), re-opening the user-reported "sidebar went blank" race the test header (L262-279) says is "pinned here". The test stays green in both cases. The `isTRUE()` wrapper is what turns "option not observable" into "pass".
- Confidence: verified-by-reading (test, app lines; grep of all of tests/ for `suspendWhenHidden`: the only other hit, test-shinyapp-modal-outputs.R:113, is about the modal JS, not these outputs); shiny internals verified by inspecting the serialized MockShinySession code of the installed shiny 1.14.0 (strings of the `makeWarnNoops` call and closure), not by reading shiny's source or running R. Cheap confirmation for the runner: nothing in tests/ sets `options(shiny.mocksession.warn = FALSE)`, so if the no-op is real every `testServer()` start should log "outputOptions is not fully implemented by MockShinySession" (the app calls `outputOptions()` ~10 times per server).
- Suggested direction: assert the value is present before asserting it (`opts <- outputOptions(output, id); expect_false(is.null(opts$suspendWhenHidden))`) — which will fail under testServer and show the contract needs a different harness (e.g. a static-source check that every listed id is passed to `outputOptions(..., suspendWhenHidden = FALSE)`, or a shinytest2/chromote run).

### F-t-shiny-2-11  [minor] [test-weak]
- Where: tests/testthat/test-shinyapp-dataset-add-remove.R:329-331 (vs inst/shiny/dta_app/app.R:10546-10577)
- What: "one dataset left in the overview count" is checked with `expect_match(metrics, ">1<")`, which any per-state count of 1 also satisfies, so the dataset total is not actually pinned.
- Evidence: `expect_match(metrics, "dataset", fixed = TRUE)` (also matches " datasets") / `expect_match(metrics, ">1<")`; the legend renders the total as `span(class = "status-n", n)` AND every non-zero state as `span(class = "status-n", k)`.
- Failure scenario: if the removal left `rv$dta` with 2 datasets while `rv$structure`/`rv$status` shrank (total renders `>2<`, but `item(nd, ...)`/`item(npend, ...)` render `>1<`), or `n` were computed off by one, both expectations still pass.
- Confidence: verified-by-reading
- Suggested direction: match the total precisely, e.g. `'status-total"><span class="status-n">1</span> dataset<'` (or compare against `length(dta_dataset_names(rv$dta))`).

### F-t-shiny-2-12  [nit] [doc-wrong]
- Where: tests/testthat/test-shinyapp-dataset-add-remove.R:180-187
- What: the regression-guard comment points at a non-existent "dta_to_yaml_text() above" and describes in-flight work ("a real bug another agent is fixing in R/DTAColumnSpecCollection-class.R") that is stale process chatter in a test file.
- Evidence: `# serialized YAML entirely (see dta_to_yaml_text() above). ... -- this is a real bug another agent is` / `# fixing in R/DTAColumnSpecCollection-class.R, not something to work around`
- Failure scenario: a reader looks for a helper "above" in this file (there is none) and for an open bug that is either fixed or tracked elsewhere.
- Confidence: verified-by-reading
- Suggested direction: state the guarded behaviour only (an empty `columns:` is dropped on write and a zero-column dataset is rebuilt on read).

### F-t-shiny-2-13  [nit] [doc-wrong]
- Where: tests/testthat/test-shinyapp-containers.R:357-360, 382-383
- What: the comment says the spec has no container, but the same test's spec gives `vs` the container `sdtm_csv.zip` — which the gate test (L127-133) itself counts as an implied container.
- Evidence: L358 `spec <- ct_spec(extra = ct_ds("vs", "{filename: vs.csv, type: csv, container: sdtm_csv.zip}", ...` / L382 `# No container in the spec: the packaging conforms, and something is bound.`
- Failure scenario: a reader concludes "Packaging conforms" is the no-container wording; it is in fact the verdict for a spec WITH an implied container whose member was not delivered.
- Confidence: verified-by-reading
- Suggested direction: "Only vs names a container, and its file was not delivered: what was loaded conforms."

(The final Coverage section is at the end of this file.)

## tests/testthat/test-shinyapp-render-coverage.R (fully read)

### F-t-shiny-2-14  [major] [test-weak]
- Where: tests/testthat/test-shinyapp-render-coverage.R:1-12 (vs inst/shiny/dta_app/app.R, e.g. :1462, :3074, :7124, :7585, :9330, :9365)
- What: at least 16 of the app's 70 `output$<id> <-` render/download functions are never executed by any test (testServer only runs an output a test reads), and this file — the suite's only "render coverage" — states stale numbers and has no check that keeps the unread set from growing.
- Evidence: header `# ... app.R defines 53 static \`output$<id> <-\` assignments ... 27 outputs remain unread after this file ... left for a future pass.` — app.R now has 70 unique `output$<id> <- ` assignments. A grep of every tests/testthat/*.R for `output$<id>` / `output[["<id>"]]` (no test uses `getOutput()` or a variable `output[[id]]`) finds none for: add_dataset_ui, create_new_body, create_new_msg, dl_ds_yaml, dl_msgs_tsv, dl_msgs_xlsx, dl_validation_summary, dl_yaml, export_filename_preview, export_trigger_download, meta_editor_msg, new_document_msg, new_version_msg, rule_editor_msg, supplier_affiliation, supplier_contacts (their ids appear only as button markup or `rv$*_msg` state, never as a rendered output).
- Failure scenario: a typo or a renamed helper inside `output$supplier_contacts` (its `receiver_contacts` twin IS read), `output$add_dataset_ui` (sidebar, every workspace in edit mode), `output$rule_editor_msg` or the `dl_yaml` / `dl_validation_summary` download content functions errors in every browser session while the whole suite stays green.
- Confidence: verified-by-reading (grep over all test files; the 16 ids checked individually). 16 is a lower bound: the grep counted any `output$<id>` text, comments and prose included, as a read.
- Suggested direction: add a meta-test that parses app.R's `output$<id> <-` ids and fails on any id no test reads (with an explicit, reviewed allow-list), and update the header numbers.

### F-t-shiny-2-15  [minor] [test-weak]
- Where: tests/testthat/test-shinyapp-render-coverage.R:74-76 (vs inst/shiny/dta_app/app.R:129-146)
- What: "aria-expanded kept in sync by the toggle script" is checked by finding the substring anywhere in app.R, not in the toggle path.
- Evidence: `src <- paste(readLines(file.path(.shiny_app_dir(), "app.R"), ...)); expect_match(src, "setAttribute('aria-expanded'", fixed = TRUE)`; the only occurrence is inside `DTA_syncMsgsDock()`, which `DTA_toggleMsgsDock()` and the `dta_msgs_dock` handler call.
- Failure scenario: delete the `DTA_syncMsgsDock(d);` call from `DTA_toggleMsgsDock()` — a click or Enter/Space folds the dock but `aria-expanded` stays "false"; the test still passes (the substring is still in the file, even if only in a comment).
- Confidence: verified-by-reading
- Suggested direction: read `msgs_dock_js` via `app_r_binding()` and assert the toggle function body calls the sync (or test in the browser suite).

## tests/testthat/test-shinyapp-modal-browser.R, -modal-reattach.R, -harness.R (fully read)

Checked: the skips in modal-browser.R do not always fire (Chrome is installed on
this machine at C:/Program Files/Google/Chrome; CI sets NOT_CRAN=true and
installs Suggests incl. shinytest2/chromote); the Bootstrap canary in
modal-reattach.R:79-91 finds all three strings in the installed bslib bundle;
app.R:630-631 really registers both snippets in `ui_page()`'s head.

### F-t-shiny-2-16  [minor] [test-weak]
- Where: tests/testthat/test-shinyapp-modal-reattach.R:44-71 and tests/testthat/test-shinyapp-modal-browser.R:173-189 (vs inst/shiny/dta_app/app.R `modal_reattach_js`)
- What: the behaviour of `modal_reattach_js` is pinned only by substrings of its source, plus a browser race the browser test itself says may not reproduce; the first browser scenario (dialog 2 requested after dialog 1 is fully shown) never reaches the wrapped `_showElement` at all.
- Evidence: reattach tests are `expect_match(js, "if (!document.body.contains(this._element)) return;", fixed = TRUE)` / `expect_match(js, "showElement.apply(this, arguments)", fixed = TRUE)` etc.; modal-browser.R:179-180 `# No control for this half: whether the two answers land inside one 150 ms fade is up to the machine, and where they do not, the steps are the ones above.`
- Failure scenario: build the wrapper but never install it (`var wrapped = function () { if (!document.body.contains(this._element)) return; return showElement.apply(this, arguments); };` without `inst._showElement = wrapped`), or move the guard after the `showElement.apply(...)` call — every substring still matches, and on any machine where requests 3 and 4 do not land inside one fade the browser test passes too: the "dead second dialog" bug is back with the suite green.
- Confidence: verified-by-reading
- Suggested direction: pin the assignment (`inst._showElement = function`) and the guard-before-apply order, and make the browser race deterministic (e.g. stub `_backdrop.show` to defer its callback until dialog 4 is requested).

### F-t-shiny-2-17  [nit] [test-weak]
- Where: tests/testthat/test-shinyapp-modal-browser.R:31-39 (vs .github/workflows/R-CMD-check.yaml:139-150, 152-162)
- What: the only real-browser check of the scroll-lock fix skips silently when no Chrome is found, and — unlike the PDF backend and the source-tree tests, which have a CI step that turns their skip into a failure — nothing guarantees it ran.
- Evidence: `skip_if(is.null(tryCatch(chromote::find_chrome(), error = function(e) NULL)), "no Chrome to run the dialogs in")`; the workflow has "Verify a DOCX -> PDF backend is present" and "Run source-tree tests (must not skip)" but no Chrome/AppDriver guard.
- Failure scenario: a runner image without Chrome (or a chromote change in how it locates it) turns both browser tests into skips; the bug that "was reported three times" (file header) loses its only runtime check with a green build.
- Confidence: verified-by-reading (workflow and test); inferred that today's GitHub images ship Chrome (not checked)
- Suggested direction: a cheap CI step asserting `chromote::find_chrome()` is non-NULL, as done for the PDF backend.

### F-t-shiny-2-18  [nit] [doc-wrong]
- Where: tests/testthat/test-shinyapp-harness.R:1-4, 19-20
- What: the harness comments are stale: app.R is 11,206 lines, not "3,700", and "nothing in the normal test suite ever parses it" is no longer true — dozens of `testServer(app_server_dir(), ...)` tests source and run it.
- Evidence: `# parse() is the only cheap way to catch a syntax error in a 3,700-line file` / `# that no other test loads.`; `# ... nothing in the normal test suite ever parses it.`
- Failure scenario: misleads about why the test exists and what else covers app.R.
- Confidence: verified-by-reading (`wc -l inst/shiny/dta_app/app.R` = 11206)
- Suggested direction: drop the line count and say the parse test gives a precise error before the server tests fail confusingly.

## tests/testthat/test-shinyapp-session-persistence.R (fully read)

### F-t-shiny-2-19  [minor] [test-weak]
- Where: tests/testthat/test-shinyapp-session-persistence.R:18-28 (vs inst/shiny/dta_app/R/utils_dta.R:5252-5257)
- What: the signed-in branch of `dta_session_key()` is checked only for shape and for differing from other inputs, never for being stable across calls, and no server test runs with a non-NULL `session$user` — so "a signed-in user can restore their own session" is not pinned anywhere.
- Evidence: `alice <- key(id, "alice"); expect_match(alice, "^[a-f0-9]{32}$"); expect_false(identical(alice, id)); expect_false(identical(alice, key(id, "mallory")))`; all `testServer()` sessions in the suite have `session$user` NULL, which returns `client_id` unchanged.
- Failure scenario: replace `rlang::hash(c(user, client_id))` with anything non-deterministic (e.g. a hash including `Sys.time()` or a fresh salt) — every assertion still passes, but on Posit Connect (authenticated) every reload computes a new file name and "Restore previous session" never finds the autosave.
- Confidence: verified-by-reading
- Suggested direction: add `expect_identical(key(id, "alice"), key(id, "alice"))` and one testServer round trip with `session$user` set.

### F-t-shiny-2-20  [nit] [inconsistency]
- Where: tests/testthat/test-shinyapp-session-persistence.R:6-10 vs tests/testthat/helper-shinyapp.R:302-309
- What: the file redefines the shared helper `clean_session_file()` with different semantics (also deletes the `_tables` directories, recursively), so the same name means two different clean-ups across the suite.
- Evidence: local `pattern = "^dtatools_app_session", ... unlink(f, recursive = TRUE, force = TRUE)` vs helper `pattern = "^dtatools_app_session.*\.rds$"` with a non-recursive `unlink(f, force = TRUE)`.
- Failure scenario: every other file's clean-up leaves `dtatools_app_session_*_tables` directories behind in tempdir(), while readers of this file assume the helper behaves like the local copy.
- Confidence: verified-by-reading
- Suggested direction: give the helper the wider pattern and drop the local copy.

## tests/testthat/test-shinyapp-contact-editor.R (fully read)

### F-t-shiny-2-21  [nit] [dead-code]
- Where: tests/testthat/test-shinyapp-contact-editor.R:182
- What: the test "bumps" the metadata re-render tokens by setting inputs nobody reads; the tokens are `rv$md_token` / `rv$contacts_token` (reactiveValues), never `input$...`.
- Evidence: `session$setInputs(md_token = 1, contacts_token = 1)`; app.R has `md_token = 0, # bump to re-render metadata editor` inside `reactiveValues(...)` (L1252) and only `rv$md_token` reads/writes — no `input$md_token` / `input$contacts_token` anywhere.
- Failure scenario: none at runtime; the line suggests a render dependency the test does not actually exercise (the output renders only because it is read).
- Confidence: verified-by-reading
- Suggested direction: delete the line.

### F-t-shiny-2-22  [nit] [inconsistency]
- Where: tests/testthat/test-shinyapp-contact-editor.R:27-34, 86-89, 122-128, 146-152 (vs tests/testthat/test-shinyapp-containers.R:6-9 and test-shinyapp-delivery.R:5-7)
- What: four tests call `testthat::local_mocked_bindings(showModal = ..., .package = "shiny")` INSIDE the `testServer()` block, while the sibling files state as a rule that a withr-deferred `local_*()` made inside the block "defers to an environment that never exits".
- Evidence: `shiny::testServer(app_server_dir(), { load_fixture(session); unlock_editing(session); shown <- 0L; testthat::local_mocked_bindings(showModal = function(ui, session = NULL) {...}, .package = "shiny")`.
- Failure scenario: if the sibling files' rule is right, these `shiny::showModal` mocks are never restored and stay in force for every later test file in the run (harmless only because MockShinySession's own `sendModal` is a no-op); if it is wrong, those headers are wrong. Either way the suite contradicts itself.
- Confidence: verified-by-reading (both texts); inferred (the actual withr/rlang behaviour for a data-mask env was not run)
- Suggested direction: mock at test level, before `testServer()`, as every other file in this slice does.

## tests/testthat/test-shinyapp-spec-recheck.R (fully read)

### F-t-shiny-2-23  [minor] [test-weak]
- Where: tests/testthat/test-shinyapp-spec-recheck.R:89-105 (vs inst/shiny/dta_app/app.R:6227-6252)
- What: "removing a column re-validates the loaded data" only asserts the status is not "pending", which the stale pre-edit verdict ("pass", set by `recheck_setup`) also satisfies.
- Evidence: `expect_false(identical(unname(rv$status[["clinical_data"]]), "pending"))`; the handler does `invalidate_dataset(ed)` (-> "pending") and later `recheck_dataset(ed)`.
- Failure scenario: drop both `invalidate_dataset(ed)` and `recheck_dataset(ed)` from the `col_del_click` observer — the dataset keeps the old "pass" (and its old validation index) for a spec that no longer has BMI, which is exactly the stale-verdict bug the file exists to prevent; the test still passes. (Saving a column is protected by the "fails ... without pressing check" test; deleting one is not.)
- Confidence: verified-by-reading
- Suggested direction: assert the re-validation happened (e.g. the dataset's validation run id / `validated_at` changed) or pin the concrete verdict for the BMI-less spec.

### F-t-shiny-2-24  [minor] [test-weak]
- Where: tests/testthat/test-shinyapp-spec-recheck.R:201-219 (vs R/DTAColumnSpec-class.R:318-327)
- What: the refusal test accepts any non-empty error, and its preamble is a stale plan note: the self-check it says is still missing exists.
- Evidence: `# Needs the spec self-check (dta_spec_value_mismatch, WP-A) in the DTAColumnSpec` / `# validator: until then nothing rejects ...`; assertions `expect_false(isTRUE(rv$col_msg$ok)); expect_true(nzchar(rv$col_msg$error))`; the package raises `"{self@id}: declared values/examples do not fit the column."` with class `dta_spec_value_mismatch`.
- Failure scenario: if the form were refused for an unrelated reason (e.g. the `col_length = "3"` parse, a renamed input), or the value check were removed and some other validator still threw, the test passes without the value/length contradiction ever being detected.
- Confidence: verified-by-reading
- Suggested direction: match the package-authored text ("do not fit the column") and drop the "until then" note.

### F-t-shiny-2-25  [nit] [design]
- Where: tests/testthat/test-shinyapp-spec-recheck.R:151-161
- What: the test pins that moving a column resets a valid verdict to "pending" while its own comment says the move "changes no verdict" — behaviour pinned as correct without the repo's `KNOWN DEFECT, pinned rather than endorsed` marker.
- Evidence: `# Moving a column changes no verdict; it keeps today's reset to "pending".` / `expect_equal(unname(rv$status[["clinical_data"]]), "pending")`; in the package column order only orders messages (`R/columnSpecChecks.R:731-751`).
- Failure scenario: a later fix that keeps the verdict across a pure reorder turns this test red as if it were a regression.
- Confidence: verified-by-reading
- Suggested direction: either keep the verdict on reorder and flip the test, or mark the reset as a pinned known defect with the assertion to switch to.

## tests/testthat/test-shinyapp-ui-components.R (fully read)

### F-t-shiny-2-26  [minor] [test-weak]
- Where: tests/testthat/test-shinyapp-ui-components.R:135-152 (vs inst/shiny/dta_app/app.R:554-610)
- What: the "waits for the document before touching body" guard is pinned by three substrings of the whole app.R, which cannot see the ORDER that is the actual fix.
- Evidence: `expect_match(src, "observe(document.body", fixed = TRUE)` / `"document.readyState === 'loading'"` / `"addEventListener('DOMContentLoaded', start)"` on `app_source("app.R")`.
- Failure scenario: add an eager `start();` as the first statement of the IIFE (or call `start()` before the `readyState` test) — `observe(document.body)` again runs while `<body>` is null, the IIFE aborts, the Ace editor is never wired; all three substrings are still present and the test passes.
- Confidence: verified-by-reading
- Suggested direction: read `yaml_ace_resize_js` via `app_r_binding()` and assert that the only `start()` call sites are the listener and the `else` branch (or run it in the browser suite).

### F-t-shiny-2-27  [nit] [test-weak]
- Where: tests/testthat/test-shinyapp-ui-components.R:116-133
- What: the "no second control with the same input id" regression guard only rejects one exact spelling.
- Evidence: `expect_no_match(src, "actionButton(\"edit_cols\"", fixed = TRUE)` (and the same for edit_rules/edit_files/edit_meta).
- Failure scenario: `actionButton(inputId = "edit_cols", ...)` or `actionLink("edit_cols", ...)` re-introduces the duplicate binding the comment warns about (Shiny binds whichever it saw last) and the test stays green.
- Confidence: verified-by-reading
- Suggested direction: assert on the rendered workspace instead (each of the four ids occurs exactly once in `ui_text(output$dataset_detail)`).

## tests/testthat/test-shinyapp-agreement-metadata.R (fully read)

(L168-174 is another `local_mocked_bindings()` inside a `testServer()` block — same
inconsistency as F-t-shiny-2-22.)

### F-t-shiny-2-28  [minor] [test-cheat]
- Where: tests/testthat/test-shinyapp-agreement-metadata.R:163-181 (vs inst/shiny/dta_app/app.R `observeEvent(input$agreement_save, ...)`)
- What: the "preserve ... entered values" half of the test cannot fail: under `testServer()` an input changes only through `session$setInputs()`, and every server-side way to lose the draft (`removeModal()`, `updateTextAreaInput()`, re-`showModal()`) is a MockShinySession no-op.
- Evidence: test name `"invalid modal saves notify and preserve both document and entered values"`; after `session$setInputs(agreement_id = "not valid", agreement_title = "Title", agreement_text = "Draft")` and a failing save it asserts `expect_identical(input$agreement_text, "Draft")` — the value the test itself just set. App on failure: `if (save_section_result(r)) { rv$editing_section <- NULL; removeModal() }`.
- Failure scenario: make the failure branch call `removeModal()` (dialog closes, the typed text is gone in a browser) or `updateTextAreaInput(session, "agreement_text", value = "")` — the assertion still passes. (Clearing `rv$editing_section` would be caught later by the `notices` count; closing the dialog alone would not.)
- Confidence: verified-by-reading (MockShinySession no-op list per the cross-slice note above)
- Suggested direction: assert what the server controls — mock `shiny::removeModal` and expect no call on a failed save, and keep `rv$editing_section` non-NULL — and drop the input read-back.

## tests/testthat/test-shinyapp-template-diff.R (fully read)

### F-t-shiny-2-29  [nit] [test-weak]
- Where: tests/testthat/test-shinyapp-template-diff.R:66-72, 85-92 (vs inst/shiny/dta_app/R/template_diff.R:136-146)
- What: two expectations are computed by calling the app's own helpers that the code under test delegates to, so they only prove delegation, not the claimed output ("flatten other fields unchanged", the diff's from/to text).
- Evidence: `expect_identical(leaves[names(leaves) != "sections"], app_fn(".dta_leaf_flatten")(ordinary))` — and `dta_metadata_leaves()` is literally `leaves <- .dta_leaf_flatten(lst)`; `expect_identical(difference$metadata$from, app_fn("dta_diff_display_value")(current@metadata@sections))`.
- Failure scenario: a `.dta_leaf_flatten()` that drops a nested leaf (e.g. `supplier.affiliation.name`) or a `dta_diff_display_value()` that renders every collection as "" changes both sides identically; these assertions stay green (the flattening itself is pinned by hand-written values only in test-shinyapp-template-rebase.R:128-150, another slice).
- Confidence: verified-by-reading
- Suggested direction: spell out the expected leaves (`title`, `study.id`, `supplier.affiliation.name`, `transmission.security`) and the expected display strings.

## tests/testthat/test-shinyapp-template-picker.R (fully read)

### F-t-shiny-2-30  [minor] [test-weak]
- Where: tests/testthat/test-shinyapp-template-picker.R:53-68 (vs inst/shiny/dta_app/R/template_index.R `template_picker_entries()`)
- What: the test named "... source then label order" uses a fixture whose label order, id order and source order all coincide, so neither the source-first ordering nor the `method = "radix"` (locale-independent) collation the app comment insists on is pinned.
- Evidence: fixture `bar`/"Bar" in `a_source`, `foo`/"Foo New" in `b_source`; app: `out <- out[order(out$label, method = "radix"), ]; out <- out[order(out$source_name, method = "radix"), ]` with the comment "this machine collates under German locale, CI under C collation, and a user-facing list must not silently depend on which one built it".
- Failure scenario: delete the `source_name` ordering line, or drop `method = "radix"` from both — `entries$id` is still `c("bar", "foo")` and the test passes; the picker then groups by label across libraries, or orders "a…"/"B…" labels differently on the German dev machine than on CI.
- Confidence: verified-by-reading
- Suggested direction: give the later-source entry the alphabetically earlier label, and add a lower/upper-case label pair ("alpha" vs "Beta") whose radix order differs from de_DE collation.

## tests/testthat/test-shinyapp-template-study-tokens.R (fully read)

No findings: expectations are hand-written (incl. the escaped regexes and the
`dta_template_dry_study()` stand-ins, pinned literally at L306-314 before L332-340
reuses them); the token functions named in the header exist where it says
(R/template_core.R:849, 893, 921, 1067).

## tests/testthat/test-shinyapp-dataset-meta.R (fully read)

### F-t-shiny-2-31  [nit] [test-weak]
- Where: tests/testthat/test-shinyapp-dataset-meta.R:265-278
- What: "changing the id preserves the dataset's columns, rules and bound tables" compares only names and counts, not the preserved content.
- Evidence: `expect_equal(names(after@specs@columns), names(before@specs@columns))`; `expect_equal(length(after@specs@rules), length(before@specs@rules))`; `expect_equal(names(after@tables), names(before@tables))`.
- Failure scenario: a rename that rebuilds the dataset and in doing so resets column types/lengths, empties a rule's condition, or binds an empty table under the old table name passes all three.
- Confidence: verified-by-reading
- Suggested direction: `expect_identical(after@specs, before@specs)` and `expect_identical(after@tables, before@tables)` (only `@id` should differ).

## tests/testthat/test-shinyapp-import-axis.R (fully read)

### F-t-shiny-2-32  [minor] [dead-code]
- Where: tests/testthat/test-shinyapp-import-axis.R:100-148, 173-195, 207-220 (vs inst/shiny/dta_app/R/utils_dta.R:1350-1395 `dta_table_status_map()` / `dta_table_status_from_status_df()`)
- What: a whole third table state ("unknown": its own icon "?", CSS class `file-unknown`, branch in the status map) plus an "absent column keeps two-axis behaviour" branch exist only for validation artifacts written before the import axis existed, and three tests pin that compatibility.
- Evidence: test comments `# A pre-v2 artifact: the two-axis run it came from recorded ok = TRUE, and the import axis was never checked (NA).` / `# ... it means the frame predates the column entirely.` / `# Exactly what a validation index written before the import axis existed looks like`; app: `import_unknown <- ("n_import_errors" %in% names(vs)) & is.na(nie)`, `st[import_unknown & st == "pass"] <- "unknown"`, plus the `tcol` fallback to `"target"` / `names(vs)[1]`.
- Failure scenario: n/a — dead weight under the owner's "no backwards compatibility before 1.0.0" rule (a current `check()` always records `n_import_errors`; the all-NA never-validated row is already "pending").
- Confidence: verified-by-reading (app function and the three tests); inferred that no current code path writes a validation-index entry without `n_import_errors` (package side not traced in full)
- Suggested direction: drop the "unknown" state, its CSS/icon and the absent-column branch with these tests, or record the pre-1.0 compat decision explicitly.

## tests/testthat/test-shinyapp-app-config-server.R (fully read)

### F-t-shiny-2-33  [minor] [test-weak]
- Where: tests/testthat/test-shinyapp-app-config-server.R:1-4, 340-384 (vs inst/shiny/dta_app/app.R:9829-9850, 8104-8120, 10307-10313)
- What: the file promises "Every gate is asserted against the SERVER (testServer) and not only the markup", but the download gates are asserted only in markup — and `dataset_yaml_download: false` not at all — while the server-side `req(...)` that enforce them are never executed by any test.
- Evidence: header `# ... Every gate is asserted against the SERVER (testServer) and not only the markup, because a control that is not rendered can still be driven over the websocket.`; tests read only `ui_text(output$msgs_dock_dl)` / `ui_text(output$main)` (`expect_no_match(main, "dl_yaml", fixed = TRUE)`); the real gates are `req(rv$dta, cfg$features$dta_yaml_download)` (dl_yaml), `req(rv$dta, rv$active, cfg$features$dataset_yaml_download)` (dl_ds_yaml), `req(msgs_active_checked(), "tsv" %in% cfg$exports$validation_messages)` (dl_msgs_tsv/xlsx), `req(rv$dta, cfg$exports$validation_summary)` (dl_validation_summary) — none of these outputs is ever read (see F-t-shiny-2-14).
- Failure scenario: drop the config term from any of those `req()` calls — a client that requests the download URL directly gets a format/file the deployment switched off; the suite stays green because the hidden buttons are all it checks. Same class of bypass the header says the file exists to rule out.
- Confidence: verified-by-reading
- Suggested direction: read each download output under the disabling config and expect it to fail with `shiny.silent.error` (as `req()` does), plus the markup check for `dl_ds_yaml` in `output$dataset_detail`.

## tests/testthat/test-shinyapp-lint.R (fully read)

### F-t-shiny-2-34  [minor] [test-weak]
- Where: tests/testthat/test-shinyapp-lint.R:311-330, 332-340, 248-255 (vs inst/shiny/dta_app/app.R:7409-7464)
- What: three refusals are asserted only as "some non-empty error", so any earlier refusal on the same path satisfies them; the first is even named "... refused with the reason in rv$rule_msg" but never looks at the reason.
- Evidence: `expect_false(isTRUE(rv$rule_msg$ok)); expect_true(nzchar(rv$rule_msg$error))` (range on GENDER); `expect_false(res$ok); expect_true(nzchar(res$error))` (duplicate rule id); `expect_false(is.null(rv$col_msg$error))` (Char->Num with text examples). The col_range branch has its own early refusals: `"Select the column to range-check."`, `"A range rule needs a minimum and/or maximum."`.
- Failure scenario: rename the `rule_min`/`rule_max` inputs (or break their parsing) — the save now stops at "A range rule needs a minimum and/or maximum." and the test still passes, while the package's "no range on a text column" refusal could be deleted unnoticed; likewise a duplicate id refused for an unrelated reason (e.g. WEIGHT renamed) passes.
- Confidence: verified-by-reading
- Suggested direction: match the package-authored reason text (or the condition class captured by `dta_try()`), e.g. the duplicate-id and range-type messages.

## tests/testthat/test-shinyapp-edit-mode.R (fully read)

(L558 and L684 set the same unread `md_token` / `contacts_token` inputs as
F-t-shiny-2-21.)

### F-t-shiny-2-35  [minor] [test-weak]
- Where: tests/testthat/test-shinyapp-edit-mode.R:186-197 and 628-642 (vs inst/shiny/dta_app/app.R:3272-3276, 6227-6231, 6254-6262, 7207-7213)
- What: two "with edit mode off, ... does nothing" tests never reach the `req(editing())` gate the file header says "these tests exercise": each handler first needs state only edit mode can create (`rv$removing_dataset`, stashed by the `remove_dataset` modal; `rv$editor_dataset`, set by opening an editor), which the tests never set — so the handlers stop on that `req()` instead.
- Evidence: test `session$setInputs(remove_dataset_confirm = 1)` with no prior `remove_dataset`; app `observeEvent(input$remove_dataset_confirm, { req(editing()); ed <- isolate(rv$removing_dataset); req(ed) ...`. Test `session$setInputs(col_del_click = 1, col_up_click = 1, col_down_click = 1)` / rule clicks with no `edit_cols`/`edit_rules`; app `req(editing()) ... ed <- isolate(rv$editor_dataset); req(ed)`. Header: `# ... every observer behind it still calls req(editing()), which is the enforcement these tests exercise.`
- Failure scenario: delete `req(editing())` from `remove_dataset_confirm`, `col_del_click`, `col_up_click`, `col_down_click`, `rule_del_click`, `rule_up_click`, `rule_down_click` — both tests still pass. (The well-built sibling at L585-607 opens the editor while editing, then leaves edit mode — but only for `col_save`.)
- Confidence: verified-by-reading
- Suggested direction: arm the state while editing (`remove_dataset = 1`, `edit_cols = 1`), then `leave_edit_mode()` and fire the clicks — or drop the redundant gate if the stash is meant to be the guard.

### F-t-shiny-2-36  [nit] [test-weak]
- Where: tests/testthat/test-shinyapp-edit-mode.R:644-672
- What: "a rejected Add dataset keeps what the user typed" pins only that `rv$add_ds_token` did not move; the other ways the server could wipe the form are MockShinySession no-ops, so they pass too (the comment rightly says the HTML cannot show it).
- Evidence: `expect_equal(rv$add_ds_token, token_before)` is the only form-state assertion.
- Failure scenario: on rejection, `removeModal()` (dialog closes, typing lost) or `updateTextInput(session, "add_ds_id", value = "")` — both invisible to testServer; the test stays green.
- Confidence: verified-by-reading
- Suggested direction: also mock `shiny::removeModal` / `shiny::updateTextInput` and assert they are not called on a rejected save.

## tests/testthat/test-shinyapp-template-history.R (fully read)

No findings: every expected change line, title and error fragment is written out by
hand from the fixture, loops (`for (change in ...)`, `for (version in ...)`) have
fixed non-empty ranges, and the placeholder/variant/prior_versions cases each
assert concrete rows.

## tests/testthat/test-shinyapp-app-config.R (fully read)

No findings: the parser tests assert the problem keys AND the value kept for each
bad input (incl. traversal/UNC/drive/absolute logo paths and CSS-injection
colours), every loop has a fixed non-empty input list, and the skeleton test
derives its section list from the defaults on purpose (that is the invariant).

## tests/testthat/test-shinyapp-template-index.R (fully read)

### F-t-shiny-2-37  [nit] [test-weak]
- Where: tests/testthat/test-shinyapp-template-index.R:417-434 (vs inst/shiny/dta_app/R/template_index.R `list_template_index_entries()`)
- What: the label-order test uses "Alpha"/"Beta", whose order is the same under radix and under de_DE collation, so the `method = "radix"` the implementation relies on (and that L627-638 pins for `dta_template_misplaced_files()`) is not pinned here.
- Evidence: fixture labels `"Beta"`, `"Alpha"`, `"Alpha"`; app `by_label <- order(rows$label, method = "radix")`.
- Failure scenario: drop `method = "radix"` — the picker's label order then differs between the German dev machine and CI for mixed-case labels ("alpha" vs "Beta"), and this test still passes.
- Confidence: verified-by-reading
- Suggested direction: add a lower-case label that radix sorts after an upper-case one (as L627-638 does).

## tests/testthat/test-shinyapp-versioning.R (fully read)

### F-t-shiny-2-38  [minor] [test-weak]
- Where: tests/testthat/test-shinyapp-versioning.R:594-608 (vs inst/shiny/dta_app/app.R:1306, 1937)
- What: "a document loaded through apply_loaded() at its default `versioned` is not locked" runs on a fresh server, where `rv$version_locked` already starts FALSE, and checks it with `isTRUE()` — so it never shows that `apply_loaded()` actually clears a lock.
- Evidence: test `apply_loaded(dta, "metadata:\n  title: Test\n"); expect_false(isTRUE(rv$version_locked))` with nothing loaded before; app `version_locked = FALSE, # TRUE while a LOADED document ...` (reactiveValues init) and `rv$version_locked <- isTRUE(versioned)` in `apply_loaded()`.
- Failure scenario: make the lock sticky (`rv$version_locked <- isTRUE(versioned) || isolate(rv$version_locked)`) or drop the assignment for `versioned = FALSE` — this test still passes (initial FALSE), and L573-592 still passes (it only checks a versioned load re-arms the lock); a user who opened a file and then creates a document from a template gets the new document locked behind "Create new version".
- Confidence: verified-by-reading
- Suggested direction: `load_fixture(session)` first (lock TRUE), then `apply_loaded(dta, ...)` and `expect_false(rv$version_locked)` (no `isTRUE()`).

## tests/testthat/test-shinyapp-template-ui.R (fully read)

### F-t-shiny-2-39  [minor] [test-weak]
- Where: tests/testthat/test-shinyapp-template-ui.R:700-734, 852-866 (vs inst/shiny/dta_app/app.R `observeEvent(input$template_create_confirm, ...)`)
- What: two tests are named for things they cannot observe — "leaves the modal open" and "is a clear error, and the modal stays open" — and assert only that no document was created (plus, in the first, that `rv$template_ref` survived).
- Evidence: assertions `expect_null(rv$dta)` / `expect_equal(rv$template_ref, "broken_tpl@1.0")`; the app's failure branches are `showNotification(..., type = "error"); return() # modal stays open` / `# LEAVE THE MODAL OPEN so the user does not lose their choices`, with `removeModal()` only after them. Neither `showNotification` nor `removeModal` is mocked, and `removeModal` is a MockShinySession no-op.
- Failure scenario: move `removeModal()` above the failure checks (or call it in a failure branch), or drop the error `showNotification(...)` for the missing carry-over file — the dialog closes and the user's choices are lost, or the click silently does nothing; both tests stay green.
- Confidence: verified-by-reading
- Suggested direction: mock `shiny::removeModal` and `shiny::showNotification` and assert no remove and one error notice on each failure path.

### F-t-shiny-2-40  [nit] [test-weak]
- Where: tests/testthat/test-shinyapp-template-ui.R:1017-1040
- What: "edit mode from a template-created document survives a reload" restores in the same session where `editing()` is already TRUE and never checks that a restore happened, so a missing autosave makes `restore_session` return early and the test passes without exercising the guarded bug.
- Evidence: `expect_true(editing())` before and after `session$setInputs(restore_session = 1)`; `restore_session` starts `if (is.null(sf) || !file.exists(sf)) return()` (app.R:11038-11041).
- Failure scenario: drop the `autosave()` on the template-create path — no session file, restore is a no-op, editing stays TRUE from before, test green (the "autosave before rv$editing" bug it pins cannot then be seen at all).
- Confidence: verified-by-reading
- Suggested direction: assert the restore ran (`rv$doc_token` bumped, or the session file exists and `saved$editing` is TRUE) before checking `editing()`.

## tests/testthat/test-shinyapp-template-create.R (fully read)

### F-t-shiny-2-41  [minor] [test-cheat]
- Where: tests/testthat/test-shinyapp-template-create.R:608-623 and 861-901 (vs inst/shiny/dta_app/R/template_create.R `apply_metadata_carry_over()`)
- What: the `import_issues` half of "carry-over never copies template or import_issues" — headed "The most important test" — cannot fail, because neither test's SOURCE metadata has any import issues to copy.
- Evidence: L611 `source_meta <- DTAtools::DTAMetaData(title = "Ancestor", version = "9.9", header = ..., template = ...)` (no import_issues; only `@template` is then asserted); L878-900 `meta_a` comes from `create_fn(def_common(), ...)` whose metadata is just `title: T, version: "1.0"`, then `carry_over = list(metadata = meta_a, fields = c(carry_over_fields, "template", "import_issues"))` and `expect_length(meta_b@import_issues, 0)`.
- Failure scenario: remove `"import_issues"` from `dta_metadata_machine_fields()` (template_core.R:366-368, `c("import_issues", "template")`), which `apply_metadata_carry_over()` strips via `setdiff(fields, dta_metadata_machine_fields())` (template_create.R:379) — an ancestor's metadata import errors (e.g. a qualified `date_last_transfer`, see test-shinyapp-import-axis.R:359-389) would then be copied into the new document; both tests stay green.
- Confidence: verified-by-reading (the strip list and its use); inferred that `meta_a` carries no import issues (its source metadata is only `title`/`version`, not built and inspected)
- Suggested direction: give the source metadata a real import issue (as import-axis.R:362-365 does) and assert it is absent from the result.

### F-t-shiny-2-42  [minor] [test-weak]
- Where: tests/testthat/test-shinyapp-template-create.R:551-590
- What: "all three datasets: forms build the identical DTA whether or not an index is supplied" compares only dataset names and the title, not the documents.
- Evidence: `expect_equal(sort(names(res_with_index$value@datasets)), sort(names(res_no_index$value@datasets)))`; `expect_equal(DTAtools::metadata(res_with_index$value)@title, DTAtools::metadata(res_no_index$value)@title)`.
- Failure scenario: the index path resolving `plain_source_ds.yaml` to a different file, or building a dataset with different files/specs, keeps both checks green.
- Confidence: verified-by-reading
- Suggested direction: `expect_identical(res_with_index$value@datasets, res_no_index$value@datasets)` (and the metadata, if provenance is not part of it).

### F-t-shiny-2-43  [nit] [doc-wrong]
- Where: tests/testthat/test-shinyapp-template-create.R:159-160, 927-945
- What: stale wording: a "pre-existing 55 tests" count with no referent, and a "Backward compatibility ... behaves exactly as before" test that only checks `ok` and an empty `@template`.
- Evidence: `# A trivial inline dataset entry, the exact shape the pre-existing 55 tests`; `test_that("create_dta_from_template() with none of the four new arguments behaves exactly as before", { ... expect_true(result$ok); expect_length(DTAtools::metadata(result$value)@template, 0) })`.
- Failure scenario: a default argument that started changing the built document (e.g. a non-empty default carry-over) passes a test whose name promises "exactly as before".
- Confidence: verified-by-reading
- Suggested direction: rename to what it checks ("without provenance no @template is written") and drop the count.

## tests/testthat/test-shinyapp-utils.R (fully read)

### F-t-shiny-2-44  [minor] [code-cheat]
- Where: tests/testthat/test-shinyapp-utils.R:177-189 (vs inst/shiny/dta_app/R/utils_dta.R `dta_handler_type()` and the comment above it)
- What: the test pins as correct that ANY object that is not a known file handler is reported as type "csv" — a silent fallback the app itself documents as a "KNOWN GAP" (a `DTAFileDelim` loses its `sep`) — without the repo's `KNOWN DEFECT, pinned rather than endorsed` marker; the app comment it rests on is also stale.
- Evidence: test `fake <- structure(list(), class = "not_a_dta_handler") ... # an object that is neither DTAFileTSV nor DTAFileCSV falls through to "csv".` / `expect_equal(app_fn("dta_handler_type")(fake), "csv")`; app: `if (inherits(h, "DTAtools::DTAFileCSV")) return("csv"); "csv"` and `# KNOWN GAP ... DTAFileFactory() ... implements csv, tsv, sas7bdat and any only` — but `DTAFileFactory()` has an `xpt = DTAFileXPT(` branch (R/00_helpers.R:917 ff.), and `dta_handler_type()` itself tests `DTAFileXPT` three lines below the comment.
- Failure scenario: a handler class the switch does not know (a Delim today, any new one tomorrow) is serialised as `type: csv` and read back as a different, comma-delimited handler — the document silently changes meaning on the next save; a fix that refuses unknown handlers would turn this test red as if it were a regression.
- Confidence: verified-by-reading
- Suggested direction: abort (or at least warn) on an unknown handler class, and either flip this expectation or mark it as a pinned known defect; fix the "csv, tsv, sas7bdat and any only" list.

### F-t-shiny-2-45  [minor] [test-weak]
- Where: tests/testthat/test-shinyapp-utils.R:1061-1081 (vs inst/shiny/dta_app/R/utils_dta.R:5070-5090 and app.R:9672-9690)
- What: the two signature tests only check that a signature equals itself and differs between two unrelated datasets, yet the signatures decide whether bound data keeps its validation after a Raw-YAML apply (`keep_validation = specs_same && handlers_same`).
- Evidence: `expect_identical(sig(ds_clinical), sig(ds_clinical))`; `expect_false(identical(sig(ds_clinical), sig(ds_gf)))` (different ids, columns, rules and files).
- Failure scenario: a `dta_specs_signature()` that drops the rules (or the matrix block) still passes both tests; a Raw-YAML edit that changes only a rule then keeps the old validation, so the dataset keeps its pre-edit "pass" for a rule it never ran (test-shinyapp-spec-recheck.R covers a column-length edit through Raw YAML, not a rule edit).
- Confidence: verified-by-reading
- Suggested direction: assert the signature changes for a single changed rule, a changed column attribute and a changed matrix cell type, and stays equal when only bound data / validation state differ.

### F-t-shiny-2-46  [nit] [test-weak]
- Where: tests/testthat/test-shinyapp-utils.R:258-264
- What: a test named "... with NULL after unloading detects the vacuous case" neither passes NULL nor unloads anything; it repeats the never-loaded assertion of the test above it (L247-248).
- Evidence: body: `dta <- app_fixture_dta(); ds <- app_fn("dta_get_dataset")(dta, "clinical_data"); expect_equal(app_fn("dta_dataset_content_count")(ds), 0L)`.
- Failure scenario: a `dta_dataset_content_count()` that counts stale tables left behind by an unload is not caught, although the name says it is.
- Confidence: verified-by-reading
- Suggested direction: load, unload via `dta_unload_all()`, then assert 0 — or delete the duplicate.

## tests/testthat/test-shinyapp-backends.R (fully read)

### F-t-shiny-2-47  [minor] [dead-code]
- Where: tests/testthat/test-shinyapp-backends.R:535-543, 651-656, 877-904 (vs the bare-type branches of `dta_column_fields()` / `dta_set_matrix()` / `dta_matrix_fields()` in inst/shiny/dta_app/R/utils_dta.R)
- What: three tests pin a "bare type (no backend prefix) is SAS" compatibility path that, by the tests' own words, "only an older object could hold"; one of them does not even exercise it, and another reaches it only by stubbing an internal helper to fabricate the old shape.
- Evidence: L541-542 `# A bare type, which only an older object could hold, is still a SAS one.` followed by `be_matrix_dta("{ type: SAS Int }", "SAS Char")` — a PREFIXED type, so the bare-type branch is never hit; L651 `# A bare type is a SAS one, as it always was` with `save(sas, "mx", list(cell_type = "Int"))` (a value the form never offers — `dta_matrix_cell_types()` returns prefixed values only); L892-903 `scope$dta_column_to_list <- function(col) list(id = "UNTYPED", type = "Char"); legacy <- app_fn("dta_column_fields"); environment(legacy) <- scope`.
- Failure scenario: n/a for the compat itself (dead weight under the owner's no-compat-before-1.0 rule); the L541 mismatch means a regression in the bare-type branch it claims to cover would not be seen.
- Confidence: verified-by-reading (tests); inferred that no current reader can produce a bare type (package readers require the prefix per the CHANGELOG "The prefix is always required")
- Suggested direction: drop the bare-type branches and these three pins, or, if kept, make L541-542 actually use `{ type: Int }`.

## tests/testthat/test-shinyapp-template-inherit.R (fully read)

### F-t-shiny-2-48  [minor] [test-weak]
- Where: tests/testthat/test-shinyapp-template-inherit.R:1310, 1462-1473, 1647, 1893-1896 (vs inst/shiny/dta_app/R/template_inherit.R `dta_template_check_required()` / `dta_template_assert_unique_keys()`)
- What: the "required path unfilled" and "duplicate key inside a mixin" refusals are asserted only as `class = "rlang_error"` — the class every `cli::cli_abort()` carries — because those aborts have no condition class of their own, so any other abort on the same path satisfies them; the file's mixin section even promises "Conditions are asserted by CLASS".
- Evidence: `expect_error(check_required(NULL, needs), class = "rlang_error")` (in the `for (case in lacking_field_cases())` loop, no message check), `expect_error(check_required(NULL, x_blank), class = "rlang_error")`, `expect_error(mx_resolve(mx_child(list("m")), defs), class = "rlang_error")`; the only specific classes in template_inherit.R are `dta_template_sealed_violation` and the four `dta_template_mixin_*`.
- Failure scenario: if `dta_template_check_required()` started aborting on every dotted path for an unrelated reason (e.g. a malformed-path abort), or the mixin path failed on something other than the duplicate, these tests stay green while the requirement/duplicate check itself is gone (only L1205-1220 and L1069-1080 also match the message).
- Confidence: verified-by-reading (grep of `class = "dta_template` in template_inherit.R)
- Suggested direction: give the required-path and duplicate-key aborts their own classes (e.g. `dta_template_required_unfilled`, `dta_template_duplicate_key`) and assert those.

### F-t-shiny-2-49  [nit] [test-weak]
- Where: tests/testthat/test-shinyapp-template-inherit.R:495-540, 208-224
- What: vestigial test scaffolding: the four-state table carries an `inherited` id per section that no assertion uses (the tests check only lengths), and a `for (field in "id")` loop runs exactly once.
- Evidence: `list(key = "options", added = ..., inherited = "o1")` … but the bodies only do `expect_length(result$def[[sec$key]], 1)` / `expect_length(result$def[[sec$key]], 2)`; `for (field in "id") { ... }`.
- Failure scenario: a merge that keeps the right number of entries but not the parent's (e.g. two copies of the child's entry) passes "a <key>: value merges with the inherited entries"; the unused field shows the identity assertion was intended.
- Confidence: verified-by-reading
- Suggested direction: assert the ids (`c(sec$inherited, sec$added$id)` / `sec$added$as`) and drop the one-element loop.

## Coverage

Fully read (every line), 29/29 slice files:
test-shinyapp-containers.R, -template-inherit.R, -delivery.R, -backends.R, -utils.R,
-template-create.R, -template-ui.R, -versioning.R, -template-index.R, -app-config.R,
-template-history.R, -edit-mode.R, -app-config-server.R, -dataset-add-remove.R,
-lint.R, -import-axis.R, -dataset-meta.R, -template-study-tokens.R,
-template-picker.R, -template-diff.R, -agreement-metadata.R, -ui-components.R,
-spec-recheck.R, -contact-editor.R, -modal-browser.R, -session-persistence.R,
-render-coverage.R, -modal-reattach.R, -harness.R.
Shared helpers fully read: helper-shinyapp.R, helper-containers.R,
helper-source-tree.R, helper-unchecked-collection.R.

Skimmed / read in part to verify claims (app code, not this slice): app.R regions
around the Delivery gate (10560-10980), restore_session (11020-11203), session
ownership (1685-1795), delivery observer (3960-3999), container/agreement/rule/
column/remove-dataset handlers, the modal and Ace JS snippets (111-610); R/containers.R
(whole); R/utils_dta.R regions (staging, signatures, export stamp, status map,
validation report, add/remove dataset, session key/table store/sweep);
R/template_index.R, R/template_ui.R, R/template_inherit.R (targeted); package
R/DTADataSetFile-class.R (check/messages), R/DTAColumnSpec-class.R (value check);
.github/workflows/R-CMD-check.yaml; CHANGELOG.md [Unreleased]. The installed
shiny 1.14.0 lazy-load DB was inspected (strings only) for MockShinySession's
no-op methods (F-t-shiny-2-10 and the cross-slice note).

Not read: helper-docx.R, helper-validation-corpus.R (not used by this slice).

Totals: 49 findings — 1 blocker, 1 major, 26 minor, 21 nit; test-weak 30,
doc-wrong 6, dead-code 4, inconsistency 4, test-cheat 3, code-cheat 1, design 1.

Method notes: the shiny 1.14.0 check (F-t-shiny-2-10) decompressed the installed
`shiny/R/shiny.rdb` with a small read-only Python script (scratchpad/shinyprobe/);
nothing under $SNAP was modified and no R was run. An accidental `cat > /tmp/x.sh`
left one empty file at /tmp/x.sh (not deleted, per the no-rm-without-asking rule).
