# Findings — slice t-shiny-1 (test-integrity review, Shiny-app tests)

Slice (28 files, `tests/testthat/`): test-shinyapp-server.R, -matrix.R, -export.R,
-vocabulary.R, -dataset-template.R, -template.R, -file-handlers.R,
-template-ui-builders.R, -dta-ops.R, -template-rebase.R, -template-sources.R,
-template-parties.R, -containers-helpers.R, -word-template-discovery.R,
-file-dataset-server.R, -upload-guard.R, -file-dataset.R, -theme.R,
-template-variants.R, -create-empty.R, -click-guard.R, -template-latest.R,
-shared-worker-limits.R, -file-count.R, -upload-boundary.R, -modal-outputs.R,
-authorized-corrections.R, -modal-scroll-lock.R. Plus shared helpers
(`helper-*.R`).

Pinned tree: `$SNAP` = scratchpad/snapshot (68408c44). All paths below are
relative to `$SNAP`. Read-only; no R run. Each finding is marked
`verified-by-reading` or `inferred`.

Files fully read so far: helper-shinyapp.R, helper-source-tree.R,
helper-unchecked-collection.R, helper-containers.R, test-shinyapp-server.R,
test-shinyapp-matrix.R, test-shinyapp-export.R, test-shinyapp-vocabulary.R,
test-shinyapp-dataset-template.R, test-shinyapp-template.R,
test-shinyapp-file-handlers.R, test-shinyapp-template-ui-builders.R,
test-shinyapp-dta-ops.R, test-shinyapp-template-rebase.R,
test-shinyapp-template-sources.R, test-shinyapp-template-parties.R,
test-shinyapp-containers-helpers.R, test-shinyapp-word-template-discovery.R,
helper-docx.R, test-shinyapp-file-dataset-server.R, test-shinyapp-upload-guard.R,
test-shinyapp-file-dataset.R, test-shinyapp-theme.R, and the rest of the slice —
see "Coverage" at the end (all 28 slice files fully read).

---

## helper-shinyapp.R (shared harness) and test-shinyapp-server.R

### F-t-shiny-1-01  [major] [test-cheat]
- Where: tests/testthat/test-shinyapp-server.R:1663-1714 (handler at inst/shiny/dta_app/app.R:8141-8159)
- What: The test named "HTML validation report download handler produces parseable HTML" never invokes the download handler; it calls `DTAtools::write_validation_report()` itself with different arguments, then asserts a near-tautology and greps app.R's source text.
- Evidence:
  `# This is the same call the downloadHandler uses.`
  `expect_no_error(DTAtools::write_validation_report(rv$dta, report_file, overwrite = TRUE, quiet = TRUE))`
  `doc <- xml2::read_html(report_file); expect_true(!is.na(doc))`
  `expect_true(grepl("output$dl_msgs_html <- downloadHandler", app_source("app.R"), fixed = TRUE), ...)`
  It is NOT the same call: the handler (app.R:8143-8148) runs `req(msgs_any_checked(), "html" %in% cfg$exports$validation_messages)` and passes `dta_app_export_args(cfg)[c("document_name","organisation","organisation_unit")]` via `do.call`. `read_html()` is libxml2's recovering HTML parser, so "parses" says nothing about the report, and `!is.na(doc)` is not a content check at all. In the whole testthat suite the success path of `output$dl_msgs_html` is never read (the only reads are this file's 2257-2260 refusal check; the success path is exercised only by inst/qualification/tests/test-OQ-APP-workflow.R:41).
- Failure scenario: change the handler's content to drop the branding args, to `do.call` with a wrong object, or to swallow errors into an empty file — this test (and the rest of tests/testthat) still passes. Reading `output$dl_msgs_html` (testServer returns the produced file path, as the OQ test does) and asserting on its content would catch all three.
- Confidence: verified-by-reading (the handler is never invoked; the exact semantics of xml2's `is.na()` on a document were not checked and the finding does not rest on them)
- Suggested direction: read `output$dl_msgs_html` after the check and assert on the report's content (dataset id, message count, document name from cfg); drop the source-text grep.

### F-t-shiny-1-02  [minor] [test-weak]
- Where: tests/testthat/test-shinyapp-server.R:2262-2267 (handler app.R:8097-8103, msgs_dl_df ~8075-8088)
- What: "Once checked, the same download produces a real file" asserts only that the CSV exists and has at least one line — no content check.
- Evidence: `csv <- output$dl_msgs_csv; expect_true(file.exists(csv)); expect_gt(length(readLines(csv, warn = FALSE)), 0)`. The fixture (clinical_data.csv) passes, so the handler writes the placeholder row "No validation messages for this dataset." — any file with a header line satisfies the test.
- Failure scenario: `msgs_dl_df()` regresses to always returning the placeholder (or drops columns, or `csv_formula_safe()` is removed) — still green. [assistant-tool].md's own rule: "a file-producing function is checked by reading the file back, not by file.exists()".
- Confidence: verified-by-reading
- Suggested direction: check a failing fixture's CSV row count/columns against `messages(rv$dta)`.

### F-t-shiny-1-03  [minor] [dead-code]
- Where: tests/testthat/test-shinyapp-server.R:211-213; app code inst/shiny/dta_app/app.R:1250, 4782-4786, 10499-10517, 11151; inst/shiny/dta_app/R/utils_dta.R:278-279, 292, 301, 314, 343
- What: The test pins `rv$dataset_only` to FALSE ("a legacy flag that must never be set again") instead of the flag being removed; the app keeps it "for back-compat with callers/old sessions", and the only way it can become TRUE is restoring an old session file (app.R:11151 `rv$dataset_only <- isTRUE(saved$dataset_only)`), which then reaches the dead branches at 4782 and 10502-10516.
- Evidence: utils_dta.R:278-279 `dataset_only : always FALSE now (...); retained for back-compat with callers/old sessions.`; app.R:1250 `dataset_only = FALSE, # legacy flag ... (never set TRUE)`.
- Failure scenario: no wrong result today; it is compat-only dead weight that owner priority 4 asks to be reported, and a test that enshrines it.
- Confidence: verified-by-reading
- Suggested direction: delete the flag, its branches and the session-restore read; drop the assertion.

### F-t-shiny-1-04  [nit] [doc-wrong]
- Where: tests/testthat/helper-shinyapp.R:3-5, 13-15, 64
- What: The harness header says the app has four helper files (utils_dta.R, utils_export.R, template_core.R, theme.R) and "~2,900 lines of helper code"; `inst/shiny/dta_app/R/` holds 18 files, ~20,200 lines (all sourced by `.shiny_app_helper_files()`).
- Evidence: `# Its helper files (R/utils_dta.R, R/utils_export.R, R/template_core.R, R/theme.R)`; `keeping app_env()'s wider file coverage (all four helper files ...)`; `# Cache, so the ~2,900 lines of helper code are parsed once`.
- Failure scenario: a reader concludes app_env() covers only four files.
- Confidence: verified-by-reading (`wc -l inst/shiny/dta_app/R/*.R`)
- Suggested direction: say "every file in inst/shiny/dta_app/R" without counts.

### F-t-shiny-1-05  [nit] [dead-code]
- Where: tests/testthat/helper-shinyapp.R:333-336
- What: `with_app_library(yaml_lines, code, ...)` documents that `code` is never evaluated and is "kept so existing call sites read the same", but no call site passes it (all 30+ calls in test-shinyapp-app-config-server.R / test-shinyapp-backends.R pass only the YAML lines).
- Evidence: `# Nothing here evaluates \`code\`; the argument is kept so existing call sites read the same.` A future caller writing `with_app_library(lines, { expect_...() })` would get a block that silently never runs.
- Failure scenario: a test written as `with_app_library(x, { ... })` asserts nothing and passes.
- Confidence: verified-by-reading (grep of every call site)
- Suggested direction: drop the `code` parameter so such a call errors.

### F-t-shiny-1-06  [nit] [test-weak]
- Where: tests/testthat/test-shinyapp-server.R:469
- What: Test title says the column save "clears validation", but the body (503-516) pins the opposite behaviour — the check is re-run and the status becomes "fail".
- Evidence: `test_that("saving a new column updates the spec, the YAML view and clears validation", {` ... `expect_equal(rv$status, c(clinical_data = "fail"))`.
- Failure scenario: misleading name only.
- Confidence: verified-by-reading
- Suggested direction: rename to "...and re-runs the check".

## test-shinyapp-matrix.R

### F-t-shiny-1-07  [minor] [test-weak]
- Where: tests/testthat/test-shinyapp-matrix.R:478-479, 572-579, 890-907 (906), 1629
- What: The "a refused edit changes nothing" assertions compare the caller's own input object with itself; an R function cannot modify its caller's S7 value, so these can never fail.
- Evidence:
  `# ... and a refused add leaves the DTA as it was.` / `expect_false("mx" %in% names(base@datasets))` (base was passed to add())
  `before <- as.list(dta@datasets$gene_counts@matrix)` ... `r <- rm_col(dta, ...)` ... `expect_identical(as.list(dta@datasets$gene_counts@matrix), before)`
  `expect_identical(as.list(dta@datasets$gene_counts@matrix), before, info = nm)` (inside the "refused and change nothing" loop)
  `expect_identical(mxc_mx(word)$cells$type, "SAS Char")` (word is the input)
- Failure scenario: the names promise "changes nothing", but the only state that could change is the returned value; a helper that returned `ok = FALSE` together with a modified `value` (which a caller using `r$value %||% dta` would then adopt) is caught only by the separate `expect_null(r$value)` lines — and 1629 / 478 have no such companion. The no-change claim itself is untestable as written.
- Confidence: verified-by-reading (S7 objects here are plain values — no environment-backed props are involved for unbound specs)
- Suggested direction: drop the self-comparisons; assert `is.null(r$value)` (as 904 does) where the refusal contract matters.

### F-t-shiny-1-08  [nit] [test-weak]
- Where: tests/testthat/test-shinyapp-matrix.R:240-252 (function at inst/shiny/dta_app/R/utils_dta.R:5077-5097)
- What: The expected tabular signature is rebuilt by re-running the function's own body (`yaml::as.yaml(list(columns = lapply(..., dta_column_to_list), rules = Filter(Negate(is.null), lapply(..., dta_rule_to_list))))`), an implementation mirror.
- Evidence: test 246-249 is line-for-line utils_dta.R:5078-5094.
- Failure scenario: a bug in `dta_column_to_list()`/`dta_rule_to_list()` (e.g. a property dropped from the signature, so a real spec edit no longer invalidates validation) changes both sides identically and passes. Only the `expect_false(grepl("matrix", sig))` line tests the stated property.
- Confidence: verified-by-reading
- Suggested direction: assert that two tabular datasets differing in one column property yield different signatures instead.

### F-t-shiny-1-09  [nit] [test-weak]
- Where: tests/testthat/test-shinyapp-matrix.R:1036
- What: "breaks only at bullets" accepts any continuation line that starts with the letters `i` or `x`.
- Evidence: `for (ln in lines[-1]) expect_match(ln, "^(ℹ|i|!|x|✖|•|\*)")` — no following space is required, unlike 1545's `[*ix!>v] `.
- Failure scenario: a mid-sentence wrap whose next word starts with "i"/"x" ("...the header\nimplies...") passes.
- Confidence: verified-by-reading
- Suggested direction: require the bullet to be followed by a space, as line 1545 does.

## test-shinyapp-export.R

### F-t-shiny-1-10  [minor] [test-weak]
- Where: tests/testthat/test-shinyapp-export.R:369-488 (call sites inst/shiny/dta_app/app.R:5384, 5857-5890, 7085)
- What: The XSS-escaping guarantee for the three editor tables is asserted on a test-built replica of the `DT::datatable()` call, not on the app's output; the real output is only checked for `"serverSide":true` and `>Actions<`, and a source regex checks that the literal `escape = -ncol(ov)` is present.
- Evidence: `escaped_cells <- function(ov) { ov$Actions <- "<button>x</button>"; wid <- DT::datatable(ov, rownames = FALSE, selection = "none", escape = -ncol(ov)); ... }` and `expect_match(block, "escape = -ncol(ov)", fixed = TRUE)`. The replica hard-codes the assumption that Actions is the LAST column, which is what makes `-ncol(ov)` safe (app.R col_tbl: `ov$Actions <- ...` then `escape = -ncol(ov)`). (The test's own comment documents the testServer wall; this finding is about what the replica cannot see.)
- Failure scenario: a call site that builds the Actions column first (`ov <- cbind(Actions = row_action_buttons(...), ov)`) or appends any column after it keeps the literal `escape = -ncol(ov)` but leaves the last data column (the user-controlled `description`) unescaped — stored XSS — while the replica (Actions last) and the source regex both still pass.
- Confidence: verified-by-reading
- Suggested direction: also pin, per call site, that the column excluded from escaping is named "Actions" (e.g. regex `ov$Actions <-` immediately before the datatable call), or move the table construction into a helper in R/ that the test can call.

### F-t-shiny-1-11  [minor] [test-weak]
- Where: tests/testthat/test-shinyapp-export.R:236-244 (code inst/shiny/dta_app/R/utils_export.R:66-111); also 151-168, 246-267
- What: Security and default-behaviour claims are tested by grepping source text, so the property can be lost while the text stays.
- Evidence: 240 `expect_match(src, "Content-Security-Policy\\\" content=\\\"default-src 'none';", fixed = TRUE)` — the CSP only reaches the page through `options = c("--standalone", "--include-in-header", header_file, ...)` (utils_export.R:107-110). 162-167 checks only that "export_format"/"export_cancel"/"export_do" appear anywhere in app.R (a comment suffices); 258 checks the literal `selected = export_choices$default_format`.
- Failure scenario: removing `"--include-in-header", header_file` from the pandoc options (or `--standalone`, which `--include-in-header` needs) ships a Chrome-printed page with no CSP while 236-244 stays green; renaming the modal's ids while a comment keeps the old names keeps 151-168 green.
- Confidence: verified-by-reading
- Suggested direction: run the HTML half of `markdown_to_pdf_via_chrome()` (mock `system2`/`find_chrome_binary`) and assert the CSP meta in the produced HTML.

### F-t-shiny-1-12  [nit] [test-weak]
- Where: tests/testthat/test-shinyapp-export.R:170-186
- What: "has_pdf_engine falls back cleanly when no engine is on the PATH" asserts only `type logical, length 1, not NA` — any TRUE/FALSE passes.
- Evidence: `expect_type(out, "logical"); expect_length(out, 1); expect_false(is.na(out))`. With PATH emptied, `has_pdf_engine()` (utils_export.R:9-20) still returns TRUE wherever TinyTeX is installed (`tinytex::is_tinytex()`), as on the owner's machine, so the "no engine found" branch the comment says it exercises is not what is observed there.
- Failure scenario: `has_pdf_engine()` regressing to `TRUE` unconditionally passes.
- Confidence: verified-by-reading
- Suggested direction: mock `Sys.which`/`tinytex::is_tinytex` to force FALSE and assert FALSE.

### F-t-shiny-1-13  [nit] [test-weak]
- Where: tests/testthat/test-shinyapp-export.R:1575-1580
- What: The linear-time check floors the small measurement at 0.05 s, so any quadratic implementation whose 20 calls on a 10k-character row take under 25 ms passes (16 x 25 ms < 8 x 0.05 s).
- Evidence: `small <- max(med(mk(10000)), 0.05); big <- med(mk(40000)); expect_lt(big, 8 * small)`.
- Failure scenario: a fast-but-quadratic split (e.g. vectorised `substring()` walks) at small sizes is not detected; only slow quadratic ones are.
- Confidence: verified-by-reading (arithmetic on the thresholds)
- Suggested direction: scale the sizes up until `small` is well above the floor, or compare 4x vs 16x on a larger pair.

## test-shinyapp-vocabulary.R

### F-t-shiny-1-14  [blocker] [logic-error]
- Where: inst/shiny/dta_app/R/vocabulary.R:776-812 and 856-866; pinned by tests/testthat/test-shinyapp-vocabulary.R:1345-1364 (app side: inst/shiny/dta_app/app.R:2013-2034)
- What: An explicitly emptied vocabulary slot ("None" in the dialog) is documented and tested as "no terms", but `vocabulary_slot_values()` returns the same NULL it returns for an untouched slot, and the only consumer treats NULL as "leave the column alone" — so the column keeps the dataset template's `values_from:` binding and is expanded to the FULL vocabulary.
- Evidence:
  vocabulary.R:788 `# An EXPLICITLY EMPTY selection ... does not fall back: it means no terms.`; 809-811 `if (length(selection) == 0) { return(NULL) }`
  vocabulary.R:860-863 `values <- vocabulary_slot_values(slot, selections[[slot$id]], resolve_vocab); if (is.null(values)) { next }`
  app.R:2014-2018 `an EMPTIED control is recorded as character(0) ... the "None" link above it would be a lie if clearing the chips silently put the default back.`
  test 1360-1361 `# Engagement: an explicitly empty selection means no terms at all.` / `expect_null(slot_values(slot, character(0), resolver))`
  The test asserts only the intermediate NULL, which is indistinguishable from "slot left alone" (asserted by the same NULL at 1086-1087); no test runs an empty selection through `create_dta_from_template()` (only `"EOT"` at 1224-1229). A second test enshrines it too: tests/testthat/test-shinyapp-template-ui.R:1089-1124 is named "... an emptied control means no terms" but stops at `expect_identical(collect_vocab_selections(), list(visit_choice = character(0)))` and never builds the DTA.
  Create path checked: inst/shiny/dta_app/R/template_create.R:236-247 builds the imported dataset (which expands `values_from` to the full vocabulary) and only then applies the overrides, so a skipped override leaves the full list in place (same order on the inline path, 285-286).
  Severity: a generated specification carries permitted values that contradict the author's explicit, on-screen choice ("0 of 2 selected"), with two tests giving false assurance — "wrong results" per the brief.
- Failure scenario: the creation template of 1194-1230 (VISIT bound `values_from: visit@1.0`, slot default `[SCR, C1D1]`); the author clears the chips and clicks "None": `vocab_selections = list(visit_choice = character(0))` -> no override -> the built column's `values` is `c("SCR","C1D1","EOT")` — more terms than the default, not "none". The UI's "None" still lies, just differently from the defect the comment says was fixed.
- Confidence: verified-by-reading (override skip and its NULL source read; that a binding left in place is expanded is pinned by this file's 850-885 / 1121-1136)
- Suggested direction: return a distinct value for "engaged, empty" (e.g. `character(0)`) and have `resolve_vocabulary_slot_overrides()` write it (clearing `values_from`); add an end-to-end test with `vocab_selections = list(visit_choice = character(0))`.

### F-t-shiny-1-15  [minor] [test-weak]
- Where: tests/testthat/test-shinyapp-vocabulary.R:106-121 (check at inst/shiny/dta_app/R/vocabulary.R:257-261)
- What: The test is named "read_vocabulary() rejects a wrong kind, a missing id, and a missing version" but has no missing-id case; the id check is untested anywhere in tests/testthat or the qualification suite.
- Evidence: the body builds only `bad_kind` and `no_version`; `write_vocabulary_fixture()` always writes `paste0("id: ", id)`. grep for "Vocabulary must define a non-empty 'id'" in tests/ and inst/qualification/tests: no hit.
- Failure scenario: deleting vocabulary.R:258-261 (`if (!nzchar(id)) stop("Vocabulary must define a non-empty 'id'.")`) lets an id-less vocabulary into the index under id "" — the suite stays green.
- Confidence: verified-by-reading
- Suggested direction: add the case (e.g. `id = ""`) or rename the test.

### F-t-shiny-1-16  [minor] [test-weak]
- Where: tests/testthat/test-shinyapp-vocabulary.R:778-800 (code inst/shiny/dta_app/R/vocabulary.R:948-1004)
- What: "vocabulary_resolver() ... caches misses" cannot detect a missing cache: it only asserts that two consecutive lookups of an unknown ref both return NULL, which an uncached resolver also does.
- Evidence: `# A miss must stay a miss on the second call. ... a naive cache would re-resolve every miss forever; the sentinel is what makes this hold.` / `expect_null(resolve("nosuch@1.0")); expect_null(resolve("nosuch@1.0"))`
- Failure scenario: deleting `cache[[ref]] <- "__none__"` (vocabulary.R:963 and 997) — the re-resolve-every-miss regression the comment describes — passes.
- Confidence: verified-by-reading
- Suggested direction: count calls to the underlying lookup (e.g. wrap `resolve_template_ref` with a counting mock in app_env via `rlang::local_bindings`) and assert it is hit once.

### F-t-shiny-1-17  [nit] [test-weak]
- Where: tests/testthat/test-shinyapp-vocabulary.R:273-311 (warning at inst/shiny/dta_app/R/vocabulary.R:178-190)
- What: "malformed and duplicate terms are dropped in one counted warning each" never checks the count (or that there is only one warning).
- Evidence: `expect_warning(vocab <- read_vocabulary(malformed)$value, "malformed")` — the fixture has two malformed terms and the message is `dropping {malformed} malformed ...`; "2" is never asserted.
- Failure scenario: a counting slip (e.g. the list branch at vocabulary.R:164 setting rather than incrementing) reports "1 malformed" and passes.
- Confidence: verified-by-reading
- Suggested direction: match "dropping 2 malformed" / "dropping 1 duplicate".

### F-t-shiny-1-18  [minor] [bug]  (cross-slice: app code)
- Where: inst/shiny/dta_app/R/vocabulary.R:162, 173-174 (`.normalise_vocabulary_terms()`)
- What: Term fields are read with `$`, which partially matches list names, so an author's misspelt or longer key is silently taken as the code/label — the exact hazard this test file pins as fixed for `vocabulary`/`values_from` (647-663, 1057-1071) and that `columns:` entries reject outright ("unknown key 'feild'", 191-197).
- Evidence: `code <- t$code` ... `label = if (is.null(t$label)) NULL else as.character(t$label)`; `$` on a list does unique-prefix partial matching.
- Failure scenario: a term written `- codes: SCR` or `- code_value: SCR` is read as code "SCR" (and `- labels: x` as label "x") with no warning, while the same typo in `columns:` is an error.
- Confidence: verified-by-reading (R `$` semantics on lists; not executed)
- Suggested direction: read with `t[["code"]]`/`t[["label"]]` and reject unknown term keys as `columns:` does.

## test-shinyapp-dataset-template.R, test-shinyapp-template.R, test-shinyapp-file-handlers.R

(All three read in full. They exercise the app's own helpers through `app_fn()`
with literal expected values, positive and negative cases, and exact-message or
class matches; no cheating pattern found. One nit below.)

### F-t-shiny-1-19  [nit] [test-weak]
- Where: tests/testthat/test-shinyapp-file-handlers.R:721-733
- What: The test is named "dta_handler_fields() returns extensions as a comma-separated string" but only checks that the value is character and contains "pdf" and "zip" somewhere.
- Evidence: `expect_true(is.character(fields$extensions)); expect_match(fields$extensions, "pdf"); expect_match(fields$extensions, "zip")`.
- Failure scenario: a change to newline- or space-joined extensions (which the form's comma-splitting would then read back as one extension "pdf zip") passes.
- Confidence: verified-by-reading
- Suggested direction: `expect_identical(fields$extensions, "pdf, zip")` (or whatever separator the form splits on).

## test-shinyapp-template-ui-builders.R

(Read in full; rendered-HTML assertions on the app's own builders, mostly exact
markup. One weak test.)

### F-t-shiny-1-20  [minor] [test-weak]
- Where: tests/testthat/test-shinyapp-template-ui-builders.R:406-412 (script at inst/shiny/dta_app/R/template_ui.R:312-316)
- What: "the picker script guards itself so a re-render cannot double-bind" only checks that three substrings occur in the script text; the guard itself is not exercised or even pinned.
- Evidence: `expect_match(html, "window.DTA_pickerBound", fixed = TRUE)` — the code is `"  if (!window.DTA_pickerBound) {", "    window.DTA_pickerBound = true;",`; the substring is present in both lines.
- Failure scenario: deleting the `if (!window.DTA_pickerBound) {` guard (keeping the assignment), or inverting it, re-binds every delegated document listener on each keystroke-driven re-render — exactly the regression named — and the test still passes.
- Confidence: verified-by-reading
- Suggested direction: at minimum match the guard expression `if (!window.DTA_pickerBound)`; better, evaluate the script twice in a JS engine (V8, which is not in Suggests) or the existing chromote browser test and count listeners.

## test-shinyapp-dta-ops.R

### F-t-shiny-1-21  [minor] [test-weak]
- Where: tests/testthat/test-shinyapp-dta-ops.R:480-497 (same pattern as F-t-shiny-1-07)
- What: "dta_set_column refuses a rename onto an id another column already holds" proves "Nothing was mutated" by inspecting the caller's own `dta`, which `fn()` cannot modify; the returned value is never checked.
- Evidence: `res <- fn(dta, "clinical_data", id = "STUDYID", old_id = "VISIT")` ... `# Nothing was mutated: both columns are still present and distinct.` / `ids <- ids_fn(dta, "clinical_data")` / `cols <- datasets(dta, "clinical_data")@specs@columns`.
- Failure scenario: a regression that returns `ok = FALSE` together with a `value` in which STUDYID was overwritten (a caller doing `res$value %||% dta` would adopt it) passes; the four assertions on `dta` can never fail.
- Confidence: verified-by-reading
- Suggested direction: assert `expect_null(res$value)` and drop the self-inspection.

### F-t-shiny-1-22  [minor] [test-weak]
- Where: tests/testthat/test-shinyapp-dta-ops.R:127-160, 204-232, 249-256, 309-317, 546-553, 590-597, 694-701, 715-722, 750-757, 808-815, 835-842, 858-865, 905-915, 931-938, 954-961
- What: Every "returns ok=FALSE without throwing for <bad input>" test asserts only `!res$ok` and `nzchar(res$error)`. Because each mutator wraps its body in `dta_try()`, ANY internal error satisfies them — the file's own header (lines 9-13) names exactly this degradation and promises tests "assert on the actual VALUES produced ... rather than merely on the list's type", which these do not.
- Evidence: e.g. 931-938 `res <- fn(app_fixture_dta(), "receiver", 99, list(email = "x@example.com")); expect_false(res$ok); expect_true(nzchar(res$error))` — the guard under test is utils_dta.R:5048 `stop("Contact index out of range.")`, never matched.
- Failure scenario: deleting that guard makes the call fail later with base R's translated "subscript out of bounds" (shown to the user verbatim by the app) — still `ok = FALSE`, still non-empty, still green. The same holds for every guard in the listed tests (unknown field, invalid side, bad direction, unknown rule type).
- Confidence: verified-by-reading (guard text for dta_update_contact read; the others inferred to follow the same `dta_try()` shape, which the file header states)
- Suggested direction: match each app-authored message (`fixed = TRUE`), as the file already does at 384 and 489.

## test-shinyapp-template-rebase.R

### F-t-shiny-1-23  [minor] [test-weak]
- Where: tests/testthat/test-shinyapp-template-rebase.R:490-517 (same pattern as F-t-shiny-1-07/-21)
- What: "rebase_apply() errors on an unresolved conflict and leaves the input DTA untouched" proves the second half by re-reading the caller's own `current`, which `rebase_apply()` cannot modify (no data is bound, the metadata is a plain S7 value); `applied$value` is never checked.
- Evidence: `applied <- app_fn("rebase_apply")(current, plan, resolutions = list())` ... `# \`current\` itself -- the object the caller still holds -- must be byte-for-byte what it was before the failed call: a half-applied rebase would silently corrupt a signed specification.` / `after_md <- DTAtools::metadata(current)` + four `expect_equal()`s on it.
- Failure scenario: a regression that returns `ok = FALSE` with a half-rebased `value` (the real "half-applied rebase" risk, since the app reads the result object) passes; the four assertions can never fail.
- Confidence: verified-by-reading
- Suggested direction: `expect_null(applied$value)`; drop the re-reads of `current`.

### F-t-shiny-1-24  [minor] [test-weak]
- Where: tests/testthat/test-shinyapp-template-rebase.R:642-656 (function list_set_path, inst/shiny/dta_app/R/template_core.R)
- What: An unmarked test pins a data-corrupting behaviour of `list_set_path()` as the expected result, without the repo's mandatory `KNOWN DEFECT, pinned rather than endorsed` marker or a named assertion to switch to.
- Evidence: `corrupted <- set_path(vh, c("1", "version"), "1.1")` / `expect_length(corrupted, 2)` / `expect_true("1" %in% names(corrupted))` — writing a positional key appends a stub entry named "1" to an unnamed list; the comment calls it corruption and uses it only to justify excluding `version_history` from rebases.
- Failure scenario: making `list_set_path()` refuse (or correctly index) a numeric key on an unnamed list — the fix for the corruption — fails this test, so the test defends the defect; and any other caller writing a positional path (a template `set:` patch or option target such as `metadata.version_history.1.changes`) silently gets the corrupt two-entry list today.
- Confidence: verified-by-reading for the test; that other callers can reach `list_set_path()` with a positional segment is inferred (template targets into `version_history.<n>` were not traced)
- Suggested direction: make `list_set_path()` abort on a positional segment into an unnamed list, and turn this into an expect_error; or mark it KNOWN DEFECT per [assistant-tool].md.

### F-t-shiny-1-25  [minor] [dead-code]
- Where: inst/shiny/dta_app/R/template_diff.R (`rebase_derive_selections()`, first 3 lines of the body); pinned by tests/testthat/test-shinyapp-template-rebase.R:368-369
- What: A compatibility branch replays `prov$selections` "for documents written before the field was dropped" — owner priority 4 says such readers are dead weight before 1.0 — and the test enshrines it.
- Evidence: `# \`prov$selections\`, when present, still wins: documents written before the field was dropped keep replaying exactly what they recorded.` / `if (!is.null(prov$selections)) { return(prov$selections) }`; test: `# A document still carrying recorded selections replays exactly those.`; 405 asserts new documents never carry `selections`.
- Failure scenario: none functionally today; the branch can only fire for legacy files, and it bypasses the variant/option re-derivation below it.
- Confidence: verified-by-reading
- Suggested direction: delete the branch and the 368-369 assertion.

### F-t-shiny-1-26  [minor] [code-cheat]  (cross-slice: app code)
- Where: inst/shiny/dta_app/R/template_diff.R (`rebase_derive_selections()`, the `def$variants` branch)
- What: A failure to resolve the document's recorded variant is swallowed and the options are read from the UNRESOLVED family definition instead.
- Evidence: `def <- tryCatch(resolve_template_variant(def, variant_sel), error = function(e) def)` — the comment directly above says the variant's own options must be read "not the unresolved family".
- Failure scenario: the target version renames/removes the recorded variant value; resolution errors, the error vanishes, and the rebase re-derives answers from the family's base options — option answers the variant contributed are silently not re-derived and fall back to defaults in the plan, with no message.
- Confidence: verified-by-reading (code path); the downstream effect on a concrete plan was not executed
- Suggested direction: let the error surface (or record it in the plan) instead of substituting the unresolved definition.

## test-shinyapp-template-sources.R

(Read in full. The git tests use a real local bare repository and ask git
itself (`config --get-urlmatch`) which header it would send — good practice.)

### F-t-shiny-1-27  [minor] [dead-code]
- Where: inst/shiny/dta_app/R/template_core.R:44-75 (`list_dta_creation_templates()`, `get_dta_creation_template_path()`); kept alive only by tests/testthat/test-shinyapp-template.R:45-76, 264-299, 364-381 and tests/testthat/test-shinyapp-template-sources.R:720-739
- What: The two legacy lookup helpers have no production caller — app.R says so itself — yet four test blocks keep exercising them (including a path-traversal "cannot escape" test of a function nothing calls), which hides that they are dead.
- Evidence: inst/shiny/dta_app/app.R:2456-2459 `rather than the legacy list_dta_creation_templates()/get_dta_creation_template_path() pair -- those still exist (template_core.R) but have no remaining callers in the app`; grep of R/, inst/shiny/dta_app and inst/qualification finds only their definitions.
- Failure scenario: none at run time; dead code plus tests whose passing says nothing about the app (the app resolves templates through the index).
- Confidence: verified-by-reading (grep across R/, inst/shiny/dta_app, inst/qualification; comment at app.R:2456)
- Suggested direction: delete both helpers and their tests; keep the traversal test for the index path instead.

### F-t-shiny-1-28  [nit] [test-weak]
- Where: tests/testthat/test-shinyapp-template-sources.R:348-361
- What: The test is named "resolve_template_source() resolves pkg:DTAtools", but it asserts the opposite — that the source FAILS to resolve.
- Evidence: `test_that("resolve_template_source() resolves pkg:DTAtools", {` ... `expect_false(result$ok)`.
- Failure scenario: misleading name; the success path of `pkg:` (a package that does ship `dta-templates/`) is never tested here.
- Confidence: verified-by-reading
- Suggested direction: rename to "... reports a package without dta-templates/" and add a success case with a temp package fixture (or `.libPaths()` stub).

### F-t-shiny-1-29  [nit] [inconsistency]
- Where: tests/testthat/test-shinyapp-template-sources.R:432-443 vs tests/testthat/helper-shinyapp.R:77-79
- What: The test mocks by `assign()`-ing into the shared, cached `app_env()`, which the harness documents as forbidden.
- Evidence: helper: `Tests must therefore treat it as read-only: assign nothing into it, and mock via local_mocked_bindings() rather than by overwriting a binding in place.`; test: `assign("dta_template_git_run", function(...) ..., envir = app_env())` restored by `withr::defer(assign(..., real_run, ...))`. (test-shinyapp-export.R:1278 explains that `local_mocked_bindings()` cannot reach app_env() at all and uses `rlang::local_bindings()`.)
- Failure scenario: none today (the defer is registered first); the harness rule is simply wrong/stale, and the next author following it cannot mock an app helper.
- Confidence: verified-by-reading
- Suggested direction: fix the helper comment to prescribe `rlang::local_bindings(.env = app_env())`, and use that here.

## test-shinyapp-template-parties.R, test-shinyapp-containers-helpers.R

(Both read in full; literal expected values, exact messages, positive and
negative cases. The only mock — `DTAtools::load_delivery` at
containers-helpers.R:416-429 — stands in for the dependency of the wrapper under
test, which is legitimate. No findings. Note for F-t-shiny-1-14: the party-slot
engine DOES distinguish absent from explicitly-empty (parties.R:527-556 pins
both outcomes on the built DTA), which shows the vocabulary slots' NULL/NULL
conflation is an inconsistency between the two slot kinds, not a shared design.)

## test-shinyapp-word-template-discovery.R (and helper-docx.R)

### F-t-shiny-1-30  [minor] [test-weak]
- Where: tests/testthat/test-shinyapp-word-template-discovery.R:127-143 (code inst/shiny/dta_app/R/app_config.R, `dta_app_config_discover_word_templates()`)
- What: "a symlink to a .docx inside the library is allowed" cannot tell "allowed" from "rejected": `expect_setequal()` against a duplicated expectation is satisfied by a single "real" row, which is exactly what a rejecting implementation would produce.
- Evidence: `expect_setequal(got$label, c("real", "real"))`. In the code, `.cfg_file_in_dir()` resolves the link to `.../real.docx`, the label is taken from the RESOLVED name (`tools::file_path_sans_ext(basename(r$value))`), and the second visit to the same path is skipped (`if (!identical(taken[[key]], r$value)) ... next`), so the observed result is one row "real" whether the alias is accepted or refused; the duplicated expected vector shows two rows were expected.
- Failure scenario: changing `.cfg_file_in_dir()` to refuse every symlink (inside the library too) leaves this test green. (Side observation: a linked template is labelled after its target, not after the link the library author named.)
- Confidence: verified-by-reading
- Suggested direction: link to a target with a different stem in a subfolder the scan does not list, and assert that label/path pair is present (`expect_identical`, not setequal).

### F-t-shiny-1-31  [nit] [test-weak]
- Where: tests/testthat/test-shinyapp-word-template-discovery.R:566-574
- What: "a correct library ... shows no layout warning" asserts only negatives on `output$template_picker_ui` (no positive control on that output) and wraps its read in `suppressWarnings()`, so an empty render or an index build that suddenly warns on a correct library both pass.
- Evidence: `frame <- suppressWarnings(ui_text(output$template_picker_ui))` / `expect_no_match(frame, "belongs in", ...)` / `expect_no_match(frame, "No creation templates were found", ...)`; the positive check is on a different output (`output$template_picker_list`). `ui_text()` returns "" for a NULL render.
- Failure scenario: a regression making `template_picker_ui` render NULL, or making a correct library emit a layout warning through `cli_warn()` (the R warning is muffled), is not caught.
- Confidence: verified-by-reading
- Suggested direction: assert something the frame must contain, and use `expect_no_warning()` instead of muffling for the correct-library case.

## test-shinyapp-file-dataset-server.R

### F-t-shiny-1-32  [minor] [test-weak]
- Where: tests/testthat/test-shinyapp-file-dataset-server.R:143-161
- What: "an ending the handler does not allow is refused as it is uploaded" only checks that nothing was bound; it never checks that the refusal is reported (load-error log, notice), so a silently dropped upload passes.
- Evidence: the only assertion is `expect_equal(length(DTAtools::datasets(rv$dta, "reports")@file_paths), 0L)`. The matrix tests (test-shinyapp-matrix.R:1387-1412) show the app is expected to log a refused upload in `rv$load_errors` and count it in the dock.
- Failure scenario: a regression in which the upload observer ignores every file for a Files dataset (or drops a refused one without a message — the user sees nothing happen) leaves this test green; only the separate positive test 78-104 would notice the first case, nothing notices the second.
- Confidence: verified-by-reading (that the Files-dataset refusal goes through the same `rv$load_errors` path was not traced)
- Suggested direction: assert `nrow(rv$load_errors) == 1` and that its message names `report_2024.csv`.

### F-t-shiny-1-33  [nit] [test-weak]
- Where: tests/testthat/test-shinyapp-file-dataset-server.R:417-426
- What: The "refuse a stashed editor_dataset" test proves nothing changed by inspecting `clinical_data`, but the forged `col_save`/`rule_save` would target `rv$editor_dataset == "reports"`, so the `clinical_data` comparisons cannot fail; the only effective checks are `expect_null(rv$col_msg)` / `expect_null(rv$rule_msg)`.
- Evidence: `expect_equal(rv$editor_dataset, "reports")` ... `before_cols <- app_fn("dta_column_ids")(rv$dta, "clinical_data")` ... `expect_equal(app_fn("dta_column_ids")(rv$dta, "clinical_data"), before_cols)`.
- Failure scenario: a guard that lets the save through and writes a column spec onto the Files dataset's object some other way (without setting `col_msg`) passes; the assertion that looks like the guard check is on an unrelated dataset.
- Confidence: verified-by-reading
- Suggested direction: assert on the "reports" dataset (class/props unchanged) instead of on clinical_data.

## test-shinyapp-upload-guard.R

(Read in full. Strong file: each refusal reason is pinned separately, the
staging step is tested for cleanup and junction/symlink aliases, and a canary
pins the Shiny internals the guard depends on. One finding, in the code it
tests.)

### F-t-shiny-1-34  [minor] [code-cheat]
- Where: inst/shiny/dta_app/R/utils_dta.R:612-620 (`dta_upload_guard_install()`); pinned by tests/testthat/test-shinyapp-upload-guard.R:128-132
- What: The security guard's production code special-cases the test harness's session class: when the session is a `MockShinySession` the client-message callback is silently not registered, so every `testServer()` test of the upload observers runs the guard with its session binding switched off.
- Evidence: `if (is.function(session$onInputReceived)) { session$onInputReceived(guard$note) } else if (!inherits(session, "MockShinySession")) { stop("... the upload guard cannot run.", call. = FALSE) }`; test: `mock <- structure(list(), class = "MockShinySession"); expect_no_error(install(make(), mock, list()))`. The server tests then simulate the binding by hand (`upload_guard$note(list(dta_file = TRUE))`, 375-411).
- Failure scenario: the brief's "behaviour that differs under test" pattern in a security path. Any regression in how the live callback marks client-written values is invisible to the server tests (they never receive a client message), and the class-name exemption is an escape hatch in production code that exists only so the suite can run.
- Confidence: verified-by-reading
- Suggested direction: give the test harness a session double that HAS `onInputReceived` (wrapping MockShinySession) instead of exempting a class name in app code; drive marks through that callback.

## test-shinyapp-file-dataset.R

### F-t-shiny-1-35  [nit] [doc-wrong]
- Where: tests/testthat/test-shinyapp-file-dataset.R:44-47, 89-92, 120-123, 156-159, 292-295, 325-328 (contradicted by 341-371 and inst/shiny/dta_app/R/utils_dta.R:781-791)
- What: A comment repeated six times says `dta_load_file()` defaults the key to `file_path_sans_ext()`, "wrong here, so it has to be given explicitly" — but the default now comes from the dataset type and keeps the extension for a Files dataset, as this file's own last test pins.
- Evidence: `# name defaults to file_path_sans_ext() inside dta_load_file(), which is right for a tabular dataset's table name but wrong here, so it has to be given explicitly.` vs utils_dta.R:781-784 `The default key belongs to the DATASET TYPE ... a file dataset keys by the delivered name, extension kept` and test 356-359 (no `name`, expects "report.pdf").
- Failure scenario: misleads the next author; the six call sites still pass `name` for a reason that no longer exists, so the default path is exercised only by 341-371.
- Confidence: verified-by-reading
- Suggested direction: delete the stale comments (and the now-redundant `name =` arguments, or keep one call that passes it on purpose).

## test-shinyapp-theme.R

(Read in full; most assertions are literal, the WCAG test re-derives contrast
independently and sweeps a colour cube. One finding.)

### F-t-shiny-1-36  [minor] [dead-code]
- Where: inst/shiny/dta_app/R/theme.R:1615-1628 (`slot_state_label()`); kept alive only by tests/testthat/test-shinyapp-theme.R:91-128
- What: `slot_state_label()` has no caller anywhere in the app (only its definition matches across inst/shiny/dta_app, R/, inst/qualification, vignettes); three tests keep it alive, and one of them (120-128) pins a silent-fallback behaviour — an unknown state renders a class-less span — that the neighbouring `status_chip()` test (79-89) calls wrong ("rejects an unrecognised status instead of silently rendering").
- Evidence: theme.R `icon <- switch(state, ok = ..., warn = ..., empty = ...)` with no default; test 120-127 `# state's icon/class come from switch() with no default branch, so an unmatched state yields NULL icon/class rather than an error` / `expect_false(grepl('class="', out, fixed = TRUE))`.
- Failure scenario: none at run time; dead code whose tests enshrine a silent degradation the codebase elsewhere treats as a defect.
- Confidence: verified-by-reading (grep across the snapshot outside tests: definition only)
- Suggested direction: delete `slot_state_label()` and its three tests.

## test-shinyapp-template-variants.R

(Read in full; platform resolution, seal enforcement and provenance are pinned
with literal values and condition classes, and a server test drives the real
dialog. No findings.)

## test-shinyapp-create-empty.R

### F-t-shiny-1-37  [minor] [test-weak]
- Where: tests/testthat/test-shinyapp-create-empty.R:247-286 and 288-312 (observer inst/shiny/dta_app/app.R:11037-11200)
- What: Both "survives a reload" tests press `restore_session` WITHOUT first discarding the live state, and every assertion afterwards already held before the restore — so a restore that silently does nothing passes both.
- Evidence: 258-263 `create_new(...); expect_true(rv$editing); session$setInputs(restore_session = 1); expect_true(rv$editing)`; then `expect_null(rv$active)`, `expect_null(rv$version_baseline_yaml)`, `expect_length(datasets(rv$dta), 0)`, title — all true of the pre-restore document. 307-310 likewise (`rv$active` is already NULL for an empty document). Contrast test-shinyapp-server.R:143-147, which nulls `rv$dta`/`rv$structure`/`rv$active` to "simulate the reload". The observer bumps `rv$doc_token` (app.R:11191) and sets `rv$editing <- isTRUE(saved$editing) && ...` (11188), neither of which is checked.
- Failure scenario: an early `return()` in the restore observer (or any refusal path — e.g. the user check at 11050 or a `dta_restore_session()` error turned into NULL at 11056-11060) leaves both tests green; so does a restore that stops applying `saved$editing`, the very regression 247-254 says it guards — after a real reload `rv$editing` starts FALSE (app.R:1311), but here it is still TRUE from `create_new()`.
- Confidence: verified-by-reading
- Suggested direction: reset `rv$editing`/`rv$dta`/`rv$active` (or start a second `testServer()` with the same client id) before restoring, and assert `rv$doc_token` advanced.

## test-shinyapp-click-guard.R

(Read in full. The file states its own limit — "SOURCE-LEVEL assertions ... no
test runs the guard in a browser" — and then claims "Each test below names that
failure rather than checking that a word is present". Several do not meet that
claim: they pin a name, not the value or the call that carries the property.)

### F-t-shiny-1-38  [minor] [test-weak]
- Where: tests/testthat/test-shinyapp-click-guard.R:110-129, 131-151, 153-170, 75-97 (script at inst/shiny/dta_app/R/ui_components.R:547-650)
- What: The safety properties the tests are named after (bounded re-arm, download cooldown, ceiling always armed, untrusted clicks bail out) are asserted as the presence/order of identifiers, so the exact failures they describe pass.
- Evidence:
  153-170 "re-arms ... but only so often" checks the re-arm line and `expect_match(guard_js(), "var CEILING_TRIES", fixed = TRUE)` — the value (`var CEILING_TRIES  = 4;`, ui_components.R:550) is never read, unlike SANITY_HOLD at 201-205.
  110-129 checks `DOWNLOAD_HOLD` is named in the download branch; its value (`= 2000`, :548) is unchecked.
  131-151 checks the substring `armCeiling(el, 0)` occurs in `hold()`.
  75-97 checks only that the first `!ev.isTrusted` precedes the first `preventDefault`.
- Failure scenario: `var CEILING_TRIES = Infinity;` gives exactly the unbounded deferral the test's comment says "would then defer forever"; `var DOWNLOAD_HOLD = 1e9;` leaves a download button dead after one click; commenting out `// armCeiling(el, 0);` removes the failsafe while the substring remains; `if (!ev.isTrusted) {}` (bail-out without `return`) keeps the ordering. All four pass.
- Confidence: verified-by-reading
- Suggested direction: parse and bound the constants (`CEILING_TRIES` finite and small, `DOWNLOAD_HOLD` within a sane range), match whole statements (`if (!ev.isTrusted) return;`, a non-comment `armCeiling(el, 0);`), or run the script under the existing chromote harness with stubbed timers.

## test-shinyapp-template-latest.R

### F-t-shiny-1-39  [minor] [test-weak]
- Where: tests/testthat/test-shinyapp-template-latest.R:74-89 (also 91-109)
- What: The test says its fixture proves `@latest` ranks versions numerically ("a string sort would pick 1.9"), but the fixture contains only 1.0 and 1.10 — no 1.9 — and for that pair a string sort also returns "1.10".
- Evidence: `write_base_ct(root, "1.0", "v1.0"); write_base_ct(root, "1.10", "v1.10")` / `# 1.10 is NEWER than 1.9/1.0 -- a string sort would pick 1.9, which is the whole reason versions are ranked with numeric_version().` / `expect_equal(fn(idx, "base_ct@latest")$version, "1.10")`. Lexicographically "1.10" > "1.0" (third character '1' > '0'). 91-109 uses the same 1.0/1.10 pair.
- Failure scenario: replacing the numeric_version ranking in `resolve_template_ref()` with a plain string `max()`/`sort()` keeps both tests green, yet resolves `x@latest` to 1.9 instead of 1.10 the moment a 1.9 exists.
- Confidence: verified-by-reading (string ordering of the two literals reasoned, not executed)
- Suggested direction: add a `1.9` fixture so the string and numeric maxima differ.

### F-t-shiny-1-40  [nit] [inconsistency]
- Where: tests/testthat/test-shinyapp-template-latest.R:11-29, 70-72
- What: The file re-defines `local_clean_template_env()` (shadowing the shared helper in helper-shinyapp.R:356-374 with a different body) and defines `index_over(root)` whose `root` argument is ignored — the index comes from whatever `DTATOOLS_TEMPLATE_SOURCES` the caller set, unlike the same-named helper in test-shinyapp-template-rebase.R:13-18 that sets it from `root`.
- Evidence: `index_over <- function(root) { app_fn("build_template_index")() }`.
- Failure scenario: a test calling `index_over(other_root)` without also setting the env var silently indexes the wrong library.
- Confidence: verified-by-reading
- Suggested direction: use the shared helper and drop the unused parameter (or set the source from it).

## test-shinyapp-shared-worker-limits.R, test-shinyapp-upload-boundary.R

(Both read in full; no findings. Cross-user isolation of the autosave — the
probe left open at test-shinyapp-server.R:155-177, where `session$user` is NULL —
IS covered: upload-boundary.R:145-175 writes a slot as "alice", shows "bob"
cannot restore it and "alice" can, and pins that the file name folds in the user.
The transcode-cache test at shared-worker-limits.R:124-136 would also pass if
caching were disabled outright, but it targets pruning, which it does catch.)

## test-shinyapp-file-count.R

### F-t-shiny-1-41  [nit] [test-weak]
- Where: tests/testthat/test-shinyapp-file-count.R:165-177
- What: "the report banner never reads PASSED over a recorded count failure" has only a negative assertion, so an empty or truncated report passes.
- Evidence: `html <- app_fn("dta_build_validation_report")(dta, status = list(d_ok = "pass"))` / `expect_false(grepl("VALIDATION PASSED", html, fixed = TRUE))` — no expectation of which banner IS shown (139-150 asserts "VALIDATION FAILED" for the ordinary case).
- Failure scenario: a builder that, for a status map/attribute disagreement, returns "" or a page with no banner at all passes.
- Confidence: verified-by-reading
- Suggested direction: also `expect_match(html, "VALIDATION FAILED", fixed = TRUE)`.

## test-shinyapp-modal-outputs.R, test-shinyapp-modal-scroll-lock.R, test-shinyapp-authorized-corrections.R

(All read in full. modal-outputs and modal-scroll-lock are source-level by
their own statement; the scroll-lock behaviour is executed by
test-shinyapp-modal-browser.R (not in this slice), the unsuspend snippet is not
executed anywhere. Every test in modal-outputs carries a positive `expect_match`,
so a missing binding fails. authorized-corrections mocks
`shiny::updateSelectizeInput` only to capture the message the observer sends —
legitimate.)

### F-t-shiny-1-42  [nit] [test-weak]
- Where: tests/testthat/test-shinyapp-modal-scroll-lock.R:103-109 and 54-56 (helper tests/testthat/helper-shinyapp.R:201-216)
- What: `app_r_binding()` returns `NA_character_` for a missing snippet and its comment promises that this "fails loudly in the tests that assert on it instead of passing vacuously" — but a test made only of `expect_no_match()` passes on NA, and `expect_type(js, "character")` (used as a presence check) is TRUE for `NA_character_`.
- Evidence: 103-109 `js <- lock_js(); expect_no_match(js, "</script", fixed = TRUE)` (the only assertion); 55-56 `js <- lock_js(); expect_type(js, "character")`. `grepl()` on NA is FALSE, so `expect_no_match` succeeds.
- Failure scenario: renaming `modal_scroll_lock_js` leaves "the snippet survives being embedded in the page" green (the file's other tests do fail, so the suite still notices — the individual test does not).
- Confidence: verified-by-reading (testthat's `expect_no_match` semantics on NA reasoned from `grepl`, not executed)
- Suggested direction: have `app_r_binding()` fail (`testthat::fail()`/`stop`) when the binding is absent, rather than returning NA.

---

## Summary

42 findings: 1 blocker, 1 major, 23 minor, 17 nit.
By category: test-weak 28, dead-code 5, code-cheat 2, doc-wrong 2,
inconsistency 2, test-cheat 1, logic-error 1, bug 1.

Patterns worth acting on across the slice:
- Tests that exercise a replica of the code instead of the code (F-01, F-08,
  F-10) and source-text checks that keep passing when the property is gone
  (F-11, F-20, F-38).
- "Nothing changed" proved by re-reading the caller's own immutable input
  (F-07, F-21, F-23) or by asserting a state that already held before the
  action (F-37).
- Refusal tests satisfied by any failure (F-22) or by one row where two were
  expected (F-30).
- Production code that changes behaviour for the test harness (F-34) and
  compat/dead code kept alive only by tests (F-03, F-25, F-27, F-36).
- One real behavioural defect hidden by a test that asserts an intermediate
  value (F-14: an explicitly emptied vocabulary slot yields the full vocabulary).

## Coverage

Fully read (every line), slice: test-shinyapp-server.R, -matrix.R, -export.R,
-vocabulary.R, -dataset-template.R, -template.R, -file-handlers.R,
-template-ui-builders.R, -dta-ops.R, -template-rebase.R, -template-sources.R,
-template-parties.R, -containers-helpers.R, -word-template-discovery.R,
-file-dataset-server.R, -upload-guard.R, -file-dataset.R, -theme.R,
-template-variants.R, -create-empty.R, -click-guard.R, -template-latest.R,
-shared-worker-limits.R, -file-count.R, -upload-boundary.R, -modal-outputs.R,
-authorized-corrections.R, -modal-scroll-lock.R (28/28).

Fully read, shared helpers: helper-shinyapp.R, helper-source-tree.R,
helper-unchecked-collection.R, helper-containers.R, helper-docx.R.

Not read: helper-validation-corpus.R (no test-shinyapp-*.R file references any
of its functions — grep for vc_specs/vc_case/vc_corpus/vc_numeric_edges).

App/package code opened only to confirm or refute specific claims (targeted
reads, not reviewed as a whole): inst/shiny/dta_app/app.R (download handlers,
restore observer, accept_upload, collect_*_selections, col_tbl, dataset_only
uses); under inst/shiny/dta_app/R/: utils_dta.R (dta_read_yaml,
dta_set_column, dta_load_file, upload guard, dta_specs_signature,
dta_dataset_messages), vocabulary.R (read_vocabulary, term normalisation, slot
values/overrides, resolvers), template_core.R (legacy template listing),
template_diff.R (rebase_derive_selections), app_config.R (Word-template
discovery, parse_dta_app_config), utils_export.R (CSP page, has_pdf_engine),
ui_components.R (click guard script), template_ui.R (picker script), theme.R
(slot_state_label); package: R/DTAFileTabular-class.R (transcode cache),
R/validationReporting.R (message columns).

Not done (by the brief's rules): no R/test execution; JS snippets were judged
from source only.
