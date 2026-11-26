# Findings — slice c1-spec (S7 specification model)

Reviewer key: `c1-spec`. Tree: `$SNAP` (pinned `68408c44`). All paths relative to `$SNAP`.

Slice (22 files, 11 077 lines):
`R/00_helpers.R`, `R/DTA-class.R`, `R/DTAColumnSpec-class.R`, `R/DTAColumnSpecCell-class.R`,
`R/DTAColumnSpecCollection-class.R`, `R/DTAColumnSpecRowLabel-class.R`,
`R/DTAColumnSpecStructure-class.R`, `R/DTAColumnSpecStructureR-class.R`,
`R/DTAColumnSpecStructureSAS-class.R`, `R/DTAContainer-class.R`, `R/DTAMatrixSpec-class.R`,
`R/DTAMetaData-class.R`, `R/DTAMetaData-helpers.R`, `R/DTARule-class.R`,
`R/DTARuleColCondition-class.R`, `R/DTARuleColRange-class.R`, `R/DTARuleColUnique-class.R`,
`R/DTARuleGroupCondition-class.R`, `R/DTAtools-package.R`, `R/columnSpecChecks.R`,
`R/lintDTA.R`, `R/zzz.R`

Files fully read: all 22 (see "Coverage" at the end). 62 findings, F-c1-spec-01 … F-c1-spec-62.

Note for the coordinator on `%||%`: inside the package (`R/`) the operator is rlang's
(`NAMESPACE`: `importFrom(rlang,"%||%")`; `R/00_helpers.R:1-11`), i.e. NULL-only. The
"zero-length is absent" variant is defined only in the Shiny app
(`inst/shiny/dta_app/R/utils_dta.R:35`) and one test file. Every `%||%` in this slice was
read with the NULL-only meaning; zero-length inputs therefore reach the code behind the
operator (see F-c1-spec-20, -33, -49, -53).

---

## Area 1 — `R/00_helpers.R`, `R/DTAtools-package.R`, `R/zzz.R`, `R/DTA-class.R` (all four read completely)

### F-c1-spec-01  [minor] [bug]
- Where: R/00_helpers.R:727-732 (also :598, :730)
- What: `DTADataSetFactory()` never checks `type` for NULL/length although its own comment says it does, and its error text is a copy-paste of the *structure* factory's ("'type' prefix … must be one of the supported:").
- Evidence:
  ```r
  # check that type is not NULL and
  if (!type %in% `__DTAtools_supported_dataset_types__`) {
    cli_abort(
      "'type' prefix '{type}'must be one of the supported: {str_flatten_comma(`__DTAtools_supported_dataset_types__`)}"
  ```
  The comment is a truncated sentence; `!NULL %in% x` is `logical(0)`, so `if()` throws base R's "argument is of length zero" (translated on a non-English machine), and a length-2 `type` throws "the condition has length > 1". A dataset type has no "prefix"; the message also lacks a space before `must` and the noun after "supported".
- Failure scenario: `DTADataSetFactory(type = NULL, id = "x")` -> base error `argument is of length zero` instead of a `cli` error naming `type`; `DTADataSetFactory(type = "tabel", id = "x")` -> "'type' prefix 'tabel'must be one of the supported: tabular, file, matrix".
- Confidence: verified-by-reading
- Suggested direction: validate `is.character(type) && length(type) == 1 && !is.na(type)` first with a `cli_abort()`, and reword the message ("Dataset type {.val {type}} must be one of …").

### F-c1-spec-02  [nit] [doc-wrong]
- Where: R/00_helpers.R:563-564, :598
- What: `DTAColumnSpecStructureFactory()` documents the backend prefix as optional ("potentially prefixed") while the code makes it mandatory, and the `type` error message is missing a space.
- Evidence:
  ```r
  #' @param type Character. The type specification, potentially prefixed with a backend identifier.
  ...
  "'type' prefix '{type_info$prefix}'must be one of the supported backends: ..."
  ```
  `__extract_prefix_and_rest__("Char")` returns `prefix = "Char"`, which is then rejected as an unsupported backend — an unprefixed type is never accepted. The sibling `format` message two lines below has the space (`' must be`).
- Failure scenario: a reader follows the `@param` text, calls `DTAColumnSpecStructureFactory(type = "Char")` and gets "'type' prefix 'Char'must be one of the supported backends: SAS, R".
- Confidence: verified-by-reading
- Suggested direction: say "prefixed with its backend (`SAS ` or `R `)" for both `type` and `format`; fix the missing space.

### F-c1-spec-03  [minor] [doc-wrong]
- Where: R/00_helpers.R:877-895
- What: The roxygen of `DTAFileFactory()` describes column-type backends, not file types, and refers to two "supported lists" that no longer exist.
- Evidence:
  ```r
  #' Constructs a DTAFile object for a specified backend (e.g., SAS or R),
  #' based on the provided type and file path.
  #' `type` may name any format in either supported list: ...
  #' @return An object derived from class \code{DTAFile}, depending on the backend specified.
  ```
  The function dispatches on file type (`csv`, `tsv`, `any`, `sas7bdat`, `xpt`); "SAS or R" are the backends of `DTAColumnSpecStructureFactory()` (the block was copied from there). There is one list, `__DTAtools_supported_file_types__`, whose own comment (:487-493) calls it "just the union".
- Failure scenario: documentation states a contract (`backend`) the function does not have.
- Confidence: verified-by-reading
- Suggested direction: rewrite description/@return in terms of file type; drop "either supported list".

### F-c1-spec-04  [minor] [bug]
- Where: R/00_helpers.R:463-485
- What: `dta_resolve_stream_mode()` uses `getOption("DTAtools.stream_threshold")` unvalidated, so the function can return `NA`, a vector, or the result of a *string* comparison, contradicting its `@return` ("A single `TRUE` … or `FALSE`").
- Evidence:
  ```r
  threshold <- getOption("DTAtools.stream_threshold", `__DTAtools_stream_threshold_default__`)
  size <- file.size(file)
  ...
  size > threshold
  ```
  The two sibling options (`DTAtools.stream_block_size`, `DTAtools.transcode_block_bytes`) are checked for type/length/NA/range with a `cli_abort()`; this one is not.
- Failure scenario: `options(DTAtools.stream_threshold = "1e9")` -> `size > "1e9"` coerces the size to a string and compares text: a 900 MB file (`"943718400" > "1e9"` is TRUE, first character `9` > `1`) streams although it is below the intended 1e9 bytes, while a 1.2 GB file (`"1288490188" > "1e9"` is FALSE, `2` sorts before `e`) is read into memory although it is above it. `options(DTAtools.stream_threshold = NA)` -> returns `NA`, and the caller's `if()` dies with a translated base error.
- Confidence: verified-by-reading (the option is read only here; callers not traced for how they consume an `NA`)
- Suggested direction: validate the option exactly like `dta_stream_block_size()` does.

### F-c1-spec-05  [nit] [dead-code]
- Where: R/00_helpers.R:551-553; R/DTA-class.R:293-295; R/DTARule-class.R:78-80; R/DTAColumnSpecCollection-class.R:231-233, :757-759; R/DTAColumnSpecStructure-class.R:96-98, :106-108, :153-155; R/DTAColumnSpecStructureR-class.R:208-210; R/DTAColumnSpecStructureSAS-class.R:199-201, :270-272 (same pattern outside the slice: DTADataSet-class.R:217, :509; DTAFile-class.R:197, :222, :995, :1043; DTAFileTabular-class.R:1975; DTAMetaData-class.R:438, :596; DTADataSetTabular-class.R:415)
- What: Every `if (!exists("<generic>", mode = "function" …)) <generic> <- new_generic(…)` guard is decided at build time by the `Collate:` order, so each one is either always TRUE (first definition) or always FALSE (dead body); one dead body would be wrong if it ever ran.
- Evidence:
  ```r
  # DTAColumnSpecCollection-class.R:757  (as_json_schema already created by DTAColumnSpec-class.R:699)
  if (!exists("as_json_schema", mode = "function", inherits = FALSE)) {
    as_json_schema <- new_generic("as_json_schema", "DTAColumnSpecCollection")   # dispatch arg named after a class
  # DTAColumnSpecStructure-class.R:153 — no inherits = FALSE: base::print always exists
  if (!exists("print", mode = "function")) { print <- new_generic("print", "x") }
  ```
  `DESCRIPTION` collates `00_helpers.R` first, so `check` cannot exist at 00_helpers.R:551 (guard always TRUE) and always exists at DTARule-class.R:78 (body dead). The `print`/`names`/`labels` guards omit `inherits = FALSE` and therefore always find base's function — their bodies are unreachable, and `NAMESPACE` consequently `export()`s three names (`print`, `names`, `labels`) the namespace never defines (it works only because R falls back to the base binding).
- Failure scenario: none at runtime today; the cost is ~20 guards whose long justification comment (00_helpers.R:529-550) describes a decision that is never actually taken, plus a latent wrong generic signature that a re-ordering of `Collate:` would activate.
- Confidence: verified-by-reading (Collate order in DESCRIPTION:69-139 compared with every guard listed by grep; the NAMESPACE fallback for `export(print)` is inferred from R's `importIntoEnv` behaviour, not run)
- Suggested direction: define each generic once, unconditionally, in the first-collated file; delete the other guards.

### F-c1-spec-06  [nit] [doc-wrong]
- Where: R/00_helpers.R:529-531
- What: The comment justifying where `check` is defined states that "R files are collated alphabetically"; the package has an explicit, non-alphabetical `Collate:` field.
- Evidence:
  ```r
  # The generic is defined in this file, rather than in an individual class file,
  # because R files are collated alphabetically and several class files need it
  ```
  DESCRIPTION:69-139 lists e.g. `'DTAColumnSpecStructureSAS-class.R'` before `'DTAColumnSpecStructureR-class.R'` and `'importConversion.R'` before `'DTADataSet-class.R'` (roxygen `@include`-driven).
- Failure scenario: a maintainer relying on the comment adds a file expecting alphabetical order.
- Confidence: verified-by-reading
- Suggested direction: say "is collated first (see `Collate:`)".

### F-c1-spec-07  [minor] [doc-missing]
- Where: R/DTAtools-package.R:44-56, :65-82
- What: The package overview is stale: "Classes" says a dataset is *either* tabular *or* file and never mentions `DTADataSetMatrix` or `DTAContainer` (both exported), and "Options" lists 7 of the 19 `DTAtools.*` options the code reads.
- Evidence:
  ```r
  #' datasets. A dataset is either a \code{\link{DTADataSetTabular}}, ... or a
  #' \code{\link{DTADataSetFile}}, whose files are only checked for arrival.
  ```
  `__DTAtools_supported_dataset_types__ <- c("tabular", "file", "matrix")` (00_helpers.R:151). Options read by `getOption()` in R/ but absent from the section: `DTAtools.stream_batch_rows`, `DTAtools.temp_dir`, `DTAtools.progress_seconds`, `DTAtools.container_max_members`, `DTAtools.container_max_bytes`, `DTAtools.container_max_member_bytes`, `DTAtools.container_max_ratio`, `DTAtools.stream_arrow_numeric`, `DTAtools.stream_arrow_numeric_min_rows`, `DTAtools.stream_arrow_unique`, `DTAtools.transcode_block_bytes`, `DTAtools.qualification`.
- Failure scenario: a user reading `?DTAtools` cannot discover the matrix dataset type, the container model, or the archive-size limits that decide whether `load_delivery()` accepts a zip.
- Confidence: verified-by-reading (option list from a grep of `getOption("DTAtools.` over R/, each hit is a real read)
- Suggested direction: add the matrix/container classes and complete (or explicitly scope) the options table.

### F-c1-spec-08  [minor] [code-cheat]
- Where: R/zzz.R:1-5
- What: `utils::globalVariables()` blanket-silences R CMD check's undefined-variable NOTE for very generic names, at least one of which (`instancePath`) is used nowhere in the package.
- Evidence:
  ```r
  utils::globalVariables(c(
    "Format", "nullable", "description", "id",
    "instancePath", "keyword", "columnspec", "data", ".", ".n",
    "Name", "Role"
  ))
  ```
  A grep of R/ finds no occurrence of `instancePath` outside this file; `keyword`, `columnspec`, `nullable`, `Format` occur only as argument names or as names in `data.frame(...)`/`list(...)` calls (no NSE use). Only `Name`/`Role` (documentBuilders.R:568-569) and `.n` (streamingScale.R:431-438) are bare NSE symbols.
- Failure scenario: a typo that leaves `id`, `data` or `description` unbound in any function of the package produces no "no visible binding" NOTE, so the one static check that would catch it is switched off for the commonest variable names.
- Confidence: verified-by-reading for `instancePath`; inferred for the others (grepped for bare-symbol/dplyr-verb use, did not read every call site of `id`/`data`/`description`)
- Suggested direction: reduce the list to the symbols that are really used non-standardly (or use `.data$`), so the NOTE works again.

### F-c1-spec-09  [minor] [doc-wrong]
- Where: R/DTA-class.R:1474-1487
- What: `create_example_DTA()` documents a `NULL` return for `index = 2` that the code no longer produces.
- Evidence:
  ```r
  #' @param index Index of the example to create. Only \code{1} is currently
  #'   available; any other value is an error (or, for \code{2}, \code{NULL}).
  ...
  if (!identical(as.numeric(index), 1)) {
    cli_abort("No example found with index {index}.")
  ```
- Failure scenario: `create_example_DTA(2)` aborts; the documentation says it returns `NULL` (the behaviour of the `switch()` the body's own comment says was removed).
- Confidence: verified-by-reading
- Suggested direction: drop the parenthesis.

### F-c1-spec-10  [minor] [doc-missing]
- Where: R/DTA-class.R:865-921 vs :1010-1028
- What: The `check()` method for `DTA` takes a `validation_run` argument that its otherwise exhaustive argument list does not document.
- Evidence:
  ```r
  method(check, DTA) <- function(x, datasets = NULL, force = FALSE, persist = TRUE,
    artifact_dir = NULL, quiet = FALSE, validation_run = NULL, batch_rows = ..., ...
  ```
  The `\describe{}` under `@param ...` has items for datasets, force, persist, artifact_dir, quiet, batch_rows, max_errors, fail_fast, on_missing_column, use_threads and benchmark — not `validation_run`.
- Failure scenario: the id that groups one run's results (`validation_run` column of `validation_status()`) cannot be set by a reader of the docs, although the method accepts it.
- Confidence: verified-by-reading
- Suggested direction: document it (or make it internal with a leading dot like `.container_globs`).

### F-c1-spec-11  [minor] [bug]
- Where: R/DTA-class.R:46-65
- What: The `DTA()` constructor silently discards everything in `...` whenever `metadata` is supplied.
- Evidence:
  ```r
  constructor = function(datasets = NULL, metadata = NULL, containers = NULL, ...) {
    ...
    if (is.null(metadata)) {
      metadata <- DTAMetaData(...)
    }
  ```
  `...` is consumed only in that branch; nothing checks `...length() == 0` otherwise.
- Failure scenario: `DTA(datasets = ds, metadata = md, title = "Study 1")` or a misspelt `DTA(datasets = ds, metadata = md, contaners = cn)` returns an object without error — the title/containers are dropped and nobody is told.
- Confidence: verified-by-reading
- Suggested direction: abort when `metadata` is given and `...` is non-empty.

### F-c1-spec-12  [minor] [inconsistency]
- Where: R/DTA-class.R:59-61 vs :135-144
- What: An unnamed `datasets` list holding a non-dataset dies in the constructor with a raw `@` error, so the validator's classed `dta_invalid_dataset_entry` message is unreachable for exactly the case (an unnamed list) the constructor advertises support for.
- Evidence:
  ```r
  if (is.list(datasets) && !is.null(datasets) && is.null(names(datasets))) {
    names(datasets) <- vapply(datasets, function(x) x@id, character(1))
  }
  ```
  The `containers` branch right below guards the same step with `all(vapply(containers, inherits, logical(1), "DTAtools::DTAContainer"))`; this one does not (`!is.null(datasets)` is also redundant after line 52-54).
- Failure scenario: `DTA(datasets = list(list(id = "a")), metadata = md)` -> base error about `@` on a list (text depends on R version and locale) instead of "Every entry in @datasets must be a DTADataSet."
- Confidence: verified-by-reading
- Suggested direction: apply the same `inherits` guard before reading `@id` and let the validator report the entry.

### F-c1-spec-13  [minor] [bug]
- Where: R/DTA-class.R:1164, :1279 (with R/DTADataSet-class.R:645-658)
- What: The per-dataset and overall import-error totals are `sum()`s over an *integer* column, so they overflow to `NA` — the very failure the surrounding comments say the double/`dta_narrow_count()` design removed — and `na.rm = TRUE` then drops that `NA` from the grand total.
- Evidence:
  ```r
  n_import_errors <- sum(val_status$n_import_errors, na.rm = TRUE)          # :1164
  total_import_errors <- sum(summary_df$n_import_errors, na.rm = TRUE) +    # :1279
    n_metadata_import_errors
  ```
  `dta_validation_result_to_row()` narrows each count to integer whenever it fits, so two tables with e.g. 1.2e9 import errors each give an integer column; base `sum()` of integers past `.Machine$integer.max` returns `NA_integer_` with a warning. DTADataSet-class.R:646-648 itself notes that an `NA` "is exactly what the DTA-level sum(..., na.rm = TRUE) rollup silently drops, under-reporting the affected axis as 0".
- Failure scenario: one dataset with two tables of 1.2e9 unconvertible cells each -> `last_validation_summary$n_import_errors` is `NA` for that dataset and it contributes 0 to `total_import_errors`. The verdict itself survives only because a table with import errors is also `ok = FALSE` (`dta_details_ok()`), so `total_invalid > 0`.
- Confidence: verified-by-reading (types traced through `dta_narrow_count()` and `dta_validation_result_to_row()`; the streaming path's index entries were not read)
- Suggested direction: `sum(as.numeric(...))` then `dta_narrow_count()`, and drop `na.rm` where an `NA` must not be read as zero.

### F-c1-spec-14  [nit] [bug]
- Where: R/DTA-class.R:298-334, :376-391, :1058-1075
- What: Dataset selectors accept inputs they do not handle: non-whole numbers are truncated silently, `NA` indices raise a base error, and repeated selectors are processed twice.
- Evidence:
  ```r
  if (any(i < 1) || any(i > length(x@datasets))) { cli_abort("Numeric index out of bounds.") }
  return(x@datasets[i])                                   # `[`
  names(x@datasets)[datasets]                             # check(): no unique()
  ```
- Failure scenario: `datasets(dta, 1.9)` returns dataset 1; `dta[NA_integer_]` -> "missing value where TRUE/FALSE needed"; `check(dta, datasets = c(1, 1))` validates the dataset twice and `last_validation_summary` carries two rows for it, doubling every total derived from it.
- Confidence: verified-by-reading
- Suggested direction: require whole, non-missing, unique indices in one shared helper.

### F-c1-spec-15  [nit] [doc-wrong]
- Where: R/DTA-class.R:122-124; :1277-1278
- What: Two comments in `DTA-class.R` describe the code inaccurately.
- Evidence:
  ```r
  # ... see the same reasoning in print(DTA) and method(datasets, DTA) above.     (:124 — both are defined BELOW, at :1421 and :298)
  # An import error fails the run on its own axis, even when every table's `ok`
  # is TRUE.                                                                      (:1277)
  ```
  `dta_details_ok()` (validationFunctions.R:396-400) already folds `import_valid` into a table's `ok`, so a table with import errors never has `ok = TRUE`; only the metadata axis reaches `total_import_errors > 0` with `total_invalid == 0`.
- Failure scenario: misleads a maintainer about which branch of `dta_overall_summary_message()` table import errors take (they take "tables with validation errors", never "values could not be imported").
- Confidence: verified-by-reading for the in-memory path; inferred for streamed tables (their index entries were not read)
- Suggested direction: correct both comments.

## Area 2 — column specs: `R/DTAColumnSpec-class.R`, `R/DTAColumnSpecCell-class.R`, `R/DTAColumnSpecRowLabel-class.R`, `R/DTAColumnSpecStructure-class.R`, `R/DTAColumnSpecStructureSAS-class.R`, `R/DTAColumnSpecStructureR-class.R`, `R/columnSpecChecks.R` (all read completely)

### F-c1-spec-16  [major] [code-cheat]
- Where: R/DTAColumnSpecStructureSAS-class.R:202-213; R/columnSpecChecks.R:1396-1409, :617-632, :721-727, :905-910
- What: A column declared with a bare backend (`type: SAS`) builds and lints clean, and at check time ALL of its value checks (nullable, values, pattern, length, format) are skipped behind a single warning, so the table is reported valid.
- Evidence:
  ```r
  method(as_json_schema_type, DTAColumnSpecStructureSAS) <- function(x) { switch(x@type, ...   # x@type is NULL -> base error
  # columnSpecChecks.R:1396  "(as_json_schema_type() aborts for a bare backend) has no constraints to test against"
  schema <- tryCatch(Filter(Negate(is.null), list(type = as_json_schema_type(spec), ...)), error = function(e) NULL)
  if (is.null(schema)) { return(character(0)) }                    # validator: "no problems"
  # :617
  schema <- tryCatch(as_json_schema(columns[[i]]), error = function(e) { cli::cli_warn(...); NULL })
  ```
  `"SAS"` splits to prefix `SAS`, rest `NULL`; `DTAColumnSpecStructureSAS(type = NULL)` passes its validator; the `DTAColumnSpec` validator swallows the schema error and returns no problems; `dta_columnspec_errors()` then does `if (is.null(col_spec)) next`, and `dta_columnspec_declared_columns()` leaves the column out of every denominator. The warning class `dta_columnspec_schema_failed` is handled nowhere (grep of R/, tests/, inst/).
- Failure scenario: YAML column `{id: ARM, type: SAS, nullable: false, values: [A, B]}` -> `lint_dta()` returns no finding, `read_dta_from_yaml()` builds; `check()` on data whose ARM column is empty or holds `Z` reports the table valid, with one warning "Column "ARM" could not be compiled into a schema; its value checks are skipped." (The R sibling is quieter still: `type: R` is silently validated as a string column — DTAColumnSpecStructureR-class.R:171-173.)
- Confidence: verified-by-reading (every step traced; `switch(NULL, ...)` erroring is base R semantics, not run)
- Suggested direction: refuse a structure that has a backend but neither type nor format in the structure validators, and make a schema that cannot be compiled an error (or a failed/not_checked column), not a skip.

### F-c1-spec-17  [major] [code-cheat]
- Where: R/00_helpers.R:718-774; R/lintDTA.R:1007-1010
- What: A `type: file` dataset silently discards its `columns:` and `rules:` — neither the factory nor the lint mentions them — while the mirror-image misplacements (`columns` on a matrix, `row_labels`/`matrix` elsewhere) are hard errors.
- Evidence:
  ```r
  "file" = { return(DTADataSetFile(files = file_handlers, ...)) },   # `columns`, `rules` are named formals: never forwarded
  # lintDTA.R:1008
  if (!(type %in% c("tabular", "matrix"))) {
    return(rbind(out, dta_lint_matrix_keys(d, label, is_matrix = FALSE)))   # only flags row_labels / matrix
  }
  ```
  `dta_dataset_from_list()` ends in `do.call(DTADataSetFactory, x)` (DTADataSet-class.R:382), so the YAML keys bind to those formals and vanish.
- Failure scenario: a dataset whose `type` is (or was changed to) `file` but which still lists `columns:` with `nullable: false`/`values:` and `rules:` -> `lint_dta()` clean, the object has no specs, `check()` tests only that the file arrived, is readable and non-empty, and the run can print "Validation PASSED" although none of the declared constraints was evaluated.
- Confidence: verified-by-reading
- Suggested direction: abort (and lint) `columns`/`rules` on a file dataset the way `dta_matrix_columns_key` does.

### F-c1-spec-18  [major] [bug]
- Where: R/columnSpecChecks.R:474-493 (used at :299-308)
- What: The SAS `DATEw.` / `TIMEw.d` / `DATETIMEw.` check is a shape-only regex, so calendar-impossible dates and out-of-range times pass as "fits the format", while the R backend's sibling check rejects the same values.
- Evidence:
  ```r
  date_core <- paste0("(?:0[1-9]|[12][0-9]|3[01])-?(?:", `__DTAtools_sas_month_re__`, ")(?:-?(?:[0-9]{2}|[0-9]{4}))?")
  time_core <- paste0("[0-9]+:[0-9]{2}(?::[0-9]{2}", fraction, ")?")
  ... datetime = paste0(date_core, ":", time_core)
  ```
  The day is 01-31 whatever the month, minutes and seconds are any two digits, the uncapped-hours rule written for a TIME duration is reused for the time of day inside DATETIME, and the year is optional. `dta_r_temporal_bad()` (rTemporalFormat.R:270-321) checks days-in-month ("`2023-02-29` and `2024-04-31` fail") and the qualification suite asserts `"12:60:00" = FALSE` for the R backend; no test covers an impossible SAS value.
- Failure scenario: column `type: SAS Date, format: SAS DATE9.` -> `31FEB2025`, `30FEB2024`, `31APR2025` and the year-less `15AUG` are all valid; `SAS TIME8.` accepts `12:75:99`; `SAS DATETIME18.` accepts `01OCT25:45:99:99`.
- Confidence: verified-by-reading
- Suggested direction: capture day/month/year (and hh/mm/ss) and range-check them as the R backend does; require the year for widths that display one.

### F-c1-spec-19  [minor] [bug]
- Where: R/DTAColumnSpec-class.R:733-746, :748-755 (consumed at R/columnSpecChecks.R:215-225)
- What: Declared `values` that YAML read as numbers are turned into text with `as.character()` (scientific notation, 15 significant digits) instead of the package's own plain-decimal `dta_numeric_text()`, producing false `enum` violations on text and untyped columns; the lint has no finding for it and the one build-time warning skips untyped columns.
- Evidence:
  ```r
  "string" = { ... as.character(values_flat) },
  ...
  if ("string" %in% schema$type) { ... values <- c(values, NA, "") }     # untyped + nullable: numbers coerced to character here
  # columnSpecChecks.R:219
  bad <- which(!(x %in% allowed_values))
  ```
  `as.character(1e-04)` is `"1e-04"`, `as.character(1e5)` is `"1e+05"`. For rule operands the lint does warn ("a number was written where the column holds text", lintDTA.R:471-479); for a column's own `values` there is no lint finding. `specs_from_list()` does `cli_warn()` via `dta_warn_numeric_values_on_text_column()` (DTAColumnSpecCollection-class.R:454-485), but only for a typed text column, only on that entry point (not `DTAColumnSpec()` nor `lint_dta()`), and its text blames YAML's dropped zeros (`1.10`) — the exponent form is added by this package, since `0.0001` has an exact plain rendering.
- Failure scenario: `{id: CONC, type: SAS Char, values: [0.0001, 0.001]}` -> the enum is `c("1e-04", "0.001")`, so every row holding `0.0001` is reported "must be equal to one of the allowed values". Same for an untyped column with `nullable: true, values: [100000.0]` (enum `"1e+05"` vs data text `100000`).
- Confidence: verified-by-reading (R's `as.character()` rendering of doubles is base semantics)
- Suggested direction: render numeric values with `dta_numeric_text()`, and lint/refuse unquoted numbers in `values` of a text column.

### F-c1-spec-20  [minor] [bug]
- Where: R/DTAColumnSpec-class.R:26-38, :246-253, :766-770; R/columnSpecChecks.R:1417-1420
- What: A zero-length `values`/`examples` is normalised to `NULL` only when it is a list; `character(0)` survives as "values provided" and then breaks or inverts the check.
- Evidence:
  ```r
  } else if (length(values) == 0) { return(NULL) }        # reached only for is.list(values)
  ...
  if (length(values) > 1) { schema$enum <- values } else { schema$const <- values[[1]] }   # character(0)[[1]]
  ```
- Failure scenario: `DTAColumnSpec(id = "A", type = "SAS Char", values = character(0))` is valid (`dta_spec_value_problems()` skips an empty vector). `as_json_schema()` then throws "subscript out of bounds", which F-c1-spec-16's `tryCatch` turns into "all value checks skipped"; with `nullable = TRUE` the enum becomes `c(NA, "")` and every non-empty value is a violation; and adding a `pattern` is refused with "'pattern' cannot be set if 'values' are provided".
- Confidence: verified-by-reading
- Suggested direction: normalise any zero-length input to `NULL` in `dta_normalise_spec_values()`.

### F-c1-spec-21  [minor] [bug]
- Where: R/DTAColumnSpecStructure-class.R:32-43; R/DTAColumnSpecStructureSAS-class.R:147-172; R/DTAColumnSpecStructureR-class.R:141-146; R/columnSpecChecks.R:182-183
- What: No structure validator looks at the value of `length`, so negative, zero, fractional, `NA` and multi-element lengths are accepted, and the evaluator then mis-reports, silently skips or crashes.
- Evidence:
  ```r
  length = class_numeric_or_null,                                   # only the type is constrained
  if (!is.null(self@length) && !is.null(type) && !identical(type, "character")) { ... }   # R: only *where* it is allowed
  # columnSpecChecks.R:183
  if (!is.null(max_length) && !is.na(max_length) && base_type == "string") {
  ```
- Failure scenario: `length: -5` -> every non-missing value is reported "must NOT have more than -5 characters"; `length: 2.5` accepted; `length: [5, 10]` -> `!is.na(max_length)` has length 2 and `check()` dies with a base error; an `NA` length is accepted and the length check is skipped without a word.
- Confidence: verified-by-reading
- Suggested direction: require a single positive whole number in the base structure validator.

### F-c1-spec-22  [minor] [bug]
- Where: R/DTAColumnSpecStructureSAS-class.R:147-172 (compare R/DTAColumnSpecStructureR-class.R:95-149)
- What: The SAS structure validator computes its backend message and throws it away, never pins the backend to `"SAS"`, and never checks that `type`/`format` are single strings — all three of which its R sibling does.
- Evidence:
  ```r
  if (is.null(self@backend) || self@backend == "") {
    "'backend' must be defined and cannot be an empty character."      # no return(): value discarded
  }
  supported_types <- unname(`__dta_sas_type_aliases__`)
  if (!is.null(self@type) && !(self@type %in% supported_types)) {
  ```
  The R validator has `return(...)` there, adds `if (!identical(self@backend, "R")) return(...)` with the comment "with another backend its stored strptime format would never be emitted or checked, silently", and tests `length(type) != 1L || is.na(type)`.
- Failure scenario: `s <- DTAColumnSpecStructureSAS(type = "Char", format = "$10."); s@backend <- "R"` is accepted; `dta_columnspec_emitted_format()` then takes its R branch and returns `NULL`, so `$10.` is never checked, and `dta_type_family(s)` is `NA`, so the lint treats the column as untyped. `DTAColumnSpecStructureSAS(type = c("Char", "Num"))` dies in `if (type_key %in% ...)` with a base "condition has length > 1" error.
- Confidence: verified-by-reading
- Suggested direction: mirror the R validator (return the message, pin the backend, check scalars, end in `NULL`).

### F-c1-spec-23  [minor] [bug]
- Where: R/DTAColumnSpecStructure-class.R:18-43
- What: The exported base class has a constructor default that can never construct, a validator branch that can never run, and a validator that crashes instead of reporting.
- Evidence:
  ```r
  constructor = function(type = NULL, format = NULL, length = NULL, backend = NULL) { new_object(S7_object(), ..., backend = backend) },
  properties = list(..., backend = class_character),
  validator = function(self) {
    if (is.null(self@backend) || self@backend == "") { "'backend' must be defined and cannot be an empty character." }
  ```
  `backend = NULL` fails S7's property type check before the validator runs, so `is.null(self@backend)` is dead; `backend = NA_character_` makes the condition `NA`.
- Failure scenario: `DTAColumnSpecStructure()` -> S7 "@backend must be <character>, not <NULL>"; `DTAColumnSpecStructure(type = "Char", backend = NA_character_)` -> base "missing value where TRUE/FALSE needed"; `DTAColumnSpecStructure(type = "Banana", backend = "zzz")` is valid and can be assigned to `spec@structure` (the property is typed on the base class), after which `as_json_schema_type()` aborts "not implemented at this level" and the column lands in F-c1-spec-16's skip.
- Confidence: verified-by-reading
- Suggested direction: make the base class abstract (or require `backend %in% __DTAtools_supported_backends__`) and return a message for every bad shape.

### F-c1-spec-24  [minor] [code-cheat]
- Where: R/DTAColumnSpec-class.R:199-209, :271, :299-316, :773; R/columnSpecChecks.R:243
- What: `is.na()` guards turn an `NA` pattern into "no pattern" at all three layers, and the other text properties are not checked for being one string.
- Evidence:
  ```r
  if (!is.na(self@pattern)) { compiles <- tryCatch(...) }                    # validator: NA skips the compile test
  if (!is.null(x@pattern) && !is.na(x@pattern)) { schema$pattern <- ... }      # schema: NA dropped
  if (!is.null(pattern) && !is.na(pattern) && base_type %in% ...) {            # evaluator: NA skipped
  if (!(self@colclass %in% valid_colclasses)) {                                # vector colclass -> base error
  ```
  The same validator refuses a pattern on an R logical column precisely because "it would be accepted and then never enforced".
- Failure scenario: `DTAColumnSpec(id = "A", type = "SAS Char", pattern = NA_character_)` is valid and no pattern is ever applied; `pattern = c("^a", "^b")` and `colclass = c("patient_info", "measurement")` die with base "condition has length > 1"; `label = c("x", "y")`, `description = character(0)` are accepted.
- Confidence: verified-by-reading
- Suggested direction: validate `label`, `description`, `pattern`, `colclass` as single non-missing strings (as `DTADataSet` does for `name`/`description`).

### F-c1-spec-25  [minor] [logic-error]
- Where: R/columnSpecChecks.R:923, :1182; R/DTAColumnSpec-class.R:611-631
- What: Every compiled column counts as "declaring" `type`, because `as_json_schema()` always emits one, so the per-check report certifies "Format check passed (n columns)" for columns that declare neither type nor format — the hollow certificate the surrounding comments say the report exists to stop.
- Evidence:
  ```r
  out$type <- declares(function(s) !is.null(s$type))
  format = "no column spec declares a type or format",        # reason text that can only appear when no column compiled at all
  ```
  For an untyped column `as_json_schema_type()` returns all four base types (plus `"null"` when nullable), which "is a no-op constraint for 'type'" by its own roxygen.
- Failure scenario: three columns given only `id` and `nullable: true` -> the console prints "Format check passed (3 columns)" and "All column spec checks passed", though no check could have failed.
- Confidence: verified-by-reading
- Suggested direction: count a column under `type` only when its structure declares one (keep the null check under its own "missing values" label).

### F-c1-spec-26  [minor] [doc-missing]
- Where: R/columnSpecChecks.R:1033-1048 (man/dta_columnspec_check_summary.Rd)
- What: `dta_columnspec_check_summary()` has five formals and documents three; `matrix` and `structure_declared` have no `@param`.
- Evidence:
  ```r
  dta_columnspec_check_summary <- function(schemas, tally = NULL, settled = NULL, matrix = FALSE, structure_declared = NULL) {
  ```
  The generated Rd's `\usage` lists all five and its `\arguments` only `schemas`, `tally`, `settled`.
- Failure scenario: `R CMD check` reports "Undocumented arguments in Rd file" for this topic (a WARNING in the codoc pass), and a reader cannot tell what `structure_declared` must contain.
- Confidence: verified-by-reading (Rd compared with the signature; the R CMD check consequence is inferred, not run)
- Suggested direction: add the two `@param` entries.

### F-c1-spec-27  [nit] [dead-code]
- Where: R/DTAColumnSpec-class.R:257-259, :341, :365, :460; R/DTAColumnSpecStructureSAS-class.R:178-190, :210, :262; R/DTAColumnSpecStructureR-class.R:155-167
- What: Unreachable or duplicated code in the column-spec classes.
- Evidence:
  ```r
  if (!is.null(self@values)) { cli_abort("{self@id}: 'values' cannot be set if pattern is provided.") }   # :257 — the block at :246-249 already aborted
  "Bool" = "bool",  /  "Bool" = "boolean",  /  "Bool" = "logical",      # `Bool` is not in __dta_sas_type_aliases__
  method(as.list, DTAColumnSpecStructureSAS) <- function(x, ...) { ...  # byte-identical to the base method (and to the R one)
  ```
  `@importFrom glue glue` on `get_arrow_type()` imports a function the body does not use; `if (!is.null(x@id))` in `print()` tests a property that cannot be `NULL`.
- Failure scenario: none; maintenance noise (three copies of `as.list` to keep in step).
- Confidence: verified-by-reading
- Suggested direction: delete the dead branches and the two duplicate methods.

### F-c1-spec-28  [minor] [doc-wrong]
- Where: R/DTAColumnSpec-class.R:501-525
- What: The shared `as.list` help page lists the classes it covers from before the R backend and the matrix classes existed, and claims a completeness it does not have.
- Evidence:
  ```r
  #' A method is defined for every class that appears in a saved specification.
  #' @param x A DTAtools object: a DTAColumnSpec, DTAColumnSpecCollection,
  #'   DTAColumnSpecStructure (or its SAS subclass), DTAMetaData, or any DTARule subclass.
  ```
  Methods also exist for `DTAColumnSpecStructureR`, `DTAMatrixSpec`, `DTAColumnSpecCell` and `DTAColumnSpecRowLabel`; none exists for `DTAFile*`, `DTADataSet*` or `DTAContainer`, all of which appear in a saved specification (`files:`, `datasets:`, `containers:`).
- Failure scenario: `as.list(DTAContainer(...))` / `as.list(create_example_DTAFileCSV())` do not behave as the page promises.
- Confidence: verified-by-reading (grep of every `method(as.list, ...)` in R/)
- Suggested direction: enumerate the real method set and scope the sentence to column specifications.

### F-c1-spec-29  [nit] [doc-wrong]
- Where: R/DTAColumnSpec-class.R:133-142, :380-388
- What: Several roxygen statements on `DTAColumnSpec` and its example builder are inaccurate or leave out the behaviour that matters.
- Evidence:
  ```r
  #' S7 method to create and return an example DTAColumnSpec object.      # create_example_DTAColumnSpec is a plain function
  #' @param index Numeric. Selector for different example objects.        # valid values 1..5 not stated
  #' @param nullable Logical or NULL. Whether the column can be null.     # NULL means "missing values fail" (as_json_schema_type, :626)
  #' @param pattern ... a Perl-compatible regular expression.             # matched unanchored with grepl(): a partial match passes
  ```
- Failure scenario: a reader assumes an unset `nullable` permits missing values, or that `pattern: "[0-9]{3}"` rejects `abc1234`.
- Confidence: verified-by-reading
- Suggested direction: state the default-nullable rule, the search (not full-match) semantics and the index range.

### F-c1-spec-30  [nit] [code-cheat]
- Where: R/DTAColumnSpec-class.R:105-110
- What: The column-id whitespace test swallows the error an unscannable string raises and answers "no forbidden whitespace".
- Evidence:
  ```r
  other_whitespace <- tryCatch(
    grepl("\\s", gsub(" ", "", id, fixed = TRUE)),
    error = function(e) FALSE
  )
  ```
- Failure scenario: an id holding bytes that are invalid in the session locale plus a tab is accepted, although a tab is exactly what the check exists to refuse (the comment keeps this "as it always was").
- Confidence: verified-by-reading (that `gsub()` errors on such input is inferred from R's documented behaviour, not run)
- Suggested direction: refuse an id that is not valid in its declared encoding.

### F-c1-spec-31  [major] [design]
- Where: R/DTAColumnSpecStructureSAS-class.R:138-145; R/DTAColumnSpecStructureR-class.R:86-93; R/DTARuleColRange-class.R:64; R/DTARuleColUnique-class.R:52 (and six constructors outside the slice); DESCRIPTION:43-45; .github/workflows/*.y*ml
- What: Ten constructors call `new_object(.parent = ...)` by name, every CI job installs exactly `cran::S7@0.2.2`, and `DESCRIPTION` admits any later S7 — so the package is never built or tested against the S7 a user's `install.packages()` resolves.
- Evidence:
  ```r
  new_object(.parent = DTAColumnSpecStructure(type = type, format = format, length = length, backend = "SAS"))
  # DESCRIPTION
  S7 (>= 0.2.2)
  # R-CMD-check.yaml:135, r-style.yaml:67, qualification.yml:94, release.yml:33, test-coverage.yaml:79,166
  extra-packages: ..., cran::S7@0.2.2
  ```
- Failure scenario: an S7 release that changes `new_object()`'s first formal breaks every one of these constructors at run time on a fresh install, while all CI legs stay green on the pinned version.
- Confidence: inferred (the pin, the unbounded dependency and the ten named-argument call sites were read; no newer S7 source is in `$SNAP`, so the actual incompatibility was NOT verified)
- Suggested direction: pass the parent positionally (as `DTA`, `DTADataSet`, `DTAColumnSpecCell` already do) and add one CI leg on current CRAN S7.

## Area 3 — `R/lintDTA.R` (read completely)

### F-c1-spec-32  [minor] [bug]
- Where: R/lintDTA.R:1231-1309, :747-768, :972-1037 (with R/00_helpers.R:834-872, :917-923)
- What: `lint_dta()` promises "every problem of a DTA document in one pass", but it never builds a file entry and never looks at unknown dataset keys, so a document can lint clean and then abort in a constructor — for a missing file `type` with base R's own, locale-translated "argument is missing" error.
- Evidence:
  ```r
  if (is.character(type) && length(type) == 1L && type %in% names(constructors)) { unknown <- ... }   # else: nothing is reported
  # dta_lint_skeleton_slots()
  tryCatch({ ... handlers <- dta_file_handlers_from_list(datasets[[i]][["files"]]) ... }, error = function(e) NULL)
  # 00_helpers.R:917
  DTAFileFactory <- function(type, ...) { if (is.null(type) || type == "") {
  ```
  The only place lint builds handlers swallows the failure and merely skips the file-name-conflict pass. `dta_file_handlers_from_list()`'s own message "must be a named list with at least a 'type' and a 'filename'" checks neither key.
- Failure scenario: `files: {filename: a.csv}` (no `type`) -> `lint_dta()` returns zero rows; `read_dta_from_yaml()` then fails with `argument "type" is missing, with no default` (in the session language). A misspelt dataset key such as `colums:` is likewise absent from the findings and surfaces as "unused argument (colums = list(...))" from `DTADataSetTabular()`.
- Confidence: verified-by-reading
- Suggested direction: build each file entry inside a `tryCatch` as columns and rules are, and report unknown dataset keys by name.

### F-c1-spec-33  [minor] [bug]
- Where: R/lintDTA.R:981-993, :1149, :1186-1189
- What: The dataset `type` test in the lint is not vector-safe, and two row-label id reads index a possibly empty vector, so odd shapes make `lint_dta()` itself abort with a base error instead of returning a finding.
- Evidence:
  ```r
  } else if (!type %in% `__DTAtools_supported_dataset_types__`) {
    ... sprintf("Dataset type '%s' is not supported.", paste(type, collapse = ","))
  ```
  The `paste(collapse = ",")` shows a vector was expected; `if()` on a length-2 logical is an error since R 4.2. In the matrix pass, `as.character(labels[[i]][["id"]] %||% paste0("#", i))[[1L]]` (and the same shape at :1188) takes `[[1L]]` of `character(0)` when the id is an empty sequence, because `%||%` only replaces `NULL`.
- Failure scenario: `type: [tabular, file]` -> `lint_dta()` stops with "the condition has length > 1" (translated), reporting none of the document's other problems; a matrix row label with `id: []` -> "subscript out of bounds".
- Confidence: verified-by-reading
- Suggested direction: test `length(type) != 1L || !type %in% ...`.

### F-c1-spec-34  [minor] [bug]
- Where: R/DTA-class.R:1549-1598 vs R/lintDTA.R:688-705
- What: The lint reads the top-level sections with exact `[[` while the builder reads them with `$`, which partial-matches, so the two disagree about what a document contains.
- Evidence:
  ```r
  if (is.null(x$datasets)) { ... }          # DTA-class.R:1549 — also x$metadata, x$containers
  datasets <- x[["datasets"]]               # lintDTA.R:705
  ```
  lintDTA.R:77 itself warns: "Exact `[[` access: `$` would partial-match".
- Failure scenario: a document with `metadata:` and a parked `datasets_old:` (no `datasets:`) -> lint emits only the warning "Unknown top-level key 'datasets_old'" and lints no dataset; `dta_from_list_build()` then resolves `x$datasets` to `datasets_old` and builds those datasets into the DTA.
- Confidence: verified-by-reading (`$` on a list partial-matches a unique prefix: base R semantics)
- Suggested direction: use `[[` in `dta_from_list_build()`.

### F-c1-spec-35  [minor] [dead-code]
- Where: R/lintDTA.R:1330-1340; R/DTAColumnSpecCollection-class.R:430-432; R/DTADataSet-class.R:382
- What: Column mappings (and dataset mappings) are deliberately passed to `do.call()` unchecked so that R's partial argument matching keeps accepting misspelt keys from old files — a backwards-compatibility allowance — and a genuinely unknown column key is reported in R's translated wording. Rules are the exception: `DTARuleFactory()` checks its argument names exactly.
- Evidence:
  ```r
  # ... do.call() would report it with R's own, translated "unused argument" message.
  # (Other keys are left to do.call(): it matches them partially, and `example:` for
  # `examples:` is in files that exist.)
  do.call(DTAColumnSpec, raw)
  ```
  Metadata, containers, file entries and rules, by contrast, have their keys checked by name (`metadata_invalid`, `container_invalid`, `file_field_unknown`, "Unknown argument(s) for a … rule").
- Failure scenario: in a column, `val: [A, B]`, `desc: ...`, `nul: true` silently become `values`, `description`, `nullable`; in a dataset, `column:`/`rule:`/`file:` silently become `columns`/`rules`/`files`; `unit: mg` yields a `column_spec_invalid` finding whose text is "unused argument (unit = "mg")" in the session language, which no test can match (repo rule: never assert on translated text).
- Confidence: verified-by-reading
- Suggested direction: check column and dataset keys against `names(formals(...))` exactly, as the other sections do; drop the `example:` allowance.

## Area 4 — `R/DTAColumnSpecCollection-class.R`, `R/DTAContainer-class.R`, `R/DTAMatrixSpec-class.R` (all read completely)

### F-c1-spec-36  [major] [bug]
- Where: R/DTAColumnSpecCollection-class.R:777-781, :873-876, :909-914
- What: Both spec writers and the JSON-Schema serialiser hand numbers to their library with its default precision — `jsonlite` rounds to 4 decimal places, `yaml` to 7 significant digits — so a specification's numeric permitted values and rule bounds are silently changed on write, and the advertised write -> read round trip returns a different specification.
- Evidence:
  ```r
  json_schema <- jsonlite::toJSON(schema, auto_unbox = TRUE, na = "null")                 # as_json_schema(): no digits =
  yaml::write_yaml(as.list(columns), file)                                               # write_columns_to_yaml(): no precision =
  jsonlite::write_json(drop_unset(as.list(columns)), path = file, pretty = pretty, auto_unbox = TRUE)   # no digits =
  ```
  A grep of R/ and the app finds no `precision =` or `digits =` on any `as.yaml()`/`write_yaml()`/`toJSON()`/`write_json()` call in these writers. `as.list()` of a rule carries its `range` bounds, of a column its numeric `values`.
- Failure scenario: a collection with a range rule `min = 0.123456, max = 37.12345` or a column with `values = c(37.12345)` -> `write_columns_to_json()` writes `0.1235` and `37.1235`; `import_specs_from_json()` reads those back, so the re-imported rule has other bounds than the original and a data value `37.12345` no longer equals the permitted value. `write_columns_to_yaml()` turns `1234567.89` into `1234568`. `as_json_schema()` publishes the rounded numbers as the "contract for other tools to consume".
- Confidence: inferred — the call sites (no precision argument) were read; `jsonlite::toJSON(digits = 4)` and `yaml::as.yaml(precision = getOption("digits"))` are those packages' documented defaults, whose sources are not in `$SNAP`, and nothing was run
- Suggested direction: pass `digits = NA` (jsonlite) and `precision = 15`/17 (yaml), and add a numeric round-trip test.

### F-c1-spec-37  [minor] [bug]
- Where: R/DTAColumnSpecCollection-class.R:202-216; R/DTARule-class.R:63-67; R/DTARuleColRange-class.R:122-129; R/DTARuleColUnique-class.R:84-97; R/DTARuleGroupCondition-class.R:278-285
- What: `print()` for a `DTAColumnSpecCollection`, `DTARule`, `DTARuleColRange`, `DTARuleColUnique` and `DTARuleGroupCondition` does not return its argument, unlike the other eleven print methods of this slice and unlike the shared help page ("@return Invisibly, x").
- Evidence:
  ```r
  if (!is.null(x@rules) && length(x@rules) > 0) {
    cli::cli_alert_info("rules ({length(x@rules)}): {rule_preview}")
  } else {
    cli::cli_alert("rules: {cli::symbol$cross}")
  }
  }                                   # no invisible(x)
  ```
- Failure scenario: `y <- print(specs)` leaves `y` as `NULL`; `specs |> print() |> rules()` fails.
- Confidence: verified-by-reading
- Suggested direction: end with `invisible(x)`.

### F-c1-spec-38  [minor] [doc-wrong]
- Where: R/DTAColumnSpecCollection-class.R:538-582, :706; R/exportFunctions.R:83-116
- What: `columns_specs_from_word()` promises a lossless Word round trip and describes its marker stripping wrongly.
- Evidence:
  ```r
  #' ... so a specification exported to Word, reviewed there, and read back arrives at the same columns.
  #' ... Both markers, and everything after them, are removed from the stored description.
  description <- trimws(gsub("[\r\n]*#@(values|pattern):[^\r\n]*", "", description))
  ```
  `export_specs_table()` writes id, label, type, format, length, nullable and the description with `#@values`/`#@pattern` only: `examples` and `colclass` are never written, numeric `values` come back as text, and values are joined with `"; "` and split on `value_sep`, so a value containing `;` splits. The `gsub()` removes each marker *line*; text on later lines is kept, not "everything after them".
- Failure scenario: a column with `examples = "V01"` or `colclass = "visit_related"` loses both in the round trip the help text calls identity.
- Confidence: verified-by-reading
- Suggested direction: state exactly which properties travel, and fix the marker sentence.

### F-c1-spec-39  [minor] [code-cheat]
- Where: R/DTAColumnSpecCollection-class.R:689-691, :736-737
- What: The Word reader silently drops any table row whose id cell is blank and silently reads every unrecognised Nullable cell as `FALSE`.
- Evidence:
  ```r
  keep <- which(!blank(ids))
  nullable = if (is.null(nullable_cell)) NULL else grepl("^(yes|y|true)([^a-z]|$)", tolower(nullable_cell)),
  ```
  The Length cell, by contrast, aborts on unreadable input (`dta_word_import_error`).
- Failure scenario: a reviewer clears a variable name by accident -> that column specification disappears from the imported collection with no message; a Nullable cell of `T`, `1` or `Ja` becomes `nullable = FALSE` although the package's own logical tokens include `T`.
- Confidence: verified-by-reading
- Suggested direction: abort (or warn) on a non-empty row without an id and on a Nullable cell that is neither a yes- nor a no-token.

### F-c1-spec-40  [nit] [doc-wrong]
- Where: R/DTAColumnSpecCollection-class.R:8, :228-236, :376-388, :806-810; R/DTA-class.R:1376-1381 (with R/DTAContainer-class.R)
- What: Assorted roxygen inaccuracies around the collection and the shared print page.
- Evidence:
  ```r
  #' @usage names(x, ...)                     # base names() is a one-argument primitive; the method is function(x)
  #' S7 method to create and return an example DTAColumnSpecCollection object.   # a plain function; valid `index` (1, 2) not stated
  #' Supports both named and unnamed lists of column specifications.            # a named list works only when its names equal the ids
  #' ... A method is defined for every class the package exports ...            # print: no method(print, DTAContainer) exists
  ```
- Failure scenario: `names(specs, 1)` errors although the usage shows `...`; `print(DTAContainer("a", "a.zip"))` falls back to S7's default dump, not the "readable summary" the page promises.
- Confidence: verified-by-reading (grep of every `method(print, ...)` in R/)
- Suggested direction: correct the four statements (or add the missing print method).

### F-c1-spec-41  [nit] [inconsistency]
- Where: R/DTAColumnSpecCollection-class.R:352-374, :529-535
- What: The three spec readers disagree on pre-flight checks: only the YAML one lints, none checks that the file exists, and the YAML one reads its sections with partial-matching `$`.
- Evidence:
  ```r
  yaml <- yaml::read_yaml(file)
  if (is.null(yaml$columns)) { ... }
  dta_lint_enforce(lint_dta(list(columns = yaml$columns, rules = yaml$rules)))
  ...
  specs_from_list(json$columns, json$rules)          # import_specs_from_json(): no lint
  ```
- Failure scenario: a YAML file whose only section is `columns_draft:` is read as if it were `columns:`; a missing path surfaces as a base "cannot open file" error rather than the `cli` message `read_dta_from_yaml()` gives.
- Confidence: verified-by-reading
- Suggested direction: share one reader front end (`file.exists`, exact `[[`, lint).

## Area 5 — `R/DTAMetaData-class.R`, `R/DTAMetaData-helpers.R` (both read completely)

### F-c1-spec-42  [minor] [code-cheat]
- Where: R/DTAMetaData-class.R:169-184, :397-415; R/DTAMetaData-helpers.R:357-362; R/documentData.R:462; R/lintDTA.R:646-655
- What: `version_history` records are outside the "nothing is silently discarded" machinery the class documents for its dates: a record's `date` and `version` may be anything, a bad date is coerced with `suppressWarnings()` to `NA`, a truncated date or a day-count, and no import issue is ever recorded.
- Evidence:
  ```r
  required_fields <- c("version", "date", "changes")           # validator: only the KEYS must exist (changes alone is type-checked)
  if (is.numeric(value)) { return(as.Date(value, origin = "1970-01-01")) }
  if (is.character(value)) { return(suppressWarnings(as.Date(value, format = "%Y-%m-%d"))) }
  Version = if (is.null(vh$version)) "" else as.character(vh$version)[[1]],     # documentData.R
  ```
  The class roxygen calls `as.Date()` dropping trailing words "the dangerous case" and records an import issue for it — but only for `date` and the two transmission dates. The lint's `version_unquoted` warning likewise inspects `metadata.version` only.
- Failure scenario: a history record `{version: 2.0, date: "2026-12-31 (pending approval)", changes: x}` -> `get_version_history_df()` returns version `"2"` and date `2026-12-31` with the qualification gone; `date: 17.12.2024` gives `NA` ("N/A" in `print()`); `date: 20241217` becomes a Date 55 000 years ahead; the exported history table prints version `2` for the unquoted `2.0`. `metadata_import_errors()` is empty and `check()` reports the metadata as clean in every case.
- Confidence: verified-by-reading
- Suggested direction: parse history dates with `.parse_metadata_date()` (recording issues) and require `version` to be a string, linting an unquoted one.

### F-c1-spec-43  [minor] [bug]
- Where: R/DTAMetaData-class.R:254-273, :330-346
- What: A transmission date that is ISO-shaped but not a real calendar date is classified as a legitimate free-text phrase and kept without an import issue.
- Evidence:
  ```r
  if (is.na(parsed)) {
    # Digit-shaped but not a real calendar date (e.g. "2026-02-30 foo").
    return(no_date)
  }
  ...
  # A phrase with no parseable date prefix (...). Documented, legitimate input
  # for a free-text field: keep it verbatim and do NOT report an import issue.
  return(none)
  ```
- Failure scenario: `transmission: {date_last_transfer: "2026-02-30"}` (or `2026-13-01`) is stored as the string `"2026-02-30"`, `metadata_import_errors()` is empty and `check()` passes; the same typo in the top-level `date` is an import error.
- Confidence: verified-by-reading
- Suggested direction: treat a `\d{4}-\d{2}-\d{2}` prefix that is not a date as `not_convertible`, not as a phrase.

### F-c1-spec-44  [minor] [bug]
- Where: R/DTAMetaData-class.R:139-167
- What: The validator compares `version`/`title` with `==` unguarded and checks nothing about `header`, `error_handling` or `date`, so non-scalar or `NA` values either crash it with a base error or are accepted.
- Evidence:
  ```r
  if (!is.null(self@version) && self@version == "") { errors <- c(errors, "'version' cannot be an empty string.") }
  if (!is.null(self@title) && self@title == "") { ...
  ```
  `class_character_or_null` admits `NA_character_`, `character(0)` and vectors; `NA == ""` is `NA`, and a length-2 right-hand side of `&&` is an error.
- Failure scenario: `DTAMetaData(title = NA_character_)` -> "missing value where TRUE/FALSE needed"; YAML `version: ["1.0", "2.0"]` -> a base coercion error that the lint relays verbatim (translated) as `metadata_invalid`; `header = c("a", "b")`, `date = as.Date(c("2026-01-01", "2026-02-02"))` and `date = as.Date(NA)` are accepted.
- Confidence: verified-by-reading
- Suggested direction: validate each scalar property with `.dta_metadata_is_text()` (already used for `study`/`sections`) and require a length-1 date.

### F-c1-spec-45  [minor] [bug]
- Where: R/DTAMetaData-helpers.R:118-123 (with :91-102 and R/DTAMetaData-class.R:176-184)
- What: `as.list()` deletes the `date` key of a version-history record whose date is an `NA` Date, producing a list (and YAML) that `DTAMetaData()` then refuses to read.
- Evidence:
  ```r
  if (is.list(record) && !is.null(record$date)) {
    record$date <- .date_to_iso(record$date)       # .date_to_iso() returns NULL for a length-1 NA Date -> the element is removed
  }
  ```
  The validator requires every record to carry `version`, `date` and `changes`.
- Failure scenario: `md <- DTAMetaData(title = "t", version = "1", version_history = list(list(version = "1", date = as.Date(NA), changes = "x")))` is valid; `do.call(DTAMetaData, as.list(md))` fails with "version_history[[1]] missing required fields: date" — the package cannot read back what it wrote.
- Confidence: verified-by-reading
- Suggested direction: write an explicit `NULL`-valued entry (`record["date"] <- list(NULL)`) or refuse an NA history date in the validator.

### F-c1-spec-46  [minor] [code-cheat]
- Where: R/DTAMetaData-helpers.R:239-263
- What: `get_receiver_reviewers()` wraps its whole body in a `tryCatch` that rewrites every internal failure into "x must be a DTAMetaData object…", and its `sapply()` returns a list instead of a character vector when nobody is a reviewer.
- Evidence:
  ```r
  reviewers <- Filter(function(c) isTRUE(c$reviewer), contacts)
  if (name_only) { sapply(reviewers, function(c) c$name %||% NA_character_) } else { reviewers }
  },
  error = function(e) { cli::cli_abort("x must be a DTAMetaData object with receiver and contacts") }
  ```
  `get_version_history_df()` (same file, :317-320) documents having removed exactly this pattern because the message "was flatly untrue for a well-formed object".
- Failure scenario: a valid object whose `receiver$contacts` holds a plain string (a shape `.format_contact()` explicitly supports) -> `c$reviewer` fails on an atomic vector and the user is told `x` is not a DTAMetaData object. With contacts but no reviewer the function returns `list()`, contradicting "@return A character vector of reviewer names".
- Confidence: verified-by-reading
- Suggested direction: guard only the property access; use `vapply(..., character(1))`.

### F-c1-spec-47  [minor] [design]
- Where: R/DTAMetaData-helpers.R:383-423; R/DTAMetaData-class.R:72-74
- What: The only consistency check on the metadata — `validate_transmission_dates()` (first transfer after last, empty phrase) — is exported but called by nothing: not by `check()`, not by `lint_dta()`, not by the app.
- Evidence:
  ```r
  if (first > last) {
    messages <- c(messages, "date_first_transfer cannot be after date_last_transfer")
    is_valid <- FALSE
  }
  ```
  A grep of R/ and inst/shiny/dta_app finds the name only in its definition and in the class roxygen, which presents it as a consumer of the converted dates.
- Failure scenario: a DTA with `date_first_transfer: 2026-12-31` and `date_last_transfer: 2026-01-01` lints clean and `check()` reports `last_metadata_summary$ok = TRUE`.
- Confidence: verified-by-reading (callers searched by grep in R/ and the app only; tests not counted)
- Suggested direction: fold the result into the metadata axis of `check()` and into the lint, or say in the docs that it is opt-in.

### F-c1-spec-48  [nit] [doc-missing]
- Where: R/DTAMetaData-class.R:567-570, :637-638; R/DTAMetaData-helpers.R:207
- What: Small documentation gaps in the metadata files.
- Evidence:
  ```r
  #' @title Format Contact Information
  #' @description Helper to format contact list as readable string
  #' @keywords internal
  .format_contact <- function(contact) {          # no @param, no @return; man/dot-format_contact.Rd has \usage but no \arguments
  create_example_DTAMetaData <- function(index = 1) {
    # nolint                                       # a bare marker on its own line: it suppresses nothing
  ```
  `get_authorized_for_corrections()`'s `@return` omits that an unset value comes back as `NULL`.
- Failure scenario: `R CMD check` lists `contact` as an undocumented argument (inferred, not run).
- Confidence: verified-by-reading
- Suggested direction: add the `@param`/`@return`, remove the stray `# nolint`.

## Area 6 — rules: `R/DTARule-class.R`, `R/DTARuleColCondition-class.R`, `R/DTARuleColRange-class.R`, `R/DTARuleColUnique-class.R`, `R/DTARuleGroupCondition-class.R` (all read completely)

### F-c1-spec-49  [major] [code-cheat]
- Where: R/DTARuleGroupCondition-class.R:148-262 (esp. :157-167, :186-191, :217-218, :224-225, :251-252)
- What: The keys of a group-condition constraint are never checked against a closed list, so a misspelt or misplaced key is dropped and its value replaced by a default — for a scope that silently turns "all" into "any" and changes what the rule enforces.
- Evidence:
  ```r
  normalize_scope <- function(scope, arg_name) { scope <- scope %||% "any" ...
  left <- cst$left %||% cst[["if"]]                       # `$` partial-matches left_scope when `left` is absent
  left_scope = normalize_scope(cst$left_scope, "left_scope"),
  if_scope = normalize_scope(cst$if_scope, "if_scope"),
  ```
  `DTARuleFactory()` checks only the rule's top-level argument names, and the lint (`dta_group_unknown_conditions()`) only the condition names — whose own comment warns "`$` would partial-match `left` onto `left_scope`". The matrix block parser, by contrast, rejects unknown keys because "a misspelt `rnage` would otherwise be dropped and the range silently never enforced" (DTAMatrixSpec-class.R:72-74).
- Failure scenario: `{type: requires, if: c1, then: c2, then_scop: all}` builds without a word and evaluates with `then_scope = "any"`; `{type: mutually_exclusive, if: c1, then: c2, if_scope: all, then_scope: all}` (alias spelling) is accepted with both scopes "any", because only `left_scope`/`right_scope` are read for that type; the same constraint with `left_scope: all` instead aborts with "references unknown condition name: all", since `cst$left` partial-matches `left_scope`.
- Confidence: verified-by-reading
- Suggested direction: validate constraint keys per type against a closed list and read every key with `[[`.

### F-c1-spec-50  [minor] [code-cheat]
- Where: R/DTARule-class.R:116-121 (vs :43-46 and R/DTAColumnSpec-class.R:70-79)
- What: `DTARuleFactory()` rewrites whitespace in a rule id to underscores before the validator that forbids whitespace can see it, so the YAML path silently renames rules while the direct constructor refuses the same id.
- Evidence:
  ```r
  id <- if (!is.null(id)) { gsub("[[:space:]]+", "_", as.character(id)) } else { id }
  ...
  if (grepl("\\s", id)) { return("@id cannot have whitespaces.") }      # dta_id_problem(), never reached via the factory
  ```
  The roxygen (`@param id A character string specifying the rule identifier.`) does not mention it.
- Failure scenario: `id: "check age"` is stored and reported as `check_age`; together with a second rule `check_age` the lint reports "Rule id 'check_age' is used by more than one rule" although the document contains that id once; a numeric `id: 7` silently becomes `"7"`.
- Confidence: verified-by-reading
- Suggested direction: pass the id through untouched and let the validator report it.

### F-c1-spec-51  [minor] [dead-code]
- Where: R/DTARuleColRange-class.R:15-17, :53-60; R/DTARuleColUnique-class.R:12-13, :31, :39-41; R/DTARuleColCondition-class.R:77-78, :91-103; R/DTA-class.R:519, :1033-1034; R/lintDTA.R:1391, :1423
- What: Backwards-compatibility leftovers in the specification model (not needed before 1.0.0 per the owner).
- Evidence:
  ```r
  #' @param range ... Legacy argument for range checks; it is used only when both min and max are NULL, and is ignored otherwise.
  #' @param type Optional character value kept for backward compatibility. If provided, it must be "col_unique".
  # The property union still admits a bare character for backward compatibility, but nothing can evaluate one: reject it here
  # An object restored from an older version lacks properties its class now has.
  if (dta_is_stale(x)) x <- dta_upgrade(x)
  ```
  The legacy `range` is read by position after `unlist()`, ignoring names: `range: {max: 100, min: 200}` is accepted as [100, 200]; and it is silently ignored when `min` or `max` is also given. The roxygen example of `DTARuleColRange` and of `lint_dta()` still use it.
- Failure scenario: dead weight plus two ways for a spec to say something other than it reads (name-blind `range`, `range` overridden by a lone `min`).
- Confidence: verified-by-reading
- Suggested direction: drop `range`, the `type` argument, the character arm of `condition`/`then`, and the stale-object upgrade calls.

### F-c1-spec-52  [minor] [bug]
- Where: R/DTARule-class.R:23-35, :83-89; R/DTARuleColCondition-class.R:151-156 (and the three sibling `check` methods)
- What: The base `DTARule` constructor has a required `description` its documentation calls optional, and the `check()` methods registered for rules contradict each other.
- Evidence:
  ```r
  constructor = function(id, type, description) {                 # no default; every subclass has description = NULL
  cli::cli_abort(stringr::str_c("Check needs to be run from Class", " derived from DTAtools::DTARule class."))      # base
  cli::cli_abort("{.fn check} is not implemented for rules; use {.fn validate_table} on a table instead.", ...)     # every derived class
  ```
- Failure scenario: `DTARule(id = "r1", type = "col_unique")` -> base "argument "description" is missing, with no default"; `check(DTARule("r", "x", NULL))` tells the user to use a derived class, whose `check()` then says it is not implemented. The `check` help page lists methods for DTA and the dataset classes only.
- Confidence: verified-by-reading
- Suggested direction: default `description = NULL`; give the base method the same message (or drop the five stub methods).

### F-c1-spec-53  [minor] [bug]
- Where: R/DTARuleGroupCondition-class.R:96-101, :124-144, :186, :264; R/DTARuleColCondition-class.R:80-104; R/DTARuleColRange-class.R:83-115
- What: Rule validators check structure only, so content that the constructors normalise or assume is not re-validated on assignment and some shapes are never validated at all.
- Evidence:
  ```r
  dta_normalize_conditions(conditions[[i]], arg = condition_names[[i]])      # returns list() for an empty mapping: accepted
  cid <- cst$id %||% paste0("constraint_", i)                                # not checked to be a string
  ids <- vapply(out, function(x) x$id, character(1))                         # base vapply error for a numeric id
  ```
  `DTARuleColCondition()` refuses an empty `condition`, but a group condition `c1 = list()` is accepted (the lint iterates zero columns for it); `@condition<-`/`@conditions<-`/`@constraints<-` run only the is-list/non-empty tests, not the operator, arity or constraint checks.
- Failure scenario: `conditions: {c1: {}}` builds a vacuous condition; a constraint with `id: 1` aborts with R's translated `vapply` type message; `rule@constraints <- list(list(type = "bogus"))` is accepted on a standalone rule.
- Confidence: verified-by-reading (how the engine evaluates an empty condition was not read)
- Suggested direction: move the normalise-and-check step into the validators (or re-run it there) and reject empty conditions and non-string ids.

### F-c1-spec-54  [minor] [inconsistency]
- Where: R/lintDTA.R:403-416 (vs R/DTAColumnSpec-class.R:40-55)
- What: A YAML-boolean rule operand (`equals: N`) is reported only when the column declares a non-logical type; on an untyped column it passes both lint and construction, whereas the same trap in a column's `values` aborts whatever the type.
- Evidence:
  ```r
  if (is.logical(vals) && !is.na(fam) && !identical(fam, "logical")) {
    fb$add("rule", rid, "error", "rule_condition_value_unfit", ... "YAML reads an unquoted Y or N as TRUE or FALSE; quote it" ...
  ```
- Failure scenario: column `SEX` with no `type`, rule `condition: {SEX: {equals: N}}` -> no finding; the operand is `FALSE`, which the text the file holds can never equal, so the rule's `then` is never demanded.
- Confidence: inferred (the lint branch and the constructor were read; the engine's comparison of a text column with a logical operand, in evaluateRules.R, was not)
- Suggested direction: flag a logical operand whenever the column is not declared logical.

### F-c1-spec-55  [nit] [doc-wrong]
- Where: R/DTARuleColRange-class.R:3-4, :126; R/DTARuleColUnique-class.R:118; R/DTARuleColCondition-class.R:126-129; R/DTARule-class.R:152-157
- What: Typos and thin documentation in the rule classes.
- Evidence:
  ```r
  #' Represents a rule for checking the range of values in a specific columns of a data table.
  cli_alert_info("columns(s): {paste(x@columns, collapse = ', ')}")           # printed label; the unique rule prints "column(s):"
  id = "rule_unqiue2",                                                        # example id
  #' create example for DTARule  /  #' @param index rule selector             # valid indices (1; 1-2 for the unique rule) not stated
  ```
  `as.list()` of a base `DTARule` returns `id` and `type` only, dropping `description`, which every subclass method keeps.
- Failure scenario: cosmetic; a user-visible label and an example id carry typos.
- Confidence: verified-by-reading
- Suggested direction: fix the three strings; document the index ranges.

## Area 7 — second pass over the whole slice (cross-file checks, roxygen sweep of all 104 Rd files generated from the slice)

### F-c1-spec-56  [minor] [logic-error]
- Where: R/columnSpecChecks.R:182-183, :874-878, :925; R/DTAColumnSpecStructureSAS-class.R:147-172
- What: A SAS numeric column that declares a `length` is counted under "Length check passed" although `maxLength` is evaluated for string data only; the comment that excuses this states something untrue.
- Evidence:
  ```r
  if (!is.null(max_length) && !is.na(max_length) && base_type == "string") {            # numbers: skipped
  out$maxLength <- declares(function(s) !is.null(s$maxLength) && !anyNA(s$maxLength))  # numbers: counted
  # ... a column that declares a length but holds numbers is counted here and skipped there. Such a
  # column always fails the `type` check as well, so the mismatch is still reported
  ```
  A `SAS Num`/`SAS Int` column holding numbers passes the `type` check, so nothing is reported; only a *text-typed* column holding numbers would fail it. The SAS validator accepts `length` on every type (the R one restricts it to `character`).
- Failure scenario: `{id: AVAL, type: SAS Num, length: 8}` (the storage length PROC CONTENTS shows) -> the report prints "Length check passed (1 column)" for a check that never ran on that column.
- Confidence: verified-by-reading
- Suggested direction: count a column under `maxLength` only when its type family is text (or refuse `length` on SAS numeric types as the R backend does).

### F-c1-spec-57  [minor] [code-cheat]
- Where: R/columnSpecChecks.R:594, :606-611, :1385-1389; R/lintDTA.R:288-295, :957-960; R/DTAColumnSpecCollection-class.R:472
- What: Further `tryCatch(..., error = function(e) <default>)` sites in the slice that turn "could not be determined" into a benign answer instead of surfacing the failure.
- Evidence:
  ```r
  columns <- tryCatch(specs@columns, error = function(e) NULL)                 # not a collection -> "no columns", no errors
  function(s) tryCatch(as.character(s@id)[[1]], error = function(e) NA_character_)
  tryCatch(as_r_type(structure), error = function(e) "character")              # unknown structure -> treated as text
  cols <- tryCatch(dta_rule_all_columns(rule), error = function(e) NULL)       # -> only a *warning* `rule_not_checkable`, all cross-checks skipped
  errs <- tryCatch(metadata_import_errors(md), error = function(e) NULL)       # -> no metadata findings
  types <- tryCatch(as_json_schema_type(spec), error = function(e) NULL)       # -> no numeric-values warning
  ```
- Failure scenario: `dta_columnspec_errors(specs = <anything without @columns>, table)` returns "no errors" (`list(summarised_error = NULL, full_error = NULL)`); a rule whose columns cannot be derived stays in a collection, because the validator blocks on errors only and this is a warning.
- Confidence: verified-by-reading (which callers can actually reach each default was traced only for the schema path, F-c1-spec-16)
- Suggested direction: let these propagate (or convert them to error-severity findings) rather than default.

### F-c1-spec-58  [minor] [bug]
- Where: R/00_helpers.R:43-54
- What: `__extract_prefix_and_rest__()` looks at the first element of `type`/`format` only and has no guard for `NA` or zero length, so a sequence-valued declaration is silently truncated and the other odd shapes die with base errors.
- Evidence:
  ```r
  parts <- stringr::str_split(x, "\s+", n = 2)[[1]]
  prefix <- parts[1]
  ...
  if (is.null(prefix) || prefix == "") {
  ```
- Failure scenario: YAML `type: [SAS Char, SAS Num]` builds a `Char` column and drops `SAS Num` without a message; `type: []` -> "subscript out of bounds"; an `NA` type -> "missing value where TRUE/FALSE needed".
- Confidence: verified-by-reading
- Suggested direction: require a single non-missing string before splitting.

### F-c1-spec-59  [minor] [security]
- Where: R/columnSpecChecks.R:243-256; R/DTAColumnSpec-class.R:271-296; R/lintDTA.R:144-152 (contrast R/00_helpers.R:956-1015)
- What: A column `pattern` is matched with base `grepl(perl = TRUE)` and no step or time limit, while the package deliberately bounds every *file-name* pattern "because ICU's backtracking engine has NO time or stack limit".
- Evidence:
  ```r
  dta_map_unique(string_text()[present], function(u) grepl(pattern, u, perl = TRUE))
  # 00_helpers.R: "This is the one place those patterns are matched, with both limits on."
  ```
  The validator only proves that the pattern compiles (`grepl(expanded_pattern, "", perl = TRUE)`).
- Failure scenario: a specification with `pattern: "^(a+)+$"` on a column of long, distinct, almost-matching values makes `check()` spend PCRE's full backtracking budget on every distinct value — in the Shiny app that is a request any uploader of a YAML can make.
- Confidence: inferred (the unbounded call sites were read; how long PCRE's built-in match limit lets one value run was not measured)
- Suggested direction: route column and rule patterns through one bounded matcher, or cap the work per value.

### F-c1-spec-60  [nit] [bug]
- Where: R/DTADataSet-class.R:100-104 (outside the slice; found while tracing the factory)
- What: The dataset-type error message has a stray `)` inside the quoted value.
- Evidence:
  ```r
  "Property 'type' is '{self@type})', must be one of: {str_flatten_comma(`__DTAtools_supported_dataset_types__`)}"
  ```
- Failure scenario: `DTADataSet(id = "a", type = "tabel")` reports "Property 'type' is 'tabel)', must be one of: ...".
- Confidence: verified-by-reading
- Suggested direction: remove the parenthesis.

### F-c1-spec-61  [minor] [bug]
- Where: R/DTAMetaData-helpers.R:450-467 (with R/DTAMetaData-class.R:104-137, :153 and R/DTA-class.R:1244-1246)
- What: `@import_issues` is computed once, in the constructor, and is afterwards an ordinary writable property: correcting the offending field does not clear its issue, and assigning to the property clears issues without correcting anything — `check()` trusts it either way.
- Evidence:
  ```r
  issues <- tryCatch(x@import_issues, ...)                     # metadata_import_errors(): returned as stored
  import_issues = S7::new_property(S7::class_list, default = list())      # no validator, no setter
  metadata_errors <- metadata_import_errors(x@metadata)        # check(DTA)
  ```
  `as.list()` in the same file does test whether "a value changed since load" before trusting an issue (:139-159); the validation path does not.
- Failure scenario: metadata read with `date: "tbd"` carries a `not_convertible` issue; after `dta@metadata@date <- as.Date("2026-01-01")` the date is valid, yet `check(dta)` still reports "1 metadata value could not be imported in the declared type" until the object is rebuilt. Conversely `dta@metadata@import_issues <- list()` turns a failing metadata axis into a passing one.
- Confidence: verified-by-reading
- Suggested direction: derive the issues from the current field values (or re-validate them against the fields in `metadata_import_errors()`), and make the property read-only.

### F-c1-spec-62  [minor] [inconsistency]
- Where: R/DTAColumnSpecCollection-class.R:761-789; R/DTAColumnSpec-class.R:665-687; R/columnSpecChecks.R:5-7, :813
- What: The JSON Schema `as_json_schema()` publishes as "the contract for other tools to consume" is weaker than, and partly not expressible as, the contract the package enforces, and the comments that describe the vocabulary disagree with each other.
- Evidence:
  ```r
  schema <- list(type = "array", items = list(properties = properties, required = required))
  # columnSpecChecks.R:5  "The constraint vocabulary the package can emit is only eight keywords wide -- type, format,
  #                        maxLength, enum, const, pattern, required and additionalProperties"
  # columnSpecChecks.R:813 "The constraint vocabulary is closed and seven keywords wide"
  ```
  The engine reports an undeclared column under the keyword `additionalProperties` ("The specs declare no such column; it must not be present.", reportRendering.R:851), but the emitted schema never contains `additionalProperties: false`; its `format` values are SAS/R formats (`"DATE9."`, `"%Y-%m-%d"`) and `finite` is not a JSON Schema keyword, so a standard validator ignores them; for an empty collection `properties` is an unnamed empty list, which serialises as `[]` rather than an object.
- Failure scenario: a partner validating a delivery with the exported schema accepts a table with extra columns, impossible dates and infinite cell values that `check()` rejects.
- Confidence: verified-by-reading for what is and is not emitted; inferred for how third-party validators treat unknown `format` values and for jsonlite's `[]` (not run)
- Suggested direction: emit `additionalProperties: false` and `type: object` on `items`, document the non-standard keywords, and fix the two vocabulary comments.

---

## Coverage

**Fully read (every line), 22 of 22 slice files:**
`R/00_helpers.R` (1029), `R/DTA-class.R` (1640), `R/DTAColumnSpec-class.R` (780), `R/DTAColumnSpecCell-class.R` (141),
`R/DTAColumnSpecCollection-class.R` (915), `R/DTAColumnSpecRowLabel-class.R` (118), `R/DTAColumnSpecStructure-class.R` (162),
`R/DTAColumnSpecStructureR-class.R` (217), `R/DTAColumnSpecStructureSAS-class.R` (338), `R/DTAContainer-class.R` (121),
`R/DTAMatrixSpec-class.R` (347), `R/DTAMetaData-class.R` (740), `R/DTAMetaData-helpers.R` (559), `R/DTARule-class.R` (157),
`R/DTARuleColCondition-class.R` (169), `R/DTARuleColRange-class.R` (175), `R/DTARuleColUnique-class.R` (144),
`R/DTARuleGroupCondition-class.R` (306), `R/DTAtools-package.R` (83), `R/columnSpecChecks.R` (1450), `R/lintDTA.R` (1473), `R/zzz.R` (13).

**Also read, to trace findings (parts only — these belong to other slices and were NOT reviewed as such):**
`DESCRIPTION`, `NAMESPACE`, `[assistant-tool].md`; `R/DTADataSet-class.R` 1-135, 350-465, 610-680; `R/DTADataSetTabular-class.R` 1170-1280, 1690-1725, 1815-1840;
`R/validationFunctions.R` 290-413; `R/evaluateRules.R` 1-100, 610-800 (`dta_as_numeric_strict`, `dta_normalize_conditions`, `dta_check_condition_arity`);
`R/importConversion.R` 455-515; `R/DTAFile-class.R` 404-500; `R/DTAFileTabular-class.R` 1385-1444; `R/rTemporalFormat.R` 260-325;
`R/sasTemporalText.R` 1-60; `R/datePatternTokens.R` (two functions); `R/streamingScale.R` 36-75; `R/exportFunctions.R` 60-135;
`R/documentData.R` 440-490; `R/formattingHelpers.R` 313-345; `man/dta_columnspec_check_summary.Rd`, `man/dot-format_contact.Rd`;
`DTA_YAML_GUIDE.md` (grep for `nullable`, `length`, `type: file`); `inst/extdata/clinical_dta_with_file_dataset.yaml` 265-286;
`.github/workflows/R-CMD-check.yaml` 118-136.

**Mechanical sweep:** `\usage` vs `\arguments`, `\value`, `\examples` of all 104 Rd files generated from the slice (awk over `man/`);
the only undocumented arguments are those in F-c1-spec-26 and F-c1-spec-48; every exported function of the slice has an `\examples` block.

**Not done / limits:**
- Nothing was executed (per the brief). Findings that rest on library behaviour outside `$SNAP` are marked `inferred`:
  F-c1-spec-31 (newer S7), F-c1-spec-36 (jsonlite `digits`, yaml `precision` defaults), F-c1-spec-59 (PCRE match limit), F-c1-spec-54 (engine comparison).
- The benchmark figures quoted in the roxygen of `dta_stream_block_size()` (00_helpers.R:241-303) were not checked against `benchmarks/`.
- Tests and the qualification suite were not reviewed (other slices); they were only grepped to see whether a behaviour is pinned
  (SAS impossible dates: no test; `dta_columnspec_schema_failed`: no handler or test; `version_unquoted`: tested).
- S7 validator contract, checked for all 14 classes of the slice: no validator can return `TRUE` or another non-`NULL`
  non-character value — each ends in `NULL`, in a character vector, or in an `if` whose false branch yields `NULL`. The defects found
  are of the other kinds: a discarded message (F-c1-spec-22), unreachable branches (F-c1-spec-23), base-R crashes on non-scalar or `NA`
  input (F-c1-spec-21, -22, -23, -24, -44) and content that is checked in the constructor only (F-c1-spec-53).

## Tally

62 findings (counted from the headings of this file).

By severity: blocker 0, major 6 (F-16, F-17, F-18, F-31, F-36, F-49), minor 43, nit 13.
By category: bug 24, code-cheat 11, doc-wrong 10, doc-missing 4, dead-code 4, inconsistency 4, logic-error 2, design 2, security 1.
