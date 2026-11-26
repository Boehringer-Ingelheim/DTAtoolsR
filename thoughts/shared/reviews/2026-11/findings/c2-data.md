# Findings — slice c2-data (data layer and validation engine)

Reviewer key: `c2-data`. Tree: `$SNAP` (pinned `68408c44`). Read-only review; no R was run.

Slice (24 files, 16 918 lines): `R/DTADataSet-class.R`, `R/DTADataSetFile-class.R`,
`R/DTADataSetMatrix-class.R`, `R/DTADataSetTabular-class.R`, `R/DTAFile-class.R`,
`R/DTAFileAny-class.R`, `R/DTAFileCSV-class.R`, `R/DTAFileDelim-class.R`,
`R/DTAFileSAS7BDAT-class.R`, `R/DTAFileTSV-class.R`, `R/DTAFileTabular-class.R`,
`R/DTAFileXPT-class.R`, `R/containerAssignments.R`, `R/containerFiles.R`,
`R/containerFindings.R`, `R/datePatternTokens.R`, `R/evaluateRules.R`,
`R/fileNameConflicts.R`, `R/formattingHelpers.R`, `R/importConversion.R`,
`R/loadDelivery.R`, `R/rTemporalFormat.R`, `R/sasTemporalText.R`,
`R/validationFunctions.R`.

Files fully read: all 24 (see "Coverage" at the end). 51 findings: 6 major, 35 minor, 10 nit.

---

Progress log (files fully read, in reading order): `R/DTAFile-class.R`, `R/DTAFileTabular-class.R`,
`R/DTAFileCSV-class.R`, `R/DTAFileTSV-class.R`, `R/DTAFileDelim-class.R`, `R/DTAFileSAS7BDAT-class.R`,
`R/DTAFileXPT-class.R`, `R/DTAFileAny-class.R`, `R/importConversion.R`, `R/rTemporalFormat.R`,
`R/sasTemporalText.R`, `R/datePatternTokens.R`, `R/DTADataSet-class.R`, `R/DTADataSetTabular-class.R`,
`R/validationFunctions.R`, `R/containerAssignments.R` (+ support, outside the slice: `R/00_helpers.R`,
`R/DTAColumnSpecStructureSAS-class.R`, `NAMESPACE`, `DESCRIPTION`, the `man/*.Rd` generated from the slice).

## Batch 1 — file handlers, readers, import typing, dataset classes, validate_table

### F-c2-data-01  [major] [doc-wrong]
- Where: man/check.Rd:7-36 (esp. 31-35); man/load_file.Rd:11-41 (esp. 33-39); man/read_file_execution.Rd (`\usage`, `\arguments`, `\value`); man/open_file_execution.Rd; man/print_info.Rd (`\usage`). Sources: R/DTADataSetTabular-class.R:1388-1456 and 841-858; R/DTAFileTSV-class.R:102-106; R/DTAFileCSV-class.R:105-111; R/DTAFileTabular-class.R:216-228.
- What: Every method documents itself into the generic's Rd topic with its own `@param x`/`@param ...`/`@return`/`@usage`, and only ONE `\item{...}` survives per page, so the installed help for `check()` and `load_file()` shows the file-dataset method's argument text and silently drops the whole tabular-method argument documentation.
- Evidence: man/check.Rd:31-33 is the only place the scan arguments appear: `\item{batch_rows, max_errors, fail_fast, on_missing_column, use_threads}{ Accepted and ignored. They tune the scan of a tabular table; a file dataset reads no rows.` -- `tab`, `datasets`, `validation_run` and the text of R/DTADataSetTabular-class.R:1391-1455 (`max_errors` spill semantics, `fail_fast`, `on_missing_column`) are absent from the Rd. man/load_file.Rd:39: `\item{stream}{Accepted and ignored; a file dataset reads nothing.}` and :33 `\item{name}{... Must equal \code{basename(file)}` -- on the same page whose section "Reading into memory, or scanning in batches" says `stream` decides how the file is held. man/read_file_execution.Rd: `\item{x}{A \code{DTAFileTSV} object containing file reading parameters.}`; its `\usage` repeats `read_file_execution(x, ...)` six times, `\value` has seven paragraphs and `\description` concatenates seven method descriptions.
- Failure scenario: a user runs `?check` to find out what `max_errors`/`fail_fast` do for a table and reads "Accepted and ignored"; `?load_file` tells them `stream` is ignored and `name` must equal `basename(file)` (false for the tabular method, whose default strips the extension). Other pages send the reader here for the detail (man/load_file.Rd "see `check()`", R/DTAFile-class.R:846-852, R/DTADataSetTabular-class.R:30-38) and the detail is not there.
- Confidence: verified-by-reading (the Rd files and the roxygen blocks; roxygen was not re-run -- the shipped `man/` is what was read)
- Suggested direction: document each generic's arguments once (one block per topic, methods described in a `@section`), or give each method its own topic; never repeat `@param x`/`@param ...`/`@usage` across blocks that share a `@name`.

### F-c2-data-02  [minor] [doc-wrong]
- Where: R/DTAFile-class.R:868-871 (also man/open_file.Rd:74); contradicted by R/DTAFileTabular-class.R:430-441 and :1894
- What: `open_file()` documents that a UTF-16/UTF-32/UCS file "must be read with `stream = "never"`", but the in-memory reader refuses the same file.
- Evidence: doc: `so such a file is refused and must be read with \code{stream = "never"}.`; code: `dta_read_delim_normalized()` starts with `dta_refuse_wide_encoding(dta_reader_parse_settings(handler)$encoding, path)`, whose message is `"UTF-16 and UTF-32 are not supported for tabular reading. Convert the file to UTF-8 first."`, and the comment above that helper says `# Refuses a wide encoding for EVERY reader (lazy and eager, whatever `stream` says)`.
- Failure scenario: a user with `encoding: UTF-16LE` follows the help, calls `load_file(..., stream = "never")`, and gets the same refusal the help said this avoids.
- Confidence: verified-by-reading
- Suggested direction: say "convert the file to UTF-8 first" in `open_file()`'s help, as the abort already does.

### F-c2-data-03  [minor] [doc-wrong]
- Where: R/DTAFileTabular-class.R:36-37, R/DTAFileCSV-class.R:27-28, R/DTAFileTSV-class.R:26-27, R/DTAFileDelim-class.R:31-32; code R/DTAFileTabular-class.R:147, 175-182
- What: `quote` is documented as "Character or `NULL`" on all four delimited handlers, but `NULL` is not accepted.
- Evidence: `#' @param quote Character or \code{NULL}; quoting character for fields.` vs. the property `quote = class_character,` (not `class_character_or_null`) and the validator's own wording `"'quote' must be a single character, or \"\" for no quoting."`.
- Failure scenario: `DTAFileCSV("a.csv", quote = NULL)` (or YAML `quote: ~`) fails S7's property type check with a plain, class-less error; the documented way to switch quoting off does not exist, and the real one (`quote = ""`) is not in the parameter text.
- Confidence: verified-by-reading (that S7 rejects `NULL` for a `class_character` property is S7's documented behaviour; not executed)
- Suggested direction: document `""` as "no quoting" and drop "or NULL" (or map `NULL` to `""` in the constructor).

### F-c2-data-04  [minor] [doc-wrong]
- Where: R/DTAFileCSV-class.R:29-32, 102-104, 131-133; R/DTAFileTSV-class.R:28-31, 98-100, 130-132; R/DTAFileDelim-class.R:33-36, 105-107, 137-138; code R/DTAFileTabular-class.R:1946-1964
- What: The three concrete handlers say declared `missing_values` are honoured "in addition to the empty string", omitting that the literal text `NA` is always read as missing too.
- Evidence: `unique(c("", "NA", declared))` and `return(NULL)` (= Arrow's `c("", "NA")`) in `dta_reader_na_values()`; only the parent `DTAFileTabular` page (R/DTAFileTabular-class.R:27-31) mentions `"NA"`.
- Failure scenario: a `SAS Char` column holding the country code `NA` (Namibia), declared `nullable: false`: every such cell is reported as missing; the page of the handler the user actually constructs (`DTAFileCSV`) gives no hint why and offers no way to turn it off.
- Confidence: verified-by-reading
- Suggested direction: state the `""`/`"NA"` rule identically on all four class pages and in the method blocks.

### F-c2-data-05  [minor] [bug]
- Where: R/DTAFile-class.R:154 (consumer :347); R/DTAFileTabular-class.R:171-173 (consumers :1488, :1643, :1665, :1683); related R/DTADataSet-class.R:100
- What: The validators accept `NA` for `pattern` and `has_header`; `pattern = NA` then dies in a base-R `if`, and `has_header = NA` silently means "no header".
- Evidence: `if (!is.logical(self@pattern) || length(self@pattern) != 1) {` and `if (!is.logical(self@has_header) || length(self@has_header) != 1) {` -- no `is.na()` test, unlike the `newlines_in_values` check a few lines below, which adds one. Consumers: `if (x@pattern) {` and `isTRUE(has_header)` throughout the reader plan. Related: the `DTADataSet` validator's `if (!self@type %in% ...)` has no length-1 guard, so a non-scalar `type` also ends in base R's condition-length error.
- Failure scenario: `DTAFileCSV("a.csv", has_header = NA)` (YAML `has_header: .na`) constructs; the reader takes the `isTRUE()` = FALSE branch, autogenerates `f0, f1, ...` and validates the header line as a data row: every declared column "missing", nothing pointing at the handler. `DTAFile("a.csv", pattern = NA)` constructs and `matches_filename()` aborts with base R's "missing value where TRUE/FALSE needed" (translated on a German machine, no class).
- Confidence: verified-by-reading
- Suggested direction: add `|| is.na(...)` to both checks, as `newlines_in_values` has.

### F-c2-data-06  [minor] [dead-code]
- Where: R/containerAssignments.R:84-253 (`dta_current_class`, `dta_class_chain`, `dta_is_stale`, `dta_is_old_matrix`, `dta_upgrade`, exported `upgrade_dta`); call sites R/DTADataSetTabular-class.R:891-892, 1491 and R/containerAssignments.R:51; R/DTAFile-class.R:437-439, 474-481; R/DTAFileTabular-class.R:384-390; R/DTADataSet-class.R:108-127, 651-658; R/validationFunctions.R:554-593 with R/DTADataSetTabular-class.R:1289-1291, 1308-1311, 1785-1799
- What: A whole backward-compatibility layer for objects and artifacts saved by older versions is still in the data layer, although no compatibility is required before 1.0.0.
- Evidence: `# A restored 0.27.0 object always qualifies (no `containers`, no `provenance`)`; `dta_handler_container <- function(h) tryCatch(h@container, error = function(e) NULL)` ("A handler restored from an autosave written before `container` existed"); `value <- tryCatch(S7::prop(handler, name), error = function(e) NULL)` ("a handler predating these properties"); `dta_migrate_validation_details()` ("recorded by an older version of the package"); "An index entry recorded before the import axis existed"; "The fallback keeps working against a result (or a restored store entry) that predates the split."
- Failure scenario: not a wrong result today; cost and risk: every `check()`/`load_file()`/`container_assignments()` runs the staleness scan, and the `tryCatch(..., error = function(e) NULL/list())` accessors turn ANY error from the property read (not only "old object") into "no container"/"no provenance"/default parse settings, so a genuinely broken handler reads as an unconstrained one.
- Confidence: verified-by-reading
- Suggested direction: delete the layer (and `upgrade_dta()`), read the properties directly, and let a pre-1.0 object fail loudly.

### F-c2-data-07  [minor] [dead-code]
- Where: R/DTAFileTabular-class.R:327-346 (`dta_normalize_column_names`); R/DTADataSetTabular-class.R:1355-1380 (`invalidate_by_spec_change`)
- What: Two helpers have no caller in `R/` or the app and are kept alive only by tests that exercise them directly.
- Evidence: `#' Neither reader needs this any more ... and the tests exercise it directly.`; grep finds `dta_normalize_column_names` only at its definition and in tests/testthat/test-DTAFile.R:39-85, and `invalidate_by_spec_change` only at its definition and in tests/testthat/test-DTADataSetTabular-validation.R:246-281 and test-importAxis.R:327. The latter's roxygen says `call it yourself after changing \code{@specs}` although it is `@keywords internal` and not exported, so a user cannot call it without `:::`.
- Failure scenario: tests named after header cleaning / spec invalidation pass while testing code no production path runs (mutation: break the real cleaning call in `dta_delim_reader_plan()` -- the three `dta_normalize_column_names` tests still pass). `invalidate_by_spec_change()` moreover drops `@import_issues` without re-typing the table, so if it were ever wired in, the dataset would hold a table typed under the old specs with no issue record.
- Confidence: verified-by-reading (grep over `R/`, `inst/shiny`, `tests/`; the definitions and the test call sites were opened)
- Suggested direction: remove both and point the tests at `dta_clean_column_names()` and at the `specs_hash` gate `check()` really uses.

### F-c2-data-08  [minor] [inconsistency]
- Where: R/DTAFileTabular-class.R:513-519 (doc) vs. :587, :636-642 (code)
- What: The transcoder documents a bound on the size of the UTF-8 COPY but enforces it on the number of SOURCE bytes consumed.
- Evidence: doc: `The copy may not grow past `DTAtools.container_max_bytes`; the conversion aborts ...`; code: `bytes_done <- bytes_done + cut` (`cut` indexes the undecoded buffer) then `if (bytes_done > max_copy) { dta_container_too_large(path, "DTAtools.container_max_bytes", bytes_done, max_copy) }`; nothing counts what `writeBin(charToRaw(piece), output)` writes.
- Failure scenario: a latin1 file that is wholly accented (the doc's own "up to twice it" case) just under the limit converts to a copy about twice the limit under `DTAtools.temp_dir`; the per-call disk bound ("the bytes one call may write; the app sets it for every load") is exceeded by up to 2x without the abort.
- Confidence: verified-by-reading
- Suggested direction: accumulate the written byte count and compare that, or reword the doc to "source bytes".

### F-c2-data-09  [minor] [security]
- Where: R/DTAFileTabular-class.R:1160-1167, 1183-1187, 1298-1307 (`dta_head_records`), reached from :1366 (`dta_matrix_read_head`) before any size check
- What: The header scan has no size cap on the FIRST record, so a matrix file with no line end (plain or gzip) is read into R memory in full.
- Evidence: `if (later && length(buf) - pos + 1L >= max_scan) {` -- the cap applies only to records after the first; for the first the loop is `r <- scan_record(pos, ...); if (!is.null(r)) break; ...; more()` with `more()` reading `max(1048576L, length(buf))` further bytes each time, through `gzfile()` for a `.gz`. The roxygen says so (`with no size cap on the first record`). The transcoder beside it does bound the same thing (`max_line <- 64 * block`, "A line that long is not a delimited file").
- Failure scenario: a matrix dataset is handed `counts.tsv.gz`, 10 MB on disk, inflating to 10 GB of bytes without `0x0A`/`0x0D` (deflate reaches about 1000:1, as the transcoder's own comment notes): `load_file()` -> `dta_matrix_read_head()` grows `buf` (plus the `c(buf, chunk)` copy and the logical masks) until the R process -- in the app, the worker shared by every session -- runs out of memory. For a tabular dataset Arrow's schema read fails first, so only the matrix route (any `column_names =` caller) is exposed.
- Confidence: verified-by-reading for the loop and the call order in `dta_matrix_read_head()`; inferred that Arrow rejects an over-long first line before `dta_header_layout()` runs on the tabular route (Arrow not run)
- Suggested direction: cap the first record too (the 250,000-field matrix limit gives a natural byte bound) and abort with a classed condition.

### F-c2-data-10  [minor] [inconsistency]
- Where: R/DTAFileTabular-class.R:1884-1933 (eager), :1531-1539, vs. :531-538 (lazy) and the doc at :79-81
- What: A misspelled `encoding` is refused "before the file is opened" only on the lazy path; the eager path swallows the first failure and leaves the user with Arrow's error.
- Evidence: `dta_check_encoding_supported()` is called only from `dta_transcode_to_utf8()` (:536) and `dta_matrix_read_head()` (:1362). On the eager route the header names go through `converted <- tryCatch(iconv(raw_names, from = encoding, to = "UTF-8"), error = function(e) NULL)` and `if (!is.null(converted)) {...}` -- the unsupported-conversion error is discarded and the undecoded names kept -- and `ifelse(is.na(converted), raw_names, converted)` likewise keeps any name that could not be decoded.
- Failure scenario: `DTAFileCSV("a.csv", encoding = "latin-1")` with `stream = "never"`: the file is opened and its header read, the iconv error is swallowed, and the read then fails inside `arrow::read_delim_arrow()` with a message naming neither the handler nor `iconvlist()`; the same handler with `stream = "always"` gets the package's classed `dta_encoding_unsupported`. One file, two different failures -- the divergence the "one reader plan" exists to prevent.
- Confidence: verified-by-reading for the call sites and the swallow; inferred that Arrow's re-encoding stream then raises its own error (not run)
- Suggested direction: call `dta_check_encoding_supported()` in `dta_delim_reader_plan()` so both readers share it, and let an undecodable header name abort instead of passing through.

### F-c2-data-11  [nit] [doc-wrong]
- Where: R/DTAFileTabular-class.R:410-428, 471-492, 826-847
- What: Two statements in the transcoding docs are stronger than the code: the wide-encoding test does not cover "the spellings `iconv` accepts", and not every `0x00` byte aborts.
- Evidence: `grepl("^(utf|ucs)[-_]?(16|32|2|4)", tolower(encoding))` matches none of the iconv aliases `UNICODE`, `UNICODELITTLE`, `UNICODEBIG`, `CSUNICODE`, `ISO-10646-UCS-2`, `WCHAR_T`; and `rawToChar(bytes[...])` silently strips TRAILING nul bytes (see `?rawToChar`), so `A block ... which holds a `0x00` byte ... aborts the conversion` and `The copy is byte-identical to what iconv(...) of the whole file would produce` do not hold for a last, unterminated line that ends in NULs.
- Failure scenario: `encoding: UNICODE` on a UTF-16 file: the lazy path reports "holds a 0x00 byte" instead of the wide-encoding advice, while the eager path hands `UNICODE` to Arrow and reads the file -- the family that is "refused for EVERY reader" is read by one of them under an alias. A NUL-padded last line loses its padding in the copy without a word.
- Confidence: inferred (alias lists are platform facts and were not queried; `rawToChar` behaviour from its documentation)
- Suggested direction: soften the two sentences, or test for a NUL explicitly before `rawToChar()`.

### F-c2-data-12  [minor] [bug]
- Where: R/DTAFileAny-class.R:196-205 (comment at :183-189)
- What: The guard that refuses a non-text `extensions` entry is bypassed by any mixed list, because `unlist()` coerces before the `is.character()` test.
- Evidence: `if (is.list(extensions)) { extensions <- unlist(extensions, use.names = FALSE) }` then `if (!is.character(extensions)) { cli::cli_abort(...) }`. The comment promises the opposite: `anything that did not already arrive as character aborts instead, naming the value and telling the author to quote it`.
- Failure scenario: `DTAFileAny("r.*", pattern = TRUE, extensions = list("pdf", FALSE))` -- what YAML `extensions: [pdf, no]` parses to -- gives `c("pdf", "FALSE")`, which IS character, so the handler silently stores the unmatchable ending `"false"`: the silent coercion the comment says was removed. Only a sequence whose members are ALL non-text is caught.
- Confidence: verified-by-reading (R's `unlist()` coercion rule; not executed)
- Suggested direction: test `vapply(extensions, is.character, logical(1))` on the list before flattening.

### F-c2-data-13  [minor] [bug]
- Where: R/DTADataSetTabular-class.R:732-756
- What: `create_example_DTADataSetTabular()` dispatches a NUMERIC `index` with `switch()`, so its "No example available" abort fires only for `index = 4`; any other out-of-range index returns `NULL` invisibly.
- Evidence: `switch(index, `1` = {...}, `2` = {...}, `3` = {...}, cli::cli_abort("No example available for the provided index."))` with the default `index = 1` (a double). For a numeric EXPR `switch()` selects by POSITION and ignores the names; the abort is merely the 4th alternative, and with no alternative selected `NULL` is returned invisibly.
- Failure scenario: `ds <- create_example_DTADataSetTabular(5)` (or `0`, `-1`, `7`) returns `NULL` with no message; the failure surfaces later, somewhere else. `index = 4` is the one value that produces the intended error. (The sibling `create_example_DTAFileCSV()` uses `if (index == 1) ... else abort` and is correct.)
- Confidence: verified-by-reading (documented `switch()` semantics; not executed)
- Suggested direction: validate `index %in% 1:3` first, or switch on `as.character(index)`.

### F-c2-data-14  [minor] [logic-error]
- Where: R/DTADataSetTabular-class.R:1277-1312; doc :1263
- What: `validation_errors(source = "memory")` is not "memory": when the in-memory store has no entry it falls through and reads the artifact from disk, exactly like `"auto"`.
- Evidence: `if (source %in% c("auto", "memory")) { in_memory <- x@validation_store[[table_name]]; if (!is.null(in_memory)) { return(...) } }` is followed unconditionally by `entry <- x@validation_index[[table_name]]` ... `readRDS(entry$artifact_path)`. Also `table_name <- table_name[[1]]` silently takes the first table when `table` names several, or is `NULL`.
- Failure scenario: with the store entry gone but the index entry present, `validation_errors(ds, "t", source = "memory")` returns whatever RDS sits at `artifact_path` -- or aborts with "No validation artifact available ... Run check() first with persist = TRUE", a message about the source the caller explicitly did not ask for. `validation_errors(ds, c("a", "b"))` answers for `a` only.
- Confidence: verified-by-reading
- Suggested direction: abort for `source = "memory"` when the store has no entry, and refuse a `table` that does not resolve to exactly one name.

### F-c2-data-15  [minor] [inconsistency]
- Where: R/DTADataSetTabular-class.R:1346-1348 (clear), :1794-1808 (refill for lazy tables only), :1019-1026 (load)
- What: `clear_validation()` deletes `@import_issues`, which for a table held in memory is a LOAD-time record that `check()` never rebuilds, so afterwards the property says "no import issues" while the verdict reports them.
- Evidence: `x@import_issues[[table_name]] <- NULL` in the clear loop; in `check()` the property is written only inside `if (dta_table_is_lazy(current_table)) {`. The eager issues are documented as `# Canonical copy on the dataset, keyed by table name.` and still ride on the table as the `dta_import_issues` attribute, which `validate_table_detailed()` reads (R/validationFunctions.R:156).
- Failure scenario: load a CSV eagerly with one unparseable `SAS Num` cell; `ds@import_issues$t` has one row. `ds <- clear_validation(ds); ds <- check(ds)`: `validation_status(ds)$n_import_errors` is 1 and `ok` is FALSE, but `ds@import_issues` is `list()` -- two answers to "does this table have import issues", depending on which accessor is asked. The same sequence on a lazily loaded table repopulates the property.
- Confidence: verified-by-reading for the two code paths; inferred that `dta_validate_any_table()` routes a materialised table to `validate_table_detailed()` (R/streamingValidation.R is outside this slice; the comment at R/validationFunctions.R:129-131 says so)
- Suggested direction: do not treat `@import_issues` as validation state (leave it alone in `clear_validation()`), or have `check()` rewrite it from `details` for every holding.

### F-c2-data-16  [minor] [perf]
- Where: R/DTADataSetTabular-class.R:1553-1563, 1810-1820, 1840; R/DTADataSet-class.R:615-624
- What: `check()` persists an RDS of the full details for every validated table on every non-skipped run by default and never removes the previous one, in a directory keyed only by the sanitised dataset id.
- Evidence: `persist = TRUE` (default); `artifact_path <- file.path(table_dir, paste0(run_id, ".rds")); saveRDS(details, artifact_path)`; the index entry is then simply replaced (`x@validation_index[[table_name]] <- index_entry`) with no `unlink()` of `previous$artifact_path`. Directory: `file.path(tempdir(), "DTAtools_validation", gsub("[^A-Za-z0-9_-]", "_", x@id))`, table sub-folder `gsub("[^A-Za-z0-9_-]", "_", table_name)`, file name a millisecond timestamp.
- Failure scenario: a long-lived process that re-validates after every edit leaves one orphaned RDS (up to `max_errors` = 10,000 retained rows per axis, plus the rule results) per table per run under `tempdir()` for the life of the process. Independently, ids/table names that differ only in non-ASCII letters or punctuation (`"Größe"`/`"Gr__e"`, `"a b"`/`"a_b"`), and equal ids in two DTAs or two app sessions, share one folder and are kept apart only by the millisecond in `run_id`.
- Confidence: verified-by-reading
- Suggested direction: delete the superseded artifact when the index entry is replaced, and add a unique component (not only a timestamp) to the file name.

### F-c2-data-17  [minor] [bug]
- Where: R/DTADataSetTabular-class.R:609-631 (`write_table_to_file`)
- What: The export is written with `write.table()` in the session's NATIVE encoding (no `fileEncoding`), so its bytes depend on the locale, while the package's own readers default to UTF-8 and the checksum sidecar vouches for whatever was written.
- Evidence: `write.table(table_data, file = temp_file, sep = sep, na = na, row.names = row.names, quote = quote, ...)` in both branches, with no `fileEncoding = "UTF-8"`; `DTAFileTabular(..., encoding = "UTF-8")` is the reader default.
- Failure scenario: under a non-UTF-8 locale (R 4.1 on Windows, which `Depends: R (>= 4.1.0)` admits, or `LANG=C` in a container) a `SAS Char` value `"μg/L"` is written as `<U+03BC>g/L` and `"Müller"` as a single cp1252 byte; the file reads back with different text or fails Arrow's UTF-8 check, and `write_metadata()` has recorded an MD5 for it. On the UTF-8 dev machine and CI the same call writes correct bytes, so no test sees it.
- Confidence: inferred (follows from `write.table()`'s documented translation to the native encoding; not executed under a non-UTF-8 locale)
- Suggested direction: pass `fileEncoding = "UTF-8"` by default (overridable through `...`).

### F-c2-data-18  [minor] [design]
- Where: R/DTAFileTabular-class.R:216-254, 100-103; R/DTADataSetTabular-class.R:199-207; R/DTAFileCSV-class.R:112-123; R/DTAFileTSV-class.R:111-122; R/DTAFileTabular-class.R:1364
- What: `DTAFileTabular` is documented as "a virtual class" but is instantiable and accepted by the tabular dataset validator, and `DTAFileCSV`/`DTAFileTSV` carry a writable `sep` property that their readers ignore.
- Evidence: `#' \code{DTAFileTabular} is a virtual class.` vs. the class's own example `handler <- DTAFileTabular(filename = "data.txt", sep = ",")` and the dataset validator `inherits(file_info, "DTAtools::DTAFileTabular") || inherits(file_info, "DTAtools::DTAFileSAS7BDAT")`; CSV reads with the literal `delim = ","` while `dta_matrix_read_head()` tokenises the header with `delim <- handler@sep`.
- Failure scenario: `DTADataSetTabular(id = "d", specs = s, files = list(DTAFileTabular("data.txt", sep = ";")))` passes the validator whose comment says it exists so that a reader-less handler does not "construct happily here and then abort deep inside the read with 'This method is not implemented'" -- which is exactly what `load_file()` then does. `h <- DTAFileCSV("m.csv"); h@sep <- ";"` validates, prints "Separator: ;", reads the body with `,` and (in a matrix dataset) the header with `;`.
- Confidence: verified-by-reading
- Suggested direction: make the base class abstract (`abstract = TRUE`) or give it the `DTAFileDelim` reader; take the CSV/TSV delimiter from `x@sep` or make the property read-only there.

### F-c2-data-19  [minor] [design]
- Where: R/validationFunctions.R:28-34, 46-98
- What: `validate_table()` signals failure in three different ways depending on the axis: a column-spec failure is RETURNED (a list, plus a warning for rules), a rule or import failure ABORTS, and success returns the table.
- Evidence: `if (!isTRUE(details$columnspec_valid)) { ...; return(columnspec_errors) }` vs. `cli::cli_abort(c("Rule violations:", ...))` and `cli::cli_abort(c("Import errors:", ...))`, with `table` returned at the end.
- Failure scenario: `clean <- validate_table(specs, df)` in a pipeline: for a table with a missing column the call "succeeds" and `clean` is an error list the next step treats as data; the same pipeline stops correctly when the only defect is a rule. A caller must test the return TYPE to learn the verdict of an exported validator.
- Confidence: verified-by-reading
- Suggested direction: one contract -- always return the details (as `validate_table_detailed()` does) or always abort with a classed condition carrying them.

### F-c2-data-20  [nit] [doc-wrong]
- Where: R/DTAFile-class.R:982 vs. R/DTADataSet-class.R:220-263; R/DTADataSet-class.R:136, 174 and R/DTAFile-class.R:205, 230; R/DTADataSetTabular-class.R:321; R/importConversion.R:437-439 vs. :462-464; R/DTADataSet-class.R:664-705 and R/00_helpers.R:506-508; R/DTADataSetTabular-class.R:708
- What: Several `@return`/`@param`/description statements do not match the code.
- Evidence: (a) `print_info`: `@return Invisibly, \code{x}.` but the `DTADataSet` method ends in a `for` loop / `cli_alert_info()` and returns `NULL`. (b) `max_number_of_files()`/`min_number_of_files()`: "numeric: number of files" / "The number of files." -- the handler method returns `NULL` for an unset bound and the dataset method `Inf`; neither is documented, and man/max_number_of_files.Rd documents `x` as "An object of class `DTAFile`" on a page that also covers datasets. (c) `specs()`: `@param x An object of class DTADataSet.` -- only `DTADataSetTabular` has a method. (d) `dta_coerce_column()`: "An `Int` target is always stored as R `integer`" while the function returns `NULL` (column untouched) for any already-numeric input, which the SAS reader relies on ("held as an Arrow double"). (e) `check()` for `DTADataSet`: "Validates a DTADataSet object's properties and structure" / "the base method, which checks structure only" -- for a base-class object it only aborts ("check() requires a DTADataSet subclass ..."). (f) `create_example_DTADataSetTabular`: "S7 method to create ..." -- a plain function.
- Failure scenario: `x <- print_info(ds)` yields `NULL`; `max_number_of_files(handler) + 1` is `numeric(0)` for an open-ended handler; `check(DTADataSet(id = "d", type = "file"))` errors where the help promises a structure check.
- Confidence: verified-by-reading
- Suggested direction: correct each sentence (and end the `DTADataSet` `print_info` method in `invisible(x)`).

### F-c2-data-21  [nit] [doc-wrong]
- Where: R/DTADataSet-class.R:102; R/DTADataSetTabular-class.R:820; R/DTADataSetTabular-class.R:1173, 1259, 1263, 1317, 1321, 1323, 1360, 1363 (-> man/validation_status.Rd:10, man/validation_errors.Rd:10 and :15); R/DTAFile-class.R:370-371; R/DTAFileAny-class.R:221-224
- What: Typos and misplaced comments in user-visible strings and help.
- Evidence: `"Property 'type' is '{self@type})', must be one of: ..."` (stray `)` inside the quotes); `"Specs: {length(x@specs@columns)} columns, {length(x@specs@rules)}, rules"` (prints "3 columns, 2, rules"); roxygen `A \'DTADataSetTabular\' or \'DTADataSetFile\' object.` -- the backslash-quote survives into the Rd verbatim instead of the `\code{}`/backticks used everywhere else; the comment `# NULL when `value` is an acceptable container ...` sits above `dta_file_count_bounds_problem()` instead of `dta_container_value_problem()` (40 lines further down); the DTAFileAny comment says the generic's block "produces no \usage section" while R/DTAFile-class.R:321 has `@usage matches_filename(x, ...)` and man/matches_filename.Rd has a `\usage`.
- Failure scenario: cosmetic; the two message typos are shown on every invalid `type` and every `print_short_info()`.
- Confidence: verified-by-reading (how R renders `\'` from an Rd was not checked)
- Suggested direction: fix the strings; use `\code{}` in the roxygen blocks.

### F-c2-data-22  [nit] [dead-code]
- Where: R/DTADataSetTabular-class.R:250-257, 277-286, 310, 816; R/DTADataSet-class.R:339-341, 350; R/validationFunctions.R:5-6, 45; R/DTAFileAny-class.R:122-124
- What: Commented-out code, an argument documented as unused, stale imports and an unreachable check.
- Evidence: two commented-out validator blocks (`# if(length(self@tables) > 0 && !all(sapply(...` and the column-name check); `# colspec <- new_generic("colspec", "x") # was already initialized`; `# super(print_short_info, x)`; `@param recursive Unused. ... never read; leave it at its default.`; `@importFrom jsonlite toJSON` and the `dplyr` verb list on `validate_table()`, whose description says nothing is serialised any more; `# TODO: consider moving validate_table() ...` between the roxygen block and the function; `if (!is.null(self@extensions) && !is.character(self@extensions))` can never be TRUE for a `class_character_or_null` property.
- Failure scenario: none at run time; noise that misleads the next reader (`recursive` is part of an exported signature).
- Confidence: verified-by-reading
- Suggested direction: delete them; drop `recursive` from `dta_dataset_from_list()`.

### F-c2-data-23  [minor] [bug]
- Where: R/DTAColumnSpecStructureSAS-class.R:147-151  (cross-slice: the file belongs to c1-spec; it was read to understand the SAS reader)
- What: The first check of the `DTAColumnSpecStructureSAS` validator builds its message and throws it away -- there is no `return()` -- so an empty `backend` is accepted.
- Evidence: `if (is.null(self@backend) || self@backend == "") { "'backend' must be defined and cannot be an empty character." }` followed by further `if` blocks that all use `return(str_glue(...))`; the bare string is the value of a statement, not of the validator.
- Failure scenario: `s <- DTAColumnSpecStructureSAS(type = "Num"); s@backend <- ""` validates; `dta_family_of("", "Num")` is then `NA`, so every family-driven decision (import typing, SAS temporal rendering) treats the column as undeclared.
- Confidence: verified-by-reading (whether the parent class's validator catches the value was not checked)
- Suggested direction: `return()` the message, as the three checks below it do.

### F-c2-data-24  [nit] [inconsistency]
- Where: R/DTADataSetTabular-class.R:15, 120-138, 232-248 vs. R/DTADataSet-class.R:586-591
- What: The constructor documents `tables` as "A named list" but neither it nor the validator requires names; an unnamed list is accepted and the object then fails at first use.
- Evidence: the validator only tests classes (`inherits(x, "Table") || inherits(x, "Dataset") || ...`); `dta_table_id_to_names()` later aborts with `"The tables of dataset {.field {x@id}} have no names."`.
- Failure scenario: `ds <- DTADataSetTabular(id = "d", specs = s, tables = list(df))` succeeds; `check(ds)`, `validation_status(ds)` and `results(ds)` all abort, far from the call that built the object.
- Confidence: verified-by-reading
- Suggested direction: refuse unnamed/duplicate table names in the validator.

## Batch 2 — rule engine (`R/evaluateRules.R`, fully read; cross-checked against `R/columnSpecChecks.R:225-432`, `R/lintDTA.R:360-490`, `R/DTARuleColCondition-class.R`, `R/DTARuleGroupCondition-class.R`, `DTA_YAML_GUIDE.md:608-722`)

### F-c2-data-25  [major] [bug]
- Where: R/evaluateRules.R:851 (comment :809-814); contrast R/columnSpecChecks.R:249-255 and :400-409
- What: The rule operator `pattern` matches a numeric column through `as.character(x)`, i.e. in R's scientific notation, while the column-spec `pattern` check was given `dta_numeric_text()` precisely to avoid that -- so the same regex on the same column gives two verdicts, and the rule's is wrong for round or small numbers.
- Evidence: rule engine: `pattern = grepl(dta_expand_date_pattern_tokens(value), as.character(x), perl = TRUE),`; column-spec engine: `function(u) grepl(pattern, dta_numeric_text(u), perl = TRUE)` with the stated reason `` `as.character(1e9)` is `"1e+09"`, which would fail a ten-digit pattern the value satisfies ``. By the time a rule runs, a `SAS Num`/`R numeric` column is a double (import typing), so `x` is not the file's text.
- Failure scenario: column `DOSE` declared `SAS Num`, file value `100000`; rule `then: {DOSE: {pattern: "^[0-9]+(\\.[0-9]+)?$"}}`. The cell is rendered `"1e+05"`, the regex fails, and the row is reported as a rule violation; `0.00001` (`"1e-05"`) likewise. In an IF clause the same miss silently takes the row OUT of scope, so the rule's THEN is never applied to it (a vacuous pass). The identical pattern declared as the column's own `pattern:` passes. Lint accepts the configuration (only a logical column is refused for `pattern`, R/lintDTA.R:389-398).
- Confidence: verified-by-reading (the two code paths; `as.character(100000)` = `"1e+05"` is base R behaviour, not executed here)
- Suggested direction: render numbers with `dta_numeric_text()` in the rule operator as well (one text rendering for both engines).

### F-c2-data-26  [minor] [bug]
- Where: R/evaluateRules.R:353-360 and :839-845 (`dta_equality_bound`, `dta_condition_mask`); R/lintDTA.R:471-479
- What: `equals`/`not_equals`/`in`/`not_in` normalise the operand only when the COLUMN is numeric; against a text column a numeric (double) operand is left to R's implicit `as.character()`, so a round number never equals its own digits.
- Evidence: `if (!is.numeric(x)) { return(value) }` then `equal = x == value` / `` `in` = x %in% value ``. The function's own roxygen describes the mirrored trap (`1e6 == "1000000"` ... is `FALSE`) and fixes it for numeric columns only. Lint reports a number against a text column as a WARNING about dropped zeros, not as an error.
- Failure scenario: `SITEID` is `SAS Char` and holds `"100000"`. `DTARuleFactory("r", "col_condition", condition = list(SITEID = list(equals = 100000)), then = ...)` (an ordinary R literal, a double) or YAML `equals: 1e5`: `"100000" == 100000` compares against `"1e+05"` and is FALSE for every row, so the IF never applies and the rule passes without having tested anything; as a THEN it flags every row. `in: [100000, 200000]` behaves the same.
- Confidence: verified-by-reading (R's character/double comparison rule; not executed)
- Suggested direction: when the column is text and the operand numeric, render the operand with `dta_numeric_text()` (or refuse it at construction, as an error rather than a lint warning).

### F-c2-data-27  [minor] [inconsistency]
- Where: R/evaluateRules.R:842-845, 1282-1287; doc R/evaluateRules.R:1197-1200 and DTA_YAML_GUIDE.md:638-639, 659-660
- What: `not_in` treats a missing value as satisfying the clause, while `not_equals` leaves it undecided, so the two negations disagree on missing cells -- and `not_in` contradicts the documented "Missing values in THEN count as a violation".
- Evidence: `not_equal = x != value,` (NA for a missing cell) vs. `not_in = !(x %in% value),` (`%in%` never yields NA, so a missing cell gives TRUE); the caller resolves THEN as `(is.na(then_rows) | !then_rows)` and IF through `dta_condition_in_scope()`, which keeps a missing-value NA out of scope.
- Failure scenario: `then: {UNIT: {not_equals: mg}}` reports a row with a blank `UNIT` as a violation; `then: {UNIT: {not_in: [mg]}}` -- the same statement -- passes it. In an IF clause it is the other way round: `condition: {SEX: {not_in: [M]}}` applies the rule to rows with missing `SEX`, `condition: {SEX: {not_equals: M}}` does not. (Symmetrically `in: [A]` is a definite FALSE for a missing cell where `equals: A` is NA; both end as a THEN violation, so only the negations diverge in outcome.)
- Confidence: verified-by-reading
- Suggested direction: decide one missing-value rule per operator family, implement `not_in` as `!(x %in% value)` only where `x` is present (NA otherwise, unless the set names a missing member), and state it in the operator table.

### F-c2-data-28  [major] [design]
- Where: R/validationFunctions.R:237-260, 355-378; R/evaluateRules.R:1448-1450 (and the empty masks of :443-458, :1251-1265); R/DTADataSetTabular-class.R:1895-1897; the package's own warning at R/DTADataSetTabular-class.R:592-597
- What: A table with a correct header and ZERO rows gets `ok = TRUE` on all three axes and is announced as "passed validation", although every value check is reported `not_checked` and every rule passes vacuously.
- Evidence: `settled = if (scanned_rows) NULL else ... c("required", "additionalProperties")` (everything else unsettled) but `columnspec_valid = !has_columnspec_errors`; `if (nrow(df) == 0) { return(list(id = rule@id, valid = TRUE, message = NULL)) }`; `any(violated)` / `sum(...)` over zero rows; then `cli::cli_alert_success("{n_total} {table_word} passed validation")`. `validate_table()`'s roxygen says of `not_checked`: `Neither is a pass.` -- and the writer's comment calls the situation out: `a header-only file still reads as a table, with no rows, and a table with no rows breaks no constraint and so validates perfectly clean. That is worse than leaving no file at all`.
- Failure scenario: a supplier's export dies after writing the header (or delivers the template file): `load_file()` + `check()` report status `validated`, `ok = TRUE`, "1 table passed validation"; a `col_unique` key, a `group_condition` "baseline exists for every subject" and every `nullable: false` column are certified without one value having been looked at. A tabular dataset has no way to demand rows (only `type: matrix` has `rows:` bounds), and the zero-COLUMN case next to it is deliberately `ok = NA` ("VALIDATION PASSED certificate covering ZERO checks").
- Confidence: verified-by-reading for the column-spec summary, `col_range`, `col_condition`, `group_condition` and the console line; inferred for `col_unique` on zero rows (`dta_count_duplicates()` is in R/arrowCompute.R, outside this slice)
- Suggested direction: give a zero-row table its own status (as "unspecified" has) or at least a warning line and an `n_rows` in the result, and offer a minimum row count for tabular datasets.

### F-c2-data-29  [minor] [doc-wrong]
- Where: R/evaluateRules.R:1231 (and :1172-1175) vs. R/DTARuleColCondition-class.R:49-51, 85-87; R/evaluateRules.R:1833-1838; R/evaluateRules.R:1711-1715 vs. :1756-1760
- What: Three statements in the rule functions' help are not true of the code.
- Evidence: (a) `rule_check_col_condition`: `If `@condition` is empty, the `@then` part applies to **all rows**.` -- the class refuses an empty condition in both constructor and validator (`cli_abort("'condition' must be a non-empty list of conditions.")`), so no rule object can reach the `rep(TRUE, nrow(df))` branch. (b) `validate_rules` example: `# Every rule passes, so the call returns quietly.` -- it calls `apply_rules(rules, table)` with the default `verbose = TRUE` and offers no argument to change it, so it prints "Rule '...' passed" and "All rules validated successfully". (c) `apply_rules` `@return`: each result has "elements `id`, `valid`, `message` and `import_errors`" -- the unknown-rule-type branch returns `list(id, valid, message)` with no `import_errors`.
- Failure scenario: a user who wants "THEN for every row" follows the help and gets a constructor error; a script relying on `validate_rules()` being silent on success gets console output; code doing `rbind` over `lapply(results, `[[`, "import_errors")` meets a `NULL`.
- Confidence: verified-by-reading
- Suggested direction: either allow an empty `condition` (the engine already supports it) or delete the sentence; give `validate_rules()` a `verbose` argument; add an empty `import_errors` frame to the unknown-type result.

### F-c2-data-30  [minor] [bug]
- Where: R/evaluateRules.R:244-249 (`dta_numeric_operands`)
- What: For a `Date`/`POSIXct` column the numeric operators hand a character bound to R's own `Ops`, so the bound is parsed by `as.Date()`/`as.POSIXct()` -- for `POSIXct` in the SESSION time zone, and with a base-R error for text that is not a date.
- Evidence: `if (inherits(x, "Date") || inherits(x, "POSIXt")) { return(list(x = x, value = value)) }` with the comment `a character bound is parsed as a date`; lint tells authors to do exactly this ("compare with a quoted ISO value, and only on Date-class data", R/lintDTA.R:373).
- Failure scenario: an in-memory table with a UTC `POSIXct` column and `greater_equal: "2025-01-01"`: on the German dev machine the bound is 2024-12-31 23:00 UTC, on CI (UTC) it is 2025-01-01 00:00 UTC, so rows in that hour pass on one machine and fail on the other. A bound such as `"01.01.2025"` aborts the whole `check()` with R's translated "character string is not in a standard unambiguous format" instead of a rule failure (it is not a `dta_rule_not_applicable`).
- Confidence: inferred (follows from `Ops.POSIXt`/`Ops.Date` semantics; only reachable for typed in-memory tables; not executed)
- Suggested direction: parse the bound explicitly (ISO only, UTC) and turn a non-date bound into a classed rule failure.

### F-c2-data-31  [minor] [code-cheat]
- Where: R/evaluateRules.R:1589-1599 (doc :1391-1393), :1590-1595; R/DTARuleGroupCondition-class.R:124-144  (the validator half is in a c1-spec file)
- What: At evaluation time an unknown constraint `type` means "no violation" and an unknown scope means `"any"`; the class validator, which runs on every property assignment, checks only that `constraints` is a non-empty list.
- Evidence: `} else { constraint_viol[[ci]] <- logical(n_groups) }` ("Constraints of any other type are ignored."); `scope_truth_vec()` does `if (identical(scope, "all")) ... else cond_any_true[[cond_name]]`; validator: `if (is.null(self@constraints) || !is.list(self@constraints) || length(self@constraints) < 1) {...}` and nothing else about them (types, scopes and names are checked only inside the constructor, by `dta_normalize_group_constraints()`).
- Failure scenario: `rule@constraints[[1]]$type <- "requries"` (or `$then_scope <- "All"`) validates; `rule_check_group_condition()` then returns `valid = TRUE` for any data (or silently applies the weaker `any` scope). A rule that can no longer fail is reported as passed.
- Confidence: verified-by-reading
- Suggested direction: abort on an unknown type/scope in the evaluator (it is a programming error there) and re-run the constraint normalisation in the validator.

## Batch 3 — file/matrix datasets, delivery loading, containers, conflicts, formatting helpers

Files fully read in this batch: `R/DTADataSetFile-class.R`, `R/DTADataSetMatrix-class.R`, `R/loadDelivery.R`,
`R/containerFiles.R`, `R/containerFindings.R`, `R/fileNameConflicts.R`, `R/formattingHelpers.R`
(+ `R/DTA-class.R:44-213, 470-543, 1120-1170` for the callers).

### F-c2-data-32  [major] [logic-error]
- Where: R/DTADataSetTabular-class.R:1066-1108 (`dta_tabular_file_count_finding`); caller R/DTA-class.R:1141-1155; contrast R/DTADataSetFile-class.R:297-362
- What: For a tabular dataset the declared file counts -- which are per file ENTRY -- are checked only as one total (sum of minimums .. sum of maximums against the number of tables), so one entry's surplus hides another entry's missing file.
- Evidence: `n <- length(ds@tables)`, `lo <- min_number_of_files(ds)`, `hi <- max_number_of_files(ds)` (both sums over `x@files`), `if (n >= lo && n <= hi) { return(NULL) }`. The file dataset counts per entry (`for (h_idx in seq_along(x@files)) { ... counts <- sum(routed) ... }`), and a tabular table keeps no record of the entry it came from (provenance holds `file`, `container`, `via` only).
- Failure scenario: a tabular dataset with entries `^lab_chem_.*\.csv$` and `^lab_hema_.*\.csv$` (each exactly one file). The delivery holds `lab_chem_a.csv` and `lab_chem_b.csv` and no haematology file. Two tables are bound, `2 >= 2 && 2 <= 2`, no `dta_file_count_mismatch`; if both tables validate, `check(dta)` passes a delivery that lacks a required file and carries a duplicate. (`load_delivery()`'s "Not in the delivery" line only covers entries whose declared container was opened, and is a load-time message, not part of the verdict.)
- Confidence: verified-by-reading
- Suggested direction: record the entry index per bound table (as `@file_handlers` does for file datasets) and judge each entry's bounds separately.

### F-c2-data-33  [minor] [bug]
- Where: R/DTADataSetMatrix-class.R:751-788 (header read at :773); guards it bypasses: R/DTAFile-class.R:623-666
- What: The matrix `load_file()` reads and judges the file's header BEFORE the name check, the existence check and the `.zip` refusal that `read_file()`/`open_file()` apply, so the wrong error wins.
- Evidence: `head <- dta_matrix_read_head(handler, file)` then `dta_matrix_gate(head, x, file)` come first; `dta_load_tabular()` (and with it `dta_check_readable_file()`: "does not match the filename or pattern", "File ... cannot be found.") runs only afterwards. The guards this method does run first (stale object, handler index, single path, directory, `name`, container) do not include those three.
- Failure scenario: `load_file(matrix_ds, file = "counts_typo.tsv")` for a path that does not exist ends in base R's `cannot open the connection` (translated, unclassed) from `file(path, open = "rb")` in `dta_head_records()` instead of the package's "cannot be found". A file dropped into the wrong slot is first read, then refused with a header-shape class (`dta_matrix_header_*`, `dta_matrix_delimiter`) rather than with the name mismatch every other dataset type reports.
- Confidence: verified-by-reading
- Suggested direction: run `dta_check_readable_file()` (namecheck included) before `dta_matrix_read_head()`.

### F-c2-data-34  [minor] [code-cheat]
- Where: R/DTADataSetMatrix-class.R:718-731 (`dta_table_structure`)
- What: Any error while re-reading the header of a lazily held matrix file is swallowed and reported as "the file changed since load_file()".
- Evidence: `handler <- tryCatch(x@files[[head$handler]], error = function(e) NULL)`; `now <- ... tryCatch(dta_matrix_read_head(handler, sources[[1L]]), error = function(e) NULL)`; `if (is.null(now) || !identical(now$fields, head$fields)) { return(fail("has a header that differs from the one recorded: the file changed since load_file(); load it again.")) }`.
- Failure scenario: the file is unchanged but the read fails for another reason -- a share that is briefly unreachable, an `encoding` the platform cannot convert (`dta_encoding_unsupported`), a header over the field limit, a handler index that no longer exists after `@files` was edited: the table is recorded as failed with the advice to load it again, which cannot help, and the real condition is gone.
- Confidence: verified-by-reading
- Suggested direction: let the classed error through as the failure text (or distinguish "could not be re-read: <reason>" from "differs").

### F-c2-data-35  [minor] [bug]
- Where: R/DTADataSetFile-class.R:1040-1106 (`messages`), :1155-1162 (`inspect`), :995-1030 (`validation_errors`)
- What: For a file dataset `messages()` and `inspect()` accept `source = c("auto", "memory", "artifact")` and never use it; only the in-memory store is read.
- Evidence: `source <- match.arg(source)` is the only mention in `messages()`; every target is answered from `details <- x@validation_store[[table_name]]; if (is.null(details)) { return(dta_empty_messages()) }`. `validation_errors()` here repeats the tabular defect of F-c2-data-14 (`"memory"` falls through to `readRDS(entry$artifact_path)`).
- Failure scenario: an object whose `@validation_store` is empty but whose index and artifacts exist (the state `source = "artifact"` is for): `messages(ds, source = "artifact")` returns zero rows for a target the index records as `ok = FALSE` -- a failed file reads as having nothing to report.
- Confidence: verified-by-reading
- Suggested direction: route `messages()` through `validation_errors(x, table, source)` or drop the argument.

### F-c2-data-36  [nit] [inconsistency]
- Where: R/DTADataSetFile-class.R:1109-1152, :972-976; doc R/DTADataSetTabular-class.R:1178-1187; R/DTADataSet-class.R:497-506
- What: The file-dataset reporting methods do not keep the contracts the shared help states.
- Evidence: (a) `validation_status()` resolves targets against `names(x@validation_index)`, so the `if (is.null(entry)) { return(dta_file_empty_status_row(table_name)) }` branch is unreachable: a file dataset with bound but unchecked files returns ZERO rows, never the documented `"not_validated"` row "per file". (b) `summary_df <- do.call(rbind, output_rows); attr(x, "last_validation_summary") <- summary_df` assigns `NULL` (removing the attribute) when there is no target, while the tabular method guarantees "a zero-row FRAME, never NULL". (c) `tables()` is registered for `DTADataSet` as `x@tables`, a property a `DTADataSetFile` does not have, so `tables(file_ds)` ends in S7's plain "Can't find property" error although the help says "Method to get tables from DTADataSet object".
- Failure scenario: `nrow(validation_status(load_file(file_ds, f)))` is 0 before the first `check()` (the tabular twin gives one `not_validated` row); `nrow(attr(check(empty_file_ds, quiet = TRUE), "last_validation_summary"))` is `NULL`, not 0.
- Confidence: verified-by-reading
- Suggested direction: resolve against the dataset's targets, always attach a frame, and register `tables()` on the tabular class only.

### F-c2-data-37  [minor] [dead-code]
- Where: R/DTADataSetFile-class.R:62-86 (doc :11-13)
- What: The file-dataset constructor silently rebuilds every reader handler as a `DTAFileAny` for the sake of documents written "before this constraint existed", reading each property through a `tryCatch(..., error = function(e) NULL)` that no `DTAFile` can trigger.
- Evidence: `# The fix is COERCION, not rejection: an existing document that named a reader type before this constraint existed must keep loading`; `filename = tryCatch(h@filename, error = function(e) NULL)` ... `info = tryCatch(h@info, error = function(e) NULL)` with the reason "a bare DTAFile lacks some of what a reader subclass would carry" -- but all seven properties read here (`filename`, `pattern`, `pattern_description`, both bounds, `info`, `container`) are defined on the base `DTAFile`; the only input the `tryCatch` changes anything for is an element that is not a `DTAFile` at all, whose real problem it turns into S7's "@filename must be <character>, not <NULL>".
- Failure scenario: `type: file` with `files: {type: csv, filename: ae.csv}` is accepted and the CSV is never parsed -- only checked for existence -- with no finding telling the author that `type: csv` has no effect there; the handler's `sep`/`encoding`/`missing_values` vanish on the way. A compatibility shim (priority 4) that also masks a specification mistake.
- Confidence: verified-by-reading
- Suggested direction: refuse a non-`any` handler in a file dataset (as the tabular validator refuses a reader-less one) and delete the coercion.

### F-c2-data-38  [minor] [doc-missing]
- Where: R/loadDelivery.R:301-313; doc :104-107, :122-132
- What: `load_delivery()` aborts with class `dta_delivery_nothing_loaded` whenever no file could be bound -- also under `on_error = "skip"` -- and neither the `on_error` text nor `@return` mentions it.
- Evidence: `if (!any(plan$status == "loaded")) { ...; cli::cli_abort(c("Nothing in {.file {delivered}} could be loaded into the specification.", lines), class = "dta_delivery_nothing_loaded", plan = plan) }` is unconditional; the help for `"skip"` reads `binds what can be bound, lists the rest as `refused` and warns once`.
- Failure scenario: a caller that chose `on_error = "skip"` to always get the `last_delivery` plan back (to show the user why files were refused) gets an error instead for exactly the delivery where every file was refused or unused; the plan is only reachable as a field of the condition, which is undocumented.
- Confidence: verified-by-reading
- Suggested direction: document the class and its `plan` field (or return `x` with the plan in skip mode).

### F-c2-data-39  [minor] [security]
- Where: R/loadDelivery.R:253-268 vs. :566-597 and R/DTAFileSAS7BDAT-class.R:202-203
- What: In `on_error = "abort"` mode a load failure is re-raised with `parent = res`, which chains the original condition -- whose text names the extracted file by its full server path -- to an error that the skip-mode path (`dta_delivery_reason()`) and the SAS readers take care to scrub.
- Evidence: `cli::cli_abort(c("Could not load {.file {file_shown}} into dataset {.field {dataset}}.", ...), class = "dta_delivery_load_error", parent = res, ...)`; elsewhere: `# No `parent`: it would print haven's text a second time, with the full server path.` and `so no server path reaches a report or a browser`.
- Failure scenario: a member extracted to `<exdir>/delivery-xxxx/1-sdtm.zip/e00003/ae.csv` fails to read; `dta_check_readable_file()`/Arrow report `File '<full path>' ...`; the abort shown to the user carries that as "Caused by error ...". In the app the path is a server path.
- Confidence: inferred (the chain and the readers' messages were read; how the app renders the condition was not checked)
- Suggested direction: put `dta_delivery_reason(res, b$cand)` in an `x` bullet and drop `parent`, as the other sites do.

### F-c2-data-40  [minor] [design]
- Where: R/DTAFileTabular-class.R:1631-1640 and :1385-1394 (`ignore_empty_lines = TRUE`); R/importConversion.R:656-663; R/validationFunctions.R:322-328
- What: The `row` every finding reports is the ordinal of the non-empty data record, not a line of the file, and nothing in the help says so; empty lines are dropped by the reader without a trace.
- Evidence: both parse-option blocks hard-code `ignore_empty_lines = TRUE`; issues are recorded as `row = as.integer(kept)` (an index into the table as read) and printed as `"Row {preview$row[i]}, column '{preview$column[i]}': value ..."`. Neither `DTAFileTabular`'s roxygen nor the README/guide defines `row` or mentions that blank lines are skipped.
- Failure scenario: a CSV with a header and a blank line 5: the defect in file line 10 is reported as "Row 8"; the supplier opens line 8 (or 9) and finds a clean record. In a ONE-column file an empty line IS a row with a missing value (Arrow's own definition of the option), so a `nullable: false` list of subject ids with gaps loses those rows instead of failing them, and the row count shrinks.
- Confidence: verified-by-reading for the options and the row arithmetic; inferred for Arrow's treatment of an empty line in a one-column file (from Arrow's documentation of `ignore_empty_lines`)
- Suggested direction: define `row` in the help ("n-th data record; header and blank lines not counted") and report or count the dropped lines.

### F-c2-data-41  [minor] [dead-code]
- Where: R/formattingHelpers.R:246-297 (`.create_heading_style`, `.create_body_style`, `.create_table_header_style`, `.create_table_cell_style`, `.create_border`); also :18 (`THEME_COLORS$accent`)
- What: Five style helpers have no caller anywhere (R/, inst/, tests/, vignettes/), and two of them cannot run.
- Evidence: grep finds only the five definitions. `.create_heading_style()` and `.create_body_style()` call `officer::fp_text(font.name = FONTS$primary, ...)`; the argument is `font.family` (as `.house_fp_text()` twenty lines above uses), and `fp_text()` has no `...`. `.create_border(pos = "all", ...)` never reads `pos`.
- Failure scenario: none today; the first caller of `.create_heading_style()` gets "unused argument (font.name = ...)".
- Confidence: verified-by-reading for the absence of callers; inferred for `officer::fp_text()`'s signature (officer's source is not in the snapshot)
- Suggested direction: delete the five functions and the unused palette entry.

### F-c2-data-42  [minor] [bug]
- Where: R/formattingHelpers.R:336-371 (`.format_scalar_value`); pinned by tests/testthat/test-formattingHelpers.R:27-33
- What: The value formatter of the exported agreement renders a logical vector of length > 1 as "No" whatever it holds, and renders round doubles in scientific notation.
- Evidence: `if (is.logical(val)) { return(if (isTRUE(val)) "Yes" else "No") }` runs before the `length(val) > 1` collapse, and `isTRUE(c(TRUE, TRUE))` is FALSE; numbers go through `val <- as.character(val)`. The test asserts the latter as intended: `# ... a value that a DTA author typed as 1000000000000000 is shown as "1e+15" in the exported agreement.` / `expect_equal(.format_scalar_value(1e15), "1e+15")`.
- Failure scenario: a transmission field `[true, true]` (YAML gives `c(TRUE, TRUE)`) is printed as "No" in the Word/Markdown document; an extension field `expected_records: 2500000000` (or `100000.0`) is printed as "2.5e+09" / "1e+05" in a document meant to be signed. The test would fail a fix rather than catch the defect (it is not marked `KNOWN DEFECT, pinned rather than endorsed`).
- Confidence: verified-by-reading
- Suggested direction: collapse vectors before the logical branch (`ifelse(val, "Yes", "No")`), render numbers with `dta_numeric_text()`, and re-point the test.

### F-c2-data-43  [nit] [doc-wrong]
- Where: R/formattingHelpers.R:1-25 -> man/THEME_COLORS.Rd; R/loadDelivery.R:869-870 with R/containerFiles.R:192-197; R/containerFiles.R:458-468, 684-695; R/loadDelivery.R:222, 604-627 with R/DTADataSetTabular-class.R:1003-1010
- What: Small inaccuracies and loose ends in the container/formatting code.
- Evidence: (a) the file-header roxygen block (`@title Professional Formatting Helpers for Document Export`, `@description Utility functions ...`) attaches to the next object, so the package ships `man/THEME_COLORS.Rd` titled "Professional Formatting Helpers for Document Export" with usage `THEME_COLORS`. (b) inside a zip every dot-file is `ignored` ("is a system file") before claims are computed, so an entry a specification names (`.manifest.json`) can be delivered loose but never in an archive. (c) `dta_gzip_trailer_ok()` opens a connection without `on.exit(close())`, and `dta_xpt_split()` leaves `oc` open when `writeBin()` fails (disk full). (d) `dta_delivery_check_inflated()` duplicates `dta_check_inflated_size()`, which `dta_load_tabular()` now runs "for every caller of load_file()": a gzip file near the cap is inflated twice before it is read.
- Failure scenario: `?THEME_COLORS` describes "utility functions"; a declared dot-file inside `delivery.zip` gets the row "is a system file" and, when its entry declares that archive, is listed as "Not in the delivery"; a failed split leaks a connection until gc; a large `.csv.gz` under a tight cap costs two full decompression passes.
- Confidence: verified-by-reading
- Suggested direction: move the header text into a plain comment (or `@noRd`), apply the ignore rule after claims, add `on.exit()`, and delete `dta_delivery_check_inflated()`.

## Batch 4 — second pass over the file-entry semantics (cross-file traces)

### F-c2-data-44  [major] [logic-error]
- Where: R/DTADataSetFile-class.R:240-260 (comment :206-217) with R/DTAFile-class.R:343-356; R/DTAFileAny-class.R:37-39, 227-239; R/DTADataSetTabular-class.R:1079-1088. Tests that claim the case: tests/testthat/test-DTADataSetFile.R:492-562, tests/testthat/test-DTAFile.R:1382-1394
- What: For a LITERAL entry that lists several file names, one delivered name satisfies the whole entry in a file dataset -- the still-missing names are never reported -- because the "one logical per declared name" the code relies on exists only for `pattern = TRUE`.
- Evidence: `dta_matches_filename_base()`: `if (x@pattern) { Reduce(`|`, lapply(candidates, ...)) } else { any(candidates %in% x@filename) }` -- ONE logical for a literal handler. `dta_file_dataset_targets()`: `matched <- Reduce(`|`, lapply(paths, function(p) { m <- matches_filename(handler, basename(p)); ... })); names_i[!matched]` -- a length-1 `!matched` is recycled over `names_i`, so `TRUE` drops every name. The comment above it says the opposite was fixed: `a handler declaring two names had the one delivered name hide the other, still-missing one from the report`, and the class doc says `matches_filename()` returns "one logical per declared name or pattern, so a handler carrying several is not collapsed to a single verdict".
- Failure scenario: `ds <- DTADataSetFile(id = "d", files = list(DTAFileAny(filename = c("report.pdf", "listing.pdf"))))` (what YAML `filename: [report.pdf, listing.pdf]` builds; literal, count forced to 1). Deliver only `report.pdf`: `check()` reports "1 file passed validation" and `listing.pdf` appears nowhere. In a TABULAR dataset the same entry means something else again: one of the two files passes, both delivered fail the run with "expects 1 file(s), got 2". Mutation the tests do not catch: every test named for this case ("does not crash on a handler declaring several file names", "still reports the undelivered name when only one of a handler's two names arrives", and "matches_filename accepts any of a handler's names", whose comment says it covers "the `%in%` branch") constructs the handler with `pattern = TRUE`, so the literal branch is never exercised.
- Confidence: verified-by-reading (the three functions and the cited tests; not executed)
- Suggested direction: return one logical per name for a literal handler as well (`x@filename %in% candidates`), decide and document what several literal names mean (all of them), and add the `pattern = FALSE` twin of each test.

### F-c2-data-45  [minor] [design]
- Where: R/DTAFileDelim-class.R:1-99 (exported, documented); R/00_helpers.R:494, 936-953 (`__DTAtools_supported_file_types__`, `DTAFileFactory`); R/DTAFile-class.R:50-55; inst/shiny/dta_app/R/utils_dta.R:3168-3175, 3192
- What: `DTAFileDelim` -- the only handler with a caller-supplied separator -- cannot be produced from a specification: the factory has no branch for it, so no YAML can declare, for instance, a semicolon-separated file, and the app writes one back as `type: csv`.
- Evidence: `` `__DTAtools_supported_file_types__` <- c("any", "csv", "tsv", "sas7bdat", "xpt") `` and the `switch(type, csv = ..., tsv = ..., any = ..., sas7bdat = ..., xpt = ..., cli_abort(...))`; the app's own comment: `# KNOWN GAP, not reachable from the app: a DTAFileDelim reports "csv", which discards its `sep`. ... the real fix is a `delim` branch in DTAFileFactory`. The class help nevertheless recommends it ("prefer a concrete subclass -- DTAFileCSV, DTAFileTSV, DTAFileDelim for a delimited file that is read").
- Failure scenario: a supplier delivers `;`-separated CSV (the default of a German-locale Excel). No `files:` entry can describe it; `type: csv` reads every row as one column and reports every declared column as missing. A DTA built in R with `DTAFileDelim(sep = ";")` and saved through the app comes back as a comma CSV handler -- the separator is silently lost.
- Confidence: verified-by-reading (factory, class, and the app comment; the app's write path beyond `dta_handler_type()` was not traced)
- Suggested direction: add `type: delim` with `sep:` to the factory, lint and writer (or remove the class from the exported API until then).

### F-c2-data-46  [nit] [doc-wrong]
- Where: R/DTAFileSAS7BDAT-class.R:72-73 vs. R/DTADataSetTabular-class.R:931-951; R/DTADataSetTabular-class.R:1388-1456 (`validation_run`); R/DTAFileTabular-class.R:219, 239
- What: Three smaller help-text inaccuracies in the reader/dataset classes.
- Evidence: (a) `DTAFileSAS7BDAT`: `\code{load_file()} with stream = "auto" reads it whatever its size, and says so with a message of class dta_sas7bdat_in_memory` -- `dta_load_is_lazy()` emits the message only `if (lazy && ...)`, i.e. only when the size alone would have streamed the file (as `?load_file` correctly says). (b) the `check()` block for `DTADataSetTabular` (and the base and file blocks) never documents the user-facing argument `validation_run`, which all three methods take. (c) `DTAFileTabular` is called "a virtual class" in two method blocks; it is a concrete, constructible class (see F-c2-data-18).
- Failure scenario: a user waits for the in-memory message on a small SAS file to confirm it was not streamed; `check(ds, validation_run = "r1")` works but cannot be found in the help.
- Confidence: verified-by-reading
- Suggested direction: align the three sentences with the code.

## Addenda (confidence updates after reading the callers)

- F-c2-data-15: the part marked inferred is now verified -- `dta_validate_any_table()` sends every non-lazy table to `validate_table_detailed(specs, as.data.frame(table), ...)` (R/streamingValidation.R:2415-2424), which reads the carried `dta_import_issues` attribute.
- F-c2-data-28: the part marked inferred is now verified -- `dta_count_duplicates()` ends in `sum(duplicated(...), na.rm = TRUE)` (R/arrowCompute.R:133-139), which is 0 for zero rows, so `col_unique` also passes vacuously.
- F-c2-data-14 also applies verbatim to the file-dataset method, R/DTADataSetFile-class.R:995-1030.
- F-c2-data-06: `R/DTADataSetFile-class.R:655-659` ("A restored legacy object may hand back NULL or a vector of another length") and `R/loadDelivery.R:159-160` are further sites of the same compatibility layer.

## Batch 5 — late additions (rule engine, S7 constructors)

### F-c2-data-47  [minor] [security]
- Where: R/evaluateRules.R:851 (sibling in a c1-spec file: R/columnSpecChecks.R:249-255); the bounded matcher: R/00_helpers.R:956-1015
- What: The regex-DoS guard the package built (`dta_str_detect()`: time and stack limit, a classed `dta_regex_limit` warning, a cap on trips) protects only FILE-NAME patterns; the patterns that run over every supplied data value -- the rule operator `pattern` and the column `pattern` -- go through bare `grepl(perl = TRUE)`.
- Evidence: `pattern = grepl(dta_expand_date_pattern_tokens(value), as.character(x), perl = TRUE),` against the helper's own rationale: `` `^(a+)+$` against a short name that ends in `!` keeps the R process busy for as long as the name is long. This is the one place those patterns are matched, with both limits on. `` -- "those patterns" lists "a file entry with `pattern: true`, a container glob" only.
- Failure scenario: a specification with an innocently nested pattern (`^([A-Z]+ ?)+$`) and a supplier column whose values end in an unexpected character: every such value is matched up to PCRE's own match limit, and a value that hits the limit is answered `FALSE` with a base-R warning -- i.e. reported as a pattern violation (or, in an IF clause, silently out of scope) with no classed condition; over millions of rows the scan does not finish in useful time. The data comes from the other party, the file names mostly from the same specification author -- the unprotected side is the exposed one.
- Confidence: inferred (the call sites were read; PCRE's limit behaviour inside `grepl()` is R's documented behaviour and was not measured)
- Suggested direction: route value patterns through one bounded matcher as well, and decide explicitly what a timed-out value means (not "no match").

### F-c2-data-48  [minor] [code-cheat]
- Where: R/evaluateRules.R:251-255 (`dta_numeric_operands`), :724-755 (`dta_check_condition_arity`)
- What: A numeric operator's bound that is not a number is silently turned into `NA` under `suppressWarnings()` at evaluation time; nothing in the rule constructors refuses it.
- Evidence: `bound <- if (is.character(value) || is.factor(value)) { suppressWarnings(as.numeric(as.character(value))) } else { value }`; the arity check only counts values and rejects `NA` (`if (n != 1L || missing) "exactly one non-missing value"`), and `"abc"` is neither. Only the collection-level lint has the test ("needs a numeric bound", R/lintDTA.R:376-382).
- Failure scenario: `rule <- DTARuleColCondition("r", condition = list(AGE = list(greater = "eighteen")), then = list(CONSENT = list(equals = "Y")))` constructs; `rule_check_col_condition(rule, df)` (exported) compares every `AGE` with `NA`, the IF is undecided for every row, `dta_condition_in_scope()` resolves "undecided but convertible" to out of scope, and the rule returns `valid = TRUE` for any data. The same bound in a THEN flags every in-scope row instead. Either way the typo is never named.
- Confidence: verified-by-reading
- Suggested direction: validate numeric operands in `dta_check_condition_arity()` (it already runs on every construction path) and abort with a classed error.

### F-c2-data-49  [nit] [dead-code]
- Where: R/evaluateRules.R:416-426 (doc), :490-497 (`dta_range_target`); R/DTARuleColRange-class.R:166-175
- What: `rule_check_range()` documents, and `dta_range_target()` implements, a `@range` spelling of the bounds that no rule object can carry.
- Evidence: `range <- rule_get_slot(rule, "range")` reads `as.list(rule)`, and the only `as.list` method for a range rule returns `id, type, description, columns, min, max` -- never `range`; the roxygen still says the bounds are "spelled either `@min` and `@max` ... or a `@range` numeric(2)".
- Failure scenario: none at run time; the first branch is unreachable and the help describes a slot that does not exist.
- Confidence: verified-by-reading
- Suggested direction: read `rule@min`/`rule@max` directly and drop the sentence.

### F-c2-data-50  [major] [inconsistency]
- Where: R/DTAFileTabular-class.R:122-123, R/DTAFileSAS7BDAT-class.R:142-143, R/DTAFileXPT-class.R:85-86, R/DTAFileAny-class.R:104-105, R/DTADataSetTabular-class.R:151-152, R/DTADataSetFile-class.R:88-89 (named `.parent =`) vs. R/DTAFile-class.R:120-121, R/DTADataSet-class.R:61-62, R/DTAFileCSV-class.R:76-77, R/DTAFileTSV-class.R:73-74, R/DTAFileDelim-class.R:80-81, R/DTADataSetMatrix-class.R:191-192 (positional); DESCRIPTION:45; .github/workflows/R-CMD-check.yaml:135 (and qualification.yml:94, r-style.yaml:67, release.yml:33, test-coverage.yaml:79, 166); renv.lock:190-192
- What: Six constructors of this slice pass the parent object to `S7::new_object()` by the argument NAME `.parent`, six pass it positionally, and the package is only ever built and tested against S7 0.2.2 although `DESCRIPTION` admits every later version.
- Evidence: `new_object(.parent = DTAFile(...), sep = sep, ...)` beside `new_object(DTAFileTabular(...))`; `Depends: ... S7 (>= 0.2.2)`; every workflow installs `cran::S7@0.2.2` and `renv.lock` pins `"Version": "0.2.2"` (CHANGELOG.md:5224: "pinned S7 dependency to version 0.2.2 to ensure consistent package behavior").
- Failure scenario: a user installs DTAtools from scratch and gets whatever S7 is current; if that release names the first argument of `new_object()` differently, every `DTAFileCSV()`, `DTAFileAny()`, `DTADataSetTabular()` and `DTADataSetFile()` call fails at construction -- and no CI leg, qualification run or test would have seen it, because all of them pin the old S7. The positional spelling used by the other six constructors does not depend on the name.
- Confidence: verified-by-reading for the twelve call sites, the DESCRIPTION range and the pins; inferred for the consequence under a newer S7 (no S7 source is in the snapshot and nothing was run)
- Suggested direction: pass the parent positionally everywhere, and add one CI leg on the newest S7 (or declare the tested upper bound).

### F-c2-data-51  [nit] [doc-missing]
- Where: R/DTAFileTabular-class.R:56, 70, 516, 526, 561-566; R/DTAFile-class.R:866; code R/00_helpers.R:1019-1029
- What: The help says the UTF-8 copy of a non-UTF-8 file is written "under `tempdir()`", but it is written to `dta_temp_dir()`, which follows the option `DTAtools.temp_dir` -- an option no help page, README or vignette mentions.
- Evidence: `destination <- tempfile("dta_transcode_", tmpdir = dta_temp_dir(), fileext = ".csv")` with `d <- getOption("DTAtools.temp_dir"); if (is.character(d) && ... && dir.exists(d)) d else tempdir()`; roxygen: `to a UTF-8 copy under \code{\link[base]{tempdir}()} that the scan reads` and `@return The path of a UTF-8 copy under `tempdir()``. A repository-wide grep finds `DTAtools.temp_dir` only in that helper's comment and in the app.
- Failure scenario: an operator sizing free disk for a 60 GB latin1 delivery provisions `tempdir()` as the help says, while the host (the app does this) has pointed the option elsewhere; a user who needs the copies on a larger volume cannot learn from the documentation that an option for it exists. An option naming a directory that does not exist is silently ignored.
- Confidence: verified-by-reading
- Suggested direction: document the option beside the other `DTAtools.*` options and say "the package's temporary directory" in the three help texts.

## Coverage

Files of the slice, all read completely (every line):

| File | Lines | Status |
| --- | --- | --- |
| R/DTADataSet-class.R | 705 | fully read |
| R/DTADataSetFile-class.R | 1200 | fully read |
| R/DTADataSetMatrix-class.R | 788 | fully read |
| R/DTADataSetTabular-class.R | 1903 | fully read |
| R/DTAFile-class.R | 1062 | fully read |
| R/DTAFileAny-class.R | 300 | fully read |
| R/DTAFileCSV-class.R | 191 | fully read |
| R/DTAFileDelim-class.R | 169 | fully read |
| R/DTAFileSAS7BDAT-class.R | 361 | fully read |
| R/DTAFileTSV-class.R | 190 | fully read |
| R/DTAFileTabular-class.R | 2018 | fully read |
| R/DTAFileXPT-class.R | 210 | fully read |
| R/containerAssignments.R | 253 | fully read |
| R/containerFiles.R | 791 | fully read |
| R/containerFindings.R | 168 | fully read |
| R/datePatternTokens.R | 169 | fully read |
| R/evaluateRules.R | 1881 | fully read |
| R/fileNameConflicts.R | 510 | fully read |
| R/formattingHelpers.R | 532 | fully read |
| R/importConversion.R | 1107 | fully read |
| R/loadDelivery.R | 1064 | fully read |
| R/rTemporalFormat.R | 404 | fully read |
| R/sasTemporalText.R | 157 | fully read |
| R/validationFunctions.R | 785 | fully read |

Read outside the slice to trace callers and contracts (fully unless a range is given): `R/00_helpers.R`,
`R/DTAColumnSpecStructureSAS-class.R`, `NAMESPACE`, `DESCRIPTION`; `R/DTA-class.R:44-213, 470-543, 1120-1170`;
`R/columnSpecChecks.R:225-445`; `R/lintDTA.R:360-550`; `R/DTARuleColCondition-class.R:40-155`;
`R/DTARuleGroupCondition-class.R:60-300`; `R/streamingValidation.R:2255-2430`; `R/arrowCompute.R:40-160`;
`inst/shiny/dta_app/R/utils_dta.R:3160-3200`; `DTA_YAML_GUIDE.md:540-770`; the `as.list` methods of the four
rule classes; `.github/workflows/R-CMD-check.yaml:108-140` and the S7 pins of the other workflows (grep),
`renv.lock:190-196`, `CHANGELOG.md:804, 5224`;
`inst/extdata/template-repo-skeleton/README.md:385-440`; generated help `man/check.Rd`, `man/load_file.Rd`,
`man/read_file_execution.Rd`, `man/open_file_execution.Rd`, `man/print_info.Rd`, `man/matches_filename.Rd`,
`man/max_number_of_files.Rd`, `man/min_number_of_files.Rd`, `man/files.Rd`, `man/tables.Rd`,
`man/validation_status.Rd` (head), `man/validation_errors.Rd` (head), `man/print_short_info.Rd` (head),
`man/print.Rd` (head), `man/specs-DTADataSetTabular.Rd`, `man/THEME_COLORS.Rd`; tests
`tests/testthat/test-DTADataSetFile.R:480-575`, `test-DTAFile.R:1300-1410`, `test-formattingHelpers.R:1-75`.

Only skimmed / not read: the rest of `man/` (about 470 Rd files) was not compared line by line with its roxygen
source -- the pages listed above were; the tests of this slice belong to other reviewers and were opened only
where a finding needed them. Not verifiable by reading (marked `inferred` in the findings): the behaviour of
Arrow, haven/ReadStat, the zip library, officer and PCRE where a finding depends on it; nothing was executed.

Things checked and found sound (no finding): zip entry-name screening and extraction (no traversal: absolute
paths, drive letters, `.`/`..`, symlinks, encrypted and case-duplicate entries are refused before extraction;
every entry is written into a fresh directory); XPORT member scanning/splitting and the row-count restoration;
the date-mask tokeniser; the R temporal format parser/regex/renderer and the SAS temporal renderer (pure
arithmetic, no `strptime()`, no locale); strict numeric/logical conversion and the import-issue counting (exact
counts, capped retention); cli usage (user text is interpolated or passed through `.cli_escape()` at every
call site read); no `try()`, no `is_testing()`/`NOT_CRAN`/environment-variable special-casing in the slice;
every `suppressWarnings()` in the slice wraps an `as.numeric()`/`as.integer()` whose `NA` is handled next
(except F-c2-data-48).
