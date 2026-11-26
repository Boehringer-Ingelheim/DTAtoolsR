# Findings — c3-stream

Slice (paths relative to $SNAP):
- R/arrowCompute.R
- R/benchmarking.R
- R/reportRendering.R
- R/rulePhrasing.R
- R/streamingScale.R
- R/streamingValidation.R
- R/validationReporting.R

Files fully read: all seven (see Coverage at the end). Findings F-c3-stream-01 .. 34.

## Area 1 — streaming driver, sinks, structural gate (R/streamingValidation.R), precompute (R/streamingScale.R), inspect/messages (R/validationReporting.R)

### F-c3-stream-01  [major] [logic-error]
- Where: R/streamingValidation.R:1028-1033, 1192, 1258 (vs R/validationFunctions.R:112-116, 194-233, 352)
- What: The streaming driver builds `summarised_error` from the error sink's *capped in-memory head*, so once more than `max_errors` column-spec errors occur the summary under-counts and omits groups, while the eager path builds it from the complete frame and documents (and tests) that it is cap-invariant.
- Evidence: streaming: `full_error <- dta_error_sink_collect(columnspec_sink)` (1192; the collected frame holds at most `max_errors` rows, the rest are spilled) then `summarised_error <- dta_summarise_columnspec_errors(full_error)` (1258). The comment at 1029-1030 claims "the summary is recomputed once at the end over the whole collected frame". Eager: "Only RETENTION is capped: ... `summarised_error` and the per-check breakdown are all computed from the complete frames" (validationFunctions.R:112-114) and truncation happens last (352). `dta_summarise_columnspec_errors()` emits `n.rows.affected = dplyr::n()`, `first/last.row.affected` per (column, keyword, message, columnspec, data) group (columnSpecChecks.R:799-806), all of which depend on how many rows it is given.
- Failure scenario: default `max_errors = 10000`; a streamed file has 12,000 `maxLength` violations in column ID (rows 1-12,000) and one `pattern` violation in column CODE at row 15,000. Streaming `summarised_error`: one group, ID/maxLength, `n.rows.affected = 10000`, `last.row.affected = 10000`; the CODE/pattern finding is absent. Eager (same file, in memory, same cap): ID n=12,000 last=12,000 plus the CODE row. No attribute on the summary flags it as partial. The parity tests cannot see this: `vs_schema_stream()` defaults `max_errors = NULL` (tests/testthat/test-streaming-validation.R:21-31, 48-73), and the cap-invariance test exists only for the eager path (tests/testthat/test-validationFunctions.R:298-301).
- Confidence: verified-by-reading
- Suggested direction: accumulate the summary (count/min/max per group key) per batch alongside `columnspec_tally`, or recompute it from head + spill; add a capped streaming-vs-eager parity assertion.

### F-c3-stream-02  [major] [bug]
- Where: R/validationReporting.R:1250-1298 (dta_inspect_table_frame), 1414-1442, 1479-1487 (dta_inspect_tabular_message); cf. R/DTADataSetTabular-class.R:80-91, R/streamingValidation.R:1000-1009
- What: For a lazily held (streamed) table, `inspect()` / `explain_messages()` recompute a rule's failing rows from the raw, *untyped* text of the Dataset, while `check()` evaluated the rule on batches typed by the specs, so the failing rows shown contradict the verdict for any declared-numeric/logical column.
- Evidence: lazy tables are stored "UNTOUCHED: neither coerced nor collected" and typed only "per batch, while scanning" (DTADataSetTabular-class.R:80-90); the scan calls `dta_coerce_table_to_specs()` before the rules (streamingValidation.R:1004-1008). `dta_inspect_table_frame()` returns `as.data.frame(reader$read_table())` of the raw Scanner (1275-1277) with no typing, and `failing_rows <- dta_rule_failure_row_indices(rule_def, table_df)` (1442) runs on it.
- Failure scenario: AGE declared `SAS Num`, rule `col_range AGE [18, 65]`, lazily loaded CSV with AGE = "30","abc","70". `check()`: "abc" is typed to NA (import error), the rule counts 1 violation (row 3). `inspect()`: `dta_as_numeric_strict("abc")` is unconvertible on raw text, so `failing_row_count = 2` (rows 2, 3). Conversely a `col_unique` on declared-Num AMOUNT with "1.50","1.5": `check()` reports 1 duplicate (typed keys equal), `inspect()` computes `duplicated()` on raw text, finds 0 failing rows, and `explain_messages()` prints "No offending rows were captured." The comment at 1061-1065 names exactly this divergence as the thing that must not happen.
- Confidence: verified-by-reading
- Suggested direction: pass the frame read by `dta_inspect_table_frame()` through `dta_coerce_table_to_specs(df, specs)` (and the same numeric fast-path-free typing) before recomputing rule rows.

### F-c3-stream-03  [minor] [perf]
- Where: R/streamingValidation.R:1864-1871, 1823-1831, 1840, 1894-1932, 1957; comment 1886-1893
- What: The grouped accumulator still writes through `state$<list>[[cond]][gids] <- ...` every batch, which by the file's own stated mechanism copies each capacity-sized per-condition vector per batch, and the hoisted head matrices are copied once per batch as well, so a batch costs O(groups accumulated), not O(groups touched) as the comment claims.
- Evidence: comment: "`state` is a function argument, so the nested replacement has no in-place path ... made a batch cost O(groups accumulated) rather than O(groups touched)" (1888-1892). Yet `state$any_true[[cond_name]][gids] <- ...`, `state$all_true[[...]][gids] <- ...`, `state$true_n[[...]][gids] <- ...` (1866-1869) and `state$n_seen[gids] <- ...` (1871) remain, and `head_matrix <- state$true_head[[cond_name]]` (1896) is still referenced by the list element when `head_matrix[id, ...] <- ...` (1905) runs, forcing one full duplicate of the capacity x 10 double matrix per (batch, condition, side) whenever any group is still "needy".
- Failure scenario: a file sorted by subject (new groups in every batch), 1e7 groups, 3 conditions, default 131,072-row batches over 1e9 rows: every one of ~7,600 batches allocates and copies ~1.7 GB of head matrices plus ~0.3 GB of per-condition vectors -- terabytes of memcpy/GC churn, quadratic in rows/batch_rows x groups.
- Confidence: inferred (R copy semantics reasoned from the file's own comment and R's evalseq/refcount rules; not measured, R not run per brief)
- Suggested direction: hoist every per-condition vector the same way and drop the list reference (`state$x[[cond]] <- NULL`) before modifying the local, then store back once; correct the comment.

### F-c3-stream-04  [minor] [inconsistency]
- Where: R/streamingValidation.R:3117-3166 (dta_structural_failure_details); cf. 1436-1481, R/validationFunctions.R:390-400
- What: The structural early return reports `rules_valid = TRUE` and `import_valid = TRUE` for axes that were never evaluated, contradicting its own description and the partial-scan convention that an unsettled axis is `NA`.
- Evidence: description: "The rule and import axes report as valid because they were never evaluated -- ... claiming a failure would be as wrong as claiming a pass." then `rules_valid = TRUE, import_valid = TRUE` (3137-3138). The partial-scan branch reports unsettled axes as `NA` (1464-1479), and `dta_details_ok()` documents `NA` as "unknown ... not a pass". CHANGELOG.md:4785-4788 records the `TRUE` values as intentional ("read `TRUE` because those axes were never evaluated, not because they passed -- the attribute exists so that cannot be misread"); the finding stands despite that entry, because it contradicts both the function's own description and the `NA` convention the partial-scan path uses for exactly this situation, and the consumers below do not read the attribute.
- Failure scenario: `validate_file_stream(specs, f, on_missing_column = "stop")` on a file missing one column: `details$rules_valid` is `TRUE` and `inspect()` emits `details_rules_valid = TRUE` (validationReporting.R:1017) although no rule ran; `dta_import_messages_to_df()` treats `import_valid = TRUE` as settled and emits no "not settled" warning (339). Only the `structural_only` attribute (which `dta_flatten_inspect_record` does not surface) says otherwise.
- Confidence: verified-by-reading
- Suggested direction: report `NA` for the two unevaluated axes, as the partial-scan path does.

### F-c3-stream-05  [minor] [inconsistency]
- Where: R/streamingValidation.R:1268-1281 (columnspec_settled for partial scans)
- What: A `fail_fast` partial scan reports the presence (`required`) check as `not_checked` even though presence was fully decided (from the header and from the first non-empty batch), while the comment beside it argues that understating a header-decided check is "not the safe direction" and settles `additionalProperties` for exactly that reason.
- Evidence: `columnspec_settled <- if (partial_scan) { if (structure_known) "additionalProperties" else character(0) }`; comment 1269-1274: "it was decided from the source's names BEFORE the first batch ... Reporting it `not_checked` understated what the run knew". `required` errors are produced only by column absence (columnSpecChecks.R:697-711), which every batch -- including the first, always read before a `fail_fast` break -- already decides.
- Failure scenario: all declared columns present, first batch has one maxLength error, `fail_fast = TRUE`: the console prints "Presence not checked: the scan stopped at the first problem" and `columnspec_checks` marks presence `not_checked`.
- Confidence: verified-by-reading
- Suggested direction: include `"required"` in the partial-scan settled set whenever at least one batch was read (or `structure_known`).

### F-c3-stream-06  [nit] [inconsistency]
- Where: R/streamingValidation.R:1464-1470
- What: In a partial scan with zero rules, `rules_valid` becomes `NA` ("not settled") although an empty rule axis is vacuously settled and is `TRUE` on every other path.
- Evidence: `details$rules_valid <- if (length(details$rule_errors) > 0) FALSE else if (length(states) > 0 && all(pre_ok)) TRUE else NA` -- `length(states) == 0` falls to `NA`.
- Failure scenario: specs with no rules, `fail_fast = TRUE`, first batch has a column-spec error: `rules_valid` is `NA`, whereas the same scan without `fail_fast`, and the eager path, say `TRUE`.
- Confidence: verified-by-reading
- Suggested direction: treat `length(states) == 0` as `TRUE`.

### F-c3-stream-07  [minor] [inconsistency]
- Where: R/streamingValidation.R:473-565 (dta_error_sink / dta_error_sink_add), 859-867, 2962-2978; cf. R/validationFunctions.R:419-447
- What: The streaming path never validates `max_errors`, so the values the eager path refuses (`NA`, negative, length > 1) are silently accepted or fail with an uninformative base-R error.
- Evidence: `room <- sink$max - sink$retained; if (room <= 0) {` (551-552) with no prior check; the eager `dta_truncate_error_frame()` aborts with a cli message for `NA`, negative and vector caps, stating a non-cap "is an error, not a silent 'keep everything'" (419-424).
- Failure scenario: `validate_file_stream(specs, f, max_errors = -1)` retains no rows and spills everything without a word; `max_errors = NA` aborts mid-scan with "missing value where TRUE/FALSE needed" at the first error row (after possibly hours); `check()` on an in-memory table with the same argument aborts up front with the documented message.
- Confidence: verified-by-reading
- Suggested direction: validate `max_errors` once at the streaming entry points with the same rule as `dta_truncate_error_frame()`.

### F-c3-stream-08  [minor] [doc-missing]
- Where: R/streamingScale.R:215-219 vs 222-224, 277-291
- What: `dta_arrow_unique_eligible()`'s `@return` documents `ok` and `numeric_columns` only; the function also returns `integer_columns`, which the caller depends on.
- Evidence: `@return A list with \`ok\` ... and \`numeric_columns\`` (215-219); `list(ok = TRUE, numeric_columns = ..., integer_columns = ...)` (287-291); consumed at 379-380.
- Failure scenario: a maintainer reading the contract does not know `integer_columns` exists or that `numeric_columns` includes the integer subset.
- Confidence: verified-by-reading
- Suggested direction: document `integer_columns` and that it is a subset of `numeric_columns`.

### F-c3-stream-09  [nit] [doc-wrong]
- Where: R/streamingValidation.R:2741-2743 vs 2783-2785; 2929-2935
- What: Two comments about the Parquet cache contradict each other, and the memory-section doc names "Parquet" as an example of a source that falls back to the R key set although the only Parquet the function documents accepting (`cache_as_parquet()` output) is all-text and therefore eligible.
- Evidence: "Parquet stores its own types ... the types were fixed when the cache was written" (2741-2743) vs "Every column is written as text (utf8) ... the declared types are applied when the cache is validated, not stored in it" (2783-2785); "That happens when the source is not a text-typed dataset (for example Parquet, which carries its own types)" (2931-2932) while `@param path` says "or to a Parquet dataset written by [cache_as_parquet()]" (2876-2877).
- Failure scenario: a reader concludes cached Parquet loses the Arrow uniqueness path or carries typed columns; neither is true.
- Confidence: verified-by-reading
- Suggested direction: fix the 2741-2743 comment and say "a Parquet dataset carrying its own (non-text) types".

### F-c3-stream-10  [minor] [code-cheat]
- Where: R/validationReporting.R:634-639, 652-653 (explain_messages); 1479-1487 (dta_inspect_tabular_message)
- What: `explain_messages()` swallows any error from `inspect()` and then reports rule messages as having no offending rows; `inspect()` itself swallows every error from the group-condition re-check.
- Evidence: `inspected <- tryCatch(inspect(x, id = msgs$id, as_tibble = FALSE), error = function(e) NULL)` (636-639); with `inspected` NULL, `detail` is the bare messages row, `dta_explain_rows_lines()` finds no `failing_` columns and `actual <- "No offending rows were captured."` (792-794). `gc_result <- tryCatch(rule_check_group_condition(rule_def, table_df), error = function(e) NULL)` (1480-1483).
- Failure scenario: any defect in `inspect()` (e.g. a lazy table whose rule column was projected away, or a programming error) turns into a confident "Actual: No offending rows were captured." for every rule message, i.e. the explanation asserts the opposite of the failure it is explaining.
- Confidence: verified-by-reading
- Suggested direction: let the error surface (or `cli_warn()` with the condition message) instead of silently degrading.

### F-c3-stream-11  [minor] [doc-missing]
- Where: R/streamingScale.R:376-461 (precompute) vs R/streamingValidation.R:2907-2910, 2334-2337
- What: `use_threads = FALSE` is documented as "the lever when RSS rather than speed is the constraint", but the uniqueness precompute -- a full extra pass over the key columns -- runs `dplyr::collect()` on the Dataset without honouring it.
- Evidence: `use_threads` is passed only to `arrow::Scanner$create()` (2467-2477, 3069-3079); `dta_stream_unique_precompute()` takes no thread argument and calls `dplyr::collect(dplyr::summarise(...))` (435-441).
- Failure scenario: a user on a memory-constrained host sets `use_threads = FALSE`; the precompute pass still runs Acero multi-threaded with its own buffering, so the RSS bound the doc promises does not hold for that pass.
- Confidence: inferred (did not verify Acero's default thread use for `collect()` beyond arrow's documented `arrow.use_threads` default)
- Suggested direction: thread `use_threads` into the precompute (e.g. via `options(arrow.use_threads=)` locally) or document the exception.

## Area 2 — inspect/messages/results (R/validationReporting.R, rest of file)

### F-c3-stream-12  [major] [bug]
- Where: R/validationReporting.R:1082-1086 (dta_rule_failure_row_indices, col_condition branch); cf. 1053-1058, 1073-1076, 1092-1095, 1509-1525; R/exportValidationReport.R:116-131
- What: `inspect()` aborts outright when a dataset carries an IF/THEN rule that "could not be evaluated" (e.g. a stale rule naming a missing column), because the col_condition branch -- unlike the range, unique and group branches -- does not guard against the `dta_rule_not_applicable` abort.
- Evidence: `return(which(dta_condition_violated(rule, df) %in% TRUE))` (1085) with no guard; `evaluate_condition()` aborts with class `dta_rule_not_applicable` for an absent column (evaluateRules.R:1105-1110). The sibling branches return `integer(0)` for a missing column (1055-1057, 1074-1076) or catch the class (1092-1095). `inspect()` loops all requested messages with no per-message guard (1519-1521). `apply_rules()` documents this case as the typical one: "typically a stale rule naming a column the table does not have" (evaluateRules.R:1763-1767).
- Failure scenario: specs hold `col_condition` on column VISITDT, the delivery lacks VISITDT. `check()` correctly reports "Rule 'r1' could not be evaluated: Column not found in table: VISITDT"; then `inspect(ds)` (all ids) aborts with that same error; `explain_messages(ds)` swallows it (F-c3-stream-10) and prints every message -- including unrelated column-spec ones -- without detail; `write_validation_report()` falls back per dataset (exportValidationReport.R:116-131), so the whole dataset's panels lose their should-be/actual detail.
- Confidence: verified-by-reading
- Suggested direction: guard the col_condition branch like the group branch (`tryCatch(..., dta_rule_not_applicable = function(cnd) integer(0))`).

### F-c3-stream-13  [major] [perf]
- Where: R/validationReporting.R:1250-1298, 1354-1363, 1380-1383, 1414-1422, 1519-1521; R/exportValidationReport.R:116-117; R/validationReporting.R:636-639
- What: For a lazily held (streamed) table, `inspect()` opens a fresh Scanner per message: a column-spec/import message reads rows 1..its row number, and every rule message reads *all* rows of the rule's columns into an R data frame; `write_validation_report()` and `explain_messages()` call `inspect()` for every message, so report generation re-scans the delivery once per message.
- Evidence: `table_df <- if (has_row) dta_inspect_table_frame(raw_table, columns = row_cols, max_row = msg_row_no)` inside the per-message function (1363, 1383); the rule branch: "the whole table -- narrowed to the columns this rule actually reads -- has to be scanned regardless" (1414-1419); `reader <- arrow::Scanner$create(query)$ToRecordBatchReader()` built on every call (1275); the report calls `inspect(x, id = msgs$id)` for all ids (exportValidationReport.R:116-117).
- Failure scenario: a dirty 60 GB delivery validated with `check()` (streamed) retains the default 10,000 column-spec rows; `write_validation_report()` then runs 10,000 separate scans from row 1 (each converting at least one default-size record batch to R), and each rule message materialises its key/group columns over every row of the file in R memory -- the exact holding the streaming path exists to avoid.
- Confidence: verified-by-reading (per-message scan structure); the run-time magnitude is inferred (not measured)
- Suggested direction: read all needed rows for all messages of a table in one bounded scan (or reuse the spilled detail/row heads the scan already has) instead of one scan per message.

### F-c3-stream-14  [minor] [doc-missing]
- Where: R/validationReporting.R:248-272, 365-383, 631, 655-657; cf. R/streamingValidation.R:611-619, R/validationFunctions.R:466
- What: `messages()` returns only the retained head of the column-spec and import frames and says nothing when they were truncated, so `messages()`/`explain_messages()` present a capped list as the complete set of findings.
- Evidence: `dta_columnspec_messages_to_df()` turns `details$columnspec_errors$full_error` row by row into messages and ignores its `truncated` / `spilled_rows` attributes (both set by the sinks and by `dta_truncate_error_frame()`); `explain_messages()` reports "... and {n_more} more message{?s}" computed from `nrow(msgs)` (631, 656), i.e. from retained rows only. The `messages()` doc promises "one row per validation message" (178).
- Failure scenario: a streamed table with 2,000,000 maxLength violations: `results()` says `n_columnspec_errors = 2000000`, `messages()` returns 10,000 column-spec rows with no warning row, and `explain_messages(n = Inf)` ends without any "and 1,990,000 more"; the pointer to `collect_full_errors()` appears nowhere in this output.
- Confidence: verified-by-reading
- Suggested direction: append one warning row per truncated axis (count retained vs total, and `collect_full_errors()` when spilled), as is already done for an unsettled import axis (339-362).

### F-c3-stream-15  [minor] [dead-code]
- Where: R/validationReporting.R:13-17, 90; R/reportRendering.R:39-41, 58; cf. R/DTADataSetTabular-class.R:1183-1185, 1235-1239, R/DTADataSetFile-class.R:1136-1141
- What: `results()` documents and computes an `n_skipped` column, and the report maps a "skipped" status, but `validation_status()` -- the only input to both -- never emits "skipped" ("skipped" exists only in `check()`'s own output rows), so the column is always 0 and the branch is unreachable.
- Evidence: `n_skipped = sum(status_df$status == "skipped", na.rm = TRUE)` (90); both `validation_status()` methods build every indexed row with `status = "validated"` (DTADataSetTabular-class.R:1235-1239, DTADataSetFile-class.R:1136-1141), and the method's own doc lists only "validated", "not_validated", "unspecified" (1183-1185).
- Failure scenario: after `check()` skips an unchanged table, `results(ds)$n_skipped` is still 0, contrary to the column's name and docs.
- Confidence: verified-by-reading
- Suggested direction: drop `n_skipped` and the "skipped" mapping, or record the skip in the index if it is meant to be reported.

### F-c3-stream-16  [nit] [doc-wrong]
- Where: R/validationReporting.R:1366
- What: The `why` text of every column-spec inspect record says "Column values violate JSON schema constraints (type/value/length/required)", but no JSON Schema validator runs any more and the finding may be an undeclared column, a format/pattern or a matrix header/dimension finding.
- Evidence: `out$why <- "Column values violate JSON schema constraints (type/value/length/required)."`; validationFunctions.R:180-187 ("Evaluated column-wise against the specs rather than row-wise against a JSON Schema validator"); `additionalProperties` findings concern a column, not values (streamingValidation.R:2657-2693).
- Failure scenario: `inspect()` of an "undeclared column" message tells the reader that column values violate a type/value/length/required constraint.
- Confidence: verified-by-reading
- Suggested direction: derive the sentence from the keyword (as `.report_columnspec_expected_text()` does).

### F-c3-stream-17  [nit] [design]
- Where: R/validationReporting.R:1113-1117, 1305-1307, 1328, 1361, 1381, 1452, 1457
- What: Row context and previews hard-code the column names `SUBJECT_ID` and `VISIT` (the bundled example's columns) instead of anything the specs declare.
- Evidence: `useful_cols <- c("SUBJECT_ID", "VISIT", msg_row$column)` (1113); `unique(c(rule_cols, "SUBJECT_ID", "VISIT"))` (1328); `preview_cols <- c("SUBJECT_ID", "VISIT")` (1457).
- Failure scenario: a real delivery keyed by `USUBJID`/`VISITNUM` gets context showing only the offending column, while a column coincidentally named `VISIT` is always pulled in -- and, for a lazy table, read from disk for every message.
- Confidence: verified-by-reading
- Suggested direction: take the context columns from the specs (e.g. key columns of uniqueness rules) or make them a parameter.

## Area 3 — HTML report rendering (R/reportRendering.R)

Escaping checked: every data- or spec-derived string reaching the HTML goes through `.report_html_escape()` (summary rows 356-364, messages table 657-684, filter options 717-736, kv tables 1070-1085, panel headline/should-be/actual/kv 1105-1215, `.rule_html_dl()` in rulePhrasing.R:1493-1539). The only unescaped interpolations are package-controlled tokens (`status_classes`, `type`, integer `id`) and numbers. No HTML-injection finding.

### F-c3-stream-18  [minor] [doc-wrong]
- Where: R/reportRendering.R:850 (and the same text through `explain_messages()`, validationReporting.R:775)
- What: The "Should be" sentence for a `required` finding says "The value must be present (not missing).", but in this package `required` means a declared *column* is absent from the table; a missing value is reported under `type`.
- Evidence: `required = "The value must be present (not missing).",`; `required` rows are built only for an absent column ("must have required property '<col>'", columnSpecChecks.R:697-711; streamingValidation.R:2639-2655); an NA in a non-nullable column is a `type` violation (columnSpecChecks.R:144-178).
- Failure scenario: a delivery lacking column AESEV: the report panel reads "Should be: The value must be present (not missing). Actual: (missing / not present)", sending the reviewer to look for empty cells in a column that does not exist.
- Confidence: verified-by-reading
- Suggested direction: "The column <name> must be present in the table." using `columnspec_columnspec`.

### F-c3-stream-19  [nit] [doc-wrong]
- Where: R/reportRendering.R:806-816
- What: The doc of `.report_columnspec_expected_text()` lists "Every columnspec keyword this package's validator emits" as seven keywords and says each carries "the constraint's own value", but the validator also emits `format`, `finite`, `minimum`, `maximum` and the matrix keywords (handled by the very switch below), and `required` carries the column name, not a constraint value, and is not built by `dta_columnspec_error_rows()`.
- Evidence: doc 808-812 vs switch 849-881; columnSpecChecks.R:313-372 (format/finite/minimum/maximum), 702-710 (`required` built inline with `columnspec = column_name`).
- Failure scenario: a maintainer extending keywords trusts the list and misses the extra branches.
- Confidence: verified-by-reading
- Suggested direction: list the keywords the switch handles and state what `columnspec` holds for `required`/`additionalProperties`.

### F-c3-stream-20  [nit] [bug]
- Where: R/reportRendering.R:894-906
- What: A present but empty string value is rendered as "(missing / not present)".
- Evidence: `if (identical(kw, "required") || is.na(data_val) || !nzchar(as.character(data_val))) { ... return("(missing / not present)") }`.
- Failure scenario: an in-memory table with `RACE = ""` failing an `enum` check: the panel says the value is missing, while it is an empty string that is present (and would pass a nullable-NA allowance that "" does not).
- Confidence: verified-by-reading
- Suggested direction: render `""` as "(an empty string)".

### F-c3-stream-21  [minor] [bug]
- Where: R/reportRendering.R:483-495 (.report_inputs_df)
- What: The "Inputs" provenance table classifies a table as "streamed" only when it is an Arrow `Dataset`; an `arrow_dplyr_query` or `RecordBatchReader` (both admitted lazy holdings) is reported as "in memory" with no source file and no hash.
- Evidence: `if (inherits(tbl, "Dataset")) { ... "streamed" ... } else { input_row(id, nm, "in memory", hash = dta_table_hash_stamp(tbl) %||% NA_character_, ...) }`; lazy holdings are `Dataset`, `arrow_dplyr_query`, `RecordBatchReader` (streamingValidation.R:2268-2272; DTADataSetTabular-class.R:80-81).
- Failure scenario: a dataset built from `open_dataset(f) |> dplyr::filter(...)` is filed in the report as an in-memory table with an empty "Source file" and "Content hash", so the report cannot be tied back to the data -- the stated purpose of the section (410-416).
- Confidence: verified-by-reading
- Suggested direction: classify with `dta_table_is_lazy()` and resolve source files for queries where possible; label readers as "stream (consumed)".

### F-c3-stream-22  [minor] [code-cheat]
- Where: R/reportRendering.R:99-107 (.report_with_metadata_row)
- What: Errors from `metadata()` / `metadata_import_errors()` are swallowed, silently dropping the metadata row whose stated purpose is that a transfer invalid only because of metadata "must not headline as 'everything passed'".
- Evidence: `md <- tryCatch(metadata(x), error = function(e) NULL)`; `errors <- tryCatch(metadata_import_errors(md), error = function(e) NULL)`; `n_errors <- if (is.data.frame(errors)) nrow(errors) else 0L`.
- Failure scenario: any defect in the metadata error accessor makes the overview show 0 failed targets for a transfer whose only fault is metadata -- the precise outcome the function documents it exists to prevent.
- Confidence: verified-by-reading (the swallow); whether the accessor can actually throw on real metadata was not checked
- Suggested direction: let the error propagate or render an explicit "metadata could not be read" failed row.

### F-c3-stream-23  [minor] [inconsistency]
- Where: R/reportRendering.R:535-548; user-facing via R/exportValidationReport.R:24, 50, 201
- What: `max_repeats = NA` (or any non-numeric value such as `"5"`) silently disables capping instead of being refused, although the comment claims invalid values are rejected and the user-facing doc says "Positive integer".
- Evidence: `capping_enabled <- !is.null(max_repeats) && is.finite(max_repeats)`; `is.finite(NA)` and `is.finite("5")` are `FALSE`, so neither reaches the `cli_abort()`; a length-2 value errors in `&&` with a base-R message instead.
- Failure scenario: `write_validation_report(x, f, max_repeats = NA)` renders every one of 10,000 identical messages uncapped, with no error -- the same "NA read as no cap" defect `dta_truncate_error_frame()` was fixed for (validationFunctions.R:419-424).
- Confidence: verified-by-reading
- Suggested direction: accept only `NULL`, `Inf` or a single positive whole number; abort on everything else.

### F-c3-stream-24  [nit] [bug]
- Where: R/reportRendering.R:375-381, 680
- What: Counts and row numbers past the integer range (which the streaming path deliberately keeps as doubles) are rendered with R's default formatting, so round values appear in scientific notation in the report.
- Evidence: `ifelse(is.na(results_df$n_columnspec_errors), .report_em_dash(), results_df$n_columnspec_errors)` pasted into the cell; `if (is.na(messages_df$row[i])) "" else messages_df$row[i]` into `%s`; counts are doubles past `.Machine$integer.max` by design (`dta_narrow_count()`, `dta_narrow_rows()`).
- Failure scenario: a missing column in a 3,000,000,000-row streamed file: the overview cell reads "3e+09" and message rows read "3e+09"-style row numbers.
- Confidence: verified-by-reading
- Suggested direction: format with `format(x, scientific = FALSE, trim = TRUE)` (or `dta_format_count()`).

## Area 4 — rule phrasing (R/rulePhrasing.R)

Checked against the evaluator and found consistent: operator table (equals/in/not_in/numeric/pattern/empty missing-value outcomes vs `dta_condition_mask()`), `in`/`not_in` list semantics vs `dta_equality_bound()`, scope handling ("anything but all is any"), constraint message defect, unsupported constraint types, default constraint ids (`constraint_<i>`, DTARuleGroupCondition-class.R:186), `.rule_then_missing_unreachable()` vs `dta_condition_in_scope()`, number rendering, date-mask token ranges (datePatternTokens.R:14-26), and HTML escaping in `.rule_html_dl()`.

### F-c3-stream-25  [major] [doc-wrong]
- Where: R/rulePhrasing.R:1101-1104 (range), 979-983 (IF), 951-956 (group IF-only), 963-972 (IF "unless another column ..." exception); header 9-11, 74-76, 123-129, 260-263
- What: The "Non-numeric values" notes describe what the rule functions do on *untyped* text, but every validation pipeline (eager `check()` and the streaming driver the header names) types the table first, turning a non-number in a declared-numeric column into NA before any rule sees it -- so for such columns the notes state the opposite of the real outcome.
- Evidence: header: "The wording describes what the evaluator (R/evaluateRules.R, R/streamingValidation.R) does" (9-11); range: "A value that is present but is not a number violates the rule." (1102); IF: "A value in C that is not a number does not by itself exempt the row from the check." (981). But the scan types each batch before the rules (streamingValidation.R:1000-1009), the eager constructor types at load (DTADataSetTabular-class.R:75-79, 120-134), an unconvertible value becomes `NA_real_` (importConversion.R:495-499), a numeric column never has `unconvertible = TRUE` (evaluateRules.R:53-61), the range mask ignores NA (534-538), and the IF rescue keys on `$unconvertible` (evaluateRules.R:930, 960-967).
- Failure scenario: AGE declared `SAS Num`. (a) `col_range AGE 18..65`, value "abc": the spec document says the value violates the rule; `check()` reports the rule as passing for that row (NA is "Not checked") and lists "abc" only as an import error. (b) "When AGE is at least 18, CONSENT must not be empty", row AGE = "abc", CONSENT empty: the document says the row is not exempt; `check()` finds no rule violation (the row is out of scope). The notes are only right for columns the specs leave untyped (Char / undeclared) or for a bare `validate_table()` on raw text.
- Confidence: verified-by-reading
- Suggested direction: state in the notes (or once in the reading guide) that a value which cannot be read as the column's declared type is reported as an import error and counts as missing for every rule; keep the current notes for undeclared/text columns.

### F-c3-stream-26  [minor] [doc-wrong]
- Where: R/rulePhrasing.R:1571-1574, 1583-1592 (reading guide); cf. R/evaluateRules.R:262-279, 353-356; R/importConversion.R:444-446, 477-486
- What: The reading guide says text values are "compared exactly, including upper and lower case" and that equality compares "numbers as numbers ... otherwise ... text", omitting that against a logical column (a column declared `R logical` is typed to R logical) a text operand is read as a logical token, so `"true"`, `"T"`, `"True"` and `"TRUE"` are all the same value.
- Evidence: `dta_equality_bound()`: `if (is.logical(x)) return(dta_logical_bound(value))` (353-356); tokens `T`, `TRUE`, `true`, `True`, `F`, ... (262-266); logical targets are typed through the same tokens at import (importConversion.R:477-486).
- Failure scenario: rule "FLAG is equal to “true”" on a column declared `R logical`: a reader of the guide expects only the literal text `true` to match, but rows written `TRUE`, `T` or `True` match too; and "FLAG is equal to “yes”" matches nothing, because "yes" was already an import error (NA) -- neither is stated anywhere.
- Confidence: verified-by-reading
- Suggested direction: add one sentence on logical columns to the equality paragraph of the reading guide.

### F-c3-stream-27  [nit] [doc-wrong]
- Where: R/rulePhrasing.R:1620-1622
- What: The reading guide says a date mask is written "between two angle brackets", but the syntax recognised is a doubled pair, `<<...>>`; text between single brackets is a literal regex.
- Evidence: "a run of the letters y, M, d, H, m and s between two angle brackets, such as yyyyMMdd"; datePatternTokens.R:4 ("written `<<yyyyMMddHHmm>>`"), 35 and 46-49 (only `<<` / `>>` delimit a mask).
- Failure scenario: an author following the guide writes `^<yyyyMMdd>$`; it is never expanded, every value fails the pattern, and no construction-time check flags it.
- Confidence: verified-by-reading
- Suggested direction: "between double angle brackets (two on each side)".

### F-c3-stream-28  [minor] [inconsistency]
- Where: R/evaluateRules.R:851 vs R/columnSpecChecks.R:249-256 (documented on the rule side only, R/rulePhrasing.R:1609-1612)
- What: The same regular expression gives different verdicts on the same numeric value depending on whether it is a column-spec `pattern` or a rule `pattern` operator: the column-spec check renders numbers in plain decimal, the rule renders them with `as.character()`.
- Evidence: rule: `pattern = grepl(dta_expand_date_pattern_tokens(value), as.character(x), perl = TRUE)`; column spec: `grepl(pattern, dta_numeric_text(u), perl = TRUE)`; the guide warns only that in a rule "a number may read as 1e+05".
- Failure scenario: `SAS Num` column LBORRES = 100000 with pattern `^[0-9]+$`: the column-spec pattern passes, a rule `LBORRES: {pattern: "^[0-9]+$"}` fails ("1e+05"); 99999 passes both.
- Confidence: verified-by-reading
- Suggested direction: render numbers the same way (`dta_numeric_text()`) in both places, then drop the caveat from the guide.

## Area 5 — benchmarking (R/benchmarking.R) and remaining streaming docs

### F-c3-stream-29  [nit] [doc-wrong]
- Where: R/benchmarking.R:1-8, 262-269, 227-233
- What: The nesting guard's stated rationale -- `check()` on a DTA, DTADataSet and DTADataSetTabular each resetting the gc peak -- describes a call hierarchy that does not exist: only `check(<DTA>)` and `validate_file_stream()` ever open a measurement and neither calls the other, so the guard never engages; `validation_benchmark()` likewise says the metrics are attached by `check()` generally.
- Evidence: `dta_benchmark_begin()` is called only at DTA-class.R:1045 and streamingValidation.R:2983 (grep over R/); no dataset-level `check()` takes `benchmark` (no "benchmark" in DTADataSetTabular-class.R or DTADataSet-class.R).
- Failure scenario: `check(ds, benchmark = TRUE)` on a DTADataSetTabular yields no metrics although `validation_benchmark()`'s doc implies it would; maintainers keep defensive code for an unreachable case.
- Confidence: verified-by-reading
- Suggested direction: correct the comment/doc (only `check(<DTA>)` and `validate_file_stream()` benchmark) or drop the guard.

### F-c3-stream-30  [minor] [doc-wrong]
- Where: R/streamingValidation.R:834-839 vs 1175-1190, 1268-1290
- What: `dta_validate_table_stream()`'s `@param known_columns` says the names are "Used only when the stream yields no rows", but they are used on every scan to report undeclared columns and to decide whether closedness (and, on an empty stream, presence) is settled.
- Evidence: `structure_known <- length(known_columns) > 0` ... `findings <- dta_structure_findings(specs, known_columns)` runs for every scan, emitting `additionalProperties` errors for non-empty streams too (1175-1190); `columnspec_settled` branches on `structure_known` (1268-1290).
- Failure scenario: a caller of the internal driver omits `known_columns` for a non-empty stream believing it only matters for empty input; undeclared columns then go unreported and closedness is silently reported `not_checked`.
- Confidence: verified-by-reading
- Suggested direction: document both uses.

### F-c3-stream-31  [minor] [doc-wrong]
- Where: R/streamingValidation.R:2866-2874 (validate_file_stream, "Choosing between this and the in-memory path")
- What: The doc recommends `validate_table()` as the faster in-memory alternative for "a file that fits in memory", but `validate_table()` takes a data frame and never types it by the specs, so it does not reproduce `validate_file_stream()`'s verdict on the same file; the in-memory equivalent is `load_file()` + `check()`.
- Evidence: "For a file that fits in memory comfortably, `validate_table()` is the faster choice and there is nothing to gain here."; `validate_table()` calls `validate_table_detailed(specs, table)` directly (validationFunctions.R:46-47), which reads only issues already carried on the table (156) and never calls `dta_coerce_table_to_specs()`; the column-spec `type` check then runs on whatever classes the user's reader produced (columnSpecChecks.R:144-178).
- Failure scenario: a user reads the CSV with `read.csv()` and calls `validate_table()`: an `ID` declared `SAS Char` with values "007" is read as integer 7 and fails the `type` check, and a `SAS Num` column holding one "abc" is read as character and fails the `type` check on every row -- results `validate_file_stream()` never reports for the same file.
- Confidence: verified-by-reading
- Suggested direction: point to `load_file()` + `check()` (which type at load) instead.

## Coverage

Files fully read (every line):
- R/arrowCompute.R (230 lines) -- no defects found; Arrow path falls back to the R reference only on error, which yields the same answer.
- R/streamingScale.R (463)
- R/streamingValidation.R (3,186)
- R/validationReporting.R (1,622)
- R/benchmarking.R (287)
- R/reportRendering.R (1,216)
- R/rulePhrasing.R (1,634)

Read outside the slice to trace parity and semantics (relevant functions, not whole files): R/evaluateRules.R (1-260, 296-470, 520-560, 581-608, 633-1300, 1308-1366, 1419-1882), R/importConversion.R (230-1108), R/validationFunctions.R (1-520), R/columnSpecChecks.R (100-390, 640-1210), R/DTADataSetTabular-class.R (70-150, 1160-1240, 1425-1450, 1825-1845), R/DTAFileTabular-class.R (1740-1940), R/DTARuleColRange-class.R (all), R/DTARuleGroupCondition-class.R (170-210), R/DTADataSet-class.R (627-661), R/DTADataSetFile-class.R (1105-1152), R/datePatternTokens.R (1-140), R/exportValidationReport.R (100-160), CHANGELOG.md (4778-4795), tests/testthat/test-streaming-validation.R (1-75, summarised_error hits), tests/testthat/test-validationFunctions.R (260-330).

Only skimmed: none of the slice files. Not read: none of the slice files. Tests of this slice were consulted only where cited; a full test-cheat audit of test-streaming-*.R / test-rulePhrasing.R was not in this slice's file list.

## Addendum (after advisor pass) — precompute parity, spill lifetime, roxygen

### F-c3-stream-32  [major] [logic-error]
- Where: R/streamingScale.R:409-415 (cast), 150-157 and 310-319 (safety claim); cf. R/importConversion.R:712-765
- What: The default-on Arrow uniqueness precompute casts declared-`Num` key text with an unrestricted Arrow `as.numeric()`, while the batch typing allows Arrow parsing only for literals proven to parse bit-identically in R (`DTA_ARROW_DOUBLE_PATTERN`); the precompute's own safety claim ("Every string both engines accept parses to the identical double") is contradicted by the measurement documented in importConversion.R.
- Evidence: precompute: `dplyr::across(dplyr::all_of(numeric_cols), ~ as.numeric(.x))` with no pattern gate (412-415); its doc: "it is exact where it succeeds. Every string both engines accept parses to the identical double, ... `\"18446744073709551616\"` included" (317-319) and "a cast that Arrow either performs exactly as R's parser does or refuses outright" (150-157). importConversion.R:720-727: R's `R_strtod()` rounds twice on x87 `LDOUBLE` while Arrow rounds once -- "20-digit integers disagree on about 1 value in 3,500, and 7-significant-digit decimals on about 1 in 3,400" -- and "one ULP is enough to make a streamed uniqueness verdict disagree with the in-memory one on the same file" (714-718); hence the batch path's restriction to <= 15 digits / <= 3 decimals (764). The eager path and the per-batch fallback both key on R's parse; only the precompute keys on Arrow's.
- Failure scenario: a key column of 16-20-digit numeric identifiers declared `SAS Num` (distinct values can then share or straddle a double). Where R's double rounding and Arrow's single rounding put one identifier one ULP apart, the set of keys that collide differs, so the duplicate count from the default lazy `check()` / `validate_file_stream()` differs from `check()` on the same file loaded in memory, and from the same scan with `options(DTAtools.stream_arrow_unique = FALSE)`; in an otherwise collision-free key set the verdict itself can flip. Keys of <= 15 significant digits are not affected (distinct values are many ULPs apart, and each engine is deterministic per value).
- Confidence: verified-by-reading (missing gate, contradicting comments, which engine each path uses); inferred (the specific colliding pair -- R not run, per brief)
- Suggested direction: apply the same `DTA_ARROW_DOUBLE_PATTERN` gate in the precompute (refuse -> per-batch fallback when any non-null key text falls outside it), and correct the 150-157 / 317-319 claim.

### F-c3-stream-33  [minor] [design]
- Where: R/streamingValidation.R:463-472, 497-513, 673-676; R/00_helpers.R:1017-1029; R/columnSpecChecks.R:697-711; default `on_missing_column = "scan"` (streamingValidation.R:2342-2345, 2901-2906, 2975)
- What: Every row past `max_errors` is spilled to an RDS part under `dta_temp_dir()` and nothing in the package ever deletes a spill directory, so in a plain R session spills accumulate until the process exits -- and with the default `on_missing_column = "scan"` a single absent declared column spills one row per row of the file.
- Evidence: `sink$spill_dir <- tempfile("dta_error_spill_", tmpdir = dta_temp_dir())` and `saveRDS(errs, ...)` per overflowing batch (505-510); a grep over R/ finds no `unlink()` of any spill directory (the only cleanup hook is the Shiny host pointing `DTAtools.temp_dir` at a per-session directory, 00_helpers.R:1019-1025); an absent column yields `row = seq_len(n_rows)` `required` errors per batch (columnSpecChecks.R:702-710). The code calls this "the honest trade" (497-500), but neither the trade's disk cost nor a way to release a spill is documented at the entry points.
- Failure scenario: a batch job calls `validate_file_stream(specs, f)` (defaults) on several 400M-row deliveries that each lack one declared column: each run keeps 10,000 rows and writes ~400M `required` rows as ~3,000 RDS parts under `tempdir()`; none is ever removed, so disk use grows by gigabytes per delivery until the R process ends.
- Confidence: verified-by-reading (no deletion path; per-row spill of a missing column); the disk volume per row is inferred
- Suggested direction: do not spill per-row restatements of a structural finding (count them instead), and offer/document a way to release a result's spill (e.g. a `release_spill(details)` or deletion when the details are discarded).

### F-c3-stream-34  [nit] [doc-wrong]
- Where: R/streamingValidation.R:699-701 vs 727-753, 1381-1392; 2317
- What: `collect_full_errors()`'s `@return` says it returns `NULL` "when the axis has none", but for `axis = "import_typing"` on a streamed result it returns the always-present zero-row frame; and `dta_validate_any_table()`'s `@param table` ("An Arrow `Table`, `Dataset`, or reader") omits the data frames and `arrow_dplyr_query` objects it also handles.
- Evidence: `import_typing_errors` is "ALWAYS present -- a zero-row frame" when nothing failed (1369-1392); `collect_full_errors()` returns `frame` unchanged when no spill directory exists (746-753). `dta_validate_any_table()` routes non-lazy input through `as.data.frame(table)` (2415-2424) and treats `arrow_dplyr_query` as lazy (`dta_table_is_lazy()`, 2268-2272).
- Failure scenario: `is.null(collect_full_errors(details, "import_typing"))` is `FALSE` for a clean streamed file, contrary to the documented contract.
- Confidence: verified-by-reading
- Suggested direction: document the zero-row return for `import_typing` (or return `NULL` for it too); list every accepted holding in `@param table`.

Other `@return` contracts checked and found accurate: `validate_file_stream()`, `cache_as_parquet()`, `set_dta_compute_threads()`, `validation_benchmark()`, `explain_messages()`, `results()`, `messages()`, `inspect()`.

## Coverage addendum

- Temp-file/resource cleanup: checked -- spill directories are never deleted by the package (F-c3-stream-33); transcoded copies are outside this slice.
- Not covered: the tests of this slice (`tests/testthat/test-streaming-*.R`, `test-rulePhrasing.R`, `test-arrowCompute`-style files) were consulted only where cited and were NOT audited for test cheats (owner priority #1). The perf findings (F-c3-stream-03, -13) and F-c3-stream-32's specific collision are unmeasured, because R was not run.
