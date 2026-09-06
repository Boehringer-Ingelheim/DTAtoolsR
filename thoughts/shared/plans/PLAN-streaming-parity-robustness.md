# Plan: streaming parity, robustness and cost of `load_file()` / `check()`

Date: 2026-09-06. Branch: `ai/review-load-file-check-33266a` (from `dev`).

## Goal

`stream = "always"` must be a drop-in for the in-memory path: the same
`check()` verdicts and error detail for the same file, no data-dependent
load failures, and bounded memory at any file size. Where the two paths
differ today, the difference is removed at the reader (both paths read a
file identically) rather than papered over in the engines.

## Findings this plan acts on

Numbers refer to the review findings reported on this branch (all reproduced
on 2026-09-06, scripts under the session scratchpad `exp/`).

| # | Defect | Fix owner |
|---|--------|-----------|
| 1 | `load_file()` on a tabular dataset keeps the replaced table's verdict | WP-B |
| 2 | Same file, different rule verdict eager vs lazy (undeclared columns typed differently) | WP-A |
| 3 | Tabular dataset with no tables aborts `check()`/`results()` of the whole DTA | WP-B |
| 4 | In-memory table hash (gzip `saveRDS` + md5) costs more than validating | WP-C (+ WP-B constructor) |
| 5 | `{` in a table/file name aborts non-quiet `check()` (cli interpolation) | WP-B, WP-A (DTAFile-class.R) |
| 6 | `batch_rows` never affects a CSV scan (Arrow block size does) | WP-A |
| 7 | Embedded newlines / non-UTF-8 bytes make a file unloadable | WP-A |
| 8 | Lazy table whose file vanished aborts `check()` | WP-B |
| 9 | `check(DTA)` cannot forward `on_missing_column`, `fail_fast`, `use_threads` | WP-B |
| 10 | Declared type looked up per error row | WP-C |
| 11 | Eager path ignores `max_errors`; retained error frames unbounded | WP-C |
| 12 | Declared-numeric uniqueness keys never use Acero | WP-C |
| 13 | `.gz` delivery stored under a second table name | WP-B |
| 14 | Tabular `handler_index` guard repeats the bug the file method fixed | WP-B |
| 15 | Objects with a vanished artifact dir cannot be modified | WP-B |
| 16 | Non-dataset entry in a DTA aborts only when not quiet | WP-B |

## Ground rules for every work package

- Work only in the review worktree of this branch, never in the main checkout.
- Touch only the files listed for your package. If you believe another file
  must change, stop and report it instead of editing it.
- No git commands that change state. No commits. Work is authored by the
  person on whose behalf it is done; nothing in code, comments, tests or the
  changelog attributes it to anyone else.
- R: `"/c/Program Files/R/R-4.5.1/bin/Rscript.exe" --vanilla <script.R>` (Git
  Bash) with `pkgload::load_all("<worktree>", quiet = TRUE, export_all = TRUE)`.
  Write scripts into a session-temporary scratch folder outside the
  repository, one subfolder per work package.
  Installed: R 4.5.1, arrow 25.0.1 (dataset/Acero on), S7 0.2.2, testthat 3.3.2,
  rlang 1.3.0, bit64, withr. No `bench`: use `system.time()`.
- Run tests with `testthat::test_file("tests/testthat/<file>.R")` after
  `load_all()`; run only the files named for your package plus any file whose
  test you changed. Do not run the whole suite; WP-D does.
- Conventions (repo `CLAUDE.md`): `cli::cli_abort()`/`cli::cli_warn()` only;
  namespaced calls to imported packages; every behaviour change gets a test in
  `tests/testthat/test-<Topic>.R`; tests assert behaviour (`expect_error()` with
  `regexp`/`class`), never translated base-R text; tidyverse style (WP-D runs
  the styler script; keep your code close to it). Do not edit `man/`,
  `NAMESPACE`, `DESCRIPTION`, `CHANGELOG.md` (WP-D does the changelog).
- cli interpolation rule (finding 5): never `cli_*(paste0(..., <user text>))`
  and never `cli_abort(stringr::str_glue(...))`. Put user text in a variable
  and interpolate it as `{var}` or `{.path {var}}`; cli escapes braces inside
  interpolated values.
- Report at the end: what you changed per file, every deviation from this
  plan, the test files you ran and their pass/fail counts, and any measurement
  you were asked for.

## WP-A: one reader configuration for both paths

Files: `R/DTAFileTabular-class.R`, `R/DTAFileCSV-class.R`, `R/DTAFileTSV-class.R`,
`R/DTAFileDelim-class.R`, `R/DTAFile-class.R`, `R/00_helpers.R` (only the
stream constants/`dta_resolve_stream_mode` region and a new option),
`tests/testthat/test-DTAFile.R`, `tests/testthat/test-load-file-streaming.R`,
`tests/testthat/test-gzip-input.R`, new `tests/testthat/test-streaming-parity.R`.

A1. Replace the two reader builders with one plan. Add
`dta_delim_reader_plan(path, specs, delim, quote, has_header, na, handler)`:
   - open the file once for its header only with `arrow::open_delim_dataset(path,
     delim, quote, col_names = has_header, ...)` (cheap: reads one block) and take
     `names(ds$schema)`; on error, `cli::cli_abort()` naming `{.path {path}}` and
     Arrow's message (duplicate/empty header names currently die with "Could not
     read schema").
   - `cleaned <- if (has_header) dta_clean_column_names(raw) else raw`.
   - `col_types`: when `specs` is not `NULL`, an `arrow::schema()` pinning EVERY
     column to `utf8()` (this is finding 2: the eager path pinned only declared
     columns, so an undeclared column was Arrow-inferred in memory but text when
     streamed, and rule verdicts diverged). When `specs` is `NULL` (a bare
     `read_file()` on a handler), keep inference (`NULL`) so standalone reads
     behave as before.
   - both readers then pass `col_names = cleaned`, `skip = as.integer(has_header)`
     and that `col_types`; there is no second read on either path. Verify with a
     test that `skip = 1` skips the header line and not the first data row on
     both readers, for clean and padded headers, with and without `has_header`.
   - `dta_read_delim_normalized()` and `dta_open_normalized_dataset()` become
     thin wrappers over the plan; `dta_normalize_column_names()` stays for its
     other callers if any (grep).
   - Keep `dta_reader_na_values()` semantics. Keep `quoted_na` defaults.
   - Do not delete `dta_reader_col_types()` (WP-C's file); it is simply no longer
     called from here.
   - Update the tests that pinned the old contract: "read_file leaves a column
     the specs do not mention to inference" (test-DTAFile.R ~399) now asserts
     the column is read as text when specs are supplied, and "read_file without
     specs still infers every column as before" stays. Add a test that the SAME
     file yields identical `validation_status()` and identical flattened
     `validation_errors()` under `stream = "never"` and `"always"` when a
     uniqueness rule reads an undeclared column holding `1.5`, `1.50`, `2`
     (this is the exact reproduction; it failed before).

A2. Handler options for real-world files (finding 7). Add to `DTAFileTabular`
two properties with defaults that leave every existing object and YAML
unchanged: `newlines_in_values` (logical, default `FALSE`) and `encoding`
(character, default `"UTF-8"`). Validate them in the class validator (single
non-NA logical; single non-empty string). Pass them through the plan into both
readers: `parse_options = arrow::csv_parse_options(newlines_in_values = ...)` and
`read_options = arrow::csv_read_options(encoding = ..., block_size = ...)`
(block size: see A3). `DTAFileFactory()` forwards `...`, so YAML keys
`newlines_in_values:` and `encoding:` reach the constructors; confirm that the
YAML export of a handler (grep the code that writes handler fields, e.g. where
`missing_values`/`sep`/`quote` are emitted for `write_dta()`; it may be generic
over properties) round-trips both, and add a test for the round trip. Add
`print_info` lines for both. Tests: a >1.5 MB CSV with quoted newlines loads
and checks identically on both paths with `newlines_in_values = TRUE` (and
fails on both without it); a Latin-1 byte loads with `encoding = "latin1"` on
both paths. Document in the class roxygen.

A3. Make `batch_rows` honest (finding 6). Arrow batches a CSV Dataset by read
block (default 1 MiB), and `Scanner$create(batch_size = )` only slices larger
batches; measured 58 batches of 6,934 rows for `batch_rows` = 1e4, 131072 and
1e6 alike. Do NOT enlarge the default block (memory: Arrow reads ahead many
blocks). Instead: add option `DTAtools.stream_block_size` (bytes, default
`1048576L`, Arrow's own default) applied via `csv_read_options(block_size = )`
in the lazy open; document on `load_file()` and `check()` that a delimited
scan's batch is one read block of about that many bytes and that `batch_rows`
only caps a batch, so memory is governed by block size times Arrow's
read-ahead, not by `batch_rows`. Add a test that a 4 MiB file opened with the
option set to 4 MiB yields one batch while the default yields several.

A4. Finding 5 in `R/DTAFile-class.R`: `dta_check_readable_file()` uses
`cli::cli_abort(stringr::str_glue(...))` twice; replace with cli variable
interpolation. Test: a path containing `{` and `}` produces the intended
"does not match"/"cannot be found" message (`expect_error(..., regexp = "does not match")`).

A5. Randomised parity test, `tests/testthat/test-streaming-parity.R`. A
generator `parity_case(seed)` builds: a spec collection of 3 to 6 columns
mixing `SAS Char` (some with `length`, some non-nullable, some with `values`)
and `SAS Num`/`SAS Int` (read the `DTAColumnSpec` constructor for the exact
argument names), 1 to 3 rules (a `DTARuleColUnique` over one or two columns,
one Char and one Num keyed; a `DTARuleColRange`; a `DTARuleColCondition`; read
their constructors), a data frame of 0 to 400 rows with injected defects
(unconvertible numerics, `"1.5"`/`"1.50"` duplicates, over-length strings,
values outside the permitted set, `NA` in a non-nullable column, censored text
like `"<0.5"` in a numeric-compared column, an undeclared extra column read by
a rule, an undeclared column read by nothing), written with a header that is
sometimes padded or quoted, sometimes gzipped. For each of 25 seeds: load the
file twice (`stream = "never"`, `"always"`), `check(persist = FALSE, quiet = TRUE)`
both, and assert `identical()` on `validation_status()` minus timestamps, on
`as.data.frame(validation_errors(ds, 1))` sorted by every column, and on
`n_import_errors`. Keep the whole file under 20 s. Wrap the expectation so a
failure prints the seed. If a seed fails for a reason outside this plan, pin it
with `expect_true(FALSE)` disabled via `skip()` naming the seed and report it;
do not silently reduce the generator.

## WP-B: dataset and DTA level

Files: `R/DTADataSetTabular-class.R`, `R/DTADataSet-class.R`, `R/DTA-class.R`,
`R/DTADataSetFile-class.R`, `R/validationReporting.R` (only if `results()`/
`messages()` need a zero-target case), `inst/shiny/dta_app/R/utils_dta.R` (only
`dta_bound_item_name`), `tests/testthat/test-DTADataSetTabular-validation.R`,
`tests/testthat/test-DTA.R`, `tests/testthat/test-DTADataSetFile.R`,
`tests/testthat/test-validation-summary.R`, `tests/testthat/test-DTADataSet.R`.

B1. Finding 1. In `method(load_file, DTADataSetTabular)`, on both the lazy and
the eager branch, when `name` is already a table, remove
`x@validation_index[[name]]` and `x@validation_store[[name]]` (the file-dataset
method already does this on replacement; mirror its comment). Test: load,
check, load a different file under the same name, `validation_status()` shows
`not_validated` and `messages()` is empty for that table.

B2. Finding 13. Default table name strips a compression suffix first:
`tools::file_path_sans_ext(dta_strip_compression_extension(basename(file)))`.
Mirror the rule in the app's `dta_bound_item_name()`. Test: `x.csv` then
`x.csv.gz` replace each other under `x`.

B3. Finding 14. Give `handler_index` the default `1` the roxygen promises and
resolve it with `dta_resolve_file_handler_index(handler_index, x@files)`
(defined in `R/DTADataSetFile-class.R`; call it, do not copy it). Tests:
`"1"` works; `NULL`, `NA`, `c(1, 1)` abort with the helper's cli message.

B4. Finding 3. A tabular dataset with no tables must report, not abort:
   - `dta_table_id_to_names(x, NULL)` returns `character(0)` when there are no
     tables (an explicit `tables` selection still aborts "not found").
   - `validation_status()` returns a zero-row frame with the standard columns.
     Generalise `dta_file_empty_status_row()` to take `target_type` and reuse it.
   - `check(DTADataSetTabular)` with no targets: `cli::cli_alert_warning()`
     unless quiet, zero-row summary attribute, return `x`.
   - `check(DTA)`: a dataset with zero targets is INCOMPLETE, never PASSED.
     Add `n_undelivered` (1 when `n_targets == 0`) to the per-dataset summary
     row, fold it into `total_unchecked`, and extend
     `dta_dataset_summary_message()` / `dta_overall_summary_message()` so the
     lines say "no tables loaded". `results()`/`messages()` on such a DTA must
     not abort (check `dta_results_from_status()` with zero rows).
   Tests: dataset-level, DTA-level (`check(dta)` returns, `last_validation_ok`
   is FALSE, `results(dta)` works), and the summary builders.

B5. Finding 8. In the `check(DTADataSetTabular)` loop, before validating a
`Dataset` holding whose `$files` no longer all exist, record a failed target
instead of scanning: details in the file-presence shape
(`dta_file_validation_details(list(ok = FALSE, message = ...))`, message names
the table and path), `table_hash` from `dta_table_change_signal()` (its NA
size/mtime hash is stable while the file stays missing, so the skip gate
applies exactly as for an undelivered file target), index/store entries as for
a validated table, `cli::cli_alert_danger()` unless quiet. Test: delete the
file, `check()` returns `ok = FALSE` with `n_rule_errors = 1`, no abort.

B6. Finding 9. `check(DTA)` gains `fail_fast = FALSE`,
`on_missing_column = c("scan", "stop")`, `use_threads = TRUE`, matched and
forwarded to every dataset; `check(DTADataSetFile)` accepts and ignores them
(same pattern as `batch_rows`). Roxygen on both. Test: `check(dta,
on_missing_column = "stop")` on a DTA whose table lacks a declared column
returns a structural-only result without scanning.

B7. Finding 15. The `validation_artifact_dir` validators of both dataset
classes only require `NULL` or a single non-NA string; `check(persist = TRUE)`
keeps creating the directory. Update any test asserting the old error.

B8. Finding 16. In `check(DTA)`, a non-`DTADataSet` element aborts regardless
of `quiet`.

B9. Finding 5 in these files: `cli::cli_h1(paste0("Dataset: ", ds_name))`,
`cli::cli_rule(paste0("Table ", ...))`, `cli::cli_rule(paste0("File ", ...))`,
`cli::cli_alert_success(paste0("File '", path, ...))`,
`cli::cli_alert_danger(validation_result$message)`, and
`cli_abort(paste0("Dataset '", ds_name, ...))`: interpolate variables instead.
Grep these four files for `paste0(` inside any `cli::` call and fix each. Test:
a table named `a{b}` and a file path containing `{b}` check without error
under `quiet = FALSE` (capture output).

B10. Finding 4, constructor half. `dta_hash_object()` in
`R/DTADataSet-class.R` becomes `rlang::hash(x)` (no tempfile, no compression;
`rlang` is already imported; keep the function name and callers). In the
`DTADataSetTabular` constructor, feed `arrow::as_arrow_table(tbl)` INTO
`dta_coerce_table_to_specs()` for materialised entries so that the returned
Table carries the content stamp WP-C adds (contract below); keep the
`as_arrow_table()` call on the result (a no-op for a Table).

B11. Finding 4, lazy import issues parity. When WP-C's streaming details carry
`import_typing_errors` (see C4), `check(DTADataSetTabular)` writes THAT frame
into `@import_issues[[name]]` for a lazy table, so the property holds the same
axis (import typing only) as the eager path records at load. Until WP-C lands
this field, fall back to `details$import_errors` as today.

## WP-C: streaming engine cost and parity

Files: `R/streamingValidation.R`, `R/streamingScale.R`, `R/importConversion.R`,
`R/validationFunctions.R`, `R/columnSpecChecks.R` (only if needed),
`tests/testthat/test-streaming-validation.R`, `tests/testthat/test-streaming-scale.R`,
`tests/testthat/test-importConversion.R`, `tests/testthat/test-validationFunctions.R`.

C1. Finding 10. `dta_apply_spec_declared_types()` looks the declared type up
once per distinct `errors$column` and maps it back (`match()`); same result,
O(distinct columns). Measure before/after on a 10,000-row frame with one column
(was 1.86 s) and put the number in your report. Add a test that the output is
identical to the previous per-row implementation on a frame mixing three
columns, two of them declared.

C2. Finding 4, engine half. Contract: when `dta_coerce_table_to_specs()`
receives an Arrow Table, the Table it returns (the rebuilt one AND the
early-return original) carries schema metadata key `dta_table_hash` whose value
is `rlang::hash()` of the typed data frame including its `dta_import_issues`
attribute. Set it with Arrow's schema-metadata replacement
(`Table$ReplaceSchemaMetadata()` or `x$metadata$... <-`; verify which arrow 25
API returns a new Table without copying data and without mutating the caller's
object; state which you used). `dta_table_change_signal()` for a `Table` returns
that stamp when present and otherwise `rlang::hash(as.data.frame(x))` (still 9x
cheaper than today's gzip `saveRDS` + md5). Measure `dta_table_change_signal()`
on a 2e6 x 20 Table before/after (was 18.6 s) and report it. Tests: stamp
present after `load_file(stream = "never")` and after the constructor path
(WP-B feeds a Table); changing a cell changes the signal; carried import issues
change the signal (existing test "carried import issues change the table hash"
must still pass).

C3. Finding 12. Let declared-numeric uniqueness keys use Acero. In
`dta_arrow_unique_eligible()`, a key column whose spec declares a numeric type
is eligible when the source is a `Dataset` and the column is `utf8`; return
which key columns are numeric. In `dta_stream_unique_precompute()`, for those
columns group on a normalised cast: `k = as.numeric(col)` (compiles to a cast;
an unparseable value makes Acero error, which the existing `tryCatch` turns
into the per-batch fallback -- exactly the R path), then `k = if_else(is.nan(k),
NA_real_, k)` (R keys `"NaN"` text as missing) and `k = if_else(k == 0, 0, k)`
(R folds `-0`). Text key columns stay as they are. Tests: (i) a clean numeric
key over a Dataset takes the Arrow path (mirror the existing "path is actually
taken" test); (ii) differential test over 20 random seeds: key columns drawn
from `"1.5"`, `"1.50"`, `"2"`, `"2.0"`, `"-0"`, `"0"`, `"NaN"`, `""`, `NA`,
`"1e2"`, `"100"`, `" 3"`, `"abc"` -- the precompute result (or its fallback)
must equal the per-batch accumulator's verdict and duplicate count, and both
must equal `validate_table_detailed()` on the materialised frame; (iii) a key
with `"abc"` falls back (no error). Report the measured time of the Acero path
vs the fastmap path on 2e6 rows of distinct numeric ids.

C4. Parity of the lazy import axis. In `dta_validate_table_stream()`, expose
the import-typing frame on its own as `details$import_typing_errors` (the
collected `carried_sink`, counts exact via its attribute; `NULL` when empty)
alongside the merged `details$import_errors`. `dta_as_validation_details()`,
`as.data.frame.dta_validation_details()`, migration and reporting must ignore
the extra field gracefully (check `results()`, `messages()`, `inspect()`).
Test: for a file with one unconvertible numeric cell and a rule reading a
Char column numerically, `import_typing_errors` holds only the typing row.

C5. Robustness of the batch loop. After `as.data.frame(batch)`, convert any
`vctrs_unspecified` column to `rep(NA_character_, n)` exactly as
`validate_table_detailed()` does (a null-typed column from a Parquet or
user-supplied Dataset otherwise reaches the checks untyped). Test through
`dta_as_batch_reader()` on a Table with an all-NULL column.

C6. Finding 11. `validate_table_detailed()` gains `max_errors = NULL`; when
finite and exceeded, retain only the first `max_errors` rows of `full_error`
and of `import_errors` with `attr(, "truncated") <- TRUE`, while
`n_columnspec_errors`, `n_import_errors` and `summarised_error` stay computed
from the complete frames. `dta_validate_any_table()` forwards `max_errors` to
it. Tests: counts exact, frames capped, verdict unchanged; default (`NULL`)
identical to today so the validation oracle tests keep passing.

## WP-D: finish (after A, B, C report)

Files: `CHANGELOG.md`, generated `man/` and `NAMESPACE`, styling of touched
files, `vignettes/` only if a documented behaviour changed.

D1. `Rscript .github/scripts/style.R` from the worktree root (needs `Rscript`
on PATH: `export PATH="/c/Program Files/R/R-4.5.1/bin:$PATH"`); it styles
`R/` and `inst/` and restores LF endings. Report which files it changed.
D2. `roxygen2::roxygenise()` only after confirming `packageVersion("roxygen2")`
equals `Config/roxygen2/version` in `DESCRIPTION` (8.1.0). Report the `man/`
and `NAMESPACE` diff summary.
D3. `CHANGELOG.md`, under `## [Unreleased]`: a `### Fixed` entry per finding in
user terms and a `### Changed` entry for the reader contract (with specs, every
column is read as text on both paths; undeclared columns are no longer
type-inferred) and the new handler options/option. No version bump.
D4. Run the full suite: `testthat::test_local(".", reporter = "summary")` under
`load_all()`; report every failure and skip with file and test name. If a
failure is in a file no package touched, report it as pre-existing after
checking `git stash`-free: just note it (do not stash).

## Contracts between packages

- WP-A's reader plan makes every column `utf8` when specs are supplied, on both
  paths. WP-C's engines therefore see text for undeclared columns in memory
  too; nothing in WP-C should assume Arrow-inferred types for undeclared
  columns any more.
- WP-C's `dta_coerce_table_to_specs()` stamps `dta_table_hash` on Arrow Table
  input; WP-B's constructor feeds a Table; WP-C's change signal reads the stamp.
- WP-C's streaming details carry `import_typing_errors`; WP-B's `check()` uses
  it for `@import_issues` when present.
- `dta_resolve_file_handler_index()` stays in `R/DTADataSetFile-class.R` and is
  called from the tabular method (WP-B).

## Out of scope (recorded, not done)

- Unifying the two validation engines by routing in-memory Tables through the
  streaming driver (`dta_as_batch_reader()` exists). Worth doing later; the
  parity test from A5 is the safety net for it.
- The per-row `required` error for a missing declared column.
- Export of `read_file()`/`read_file_execution()` without `@examples`.

## WP-E: review fixes (after the adversarial review of WP-C)

Files: `R/importConversion.R`, `R/streamingValidation.R`, `R/streamingScale.R`,
`R/validationFunctions.R`, `R/DTADataSetTabular-class.R` (only the
`@import_issues` write-back in `check()`), and the tests of those files plus
`tests/testthat/test-streaming-parity.R` if a case is added.

E1. Stamp identity, not schema metadata. The content stamp written by
`dta_stamp_table_hash()` rides on the Table's schema metadata, which every
Arrow operation preserves -- `arrow::concat_tables()`, `Table$Slice()`, `[`,
`dplyr::filter() |> compute()` -- so a table changed that way keeps the old
stamp and `check()` skips it with a stale verdict (reproduced: concat a
checked table with itself, every id now duplicated, `check()` reports
status `skipped`, ok `TRUE`). Store the stamp on the R object instead:
`attr(table, "dta_table_hash") <- hash` on the arrow R6 object (an
environment; the attribute is shared by every reference to that object and
absent from any object Arrow builds anew), and read it back with
`attr(x, "dta_table_hash", exact = TRUE)`. Remove the schema-metadata
mechanism and the `ReplaceSchemaMetadata()` call. Tests: (i) a concatenated,
sliced, subset and filtered-and-computed table has no stamp and is
revalidated; (ii) the same object assigned back is still skipped; (iii) a
`Table` with no stamp is revalidated once and skipped on the next `check()`
(the fallback signal is stable); (iv) the constructor path still stamps;
(v) `clear_validation()` leaves the stamp alone.

E2. One content hash for stamp and fallback. `rlang::hash()` of a data frame
depends on attribute ORDER, and the Arrow round trip reorders `class` and
`dta_import_issues`, so for any table with import issues the stamp and the
fallback `rlang::hash(as.data.frame(x))` differ and a re-built table is
rescanned on every `check()`. Add `dta_table_content_hash(df)` that hashes a
canonical list -- `names(df)`, the unnamed column vectors, and the issues
frame reduced to `names`, its unnamed columns and its `n_import_errors`
attribute -- and use it in both `dta_coerce_table_to_specs()` (the stamp) and
the `Table` fallback in `dta_table_change_signal()`. Test: for a table with
one unconvertible cell, the stamp equals the fallback computed on
`as.data.frame()` of the stamped table; and it differs from the hash of the
same table read from a file that differs only in that cell.

E3. Typing axis always present. `dta_validate_table_stream()` sets
`details$import_typing_errors` to `NULL` when the typing axis is empty, which
the consumer in `check(DTADataSetTabular)` cannot tell from "field absent",
so it falls back to the MERGED frame and a streamed table whose only import
errors are rule-time ones writes rule-time rows into `@import_issues` while
the eager path writes none. Make the field always present on the streaming
details: a zero-row `dta_empty_import_errors()` frame carrying
`n_import_errors = 0` when nothing failed to type, otherwise the collected
carried sink WITH the sink's spill attributes (`truncated`, `spilled_rows`,
`spill_dir`) so a capped scan is recoverable. In `check()`, use the field
when `"import_typing_errors" %in% names(details)` and fall back to
`import_errors` only when it is absent (older artifacts). Extend
`collect_full_errors()` with `axis = "import_typing"` reading that frame and
its single spill dir. Tests: (i) rule-time-only errors give identical
`@import_issues` on both paths (none); (ii) typing-only and both-axes cases
unchanged; (iii) `max_errors = 1` with three typing errors: the field has one
row, count attribute 3, and `collect_full_errors(details, "import_typing")`
returns all three.

E4. Silent truncation on the in-memory path. `collect_full_errors()` returns
the retained head with no diagnostic when the frame is flagged `truncated`
and there is no spill dir (the in-memory `max_errors` cap from WP-C). Emit
`cli::cli_warn()` naming the retained and total counts in that case (both
axes). Test with `expect_warning(..., regexp = "of")` on an in-memory result
capped at 3 of 20.

E5. `dta_truncate_error_frame()` treats `NA` or a length != 1 `max_errors` as
"no cap". Abort with `cli::cli_abort()` for `NA`, non-numeric, negative or
length != 1; keep `NULL` and `Inf` as "no cap". Test both.

E6. `dta_arrow_unique_eligible()`: the roxygen now claims a natively typed
(Parquet, double) key column would key differently from the per-batch path;
it would not (verified: the fallback verdict matches). Reword the reason as
"conservative: only text-typed sources have been validated against the
per-batch path", and remove the dead `arrow_dplyr_query` term from
`consumable`, which the next line contradicts.

E7. When done: run `.github/scripts/style.R` (with Rscript on PATH), then
`roxygen2::roxygenise('.')` (8.1.0), then the test files of every file you
touched plus `test-streaming-parity.R`, `test-load-file-streaming.R` and
`test-DTADataSetTabular-validation.R`, and report counts. Do not run the
whole suite (the main thread does). No commits.
