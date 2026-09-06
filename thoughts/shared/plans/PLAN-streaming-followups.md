# Plan: streaming follow-ups (after PR #116)

Date: 2026-09-06. Builds on `PLAN-streaming-parity-robustness.md`, which
made the streamed and in-memory paths read a file identically and pinned
that with `tests/testthat/test-streaming-parity.R`. This plan closes what
that work left open, in the order of value.

## Goal

One validation engine, most of the per-row work in Arrow's C++ rather than
R, every declared encoding streamable, the new handler options reachable
from the app, and the small leftovers gone. Every package keeps the parity
test green and reports a measurement where it claims a cost change.

## Ground rules

Those of `PLAN-streaming-parity-robustness.md` apply unchanged: work only
in the review worktree of the branch, touch only your package's files,
targeted tests only (the finish package runs the suite), `cli` only,
namespaced calls, a test per behaviour change, no version bump, no
commits, and nothing that attributes the work to anyone but its author.
Measure with `system.time()` on a synthetic 170 MB / 1e6 x 20 CSV like the
one used for the previous plan's numbers (10 declared `SAS Num`, 10 declared
`SAS Char`, one uniqueness rule), plus a dirty variant with one
unconvertible value per 1,000 rows in every numeric column.

## WP-F: one validation engine

Files: `R/streamingValidation.R`, `R/streamingScale.R`,
`R/DTADataSetTabular-class.R` (only `check()`), `R/validationFunctions.R`
(only if a helper must be shared), tests of those files, and
`tests/testthat/test-validation-oracle.R` only to add cases.

Today `dta_validate_any_table()` sends an in-memory `Table` to
`validate_table_detailed()` and a lazy holding to the batch driver. The two
now agree because the reader feeds them identical data, but they are still
two implementations of every check, and the in-memory one materialises the
whole frame a second time inside `check()`.

F1. Route a `Table` through the batch driver. In `dta_validate_any_table()`
wrap a non-lazy table as `arrow::InMemoryDataset$create(table)` and take
the Dataset branch: structural gate, uniqueness precompute, projection,
`Scanner$create(batch_size = batch_rows)`, `dta_validate_table_stream()`.
`validate_table_detailed()` stays as the reference implementation used by
`validate_table()` and the oracle tests; `check()` no longer calls it.

F2. Carried import issues. An eagerly loaded table carries its load-time
typing issues as the `dta_import_issues` attribute in the Table's R
metadata, and `validate_table_detailed()` reads them; the batch driver does
not. Read them once, before the scan, from `as.data.frame(table$Slice(0, 0))`
(verify the 0-row conversion restores attributes) and seed the driver's
carried sink with that frame and its `n_import_errors` count. Then measure
whether per-batch `as.data.frame(batch)` deserialises the metadata on every
batch (it applies R metadata on each conversion); if it costs more than a
few percent on the 170 MB file, scan a copy of the table whose schema
metadata has been dropped (`ReplaceSchemaMetadata(NULL)` shares buffers) and
add parity cases with `integer64`, factor, `Date` and `POSIXct` columns so
nothing that depended on that metadata is lost silently.

F3. Uniqueness on typed numeric columns. `dta_arrow_unique_eligible()`
admits only `utf8` key columns; an in-memory table has `float64`/`int32`
declared-numeric keys after coercion. Admit a key column whose Arrow type is
numeric and whose spec declares a numeric type, group on the zero-folded
value without a cast, and extend the differential test in
`test-streaming-scale.R` with typed numeric keys on an `InMemoryDataset`.

F4. Contract of `check()` for in-memory tables afterwards: `max_errors`,
`fail_fast`, `on_missing_column`, `use_threads` and progress all apply; the
details are the streaming shape (tagged, `n_rows_scanned`,
`import_typing_errors`); `@import_issues` for an eagerly loaded table stays
what `load_file()` recorded (do not overwrite it from the scan). Update the
roxygen of `check()` accordingly.

F5. Tests and gates. Every corpus case checked through `check()` on an
in-memory table must equal `validate_table_detailed()` field for field (the
oracle file already does this for the streamed path; add the in-memory
route). The parity test must stay green. Measure `check()` on the 170 MB
in-memory table before (1.7 s) and after, and its peak R heap; the second
full materialisation should be gone.

## WP-G: numeric parsing in Arrow for clean batches

Files: `R/streamingValidation.R` (the batch loop only), a new helper in
`R/importConversion.R`, their tests. Runs after WP-F.

Profiling shows about half of a streamed check is R parsing declared-numeric
columns that were read as text (`as.numeric()` plus its `is.na()` passes).
Arrow 25 has no cast that tolerates a bad value, so the cast can only be
used when the batch is known to be clean.

G1. Per batch, for every declared-numeric column still held as `utf8`:
evaluate `match_substring_regex` in Arrow with a pattern that accepts only
strings on which `as.numeric()` and Arrow's `cast` are known to agree
bitwise (start from `^[+-]?(?:[0-9]+\.?[0-9]*|\.[0-9]+)(?:[eE][+-]?[0-9]+)?$`
and narrow it by experiment: test `"1."`, `".5"`, `"+4"`, `"1e5"`, `"1E5"`,
`"-0"`, `"007"`, 20-digit mantissas, and exponents past 308 on both
parsers). If every non-null value matches, cast the column to `float64` in
Arrow and hand the batch to R with numbers; otherwise leave the column as
text and let the existing R path record the unconvertible values. Integer
narrowing stays in R as today.

G2. Differential test: 50,000 random numeric strings in the accepted forms,
parsed both ways, must be identical doubles; and a batch with one bad value
must take the R path and report exactly what it reports today.

G3. Measurement gate: rows per second of `check()` on the clean 170 MB file
streamed and in memory, and on the dirty variant (which must not get
slower). Report the profile shares before and after.

## WP-H: streaming a non-UTF-8 file

Files: `R/DTAFileTabular-class.R`, `R/DTAFile-class.R` (only if the
refusal message lives there), `R/00_helpers.R` (an option), tests in
`test-DTAFile.R` and `test-load-file-streaming.R`.

`open_file()` refuses `encoding != "UTF-8"` because the dataset scanner has
no re-encoding step. Replace the refusal with a one-time transcoding pass.

H1. `dta_transcode_to_utf8(path, encoding)`: read the file through an R
connection opened with that encoding (a `gzfile` connection for `.gz`) in
blocks of 65,536 lines and write UTF-8 to `tempfile(fileext = ".csv")`;
bounded memory, linear time. Cache the result per session keyed by path,
size, mtime and encoding so a repeated `load_file()` reuses it; the cache
lives in a package environment and is not user configuration.

H2. `dta_open_normalized_dataset()` opens the transcoded copy when the
declared encoding is not UTF-8. The change signal of such a Dataset must
identify the ORIGINAL file (path, size, mtime), not the temporary copy;
carry the original path on the Dataset object the way the table stamp is
carried (an attribute on the R6 object) and read it in
`dta_table_change_signal()`.

H3. Tests: a Latin-1 file loads and checks identically on both paths; a
`.gz` Latin-1 file too; a second `load_file()` reuses the copy; the copy
disappears with the session. Measure the transcoding rate in MB/s and state
the disk cost in the roxygen of `encoding`.

## WP-I: the app reaches the new handler options

Files: `inst/shiny/dta_app/R/*.R` handler editing and serialisation only,
`inst/shiny/dta_app/app.R` only where the editor is wired,
`tests/testthat/test-shinyapp-file-handlers.R`, and the manifest resync.

I1. The handler editor gets a checkbox for `newlines_in_values` and a
select for `encoding` (`UTF-8`, `latin1`, `windows-1252`, plus free text).

I2. `dta_handler_to_list()` currently drops `sep`, `quote`, `has_header`
and `missing_values` as well as the two new fields. Make it generic over
the handler's tabular properties so a document saved from the app carries
every reader setting the YAML can express, and add a round-trip test
(YAML -> app -> YAML) for a handler with every field set.

I3. Run `bump_version.R --sync-manifest` after the app edits; report the
checksum diff.

## WP-J: leftovers

Files: `R/importConversion.R` (remove `dta_reader_col_types()` and its
tests: it has no production caller since the reader plan), `R/DTAFile-class.R`
and `R/DTAFileTabular-class.R` (runnable `@examples` for `read_file()`,
`read_file_execution()`, `open_file_execution()`), `R/validationFunctions.R`
(`validate_table()` gets `@examples`), the `print()` and
`print_short_info()` methods of every class that pastes a user-supplied name
into `{.field ...}` markup (interpolate the name as a variable so a brace in
a dataset or table name cannot break printing; test with a name containing
`{`), and `tests/testthat/test-examples.R` if it lists example files.

## Finish

Style script, roxygen2 8.1.0, changelog entries under Unreleased for every
user-visible change, the full suite, the four `.github/scripts` guards, and
the perf script with before/after numbers in the PR body. Then CI on the
PR, all matrix targets green, before any merge.

## Sequencing

WP-F, WP-H, WP-I and WP-J touch disjoint files and can run in parallel.
WP-G edits the batch loop WP-F reshapes and runs after it. The finish runs
last. Each package gets an adversarial review before the finish; the parity
test and the oracle file are the two safety nets every package must leave
green.
