# Plan: streaming follow-ups (after PR #116)

Date: 2026-09-06, amended the same day. Builds on
`PLAN-streaming-parity-robustness.md`, which made the streamed and in-memory
paths read a file identically and pinned that with
`tests/testthat/test-streaming-parity.R`.

## Goal

Tables reach `check()` two ways: read from a delivered file (in memory or
streamed), or constructed in R and checked right away. Both are first-class.
The whole-column engine stays the engine for in-memory tables and is kept
fast and exact; the streaming engine is made cheaper where it is slow;
every declared encoding becomes streamable; the new handler options become
reachable from the app; and the small leftovers go. Every package keeps the
parity test and the oracle tests green and reports a measurement where it
claims a cost change.

A single engine (routing in-memory tables through the batch driver) was
considered and dropped: it would slow tables that fit in memory, expose them
to batch-boundary behaviour they never needed, and concentrate all risk in
the most complex code in the package, for a memory saving of at most one
table-size at check time.

## Ground rules

Those of `PLAN-streaming-parity-robustness.md` apply unchanged: work only
in the review worktree of the branch, touch only your package's files,
targeted tests only (the finish package runs the suite), `cli` only,
namespaced calls, a test per behaviour change, no version bump, no
commits, and nothing that attributes the work to anyone but its author.
Measure with `system.time()`. Reference inputs: a synthetic 170 MB /
1e6 x 20 CSV (10 declared `SAS Num`, 10 declared `SAS Char`, one
uniqueness rule), a dirty variant with one unconvertible value per 1,000
rows in every numeric column, and for the constructed-table package a
1e6 x 20 data frame with the same shape built in R.

## WP-K: the constructed-table path

Files: `R/DTADataSetTabular-class.R` (constructor and its roxygen; the
`print()`/`print_short_info()` methods in this file), `R/importConversion.R`,
`tests/testthat/test-importConversion.R`, `test-streaming-parity.R`,
`test-validation-oracle.R`, `test-DTADataSetTabular-validation.R`.

K1. No Arrow detour at construction. The constructor now converts a data
frame to an Arrow Table, which the coercion converts back to a data frame
and then to a Table again, only so the content stamp lands on a Table. For
a data frame input: coerce the data frame directly (as before), build the
Table once with `arrow::as_arrow_table()`, and stamp it with
`dta_table_content_hash()` of the coerced frame (issues attribute included)
through the existing stamp setter. For an Arrow Table input keep the
current route (the coercion stamps it). Measure construct-plus-`check()`
on the 1e6 x 20 data frame before and after, and peak R heap.

K2. Remove `dta_reader_col_types()` and its tests; it has had no production
caller since the reader plan. Confirm with a grep over `R/`, `inst/`,
`tests/`, `benchmarks/`, `vignettes/`.

K3. Parity for R-typed tables. Add to `test-streaming-parity.R` a second
generator, `parity_case_memory(seed)`, that builds a data frame in R with
double, integer, character, factor, logical, `Date` and `POSIXct` columns
(and `integer64` when bit64 is installed), specs declaring some of them as
`SAS Char`/`SAS Num`/`SAS Int` with `length`, `nullable` and `values`, an
undeclared numeric column read by a rule, and the same rule mix as the file
generator. Compare, per seed: `check()` on `DTADataSetTabular(tables =
list(t = df))` (whole-column engine on typed data) against the streaming
engine over `dta_as_batch_reader(df, batch_rows = <small>)` with `coerce =
TRUE` and `known_columns = names(df)`, on `validation_status()` minus
timestamps, the flattened errors sorted on every column, and
`n_import_errors`. Keep values inside the integer range so the pinned Int
narrowing defect is not triggered; say so in a comment. Add the same
R-typed cases to the oracle corpus if the corpus helper admits them.

K4. Detail cap. `max_errors` (default 10,000) now caps retained detail for
in-memory tables too. Keep the cap (unbounded frames were a memory finding)
but document on `check()`, `DTADataSetTabular()` and `collect_full_errors()`
that a table constructed for an immediate check with complete detail wanted
should pass `max_errors = Inf`, and that counts and verdicts are exact
regardless.

K5. Brace-safe printing in this file: `print()`/`print_short_info()` paste
dataset and table names into `{.field ...}` markup; interpolate them as
variables and test with a table named `a{b}`.

## WP-G: numeric parsing in Arrow for clean batches

Files: `R/streamingValidation.R` (the batch loop only), a new helper in
`R/importConversion.R`, `tests/testthat/test-streaming-validation.R`,
`test-importConversion.R`. Runs after WP-K (shared files).

Profiling shows about half of a streamed check is R parsing declared-numeric
columns that were read as text. Arrow 25 has no cast that tolerates a bad
value, so the cast can only be used when a batch is known to be clean.

G1. Per batch, for every declared-numeric column still held as `utf8`:
evaluate `match_substring_regex` in Arrow with a pattern that accepts only
strings on which `as.numeric()` and Arrow's `cast` are known to agree
bitwise (start from `^[+-]?(?:[0-9]+\.?[0-9]*|\.[0-9]+)(?:[eE][+-]?[0-9]+)?$`
and narrow it by experiment: `"1."`, `".5"`, `"+4"`, `"1e5"`, `"1E5"`,
`"-0"`, `"007"`, 20-digit mantissas, exponents past 308). If every non-null
value matches, cast the column to `float64` in Arrow and hand the batch to R
with numbers; otherwise leave it as text and let the existing R path record
the unconvertible values. Integer narrowing stays in R as today.

G2. Differential test: 50,000 random numeric strings in the accepted forms
parsed both ways must be identical doubles; a batch with one bad value must
take the R path and report exactly what it reports today.

G3. Measurement gate: rows per second of `check()` on the clean 170 MB file
streamed, and on the dirty variant (must not get slower). Report the profile
shares before and after.

## WP-H: streaming a non-UTF-8 file, and reader examples

Files: `R/DTAFileTabular-class.R`, `R/DTAFile-class.R`, `R/DTAFileCSV-class.R`,
`R/DTAFileTSV-class.R`, `R/DTAFileDelim-class.R`, `R/00_helpers.R` (an
option and a cache environment), `tests/testthat/test-DTAFile.R`,
`test-load-file-streaming.R`, `test-gzip-input.R`.

H1. `dta_transcode_to_utf8(path, encoding)`: read the file through an R
connection opened with that encoding (`gzfile` for `.gz`) in blocks of
65,536 lines and write UTF-8 to `tempfile(fileext = ".csv")`; bounded
memory, linear time. Cache per session keyed by path, size, mtime and
encoding in a package environment so a repeated `load_file()` reuses it.

H2. `dta_open_normalized_dataset()` opens the transcoded copy when the
declared encoding is not UTF-8, replacing today's refusal. The change signal
of such a Dataset must identify the ORIGINAL file (path, size, mtime), not
the copy: carry the original path as an attribute on the Dataset's R object
(the same mechanism as the table stamp) and read it in
`dta_table_change_signal()`.

H3. Tests: a Latin-1 file loads and checks identically on both paths; a
`.gz` Latin-1 file too; a second `load_file()` reuses the copy; the copy is
under `tempdir()`. Measure the transcoding rate in MB/s and state the disk
cost in the roxygen of `encoding`.

H4. Runnable `@examples` for the exported `read_file()`,
`read_file_execution()` and `open_file_execution()` (the repo requires one
per exported function), using the bundled `clinical_data.csv`.

## WP-I: the app reaches the new handler options

Files: `inst/shiny/dta_app/R/*.R` handler editing and serialisation,
`inst/shiny/dta_app/app.R` only where the editor is wired,
`tests/testthat/test-shinyapp-file-handlers.R`, and the manifest resync.

I1. The handler editor gets a checkbox for `newlines_in_values` and a
select for `encoding` (`UTF-8`, `latin1`, `windows-1252`, plus free text).

I2. `dta_handler_to_list()` drops `sep`, `quote`, `has_header` and
`missing_values` as well as the two new fields. Make it generic over the
handler's tabular properties so a document saved from the app carries every
reader setting the YAML can express; add a YAML -> app -> YAML round-trip
test for a handler with every field set.

I3. Run `Rscript .github/scripts/bump_version.R --sync-manifest` after the
app edits and report the checksum diff.

## WP-J: leftovers outside the files above

Files: `R/DTA-class.R`, `R/DTADataSet-class.R`, `R/DTADataSetFile-class.R`,
`R/DTAColumnSpec*-class.R`, `R/DTARule*-class.R`, `R/DTAMetaData-class.R`
(only their `print()`/`print_short_info()`/`print_info()` methods),
`R/validationFunctions.R` (`validate_table()` gets `@examples`),
`tests/testthat/test-examples.R` if it enumerates example files, and one
test file per class touched. Runs after WP-K (so the DTADataSetTabular
methods are already done there).

J1. Every print method that pastes a user-supplied name into `{.field ...}`
or `{.emph ...}` markup interpolates it as a variable; test one class per
file with a name containing `{`.

J2. `validate_table()` gets a runnable example.

## Finish

Style script, roxygen2 8.1.0, changelog entries under Unreleased for every
user-visible change, manifest resync, the four `.github/scripts` guards,
the full suite, and the perf script with before/after numbers. Then a
branch off this one, a commit, and a PR stacked on #116 (retarget to `dev`
once #116 merges). CI green on all matrix targets before any merge.

## Sequencing

Wave 1 in parallel: WP-K, WP-H, WP-I. Wave 2 in parallel: WP-G, WP-J.
Then one adversarial review of K, G and H together, a fix package if it
finds anything, and the finish.

## WP-L: review fixes (after the adversarial review of K, G and H)

Files: `R/DTAFileTabular-class.R`, `R/00_helpers.R` (the transcode cache),
`R/DTADataSetTabular-class.R` (the `check()` loop and roxygen),
`R/importConversion.R`, `R/DTA-class.R` (roxygen only), and the tests of
those files (`test-DTAFile.R`, `test-load-file-streaming.R`,
`test-gzip-input.R`, `test-DTADataSetTabular-validation.R`,
`test-importConversion.R`, `test-streaming-validation.R`).

L1. A transcoded dataset must follow its delivery. The lazy holding of a
non-UTF-8 file is an Arrow Dataset over the UTF-8 copy made at
`load_file()` time; when the delivery changes afterwards, the change signal
(now keyed on the original) says "changed", the skip gate opens, and the
scan reads the stale copy -- a clean verdict is reported as fresh for data
that now fails. Carry on the Dataset's R object everything needed to
re-open it: the original path, the declared encoding, the source
fingerprint (size, mtime) the copy was made from, and the exact
`open_delim_dataset()` argument list. In the `check()` loop, before the
missing-file guard, call a `dta_refresh_transcoded_dataset(table)` that
returns the object unchanged when the source fingerprint still matches,
and otherwise transcodes again (through the cache) and re-opens with the
stored arguments, stamping the new object; write the refreshed object back
into `x@tables[[name]]`. Test: load a Latin-1 file lazily, check (ok), add
a spec-violating row to the delivery, check again: ok is FALSE and the
row count is the new one; the UTF-8 control behaves the same.

L2. Byte-faithful transcoding. `readLines()` normalises CRLF and lone CR to
LF, so a CRLF inside a quoted value is one character shorter when streamed
than in memory. Rewrite `dta_transcode_to_utf8()` to work on raw blocks:
read `readBin(con, "raw", n = block_bytes)` (a 4 MiB block; `gzfile()` for
`.gz`), cut each block at its last 0x0A byte and carry the remainder into
the next block (safe for every ASCII-compatible single- and multi-byte
encoding; wide encodings stay refused), refuse a block containing 0x00 with
a cli message naming the file, convert with `iconv(rawToChar(bytes), from,
"UTF-8")` and write the result back as bytes with `writeBin(charToRaw())`.
Nothing else about the copy changes (header intact, cache, tempdir). Test
that a quoted "a\r\nb" value is identical on both paths, that a lone CR
inside a value survives, and that the 170 MB rate is not worse than before
(report it and update the roxygen figure).

L3. `dta_frame_is_arrow_stable()`: a Date or POSIXct column is stable only
when its storage is double (an integer-storage Date comes back double).
Add the `is.double()` clause and a test for both storages.

L4. `dta_missing_table_files()` must identify a transcoded dataset by its
delivery, not by the copy: use `dta_dataset_source_files()` there, so a
deleted delivery is reported by its own path and a deleted copy is simply
re-made by L1. Test both.

L5. An unsupported encoding name (`"latin-1"`, `"cp-1252"`) currently
surfaces as a translated base-R `iconv()` error. Validate the name up front
with `iconv(character(0), from = encoding, to = "UTF-8")` in a `tryCatch()`
and abort with `cli::cli_abort()` naming the file, the handler's declared
encoding and `iconvlist()` as the place to look. Test with `expect_error(...,
class = "rlang_error")` and a regexp on the encoding name.

L6. Document the two new options where users read: `DTAtools.stream_arrow_numeric`
(diagnostic switch, default TRUE) and `DTAtools.transcode_block_lines` (or
its byte-based successor from L2) on `check()` and `load_file()` next to
`DTAtools.stream_block_size`; and add the `max_errors = Inf` sentence to
`check(DTA)`'s roxygen (`R/DTA-class.R`), matching the dataset-level text.

L7. The Arrow numeric backoff never resets after a successful batch; reset
the column's backoff to 1 on success so a column that is clean 99.9% of the
time regains the fast path. Test with a dirty-clean-clean-clean sequence.

L8. Transcode cache eviction: when a new key is written for a path that
already has a copy (a re-delivery with a new size or mtime), unlink the
superseded copy and drop its entry, so a session holds at most one copy per
delivery. Test that the old copy is gone after a rewrite.

L9. Run `.github/scripts/style.R` (with `--vanilla`), then
`roxygen2::roxygenise('.')` (8.1.0) so `man/dta_reader_col_types.Rd` is
removed and `DTAFileTabular-class.Rd` no longer says a non-UTF-8 file cannot
be validated lazily; confirm `NAMESPACE` is unchanged.

L10. Run the six test files above plus `test-streaming-parity.R` and
`test-validation-oracle.R`; report counts and any failure text.
