# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/), and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- **A file handler for deliverables that are never parsed: `DTAFileAny`,
  written `type: any` in YAML.** A `DTADataSetFile` exists to confirm that
  PDFs, archives, reports and raw instrument output arrived intact, but the
  only handler types on offer were `csv` and `tsv` — so such a file had to be
  declared as something it was not.

  The handler optionally carries `extensions`, an **open** allow-list of file
  endings (`extensions: [pdf, zip]`). Left unset, any ending is accepted.
  Because the list is open rather than a fixed enum, a delivery of `.xpt` or
  `.sas7bdat` files needs no change to this package. Entries are normalised on
  construction — lower-cased, leading dot removed — so `.PDF` and `pdf` declare
  the same thing, and a compressed delivery satisfies the ending underneath it
  (`report.pdf.gz` passes `extensions: [pdf]`, exactly as `data.csv.gz` already
  matches a handler declared `data.csv`).

  The restriction is enforced by `matches_filename()`, so a file with the wrong
  ending is refused as it is offered rather than at validation time.

- **`load_file()` for `DTADataSetFile`.** A file dataset can now be populated
  the same way a tabular one is, binding each file to a declared handler as it
  arrives, instead of only through the `paths =` constructor shortcut.

  Nothing is read or stat-ed when a file is bound: whether it exists, is
  non-empty and can be opened remains the whole contract of `check()`, which is
  what lets a specification bind a file that has not arrived yet and report it
  as missing. The delivered name is still checked against the handler, as it is
  for a tabular dataset.

- **`clear_validation()` for `DTADataSetFile`**, matching the tabular method.

### Changed

- `__DTAtools_supported_file_types__` is split in two. `type:` was answering two
  unrelated questions with one list: *which reader parses this file* (tabular
  only) and *what may this file be* (file datasets). Tabular handlers now
  validate against `__DTAtools_supported_file_types_tabular__` (`csv`, `tsv`)
  and file datasets against `__DTAtools_supported_file_types_file__`, which adds
  `any`.

- `DTADataSetTabular` now requires every file handler to be readable (a
  `DTAFileTabular`). A tabular dataset parses everything it is given, so a
  reader-less handler used to construct happily and then abort deep inside the
  read, naming the wrong problem long after the document that caused it was
  accepted. This is also what keeps `type: any` out of a tabular dataset without
  threading the dataset's type through the YAML reader.

- `DTADataSetFile(paths = ...)` builds `DTAFileAny` handlers rather than bare
  `DTAFile` ones, so a dataset built that way round-trips through YAML.

- **The Shiny app no longer lets a Files dataset's handler be declared `csv` or
  `tsv`.** Adding a file to such a dataset offered a choice of `any`, `csv` and
  `tsv`; a `DTADataSetFile` never reads a row, so the two parsing types
  described a parse that never happens and invited a PDF or an archive to be
  declared as something it was not. The type is now fixed at *Any file (not
  parsed)* and shown read-only, and a `csv`/`tsv` save is refused with a
  sentence saying why. Tabular datasets are unaffected — they still choose
  between `csv` and `tsv`, and are still never offered `any`.

  A document written by hand that declares a parsing handler inside a file
  dataset still loads and still works; opening that handler in the editor and
  saving it will retype it to `any`.

- The bare `stop()` in the `load_file()` fallback is now a `cli::cli_abort()`
  naming the class it could not dispatch on.

### Fixed

- **The Shiny app's downloadable validation summary announced *VALIDATION
  PASSED* while a dataset was still missing its data.** With several datasets
  in a DTA, checking them all leaves any dataset whose files never arrived at
  *No data* — it is skipped, not validated. The summary counted only the
  datasets that were actually validated, found no failures among them, and
  certified the whole DTA as passed; the skipped dataset appeared in the table
  below the banner, contradicting it.

  The banner is now three-state and is only green when **every** dataset in the
  DTA was validated and passed. A dataset that is *No data* or *Not validated*
  yields *VALIDATION INCOMPLETE* (amber) together with a line naming what is
  missing, and any failure yields *VALIDATION FAILED* — previously a failure
  was also reported as merely "incomplete". The counts line now includes
  datasets still awaiting validation, not just those without data.

- **Two different downloads were both called the validation report.** The
  sidebar button is now **"Validation summary"** — the whole-DTA outcome, one
  row per dataset, downloading as `validation_summary_<timestamp>.html` — and
  is styled amber rather than green when it would report an incomplete run. The
  **"Report"** button in the Validation messages dock is unchanged and remains
  the message-level `write_validation_report()` output; both now carry tooltips
  saying which is which.

- **Uploading into a Files dataset reported *"subscript out of bounds"* in the
  Loaded files panel, and the file was bound anyway.** The upload itself
  succeeded — which is why a second attempt offered to overwrite a file the
  user had never seen arrive — and the panel then threw while drawing the
  per-file pass/fail tick.

  The tick looks a bound item up in the dataset's status map, defaulting to
  *pending* when it is absent, and absent is the normal state for a file that
  has just arrived: a file dataset's `validation_status()` is empty until
  `check()` runs, where a tabular dataset already carries a pending row per
  loaded table. That default was written `map[[name]] %||% "pending"`, which
  cannot work — the status map is an atomic character vector, `[[` on an atomic
  vector *throws* for an absent name rather than returning `NULL`, and the
  error is raised while evaluating the left operand, so `%||%` never runs. On a
  list the same expression is fine, which is why only the file datasets broke.

  Every read of a status map now goes through one helper that is null-safe for
  both shapes. Only the Loaded files panel was reachable from the reported
  steps; the other five reads — the dataset nav's per-dataset icon, the dataset
  status line, two in the Raw YAML apply and the validation report builder —
  spell the lookup the same way and are safe only because each happens to
  iterate the map's own names. Nothing enforced that, so they go through the
  helper too.

  The suite drove this whole journey already — add a Files dataset, declare a
  file, upload, validate, overwrite — and still missed it, because it only ever
  inspected the reactive state afterwards and never asked for the output. The
  Loaded files panel is now rendered by two tests, one either side of
  `check()`.

- **Files could not be uploaded into a Files dataset in the Shiny app.**
  Dropping a file reported *"This method needs to be implemented in derived
  classes"* — the text of the unimplemented `load_file()` stub, surfaced
  verbatim to the user. Three further defects sat behind it, each of which would
  have surfaced as soon as the first was fixed:

  - `clear_validation()` had no `DTADataSetFile` method either, so the *second*
    upload of a file, and every specification edit, would still have failed.
  - The app keyed a bound file by its basename *without* the extension, while
    the package keys it *with* — silently breaking overwrite detection, the
    per-file pass/fail tick and the remove button. Both sides now agree, through
    one helper.
  - `validation_status()` returned `NULL` rather than an empty table for a
    dataset with nothing validated yet, which `check()` then measured with
    `nrow()`.

- The Files editor announced *"This dataset expects no files at all, so nothing
  can be loaded into it"* in the red styling used for save failures, for a
  dataset the user had merely not finished configuring — and neither the Columns
  nor the Rules editor has any equivalent. The empty state now sits in the table
  where the eye already is, and the dataset page points at the editor rather
  than stating a dead end.

- A dataset that could not report its results made every *other* dataset look
  unvalidated. The app asks `results()` for the whole document at once, and that
  call aborts outright if any one dataset cannot answer — a tabular dataset with
  no data loaded raises *"No tables found in dataset."* — so checking a dataset
  that passed left nothing turning green. Each dataset is now asked separately
  when the combined call fails, keeping the failure local to the dataset that
  actually cannot report.

- `load_file()` on a `DTA` chose the name a bound item is stored under before it
  knew which kind of dataset it was dispatching to, always stripping the file's
  extension. That is right for a tabular dataset, whose tables are named after
  the file, and wrong for a file dataset, which keys by the delivered name in
  full — so a file re-delivered through the `DTA` API was *appended* rather than
  replaced, growing `file_paths` on every call and leaving the duplicate entries
  disambiguated as `<path>_1`, `<path>_2`. The name is now left to whichever
  dataset method receives it, each of which already had the right default.

## [0.21.0] - 2026-08-24

### Added

- **Datasets can be added and removed in the Shiny app.** A new
  **+ Add dataset** button under the dataset list creates an empty dataset,
  choosing between a **Tabular** dataset (validated column by column against a
  specification) and a **Files** dataset (which only checks that the expected
  files arrive). Tabular is the default. A dataset is removed through
  **Edit → Remove dataset**, below a divider and styled as destructive because
  it deletes the dataset and unloads its files rather than opening an editor.

  A dataset's `type` is fixed when it is created. The property is a plain
  character whose validator only checks set membership, so assigning it would
  produce an object claiming to be file-backed while still carrying `@specs`
  and `@tables` — everything downstream dispatches on the S7 class, not the
  string. Changing a type therefore means adding a new dataset and removing the
  old one, and no control offers to do it in place.

  New datasets are appended to the end of the list. Every nav button, upload
  slot and example picker is keyed by a dataset's *position* and resolves its
  name only at click time, so appending is the one way the list can grow
  without silently repointing an existing control at the wrong dataset.

- **An Edit mode switch in the header, off by default.** Until it is turned on
  the document is read-only: the dataset **Edit** menu is hidden, the Metadata
  tab renders its values as plain text instead of form controls (contacts
  included, with no add/edit/remove controls), the Raw YAML editor is read-only
  with no **Apply changes** button, and datasets can be neither added nor
  removed.

  Every surface that writes to the specification is gated twice — the control
  is not rendered, *and* the observer behind it re-checks. Hiding a control is
  not sufficient on its own: an input that is off screen can still be driven
  over the websocket, the contact observers are registered for every contact
  the moment a document loads, and the Metadata tab's fields save through a
  700ms debounce that would otherwise flush after the switch was already turned
  off. Turning Edit mode off also closes any open editor and clears the dataset
  it targeted, so a save handler armed earlier in the session cannot fire
  against a document that has since become read-only.

  What Edit mode gates is the *specification* — columns, rules, file handlers,
  dataset and document metadata, contacts, the raw YAML, and adding or removing
  datasets. It deliberately does not gate working with *data* against that
  specification: loading a document, uploading and unloading files, and running
  checks all stay available, since validating a transfer is what most users
  open the app to do.

  While read-only, a contact is shown in full — email, department, phone,
  address and the signature/reviewer flags. Editable mode keeps those one click
  away behind the edit modal, but read-only has no click to offer, so the
  information would otherwise be unreachable.

### Changed

- The dataset Edit menu's **Metadata** entry is now called **Details**, and the
  modal it opens is titled to match. The input id is unchanged.
- The **+ Add dataset** button is quieter — it sat directly above "Check all
  datasets" and read as a competing primary action rather than a small addition
  to the list above it.
- The Edit mode switch is vertically centred against the "Report issues" and
  "About" links. `.app-actions` had no `align-items`, so it defaulted to
  `stretch` and the switch, which has no vertical padding of its own, sat off
  the pills' centre line.

### Fixed

- **The Columns and Rules editors are no longer offered for a file dataset.**
  Both act on `ds@specs`, which only `DTADataSetTabular` has. This was not
  merely a redundant menu entry: `dta_column_ids()` swallows the missing
  property with `tryCatch(...) %||% list()` rather than erroring, so on a
  `DTADataSetFile` the column editor opened *empty* and let the user add a
  column that had nowhere to be stored. The Edit menu now takes the dataset's
  type and offers Files, Details and Remove for both kinds, Columns and Rules
  for tabular only.

- **The Raw YAML editor can be resized, and Ace follows.** The editor is taller
  (70vh, down to 30vh) and the box has a drag handle. A CSS `resize` handle
  alone is not enough twice over: `shinyAce::aceEditor(height =)` writes a fixed
  inline style that only `!important` overrides, and Ace does not observe its
  own container, so its canvas has to be told to re-lay-out. The
  `ResizeObserver` that does that is now started on `DOMContentLoaded` — the
  script is registered in `tags$head()`, where `document.body` is still `null`,
  and `MutationObserver.observe(document.body)` threw there and aborted the
  whole script silently, leaving the editor permanently unwired.

- **A tabular dataset with no columns can be read back after being written.**
  `specs_from_list()` rejected an absent `columns:` key outright, while the
  serializer omits that key entirely for a dataset that has none — so the
  package could not parse YAML it had itself just produced. Reaching it needed
  no new feature: deleting a dataset's last column in the column editor was
  enough, after which **Apply changes** on the Raw YAML tab, and reloading an
  exported document, both failed with `` `columns` must be a list. `` An absent
  key is now read as "no columns declared", which is also what every newly
  added dataset starts as.

## [0.20.1] - 2026-08-24

### Fixed

- **The Shiny app can be deployed to Posit Connect again.** Both `master` and
  `dev` shipped an app manifest whose `DTAtools` entry had no `GithubSHA1` /
  `RemoteSha`. Connect builds the package's archive download URL from that SHA,
  so every deploy failed in Connect's installer with
  `if (!grepl("^http", archiveUrl)) : argument is of length zero` before
  anything else ran. `dev` was doubly broken: its `GithubRef` named `v0.20.0`,
  a tag that was never cut.

  The cause was that the version tooling assumed a release tag is the only
  thing ever deployed. `bump_version.R` rewrote the ref to `v<version>` on
  every bump and cleared the SHA, on the reasoning that an absent field fails
  loudly until the release exists. That holds for `master`, deployed at a tag;
  it does not hold for `dev`, which is deployed continuously from the branch.
  Refs are now either release-shaped (`v1.2.3`, tracks `DESCRIPTION`, clears
  the SHA on a bump) or a branch (left alone by a bump, since neither the ref
  nor its SHA is invalidated by a version change).

### Changed

- `check_manifest.R` now **requires** `GithubSHA1` / `RemoteSha` rather than
  treating an absent one as expected. That single exemption is why CI reported
  the manifest healthy through the entire outage. A release ref must equal its
  tag's commit; a branch ref must merely contain the recorded one, and how far
  the pin trails the branch is reported without failing the build.
- `bump_version.R` gains `--set-deploy-sha` (replacing `--set-release-sha`,
  still accepted as an alias) and `--set-deploy-ref`. The release-vs-branch
  predicate that both scripts turn on now lives in one place,
  `.github/scripts/ref_shape.R`, so the writer and the checker cannot drift
  apart on the one distinction the fix depends on.
- The deploy pin is now maintained entirely through pull requests, because
  neither long-lived branch accepts a direct push: `dev` and `master` both
  carry a ruleset requiring changes to arrive via PR (`master` with no bypass
  at all), so any workflow pushing to them is rejected with `GH013`. A
  push-triggered pinner was written first and failed on its only run; the
  pre-existing `manifest-release-sha.yml` would have failed the same way, which
  had never been discovered because it had never run at all.
  - `manifest-sync.yml` now points the manifest at the branch each PR merges
    into, pinned to that branch's tip, alongside the checksum resync it already
    did. It skips that when the PR's *head* is itself a protected branch — the
    `dev` → `master` release PR — since the commit is pushed to the head branch
    and `dev` rejects pushes just as `master` does.
  - `manifest-release-sha.yml` opens one PR per release to supersede that with
    the tagged commit. It cannot happen in the release PR itself: `master`
    gains its release commit only when that PR merges, so nothing can name
    that SHA while the PR is open.
- The app manifest now identifies its deploy target by **branch** on both
  branches (`dev` / `master`) rather than by release tag. Connect resolves the
  archive URL from the SHA, not the ref, so a SHA pins strictly more precisely
  than a tag — and a tag ref cannot be validated in the release PR that
  introduces it, since the tag does not exist yet.

## [0.20.0] - 2026-08-24

### Added

- **A dataset's metadata is now editable in the Shiny app.** The dataset **Edit**
  menu has a fourth entry, **Metadata**, after *Files*. It opens a dialog over
  the `DTADataSet`-level properties — `name`, `description`, `template_source`,
  `template_version` and `template_date` — which until now could only be changed
  by hand-editing the Raw YAML. Clearing a field removes it from the
  specification entirely rather than storing an empty value.

  Renaming a dataset re-keys everything that referred to it by name (loaded
  files, per-file controls, validation status, the selected dataset) while
  leaving it in its original position in the document, and clears its
  validation — stored results carry the name they were checked under, so they no
  longer describe the dataset once it is renamed. Editing only the description or
  the template fields does not touch validation, since none of them take part in
  it.

  A dataset's `type` is deliberately **not** editable. It is fixed by the
  concrete class (`DTADataSetTabular` / `DTADataSetFile`) and everything
  downstream dispatches on that class, so assigning the property would yield a
  dataset whose declared type and actual behaviour disagree. It remains visible
  as a chip on the dataset page.

## [0.19.0] - 2026-08-22

### Fixed

- **Roxygen parameter documentation coverage:** Fixed parameter documentation
  for `as_json_schema()`, `as_json_schema_length()`, and `as_json_schema_type()`
  generics to ensure argument `x` is properly documented in Rd pages. Added
  missing `@param` descriptions to internal helpers
  `dta_validate_table_stream()`, `dta_error_sink_add()`, and
  `validate_table_detailed()`.

- **The numeric conversion cache held about ten times the column it cached.**
  `dta_as_numeric_strict()` built an `as.character()` copy of every column it
  converted and returned it as `raw`. On a 200,000-row numeric column the
  source was 1.53 MB and the returned entry 16.02 MB, of which the character
  copy alone was 12.97 MB. `dta_build_numeric_cache()` holds one such entry per
  column that any rule reads numerically, all at once, so the cost was paid per
  column per batch — and, on the materialising path, over the whole table.

  The copy was only ever read at the rows that failed to convert, which is
  usually none of them. The conversion now keeps the *source vector* — shared,
  not copied — and renders the text on demand at those indices through
  `dta_numeric_raw()`. A batch of 131,072 rows across 20 numeric columns now
  adds about 20 MB of cache instead of about 190 MB.

  This regressed in 0.17.2, in the rule-evaluation speed work. That speed win
  is untouched: it came from converting each column once per batch rather than
  once per rule, which never depended on how the source text was obtained.

- **A violating group could be reported as passing past 2.1 billion rows.**
  Four accumulators in the streaming path were R integers, which overflow to
  `NA` with a warning rather than an error: the driver's row offset, the
  grouped-rule row offset, and a grouped condition's `n_seen`, `true_n` and
  `false_n`.

  The row offsets corrupted reported row numbers — every error pointed at `NA`
  while the counts and the verdict still looked authoritative. `n_seen` was
  worse: `dta_group_stream_truth()` evaluates `n_seen > 0 && all_true` for an
  `"all"` scope, an overflowed `n_seen` makes that `NA`, and `isTRUE(NA)` is
  `FALSE`, so a group that genuinely violated its constraint was silently
  reported as passing.

  All four are now doubles, which count whole numbers exactly to 2^53.
  Reported row numbers are narrowed back to integer when they fit, so the type
  callers see is unchanged for any realistic file. This is the same class of
  defect fixed for the error-sink counters in 0.18.1; these are the instances
  that fix did not reach.

### Changed

- **Uniqueness and grouping keys are built about 2.1x faster, and now agree
  with `duplicated()` on the atomic types validation deals with, where they
  previously did not.**
  `dta_unique_key()` and `dta_group_key()` now share a single encoder, the new
  internal `dta_row_key()`, instead of each length-prefixing or
  `gsub()`-escaping every value on every batch. The encoding stays injective:
  the rows `c("x", "y<US>z")` and `c("x<US>y", "z")` are different rows and get
  different keys, where a plain separator join would have merged them and
  reported a duplicate -- or a group rule violation -- that the data does not
  contain. A reserved byte is escaped only when the column actually contains
  one, which is a pure optimisation rather than a second encoding, because the
  escape is the identity on text that does not contain it; equal rows therefore
  still key equally across batch boundaries, and strings are normalised to
  UTF-8 first so that the marked encoding of a value cannot change its key
  either.

  Several verdicts that disagreed between the streaming and materialising
  paths are fixed along the way, all of them on key columns holding doubles:
  `0.1 + 0.2` and `0.3` are no longer reported as a duplicate pair (keys render
  doubles with `%.17g` rather than through `as.character()`, which rounds to 15
  significant digits), `NaN` is no longer treated as a missing value, and a
  sub-second `POSIXct` now keys on the instant it names. The same applies to
  grouping, where two doubles that render alike are now two groups rather than
  one, on both paths -- so a `check_group_condition` grouped by a double or a
  timestamp column can report differently than before, and correctly. `0` and
  `-0` remain one value, as `duplicated()` has them, and `integer64` is
  rendered by bit64 rather than reinterpreted as the double it is stored as. A
  value that merely looks like the internal missing-value marker is also no
  longer read as missing. Measured on 5e5 rows over three key columns: 0.58s
  before, 0.28s after.

- **Column schemas are now compiled once per scan instead of once per batch.**
  On the streaming path `dta_columnspec_errors()` runs once per batch, and it
  previously re-derived every column's schema through `as_json_schema()` on each
  call. A column's schema is a pure function of its `DTAColumnSpec` and does not
  change while a table is being validated, so the derivation is now hoisted out
  of the batch loop by the new internal `dta_compile_columnspec_schemas()`. The
  cost is now proportional to the width of the spec rather than to the number of
  batches; validation results are unchanged, which
  `tests/testthat/test-streaming-validation.R` asserts over the whole
  validation corpus.

- **The streaming scan no longer summarises every batch and discards it.**
  `dta_columnspec_errors()` always built both a per-row error frame and a
  grouped summary of it. The streaming driver reads only the frame and
  recomputes the summary once at the end, so every batch paid for a `dplyr`
  grouped summarise whose result was thrown away.

  The cost scales with the error frame and with how distinct the offending
  values are, because the offending value is part of the grouping key:
  measured at 0.318 s versus 0.006 s per call on a 10,000-row frame of
  distinct bad values. On a dirty file at the default batch size this was tens
  of seconds per batch. `dta_columnspec_errors()` gained a `summarise`
  argument; the streaming driver passes `FALSE`. Both other callers are
  unchanged.

  `dta_build_numeric_cache()` gained a `columns` argument. The streaming driver
  already computed the numeric columns once for the whole scan, but the cache
  re-derived them from the rules on every batch, re-parsing each rule's clause
  structure. It now uses the list that was already computed.

### Added

- **`check()` can reach the levers that make a very large scan survivable.**
  `fail_fast`, `on_missing_column` and `use_threads` were documented on
  `validate_file_stream()` and recommended for exactly this scale, but were
  unreachable through `check()`, which is the documented entry point. All three
  are now forwarded, with defaults that reproduce the previous behaviour
  exactly.

  `on_missing_column = "stop"` decides a missing required column from the
  column names alone and reads nothing, instead of scanning the whole table to
  restate the same fact once per row. It reaches the same verdict as a full
  scan. Unlike the other two it applies to a table held in memory as well.

- **Progress reporting during a scan.** A scan of a table large enough to take
  hours previously printed one line and then nothing, making a healthy run
  indistinguishable from a hang. When `verbose` is set, the scan now reports
  rows read and the current rate, throttled by wall time via
  `options(DTAtools.progress_seconds = )` (default 30) so that short runs stay
  silent. There is no total row count for a stream, so no percentage or ETA is
  invented.

## [0.18.2] - 2026-08-17

### Added

- **Opt-in benchmark metrics for `check()` and `validate_file_stream()`.**
  Passing `benchmark = TRUE` or setting `options(DTAtools.benchmark = TRUE)`
  attaches elapsed time, CPU time, memory, and throughput metrics to the
  validation result. The new `validation_benchmark()` function retrieves them.

### Fixed

- **Float value in a declared `Int` column no longer aborts the read.**
  Arrow infers a column as `int64` when early rows look like integers and then
  aborts with `CSV conversion error to int64: invalid value '0.01'` if a
  fractional value appears further down. All declared columns are now pinned to
  `utf8` at read time; `dta_coerce_table_to_specs()` handles conversion and
  leaves the fractional value as a double so the schema-validation axis can
  report it as a type violation.

- The Shiny app manifest is now synchronized with the files on disk, including
  live checksums and the `v0.18.2` package reference. Release SHA fields remain
  empty until the release tag exists, preventing a new version from inheriting
  the previous release's commit.

## [0.18.1] - 2026-08-16

A bug-fix release for the validation error counters. It also carries the Shiny
app manifest verification work, which had been merged to `dev` without a release
of its own and so ships here.

### Fixed

#### Validation error counting

- **`validate_file_stream()` stopped counting errors — and stopped judging the
  file — once a scan passed `.Machine$integer.max` errors.** The error sink
  accumulated its totals as integers, so on a file dirty enough to exceed the
  integer range the addition returned `NA` with a
  `NAs produced by integer overflow` warning rather than a count. Because
  `NA > 0` is `NA`, the `NA` then propagated into `columnspec_valid`,
  `import_valid`, and the fail-fast check, so the files too broken to count were
  exactly the files that stopped receiving a verdict. Retention is capped but
  counting deliberately is not, which is what makes these counters the one
  unbounded quantity in the streaming path. They are now accumulated as doubles,
  exact for whole numbers to 2^53.
- The same overflow was reachable on the import-typing axis outside streaming:
  `dta_coerce_table_to_specs()` accumulated its per-cell count as an integer,
  and `dta_import_error_count()` round-tripped the recorded total through
  `as.integer()`. The latter was the more damaging of the two — an `NA` count is
  read as "no count recorded", which falls back to the *capped* row count and
  under-reports by however much the cap discarded.
- Counts are still reported as integers wherever they fit, so the `details`
  contract is unchanged; only a count that cannot be an integer without becoming
  `NA` is now widened.

#### Shiny app manifest verification

- **The Shiny app's `manifest.json` was verified on two lines out of 3,143, and
  the unverified remainder had been wrong in every release that touched the
  app.** `bump_version.R` kept the `DTAtools` `Version` entry and the `VERSION`
  file's checksum in step with `DESCRIPTION`; everything else was maintained by
  hand. The consequences, all present in released tags:
  - **Six of the eight file checksums were checked by nothing.** Release 0.16.0
    shipped with `app.R` recorded as `df2b7079…` while the file on disk was
    `9d798811…`. App source changed in two releases with no accompanying
    manifest commit.
  - **The version-bearing fields contradicted each other.** 0.17.3 shipped with
    `RemoteRef` still reading `v0.17.2`; 0.18.0 shipped with hand-added
    `GithubSHA1`/`RemoteSha` pointing at the *0.17.3* release commit.
  - **Nothing checked the file was even parseable**, though it is patched by
    text substitution.
- `RemoteRef` and `GithubRef` are now version sites in
  `.github/scripts/bump_version.R`, so the writer and the checker cannot
  disagree about them. `GithubSHA1`, `RemoteSha`, `Packaged`, and `Built` were
  **removed** from the `DTAtools` entry rather than checked: a bump commit
  cannot know the SHA of the release commit that will contain it, so there is no
  value they could ever be verified against, and each hand-maintained attempt
  recorded the previous release's commit.

### Added

All of the following belong to the manifest verification work above.

- `.github/scripts/check_manifest.R`, run by the `r-style` workflow: asserts the
  manifest's file list matches the app directory exactly, that every checksum is
  live, that the removed fields stay removed, and that every package the app
  loads has a `packages` entry. It deliberately does not police the `packages`
  block's contents, which is a frozen snapshot of one developer's `renv` library
  and is not reproducible on another machine.
- `Rscript .github/scripts/bump_version.R --sync-manifest`, which rebuilds the
  manifest's `files` block from the app directory — handling added and removed
  files, which a line patcher cannot see. Entry order is sorted with
  `method = "radix"` so it does not depend on the collation locale; plain
  `sort()` orders the block differently under `de_DE` than under CI's `C`
  collation, which would have made the file flip on every hop between machines.
- `.github/workflows/manifest-sync.yml`, which runs that repair on pull requests
  and pushes the result to the PR branch, so checksums are never copied by hand.
  Only the mechanical half is auto-committed; a version mismatch, a missing
  package entry, or a re-added unverifiable field still fails `r-style` and must
  be fixed deliberately.
- The `check-json` pre-commit hook, covering the parse check that nothing
  performed before. It is pure Python, so the `pre-commit` workflow still
  installs no R.

### Added

- **Opt-in benchmark mode for `check()` and `validate_file_stream()`.** Passing
  `benchmark = TRUE` (or setting `options(DTAtools.benchmark = TRUE)`) attaches a
  one-row metrics `data.frame` — elapsed time, CPU time, R heap peak, process
  RSS, Arrow's memory-pool peak, and rows/sec — as a `"benchmark"` attribute on
  the result, retrievable with the new exported `validation_benchmark()`. Off by
  default, so the normal call path is unaffected. R's heap peak is read from
  `gc()`'s `max used` counters rather than a before/after delta, which would
  miss transient allocations entirely.
- The instrument is built not to distort or break what it measures. `gc()` runs
  outside the timed region at both ends, so the bracketing collections are never
  charged to the call. A nesting guard makes the outermost call the one that
  measures, so an inner `gc(reset = TRUE)` cannot silently corrupt an outer
  figure, and the guard is released via `on.exit()` in the caller's own frame,
  so a call that aborts part-way does not leave benchmarking dead for the rest
  of the session. Nothing about the verdict changes when the flag is on.
- Figures that cannot be measured say so instead of guessing. Arrow's memory
  pool has no reset in the installed `arrow` version, so its peak is reported as
  the per-process high-water mark it is, alongside an `arrow_call_exact` flag
  saying whether the figure attributed to this call is exact or merely a lower
  bound; an unreadable pool reports `NA`, never `0`. Process RSS needs the new
  `Suggests`-only `ps` package and reports `NA` without it. `check()` reports
  `rows` as `NA` because there is no cheap, trustworthy row total across every
  dataset at that level. Measuring the R heap peak requires resetting `gc()`'s
  peak counters, a session-wide side effect documented on
  `validation_benchmark()`.

### Fixed

#### Shiny app manifest: R Connect archiveUrl crash

- **R Connect failed to deploy the Shiny app with `Error in if (!grepl("^http",
  archiveUrl)) { : argument is of length zero`.** Posit Connect needs a
  resolvable commit SHA (`RemoteSha`/`GithubSHA1`) to build the archive
  download URL for `manifest.json`'s `Source: "github"` DTAtools entry; without
  one it computes a `NULL` `archiveUrl` and crashes on the `grepl()` guard.
  `v0.18.1` shipped with neither field present, because the manifest-validation
  work added in `0.18.1` itself (`#47`) had them forbidden outright rather than
  verified -- a reaction to those fields going *stale* every previous release
  (`v0.17.3` shipped with a `RemoteRef` still naming the prior tag; `v0.18.0`
  shipped hand-added SHAs pointing at the `v0.17.3` commit), but removing them
  broke every deploy instead of just a stale one.
- `manifest.json`'s DTAtools entry now carries `RemoteSha`/`GithubSHA1` again,
  pinned to the `v0.18.1` release commit.
- `.github/scripts/bump_version.R` gained `--set-release-sha <sha>`, the one
  place that writes those two fields, and its version-bump `write()` now
  CLEARS them whenever `RemoteRef`/`GithubRef` move to a new tag -- an existing
  SHA belongs to the *old* tag and is stale the instant the ref changes, so a
  release-automation failure now degrades to a loud, visible deploy crash
  rather than a silent stale deploy.
- `.github/scripts/check_manifest.R` no longer forbids `RemoteSha`/`GithubSHA1`
  outright. When present, it resolves the field's ref with `git rev-parse` and
  fails if the recorded SHA doesn't match -- a correctness check the old
  "must stay absent" rule could never provide, and would have caught the
  `v0.18.0` incident directly.
- New workflow `.github/workflows/manifest-release-sha.yml`, triggered on
  `release: published`, resolves the tag's commit and pins
  `RemoteSha`/`GithubSHA1` automatically -- replacing the hand-written
  follow-up PR every previous release needed (`#41`, `#43`, `#45`) and which
  was, predictably, forgotten for `v0.18.1`.
- `.github/workflows/r-style.yaml`'s checkout now fetches full history
  (`fetch-depth: 0`) so `check_manifest.R`'s `git rev-parse` calls can actually
  see the release tags.

## [0.18.1] - 2026-08-16

A bug-fix release for the validation error counters. It also carries the Shiny
app manifest verification work, which had been merged to `dev` without a release
of its own and so ships here.

### Fixed

#### Validation error counting

- **`validate_file_stream()` stopped counting errors — and stopped judging the
  file — once a scan passed `.Machine$integer.max` errors.** The error sink
  accumulated its totals as integers, so on a file dirty enough to exceed the
  integer range the addition returned `NA` with a
  `NAs produced by integer overflow` warning rather than a count. Because
  `NA > 0` is `NA`, the `NA` then propagated into `columnspec_valid`,
  `import_valid`, and the fail-fast check, so the files too broken to count were
  exactly the files that stopped receiving a verdict. Retention is capped but
  counting deliberately is not, which is what makes these counters the one
  unbounded quantity in the streaming path. They are now accumulated as doubles,
  exact for whole numbers to 2^53.
- The same overflow was reachable on the import-typing axis outside streaming:
  `dta_coerce_table_to_specs()` accumulated its per-cell count as an integer,
  and `dta_import_error_count()` round-tripped the recorded total through
  `as.integer()`. The latter was the more damaging of the two — an `NA` count is
  read as "no count recorded", which falls back to the *capped* row count and
  under-reports by however much the cap discarded.
- Counts are still reported as integers wherever they fit, so the `details`
  contract is unchanged; only a count that cannot be an integer without becoming
  `NA` is now widened.

#### Shiny app manifest verification

- **The Shiny app's `manifest.json` was verified on two lines out of 3,143, and
  the unverified remainder had been wrong in every release that touched the
  app.** `bump_version.R` kept the `DTAtools` `Version` entry and the `VERSION`
  file's checksum in step with `DESCRIPTION`; everything else was maintained by
  hand. The consequences, all present in released tags:
  - **Six of the eight file checksums were checked by nothing.** Release 0.16.0
    shipped with `app.R` recorded as `df2b7079…` while the file on disk was
    `9d798811…`. App source changed in two releases with no accompanying
    manifest commit.
  - **The version-bearing fields contradicted each other.** 0.17.3 shipped with
    `RemoteRef` still reading `v0.17.2`; 0.18.0 shipped with hand-added
    `GithubSHA1`/`RemoteSha` pointing at the *0.17.3* release commit.
  - **Nothing checked the file was even parseable**, though it is patched by
    text substitution.
- `RemoteRef` and `GithubRef` are now version sites in
  `.github/scripts/bump_version.R`, so the writer and the checker cannot
  disagree about them. `GithubSHA1`, `RemoteSha`, `Packaged`, and `Built` were
  **removed** from the `DTAtools` entry rather than checked: a bump commit
  cannot know the SHA of the release commit that will contain it, so there is no
  value they could ever be verified against, and each hand-maintained attempt
  recorded the previous release's commit.

### Added

All of the following belong to the manifest verification work above.

- `.github/scripts/check_manifest.R`, run by the `r-style` workflow: asserts the
  manifest's file list matches the app directory exactly, that every checksum is
  live, that the removed fields stay removed, and that every package the app
  loads has a `packages` entry. It deliberately does not police the `packages`
  block's contents, which is a frozen snapshot of one developer's `renv` library
  and is not reproducible on another machine.
- `Rscript .github/scripts/bump_version.R --sync-manifest`, which rebuilds the
  manifest's `files` block from the app directory — handling added and removed
  files, which a line patcher cannot see. Entry order is sorted with
  `method = "radix"` so it does not depend on the collation locale; plain
  `sort()` orders the block differently under `de_DE` than under CI's `C`
  collation, which would have made the file flip on every hop between machines.
- `.github/workflows/manifest-sync.yml`, which runs that repair on pull requests
  and pushes the result to the PR branch, so checksums are never copied by hand.
  Only the mechanical half is auto-committed; a version mismatch, a missing
  package entry, or a re-added unverifiable field still fails `r-style` and must
  be fixed deliberately.
- The `check-json` pre-commit hook, covering the parse check that nothing
  performed before. It is pure Python, so the `pre-commit` workflow still
  installs no R.

## [0.18.0] - 2026-08-15

### Fixed

- **`validate_file_stream()` leaked memory permanently, proportional to the
  number of distinct uniqueness keys and group labels.** Both cross-batch
  accumulators used an R environment as a hash set. `assign(key, ...)` and
  `env[[key]]` intern every key in R's global **symbol table**, which has no
  garbage collector, so the memory was never reclaimed — not when the
  accumulator was dropped, not after `gc()`, not for the life of the session.
  Measured at 2,000,000 distinct keys: RSS rose 606 MB and **556 MB of that
  remained after the accumulator was removed and `gc(full = TRUE)` ran twice**,
  i.e. ~278 bytes leaked per distinct key. On a file with a per-row natural key
  this is O(rows) with a large constant, and it is the dominant term in reports
  of a 60 GB input consuming several hundred GB of RAM. Both accumulators now
  use `fastmap::fastmap()`, a C++ hash map that does not touch the symbol
  table; the same 2,000,000 keys now return 402 MB of 486 MB to the process on
  `gc()`. Validation results are unchanged.
- The per-batch duplicate count in the uniqueness accumulator was an R `for`
  loop over every row (~10 µs/row, hours on a large file). It is now vectorised
  over the whole batch and pinned by an oracle test against `duplicated()`
  across batch boundaries.
- `dta_group_stream_update()` accumulated group keys with
  `state$keys <- c(state$keys, key)` inside a per-group loop, which is
  quadratic in the number of groups. The keys are now read back from the map at
  finalise, so the append is gone entirely.
- The Arrow record batch and the pre-coercion data frame stayed live for the
  whole of each batch iteration alongside the coerced frame and the numeric
  cache, multiplying whatever `batch_rows` the caller chose. Both are now
  dropped as soon as they are consumed.
- A rule's violation counter was an R integer, so a rule violated by more than
  `.Machine$integer.max` (~2.1 billion) rows silently counted `NA` rather than
  overflowing loudly — inside the range this path is explicitly built for. It
  is now a double, which counts whole numbers exactly to 2^53. Pre-existing,
  but on the same line as the vectorised rewrite above.

### Changed

- **`max_errors` now defaults to `10000` rather than `NULL`** everywhere a
  retained-error sink is created — `validate_file_stream()`,
  `dta_validate_any_table()` and the internal streaming driver — configurable
  with `options(DTAtools.max_errors = )`. The three per-cell error sinks retain one
  row per bad cell, so the previous unbounded default exhausted memory on a
  large dirty file exactly as holding the data would — the very case the
  streaming path exists to serve. Counting is unaffected: the totals and the
  pass/fail verdict stay exact, and a truncated frame is still flagged.
  Pass `max_errors = NULL` for the previous behaviour. `check()` on a `DTA` and
  on a `DTADataSetTabular` defaults the same way; both previously passed `NULL`
  through explicitly, so the bounded default would never have reached the main
  user-facing path.
- `validate_file_stream()` gains a `use_threads` argument, forwarded to Arrow's
  `Scanner`. Arrow buffers batches ahead of R in its own C++ pool, outside the
  R heap and invisible to `gc()`; single-threaded scanning is the lever when
  resident memory rather than throughput is the binding constraint.
- With `verbose = TRUE`, `validate_file_stream()` now reports Arrow's C++ pool
  high-water mark, which no existing memory measurement in this package could
  see.

### Added

- Two resource budgets, `options(DTAtools.max_unique_keys = )` (default
  50,000,000) and `options(DTAtools.max_groups = )` (default 5,000,000). A scan
  that exceeds one aborts with the classed condition
  `dta_stream_budget_exceeded`, naming the rule and the option to raise. This
  is deliberately an abort and not a rule failure: a resource limit is not a
  data verdict, and reporting a uniqueness constraint as "not applicable"
  would present a clean-looking result for a constraint never actually checked.
- `benchmarks/bench_streaming.R` now measures process RSS, and RSS again after
  two full `gc()`s, alongside the R-heap figure it reported before — a leak is
  visible only in the gap between them. Its fixture gained a dirty-row
  fraction, a unique per-row `SUBJID`, and a `SITE` grouping column, with
  matching uniqueness and group-condition rules. The previous fixture was
  clean and had neither rule, so it exercised none of the three defects above,
  and the R-heap-only measurement could not have seen them regardless.

## [0.17.3] - 2026-08-15

### Added

- `.github/scripts/bump_version.R`, which rewrites every file that records the
  package version — `DESCRIPTION`, `inst/shiny/dta_app/VERSION`, the DTAtools
  entry and the `VERSION` checksum in the app's `manifest.json`, and the badge
  and footer in `docs/index.html` — plus the `CHANGELOG` heading. Run it as
  `Rscript .github/scripts/bump_version.R <version>`, or from the Actions tab
  via the new **Bump version** workflow, which opens a PR against `dev`.
  `--check` reports drift without writing and is what CI runs.

### Changed

- The GitHub Pages tutorial (`docs/index.html`) now uses the Boehringer
  Ingelheim brand palette that the Shiny app and the Word/PDF export already
  share, replacing the stock Bootstrap blue, and shows the real DTAtools logo
  instead of an emoji placeholder. The dark theme uses lifted tints of the
  brand greens; every foreground/background pair passes WCAG AA. (#37)
- `.github/scripts/check_shiny_version_file.R` is replaced by
  `check_version_sync.R`, which checks all six version sites rather than only
  `inst/shiny/dta_app/VERSION`. The two sites it did not cover had both
  silently drifted: `manifest.json` still said `0.17.2` with a stale `VERSION`
  checksum, and `docs/index.html` still said `0.17.1`. All are now resynced.

### Removed

- The "Upgrading from 0.12.x" sections in `README.md` and
  `vignettes/DTAtools.Rmd`, and the inline references to that migration. The
  `force = TRUE` behaviour they described is still documented, on its own
  terms rather than as an upgrade step. The 0.13.0 CHANGELOG entry remains as
  the historical record. (#35)

### Fixed

- `%||%` is now imported from `rlang`. It is used throughout the package but
  only entered base R in 4.4.0, while `DESCRIPTION` declares `R (>= 4.1.0)`,
  so on R 4.1–4.3 operations such as reading a DTA from YAML failed with
  `could not find function "%||%"`.

### Added

- `tests/testthat/test-namespaceImports.R`, asserting `%||%` is bound in the
  package's own imports environment rather than inherited from base R.

## [0.17.2] - 2026-08-15

### Added

- Oracle tests for `dta_count_duplicates()` pinning agreement with
  `sum(duplicated())` for factor, logical, `Date`, and `POSIXct` key columns,
  and a mixed character+integer multi-column key.
- A regression test that `rule_check_range()` recomputes rather than reuses a
  numeric cache whose cached column length no longer matches the frame being
  checked (e.g. a cache built for a full table passed alongside a filtered
  subset of it).
- A test running `rule_check_unique()` with the opt-in Arrow duplicate-count
  path both on and off, asserting identical `valid`/`message` in both cases.
- A test that a `DTARuleGroupCondition` constraint referencing an unknown
  condition name (only reachable by bypassing the constructor) is surfaced as
  a FAILED rule via the narrowly classed `dta_rule_not_applicable` condition,
  both through `rule_check_group_condition()`/`apply_rules()` and through the
  streaming path (`dta_group_stream_finalise()`/`dta_rule_stream_finalise()`).

- **Opt-in Arrow compute path for rule checking**, off by default
  (`options(DTAtools.use_arrow_compute = TRUE)`). The R implementation
  remains the reference behaviour for every rule check; Arrow is only ever
  consulted when a user explicitly opts in, and any Arrow failure falls back
  to the R path rather than aborting validation. The first (and so far only)
  place this is wired up is `check_unique`'s duplicate count, and only for
  tables at or above `getOption("DTAtools.arrow_min_rows", 100000L)` rows
  with no key column stored as a double. Validation **results are unchanged**
  by this option -- it is a performance path, not a behaviour change.
- `set_dta_compute_threads()`: reports or sets the thread count used by
  Arrow's own multi-threaded compute kernels (`arrow::cpu_count()` /
  `arrow::set_cpu_count()`). `DTAtools` does not spawn its own R-level worker
  processes, so this is the only parallelism knob the package exposes.
- Rule-heavy benchmark script `benchmarks/bench_rules.R` (20,000 rows / 4,000
  groups) used to measure the rule-checking changes below.
- `benchmarks/bench_rules.R` gained a `--violation-rate` argument (default
  `0.01`), and its grouped-rule fixture now constructs group-consistent
  values for the columns the grouped rules read, then deliberately corrupts
  only that fraction of groups -- matching how rarely real DTA/DTS transfer
  data actually violates cross-row rules, instead of the previous uniform
  per-row sampling that made 12-22% of groups violate. The console summary
  now prints the actual measured violating-group fraction per grouped rule.

### Changed

- **The Shiny app no longer asks whether to stream a file; it just does the
  right thing.** The "Load large files without reading them into memory"
  checkbox added in 0.17.1 is gone, and the app now calls `load_file()` with
  its own default of `stream = "auto"` -- reading a file into memory below
  `getOption("DTAtools.stream_threshold")` and scanning a larger one in
  batches. The checkbox put a storage-strategy question in front of someone
  who came to validate a file, and the answer is one the size of the file
  already determines. Nothing is lost: `"auto"` still streams the files that
  need it.

- `set_dta_compute_threads()`'s roxygen documentation now describes both
  `options(DTAtools.use_arrow_compute)` and
  `options(DTAtools.arrow_min_rows)` under `@details` -- these were
  previously only documented in this changelog and had no `?`-reachable
  description.
- **Performance: grouped-condition rules (`check_group_condition`) now
  evaluate each condition once per table/batch and reduce per group**,
  instead of copying/filtering the full data frame once for every group.
  Measured on `benchmarks/bench_rules.R` (20,000 rows / 4,000 groups): 9.1x
  faster on the grouped-condition rule alone, and 6.2x faster across the
  full rule suite. Numeric conversions used during rule evaluation are now
  cached once per table/batch and shared with the import-time numeric
  conversion path, instead of being repeated per rule/per group. Uniqueness
  checking (`check_unique`) now keys rows with a hashed key instead of
  `data.frame`-level `duplicated()`. **Validation results are unchanged** --
  these are performance-only changes, verified by the existing rule-checking
  test suite.
- **Performance: grouped-condition rules now do per-group work (building
  group labels, row evidence, and messages) only for groups that actually
  violate a constraint**, instead of for every group in the table/batch.
  Each constraint's truth outcome (`mutually_exclusive`, `requires`) is now
  evaluated as a vector across all groups at once, and only the resulting
  violating group/constraint pairs are turned into messages, in the same
  sorted-group / declared-constraint order as before. Measured on
  `benchmarks/bench_rules.R` (50,000 rows / 20,000 groups): `check_group_condition`
  drops from 31.6s to 5.2s (~6x) on a fixture where ~12-22% of groups violate;
  real data, where violations are rare, benefits more. **Validation results
  are unchanged** -- this is a performance-only change, verified by the
  existing rule-checking test suite plus a new regression test pinning that
  non-violating groups contribute nothing to the reported message.

### Fixed

- `dta_count_duplicates()`'s fast hashed-key path (`dta_unique_key()`, which
  compares `as.character()` renderings) is now restricted to key column types
  provably safe to render as character -- `character`, `factor`, `integer`,
  `logical` -- instead of merely excluding `double`. Any other type (`Date`,
  `POSIXt`, `complex`, `list`, or an unrecognised type) now falls back to
  `duplicated(df[, cols, drop = FALSE])`, matching what was already done for
  `double` columns, since those types can collide on the same rendered string
  the same way doubles can.
- `dta_numeric_cache_get()` now checks that a cached numeric conversion's
  length matches the frame being checked before reusing it, falling back to a
  fresh `dta_as_numeric_strict()` conversion otherwise. Previously a cache
  built for one frame silently recycled against a shorter frame if ever
  reused across a subset -- not reachable via any current caller, but latent.
- `rule_check_group_condition()` no longer allocates the per-group row index
  (`split(seq_len(nrow(df)), gid)`) unconditionally for every grouped rule; it
  is now built lazily and only for the (usually rare) groups that violate a
  `requires` constraint with `then_scope = "all"`, which is the only place it
  is read.
- A `DTARuleGroupCondition` constraint referencing a condition name that does
  not exist (only reachable by bypassing the constructor, which already
  rejects this) previously degraded to silently reporting **no violation**
  in the vectorised constraint evaluation, in both
  `rule_check_group_condition()` and the streaming path
  (`dta_group_stream_finalise()`). Both now abort with the narrowly classed
  `dta_rule_not_applicable` condition, which `apply_rules()` and
  `dta_rule_stream_finalise()` already convert into a FAILED rule rather than
  an aborted run -- silent no-violation is the worst failure mode for a
  validation package, so this is now a loud, attributed failure instead.
- The streaming path's `dta_rule_stream_finalise()` did not catch
  `dta_rule_not_applicable` raised while finalising a grouped rule, unlike
  every other rule kind; a rule failure there would have aborted the whole
  streaming scan instead of being reported as a failed rule.

## [0.17.1] - 2026-08-14

### Added

- **`load_file()` can now keep a file lazy instead of reading it into memory.**
  A new `stream` argument decides how the table is held: `"never"` reads the
  whole file as an Arrow `Table` (the previous, and still the usual, behaviour),
  `"always"` keeps it as an Arrow `Dataset` that `check()` scans in batches, and
  `"auto"` (the default) picks `"always"` only for files above
  `getOption("DTAtools.stream_threshold")`, 512 MB by default. `TRUE`/`FALSE`
  work as aliases for `"always"`/`"never"`, and the session-wide default can be
  set with `options(DTAtools.stream = ...)`.

  This makes a file larger than memory validatable through the ordinary object
  model. Previously the streaming validator was reachable only via
  `validate_file_stream()`, which bypasses the `DTA` object entirely.

  Both paths produce the same verdict and the same error counts. They differ in
  *when* import errors are found: the in-memory path reports them during
  `load_file()`, the streaming path during `check()`. After a streaming load,
  the dataset's `@import_issues` is therefore empty until `check()` has run.

- **`open_file()`**, the lazy counterpart of `read_file()`, opening a file as an
  Arrow `Dataset` using the same name checks and the same spec-driven column
  typing. Implemented for `DTAFileCSV`, `DTAFileTSV` and `DTAFileDelim`; a
  handler without a lazy opener aborts with a message pointing at
  `stream = "never"`.

- **`check()` gained `batch_rows` and `max_errors`**, tuning the batch size and
  the cap on retained per-cell error detail when scanning a streamed table.
  These reach the scanner for the first time -- previously the streaming
  validator's own defaults were unreachable from `check()`. Counts and the
  verdict are unaffected by `max_errors`; only how much failure detail is kept
  is. `batch_rows` defaults to
  `getOption("DTAtools.stream_batch_rows", 131072L)`.

- **Shiny app: a "Load large files without reading them into memory" toggle** on
  the Datasets page, applying to subsequent uploads in that dataset.

- **Standalone HTML validation report.** `write_validation_report()` renders a
  self-contained `.html` file (no external assets) summarizing validation
  results for a `DTA` object: a pass/fail overview per dataset/target, and a
  sortable, filterable table of every validation message with click-to-inspect
  detail, matching the look of the Shiny app's validation-messages tab.
  Repeated identical messages (e.g. the same `required property 'HEIGHT'`
  violation on many rows) are capped at `max_repeats` (default `5`) in the
  default view, with a "show all" toggle in the file itself so no data is
  lost. The Shiny app's validation-messages dock gained a "Report" download
  button alongside the existing CSV/TSV/XLSX exports.

### Changed

- **The Shiny export dialog now defaults to "Word Document" with "Embed YAML
  specification at end of document" ticked**, the combination that produces the
  hand-over document most users want without any extra clicks.

- **The built-in DOCX export was redesigned to a single, congruent house
  style.** The document previously mixed three font families (the template's
  Cambria body style, Calibri headings, and flextable's Arial default) and
  several unrelated blues. All package-emitted text and every table now uses
  one family and the Boehringer Ingelheim brand palette — the same green family
  the Shiny app is themed with — with a single brand-green table header,
  zebra-striped body rows and hairline rules instead of per-table ad-hoc
  colours, sizes and padding. The bundled reference template
  (`inst/extdata/templates/dta_numbered_template.docx`) was re-themed to match,
  so Word's own heading styles render in the brand green rather than black.

- **The supplier is now introduced before the receiver** in both the Word and
  the Markdown export, following the direction the data actually flows.

- **Exported document file names now carry the document version and the export
  time**, between the title and the timestamp
  (`Clinical_Data_Specification-v0.2-2026-08-14_14-07.docx`). The version
  segment is omitted when the DTA has no version set. The Shiny export modal's
  filename preview and the two export branches share one helper, so the preview
  cannot disagree with the file that is downloaded.

- **Signatures now open the DOCX export.** "Approval & Signatures" is the first
  chapter after the top-level heading in `write_dta()`, and comes directly
  after the title in `write_dataset_metadata()`, rather than being buried at
  the end of the "Process Information" chapter. Consequently "Process
  Information" is rendered only when there is genuine process content
  (transmission details, error handling, or authorized-for-corrections
  entries).

- **The Shiny app's "Export PDF" button is now called "Export DTA".** The button
  opens an "Export Document" modal whose formats are Markdown and Word, with
  PDF only an option within the Markdown branch, so the old label named the one
  thing the button does not do directly.

- **Built-in DOCX export puts the dataset column and validation rules tables on
  landscape pages.** The "Column Specifications" table is 8.4 in wide and did
  not fit the ~6.3 in text column of an A4 portrait page, so each dataset's
  table block — the column table together with the validation rules table that
  follows it — is now emitted in its own landscape section. The rest of the
  document (title page, metadata, dataset headings, embedded YAML, signatures
  and footer) stays portrait. This applies to both `write_dta()` and
  `write_dataset_metadata()`. The landscape pages reuse the page size and
  margins of the document itself rather than being forced to A4, and the
  section break is `nextPage`, so no blank filler pages are inserted.

### Removed

- **Filler signature lines and the signatory footnote in the DOCX export.** The
  per-contact "Signature: ____ Date: ____" underlines in the receiver/supplier
  sections, the generic "Approved by: / Signature: ____" fallback shown when no
  signatory was defined, and the "Note: signatories listed above are contacts
  marked as authorized signers" explanation have all been dropped. The single
  approval table is the one place to sign; when no contact is marked
  `signature = TRUE` the chapter is omitted entirely rather than padded with an
  anonymous underline.

### Fixed

- **`n_columnspec_errors` is now always an integer.** In-memory validation
  reported it as a double when the count was zero and an integer otherwise,
  because one branch used a bare `0` where the other used `nrow()`. Every other
  count in the package is an integer, and the streaming path already reported
  one, so `identical()` comparisons between the two paths' results failed on
  storage type alone.

- **Column names are now normalized identically on the eager and lazy read
  paths.** A header with surrounding quotes or whitespace (`" AGE "`) was
  trimmed when the file was read into memory but left as-is when opened as a
  lazy dataset, so the same file could match the column specification on one
  path and fail on the other. The lazy opener supplies cleaned names when the
  dataset is opened, since an Arrow `Dataset` has no `names<-`.

- **Documentation site: the "Large Files" navigation link went to the wrong
  section.** Two sections shared `id="section-4-7"`, so the link resolved to the
  first of them (`group_condition Rule`) and the streaming section could not be
  reached from the sidebar at all.

- **The Shiny export dialog announced itself twice.** The modal's own title bar
  and a heading at the top of its body both read "Export Document", one directly
  above the other. The heading is gone; the dialog title stands alone, as it
  does in every other modal in the app.

- **`group_condition` rules are now described in full in the exported
  documents.** They fell through to the default rule formatter, which printed
  either the author's one-line description or
  `"Rule type 'group_condition' ... no description available"` — the grouping
  columns, the named conditions and the constraints between them never reached
  the document at all. The rules table now spells out which columns rows are
  grouped by, what each named condition means in terms of its columns, and what
  each constraint requires, including the difference between the `any` and
  `all` scopes ("for at least one row in the group" vs "for every row in the
  group"). An author-written description is kept as the leading summary rather
  than replaced.

## [0.17.0] - 2026-08-14

### Changed

- **Group condition rule violation messages are now human-readable.**
  The technical internal format (`"Constraint 'X' failed: ... scope=any; rows=..."`)
  has been replaced with plain-English descriptions:
  - `mutually_exclusive`: `In group [A=1, B=2]: "cond1" and "cond2" must not both occur, but both were found (rows matching "cond1": 1; rows matching "cond2": 3).`
  - `requires`: `In group [A=1]: when "cond1" occurs (rows: 1), "cond2" must also hold, but it does not (no row in the group satisfies "cond2").`
  The group key is now embedded directly in each violation message. Users see what the rule checks, which group failed, and which rows are involved — without needing to know about scopes or constraint IDs.

- **Shiny app inspect view for group condition rules shows all values involved.**
  The inspect modal now displays: (1) a violation breakdown table listing each
  failing group, the constraint, the message, and all involved row numbers; and
  (2) a second table with the actual data values for all offending rows (all
  relevant columns: group-by columns plus all columns referenced in conditions).
  Previously the modal showed at most 10 rows and only `SUBJECT_ID`/`VISIT`.

- **The validation axis formerly called "schema" is now called "columnspec".**
  The name was inherited from the JSON Schema validator that used to evaluate
  it. That validator is gone — the axis is evaluated directly against the
  `DTAColumnSpec` objects a specification declares — so the name now says what
  the axis actually checks. A violation is a *column spec* violation, and the
  other axis is *rules*.

  There is no compatibility shim; the old names are simply gone:

  | was | is |
  | --- | --- |
  | `n_schema_errors` | `n_columnspec_errors` |
  | `schema_valid` | `columnspec_valid` |
  | `schema_errors` | `columnspec_errors` |
  | `schema_version` | `result_version` |
  | `source == "schema"` | `source == "columnspec"` |
  | the `schema` column of an error frame | the `columnspec` column |
  | `apply_schema_rules()` | `apply_rules()` |
  | `get_arrow_schema_type()` | `get_arrow_type()` |

  `as_json_schema()`, `as_json_schema_type()` and `as_json_schema_length()`
  keep their names: they serialise to JSON Schema the standard, and still do.

### Added

- **Gzipped input files are supported and tested.** Arrow already decompressed
  `.gz` transparently on read; what was missing is that a specification
  declaring `data.csv` did not recognise `data.csv.gz` as the file it asked
  for. Compression is a transport detail, not part of the data's identity, so
  `matches_filename()` now accepts the compressed form — for literal filenames
  and for anchored patterns alike. Validation results are identical either way,
  including row numbers under batched streaming. `inst/extdata` ships
  `clinical_data2.csv.gz` as a worked example.

### Fixed

- **A conditional rule no longer waves through a row whose IF clause could not
  be evaluated.** `AGE = "ninety-five"` under `condition: {AGE: {greater: 18}}`
  made the IF mask `NA`, which `sum(na.rm = TRUE)` then discarded — so a row
  whose THEN clause definitively failed was reported as passing. An
  unconvertible value now keeps the row in scope; a *missing* one still means
  the rule does not apply. The materialising path, the streaming path and
  `inspect()`'s row lookup now share one definition of a violation.

- **`inspect()` reports the rows a `group_condition` rule failed on.** The row
  lookup had no branch for that class and returned nothing, so a rule that
  unambiguously failed showed `failing_row_count = 0`.

- **Streamed `n_import_errors` no longer double-counts.** A cell flagged both by
  import typing and by a rule reading the column numerically is one error, not
  two; the streaming path summed the raw per-axis totals and could report more
  import errors than `import_errors` had rows, disagreeing with the
  materialising path on the same input.

- **`fail_fast` now stops on a grouped constraint.** Its decision read
  `state$count`, which a grouped rule never increments, so a file whose first
  rows already broke a `mutually_exclusive` constraint was still scanned to the
  end. Unsupported and not-applicable rules trip it too. Constraints a later
  batch could still rescue — `requires`, and any `all`-scoped side — are
  deliberately left to the end of the scan.

- **A truncated grouped-violation row list says so.** The streaming path caps
  retained row numbers at ten to stay memory-bounded while the materialising
  path keeps them all; both now carry `rows_truncated`, so ten rows can no
  longer be mistaken for all of them.

- **The Shiny app's document export no longer collides between sessions.** The
  export wrote to a path built from the document title and the date, so two
  untitled exports on the same day shared one file — and because the browser
  fetches it on a later request, whichever session wrote last was the one both
  downloads received. Each export now writes to its own `tempfile()`; the name
  the browser saves it under is unchanged.

- **A multi-line description no longer corrupts a Markdown report.** Pipe tables
  are line-based, so a newline inside a cell split the row and the renderer read
  the tail as a fabricated extra row, dropping the text after the last pipe.
  Newlines are now folded to `<br>`.

- **A duplicated column `id` in a specification is rejected.** It was accepted
  silently, after which `colspec()` saw only the first definition while the
  column spec axis evaluated the table column against both.

- `DTAColumnSpecCollection`'s validator no longer computes an error message and
  discards it, which made a bad `@columns` assignment fail with an unrelated
  R-level error instead of the message the code appears to produce.

- **Permitted values a YAML parser turned into numbers now warn.** `values:
  [1.10, 2.00]` written unquoted arrives as `1.1` and `2`; a text column
  compares them as text, so data written `1.10` failed the check for a reason
  nothing in the output explained. The original spelling is gone by the time
  the parser hands the list over and cannot be recovered, so importing such a
  specification now says which column is affected and to quote the values.

- **Template-based Word export now preserves multiline placeholder formatting.**
  User-supplied template variables can now carry line breaks and tabs into the
  generated `.docx` as real Word line/tab elements instead of collapsed
  one-line text. Markdown-like placeholder content is now de-marked on insert
  (for example `##` headings, `**bold**`, and `-` list markers), so dataset
  blocks no longer render with raw markdown syntax in Word templates. Dense
  one-line column/rule bullets are expanded into structured multi-line fields
  (`Description`, `Type`, `Nullable`, `Length`, `Values`) for easier reading in
  custom templates, rendered as nested list items instead of literal markdown
  markers. Nested lists now use depth-specific symbols and stronger indentation
  (top-level `•`, sub-level `◦`, sub-sub-level `▪`) for visual clarity. Rules
  now render as list items without a `Description` label, and
  `group_condition` rules are expanded with an explicit premise-oriented
  breakdown (`Grouped by`, `Conditions`, `Constraints`, `Premise`, `Context`).
  When the referenced `group_condition` rule object is present in the DTA, the
  custom-template output now also includes explicit condition and constraint
  definitions (condition expressions and requires/mutually-exclusive links with
  scopes), so readers can see how the grouped rule works. Count-only lines for
  conditions/constraints are no longer shown. Template metadata placeholders now
  include richer contact/signatory/process details (full contact fields and
  signature lines) instead of names-only contact strings.
  YAML
  placeholders intentionally keep raw YAML content unchanged (including `-`
  list markers), and pure YAML placeholder runs (for example a paragraph
  containing only `{YAML_BLOCK}`) are rendered in a small monospace run style
  (Consolas, 6pt), matching the built-in embedded YAML section appearance.

## [0.16.1] - 2026-08-14

### Security

- **The Shiny app's "Restore previous session" no longer exposes one user's
  work to another.** The autosaved workspace was written to a single fixed path
  in `tempdir()`. `tempdir()` is per R *process*, not per Shiny session, so
  every browser session served by the same worker — the normal arrangement
  under Shiny Server, Posit Connect, or a shared `runApp()` — read and wrote
  the same file. One user's spec, metadata, upload paths and collected table
  contents were offered to the next visitor behind the restore button, and each
  session silently clobbered the other's saved state.

  The slot is now keyed to a 128-bit random id the browser keeps in
  `localStorage`, and the payload carries that id and is rejected on restore if
  it does not match. The id is re-validated server-side as 32 lowercase hex
  characters before it is used to build a path, so a hostile value on the
  websocket cannot steer the write. Recovery after a reload or a crash still
  works, because the id is stable for a browser profile — unlike `session$token`,
  which is minted afresh on every page load and would have made the feature
  unreachable.

- **The Shiny app's custom Word template picker no longer resolves a name
  outside the bundled templates directory.** `get_template_path()` pasted
  `input$export_template_select` straight into a path. A Shiny client is not
  bound by the choices offered in a `selectInput` and can put any string on the
  websocket, so `"../.."` or an absolute path escaped
  `inst/extdata/templates`; the file was then rendered by
  `export_with_template()` and returned to the client as a download. The name
  must now match one of the bundled templates exactly.

## [0.16.0] - 2026-08-14

### Added

- **`validate_file_stream()` validates a delimited file without loading it.**
  The file is opened as a lazy Arrow dataset and scanned in batches, so peak
  memory is governed by the batch size rather than by the size of the file.
  This is what makes a file larger than memory checkable at all: the existing
  path has to hold the whole table as an R data frame before it can validate a
  single row.

  It returns the same validation details the in-memory path returns, so
  `results()`, `messages()` and `inspect()` accept the result unchanged.

  `max_errors` caps how much per-cell error detail is retained. Counting is
  unaffected, so totals and the pass/fail verdict stay exact even when the
  retained detail is truncated — a report says "20 problems, here are 5", never
  "5 problems".

  Nothing in the validation path now scales with the number of rows. Memory is
  bounded by the batch size for the column-spec checks, by the number of
  distinct keys for uniqueness rules, by the number of distinct groups for
  grouped rules, and by `max_errors` for retained error detail.

  **This buys feasibility, not speed.** Measured across a 16-fold increase in
  input, the working set held by the scan stayed flat at ~19 MB while the
  in-memory path's grew from 51 MB to 272 MB — but scanning ran about twice as
  slow, since every batch pays its own dispatch and typing overhead. Use it
  when holding the file is the problem; for a file that fits in memory
  comfortably, `validate_table()` remains the faster choice.

- A `DTADataSetTabular`'s `tables` may now hold a lazy Arrow `Dataset`,
  `arrow_dplyr_query` or `RecordBatchReader` as well as a materialised `Table`.

- **`validate_file_stream(fail_fast = TRUE)` stops at the first problem.**
  Answers "is this file valid?" without paying for a full pass, which on a
  large file that fails early is the difference between seconds and hours.

  The resulting report is explicitly incomplete rather than quietly so. It
  carries a `partial_scan` attribute, lists only rules that actually failed,
  and reports `NA` — not `TRUE` — for any axis that could not be settled. A
  rule that has not failed yet has not passed: a duplicate further into the
  file was simply never read. The overall `ok` verdict is unaffected, since it
  requires all three axes to be `TRUE`.

- **A structural gate, via `validate_file_stream(on_missing_column = "stop")`.**
  A column the specs require but the file lacks is decidable from the header
  alone. Scanning reports that absence once per *row* — faithful to what the
  generated schema meant, but useless at scale, since a 400-million-row file
  restates the same fact 400 million times. `"stop"` reports it once, having
  read nothing.

  The default is `"scan"`, so existing behaviour is unchanged unless you ask
  for the gate.

  A result produced this way carries a `structural_only` attribute. Its
  `rules_valid` and `import_valid` read `TRUE` because those axes were never
  evaluated, not because they passed — the attribute exists so that cannot be
  misread.

- **Columns present in the file but absent from the specs are now reported.**
  Previously invisible: the per-row checks have no way to notice a column no
  spec describes.

- **`check()` validates a lazy table by scanning it.** A dataset held in
  `tables` is no longer converted to a data frame first, so the streaming path
  is reachable from the ordinary workflow rather than only through
  `validate_file_stream()`.

  Cached validation results are keyed differently for lazy tables. A
  materialised table is still identified by hashing its contents; a dataset is
  identified by the files behind it — names, sizes, modification times — plus
  its column names, because hashing the contents would mean serialising the
  whole table to decide whether to skip validating it. The trade: file metadata
  can in principle miss an edit that preserves both size and timestamp. Where
  no identity can be established the table is treated as changed, so the
  failure direction is revalidating unnecessarily rather than skipping a table
  that needed checking.

### Changed

- Column spec validation no longer serialises the table to JSON and runs a JSON
  Schema validator over it. Each column's values are now checked directly
  against the constraints its spec declares. Validation output is unchanged:
  the same keywords (`type`, `maxLength`, `enum`, `const`, `pattern`,
  `required`), the same messages, the same row and column attribution, and the
  same summarised error frame. A golden-oracle test suite recorded against the
  previous implementation reports no differences.

  On a 1,000,000-row table the column spec axis went from being unmeasurably slow at
  that size to 8.7 seconds, and throughput rose from 4.5 MB/s to 17.6 MB/s.
  The axis is still the dominant cost of validation.

### Removed

- **`jsonvalidate` and `tidyr` are no longer dependencies.** `jsonvalidate`
  brought **V8** with it, a heavy system dependency that slowed installation
  and CI everywhere, not just validation. `tidyr` was used only to parse the
  validator's error paths back into row and column numbers, which is no longer
  necessary.

  `as_json_schema()` remains exported and unchanged. Serialising a spec
  collection to JSON Schema for other tools is useful in its own right; it is
  simply no longer how this package validates. It also no longer compiles a
  validator on every call and throws it away.

## [0.15.1] - 2026-08-13

### Fixed

- The `DTARuleGroupCondition()` documentation example could not be parsed, so
  `R CMD check` failed on it. The `requires` constraint was written as
  `list(type = "requires", if = "c1_failed", ...)`, but `if` is a reserved word
  and cannot be an unquoted argument name; it is now `` `if` = ``. The example
  runs. The test suite could not have caught this — examples are executed by
  `R CMD check`, not by `devtools::test()`.

- **Shiny app shows correct version on Posit Connect.** The footer's version
  lookup previously preferred the *installed* package version, which on Connect
  is the server-side library version rather than the deployed app's version. The
  lookup order is now reversed: a nearby `DESCRIPTION` file (the app bundle) is
  checked first, and the installed package is only the fallback. The displayed
  version now matches the running app on Connect and during local source
  development.

### Added

- GitHub Pages tutorial site (`docs/`): a self-contained, five-part static HTML
  tutorial covering installation, the beginner validation workflow, column
  specifications, all four rule types (`col_condition`, `col_range`,
  `col_unique`, `group_condition`), advanced API usage, and a quick-reference
  cheat sheet. Deployed automatically via `.github/workflows/pages.yml`.
- `.github/workflows/pages.yml`: GitHub Actions workflow that deploys `docs/`
  to GitHub Pages on every push to `dev` that touches the folder.
- `.github/copilot-instructions.md`: repository-level instructions for GitHub
  Copilot covering commands, S7 architecture, conventions, and subagent
  workflow.

### Changed

- `README.md` and `vignettes/DTAtools.Rmd`: expanded `group_condition`
  documentation with full prose, constraint-type and scope-value reference
  tables, two annotated YAML examples (`requires` and `mutually_exclusive`),
  and a programmatic `DTARuleGroupCondition()` constructor example.

## [0.15.0] - 2026-08-13

### Added

- New rule type `group_condition` for grouped cross-row validation.
  Rules define `group_by`, named `conditions`, and `constraints` so checks like
  mutually exclusive statuses or implication logic can be enforced within each
  group. Constraint aliases are supported: `not_both` maps to
  `mutually_exclusive`, and `implies` maps to `requires`.
- The Shiny app's *Edit rules* dialog now supports full GUI authoring of
  `group_condition` rules, including grouped condition rows, grouped
  constraints, and round-trip serialization to YAML.

- `dta_template_placeholders()` is exported. It lists the `{PLACEHOLDER}`
  tokens a Word export template may use, and given a `DTA` resolves each one, so
  a template author can discover the set without exporting a document to find
  out or reading it out of the documentation by hand.
- Creation templates accept a **`target:` shorthand**. `target: metadata.title`
  replaces the four-line `effects: / __selection__: / path: / value:` block that
  every option previously needed to say "write my value to this field".
  `effects:` still works, and is still the way to have one choice set several
  fields at once.
- Creation-template values may use **`${today}` and `${version}`**, resolved
  when the DTA is created.
- Creation templates are searched for in more than one place:
  `getOption("DTAtools.template_dir")`, then a project-local `./dta-templates`,
  then the packaged directory. The packaged directory sits inside the installed
  library, which users cannot write to and a reinstall wipes, so it could not
  remain the only place a template was allowed to live.
- A dataset's **file handlers can be edited in the Shiny app**. A third button,
  *Edit files*, sits next to *Edit columns* and *Edit rules* and opens the same
  kind of list/form dialog: add, edit, remove and reorder the expected files
  (name or pattern, type, how many files may match, description). Each entry is
  one upload slot, so adding one adds a slot and removing one removes it.
- `pattern_description` reaches the concrete file classes. `DTAFile` has always
  had the property and the app has always written it into `files:`, but no
  `DTAFileCSV`/`DTAFileTSV`/`DTAFileDelim`/`DTAFileTabular` constructor accepted
  one, so a handler that described its own pattern in words could be written and
  never read back.
- A dataset may declare **more than one file handler in YAML**. `files:` is now
  read as either a single mapping (one handler, unchanged) or a sequence of
  mappings (one per handler). A dataset with no `files:` block at all is read as
  a dataset with no handlers instead of aborting. `dta_file_handlers_from_list()`
  is exported for the conversion.

### Changed

- **Unresolved placeholders in a Word template are now reported whatever their
  case.** Detection and reporting previously used different patterns, so a
  lower- or mixed-case token such as `{customField}` was left untouched *and*
  never warned about, contradicting the documented contract that every
  placeholder without a value is reported. Both now read one shared grammar.
  Note the consequence: braces used as prose in a template, such as `{n}`, will
  now produce a warning. The text is still left exactly as written.
- A creation-template option that omits `default:` inherits the value from
  `base.metadata`, so a template states each value once. Previously the two
  duplicated each other with nothing enforcing agreement, and the option
  silently won whenever they drifted apart.
- The metadata fields a creation template may write are derived from the
  `DTAMetaData` S7 class instead of being mirrored by hand in three places, so a
  new property cannot silently become un-settable from a template.
- The three *Edit files* / *Edit columns* / *Edit rules* buttons are now one
  **Edit** menu. They all act on the same object — this dataset's specification
  — so they read as one entry point instead of three siblings competing with
  *Check this dataset* and the export. Each row names what it changes
  (*Columns*, *Rules*, *Files*, in that order) with a one-line description.
- Removing a file handler in the app also unloads the files that were loaded
  through it, after a confirmation listing them by name. The specification and
  the loaded data are kept in step: a slot that no longer exists can no longer
  hide bound data from the *Loaded files* list.
- Applying edited **Raw YAML** is less destructive. Editing a dataset's `files:`
  block used to discard every file loaded into that dataset; loaded files are
  now kept as long as their own slot is still in the document, and follow it if
  the entries were reordered. A file whose slot was deleted or rewritten is
  unloaded with it, rather than left bound to the dataset under a slot that asks
  for something else. Validation is still cleared whenever files, columns or
  rules changed.
- The `pre-commit` hooks run for the first time. The R hooks could not build
  their environment on R 4.5 (the pinned revision installed a `digest` that no
  longer compiles), so `styler` and `roxygen` had never been applied; the
  source is reformatted accordingly. Hook revisions are updated, `roxygenize`
  declares the dependencies it needs to load the package, and the vendored
  `renv/staging_excluded/` tree is excluded so hooks stop trying to lint other
  packages' sources.
- The `pre-commit` CI job no longer fails when GitHub's cache service is
  unavailable. `pre-commit/action@v2.0.3` bundled its own cache step with no
  error tolerance, so an outage failed the job before a single hook ran. The
  cache is now inlined with `continue-on-error`, and a cache problem only makes
  the run slower.
- The R-specific `pre-commit` hooks (`lorenzwalthert/precommit`) are removed.
  They built a second, isolated ~40-package `renv` library on every run,
  separate from the one CI already installs, and that duplication broke three
  times in a row: a `digest` that will not compile on R 4.5, a cache outage
  with no error tolerance, and finally the two `renv` caches colliding on the
  same runner path. `styler`, `roxygen2`/`NAMESPACE` and a
  dependency-declaration check now run in a new `r-style` workflow against the
  project's own installed library. `pre-commit` keeps only the fast,
  language-agnostic hooks. Note that `styler::style_pkg()` and
  `roxygen2::roxygenise()` are no longer run for you locally — see the Commands
  table in `CLAUDE.md`. CI fails on a diff rather than auto-fixing, so stale
  `man/`/`NAMESPACE` cannot be silently committed on your behalf.

### Fixed

- **A Word template no longer loses its formatting where a placeholder sits.**
  Any placeholder in a paragraph caused the whole paragraph's text to be written
  into its first run with every other run blanked, so
  `Vendor: **{SUPPLIER_NAME}** (confidential)` came back with the bold and the
  trailing run's styling gone. Substitution is now run-local, and falls back to
  joining the paragraph only when a placeholder genuinely straddles a run
  boundary — which Word does routinely, and which is the only case where
  joining is the sole way to match the placeholder at all.
- **A placeholder value containing another placeholder's token is no longer
  re-substituted.** Substitution looped `gsub()` over the variable names,
  mutating the text each pass, so a title such as `"See {DTA_VERSION} below"`
  had the version interpolated into it. Substitution is now a single pass over
  the original text and a value is never rescanned. This also removes the
  mirror-image defect where braces arriving from a value were reported as
  unresolved placeholders the template never contained.
- A creation template's dataset reference is no longer resolved against the
  process working directory. A packaged template asking for `gf_dataset.yaml`
  could silently pick up an unrelated file of that name from wherever the app
  happened to be launched; a bare relative name is now resolved against the
  template's own directory, then the package, and only a genuinely absolute
  path is taken as given.
- A DTA created from the bundled GF template is dated the day it was created,
  and its first version-history entry records the version the user actually
  chose. Both were frozen at the template author's values, so every DTA claimed
  to be dated 2026-07-29 and at version 1.0 regardless.
- `export_with_template()`'s example is no longer wrapped in `\dontrun{}`. It
  writes only to `tempdir()`, so it now runs under `R CMD check` like every
  other example instead of being documentation nobody executes.
- A handler that is not a pattern now rejects any file count other than 1
  whichever way it is declared. The guard only ever looked at
  `number_of_files`, so a `min_number_of_files`/`max_number_of_files` pair went
  unchecked, and declaring only a minimum compared against a zero-length value
  and failed with a message about something else.
- A file handler could not carry more than one file name. `filename` is
  documented as a character vector and `matches_filename()` implements the
  several-names case, but the validator tested `filename == ""` — a length-1
  test that made the condition length 2 and errored — and a YAML `filename:`
  sequence arrived as a list the character property refused. Both now work.
- A specification with more than one file handler could be written but never
  read back. The app already serialised such a dataset as a `files:` sequence,
  while the reader passed the whole sequence where a single handler was
  expected and died inside a base-R coercion, so exporting a two-handler DTA
  produced a document the app itself rejected on load.
- The whitespace hooks no longer rewrite files under `inst/extdata`. In a
  delimited file trailing whitespace is data: `trailing-whitespace` stripped
  the trailing tabs from the one row of `gf_data_small_smirna.tsv` whose last
  columns are legitimately empty, turning a 33-field row into a 27-field one.

## [0.13.0] - 2026-08-12

> **Data that passed validation before may now fail, and that is the point of
> this release.** Several defects caused invalid data to be reported as clean.
> Re-run `check(..., force = TRUE)` on existing datasets: validation artifacts
> written by earlier versions report the new import axis as unknown rather than
> as passing, because they were never checked for it.

### Added

- Validation now has a third axis. Alongside column spec and rule errors, an **import
  error** records a value that cannot be represented in its declared type. The
  value becomes `NA`, the original text is retained, and any import error makes
  validation fail. Surfaced through `validation_status()`, `results()`,
  `messages()` (as `source = "import"`) and `inspect()`, all of which gain
  `n_import_errors`.
- Columns are typed against the specification at import. `as_r_type()` maps a
  declared type to an R type, and `dta_coerce_table_to_specs()` applies it when
  a file is read. Previously the declared type was used only to build the JSON
  schema and never to read a column.
- `check()` validates metadata. A metadata import error now fails the whole DTA
  instead of being recorded while the banner still reported success.
- `metadata_import_errors()` returns a `DTAMetaData` object's import issues.
- `as.data.frame()` method for validation details.
- `labels()` is exported. It was defined but never exported, so in an installed
  package the call fell through to `base::labels.default` and silently returned
  `"1"` — a wrong answer rather than an error. It is registered through the same
  guard the package uses for `names()` and `print()`, so `base::labels` keeps
  working for every other class.
- Unit tests for the bundled Shiny app, which previously had none: its helper
  files are auto-sourced by Shiny at launch and were therefore invisible to the
  test suite.
- `dta_pdf_backend()` reports which DOCX-to-PDF backend will be used, or `NULL`
  when none is available, so a user can check their setup before they need it.
- PDF export tries LibreOffice (`soffice`), then TinyTeX, then pandoc with any
  other PDF engine. LibreOffice is preferred where present because it renders
  the Word document as Word does, preserving table shading, column widths and
  numbered-heading fields; pandoc re-parses to its own AST and reflows the
  layout through LaTeX.
- An end-to-end PDF export test that performs a real conversion and asserts the
  `%PDF` magic bytes, rather than mocking the converter. CI installs TinyTeX on
  all five platforms and fails fast if no backend is present, so that test
  cannot silently start skipping.
- `inst/extdata/clinical_data_error_import.csv`, an example file isolating the
  import-error axis the way the existing fixtures isolate column spec and rule
  errors. It deliberately includes a genuinely blank cell alongside the
  unconvertible ones, because missing and unconvertible are different defects
  and only the latter is an import error.
- `inst/extdata/clinical_data_error_all.csv` now carries import errors too, so
  it exercises all three validation axes rather than two.

### Changed

- Dates in exported documents are ISO 8601 (`YYYY-MM-DD`). They previously used
  `%B`, so the same DTA produced `Januar 15, 2026` on a German machine and
  `January 15, 2026` in CI — two different legal documents from one input.
- `write_dta(format = "pdf")` aborts when no PDF backend is available instead of
  producing a DOCX with a `.pdf` extension.
- The template fallback notice is a warning condition rather than a message, so
  callers can trap it with `tryCatch()` or `options(warn = 2)`.
- `include_yaml = TRUE` warns when it cannot be honoured instead of being
  silently discarded.
- `@values` is normalised to an atomic vector, so a spec collection survives a
  YAML round trip with whole-object equality.
- `check()` on file datasets honours `force`, `persist`, `artifact_dir`, `quiet`
  and `tables`, which it previously accepted and ignored.
- Removed the unused `export_modal_ui()` helper from the Shiny app. The export
  modal has always been built inline in `app.R`; the orphaned builder produced
  module-namespaced ids that no server ever observed.
- `inst/extdata/gf_data_small_smirna.tsv` reduced from 20940 rows to 490,
  taking the installed package from 7.6 MB to under 1 MB and clearing the CRAN
  installed-size NOTE. The rows were selected rather than truncated: every
  distinct value of every column with at most 100 distinct values survives,
  plus a systematic sample. A plain `head` would have dropped the file's single
  `GFSTAT = "NOT DONE"` record, the only carrier of the second value of five
  columns. A test pins what the reduction was chosen to keep.

### Fixed

- **Conditions with more than one operator dropped all but the first.**
  `then: {AGE: {greater: 18, less: 65}}` never evaluated `less`, so `AGE = 999`
  validated as clean.
- **Range rules compared factor level codes.** `factor(c("500","600","700"))`
  against `min: 0, max: 100` passed, because `as.numeric()` yielded `1, 2, 3`.
  Non-numeric text coerced to `NA`, which counted as a pass.
- **Numeric comparisons on character columns used locale collation**, so
  `"9" > 65` was `TRUE` and an underage subject passed an adults-only rule.
- **Conditions written as a YAML sequence returned valid = TRUE**, silently
  passing every row rather than being evaluated.
- **Rule violations were invisible whenever a column spec error existed**, because
  validation returned early. Both axes are always evaluated now.
- **A rule naming a column absent from the table aborted the entire run**
  instead of reporting a rule failure.
- **A date-prefixed phrase was silently converted to a date.**
  `"2026-12-31 at the earliest"` became `2026-12-31`, destroying the
  qualification. The date is kept and the original text is recorded as an
  import error.
- **Metadata dates serialized as bare numbers**, so a written DTA could not be
  read back.
- **Declared `Char` columns lost their text at read.** `"007"` arrived as `7`
  because arrow inferred the type from the data before any package code ran.
- `DTAFileTabular` was registered under the name `DTAFile`, so its methods
  overwrote the base class methods and every load printed `Overwriting method`.
- `DTAFileDelim` never passed its separator to the reader, so tab-delimited
  files parsed into a single column.
- `has_header = FALSE` discarded the first data row and promoted the second to
  the header.
- Two of the three `DTAFileTabular` validator rules could never fire.
- `DTADataSet` accepted `description` and the three `template_*` arguments and
  discarded them.
- Two files sharing a basename collapsed into one validation result, and
  `inspect()` reported the wrong path for them.
- A single `NA` metadata field aborted every export format.
- The compact six-column specification table returned seven columns.
- `as.list()` emitted the literal `"SAS "` for an unset format, so the bundled
  example collection could not survive its own YAML round trip.
- `validation_errors()` returned a list that could not be coerced to a data
  frame.
- The Shiny app ignored import errors when colouring table status, showing a
  failing table as clean.
- **`check()` claimed a table was valid and then failed it.** The console report
  covered the column spec and rule axes but not the import axis, so a table whose
  only defect was an unconvertible value printed
  `Table format, length, pattern, and values are valid` followed by
  `0 of 1 table valid`, with no stated cause. It now names the row, column, raw
  text and declared type.
- `export_column_value_table()` on a column with no `values` failed with a raw
  R error about `names` attribute lengths instead of naming the column.
- The vignette could not be built. It selected `inspect()` columns by a
  hard-coded message id, which the new import axis reordered.
- Declared `htmltools` and `tinytex` in `Suggests`. Both are used by the bundled
  Shiny app but were undeclared, working only because `htmltools` arrives
  transitively with `shiny`. `R CMD check` does not scan `inst/`, so neither was
  flagged.
- **PDF export reported that a backend was available when it was not.** The
  check tested only for pandoc, but pandoc cannot write a PDF on its own — it
  needs a separate PDF engine. With pandoc installed and no engine the guard
  returned `TRUE`, so users bypassed the actionable "install this" error and
  received a raw `pandoc document conversion failed with error 47` instead. The
  one branch that named a fix was unreachable exactly when it was needed.
- PDF export via TinyTeX routes through `tinytex::latexmk()` rather than
  invoking pandoc's LaTeX path directly. Going direct bypasses TinyTeX's
  on-demand package installation, so a bare TinyTeX failed for want of
  `caption.sty`.
- TinyTeX's binary directory is not always on the session `PATH`, which made
  `pdflatex` appear missing even when TinyTeX was installed. It is now resolved
  through `tinytex::tinytex_root()`.
- External tool output containing braces crashed the error formatter, because
  the text was interpolated by cli before being reported.
- The two Shiny test harnesses left behind by a merge are consolidated into
  one. The surviving harness gained the more defensive app-directory lookup
  from the one it replaced: it validates that `app.R` actually exists at the
  resolved path instead of only checking the path string is non-empty.

## [0.12.2] - 2026-08-04

### Added

- Shiny app header now includes quick links for `Report issues` and `About`, so users can directly open the GitHub issue tracker, repository overview, and package documentation from the top bar.

### Changed

- Refined Shiny app brand-bar action styling for better visibility and responsive wrapping on smaller screens.

## [0.12.1] - 2026-07-30

### Added

- `export_with_template()` for filling a user-authored Word (`.docx`) template with values from a `DTA` object. Placeholders use a single-brace, upper-case convention (e.g. `{DTA_TITLE}`, `{SUPPLIER_NAME}`, `{RECEIVER_CONTACTS}`, `{TRANSMISSION_TYPE}`, `{TOTAL_COLUMNS}`) covering agreement metadata, supplier/receiver affiliation and contacts, transmission details, dataset content, and process information. Substitution runs at the WordprocessingML level so special characters (`&`, `<`, `>`) are escaped automatically and placeholders split across runs are still matched; callers can override or add values via `variables`, and template failures fall back to the built-in layout unless `fallback = FALSE`.
- `write_dta()` gains `template` and `template_variables` arguments that route DOCX/PDF output through `export_with_template()` when a template is supplied.
- Shiny app export dialog replacing the two separate export buttons with a single "Export" action that offers Markdown (optionally converted to PDF via Pandoc, with an optional hidden embedded-YAML block), built-in Word, and custom-template Word output. Custom templates are auto-discovered from `inst/extdata/templates`, and the `{DATASETS_SUMMARY}`, `{DATASETS_DETAIL}`, and `{YAML_EMBEDDED}` placeholders are filled from the current `DTA`. Adds `shinyjs` to Suggests.
- Shiny app "Create new from template" workflow on the landing page: a declarative YAML template engine (`inst/shiny/dta_app/R/template_core.R`, templates discovered from `inst/extdata/templates/*.dta-template.yaml`) builds a new `DTA` from a bundled dataset structure plus a two-step options modal, and ships a `biomarker_gf` GF DTS template. Template `base.metadata` and every option `effect` target only canonical `DTAMetaData` fields (`title`, `version`, `date`, `header`, `version_history`, `receiver`, `supplier`, `error_handling`, `authorized_for_corrections`, and `transmission.{type,frequency,notification,date_first_transfer,date_last_transfer,test_upload,blinded_transfer}`), so every generated value is visible and editable in the app metadata editor. A single option can set multiple fields, `boolean` options render as Yes/No, and every non-boolean dropdown offers its suggested values plus a `(leave blank)` entry and a `Custom...` entry that reveals a companion text field for a free-typed value. The bundled `biomarker_gf` template uses generic, vendor-neutral example values.
- `write_dta()` gains `include_yaml` and `yaml_text` arguments that append the machine-readable YAML specification as a final, very small-font monospace section of the built-in DOCX/PDF layout (YAML indentation is preserved). Bundles a numbered Word reference template (`inst/extdata/templates/dta_numbered_template.docx`) that adds a `heading 4` style bound to the template's multilevel heading list, so the built-in writer's headings render as true Word list fields that auto-number (`1` / `1.1` / `1.1.1` / `1.1.1.1`) and renumber if the document is edited.

### Changed

- Built-in Word/PDF export (`write_dta()`) now nests all content under a single numbered `Data Transfer Agreement` chapter, and folds each dataset's specifications into the `Datasets` section instead of emitting flat, unnumbered bold subheadings: `Datasets` → `<dataset>` → `Files` (e.g. `1.4.1.1`) and `Dataset Specifications` (e.g. `1.4.1.2`, with `Column Specifications` and `Validation Rules` beneath it). The `Files` section now states each expected file's match type (exact name vs. regex pattern), format (CSV/TSV/Delimited), expected count, and description.
- Shiny app export dialog: the "Embed YAML" checkbox now also applies to the built-in Word export (previously it only affected custom templates), appending the small-font YAML section, and is relabelled `Embed YAML specification at end of document`.

### Fixed

- `write_dta()` (and the built-in DOCX/PDF writer) no longer aborts with `invalid 'pattern' argument` when the `DTA` metadata has no `title` (for example a `DTA` built from a standalone dataset YAML) or the title contains regex metacharacters. The title section no longer matches the title text as a regex via `officer::cursor_reach()`.
- Shiny app export dialog: Markdown and built-in Word exports no longer fail with `argument is of length zero`. The handlers wrongly tested a non-existent `$ok` field on the `write_dta()` / `export_with_template()` return value; both functions signal failure by erroring, not by returning a status list.
- Shiny app export dialog: the dataset/specs detail block no longer fails with `cannot coerce type 'object' to vector of type 'character'`; column and rule descriptions are now built via the app's list extractors (`dta_column_to_list()`, `dta_rule_to_list()`) instead of coercing raw `S7` objects to character.
- Shiny app export dialog: the `{YAML_EMBEDDED}` placeholder is always filled (blanked when YAML embedding is off) instead of being left as literal text in the exported Word document.
- Shiny app export dialog: exporting now actually starts the browser download instead of only showing `Document exported successfully`. The hidden download button was fired with `shinyjs::click()` (a jQuery-style event that does not invoke a download link's native navigation) inside a `display: none` container (which prevents native download clicks in some browsers). It is now triggered via a custom message handler that calls the element's native `.click()`, and the button is rendered off-screen instead of `display: none`.
- Shiny app export dialog: Markdown-to-PDF export now works without a LaTeX installation. It first tries pandoc with a LaTeX/`wkhtmltopdf` engine (best typography when present) and otherwise prints the rendered HTML to PDF with a headless Chrome/Edge browser (no extra R packages required; honours the `DTATOOLS_CHROME` override). Only if neither route is available does it fall back to delivering the Markdown file with a clear notification, instead of aborting the export with `pandoc document conversion failed` / `pdflatex not found`.
- Shiny app: replaced `shiny::hidden()` (not exported by older `shiny`) with a CSS-hidden container so the app loads on those versions.

## [0.12.0] - 2026-07-28

### Added

- `run_dta_app()` and a bundled Shiny application (`inst/shiny/dta_app`) providing a modern, Boehringer Ingelheim-themed UI for the package: drag-and-drop loading of a DTA YAML, per-dataset file uploads driven by each dataset's `DTAFile` handlers (with filename/pattern and min/max-count guidance), one-click or per-dataset validation via `check()` with green/red status, a clickable messages list backed by `inspect()`, incremental metadata and contacts editing, Word/PDF export via `write_dta()`, a read-only raw-YAML view, and in-session autosave/recovery. Requires the suggested packages `shiny`, `bslib`, and `DT`.

### Fixed

- GitHub Actions CI configuration: pinned S7 dependency to version 0.2.2 to ensure consistent package behavior across local development and GitHub Actions runners
- renv bootstrap files (`renv/activate.R` and `renv.lock`) are now tracked in git to enable proper environment restoration in GitHub Actions CI; `.gitignore` updated to only exclude generated directories (`renv/library`, `renv/staging`, `renv/settings.json`)
- added `remotes` to package Suggests and GitHub Actions workflow setup-r-dependencies to resolve macOS CI failures during S7 installation
- `inspect()` (and the Shiny app's message inspector) no longer errors with `Can't find property <DTAtools::DTARuleColRange>@range` when highlighting the failing rows of a `check_range` rule. The failing-row helper now reads the rule's `min`/`max` properties directly instead of a non-existent `range` property.
- `validate_table()` / `check()` no longer abort with `No method asJSON S3 class: vctrs_unspecified` when a table contains a fully-empty column. Arrow reads such columns as its `null` type (a `vctrs_unspecified` vector in R), which `jsonlite::toJSON()` cannot serialise; they are now coerced to all-`NA` and emitted as JSON `null`, so nullable empty columns validate correctly instead of crashing the whole validation run.


## [0.11.0] - 2026-07-27

### Added

- `max_number_of_files()` and `min_number_of_files()` generics on `DTADataSet`, aggregating counts across all files in the dataset
- comprehensive test coverage for previously untested export APIs (`write_dta()`, `write_dataset_metadata()`, `write_file_specification()`, `export_specs_table()`, `export_column_value_table()`, `write_metadata()`)
- direct unit tests for `validate_table()` / `validate_table_detailed()` behavior on valid input, column spec violations, and rule violations
- package architecture diagram (`img/DTAtools_architecture.svg`, also embedded in `vignettes/`) illustrating the `DTA`/`DTADataSet`/`DTAColumnSpecCollection`/`DTAFile` class hierarchy, referenced from both the vignette and a new `README.md` "Package Architecture" section

### Changed

- reworked the vignette and `README.md` guidance around DTA structure and YAML-first import workflows, clarifying the relationship between metadata and datasets and prioritizing `read_dta_from_yaml()` / `read_dataset_from_yaml()` as the primary entry points
- updated the `R-CMD-check` GitHub Actions workflow so pushes and pull requests targeting `dev` also trigger checks
- brought `README.md` closer to parity with the vignette: expanded the Core Classes / Key Functions reference tables (`DTAFileDelim`, `validation_status()`, `validation_errors()`, `datasets()`, `tables()`, `get_table()`, `labels()`, `specs()`, `colspec()`, `rules()`, `metadata()`, `write_columns_to_yaml()`/`write_columns_to_json()`, `as_json_schema()`), and added sections on inspecting results with `inspect()`/`validation_status()` and on building a full `DTA` object that mixes `DTADataSetTabular` and `DTADataSetFile` datasets
- added the package logo to the vignette

### Removed

- removed the `DTARuleCollection` class; `DTAColumnSpecCollection@rules` (and `rules(x)`) is now a plain list of `DTARule` objects (or `NULL`) instead of a `DTARuleCollection` wrapper object

### Fixed

- resolved all `R CMD check` WARNINGs (`checking Rd \usage sections`, code/documentation mismatches) stemming from S7 methods being documented against the generic dispatcher's `(x, ...)` signature rather than each method's real formals; S7 generics with multiple class-specific methods (`check`, `colspec`, `load_file`, `rules`, `read_file_execution`, `max_number_of_files`, `min_number_of_files`, `metadata`, `files`, `tables`, `columns`, `get_table`, `inspect`, `validation_status`, `validation_errors`, `clear_validation`, `column_preview`, `rule_preview`) now document all class methods on a single shared Rd page per generic
- fixed stale/incorrect constructor argument documentation: `DTAFile`/`DTAFileCSV`/`DTAFileDelim`/`DTAFileTSV` incorrectly documented a `sep` parameter that doesn't exist on their constructors, while omitting the real `min_number_of_files`/`max_number_of_files`/`info`/`pattern_description` parameters; `DTAColumnSpec` (`examples`, `colclass`), `DTAColumnSpecStructure` (`backend`), `DTADataSet`/`DTADataSetFile`/`DTADataSetTabular` (`description`, `template_source`, `template_version`, `template_date`), `DTARule`/`DTARuleColCondition`/`DTARuleColRange`/`DTARuleColUnique` (`description`, and `min`/`max` for `DTARuleColRange`) were also missing `@param` documentation for real constructor arguments
- removed the unused, dead `encoding` constructor parameter from `DTAFileTabular` (accepted but never stored or used)
- fixed malformed `\usage{}` blocks for the `` `[[` `` / `` `[` `` operator methods on `DTA`, and added missing `\alias{}` entries for the operators
- fixed duplicate Rd `\name{}` entries and out-of-tree scratch files leaking into the build (`.Rbuildignore` additions)
- closed test-coverage gaps for DTA/Dataset YAML error paths, `DTAFileDelim` read behavior, `DTAColumnSpecStructureFactory` backend-prefix validation, and rule-engine edge cases (`min`/`max`, comparison operators, missing columns, empty rule lists)

## [0.10.0] - 2026-07-25

### Added

- print method to all instantiable classes
- added `examples` to `DTAColumnSpecs`, which need to be following a pattern if provided and are mutually exclusive from `values`
- introduced `colclass`, which is a placeholder for further addition for automatic preprocessing efforts
- file handling with classes: `DTAFile`, `DTAFileTabular`, `DTAFileCSV`, `DTAFileTSV`, `DTAFileDelim`
- introduced classes `DTARule`, `DTARuleCollection`, `DTARuleColCondition`, `DTARuleColRange`, `DTARuleColUnique` for rules
- introduced classes `DTADataSetTabular` handling tabular data, deriving from `DTADataSet`
- introduced class `DTADataSetFile` for validating file presence, non-emptiness, and readability of non-tabular deliverables, with its own `check()`, `results()`, `messages()`, and `inspect()` methods
- introduced `inspect()` generic for `DTADataSetTabular` and `DTADataSetFile` to drill into a specific validation error by `id`, returning row context, the failing JSON Schema clause, or the rows that violated a rule
- functions to access slots
- S7 validators for all classes
- introduced `DTAColumnSpecStructure` and `DTAColumnSpecStructureSAS` for handling `type`, `format`, `length` of a column spec
- info variables to `DTADataSet` and `DTADataSetTabular`
- example factory functions: `create_example_DTA()`, `create_example_DTAColumnSpec()`, `create_example_DTAColumnSpecCollection()`, `create_example_DTADataSetTabular()`, `create_example_DTAFileCSV()`, `create_example_DTAFileTSV()`, `create_example_DTAMetaData()`, `create_example_DTARuleColCondition()`, `create_example_DTARuleColUnique()`, `create_example_DTARuleColRange()`
- `as.list` methods for `DTAColumnSpecCollection`, `DTAColumnSpec`, and `DTARule` derivatives

### Changed

- renamed functions to snake_case
- renamed DTAFileInfo to DTAFile
- renamed DTAContainer to DTADataSet
- improved GitHub Action workflows
- reworked data backend to use arrow::Table for better performance and memory usage
- completely reworked the package vignette (`vignettes/DTAtools.Rmd`) with a full walkthrough of architecture, column specs, validation, rules, `DTADataSetFile`, file-based workflows, the full `DTA` object, `inspect()`, and exporting — every code chunk verified to run against the installed package
- completely reworked `README.md` to match the vignette: corrected terminology (Data Transmission Agreement/Specification instead of Data Transfer), fixed outdated/broken code examples, updated rule type names (`col_condition`/`col_range`/`col_unique`), and documented `DTADataSetFile` and `inspect()`
- re-prioritized YAML import guidance in the vignette and `README.md`: `read_dta_from_yaml()` and `read_dataset_from_yaml()` are now presented as the primary entry points, with `import_specs_from_yaml()` documented as the third, most manual option; added a `read_dataset_from_yaml()` walkthrough to the Quickstart in both documents
- fixed broken anchor links in `README.md` caused by unsupported Pandoc-style `{#custom-id}` heading syntax
- fixed `load_file()` so it is properly exported from the package namespace (previously only accessible via `DTAtools:::load_file`)
- improved testthat tests
- improved CLI messages
- moved rules to `DTARule` and derivative classes
- renamed getter functions to shorter names
- renamed constructor variables
- DTA-class constructor to handle DTAMetaData
- changed `container` to `datasets` in class DTA
- moved json schema generation to classes
- removed stored `json_schema` as it can be dynamically generated

## [0.9.0] - 2025-09-11

### Added

- Added GitHub Action workflows
- Added Metadata to DTAData

### Changed

- Moved project to GitHub.com
- Modified documentation and examples to adhere to R CMD checks for CRAN

## [0.8.1] - 2025-08-18

### Added

- `DTAColumnSpecCollectionToList` for returning a list object
- tests for new function

### Changed

- `write_specs_to_yaml` exports also rules

## [0.8.0] - 2025-08-18

### Added

- methods: `get_rules`, `metadata`
- better `cli` messages for improved reporing
- `DTARule` class for object-oriented usage of defined rules
- new tests, including tests with the test data sets

### Changed

- Vignette and `README.md` to fit new functions and usability
- simplified evaluation checks
- CLI
- `rules` usage caused by change to `DTARule`

### Removed

- BI specific information
- redundant validation code

### Deprecated

- `validate_table` without using jsonschema

## [0.7.5] - 2025-07-07

### Added

- `specs_from_list`: Simple import from lists. Can be used in combination with `params.yaml` files managed by dso

### Changed

- allowing numeric values
- pattern information - no quoted strings allowed
- required statement is now in the correct location in the jsonschema

### Fixed

- include dplyr functions

## [0.7.4] - 2025-06-26

### Changed

- jsonschema is now part of the `DTAColumnSpecCollection`
- implemented progress bar to signal status of table validation
- better function messages

## [0.7.3] - 2025-06-26

### Fixed

- export functions are now exported into the NAMESPACE

## [0.7.2] - 2025-06-26

### Fixed

- `DTAColumnSpecCollection_to_jsonschema` - fix values when values: ""

## [0.7.1] - 2025-06-26

### Fixed

- `validate_table_with_jsonschema` fix

## [0.7.0] - 2025-06-25

### Changed

- Switch to using `jsonschema` for validating the correctness of the table
- Improving error messages when using `jsonschema`
- introducing the variable length to check the maximum length of a value in a column

## [0.6.0] - 2025-06-13

### Changed

- `import_specs_from_word` has been updated to new value and pattern format, making it easier to read and extract
- `export_specs_table`, similarly to above. Values and pattern will now be printed in a new format

## [0.5.0] - 2025-06-13

### Added

#### Core Classes

- `DTAColumnSpec`: Defines metadata and validation rules for a single column.
- `DTAColumnSpecCollection`: Manages a collection of `DTAColumnSpec` objects with optional metadata and rules.
- `DTAContainer`: Encapsulates validated data tables and their associated column specifications.

#### Import/Export

- YAML and JSON import/export for `DTAColumnSpecCollection`.
- Word document import via `docxtractr` for DTA specifications.
- Word document export of column specs and value tables using `flextable`.

#### Validation

- Column-level validation:
  - Type checking
  - Format length enforcement
  - Nullability enforcement
  - Value list enforcement
  - Regex pattern matching
- Table-level validation:
  - Ensures all required columns are present
  - Applies all column validations
  - Applies rules if defined
- JSON Schema generation and validation support

#### Schema Rule Engine

- Rule types implemented:
  - `check_equal`
  - `check_unequal`
  - `check_range`
  - `check_dependency`
  - `check_mutual_exclusive`
  - `check_unique`
  - `check_allowed_combinations`
- `apply_rules()` to evaluate all rules with CLI feedback

#### Utilities

- `checkFormat`, `checkType`, `checkNullable`, `checkValues`, `checkPattern`, `change_type`, `changeNAs`, `prepareTable`, `validateColumn`
- `validateSchemaRulesFormat()` to validate rule structure before use

#### Output

- `write_table_to_file()` to export validated tables with optional sorting, compression, and metadata
- MD5 checksum and dimension reporting

#### Developer Support

- `testthat` test suite for all validation and rule functions
- Pre-commit hook configuration to enforce test execution before commits

### Notes

- This is the initial release of the `dtatoolsr` package.
- Designed for robust, schema-driven validation of tabular datasets in regulated environments.

## [0.4.0]

- Implemented rule system for validating table format
- Started to implement the use of jsonschema as table validator
- added testthat tests

## [0.3.0]

- Implement function to write word table for DTA
- Improve warnings, errors, and info messages
- Make naming convention consistent
- Update write table function to include md5sum and dimension calculations
- Export tables into word file
- Import ColumnCollection from Word File

## [0.2.0]

- Improved information printouts
- improved naming conventions
- added write_table_to_file function to write valid DTA table
- added methods to extract tables, columns and columncollections

## [0.1.0]

- Initial internal release

[Unreleased]: https://github.com/Boehringer-Ingelheim/DTAtoolsR/compare/v0.10.0...HEAD
[0.10.0]: https://github.com/Boehringer-Ingelheim/DTAtoolsR/compare/v0.9.0...v0.10.0
[0.9.0]: https://github.com/Boehringer-Ingelheim/DTAtoolsR/compare/v0.8.1...v0.9.0
[0.8.1]: https://github.com/Boehringer-Ingelheim/DTAtoolsR/compare/v0.8.0...v0.8.1
[0.8.0]: https://github.com/Boehringer-Ingelheim/DTAtoolsR/compare/v0.7.5...v0.8.0
[0.7.5]: https://github.com/Boehringer-Ingelheim/DTAtoolsR/compare/v0.7.4...v0.7.5
[0.7.4]: https://github.com/Boehringer-Ingelheim/DTAtoolsR/compare/v0.7.3...v0.7.4
[0.7.3]: https://github.com/Boehringer-Ingelheim/DTAtoolsR/compare/v0.7.2...v0.7.3
[0.7.2]: https://github.com/Boehringer-Ingelheim/DTAtoolsR/compare/v0.7.1...v0.7.2
[0.7.1]: https://github.com/Boehringer-Ingelheim/DTAtoolsR/compare/v0.7.0...v0.7.1
[0.7.0]: https://github.com/Boehringer-Ingelheim/DTAtoolsR/compare/v0.6.0...v0.7.0
[0.6.0]: https://github.com/Boehringer-Ingelheim/DTAtoolsR/compare/v0.5.0...v0.6.0
[0.5.0]: https://github.com/Boehringer-Ingelheim/DTAtoolsR/compare/v0.4.0...v0.5.0
[0.4.0]: https://github.com/Boehringer-Ingelheim/DTAtoolsR/compare/v0.3.0...v0.4.0
[0.3.0]: https://github.com/Boehringer-Ingelheim/DTAtoolsR/compare/v0.2.0...v0.3.0
[0.2.0]: https://github.com/Boehringer-Ingelheim/DTAtoolsR/compare/v0.1.0...v0.2.0
