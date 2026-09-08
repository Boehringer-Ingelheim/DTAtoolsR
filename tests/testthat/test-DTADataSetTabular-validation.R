test_that("DTADataSetTabular stores validation state per table", {
  ds <- create_example_DTADataSetTabular(2)

  ds <- check(ds, tables = "tab1", force = TRUE, persist = FALSE, quiet = TRUE)

  status <- validation_status(ds, tables = "tab1")
  expect_equal(nrow(status), 1)
  expect_equal(status$table, "tab1")
  # `status` records that a run happened, NOT that the data passed. The example
  # fixture deliberately carries 7 column spec errors, so `ok` is FALSE while
  # `status` is "validated" — assert both so the two can never be conflated.
  expect_equal(status$status, "validated")
  expect_false(status$ok)
  expect_equal(status$n_columnspec_errors, 7)
  expect_equal(status$n_rule_errors, 0)

  details <- validation_errors(ds, table = "tab1", source = "memory")
  expect_true(is.list(details))
  expect_true(all(c(
    "ok",
    "columnspec_valid",
    "rules_valid",
    "n_columnspec_errors",
    "n_rule_errors",
    "columnspec_errors",
    "rule_results",
    "rule_errors"
  ) %in% names(details)))

  result_tbl <- results(ds, tables = "tab1")
  expect_true(is.data.frame(result_tbl))
  expect_equal(nrow(result_tbl), 1)
  expect_equal(result_tbl$target, "tab1")
  # Was `if (status$ok) "validated" else "failed"`, which derived the expected
  # value from the object under test and therefore could never fail.
  expect_equal(result_tbl$status, "failed")
  expect_equal(result_tbl$dataset, "demographics")
  expect_equal(result_tbl$n_targets, 1)
  expect_equal(result_tbl$n_validated, 1)
})


test_that("validation_errors() output coerces to a data frame", {
  # Previously the returned list mixed a 5-row summary with a 7-row full error
  # table inside `columnspec_errors`, so as.data.frame() died with
  # "arguments imply differing number of rows: 5, 7".
  ds <- check(
    create_example_DTADataSetTabular(2),
    tables = "tab1",
    persist = FALSE,
    quiet = TRUE
  )
  details <- validation_errors(ds, table = "tab1", source = "memory")

  errors_df <- as.data.frame(details)
  expect_s3_class(errors_df, "data.frame")
  expect_equal(nrow(errors_df), details$n_columnspec_errors)
  expect_true(all(
    c("source", "rule_id", "row", "column", "keyword", "message") %in%
      names(errors_df)
  ))
  expect_true(all(errors_df$source == "columnspec"))
  expect_false(any(is.na(errors_df$message)))

  # The list interface callers already rely on is untouched.
  expect_true(is.list(details))
  expect_true(all(c(
    "ok",
    "columnspec_valid",
    "rules_valid",
    "n_columnspec_errors",
    "n_rule_errors",
    "columnspec_errors",
    "rule_results",
    "rule_errors"
  ) %in% names(details)))
  expect_equal(nrow(as.data.frame(details$columnspec_errors$full_error)), 7)
})

test_that("validation_errors() data frame carries rule failures alongside column spec ones", {
  ds <- create_example_DTADataSetTabular(2)
  ds@specs@rules <- list(create_example_DTARuleColUnique())

  bad_tab <- as.data.frame(ds@tables[["tab1"]])
  bad_tab$SUBJID[3] <- bad_tab$SUBJID[1]
  ds@tables[["tab1"]] <- arrow::arrow_table(bad_tab)

  ds <- check(ds, tables = "tab1", force = TRUE, persist = FALSE, quiet = TRUE)
  details <- validation_errors(ds, table = "tab1", source = "memory")

  errors_df <- as.data.frame(details)
  expect_equal(
    nrow(errors_df),
    details$n_columnspec_errors + details$n_rule_errors
  )

  rule_rows <- errors_df[errors_df$source == "rule", ]
  expect_equal(nrow(rule_rows), 1)
  expect_equal(rule_rows$rule_id, "rule_unique1")
  expect_match(rule_rows$message, "violated")
})


test_that("DTADataSetTabular can skip unchanged table validation", {
  ds <- create_example_DTADataSetTabular(2)

  ds <- check(ds, tables = "tab1", force = TRUE, persist = FALSE, quiet = TRUE)
  first_summary <- attr(ds, "last_validation_summary")

  ds <- check(ds, tables = "tab1", force = FALSE, persist = FALSE, quiet = TRUE)
  second_summary <- attr(ds, "last_validation_summary")

  expect_equal(first_summary$status[[1]], "validated")
  expect_equal(second_summary$status[[1]], "skipped")
})


test_that("DTADataSetTabular can persist and reload validation artifacts", {
  ds <- create_example_DTADataSetTabular(2)
  artifact_dir <- file.path(tempdir(), "dtatools-validation-artifacts-test")
  unlink(artifact_dir, recursive = TRUE, force = TRUE)

  ds <- check(
    ds,
    tables = "tab1",
    force = TRUE,
    persist = TRUE,
    artifact_dir = artifact_dir,
    quiet = TRUE
  )

  index_entry <- ds@validation_index[["tab1"]]
  expect_true(file.exists(index_entry$artifact_path))

  # `readRDS` of a list is trivially a list, so is.list() proved nothing about
  # the artifact. Compare the round-tripped artifact against memory instead --
  # that is what catches a stale or corrupt file.
  expect_identical(
    validation_errors(ds, table = "tab1", source = "artifact"),
    validation_errors(ds, table = "tab1", source = "memory")
  )

  artifact_path <- index_entry$artifact_path
  ds <- clear_validation(ds, tables = "tab1", remove_artifacts = TRUE)
  expect_true(is.null(ds@validation_index[["tab1"]]))
  expect_true(is.null(ds@validation_store[["tab1"]]))
  # remove_artifacts = TRUE must also delete the file it wrote; without this
  # the branch could be deleted and the suite would stay green.
  expect_false(file.exists(artifact_path))
})

test_that("check() validates a single table by name", {
  ds <- create_example_DTADataSetTabular(2)

  ds <- check(ds, tab = "tab1", quiet = TRUE)
  result <- attr(ds, "last_validation_details")

  expect_true(is.list(result))
  expect_true(all(c("ok", "columnspec_valid", "rules_valid") %in% names(result)))

  status <- validation_status(ds, tables = "tab1")
  expect_equal(nrow(status), 1)
  expect_equal(status$status, "validated")
})

test_that("check() validates a single table by index", {
  ds <- create_example_DTADataSetTabular(2)

  ds <- check(ds, tab = 1, quiet = TRUE)
  result <- attr(ds, "last_validation_details")

  expect_true(is.list(result))
  status <- validation_status(ds, tables = 1)
  expect_equal(nrow(status), 1)
  expect_equal(status$status, "validated")
})

test_that("check() skips unchanged table validation", {
  ds <- create_example_DTADataSetTabular(2)

  # First validation
  ds <- check(ds, tab = "tab1", force = TRUE, quiet = TRUE)
  first_status <- validation_status(ds, tables = "tab1")
  first_time <- first_status$validated_at

  Sys.sleep(0.1)

  # Second validation without force (should skip)
  ds <- check(ds, tab = "tab1", force = FALSE, quiet = TRUE)

  # Since skipped, result should still be from cache
  status <- validation_status(ds, tables = "tab1")
  # First validation should still be the same time
  expect_equal(first_time, status$validated_at)
})

test_that("check() forces re-validation when force=TRUE", {
  ds <- create_example_DTADataSetTabular(2)

  # First validation
  ds <- check(ds, tab = "tab1", force = TRUE, quiet = TRUE)
  first_status <- validation_status(ds, tables = "tab1")
  first_time <- first_status$validated_at

  Sys.sleep(0.1)

  # Second validation with force
  ds <- check(ds, tab = "tab1", force = TRUE, quiet = TRUE)
  second_status <- validation_status(ds, tables = "tab1")
  second_time <- second_status$validated_at

  # Times should be different (re-validated)
  expect_gt(second_time, first_time)
})

test_that("check() with neither `tab` nor `tables` validates every table", {
  # Was named "check() aborts ...", asserted the opposite of its own title, and
  # the only assertion was expect_true(TRUE) — it passed no matter what check()
  # returned. Assert the real all-tables contract instead.
  ds <- create_example_DTADataSetTabular(2)
  ds@tables[["tab2"]] <- ds@tables[["tab1"]]

  ds <- check(ds, persist = FALSE, quiet = TRUE)

  status <- validation_status(ds)
  expect_setequal(status$table, c("tab1", "tab2"))
  expect_true(all(status$status == "validated"))
  expect_equal(nrow(status), 2)
})

test_that("check() rejects `tab` and `tables` given together", {
  ds <- create_example_DTADataSetTabular(2)
  expect_error(check(ds, tab = "tab1", tables = "tab1"), "Cannot specify both")
})

test_that("check() aborts on non-existent table", {
  ds <- create_example_DTADataSetTabular(2)
  expect_error(check(ds, tab = "nonexistent"), "not found")
})

test_that("invalidate_by_spec_change() marks validation as outdated", {
  ds <- create_example_DTADataSetTabular(2)

  # Validate table
  ds <- check(ds, tab = "tab1", force = TRUE, quiet = TRUE)
  specs_hash_before <- ds@validation_index[["tab1"]]$specs_hash
  expect_false(is.null(specs_hash_before))

  # Invalidate by spec change
  ds <- invalidate_by_spec_change(ds, tables = "tab1")

  # `NULL$specs_hash` is also NULL, so asserting only that the hash is gone
  # would still pass if the whole index entry had been dropped. Pin both.
  entry <- ds@validation_index[["tab1"]]
  expect_false(is.null(entry))
  expect_true(is.null(entry$specs_hash))
  expect_false(is.null(entry$table_hash))

  # The documented point of invalidating is that the next check() re-runs
  # rather than reporting "skipped".
  ds <- check(ds, tab = "tab1", force = FALSE, persist = FALSE, quiet = TRUE)
  expect_equal(attr(ds, "last_validation_summary")$status[[1]], "validated")
})

test_that("invalidate_by_spec_change() invalidates all tables when no tables specified", {
  ds <- create_example_DTADataSetTabular(2)
  # create_example_DTADataSetTabular(2) only ships a single table ("tab1");
  # add a second table (same schema) so both tables can be invalidated here.
  ds@tables[["tab2"]] <- ds@tables[["tab1"]]

  # Validate both tables
  ds <- check(ds, tab = "tab1", force = TRUE, quiet = TRUE)
  ds <- check(ds, tab = "tab2", force = TRUE, quiet = TRUE)

  # Invalidate all tables
  ds <- invalidate_by_spec_change(ds)

  # Both specs_hash should be NULL
  expect_true(is.null(ds@validation_index[["tab1"]]$specs_hash))
  expect_true(is.null(ds@validation_index[["tab2"]]$specs_hash))
})

test_that("messages() returns flattened rule failures", {
  ds <- create_example_DTADataSetTabular(2)

  bad_rule <- create_example_DTARuleColUnique()
  ds@specs@rules <- list(bad_rule)

  # Duplicate SUBJID to violate uniqueness rule.
  bad_tab <- as.data.frame(ds@tables[["tab1"]])
  bad_tab$SUBJID[3] <- bad_tab$SUBJID[1]
  ds@tables[["tab1"]] <- arrow::arrow_table(bad_tab)

  ds <- check(ds, tab = "tab1", force = TRUE, persist = FALSE, quiet = TRUE)
  msgs <- messages(ds, tables = "tab1", as_tibble = FALSE)

  # A duplicated SUBJID was injected at row 3, so the unique rule must fire
  # exactly once. `nrow(msgs) >= 1` would also pass if the rule never ran and
  # only pre-existing column spec errors were reported.
  expect_s3_class(msgs, "data.frame")
  rule_msgs <- msgs[msgs$source == "rule", ]
  expect_equal(nrow(rule_msgs), 1)
  expect_match(rule_msgs$message, "violated")
  expect_true(all(unique(msgs$source) %in% c("columnspec", "rule")))
})

test_that("manually added table can be validated without errors", {
  ds <- create_example_DTADataSetTabular(2)

  manual_df <- as.data.frame(ds@tables[["tab1"]])
  manual_df$SUBJID <- paste0(manual_df$SUBJID, "_MANUAL")
  ds@tables[["manual_tab"]] <- arrow::arrow_table(manual_df)

  ds <- check(ds, tables = "manual_tab", force = TRUE, persist = FALSE, quiet = TRUE)

  status <- validation_status(ds, tables = "manual_tab")
  expect_equal(nrow(status), 1)
  expect_equal(status$table, "manual_tab")
  expect_equal(status$status, "validated")

  # The old test was named "without errors" and used expect_no_error() as its
  # only assertion, which said nothing about what validation actually found.
  # The manual table copies tab1 (7 column spec errors) and suffixes SUBJID with
  # "_MANUAL", which adds 2 more SUBJID violations -- 9 in total.
  expect_false(status$ok)
  expect_equal(status$n_columnspec_errors, 9)
  expect_equal(status$n_rule_errors, 0)

  msgs <- messages(ds, tables = "manual_tab", as_tibble = FALSE)
  expect_s3_class(msgs, "data.frame")
  expect_equal(nrow(msgs), 9)

  info <- inspect(ds, as_tibble = FALSE)
  expect_s3_class(info, "data.frame")
  expect_equal(nrow(info), nrow(messages(ds, as_tibble = FALSE)))
})

test_that("multiple manually added tables can be validated without errors", {
  ds <- create_example_DTADataSetTabular(2)

  base_df <- as.data.frame(ds@tables[["tab1"]])
  ds@tables[["manual_tab_a"]] <- arrow::arrow_table(base_df)

  base_df_b <- base_df
  base_df_b$VISIT <- as.character(base_df_b$VISIT)
  ds@tables[["manual_tab_b"]] <- arrow::arrow_table(base_df_b)

  expect_no_error({
    ds <- check(
      ds,
      tables = c("tab1", "manual_tab_a", "manual_tab_b"),
      force = TRUE,
      persist = FALSE,
      quiet = TRUE
    )
  })

  status <- validation_status(ds, tables = c("tab1", "manual_tab_a", "manual_tab_b"))
  expect_equal(nrow(status), 3)
  expect_true(all(status$status == "validated"))

  expect_no_error({
    res <- results(ds, tables = c("tab1", "manual_tab_a", "manual_tab_b"))
    expect_equal(nrow(res), 3)
  })
})

# ---------------------------------------------------------------------------
# A table checked against zero column specs is "unspecified", not a pass
# ---------------------------------------------------------------------------

test_that("a table checked against zero column specs is 'unspecified', not a pass", {
  # Before the fix, a DTAColumnSpecCollection with no columns validated every
  # table against it as a clean PASS -- a "VALIDATION PASSED" certificate
  # covering zero actual checks. `ok = NA` (never FALSE) is deliberate: n_valid
  # counts `ok == TRUE` and n_invalid counts `ok == FALSE`, both with
  # na.rm = TRUE, so an NA row is skipped by BOTH tallies rather than counted
  # as either a pass or a fail -- which is what makes the dataset read as
  # incomplete instead of either verdict.
  ds <- DTADataSetTabular(
    name = "d",
    specs = specs_from_list(NULL),
    files = list(DTAFileCSV(filename = "clinical_data.csv"))
  )
  ds <- load_file(
    ds,
    file = system.file("extdata", "clinical_data.csv", package = "DTAtools"),
    handler_index = 1
  )
  ds <- check(ds, quiet = TRUE, persist = FALSE)

  status <- validation_status(ds)
  expect_equal(status$status, "unspecified")
  expect_true(is.na(status$ok))

  res <- results(ds)
  expect_equal(res$n_valid, 0)
  expect_equal(res$n_invalid, 0)
})


# ---------------------------------------------------------------------------
# Replacing a bound table drops that table's verdict
# ---------------------------------------------------------------------------

# One character column of at most four characters: "A001" passes, "TOO-LONG"
# fails, and nothing else about the file matters.
tv_id_specs <- function() {
  DTAColumnSpecCollection(columns = list(
    ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE)
  ))
}

# Console output of one expression, whitespace-normalised. cli wraps to the
# terminal width, so a message asserted on raw output can break in the middle
# of the phrase being matched.
tv_console <- function(expr) {
  gsub("[[:space:]]+", " ", paste(testthat::capture_messages(expr), collapse = " "))
}

test_that("loading a different file under a bound table name drops the old verdict", {
  # Only @tables was replaced, so the validation index, the store and every
  # report built from them kept describing the file that had just been thrown
  # away: validation_status() certified data it had never seen, under the name
  # of data that was no longer there.
  dir <- withr::local_tempdir()
  good <- file.path(dir, "t.csv")
  bad <- file.path(dir, "t2.csv")
  writeLines(c("ID", "A001"), good)
  writeLines(c("ID", "TOO-LONG"), bad)

  ds <- DTADataSetTabular(
    name = "d",
    specs = tv_id_specs(),
    files = list(DTAFileCSV(filename = "t.csv"), DTAFileCSV(filename = "t2.csv"))
  )

  ds <- load_file(ds, file = good, handler_index = 1, name = "t")
  ds <- check(ds, persist = FALSE, quiet = TRUE)
  expect_true(validation_status(ds)$ok)

  ds <- load_file(ds, file = bad, handler_index = 2, name = "t")

  status <- validation_status(ds)
  expect_equal(status$status, "not_validated")
  expect_true(is.na(status$ok))
  expect_null(ds@validation_store[["t"]])
  expect_equal(nrow(messages(ds, as_tibble = FALSE)), 0)

  # ... and the next check() judges the file that is actually bound.
  ds <- check(ds, persist = FALSE, quiet = TRUE)
  expect_false(validation_status(ds)$ok)
})

test_that("re-loading the same file under a bound name also drops the old verdict", {
  # The state is dropped on REPLACEMENT, not on a change of file: a table
  # reloaded from the same path has not been validated in its new incarnation
  # either, and check() re-establishes the verdict from the hash as before.
  dir <- withr::local_tempdir()
  path <- file.path(dir, "t.csv")
  writeLines(c("ID", "A001"), path)

  ds <- DTADataSetTabular(
    name = "d", specs = tv_id_specs(),
    files = list(DTAFileCSV(filename = "t.csv"))
  )
  ds <- check(
    load_file(ds, file = path, handler_index = 1, name = "t"),
    persist = FALSE, quiet = TRUE
  )
  expect_true(validation_status(ds)$ok)

  ds <- load_file(ds, file = path, handler_index = 1, name = "t")
  expect_equal(validation_status(ds)$status, "not_validated")
})


# ---------------------------------------------------------------------------
# A tabular dataset with no tables reports, it does not abort
# ---------------------------------------------------------------------------

test_that("a tabular dataset with no tables reports rather than aborting", {
  # A specification whose data has not been delivered yet is the ordinary
  # state of a DTA, not an error. Every report used to abort with "No tables
  # found in dataset." -- and at DTA level that took down check(), results()
  # and messages() for every OTHER dataset alongside it.
  ds <- create_example_DTADataSetTabular(1)
  expect_length(tables(ds), 0)

  status <- validation_status(ds)
  expect_true(is.data.frame(status))
  expect_equal(nrow(status), 0)
  expect_true(all(
    c("table", "target_type", "status", "ok", "n_rule_errors") %in% names(status)
  ))

  expect_equal(nrow(results(ds)), 0)
  expect_equal(nrow(messages(ds, as_tibble = FALSE)), 0)

  ds <- check(ds, persist = FALSE, quiet = TRUE)
  expect_s3_class(ds, "DTAtools::DTADataSetTabular")
  expect_equal(nrow(attr(ds, "last_validation_summary")), 0)
})

test_that("check() says out loud that a dataset had no tables to check", {
  out <- tv_console(check(create_example_DTADataSetTabular(1), persist = FALSE))

  expect_match(out, "no tables loaded", fixed = TRUE)
  expect_false(grepl("passed validation", out, fixed = TRUE))
})

test_that("tables that carry no names are still a hard error", {
  # "No names" and "no tables" are different situations and only the second is
  # benign: an unnamed table cannot be addressed by any report, so answering
  # with an empty selection would quietly leave its data out of all of them.
  ds <- create_example_DTADataSetTabular(2)
  names(ds@tables) <- NULL

  expect_error(validation_status(ds), "have no names")
  expect_error(check(ds, persist = FALSE, quiet = TRUE), "have no names")
})

test_that("an explicit table selection on a dataset with no tables still aborts", {
  # The empty answer is for "check everything"; asking for a table by name or
  # index is a mistake about the dataset, and answering it with nothing would
  # silently drop the caller's request.
  ds <- create_example_DTADataSetTabular(1)

  expect_error(check(ds, tables = "nope", quiet = TRUE), "not found")
  expect_error(validation_status(ds, tables = 1), "out of bounds")
  expect_error(validation_errors(ds, table = "nope"), "not found")
})


# ---------------------------------------------------------------------------
# A lazy table whose file has vanished
# ---------------------------------------------------------------------------

test_that("check() reports a lazily held table whose file has vanished", {
  # A lazy table is a scan plan over files. Scanning one whose file was
  # deleted raised an Arrow IOError from inside the scanner and took the whole
  # check() down, so a single cleaned-up delivery destroyed the verdicts of
  # every other table in the dataset.
  dir <- withr::local_tempdir()
  path <- file.path(dir, "vanish.csv")
  writeLines(c("ID", "A001"), path)

  ds <- DTADataSetTabular(
    name = "d", specs = tv_id_specs(),
    files = list(DTAFileCSV(filename = "vanish.csv"))
  )
  ds <- load_file(ds, file = path, handler_index = 1, stream = "always")
  unlink(path)

  ds <- check(ds, persist = FALSE, quiet = TRUE)

  status <- validation_status(ds)
  expect_equal(status$status, "validated")
  expect_false(status$ok)
  expect_equal(status$n_rule_errors, 1)
  expect_equal(status$n_columnspec_errors, 0)

  msgs <- messages(ds, as_tibble = FALSE)
  expect_equal(nrow(msgs), 1)
  expect_equal(msgs$rule_id, "file_presence")
  expect_match(msgs$message, "not found", fixed = TRUE)

  # The change signal of a Dataset whose file is gone is stable, so a repeat
  # check() skips rather than re-reporting the same absence as a fresh finding.
  ds <- check(ds, persist = FALSE, quiet = TRUE)
  expect_equal(attr(ds, "last_validation_summary")$status, "skipped")
})

test_that("a vanished delivery is reported by its own path, not by its copy", {
  # A non-UTF-8 delivery is scanned through a UTF-8 copy under tempdir(). Read
  # from `$files`, the absence was reported by a path the user never chose and
  # cannot act on.
  dir <- withr::local_tempdir()
  path <- file.path(dir, "vanish_latin1.csv")
  con <- file(path, "wb")
  writeBin(c(charToRaw("ID\n"), as.raw(c(0x41, 0xfc, 0x31)), charToRaw("\n")), con)
  close(con)

  ds <- DTADataSetTabular(
    name = "d", specs = tv_id_specs(),
    files = list(DTAFileCSV(filename = "vanish_latin1.csv", encoding = "latin1"))
  )
  ds <- load_file(ds, file = path, handler_index = 1, stream = "always")

  table <- tables(ds)[["vanish_latin1"]]
  copy <- normalizePath(table$files[[1]], winslash = "/")
  delivery <- dta_dataset_source_files(table)

  # The premise: the scan really is reading something else, and the dataset is
  # identified by the delivery all the same.
  expect_false(identical(copy, delivery))
  expect_identical(basename(delivery), "vanish_latin1.csv")

  # A copy that has gone is the session's own housekeeping, not a delivery
  # failure: the delivery is still there and is simply converted again.
  unlink(copy)
  expect_identical(dta_missing_table_files(table), character(0))

  unlink(path)
  expect_identical(dta_missing_table_files(table), delivery)

  ds <- check(ds, persist = FALSE, quiet = TRUE)
  msgs <- messages(ds, as_tibble = FALSE)
  expect_equal(msgs$rule_id, "file_presence")
  expect_match(msgs$message, basename(path), fixed = TRUE)
  # Named by the delivery, and never by the temporary it was copied into.
  expect_false(grepl(basename(copy), msgs$message, fixed = TRUE))
})


# ---------------------------------------------------------------------------
# @import_issues records the same axis however the table was held
# ---------------------------------------------------------------------------

test_that("check() records the same import issues eager and streamed", {
  # @import_issues is the axis a materialising load records at import: values
  # the DECLARED TYPE could not represent. A streamed table has no such moment,
  # so check() writes the streaming result's typing frame -- and where it took
  # the MERGED frame instead, the same file showed one set of import issues
  # loaded eagerly and another streamed.
  dir <- withr::local_tempdir()

  both_paths <- function(name, lines, specs) {
    path <- file.path(dir, name)
    writeLines(lines, path)
    lapply(c(never = "never", always = "always"), function(stream) {
      ds <- DTADataSetTabular(
        name = "d", specs = specs, files = list(DTAFileCSV(filename = name))
      )
      ds <- load_file(ds, file = path, handler_index = 1, stream = stream)
      ds <- check(ds, persist = FALSE, quiet = TRUE)
      ds@import_issues[[tools::file_path_sans_ext(name)]]
    })
  }

  # Rule-time only: a Char column a range rule compares numerically. Nothing
  # failed to TYPE, so neither path records an import issue.
  rule_only <- both_paths(
    "rule_only.csv",
    c("ID,TXT", "A001,xyz", "A002,xyz"),
    DTAColumnSpecCollection(
      columns = list(
        ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE),
        TXT = DTAColumnSpec(id = "TXT", type = "SAS Char", length = 8, nullable = TRUE)
      ),
      rules = list(DTARuleColRange(id = "textrange", columns = "TXT", min = 0, max = 100))
    )
  )
  expect_null(rule_only$never)
  expect_null(rule_only$always)

  # Typing only: one cell a declared Num column cannot hold. One row on both.
  typing <- both_paths(
    "typing_only.csv",
    c("ID,VAL", "A001,1", "A002,zzz"),
    DTAColumnSpecCollection(
      columns = list(
        ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE),
        VAL = DTAColumnSpec(id = "VAL", type = "SAS Num", nullable = TRUE)
      )
    )
  )
  expect_equal(nrow(typing$never), 1L)
  expect_equal(nrow(typing$always), 1L)
  for (field in c("row", "column", "raw", "declared_type", "reason")) {
    expect_identical(typing$always[[field]], typing$never[[field]], info = field)
  }
})


# ---------------------------------------------------------------------------
# Braces in a table name are data, not cli syntax
# ---------------------------------------------------------------------------

test_that("a table whose name contains braces checks without aborting", {
  # cli parses `{...}` in the string it is handed, so a table called `a{b}` --
  # the default name for a delivered `a{b}.csv` -- aborted every non-quiet
  # check() with "Could not evaluate cli `{}` expression".
  dir <- withr::local_tempdir()
  path <- file.path(dir, "a{b}.csv")
  writeLines(c("ID", "A001"), path)

  ds <- DTADataSetTabular(
    name = "d", specs = tv_id_specs(),
    files = list(DTAFileCSV(filename = "a{b}.csv"))
  )
  ds <- load_file(ds, file = path, handler_index = 1)
  expect_named(tables(ds), "a{b}")

  out <- tv_console(ds <- check(ds, persist = FALSE, quiet = FALSE))

  expect_match(out, "a{b}", fixed = TRUE)
  expect_true(validation_status(ds)$ok)
})


test_that("print() survives braces in the dataset name and in a table name", {
  # print() pasted every table name into `{.field <name>}` markup and handed
  # the result to cli, which then tried to evaluate `b` as an R expression:
  # printing an object built from a delivered `a{b}.csv` aborted. The dataset
  # name goes through the same path and is asserted alongside it.
  ds <- DTADataSetTabular(
    name = "d{x}",
    specs = tv_id_specs(),
    tables = list(`a{b}` = data.frame(ID = "A001", stringsAsFactors = FALSE))
  )

  out <- tv_console(expect_invisible(print(ds)))

  expect_match(out, "d{x}", fixed = TRUE)
  expect_match(out, "Tables (1): a{b}", fixed = TRUE)

  # The count-only method in this file must survive it too.
  short <- tv_console(expect_invisible(print_short_info(ds)))
  expect_match(short, "Tables: (1)", fixed = TRUE)
})


test_that("print() lists several table names comma-separated and truncates past five", {
  # The interpolation had to be given cli_vec() separators to keep printing
  # "a, b, c"; cli's own default would have written "a, b and c", and the
  # elision marker of the >5 case would have been collapsed with "and" too.
  frame <- function() data.frame(ID = "A001", stringsAsFactors = FALSE)
  named <- function(names) {
    stats::setNames(lapply(names, function(ignored) frame()), names)
  }

  three <- DTADataSetTabular(
    name = "d", specs = tv_id_specs(), tables = named(c("t1", "t2", "t3"))
  )
  expect_match(
    tv_console(print(three)), "Tables (3): t1, t2, t3",
    fixed = TRUE
  )

  many <- DTADataSetTabular(
    name = "d", specs = tv_id_specs(), tables = named(sprintf("t%d", 1:7))
  )
  expect_match(
    tv_console(print(many)), "Tables (7): t1, t2, t3, t4, ..., t7",
    fixed = TRUE
  )
})


# ---------------------------------------------------------------------------
# A vanished artifact directory does not freeze the object
# ---------------------------------------------------------------------------

test_that("a dataset whose artifact directory has vanished can still be modified", {
  # The property remembers WHERE artifacts were written, and that directory is
  # temporary by default. Requiring it to exist made S7's revalidation abort
  # every later property assignment, so the object could no longer be cleared,
  # loaded into, or even checked with persist = FALSE.
  artifact_dir <- file.path(withr::local_tempdir(), "artifacts")

  ds <- check(
    create_example_DTADataSetTabular(2),
    persist = TRUE, artifact_dir = artifact_dir, quiet = TRUE
  )
  expect_equal(ds@validation_artifact_dir, artifact_dir)

  unlink(artifact_dir, recursive = TRUE)
  expect_false(dir.exists(artifact_dir))

  ds <- clear_validation(ds)
  expect_length(ds@validation_index, 0)
  ds <- check(ds, persist = FALSE, quiet = TRUE)
  expect_equal(nrow(validation_status(ds)), 1)

  # What the validator still rejects is a value that is not a single path.
  expect_error(
    {
      ds@validation_artifact_dir <- c("a", "b")
    },
    "single directory path"
  )
})


# ---------------------------------------------------------------------------
# A table constructed from a data frame is stamped, not re-hashed
# ---------------------------------------------------------------------------

# The constructor used to convert a data frame to Arrow, hand THAT to the
# coercion (which turned it straight back into a data frame), and rebuild a
# Table from the result: three conversions of the whole table, where the only
# thing the detour bought was the content stamp the coercion applies to an
# Arrow input. The frame is now coerced as a frame and converted once, and the
# stamp is applied in the constructor from the same digest. These tests pin
# what that stamp has to be worth for the change to be an improvement rather
# than a shortcut.

tv_stamp_specs <- function() {
  DTAColumnSpecCollection(
    columns = list(
      ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE),
      VAL = DTAColumnSpec(id = "VAL", type = "SAS Num", nullable = TRUE)
    ),
    rules = list(DTARuleColUnique(id = "uid", columns = "ID"))
  )
}

tv_summary <- function(x) attr(x, "last_validation_summary")

test_that("a table built from a data frame carries the digest check() would derive", {
  ds <- DTADataSetTabular(
    name = "d",
    specs = tv_stamp_specs(),
    tables = list(t = data.frame(
      ID = c("a", "b", "c"), VAL = c(1, 2, 3), stringsAsFactors = FALSE
    ))
  )

  stored <- ds@tables[["t"]]
  expect_s3_class(stored, "Table")
  expect_false(is.null(dta_table_hash_stamp(stored)))

  # The stamp is worse than no stamp unless it is the SAME digest the change
  # signal falls back to for an unstamped table: a rebuild of this table's own
  # contents would otherwise hash differently and be rescanned forever.
  expect_identical(
    dta_table_change_signal(stored),
    dta_table_content_hash(as.data.frame(stored))
  )

  ds <- check(ds, persist = FALSE, quiet = TRUE)
  expect_identical(tv_summary(ds)$status, "validated")
  expect_true(tv_summary(ds)$ok)

  # Nothing changed, so nothing is rescanned.
  ds <- check(ds, persist = FALSE, quiet = TRUE)
  expect_identical(tv_summary(ds)$status, "skipped")

  # And an unstamped rebuild from the stored table's own contents is
  # recognised rather than rescanned -- which is exactly what the digest
  # having to be the same one buys.
  rebuilt <- ds
  rebuilt@tables[["t"]] <- arrow::as_arrow_table(as.data.frame(ds@tables[["t"]]))
  expect_null(dta_table_hash_stamp(rebuilt@tables[["t"]]))
  rebuilt <- check(rebuilt, persist = FALSE, quiet = TRUE)
  expect_identical(tv_summary(rebuilt)$status, "skipped")
})

test_that("an import issue raised at construction is inside the stamp", {
  # The digest covers the issues attribute, so a table whose import issues
  # changed can never be skipped with a stale verdict.
  build <- function(vals) {
    DTADataSetTabular(
      name = "d",
      specs = tv_stamp_specs(),
      tables = list(t = data.frame(
        ID = c("a", "b"), VAL = vals, stringsAsFactors = FALSE
      ))
    )
  }

  clean <- build(c("1", "2"))
  dirty <- build(c("1", "nope"))

  expect_length(clean@import_issues, 0)
  expect_equal(nrow(dirty@import_issues[["t"]]), 1)
  expect_false(identical(
    dta_table_change_signal(clean@tables[["t"]]),
    dta_table_change_signal(dirty@tables[["t"]])
  ))

  # Same data, same issues, built twice: one digest, so a rebuilt object is
  # not rescanned merely for having been rebuilt.
  expect_identical(
    dta_table_change_signal(dirty@tables[["t"]]),
    dta_table_change_signal(build(c("1", "nope"))@tables[["t"]])
  )
})

# The two frames below carry a column of a type the predicate rejects
# alongside the declared ones. VAL decides which branch of the coercion runs:
# given as text it has to be typed, so the coercion rebuilds the table and the
# stamp is taken from the round-tripped frame; given as numbers nothing needs
# typing, the coercion hands back the Table it was given, and the stamp is the
# digest of that Table's own as.data.frame(). Both branches have to agree with
# dta_table_change_signal(), so both are exercised.
tv_unstable_frames <- function(val) {
  frames <- list(
    difftime = data.frame(
      ID = c("a", "b"), VAL = val,
      D = as.difftime(c(1, 2), units = "hours"),
      stringsAsFactors = FALSE
    )
  )
  if (requireNamespace("bit64", quietly = TRUE)) {
    frames$integer64 <- data.frame(
      ID = c("a", "b"), VAL = val,
      B = bit64::as.integer64(c(1, 2)),
      stringsAsFactors = FALSE
    )
  }
  frames
}

test_that("a frame Arrow does not return unchanged is stamped from the round trip", {
  # A `difftime` comes back from Arrow in seconds whatever units it went in
  # as, and a small `integer64` comes back as `integer`. A stamp taken from
  # the frame as written would then not be the digest of the Table that was
  # stored, so such a frame takes the original route and is stamped from the
  # round-tripped copy instead. The stamp still has to equal what the change
  # signal derives, which is what this asserts.
  for (val in list(c("1", "2"), c(1, 2))) {
    frames <- tv_unstable_frames(val)
    typed <- if (is.character(val)) "text VAL" else "numeric VAL"

    for (label in names(frames)) {
      info <- paste(label, typed)
      expect_false(dta_frame_is_arrow_stable(frames[[label]]), info = info)

      ds <- DTADataSetTabular(
        name = "d", specs = tv_stamp_specs(), tables = list(t = frames[[label]])
      )
      stored <- ds@tables[["t"]]

      expect_false(is.null(dta_table_hash_stamp(stored)), info = info)
      expect_identical(
        dta_table_change_signal(stored),
        dta_table_content_hash(as.data.frame(stored)),
        info = info
      )

      ds <- check(ds, persist = FALSE, quiet = TRUE)
      expect_identical(tv_summary(ds)$status, "validated", info = info)
      ds <- check(ds, persist = FALSE, quiet = TRUE)
      expect_identical(tv_summary(ds)$status, "skipped", info = info)
    }
  }
})

test_that("a POSIXct with no named timezone still checks, and still skips a re-check", {
  # KNOWN DEFECT, pinned rather than endorsed: `tzone = ""` -- what
  # as.POSIXct() leaves when no timezone is named -- is the one type that is
  # not stable under REPEATED Arrow round trips. It comes back from the first
  # with no tzone and from the second with the session's timezone, so when the
  # coercion rebuilds the table (which it does only when some column needs
  # typing) the stamp is the digest of the first round trip while a rebuild of
  # the stored table hashes the second. Such a table is rescanned every time it
  # is rebuilt from its own contents. It is only ever a cost, never a wrong
  # answer: the digest is still of real contents, so it can never claim
  # "unchanged" for data that changed, and the object's own stamp is stable, so
  # a re-check of the same object is still skipped. If Arrow ever round-trips
  # `tzone = ""` faithfully, the expect_false() below becomes an
  # expect_identical() of the two digests.
  frame <- function(val) {
    data.frame(
      ID = c("a", "b"), VAL = val,
      W = as.POSIXct(c("2026-01-01 10:00:00", "2026-01-02 11:00:00")),
      stringsAsFactors = FALSE
    )
  }
  expect_false(dta_frame_is_arrow_stable(frame(c(1, 2))))

  # Nothing to type: the coercion hands back the Table it was given, whose own
  # as.data.frame() IS what was hashed, so the digests agree after all.
  untyped <- DTADataSetTabular(
    name = "d", specs = tv_stamp_specs(), tables = list(t = frame(c(1, 2)))
  )@tables[["t"]]
  expect_identical(
    dta_table_change_signal(untyped),
    dta_table_content_hash(as.data.frame(untyped))
  )

  # VAL arrives as text and has to be typed, so the coercion rebuilds -- and
  # this is where the second round trip bites.
  ds <- DTADataSetTabular(
    name = "d", specs = tv_stamp_specs(), tables = list(t = frame(c("1", "2")))
  )
  stored <- ds@tables[["t"]]
  expect_false(identical(
    dta_table_change_signal(stored),
    dta_table_content_hash(as.data.frame(stored))
  ))

  # The verdict is unaffected, and the same object is still skipped.
  ds <- check(ds, persist = FALSE, quiet = TRUE)
  expect_identical(tv_summary(ds)$status, "validated")
  expect_identical(tv_summary(ds)$n_import_errors, 0L)
  ds <- check(ds, persist = FALSE, quiet = TRUE)
  expect_identical(tv_summary(ds)$status, "skipped")
})


# ---------------------------------------------------------------------------
# The retained-error cap on a table held in memory
# ---------------------------------------------------------------------------

test_that("max_errors bounds retained detail on an in-memory table, never the counts", {
  # The cap exists because retention is one row per bad cell, so an unbounded
  # error frame is a memory finding on a large dirty input. A table built in R
  # and checked straight away is the case it is least suited to -- it is
  # already in memory -- which is why check(), DTADataSetTabular() and
  # collect_full_errors() all say to pass `max_errors = Inf` there. This pins
  # both halves of that advice: the cap really does drop detail, and it really
  # does not touch the answer.
  ds <- DTADataSetTabular(
    name = "d",
    specs = tv_id_specs(),
    tables = list(t = data.frame(
      ID = rep("TOO-LONG", 25), stringsAsFactors = FALSE
    ))
  )

  outcome <- function(cap) {
    checked <- suppressWarnings(
      check(ds, persist = FALSE, quiet = TRUE, force = TRUE, max_errors = cap)
    )
    status <- validation_status(checked)
    details <- validation_errors(checked, table = "t", source = "memory")
    list(
      ok = status$ok,
      counted = status$n_columnspec_errors,
      rows = nrow(as.data.frame(details))
    )
  }

  capped <- outcome(3)
  uncapped <- outcome(Inf)

  # The verdict and the count are the same at either cap.
  expect_false(capped$ok)
  expect_false(uncapped$ok)
  expect_equal(capped$counted, 25)
  expect_equal(uncapped$counted, 25)

  # The retained detail is not.
  expect_equal(capped$rows, 3)
  expect_equal(uncapped$rows, 25)
})
