# The result accessors. REQ-VAL-022 .. REQ-VAL-026.
#
# These are the interface a caller builds a report or a downstream decision on,
# so their shape is part of the contract and not an implementation detail. A
# column that quietly disappears breaks every consumer at once, and a column
# that quietly changes meaning breaks them silently, which is worse.

va_specs <- function() {
  DTAColumnSpecCollection(
    columns = list(
      ID = DTAColumnSpec(
        id = "ID", type = "SAS Char", length = 4, nullable = FALSE,
        pattern = "^[A-Z][0-9]{3}$"
      ),
      SEX = DTAColumnSpec(
        id = "SEX", type = "SAS Char", length = 1, nullable = FALSE,
        values = c("M", "F")
      ),
      AGE = DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
    ),
    rules = list(
      DTARuleColRange(id = "age_range", columns = "AGE", range = c(18, 70))
    )
  )
}

# One dataset carrying a violation on each of the three axes, so every accessor
# has something of every kind to report.
va_dataset <- function(dir) {
  path <- file.path(dir, "accessors.csv")
  utils::write.csv(
    data.frame(
      ID = c("A001", "A002", "A003", "A004"),
      SEX = c("M", "X", "F", "M"),
      AGE = c("30", "40", "99", "abc"),
      stringsAsFactors = FALSE
    ),
    path,
    row.names = FALSE, na = ""
  )
  ds <- DTADataSetTabular(
    name = "acc", specs = va_specs(),
    files = list(DTAFileCSV(filename = "accessors.csv"))
  )
  ds <- load_file(ds, file = path, handler_index = 1, stream = "never")
  check(ds, persist = FALSE, quiet = TRUE)
}

test_that("OQ-VAL-030 | validation_status reports one row per table with its documented columns | REQ-VAL-022", {
  ds <- va_dataset(qa_tempdir())
  status <- validation_status(ds)

  qa_step(
    "the status frame carries exactly its documented columns",
    c(
      "table", "target_type", "status", "ok", "validated_at", "run_id",
      "validation_run", "n_columnspec_errors", "n_rule_errors",
      "n_import_errors"
    ),
    names(status)
  )
  qa_step("one row per table", 1L, nrow(status))
  qa_step("the target type says what kind of thing was checked", "table", status$target_type)
  qa_step(
    "and the three axis counts are the ones the dataset earned",
    c(columnspec = 1L, rule = 1L, import = 1L),
    c(
      columnspec = as.integer(status$n_columnspec_errors),
      rule = as.integer(status$n_rule_errors),
      import = as.integer(status$n_import_errors)
    )
  )
})

test_that("OQ-VAL-031 | messages reports one row per finding, tagged with its axis | REQ-VAL-023", {
  ds <- va_dataset(qa_tempdir())
  msgs <- as.data.frame(messages(ds))

  qa_step(
    "the message frame carries exactly its documented columns",
    c(
      "id", "dataset", "target", "severity", "source", "rule_id", "row",
      "column", "keyword", "message"
    ),
    names(msgs)
  )

  # The axis label is what lets a reader tell a broken value from a broken
  # relationship between values, which are different problems for whoever has
  # to fix the delivery.
  qa_step(
    "every finding is attributed to one of the three axes",
    c("columnspec", "import", "rule"),
    sort(unique(as.character(msgs$source)))
  )
  qa_step(
    "identifiers are sequential from one, so a reader can cite them",
    seq_len(nrow(msgs)), as.integer(msgs$id)
  )
  qa_step(
    "a rule finding names its rule and a column finding names its column",
    list(rule = "age_range", column = "SEX"),
    list(
      rule = as.character(msgs$rule_id[msgs$source == "rule"][[1]]),
      column = as.character(msgs$column[msgs$source == "columnspec"][[1]])
    )
  )
  qa_step(
    "every finding carries a non-empty explanation",
    TRUE, all(nzchar(as.character(msgs$message)))
  )
})

test_that("OQ-VAL-032 | results summarises every dataset of a DTA | REQ-VAL-024", {
  ds <- va_dataset(qa_tempdir())
  dta <- DTA(datasets = list(acc = ds), metadata = create_example_DTAMetaData())
  res <- as.data.frame(results(dta))

  qa_step(
    "the summary carries the per-target columns and the totals",
    c(
      "dataset", "target", "target_type", "status", "validated_at", "run_id",
      "validation_run", "n_columnspec_errors", "n_rule_errors",
      "n_import_errors", "n_targets", "n_validated", "n_valid", "n_invalid",
      "n_not_validated", "n_skipped"
    ),
    intersect(
      c(
        "dataset", "target", "target_type", "status", "validated_at", "run_id",
        "validation_run", "n_columnspec_errors", "n_rule_errors",
        "n_import_errors", "n_targets", "n_validated", "n_valid", "n_invalid",
        "n_not_validated", "n_skipped"
      ),
      names(res)
    )
  )
  qa_step(
    "one invalid target is counted as validated, not valid, and not skipped",
    c(targets = 1L, validated = 1L, valid = 0L, invalid = 1L),
    c(
      targets = as.integer(res$n_targets[[1]]),
      validated = as.integer(res$n_validated[[1]]),
      valid = as.integer(res$n_valid[[1]]),
      invalid = as.integer(res$n_invalid[[1]])
    )
  )
})

test_that("OQ-VAL-033 | inspect expands a message into the detail behind it | REQ-VAL-025", {
  ds <- va_dataset(qa_tempdir())
  msgs <- as.data.frame(messages(ds))
  target <- msgs$id[msgs$source == "columnspec"][[1]]
  detail <- as.data.frame(inspect(ds, target))

  qa_step("the requested message is the one expanded", as.integer(target), as.integer(detail$id[[1]]))
  qa_step("and it keeps its axis", "columnspec", as.character(detail$source[[1]]))

  # The offending value is the thing a supplier needs in order to fix the
  # delivery. A report that says only "row 2 is wrong" costs another round trip.
  qa_check(
    "the offending value is carried through",
    any(vapply(detail, function(col) any(as.character(col) %in% "X"), logical(1)))
  )

  # An identifier nobody issued must be an error, not an empty frame that would
  # read as "there was nothing wrong with it".
  err <- tryCatch(inspect(ds, 9999L), error = function(e) e)
  qa_check("an unknown message identifier raises a condition", inherits(err, "condition"))
})

test_that("OQ-VAL-034 | validation_errors carries the full detail and its scan record | REQ-VAL-026", {
  ds <- va_dataset(qa_tempdir())
  details <- validation_errors(ds, names(tables(ds))[[1]])

  qa_step(
    "the detail names each axis verdict and each axis count",
    c(
      "ok", "columnspec_valid", "rules_valid", "import_valid",
      "n_columnspec_errors", "n_rule_errors", "n_import_errors"
    ),
    intersect(
      c(
        "ok", "columnspec_valid", "rules_valid", "import_valid",
        "n_columnspec_errors", "n_rule_errors", "n_import_errors"
      ),
      names(details)
    )
  )
  qa_step(
    "the result carries its version, so a consumer can tell shapes apart",
    2L, as.integer(details$result_version)
  )

  # How much of the table was actually looked at is part of the verdict's
  # meaning: a partial scan that reported no errors has not said the table is
  # clean. The flags are absent rather than FALSE on a complete run, so a
  # caller has to read them as "absent means not partial" (LIM-003).
  qa_step(
    "a complete scan is not flagged as partial or structural-only",
    c(partial = FALSE, structural_only = FALSE),
    c(
      partial = isTRUE(attr(details, "partial_scan")),
      structural_only = isTRUE(attr(details, "structural_only"))
    )
  )

  # The row count is recorded by the streaming engine and by nothing else, so
  # the same question has an answer or no answer depending only on how the
  # table was loaded. Pinned as observed; see LIM-003.
  eager_rows <- attr(details, "n_rows_scanned")
  qa_step(
    "the whole-column engine records no scanned-row count",
    TRUE, is.null(eager_rows)
  )

  dir <- qa_tempdir()
  path <- file.path(dir, "scanned.csv")
  utils::write.csv(
    data.frame(
      ID = c("A001", "A002", "A003", "A004"), SEX = c("M", "F", "M", "F"),
      AGE = c("30", "40", "50", "60"), stringsAsFactors = FALSE
    ),
    path,
    row.names = FALSE, na = ""
  )
  streamed <- DTADataSetTabular(
    name = "acc", specs = va_specs(),
    files = list(DTAFileCSV(filename = "scanned.csv"))
  )
  streamed <- check(
    load_file(streamed, file = path, handler_index = 1, stream = "always"),
    persist = FALSE, quiet = TRUE
  )
  qa_step(
    "while the streaming engine records every row it read",
    4L,
    as.integer(attr(
      validation_errors(streamed, names(tables(streamed))[[1]]), "n_rows_scanned"
    ))
  )
})

test_that("OQ-VAL-035 | the accessors agree with one another | REQ-VAL-022 REQ-VAL-023 REQ-VAL-024", {
  ds <- va_dataset(qa_tempdir())
  status <- validation_status(ds)
  msgs <- as.data.frame(messages(ds))
  dta <- DTA(datasets = list(acc = ds), metadata = create_example_DTAMetaData())
  res <- as.data.frame(results(dta))

  # Three views of one run. If they disagreed, a report built from one of them
  # would contradict a report built from another, and neither would be wrong on
  # its own terms.
  total <- as.integer(status$n_columnspec_errors) +
    as.integer(status$n_rule_errors) +
    as.integer(status$n_import_errors)
  qa_step(
    "the message count is the sum of the three axis counts",
    total, nrow(msgs)
  )
  qa_step(
    "and the dataset summary repeats the same counts",
    c(
      columnspec = as.integer(status$n_columnspec_errors),
      rule = as.integer(status$n_rule_errors),
      import = as.integer(status$n_import_errors)
    ),
    c(
      columnspec = as.integer(res$n_columnspec_errors[[1]]),
      rule = as.integer(res$n_rule_errors[[1]]),
      import = as.integer(res$n_import_errors[[1]])
    )
  )
})
