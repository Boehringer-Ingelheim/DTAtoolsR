load_clinical_fixture_for_inspect <- function(filename) {
  spec_path <- system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
  fixture_path <- system.file("extdata", filename, package = "DTAtools")
  # These are guaranteed package assets, not optional dependencies. A bare
  # skip_if_not() here meant a broken install reported "skipped" and left CI
  # green with zero coverage of validation, inspection and reporting.
  expect_true(nzchar(spec_path), info = "clinical_dta.yaml missing from extdata")
  expect_true(nzchar(fixture_path), info = paste(filename, "missing from extdata"))

  dta <- read_dta_from_yaml(spec_path)
  dta <- load_file(dta, 1, file = fixture_path)
  check(dta, persist = FALSE, quiet = TRUE)
}

test_that("messages() provides sequential numeric ids", {
  dta <- load_clinical_fixture_for_inspect("clinical_data_error_all.csv")

  msgs <- messages(dta, as_tibble = FALSE)
  expect_true(is.data.frame(msgs))
  expect_gt(nrow(msgs), 0)
  expect_true(is.numeric(msgs$id))
  expect_equal(msgs$id, seq_len(nrow(msgs)))
})

test_that("inspect() gives detailed schema context", {
  dta <- load_clinical_fixture_for_inspect("clinical_data_error_columnspec.csv")

  msgs <- messages(dta, as_tibble = FALSE)
  columnspec_id <- msgs$id[msgs$source == "columnspec"][1]
  expect_false(is.na(columnspec_id))

  info <- inspect(dta, id = columnspec_id, as_tibble = FALSE)

  expect_true(is.data.frame(info))
  expect_gt(nrow(info), 0)
  expect_true(all(info$id == columnspec_id))
  expect_true(all(info$type == "columnspec"))
  expect_true(is.character(info$headline[[1]]) && nzchar(info$headline[[1]]))
  expect_true(is.character(info$why[[1]]) && nzchar(info$why[[1]]))
  expect_true("columnspec_keyword" %in% names(info))
  expect_true("columnspec_message" %in% names(info))
  expect_true(any(grepl("required|type|length|range|pattern", info$columnspec_keyword, ignore.case = TRUE), na.rm = TRUE))
})

test_that("inspect() schema matches stay specific to required HEIGHT message", {
  dta <- load_clinical_fixture_for_inspect("clinical_data_error_columnspec.csv")

  msgs <- messages(dta, as_tibble = FALSE)
  target <- msgs[
    msgs$source == "columnspec" &
      msgs$keyword == "required" &
      grepl("required property 'HEIGHT'", msgs$message, fixed = TRUE), ,
    drop = FALSE
  ]
  expect_gt(nrow(target), 0)

  info <- inspect(dta, id = target$id[[1]], as_tibble = FALSE)

  expect_true(is.data.frame(info))
  expect_gt(nrow(info), 0)
  expect_true(all(info$columnspec_keyword == "required"))
  expect_true(all(grepl("required property 'HEIGHT'", info$columnspec_message, fixed = TRUE)))
  expect_false(any(info$columnspec_column %in% c("BMI", "GENDER"), na.rm = TRUE))
})

test_that("inspect() gives detailed rule context with failing rows", {
  dta <- load_clinical_fixture_for_inspect("clinical_data_error_rules.csv")

  msgs <- messages(dta, as_tibble = FALSE)
  rule_id <- msgs$id[msgs$source == "rule"][1]
  expect_false(is.na(rule_id))

  info <- inspect(dta, id = rule_id, as_tibble = FALSE)

  expect_true(is.data.frame(info))
  expect_gt(nrow(info), 0)
  expect_true(all(info$id == rule_id))
  expect_true(all(info$type == "rule"))
  expect_true(is.character(info$rule_id[[1]]) && nzchar(info$rule_id[[1]]))
  expect_true(is.numeric(info$failing_row_count[[1]]))
  expect_gte(info$failing_row_count[[1]], 0)
  expect_true(any(grepl("^failing_", names(info))))
})

test_that("inspect() supports DTADataSetFile messages", {
  path <- tempfile(fileext = ".txt")
  if (file.exists(path)) {
    unlink(path)
  }

  ds <- DTADataSetFile(name = "missing_file", paths = path)
  ds <- check(ds, quiet = TRUE)

  msgs <- messages(ds, as_tibble = FALSE)
  expect_equal(nrow(msgs), 1)

  info <- inspect(ds, id = msgs$id[[1]], as_tibble = FALSE)

  expect_true(is.data.frame(info))
  expect_gt(nrow(info), 0)
  expect_equal(info$type[[1]], "rule")
  expect_equal(info$rule_id[[1]], "file_presence")
  expect_true(grepl("not found|readable|empty", info$message[[1]], ignore.case = TRUE))
})

test_that("inspect() supports multiple ids and tibble/data.frame output", {
  dta <- load_clinical_fixture_for_inspect("clinical_data_error_all.csv")
  msgs <- messages(dta, as_tibble = FALSE)
  ids <- c(1, 2)

  info_df <- inspect(dta, id = ids, as_tibble = FALSE)
  expect_true(is.data.frame(info_df))
  expect_false(inherits(info_df, "tbl_df"))
  expect_equal(sort(unique(info_df$id)), ids)

  info_tbl <- inspect(dta, id = ids, as_tibble = TRUE)
  if (requireNamespace("tibble", quietly = TRUE)) {
    expect_true(inherits(info_tbl, "tbl_df"))
  } else {
    expect_true(is.data.frame(info_tbl))
  }
  expect_equal(sort(unique(info_tbl$id)), ids)
})

test_that("inspect() without id returns all messages", {
  dta <- load_clinical_fixture_for_inspect("clinical_data_error_all.csv")
  msgs <- messages(dta, as_tibble = FALSE)

  info_df <- inspect(dta, as_tibble = FALSE)
  expect_true(is.data.frame(info_df))
  expect_equal(sort(unique(info_df$id)), msgs$id)

  info_tbl <- inspect(dta)
  if (requireNamespace("tibble", quietly = TRUE)) {
    expect_true(inherits(info_tbl, "tbl_df"))
  } else {
    expect_true(is.data.frame(info_tbl))
  }
  expect_equal(sort(unique(info_tbl$id)), msgs$id)
})

test_that("inspect() errors on unknown id", {
  dta <- load_clinical_fixture_for_inspect("clinical_data_error_all.csv")
  msgs <- messages(dta, as_tibble = FALSE)

  expect_error(inspect(dta, id = max(msgs$id) + 1), "not found")
  expect_error(inspect(dta, id = 0), "positive")
})

# ---------------------------------------------------------------------------
# Lazy tables in inspect(): a streamed table must not be pulled fully into
# memory just to show one message's context (dta_inspect_table_frame()), and
# a table that cannot be safely re-read (a one-shot RecordBatchReader) must
# say so rather than silently claiming a clean row or zero failing rows.
# ---------------------------------------------------------------------------

# AGE carries the import error and SCORE carries the rule violation, kept
# apart on purpose. A lazily-opened table has every column pinned to utf8
# (see dta_delim_reader_plan() in R/DTAFileTabular-class.R), so a cell that
# fails import stays literal text ("abc") there while the in-memory path
# already holds the coerced NA that replaced it -- a real difference between
# the two paths, but one this fix does not touch and cannot remove from
# R/validationReporting.R alone. Routing the rule at a clean numeric column
# keeps that pre-existing divergence out of the failing-row preview, which is
# what this fix actually governs.
inspect_stream_specs <- function() {
  DTAColumnSpecCollection(
    columns = list(
      SUBJECT_ID = DTAColumnSpec(id = "SUBJECT_ID", type = "SAS Char", length = 4, nullable = FALSE),
      VISIT = DTAColumnSpec(id = "VISIT", type = "SAS Char", nullable = TRUE),
      AGE = DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE),
      SCORE = DTAColumnSpec(id = "SCORE", type = "SAS Num", nullable = TRUE),
      STATUS = DTAColumnSpec(
        id = "STATUS", type = "SAS Char", nullable = FALSE, values = c("OK", "BAD")
      )
    ),
    rules = list(
      DTARuleFactory("score_range", "col_range", columns = "SCORE", min = 0, max = 99)
    )
  )
}

inspect_stream_csv <- function(path) {
  writeLines(c(
    "SUBJECT_ID,VISIT,AGE,SCORE,STATUS",
    "S001,V1,25,50,OK", # clean
    "S002,V1,abc,60,OK", # import error: AGE unconvertible
    "S003,V1,30,150,OK", # rule error: SCORE out of range
    "S004,V1,35,70,WRONG", # columnspec error: STATUS not in {OK, BAD}
    "S005,V1,40,80,OK" # clean
  ), path)
}

# Every stored value compared as text: a lazily-held table's columns are
# always character (see the note above), while the in-memory path's are typed
# per spec, so "150" and 150 must compare equal here without that pinning
# difference registering as a mismatch.
inspect_stream_as_char <- function(df) {
  as.data.frame(lapply(df, as.character), stringsAsFactors = FALSE)
}

inspect_stream_dataset <- function(path, stream) {
  ds <- DTADataSetTabular(
    name = "d",
    specs = inspect_stream_specs(),
    files = list(DTAFileCSV(filename = basename(path)))
  )
  ds <- load_file(ds, file = path, handler_index = 1, stream = stream)
  check(ds, persist = FALSE, quiet = TRUE)
}

# Identifies the same underlying defect across two loads of the same file,
# independent of whatever id numbering or row order each load assigns.
inspect_stream_message_key <- function(msgs) {
  paste(msgs$source, msgs$row, msgs$column, msgs$rule_id, sep = "|")
}

test_that("inspect() agrees between an in-memory table and a streamed one", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "inspect_stream.csv")
  inspect_stream_csv(path)

  eager <- inspect_stream_dataset(path, stream = "never")
  lazy <- inspect_stream_dataset(path, stream = "always")

  expect_false(dta_table_is_lazy(tables(eager)[["inspect_stream"]]))
  expect_true(dta_table_is_lazy(tables(lazy)[["inspect_stream"]]))

  eager_msgs <- messages(eager, as_tibble = FALSE)
  lazy_msgs <- messages(lazy, as_tibble = FALSE)

  # The fixture is built to exercise all three axes; a fixture that tripped
  # only one would leave the other two branches of the fix unexercised.
  expect_setequal(unique(eager_msgs$source), c("columnspec", "import", "rule"))
  eager_key <- inspect_stream_message_key(eager_msgs)
  lazy_key <- inspect_stream_message_key(lazy_msgs)
  expect_setequal(eager_key, lazy_key)

  for (key in unique(eager_key)) {
    eager_id <- eager_msgs$id[eager_key == key][[1]]
    lazy_id <- lazy_msgs$id[lazy_key == key][[1]]
    source <- eager_msgs$source[eager_key == key][[1]]

    eager_info <- inspect(eager, id = eager_id, as_tibble = FALSE)
    lazy_info <- inspect(lazy, id = lazy_id, as_tibble = FALSE)

    context_cols <- intersect(
      grep("^context_", names(eager_info), value = TRUE),
      grep("^context_", names(lazy_info), value = TRUE)
    )
    # The import message's own column is deliberately excluded: it is the one
    # case where the two paths are expected to disagree (see the note above),
    # not a case this fix is meant to make agree.
    if (identical(source, "import")) {
      context_cols <- setdiff(context_cols, paste0("context_", eager_msgs$column[eager_key == key][[1]]))
    }
    if (length(context_cols) > 0) {
      expect_equal(
        inspect_stream_as_char(eager_info[, context_cols, drop = FALSE]),
        inspect_stream_as_char(lazy_info[, context_cols, drop = FALSE]),
        info = paste("row_context differs for", key)
      )
    }

    preview_cols <- intersect(
      grep("^failing_", names(eager_info), value = TRUE),
      grep("^failing_", names(lazy_info), value = TRUE)
    )
    if (length(preview_cols) > 0) {
      expect_equal(
        inspect_stream_as_char(eager_info[, preview_cols, drop = FALSE]),
        inspect_stream_as_char(lazy_info[, preview_cols, drop = FALSE]),
        info = paste("failing-row preview differs for", key)
      )
    }
  }
})

test_that("dta_inspect_table_frame reads only the requested columns and row bound from a Dataset", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "frame.csv")
  writeLines(c("A,B,C", "1,x,10", "2,y,20", "3,z,30", "4,w,40"), path)

  ds <- dta_open_normalized_dataset(path, delim = ",", quote = '"', has_header = TRUE)
  expect_true(dta_table_is_lazy(ds))

  bounded <- dta_inspect_table_frame(ds, columns = c("A", "C"), max_row = 2)
  expect_identical(names(bounded), c("A", "C"))
  expect_equal(nrow(bounded), 2L)
  expect_identical(as.character(bounded$A), c("1", "2"))

  unbounded <- dta_inspect_table_frame(ds, columns = c("A", "C"))
  expect_equal(nrow(unbounded), 4L)

  # No columns and no row bound must match reading the whole table directly.
  # (`as.data.frame(ds)` itself comes back as a tibble; only the class differs.)
  expected <- as.data.frame(ds)
  class(expected) <- "data.frame"
  expect_equal(dta_inspect_table_frame(ds), expected)
})

test_that("dta_inspect_table_frame refuses to re-read a RecordBatchReader", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "reader.csv")
  writeLines(c("A,B", "1,x", "2,y"), path)

  ds <- dta_open_normalized_dataset(path, delim = ",", quote = '"', has_header = TRUE)
  reader <- ds$NewScan()$Finish()$ToRecordBatchReader()
  expect_true(inherits(reader, "RecordBatchReader"))

  # One-shot and possibly already spent by the scan that just ran: there is no
  # safe way to read it again, so this must say so (NULL) rather than guess.
  expect_null(dta_inspect_table_frame(reader, columns = "A", max_row = 1))
})

test_that("inspect() reports unreadable failing rows rather than claiming zero", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "inspect_reader.csv")
  inspect_stream_csv(path)

  lazy <- inspect_stream_dataset(path, stream = "always")
  table_name <- "inspect_reader"
  reader <- tables(lazy)[["inspect_reader"]]$NewScan()$Finish()$ToRecordBatchReader()
  lazy@tables[[table_name]] <- reader

  msgs <- messages(lazy, as_tibble = FALSE)
  rule_id <- msgs$id[msgs$source == "rule"][[1]]

  info <- inspect(lazy, id = rule_id, as_tibble = FALSE)
  expect_true(is.na(info$failing_row_count[[1]]))
  expect_true(grepl("unavailable", info$why[[1]], fixed = TRUE))
  expect_false(any(grepl("^failing_", names(info)) & names(info) != "failing_row_count"))
})
