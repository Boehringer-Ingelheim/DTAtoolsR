# Stage 0 of the import error mechanism.
#
# The import axis is wired through every result shape *before* anything starts
# producing import errors, and always reports "no issues" here. These tests pin
# the wiring, so a later stage that starts recording errors cannot silently skip
# a reporting surface -- and they pin that stage 0 changed no `ok` value.

import_detail_fields <- c(
  "import_valid",
  "n_import_errors",
  "import_errors",
  "schema_version"
)

example_import_errors <- function() {
  data.frame(
    row = c(2L, 5L),
    column = c("AGE", "CONSENT_DATE"),
    raw = c("ninety", "2026-12-31 at the earliest"),
    declared_type = c("SAS Num", "SAS Date"),
    reason = c("not_convertible", "not_convertible"),
    stringsAsFactors = FALSE
  )
}

legacy_details <- function(ok = TRUE, schema_valid = TRUE) {
  # Exactly the shape validate_table_detailed() returned before the import axis
  # existed: no import_valid, no n_import_errors, no schema_version.
  list(
    ok = ok,
    schema_valid = schema_valid,
    rules_valid = TRUE,
    n_schema_errors = 0,
    n_rule_errors = 0,
    schema_errors = list(summarised_error = NULL, full_error = NULL),
    rule_results = list(),
    rule_errors = list()
  )
}


test_that("validate_table_detailed() reports a clean import axis", {
  ds <- create_example_DTADataSetTabular(2)
  details <- validate_table_detailed(
    ds@specs,
    as.data.frame(ds@tables[["tab1"]]),
    verbose = FALSE
  )

  expect_true(all(import_detail_fields %in% names(details)))
  expect_true(details$import_valid)
  expect_identical(details$n_import_errors, 0L)
  expect_null(details$import_errors)
  expect_identical(details$schema_version, 2L)
})


test_that("ok is the conjunction of three independent axes", {
  clean <- list(schema_valid = TRUE, rules_valid = TRUE, import_valid = TRUE)
  expect_true(dta_details_ok(clean))

  # The point of the axis: schema and rules are clean, and the table still
  # fails because a value could not be represented in its declared type.
  import_broken <- clean
  import_broken$import_valid <- FALSE
  expect_false(dta_details_ok(import_broken))

  # "Unknown" is not a pass either.
  import_unknown <- clean
  import_unknown$import_valid <- NA
  expect_false(dta_details_ok(import_unknown))

  schema_broken <- clean
  schema_broken$schema_valid <- FALSE
  expect_false(dta_details_ok(schema_broken))

  rules_broken <- clean
  rules_broken$rules_valid <- FALSE
  expect_false(dta_details_ok(rules_broken))
})


test_that("pre-v2 details migrate to unknown, never to clean", {
  migrated <- dta_migrate_validation_details(legacy_details(ok = TRUE))

  # NA, not TRUE/0: an artifact written before import checking existed must not
  # assert a clean import axis it never checked.
  expect_true(is.na(migrated$import_valid))
  expect_false(isTRUE(migrated$import_valid))
  expect_identical(migrated$n_import_errors, NA_integer_)
  expect_true("import_errors" %in% names(migrated))
  expect_null(migrated$import_errors)
  expect_identical(migrated$schema_version, 1L)

  # The recorded ok is left exactly as recorded, never recomputed from
  # incomplete data.
  expect_true(migrated$ok)
  expect_false(
    dta_migrate_validation_details(legacy_details(ok = FALSE, schema_valid = FALSE))$ok
  )

  # A current result is returned untouched.
  ds <- create_example_DTADataSetTabular(2)
  current <- validate_table_detailed(
    ds@specs,
    as.data.frame(ds@tables[["tab1"]]),
    verbose = FALSE
  )
  expect_identical(dta_migrate_validation_details(current), current)
})


test_that("a pre-v2 artifact is migrated on read and reported as unknown", {
  artifact_dir <- file.path(tempdir(), "dta_import_axis_artifacts")
  on.exit(unlink(artifact_dir, recursive = TRUE), add = TRUE)

  ds <- check(
    create_example_DTADataSetTabular(2),
    tables = "tab1",
    persist = TRUE,
    artifact_dir = artifact_dir,
    quiet = TRUE
  )
  path <- ds@validation_index[["tab1"]]$artifact_path
  expect_true(file.exists(path))

  # Rewrite the artifact in the pre-import-axis shape, with a recorded ok of
  # TRUE that must survive the migration untouched.
  stored <- readRDS(path)
  stored$import_valid <- NULL
  stored$n_import_errors <- NULL
  stored$import_errors <- NULL
  stored$schema_version <- NULL
  stored$ok <- TRUE
  saveRDS(stored, path)

  details <- validation_errors(ds, table = "tab1", source = "artifact")
  expect_true(is.na(details$import_valid))
  expect_identical(details$n_import_errors, NA_integer_)
  expect_identical(details$schema_version, 1L)
  expect_true(details$ok)

  msgs <- messages(ds, source = "artifact", as_tibble = FALSE)
  # The column contract holds even though the import frame is populated.
  expect_named(
    msgs,
    c("id", "dataset", "target", "severity", "source", "rule_id", "row", "column", "keyword", "message")
  )

  import_rows <- msgs[msgs$source == "import", , drop = FALSE]
  expect_equal(nrow(import_rows), 1)
  expect_equal(import_rows$severity, "warning")
  expect_equal(
    import_rows$message,
    "validation artifact predates import checking (schema_version 1); re-run check(force = TRUE)"
  )
})


test_that("import messages match the rule message column contract exactly", {
  details <- list(
    import_valid = FALSE,
    n_import_errors = 2L,
    import_errors = example_import_errors(),
    schema_version = 2L
  )
  rule_details <- list(rule_errors = list(list(id = "r1", message = "rule violated")))

  import_df <- dta_import_messages_to_df("ds", "tab", details)
  rule_df <- dta_rule_messages_to_df("ds", "tab", rule_details)

  # Two populated frames with differing columns make rbind() error, not forgive.
  expect_identical(names(import_df), names(rule_df))
  expect_equal(ncol(import_df), 9)
  expect_equal(nrow(rbind(rule_df, import_df)), 3)

  expect_equal(nrow(import_df), 2)
  expect_true(all(import_df$source == "import"))
  expect_true(all(import_df$severity == "error"))
  expect_equal(import_df$row, c(2, 5))
  expect_equal(import_df$column, c("AGE", "CONSENT_DATE"))

  # The raw value travels inside the message string; adding a column for it
  # would break the rbind contract above.
  expect_false("raw" %in% names(import_df))
  expect_true(grepl("ninety", import_df$message[1], fixed = TRUE))
  expect_true(grepl("SAS Num", import_df$message[1], fixed = TRUE))

  # No issues -> the shared empty frame.
  clean <- list(import_valid = TRUE, n_import_errors = 0L, import_errors = NULL, schema_version = 2L)
  expect_identical(dta_import_messages_to_df("ds", "tab", clean), dta_empty_messages())

  # Unknown -> exactly one warning row.
  unknown <- dta_import_messages_to_df("ds", "tab", dta_migrate_validation_details(legacy_details()))
  expect_equal(nrow(unknown), 1)
  expect_identical(names(unknown), names(rule_df))
  expect_equal(unknown$severity, "warning")
})


test_that("as.data.frame() on details flattens import errors", {
  details <- dta_as_validation_details(list(
    ok = FALSE,
    schema_valid = TRUE,
    rules_valid = TRUE,
    import_valid = FALSE,
    n_schema_errors = 0,
    n_rule_errors = 0,
    n_import_errors = 2L,
    schema_errors = list(summarised_error = NULL, full_error = NULL),
    rule_results = list(),
    rule_errors = list(),
    import_errors = example_import_errors(),
    schema_version = 2L
  ))

  df <- as.data.frame(details)
  expect_identical(
    names(df),
    c("source", "rule_id", "row", "column", "keyword", "message")
  )
  expect_equal(nrow(df), 2)
  expect_true(all(df$source == "import"))
  expect_equal(df$row, c(2L, 5L))
  expect_equal(df$keyword, c("not_convertible", "not_convertible"))
  expect_false(any(is.na(df$message)))
})


test_that("validation_status() and results() carry n_import_errors", {
  ds <- check(
    create_example_DTADataSetTabular(2),
    tables = "tab1",
    persist = FALSE,
    quiet = TRUE
  )

  status <- validation_status(ds)
  expect_true("n_import_errors" %in% names(status))
  expect_equal(status$n_import_errors, 0L)

  res <- results(ds)
  expect_true("n_import_errors" %in% names(res))
  expect_equal(res$n_import_errors, 0L)

  # not_validated stub
  fresh <- create_example_DTADataSetTabular(2)
  status_stub <- validation_status(fresh)
  expect_true("n_import_errors" %in% names(status_stub))
  expect_true(is.na(status_stub$n_import_errors))

  res_stub <- results(fresh)
  expect_true("n_import_errors" %in% names(res_stub))
  expect_true(is.na(res_stub$n_import_errors))

  # An index entry written before the axis existed is unknown, not clean.
  legacy_entry <- list(
    validated_at = Sys.time(),
    ok = TRUE,
    run_id = "run",
    validation_run = "run",
    n_schema_errors = 0L,
    n_rule_errors = 0L
  )
  row <- dta_validation_result_to_row("tab1", "validated", legacy_entry)
  expect_true("n_import_errors" %in% names(row))
  expect_true(is.na(row$n_import_errors))

  # Both dta_results_from_status() branches. The empty branch is called with a
  # zero-length dataset name here: it recycles `dataset_name` into a zero-row
  # frame, so a length-1 name makes data.frame() abort. That is pre-existing
  # behaviour, unrelated to the import axis, and left untouched by stage 0.
  empty_null <- dta_results_from_status(NULL, character(0))
  empty_zero_row <- dta_results_from_status(status[0, ], character(0))
  expect_equal(nrow(empty_null), 0)
  expect_true("n_import_errors" %in% names(empty_null))
  expect_true("n_import_errors" %in% names(empty_zero_row))
  expect_identical(names(empty_null), names(empty_zero_row))
})


test_that("results(DTA) keeps one column set for real and stub datasets", {
  dta <- create_example_DTA()
  # The non-DTADataSet stub frame is populated, so a column mismatch with the
  # real frame makes the rbind error rather than forgive.
  dta@datasets[["not_a_dataset"]] <- list()

  res <- results(dta)
  expect_true("n_import_errors" %in% names(res))
  expect_equal(nrow(res), 3)
  expect_true(is.na(res$n_import_errors[res$dataset == "not_a_dataset"]))
})


test_that("DTADataSetTabular records the import axis in its index entry", {
  ds <- check(
    create_example_DTADataSetTabular(2),
    tables = "tab1",
    persist = FALSE,
    quiet = TRUE
  )

  entry <- ds@validation_index[["tab1"]]
  expect_true("n_import_errors" %in% names(entry))
  expect_identical(entry$n_import_errors, 0L)
})


test_that("DTADataSetTabular carries import_issues keyed by table name", {
  ds <- create_example_DTADataSetTabular(2)
  expect_identical(ds@import_issues, list())

  ds@import_issues[["tab1"]] <- list(placeholder = TRUE)
  expect_named(ds@import_issues, "tab1")

  cleared <- clear_validation(ds, tables = "tab1")
  expect_length(cleared@import_issues, 0)

  invalidated <- invalidate_by_spec_change(ds, tables = "tab1")
  expect_length(invalidated@import_issues, 0)

  expect_error(
    {
      ds@import_issues <- list(a = 1, b = 2)
      ds
    },
    "import_issues"
  )
})


test_that("DTADataSetFile check() records the import axis", {
  path <- tempfile(fileext = ".txt")
  writeLines("hello world", path)
  on.exit(unlink(path), add = TRUE)

  ds <- check(DTADataSetFile(name = "notes", paths = path), quiet = TRUE)
  key <- basename(path)

  entry <- ds@validation_index[[key]]
  expect_identical(entry$n_import_errors, 0L)

  details <- ds@validation_store[[key]]
  expect_true(all(import_detail_fields %in% names(details)))
  expect_true(details$import_valid)
  expect_identical(details$n_import_errors, 0L)
  expect_identical(details$schema_version, 2L)

  status <- validation_status(ds)
  expect_equal(status$n_import_errors, 0L)
  expect_true("n_import_errors" %in% names(results(ds)))

  # The not_validated stub of validation_status(DTADataSetFile).
  fresh <- DTADataSetFile(name = "notes", paths = path)
  fresh@validation_index[[key]] <- NULL
  fresh@validation_index[key] <- list(NULL)
  stub <- validation_status(fresh, tables = key)
  expect_true("n_import_errors" %in% names(stub))
  expect_true(is.na(stub$n_import_errors))
})


test_that("failing DTADataSetFile details also carry the import axis", {
  path <- tempfile(fileext = ".txt")
  if (file.exists(path)) {
    unlink(path)
  }

  ds <- check(DTADataSetFile(name = "missing_file", paths = path), quiet = TRUE)
  details <- ds@validation_store[[basename(path)]]

  expect_false(details$ok)
  expect_true(details$import_valid)
  expect_identical(details$n_import_errors, 0L)
  expect_identical(details$schema_version, 2L)
})


test_that("check(DTA) summarises the import axis", {
  dta <- check(create_example_DTA(), persist = FALSE, quiet = TRUE)
  summary_df <- attr(dta, "last_validation_summary")

  expect_true("n_import_errors" %in% names(summary_df))
  expect_equal(sum(summary_df$n_import_errors, na.rm = TRUE), 0)
})


test_that("inspect() surfaces the import axis of a details list", {
  path <- tempfile(fileext = ".txt")
  if (file.exists(path)) {
    unlink(path)
  }

  ds <- check(DTADataSetFile(name = "missing_file", paths = path), quiet = TRUE)
  ins <- inspect(ds, id = 1, as_tibble = FALSE)

  expect_true(all(
    c("details_import_valid", "details_n_import_errors") %in% names(ins)
  ))
  expect_true(ins$details_import_valid)
  expect_equal(ins$details_n_import_errors, 0L)
})


test_that("flattened inspect records keep an unknown import axis unknown", {
  record <- list(
    id = 1L,
    dataset = "ds",
    target = "tab",
    source = "import",
    severity = "warning",
    type = "import",
    headline = "h",
    message = "m",
    details = dta_migrate_validation_details(legacy_details())
  )

  flat <- dta_flatten_inspect_record(record)
  # isTRUE(NA) would report FALSE here and invent a failure never observed.
  expect_true(is.na(flat$details_import_valid))
  expect_true(is.na(flat$details_n_import_errors))
})


test_that("import messages get their own inspect branch, not the rule branch", {
  ds <- check(
    create_example_DTADataSetTabular(2),
    tables = "tab1",
    persist = FALSE,
    quiet = TRUE
  )

  msg_row <- data.frame(
    id = 1L,
    dataset = "demographics",
    target = "tab1",
    severity = "error",
    source = "import",
    rule_id = NA_character_,
    row = 2,
    column = "AGE",
    keyword = "not_convertible",
    message = "value 'ninety' in column 'AGE' cannot be represented",
    stringsAsFactors = FALSE
  )

  record <- dta_inspect_tabular_message(ds, msg_row, source = "memory")
  # Without the branch this would fall through to the rule lookup and report
  # type "rule" with rule_id NA.
  expect_equal(record$type, "import")
  expect_null(record$rule_id)
  expect_true(grepl("declared type", record$why, fixed = TRUE))
})


test_that("stage 0 regression canaries: no behaviour changed", {
  # A genuinely missing value is NOT an import error, and never becomes one.
  all_na <- rule_check_range(
    DTARuleColRange(id = "range_na", columns = "AGE", min = 18, max = 65),
    data.frame(AGE = c(NA, NA), stringsAsFactors = FALSE)
  )
  expect_true(all_na$valid)

  # The clean clinical fixture still validates, on all three axes.
  dta <- read_dta_from_yaml(
    system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
  )
  dta <- load_file(
    dta,
    "clinical_data",
    file = system.file("extdata", "clinical_data.csv", package = "DTAtools")
  )
  dta <- check(dta, persist = FALSE, quiet = TRUE)

  res <- results(dta)
  expect_equal(res$status, "validated")
  expect_equal(res$n_import_errors, 0L)

  status <- validation_status(dta[[1]])
  expect_true(all(status$ok))

  # The messages() column contract is unchanged by the new source.
  msgs <- messages(dta, as_tibble = FALSE)
  expect_named(
    msgs,
    c("id", "dataset", "target", "severity", "source", "rule_id", "row", "column", "keyword", "message")
  )
  expect_equal(nrow(msgs), 0)

  # The example fixture's failure is still a failure, for the same reason.
  ds <- check(
    create_example_DTADataSetTabular(2),
    tables = "tab1",
    persist = FALSE,
    quiet = TRUE
  )
  ds_status <- validation_status(ds)
  expect_false(ds_status$ok)
  expect_equal(ds_status$n_schema_errors, 7)
  expect_equal(ds_status$n_import_errors, 0L)
})


# ---------------------------------------------------------------------------
# Stage 1: the rule layer starts producing real import errors.
# ---------------------------------------------------------------------------

import_only_specs <- function() {
  # An unconvertible value in the IF column leaves the row's IF condition NA, so
  # the row is not counted as a rule violation. That isolates the import axis:
  # schema and rules are clean and the table still fails.
  DTAColumnSpecCollection(
    columns = list(
      AGE = DTAColumnSpec(id = "AGE", type = "SAS Char", length = 10, nullable = TRUE),
      STATUS = DTAColumnSpec(id = "STATUS", type = "SAS Char", length = 10, nullable = TRUE)
    ),
    rules = list(DTARuleColCondition(
      id = "adult_status",
      condition = list(AGE = list(greater_equal = 18)),
      then = list(STATUS = list(equals = "OK"))
    ))
  )
}

range_import_specs <- function() {
  DTAColumnSpecCollection(
    columns = list(
      AGE = DTAColumnSpec(id = "AGE", type = "SAS Char", length = 10, nullable = TRUE)
    ),
    rules = list(DTARuleColRange(id = "age_range", columns = "AGE", min = 18, max = 65))
  )
}


test_that("an unconvertible value fails the run on the import axis alone", {
  details <- validate_table_detailed(
    import_only_specs(),
    data.frame(AGE = c("30", "ninety"), STATUS = c("OK", "OK"), stringsAsFactors = FALSE),
    verbose = FALSE
  )

  expect_true(details$schema_valid)
  expect_true(details$rules_valid)
  expect_equal(details$n_rule_errors, 0)

  # The only problem is a value that could not be represented as a number.
  expect_false(details$import_valid)
  expect_identical(details$n_import_errors, 1L)
  expect_false(details$ok)

  expect_equal(details$import_errors$row, 2L)
  expect_equal(details$import_errors$column, "AGE")
  expect_equal(details$import_errors$raw, "ninety")
  expect_equal(details$import_errors$reason, "not_convertible")
  # The declared type comes from the column spec, not from the storage type.
  expect_equal(details$import_errors$declared_type, "SAS Char")

  # validate_table() surfaces it rather than returning the table as valid.
  expect_error(
    validate_table(
      import_only_specs(),
      data.frame(AGE = c("30", "ninety"), STATUS = c("OK", "OK"), stringsAsFactors = FALSE),
      verbose = FALSE
    ),
    class = "rlang_error"
  )
})


test_that("an unconvertible value is reported on BOTH axes, never moved between them", {
  details <- validate_table_detailed(
    range_import_specs(),
    data.frame(AGE = c("30", "ninety", "700"), stringsAsFactors = FALSE),
    verbose = FALSE
  )

  # Import axis: one unrepresentable value.
  expect_false(details$import_valid)
  expect_identical(details$n_import_errors, 1L)
  expect_equal(details$import_errors$raw, "ninety")

  # Rule axis: unchanged in kind, and it counts BOTH offending rows. Moving
  # "ninety" to the import axis alone would make a consumer reading
  # n_rule_errors see fewer errors than before.
  expect_false(details$rules_valid)
  expect_equal(details$n_rule_errors, 1)
  expect_match(details$rule_errors[[1]]$message, "violated: 2 rows")

  expect_false(details$ok)

  # Flattened details carry both sources.
  flat <- as.data.frame(dta_as_validation_details(details))
  expect_equal(sort(unique(flat$source)), c("import", "rule"))
})


test_that("import errors reach messages() as source 'import' carrying the raw value", {
  ds <- DTADataSetTabular(
    name = "imports",
    specs = range_import_specs(),
    tables = list(tab = arrow::arrow_table(
      data.frame(AGE = c("30", "ninety", "700"), stringsAsFactors = FALSE)
    ))
  )
  ds <- check(ds, persist = FALSE, quiet = TRUE)

  msgs <- messages(ds, as_tibble = FALSE)
  # The column contract is unchanged by the populated import frame.
  expect_named(
    msgs,
    c("id", "dataset", "target", "severity", "source", "rule_id", "row", "column", "keyword", "message")
  )

  import_msgs <- msgs[msgs$source == "import", , drop = FALSE]
  expect_equal(nrow(import_msgs), 1)
  expect_equal(import_msgs$severity, "error")
  expect_equal(import_msgs$row, 2)
  expect_equal(import_msgs$column, "AGE")
  expect_equal(import_msgs$keyword, "not_convertible")
  # The raw offending text must be visible in the message itself.
  expect_true(grepl("ninety", import_msgs$message, fixed = TRUE))
  expect_true(grepl("SAS Char", import_msgs$message, fixed = TRUE))

  # The rule message is still there: both axes report.
  expect_equal(sum(msgs$source == "rule"), 1)

  status <- validation_status(ds)
  expect_false(status$ok)
  expect_identical(status$n_import_errors, 1L)
  expect_equal(results(ds)$status, "failed")

  # inspect() routes the import message to the import branch and shows the raw
  # value it matched.
  info <- inspect(ds, id = import_msgs$id[[1]], as_tibble = FALSE)
  expect_true(all(info$type == "import"))
  expect_true(any(grepl("ninety", info$import_raw, fixed = TRUE)))
})


test_that("stage 1 canaries: over-firing would show up here", {
  # A genuinely missing value never becomes an import error.
  clean_na <- validate_table_detailed(
    range_import_specs(),
    data.frame(AGE = c(NA, NA), stringsAsFactors = FALSE),
    verbose = FALSE
  )
  expect_true(clean_na$rules_valid)
  expect_true(clean_na$import_valid)
  expect_identical(clean_na$n_import_errors, 0L)
  expect_null(clean_na$import_errors)
  expect_true(clean_na$ok)

  # The clean clinical fixture is still validated on all three axes. This is
  # the over-firing canary: it has range, min/max and greater_equal rules over
  # real numeric columns.
  dta <- read_dta_from_yaml(
    system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
  )
  dta <- load_file(
    dta,
    "clinical_data",
    file = system.file("extdata", "clinical_data.csv", package = "DTAtools")
  )
  dta <- check(dta, persist = FALSE, quiet = TRUE)

  res <- results(dta)
  expect_equal(res$status, "validated")
  expect_equal(res$n_import_errors, 0L)
  expect_equal(nrow(messages(dta, as_tibble = FALSE)), 0)
})


# ---------------------------------------------------------------------------
# check() console reporting must name the import axis.
# ---------------------------------------------------------------------------

test_that("check() console output states the import failure instead of only claiming success", {
  specs <- DTAColumnSpecCollection(
    columns = list(
      BMI = DTAColumnSpec(id = "BMI", type = "SAS Num", nullable = TRUE)
    )
  )
  ds <- DTADataSetTabular(
    name = "imports",
    specs = specs,
    tables = list(tab1 = data.frame(BMI = c("20.5", "heavy"), stringsAsFactors = FALSE))
  )

  # Confirm this is exactly the reported scenario: schema and rules are both
  # clean (the column is nullable, so the NA left by the failed conversion is
  # not itself a schema error), and the ONLY defect is on the import axis.
  details <- validate_table_detailed(
    specs,
    as.data.frame(ds@tables[["tab1"]]),
    verbose = FALSE
  )
  expect_true(details$schema_valid)
  expect_true(details$rules_valid)
  expect_false(details$import_valid)
  expect_false(details$ok)

  # cli's alerts are emitted as conditions (message()), not stdout, so they
  # must be captured with capture_messages() -- capture_output_lines() alone
  # sees none of this output and would pass vacuously.
  output <- paste(
    testthat::capture_messages(
      check(ds, persist = FALSE, quiet = FALSE)
    ),
    collapse = "\n"
  )

  # The console report must name the actual cause of failure: the import
  # axis, the offending column, and the raw text that could not be
  # represented -- not merely assert schema/rule success and then fail
  # silently.
  expect_true(grepl("import", output, ignore.case = TRUE))
  expect_true(grepl("BMI", output, fixed = TRUE))
  expect_true(grepl("heavy", output, fixed = TRUE))
})
