# Tests for the Shiny app shipped under inst/shiny/dta_app.
#
# The app is not part of the package namespace, so its helpers are sourced into
# a scratch environment by helper-shiny-app.R and fetched with dta_app_fn().
# app.R itself calls Shiny at top level and cannot be evaluated outside a
# running app; the few assertions about its wiring parse it instead.
#
# Focus: the three-axis validation contract (schema / rules / import). The app
# was written against two axes, so anything reading only n_schema_errors and
# n_rule_errors reports a table with an unrepresentable value as clean.

# A tabular dataset with one character and one numeric column. `val` decides
# whether VAL holds an import error, and nothing else is ever wrong with it --
# so a red status can only come from the import axis.
app_test_dataset <- function(val, name = "ds1") {
  DTAtools::DTADataSetTabular(
    name = name,
    specs = dta_app_char_num_specs(),
    tables = list(
      t1 = data.frame(
        SUBJID = c("a", "b"),
        VAL = val,
        stringsAsFactors = FALSE
      )
    )
  )
}

app_test_dta <- function(val, name = "ds1") {
  DTAtools::DTA(
    datasets = DTAtools::check(
      app_test_dataset(val, name),
      force = TRUE, persist = FALSE, quiet = TRUE
    )
  )
}

# One row of a validation_status() frame.
app_status_row <- function(table = "t1", ok = TRUE, schema = 0L, rule = 0L,
                           import = 0L) {
  data.frame(
    table = table,
    target_type = "table",
    status = if (is.na(ok)) "not_validated" else "validated",
    ok = ok,
    n_schema_errors = schema,
    n_rule_errors = rule,
    n_import_errors = import,
    stringsAsFactors = FALSE
  )
}


test_that("the app directory is found and its helpers load", {
  dir <- dta_app_dir()
  expect_true(dir.exists(dir))
  expect_true(file.exists(file.path(dir, "app.R")))
  expect_true(is.function(dta_app_fn("dta_table_status_map")))
})


# ---- Task 1: the per-file tick weighs the import axis ---------------------

test_that("dta_table_status_from_status_df weighs all three axes", {
  status_of <- dta_app_fn("dta_table_status_from_status_df")

  # The clean case: nothing wrong on any axis.
  expect_identical(
    status_of(app_status_row(ok = TRUE)),
    c(t1 = "pass")
  )

  # Import errors ONLY. Schema and rules are clean, so a two-axis reading paints
  # this green; ok is FALSE and it must be red.
  expect_identical(
    status_of(app_status_row(ok = FALSE, import = 1L)),
    c(t1 = "fail")
  )

  # ... and the import count alone is enough, even if `ok` disagrees. A status
  # frame that claims ok while reporting an import error is inconsistent, and
  # the inconsistency must resolve to red, never to green.
  expect_identical(
    status_of(app_status_row(ok = TRUE, import = 2L)),
    c(t1 = "fail")
  )

  # The other two axes keep working.
  expect_identical(
    status_of(app_status_row(ok = FALSE, schema = 3L)),
    c(t1 = "fail")
  )
  expect_identical(
    status_of(app_status_row(ok = FALSE, rule = 1L)),
    c(t1 = "fail")
  )
})


test_that("an unknown import axis is neither pass nor fail", {
  status_of <- dta_app_fn("dta_table_status_from_status_df")

  # A pre-v2 artifact: the two-axis run it came from recorded ok = TRUE, and the
  # import axis was never checked (NA). Reading NA as 0 would paint it green and
  # assert a check that never happened.
  unknown <- status_of(app_status_row(ok = TRUE, import = NA_integer_))
  expect_identical(unknown, c(t1 = "unknown"))
  expect_false(identical(unlist(unknown, use.names = FALSE), "pass"))
  expect_false(identical(unlist(unknown, use.names = FALSE), "fail"))

  # A definite defect outranks the unknown axis: still a plain failure.
  expect_identical(
    status_of(app_status_row(ok = FALSE, schema = 2L, import = NA_integer_)),
    c(t1 = "fail")
  )

  # A table that was never validated carries NA everywhere, including
  # n_import_errors. That is "pending", not "unknown" -- the import axis is no
  # more unknown than the other two.
  expect_identical(
    status_of(app_status_row(ok = NA, schema = NA_integer_,
                             rule = NA_integer_, import = NA_integer_)),
    c(t1 = "pending")
  )
})


test_that("a status frame without the import column keeps two-axis behaviour", {
  status_of <- dta_app_fn("dta_table_status_from_status_df")

  # The column being ABSENT is not the same as being NA: it means the frame
  # predates the column entirely. Treating that as "unknown" would turn every
  # tick amber for such an object.
  old <- app_status_row(ok = TRUE)
  old$n_import_errors <- NULL
  expect_identical(status_of(old), c(t1 = "pass"))

  expect_identical(
    status_of(NULL),
    stats::setNames(character(0), character(0))
  )
  expect_identical(
    status_of(app_status_row()[0, , drop = FALSE]),
    stats::setNames(character(0), character(0))
  )
})


test_that("dta_table_status_map reports a real import-only failure as fail", {
  dta <- app_test_dta(c("1.5", "heavy"))

  # Pin the premise: the ONLY thing wrong is the import axis.
  vs <- as.data.frame(DTAtools::validation_status(
    DTAtools::datasets(dta, "ds1")
  ))
  # expect_equal, not expect_identical: validation_status() returns
  # n_schema_errors as double and n_rule_errors as integer, so an identical
  # check would pin that inconsistency rather than the counts being asserted.
  expect_equal(vs$n_schema_errors, 0)
  expect_equal(vs$n_rule_errors, 0)
  expect_equal(vs$n_import_errors, 1)
  expect_false(vs$ok)

  expect_identical(
    dta_app_fn("dta_table_status_map")(dta, "ds1"),
    c(t1 = "fail")
  )
})


test_that("dta_table_status_map reports a pre-import-axis artifact as unknown", {
  ds <- DTAtools::check(
    app_test_dataset(c("1.5", "2.5")),
    force = TRUE, persist = FALSE, quiet = TRUE
  )

  # Exactly what a validation index written before the import axis existed looks
  # like: the two-axis verdict is kept, the import count is simply not there.
  entry <- ds@validation_index[["t1"]]
  expect_true("n_import_errors" %in% names(entry))
  entry$n_import_errors <- NULL
  ds@validation_index[["t1"]] <- entry

  vs <- as.data.frame(DTAtools::validation_status(ds))
  expect_true(vs$ok)
  expect_true(is.na(vs$n_import_errors))

  dta <- DTAtools::DTA(datasets = ds)
  expect_identical(
    dta_app_fn("dta_table_status_map")(dta, "ds1"),
    c(t1 = "unknown")
  )
})


test_that("dta_table_status_map passes a table that is clean on all axes", {
  dta <- app_test_dta(c("1.5", "2.5"))
  expect_identical(
    dta_app_fn("dta_table_status_map")(dta, "ds1"),
    c(t1 = "pass")
  )
})


test_that("the loaded-file list renders unknown distinctly from pass and fail", {
  app_src <- dta_app_source("app.R")

  # Three distinct icons, three distinct CSS classes: an unknown import axis
  # must not fall through to the pending dash or borrow the pass/fail look.
  expect_match(app_src, 'unknown = "file-unknown"', fixed = TRUE)
  expect_match(app_src, 'unknown = "?"', fixed = TRUE)

  testthat::skip_if_not_installed("shiny")
  css <- as.character(dta_app_fn("bi_css")())
  expect_match(css, ".file-unknown", fixed = TRUE)
  expect_match(css, ".file-ok", fixed = TRUE)
  expect_match(css, ".file-fail", fixed = TRUE)
})


# ---- Task 2: the exported HTML report shows the import axis ---------------

test_that("the HTML validation report carries the import error count", {
  dta <- app_test_dta(c("1.5", "heavy"))
  report <- dta_app_fn("dta_build_validation_report")(dta, list(ds1 = "fail"))

  expect_true(is.character(report))
  # Without the column the report shows two zero counts and no reason at all
  # for the failure it is reporting.
  expect_match(report, "<th>n_import_errors</th>", fixed = TRUE)
  expect_match(report, "<th>n_schema_errors</th>", fixed = TRUE)
  expect_match(report, "<th>n_rule_errors</th>", fixed = TRUE)

  # And the count itself reaches the body, not just the header.
  body <- sub("^.*n_import_errors</th>", "", report)
  expect_match(body, "<td>1</td>", fixed = TRUE)
})


# ---- Task 3: unloading a table drops its import issues --------------------

test_that("dta_unload_table drops the table's import issues", {
  dta <- app_test_dta(c("1.5", "heavy"))
  expect_named(DTAtools::datasets(dta, "ds1")@import_issues, "t1")

  res <- dta_app_fn("dta_unload_table")(dta, "ds1", "t1")
  expect_true(res$ok)

  ds <- DTAtools::datasets(res$value, "ds1")
  expect_length(ds@tables, 0)
  expect_length(ds@validation_index, 0)
  expect_length(ds@validation_store, 0)
  # Stale issues left here would be re-attached to the next file that happens
  # to load under the same table name.
  expect_length(ds@import_issues, 0)
})


test_that("dta_unload_all drops every table's import issues", {
  dta <- app_test_dta(c("1.5", "heavy"))
  res <- dta_app_fn("dta_unload_all")(dta, "ds1")
  expect_true(res$ok)

  ds <- DTAtools::datasets(res$value, "ds1")
  expect_length(ds@tables, 0)
  expect_length(ds@import_issues, 0)
})


test_that("unloading works on a dataset that has no import_issues property", {
  # A file dataset carries no typed tables, so @import_issues does not exist on
  # it. The unload helpers must not assume the property is there.
  has_issues <- dta_app_fn("dta_has_import_issues")
  expect_true(has_issues(app_test_dataset(c("1.5", "2.5"))))
  expect_false(has_issues(DTAtools::DTAMetaData(title = "not a dataset")))
  expect_false(has_issues(NULL))
})


# ---- Task 4: the inspect modal has an import branch -----------------------

test_that("dta_inspect_import_fields reads what inspect() emits for an import message", {
  dta <- app_test_dta(c("1.5", "heavy"))
  ds <- DTAtools::datasets(dta, "ds1")

  msgs <- as.data.frame(DTAtools::messages(ds))
  import_ids <- msgs$id[msgs$source == "import"]
  expect_length(import_ids, 1)

  rec <- as.data.frame(DTAtools::inspect(ds, id = import_ids[[1]]))
  expect_identical(as.character(rec$type[1]), "import")

  fields <- dta_app_fn("dta_inspect_import_fields")(as.list(rec[1, , drop = FALSE]))

  # The three things the modal promises to show: the raw value, the declared
  # type, and the reason it could not be represented.
  expect_identical(fields$raw, "heavy")
  expect_identical(fields$declared_type, "SAS Num")
  expect_identical(fields$reason, "not_convertible")
  expect_identical(fields$column, "VAL")
  expect_identical(fields$row, "2")
})


test_that("dta_inspect_import_fields falls back to the flat message columns", {
  fields <- dta_app_fn("dta_inspect_import_fields")(list(
    column = "AGE",
    row = 7,
    keyword = "not_convertible"
  ))
  expect_identical(fields$column, "AGE")
  expect_identical(fields$row, "7")
  expect_identical(fields$reason, "not_convertible")

  # Nothing available is "" throughout, never NA or NULL -- the modal calls
  # nzchar() on every one of them.
  empty <- dta_app_fn("dta_inspect_import_fields")(list())
  expect_identical(
    empty,
    list(column = "", raw = "", declared_type = "", reason = "", row = "")
  )
  expect_identical(
    dta_app_fn("dta_inspect_import_fields")(
      list(import_column = NA_character_, import_raw = NA_character_)
    )$raw,
    ""
  )
})


test_that("the inspect modal routes import records to their own branch", {
  app_src <- dta_app_source("app.R")

  # app.R cannot be sourced outside a running app, so parse it: this is what
  # catches a syntax error in the branch that was added.
  expect_silent(parse(text = app_src))

  # Before this branch existed an import record fell into the schema `else` and
  # rendered two empty schema_* panels.
  expect_match(app_src, 'identical(typ, "import")', fixed = TRUE)
  expect_match(app_src, 'class = "inspect-badge import"', fixed = TRUE)
  # The axis is taken from `type`, with `source` -- not the rule_id guess -- as
  # the fallback, so an import record can never be mistaken for a schema one.
  expect_match(app_src, '.first_nonempty(r[["type"]], r[["source"]])', fixed = TRUE)

  testthat::skip_if_not_installed("shiny")
  css <- as.character(dta_app_fn("bi_css")())
  # Its own hue, so it never reads as a rule or schema failure.
  expect_match(css, ".inspect-badge.import", fixed = TRUE)
  expect_match(css, ".inspect-badge.rule", fixed = TRUE)
  expect_match(css, ".inspect-badge.schema", fixed = TRUE)
})


# ---- Task 5: metadata import errors reach the metadata editor -------------

test_that("dta_metadata_import_messages surfaces DTA-level metadata errors", {
  # "2026-12-31 at the earliest" parses as a Date only by discarding the
  # qualification, which is recorded as a metadata import error.
  md <- DTAtools::DTAMetaData(
    title = "Qualified Date",
    transmission = list(date_last_transfer = "2026-12-31 at the earliest")
  )
  dta <- DTAtools::DTA(
    metadata = md,
    datasets = DTAtools::check(
      app_test_dataset(c("1.5", "2.5")),
      force = TRUE, persist = FALSE, quiet = TRUE
    )
  )

  found <- dta_app_fn("dta_metadata_import_messages")(dta)
  expect_true(is.data.frame(found))
  expect_equal(nrow(found), 1)
  expect_identical(as.character(found$target), "metadata")
  expect_identical(as.character(found$source), "import")
  expect_identical(
    as.character(found$column),
    "transmission$date_last_transfer"
  )
  expect_true(nzchar(as.character(found$message)))

  # These are DTA-level, so the per-dataset dock -- which reads messages(ds) --
  # cannot show them. That is the whole reason the metadata editor has to.
  ds_msgs <- dta_app_fn("dta_dataset_messages")(dta, "ds1")
  expect_false("metadata" %in% as.character(ds_msgs$target))
})


test_that("clean metadata produces no import notice", {
  dta <- app_test_dta(c("1.5", "2.5"))
  found <- dta_app_fn("dta_metadata_import_messages")(dta)
  expect_equal(nrow(found), 0)
})


test_that("the metadata editor renders the import notice", {
  app_src <- dta_app_source("app.R")
  expect_match(app_src, "dta_metadata_import_messages(dta)", fixed = TRUE)
  expect_match(app_src, 'class = "md-import-warn"', fixed = TRUE)

  testthat::skip_if_not_installed("shiny")
  expect_match(
    as.character(dta_app_fn("bi_css")()),
    ".md-import-warn",
    fixed = TRUE
  )
})
