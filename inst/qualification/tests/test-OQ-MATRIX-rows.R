# Rows, the row key and the dimensions of a matrix. REQ-MATRIX-018 .. REQ-MATRIX-025.
#
# The row-label columns are ordinary column specifications matched to the file
# by position; the row key is unique and never missing; the numbers of data
# columns and data rows have bounds that are exact, a minimum, a maximum or
# both. Every expected value comes from those sentences and from the files
# written here.
#
# Each case runs on the table held in memory (stream = "never") and scanned in
# batches (stream = "always"), except the partial-scan case, which exists only
# for a scanned table.

# ---- row labels --------------------------------------------------------------------

test_that("OQ-MATRIX-016 | row-label columns receive the tabular checks, matched by position | REQ-MATRIX-018", {
  ds <- dta_dataset_from_list(list(
    id = "m", type = "matrix", files = list(filename = "m.tsv", type = "tsv"),
    row_labels = list(
      list(id = "ID", type = "SAS Char", length = 3, nullable = FALSE, pattern = "^r[0-9]$"),
      list(id = "DESC", type = "SAS Char", length = 1, nullable = FALSE)
    ),
    matrix = list(
      header = list(corner = "blank"),
      cells = list(type = "SAS Int", nullable = FALSE)
    )
  ))
  # The header's own text above the row labels says nothing about their ids:
  # file column 1 is ID and column 2 is DESC because of the order alone.
  path <- mx_path(paste0(
    "\t\tS1\n",
    "r1\ta\t1\n", # fine
    "rx\ta\t2\n", # row 2: ID breaks the pattern
    "r3\tcc\t3\n", # row 3: DESC longer than its length of 1
    "r4\t\t4\n" # row 4: DESC missing
  ))
  mx_each(ds, path, function(d, stream) {
    qa_step(
      sprintf("each row-label finding is at its row and its row label (%s)", stream),
      mx_expect("columnspec", c(2, 3, 4), c("ID", "DESC", "DESC"), c("pattern", "maxLength", "type")),
      mx_found(d)
    )
    qa_step(
      sprintf("the columns are the row-label ids then the data label (%s)", stream),
      c("ID", "DESC", "S1"), names(tables(d)[[1]])
    )
  })
})

# ---- the row key -------------------------------------------------------------------

test_that("OQ-MATRIX-017 | a repeated row key is one rule error, rule id .matrix:row_key, and explain_messages() resolves it | REQ-MATRIX-019", {
  ds <- mx_ds()
  path <- mx_path("\tS1\nr1\t1\nr1\t2\nr2\t3\n")
  mx_each(ds, path, function(d, stream) {
    m <- as.data.frame(messages(d))
    st <- validation_status(d)
    qa_step(
      sprintf("one rules-axis finding and nothing on the other axes (%s)", stream),
      c(0L, 1L, 0L),
      c(as.integer(st$n_columnspec_errors), as.integer(st$n_rule_errors), as.integer(st$n_import_errors))
    )
    qa_step(
      sprintf("it is the generated rule (%s)", stream),
      ".matrix:row_key", m$rule_id[m$source == "rule"]
    )
    qa_step(
      sprintf("and the key is unique again once the repeat is gone (%s)", stream),
      TRUE,
      validation_status(mx_checked(ds, mx_path("\tS1\nr1\t1\nr2\t2\n"), stream))$ok
    )
    out <- suppressMessages(explain_messages(d))
    qa_step(
      sprintf("explain_messages() resolves the rule and names the key column (%s)", stream),
      c(TRUE, TRUE),
      c(nrow(out) >= 1L, any(grepl("ID", out$should_be, fixed = TRUE)))
    )
  })
})

test_that("OQ-MATRIX-018 | a composite row key is unique as a combination | REQ-MATRIX-020", {
  ds <- mx_ds(k = 2L, corner = "named", rows = list(key = list("ID", "DESC")), required_labels = TRUE)
  # (a, x) occurs twice; (a, y) and (b, x) are distinct although each id repeats.
  repeated <- mx_path("ID\tDESC\tS1\na\tx\t1\na\ty\t2\nb\tx\t3\na\tx\t4\n")
  mx_each(ds, repeated, function(d, stream) {
    qa_step(
      sprintf("the repeated combination is one error (%s)", stream),
      1L, as.integer(validation_status(d)$n_rule_errors)
    )
  })
  distinct <- mx_path("ID\tDESC\tS1\na\tx\t1\na\ty\t2\nb\tx\t3\nb\ty\t4\n")
  mx_each(ds, distinct, function(d, stream) {
    qa_step(
      sprintf("ids that repeat in distinct combinations are not (%s)", stream),
      TRUE, mh_clean(d)
    )
  })
})

test_that("OQ-MATRIX-019 | a nullable row key is refused and a missing key cell is a type finding | REQ-MATRIX-021", {
  spec <- function(nullable) {
    list(
      id = "m", type = "matrix", files = list(filename = "m.tsv", type = "tsv"),
      row_labels = list(list(id = "ID", type = "SAS Char", nullable = nullable)),
      matrix = list(header = list(corner = "blank"), cells = list(type = "SAS Int", nullable = FALSE))
    )
  }
  qa_step(
    "a key column declared nullable true cannot be built",
    TRUE, mx_is(mx_error(dta_dataset_from_list(spec(TRUE))), "dta_matrix_invalid_key_nullable")
  )
  ds <- dta_dataset_from_list(spec(FALSE))
  mx_each(ds, mx_path("\tS1\nr1\t1\n\t2\n"), function(d, stream) {
    qa_step(
      sprintf("a row with no key is a type finding at that row and the key column (%s)", stream),
      mx_expect("columnspec", 2, "ID", "type"), mx_found(d)
    )
  })
})

# ---- the row count -----------------------------------------------------------------

test_that("OQ-MATRIX-020 | the number of data rows outside rows.count is minItems or maxItems, and a header-only file fails unless the minimum is 0 | REQ-MATRIX-022", {
  two_rows <- mx_path("\tS1\nr1\t1\nr2\t2\n")
  header_only <- mx_path("\tS1\tS2\n")
  no_cell <- function(keyword) mx_expect("columnspec", NA, NA, keyword)

  for (case in list(
    list("min 3 of 2 rows", two_rows, list(count = list(min = 3)), no_cell("minItems")),
    list("max 1 of 2 rows", two_rows, list(count = list(max = 1)), no_cell("maxItems")),
    list("exactly 2 of 2 rows", two_rows, list(count = 2), mx_expect()),
    list("min 2 max 2 of 2 rows", two_rows, list(count = list(min = 2, max = 2)), mx_expect()),
    list("the default for 2 rows", two_rows, NULL, mx_expect()),
    list("the default for a header-only file", header_only, NULL, no_cell("minItems")),
    list("a maximum alone leaves the minimum at 1", header_only, list(count = list(max = 5)), no_cell("minItems")),
    list("min 0 for a header-only file", header_only, list(count = list(min = 0)), mx_expect()),
    list("exactly 0 for a header-only file", header_only, list(count = 0), mx_expect()),
    list("exactly 0 of 2 rows", two_rows, list(count = 0), no_cell("maxItems"))
  )) {
    mx_each(mx_ds(rows = case[[3]]), case[[2]], function(d, stream) {
      qa_step(sprintf("%s (%s)", case[[1]], stream), case[[4]], mx_found(d))
    })
  }
})

test_that("OQ-MATRIX-021 | a scan stopped early reports the row-count check as failed, passed or not_checked by the rows seen, and never raises minItems | REQ-MATRIX-023", {
  # The first label breaks the header pattern, so the very first batch shows a
  # problem and fail_fast stops after it. Four data rows follow.
  text <- "\tbad\tS2\nr1\t1\t2\nr2\t1\t2\nr3\t1\t2\nr4\t1\t2\n"
  run <- function(rows, batch_rows) {
    ds <- mx_ds(header = list(pattern = "^S[0-9]$"), rows = rows)
    d <- mx_checked(ds, mx_path(text), "always", fail_fast = TRUE, batch_rows = batch_rows)
    ck <- mx_checks(d)
    list(
      min = as.character(ck$status[ck$keyword == "minItems"]),
      max = as.character(ck$status[ck$keyword == "maxItems"]),
      found = mx_found(d)
    )
  }

  # One row seen, a minimum of 5: the minimum can neither be said to hold nor
  # to fail.
  r <- run(list(count = list(min = 5)), 1L)
  qa_step("min 5 after one row: not_checked", "not_checked", r$min)
  qa_step("and no minItems finding is raised", 0L, sum(r$found$keyword == "minItems", na.rm = TRUE))
  qa_step("the header finding that stopped the scan is reported", TRUE, "propertyNames" %in% r$found$keyword)

  # One row seen, a minimum of 1: reached.
  r <- run(list(count = list(min = 1)), 1L)
  qa_step("min 1 after one row: passed", "passed", r$min)

  # Two rows seen, a maximum of 1: already exceeded, whatever follows.
  r <- run(list(count = list(max = 1)), 2L)
  qa_step("max 1 after two rows: failed", "failed", r$max)
  qa_step("and a maxItems finding is reported", TRUE, "maxItems" %in% r$found$keyword)

  # One row seen, a maximum of 10: not yet exceeded, and not settled.
  r <- run(list(count = list(max = 10)), 1L)
  qa_step("max 10 after one row: not_checked", "not_checked", r$max)
})

# ---- dimensions ----------------------------------------------------------------------

test_that("OQ-MATRIX-022 | a count is exact as a scalar, a minimum or a maximum as a mapping, and defaults when absent | REQ-MATRIX-024", {
  path <- mx_path("\tS1\tS2\tS3\nr1\t1\t2\t3\nr2\t1\t2\t3\n") # 3 data columns, 2 rows
  none <- mx_expect()
  one <- function(keyword) mx_expect("columnspec", NA, NA, keyword)

  # header.count against 3 data columns
  for (case in list(
    list("3", 3, none), list("{min: 3}", list(min = 3), none),
    list("{max: 3}", list(max = 3), none), list("{min: 1, max: 3}", list(min = 1, max = 3), none),
    list("4", 4, one("minProperties")), list("{min: 4}", list(min = 4), one("minProperties")),
    list("2", 2, one("maxProperties")), list("{max: 2}", list(max = 2), one("maxProperties"))
  )) {
    mx_each(mx_ds(header = list(count = case[[2]])), path, function(d, stream) {
      qa_step(sprintf("header.count %s (%s)", case[[1]], stream), case[[3]], mx_found(d))
    })
  }
  # rows.count against 2 data rows
  for (case in list(
    list("2", 2, none), list("{min: 2}", list(min = 2), none),
    list("{max: 2}", list(max = 2), none), list("3", 3, one("minItems")),
    list("{min: 3}", list(min = 3), one("minItems")), list("1", 1, one("maxItems")),
    list("{max: 1}", list(max = 1), one("maxItems"))
  )) {
    mx_each(mx_ds(rows = list(count = case[[2]])), path, function(d, stream) {
      qa_step(sprintf("rows.count %s (%s)", case[[1]], stream), case[[3]], mx_found(d))
    })
  }
})

test_that("OQ-MATRIX-023 | a count that is fractional, negative, reversed or zero data columns is refused when the dataset is built | REQ-MATRIX-025", {
  build <- function(header = list(), rows = NULL) {
    m <- list(header = c(list(corner = "blank"), header), cells = list(type = "SAS Int"))
    if (!is.null(rows)) m$rows <- rows
    dta_dataset_from_list(list(
      id = "m", type = "matrix", files = list(filename = "m.tsv", type = "tsv"),
      row_labels = list(list(id = "ID", type = "SAS Char", nullable = FALSE)),
      matrix = m
    ))
  }
  refused <- function(expr) mx_is(mx_error(expr), "dta_matrix_count_invalid")

  qa_step("header.count 1.5", TRUE, refused(build(header = list(count = 1.5))))
  qa_step("header.count 0 (no data column is never valid)", TRUE, refused(build(header = list(count = 0))))
  qa_step("header.count with min above max", TRUE, refused(build(header = list(count = list(min = 3, max = 2)))))
  qa_step("header.count negative", TRUE, refused(build(header = list(count = -2))))
  qa_step("rows.count -1", TRUE, refused(build(rows = list(count = -1))))
  qa_step("rows.count 2.5", TRUE, refused(build(rows = list(count = 2.5))))
  qa_step("rows.count with min above max", TRUE, refused(build(rows = list(count = list(min = 5, max = 4)))))
  qa_step("while rows.count 0 is allowed, a header-only file being legal", NULL, mx_error(build(rows = list(count = 0))))
})
