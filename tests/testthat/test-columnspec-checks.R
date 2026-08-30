# Per-check reporting of the column spec axis.
#
# The defect these pin: the axis reported one lumped success line -- "Table
# format, length, pattern, and values are valid" -- and printed NOTHING at all
# when it failed. A reader of a failing run saw the section header, the rules
# passing, and a FAILED verdict with no stated cause on this axis.
#
# Two properties are asserted throughout. First, every check kind reports its
# own verdict, so what passed and what failed is legible. Second, a verdict is
# never invented: a constraint no column declares, a column the table does not
# have, a table with no rows and a scan that stopped early all report as
# something other than a pass.

cc_specs <- function() {
  DTAColumnSpecCollection(
    columns = list(
      ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE),
      SEX = DTAColumnSpec(
        id = "SEX", type = "SAS Char", length = 1,
        nullable = FALSE, values = c("M", "F")
      ),
      CODE = DTAColumnSpec(
        id = "CODE", type = "SAS Char", length = 6,
        nullable = FALSE, pattern = "^[A-Z]{3}[0-9]{3}$"
      )
    )
  )
}

cc_table <- function() {
  data.frame(
    ID = c("A001", "TOO-LONG", "A003"),
    SEX = c("M", "X", "F"),
    CODE = c("ABC123", "ABC124", "ABC125"),
    stringsAsFactors = FALSE
  )
}

# The status of one keyword, by name rather than by row position, so a change to
# the report order does not silently retarget an assertion.
cc_status <- function(checks, keyword) {
  checks$status[match(keyword, checks$keyword)]
}

cc_row <- function(checks, keyword) {
  checks[match(keyword, checks$keyword), , drop = FALSE]
}

# cli wraps long alerts across lines and prefixes them with a symbol, so the
# emitted text is matched against a whitespace-collapsed form rather than
# verbatim. These are package-authored English strings, not translated ones.
cc_messages <- function(expr) {
  msgs <- testthat::capture_messages(expr)
  gsub("\\s+", " ", paste(msgs, collapse = " "))
}


test_that("a failing column spec axis names each check that failed", {
  details <- NULL
  out <- cc_messages(
    details <- validate_table_detailed(cc_specs(), cc_table(), verbose = TRUE)
  )

  # The regression itself: the axis used to print nothing here.
  expect_match(out, "Length check failed", fixed = TRUE)
  expect_match(out, "Values check failed", fixed = TRUE)
  # And it still says what did NOT fail, rather than going silent on the rest.
  expect_match(out, "Pattern check passed", fixed = TRUE)
  expect_match(out, "Presence check passed", fixed = TRUE)
  expect_match(out, "column spec check", fixed = TRUE)

  # The offending columns are named, which is what makes the line actionable.
  expect_match(out, "ID", fixed = TRUE)
  expect_match(out, "SEX", fixed = TRUE)

  expect_false(details$columnspec_valid)
})


test_that("a clean table reports a verdict per check, not one lumped claim", {
  clean <- cc_table()
  clean$ID[2] <- "A002"
  clean$SEX[2] <- "F"

  out <- cc_messages(validate_table_detailed(cc_specs(), clean, verbose = TRUE))

  expect_match(out, "Presence check passed", fixed = TRUE)
  expect_match(out, "Format check passed", fixed = TRUE)
  expect_match(out, "Length check passed", fixed = TRUE)
  expect_match(out, "Values check passed", fixed = TRUE)
  expect_match(out, "Pattern check passed", fixed = TRUE)
  expect_match(out, "All column spec checks passed", fixed = TRUE)

  # The old lumped line asserted four checks at once, including checks the
  # specs may never have declared.
  expect_false(grepl("format, length, pattern, and values", out, fixed = TRUE))
})


test_that("each check carries its own status, count and failing columns", {
  details <- validate_table_detailed(cc_specs(), cc_table(), verbose = FALSE)
  checks <- details$columnspec_checks

  expect_equal(cc_status(checks, "maxLength"), "failed")
  expect_equal(cc_status(checks, "enum"), "failed")
  expect_equal(cc_status(checks, "required"), "passed")
  expect_equal(cc_status(checks, "type"), "passed")
  expect_equal(cc_status(checks, "pattern"), "passed")
  # Nothing declares a single permitted value, so this check never ran -- which
  # is not the same as running and passing.
  expect_equal(cc_status(checks, "const"), "not_applicable")

  expect_equal(cc_row(checks, "maxLength")$n_errors, 1L)
  expect_equal(cc_row(checks, "maxLength")$failed_columns, "ID")
  expect_equal(cc_row(checks, "enum")$failed_columns, "SEX")
  expect_true(is.na(cc_row(checks, "type")$failed_columns))

  # Three columns declare a length, one declares a value list, one a pattern.
  expect_equal(cc_row(checks, "maxLength")$columns_declared, 3L)
  expect_equal(cc_row(checks, "enum")$columns_declared, 1L)
  expect_equal(cc_row(checks, "pattern")$columns_declared, 1L)
})


test_that("the per-check counts account for every reported violation", {
  details <- validate_table_detailed(cc_specs(), cc_table(), verbose = FALSE)

  expect_equal(
    sum(details$columnspec_checks$n_errors),
    details$n_columnspec_errors
  )
})


test_that("a table with no rows still has its structure checked", {
  # An empty table is a legitimate result -- an analysis that yielded nothing --
  # but only if it carries the columns its specs declare. Presence is decidable
  # from the column names alone and is therefore settled; the value checks are
  # properties of values, of which there are none.
  details <- validate_table_detailed(cc_specs(), cc_table()[0, ], verbose = FALSE)
  checks <- details$columnspec_checks

  expect_equal(cc_status(checks, "required"), "passed")
  expect_true(details$columnspec_valid)
  expect_true(details$ok)

  expect_equal(cc_status(checks, "type"), "not_checked")
  expect_equal(cc_status(checks, "maxLength"), "not_checked")

  out <- cc_messages(validate_table_detailed(cc_specs(), cc_table()[0, ], verbose = TRUE))
  expect_match(out, "Presence check passed", fixed = TRUE)
  expect_match(out, "the table has no rows", fixed = TRUE)
})


test_that("a table that is empty AND missing a declared column fails", {
  # The gap this closes: dta_columnspec_errors() used to return before the
  # presence check whenever the table had no rows, so a table that was both
  # empty and missing half its declared columns reported completely clean --
  # the one case where "empty" must not mean "nothing to check".
  empty_missing <- cc_table()[0, c("ID", "SEX"), drop = FALSE]
  details <- validate_table_detailed(cc_specs(), empty_missing, verbose = FALSE)

  expect_false(details$columnspec_valid)
  expect_false(details$ok)
  expect_equal(cc_status(details$columnspec_checks, "required"), "failed")
  expect_equal(cc_row(details$columnspec_checks, "required")$failed_columns, "CODE")

  # Reported once, about the table, rather than once per row -- there are no
  # rows to attach it to, so the row is NA, as it is for a structural finding.
  full_error <- details$columnspec_errors$full_error
  expect_equal(nrow(full_error), 1L)
  expect_true(is.na(full_error$row))
  expect_equal(full_error$keyword, "required")
  expect_equal(full_error$columnspec, "CODE")

  out <- cc_messages(validate_table_detailed(cc_specs(), empty_missing, verbose = TRUE))
  expect_match(out, "Presence check failed", fixed = TRUE)
})


test_that("an empty stream has its structure checked too", {
  # The streaming driver never enters the batch loop for an empty stream, so the
  # presence check has to come from the source's column names.
  reader <- function(tbl) dta_as_batch_reader(arrow::as_arrow_table(tbl), batch_rows = 8L)

  well_formed <- dta_validate_any_table(
    cc_specs(), reader(cc_table()[0, ]),
    verbose = FALSE
  )
  expect_true(well_formed$ok)
  expect_equal(cc_status(well_formed$columnspec_checks, "required"), "passed")
  expect_equal(cc_status(well_formed$columnspec_checks, "type"), "not_checked")

  missing_column <- dta_validate_any_table(
    cc_specs(), reader(cc_table()[0, c("ID", "SEX"), drop = FALSE]),
    verbose = FALSE
  )
  expect_false(missing_column$ok)
  expect_equal(missing_column$n_columnspec_errors, 1L)
  expect_equal(cc_status(missing_column$columnspec_checks, "required"), "failed")
})


test_that("presence stays unsettled when the column names cannot be read", {
  # The driver called without the source's names cannot decide presence, and
  # says so rather than guessing either way.
  details <- dta_validate_table_stream(
    cc_specs(),
    dta_as_batch_reader(arrow::as_arrow_table(cc_table()[0, ]), batch_rows = 8L),
    verbose = FALSE, coerce = FALSE
  )

  expect_equal(cc_status(details$columnspec_checks, "required"), "not_checked")
  expect_false(any(details$columnspec_checks$status == "passed"))
})


test_that("checks on an absent column are not reported as passed", {
  # CODE is the only column declaring a pattern, and the table lacks it.
  without_code <- cc_table()[, c("ID", "SEX"), drop = FALSE]
  details <- validate_table_detailed(cc_specs(), without_code, verbose = FALSE)
  checks <- details$columnspec_checks

  expect_equal(cc_status(checks, "required"), "failed")
  expect_equal(cc_row(checks, "required")$failed_columns, "CODE")

  # Its type, length and pattern were undefined, not satisfied.
  expect_equal(cc_status(checks, "pattern"), "not_checked")
  expect_equal(cc_row(checks, "pattern")$columns_checked, 0L)
  expect_equal(cc_row(checks, "pattern")$columns_declared, 1L)

  # The checks that other columns could still be judged on report the real
  # denominator rather than counting the absent column as one that passed.
  expect_equal(cc_row(checks, "type")$columns_declared, 3L)
  expect_equal(cc_row(checks, "type")$columns_checked, 2L)
  expect_equal(cc_status(checks, "type"), "passed")
})


test_that("the report renders a count past the integer range", {
  # The streaming path exists for files past `.Machine$integer.max` rows, where
  # one missing column is one error PER ROW. `cli::qty()` coerces to integer, so
  # such a count arrived as NA and cli aborted the report with "Multiple
  # quantities for pluralization" -- after a scan that had already run for
  # hours. `dta_format_count()` covers the other half: an unrendered double
  # prints as `3.2e+09`.
  schemas <- list(
    list(name = "A", schema = list(type = "string", maxLength = 4)),
    list(name = "B", schema = list(type = "string", maxLength = 4))
  )
  tally <- dta_empty_columnspec_tally()
  tally$n_errors[["maxLength"]] <- 3.2e9
  tally$columns[["maxLength"]] <- "A"

  checks <- dta_columnspec_check_summary(schemas, tally = tally)
  expect_type(checks$n_errors, "double")

  out <- cc_messages(expect_no_error(dta_report_columnspec_checks(checks)))
  expect_match(out, "3200000000 values", fixed = TRUE)
  expect_false(grepl("e+09", out, fixed = TRUE))
})


test_that("the report pluralises the value count it prints", {
  schemas <- list(list(name = "A", schema = list(type = "string", maxLength = 4)))
  tally_for <- function(n) {
    tally <- dta_empty_columnspec_tally()
    tally$n_errors[["maxLength"]] <- n
    tally$columns[["maxLength"]] <- "A"
    tally
  }

  one <- cc_messages(dta_report_columnspec_checks(
    dta_columnspec_check_summary(schemas, tally = tally_for(1))
  ))
  many <- cc_messages(dta_report_columnspec_checks(
    dta_columnspec_check_summary(schemas, tally = tally_for(3))
  ))

  expect_match(one, "1 value in", fixed = TRUE)
  expect_match(many, "3 values in", fixed = TRUE)
})


test_that("a spec collection declaring nothing reports no check as passed", {
  empty <- DTAColumnSpecCollection(columns = list())
  checks <- dta_columnspec_check_summary(dta_compile_columnspec_schemas(empty))

  expect_true(all(checks$status == "not_applicable"))

  out <- cc_messages(dta_report_columnspec_checks(checks))
  expect_match(out, "No column spec check ran", fixed = TRUE)
})


test_that("the streaming path reports the same per-check summary as the eager one", {
  # The property the streaming path has to earn everywhere: not "it reports
  # something" but "it reports exactly what the materialising path reports".
  for (case in vc_corpus()) {
    eager <- validate_table_detailed(case$specs, case$table, verbose = FALSE)
    streamed <- dta_validate_table_stream(
      case$specs,
      dta_as_batch_reader(arrow::as_arrow_table(case$table), batch_rows = 1L),
      verbose = FALSE, coerce = FALSE
    )

    expect_equal(
      streamed$columnspec_checks,
      eager$columnspec_checks,
      info = case$label
    )
  }
})


test_that("streamed per-check counts survive the retained-error cap", {
  reader <- function() {
    dta_as_batch_reader(arrow::as_arrow_table(cc_table()), batch_rows = 1L)
  }

  uncapped <- dta_validate_table_stream(
    cc_specs(), reader(),
    verbose = FALSE, coerce = FALSE, max_errors = NULL
  )
  capped <- dta_validate_table_stream(
    cc_specs(), reader(),
    verbose = FALSE, coerce = FALSE, max_errors = 1L
  )

  # The cap spills rows out of the frame held in memory. It must not reach the
  # verdict: a check whose only violations were truncated would otherwise be
  # reported as passed.
  expect_lt(
    nrow(capped$columnspec_errors$full_error),
    nrow(uncapped$columnspec_errors$full_error)
  )
  expect_equal(capped$columnspec_checks, uncapped$columnspec_checks)
  expect_equal(cc_status(capped$columnspec_checks, "maxLength"), "failed")
  expect_equal(cc_status(capped$columnspec_checks, "enum"), "failed")
})


test_that("a scan that stopped early settles only the checks that failed", {
  streamed <- dta_validate_table_stream(
    cc_specs(),
    dta_as_batch_reader(arrow::as_arrow_table(cc_table()), batch_rows = 1L),
    verbose = FALSE, coerce = FALSE, fail_fast = TRUE
  )
  checks <- streamed$columnspec_checks

  # A found error is certain; an absent error, on a scan that stopped at the
  # first problem, only means no later batch was read.
  expect_equal(cc_status(checks, "maxLength"), "failed")
  expect_equal(cc_status(checks, "enum"), "failed")
  expect_equal(cc_status(checks, "required"), "not_checked")
  expect_equal(cc_status(checks, "type"), "not_checked")
  expect_false(any(checks$status == "passed"))
})


test_that("a folded check names the reason its own rows were not checked", {
  # `enum` and `const` print as one "values" line. They can differ: here the
  # column declaring a value list is absent while the one declaring a constant
  # is present and passes. Reading the reason off the folded line rather than
  # off the unchecked rows reported "the table has no rows" for a table with
  # three of them.
  specs <- DTAColumnSpecCollection(columns = list(
    A = DTAColumnSpec(
      id = "A", type = "SAS Char", length = 1,
      nullable = FALSE, values = c("X", "Y")
    ),
    B = DTAColumnSpec(
      id = "B", type = "SAS Char", length = 1,
      nullable = FALSE, values = "Z"
    )
  ))
  tab <- data.frame(B = c("Z", "Z", "Z"), stringsAsFactors = FALSE)

  details <- validate_table_detailed(specs, tab, verbose = FALSE)
  expect_equal(cc_status(details$columnspec_checks, "enum"), "not_checked")
  expect_equal(cc_status(details$columnspec_checks, "const"), "passed")

  out <- cc_messages(validate_table_detailed(specs, tab, verbose = TRUE))
  expect_match(out, "Values check not checked: no column that declares it is present", fixed = TRUE)
  expect_false(grepl("the table has no rows", out, fixed = TRUE))
  # Only one of the two folded checks was left unchecked.
  expect_match(out, "(1 declaring column)", fixed = TRUE)
})


test_that("a structural early return reports presence only", {
  specs <- cc_specs()
  findings <- dta_structure_findings(specs, c("ID", "SEX"))
  details <- dta_structural_failure_details(
    findings,
    schemas = dta_compile_columnspec_schemas(specs)
  )
  checks <- details$columnspec_checks

  expect_equal(cc_status(checks, "required"), "failed")
  # Not one row was read, so nothing else has a verdict to report.
  expect_false(any(checks$status == "passed"))
  expect_equal(cc_status(checks, "type"), "not_checked")
})


test_that("stopping on a missing column still says what was not checked", {
  # on_missing_column = "stop" decides from the header and reads nothing. That
  # early return used to print only "Missing required column(s)", leaving a
  # reader to guess whether the other checks had run and passed.
  table <- arrow::as_arrow_table(cc_table()[, c("ID", "SEX"), drop = FALSE])

  out <- cc_messages(
    details <- dta_validate_any_table(
      cc_specs(), table,
      verbose = TRUE, on_missing_column = "stop"
    )
  )

  expect_match(out, "Presence check failed", fixed = TRUE)
  expect_match(out, "Format check not checked", fixed = TRUE)
  expect_match(out, "the table was not read", fixed = TRUE)
  expect_false(any(details$columnspec_checks$status == "passed"))
})


test_that("check() names the failing column spec checks for a whole dataset", {
  ds <- DTADataSetTabular(
    name = "cc",
    specs = cc_specs(),
    tables = list(tab = cc_table())
  )

  out <- cc_messages(ds <- check(ds, persist = FALSE, quiet = FALSE))

  expect_match(out, "Length check failed", fixed = TRUE)
  expect_match(out, "Values check failed", fixed = TRUE)
  expect_equal(results(ds)$status, "failed")

  # And the breakdown reaches the stored result, not only the console.
  stored <- validation_errors(ds, table = "tab", source = "memory")
  expect_equal(cc_status(stored$columnspec_checks, "maxLength"), "failed")
  expect_equal(cc_status(stored$columnspec_checks, "pattern"), "passed")
})
