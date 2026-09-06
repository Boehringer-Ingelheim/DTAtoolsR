# Golden oracle for the validation-engine rewrite (P0).
#
# These tests exist to make "the rewrite did not change behaviour" a test
# result rather than a claim. They are split into two deliberately different
# kinds of assertion:
#
#   1. Axis facts  - validity flags, error counts, and the row/column each
#                    error is attributed to. These are the behavioural
#                    contract. They must survive the rewrite unchanged, and a
#                    diff here means a real regression.
#
#   2. Snapshots   - the full flattened error frame, message text included.
#                    Message strings on the column spec axis come from ajv, which
#                    the rewrite deletes, so these snapshots are EXPECTED to
#                    change at P1. Keeping them separate means that churn is
#                    visibly distinct from a behavioural drift.
#
# See benchmarks/bench_validation.R for the cost side of the same work.

# Tag details so the as.data.frame() contract method applies, whether or not
# validate_table_detailed() already returned a tagged object.
vc_flat <- function(details) {
  tagged <- if (inherits(details, "dta_validation_details")) {
    details
  } else {
    dta_as_validation_details(details)
  }
  flat <- as.data.frame(tagged)
  rownames(flat) <- NULL
  flat
}

# The stable half of the contract: verdicts and counts, no free text.
vc_axis_facts <- function(details) {
  data.frame(
    ok = details$ok,
    columnspec_valid = details$columnspec_valid,
    rules_valid = details$rules_valid,
    import_valid = details$import_valid,
    n_columnspec_errors = as.integer(details$n_columnspec_errors),
    n_rule_errors = as.integer(details$n_rule_errors),
    n_import_errors = as.integer(details$n_import_errors),
    stringsAsFactors = FALSE
  )
}

vc_details <- function(case) {
  validate_table_detailed(
    specs = case$specs,
    table = case$table,
    verbose = FALSE
  )
}

# ---- the clean case must be completely quiet --------------------------------

test_that("a table satisfying every constraint reports no errors on any axis", {
  case <- vc_corpus()$clean
  details <- vc_details(case)

  expect_true(details$ok)
  expect_true(details$columnspec_valid)
  expect_true(details$rules_valid)
  expect_true(details$import_valid)
  expect_equal(details$n_columnspec_errors, 0)
  expect_equal(details$n_rule_errors, 0)
  expect_equal(as.integer(details$n_import_errors), 0L)
  expect_equal(nrow(vc_flat(details)), 0)
})

# ---- every other case must fail, and fail visibly ---------------------------

test_that("every corpus violation is detected", {
  corpus <- vc_corpus()
  violations <- corpus[names(corpus) != "clean"]

  for (name in names(violations)) {
    details <- vc_details(violations[[name]])

    # `ok` is the composite three-axis verdict. A case that was built to
    # violate something must not come back clean.
    expect_false(
      isTRUE(details$ok),
      info = paste0("case '", name, "' was expected to fail but reported ok")
    )

    # And the failure must be attributable: at least one error must reach the
    # flattened report a user actually reads.
    expect_gt(nrow(vc_flat(details)), 0)
  }
})

# ---- axis facts, pinned per case --------------------------------------------

test_that("axis verdicts and error counts are stable across the corpus", {
  corpus <- vc_corpus()
  facts <- do.call(
    rbind,
    lapply(names(corpus), function(name) {
      cbind(case = name, vc_axis_facts(vc_details(corpus[[name]])))
    })
  )
  rownames(facts) <- NULL

  expect_snapshot_value(facts, style = "json2")
})

# ---- the error attribution, without message text ----------------------------

test_that("each error is attributed to a stable source, row and column", {
  corpus <- vc_corpus()
  located <- do.call(
    rbind,
    lapply(names(corpus), function(name) {
      flat <- vc_flat(vc_details(corpus[[name]]))
      if (nrow(flat) == 0) {
        return(NULL)
      }
      # Message text is excluded here on purpose; it lives in the snapshot
      # below, which is allowed to churn when ajv is removed.
      cbind(
        case = name,
        flat[, c("source", "rule_id", "row", "column", "keyword")],
        stringsAsFactors = FALSE
      )
    })
  )
  rownames(located) <- NULL

  expect_snapshot_value(located, style = "json2")
})

# ---- full report including message text (expected to churn at P1) -----------

test_that("the flattened validation report is unchanged", {
  corpus <- vc_corpus()
  full <- do.call(
    rbind,
    lapply(names(corpus), function(name) {
      flat <- vc_flat(vc_details(corpus[[name]]))
      if (nrow(flat) == 0) {
        return(NULL)
      }
      cbind(case = name, flat, stringsAsFactors = FALSE)
    })
  )
  rownames(full) <- NULL

  expect_snapshot_value(full, style = "json2")
})

# ---- strict-numeric semantics ------------------------------------------------

test_that("dta_as_numeric_strict separates missing from unconvertible", {
  edges <- vc_numeric_edges()
  got <- dta_as_numeric_strict(edges$input)

  expect_equal(got$values, edges$value)
  expect_equal(got$missing, edges$missing)
  expect_equal(got$unconvertible, edges$unconvertible)

  # The three categories must partition: a value is missing, unconvertible, or
  # usable - never two at once. The rewrite must preserve that invariant even
  # if a specific classification is later corrected.
  expect_false(any(got$missing & got$unconvertible))
})

# ---- the summarised error frame ---------------------------------------------

# `as.data.frame(details)` selects only source/rule_id/row/column/keyword/
# message, so the snapshots above never see `summarised_error`. It is returned
# to users by validate_table(), and its grouping was rewritten along with the
# rest of the column spec axis, so it needs assertions of its own.

test_that("repeated identical violations collapse into one summarised row", {
  specs <- vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE)
  ))
  table <- data.frame(
    ID = c("A001", "TOOLONG", "B002", "TOOLONG"),
    stringsAsFactors = FALSE
  )

  details <- validate_table_detailed(specs = specs, table = table, verbose = FALSE)
  summarised <- details$columnspec_errors$summarised_error

  expect_equal(nrow(summarised), 1)
  expect_equal(summarised$keyword, "maxLength")
  expect_equal(summarised$first.row.affected, 2)
  expect_equal(summarised$last.row.affected, 4)
  expect_equal(summarised$n.rows.affected, 2L)
})

test_that("distinct offending values are summarised separately", {
  specs <- vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE)
  ))
  table <- data.frame(
    ID = c("A001", "TOOLONG", "ALSOTOOLONG"),
    stringsAsFactors = FALSE
  )

  details <- validate_table_detailed(specs = specs, table = table, verbose = FALSE)
  summarised <- details$columnspec_errors$summarised_error

  # Same constraint, different values: two groups, each spanning one row.
  expect_equal(nrow(summarised), 2)
  expect_setequal(summarised$data, c("TOOLONG", "ALSOTOOLONG"))
  expect_true(all(summarised$n.rows.affected == 1L))
})

test_that("a missing column is summarised by constraint, not by row range", {
  # A column absent from every row gains nothing from a row range, so the
  # summary collapses to the distinct constraint and its message.
  details <- vc_details(vc_corpus()$columnspec_required)
  summarised <- details$columnspec_errors$summarised_error

  expect_equal(nrow(summarised), 1)
  expect_equal(summarised$keyword, "required")
  expect_match(summarised$message, "must have required property 'MISSING'", fixed = TRUE)
  expect_false("first.row.affected" %in% names(summarised))
})

# ---- the cost of a structural failure ---------------------------------------

test_that("a missing column costs one column spec error per row, not one per table", {
  # The generated schema is `type: array` with `items.required`, so the
  # validator reports the absent property once for EVERY row rather than once
  # for the table. At 400M rows a single missing column yields 400M error
  # objects, each carrying its own schema and data payload.
  #
  # This is the strongest argument for gating structural checks ahead of any
  # row scan: the answer "column MISSING is absent" is knowable from the header
  # alone, and discovering it per-row is both slower and less useful.
  specs <- vc_corpus()$columnspec_required$specs

  for (n in c(2, 5, 9)) {
    tbl <- data.frame(ID = sprintf("A%03d", seq_len(n)), stringsAsFactors = FALSE)
    details <- validate_table_detailed(specs = specs, table = tbl, verbose = FALSE)
    expect_equal(details$n_columnspec_errors, n)
  }
})

# ---- the reader and coercion path -------------------------------------------

# The corpus above hands a data frame straight to validate_table_detailed(),
# which exercises the validation engine but bypasses the reader and the import
# typing entirely. Those two stages are precisely what P3 replaces, so they
# need their own oracle: the same cases routed through
# read -> dta_coerce_table_to_specs -> as.data.frame, which is the sequence
# an eager load_file() actually performs.
#
# The read goes through dta_read_delim_normalized(), the production eager
# reader, rather than a hand-built arrow::read_csv_arrow() call. The hand-built
# call took its schema from dta_reader_col_types(), which no production path had
# used since the reader plan replaced it and which is now gone; keeping a
# second, test-only reader configuration is how the two paths drifted apart in
# the first place.
vc_roundtrip <- function(case) {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  utils::write.csv(case$table, path, row.names = FALSE, na = "")

  tryCatch(
    {
      tbl <- dta_read_delim_normalized(
        path,
        delim = ",",
        quote = "\"",
        has_header = TRUE,
        specs = case$specs
      )
      coerced <- dta_coerce_table_to_specs(tbl, case$specs)
      details <- validate_table_detailed(
        specs = case$specs,
        table = as.data.frame(coerced$table),
        verbose = FALSE
      )
      cbind(read_ok = TRUE, vc_axis_facts(details))
    },
    error = function(e) {
      # Recorded rather than skipped: a case that cannot survive the read path
      # is itself a fact about the current pipeline worth pinning.
      data.frame(
        read_ok = FALSE,
        ok = NA, columnspec_valid = NA, rules_valid = NA, import_valid = NA,
        n_columnspec_errors = NA_integer_,
        n_rule_errors = NA_integer_,
        n_import_errors = NA_integer_,
        stringsAsFactors = FALSE
      )
    }
  )
}

test_that("a row whose every field is empty is dropped by the CSV reader", {
  # Known reader behaviour, pinned so the streaming rewrite reproduces it
  # rather than diverging by accident.
  #
  # A line with no content at all is treated as a record separator and skipped,
  # which is what essentially every CSV parser does. A row that is merely
  # PARTIALLY empty survives intact, so this is narrow: it costs a row only
  # when every column of that row is absent. The practical consequence is that
  # a file containing all-empty rows validates fewer rows than it contains,
  # with no warning.
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)

  writeLines(c("ID,AGE", "A001,30", "", "B002,50"), path)
  expect_equal(nrow(arrow::read_csv_arrow(path, as_data_frame = FALSE)), 2)

  # The contrast: partially-populated rows are NOT lost. Both a leading and a
  # trailing empty field round-trip, so nullability and uniqueness checks on
  # real multi-column data are unaffected.
  writeLines(c("ID,AGE", "A001,30", ",40", "B002,"), path)
  partial <- arrow::read_csv_arrow(path, as_data_frame = FALSE)
  expect_equal(nrow(partial), 3)
  expect_true(is.na(as.vector(partial$ID)[[2]]))
  expect_true(is.na(as.vector(partial$AGE)[[3]]))
})

test_that("the read and import-typing path produces stable verdicts", {
  corpus <- vc_corpus()
  facts <- do.call(
    rbind,
    lapply(names(corpus), function(name) {
      cbind(case = name, vc_roundtrip(corpus[[name]]))
    })
  )
  rownames(facts) <- NULL

  expect_snapshot_value(facts, style = "json2")
})

test_that("dta_as_numeric_strict never flags typed columns as unconvertible", {
  # Date, POSIXt, numeric and logical inputs take early-return branches that
  # bypass string parsing entirely. Those branches are easy to drop in a
  # rewrite that assumes every column arrives as text.
  expect_false(any(dta_as_numeric_strict(as.Date(c("2026-01-01", NA)))$unconvertible))
  expect_false(any(dta_as_numeric_strict(c(1.5, NA_real_))$unconvertible))
  expect_false(any(dta_as_numeric_strict(c(TRUE, FALSE, NA))$unconvertible))

  # Factors are routed through as.character() first, so a factor of digits must
  # convert rather than yielding its integer codes.
  expect_equal(
    dta_as_numeric_strict(factor(c("10", "20")))$values,
    c(10, 20)
  )
})


# ---- the same data, typed in R rather than read from a file -----------------

# Every case in `vc_corpus()` is a frame of text and numbers, and every
# consumer of that corpus -- here, `test-columnspec-checks.R` and
# `test-streaming-validation.R` -- writes it to a CSV before checking it. So
# the corpus cannot say anything about a table that never passes a reader:
# by the time any of its consumers sees a case, `factor`, `Date`, `POSIXct`
# and `logical` have all become text. That is why the R-typed cases live here
# rather than in the shared helper, where adding them would only have churned
# three other files' expectations without testing the typing.
#
# The claim under test is the one the package makes about constructed tables:
# a table built in R and a file carrying the same values reach the SAME
# verdict. Both sides are reduced to axis facts -- validity flags and error
# counts -- because the message text on the two paths legitimately names
# different things (an R factor level and the string a reader produced from
# it) while the verdict must not differ.

vc_typed_cases <- function() {
  ids <- c("A001", "A002", "TOO-LONG")

  list(
    # A declared Char column held as a factor. The declared type is never
    # coerced, so the engine sees factor levels where the file path sees
    # strings, and the length and codelist checks have to agree anyway.
    factor_char = vc_case(
      "declared Char column held as a factor",
      "columnspec",
      vc_specs(list(
        DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE),
        DTAColumnSpec(
          id = "SEX", type = "SAS Char", length = 1,
          nullable = FALSE, values = c("M", "F")
        )
      )),
      data.frame(
        ID = ids,
        SEX = factor(c("M", "X", "F"), levels = c("M", "F", "X")),
        stringsAsFactors = FALSE
      )
    ),

    # Declared numerics that arrive as R numbers rather than as text: nothing
    # to parse, so the import axis must stay silent while the range rule still
    # fires.
    numeric_typed = vc_case(
      "declared Num and Int columns already typed in R",
      "rule",
      vc_specs(
        list(
          DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE),
          DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE),
          # Whole and far inside the integer range, so the pinned Int
          # narrowing defect cannot be what this case measures.
          DTAColumnSpec(id = "CNT", type = "SAS Int", nullable = TRUE)
        ),
        list(DTARuleColRange(id = "age_range", columns = "AGE", range = c(18, 70)))
      ),
      data.frame(
        ID = c("A001", "A002", "A003"),
        AGE = c(30, 17, 71),
        CNT = c(1L, 2L, 3L),
        stringsAsFactors = FALSE
      )
    ),

    # Columns only R can hold, none of them declared. They are read by nothing,
    # and must still cost exactly what an undeclared text column costs.
    r_only_types = vc_case(
      "undeclared Date, POSIXct and logical columns",
      "columnspec",
      vc_specs(list(
        DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE)
      )),
      data.frame(
        ID = c("A001", "A002"),
        DAY = as.Date(c("2026-01-01", "2026-01-02")),
        WHEN = as.POSIXct(
          c("2026-01-01 10:00:00", "2026-01-02 11:00:00"),
          tz = "UTC"
        ),
        FLAG = c(TRUE, FALSE),
        stringsAsFactors = FALSE
      )
    )
  )
}

test_that("an R-typed table and the same values in a file reach the same verdict", {
  for (name in names(vc_typed_cases())) {
    case <- vc_typed_cases()[[name]]
    from_r <- vc_axis_facts(vc_details(case))
    from_file <- vc_roundtrip(case)

    expect_true(from_file$read_ok, info = name)
    expect_equal(
      from_r,
      from_file[, names(from_r), drop = FALSE],
      ignore_attr = TRUE,
      info = name
    )
  }
})

test_that("the R-typed cases really are typed, and really do exercise every axis", {
  # Without this the test above would compare two frames of text and prove
  # nothing about R typing.
  classes <- unlist(lapply(vc_typed_cases(), function(case) {
    vapply(case$table, function(column) class(column)[[1]], character(1))
  }), use.names = FALSE)
  expect_true(all(
    c("factor", "numeric", "integer", "Date", "POSIXct", "logical") %in% classes
  ))

  facts <- do.call(rbind, lapply(vc_typed_cases(), function(case) {
    vc_axis_facts(vc_details(case))
  }))
  expect_false(all(facts$columnspec_valid))
  expect_false(all(facts$rules_valid))
  expect_true(all(facts$import_valid))
})
