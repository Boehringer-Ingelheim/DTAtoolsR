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

# ---- hand-derived companions to the two snapshots above ---------------------
#
# The snapshots above pin whatever validate_table_detailed() currently
# returns. Nothing stops a wrong count or a mis-attributed row from being
# accepted the first time snapshot_accept() runs, because the expectation is
# CAPTURED from a run rather than checked against one. The tests below assert
# the same axis facts and error attribution against values worked out by hand
# from how each fixture in helper-validation-corpus.R is built -- see the
# comment on each corpus case for what it is designed to violate and why.

test_that("columnspec-axis errors are pinned to the row, column and keyword the fixture implies", {
  # Every expectation below is read off the fixture in
  # helper-validation-corpus.R and traced by hand through
  # dta_check_column_spec() (R/columnSpecChecks.R) -- not observed from a run.
  expected <- list(
    # MISSING is absent from the table entirely. A missing required column is
    # an OBJECT-level finding: dta_columnspec_errors() reports it once per row
    # of the 2-row table (R/columnSpecChecks.R), and -- because it is a fact
    # about the table's shape rather than a cell -- it carries no `column`,
    # only the constraint name inside `message`.
    columnspec_required = list(row = c(1L, 2L), column = NA_character_, keyword = "required"),

    # AGE is declared "SAS Num" (JSON type "number"), but the fixture hands it
    # to the validator as an R CHARACTER column: c("30", "not-a-number"). The
    # type check classifies a column by its R class as a WHOLE, not value by
    # value, so both rows fail -- including "30", which merely looks numeric.
    columnspec_type = list(row = c(1L, 2L), column = "AGE", keyword = "type"),

    # ID allows 4 characters. "A001" is exactly 4 (fine); "TOO-LONG" is 8, so
    # only row 2 fails.
    columnspec_maxlength = list(row = 2L, column = "ID", keyword = "maxLength"),

    # SEX permits c("M", "F") -- 2 values, so the generated schema uses "enum"
    # (a single permitted value uses "const" instead; see DTAColumnSpec's
    # as_json_schema() method). "M" is allowed, "X" is not.
    columnspec_enum = list(row = 2L, column = "SEX", keyword = "enum"),

    # CODE must match 3 letters then 3 digits. "ABC123" matches; "bad!!!" does
    # not. Both strings are exactly 6 characters -- CODE's declared length --
    # so this is not also a maxLength hit.
    columnspec_pattern = list(row = 2L, column = "CODE", keyword = "pattern"),

    # ID is non-nullable, so its allowed JSON type is "string" alone -- "null"
    # is added to the type list ONLY when nullable = TRUE (see
    # as_json_schema_type() for DTAColumnSpec). Row 2's NA therefore fails the
    # TYPE check, not a dedicated "required"/"nullable" keyword: "type" is
    # what this case actually produces despite its name.
    columnspec_nullable = list(row = 2L, column = "ID", keyword = "type")
  )

  corpus <- vc_corpus()
  for (case_name in names(expected)) {
    want <- expected[[case_name]]
    details <- vc_details(corpus[[case_name]])

    expect_false(details$columnspec_valid, info = case_name)
    expect_true(details$rules_valid, info = case_name)
    expect_true(details$import_valid, info = case_name)
    expect_equal(details$n_columnspec_errors, length(want$row), info = case_name)
    expect_equal(details$n_rule_errors, 0, info = case_name)
    expect_equal(details$n_import_errors, 0, info = case_name)

    flat <- vc_flat(details)
    expect_equal(flat$row, want$row, info = case_name)
    expect_equal(flat$column, rep(want$column, length(want$row)), info = case_name)
    expect_equal(flat$keyword, rep(want$keyword, length(want$row)), info = case_name)
  }
})

test_that("rule-axis errors count failed rules, not failed rows, and carry no row or column", {
  # n_rule_errors is length(Filter(!valid, rule_results)) in
  # validate_table_detailed() (R/validationFunctions.R): one count per RULE
  # OBJECT that failed, regardless of how many rows within it were bad. And
  # every rule-sourced row of as.data.frame() sets row/column to NA
  # unconditionally (R/validationFunctions.R, as.data.frame.dta_validation_details)
  # -- only `rule_id` identifies which rule failed, because a rule violation is
  # a claim about a whole column or a whole group, not about one cell.
  cases <- list(
    # AGE = c(18, 70, 17, 71) against range [18, 70]. 18 and 70 sit on the
    # inclusive bounds (fine); 17 and 71 do not -- 2 rows violate, but it is
    # still exactly ONE rule that failed.
    age_range = "rule_range",
    # (SUBJ, VISIT) = (A,V1), (A,V1), (B,V1). Rows 1 and 2 collide, so 1
    # duplicate -- still one failed rule.
    subj_visit = "rule_unique",
    # IF AGE >= 18 THEN STATUS == "OK". Row 1 (AGE 20, OK) holds; row 2 (AGE
    # 20, BAD) breaks the THEN; row 3 (AGE 10) never triggers the IF. One
    # violating row is still one failed rule.
    adult_status = "rule_condition",
    # Group SUBJ="A" (rows 1-2) has REASND="BROKEN" in row 1 (satisfies
    # "failed") AND a row with REASND empty plus ORRES populated in row 2
    # (satisfies "reported") -- both hold for the SAME group, which is exactly
    # what "mutually_exclusive" forbids. Group SUBJ="B" (row 3) only satisfies
    # "reported". One group violates -- one failed rule.
    grp_exclusive = "rule_group_exclusive",
    # Group SUBJ="A" satisfies "failed" (REASND="BROKEN"), but STAT="DONE"
    # never satisfies "not_done" ("NOT DONE") anywhere in the group, so the
    # implication breaks. Group SUBJ="B" never satisfies "failed" at all, so
    # the implication holds vacuously. One group violates -- one failed rule.
    grp_requires = "rule_group_requires"
  )

  corpus <- vc_corpus()
  for (rule_id in names(cases)) {
    case_name <- cases[[rule_id]]
    details <- vc_details(corpus[[case_name]])

    expect_true(details$columnspec_valid, info = case_name)
    expect_false(details$rules_valid, info = case_name)
    expect_true(details$import_valid, info = case_name)
    expect_equal(details$n_columnspec_errors, 0, info = case_name)
    expect_equal(details$n_rule_errors, 1, info = case_name)
    expect_equal(details$n_import_errors, 0, info = case_name)

    flat <- vc_flat(details)
    expect_equal(flat$rule_id, rule_id, info = case_name)
    expect_equal(flat$row, NA_integer_, info = case_name)
    expect_equal(flat$column, NA_character_, info = case_name)
  }
})

test_that("a text-typed numeric column fails all three axes when validated without the reader", {
  # VAL is declared "SAS Num" but the fixture hands the validator an R
  # CHARACTER column: c("10", "abc", "", NA) -- exactly the same shape as
  # columnspec_type above, and that is not incidental to the import axis this
  # case is named for.
  #
  #   columnspec: every non-NA value ("10", "abc", "") is the wrong JSON type
  #   for a number column -- rows 1-3. Row 4 is a genuine NA, and VAL is
  #   nullable = TRUE, so it is not flagged.
  #
  #   rule: dta_as_numeric_strict() converts "10" cleanly (10, inside [0,
  #   100]) and treats "" and NA as MISSING (ignored by the range check, per
  #   "Range rules evaluate inclusive bounds and ignore missing values" in
  #   test-evaluateRules.R) -- but "abc" is UNCONVERTIBLE, and an
  #   unconvertible value counts as a violation, not a silently-ignored
  #   missing one (see "Range rules treat an unconvertible value as a
  #   violation" in the same file). So val_range fails on "abc" alone.
  #
  #   import: the same "abc" is the one value dta_rule_import_errors() cannot
  #   represent as a number.
  #
  # This is a property of testing the corpus directly against
  # validate_table_detailed(), which never types the table. The read-path test
  # below shows the SAME fixture, read and coerced through the real pipeline,
  # failing on the import axis ALONE -- which is what the case's name
  # actually promises.
  details <- vc_details(vc_corpus()$import_unconvertible)
  expect_false(details$columnspec_valid)
  expect_false(details$rules_valid)
  expect_false(details$import_valid)
  expect_equal(details$n_columnspec_errors, 3)
  expect_equal(details$n_rule_errors, 1)
  expect_equal(details$n_import_errors, 1)

  flat <- vc_flat(details)
  columnspec_rows <- flat[flat$source == "columnspec", ]
  expect_equal(columnspec_rows$row, c(1L, 2L, 3L))
  expect_equal(columnspec_rows$column, rep("VAL", 3))
  expect_equal(columnspec_rows$keyword, rep("type", 3))

  rule_row <- flat[flat$source == "rule", ]
  expect_equal(rule_row$rule_id, "val_range")
  expect_equal(rule_row$row, NA_integer_)
  expect_equal(rule_row$column, NA_character_)

  import_row <- flat[flat$source == "import", ]
  expect_equal(import_row$row, 2L)
  expect_equal(import_row$column, "VAL")
  expect_equal(import_row$keyword, "not_convertible")
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

# ---- hand-derived companion to the read-path snapshot above -----------------

test_that("the read and coercion pipeline reproduces the direct-path verdict for already-typed cases", {
  # vc_roundtrip() (defined above) writes the fixture to CSV and reads it back
  # through dta_read_delim_normalized() + dta_coerce_table_to_specs() -- the
  # real pipeline validate_table() sees via load_file(). Where the fixture's R
  # values already match their declared type (a numeric column held as real R
  # numbers, a character column holding ordinary non-empty text), coercion has
  # nothing to fix, so the roundtrip should reproduce the direct-path facts
  # derived in the tests above exactly.
  expected <- list(
    clean = list(columnspec_valid = TRUE, rules_valid = TRUE, import_valid = TRUE),
    rule_range = list(columnspec_valid = TRUE, rules_valid = FALSE, import_valid = TRUE),
    columnspec_maxlength = list(columnspec_valid = FALSE, rules_valid = TRUE, import_valid = TRUE)
  )

  corpus <- vc_corpus()
  for (case_name in names(expected)) {
    want <- expected[[case_name]]
    got <- vc_roundtrip(corpus[[case_name]])

    expect_true(got$read_ok, info = case_name)
    expect_equal(got$columnspec_valid, want$columnspec_valid, info = case_name)
    expect_equal(got$rules_valid, want$rules_valid, info = case_name)
    expect_equal(got$import_valid, want$import_valid, info = case_name)
  }
})

test_that("the read and coercion pipeline changes the verdict when the fixture itself is not yet typed", {
  # columnspec_type and import_unconvertible both hand the validator a
  # CHARACTER column against a declared Num spec (see the direct-path tests
  # above). Through the real pipeline that text is coerced to numbers BEFORE
  # validation runs, which resolves the whole-column type mismatch -- so the
  # axis facts genuinely differ from the direct path here, not merely the
  # message text the file header warns will churn.

  # Direct path: 2 columnspec errors (both rows are the wrong JSON type),
  # import_valid = TRUE (nothing was ever typed, so nothing was recorded as
  # unrepresentable). After coercion: "30" converts cleanly, "not-a-number"
  # does not and becomes NA plus one carried import issue. AGE is declared
  # non-nullable, so that NA is now the ONE remaining columnspec error
  # (failing "type" because null is not allowed there) rather than two
  # whole-column mismatches -- and the import axis, clean on the direct path,
  # now fails because coercion recorded the value it could not represent.
  type_rt <- vc_roundtrip(vc_corpus()$columnspec_type)
  expect_true(type_rt$read_ok)
  expect_false(type_rt$columnspec_valid)
  expect_true(type_rt$rules_valid)
  expect_false(type_rt$import_valid)
  expect_equal(type_rt$n_columnspec_errors, 1)
  expect_equal(type_rt$n_rule_errors, 0)
  expect_equal(type_rt$n_import_errors, 1)

  # Direct path: fails all 3 axes (see above). After coercion, VAL becomes a
  # genuine numeric column: "10" -> 10, "abc" -> NA (the one import issue).
  # The row that was originally an R NA is dropped before coercion even runs:
  # VAL is this case's only column, so a missing VAL is an entirely blank CSV
  # line, and "a row whose every field is empty is dropped by the CSV reader"
  # (pinned above). The empty-STRING row survives that same read, because
  # write.csv() quotes it as `""` rather than leaving a blank line -- only 3
  # of the original 4 rows reach coercion. VAL is nullable, so neither
  # remaining NA fails the columnspec axis, and rule_check_range() ignores
  # missing values -- so once the text is properly typed, only the import
  # axis fails, which is what this case's name and axis label actually claim.
  import_rt <- vc_roundtrip(vc_corpus()$import_unconvertible)
  expect_true(import_rt$read_ok)
  expect_true(import_rt$columnspec_valid)
  expect_true(import_rt$rules_valid)
  expect_false(import_rt$import_valid)
  expect_equal(import_rt$n_columnspec_errors, 0)
  expect_equal(import_rt$n_rule_errors, 0)
  expect_equal(import_rt$n_import_errors, 1)
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
