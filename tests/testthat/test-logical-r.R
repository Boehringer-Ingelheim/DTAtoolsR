# A column declared `R logical`: the eight tokens, what import does with them,
# how a rule compares an operand with the typed column, and that the streamed
# scan reaches the same verdicts as the in-memory one.
#
# Where a test puts a rule on the logical column it builds the collection with
# unchecked_collection(), as test-streaming-parity.R does: the rule lint decides
# which operands a column may be given, and the evaluator -- what these tests are
# about -- must give the right answer for every operand it can be handed.

# ---- the token table --------------------------------------------------------

test_that("dta_as_logical_strict reads exactly the eight tokens", {
  true_tokens <- c("TRUE", "T", "true", "True")
  false_tokens <- c("FALSE", "F", "false", "False")

  as_true <- dta_as_logical_strict(true_tokens)
  expect_identical(as_true$values, rep(TRUE, 4L))
  expect_identical(as_true$missing, rep(FALSE, 4L))
  expect_identical(as_true$unconvertible, rep(FALSE, 4L))

  as_false <- dta_as_logical_strict(false_tokens)
  expect_identical(as_false$values, rep(FALSE, 4L))
  expect_identical(as_false$missing, rep(FALSE, 4L))
  expect_identical(as_false$unconvertible, rep(FALSE, 4L))
})

test_that("dta_as_logical_strict refuses everything that is not a token", {
  # Case-sensitive, exact, and the token itself is never trimmed. The text `NA`
  # is a value that is not a token, not a missing marker.
  refused <- c(
    "NA", "yes", "no", "Y", "N", "1", "0", "-1", "2", "tRuE", "TRue", "fALSE",
    "FALSE0", " TRUE", "TRUE ", "T ", " F", "\tT", "TRUE\n", "t", "f", "On",
    "TRUE,", "T;", "NaN", "nan", "null", "x"
  )
  out <- dta_as_logical_strict(refused)

  expect_identical(out$values, rep(NA, length(refused)))
  expect_identical(out$unconvertible, rep(TRUE, length(refused)))
  expect_identical(out$missing, rep(FALSE, length(refused)))
})

test_that("blank, whitespace-only and NA input is missing, never unconvertible", {
  blanks <- c("", " ", "   ", "\t", " \t ", NA_character_)
  out <- dta_as_logical_strict(blanks)

  expect_identical(out$values, rep(NA, length(blanks)))
  expect_identical(out$missing, rep(TRUE, length(blanks)))
  expect_identical(out$unconvertible, rep(FALSE, length(blanks)))
})

test_that("dta_as_logical_strict keeps the shape of its numeric sibling", {
  x <- c("T", "yes", "", NA, "false")
  out <- dta_as_logical_strict(x)

  expect_identical(out$values, c(TRUE, NA, NA, NA, FALSE))
  expect_identical(out$missing, c(FALSE, FALSE, TRUE, TRUE, FALSE))
  expect_identical(out$unconvertible, c(FALSE, TRUE, FALSE, FALSE, FALSE))
  # The raw text of row `i` is read back off `source`, as for numbers.
  expect_identical(out$source, x)
  expect_identical(dta_numeric_raw(out, 2L), "yes")

  # An already logical vector comes back as it is; a zero-length one stays so.
  logical_in <- c(TRUE, NA, FALSE)
  as_is <- dta_as_logical_strict(logical_in)
  expect_identical(as_is$values, logical_in)
  expect_identical(as_is$missing, c(FALSE, TRUE, FALSE))
  expect_identical(as_is$unconvertible, rep(FALSE, 3L))
  expect_identical(dta_as_logical_strict(character(0))$values, logical(0))

  # A factor is read through its labels.
  expect_identical(dta_as_logical_strict(factor(c("F", "T", "x")))$values, c(FALSE, TRUE, NA))
})

test_that("dta_as_logical_token is the values of the strict reading", {
  x <- c("TRUE", "F", "True", "yes", "", NA, " T")

  expect_identical(dta_as_logical_token(x), c(TRUE, FALSE, TRUE, NA, NA, NA, NA))
  expect_identical(dta_as_logical_token(x), dta_as_logical_strict(x)$values)
  expect_identical(dta_as_logical_token(c(TRUE, NA)), c(TRUE, NA))
})

# ---- the import message -----------------------------------------------------

test_that("the import message for an unconvertible logical names value, column and type", {
  issues <- data.frame(
    row = 2L, column = "FLAG", raw = "yes",
    declared_type = "R logical", reason = "not_convertible",
    stringsAsFactors = FALSE
  )

  message <- dta_import_error_messages(issues)

  expect_length(message, 1L)
  expect_match(message, "'yes'", fixed = TRUE)
  expect_match(message, "'FLAG'", fixed = TRUE)
  expect_match(message, "'R logical'", fixed = TRUE)
  expect_match(message, "imported as NA", fixed = TRUE)
})

# ---- rules: an operand against a logical column -----------------------------

lg_frame <- function() {
  data.frame(
    ID = letters[1:6],
    FLAG = c(TRUE, FALSE, NA, TRUE, FALSE, NA),
    stringsAsFactors = FALSE
  )
}

lg_mask <- function(clause, df = lg_frame()) {
  evaluate_conditions(list(FLAG = clause), df)
}

test_that("equals and in read a text token against a logical column", {
  # Rows 1 and 4 hold TRUE, rows 2 and 5 FALSE, rows 3 and 6 are missing.
  expect_identical(lg_mask(list(equals = "T")), c(TRUE, FALSE, NA, TRUE, FALSE, NA))
  expect_identical(lg_mask(list(equals = "true")), c(TRUE, FALSE, NA, TRUE, FALSE, NA))
  expect_identical(lg_mask(list(equal = "False")), c(FALSE, TRUE, NA, FALSE, TRUE, NA))
  expect_identical(lg_mask(list(`in` = list("T", "F"))), c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE))
  expect_identical(lg_mask(list(`in` = c("TRUE", "false"))), c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE))
  expect_identical(lg_mask(list(`in` = list("F"))), c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE))
})

test_that("the negated operators read a text token the same way", {
  expect_identical(lg_mask(list(not_equals = "T")), c(FALSE, TRUE, NA, FALSE, TRUE, NA))
  expect_identical(lg_mask(list(not_equal = "F")), c(TRUE, FALSE, NA, TRUE, FALSE, NA))
  expect_identical(lg_mask(list(not_in = list("T"))), c(FALSE, TRUE, TRUE, FALSE, TRUE, TRUE))
  expect_identical(
    lg_mask(list(not_in = list("T", "F"))),
    c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE)
  )
})

test_that("an operand that is already logical compares as it did before", {
  expect_identical(lg_mask(list(equals = TRUE)), c(TRUE, FALSE, NA, TRUE, FALSE, NA))
  expect_identical(lg_mask(list(equals = FALSE)), c(FALSE, TRUE, NA, FALSE, TRUE, NA))
  expect_identical(lg_mask(list(not_equals = TRUE)), c(FALSE, TRUE, NA, FALSE, TRUE, NA))
  expect_identical(lg_mask(list(`in` = list(TRUE))), c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE))
  expect_identical(lg_mask(list(not_in = list(FALSE))), c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE))
  # Mixed spellings of one value: a YAML `true` next to the text `F`.
  expect_identical(
    lg_mask(list(`in` = list(TRUE, "F"))),
    c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE)
  )
})

test_that("a missing member of a set matches a missing value as it does on any other column", {
  # Inside a list -- what a YAML sequence parses to -- a missing entry is
  # compared as the text "NA", so it never selects a missing value (this is what
  # rulePhrasing words, and test-rulePhrasing.R pins, for every column).
  expect_identical(
    lg_mask(list(`in` = list("T", NA))),
    c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE)
  )
  expect_identical(
    lg_mask(list(not_in = list("T", NA))),
    c(FALSE, TRUE, TRUE, FALSE, TRUE, TRUE)
  )
  expect_identical(
    lg_mask(list(`in` = list(TRUE, NA))),
    c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE)
  )

  # A missing TEXT entry (YAML `.na.character`) stays a missing value, so it
  # selects the missing rows -- and so does one in a plain vector.
  expect_identical(
    lg_mask(list(`in` = list("T", NA_character_))),
    c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE)
  )
  expect_identical(
    lg_mask(list(not_in = list(TRUE, NA_character_))),
    c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE)
  )

  # In a plain vector the missing member stays missing, as on a text column.
  expect_identical(
    lg_mask(list(`in` = c("T", NA))),
    c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE)
  )
})

test_that("a text operand that is not a token never matches and never errors", {
  for (operand in c("yes", "1", " T", "T ", "tRuE", "NA", "")) {
    expect_no_error(mask <- lg_mask(list(equals = operand)))
    expect_false(any(mask, na.rm = TRUE), info = paste0("equals '", operand, "'"))

    expect_no_error(
      negated <- lg_mask(list(not_equals = operand))
    )
    # The column differs from a value it can never hold, wherever it has one.
    expect_identical(
      negated,
      c(TRUE, TRUE, NA, TRUE, TRUE, NA),
      info = paste0("not_equals '", operand, "'")
    )

    expect_no_error(member <- lg_mask(list(`in` = list(operand))))
    expect_false(any(member), info = paste0("in ['", operand, "']"))
  }

  # A token next to a non-token in one set: the token still matches.
  expect_identical(
    lg_mask(list(`in` = list("T", "yes"))),
    c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE)
  )
  expect_identical(
    lg_mask(list(not_in = list("T", "yes"))),
    c(FALSE, TRUE, TRUE, FALSE, TRUE, TRUE)
  )
})

test_that("the class of the column decides, never the declared type", {
  # What a table holds when nothing has coerced it: the spec may say `R logical`
  # (see the dataset test below), but `validate_table()` does not type a raw
  # data.frame, so these are plain text and compare as text.
  text <- data.frame(
    ID = letters[1:5],
    FLAG = c("T", "TRUE", "F", "true", NA),
    stringsAsFactors = FALSE
  )

  expect_identical(lg_mask(list(equals = "T"), text), c(TRUE, FALSE, FALSE, FALSE, NA))
  expect_identical(lg_mask(list(`in` = list("T", "F")), text), c(TRUE, FALSE, TRUE, FALSE, FALSE))
  expect_identical(lg_mask(list(not_equals = "T"), text), c(FALSE, TRUE, TRUE, TRUE, NA))

  # And the operand helper is inert on a column that is not logical.
  expect_identical(dta_equality_bound(c("T", "F"), "T"), "T")
  expect_identical(dta_equality_bound(c("T", "F"), list("T", "F")), list("T", "F"))
})

test_that("a number compared with a logical column behaves as it always did", {
  expect_identical(lg_mask(list(equals = 1)), c(TRUE, FALSE, NA, TRUE, FALSE, NA))
  expect_identical(dta_equality_bound(c(TRUE, FALSE), 1), 1)
})

test_that("a rule on the logical column judges the coerced values", {
  df <- data.frame(
    ID = letters[1:5],
    FLAG = c("T", "TRUE", "F", "true", "yes"),
    stringsAsFactors = FALSE
  )
  specs <- unchecked_collection(
    columns = list(
      ID = DTAColumnSpec(id = "ID", type = "R character", nullable = FALSE),
      FLAG = DTAColumnSpec(id = "FLAG", type = "R logical", nullable = TRUE)
    )
  )
  typed <- dta_coerce_table_to_specs(df, specs)$table

  rule <- DTARuleColCondition(
    id = "flagged_ids",
    condition = list(FLAG = list(equals = "T")),
    then = list(ID = list(`in` = list("a", "b")))
  )

  # Rows a, b and d hold TRUE; d is not an allowed id, so one row fails.
  result <- rule_check_col_condition(rule, typed)
  expect_false(result$valid)
  expect_match(result$message, "1 rows failed the THEN conditions", fixed = TRUE)

  # On the text as read, only the single text `T` would match, and it is allowed.
  expect_true(rule_check_col_condition(rule, df)$valid)
})

test_that("a raw text table declared R logical keeps comparing as text in validate_table_detailed", {
  rule <- DTARuleColCondition(
    id = "t_only",
    condition = list(FLAG = list(equals = "T")),
    then = list(ID = list(`in` = list("a")))
  )
  specs <- unchecked_collection(
    columns = list(
      ID = DTAColumnSpec(id = "ID", type = "R character", nullable = FALSE),
      FLAG = DTAColumnSpec(id = "FLAG", type = "R logical", nullable = TRUE)
    ),
    rules = list(rule)
  )

  # `validate_table_detailed()` types nothing: the column is still text here, so
  # only the text `T` meets the condition, and its row (a) is the allowed one.
  raw <- data.frame(
    ID = c("a", "x", "c"), FLAG = c("T", "TRUE", "F"), stringsAsFactors = FALSE
  )
  expect_true(validate_table_detailed(specs, raw, verbose = FALSE)$rules_valid)

  # Typed the way a load types it, `TRUE` is the same value as `T`: row x also
  # meets the condition and is not an allowed id.
  typed <- dta_coerce_table_to_specs(raw, specs)$table
  expect_identical(typed$FLAG, c(TRUE, TRUE, FALSE))
  details <- validate_table_detailed(specs, typed, verbose = FALSE)
  expect_false(details$rules_valid)
  expect_match(details$rule_errors[[1]]$message, "1 rows failed the THEN", fixed = TRUE)
})

# ---- one declared logical column, in memory and streamed --------------------

lg_specs <- function(rules = list(), flag_type = "R logical", nullable = TRUE) {
  unchecked_collection(
    columns = list(
      ID = DTAColumnSpec(id = "ID", type = "R character", nullable = FALSE),
      FLAG = DTAColumnSpec(id = "FLAG", type = flag_type, nullable = nullable)
    ),
    rules = rules
  )
}

lg_write <- function(lines, name = "lg_flags.csv") {
  path <- file.path(tempdir(), name)
  writeLines(lines, path)
  path
}

# Rows: T, TRUE, F, an unconvertible `yes`, a blank, false and True. Seven rows,
# one import issue, and as logical values T T F NA NA F T.
lg_lines <- c(
  "ID,FLAG", "a,T", "b,TRUE", "c,F", "d,yes", "e,", "f,false", "g,True"
)

lg_run <- function(specs, path, stream, batch_rows = NULL) {
  ds <- DTADataSetTabular(
    id = "lg",
    specs = specs,
    files = list(DTAFileCSV(filename = basename(path)))
  )
  ds <- load_file(ds, file = path, handler_index = 1, stream = stream)
  args <- list(ds, quiet = TRUE, persist = FALSE)
  if (!is.null(batch_rows)) {
    args$batch_rows <- batch_rows
  }
  checked <- do.call(check, args)

  table_name <- names(tables(checked))[[1]]
  status <- validation_status(checked)
  status <- status[
    , setdiff(names(status), c("validated_at", "run_id", "validation_run")),
    drop = FALSE
  ]
  errors <- as.data.frame(validation_errors(checked, table_name))
  if (nrow(errors) > 0) {
    keys <- lapply(errors, as.character)
    errors <- errors[do.call(order, keys), , drop = FALSE]
    rownames(errors) <- NULL
  }

  list(
    status = status,
    errors = errors,
    n_import_errors = checked@validation_store[[table_name]]$n_import_errors,
    table = if (identical(stream, "never")) {
      as.data.frame(tables(checked)[[table_name]])
    }
  )
}

test_that("the in-memory and the streamed load give one verdict on a declared logical column", {
  path <- lg_write(lg_lines)
  on.exit(unlink(path), add = TRUE)

  specs <- lg_specs(rules = list(
    DTARuleColUnique(id = "flag_unique", columns = "FLAG"),
    DTARuleColCondition(
      id = "flagged_ids",
      condition = list(FLAG = list(equals = "T")),
      then = list(ID = list(`in` = list("a", "b")))
    )
  ))

  eager <- lg_run(specs, path, "never")
  lazy <- lg_run(specs, path, "always")

  # Typed at load: the column really is logical, the blank is missing, `yes` is NA.
  expect_identical(eager$table$FLAG, c(TRUE, TRUE, FALSE, NA, NA, FALSE, TRUE))

  # One import error, `yes`, in row 4; the blank in row 5 is not one.
  expect_equal(eager$n_import_errors, 1)
  import <- eager$errors[eager$errors$source == "import", , drop = FALSE]
  expect_equal(import$row, 4L)
  expect_identical(import$column, "FLAG")
  expect_identical(import$keyword, "not_convertible")

  # TRUE appears three times (T, TRUE, True) and FALSE twice, missing twice:
  # 7 rows - 3 distinct = 4 duplicates, whatever the spelling.
  rules <- eager$errors[eager$errors$source == "rule", , drop = FALSE]
  expect_setequal(rules$rule_id, c("flag_unique", "flagged_ids"))
  expect_match(
    rules$message[rules$rule_id == "flag_unique"], "4 duplicate row",
    fixed = TRUE
  )
  # IF FLAG is T: rows a, b and g. THEN ID is a or b: g fails.
  expect_match(
    rules$message[rules$rule_id == "flagged_ids"], "1 rows failed the THEN",
    fixed = TRUE
  )

  expect_identical(lazy$status, eager$status)
  expect_identical(lazy$errors, eager$errors)
  expect_identical(lazy$n_import_errors, eager$n_import_errors)
})

test_that("the verdicts do not depend on how the scan is cut into batches", {
  # `batch_rows` caps a batch: at 1 the blank row is a batch of nothing but
  # missing values, which is never typed (the column stays text there), and at 2
  # the `yes` and the blank share one. A non-nullable column puts the column
  # spec axis into the comparison as well: a missing value is a finding there.
  path <- lg_write(lg_lines, "lg_batches.csv")
  on.exit(unlink(path), add = TRUE)

  specs <- lg_specs(
    rules = list(
      DTARuleColUnique(id = "flag_unique", columns = "FLAG"),
      DTARuleColCondition(
        id = "flagged_ids",
        condition = list(FLAG = list(equals = "T")),
        then = list(ID = list(`in` = list("a", "b")))
      )
    ),
    nullable = FALSE
  )

  eager <- lg_run(specs, path, "never")

  # All three axes are in play, so agreeing is not agreeing on nothing: the two
  # missing FLAG values (rows 4 and 5) are column spec findings.
  expect_setequal(unique(eager$errors$source), c("columnspec", "rule", "import"))
  expect_setequal(
    as.integer(eager$errors$row[eager$errors$source == "columnspec"]),
    c(4L, 5L)
  )

  for (rows in c(1L, 2L, 3L, 100L)) {
    lazy <- lg_run(specs, path, "always", batch_rows = rows)

    expect_identical(lazy$status, eager$status, info = paste("batch_rows", rows))
    expect_identical(lazy$errors, eager$errors, info = paste("batch_rows", rows))
    expect_identical(
      lazy$n_import_errors, eager$n_import_errors,
      info = paste("batch_rows", rows)
    )
  }
})

test_that("T and TRUE are one key in a logical column and two in a character one", {
  path <- lg_write(lg_lines, "lg_unique.csv")
  on.exit(unlink(path), add = TRUE)

  rule <- DTARuleColUnique(id = "flag_unique", columns = "FLAG")

  for (stream in c("never", "always")) {
    logical_run <- lg_run(lg_specs(list(rule)), path, stream)
    logical_dups <- logical_run$errors[logical_run$errors$rule_id %in% "flag_unique", ]
    expect_equal(nrow(logical_dups), 1L, info = stream)
    expect_match(logical_dups$message, "4 duplicate row", fixed = TRUE, info = stream)

    # Declared as text the same file has no duplicate but the one blank key.
    text_run <- lg_run(lg_specs(list(rule), flag_type = "R character"), path, stream)
    expect_false("flag_unique" %in% text_run$errors$rule_id, info = stream)
  }
})

test_that("a logical key is answered by the per-batch path, across batches", {
  path <- lg_write(lg_lines, "lg_unique_batches.csv")
  on.exit(unlink(path), add = TRUE)

  rule <- DTARuleColUnique(id = "flag_unique", columns = "FLAG")
  specs <- lg_specs(list(rule))
  dataset <- dta_open_normalized_dataset(path, specs = specs)

  # The key columns are text on the wire, so only the declaration says logical.
  eligibility <- dta_arrow_unique_eligible(rule, dataset, specs)
  expect_false(eligibility$ok)
  expect_identical(eligibility$numeric_columns, character(0))
  expect_null(dta_stream_unique_precompute(specs, dataset, specs@rules)[[1]])

  # The same source with the column declared as text is still eligible, so the
  # refusal above is the logical declaration and nothing else.
  text_specs <- lg_specs(list(rule), flag_type = "R character")
  text_dataset <- dta_open_normalized_dataset(path, specs = text_specs)
  expect_true(dta_arrow_unique_eligible(rule, text_dataset, text_specs)$ok)

  # One row per batch: `T` and `True` sit in different batches, and the scan
  # still counts them as one key.
  details <- validate_file_stream(specs, path, batch_rows = 1L, verbose = FALSE)
  coerced <- dta_coerce_table_to_specs(
    utils::read.csv(path, colClasses = "character", na.strings = ""),
    specs
  )
  expected <- rule_check_unique(rule, as.data.frame(coerced$table))

  expect_false(expected$valid)
  expect_false(details$rules_valid)
  expect_identical(details$rule_errors[[1]]$message, expected$message)
  expect_equal(details$n_import_errors, 1)
})

test_that("a key made of a logical and a text column is also left to the per-batch path", {
  path <- lg_write(c("ID,FLAG", "a,T", "a,TRUE", "b,F"), "lg_pair.csv")
  on.exit(unlink(path), add = TRUE)

  rule <- DTARuleColUnique(id = "pair_unique", columns = c("ID", "FLAG"))
  specs <- lg_specs(list(rule))
  dataset <- dta_open_normalized_dataset(path, specs = specs)

  expect_false(dta_arrow_unique_eligible(rule, dataset, specs)$ok)

  # (a, T) and (a, TRUE) are one key once FLAG is logical.
  details <- validate_file_stream(specs, path, batch_rows = 1L, verbose = FALSE)
  expect_false(details$rules_valid)
  expect_match(details$rule_errors[[1]]$message, "1 duplicate row", fixed = TRUE)
})
