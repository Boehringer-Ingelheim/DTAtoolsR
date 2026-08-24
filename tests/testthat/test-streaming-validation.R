# Equivalence of the streaming and non-streaming column spec axes (P3).
#
# The claim the streaming path has to earn is not "it works" but "it produces
# exactly what the materialising path produced". These tests assert that
# directly, over the same corpus the golden oracle uses, so every constraint
# type is covered rather than a convenient subset.
#
# Small batch sizes are used deliberately. A batch size of 1 is the harshest
# available test of the row-offset arithmetic: every row lands in its own
# batch, so any error in the offset shows up as a wrong row number on the very
# first violation rather than hiding until some larger boundary is crossed.

vs_reader <- function(table, batch_rows) {
  dta_as_batch_reader(arrow::as_arrow_table(table), batch_rows = batch_rows)
}

# The column spec axis as the production driver evaluates it. These tests used to
# call a separate column-spec-only streamer, which was a second hand-maintained copy
# of the batch loop that nothing in the package actually used. Going through the
# real driver means these assertions constrain the code that ships.
vs_schema_stream <- function(specs, reader, max_errors = NULL) {
  details <- dta_validate_table_stream(
    specs, reader,
    verbose = FALSE, coerce = FALSE, max_errors = max_errors
  )
  list(
    full_error = details$columnspec_errors$full_error,
    summarised_error = details$columnspec_errors$summarised_error,
    n_errors = details$n_columnspec_errors
  )
}

test_that("the batch reader actually yields more than one batch", {
  # Without this, every equivalence test below could pass trivially by handing
  # the whole table over in a single batch and never exercising an offset.
  reader <- vs_reader(data.frame(x = 1:10), batch_rows = 2L)

  n_batches <- 0L
  repeat {
    batch <- reader$read_next_batch()
    if (is.null(batch)) break
    n_batches <- n_batches + 1L
  }

  expect_gt(n_batches, 1L)
})

test_that("streaming reproduces the materialised column spec axis for every corpus case", {
  corpus <- vc_corpus()

  for (name in names(corpus)) {
    case <- corpus[[name]]
    expected <- dta_columnspec_errors(case$specs, case$table)

    for (batch_rows in c(1L, 2L)) {
      streamed <- vs_schema_stream(
        case$specs,
        vs_reader(case$table, batch_rows)
      )

      expect_equal(
        streamed$full_error,
        expected$full_error,
        info = paste0("case '", name, "' at batch_rows = ", batch_rows)
      )
      expect_equal(
        streamed$summarised_error,
        expected$summarised_error,
        info = paste0("case '", name, "' summary at batch_rows = ", batch_rows)
      )
    }
  }
})

test_that("compiling a collection's schemas once matches deriving them per call", {
  # The streaming driver derives each column's schema once for the whole scan
  # and passes it into every batch. That is only sound if the compiled form is
  # what `dta_columnspec_errors()` would have derived for itself, so assert the
  # two agree rather than trusting that the derivation is pure.
  corpus <- vc_corpus()

  for (name in names(corpus)) {
    case <- corpus[[name]]

    compiled <- dta_compile_columnspec_schemas(case$specs)

    expect_equal(
      vapply(compiled, function(entry) entry$name, character(1)),
      names(case$specs@columns),
      info = paste0("case '", name, "' compiled names")
    )
    expect_equal(
      lapply(compiled, function(entry) entry$schema),
      unname(lapply(case$specs@columns, as_json_schema)),
      info = paste0("case '", name, "' compiled schemas")
    )

    expect_equal(
      dta_columnspec_errors(case$specs, case$table, schemas = compiled),
      dta_columnspec_errors(case$specs, case$table),
      info = paste0("case '", name, "' results")
    )
  }
})

test_that("compiling schemas keeps the branches the refactor moved", {
  # These are the paths that moved into dta_compile_columnspec_schemas(), and
  # the corpus does not reach any of them. Without these, a regression in
  # exactly the code that changed would still pass the whole suite.
  corpus <- vc_corpus()

  # A collection whose columns cannot be read at all compiles to nothing, and
  # a table validated against nothing is not thereby invalid.
  expect_equal(dta_compile_columnspec_schemas(list()), list())
  expect_equal(
    dta_columnspec_errors(list(), corpus[[1]]$table, schemas = list()),
    list(summarised_error = NULL, full_error = NULL)
  )

  # A column whose schema cannot be derived is skipped, not dropped: it keeps
  # its index, so the `.col_order` of every later column is unchanged and only
  # that column's own violations disappear. Run over the whole corpus, because
  # the first case ("clean") has no violations at all to lose.
  exercised <- 0L

  for (nm in names(corpus)) {
    case <- corpus[[nm]]
    compiled <- dta_compile_columnspec_schemas(case$specs)
    expect_length(compiled, length(case$specs@columns))

    first_name <- compiled[[1]]$name
    without_first <- function(errs) {
      if (is.null(errs)) {
        return(NULL)
      }
      kept <- errs[!(!is.na(errs$column) & errs$column == first_name), , drop = FALSE]
      rownames(kept) <- NULL
      if (nrow(kept) == 0) NULL else kept
    }

    full <- dta_columnspec_errors(case$specs, case$table, schemas = compiled)$full_error

    undecidable <- compiled
    undecidable[[1]]$schema <- NULL
    skipped <- dta_columnspec_errors(case$specs, case$table, schemas = undecidable)$full_error

    expect_equal(skipped, without_first(full), info = paste0("case '", nm, "'"))

    if (!is.null(full) && any(!is.na(full$column) & full$column == first_name)) {
      exercised <- exercised + 1L
    }
  }

  # Guards the assertion above against being vacuous: with no violation on any
  # first column, dropping that column's schema removes nothing and the
  # comparison only ever pits NULL against NULL.
  expect_gt(exercised, 0L)

  # A spec column absent from the data still yields one `required` error per
  # row, sourced from the compiled name rather than from the live spec.
  case <- corpus[["clean"]]
  compiled <- dta_compile_columnspec_schemas(case$specs)
  first_name <- compiled[[1]]$name
  missing_first <- case$table[, setdiff(names(case$table), first_name), drop = FALSE]
  expect_false(first_name %in% names(missing_first))

  res <- dta_columnspec_errors(case$specs, missing_first, schemas = compiled)
  required <- res$full_error[res$full_error$keyword == "required", , drop = FALSE]
  expect_equal(nrow(required), nrow(missing_first))
  expect_true(all(required$columnspec == first_name))

  # An empty table short-circuits before any schema is consulted.
  expect_equal(
    dta_columnspec_errors(case$specs, case$table[0, , drop = FALSE], schemas = compiled),
    list(summarised_error = NULL, full_error = NULL)
  )
})

test_that("dta_columnspec_errors(summarise = FALSE) changes only whether the summary is built", {
  # The streaming driver passes summarise = FALSE and recomputes the summary
  # once at the end. That is only sound if skipping it leaves `full_error`
  # byte-for-byte what the eager call produced, so assert identity rather than
  # equality, over every corpus case that actually produces violations.
  corpus <- vc_corpus()
  n_error_cases <- 0L

  for (name in names(corpus)) {
    case <- corpus[[name]]
    eager <- dta_columnspec_errors(case$specs, case$table)
    lazy <- dta_columnspec_errors(case$specs, case$table, summarise = FALSE)

    expect_named(lazy, c("summarised_error", "full_error"))
    expect_identical(
      lazy$full_error,
      eager$full_error,
      info = paste0("case '", name, "'")
    )
    expect_null(lazy$summarised_error)

    if (!is.null(eager$full_error)) {
      n_error_cases <- n_error_cases + 1L
      # The eager call must still be doing the work that was skipped.
      expect_false(is.null(eager$summarised_error), info = paste0("case '", name, "'"))
    }
  }

  # Guards the loop above against passing on a corpus of clean tables, where
  # both fields are NULL for trivial reasons.
  expect_gt(n_error_cases, 0L)
})

test_that("dta_columnspec_errors summarises by default for its non-streaming callers", {
  # The two callers that are NOT the streaming driver rely on the default, so
  # pin it: an unnamed call must still return a populated summary.
  specs <- vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 2, nullable = FALSE)
  ))
  table <- data.frame(ID = c("ok", "toolong", "alsotoolong"), stringsAsFactors = FALSE)

  eager <- dta_columnspec_errors(specs, table)
  lazy <- dta_columnspec_errors(specs, table, summarise = FALSE)

  expect_s3_class(eager$summarised_error, "data.frame")
  expect_gt(nrow(eager$summarised_error), 0L)
  expect_null(lazy$summarised_error)
  expect_identical(lazy$full_error, eager$full_error)
  expect_equal(nrow(eager$full_error), 2L)
})

test_that("row numbers are positions in the input, not in the batch", {
  # A violation in the last row of a multi-batch scan is the case that a
  # missing offset gets wrong: batch-local numbering would report row 1.
  specs <- vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE)
  ))
  table <- data.frame(
    ID = c("A001", "B002", "C003", "TOOLONG"),
    stringsAsFactors = FALSE
  )

  streamed <- vs_schema_stream(specs, vs_reader(table, batch_rows = 1L))

  expect_equal(nrow(streamed$full_error), 1)
  expect_equal(streamed$full_error$row, 4L)
  expect_equal(streamed$full_error$keyword, "maxLength")
})

test_that("violations spread across batches are all reported, in row order", {
  specs <- vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE)
  ))
  table <- data.frame(
    ID = c("TOOLONG1", "B002", "TOOLONG2", "D004", "TOOLONG3"),
    stringsAsFactors = FALSE
  )

  streamed <- vs_schema_stream(specs, vs_reader(table, batch_rows = 2L))

  expect_equal(streamed$full_error$row, c(1L, 3L, 5L))
  expect_equal(streamed$n_errors, 3L)
})

test_that("a missing column is reported for every row across every batch", {
  # The per-row cost of a structural failure, pinned in the streaming path too:
  # the count must not depend on how the scan happened to be divided.
  specs <- vc_corpus()$columnspec_required$specs
  table <- data.frame(ID = sprintf("A%03d", 1:7), stringsAsFactors = FALSE)

  for (batch_rows in c(1L, 3L, 7L, 100L)) {
    streamed <- vs_schema_stream(specs, vs_reader(table, batch_rows))
    expect_equal(nrow(streamed$full_error), 7)
    expect_equal(streamed$full_error$row, 1:7)
  }
})

# ---- the full driver ---------------------------------------------------------

# The strongest claim P3 makes: a streamed validation and a materialised one
# produce the same verdict, the same counts and the same errors. Asserted over
# the whole corpus so every axis and every rule type is covered.

vs_details_equal <- function(streamed, expected, label) {
  expect_equal(streamed$ok, expected$ok, info = label)
  expect_equal(streamed$columnspec_valid, expected$columnspec_valid, info = label)
  expect_equal(streamed$rules_valid, expected$rules_valid, info = label)
  expect_equal(streamed$import_valid, expected$import_valid, info = label)
  expect_equal(streamed$n_columnspec_errors, expected$n_columnspec_errors, info = label)
  expect_equal(streamed$n_rule_errors, expected$n_rule_errors, info = label)
  expect_equal(
    as.integer(streamed$n_import_errors),
    as.integer(expected$n_import_errors),
    info = label
  )
}

test_that("streaming reproduces the materialised verdict for every corpus case", {
  corpus <- vc_corpus()

  for (name in names(corpus)) {
    case <- corpus[[name]]
    expected <- validate_table_detailed(
      specs = case$specs, table = case$table, verbose = FALSE
    )

    for (batch_rows in c(1L, 2L, 1000L)) {
      streamed <- dta_validate_table_stream(
        case$specs,
        vs_reader(case$table, batch_rows),
        verbose = FALSE,
        # The materialising path does not type the table itself; it receives one
        # already typed. Match that here so the comparison is like for like.
        coerce = FALSE
      )
      vs_details_equal(
        streamed, expected,
        paste0("case '", name, "' at batch_rows = ", batch_rows)
      )
    }
  }
})

test_that("streamed column spec errors match the materialised ones row for row", {
  corpus <- vc_corpus()

  for (name in names(corpus)) {
    case <- corpus[[name]]
    expected <- validate_table_detailed(
      specs = case$specs, table = case$table, verbose = FALSE
    )
    streamed <- dta_validate_table_stream(
      case$specs, vs_reader(case$table, 1L),
      verbose = FALSE, coerce = FALSE
    )

    expect_equal(
      streamed$columnspec_errors$full_error,
      expected$columnspec_errors$full_error,
      info = paste0("case '", name, "'")
    )
  }
})

test_that("streamed rule messages match the materialised ones", {
  corpus <- vc_corpus()
  rule_cases <- Filter(function(case) length(case$specs@rules) > 0, corpus)

  for (name in names(rule_cases)) {
    case <- rule_cases[[name]]
    expected <- validate_table_detailed(
      specs = case$specs, table = case$table, verbose = FALSE
    )
    streamed <- dta_validate_table_stream(
      case$specs, vs_reader(case$table, 1L),
      verbose = FALSE, coerce = FALSE
    )

    expect_equal(
      vapply(streamed$rule_errors, function(e) e$message, character(1)),
      vapply(expected$rule_errors, function(e) e$message, character(1)),
      info = paste0("case '", name, "'")
    )
  }
})

test_that("the streamed details object satisfies the published contract", {
  # Every field the exported reporting functions read must be present, with the
  # type they expect. A streamed result has to be a drop-in for a materialised
  # one, not merely similar to it.
  case <- vc_corpus()$all_axes
  streamed <- dta_validate_table_stream(
    case$specs, vs_reader(case$table, 2L),
    verbose = FALSE, coerce = FALSE
  )

  expect_named(
    streamed,
    c(
      "ok", "columnspec_valid", "rules_valid", "import_valid",
      "n_columnspec_errors", "n_rule_errors", "n_import_errors",
      "columnspec_errors", "rule_results", "rule_errors", "import_errors",
      "result_version"
    )
  )
  expect_named(streamed$columnspec_errors, c("summarised_error", "full_error"))
  expect_type(streamed$n_import_errors, "integer")

  # And it must survive the coercion every consumer goes through.
  flat <- as.data.frame(dta_as_validation_details(streamed))
  expect_true(all(
    c("source", "rule_id", "row", "column", "keyword", "message") %in% names(flat)
  ))
  expect_gt(nrow(flat), 0)
})

test_that("streaming types each batch when asked, recording import errors", {
  # With coerce = TRUE the driver does the work the import layer does once for
  # a materialised table, but batch by batch and with global row numbers.
  specs <- vc_specs(list(
    DTAColumnSpec(id = "VAL", type = "SAS Num", nullable = TRUE)
  ))
  table <- data.frame(
    VAL = c("10", "abc", "30", "xyz"),
    stringsAsFactors = FALSE
  )

  streamed <- dta_validate_table_stream(
    specs, vs_reader(table, 1L),
    verbose = FALSE, coerce = TRUE
  )

  expect_false(streamed$import_valid)
  expect_equal(as.integer(streamed$n_import_errors), 2L)
  # Row numbers are global, so the second bad value is row 4 and not row 1 of
  # its own batch.
  expect_equal(sort(streamed$import_errors$row), c(2L, 4L))
})

test_that("max_errors caps retained column spec detail without changing the verdict", {
  specs <- vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 2, nullable = FALSE)
  ))
  table <- data.frame(ID = rep("TOOLONG", 20), stringsAsFactors = FALSE)

  capped <- dta_validate_table_stream(
    specs, vs_reader(table, 3L),
    verbose = FALSE, max_errors = 5L, coerce = FALSE
  )
  uncapped <- dta_validate_table_stream(
    specs, vs_reader(table, 3L),
    verbose = FALSE, coerce = FALSE
  )

  expect_equal(nrow(capped$columnspec_errors$full_error), 5)
  expect_equal(nrow(uncapped$columnspec_errors$full_error), 20)

  # The count and the verdict are unaffected by how much detail was kept.
  expect_equal(capped$n_columnspec_errors, 20)
  expect_equal(capped$n_columnspec_errors, uncapped$n_columnspec_errors)
  expect_equal(capped$ok, uncapped$ok)
  expect_false(capped$columnspec_valid)
})

# ---- end to end, from a file -------------------------------------------------

vs_write_csv <- function(df) {
  path <- tempfile(fileext = ".csv")
  utils::write.csv(df, path, row.names = FALSE)
  path
}

test_that("a file is validated by scanning it, matching the in-memory verdict", {
  specs <- vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE),
    DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
  ))
  table <- data.frame(
    ID = c("A001", "TOOLONG", "B002"),
    AGE = c(30, 40, 50),
    stringsAsFactors = FALSE
  )
  path <- vs_write_csv(table)
  on.exit(unlink(path), add = TRUE)

  streamed <- validate_file_stream(specs, path, verbose = FALSE)

  expect_false(streamed$columnspec_valid)
  expect_equal(streamed$n_columnspec_errors, 1)
  expect_equal(streamed$columnspec_errors$full_error$row, 2L)
  expect_equal(streamed$columnspec_errors$full_error$keyword, "maxLength")
})

test_that("the scanned result is a drop-in for the reporting functions", {
  specs <- vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE)
  ))
  path <- vs_write_csv(data.frame(ID = c("A001", "TOOLONG"), stringsAsFactors = FALSE))
  on.exit(unlink(path), add = TRUE)

  streamed <- validate_file_stream(specs, path, verbose = FALSE)
  flat <- as.data.frame(dta_as_validation_details(streamed))

  expect_true(all(
    c("source", "rule_id", "row", "column", "keyword", "message") %in% names(flat)
  ))
  expect_equal(flat$source, "columnspec")
  expect_equal(flat$row, 2)
})

test_that("row numbers survive a scan divided into many batches", {
  # The point of the whole exercise: a violation deep in a file is reported at
  # its position in the file, whatever batch size the scan happened to use.
  specs <- vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE)
  ))
  ids <- sprintf("A%03d", 1:200)
  ids[177] <- "WAY-TOO-LONG"
  path <- vs_write_csv(data.frame(ID = ids, stringsAsFactors = FALSE))
  on.exit(unlink(path), add = TRUE)

  for (batch_rows in c(8L, 64L, 100000L)) {
    streamed <- validate_file_stream(
      specs, path,
      batch_rows = batch_rows, verbose = FALSE
    )
    expect_equal(streamed$n_columnspec_errors, 1)
    expect_equal(streamed$columnspec_errors$full_error$row, 177L)
  }
})

test_that("rules are enforced across a scanned file", {
  specs <- vc_specs(
    list(
      DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE),
      DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
    ),
    list(
      DTARuleColRange(id = "age_range", columns = "AGE", range = c(18, 70)),
      DTARuleColUnique(id = "id_unique", columns = "ID")
    )
  )
  table <- data.frame(
    ID = c("A001", "A002", "A001", "A004"),
    AGE = c(30, 99, 40, 50),
    stringsAsFactors = FALSE
  )
  path <- vs_write_csv(table)
  on.exit(unlink(path), add = TRUE)

  # A batch size of 1 puts the duplicate in a different batch from its original
  # and the out-of-range value in a batch of its own.
  streamed <- validate_file_stream(specs, path, batch_rows = 1L, verbose = FALSE)

  expect_false(streamed$rules_valid)
  expect_equal(streamed$n_rule_errors, 2)

  messages <- vapply(streamed$rule_errors, function(e) e$message, character(1))
  expect_true(any(grepl("age_range", messages, fixed = TRUE)))
  expect_true(any(grepl("id_unique", messages, fixed = TRUE)))
})

test_that("the returned result is usable without reaching for internals", {
  # It is handed straight to a user, so as.data.frame() must dispatch to the
  # method that flattens it. An untagged list fails with a row-count error that
  # says nothing about the cause, and the internal tagging helper is not
  # exported, so there would be no way for a caller to recover.
  specs <- vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE)
  ))
  path <- vs_write_csv(data.frame(ID = c("A001", "TOOLONG"), stringsAsFactors = FALSE))
  on.exit(unlink(path), add = TRUE)

  details <- validate_file_stream(specs, path, verbose = FALSE)

  expect_s3_class(details, "dta_validation_details")

  flat <- as.data.frame(details)
  expect_true(all(
    c("source", "rule_id", "row", "column", "keyword", "message") %in% names(flat)
  ))
  expect_equal(nrow(flat), 1)
  expect_equal(flat$row, 2)
})

test_that("tagging does not lose the partial_scan marker", {
  # The class is applied last; an attribute set earlier must survive it.
  specs <- vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE)
  ))
  ids <- sprintf("A%03d", 1:20)
  ids[2] <- "TOOLONG"
  path <- vs_write_csv(data.frame(ID = ids, stringsAsFactors = FALSE))
  on.exit(unlink(path), add = TRUE)

  quick <- validate_file_stream(
    specs, path,
    batch_rows = 4L, fail_fast = TRUE, verbose = FALSE
  )

  expect_s3_class(quick, "dta_validation_details")
  expect_true(isTRUE(attr(quick, "partial_scan")))
})

test_that("a structural verdict is tagged and flattens too", {
  specs <- vc_specs(list(
    DTAColumnSpec(id = "GONE", type = "SAS Char", length = 4, nullable = FALSE)
  ))
  path <- vs_write_csv(data.frame(ID = "A001", stringsAsFactors = FALSE))
  on.exit(unlink(path), add = TRUE)

  stopped <- validate_file_stream(
    specs, path,
    on_missing_column = "stop", verbose = FALSE
  )

  expect_s3_class(stopped, "dta_validation_details")
  expect_true(isTRUE(attr(stopped, "structural_only")))
  expect_equal(nrow(as.data.frame(stopped)), 1)
})

test_that("a missing file is reported rather than failing obscurely", {
  specs <- vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE)
  ))
  expect_error(
    validate_file_stream(specs, file.path(tempdir(), "definitely-not-here.csv")),
    "File not found"
  )
})

# ---- the structural gate -----------------------------------------------------

test_that("structural findings name both missing and unexpected columns", {
  specs <- vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE),
    DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
  ))

  findings <- dta_structure_findings(specs, c("ID", "EXTRA"))

  expect_equal(findings$missing, "AGE")
  expect_equal(findings$unexpected, "EXTRA")
  expect_false(findings$ok)
})

test_that("a file whose columns all match reports a sound structure", {
  specs <- vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE)
  ))
  findings <- dta_structure_findings(specs, "ID")

  expect_length(findings$missing, 0)
  expect_length(findings$unexpected, 0)
  expect_true(findings$ok)
})

test_that("stopping on a missing column reports it once, not once per row", {
  # The whole point of the gate. Scanning reports the absence for every row,
  # which on a large file means restating one fact millions of times.
  specs <- vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE),
    DTAColumnSpec(id = "MISSING", type = "SAS Char", length = 4, nullable = FALSE)
  ))
  path <- vs_write_csv(data.frame(ID = sprintf("A%03d", 1:50), stringsAsFactors = FALSE))
  on.exit(unlink(path), add = TRUE)

  stopped <- validate_file_stream(
    specs, path,
    on_missing_column = "stop", verbose = FALSE
  )
  scanned <- validate_file_stream(
    specs, path,
    on_missing_column = "scan", verbose = FALSE
  )

  expect_equal(stopped$n_columnspec_errors, 1)
  expect_equal(scanned$n_columnspec_errors, 50)

  # Both agree the file is invalid; they differ only in how much they say.
  expect_false(stopped$ok)
  expect_false(scanned$ok)
  expect_false(stopped$columnspec_valid)
  expect_false(scanned$columnspec_valid)
})

test_that("the default still scans, so existing behaviour is unchanged", {
  specs <- vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE),
    DTAColumnSpec(id = "MISSING", type = "SAS Char", length = 4, nullable = FALSE)
  ))
  path <- vs_write_csv(data.frame(ID = sprintf("A%03d", 1:7), stringsAsFactors = FALSE))
  on.exit(unlink(path), add = TRUE)

  defaulted <- validate_file_stream(specs, path, verbose = FALSE)
  expect_equal(defaulted$n_columnspec_errors, 7)
})

test_that("a structural verdict is marked as resting on the header alone", {
  # Without this a caller could read rules_valid = TRUE as "the rules were
  # checked and passed", when in fact nothing was read.
  specs <- vc_specs(list(
    DTAColumnSpec(id = "GONE", type = "SAS Char", length = 4, nullable = FALSE)
  ))
  path <- vs_write_csv(data.frame(ID = "A001", stringsAsFactors = FALSE))
  on.exit(unlink(path), add = TRUE)

  stopped <- validate_file_stream(
    specs, path,
    on_missing_column = "stop", verbose = FALSE
  )

  expect_true(isTRUE(attr(stopped, "structural_only")))
  expect_equal(stopped$columnspec_errors$full_error$keyword, "required")
  expect_match(
    stopped$columnspec_errors$full_error$message,
    "must have required property 'GONE'",
    fixed = TRUE
  )
})

test_that("the structural result still satisfies the details contract", {
  specs <- vc_specs(list(
    DTAColumnSpec(id = "GONE", type = "SAS Char", length = 4, nullable = FALSE)
  ))
  path <- vs_write_csv(data.frame(ID = "A001", stringsAsFactors = FALSE))
  on.exit(unlink(path), add = TRUE)

  stopped <- validate_file_stream(
    specs, path,
    on_missing_column = "stop", verbose = FALSE
  )
  flat <- as.data.frame(dta_as_validation_details(stopped))

  expect_true(all(
    c("source", "rule_id", "row", "column", "keyword", "message") %in% names(flat)
  ))
  expect_equal(nrow(flat), 1)
})

# ---- change detection without materialising -----------------------------------

test_that("an in-memory table is identified by its contents", {
  a <- arrow::as_arrow_table(data.frame(x = 1:3))
  b <- arrow::as_arrow_table(data.frame(x = 1:3))
  c <- arrow::as_arrow_table(data.frame(x = 1:4))

  expect_equal(dta_table_change_signal(a), dta_table_change_signal(b))
  expect_false(identical(dta_table_change_signal(a), dta_table_change_signal(c)))
})

test_that("a dataset is identified without reading its rows", {
  path <- vs_write_csv(data.frame(x = 1:100))
  on.exit(unlink(path), add = TRUE)

  ds <- arrow::open_delim_dataset(path, delim = ",")
  first <- dta_table_change_signal(ds)

  expect_type(first, "character")
  # Stable across repeated calls on an unchanged file.
  expect_equal(first, dta_table_change_signal(arrow::open_delim_dataset(path, delim = ",")))
})

test_that("rewriting the file changes the dataset's signal", {
  path <- vs_write_csv(data.frame(x = 1:100))
  on.exit(unlink(path), add = TRUE)
  before <- dta_table_change_signal(arrow::open_delim_dataset(path, delim = ","))

  utils::write.csv(data.frame(x = 1:250), path, row.names = FALSE)
  after <- dta_table_change_signal(arrow::open_delim_dataset(path, delim = ","))

  expect_false(identical(before, after))
})

test_that("a consumable reader has no stable identity and always revalidates", {
  # Reading a reader to identify it would spend the very thing the caller
  # needs, so it reports no identity rather than a wrong one.
  reader <- vs_reader(data.frame(x = 1:4), 2L)
  expect_null(dta_table_change_signal(reader))
})

test_that("lazy and materialised tables are told apart", {
  path <- vs_write_csv(data.frame(x = 1:4))
  on.exit(unlink(path), add = TRUE)

  expect_false(dta_table_is_lazy(arrow::as_arrow_table(data.frame(x = 1:4))))
  expect_true(dta_table_is_lazy(arrow::open_delim_dataset(path, delim = ",")))
  expect_true(dta_table_is_lazy(vs_reader(data.frame(x = 1:4), 2L)))
})

test_that("dispatching by holding produces the same verdict either way", {
  specs <- vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE)
  ))
  frame <- data.frame(ID = c("A001", "TOOLONG", "B002"), stringsAsFactors = FALSE)
  path <- vs_write_csv(frame)
  on.exit(unlink(path), add = TRUE)

  materialised <- dta_validate_any_table(
    specs, arrow::as_arrow_table(frame),
    verbose = FALSE
  )
  lazy <- dta_validate_any_table(
    specs, arrow::open_delim_dataset(path, delim = ","),
    verbose = FALSE
  )

  expect_equal(materialised$ok, lazy$ok)
  expect_equal(materialised$columnspec_valid, lazy$columnspec_valid)
  expect_equal(materialised$n_columnspec_errors, lazy$n_columnspec_errors)
  expect_equal(
    materialised$columnspec_errors$full_error$row,
    lazy$columnspec_errors$full_error$row
  )
})

# ---- stopping at the first problem -------------------------------------------

test_that("fail_fast stops the scan once something is wrong", {
  specs <- vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE)
  ))
  ids <- sprintf("A%03d", 1:100)
  ids[3] <- "TOOLONG"
  ids[90] <- "ALSOTOOLONG"
  path <- vs_write_csv(data.frame(ID = ids, stringsAsFactors = FALSE))
  on.exit(unlink(path), add = TRUE)

  quick <- validate_file_stream(
    specs, path,
    batch_rows = 10L, fail_fast = TRUE, verbose = FALSE
  )
  full <- validate_file_stream(specs, path, batch_rows = 10L, verbose = FALSE)

  # Both agree the file is invalid.
  expect_false(quick$ok)
  expect_false(full$ok)
  expect_false(quick$columnspec_valid)

  # The full scan sees both violations; the quick one stops after the first
  # batch that showed a problem, so it sees only the early one.
  expect_equal(full$n_columnspec_errors, 2)
  expect_equal(quick$n_columnspec_errors, 1)
  expect_true(isTRUE(attr(quick, "partial_scan")))
})

test_that("a stopped scan does not claim the axes it never finished are clean", {
  # The trap this guards: reporting rules_valid = TRUE after reading a tenth of
  # the file would state that no duplicates exist, having never looked.
  specs <- vc_specs(
    list(
      DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE),
      DTAColumnSpec(id = "K", type = "SAS Char", length = 8, nullable = TRUE)
    ),
    list(DTARuleColUnique(id = "k_unique", columns = "K"))
  )
  ids <- sprintf("A%03d", 1:40)
  ids[2] <- "TOOLONG"
  # The duplicate sits far past where the scan will stop.
  ks <- sprintf("K%03d", 1:40)
  ks[40] <- ks[1]
  path <- vs_write_csv(data.frame(ID = ids, K = ks, stringsAsFactors = FALSE))
  on.exit(unlink(path), add = TRUE)

  quick <- validate_file_stream(
    specs, path,
    batch_rows = 5L, fail_fast = TRUE, verbose = FALSE
  )

  expect_true(isTRUE(attr(quick, "partial_scan")))
  # Not TRUE, and not FALSE: unknown.
  expect_true(is.na(quick$rules_valid))
  expect_false(quick$ok)

  # The full scan does find the duplicate, which is exactly why the quick one
  # must not have claimed there was none.
  full <- validate_file_stream(specs, path, batch_rows = 5L, verbose = FALSE)
  expect_false(full$rules_valid)
})

test_that("fail_fast on a clean file scans it all and reports normally", {
  specs <- vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE)
  ))
  path <- vs_write_csv(data.frame(ID = sprintf("A%03d", 1:50), stringsAsFactors = FALSE))
  on.exit(unlink(path), add = TRUE)

  quick <- validate_file_stream(
    specs, path,
    batch_rows = 5L, fail_fast = TRUE, verbose = FALSE
  )

  expect_true(quick$ok)
  expect_true(quick$columnspec_valid)
  expect_true(quick$rules_valid)
  # Nothing was cut short, so the result is complete.
  expect_null(attr(quick, "partial_scan"))
})

test_that("fail_fast defaults off so a full report is the norm", {
  specs <- vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE)
  ))
  ids <- sprintf("A%03d", 1:20)
  ids[1] <- "TOOLONG"
  ids[20] <- "ALSOTOOLONG"
  path <- vs_write_csv(data.frame(ID = ids, stringsAsFactors = FALSE))
  on.exit(unlink(path), add = TRUE)

  defaulted <- validate_file_stream(specs, path, batch_rows = 2L, verbose = FALSE)
  expect_equal(defaulted$n_columnspec_errors, 2)
  expect_null(attr(defaulted, "partial_scan"))
})

# ---- the Parquet cache --------------------------------------------------------

test_that("a cached file validates to the same verdict as the original", {
  # The cache must not change any answer. If it did it would be a way of
  # getting a different result by being fast, which is worthless.
  specs <- vc_specs(
    list(
      DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE),
      DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
    ),
    list(DTARuleColRange(id = "age_range", columns = "AGE", range = c(18, 70)))
  )
  frame <- data.frame(
    ID = c("A001", "TOOLONG", "B002"),
    AGE = c(30, 40, 99),
    stringsAsFactors = FALSE
  )
  csv <- vs_write_csv(frame)
  cache <- file.path(tempdir(), paste0("vs_cache_", as.integer(runif(1, 1, 1e8))))
  on.exit(
    {
      unlink(csv)
      unlink(cache, recursive = TRUE)
    },
    add = TRUE
  )

  cache_as_parquet(specs, csv, cache_path = cache)
  expect_true(dir.exists(cache))

  from_csv <- validate_file_stream(specs, csv, verbose = FALSE)
  from_cache <- validate_file_stream(specs, cache, verbose = FALSE)

  expect_equal(from_cache$ok, from_csv$ok)
  expect_equal(from_cache$columnspec_valid, from_csv$columnspec_valid)
  expect_equal(from_cache$rules_valid, from_csv$rules_valid)
  expect_equal(from_cache$n_columnspec_errors, from_csv$n_columnspec_errors)
  expect_equal(from_cache$n_rule_errors, from_csv$n_rule_errors)
  expect_equal(
    from_cache$columnspec_errors$full_error$row,
    from_csv$columnspec_errors$full_error$row
  )
})

test_that("caching preserves declared types rather than re-inferring them", {
  # The reason the cache is written through the specs: an ID like "007" must
  # stay text. A cache that let Parquet infer the type would turn it into 7 and
  # silently change what the file means.
  specs <- vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE)
  ))
  csv <- vs_write_csv(data.frame(ID = c("007", "042"), stringsAsFactors = FALSE))
  cache <- file.path(tempdir(), paste0("vs_types_", as.integer(runif(1, 1, 1e8))))
  on.exit(
    {
      unlink(csv)
      unlink(cache, recursive = TRUE)
    },
    add = TRUE
  )

  cache_as_parquet(specs, csv, cache_path = cache)
  back <- as.data.frame(arrow::open_dataset(cache, format = "parquet"))

  expect_type(back$ID, "character")
  expect_equal(back$ID, c("007", "042"))
})

test_that("the format is chosen from the path", {
  csv <- vs_write_csv(data.frame(ID = "A001", stringsAsFactors = FALSE))
  cache <- file.path(tempdir(), paste0("vs_fmt_", as.integer(runif(1, 1, 1e8))))
  on.exit(
    {
      unlink(csv)
      unlink(cache, recursive = TRUE)
    },
    add = TRUE
  )

  specs <- vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE)
  ))
  cache_as_parquet(specs, csv, cache_path = cache)

  expect_s3_class(dta_open_validation_dataset(csv, specs), "Dataset")
  expect_s3_class(dta_open_validation_dataset(cache, specs), "Dataset")
})

test_that("caching a file that is not there is reported plainly", {
  specs <- vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE)
  ))
  expect_error(
    cache_as_parquet(specs, file.path(tempdir(), "no-such-input.csv")),
    "File not found"
  )
})

# ---- counting past the per-column retention cap -------------------------------

test_that("import errors are counted in full even past the retention cap", {
  # Import typing keeps at most dta_import_max_rows_per_column (10,000) rows per
  # column but records the true total on the frame. Counting the retained rows
  # instead of that total under-reports exactly the case the cap exists for, and
  # every smaller test passes while it is wrong.
  specs <- vc_specs(list(
    DTAColumnSpec(id = "VAL", type = "SAS Num", nullable = TRUE)
  ))
  n_bad <- 12000L
  path <- vs_write_csv(data.frame(VAL = rep("abc", n_bad), stringsAsFactors = FALSE))
  on.exit(unlink(path), add = TRUE)

  # One batch, so the whole column is typed in a single coercion and the cap
  # bites within it.
  streamed <- validate_file_stream(
    specs, path,
    batch_rows = 131072L, verbose = FALSE
  )

  expect_false(streamed$import_valid)
  expect_equal(as.integer(streamed$n_import_errors), n_bad)
  # The retained detail is capped; the count is not.
  expect_lt(nrow(streamed$import_errors), n_bad)
})

# ---- a lazy table inside a dataset --------------------------------------------

test_that("check() scans a lazy table held in a DTADataSetTabular", {
  # The @tables contract was widened to accept a Dataset, and check() dispatches
  # to the streaming path for one. Asserted through the object itself, not just
  # through the helper, because that is what the widened contract promises.
  specs <- vc_specs(
    list(
      DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE),
      DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
    ),
    list(DTARuleColRange(id = "age_range", columns = "AGE", range = c(18, 70)))
  )
  frame <- data.frame(
    ID = c("A001", "TOOLONG", "B002"),
    AGE = c(30, 40, 99),
    stringsAsFactors = FALSE
  )
  path <- vs_write_csv(frame)
  on.exit(unlink(path), add = TRUE)

  ds <- DTADataSetTabular(
    name = "lazy_demo",
    specs = specs,
    tables = list(demo = arrow::as_arrow_table(frame))
  )

  # Swap the materialised table for a lazy dataset over the same rows.
  ds@tables[["demo"]] <- arrow::open_delim_dataset(path, delim = ",")

  ds <- check(ds, quiet = TRUE)
  status <- validation_status(ds)

  expect_false(status$ok[[1]])
  expect_equal(status$n_columnspec_errors[[1]], 1)
  expect_equal(status$n_rule_errors[[1]], 1)
})

test_that("last_validation_details is usable however the table was held", {
  # dta_validate_any_table() returns a tagged result for a lazy table and an
  # untagged one for a materialised table. The attribute must not inherit that
  # difference, or as.data.frame() would work on one and fail on the other.
  specs <- vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE)
  ))
  frame <- data.frame(ID = c("A001", "TOOLONG"), stringsAsFactors = FALSE)
  path <- vs_write_csv(frame)
  on.exit(unlink(path), add = TRUE)

  materialised <- DTADataSetTabular(
    name = "eager", specs = specs,
    tables = list(demo = arrow::as_arrow_table(frame))
  )
  # `tab =` is what puts check() in single-table mode; the attribute is only
  # attached there, which is deliberate and not something this test should
  # widen.
  materialised <- check(materialised, tab = "demo", quiet = TRUE)

  lazy <- DTADataSetTabular(
    name = "lazy", specs = specs,
    tables = list(demo = arrow::as_arrow_table(frame))
  )
  lazy@tables[["demo"]] <- arrow::open_delim_dataset(path, delim = ",")
  lazy <- check(lazy, tab = "demo", quiet = TRUE)

  for (obj in list(materialised, lazy)) {
    details <- attr(obj, "last_validation_details")
    expect_s3_class(details, "dta_validation_details")
    expect_equal(nrow(as.data.frame(details)), 1)
  }
})

test_that("a lazy table is accepted by the tables property contract", {
  path <- vs_write_csv(data.frame(ID = "A001", stringsAsFactors = FALSE))
  on.exit(unlink(path), add = TRUE)

  specs <- vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE)
  ))
  ds <- DTADataSetTabular(
    name = "lazy_ok",
    specs = specs,
    tables = list(demo = arrow::as_arrow_table(data.frame(ID = "A001")))
  )

  expect_no_error({
    ds@tables[["demo"]] <- arrow::open_delim_dataset(path, delim = ",")
  })
  # A plain data frame is still rejected: the point was to admit lazier forms,
  # not looser ones.
  expect_error(
    {
      ds@tables[["demo"]] <- data.frame(ID = "A001")
    },
    "Table"
  )
})

# ---- bounded retention -------------------------------------------------------

test_that("max_errors bounds retained detail while keeping the count exact", {
  # On a genuinely dirty large file the error frame is itself O(rows) and can
  # exhaust memory as surely as the data. Retention is capped; counting is not,
  # so the reported total stays exact and `ok` is never affected by truncation.
  specs <- vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 2, nullable = FALSE)
  ))
  table <- data.frame(ID = rep("TOOLONG", 10), stringsAsFactors = FALSE)

  streamed <- vs_schema_stream(
    specs,
    vs_reader(table, batch_rows = 2L),
    max_errors = 3L
  )

  expect_equal(nrow(streamed$full_error), 3)
  expect_equal(streamed$n_errors, 10L)
  expect_true(isTRUE(attr(streamed$full_error, "truncated")))
})

# ---- streaming the rules axis ------------------------------------------------

vs_stream_rule <- function(rule, table, batch_rows) {
  reader <- vs_reader(table, batch_rows)
  state <- dta_rule_stream_init(rule)
  repeat {
    batch <- reader$read_next_batch()
    if (is.null(batch)) break
    dta_rule_stream_update(state, rule, as.data.frame(batch))
  }
  dta_rule_stream_finalise(state, rule)
}

test_that("a constraint referencing an unknown condition fails the rule via streaming, not the run (FIX 8)", {
  # Mirrors the materialising path's regression test: mutate a validly
  # constructed rule's `@constraints` slot to reference a condition name that
  # is not defined, and verify the streaming finaliser surfaces this as a rule
  # FAILURE (via the `dta_rule_not_applicable` class), not an aborted scan.
  rule <- DTARuleGroupCondition(
    id = "bad_constraint_stream",
    group_by = "SUBJ",
    conditions = list(
      c_failed = list(STATUS = list(equals = "FAILED"))
    ),
    constraints = list(
      list(type = "mutually_exclusive", left = "c_failed", right = "c_failed")
    )
  )
  S7::prop(rule, "constraints") <- list(list(
    id = "constraint_1",
    type = "mutually_exclusive",
    left = "c_failed",
    right = "c_ghost", # not a defined condition
    left_scope = "any",
    right_scope = "any",
    message = NULL
  ))

  table <- data.frame(
    SUBJ = c("S1", "S1"),
    STATUS = c("FAILED", "OK"),
    stringsAsFactors = FALSE
  )

  result <- vs_stream_rule(rule, table, batch_rows = 1L)

  expect_false(result$valid)
  expect_match(result$message, "could not be evaluated", fixed = TRUE)
  expect_match(result$message, "c_ghost", fixed = TRUE)
})

test_that("rules are classified by how much of the table they need", {
  corpus <- vc_corpus()
  kinds <- vapply(
    c(
      corpus$rule_range$specs@rules[[1]],
      corpus$rule_unique$specs@rules[[1]],
      corpus$rule_condition$specs@rules[[1]],
      corpus$rule_group_exclusive$specs@rules[[1]]
    ),
    dta_rule_stream_kind,
    character(1)
  )

  expect_equal(
    unname(kinds),
    c("decomposable", "keyed", "decomposable", "grouped")
  )
})

test_that("streaming reproduces every rule result from the corpus", {
  corpus <- vc_corpus()
  rule_cases <- Filter(function(case) length(case$specs@rules) > 0, corpus)

  for (name in names(rule_cases)) {
    case <- rule_cases[[name]]

    for (rule in case$specs@rules) {
      expected <- apply_rules(list(rule), case$table, verbose = FALSE)[[1]]

      for (batch_rows in c(1L, 2L)) {
        streamed <- vs_stream_rule(rule, case$table, batch_rows)

        expect_equal(
          streamed$valid, expected$valid,
          info = paste0(name, " / ", rule@id, " @ batch ", batch_rows)
        )
        expect_equal(
          streamed$message, expected$message,
          info = paste0(name, " / ", rule@id, " @ batch ", batch_rows)
        )
      }
    }
  }
})

test_that("a duplicate is found when it lands in a different batch", {
  # The case a per-batch uniqueness check gets wrong: the two identical rows
  # never appear together, so only cross-batch state can see them.
  rule <- DTARuleColUnique(id = "k_unique", columns = "K")
  table <- data.frame(
    K = c("a", "b", "c", "a"),
    stringsAsFactors = FALSE
  )

  streamed <- vs_stream_rule(rule, table, batch_rows = 1L)
  expect_false(streamed$valid)
  expect_match(streamed$message, "1 duplicate row", fixed = TRUE)
})

test_that("uniqueness keys do not collide across column boundaries", {
  # Two rows that are genuinely different but whose concatenated columns could
  # look identical to a naive separator-joined key.
  rule <- DTARuleColUnique(id = "k", columns = c("A", "B"))
  table <- data.frame(
    A = c("x", "xy"),
    B = c("yz", "z"),
    stringsAsFactors = FALSE
  )

  streamed <- vs_stream_rule(rule, table, batch_rows = 1L)
  expect_true(streamed$valid)
  expect_equal(
    streamed$valid,
    apply_rules(list(rule), table, verbose = FALSE)[[1]]$valid
  )
})

test_that("uniqueness keys survive separator bytes in the data", {
  # REGRESSION GUARD for the key encoding. A raw separator join would render
  # both of these rows as "x<sep>y<sep>z" and report a duplicate that the data
  # does not contain. Asserted against the materialising path so the two can
  # never disagree, and at batch_rows = 1 so every row is keyed in its own
  # batch.
  sep <- intToUtf8(31L)
  rule <- DTARuleColUnique(id = "k", columns = c("A", "B"))
  table <- data.frame(
    A = c(paste0("x", sep, "y"), "x"),
    B = c("z", paste0("y", sep, "z")),
    stringsAsFactors = FALSE
  )

  expected <- apply_rules(list(rule), table, verbose = FALSE)[[1]]
  streamed <- vs_stream_rule(rule, table, batch_rows = 1L)

  expect_true(expected$valid)
  expect_equal(streamed$valid, expected$valid)
  expect_equal(streamed$message, expected$message)
})

test_that("a separator appearing only in a later batch does not change the encoding", {
  # The hazard a "join raw, escape only when needed" key would have if the
  # decision were made per batch: rows 1 and 3 are identical and must be seen
  # as duplicates even though the separator only enters the stream with row 2.
  sep <- intToUtf8(31L)
  rule <- DTARuleColUnique(id = "k", columns = c("A", "B"))
  table <- data.frame(
    A = c("x", paste0("p", sep, "q"), "x"),
    B = c("y", "r", "y"),
    stringsAsFactors = FALSE
  )

  expected <- apply_rules(list(rule), table, verbose = FALSE)[[1]]
  streamed <- vs_stream_rule(rule, table, batch_rows = 1L)

  expect_false(expected$valid)
  expect_equal(streamed$valid, expected$valid)
  expect_equal(streamed$message, expected$message)
})

test_that("a literal that looks like the missing-value marker is not a missing value", {
  # The marker is unreachable from real data because a literal escape byte is
  # itself escaped, so this row is distinct from the NA row rather than a
  # duplicate of it.
  marker_like <- paste0(intToUtf8(1L), "n")
  rule <- DTARuleColUnique(id = "k", columns = "K")
  table <- data.frame(
    K = c(marker_like, NA_character_, "a"),
    stringsAsFactors = FALSE
  )

  expected <- apply_rules(list(rule), table, verbose = FALSE)[[1]]
  streamed <- vs_stream_rule(rule, table, batch_rows = 1L)

  expect_true(expected$valid)
  expect_equal(streamed$valid, expected$valid)
  expect_equal(streamed$message, expected$message)
})

test_that("factor and date key columns stream the same verdict as they materialise", {
  # Neither type is character, and both can hold NA. A key builder that
  # subassigned its missing-value marker into the column itself would corrupt
  # the factor (an invalid level becomes NA) and fail outright on the Date.
  rule <- DTARuleColUnique(id = "k", columns = c("F", "D"))
  table <- data.frame(
    F = factor(c("a", "b", NA, NA, "a")),
    D = as.Date(c("2020-01-01", "2020-01-02", NA, NA, "2020-01-01")),
    stringsAsFactors = FALSE
  )

  expected <- apply_rules(list(rule), table, verbose = FALSE)[[1]]
  streamed <- expect_no_warning(vs_stream_rule(rule, table, batch_rows = 1L))

  expect_false(expected$valid)
  expect_equal(streamed$valid, expected$valid)
  expect_equal(streamed$message, expected$message)
})

test_that("double key columns key on the value, not on a 15-digit rendering", {
  # `as.character()` renders both of these to "0.3", but they are two different
  # doubles and `duplicated()` keeps them apart. A key built on the rendering
  # would report a duplicate that the file does not contain.
  rule <- DTARuleColUnique(id = "k", columns = "K")
  table <- data.frame(K = c(0.1 + 0.2, 0.3), stringsAsFactors = FALSE)

  expected <- apply_rules(list(rule), table, verbose = FALSE)[[1]]
  streamed <- vs_stream_rule(rule, table, batch_rows = 1L)

  expect_true(expected$valid)
  expect_equal(streamed$valid, expected$valid)
  expect_equal(streamed$message, expected$message)
})

test_that("NaN is not a missing value and -0 is not a second zero", {
  # Two conventions that have to be taken from `duplicated()` rather than
  # guessed: it treats NA and NaN as different values, and 0 and -0 as the
  # same one.
  rule <- DTARuleColUnique(id = "k", columns = "K")
  table <- data.frame(K = c(NA_real_, NaN, 0, -0), stringsAsFactors = FALSE)

  expected <- apply_rules(list(rule), table, verbose = FALSE)[[1]]
  streamed <- vs_stream_rule(rule, table, batch_rows = 1L)

  expect_false(expected$valid)
  expect_equal(streamed$valid, expected$valid)
  expect_equal(streamed$message, expected$message)
})

test_that("a sub-second timestamp keys on the instant it names", {
  # Asserted on the key directly, and at a precision below the one
  # `as.character.POSIXct()` renders, because on R >= 4.3 that method keeps
  # enough digits for a coarser case to pass without the fix. It is asserted
  # on the key rather than through `vs_stream_rule()` because the batch reader
  # goes through arrow, whose timestamp type cannot carry a difference this
  # small -- the two instants would arrive equal and the test would be
  # measuring arrow's resolution instead of the key's.
  t1 <- as.POSIXct(1, origin = "1970-01-01", tz = "UTC")
  t2 <- t1 + 1e-9
  df <- data.frame(K = c(t1, t2, t1))

  expect_identical(as.character(t1), as.character(t2))
  expect_equal(
    sum(duplicated(dta_unique_key(df, "K"))),
    sum(duplicated(df))
  )
})

test_that("a complex key column keys on both parts at full precision", {
  z <- c(complex(real = 0.1 + 0.2, imaginary = 0), 0.3 + 0i)
  df <- data.frame(K = z)

  expect_identical(as.character(z[[1]]), as.character(z[[2]]))
  expect_equal(
    sum(duplicated(dta_unique_key(df, "K"))),
    sum(duplicated(df))
  )
})

test_that("an integer64 key column is not reinterpreted as a double", {
  skip_if_not_installed("bit64")

  # integer64 is stored as a double, so a key that unclassed it would read
  # NA_integer64_ (INT64_MIN) as the double -0 and key it as the value 0.
  df <- data.frame(K = bit64::as.integer64(c(0, NA, 1, 0)))

  expect_equal(
    sum(duplicated(dta_unique_key(df, "K"))),
    sum(duplicated(df))
  )
})

test_that("a non-UTF-8 marked string keys identically in every batch", {
  # The hazard: the escaping pass can re-encode a latin1 string, and it only
  # runs in a batch that actually contains a reserved byte. Rows 1 and 3 are
  # the same value and must be recognised as duplicates even though only the
  # batch holding row 2 triggers escaping.
  #
  # Asserted on the raw bytes, not with expect_equal(): R's string comparison
  # translates before comparing, so it would call two differently marked
  # encodings of one value equal, while `fastmap` -- which is what actually
  # holds these keys across batches -- hashes the bytes and would not.
  latin1 <- rawToChar(as.raw(c(0x63, 0x61, 0x66, 0xe9)))
  Encoding(latin1) <- "latin1"
  sep <- intToUtf8(31L)

  table <- data.frame(
    K = c(latin1, paste0("p", sep, "q"), latin1),
    stringsAsFactors = FALSE
  )

  escaping_batch <- dta_unique_key(table[1:2, , drop = FALSE], "K")
  plain_batch <- dta_unique_key(table[3, , drop = FALSE], "K")

  expect_identical(charToRaw(plain_batch[[1]]), charToRaw(escaping_batch[[1]]))
  expect_identical(Encoding(plain_batch[[1]]), Encoding(escaping_batch[[1]]))

  # And through the accumulator that keys on those bytes, with the two
  # occurrences landing in different batches.
  rule <- DTARuleColUnique(id = "k", columns = "K")
  expected <- apply_rules(list(rule), table, verbose = FALSE)[[1]]
  streamed <- vs_stream_rule(rule, table, batch_rows = 2L)

  expect_false(expected$valid)
  expect_equal(streamed$valid, expected$valid)
  expect_equal(streamed$message, expected$message)
})

test_that("a key over no columns makes every row the same row", {
  # Degenerate, but the two paths have to agree on it: `duplicated()` on a
  # zero-column data frame calls every row after the first a duplicate.
  df <- data.frame(A = c("x", "y", "z"), stringsAsFactors = FALSE)

  expect_equal(
    sum(duplicated(dta_unique_key(df, character(0)))),
    sum(duplicated(df[, character(0), drop = FALSE]))
  )
})

test_that("repeated missing values count as duplicates when streamed", {
  # duplicated() treats repeated NAs as duplicates. A key that dropped them, or
  # gave each its own identity, would silently disagree.
  rule <- DTARuleColUnique(id = "k", columns = "K")
  table <- data.frame(K = c("a", NA_character_, NA_character_), stringsAsFactors = FALSE)

  streamed <- vs_stream_rule(rule, table, batch_rows = 1L)
  expected <- apply_rules(list(rule), table, verbose = FALSE)[[1]]

  expect_equal(streamed$valid, expected$valid)
  expect_equal(streamed$message, expected$message)
})

test_that("grouped rules stream rather than retaining their rows", {
  rule <- vc_corpus()$rule_group_exclusive$specs@rules[[1]]
  expect_equal(dta_rule_stream_kind(rule), "grouped")
})

test_that("a group split across batches still reaches the same verdict", {
  # The case that decides whether grouped streaming works at all: the two rows
  # that jointly violate the constraint never appear in the same batch, so only
  # a per-group reduction folded across batches can see them together.
  rule <- DTARuleGroupCondition(
    id = "grp",
    group_by = "SUBJ",
    conditions = list(
      failed = list(REASND = list(empty = FALSE)),
      reported = list(REASND = list(empty = TRUE), ORRES = list(empty = FALSE))
    ),
    constraints = list(
      list(type = "mutually_exclusive", left = "failed", right = "reported")
    )
  )
  table <- data.frame(
    SUBJ = c("A", "B", "A"),
    REASND = c("BROKEN", NA_character_, NA_character_),
    ORRES = c(NA_character_, "9", "12"),
    stringsAsFactors = FALSE
  )

  expected <- rule_check_group_condition(rule, table)

  for (batch_rows in c(1L, 2L, 100L)) {
    streamed <- vs_stream_rule(rule, table, batch_rows)
    expect_equal(streamed$valid, expected$valid, info = paste("batch", batch_rows))
    expect_equal(streamed$message, expected$message, info = paste("batch", batch_rows))
  }
})

test_that("an implication across batches reproduces the materialised message", {
  rule <- DTARuleGroupCondition(
    id = "grp_req",
    group_by = "SUBJ",
    conditions = list(
      failed = list(REASND = list(empty = FALSE)),
      not_done = list(STAT = list(equals = "NOT DONE"))
    ),
    constraints = list(
      list(type = "requires", `if` = "failed", then = "not_done")
    )
  )
  table <- data.frame(
    SUBJ = c("A", "B", "A", "B"),
    REASND = c("BROKEN", NA_character_, NA_character_, "ALSO"),
    STAT = c("DONE", "DONE", "DONE", "DONE"),
    stringsAsFactors = FALSE
  )

  expected <- rule_check_group_condition(rule, table)
  for (batch_rows in c(1L, 2L, 3L)) {
    streamed <- vs_stream_rule(rule, table, batch_rows)
    expect_equal(streamed$valid, expected$valid, info = paste("batch", batch_rows))
    expect_equal(streamed$message, expected$message, info = paste("batch", batch_rows))
  }
})

test_that("row numbers in grouped messages are global and truncate identically", {
  # More than ten violating rows, so the "(+N more)" branch is exercised in both
  # paths -- the streamed one from a capped head plus a count, the materialised
  # one from the whole vector.
  rule <- DTARuleGroupCondition(
    id = "grp_many",
    group_by = "SUBJ",
    conditions = list(
      failed = list(REASND = list(empty = FALSE)),
      reported = list(REASND = list(empty = TRUE), ORRES = list(empty = FALSE))
    ),
    constraints = list(
      list(type = "mutually_exclusive", left = "failed", right = "reported")
    )
  )
  n <- 30
  table <- data.frame(
    SUBJ = rep("A", n),
    REASND = c(rep("BROKEN", 15), rep(NA_character_, n - 15)),
    ORRES = c(rep(NA_character_, 15), rep("12", n - 15)),
    stringsAsFactors = FALSE
  )

  expected <- rule_check_group_condition(rule, table)
  streamed <- vs_stream_rule(rule, table, batch_rows = 4L)

  expect_equal(streamed$message, expected$message)
  expect_match(streamed$message, "more)", fixed = TRUE)
})

test_that("scope = all is not satisfied vacuously by an empty group", {
  # all(logical(0)) is TRUE, so a group that contributed no rows must not be
  # allowed to satisfy an "all" scope by accident.
  rule <- DTARuleGroupCondition(
    id = "grp_all",
    group_by = "SUBJ",
    conditions = list(
      done = list(STAT = list(equals = "DONE"))
    ),
    constraints = list(
      list(type = "requires", `if` = "done", then = "done", then_scope = "all")
    )
  )
  table <- data.frame(
    SUBJ = c("A", "A", "B"),
    STAT = c("DONE", "OPEN", "DONE"),
    stringsAsFactors = FALSE
  )

  expected <- rule_check_group_condition(rule, table)
  for (batch_rows in c(1L, 2L)) {
    streamed <- vs_stream_rule(rule, table, batch_rows)
    expect_equal(streamed$valid, expected$valid, info = paste("batch", batch_rows))
    expect_equal(streamed$message, expected$message, info = paste("batch", batch_rows))
  }
})

test_that("a high-cardinality grouped rule streams to the same verdict when batches split groups", {
  # Item B rewrote both rule_check_group_condition() (materialising) and
  # dta_group_stream_update() (streaming) to evaluate each condition once
  # rather than once per group. A batch size that does not align with group
  # boundaries is what would expose any drift between the two rewrites.
  n_groups <- 120
  df <- data.frame(
    SUBJ = sprintf("S%03d", rep(seq_len(n_groups), each = 3)),
    STATUS = rep("FAILED", n_groups * 3),
    RESULT = rep(NA_character_, n_groups * 3),
    stringsAsFactors = FALSE
  )
  # A handful of groups actually violate the constraint.
  violating <- c("S010", "S055", "S119")
  df$RESULT[df$SUBJ %in% violating] <- "12"

  rule <- DTARuleGroupCondition(
    id = "high_card_stream",
    group_by = "SUBJ",
    conditions = list(
      c_failed = list(STATUS = list(equals = "FAILED")),
      c_reported = list(RESULT = list(empty = FALSE))
    ),
    constraints = list(list(type = "mutually_exclusive", left = "c_failed", right = "c_reported"))
  )

  expected <- rule_check_group_condition(rule, df)
  expect_false(expected$valid)

  # batch_rows = 2 never aligns with the 3-row groups, so every group is split
  # across at least two batches.
  for (batch_rows in c(2L, 7L, 1000L)) {
    streamed <- vs_stream_rule(rule, df, batch_rows)
    expect_equal(streamed$valid, expected$valid, info = paste("batch", batch_rows))
    expect_equal(streamed$message, expected$message, info = paste("batch", batch_rows))
  }
})

test_that("a grouped rule and a uniqueness rule together stream to the materialised verdict", {
  # Both item B (grouped) and item C (keyed uniqueness) touch the streaming
  # driver; validating them together through the real driver is what item 7
  # of the design asks for, rather than each rule in isolation.
  n_groups <- 40
  df <- data.frame(
    SUBJ = sprintf("S%02d", rep(seq_len(n_groups), each = 3)),
    STATUS = rep("FAILED", n_groups * 3),
    RESULT = rep(NA_character_, n_groups * 3),
    ID = seq_len(n_groups * 3),
    stringsAsFactors = FALSE
  )
  df$RESULT[df$SUBJ == "S20"] <- "12"
  # Introduce one duplicate ID, split away from its twin by row position.
  df$ID[nrow(df)] <- df$ID[1]

  group_rule <- DTARuleGroupCondition(
    id = "combo_group",
    group_by = "SUBJ",
    conditions = list(
      c_failed = list(STATUS = list(equals = "FAILED")),
      c_reported = list(RESULT = list(empty = FALSE))
    ),
    constraints = list(list(type = "mutually_exclusive", left = "c_failed", right = "c_reported"))
  )
  unique_rule <- DTARuleColUnique(id = "combo_unique", columns = "ID")

  specs <- vc_specs(list(
    DTAColumnSpec(id = "SUBJ", type = "SAS Char", length = 3, nullable = FALSE),
    DTAColumnSpec(id = "STATUS", type = "SAS Char", length = 8, nullable = FALSE),
    DTAColumnSpec(id = "RESULT", type = "SAS Char", length = 8, nullable = TRUE),
    DTAColumnSpec(id = "ID", type = "SAS Num", length = 8, nullable = FALSE)
  ), rules = list(group_rule, unique_rule))

  expected <- validate_table_detailed(specs = specs, table = df, verbose = FALSE)
  expect_false(expected$rules_valid)

  for (batch_rows in c(2L, 5L)) {
    streamed <- dta_validate_table_stream(
      specs, vs_reader(df, batch_rows),
      verbose = FALSE, coerce = FALSE
    )
    expect_equal(streamed$rules_valid, expected$rules_valid, info = paste("batch", batch_rows))
    expect_equal(streamed$n_rule_errors, expected$n_rule_errors, info = paste("batch", batch_rows))
    expect_equal(
      vapply(streamed$rule_errors, function(e) e$message, character(1)),
      vapply(expected$rule_errors, function(e) e$message, character(1)),
      info = paste("batch", batch_rows)
    )
  }
})

test_that("max_errors leaves an under-cap result untruncated", {
  specs <- vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 2, nullable = FALSE)
  ))
  table <- data.frame(
    ID = c("OK", "TOOLONG", "OK"),
    stringsAsFactors = FALSE
  )

  streamed <- vs_schema_stream(
    specs,
    vs_reader(table, batch_rows = 1L),
    max_errors = 100L
  )

  expect_equal(nrow(streamed$full_error), 1)
  expect_equal(streamed$n_errors, 1L)
  expect_null(attr(streamed$full_error, "truncated"))
})

test_that("a grouped rule reports failure before the scan ends", {
  # fail_fast reads dta_rule_stream_failing(), not state$count: a grouped rule
  # never touches `count`, so reading it left fail_fast scanning a whole file
  # whose very first rows already broke a mutually_exclusive constraint.
  rule <- DTARuleGroupCondition(
    id = "excl",
    group_by = "SUBJECT_ID",
    conditions = list(
      c_failed = list(STATUS = list(equals = "FAILED")),
      c_reported = list(RESULT = list(empty = FALSE))
    ),
    constraints = list(list(
      type = "mutually_exclusive", left = "c_failed", right = "c_reported"
    ))
  )

  state <- dta_rule_stream_init(rule)
  expect_false(dta_rule_stream_failing(state))

  first_batch <- data.frame(
    SUBJECT_ID = c("S1", "S1"),
    STATUS = c("FAILED", "FAILED"),
    RESULT = c(NA, 12)
  )
  dta_rule_stream_update(state, rule, first_batch)
  expect_true(dta_rule_stream_failing(state))

  # And the verdict the finalised result reports is unchanged by that.
  expect_false(dta_rule_stream_finalise(state, rule)$valid)
})

test_that("a group whose constraint could still be rescued is not called failed early", {
  # `requires` is not monotone -- a later batch can satisfy the THEN -- so it
  # must not trip fail_fast on the strength of one batch.
  rule <- DTARuleGroupCondition(
    id = "req",
    group_by = "SUBJECT_ID",
    conditions = list(
      c_if = list(STATUS = list(equals = "FAILED")),
      c_then = list(RESULT = list(empty = FALSE))
    ),
    constraints = list(list(
      type = "requires", "if" = "c_if", "then" = "c_then"
    ))
  )

  state <- dta_rule_stream_init(rule)
  dta_rule_stream_update(state, rule, data.frame(
    SUBJECT_ID = "S1", STATUS = "FAILED", RESULT = NA
  ))
  expect_false(dta_rule_stream_failing(state))

  dta_rule_stream_update(state, rule, data.frame(
    SUBJECT_ID = "S1", STATUS = "OK", RESULT = 12
  ))
  expect_true(dta_rule_stream_finalise(state, rule)$valid)
})

test_that("import errors from both axes are counted once each, cap or no cap", {
  # Import errors arrive on two axes: import typing records a value it could
  # not represent, and the rule layer records a value it could not read as a
  # number. They are merged, and the merge is the only place that knows a cell
  # flagged on both is one error rather than two. Summing the raw sink totals
  # bypassed it and could report more import errors than `import_errors` had
  # rows. Nothing else in the suite populates BOTH sinks at once, which is
  # exactly the combination that broke.
  specs <- vc_specs(
    list(
      DTAColumnSpec(id = "NUMCOL", type = "SAS Num", nullable = TRUE),
      DTAColumnSpec(id = "TEXTCOL", type = "SAS Char", length = 8, nullable = TRUE)
    ),
    rules = list(
      DTARuleColRange(id = "textrange", columns = "TEXTCOL", min = 0, max = 100)
    )
  )

  n <- 20L
  path <- vs_write_csv(data.frame(
    NUMCOL = rep("abc", n),
    TEXTCOL = rep("xyz", n),
    stringsAsFactors = FALSE
  ))
  on.exit(unlink(path), add = TRUE)

  full <- validate_file_stream(specs, path, verbose = FALSE)

  expect_false(full$import_valid)
  # NUMCOL is flagged by import typing, TEXTCOL by the range rule reading it as
  # a number. Different cells, so the merge deduplicates nothing away.
  expect_equal(as.integer(full$n_import_errors), 2L * n)
  # The headline count may never exceed the detail it claims to summarise while
  # nothing has been capped. That invariant is what the double-count broke.
  expect_equal(as.integer(full$n_import_errors), nrow(full$import_errors))
  expect_setequal(unique(full$import_errors$column), c("NUMCOL", "TEXTCOL"))

  # With the retained-error cap on, the detail shrinks and the count does not.
  capped <- validate_file_stream(specs, path, max_errors = 5L, verbose = FALSE)
  expect_equal(as.integer(capped$n_import_errors), 2L * n)
  expect_lt(nrow(capped$import_errors), 2L * n)
})

# ---- cross-batch determinism and resource budgets ---------------------------

test_that("a duplicate is counted the same however the batches fall", {
  # Oracle test: the answer must not depend on where a batch boundary falls.
  # Duplicates are deliberately placed far apart so they land in different batches.
  rule <- DTARuleColUnique(id = "multi_key", columns = c("A", "B"))

  table <- data.frame(
    A = c("x", "y", "z", "x", "a", "b"),
    B = c("1", "2", "3", "1", "4", "5"),
    stringsAsFactors = FALSE
  )
  # At this point: rows 1 and 4 share (A, B) = ("x", "1"), a duplicate.

  # Add a row with NA in a key column; repeated NAs are duplicates.
  table_with_na <- rbind(
    table,
    data.frame(A = NA_character_, B = "9", stringsAsFactors = FALSE),
    data.frame(A = NA_character_, B = "9", stringsAsFactors = FALSE)
  )

  # Two duplicate rows in all: row 4 repeats row 1 on ("x", "1"), and the second
  # NA row repeats the first, because dta_unique_key() gives NA a value of its
  # own rather than dropping it.
  expect_equal(sum(duplicated(table_with_na[, c("A", "B")])), 2L)

  # The materialising path is the oracle. Comparing the whole message rather
  # than a count scraped back out of it means the count, the pluralisation and
  # the named columns are all pinned, and a message-format change cannot let a
  # wrong count through unnoticed.
  expected <- rule_check_unique(rule, table_with_na)
  expect_false(expected$valid)

  for (batch_rows in c(1L, 3L, 7L, 1000L)) {
    streamed <- vs_stream_rule(rule, table_with_na, batch_rows)

    expect_equal(
      streamed$valid, expected$valid,
      info = paste("verdict at batch_rows =", batch_rows)
    )
    expect_equal(
      streamed$message, expected$message,
      info = paste("message at batch_rows =", batch_rows)
    )
  }
})

test_that("a uniqueness scan that exceeds its key budget aborts rather than reporting a verdict", {
  # When a uniqueness scan hits the key budget, it aborts with dta_stream_budget_exceeded,
  # not by reporting a rule failure. A resource limit must not be dressed up as a data
  # verdict, which would present a clean-looking result for a constraint never checked.
  rule <- DTARuleColUnique(id = "k_budget", columns = "K")

  table <- data.frame(
    K = c("key1", "key2", "key3", "key4"),
    stringsAsFactors = FALSE
  )

  old <- getOption("DTAtools.max_unique_keys")
  on.exit(options(DTAtools.max_unique_keys = old), add = TRUE)
  options(DTAtools.max_unique_keys = 2L)

  reader <- vs_reader(table, batch_rows = 1L)
  state <- dta_rule_stream_init(rule)

  # Advance through batches until the budget is exceeded.
  expect_error(
    {
      repeat {
        batch <- reader$read_next_batch()
        if (is.null(batch)) break
        dta_rule_stream_update(state, rule, as.data.frame(batch))
      }
    },
    class = "dta_stream_budget_exceeded"
  )

  # The point of the test. Run the SAME fixture through the production driver,
  # which wraps every rule update in a tryCatch for dta_rule_not_applicable: the
  # budget condition must escape that handler and abort the run. If it were
  # caught, the driver would return a details object reporting the rule as an
  # ordinary failure, and a caller could not tell "this key is duplicated" from
  # "this constraint was never actually checked".
  specs <- vc_specs(
    list(DTAColumnSpec(id = "K", type = "SAS Char", length = 8, nullable = FALSE)),
    list(rule)
  )
  expect_error(
    dta_validate_table_stream(
      specs, vs_reader(table, batch_rows = 1L),
      verbose = FALSE, coerce = FALSE
    ),
    class = "dta_stream_budget_exceeded"
  )
})

test_that("a grouped scan that exceeds its group budget aborts", {
  # Grouped rules track distinct groups and conditions. When the group count exceeds
  # the budget, the scan aborts with dta_stream_budget_exceeded, not a rule failure.
  rule <- DTARuleGroupCondition(
    id = "grp_budget",
    group_by = "SUBJ",
    conditions = list(
      c_ok = list(STATUS = list(equals = "OK")),
      c_bad = list(STATUS = list(equals = "BAD"))
    ),
    constraints = list(
      list(type = "mutually_exclusive", left = "c_ok", right = "c_bad")
    )
  )

  # Three groups, none of which actually violates the constraint. The abort must
  # come from the group budget alone, so that a passing scan is what the budget
  # interrupts -- not a scan that was going to fail anyway.
  table <- data.frame(
    SUBJ = c("S1", "S2", "S3"),
    STATUS = c("OK", "OK", "OK"),
    stringsAsFactors = FALSE
  )

  old <- getOption("DTAtools.max_groups")
  on.exit(options(DTAtools.max_groups = old), add = TRUE)
  options(DTAtools.max_groups = 1L)

  reader <- vs_reader(table, batch_rows = 1L)
  state <- dta_rule_stream_init(rule)

  # Advance through batches; the second distinct group should trigger the budget.
  expect_error(
    {
      repeat {
        batch <- reader$read_next_batch()
        if (is.null(batch)) break
        dta_rule_stream_update(state, rule, as.data.frame(batch))
      }
    },
    class = "dta_stream_budget_exceeded"
  )
})

test_that("grouped violations are reported in the same order after the accumulator swap", {
  # Group keys moved from a separate state$keys vector to being read back from
  # the fastmap. The order matters: the assembled message must match the
  # materialising path, which builds violations in sorted-key order.
  rule <- DTARuleGroupCondition(
    id = "grp_order",
    group_by = "SUBJ",
    conditions = list(
      c_a = list(STAT = list(equals = "A")),
      c_b = list(STAT = list(equals = "B"))
    ),
    constraints = list(
      list(type = "mutually_exclusive", left = "c_a", right = "c_b")
    )
  )

  # Build a table where groups appear in reverse-alphabetical order but will be
  # reported in alphabetical order.
  table <- data.frame(
    SUBJ = c("Z", "Z", "Y", "Y", "X", "X"),
    STAT = c("A", "B", "A", "B", "A", "B"),
    stringsAsFactors = FALSE
  )

  # Materialising path result.
  expected <- rule_check_group_condition(rule, table)

  # Streaming path result with a small batch size to split groups across batches.
  streamed <- vs_stream_rule(rule, table, batch_rows = 1L)

  # Messages must be identical, including the group order.
  expect_equal(streamed$message, expected$message)
  expect_equal(streamed$valid, expected$valid)
})

test_that("the retained-error cap truncates detail without changing the counts", {
  # The error-retention cap is finite by default. It must truncate the retained
  # rows but not the counts, and must flag the truncation.
  specs <- vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 2, nullable = FALSE),
    DTAColumnSpec(id = "VAL", type = "SAS Char", length = 1, nullable = FALSE)
  ))

  table <- data.frame(
    ID = rep(c("TOOLONG", "OK"), 5),
    VAL = rep(c("XYZ", "A"), 5),
    stringsAsFactors = FALSE
  )

  old <- getOption("DTAtools.max_errors")
  on.exit(options(DTAtools.max_errors = old), add = TRUE)
  options(DTAtools.max_errors = 5L)

  capped <- dta_validate_table_stream(
    specs,
    vs_reader(table, batch_rows = 2L),
    verbose = FALSE, coerce = FALSE
  )

  # Retained error detail is capped.
  expect_lte(nrow(capped$columnspec_errors$full_error), 5)
  # But the count is exact.
  expect_equal(capped$n_columnspec_errors, 10L)
  # And the truncation is flagged.
  expect_true(isTRUE(attr(capped$columnspec_errors$full_error, "truncated")))
})

test_that("max_errors defaults to a finite cap", {
  # The default changed from NULL (unbounded) to a finite cap via getOption().
  # The default is 10000 and is configurable.
  default_cap <- eval(formals(validate_file_stream)$max_errors)

  expect_equal(default_cap, 10000L)

  # And options() can override it.
  old <- getOption("DTAtools.max_errors")
  on.exit(options(DTAtools.max_errors = old), add = TRUE)
  options(DTAtools.max_errors = 25L)
  new_default <- eval(formals(validate_file_stream)$max_errors)

  expect_equal(new_default, 25L)
})

test_that("use_threads does not change the verdict", {
  # The use_threads parameter is passed to Arrow's Scanner. Both settings must
  # produce the same verdict and error counts.
  specs <- vc_specs(
    list(
      DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE),
      DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
    ),
    list(DTARuleColUnique(id = "id_unique", columns = "ID"))
  )

  table <- data.frame(
    ID = c("A001", "TOOLONG", "A001", "A004"),
    AGE = c(30, 40, 50, 60),
    stringsAsFactors = FALSE
  )
  path <- vs_write_csv(table)
  on.exit(unlink(path), add = TRUE)

  single <- validate_file_stream(specs, path, use_threads = FALSE, verbose = FALSE)
  multi <- validate_file_stream(specs, path, use_threads = TRUE, verbose = FALSE)

  expect_equal(single$ok, multi$ok)
  expect_equal(single$n_columnspec_errors, multi$n_columnspec_errors)
  expect_equal(single$n_rule_errors, multi$n_rule_errors)
  expect_equal(single$n_import_errors, multi$n_import_errors)
  expect_equal(
    single$columnspec_errors$full_error,
    multi$columnspec_errors$full_error
  )
})


# Error counting past the integer limit.
#
# Retention is capped; counting is not. That is the whole point of the sink --
# the verdict must not become an artefact of truncation -- but it also means the
# counters are the one quantity in the streaming path that grows without bound.
# An integer accumulator does not error when it runs out of range: it returns
# `NA` with a warning, and `NA > 0` is `NA`, so the file too dirty to count was
# the file that stopped being judged.

test_that("the error sink counts past the integer limit without overflowing", {
  sink <- dta_error_sink(max_errors = 1L)
  one <- data.frame(row = 1L, column = "A", stringsAsFactors = FALSE)

  # Two batches each reporting more than half of `.Machine$integer.max` errors
  # is the smallest reproduction of a scan whose total leaves the integer range.
  expect_no_warning({
    dta_error_sink_add(sink, one, n_total = 1.5e9)
    dta_error_sink_add(sink, one, n_total = 1.5e9)
  })

  expect_equal(sink$total, 3e9)
  expect_false(is.na(sink$total))
  # The assertion that actually matters: this comparison is what the pass/fail
  # verdict reads, and it was `NA` rather than `TRUE`.
  expect_true(sink$total > 0)
  expect_true(sink$truncated)
})


test_that("counts are reported as integers until they cannot be", {
  # Every consumer of `details` has always seen integer counts, and the
  # materialising path is explicit about not letting the type depend on whether
  # anything failed. Widening happens only where integer would mean `NA`.
  expect_identical(dta_narrow_count(0), 0L)
  expect_identical(dta_narrow_count(5), 5L)
  expect_identical(dta_narrow_count(.Machine$integer.max), .Machine$integer.max)
  expect_identical(dta_narrow_count(3e9), 3e9)

  # Anything the integer range cannot hold is passed through untouched rather
  # than coerced, because coercion here is what produced `NA` in the first
  # place. That includes the inputs no call site currently produces: narrowing
  # must never be the step that loses a value.
  expect_no_warning({
    expect_identical(dta_narrow_count(NA_real_), NA_real_)
    expect_identical(dta_narrow_count(-3e9), -3e9)
    expect_identical(dta_narrow_count(c(1, 2)), c(1, 2))
    expect_identical(dta_narrow_count(numeric(0)), numeric(0))
  })
})


# Row counting past the integer limit.
#
# Same failure mode as the error counters above, on the accumulators that count
# ROWS rather than errors: the streaming path exists for files whose row count
# leaves the integer range, so every accumulator that grows once per row is a
# candidate. An integer one returns `NA` with a warning rather than erroring,
# and the `NA` then spreads into whatever reads it -- every reported row number
# for the offsets, and for `n_seen` the pass/fail verdict of a whole group.
#
# None of this can be reproduced by building a 2.1-billion-row file, so the
# arithmetic is tested directly at the boundary instead.

test_that("group condition counters are doubles that survive the integer limit", {
  cond <- dta_group_cond_state()

  # The type is the fix. `0L` here is what made the counters overflow, so
  # asserting the values alone would let the defect back in unnoticed.
  expect_type(cond$n_seen, "double")
  expect_type(cond$true_n, "double")
  expect_type(cond$false_n, "double")

  # A group whose rows span the integer boundary: the same arithmetic on
  # integers is `NA`, which is what the "all"-scope verdict below reads.
  expect_true(is.na(suppressWarnings(2147400000L + 2000000L)))

  expect_no_warning({
    cond$n_seen <- cond$n_seen + 2147400000
    cond$n_seen <- cond$n_seen + 2000000
  })
  expect_false(is.na(cond$n_seen))
  expect_true(is.finite(cond$n_seen))
  expect_equal(cond$n_seen, 2149400000)
})

test_that("folding row numbers keeps counting past the integer limit", {
  # `dta_group_fold_rows()` caps the head it retains but not the count, exactly
  # as the error sink caps retention and not counting. The count is therefore
  # unbounded in file size, and `count + length(new_rows)` on an integer count
  # is the overflow. The head is capped at ten, so only the count can move here.
  folded <- dta_group_fold_rows(1:10, 2147483000, 1:1000)

  expect_false(is.na(folded$count))
  expect_true(is.finite(folded$count))
  expect_equal(folded$count, 2147484000)
  expect_identical(folded$head, 1:10)
})

test_that("reported row numbers stay integer until they cannot", {
  # The offset a batch-local row number is shifted by is now a double, and
  # adding it would otherwise widen the reported `row` column to double for
  # every file, however small. Integer is what consumers have always seen.
  narrowed <- dta_narrow_rows(c(1, 2, 3) + 0)
  expect_type(narrowed, "integer")
  expect_identical(narrowed, c(1L, 2L, 3L))

  expect_identical(dta_narrow_rows(integer(0)), integer(0))
  expect_identical(dta_narrow_rows(.Machine$integer.max + 0), .Machine$integer.max)

  # A row number genuinely beyond the integer range is passed through as a
  # double rather than coerced, because coercing it is what produces the `NA`
  # this whole change exists to prevent. Missing row numbers narrow cleanly,
  # since `as.integer(NA)` loses nothing.
  expect_no_warning({
    expect_identical(dta_narrow_rows(3e9), 3e9)
    expect_identical(dta_narrow_rows(c(1, 3e9)), c(1, 3e9))
    expect_identical(dta_narrow_rows(c(1, NA)), c(1L, NA_integer_))
    expect_identical(dta_narrow_rows(NA_real_), NA_integer_)
  })
})

test_that("an all-scope group verdict is still reached past the integer limit", {
  # The reason this task exists. `dta_group_stream_truth()` reads
  # `cond$n_seen > 0 && cond$all_true` for an "all" scope. With `n_seen`
  # overflowed to `NA` that expression is `NA` (whenever `all_true` is TRUE),
  # the `NA` flows through `if_truth & !then_truth` into `constraint_viol`, and
  # the finaliser's `isTRUE(constraint_viol[[ci]][g])` is then FALSE -- so a
  # group that really does violate the constraint is silently reported as
  # passing. That is a wrong verdict, not a wrong message.
  huge <- 2149400000

  # A condition that held for every one of a group's 2.1-billion-plus rows.
  all_rows <- dta_group_cond_state()
  all_rows$n_seen <- huge
  all_rows$any_true <- TRUE
  all_rows$all_true <- TRUE

  # And one that held for some row but not for all of them.
  some_rows <- dta_group_cond_state()
  some_rows$n_seen <- huge
  some_rows$any_true <- TRUE
  some_rows$all_true <- FALSE

  expect_identical(dta_group_stream_truth(all_rows, "all"), TRUE)
  expect_identical(dta_group_stream_truth(some_rows, "all"), FALSE)
  expect_identical(dta_group_stream_truth(some_rows, "any"), TRUE)

  # The `requires` reduction exactly as the finaliser computes it, with an
  # `if_scope` of "all" so the verdict actually depends on `n_seen`: the IF
  # condition holds for every row, the THEN condition does not hold for all of
  # them, so the group is in violation and the finaliser must see a hard TRUE.
  if_truth <- dta_group_stream_truth(all_rows, "all")
  then_truth <- dta_group_stream_truth(some_rows, "all")
  constraint_viol <- if_truth & !then_truth

  expect_false(is.na(constraint_viol))
  expect_true(isTRUE(constraint_viol))

  # The overflowed counterpart, kept alongside so the mechanism is pinned and
  # not merely its absence: this is the same violating group as above, seen
  # through an integer `n_seen`. The violation is real and the finaliser drops
  # it, because `isTRUE(NA)` is FALSE.
  overflowed <- dta_group_cond_state()
  overflowed$n_seen <- NA_real_
  overflowed$any_true <- TRUE
  overflowed$all_true <- TRUE

  overflowed_truth <- dta_group_stream_truth(overflowed, "all")
  expect_true(is.na(overflowed_truth))
  expect_false(isTRUE(overflowed_truth & !then_truth))
})


# ---- the scan levers, reachable from check() ---------------------------------
#
# `fail_fast`, `on_missing_column` and `use_threads` are what make a scan of a
# table too large to hold survivable, and they were previously reachable only
# through `validate_file_stream()` -- not through `check()`, which is the
# documented entry point. These tests pin the forwarding by its EFFECT, because
# an argument that is accepted and then dropped would pass a signature check.

test_that("dta_table_column_names reads names without consuming the holding", {
  frame <- data.frame(ID = "A001", AGE = 1, stringsAsFactors = FALSE)
  path <- vs_write_csv(frame)
  on.exit(unlink(path), add = TRUE)

  expect_identical(dta_table_column_names(frame), c("ID", "AGE"))
  expect_identical(
    dta_table_column_names(arrow::as_arrow_table(frame)), c("ID", "AGE")
  )

  dataset <- arrow::open_delim_dataset(path, delim = ",")
  expect_identical(dta_table_column_names(dataset), c("ID", "AGE"))

  # The one that matters. A reader is consumable, so asking it for its columns
  # must not spend the rows the caller still needs -- the structural gate runs
  # BEFORE the scan and would otherwise silently eat the first batch.
  reader <- arrow::Scanner$create(dataset, batch_size = 1L)$ToRecordBatchReader()
  expect_identical(dta_table_column_names(reader), c("ID", "AGE"))
  expect_false(is.null(reader$read_next_batch()))

  # An unfamiliar holding is a reason to fall back to scanning, not to abort.
  expect_identical(dta_table_column_names(42L), character(0))
})

test_that("on_missing_column = 'stop' reaches the same verdict without scanning", {
  specs <- vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE),
    DTAColumnSpec(id = "ABSENT", type = "SAS Num", nullable = TRUE)
  ))
  frame <- data.frame(ID = c("A001", "A002", "A003"), stringsAsFactors = FALSE)

  scanned <- dta_validate_any_table(specs, frame, verbose = FALSE)
  stopped <- dta_validate_any_table(
    specs, frame,
    verbose = FALSE, on_missing_column = "stop"
  )

  # Same verdict, reached at different cost. The two paths disagreeing about
  # whether the table is valid would make the lever unusable.
  expect_false(scanned$ok)
  expect_false(stopped$ok)

  # The scan restates the absence once per row; the gate says it once.
  expect_equal(scanned$n_columnspec_errors, nrow(frame))
  expect_equal(stopped$n_columnspec_errors, 1)

  # Flagged, so no reader mistakes a header verdict for one about the rows.
  expect_true(isTRUE(attr(stopped, "structural_only")))
  expect_false(isTRUE(attr(scanned, "structural_only")))

  # The default must be the historical behaviour, unchanged.
  expect_equal(
    dta_validate_any_table(specs, frame, verbose = FALSE)$n_columnspec_errors,
    dta_validate_any_table(
      specs, frame,
      verbose = FALSE, on_missing_column = "scan"
    )$n_columnspec_errors
  )
})

test_that("check() forwards on_missing_column to the structural gate", {
  specs <- vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE),
    DTAColumnSpec(id = "ABSENT", type = "SAS Num", nullable = TRUE)
  ))
  frame <- data.frame(ID = c("A001", "A002", "A003"), stringsAsFactors = FALSE)

  build <- function() {
    DTADataSetTabular(
      name = "gate", specs = specs,
      tables = list(demo = arrow::as_arrow_table(frame))
    )
  }

  scanned <- check(build(), tab = "demo", quiet = TRUE, persist = FALSE)
  stopped <- check(
    build(),
    tab = "demo", quiet = TRUE, persist = FALSE,
    on_missing_column = "stop"
  )

  scanned_details <- attr(scanned, "last_validation_details")
  stopped_details <- attr(stopped, "last_validation_details")

  expect_equal(scanned_details$n_columnspec_errors, nrow(frame))
  expect_equal(stopped_details$n_columnspec_errors, 1)
  expect_true(isTRUE(attr(stopped_details, "structural_only")))

  # Both still report the table as invalid.
  expect_false(validation_status(scanned)$ok[[1]])
  expect_false(validation_status(stopped)$ok[[1]])
})

test_that("check() forwards fail_fast and use_threads without changing defaults", {
  specs <- vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE)
  ))
  # Every row is over length, so the first batch already settles the verdict.
  frame <- data.frame(
    ID = rep("TOOLONG", 40), stringsAsFactors = FALSE
  )
  path <- vs_write_csv(frame)
  on.exit(unlink(path), add = TRUE)

  build <- function() {
    ds <- DTADataSetTabular(
      name = "ff", specs = specs,
      tables = list(demo = arrow::as_arrow_table(frame))
    )
    ds@tables[["demo"]] <- arrow::open_delim_dataset(path, delim = ",")
    ds
  }

  full <- check(
    build(),
    tab = "demo", quiet = TRUE, persist = FALSE, batch_rows = 8L
  )
  fast <- check(
    build(),
    tab = "demo", quiet = TRUE, persist = FALSE, batch_rows = 8L,
    fail_fast = TRUE, use_threads = FALSE
  )

  full_details <- attr(full, "last_validation_details")
  fast_details <- attr(fast, "last_validation_details")

  # A partial scan is flagged as such; a complete one is not.
  expect_false(isTRUE(attr(full_details, "partial_scan")))
  expect_true(isTRUE(attr(fast_details, "partial_scan")))

  # Stopping early must not turn an invalid table into a valid one.
  expect_false(isTRUE(full_details$ok))
  expect_false(isTRUE(fast_details$ok))

  # It stopped early: it saw fewer rows than the full pass reported errors for.
  expect_lt(fast_details$n_columnspec_errors, full_details$n_columnspec_errors)
})

test_that("scan progress is throttled by wall time, not printed per batch", {
  specs <- vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE)
  ))
  frame <- data.frame(
    ID = sprintf("A%03d", seq_len(400) %% 1000), stringsAsFactors = FALSE
  )
  path <- vs_write_csv(frame)
  on.exit(unlink(path), add = TRUE)

  # The package's own cli string, not a translated base message.
  progress_lines <- function(out) sum(grepl("rows so far", out, fixed = TRUE))

  # A short scan must stay silent: the first line is due only after a full
  # interval, so a run that finishes inside one prints nothing at all.
  quiet_out <- capture.output(
    invisible(validate_file_stream(
      specs, path,
      batch_rows = 32L, verbose = TRUE
    )),
    type = "message"
  )
  expect_identical(progress_lines(quiet_out), 0L)

  # With the interval collapsed, the same scan reports. Base `options()` rather
  # than withr: the package does not depend on withr, and `R CMD check` fails a
  # `::` call to a package DESCRIPTION does not declare.
  previous <- options(DTAtools.progress_seconds = 0)
  on.exit(options(previous), add = TRUE)

  loud_out <- capture.output(
    invisible(validate_file_stream(
      specs, path,
      batch_rows = 32L, verbose = TRUE
    )),
    type = "message"
  )
  expect_gt(progress_lines(loud_out), 0L)
})
