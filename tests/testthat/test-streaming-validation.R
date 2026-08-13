# Equivalence of the streaming and non-streaming schema axes (P3).
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

test_that("streaming reproduces the materialised schema axis for every corpus case", {
  corpus <- vc_corpus()

  for (name in names(corpus)) {
    case <- corpus[[name]]
    expected <- dta_schema_errors(case$specs, case$table)

    for (batch_rows in c(1L, 2L)) {
      streamed <- dta_schema_errors_stream(
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

  streamed <- dta_schema_errors_stream(specs, vs_reader(table, batch_rows = 1L))

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

  streamed <- dta_schema_errors_stream(specs, vs_reader(table, batch_rows = 2L))

  expect_equal(streamed$full_error$row, c(1L, 3L, 5L))
  expect_equal(streamed$n_errors, 3L)
})

test_that("a missing column is reported for every row across every batch", {
  # The per-row cost of a structural failure, pinned in the streaming path too:
  # the count must not depend on how the scan happened to be divided.
  specs <- vc_corpus()$schema_required$specs
  table <- data.frame(ID = sprintf("A%03d", 1:7), stringsAsFactors = FALSE)

  for (batch_rows in c(1L, 3L, 7L, 100L)) {
    streamed <- dta_schema_errors_stream(specs, vs_reader(table, batch_rows))
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
  expect_equal(streamed$schema_valid, expected$schema_valid, info = label)
  expect_equal(streamed$rules_valid, expected$rules_valid, info = label)
  expect_equal(streamed$import_valid, expected$import_valid, info = label)
  expect_equal(streamed$n_schema_errors, expected$n_schema_errors, info = label)
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

test_that("streamed schema errors match the materialised ones row for row", {
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
      streamed$schema_errors$full_error,
      expected$schema_errors$full_error,
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
      "ok", "schema_valid", "rules_valid", "import_valid",
      "n_schema_errors", "n_rule_errors", "n_import_errors",
      "schema_errors", "rule_results", "rule_errors", "import_errors",
      "schema_version"
    )
  )
  expect_named(streamed$schema_errors, c("summarised_error", "full_error"))
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

test_that("max_errors caps retained schema detail without changing the verdict", {
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

  expect_equal(nrow(capped$schema_errors$full_error), 5)
  expect_equal(nrow(uncapped$schema_errors$full_error), 20)

  # The count and the verdict are unaffected by how much detail was kept.
  expect_equal(capped$n_schema_errors, 20)
  expect_equal(capped$n_schema_errors, uncapped$n_schema_errors)
  expect_equal(capped$ok, uncapped$ok)
  expect_false(capped$schema_valid)
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

  expect_false(streamed$schema_valid)
  expect_equal(streamed$n_schema_errors, 1)
  expect_equal(streamed$schema_errors$full_error$row, 2L)
  expect_equal(streamed$schema_errors$full_error$keyword, "maxLength")
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
  expect_equal(flat$source, "schema")
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
    expect_equal(streamed$n_schema_errors, 1)
    expect_equal(streamed$schema_errors$full_error$row, 177L)
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

  expect_equal(stopped$n_schema_errors, 1)
  expect_equal(scanned$n_schema_errors, 50)

  # Both agree the file is invalid; they differ only in how much they say.
  expect_false(stopped$ok)
  expect_false(scanned$ok)
  expect_false(stopped$schema_valid)
  expect_false(scanned$schema_valid)
})

test_that("the default still scans, so existing behaviour is unchanged", {
  specs <- vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE),
    DTAColumnSpec(id = "MISSING", type = "SAS Char", length = 4, nullable = FALSE)
  ))
  path <- vs_write_csv(data.frame(ID = sprintf("A%03d", 1:7), stringsAsFactors = FALSE))
  on.exit(unlink(path), add = TRUE)

  defaulted <- validate_file_stream(specs, path, verbose = FALSE)
  expect_equal(defaulted$n_schema_errors, 7)
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
  expect_equal(stopped$schema_errors$full_error$keyword, "required")
  expect_match(
    stopped$schema_errors$full_error$message,
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
  expect_equal(materialised$schema_valid, lazy$schema_valid)
  expect_equal(materialised$n_schema_errors, lazy$n_schema_errors)
  expect_equal(
    materialised$schema_errors$full_error$row,
    lazy$schema_errors$full_error$row
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
  expect_false(quick$schema_valid)

  # The full scan sees both violations; the quick one stops after the first
  # batch that showed a problem, so it sees only the early one.
  expect_equal(full$n_schema_errors, 2)
  expect_equal(quick$n_schema_errors, 1)
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
  expect_true(quick$schema_valid)
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
  expect_equal(defaulted$n_schema_errors, 2)
  expect_null(attr(defaulted, "partial_scan"))
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

  streamed <- dta_schema_errors_stream(
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
      expected <- apply_schema_rules(list(rule), case$table, verbose = FALSE)[[1]]

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
    apply_schema_rules(list(rule), table, verbose = FALSE)[[1]]$valid
  )
})

test_that("repeated missing values count as duplicates when streamed", {
  # duplicated() treats repeated NAs as duplicates. A key that dropped them, or
  # gave each its own identity, would silently disagree.
  rule <- DTARuleColUnique(id = "k", columns = "K")
  table <- data.frame(K = c("a", NA_character_, NA_character_), stringsAsFactors = FALSE)

  streamed <- vs_stream_rule(rule, table, batch_rows = 1L)
  expected <- apply_schema_rules(list(rule), table, verbose = FALSE)[[1]]

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

test_that("max_errors leaves an under-cap result untruncated", {
  specs <- vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 2, nullable = FALSE)
  ))
  table <- data.frame(
    ID = c("OK", "TOOLONG", "OK"),
    stringsAsFactors = FALSE
  )

  streamed <- dta_schema_errors_stream(
    specs,
    vs_reader(table, batch_rows = 1L),
    max_errors = 100L
  )

  expect_equal(nrow(streamed$full_error), 1)
  expect_equal(streamed$n_errors, 1L)
  expect_null(attr(streamed$full_error, "truncated"))
})
