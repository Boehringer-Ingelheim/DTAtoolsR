# Randomised engine parity between the eager (whole-column) and streaming
# validation engines. REQ-STREAM-018 .. REQ-STREAM-023.
#
# The generators live in helper-parity.R (the `qp_` prefix is theirs, chosen
# so they cannot collide with a helper from another area sourced into the same
# session). Everything here either drives that generator across
# qa_parity_seeds() cases, or builds one small, hand-placed case naming a
# specific mechanism -- an undeclared column keyed differently, or a defect at
# a batch edge -- so that a failure there points at the mechanism rather than
# at a seed a reader has to reproduce and diagnose from scratch.

# ---- the file-based campaign -------------------------------------------------

test_that("OQ-STREAM-020 | the eager and streaming engines agree on every generated file-based case | REQ-STREAM-018 | tags: slow", {
  results <- qp_file_results(qa_parity_seeds())
  for (entry in results) {
    label <- sprintf("seed for %s (%d rows)", entry$case$declared, entry$case$rows)
    qa_step(paste("validation status:", label), entry$eager$status, entry$lazy$status)
    qa_step(paste("sorted error detail:", label), entry$eager$errors, entry$lazy$errors)
    qa_step(
      paste("import-error count:", label),
      entry$eager$n_import_errors, entry$lazy$n_import_errors
    )
  }
})

test_that("OQ-STREAM-021 | the file-based campaign exercises every validation axis | REQ-STREAM-018", {
  # Without this, OQ-STREAM-020 would pass on cases that never disagree because
  # they never triggered anything to disagree about, and would prove nothing.
  results <- qp_file_results(qa_parity_seeds())
  seen <- c(columnspec = FALSE, rule = FALSE, import = FALSE)

  for (entry in results) {
    errors <- entry$eager$errors
    if (nrow(errors) > 0) {
      sources <- as.character(errors$source)
      seen[["columnspec"]] <- seen[["columnspec"]] || any(sources == "columnspec")
      seen[["rule"]] <- seen[["rule"]] || any(sources == "rule")
    }
    seen[["import"]] <- seen[["import"]] || entry$eager$n_import_errors > 0
  }

  qa_step(
    "every axis the campaign claims to exercise appeared at least once",
    c(columnspec = TRUE, rule = TRUE, import = TRUE), seen
  )
})

test_that("OQ-STREAM-022 | the file-based campaign varies its shape and delivers both a compressed and an uncompressed file | REQ-STREAM-018", {
  results <- qp_file_results(qa_parity_seeds())
  cases <- lapply(results, function(entry) entry$case)

  n_columns <- vapply(cases, function(x) length(x$specs@columns), integer(1))
  n_rules <- vapply(cases, function(x) length(x$specs@rules), integer(1))
  gzipped <- vapply(cases, function(x) grepl("[.]gz$", x$path), logical(1))

  qa_check("the number of declared columns never drops below the generator's minimum of three", min(n_columns) >= 3L)
  qa_check("nor exceeds its maximum of six", max(n_columns) <= 6L)
  qa_check("the number of rules never drops below one", min(n_rules) >= 1L)
  qa_check("nor exceeds three", max(n_rules) <= 3L)
  # Both deliveries have to occur, or half the reader is untested by this
  # campaign.
  qa_check("at least one generated case is delivered compressed", any(gzipped))
  qa_check("and at least one is delivered uncompressed", any(!gzipped))
})

test_that("OQ-STREAM-023 | the eager and streaming engines agree on a file with no rows and on one with a single row | REQ-STREAM-020", {
  dir <- qa_tempdir()
  # The campaign draws a row count and is not guaranteed to ever land on
  # either boundary: a header-only file yields no batch at all, and a one-row
  # file has exactly one. Both are where two independently written scan loops
  # are most likely to diverge.
  for (n in c(0L, 1L)) {
    columns <- list(
      ID = qp_column_values("ID", n),
      AGE = qp_column_values("AGE", n),
      XKEY = if (n == 0) character(0) else "1.50",
      XPAD = if (n == 0) character(0) else "p001"
    )
    frame <- as.data.frame(columns, stringsAsFactors = FALSE)
    body_rows <- if (n == 0) character(0) else apply(frame, 1, paste, collapse = ",")
    path <- file.path(dir, sprintf("boundary_%d.csv", n))
    writeLines(c(paste(names(frame), collapse = ","), body_rows), path)

    case <- list(
      path = path,
      declared = basename(path),
      specs = DTAColumnSpecCollection(
        columns = qp_column_pool()[c("ID", "AGE")],
        rules = qp_rules(c("ID", "AGE"))
      ),
      rows = n
    )

    eager <- qp_run(case, "never")
    lazy <- qp_run(case, "always")

    qa_step(sprintf("validation status at %d row(s)", n), eager$status, lazy$status)
    qa_step(sprintf("sorted error detail at %d row(s)", n), eager$errors, lazy$errors)
    qa_step(sprintf("import-error count at %d row(s)", n), eager$n_import_errors, lazy$n_import_errors)
  }
})

# ---- the R-typed campaign -----------------------------------------------------

test_that("OQ-STREAM-024 | the eager and streaming engines agree on every generated R-typed table | REQ-STREAM-019 | tags: slow", {
  results <- qp_memory_results(qa_parity_seeds())
  for (entry in results) {
    label <- sprintf(
      "memory seed %d (%d rows, batch_rows %d)",
      entry$case$seed, entry$case$rows, entry$case$batch_rows
    )
    qa_step(paste("validation status:", label), entry$memory$status, entry$stream$status)
    qa_step(paste("sorted error detail:", label), entry$memory$errors, entry$stream$errors)
    qa_step(
      paste("import-error count:", label),
      entry$memory$n_import_errors, entry$stream$n_import_errors
    )
  }
})

test_that("OQ-STREAM-025 | the R-typed campaign carries the R types and axes it claims to | REQ-STREAM-019", {
  # Without this, OQ-STREAM-024 would compare two engines over tables that
  # happen to hold only character columns and prove nothing about R typing.
  results <- qp_memory_results(qa_parity_seeds())
  type_names <- c("double", "integer", "character", "factor", "logical", "Date", "POSIXct")
  found <- stats::setNames(rep(FALSE, length(type_names)), type_names)
  axes <- c(columnspec = FALSE, rule = FALSE)

  for (entry in results) {
    for (column in entry$case$frame) {
      cls <- class(column)[[1]]
      if (identical(cls, "numeric")) cls <- "double"
      if (cls %in% names(found)) found[[cls]] <- TRUE
    }
    errors <- entry$memory$errors
    if (nrow(errors) > 0) {
      sources <- as.character(errors$source)
      axes[["columnspec"]] <- axes[["columnspec"]] || any(sources == "columnspec")
      axes[["rule"]] <- axes[["rule"]] || any(sources == "rule")
    }
  }

  qa_step(
    "every R type the generator claims to exercise appears at least once",
    stats::setNames(rep(TRUE, length(type_names)), type_names), found
  )
  qa_check("the columnspec axis was exercised", axes[["columnspec"]])
  qa_check("the rule axis was exercised", axes[["rule"]])

  if (requireNamespace("bit64", quietly = TRUE)) {
    has_big <- vapply(results, function(entry) "BIG" %in% names(entry$case$frame), logical(1))
    qa_check(
      "bit64 is installed, so every generated table also carries an integer64 column",
      all(has_big)
    )
  }
})

test_that("OQ-STREAM-026 | the eager and streaming engines agree on an R-typed table with no rows and with one row | REQ-STREAM-020", {
  for (n in c(0L, 1L)) {
    columns <- c(
      list(ID = qp_memory_values("ID", n), AGE = qp_memory_values("AGE", n)),
      qp_memory_extras(n)
    )
    case <- list(
      frame = as.data.frame(columns, stringsAsFactors = FALSE),
      specs = DTAColumnSpecCollection(
        columns = qp_memory_pool()[c("ID", "AGE")],
        rules = qp_rules(c("ID", "AGE"))
      ),
      rows = n,
      batch_rows = 3L,
      seed = 900L + n
    )

    memory <- qp_run_memory(case, "memory")
    stream <- qp_run_memory(case, "stream")

    qa_step(sprintf("validation status at %d row(s)", n), memory$status, stream$status)
    qa_step(sprintf("sorted error detail at %d row(s)", n), memory$errors, stream$errors)
    qa_step(sprintf("import-error count at %d row(s)", n), memory$n_import_errors, stream$n_import_errors)
  }
})

# ---- the reduced, named divergence case ------------------------------------

test_that("OQ-STREAM-027 | an undeclared column read by a rule is keyed identically on both engines | REQ-STREAM-021", {
  # The reduced form of the campaign's core case, kept separate so a failure
  # names the mechanism rather than a seed: "1.5" and "1.50" are one number and
  # two strings. Because nothing declares XKEY, both engines are required to
  # key it as text, and the answer -- no duplicate -- has to be the same
  # whichever engine read the file.
  dir <- qa_tempdir()
  path <- file.path(dir, "undeclared_key.csv")
  writeLines(c("ID,XKEY", "A001,1.5", "A002,1.50", "A003,2"), path)

  specs <- DTAColumnSpecCollection(
    columns = list(
      ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE)
    ),
    rules = list(DTARuleColUnique(id = "xkey_unique", columns = "XKEY"))
  )

  case <- list(path = path, declared = basename(path), specs = specs, rows = 3)
  eager <- qp_run(case, "never")
  lazy <- qp_run(case, "always")

  qa_step("validation status agrees between the two engines", eager$status, lazy$status)
  qa_step("sorted error detail agrees between the two engines", eager$errors, lazy$errors)
  qa_step(
    "three textually distinct values are keyed as three distinct keys, so the table is valid on both",
    list(ok = TRUE, n_rule_errors = 0L),
    list(ok = eager$status$ok, n_rule_errors = as.integer(eager$status$n_rule_errors))
  )
})

# ---- batch-boundary cases ----------------------------------------------------

test_that("OQ-STREAM-028 | a column-specification violation at a batch boundary is reported identically by both engines | REQ-STREAM-022", {
  dir <- qa_tempdir()
  batch_rows <- 5L
  n <- 12L
  # The last row of batch 1, the first row of batch 2, and the last row of the
  # file (a short final batch of its own): the three positions a batch loop
  # keeping a running row offset is most likely to get wrong.
  boundary_rows <- c(batch_rows, batch_rows + 1L, n)

  specs <- DTAColumnSpecCollection(columns = list(
    SUBJID = DTAColumnSpec(id = "SUBJID", type = "SAS Char", length = 8, nullable = FALSE),
    SEX = DTAColumnSpec(id = "SEX", type = "SAS Char", length = 1, nullable = FALSE, values = c("M", "F"))
  ))
  frame <- data.frame(
    SUBJID = sprintf("S%07d", seq_len(n)), SEX = rep("M", n),
    stringsAsFactors = FALSE
  )
  frame$SEX[boundary_rows] <- "X"
  path <- file.path(dir, "boundary_columnspec.csv")
  utils::write.csv(frame, path, row.names = FALSE)
  case <- list(path = path, declared = basename(path), specs = specs, rows = n)

  eager <- qp_run(case, "never")
  lazy <- qp_run(case, "always", batch_rows = batch_rows)

  qa_step(
    "the eager and streaming engines report identical detail across the batch edges",
    eager$errors, lazy$errors
  )
  qa_step(
    "exactly the three planted rows are found, none lost, duplicated, or misnumbered",
    boundary_rows, sort(as.integer(eager$errors$row))
  )
  qa_step(
    "the column-specification count agrees between the two engines",
    eager$status$n_columnspec_errors, lazy$status$n_columnspec_errors
  )
  qa_step("and equals exactly three", 3L, as.integer(eager$status$n_columnspec_errors))
})

test_that("OQ-STREAM-029 | an import violation at a batch boundary is reported identically by both engines | REQ-STREAM-022", {
  dir <- qa_tempdir()
  batch_rows <- 5L
  n <- 12L
  boundary_rows <- c(batch_rows, batch_rows + 1L, n)

  specs <- DTAColumnSpecCollection(columns = list(
    SUBJID = DTAColumnSpec(id = "SUBJID", type = "SAS Char", length = 8, nullable = FALSE),
    AGE = DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
  ))
  frame <- data.frame(
    SUBJID = sprintf("S%07d", seq_len(n)), AGE = rep("30", n),
    stringsAsFactors = FALSE
  )
  frame$AGE[boundary_rows] <- "abc"
  path <- file.path(dir, "boundary_import.csv")
  utils::write.csv(frame, path, row.names = FALSE)
  case <- list(path = path, declared = basename(path), specs = specs, rows = n)

  eager <- qp_run(case, "never")
  lazy <- qp_run(case, "always", batch_rows = batch_rows)

  qa_step(
    "the eager and streaming engines report identical detail across the batch edges",
    eager$errors, lazy$errors
  )
  qa_step(
    "exactly the three planted unconvertible values are found, on the import axis",
    boundary_rows,
    sort(as.integer(eager$errors$row[eager$errors$source == "import"]))
  )
  qa_step("the import-error count agrees between the two engines", eager$n_import_errors, lazy$n_import_errors)
  qa_step("and equals exactly three", 3L, as.integer(eager$n_import_errors))
})

test_that("OQ-STREAM-030 | a duplicate key split across two batches is detected identically by both engines | REQ-STREAM-023", {
  dir <- qa_tempdir()
  batch_rows <- 5L
  n <- 12L

  specs <- DTAColumnSpecCollection(
    columns = list(SUBJID = DTAColumnSpec(id = "SUBJID", type = "SAS Char", length = 8, nullable = FALSE)),
    rules = list(DTARuleColUnique(id = "subjid_unique", columns = "SUBJID"))
  )
  frame <- data.frame(SUBJID = sprintf("S%07d", seq_len(n)), stringsAsFactors = FALSE)
  # Row 1 falls in batch 1 and row 7 in batch 2 (batch_rows = 5): finding the
  # match requires the streaming engine to carry every key forward across the
  # whole scan, not just within one batch.
  frame$SUBJID[7] <- frame$SUBJID[1]
  path <- file.path(dir, "duplicate_split.csv")
  utils::write.csv(frame, path, row.names = FALSE)
  case <- list(path = path, declared = basename(path), specs = specs, rows = n)

  eager <- qp_run(case, "never")
  lazy <- qp_run(case, "always", batch_rows = batch_rows)

  qa_step(
    "the eager and streaming engines report the same number of violated rules",
    eager$status$n_rule_errors, lazy$status$n_rule_errors
  )
  qa_step("exactly the one uniqueness rule is violated", 1L, as.integer(eager$status$n_rule_errors))
  qa_step("the reported detail agrees exactly", eager$errors, lazy$errors)
  qa_check(
    "and the table is invalid on both engines, or the case proves nothing",
    isFALSE(eager$status$ok) && isFALSE(lazy$status$ok)
  )
})

# ---- inspect() row context on a streamed table -----------------------------

test_that("OQ-STREAM-031 | inspect() reports the same row context for a streamed table as for the same data in memory | REQ-STREAM-024 | tags: white-box", {
  dir <- qa_tempdir()
  path <- file.path(dir, "inspect_parity.csv")
  writeLines(c(
    "SUBJECT_ID,VISIT,SCORE,STATUS",
    "S001,V1,50,OK", # clean
    "S002,V1,150,OK", # rule error: SCORE out of range
    "S003,V1,70,WRONG" # columnspec error: STATUS not in {OK, BAD}
  ), path)

  specs <- DTAColumnSpecCollection(
    columns = list(
      SUBJECT_ID = DTAColumnSpec(id = "SUBJECT_ID", type = "SAS Char", length = 4, nullable = FALSE),
      VISIT = DTAColumnSpec(id = "VISIT", type = "SAS Char", nullable = TRUE),
      SCORE = DTAColumnSpec(id = "SCORE", type = "SAS Num", nullable = TRUE),
      STATUS = DTAColumnSpec(id = "STATUS", type = "SAS Char", nullable = FALSE, values = c("OK", "BAD"))
    ),
    rules = list(DTARuleFactory("score_range", "col_range", columns = "SCORE", min = 0, max = 99))
  )

  ip_run <- function(stream) {
    ds <- DTADataSetTabular(
      name = "parity", specs = specs,
      files = list(DTAFileCSV(filename = basename(path)))
    )
    ds <- load_file(ds, file = path, handler_index = 1, stream = stream)
    check(ds, persist = FALSE, quiet = TRUE)
  }

  eager <- ip_run("never")
  lazy <- ip_run("always")

  # The table is keyed by what the reader derives from the delivered file,
  # not by the dataset's own name -- read it back rather than assume it.
  eager_table <- tables(eager)[[names(tables(eager))[[1]]]]
  lazy_table <- tables(lazy)[[names(tables(lazy))[[1]]]]

  # dta_table_is_lazy() is internal (R/streamingValidation.R); reached
  # directly here only to confirm the fixture actually exercises a lazily-
  # held table -- otherwise the comparison below would prove nothing.
  qa_check(
    "the eager table is in memory and the streamed one is genuinely lazy",
    !dta_table_is_lazy(eager_table) && dta_table_is_lazy(lazy_table)
  )

  ip_key <- function(msgs) paste(msgs$source, msgs$row, msgs$column, msgs$rule_id, sep = "|")
  eager_msgs <- messages(eager, as_tibble = FALSE)
  lazy_msgs <- messages(lazy, as_tibble = FALSE)
  eager_key <- ip_key(eager_msgs)
  lazy_key <- ip_key(lazy_msgs)

  qa_step(
    "eager and streamed report the same set of messages",
    sort(unique(eager_key)), sort(unique(lazy_key))
  )
  qa_check(
    "and both the rule and the column-specification axis are exercised, or the parity check below is incomplete",
    any(eager_msgs$source == "rule") && any(eager_msgs$source == "columnspec")
  )

  ip_as_char <- function(df) as.data.frame(lapply(df, as.character), stringsAsFactors = FALSE)

  for (k in unique(eager_key)) {
    eager_id <- eager_msgs$id[eager_key == k][[1]]
    lazy_id <- lazy_msgs$id[lazy_key == k][[1]]
    eager_info <- inspect(eager, id = eager_id, as_tibble = FALSE)
    lazy_info <- inspect(lazy, id = lazy_id, as_tibble = FALSE)

    # A columnspec message carries its row context under context_*; a rule
    # message instead carries a failing-row preview under failing_* (see
    # dta_inspect_tabular_message()). Whichever this message has, compare it.
    compare_cols <- intersect(
      grep("^(context_|failing_)", names(eager_info), value = TRUE),
      grep("^(context_|failing_)", names(lazy_info), value = TRUE)
    )
    qa_check(paste("row detail is reported on both engines for", k), length(compare_cols) > 0)
    qa_step(
      paste("and it matches between the eager and the streamed engine for", k),
      ip_as_char(eager_info[, compare_cols, drop = FALSE]),
      ip_as_char(lazy_info[, compare_cols, drop = FALSE])
    )
  }
})
