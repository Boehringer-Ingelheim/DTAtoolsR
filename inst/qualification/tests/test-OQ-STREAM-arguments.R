# validate_file_stream() argument by argument. REQ-STREAM-001 .. REQ-STREAM-017.
#
# Every case calls validate_file_stream() directly rather than through
# check(): it is the streaming engine's own public entry point, and an
# argument's effect is clearest shown where it is declared. Whether the
# streaming engine agrees with the eager one is a different question, answered
# by the randomised campaign in test-OQ-STREAM-parity.R; this file is only
# about what each argument of the streaming call itself does.

# ---- fixtures ---------------------------------------------------------------

sa_specs <- function(cols, rules = list()) {
  DTAColumnSpecCollection(
    columns = stats::setNames(cols, vapply(cols, function(x) x@id, character(1))),
    rules = rules
  )
}

# A small, three-column specification reused by several cases below: a
# non-nullable text key, a two-valued codelist, and a nullable number.
sa_base_specs <- function(rules = list()) {
  sa_specs(
    list(
      SUBJID = DTAColumnSpec(id = "SUBJID", type = "SAS Char", length = 8, nullable = FALSE),
      SEX = DTAColumnSpec(id = "SEX", type = "SAS Char", length = 1, nullable = FALSE, values = c("M", "F")),
      AGE = DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
    ),
    rules
  )
}

sa_write <- function(lines, dir, name = "t.csv") {
  path <- file.path(dir, name)
  writeLines(lines, path)
  path
}

# The structural detail of a streaming result, reduced to what a requirement
# is about and sorted so that two runs producing the same errors in a
# different order are not reported as disagreeing. Message text is excluded
# by default: it is prose, and what this file asserts is whether the detail
# is identical, not whether the wording is.
sa_found <- function(details, with_message = FALSE) {
  df <- as.data.frame(details)
  keep <- if (with_message) names(df) else setdiff(names(df), "message")
  df <- df[, intersect(keep, names(df)), drop = FALSE]
  if (nrow(df) == 0) {
    return(df)
  }
  df <- df[do.call(order, lapply(df, as.character)), , drop = FALSE]
  rownames(df) <- NULL
  df
}

# ---- reading the file: delim, quote, has_header ----------------------------

test_that("OQ-STREAM-001 | delim controls how a line is split into fields | REQ-STREAM-001", {
  dir <- qa_tempdir()
  specs <- sa_specs(list(
    ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE),
    NAME = DTAColumnSpec(id = "NAME", type = "SAS Char", length = 20, nullable = TRUE),
    AGE = DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
  ))
  path <- sa_write(c("ID;NAME;AGE", "A001;Smith;30", "A002;Jones;40"), dir, "semi.csv")

  right <- validate_file_stream(specs, path, delim = ";", verbose = FALSE)
  qa_step(
    "the file's own delimiter reads it cleanly",
    list(ok = TRUE, n = 0L),
    list(ok = right$ok, n = as.integer(right$n_columnspec_errors))
  )

  wrong <- validate_file_stream(specs, path, delim = ",", verbose = FALSE)
  qa_step(
    "the wrong delimiter reads each whole line as one field: every declared column is absent from every row (2 rows x 3 columns), plus the one undeclared column",
    7L, as.integer(wrong$n_columnspec_errors)
  )
  qa_check("and the table is reported invalid", isFALSE(wrong$ok))
})

test_that("OQ-STREAM-002 | quote keeps a delimiter-holding field together; disabling it breaks the row | REQ-STREAM-002", {
  dir <- qa_tempdir()
  specs <- sa_specs(list(
    ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE),
    NAME = DTAColumnSpec(id = "NAME", type = "SAS Char", length = 20, nullable = TRUE),
    AGE = DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
  ))
  path <- sa_write(c(
    "ID,NAME,AGE",
    '"A001","Smith, Jr.",30',
    '"A002","Jones",40'
  ), dir, "quote.csv")

  quoted <- validate_file_stream(specs, path, quote = "\"", verbose = FALSE)
  qa_step(
    "a comma inside a quoted field does not split the row",
    list(ok = TRUE, n = 0L),
    list(ok = quoted$ok, n = as.integer(quoted$n_columnspec_errors))
  )

  err <- tryCatch(
    validate_file_stream(specs, path, quote = "", verbose = FALSE),
    error = function(e) e
  )
  qa_check(
    "disabling quoting on a row that relies on it to keep a field together raises a condition",
    inherits(err, "condition")
  )
})

test_that("OQ-STREAM-003 | a non-default quote character is honoured, and the default is not silently substituted | REQ-STREAM-002", {
  dir <- qa_tempdir()
  specs <- sa_specs(list(
    ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE),
    NAME = DTAColumnSpec(id = "NAME", type = "SAS Char", length = 20, nullable = TRUE),
    AGE = DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
  ))
  path <- sa_write(c(
    "ID,NAME,AGE",
    "'A001','Smith, Jr.',30",
    "'A002','Jones',40"
  ), dir, "singlequote.csv")

  right <- validate_file_stream(specs, path, quote = "'", verbose = FALSE)
  qa_step(
    "the declared quote character keeps the comma-holding field together",
    list(ok = TRUE, n = 0L),
    list(ok = right$ok, n = as.integer(right$n_columnspec_errors))
  )

  err <- tryCatch(
    validate_file_stream(specs, path, quote = "\"", verbose = FALSE),
    error = function(e) e
  )
  qa_check(
    "the default double quote does not recognise the single-quoted field, and the same row breaks",
    inherits(err, "condition")
  )
})

test_that("OQ-STREAM-004 | has_header decides whether the first line is names or data | REQ-STREAM-003", {
  dir <- qa_tempdir()
  specs <- sa_specs(list(
    ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE),
    AGE = DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
  ))
  data_lines <- c("A001,30", "A002,40")

  with_header <- sa_write(c("ID,AGE", data_lines), dir, "headed.csv")
  headed <- validate_file_stream(specs, with_header, has_header = TRUE, verbose = FALSE)
  qa_step(
    "with a header, the two data rows validate cleanly",
    list(ok = TRUE, n = 0L),
    list(ok = headed$ok, n = as.integer(headed$n_columnspec_errors))
  )

  without_header <- sa_write(data_lines, dir, "headless.csv")
  headless <- validate_file_stream(specs, without_header, has_header = FALSE, verbose = FALSE)
  qa_step(
    "without a header both lines become data: every declared column is absent from every row (2 rows x 2 columns), plus the two automatically named columns are undeclared",
    6L, as.integer(headless$n_columnspec_errors)
  )
  qa_step("and both lines were read as rows", 2L, as.integer(attr(headless, "n_rows_scanned")))
  qa_step(
    "the automatically named columns are the ones reported as undeclared",
    c("f0", "f1"),
    sort(sa_found(headless)$column[sa_found(headless)$keyword == "additionalProperties"])
  )
})

# ---- batch_rows: memory scheduling, not a second answer --------------------

test_that("OQ-STREAM-005 | batch_rows changes scheduling only: three sizes reach the identical verdict | REQ-STREAM-004", {
  dir <- qa_tempdir()
  specs <- qa_specs()
  path <- file.path(dir, "big.csv")
  qa_write_csv(qa_frame(300L, seed = 7L), path)
  oracle <- qa_oracle()

  sizes <- c(3L, 64L, 131072L)
  results <- lapply(sizes, function(b) {
    validate_file_stream(specs, path, batch_rows = b, verbose = FALSE)
  })

  for (i in seq_along(sizes)) {
    qa_step(
      sprintf("batch_rows = %d reaches the fixture's known verdict", sizes[[i]]),
      list(
        ok = oracle$ok, cs = oracle$n_columnspec_errors,
        rule = oracle$n_rule_errors, imp = oracle$n_import_errors
      ),
      list(
        ok = results[[i]]$ok, cs = as.integer(results[[i]]$n_columnspec_errors),
        rule = as.integer(results[[i]]$n_rule_errors), imp = as.integer(results[[i]]$n_import_errors)
      )
    )
  }

  reference <- sa_found(results[[1]])
  for (i in 2:length(sizes)) {
    qa_step(
      sprintf(
        "the reported detail at batch_rows = %d matches batch_rows = %d exactly",
        sizes[[i]], sizes[[1]]
      ),
      reference, sa_found(results[[i]])
    )
  }
})

# ---- max_errors: a cap on retained detail, never on a count ----------------

test_that("OQ-STREAM-006 | max_errors caps retained detail on the columnspec axis while the axis count stays exact | REQ-STREAM-005 REQ-STREAM-006 REQ-STREAM-007", {
  dir <- qa_tempdir()
  specs <- sa_specs(list(
    ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE)
  ))
  path <- file.path(dir, "dirty.csv")
  utils::write.csv(data.frame(ID = c("TOOLONG1", "TOOLONG2", "TOOLONG3")), path, row.names = FALSE)

  details <- validate_file_stream(specs, path, max_errors = 1, verbose = FALSE)
  qa_step("the exact count is unaffected by the cap", 3L, as.integer(details$n_columnspec_errors))

  head <- details$columnspec_errors$full_error
  qa_step("only the cap's worth is retained in memory", 1L, nrow(head))
  qa_check("the retained head is flagged truncated", isTRUE(attr(head, "truncated", exact = TRUE)))
  qa_step("and it names how many more rows were spilled", 2, attr(head, "spilled_rows", exact = TRUE))

  full <- collect_full_errors(details, axis = "columnspec")
  qa_step("collect_full_errors() reassembles every row the cap left out", 3L, nrow(full))
  qa_check(
    "and the reassembled frame is no longer flagged truncated",
    is.null(attr(full, "truncated", exact = TRUE))
  )
})

test_that("OQ-STREAM-007 | max_errors caps the import and import-typing axes the same way | REQ-STREAM-005 REQ-STREAM-006 REQ-STREAM-007", {
  dir <- qa_tempdir()
  specs <- sa_specs(list(
    ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE),
    AGE = DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
  ))
  path <- file.path(dir, "importdirty.csv")
  utils::write.csv(data.frame(ID = c("A1", "A2", "A3"), AGE = c("x", "y", "z")), path, row.names = FALSE)

  details <- validate_file_stream(specs, path, max_errors = 1, verbose = FALSE)
  qa_step("the exact import count is unaffected by the cap", 3L, as.integer(details$n_import_errors))

  import_head <- details$import_errors
  typing_head <- details$import_typing_errors
  qa_step("the retained import head holds only the cap's worth", 1L, nrow(import_head))
  qa_step("the retained import-typing head holds only the cap's worth", 1L, nrow(typing_head))
  qa_check("the import head is flagged truncated", isTRUE(attr(import_head, "truncated", exact = TRUE)))
  qa_check("the import-typing head is flagged truncated", isTRUE(attr(typing_head, "truncated", exact = TRUE)))

  qa_step(
    "collect_full_errors() reassembles the whole merged import axis",
    3L, nrow(collect_full_errors(details, "import"))
  )
  qa_step(
    "and the import-typing axis on its own",
    3L, nrow(collect_full_errors(details, "import_typing"))
  )
})

test_that("OQ-STREAM-008 | max_errors = NULL retains every error row | REQ-STREAM-005", {
  dir <- qa_tempdir()
  specs <- sa_specs(list(
    ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE)
  ))
  path <- file.path(dir, "dirty.csv")
  utils::write.csv(data.frame(ID = c("TOOLONG1", "TOOLONG2", "TOOLONG3")), path, row.names = FALSE)

  details <- validate_file_stream(specs, path, max_errors = NULL, verbose = FALSE)
  full <- details$columnspec_errors$full_error
  qa_step("nothing is capped: the retained frame already holds every row", 3L, nrow(full))
  qa_check("so nothing needed to spill", is.null(attr(full, "truncated", exact = TRUE)))
})

# ---- stopping early, and columns the file does not have --------------------

test_that("OQ-STREAM-009 | fail_fast stops at the first batch with a problem, and settles nothing past it | REQ-STREAM-008 REQ-STREAM-014", {
  dir <- qa_tempdir()
  specs <- sa_base_specs(list(DTARuleColRange(id = "age_range", columns = "AGE", range = c(18, 70))))
  n <- 20L
  frame <- data.frame(
    SUBJID = sprintf("S%07d", seq_len(n)), SEX = rep("M", n), AGE = rep("30", n),
    stringsAsFactors = FALSE
  )
  frame$SEX[c(3, 10, 17)] <- "X"
  path <- file.path(dir, "t.csv")
  utils::write.csv(frame, path, row.names = FALSE)

  stopped <- validate_file_stream(specs, path, batch_rows = 5L, fail_fast = TRUE, verbose = FALSE)
  qa_step("the scan stops as soon as the first batch shows a problem", FALSE, stopped$ok)
  qa_check("the result is flagged as a partial scan", isTRUE(attr(stopped, "partial_scan", exact = TRUE)))
  qa_step("only the first batch was read", 5L, as.integer(attr(stopped, "n_rows_scanned")))
  qa_step("only the defect inside that batch was found", 1L, as.integer(stopped$n_columnspec_errors))
  qa_check(
    "a rule that has not failed yet is reported as unsettled, not as passed",
    is.na(stopped$rules_valid)
  )

  clean <- frame
  clean$SEX <- "M"
  clean_path <- file.path(dir, "clean.csv")
  utils::write.csv(clean, clean_path, row.names = FALSE)
  completed <- validate_file_stream(specs, clean_path, batch_rows = 5L, fail_fast = TRUE, verbose = FALSE)
  qa_step("a scan with nothing to stop for runs to completion", TRUE, completed$ok)
  qa_step("every row was read", n, as.integer(attr(completed, "n_rows_scanned")))
  qa_check(
    "and it carries no partial_scan flag at all",
    is.null(attr(completed, "partial_scan", exact = TRUE))
  )
})

test_that("OQ-STREAM-010 | on_missing_column scans and restates the absence per row, or stops and reports it once | REQ-STREAM-009 REQ-STREAM-014", {
  dir <- qa_tempdir()
  specs <- sa_base_specs()

  for (n in c(1L, 2L)) {
    lines <- c("SUBJID,SEX", sprintf("S%07d,M", seq_len(n)))
    path <- sa_write(lines, dir, sprintf("missing%d.csv", n))

    scanned <- validate_file_stream(specs, path, on_missing_column = "scan", verbose = FALSE)
    qa_step(
      sprintf("'scan' restates the absent column once per row, at %d row(s)", n),
      n, as.integer(scanned$n_columnspec_errors)
    )
    qa_step(
      sprintf("and reads every row, at %d row(s)", n),
      n, as.integer(attr(scanned, "n_rows_scanned"))
    )
    qa_check(
      "a completed scan carries no structural_only flag",
      is.null(attr(scanned, "structural_only", exact = TRUE))
    )

    stopped <- validate_file_stream(specs, path, on_missing_column = "stop", verbose = FALSE)
    qa_step(
      sprintf("'stop' reports the same absence once, regardless of row count (%d row(s))", n),
      1L, as.integer(stopped$n_columnspec_errors)
    )
    qa_step(
      sprintf("and reads nothing, at %d row(s)", n),
      0L, as.integer(attr(stopped, "n_rows_scanned"))
    )
    qa_check(
      "a structural report is flagged structural_only",
      isTRUE(attr(stopped, "structural_only", exact = TRUE))
    )
  }
})

# ---- threads, console output and instrumentation ----------------------------

test_that("OQ-STREAM-011 | use_threads changes scheduling only: the reported detail is identical | REQ-STREAM-010", {
  dir <- qa_tempdir()
  specs <- qa_specs()
  path <- file.path(dir, "t.csv")
  qa_write_csv(qa_frame(300L, seed = 11L), path)

  threaded <- validate_file_stream(specs, path, use_threads = TRUE, verbose = FALSE)
  unthreaded <- validate_file_stream(specs, path, use_threads = FALSE, verbose = FALSE)

  qa_step(
    "the structural detail does not depend on whether scanning is threaded",
    sa_found(threaded), sa_found(unthreaded)
  )
  qa_step(
    "nor do the three axis counts",
    list(
      ok = threaded$ok, cs = as.integer(threaded$n_columnspec_errors),
      rule = as.integer(threaded$n_rule_errors), imp = as.integer(threaded$n_import_errors)
    ),
    list(
      ok = unthreaded$ok, cs = as.integer(unthreaded$n_columnspec_errors),
      rule = as.integer(unthreaded$n_rule_errors), imp = as.integer(unthreaded$n_import_errors)
    )
  )
})

test_that("OQ-STREAM-012 | verbose is console output only: nothing when FALSE, the undeclared column named when TRUE | REQ-STREAM-011", {
  dir <- qa_tempdir()
  specs <- sa_base_specs()
  path <- sa_write(c("SUBJID,SEX,AGE,EXTRA", "S0000001,M,30,zz"), dir, "extra.csv")

  quiet_lines <- capture.output(
    quiet_details <- validate_file_stream(specs, path, verbose = FALSE),
    type = "message"
  )
  qa_step("verbose = FALSE prints nothing", 0L, length(quiet_lines))

  loud_lines <- capture.output(
    loud_details <- validate_file_stream(specs, path, verbose = TRUE),
    type = "message"
  )
  qa_check("verbose = TRUE prints at least one line", length(loud_lines) > 0)
  qa_check(
    "and names the column the specs do not describe",
    any(grepl("EXTRA", loud_lines, fixed = TRUE))
  )
  qa_step(
    "verbose changes only what is printed, not the verdict",
    list(ok = quiet_details$ok, n = as.integer(quiet_details$n_columnspec_errors)),
    list(ok = loud_details$ok, n = as.integer(loud_details$n_columnspec_errors))
  )
})

test_that("OQ-STREAM-013 | benchmark attaches the thirteen documented metrics at metrics_version 1 | REQ-STREAM-012", {
  dir <- qa_tempdir()
  specs <- sa_base_specs()
  path <- sa_write(c("SUBJID,SEX,AGE", "S0000001,M,30"), dir, "t.csv")

  expected_columns <- c(
    "elapsed_sec", "cpu_user_sec", "cpu_sys_sec", "r_peak_mb", "rss_start_mb",
    "rss_end_mb", "arrow_pool_peak_mb", "arrow_call_mb", "arrow_call_exact",
    "rows", "rows_per_sec", "benchmarked_at", "metrics_version"
  )

  via_argument <- validate_file_stream(specs, path, benchmark = TRUE, verbose = FALSE)
  bm <- validation_benchmark(via_argument)
  qa_step("the metrics carry exactly the documented columns", sort(expected_columns), sort(names(bm)))
  qa_step("at metrics_version 1", 1L, bm$metrics_version)
  qa_step("naming the row count the scan actually read", 1, bm$rows)

  qa_check(
    "a call that does not ask for a benchmark attaches none",
    is.null(validation_benchmark(validate_file_stream(specs, path, verbose = FALSE)))
  )

  old <- options(DTAtools.benchmark = TRUE)
  via_option <- validate_file_stream(specs, path, verbose = FALSE)
  options(old)
  qa_check(
    "options(DTAtools.benchmark = TRUE) attaches a benchmark without the argument",
    !is.null(validation_benchmark(via_option))
  )
})

test_that("OQ-STREAM-014 | a path that does not exist raises a condition | REQ-STREAM-013", {
  dir <- qa_tempdir()
  specs <- sa_base_specs()
  missing_path <- file.path(dir, "does-not-exist.csv")

  err <- tryCatch(
    validate_file_stream(specs, missing_path, verbose = FALSE),
    error = function(e) e
  )
  qa_check("a missing file raises a condition rather than returning a result", inherits(err, "condition"))
  qa_check(
    "naming the path, per the package's own cli text",
    grepl("File not found", conditionMessage(err), fixed = TRUE)
  )
})

test_that("OQ-STREAM-015 | a complete scan reports the exact number of rows it read | REQ-STREAM-014", {
  dir <- qa_tempdir()
  specs <- sa_base_specs()
  n <- 7L
  lines <- c("SUBJID,SEX,AGE", sprintf("S%07d,M,30", seq_len(n)))
  path <- sa_write(lines, dir, "t.csv")

  details <- validate_file_stream(specs, path, verbose = FALSE)
  qa_step(
    "a complete scan reports exactly the rows it read",
    n, as.integer(attr(details, "n_rows_scanned"))
  )
  qa_check("and the scan is not flagged partial", is.null(attr(details, "partial_scan", exact = TRUE)))
})

# ---- an alternative on-disk format, and the compute engine ------------------

test_that("OQ-STREAM-016 | cache_as_parquet() reaches the same verdict as the delimited file it was built from | REQ-STREAM-015", {
  dir <- qa_tempdir()
  specs <- sa_specs(
    list(
      ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE),
      AGE = DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
    ),
    list(DTARuleColUnique(id = "id_unique", columns = "ID"))
  )
  path <- file.path(dir, "src.csv")
  utils::write.csv(
    data.frame(ID = c("A1", "TOOLONG1", "A1"), AGE = c("30", "40", "abc")),
    path,
    row.names = FALSE
  )

  from_csv <- validate_file_stream(specs, path, verbose = FALSE)
  cache_dir <- file.path(dir, "cache_parquet")
  cache_as_parquet(specs, path, cache_path = cache_dir)
  from_parquet <- validate_file_stream(specs, cache_dir, verbose = FALSE)

  verdict <- function(d) {
    list(
      ok = d$ok, cs = as.integer(d$n_columnspec_errors),
      rule = as.integer(d$n_rule_errors), imp = as.integer(d$n_import_errors)
    )
  }
  qa_step(
    "the cached Parquet dataset reaches the same verdict as its source CSV on every axis",
    verdict(from_csv), verdict(from_parquet)
  )
  qa_check("and the verdict is actually invalid, or the comparison proves nothing", isFALSE(from_csv$ok))
})

test_that("OQ-STREAM-017 | cache_as_parquet()'s compression codec does not change the verdict | REQ-STREAM-015", {
  dir <- qa_tempdir()
  specs <- sa_specs(
    list(ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE)),
    list(DTARuleColUnique(id = "id_unique", columns = "ID"))
  )
  path <- file.path(dir, "dup.csv")
  utils::write.csv(data.frame(ID = c("A001", "A002", "A002")), path, row.names = FALSE)
  from_csv <- validate_file_stream(specs, path, verbose = FALSE)

  for (codec in c("snappy", "uncompressed")) {
    cache_dir <- file.path(dir, paste0("cache_", codec))
    cache_as_parquet(specs, path, cache_path = cache_dir, compression = codec)
    from_cache <- validate_file_stream(specs, cache_dir, verbose = FALSE)
    qa_step(
      sprintf("compression = '%s' reaches the same verdict as the default", codec),
      list(ok = from_csv$ok, rule = as.integer(from_csv$n_rule_errors)),
      list(ok = from_cache$ok, rule = as.integer(from_cache$n_rule_errors))
    )
  }
})

test_that("OQ-STREAM-018 | set_dta_compute_threads() round-trips a thread count and leaves the verdict it governs unchanged | REQ-STREAM-016", {
  qa_requires("arrow")
  dir <- qa_tempdir()
  specs <- sa_specs(
    list(ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE)),
    list(DTARuleColUnique(id = "id_unique", columns = "ID"))
  )
  path <- file.path(dir, "dup.csv")
  utils::write.csv(data.frame(ID = c("A001", "A002", "A002")), path, row.names = FALSE)

  before <- validate_file_stream(specs, path, verbose = FALSE)

  original <- arrow::cpu_count()
  target <- if (identical(original, 3L)) 4L else 3L
  previous <- set_dta_compute_threads(target)
  qa_step("the previously active thread count is returned", original, previous)
  qa_step("the thread count is updated to the requested value", target, arrow::cpu_count())

  during <- validate_file_stream(specs, path, verbose = FALSE)
  qa_step(
    "the verdict does not depend on the compute thread count",
    list(ok = before$ok, rule = as.integer(before$n_rule_errors)),
    list(ok = during$ok, rule = as.integer(during$n_rule_errors))
  )

  set_dta_compute_threads(previous)
  qa_step("the thread count is restored exactly", original, arrow::cpu_count())
})

test_that("OQ-STREAM-019 | DTAtools.use_arrow_compute reaches an identical verdict to the plain R engine | REQ-STREAM-017", {
  qa_requires("arrow")
  specs <- sa_specs(
    list(ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE)),
    list(DTARuleColUnique(id = "id_unique", columns = "ID"))
  )
  frame <- data.frame(ID = c("A001", "A002", "A002", "A003", "A004"), stringsAsFactors = FALSE)
  ds <- DTADataSetTabular(name = "dup", specs = specs, tables = list(t = frame))

  old <- options(DTAtools.arrow_min_rows = 1L, DTAtools.use_arrow_compute = TRUE)
  arrow_checked <- check(ds, quiet = TRUE, persist = FALSE)
  options(DTAtools.use_arrow_compute = FALSE)
  r_checked <- check(ds, quiet = TRUE, persist = FALSE)
  options(old)

  qa_step(
    "the Arrow-accelerated duplicate count and the plain R one agree exactly",
    qa_messages_norm(r_checked), qa_messages_norm(arrow_checked)
  )
  qa_check(
    "and the rule actually did violate, or the comparison proves nothing",
    !validation_status(r_checked)$ok
  )
})
