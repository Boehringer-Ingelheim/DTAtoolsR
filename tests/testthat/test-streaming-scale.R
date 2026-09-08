# The scale and stability work on the streaming path: Arrow-side uniqueness,
# scan projection, all-utf8 pinning, error-detail spill, and the crashes that
# used to end a scan (empty-string keys, over-integer counts).

ss_write_csv <- function(df, name) {
  path <- file.path(tempdir(), name)
  utils::write.csv(df, path, row.names = FALSE, na = "")
  path
}

test_that("Arrow-side uniqueness agrees with the materialised verdict, engine on or off", {
  specs <- vc_specs(
    list(
      DTAColumnSpec(id = "K", type = "SAS Char", length = 8, nullable = TRUE),
      DTAColumnSpec(id = "V", type = "SAS Char", length = 8, nullable = TRUE)
    ),
    list(DTARuleColUnique(id = "kv_unique", columns = c("K", "V")))
  )

  # Duplicates across batches, and an all-missing key pair that duplicated()
  # counts as a duplicate too.
  df <- data.frame(
    K = c("A", "B", "A", NA, NA, "C"),
    V = c("x", "y", "x", NA, NA, "z"),
    stringsAsFactors = FALSE
  )
  path <- ss_write_csv(df, "ss_unique.csv")
  on.exit(unlink(path), add = TRUE)

  expected <- rule_check_unique(specs@rules[[1]], df)
  expect_false(expected$valid)

  for (engine_on in c(TRUE, FALSE)) {
    withr::local_options(DTAtools.stream_arrow_unique = engine_on)
    details <- validate_file_stream(
      specs, path,
      batch_rows = 2L, verbose = FALSE
    )
    expect_false(details$rules_valid, info = paste("engine on:", engine_on))
    expect_length(details$rule_errors, 1)
    expect_identical(
      details$rule_errors[[1]]$message, expected$message,
      info = paste("engine on:", engine_on)
    )
  }
})

test_that("the Arrow uniqueness path is actually taken for a text-keyed Dataset", {
  rule <- DTARuleColUnique(id = "k_unique", columns = "K")
  specs <- vc_specs(
    list(DTAColumnSpec(id = "K", type = "SAS Char", length = 8, nullable = FALSE)),
    list(rule)
  )
  path <- ss_write_csv(
    data.frame(K = c("a", "b", "a"), stringsAsFactors = FALSE),
    "ss_unique_taken.csv"
  )
  on.exit(unlink(path), add = TRUE)

  dataset <- dta_open_normalized_dataset(path, specs = specs)
  eligibility <- dta_arrow_unique_eligible(rule, dataset, specs)
  expect_true(eligibility$ok)
  # A text key is grouped exactly as it is read; nothing needs normalising.
  expect_identical(eligibility$numeric_columns, character(0))

  pre <- dta_stream_unique_precompute(specs, dataset, specs@rules)
  expect_length(pre, 1)
  expect_false(is.null(pre[[1]]))
  expect_false(pre[[1]]$valid)
  expect_match(pre[[1]]$message, "1 duplicate row")

  # A consumable reader must never be precomputed: the pass would spend it.
  reader <- dta_as_batch_reader(data.frame(K = "a"), batch_rows = 1L)
  pre_reader <- dta_stream_unique_precompute(specs, reader, specs@rules)
  expect_true(is.null(pre_reader[[1]]))
})

test_that("the Arrow uniqueness path is taken for a declared-numeric key too", {
  # Every streamed column is utf8 at scan time, so the schema type alone cannot
  # tell a text key from a declared-Num one -- and the two are keyed
  # differently: the per-batch path keys a declared-Num column on its coerced
  # NUMBERS, where "1.50" and "1.5" are one key, while raw text grouping sees
  # two. Eligibility reports which key columns are numeric so the precompute can
  # group them on the same values.
  rule <- DTARuleColUnique(id = "n_unique", columns = "N")
  specs <- vc_specs(
    list(
      DTAColumnSpec(id = "N", type = "SAS Num", nullable = TRUE),
      DTAColumnSpec(id = "S", type = "SAS Char", length = 4, nullable = TRUE)
    ),
    list(rule)
  )
  df <- data.frame(N = c("1.50", "1.5", "2"), S = c("a", "b", "c"), stringsAsFactors = FALSE)
  path <- ss_write_csv(df, "ss_declared_num_key.csv")
  on.exit(unlink(path), add = TRUE)

  dataset <- dta_open_normalized_dataset(path, specs = specs)
  eligibility <- dta_arrow_unique_eligible(rule, dataset, specs)
  expect_true(eligibility$ok)
  expect_identical(eligibility$numeric_columns, "N")

  pre <- dta_stream_unique_precompute(specs, dataset, specs@rules)
  expect_false(is.null(pre[[1]]))

  coerced <- dta_coerce_table_to_specs(df, specs)
  expected <- rule_check_unique(rule, as.data.frame(coerced$table))
  expect_false(expected$valid)
  expect_identical(pre[[1]]$message, expected$message)

  details <- validate_file_stream(specs, path, batch_rows = 1L, verbose = FALSE)
  expect_false(details$rules_valid)
  expect_identical(details$rule_errors[[1]]$message, expected$message)
})

test_that("a declared-Int key is grouped on numbers as well", {
  rule <- DTARuleColUnique(id = "i_unique", columns = "I")
  specs <- vc_specs(
    list(
      DTAColumnSpec(id = "I", type = "SAS Int", nullable = TRUE),
      DTAColumnSpec(id = "S", type = "SAS Char", length = 4, nullable = TRUE)
    ),
    list(rule)
  )
  path <- ss_write_csv(
    data.frame(I = c("07", "7", "8"), S = c("a", "b", "c"), stringsAsFactors = FALSE),
    "ss_declared_int_key.csv"
  )
  on.exit(unlink(path), add = TRUE)

  dataset <- dta_open_normalized_dataset(path, specs = specs)
  expect_identical(dta_arrow_unique_eligible(rule, dataset, specs)$numeric_columns, "I")

  # "07" and "7" are one number and therefore one key; as raw text they are two.
  pre <- dta_stream_unique_precompute(specs, dataset, specs@rules)
  expect_false(is.null(pre[[1]]))
  expect_match(pre[[1]]$message, "1 duplicate row")
})

test_that("a numeric key value Acero cannot parse falls back without erroring", {
  # Acero's string-to-double cast raises rather than guessing, which is what
  # makes the path safe: an unparseable value is not a wrong key, it is a
  # refusal that routes the rule to the per-batch accumulator -- which reads
  # that value as NA and reports it on the import axis instead.
  rule <- DTARuleColUnique(id = "n_unique", columns = "N")
  specs <- vc_specs(
    list(
      DTAColumnSpec(id = "N", type = "SAS Num", nullable = TRUE),
      DTAColumnSpec(id = "S", type = "SAS Char", length = 4, nullable = TRUE)
    ),
    list(rule)
  )
  df <- data.frame(N = c("abc", "1", "1"), S = c("a", "b", "c"), stringsAsFactors = FALSE)
  path <- ss_write_csv(df, "ss_unparseable_num_key.csv")
  on.exit(unlink(path), add = TRUE)

  dataset <- dta_open_normalized_dataset(path, specs = specs)
  # Eligible -- the source and schema are right -- but the computation refuses.
  expect_true(dta_arrow_unique_eligible(rule, dataset, specs)$ok)
  expect_no_error(pre <- dta_stream_unique_precompute(specs, dataset, specs@rules))
  expect_true(is.null(pre[[1]]))

  coerced <- dta_coerce_table_to_specs(df, specs)
  expected <- rule_check_unique(rule, as.data.frame(coerced$table))
  details <- validate_file_stream(specs, path, batch_rows = 1L, verbose = FALSE)
  expect_identical(details$rule_errors[[1]]$message, expected$message)
})

test_that("randomised numeric keys agree across the Arrow, per-batch and eager paths", {
  # The differential test the Acero numeric key rests on. The pool is chosen so
  # that every hazard is reachable: values that are one number and two strings
  # ("1.5"/"1.50", "1e2"/"100"), the -0/0 fold, NaN against a missing value,
  # and values Acero refuses to parse at all (" 3", "abc", "0x10") which must
  # produce a fallback rather than a wrong answer.
  # Split in two deliberately. Drawn from one pool holding the refused values,
  # a table of any size almost always contains one, and every seed would fall
  # back -- the loop would then never once exercise the Arrow path it exists to
  # check. Half the seeds therefore draw only from values both engines parse.
  parseable <- c("1.5", "1.50", "2", "2.0", "-0", "0", "NaN", "", NA, "1e2", "100")
  refused <- c(" 3", "abc", "0x10")

  arrow_answers <- 0L

  for (seed in seq_len(20L)) {
    withr::local_seed(seed)

    pool <- if (seed %% 2L == 0L) parseable else c(parseable, refused)
    n_keys <- sample(1:2, 1)
    key_cols <- c("K1", "K2")[seq_len(n_keys)]
    n_rows <- sample(4:30, 1)

    df <- as.data.frame(
      stats::setNames(
        lapply(key_cols, function(nm) sample(pool, n_rows, replace = TRUE)),
        key_cols
      ),
      stringsAsFactors = FALSE
    )
    # A filler column keeps every line populated: a row whose only values are
    # missing serialises to a blank line, which every CSV parser skips.
    df$S <- paste0("s", seq_len(n_rows))

    specs <- vc_specs(
      c(
        lapply(key_cols, function(nm) DTAColumnSpec(id = nm, type = "SAS Num", nullable = TRUE)),
        list(DTAColumnSpec(id = "S", type = "SAS Char", length = 8, nullable = FALSE))
      ),
      list(DTARuleColUnique(id = "k_unique", columns = key_cols))
    )
    rule <- specs@rules[[1]]

    path <- ss_write_csv(df, "ss_numeric_key_fuzz.csv")
    dataset <- dta_open_normalized_dataset(path, specs = specs)

    # The eager oracle, read through the same dataset so the two paths cannot
    # disagree about what the file contains before they disagree about keys.
    materialised <- dta_coerce_table_to_specs(
      as.data.frame(dplyr::collect(dataset)), specs
    )$table
    eager <- rule_check_unique(rule, as.data.frame(materialised))

    per_batch <- withr::with_options(
      list(DTAtools.stream_arrow_unique = FALSE),
      validate_file_stream(specs, path, batch_rows = 3L, verbose = FALSE)
    )
    streamed <- validate_file_stream(specs, path, batch_rows = 3L, verbose = FALSE)
    pre <- dta_stream_unique_precompute(specs, dataset, list(rule))[[1]]

    unlink(path)

    verdict <- function(details) {
      if (length(details$rule_errors) == 0) NA_character_ else details$rule_errors[[1]]$message
    }
    eager_message <- if (isTRUE(eager$valid)) NA_character_ else eager$message

    info <- paste("seed", seed)
    expect_identical(verdict(per_batch), eager_message, info = info)
    expect_identical(verdict(streamed), eager_message, info = info)

    if (!is.null(pre)) {
      arrow_answers <- arrow_answers + 1L
      pre_message <- if (isTRUE(pre$valid)) NA_character_ else pre$message
      expect_identical(pre_message, eager_message, info = info)
    }
  }

  # Without this the whole loop could pass by never taking the Arrow path: a
  # precompute that always returned NULL would be comparing the per-batch path
  # against itself.
  expect_gte(arrow_answers, 10L)
})

test_that("an empty-string key value no longer aborts the scan", {
  # fastmap rejects "" as a key, and the failure escaped every handler: one
  # blank cell in a key column crashed check() while the in-memory path
  # returned a verdict. The key encoding now remaps "" injectively.
  rule <- DTARuleColUnique(id = "k_unique", columns = "K")
  specs <- vc_specs(
    list(DTAColumnSpec(id = "K", type = "SAS Char", length = 8, nullable = TRUE)),
    list(rule)
  )
  df <- data.frame(K = c("", "A", ""), stringsAsFactors = FALSE)

  expected <- rule_check_unique(rule, df)

  details <- dta_validate_table_stream(
    specs, dta_as_batch_reader(df, batch_rows = 1L),
    verbose = FALSE, coerce = FALSE
  )
  expect_false(details$rules_valid)
  expect_identical(details$rule_errors[[1]]$message, expected$message)

  # The remap must not merge "" with any real value.
  clean <- dta_validate_table_stream(
    vc_specs(
      list(DTAColumnSpec(id = "K", type = "SAS Char", length = 8, nullable = TRUE)),
      list(rule)
    ),
    dta_as_batch_reader(
      data.frame(K = c("", "A"), stringsAsFactors = FALSE),
      batch_rows = 1L
    ),
    verbose = FALSE, coerce = FALSE
  )
  expect_true(clean$rules_valid)
})

test_that("projection leaves the verdict untouched and rules may read undeclared columns", {
  specs <- vc_specs(
    list(
      DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE),
      DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
    ),
    list(
      DTARuleColRange(id = "age_range", columns = "AGE", range = c(18, 70)),
      # JUNK1 is declared by no spec; projection must still scan it.
      DTARuleColRange(id = "junk_range", columns = "JUNK1", range = c(0, 10))
    )
  )

  df <- data.frame(
    ID = c("A001", "A002"),
    AGE = c("25", "90"),
    JUNK1 = c("5", "50"),
    JUNK2 = c("noise", "noise"),
    stringsAsFactors = FALSE
  )
  path <- ss_write_csv(df, "ss_projection.csv")
  on.exit(unlink(path), add = TRUE)

  projection <- dta_scan_projection(specs, specs@rules, names(df))
  expect_setequal(projection, c("ID", "AGE", "JUNK1"))

  coerced <- dta_coerce_table_to_specs(df, specs)
  expected <- validate_table_detailed(specs, coerced$table, verbose = FALSE)

  details <- validate_file_stream(specs, path, batch_rows = 1L, verbose = FALSE)
  expect_identical(details$rules_valid, expected$rules_valid)
  expect_identical(
    vapply(details$rule_errors, function(e) e$message, character(1)),
    vapply(expected$rule_errors, function(e) e$message, character(1))
  )
  expect_identical(details$n_columnspec_errors, expected$n_columnspec_errors)
})

test_that("a header-only file fails rules on absent columns exactly as the in-memory path", {
  rule <- DTARuleColUnique(id = "u_missing", columns = "AGE")
  specs <- vc_specs(
    list(DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = TRUE)),
    list(rule)
  )
  path <- file.path(tempdir(), "ss_header_only.csv")
  writeLines("ID", path)
  on.exit(unlink(path), add = TRUE)

  expected <- validate_table_detailed(
    specs, data.frame(ID = character(0), stringsAsFactors = FALSE),
    verbose = FALSE
  )
  expect_false(expected$rules_valid)

  details <- validate_file_stream(specs, path, verbose = FALSE)
  expect_false(details$rules_valid)
  expect_identical(
    details$rule_errors[[1]]$message,
    expected$rule_errors[[1]]$message
  )
})

test_that("a late unconvertible value in an undeclared column no longer kills the scan", {
  # Arrow used to lock an inferred type from the first ~1 MB block and abort
  # the whole scan on the first later value that did not convert. Every
  # scanned column is now read as text, so the scan completes whatever the
  # bytes hold.
  specs <- vc_specs(
    list(DTAColumnSpec(id = "ID", type = "SAS Char", length = 12, nullable = FALSE)),
    # The rule is what forces the undeclared column X into the scan under
    # projection -- and X is where the inference abort used to live.
    list(DTARuleColRange(id = "x_range", columns = "X", range = c(0, 1e14)))
  )
  path <- file.path(tempdir(), "ss_inference.csv")
  n_int_rows <- 80000L
  writeLines(
    c(
      "ID,X",
      paste0("r", seq_len(n_int_rows), ",1234567890123"),
      "rlast,0.01"
    ),
    path
  )
  on.exit(unlink(path), add = TRUE)

  details <- validate_file_stream(specs, path, verbose = FALSE)
  expect_true(details$ok)
  expect_identical(
    as.double(attr(details, "n_rows_scanned")),
    as.double(n_int_rows + 1L)
  )
})

test_that("error detail past max_errors spills and collect_full_errors() recovers all of it", {
  specs <- vc_specs(
    list(
      DTAColumnSpec(id = "ID", type = "SAS Char", length = 2, nullable = FALSE),
      DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
    )
  )
  df <- data.frame(
    ID = paste0("TOOLONG", 1:5),
    AGE = rep("abc", 5),
    stringsAsFactors = FALSE
  )
  path <- ss_write_csv(df, "ss_spill.csv")
  on.exit(unlink(path), add = TRUE)

  full <- validate_file_stream(specs, path, batch_rows = 1L, max_errors = NULL, verbose = FALSE)
  capped <- validate_file_stream(specs, path, batch_rows = 1L, max_errors = 2L, verbose = FALSE)

  # Counts and verdicts never depended on the cap.
  expect_identical(capped$n_columnspec_errors, full$n_columnspec_errors)
  expect_identical(capped$n_import_errors, full$n_import_errors)
  expect_identical(capped$ok, full$ok)

  # The in-memory head is capped; the reassembled detail is complete.
  expect_identical(nrow(capped$columnspec_errors$full_error), 2L)
  expect_true(isTRUE(attr(capped$columnspec_errors$full_error, "truncated")))

  collected <- collect_full_errors(capped, axis = "columnspec")
  reference <- full$columnspec_errors$full_error
  attr(reference, "truncated") <- NULL
  expect_identical(nrow(collected), nrow(reference))
  expect_identical(collected$row, reference$row)
  expect_identical(collected$message, reference$message)

  collected_import <- collect_full_errors(capped, axis = "import")
  full_import <- collect_full_errors(full, axis = "import")
  expect_identical(nrow(collected_import), nrow(full_import))
  expect_identical(collected_import$row, full_import$row)
  expect_identical(collected_import$raw, full_import$raw)

  # Even a cap of zero strands nothing: the in-memory head is an empty frame,
  # but it still carries the pointer to the rows on disk. (Returning NULL
  # here made "every error is on disk" indistinguishable from "no errors".)
  none <- validate_file_stream(
    specs, path,
    batch_rows = 1L, max_errors = 0L, verbose = FALSE
  )
  expect_identical(none$n_columnspec_errors, full$n_columnspec_errors)
  expect_identical(nrow(none$columnspec_errors$full_error), 0L)
  collected_none <- collect_full_errors(none, axis = "columnspec")
  expect_identical(nrow(collected_none), nrow(reference))
  expect_identical(collected_none$message, reference$message)
})

test_that("a capped typing axis keeps its count and its spilled detail", {
  # The typing axis is what check() records in a dataset's @import_issues, so
  # capping it must not lose the count OR strand the rows: the frame carries the
  # sink's spill pointer, and collect_full_errors() reads it back.
  specs <- vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE),
    DTAColumnSpec(id = "VAL", type = "SAS Num", nullable = TRUE)
  ))
  df <- data.frame(
    ID = c("a", "b", "c"),
    VAL = c("p", "q", "r"),
    stringsAsFactors = FALSE
  )
  path <- ss_write_csv(df, "ss_typing_cap.csv")
  on.exit(unlink(path), add = TRUE)

  capped <- validate_file_stream(specs, path, max_errors = 1L, verbose = FALSE)
  typing <- capped$import_typing_errors

  expect_identical(nrow(typing), 1L)
  expect_true(isTRUE(attr(typing, "truncated", exact = TRUE)))
  # The count is the number that failed to type, not the number retained.
  expect_equal(dta_import_error_count(typing), 3)

  collected <- collect_full_errors(capped, axis = "import_typing")
  expect_identical(nrow(collected), 3L)
  expect_setequal(collected$raw, c("p", "q", "r"))
  expect_true(all(collected$column == "VAL"))

  # Uncapped, the axis is complete in memory and collecting it changes nothing.
  full <- validate_file_stream(specs, path, max_errors = NULL, verbose = FALSE)
  expect_identical(nrow(full$import_typing_errors), 3L)
  expect_identical(nrow(collect_full_errors(full, axis = "import_typing")), 3L)
})


test_that("collect_full_errors() says so when it cannot collect an in-memory cap", {
  # The streaming sink spills what it cannot hold, so its truncation is always
  # recoverable. The materialising path's max_errors cap is not -- and returning
  # the head in silence is how a partial frame passes for a complete one, which
  # for a function called collect_full_errors() is the failure that matters.
  specs <- vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 2, nullable = FALSE)
  ))
  table <- data.frame(ID = paste0("TOOLONG", 1:20), stringsAsFactors = FALSE)

  capped <- validate_table_detailed(specs, table, verbose = FALSE, max_errors = 3L)
  expect_identical(capped$n_columnspec_errors, 20L)
  expect_identical(nrow(capped$columnspec_errors$full_error), 3L)

  # Both counts are named, so the caller can see how much of the detail is gone.
  expect_warning(
    collected <- collect_full_errors(capped, axis = "columnspec"),
    regexp = "3 of 20"
  )
  expect_identical(nrow(collected), 3L)

  expect_warning(
    collect_full_errors(
      validate_table_detailed(
        specs,
        dta_coerce_table_to_specs(
          data.frame(ID = rep("x", 4), VAL = rep("abc", 4), stringsAsFactors = FALSE),
          vc_specs(list(DTAColumnSpec(id = "VAL", type = "SAS Num", nullable = TRUE)))
        )$table,
        verbose = FALSE, max_errors = 1L
      ),
      axis = "import"
    ),
    regexp = "1 of 4"
  )

  # An uncapped result is complete, and complete results say nothing.
  full <- validate_table_detailed(specs, table, verbose = FALSE)
  expect_no_warning(collect_full_errors(full, axis = "columnspec"))

  # Nor does a streaming result: its dropped rows are on disk and get collected.
  path <- ss_write_csv(table, "ss_collect_warn.csv")
  on.exit(unlink(path), add = TRUE)
  streamed <- validate_file_stream(specs, path, max_errors = 3L, verbose = FALSE)
  expect_no_warning(collected <- collect_full_errors(streamed, axis = "columnspec"))
  expect_identical(nrow(collected), 20L)
})


test_that("import-error counts no longer inflate when several rules read one column", {
  # Two rules reading the same unconvertible column used to push two copies of
  # every bad cell into the sink; dedup ran only over the retained rows, so
  # the count doubled once the cap hid the duplicates.
  specs <- vc_specs(
    list(DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE)),
    list(
      DTARuleColRange(id = "r1", columns = "AGE", range = c(0, 100)),
      DTARuleColRange(id = "r2", columns = "AGE", range = c(10, 90))
    )
  )
  df <- data.frame(
    ID = c("A001", "A002", "A003", "A004"),
    AGE = c("abc", "def", "ghi", "jkl"),
    stringsAsFactors = FALSE
  )
  path <- ss_write_csv(df, "ss_inflation.csv")
  on.exit(unlink(path), add = TRUE)

  expected <- validate_table_detailed(specs, df, verbose = FALSE)

  details <- validate_file_stream(
    specs, path,
    batch_rows = 1L, max_errors = 1L, verbose = FALSE
  )
  expect_identical(
    as.double(details$n_import_errors),
    as.double(expected$n_import_errors)
  )
})

test_that("violation messages render counts and row numbers past the integer range", {
  # sprintf("%d", <double past 2^31>) errors, and as.integer() on such row
  # numbers produced NAs that sort() silently dropped -- a completed
  # multi-hour scan then crashed (or lost its evidence) while composing its
  # own message.
  big <- 3e9
  expect_no_error(msg <- dta_unique_violation_message("r", big, "K"))
  expect_match(msg, "3000000000", fixed = TRUE)

  expect_no_error(msg2 <- dta_condition_violation_message("r", big))
  expect_match(msg2, "3000000000", fixed = TRUE)

  expect_no_error(msg3 <- dta_range_violation_message("r", big, "K", c(0, 1)))
  expect_match(msg3, "3000000000", fixed = TRUE)

  rows <- c(2147483650, 2147483651)
  rendered <- dta_format_group_rows(rows, 2)
  expect_identical(rendered, "2147483650,2147483651")
  rendered_more <- dta_format_group_rows(rows, big, max_show = 1L)
  expect_match(rendered_more, "^2147483650 \\(\\+2999999999 more\\)$")
})

test_that("declared missing_values are honored on both the eager and lazy readers", {
  # The property was stored and never consulted: a handler declaring the SAS
  # convention "." got one spurious not_convertible import error per '.' cell.
  specs <- vc_specs(
    list(
      DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE),
      DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
    )
  )
  path <- file.path(tempdir(), "ss_missing_values.csv")
  writeLines(c("ID,AGE", "A001,.", "A002,30"), path)
  on.exit(unlink(path), add = TRUE)

  plain <- DTAFileCSV(filename = "ss_missing_values.csv")
  declaring <- DTAFileCSV(
    filename = "ss_missing_values.csv",
    missing_values = "."
  )

  # Without the declaration, "." is a value that fails to convert.
  coerced_plain <- dta_coerce_table_to_specs(
    as.data.frame(read_file(plain, path, specs = specs)), specs
  )
  expect_identical(dta_import_error_count(coerced_plain$issues), 1L)

  # With it, "." is missing at read time: NA in the column, no issue.
  eager <- as.data.frame(read_file(declaring, path, specs = specs))
  coerced <- dta_coerce_table_to_specs(eager, specs)
  expect_identical(dta_import_error_count(coerced$issues), 0L)
  expect_true(is.na(coerced$table$AGE[[1]]))

  lazy <- open_file(declaring, path, specs = specs)
  details <- dta_validate_any_table(specs, lazy, verbose = FALSE)
  expect_identical(details$n_import_errors, 0L)
  expect_true(details$ok)
})

test_that("stream = 'auto' accounts for gzip compression", {
  plain <- file.path(tempdir(), "ss_auto_plain.csv")
  writeLines(rep("x", 200), plain)
  gz <- file.path(tempdir(), "ss_auto_sized.csv.gz")
  file.copy(plain, gz, overwrite = TRUE)
  on.exit(unlink(c(plain, gz)), add = TRUE)

  size <- file.size(plain)
  withr::local_options(DTAtools.stream_threshold = size * 2)

  # Same bytes on disk: the plain file sits under the threshold, the .gz name
  # is judged by its estimated expansion and streams.
  expect_false(dta_resolve_stream_mode("auto", plain))
  expect_true(dta_resolve_stream_mode("auto", gz))
})

test_that("a .zip input is refused with a clear message", {
  path <- file.path(tempdir(), "ss_refused.zip")
  writeLines("not really an archive", path)
  on.exit(unlink(path), add = TRUE)

  handler <- DTAFileCSV(filename = "ss_refused.zip")
  expect_error(
    read_file(handler, path),
    regexp = "zip archives are not supported"
  )
  expect_error(
    open_file(handler, path),
    regexp = "zip archives are not supported"
  )
})

test_that("quoted numeric bounds compare numerically on both paths", {
  # equals/not_equals/in/not_in against a numeric column now compare
  # numerically when the supplied bound parses as a number, so the quoted
  # bound "1000000" matches the value 1000000 exactly as the unquoted number
  # would -- regardless of whether a declared-Int column happened to get
  # narrowed to R integer for this batch/table, or stayed double. The IF
  # clause therefore fires on the row holding 1000000 on BOTH the eager and
  # the streamed path, and its THEN (equals "999") fails there, so both now
  # report rules_valid = FALSE with an identical message.
  specs <- vc_specs(
    list(DTAColumnSpec(id = "NUM", type = "SAS Int", nullable = TRUE)),
    list(DTARuleColCondition(
      id = "eq_str",
      condition = list(NUM = list(equals = "1000000")),
      then = list(NUM = list(equals = "999"))
    ))
  )
  df <- data.frame(NUM = c("1000000", "2.5"), stringsAsFactors = FALSE)
  path <- ss_write_csv(df, "ss_narrowing.csv")
  on.exit(unlink(path), add = TRUE)

  coerced <- dta_coerce_table_to_specs(df, specs)
  eager <- validate_table_detailed(specs, coerced$table, verbose = FALSE)
  expect_false(eager$rules_valid)

  streamed <- validate_file_stream(specs, path, batch_rows = 1L, verbose = FALSE)
  expect_false(streamed$rules_valid)

  expect_identical(
    streamed$rule_errors[[1]]$message,
    eager$rule_errors[[1]]$message
  )
})

test_that("validate_file_stream cleans a padded header like every other entry point", {
  # AGE is declared as well as ID: the padding is what this test is about, and
  # an undeclared column would add a second, unrelated structural finding.
  specs <- vc_specs(
    list(
      DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE),
      DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
    )
  )
  path <- file.path(tempdir(), "ss_padded_header.csv")
  writeLines(c("ID ,AGE", "A001,30"), path)
  on.exit(unlink(path), add = TRUE)

  details <- validate_file_stream(specs, path, verbose = FALSE)
  # Previously the raw name "ID " matched nothing: the clean column was
  # reported missing (once per row) and the padded one unexpected.
  expect_true(details$columnspec_valid)
  expect_identical(details$n_columnspec_errors, 0L)
})
