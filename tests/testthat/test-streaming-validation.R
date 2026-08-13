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
    c("decomposable", "keyed", "decomposable", "buffered")
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

test_that("a buffered rule retains only the columns it reads", {
  rule <- vc_corpus()$rule_group_exclusive$specs@rules[[1]]
  needed <- dta_rule_buffer_columns(rule)

  expect_true(all(c("SUBJ", "REASND", "ORRES") %in% needed))
  # A wide table's other columns must not be retained.
  expect_false("UNRELATED" %in% needed)
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
