# Matrix structure findings (header + row count) handed to the engine through
# `structure =`. Both paths must agree: order [header, row count, cells], exact
# counts, structural rows retained first. Expected values come from the rules
# and the fixtures below, not from observed output.

ms_specs <- function() {
  vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 2, nullable = FALSE),
    DTAColumnSpec(id = "VAL", type = "SAS Char", length = 10, nullable = TRUE)
  ))
}

# Six rows, five of them with an ID longer than the declared length 2.
ms_table <- function() {
  data.frame(
    ID = c("ok", "aaa", "bbb", "ccc", "ddd", "eee"),
    VAL = letters[1:6],
    stringsAsFactors = FALSE
  )
}

ms_header_findings <- function() {
  data.frame(
    row = c(NA_integer_, NA_integer_),
    column = c("corner_id", "A"),
    keyword = c("corner", "uniqueNames"),
    message = c("corner differs", "label repeated"),
    columnspec = c("ROW", "A"),
    data = c("X", "A"),
    stringsAsFactors = FALSE
  )
}

ms_structure <- function(rows = c(1, Inf), findings = ms_header_findings()) {
  list(
    findings = findings,
    rows = rows,
    declared = c(
      corner = 1L, propertyNames = 1L, uniqueNames = 1L,
      minProperties = 1L, maxProperties = 1L, minItems = 1L, maxItems = 1L
    ),
    matrix = TRUE
  )
}

# Validates `table` held in memory or as a lazy TSV dataset.
ms_run <- function(structure, how, table = ms_table(), env = parent.frame(),
                   verbose = FALSE, ...) {
  if (how == "memory") {
    return(dta_validate_any_table(ms_specs(), table, verbose = verbose, structure = structure, ...))
  }
  dir <- withr::local_tempdir(.local_envir = env)
  path <- file.path(dir, "t.tsv")
  utils::write.table(table, path, sep = "\t", quote = FALSE, row.names = FALSE)
  lazy <- open_file(DTAFileTSV(filename = "t.tsv"), path, specs = ms_specs())
  dta_validate_any_table(ms_specs(), lazy, verbose = verbose, structure = structure, ...)
}

check_status <- function(details, keyword) {
  details$columnspec_checks$status[details$columnspec_checks$keyword == keyword]
}

for (how in c("memory", "stream")) {
  local({
    how <- how

    test_that(paste0("[", how, "] structure findings precede the cells, counts are exact"), {
      # 6 rows against a minimum of 10: one minItems finding.
      details <- ms_run(ms_structure(rows = c(10, Inf)), how)
      fe <- details$columnspec_errors$full_error

      expect_identical(
        fe$keyword,
        c("corner", "uniqueNames", "minItems", rep("maxLength", 5))
      )
      expect_identical(fe$message[[3]], "must have at least 10 rows (has 6)")
      expect_identical(fe$columnspec[[3]], "10")
      expect_identical(fe$data[[3]], "6")
      expect_true(is.na(fe$row[[3]]))
      expect_identical(details$n_columnspec_errors, 8L)
      expect_false(details$columnspec_valid)
      expect_false(details$ok)

      expect_true(isTRUE(attr(details$columnspec_checks, "matrix")))
      expect_identical(check_status(details, "corner"), "failed")
      expect_identical(check_status(details, "uniqueNames"), "failed")
      expect_identical(check_status(details, "minItems"), "failed")
      expect_identical(check_status(details, "maxItems"), "passed")
      expect_identical(check_status(details, "required"), "not_applicable")
      expect_identical(check_status(details, "additionalProperties"), "not_applicable")
    })

    test_that(paste0("[", how, "] a row count within bounds adds no dimension finding"), {
      details <- ms_run(ms_structure(rows = c(1, 6)), how)
      fe <- details$columnspec_errors$full_error

      expect_identical(fe$keyword, c("corner", "uniqueNames", rep("maxLength", 5)))
      expect_identical(details$n_columnspec_errors, 7L)
      expect_identical(check_status(details, "minItems"), "passed")
      expect_identical(check_status(details, "maxItems"), "passed")
    })

    test_that(paste0("[", how, "] too many rows is a maxItems finding"), {
      details <- ms_run(
        ms_structure(rows = c(1, 4), findings = ms_header_findings()[0, ]), how
      )
      fe <- details$columnspec_errors$full_error

      expect_identical(fe$keyword, c("maxItems", rep("maxLength", 5)))
      expect_identical(fe$message[[1]], "must have at most 4 rows (has 6)")
      expect_identical(check_status(details, "maxItems"), "failed")
      expect_identical(check_status(details, "corner"), "passed")
    })

    test_that(paste0("[", how, "] no findings and an in-bounds count leave a clean table valid"), {
      details <- ms_run(
        ms_structure(rows = c(1, 5), findings = ms_header_findings()[0, ]),
        how,
        table = ms_table()[1, ]
      )
      expect_true(details$columnspec_valid)
      expect_identical(details$n_columnspec_errors, 0L)
      expect_identical(check_status(details, "corner"), "passed")
      expect_identical(check_status(details, "minItems"), "passed")
    })
  })
}

test_that("max_errors retains the same head on both paths; the stream spills the rest in order", {
  expected <- c("corner", "uniqueNames", "minItems", rep("maxLength", 5))
  for (cap in c(0, 1, 2, 3, 4, 8, 20)) {
    mem <- ms_run(ms_structure(rows = c(10, Inf)), "memory", max_errors = cap)
    str <- ms_run(ms_structure(rows = c(10, Inf)), "stream", max_errors = cap)
    label <- paste("max_errors =", cap)

    expect_identical(str$n_columnspec_errors, 8L, label = label)
    expect_identical(mem$n_columnspec_errors, 8L, label = label)
    # The retained head is the first `cap` rows of the full order, on both paths.
    expect_identical(
      str$columnspec_errors$full_error$keyword, expected[seq_len(min(cap, 8L))],
      label = label
    )
    expect_identical(
      mem$columnspec_errors$full_error$keyword, expected[seq_len(min(cap, 8L))],
      label = label
    )
    # The stream spills the remainder: every row once, in the full order.
    all_rows <- collect_full_errors(str)
    expect_identical(all_rows$keyword, expected, label = label)
    expect_identical(sort(all_rows$row[!is.na(all_rows$row)]), 2:6, label = label)
  }

  complete <- ms_run(ms_structure(rows = c(10, Inf)), "stream")
  expect_identical(complete$columnspec_errors$full_error$keyword, expected)
  expect_identical(collect_full_errors(complete)$keyword, expected)
})

test_that("[stream] fail_fast with a header finding stops after one batch without a false minItems", {
  # One row per batch, minimum of 4 rows: stopping after the first batch has
  # seen 1 row, which proves nothing about the minimum.
  details <- ms_run(
    ms_structure(rows = c(4, Inf)), "stream",
    fail_fast = TRUE, batch_rows = 1L,
    table = ms_table()[c(2, 1, 3:6), ] # a bad cell in the first batch
  )
  fe <- details$columnspec_errors$full_error

  expect_false("minItems" %in% fe$keyword)
  expect_true("corner" %in% fe$keyword)
  expect_identical(check_status(details, "minItems"), "not_checked")
  expect_identical(check_status(details, "corner"), "failed")
  expect_false(details$ok)
  # Stopped early: the first batch's cell finding only, not the five bad rows.
  n_cells <- sum(fe$keyword == "maxLength")
  expect_gte(n_cells, 1L)
  expect_lt(n_cells, 5L)
})

test_that("[stream] fail_fast reports a certain maxItems failure", {
  details <- ms_run(ms_structure(rows = c(1, 2)), "stream", fail_fast = TRUE)
  # The single batch holds all 6 rows, already above the maximum of 2.
  expect_true("maxItems" %in% details$columnspec_errors$full_error$keyword)
  expect_identical(check_status(details, "maxItems"), "failed")
})

test_that("structure = NULL keeps the legacy shape", {
  for (how in c("memory", "stream")) {
    details <- ms_run(NULL, how)
    cc <- details$columnspec_checks

    expect_identical(nrow(cc), 8L)
    expect_null(attr(cc, "matrix"))
    expect_identical(details$n_columnspec_errors, 5L)
    expect_identical(unique(details$columnspec_errors$full_error$keyword), "maxLength")
  }
})

# ---- order under every cap, header overflow ------------------------------------

ms_header3 <- function() {
  data.frame(
    row = rep(NA_integer_, 3),
    column = c("corner_id", "A", "B"),
    keyword = c("corner", "uniqueNames", "propertyNames"),
    message = c("m1", "m2", "m3"),
    columnspec = c("ROW", "A", "B"),
    data = c("X", "A", "B"),
    stringsAsFactors = FALSE
  )
}

test_that("[stream] collect_full_errors keeps [header, row count, cells] at every cap", {
  expected <- c("corner", "uniqueNames", "propertyNames", "minItems", rep("maxLength", 5))
  for (cap in c(0, 1, 2, 3, 10, Inf)) {
    details <- ms_run(
      ms_structure(rows = c(10, Inf), findings = ms_header3()), "stream",
      max_errors = cap
    )
    all_rows <- suppressWarnings(collect_full_errors(details))
    info <- paste("cap", cap)

    expect_identical(details$n_columnspec_errors, 9L, info = info)
    expect_identical(all_rows$keyword, expected, info = info)
    expect_identical(nrow(all_rows), 9L, info = info)
    # Row numbers of the cells, each exactly once and in scan order.
    expect_identical(all_rows$row[!is.na(all_rows$row)], 2:6, info = info)
    expect_identical(all_rows$message[1:3], c("m1", "m2", "m3"), info = info)

    if (cap < 3) {
      # The retained head is the in-memory path's: the first `cap` header rows.
      memory <- ms_run(
        ms_structure(rows = c(10, Inf), findings = ms_header3()), "memory",
        max_errors = cap
      )
      expect_identical(
        details$columnspec_errors$full_error$keyword,
        memory$columnspec_errors$full_error$keyword,
        info = info
      )
      expect_identical(
        attr(details$columnspec_errors$full_error, "spilled_rows"), 9 - cap,
        info = info
      )
    }
  }
})

test_that("a zero-row findings frame leaves no zero-row full_error on either path", {
  for (how in c("memory", "stream")) {
    details <- ms_run(
      ms_structure(rows = c(1, Inf), findings = ms_header_findings()[0, ]), how,
      table = ms_table()[1, ]
    )
    expect_null(details$columnspec_errors$full_error, info = how)
  }
})

test_that("on_missing_column = 'stop' has no effect when a structure is supplied", {
  table <- ms_table()[, "VAL", drop = FALSE] # the required ID is missing
  for (how in c("memory", "stream")) {
    details <- ms_run(
      ms_structure(rows = c(1, Inf)), how,
      table = table, on_missing_column = "stop"
    )
    # Not the early structural return: the header findings are reported.
    expect_true("corner" %in% details$columnspec_errors$full_error$keyword, info = how)
    expect_identical(check_status(details, "corner"), "failed", info = how)
    expect_identical(check_status(details, "minItems"), "passed", info = how)
  }
})

test_that("a malformed structure is refused up front", {
  bad <- list(
    no_frame = list(findings = NULL, rows = c(1, Inf)),
    extra_column = list(findings = cbind(ms_header_findings(), extra = 1), rows = c(1, Inf)),
    short_rows = list(findings = ms_header_findings(), rows = 1),
    inverted = list(findings = ms_header_findings(), rows = c(5, 2)),
    text_rows = list(findings = ms_header_findings(), rows = c("1", "Inf"))
  )
  for (nm in names(bad)) {
    expect_error(
      dta_validate_any_table(ms_specs(), ms_table(), structure = bad[[nm]]),
      class = "dta_structure_invalid", info = nm
    )
  }
})

# ---- row-count edges -------------------------------------------------------------

test_that("[stream] a partial scan that reached the minimum settles minItems only", {
  details <- ms_run(
    ms_structure(rows = c(1, Inf)), "stream",
    fail_fast = TRUE, batch_rows = 1L
  )
  expect_false("minItems" %in% details$columnspec_errors$full_error$keyword)
  expect_identical(check_status(details, "minItems"), "passed")
  expect_identical(check_status(details, "maxItems"), "not_checked")
})

test_that("[stream] an empty stream below the minimum reports exactly one minItems", {
  details <- ms_run(
    ms_structure(rows = c(1, Inf), findings = ms_header_findings()[0, ]), "stream",
    table = ms_table()[0, ]
  )
  fe <- details$columnspec_errors$full_error
  expect_identical(sum(fe$keyword == "minItems"), 1L)
  expect_identical(check_status(details, "minItems"), "failed")
})

test_that("a minimum of zero rows is satisfied by an empty table on both paths", {
  for (how in c("memory", "stream")) {
    details <- ms_run(
      ms_structure(rows = c(0, Inf), findings = ms_header_findings()[0, ]), how,
      table = ms_table()[0, ]
    )
    expect_false(
      any(c("minItems", "maxItems") %in% details$columnspec_errors$full_error$keyword),
      info = how
    )
    expect_identical(check_status(details, "minItems"), "passed", info = how)
    expect_identical(check_status(details, "maxItems"), "passed", info = how)
  }
})

test_that("[memory] max_errors = 0 retains nothing but keeps exact counts", {
  details <- ms_run(ms_structure(rows = c(10, Inf)), "memory", max_errors = 0)
  expect_identical(details$n_columnspec_errors, 8L)
  expect_identical(NROW(details$columnspec_errors$full_error), 0L)
  expect_false(details$columnspec_valid)
  expect_identical(check_status(details, "minItems"), "failed")
})

test_that("verbose runs print the header and dimensions findings on both paths", {
  for (how in c("memory", "stream")) {
    msgs <- paste(
      testthat::capture_messages(
        ms_run(ms_structure(rows = c(10, Inf)), how, verbose = TRUE)
      ),
      collapse = "\n"
    )
    expect_match(msgs, "Header check failed", fixed = TRUE, info = how)
    expect_match(msgs, "Dimensions check failed", fixed = TRUE, info = how)
  }
})
