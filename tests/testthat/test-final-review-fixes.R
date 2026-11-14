# Fixes from the final review of the matrix dataset work. Expected values come
# from the rules (an unconvertible token is missing; a missing field selects the
# rows missing it), not from observed output.

fr_tabular <- function(nullable) {
  DTADataSetTabular(
    id = "t",
    specs = DTAColumnSpecCollection(columns = list(
      ID = DTAColumnSpec(id = "ID", type = "SAS Char", nullable = FALSE),
      X = DTAColumnSpec(id = "X", type = "SAS Num", nullable = nullable, pattern = "^[0-9]+$")
    )),
    files = list(DTAFileCSV(filename = "t.csv"))
  )
}

fr_matrix <- function(nullable, header = list(corner = "blank"), cells = list()) {
  DTADataSetMatrix(
    id = "m",
    specs = DTAColumnSpecCollection(columns = list(
      ID = DTAColumnSpec(id = "ID", type = "SAS Char", nullable = FALSE)
    )),
    matrix = list(
      header = header,
      cells = c(list(type = "SAS Num", nullable = nullable), cells)
    ),
    files = list(DTAFileTSV(filename = "m.tsv"))
  )
}

fr_write <- function(lines, name, env = parent.frame()) {
  path <- file.path(withr::local_tempdir(.local_envir = env), name)
  writeLines(lines, path)
  path
}

fr_findings <- function(d) {
  m <- as.data.frame(messages(d))
  m[order(m$source, m$keyword), c("source", "row", "column", "keyword")]
}

test_that("an unconvertible NaN token is missing in the typed column, on every path", {
  tab <- fr_write(c("ID,X", "r1,NaN", "r2,2", "r3,nan", "r4,-nan"), "t.csv")
  mat <- fr_write(c("\tS1\tS2", "r1\tNaN\t1", "r2\t2\tnan"), "m.tsv")

  for (nullable in c(TRUE, FALSE)) {
    for (how in c("never", "always")) {
      label <- paste("nullable", nullable, how)

      d <- check(load_file(fr_tabular(nullable), file = tab, stream = how), quiet = TRUE, persist = FALSE)
      m <- as.data.frame(messages(d))
      expect_identical(sort(unique(m$keyword[m$source == "import"])), "not_convertible", label = label)
      expect_identical(sum(m$source == "import"), 3L, label = label)
      # Only a non-nullable cell is also a type finding; a pattern is never run on the text "NaN".
      expect_identical(
        sort(unique(m$keyword[m$source == "columnspec"])),
        if (nullable) character(0) else "type",
        label = label
      )
      if (how == "never") {
        x <- as.data.frame(tables(d)[[1]])$X
        expect_false(any(is.nan(x)), label = label)
        expect_identical(is.na(x), c(TRUE, FALSE, TRUE, TRUE), label = label)
      }

      d <- check(load_file(fr_matrix(nullable), file = mat, stream = how), quiet = TRUE, persist = FALSE)
      m <- as.data.frame(messages(d))
      expect_identical(sort(unique(m$keyword[m$source == "import"])), "not_convertible", label = label)
      expect_identical(sum(m$source == "import"), 2L, label = label)
      expect_identical(
        sort(unique(m$keyword[m$source == "columnspec"])),
        if (nullable) character(0) else "type",
        label = label
      )
      if (how == "never") {
        tb <- as.data.frame(tables(d)[[1]])
        expect_false(any(is.nan(unlist(tb[c("S1", "S2")]))), label = label)
      }
    }
  }
})

test_that("a label finding and a duplicate-free inspect(): a missing field selects missing rows", {
  mat <- fr_write(c("\tS1\tbad", "r1\t-1\t2", "r2\t3\t4"), "m.tsv")
  ds <- fr_matrix(FALSE,
    header = list(corner = "blank", pattern = "^S[0-9]$"),
    cells = list(type = "SAS Int", range = list(min = 0))
  )
  x <- check(DTA(datasets = list(ds)) |> load_delivery(mat, stream = "never"), quiet = TRUE, persist = FALSE)

  ex <- explain_messages(x, n = Inf)
  expect_identical(ex$actual[ex$column %in% "bad"], "bad")
  expect_identical(ex$actual[ex$column %in% "S1"], "-1")

  i <- as.data.frame(inspect(x))
  expect_identical(nrow(i), 2L)
  expect_false(anyNA(i$columnspec_keyword))
  expect_identical(sort(i$columnspec_keyword), c("minimum", "propertyNames"))

  f <- file.path(withr::local_tempdir(), "r.html")
  write_validation_report(x, f, overwrite = TRUE, quiet = TRUE)
  html <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_false(grepl("(missing / not present)", html, fixed = TRUE))
})

test_that("a missing required column still renders as missing, once per row", {
  tab <- fr_write(c("ID,A", "r1,-1", "r2,-2"), "t.csv")
  ds <- DTADataSetTabular(
    id = "t",
    specs = DTAColumnSpecCollection(columns = list(
      ID = DTAColumnSpec(id = "ID", type = "SAS Char", nullable = FALSE),
      A = DTAColumnSpec(id = "A", type = "SAS Int", nullable = FALSE, values = c("1", "2")),
      B = DTAColumnSpec(id = "B", type = "SAS Int", nullable = FALSE)
    )),
    files = list(DTAFileCSV(filename = "t.csv"))
  )
  d <- check(load_file(ds, file = tab, stream = "never"), quiet = TRUE, persist = FALSE)
  ex <- explain_messages(d, n = Inf)
  expect_identical(ex$actual[is.na(ex$column)], rep("(missing / not present)", 2L))
  expect_identical(ex$actual[ex$column %in% "A"], c("-1", "-2"))
  i <- as.data.frame(inspect(d))
  expect_identical(nrow(i), 4L)
  expect_false(anyNA(i$columnspec_keyword))
})

test_that("a fail_fast partial scan says the import axis was not settled, not that it predates", {
  tab <- fr_write(c("ID,X", "r1,2", "r2,3"), "t.csv")
  mat <- fr_write(c("\tS1", "r1\t1", "r2\t2"), "m.tsv")
  details <- list(import_valid = NA, result_version = 2L)
  attr(details, "partial_scan") <- TRUE
  msg <- dta_import_messages_to_df("d", "t", details)$message
  expect_match(msg, "scan stopped early", fixed = TRUE)
  expect_no_match(msg, "predates", fixed = TRUE)
  # A genuinely old artifact keeps its text.
  old <- dta_import_messages_to_df("d", "t", list(import_valid = NULL))$message
  expect_match(old, "predates import checking", fixed = TRUE)
})

test_that("a fail_fast scan of a real dataset reports the unsettled import axis in its own words", {
  tab <- fr_write(c("ID,X", "r1,NaN", "r2,3", "r3,4"), "t.csv")
  d <- load_file(fr_tabular(FALSE), file = tab, stream = "always")
  d <- check(d, quiet = TRUE, persist = FALSE, fail_fast = TRUE)
  m <- as.data.frame(messages(d))
  expect_false(any(grepl("predates import checking", m$message, fixed = TRUE)))
})

test_that("a matrix prints under its own class name; a tabular dataset is unchanged", {
  m_out <- cli::cli_fmt(print(fr_matrix(TRUE)))
  expect_match(m_out[[1]], "<DTADataSetMatrix>", fixed = TRUE)
  expect_no_match(m_out[[1]], "DTADataSetTabular", fixed = TRUE)
  t_out <- cli::cli_fmt(print(fr_tabular(TRUE)))
  expect_match(t_out[[1]], "<DTADataSetTabular>", fixed = TRUE)
})

test_that("matrix counts and bounds are written as integers and round trip identically", {
  m <- dta_matrix_spec_from_list(list(
    header = list(corner = "blank", count = list(min = 1, max = 4)),
    rows = list(count = 7),
    cells = list(type = "SAS Int", range = list(min = 0, max = 10))
  ))
  l <- as.list(m)
  expect_identical(l$header$count, list(min = 1L, max = 4L))
  expect_identical(l$rows$count, 7L)
  expect_identical(l$cells$range, list(min = 0L, max = 10L))
  expect_no_match(yaml::as.yaml(l), "\\d\\.0\\b")
  expect_identical(dta_matrix_spec_from_list(l), m)

  frac <- dta_matrix_spec_from_list(list(
    header = list(corner = "blank"),
    cells = list(type = "SAS Num", range = list(min = 0.5))
  ))
  expect_identical(as.list(frac)$cells$range, list(min = 0.5))
  expect_identical(dta_matrix_spec_from_list(as.list(frac)), frac)
})

fr_lint_matrix <- function(...) {
  base <- list(
    id = "m", type = "matrix",
    files = list(filename = "m.tsv", type = "tsv"),
    row_labels = list(list(id = "ID", type = "SAS Char", nullable = FALSE)),
    matrix = list(header = list(corner = "blank"), cells = list(type = "SAS Int", nullable = FALSE))
  )
  over <- list(...)
  for (nm in names(over)) base[nm] <- list(over[[nm]])
  base
}
fr_codes <- function(d) lint_dta(d)$code

test_that("header values that can never reach the minimum count are a lint error", {
  d <- fr_lint_matrix(matrix = list(
    header = list(corner = "blank", values = c("A", "B"), count = list(min = 3)),
    cells = list(type = "SAS Int", nullable = FALSE)
  ))
  expect_true("matrix_header_count_unsatisfiable" %in% fr_codes(d))
  ok <- fr_lint_matrix(matrix = list(
    header = list(corner = "blank", values = c("A", "B"), count = list(min = 2)),
    cells = list(type = "SAS Int", nullable = FALSE)
  ))
  expect_false("matrix_header_count_unsatisfiable" %in% fr_codes(ok))
})
