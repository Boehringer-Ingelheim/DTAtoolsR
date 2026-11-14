# What is reported, on which axis, in which order, and what happens when the
# recorded state of a matrix table no longer matches. REQ-MATRIX-042 ..
# REQ-MATRIX-046.
#
# The expected findings are listed from the injection plan of the file written
# in each case: every defect is placed on purpose and the findings that follow
# from it are written down from the requirement sentences.
#
# Each case runs on the table held in memory (stream = "never") and scanned in
# batches (stream = "always").

test_that("OQ-MATRIX-040 | every kind of finding is on the axis the requirement gives it, and the axis counts agree | REQ-MATRIX-042", {
  ds <- mx_ds(
    header = list(pattern = "^S[0-9]$", count = list(max = 3)),
    rows = list(count = list(max = 1)),
    cells = list(type = "SAS Num", nullable = FALSE, range = list(min = 0, max = 10))
  )
  # Corner "x" where blank is declared; S1 twice; "bad" against the pattern;
  # four data columns against a maximum of three; two rows against a maximum
  # of one. Row 1: <column 3> is Inf, "bad" holds abc, S4 is -1. Row 2: "bad"
  # is missing, S4 is 11, and the key r1 repeats.
  path <- mx_path("x\tS1\tS1\tbad\tS4\nr1\t1\tInf\tabc\t-1\nr1\t2\t3\t\t11\n")

  expected_columnspec <- mx_expect(
    "columnspec",
    c(NA, NA, NA, NA, NA, 1, 1, 1, 2, 2),
    c("ID", "S1", "bad", NA, NA, "<column 3>", "bad", "S4", "bad", "S4"),
    c(
      "corner", "uniqueNames", "propertyNames", "maxProperties", "maxItems",
      "finite", "type", "minimum", "type", "maximum"
    )
  )
  mx_each(ds, path, function(d, stream) {
    f <- mx_found(d)
    by_source <- function(src) {
      g <- f[f$source == src, , drop = FALSE]
      rownames(g) <- NULL
      g
    }
    qa_step(
      sprintf("the column-specification findings (%s)", stream),
      expected_columnspec, by_source("columnspec")
    )
    qa_step(
      sprintf("the import finding: abc is not convertible (%s)", stream),
      mx_expect("import", 1, "bad", "not_convertible"), by_source("import")
    )
    qa_step(
      sprintf("the repeated key is one rules-axis finding (%s)", stream),
      1L, sum(f$source == "rule")
    )
    st <- validation_status(d)
    qa_step(
      sprintf("validation_status() counts the three axes (%s)", stream),
      c(10L, 1L, 1L),
      c(as.integer(st$n_columnspec_errors), as.integer(st$n_import_errors), as.integer(st$n_rule_errors))
    )
    qa_step(sprintf("and the table is invalid (%s)", stream), FALSE, isTRUE(st$ok))
  })
})

test_that("OQ-MATRIX-041 | structural findings are retained ahead of cell findings and the counts are exact at every cap | REQ-MATRIX-043", {
  ds <- mx_ds(
    header = list(pattern = "^S[0-9]$"), rows = list(count = list(min = 10)),
    cells = list(type = "SAS Int", nullable = FALSE, range = list(min = 0))
  )
  # Five findings: the label "bad" (header), 2 rows against a minimum of 10
  # (row count) and three negative cells: S1 in row 1, "bad" in rows 1 and 2.
  path <- mx_path("\tS1\tbad\nr1\t-1\t-2\nr2\t5\t-3\n")
  structural <- c("propertyNames", "minItems")

  # The complete ordered set: header finding, row-count finding, then the
  # cells in file order (row 1: S1, bad; row 2: bad).
  complete <- c("propertyNames", "minItems", "minimum", "minimum", "minimum")
  heads <- list()
  for (stream in MX_STREAMS) {
    for (cap in c(0, 1, 2, 3, Inf)) {
      d <- mx_checked(ds, path, stream, max_errors = cap)
      kept <- as.data.frame(messages(d))$keyword
      heads[[paste(stream, cap)]] <- kept
      qa_step(
        sprintf("the count of column-specification errors is 5 at max_errors = %s (%s)", cap, stream),
        5L, as.integer(validation_status(d)$n_columnspec_errors)
      )
      if (cap %in% c(1, 2)) {
        # The retained findings begin with the structural ones, in order.
        qa_step(
          sprintf("what is kept at max_errors = %s begins with the structural findings in order (%s)", cap, stream),
          structural[seq_len(cap)], kept[seq_len(cap)]
        )
      }
      if (is.infinite(cap)) {
        qa_step(sprintf("with no cap all five are kept (%s)", stream), sort(complete, method = "radix"), sort(kept, method = "radix"))
      }
    }
  }
  # The same retained findings, in the same order, on both holdings.
  for (cap in c(0, 1, 2, 3, Inf)) {
    qa_step(
      sprintf("the retained findings at max_errors = %s agree between the holdings", cap),
      heads[[paste("never", cap)]], heads[[paste("always", cap)]]
    )
  }
  # A scanned table reassembles the complete ordered set at every cap.
  for (cap in c(0, 1, 2, 3)) {
    d <- mx_checked(ds, path, "always", max_errors = cap)
    full <- collect_full_errors(validation_errors(d, table = "m"), axis = "columnspec")
    qa_step(
      sprintf("collect_full_errors() returns all five in order at max_errors = %s", cap),
      complete, as.character(full$keyword)
    )
  }
})

test_that("OQ-MATRIX-054 | explain_messages() describes a header finding by the offending text, not as a missing value | REQ-MATRIX-056", {
  ds <- mx_ds(header = list(pattern = "^S[0-9]$"))
  path <- mx_path("	S1	bad
r1	1	2
")
  mx_each(ds, path, function(d, stream) {
    out <- suppressMessages(explain_messages(d))
    qa_step(sprintf("one finding is explained (%s)", stream), 1L, nrow(out))
    qa_step(sprintf("its actual text names the offending label (%s)", stream), TRUE, grepl("bad", out$actual[[1]], fixed = TRUE))
    qa_step(
      sprintf("and does not call it missing or absent (%s)", stream),
      FALSE, grepl("missing|not present|absent", out$actual[[1]], ignore.case = TRUE)
    )
  })
})

# ---- stale state ------------------------------------------------------------------------

# A second dataset in the same delivery, to show it is still validated.
mf_other <- function() create_example_DTADataSetTabular(2)

test_that("OQ-MATRIX-042 | a table with no recorded header is that table's failure, not an abort, and other datasets are still validated | REQ-MATRIX-044", {
  ds <- mx_ds()
  for (stream in MX_STREAMS) {
    d <- load_file(ds, file = mx_path("\tS1\nr1\t1\n"), stream = stream)
    qa_step(
      sprintf("loaded, the table is valid and its header is recorded (%s)", stream),
      c(TRUE, "m"),
      c(validation_status(check(d, quiet = TRUE, persist = FALSE))$ok, names(d@heads))
    )
    d@heads <- list()
    res <- check(d, quiet = TRUE, persist = FALSE)
    qa_step(sprintf("without it the table is not valid (%s)", stream), FALSE, isTRUE(validation_status(res)$ok))
    qa_step(
      sprintf("the message says to load it again (%s)", stream),
      TRUE, any(grepl("load it again", as.data.frame(messages(res))$message, fixed = TRUE))
    )

    delivery <- check(DTA(datasets = list(d, mf_other())), quiet = TRUE, persist = FALSE)
    qa_step(
      sprintf("in a DTA the matrix fails and the other dataset is still validated (%s)", stream),
      c(FALSE, "validated"),
      c(isTRUE(validation_status(datasets(delivery, "m"))$ok), validation_status(datasets(delivery, "demographics"))$status)
    )
  }
})

test_that("OQ-MATRIX-043 | a table whose recorded header no longer gives its column names under the specification is that table's failure | REQ-MATRIX-045", {
  loaded <- load_file(mx_ds(), file = mx_path("\tS1\nr1\t1\n"), stream = "never")
  qa_step("the loaded table is valid", TRUE, validation_status(check(loaded, quiet = TRUE, persist = FALSE))$ok)

  # The same table and recorded header under a specification whose row label
  # is called ROW: as if the row label had been renamed after loading.
  renamed <- dta_dataset_from_list(list(
    id = "m", type = "matrix", files = list(filename = "m.tsv", type = "tsv"),
    row_labels = list(list(id = "ROW", type = "SAS Char", nullable = FALSE)),
    matrix = list(header = list(corner = "blank"), cells = list(type = "SAS Int", nullable = FALSE))
  ))
  renamed@tables <- loaded@tables
  renamed@heads <- loaded@heads

  res <- check(renamed, quiet = TRUE, persist = FALSE)
  qa_step("the table is not valid", FALSE, isTRUE(validation_status(res)$ok))
  qa_step(
    "the message says to load it again",
    TRUE, any(grepl("load it again", as.data.frame(messages(res))$message, fixed = TRUE))
  )
  delivery <- check(DTA(datasets = list(renamed, mf_other())), quiet = TRUE, persist = FALSE)
  qa_step(
    "in a DTA the other dataset is still validated",
    "validated", validation_status(datasets(delivery, "demographics"))$status
  )
})

test_that("OQ-MATRIX-044 | a lazily held file whose header changed after loading is that table's failure | REQ-MATRIX-046", {
  path <- mx_path("\tS1\tS2\nr1\t1\t2\n")
  d <- load_file(mx_ds(), file = path, stream = "always")
  qa_step("before the change the table is valid", TRUE, validation_status(check(d, quiet = TRUE, persist = FALSE))$ok)

  # Same size, a later modification time: the label S2 becomes S3.
  Sys.sleep(1.1)
  writeBin(charToRaw("\tS1\tS3\nr1\t1\t2\n"), path)
  res <- check(d, quiet = TRUE, persist = FALSE)
  msg <- as.data.frame(messages(res))$message
  qa_step("after it the table is not valid", FALSE, isTRUE(validation_status(res)$ok))
  qa_step(
    "the message says the file changed since it was loaded and to load it again",
    c(TRUE, TRUE), c(any(grepl("changed since", msg, fixed = TRUE)), any(grepl("load it again", msg, fixed = TRUE)))
  )
  again <- load_file(mx_ds(), file = path, stream = "always")
  qa_step(
    "loading it again gives a valid table",
    TRUE, validation_status(check(again, quiet = TRUE, persist = FALSE))$ok
  )
})
