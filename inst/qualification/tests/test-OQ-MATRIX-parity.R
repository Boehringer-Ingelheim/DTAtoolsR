# One matrix on both engines, and a matrix delivered. REQ-MATRIX-054,
# REQ-MATRIX-055.
#
# The two holdings of a table are one contract: whole in memory and scanned in
# batches must give one verdict. The file below is generated, with every defect
# placed by this file at a position it chooses, so that what must be reported
# follows from the injection plan and not from a run.

# ---- the injection plan -----------------------------------------------------------

# 60 rows R01 .. R60 and 12 data columns S01 .. S12; the clean value at row i,
# column j is (i + j) mod 40, always within [0, 39]. The cell spec is an Int
# that is not nullable with range [0, 50] and the header pattern ^S[0-9]{2}$.
mp_file <- function() {
  rows <- 60L
  cols <- 12L
  value <- outer(seq_len(rows), seq_len(cols), function(i, j) (i + j) %% 40L)
  cells <- matrix(as.character(value), rows, cols)
  cells[5, 3] <- "-4" # below the minimum
  cells[17, 7] <- "abc" # cannot be typed
  cells[33, 2] <- "" # missing
  cells[50, 12] <- "99" # above the maximum
  keys <- sprintf("R%02d", seq_len(rows))
  keys[41] <- keys[40] # a repeated key
  body <- apply(cbind(keys, cells), 1, paste, collapse = "\t")
  paste0(
    "\t", paste(sprintf("S%02d", seq_len(cols)), collapse = "\t"), "\n",
    paste(body, collapse = "\n"), "\n"
  )
}

mp_ds <- function() {
  mx_ds(
    header = list(pattern = "^S[0-9]{2}$"),
    cells = list(type = "SAS Int", nullable = FALSE, range = list(min = 0, max = 50))
  )
}

test_that("OQ-MATRIX-052 | a matrix held in memory and the same file scanned in batches report the same findings and counts, at any batch size | REQ-MATRIX-054", {
  path <- mx_path(mp_file())

  # Column-specification: the -4 (minimum) at row 5 column S03, the abc (type,
  # because the cell is then missing and not nullable) at row 17 column S07,
  # the missing cell (type) at row 33 column S02, the 99 (maximum) at row 50
  # column S12. Import: the abc, once. Rules: the key R40 repeated, once.
  expected <- mx_expect(
    c("columnspec", "columnspec", "columnspec", "columnspec", "import", "rule"),
    c(5, 17, 33, 50, 17, NA),
    c("S03", "S07", "S02", "S12", "S07", NA),
    c("minimum", "type", "type", "maximum", "not_convertible", NA)
  )

  runs <- list(
    "in memory" = mx_checked(mp_ds(), path, "never"),
    "scanned, batches of 7 rows" = mx_checked(mp_ds(), path, "always", batch_rows = 7L),
    "scanned, batches of 1000 rows" = mx_checked(mp_ds(), path, "always", batch_rows = 1000L)
  )
  for (nm in names(runs)) {
    d <- runs[[nm]]
    f <- mx_found(d)
    # The rule row's own row and column are not part of the contract here.
    f$row[f$source == "rule"] <- NA_integer_
    f$column[f$source == "rule"] <- NA_character_
    f$keyword[f$source == "rule"] <- NA_character_
    f <- f[order(f$source, f$row, f$column, f$keyword, method = "radix"), , drop = FALSE]
    rownames(f) <- NULL
    qa_step(sprintf("%s: the findings", nm), expected, f)
    st <- validation_status(d)
    qa_step(
      sprintf("%s: the counts on the three axes", nm),
      c(4L, 1L, 1L),
      c(as.integer(st$n_columnspec_errors), as.integer(st$n_import_errors), as.integer(st$n_rule_errors))
    )
    qa_step(sprintf("%s: invalid", nm), FALSE, isTRUE(st$ok))
  }
})

test_that("OQ-MATRIX-053 | load_delivery() binds a matrix delivered loose or in a zip, records its header and judges it like load_file() | REQ-MATRIX-055", {
  text <- "\tS1\tS2\nr1\t1\t2\nr2\t3\t4\n"
  dta <- function() DTA(datasets = list(mx_ds()))

  loose <- load_delivery(dta(), cont_write("m.tsv", text))
  d <- datasets(loose, "m")
  qa_step(
    "a loose file: one table named after the file, header recorded",
    c("m", "m"), c(names(tables(d)), names(d@heads))
  )
  res <- check(loose, quiet = TRUE, persist = FALSE)
  qa_step("and it is valid", TRUE, validation_status(datasets(res, "m"))$ok)

  zipped <- load_delivery(dta(), cont_zip(list("m.tsv" = text), "delivery.zip"))
  d <- datasets(zipped, "m")
  qa_step(
    "from a zip: one table named after the member, header recorded",
    c("m", "m"), c(names(tables(d)), names(d@heads))
  )
  res <- check(zipped, quiet = TRUE, persist = FALSE)
  qa_step("and it is valid", TRUE, validation_status(datasets(res, "m"))$ok)

  # A header that does not match is judged the same way as through load_file().
  bad <- load_delivery(dta(), cont_zip(list("m.tsv" = "wrong\tS1\tS2\nr1\t1\t2\n"), "bad.zip"))
  res <- check(bad, quiet = TRUE, persist = FALSE)
  qa_step(
    "a wrong corner inside the zip is the corner finding",
    mx_expect("columnspec", NA, "ID", "corner"),
    mx_found(datasets(res, "m"))
  )
})
