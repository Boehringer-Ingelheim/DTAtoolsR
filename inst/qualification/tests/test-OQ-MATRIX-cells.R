# What a matrix cell may hold. REQ-MATRIX-026 .. REQ-MATRIX-041.
#
# One case per row of the cell contract in the requirements. Each case writes a
# file with ONE data row and one data column per raw token, labelled C1, C2 ...,
# so a finding names the token it is about by its column, and states for every
# token what must be reported. A value is verified by bounding it: a cell
# spec that is not nullable and whose range is [v, v] reports nothing exactly
# when the cell is present and equal to v, because a cell that is missing or
# cannot be typed is a finding of its own.
#
# Every case runs on the table held in memory (stream = "never") and scanned in
# batches (stream = "always").

# ---- local helpers ------------------------------------------------------------

# One-row matrix: row label r1, one data column per raw text, labelled C1 ...
mc_run <- function(raws, cells, ext = "tsv", files_extra = list(), env = parent.frame()) {
  sep <- if (ext == "tsv") "\t" else ","
  labels <- paste0("C", seq_along(raws))
  text <- paste0(
    sep, paste(labels, collapse = sep), "\n",
    "r1", sep, paste(raws, collapse = sep), "\n"
  )
  list(
    ds = mx_ds(cells = cells, ext = ext, files_extra = files_extra),
    path = mx_path(text, paste0("m.", ext), env)
  )
}

# What was reported for column `label`, as sorted "source keyword" strings.
mc_col <- function(found, label) {
  i <- !is.na(found$column) & found$column == label
  sort(paste(found$source[i], found$keyword[i]), method = "radix")
}

# Check one-row matrices on both holdings and compare, per raw text, what was
# reported with `expected` (a list, one character vector per raw text, "" for
# nothing). Also states that nothing was reported outside the raw texts'
# columns.
mc_expect <- function(what, raws, cells, expected, ext = "tsv", files_extra = list()) {
  fx <- mc_run(raws, cells, ext = ext, files_extra = files_extra)
  mx_each(fx$ds, fx$path, function(d, stream) {
    f <- mx_found(d)
    for (i in seq_along(raws)) {
      qa_step(
        sprintf("%s: %s (%s)", what, raws[[i]], stream),
        sort(expected[[i]], method = "radix"), mc_col(f, paste0("C", i))
      )
    }
    qa_step(
      sprintf("%s: nothing is reported outside the cells (%s)", what, stream),
      sum(vapply(expected, length, integer(1))), nrow(f)
    )
  })
}

# The value of every raw text, by the bounding test described at the top.
mc_value <- function(raws, value, type, files_extra = list()) {
  mc_expect(
    sprintf("%s reads as %s in a %s cell", paste(raws, collapse = " "), value, type),
    raws,
    list(type = type, nullable = FALSE, range = list(min = value, max = value)),
    rep(list(character(0)), length(raws)),
    files_extra = files_extra
  )
}

# Char cells keep the text: a value list holding exactly the raw texts accepts
# them, and one that does not hold a text rejects it by that cell's column.
mc_char_kept <- function(raws, files_extra = list()) {
  mc_expect(
    "a SAS Char cell keeps the text", raws,
    list(type = "SAS Char", nullable = FALSE, values = as.list(raws)),
    rep(list(character(0)), length(raws)),
    files_extra = files_extra
  )
  mc_expect(
    "and a Char value list that omits the text rejects it", raws,
    list(type = "SAS Char", nullable = FALSE, values = list("zzz", "yyy")),
    rep(list("columnspec enum"), length(raws)),
    files_extra = files_extra
  )
}

# ---- the contract table, token by token ------------------------------------------

test_that("OQ-MATRIX-024 | 12, ' 12 ', +12, 012 and 1.2e1 are the value 12 in a Num or Int cell and text in a Char cell | REQ-MATRIX-026", {
  raws <- c("12", " 12 ", "+12", "012", "1.2e1")
  mc_value(raws, 12, "SAS Num")
  mc_value(raws, 12, "SAS Int")
  # LIM-025: +12 and 012 are accepted as numbers, as they are in a tabular
  # column, so a supplier's leading zero or plus sign is not reported.
  mc_char_kept(raws)
})

test_that("OQ-MATRIX-025 | 1.5 is the value 1.5 in a Num cell and not whole in an Int cell | REQ-MATRIX-027", {
  mc_value("1.5", 1.5, "SAS Num")
  mc_expect(
    "1.5 in an Int cell", "1.5",
    list(type = "SAS Int", nullable = TRUE), list("import not_whole")
  )
  mc_char_kept("1.5")
})

test_that("OQ-MATRIX-026 | 1.0, 1e3, 1e+05 and 123.0 are 1, 1000, 100000 and 123 in a Num or Int cell | REQ-MATRIX-028", {
  raws <- c("1.0", "1e3", "1e+05", "123.0")
  values <- c(1, 1000, 100000, 123)
  for (i in seq_along(raws)) {
    mc_value(raws[[i]], values[[i]], "SAS Num")
    mc_value(raws[[i]], values[[i]], "SAS Int")
  }
  mc_char_kept(raws)
})

test_that("OQ-MATRIX-027 | 0x1F is accepted as 31, as in a tabular column | REQ-MATRIX-029", {
  # LIM-025: hexadecimal text is a number package-wide, so a matrix cell
  # written 0x1F is accepted rather than reported.
  mc_value("0x1F", 31, "SAS Num")
  mc_value("0x1F", 31, "SAS Int")
  mc_char_kept("0x1F")
})

test_that("OQ-MATRIX-028 | 2147483648 is a value in a Num cell and out of range in an Int cell | REQ-MATRIX-030", {
  mc_value("2147483648", 2147483648, "SAS Num")
  mc_expect(
    "2147483648 in an Int cell", "2147483648",
    list(type = "SAS Int", nullable = TRUE), list("import out_of_range")
  )
  mc_char_kept("2147483648")
})

test_that("OQ-MATRIX-029 | Inf, -Inf, inf, Infinity and 1e400 are infinite values: a finite finding by default, judged by the range when infinite is allowed | REQ-MATRIX-031", {
  raws <- c("Inf", "-Inf", "inf", "Infinity", "1e400")

  # Default (infinite false): exactly one finite finding each, and a range does
  # not add a second finding for the same cell.
  mc_expect(
    "infinite is rejected by default", raws,
    list(type = "SAS Num", nullable = TRUE),
    rep(list("columnspec finite"), 5)
  )
  mc_expect(
    "and a range adds nothing for an infinite cell", raws,
    list(type = "SAS Num", nullable = TRUE, range = list(min = 0, max = 10)),
    rep(list("columnspec finite"), 5)
  )

  # Infinite allowed: no finding without a range; with one, +Inf is above a
  # finite maximum and -Inf below a finite minimum.
  mc_expect(
    "infinite allowed, no range", raws,
    list(type = "SAS Num", nullable = TRUE, infinite = TRUE),
    rep(list(character(0)), 5)
  )
  mc_expect(
    "infinite allowed, range [0, 10]", c("Inf", "-Inf", "inf", "Infinity", "1e400"),
    list(type = "SAS Num", nullable = TRUE, infinite = TRUE, range = list(min = 0, max = 10)),
    list("columnspec maximum", "columnspec minimum", "columnspec maximum", "columnspec maximum", "columnspec maximum")
  )

  # An Int cell cannot hold an infinite value; a Char cell keeps the text.
  mc_expect(
    "infinite in an Int cell", raws,
    list(type = "SAS Int", nullable = TRUE),
    rep(list("import not_whole"), 5)
  )
  mc_char_kept(raws)
})

test_that("OQ-MATRIX-030 | 1.7976931348623157e308 is finite in a Num cell and out of range in an Int cell | REQ-MATRIX-032", {
  raw <- "1.7976931348623157e308"
  mc_expect("the largest double in a Num cell", raw, list(type = "SAS Num", nullable = TRUE), list(character(0)))
  mc_expect("the largest double in an Int cell", raw, list(type = "SAS Int", nullable = TRUE), list("import out_of_range"))
  mc_char_kept(raw)
})

test_that("OQ-MATRIX-031 | NaN, nan and -nan are never a value: not convertible, or missing when declared | REQ-MATRIX-033", {
  raws <- c("NaN", "nan", "-nan")
  for (type in c("SAS Num", "SAS Int")) {
    mc_expect(
      paste("NaN family in a", type, "cell"), raws,
      list(type = type, nullable = TRUE), rep(list("import not_convertible"), 3)
    )
  }
  # A cell that fails typing is missing afterwards (plan 3.4), so where missing
  # is not allowed the same cell is also a type finding, and the text "NaN" is
  # never judged against the range.
  for (type in c("SAS Num", "SAS Int")) {
    mc_expect(
      paste("NaN family in a", type, "cell that is not nullable"), raws,
      list(type = type, nullable = FALSE, range = list(min = 0, max = 10)),
      rep(list(c("columnspec type", "import not_convertible")), 3)
    )
  }
  # LIM-002: NaN is classified as unconvertible, not as missing; naming it in
  # files.missing_values is how a supplier's NaN is made missing.
  mc_expect(
    "declared as a missing token, NaN is missing and allowed", "NaN",
    list(type = "SAS Num", nullable = TRUE), list(character(0)),
    files_extra = list(missing_values = "NaN")
  )
  mc_expect(
    "declared as a missing token, NaN is missing and a Num cell that is not nullable reports it", "NaN",
    list(type = "SAS Num", nullable = FALSE), list("columnspec type"),
    files_extra = list(missing_values = "NaN")
  )
  mc_char_kept(raws)
})

test_that("OQ-MATRIX-032 | N/A, NULL, null, n/a, #N/A and ' NA ' are not missing: they are not convertible in a Num or Int cell | REQ-MATRIX-034", {
  raws <- c("N/A", "NULL", "null", "n/a", "#N/A", " NA ")
  # LIM-024: only an empty field, NA and the declared tokens are missing. The
  # common spellings of "not available" are not, so a supplier who writes them
  # is told so instead of having them skipped.
  for (type in c("SAS Num", "SAS Int")) {
    mc_expect(
      paste("N/A family in a", type, "cell"), raws,
      list(type = type, nullable = TRUE), rep(list("import not_convertible"), 6)
    )
  }
  mc_char_kept(raws)
})

test_that("OQ-MATRIX-033 | 1,5, abc and TRUE are not convertible in a Num or Int cell and text in a Char cell | REQ-MATRIX-035", {
  raws <- c("1,5", "abc", "TRUE")
  for (type in c("SAS Num", "SAS Int")) {
    mc_expect(
      paste("text in a", type, "cell"), raws,
      list(type = type, nullable = TRUE), rep(list("import not_convertible"), 3)
    )
  }
  mc_char_kept(raws)
})

test_that("OQ-MATRIX-034 | an empty field, NA, a quoted empty string, a quoted NA and a declared token are missing, a field of blanks is missing except in a Char cell | REQ-MATRIX-036", {
  # Comma-separated so that quoted fields can be written. Not nullable, so
  # that being missing is itself a finding (keyword type).
  raws <- c("", "NA", "\"\"", "\"NA\"", "x")
  for (type in c("SAS Num", "SAS Int", "SAS Char")) {
    mc_expect(
      paste("missing forms in a", type, "cell"), raws,
      list(type = type, nullable = FALSE), rep(list("columnspec type"), 5),
      ext = "csv", files_extra = list(missing_values = "x")
    )
  }
  # LIM-026: an empty field and the text NA are one notion, missing. Both are
  # allowed together and refused together; neither can be required while the
  # other is permitted.
  for (type in c("SAS Num", "SAS Char")) {
    mc_expect(
      paste("empty and NA are allowed together in a nullable", type, "cell"), c("", "NA"),
      list(type = type, nullable = TRUE), list(character(0), character(0)),
      ext = "csv"
    )
  }
  # A declared token is case-sensitive: X is not x.
  mc_expect(
    "a declared token is case-sensitive", c("x", "X"),
    list(type = "SAS Char", nullable = FALSE), list("columnspec type", character(0)),
    ext = "csv", files_extra = list(missing_values = "x")
  )
  mc_expect(
    "so X is not convertible where x is missing", c("x", "X"),
    list(type = "SAS Num", nullable = TRUE), list(character(0), "import not_convertible"),
    ext = "csv", files_extra = list(missing_values = "x")
  )
  # Blanks only: missing for a number, present text for Char.
  for (type in c("SAS Num", "SAS Int")) {
    mc_expect(
      paste("blanks only in a not nullable", type, "cell"), "  ",
      list(type = type, nullable = FALSE), list("columnspec type")
    )
  }
  # LIM-023: a Char cell of blanks is present text, so it passes a Char cell
  # that is not nullable and is judged by the value list or pattern, not by
  # nullability; a pattern is how a delivery constrains it.
  mc_expect(
    "blanks only in a not nullable Char cell", "  ",
    list(type = "SAS Char", nullable = FALSE), list(character(0))
  )
  mc_expect(
    "and a pattern for non-blank text rejects it", "  ",
    list(type = "SAS Char", nullable = FALSE, pattern = "[^ ]"), list("columnspec pattern")
  )
})

test_that("OQ-MATRIX-035 | a missing cell is a type finding unless the cell spec is nullable | REQ-MATRIX-037", {
  ds_text <- "\tS1\tS2\tS3\nr1\t1\t\t3\nr2\t\t5\t6\n"
  for (nullable in list(NULL, FALSE)) {
    ds <- mx_ds(cells = list(type = "SAS Int", nullable = nullable))
    path <- mx_path(ds_text)
    mx_each(ds, path, function(d, stream) {
      qa_step(
        sprintf("a missing cell is reported at its row and column when nullable is %s (%s)", deparse(nullable), stream),
        mx_expect("columnspec", c(1, 2), c("S2", "S1"), c("type", "type")),
        mx_found(d)
      )
    })
  }
  ds <- mx_ds(cells = list(type = "SAS Int", nullable = TRUE))
  mx_each(ds, mx_path(ds_text), function(d, stream) {
    qa_step(sprintf("nothing is reported when nullable is true (%s)", stream), 0L, nrow(mx_found(d)))
  })
})

test_that("OQ-MATRIX-036 | an untypable cell is an import finding and, when not nullable, a type finding as well, never a range finding | REQ-MATRIX-038", {
  ds <- mx_ds(cells = list(type = "SAS Int", nullable = FALSE, range = list(min = 0, max = 10)))
  path <- mx_path("\tS1\tS2\nr1\tabc\t5\nr2\t3\t4\n")
  mx_each(ds, path, function(d, stream) {
    qa_step(
      sprintf("two findings for the one cell, none against the range (%s)", stream),
      mx_expect(c("columnspec", "import"), c(1, 1), c("S1", "S1"), c("type", "not_convertible")),
      mx_found(d)
    )
    st <- validation_status(d)
    qa_step(
      sprintf("counted once on each axis (%s)", stream),
      c(1L, 1L),
      c(as.integer(st$n_columnspec_errors), as.integer(st$n_import_errors))
    )
  })
})

test_that("OQ-MATRIX-037 | a cell below range.min or above range.max is reported at its row and column, a cell on a bound is not | REQ-MATRIX-039", {
  ds <- mx_ds(cells = list(type = "SAS Num", nullable = TRUE, range = list(min = 0, max = 10)))
  # r1: S1 on the minimum, S2 on the maximum. r2: S1 below, S2 above. r3: missing.
  path <- mx_path("\tS1\tS2\nr1\t0\t10\nr2\t-0.5\t10.5\nr3\t\t\n")
  mx_each(ds, path, function(d, stream) {
    qa_step(
      sprintf("only the cells outside the bounds are reported (%s)", stream),
      mx_expect("columnspec", c(2, 2), c("S1", "S2"), c("minimum", "maximum")),
      mx_found(d)
    )
  })
  # An open side leaves that side unchecked.
  ds <- mx_ds(cells = list(type = "SAS Num", nullable = TRUE, range = list(min = 0)))
  mx_each(ds, mx_path("\tS1\nr1\t-1\nr2\t1000000\n"), function(d, stream) {
    qa_step(
      sprintf("a range with only a minimum has no upper bound (%s)", stream),
      mx_expect("columnspec", 1, "S1", "minimum"),
      mx_found(d)
    )
  })
})

test_that("OQ-MATRIX-038 | the value list, pattern, length and range all have to hold | REQ-MATRIX-040", {
  ds <- mx_ds(cells = list(type = "SAS Int", nullable = FALSE, values = list(0, 1, 2), range = list(min = 0)))
  # -1 breaks the list and the minimum; 3 breaks the list only; 1 breaks neither.
  path <- mx_path("\tS1\tS2\tS3\nr1\t-1\t3\t1\n")
  mx_each(ds, path, function(d, stream) {
    qa_step(
      sprintf("a cell breaking both is reported once for each (%s)", stream),
      mx_expect("columnspec", c(1, 1, 1), c("S1", "S1", "S2"), c("enum", "minimum", "enum")),
      mx_found(d)
    )
  })
  ds <- mx_ds(cells = list(type = "SAS Char", nullable = FALSE, pattern = "^[ACGT]+$", length = 4))
  mx_each(ds, mx_path("\tS1\tS2\tS3\nr1\tACGT\tACGU\tACGTA\n"), function(d, stream) {
    qa_step(
      sprintf("a pattern and a length are each enforced on a Char cell (%s)", stream),
      mx_expect("columnspec", c(1, 1), c("S2", "S3"), c("pattern", "maxLength")),
      mx_found(d)
    )
  })
})

test_that("OQ-MATRIX-039 | an inapplicable range or infinite setting, a reversed range and a textual bound are refused when the dataset is built | REQ-MATRIX-041", {
  build <- function(cells) {
    dta_dataset_from_list(list(
      id = "m", type = "matrix", files = list(filename = "m.tsv", type = "tsv"),
      row_labels = list(list(id = "ID", type = "SAS Char", nullable = FALSE)),
      matrix = list(header = list(corner = "blank"), cells = cells)
    ))
  }
  qa_step(
    "a range on a Char cell",
    TRUE, mx_is(mx_error(build(list(type = "SAS Char", range = list(min = 1)))), "dta_cell_range_type")
  )
  qa_step(
    "infinite on an Int cell",
    TRUE, mx_is(mx_error(build(list(type = "SAS Int", infinite = TRUE))), "dta_cell_finite_type")
  )
  qa_step(
    "infinite on a Char cell",
    TRUE, mx_is(mx_error(build(list(type = "SAS Char", infinite = FALSE))), "dta_cell_finite_type")
  )
  qa_step(
    "a range whose min is above its max",
    TRUE, mx_is(mx_error(build(list(type = "SAS Int", range = list(min = 5, max = 1)))), "dta_cell_range_invalid")
  )
  # YAML 1.1 reads 1e3 as text, so a bound written that way arrives as "1e3".
  qa_step(
    "a bound that arrived as the text 1e3",
    TRUE, mx_is(mx_error(build(list(type = "SAS Num", range = list(max = "1e3")))), "dta_matrix_number_text")
  )
  qa_step(
    "while the same bound written as 1000 is accepted",
    NULL, mx_error(build(list(type = "SAS Num", range = list(max = 1000))))
  )
})
