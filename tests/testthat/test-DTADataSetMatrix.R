# DTADataSetMatrix: loading, the header contract, dimensions, cells, the row key
# and the failure shapes. Expected values come from the plan's rules (the corner
# table, the findings table), never from output; every semantic case runs on the
# in-memory path (stream = "never") and the lazy one (stream = "always").

mxd_fixture <- function(name) test_path("fixtures", "matrix", name)

# Write bytes to a fresh temp file called `name`.
mxd_file <- function(text, name = "m.tsv", env = parent.frame()) {
  p <- file.path(withr::local_tempdir(.local_envir = env), name)
  writeBin(if (is.raw(text)) text else charToRaw(text), p)
  p
}

# A one-row-label (or k-row-label) matrix dataset, written as the list YAML gives.
mxd_ds <- function(corner = "blank", k = 1L, header = list(), cells = NULL, rows = NULL,
                   ext = "tsv", rules = NULL, files_extra = list(), all_required = FALSE) {
  ids <- c("ID", "DESC", "EXTRA")
  labels <- lapply(seq_len(k), function(i) {
    list(id = ids[[i]], type = "SAS Char", nullable = if (all_required) FALSE else i > 1L)
  })
  m <- list(
    header = c(list(corner = corner), header),
    cells = cells %||% list(type = "SAS Int", nullable = FALSE)
  )
  if (!is.null(rows)) m$rows <- rows
  spec <- list(
    id = "m", type = "matrix",
    files = c(list(filename = paste0("m.", ext), type = ext), files_extra),
    row_labels = labels, matrix = m
  )
  if (!is.null(rules)) spec$rules <- rules
  dta_dataset_from_list(spec)
}

mxd_checked <- function(ds, file, stream) {
  d <- load_file(ds, file = file, stream = stream)
  check(d, quiet = TRUE, persist = FALSE)
}

# The structural / cell findings of the column-spec axis, as (column, keyword) rows.
mxd_findings <- function(d, source = "columnspec") {
  m <- as.data.frame(messages(d))
  m <- m[m$source == source, , drop = FALSE]
  m[order(m$keyword, m$column, method = "radix"), c("row", "column", "keyword", "message"), drop = FALSE]
}

# Run `code(d, stream)` for both holdings.
mxd_both <- function(ds, file, code) {
  for (stream in c("never", "always")) {
    code(mxd_checked(ds, file, stream), stream)
  }
}

# ---- the plan's three examples ---------------------------------------------------

test_that("the genes x samples example (named corner, two row labels, fractional counts) validates", {
  ds <- dta_dataset_from_list(yaml::yaml.load('
id: gene_counts
type: matrix
files: { filename: "gene_counts.tsv", type: tsv }
row_labels:
  - id: GENE_ID
    type: SAS Char
    nullable: No
    pattern: "^ENSG[0-9]{11}(\\\\.[0-9]+)?(_PAR_Y)?$"
  - id: GENE_NAME
    type: SAS Char
    nullable: Yes
matrix:
  header:
    corner: [gene_id, gene_name]
    label: Sample ID
    pattern: "^S[0-9]{4}$"
    count: { min: 1, max: 384 }
  rows:
    key: GENE_ID
    count: { min: 3 }
  cells:
    type: SAS Num
    nullable: No
    range: { min: 0 }
    infinite: false
'))
  mxd_both(ds, mxd_fixture("gene_counts.tsv"), function(d, stream) {
    expect_true(validation_status(d)$ok, info = stream)
    expect_identical(names(tables(d)), "gene_counts")
    # Reading names: the row-label ids, then the data labels as written.
    expect_identical(
      names(tables(d)[[1]]),
      c("GENE_ID", "GENE_NAME", "S0001", "S0002", "S0003")
    )
    expect_identical(nrow(messages(d)), 0L)
  })
})

test_that("the samples x miRNAs example (write.csv, blank corner) validates", {
  ds <- dta_dataset_from_list(yaml::yaml.load('
id: mirna
type: matrix
files: { filename: "mature_counts.csv", type: csv }
row_labels: [ { id: SAMPLE, type: SAS Char, nullable: No } ]
matrix:
  header: { corner: blank, pattern: "^hsa-(miR|let)-" }
  cells: { type: SAS Int, nullable: No, range: { min: 0 } }
'))
  mxd_both(ds, mxd_fixture("mature_counts.csv"), function(d, stream) {
    expect_true(validation_status(d)$ok, info = stream)
  })
})

test_that("the genotype example (missing token on the file, values 0/1/2) validates", {
  ds <- dta_dataset_from_list(yaml::yaml.load('
id: geno
type: matrix
files: { filename: "geno.tsv", type: tsv, missing_values: "./." }
row_labels: [ { id: SNP, type: SAS Char, nullable: No } ]
matrix:
  header: { corner: [snp], pattern: "^S[0-9]+$" }
  cells: { type: SAS Int, nullable: Yes, values: [0, 1, 2] }
'))
  mxd_both(ds, mxd_fixture("geno.tsv"), function(d, stream) {
    expect_true(validation_status(d)$ok, info = stream)
  })
  # A value outside 0/1/2 is a finding at its row and column.
  bad <- mxd_file("snp\tS1\tS2\nrs1\t0\t3\n", "geno.tsv")
  mxd_both(ds, bad, function(d, stream) {
    f <- mxd_findings(d)
    expect_identical(f$column, "S2", info = stream)
    expect_equal(f$row, 1L, info = stream)
  })
})

# ---- the corner, writer by writer ------------------------------------------------

test_that("each writer's corner validates, and a wrong one is a corner finding", {
  cases <- list(
    # name, file text, ext, corner, k
    list("write.csv row names", "\"\",S1,S2\n\"r1\",1,2\n", "csv", "blank", 1L),
    list("pandas unnamed index", ",S1,S2\nr1,1,2\n", "csv", "blank", 1L),
    list("write.table row names (no field)", "S1\tS2\nr1\t1\t2\n", "tsv", list(NULL), 1L),
    list("write.table with a description", "desc\tS1\tS2\nr1\tg\t1\t2\n", "tsv", list(NULL, "desc"), 2L),
    list("pandas unnamed index + description", ",desc,S1\nr1,g,1\n", "csv", list("", "desc"), 2L),
    list("pandas named index", "protein_id,S1,S2\nr1,1,2\n", "csv", list("protein_id"), 1L),
    list("header text = ids", "ID\tDESC\tS1\nr1\tg\t1\n", "tsv", "named", 2L),
    list("text with spaces", "Sample ID,S1\nr1,1\n", "csv", list("Sample ID"), 1L)
  )
  for (cs in cases) {
    ds <- mxd_ds(corner = cs[[4]], k = cs[[5]], ext = cs[[3]])
    path <- mxd_file(cs[[2]], paste0("m.", cs[[3]]))
    mxd_both(ds, path, function(d, stream) {
      expect_true(validation_status(d)$ok, info = paste(cs[[1]], stream))
    })
  }

  # Wrong text above the first row label: one corner finding at the row-label id.
  ds <- mxd_ds(corner = "blank")
  path <- mxd_file("wrong\tS1\nr1\t1\n")
  mxd_both(ds, path, function(d, stream) {
    f <- mxd_findings(d)
    expect_identical(f$keyword, "corner", info = stream)
    expect_identical(f$column, "ID", info = stream)
    expect_true(is.na(f$row), info = stream)
    expect_match(f$message, "wrong", info = stream)
  })

  # With a leading null the finding names the row-label id the field belongs to.
  ds <- mxd_ds(corner = list(NULL, "desc"), k = 2L)
  path <- mxd_file("other\tS1\nr1\tg\t1\n")
  mxd_both(ds, path, function(d, stream) {
    f <- mxd_findings(d)
    expect_identical(f$keyword, "corner", info = stream)
    expect_identical(f$column, "DESC", info = stream)
  })
})

# ---- the head gate (T7) ----------------------------------------------------------

test_that("the field-count gate names the case by condition class, before any data is read", {
  load <- function(text, corner = "blank", k = 1L) {
    load_file(mxd_ds(corner = corner, k = k), file = mxd_file(text), stream = "never")
  }
  # Header "", S1, S2 (3 fields), blank corner: a data row should have 3.
  # A data row of 4 means the HEADER is one field short: R write.table writes no
  # field above the row names.
  expect_error(load("\tS1\tS2\nr1\t1\t2\t3\n"), class = "dta_matrix_header_fewer")
  # ... and the same file is fine once the corner declares the missing field.
  expect_no_error(load("S1\tS2\nr1\t1\t2\n", corner = list(NULL)))
  expect_error(load("S1\tS2\nr1\t1\t2\n"), class = "dta_matrix_header_fewer")
  # A data row of 2 means the header has a corner field the spec does not
  # declare: `[null]` given, but the file has the field.
  expect_error(load("\tS1\tS2\nr1\t1\t2\n", corner = list(NULL)), class = "dta_matrix_header_more")
  expect_error(load("\tS1\tS2\nr1\t1\n"), class = "dta_matrix_header_more")
  expect_error(load("\tS1\tS2\nr1\t1\t2\t3\t4\t5\n"), class = "dta_matrix_header_ragged")
  # Every gate error is also the family class, and the hint points the right way.
  e <- expect_error(load("S1\tS2\nr1\t1\t2\n"), class = "dta_matrix_header_gate")
  expect_match(paste(conditionMessage(e), collapse = " "), "null", fixed = TRUE)
  e <- expect_error(load("\tS1\tS2\nr1\t1\t2\n", corner = list(NULL)), class = "dta_matrix_header_gate")
  expect_match(paste(conditionMessage(e), collapse = " "), "drop", fixed = TRUE)
  # A header with fewer fields than the corner needs (k = 2, blank corner).
  expect_error(load("S1\nr1\n", k = 2L), class = "dta_matrix_header_short")
  expect_error(load("S1\nr1\n", k = 2L), class = "dta_matrix_header_gate")
  expect_error(load("#preamble\nS1\tS2\n"), class = "dta_matrix_preamble")
  expect_error(load("a,b,c\nr1,1,2\n"), class = "dta_matrix_delimiter")
  expect_error(load(""), class = "dta_matrix_header_empty")
})


# ---- labels (T4) -----------------------------------------------------------------

test_that("a repeated label is a uniqueNames finding and its cells are still validated under <column p>", {
  ds <- mxd_ds(cells = list(type = "SAS Int", nullable = FALSE, range = list(min = 0)))
  # S1 at file columns 2 and 3; the -2 sits in the second one.
  path <- mxd_file("\tS1\tS1\tS3\nr1\t1\t-2\t3\n")
  mxd_both(ds, path, function(d, stream) {
    f <- mxd_findings(d)
    expect_identical(f$keyword, c("minimum", "uniqueNames"), info = stream)
    expect_identical(f$column, c("<column 3>", "S1"), info = stream)
    expect_equal(f$row[[1]], 1L, info = stream)
    # The finding lists positions and reading names.
    expect_match(f$message[[2]], "column 2", fixed = TRUE, info = stream)
    expect_match(f$message[[2]], "<column 3>", fixed = TRUE, info = stream)
  })
})

test_that("a label equal to a row-label id is a uniqueNames finding", {
  ds <- mxd_ds()
  path <- mxd_file("\tID\tS2\nr1\t1\t2\n")
  mxd_both(ds, path, function(d, stream) {
    f <- mxd_findings(d)
    expect_identical(f$keyword, "uniqueNames", info = stream)
    # The finding sits on the data column (read as <column 2>), not on the clean
    # row-label column; the message still names the colliding id.
    expect_identical(f$column, "<column 2>", info = stream)
    expect_match(f$message, "\"ID\"", fixed = TRUE, info = stream)
    expect_identical(
      names(tables(d)[[1]]), c("ID", "<column 2>", "S2"),
      info = stream
    )
  })
})

test_that("an empty label is a propertyNames finding and its cells carry <column p>", {
  ds <- mxd_ds(cells = list(type = "SAS Int", nullable = FALSE, range = list(min = 0)))
  path <- mxd_file("\tS1\t\tS3\nr1\t1\t-5\t3\n")
  mxd_both(ds, path, function(d, stream) {
    f <- mxd_findings(d)
    expect_identical(f$keyword, c("minimum", "propertyNames"), info = stream)
    expect_identical(f$column, c("<column 3>", "<column 3>"), info = stream)
  })
})

test_that("a label that fails the header pattern is a propertyNames finding at the label", {
  ds <- mxd_ds(header = list(pattern = "^S[0-9]+$"))
  path <- mxd_file("\tS1\tbad\nr1\t1\t2\n")
  mxd_both(ds, path, function(d, stream) {
    f <- mxd_findings(d)
    expect_identical(f$keyword, "propertyNames", info = stream)
    expect_identical(f$column, "bad", info = stream)
    expect_true(is.na(f$row), info = stream)
  })
})

test_that("a label that looks like a generated name does not collide with one", {
  # Column 3 is unlabelled -> "<column 3>"; the label of column 2 is that very text.
  ds <- mxd_ds()
  path <- mxd_file("\t<column 3>\t\nr1\t1\t2\n")
  d <- load_file(ds, file = path, stream = "never")
  nm <- names(tables(d)[[1]])
  expect_identical(anyDuplicated(nm), 0L)
  expect_identical(nm[[1]], "ID")
})

test_that("labels with braces and spaces validate (the clones skip the id validator)", {
  ds <- mxd_ds()
  path <- mxd_file("\ta{b}\tS 1\nr1\t1\t2\n")
  mxd_both(ds, path, function(d, stream) {
    expect_true(validation_status(d)$ok, info = stream)
    expect_identical(names(tables(d)[[1]]), c("ID", "a{b}", "S 1"), info = stream)
  })
})

# ---- dimensions ------------------------------------------------------------------

test_that("data-column bounds are minProperties / maxProperties findings without a column", {
  path <- mxd_file("\tS1\tS2\tS3\nr1\t1\t2\t3\n")

  low <- mxd_ds(header = list(count = list(min = 5)))
  mxd_both(low, path, function(d, stream) {
    f <- mxd_findings(d)
    expect_identical(f$keyword, "minProperties", info = stream)
    expect_true(is.na(f$column), info = stream)
  })

  high <- mxd_ds(header = list(count = list(max = 2)))
  mxd_both(high, path, function(d, stream) {
    f <- mxd_findings(d)
    expect_identical(f$keyword, "maxProperties", info = stream)
    expect_true(is.na(f$column), info = stream)
  })

  exact <- mxd_ds(header = list(count = 3))
  mxd_both(exact, path, function(d, stream) {
    expect_true(validation_status(d)$ok, info = stream)
  })
})

test_that("row-count bounds are minItems / maxItems findings (the engine counts the rows)", {
  path <- mxd_file("\tS1\nr1\t1\nr2\t2\n")

  few <- mxd_ds(rows = list(count = list(min = 3)))
  mxd_both(few, path, function(d, stream) {
    f <- mxd_findings(d)
    expect_identical(f$keyword, "minItems", info = stream)
    expect_true(is.na(f$column), info = stream)
  })

  many <- mxd_ds(rows = list(count = list(max = 1)))
  mxd_both(many, path, function(d, stream) {
    expect_identical(mxd_findings(d)$keyword, "maxItems", info = stream)
  })

  exact <- mxd_ds(rows = list(count = 2))
  mxd_both(exact, path, function(d, stream) {
    expect_true(validation_status(d)$ok, info = stream)
  })
})

test_that("a header-only delivery fails by default and passes when zero rows are written", {
  path <- mxd_file("\tS1\tS2\n")
  mxd_both(mxd_ds(), path, function(d, stream) {
    expect_identical(mxd_findings(d)$keyword, "minItems", info = stream)
  })
  mxd_both(mxd_ds(rows = list(count = list(min = 0))), path, function(d, stream) {
    expect_true(validation_status(d)$ok, info = stream)
  })
})

# ---- cells -----------------------------------------------------------------------

test_that("range and finiteness findings carry the row and the reading name of the column", {
  ds <- mxd_ds(cells = list(
    type = "SAS Num", nullable = FALSE, range = list(max = 10), infinite = FALSE
  ))
  # r1: S1 = 11 is above the maximum; S2 = Inf is not finite. r2 is clean.
  path <- mxd_file("\tS1\tS2\nr1\t11\tInf\nr2\t1\t2\n")
  mxd_both(ds, path, function(d, stream) {
    f <- mxd_findings(d)
    expect_identical(f$keyword, c("finite", "maximum"), info = stream)
    expect_identical(f$column, c("S2", "S1"), info = stream)
    expect_equal(f$row, c(1L, 1L), info = stream)
  })
})

test_that("a missing cell fails a non-nullable cell spec at its row and column", {
  ds <- mxd_ds()
  path <- mxd_file("\tS1\tS2\nr1\t1\t\nr2\t3\t4\n")
  mxd_both(ds, path, function(d, stream) {
    f <- mxd_findings(d)
    expect_identical(f$keyword, "type", info = stream)
    expect_identical(f$column, "S2", info = stream)
    expect_equal(f$row, 1L, info = stream)
  })
})

test_that("an untypable cell is an import finding on the cell's column", {
  ds <- mxd_ds()
  path <- mxd_file("\tS1\tS2\nr1\t1\tabc\n")
  mxd_both(ds, path, function(d, stream) {
    expect_identical(validation_status(d)$n_import_errors, 1L, info = stream)
  })
})

# ---- the row key -----------------------------------------------------------------

test_that("a repeated row key is a .matrix:row_key rule error that explain_messages() resolves", {
  ds <- mxd_ds()
  path <- mxd_file("\tS1\nr1\t1\nr1\t2\nr2\t3\n")
  mxd_both(ds, path, function(d, stream) {
    m <- as.data.frame(messages(d))
    expect_identical(m$rule_id[m$source == "rule"], ".matrix:row_key", info = stream)
    expect_identical(validation_status(d)$n_rule_errors, 1L, info = stream)

    rule <- dta_find_rule_def(d, "m", ".matrix:row_key")
    expect_s3_class(rule, "DTAtools::DTARuleColUnique")
    expect_identical(rule@columns, "ID", info = stream)

    out <- suppressMessages(explain_messages(d))
    expect_gte(nrow(out), 1L)
    expect_identical(out$should_be, "No two rows may have the same value in column ID.", info = stream)
  })
})

test_that("a composite row key is tuple-unique", {
  ds <- mxd_ds(k = 2L, corner = "named", rows = list(key = list("ID", "DESC")), all_required = TRUE)
  # (a, x) repeats; (a, y) and (b, x) are distinct although each id repeats.
  path <- mxd_file("ID\tDESC\tS1\na\tx\t1\na\ty\t2\nb\tx\t3\na\tx\t4\n")
  mxd_both(ds, path, function(d, stream) {
    expect_identical(validation_status(d)$n_rule_errors, 1L, info = stream)
  })
})

test_that("user rules on row-label columns still run beside the generated rule", {
  ds <- mxd_ds(rules = list(list(id = "id_again", type = "col_unique", columns = "ID")))
  expect_true(any(vapply(dta_matrix_rules(ds), function(r) identical(r@id, "id_again"), logical(1))))
  expect_identical(dta_matrix_rules(ds)[[2]]@id, ".matrix:row_key")
})

# ---- stale state (T11, T12) ------------------------------------------------------

test_that("an eager table whose names no longer match the spec is that table's failure, not an abort", {
  ds <- mxd_ds()
  path <- mxd_file("\tS1\nr1\t1\n")
  d <- load_file(ds, file = path, stream = "never")
  expect_true(validation_status(check(d, quiet = TRUE, persist = FALSE))$ok)

  # The row label is renamed after loading.
  d2 <- d
  S7::prop(d2, "specs", check = FALSE) <- DTAColumnSpecCollection(columns = list(
    ROW = DTAColumnSpec(id = "ROW", type = "SAS Char", nullable = FALSE)
  ))
  S7::prop(d2@matrix, "row_key", check = FALSE) <- "ROW"
  res <- check(d2, quiet = TRUE, persist = FALSE)
  st <- validation_status(res)
  expect_false(st$ok)
  expect_match(as.data.frame(messages(res))$message, "load it again", fixed = TRUE)

  # ... and a second dataset in the same DTA still validates.
  other <- create_example_DTADataSetTabular(2)
  dta <- DTA(datasets = list(d2, other))
  res <- check(dta, quiet = TRUE, persist = FALSE)
  expect_false(validation_status(datasets(res, "m"))$ok)
  # The other dataset was still validated (its own verdict is not this test's business).
  expect_identical(validation_status(datasets(res, "demographics"))$status, "validated")
})

test_that("a table with no recorded head is that table's failure", {
  ds <- mxd_ds()
  d <- load_file(ds, file = mxd_file("\tS1\nr1\t1\n"), stream = "never")
  d@heads <- list()
  res <- check(d, quiet = TRUE, persist = FALSE)
  expect_false(validation_status(res)$ok)
  expect_match(as.data.frame(messages(res))$message, "no recorded header", fixed = TRUE)
})

test_that("a lazily held file edited in place is that table's failure", {
  ds <- mxd_ds()
  path <- mxd_file("\tS1\tS2\nr1\t1\t2\n")
  d <- load_file(ds, file = path, stream = "always")
  expect_true(validation_status(check(d, quiet = TRUE, persist = FALSE))$ok)

  # Same size, a later mtime: the skip gate must open on its own, no force.
  Sys.sleep(1.1)
  writeBin(charToRaw("\tS1\tS3\nr1\t1\t2\n"), path)
  res <- check(d, quiet = TRUE, persist = FALSE)
  expect_false(validation_status(res)$ok)
  expect_match(as.data.frame(messages(res))$message, "changed since load_file", fixed = TRUE)
})

test_that("a changed cell spec revalidates and a changed header spec is not skipped as unchanged", {
  ds <- mxd_ds(cells = list(type = "SAS Int", nullable = FALSE, range = list(min = 0)))
  d <- load_file(ds, file = mxd_file("\tS1\nr1\t5\n"), stream = "never")
  d <- check(d, quiet = TRUE, persist = FALSE)
  expect_true(validation_status(d)$ok)

  d@matrix <- dta_matrix_spec_from_list(list(
    header = list(corner = "blank"),
    cells = list(type = "SAS Int", nullable = FALSE, range = list(min = 10))
  ))
  d <- check(d, quiet = TRUE, persist = FALSE)
  expect_false(validation_status(d)$ok)
  expect_identical(mxd_findings(d)$keyword, "minimum")
})

# ---- containers (T31, T32) -------------------------------------------------------

test_that("a .zip path is refused by load_file() before any head is read", {
  ds <- mxd_ds()
  zip_path <- mxd_file(as.raw(c(0x50, 0x4b, 0x03, 0x04)), "m.zip")
  expect_error(load_file(ds, file = zip_path), class = "dta_load_file_container")
})

test_that("load_delivery() binds a matrix from a zip and container_findings() sees it recorded", {
  dta <- DTA(datasets = list(mxd_ds()))
  z <- cont_zip(list("m.tsv" = "\tS1\tS2\nr1\t1\t2\nr2\t3\t4\n"), "delivery.zip")
  loaded <- load_delivery(dta, z)
  ds <- datasets(loaded, "m")
  expect_s3_class(ds, "DTAtools::DTADataSetMatrix")
  expect_identical(names(tables(ds)), "m")
  expect_identical(ds@provenance[["m"]]$container, "delivery.zip")
  expect_identical(ds@provenance[["m"]]$via, "delivery")
  expect_identical(names(ds@heads), "m")

  res <- check(loaded, quiet = TRUE, persist = FALSE)
  expect_true(validation_status(datasets(res, "m"))$ok)

  # Delivered inside the declared container: nothing to report.
  expect_identical(nrow(container_findings(loaded)), 0L)

  # The same matrix delivered loose is recorded and judged: outside.
  contained <- DTA(
    datasets = list(mxd_ds()),
    containers = list(DTAContainer("main", "delivery.zip", default = TRUE))
  )
  loose <- load_delivery(contained, cont_write("m.tsv", "\tS1\tS2\nr1\t1\t2\n"))
  cf <- container_findings(loose)
  expect_identical(nrow(cf), 1L)
  expect_identical(cf$dataset, "m")
  expect_identical(cf$arrived, "loose")
  expect_identical(cf$status, "outside")
})

test_that("load_delivery() route = binds an ambiguous file to a matrix", {
  mx <- function(id, pattern) {
    dta_dataset_from_list(list(
      id = id, type = "matrix", files = list(filename = pattern, type = "tsv", pattern = TRUE),
      row_labels = list(list(id = "ID", type = "SAS Char", nullable = FALSE)),
      matrix = list(header = list(corner = "blank"), cells = list(type = "SAS Int", nullable = FALSE))
    ))
  }
  dta <- DTA(datasets = list(mx("a", "^a.*[.]tsv$"), mx("b", "^.*e[.]tsv$")))
  f <- cont_write("ae.tsv", "\tS1\tS2\nr1\t1\t2\n")
  expect_error(suppressMessages(load_delivery(dta, f)), class = "dta_delivery_nothing_loaded")
  res <- suppressMessages(load_delivery(dta, f, route = list("ae.tsv" = "b")))
  expect_identical(names(tables(datasets(res, "b"))), "ae")
  expect_identical(length(tables(datasets(res, "a"))), 0L)
  expect_identical(names(datasets(res, "b")@heads), "ae")
  chk <- check(res, quiet = TRUE, persist = FALSE)
  expect_true(validation_status(datasets(chk, "b"))$ok)
})

# ---- construction (T2) -----------------------------------------------------------

test_that("the factory and the constructor refuse the wrong keys by class", {
  rl <- list(list(id = "ID", type = "SAS Char", nullable = FALSE))
  m <- list(header = list(corner = "blank"), cells = list(type = "SAS Int"))
  f <- list(type = "tsv", filename = "m.tsv")

  expect_error(
    DTADataSetFactory("matrix", id = "a", files = f, columns = rl, matrix = m),
    class = "dta_matrix_columns_key"
  )
  expect_error(
    DTADataSetFactory("tabular", id = "a", files = f, row_labels = rl),
    class = "dta_row_labels_key"
  )
  expect_error(
    DTADataSetFactory("file", id = "a", files = f, matrix = m),
    class = "dta_matrix_key"
  )
  expect_s3_class(
    DTADataSetFactory("matrix", id = "a", files = f, row_labels = rl, matrix = m),
    "DTAtools::DTADataSetMatrix"
  )

  specs <- DTAColumnSpecCollection(columns = list(ID = DTAColumnSpec(id = "ID", type = "SAS Char", nullable = FALSE)))
  expect_error(
    DTADataSetMatrix(id = "a", specs = specs, matrix = m, tables = list(t = data.frame(a = 1))),
    class = "dta_matrix_tables_refused"
  )
})

test_that("the validator refuses an inconsistent matrix by class", {
  col <- function(id, ...) DTAColumnSpec(id = id, type = "SAS Char", ...)
  specs <- function(...) DTAColumnSpecCollection(columns = list(...))
  m <- list(header = list(corner = "blank"), cells = list(type = "SAS Int"))
  tsv <- list(DTAFileTSV(filename = "m.tsv"))

  expect_error(
    DTADataSetMatrix(id = "a", specs = DTAColumnSpecCollection(columns = list()), matrix = m),
    class = "dta_matrix_invalid_row_labels"
  )
  expect_error(
    DTADataSetMatrix(
      id = "a", specs = specs(ID = col("ID", nullable = FALSE)),
      matrix = list(header = list(corner = "blank"), rows = list(key = "NOPE"), cells = list(type = "SAS Int"))
    ),
    class = "dta_matrix_invalid_row_key"
  )
  expect_error(
    DTADataSetMatrix(id = "a", specs = specs(ID = col("ID", nullable = TRUE)), matrix = m),
    class = "dta_matrix_invalid_key_nullable"
  )
  expect_error(
    DTADataSetMatrix(
      id = "a", specs = specs(ID = col("ID", nullable = FALSE)),
      matrix = list(header = list(corner = list("a", "b")), cells = list(type = "SAS Int"))
    ),
    class = "dta_matrix_invalid_corner"
  )
  expect_error(
    DTADataSetMatrix(
      id = "a", specs = specs(ID = col("ID", nullable = FALSE)), matrix = m,
      files = list(DTAFileTSV(filename = "m.tsv", has_header = FALSE))
    ),
    class = "dta_matrix_invalid_file"
  )
  expect_error(
    DTADataSetMatrix(
      id = "a", specs = specs(ID = col("ID", nullable = FALSE)), matrix = m,
      files = list(DTAFileDelim(filename = "m.txt", sep = ";"))
    ),
    class = "dta_matrix_invalid_file"
  )
  with_rule <- DTAColumnSpecCollection(
    columns = list(ID = col("ID", nullable = FALSE)),
    rules = list(DTARuleColUnique(id = ".matrix:mine", columns = "ID"))
  )
  expect_error(
    DTADataSetMatrix(id = "a", specs = with_rule, matrix = m, files = tsv),
    class = "dta_matrix_invalid_rule"
  )
})

test_that("the row key defaults to the first row label and the spec round-trips", {
  ds <- mxd_ds(k = 2L, corner = "named")
  expect_identical(ds@matrix@row_key, "ID")
  expect_identical(ds@type, "matrix")
  expect_identical(ds@heads, list())
  expect_identical(as.list(ds@matrix)$header$corner, "named")
})

# ---- the helpers, directly -------------------------------------------------------

test_that("reading names follow the plan: ids, then usable labels, else <column p>", {
  ds <- mxd_ds(corner = list(NULL, "d"), k = 2L)
  # k = 2, one corner field (d): data labels start at header field 2.
  expect_identical(
    dta_matrix_reading_names(c("d", "S1", "S2"), ds),
    c("ID", "DESC", "S1", "S2")
  )
  # Empty, repeated and id-equal labels are generated from the data-row position.
  expect_identical(
    dta_matrix_reading_names(c("d", "", "A", "A", "DESC", "Z"), ds),
    c("ID", "DESC", "<column 3>", "<column 4>", "<column 5>", "<column 6>", "Z")
  )
})

test_that("header findings are ordered by position and declare their denominators", {
  ds <- mxd_ds(corner = "named", k = 2L, header = list(pattern = "^S[0-9]$", count = list(max = 2)))
  hf <- dta_matrix_head_findings(c("x", "DESC", "S1", "Q", "S1"), ds)
  expect_identical(
    hf$findings$keyword,
    c("corner", "uniqueNames", "propertyNames", "maxProperties")
  )
  expect_identical(hf$findings$column, c("ID", "S1", "Q", NA))
  expect_identical(
    hf$declared,
    c(
      corner = 2L, propertyNames = 3L, uniqueNames = 3L, minProperties = 1L,
      maxProperties = 1L, minItems = 1L, maxItems = 0L
    )
  )
})

test_that("effective specs hold the labels, one clone per data column and the key rule", {
  ds <- mxd_ds(cells = list(type = "SAS Num", nullable = FALSE, range = list(min = 0)))
  eff <- dta_matrix_effective_specs(ds, c("ID", "S 1", "a{b}"))
  expect_identical(names(eff@columns), c("ID", "S 1", "a{b}"))
  expect_identical(eff@columns[["S 1"]]@id, "S 1")
  expect_identical(eff@columns[["a{b}"]]@minimum, 0)
  expect_identical(vapply(eff@rules, function(r) r@id, ""), ".matrix:row_key")
  # The dataset's own specs are untouched.
  expect_identical(names(ds@specs@columns), "ID")
})

# ---- print -----------------------------------------------------------------------

test_that("a matrix prints, short-prints and autoprints, loaded or not", {
  ds <- mxd_ds(corner = "named", k = 2L)
  loaded <- load_file(mxd_ds(), file = mxd_file("\tS1\nr1\t1\n"), stream = "never")
  for (d in list(ds, loaded)) {
    out <- NULL
    expect_no_error(out <- capture.output(print(d), type = "message"))
    expect_true(any(grepl("Matrix:", out, fixed = TRUE)))
    expect_true(any(grepl("corner:", out, fixed = TRUE)))
    expect_true(any(grepl("<DTADataSetMatrix>", out, fixed = TRUE)))
    expect_false(any(grepl("DTADataSetTabular", out, fixed = TRUE)))
    expect_no_error(capture.output(print_short_info(d), type = "message"))
    expect_no_error(capture.output(d))
    expect_false(suppressMessages(withVisible(print(d)))$visible)
  }
})

test_that("printing a tabular dataset is unchanged by the shared print body", {
  d <- create_example_DTADataSetTabular(2)
  expect_identical(
    capture.output(print(d), type = "message"),
    capture.output(dta_print_tabular(d), type = "message")
  )
  expect_false(suppressMessages(withVisible(print(d)))$visible)
  # The pre-refactoring method, from the base commit, writes the same lines.
  base <- tryCatch(
    suppressWarnings(system2("git", c("show", "f6189bf:R/DTADataSetTabular-class.R"), stdout = TRUE, stderr = FALSE)),
    error = function(e) character(0)
  )
  skip_if(length(base) == 0L, "the base commit is not available")
  start <- grep("^method\\(print, DTADataSetTabular\\) <- function", base)
  skip_if(length(start) != 1L, "the base method was not found")
  end <- start + which(base[start:length(base)] == "}")[1] - 1L
  old <- eval(parse(text = base[start:end])[[1]][[3]], asNamespace("DTAtools"))
  expect_identical(
    capture.output(old(d), type = "message"),
    capture.output(print(d), type = "message")
  )
})

# ---- an object saved before `heads` existed (T31) --------------------------------

test_that("an older-shaped matrix is upgraded, fails its check legibly and loads again", {
  path <- mxd_file("\tS1\tS2\nr1\t1\t2\n")
  d <- load_file(mxd_ds(), file = path, stream = "never")
  old <- d
  cls <- attr(old, "S7_class")
  props <- attr(cls, "properties")
  props$heads <- NULL
  attr(cls, "properties") <- props
  attr(old, "S7_class") <- cls
  attr(old, "heads") <- NULL

  expect_true(dta_is_stale(old))
  expect_identical(attr(dta_upgrade(old), "heads"), list())

  res <- check(old, quiet = TRUE, persist = FALSE)
  expect_false(validation_status(res)$ok)
  expect_match(as.data.frame(messages(res))$message, "no recorded header", fixed = TRUE)

  again <- load_file(old, file = path, stream = "never")
  expect_identical(names(again@heads), "m")
  expect_true(validation_status(check(again, quiet = TRUE, persist = FALSE))$ok)
})

# ---- the header findings at scale ------------------------------------------------

test_that("header findings for a large failing header are cheap and ordered", {
  ds <- mxd_ds(header = list(pattern = "^S[0-9]+$"))
  n <- 20000L
  bad <- c("", sprintf("g%05d", seq_len(n)))
  hf <- dta_matrix_head_findings(bad, ds)
  expect_identical(nrow(hf$findings), n)
  expect_identical(unique(hf$findings$keyword), "propertyNames")
  expect_identical(hf$findings$column[c(1L, n)], c("g00001", sprintf("g%05d", n)))

  dup <- c("", rep(sprintf("S%05d", seq_len(n %/% 2L)), 2L))
  hf <- dta_matrix_head_findings(dup, ds)
  expect_identical(nrow(hf$findings), n %/% 2L)
  expect_identical(unique(hf$findings$keyword), "uniqueNames")
})

test_that("counts in the bound findings are written out, not in scientific notation", {
  ds <- mxd_ds(header = list(count = list(min = 1000000)))
  hf <- dta_matrix_head_findings(c("", "S1"), ds)
  f <- hf$findings[hf$findings$keyword == "minProperties", ]
  expect_identical(f$columnspec, "1000000")
  expect_match(f$message, "at least 1000000 required", fixed = TRUE)
})
