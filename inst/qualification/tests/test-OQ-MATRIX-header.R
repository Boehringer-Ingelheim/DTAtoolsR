# The header of a matrix file. REQ-MATRIX-001 .. REQ-MATRIX-017.
#
# The header decides what every later column is called, so these cases state,
# for each shape of header a writer produces, what the reader must do with it.
# Every file is written by the case that uses it, as bytes. Every expected value
# comes from the requirement sentences: the field-count gate from the formula
# h + (k - c), the header texts from the writer table, the reading names from the
# naming rule.
#
# Each case runs on the table held in memory (stream = "never") and scanned in
# batches (stream = "always").

# ---- the three examples a specification is written from ------------------------

test_that("OQ-MATRIX-001 | a matrix of each shape in the specification validates clean and binds one table named after its file | REQ-MATRIX-001", {
  # Genes x samples: two row labels, header texts differing from the ids, fractional counts.
  genes <- dta_dataset_from_list(list(
    id = "gene_counts", type = "matrix",
    files = list(filename = "gene_counts.tsv", type = "tsv"),
    row_labels = list(
      list(
        id = "GENE_ID", header = "gene_id", type = "SAS Char", nullable = FALSE,
        pattern = "^ENSG[0-9]{11}(\\.[0-9]+)?(_PAR_Y)?$"
      ),
      list(id = "GENE_NAME", header = "gene_name", type = "SAS Char", nullable = TRUE)
    ),
    matrix = list(
      columns = list(
        label = "Sample ID",
        pattern = "^S[0-9]{4}$", count = list(min = 1, max = 384)
      ),
      rows = list(key = "GENE_ID", count = list(min = 3)),
      cells = list(type = "SAS Num", nullable = FALSE, range = list(min = 0), infinite = FALSE)
    )
  ))
  genes_file <- mx_path(
    paste0(
      "gene_id\tgene_name\tS0001\tS0002\tS0003\n",
      "ENSG00000000003.15\tTSPAN6\t10.5\t0\t7\n",
      "ENSG00000000005\t\t3\t25\t1\n",
      "ENSG00000000419_PAR_Y\tDPM1\t0\t0\t2.25\n"
    ),
    "gene_counts.tsv"
  )
  mx_each(genes, genes_file, function(d, stream) {
    qa_step(sprintf("the gene matrix is valid and silent (%s)", stream), TRUE, mh_clean(d))
    qa_step(sprintf("one table, named after the file (%s)", stream), "gene_counts", names(tables(d)))
    qa_step(
      sprintf("row-label ids first, then the data labels in file order (%s)", stream),
      c("GENE_ID", "GENE_NAME", "S0001", "S0002", "S0003"), mh_names(d)
    )
    qa_step(
      sprintf("it is a tabular dataset of type matrix (%s)", stream),
      c(TRUE, TRUE, "matrix"),
      c(
        inherits(d, "DTAtools::DTADataSetMatrix"), inherits(d, "DTAtools::DTADataSetTabular"),
        d@type
      )
    )
  })

  # Samples x miRNAs: R write.csv, an empty header text.
  mirna <- dta_dataset_from_list(list(
    id = "mirna", type = "matrix",
    files = list(filename = "mirna.csv", type = "csv"),
    row_labels = list(list(id = "SAMPLE", header = "", type = "SAS Char", nullable = FALSE)),
    matrix = list(
      columns = list(pattern = "^hsa-(miR|let)-"),
      cells = list(type = "SAS Int", nullable = FALSE, range = list(min = 0))
    )
  ))
  mirna_file <- mx_path(
    "\"\",hsa-miR-21-5p,hsa-let-7a-5p\n\"S1\",10,3\n\"S2\",0,55\n",
    "mirna.csv"
  )
  mx_each(mirna, mirna_file, function(d, stream) {
    qa_step(sprintf("the miRNA matrix is valid and silent (%s)", stream), TRUE, mh_clean(d))
  })

  # Genotypes: a missing token declared on the file, values 0, 1 and 2 only.
  geno <- dta_dataset_from_list(list(
    id = "geno", type = "matrix",
    files = list(filename = "geno.tsv", type = "tsv", missing_values = "./."),
    row_labels = list(list(id = "SNP", header = "snp", type = "SAS Char", nullable = FALSE)),
    matrix = list(
      columns = list(pattern = "^S[0-9]+$"),
      cells = list(type = "SAS Int", nullable = TRUE, values = list(0, 1, 2))
    )
  ))
  mx_each(geno, mx_path("snp\tS1\tS2\nrs1\t0\t./.\nrs2\t1\t2\n", "geno.tsv"), function(d, stream) {
    qa_step(sprintf("the genotype matrix is valid and silent (%s)", stream), TRUE, mh_clean(d))
  })
  mx_each(geno, mx_path("snp\tS1\tS2\nrs1\t0\t3\n", "geno.tsv"), function(d, stream) {
    qa_step(
      sprintf("a call outside 0, 1, 2 is reported at its row and column (%s)", stream),
      mx_expect("columnspec", 1, "S2", "enum"), mx_found(d)
    )
  })
})

# ---- head records and tokenising ----------------------------------------------

test_that("OQ-MATRIX-002 | blank lines and a byte-order mark before the header are skipped | REQ-MATRIX-002", {
  body <- "\tS1\tS2\nr1\t1\t2\n"
  bom <- as.raw(c(0xef, 0xbb, 0xbf))
  variants <- list(
    "no preamble" = charToRaw(body),
    "two LF lines" = charToRaw(paste0("\n\n", body)),
    "a CRLF line" = charToRaw(paste0("\r\n", body)),
    "a lone CR line" = charToRaw(paste0("\r", body)),
    "a byte-order mark" = c(bom, charToRaw(body))
  )
  # The header text is empty: a mark left on the first field, or a blank line taken
  # for the header, would make it differ and be reported.
  ds <- mx_ds()
  for (nm in names(variants)) {
    path <- mx_path(variants[[nm]])
    mx_each(ds, path, function(d, stream) {
      qa_step(sprintf("%s reads as the plain file (%s)", nm, stream), TRUE, mh_clean(d))
      qa_step(sprintf("%s: the columns (%s)", nm, stream), c("ID", "S1", "S2"), mh_names(d))
    })
  }
})

test_that("OQ-MATRIX-003 | header fields are text: 007, 1e3, NA and a declared missing token stay as written | REQ-MATRIX-003", {
  ds <- mx_ds(files_extra = list(missing_values = "."))
  path <- mx_path("\t007\t1e3\tNA\t.\nr1\t1\t2\t3\t4\n")
  mx_each(ds, path, function(d, stream) {
    qa_step(
      sprintf("the labels are read verbatim (%s)", stream),
      c("ID", "007", "1e3", "NA", "."), mh_names(d)
    )
    qa_step(sprintf("and none is a finding (%s)", stream), TRUE, mh_clean(d))
  })
})

# ---- the field-count gate ---------------------------------------------------------

test_that("OQ-MATRIX-004 | a first data row of the wrong width aborts the load as a header-gate error | REQ-MATRIX-004", {
  ds <- mx_ds()
  # Header text "", k = 1, c = 1: a header of h fields needs data rows of h fields.
  for (stream in MX_STREAMS) {
    ok <- mx_load_error(ds, mx_path("\tS1\tS2\nr1\t1\t2\n"), stream)
    qa_step(sprintf("3 header fields, 3 in the data row: accepted (%s)", stream), NULL, ok)
    one_short <- mx_load_error(ds, mx_path("\tS1\tS2\nr1\t1\n"), stream)
    qa_step(
      sprintf("3 header fields, 2 in the data row: a gate error (%s)", stream),
      TRUE, mx_is(one_short, "dta_matrix_header_gate")
    )
    one_long <- mx_load_error(ds, mx_path("\tS1\tS2\nr1\t1\t2\t3\n"), stream)
    qa_step(
      sprintf("3 header fields, 4 in the data row: a gate error (%s)", stream),
      TRUE, mx_is(one_long, "dta_matrix_header_gate")
    )
    for (empty in list("", "\n\n", "\r\n")) {
      e <- mx_load_error(ds, mx_path(empty), stream)
      qa_step(
        sprintf("a file with no non-blank line (%d bytes): empty and a gate error (%s)", nchar(empty), stream),
        c(TRUE, TRUE),
        c(mx_is(e, "dta_matrix_header_empty"), mx_is(e, "dta_matrix_header_gate"))
      )
    }
  }
  # With two row labels, header: No on the first and a text on the second, c = 1: the data
  # row needs h + 1 fields.
  ds2 <- mx_ds(row_header = list(FALSE, "d"), k = 2L)
  qa_step(
    "k = 2, c = 1, header of 3: a data row of 4 is accepted",
    NULL, mx_load_error(ds2, mx_path("d\tS1\tS2\nr1\tg\t1\t2\n"))
  )
  qa_step(
    "and one of 3 is not",
    TRUE, mx_is(mx_load_error(ds2, mx_path("d\tS1\tS2\nr1\t1\t2\n")), "dta_matrix_header_gate")
  )
})

test_that("OQ-MATRIX-005 | a data row one field wider than the header implies is header_fewer, and the message names header: No | REQ-MATRIX-005", {
  # R write.table with row names writes no field above them: h = 2, but the
  # data rows have 3. Declared with an empty header text this is one field too many.
  path <- mx_path("S1\tS2\nr1\t1\t2\n")
  for (stream in MX_STREAMS) {
    e <- mx_load_error(mx_ds(), path, stream)
    qa_step(sprintf("class dta_matrix_header_fewer (%s)", stream), TRUE, mx_is(e, "dta_matrix_header_fewer"))
    msg <- paste(conditionMessage(e), collapse = " ")
    qa_step(
      sprintf("the message names header: No on the first row label (%s)", stream),
      TRUE, grepl("header: No", msg, fixed = TRUE)
    )
  }
  # Declaring the missing field is the remedy the message names.
  mx_each(mx_ds(row_header = list(FALSE)), path, function(d, stream) {
    qa_step(sprintf("with header: No on the first row label the file is valid (%s)", stream), TRUE, mh_clean(d))
  })
  # header: No is declared and the data row is still one field too wide
  # (h = 2, so 3 are wanted, and it has 4): declaring it again cannot help.
  for (stream in MX_STREAMS) {
    e <- mx_load_error(mx_ds(row_header = list(FALSE)), mx_path("S1\tS2\nr1\t1\t2\t3\n"), stream)
    qa_step(sprintf("already declared: class dta_matrix_header_fewer (%s)", stream), TRUE, mx_is(e, "dta_matrix_header_fewer"))
    msg <- paste(conditionMessage(e), collapse = " ")
    qa_step(
      sprintf("already declared: the message says the data row has a field too many (%s)", stream),
      TRUE, grepl("field too many", gsub("[[:space:]]+", " ", msg), fixed = TRUE)
    )
  }
})

test_that("OQ-MATRIX-006 | a data row one field narrower is header_more and one two or more away is header_ragged | REQ-MATRIX-006 REQ-MATRIX-007", {
  # header: No says the header has no field above the row names, but this
  # header starts with one: h = 3, c = 0, the data row has 3, and 4 are needed.
  for (stream in MX_STREAMS) {
    e <- mx_load_error(mx_ds(row_header = list(FALSE)), mx_path("\tS1\tS2\nr1\t1\t2\n"), stream)
    qa_step(sprintf("class dta_matrix_header_more (%s)", stream), TRUE, mx_is(e, "dta_matrix_header_more"))
    qa_step(
      sprintf("and not the ragged class (%s)", stream),
      FALSE, mx_is(e, "dta_matrix_header_ragged")
    )
    qa_step(
      sprintf("the message names the row label's header setting (%s)", stream),
      TRUE, grepl("header: No", paste(conditionMessage(e), collapse = " "), fixed = TRUE)
    )
    # Without header: No (h = 3, 3 are wanted, the row has 2) the row is short.
    short <- mx_load_error(mx_ds(), mx_path("\tS1\tS2\nr1\t1\n"), stream)
    qa_step(sprintf("no header: No: class dta_matrix_header_more (%s)", stream), TRUE, mx_is(short, "dta_matrix_header_more"))
    qa_step(
      sprintf("no header: No: the message says the first data row is missing a field (%s)", stream),
      TRUE, grepl("missing a field", gsub("[[:space:]]+", " ", conditionMessage(short)), fixed = TRUE)
    )

    # Three fields where 3 + 3 are wanted: far off in either direction.
    wide <- mx_load_error(mx_ds(), mx_path("\tS1\tS2\nr1\t1\t2\t3\t4\t5\n"), stream)
    qa_step(
      sprintf("a data row three fields too wide is dta_matrix_header_ragged (%s)", stream),
      c(TRUE, FALSE, FALSE),
      c(
        mx_is(wide, "dta_matrix_header_ragged"), mx_is(wide, "dta_matrix_header_fewer"),
        mx_is(wide, "dta_matrix_header_more")
      )
    )
    narrow <- mx_load_error(mx_ds(), mx_path("\tS1\tS2\tS3\tS4\nr1\t1\n"), stream)
    qa_step(
      sprintf("and one three fields too narrow too (%s)", stream),
      TRUE, mx_is(narrow, "dta_matrix_header_ragged")
    )
  }
})

test_that("OQ-MATRIX-007 | a preamble line and a header that is one field holding another delimiter are refused by class | REQ-MATRIX-008 REQ-MATRIX-009", {
  for (stream in MX_STREAMS) {
    pre <- mx_load_error(mx_ds(), mx_path("#featureCounts v2\n\tS1\tS2\nr1\t1\t2\n"), stream)
    qa_step(sprintf("a first record starting with # (%s)", stream), TRUE, mx_is(pre, "dta_matrix_preamble"))
    other <- mx_load_error(mx_ds(), mx_path("a,b,c\nr1,1,2\n"), stream)
    qa_step(
      sprintf("a comma-separated file declared as tab-separated (%s)", stream),
      TRUE, mx_is(other, "dta_matrix_delimiter")
    )
    # The same text in a comma-separated declaration is an ordinary header.
    fine <- mx_load_error(mx_ds(row_header = "a", ext = "csv"), mx_path("a,b,c\nr1,1,2\n", "m.csv"), stream)
    qa_step(sprintf("and is no error when it is declared as comma-separated (%s)", stream), NULL, fine)
  }
})

# ---- the header text of the row labels ----------------------------------------------------------------

test_that("OQ-MATRIX-008 | the header each common writer produces validates when the row labels declare it as the requirement says | REQ-MATRIX-010", {
  cases <- list(
    list("R write.csv, row names (header: \"\")", "\"\",S1,S2\n\"r1\",1,2\n", "csv", "", 1L),
    list("pandas, unnamed index (header: \"\")", ",S1,S2\nr1,1,2\n", "csv", "", 1L),
    list("R write.table, row names (header: No)", "S1\tS2\nr1\t1\t2\n", "tsv", list(FALSE), 1L),
    list("write.table with a description column", "desc\tS1\tS2\nr1\tg\t1\t2\n", "tsv", list(FALSE, "desc"), 2L),
    list("pandas, unnamed index and a description", ",desc,S1\nr1,g,1\n", "csv", list("", "desc"), 2L),
    list("pandas, named index (header equal to its name)", "protein_id,S1,S2\nr1,1,2\n", "csv", list("protein_id"), 1L),
    list("header texts equal to the row-label ids (no header key)", "ID\tDESC\tS1\nr1\tg\t1\n", "tsv", list(), 2L),
    list("a header text with a space", "Sample ID,S1\nr1,1\n", "csv", list("Sample ID"), 1L)
  )
  for (cs in cases) {
    ds <- mx_ds(row_header = cs[[4]], k = cs[[5]], ext = cs[[3]])
    path <- mx_path(cs[[2]], paste0("m.", cs[[3]]))
    mx_each(ds, path, function(d, stream) {
      qa_step(sprintf("%s: no rowLabelHeader finding, nothing reported (%s)", cs[[1]], stream), TRUE, mh_clean(d))
    })
  }
  # Compared after surrounding spaces and double quotes are removed, on both sides.
  trimmed <- list(
    list("a header text of spaces only is an empty field", "\tS1\nr1\t1\n", "tsv", "   "),
    list("spaces around the declared text", "Gene\tS1\nr1\t1\n", "tsv", "  Gene "),
    list("spaces around the header field", " Gene \tS1\nr1\t1\n", "tsv", "Gene"),
    list("double quotes around the header field", "\"Gene\",S1\nr1,1\n", "csv", "Gene")
  )
  for (cs in trimmed) {
    mx_each(mx_ds(row_header = cs[[4]], ext = cs[[3]]), mx_path(cs[[2]], paste0("m.", cs[[3]])), function(d, stream) {
      qa_step(sprintf("%s: nothing reported (%s)", cs[[1]], stream), TRUE, mh_clean(d))
    })
  }
})

test_that("OQ-MATRIX-009 | a header field that differs from the row label's header text is one rowLabelHeader finding on that row label | REQ-MATRIX-011", {
  # Header text "", wrong text in the one field above the row label.
  mx_each(mx_ds(), mx_path("wrong\tS1\nr1\t1\n"), function(d, stream) {
    qa_step(
      sprintf("one finding, no row, on the row label (%s)", stream),
      mx_expect("columnspec", NA, "ID", "rowLabelHeader"), mx_found(d)
    )
  })
  # header: No leaves the first row label without a field: the one field
  # above the row labels belongs to the second row label.
  mx_each(mx_ds(row_header = list(FALSE, "desc"), k = 2L), mx_path("other\tS1\nr1\tg\t1\n"), function(d, stream) {
    qa_step(
      sprintf("with header: No on the first the finding is on the second row label (%s)", stream),
      mx_expect("columnspec", NA, "DESC", "rowLabelHeader"), mx_found(d)
    )
  })
  # Two wrong fields, two findings, one per field.
  mx_each(mx_ds(row_header = list("a", "b"), k = 2L), mx_path("x\ty\tS1\nr1\tg\t1\n"), function(d, stream) {
    qa_step(
      sprintf("two wrong fields are two findings (%s)", stream),
      mx_expect("columnspec", c(NA, NA), c("ID", "DESC"), c("rowLabelHeader", "rowLabelHeader")), mx_found(d)
    )
  })
})

test_that("OQ-MATRIX-056 | the header texts named and blank are ordinary texts, expected exactly as written | REQ-MATRIX-058", {
  env <- environment()
  for (word in c("named", "blank")) {
    ds <- mx_ds(row_header = word)
    path <- function(first) mx_path(paste0(first, "\tS1\nr1\t1\n"), env = env)
    mx_each(ds, path(word), function(d, stream) {
      qa_step(sprintf("header text %s above the row label is accepted (%s)", word, stream), TRUE, mh_clean(d))
    })
    mx_each(ds, path(""), function(d, stream) {
      qa_step(
        sprintf("an empty field where %s is declared is a rowLabelHeader finding (%s)", word, stream),
        mx_expect("columnspec", NA, "ID", "rowLabelHeader"), mx_found(d)
      )
    })
    mx_each(ds, path("ID"), function(d, stream) {
      qa_step(
        sprintf("the row label's id where %s is declared is a rowLabelHeader finding (%s)", word, stream),
        mx_expect("columnspec", NA, "ID", "rowLabelHeader"), mx_found(d)
      )
    })
  }
  # The message form of the requirement sentence.
  d <- mx_checked(mx_ds(row_header = list("gene_id")), mx_path("Gene\tS1\nr1\t1\n"), "never")
  qa_step(
    "the finding names the position, the row label, the text found and the text expected",
    "Header field 1, above row label ID, is \"Gene\"; expected \"gene_id\".",
    as.data.frame(messages(d))$message
  )
})

# ---- labels -----------------------------------------------------------------------

test_that("OQ-MATRIX-010 | a data label failing the header pattern, value list or length is a propertyNames finding at the label | REQ-MATRIX-012", {
  data_row <- "r1\t1\t2\t3\n"

  mx_each(mx_ds(columns = list(pattern = "^S[0-9]+$")), mx_path(paste0("\tS1\tbad\tS3\n", data_row)), function(d, stream) {
    qa_step(
      sprintf("pattern: the label that does not match (%s)", stream),
      mx_expect("columnspec", NA, "bad", "propertyNames"), mx_found(d)
    )
  })
  mx_each(mx_ds(columns = list(values = list("S1", "S2"))), mx_path(paste0("\tS1\tS2\tS3\n", data_row)), function(d, stream) {
    qa_step(
      sprintf("values: the label outside the list (%s)", stream),
      mx_expect("columnspec", NA, "S3", "propertyNames"), mx_found(d)
    )
  })
  mx_each(mx_ds(columns = list(length = 3)), mx_path(paste0("\tS1\tS2\tS1234\n", data_row)), function(d, stream) {
    qa_step(
      sprintf("length: the label longer than 3 (%s)", stream),
      mx_expect("columnspec", NA, "S1234", "propertyNames"), mx_found(d)
    )
  })
  # Findings come in header order, one per failing label.
  mx_each(mx_ds(columns = list(pattern = "^S[0-9]+$")), mx_path(paste0("\tbad1\tS2\tbad3\n", data_row)), function(d, stream) {
    qa_step(
      sprintf("two failing labels are two findings (%s)", stream),
      mx_expect("columnspec", c(NA, NA), c("bad1", "bad3"), c("propertyNames", "propertyNames")),
      mx_found(d)
    )
  })
})

test_that("OQ-MATRIX-011 | an empty data label is a propertyNames finding on <column p>, and its cells are reported under that name | REQ-MATRIX-013", {
  ds <- mx_ds(cells = list(type = "SAS Int", nullable = FALSE, range = list(min = 0)))
  # The row label is file column 1, S1 column 2, the empty label column 3; the
  # -5 sits under the empty label.
  path <- mx_path("\tS1\t\tS3\nr1\t1\t-5\t3\n")
  mx_each(ds, path, function(d, stream) {
    qa_step(
      sprintf("the label and its cell are both on <column 3> (%s)", stream),
      mx_expect(
        c("columnspec", "columnspec"), c(NA, 1), c("<column 3>", "<column 3>"),
        c("propertyNames", "minimum")
      ),
      mx_found(d)
    )
  })
})

test_that("OQ-MATRIX-012 | a repeated label or one equal to a row-label id is a uniqueNames finding, and its cells are still validated under <column p> | REQ-MATRIX-014", {
  ds <- mx_ds(cells = list(type = "SAS Int", nullable = FALSE, range = list(min = 0)))
  # S1 is at file columns 2 and 3; the -2 sits in the second.
  mx_each(ds, mx_path("\tS1\tS1\tS3\nr1\t1\t-2\t3\n"), function(d, stream) {
    f <- mx_found(d)
    qa_step(
      sprintf("one uniqueNames finding for the label, the cell finding on <column 3> (%s)", stream),
      mx_expect(
        c("columnspec", "columnspec"), c(NA, 1), c("S1", "<column 3>"),
        c("uniqueNames", "minimum")
      ),
      f
    )
    msg <- as.data.frame(messages(d))$message[as.data.frame(messages(d))$keyword == "uniqueNames"]
    qa_step(
      sprintf("the finding lists the reading names of both columns (%s)", stream),
      c(TRUE, TRUE), c(grepl("<column 2>", msg, fixed = TRUE), grepl("<column 3>", msg, fixed = TRUE))
    )
    qa_step(
      sprintf("the message says how many times the label appears (%s)", stream),
      TRUE, grepl("appears 2 times", msg, fixed = TRUE)
    )
  })
  # A label repeated three times is still one finding.
  mx_each(ds, mx_path("\tS1\tS1\tS1\nr1\t1\t2\t3\n"), function(d, stream) {
    qa_step(
      sprintf("three occurrences are one finding (%s)", stream),
      mx_expect("columnspec", NA, "S1", "uniqueNames"), mx_found(d)
    )
  })
  # A data label equal to a row-label id.
  mx_each(ds, mx_path("\tID\tS2\nr1\t1\t2\n"), function(d, stream) {
    f <- mx_found(d)
    qa_step(
      sprintf("a label equal to the row-label id is one uniqueNames finding (%s)", stream),
      c(1L, "uniqueNames"), c(nrow(f), f$keyword)
    )
    qa_step(
      sprintf("with no row (%s)", stream),
      TRUE, is.na(f$row)
    )
    msg <- as.data.frame(messages(d))$message[as.data.frame(messages(d))$keyword == "uniqueNames"]
    qa_step(
      sprintf("the message says the label is also the ID of a row label, not that it appears more than once (%s)", stream),
      c(TRUE, FALSE), c(grepl("is also the ID of a row label", msg, fixed = TRUE), grepl("appears", msg, fixed = TRUE))
    )
  })
})

test_that("OQ-MATRIX-013 | the table's column names are the row-label ids, then each usable label, else <column p>, always unique | REQ-MATRIX-015", {
  cases <- list(
    list(
      "braces and spaces are legal labels", mx_ds(), "\ta{b}\tS 1\nr1\t1\t2\n",
      c("ID", "a{b}", "S 1")
    ),
    list(
      "an empty label is generated", mx_ds(), "\tS1\t\tS3\nr1\t1\t2\t3\n",
      c("ID", "S1", "<column 3>", "S3")
    ),
    list(
      "both of two equal labels are generated", mx_ds(), "\tS1\tS1\tS3\nr1\t1\t2\t3\n",
      c("ID", "<column 2>", "<column 3>", "S3")
    ),
    list(
      "a label equal to a row-label id is generated", mx_ds(), "\tID\tS2\nr1\t1\t2\n",
      c("ID", "<column 2>", "S2")
    ),
    list(
      "a label that is the text of a generated name is replaced", mx_ds(), "\t<column 3>\t\nr1\t1\t2\n",
      c("ID", "<column 2>", "<column 3>")
    ),
    list(
      "with two row labels the positions count them", mx_ds(row_header = list(FALSE, "d"), k = 2L),
      "d\t\tS2\nr1\tg\t1\t2\n",
      c("ID", "DESC", "<column 3>", "S2")
    )
  )
  for (cs in cases) {
    mx_each(cs[[2]], mx_path(cs[[3]]), function(d, stream) {
      qa_step(sprintf("%s (%s)", cs[[1]], stream), cs[[4]], mh_names(d))
      qa_step(sprintf("%s: the names are unique (%s)", cs[[1]], stream), 0L, anyDuplicated(mh_names(d)))
    })
  }
})

test_that("OQ-MATRIX-014 | the number of data columns outside matrix.columns.count is minProperties or maxProperties, with no row and no column | REQ-MATRIX-016", {
  path <- mx_path("\tS1\tS2\tS3\nr1\t1\t2\t3\n")
  no_cell <- function(keyword) mx_expect("columnspec", NA, NA, keyword)

  for (case in list(
    list("min 5 of 3 columns", list(count = list(min = 5)), no_cell("minProperties")),
    list("max 2 of 3 columns", list(count = list(max = 2)), no_cell("maxProperties")),
    list("scalar 2 of 3 columns", list(count = 2), no_cell("maxProperties")),
    list("scalar 4 of 3 columns", list(count = 4), no_cell("minProperties")),
    list("scalar 3 of 3 columns", list(count = 3), mx_expect()),
    list("min 3 max 3 of 3 columns", list(count = list(min = 3, max = 3)), mx_expect()),
    list("no count at all", list(), mx_expect())
  )) {
    mx_each(mx_ds(columns = case[[2]]), path, function(d, stream) {
      qa_step(sprintf("%s (%s)", case[[1]], stream), case[[3]], mx_found(d))
    })
  }
})

test_that("OQ-MATRIX-015 | matrix.columns.values with a count of the same size fixes the labels, in any order | REQ-MATRIX-017", {
  ds <- mx_ds(columns = list(values = list("S1", "S2", "S3"), count = 3))
  data_row <- "r1\t1\t2\t3\n"

  mx_each(ds, mx_path(paste0("\tS3\tS1\tS2\n", data_row)), function(d, stream) {
    qa_step(sprintf("the same labels in another order (%s)", stream), TRUE, mh_clean(d))
  })
  mx_each(ds, mx_path("\tS1\tS2\nr1\t1\t2\n"), function(d, stream) {
    qa_step(
      sprintf("fewer labels (%s)", stream),
      mx_expect("columnspec", NA, NA, "minProperties"), mx_found(d)
    )
  })
  mx_each(ds, mx_path(paste0("\tS1\tS2\tS9\n", data_row)), function(d, stream) {
    qa_step(
      sprintf("a label that is not listed (%s)", stream),
      mx_expect("columnspec", NA, "S9", "propertyNames"), mx_found(d)
    )
  })
  mx_each(ds, mx_path(paste0("\tS1\tS1\tS2\n", data_row)), function(d, stream) {
    qa_step(
      sprintf("a listed label repeated (%s)", stream),
      mx_expect("columnspec", NA, "S1", "uniqueNames"), mx_found(d)
    )
  })
})
