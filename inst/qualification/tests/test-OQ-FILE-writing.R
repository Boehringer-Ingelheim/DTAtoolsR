# write_table_to_file(): its parameters, compression, the MD5 sidecar, the
# overwrite guard, and the write/read round trip. REQ-FILE-025 .. REQ-FILE-029.

# ---- fixtures ---------------------------------------------------------------

# A table with a real missing value, which create_example_DTADataSetTabular()
# does not have and the na = "." case needs.
fw_dataset_with_na <- function() {
  tbl <- arrow::arrow_table(data.frame(
    ID = c("A", "B"), VAL = c(1, NA), stringsAsFactors = FALSE
  ))
  DTADataSetTabular(
    name = "na_demo", specs = create_example_DTAColumnSpecCollection(1),
    tables = list(t = tbl)
  )
}

fw_specs <- function() {
  DTAColumnSpecCollection(columns = list(
    ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE),
    AGE = DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
  ))
}

# ---- the full parameter signature --------------------------------------------------

test_that("OQ-FILE-040 | write_table_to_file honours sep, na, row.names, quote and arrange_by | REQ-FILE-025", {
  dir <- qa_tempdir()
  ds <- create_example_DTADataSetTabular(2) # tab1: STUDYID, SUBJID, VISIT, AGE; 3 rows

  p_sep <- file.path(dir, "sep.csv")
  write_table_to_file(ds, table = "tab1", filename = p_sep, sep = "|", arrange_by = NULL, quiet = TRUE)
  qa_step(
    "sep is used as the field separator, header and data alike",
    c(
      "STUDYID|SUBJID|VISIT|AGE",
      "STUDY001|001|SCREENING|25",
      "STUDY001|002|BASELINE|34",
      "STUDY001|003|WEEK_4|29"
    ),
    readLines(p_sep)
  )

  p_na <- file.path(dir, "na.csv")
  write_table_to_file(fw_dataset_with_na(), table = "t", filename = p_na, sep = ",", na = ".", arrange_by = NULL, quiet = TRUE)
  qa_step(
    "na replaces a missing value in the written text; a real value is untouched",
    c("ID,VAL", "A,1", "B,."),
    readLines(p_na)
  )

  p_rn <- file.path(dir, "rownames.csv")
  write_table_to_file(ds, table = "tab1", filename = p_rn, sep = ",", row.names = TRUE, arrange_by = NULL, quiet = TRUE)
  # utils::write.table() (called here directly, not write.csv()) does not
  # insert a blank leading header cell for the row-name column, so the header
  # ends up with one fewer field than every data row -- a well-known base-R
  # quirk, not a DTAtools one, but part of what "row.names = TRUE" actually
  # produces through this function and therefore worth pinning exactly.
  qa_step(
    "row.names = TRUE prepends a row-number column to the data but not to the header",
    c("STUDYID,SUBJID,VISIT,AGE", "1,STUDY001,001,SCREENING,25"),
    readLines(p_rn)[1:2]
  )

  p_q <- file.path(dir, "quoted.csv")
  write_table_to_file(ds, table = "tab1", filename = p_q, sep = ",", quote = TRUE, arrange_by = NULL, quiet = TRUE)
  qa_step(
    "quote = TRUE quotes character fields; a numeric column (AGE) is left unquoted",
    c("\"STUDYID\",\"SUBJID\",\"VISIT\",\"AGE\"", "\"STUDY001\",\"001\",\"SCREENING\",25"),
    readLines(p_q)[1:2]
  )

  p_a <- file.path(dir, "arranged.csv")
  res_a <- write_table_to_file(ds, table = "tab1", filename = p_a, sep = ",", arrange_by = "SUBJID", arrange_desc = TRUE, quiet = TRUE)
  qa_step("arrange_by + arrange_desc reorders the written table before it is written", c("003", "002", "001"), res_a$tables$SUBJID)
})

# ---- compression --------------------------------------------------------------------

test_that("OQ-FILE-041 | compression = 'gzip' writes a file the package's own reader round-trips | REQ-FILE-026", {
  dir <- qa_tempdir()
  ds <- create_example_DTADataSetTabular(2)
  p_gz <- file.path(dir, "out.csv.gz")

  write_table_to_file(ds, table = "tab1", filename = p_gz, sep = ",", arrange_by = NULL, compression = "gzip", quiet = TRUE)

  # A ".gz"-named handler matches this file by the compression-suffix rule
  # (REQ-FILE-009), and reading it back should reproduce the same text the
  # uncompressed writer would have produced. Read with a (column-less) specs
  # object so every value comes back as the text it is in the file: a bare
  # read_file() would otherwise let Arrow infer SUBJID's "001"/"002"/"003" as
  # the integers 1/2/3, which is a fact about inference (already covered
  # elsewhere) and not about what this requirement -- compression fidelity --
  # is asking.
  text_specs <- DTAColumnSpecCollection(columns = list())
  t <- read_file(DTAFileCSV(filename = "out.csv.gz"), p_gz, specs = text_specs)
  qa_step(
    "the decompressed content matches what compression = 'none' would have written",
    data.frame(
      STUDYID = rep("STUDY001", 3), SUBJID = c("001", "002", "003"),
      VISIT = c("SCREENING", "BASELINE", "WEEK_4"), AGE = c("25", "34", "29"),
      stringsAsFactors = FALSE
    ),
    as.data.frame(t)
  )
})

# ---- the MD5 sidecar ------------------------------------------------------------------

test_that("OQ-FILE-042 | get_md5sum matches tools::md5sum, and the sidecar records it with the row and column counts | REQ-FILE-027", {
  dir <- qa_tempdir()
  ds <- create_example_DTADataSetTabular(2)
  p <- file.path(dir, "out.csv")

  res <- write_table_to_file(ds, table = "tab1", filename = p, sep = ",", arrange_by = NULL, quiet = TRUE)

  qa_step("the computed checksum equals tools::md5sum() of the written file", unname(tools::md5sum(p)), res$md5sum$md5sum)
  qa_step("the recorded column count matches what was written", 4L, res$md5sum$n_cols)
  qa_step("the recorded row count matches what was written", 3L, res$md5sum$n_rows)

  qa_step(
    "the sidecar file holds exactly the checksum, column count and row count, one per line",
    c(
      paste0("md5sum: ", res$md5sum$md5sum),
      "Number of Columns: 4",
      "Number of Rows: 3"
    ),
    readLines(paste0(p, ".md5"))
  )
})

test_that("OQ-FILE-043 | the returned md5sum is a list, not the bare checksum string, when get_md5sum is TRUE | REQ-FILE-027", {
  dir <- qa_tempdir()
  ds <- create_example_DTADataSetTabular(2)

  p1 <- file.path(dir, "with_md5.csv")
  res1 <- write_table_to_file(ds, table = "tab1", filename = p1, sep = ",", arrange_by = NULL, get_md5sum = TRUE, quiet = TRUE)
  qa_step("get_md5sum = TRUE: the md5sum element is a list, not a character scalar", "list", class(res1$md5sum))
  qa_step("...carrying the checksum under its own md5sum name", unname(tools::md5sum(p1)), res1$md5sum$md5sum)

  # The practical trap this creates: the function's own documentation
  # describes md5sum as "the checksum, or NA when get_md5sum = FALSE" -- which
  # reads as a bare, comparable value. A caller following that literally hits
  # R's own "condition has length > 1" instead of the intended branch, because
  # is.na() on a 3-element list yields a 3-element logical. The message is
  # base R's own and therefore locale-dependent, so only the fact that it
  # raises a condition is asserted, not its text.
  trap <- tryCatch(
    {
      if (!is.na(res1$md5sum)) NULL
    },
    error = function(e) e,
    warning = function(w) w
  )
  qa_check("the documented if (!is.na(result$md5sum)) idiom does not run cleanly for get_md5sum = TRUE", inherits(trap, "condition"))
  qa_known_deviation("DEV-007", !is.character(res1$md5sum) && inherits(trap, "condition"))

  p2 <- file.path(dir, "without_md5.csv")
  res2 <- write_table_to_file(ds, table = "tab1", filename = p2, sep = ",", arrange_by = NULL, get_md5sum = FALSE, quiet = TRUE)
  qa_step("get_md5sum = FALSE: md5sum is exactly NA, matching the documentation for this branch", NA, res2$md5sum)
  qa_check("...and no sidecar file is written", !file.exists(paste0(p2, ".md5")))
})

# ---- the overwrite guard --------------------------------------------------------------

test_that("OQ-FILE-044 | the overwrite guard blocks an existing file unless overwrite = TRUE | REQ-FILE-028", {
  dir <- qa_tempdir()
  ds <- create_example_DTADataSetTabular(2)
  p <- file.path(dir, "guarded.csv")

  write_table_to_file(ds, table = "tab1", filename = p, sep = ",", arrange_by = NULL, quiet = TRUE)
  first_write_time <- file.info(p)$mtime

  err <- tryCatch(
    write_table_to_file(ds, table = "tab1", filename = p, sep = ",", arrange_by = NULL, quiet = TRUE),
    error = function(e) e
  )
  qa_check("a second write without overwrite = TRUE aborts", inherits(err, "condition"))
  qa_step(
    "the message says the file already exists and names the escape hatch",
    TRUE,
    grepl("already exists", conditionMessage(err), fixed = TRUE) &&
      grepl("overwrite = TRUE", conditionMessage(err), fixed = TRUE)
  )
  qa_step("the original file is untouched by the aborted attempt", first_write_time, file.info(p)$mtime)

  res_ow <- write_table_to_file(ds, table = "tab1", filename = p, sep = ",", arrange_by = NULL, overwrite = TRUE, quiet = TRUE)
  qa_check("overwrite = TRUE replaces the file without error", file.exists(p) && nrow(res_ow$tables) == 3L)
})

# ---- the write/read round trip ---------------------------------------------------------

test_that("OQ-FILE-045 | a written table read back reaches the identical validation verdict | REQ-FILE-029", {
  dir <- qa_tempdir()
  specs <- fw_specs()

  p_src <- file.path(dir, "src.csv")
  writeLines(c("ID,AGE", "A0000001,30", "A0000002,40"), p_src)
  ds1 <- DTADataSetTabular(name = "s1", specs = specs, files = list(DTAFileCSV(filename = "src.csv")))
  ds1 <- load_file(ds1, file = p_src, handler_index = 1, stream = "never")
  ds1 <- check(ds1, persist = FALSE, quiet = TRUE)
  before <- validation_status(ds1)

  p_out <- file.path(dir, "roundtrip.csv")
  write_table_to_file(ds1, table = names(tables(ds1))[[1]], filename = p_out, sep = ",", arrange_by = NULL, quiet = TRUE)

  ds2 <- DTADataSetTabular(name = "s2", specs = specs, files = list(DTAFileCSV(filename = "roundtrip.csv")))
  ds2 <- load_file(ds2, file = p_out, handler_index = 1, stream = "never")
  ds2 <- check(ds2, persist = FALSE, quiet = TRUE)
  after <- validation_status(ds2)

  qa_step(
    "the verdict and every axis count survive the write/read round trip unchanged",
    list(ok = before$ok, columnspec = as.integer(before$n_columnspec_errors), rule = as.integer(before$n_rule_errors), import = as.integer(before$n_import_errors)),
    list(ok = after$ok, columnspec = as.integer(after$n_columnspec_errors), rule = as.integer(after$n_rule_errors), import = as.integer(after$n_import_errors))
  )
  qa_step("...and the verdict is the one actually expected (a clean pass), not merely a stable one", TRUE, before$ok)
})
