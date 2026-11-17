# Column ids and matrix row-label ids. REQ-SPEC-033 .. REQ-SPEC-035.
#
# A column id is the exact text of a header, so it may hold spaces and
# punctuation ("Subject ID", "Dose mg/kg"); a matrix row-label id may not,
# because its exact header text has its own key. Every expected value below is
# written from the requirement text and a fixture built in the case: which ids
# are accepted, which are refused and under which condition class, and what a
# delivery with such a header must report. None of it is a recording of a run.
#
# A frame with a spaced column name is always built with check.names = FALSE;
# without it R renames "Subject ID" to "Subject.ID" and the case would test a
# header the specification never declared.

si_spec <- function(id) DTAColumnSpec(id = id, type = "SAS Char")

# TRUE when constructing a column specification with this id is refused with
# the condition class the requirement names.
si_refused <- function(id) {
  inherits(tryCatch(si_spec(id), error = function(e) e), "dta_spec_invalid_id")
}

# Write `lines` as UTF-8 bytes, so a header holding a no-break space reaches
# the reader as exactly those bytes whatever the platform's native encoding.
si_write_lines <- function(lines, path) {
  writeBin(charToRaw(enc2utf8(paste0(paste(lines, collapse = "\n"), "\n"))), path)
  path
}

test_that("OQ-SPEC-059 | a column id may hold spaces between characters and is carried exactly; an edge space, a tab or a line break is refused | REQ-SPEC-033", {
  # REQ-SPEC-033, first sentence: spaces between characters, including two in
  # a row, and every id that has no whitespace at all.
  accepted <- c(
    "Subject ID", "Visit Date (UTC)", "Dose mg/kg", "Subject  ID", "%Change",
    "{id}", "Length 5\"", "O'Brien", "Wt. (kg)", "SUBJID"
  )
  qa_step(
    "each id is carried unchanged by the column specification",
    accepted,
    vapply(accepted, function(id) si_spec(id)@id, character(1), USE.NAMES = FALSE)
  )

  coll <- DTAColumnSpecCollection(columns = stats::setNames(lapply(accepted, si_spec), accepted))
  qa_step("a collection holds each under its exact id", accepted, names(coll@columns))

  # REQ-SPEC-033: carried through write_columns_to_yaml() and
  # import_specs_from_yaml(). Ids YAML would misread unquoted (%Change, {id},
  # " #") are the ones a writer can get wrong.
  path <- file.path(qa_tempdir(), "spaced_ids.yaml")
  write_columns_to_yaml(coll, file = path)
  back <- import_specs_from_yaml(path)
  qa_step("written to YAML and read back, the column keys are the same", accepted, names(back@columns))
  qa_step(
    "and so are the ids the columns carry",
    accepted,
    vapply(back@columns, function(column) column@id, character(1), USE.NAMES = FALSE)
  )

  # REQ-SPEC-033, second sentence: refused, each with the named class.
  refused <- list(
    " Subject ID", "Subject ID ", " ", "\tSubject", "Subject\tID", "Subject\nID",
    "Subject\rID", "Subject\vID", "Subject\fID", "Subject ID\n", "", NA_character_, c("A", "B")
  )
  for (id in refused) {
    qa_check(
      sprintf("the id %s is refused with class dta_spec_invalid_id", paste(encodeString(id, quote = "\""), collapse = " + ")),
      si_refused(id)
    )
  }
})

test_that("OQ-SPEC-060 | a header is compared with the declared column ids exactly: Subject ID checks clean in memory and streamed, and a different spelling is one missing and one undeclared column | REQ-SPEC-034", {
  dir <- qa_tempdir()
  specs <- DTAColumnSpecCollection(columns = list(
    `Subject ID` = DTAColumnSpec(id = "Subject ID", type = "SAS Char", length = 8, nullable = FALSE),
    `Visit Date (UTC)` = DTAColumnSpec(id = "Visit Date (UTC)", type = "SAS Char", length = 10, nullable = FALSE)
  ))
  checked <- function(path, stream) {
    check(qa_dta(path, specs, stream = stream), persist = FALSE, quiet = TRUE)
  }

  frame <- data.frame(
    `Subject ID` = c("A1", "A2"), `Visit Date (UTC)` = c("2026-01-01", "2026-01-02"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  clean <- qa_write_csv(frame, file.path(dir, "clean.csv"))
  # Second subject id is 10 characters; the column declares length 8, so that
  # one cell is the only thing wrong with the file.
  long <- frame
  long$`Subject ID`[2] <- "A123456789"
  too_long <- qa_write_csv(long, file.path(dir, "long.csv"))

  for (mode in c("never", "always")) {
    ok <- validation_status(checked(clean, mode))
    qa_step(
      sprintf("stream = \"%s\": a file headed Subject ID and Visit Date (UTC) is valid, with no error on any axis", mode),
      list(ok = TRUE, columnspec = 0L, import = 0L),
      list(
        ok = ok$ok, columnspec = as.integer(ok$n_columnspec_errors),
        import = as.integer(ok$n_import_errors)
      )
    )

    found <- as.data.frame(messages(checked(too_long, mode)))
    qa_step(
      sprintf("stream = \"%s\": the one over-long cell is one finding, on the column named Subject ID", mode),
      list(n = 1L, column = "Subject ID"),
      list(n = nrow(found), column = as.character(found$column))
    )
  }

  # Three spellings that differ from the declared "Subject ID": one more space,
  # another letter case, and a no-break space (U+00A0) in place of the space.
  other <- c(
    two_spaces = "Subject  ID", lower_case = "subject id",
    no_break_space = paste0("Subject", intToUtf8(160L), "ID")
  )
  for (kind in names(other)) {
    header <- other[[kind]]
    path <- si_write_lines(
      c(paste(c(header, "Visit Date (UTC)"), collapse = ","), "A1,2026-01-01"),
      file.path(dir, paste0(kind, ".csv"))
    )
    for (mode in c("never", "always")) {
      d <- checked(path, mode)
      status <- validation_status(d)
      qa_step(
        sprintf("stream = \"%s\", %s: not valid, with one missing and one undeclared column-specification error", mode, kind),
        list(ok = FALSE, columnspec = 2L, import = 0L),
        list(
          ok = status$ok, columnspec = as.integer(status$n_columnspec_errors),
          import = as.integer(status$n_import_errors)
        )
      )
      qa_check(
        sprintf("stream = \"%s\", %s: the undeclared column is the header exactly as the file spells it", mode, kind),
        header %in% as.character(messages(d)$column)
      )
    }
  }
})

test_that("OQ-SPEC-061 | a matrix row-label id with a space is refused, built, linted and read from YAML alike, and the exact header text under header is accepted | REQ-SPEC-035", {
  dir <- qa_tempdir()
  label <- list(id = "Sample ID", header = "", type = "SAS Char", nullable = FALSE)

  # As a row-label column, the way every reader builds it.
  qa_check(
    "dta_dataset_from_list() refuses the id with class dta_spec_invalid_id",
    mx_is(mx_error(dta_dataset_from_list(mx_body(row_labels = list(label)))), "dta_spec_invalid_id")
  )

  # As a plain column specification, which DTADataSetMatrix() also accepts as a
  # row label.
  plain <- DTAColumnSpecCollection(columns = list(
    `Sample ID` = DTAColumnSpec(id = "Sample ID", type = "SAS Char", nullable = FALSE)
  ))
  qa_check(
    "DTADataSetMatrix() refuses a plain column specification with that id, with the same class",
    mx_is(
      mx_error(DTADataSetMatrix(
        id = "m", specs = plain, matrix = list(cells = list(type = "SAS Int", nullable = FALSE)),
        files = list(DTAFileTSV(filename = "m.tsv"))
      )),
      "dta_spec_invalid_id"
    )
  )

  # lint_dta() reports the raw entry, since it never builds the dataset.
  found <- lint_dta(mx_body(row_labels = list(label)))
  qa_step(
    "lint_dta() reports exactly one finding, matrix_row_label_id_invalid, at severity error",
    list(code = "matrix_row_label_id_invalid", severity = "error"),
    list(code = found$code, severity = found$severity)
  )

  # Only a character scalar that contains whitespace: an empty, missing or
  # non-string id is column_spec_invalid, and not also this code.
  for (odd in list("", NA_character_, 5, c("A", "B"))) {
    codes <- lint_dta(mx_body(row_labels = list(list(id = odd, header = "", type = "SAS Char", nullable = FALSE))))$code
    qa_step(
      sprintf("a row-label id %s is column_spec_invalid, and not matrix_row_label_id_invalid", paste(deparse(odd), collapse = "")),
      list(has_spec_invalid = TRUE, has_whitespace_code = FALSE),
      list(
        has_spec_invalid = "column_spec_invalid" %in% codes,
        has_whitespace_code = "matrix_row_label_id_invalid" %in% codes
      )
    )
  }

  # So a YAML file carrying it is refused by the reader with the lint error.
  path <- file.path(dir, "spaced_row_label.yaml")
  yaml::write_yaml(
    list(metadata = list(title = "T", version = "1"), datasets = list(mx_body(row_labels = list(label)))),
    path
  )
  got <- tryCatch(read_dta_from_yaml(path), error = function(e) e)
  qa_check("read_dta_from_yaml() aborts with class dta_lint_error", inherits(got, "dta_lint_error"))
  qa_step(
    "and the finding it carries is matrix_row_label_id_invalid",
    "matrix_row_label_id_invalid",
    if (inherits(got, "dta_lint_error")) unique(got$findings$code) else NA_character_
  )

  # The exact header text goes under header; the id has no whitespace.
  accepted <- list(id = "SAMPLE_ID", header = "Sample ID", type = "SAS Char", nullable = FALSE)
  qa_step(
    "id SAMPLE_ID with header \"Sample ID\" lints with no finding",
    0L, nrow(lint_dta(mx_body(row_labels = list(accepted))))
  )
  ds <- dta_dataset_from_list(mx_body(row_labels = list(accepted)))
  path <- mx_path("Sample ID\tS1\tS2\ng1\t1\t2\n")
  for (mode in MX_STREAMS) {
    d <- mx_checked(ds, path, mode)
    qa_check(
      sprintf("stream = \"%s\": a file whose header line starts with Sample ID is valid and reports nothing", mode),
      mh_clean(d)
    )
  }
})
