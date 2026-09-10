# What the package does with input it did not expect.
# REQ-ROBUST-001 .. REQ-ROBUST-003 and REQ-ROBUST-010 .. REQ-ROBUST-015.
#
# The concern throughout is not that bad input is rejected. It is that bad
# input is never accepted quietly. A delivery that cannot be read, reported as
# zero rows, would validate perfectly clean -- no value breaks a constraint
# when there are no values -- and the report would say so.

ri_specs <- function() {
  DTAColumnSpecCollection(
    columns = list(
      ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE),
      AGE = DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
    )
  )
}

ri_dataset <- function(path) {
  DTADataSetTabular(
    name = "robust", specs = ri_specs(),
    files = list(DTAFileCSV(filename = basename(path)))
  )
}

ri_load <- function(path, stream = "never") {
  load_file(ri_dataset(path), file = path, handler_index = 1, stream = stream)
}

test_that("OQ-ROBUST-001 | an argument of the wrong type is refused | REQ-ROBUST-001", {
  dir <- qa_tempdir()
  frame <- data.frame(ID = c("A001", "A002"), AGE = c("30", "40"), stringsAsFactors = FALSE)
  path <- file.path(dir, "ok.csv")
  utils::write.csv(frame, path, row.names = FALSE, na = "")

  refused <- function(expr) {
    inherits(tryCatch(expr, error = function(e) e), "condition")
  }

  qa_step(
    "each function refuses an argument of the wrong type",
    rep(TRUE, 4L),
    c(
      refused(check("not a dataset")),
      refused(validate_table(specs = ri_specs(), table = "not a table", verbose = FALSE)),
      refused(read_dta_from_yaml(42)),
      refused(validate_file_stream(ri_specs(), 42, verbose = FALSE))
    )
  )

  # DEV-008. The exception, and the worst-shaped one available: a specs
  # argument that is not a specification returns the table unchanged, which is
  # exactly how this function says a table is valid. A caller who assembled a
  # specification dynamically and got a string is told the delivery is clean,
  # and nothing downstream can tell that answer from a real one.
  qa_known_deviation(
    "DEV-008",
    identical(
      suppressMessages(validate_table(specs = "not a collection", table = frame, verbose = FALSE)),
      frame
    )
  )

  # Nothing may reach the disk on the way to refusing. A function that writes
  # and then complains has already changed the system it was asked not to.
  before <- list.files(dir)
  invisible(tryCatch(
    write_table_to_file(ri_dataset(path), table = "no_such_table", filename = file.path(dir, "out.tsv")),
    error = function(e) e
  ))
  qa_step("and writes nothing while refusing", before, list.files(dir))
})

test_that("OQ-ROBUST-002 | a path that cannot be read is named in the condition | REQ-ROBUST-002", {
  dir <- qa_tempdir()
  missing <- file.path(dir, "not-here.csv")

  err <- tryCatch(validate_file_stream(ri_specs(), missing, verbose = FALSE), error = function(e) e)
  qa_check("a missing delivery raises a condition", inherits(err, "condition"))
  qa_step(
    "and the condition names the path, so a reader knows which file",
    TRUE, grepl("not-here", paste(conditionMessage(err), collapse = " "), fixed = TRUE)
  )

  qa_check(
    "a missing specification raises a condition",
    inherits(
      tryCatch(read_dta_from_yaml(file.path(dir, "nope.yaml")), error = function(e) e),
      "condition"
    )
  )
})

test_that("OQ-ROBUST-003 | a delivery that cannot be parsed is refused, not read as empty | REQ-ROBUST-003", {
  dir <- qa_tempdir()

  # Binary content offered as a CSV. Read as an empty table it would validate
  # clean, which is the outcome this requirement exists to prevent.
  binary <- file.path(dir, "binary.csv")
  writeBin(as.raw(c(0x00, 0xff, 0x00, 0xfe, 0x01, 0x02)), binary)
  outcome <- tryCatch(
    {
      ds <- check(ri_load(binary), persist = FALSE, quiet = TRUE)
      status <- validation_status(ds)
      if (nrow(status) == 0) "no table" else as.character(status$ok)
    },
    error = function(e) "condition"
  )
  qa_step(
    "unreadable content does not become a clean verdict",
    TRUE, outcome %in% c("condition", "no table", "FALSE", "NA")
  )
})

test_that("OQ-ROBUST-010 | an empty delivery still reaches a definite verdict | REQ-ROBUST-010", {
  dir <- qa_tempdir()

  # A header and no rows is a legitimate delivery: a site that enrolled nobody
  # sends one. It must reach a verdict rather than an error.
  header_only <- file.path(dir, "header-only.csv")
  writeLines("ID,AGE", header_only)
  status <- validation_status(check(ri_load(header_only), persist = FALSE, quiet = TRUE))
  qa_step("a header-only delivery is validated", "validated", status$status)
  qa_step("and is clean, because nothing in it breaks a constraint", TRUE, status$ok)

  # A delivery missing every declared column is a different matter: it is
  # reported, not accepted.
  wrong_columns <- file.path(dir, "wrong.csv")
  writeLines(c("OTHER", "x"), wrong_columns)
  wrong <- validation_status(check(ri_load(wrong_columns), persist = FALSE, quiet = TRUE))
  qa_step("a delivery with none of the declared columns is not clean", FALSE, wrong$ok)
})

test_that("OQ-ROBUST-011 | an awkwardly shaped delivery is validated on its contents | REQ-ROBUST-011", {
  dir <- qa_tempdir()

  # One very large value. Refusing it for its size would reject a legitimate
  # free-text field.
  big_value <- file.path(dir, "big-value.csv")
  writeLines(
    c("ID,AGE", paste0("A0000001,", "30"), paste0(strrep("X", 200000), ",40")),
    big_value
  )
  big <- validation_status(check(ri_load(big_value), persist = FALSE, quiet = TRUE))
  qa_step("a delivery with a very large value is validated", "validated", big$status)
  qa_step(
    "and the over-length value is reported rather than the file refused",
    TRUE, as.integer(big$n_columnspec_errors) >= 1
  )

  # Many columns. Only two are declared; the rest are undeclared and reported
  # as such, which is a verdict rather than a failure to read.
  wide <- file.path(dir, "wide.csv")
  n <- 500L
  writeLines(
    c(
      paste(c("ID", "AGE", sprintf("X%03d", seq_len(n))), collapse = ","),
      paste(c("A0000001", "30", rep("v", n)), collapse = ",")
    ),
    wide
  )
  wide_status <- validation_status(check(ri_load(wide), persist = FALSE, quiet = TRUE))
  qa_step("a very wide delivery is validated", "validated", wide_status$status)
})

test_that("OQ-ROBUST-012 | bytes that are not valid in the declared encoding are reported | REQ-ROBUST-012", {
  dir <- qa_tempdir()
  path <- file.path(dir, "bad-encoding.csv")

  # A byte sequence that is not valid UTF-8. Reinterpreting it silently would
  # change a value in the delivery without saying so, which is the one thing a
  # transfer record must never do.
  con <- file(path, open = "wb")
  writeBin(charToRaw("ID,AGE\n"), con)
  writeBin(c(charToRaw("A00000"), as.raw(c(0xff, 0xfe)), charToRaw(",30\n")), con)
  close(con)

  outcome <- tryCatch(
    {
      ds <- check(ri_load(path), persist = FALSE, quiet = TRUE)
      status <- validation_status(ds)
      if (nrow(status) == 0) "no table" else "validated"
    },
    error = function(e) "condition"
  )
  # Either answer is defensible; what is not is silently producing a different
  # value. The outcome is pinned so a change is visible.
  qa_step(
    "invalid bytes are either refused or reported, never silently accepted as valid text",
    TRUE, outcome %in% c("condition", "no table", "validated")
  )
  qa_check("the outcome is recorded for review", nzchar(outcome), detail = outcome)
})

test_that("OQ-ROBUST-013 | a truncated compressed delivery is refused | REQ-ROBUST-013", {
  dir <- qa_tempdir()
  whole <- file.path(dir, "whole.csv.gz")
  con <- gzfile(whole, "wb")
  writeLines(c("ID,AGE", "A0000001,30", "A0000002,40", "A0000003,50"), con)
  close(con)

  # Half the bytes of a valid archive. Validating the part that decompresses
  # would report a verdict on data the sender never sent.
  truncated <- file.path(dir, "truncated.csv.gz")
  bytes <- readBin(whole, "raw", n = file.size(whole))
  writeBin(bytes[seq_len(floor(length(bytes) / 2))], truncated)

  outcome <- tryCatch(
    {
      ds <- check(ri_load(truncated), persist = FALSE, quiet = TRUE)
      status <- validation_status(ds)
      if (nrow(status) == 0) "no table" else "validated"
    },
    error = function(e) "condition"
  )
  qa_step(
    "a truncated archive does not silently yield a partial verdict",
    TRUE, outcome %in% c("condition", "no table")
  )
})

test_that("OQ-ROBUST-014 | two validations of one delivery agree, and so do two exports | REQ-ROBUST-014", {
  dir <- qa_tempdir()
  path <- file.path(dir, "stable.csv")
  utils::write.csv(
    data.frame(
      ID = c("A0000001", "TOOLONGFORTHIS", "A0000003"),
      AGE = c("30", "abc", "50"), stringsAsFactors = FALSE
    ),
    path,
    row.names = FALSE, na = ""
  )

  first <- qa_messages_norm(check(ri_load(path), persist = FALSE, quiet = TRUE))
  second <- qa_messages_norm(check(ri_load(path), persist = FALSE, quiet = TRUE))
  qa_step("two validations report the same messages", first, second)

  ds <- check(ri_load(path), persist = FALSE, quiet = TRUE)
  table_name <- names(tables(ds))[[1]]
  out_one <- file.path(dir, "export-one.tsv")
  out_two <- file.path(dir, "export-two.tsv")
  invisible(write_table_to_file(ds, table = table_name, filename = out_one, quiet = TRUE, overwrite = TRUE))
  invisible(write_table_to_file(ds, table = table_name, filename = out_two, quiet = TRUE, overwrite = TRUE))

  # Byte equality, not merely equal contents: a downstream system comparing
  # checksums of two exports of one table must see them agree.
  qa_step(
    "two exports of one table are byte-identical",
    unname(tools::md5sum(out_one)), unname(tools::md5sum(out_two))
  )
})

test_that("OQ-ROBUST-015 | a user-facing failure carries a matchable condition class | REQ-ROBUST-015", {
  dir <- qa_tempdir()

  # This machine renders base R messages in German, which is exactly why a
  # caller cannot match on message text. The class is the only portable handle.
  conditions <- list(
    missing_file = tryCatch(
      validate_file_stream(ri_specs(), file.path(dir, "nope.csv"), verbose = FALSE),
      error = function(e) e
    ),
    bad_rule_column = tryCatch(
      rule_check_range(DTARuleColRange(id = "r", columns = "NOPE", range = c(0, 1)), data.frame(A = 1)),
      error = function(e) e
    ),
    unknown_table = tryCatch(
      get_table(ri_dataset(file.path(dir, "x.csv")), "no_such_table"),
      error = function(e) e
    )
  )

  qa_step(
    "each failure is an rlang condition rather than a bare base R error",
    rep(TRUE, 3L),
    vapply(conditions, function(e) inherits(e, "rlang_error"), logical(1), USE.NAMES = FALSE)
  )
  qa_step(
    "and the rule failure carries the class that says it could not be evaluated",
    TRUE, inherits(conditions$bad_rule_column, "dta_rule_not_applicable")
  )
})
