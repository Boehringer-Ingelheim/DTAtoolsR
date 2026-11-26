# What the package does with input it did not expect.
# REQ-ROBUST-001 .. REQ-ROBUST-003 and REQ-ROBUST-010 .. REQ-ROBUST-018.
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
    id = "robust", specs = ri_specs(),
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

  # The specification argument, in every shape a caller can arrive with by
  # accident: a path where an object was meant, a NULL coerced along, a list
  # still being assembled. This one is called out separately because of what
  # the alternative looks like -- validate_table() signals a clean delivery by
  # returning the table unchanged, so a specification that is silently ignored
  # produces the same answer as one that was met. It was DEV-008 until the
  # guard landed; the assertion is now on the correct behaviour, not the
  # defect.
  qa_step(
    "a specification that is not one is refused rather than ignored",
    rep(TRUE, 3L),
    c(
      refused(validate_table(specs = "not a collection", table = frame, verbose = FALSE)),
      refused(validate_table(specs = NULL, table = frame, verbose = FALSE)),
      refused(validate_table(specs = list(), table = frame, verbose = FALSE))
    )
  )
  qa_step(
    "and the refusal is a package condition naming the argument",
    TRUE,
    grepl(
      "specs",
      conditionMessage(tryCatch(
        validate_table(specs = "not a collection", table = frame, verbose = FALSE),
        error = function(e) e
      )),
      fixed = TRUE
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

# What one delivery comes to when a caller does what the package is for: loads
# it and then checks it. An error condition from either entry point is a
# refusal, and the entry point and the condition's classes are all that is
# recorded of it; otherwise the checked dataset is handed back, for the caller
# to read the verdict and the table from. The one place the refusal is
# recognised, so that every case that speaks of one means the same thing by it.
ri_run <- function(path, stream) {
  loaded <- tryCatch(ri_load(path, stream), error = function(e) e)
  if (inherits(loaded, "error")) {
    return(list(refused_at = "load_file()", classes = class(loaded), checked = NULL))
  }
  checked <- tryCatch(check(loaded, persist = FALSE, quiet = TRUE), error = function(e) e)
  if (inherits(checked, "error")) {
    return(list(refused_at = "check()", classes = class(checked), checked = NULL))
  }
  list(refused_at = NULL, classes = NULL, checked = checked)
}

# The rows the table of a run holds, or NA when the delivery was refused. A table
# held lazily is read in full to count them, and a read that fails there is also
# NA: the count is evidence for a step to compare, and the step is what fails.
ri_run_rows <- function(run) {
  if (is.null(run$checked)) {
    return(NA_integer_)
  }
  tryCatch(
    nrow(as.data.frame(tables(run$checked)[[1]])),
    error = function(e) NA_integer_
  )
}

# What a run looks like in the evidence: the branch it took and the observed
# values behind it, so that a failing step shows what was seen and not only that
# it failed.
ri_run_detail <- function(run) {
  if (!is.null(run$refused_at)) {
    return(sprintf(
      "refused by %s, condition class %s", run$refused_at,
      paste(run$classes, collapse = "/")
    ))
  }
  status <- validation_status(run$checked)
  sprintf(
    "read and checked: status = %s, ok = %s, %d row(s) in the table",
    status$status, status$ok, ri_run_rows(run)
  )
}

# A gzip-compressed CSV of `n_rows` data rows, whole and then cut in half: the
# first half of its bytes, which is what a transfer that stopped part way leaves.
ri_archive <- function(dir, n_rows) {
  whole <- file.path(dir, "whole.csv.gz")
  con <- gzfile(whole, "wb")
  writeLines(
    c("ID,AGE", sprintf("A%07d,%d", seq_len(n_rows), seq_len(n_rows) %% 100L)),
    con
  )
  close(con)

  truncated <- file.path(dir, "truncated.csv.gz")
  bytes <- readBin(whole, "raw", n = file.size(whole))
  writeBin(bytes[seq_len(floor(length(bytes) / 2))], truncated)
  list(whole = whole, truncated = truncated)
}

test_that("OQ-ROBUST-003 | a delivery that cannot be parsed is refused, not read as empty | REQ-ROBUST-003", {
  dir <- qa_tempdir()

  # The contract is the requirement's own sentence: a delivery that is not
  # readable as the declared format "shall raise a condition rather than
  # validate as an empty or partial table". One outcome meets it -- an error
  # condition, from load_file() or from check() -- and the case accepts no
  # other. In particular it does not accept a delivery that was read and then
  # given a poor verdict: a table made of whatever the bytes could be taken for
  # is what the requirement is written against, however the verdict reads.
  #
  # Two deliveries that are not CSV, in the two ways a reader can fail:
  #   binary  six bytes with no header line and no field in them. Nothing in it
  #           is a table; a reader that did not refuse would hold an empty one,
  #           which validates clean because no value breaks a constraint when
  #           there are no values.
  #   ragged  3000 good rows, then a row of three fields under a header that
  #           declares two. A reader that stopped at it would hold the 3000
  #           before it: a partial table, validated as if it were the delivery.
  #           Read in 1 KiB blocks (set below) the streaming reader is far past
  #           its first block by then, so for it the bad row meets the scan that
  #           check() runs rather than the opening of the dataset.
  # Each is held against a CSV that is read: the binary file against a CSV of
  # one row, the ragged one against the same 3001 rows with no row of the wrong
  # width. Were those refused too, a refusal of the deliveries would say nothing
  # about their content, and a reader that read the deliveries would be seen to
  # fail the case.
  withr::local_options(DTAtools.stream_block_size = 1024L)

  binary <- file.path(dir, "binary.csv")
  writeBin(as.raw(c(0x00, 0xff, 0x00, 0xfe, 0x01, 0x02)), binary)
  small <- file.path(dir, "small.csv")
  writeLines(c("ID,AGE", "A0000001,30"), small)

  valid_rows <- sprintf("A%07d,30", seq_len(3000L))
  ragged <- file.path(dir, "ragged.csv")
  writeLines(c("ID,AGE", valid_rows, "A9999999,30,EXTRA"), ragged)
  long <- file.path(dir, "long.csv")
  writeLines(c("ID,AGE", valid_rows, "A9999999,30"), long)

  twins <- list(
    list(what = "a CSV of one row", path = small, rows = 1L),
    list(what = "3001 rows of two fields", path = long, rows = 3001L)
  )
  deliveries <- list(
    list(what = "binary content", path = binary),
    list(what = "3000 rows and then a row of three fields", path = ragged)
  )

  for (stream in c("never", "always")) {
    route <- sprintf("stream = \"%s\"", stream)

    for (twin in twins) {
      run <- ri_run(twin$path, stream)
      qa_check(
        sprintf("%s, %s: read and validated clean", route, twin$what),
        is.null(run$refused_at) && isTRUE(validation_status(run$checked)$ok),
        detail = ri_run_detail(run)
      )
      qa_step(
        sprintf("%s, %s: and every row is in the table", route, twin$what),
        twin$rows, ri_run_rows(run)
      )
    }

    for (delivery in deliveries) {
      run <- ri_run(delivery$path, stream)
      qa_check(
        sprintf("%s, %s: refused with a condition, and given no verdict", route, delivery$what),
        !is.null(run$refused_at),
        detail = ri_run_detail(run)
      )
    }
  }
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

# What load_file() and then check() made of one delivery, reduced to the facts
# REQ-ROBUST-012 speaks about. `row` is the data row of the delivery whose ID
# carries the bytes in question. A refusal is what ri_run() recognises as one,
# and the entry point and the condition's classes are all that is recorded of
# it; otherwise the verdict, whether any message names that row of column ID,
# and the bytes of the ID that was read back.
#
# This case accepts a refusal of any class: REQ-ROBUST-012 asks only that the
# bytes be reported, and the classes are recorded as evidence. That the refusal
# is a condition of one particular package class is REQ-ROBUST-018's to say, and
# OQ-ROBUST-018 asserts it. That a refusal is about the bytes and not about
# something else the case did wrong is shown by ri_bytes_outcome() being run on
# a delivery with valid bytes as well.
ri_bytes_outcome <- function(path, stream, row = 1L) {
  run <- ri_run(path, stream)
  if (!is.null(run$refused_at)) {
    return(list(refused_at = run$refused_at, classes = run$classes))
  }
  checked <- run$checked

  messages_frame <- as.data.frame(messages(checked))
  rows <- as.data.frame(tables(checked)[[1]])
  read_back <- if (nrow(rows) < row || is.na(rows$ID[[row]])) NULL else charToRaw(rows$ID[[row]])
  list(
    refused_at = NULL,
    ok = validation_status(checked)$ok,
    names_row = any(messages_frame$column %in% "ID" & messages_frame$row %in% row),
    id = read_back
  )
}

# What an outcome looks like in the evidence: which branch of the contract it
# took and the observed values behind it, so that a failing step shows what was
# seen and not only that it failed.
ri_outcome_detail <- function(outcome, row) {
  if (!is.null(outcome$refused_at)) {
    return(sprintf(
      "refused by %s, condition class %s", outcome$refused_at,
      paste(outcome$classes, collapse = "/")
    ))
  }
  sprintf(
    "read: ok = %s, a message names row %d of ID = %s, stored ID bytes = %s",
    outcome$ok, row, outcome$names_row,
    if (is.null(outcome$id)) "none" else paste(as.character(outcome$id), collapse = " ")
  )
}

test_that("OQ-ROBUST-012 | bytes that are not valid in the declared encoding are reported | REQ-ROBUST-012", {
  dir <- qa_tempdir()

  # The contract is the requirement's own sentence: bytes that are not valid in
  # the declared encoding "shall be reported rather than silently
  # reinterpreted". Two outcomes meet it, and the case accepts exactly those:
  #   (a) an error condition, from load_file() or from check();
  #   (b) a table that is read, but whose verdict is not ok and has a message
  #       naming the row and column ID, and whose stored ID is the delivered
  #       bytes and nothing else -- no U+FFFD, no dropped bytes, no NA.
  # What it forbids is the third outcome, a table whose verdict is clean because
  # the bytes were turned into something valid on the way in. That would change
  # a value in the delivery without saying so, which is the one thing a transfer
  # record must never do.
  #
  # The declared encoding is the default, UTF-8, and the bytes FF FE occur in no
  # UTF-8 text.
  delivered <- c(charToRaw("A00000"), as.raw(c(0xff, 0xfe)))

  # The delivery as it was first written: its only data row holds the bytes.
  only_row <- file.path(dir, "bad-only-row.csv")
  writeBin(c(charToRaw("ID,AGE\n"), delivered, charToRaw(",30\n")), only_row)

  # The same bytes in the last of 3001 data rows. Read in 1 KiB blocks (set
  # below), the streaming reader has long left its first block by then, so the
  # bytes reach the scan that check() runs and not the opening of the dataset.
  valid_rows <- sprintf("A%07d,30\n", seq_len(3000L))
  late_row <- file.path(dir, "bad-late-row.csv")
  writeBin(
    c(charToRaw("ID,AGE\n"), charToRaw(paste(valid_rows, collapse = "")), delivered, charToRaw(",30\n")),
    late_row
  )
  withr::local_options(DTAtools.stream_block_size = 1024L)

  # The same delivery with every byte valid: the two bytes are U+00E9 instead.
  # If this were refused, or came back with a different ID, a refusal of the
  # files above would say nothing about their bytes.
  valid_twin <- c(charToRaw("A00000"), as.raw(c(0xc3, 0xa9)))
  control_path <- file.path(dir, "good-encoding.csv")
  writeBin(c(charToRaw("ID,AGE\n"), valid_twin, charToRaw(",30\n")), control_path)

  deliveries <- list(
    list(what = "the only data row", path = only_row, row = 1L),
    list(what = "data row 3001, after 3000 valid rows", path = late_row, row = 3001L)
  )

  # Both readers: the in-memory one and the streaming (dataset) one open and
  # read the file by different code.
  for (stream in c("never", "always")) {
    route <- sprintf("stream = \"%s\"", stream)

    control <- ri_bytes_outcome(control_path, stream)
    qa_step(
      sprintf("%s: the same delivery with valid bytes is read and validated clean", route),
      TRUE, is.null(control$refused_at) && isTRUE(control$ok)
    )
    qa_step(
      sprintf("%s: and its ID is read back as delivered", route),
      valid_twin, control$id
    )

    for (delivery in deliveries) {
      bad <- ri_bytes_outcome(delivery$path, stream, delivery$row)
      refused <- !is.null(bad$refused_at)
      detail <- ri_outcome_detail(bad, delivery$row)
      qa_check(
        sprintf(
          "%s, invalid bytes in %s: refused, or reported against row %d of ID",
          route, delivery$what, delivery$row
        ),
        refused || (isFALSE(bad$ok) && isTRUE(bad$names_row)),
        detail = detail
      )
      qa_check(
        sprintf(
          "%s, invalid bytes in %s: no value other than the delivered bytes is stored",
          route, delivery$what
        ),
        refused || identical(bad$id, delivered),
        detail = detail
      )
    }
  }
})

test_that("OQ-ROBUST-013 | a truncated compressed delivery is refused | REQ-ROBUST-013", {
  dir <- qa_tempdir()

  # The contract is the requirement's own sentence: a truncated compressed
  # delivery "shall raise a condition rather than validate the part that could
  # be read". One outcome meets it -- an error condition, from load_file() or
  # from check() -- and the case accepts no other, so a delivery that is read
  # up to the cut and given a verdict fails it, whatever the verdict says.
  #
  # The archive is 20000 rows long and cut in half, so that the part that could
  # be read is most of ten thousand rows and not, as an archive of a few lines
  # would leave, a header the reader cannot use anyway: a refusal of a cut that
  # left no table would say nothing about a reader that tolerates the cut.
  # Read in 1 KiB blocks (set below) the streaming reader opens the dataset on
  # the first of them, which is readable, so for it the cut is met by the scan
  # that check() runs.
  withr::local_options(DTAtools.stream_block_size = 1024L)

  n_rows <- 20000L
  archive <- ri_archive(dir, n_rows)
  whole <- archive$whole
  truncated <- archive$truncated

  # What the cut leaves, by a reader that is not this package's: base R's gzfile
  # hands back every line it can decompress, the last of them cut short. The
  # case needs this to be a real table's worth, or a refusal below could be put
  # down to nothing being readable.
  readable <- local({
    con <- gzfile(truncated, "r")
    on.exit(close(con))
    suppressWarnings(readLines(con))
  })
  complete_rows <- length(readable) - 2L
  qa_check(
    "the cut archive still holds its header and over a thousand complete rows",
    identical(readable[[1]], "ID,AGE") && complete_rows > 1000L,
    detail = sprintf("%d complete data row(s) readable of %d", complete_rows, n_rows)
  )

  for (stream in c("never", "always")) {
    route <- sprintf("stream = \"%s\"", stream)

    intact <- ri_run(whole, stream)
    qa_check(
      sprintf("%s: the uncut archive is read and validated clean", route),
      is.null(intact$refused_at) && isTRUE(validation_status(intact$checked)$ok),
      detail = ri_run_detail(intact)
    )
    qa_step(
      sprintf("%s: and every row of it is in the table", route),
      n_rows, ri_run_rows(intact)
    )

    cut <- ri_run(truncated, stream)
    qa_check(
      sprintf("%s: the cut archive is refused with a condition, and given no verdict", route),
      !is.null(cut$refused_at),
      detail = ri_run_detail(cut)
    )
  }
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

test_that("OQ-ROBUST-016 | specification-derived text in a condition message is shown literally, never evaluated | REQ-ROBUST-016", {
  # A payload that would visibly misbehave if handed to cli as part of the
  # FORMAT string rather than as literal content: cli/glue evaluates an
  # interpolated "{1+1}" and would print "2" in its place. Escaped, the
  # braces come through the abort unchanged -- this is the only way to tell
  # the two apart from outside the function.
  payload_id <- "range_rule_{1+1}"

  specs <- DTAColumnSpecCollection(
    columns = list(AGE = DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)),
    rules = list(DTARuleColRange(id = payload_id, columns = "AGE", range = c(0, 10)))
  )
  # Clean on the column-specification axis, so validate_table() takes the
  # direct "Rule violations:" abort rather than the columnspec-error return
  # path, whose warning a tryCatch(error = ) would not even catch.
  frame <- data.frame(AGE = c(5, 99), stringsAsFactors = FALSE)

  rules_err <- tryCatch(validate_rules(specs, frame), error = function(e) e)
  table_err <- tryCatch(validate_table(specs, frame, verbose = FALSE), error = function(e) e)

  qa_check(
    "validate_rules() and validate_table() both refuse the out-of-range table",
    inherits(rules_err, "condition") && inherits(table_err, "condition")
  )
  qa_step(
    "and each carries the rule id literally, braces and all",
    c(rules = TRUE, table = TRUE),
    c(
      rules = grepl(payload_id, conditionMessage(rules_err), fixed = TRUE),
      table = grepl(payload_id, conditionMessage(table_err), fixed = TRUE)
    )
  )
  qa_step(
    "not what an evaluated \"{1+1}\" would have printed instead",
    c(rules = FALSE, table = FALSE),
    c(
      rules = grepl("range_rule_2", conditionMessage(rules_err), fixed = TRUE),
      table = grepl("range_rule_2", conditionMessage(table_err), fixed = TRUE)
    )
  )

  # validate_template(strict = TRUE): the payload arrives as a template FILE
  # NAME this time rather than a rule id, but the mechanism under test
  # (.cli_escape(), R/validateTemplate.R) and the visible misbehaviour if it
  # were skipped are the same.
  tdir <- qa_tempdir()
  dir.create(file.path(tdir, "templates"))
  writeLines(
    c("kind: dta_creation_template", "id: cli_inj", "version: 1.10", "abstract: true"),
    file.path(tdir, "templates", "bad_{1+1}_version.dta-template.yaml")
  )
  tmpl_err <- tryCatch(validate_template(tdir, strict = TRUE), error = function(e) e)

  qa_check(
    "a strict validation with an error-severity finding (unquoted version) aborts",
    inherits(tmpl_err, "condition")
  )
  qa_step(
    "and the offending file name is shown literally, not evaluated",
    c(literal = TRUE, evaluated = FALSE),
    c(
      literal = grepl("bad_{1+1}_version", conditionMessage(tmpl_err), fixed = TRUE),
      evaluated = grepl("bad_2_version", conditionMessage(tmpl_err), fixed = TRUE)
    )
  )
})

test_that("OQ-ROBUST-017 | a lazily held table whose file has vanished fails alone | REQ-ROBUST-017", {
  dir <- qa_tempdir()
  kept <- file.path(dir, "kept.csv")
  gone <- file.path(dir, "gone.csv")
  writeLines(c("ID,AGE", "A001,40"), kept)
  writeLines(c("ID,AGE", "A002,41"), gone)

  ds <- DTADataSetTabular(
    id = "robust", specs = ri_specs(),
    files = list(
      DTAFileCSV(filename = "kept.csv"),
      DTAFileCSV(filename = "gone.csv")
    )
  )
  ds <- load_file(ds, file = kept, handler_index = 1, stream = "always")
  ds <- load_file(ds, file = gone, handler_index = 2, stream = "always")
  unlink(gone)

  checked <- tryCatch(check(ds, persist = FALSE, quiet = TRUE), error = function(e) e)
  qa_check("check() completes although one table's file is gone", S7::S7_inherits(checked, DTADataSetTabular))

  status <- validation_status(checked)
  status <- status[order(status$table, method = "radix"), ]
  qa_step(
    "the table whose file is present keeps its own clean verdict",
    TRUE, status$ok[status$table == "kept"]
  )
  qa_step(
    "the vanished table is validated, not ok, with one rule finding and nothing else",
    list(status = "validated", ok = FALSE, n_rule_errors = 1, n_columnspec_errors = 0, n_import_errors = 0),
    with(status[status$table == "gone", ], list(
      status = status, ok = ok,
      n_rule_errors = as.numeric(n_rule_errors),
      n_columnspec_errors = as.numeric(n_columnspec_errors),
      n_import_errors = as.numeric(n_import_errors)
    ))
  )

  msgs <- messages(checked, as_tibble = FALSE)
  qa_step("the finding is file_presence", "file_presence", msgs$rule_id)
  qa_check("and its message names the missing file", grepl("gone.csv", msgs$message, fixed = TRUE))
})

test_that("OQ-ROBUST-018 | the refusal of bytes that are not valid UTF-8 is a condition a caller can match | REQ-ROBUST-018", {
  dir <- qa_tempdir()

  # The contract is the requirement's own sentence: a delimited delivery whose
  # bytes are not valid UTF-8 "shall be refused with a condition of class
  # `dta_encoding_invalid`" when they are in its header or in a column the
  # specification declares (ID is one), whichever of load_file() and check()
  # meets them, and "no other failure shall carry that class". So the case holds
  # three kinds of delivery to it:
  #   - bytes FF FE, which occur in no UTF-8 text, in the only data row, in data
  #     row 3001 after 3000 valid rows, and in the header: each must be refused
  #     with the class. Read in 1 KiB blocks (set below), the streaming reader
  #     is far past its first block by row 3001, so those bytes reach the scan
  #     that check() runs and not the opening of the dataset; the in-memory
  #     reader, the opening and the scan are three different routes to the same
  #     refusal, and the two stream settings cover all three.
  #   - the same three with valid bytes (U+00E9 for FF FE): each must be read.
  #     Were they refused too, a refusal of the others would say nothing about
  #     their bytes.
  #   - a row of three fields where the header declares two, and an archive cut
  #     in half: each is refused, because it cannot be read, and must NOT carry
  #     the class. A class given to every read failure would pass every step
  #     above, which is why these are here.
  withr::local_options(DTAtools.stream_block_size = 1024L)

  valid_rows <- paste(sprintf("A%07d,30\n", seq_len(3000L)), collapse = "")
  bad_id <- c(charToRaw("A00000"), as.raw(c(0xff, 0xfe)))
  valid_id <- c(charToRaw("A00000"), as.raw(c(0xc3, 0xa9)))
  write_csv_bytes <- function(name, ...) {
    path <- file.path(dir, name)
    pieces <- lapply(list(...), function(piece) if (is.raw(piece)) piece else charToRaw(piece))
    writeBin(do.call(c, pieces), path)
    path
  }

  bad <- list(
    list(what = "the only data row", path = write_csv_bytes("bad-only-row.csv", "ID,AGE\n", bad_id, ",30\n")),
    list(what = "data row 3001", path = write_csv_bytes("bad-late-row.csv", "ID,AGE\n", valid_rows, bad_id, ",30\n")),
    list(what = "the header", path = write_csv_bytes("bad-header.csv", "ID", as.raw(c(0xff, 0xfe)), ",AGE\nA0000001,30\n"))
  )
  twins <- list(
    list(what = "the only data row", path = write_csv_bytes("ok-only-row.csv", "ID,AGE\n", valid_id, ",30\n")),
    list(what = "data row 3001", path = write_csv_bytes("ok-late-row.csv", "ID,AGE\n", valid_rows, valid_id, ",30\n")),
    list(what = "the header", path = write_csv_bytes("ok-header.csv", "ID", as.raw(c(0xc3, 0xa9)), ",AGE\nA0000001,30\n"))
  )
  ragged <- file.path(dir, "ragged.csv")
  writeLines(c("ID,AGE", sprintf("A%07d,30", seq_len(3000L)), "A9999999,30,EXTRA"), ragged)
  cut <- ri_archive(dir, 20000L)$truncated
  unreadable <- list(
    list(what = "a row of three fields under a header of two", path = ragged),
    list(what = "a gzip archive cut in half", path = cut)
  )

  for (stream in c("never", "always")) {
    route <- sprintf("stream = \"%s\"", stream)

    for (twin in twins) {
      run <- ri_run(twin$path, stream)
      qa_check(
        sprintf("%s, valid bytes in %s: read", route, twin$what),
        is.null(run$refused_at),
        detail = ri_run_detail(run)
      )
    }
    for (delivery in bad) {
      run <- ri_run(delivery$path, stream)
      qa_check(
        sprintf("%s, invalid bytes in %s: refused with a condition of class dta_encoding_invalid", route, delivery$what),
        "dta_encoding_invalid" %in% run$classes,
        detail = ri_run_detail(run)
      )
    }
    for (delivery in unreadable) {
      run <- ri_run(delivery$path, stream)
      qa_check(
        sprintf("%s, %s: refused, and not with that class", route, delivery$what),
        !is.null(run$refused_at) && !("dta_encoding_invalid" %in% run$classes),
        detail = ri_run_detail(run)
      )
    }
  }
})
