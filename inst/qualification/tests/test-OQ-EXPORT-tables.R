# Data and specification export. REQ-EXPORT-020 .. REQ-EXPORT-029.
#
# write_table_to_file(), write_columns_to_yaml() and write_columns_to_json().
# Every case reads the written file back with an independent reader (base
# R's own read.table()/gzfile()/jsonlite, never the function under test) and
# compares against the fixture the test itself declares.

# ---- fixtures ---------------------------------------------------------------

et_table_specs <- function() {
  DTAColumnSpecCollection(columns = list(
    ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE),
    AGE = DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
  ))
}

# Deliberately out of ID order and carrying one NA, so "sorted" and "as
# stored" and "with a missing value written out" are all distinguishable.
et_table_frame <- function() {
  data.frame(ID = c("B02", "A01", "C03"), AGE = c(30, NA, 25), stringsAsFactors = FALSE)
}

et_dataset <- function(frame = et_table_frame(), name = "et") {
  DTADataSetTabular(name = name, specs = et_table_specs(), tables = list(t1 = frame))
}

# A delimited round trip does not promise to preserve R's storage mode (an
# integer written without a decimal point reads back as integer even if it
# started out double), so comparisons are made as text, which is what a
# reader of the file actually sees.
et_as_text <- function(df) as.data.frame(lapply(df, as.character), stringsAsFactors = FALSE)

# A dataset whose extra (undeclared) column carries a value that fails when
# write.table() tries to convert it with as.character() -- undeclared columns
# are passed through untouched by the column-spec coercion, so the class
# survives into the stored Arrow table and back out again. A fresh class name
# per call keeps repeated calls in the same test from colliding, and the S3
# method is registered on .GlobalEnv because write.table() (defined in the
# utils namespace) still finds a method registered there for a class it does
# not know -- the same mechanism that lets a user's own print.<class> reach a
# base generic called deep inside another package -- and removed again as
# soon as the calling test exits, however it exits.
et_poison_dataset <- function(env = parent.frame()) {
  cls <- paste0("qa_export_poison_", as.integer(stats::runif(1, 1, 1e9)))
  method_name <- paste0("as.character.", cls)
  assign(method_name, function(x, ...) stop("qa_export_poison: simulated write failure"), envir = globalenv())
  withr::defer(rm(list = method_name, envir = globalenv()), envir = env)

  frame <- data.frame(ID = 1:5, EXTRA = structure(1:5, class = cls))
  DTADataSetTabular(
    name = "et_poison",
    specs = DTAColumnSpecCollection(columns = list(
      ID = DTAColumnSpec(id = "ID", type = "SAS Num", nullable = FALSE)
    )),
    tables = list(t1 = frame)
  )
}

# A column collection exercising every facet write_columns_to_yaml()/
# write_columns_to_json() are asked to preserve: a length, an enumerated
# codelist carrying a leading-zero value, and a pattern on a second column
# (pattern and values are mutually exclusive on one column).
et_yaml_specs <- function() {
  DTAColumnSpecCollection(columns = list(
    CODE = DTAColumnSpec(
      id = "CODE", label = "Code", type = "SAS Char", length = 5, nullable = FALSE,
      values = c("007", "008", "Y", "N", "01"), description = "A code with leading-zero values"
    ),
    NOTE = DTAColumnSpec(id = "NOTE", type = "SAS Char", nullable = TRUE, pattern = "^[A-Z]{3}$")
  ))
}

# ---- write_table_to_file(): separator, NA string --------------------------

test_that("OQ-EXPORT-027 | write_table_to_file writes the declared separator and NA representation | REQ-EXPORT-020", {
  dir <- qa_tempdir()
  out <- file.path(dir, "plain.csv")
  write_table_to_file(
    et_dataset(),
    table = "t1", filename = out,
    sep = ";", na = "MISSING", overwrite = TRUE, quiet = TRUE, arrange_by = NULL
  )

  back <- read.table(out, sep = ";", header = TRUE, na.strings = "MISSING", stringsAsFactors = FALSE)
  qa_step(
    "the file reads back to the same values with the declared separator and NA string",
    et_as_text(et_table_frame()), et_as_text(back)
  )
  qa_step(
    "the missing value is written using the declared string, not the default \"NA\"",
    TRUE, any(grepl("MISSING", readLines(out), fixed = TRUE))
  )
})

# ---- write_table_to_file(): arrange_by -------------------------------------

test_that("OQ-EXPORT-028 | arrange_by \"all\" sorts by every column and NULL preserves original order | REQ-EXPORT-021", {
  ds <- et_dataset()
  dir <- qa_tempdir()

  sorted <- file.path(dir, "sorted.csv")
  write_table_to_file(ds, table = "t1", filename = sorted, sep = ",", overwrite = TRUE, quiet = TRUE) # arrange_by = "all" is the default
  qa_step(
    "arrange_by = \"all\" (the default) sorts rows ascending by ID",
    c("A01", "B02", "C03"),
    read.table(sorted, sep = ",", header = TRUE, stringsAsFactors = FALSE)$ID
  )

  unsorted <- file.path(dir, "unsorted.csv")
  write_table_to_file(ds, table = "t1", filename = unsorted, sep = ",", overwrite = TRUE, quiet = TRUE, arrange_by = NULL)
  qa_step(
    "arrange_by = NULL keeps the table's own row order",
    c("B02", "A01", "C03"),
    read.table(unsorted, sep = ",", header = TRUE, stringsAsFactors = FALSE)$ID
  )
})

test_that("OQ-EXPORT-029 | arrange_by with named columns sorts by those columns only | REQ-EXPORT-021", {
  specs <- DTAColumnSpecCollection(columns = list(
    GROUP = DTAColumnSpec(id = "GROUP", type = "SAS Char", length = 1, nullable = FALSE),
    ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 1, nullable = FALSE)
  ))
  # GROUP and ID disagree about row order: sorting by ID alone ties two rows
  # on GROUP = "B", which a stable sort resolves by original position, while
  # sorting by every column sorts GROUP first. The two results below can only
  # coincide if arrange_by is actually restricting the sort to ID.
  frame <- data.frame(GROUP = c("B", "A", "B"), ID = c("2", "2", "1"), stringsAsFactors = FALSE)
  ds <- DTADataSetTabular(name = "et_arrange", specs = specs, tables = list(t1 = frame))
  dir <- qa_tempdir()

  by_id <- file.path(dir, "by_id.csv")
  write_table_to_file(ds, table = "t1", filename = by_id, sep = ",", overwrite = TRUE, quiet = TRUE, arrange_by = "ID")
  qa_step(
    "sorting by ID alone ignores GROUP",
    c("B", "B", "A"), read.table(by_id, sep = ",", header = TRUE, stringsAsFactors = FALSE)$GROUP
  )

  by_all <- file.path(dir, "by_all.csv")
  write_table_to_file(ds, table = "t1", filename = by_all, sep = ",", overwrite = TRUE, quiet = TRUE, arrange_by = "all")
  qa_step(
    "sorting by every column sorts GROUP first",
    c("A", "B", "B"), read.table(by_all, sep = ",", header = TRUE, stringsAsFactors = FALSE)$GROUP
  )
})

# ---- write_table_to_file(): compression ------------------------------------

test_that("OQ-EXPORT-030 | compression = \"gzip\" writes a file that decompresses to the same table | REQ-EXPORT-022", {
  dir <- qa_tempdir()
  out <- file.path(dir, "compressed.csv.gz")
  write_table_to_file(
    et_dataset(),
    table = "t1", filename = out,
    sep = ",", na = "NA", overwrite = TRUE, quiet = TRUE, compression = "gzip", arrange_by = NULL
  )

  back <- read.table(gzfile(out), sep = ",", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE)
  qa_step(
    "the gzipped file decompresses to the same values as the table it was written from",
    et_as_text(et_table_frame()), et_as_text(back)
  )
})

# ---- write_table_to_file(): overwrite guard --------------------------------

test_that("OQ-EXPORT-031 | write_table_to_file refuses to overwrite an existing file and leaves it untouched | REQ-EXPORT-023", {
  dir <- qa_tempdir()
  out <- file.path(dir, "existing.csv")
  writeLines("ORIGINAL,CONTENT", out)
  original <- readLines(out)

  cond <- tryCatch(
    write_table_to_file(et_dataset(), table = "t1", filename = out, quiet = TRUE, arrange_by = NULL),
    error = function(e) e
  )
  qa_check("a condition was raised", inherits(cond, "condition"))
  qa_step("the original file content is unchanged", original, readLines(out))
})

# ---- write_table_to_file(): md5 sidecar -------------------------------------

test_that("OQ-EXPORT-032 | the md5 sidecar names the checksum, column count and row count of the file written | REQ-EXPORT-024", {
  dir <- qa_tempdir()
  out <- file.path(dir, "data.csv")
  written <- write_table_to_file(
    et_dataset(),
    table = "t1", filename = out, sep = ",", overwrite = TRUE, quiet = TRUE, arrange_by = NULL
  )

  sidecar <- paste0(out, ".md5")
  qa_check("the sidecar file was written", file.exists(sidecar))

  # Checked against an independent computation of the file's own checksum,
  # never a hardcoded hash -- the same cross-check PQ-PERF-008 uses.
  disk_checksum <- unname(tools::md5sum(out))
  qa_step(
    "the recorded checksum is the checksum of the file actually on disk",
    disk_checksum, as.character(written$md5sum[[1]])
  )
  qa_step(
    "the recorded dimensions are the table's",
    c(rows = 3L, columns = 2L),
    c(rows = as.integer(written$md5sum[[2]]), columns = as.integer(written$md5sum[[3]]))
  )
  qa_step(
    "the sidecar file names the same three facts in the documented format",
    c(paste0("md5sum: ", disk_checksum), "Number of Columns: 2", "Number of Rows: 3"),
    readLines(sidecar)
  )
})

test_that("OQ-EXPORT-033 | get_md5sum = FALSE returns no checksum and writes no sidecar | REQ-EXPORT-024", {
  dir <- qa_tempdir()
  out <- file.path(dir, "data.csv")
  written <- write_table_to_file(
    et_dataset(),
    table = "t1", filename = out,
    overwrite = TRUE, quiet = TRUE, arrange_by = NULL, get_md5sum = FALSE
  )

  qa_check("the checksum is reported as NA", is.na(written$md5sum))
  qa_check("no sidecar file was written", !file.exists(paste0(out, ".md5")))
})

# ---- write_table_to_file(): atomicity (pinned finding) ---------------------

test_that("OQ-EXPORT-034 | a write that fails leaves the destination exactly as it was, on both branches | REQ-EXPORT-025", {
  dir <- qa_tempdir()

  # The case that matters is not a failed write to a fresh path. It is a failed
  # RE-export over a delivery that was already there and already good, because
  # that is when a truncated file replaces something a receiver was relying on.
  plain <- file.path(dir, "plain.csv")
  write_table_to_file(
    et_dataset(),
    table = "t1", filename = plain,
    sep = ",", quiet = TRUE, arrange_by = NULL
  )
  before <- readLines(plain)
  qa_check("a good export is in place to be overwritten", length(before) > 1)

  cond1 <- tryCatch(
    write_table_to_file(
      et_poison_dataset(),
      table = "t1", filename = plain,
      sep = ",", overwrite = TRUE, quiet = TRUE, arrange_by = NULL
    ),
    error = function(e) e
  )
  qa_check("the plain write fails, as the poisoned column is designed to make it", inherits(cond1, "condition"))
  # A header-only file is worse than no file: it still reads as a table, with
  # no rows, and a table with no rows breaks no constraint and so validates
  # perfectly clean. The write now goes to a temporary file beside the
  # destination and is renamed into place only once the whole table converted.
  qa_step("the delivery that was already there is byte-for-byte unchanged", before, readLines(plain))
  qa_step(
    "and no partial file is left beside it",
    character(0),
    Filter(function(f) endsWith(f, ".part"), list.files(dir))
  )

  gzipped <- file.path(dir, "gzipped.csv.gz")
  cond2 <- tryCatch(
    write_table_to_file(
      et_poison_dataset(),
      table = "t1", filename = gzipped,
      sep = ",", overwrite = TRUE, quiet = TRUE, arrange_by = NULL, compression = "gzip"
    ),
    error = function(e) e
  )
  qa_check("the gzip write fails the same way", inherits(cond2, "condition"))
  qa_step("and creates nothing at a destination that did not exist", FALSE, file.exists(gzipped))

  # The same must hold for the plain branch at a fresh path: a failure there
  # leaves nothing, rather than a file a later reader would take for an export.
  fresh <- file.path(dir, "fresh.csv")
  invisible(tryCatch(
    write_table_to_file(
      et_poison_dataset(),
      table = "t1", filename = fresh,
      sep = ",", quiet = TRUE, arrange_by = NULL
    ),
    error = function(e) e
  ))
  qa_step("the plain branch likewise creates nothing when it cannot finish", FALSE, file.exists(fresh))
})

# ---- write_table_to_file(): verdict round trip -----------------------------

test_that("OQ-EXPORT-035 | the written file re-read reaches the same validation verdict as the table it came from | REQ-EXPORT-026", {
  ds_checked <- check(et_dataset(), persist = FALSE, quiet = TRUE)

  dir <- qa_tempdir()
  out <- file.path(dir, "roundtrip.csv")
  write_table_to_file(et_dataset(), table = "t1", filename = out, sep = ",", na = "", overwrite = TRUE, quiet = TRUE, arrange_by = NULL)

  ds2 <- DTADataSetTabular(
    name = "et2", specs = et_table_specs(),
    files = list(DTAFileCSV(filename = basename(out)))
  )
  ds2 <- load_file(ds2, file = out, handler_index = 1, stream = "never")
  ds2 <- check(ds2, persist = FALSE, quiet = TRUE)

  qa_step(
    "the re-read file reaches the same verdict as the table it was written from",
    validation_status(ds_checked)$ok, validation_status(ds2)$ok
  )
})

# ---- write_columns_to_yaml() ------------------------------------------------

test_that("OQ-EXPORT-036 | write_columns_to_yaml round trips ids, types, lengths, nullability, values and patterns | REQ-EXPORT-027", {
  dir <- qa_tempdir()
  out <- file.path(dir, "specs.yaml")
  write_columns_to_yaml(et_yaml_specs(), out)
  back <- import_specs_from_yaml(out)

  qa_step("the same column identifiers come back, in order", c("CODE", "NOTE"), names(back@columns))
  qa_step(
    "CODE keeps its type, length, nullability and permitted values",
    list(type = "Char", length = 5, nullable = FALSE, values = c("007", "008", "Y", "N", "01")),
    list(
      type = back@columns$CODE@structure@type,
      length = as.numeric(back@columns$CODE@structure@length),
      nullable = back@columns$CODE@nullable,
      values = as.character(back@columns$CODE@values)
    )
  )
  qa_step(
    "NOTE keeps its type, nullability and pattern",
    list(type = "Char", nullable = TRUE, pattern = "^[A-Z]{3}$"),
    list(
      type = back@columns$NOTE@structure@type,
      nullable = back@columns$NOTE@nullable,
      pattern = back@columns$NOTE@pattern
    )
  )
})

test_that("OQ-EXPORT-037 | the yaml round trip preserves a leading-zero value such as \"007\" as text | REQ-EXPORT-027", {
  dir <- qa_tempdir()
  out <- file.path(dir, "specs.yaml")
  write_columns_to_yaml(et_yaml_specs(), out)
  values <- import_specs_from_yaml(out)@columns$CODE@values

  qa_step("the value is stored as text, not converted to a number", "character", class(values))
  qa_check("\"007\" survives the round trip with its leading zero intact", "007" %in% values)
})

# ---- write_columns_to_json() ------------------------------------------------

test_that("OQ-EXPORT-038 | write_columns_to_json writes the declared id, type, length, nullable and values | REQ-EXPORT-028", {
  dir <- qa_tempdir()
  out <- file.path(dir, "specs.json")
  write_columns_to_json(et_yaml_specs(), out)

  json <- jsonlite::fromJSON(out, simplifyVector = FALSE)
  code <- json$columns$CODE
  qa_step(
    "the CODE entry names its id, type, length, nullable and permitted values",
    list(id = "CODE", type = "SAS Char", length = 5, nullable = FALSE, values = c("007", "008", "Y", "N", "01")),
    list(
      id = code$id, type = code$type, length = as.numeric(code$length), nullable = isTRUE(code$nullable),
      values = vapply(code$values, identity, character(1))
    )
  )
})

test_that("OQ-EXPORT-039 | write_columns_to_json cannot round trip a column that leaves any property unset | REQ-EXPORT-029", {
  # Mirrors the commented-out import_specs_from_json() left in
  # R/DTAColumnSpecCollection-class.R -- the same specs_from_list()
  # construction import_specs_from_yaml() uses for YAML, applied to the
  # parsed JSON instead. There is no package-provided JSON reader to call.
  et_specs_from_json <- function(file) {
    parsed <- jsonlite::fromJSON(file, simplifyVector = FALSE)
    specs_from_list(parsed$columns, parsed$rules)
  }

  dir <- qa_tempdir()
  out <- file.path(dir, "specs.json")
  write_columns_to_json(et_yaml_specs(), out)

  cond <- tryCatch(et_specs_from_json(out), error = function(e) e)
  # PINNED: CODE leaves colclass unset (never declared) and, because values is
  # set, examples and pattern unset too (the two are mutually exclusive on one
  # column); every one of the three is written as "{}" rather than null, and
  # jsonlite reads "{}" back as an empty list rather than NULL. NOTE leaves
  # label, description, colclass and values unset the same way. No column,
  # however fully specified, currently avoids leaving at least one property
  # unset, so this reconstruction fails for every collection, not a
  # cherry-picked one.
  qa_check(
    "PINNED: reconstructing the collection from write_columns_to_json's own output fails",
    inherits(cond, "condition")
  )
  qa_step(
    "PINNED: the failure is the property-type mismatch an unset field written as {} leaves behind",
    TRUE, grepl("<list>", conditionMessage(cond), fixed = TRUE)
  )
  qa_known_deviation("DEV-015", inherits(cond, "condition"))
})
