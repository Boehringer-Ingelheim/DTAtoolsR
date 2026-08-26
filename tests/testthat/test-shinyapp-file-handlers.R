# File-handler editing in the Shiny app.
#
# A file handler is not just a piece of specification: it IS an upload slot. The
# app keys uploads, per-slot file inputs and per-file trash buttons by the
# handler's POSITION, so adding, removing or reordering one moves state that
# lives outside the DTA object. These tests cover the helpers behind the
# Edit-files dialog; test-shinyapp-server.R covers what the server does with
# them.

# ---- Reading handlers -------------------------------------------------------

test_that("dta_handlers_overview describes the fixture's single handler", {
  fn <- app_fn("dta_handlers_overview")

  ov <- fn(app_fixture_dta(), "clinical_data")

  expect_equal(nrow(ov), 1)
  expect_equal(ov$filename, "clinical_data.*.csv$")
  expect_equal(ov$type, "csv")
  expect_equal(ov$pattern, "yes")
  expect_equal(ov$files, "1 file")
})

test_that("dta_handlers_overview of a dataset without handlers is empty", {
  rm_fn <- app_fn("dta_remove_handler")
  fn <- app_fn("dta_handlers_overview")

  stripped <- rm_fn(app_fixture_dta(), "clinical_data", 1)$value

  expect_equal(nrow(fn(stripped, "clinical_data")), 0)
})

test_that("dta_handler_fields returns NULL for an index that is not there", {
  fn <- app_fn("dta_handler_fields")

  expect_null(fn(app_fixture_dta(), "clinical_data", 2))
  expect_null(fn(app_fixture_dta(), "clinical_data", 0))
})

test_that("dta_handler_fields round-trips through dta_set_handler unchanged", {
  fields_fn <- app_fn("dta_handler_fields")
  set_fn <- app_fn("dta_set_handler")

  before <- fields_fn(app_fixture_dta(), "clinical_data", 1)
  res <- do.call(set_fn, c(
    list(app_fixture_dta(), "clinical_data", index = 1),
    before[c(
      "filename", "type", "pattern", "count_mode", "number_of_files",
      "min_number_of_files", "max_number_of_files", "pattern_description", "info"
    )]
  ))

  expect_true(res$ok)
  expect_identical(fields_fn(res$value, "clinical_data", 1), before)
})

# ---- Adding and editing -----------------------------------------------------

test_that("a handler added with index = NULL is appended, leaving the first alone", {
  set_fn <- app_fn("dta_set_handler")
  handlers_fn <- app_fn("dta_handlers")

  res <- set_fn(app_fixture_dta(), "clinical_data",
    index = NULL, filename = "extra.tsv", type = "tsv"
  )

  expect_true(res$ok)
  hs <- handlers_fn(datasets(res$value, "clinical_data"))
  expect_length(hs, 2)
  expect_equal(hs[[1]]@filename, "clinical_data.*.csv$")
  expect_equal(hs[[2]]@filename, "extra.tsv")
  expect_s3_class(hs[[2]], "DTAtools::DTAFileTSV")
})

test_that("editing a handler in place does not change how many there are", {
  set_fn <- app_fn("dta_set_handler")
  handlers_fn <- app_fn("dta_handlers")

  res <- set_fn(app_fixture_dta(), "clinical_data",
    index = 1, filename = "renamed.csv", type = "csv", pattern = FALSE
  )

  expect_true(res$ok)
  hs <- handlers_fn(datasets(res$value, "clinical_data"))
  expect_length(hs, 1)
  expect_equal(hs[[1]]@filename, "renamed.csv")
  expect_false(hs[[1]]@pattern)
})

test_that("a handler keeps its pattern description and info", {
  # DTAFile has always had a pattern_description property and the app has always
  # serialised it, but no concrete subclass constructor accepted one -- so a
  # handler carrying it could be written and never read back.
  set_fn <- app_fn("dta_set_handler")
  fields_fn <- app_fn("dta_handler_fields")

  res <- set_fn(app_fixture_dta(), "clinical_data",
    index = 1, filename = "clinical_data.*[.]csv$", type = "csv",
    pattern = TRUE, count_mode = "range",
    min_number_of_files = 1, max_number_of_files = 3,
    pattern_description = "one file per site",
    info = c("data: clinical", "code: cl")
  )

  expect_true(res$ok)
  got <- fields_fn(res$value, "clinical_data", 1)
  expect_equal(got$pattern_description, "one file per site")
  expect_equal(got$info, "data: clinical\ncode: cl")
  expect_equal(got$count_mode, "range")
  expect_equal(got$min_number_of_files, 1L)
  expect_equal(got$max_number_of_files, 3L)
})

# ---- What the form must refuse ----------------------------------------------

test_that("a non-pattern handler may not expect more than one file", {
  # DTAFile's own contract: one exact name matches one file. Caught before the
  # constructor so the editor can show a sentence instead of a class abort.
  fn <- app_fn("dta_set_handler")

  res <- fn(app_fixture_dta(), "clinical_data",
    index = 1, filename = "clinical_data.csv", type = "csv",
    pattern = FALSE, count_mode = "exact", number_of_files = 2
  )

  expect_false(res$ok)
  expect_match(res$error, "exactly 1 file")
})

test_that("a range of files is rejected without a pattern", {
  fn <- app_fn("dta_set_handler")

  res <- fn(app_fixture_dta(), "clinical_data",
    index = 1, filename = "clinical_data.csv", type = "csv",
    pattern = FALSE, count_mode = "range",
    min_number_of_files = 1, max_number_of_files = 3
  )

  expect_false(res$ok)
  expect_match(res$error, "only makes sense for a pattern")
})

test_that("a range whose minimum exceeds its maximum is rejected", {
  fn <- app_fn("dta_set_handler")

  res <- fn(app_fixture_dta(), "clinical_data",
    index = 1, filename = "a.*[.]csv$", type = "csv",
    pattern = TRUE, count_mode = "range",
    min_number_of_files = 4, max_number_of_files = 2
  )

  expect_false(res$ok)
  expect_match(res$error, "cannot exceed the maximum")
})

test_that("a handler needs a file name and a supported type", {
  fn <- app_fn("dta_set_handler")

  blank <- fn(app_fixture_dta(), "clinical_data", index = 1, filename = "  ")
  expect_false(blank$ok)
  expect_match(blank$error, "file name or pattern is required")

  bad_type <- fn(app_fixture_dta(), "clinical_data",
    index = 1, filename = "a.csv", type = "delim"
  )
  expect_false(bad_type$ok)
  expect_match(bad_type$error, "must be one of")
})

test_that("the editor offers only the types a reader can restore", {
  # `delim` is a real DTAFile subclass, but DTAFileFactory cannot build one, so
  # offering it would create a handler no document could be read back into.
  types <- app_fn("dta_handler_types")()

  expect_equal(types, c("csv", "tsv"))
})

test_that("only a pattern handler may carry several file names", {
  fn <- app_fn("dta_set_handler")

  res <- fn(app_fixture_dta(), "clinical_data",
    index = 1, filename = c("a.csv", "b.csv"), type = "csv", pattern = FALSE
  )

  expect_false(res$ok)
  expect_match(res$error, "more than one file name")
})

test_that("dta_set_handler rejects an index that is not there", {
  fn <- app_fn("dta_set_handler")

  res <- fn(app_fixture_dta(), "clinical_data",
    index = 7, filename = "a.csv", type = "csv"
  )

  expect_false(res$ok)
  expect_match(res$error, "not found")
})

# ---- Removing and reordering ------------------------------------------------

test_that("removing a handler removes that one and renumbers the rest", {
  set_fn <- app_fn("dta_set_handler")
  rm_fn <- app_fn("dta_remove_handler")
  handlers_fn <- app_fn("dta_handlers")

  dta <- app_fixture_dta()
  dta <- set_fn(dta, "clinical_data", index = NULL, filename = "b.tsv", type = "tsv")$value
  dta <- set_fn(dta, "clinical_data", index = NULL, filename = "c.csv", type = "csv")$value

  res <- rm_fn(dta, "clinical_data", 1)

  expect_true(res$ok)
  hs <- handlers_fn(datasets(res$value, "clinical_data"))
  expect_length(hs, 2)
  expect_equal(vapply(hs, function(h) h@filename, character(1)), c("b.tsv", "c.csv"))
})

test_that("removing the last handler leaves a dataset that expects no files", {
  rm_fn <- app_fn("dta_remove_handler")
  handlers_fn <- app_fn("dta_handlers")

  res <- rm_fn(app_fixture_dta(), "clinical_data", 1)

  expect_true(res$ok)
  ds <- datasets(res$value, "clinical_data")
  expect_length(handlers_fn(ds), 0)
  expect_equal(max_number_of_files(ds), 0)
})

test_that("dta_remove_handler rejects an index that is not there", {
  fn <- app_fn("dta_remove_handler")

  res <- fn(app_fixture_dta(), "clinical_data", 2)

  expect_false(res$ok)
  expect_match(res$error, "not found")
})

test_that("dta_move_handler swaps neighbours and stops at both ends", {
  set_fn <- app_fn("dta_set_handler")
  move_fn <- app_fn("dta_move_handler")
  handlers_fn <- app_fn("dta_handlers")
  names_of <- function(dta) {
    vapply(
      handlers_fn(datasets(dta, "clinical_data")),
      function(h) h@filename, character(1)
    )
  }

  dta <- app_fixture_dta()
  dta <- set_fn(dta, "clinical_data", index = NULL, filename = "b.tsv", type = "tsv")$value
  first <- names_of(dta)[1]

  down <- move_fn(dta, "clinical_data", 1, "down")
  expect_true(down$ok)
  expect_equal(names_of(down$value), c("b.tsv", first))

  # Past either end the object comes back untouched rather than erroring.
  top <- move_fn(dta, "clinical_data", 1, "up")
  expect_true(top$ok)
  expect_equal(names_of(top$value), c(first, "b.tsv"))

  bottom <- move_fn(dta, "clinical_data", 2, "down")
  expect_true(bottom$ok)
  expect_equal(names_of(bottom$value), c(first, "b.tsv"))
})

# ---- Where the upload records have to move to -------------------------------

test_that("dta_handler_index_map says where each handler ended up", {
  fn <- app_fn("dta_handler_index_map")

  # Adding leaves every existing handler where it was.
  expect_equal(unname(fn(3, "add")), c(1L, 2L, 3L))
  # Removing the first: it is gone (NA) and the rest shift down.
  expect_equal(unname(fn(3, "remove", index = 1)), c(NA_integer_, 1L, 2L))
  # Removing the last shifts nothing.
  expect_equal(unname(fn(3, "remove", index = 3)), c(1L, 2L, NA_integer_))
  # A move swaps exactly two positions.
  expect_equal(unname(fn(3, "move", index = 2, direction = "up")), c(2L, 1L, 3L))
  expect_equal(unname(fn(3, "move", index = 2, direction = "down")), c(1L, 3L, 2L))
  # A move past the end, and an index nobody has, change nothing.
  expect_equal(unname(fn(3, "move", index = 1, direction = "up")), c(1L, 2L, 3L))
  expect_equal(unname(fn(3, "remove", index = 9)), c(1L, 2L, 3L))
  expect_length(fn(0, "remove", index = 1), 0)
})

test_that("a handler change is visible in the handlers signature", {
  # The signature is what the raw-YAML apply path compares to decide whether a
  # dataset's slots still line up with its loaded files.
  set_fn <- app_fn("dta_set_handler")
  sig_fn <- app_fn("dta_handlers_signature")

  before <- app_fixture_dta()
  after <- set_fn(before, "clinical_data",
    index = NULL, filename = "b.tsv", type = "tsv"
  )$value

  expect_false(identical(
    sig_fn(datasets(before, "clinical_data")),
    sig_fn(datasets(after, "clinical_data"))
  ))
})

# ---- Round trips ------------------------------------------------------------

test_that("a dataset edited to two handlers survives a YAML round trip", {
  # The reason the app could not offer this before: it already serialised a
  # multi-handler dataset as a `files:` SEQUENCE, which the reader could not
  # parse -- so "Export DTA YAML" produced a document the app itself rejected.
  set_fn <- app_fn("dta_set_handler")
  to_yaml <- app_fn("dta_to_yaml_text")
  from_yaml <- app_fn("dta_read_yaml_text")
  handlers_fn <- app_fn("dta_handlers")

  dta <- set_fn(app_fixture_dta(), "clinical_data",
    index = NULL, filename = "extra.*[.]tsv$", type = "tsv",
    pattern = TRUE, count_mode = "range",
    min_number_of_files = 0, max_number_of_files = 2,
    pattern_description = "optional per-site extras"
  )$value

  txt <- to_yaml(dta)
  expect_true(txt$ok)

  back <- from_yaml(txt$value)
  expect_true(back$ok)

  hs <- handlers_fn(datasets(back$value, "clinical_data"))
  expect_length(hs, 2)
  expect_equal(hs[[2]]@filename, "extra.*[.]tsv$")
  expect_equal(hs[[2]]@pattern_description, "optional per-site extras")
  expect_equal(min_number_of_files(hs[[2]]), 0)
  expect_equal(max_number_of_files(hs[[2]]), 2)
})

test_that("a dataset with every handler removed survives a YAML round trip", {
  rm_fn <- app_fn("dta_remove_handler")
  to_yaml <- app_fn("dta_to_yaml_text")
  from_yaml <- app_fn("dta_read_yaml_text")
  handlers_fn <- app_fn("dta_handlers")

  stripped <- rm_fn(app_fixture_dta(), "clinical_data", 1)$value
  txt <- to_yaml(stripped)
  expect_true(txt$ok)

  back <- from_yaml(txt$value)
  expect_true(back$ok)
  expect_length(handlers_fn(datasets(back$value, "clinical_data")), 0)
})

# ---- The free-form info block ------------------------------------------------
# `info:` is a YAML sequence whose entries may be plain strings OR single-key
# mappings (`- data: smrnaseq`, as the bundled GF spec writes them). The editor
# shows one entry per line, so a keyed entry has to survive the trip out to a
# line and back, or saving a handler would silently strip the keys.

test_that("keyed info entries render as key: value and parse back", {
  to_lines <- app_fn(".dta_info_to_lines")
  from_lines <- app_fn(".dta_lines_to_info")

  info <- list(list(data = "smrnaseq"), list(code = "smr"))
  lines <- to_lines(info)

  expect_equal(lines, c("data: smrnaseq", "code: smr"))
  expect_equal(from_lines(lines), info)
})

test_that("plain info strings survive unchanged", {
  to_lines <- app_fn(".dta_info_to_lines")
  from_lines <- app_fn(".dta_lines_to_info")

  expect_equal(to_lines(list("just text", "more text")), c("just text", "more text"))
  expect_equal(from_lines(c("just text", "more text")), list("just text", "more text"))
})

test_that("an empty info block stays empty in both directions", {
  to_lines <- app_fn(".dta_info_to_lines")
  from_lines <- app_fn(".dta_lines_to_info")

  expect_length(to_lines(NULL), 0)
  expect_length(to_lines(list()), 0)
  expect_null(from_lines(character(0)))
  expect_null(from_lines(c("", "   ")))
})

test_that("saving a handler keeps the keys of its info entries", {
  # The GF specification is the one bundled document whose handler carries keyed
  # info, so it is the fixture that would have lost them.
  fields_fn <- app_fn("dta_handler_fields")
  set_fn <- app_fn("dta_set_handler")
  handlers_fn <- app_fn("dta_handlers")

  gf <- DTAtools::DTA(datasets = read_dataset_from_yaml(
    system.file("extdata", "gf_dataset.yaml", package = "DTAtools")
  ))
  ds_name <- names(datasets(gf))[1]

  before <- fields_fn(gf, ds_name, 1)
  expect_equal(before$info, "data: smrnaseq\ncode: smr")

  res <- do.call(set_fn, c(
    list(gf, ds_name, index = 1),
    before[c(
      "filename", "type", "pattern", "count_mode", "number_of_files",
      "min_number_of_files", "max_number_of_files", "pattern_description", "info"
    )]
  ))

  expect_true(res$ok)
  expect_equal(
    handlers_fn(datasets(res$value, ds_name))[[1]]@info,
    list(list(data = "smrnaseq"), list(code = "smr"))
  )
})

# ---- One entry per line, in both directions ---------------------------------
# dta_handler_fields() joins multi-valued fields with newlines for the text
# areas, and the text areas hand that single string straight back. Anything that
# only split a character VECTOR would fold every line into one value: two file
# names would become one impossible name, two info entries one entry.

test_that("a handler with several file names survives a fields -> set round trip", {
  fields_fn <- app_fn("dta_handler_fields")
  set_fn <- app_fn("dta_set_handler")
  handlers_fn <- app_fn("dta_handlers")

  seeded <- set_fn(app_fixture_dta(), "clinical_data",
    index = 1, filename = c("site_a.csv", "site_b.csv"), type = "csv",
    pattern = TRUE, count_mode = "exact", number_of_files = 2
  )
  expect_true(seeded$ok)

  before <- fields_fn(seeded$value, "clinical_data", 1)
  expect_equal(before$filename, "site_a.csv\nsite_b.csv")

  again <- do.call(set_fn, c(
    list(seeded$value, "clinical_data", index = 1),
    before[c(
      "filename", "type", "pattern", "count_mode", "number_of_files",
      "min_number_of_files", "max_number_of_files", "pattern_description", "info"
    )]
  ))

  expect_true(again$ok)
  expect_equal(
    handlers_fn(datasets(again$value, "clinical_data"))[[1]]@filename,
    c("site_a.csv", "site_b.csv")
  )
})

test_that("a newline-joined info block splits back into separate entries", {
  from_lines <- app_fn(".dta_lines_to_info")

  expect_equal(
    from_lines("data: smrnaseq\ncode: smr"),
    list(list(data = "smrnaseq"), list(code = "smr"))
  )
})

test_that("dta_split_lines drops blank lines and trims what is left", {
  fn <- app_fn(".dta_split_lines")

  expect_equal(fn("  a.csv \n\n  b.csv  \n"), c("a.csv", "b.csv"))
  expect_equal(fn(c("a.csv", "b.csv")), c("a.csv", "b.csv"))
  expect_length(fn(""), 0)
  expect_length(fn(NULL), 0)
})


# ---- Matching handlers across a re-parse ------------------------------------
# The Edit-files dialog knows which operation it performed. A re-parsed document
# does not: entries may have been reordered, inserted in the middle or rewritten
# in place, so upload records have to follow their OWN handler rather than the
# position they used to sit at.

test_that("dta_match_handlers follows a handler that moved", {
  set_fn <- app_fn("dta_set_handler")
  move_fn <- app_fn("dta_move_handler")
  match_fn <- app_fn("dta_match_handlers")

  before <- set_fn(app_fixture_dta(), "clinical_data",
    index = NULL, filename = "b.tsv", type = "tsv"
  )$value
  after <- move_fn(before, "clinical_data", 1, "down")$value

  expect_equal(
    unname(match_fn(
      datasets(before, "clinical_data"),
      datasets(after, "clinical_data")
    )),
    c(2L, 1L)
  )
})

test_that("dta_match_handlers reports a handler that is gone as NA", {
  set_fn <- app_fn("dta_set_handler")
  rm_fn <- app_fn("dta_remove_handler")
  match_fn <- app_fn("dta_match_handlers")

  before <- set_fn(app_fixture_dta(), "clinical_data",
    index = NULL, filename = "b.tsv", type = "tsv"
  )$value
  after <- rm_fn(before, "clinical_data", 1)$value

  expect_equal(
    unname(match_fn(
      datasets(before, "clinical_data"),
      datasets(after, "clinical_data")
    )),
    c(NA_integer_, 1L)
  )
})

test_that("dta_match_handlers survives an insertion in the middle", {
  set_fn <- app_fn("dta_set_handler")
  move_fn <- app_fn("dta_move_handler")
  match_fn <- app_fn("dta_match_handlers")

  before <- set_fn(app_fixture_dta(), "clinical_data",
    index = NULL, filename = "b.tsv", type = "tsv"
  )$value
  # Append a third and pull it into the middle: [A, B] -> [A, C, B].
  after <- set_fn(before, "clinical_data",
    index = NULL, filename = "c.csv", type = "csv"
  )$value
  after <- move_fn(after, "clinical_data", 3, "up")$value

  expect_equal(
    unname(match_fn(
      datasets(before, "clinical_data"),
      datasets(after, "clinical_data")
    )),
    c(1L, 3L)
  )
})

test_that("dta_match_handlers keeps identical handlers in order", {
  # Two entries that serialise the same cannot be told apart, so the match must
  # stay stable (1 -> 1, 2 -> 2) rather than collapsing both onto one.
  set_fn <- app_fn("dta_set_handler")
  match_fn <- app_fn("dta_match_handlers")

  twice <- set_fn(app_fixture_dta(), "clinical_data",
    index = NULL, filename = "clinical_data.*.csv$", type = "csv",
    pattern = TRUE, count_mode = "exact", number_of_files = 1
  )$value

  ds <- datasets(twice, "clinical_data")
  expect_equal(unname(match_fn(ds, ds)), c(1L, 2L))
})

test_that("dta_match_handlers on a rewritten handler reports it as gone", {
  # An entry edited in place is not the same handler any more; its file may no
  # longer be what the slot asks for, so the record must not silently follow it.
  set_fn <- app_fn("dta_set_handler")
  match_fn <- app_fn("dta_match_handlers")

  before <- app_fixture_dta()
  after <- set_fn(before, "clinical_data",
    index = 1, filename = "something_else.csv", type = "csv", pattern = FALSE
  )$value

  expect_equal(
    unname(match_fn(
      datasets(before, "clinical_data"),
      datasets(after, "clinical_data")
    )),
    NA_integer_
  )
})


# ---------------------------------------------------------------------------
# DTAFileAny / "any" type in the Shiny app helpers -- new tests
# ---------------------------------------------------------------------------

test_that("dta_handler_types() with no argument still returns exactly c('csv','tsv')", {
  fn <- app_fn("dta_handler_types")
  expect_equal(fn(), c("csv", "tsv"))
})

test_that("dta_handler_types('file') includes 'any'", {
  fn <- app_fn("dta_handler_types")
  types <- fn("file")
  expect_true("any" %in% types)
  expect_equal(types, c("any", "csv", "tsv"))
})

test_that("dta_handler_type() returns 'any' for a DTAFileAny", {
  fn <- app_fn("dta_handler_type")
  h <- DTAtools::DTAFileAny(filename = "report.pdf")
  expect_equal(fn(h), "any")
})

test_that("handler_expected() appends extensions for a DTAFileAny", {
  fn <- app_fn("handler_expected")
  h <- DTAtools::DTAFileAny(filename = ".*", pattern = TRUE, extensions = c("pdf", "zip"))
  result <- fn(h)
  expect_match(result, "pdf")
  expect_match(result, "zip")
})

test_that("dta_set_handler() rejects type='any' when dataset_type is tabular (the default)", {
  fn <- app_fn("dta_set_handler")

  res <- fn(app_fixture_dta(), "clinical_data",
    index = 1, filename = "report.pdf", type = "any"
  )
  expect_false(res$ok)
  expect_match(res$error, "must be one of")
})

test_that("a type='any' handler with extensions survives a YAML round trip", {
  set_fn <- app_fn("dta_set_handler")
  to_yaml <- app_fn("dta_to_yaml_text")
  from_yaml <- app_fn("dta_read_yaml_text")
  handlers_fn <- app_fn("dta_handlers")
  fields_fn <- app_fn("dta_handler_fields")

  # Build a file-type dataset with an "any" handler carrying extensions
  h <- DTAtools::DTAFileAny(
    filename = "^report_.*",
    pattern = TRUE,
    extensions = c("pdf", "zip")
  )
  ds <- DTAtools::DTADataSetFile(name = "reports", files = list(h))
  dta_obj <- DTAtools::DTA(datasets = list(reports = ds))

  txt <- to_yaml(dta_obj)
  expect_true(txt$ok)

  back <- from_yaml(txt$value)
  expect_true(back$ok)

  hs_back <- handlers_fn(DTAtools::datasets(back$value, "reports"))
  expect_length(hs_back, 1)
  h_back <- hs_back[[1]]
  expect_s3_class(h_back, "DTAtools::DTAFileAny")

  # Extensions must survive the round trip
  ext_back <- tryCatch(h_back@extensions, error = function(e) NULL)
  expect_equal(sort(ext_back), c("pdf", "zip"))
})

test_that("dta_handler_fields() returns extensions as a comma-separated string", {
  fn <- app_fn("dta_handler_fields")

  h <- DTAtools::DTAFileAny(filename = "report.pdf", extensions = c("pdf", "zip"))
  ds <- DTAtools::DTADataSetFile(name = "rpts", files = list(h))
  dta_obj <- DTAtools::DTA(datasets = list(rpts = ds))

  fields <- fn(dta_obj, "rpts", 1)
  # extensions field must be a comma-separated string
  expect_true(is.character(fields$extensions))
  expect_match(fields$extensions, "pdf")
  expect_match(fields$extensions, "zip")
})
