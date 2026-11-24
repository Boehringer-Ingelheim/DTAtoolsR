# The Shiny app and the second column-type backend (R): the helpers that read the
# package's type vocabulary, the column editor (preselected backend, type choices
# per backend, switching the backend), matrix cells of either backend, and the
# range-rule picker.
#
# `data_types.default` of dta-app.yaml decides ONLY which backend the app
# preselects for NEW columns and new matrix cells; it never hides the other one,
# and a column or matrix that exists keeps the backend it is written with. With
# the default (SAS) everything reads as it always did.
#
# Expected vocabularies and wordings are written out by hand here, never taken
# from what the app or the package returns.

skip_if_not_installed("shiny")

be_sas_types <- c("Char", "Num", "Int", "Date", "Time", "DateTime")
be_r_types <- c("character", "numeric", "integer", "logical", "Date", "POSIXct", "hms")

# One tabular dataset with a column of every kind, SAS and R side by side. The R
# Date has no format (the package stores its default); the SAS formats carry the
# prefix, as in every specification.
be_yaml <- c(
  "datasets:",
  "  - id: mixed",
  "    type: tabular",
  "    files: { filename: \"mixed.csv\", type: csv }",
  "    columns:",
  "      - { id: S_CHAR, type: SAS Char, length: 12, nullable: No }",
  "      - { id: S_NUM, type: SAS Num, format: \"SAS 8.2\" }",
  "      - { id: S_INT, type: SAS Int }",
  "      - { id: S_DATE, type: SAS Date, format: \"SAS DATE9.\" }",
  "      - { id: R_CHAR, type: R character, length: 12 }",
  "      - { id: R_NUM, type: R numeric }",
  "      - { id: R_INT, type: R integer }",
  "      - { id: R_LGL, type: R logical }",
  "      - { id: R_DATE, type: R Date }",
  "      - { id: R_STAMP, type: R POSIXct, format: \"R %Y-%m-%dT%H:%M:%S\" }"
)
be_dta <- function(lines = be_yaml, env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".yaml", .local_envir = env)
  writeLines(lines, path)
  suppressMessages(read_dta_from_yaml(path))
}

# ---- the vocabulary the app reads from the package ---------------------------

test_that("dta_backend_types lists the types of each backend in the package's order", {
  types <- app_fn("dta_backend_types")

  expect_identical(types("SAS"), be_sas_types)
  expect_identical(types("R"), be_r_types)
  # An unknown backend has no types; so does nothing at all.
  expect_identical(types("Python"), character(0))
  expect_identical(types(NULL), character(0))
  expect_identical(types(NA_character_), character(0))
})

test_that("the app's type lists are the package's own, for every supported backend", {
  for (b in app_fn("dta_supported_backends")()) {
    expect_identical(app_fn("dta_backend_types")(b), dta_backend_types(b), info = b)
  }
  expect_identical(app_fn("dta_sas_types")(), be_sas_types)
})

test_that("every type the app lists has the family the package gives it", {
  fam <- app_fn("dta_pkg_family_of")
  expect_identical(fam("SAS", "Char"), "text")
  expect_identical(fam("SAS", "Num"), "number")
  expect_identical(fam("SAS", "Int"), "integer")
  expect_identical(fam("R", "logical"), "logical")
  expect_identical(fam("R", "hms"), "temporal")
  expect_identical(fam("R", "Num"), NA_character_)
  # What the app reads off a structure is what it reads off the pieces.
  st <- DTAtools::DTAColumnSpecStructureFactory(type = "R integer")
  expect_identical(app_fn("dta_pkg_type_family")(st), "integer")
  expect_identical(app_fn("dta_pkg_type_family")(NULL), NA_character_)
})

test_that("the backend type map pairs types of the same family and misses only R logical", {
  m <- app_fn("dta_backend_type_map")()

  expect_identical(names(m), c("SAS", "R"))
  expect_identical(m$SAS, c("Char", "Num", "Int", "Date", "DateTime", "Time"))
  expect_identical(m$R, c("character", "numeric", "integer", "Date", "POSIXct", "hms"))
  # A pair is the same kind of column, whichever way the package names it.
  for (i in seq_len(nrow(m))) {
    expect_identical(
      dta_family_of("SAS", m$SAS[i]), dta_family_of("R", m$R[i]),
      info = paste(m$SAS[i], m$R[i])
    )
  }
  # Nothing is listed twice and nothing the package offers is left out.
  expect_setequal(m$SAS, dta_backend_types("SAS"))
  expect_setequal(c(m$R, "logical"), dta_backend_types("R"))
})

test_that("the text type of a backend is the one a new column starts as", {
  expect_identical(app_fn("dta_text_type")("SAS"), "Char")
  expect_identical(app_fn("dta_text_type")("R"), "character")
  expect_identical(app_fn("dta_type_of_family")("R", "logical"), "logical")
  expect_identical(app_fn("dta_type_of_family")("SAS", "logical"), NA_character_)
  expect_identical(app_fn("dta_type_of_family")("SAS", "number"), "Num")
  expect_identical(app_fn("dta_type_of_family")("R", "integer"), "integer")
})

# ---- the configured default ----------------------------------------------------

test_that("dta_default_backend reads data_types.default and falls back to SAS for anything else", {
  def <- app_fn("dta_default_backend")
  cfg <- function(v) list(data_types = list(default = v))

  expect_identical(def(cfg("R")), "R")
  expect_identical(def(cfg("SAS")), "SAS")
  # Whatever else a hand-built config holds, the answer is SAS and nothing throws.
  expect_identical(def(), "SAS")
  expect_identical(def(NULL), "SAS")
  expect_identical(def(list()), "SAS")
  expect_identical(def(cfg("Python")), "SAS")
  expect_identical(def(cfg("r")), "SAS")
  expect_identical(def(cfg(c("R", "SAS"))), "SAS")
  expect_identical(def(cfg(NA_character_)), "SAS")
  expect_identical(def(cfg(NULL)), "SAS")
  expect_identical(def(cfg(1)), "SAS")
  expect_identical(def(list(data_types = "R")), "SAS")
  expect_identical(def("R"), "SAS")
  expect_identical(def(c(data_types = "R")), "SAS")
  # The stock config is SAS, and so is the parsed one.
  expect_identical(def(app_fn("dta_app_config_defaults")()), "SAS")
  expect_identical(def(utils::modifyList(app_fn("dta_app_config_defaults")(), cfg("R"))), "R")
})

# ---- reading a prefixed type ---------------------------------------------------

test_that("dta_split_type reads the backend, the type and the family off a prefixed type", {
  split <- app_fn("dta_split_type")

  expect_identical(split("R numeric"), list(backend = "R", type = "numeric", family = "number"))
  expect_identical(split("SAS Char"), list(backend = "SAS", type = "Char", family = "text"))
  expect_identical(split("SAS Int")$family, "integer")
  expect_identical(split("R logical")$family, "logical")
  expect_identical(split("R POSIXct")$family, "temporal")
  # The backend may be written in any case; it comes back canonical.
  expect_identical(split("sas Num"), list(backend = "SAS", type = "Num", family = "number"))
  expect_identical(split("  r   integer ")$backend, "R")
  # A type of the other backend is a type with no family, not a guess.
  expect_identical(split("R Num"), list(backend = "R", type = "Num", family = NA_character_))
  # No backend in front, an unknown one, or nothing: all NA.
  none <- list(backend = NA_character_, type = NA_character_, family = NA_character_)
  expect_identical(split("Num"), none)
  expect_identical(split("Python numeric"), none)
  expect_identical(split(""), none)
  expect_identical(split(NULL), none)
  expect_identical(split(NA_character_), none)
})

test_that("dta_split_format takes a typed backend off the front of a format and nothing else", {
  split <- app_fn("dta_split_format")

  expect_identical(split("R %Y-%m-%d"), list(backend = "R", rest = "%Y-%m-%d"))
  expect_identical(split("  SAS 8.2"), list(backend = "SAS", rest = "8.2"))
  expect_identical(split("sas $9."), list(backend = "SAS", rest = "$9."))
  expect_identical(split("%Y-%m-%d %H:%M"), list(backend = NA_character_, rest = "%Y-%m-%d %H:%M"))
  expect_identical(split("DATE9."), list(backend = NA_character_, rest = "DATE9."))
  expect_identical(split("R"), list(backend = NA_character_, rest = "R"))
  expect_identical(split(""), list(backend = NA_character_, rest = ""))
  expect_identical(split(NULL), list(backend = NA_character_, rest = ""))
  expect_identical(split(NA_character_), list(backend = NA_character_, rest = ""))
})

# ---- the cell types of a matrix ------------------------------------------------

test_that("the matrix cell types of SAS read exactly as they always did, and R has the same three kinds", {
  cells <- app_fn("dta_matrix_cell_types")

  expect_identical(
    cells("SAS"),
    c(
      "SAS Num (decimal numbers)" = "SAS Num", "SAS Int (whole numbers)" = "SAS Int",
      "SAS Char (text)" = "SAS Char"
    )
  )
  expect_identical(
    cells("R"),
    c(
      "R numeric (decimal numbers)" = "R numeric", "R integer (whole numbers)" = "R integer",
      "R character (text)" = "R character"
    )
  )
  expect_identical(cells("Python"), character(0))
})

test_that("the layout form's conditions are built from the cell types of the matrix's own backend", {
  cond <- app_fn("dta_matrix_cell_conditions")

  expect_identical(cond("SAS")$range, "['SAS Char'].indexOf(input.mx_cell_type) < 0")
  expect_identical(cond("SAS")$infinite, "['SAS Num'].indexOf(input.mx_cell_type) >= 0")
  expect_identical(cond("R")$range, "['R character'].indexOf(input.mx_cell_type) < 0")
  expect_identical(cond("R")$infinite, "['R numeric'].indexOf(input.mx_cell_type) >= 0")
  expect_identical(
    app_fn("dta_js_one_of")("x", c("A", "B")),
    "['A', 'B'].indexOf(input.x) >= 0"
  )
})

test_that("only a SAS matrix has a cell Format field", {
  label <- app_fn("dta_matrix_cell_format_label")

  expect_identical(label("SAS"), "SAS format (e.g. 8.2)")
  expect_null(label("R"))
  expect_null(label(NA_character_))
  expect_null(label(NULL))
})

test_that("the Format placeholder and hint are per backend, and SAS shows no hint", {
  expect_identical(app_fn("dta_format_placeholder")("SAS"), "e.g. $9. / 8.2")
  expect_identical(app_fn("dta_format_placeholder")("R"), "e.g. %Y-%m-%d (Date, POSIXct, hms only)")
  expect_identical(app_fn("dta_format_placeholder")("Python"), "")
  expect_identical(app_fn("dta_format_placeholder")(NULL), "")
  expect_identical(app_fn("dta_format_hint")("SAS"), "")
  expect_match(app_fn("dta_format_hint")("R"), "Date, POSIXct and hms only", fixed = TRUE)
  expect_match(app_fn("dta_format_hint")("R"), "Length applies to character only", fixed = TRUE)
  # Every supported backend has a placeholder, so a new one cannot ship without.
  for (b in app_fn("dta_supported_backends")()) {
    expect_true(nzchar(app_fn("dta_format_placeholder")(b)), info = b)
  }
})

test_that("a format the Inspect panel names is a SAS one unless it holds a %", {
  say <- app_fn("dta_format_expected_text")

  expect_identical(say("DATE9."), "fit SAS format DATE9.")
  expect_identical(say("8.2"), "fit SAS format 8.2")
  expect_identical(say("%Y-%m-%d"), "fit R date/time format %Y-%m-%d")
  expect_identical(say(""), "fit SAS format ")
  expect_identical(say(NULL), "fit SAS format ")
})

# ---- switching the backend in the editor ---------------------------------------

test_that("switching the backend maps the type to the same kind of column", {
  sw <- app_fn("dta_switch_backend")
  to_r <- c(Char = "character", Num = "numeric", Int = "integer", Date = "Date", DateTime = "POSIXct", Time = "hms")

  for (ty in names(to_r)) {
    r <- sw("SAS", "R", ty)
    expect_identical(r$type, unname(to_r[[ty]]), info = ty)
    expect_null(r$notice, info = ty)
    # ... and back again.
    back <- sw("R", "SAS", unname(to_r[[ty]]))
    expect_identical(back$type, ty, info = ty)
    expect_null(back$notice, info = ty)
  }
})

test_that("R logical has no SAS counterpart: the text type and a notice that says so", {
  r <- app_fn("dta_switch_backend")("R", "SAS", "logical", length = "3")

  expect_identical(r$type, "Char")
  expect_identical(r$format, "")
  # Text keeps its length, so it is kept here too.
  expect_identical(r$length, "3")
  expect_match(r$notice, "logical", fixed = TRUE)
  expect_match(r$notice, "SAS Char", fixed = TRUE)
})

test_that("a switch always clears the format, and the length unless the new type is text", {
  sw <- app_fn("dta_switch_backend")

  # Whatever the types, the old format is gone: a SAS and a strptime format are
  # not interchangeable.
  expect_identical(sw("SAS", "R", "Date", "DATE9.", "")$format, "")
  expect_identical(sw("R", "SAS", "Date", "%d.%m.%Y", "")$format, "")
  expect_identical(sw("SAS", "R", "DateTime", "DATETIME20.", "")$format, "")
  expect_identical(sw("SAS", "R", "Char", "$9.", "")$format, "")

  # Text stays text and keeps its length; every other type drops it.
  expect_identical(sw("SAS", "R", "Char", "", "12")$length, "12")
  expect_identical(sw("R", "SAS", "character", "", "12")$length, "12")
  expect_identical(sw("SAS", "R", "Num", "8.2", "8")$length, "")
  expect_identical(sw("SAS", "R", "Int", "", "4")$length, "")
  expect_identical(sw("SAS", "R", "Date", "", "8")$length, "")
  expect_identical(sw("R", "SAS", "numeric", "", "8")$length, "")
})

test_that("a type the map does not know becomes the text type, with a notice only when one was chosen", {
  sw <- app_fn("dta_switch_backend")

  odd <- sw("SAS", "R", "Bool")
  expect_identical(odd$type, "character")
  expect_false(is.null(odd$notice))

  blank <- sw("SAS", "R", "")
  expect_identical(blank$type, "character")
  expect_null(blank$notice)
  expect_identical(sw("R", "SAS", NULL)$type, "Char")
})

test_that("switching to the backend the editor already has changes nothing", {
  r <- app_fn("dta_switch_backend")("R", "R", "numeric", "keep", "7")

  expect_identical(r, list(type = "numeric", format = "keep", length = "7", notice = NULL))
})

# ---- dta_set_column and dta_column_fields with R columns --------------------------

be_column <- function(dta, dataset, id) {
  DTAtools::datasets(dta, dataset)@specs@columns[[id]]
}

test_that("an R character column saves as an R column and reads back with its backend, type and length", {
  dta <- app_fixture_dta()
  res <- app_fn("dta_set_column")(
    dta, "clinical_data",
    id = "NOTE", backend = "R", type = "character", length = "12", nullable = TRUE
  )
  expect_true(res$ok, info = res$error)

  st <- be_column(res$value, "clinical_data", "NOTE")@structure
  expect_identical(c(st@backend, st@type, st@length), c("R", "character", "12"))
  f <- app_fn("dta_column_fields")(res$value, "clinical_data", "NOTE")
  expect_identical(f$backend, "R")
  expect_identical(f$type, "character")
  expect_identical(f$length, "12")
  expect_identical(f$format, "")
})

test_that("an R numeric column and a SAS one both read back, each under its own backend", {
  dta <- app_fixture_dta()
  set_col <- app_fn("dta_set_column")
  r <- set_col(dta, "clinical_data", id = "SCORE", backend = "R", type = "numeric", nullable = TRUE)
  expect_true(r$ok, info = r$error)
  s <- set_col(r$value, "clinical_data", id = "SCORE_S", backend = "SAS", type = "Num", format = "8.2", nullable = TRUE)
  expect_true(s$ok, info = s$error)

  fields <- function(id) app_fn("dta_column_fields")(s$value, "clinical_data", id)
  expect_identical(fields("SCORE")[c("backend", "type", "format", "length")], list(backend = "R", type = "numeric", format = "", length = ""))
  expect_identical(fields("SCORE_S")[c("backend", "type", "format", "length")], list(backend = "SAS", type = "Num", format = "8.2", length = ""))
})

test_that("an R Date column gets its default format stored, or keeps the one it is given", {
  dta <- app_fixture_dta()
  set_col <- app_fn("dta_set_column")
  plain <- set_col(dta, "clinical_data", id = "VISIT_ON", backend = "R", type = "Date", nullable = TRUE)
  expect_true(plain$ok, info = plain$error)
  # The package fills the default in; the form shows it without the prefix.
  expect_identical(app_fn("dta_column_fields")(plain$value, "clinical_data", "VISIT_ON")$format, "%Y-%m-%d")

  own <- set_col(dta, "clinical_data", id = "VISIT_ON", backend = "R", type = "Date", format = "%d.%m.%Y", nullable = TRUE)
  expect_true(own$ok, info = own$error)
  f <- app_fn("dta_column_fields")(own$value, "clinical_data", "VISIT_ON")
  expect_identical(c(f$backend, f$type, f$format), c("R", "Date", "%d.%m.%Y"))

  stamp <- set_col(
    dta, "clinical_data",
    id = "STAMP", backend = "R", type = "POSIXct", format = "%Y-%m-%d %H:%M:%S", nullable = TRUE
  )
  expect_true(stamp$ok, info = stamp$error)
  expect_identical(
    app_fn("dta_column_fields")(stamp$value, "clinical_data", "STAMP")$format,
    "%Y-%m-%d %H:%M:%S"
  )
})

test_that("a backend typed in front of the format is taken off before the real one is put on", {
  dta <- app_fixture_dta()
  set_col <- app_fn("dta_set_column")

  typed <- set_col(dta, "clinical_data", id = "D", backend = "R", type = "Date", format = "R %d.%m.%Y", nullable = TRUE)
  expect_true(typed$ok, info = typed$error)
  expect_identical(app_fn("dta_column_fields")(typed$value, "clinical_data", "D")$format, "%d.%m.%Y")
  st <- be_column(typed$value, "clinical_data", "D")@structure
  expect_identical(c(st@backend, st@format), c("R", "%d.%m.%Y"))

  sas <- set_col(dta, "clinical_data", id = "N", backend = "SAS", type = "Num", format = "SAS 8.2", nullable = TRUE)
  expect_true(sas$ok, info = sas$error)
  expect_identical(app_fn("dta_column_fields")(sas$value, "clinical_data", "N")$format, "8.2")

  # The same prefix, any case, still the one backend.
  lower <- set_col(dta, "clinical_data", id = "D", backend = "R", type = "Date", format = "r %d.%m.%Y", nullable = TRUE)
  expect_true(lower$ok, info = lower$error)
})

test_that("a typed backend that is not the chosen one is refused, naming both, and nothing is saved", {
  dta <- app_fixture_dta()
  before <- app_fn("dta_column_ids")(dta, "clinical_data")

  res <- app_fn("dta_set_column")(
    dta, "clinical_data",
    id = "D", backend = "R", type = "Date", format = "SAS DATE9.", nullable = TRUE
  )

  expect_false(res$ok)
  expect_match(res$error, "SAS", fixed = TRUE)
  expect_match(res$error, "backend is R", fixed = TRUE)
  expect_identical(app_fn("dta_column_ids")(dta, "clinical_data"), before)
})

test_that("what the package refuses for an R column comes back as the save error, with nothing saved", {
  dta <- app_fixture_dta()
  set_col <- app_fn("dta_set_column")
  before <- app_fn("dta_column_ids")(dta, "clinical_data")

  # A length on a number, a format on text and an unsupported directive.
  cases <- list(
    "length on numeric" = list(type = "numeric", length = "8"),
    "format on character" = list(type = "character", format = "%Y"),
    "format on numeric" = list(type = "numeric", format = "%Y-%m-%d"),
    "length on Date" = list(type = "Date", length = "10"),
    "month name" = list(type = "Date", format = "%d %B %Y"),
    "pattern on logical" = list(type = "logical", pattern = "^T")
  )
  for (nm in names(cases)) {
    res <- do.call(set_col, c(list(dta, "clinical_data", id = "BAD", backend = "R", nullable = TRUE), cases[[nm]]))
    expect_false(res$ok, info = nm)
    expect_true(nzchar(res$error), info = nm)
    expect_identical(app_fn("dta_column_ids")(dta, "clinical_data"), before, info = nm)
  }
})

test_that("an R logical column takes its allowed values as the quoted text the editor holds", {
  res <- app_fn("dta_set_column")(
    app_fixture_dta(), "clinical_data",
    id = "ALIVE", backend = "R", type = "logical", values = c("TRUE", "FALSE"), nullable = TRUE
  )

  expect_true(res$ok, info = res$error)
  f <- app_fn("dta_column_fields")(res$value, "clinical_data", "ALIVE")
  expect_identical(f$type, "logical")
  expect_identical(f$values, "TRUE\nFALSE")
})

test_that("a SAS column saves and reads back exactly as it did, whichever default the library has", {
  dta <- app_fixture_dta()
  res <- app_fn("dta_set_column")(
    dta, "clinical_data",
    id = "REMARK", type = "Char", length = "20", nullable = TRUE
  )
  expect_true(res$ok, info = res$error)
  f <- app_fn("dta_column_fields")(res$value, "clinical_data", "REMARK")
  expect_identical(f$backend, "SAS")
  expect_identical(f$type, "Char")
  expect_identical(f$length, "20")
})

test_that("dta_column_fields reads an existing R column of the loaded document", {
  dta <- be_dta()
  fields <- function(id) app_fn("dta_column_fields")(dta, "mixed", id)

  expect_identical(fields("R_CHAR")[c("backend", "type", "length")], list(backend = "R", type = "character", length = "12"))
  expect_identical(fields("R_LGL")[c("backend", "type")], list(backend = "R", type = "logical"))
  expect_identical(fields("R_STAMP")[c("backend", "type", "format")], list(backend = "R", type = "POSIXct", format = "%Y-%m-%dT%H:%M:%S"))
  expect_identical(fields("S_NUM")[c("backend", "type", "format")], list(backend = "SAS", type = "Num", format = "8.2"))
})

# ---- the range-rule picker -----------------------------------------------------

test_that("a range rule is offered the number and integer columns of both backends", {
  dta <- be_dta()

  expect_setequal(
    app_fn("dta_numeric_column_ids")(dta, "mixed"),
    c("S_NUM", "S_INT", "R_NUM", "R_INT")
  )
})

# ---- adding a matrix dataset -----------------------------------------------------

test_that("Add dataset builds an R matrix: the chosen cells and R character row labels", {
  add <- app_fn("dta_add_dataset")
  base <- app_fixture_dta()

  for (ct in c("R numeric", "R integer", "R character")) {
    r <- add(base, "mx", type = "matrix", row_label = "GENE_ID", header_mode = "id", cell_type = ct)
    expect_true(r$ok, info = paste(ct, r$error))
    ds <- r$value@datasets[["mx"]]
    col <- be_column(r$value, "mx", "GENE_ID")
    expect_identical(c(col@structure@backend, col@structure@type), c("R", "character"), info = ct)
    expect_false(col@nullable, info = ct)
    expect_identical(as.list(ds@matrix)$cells$type, ct)
    expect_identical(as.character(ds@matrix@row_key), "GENE_ID")
    # The dataset as saved names no SAS type anywhere, and reads back as it was.
    txt <- app_fn("dta_dataset_to_yaml_text")(r$value, "mx")
    expect_true(txt$ok, info = ct)
    expect_match(txt$value, paste0("type: ", ct), fixed = TRUE)
    expect_match(txt$value, "type: R character", fixed = TRUE)
    expect_no_match(txt$value, "SAS", fixed = TRUE)
  }
})

test_that("Add dataset with SAS cells is what it always was, SAS Char row labels included", {
  r <- app_fn("dta_add_dataset")(
    app_fixture_dta(), "mx",
    type = "matrix", row_label = "GENE_ID", header_mode = "id", cell_type = "SAS Num"
  )
  expect_true(r$ok, info = r$error)
  col <- be_column(r$value, "mx", "GENE_ID")
  expect_identical(c(col@structure@backend, col@structure@type), c("SAS", "Char"))
  expect_identical(as.list(r$value@datasets[["mx"]]@matrix)$cells$type, "SAS Num")
})

test_that("Add dataset refuses a cell type no matrix can hold, with the sentence it always used", {
  add <- app_fn("dta_add_dataset")
  base <- app_fixture_dta()

  for (ct in list("R logical", "R Date", "R POSIXct", "R hms", "SAS Date", "SAS Time", "Num", "numeric", "Python numeric", "", NULL, NA)) {
    r <- add(base, "mx", type = "matrix", row_label = "G", header_mode = "id", cell_type = ct)
    expect_false(r$ok, info = format(ct))
    expect_match(r$error, "Choose the cell type", fixed = TRUE, info = format(ct))
  }
  expect_false("mx" %in% names(base@datasets))
})

# ---- the matrix layout form with R cells -----------------------------------------

be_matrix_yaml <- function(cells = "{ type: R numeric, nullable: No }", labels = "R character") {
  c(
    "datasets:",
    "  - id: mx",
    "    type: matrix",
    "    files: { filename: \"mx.tsv\", type: tsv }",
    "    row_labels:",
    sprintf("      - { id: GENE_ID, type: %s, nullable: No }", labels),
    "    matrix:",
    "      rows: { key: GENE_ID }",
    paste0("      cells: ", cells)
  )
}
be_matrix_dta <- function(cells = "{ type: R numeric, nullable: No }", labels = "R character", env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".yaml", .local_envir = env)
  writeLines(be_matrix_yaml(cells, labels), path)
  suppressMessages(read_dta_from_yaml(path))
}
be_mx_cells <- function(dta, id = "mx") as.list(dta@datasets[[id]]@matrix)$cells

test_that("the layout form of an R matrix carries the R cell type and no prefix of the other backend", {
  dta <- be_matrix_dta()
  f <- app_fn("dta_matrix_fields")(dta, "mx")

  expect_identical(f$cell_type, "R numeric")
  expect_identical(f$cell_format, "")
  # A bare type, which only an older object could hold, is still a SAS one.
  expect_identical(app_fn("dta_matrix_fields")(be_matrix_dta("{ type: SAS Int }", "SAS Char"), "mx")$cell_type, "SAS Int")
})

test_that("opening an R matrix's layout and saving it unchanged keeps R numeric and never writes SAS R", {
  dta <- be_matrix_dta("{ type: R numeric, nullable: No, range: { min: 0 }, infinite: true }")
  r <- app_fn("dta_set_matrix")(dta, "mx", list())

  expect_true(r$ok, info = r$error)
  expect_identical(be_mx_cells(r$value), be_mx_cells(dta))
  expect_identical(be_mx_cells(r$value)$type, "R numeric")
  # Inf allowed is a setting of the number family, R numeric included: an
  # unrelated edit must not take it away or report it as dropped.
  expect_true(be_mx_cells(r$value)$infinite)
  expect_identical(r$dropped, character(0))
  txt <- app_fn("dta_to_yaml_text")(r$value)
  expect_no_match(txt$value, "SAS", fixed = TRUE)
})

test_that("saving the layout of each R cell type unchanged writes it back as it was", {
  for (cells in c("{ type: R integer, nullable: No }", "{ type: R character, nullable: No, length: 3 }")) {
    dta <- be_matrix_dta(cells)
    r <- app_fn("dta_set_matrix")(dta, "mx", list())
    expect_true(r$ok, info = paste(cells, r$error))
    expect_identical(be_mx_cells(r$value), be_mx_cells(dta), info = cells)
  }
})

test_that("an R cell type change drops what the new type cannot carry and reports it", {
  save <- app_fn("dta_set_matrix")

  # R numeric -> R character: the range goes and is reported; Inf was off.
  num <- be_matrix_dta("{ type: R numeric, nullable: No, range: { min: 0, max: 9 }, infinite: false }")
  r <- save(num, "mx", list(cell_type = "R character"))
  expect_true(r$ok, info = r$error)
  expect_identical(r$dropped, "range")
  cells <- be_mx_cells(r$value)
  expect_identical(cells$type, "R character")
  expect_null(cells$range)
  expect_null(cells$infinite)

  # R numeric with Inf allowed -> R integer: Inf is reported.
  inf <- be_matrix_dta("{ type: R numeric, nullable: No, infinite: true }")
  r <- save(inf, "mx", list(cell_type = "R integer"))
  expect_true(r$ok, info = r$error)
  expect_identical(r$dropped, "Inf allowed")
  expect_identical(be_mx_cells(r$value)$type, "R integer")

  # R character with a length -> R numeric: only text takes a length.
  chr <- be_matrix_dta("{ type: R character, nullable: No, length: 3 }")
  r <- save(chr, "mx", list(cell_type = "R numeric"))
  expect_true(r$ok, info = r$error)
  expect_identical(r$dropped, "length")
  expect_null(be_mx_cells(r$value)$length)
  expect_identical(be_mx_cells(r$value)$type, "R numeric")

  # R integer -> R numeric keeps the range, and Inf is a number's setting again.
  int <- be_matrix_dta("{ type: R integer, nullable: No, range: { min: 1 } }")
  r <- save(int, "mx", list(cell_type = "R numeric"))
  expect_true(r$ok, info = r$error)
  expect_identical(r$dropped, character(0))
  expect_equal(be_mx_cells(r$value)$range$min, 1)
})

test_that("allowed values of an R cell follow its type and name both backends' sentence on a non-number", {
  save <- app_fn("dta_set_matrix")
  num <- be_matrix_dta()

  ok <- save(num, "mx", list(cell_values = "1\n2.5"))
  expect_true(ok$ok, info = ok$error)
  expect_equal(unlist(be_mx_cells(ok$value)$values), c(1, 2.5))

  bad <- save(num, "mx", list(cell_values = "abc"))
  expect_false(bad$ok)
  expect_match(bad$error, "must be numbers", fixed = TRUE)
  expect_match(bad$error, "R numeric and R integer", fixed = TRUE)

  # Untouched allowed values follow a cell type change, as for SAS.
  txt <- be_matrix_dta("{ type: R character, nullable: No, values: [\"1\", \"2\"] }")
  r <- save(txt, "mx", list(cell_type = "R integer"))
  expect_true(r$ok, info = r$error)
  expect_equal(unlist(be_mx_cells(r$value)$values), c(1, 2))
  word <- be_matrix_dta("{ type: R character, nullable: No, values: [\"a\"] }")
  refused <- save(word, "mx", list(cell_type = "R numeric"))
  expect_false(refused$ok)
  expect_match(refused$error, "R numeric and R integer", fixed = TRUE)
})

test_that("the SAS sentence for values that are not numbers is the one it always was", {
  dta <- be_matrix_dta("{ type: SAS Num, nullable: No }", "SAS Char")
  bad <- app_fn("dta_set_matrix")(dta, "mx", list(cell_values = "abc"))

  expect_false(bad$ok)
  expect_identical(bad$error, "Allowed cell values must be numbers for SAS Num and SAS Int cells.")
})

test_that("a cell type is taken with the backend it is written with, a bare one as SAS, anything else refused", {
  save <- app_fn("dta_set_matrix")
  dta <- be_matrix_dta()

  r <- save(dta, "mx", list(cell_type = "R integer"))
  expect_true(r$ok, info = r$error)
  expect_identical(be_mx_cells(r$value)$type, "R integer")

  # Not a type of this form: no logical, no temporal, no unknown backend.
  for (ct in c("R logical", "R Date", "SAS Date", "Python numeric", "")) {
    bad <- save(dta, "mx", list(cell_type = ct))
    expect_false(bad$ok, info = ct)
    expect_match(bad$error, "Choose the cell type", fixed = TRUE, info = ct)
  }
  # A bare type is a SAS one, as it always was (and the SAS length is kept).
  sas <- be_matrix_dta("{ type: SAS Char, nullable: No, length: 3 }", "SAS Char")
  bare <- save(sas, "mx", list(cell_type = "Int"))
  expect_true(bare$ok, info = bare$error)
  expect_identical(be_mx_cells(bare$value)$type, "SAS Int")
  expect_equal(be_mx_cells(bare$value)$length, 3)
})

test_that("a SAS cell format is written with the SAS prefix, and an R cell's refusal is the package's", {
  sas <- app_fn("dta_set_matrix")(be_matrix_dta("{ type: SAS Num, nullable: No }", "SAS Char"), "mx", list(cell_format = "8.2"))
  expect_true(sas$ok, info = sas$error)
  expect_identical(be_mx_cells(sas$value)$format, "SAS 8.2")

  # The form has no such field for R; a direct call is refused, saving nothing.
  r <- app_fn("dta_set_matrix")(be_matrix_dta(), "mx", list(cell_format = "8.2"))
  expect_false(r$ok)
  expect_true(nzchar(r$error))
})

# ---- a document with R columns, as the app serialises it ----------------------------

test_that("a loaded R document serialises with its R types and the default format it was given", {
  dta <- be_dta()
  txt <- app_fn("dta_to_yaml_text")(dta)
  expect_true(txt$ok, info = txt$error)

  expect_match(txt$value, "type: R character", fixed = TRUE)
  expect_match(txt$value, "type: R logical", fixed = TRUE)
  expect_match(txt$value, "type: R POSIXct", fixed = TRUE)
  expect_match(txt$value, "format: R %Y-%m-%dT%H:%M:%S", fixed = TRUE)
  # The R Date had no format; the package stored its default.
  expect_match(txt$value, "type: R Date", fixed = TRUE)
  expect_match(txt$value, "format: R %Y-%m-%d\n", fixed = TRUE)
  expect_match(txt$value, "type: SAS Num", fixed = TRUE)

  # And it reads back as the same document.
  again <- be_dta(strsplit(txt$value, "\n", fixed = TRUE)[[1]])
  expect_identical(
    vapply(DTAtools::datasets(again, "mixed")@specs@columns, function(c) as.list(c@structure)$type, ""),
    vapply(DTAtools::datasets(dta, "mixed")@specs@columns, function(c) as.list(c@structure)$type, "")
  )
})

test_that("a freshly loaded R column is not a change: the version summary diffs it against its own text as clean", {
  # The baseline of a version summary is the text as loaded, re-read like any
  # other; the R Date in it has no format, the document in memory has the stored
  # default. Both sides come out of the package the same way, so nothing differs.
  dta <- be_dta()
  baseline <- be_yaml
  parsed <- app_fn("dta_read_yaml_text")(paste(baseline, collapse = "\n"))
  expect_true(parsed$ok, info = parsed$error)

  diff <- app_fn("dta_diff")(parsed$value, dta)
  rows <- vapply(diff, function(d) if (is.data.frame(d)) nrow(d) else 0L, integer(1))
  expect_identical(sum(rows), 0L)

  # ... and so is the document after the app's own serialise and re-read.
  txt <- app_fn("dta_to_yaml_text")(dta)
  again <- app_fn("dta_read_yaml_text")(txt$value)
  rows2 <- vapply(app_fn("dta_diff")(again$value, dta), function(d) if (is.data.frame(d)) nrow(d) else 0L, integer(1))
  expect_identical(sum(rows2), 0L)
})

# ==== the column editor, the matrix dialogs and the rule picker, in a server ==========

skip_if_not_installed("bslib")
skip_if_not_installed("DT")
skip_if_not_installed("shinyjs")

# The options of the <select> `id` in rendered HTML, and the one selected.
be_select <- function(html, id) {
  m <- regmatches(html, regexpr(sprintf('(?s)<select id="%s".*?</select>', id), html, perl = TRUE))
  expect_length(m, 1L)
  opts <- regmatches(m, gregexpr('<option value="[^"]*"( selected)?', m))[[1]]
  value <- function(o) sub('^<option value="([^"]*)".*$', "\\1", o)
  list(choices = value(opts), selected = value(opts[grepl(" selected$", opts)]))
}
# The placeholder / the value of the <input> `id`.
be_attr <- function(html, id, attr) {
  m <- regmatches(html, regexpr(sprintf('<input id="%s"[^>]*>', id), html))
  expect_length(m, 1L)
  a <- regmatches(m, regexpr(sprintf('%s="[^"]*"', attr), m))
  if (length(a) == 0L) NA_character_ else sub(sprintf('^%s="(.*)"$', attr), "\\1", a)
}
be_upload <- function(lines = be_yaml, env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".yaml", .local_envir = env)
  writeLines(lines, path)
  app_upload_value(path, "doc.yaml")
}
# Every test below starts from the library `lines` makes the app config of.
be_library <- function(default = NULL, env = parent.frame()) {
  with_app_library(
    if (is.null(default)) "# all defaults" else c("data_types:", paste0("  default: ", default)),
    .local_envir = env
  )
}

test_that("with the stock config a NEW column starts as SAS Char, and both backends are on offer", {
  be_library()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = be_upload())
    unlock_editing(session)
    session$setInputs(edit_cols = 1)
    session$setInputs(col_add = 1)
    html <- ui_text(output$col_modal_body)

    backend <- be_select(html, "col_backend")
    expect_identical(backend$choices, c("SAS", "R"))
    expect_identical(backend$selected, "SAS")
    type <- be_select(html, "col_type")
    expect_identical(type$choices, be_sas_types)
    expect_identical(type$selected, "Char")
    expect_identical(be_attr(html, "col_format", "placeholder"), "e.g. $9. / 8.2")
    # SAS shows no hint, as before.
    expect_no_match(html, "Date, POSIXct and hms only", fixed = TRUE)
    expect_identical(col_shown_backend(), "SAS")
  })
})

test_that("data_types.default: R starts a NEW column as R character, with the R format placeholder and hint", {
  be_library("R")

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = be_upload())
    unlock_editing(session)
    session$setInputs(edit_cols = 1)
    session$setInputs(col_add = 1)
    html <- ui_text(output$col_modal_body)

    backend <- be_select(html, "col_backend")
    # The other backend is not hidden, only not preselected.
    expect_identical(backend$choices, c("SAS", "R"))
    expect_identical(backend$selected, "R")
    type <- be_select(html, "col_type")
    expect_identical(type$choices, be_r_types)
    expect_identical(type$selected, "character")
    expect_identical(be_attr(html, "col_format", "placeholder"), "e.g. %Y-%m-%d (Date, POSIXct, hms only)")
    expect_match(html, "Date, POSIXct and hms only", fixed = TRUE)
    expect_identical(col_shown_backend(), "R")
  })
})

test_that("an existing column keeps its own backend, whichever the library prefers", {
  for (default in c("SAS", "R")) {
    be_library(default)

    shiny::testServer(app_server_dir(), {
      session$setInputs(dta_file = be_upload())
      unlock_editing(session)
      session$setInputs(edit_cols = 1)
      ids <- app_fn("dta_column_ids")(rv$dta, "mixed")

      session$setInputs(col_edit_click = match("S_NUM", ids))
      html <- ui_text(output$col_modal_body)
      expect_identical(be_select(html, "col_backend")$selected, "SAS", info = default)
      expect_identical(be_select(html, "col_type")$choices, be_sas_types, info = default)
      expect_identical(be_select(html, "col_type")$selected, "Num", info = default)
      expect_identical(be_attr(html, "col_format", "value"), "8.2", info = default)

      session$setInputs(col_back = 1)
      session$setInputs(col_edit_click = match("R_STAMP", ids))
      html <- ui_text(output$col_modal_body)
      expect_identical(be_select(html, "col_backend")$selected, "R", info = default)
      expect_identical(be_select(html, "col_type")$choices, be_r_types, info = default)
      expect_identical(be_select(html, "col_type")$selected, "POSIXct", info = default)
      expect_identical(be_attr(html, "col_format", "value"), "%Y-%m-%dT%H:%M:%S", info = default)

      session$setInputs(col_back = 1)
      session$setInputs(col_edit_click = match("R_LGL", ids))
      html <- ui_text(output$col_modal_body)
      expect_identical(be_select(html, "col_type")$selected, "logical", info = default)
    })
  }
})

be_typeless_yaml <- c(
  "datasets:", "  - id: d", "    type: tabular",
  "    files: { filename: \"d.csv\", type: csv }",
  "    columns:",
  "      - { id: UNTYPED, nullable: Yes }",
  "      - { id: SAS_TYPED, type: SAS Num }",
  "      - { id: R_TYPED, type: R numeric }"
)

test_that("a column with no declared type opens in the library's default backend, as its text type", {
  for (case in list(
    list(default = "R", backend = "R", types = be_r_types, type = "character"),
    list(default = "SAS", backend = "SAS", types = be_sas_types, type = "Char")
  )) {
    be_library(case$default)

    shiny::testServer(app_server_dir(), {
      session$setInputs(dta_file = be_upload(be_typeless_yaml))
      unlock_editing(session)
      session$setInputs(edit_cols = 1)
      session$setInputs(col_edit_click = 1)
      html <- ui_text(output$col_modal_body)

      expect_identical(be_select(html, "col_backend")$selected, case$backend, info = case$default)
      expect_identical(be_select(html, "col_type")$choices, case$types, info = case$default)
      expect_identical(be_select(html, "col_type")$selected, case$type, info = case$default)

      # Editing only its label saves what the form shows: the library's backend.
      session$setInputs(
        col_id = "UNTYPED", col_label = "Now labelled", col_backend = case$backend,
        col_type = case$type, col_format = "", col_length = "", col_nullable = TRUE,
        col_values = "", col_pattern = "", col_examples = "", col_desc = "", col_save = 1
      )
      expect_null(rv$col_msg)
      st <- be_column(rv$dta, "d", "UNTYPED")@structure
      expect_identical(c(st@backend, st@type), c(case$backend, case$type), info = case$default)

      # A column that has a type keeps its own backend, whichever the default.
      session$setInputs(col_edit_click = 2)
      html <- ui_text(output$col_modal_body)
      expect_identical(be_select(html, "col_backend")$selected, "SAS", info = case$default)
      expect_identical(be_select(html, "col_type")$selected, "Num", info = case$default)
      session$setInputs(col_edit_click = 3)
      html <- ui_text(output$col_modal_body)
      expect_identical(be_select(html, "col_backend")$selected, "R", info = case$default)
      expect_identical(be_select(html, "col_type")$selected, "numeric", info = case$default)
    })
  }
})

test_that("the fields of a column with no type take the backend they are given; a legacy bare type stays SAS", {
  dta <- be_dta(be_typeless_yaml)
  fields <- function(id, ...) app_fn("dta_column_fields")(dta, "d", id, ...)

  expect_identical(fields("UNTYPED")[c("backend", "type")], list(backend = "SAS", type = ""))
  expect_identical(
    fields("UNTYPED", default_backend = "R")[c("backend", "type")],
    list(backend = "R", type = "")
  )
  # A column with a type keeps its own, whatever default is passed.
  expect_identical(fields("SAS_TYPED", default_backend = "R")[c("backend", "type")], list(backend = "SAS", type = "Num"))
  expect_identical(fields("R_TYPED", default_backend = "SAS")[c("backend", "type")], list(backend = "R", type = "numeric"))
  # No such column is still NULL.
  expect_null(fields("MISSING", default_backend = "R"))

  # A type with no backend in front (only an older object could hold one) is a
  # SAS type, whatever default is passed.
  # The app's environment is shared and read-only, so the copy of the function
  # that sees the stand-in gets a child of it.
  scope <- new.env(parent = app_env())
  scope$dta_column_to_list <- function(col) list(id = "UNTYPED", type = "Char")
  legacy <- app_fn("dta_column_fields")
  environment(legacy) <- scope
  expect_identical(
    legacy(dta, "d", "UNTYPED", default_backend = "R")[c("backend", "type")],
    list(backend = "SAS", type = "Char")
  )
})

test_that("the echo of a render, and a re-opened editor's stale inputs, are not a backend switch", {
  be_library("SAS")

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = be_upload())
    unlock_editing(session)
    session$setInputs(edit_cols = 1)
    ids <- app_fn("dta_column_ids")(rv$dta, "mixed")

    # An R column is opened and the client reports its form.
    session$setInputs(col_edit_click = match("R_NUM", ids))
    invisible(ui_text(output$col_modal_body))
    session$setInputs(col_backend = "R", col_type = "numeric", col_format = "", col_length = "")
    expect_identical(col_shown_backend(), "R")

    # A SAS column with a format is opened next. The server still holds the R
    # column's inputs, and the client reports the SAS select it has just rendered.
    session$setInputs(col_back = 1)
    session$setInputs(col_edit_click = match("S_NUM", ids))
    html <- ui_text(output$col_modal_body)
    token <- rv$col_token
    expect_identical(col_shown_backend(), "SAS")
    session$setInputs(col_backend = "SAS", col_type = "Num")

    # Nothing was cleared, nothing re-rendered, the prefill is the column's own.
    expect_identical(rv$col_token, token)
    expect_identical(rv$col_prefill$backend, "SAS")
    expect_identical(rv$col_prefill$type, "Num")
    expect_identical(rv$col_prefill$format, "8.2")
    expect_identical(be_attr(ui_text(output$col_modal_body), "col_format", "value"), "8.2")

    # Add column, with the previous column's R inputs still held by the server:
    # the form is built from the (empty) prefill and the library's SAS default.
    session$setInputs(col_backend = "R", col_type = "integer")
    session$setInputs(col_back = 1)
    session$setInputs(col_add = 1)
    html <- ui_text(output$col_modal_body)
    expect_identical(be_select(html, "col_backend")$selected, "SAS")
    expect_identical(be_select(html, "col_type")$selected, "Char")
    expect_identical(be_select(html, "col_type")$choices, be_sas_types)
    token <- rv$col_token
    session$setInputs(col_backend = "SAS", col_type = "Char")
    expect_identical(rv$col_token, token)
    expect_identical(rv$col_prefill, list())
  })
})

test_that("closing and re-opening the editor shows the library's default again, not the last form's backend", {
  be_library("R")

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = be_upload())
    unlock_editing(session)
    ids <- app_fn("dta_column_ids")(rv$dta, "mixed")

    # A SAS column is edited, then the editor is closed.
    session$setInputs(edit_cols = 1)
    session$setInputs(col_edit_click = match("S_INT", ids))
    invisible(ui_text(output$col_modal_body))
    session$setInputs(col_backend = "SAS", col_type = "Int", col_format = "", col_length = "")

    # Opened again: the list view first, and a new column built from the default.
    session$setInputs(edit_cols = 2)
    expect_identical(rv$col_view, "list")
    session$setInputs(col_add = 1)
    html <- ui_text(output$col_modal_body)
    expect_identical(be_select(html, "col_backend")$selected, "R")
    expect_identical(be_select(html, "col_type")$selected, "character")
    expect_identical(be_select(html, "col_type")$choices, be_r_types)
    expect_identical(rv$col_prefill, list())
  })
})

test_that("switching the backend remaps the type, clears format and length, and keeps everything else typed", {
  be_library("SAS")

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = be_upload())
    unlock_editing(session)
    session$setInputs(edit_cols = 1)
    session$setInputs(col_add = 1)
    invisible(ui_text(output$col_modal_body))

    # The user fills the form, then picks R.
    session$setInputs(
      col_id = "SCORE", col_label = "A score", col_type = "Num", col_format = "8.2",
      col_length = "8", col_nullable = FALSE, col_values = "", col_pattern = "",
      col_examples = "1", col_desc = "typed"
    )
    session$setInputs(col_backend = "R")

    expect_identical(rv$col_prefill$backend, "R")
    expect_identical(rv$col_prefill$type, "numeric")
    expect_identical(rv$col_prefill$format, "")
    expect_identical(rv$col_prefill$length, "")
    expect_identical(rv$col_prefill$id, "SCORE")
    expect_identical(rv$col_prefill$label, "A score")
    expect_false(rv$col_prefill$nullable)
    expect_identical(rv$col_prefill$examples, "1")
    expect_identical(rv$col_prefill$description, "typed")

    # The form is rebuilt for R: its types, its placeholder, its hint.
    html <- ui_text(output$col_modal_body)
    expect_identical(be_select(html, "col_backend")$selected, "R")
    expect_identical(be_select(html, "col_type")$choices, be_r_types)
    expect_identical(be_select(html, "col_type")$selected, "numeric")
    expect_identical(be_attr(html, "col_format", "placeholder"), "e.g. %Y-%m-%d (Date, POSIXct, hms only)")
    expect_identical(be_attr(html, "col_id", "value"), "SCORE")
    expect_match(html, "Date, POSIXct and hms only", fixed = TRUE)
    expect_identical(col_shown_backend(), "R")

    # ... and back: the same kind of column in SAS, and the hint is gone.
    session$setInputs(col_type = "numeric", col_format = "", col_length = "")
    session$setInputs(col_backend = "SAS")
    expect_identical(rv$col_prefill$backend, "SAS")
    expect_identical(rv$col_prefill$type, "Num")
    html <- ui_text(output$col_modal_body)
    expect_identical(be_select(html, "col_type")$choices, be_sas_types)
    expect_identical(be_select(html, "col_type")$selected, "Num")
    expect_no_match(html, "Date, POSIXct and hms only", fixed = TRUE)
  })
})

test_that("switching a text column's backend keeps its length; a date column's format does not survive", {
  be_library("SAS")

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = be_upload())
    unlock_editing(session)
    session$setInputs(edit_cols = 1)
    ids <- app_fn("dta_column_ids")(rv$dta, "mixed")

    session$setInputs(col_edit_click = match("S_CHAR", ids))
    invisible(ui_text(output$col_modal_body))
    session$setInputs(col_id = "S_CHAR", col_type = "Char", col_format = "", col_length = "12")
    session$setInputs(col_backend = "R")
    expect_identical(rv$col_prefill$type, "character")
    expect_identical(rv$col_prefill$length, "12")

    session$setInputs(col_back = 1)
    session$setInputs(col_edit_click = match("S_DATE", ids))
    invisible(ui_text(output$col_modal_body))
    session$setInputs(col_id = "S_DATE", col_type = "Date", col_format = "DATE9.", col_length = "")
    session$setInputs(col_backend = "R")
    expect_identical(rv$col_prefill$type, "Date")
    expect_identical(rv$col_prefill$format, "")
  })
})

test_that("R logical switched to SAS becomes SAS Char and the author is told", {
  be_library("R")
  notes <- character(0)
  testthat::local_mocked_bindings(
    showNotification = function(ui, ...) {
      notes <<- c(notes, as.character(ui))
      invisible("id")
    },
    .package = "shiny"
  )

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = be_upload())
    unlock_editing(session)
    session$setInputs(edit_cols = 1)
    ids <- app_fn("dta_column_ids")(rv$dta, "mixed")
    session$setInputs(col_edit_click = match("R_LGL", ids))
    invisible(ui_text(output$col_modal_body))
    session$setInputs(col_id = "R_LGL", col_type = "logical", col_format = "", col_length = "")
    notes <<- character(0)

    session$setInputs(col_backend = "SAS")

    expect_identical(rv$col_prefill$backend, "SAS")
    expect_identical(rv$col_prefill$type, "Char")
    expect_length(notes, 1L)
    expect_match(notes, "logical", fixed = TRUE)
    expect_match(notes, "SAS Char", fixed = TRUE)
  })
})

test_that("saving from the editor writes the backend the form holds, R and SAS side by side", {
  be_library("R")

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = be_upload())
    unlock_editing(session)
    session$setInputs(edit_cols = 1)
    session$setInputs(col_add = 1)
    invisible(ui_text(output$col_modal_body))
    form <- list(
      col_label = "", col_nullable = TRUE, col_values = "", col_pattern = "",
      col_examples = "", col_desc = ""
    )
    save <- function(...) {
      do.call(session$setInputs, c(form, list(...)))
      session$setInputs(col_save = rv$col_token + 1000)
    }

    save(col_id = "N", col_backend = "R", col_type = "numeric", col_format = "", col_length = "")
    expect_null(rv$col_msg)
    st <- be_column(rv$dta, "mixed", "N")@structure
    expect_identical(c(st@backend, st@type), c("R", "numeric"))

    # A typed prefix on the format is taken off; the Date keeps the format.
    session$setInputs(col_add = 2)
    save(col_id = "D", col_backend = "R", col_type = "Date", col_format = "R %d.%m.%Y", col_length = "")
    expect_null(rv$col_msg)
    st <- be_column(rv$dta, "mixed", "D")@structure
    expect_identical(c(st@backend, st@type, st@format), c("R", "Date", "%d.%m.%Y"))

    # A SAS column from the same form, in a library that prefers R.
    session$setInputs(col_add = 3)
    save(col_id = "S", col_backend = "SAS", col_type = "Char", col_format = "", col_length = "9")
    expect_null(rv$col_msg)
    st <- be_column(rv$dta, "mixed", "S")@structure
    expect_identical(c(st@backend, st@type, st@length), c("SAS", "Char", "9"))

    # What the package refuses is the editor's error, and nothing is added.
    n <- length(app_fn("dta_column_ids")(rv$dta, "mixed"))
    session$setInputs(col_add = 4)
    save(col_id = "BAD", col_backend = "R", col_type = "numeric", col_format = "", col_length = "8")
    expect_false(isTRUE(rv$col_msg$ok))
    expect_true(nzchar(rv$col_msg$error))
    expect_length(app_fn("dta_column_ids")(rv$dta, "mixed"), n)
  })
})

# ---- Add dataset and the matrix layout ------------------------------------------------

test_that("the Add dataset dialog offers the cell types of the library's default backend", {
  for (default in c("SAS", "R")) {
    be_library(default)

    shiny::testServer(app_server_dir(), {
      session$setInputs(dta_file = be_upload())
      unlock_editing(session)
      session$setInputs(add_dataset_open = 1)
      html <- ui_text(output$add_ds_body)

      sas <- default == "SAS"
      values <- if (sas) c("SAS Num", "SAS Int", "SAS Char") else c("R numeric", "R integer", "R character")
      labels <- if (sas) {
        c("SAS Num (decimal numbers)", "SAS Int (whole numbers)", "SAS Char (text)")
      } else {
        c("R numeric (decimal numbers)", "R integer (whole numbers)", "R character (text)")
      }
      radios <- regmatches(html, regexpr('(?s)<div id="add_ds_mx_cells".*?</div>\\s*</div>', html, perl = TRUE))
      expect_length(radios, 1L)
      for (i in 1:3) {
        expect_match(radios, sprintf('value="%s"', values[i]), fixed = TRUE, info = default)
        expect_match(radios, labels[i], fixed = TRUE, info = default)
      }
      # Nothing is preselected, and only the default backend's cells are listed.
      expect_no_match(radios, "checked", fixed = TRUE)
      other <- if (sas) "R character" else "SAS Char"
      expect_no_match(radios, other, fixed = TRUE)
    })
  }
})

test_that("a matrix added in the app takes the cells the form sends, either backend, with row labels of the same one", {
  be_library("SAS")

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = be_upload())
    unlock_editing(session)

    session$setInputs(
      add_ds_id = "mx_s", add_ds_type = "matrix", add_ds_mx_label = "G",
      add_ds_mx_header = "id", add_ds_mx_cells = "SAS Int", add_ds_save = 1
    )
    expect_true("mx_s" %in% names(rv$dta@datasets))
    expect_identical(as.list(rv$dta@datasets$mx_s@matrix)$cells$type, "SAS Int")
    expect_identical(be_column(rv$dta, "mx_s", "G")@structure@backend, "SAS")

    # The library prefers SAS, but nothing stops the other backend over the wire.
    session$setInputs(
      add_ds_id = "mx_r", add_ds_type = "matrix", add_ds_mx_label = "G",
      add_ds_mx_header = "id", add_ds_mx_cells = "R character", add_ds_save = 2
    )
    expect_true("mx_r" %in% names(rv$dta@datasets))
    expect_identical(as.list(rv$dta@datasets$mx_r@matrix)$cells$type, "R character")
    st <- be_column(rv$dta, "mx_r", "G")@structure
    expect_identical(c(st@backend, st@type), c("R", "character"))
  })
})

be_two_matrices <- c(
  "datasets:",
  "  - id: mx_r",
  "    type: matrix",
  "    files: { filename: \"mx_r.tsv\", type: tsv }",
  "    row_labels:",
  "      - { id: GENE_ID, type: R character, nullable: No }",
  "    matrix:",
  "      rows: { key: GENE_ID }",
  "      cells: { type: R numeric, nullable: No, range: { min: 0 }, infinite: true }",
  "  - id: mx_s",
  "    type: matrix",
  "    files: { filename: \"mx_s.tsv\", type: tsv }",
  "    row_labels:",
  "      - { id: GENE_ID, type: SAS Char, nullable: No }",
  "    matrix:",
  "      rows: { key: GENE_ID }",
  "      cells: { type: SAS Num, nullable: No, format: \"SAS 8.2\" }"
)

test_that("the layout form offers the cell types of the matrix's own backend, whichever the library prefers", {
  for (default in c("SAS", "R")) {
    be_library(default)

    shiny::testServer(app_server_dir(), {
      session$setInputs(dta_file = be_upload(be_two_matrices))
      unlock_editing(session)

      rv$active <- "mx_r"
      session$setInputs(edit_matrix = 1)
      html <- ui_text(output$mx_modal_body)
      radios <- regmatches(html, regexpr('(?s)<div id="mx_cell_type".*?</div>\\s*</div>', html, perl = TRUE))
      expect_length(radios, 1L)
      expect_match(radios, 'value="R numeric" checked', fixed = TRUE, info = default)
      expect_match(radios, 'value="R character"', fixed = TRUE, info = default)
      expect_no_match(radios, "SAS", fixed = TRUE)
      # The conditions come from the values on offer, not from a literal type name.
      expect_match(html, "[&#39;R character&#39;].indexOf(input.mx_cell_type) &lt; 0", fixed = TRUE, info = default)
      expect_match(html, "[&#39;R numeric&#39;].indexOf(input.mx_cell_type) &gt;= 0", fixed = TRUE, info = default)
      # R cells take no format: the field is not on the page.
      expect_no_match(html, 'id="mx_cell_format"', fixed = TRUE, info = default)
      expect_match(html, 'id="mx_cell_length"', fixed = TRUE, info = default)
      session$setInputs(mx_save = 0)

      rv$active <- "mx_s"
      session$setInputs(edit_matrix = 2)
      html <- ui_text(output$mx_modal_body)
      radios <- regmatches(html, regexpr('(?s)<div id="mx_cell_type".*?</div>\\s*</div>', html, perl = TRUE))
      expect_match(radios, 'value="SAS Num" checked', fixed = TRUE, info = default)
      expect_match(radios, 'value="SAS Char"', fixed = TRUE, info = default)
      expect_no_match(radios, "R numeric", fixed = TRUE, info = default)
      expect_match(html, "[&#39;SAS Char&#39;].indexOf(input.mx_cell_type) &lt; 0", fixed = TRUE, info = default)
      expect_match(html, "[&#39;SAS Num&#39;].indexOf(input.mx_cell_type) &gt;= 0", fixed = TRUE, info = default)
      expect_match(html, "SAS format (e.g. 8.2)", fixed = TRUE, info = default)
      expect_identical(be_attr(html, "mx_cell_format", "value"), "8.2")
    })
  }
})

test_that("saving an R matrix's layout does not read the SAS matrix's Format field left behind in the form", {
  be_library("SAS")

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = be_upload(be_two_matrices))
    unlock_editing(session)

    # The SAS matrix's form is open and reports its format ...
    rv$active <- "mx_s"
    session$setInputs(edit_matrix = 1)
    invisible(ui_text(output$mx_modal_body))
    session$setInputs(mx_cell_format = "8.2")

    # ... then the R matrix's, which has no such field. The server still holds
    # the old value; the save must not take it for this form's.
    rv$active <- "mx_r"
    session$setInputs(edit_matrix = 2)
    invisible(ui_text(output$mx_modal_body))
    before <- as.list(rv$dta@datasets$mx_r@matrix)
    session$setInputs(mx_row_key = "GENE_ID", mx_cell_label = "Counts", mx_save = 1)

    expect_null(rv$mx_msg)
    after <- as.list(rv$dta@datasets$mx_r@matrix)
    expect_identical(after$cells$label, "Counts")
    expect_null(after$cells$format)
    expect_identical(after$cells$type, "R numeric")
    expect_true(after$cells$infinite)
    after$cells$label <- NULL
    expect_identical(after, before)
  })
})

# ---- rules ---------------------------------------------------------------------------

test_that("the range-rule picker lists the number and integer columns of both backends and no others", {
  be_library()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = be_upload())
    unlock_editing(session)
    session$setInputs(edit_rules = 1)
    session$setInputs(rule_add = 1)
    session$setInputs(rule_type = "col_range")
    html <- ui_text(output$rule_modal_body)

    picker <- be_select(html, "rule_col_single")
    expect_setequal(setdiff(picker$choices, ""), c("S_NUM", "S_INT", "R_NUM", "R_INT"))
    expect_match(html, "SAS Num or Int, R numeric or integer", fixed = TRUE)
  })
})

test_that("a rule on an R logical column keeps true and false as the text typed, and the column's rule list says so", {
  be_library()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = be_upload())
    unlock_editing(session)
    session$setInputs(edit_rules = 1)
    session$setInputs(rule_add = 1)
    session$setInputs(rule_type = "col_condition")
    invisible(ui_text(output$rule_modal_body))
    session$setInputs(
      rule_id = "lgl_rule", rule_desc = "",
      cond_col_1 = "R_LGL", cond_op_1 = "equals", cond_val_1 = "true",
      then_col_1 = "S_CHAR", then_op_1 = "equals", then_val_1 = "x"
    )
    session$setInputs(rule_save = 1)

    expect_null(rv$rule_msg)
    rl <- DTAtools::datasets(rv$dta, "mixed")@specs@rules
    expect_length(rl, 1L)
    cond <- app_fn("dta_rule_to_list")(rl[[1L]])$condition
    expect_identical(cond$R_LGL$equals, "true")
  })
})

# ---- dataset templates hand the package their R types as written --------------------

test_that("a dataset template with R columns builds a dataset whose columns are R, and saves them as R", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "template.yaml")
  writeLines(
    c(
      "kind: dta_dataset_template",
      "id: r_findings",
      "version: \"1.0\"",
      "dataset:",
      "  id: findings",
      "  type: tabular",
      "  files: { filename: f.csv, type: csv }",
      "  columns:",
      "    - { id: SUBJ, type: R character, length: 8, nullable: No }",
      "    - { id: SCORE, type: R numeric }",
      "    - { id: VISIT_ON, type: R Date }",
      "    - { id: SEEN_AT, type: R POSIXct, format: \"R %Y-%m-%d %H:%M\" }",
      "    - { id: ALIVE, type: R logical }",
      "  rules: []"
    ),
    path
  )
  def <- app_fn("read_dataset_template")(path)
  expect_true(def$ok, info = def$error)
  built <- app_fn("build_dataset_from_template")(def$value)
  expect_true(built$ok, info = built$error)
  ds <- DTAtools::dta_dataset_from_list(built$value$dataset)

  types <- vapply(ds@specs@columns, function(c) as.list(c@structure)$type, "")
  expect_identical(
    unname(types),
    c("R character", "R numeric", "R Date", "R POSIXct", "R logical")
  )
  # The package stored the Date's default format and kept the typed one.
  fmts <- lapply(ds@specs@columns, function(c) c@structure@format)
  expect_identical(fmts[["VISIT_ON"]], "%Y-%m-%d")
  expect_identical(fmts[["SEEN_AT"]], "%Y-%m-%d %H:%M")

  # The app's own column writer keeps the prefix on both the type and the format.
  date_col <- app_fn("dta_column_to_list")(ds@specs@columns[["VISIT_ON"]])
  expect_identical(c(date_col$type, date_col$format), c("R Date", "R %Y-%m-%d"))
  seen_col <- app_fn("dta_column_to_list")(ds@specs@columns[["SEEN_AT"]])
  expect_identical(c(seen_col$type, seen_col$format), c("R POSIXct", "R %Y-%m-%d %H:%M"))
  expect_identical(app_fn("dta_column_to_list")(ds@specs@columns[["SUBJ"]])$length, 8L)
})

test_that("a dataset template with an R matrix builds it, R row labels and R cells", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "template.yaml")
  writeLines(
    c(
      "kind: dta_dataset_template",
      "id: r_counts",
      "version: \"1.0\"",
      "dataset:",
      "  id: counts",
      "  type: matrix",
      "  files: { filename: c.tsv, type: tsv }",
      "  row_labels:",
      "    - { id: GENE_ID, type: R character, nullable: No }",
      "  matrix:",
      "    rows: { key: GENE_ID }",
      "    cells: { type: R integer, nullable: No, range: { min: 0 } }"
    ),
    path
  )
  def <- app_fn("read_dataset_template")(path)
  expect_true(def$ok, info = def$error)
  built <- app_fn("build_dataset_from_template")(def$value)
  expect_true(built$ok, info = built$error)
  ds <- DTAtools::dta_dataset_from_list(built$value$dataset)

  expect_identical(as.list(ds@matrix)$cells$type, "R integer")
  expect_identical(ds@specs@columns[["GENE_ID"]]@structure@backend, "R")
})

# ---- a new row label starts in the matrix's own backend ---------------------------------

test_that("dta_matrix_backend reads the backend of a matrix's cells, and is NA for anything else", {
  mb <- app_fn("dta_matrix_backend")
  two <- be_dta(be_two_matrices)

  expect_identical(mb(two, "mx_r"), "R")
  expect_identical(mb(two, "mx_s"), "SAS")
  expect_identical(mb(be_dta(), "mixed"), NA_character_)
  expect_identical(mb(two, "no_such_dataset"), NA_character_)
  expect_identical(mb(two, NULL), NA_character_)
})

test_that("a new row label of a matrix starts as the text type of the matrix's own backend, not the library's", {
  for (case in list(
    list(default = "R", active = "mx_s", backend = "SAS", types = be_sas_types, type = "Char"),
    list(default = "SAS", active = "mx_r", backend = "R", types = be_r_types, type = "character"),
    list(default = "SAS", active = "mx_s", backend = "SAS", types = be_sas_types, type = "Char"),
    list(default = "R", active = "mx_r", backend = "R", types = be_r_types, type = "character")
  )) {
    be_library(case$default)
    info <- paste("library", case$default, "matrix", case$active)

    shiny::testServer(app_server_dir(), {
      session$setInputs(dta_file = be_upload(be_two_matrices))
      unlock_editing(session)
      rv$active <- case$active
      session$setInputs(edit_cols = 1)
      session$setInputs(col_add = 1)
      html <- ui_text(output$col_modal_body)

      expect_identical(be_select(html, "col_backend")$selected, case$backend, info = info)
      # The other backend is still on offer.
      expect_identical(be_select(html, "col_backend")$choices, c("SAS", "R"), info = info)
      expect_identical(be_select(html, "col_type")$choices, case$types, info = info)
      expect_identical(be_select(html, "col_type")$selected, case$type, info = info)
      expect_identical(col_shown_backend(), case$backend, info = info)

      # Saved as the form shows it: a second row label of the matrix's own backend.
      session$setInputs(
        col_id = "GENE_NAME", col_label = "", col_backend = case$backend, col_type = case$type,
        col_format = "", col_length = "", col_nullable = TRUE, col_values = "",
        col_pattern = "", col_examples = "", col_desc = "",
        col_header_mode = "id", col_header_text = "", col_save = 1
      )
      expect_null(rv$col_msg)
      st <- be_column(rv$dta, case$active, "GENE_NAME")@structure
      expect_identical(c(st@backend, st@type), c(case$backend, case$type), info = info)
    })
  }
})

test_that("a new column of a tabular dataset still starts with the library's default, in a document that also has matrices", {
  be_library("R")

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = be_upload(c(be_two_matrices, be_typeless_yaml[-1])))
    unlock_editing(session)
    rv$active <- "d"
    session$setInputs(edit_cols = 1)
    session$setInputs(col_add = 1)
    html <- ui_text(output$col_modal_body)

    expect_identical(be_select(html, "col_backend")$selected, "R")
    expect_identical(be_select(html, "col_type")$selected, "character")
    expect_identical(rv$col_prefill, list())
  })
})
