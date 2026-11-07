# write_template_guide(): one readable document per usable template.
# REQ-TMPL-040.
#
# Every expected value below is read straight from the bundled template YAML
# (qa_read_yaml) or taken from the requirement text -- never from the function
# under qualification or its internals. Output goes to qa_tempdir() only.

guide_lib <- function() system.file("extdata", "templates", package = "DTAtools")
guide_yaml <- function(name) qa_read_yaml(file.path(guide_lib(), name))

# The usable (non-abstract) creation templates of the bundled library, with the
# file name each must be written to: `<id>_v<version>_template_guide.docx`.
guide_templates <- function() {
  files <- list.files(guide_lib(), pattern = "[.]dta-template[.]yaml$")
  docs <- Filter(function(d) !isTRUE(d[["abstract"]]), lapply(files, guide_yaml))
  stats::setNames(
    vapply(docs, function(d) sprintf("%s_v%s_template_guide.docx", d[["id"]], d[["version"]]), ""),
    vapply(docs, function(d) as.character(d[["id"]]), "")
  )
}

guide_write <- function(root, ...) {
  dir <- tempfile("guides", tmpdir = root)
  paths <- suppressMessages(write_template_guide(guide_lib(), dir, quiet = TRUE, ...))
  list(dir = dir, paths = paths)
}

# The raw XML of every header and footer part of a .docx.
guide_hf_xml <- function(file) {
  out <- tempfile("unz", tmpdir = dirname(file))
  utils::unzip(file, exdir = out)
  parts <- list.files(file.path(out, "word"), pattern = "^(header|footer)[0-9]*[.]xml$", full.names = TRUE)
  vapply(parts, function(p) paste(readLines(p, warn = FALSE, encoding = "UTF-8"), collapse = ""), "")
}

# Table rows of a document, each as a character vector of its cells.
guide_rows <- function(file) {
  rows <- list()
  for (tb in qa_docx_tables(file)) {
    for (i in seq_len(nrow(tb))) rows[[length(rows) + 1L]] <- as.character(unlist(tb[i, ]))
  }
  rows
}

# ---- REQ-TMPL-040: what the guide says ---------------------------------------

test_that("OQ-TMPL-086 | one document is written per usable template, named from its id and version | REQ-TMPL-040", {
  expected <- guide_templates()
  res <- guide_write(qa_tempdir())

  qa_check("the library has at least two usable templates", length(expected) >= 2L)
  qa_step("the result is named by template id", sort(names(expected)), sort(names(res$paths)))
  qa_step("the files written are <id>_v<version>_template_guide.docx", sort(unname(expected)), sort(list.files(res$dir)))
  qa_check("each file is a readable document with text", all(vapply(
    res$paths, function(f) nchar(qa_docx_text(f)) > 0L, logical(1)
  )))
})

test_that("OQ-TMPL-087 | a question card shows the option label together with its pre-filled answer | REQ-TMPL-040", {
  parent <- guide_yaml("biomarker_gf.dta-template.yaml")
  child <- guide_yaml("biomarker_gf_acme.dta-template.yaml")
  header_id <- as.character(child[["options"]][[1]][["id"]])
  header_label <- as.character(Filter(function(o) identical(o[["id"]], header_id), parent[["options"]])[[1]][["label"]])
  prefilled <- as.character(child[["options"]][[1]][["default"]])
  res <- guide_write(qa_tempdir())
  # One card is one table: its heading row carries the label, a body row the answer.
  together <- vapply(qa_docx_tables(res$paths[["biomarker_gf_acme"]]), function(tb) {
    cells <- as.matrix(tb)
    has_label <- any(startsWith(as.character(cells[, ncol(cells)]), header_label))
    has_answer <- any(cells[, 1] %in% "Pre-filled answer" & cells[, ncol(cells)] %in% prefilled)
    has_label && has_answer
  }, logical(1))

  qa_step("the child YAML pre-fills the header with ACME Labs", "ACME Labs", prefilled)
  qa_check("a single table holds both the question label and the pre-filled answer", any(together))
})

test_that("OQ-TMPL-088 | each page has a running header and footer with page fields, the organisation, and the template id and version | REQ-TMPL-040", {
  parent <- guide_yaml("biomarker_gf.dta-template.yaml")
  res <- guide_write(qa_tempdir(), organisation = "Qualification Org")
  all <- paste(guide_hf_xml(res$paths[["biomarker_gf"]]), collapse = " ")

  qa_check("a PAGE field is present", grepl("PAGE", all, fixed = TRUE))
  qa_check("a NUMPAGES field is present", grepl("NUMPAGES", all, fixed = TRUE))
  qa_check("the organisation passed in is printed", grepl("Qualification Org", all, fixed = TRUE))
  qa_check("the template id is printed", grepl(parent[["id"]], all, fixed = TRUE))
  qa_check("the template version is printed", grepl(as.character(parent[["version"]]), all, fixed = TRUE))
})

test_that("OQ-TMPL-089 | every column of a dataset template is listed, and a column the DTA template adds is tagged | REQ-TMPL-040", {
  ds <- guide_yaml("gf_smrnaseq.dta-dataset-template.yaml")
  ids <- vapply(ds[["dataset"]][["columns"]], function(cl) as.character(cl[["id"]]), "")
  child <- guide_yaml("biomarker_gf_acme.dta-template.yaml")
  added <- as.character(child[["datasets"]][[1]][["patch"]][["add_columns"]][[1]][["id"]])
  file <- guide_write(qa_tempdir())$paths[["biomarker_gf_acme"]]
  text <- qa_docx_text(file)
  # The tag and the id share one table row.
  tagged <- vapply(guide_rows(file), function(cells) {
    any(startsWith(cells, added)) && any(grepl("Added by this template", cells, fixed = TRUE))
  }, logical(1))

  qa_check("the dataset template declares columns", length(ids) > 0L)
  qa_step("no column id of the dataset template is missing", character(0), ids[!vapply(ids, grepl, NA, x = text, fixed = TRUE)])
  qa_check("the added column's row carries 'Added by this template'", any(tagged))
})

# ---- REQ-TMPL-040: what the guide refuses ------------------------------------

test_that("OQ-TMPL-090 | an abstract template gets no document of its own and is refused when asked for | REQ-TMPL-040", {
  lib <- file.path(qa_tempdir(), "abstractlib")
  dir.create(lib)
  src <- readLines(file.path(guide_lib(), "biomarker_gf.dta-template.yaml"), encoding = "UTF-8")
  writeLines(
    sub("^id: biomarker_gf$", "id: building_block\nabstract: true", src),
    file.path(lib, "building_block.dta-template.yaml")
  )
  file.copy(file.path(guide_lib(), "biomarker_gf.dta-template.yaml"), lib)
  dir <- file.path(qa_tempdir(), "abstractout")

  err <- tryCatch(write_template_guide(lib, dir, templates = "building_block", quiet = TRUE), error = function(e) e)
  qa_check("asking for the abstract template is refused with the abstract condition", inherits(err, "dta_template_guide_abstract"))
  suppressMessages(write_template_guide(lib, dir, quiet = TRUE))
  qa_step("a run over the library writes only the usable template", "biomarker_gf_v1.0_template_guide.docx", list.files(dir))
})

test_that("OQ-TMPL-091 | an existing output file is not replaced unless overwrite = TRUE | REQ-TMPL-040", {
  dir <- file.path(qa_tempdir(), "exists")
  dir.create(dir)
  file <- file.path(dir, guide_templates()[["biomarker_gf"]])
  writeLines("precious", file)
  before <- unname(tools::md5sum(file))

  err <- tryCatch(write_template_guide(guide_lib(), dir, quiet = TRUE), error = function(e) e)
  qa_check("the call is refused with the exists condition", inherits(err, "dta_template_guide_exists"))
  qa_step("and the file is byte-for-byte unchanged", before, unname(tools::md5sum(file)))
})

test_that("OQ-TMPL-092 | a format other than docx or pdf is refused | REQ-TMPL-040", {
  dir <- file.path(qa_tempdir(), "badformat")
  err <- tryCatch(write_template_guide(guide_lib(), dir, format = "html", quiet = TRUE), error = function(e) e)
  qa_check("html is refused with the format condition", inherits(err, "dta_template_guide_format"))
  qa_check("and nothing was written", length(list.files(dir)) == 0L)
})
