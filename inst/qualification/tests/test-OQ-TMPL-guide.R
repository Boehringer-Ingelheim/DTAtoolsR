# write_template_guide(): the handbook of a template library.
# REQ-TMPL-040.
#
# Every expected value below is read straight from the bundled template YAML
# (qa_read_yaml) or taken from the requirement text -- never from the function
# under qualification or its internals. Output goes to qa_tempdir() only.

guide_lib <- function() system.file("extdata", "templates", package = "DTAtools")
guide_yaml <- function(name) qa_read_yaml(file.path(guide_lib(), name))

# ---- REQ-TMPL-040: what the guide says ---------------------------------------

test_that("OQ-TMPL-086 | every option label of a creation template appears as text in its guide | REQ-TMPL-040", {
  labels <- vapply(guide_yaml("biomarker_gf.dta-template.yaml")[["options"]], function(o) as.character(o[["label"]]), "")
  file <- file.path(qa_tempdir(), "guide.docx")
  suppressMessages(write_template_guide(guide_lib(), file, quiet = TRUE))
  text <- qa_docx_text(file)

  qa_check("the source template declares options to look for", length(labels) > 0L)
  qa_step(
    "no option label of biomarker_gf is missing from the document",
    character(0), labels[!vapply(labels, grepl, NA, x = text, fixed = TRUE)]
  )
})

test_that("OQ-TMPL-087 | a child template's pre-filled answer sits in the row of the question it answers, and its added column appears | REQ-TMPL-040", {
  parent <- guide_yaml("biomarker_gf.dta-template.yaml")
  child <- guide_yaml("biomarker_gf_acme.dta-template.yaml")
  header_id <- as.character(child[["options"]][[1]][["id"]])
  header_label <- as.character(Filter(function(o) identical(o[["id"]], header_id), parent[["options"]])[[1]][["label"]])
  prefilled <- as.character(child[["options"]][[1]][["default"]])
  added <- as.character(child[["datasets"]][[1]][["patch"]][["add_columns"]][[1]][["id"]])
  file <- file.path(qa_tempdir(), "guide.docx")
  suppressMessages(write_template_guide(guide_lib(), file, quiet = TRUE))
  text <- qa_docx_text(file)
  rows <- list()
  for (tb in qa_docx_tables(file)) {
    for (i in seq_len(nrow(tb))) rows[[length(rows) + 1L]] <- as.character(unlist(tb[i, ]))
  }
  together <- vapply(rows, function(cells) {
    any(grepl(header_label, cells, fixed = TRUE)) && any(startsWith(cells, prefilled))
  }, logical(1))

  qa_step("the YAML pre-fills the header with ACME Labs", "ACME Labs", prefilled)
  qa_check("a single table row holds both the header question and ACME's pre-filled answer", any(together))
  qa_check("the column the dataset template patch adds is in the document", grepl(added, text, fixed = TRUE))
})

# ---- REQ-TMPL-040: what the guide refuses ------------------------------------

test_that("OQ-TMPL-088 | an existing output file is not replaced unless overwrite = TRUE | REQ-TMPL-040", {
  file <- file.path(qa_tempdir(), "guide.docx")
  writeLines("precious", file)
  before <- unname(tools::md5sum(file))

  err <- tryCatch(write_template_guide(guide_lib(), file, quiet = TRUE), error = function(e) e)
  qa_check("the call is refused with the exists condition", inherits(err, "dta_template_guide_exists"))
  qa_step("and the file is byte-for-byte unchanged", before, unname(tools::md5sum(file)))
})

test_that("OQ-TMPL-089 | a format other than docx or pdf is refused | REQ-TMPL-040", {
  file <- file.path(qa_tempdir(), "guide.html")
  err <- tryCatch(write_template_guide(guide_lib(), file, format = "html", quiet = TRUE), error = function(e) e)
  qa_check("html is refused with the format condition", inherits(err, "dta_template_guide_format"))
  qa_check("and nothing was written", !file.exists(file))
})
