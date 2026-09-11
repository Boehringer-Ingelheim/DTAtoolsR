# Repeating regions in a Word template: {#DATASETS} .. {/DATASETS}. REQ-EXPORT-031.
#
# A region is a body paragraph holding only {#DATASETS}, paired with a later
# body paragraph holding only {/DATASETS}. Everything between the two markers
# -- paragraphs and tables alike -- is emitted once per dataset the DTA
# declares, in the DTA's own dataset order, and the marker paragraphs
# themselves never appear in the output. Six placeholders resolve inside a
# repetition to that one dataset's own values, and a dataset block
# placeholder written bare inside the region (no `:NAME` argument) contributes
# only its table, because the region's own paragraphs are what supply the
# heading. A marker that is not alone in a body paragraph, or that has no
# counterpart, is left as written and named in the existing block-placeholder
# warning.

# ---- fixtures ---------------------------------------------------------------

# Two tabular datasets, built here rather than borrowed from the shipped
# clinical_dta.yaml fixture, so that every expected name below ("ADSL",
# "ADAE", their column ids) is this test's own literal rather than something
# read out of a YAML file elsewhere.
er_dta <- function() {
  DTA(
    datasets = list(
      DTADataSetTabular(name = "ADSL", specs = create_example_DTAColumnSpecCollection(1)),
      DTADataSetTabular(name = "ADAE", specs = create_example_DTAColumnSpecCollection(2))
    ),
    metadata = create_example_DTAMetaData()
  )
}

# A minimal Word template with one paragraph per line. File-local copy of
# test-OQ-EXPORT-documents.R's et_write_template(), which is itself file-local
# to that file.
er_write_template <- function(lines, dir) {
  path <- file.path(dir, paste0("template-", as.integer(stats::runif(1, 1, 1e9)), ".docx"))
  doc <- officer::read_docx()
  for (ln in lines) {
    doc <- officer::body_add_par(doc, ln)
  }
  print(doc, target = path)
  path
}

# Shared shape of OQ-EXPORT-043's two malformed-marker cases: export without
# quiet (so the leftover-placeholder warning is not suppressed), collect every
# warning raised, confirm the marker is named in one of them, and confirm the
# template's own lines survive completely unchanged.
er_check_left_as_written <- function(dta, lines, dir) {
  template <- er_write_template(lines, dir)
  out <- file.path(dir, "out.docx")

  seen <- character(0)
  suppressMessages(withCallingHandlers(
    export_with_template(dta, template, out),
    warning = function(w) {
      seen <<- c(seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  ))
  qa_check("a warning names the marker", any(grepl("{#DATASETS}", seen, fixed = TRUE)))

  s <- qa_docx_summary(out)
  paras <- s$text[s$content_type == "paragraph"]
  qa_step("the document is left as written", lines, paras[nzchar(paras)])
}

# ---- tests --------------------------------------------------------------

test_that("OQ-EXPORT-042 | a {#DATASETS} region is emitted once per dataset with that dataset's own values | REQ-EXPORT-031", {
  dir <- qa_tempdir()
  dta <- er_dta()
  template <- er_write_template(
    c(
      "BEFORE", "{#DATASETS}", "Dataset {DATASET_NAME} ({DATASET_TYPE})",
      "{COLUMN_SPECS}", "{/DATASETS}", "AFTER"
    ),
    dir
  )
  out <- file.path(dir, "regions.docx")
  export_with_template(dta, template, out, quiet = TRUE)

  s <- qa_docx_summary(out)
  paras <- s$text[s$content_type == "paragraph"]

  # Block rendering leaves empty spacer paragraphs around a table, so the
  # comparison drops empty strings rather than pinning an exact paragraph
  # count that has nothing to do with what this requirement specifies.
  qa_step(
    "the region is repeated once per dataset, in order, and the markers are gone",
    c("BEFORE", "Dataset ADSL (tabular)", "Dataset ADAE (tabular)", "AFTER"),
    paras[nzchar(paras)]
  )
  qa_check(
    "no generated dataset heading is added inside the region",
    !any(startsWith(paras, "Column Specifications"))
  )

  tables <- qa_docx_tables(out)
  qa_step("one column table per dataset", 2L, length(tables))

  coll1 <- create_example_DTAColumnSpecCollection(1)
  coll2 <- create_example_DTAColumnSpecCollection(2)
  ids1 <- unname(vapply(coll1@columns, function(s) s@id, character(1)))
  ids2 <- unname(vapply(coll2@columns, function(s) s@id, character(1)))

  # The header row is the table's first row; dropped positionally, because the
  # header text is the column table's own formatting, not what this
  # requirement is about.
  qa_step("the first dataset's table lists its own column ids", ids1, tables[[1]]$col1[-1])
  qa_step("the second dataset's table lists its own column ids", ids2, tables[[2]]$col1[-1])
})

test_that("OQ-EXPORT-043 | a region marker that is not alone in a body paragraph, or has no partner, is left as written and named in a warning | REQ-EXPORT-031", {
  dta <- er_dta()

  # (a) The opening marker is mid-sentence, so there is no region at all:
  # {/DATASETS} and {DATASET_NAME} stay literal too, since no region means no
  # repetition. {DATASET_NAME} is separately named by the pre-existing
  # unresolved-placeholder warning here, since with no region it is just
  # another unknown token -- expected, and not asserted on above.
  dir_a <- qa_tempdir()
  er_check_left_as_written(dta, c("Start {#DATASETS}", "{DATASET_NAME}", "{/DATASETS}"), dir_a)

  # (b) The region is opened but never closed.
  dir_b <- qa_tempdir()
  er_check_left_as_written(dta, c("BEFORE", "{#DATASETS}", "middle"), dir_b)
})
