# The reading entry points a handler delegates to. REQ-FILE-030.
#
# read_file() and open_file() match a delivery against a handler's declared
# filename and then hand the work to these. A caller reaching them directly
# skips that matching, which is the only reason they are exported -- and it is
# also why the abstract class must refuse: it has no separator of its own, so a
# read through it would have to guess.

fe_handler <- function(kind, filename) {
  switch(kind,
    csv = DTAFileCSV(filename = filename),
    tsv = DTAFileTSV(filename = filename),
    abstract = DTAFileTabular(filename = filename)
  )
}

test_that("OQ-FILE-050 | the reading entry points read and open a concrete delivery | REQ-FILE-030", {
  dir <- qa_tempdir()
  csv <- file.path(dir, "delivery.csv")
  utils::write.csv(
    data.frame(ID = c("A001", "A002"), AGE = c(30, 40), stringsAsFactors = FALSE),
    csv,
    row.names = FALSE, na = ""
  )

  handler <- fe_handler("csv", "delivery.csv")

  read <- read_file_execution(handler, file = csv)
  qa_step("the read returns every row of the delivery", 2L, nrow(as.data.frame(read)))
  qa_step(
    "and every column of it",
    c("ID", "AGE"), names(as.data.frame(read))
  )

  # The lazy open is what a delivery too large to hold goes through, so it must
  # describe the file without reading it.
  opened <- open_file_execution(handler, file = csv)
  qa_step(
    "the lazy open reports the same columns without materialising the rows",
    c("ID", "AGE"), names(opened)
  )
  qa_check(
    "and returns an Arrow object rather than a data frame",
    !is.data.frame(opened)
  )
})

test_that("OQ-FILE-051 | a tab-separated delivery is read by its own handler | REQ-FILE-030", {
  dir <- qa_tempdir()
  tsv <- file.path(dir, "delivery.tsv")
  writeLines(c("ID\tAGE", "A001\t30", "A002\t40"), tsv)

  read <- read_file_execution(fe_handler("tsv", "delivery.tsv"), file = tsv)
  # Read with the wrong separator this would be one column of joined text, and
  # every later check would fail for a reason that has nothing to do with the
  # data.
  qa_step("the tab-separated delivery splits into its columns", c("ID", "AGE"), names(as.data.frame(read)))
  qa_step("and keeps its rows", 2L, nrow(as.data.frame(read)))
})

test_that("OQ-FILE-052 | the abstract handler refuses to read | REQ-FILE-030", {
  dir <- qa_tempdir()
  csv <- file.path(dir, "delivery.csv")
  writeLines(c("ID,AGE", "A001,30"), csv)

  abstract <- fe_handler("abstract", "delivery.csv")

  # The abstract class declares no separator. Guessing one would read the
  # delivery in a way nobody specified, and the wrong reading of a valid file
  # is indistinguishable in the report from a genuinely malformed one.
  qa_check(
    "reading through the abstract handler raises a condition",
    inherits(tryCatch(read_file_execution(abstract, file = csv), error = function(e) e), "condition")
  )
  qa_check(
    "and so does opening through it",
    inherits(tryCatch(open_file_execution(abstract, file = csv), error = function(e) e), "condition")
  )
})
