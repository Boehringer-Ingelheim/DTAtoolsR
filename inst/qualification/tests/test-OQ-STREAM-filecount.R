# File count is independent of how the tables are held. REQ-STREAM-025.

test_that("OQ-STREAM-032 | a DTA check judges file count identically for streamed and in-memory tables | REQ-STREAM-025", {
  dir <- qa_tempdir()
  stems <- c("x_a", "x_b", "x_c")
  for (nm in stems) {
    utils::write.csv(
      data.frame(ID = paste0(nm, c("1", "2")), stringsAsFactors = FALSE),
      file.path(dir, paste0(nm, ".csv")),
      row.names = FALSE
    )
  }
  specs <- DTAColumnSpecCollection(columns = list(
    ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE)
  ))

  held <- function(stream, lo, hi) {
    ds <- DTADataSetTabular(
      id = "d", specs = specs,
      files = list(DTAFileCSV(
        filename = "x_.*[.]csv$", pattern = TRUE,
        min_number_of_files = lo, max_number_of_files = hi
      ))
    )
    dta <- DTA(datasets = list(d = ds), metadata = DTAMetaData(title = "Count", version = "1.0"))
    for (nm in stems) {
      dta <- load_file(
        dta,
        dataset = "d", file = file.path(dir, paste0(nm, ".csv")),
        name = nm, stream = stream
      )
    }
    res <- suppressWarnings(check(dta, persist = FALSE, quiet = TRUE))
    list(
      ok = attr(res, "last_validation_ok"),
      lazy_tables = all(vapply(tables(datasets(res, "d")), dta_table_is_lazy, logical(1))),
      n_err = attr(res, "last_validation_summary")$n_file_count_errors,
      found = attr(res, "last_file_count_findings")[, c("dataset", "n_supplied", "min", "max")]
    )
  }

  # Three files are supplied. From the bounds alone: 1..2 is too many, 4..5 is
  # too few, 2..3 is within.
  cases <- list(
    list(label = "above the maximum (3 files, bounds 1..2)", lo = 1, hi = 2, bad = TRUE),
    list(label = "below the minimum (3 files, bounds 4..5)", lo = 4, hi = 5, bad = TRUE),
    list(label = "within bounds (3 files, bounds 2..3)", lo = 2, hi = 3, bad = FALSE)
  )
  for (cs in cases) {
    mem <- held("never", cs$lo, cs$hi)
    lazy <- held("always", cs$lo, cs$hi)
    qa_check(paste("the streamed tables are held lazily:", cs$label), lazy$lazy_tables)
    qa_check(paste("the in-memory tables are not lazy:", cs$label), !mem$lazy_tables)
    qa_step(paste("in-memory findings:", cs$label), as.integer(cs$bad), nrow(mem$found))
    qa_step(paste("streamed findings:", cs$label), as.integer(cs$bad), nrow(lazy$found))
    qa_step(paste("streamed n_file_count_errors:", cs$label), as.integer(cs$bad), lazy$n_err)
    qa_step(paste("streamed verdict:", cs$label), !cs$bad, lazy$ok)
    qa_step(paste("streamed and in-memory agree on the findings:", cs$label), mem$found, lazy$found)
    qa_step(paste("and on the verdict:", cs$label), mem$ok, lazy$ok)
    if (cs$bad) {
      qa_step(
        paste("supplied count and bounds:", cs$label), c(3, cs$lo, cs$hi),
        unlist(lazy$found[1, c("n_supplied", "min", "max")], use.names = FALSE)
      )
    }
  }
})
