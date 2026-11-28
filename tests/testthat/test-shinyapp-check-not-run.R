# The inspector of the app explains a column whose checks could not be run.
#
# Its "Should be" and "Actual value" texts are derived from the keyword of the
# finding. For `dta_check_not_run` the fallbacks found nothing to show and the
# panel read "(empty)" for a column that is present and was simply never
# examined: the same backwards statement the HTML report avoids for an
# undeclared column.

skip_if_not_installed("shiny")
skip_if_not_installed("bslib")
skip_if_not_installed("DT")
skip_if_not_installed("shinyjs")

# A column whose structure cannot say how it validates, beside one that can.
ins_dta <- function() {
  bad <- DTAColumnSpec(id = "BAD", type = "SAS Char")
  bad@structure <- DTAColumnSpecStructure(backend = "SAS")
  specs <- DTAColumnSpecCollection(columns = list(
    SEX = DTAColumnSpec(id = "SEX", type = "SAS Char", length = 1, nullable = FALSE),
    BAD = bad
  ))
  ds <- DTADataSetTabular(
    id = "nr", specs = specs,
    tables = list(tab = data.frame(SEX = c("M", "F"), BAD = c("a", NA), stringsAsFactors = FALSE))
  )
  suppressWarnings(suppressMessages(check(DTA(datasets = list(ds)), persist = FALSE, quiet = TRUE)))
}

test_that("the inspector says no check was run on the column instead of showing (empty)", {
  dta <- ins_dta()

  shiny::testServer(app_server_dir(), {
    msgs <- app_fn("dta_dataset_messages")(dta, "nr")
    id <- msgs$id[match("dta_check_not_run", msgs$keyword)]
    expect_false(is.na(id))

    res <- app_fn("dta_inspect")(dta, "nr", id)
    expect_true(res$ok, info = res$error)
    html <- render_html(render_inspect_body(res$value, "nr"))

    expect_match(html, "no check was run on this column", fixed = TRUE)
    expect_match(html, "can be turned into checks", fixed = TRUE)
    expect_false(grepl("(empty)", html, fixed = TRUE))
  })
})
