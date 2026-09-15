# shiny::testServer() only evaluates a render function when a test reads its
# output. app.R defines 53 static `output$<id> <-` assignments; before this
# file, the suite read 21 of them -- the other 32 render bodies never ran, so
# a crash inside one shipped green. This file adds the two highest-value
# outputs from that unread set: the Word export (the app's core deliverable,
# and the one output here that calls DTAtools::dta_export()) and the in-app
# validation-messages table. output$file_tbl/col_tbl/rule_tbl, the next
# highest-value outputs, are covered in test-shinyapp-export.R instead, where
# they share a fixture with the escaping test that already drives them.
#
# 27 outputs remain unread after this file (mostly *_editor_msg/*_modal_body
# renderUI bodies and other download handlers) -- left for a future pass.

skip_if_not_installed("shiny")
skip_if_not_installed("bslib")
skip_if_not_installed("DT")
skip_if_not_installed("shinyjs")

app_server_dir <- function() .shiny_app_dir()

# A shiny fileInput value for a bundled fixture (same shape as
# test-shinyapp-server.R's app_file_input(); redefined here rather than
# shared, because testthat sources test-*.R files in alphabetical order and
# "export" precedes "server" -- relying on the other file's copy would work
# only by accident of run order).
app_file_input <- function(filename) {
  path <- app_fixture_path(filename)
  data.frame(
    name = filename, size = file.size(path), type = "", datapath = path,
    stringsAsFactors = FALSE
  )
}

test_that("the Word export download produces a real .docx with the document's own content", {
  # output$dl_docx's whole content function is `dta_export(export_dta(), file,
  # "docx")`, gated only on `req(rv$dta)` -- no active dataset, no upload, no
  # check needed. That also makes it the cheapest possible reproduction of the
  # "never read" risk: a typo in that one line ships with nothing to catch it.
  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))

    path <- output$dl_docx
    expect_true(file.exists(path))

    # The title and the dataset name are read from clinical_dta.yaml by hand
    # (metadata.title / the dataset's own key), not from anything this test
    # ran -- so a docx that came back empty, truncated, or built from the
    # wrong document would fail here even though the file itself exists.
    text <- .docx_text(path)
    expect_true(any(grepl("Clinical Data Specification", text, fixed = TRUE)))
    expect_true(any(grepl("clinical_data", text, fixed = TRUE)))
  })
})

test_that("the validation-messages table renders the empty state before a check", {
  # DT::renderDataTable() defaults to server-side processing, and in that mode
  # the row DATA (including this placeholder's own "No validation messages."
  # text) never reaches the value testServer can read -- it is registered for
  # a later AJAX fetch that shiny's MockShinySession deliberately refuses (see
  # the WHY comment on the escaping test in test-shinyapp-export.R for the
  # full trail). What IS visible without that round trip is which of the two
  # DT::datatable() calls in output$msgs ran: only the empty-state branch sets
  # `options = list(dom = "t")`, the real branch always adds "p" (pagination)
  # to that string. That is what this test pins.
  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    session$setInputs(up_1_1 = app_file_input("clinical_data.csv"))
    expect_equal(unname(rv$status[["clinical_data"]]), "pending")

    out <- unclass(output$msgs)
    expect_match(out, '"dom":"t"', fixed = TRUE)
    expect_no_match(out, '"dom":"tp"', fixed = TRUE)
  })
})

test_that("the validation-messages table renders genuine rows for a failing check", {
  # clinical_data_error_rules.csv against clinical_dta.yaml is a fixture pair
  # already pinned in test-clinical-error-fixtures.R: exactly 7 rule failures,
  # nothing on the column-spec or import axes, every message's source equal
  # to "rule". That is read here, not re-derived from this test's own run.
  #
  # The server-side wall above rules out reading the table body, but
  # output$msgs turns Dataset/Table/Source into factors before building the
  # widget (so the filter dropdowns have something to offer), and DT computes
  # those dropdown option lists from the real data up front -- unlike the row
  # data itself, they ARE embedded in the initial widget, one JSON string
  # escaped inside another. That is a real, if narrow, window onto genuine
  # content, and what the assertions below read.
  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    session$setInputs(up_1_1 = app_file_input("clinical_data_error_rules.csv"))
    session$setInputs(check_all = 1)
    expect_equal(unname(rv$status[["clinical_data"]]), "fail")

    out <- unclass(output$msgs)
    expect_match(out, '"dom":"tp"', fixed = TRUE)

    # Column headers, proving messages_display()'s selection/rename ran.
    for (h in c(">ID<", ">Dataset<", ">Table<", ">Rule<", ">Message<")) {
      expect_match(out, h, fixed = TRUE, info = h)
    }

    # Filter-dropdown option lists, proving genuine per-dataset messages (not
    # the empty placeholder, and not another dataset's rows) reached the
    # render: the active dataset's name, the uploaded file's own basename as
    # the source table, and "rule" as the only message source this fixture
    # produces.
    # (The value is quoted here with an escaped `\"` rather than `"`: `out`
    # is the RAW JSON text, one JSON string away from the browser's own
    # JSON.parse(), so the HTML attribute's own quotes are still
    # backslash-escaped at this level.)
    expect_match(out, 'data-options=\\"[&quot;clinical_data&quot;]\\"', fixed = TRUE)
    expect_match(out, 'data-options=\\"[&quot;clinical_data_error_rules&quot;]\\"', fixed = TRUE)
    expect_match(out, 'data-options=\\"[&quot;rule&quot;]\\"', fixed = TRUE)
  })
})
