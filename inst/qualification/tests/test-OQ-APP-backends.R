# The application and the two type systems. REQ-APP-051 .. REQ-APP-060, and
# REQ-APP-041 for the R cell types.
#
# A column's declared type is written with its type system in front, SAS Num or
# R numeric (REQ-TYPE-003). The key data_types.default of a library's
# dta-app.yaml names the type system the column editor and the Add dataset dialog
# PRESELECT for new columns and new matrix cells; it hides nothing, and a column
# or matrix that exists keeps its own. Every expected value below is written from
# a requirement sentence or from the YAML text a test writes itself, never taken
# from what the application returned.
#
# The configuration is read when a server starts, so each case writes the
# library's dta-app.yaml first (qapp_with_library()). What a user sees is read
# from the HTML the editors render, and what the app saves from the document it
# holds afterwards. The forms are filled the way a browser fills them: a browser
# reports every entry of the form it has just been given, which bka_report()
# reproduces from the rendered HTML rather than from the answer under test.

# The types of each type system, REQ-TYPE-003.
bka_sas_types <- c("Char", "Num", "Int", "Date", "Time", "DateTime")
bka_r_types <- c("character", "numeric", "integer", "logical", "Date", "POSIXct", "hms")

# One tabular dataset with a column of every type of both systems, in this order,
# and one column that declares no type.
bka_ids <- c(
  "S_CHAR", "S_NUM", "S_INT", "S_DATE", "S_TIME", "S_STAMP",
  "R_CHAR", "R_NUM", "R_INT", "R_LGL", "R_DATE", "R_STAMP", "R_STAMP0", "R_TIME",
  "UNTYPED"
)
bka_tabular <- c(
  "datasets:",
  "  - id: mixed",
  "    type: tabular",
  "    files: { filename: \"mixed.csv\", type: csv }",
  "    columns:",
  "      - { id: S_CHAR, type: SAS Char, length: 12, nullable: No }",
  "      - { id: S_NUM, type: SAS Num, format: \"SAS 8.2\" }",
  "      - { id: S_INT, type: SAS Int }",
  "      - { id: S_DATE, type: SAS Date, format: \"SAS DATE9.\" }",
  "      - { id: S_TIME, type: SAS Time, format: \"SAS TIME8.\" }",
  "      - { id: S_STAMP, type: SAS DateTime, format: \"SAS DATETIME20.\" }",
  "      - { id: R_CHAR, type: R character, length: 12 }",
  "      - { id: R_NUM, type: R numeric }",
  "      - { id: R_INT, type: R integer }",
  "      - { id: R_LGL, type: R logical }",
  "      - { id: R_DATE, type: R Date }",
  "      - { id: R_STAMP, type: R POSIXct, format: \"R %Y-%m-%dT%H:%M:%S\" }",
  "      - { id: R_STAMP0, type: R POSIXct }",
  "      - { id: R_TIME, type: R hms }",
  "      - { id: UNTYPED, nullable: Yes }"
)

# A matrix dataset, and four of them: three of R cells, one of SAS cells.
bka_matrix <- function(id, labels, cells) {
  c(
    paste0("  - id: ", id),
    "    type: matrix",
    paste0("    files: { filename: \"", id, ".tsv\", type: tsv }"),
    "    row_labels:",
    paste0("      - { id: GENE_ID, type: ", labels, ", nullable: No }"),
    "    matrix:",
    "      rows: { key: GENE_ID }",
    paste0("      cells: ", cells)
  )
}
bka_mx_num <- bka_matrix("mx_num", "R character", "{ type: R numeric, nullable: No, range: { min: 0 }, infinite: true }")
bka_mx_int <- bka_matrix("mx_int", "R character", "{ type: R integer, nullable: No }")
bka_mx_chr <- bka_matrix("mx_chr", "R character", "{ type: R character, nullable: No, length: 3 }")
bka_mx_sas <- bka_matrix("mx_sas", "SAS Char", "{ type: SAS Num, nullable: No, format: \"SAS 8.2\" }")
bka_matrices <- c("datasets:", bka_mx_num, bka_mx_int, bka_mx_chr, bka_mx_sas)

# The YAML text written to a file the calling test owns.
bka_write <- function(lines, env = parent.frame()) {
  path <- file.path(qa_tempdir(env), "doc.yaml")
  writeLines(lines, path)
  path
}

# ---- reading what an editor rendered --------------------------------------------

# The HTML of a renderUI output, as one string.
bka_html <- function(out) {
  if (is.null(out) || is.null(out$html)) "" else paste(as.character(out$html), collapse = "")
}

# The options of the <select> `id`, and the one selected.
bka_select <- function(html, id) {
  m <- regmatches(html, regexpr(sprintf('(?s)<select id="%s".*?</select>', id), html, perl = TRUE))
  if (length(m) == 0L) {
    return(list(choices = character(0), selected = character(0)))
  }
  opts <- regmatches(m, gregexpr('<option value="[^"]*"( selected)?', m))[[1]]
  value <- function(o) sub('^<option value="([^"]*)".*$', "\\1", o)
  list(choices = value(opts), selected = value(opts[grepl(" selected$", opts)]))
}

# One attribute of the <input> `id` (NA when there is no such input or attribute).
bka_input <- function(html, id, attr = "value") {
  m <- regmatches(html, regexpr(sprintf('<input id="%s"[^>]*>', id), html))
  if (length(m) == 0L) {
    return(NA_character_)
  }
  a <- regmatches(m, regexpr(sprintf(' %s="[^"]*"', attr), m))
  if (length(a) == 0L) NA_character_ else sub(sprintf('^ %s="(.*)"$', attr), "\\1", a)
}

# Whether the checkbox `id` is ticked.
bka_checked <- function(html, id) grepl(sprintf('<input id="%s"[^>]*checked', id), html)

# The text of the <textarea> `id`.
bka_text <- function(html, id) {
  m <- regmatches(html, regexec(sprintf('(?s)<textarea id="%s"[^>]*>(.*?)</textarea>', id), html, perl = TRUE))[[1]]
  if (length(m) < 2L) NA_character_ else m[[2]]
}

# The values, the labels and the chosen value of the radio group `id`.
bka_radios <- function(html, id) {
  g <- regmatches(html, regexpr(sprintf('(?s)<div id="%s".*?</div>\\s*</div>', id), html, perl = TRUE))
  if (length(g) == 0L) {
    return(list(values = character(0), labels = character(0), checked = character(0)))
  }
  r <- regmatches(g, gregexpr(sprintf('<input type="radio" name="%s" value="[^"]*"( checked="checked")?', id), g))[[1]]
  value <- function(x) sub('^.* value="([^"]*)".*$', "\\1", x)
  list(
    values = value(r),
    labels = sub("^<span>(.*)</span>$", "\\1", regmatches(g, gregexpr("<span>[^<]*</span>", g))[[1]]),
    checked = value(r[grepl("checked", r, fixed = TRUE)])
  )
}

# What the browser reports for the column form it has been given: every entry of
# the form, as the form shows it.
bka_report <- function(html) {
  list(
    col_id = bka_input(html, "col_id"), col_label = bka_input(html, "col_label"),
    col_backend = bka_select(html, "col_backend")$selected,
    col_type = bka_select(html, "col_type")$selected,
    col_format = bka_input(html, "col_format"), col_length = bka_input(html, "col_length"),
    col_nullable = bka_checked(html, "col_nullable"), col_values = bka_text(html, "col_values"),
    col_pattern = bka_input(html, "col_pattern"), col_examples = bka_text(html, "col_examples"),
    col_desc = bka_text(html, "col_desc")
  )
}

# The backend, the type, the format and the length a column form shows.
bka_shown <- function(html) {
  c(
    backend = bka_select(html, "col_backend")$selected,
    type = bka_select(html, "col_type")$selected,
    format = bka_input(html, "col_format"),
    length = bka_input(html, "col_length")
  )
}

# A missing entry as the empty text, so a declared type and format compare as text.
bka_chr <- function(x) if (is.null(x)) "" else as.character(x)

# The type, format and length a column of the held document is declared with.
bka_declared <- function(dta, dataset, id) {
  d <- as.list(datasets(dta, dataset)@specs@columns[[id]])
  c(type = bka_chr(d$type), format = bka_chr(d$format), length = bka_chr(d$length))
}

# ---- the column editor of a new column -----------------------------------------------

test_that("OQ-APP-061 | a new column's editor preselects the type system of data_types.default and its text type, lists that system's types and offers both backends | REQ-APP-051", {
  app <- qapp_skip_unless_app_installed()
  doc <- bka_write(bka_tabular)
  sas <- list(backend = "SAS", type = "Char", types = bka_sas_types, shows = "$9.", not = "%")
  r <- list(backend = "R", type = "character", types = bka_r_types, shows = "%Y-%m-%d", not = "$9.")
  cases <- list(
    list(label = "data_types.default R", lines = c("data_types:", "  default: R"), want = r),
    list(label = "data_types.default SAS", lines = c("data_types:", "  default: SAS"), want = sas),
    list(label = "a configuration with no data_types", lines = c("branding:", "  app_title: Qualification"), want = sas),
    list(label = "no configuration file", lines = NULL, want = sas)
  )
  for (case in cases) {
    qapp_with_library(case$lines)

    shiny::testServer(app, {
      session$setInputs(dta_file = qapp_upload_value(doc, "doc.yaml"))
      qapp_enter_edit_mode(session)
      session$setInputs(edit_cols = 1)
      session$setInputs(col_add = 1)
      html <- bka_html(output$col_modal_body)
      backend <- bka_select(html, "col_backend")
      type <- bka_select(html, "col_type")
      placeholder <- bka_input(html, "col_format", "placeholder")

      qa_step(paste(case$label, "- the backend preselected"), case$want$backend, backend$selected)
      qa_step(paste(case$label, "- the type preselected is its text type"), case$want$type, type$selected)
      qa_step(
        paste(case$label, "- the types listed are that system's"),
        sort(case$want$types, method = "radix"), sort(type$choices, method = "radix")
      )
      qa_step(
        paste(case$label, "- both backends are offered"),
        c("R", "SAS"), sort(backend$choices, method = "radix")
      )
      qa_check(
        paste(case$label, "- the format placeholder gives a format of that system"),
        grepl(case$want$shows, placeholder, fixed = TRUE) && !grepl(case$want$not, placeholder, fixed = TRUE)
      )
    })
  }
})

test_that("OQ-APP-062 | data_types.default is read as SAS or R in any letter case, any other value is one warning naming the key and SAS is used | REQ-APP-052", {
  app <- qapp_skip_unless_app_installed()
  doc <- bka_write(bka_tabular)
  cases <- list(
    list(value = "r", backend = "R", warns = FALSE),
    list(value = "'  R  '", backend = "R", warns = FALSE),
    list(value = "sas", backend = "SAS", warns = FALSE),
    list(value = "Python", backend = "SAS", warns = TRUE),
    list(value = "3", backend = "SAS", warns = TRUE),
    list(value = "''", backend = "SAS", warns = TRUE),
    list(value = "'   '", backend = "SAS", warns = TRUE),
    list(value = "[R, SAS]", backend = "SAS", warns = TRUE)
  )
  for (case in cases) {
    qapp_with_library(c("data_types:", paste0("  default: ", case$value)))
    warned <- character(0)

    withCallingHandlers(
      shiny::testServer(app, {
        session$setInputs(dta_file = qapp_upload_value(doc, "doc.yaml"))
        qapp_enter_edit_mode(session)
        session$setInputs(edit_cols = 1)
        session$setInputs(col_add = 1)
        html <- bka_html(output$col_modal_body)
        qa_step(
          sprintf("default: %s preselects the backend", case$value),
          case$backend, bka_select(html, "col_backend")$selected
        )
      }),
      warning = function(w) {
        warned <<- c(warned, cli::ansi_strip(conditionMessage(w)))
        invokeRestart("muffleWarning")
      }
    )
    qa_step(
      sprintf("default: %s raises %s warning naming data_types.default", case$value, if (case$warns) "one" else "no"),
      if (case$warns) 1L else 0L,
      sum(grepl("data_types.default", warned, fixed = TRUE))
    )
  }
})

test_that("OQ-APP-063 | an existing column opens with its own backend, type, format and length whatever the default is, saving it unchanged keeps them, and a column with no type opens with the default | REQ-APP-053", {
  app <- qapp_skip_unless_app_installed()
  doc <- bka_write(bka_tabular)
  # What the form shows, and what the column is declared with before and after.
  columns <- list(
    list(
      id = "R_STAMP", shown = c(backend = "R", type = "POSIXct", format = "%Y-%m-%dT%H:%M:%S", length = ""),
      declared = c(type = "R POSIXct", format = "R %Y-%m-%dT%H:%M:%S", length = "")
    ),
    list(
      id = "S_NUM", shown = c(backend = "SAS", type = "Num", format = "8.2", length = ""),
      declared = c(type = "SAS Num", format = "SAS 8.2", length = "")
    ),
    list(
      id = "R_CHAR", shown = c(backend = "R", type = "character", format = "", length = "12"),
      declared = c(type = "R character", format = "", length = "12")
    )
  )
  for (default in c("SAS", "R")) {
    qapp_with_library(c("data_types:", paste0("  default: ", default)))

    shiny::testServer(app, {
      session$setInputs(dta_file = qapp_upload_value(doc, "doc.yaml"))
      qapp_enter_edit_mode(session)
      session$setInputs(edit_cols = 1)
      n <- 0L
      for (col in columns) {
        n <- n + 1L
        session$setInputs(col_back = n)
        session$setInputs(col_edit_click = match(col$id, bka_ids))
        html <- bka_html(output$col_modal_body)
        qa_step(
          sprintf("%s under a default of %s opens as its own", col$id, default),
          col$shown, bka_shown(html)
        )
        do.call(session$setInputs, bka_report(html))
        session$setInputs(col_save = n)
        qa_step(
          sprintf("%s saved unchanged is still declared as it was", col$id),
          col$declared, bka_declared(rv$dta, "mixed", col$id)
        )
      }

      session$setInputs(col_back = n + 1L)
      session$setInputs(col_edit_click = match("UNTYPED", bka_ids))
      html <- bka_html(output$col_modal_body)
      qa_step(
        sprintf("a column with no type opens as the text type of the default %s", default),
        if (default == "R") c(backend = "R", type = "character") else c(backend = "SAS", type = "Char"),
        bka_shown(html)[c("backend", "type")]
      )
    })
  }
})

# ---- changing the Backend ----------------------------------------------------------------

test_that("OQ-APP-064 | changing the backend maps the type to the other backend's type of the same kind, empties the format, keeps the length of text only, and turns R logical into SAS Char with a notice | REQ-APP-054 | tags: white-box", {
  qapp_skip_unless_app_installed()
  switch_backend <- qapp_fn("dta_switch_backend")

  # The six kinds of column the two systems share (REQ-APP-054).
  pairs <- data.frame(
    sas = c("Char", "Num", "Int", "Date", "DateTime", "Time"),
    r = c("character", "numeric", "integer", "Date", "POSIXct", "hms"),
    stringsAsFactors = FALSE
  )
  for (i in seq_len(nrow(pairs))) {
    to_r <- switch_backend("SAS", "R", pairs$sas[[i]])
    qa_step(sprintf("SAS %s becomes R %s", pairs$sas[[i]], pairs$r[[i]]), pairs$r[[i]], to_r$type)
    qa_check(sprintf("with no notice (SAS %s)", pairs$sas[[i]]), is.null(to_r$notice))
    to_sas <- switch_backend("R", "SAS", pairs$r[[i]])
    qa_step(sprintf("R %s becomes SAS %s", pairs$r[[i]], pairs$sas[[i]]), pairs$sas[[i]], to_sas$type)
    qa_check(sprintf("with no notice (R %s)", pairs$r[[i]]), is.null(to_sas$notice))
  }

  # The format never survives: a SAS format and an R format are not interchangeable.
  qa_step("DATE9. is emptied going to R Date", "", switch_backend("SAS", "R", "Date", "DATE9.", "")$format)
  qa_step("DATETIME20. is emptied going to R POSIXct", "", switch_backend("SAS", "R", "DateTime", "DATETIME20.", "")$format)
  qa_step("%Y-%m-%d is emptied going to SAS Date", "", switch_backend("R", "SAS", "Date", "%Y-%m-%d", "")$format)
  qa_step("$9. is emptied although the type stays text", "", switch_backend("SAS", "R", "Char", "$9.", "")$format)

  # The length stays on a text type and goes from an R type that takes none.
  qa_step("a Char length of 12 stays on character", "12", switch_backend("SAS", "R", "Char", "", "12")$length)
  qa_step("a character length of 12 stays on Char", "12", switch_backend("R", "SAS", "character", "", "12")$length)
  qa_step("a Num length of 8 goes, R numeric takes none", "", switch_backend("SAS", "R", "Num", "8.2", "8")$length)
  qa_step("an Int length of 4 goes, R integer takes none", "", switch_backend("SAS", "R", "Int", "", "4")$length)
  qa_step("a Date length of 8 goes, R Date takes none", "", switch_backend("SAS", "R", "Date", "", "8")$length)

  # R logical has no SAS counterpart.
  logical <- switch_backend("R", "SAS", "logical", "", "3")
  qa_step("R logical becomes Char", "Char", logical$type)
  qa_check(
    "with a notice that names logical and SAS Char",
    is.character(logical$notice) && grepl("logical", logical$notice, fixed = TRUE) &&
      grepl("SAS Char", logical$notice, fixed = TRUE)
  )

  # Asking for the backend the form already holds changes nothing.
  same <- switch_backend("R", "R", "numeric", "keep", "7")
  qa_step(
    "the backend already held changes nothing",
    list(type = "numeric", format = "keep", length = "7", notice = NULL), same
  )
})

test_that("OQ-APP-065 | choosing the other backend in the editor shows the mapped type, that backend's types, an empty format, and keeps the ID, label, nullable and description | REQ-APP-054 | tags: white-box", {
  app <- qapp_skip_unless_app_installed()
  doc <- bka_write(bka_tabular)
  qapp_with_library(c("data_types:", "  default: SAS"))
  notes <- character(0)
  testthat::local_mocked_bindings(
    showNotification = function(ui, ...) {
      notes <<- c(notes, as.character(ui))
      invisible("id")
    },
    .package = "shiny"
  )

  shiny::testServer(app, {
    session$setInputs(dta_file = qapp_upload_value(doc, "doc.yaml"))
    qapp_enter_edit_mode(session)
    session$setInputs(edit_cols = 1)
    session$setInputs(col_add = 1)
    invisible(bka_html(output$col_modal_body))

    # The user fills in a SAS numeric column, then chooses the backend R.
    session$setInputs(
      col_id = "SCORE", col_label = "A score", col_type = "Num", col_format = "8.2",
      col_length = "8", col_nullable = FALSE, col_values = "", col_pattern = "",
      col_examples = "", col_desc = "typed"
    )
    session$setInputs(col_backend = "R")
    html <- bka_html(output$col_modal_body)
    qa_step(
      "SAS Num with a format and a length shows as R numeric with neither",
      c(backend = "R", type = "numeric", format = "", length = ""), bka_shown(html)
    )
    qa_step(
      "the types listed are the R ones",
      sort(bka_r_types, method = "radix"), sort(bka_select(html, "col_type")$choices, method = "radix")
    )
    qa_step(
      "the ID, the label, nullable and the description are as the user typed them",
      list(id = "SCORE", label = "A score", nullable = FALSE, desc = "typed"),
      list(
        id = bka_input(html, "col_id"), label = bka_input(html, "col_label"),
        nullable = bka_checked(html, "col_nullable"), desc = bka_text(html, "col_desc")
      )
    )

    # And back: the same kind of column in SAS.
    session$setInputs(col_type = "numeric", col_format = "", col_length = "")
    session$setInputs(col_backend = "SAS")
    html <- bka_html(output$col_modal_body)
    qa_step(
      "R numeric shows as SAS Num again, with the SAS types",
      list(shown = c(backend = "SAS", type = "Num", format = "", length = ""), types = sort(bka_sas_types, method = "radix")),
      list(shown = bka_shown(html), types = sort(bka_select(html, "col_type")$choices, method = "radix"))
    )

    # A format does not survive the switch.
    session$setInputs(col_type = "Date", col_format = "DATE9.")
    session$setInputs(col_backend = "R")
    html <- bka_html(output$col_modal_body)
    qa_step(
      "SAS Date with DATE9. shows as R Date with an empty format",
      c(backend = "R", type = "Date", format = "", length = ""), bka_shown(html)
    )

    # The length of a text type does.
    session$setInputs(col_type = "character", col_format = "", col_length = "12")
    session$setInputs(col_backend = "SAS")
    html <- bka_html(output$col_modal_body)
    qa_step(
      "R character with a length of 12 shows as SAS Char with the same length",
      c(backend = "SAS", type = "Char", format = "", length = "12"), bka_shown(html)
    )

    # R logical has no SAS counterpart.
    session$setInputs(col_backend = "R")
    invisible(bka_html(output$col_modal_body))
    session$setInputs(col_type = "logical", col_length = "")
    notes <<- character(0)
    session$setInputs(col_backend = "SAS")
    html <- bka_html(output$col_modal_body)
    qa_step("R logical shows as SAS Char", c(backend = "SAS", type = "Char"), bka_shown(html)[c("backend", "type")])
    qa_step("and one notice is shown", 1L, length(notes))
    qa_check(
      "that names logical and SAS Char",
      length(notes) == 1L && grepl("logical", notes, fixed = TRUE) && grepl("SAS Char", notes, fixed = TRUE)
    )
  })
})

test_that("OQ-APP-066 | a re-opened column editor shows the column's own backend, type and format, and the default for a new column, whatever the browser still holds | REQ-APP-055", {
  app <- qapp_skip_unless_app_installed()
  doc <- bka_write(bka_tabular)

  qapp_with_library(c("data_types:", "  default: SAS"))
  shiny::testServer(app, {
    session$setInputs(dta_file = qapp_upload_value(doc, "doc.yaml"))
    qapp_enter_edit_mode(session)
    session$setInputs(edit_cols = 1)

    # An R column is opened and the browser reports its form.
    session$setInputs(col_edit_click = match("R_NUM", bka_ids))
    html <- bka_html(output$col_modal_body)
    qa_step("R_NUM opens as R numeric", c("R", "numeric"), unname(bka_shown(html)[c("backend", "type")]))
    do.call(session$setInputs, bka_report(html))

    # A SAS column with a format is opened next. The server still holds the R
    # column's entries; the form must be the SAS column's own.
    session$setInputs(col_back = 1)
    session$setInputs(col_edit_click = match("S_NUM", bka_ids))
    html <- bka_html(output$col_modal_body)
    qa_step(
      "S_NUM opens as SAS Num with the format 8.2 although R entries are held",
      c(backend = "SAS", type = "Num", format = "8.2", length = ""), bka_shown(html)
    )

    # The browser reports the selects of the form it has just been given.
    session$setInputs(col_backend = "SAS", col_type = "Num")
    html <- bka_html(output$col_modal_body)
    qa_step(
      "that report changes neither the type nor the format shown",
      c(backend = "SAS", type = "Num", format = "8.2", length = ""), bka_shown(html)
    )
    do.call(session$setInputs, bka_report(html))
    session$setInputs(col_save = 1)
    qa_step(
      "and the column saved from it is still SAS Num with the format SAS 8.2",
      c(type = "SAS Num", format = "SAS 8.2", length = ""), bka_declared(rv$dta, "mixed", "S_NUM")
    )

    # A new column, with the R entries of the earlier form still held.
    session$setInputs(col_backend = "R", col_type = "integer")
    session$setInputs(col_back = 2)
    session$setInputs(col_add = 1)
    html <- bka_html(output$col_modal_body)
    qa_step(
      "a new column opens with the preselected SAS Char, not the R integer held",
      c(backend = "SAS", type = "Char"), bka_shown(html)[c("backend", "type")]
    )
    session$setInputs(col_backend = "SAS", col_type = "Char")
    html <- bka_html(output$col_modal_body)
    qa_step(
      "and the report of that form leaves it so",
      c(backend = "SAS", type = "Char"), bka_shown(html)[c("backend", "type")]
    )
  })

  # A library that prefers R: a SAS column is edited, the editor is closed and
  # opened again for a new column.
  qapp_with_library(c("data_types:", "  default: R"))
  shiny::testServer(app, {
    session$setInputs(dta_file = qapp_upload_value(doc, "doc.yaml"))
    qapp_enter_edit_mode(session)
    session$setInputs(edit_cols = 1)
    session$setInputs(col_edit_click = match("S_INT", bka_ids))
    html <- bka_html(output$col_modal_body)
    qa_step("S_INT opens as SAS Int", c("SAS", "Int"), unname(bka_shown(html)[c("backend", "type")]))
    do.call(session$setInputs, bka_report(html))

    session$setInputs(edit_cols = 2)
    session$setInputs(col_add = 1)
    html <- bka_html(output$col_modal_body)
    qa_step(
      "the editor opened again for a new column shows the preselected R character, not the SAS Int edited last",
      list(shown = c(backend = "R", type = "character"), types = sort(bka_r_types, method = "radix")),
      list(
        shown = bka_shown(html)[c("backend", "type")],
        types = sort(bka_select(html, "col_type")$choices, method = "radix")
      )
    )
  })
})

# ---- matrix datasets --------------------------------------------------------------------

test_that("OQ-APP-067 | the Add dataset dialog offers a matrix the three cell types of the default type system and no others, none preselected | REQ-APP-056", {
  app <- qapp_skip_unless_app_installed()
  doc <- bka_write(bka_tabular)
  sas <- list(
    values = c("SAS Num", "SAS Int", "SAS Char"),
    labels = c("SAS Num (decimal numbers)", "SAS Int (whole numbers)", "SAS Char (text)")
  )
  r <- list(
    values = c("R numeric", "R integer", "R character"),
    labels = c("R numeric (decimal numbers)", "R integer (whole numbers)", "R character (text)")
  )
  cases <- list(
    list(label = "data_types.default SAS", lines = c("data_types:", "  default: SAS"), want = sas),
    list(label = "data_types.default R", lines = c("data_types:", "  default: R"), want = r),
    list(label = "no configuration file", lines = NULL, want = sas)
  )
  for (case in cases) {
    qapp_with_library(case$lines)

    shiny::testServer(app, {
      session$setInputs(dta_file = qapp_upload_value(doc, "doc.yaml"))
      qapp_enter_edit_mode(session)
      session$setInputs(add_dataset_open = 1)
      radios <- bka_radios(bka_html(output$add_ds_body), "add_ds_mx_cells")

      # LIM-033: only the default system's cells are on offer.
      qa_step(
        paste(case$label, "- the cell types offered"),
        sort(case$want$values, method = "radix"), sort(radios$values, method = "radix")
      )
      qa_step(
        paste(case$label, "- and how each is labelled"),
        sort(case$want$labels, method = "radix"), sort(radios$labels, method = "radix")
      )
      qa_step(paste(case$label, "- none is preselected"), character(0), radios$checked)
    })
  }
})

test_that("OQ-APP-068 | adding a matrix takes the number, integer and text cell types of either system, builds its row label of that system's text type, and refuses any other type | REQ-APP-041", {
  app <- qapp_skip_unless_app_installed()
  doc <- bka_write(bka_tabular)
  qapp_with_library(NULL)
  # cell type chosen -> the type of the row label added with it
  accepted <- c(
    "SAS Num" = "SAS Char", "SAS Int" = "SAS Char", "SAS Char" = "SAS Char",
    "R numeric" = "R character", "R integer" = "R character", "R character" = "R character"
  )
  refused <- c(
    "R logical", "R Date", "R POSIXct", "R hms", "SAS Date", "SAS Time", "SAS DateTime",
    "Num", "numeric", "Python numeric"
  )

  shiny::testServer(app, {
    session$setInputs(dta_file = qapp_upload_value(doc, "doc.yaml"))
    qapp_enter_edit_mode(session)
    n <- 0L
    for (cell in names(accepted)) {
      n <- n + 1L
      id <- paste0("mx_", n)
      session$setInputs(
        add_ds_id = id, add_ds_type = "matrix", add_ds_mx_label = "GENE_ID",
        add_ds_mx_header = "id", add_ds_mx_cells = cell
      )
      session$setInputs(add_ds_save = n)
      qa_step(paste(cell, "- the dataset is added"), TRUE, id %in% names(rv$dta@datasets))
      ds <- rv$dta@datasets[[id]]
      label <- ds@specs@columns[["GENE_ID"]]
      qa_step(
        paste(cell, "- its row label is the text type of that system, not nullable, and the row key"),
        list(type = unname(accepted[[cell]]), nullable = FALSE, key = "GENE_ID"),
        list(type = as.list(label)$type, nullable = label@nullable, key = ds@matrix@row_key)
      )
      qa_step(paste(cell, "- and the cell type is the one chosen"), cell, as.list(ds@matrix)$cells$type)
    }

    # LIM-033: the dialog of a library that prefers SAS offers no R cell, yet the
    # R cell types sent above were accepted.
    for (cell in refused) {
      n <- n + 1L
      before <- names(rv$dta@datasets)
      session$setInputs(
        add_ds_id = "mx_refused", add_ds_type = "matrix", add_ds_mx_label = "GENE_ID",
        add_ds_mx_header = "id", add_ds_mx_cells = cell
      )
      session$setInputs(add_ds_save = n)
      qa_step(paste(cell, "- is refused and the document is unchanged"), before, names(rv$dta@datasets))
      qa_check(paste(cell, "- and the refusal is reported"), isFALSE(rv$add_ds_msg$ok))
    }
  })
})

test_that("OQ-APP-069 | the matrix-layout editor offers the three cell types of the matrix's own system with its own chosen, and a format field for a SAS matrix only | REQ-APP-057", {
  app <- qapp_skip_unless_app_installed()
  doc <- bka_write(bka_matrices)
  # Dataset position, the cell types the editor offers, the one chosen, and
  # whether a format field is on the page.
  r_cells <- c("R numeric", "R integer", "R character")
  s_cells <- c("SAS Num", "SAS Int", "SAS Char")
  matrices <- list(
    list(k = 1L, id = "mx_num", offers = r_cells, chosen = "R numeric", format_field = FALSE),
    list(k = 2L, id = "mx_int", offers = r_cells, chosen = "R integer", format_field = FALSE),
    list(k = 3L, id = "mx_chr", offers = r_cells, chosen = "R character", format_field = FALSE),
    list(k = 4L, id = "mx_sas", offers = s_cells, chosen = "SAS Num", format_field = TRUE)
  )
  for (default in c("SAS", "R")) {
    qapp_with_library(c("data_types:", paste0("  default: ", default)))

    shiny::testServer(app, {
      session$setInputs(dta_file = qapp_upload_value(doc, "doc.yaml"))
      qapp_enter_edit_mode(session)
      for (m in matrices) {
        if (m$k > 1L) do.call(session$setInputs, stats::setNames(list(1L), paste0("selds_", m$k)))
        session$setInputs(edit_matrix = m$k)
        html <- bka_html(output$mx_modal_body)
        radios <- bka_radios(html, "mx_cell_type")
        # LIM-033: the matrix's own system only, whatever the library prefers.
        qa_step(
          sprintf("%s under a default of %s offers its own system's cell types", m$id, default),
          sort(m$offers, method = "radix"), sort(radios$values, method = "radix")
        )
        qa_step(sprintf("%s under a default of %s has its own cell type chosen", m$id, default), m$chosen, radios$checked)
        qa_step(
          sprintf("%s under a default of %s %s a format field", m$id, default, if (m$format_field) "has" else "has no"),
          m$format_field, grepl('id="mx_cell_format"', html, fixed = TRUE)
        )
      }
    })
  }
})

test_that("OQ-APP-070 | saving the layout of an R matrix keeps its cell type with the prefix R and its cell settings, gives its cells no format, and writes a chosen cell type with the same prefix | REQ-APP-057", {
  app <- qapp_skip_unless_app_installed()
  doc <- bka_write(bka_matrices)
  qapp_with_library(c("data_types:", "  default: SAS"))

  shiny::testServer(app, {
    session$setInputs(dta_file = qapp_upload_value(doc, "doc.yaml"))
    qapp_enter_edit_mode(session)
    matrix_of <- function(id) as.list(rv$dta@datasets[[id]]@matrix)
    before <- list(mx_num = matrix_of("mx_num"), mx_int = matrix_of("mx_int"), mx_chr = matrix_of("mx_chr"))
    # The cell settings the YAML above wrote: the unchanged saves below are
    # compared with the document as loaded, so these anchor it to the YAML text.
    qa_step("the R numeric matrix was loaded with the cell type written", "R numeric", before$mx_num$cells$type)
    qa_step(
      "and the range minimum 0, Inf allowed",
      list(min = 0, infinite = TRUE),
      list(min = as.numeric(before$mx_num$cells$range$min), infinite = before$mx_num$cells$infinite)
    )
    qa_step("the R character matrix was loaded with the length 3", 3, as.numeric(before$mx_chr$cells$length))

    # The layout form of the SAS matrix is opened, and the browser reports the
    # Format field it holds; the R matrices' forms have no such field.
    session$setInputs(selds_4 = 1)
    session$setInputs(edit_matrix = 1)
    invisible(bka_html(output$mx_modal_body))
    session$setInputs(mx_cell_format = "8.2")

    saves <- 0L
    save_layout <- function(k, ...) {
      saves <<- saves + 1L
      do.call(session$setInputs, stats::setNames(list(saves), paste0("selds_", k)))
      session$setInputs(edit_matrix = 1L + saves)
      invisible(bka_html(output$mx_modal_body))
      session$setInputs(mx_row_key = "GENE_ID", ...)
      session$setInputs(mx_save = saves)
    }

    # Saved unchanged, each keeps what the YAML declared.
    for (k in 1:3) {
      id <- c("mx_num", "mx_int", "mx_chr")[[k]]
      save_layout(k)
      qa_step(
        paste(id, "- saved unchanged, the layout is as it was, with its type still prefixed R"),
        before[[id]], matrix_of(id)
      )
    }

    # With another setting edited, the rest stays, and no format appears.
    save_layout(1L, mx_cell_label = "Counts")
    after <- matrix_of("mx_num")
    qa_step("a label edited on the R numeric matrix is saved", "Counts", after$cells$label)
    qa_step("and the cell type is still R numeric", "R numeric", after$cells$type)
    qa_step("and its cells have no format, although the SAS form reported 8.2", NULL, after$cells$format)
    after$cells$label <- NULL
    qa_step("and nothing else changed, Inf allowed and the range included", before$mx_num, after)

    # Choosing another of the three types writes it with the same prefix.
    save_layout(1L, mx_cell_type = "R integer")
    qa_step("R numeric changed to R integer", "R integer", matrix_of("mx_num")$cells$type)
    save_layout(2L, mx_cell_type = "R character")
    qa_step("R integer changed to R character", "R character", matrix_of("mx_int")$cells$type)
    save_layout(3L, mx_cell_type = "R numeric")
    qa_step("R character changed to R numeric", "R numeric", matrix_of("mx_chr")$cells$type)
  })
})

test_that("OQ-APP-071 | a row label added to a matrix starts in the matrix's own type system and its text type, a column added to a tabular dataset starts with the default, whichever the default | REQ-APP-058", {
  app <- qapp_skip_unless_app_installed()
  tabular <- bka_tabular[-1L]
  doc <- bka_write(c("datasets:", bka_mx_num, bka_mx_sas, tabular))
  for (default in c("SAS", "R")) {
    # Dataset position, and the backend and type the editor preselects.
    want <- list(
      list(k = 1L, id = "mx_num", backend = "R", type = "character"),
      list(k = 2L, id = "mx_sas", backend = "SAS", type = "Char"),
      list(k = 3L, id = "mixed", backend = default, type = if (default == "R") "character" else "Char")
    )
    qapp_with_library(c("data_types:", paste0("  default: ", default)))

    shiny::testServer(app, {
      session$setInputs(dta_file = qapp_upload_value(doc, "doc.yaml"))
      qapp_enter_edit_mode(session)
      for (w in want) {
        if (w$k > 1L) do.call(session$setInputs, stats::setNames(list(1L), paste0("selds_", w$k)))
        session$setInputs(edit_cols = w$k)
        session$setInputs(col_add = w$k)
        html <- bka_html(output$col_modal_body)
        qa_step(
          sprintf("%s under a default of %s starts as its own system's text type", w$id, default),
          c(backend = w$backend, type = w$type), bka_shown(html)[c("backend", "type")]
        )
        qa_step(
          sprintf("%s under a default of %s still offers both backends", w$id, default),
          c("R", "SAS"), sort(bka_select(html, "col_backend")$choices, method = "radix")
        )
        session$setInputs(col_back = w$k)
      }
    })
  }
})

# ---- rules and saving ---------------------------------------------------------------------

test_that("OQ-APP-072 | the column choice of a range rule lists the number and integer columns of both type systems and no text, date, time or logical column | REQ-APP-059", {
  app <- qapp_skip_unless_app_installed()
  doc <- bka_write(bka_tabular)
  # SAS Num, SAS Int, R numeric and R integer; every other column of the document
  # is SAS Char, Date, Time, DateTime, R character, logical, Date, POSIXct, hms or has no type.
  numeric_columns <- c("S_NUM", "S_INT", "R_NUM", "R_INT")
  for (default in c("SAS", "R")) {
    qapp_with_library(c("data_types:", paste0("  default: ", default)))

    shiny::testServer(app, {
      session$setInputs(dta_file = qapp_upload_value(doc, "doc.yaml"))
      qapp_enter_edit_mode(session)
      session$setInputs(edit_rules = 1)
      session$setInputs(rule_add = 1)
      session$setInputs(rule_type = "col_range")
      picker <- bka_select(bka_html(output$rule_modal_body), "rule_col_single")
      qa_step(
        sprintf("under a default of %s the range rule is offered exactly the number and integer columns", default),
        sort(numeric_columns, method = "radix"), sort(setdiff(picker$choices, ""), method = "radix")
      )
    })
  }
})

test_that("OQ-APP-073 | the saved specification writes each type and format with its type system and the stored default format of an R date or time, reads back the same and saves identically | REQ-APP-060", {
  app <- qapp_skip_unless_app_installed()
  doc <- bka_write(bka_tabular)
  dta <- suppressMessages(read_dta_from_yaml(doc))
  save_text <- qapp_fn("dta_to_yaml_text")

  # What the text must hold per column: written from the declarations above and
  # the default formats of REQ-TYPE-016, nothing taken from the saved text.
  expected <- list(
    S_CHAR = c(type = "SAS Char", format = ""),
    S_NUM = c(type = "SAS Num", format = "SAS 8.2"),
    S_DATE = c(type = "SAS Date", format = "SAS DATE9."),
    R_CHAR = c(type = "R character", format = ""),
    R_NUM = c(type = "R numeric", format = ""),
    R_LGL = c(type = "R logical", format = ""),
    R_DATE = c(type = "R Date", format = "R %Y-%m-%d"),
    R_STAMP = c(type = "R POSIXct", format = "R %Y-%m-%dT%H:%M:%S"),
    R_STAMP0 = c(type = "R POSIXct", format = "R %Y-%m-%d %H:%M:%S"),
    R_TIME = c(type = "R hms", format = "R %H:%M:%S")
  )
  as_written <- function(text) {
    cols <- yaml::yaml.load(text)$datasets[[1]]$columns
    stats::setNames(
      lapply(cols, function(c) c(type = bka_chr(c$type), format = bka_chr(c$format))),
      vapply(cols, function(c) c$id, "")
    )
  }

  first <- save_text(dta)
  qa_check("the document is saved", isTRUE(first$ok))
  written <- as_written(first$value)
  for (id in names(expected)) {
    qa_step(paste(id, "is written with its type system in front"), expected[[id]], written[[id]])
  }

  path <- file.path(qa_tempdir(), "saved.yaml")
  writeLines(first$value, path)
  again <- suppressMessages(read_dta_from_yaml(path))
  qa_step(
    "reading the saved text back gives every column the type it was written with",
    vapply(expected, function(e) e[["type"]], ""),
    vapply(names(expected), function(id) bka_declared(again, "mixed", id)[["type"]], "")
  )
  qa_step("saving the document read back gives the identical text", first$value, save_text(again)$value)

  # LIM-034: a column added in the editor as R Date with the Format left empty is
  # saved with the default format, and so is a column whose format is emptied.
  qapp_with_library(NULL)
  shiny::testServer(app, {
    session$setInputs(dta_file = qapp_upload_value(doc, "doc.yaml"))
    qapp_enter_edit_mode(session)
    session$setInputs(edit_cols = 1)
    session$setInputs(col_add = 1)
    invisible(bka_html(output$col_modal_body))
    session$setInputs(
      col_id = "ADDED", col_label = "", col_backend = "R", col_type = "Date", col_format = "",
      col_length = "", col_nullable = TRUE, col_values = "", col_pattern = "",
      col_examples = "", col_desc = ""
    )
    session$setInputs(col_save = 1)
    qa_step(
      "a column added as R Date with an empty Format is saved with R %Y-%m-%d",
      c(type = "R Date", format = "R %Y-%m-%d"),
      as_written(save_text(rv$dta)$value)[["ADDED"]]
    )

    session$setInputs(col_back = 1)
    session$setInputs(col_edit_click = match("R_STAMP", bka_ids))
    html <- bka_html(output$col_modal_body)
    form <- bka_report(html)
    form$col_format <- ""
    do.call(session$setInputs, form)
    session$setInputs(col_save = 2)
    qa_step(
      "an R POSIXct column whose Format is emptied is saved with its type's default format",
      c(type = "R POSIXct", format = "R %Y-%m-%d %H:%M:%S"),
      as_written(save_text(rv$dta)$value)[["R_STAMP"]]
    )
  })
})
