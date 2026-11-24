# Vocabulary slots: what an author's selection does to the column it targets
# (REQ-TMPL-065). A small library is written once per test under qa_tempdir():
# one vocabulary of three visit codes, a dataset template that binds its VISIT
# column to all of them, and creation templates that offer one slot over it.

# The library's root. `env` is the frame it lives as long as: qa_tempdir()
# removes it when that frame ends, so it must be the test's, not this helper's.
tvs_library <- function(env = parent.frame()) {
  root <- file.path(qa_tempdir(env), "library")
  dir.create(file.path(root, "templates"), recursive = TRUE)
  dir.create(file.path(root, "vocabulary"))
  writeLines(
    c(
      "kind: dta_vocabulary", "id: visit", "version: \"1.0\"", "type: text",
      "terms:",
      "  - {code: SCR, label: Screening}",
      "  - {code: C1D1, label: Cycle 1 Day 1}",
      "  - {code: EOT, label: End of treatment}"
    ),
    file.path(root, "vocabulary", "visit.dta-vocabulary.yaml")
  )
  writeLines(
    c(
      "kind: dta_dataset_template", "id: bound_ds", "version: \"1.0\"",
      "dataset:",
      "  id: ds1",
      "  type: tabular",
      "  files: {filename: x.csv, type: csv}",
      "  columns:",
      "    - id: VISIT",
      "      type: SAS Char",
      "      values_from: visit@1.0"
    ),
    file.path(root, "templates", "bound_ds.dta-dataset-template.yaml")
  )
  root
}

# One creation template over the library, written to `<id>.dta-template.yaml`.
# Its dataset is imported from the dataset template (`kind = "imported"`), written
# out inline with the same column (`"inline"`), or a matrix whose first row label
# is bound the same way (`"matrix"`); `slot_lines` finish the one slot it offers.
tvs_template <- function(root, id, slot_lines, kind = "imported") {
  dataset <- switch(kind,
    imported = "  - template: bound_ds@1.0",
    inline = c(
      "  - id: ds1", "    type: tabular", "    files: {filename: x.csv, type: csv}",
      "    columns:", "      - id: VISIT", "        type: SAS Char", "        values_from: visit@1.0"
    ),
    matrix = c(
      "  - id: ds1", "    type: matrix", "    files: {filename: x.tsv, type: tsv}",
      "    row_labels:",
      "      - {id: VISIT, header: visit, type: SAS Char, nullable: No, values_from: visit@1.0}",
      "      - {id: OTHER, header: other, type: SAS Char, nullable: No}",
      "    matrix:", "      cells: {type: SAS Num}"
    )
  )
  segment <- if (identical(kind, "matrix")) "row_labels" else "columns"
  writeLines(
    c(
      "kind: dta_creation_template", paste0("id: ", id), "version: \"1.0\"",
      "base:", "  metadata:", "    title: Slotted",
      "datasets:", dataset,
      "vocabulary_slots:",
      "  - id: visit_choice",
      paste0("    target: datasets.ds1.", segment, ".VISIT.values"),
      "    vocabulary: visit@1.0",
      slot_lines
    ),
    file.path(root, "templates", paste0(id, ".dta-template.yaml"))
  )
  invisible(id)
}

# The engine over the library at `root`, as a function that creates a document
# from one of its templates with the given vocabulary selections.
tvs_creator <- function(root, .env = parent.frame()) {
  withr::local_envvar(
    c(DTATOOLS_TEMPLATE_SOURCES = paste0("qualification=dir:", root), DTATOOLS_TEMPLATE_INCLUDE_BUILTIN = "false"),
    .local_envir = .env
  )
  engine <- getFromNamespace(".dta_template_engine", "DTAtools")()
  engine$dta_template_index_invalidate()
  index <- engine$build_template_index(refresh = TRUE)
  function(id, selections = NULL) {
    loaded <- engine$load_template_definition(paste0(id, "@1.0"), index)
    qa_check(paste("template", id, "loads"), isTRUE(loaded$ok))
    engine$create_dta_from_template(
      loaded$value$def, loaded$value$path,
      index = index, vocab_selections = selections
    )
  }
}

# The targeted column's spec: a matrix keeps its row labels among its specs'
# columns, so one accessor serves all three shapes.
tvs_column <- function(built) built$value@datasets[[1]]@specs@columns[["VISIT"]]

test_that("OQ-TMPL-151 | choosing None builds the column with no permitted values; a chosen term, the default and the order are as selected | REQ-TMPL-065 | tags: white-box", {
  qapp_skip_unless_app_installed()
  root <- tvs_library()
  kinds <- c("imported", "inline", "matrix")
  for (kind in kinds) {
    tvs_template(root, paste0("default_", kind), "    default: [SCR, C1D1]", kind)
  }
  create <- tvs_creator(root)
  engine <- getFromNamespace(".dta_template_engine", "DTAtools")()

  for (kind in kinds) {
    id <- paste0("default_", kind)
    # The ordinary cases first, so the library is shown to constrain the column
    # at all: "no permitted values" below could otherwise be a library that
    # never gave it any.
    silent <- create(id, NULL)
    qa_check(paste(kind, ": a slot given no selection builds"), isTRUE(silent$ok))
    qa_step(paste(kind, ": no selection at all keeps the slot's default"), c("SCR", "C1D1"), tvs_column(silent)@values)

    one <- create(id, list(visit_choice = "EOT"))
    qa_step(paste(kind, ": a selected term is the column's only permitted value"), "EOT", tvs_column(one)@values)
    two <- create(id, list(visit_choice = c("EOT", "SCR")))
    qa_step(paste(kind, ": selected terms come out in the vocabulary's order"), c("SCR", "EOT"), tvs_column(two)@values)

    # None: neither the default (SCR, C1D1) nor the vocabulary (SCR, C1D1, EOT).
    none <- create(id, list(visit_choice = character(0)))
    qa_check(paste(kind, ": None builds the document"), isTRUE(none$ok))
    col <- tvs_column(none)
    qa_step(paste(kind, ": None leaves the column with no permitted values"), NULL, col@values)
    qa_step(paste(kind, ": and changes nothing else about the column"), c("VISIT", "SAS Char"), c(col@id, as.list(col)$type))

    written <- engine$dta_to_yaml_text(none$value)
    qa_check(paste(kind, ": the document serialises"), isTRUE(written$ok))
    parsed <- yaml::yaml.load(written$value)$datasets[[1]]
    entries <- if (identical(kind, "matrix")) parsed$row_labels else parsed$columns
    qa_step(paste(kind, ": the written column has no values"), NULL, entries[[1]]$values)
  }
})

test_that("OQ-TMPL-152 | an open slot adds the author's own terms after the vocabulary's; a slot that requires terms refuses too few; no selection leaves a slot without a default alone | REQ-TMPL-065 | tags: white-box", {
  qapp_skip_unless_app_installed()
  root <- tvs_library()
  kinds <- c("imported", "inline", "matrix")
  for (kind in kinds) {
    tvs_template(root, paste0("open_", kind), c("    mode: open", "    include: [SCR, C1D1]", "    default: [SCR]"), kind)
    tvs_template(root, paste0("needs_", kind), c("    default: [SCR]", "    min: 2"), kind)
    tvs_template(root, paste0("bare_", kind), character(0), kind)
  }
  create <- tvs_creator(root)

  for (kind in kinds) {
    # Only the author's own term: not the vocabulary's three, and not the two
    # the slot's include: offers either.
    open <- paste0("open_", kind)
    own <- create(open, list(visit_choice = "UNSCHED"))
    qa_check(paste(kind, ": a selection of only an own term builds"), isTRUE(own$ok))
    qa_step(paste(kind, ": it gives exactly that term"), "UNSCHED", tvs_column(own)@values)
    both <- create(open, list(visit_choice = c("UNSCHED", "SCR", "RETEST")))
    qa_step(
      paste(kind, ": offered terms come first, then the author's own in the order entered"),
      c("SCR", "UNSCHED", "RETEST"), tvs_column(both)@values
    )

    # A slot with min 2 refuses one term and none, naming itself and the floor,
    # and no document results; two terms satisfy it.
    needs <- paste0("needs_", kind)
    for (selection in list(character(0), "EOT")) {
      refused <- create(needs, list(visit_choice = selection))
      qa_step(paste(kind, ": a selection of", length(selection), "term(s) is refused by min 2"), FALSE, refused$ok)
      qa_step(paste(kind, ": and no document results"), TRUE, is.null(refused$value))
      qa_check(
        paste(kind, ": the refusal names the slot and the minimum"),
        grepl("visit_choice", refused$error, fixed = TRUE) && grepl("at least 2", gsub("\\s+", " ", refused$error), fixed = TRUE)
      )
    }
    ok <- create(needs, list(visit_choice = c("EOT", "SCR")))
    qa_step(paste(kind, ": two terms satisfy the same slot"), c("SCR", "EOT"), tvs_column(ok)@values)

    # No selection at all, and no default either: the column is as its dataset
    # template made it, which here is the whole vocabulary in its order.
    bare <- paste0("bare_", kind)
    qa_step(
      paste(kind, ": no selection and no default leaves the column bound to the whole vocabulary"),
      c("SCR", "C1D1", "EOT"), tvs_column(create(bare, NULL))@values
    )
    qa_step(paste(kind, ": None on the same slot removes it"), NULL, tvs_column(create(bare, list(visit_choice = character(0))))@values)
  }
})

test_that("OQ-TMPL-153 | the options dialog gives every slot the terms its control holds, so an empty control builds the column with no permitted values, with a default or without | REQ-TMPL-065", {
  app <- qapp_skip_unless_app_installed()
  root <- tvs_library()
  tvs_template(root, "dialog_default", "    default: [SCR, C1D1]")
  tvs_template(root, "dialog_bare", character(0))
  withr::local_envvar(c(
    DTATOOLS_TEMPLATE_SOURCES = paste0("qualification=dir:", root),
    DTATOOLS_TEMPLATE_INCLUDE_BUILTIN = "false"
  ))

  # `held` is what the browser reports for the control: its terms, or nothing
  # at all for a control that holds none (NULL, which setInputs() cannot send,
  # so the input is left unset). Pressing Create DTA is the last step.
  dialog_values <- function(id, held) {
    qapp_clean_session_files()
    qapp_fn("dta_template_index_invalidate")()
    out <- NULL
    shiny::testServer(app, {
      session$setInputs(dta_client_id = strrep("v", 32))
      session$setInputs(create_from_template = 1)
      invisible(output$template_picker_ui)
      session$setInputs(template_select_name = id, template_select_version = "1.0")
      session$setInputs(template_select_next = 1)
      if (!is.null(held)) {
        session$setInputs(tmpl_vocab_visit_choice = held)
      }
      session$setInputs(tmpl_carry_source = "none")
      session$setInputs(template_create_confirm = 1)
      out <<- list(
        created = !is.null(rv$dta),
        values = if (is.null(rv$dta)) NULL else rv$dta@datasets[[1]]@specs@columns[["VISIT"]]@values
      )
    })
    out
  }

  held_default <- dialog_values("dialog_default", c("SCR", "C1D1"))
  qa_check("a control still holding the default builds", held_default$created)
  qa_step("and the column gets the default", c("SCR", "C1D1"), held_default$values)

  one_term <- dialog_values("dialog_default", "EOT")
  qa_step("a control holding one term gives that term", "EOT", one_term$values)

  emptied <- dialog_values("dialog_default", character(0))
  qa_check("a control emptied after opening on a default builds", emptied$created)
  qa_step("and the column has no permitted values, not the default", NULL, emptied$values)

  untouched <- dialog_values("dialog_bare", NULL)
  qa_check("a control that never held a term builds", untouched$created)
  qa_step(
    "and the column of a slot without a default has no permitted values, not the vocabulary",
    NULL, untouched$values
  )
})
