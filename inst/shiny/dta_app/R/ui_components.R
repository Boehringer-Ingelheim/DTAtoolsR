# UI building blocks that are pure functions of their arguments.
#
# Kept out of app.R so they can be rendered and asserted on without starting a
# server. Shiny/htmltools functions are called unqualified here, as in the other
# helper files: app.R starts with library(shiny) and the test harness sources
# these files into an environment whose parent is shiny's namespace.

# The single control that takes the app out of read-only mode.
#
# The app is read-only by default: every editing surface -- the dataset Edit
# menu, the Metadata tab, the Raw YAML editor, and adding or removing datasets
# -- is hidden or disabled until this switch is turned on. It must default to
# off, so opening (or reloading) the app never leaves a validated dataset
# editable by accident.
#
# This switch is the affordance, not the enforcement: the server
# independently guards each of those surfaces rather than trusting the
# client-side toggle state, because a client can be made to send
# input$edit_mode = TRUE regardless of what is actually drawn on screen.
edit_mode_switch <- function() {
  bslib::input_switch("edit_mode", "Edit mode", value = FALSE)
}

# One row of the dataset Edit menu: an icon tile, a title, and a one-line
# description of what that editor changes.
#
# `id` is the Shiny input id the row fires. The three editors keep the ids they
# have always had (edit_cols / edit_rules / edit_files), so their observers --
# and every test that drives them directly -- are untouched by the move from
# three buttons to one menu.
#
# actionLink() renders an <a class="action-button">, which is what Shiny's
# click binding looks for; adding Bootstrap's own `dropdown-item` alongside it
# is what makes the menu close on click.
#
# `class` appends extra CSS classes (e.g. "ds-edit-item-danger" for Remove
# dataset) without duplicating this function -- every row still gets the
# action-button/dropdown-item bindings above, only the visual treatment
# differs.
ds_edit_menu_item <- function(id, icon, title, description, class = NULL) {
  actionLink(
    id,
    class = paste(c("dropdown-item", "ds-edit-item", class), collapse = " "),
    label = tagList(
      span(class = "ds-edit-icon", HTML(icon)),
      span(
        class = "ds-edit-text",
        span(class = "ds-edit-title", title),
        span(class = "ds-edit-desc", description)
      )
    )
  )
}

# The dataset Edit control: one button opening a menu over up to four
# editors, plus a fifth, destructive action.
#
# The editors all act on one object -- this dataset -- so they read as one
# entry point rather than up to four sibling buttons competing with "Check
# this dataset" and the export. Columns, then rules, then files: the order
# the specification itself is written in, and the order a user fills it out.
# Details comes last because it describes the dataset rather than its
# contents -- the others are what a user came to edit, and this one is the
# header they set once.
#
# Columns and Rules are offered only when `type` is "tabular". Both editors
# work on ds@specs (a DTAColumnSpecCollection); a DTADataSetFile has no
# @specs property at all. Before this gate, opening either editor on a file
# dataset was not just unhelpful, it was actively wrong: dta_column_ids()
# swallows the missing property with tryCatch(...) %||% list() rather than
# erroring, so the column editor rendered empty and let the user "add" a
# column with nowhere to store it. `type` is the caller's to supply -- the
# call site in app.R already has the active dataset's type in hand (from
# build_structure()/rv$structure, or the dataset's own @type) and passes it
# straight through rather than this function looking it up itself.
#
# Remove dataset sits below a divider, apart from the editors above it: those
# open an editor to change something about the dataset, this one deletes the
# dataset and its loaded files outright, with no editor in between. The
# divider plus the danger styling on the row itself (ds-edit-item-danger,
# theme.R) are what keep a destructive action from reading like one more,
# equally-reversible editor.
#
# This still needs no `editing` flag of its own: the whole menu is hidden by
# its call site when edit mode is off, so the one argument it does take is
# only about WHICH editors apply to this dataset, not whether editing is
# allowed at all.
ds_edit_menu <- function(type = "tabular") {
  div(
    class = "dropdown ds-edit",
    tags$button(
      id = "ds_edit_toggle",
      class = "btn btn-outline-secondary dropdown-toggle ds-edit-toggle",
      type = "button",
      `data-bs-toggle` = "dropdown",
      `data-bs-auto-close` = "true",
      `aria-expanded` = "false",
      title = "Edit this dataset's specification",
      HTML("&#x270F;&#xFE0F; Edit")
    ),
    tags$ul(
      class = "dropdown-menu ds-edit-menu",
      `aria-labelledby` = "ds_edit_toggle",
      tags$li(tags$h6(class = "dropdown-header", "Edit specification")),
      # See the WHY comment above: a file dataset has no @specs, so a column
      # or rule editor would open onto nothing.
      if (identical(type, "tabular")) {
        tagList(
          tags$li(ds_edit_menu_item(
            "edit_cols", "&#x1F4D0;", "Columns",
            "Names, types and allowed values"
          )),
          tags$li(ds_edit_menu_item(
            "edit_rules", "&#x2696;&#xFE0F;", "Rules",
            "Checks that span several columns"
          ))
        )
      },
      tags$li(ds_edit_menu_item(
        "edit_files", "&#x1F5C2;&#xFE0F;", "Files",
        "Expected files, and the upload slots they create"
      )),
      tags$li(ds_edit_menu_item(
        "edit_meta", "&#x1F4CB;", "Details",
        "Name, description and template info"
      )),
      tags$li(tags$hr(class = "dropdown-divider")),
      tags$li(ds_edit_menu_item(
        "remove_dataset", "&#x1F5D1;&#xFE0F;", "Remove dataset",
        "Delete this dataset and its loaded files",
        class = "ds-edit-item-danger"
      ))
    )
  )
}

# Normalise a value that may arrive as NULL, character(0), NA, or a
# multi-element list/vector into a single display string. Shared by every
# read-only field renderer in this file (meta_field_text(),
# contact_detail_block()) because an unset S7 property and an unset YAML list
# element both show up as NULL/character(0)/NA: %||% only catches NULL, and
# going straight to nzchar() on the rest would abort, since `if (logical(0))`
# is an error rather than FALSE and `nzchar(NA)` is NA, not FALSE.
#
# Read-only mode is the ONLY place these fields render (see the WHY comment
# above contact_detail_block()), so this must never silently keep only the
# first element and drop the rest -- a YAML sequence (`address:` as several
# lines) or a nested mapping has to show everything it holds. unlist()
# flattens either shape to a plain vector; NA and empty elements are dropped
# (unlist() already drops NULL entries on its own, which is what keeps
# `address: [~]` from rendering as the literal string "NULL"); what remains
# is joined with ", " into one line, which is fine because the value span
# this feeds already sets `word-break`.
.ro_field_value <- function(value) {
  v <- value %||% ""
  if (length(v) == 0) {
    return("")
  }
  v <- unlist(v, use.names = FALSE)
  if (length(v) == 0) {
    return("")
  }
  v <- as.character(v)
  v <- v[!is.na(v) & nzchar(v)]
  if (length(v) == 0) "" else paste(v, collapse = ", ")
}

# The read-only counterpart of a textInput()/textAreaInput(), used on the
# Metadata tab while edit mode is off so the form shows plain values instead
# of editable controls.
#
# An unset value renders as an em dash rather than an empty gap, so the row
# still reads as a field that exists but is not filled in, instead of looking
# like a layout gap or a value that failed to render.
meta_field_text <- function(label, value) {
  v <- .ro_field_value(value)
  div(
    class = "md-ro-field",
    div(class = "md-ro-label", label),
    div(class = "md-ro-value", if (nzchar(v)) v else "\u2014")
  )
}

# The read-only counterpart of one contact row, used by render_contacts()
# (app.R) while edit mode is off.
#
# Editable mode only ever shows contact_display(person)'s short name/role
# summary per row, because the rest is one click away behind an actionLink()
# that opens an edit modal. Read-only has no click -- editing() gates the
# actionLink() itself away, not just what it would open -- so whatever this
# omits is simply unreachable to whoever is reading the page. This renders
# every field a contact can carry (email, department, phone, address, and the
# signature/reviewer flags; see inst/extdata/clinical_dta.yaml for a fixture
# using all of them), and omits a field entirely rather than printing an
# empty row, because a contact only has 2 of the 4 optional fields set far
# more often than it has all 4.
#
# `person` is a plain list parsed off the metadata YAML (dta_contacts(),
# utils_dta.R), not an S7 object -- a field that was never set just reads
# back as a missing list element (NULL) -- but .ro_field_value() handles
# NULL, character(0) and NA alike regardless, because a hand-edited YAML or
# a Raw-YAML apply can still produce any of those forms for a field that IS
# present in the list.
contact_detail_block <- function(person) {
  field_labels <- c(
    email = "Email", department = "Department",
    phone = "Phone", address = "Address"
  )
  rows <- lapply(names(field_labels), function(key) {
    v <- .ro_field_value(person[[key]])
    if (!nzchar(v)) {
      return(NULL)
    }
    div(
      class = "contact-detail-field",
      span(class = "contact-detail-label", field_labels[[key]]),
      span(class = "contact-detail-value", v)
    )
  })
  # isTRUE(), not a truthiness check: matches how the package itself reads
  # these two flags (.format_contact(), R/DTAMetaData-class.R), so a field
  # that is absent, FALSE, or anything other than TRUE stays unmentioned
  # rather than rendering a "Signature: no" row nobody asked for.
  flags <- c(
    if (isTRUE(person$signature)) "Signature",
    if (isTRUE(person$reviewer)) "Reviewer"
  )
  div(
    class = "contact-detail",
    div(class = "contact-detail-head", contact_display(person)),
    rows,
    if (length(flags) > 0) {
      div(
        class = "contact-detail-flags",
        lapply(flags, function(fl) span(class = "contact-detail-flag", fl))
      )
    }
  )
}
