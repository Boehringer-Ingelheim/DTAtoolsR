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

# The dataset Edit control: one button opening a menu over the four editors,
# plus a fifth, destructive action.
#
# The four editors all act on one object -- this dataset -- so they read as
# one entry point rather than four sibling buttons competing with "Check this
# dataset" and the export. Columns, then rules, then files: the order the
# specification itself is written in, and the order a user fills it out.
# Metadata comes last because it describes the dataset rather than its
# contents -- the other three are what a user came to edit, and this one is
# the header they set once.
#
# Remove dataset sits below a divider, apart from the four editors: those open
# an editor to change something about the dataset, this one deletes the
# dataset and its loaded files outright, with no editor in between. The
# divider plus the danger styling on the row itself (ds-edit-item-danger,
# theme.R) are what keep a destructive action from reading like a fifth,
# equally-reversible editor.
#
# This stays a zero-argument function, same as before: the whole menu is
# hidden by its call site when edit mode is off, so nothing here needs to
# gate on an `editing` flag of its own.
ds_edit_menu <- function() {
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
      tags$li(ds_edit_menu_item(
        "edit_cols", "&#x1F4D0;", "Columns",
        "Names, types and allowed values"
      )),
      tags$li(ds_edit_menu_item(
        "edit_rules", "&#x2696;&#xFE0F;", "Rules",
        "Checks that span several columns"
      )),
      tags$li(ds_edit_menu_item(
        "edit_files", "&#x1F5C2;&#xFE0F;", "Files",
        "Expected files, and the upload slots they create"
      )),
      tags$li(ds_edit_menu_item(
        "edit_meta", "&#x1F4CB;", "Metadata",
        "Name, description and template details"
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

# The read-only counterpart of a textInput()/textAreaInput(), used on the
# Metadata tab while edit mode is off so the form shows plain values instead
# of editable controls.
#
# An unset value renders as an em dash rather than an empty gap, so the row
# still reads as a field that exists but is not filled in, instead of looking
# like a layout gap or a value that failed to render.
# The value is normalised the same way dta_dataset_meta_fields()'s g() does it:
# NULL, character(0) and NA all collapse to "". Going straight to nzchar() would
# abort on a length-0 value, because `if (logical(0))` is an error rather than
# FALSE -- and an unset S7 property reaches here as character(0), not NULL.
meta_field_text <- function(label, value) {
  v <- value %||% ""
  v <- if (length(v) == 0 || is.na(v[1])) "" else as.character(v)[1]
  div(
    class = "md-ro-field",
    div(class = "md-ro-label", label),
    div(class = "md-ro-value", if (nzchar(v)) v else "\u2014")
  )
}
