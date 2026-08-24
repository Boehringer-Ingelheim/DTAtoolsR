# UI building blocks that are pure functions of their arguments.
#
# Kept out of app.R so they can be rendered and asserted on without starting a
# server. Shiny/htmltools functions are called unqualified here, as in the other
# helper files: app.R starts with library(shiny) and the test harness sources
# these files into an environment whose parent is shiny's namespace.

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
ds_edit_menu_item <- function(id, icon, title, description) {
  actionLink(
    id,
    class = "dropdown-item ds-edit-item",
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

# The dataset Edit control: one button opening a menu over the four editors.
#
# They all act on one object -- this dataset -- so they read as one entry point
# rather than four sibling buttons competing with "Check this dataset" and the
# export. Columns, then rules, then files: the order the specification itself is
# written in, and the order a user fills it out. Metadata comes last because it
# describes the dataset rather than its contents -- the other three are what a
# user came to edit, and this one is the header they set once.
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
      ))
    )
  )
}
