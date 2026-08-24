# The dataset Edit menu.
#
# Three sibling buttons (Edit files / Edit columns / Edit rules) were replaced
# by one Edit button opening a menu. The input ids did NOT change, which is the
# whole point: every observer and every server test still drives edit_cols,
# edit_rules and edit_files directly, and those tests are what prove the menu
# is still wired to the editors. What is asserted here is the part only this
# component owns -- that the markup Shiny and Bootstrap each need is present,
# and that the rows are in the documented order.

render_html <- function(tag) {
  paste(as.character(tag), collapse = "\n")
}

test_that("the Edit menu offers columns, rules, files and metadata in that order", {
  html <- render_html(app_fn("ds_edit_menu")())

  positions <- vapply(
    c("edit_cols", "edit_rules", "edit_files", "edit_meta"),
    function(id) regexpr(paste0("id=\"", id, "\""), html, fixed = TRUE)[[1]],
    numeric(1)
  )

  # Every row is present...
  expect_true(all(positions > 0))
  # ...and in the order a specification is read in, with metadata last: it
  # describes the dataset, the other three describe its contents.
  expect_false(is.unsorted(positions))
})

test_that("the Metadata row carries both bindings and names what it edits", {
  html <- render_html(app_fn("ds_edit_menu")())

  expect_match(html, "id=\"edit_meta\"", fixed = TRUE)
  expect_match(html, "Metadata", fixed = TRUE)
  expect_match(html, "Name, description and template details", fixed = TRUE)
})

test_that("the Edit menu offers no control for a dataset's type", {
  # A dataset's type is fixed by its S7 class; the property is assignable but
  # doing so yields an object whose declared type and behaviour disagree. The
  # editor therefore offers no way in -- not even a disabled one, which would
  # still put an input id on the page.
  html <- render_html(app_fn("ds_edit_menu")())

  expect_no_match(html, "meta_type", fixed = TRUE)
  expect_no_match(app_source("app.R"), "input$meta_type", fixed = TRUE)
})

test_that("each Edit menu row carries both bindings it needs", {
  # `action-button` is what Shiny's click binding looks for; without it the row
  # renders but fires nothing. `dropdown-item` is what Bootstrap looks for to
  # close the menu on click; without it the menu stays open over the modal the
  # click just opened.
  html <- render_html(app_fn("ds_edit_menu_item")(
    "edit_cols", "&#x1F4D0;", "Columns", "Names, types and allowed values"
  ))

  expect_match(html, "action-button")
  expect_match(html, "dropdown-item")
  expect_match(html, "id=\"edit_cols\"", fixed = TRUE)
  expect_match(html, "Columns", fixed = TRUE)
  expect_match(html, "Names, types and allowed values", fixed = TRUE)
})

test_that("the Edit toggle is what opens the menu", {
  # Bootstrap opens a dropdown from the data-bs-toggle attribute alone. Without
  # it the button is inert and the menu is unreachable.
  html <- render_html(app_fn("ds_edit_menu")())

  expect_match(html, "data-bs-toggle=\"dropdown\"", fixed = TRUE)
  expect_match(html, "dropdown-menu", fixed = TRUE)
  expect_match(html, "aria-labelledby=\"ds_edit_toggle\"", fixed = TRUE)
})

test_that("the dataset actions no longer render three separate edit buttons", {
  # The regression this guards: re-adding a standalone button would put a
  # second control with the same input id on the page, and Shiny would bind
  # whichever it saw last.
  #
  # Matched on the actionButton() call, not on the label: "Edit columns" and
  # "Edit rules" still appear in app.R as the titles of the modals those ids
  # open, and always should.
  src <- app_source("app.R")

  expect_no_match(src, "actionButton(\"edit_cols\"", fixed = TRUE)
  expect_no_match(src, "actionButton(\"edit_rules\"", fixed = TRUE)
  expect_no_match(src, "actionButton(\"edit_files\"", fixed = TRUE)
  expect_no_match(src, "actionButton(\"edit_meta\"", fixed = TRUE)
  # ...and the one entry point is rendered where they used to be.
  expect_match(src, "ds_edit_menu()", fixed = TRUE)
})
