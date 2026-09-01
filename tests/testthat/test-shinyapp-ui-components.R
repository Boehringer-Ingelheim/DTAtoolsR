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

test_that("the Edit menu offers columns, rules, files and details in that order", {
  html <- render_html(app_fn("ds_edit_menu")("tabular"))

  positions <- vapply(
    c("edit_cols", "edit_rules", "edit_files", "edit_meta"),
    function(id) regexpr(paste0("id=\"", id, "\""), html, fixed = TRUE)[[1]],
    numeric(1)
  )

  # Every row is present...
  expect_true(all(positions > 0))
  # ...and in the order a specification is read in, with details last: it
  # describes the dataset, the other three describe its contents.
  expect_false(is.unsorted(positions))
})

test_that("Columns and Rules are tabular-only; Files, Details and Remove are not", {
  # Both editors read ds@specs, a property only DTADataSetTabular has -- see
  # the WHY comment on ds_edit_menu() itself. A file dataset must still reach
  # its other three rows.
  tabular_html <- render_html(app_fn("ds_edit_menu")("tabular"))
  file_html <- render_html(app_fn("ds_edit_menu")("file"))

  expect_match(tabular_html, "id=\"edit_cols\"", fixed = TRUE)
  expect_match(tabular_html, "id=\"edit_rules\"", fixed = TRUE)

  expect_no_match(file_html, "id=\"edit_cols\"", fixed = TRUE)
  expect_no_match(file_html, "id=\"edit_rules\"", fixed = TRUE)
  expect_match(file_html, "id=\"edit_files\"", fixed = TRUE)
  expect_match(file_html, "id=\"edit_meta\"", fixed = TRUE)
  expect_match(file_html, "id=\"remove_dataset\"", fixed = TRUE)
})

test_that("Remove dataset is present and appears after Details", {
  html <- render_html(app_fn("ds_edit_menu")("tabular"))

  meta_pos <- regexpr("id=\"edit_meta\"", html, fixed = TRUE)[[1]]
  remove_pos <- regexpr("id=\"remove_dataset\"", html, fixed = TRUE)[[1]]

  expect_true(meta_pos > 0)
  expect_true(remove_pos > 0)
  expect_true(remove_pos > meta_pos)
})

test_that("a divider separates Remove dataset from the four editors", {
  # The divider (plus the danger styling on the row itself, asserted at the
  # CSS level in theme.R) is what keeps a destructive action from reading
  # like a fifth, equally-reversible editor -- assert it sits strictly
  # between the last editor row and the destructive one, not merely present
  # somewhere in the menu.
  html <- render_html(app_fn("ds_edit_menu")("tabular"))

  meta_pos <- regexpr("id=\"edit_meta\"", html, fixed = TRUE)[[1]]
  divider_pos <- regexpr("dropdown-divider", html, fixed = TRUE)[[1]]
  remove_pos <- regexpr("id=\"remove_dataset\"", html, fixed = TRUE)[[1]]

  expect_true(divider_pos > meta_pos)
  expect_true(divider_pos < remove_pos)
})

test_that("the Details row carries both bindings and names what it edits", {
  html <- render_html(app_fn("ds_edit_menu")("tabular"))

  expect_match(html, "id=\"edit_meta\"", fixed = TRUE)
  expect_match(html, "Details", fixed = TRUE)
  expect_match(html, "Name, description and template info", fixed = TRUE)
})

test_that("the Edit menu offers no control for a dataset's type", {
  # A dataset's type is fixed by its S7 class; the property is assignable but
  # doing so yields an object whose declared type and behaviour disagree. The
  # editor therefore offers no way in -- not even a disabled one, which would
  # still put an input id on the page.
  html <- render_html(app_fn("ds_edit_menu")("tabular"))

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
  html <- render_html(app_fn("ds_edit_menu")("tabular"))

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
  # ...and the one entry point is rendered where they used to be, passed the
  # active dataset's type (s$type) so it knows whether to offer Columns/Rules.
  expect_match(src, "ds_edit_menu(s$type)", fixed = TRUE)
})

test_that("the Ace resize script waits for the document before touching body", {
  # yaml_ace_resize_js is registered inside tags$head(), so it is evaluated
  # BEFORE <body> exists. Calling MutationObserver.observe(document.body) at
  # that point throws "parameter 1 is not of type 'Node'", which aborts the
  # whole IIFE -- and does so SILENTLY as far as the app is concerned: the
  # editor simply never gets its ResizeObserver, so dragging the resize handle
  # stretches the box while Ace goes on painting at the old height. Nothing
  # else depends on that script, so no other test would notice.
  #
  # Asserted on the source rather than by rendering, because the failure is in
  # the ORDER the browser evaluates head vs body, which no server-side render
  # reproduces.
  src <- app_source("app.R")

  expect_match(src, "observe(document.body", fixed = TRUE)
  expect_match(src, "document.readyState === 'loading'", fixed = TRUE)
  expect_match(src, "addEventListener('DOMContentLoaded', start)", fixed = TRUE)
})

# The Edit dropdown (edit_menu()) that replaced the old "Create new version"
# button and "Edit mode" switch in the brandbar. See the WHY comment on
# edit_menu() (ui_components.R) for why every route acts on the same object,
# and why the enable/stop pair is one row that flips rather than two rows.

test_that("edit_menu() offers all three routes into editing", {
  html <- render_html(app_fn("edit_menu")())

  expect_match(html, 'id="create_new_version"', fixed = TRUE)
  expect_match(html, 'id="enable_edit_mode"', fixed = TRUE)
  expect_match(html, 'id="create_new_document"', fixed = TRUE)
})

test_that("edit_menu()'s enable/stop rows are one toggle: exactly one is rendered, chosen by `editing`", {
  menu <- app_fn("edit_menu")
  closed <- render_html(menu(editing = FALSE))
  open <- render_html(menu(editing = TRUE))

  expect_match(closed, 'id="enable_edit_mode"', fixed = TRUE)
  expect_no_match(closed, 'id="stop_editing"', fixed = TRUE)

  expect_match(open, 'id="stop_editing"', fixed = TRUE)
  expect_no_match(open, 'id="enable_edit_mode"', fixed = TRUE)

  # Neither state can pass by rendering nothing: the other routes stay put.
  for (html in list(closed, open)) {
    expect_match(html, 'id="create_new_version"', fixed = TRUE)
    expect_match(html, 'id="create_new_document"', fixed = TRUE)
  }
})

test_that("edit_menu() keeps offering the way back in once a version entry is open", {
  # THE BUG THIS GUARDS: the enable row used to be withheld on entry_open,
  # which stays TRUE for the rest of the session once a version is created.
  # Creating a version and then stopping left no route back into edit mode.
  # See the WHY comment on edit_menu() (ui_components.R).
  menu <- app_fn("edit_menu")

  html <- render_html(menu(editing = FALSE, entry_open = TRUE))
  expect_match(html, 'id="enable_edit_mode"', fixed = TRUE)
})

test_that("edit_menu()'s enable row says where the changes are recorded", {
  # entry_open is wording only now, but it is wording that matters: with an
  # entry open the edits land in that version's change summary, with none
  # they are recorded nowhere at all.
  menu <- app_fn("edit_menu")

  expect_match(
    render_html(menu(editing = FALSE, entry_open = FALSE)),
    "Not recorded in the version history",
    fixed = TRUE
  )
  expect_match(
    render_html(menu(editing = FALSE, entry_open = TRUE)),
    "recorded in the version you created",
    fixed = TRUE
  )
})

test_that("create_new_document's row carries the danger class marking it destructive", {
  # Split on </li> rather than grepl() on the whole menu, so the assertion is
  # tied to THIS row's markup, not merely to the class appearing somewhere in
  # the menu.
  html <- render_html(app_fn("edit_menu")())
  rows <- strsplit(html, "</li>", fixed = TRUE)[[1]]
  row <- rows[grepl('id="create_new_document"', rows, fixed = TRUE)]

  expect_length(row, 1)
  expect_match(row, "ds-edit-item-danger", fixed = TRUE)
})

test_that("edit_menu()'s dropdown opens right-aligned", {
  # This toggle sits at the right-hand end of the brandbar (unlike
  # ds_edit_menu()'s own dropdown, which has no dropdown-menu-end) -- a menu
  # opening flush with its trigger's left edge would run past the right edge
  # of the viewport from this position. See the WHY comment on edit_menu().
  html <- render_html(app_fn("edit_menu")())
  expect_match(html, "dropdown-menu-end", fixed = TRUE)
})

test_that("edit_status_tag() says 'Edit mode' and is a label, not a control", {
  # One wording for one fact -- whether editing is allowed. It used to name
  # the route taken in ("Editing new version" / "Editing new document"),
  # three labels for a state the rest of the app treats as one flag. See the
  # WHY comment on edit_status_tag() (ui_components.R).
  html <- render_html(app_fn("edit_status_tag")())

  expect_match(html, "Edit mode", fixed = TRUE)
  expect_no_match(html, "Editing new", fixed = TRUE)

  # role="status" announces the mode change to a screen reader; brand-link
  # (the brandbar's clickable pills, theme.R) would make this read as
  # clickable when nothing reaches it -- see the WHY comment on
  # edit_status_tag().
  expect_match(html, 'role="status"', fixed = TRUE)
  expect_no_match(html, "brand-link", fixed = TRUE)
})

test_that("new_document_modal_body() names the current title and version, and prefills the version field with 0.1", {
  body <- app_fn("new_document_modal_body")
  html <- render_html(body("Clinical Data Specification", "1.0"))

  expect_match(html, "Clinical Data Specification", fixed = TRUE)
  expect_match(html, "v1.0", fixed = TRUE)
  expect_match(html, 'id="new_document_version"', fixed = TRUE)
  expect_match(html, 'value="0.1"', fixed = TRUE)
})
