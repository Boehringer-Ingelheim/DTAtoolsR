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

# The click guard: an immediate busy state on every button, and the swallowing
# of the repeat clicks that a slow server invites.
#
# WHY THIS CANNOT BE DONE FROM THE SERVER. Two failure modes, neither fixable
# in R:
#
#   * actionButton. A click sets an input over the websocket, and R is
#     single-threaded -- so a click landing while an observer runs is queued
#     and replayed once it finishes. Three impatient clicks on "Check all" are
#     three full validations. Disabling the button from inside the observer
#     does not close the window: the observer only starts after the round-trip
#     the user is already tired of waiting for, and the extra clicks have been
#     recorded by then.
#   * downloadButton. Shiny renders it as <a href="session/.../download/id">
#     and the click is native browser navigation -- it never reaches the
#     websocket at all. The server cannot see the second click, let alone
#     ignore it. Two clicks are two downloads, always. (This is the same
#     property that makes a downloadButton un-disableable from the server;
#     see the dl_btn() comment in app.R.)
#
# A server-side re-entrancy flag is not an alternative, either: by the time a
# duplicate's observer runs, the first has already RETURNED, so any "in
# flight" flag is clear again. The only server-side option is a wall-clock
# debounce, which cannot tell an accidental double-click from a deliberate
# repeat -- it would silently discard real work. So the guard belongs in the
# browser, in the capture phase, ahead of Shiny's own click binding: a capture
# listener on document runs before the target, and stopPropagation() there
# keeps the event from ever reaching the element's own (bubble-phase)
# handlers.
#
# WHEN A BUTTON IS RELEASED. shiny.js keeps a `shiny-busy` class on <html> and
# fires shiny:busy / shiny:idle from a message the SERVER sends: shiny's
# Observer increments a session busy count when an observer is INVALIDATED --
# i.e. before the slow work starts -- and decrements it in the observer's
# finally, and the session writes those messages straight to the websocket
# rather than waiting for the reactive flush. Two consequences this design
# leans on: the browser learns the server is busy before the work begins, and
# shiny:idle cannot arrive before this click's own work has finished. That
# makes idle an exact release signal, never an early one.
#
# Download links get no such signal -- nothing about an HTTP file download is
# reported back to the page -- so they fall back to a fixed cooldown. Two
# seconds absorbs a double-click without fighting a user who genuinely wants
# the file twice.
#
# FAILING SAFE IS THE POINT. A guard that can stick is worse than no guard, so
# every hold carries two independent releases besides shiny:idle:
#   * a sanity release, taken only if the server never went busy at ALL since
#     the click. Not every button has an observer bound to it, and one that
#     has none produces no busy/idle pair -- without this it would sit dead
#     until the ceiling. It compares a busy-transition counter rather than
#     sampling `shiny-busy` at a deadline, so a click still travelling to a
#     slow server is not mistaken for one nothing will answer.
#   * a 30s ceiling for a lost idle (an error escaping shiny's handler). It
#     re-arms while <html> still carries shiny-busy, so a genuinely long
#     export is never released early by it -- but only a bounded number of
#     times, because the stuck thing can BE `shiny-busy`.
#   * a release on shiny:disconnected: once the socket is gone, no idle is
#     coming for anything still held.
#
# isTrusted is load-bearing rather than hygiene: download_trigger_js in app.R
# starts the export download by calling .click() on a hidden downloadButton.
# That synthetic event matches .shiny-download-link, and swallowing it would
# break the export outright. Only real user gestures are guarded, which also
# leaves any shinyjs::click() untouched.
#
# The busy LOOK is delayed by 120ms while the swallowing starts immediately: a
# dropdown item that opens a modal in 30ms should not flash a spinner, and a
# five-second export must show one.
click_guard_script <- function() {
  tags$script(HTML("
(function(){
  var GUARD_SELECTOR = '.action-button, .shiny-download-link';
  var SPINNER_DELAY  = 120;   // ms before the busy look appears
  // Long enough to outlast a click's round trip on a slow link. A shorter
  // deadline is the trap: it can fire while the click is still in flight,
  // see an idle server, conclude no observer exists, and re-open the very
  // double-click window this guard is for. It only ever delays the release
  // of a button that has NO observer behind it, which costs nothing.
  var SANITY_HOLD    = 1500;
  var DOWNLOAD_HOLD  = 2000;  // ms a download link stays guarded
  var CEILING        = 30000; // ms failsafe release
  var CEILING_TRIES  = 4;     // ...re-armed at most this often while busy

  // Action buttons awaiting shiny:idle. Download links are never in here --
  // they have no idle to wait for.
  var pending = [];

  // Count of shiny:busy transitions seen. Compared against the value stamped
  // on a button when it was held, this answers 'did the server ever start
  // work for this click?' without racing the round trip.
  var busyTicks = 0;

  function serverBusy() {
    return document.documentElement.classList.contains('shiny-busy');
  }

  function clearTimer(el, name) {
    if (el[name]) { clearTimeout(el[name]); el[name] = null; }
  }

  function release(el) {
    if (!el) return;
    clearTimer(el, 'dtaBusyShow');
    clearTimer(el, 'dtaBusySanity');
    clearTimer(el, 'dtaBusyCeiling');
    el.classList.remove('dta-busy', 'dta-busy-shown');
    el.removeAttribute('aria-busy');
    var i = pending.indexOf(el);
    if (i > -1) pending.splice(i, 1);
  }

  function releaseAll() {
    pending.slice().forEach(function(el){ release(el); });
  }

  function armCeiling(el, tries) {
    el.dtaBusyCeiling = setTimeout(function(){
      el.dtaBusyCeiling = null;
      // Still demonstrably working: give it another window rather than
      // releasing a button whose job has not finished. BOUNDED, because the
      // scenario this failsafe exists for can be one where `shiny-busy` is
      // itself what is stuck -- a socket dropped after the busy message
      // arrived but before idle could. An unbounded re-arm would defer
      // forever in exactly the case it is meant to cover.
      if (serverBusy() && tries < CEILING_TRIES) { armCeiling(el, tries + 1); return; }
      release(el);
    }, CEILING);
  }

  function hold(el, isDownload) {
    el.classList.add('dta-busy');
    el.setAttribute('aria-busy', 'true');
    el.dtaBusyShow = setTimeout(function(){
      el.dtaBusyShow = null;
      el.classList.add('dta-busy-shown');
    }, SPINNER_DELAY);

    if (isDownload) {
      el.dtaBusyCeiling = setTimeout(function(){ release(el); }, DOWNLOAD_HOLD);
      return;
    }

    pending.push(el);
    el.dtaBusyTick = busyTicks;
    el.dtaBusySanity = setTimeout(function(){
      el.dtaBusySanity = null;
      // No busy transition since this click, and nothing running now: there
      // is no observer behind this button, so no idle is ever coming.
      if (busyTicks === el.dtaBusyTick && !serverBusy()) release(el);
    }, SANITY_HOLD);
    armCeiling(el, 0);
  }

  document.addEventListener('click', function(ev){
    // Synthetic clicks -- download_trigger_js, shinyjs::click() -- must pass
    // through untouched. See the comment on this function.
    if (!ev.isTrusted) return;
    var el = ev.target && ev.target.closest ? ev.target.closest(GUARD_SELECTOR) : null;
    if (!el) return;
    if (el.classList.contains('dta-busy')) {
      ev.preventDefault();
      ev.stopPropagation();
      return;
    }
    hold(el, el.classList.contains('shiny-download-link'));
  }, true);

  // These are jQuery-triggered events: a native addEventListener would never
  // see them. jQuery is a shiny dependency and is in <head> before this
  // script, but the DOMContentLoaded fallback means a load order that put it
  // after would degrade to the sanity/ceiling releases rather than leaving
  // buttons stuck.
  function onShiny(name, fn) {
    if (window.jQuery) { window.jQuery(document).on(name, fn); return; }
    document.addEventListener('DOMContentLoaded', function(){
      if (window.jQuery) window.jQuery(document).on(name, fn);
    });
  }
  onShiny('shiny:busy', function(){ busyTicks++; });
  // Releasing every held button on ONE session-wide idle is exact only
  // because this app does no async work: R is single-threaded and the app
  // uses no promises/future/ExtendedTask, so observers run to completion in
  // turn and the session busy count reaches zero only once the queue --
  // including this click's own observer -- is empty. Introducing async work
  // would break that: a fast button could then be held until unrelated slow
  // work finished, and this would need to become per-button.
  onShiny('shiny:idle', releaseAll);
  // A dropped connection means no idle is coming for anything in flight.
  onShiny('shiny:disconnected', releaseAll);
})();
"))
}

# ---- Template picker: source status + diagnostics --------------------------
#
# "Create new from template" (app.R) is backed by dta_template_index_cached()
# (template_index.R), which can draw templates from several configured
# sources (template_sources.R) at once. These render the parts of that picker
# that are pure functions of the resolved source records, so they can be
# exercised without a running server -- matching this file's own stated
# purpose. The picker's reactive plumbing (the grouped selectInput, the
# version list, the refresh button) stays in app.R, alongside render_
# template_option_input() and friends, because it closes over `input`/`rv`.

# Human-readable age for a `stale_age` (seconds, from resolve_git_source(),
# template_sources.R), rounded to the coarsest unit that keeps the number
# meaningful ("just now", "5 minutes ago", "3 hours ago", "2 days ago") -- a
# raw second count is not something an admin reading the picker can act on at
# a glance.
format_stale_age <- function(seconds) {
  s <- suppressWarnings(as.numeric(seconds))
  if (length(s) == 0 || is.na(s) || s < 0) {
    return("unknown age")
  }
  if (s < 60) {
    return("just now")
  }
  mins <- floor(s / 60)
  if (mins < 60) {
    return(sprintf("%d minute%s ago", mins, if (mins == 1) "" else "s"))
  }
  hours <- floor(mins / 60)
  if (hours < 24) {
    return(sprintf("%d hour%s ago", hours, if (hours == 1) "" else "s"))
  }
  days <- floor(hours / 24)
  sprintf("%d day%s ago", days, if (days == 1) "" else "s")
}

# One resolved source's status line: its name and a scheme badge, plus --
# only when it is CURRENTLY being served from a stale cache (a failed git
# refresh that fell back to the last good checkout, see resolve_git_source())
# -- a warning naming how old that cache is. A source that failed outright
# (ok == FALSE) has no status of its own to show here; template_source_
# diagnostics_ui() below is where that belongs, because a failed source's
# only useful fact is its error.
template_source_status_row <- function(source) {
  if (!isTRUE(source$ok)) {
    return(NULL)
  }
  div(
    class = "tmpl-source-status",
    tags$strong(as.character(source$name %||% "")),
    " ",
    tags$span(class = "badge bg-secondary", as.character(source$scheme %||% "")),
    if (isTRUE(source$stale)) {
      tags$span(
        class = "msg-hint", style = "color:#b45309;",
        sprintf(" — serving a stale cache (%s)", format_stale_age(source$stale_age))
      )
    }
  )
}

# Failed-source diagnostics: name, scheme, and the error -- already redacted
# by resolve_template_source()/redact_secrets() (template_sources.R) before it
# ever reaches here, so this never has to know how to strip a credential
# itself. NULL when nothing failed, so a call site can splice this straight
# into a tagList() without an extra length() check first.
template_source_diagnostics_ui <- function(sources) {
  failed <- Filter(function(s) !isTRUE(s$ok), sources %||% list())
  if (length(failed) == 0) {
    return(NULL)
  }
  tagList(
    div(
      class = "msg-hint", style = "color:#b91c1c;",
      tags$strong("Some template sources could not be loaded:")
    ),
    tags$ul(lapply(failed, function(s) {
      tags$li(sprintf(
        "%s (%s): %s",
        as.character(s$name %||% ""), as.character(s$scheme %||% ""),
        as.character(s$error %||% "unknown error")
      ))
    }))
  )
}

# The read-only "where this document came from" block for the Metadata tab
# (output$metadata_editor, app.R), shown only when `prov` (DTAMetaData@
# template) is non-empty. Built entirely from meta_field_text() (this file),
# which never renders an input control -- so this block cannot become
# editable no matter what mode the tab is in. That is deliberate, not
# incidental: `template` is machine-owned (dta_metadata_machine_fields(),
# template_core.R), and neither an option effect nor a carry-over can ever
# write it (apply_metadata_carry_over() strips it before anything else runs),
# so a decorative, input-free render is the only thing that may show it here.
template_provenance_block <- function(prov) {
  prov <- prov %||% list()
  if (length(prov) == 0) {
    return(NULL)
  }
  ds_prov <- prov$datasets %||% list()
  ds_rows <- if (length(ds_prov) > 0) {
    lapply(ds_prov, function(d) {
      ndev <- length(d$deviations %||% list())
      meta_field_text(
        as.character(d$name %||% ""),
        sprintf(
          "%s@%s — %d deviation%s",
          as.character(d$template %||% ""), as.character(d$version %||% ""),
          ndev, if (ndev == 1) "" else "s"
        )
      )
    })
  } else {
    NULL
  }
  tagList(
    tags$hr(),
    div(class = "md-section-title", "Template provenance"),
    div(
      class = "msg-hint", style = "margin:-4px 0 8px;",
      "Where this document was created from. Machine-recorded, not editable here."
    ),
    meta_field_text(
      "Template",
      paste0(as.character(prov$id %||% ""), "@", as.character(prov$version %||% ""))
    ),
    meta_field_text("Source", as.character(prov$source %||% "")),
    meta_field_text(
      "Created",
      if (!is.null(prov$created) && length(prov$created) > 0) as.character(prov$created) else ""
    ),
    meta_field_text(
      "Lineage",
      if (length(prov$lineage %||% character(0)) > 0) {
        paste(as.character(prov$lineage), collapse = " → ")
      } else {
        ""
      }
    ),
    if (length(ds_rows) > 0) tagList(div(class = "section-label", "Datasets"), ds_rows)
  )
}
