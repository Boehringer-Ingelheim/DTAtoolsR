# Pure UI builders for the "Create new from template" flow.
#
# Everything here is a function of its arguments alone -- no reactives, no
# `input`, no `session` -- so the whole picker and the whole options dialog can
# be asserted on as rendered HTML from testthat, without a running server. That
# is the same reason the picker stays a declarative render rather than a set of
# update*Input() pushes: a client-side round trip is invisible to
# shiny::testServer(), and a UI that only exists after one cannot be tested.
#
# app.R holds the wiring (which reactive feeds which builder, which observer
# reads which input); this file holds the markup.

# Count + noun, with an explicit plural for the irregular ones ("party" ->
# "parties"). A dependency for this would be absurd, and "1 datasets" in a
# summary sentence is exactly the kind of tell the design pass rules out.
tmpl_count_label <- function(n, singular, plural = paste0(singular, "s")) {
  n <- as.integer(n)
  paste0(n, " ", if (identical(n, 1L)) singular else plural)
}

# The label the index carries for one id@version, or the raw id when the index
# does not know it -- an ancestor may be abstract, from a source that has since
# gone away, or simply never have been indexed. A breadcrumb that silently
# dropped such a link would misstate the family.
tmpl_index_label <- function(index, id, version) {
  if (is.null(index) || nrow(index) == 0) {
    return(id)
  }
  hit <- index[
    index$kind == "dta_creation_template" & index$id == id & index$version == version, ,
    drop = FALSE
  ]
  if (nrow(hit) == 0) {
    return(id)
  }
  lab <- as.character(hit$label[[1]] %||% "")
  if (nzchar(lab)) lab else id
}

# ---- Picker: search, source filter, highlighting ----------------------------

# The search field, built by hand rather than with textInput() for two reasons
# the wrapper cannot give: `type="search"` (the browser's own clear button, and
# Shiny's text binding accepts it -- it binds on
# `input[type=text], input[type=search], ...`), and the bare `autofocus`
# attribute, which is what lets the modal's un-suspend sweep find the field to
# focus. `autofocus = NA` renders the attribute with no value.
template_picker_search_ui <- function(value = "") {
  div(
    class = "form-group shiny-input-container tmpl-search",
    tags$label(class = "control-label", `for` = "template_search", "Search templates"),
    tags$input(
      id = "template_search", type = "search", class = "form-control",
      value = as.character(value %||% ""),
      placeholder = "Name, id, vendor, or a word from the description",
      autocomplete = "off", autofocus = NA
    )
  )
}

# The library filter, which only exists when there is a choice to make: with
# one configured library the control would be a single chip, which says nothing
# and costs a line of the dialog.
#
# A multi-select, because "these two of my three libraries" is a real question
# and a radio group could only ever answer "one" or "all". Nothing checked means
# every library (template_picker_restrict_sources(), template_index.R): Shiny
# reports an unchecked group and a not-yet-bound one identically, as NULL, so
# "none" is the only reading of NULL that is right in both cases.
#
# Checkbox chips stay scannable to about six libraries; past that they wrap
# into a block of noise, so the same choices become a multi-select dropdown.
# `counts`, named by library, puts the template count on each chip.
template_picker_source_ui <- function(sources, selected = character(0), counts = NULL) {
  sources <- as.character(sources %||% character(0))
  if (length(sources) < 2) {
    return(NULL)
  }
  selected <- intersect(as.character(selected %||% character(0)), sources)
  if (length(sources) > 6) {
    return(selectizeInput(
      "template_select_source", "Libraries",
      choices = sources, selected = selected, multiple = TRUE,
      options = list(placeholder = "All libraries", plugins = list("remove_button"))
    ))
  }
  labels <- lapply(sources, function(s) {
    if (!(s %in% names(counts))) {
      return(s)
    }
    tagList(s, span(class = "tmpl-chip-count", as.integer(counts[[s]])))
  })
  div(
    class = "tmpl-source-filter",
    checkboxGroupInput(
      "template_select_source", "Libraries",
      choiceNames = labels, choiceValues = sources, selected = selected, inline = TRUE
    )
  )
}

# Wrap every case-insensitive occurrence of any token in <mark>, as a tagList of
# plain strings and tag objects.
#
# Never HTML(): the text is a template author's label/id/description, and
# splicing it into a string of markup would make any template file able to
# inject script into the picker. Building tags instead leaves htmltools to
# escape every plain segment, which is why a label containing "<b>" comes out
# as text rather than as bold.
#
# Longest token first at each position, so "biomarker" wins over "bio" where
# both match, and matches never overlap.
template_picker_highlight <- function(text, tokens) {
  text <- as.character(text %||% "")
  tokens <- as.character(tokens %||% character(0))
  tokens <- unique(tokens[!is.na(tokens) & nzchar(tokens)])
  if (!nzchar(text) || length(tokens) == 0) {
    return(tagList(text))
  }
  tokens <- tokens[order(nchar(tokens), decreasing = TRUE)]

  # One case-insensitive regex pass per string, the tokens as alternatives with
  # the longest first, matching where the filter matches -- at the start of a
  # word (template_picker_token_regex(), template_index.R): the list
  # re-renders on every keystroke, and walking a few hundred descriptions
  # character by character is what made it lag. The matches are sliced out of
  # the ORIGINAL text, so a marked segment keeps the author's own casing
  # instead of being silently re-cased.
  hits <- gregexpr(template_picker_token_regex(tokens), text, ignore.case = TRUE, perl = TRUE)[[1]]
  if (identical(hits[[1]], -1L)) {
    return(tagList(text))
  }
  lens <- attr(hits, "match.length")
  n <- nchar(text)

  out <- list()
  plain_from <- 1L
  for (k in seq_along(hits)) {
    start <- hits[[k]]
    if (start > plain_from) {
      out[[length(out) + 1L]] <- substr(text, plain_from, start - 1L)
    }
    # .noWS: htmltools otherwise puts a line break either side of a child tag,
    # which the browser renders as a space -- "biomarker_gf_ acme".
    out[[length(out) + 1L]] <- tags$mark(substr(text, start, start + lens[[k]] - 1L), .noWS = "outside")
    plain_from <- start + lens[[k]]
  }
  if (plain_from <= n) {
    out[[length(out) + 1L]] <- substr(text, plain_from, n)
  }
  do.call(tagList, out)
}

# ---- Picker: the list ------------------------------------------------------

# The template list as ONE hand-built radio group.
#
# Hand-built rather than radioButtons() because the rows carry structure the
# wrapper cannot express: a data-family attribute per row, a data-top marker,
# and an `.open` class the client toggles on click. Shiny's radio binding does
# not care who built the markup -- it binds
# `.shiny-input-radiogroup` and reads `input:radio[name=<id>]` inside it -- so
# this is still an ordinary `input$template_select_name`.
#
# Collapsing families client-side is the point of all of it: with a few hundred
# templates a server round trip per expand would re-render the list, throwing
# away keyboard focus and scroll position on every click. The server renders the
# SELECTED family already open so the first paint agrees with what the script
# would do.
template_picker_list_ui <- function(entries, selected = NULL, show_source = FALSE,
                                    flat = FALSE, tokens = character(0),
                                    total = nrow(entries)) {
  n <- if (is.null(entries)) 0L else nrow(entries)
  total <- as.integer(total %||% n)

  if (n == 0) {
    return(div(
      class = if (flat) "tmpl-picker-list flat" else "tmpl-picker-list",
      p(
        class = "msg-hint",
        "No templates match. Clear the search or choose other libraries."
      )
    ))
  }

  count_text <- if (identical(as.integer(n), total)) {
    tmpl_count_label(n, "template")
  } else {
    paste0(n, " of ", tmpl_count_label(total, "template"))
  }

  # A `selected` that is not among the rows (a search just hid it, or the
  # caller passed NULL on first open) falls back to the first row, because
  # that is what Shiny's binding will report once it binds: the first radio of
  # a group with nothing checked. Rendering "nothing selected" would leave the
  # server's idea of the selection disagreeing with the client's.
  ids <- as.character(entries$id)
  sel <- as.character(selected %||% "")
  if (!nzchar(sel) || !(sel %in% ids)) {
    sel <- ids[[1]]
  }

  # Each row collapses under the nearest top row above it -- the rows arrive in
  # depth-first tree order (template_picker_lineage()), so that IS its visible
  # parent's family. Not the `family` column: that names the root ancestor,
  # which may be hidden (abstract, or in a library filtered out), and two
  # sibling top rows under one hidden root would then claim each other's
  # variants and open together. A child with no top row above it is a top row
  # itself, rather than a row nothing can ever expand.
  tops <- logical(n)
  group <- character(n)
  cur <- ""
  for (i in seq_len(n)) {
    tops[[i]] <- as.integer(entries$indent[[i]]) == 0L || !nzchar(cur)
    if (tops[[i]]) cur <- ids[[i]]
    group[[i]] <- cur
  }
  sel_family <- group[[match(sel, ids)]]

  rows <- lapply(seq_len(n), function(i) {
    id <- ids[[i]]
    family <- group[[i]]
    is_top <- tops[[i]]
    indent <- if (is_top) 0L else as.integer(entries$indent[[i]])
    depth <- as.integer(entries$depth[[i]])
    is_sel <- identical(id, sel)

    # A top row stands for its whole family while the family is collapsed, so
    # it says how many rows are hiding behind it. A search result is flat and
    # hides nothing, so it gets no badge.
    variants <- if (is_top && !flat) sum(group == family & !tops) else 0L

    classes <- "tmpl-row"
    if (flat || identical(family, sel_family)) {
      classes <- paste(classes, "open")
    }
    if (is_sel) {
      classes <- paste(classes, "selected")
    }

    desc <- as.character(entries$description[[i]] %||% "")
    lineage <- as.character(entries$lineage[[i]] %||% "")

    div(
      class = classes,
      `data-family` = family,
      `data-id` = id,
      `data-top` = if (is_top) "" else NULL,
      tags$label(
        tags$input(
          type = "radio", name = "template_select_name", value = id,
          checked = if (is_sel) NA else NULL
        ),
        div(
          class = "tmpl-entry",
          style = if (!flat && indent > 0L) paste0("margin-left:", indent * 18L, "px") else NULL,
          # Name, library and version on the line the eye scans down; the id,
          # the parent and the variants on a quieter line under it.
          div(
            class = "tmpl-title",
            tags$strong(
              class = "tmpl-label",
              template_picker_highlight(as.character(entries$label[[i]] %||% id), tokens)
            ),
            if (show_source) {
              span(class = "badge tmpl-source", as.character(entries$source_name[[i]] %||% ""))
            },
            span(class = "tmpl-version", paste0("version ", as.character(entries$version[[i]])))
          ),
          div(
            class = "tmpl-meta",
            span(class = "tmpl-id", template_picker_highlight(id, tokens)),
            if (depth > 0L && nzchar(lineage)) {
              span(class = "tmpl-lineage", paste0("extends ", lineage))
            },
            if (variants > 0L) {
              span(class = "tmpl-variants", tmpl_count_label(variants, "variant"))
            }
          ),
          if (nzchar(desc)) {
            div(class = "tmpl-desc msg-hint", template_picker_highlight(desc, tokens))
          }
        )
      )
    )
  })

  # The count is the box's sticky header (theme.R), so it stays in view however
  # far down the list the user is.
  div(
    class = if (flat) "tmpl-picker-list flat" else "tmpl-picker-list",
    p(class = "msg-hint tmpl-count", count_text),
    div(
      id = "template_select_name",
      class = "shiny-input-radiogroup shiny-input-container",
      role = "radiogroup",
      `aria-label` = "Templates",
      rows
    )
  )
}

# The picker's client-side half: family expand/collapse, Enter-to-continue, the
# report that the modal closed, and the focus nudge. The listeners are
# delegated document listeners guarded by one window flag, because the picker
# body is a renderUI output that is thrown away and rebuilt whenever the search
# or source filter changes -- a listener bound to the elements themselves would
# be re-bound on every keystroke.
#
# HTML() here is safe and necessary: this is our own script text, never a
# template author's.
template_picker_scripts <- function() {
  tags$script(HTML(paste(
    "(function(){",
    "  if (!window.DTA_pickerBound) {",
    "    window.DTA_pickerBound = true;",
    "    document.addEventListener('change', function(ev){",
    "      var t = ev.target;",
    "      if (!t || t.name !== 'template_select_name') return;",
    "      var row = t.closest('.tmpl-row');",
    "      if (!row) return;",
    "      var list = row.closest('.tmpl-picker-list');",
    "      if (!list) return;",
    "      var fam = row.getAttribute('data-family');",
    "      var rows = list.querySelectorAll('.tmpl-row');",
    "      for (var i = 0; i < rows.length; i++) {",
    "        rows[i].classList.toggle('open', rows[i].getAttribute('data-family') === fam);",
    "        rows[i].classList.toggle('selected', rows[i] === row);",
    "      }",
    "    });",
    # Enter sends the search text explicitly BEFORE clicking Next: the text
    # binding is debounced, so a click alone could run the handler against the
    # previous filter.
    # The picker's outputs go blank while its modal is closed (tmpl_picker_open,
    # app.R); this is what tells the server the modal closed -- for Cancel,
    # Escape and the backdrop as much as for "Use this template".
    "    document.addEventListener('hidden.bs.modal', function(ev){",
    "      if (!ev.target || !ev.target.querySelector) return;",
    "      if (!ev.target.querySelector('#template_picker_ui')) return;",
    "      if (window.Shiny && Shiny.setInputValue) {",
    "        Shiny.setInputValue('template_picker_closed', Date.now(), {priority: 'event'});",
    "      }",
    "    });",
    "    document.addEventListener('keydown', function(ev){",
    "      if (!ev.target || ev.target.id !== 'template_search') return;",
    "      if (ev.key !== 'Enter') return;",
    "      ev.preventDefault();",
    "      if (window.Shiny && Shiny.setInputValue) {",
    "        Shiny.setInputValue('template_search', ev.target.value);",
    "      }",
    "      var nx = document.getElementById('template_select_next');",
    "      if (nx) nx.click();",
    "    });",
    # A double-click on a row is "this one, go on". The two clicks before it
    # have already checked the radio, and Shiny sends that input ahead of the
    # button event, so Next acts on the row that was double-clicked.
    "    document.addEventListener('dblclick', function(ev){",
    "      if (!ev.target || !ev.target.closest || !ev.target.closest('.tmpl-row')) return;",
    "      var nx = document.getElementById('template_select_next');",
    "      if (nx) nx.click();",
    "    });",
    # The list is re-rendered whenever the search or the library filter
    # changes, and a selection that survives the change can land below the
    # fold. shiny:value fires before the new HTML is in, hence the timeout.
    "    if (window.jQuery) jQuery(document).on('shiny:value', function(ev){",
    "      if (ev.name !== 'template_picker_list') return;",
    "      setTimeout(function(){",
    "        var row = document.querySelector('.tmpl-picker-list .tmpl-row.selected');",
    "        if (row) row.scrollIntoView({block: 'nearest'});",
    "      }, 0);",
    "    });",
    "  }",
    # Runs on every render (not behind the flag): the body may arrive after the
    # modal was already shown, in which case nothing else will focus the field.
    # Focus is never stolen from someone already arrowing through the list.
    "  var box = document.getElementById('template_search');",
    "  if (box && document.querySelector('.modal.show')) {",
    "    var a = document.activeElement;",
    "    if (!(a && a.closest && a.closest('.tmpl-picker-list'))) box.focus();",
    "  }",
    "})();",
    sep = "\n"
  )))
}

# ---- Picker: the detail panel ----------------------------------------------

# "based on A (a@1.0) > B (b@1.0)" for a resolved lineage, or "" for a root.
#
# `lineage` arrives NEAREST-first from resolve_template_inheritance() (each hop
# conses its parent onto the front), and is read ROOT-first here: a breadcrumb
# that started at the immediate parent and walked backwards would read as the
# opposite claim about who extends whom.
template_lineage_text <- function(lineage, index) {
  refs <- as.character(lineage %||% character(0))
  refs <- refs[!is.na(refs) & nzchar(refs)]
  if (length(refs) == 0) {
    return("")
  }
  refs <- rev(refs)
  parts <- vapply(refs, function(ref) {
    id <- sub("@.*$", "", ref)
    version <- if (grepl("@", ref, fixed = TRUE)) sub("^[^@]*@", "", ref) else ""
    paste0(tmpl_index_label(index, id, version), " (", ref, ")")
  }, character(1), USE.NAMES = FALSE)
  paste0("based on ", paste(parts, collapse = " › "))
}

# The names of the datasets a definition builds -- one per `datasets:` entry,
# in the four shapes template_dataset_entry_kind() recognises. `[[` rather than
# `$` throughout: these are raw YAML entries, where `$`'s partial matching
# would let an unrelated key answer for a missing one.
tmpl_dataset_names <- function(def) {
  entries <- def$datasets %||% list()
  if (length(entries) == 0) {
    return(character(0))
  }
  out <- vapply(entries, function(e) {
    if (is.character(e)) {
      return(as.character(e)[[1]])
    }
    if (!is.list(e)) {
      return("")
    }
    as.character(e[["as"]] %||% e[["id"]] %||% e[["template"]] %||% "")
  }, character(1), USE.NAMES = FALSE)
  out[nzchar(out)]
}

# One sentence naming what this template will build and what it will ask for.
# Sentences rather than middle-dot-joined fragments, per the design pass: a
# meta strip reads as decoration, a sentence reads as information.
tmpl_detail_summary <- function(def) {
  if (!is.null(def$variants)) {
    return(paste0(
      "Choose ", def$variants$label %||% def$variants$id,
      " first. The selected platform determines the datasets and agreement options."
    ))
  }
  ds <- tmpl_dataset_names(def)
  n_opts <- length(def$options %||% list())
  n_party <- length(tryCatch(normalise_party_slots(def$party_slots), error = function(e) list()))
  n_vocab <- length(
    tryCatch(normalise_vocabulary_slots(def$vocabulary_slots), error = function(e) list())
  )

  builds <- if (length(ds) > 0) {
    paste0(
      "Builds ", tmpl_count_label(length(ds), "dataset"), ", ",
      paste(ds, collapse = ", "), "."
    )
  } else {
    "Builds no datasets."
  }
  asks <- paste0(
    "Asks for ", tmpl_count_label(n_opts, "option"), ", ",
    tmpl_count_label(n_party, "party", "parties"), " and ",
    tmpl_count_label(n_vocab, "vocabulary", "vocabularies"), "."
  )
  paste(builds, asks)
}

# The panel under the list: what exactly is selected, which version of it, what
# it is based on, and what it will do.
#
# The version control is rendered on BOTH branches. An invalid template is very
# often invalid only at one version, and hiding the version dropdown behind the
# error would leave the user with no way to reach a working one but to pick a
# different template entirely.
template_picker_detail_ui <- function(loaded, versions, selected_version, index) {
  version_ui <- selectInput(
    "template_select_version", "Version",
    choices = versions, selected = selected_version, width = "160px"
  )

  if (!isTRUE(loaded$ok)) {
    return(div(
      class = "tmpl-detail",
      version_ui,
      p(
        class = "text-danger",
        paste0(
          "This template cannot be used: ", as.character(loaded$error %||% "unknown error"),
          ". Choose another one, or fix the file in its source."
        )
      )
    ))
  }

  value <- loaded$value
  def <- value$def
  source_name <- as.character(value$source_name %||% "")
  lineage <- template_lineage_text(value$lineage, index)
  description <- as.character(def$description %||% "")

  div(
    class = "tmpl-detail",
    div(
      class = "tmpl-detail-head",
      tags$h5(class = "tmpl-detail-title", as.character(def$label %||% def$id %||% "")),
      div(
        class = "tmpl-detail-sub",
        span(class = "tmpl-id", as.character(def$id %||% "")),
        if (nzchar(source_name)) span(class = "badge tmpl-source", source_name)
      )
    ),
    version_ui,
    if (nzchar(lineage)) p(class = "tmpl-lineage msg-hint", lineage),
    if (nzchar(description)) p(class = "tmpl-detail-desc", description),
    div(class = "section-label", "What it creates"),
    p(class = "tmpl-summary", tmpl_detail_summary(def)),
    if (!is.null(index)) {
      # The history is extra information: a failure computing it must never
      # take the picker's detail panel down with it.
      history <- tryCatch(template_history_cached(index, value$id), error = function(e) list())
      template_history_ui(history, selected_version)
    }
  )
}

# ---- Version history (template_history.R) -----------------------------------

# One change as a list item: "Dataset 'gf', column 'AGE': type  integer -> numeric".
template_history_change_item <- function(row) {
  clip <- function(x) {
    if (is.na(x)) "" else if (nchar(x) > 200) paste0(substr(x, 1, 200), "...") else x
  }
  detail <- switch(row$change,
    added = if (is.na(row$to)) "added" else paste0("added: ", clip(row$to)),
    removed = if (is.na(row$from)) "removed" else paste0("removed (was: ", clip(row$from), ")"),
    paste0(clip(row$from), " → ", clip(row$to))
  )
  tags$li(span(class = "msg-hint", row$area), " ", row$what, " — ", detail)
}

template_history_entry_body <- function(entry, has_prior = FALSE) {
  if (!is.null(entry$error)) {
    lead <- if (identical(entry$type, "manual")) "Could not be read: " else "Could not be compared: "
    return(p(class = "text-danger", paste0(lead, entry$error)))
  }
  if (identical(entry$type, "manual")) {
    return(p(class = "tmpl-history-note", entry$note))
  }
  if (identical(entry$type, "initial")) {
    return(p(
      class = "msg-hint",
      if (has_prior) "First version in the template library." else "First version of this template."
    ))
  }
  if (nrow(entry$changes) == 0) {
    return(p(class = "msg-hint", "No change to what this template builds."))
  }
  tags$ul(lapply(seq_len(nrow(entry$changes)), function(i) {
    template_history_change_item(entry$changes[i, , drop = FALSE])
  }))
}

# What changed in the selected version, then the whole history behind a
# native <details> -- the picker is itself a modal, and Shiny has only one.
template_history_ui <- function(history, selected_version) {
  if (length(history) == 0) {
    return(NULL)
  }
  own <- Filter(function(e) e$type %in% c("version", "initial") && identical(e$version, selected_version), history)
  has_prior <- any(vapply(history, function(e) identical(e$type, "manual") && is.null(e$error), logical(1)))
  div(
    class = "tmpl-history",
    div(class = "section-label", "History"),
    if (length(own) > 0) {
      tagList(
        tags$strong(paste0("What changed in ", selected_version)),
        template_history_entry_body(own[[1]], has_prior)
      )
    },
    tags$details(
      tags$summary(sprintf("Full history (%d entries)", length(history))),
      lapply(history, function(e) {
        tagList(tags$strong(e$title), template_history_entry_body(e, has_prior))
      })
    )
  )
}

# ---- Options dialog: one control per option --------------------------------

template_variant_input <- function(def) {
  spec <- def$variants
  if (is.null(spec)) {
    return(NULL)
  }
  dta_template_validate_variants(def)
  label <- as.character(spec$label %||% spec$id)
  choices <- stats::setNames(
    vapply(spec$choices, function(choice) choice$value, character(1)),
    vapply(spec$choices, function(choice) choice$label %||% choice$value, character(1))
  )
  selectInput(
    paste0("tmpl_variant_", spec$id), label,
    choices = c(stats::setNames("", paste("Choose", label)), choices),
    selected = "", selectize = FALSE, width = "100%"
  )
}

# Build one input control for a creation-template option.
#
# An option that takes free text AND has `choices:` is a visible field plus a
# separate "Choose a suggestion" picker that fills it. A lone dropdown with
# typing hidden inside it reads as a closed list, so the field is the thing the
# row's label points at and the picker is a helper beside it. "(leave blank)"
# stays a real picker row: its `__blank__` sentinel empties the field.
#
# An option that only accepts its choices (`type: select`, or
# `allow_custom: false`) is a strict dropdown with the same "(leave blank)" row;
# collect_template_selections() reads that sentinel as an empty value.
#
# Whether typing is allowed follows the template, not the widget:
# `dta_template_allow_custom(opt, default = typ != "select")` means `type: text`
# permits it unless the template says otherwise, and `type: select` forbids it
# unless the template opts in.
#
# A default that is NOT among the choices (a vendor deviation overriding its
# base's suggestion) is added to the picker, so it is shown as selected instead
# of silently vanishing.
render_template_option_input <- function(opt, base_metadata = list()) {
  oid <- as.character(opt$id %||% "")
  if (!nzchar(oid)) {
    return(NULL)
  }
  iid <- paste0("tmpl_opt_", oid)
  label <- as.character(opt$label %||% oid)
  typ <- tolower(as.character(opt$type %||% "text"))
  def <- dta_template_default(opt, base_metadata)
  help <- as.character(opt$help %||% "")
  blank_val <- "__blank__"
  allow <- dta_template_allow_custom(opt, default = !identical(typ, "select"))
  def_chr <- if (is.null(def)) "" else as.character(def)[[1]]

  # The picker's rows -- "(leave blank)", the default when it is not itself a
  # suggestion, then the suggestions -- and the selection a strict dropdown
  # starts on. A suggestion picker starts on nothing (see with_suggestions()).
  picker_choices <- function(ch) {
    extra <- character(0)
    selected <- blank_val
    if (nzchar(def_chr)) {
      selected <- def_chr
      if (!(def_chr %in% unname(ch))) {
        extra <- stats::setNames(def_chr, def_chr)
      }
    }
    list(
      choices = c(stats::setNames(blank_val, "(leave blank)"), extra, ch),
      selected = selected
    )
  }

  # A strict dropdown: the value must be one of the choices.
  strict_select <- function(ch) {
    pc <- picker_choices(ch)
    selectizeInput(
      iid, NULL,
      choices = pc$choices, selected = pc$selected, multiple = FALSE, width = "100%",
      options = list(create = FALSE, placeholder = "Pick a value")
    )
  }

  # The field `value_tag` (id `iid`) with a picker that writes into it. The
  # picker's label names its field for screen readers, so the pickers of a
  # whole dialog are not a column of identical "Choose a suggestion"s. A
  # textarea has the picker above it; a one-line field has it below, so the
  # field -- what the row's label points at -- comes first in tab order.
  with_suggestions <- function(value_tag, ch, picker_first) {
    pc <- picker_choices(ch)
    on_change <- sprintf(
      paste0(
        "function(value) {",
        "var target = document.getElementById(%s);",
        "if (target) {",
        "target.value = value === %s ? '' : (value || '');",
        "target.dispatchEvent(new Event('input', { bubbles: true }));",
        "}",
        # Back to empty after every pick: selectize fires nothing for the
        # item that is already selected, so a picker left on the default
        # could never put the default (or "(leave blank)") back after typing.
        "this.clear(true);",
        "}"
      ),
      jsonlite::toJSON(iid, auto_unbox = TRUE),
      jsonlite::toJSON(blank_val, auto_unbox = TRUE)
    )
    picker <- selectizeInput(
      paste0(iid, "_suggestion"),
      tagList("Choose a suggestion", span(class = "visually-hidden", paste0(" for ", label))),
      choices = pc$choices, multiple = FALSE, width = "100%",
      options = list(
        create = FALSE, placeholder = "Pick to fill the field",
        onInitialize = I("function() { this.clear(true); }"),
        onChange = I(on_change)
      )
    )
    if (picker_first) tagList(picker, value_tag) else tagList(value_tag, picker)
  }

  ch <- if (typ %in% c("boolean", "textarea", "number")) character(0) else dta_template_choices(opt)
  strict <- length(ch) > 0 && !allow
  # The controls carry no label of their own: tmpl_field_row() puts the label
  # and the help text in the row's left column, so every control starts on the
  # same vertical line however long its help is.
  ctl <- switch(typ,
    # A switch whose state reads "Yes"/"No" beside it (CSS ::after), so the
    # row says what the document will hold, not just that a box is ticked.
    boolean = div(
      class = "tmpl-switch",
      checkboxInput(iid, NULL, value = identical(def, TRUE) || identical(def, "yes"))
    ),
    textarea = {
      suggestions <- dta_template_choices(opt)
      value <- textAreaInput(
        iid, NULL,
        value = def_chr, rows = 3, width = "100%"
      )
      if (length(suggestions) == 0) {
        value
      } else {
        with_suggestions(value, suggestions, picker_first = TRUE)
      }
    },
    number = numericInput(
      iid, NULL,
      value = suppressWarnings(as.numeric(def %||% 0)), width = "100%"
    ),
    if (strict) {
      strict_select(ch)
    } else if (length(ch) == 1) {
      # One suggestion and free text allowed: a picker of one is pointless, so
      # the suggestion is simply the field's starting text, edited in place in a
      # textarea the author can drag taller (vertical only: the control sits in
      # a fixed grid column that a wider box would overflow).
      textAreaInput(iid, NULL,
        value = if (is.null(def)) unname(ch[[1]]) else def_chr,
        width = "100%", resize = "vertical"
      )
    } else if (length(ch) > 1) {
      with_suggestions(
        textInput(iid, NULL, value = def_chr, width = "100%"),
        ch,
        picker_first = FALSE
      )
    } else {
      textInput(iid, NULL,
        value = def_chr,
        placeholder = "Type a value", width = "100%"
      )
    }
  )

  tmpl_field_row(
    # Only a strict dropdown hides its <select> and types into
    # "<id>-selectized"; everywhere else the row's label points at the field
    # that holds the value.
    for_id = if (strict) paste0(iid, "-selectized") else iid,
    label = label,
    hints = if (nzchar(help)) div(class = "msg-hint tmpl-help", help),
    control = ctl,
    # A textarea gets the row's full width under its label: three lines of
    # prose in the narrower control column wrap far too early.
    class = if (identical(typ, "textarea")) "tmpl-opt tmpl-opt-wide" else "tmpl-opt"
  )
}

# One row of the options dialog: what the field is -- its label, any badge,
# its help -- in the left column, the control in the right. Every section of
# the dialog (options, parties, vocabularies) is built from this, so labels,
# help and controls line up down the whole form.
#
# `group = TRUE` is for a control that is a set of inputs (radio buttons,
# checkboxes): a <label for=> cannot name a group, so the text becomes the
# group's aria-labelledby instead.
tmpl_field_row <- function(for_id, label, hints = NULL, control = NULL, class = NULL,
                           badge = NULL, group = FALSE) {
  label_id <- paste0(for_id, "-row-label")
  div(
    class = paste(c("tmpl-field", class), collapse = " "),
    div(
      class = "tmpl-field-text",
      if (group) {
        span(class = "tmpl-field-label", id = label_id, label, badge)
      } else {
        tags$label(class = "tmpl-field-label", `for` = for_id, label, badge)
      },
      hints
    ),
    if (group) {
      div(class = "tmpl-field-control", role = "group", `aria-labelledby` = label_id, control)
    } else {
      div(class = "tmpl-field-control", control)
    }
  )
}

# "Required" next to a label: a word rather than an asterisk, which needs a
# legend to mean anything.
tmpl_required_badge <- function() span(class = "tmpl-req", "Required")

# "error_handling" -> "Error handling".
tmpl_humanise <- function(x) {
  x <- gsub("_", " ", as.character(x), fixed = TRUE)
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}

# The group an option is listed under: the metadata block its `target:` writes
# to ("metadata.transmission.type" -> "transmission"), or "general" for a
# top-level field, an `effects:` longhand, or no target at all. Derived rather
# than declared, so a template needs no extra key for the dialog to read well.
tmpl_option_group <- function(opt) {
  target <- opt[["target"]]
  if (!is.character(target) || length(target) != 1) {
    return("general")
  }
  parts <- strsplit(target, ".", fixed = TRUE)[[1]]
  if (length(parts) >= 3 && identical(parts[[1]], "metadata") && nzchar(parts[[2]])) parts[[2]] else "general"
}

# ---- Options dialog: vocabulary slots --------------------------------------

# Resolve every vocabulary slot of `def` into the item objects its selectize
# control needs, WITHOUT touching the UI -- so a slow or unreachable source is
# paid for once per modal open rather than once per re-render, and so a test can
# assert on the resolved terms directly.
#
# The shape is uniform on every path: list(slots = <unnamed list>, error = chr
# or NULL). `error` non-NULL means the slot LIST itself could not be read (a
# malformed `vocabulary_slots:` block); an individual slot that cannot resolve
# its vocabulary carries its own `error` and an empty `items`, because the rest
# of the template is still perfectly usable and one unreachable private
# vocabulary must not make document creation impossible.
template_vocab_slot_specs <- function(def, index) {
  slots <- tryCatch(
    normalise_vocabulary_slots(def$vocabulary_slots),
    error = function(e) e
  )
  if (inherits(slots, "condition")) {
    return(list(slots = list(), error = conditionMessage(slots)))
  }
  if (length(slots) == 0) {
    return(list(slots = list(), error = NULL))
  }

  resolve_vocab <- vocabulary_resolver(index)
  records <- lapply(slots, function(slot) {
    choices <- tryCatch(vocabulary_slot_choices(slot, resolve_vocab), error = function(e) e)
    if (inherits(choices, "condition")) {
      return(list(slot = slot, items = list(), error = conditionMessage(choices)))
    }
    items <- lapply(choices$terms, function(term) {
      code <- as.character(term$code)
      lab <- as.character(term$label %||% "")
      list(
        value = code,
        # "CODE - Label" so the row is readable, while the VALUE stays the bare
        # code: the label is authoring metadata and must never leak into a
        # column's permitted values.
        label = if (nzchar(lab) && !identical(lab, code)) paste0(code, " — ", lab) else code,
        description = as.character(term$description %||% "")
      )
    })
    list(slot = slot, items = items, error = NULL)
  })
  list(slots = records, error = NULL)
}

# The selectize configuration for one vocabulary slot.
#
# EVERY array here is an unnamed list on purpose. Shiny serialises this block
# with auto_unbox and keep_vec_names on, so a named character vector would
# arrive as a JSON object and a length-one vector as a bare scalar -- which
# silently breaks `options`, `items`, `searchField` and `plugins` for exactly
# the smallest, least-suspicious cases (one term, one default).
tmpl_vocab_selectize_options <- function(slot, items) {
  open <- identical(slot$mode, "open")
  render_option <- paste0(
    "{ option: function(item, escape) { ",
    "var d = item.description ? '<div class=\"vocab-opt-desc\">' + ",
    "escape(item.description) + '</div>' : ''; ",
    "return '<div class=\"vocab-opt\"><div>' + escape(item.label) + '</div>' + d + '</div>'; ",
    "} }"
  )
  list(
    options = items,
    items = as.list(as.character(slot$default %||% character(0))),
    valueField = "value",
    labelField = "label",
    searchField = list("label", "value", "description"),
    create = open,
    createOnBlur = open,
    plugins = list("remove_button"),
    closeAfterSelect = FALSE,
    hideSelected = TRUE,
    maxOptions = 10000,
    placeholder = "Type to search terms",
    # I() marks the string for eval on the client (Shiny renders it under
    # `data-eval`), which is what lets a render function reach selectize
    # without a JS file of our own.
    render = I(render_option)
  )
}

# One vocabulary slot: heading, quick actions, the multi-select, and its hints.
tmpl_vocab_slot_ui <- function(record) {
  slot <- record$slot
  iid <- paste0("tmpl_vocab_", slot$id)
  if (!is.null(record$error)) {
    return(tmpl_field_row(
      iid, slot$label,
      control = p(class = "text-danger tmpl-field-error", record$error),
      class = "tmpl-vocab"
    ))
  }
  items <- record$items
  defaults <- as.character(slot$default %||% character(0))

  # Client-side links rather than one observer per slot: these slots come and
  # go with every modal open, and server observers created there would
  # accumulate for the life of the session with nothing to destroy them.
  # DTA_vocabSet() (app.R) writes through selectize, so the server still sees
  # an ordinary input update.
  #
  # The onclick text is a CONSTANT: the link hands itself over and the helper
  # finds the control from it. A slot id -- template-authored text -- must
  # never be spliced into a JS string literal, where one quote would turn a
  # private template into script running in the app. The default values ride
  # in a data attribute, which htmltools escapes.
  actions <- list(
    tags$a(
      href = "#", class = "tmpl-vocab-action",
      onclick = "return DTA_vocabSet(this,'all')",
      paste0("All (", length(items), ")")
    ),
    tags$a(
      href = "#", class = "tmpl-vocab-action",
      onclick = "return DTA_vocabSet(this,'none')",
      "None"
    ),
    if (length(defaults) > 0) {
      tags$a(
        href = "#", class = "tmpl-vocab-action",
        onclick = "return DTA_vocabSet(this,'set')",
        # A unit separator, because a term code may legitimately contain a
        # comma, a space or a semicolon but never a control character.
        `data-vals` = paste(defaults, collapse = "\u001f"),
        paste0("Default (", length(defaults), ")")
      )
    },
    tagAppendAttributes(
      textOutput(paste0(iid, "_count"), inline = TRUE),
      class = "tmpl-vocab-count"
    )
  )

  # `.tmpl-vocab` is what DTA_vocabSet() climbs to from a link, and the first
  # <select> under it is the control it sets -- keep both on this row.
  tmpl_field_row(
    for_id = paste0(iid, "-selectized"),
    label = slot$label,
    badge = if (slot$min > 0) tmpl_required_badge(),
    hints = tagList(
      if (nzchar(slot$description)) div(class = "msg-hint tmpl-help", slot$description),
      # "Required" already says "at least one"; only a higher floor needs words.
      if (slot$min > 1) div(class = "msg-hint tmpl-help", paste0("Choose at least ", slot$min, "."))
    ),
    control = tagList(
      selectizeInput(
        iid, NULL,
        choices = NULL, multiple = TRUE, width = "100%",
        options = tmpl_vocab_selectize_options(slot, items)
      ),
      div(
        class = "tmpl-vocab-bar",
        div(class = "tmpl-vocab-actions", actions[1:3]),
        actions[[4]]
      ),
      if (identical(slot$mode, "open")) {
        div(class = "msg-hint tmpl-help", "Type a term and press Enter to add one that is not in the list.")
      }
    ),
    class = "tmpl-vocab"
  )
}

# The column editor's term picker: a checklist, not a chip box.
#
# Every term is a visible row -- code, label, description -- so a 3-term
# vocabulary is three clicks with nothing hidden, and a 300-term one stays a
# scrollable box of fixed height instead of 300 chips pushing the buttons
# off-screen. From 11 terms up a filter narrows the rows, and All/None act on
# the rows shown, so "all but these three" is: All, filter, untick.
# DTA_vocabFilter()/DTA_vocabCheck()/DTA_vocabCount() (app.R) do that
# client-side; the onclick/oninput text is constant, as in tmpl_vocab_slot_ui().
#
# `extras` are values the column holds that are no term of this vocabulary.
# They are listed first so that adding one term to a column does not quietly
# drop the column's own values on the way.
vocab_term_checklist <- function(id, terms, selected, extras = character(0)) {
  codes <- vapply(terms, function(t) as.character(t$code), character(1))
  term_row <- function(t) {
    lab <- as.character(t$label %||% "")
    desc <- as.character(t$description %||% "")
    tagList(
      tags$span(class = "vocab-code", t$code),
      if (nzchar(lab) && !identical(lab, t$code)) paste0(" — ", lab),
      if (nzchar(desc)) tags$span(class = "vocab-desc", desc)
    )
  }
  rows <- c(
    lapply(extras, function(v) {
      tagList(tags$span(class = "vocab-code", v), tags$span(class = "vocab-desc", "Not in this vocabulary"))
    }),
    lapply(terms, term_row)
  )
  values <- c(extras, codes)

  div(
    class = "vocab-pick",
    div(
      class = "vocab-pick-head",
      tags$strong("Terms"),
      div(
        class = "tmpl-vocab-actions",
        tags$a(href = "#", onclick = "return DTA_vocabCheck(this, true)", title = "Tick every term shown", "All"),
        tags$a(href = "#", onclick = "return DTA_vocabCheck(this, false)", title = "Untick every term shown", "None")
      ),
      tags$span(
        class = "tmpl-vocab-count vocab-pick-count", `aria-live` = "polite",
        sprintf("%d of %d selected", sum(values %in% selected), length(values))
      )
    ),
    if (length(values) > 10) {
      tags$input(
        type = "search", class = "form-control form-control-sm vocab-pick-filter",
        placeholder = sprintf("Filter %d terms by code, label or description", length(values)),
        `aria-label` = "Filter terms", oninput = "DTA_vocabFilter(this)",
        # Escape in a type=search field clears it and fires `search` only.
        onsearch = "DTA_vocabFilter(this)"
      )
    },
    div(
      class = "vocab-pick-list",
      checkboxGroupInput(id, NULL,
        choiceNames = rows, choiceValues = values,
        selected = intersect(values, selected), width = "100%"
      ),
      p(class = "msg-hint vocab-pick-empty", style = "display:none;", "No term matches the filter.")
    )
  )
}

# ---- Options dialog: the whole body ----------------------------------------

# One titled section of the options dialog. `key` is ours, never template
# text: the section navigation finds a section by it (template_options_scripts()).
tmpl_section <- function(key, title, hint = NULL, ...) {
  tags$section(
    class = "tmpl-sec", `data-sec` = key,
    div(
      class = "tmpl-sec-head",
      tags$h3(class = "tmpl-sec-title", title),
      if (!is.null(hint)) p(class = "msg-hint tmpl-sec-hint", hint)
    ),
    ...
  )
}

# The dialog's client-side half: the section links scroll the form, and the
# link of the section being read is highlighted as the form scrolls. Delegated
# document listeners behind one window flag, as in template_picker_scripts():
# the dialog is rebuilt on every open. The section keys are our own constants
# (tmpl_section()), so the selector built from one is never template text.
template_options_scripts <- function() {
  tags$script(HTML(paste(
    "(function(){",
    "  if (window.DTA_tmplOptsBound) return;",
    "  window.DTA_tmplOptsBound = true;",
    # A section near the end cannot scroll to the top of the form, so the
    # position alone would credit the LAST section after a click on one of
    # the others. The clicked link therefore holds until the smooth scroll it
    # started has settled.
    "  var pinnedUntil = 0;",
    "  function mark(body, key){",
    "    body.querySelectorAll('.tmpl-toc-link').forEach(function(a){",
    "      var on = a.getAttribute('data-sec') === key;",
    "      a.classList.toggle('active', on);",
    "      if (on) a.setAttribute('aria-current', 'true'); else a.removeAttribute('aria-current');",
    "    });",
    "  }",
    "  function spy(m){",
    "    if (Date.now() < pinnedUntil) return;",
    "    var body = m.closest('.tmpl-options-body');",
    "    var secs = m.querySelectorAll('.tmpl-sec');",
    "    if (!body || !secs.length) return;",
    "    var cur = secs[0];",
    "    for (var i = 0; i < secs.length; i++) {",
    "      if (secs[i].offsetTop - m.scrollTop <= 96) cur = secs[i];",
    "    }",
    "    if (m.scrollTop + m.clientHeight >= m.scrollHeight - 4) cur = secs[secs.length - 1];",
    "    mark(body, cur.getAttribute('data-sec'));",
    "  }",
    "  document.addEventListener('click', function(ev){",
    "    var a = ev.target && ev.target.closest ? ev.target.closest('.tmpl-toc-link') : null;",
    "    if (!a) return;",
    "    ev.preventDefault();",
    "    var body = a.closest('.tmpl-options-body');",
    "    var m = body ? body.querySelector('.tmpl-main') : null;",
    "    var sec = m ? m.querySelector('.tmpl-sec[data-sec=\"' + a.getAttribute('data-sec') + '\"]') : null;",
    "    if (!sec) return;",
    "    var calm = window.matchMedia && matchMedia('(prefers-reduced-motion: reduce)').matches;",
    "    mark(body, a.getAttribute('data-sec'));",
    "    pinnedUntil = Date.now() + (calm ? 150 : 900);",
    "    if (m.scrollHeight > m.clientHeight + 1) {",
    "      m.scrollTo({top: sec.offsetTop - 4, behavior: calm ? 'auto' : 'smooth'});",
    "    } else {",
    "      sec.scrollIntoView({block: 'start', behavior: calm ? 'auto' : 'smooth'});",
    "    }",
    "  });",
    "  document.addEventListener('scroll', function(ev){",
    "    var t = ev.target;",
    "    if (t && t.classList && t.classList.contains('tmpl-main')) spy(t);",
    "  }, true);",
    "})();",
    sep = "\n"
  )))
}

# Everything between the modal's title and its footer: a sidebar that says
# which template this is, what it builds and where each part of the form is,
# and the form itself, in sections of label-left / control-right rows.
#
# `vocab_specs` is passed in rather than computed here so the caller resolves
# each vocabulary once per open (template_vocab_slot_specs()); `has_open_doc`
# rather than a reachable `rv$dta` so this stays a pure function of arguments.
# Input ids are unchanged from the dialog this replaces -- tmpl_opt_<id>,
# tmpl_party_<id>, tmpl_vocab_<id>, tmpl_carry_* -- because the collectors in
# app.R read them by name. `aside_extra` is a sidebar block the server builds
# (the chosen platform and its "Change platform" button).
template_options_modal_body <- function(def, loaded, index, vocab_specs, has_open_doc = FALSE,
                                        aside_extra = NULL) {
  value <- loaded$value
  source_name <- as.character(value$source_name %||% "")
  version <- as.character(value$version %||% def$version %||% "")
  lineage <- template_lineage_text(value$lineage, index)
  description <- as.character(def$description %||% "")
  datasets <- tmpl_dataset_names(def)

  # -- Options, grouped by the metadata block they write to ------------------
  opts <- def$options %||% list()
  opts <- opts[vapply(opts, function(o) nzchar(as.character(o$id %||% "")), logical(1))]
  # Resolve ${today} for the preview too, so the dialog never offers a raw
  # token as a default where the created DTA would carry a date.
  base_md <- resolve_template_expressions(def$base$metadata %||% list(), dta_template_today_env())
  groups <- vapply(opts, tmpl_option_group, character(1))
  options_sec <- tmpl_section(
    "options", "Options",
    paste(
      "Pre-filled with the template's values. Everything here lands in the",
      "document's metadata and can be changed later."
    ),
    if (length(opts) == 0) {
      p(class = "tmpl-empty", "This template has no configurable options.")
    } else {
      lapply(unique(groups), function(g) {
        tagList(
          if (length(unique(groups)) > 1) tags$h4(class = "tmpl-group-title", tmpl_humanise(g)),
          div(
            class = "tmpl-opts-grid",
            lapply(opts[groups == g], render_template_option_input, base_metadata = base_md)
          )
        )
      })
    }
  )

  # -- Parties --------------------------------------------------------------
  slots <- normalise_party_slots(def$party_slots)
  profiles <- if (!is.null(index)) template_party_profiles(index) else list()
  party_sec <- if (length(slots) > 0) {
    tmpl_section(
      "parties", "Parties",
      "Keep the template's party, or replace it with a saved profile.",
      div(class = "tmpl-opts-grid", lapply(slots, function(slot) {
        # The first row is the template's own party, shown by name and
        # selected: the author sees what they get if they leave it alone. Its
        # value stays "", which collect_party_selections() reads as "leave the
        # slot's target untouched". With no party block in the template the
        # row says so, because leaving it empties the slot.
        party_block <- def$base$metadata[[slot$role]]
        base_name <- as.character(party_block[["affiliation"]][["name"]] %||% "")
        first_label <- if (nzchar(base_name)) {
          base_name
        } else if (is.null(party_block)) {
          "(none)"
        } else {
          "(as in template)"
        }
        choices <- stats::setNames("", first_label)
        eligible <- party_profiles_for_slot(profiles, slot)
        if (length(eligible) > 0) {
          profile_labels <- vapply(
            eligible, function(p) as.character(p$label %||% p$id), character(1)
          )
          # A saved profile that shares the template party's name would be a
          # second identical row.
          if (nzchar(base_name)) {
            same <- profile_labels == base_name
            profile_labels[same] <- paste0(profile_labels[same], " (saved profile)")
          }
          choices <- c(choices, stats::setNames(
            vapply(eligible, function(p) as.character(p$id), character(1)),
            profile_labels
          ))
        }
        iid <- paste0("tmpl_party_", slot$id)
        tmpl_field_row(
          for_id = iid,
          label = slot$label,
          badge = if (isTRUE(slot$required)) tmpl_required_badge(),
          hints = div(
            class = "msg-hint tmpl-help",
            if (length(eligible) > 0) {
              paste0(tmpl_count_label(length(eligible), "saved profile"), " available.")
            } else {
              "No saved profile fits this party."
            }
          ),
          # A plain <select>, not selectize: selectize drops an option whose
          # value is "" (its allowEmptyOption is off), and that is exactly the
          # template's-own-party row -- the control would open naming nothing,
          # with no way back to it once a profile had been picked.
          control = selectInput(iid, NULL,
            choices = choices, selected = "", selectize = FALSE, width = "100%"
          ),
          class = "tmpl-party"
        )
      }))
    )
  }

  # -- Controlled vocabularies ------------------------------------------------
  vocab_sec <- if (!is.null(vocab_specs$error)) {
    tmpl_section(
      "vocab", "Controlled vocabularies", NULL,
      p(class = "text-danger", paste(
        "This template's vocabulary slots could not be read:", vocab_specs$error
      ))
    )
  } else if (length(vocab_specs$slots) > 0) {
    tmpl_section(
      "vocab", "Controlled vocabularies",
      "Pick the terms this study collects. Each column keeps exactly the terms chosen here.",
      div(class = "tmpl-opts-grid", lapply(vocab_specs$slots, tmpl_vocab_slot_ui))
    )
  }

  # -- Carry-over -------------------------------------------------------------
  # "The open document" is offered ONLY when a document is actually open --
  # there is nothing to carry over from otherwise -- and is the default in that
  # case, since a user who already has a document open and is creating a
  # related one most often wants its relationship metadata to follow.
  carry_choices <- stats::setNames("none", "Nothing")
  if (isTRUE(has_open_doc)) {
    carry_choices <- c(carry_choices, stats::setNames("open", "The open document"))
  }
  carry_choices <- c(carry_choices, stats::setNames("file", "A DTA file"))
  carry_default <- if (isTRUE(has_open_doc)) "open" else "none"
  fields <- dta_template_metadata_fields()
  field_labels <- tmpl_humanise(fields)
  field_labels[fields == "study"] <- "Study identity"
  field_labels[fields == "sections"] <- "Agreement sections (replace template clauses)"

  carry_sec <- tmpl_section(
    "carry", "Carry over metadata",
    "Start from an existing DTA's parties, transfer details or study instead of the template's values.",
    tmpl_field_row(
      for_id = "tmpl_carry_source",
      label = "Copy from", group = TRUE,
      control = div(
        class = "tmpl-segmented",
        radioButtons("tmpl_carry_source", NULL,
          choices = carry_choices, selected = carry_default, inline = TRUE
        )
      )
    ),
    conditionalPanel(
      condition = "input.tmpl_carry_source == 'file'",
      tmpl_field_row(
        for_id = "tmpl_carry_file",
        label = "DTA file",
        hints = div(class = "msg-hint tmpl-help", "The YAML file to carry metadata over from."),
        control = fileInput("tmpl_carry_file", NULL, accept = c(".yaml", ".yml"), width = "100%")
      )
    ),
    conditionalPanel(
      condition = "input.tmpl_carry_source != 'none'",
      tmpl_field_row(
        for_id = "tmpl_carry_fields",
        label = "Fields to carry over", group = TRUE,
        hints = div(class = "msg-hint tmpl-help", "Ticked fields replace the template's values."),
        control = div(
          class = "tmpl-carry-fields",
          checkboxGroupInput("tmpl_carry_fields", NULL,
            choices = stats::setNames(fields, field_labels),
            selected = carry_over_default_fields(), width = "100%"
          )
        )
      )
    )
  )

  # -- Sidebar ----------------------------------------------------------------
  sections <- Filter(Negate(is.null), list(
    list(key = "options", title = "Options", n = length(opts)),
    if (!is.null(party_sec)) list(key = "parties", title = "Parties", n = length(slots)),
    if (!is.null(vocab_sec)) {
      list(key = "vocab", title = "Vocabularies", n = length(vocab_specs$slots))
    },
    list(key = "carry", title = "Carry over", n = NULL)
  ))
  toc <- tags$nav(
    class = "tmpl-toc", `aria-label` = "Sections of this form",
    div(class = "section-label", "In this form"),
    lapply(seq_along(sections), function(i) {
      s <- sections[[i]]
      tags$a(
        href = "#", class = if (i == 1) "tmpl-toc-link active" else "tmpl-toc-link",
        `data-sec` = s$key,
        span(s$title),
        if (!is.null(s$n)) span(class = "tmpl-toc-count", s$n)
      )
    })
  )

  aside <- tags$aside(
    class = "tmpl-aside",
    div(
      class = "tmpl-aside-block",
      div(class = "section-label", "Template"),
      div(
        class = "tmpl-head",
        span(class = "tmpl-id", as.character(def$id %||% "")),
        if (nzchar(version)) span(class = "tmpl-version", paste0("version ", version)),
        if (nzchar(source_name)) span(class = "badge tmpl-source", source_name)
      ),
      if (nzchar(lineage)) p(class = "tmpl-lineage msg-hint", lineage),
      if (nzchar(description)) p(class = "tmpl-aside-desc", description)
    ),
    if (length(datasets) > 0) {
      div(
        class = "tmpl-aside-block",
        div(class = "section-label", paste("Creates", tmpl_count_label(length(datasets), "dataset"))),
        tags$ul(class = "tmpl-ds-list", lapply(datasets, tags$li))
      )
    },
    aside_extra,
    toc
  )

  div(
    class = "tmpl-options-body",
    aside,
    div(
      class = "tmpl-main",
      options_sec,
      party_sec,
      vocab_sec,
      carry_sec
    ),
    template_options_scripts()
  )
}
