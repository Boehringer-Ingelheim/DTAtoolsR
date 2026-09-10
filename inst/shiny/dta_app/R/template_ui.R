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

# The source filter, which only exists when there is a choice to make: with one
# configured library the control would be a radio group of one, which says
# nothing and costs a line of the dialog.
#
# Inline radios stay readable to about four options; past that they wrap into a
# block of noise, so the same choices become a dropdown. "All sources" is always
# first and always the empty value, which is what template_picker_filter()
# reads as "do not restrict".
template_picker_source_ui <- function(sources, selected = "") {
  sources <- as.character(sources %||% character(0))
  if (length(sources) < 2) {
    return(NULL)
  }
  choices <- c(stats::setNames("", "All sources"), stats::setNames(sources, sources))
  if (length(sources) <= 4) {
    radioButtons(
      "template_select_source", "Source",
      choices = choices, selected = selected, inline = TRUE
    )
  } else {
    selectInput("template_select_source", "Source", choices = choices, selected = selected)
  }
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
    out[[length(out) + 1L]] <- tags$mark(substr(text, start, start + lens[[k]] - 1L))
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
        "No templates match. Clear the search or pick another source."
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
  sel <- as.character(selected %||% "")
  if (!nzchar(sel) || !(sel %in% entries$id)) {
    sel <- as.character(entries$id[[1]])
  }
  sel_family <- as.character(entries$family[[match(sel, entries$id)]])

  rows <- lapply(seq_len(n), function(i) {
    id <- as.character(entries$id[[i]])
    family <- as.character(entries$family[[i]])
    indent <- as.integer(entries$indent[[i]])
    depth <- as.integer(entries$depth[[i]])
    is_top <- indent == 0L
    is_sel <- identical(id, sel)

    # A top row stands for its whole family while the family is collapsed, so
    # it says how many rows are hiding behind it. A search result is flat and
    # hides nothing, so it gets no badge.
    variants <- if (is_top && !flat) sum(entries$family == family & entries$indent > 0L) else 0L

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
          div(
            class = "tmpl-title",
            tags$strong(
              class = "tmpl-label",
              template_picker_highlight(as.character(entries$label[[i]] %||% id), tokens)
            ),
            if (show_source) {
              span(class = "badge tmpl-source", as.character(entries$source_name[[i]] %||% ""))
            },
            span(class = "tmpl-id", template_picker_highlight(id, tokens)),
            span(class = "tmpl-version", paste0("version ", as.character(entries$version[[i]]))),
            if (variants > 0L) {
              span(class = "tmpl-variants", tmpl_count_label(variants, "variant"))
            }
          ),
          if (depth > 0L && nzchar(lineage)) {
            div(class = "tmpl-lineage msg-hint", paste0("extends ", lineage))
          },
          if (nzchar(desc)) {
            div(class = "tmpl-desc msg-hint", template_picker_highlight(desc, tokens))
          }
        )
      )
    )
  })

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
    as.character(e[["as"]] %||% e[["name"]] %||% e[["template"]] %||% "")
  }, character(1), USE.NAMES = FALSE)
  out[nzchar(out)]
}

# One sentence naming what this template will build and what it will ask for.
# Sentences rather than middle-dot-joined fragments, per the design pass: a
# meta strip reads as decoration, a sentence reads as information.
tmpl_detail_summary <- function(def) {
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
      tags$strong(as.character(def$label %||% def$id %||% "")),
      span(class = "tmpl-id", as.character(def$id %||% "")),
      if (nzchar(source_name)) span(class = "badge tmpl-source", source_name)
    ),
    version_ui,
    if (nzchar(lineage)) p(class = "tmpl-lineage msg-hint", lineage),
    if (nzchar(description)) p(class = "msg-hint", description),
    p(class = "tmpl-summary", tmpl_detail_summary(def))
  )
}

# ---- Options dialog: one control per option --------------------------------

# Build one input control for a creation-template option.
#
# An option with `choices:` is ONE combobox rather than a dropdown padded with
# sentinel rows and a companion text field: selectize's `create` lets the author
# type a value in place, so "pick a suggestion, or type your own" needs one
# control instead of two that had to be reconciled. "(leave blank)" survives as
# a real row -- its `__blank__` sentinel is what collect_template_selections()
# already reads as an empty value.
#
# Whether typing is allowed follows the template, not the widget:
# `dta_template_allow_custom(opt, default = typ != "select")` means `type: text`
# permits it unless the template says otherwise, and `type: select` forbids it
# unless the template opts in -- the flag the old dialog parsed and then
# ignored.
#
# A default that is NOT among the choices (a vendor deviation overriding its
# base's suggestion) is added to the list, so it can be shown as selected
# instead of silently vanishing.
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

  combobox <- function(ch) {
    allow <- dta_template_allow_custom(opt, default = !identical(typ, "select"))
    def_chr <- if (is.null(def)) "" else as.character(def)[[1]]
    extra <- character(0)
    selected <- blank_val
    if (nzchar(def_chr)) {
      selected <- def_chr
      if (!(def_chr %in% unname(ch))) {
        extra <- stats::setNames(def_chr, def_chr)
      }
    }
    selectizeInput(
      iid, label,
      choices = c(stats::setNames(blank_val, "(leave blank)"), extra, ch),
      selected = selected, multiple = FALSE, width = "100%",
      options = list(
        create = allow,
        createOnBlur = allow,
        placeholder = if (allow) "Pick a suggestion or type your own" else "Pick a value"
      )
    )
  }

  ctl <- switch(typ,
    boolean = checkboxInput(iid, label, value = identical(def, TRUE) || identical(def, "yes")),
    textarea = textAreaInput(
      iid, label,
      value = as.character(def %||% ""), rows = 3, width = "100%"
    ),
    number = numericInput(
      iid, label,
      value = suppressWarnings(as.numeric(def %||% 0)), width = "100%"
    ),
    {
      ch <- dta_template_choices(opt)
      if (length(ch) > 0) {
        combobox(ch)
      } else {
        textInput(iid, label,
          value = as.character(def %||% ""),
          placeholder = "Type a value", width = "100%"
        )
      }
    }
  )

  div(
    # A textarea spans the whole grid: three lines of prose squeezed into one
    # column of a two-column grid is unreadable at any width.
    class = if (identical(typ, "textarea")) "tmpl-opt tmpl-opt-wide" else "tmpl-opt",
    ctl,
    if (nzchar(help)) div(class = "msg-hint tmpl-help", help)
  )
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
    return(p(class = "msg-hint", paste0(slot$label, ": ", record$error)))
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

  div(
    class = "tmpl-vocab",
    div(
      class = "tmpl-vocab-head",
      tags$strong(slot$label),
      div(class = "tmpl-vocab-actions", actions)
    ),
    selectizeInput(
      iid, NULL,
      choices = NULL, multiple = TRUE, width = "100%",
      options = tmpl_vocab_selectize_options(slot, items)
    ),
    if (nzchar(slot$description)) p(class = "msg-hint", slot$description),
    if (slot$min > 0) p(class = "msg-hint", paste0("Choose at least ", slot$min, ".")),
    if (identical(slot$mode, "open")) {
      p(class = "msg-hint", "Type a term and press Enter to add one that is not in the list.")
    }
  )
}

# ---- Options dialog: the whole body ----------------------------------------

# Everything between the modal's title and its footer.
#
# `vocab_specs` is passed in rather than computed here so the caller resolves
# each vocabulary once per open (template_vocab_slot_specs()); `has_open_doc`
# rather than a reachable `rv$dta` so this stays a pure function of arguments.
# Input ids are unchanged from the dialog this replaces -- tmpl_opt_<id>,
# tmpl_party_<id>, tmpl_vocab_<id>, tmpl_carry_* -- because the collectors in
# app.R read them by name.
template_options_modal_body <- function(def, loaded, index, vocab_specs, has_open_doc = FALSE) {
  value <- loaded$value
  source_name <- as.character(value$source_name %||% "")
  version <- as.character(value$version %||% def$version %||% "")
  lineage <- template_lineage_text(value$lineage, index)
  description <- as.character(def$description %||% "")

  head_ui <- tagList(
    div(
      class = "tmpl-head",
      span(class = "tmpl-id", as.character(def$id %||% "")),
      if (nzchar(version)) span(class = "tmpl-version", paste0("version ", version)),
      if (nzchar(source_name)) span(class = "badge tmpl-source", source_name)
    ),
    if (nzchar(lineage)) p(class = "tmpl-lineage msg-hint", lineage),
    if (nzchar(description)) p(class = "msg-hint", description)
  )

  opts <- def$options %||% list()
  options_ui <- tagList(
    tags$h6("Options"),
    p(class = "msg-hint", "Values land in the document's metadata and can be changed later."),
    if (length(opts) == 0) {
      p("This template has no configurable options.")
    } else {
      div(
        class = "tmpl-opts-grid",
        lapply(
          opts, render_template_option_input,
          # Resolve ${today} for the preview too, so the dialog never offers a
          # raw token as a default where the created DTA would carry a date.
          base_metadata = resolve_template_expressions(
            def$base$metadata %||% list(), dta_template_today_env()
          )
        )
      )
    }
  )

  slots <- normalise_party_slots(def$party_slots)
  profiles <- if (!is.null(index)) template_party_profiles(index) else list()
  party_ui <- if (length(slots) > 0) {
    tagList(
      tags$h6("Parties"),
      p(class = "msg-hint", "Leave a party on its template default, or pick a saved profile."),
      lapply(slots, function(slot) {
        # The template's own default is named rather than described: "(use
        # template default)" told the author nothing about what they would get
        # if they left it alone.
        base_name <- as.character(
          def$base$metadata[[slot$role]][["affiliation"]][["name"]] %||% ""
        )
        placeholder <- if (nzchar(base_name)) {
          paste0("(template default: ", base_name, ")")
        } else {
          "(template default)"
        }
        choices <- stats::setNames("", placeholder)
        eligible <- party_profiles_for_slot(profiles, slot)
        if (length(eligible) > 0) {
          choices <- c(choices, stats::setNames(
            vapply(eligible, function(p) as.character(p$id), character(1)),
            vapply(eligible, function(p) as.character(p$label %||% p$id), character(1))
          ))
        }
        label <- if (isTRUE(slot$required)) paste0(slot$label, " *") else slot$label
        # A plain <select>, not selectize: selectize drops an option whose
        # value is "" (its allowEmptyOption is off), and that is exactly the
        # template-default row -- the control would open naming nothing, with
        # no way back to the default once a profile had been picked.
        selectInput(paste0("tmpl_party_", slot$id), label,
          choices = choices, selected = "", selectize = FALSE
        )
      })
    )
  }

  vocab_ui <- if (!is.null(vocab_specs$error)) {
    tagList(
      tags$h6("Controlled vocabularies"),
      p(
        class = "msg-hint",
        paste("This template's vocabulary slots could not be read:", vocab_specs$error)
      )
    )
  } else if (length(vocab_specs$slots) > 0) {
    tagList(
      tags$h6("Controlled vocabularies"),
      p(
        class = "msg-hint",
        "Pick the terms this study collects. Each column keeps exactly the terms chosen here."
      ),
      lapply(vocab_specs$slots, tmpl_vocab_slot_ui)
    )
  }

  # "From the open document" is offered ONLY when a document is actually open --
  # there is nothing to carry over from otherwise -- and is the default in that
  # case, since a user who already has a document open and is creating a
  # related one most often wants its relationship metadata to follow.
  carry_choices <- stats::setNames("none", "Don't carry anything over")
  if (isTRUE(has_open_doc)) {
    carry_choices <- c(carry_choices, stats::setNames("open", "From the open document"))
  }
  carry_choices <- c(carry_choices, stats::setNames("file", "From a file"))
  carry_default <- if (isTRUE(has_open_doc)) "open" else "none"

  # The <summary> line is this section's heading; a separate one above it would
  # say the same thing twice.
  carry_ui <- tagList(
    tags$details(
      tags$summary("Carry over metadata from an existing document"),
      div(
        style = "padding:8px 0 0;",
        radioButtons("tmpl_carry_source", NULL, choices = carry_choices, selected = carry_default),
        conditionalPanel(
          condition = "input.tmpl_carry_source == 'file'",
          fileInput("tmpl_carry_file", "DTA YAML to carry metadata over from",
            accept = c(".yaml", ".yml")
          )
        ),
        checkboxGroupInput(
          "tmpl_carry_fields", "Fields to carry over",
          choices = stats::setNames(
            dta_template_metadata_fields(), dta_template_metadata_fields()
          ),
          selected = carry_over_default_fields()
        )
      )
    )
  )

  div(
    class = "tmpl-options-body",
    head_ui,
    options_ui,
    party_ui,
    vocab_ui,
    carry_ui
  )
}
