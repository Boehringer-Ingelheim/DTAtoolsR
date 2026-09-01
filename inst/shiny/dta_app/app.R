# =============================================================================
# DTAtools Shiny app
# Modern UI for the DTAtools R package.
# Launched via DTAtools::run_dta_app(). Helper code lives in ./R (auto-sourced).
# =============================================================================

library(shiny)
library(bslib)
library(shinyjs)

# Allow large uploads (clinical data files can be big).
options(shiny.maxRequestSize = 1024 * 1024^2) # 1 GB

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
brandbar <- div(
  class = "app-brandbar",
  tags$img(class = "brand-logo", src = "dtatools_logo_small.png", alt = "DTAtools logo"),
  div(
    div(class = "brand-title", "DTAtools"),
    div(class = "brand-sub", "Data Transfer Agreements (DTA) / Data Transmission Specifications (DTS) \u2014 validation & authoring")
  ),
  div(
    class = "app-actions",
    uiOutput("edit_gate", inline = TRUE),
    tags$a(
      class = "brand-link",
      href = "https://github.com/Boehringer-Ingelheim/DTAtoolsR/issues",
      target = "_blank", rel = "noopener noreferrer",
      "Report issues"
    ),
    tags$a(
      class = "brand-link",
      href = "https://github.com/Boehringer-Ingelheim/DTAtoolsR#credits",
      target = "_blank", rel = "noopener noreferrer",
      "About"
    ) # ,
    # tags$a(
    #  class = "brand-link",
    #  href = "https://github.com/Boehringer-Ingelheim/DTAtoolsR/blob/master/doc/DTAtools.html",
    #  target = "_blank", rel = "noopener noreferrer",
    #  "Documentation"
    # )
  )
)

# Non-floating footer: DTAtools version + author + link to the GitHub repo.
# The app bundle and installed package can differ on Posit Connect, so report
# both when they do.
dta_bundle_version <- function() {
  app_version_file <- file.path(getwd(), "VERSION")
  if (file.exists(app_version_file)) {
    vv <- tryCatch(trimws(readLines(app_version_file, n = 1, warn = FALSE, encoding = "UTF-8")),
      error = function(e) ""
    )
    if (length(vv) > 0 && nzchar(vv[[1]])) {
      return(vv[[1]])
    }
  }

  roots <- unique(normalizePath(c(
    getwd(),
    file.path(getwd(), ".."),
    file.path(getwd(), "..", ".."),
    file.path(getwd(), "..", "..", ".."),
    file.path(getwd(), "..", "..", "..", "..")
  ), winslash = "/", mustWork = FALSE))

  for (root in roots) {
    desc <- file.path(root, "DESCRIPTION")
    if (!file.exists(desc)) {
      next
    }
    lines <- tryCatch(readLines(desc, warn = FALSE, encoding = "UTF-8"),
      error = function(e) character(0)
    )
    hit <- grep("^Version:\\s*", lines, value = TRUE)
    if (length(hit) > 0) {
      vv <- trimws(sub("^Version:\\s*", "", hit[[1]]))
      if (nzchar(vv)) {
        return(vv)
      }
    }
  }

  ""
}

dta_runtime_package_version <- function() {
  v <- tryCatch(as.character(utils::packageVersion("DTAtools")),
    error = function(e) ""
  )
  if (nzchar(v)) {
    return(v)
  }

  ""
}

dta_version_label <- function() {
  bundle_version <- dta_bundle_version()
  runtime_version <- dta_runtime_package_version()

  if (nzchar(bundle_version) && nzchar(runtime_version) && bundle_version != runtime_version) {
    return(paste0("app v", bundle_version, " (pkg v", runtime_version, ")"))
  }

  if (nzchar(bundle_version)) {
    return(paste0("v", bundle_version))
  }

  if (nzchar(runtime_version)) {
    return(paste0("v", runtime_version))
  }

  ""
}

dta_pkg_version <- dta_version_label()
app_footer <- tags$footer(
  class = "app-footer",
  tags$span(class = "foot-name", "DTAtools"),
  if (nzchar(dta_pkg_version)) tags$span(class = "foot-ver", dta_pkg_version),
  tags$span(class = "foot-sep", "\u2022"),
  tags$span("Boehringer Ingelheim"),
  tags$span(class = "foot-sep", "\u2022"),
  tags$a(
    href = "https://github.com/Boehringer-Ingelheim/DTAtoolsR",
    target = "_blank", rel = "noopener noreferrer", "GitHub repository"
  )
)

# Clear a fileInput's text field after an upload is processed (so the control
# reads empty and is ready for the next drop). Triggered from the server via
# session$sendCustomMessage("dta_reset_fileinput", <input id>).
reset_fileinput_js <- "
Shiny.addCustomMessageHandler('dta_reset_fileinput', function(id) {
  var el = document.getElementById(id);
  if (!el) return;
  try { el.value = ''; } catch (e) {}
  var box = el.closest('.shiny-input-container') || el.closest('.form-group');
  if (box) {
    var txt = box.querySelector('input[type=text].form-control');
    if (txt) { txt.value = ''; }
    var bar = box.querySelector('.progress-bar');
    if (bar) { bar.style.width = '0%'; bar.textContent = ''; }
  }
});
"

# Floating validation-messages dock: fold/unfold on header click, plus a custom
# handler the server calls to open (reveal) it after a validation run. Opening
# fires a window resize so the DataTable inside (rendered while hidden) re-fits.
msgs_dock_js <- "
function DTA_toggleMsgsDock(e){
  if (e) { e.stopPropagation(); }
  var d = document.getElementById('dta-msgs-dock');
  if (!d) return;
  d.classList.toggle('collapsed');
  if (!d.classList.contains('collapsed')) { window.dispatchEvent(new Event('resize')); }
}
Shiny.addCustomMessageHandler('dta_msgs_dock', function(action){
  var d = document.getElementById('dta-msgs-dock');
  if (!d) return;
  if (action === 'open') { d.classList.remove('collapsed'); window.dispatchEvent(new Event('resize')); }
  else if (action === 'close') { d.classList.add('collapsed'); }
  else { d.classList.toggle('collapsed'); }
});
"

# Programmatic trigger for the hidden export download button. shinyjs::click()
# dispatches a jQuery-style event that does NOT invoke an anchor's native
# download navigation, so a Shiny downloadButton never actually downloads from
# it. Calling the element's NATIVE .click() (which works even on a display:none
# link) does start the browser download. The server fires this via
# session$sendCustomMessage("dta_trigger_download", <download output id>).
download_trigger_js <- "
Shiny.addCustomMessageHandler('dta_trigger_download', function(id) {
  var el = document.getElementById(id);
  if (!el) return;
  setTimeout(function(){ el.click(); }, 50);
});
"

# Per-browser secret backing 'Restore previous session'. The autosaved session
# must outlive the Shiny session (the whole point is recovering after a reload
# or a crash), so it cannot be keyed to session$token, which is regenerated on
# every page load. It is keyed instead to a 128-bit random id held in the
# browser's localStorage: stable across reloads for one browser profile,
# unguessable by, and never shared with, any other visitor of the same app.
# If localStorage is unavailable (private mode) a fresh id is minted per load,
# so the feature degrades to "no session found" rather than leaking one.
client_id_js <- "
(function(){
  function dtaClientId(){
    var KEY = 'dtatools_client_id';
    var id = null;
    try { id = window.localStorage.getItem(KEY); } catch (e) { id = null; }
    if (!id || !/^[a-f0-9]{32}$/.test(id)) {
      var buf = new Uint8Array(16);
      if (window.crypto && window.crypto.getRandomValues) {
        window.crypto.getRandomValues(buf);
      } else {
        for (var i = 0; i < buf.length; i++) { buf[i] = Math.floor(Math.random() * 256); }
      }
      id = Array.prototype.map.call(buf, function(b){
        return ('0' + b.toString(16)).slice(-2);
      }).join('');
      try { window.localStorage.setItem(KEY, id); } catch (e) {}
    }
    return id;
  }
  $(document).on('shiny:connected', function(){
    Shiny.setInputValue('dta_client_id', dtaClientId());
  });
})();
"

# The Raw YAML tab's editor (.yaml-ace-wrap, theme.R) is user-resizable via
# CSS `resize: vertical`, but Ace never notices its own container changing
# size -- it only recalculates gutter width, visible-row count and scrollbar
# geometry when explicitly told to, via the editor's resize() method.
# theme.R's `.yaml-ace-wrap .ace_editor { height: 100% !important; }` makes
# the editor element's BOX track the wrapper as it is dragged (needed because
# shinyAce::aceEditor(height = ...) sets a fixed inline `style="height: ..."`
# on that same element, which would otherwise pin it at its initial size
# regardless of the drag -- only `!important` in a stylesheet outranks an
# inline style). This is the other half: telling Ace's own internal layout
# that the box it is already filling has a new size.
#
# The wrapper does not exist until output$main's renderUI first draws the Raw
# YAML tab (itself gated on shinyAce being installed at all), and is replaced
# outright on every later re-render (a new load, a reset, a restored
# session) -- so this cannot just run once at page load and grab one element.
# A MutationObserver on the whole document is what lets it attach whenever
# that first happens and again every time it recurs, instead of assuming the
# wrapper exists yet or exists exactly once. `ace` (the global the vendored
# ace.js attaches) may legitimately never appear at all -- if shinyAce is not
# installed server-side, aceEditor() is never called and neither the wrapper
# nor ace.js itself is ever added to the page -- so every step bails out
# quietly rather than erroring.
yaml_ace_resize_js <- "
(function(){
  function wireAceResize(wrap, attempt) {
    if (!wrap || wrap.dataset.dtaAceResizeWired) return;
    if (typeof ResizeObserver === 'undefined') return;
    attempt = attempt || 0;
    var el = wrap.querySelector('.ace_editor');
    // shinyAce builds the editor ASYNCHRONOUSLY, after its container node has
    // already been inserted -- so the MutationObserver below almost always
    // sees .yaml-ace-wrap before .ace_editor exists inside it. Giving up on
    // that first sighting is what leaves the drag handle resizing the box
    // while Ace goes on painting at the old height, with a blank strip at the
    // bottom. Retry briefly instead, then stop: ~2s is far longer than the
    // editor takes to appear, and bounding it means a tab that never gets an
    // editor does not leave a timer running for the life of the session.
    if (!el || typeof ace === 'undefined') {
      if (attempt < 40) {
        setTimeout(function () { wireAceResize(wrap, attempt + 1); }, 50);
      }
      return;
    }
    var editor = ace.edit(el);
    if (!editor || typeof editor.resize !== 'function') return;
    wrap.dataset.dtaAceResizeWired = 'true';
    new ResizeObserver(function () { editor.resize(); }).observe(wrap);
  }
  function scan(root) {
    if (root && root.querySelectorAll) {
      root.querySelectorAll('.yaml-ace-wrap').forEach(wireAceResize);
    }
  }
  function start() {
    scan(document);
    new MutationObserver(function (records) {
      records.forEach(function (record) {
        record.addedNodes.forEach(function (node) {
          if (node.nodeType !== 1) return;
          if (node.classList && node.classList.contains('yaml-ace-wrap')) {
            wireAceResize(node);
          }
          scan(node);
        });
      });
    }).observe(document.body, { childList: true, subtree: true });
  }
  // This script is registered in tags$head(), so it runs BEFORE <body> exists:
  // document.body is null at that point and MutationObserver.observe() throws
  // 'parameter 1 is not of type Node', which aborts the whole IIFE and leaves
  // the editor permanently unwired -- silently, since nothing else here
  // depends on it. Wait for the document to be ready before touching body.
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', start);
  } else {
    start();
  }
})();
"

ui <- bslib::page_fluid(
  theme = bi_theme(),
  shinyjs::useShinyjs(),
  tags$head(
    tags$style(bi_css()),
    tags$script(shiny::HTML(reset_fileinput_js)),
    tags$script(shiny::HTML(msgs_dock_js)),
    tags$script(shiny::HTML(download_trigger_js)),
    tags$script(shiny::HTML(client_id_js)),
    tags$script(shiny::HTML(yaml_ace_resize_js)),
    # Unlike the five above, this one is a function in R/ui_components.R
    # rather than a string here, because its behaviour is worth testing
    # separately -- see click_guard_script() there, and the test file, for why
    # double-click protection cannot live on the server at all. Its position
    # in this list is not load-bearing: it installs a capture-phase listener
    # on `document`, which runs ahead of every element's own handler whenever
    # it was registered.
    click_guard_script()
  ),
  brandbar,
  div(style = "padding: 18px;", uiOutput("main")),
  app_footer,
  uiOutput("floating_msgs"),
  # Off-screen (but still RENDERED) download button for the export modal. It is
  # triggered programmatically via download_trigger_js. A display:none element
  # cannot be reliably .click()ed to start a browser download in every browser,
  # so it is moved off-screen instead of being hidden with display:none.
  div(
    style = paste(
      "position: absolute; left: -9999px; top: -9999px;",
      "width: 1px; height: 1px; overflow: hidden;"
    ),
    downloadButton("export_trigger_download", "")
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
server <- function(input, output, session) {
  # --- reactive state (single source of truth) ---------------------------
  rv <- reactiveValues(
    dta = NULL, # the DTA S7 object
    yaml_text = NULL, # original uploaded YAML text (for raw view)
    structure = NULL, # stable per-dataset handler metadata (for slots)
    active = NULL, # currently selected dataset name
    uploads = list(), # key "dataset||handlerIdx" -> list of {file, table} records
    status = list(), # dataset name -> "pass" | "fail" | "pending" | "nodata"
    pending_upload = NULL, # deferred upload awaiting an overwrite confirmation
    example_target = NULL, # list(ds_idx, hi) the example-file modal loads into
    dataset_only = FALSE, # legacy flag; a loaded dataset YAML is now wrapped into a full DTA (never set TRUE)
    is_example = FALSE, # TRUE when the bundled example DTA is loaded (enables example-file pickers)
    md_token = 0, # bump to re-render metadata editor
    contacts_token = 0, # bump to re-render contacts list
    doc_token = 0, # bump ONLY on load/reset/restore -> re-render the main layout
    yaml_msg = NULL, # raw-YAML apply result: NULL | list(ok, error)
    editing_contact = NULL, # list(side, index) while a contact edit modal is open
    editor_dataset = NULL, # dataset name the file/column/rule editor modal targets
    file_view = "list", # file-handler editor view: "list" | "form"
    file_token = 0, # bump to re-render the file-handler editor body
    file_edit_index = NULL, # index of the handler being edited (NULL = adding new)
    file_prefill = NULL, # list() of the handler fields currently loaded in the form
    file_msg = NULL, # inline file-handler-editor result: NULL | list(ok, error)
    pending_handler_removal = NULL, # list(index, tables) awaiting a remove confirmation
    col_view = "list", # column editor view: "list" | "form" | "vocab"
    col_vocab_ref = NULL, # last vocabulary picked, so the picker reopens on it
    col_token = 0, # bump to re-render the column editor body
    col_edit_id = NULL, # id of the column being edited (NULL = adding new)
    col_prefill = NULL, # list() of the column fields loaded in the form
    rule_view = "list", # rule editor view: "list" | "form"
    rule_token = 0, # bump to re-render the rule editor body
    rule_edit_index = NULL, # index of the rule being edited (NULL = adding new)
    rule_prefill = NULL, # list() of the rule fields currently loaded in the form
    meta_token = 0, # bump to re-render the dataset-metadata editor body
    meta_prefill = NULL, # list() of the dataset metadata fields loaded in the form
    meta_msg = NULL, # inline dataset-metadata-editor result: NULL | list(ok, error)
    col_msg = NULL, # inline column-editor result: NULL | list(ok, error)
    rule_msg = NULL, # inline rule-editor result: NULL | list(ok, error)
    cond_n = 1L, # condition-builder row count (IF ...)
    then_n = 1L, # condition-builder row count (THEN ...)
    gcond_n = 1L, # grouped condition row count
    gconstr_n = 1L, # grouped constraint row count
    template_ref = NULL, # "id@version" of the creation template chosen in the picker
    template_index = NULL, # template index snapshot frozen when "Next" was clicked
    add_ds_msg = NULL, # inline add-dataset result: NULL | list(ok, error)
    add_ds_token = 0, # bump to re-render the add-dataset modal body
    create_new_msg = NULL, # inline create-new-DTA result: NULL | list(ok, error)
    create_new_token = 0, # bump to re-render the create-new-DTA modal body
    removing_dataset = NULL, # dataset name the remove-dataset confirm modal targets
    version_locked = FALSE, # TRUE while a LOADED document has not yet had a new version created
    version_baseline_yaml = NULL, # the document exactly as loaded -- the left side of every change summary
    version_entry_index = NULL, # index into metadata@version_history of the entry this session opened
    version_note = "", # the optional note typed in the new-version modal
    new_version_msg = NULL, # inline new-version-modal result: NULL | list(ok, error)
    editing = FALSE, # TRUE while the author is in edit mode -- see the WHY on editing() below
    new_document_msg = NULL # inline new-document-modal result: NULL | list(ok, error)
  )

  # The single gate for every editing surface. Off by default: rv$editing is
  # created FALSE (below) and isTRUE(FALSE) is FALSE, so the app is read-only
  # from the first frame -- before any menu row has been clicked.
  #
  # Each surface is gated TWICE: its control is not rendered, and the observer
  # behind it calls req(editing()). The render is the affordance; the observer
  # guard is what actually holds, since an input that is not on screen can still
  # be driven over the websocket. For the Metadata tab the guard is load-bearing
  # for a second reason -- see save_md() below.
  #
  # WHAT IT GATES is the SPECIFICATION: columns, rules, file handlers, dataset
  # metadata, document metadata, contacts, the raw YAML, and adding or removing
  # datasets. Every observer that writes one of those calls req(editing()).
  #
  # WHAT IT DELIBERATELY DOES NOT GATE is working with DATA against that
  # specification: loading a document, uploading and unloading files, running
  # checks, restoring a session, and Start over. Read-only is about not being
  # able to change what the document SAYS, not about being unable to use it --
  # gating uploads or checks would make the app's default mode useless, since
  # validating a transfer is the thing most users open it to do.
  #
  # Editing state is server-owned (rv$editing) rather than an input, which is
  # what removes the trap the previous switch-based design needed four
  # server-to-client reset calls to work around: an input's value survives
  # its control leaving the DOM, so a switch that had been turned on and then
  # un-rendered would leave its old TRUE value sitting behind the empty slot,
  # armed the moment the control reappeared. A reactiveValues field has no
  # such afterlife -- it does not exist independently of the server code that
  # writes it, so there is nothing left to compensate for and none of those
  # four calls survive this design.
  #
  # rv$version_locked survives, but it no longer gates editing. It now only
  # records that a LOADED document has not yet had a new version created in
  # this session, which decides emphasis in the Edit menu (e.g. whether
  # "Create new version" reads as the primary route), not whether editing is
  # possible at all. A loaded document CAN be edited without creating a new
  # version first -- "Enable edit mode" is a deliberate route to exactly
  # that, recording nothing in the version history when no version entry is
  # open.
  editing <- reactive(isTRUE(rv$editing))

  # Turning Edit mode off closes whatever editor was open and disarms it.
  #
  # The file/column/rule/dataset-metadata save handlers resolve their target
  # from rv$editor_dataset, which is set when an editor is opened and otherwise
  # cleared only by a rename or a removal. Without this, an editor opened once
  # would leave those handlers addressable for the rest of the session. They
  # each re-check editing() as well -- this is what stops a modal from sitting
  # open, apparently editable, over a document that has just gone read-only.
  observeEvent(editing(),
    {
      if (!editing()) {
        rv$editor_dataset <- NULL
        removeModal()
      }
    },
    ignoreInit = TRUE
  )

  # The brandbar slot: empty on the landing page, otherwise the Edit dropdown
  # (edit_menu()) plus, while editing, the read-only status tag next to it.
  #
  # The landing page has nothing to edit, so the slot renders nothing there.
  # rv$structure is the same landing-vs-workspace test output$main makes, read
  # the same way: under isolate(), behind a dependency on rv$doc_token, which
  # is bumped by exactly the three assignments that can change the answer
  # (apply_loaded, confirm_reset, restore_session) and by nothing that merely
  # mutates a loaded document. Depending on rv$structure itself would rebuild
  # this slot every time a dataset was added, removed or renamed -- the same
  # mid-click rebuild the isolate() below avoids.
  #
  # rv$editing and rv$version_entry_index are read WITHOUT isolate() here, on
  # purpose: the menu's toggle row (whether it reads "Enable edit mode" or
  # "Stop editing"), the wording beneath it, and the status tag next to it
  # must follow those two immediately, the moment either changes -- unlike
  # rv$structure above, which only needs to be current as of the last
  # load/reset/restore.
  output$edit_gate <- renderUI({
    rv$doc_token
    if (is.null(isolate(rv$structure))) {
      NULL
    } else {
      tagList(
        edit_menu(
          editing = isTRUE(rv$editing),
          entry_open = !is.null(rv$version_entry_index)
        ),
        if (isTRUE(rv$editing)) edit_status_tag()
      )
    }
  })

  # --- create new version --------------------------------------------------
  # Same shape as the add-dataset modal above: an inline error output the
  # modal body embeds, and a confirm handler that either rejects without
  # closing the modal (leaving what the author typed on screen) or commits
  # and closes it. See rv$add_ds_msg / output$add_ds_body for the pattern
  # this mirrors.
  observeEvent(input$create_new_version, {
    req(rv$dta)
    rv$new_version_msg <- NULL
    current <- tryCatch(S7::prop(DTAtools::metadata(rv$dta), "version"), error = function(e) NULL)
    showModal(modalDialog(
      title = "Create new version",
      new_version_modal_body(current, dta_next_version(current)),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("new_version_confirm", "Create version", class = "btn btn-primary")
      ),
      easyClose = TRUE
    ))
  })

  output$new_version_msg <- renderUI({
    m <- rv$new_version_msg
    if (is.null(m) || isTRUE(m$ok)) {
      return(NULL)
    }
    div(class = "yaml-valid err", HTML("&#x2716;"), " ", m$error)
  })

  observeEvent(input$new_version_confirm, {
    req(rv$dta)
    v <- trimws(as.character(input$new_version_value %||% ""))
    if (!nzchar(v)) {
      rv$new_version_msg <- list(ok = FALSE, error = "Enter a version.")
      return()
    }
    current <- tryCatch(S7::prop(DTAtools::metadata(rv$dta), "version"), error = function(e) NULL)
    if (!is.null(current) && identical(v, as.character(current)[1])) {
      rv$new_version_msg <- list(ok = FALSE, error = "That is already the current version.")
      return()
    }
    # The Edit menu is reachable while already editing (there is no
    # req(rv$version_locked) left to stop it), so a second version bump in
    # one session is now possible. If an entry from an earlier bump this
    # session is still open, it has to be closed here first -- otherwise it
    # would keep dta_version_placeholder() forever, and the version history
    # would end up claiming nothing happened between the two versions.
    # dta_version_finalise() is the same "diff against the baseline, write
    # the summary" step export_dta() runs at download time, pulled out so
    # both callers share one definition.
    had_open_entry <- !is.null(rv$version_entry_index)
    base <- rv$dta
    if (had_open_entry) {
      base <- dta_version_finalise(
        base, rv$version_entry_index, rv$version_baseline_yaml,
        note = rv$version_note %||% ""
      )
    }
    res <- dta_append_version_entry(base, v, Sys.Date(), dta_version_placeholder())
    if (!isTRUE(res$ok)) {
      rv$new_version_msg <- list(ok = FALSE, error = res$error)
      return()
    }
    rv$dta <- res$value
    rv$version_entry_index <- length(S7::prop(DTAtools::metadata(rv$dta), "version_history"))
    rv$version_locked <- FALSE
    rv$version_note <- trimws(as.character(input$new_version_note %||% ""))
    if (had_open_entry || is.null(rv$version_baseline_yaml)) {
      # Re-baseline for the entry just opened, in two cases.
      #
      # had_open_entry: the earlier baseline (the document as loaded, or as it
      # stood after the previous bump) is now the LEFT side of the entry
      # closed above, not of this one.
      #
      # No baseline at all: a document that was never loaded from YAML has
      # none -- one created from a template, or restarted by "Create new from
      # current". Without this the entry just opened would be summarised
      # against nothing, and dta_version_finalise()'s missing-baseline guard
      # would leave its `changes` on the placeholder permanently, so the
      # exported history would report a version as having changed nothing.
      #
      # An existing baseline is otherwise kept: on the first bump of a loaded
      # document the summary is meant to reach back to the document as
      # loaded, which is exactly what apply_loaded() put there.
      yres <- dta_to_yaml_text(rv$dta)
      if (isTRUE(yres$ok)) rv$version_baseline_yaml <- yres$value
    }
    rv$new_version_msg <- NULL
    rv$md_token <- rv$md_token + 1
    # Both of these MUST be set before sync_yaml_text(), which ends in
    # autosave(): the snapshot it writes is what restore_session() reads back
    # after a page reload. Setting them afterwards saved editing = FALSE and
    # brought the author back out of edit mode on the very next reload,
    # having just created a version in order to edit.
    rv$editing <- TRUE
    sync_yaml_text()
    removeModal()
    showNotification(sprintf("Version %s created — now editing it.", v), type = "message")
  })

  # --- enable edit mode (no version bump) ----------------------------------
  # The "on" half of edit_menu()'s toggle: it unlocks the document exactly as
  # it stands and touches NOTHING about the version record.
  #
  # It is deliberately usable whether or not a version entry is already open,
  # because those are two different things happening to the same document and
  # this observer is only responsible for one of them:
  #
  #   No entry open -- nothing is written to the document, so export_dta()
  #   short-circuits (no rv$version_entry_index, no baseline) and writes no
  #   change summary. That is the point of this route, and the menu row's own
  #   description says so.
  #
  #   An entry open (a version was created earlier this session, then "Stop
  #   editing" was chosen) -- this RESUMES that entry rather than starting
  #   anything. The change summary keeps accumulating into it, which is why
  #   the row's description says so instead, and why every version field is
  #   left untouched below.
  #
  # THE BUG THIS FIXES: this observer used to carry a
  # req(is.null(rv$version_entry_index)) guard, matched by edit_menu()
  # withholding the row on the same condition. But version_entry_index stays
  # set for the rest of the session once a version is created, while editing
  # stops the moment the author asks it to -- so creating a version and then
  # stopping left NO route back into edit mode, neither by the menu nor over
  # the websocket. The guard is gone and the row now follows rv$editing; see
  # the WHY comment on edit_menu() (ui_components.R).
  observeEvent(input$enable_edit_mode, {
    req(rv$dta)
    rv$version_locked <- FALSE
    # rv$version_baseline_yaml is deliberately LEFT ALONE. Clearing it looked
    # harmless -- with no entry open, export_dta() writes no summary on this
    # route either way, because that is gated on version_entry_index. But the
    # author can edit in place and THEN decide the change deserves a version
    # after all, and the summary for that version has to reach back to the
    # document as loaded; a baseline destroyed here could not be recovered,
    # and the new version's entry would keep its placeholder for ever.
    #
    # rv$version_entry_index and rv$version_note are left alone for the same
    # reason, and now that this route is reachable with an entry open it
    # matters rather than merely being tidy: clearing them would orphan the
    # open entry on its placeholder and discard the note typed into the
    # new-version modal. Neither was ever this observer's to write -- with no
    # entry open they are already NULL and "" (every path that clears one
    # clears the other), so dropping the two assignments changes nothing on
    # the route that used to be the only one allowed.
    rv$editing <- TRUE
    # Nothing about the document changed, so there is no sync_yaml_text() to
    # ride along on -- but the session snapshot still has to learn that edit
    # mode is on, or a reload before the author's first actual edit would
    # bring them back read-only. autosave() is the only thing that persists
    # it, and it is called at every other state change for the same reason.
    autosave()
  })

  # --- create new from current ----------------------------------------------
  # Same shape as the new-version modal above: an inline error output the
  # modal body embeds, and a confirm handler that either rejects without
  # closing the modal (leaving what the author typed on screen) or commits
  # and closes it.
  observeEvent(input$create_new_document, {
    req(rv$dta)
    rv$new_document_msg <- NULL
    md <- tryCatch(DTAtools::metadata(rv$dta), error = function(e) NULL)
    cur_v <- tryCatch(S7::prop(md, "version"), error = function(e) NULL)
    cur_t <- tryCatch(S7::prop(md, "title"), error = function(e) NULL)
    showModal(modalDialog(
      title = "Create new from current",
      new_document_modal_body(cur_t, cur_v),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("create_new_document_confirm", "Create document", class = "btn btn-primary")
      ),
      easyClose = TRUE
    ))
  })

  output$new_document_msg <- renderUI({
    m <- rv$new_document_msg
    if (is.null(m) || isTRUE(m$ok)) {
      return(NULL)
    }
    div(class = "yaml-valid err", HTML("&#x2716;"), " ", m$error)
  })

  observeEvent(input$create_new_document_confirm, {
    req(rv$dta)
    v <- trimws(as.character(input$new_document_version %||% ""))
    if (!nzchar(v)) {
      rv$new_document_msg <- list(ok = FALSE, error = "Enter a version.")
      return()
    }
    res <- dta_restart_version_history(rv$dta, v, Sys.Date())
    if (!isTRUE(res$ok)) {
      rv$new_document_msg <- list(ok = FALSE, error = res$error)
      return()
    }
    rv$dta <- res$value
    rv$version_locked <- FALSE
    # The seeded history entry dta_restart_version_history() writes is
    # deliberately left CLOSED (no rv$version_entry_index), so an export does
    # not overwrite its "Created from ..." text with a diff -- the same
    # behaviour a template-created document already has.
    rv$version_entry_index <- NULL
    # The restarted document is the baseline for whatever version comes after
    # it. Leaving this NULL would push the re-baseline forward to the moment
    # of the next bump, and everything the author changed between restarting
    # and bumping would drop out of that version's summary.
    yres <- dta_to_yaml_text(res$value)
    rv$version_baseline_yaml <- if (isTRUE(yres$ok)) yres$value else NULL
    rv$version_note <- ""
    rv$editing <- TRUE
    rv$new_document_msg <- NULL
    rv$md_token <- rv$md_token + 1
    sync_yaml_text()
    removeModal()
  })

  # --- stop editing ----------------------------------------------------------
  # Every version field is left alone, so re-entering edit mode resumes
  # whatever entry was already open rather than starting a new one. The
  # observeEvent(editing(), ...) above already closes any open modal and
  # clears rv$editor_dataset when this flips to FALSE.
  observeEvent(input$stop_editing, {
    rv$editing <- FALSE
    # Persist it for the same reason enable_edit_mode does, and more
    # sharply in this direction: without it, deliberately leaving edit mode
    # and then reloading would put the author straight back into it, because
    # the last snapshot still said editing = TRUE.
    autosave()
  })

  upload_registry <- new.env(parent = emptyenv())

  # Autosave slot for 'Restore previous session', keyed to the browser's
  # localStorage id (see client_id_js) rather than to session$token: the file
  # has to survive the Shiny session to be restorable after a reload, but must
  # still be reachable only by the browser that wrote it. The id is re-validated
  # here because an input value is client-supplied and could be anything; only a
  # 32-char lowercase hex string is accepted, which also makes it path-safe.
  client_id <- function() {
    id <- isolate(input$dta_client_id)
    if (is.null(id) || length(id) != 1L || is.na(id) || !grepl("^[a-f0-9]{32}$", id)) {
      return(NULL)
    }
    id
  }
  session_file <- function() {
    id <- client_id()
    if (is.null(id)) {
      return(NULL)
    }
    file.path(tempdir(), paste0("dtatools_app_session_", id, ".rds"))
  }

  # Stable id per bound file so its trash button keeps working across renders.
  file_id_env <- new.env(parent = emptyenv()) # "ds\u0001hi\u0001table" -> integer id
  file_id_meta <- new.env(parent = emptyenv()) # id (as chr) -> list(dataset, hi, table)
  file_rm_registry <- new.env(parent = emptyenv()) # button id -> TRUE once observed
  gcond_rm_registry <- new.env(parent = emptyenv()) # grouped-condition remove observers
  gconstr_rm_registry <- new.env(parent = emptyenv()) # grouped-constraint remove observers
  file_id_counter <- 0L
  get_file_id <- function(dsname, hi, table) {
    key <- paste(dsname, hi, table, sep = "\u0001")
    id <- file_id_env[[key]]
    if (is.null(id)) {
      file_id_counter <<- file_id_counter + 1L
      id <- file_id_counter
      file_id_env[[key]] <- id
      file_id_meta[[as.character(id)]] <- list(dataset = dsname, hi = hi, table = table)
    }
    id
  }

  # --- helpers ------------------------------------------------------------
  build_structure <- function(dta) {
    names_ds <- dta_dataset_names(dta)
    if (length(names_ds) == 0) {
      return(list())
    }
    stats::setNames(lapply(seq_along(names_ds), function(i) {
      ds <- dta_get_dataset(dta, names_ds[i])
      handlers <- lapply(dta_handlers(ds), function(h) {
        list(
          expected = handler_expected(h),
          hint     = handler_hint(h),
          count    = handler_count_label(h),
          min      = handler_min(h),
          max      = handler_max(h),
          pattern  = handler_is_pattern(h)
        )
      })
      list(
        index = i,
        name = names_ds[i],
        type = tryCatch(ds@type, error = function(e) NA_character_),
        handlers = handlers
      )
    }), names_ds)
  }

  autosave <- function() {
    target <- session_file()
    if (is.null(target)) {
      return(invisible(NULL))
    }
    try(saveRDS(
      list(
        client_id = client_id(),
        dump = dta_dump_session(isolate(rv$dta)),
        yaml_text = isolate(rv$yaml_text),
        structure = isolate(rv$structure),
        active = isolate(rv$active),
        uploads = isolate(rv$uploads),
        status = isolate(rv$status),
        dataset_only = isolate(rv$dataset_only),
        is_example = isolate(rv$is_example),
        version_locked = isolate(rv$version_locked),
        version_baseline_yaml = isolate(rv$version_baseline_yaml),
        version_entry_index = isolate(rv$version_entry_index),
        version_note = isolate(rv$version_note),
        editing = isolate(rv$editing)
      ),
      target
    ), silent = TRUE)
  }

  apply_loaded <- function(dta, yaml_text, dataset_only = FALSE, is_example = FALSE,
                           wrapped_dataset = FALSE, versioned = FALSE,
                           start_editing = FALSE) {
    names_ds <- dta_dataset_names(dta)
    # A standalone dataset wrapped into a new empty DTA: show the full DTA YAML
    # (empty metadata + the dataset) in the Raw view so the state is coherent.
    if (isTRUE(wrapped_dataset)) {
      ser <- dta_to_yaml_text(dta)
      if (isTRUE(ser$ok)) yaml_text <- ser$value
    }
    rv$dta <- dta
    rv$yaml_text <- yaml_text
    rv$structure <- build_structure(dta)
    rv$uploads <- list()
    rv$status <- stats::setNames(rep("pending", length(names_ds)), names_ds)
    rv$active <- if (length(names_ds) > 0) names_ds[1] else NULL
    rv$dataset_only <- isTRUE(dataset_only)
    rv$is_example <- isTRUE(is_example)
    rv$md_token <- rv$md_token + 1
    rv$contacts_token <- rv$contacts_token + 1
    rv$doc_token <- rv$doc_token + 1
    # `versioned` is TRUE for a document LOADED from an existing one (upload,
    # bundled example) as opposed to one just created from a template -- see
    # the WHY comment on editing() above for the read-only-until-versioned
    # rule this arms. rv$version_baseline_yaml is captured from `yaml_text`
    # AFTER the wrapped_dataset re-serialisation above, so the baseline is the
    # text the Raw tab actually shows, not whatever the caller originally
    # passed in.
    rv$version_locked <- isTRUE(versioned)
    rv$version_baseline_yaml <- if (isTRUE(versioned)) yaml_text else NULL
    rv$version_entry_index <- NULL
    rv$version_note <- ""
    rv$new_version_msg <- NULL
    # Load-bearing, not cosmetic: without this, loading a second document
    # after editing a first would carry the previous document's edit state
    # forward, momentarily unlocking (or leaving unlocked) a document that
    # just arrived and has not been versioned. A newly loaded document is
    # never mid-edit. `start_editing` chooses which way this is written; it
    # does not skip the write, so that guarantee holds either way.
    #
    # Only the landing page's "Create new" passes TRUE. An empty document has
    # no datasets AND -- because output$add_dataset_ui is gated purely on
    # editing() -- no "+ Add dataset" control until edit mode is on, so
    # arriving read-only would leave the user in a workspace with nothing to
    # do and no visible way forward. Every other caller keeps the default: a
    # document LOADED from an existing one is read-only until versioned, and a
    # template-created one still starts read-only.
    rv$editing <- isTRUE(start_editing)
    autosave()
  }

  # Build one input control for a creation-template option.
  #
  # Every non-boolean dropdown offers its suggested `choices` plus a
  # "(leave blank)" entry and a "Custom..." entry. Choosing "Custom..." reveals
  # a companion text field next to the dropdown for a free-typed value, so any
  # option can be a suggestion, blank, or custom text.
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

    # Sentinel values for the extra dropdown entries.
    blank_val <- "__blank__"
    custom_val <- "__custom__"

    # Dropdown with suggestions + "(leave blank)" + "Custom..." and a companion
    # text field revealed only when "Custom..." is selected.
    dropdown_with_custom <- function(ch) {
      def_chr <- if (is.null(def)) "" else as.character(def)[[1]]
      choices <- c(
        ch,
        stats::setNames(blank_val, "(leave blank)"),
        stats::setNames(custom_val, "Custom...")
      )
      in_choices <- nzchar(def_chr) && def_chr %in% unname(ch)
      selected <- if (in_choices) {
        def_chr
      } else if (nzchar(def_chr)) {
        custom_val
      } else {
        blank_val
      }
      prefill <- if (!in_choices && nzchar(def_chr)) def_chr else ""
      cid <- paste0(iid, "_custom")
      div(
        class = "tmpl-opt-row",
        style = "display:flex; gap:10px; align-items:flex-end; flex-wrap:wrap;",
        div(
          style = "flex:1 1 240px; min-width:200px;",
          selectInput(iid, label, choices = choices, selected = selected)
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == '%s'", iid, custom_val),
          style = "flex:1 1 240px; min-width:200px;",
          textInput(cid, "Custom value",
            value = prefill, placeholder = "Type a custom value"
          )
        )
      )
    }

    ctl <- switch(typ,
      select = dropdown_with_custom(dta_template_choices(opt)),
      boolean = {
        ch <- c("Yes" = "yes", "No" = "no")
        selectInput(iid, label,
          choices = ch,
          selected = if (identical(def, TRUE) || identical(def, "yes")) "yes" else "no"
        )
      },
      textarea = {
        textAreaInput(iid, label, value = as.character(def %||% ""), rows = 3)
      },
      number = {
        numericInput(iid, label, value = suppressWarnings(as.numeric(def %||% 0)))
      },
      # Default: free text. With suggested `choices` it becomes a dropdown with
      # blank + Custom...; otherwise a plain (already fully custom) text field.
      {
        ch <- dta_template_choices(opt)
        if (length(ch) > 0) {
          dropdown_with_custom(ch)
        } else {
          textInput(iid, label,
            value = as.character(def %||% ""),
            placeholder = "Type a value"
          )
        }
      }
    )

    if (nzchar(help)) {
      tagList(ctl, div(class = "msg-hint", style = "margin:-8px 0 8px;", help))
    } else {
      ctl
    }
  }

  # Collect option values from the currently-open template modal.
  collect_template_selections <- function(def) {
    out <- list()
    opts <- def$options %||% list()
    for (opt in opts) {
      oid <- as.character(opt$id %||% "")
      if (!nzchar(oid)) next
      iid <- paste0("tmpl_opt_", oid)
      val <- input[[iid]]
      typ <- tolower(as.character(opt$type %||% "text"))
      if (identical(typ, "boolean")) {
        val <- identical(as.character(val %||% ""), "yes")
      } else {
        vchr <- as.character(val %||% "")
        if (identical(vchr, "__custom__")) {
          # Read the free-typed value from the companion field next to the dropdown.
          val <- as.character(input[[paste0(iid, "_custom")]] %||% "")
        } else if (identical(vchr, "__blank__")) {
          val <- ""
        }
      }
      out[[oid]] <- val
    }
    out
  }

  # Collect a user's slot -> profile-id choices from the currently-open
  # options modal. Mirrors collect_template_selections() exactly: an unset
  # (or "(use template default)") control contributes NOTHING to the returned
  # list -- apply_party_selections() (party_profiles.R) already treats a
  # missing entry as "leave this slot's target untouched", which is exactly
  # what "(use template default)" is supposed to mean.
  collect_party_selections <- function(def) {
    slots <- normalise_party_slots(def$party_slots)
    out <- list()
    for (slot in slots) {
      val <- as.character(input[[paste0("tmpl_party_", slot$id)]] %||% "")
      if (length(val) > 0 && nzchar(val[[1]])) {
        out[[slot$id]] <- val[[1]]
      }
    }
    out
  }

  # The vocabulary-slot counterpart of collect_party_selections(). A slot the
  # author left empty is OMITTED rather than recorded as character(0), so
  # vocabulary_slot_values() takes its "fall back to the slot's default"
  # branch -- the same distinction party slots draw between "no choice made"
  # and "an explicit choice".
  collect_vocab_selections <- function(def) {
    slots <- tryCatch(normalise_vocabulary_slots(def$vocabulary_slots), error = function(e) list())
    out <- list()
    for (slot in slots) {
      val <- as.character(input[[paste0("tmpl_vocab_", slot$id)]] %||% character(0))
      val <- val[!is.na(val) & nzchar(val)]
      if (length(val) > 0) {
        out[[slot$id]] <- val
      }
    }
    out
  }

  # The `carried_over_from` provenance field (template_provenance(),
  # template_create.R): the ancestor document's OWN template id/version, if
  # it had one. NULL when the ancestor carries no @template of its own --
  # there is nothing to attribute this carry-over to.
  carried_over_from_record <- function(meta) {
    t <- tryCatch(S7::prop(meta, "template"), error = function(e) NULL)
    if (is.null(t) || length(t) == 0) {
      return(NULL)
    }
    list(id = as.character(t$id %||% ""), version = as.character(t$version %||% ""))
  }

  # Turn the carry-over controls (tmpl_carry_source/_file/_fields, added to
  # show_template_options_modal() below) into the `carry_over` argument
  # create_dta_from_template() expects, or a clear error for the confirm
  # handler to surface without closing the modal. "none" -- the only option
  # offered when no document is open -- is not an error: it means create the
  # document with no ancestor at all, same as a template with no carry-over
  # feature ever had.
  resolve_carry_over <- function() {
    src <- as.character(input$tmpl_carry_source %||% "none")
    fields <- as.character(input$tmpl_carry_fields %||% character(0))

    if (!identical(src, "open") && !identical(src, "file")) {
      return(list(ok = TRUE, carry_over = NULL, carried_over_from = NULL))
    }

    if (identical(src, "open")) {
      if (is.null(rv$dta)) {
        return(list(ok = FALSE, error = "No document is open to carry metadata over from."))
      }
      meta <- DTAtools::metadata(rv$dta)
    } else {
      f <- input$tmpl_carry_file
      if (is.null(f) || is.null(f$datapath) || !nzchar(f$datapath)) {
        return(list(ok = FALSE, error = "Choose a DTA YAML file to carry metadata over from."))
      }
      res <- dta_read_yaml(f$datapath)
      if (!isTRUE(res$ok)) {
        return(list(ok = FALSE, error = paste("Could not read the carry-over file:", res$error)))
      }
      meta <- DTAtools::metadata(res$value)
    }

    list(
      ok = TRUE,
      carry_over = list(metadata = meta, fields = fields),
      carried_over_from = carried_over_from_record(meta)
    )
  }

  # `index` resolves the template's party profiles (template_party_profiles(),
  # template_create.R) -- NULL only for a caller that never went through the
  # index at all, which no longer happens from this modal, but is kept
  # optional so the function degrades to "no party slots offered" rather than
  # erroring if it ever is.
  show_template_options_modal <- function(def, index) {
    opts <- def$options %||% list()
    slots <- normalise_party_slots(def$party_slots)
    profiles <- if (!is.null(index)) template_party_profiles(index) else list()

    # One selectInput per party slot: the template's own default (an empty
    # selection, which apply_party_selections() leaves untouched) plus every
    # profile eligible for that slot's role/allow-list
    # (party_profiles_for_slot(), party_profiles.R).
    party_ui <- if (length(slots) > 0) {
      tagList(
        tags$hr(),
        tags$h6("Parties"),
        lapply(slots, function(slot) {
          eligible <- party_profiles_for_slot(profiles, slot)
          ch <- stats::setNames("", "(use template default)")
          if (length(eligible) > 0) {
            ch <- c(ch, stats::setNames(
              vapply(eligible, function(p) as.character(p$id), character(1)),
              vapply(eligible, function(p) as.character(p$label %||% p$id), character(1))
            ))
          }
          selectInput(paste0("tmpl_party_", slot$id), slot$label, choices = ch, selected = "")
        })
      )
    }

    # One multi-select per vocabulary slot, seeded with the terms that slot
    # offers. `create = TRUE` in "open" mode is what makes ONE control serve
    # both modes: selectize lets the author type a term the vocabulary does
    # not have, which is exactly "pick from the vocabulary, or use your own",
    # without a second free-text box to reconcile.
    #
    # An unresolvable vocabulary is reported INLINE and leaves the slot out,
    # rather than aborting the modal: the rest of the template is still
    # perfectly usable, and a private source that is temporarily unreachable
    # must not make document creation impossible.
    vocab_slots <- tryCatch(normalise_vocabulary_slots(def$vocabulary_slots), error = function(e) e)
    vocab_ui <- if (inherits(vocab_slots, "condition")) {
      tagList(
        tags$hr(),
        tags$h6("Controlled vocabularies"),
        p(paste("This template's vocabulary slots could not be read:", conditionMessage(vocab_slots)),
          class = "msg-hint"
        )
      )
    } else if (length(vocab_slots) > 0) {
      resolve_vocab <- vocabulary_resolver(index)
      tagList(
        tags$hr(),
        tags$h6("Controlled vocabularies"),
        lapply(vocab_slots, function(slot) {
          choices <- tryCatch(vocabulary_slot_choices(slot, resolve_vocab), error = function(e) e)
          if (inherits(choices, "condition")) {
            return(p(paste0(slot$label, ": ", conditionMessage(choices)), class = "msg-hint"))
          }
          codes <- vapply(choices$terms, function(t) t$code, character(1))
          # "CODE — Label" so the picker is readable, while the VALUE stays the
          # bare code: the label is authoring metadata and must never leak into
          # a column's permitted values.
          labels <- vapply(choices$terms, function(t) {
            lb <- as.character(t$label %||% "")
            if (nzchar(lb) && !identical(lb, t$code)) paste0(t$code, " — ", lb) else t$code
          }, character(1))

          tagList(
            selectizeInput(
              paste0("tmpl_vocab_", slot$id), slot$label,
              choices = stats::setNames(codes, labels),
              selected = slot$default,
              multiple = TRUE, width = "100%",
              options = list(create = identical(slot$mode, "open"))
            ),
            if (nzchar(slot$description)) p(slot$description, class = "msg-hint"),
            if (identical(slot$mode, "open")) {
              p("You may also type a value that is not in this vocabulary.", class = "msg-hint")
            }
          )
        })
      )
    }

    # "From the open document" is offered ONLY when a document is actually
    # open -- there is nothing to carry over from otherwise -- and is the
    # default in that case, since a user who already has a document open and
    # is creating a related one most often wants its relationship metadata to
    # follow. With no document open, "Don't carry anything over" is both the
    # only sensible default and the only option besides a file upload.
    carry_choices <- stats::setNames("none", "Don't carry anything over")
    if (!is.null(rv$dta)) {
      carry_choices <- c(carry_choices, stats::setNames("open", "From the open document"))
    }
    carry_choices <- c(carry_choices, stats::setNames("file", "From a file"))
    carry_default <- if (!is.null(rv$dta)) "open" else "none"

    carry_ui <- tagList(
      tags$hr(),
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

    showModal(modalDialog(
      title = paste0("Create from template: ", as.character(def$label %||% def$id %||% "template")),
      if (nzchar(as.character(def$description %||% ""))) {
        p(as.character(def$description), class = "msg-hint")
      },
      if (length(opts) == 0) {
        p("This template has no configurable options.")
      } else {
        tagList(lapply(
          opts,
          render_template_option_input,
          # Resolve ${today} for the preview as well, so the modal never offers
          # a raw token as a default where the created DTA would carry a date.
          base_metadata = resolve_template_expressions(
            def$base$metadata %||% list(),
            dta_template_today_env()
          )
        ))
      },
      party_ui,
      vocab_ui,
      carry_ui,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("template_create_confirm", "Create DTA", class = "btn btn-primary")
      ),
      size = "l",
      easyClose = FALSE
    ))
  }

  # --- landing: load a DTA YAML ------------------------------------------
  observeEvent(input$dta_file, {
    f <- input$dta_file
    req(f)
    txt <- tryCatch(paste(readLines(f$datapath, warn = FALSE), collapse = "\n"),
      error = function(e) NULL
    )
    res <- dta_read_yaml(f$datapath)
    if (!res$ok) {
      showNotification(
        paste("Could not load DTA YAML:", res$error),
        type = "error", duration = 10
      )
      return()
    }
    apply_loaded(res$value, txt,
      dataset_only = isTRUE(res$dataset_only),
      wrapped_dataset = isTRUE(res$wrapped_dataset),
      versioned = TRUE
    )
    showNotification(
      if (isTRUE(res$wrapped_dataset)) {
        "Dataset loaded into a new DTA \u2014 add metadata to complete it."
      } else {
        "DTA loaded."
      },
      type = "message"
    )
  })

  observeEvent(input$load_example, {
    files <- dta_example_yaml_files()
    if (length(files) == 0) {
      showNotification("No example specifications found.", type = "error")
      return()
    }
    showModal(modalDialog(
      title = "Load example DTA",
      radioButtons("example_dta_choice",
        "Choose a bundled example specification:",
        choices = files, selected = files[[1]]
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("example_dta_load", "Load", class = "btn btn-primary")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$example_dta_load, {
    sel <- input$example_dta_choice
    if (is.null(sel) || length(sel) == 0 || !nzchar(sel)) {
      showNotification("Choose an example specification first.", type = "warning")
      return()
    }
    path <- dta_example_yaml_path(sel)
    if (!nzchar(path)) {
      showNotification("Example file not found.", type = "error")
      return()
    }
    txt <- tryCatch(paste(readLines(path, warn = FALSE), collapse = "\n"),
      error = function(e) NULL
    )
    res <- dta_read_yaml(path)
    if (!res$ok) {
      showNotification(paste("Could not load example:", res$error), type = "error")
      return()
    }
    removeModal()
    apply_loaded(res$value, txt,
      dataset_only = isTRUE(res$dataset_only), is_example = TRUE,
      wrapped_dataset = isTRUE(res$wrapped_dataset), versioned = TRUE
    )
    showNotification(sprintf("Example \u201c%s\u201d loaded.", sel), type = "message")
  })

  # --- landing: create a new, empty DTA -----------------------------------
  #
  # The third way in, alongside uploading a YAML and expanding a template:
  # start from nothing. The modal asks for a title and a version and nothing
  # else -- the rest of the metadata is filled in afterwards on the Metadata
  # tab, and the datasets with "+ Add dataset". Both fields are required:
  # DTAMetaData()'s validator rejects an empty string outright, and while it
  # does accept NULL, a document the user has just chosen to name should
  # carry a name.
  #
  # Both observers below refuse to run unless the app is on the landing page
  # (rv$structure NULL). The button only EXISTS there, but its input id
  # outlives the landing DOM, so a delayed or duplicated websocket message
  # could otherwise silently replace a document the user has already loaded
  # and edited. The document a user has been working on is the one thing in
  # this app that cannot be recovered from disk, so the cheap guard is worth
  # it even though the message is unlikely.

  observeEvent(input$create_new, {
    req(is.null(rv$structure))
    rv$create_new_msg <- NULL
    rv$create_new_token <- rv$create_new_token + 1
    showModal(modalDialog(
      title = "Create a new DTA",
      uiOutput("create_new_body"),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("create_new_confirm", "Create DTA", class = "btn btn-primary")
      )
    ))
  })

  output$create_new_body <- renderUI({
    rv$create_new_token
    tagList(
      textInput("create_new_title", "Title",
        value = "", width = "100%",
        placeholder = "e.g. Clinical Data Transfer"
      ),
      textInput("create_new_version", "Version", value = "1.0", width = "100%"),
      div(
        class = "msg-hint",
        HTML("The new DTA starts with <b>no datasets</b>. Add them with <b>+ Add dataset</b> once the workspace opens, and fill in the rest of the metadata on the Metadata tab.")
      ),
      uiOutput("create_new_msg")
    )
  })

  output$create_new_msg <- renderUI({
    m <- rv$create_new_msg
    if (is.null(m) || isTRUE(m$ok)) {
      return(NULL)
    }
    div(class = "yaml-valid err", HTML("&#x2716;"), " ", m$error)
  })

  observeEvent(input$create_new_confirm, {
    req(is.null(rv$structure))
    title <- trimws(as.character(input$create_new_title %||% "")[1])
    version <- trimws(as.character(input$create_new_version %||% "")[1])
    # Every failure below leaves the modal open with what the user typed still
    # in it and changes nothing in rv. Deliberately NOT bumping
    # rv$create_new_token: that is create_new_body's only dependency, and
    # re-rendering would blank both fields, making the user retype a title
    # they can see is wrong rather than correct it in place.
    # output$create_new_msg reacts to rv$create_new_msg on its own, so the
    # error still appears. Same contract as add_ds_save below.
    if (!nzchar(title)) {
      rv$create_new_msg <- list(ok = FALSE, error = "Enter a title.")
      return()
    }
    if (!nzchar(version)) {
      rv$create_new_msg <- list(ok = FALSE, error = "Enter a version.")
      return()
    }
    created <- dta_create_empty(title, version)
    if (!isTRUE(created$ok)) {
      rv$create_new_msg <- list(ok = FALSE, error = created$error)
      return()
    }
    yres <- dta_to_yaml_text(created$value)
    yaml_text <- if (isTRUE(yres$ok)) yres$value else ""
    rv$create_new_msg <- NULL
    removeModal()
    # versioned = FALSE: this document is NEW, not loaded from an existing one,
    # so it is not gated behind the "Create new version" flow. start_editing =
    # TRUE: an empty document is unusable read-only -- see apply_loaded().
    apply_loaded(created$value, yaml_text,
      dataset_only = FALSE, is_example = FALSE, wrapped_dataset = FALSE,
      versioned = FALSE, start_editing = TRUE
    )
    showNotification(
      sprintf("New DTA \u201c%s\u201d created.", title),
      type = "message"
    )
  })


  # --- landing: create new DTA from a template ----------------------------
  #
  # Two-step modal, both steps backed by dta_template_index_cached() (template_
  # index.R) rather than the legacy list_dta_creation_templates()/get_dta_
  # creation_template_path() pair -- those still exist (template_core.R) and
  # are still used internally (read_dta_creation_template(), load_template_
  # definition()), but no longer drive the UI directly. That is what makes a
  # configured private source (template_sources.R) visible here the same way
  # the packaged demo always was, with no separate code path for either:
  #
  #   1. show_template_picker_modal()/output$template_picker_ui -- choose a
  #      template (grouped by source) and a version.
  #   2. show_template_options_modal() -- configure options, party slots and
  #      metadata carry-over for the chosen template@version.

  # One row per distinct, non-abstract creation-template id: the picker's
  # first step shows one entry per id, keeping whichever row ranks highest by
  # version (template_version_rank(), template_index.R) for its label/
  # description/source -- the SPECIFIC version is a separate control
  # (template_id_versions(), below). Ordered by source then label, both with
  # method = "radix": this machine collates under German locale, CI under C
  # collation, and a user-facing list must not silently depend on which one
  # built it (the project's pinned "locale collation diverges from CI"
  # lesson).
  template_picker_entries <- function(index) {
    rows <- index[index$kind == "dta_creation_template" & !index$abstract, , drop = FALSE]
    if (nrow(rows) == 0) {
      return(rows)
    }
    ids <- unique(rows$id)
    picked <- lapply(ids, function(id) {
      sub <- rows[rows$id == id, , drop = FALSE]
      ranks <- template_version_rank(sub$version)
      top <- order(ranks, decreasing = TRUE, na.last = TRUE)[[1]]
      sub[top, , drop = FALSE]
    })
    out <- do.call(rbind, picked)
    out <- out[order(out$label, method = "radix"), , drop = FALSE]
    out <- out[order(out$source_name, method = "radix"), , drop = FALSE]
    rownames(out) <- NULL
    out
  }

  # selectInput() choices grouped into one <optgroup> per source_name -- a
  # named list of named vectors is shiny's own recipe for optgroups (see
  # ?shiny::selectInput). `entries` is already ordered by source then label
  # (template_picker_entries()), so neither the groups nor the rows within
  # them need re-sorting here.
  template_picker_grouped_choices <- function(entries) {
    groups <- unique(entries$source_name)
    out <- list()
    for (g in groups) {
      sub <- entries[entries$source_name == g, , drop = FALSE]
      out[[g]] <- stats::setNames(sub$id, sub$label)
    }
    out
  }

  # The descriptive reading list shown ABOVE the dropdown: one heading per
  # source, one row per template naming its label and (when it has one) its
  # description. The dropdown alone only ever shows a label; a template
  # author's description would otherwise never be seen before creating a
  # document from it.
  template_picker_listing_ui <- function(entries) {
    groups <- unique(entries$source_name)
    tagList(lapply(groups, function(g) {
      sub <- entries[entries$source_name == g, , drop = FALSE]
      tagList(
        tags$h6(g),
        lapply(seq_len(nrow(sub)), function(i) {
          div(
            class = "tmpl-entry",
            tags$strong(sub$label[[i]]),
            if (nzchar(sub$description[[i]])) {
              p(class = "msg-hint", style = "margin:0 0 6px;", sub$description[[i]])
            }
          )
        })
      )
    }))
  }

  # Every version of ONE creation-template id, newest first -- the choices
  # for the "Version:" selectInput. output$template_picker_ui (below) reads
  # input$template_select_name directly and rebuilds this list every time it
  # changes, rather than pushing an update via updateSelectInput(): that keeps
  # the whole picker body ONE declarative render, and is what makes "changing
  # the template updates the version list" directly observable in the
  # rendered HTML instead of only in a client-side round trip a test harness
  # cannot see.
  template_id_versions <- function(index, id) {
    rows <- index[index$kind == "dta_creation_template" & index$id == id, , drop = FALSE]
    if (nrow(rows) == 0) {
      return(character(0))
    }
    ord <- order(template_version_rank(rows$version), decreasing = TRUE, na.last = TRUE)
    vers <- rows$version[ord]
    stats::setNames(vers, vers)
  }

  show_template_picker_modal <- function() {
    showModal(modalDialog(
      title = "Create new from template",
      p("Select a template, then configure options in the next step.", class = "msg-hint"),
      uiOutput("template_picker_ui"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("template_select_next", "Next", class = "btn btn-primary")
      ),
      size = "l",
      easyClose = TRUE
    ))
  }

  # Bumped by "Refresh templates" (below) to force output$template_picker_ui
  # to recompute even though dta_template_index_cached() alone would not be
  # seen as "changed" by the reactive graph -- it is a plain function call
  # backed by a file-local cache (template_index.R), not a reactive value.
  tmpl_refresh_tick <- reactiveVal(0)

  output$template_picker_ui <- renderUI({
    tmpl_refresh_tick()
    idx <- dta_template_index_cached()
    sources <- attr(idx, "sources") %||% list()
    entries <- template_picker_entries(idx)

    diagnostics <- template_source_diagnostics_ui(sources)
    refresh_btn <- actionButton(
      "tmpl_refresh_templates", "Refresh templates",
      class = "btn btn-sm btn-outline-secondary"
    )

    if (nrow(entries) == 0) {
      # Private-replaces-public (template_sources.R): with a private source
      # configured and nothing usable in it, the packaged demo is NOT offered
      # as a fallback -- say so plainly, backed by the diagnostics above
      # naming exactly which source(s) failed and why.
      msg <- if (dta_template_private_configured()) {
        p(
          class = "msg-hint",
          paste(
            "No templates are available: every configured private template",
            "source failed to load, and the packaged demo template is not",
            "used as a fallback while a private source is configured."
          )
        )
      } else {
        p("No creation templates found.")
      }
      return(tagList(diagnostics, msg, refresh_btn))
    }

    status <- tagList(lapply(sources, template_source_status_row))

    # The template currently highlighted for the version dropdown: the user's
    # own choice if it still names a real entry, else the first one. This is
    # what makes changing input$template_select_name rebuild the version list
    # on the very next render, with no separate observer/updateSelectInput().
    chosen_id <- as.character(input$template_select_name %||% "")
    if (!nzchar(chosen_id) || !(chosen_id %in% entries$id)) {
      chosen_id <- entries$id[[1]]
    }
    version_choices <- template_id_versions(idx, chosen_id)

    tagList(
      status,
      diagnostics,
      template_picker_listing_ui(entries),
      selectInput(
        "template_select_name", "Template:",
        choices = template_picker_grouped_choices(entries), selected = chosen_id
      ),
      selectInput(
        "template_select_version", "Version:",
        choices = version_choices,
        selected = if (length(version_choices) > 0) version_choices[[1]] else character(0)
      ),
      refresh_btn
    )
  })

  observeEvent(input$tmpl_refresh_templates, {
    dta_template_index_invalidate()
    tmpl_refresh_tick(tmpl_refresh_tick() + 1)
  })

  observeEvent(input$create_from_template, {
    idx <- dta_template_index_cached()
    entries <- template_picker_entries(idx)
    if (nrow(entries) == 0 && !dta_template_private_configured()) {
      # No private source configured AND nothing packaged/local either --
      # exactly today's pre-index behaviour: a notification, no modal.
      showNotification(
        paste(
          "No creation templates found. Add *.dta-template.yaml files to a",
          "./dta-templates folder, or point options(DTAtools.template_dir=) at",
          "a directory of your own."
        ),
        type = "warning", duration = 8
      )
      return()
    }
    show_template_picker_modal()
  })

  observeEvent(input$template_select_next, {
    idx <- dta_template_index_cached()
    tid <- as.character(input$template_select_name %||% "")
    tver <- as.character(input$template_select_version %||% "")
    if (!nzchar(tid) || !nzchar(tver)) {
      showNotification("Choose a template and a version first.", type = "warning")
      return()
    }
    ref <- paste0(tid, "@", tver)
    loaded <- load_template_definition(ref, index = idx)
    if (!isTRUE(loaded$ok)) {
      showNotification(paste("Template is invalid:", loaded$error), type = "error", duration = 10)
      return()
    }
    # Frozen HERE, not re-fetched at confirm time: the options/party/carry-
    # over step must build and create against the EXACT index the user picked
    # from, even if a background "Refresh templates" happens while that step
    # is open.
    rv$template_ref <- ref
    rv$template_index <- idx
    removeModal()
    show_template_options_modal(loaded$value$def, idx)
  })

  observeEvent(input$template_create_confirm, {
    req(rv$template_ref)
    idx <- rv$template_index

    loaded <- load_template_definition(rv$template_ref, index = idx)
    if (!isTRUE(loaded$ok)) {
      showNotification(paste("Could not load template:", loaded$error), type = "error", duration = 10)
      return() # modal stays open
    }
    def <- loaded$value$def

    sels <- collect_template_selections(def)
    party_sel <- collect_party_selections(def)
    vocab_sel <- collect_vocab_selections(def)

    co <- resolve_carry_over()
    if (!isTRUE(co$ok)) {
      showNotification(co$error, type = "error", duration = 8)
      return() # modal stays open
    }

    prov <- template_provenance(
      def, loaded$value, sels,
      lineage = loaded$value$lineage,
      carried_over_from = co$carried_over_from,
      vocab_selections = vocab_sel
    )

    created <- create_dta_from_template(
      def, loaded$value$path, sels,
      index = idx, carry_over = co$carry_over,
      party_selections = party_sel, provenance = prov,
      vocab_selections = vocab_sel
    )
    if (!isTRUE(created$ok)) {
      showNotification(paste("Could not create DTA from template:", created$error),
        type = "error", duration = 10
      )
      return() # LEAVE THE MODAL OPEN so the user does not lose their choices
    }

    yres <- dta_to_yaml_text(created$value)
    yaml_text <- if (isTRUE(yres$ok)) yres$value else ""
    removeModal()
    # versioned deliberately left at its default (FALSE): a template-created
    # document is NEW, not loaded from an existing one, so it is not gated
    # behind the "Create new version" flow.
    apply_loaded(created$value, yaml_text,
      dataset_only = FALSE, is_example = FALSE, wrapped_dataset = FALSE
    )
    # apply_loaded() above always leaves rv$editing FALSE -- correct for a
    # load, but this is not a load, it is a document just CREATED. Leaving it
    # non-editing would strand the author in a read-only view of a document
    # they made themselves seconds ago, one Edit-menu trip from being able to
    # touch it.
    rv$editing <- TRUE
    # apply_loaded() autosaved a moment ago, while rv$editing was still FALSE,
    # so the snapshot on disk contradicts the line above. Persist again or a
    # reload right after creating the document restores it read-only -- the
    # very outcome the comment above says this code exists to prevent.
    autosave()
    showNotification(
      paste0("New DTA created from template \"", as.character(def$label %||% def$id %||% ""), "\"."),
      type = "message"
    )
  })

  # --- dataset navigation (custom list: select + per-dataset check icon) --
  output$dataset_nav_ui <- renderUI({
    req(rv$structure)
    names_ds <- names(rv$structure)
    active <- rv$active
    st_map <- rv$status
    div(
      class = "dataset-nav",
      div(class = "section-label", "Datasets"),
      div(
        class = "dataset-nav-list",
        lapply(seq_along(names_ds), function(i) {
          nm <- names_ds[i]
          st <- dta_lookup(st_map, nm, "pending")
          # Row background + icon encode status: passed (green), failed (red),
          # missing/no-data (orange), not-checked-yet (neutral grey).
          st2 <- switch(st,
            pass = "pass",
            fail = "fail",
            nodata = "nodata",
            "pending"
          )
          ic_ch <- switch(st,
            pass = "\u2714",
            fail = "\u2716",
            nodata = "\u2716",
            "\u2013"
          )
          ic_cls <- switch(st,
            pass = "nav-ic-pass",
            fail = "nav-ic-fail",
            nodata = "nav-ic-nodata",
            "nav-ic-pending"
          )
          ic_ttl <- switch(st,
            pass = "Passed all checks",
            fail = "Validation failed",
            nodata = "No data loaded (missing data)",
            "Not validated yet"
          )
          row_cls <- paste0(
            "dataset-nav-row nav-st-", st2,
            if (identical(nm, active)) " active" else ""
          )
          div(
            class = row_cls,
            actionLink(
              paste0("selds_", i),
              class = "nav-select",
              label = tagList(
                span(class = paste("nav-ic", ic_cls), title = ic_ttl, ic_ch),
                span(class = "nav-name", nm)
              )
            ),
            actionButton(
              paste0("checkds_", i),
              label = HTML("&#x25B6;"),
              class = "btn btn-sm nav-check",
              title = sprintf("Check '%s'", nm)
            )
          )
        })
      )
    )
  })

  # Register one select + one check observer per dataset slot (index-stable;
  # resolves the dataset name at click time so it tracks the loaded DTA).
  nav_registry <- new.env(parent = emptyenv())
  observe({
    req(rv$structure)
    for (i in seq_along(names(rv$structure))) {
      sel_id <- paste0("selds_", i)
      if (is.null(nav_registry[[sel_id]])) {
        nav_registry[[sel_id]] <- TRUE
        local({
          IDX <- i
          observeEvent(input[[paste0("selds_", IDX)]],
            {
              nms <- names(rv$structure)
              if (IDX <= length(nms)) rv$active <- nms[IDX]
            },
            ignoreInit = TRUE
          )
          observeEvent(input[[paste0("checkds_", IDX)]],
            {
              nms <- names(rv$structure)
              if (IDX <= length(nms)) run_check(nms[IDX])
            },
            ignoreInit = TRUE
          )
        })
      }
    }
  })

  # --- add / remove a whole dataset ---------------------------------------
  # Both are gated on Edit mode twice over: the control is only rendered while
  # editing() is TRUE, and the observer behind it re-checks. See editing().

  output$add_dataset_ui <- renderUI({
    if (!editing()) {
      return(NULL)
    }
    # Quiet on purpose: "Check all datasets" (btn-primary) sits directly
    # below this in the sidebar, and that is the button that should read as
    # the call to action. btn-outline-secondary + add-dataset-btn (theme.R)
    # is what keeps this from competing with it -- Bootstrap's own
    # btn-sm/btn-outline-secondary alone still draws a full-width bordered
    # box the same size as the button under it, hence the extra CSS.
    actionButton("add_dataset_open", "+ Add dataset",
      class = "btn btn-sm btn-outline-secondary add-dataset-btn w-100",
      style = "margin-bottom: 6px;"
    )
  })

  observeEvent(input$add_dataset_open, {
    req(editing())
    rv$add_ds_msg <- NULL
    rv$add_ds_token <- rv$add_ds_token + 1
    showModal(modalDialog(
      title = "Add a dataset",
      uiOutput("add_ds_body"),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("add_ds_save", "Add dataset", class = "btn btn-primary")
      )
    ))
  })

  output$add_ds_body <- renderUI({
    rv$add_ds_token
    tagList(
      textInput("add_ds_name", "Name",
        value = "", width = "100%",
        placeholder = "e.g. demographics"
      ),
      radioButtons("add_ds_type", "Type",
        choices = c("Tabular" = "tabular", "Files" = "file"),
        selected = "tabular"
      ),
      div(
        class = "msg-hint",
        HTML("A <b>Tabular</b> dataset is validated column by column against a specification you define. A <b>Files</b> dataset only checks that the expected files arrive. The type is fixed once the dataset is created — to change it, add a new dataset and remove this one.")
      ),
      uiOutput("add_ds_msg")
    )
  })

  output$add_ds_msg <- renderUI({
    m <- rv$add_ds_msg
    if (is.null(m) || isTRUE(m$ok)) {
      return(NULL)
    }
    div(class = "yaml-valid err", HTML("&#x2716;"), " ", m$error)
  })

  observeEvent(input$add_ds_save, {
    req(editing())
    r <- dta_add_dataset(
      isolate(rv$dta),
      name = input$add_ds_name,
      type = input$add_ds_type %||% "tabular"
    )
    if (!isTRUE(r$ok)) {
      # The modal stays open with everything the user typed still in it, and
      # nothing in rv changes -- the same contract as meta_save() below.
      # Deliberately NOT bumping rv$add_ds_token here: that is add_ds_body's
      # only dependency, and re-rendering would reset the name field to "" and
      # the type back to "tabular", making the user retype a name they can see
      # is wrong rather than correct it in place. output$add_ds_msg reacts to
      # rv$add_ds_msg on its own, so the error still appears.
      rv$add_ds_msg <- list(ok = FALSE, error = r$error)
      return()
    }
    nm <- trimws(as.character(input$add_ds_name)[1])
    rv$dta <- r$value
    # build_structure() is what registers the new dataset's nav and upload
    # observers, so it has to run before anything can address the new slot.
    rv$structure <- build_structure(rv$dta)
    st <- rv$status
    st[[nm]] <- "nodata"
    rv$status <- st
    rv$active <- nm
    rv$add_ds_msg <- NULL
    sync_yaml_text()
    removeModal()
    showNotification(sprintf("Dataset '%s' added.", nm), type = "message")
  })

  observeEvent(input$remove_dataset, {
    req(editing())
    req(rv$active)
    ed <- rv$active
    # Stash the name the modal is ABOUT this dataset under rv$removing_dataset,
    # closing over it the way the contact-removal flow closes over its index
    # in local() -- so the confirm handler below acts on the dataset this
    # modal actually named, not on whatever rv$active happens to be when the
    # user clicks Remove. rv$active can change while the modal is still open
    # (easyClose = TRUE lets it be left open without cancelling), and this
    # delete is irreversible.
    rv$removing_dataset <- ed
    n_files <- length(dta_dataset_table_names(dta_get_dataset(rv$dta, ed)))
    showModal(modalDialog(
      title = "Remove this dataset?",
      div(
        # tags$b() rather than HTML(sprintf(...)): the name is user-supplied and
        # htmltools escapes a plain string, so a dataset called "<b>x" cannot
        # inject markup here.
        tags$p("Dataset ", tags$b(ed), " and its specification will be removed from the document."),
        if (n_files > 0) {
          div(class = "msg-hint", sprintf(
            "%d loaded file%s will be unloaded with it.",
            n_files, if (n_files == 1) "" else "s"
          ))
        }
      ),
      easyClose = TRUE,
      footer = tagList(
        # A plain actionButton, not modalButton(), because cancelling has to
        # clear rv$removing_dataset too -- modalButton() only closes the
        # modal client-side and never reaches the server.
        actionButton("remove_dataset_cancel", "Cancel", class = "btn btn-outline-secondary"),
        actionButton("remove_dataset_confirm", "Remove", class = "btn btn-danger")
      )
    ))
  })

  observeEvent(input$remove_dataset_cancel, {
    rv$removing_dataset <- NULL
    removeModal()
  })

  observeEvent(input$remove_dataset_confirm, {
    req(editing())
    ed <- isolate(rv$removing_dataset)
    req(ed)
    rv$removing_dataset <- NULL
    r <- dta_remove_dataset(isolate(rv$dta), ed)
    if (!isTRUE(r$ok)) {
      showNotification(paste("Could not remove dataset —", r$error),
        type = "error", duration = 10
      )
      return()
    }
    rv$dta <- r$value

    # The same state rename_dataset_state() re-keys for a rename, dropped
    # instead: uploads keyed "<dataset>||<handlerIdx>", the minted file ids, the
    # status entry, and any modal still pointing at this dataset.
    up <- rv$uploads
    keys <- names(up) %||% character(0)
    hit <- startsWith(keys, paste0(ed, "||"))
    if (any(hit)) rv$uploads <- up[!hit]

    purge_file_ids(ed)

    st <- rv$status
    if (ed %in% names(st)) rv$status <- st[setdiff(names(st), ed)]

    if (identical(rv$editor_dataset, ed)) rv$editor_dataset <- NULL

    rv$structure <- build_structure(rv$dta)
    remaining <- names(rv$structure)
    rv$active <- if (length(remaining) > 0) remaining[[1]] else NULL

    sync_yaml_text()
    removeModal()
    showNotification(sprintf("Dataset '%s' removed.", ed), type = "message")
  })

  # --- upload observers (registered once per handler) ---------------------
  handle_upload <- function(ds_idx, hi, fileinfo, overwrite = FALSE) {
    if (is.null(fileinfo)) {
      return()
    }
    names_ds <- dta_dataset_names(rv$dta)
    if (ds_idx < 1 || ds_idx > length(names_ds)) {
      return()
    }
    dsname <- names_ds[ds_idx]
    ds <- dta_get_dataset(rv$dta, dsname)
    handlers <- dta_handlers(ds)
    if (hi < 1 || hi > length(handlers)) {
      return()
    }
    h <- handlers[[hi]]
    key <- paste0(dsname, "||", hi)

    # A dropped file will occupy a table named after it (for tabular datasets,
    # load_file uses file_path_sans_ext; for file datasets, the basename with
    # extension is kept). This mapping drives overwrite detection and binds.
    ds_type <- tryCatch(ds@type, error = function(e) NA_character_)
    tbl_of <- function(nm) dta_bound_item_name(ds_type, nm)
    existing <- dta_dataset_table_names(ds) # dataset-wide bound items

    # Overwrite gate: if a dropped file targets an already-bound table and the
    # user has not confirmed, ask first -- never silently replace bound data.
    if (!isTRUE(overwrite)) {
      dropped_tbls <- vapply(as.character(fileinfo$name), tbl_of, character(1))
      conflicts <- unique(dropped_tbls[dropped_tbls %in% existing])
      if (length(conflicts) > 0) {
        rv$pending_upload <- list(ds_idx = ds_idx, hi = hi, fileinfo = fileinfo)
        showModal(modalDialog(
          title = "Overwrite existing file(s)?",
          tags$p(sprintf("These file(s) are already loaded in '%s':", dsname)),
          tags$ul(lapply(conflicts, function(t) tags$li(tags$code(t)))),
          tags$p("Overwrite them with the new upload? The affected table(s) will be marked as not validated."),
          footer = tagList(
            actionButton("cancel_overwrite", "Cancel"),
            actionButton("confirm_overwrite", "Overwrite", class = "btn btn-warning")
          ),
          easyClose = TRUE
        ))
        return()
      }
    }

    # G2 -- count gate: only NEW (non-replacing) files add to the slot's count.
    mx <- handler_max(h)
    slot_recs <- rv$uploads[[key]] %||% list()
    slot_tbls <- vapply(slot_recs, function(r) r$table %||% "", character(1))
    dropped_tbls <- vapply(as.character(fileinfo$name), tbl_of, character(1))
    kept_after <- length(setdiff(slot_tbls, dropped_tbls))
    if (!is.na(mx) && (kept_after + nrow(fileinfo)) > mx) {
      showNotification(
        sprintf(
          "This slot accepts at most %d file(s); remove one before adding more.",
          as.integer(mx)
        ),
        type = "error"
      )
      return()
    }

    loaded <- character(0)
    rejected <- character(0)
    overwritten <- character(0)
    loaded_recs <- list()
    for (r in seq_len(nrow(fileinfo))) {
      nm <- fileinfo$name[r]
      dp <- fileinfo$datapath[r]
      sz <- suppressWarnings(as.numeric(fileinfo$size[r]))
      tbl <- tbl_of(nm)
      was_existing <- tbl %in% existing

      # G1 -- transfer complete & non-empty (guards truncated/partial uploads).
      if (is.na(sz) || sz <= 0) {
        rejected <- c(rejected, sprintf("'%s' (empty or truncated)", nm))
        next
      }
      # G3 -- filename must match the handler (mirrors matches_filename()).
      if (!handler_matches(h, nm)) {
        rejected <- c(
          rejected,
          sprintf("'%s' (name does not match %s)", nm, handler_expected(h))
        )
        next
      }
      # G4 -- bind via load_file(). Shiny stores the upload under a temp name
      # like "0.csv"; matches_filename()/read_file() key off basename(file), so
      # stage the bytes under the ORIGINAL name first, else a valid file is
      # rejected inside load_file() (failure mode F7).
      staged <- dta_stage_upload(dp, nm)
      before <- dta_dataset_content_count(dta_get_dataset(rv$dta, dsname))
      res <- dta_load_file(
        rv$dta,
        dataset = dsname, file = staged, handler_index = hi, name = tbl
      )
      if (!res$ok) {
        rejected <- c(rejected, sprintf("'%s' (%s)", nm, res$error))
        next
      }
      # G5 -- VERIFY the file landed. A new file increases the count; an
      # overwrite replaces in place (count unchanged) and is still valid.
      after <- dta_dataset_content_count(dta_get_dataset(res$value, dsname))
      if (after <= before && !was_existing) {
        rejected <- c(rejected, sprintf("'%s' (could not be bound)", nm))
        next
      }
      rv$dta <- res$value
      # Overwrite -> clear the stale validation status of the replaced table.
      if (was_existing) {
        cv <- dta_clear_validation(rv$dta, dsname, tables = tbl)
        if (cv$ok) rv$dta <- cv$value
        overwritten <- c(overwritten, tbl)
      }
      loaded <- c(loaded, nm)
      loaded_recs[[length(loaded_recs) + 1L]] <- list(file = nm, table = tbl)
      existing <- unique(c(existing, tbl))
    }

    # Record only VERIFIED binds; changed data must be re-validated.
    if (length(loaded) > 0) {
      up <- rv$uploads
      cur <- up[[key]] %||% list()
      new_tbls <- vapply(loaded_recs, function(x) x$table, character(1))
      cur <- Filter(function(x) !((x$table %||% "") %in% new_tbls), cur)
      up[[key]] <- c(cur, loaded_recs)
      rv$uploads <- up
      st <- rv$status
      st[[dsname]] <- "pending"
      rv$status <- st
      autosave()
    }

    # ONE reconciled outcome (Contract C1) with honest counts (C8): never a
    # success message when nothing was actually bound.
    ow <- if (length(overwritten) > 0) {
      sprintf(" (%d overwritten)", length(overwritten))
    } else {
      ""
    }
    if (length(loaded) > 0 && length(rejected) == 0) {
      showNotification(
        sprintf(
          "Loaded %d file(s) into '%s'%s. Run Check to validate.",
          length(loaded), dsname, ow
        ),
        type = "message"
      )
    } else if (length(loaded) > 0) {
      showNotification(
        sprintf(
          "Loaded %d file(s) into '%s'%s; rejected %d: %s",
          length(loaded), dsname, ow, length(rejected),
          paste(rejected, collapse = "; ")
        ),
        type = "warning", duration = 10
      )
    } else {
      showNotification(
        sprintf(
          "No files added to '%s'. Rejected %d: %s",
          dsname, length(rejected), paste(rejected, collapse = "; ")
        ),
        type = "warning", duration = 10
      )
    }

    # Clear the fileInput's text field so the control reads empty and is ready
    # for the next drop (runs whether files were accepted or rejected).
    session$sendCustomMessage("dta_reset_fileinput", sprintf("up_%d_%d", ds_idx, hi))
  }

  # Open a modal listing the bundled example files (inst/extdata). The chosen
  # file is loaded into the slot recorded in rv$example_target (set when that
  # slot's button was clicked) and confirmed via input$example_pick_confirm.
  show_example_modal <- function() {
    files <- dta_example_data_files()
    if (length(files) == 0) {
      showNotification("No example files are bundled with this app.", type = "warning")
      return()
    }
    showModal(modalDialog(
      title = "Load a bundled example file",
      div(
        class = "msg-hint", style = "margin-bottom:10px;",
        "Pick one of the bundled example files. It is validated against the ",
        "expected file name exactly as if you had uploaded it yourself."
      ),
      radioButtons("example_pick_choice",
        label = NULL,
        choices = files, selected = character(0), width = "100%"
      ),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("example_pick_confirm", "Load file", class = "btn btn-primary")
      )
    ))
  }

  # Populate a slot from a bundled example file (inst/extdata) instead of an
  # upload. Builds the same fileinfo shape Shiny's fileInput produces and reuses
  # the exact upload pipeline (handler match, count/overwrite gates, binding).
  handle_example_pick <- function(ds_idx, hi, sel) {
    if (is.null(sel) || length(sel) == 0 || !nzchar(sel)) {
      return()
    }
    path <- dta_example_data_path(sel)
    if (!nzchar(path) || !file.exists(path)) {
      showNotification(sprintf("Example file '%s' is not available.", sel), type = "error")
      return()
    }
    fileinfo <- data.frame(
      name = basename(sel),
      size = as.numeric(file.size(path)),
      type = "",
      datapath = path,
      stringsAsFactors = FALSE
    )
    handle_upload(ds_idx, hi, fileinfo)
  }

  observeEvent(rv$structure, {
    req(rv$structure)
    for (dsname in names(rv$structure)) {
      s <- rv$structure[[dsname]]
      for (hi in seq_along(s$handlers)) {
        upid <- sprintf("up_%d_%d", s$index, hi)
        if (is.null(upload_registry[[upid]])) {
          upload_registry[[upid]] <- TRUE
          local({
            UP <- upid
            DSIDX <- s$index
            HI <- hi
            observeEvent(input[[UP]],
              {
                handle_upload(DSIDX, HI, input[[UP]])
              },
              ignoreInit = TRUE
            )
          })
        }
        exid <- sprintf("expick_%d_%d", s$index, hi)
        if (is.null(upload_registry[[exid]])) {
          upload_registry[[exid]] <- TRUE
          local({
            EX <- exid
            DSIDX <- s$index
            HI <- hi
            observeEvent(input[[EX]],
              {
                rv$example_target <- list(ds_idx = DSIDX, hi = HI)
                show_example_modal()
              },
              ignoreInit = TRUE
            )
          })
        }
      }
    }
  })

  # --- overwrite confirmation (deferred upload) --------------------------
  observeEvent(input$confirm_overwrite, {
    pu <- rv$pending_upload
    rv$pending_upload <- NULL
    removeModal()
    if (!is.null(pu)) handle_upload(pu$ds_idx, pu$hi, pu$fileinfo, overwrite = TRUE)
  })
  observeEvent(input$cancel_overwrite, {
    rv$pending_upload <- NULL
    removeModal()
  })

  # Confirm the example-file modal: load the chosen file into the target slot.
  observeEvent(input$example_pick_confirm, {
    tgt <- rv$example_target
    sel <- input$example_pick_choice
    removeModal()
    rv$example_target <- NULL
    if (is.null(tgt) || is.null(sel) || length(sel) == 0 || !nzchar(sel)) {
      return()
    }
    handle_example_pick(tgt$ds_idx, tgt$hi, sel)
  })

  # --- remove one loaded file / discard all ------------------------------
  do_remove_file <- function(dsname, hi, table) {
    req(rv$dta)
    r <- dta_unload_table(rv$dta, dsname, table)
    if (!r$ok) {
      showNotification(paste("Could not remove file:", r$error), type = "error")
      return()
    }
    rv$dta <- r$value
    key <- paste0(dsname, "||", hi)
    up <- rv$uploads
    up[[key]] <- Filter(function(x) !identical(x$table, table), up[[key]] %||% list())
    rv$uploads <- up
    cnt <- dta_dataset_content_count(dta_get_dataset(rv$dta, dsname))
    st <- rv$status
    st[[dsname]] <- if (cnt == 0) "nodata" else "pending"
    rv$status <- st
    autosave()
    showNotification(sprintf("Removed '%s' from '%s'.", table, dsname), type = "message")
  }

  # Register one remove-observer per bound file (stable id per ds|slot|table).
  observe({
    up <- rv$uploads
    req(rv$dta)
    for (key in names(up)) {
      parts <- strsplit(key, "||", fixed = TRUE)[[1]]
      if (length(parts) < 2) next
      dsname <- parts[1]
      hi <- suppressWarnings(as.integer(parts[2]))
      for (rec in (up[[key]] %||% list())) {
        fid <- get_file_id(dsname, hi, rec$table)
        bid <- paste0("rmfile_", fid)
        if (is.null(file_rm_registry[[bid]])) {
          file_rm_registry[[bid]] <- TRUE
          local({
            BID <- bid
            META <- file_id_meta[[as.character(fid)]]
            observeEvent(input[[BID]],
              {
                do_remove_file(META$dataset, META$hi, META$table)
              },
              ignoreInit = TRUE
            )
          })
        }
      }
    }
  })

  observeEvent(input$discard_all, {
    req(rv$active)
    showModal(modalDialog(
      title = "Discard all loaded files?",
      sprintf(
        "Remove all loaded files from '%s'? You will need to upload them again.",
        rv$active
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_discard_all", "Discard all", class = "btn btn-danger")
      ),
      easyClose = TRUE
    ))
  })
  observeEvent(input$confirm_discard_all, {
    req(rv$active)
    r <- dta_unload_all(rv$dta, rv$active)
    if (r$ok) {
      rv$dta <- r$value
      up <- rv$uploads
      pref <- paste0(rv$active, "||")
      for (k in names(up)) if (startsWith(k, pref)) up[[k]] <- list()
      rv$uploads <- up
      st <- rv$status
      st[[rv$active]] <- "nodata"
      rv$status <- st
      autosave()
      showNotification(sprintf("Discarded all files from '%s'.", rv$active), type = "message")
    } else {
      showNotification(r$error, type = "error")
    }
    removeModal()
  })

  # --- validation ---------------------------------------------------------
  run_check <- function(dataset = NULL) {
    req(rv$dta)
    names_ds <- dta_dataset_names(rv$dta)
    targets <- if (is.null(dataset)) names_ds else intersect(dataset, names_ds)
    if (length(targets) == 0) {
      return()
    }

    # C7 -- pre-flight: only validate datasets that actually have data bound.
    ready <- Filter(function(nm) dta_dataset_readiness(rv$dta, nm)$has_data, targets)
    nodata <- setdiff(targets, ready)

    # Mark no-data datasets explicitly; they are never reported as passed (C5).
    if (length(nodata) > 0) {
      st <- rv$status
      for (nm in nodata) st[[nm]] <- "nodata"
      rv$status <- st
    }

    if (length(ready) == 0) {
      msg <- if (length(targets) == 1) {
        sprintf(
          "'%s' has no data loaded \u2014 upload the required file(s) before validating.",
          targets[1]
        )
      } else {
        "No selected dataset has data loaded yet \u2014 upload files before validating."
      }
      showNotification(msg, type = "warning", duration = 8)
      return()
    }

    ok <- FALSE
    err <- NULL
    withProgress(message = "Validating\u2026", value = 0.4, {
      res <- dta_check(rv$dta, dataset = ready)
      setProgress(0.8)
      if (res$ok) {
        rv$dta <- res$value
        rv$status <- dta_status_map(rv$dta)
        ok <- TRUE
      } else {
        err <- res$error
      }
      setProgress(1)
    })

    if (!ok) {
      showNotification(paste("Validation error:", err), type = "error", duration = 10)
      return()
    }
    autosave()

    # ONE reconciled summary (C1/C6) derived from the verified status map.
    st <- unlist(rv$status)
    n_pass <- sum(st[ready] == "pass", na.rm = TRUE)
    n_fail <- sum(st[ready] == "fail", na.rm = TRUE)
    parts <- character(0)
    if (n_pass > 0) parts <- c(parts, sprintf("%d passed", n_pass))
    if (n_fail > 0) parts <- c(parts, sprintf("%d failed", n_fail))
    if (length(nodata) > 0) parts <- c(parts, sprintf("%d skipped (no data)", length(nodata)))
    if (length(parts) == 0) parts <- "nothing to validate"
    showNotification(
      sprintf("Validation complete \u2014 %s.", paste(parts, collapse = ", ")),
      type = if (n_fail > 0) "warning" else "message"
    )

    # Reveal the floating messages dock when the active dataset now has any
    # validation messages to show.
    active_has_msgs <- tryCatch(
      {
        m <- dta_dataset_messages(rv$dta, rv$active)
        !is.null(m) && nrow(m) > 0
      },
      error = function(e) FALSE
    )
    if (isTRUE(active_has_msgs)) {
      session$sendCustomMessage("dta_msgs_dock", "open")
    }
  }

  observeEvent(input$check_all, run_check(NULL))
  observeEvent(input$check_one, {
    req(rv$active)
    run_check(rv$active)
  })

  # After a spec edit, refresh the in-memory YAML text + editor and persist.
  sync_yaml_text <- function() {
    res <- if (isolate(rv$dataset_only)) {
      dta_dataset_to_yaml_text(isolate(rv$dta), isolate(rv$active))
    } else {
      dta_to_yaml_text(isolate(rv$dta))
    }
    if (isTRUE(res$ok)) {
      rv$yaml_text <- res$value
      set_yaml_editor(res$value)
    }
    autosave()
  }
  # A spec change invalidates only THIS dataset's validation.
  invalidate_dataset <- function(ed) {
    cv <- dta_clear_validation(rv$dta, ed)
    if (isTRUE(cv$ok)) rv$dta <- cv$value
    st <- rv$status
    st[[ed]] <- if (dta_dataset_content_count(dta_get_dataset(rv$dta, ed)) == 0) {
      "nodata"
    } else {
      "pending"
    }
    rv$status <- st
  }

  # Re-key a dataset's upload records after its file handlers were added,
  # removed or reordered.
  #
  # Uploads are keyed by handler POSITION ("<dataset>||<hi>"), and so are the
  # per-slot file inputs and the stable ids behind the per-file trash buttons.
  # Removing handler 1 of 3 therefore shifts 2 -> 1 and 3 -> 2 while the records
  # still sit under the old keys: the files vanish from "Loaded files" but stay
  # bound inside the dataset, counting towards validation and export with no way
  # to reach them. `map` (from dta_handler_index_map()) says where each old index
  # went; NA means the handler is gone and its records go with it.
  remap_uploads <- function(dsname, map) {
    up <- rv$uploads
    prefix <- paste0(dsname, "||")
    # names() of an empty list is NULL, which startsWith() rejects outright.
    keys <- names(up) %||% character(0)
    kept <- up[!startsWith(keys, prefix)]
    for (old in seq_along(map)) {
      new <- map[[old]]
      if (is.na(new)) next
      recs <- up[[paste0(prefix, old)]]
      if (is.null(recs) || length(recs) == 0) next
      kept[[paste0(prefix, new)]] <- recs
    }
    rv$uploads <- kept

    purge_file_ids(dsname)
  }

  # Forget the stable per-file ids of one dataset (or of every dataset when
  # `dsname` is NULL). Those ids encode the handler index they were minted at,
  # so after a shift a trash button would remove from the wrong key;
  # get_file_id() mints them again against the new positions on the next render.
  # Selected via file_id_meta, which records the dataset outright, rather than
  # by parsing file_id_env's composite keys.
  purge_file_ids <- function(dsname = NULL) {
    stale <- Filter(
      function(id) {
        is.null(dsname) || identical(file_id_meta[[id]]$dataset, dsname)
      },
      ls(file_id_meta)
    )
    if (length(stale) == 0) {
      return(invisible(NULL))
    }
    rm(list = stale, envir = file_id_meta)
    for (key in ls(file_id_env)) {
      if (as.character(file_id_env[[key]]) %in% stale) {
        rm(list = key, envir = file_id_env)
      }
    }
    invisible(NULL)
  }

  # Move every piece of app state a dataset owns from its old name to its new
  # one, after dta_set_dataset_meta() renamed it inside the DTA.
  #
  # Five structures are keyed by dataset NAME and each fails differently if it
  # is left behind:
  #   rv$uploads   -- keyed "<name>||<handlerIdx>". Stale keys make the loaded
  #                   files disappear from "Loaded files" while staying bound to
  #                   the dataset, still counting towards validation and export
  #                   with no way to reach them (the same failure remap_uploads()
  #                   exists to prevent).
  #   file_id_env  -- the stable ids behind the per-file trash buttons encode the
  #                   name they were minted under, so a stale id removes from the
  #                   wrong key. Purged rather than migrated; get_file_id() mints
  #                   them again on the next render.
  #   rv$status    -- the nav list reads the status by name; a stale entry leaves
  #                   the renamed dataset with no status at all.
  #   rv$active / rv$editor_dataset -- the selected dataset and the dataset the
  #                   open modal targets, both bare names.
  #
  # rv$structure is NOT migrated here: build_structure() rebuilds it wholesale
  # from the DTA, and the caller does that straight after.
  rename_dataset_state <- function(old, new) {
    if (identical(old, new)) {
      return(invisible(NULL))
    }

    up <- rv$uploads
    keys <- names(up) %||% character(0)
    old_prefix <- paste0(old, "||")
    hit <- startsWith(keys, old_prefix)
    if (any(hit)) {
      keys[hit] <- paste0(new, "||", substring(keys[hit], nchar(old_prefix) + 1L))
      names(up) <- keys
      rv$uploads <- up
    }

    purge_file_ids(old)

    st <- rv$status
    if (old %in% names(st)) {
      names(st)[match(old, names(st))] <- new
      rv$status <- st
    }

    if (identical(rv$active, old)) rv$active <- new
    if (identical(rv$editor_dataset, old)) rv$editor_dataset <- new

    invisible(NULL)
  }

  # Clear every file input of a dataset's slots. The controls are position-based,
  # so after a shift a slot would keep showing the file name dropped on whatever
  # handler used to occupy that position.
  reset_dataset_fileinputs <- function(dsname) {
    s <- rv$structure[[dsname]]
    if (is.null(s)) {
      return(invisible(NULL))
    }
    # One past the current count as well: the input of a just-removed slot is
    # still in the DOM until the re-render lands.
    for (hi in seq_len(length(s$handlers) + 1L)) {
      session$sendCustomMessage(
        "dta_reset_fileinput", sprintf("up_%d_%d", s$index, hi)
      )
    }
    invisible(NULL)
  }

  # The four things every file-handler mutation must do: clear this dataset's
  # validation, rebuild the slot structure (which is what registers the upload
  # observers for a new slot), re-render the editor, and re-serialise the YAML.
  after_handler_change <- function(ed, map = NULL) {
    if (!is.null(map)) remap_uploads(ed, map)
    invalidate_dataset(ed)
    rv$structure <- build_structure(rv$dta)
    reset_dataset_fileinputs(ed)
    rv$file_token <- rv$file_token + 1
    sync_yaml_text()
  }

  # Remove handler `idx` of `ed`, unloading the tables bound through it first.
  # Unloading before removing keeps every intermediate object valid, the same
  # ordering dta_unload_table() itself relies on.
  do_remove_handler <- function(ed, idx, tables) {
    req(editing())
    n <- length(dta_handlers(dta_get_dataset(isolate(rv$dta), ed)))
    for (tbl in tables) {
      r <- dta_unload_table(rv$dta, ed, tbl)
      if (isTRUE(r$ok)) {
        rv$dta <- r$value
      } else {
        showNotification(paste("Could not unload file:", r$error), type = "error")
        return(invisible(NULL))
      }
    }
    r <- dta_remove_handler(rv$dta, ed, idx)
    if (!isTRUE(r$ok)) {
      showNotification(r$error, type = "error")
      return(invisible(NULL))
    }
    rv$dta <- r$value
    after_handler_change(
      ed,
      map = dta_handler_index_map(n, "remove", index = idx)
    )
    showNotification(
      if (length(tables) > 0) {
        sprintf("File removed, along with %d loaded file(s).", length(tables))
      } else {
        "File removed."
      },
      type = "message"
    )
    invisible(NULL)
  }

  move_handler <- function(idx, direction) {
    req(editing())
    ed <- isolate(rv$editor_dataset)
    req(ed)
    n <- length(dta_handlers(dta_get_dataset(isolate(rv$dta), ed)))
    if (length(idx) != 1 || is.na(idx) || idx < 1 || idx > n) {
      return(invisible(NULL))
    }
    r <- dta_move_handler(isolate(rv$dta), ed, idx, direction)
    if (!isTRUE(r$ok)) {
      showNotification(r$error, type = "error")
      return(invisible(NULL))
    }
    rv$dta <- r$value
    after_handler_change(
      ed,
      map = dta_handler_index_map(n, "move", index = idx, direction = direction)
    )
    invisible(NULL)
  }

  show_file_editor_modal <- function(ed) {
    showModal(modalDialog(
      title = paste("Edit files —", ed),
      size = "xl", easyClose = FALSE,
      uiOutput("file_modal_body"),
      footer = NULL
    ))
  }

  # The remove-confirmation dialog replaces the editor modal (Shiny shows one at
  # a time), so the editor is re-opened afterwards rather than leaving the user
  # back at the dataset view mid-edit.
  reopen_file_editor <- function() {
    ed <- isolate(rv$editor_dataset)
    if (is.null(ed)) {
      return(invisible(NULL))
    }
    rv$file_view <- "list"
    rv$file_msg <- NULL
    rv$file_token <- rv$file_token + 1
    show_file_editor_modal(ed)
    invisible(NULL)
  }

  # Per-row edit (pencil) + delete (bin) buttons for the editor DT tables.
  # Clicking sets a Shiny input to the 1-based data-row index (priority=event
  # so re-clicking the same row still fires). Render the column with escape=FALSE.
  row_action_buttons <- function(edit_input, del_input, n,
                                 up_input = NULL, down_input = NULL) {
    vapply(seq_len(n), function(i) {
      up_btn <- if (is.null(up_input)) {
        ""
      } else if (i > 1L) {
        sprintf(
          "<button class=\"btn btn-sm btn-outline-secondary dta-row-btn\" title=\"Move up\" onclick=\"Shiny.setInputValue('%s', %d, {priority:'event'})\">&#x25B2;</button> ",
          up_input, i
        )
      } else {
        "<button class=\"btn btn-sm btn-outline-secondary dta-row-btn\" title=\"Move up\" disabled>&#x25B2;</button> "
      }
      down_btn <- if (is.null(down_input)) {
        ""
      } else if (i < n) {
        sprintf(
          "<button class=\"btn btn-sm btn-outline-secondary dta-row-btn\" title=\"Move down\" onclick=\"Shiny.setInputValue('%s', %d, {priority:'event'})\">&#x25BC;</button> ",
          down_input, i
        )
      } else {
        "<button class=\"btn btn-sm btn-outline-secondary dta-row-btn\" title=\"Move down\" disabled>&#x25BC;</button> "
      }
      paste0(
        up_btn, down_btn,
        sprintf(
          "<button class=\"btn btn-sm btn-outline-secondary dta-row-btn\" title=\"Edit\" onclick=\"Shiny.setInputValue('%s', %d, {priority:'event'})\">&#x270E;</button> ",
          edit_input, i
        ),
        sprintf(
          "<button class=\"btn btn-sm btn-outline-danger dta-row-btn\" title=\"Remove\" onclick=\"Shiny.setInputValue('%s', %d, {priority:'event'})\">&#x1F5D1;</button>",
          del_input, i
        )
      )
    }, character(1))
  }

  # ========================= Edit file handlers ===========================
  # Same single-modal, two-view (list <-> form) shape as the column and rule
  # editors. What is different here is the blast radius: a file handler IS an
  # upload slot, and the app keys uploads, file inputs and per-file trash
  # buttons by the handler's POSITION -- so every mutation goes through
  # after_handler_change(), which re-keys that state before anything re-renders.
  observeEvent(input$edit_files, {
    req(editing())
    req(rv$active)
    rv$editor_dataset <- rv$active
    rv$file_view <- "list"
    rv$file_edit_index <- NULL
    rv$file_prefill <- list()
    rv$file_msg <- NULL
    rv$file_token <- rv$file_token + 1
    show_file_editor_modal(rv$active)
  })

  output$file_modal_body <- renderUI({
    rv$file_token
    ed <- isolate(rv$editor_dataset)
    req(ed)
    if (identical(isolate(rv$file_view), "list")) {
      # Zero handlers is not a corner case: it is the STARTING state of every
      # dataset created via "+ Add dataset", and nothing can be uploaded into
      # it until at least one entry exists. The DT emptyTable string on
      # file_tbl below covers the table itself; this banner is what makes the
      # consequence explicit and hard to miss in the editor that can fix it.
      n_handlers <- length(dta_handlers(dta_get_dataset(isolate(rv$dta), ed)))
      tagList(
        div(
          class = "spec-toolbar",
          actionButton("file_add", HTML("&#x2795; Add file"),
            class = "btn btn-sm btn-outline-primary"
          ),
          span(
            class = "spec-hint",
            paste(
              "Each entry is one expected file (or pattern) and becomes an upload slot.",
              "Any change resets this dataset's validation; removing an entry also",
              "unloads the files that were loaded into it."
            )
          )
        ),
        DT::dataTableOutput("file_tbl"),
        if (n_handlers == 0) {
          div(
            class = "yaml-valid err", style = "margin-top:8px;",
            HTML("&#x2716;"),
            paste(
              " This dataset expects no files at all, so nothing can be loaded",
              "into it. Add at least one entry."
            )
          )
        },
        tags$hr(),
        div(style = "text-align:right;", modalButton("Close"))
      )
    } else {
      pf <- isolate(rv$file_prefill) %||% list()
      g <- function(k, d = "") pf[[k]] %||% d
      is_pattern <- isTRUE(pf$pattern)
      # A file dataset parses nothing, so `any` is the only type it may
      # declare -- and it is the only dataset for which an ending restriction
      # means anything. dta_handler_types() defaults to the tabular list, so a
      # tabular dataset's editor is unchanged.
      ds_type <- tryCatch(
        dta_get_dataset(isolate(rv$dta), ed)@type,
        error = function(e) "tabular"
      )
      type_choices <- dta_handler_types(ds_type)
      # Labelled only where the bare token would not explain itself.
      names(type_choices) <- ifelse(
        type_choices == "any", "Any file (not parsed)", type_choices
      )
      # With a single legal type there is nothing to choose, so the control is
      # shown READ-ONLY rather than removed: `input$file_type` still has to
      # exist client-side, because the endings panel below is conditional on
      # it and an absent input reads as falsy -- which would hide the endings
      # field on precisely the dataset type that has one. selectize = FALSE is
      # what makes `disabled` visible: selectize.js replaces the original
      # <select>, so the attribute alone would grey out nothing.
      type_input <- if (length(type_choices) == 1L) {
        shinyjs::disabled(
          selectInput("file_type", "File type",
            choices = type_choices, selected = g("type", type_choices[[1]]),
            width = "100%", selectize = FALSE
          )
        )
      } else {
        selectInput("file_type", "File type",
          choices = type_choices,
          selected = g("type", type_choices[[1]]), width = "100%"
        )
      }
      tagList(
        div(
          class = "spec-form",
          layout_columns(
            col_widths = c(8, 4),
            textAreaInput("file_filename", "File name or pattern",
              value = g("filename"), width = "100%", rows = 2,
              placeholder = "clinical_data.csv   |   clinical_data.*[.]csv$"
            ),
            type_input
          ),
          if (length(type_choices) == 1L) {
            div(
              class = "msg-hint", style = "margin:-8px 0 10px;",
              paste(
                "This dataset's files are checked for arrival, readability and",
                "content, never parsed, so the type cannot be changed."
              )
            )
          },
          conditionalPanel(
            condition = "input.file_type == 'any'",
            textInput("file_extensions", "Allowed file endings (optional)",
              value = g("extensions"), width = "100%",
              placeholder = "pdf, zip, xpt"
            ),
            div(
              class = "msg-hint", style = "margin:-8px 0 10px;",
              paste(
                "Separate several endings with commas. Leave blank to accept",
                "any ending. A file whose ending is not listed is refused as",
                "it is uploaded, not at validation time."
              )
            )
          ),
          checkboxInput("file_pattern",
            "Filename is a regular-expression pattern (matches several files)",
            value = is_pattern
          ),
          div(
            class = "msg-hint", style = "margin:-8px 0 10px;",
            paste(
              "Without a pattern the entry matches one exact file name and",
              "expects exactly 1 file. One name or pattern per line."
            )
          ),
          conditionalPanel(
            condition = "input.file_pattern == true",
            radioButtons("file_count_mode", "How many files may match?",
              choices = c(
                "An exact number" = "exact",
                "A range (min to max)" = "range"
              ),
              selected = g("count_mode", "exact"), inline = TRUE
            ),
            layout_columns(
              col_widths = c(4, 4, 4),
              conditionalPanel(
                condition = "input.file_count_mode == 'exact'",
                numericInput("file_number_of_files", "Number of files",
                  value = as.integer(g("number_of_files", 1L)), min = 0, step = 1,
                  width = "100%"
                )
              ),
              conditionalPanel(
                condition = "input.file_count_mode == 'range'",
                numericInput("file_min_number_of_files", "Minimum",
                  value = as.integer(g("min_number_of_files", 1L)), min = 0, step = 1,
                  width = "100%"
                )
              ),
              conditionalPanel(
                condition = "input.file_count_mode == 'range'",
                numericInput("file_max_number_of_files", "Maximum",
                  value = as.integer(g("max_number_of_files", 1L)), min = 0, step = 1,
                  width = "100%"
                )
              )
            )
          ),
          textInput("file_pattern_description", "Pattern description",
            value = g("pattern_description"), width = "100%",
            placeholder = "What the pattern means, in words"
          ),
          textAreaInput("file_info", "Info (one entry per line)",
            value = g("info"), width = "100%", rows = 2
          )
        ),
        uiOutput("file_editor_msg"),
        div(
          style = "display:flex; justify-content:space-between; margin-top:8px;",
          actionButton("file_back", HTML("&#x2190; Back to list"),
            class = "btn btn-outline-secondary"
          ),
          actionButton("file_save", "Save file", class = "btn btn-primary")
        )
      )
    }
  })

  output$file_tbl <- DT::renderDataTable({
    rv$file_token
    ed <- isolate(rv$editor_dataset)
    req(ed)
    ov <- dta_handlers_overview(isolate(rv$dta), ed)
    if (is.null(ov) || nrow(ov) == 0) {
      ov <- data.frame(
        filename = character(0), type = character(0), pattern = character(0),
        files = character(0), endings = character(0), description = character(0),
        stringsAsFactors = FALSE
      )
    }
    ov$Actions <- if (nrow(ov) > 0) {
      row_action_buttons(
        "file_edit_click", "file_del_click", nrow(ov),
        "file_up_click", "file_down_click"
      )
    } else {
      character(0)
    }
    DT::datatable(
      ov,
      rownames = FALSE, selection = "none", escape = FALSE,
      class = "display compact", width = "100%",
      options = list(
        pageLength = 8, dom = "tp", scrollX = TRUE,
        columnDefs = list(list(orderable = FALSE, targets = ncol(ov) - 1L)),
        language = list(emptyTable = "No files declared yet - add one entry for each file you expect to receive.")
      )
    )
  })

  output$file_editor_msg <- renderUI({
    m <- rv$file_msg
    if (is.null(m)) {
      return(NULL)
    }
    if (isTRUE(m$ok)) {
      div(class = "yaml-valid ok", HTML("&#x2714;"), " File saved.")
    } else {
      div(class = "yaml-valid err", HTML("&#x2716;"), " ", m$error)
    }
  })

  observeEvent(input$file_add, {
    rv$file_edit_index <- NULL
    rv$file_prefill <- list()
    rv$file_msg <- NULL
    rv$file_view <- "form"
    rv$file_token <- rv$file_token + 1
  })

  observeEvent(input$file_back, {
    rv$file_view <- "list"
    rv$file_msg <- NULL
    rv$file_token <- rv$file_token + 1
  })

  observeEvent(input$file_edit_click, {
    idx <- as.integer(input$file_edit_click)
    ed <- isolate(rv$editor_dataset)
    req(ed)
    f <- dta_handler_fields(isolate(rv$dta), ed, idx)
    if (is.null(f)) {
      return()
    }
    rv$file_edit_index <- idx
    rv$file_prefill <- f
    rv$file_msg <- NULL
    rv$file_view <- "form"
    rv$file_token <- rv$file_token + 1
  })

  # Removing a handler removes an upload slot. Anything loaded through it would
  # otherwise stay bound to the dataset with no slot to show it under, so the
  # files go too -- and the user is told which ones before it happens.
  observeEvent(input$file_del_click, {
    idx <- as.integer(input$file_del_click)
    ed <- isolate(rv$editor_dataset)
    req(ed)
    n <- length(dta_handlers(dta_get_dataset(isolate(rv$dta), ed)))
    if (length(idx) != 1 || is.na(idx) || idx < 1 || idx > n) {
      return()
    }
    recs <- isolate(rv$uploads)[[paste0(ed, "||", idx)]] %||% list()
    tbls <- vapply(recs, function(r) r$table %||% "", character(1))
    tbls <- tbls[nzchar(tbls)]
    if (length(tbls) == 0) {
      do_remove_handler(ed, idx, character(0))
      return()
    }
    rv$pending_handler_removal <- list(dataset = ed, index = idx, tables = tbls)
    showModal(modalDialog(
      title = "Remove this file and its loaded data?",
      tags$p(sprintf(
        "Removing this entry also unloads %d loaded file(s) from '%s':",
        length(tbls), ed
      )),
      tags$ul(lapply(tbls, function(t) tags$li(tags$code(t)))),
      tags$p("The specification and the loaded data are kept in step; you can load them again afterwards."),
      footer = tagList(
        actionButton("cancel_remove_handler", "Cancel"),
        actionButton("confirm_remove_handler", "Remove", class = "btn btn-danger")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$cancel_remove_handler, {
    rv$pending_handler_removal <- NULL
    removeModal()
    # The removal modal replaced the editor modal; bring the editor back.
    reopen_file_editor()
  })

  observeEvent(input$confirm_remove_handler, {
    pending <- rv$pending_handler_removal
    rv$pending_handler_removal <- NULL
    removeModal()
    if (is.null(pending)) {
      return()
    }
    do_remove_handler(pending$dataset, pending$index, pending$tables)
    reopen_file_editor()
  })

  observeEvent(input$file_up_click, {
    move_handler(as.integer(input$file_up_click), "up")
  })

  observeEvent(input$file_down_click, {
    move_handler(as.integer(input$file_down_click), "down")
  })

  observeEvent(input$file_save, {
    req(editing())
    ed <- isolate(rv$editor_dataset)
    req(ed)
    # Both text areas are one entry per line; dta_set_handler() does the split.
    pattern <- isTRUE(input$file_pattern)
    # The count controls only exist while `pattern` is ticked; without it the
    # class contract fixes the count at exactly 1.
    r <- dta_set_handler(
      isolate(rv$dta), ed,
      index = isolate(rv$file_edit_index),
      filename = input$file_filename,
      type = input$file_type,
      pattern = pattern,
      count_mode = if (pattern) (input$file_count_mode %||% "exact") else "exact",
      number_of_files = if (pattern) (input$file_number_of_files %||% 1) else 1,
      min_number_of_files = input$file_min_number_of_files %||% 1,
      max_number_of_files = input$file_max_number_of_files %||% 1,
      pattern_description = input$file_pattern_description,
      info = input$file_info,
      extensions = input$file_extensions,
      # The dataset decides which types are on offer, so it has to decide which
      # ones validate too -- otherwise the form could offer `any` and the save
      # would reject it.
      dataset_type = tryCatch(
        dta_get_dataset(isolate(rv$dta), ed)@type,
        error = function(e) "tabular"
      )
    )
    if (!isTRUE(r$ok)) {
      rv$file_msg <- list(ok = FALSE, error = r$error)
      return()
    }
    adding <- is.null(isolate(rv$file_edit_index))
    rv$dta <- r$value
    # An edit in place and an append both leave every existing handler at its
    # own index, so no upload record has to move.
    after_handler_change(ed, map = NULL)
    rv$file_msg <- NULL
    rv$file_view <- "list"
    rv$file_token <- rv$file_token + 1
    showNotification(
      if (adding) "File added." else "File updated.",
      type = "message"
    )
  })

  # ============================ Edit columns ==============================
  # A single modal with two swappable views (list <-> form) so only ONE popup
  # is ever open. rv$col_view drives which view renders; rv$col_token forces a
  # re-render. The form pre-fills from rv$col_prefill (never stale inputs).
  observeEvent(input$edit_cols, {
    req(editing())
    req(rv$active)
    # ds_edit_menu() only renders this control for a "tabular" dataset -- a
    # "file" dataset has no @specs for dta_column_ids() to read. That render
    # gate is only the affordance, though; edit_cols can still be driven over
    # the websocket on a file dataset, so re-check the type here too. Resolve
    # it the same way the ds_edit_menu(s$type) call site does: from
    # rv$structure, not by re-deriving it from rv$dta.
    req(identical(rv$structure[[rv$active]]$type, "tabular"))
    rv$editor_dataset <- rv$active
    rv$col_view <- "list"
    rv$col_edit_id <- NULL
    rv$col_prefill <- list()
    rv$col_msg <- NULL
    rv$col_token <- rv$col_token + 1
    showModal(modalDialog(
      title = paste("Edit columns \u2014", rv$active),
      size = "xl", easyClose = FALSE,
      uiOutput("col_modal_body"),
      footer = NULL
    ))
  })

  output$col_modal_body <- renderUI({
    rv$col_token
    ed <- isolate(rv$editor_dataset)
    req(ed)
    if (identical(isolate(rv$col_view), "list")) {
      tagList(
        div(
          class = "spec-toolbar",
          actionButton("col_add", HTML("&#x2795; Add column"),
            class = "btn btn-sm btn-outline-primary"
          ),
          span(
            class = "spec-hint",
            "Use the pencil to edit a column or the bin to remove it. Any change resets this dataset's validation."
          )
        ),
        DT::dataTableOutput("col_tbl"),
        tags$hr(),
        div(style = "text-align:right;", modalButton("Close"))
      )
    } else if (identical(isolate(rv$col_view), "vocab")) {
      # A THIRD view of the same modal, not a nested modal: Shiny's showModal()
      # replaces rather than stacks, so a modal opened from inside this one
      # would destroy the column form (and every unsaved edit in it) on the way
      # in and have nothing to return to on the way out. rv$col_view already
      # exists to switch this body; a third state costs nothing.
      idx <- dta_template_index_cached()
      vocabs <- list_template_index_entries(idx, kind = "dta_vocabulary")
      pf <- isolate(rv$col_prefill) %||% list()

      tagList(
        tags$h6("Choose permitted values from a controlled vocabulary"),
        if (is.null(vocabs) || nrow(vocabs) == 0) {
          p(
            "No controlled vocabularies are available from the configured template sources.",
            class = "msg-hint"
          )
        } else {
          tagList(
            selectInput("col_vocab_ref", "Vocabulary",
              choices = stats::setNames(
                paste0(vocabs$id, "@", vocabs$version),
                paste0(vocabs$label, " (", vocabs$id, "@", vocabs$version, ")")
              ),
              selected = isolate(rv$col_vocab_ref), width = "100%"
            ),
            uiOutput("col_vocab_terms"),
            p(
              paste(
                "Selected terms replace this column's allowed values.",
                "Nothing is written until you save the column."
              ),
              class = "msg-hint"
            )
          )
        },
        uiOutput("col_editor_msg"),
        div(
          style = "display:flex; justify-content:space-between; margin-top:8px;",
          actionButton("col_vocab_cancel", HTML("&#x2190; Back to column"),
            class = "btn btn-outline-secondary"
          ),
          if (!is.null(vocabs) && nrow(vocabs) > 0) {
            actionButton("col_vocab_apply", "Use these values", class = "btn btn-primary")
          }
        )
      )
    } else {
      pf <- isolate(rv$col_prefill) %||% list()
      g <- function(k, d = "") pf[[k]] %||% d
      tagList(
        div(
          class = "spec-form",
          layout_columns(
            col_widths = c(4, 8),
            textInput("col_id", "Column ID", value = g("id"), width = "100%"),
            textInput("col_label", "Label", value = g("label"), width = "100%")
          ),
          layout_columns(
            col_widths = c(3, 3, 3, 3),
            selectInput("col_backend", "Backend",
              choices = dta_supported_backends(),
              selected = g("backend", dta_supported_backends()[1]), width = "100%"
            ),
            selectInput("col_type", "Type",
              choices = dta_sas_types(),
              selected = g("type", "Char"), width = "100%"
            ),
            textInput("col_format", "Format",
              value = g("format"), width = "100%",
              placeholder = "e.g. $9. / 8.2"
            ),
            textInput("col_length", "Length",
              value = g("length"), width = "100%",
              placeholder = "e.g. 8"
            )
          ),
          checkboxInput("col_nullable", "Nullable (missing values allowed)",
            value = if (is.null(pf$nullable)) TRUE else isTRUE(pf$nullable)
          ),
          layout_columns(
            col_widths = c(6, 6),
            tagList(
              textAreaInput("col_values", "Allowed values (one per line)",
                value = g("values"), width = "100%", rows = 3
              ),
              actionButton("col_vocab_open", "Choose from vocabulary…",
                class = "btn btn-sm btn-outline-secondary"
              )
            ),
            textInput("col_pattern", "Pattern (regex)", value = g("pattern"), width = "100%")
          ),
          textAreaInput("col_desc", "Description", value = g("description"), width = "100%", rows = 2)
        ),
        uiOutput("col_editor_msg"),
        div(
          style = "display:flex; justify-content:space-between; margin-top:8px;",
          actionButton("col_back", HTML("&#x2190; Back to list"),
            class = "btn btn-outline-secondary"
          ),
          actionButton("col_save", "Save column", class = "btn btn-primary")
        )
      )
    }
  })

  output$col_tbl <- DT::renderDataTable({
    rv$col_token
    ed <- isolate(rv$editor_dataset)
    req(ed)
    ov <- dta_columns_overview(isolate(rv$dta), ed)
    if (is.null(ov) || nrow(ov) == 0) {
      ov <- data.frame(
        id = character(0), label = character(0), type = character(0),
        length = character(0), nullable = character(0),
        constraint = character(0), description = character(0),
        stringsAsFactors = FALSE
      )
    }
    ov$Actions <- if (nrow(ov) > 0) {
      row_action_buttons(
        "col_edit_click", "col_del_click", nrow(ov),
        "col_up_click", "col_down_click"
      )
    } else {
      character(0)
    }
    DT::datatable(
      ov,
      rownames = FALSE, selection = "none", escape = FALSE,
      class = "display compact", width = "100%",
      options = list(
        pageLength = 8, dom = "tp", scrollX = TRUE,
        columnDefs = list(list(orderable = FALSE, targets = ncol(ov) - 1L))
      )
    )
  })

  output$col_editor_msg <- renderUI({
    m <- rv$col_msg
    if (is.null(m)) {
      return(NULL)
    }
    if (isTRUE(m$ok)) {
      div(class = "yaml-valid ok", HTML("&#x2714;"), " Column saved.")
    } else {
      div(class = "yaml-valid err", HTML("&#x2716;"), " ", m$error)
    }
  })

  observeEvent(input$col_add, {
    rv$col_edit_id <- NULL
    rv$col_prefill <- list()
    rv$col_msg <- NULL
    rv$col_view <- "form"
    rv$col_token <- rv$col_token + 1
  })

  observeEvent(input$col_back, {
    rv$col_view <- "list"
    rv$col_msg <- NULL
    rv$col_token <- rv$col_token + 1
  })

  # --- column editor: pick permitted values from a controlled vocabulary ---
  #
  # The form's CURRENT inputs are snapshotted into rv$col_prefill on the way
  # into the picker and restored on the way back. Without this, leaving the
  # form re-renders it from a stale prefill and silently discards everything
  # typed since the form was opened.
  snapshot_col_form <- function() {
    list(
      id = input$col_id %||% "",
      label = input$col_label %||% "",
      backend = input$col_backend %||% "",
      type = input$col_type %||% "",
      format = input$col_format %||% "",
      length = input$col_length %||% "",
      nullable = isTRUE(input$col_nullable),
      values = input$col_values %||% "",
      pattern = input$col_pattern %||% "",
      description = input$col_desc %||% ""
    )
  }

  observeEvent(input$col_vocab_open, {
    req(editing())
    rv$col_prefill <- snapshot_col_form()
    rv$col_msg <- NULL
    rv$col_view <- "vocab"
    rv$col_token <- rv$col_token + 1
  })

  observeEvent(input$col_vocab_cancel, {
    rv$col_msg <- NULL
    rv$col_view <- "form"
    rv$col_token <- rv$col_token + 1
  })

  # The terms of whichever vocabulary is selected, pre-ticked with any of them
  # the column already lists -- so re-opening the picker on a column that was
  # built from this vocabulary shows the current selection rather than a blank
  # form.
  output$col_vocab_terms <- renderUI({
    ref <- input$col_vocab_ref
    req(ref)
    resolve <- vocabulary_resolver(dta_template_index_cached())
    vocab <- tryCatch(resolve(ref), error = function(e) e)
    if (inherits(vocab, "condition") || is.null(vocab)) {
      msg <- if (inherits(vocab, "condition")) conditionMessage(vocab) else "could not be resolved."
      return(p(paste("Vocabulary", ref, msg), class = "msg-hint"))
    }

    codes <- vapply(vocab$terms, function(t) as.character(t$code), character(1))
    labels <- vapply(vocab$terms, function(t) {
      lb <- as.character(t$label %||% "")
      if (nzchar(lb) && !identical(lb, t$code)) paste0(t$code, " — ", lb) else t$code
    }, character(1))

    current <- trimws(strsplit(isolate(rv$col_prefill)$values %||% "", "\n")[[1]])
    selectizeInput("col_vocab_terms_sel", "Terms",
      choices = stats::setNames(codes, labels),
      selected = intersect(codes, current[nzchar(current)]),
      multiple = TRUE, width = "100%",
      # `create = TRUE`: the column editor is the free-form surface of the app,
      # so a value the vocabulary does not have is allowed here by default --
      # "pick from the vocabulary, or use your own". A template's slot is where
      # a closed list is enforced.
      options = list(create = TRUE)
    )
  })

  observeEvent(input$col_vocab_apply, {
    sel <- as.character(input$col_vocab_terms_sel %||% character(0))
    sel <- sel[!is.na(sel) & nzchar(sel)]
    if (length(sel) == 0) {
      rv$col_msg <- list(ok = FALSE, error = "Select at least one term, or go back without applying.")
      return()
    }
    rv$col_vocab_ref <- input$col_vocab_ref
    pf <- isolate(rv$col_prefill) %||% list()
    pf$values <- paste(sel, collapse = "\n")
    # Permitted values and a pattern are mutually exclusive on DTAColumnSpec,
    # so clear the pattern here rather than let the save fail on a conflict the
    # user cannot see from this view.
    pf$pattern <- ""
    rv$col_prefill <- pf
    rv$col_msg <- NULL
    rv$col_view <- "form"
    rv$col_token <- rv$col_token + 1
  })

  observeEvent(input$col_edit_click, {
    idx <- as.integer(input$col_edit_click)
    ed <- isolate(rv$editor_dataset)
    req(ed)
    ids <- dta_column_ids(isolate(rv$dta), ed)
    if (length(idx) != 1 || is.na(idx) || idx < 1 || idx > length(ids)) {
      return()
    }
    f <- dta_column_fields(isolate(rv$dta), ed, ids[[idx]])
    if (is.null(f)) {
      return()
    }
    rv$col_edit_id <- ids[[idx]]
    rv$col_prefill <- f
    rv$col_msg <- NULL
    rv$col_view <- "form"
    rv$col_token <- rv$col_token + 1
  })

  observeEvent(input$col_del_click, {
    req(editing())
    idx <- as.integer(input$col_del_click)
    ed <- isolate(rv$editor_dataset)
    req(ed)
    ids <- dta_column_ids(isolate(rv$dta), ed)
    if (length(idx) != 1 || is.na(idx) || idx < 1 || idx > length(ids)) {
      return()
    }
    r <- dta_remove_column(isolate(rv$dta), ed, ids[[idx]])
    if (!isTRUE(r$ok)) {
      showNotification(r$error, type = "error")
      return()
    }
    rv$dta <- r$value
    invalidate_dataset(ed)
    rv$col_token <- rv$col_token + 1
    sync_yaml_text()
    showNotification(sprintf("Removed column \u201c%s\u201d.", ids[[idx]]), type = "message")
  })

  observeEvent(input$col_up_click, {
    req(editing())
    idx <- as.integer(input$col_up_click)
    ed <- isolate(rv$editor_dataset)
    req(ed)
    ids <- dta_column_ids(isolate(rv$dta), ed)
    if (length(idx) != 1 || is.na(idx) || idx <= 1 || idx > length(ids)) {
      return()
    }
    r <- dta_move_column(isolate(rv$dta), ed, ids[[idx]], "up")
    if (!isTRUE(r$ok)) {
      showNotification(r$error, type = "error")
      return()
    }
    rv$dta <- r$value
    invalidate_dataset(ed)
    rv$col_token <- rv$col_token + 1
    sync_yaml_text()
  })

  observeEvent(input$col_down_click, {
    req(editing())
    idx <- as.integer(input$col_down_click)
    ed <- isolate(rv$editor_dataset)
    req(ed)
    ids <- dta_column_ids(isolate(rv$dta), ed)
    if (length(idx) != 1 || is.na(idx) || idx < 1 || idx >= length(ids)) {
      return()
    }
    r <- dta_move_column(isolate(rv$dta), ed, ids[[idx]], "down")
    if (!isTRUE(r$ok)) {
      showNotification(r$error, type = "error")
      return()
    }
    rv$dta <- r$value
    invalidate_dataset(ed)
    rv$col_token <- rv$col_token + 1
    sync_yaml_text()
  })

  observeEvent(input$col_save, {
    req(editing())
    ed <- isolate(rv$editor_dataset)
    req(ed)
    # rv$editor_dataset is only set by opening the column editor, but that
    # observer's own guard can be raced or bypassed the same way edit_cols
    # can -- refuse here too rather than trust the stashed name.
    req(identical(rv$structure[[ed]]$type, "tabular"))
    id <- trimws(input$col_id %||% "")
    if (!nzchar(id)) {
      rv$col_msg <- list(ok = FALSE, error = "A column ID is required.")
      return()
    }
    vals <- trimws(strsplit(input$col_values %||% "", "\n")[[1]])
    vals <- vals[nzchar(vals)]
    r <- dta_set_column(
      isolate(rv$dta), ed,
      id = id, label = input$col_label,
      backend = input$col_backend, type = input$col_type,
      format = input$col_format, length = input$col_length,
      nullable = isTRUE(input$col_nullable),
      values = if (length(vals)) vals else NULL,
      pattern = input$col_pattern, description = input$col_desc,
      old_id = isolate(rv$col_edit_id)
    )
    if (!isTRUE(r$ok)) {
      rv$col_msg <- list(ok = FALSE, error = r$error)
      return()
    }
    rv$dta <- r$value
    invalidate_dataset(ed)
    rv$col_msg <- NULL
    rv$col_view <- "list"
    rv$col_token <- rv$col_token + 1
    sync_yaml_text()
    showNotification("Column saved.", type = "message")
  })

  # ============================= Edit rules ===============================
  # One editable condition row (Column / Operator / Value). The value field is
  # a single text box whose meaning depends on the operator (see the hint under
  # the builder). Values come from the prefill only (fresh renders happen on
  # token bumps where prefill is authoritative; insertUI rows keep DOM state).
  one_cond_row <- function(prefix, i, cols, pf = list(col = "", op = "equals", val = "")) {
    cid <- paste0(prefix, "_col_", i)
    oid <- paste0(prefix, "_op_", i)
    vid <- paste0(prefix, "_val_", i)
    div(
      class = "cond-row",
      selectInput(cid, if (i == 1) "Column" else NULL,
        choices = c("(select)" = "", cols),
        selected = pf$col %||% "", width = "100%"
      ),
      selectInput(oid, if (i == 1) "Operator" else NULL,
        choices = dta_condition_operators(),
        selected = pf$op %||% "equals", width = "100%"
      ),
      textInput(vid, if (i == 1) "Value" else NULL,
        value = pf$val %||% "", width = "100%",
        placeholder = "5 | a, b | 0, 99 | true"
      )
    )
  }
  build_cond_rows <- function(prefix, n, cols, prefill) {
    lapply(seq_len(max(1L, n)), function(i) {
      pf <- if (i <= length(prefill)) prefill[[i]] else list(col = "", op = "equals", val = "")
      one_cond_row(prefix, i, cols, pf)
    })
  }
  # named-list {COL:{op:val}} -> list of editable rows. "min"/"max" keys (single
  # or combined) map to the UI "min_max" operator with a "min, max" value.
  cond_to_rows <- function(cond) {
    if (is.null(cond) || length(cond) == 0) {
      return(list())
    }
    lapply(names(cond), function(col) {
      spec <- cond[[col]]
      if (is.list(spec) && length(spec) > 0) {
        keys <- names(spec)
        if ("min" %in% keys || "max" %in% keys) {
          op <- "min_max"
          mn <- if ("min" %in% keys) as.character(spec[["min"]]) else ""
          mx <- if ("max" %in% keys) as.character(spec[["max"]]) else ""
          val <- sub(",\\s*$", "", paste0(mn, ", ", mx))
        } else {
          op <- keys[1]
          raw <- spec[[1]]
          val <- if (identical(op, "empty")) {
            if (isTRUE(raw)) "true" else "false"
          } else {
            paste(as.character(unlist(raw)), collapse = ", ")
          }
        }
      } else {
        op <- "equals"
        val <- as.character(spec)
      }
      list(col = col, op = op, val = val)
    })
  }
  parse_cond_value <- function(op, txt) {
    txt <- txt %||% ""
    if (identical(op, "empty")) {
      v <- tolower(trimws(txt))
      return(!(v %in% c("false", "no", "0", "f", "n"))) # default TRUE
    }
    if (identical(op, "pattern")) {
      return(as.character(txt))
    }
    if (op %in% c("in", "not_in")) {
      parts <- trimws(strsplit(txt, ",")[[1]])
      parts <- parts[nzchar(parts)]
      if (length(parts) == 0) {
        return(character(0))
      }
      nums <- suppressWarnings(as.numeric(parts))
      if (all(!is.na(nums))) {
        return(nums)
      }
      return(parts)
    }
    if (!nzchar(trimws(txt))) {
      return("")
    }
    num <- suppressWarnings(as.numeric(txt))
    if (!is.na(num)) {
      return(num)
    }
    txt
  }
  collect_cond <- function(prefix, n) {
    out <- list()
    for (i in seq_len(max(1L, n))) {
      col <- trimws(input[[paste0(prefix, "_col_", i)]] %||% "")
      if (!nzchar(col)) next
      op <- input[[paste0(prefix, "_op_", i)]] %||% "equals"
      txt <- input[[paste0(prefix, "_val_", i)]]
      inner <- list()
      if (identical(op, "min_max")) {
        # Positional "min, max" parse (no nzchar filter so ", 9" means max-only).
        parts <- trimws(strsplit(txt %||% "", ",", fixed = TRUE)[[1]])
        mn <- if (length(parts) >= 1 && nzchar(parts[1])) suppressWarnings(as.numeric(parts[1])) else NA_real_
        mx <- if (length(parts) >= 2 && nzchar(parts[2])) suppressWarnings(as.numeric(parts[2])) else NA_real_
        if (!is.na(mn)) inner$min <- mn
        if (!is.na(mx)) inner$max <- mx
        if (length(inner) == 0) next
      } else {
        inner[[op]] <- parse_cond_value(op, txt)
      }
      out[[col]] <- inner
    }
    out
  }

  one_group_cond_row <- function(i, cols, pf = list(name = "", col = "", op = "equals", val = "")) {
    rid <- paste0("gcond_row_", i)
    nid <- paste0("gcond_name_", i)
    cid <- paste0("gcond_col_", i)
    oid <- paste0("gcond_op_", i)
    vid <- paste0("gcond_val_", i)
    bid <- paste0("gcond_remove_", i)
    div(
      id = rid,
      class = "cond-row",
      textInput(nid, if (i == 1) "Condition name" else NULL,
        value = pf$name %||% "", width = "100%",
        placeholder = "c1_failed"
      ),
      selectInput(cid, if (i == 1) "Column" else NULL,
        choices = c("(select)" = "", cols),
        selected = pf$col %||% "", width = "100%"
      ),
      selectInput(oid, if (i == 1) "Operator" else NULL,
        choices = dta_condition_operators(),
        selected = pf$op %||% "equals", width = "100%"
      ),
      textInput(vid, if (i == 1) "Value" else NULL,
        value = pf$val %||% "", width = "100%",
        placeholder = "5 | a, b | 0, 99 | true"
      ),
      actionButton(
        bid,
        if (i == 1) "Remove" else "×",
        class = "btn btn-sm btn-outline-danger"
      )
    )
  }

  flatten_group_conditions <- function(conditions) {
    if (is.null(conditions) || length(conditions) == 0) {
      return(list())
    }
    rows <- list()
    for (nm in names(conditions)) {
      spec <- conditions[[nm]]
      converted <- cond_to_rows(spec)
      if (length(converted) == 0) {
        rows[[length(rows) + 1L]] <- list(name = nm, col = "", op = "equals", val = "")
      } else {
        for (entry in converted) {
          rows[[length(rows) + 1L]] <- c(list(name = nm), entry)
        }
      }
    }
    rows
  }

  build_group_cond_rows <- function(n, cols, prefill) {
    lapply(seq_len(max(1L, n)), function(i) {
      pf <- if (i <= length(prefill)) prefill[[i]] else list(name = "", col = "", op = "equals", val = "")
      one_group_cond_row(i, cols, pf)
    })
  }

  collect_group_conditions <- function(n) {
    out <- list()
    for (i in seq_len(max(1L, n))) {
      nm <- trimws(input[[paste0("gcond_name_", i)]] %||% "")
      col <- trimws(input[[paste0("gcond_col_", i)]] %||% "")
      if (!nzchar(nm) || !nzchar(col)) next

      op <- input[[paste0("gcond_op_", i)]] %||% "equals"
      txt <- input[[paste0("gcond_val_", i)]]

      if (is.null(out[[nm]])) {
        out[[nm]] <- list()
      }

      if (is.null(out[[nm]][[col]])) {
        out[[nm]][[col]] <- list()
      }

      if (identical(op, "min_max")) {
        parts <- trimws(strsplit(txt %||% "", ",", fixed = TRUE)[[1]])
        mn <- if (length(parts) >= 1 && nzchar(parts[1])) suppressWarnings(as.numeric(parts[1])) else NA_real_
        mx <- if (length(parts) >= 2 && nzchar(parts[2])) suppressWarnings(as.numeric(parts[2])) else NA_real_
        if (!is.na(mn)) out[[nm]][[col]]$min <- mn
        if (!is.na(mx)) out[[nm]][[col]]$max <- mx
      } else {
        out[[nm]][[col]][[op]] <- parse_cond_value(op, txt)
      }
    }
    out
  }

  one_group_constraint_row <- function(i, cond_names, pf = list()) {
    rowid <- paste0("gconstr_row_", i)
    cid <- paste0("gconstr_id_", i)
    tid <- paste0("gconstr_type_", i)
    lid <- paste0("gconstr_left_", i)
    rid <- paste0("gconstr_right_", i)
    lsid <- paste0("gconstr_lscope_", i)
    rsid <- paste0("gconstr_rscope_", i)
    mid <- paste0("gconstr_msg_", i)
    bid <- paste0("gconstr_remove_", i)

    ctype <- pf$type %||% "mutually_exclusive"
    left_name <- pf$left %||% pf[["if"]] %||% ""
    right_name <- pf$right %||% pf$then %||% ""

    div(
      id = rowid,
      class = "cond-row",
      textInput(cid, if (i == 1) "Constraint id" else NULL,
        value = pf$id %||% "", width = "100%",
        placeholder = paste0("constraint_", i)
      ),
      selectInput(tid, if (i == 1) "Type" else NULL,
        choices = c("mutually_exclusive", "requires"),
        selected = ctype,
        width = "100%"
      ),
      selectInput(lid, if (i == 1) "Left / IF" else NULL,
        choices = c("(select)" = "", cond_names),
        selected = left_name,
        width = "100%"
      ),
      selectInput(rid, if (i == 1) "Right / THEN" else NULL,
        choices = c("(select)" = "", cond_names),
        selected = right_name,
        width = "100%"
      ),
      selectInput(lsid, if (i == 1) "Left/IF scope" else NULL,
        choices = c("any", "all"),
        selected = pf$left_scope %||% pf$if_scope %||% "any",
        width = "100%"
      ),
      selectInput(rsid, if (i == 1) "Right/THEN scope" else NULL,
        choices = c("any", "all"),
        selected = pf$right_scope %||% pf$then_scope %||% "any",
        width = "100%"
      ),
      textInput(mid, if (i == 1) "Message" else NULL,
        value = pf$message %||% "", width = "100%"
      ),
      actionButton(
        bid,
        if (i == 1) "Remove" else "×",
        class = "btn btn-sm btn-outline-danger"
      )
    )
  }

  visible_group_condition_rows <- function() {
    out <- integer(0)
    for (i in seq_len(max(1L, isolate(rv$gcond_n)))) {
      if (!is.null(input[[paste0("gcond_name_", i)]])) {
        out <- c(out, i)
      }
    }
    out
  }

  visible_group_constraint_rows <- function() {
    out <- integer(0)
    for (i in seq_len(max(1L, isolate(rv$gconstr_n)))) {
      if (!is.null(input[[paste0("gconstr_type_", i)]])) {
        out <- c(out, i)
      }
    }
    out
  }

  find_condition_dependencies <- function(condition_name, excluding_row) {
    if (!nzchar(condition_name)) {
      return(character(0))
    }

    # If another condition row with the same name remains, constraints still
    # have a valid target and this row can be removed safely.
    has_other_named_row <- FALSE
    for (k in visible_group_condition_rows()) {
      if (k == excluding_row) next
      nm <- trimws(input[[paste0("gcond_name_", k)]] %||% "")
      if (identical(nm, condition_name)) {
        has_other_named_row <- TRUE
        break
      }
    }
    if (has_other_named_row) {
      return(character(0))
    }

    deps <- character(0)
    for (j in visible_group_constraint_rows()) {
      left <- trimws(input[[paste0("gconstr_left_", j)]] %||% "")
      right <- trimws(input[[paste0("gconstr_right_", j)]] %||% "")
      if (identical(left, condition_name) || identical(right, condition_name)) {
        cid <- trimws(input[[paste0("gconstr_id_", j)]] %||% "")
        deps <- c(deps, if (nzchar(cid)) cid else paste0("constraint_", j))
      }
    }

    unique(deps)
  }

  clear_group_condition_row <- function(i) {
    updateTextInput(session, paste0("gcond_name_", i), value = "")
    updateSelectInput(session, paste0("gcond_col_", i), selected = "")
    updateSelectInput(session, paste0("gcond_op_", i), selected = "equals")
    updateTextInput(session, paste0("gcond_val_", i), value = "")
  }

  clear_group_constraint_row <- function(i) {
    updateTextInput(session, paste0("gconstr_id_", i), value = "")
    updateSelectInput(session, paste0("gconstr_type_", i), selected = "mutually_exclusive")
    updateSelectInput(session, paste0("gconstr_left_", i), selected = "")
    updateSelectInput(session, paste0("gconstr_right_", i), selected = "")
    updateSelectInput(session, paste0("gconstr_lscope_", i), selected = "any")
    updateSelectInput(session, paste0("gconstr_rscope_", i), selected = "any")
    updateTextInput(session, paste0("gconstr_msg_", i), value = "")
  }

  ensure_gcond_remove_observer <- function(i) {
    bid <- paste0("gcond_remove_", i)
    if (isTRUE(gcond_rm_registry[[bid]])) {
      return(invisible(NULL))
    }
    gcond_rm_registry[[bid]] <- TRUE

    observeEvent(input[[bid]],
      {
        row_name <- trimws(input[[paste0("gcond_name_", i)]] %||% "")
        deps <- find_condition_dependencies(row_name, excluding_row = i)

        if (length(deps) > 0) {
          rv$rule_msg <- list(
            ok = FALSE,
            error = paste0(
              "Cannot remove condition '", row_name,
              "' because it is referenced by: ",
              paste(deps, collapse = ", "),
              ". Remove dependent constraints first."
            )
          )
          return()
        }

        vis <- visible_group_condition_rows()
        if (length(vis) <= 1) {
          clear_group_condition_row(i)
        } else {
          removeUI(selector = paste0("#gcond_row_", i))
        }
        rv$rule_msg <- NULL
      },
      ignoreInit = TRUE
    )
  }

  ensure_gconstr_remove_observer <- function(i) {
    bid <- paste0("gconstr_remove_", i)
    if (isTRUE(gconstr_rm_registry[[bid]])) {
      return(invisible(NULL))
    }
    gconstr_rm_registry[[bid]] <- TRUE

    observeEvent(input[[bid]],
      {
        vis <- visible_group_constraint_rows()
        if (length(vis) <= 1) {
          clear_group_constraint_row(i)
        } else {
          removeUI(selector = paste0("#gconstr_row_", i))
        }
        rv$rule_msg <- NULL
      },
      ignoreInit = TRUE
    )
  }

  flatten_group_constraints <- function(constraints) {
    if (is.null(constraints) || length(constraints) == 0) {
      return(list())
    }
    lapply(constraints, function(cst) {
      if (identical(cst$type %||% "", "not_both")) cst$type <- "mutually_exclusive"
      if (identical(cst$type %||% "", "implies")) cst$type <- "requires"
      cst
    })
  }

  collect_group_constraints <- function(n) {
    out <- list()
    for (i in seq_len(max(1L, n))) {
      ctype <- trimws(input[[paste0("gconstr_type_", i)]] %||% "")
      left <- trimws(input[[paste0("gconstr_left_", i)]] %||% "")
      right <- trimws(input[[paste0("gconstr_right_", i)]] %||% "")
      if (!nzchar(ctype) || !nzchar(left) || !nzchar(right)) next

      cid <- trimws(input[[paste0("gconstr_id_", i)]] %||% "")
      msg <- trimws(input[[paste0("gconstr_msg_", i)]] %||% "")
      left_scope <- trimws(input[[paste0("gconstr_lscope_", i)]] %||% "any")
      right_scope <- trimws(input[[paste0("gconstr_rscope_", i)]] %||% "any")

      if (identical(ctype, "requires")) {
        out[[length(out) + 1L]] <- list(
          id = if (nzchar(cid)) cid else paste0("constraint_", i),
          type = "requires",
          `if` = left,
          then = right,
          if_scope = left_scope,
          then_scope = right_scope,
          message = if (nzchar(msg)) msg else NULL
        )
      } else {
        out[[length(out) + 1L]] <- list(
          id = if (nzchar(cid)) cid else paste0("constraint_", i),
          type = "mutually_exclusive",
          left = left,
          right = right,
          left_scope = left_scope,
          right_scope = right_scope,
          message = if (nzchar(msg)) msg else NULL
        )
      }
    }
    out
  }

  # A single modal with two swappable views (list <-> form) so only ONE popup is
  # ever open. rv$rule_view drives the view; rv$rule_token forces re-renders.
  observeEvent(input$edit_rules, {
    req(editing())
    req(rv$active)
    # Same double-gating as edit_cols above: ds_edit_menu() hides this control
    # for a "file" dataset, but the observer must refuse it too, since an
    # input that is not on screen can still be driven over the websocket.
    req(identical(rv$structure[[rv$active]]$type, "tabular"))
    rv$editor_dataset <- rv$active
    rv$rule_view <- "list"
    rv$rule_edit_index <- NULL
    rv$rule_prefill <- list()
    rv$rule_msg <- NULL
    rv$cond_n <- 1L
    rv$then_n <- 1L
    rv$gcond_n <- 1L
    rv$gconstr_n <- 1L
    rv$rule_token <- rv$rule_token + 1
    showModal(modalDialog(
      title = paste("Edit rules \u2014", rv$active),
      size = "xl", easyClose = FALSE,
      uiOutput("rule_modal_body"),
      footer = NULL
    ))
  })

  output$rule_modal_body <- renderUI({
    rv$rule_token
    ed <- isolate(rv$editor_dataset)
    req(ed)
    if (identical(isolate(rv$rule_view), "list")) {
      return(tagList(
        div(
          class = "spec-toolbar",
          actionButton("rule_add", HTML("&#x2795; Add rule"),
            class = "btn btn-sm btn-outline-primary"
          ),
          span(
            class = "spec-hint",
            "Use the pencil to edit a rule or the bin to remove it. Any change resets this dataset's validation."
          )
        ),
        DT::dataTableOutput("rule_tbl"),
        tags$hr(),
        div(style = "text-align:right;", modalButton("Close"))
      ))
    }
    pf <- isolate(rv$rule_prefill) %||% list()
    is_edit <- !is.null(isolate(rv$rule_edit_index))
    rt <- pf$type %||% ""
    cols <- dta_column_ids(isolate(rv$dta), ed)
    type_ui <- if (identical(rt, "col_condition")) {
      tagList(
        div(
          class = "cond-builder",
          div(class = "cond-title", "IF (all of these hold):"),
          div(
            id = "cond_rows",
            build_cond_rows("cond", isolate(rv$cond_n), cols, cond_to_rows(pf$condition))
          ),
          actionButton("cond_add", HTML("&#x2795; Add condition"),
            class = "btn btn-sm btn-outline-secondary"
          )
        ),
        div(
          class = "cond-builder",
          div(class = "cond-title", "THEN (all of these must hold):"),
          div(
            id = "then_rows",
            build_cond_rows("then", isolate(rv$then_n), cols, cond_to_rows(pf$then))
          ),
          actionButton("then_add", HTML("&#x2795; Add THEN condition"),
            class = "btn btn-sm btn-outline-secondary"
          )
        ),
        div(class = "cond-hint", HTML(paste0(
          "Value formats &mdash; single: <code>5</code> &middot; ",
          "list (in / not in): <code>a, b, c</code> &middot; ",
          "between (min/max): <code>0, 99</code> &middot; ",
          "empty: <code>true</code> / <code>false</code> &middot; ",
          "pattern: a regular expression."
        )))
      )
    } else if (identical(rt, "col_range")) {
      tagList(
        selectInput("rule_col_single", "Column",
          choices = c("(select)" = "", cols),
          selected = (pf$columns %||% "")[1], width = "100%"
        ),
        layout_columns(
          col_widths = c(6, 6),
          textInput("rule_min", "Minimum", value = pf$min %||% "", width = "100%"),
          textInput("rule_max", "Maximum", value = pf$max %||% "", width = "100%")
        ),
        div(
          class = "cond-hint",
          "A range rule checks ONE column against a minimum and/or maximum."
        )
      )
    } else if (identical(rt, "col_unique")) {
      tagList(
        selectizeInput("rule_cols", "Column(s) that are unique together",
          choices = cols, selected = pf$columns, multiple = TRUE, width = "100%"
        ),
        div(
          class = "cond-hint",
          "Rows must be unique across the selected column(s) taken together."
        )
      )
    } else if (identical(rt, "group_condition")) {
      group_rows <- flatten_group_conditions(pf$conditions)
      constraint_rows <- flatten_group_constraints(pf$constraints)
      cond_names <- unique(vapply(group_rows, function(x) trimws(x$name %||% ""), character(1)))
      cond_names <- cond_names[nzchar(cond_names)]
      tagList(
        selectizeInput("rule_group_by", "Group by column(s)",
          choices = cols,
          selected = pf$group_by %||% character(0),
          multiple = TRUE,
          width = "100%"
        ),
        div(
          class = "cond-builder",
          div(class = "cond-title", "Conditions (named):"),
          div(
            id = "gcond_rows",
            build_group_cond_rows(
              isolate(rv$gcond_n),
              cols,
              if (length(group_rows) > 0) group_rows else list()
            )
          ),
          actionButton("gcond_add", HTML("&#x2795; Add condition row"),
            class = "btn btn-sm btn-outline-secondary"
          )
        ),
        div(
          class = "cond-builder",
          div(class = "cond-title", "Constraints:"),
          div(
            id = "gconstr_rows",
            lapply(seq_len(max(1L, isolate(rv$gconstr_n))), function(i) {
              pf_row <- if (i <= length(constraint_rows)) constraint_rows[[i]] else list()
              one_group_constraint_row(i, cond_names, pf_row)
            })
          ),
          actionButton("gconstr_add", HTML("&#x2795; Add constraint"),
            class = "btn btn-sm btn-outline-secondary"
          )
        ),
        div(
          class = "cond-hint",
          "Use one condition name across multiple rows to build compound conditions."
        )
      )
    } else {
      div(
        class = "cond-hint",
        "Choose a rule type above to configure it."
      )
    }
    # The rule type is chosen only when the rule is created. When editing an
    # existing rule the type is locked (shown read-only) so it cannot change.
    type_field <- if (is_edit) {
      div(
        class = "form-group shiny-input-container", style = "width:100%;",
        tags$label(class = "control-label", "Type"),
        div(class = "rule-type-fixed", dta_rule_type_label(rt))
      )
    } else {
      selectInput("rule_type", "Type",
        choices = c(
          "\u2014 select a rule type \u2014" = "",
          "Conditional (IF/THEN)" = "col_condition",
          "Range" = "col_range",
          "Unique" = "col_unique",
          "Grouped condition" = "group_condition"
        ),
        selected = rt, width = "100%"
      )
    }
    tagList(
      div(
        class = "spec-form",
        layout_columns(
          col_widths = c(4, 4, 4),
          textInput("rule_id", "Rule ID", value = pf$id %||% "", width = "100%"),
          type_field,
          textInput("rule_desc", "Description", value = pf$description %||% "", width = "100%")
        ),
        type_ui
      ),
      uiOutput("rule_editor_msg"),
      div(
        style = "display:flex; justify-content:space-between; margin-top:8px;",
        actionButton("rule_back", HTML("&#x2190; Back to list"),
          class = "btn btn-outline-secondary"
        ),
        actionButton("rule_save", "Save rule", class = "btn btn-primary")
      )
    )
  })

  output$rule_tbl <- DT::renderDataTable({
    rv$rule_token
    ed <- isolate(rv$editor_dataset)
    req(ed)
    ov <- dta_rules_overview(isolate(rv$dta), ed)
    if (is.null(ov) || nrow(ov) == 0) {
      ov <- data.frame(
        index = integer(0), id = character(0), type = character(0),
        detail = character(0), description = character(0),
        stringsAsFactors = FALSE
      )
    }
    ov$Actions <- if (nrow(ov) > 0) {
      row_action_buttons(
        "rule_edit_click", "rule_del_click", nrow(ov),
        "rule_up_click", "rule_down_click"
      )
    } else {
      character(0)
    }
    DT::datatable(
      ov,
      rownames = FALSE, selection = "none", escape = FALSE,
      class = "display compact", width = "100%",
      options = list(
        pageLength = 8, dom = "tp", scrollX = TRUE,
        columnDefs = list(list(orderable = FALSE, targets = ncol(ov) - 1L))
      )
    )
  })

  output$rule_editor_msg <- renderUI({
    m <- rv$rule_msg
    if (is.null(m)) {
      return(NULL)
    }
    if (isTRUE(m$ok)) {
      div(class = "yaml-valid ok", HTML("&#x2714;"), " Rule saved.")
    } else {
      div(class = "yaml-valid err", HTML("&#x2716;"), " ", m$error)
    }
  })

  observeEvent(input$rule_add, {
    # New rule: the user must first pick a type (no default), so the type-
    # specific fields only appear once a type is chosen.
    rv$rule_edit_index <- NULL
    rv$rule_prefill <- list()
    rv$rule_msg <- NULL
    rv$cond_n <- 1L
    rv$then_n <- 1L
    rv$gcond_n <- 1L
    rv$gconstr_n <- 1L
    rv$rule_view <- "form"
    rv$rule_token <- rv$rule_token + 1
  })

  observeEvent(input$rule_back, {
    rv$rule_view <- "list"
    rv$rule_msg <- NULL
    rv$rule_token <- rv$rule_token + 1
  })

  observeEvent(input$rule_edit_click, {
    idx <- as.integer(input$rule_edit_click)
    ed <- isolate(rv$editor_dataset)
    req(ed)
    if (length(idx) != 1 || is.na(idx)) {
      return()
    }
    f <- dta_rule_fields(isolate(rv$dta), ed, idx)
    if (is.null(f)) {
      return()
    }
    rv$rule_edit_index <- idx
    rv$rule_prefill <- f
    rv$rule_msg <- NULL
    rv$cond_n <- max(1L, length(cond_to_rows(f$condition)))
    rv$then_n <- max(1L, length(cond_to_rows(f$then)))
    rv$gcond_n <- max(1L, length(flatten_group_conditions(f$conditions)))
    rv$gconstr_n <- max(1L, length(flatten_group_constraints(f$constraints)))
    rv$rule_view <- "form"
    rv$rule_token <- rv$rule_token + 1
  })

  observeEvent(input$rule_del_click, {
    req(editing())
    idx <- as.integer(input$rule_del_click)
    ed <- isolate(rv$editor_dataset)
    req(ed)
    if (length(idx) != 1 || is.na(idx)) {
      return()
    }
    r <- dta_remove_rule(isolate(rv$dta), ed, idx)
    if (!isTRUE(r$ok)) {
      showNotification(r$error, type = "error")
      return()
    }
    rv$dta <- r$value
    invalidate_dataset(ed)
    rv$rule_token <- rv$rule_token + 1
    sync_yaml_text()
    showNotification("Rule removed.", type = "message")
  })

  observeEvent(input$rule_up_click, {
    req(editing())
    idx <- as.integer(input$rule_up_click)
    ed <- isolate(rv$editor_dataset)
    req(ed)
    if (length(idx) != 1 || is.na(idx) || idx <= 1) {
      return()
    }
    r <- dta_move_rule(isolate(rv$dta), ed, idx, "up")
    if (!isTRUE(r$ok)) {
      showNotification(r$error, type = "error")
      return()
    }
    rv$dta <- r$value
    invalidate_dataset(ed)
    rv$rule_token <- rv$rule_token + 1
    sync_yaml_text()
  })

  observeEvent(input$rule_down_click, {
    req(editing())
    idx <- as.integer(input$rule_down_click)
    ed <- isolate(rv$editor_dataset)
    req(ed)
    n <- nrow(dta_rules_overview(isolate(rv$dta), ed))
    if (length(idx) != 1 || is.na(idx) || idx < 1 || idx >= n) {
      return()
    }
    r <- dta_move_rule(isolate(rv$dta), ed, idx, "down")
    if (!isTRUE(r$ok)) {
      showNotification(r$error, type = "error")
      return()
    }
    rv$dta <- r$value
    invalidate_dataset(ed)
    rv$rule_token <- rv$rule_token + 1
    sync_yaml_text()
  })

  observeEvent(input$cond_add, {
    ed <- isolate(rv$editor_dataset)
    req(ed)
    cols <- dta_column_ids(isolate(rv$dta), ed)
    rv$cond_n <- isolate(rv$cond_n) + 1L
    insertUI("#cond_rows",
      where = "beforeEnd",
      ui = one_cond_row("cond", isolate(rv$cond_n), cols)
    )
  })
  observeEvent(input$then_add, {
    ed <- isolate(rv$editor_dataset)
    req(ed)
    cols <- dta_column_ids(isolate(rv$dta), ed)
    rv$then_n <- isolate(rv$then_n) + 1L
    insertUI("#then_rows",
      where = "beforeEnd",
      ui = one_cond_row("then", isolate(rv$then_n), cols)
    )
  })

  observeEvent(input$gcond_add, {
    ed <- isolate(rv$editor_dataset)
    req(ed)
    cols <- dta_column_ids(isolate(rv$dta), ed)
    rv$gcond_n <- isolate(rv$gcond_n) + 1L
    insertUI("#gcond_rows",
      where = "beforeEnd",
      ui = one_group_cond_row(isolate(rv$gcond_n), cols)
    )
  })

  observeEvent(input$gconstr_add, {
    cond_names <- character(0)
    for (i in seq_len(max(1L, isolate(rv$gcond_n)))) {
      nm <- trimws(input[[paste0("gcond_name_", i)]] %||% "")
      if (nzchar(nm)) cond_names <- c(cond_names, nm)
    }
    cond_names <- unique(cond_names)
    rv$gconstr_n <- isolate(rv$gconstr_n) + 1L
    insertUI("#gconstr_rows",
      where = "beforeEnd",
      ui = one_group_constraint_row(isolate(rv$gconstr_n), cond_names)
    )
  })

  observe({
    req(identical(rv$rule_view, "form"))
    req(rv$gconstr_n >= 1)

    for (i in seq_len(max(1L, rv$gcond_n))) {
      ensure_gcond_remove_observer(i)
    }
    for (i in seq_len(max(1L, rv$gconstr_n))) {
      ensure_gconstr_remove_observer(i)
    }

    cond_names <- character(0)
    for (i in seq_len(max(1L, rv$gcond_n))) {
      nm <- trimws(input[[paste0("gcond_name_", i)]] %||% "")
      if (nzchar(nm)) cond_names <- c(cond_names, nm)
    }
    cond_names <- unique(cond_names)

    for (i in seq_len(max(1L, rv$gconstr_n))) {
      left_id <- paste0("gconstr_left_", i)
      right_id <- paste0("gconstr_right_", i)
      left_selected <- input[[left_id]]
      right_selected <- input[[right_id]]

      updateSelectInput(
        session,
        left_id,
        choices = c("(select)" = "", cond_names),
        selected = if (!is.null(left_selected) && nzchar(left_selected)) left_selected else ""
      )
      updateSelectInput(
        session,
        right_id,
        choices = c("(select)" = "", cond_names),
        selected = if (!is.null(right_selected) && nzchar(right_selected)) right_selected else ""
      )
    }
  })

  # When the user switches the rule type, reset the type-specific part but keep
  # the id/description they typed. A programmatic sync (the select rendering with
  # its prefilled value) is ignored by comparing against the prefill's type.
  observeEvent(input$rule_type,
    {
      if (!identical(isolate(rv$rule_view), "form")) {
        return()
      }
      newt <- input$rule_type %||% ""
      pf <- isolate(rv$rule_prefill) %||% list()
      if (!nzchar(newt)) {
        return()
      } # ignore the "(select)" placeholder
      if (identical(newt, pf$type %||% "")) {
        return()
      } # no real change
      pf$id <- input$rule_id %||% pf$id
      pf$description <- input$rule_desc %||% pf$description
      pf$type <- newt
      pf$condition <- NULL
      pf$then <- NULL
      pf$columns <- NULL
      pf$min <- NULL
      pf$max <- NULL
      pf$group_by <- NULL
      pf$conditions <- NULL
      pf$constraints <- NULL
      rv$rule_prefill <- pf
      rv$cond_n <- 1L
      rv$then_n <- 1L
      rv$gcond_n <- 1L
      rv$gconstr_n <- 1L
      rv$rule_msg <- NULL
      rv$rule_token <- rv$rule_token + 1
    },
    ignoreInit = TRUE
  )

  observeEvent(input$rule_save, {
    req(editing())
    ed <- isolate(rv$editor_dataset)
    req(ed)
    # Same defense-in-depth as col_save: don't trust the stashed
    # rv$editor_dataset alone, re-check its type is "tabular".
    req(identical(rv$structure[[ed]]$type, "tabular"))
    id <- trimws(input$rule_id %||% "")
    if (!nzchar(id)) {
      rv$rule_msg <- list(ok = FALSE, error = "A rule ID is required.")
      return()
    }
    # On edit the type is locked, so read it from the prefill (the select input
    # is not rendered); on add the user must have picked a type.
    rt <- if (!is.null(isolate(rv$rule_edit_index))) {
      isolate(rv$rule_prefill$type) %||% "col_condition"
    } else {
      trimws(input$rule_type %||% "")
    }
    if (!nzchar(rt)) {
      rv$rule_msg <- list(ok = FALSE, error = "Please choose a rule type.")
      return()
    }
    args <- list(
      dta = isolate(rv$dta), dataset = ed, index = isolate(rv$rule_edit_index),
      id = id, type = rt, description = input$rule_desc
    )
    if (identical(rt, "col_condition")) {
      cond <- collect_cond("cond", isolate(rv$cond_n))
      then <- collect_cond("then", isolate(rv$then_n))
      if (length(cond) == 0 || length(then) == 0) {
        rv$rule_msg <- list(
          ok = FALSE,
          error = "A conditional rule needs at least one IF and one THEN condition."
        )
        return()
      }
      args$condition <- cond
      args$then <- then
    } else if (identical(rt, "col_range")) {
      col1 <- trimws(input$rule_col_single %||% "")
      if (!nzchar(col1)) {
        rv$rule_msg <- list(ok = FALSE, error = "Select the column to range-check.")
        return()
      }
      args$columns <- col1
      mn <- suppressWarnings(as.numeric(input$rule_min))
      mx <- suppressWarnings(as.numeric(input$rule_max))
      args$min <- if (!is.na(mn)) mn else NULL
      args$max <- if (!is.na(mx)) mx else NULL
      if (is.null(args$min) && is.null(args$max)) {
        rv$rule_msg <- list(ok = FALSE, error = "A range rule needs a minimum and/or maximum.")
        return()
      }
    } else if (identical(rt, "col_unique")) {
      cols <- input$rule_cols
      if (length(cols) == 0) {
        rv$rule_msg <- list(ok = FALSE, error = "Select at least one column.")
        return()
      }
      args$columns <- cols
    } else if (identical(rt, "group_condition")) {
      gby <- input$rule_group_by %||% character(0)
      if (length(gby) == 0) {
        rv$rule_msg <- list(ok = FALSE, error = "Select at least one grouping column.")
        return()
      }

      gconds <- collect_group_conditions(isolate(rv$gcond_n))
      if (length(gconds) == 0) {
        rv$rule_msg <- list(
          ok = FALSE,
          error = "Define at least one named grouped condition with a column and operator."
        )
        return()
      }

      gconstraints <- collect_group_constraints(isolate(rv$gconstr_n))
      if (length(gconstraints) == 0) {
        rv$rule_msg <- list(ok = FALSE, error = "Define at least one grouped constraint.")
        return()
      }

      args$group_by <- gby
      args$conditions <- gconds
      args$constraints <- gconstraints
    } else {
      rv$rule_msg <- list(ok = FALSE, error = sprintf("Unsupported rule type: %s", rt))
      return()
    }
    r <- do.call(dta_set_rule, args)
    if (!isTRUE(r$ok)) {
      rv$rule_msg <- list(ok = FALSE, error = r$error)
      return()
    }
    rv$dta <- r$value
    invalidate_dataset(ed)
    rv$rule_msg <- NULL
    rv$rule_view <- "list"
    rv$rule_token <- rv$rule_token + 1
    sync_yaml_text()
    showNotification("Rule saved.", type = "message")
  })

  # ======================= Edit dataset metadata ==========================
  # The DATASET's own properties -- name, description, template_* -- as opposed
  # to the DOCUMENT-level DTAMetaData edited on the Metadata page. Both are
  # called metadata; these belong to one dataset and travel with it in the
  # exported dataset YAML.
  #
  # Unlike the file, column and rule editors this is a SINGLE-VIEW modal: those
  # three edit a collection and need a list view to pick from, while a dataset
  # has exactly one metadata record, so a list would be an empty ceremony.
  # rv$meta_token drives the re-render; the form pre-fills from rv$meta_prefill
  # rather than from live inputs, so a re-render never resurrects a stale value.
  #
  # `type` is NOT offered. See the note in dta_set_dataset_meta(): it is fixed by
  # the concrete S7 class, and assigning it would produce a dataset whose
  # declared type and actual behaviour disagree. It is shown as a chip on the
  # dataset page, which is where a fact about the dataset belongs.
  show_meta_editor_modal <- function(ed) {
    showModal(modalDialog(
      # "Edit details", not "Edit metadata": this is the modal the Details row
      # of ds_edit_menu() opens (ui_components.R), and its title should read
      # like the label the user just clicked, not the internal name the
      # server-side code still uses for the concept (edit_meta, rv$meta_*).
      title = paste("Edit details —", ed),
      size = "l", easyClose = FALSE,
      uiOutput("meta_modal_body"),
      footer = NULL
    ))
  }

  observeEvent(input$edit_meta, {
    req(editing())
    req(rv$active)
    rv$editor_dataset <- rv$active
    rv$meta_prefill <- dta_dataset_meta_fields(rv$dta, rv$active)
    rv$meta_msg <- NULL
    rv$meta_token <- rv$meta_token + 1
    show_meta_editor_modal(rv$active)
  })

  output$meta_modal_body <- renderUI({
    rv$meta_token
    ed <- isolate(rv$editor_dataset)
    req(ed)
    pf <- isolate(rv$meta_prefill) %||% list()
    g <- function(k, d = "") pf[[k]] %||% d
    tagList(
      div(
        class = "spec-form",
        textInput("meta_name", "Dataset name",
          value = g("name"), width = "100%",
          placeholder = "vitals"
        ),
        div(
          class = "msg-hint", style = "margin:-8px 0 12px;",
          paste(
            "The name identifies the dataset in the specification, in every",
            "validation message and in the exported documents. Renaming it",
            "clears this dataset's validation, because results recorded under",
            "the old name no longer describe it."
          )
        ),
        textAreaInput("meta_description", "Description",
          value = g("description"), width = "100%", rows = 2,
          placeholder = "What this dataset contains, in words"
        ),
        div(
          class = "msg-hint", style = "margin:-8px 0 12px;",
          "Shown as the heading of this dataset's page when set."
        ),
        tags$h6("Template", style = "margin-top:4px;"),
        div(
          class = "msg-hint", style = "margin:-4px 0 8px;",
          "Where this dataset's specification came from. Documentation only — none of it affects validation."
        ),
        layout_columns(
          col_widths = c(4, 4, 4),
          textInput("meta_template_source", "Source",
            value = g("template_source"), width = "100%"
          ),
          textInput("meta_template_version", "Version",
            value = g("template_version"), width = "100%"
          ),
          textInput("meta_template_date", "Date",
            value = g("template_date"), width = "100%",
            placeholder = "2026-01-15"
          )
        ),
        div(
          class = "msg-hint", style = "margin:-8px 0 0;",
          "Clearing a field removes it from the specification entirely."
        )
      ),
      uiOutput("meta_editor_msg"),
      tags$hr(),
      div(
        style = "display:flex; justify-content:space-between; margin-top:8px;",
        modalButton("Close"),
        actionButton("meta_save", "Save metadata", class = "btn btn-primary")
      )
    )
  })

  output$meta_editor_msg <- renderUI({
    m <- rv$meta_msg
    if (is.null(m) || isTRUE(m$ok)) {
      return(NULL)
    }
    div(class = "yaml-valid err", HTML("&#x2716;"), " ", m$error)
  })

  observeEvent(input$meta_save, {
    req(editing())
    ed <- isolate(rv$editor_dataset)
    req(ed)
    r <- dta_set_dataset_meta(
      isolate(rv$dta), ed,
      name = input$meta_name,
      description = input$meta_description,
      template_source = input$meta_template_source,
      template_version = input$meta_template_version,
      template_date = input$meta_template_date
    )
    if (!isTRUE(r$ok)) {
      # The modal stays open with everything the user typed still in it, and
      # nothing in rv has changed.
      rv$meta_msg <- list(ok = FALSE, error = r$error)
      return()
    }

    new_name <- trimws(as.character(input$meta_name %||% "")[1])
    renamed <- !identical(new_name, ed)
    rv$dta <- r$value

    # ORDER MATTERS: the state migration has to land before anything reads the
    # dataset by name again -- invalidate_dataset() looks its status up by name,
    # and every output keyed off rv$active re-renders as soon as it changes.
    if (renamed) {
      rename_dataset_state(ed, new_name)
      # A rename does not change what the data IS, but every stored validation
      # record carries the dataset name it was checked under, so results left
      # in place would report a dataset that no longer exists. The description
      # and template fields take no part in validation, so editing only those
      # deliberately leaves a passed check passed.
      invalidate_dataset(new_name)
      # Only a rename touches rv$structure -- it is keyed by name and caches
      # each dataset's name, so the nav list would otherwise keep showing the
      # old one. Assigning it re-renders every structure-dependent output
      # (dataset nav, detail panel and its file inputs), which is why the
      # column and rule editors never touch it and why a description-only edit
      # here must not either. It does NOT rebuild the whole workspace:
      # output$main depends only on rv$doc_token (load/reset/restore).
      rv$structure <- build_structure(rv$dta)
    }

    rv$meta_msg <- NULL
    sync_yaml_text()
    removeModal()
    showNotification("Metadata updated.", type = "message")
  })

  # --- messages + inspect -------------------------------------------------
  msgs_r <- reactive({
    req(rv$active)
    rv$status # depend on validation state
    dta_dataset_messages(rv$dta, rv$active)
  })

  # Floating, foldable dock that holds the validation messages for the ACTIVE
  # dataset. Rendered once per loaded document -- rv$doc_token, the same
  # contract as output$main, so the DT inside stays stable across dataset
  # add/remove/rename; the table + count badge update reactively via their own
  # outputs. The server sends 'dta_msgs_dock' -> 'open' after a check that
  # produced messages.
  output$floating_msgs <- renderUI({
    rv$doc_token
    if (is.null(isolate(rv$structure))) {
      return(NULL)
    }
    div(
      id = "dta-msgs-dock", class = "msgs-dock collapsed",
      div(
        class = "msgs-dock-bar", onclick = "DTA_toggleMsgsDock(event)",
        title = "Click to fold or unfold the validation messages",
        tags$span(class = "msgs-dock-title", HTML("&#x2696;&#xFE0F; Validation messages")),
        uiOutput("msgs_dock_meta", inline = TRUE),
        div(
          class = "msgs-dock-actions",
          # The .msgs-dock-dl flex container IS the uiOutput's own span: an
          # extra wrapper around it would leave the buttons as inline children
          # of a one-child flex box and lose the gap between them.
          uiOutput("msgs_dock_dl",
            inline = TRUE, class = "msgs-dock-dl",
            onclick = "event.stopPropagation();"
          ),
          tags$span(class = "msgs-dock-chevron", HTML("&#x25BC;"))
        )
      ),
      div(
        class = "msgs-dock-body",
        div(class = "msgs-table", DT::dataTableOutput("msgs")),
        div(
          class = "msg-hint",
          "Use the filters at the top of each column to search or pick a Dataset / Table. Click a message row to open the detailed inspect report."
        )
      )
    )
  })

  # Has the ACTIVE dataset actually been checked? "pending" (data bound, never
  # validated) and "nodata" (skipped for missing data) are both NOT a check --
  # see the same distinction drawn for the sidebar's Validation summary.
  msgs_active_checked <- reactive({
    isTRUE(dta_lookup(rv$status, rv$active, "pending") %in% c("pass", "fail"))
  })

  # Has ANY dataset been checked? The whole-DTA report follows this, not the
  # active dataset.
  msgs_any_checked <- reactive({
    st <- unlist(rv$status)
    length(st) > 0 && any(st %in% c("pass", "fail"))
  })

  # The dock's download buttons, in their own output rather than inline in
  # output$floating_msgs: the dock is deliberately rendered once per structure
  # so the DT inside stays stable, and therefore cannot react to validation
  # state -- but these buttons must.
  #
  # Before a check has been run there is nothing to export. Exporting anyway
  # would hand back a file that reads as a clean result ("No validation
  # messages for this dataset.") when in truth nothing was ever looked at, so
  # the buttons are disabled instead.
  #
  # The three table exports are scoped to the active dataset and follow ITS
  # status; "Report" is the whole-DTA write_validation_report() output and
  # follows whether any dataset has been checked. The two can legitimately
  # disagree, so they are gated separately.
  output$msgs_dock_dl <- renderUI({
    active_ok <- msgs_active_checked()
    any_ok <- msgs_any_checked()
    ds <- rv$active %||% "this dataset"

    # A DISABLED downloadButton is not an option: shiny already renders every
    # downloadButton with class "disabled" and aria-disabled, and its
    # shiny-download-link binding drops both the moment the handler URL
    # arrives from the server -- which it always does, since the handler is
    # registered unconditionally. Marking it up as disabled therefore changes
    # nothing on screen.
    #
    # So the off state is a look-alike <button> instead: same Bootstrap
    # classes, no shiny-download-link class, and hence no binding to switch it
    # back on. Bootstrap 5's `.btn.disabled` (pointer-events: none) plus the
    # `disabled` attribute -- which a <button>, unlike an <a>, actually honours
    # -- make it inert for mouse and keyboard alike.
    dl_btn <- function(id, label, enabled, title) {
      if (enabled) {
        return(downloadButton(id, label,
          class = "btn btn-sm btn-outline-secondary", title = title
        ))
      }
      tags$button(
        id = id, type = "button", disabled = NA,
        class = "btn btn-sm btn-outline-secondary disabled",
        `aria-disabled` = "true", title = title,
        icon("download"), " ", label
      )
    }

    tagList(
      dl_btn("dl_msgs_csv", "CSV", active_ok, if (active_ok) {
        sprintf("Download the validation messages for %s as CSV", ds)
      } else {
        sprintf("Run a check on %s first -- there are no results to export yet", ds)
      }),
      dl_btn("dl_msgs_tsv", "TSV", active_ok, if (active_ok) {
        sprintf("Download the validation messages for %s as TSV", ds)
      } else {
        sprintf("Run a check on %s first -- there are no results to export yet", ds)
      }),
      dl_btn("dl_msgs_xlsx", "XLSX", active_ok, if (active_ok) {
        sprintf("Download the validation messages for %s as XLSX", ds)
      } else {
        sprintf("Run a check on %s first -- there are no results to export yet", ds)
      }),
      # Named apart from the sidebar's "Validation summary": this is the
      # message-level report, that one is the per-dataset outcome.
      dl_btn("dl_msgs_html", "Report", any_ok, if (any_ok) {
        "Download the full validation message report"
      } else {
        "Run a check first -- there are no results to report yet"
      })
    )
  })

  # Count badge + active-dataset label shown in the dock header bar.
  output$msgs_dock_meta <- renderUI({
    req(rv$active)
    m <- msgs_r()
    n <- if (is.null(m)) 0L else nrow(m)
    tagList(
      tags$span(
        class = paste0("msgs-dock-count", if (n == 0) " zero" else ""),
        sprintf("%d", n)
      ),
      tags$span(class = "msgs-dock-ds", rv$active)
    )
  })

  output$msgs <- DT::renderDataTable({
    disp <- messages_display(msgs_r())
    if (is.null(disp) || nrow(disp) == 0) {
      return(DT::datatable(
        data.frame(Message = "No validation messages."),
        rownames = FALSE, options = list(dom = "t"), selection = "none"
      ))
    }
    # Dropdown (select) filters for categorical columns; search boxes for the
    # rest -- lets the user pick a Dataset/Table or type a free-text search.
    for (fc in intersect(c("Dataset", "Table", "Source"), names(disp))) {
      disp[[fc]] <- as.factor(disp[[fc]])
    }
    # Keep the table full-width but bias the available space toward the
    # Message column: the short metadata columns get small explicit widths so
    # the (usually long) Message text gets the lion's share of the row.
    col_w <- c(
      ID = "46px", Dataset = "110px", Table = "120px", Source = "82px",
      Row = "54px", Column = "96px", Rule = "130px", Message = "50%"
    )
    nm <- names(disp)
    coldefs <- lapply(seq_along(nm), function(i) {
      cls <- if (identical(nm[i], "Message")) "msg-cell" else "dt-nowrap"
      d <- list(targets = i - 1L, className = cls)
      if (nm[i] %in% names(col_w)) d$width <- col_w[[nm[i]]]
      d
    })
    DT::datatable(
      disp,
      rownames = FALSE, selection = "single", filter = "top",
      class = "display compact", width = "100%",
      options = list(
        pageLength = 8, dom = "tp", scrollX = TRUE,
        autoWidth = TRUE, columnDefs = coldefs
      )
    )
  })

  # Shared display shape (column selection + pretty names) for the messages
  # table AND the CSV/TSV/XLSX downloads, so the two never diverge.
  messages_display <- function(m) {
    disp <- as.data.frame(m)
    if (is.null(disp) || nrow(disp) == 0) {
      return(disp)
    }
    # Surface WHERE each message comes from: dataset + table (source table).
    if ("target" %in% names(disp)) names(disp)[names(disp) == "target"] <- "table"
    cols <- intersect(
      c("id", "dataset", "table", "source", "row", "column", "rule_id", "message"),
      names(disp)
    )
    disp <- disp[, cols, drop = FALSE]
    pretty <- c(
      id = "ID", dataset = "Dataset", table = "Table", source = "Source",
      row = "Row", column = "Column", rule_id = "Rule", message = "Message"
    )
    hit <- names(disp) %in% names(pretty)
    names(disp)[hit] <- unname(pretty[names(disp)[hit]])
    disp
  }

  # Downloadable messages (CSV / TSV / XLSX). Same display shape as the table;
  # falls back to a one-row note when the dataset has no messages.
  msgs_dl_df <- reactive({
    disp <- messages_display(msgs_r())
    if (is.null(disp) || nrow(disp) == 0) {
      disp <- data.frame(Message = "No validation messages for this dataset.")
    }
    disp
  })
  msgs_dl_base <- function() {
    nm <- rv$active %||% "dataset"
    paste0(gsub("[^A-Za-z0-9._-]+", "_", nm), "_validation_messages")
  }
  # The `req()` in each content function is not redundant with the disabled
  # buttons above: a downloadHandler's URL stays reachable whatever the button
  # looks like, so the "no check, no export" rule is enforced here as well.
  output$dl_msgs_csv <- downloadHandler(
    filename = function() paste0(msgs_dl_base(), ".csv"),
    content = function(file) {
      req(msgs_active_checked())
      utils::write.csv(msgs_dl_df(), file, row.names = FALSE, na = "")
    }
  )
  output$dl_msgs_tsv <- downloadHandler(
    filename = function() paste0(msgs_dl_base(), ".tsv"),
    content = function(file) {
      req(msgs_active_checked())
      utils::write.table(
        msgs_dl_df(), file,
        sep = "\t", row.names = FALSE, na = "", qmethod = "double"
      )
    }
  )
  output$dl_msgs_xlsx <- downloadHandler(
    filename = function() paste0(msgs_dl_base(), ".xlsx"),
    content = function(file) {
      req(msgs_active_checked())
      if (!requireNamespace("writexl", quietly = TRUE)) {
        showNotification(
          "XLSX export needs the 'writexl' package. Install it, or use CSV / TSV.",
          type = "error", duration = 10
        )
        stop("writexl not available")
      }
      writexl::write_xlsx(msgs_dl_df(), file)
    }
  )

  # Filename base for the whole-DTA HTML report (unlike msgs_dl_base(), which
  # is scoped to the single active dataset). Timestamped so repeated
  # downloads across a working session don't overwrite each other in the
  # browser's downloads folder.
  report_dl_base <- function() {
    title <- tryCatch(DTAtools::metadata(rv$dta)@title, error = function(e) NULL)
    nm <- if (!is.null(title) && length(title) == 1 && nzchar(title)) title else "dta"
    paste0(
      gsub("[^A-Za-z0-9._-]+", "_", nm), "_validation_report_",
      format(Sys.time(), "%Y%m%d_%H%M%S")
    )
  }
  output$dl_msgs_html <- downloadHandler(
    filename = function() paste0(report_dl_base(), ".html"),
    content = function(file) {
      req(msgs_any_checked())
      tryCatch(
        DTAtools::write_validation_report(rv$dta, file, overwrite = TRUE, quiet = TRUE),
        error = function(e) {
          showNotification(
            paste("Could not build the validation report:", conditionMessage(e)),
            type = "error", duration = 10
          )
          stop(e)
        }
      )
    }
  )

  df_to_kv <- function(d) {
    # single-row -> transposed Field/Value (drop NA/empty)
    row <- d[1, , drop = FALSE]
    keep <- vapply(names(row), function(k) {
      v <- row[[k]]
      !(is.null(v) || all(is.na(v)) || (is.character(v) && all(!nzchar(v))))
    }, logical(1))
    fields <- names(row)[keep]
    tags$table(
      class = "table table-sm",
      tags$tbody(
        lapply(fields, function(k) {
          tags$tr(
            tags$th(style = "white-space:nowrap; color:var(--bi-grey);", k),
            tags$td(as.character(row[[k]]))
          )
        })
      )
    )
  }

  df_to_html_table <- function(d) {
    # drop all-NA columns
    keep <- vapply(d, function(v) !all(is.na(v)), logical(1))
    d <- d[, keep, drop = FALSE]
    tags$div(
      style = "overflow-x:auto;",
      tags$table(
        class = "table table-sm table-striped",
        tags$thead(tags$tr(lapply(names(d), tags$th))),
        tags$tbody(
          lapply(seq_len(nrow(d)), function(i) {
            tags$tr(lapply(names(d), function(k) tags$td(as.character(d[[k]][i]))))
          })
        )
      )
    )
  }

  # --- inspect helpers: a plain-language description of the failing rule/
  # constraint (what it was) plus a highlighted "should be" vs "actual" split.
  .first_nonempty <- function(...) {
    for (v in list(...)) {
      if (!is.null(v) && length(v) >= 1 && !is.na(v[[1]]) && nzchar(as.character(v[[1]]))) {
        return(as.character(v[[1]]))
      }
    }
    ""
  }

  # Human "should be" text for a column spec violation, derived from its keyword.
  columnspec_expected_text <- function(r) {
    kw <- as.character(r[["columnspec_keyword"]] %||% "")
    switch(kw,
      enum = paste0("one of: ", .first_nonempty(
        r[["columnspec_params.allowedValues"]],
        r[["columnspec_parent.enum"]],
        r[["columnspec_columnspec"]]
      )),
      const = paste0("exactly: ", .first_nonempty(
        r[["columnspec_parent.const"]],
        r[["columnspec_columnspec"]]
      )),
      maxLength = paste0(
        "at most ", .first_nonempty(
          r[["columnspec_params.limit"]],
          r[["columnspec_parent.maxLength"]]
        ),
        " character(s)"
      ),
      minLength = paste0(
        "at least ", .first_nonempty(
          r[["columnspec_params.limit"]],
          r[["columnspec_parent.minLength"]]
        ),
        " character(s)"
      ),
      maximum = paste0("at most ", .first_nonempty(r[["columnspec_params.limit"]])),
      minimum = paste0("at least ", .first_nonempty(r[["columnspec_params.limit"]])),
      type = paste0("type: ", .first_nonempty(r[["columnspec_parent.type"]])),
      pattern = paste0("match pattern ", .first_nonempty(
        r[["columnspec_params.pattern"]],
        r[["columnspec_columnspec"]]
      )),
      required = "the value must be present (not missing)",
      additionalProperties = "no such column to be present: the specs do not declare it",
      .first_nonempty(r[["columnspec_message"]], r[["message"]], "(see message)")
    )
  }

  # Highlighted table of the failing rows captured for a rule violation.
  inspect_failing_rows_ui <- function(d) {
    fcols <- grep("^failing_", names(d), value = TRUE)
    fcols <- setdiff(fcols, "failing_row_count")
    if (length(fcols) == 0) {
      return(tags$em(class = "inspect-none", "No offending rows were captured."))
    }
    sub <- d[, fcols, drop = FALSE]
    names(sub) <- sub("^failing_", "", names(sub))
    names(sub)[names(sub) == ".row"] <- "Row"
    if ("Row" %in% names(sub)) {
      sub <- sub[, c("Row", setdiff(names(sub), "Row")), drop = FALSE]
    }
    tags$table(
      class = "inspect-hl-table",
      tags$thead(tags$tr(lapply(names(sub), function(k) tags$th(k)))),
      tags$tbody(
        lapply(seq_len(nrow(sub)), function(i) {
          tags$tr(lapply(names(sub), function(k) {
            is_row <- identical(k, "Row")
            tags$td(
              class = if (is_row) "inspect-hl-row" else "inspect-hl-val",
              as.character(sub[[k]][i])
            )
          }))
        })
      )
    )
  }

  # Build the full inspect modal body: a summary card describing the failing
  # rule/constraint (Task: "what the rule was"), a highlighted should-be vs
  # actual split, and the raw technical detail in a collapsible section.
  render_inspect_body <- function(d, dataset) {
    r <- as.list(d[1, , drop = FALSE])
    # `source` is the fallback for `type`: both name the axis ("columnspec", "rule",
    # "import"), and falling back on the rule_id guess alone would route an
    # import record into the column spec branch.
    typ <- .first_nonempty(r[["type"]], r[["source"]])
    if (!nzchar(typ)) {
      typ <- if ("rule_id" %in% names(d)) "rule" else "columnspec"
    }
    msg <- .first_nonempty(r$message, r$headline)

    if (identical(typ, "rule")) {
      rid <- as.character(r$rule_id %||% "")
      ov <- tryCatch(dta_rules_overview(rv$dta, dataset), error = function(e) NULL)
      rrow <- if (!is.null(ov) && nrow(ov) > 0 && nzchar(rid)) {
        ov[ov$id == rid, , drop = FALSE]
      } else {
        NULL
      }
      have <- !is.null(rrow) && nrow(rrow) > 0
      rtype <- if (have) rrow$type[1] else ""
      rdetail <- if (have) rrow$detail[1] else ""
      rdesc <- if (have) rrow$description[1] else ""
      badge <- tags$span(class = "inspect-badge rule", "Rule failure")
      desc <- div(
        class = "inspect-desc",
        div(
          class = "inspect-desc-main",
          tags$strong(if (nzchar(rid)) rid else "(rule)"),
          if (nzchar(rtype)) {
            tags$span(
              class = "inspect-desc-type",
              dta_rule_type_label(rtype)
            )
          }
        ),
        if (nzchar(rdetail)) div(class = "inspect-desc-detail", rdetail),
        if (nzchar(rdesc)) div(class = "inspect-desc-note", rdesc)
      )
      expected_ui <- div(
        class = "inspect-should",
        if (nzchar(rdetail)) rdetail else msg
      )
      actual_ui <- inspect_failing_rows_ui(d)
      actual_title <- "Offending row(s) \u2014 actual values"

      # For group condition rules render an additional breakdown table showing
      # each violation: group, constraint, message, and all row numbers involved.
      gvcols <- grep("^group_violation_", names(d), value = TRUE)
      if (length(gvcols) > 0) {
        gv <- d[, gvcols, drop = FALSE]
        # dta_flatten_inspect_record recycles the violation rows to match the
        # number of failing_rows_preview rows, so we de-duplicate on content.
        gv <- unique(gv)
        # Drop fully-empty rows (all empty strings) that may arise from padding.
        has_content <- vapply(seq_len(nrow(gv)), function(i) {
          any(nzchar(as.character(gv[i, , drop = TRUE])))
        }, logical(1))
        gv <- gv[has_content, , drop = FALSE]
        if (nrow(gv) > 0) {
          names(gv) <- sub("^group_violation_", "", names(gv))
          names(gv) <- tools::toTitleCase(names(gv))
          actual_title <- "Group constraint violations \u2014 details"
          failing_fcols <- grep("^failing_", names(d), value = TRUE)
          actual_ui <- tagList(
            tags$table(
              class = "inspect-hl-table",
              tags$thead(tags$tr(lapply(names(gv), tags$th))),
              tags$tbody(
                lapply(seq_len(nrow(gv)), function(i) {
                  tags$tr(lapply(names(gv), function(k) {
                    tags$td(
                      class = "inspect-hl-val",
                      as.character(gv[[k]][i])
                    )
                  }))
                })
              )
            ),
            if (length(failing_fcols) > 0) {
              tagList(
                tags$p(
                  class = "inspect-desc-note",
                  style = "margin-top:1em;",
                  "Rows involved (all values):"
                ),
                inspect_failing_rows_ui(d)
              )
            }
          )
        }
      }
    } else if (identical(typ, "import")) {
      # Third validation axis: the value could not be represented in the type
      # the spec declares, so the typed column holds NA and the raw text was
      # kept. inspect() supplies it as import_* columns (from import_matches).
      # Without this branch the record fell into the column spec branch below and
      # rendered two empty columnspec_* panels.
      f <- dta_inspect_import_fields(r)
      col <- f$column
      raw <- f$raw
      dtype <- f$declared_type
      reason <- f$reason
      arow <- f$row
      badge <- tags$span(class = "inspect-badge import", "Import error")
      desc <- div(
        class = "inspect-desc",
        div(
          class = "inspect-desc-main",
          tags$strong(if (nzchar(col)) col else "(column)"),
          if (nzchar(dtype)) tags$span(class = "inspect-desc-type", dtype)
        ),
        if (nzchar(reason)) div(class = "inspect-desc-detail", reason),
        div(
          class = "inspect-desc-note",
          "The value was kept verbatim; the typed column holds NA."
        )
      )
      expected_ui <- div(
        class = "inspect-should",
        if (nzchar(dtype)) {
          paste0("a value representable as declared type ", dtype)
        } else {
          "a value representable in the column's declared type"
        }
      )
      loc <- paste0(
        if (nzchar(col)) paste0("column ", col) else "",
        if (nzchar(arow)) paste0(if (nzchar(col)) ", " else "", "row ", arow) else ""
      )
      actual_ui <- tagList(
        div(class = "inspect-actual-val", if (nzchar(raw)) raw else "(empty)"),
        if (nzchar(loc)) div(class = "inspect-actual-loc", loc)
      )
      actual_title <- "Raw value that could not be imported"
    } else {
      col <- .first_nonempty(r[["columnspec_column"]], r[["column"]])
      kw <- .first_nonempty(r[["columnspec_keyword"]], r[["keyword"]])
      smsg <- .first_nonempty(r[["columnspec_message"]], msg)
      badge <- tags$span(class = "inspect-badge columnspec", "Column spec violation")
      desc <- div(
        class = "inspect-desc",
        div(
          class = "inspect-desc-main",
          tags$strong(if (nzchar(col)) col else "(column)"),
          if (nzchar(kw)) tags$span(class = "inspect-desc-type", kw)
        ),
        if (nzchar(smsg)) div(class = "inspect-desc-detail", smsg)
      )
      expected_ui <- div(class = "inspect-should", columnspec_expected_text(r))
      # An undeclared column carries no offending VALUE -- its presence is the
      # whole finding -- so `columnspec_data` is NA and the fallbacks below find
      # nothing either. Rendered unguarded that reads "(empty)" for a column
      # that is emphatically present, which is exactly backwards. Name what was
      # actually found instead, as .report_columnspec_actual_text() does for the
      # HTML report.
      aval <- if (identical(kw, "additionalProperties")) {
        "(an undeclared column)"
      } else {
        .first_nonempty(
          r[["columnspec_data"]],
          if (nzchar(col)) r[[paste0("context_", col)]] else NULL
        )
      }
      arow <- .first_nonempty(r[["columnspec_row"]], r[["context_.row"]])
      loc <- paste0(
        if (nzchar(col)) paste0("column ", col) else "",
        if (nzchar(arow)) paste0(if (nzchar(col)) ", " else "", "row ", arow) else ""
      )
      actual_ui <- tagList(
        div(class = "inspect-actual-val", if (nzchar(aval)) aval else "(empty)"),
        if (nzchar(loc)) div(class = "inspect-actual-loc", loc)
      )
      actual_title <- "Actual value"
    }

    full <- if (nrow(d) == 1) df_to_kv(d) else df_to_html_table(d)

    tagList(
      div(
        class = "inspect-summary",
        div(class = "inspect-summary-head", badge),
        div(class = "inspect-msg", msg),
        desc
      ),
      div(
        class = "inspect-cmp",
        div(
          class = "inspect-box inspect-expected",
          div(class = "inspect-box-title", HTML("&#x2714; Should be")),
          div(class = "inspect-box-body", expected_ui)
        ),
        div(
          class = "inspect-box inspect-actual",
          div(class = "inspect-box-title", HTML(paste0("&#x2716; ", actual_title))),
          div(class = "inspect-box-body", actual_ui)
        )
      ),
      tags$details(
        class = "inspect-details",
        tags$summary("Full technical detail"),
        div(class = "dta-inspect-wrap", full)
      )
    )
  }

  show_inspect_modal <- function(dataset, id) {
    res <- dta_inspect(rv$dta, dataset, id)
    if (!res$ok || is.null(res$value) || nrow(res$value) == 0) {
      showModal(modalDialog(
        title = paste("Message", id),
        tags$p("No detailed inspect report is available for this message."),
        if (!is.null(res$error)) tags$pre(res$error),
        easyClose = TRUE, footer = modalButton("Close")
      ))
      return()
    }
    showModal(modalDialog(
      title = paste("Inspect \u2014 message", id),
      size = "xl", easyClose = TRUE, footer = modalButton("Close"),
      div(class = "inspect-modal-body", render_inspect_body(res$value, dataset))
    ))
  }

  observeEvent(input$msgs_rows_selected, {
    sel <- input$msgs_rows_selected
    m <- msgs_r()
    if (is.null(sel) || is.null(m) || nrow(m) == 0) {
      return()
    }
    id <- if ("id" %in% names(m)) m$id[sel] else sel
    show_inspect_modal(rv$active, id)
  })

  # --- metadata editing ---------------------------------------------------
  output$metadata_editor <- renderUI({
    rv$md_token
    dta <- isolate(rv$dta)
    req(dta)
    md <- DTAtools::metadata(dta)
    getf <- function(field) {
      v <- tryCatch(S7::prop(md, field), error = function(e) NULL)
      if (is.null(v) || length(v) == 0) "" else as.character(v)[1]
    }
    # Metadata import errors are DTA-level (target == "metadata"), so the
    # per-dataset messages dock can never show them. The metadata editor is the
    # one place the affected fields are on screen, so the notice goes here.
    md_import <- dta_metadata_import_messages(dta)
    md_import_ui <- if (is.data.frame(md_import) && nrow(md_import) > 0) {
      div(
        class = "md-import-warn",
        div(
          class = "md-import-warn-head",
          sprintf(
            "%d metadata value%s could not be imported in the declared type",
            nrow(md_import),
            if (nrow(md_import) == 1) "" else "s"
          )
        ),
        tags$ul(lapply(seq_len(nrow(md_import)), function(i) {
          tags$li(as.character(md_import$message[i]))
        }))
      )
    } else {
      NULL
    }
    # Read-only, machine-recorded "where this document came from" block --
    # see template_provenance_block() (ui_components.R) for why it can never
    # be an editable field. Shown only when a template actually created this
    # document; a hand-authored or legacy DTA carries no @template at all.
    provenance_ui <- template_provenance_block(tryCatch(S7::prop(md, "template"), error = function(e) NULL))
    date_val <- tryCatch(S7::prop(md, "date"), error = function(e) NULL)
    tr <- dta_transmission(dta)
    trf <- function(k) {
      v <- tr[[k]]
      if (is.null(v)) "" else if (inherits(v, "Date")) format(v, "%Y-%m-%d") else as.character(v)[1]
    }
    # Read-only renders static text instead of form controls. Only the LEAF
    # controls swap; every div, layout_columns, card and section title below is
    # shared, so the tab keeps its shape when the switch flips and there is one
    # layout to maintain rather than two.
    #
    # editing() is deliberately NOT isolated -- this render has to re-run when
    # the switch changes. rv$dta stays isolated, as before, so typing in a field
    # still does not rebuild the form under the cursor.
    ro <- !editing()
    f_text <- function(id, label, value, ...) {
      if (ro) meta_field_text(label, value) else textInput(id, label, value = value, ...)
    }
    f_area <- function(id, label, value, ...) {
      if (ro) meta_field_text(label, value) else textAreaInput(id, label, value = value, ...)
    }
    tagList(
      md_import_ui,
      div(class = "md-section-title", "Document"),
      layout_columns(
        col_widths = c(6, 6),
        f_text("md_title", "Title", getf("title"), width = "100%"),
        f_text("md_version", "Version", getf("version"), width = "100%")
      ),
      layout_columns(
        col_widths = c(6, 6),
        # Optional date: a native dateInput shows TODAY when value = NULL, so to
        # render an EMPTY picker for an unset date we pass NA (-> empty
        # data-initial-date). suppressWarnings() hides the NA->date coercion note.
        if (ro) {
          meta_field_text(
            "Date",
            if (inherits(date_val, "Date") && !is.na(date_val)) {
              format(date_val, "%Y-%m-%d")
            } else {
              ""
            }
          )
        } else {
          suppressWarnings(dateInput(
            "md_date", "Date",
            value = if (inherits(date_val, "Date") && !is.na(date_val)) date_val else NA,
            width = "100%"
          ))
        },
        f_text("md_header", "Header / organization", getf("header"), width = "100%")
      ),
      tags$hr(),
      div(class = "md-section-title", "Parties"),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Receiver"),
          card_body(
            div(class = "section-label", "Affiliation"),
            uiOutput("receiver_affiliation"),
            tags$hr(),
            div(
              class = "section-label",
              style = "display:flex; justify-content:space-between; align-items:center; gap:8px;",
              tags$span("Contacts"),
              if (!ro) actionButton("add_receiver", "Add person", class = "btn btn-sm btn-outline-primary")
            ),
            uiOutput("receiver_contacts")
          )
        ),
        card(
          card_header("Supplier"),
          card_body(
            div(class = "section-label", "Affiliation"),
            uiOutput("supplier_affiliation"),
            tags$hr(),
            div(
              class = "section-label",
              style = "display:flex; justify-content:space-between; align-items:center; gap:8px;",
              tags$span("Contacts"),
              if (!ro) actionButton("add_supplier", "Add person", class = "btn btn-sm btn-outline-primary")
            ),
            uiOutput("supplier_contacts")
          )
        )
      ),
      tags$hr(),
      div(class = "md-section-title", "Transmission"),
      layout_columns(
        col_widths = c(4, 4, 4),
        f_text("tr_type", "Type",
          trf("type"),
          width = "100%",
          placeholder = "e.g. secure S3 bucket"
        ),
        f_text("tr_frequency", "Frequency",
          trf("frequency"),
          width = "100%",
          placeholder = "e.g. one-time, weekly"
        ),
        f_text("tr_notification", "Notification",
          trf("notification"),
          width = "100%",
          placeholder = "e.g. email"
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        f_text("tr_date_first", "Date of first transfer",
          trf("date_first_transfer"),
          width = "100%", placeholder = "YYYY-MM-DD or phrase"
        ),
        f_text("tr_date_last", "Date of last transfer",
          trf("date_last_transfer"),
          width = "100%", placeholder = "YYYY-MM-DD or phrase"
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        # Read-only shows the CHOICE ("undefined" / "yes" / "no"), which is
        # exactly what dta_flag_to_choice() already returns, rather than the
        # underlying flag.
        if (ro) {
          meta_field_text("Test upload", dta_flag_to_choice(tr$test_upload))
        } else {
          selectInput("tr_test_upload", "Test upload",
            choices = c("undefined", "yes", "no"),
            selected = dta_flag_to_choice(tr$test_upload), width = "100%"
          )
        },
        if (ro) {
          meta_field_text("Blinded transfer", dta_flag_to_choice(tr$blinded_transfer))
        } else {
          selectInput("tr_blinded", "Blinded transfer",
            choices = c("undefined", "yes", "no"),
            selected = dta_flag_to_choice(tr$blinded_transfer), width = "100%"
          )
        }
      ),
      tags$hr(),
      div(class = "md-section-title", "Error handling & corrections"),
      f_area("md_error_handling", "Error handling",
        getf("error_handling"),
        width = "100%", rows = 2,
        placeholder = "How data/format errors are handled and communicated."
      ),
      f_text("md_authorized", "Authorized for corrections",
        getf("authorized_for_corrections"),
        width = "100%",
        placeholder = "Contact(s) authorized to request corrections"
      ),
      # Only true while editing; in read-only there is nothing to save.
      if (!ro) {
        div(
          class = "msg-hint",
          "Changes are saved automatically to the current session as you type."
        )
      },
      provenance_ui
    )
  })

  # per-field debounced saves (incremental, non-destructive)
  #
  # req(editing()) here is the server half of the read-only gate. The controls
  # not being rendered is not sufficient on its own: these fields save through a
  # 700ms debounce, so a value typed just before the switch is flipped off still
  # flushes afterwards, and an input driven straight over the websocket arrives
  # whatever is on screen. Both write into the document without this.
  #
  # (An input going NULL when its control stops rendering is already harmless --
  # observeEvent() defaults to ignoreNULL = TRUE, so the debounced observers
  # below never fire on it. The guard is about non-NULL values arriving while
  # the document is meant to be read-only.)
  save_md <- function(field, value) {
    req(editing())
    req(rv$dta)
    r <- dta_set_metadata_field(rv$dta, field, value)
    if (r$ok) {
      rv$dta <- r$value
      # Keep the raw YAML text (and autosave) in sync so the edit is not
      # reverted when the user later applies the Raw YAML document.
      sync_yaml_text()
    } else {
      showNotification(paste("Could not update", field, "\u2014", r$error),
        type = "error"
      )
    }
  }
  save_tr <- function(field, value) {
    req(editing()) # same reasoning as save_md() above
    req(rv$dta)
    r <- dta_set_transmission_field(rv$dta, field, value)
    if (r$ok) {
      rv$dta <- r$value
      sync_yaml_text()
    } else {
      showNotification(paste("Could not update transmission", field, "\u2014", r$error),
        type = "error"
      )
    }
  }
  title_d <- debounce(reactive(input$md_title), 700)
  version_d <- debounce(reactive(input$md_version), 700)
  header_d <- debounce(reactive(input$md_header), 700)
  errh_d <- debounce(reactive(input$md_error_handling), 700)
  auth_d <- debounce(reactive(input$md_authorized), 700)
  # title and version are REQUIRED (non-nullable); block saves of empty/blank values
  observeEvent(title_d(),
    {
      v <- title_d()
      is_blank <- is.null(v) || length(v) == 0 || !nzchar(trimws(as.character(v)[1]))
      if (!is_blank) save_md("title", v)
    },
    ignoreInit = TRUE
  )
  observeEvent(version_d(),
    {
      v <- version_d()
      is_blank <- is.null(v) || length(v) == 0 || !nzchar(trimws(as.character(v)[1]))
      if (!is_blank) save_md("version", v)
    },
    ignoreInit = TRUE
  )
  observeEvent(header_d(), save_md("header", header_d()), ignoreInit = TRUE)
  observeEvent(errh_d(), save_md("error_handling", errh_d()), ignoreInit = TRUE)
  observeEvent(auth_d(), save_md("authorized_for_corrections", auth_d()), ignoreInit = TRUE)
  observeEvent(input$md_date,
    {
      v <- input$md_date
      is_empty <- is.null(v) || length(v) == 0 ||
        (length(v) == 1 && is.na(v)) ||
        !nzchar(trimws(as.character(v)[1]))
      save_md("date", if (is_empty) NULL else v)
    },
    ignoreInit = TRUE,
    ignoreNULL = FALSE
  )
  # transmission fields (debounced text + immediate flags)
  tr_type_d <- debounce(reactive(input$tr_type), 700)
  tr_freq_d <- debounce(reactive(input$tr_frequency), 700)
  tr_notif_d <- debounce(reactive(input$tr_notification), 700)
  tr_first_d <- debounce(reactive(input$tr_date_first), 700)
  tr_last_d <- debounce(reactive(input$tr_date_last), 700)
  observeEvent(tr_type_d(), save_tr("type", tr_type_d()), ignoreInit = TRUE)
  observeEvent(tr_freq_d(), save_tr("frequency", tr_freq_d()), ignoreInit = TRUE)
  observeEvent(tr_notif_d(), save_tr("notification", tr_notif_d()), ignoreInit = TRUE)
  observeEvent(tr_first_d(), save_tr("date_first_transfer", tr_first_d()), ignoreInit = TRUE)
  observeEvent(tr_last_d(), save_tr("date_last_transfer", tr_last_d()), ignoreInit = TRUE)
  observeEvent(input$tr_test_upload, save_tr("test_upload", dta_choice_to_flag(input$tr_test_upload)), ignoreInit = TRUE)
  observeEvent(input$tr_blinded, save_tr("blinded_transfer", dta_choice_to_flag(input$tr_blinded)), ignoreInit = TRUE)

  # --- people / contacts --------------------------------------------------
  # Shared field set so the Add and Edit person modals capture the SAME details
  # (name, roles, email, department, phone, address). `prefix` namespaces the
  # input ids ("new_contact" for add, "edit_contact" for edit); `p` pre-fills.
  contact_modal_inputs <- function(prefix, p = list()) {
    g <- function(k) p[[k]] %||% ""
    tagList(
      textInput(paste0(prefix, "_name"), "Name", value = g("name")),
      textInput(paste0(prefix, "_roles"), "Role(s)",
        value = g("role"),
        placeholder = "e.g. Data Manager, Reviewer"
      ),
      textInput(paste0(prefix, "_email"), "Email", value = g("email")),
      layout_columns(
        col_widths = c(6, 6),
        textInput(paste0(prefix, "_department"), "Department", value = g("department")),
        textInput(paste0(prefix, "_phone"), "Phone", value = g("phone"))
      ),
      textInput(paste0(prefix, "_address"), "Address", value = g("address"))
    )
  }

  render_contacts <- function(side) {
    cs <- dta_contacts(isolate(rv$dta), side)
    ro <- !editing()
    if (length(cs) == 0) {
      # Read-only has no "Add person" button to point at, so the empty state
      # must not tell the user to click one.
      return(div(class = "msg-hint", if (ro) {
        "No contacts."
      } else {
        "No contacts yet. Click \u201cAdd person\u201d to create one."
      }))
    }
    tags$ul(
      class = "list-group",
      lapply(seq_along(cs), function(i) {
        if (ro) {
          # Full detail, not just contact_display()'s "name — role": editable
          # mode hides the rest of a contact's fields behind a click
          # (actionLink() below), but read-only has no click at all, so
          # whatever contact_detail_block() does not show here is simply
          # unreachable. "contact-item" is deliberately NOT applied to this
          # <li> -- that class carries the pointer cursor/hover highlight
          # that signal "click me", and this row no longer does anything.
          return(tags$li(
            class = "list-group-item",
            contact_detail_block(cs[[i]])
          ))
        }
        tags$li(
          class = "list-group-item contact-item d-flex justify-content-between align-items-center",
          actionLink(
            paste0("editc_", side, "_", i),
            class = "flex-grow-1 text-decoration-none",
            title = "Click to edit this person",
            label = tagList(
              span(contact_display(cs[[i]])),
              span(class = "contact-edit-ic", "\u270E edit")
            )
          ),
          actionButton(
            paste0("rm_", side, "_", i), "Remove",
            class = "btn btn-sm btn-outline-danger"
          )
        )
      })
    )
  }

  # Open a pre-filled modal to edit one contact's details.
  edit_contact_flow <- function(side, index) {
    req(editing())
    p <- dta_contact_at(isolate(rv$dta), side, index)
    if (is.null(p)) {
      return()
    }
    rv$editing_contact <- list(side = side, index = index)
    showModal(modalDialog(
      title = paste("Edit", side, "contact"),
      contact_modal_inputs("edit_contact", p),
      div(
        class = "msg-hint",
        "Separate multiple roles with commas. Other fields on this person (e.g. signature flags) are preserved."
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_edit_contact", "Save", class = "btn btn-primary")
      ),
      easyClose = TRUE
    ))
  }

  observeEvent(input$confirm_edit_contact, {
    req(editing())
    ec <- rv$editing_contact
    req(ec)
    nm <- trimws(input$edit_contact_name %||% "")
    if (!nzchar(nm)) {
      showNotification("A name is required.", type = "warning")
      return()
    }
    roles <- paste(trimws(strsplit(input$edit_contact_roles %||% "", ",")[[1]]), collapse = ", ")
    fields <- list(
      name = nm,
      role = roles,
      email = input$edit_contact_email %||% "",
      department = input$edit_contact_department %||% "",
      phone = input$edit_contact_phone %||% "",
      address = input$edit_contact_address %||% ""
    )
    r <- dta_update_contact(rv$dta, ec$side, ec$index, fields)
    if (r$ok) {
      rv$dta <- r$value
      rv$editing_contact <- NULL
      rv$contacts_token <- rv$contacts_token + 1
      sync_yaml_text()
      removeModal()
    } else {
      showNotification(r$error, type = "error")
    }
  })

  output$receiver_contacts <- renderUI({
    rv$contacts_token
    req(isolate(rv$dta))
    render_contacts("receiver")
  })
  output$supplier_contacts <- renderUI({
    rv$contacts_token
    req(isolate(rv$dta))
    render_contacts("supplier")
  })

  # --- affiliation (side-level: receiver / supplier) ----------------------
  render_affiliation <- function(side) {
    aff <- dta_affiliation(isolate(rv$dta), side)
    g <- function(k) aff[[k]] %||% ""
    # Same leaf-swap rule as the metadata form above: the layout is shared, only
    # the controls differ. Reading editing() here is also what gives the two
    # renderUI wrappers below their dependency on the switch.
    ro <- !editing()
    ff <- function(id, label, value, ...) {
      if (ro) meta_field_text(label, value) else textInput(id, label, value = value, ...)
    }
    tagList(
      ff(paste0(side, "_aff_name"), "Organization",
        g("name"),
        width = "100%",
        placeholder = "e.g. Test Company"
      ),
      layout_columns(
        col_widths = c(6, 6),
        ff(paste0(side, "_aff_country"), "Country", g("country"), width = "100%"),
        ff(paste0(side, "_aff_address"), "Address", g("address"), width = "100%")
      )
    )
  }
  output$receiver_affiliation <- renderUI({
    rv$md_token
    req(isolate(rv$dta))
    render_affiliation("receiver")
  })
  output$supplier_affiliation <- renderUI({
    rv$md_token
    req(isolate(rv$dta))
    render_affiliation("supplier")
  })

  save_affiliation <- function(side, field, value) {
    req(editing()) # same reasoning as save_md() above
    req(rv$dta)
    kv <- list()
    kv[[field]] <- value %||% ""
    r <- do.call(dta_set_affiliation, c(list(rv$dta, side), kv))
    if (r$ok) {
      rv$dta <- r$value
      sync_yaml_text()
    } else {
      showNotification(paste("Could not update affiliation \u2014", r$error), type = "error")
    }
  }
  for (.side in c("receiver", "supplier")) {
    for (.field in c("name", "country", "address")) {
      local({
        SIDE <- .side
        FIELD <- .field
        inid <- paste0(SIDE, "_aff_", FIELD)
        d <- debounce(reactive(input[[inid]]), 700)
        observeEvent(d(), save_affiliation(SIDE, FIELD, d()), ignoreInit = TRUE)
      })
    }
  }

  # dynamic remove observers for contacts
  contact_rm_registry <- new.env(parent = emptyenv())
  observe({
    rv$contacts_token
    req(isolate(rv$dta))
    for (side in c("receiver", "supplier")) {
      cs <- dta_contacts(isolate(rv$dta), side)
      for (i in seq_along(cs)) {
        edid <- paste0("editc_", side, "_", i)
        if (is.null(contact_rm_registry[[edid]])) {
          contact_rm_registry[[edid]] <- TRUE
          local({
            SIDE <- side
            IDX <- i
            EID <- edid
            observeEvent(input[[EID]], edit_contact_flow(SIDE, IDX), ignoreInit = TRUE)
          })
        }
        rmid <- paste0("rm_", side, "_", i)
        if (is.null(contact_rm_registry[[rmid]])) {
          contact_rm_registry[[rmid]] <- TRUE
          local({
            SIDE <- side
            IDX <- i
            ID <- rmid
            observeEvent(input[[ID]],
              {
                showModal(modalDialog(
                  title = "Remove contact?",
                  sprintf("Remove this %s contact?", SIDE),
                  footer = tagList(
                    modalButton("Cancel"),
                    actionButton(paste0("confirm_", ID), "Remove", class = "btn btn-danger")
                  ),
                  easyClose = TRUE
                ))
              },
              ignoreInit = TRUE
            )
            observeEvent(input[[paste0("confirm_", ID)]],
              {
                req(editing())
                r <- dta_remove_contact(rv$dta, SIDE, IDX)
                if (r$ok) {
                  rv$dta <- r$value
                  rv$contacts_token <- rv$contacts_token + 1
                  sync_yaml_text()
                } else {
                  showNotification(r$error, type = "error")
                }
                removeModal()
              },
              ignoreInit = TRUE
            )
          })
        }
      }
    }
  })

  add_contact_flow <- function(side) {
    showModal(modalDialog(
      title = paste("Add", side, "contact"),
      contact_modal_inputs("new_contact"),
      div(
        class = "msg-hint",
        "Separate multiple roles with commas. Affiliation is set once per side (above), not per person."
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(paste0("confirm_add_", side), "Add", class = "btn btn-primary")
      ),
      easyClose = TRUE
    ))
  }
  observeEvent(input$add_receiver, add_contact_flow("receiver"))
  observeEvent(input$add_supplier, add_contact_flow("supplier"))

  confirm_add <- function(side) {
    req(editing())
    nm <- trimws(input$new_contact_name %||% "")
    if (!nzchar(nm)) {
      showNotification("A name is required.", type = "warning")
      return()
    }
    roles <- trimws(strsplit(input$new_contact_roles %||% "", ",")[[1]])
    r <- dta_add_contact(
      rv$dta, side,
      name = nm, roles = roles,
      email = input$new_contact_email %||% "",
      department = input$new_contact_department %||% "",
      phone = input$new_contact_phone %||% "",
      address = input$new_contact_address %||% ""
    )
    if (r$ok) {
      rv$dta <- r$value
      rv$contacts_token <- rv$contacts_token + 1
      sync_yaml_text()
      removeModal()
    } else {
      showNotification(r$error, type = "error")
    }
  }
  observeEvent(input$confirm_add_receiver, confirm_add("receiver"))
  observeEvent(input$confirm_add_supplier, confirm_add("supplier"))

  # --- raw YAML: editable, validated as YAML AND as a DTA/DTADataSet ------
  output$yaml_validation_msg <- renderUI({
    m <- rv$yaml_msg
    if (is.null(m)) {
      return(NULL)
    }
    if (isTRUE(m$ok)) {
      div(
        class = "yaml-valid ok", HTML("&#x2714;"),
        " Valid — document applied."
      )
    } else {
      div(class = "yaml-valid err", HTML("&#x2716;"), " ", m$error)
    }
  })

  # Push text into the Raw YAML editor (Ace when available, else textarea).
  set_yaml_editor <- function(text) {
    if (requireNamespace("shinyAce", quietly = TRUE)) {
      shinyAce::updateAceEditor(session, "raw_yaml_editor", value = text %||% "")
    } else {
      updateTextAreaInput(session, "raw_yaml_editor", value = text %||% "")
    }
  }

  observeEvent(input$revert_yaml, {
    set_yaml_editor(rv$yaml_text %||% "")
    rv$yaml_msg <- NULL
  })

  # The Raw YAML tab is a full-document write path, so it is gated like every
  # other editing surface: the buttons are not rendered, the Ace editor is put
  # into readOnly, and the apply observer re-checks.
  output$yaml_edit_hint <- renderUI({
    if (!editing()) {
      return(div(
        class = "msg-hint",
        HTML("This document is <b>read-only</b>. Turn on <b>Edit mode</b> (top right) to change it.")
      ))
    }
    div(
      class = "msg-hint",
      HTML("Edit the document and click <b>Apply changes</b>. It is validated as YAML <i>and</i> as a full DTA / DTADataSet before it replaces the loaded document — on any error nothing changes and the reason is shown below. Loaded files are kept, including when you edit <code>files:</code>, as long as the dataset still has a slot to show them under; files belonging to a slot you deleted are unloaded with it. A dataset's validation is cleared whenever its files, columns or rules changed, and deleted datasets drop everything.")
    )
  })

  output$yaml_edit_actions <- renderUI({
    if (!editing()) {
      return(NULL)
    }
    div(
      class = "yaml-edit-actions",
      actionButton("apply_yaml", "Apply changes", class = "btn btn-sm btn-primary"),
      actionButton("revert_yaml", "Revert", class = "btn btn-sm btn-outline-secondary")
    )
  })

  observe({
    ro <- !editing()
    if (requireNamespace("shinyAce", quietly = TRUE)) {
      shinyAce::updateAceEditor(session, "raw_yaml_editor", readOnly = ro)
    } else if (ro) {
      shinyjs::disable("raw_yaml_editor")
    } else {
      shinyjs::enable("raw_yaml_editor")
    }
  })

  observeEvent(input$apply_yaml, {
    req(editing())
    txt <- input$raw_yaml_editor %||% ""
    if (!nzchar(trimws(txt))) {
      rv$yaml_msg <- list(ok = FALSE, error = "The editor is empty — nothing to apply.")
      return()
    }
    res <- dta_read_yaml_text(txt)
    if (!isTRUE(res$ok)) {
      rv$yaml_msg <- list(ok = FALSE, error = res$error %||% "Invalid YAML or DTA.")
      showNotification(paste("YAML not applied:", res$error), type = "error", duration = 10)
      return()
    }
    # Smart reconcile: keep uploads for datasets whose file handlers are
    # unchanged, keep validation only when columns/rules are also unchanged,
    # and drop uploads for datasets that were removed from the document.
    new_dta <- res$value
    old_dta <- isolate(rv$dta)
    old_uploads <- isolate(rv$uploads)
    old_status <- isolate(rv$status)
    prev_active <- isolate(rv$active)
    new_names <- dta_dataset_names(new_dta)
    old_names <- if (is.null(old_dta)) character(0) else dta_dataset_names(old_dta)

    new_uploads <- list()
    new_status <- stats::setNames(rep("pending", length(new_names)), new_names)
    for (nm in new_names) {
      if (!(nm %in% old_names)) next # brand-new dataset -> fresh, pending
      old_ds <- dta_get_dataset(old_dta, nm)
      new_ds <- dta_get_dataset(new_dta, nm)
      handlers_same <- identical(
        dta_handlers_signature(old_ds),
        dta_handlers_signature(new_ds)
      )
      specs_same <- identical(
        dta_specs_signature(old_ds),
        dta_specs_signature(new_ds)
      )
      if (dta_dataset_content_count(old_ds) == 0) next # nothing to carry over

      # A dataset that still declares at least one file handler keeps its bound
      # data even when the handlers changed -- the same bargain the file editor
      # offers, so editing `files:` here is not silently more destructive than
      # editing it in the Edit-files dialog. Validation survives only when the
      # columns and rules are untouched.
      n_new_handlers <- length(dta_handlers(new_ds))
      if (!handlers_same && n_new_handlers == 0) next # no slot left to show data in

      tr <- dta_transfer_bound_data(
        new_dta, nm, old_ds,
        keep_validation = specs_same && handlers_same
      )
      if (!isTRUE(tr$ok)) next
      new_dta <- tr$value

      # Upload records are keyed by handler POSITION, and a re-parsed document
      # may have reordered or replaced entries as easily as appended one. Match
      # the old handlers to the new ones by identity and move each record to
      # where ITS handler went; a record whose handler is gone has no slot left
      # to appear under, so that file is unloaded rather than left bound to the
      # dataset and unreachable.
      hmap <- dta_match_handlers(old_ds, new_ds)
      for (k in names(old_uploads)) {
        if (!startsWith(k, paste0(nm, "||"))) next
        hi <- suppressWarnings(as.integer(sub(".*\\|\\|", "", k)))
        target <- if (!is.na(hi) && hi >= 1 && hi <= length(hmap)) hmap[[hi]] else NA_integer_
        if (!is.na(target)) {
          new_uploads[[paste0(nm, "||", target)]] <- old_uploads[[k]]
          next
        }
        for (rec in (old_uploads[[k]] %||% list())) {
          uv <- dta_unload_table(new_dta, nm, rec$table)
          if (isTRUE(uv$ok)) new_dta <- uv$value
        }
      }
      new_status[[nm]] <- if (specs_same && handlers_same) {
        dta_lookup(old_status, nm, "pending")
      } else {
        "pending"
      }
    }

    # The version record is owned by the new-version flow, not by whatever text
    # happens to be in the editor: applying pasted YAML is an edit to the
    # specification, not a replacement of the document's identity. Both properties
    # are carried over from the live document so a paste cannot silently roll the
    # version back or drop the history the author is currently writing into.
    if (!is.null(old_dta)) {
      keep_md <- tryCatch(DTAtools::metadata(old_dta), error = function(e) NULL)
      new_md <- tryCatch(DTAtools::metadata(new_dta), error = function(e) NULL)
      if (!is.null(keep_md) && !is.null(new_md)) {
        S7::prop(new_md, "version") <- S7::prop(keep_md, "version")
        S7::prop(new_md, "version_history") <- S7::prop(keep_md, "version_history")
        new_dta@metadata <- new_md
      }
    }
    rv$dta <- new_dta
    rv$structure <- build_structure(new_dta)
    rv$uploads <- new_uploads
    # A re-parsed document can move any dataset's handlers, so the same
    # position-keyed state the Edit-files dialog resets has to be reset here:
    # stale per-file ids, and file inputs still displaying the name dropped on
    # whatever handler used to occupy that position.
    purge_file_ids()
    for (nm in new_names) reset_dataset_fileinputs(nm)
    for (nm in new_names) {
      if (dta_dataset_content_count(dta_get_dataset(new_dta, nm)) == 0 &&
        identical(dta_lookup(new_status, nm, "pending"), "pending")) {
        new_status[[nm]] <- "nodata"
      }
    }
    rv$status <- new_status
    rv$dataset_only <- isTRUE(res$dataset_only)
    rv$active <- if (!is.null(prev_active) && prev_active %in% new_names) {
      prev_active
    } else if (length(new_names) > 0) {
      new_names[1]
    } else {
      NULL
    }
    rv$md_token <- rv$md_token + 1
    rv$contacts_token <- rv$contacts_token + 1
    # The version-preservation step above can have changed metadata that the
    # pasted `txt` does not carry (a rolled-back version, a stripped
    # version_history), so the Raw tab must be re-serialised from the document
    # rather than simply echoing what was pasted -- sync_yaml_text() reads
    # rv$dta via isolate(), which is why this has to run down here, after
    # rv$dta and rv$dataset_only are both already up to date. It also pushes
    # the editor and autosaves, replacing the plain autosave() this handler
    # used to end on.
    sync_yaml_text()
    rv$yaml_msg <- list(ok = TRUE, error = NULL)
    showNotification(
      if (isTRUE(res$wrapped_dataset)) {
        "Dataset YAML wrapped into a new DTA (loaded files kept where a slot remains for them)."
      } else {
        "DTA YAML applied (loaded files kept where a slot remains for them)."
      },
      type = "message"
    )
  })

  # --- export -------------------------------------------------------------
  # helper: write a UTF-8 YAML string to a download file (binary-safe)
  write_yaml_download <- function(text, file) {
    con <- file(file, "wb")
    on.exit(close(con))
    writeLines(enc2utf8(text), con, useBytes = TRUE)
  }
  yaml_export_stub <- function() {
    md <- tryCatch(DTAtools::metadata(rv$dta), error = function(e) NULL)
    ttl <- tryCatch(as.character(S7::prop(md, "title"))[1], error = function(e) NULL)
    base <- if (!is.null(ttl) && nzchar(ttl)) gsub("[^A-Za-z0-9]+", "_", ttl) else "DTA"
    base
  }

  # The document as it should be WRITTEN OUT: the live document, with the open
  # version-history entry's `changes` replaced by a summary of everything that
  # differs between the document as loaded and the document as it now stands.
  #
  # A pure read, deliberately: it returns a modified copy and never assigns to
  # rv$dta. Writing reactive state from inside a downloadHandler's content
  # function would re-render the Metadata tab and the Raw YAML editor in the
  # middle of a download for no benefit.
  #
  # Every failure path returns the live document untouched. A summary that cannot
  # be computed -- an unparseable baseline, a diff that throws -- must never be
  # the reason an export fails.
  export_dta <- function() {
    dta <- isolate(rv$dta)
    if (is.null(dta)) {
      return(dta)
    }
    # The whole body of this function is dta_version_finalise() -- it lives in
    # versioning.R because the second-version-bump path in new_version_confirm
    # has to close an open entry too, and two copies of "diff against the
    # baseline and write the summary" would drift apart the first time either
    # was hardened without the other. It already returns the document
    # untouched for a NULL index, a missing or unparseable baseline, or a diff
    # that throws, which is exactly this function's own contract.
    dta_version_finalise(
      dta,
      isolate(rv$version_entry_index),
      isolate(rv$version_baseline_yaml),
      note = isolate(rv$version_note) %||% ""
    )
  }

  output$dl_yaml <- downloadHandler(
    filename = function() paste0(yaml_export_stub(), "_", Sys.Date(), ".yaml"),
    content = function(file) {
      req(rv$dta)
      res <- dta_to_yaml_text(export_dta())
      if (!res$ok) {
        showNotification(paste("YAML export failed:", res$error), type = "error", duration = 10)
        stop(res$error)
      }
      write_yaml_download(res$value, file)
    }
  )
  output$dl_ds_yaml <- downloadHandler(
    filename = function() {
      nm <- rv$active %||% "dataset"
      paste0(gsub("[^A-Za-z0-9]+", "_", nm), "_", Sys.Date(), ".yaml")
    },
    content = function(file) {
      req(rv$dta, rv$active)
      res <- dta_dataset_to_yaml_text(rv$dta, rv$active)
      if (!res$ok) {
        showNotification(paste("Dataset YAML export failed:", res$error),
          type = "error", duration = 10
        )
        stop(res$error)
      }
      write_yaml_download(res$value, file)
    }
  )
  output$dl_docx <- downloadHandler(
    filename = function() paste0("DTA_", Sys.Date(), ".docx"),
    content = function(file) {
      req(rv$dta)
      res <- dta_export(export_dta(), file, "docx")
      if (!res$ok) {
        showNotification(paste("Word export failed:", res$error), type = "error", duration = 10)
        stop(res$error)
      }
    }
  )

  # --- export modal --------------------------------------------------------
  # Reactive values to store export options and file path
  export_state <- reactiveValues(
    file_path = NULL,
    file_name = NULL
  )

  # Open export modal
  observeEvent(input$export_modal_open, {
    req(rv$dta)
    templates <- list_available_templates()

    modal_content <- div(
      # No heading here: modalDialog() renders `title` as the dialog's own
      # header, so an h4 repeating those words printed the dialog name twice,
      # one line above the other. Every other modal here relies on `title`
      # alone.
      p("Choose the format and options for your DTA export."),
      h5("Format", class = "text-muted"),
      radioButtons("export_format", NULL,
        choices = c("Markdown" = "markdown", "Word Document" = "word"),
        selected = "word"
      ),
      # Markdown options
      conditionalPanel(
        condition = "input.export_format == 'markdown'",
        hr(),
        h5("Markdown Options", class = "text-muted"),
        checkboxInput("export_as_pdf", "Export as PDF (via Pandoc)", value = FALSE),
        checkboxInput("export_include_yaml_md", "Include YAML in document (hidden at end)", value = FALSE)
      ),
      # Word options
      conditionalPanel(
        condition = "input.export_format == 'word'",
        hr(),
        h5("Word Document Options", class = "text-muted"),
        radioButtons("export_word_mode", NULL,
          choices = c("Use built-in template" = "builtin", "Use custom template" = "custom"),
          selected = "builtin"
        ),
        conditionalPanel(
          condition = "input.export_word_mode == 'custom'",
          if (length(templates) > 0) {
            selectInput("export_template_select",
              "Available templates:",
              choices = stats::setNames(templates, templates),
              selected = templates[1]
            )
          } else {
            p("No custom templates available.", class = "text-muted")
          }
        ),
        checkboxInput("export_include_yaml_word", "Embed YAML specification at end of document", value = TRUE)
      ),
      hr(),
      h5("Output filename", class = "text-muted"),
      textOutput("export_filename_preview")
    )

    showModal(modalDialog(
      modal_content,
      title = "Export Document",
      footer = tagList(
        actionButton("export_cancel", "Cancel", class = "btn btn-outline-secondary"),
        actionButton("export_do", "Export", class = "btn btn-primary")
      ),
      size = "m"
    ))
  })

  # Export filename preview
  output$export_filename_preview <- renderText({
    req(rv$dta)
    ext <- if (input$export_format == "markdown") {
      if (isTRUE(input$export_as_pdf)) ".pdf" else ".md"
    } else {
      ".docx"
    }
    paste0(dta_export_stem(rv$dta), ext)
  })

  # Cancel export
  observeEvent(input$export_cancel, {
    removeModal()
  })

  # Execute export
  observeEvent(input$export_do, {
    req(rv$dta)

    stem <- dta_export_stem(rv$dta)
    # Computed ONCE: export_dta() re-parses the version baseline and runs a
    # full dta_diff(), and the Word-with-custom-template branch below would
    # otherwise pay for that up to five times in a single export.
    doc <- export_dta()

    tryCatch(
      {
        if (input$export_format == "markdown") {
          # Markdown export
          ext <- if (isTRUE(input$export_as_pdf)) ".pdf" else ".md"
          filename <- paste0(stem, ext)
          # The browser fetches the file on a LATER request, so the path it
          # waits on must be unique. Deriving it from title and date meant two
          # untitled exports on the same day shared one path, and whichever
          # session wrote last was the one both downloads received.
          # `filename` still decides what the browser saves it as.
          output_file <- tempfile(pattern = "dta-export-", fileext = ext)

          # write_dta() throws on error (caught below); it does not return $ok.
          DTAtools::write_dta(doc, output_file, format = "md", overwrite = TRUE, quiet = TRUE)

          # Optionally embed YAML
          if (isTRUE(input$export_include_yaml_md)) {
            md_text <- readLines(output_file, warn = FALSE)
            md_text <- paste(md_text, collapse = "\n")
            md_text <- embed_yaml_markdown(md_text, doc)
            writeLines(enc2utf8(md_text), output_file, useBytes = TRUE)
          }

          # Convert to PDF if requested, trying two routes so it works with or
          # without a LaTeX install:
          #   1. pandoc + a real PDF engine (LaTeX/TinyTeX/wkhtmltopdf) - best
          #      typography when it is available.
          #   2. headless Chrome/Edge printing the rendered HTML - needs no
          #      LaTeX and no extra R packages, just an installed browser.
          # If neither route yields a PDF, fall back to delivering the Markdown
          # file so the export still succeeds (and still downloads).
          if (isTRUE(input$export_as_pdf)) {
            pdf_file <- sub("\\.md$", ".pdf", output_file)
            pdf_ok <- FALSE
            has_pandoc <- requireNamespace("rmarkdown", quietly = TRUE) &&
              rmarkdown::pandoc_available()

            # Route 1: pandoc + LaTeX/wkhtmltopdf engine.
            if (has_pandoc && has_pdf_engine()) {
              pdf_ok <- tryCatch(
                {
                  rmarkdown::pandoc_convert(
                    input = normalizePath(output_file),
                    to = "pdf",
                    output = pdf_file
                  )
                  file.exists(pdf_file)
                },
                error = function(e) FALSE
              )
            }

            # Route 2: headless browser (no LaTeX required).
            chrome <- if (!isTRUE(pdf_ok)) find_chrome_binary() else ""
            if (!isTRUE(pdf_ok) && has_pandoc && nzchar(chrome)) {
              pdf_ok <- tryCatch(
                {
                  markdown_to_pdf_via_chrome(output_file, pdf_file, chrome = chrome)
                  file.exists(pdf_file) && file.info(pdf_file)$size > 0
                },
                error = function(e) {
                  showNotification(
                    paste0("Browser-based PDF failed (", conditionMessage(e), ")."),
                    type = "warning",
                    duration = 8
                  )
                  FALSE
                }
              )
            }

            if (isTRUE(pdf_ok) && file.exists(pdf_file)) {
              output_file <- pdf_file
              filename <- sub("\\.md$", ".pdf", filename)
            } else {
              showNotification(
                paste0(
                  "PDF export needs a LaTeX engine or a Chrome/Edge browser, ",
                  "and neither produced a PDF. Exporting Markdown instead."
                ),
                type = "warning",
                duration = 10
              )
            }
          }

          export_state$file_path <- output_file
          export_state$file_name <- filename
        } else {
          # Word export
          filename <- paste0(stem, ".docx")
          # Unique for the same reason as the markdown branch above.
          output_file <- tempfile(pattern = "dta-export-", fileext = ".docx")

          if (input$export_word_mode == "custom") {
            template_name <- input$export_template_select
            template_path <- get_template_path(template_name)
            if (is.null(template_path)) {
              stop("Template file not found. It may have been deleted.")
            }

            # Prepare template variables (dataset/specs content). Always supply
            # {YAML_EMBEDDED} so the placeholder is cleanly filled (or blanked)
            # rather than left as literal text in the document.
            variables <- list(
              "{DATASETS_SUMMARY}" = format_datasets_summary(doc),
              "{DATASETS_DETAIL}" = format_datasets_detail(doc),
              "{YAML_EMBEDDED}" = ""
            )

            # Add YAML if requested
            if (isTRUE(input$export_include_yaml_word)) {
              res_yaml <- dta_to_yaml_text(doc)
              if (isTRUE(res_yaml$ok)) {
                variables[["{YAML_EMBEDDED}"]] <- res_yaml$value
              }
            }

            # export_with_template() throws on error (caught below); no $ok.
            DTAtools::export_with_template(
              doc,
              template = template_path,
              output = output_file,
              variables = variables,
              quiet = TRUE,
              fallback = FALSE
            )
          } else {
            # Built-in template. write_dta() throws on error; no $ok returned.
            # Optionally append the machine-readable YAML as a small-font
            # section at the end of the document.
            yaml_text <- NULL
            if (isTRUE(input$export_include_yaml_word)) {
              res_yaml <- dta_to_yaml_text(doc)
              if (isTRUE(res_yaml$ok)) yaml_text <- res_yaml$value
            }
            DTAtools::write_dta(
              doc, output_file,
              format = "docx", overwrite = TRUE, quiet = TRUE,
              include_yaml = !is.null(yaml_text), yaml_text = yaml_text
            )
          }

          export_state$file_path <- output_file
          export_state$file_name <- filename
        }

        removeModal()
        # Trigger the hidden download button via a NATIVE click (see
        # download_trigger_js). shinyjs::click() does not reliably start a
        # downloadButton's browser download.
        session$sendCustomMessage("dta_trigger_download", "export_trigger_download")
        showNotification("Document exported successfully", type = "message", duration = 5)
      },
      error = function(e) {
        showNotification(
          paste("Export failed:", as.character(e)),
          type = "error",
          duration = 12
        )
      }
    )
  })

  # Hidden download handler
  output$export_trigger_download <- downloadHandler(
    filename = function() {
      export_state$file_name %||% "export.txt"
    },
    content = function(file) {
      if (!is.null(export_state$file_path) && file.exists(export_state$file_path)) {
        file.copy(export_state$file_path, file, overwrite = TRUE)
      }
    }
  )

  # Validation SUMMARY: the whole-DTA outcome, one row per dataset. Deliberately
  # named apart from the "Report" in the Validation messages dock, which is the
  # message-level DTAtools::write_validation_report() output.
  #
  # Offered once at least one dataset has passed and none has failed. It may
  # still report the run as INCOMPLETE -- datasets skipped for missing data are
  # not a pass, and the summary says so rather than certifying one.
  output$validation_report_ui <- renderUI({
    st <- unlist(rv$status)
    validated <- names(st)[st %in% c("pass", "fail")]
    ok <- length(validated) > 0 && all(st[validated] == "pass")
    if (!isTRUE(ok)) {
      return(NULL)
    }
    complete <- length(validated) == length(st)
    tagList(
      tags$hr(),
      downloadButton("dl_validation_summary", "Validation summary",
        class = if (complete) "btn btn-success w-100" else "btn btn-warning w-100",
        title = if (complete) {
          "Download a summary certifying this successful validation"
        } else {
          paste(
            "Download a summary of this validation.",
            "Some datasets have not been validated, so it will report the",
            "validation as incomplete rather than passed."
          )
        }
      )
    )
  })

  output$dl_validation_summary <- downloadHandler(
    filename = function() {
      paste0("validation_summary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
    },
    content = function(file) {
      req(rv$dta)
      html <- dta_build_validation_report(rv$dta, rv$status)
      writeLines(html, file, useBytes = TRUE)
    }
  )

  # --- reset --------------------------------------------------------------
  observeEvent(input$reset_app, {
    showModal(modalDialog(
      title = "Start over?",
      "This clears the loaded DTA and all uploads from this session.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_reset", "Start over", class = "btn btn-danger")
      ),
      easyClose = TRUE
    ))
  })
  observeEvent(input$confirm_reset, {
    rv$dta <- NULL
    rv$yaml_text <- NULL
    rv$structure <- NULL
    rv$active <- NULL
    rv$uploads <- list()
    rv$status <- list()
    rv$is_example <- FALSE
    rv$example_target <- NULL
    rv$doc_token <- rv$doc_token + 1
    # "Start over" clears the document entirely, so nothing is left loaded
    # to be version-locked -- the next document to arrive decides that fresh.
    rv$version_locked <- FALSE
    rv$version_baseline_yaml <- NULL
    rv$version_entry_index <- NULL
    rv$version_note <- ""
    rv$new_version_msg <- NULL
    rv$editing <- FALSE
    try(unlink(session_file() %||% character(0)), silent = TRUE)
    removeModal()
  })

  # --- status / summary outputs ------------------------------------------
  output$dataset_status_line <- renderUI({
    req(rv$active)
    st <- dta_lookup(rv$status, rv$active, "pending")
    s <- rv$structure[[rv$active]]
    type_txt <- if (!is.null(s) && !is.na(s$type)) s$type else ""
    ds <- dta_get_dataset(rv$dta, rv$active)
    desc <- tryCatch(ds@description, error = function(e) NULL) %||% ""
    heading <- if (nzchar(desc)) desc else rv$active
    div(
      div(
        class = "ds-head",
        tags$h4(class = "ds-desc", heading)
      ),
      div(
        style = "display:flex; align-items:center; gap:8px; margin-bottom:6px; flex-wrap:wrap;",
        # Every dataset carries a tag with its name (alongside the type tag),
        # so the dataset is always clearly identified on the Datasets page.
        tags$span(class = "dta-name-chip", rv$active),
        if (nzchar(type_txt)) tags$span(class = "status-chip status-pending", type_txt),
        status_chip(st)
      )
    )
  })

  output$loaded_files <- renderUI({
    req(rv$active, rv$structure)
    rv$uploads
    rv$status # re-render when files or validation state change
    dsname <- rv$active
    s <- rv$structure[[dsname]]
    tstatus <- dta_table_status_map(rv$dta, dsname)
    any_files <- FALSE
    blocks <- lapply(seq_along(s$handlers), function(hi) {
      h <- s$handlers[[hi]]
      key <- paste0(dsname, "||", hi)
      recs <- rv$uploads[[key]] %||% list()
      if (length(recs) == 0) {
        return(NULL)
      }
      any_files <<- TRUE
      rows <- lapply(recs, function(rec) {
        fid <- get_file_id(dsname, hi, rec$table)
        st <- dta_lookup(tstatus, rec$table, "pending")
        # "unknown" is the third tick state: the table was validated, but by a
        # run that predates import checking, so its import axis is unknown --
        # neither the green pass nor the red fail (see dta_table_status_map()).
        icon_ch <- switch(st,
          pass = "\u2714",
          fail = "\u2716",
          unknown = "?",
          "\u2014"
        )
        icls <- switch(st,
          pass = "file-ok",
          fail = "file-fail",
          unknown = "file-unknown",
          "file-pending"
        )
        ttl <- switch(st,
          pass = "Validated \u2014 no errors",
          fail = "Validation errors \u2014 see messages below",
          unknown = paste(
            "Import status unknown \u2014 validated before import checking.",
            "Re-run validation (force) to complete the check."
          ),
          "Not validated yet"
        )
        div(
          class = "loaded-file-row",
          tags$span(class = paste("file-status", icls), title = ttl, icon_ch),
          tags$span(class = "file-name", rec$file),
          if (!identical(s$type, "file")) {
            tags$span(
              class = "file-table", title = "Table name",
              paste0("\u2192 ", rec$table)
            )
          },
          actionButton(paste0("rmfile_", fid),
            label = HTML("&#x1F5D1;&#xFE0F;"),
            class = "btn btn-sm file-remove", title = "Remove this file"
          )
        )
      })
      div(
        class = "loaded-slot",
        div(class = "loaded-slot-head", tags$span(class = "slot-expected", h$expected)),
        tagList(rows)
      )
    })
    if (!any_files) {
      return(div(class = "msg-hint", "No files loaded yet."))
    }
    tagList(blocks)
  })

  output$workspace_header <- renderUI({
    req(rv$dta)
    md <- tryCatch(DTAtools::metadata(rv$dta), error = function(e) NULL)
    getf <- function(field) {
      v <- tryCatch(S7::prop(md, field), error = function(e) NULL)
      if (is.null(v) || length(v) == 0) "" else as.character(v)[1]
    }
    dataset_only <- isTRUE(rv$dataset_only)
    title <- getf("title")
    if (!nzchar(title)) {
      title <- if (dataset_only) {
        dta_dataset_names(rv$dta)[1] %||% "Dataset"
      } else {
        "Untitled DTA"
      }
    }
    version <- getf("version")
    date <- getf("date")
    div(
      class = "workspace-header",
      div(class = "ws-title", title),
      if (dataset_only || nzchar(version) || nzchar(date)) {
        div(
          class = "ws-meta",
          if (dataset_only) tags$span(class = "ws-pill", "Dataset \u2014 no metadata"),
          if (nzchar(version)) tags$span(class = "ws-pill", paste0("v", version)),
          if (nzchar(date)) tags$span(class = "ws-pill", date)
        )
      }
    )
  })

  output$summary_metrics <- renderUI({
    req(rv$dta)
    st <- unlist(rv$status)
    n <- length(dta_dataset_names(rv$dta))
    np <- sum(st == "pass")
    nf <- sum(st == "fail")
    nd <- sum(st == "nodata")
    div(
      style = "display:flex; gap:18px; margin-bottom:8px; flex-wrap:wrap;",
      div(div(class = "metric", n), div(class = "slot-meta", "datasets")),
      div(div(class = "metric", np), div(class = "slot-meta", "passed")),
      div(div(class = "metric", nf), div(class = "slot-meta", "failed")),
      div(div(class = "metric", nd), div(class = "slot-meta", "no data"))
    )
  })

  # The sidebar's dynamic outputs must never be suspended as "hidden".
  #
  # Whenever output$main re-renders (a load, a reset, a restored session --
  # rv$doc_token) it replaces the whole workspace DOM, including the sidebar's
  # uiOutput placeholders. When the client re-binds those it snapshots their
  # visibility, and that snapshot can race the DOM swap and report a visible
  # output as hidden. Under the default suspendWhenHidden the server then
  # suspends the render and never sends its HTML -- and unlike an output
  # inside a nav_panel(), whose visibility is re-checked when its tab fires
  # shown.bs.tab, nothing in the sidebar ever triggers a re-check, so the
  # workspace header, the summary counts and the dataset list stay blank
  # until something incidental (a window resize) forces a re-scan. Seen in the
  # wild as "removed a dataset and the overview and Datasets list
  # disappeared" -- back when every rv$structure assignment re-rendered
  # output$main; the doc_token decoupling has since removed those extra swaps,
  # but every swap that remains still races. These renders are cheap; send
  # them unconditionally.
  #
  # edit_gate lives in the brandbar, outside output$main, so it never races
  # the workspace DOM swap itself -- but it re-renders exactly when a document
  # loads (rv$version_locked flips), which is the same moment a fresh page
  # visit's own visibility snapshot can be unreliable, so it is included here
  # for the same belt-and-braces reason as the sidebar outputs above.
  for (sidebar_output in c(
    "workspace_header", "summary_metrics", "dataset_nav_ui",
    "add_dataset_ui", "validation_report_ui", "edit_gate"
  )) {
    outputOptions(output, sidebar_output, suspendWhenHidden = FALSE)
  }

  # --- dataset detail (structure only -> stable file inputs) --------------
  output$dataset_detail <- renderUI({
    req(rv$active, rv$structure)
    s <- rv$structure[[rv$active]]
    req(!is.null(s))
    ds_idx <- s$index
    example_files <- if (isTRUE(rv$is_example)) dta_example_data_files() else character(0)

    if (length(s$handlers) == 0) {
      slots <- div(class = "msg-hint", "No files are declared for this dataset yet. Use Edit > Files to add the files you expect to receive.")
    } else {
      slot_cards <- lapply(seq_along(s$handlers), function(hi) {
        h <- s$handlers[[hi]]
        upid <- sprintf("up_%d_%d", ds_idx, hi)
        exid <- sprintf("expick_%d_%d", ds_idx, hi)
        multiple <- isTRUE(!is.na(h$max) && h$max > 1)
        up_ctrl <- div(
          class = "dropzone",
          fileInput(upid,
            label = if (multiple) "Drop or choose file(s)" else "Drop or choose a file",
            multiple = multiple
          )
        )
        card(
          class = "slot-card",
          card_header(
            div(
              tags$strong("Expected: "),
              tags$span(class = "slot-expected", h$expected),
              tags$span(class = "slot-meta", paste0("  \u2022  ", h$count))
            )
          ),
          card_body(
            if (nzchar(h$hint)) div(class = "slot-meta", style = "margin-bottom:8px;", h$hint),
            if (length(example_files) > 0) {
              div(
                class = "slot-example",
                up_ctrl,
                div(
                  class = "slot-example-or",
                  tags$span("or"),
                  actionButton(
                    exid,
                    label = HTML("&#x1F4C2; Load an example file\u2026"),
                    class = "btn slot-example-btn",
                    title = "Pick a bundled example file to load into this slot"
                  )
                )
              )
            } else {
              up_ctrl
            }
          )
        )
      })
      slots <- do.call(bslib::layout_columns, c(list(col_widths = 12), slot_cards))
    }

    tagList(
      uiOutput("dataset_status_line"),
      div(
        class = "ds-actions",
        style = "margin-bottom:12px;",
        actionButton("check_one", "Check this dataset",
          class = "btn btn-primary"
        ),
        # Editing the specification is gated on Edit mode; checking it and
        # exporting it are not, so those two stay put and only the menu between
        # them appears and disappears. div() drops a NULL child, so nothing else
        # in this row has to know.
        #
        # s$type ("tabular"/"file", set by build_structure()) is what decides
        # whether ds_edit_menu() offers Columns/Rules at all -- a file dataset
        # has no @specs for either to edit. `s` is already this render's own
        # dataset (req(!is.null(s)) above), so this reads it rather than
        # re-deriving the type from rv$active/rv$dta a second time.
        if (editing()) ds_edit_menu(s$type),
        downloadButton("dl_ds_yaml", "Export DataSet YAML",
          class = "btn btn-outline-primary",
          title = "Download this dataset's specification as YAML"
        )
      ),
      # div(
      #  class = "msg-hint", style = "margin:-6px 0 12px;",
      #  "Uploads are validated against the file handler as you add them."
      # ),
      # tags$h5("Expected files"),
      div(
        class = "msg-hint", style = "margin:-4px 0 10px;",
        "Drop each required file below. Loaded files appear underneath."
      ),
      slots,
      card(
        card_header(
          div(
            style = "display:flex; justify-content:space-between; align-items:center; gap:8px;",
            tags$span("Loaded files"),
            actionButton("discard_all",
              label = HTML("&#x1F5D1;&#xFE0F; Discard all"),
              class = "btn btn-sm btn-outline-danger",
              title = "Remove all loaded files from this dataset"
            )
          )
        ),
        card_body(uiOutput("loaded_files"))
      )
      # Validation messages now live in the floating, foldable dock pinned to
      # the bottom of the window (output$floating_msgs), not inline here.
    )
  })

  # --- main layout (landing vs workspace) --------------------------------
  output$main <- renderUI({
    # Depend ONLY on rv$doc_token, bumped exactly where the DOCUMENT changes
    # identity: apply_loaded() (a new load), confirm_reset and restore_session.
    # Nothing that merely mutates the loaded document -- metadata edits,
    # uploads, adding/removing/renaming a dataset, handler edits, a raw-YAML
    # apply -- may rebuild the whole workspace DOM: that would reset the
    # active nav tab and every file input, and re-open the client-side
    # visibility-snapshot race described at the suspendWhenHidden block below
    # for every output in the swapped DOM. rv$structure is therefore read
    # under isolate(): it only decides landing vs workspace here, and every
    # assignment that changes THAT answer also bumps rv$doc_token. Live bits
    # live in their own outputs.
    rv$doc_token
    if (is.null(isolate(rv$structure))) {
      # Landing. Reference input$dta_client_id directly so this re-renders once
      # the browser reports its id and the restore button can appear.
      restore_available <- {
        input$dta_client_id
        sf <- session_file()
        !is.null(sf) && file.exists(sf)
      }
      card(
        max_height = "620px",
        card_header(tags$h3("Load a DTA / DTS specification file", style = "margin:0;")),
        card_body(
          p(
            "Drag and drop a DTA / DTS settings ", tags$code(".yaml"),
            " file to begin, or load the bundled example."
          ),
          div(
            class = "msg-hint", style = "margin:-6px 0 8px;",
            "A full DTA (with metadata) or a standalone dataset spec is accepted; ",
            "a dataset-only file is loaded without a Metadata section."
          ),
          div(
            class = "dropzone",
            fileInput("dta_file", "Drop or choose a .yaml / .yml file",
              accept = c(".yaml", ".yml"), width = "100%"
            )
          ),
          div(
            style = "display:flex; gap:10px; margin-top:8px;",
            actionButton("create_new", "Create new", class = "btn btn-outline-primary"),
            actionButton("create_from_template", "Create new from template", class = "btn btn-primary"),
            actionButton("load_example", "Load example", class = "btn btn-outline-primary"),
            if (restore_available) {
              actionButton("restore_session", "Restore previous session",
                class = "btn btn-outline-secondary"
              )
            }
          )
        )
      )
    } else {
      # Workspace
      layout_sidebar(
        sidebar = sidebar(
          width = 320,
          uiOutput("workspace_header"),
          uiOutput("summary_metrics"),
          uiOutput("dataset_nav_ui"),
          uiOutput("add_dataset_ui"),
          actionButton("check_all", "Check all datasets", class = "btn btn-primary w-100"),
          tags$hr(),
          downloadButton("dl_yaml", "Export DTA YAML", class = "btn btn-outline-primary w-100"),
          actionButton("export_modal_open", "Export DTA", class = "btn btn-primary w-100", style = "margin-top: 6px;"),
          uiOutput("validation_report_ui"),
          tags$hr(),
          actionButton("reset_app", "Start over", class = "btn btn-outline-danger w-100")
        ),
        {
          # The Metadata tab is always available: a loaded dataset YAML is wrapped
          # into a full (empty-metadata) DTA that the user can complete.
          panels <- list(
            nav_panel("Datasets", uiOutput("dataset_detail")),
            nav_panel("Metadata", uiOutput("metadata_editor")),
            nav_panel(
              "Raw YAML",
              div(
                class = "yaml-edit-bar",
                uiOutput("yaml_edit_hint"),
                uiOutput("yaml_edit_actions")
              ),
              uiOutput("yaml_validation_msg"),
              if (requireNamespace("shinyAce", quietly = TRUE)) {
                div(
                  class = "yaml-ace-wrap",
                  shinyAce::aceEditor(
                    "raw_yaml_editor",
                    value = isolate(rv$yaml_text) %||% "",
                    mode = "yaml", theme = "tomorrow_night",
                    # The CSS (.yaml-ace-wrap, theme.R) is what actually governs
                    # the rendered height once the wrapper is draggable -- see
                    # yaml_ace_resize_js above -- but this stays a realistic
                    # initial value in its own right, matching it, rather than
                    # relying entirely on the !important override.
                    height = "70vh", fontSize = 13,
                    showLineNumbers = TRUE, wordWrap = FALSE,
                    debounce = 100, autoComplete = "disabled",
                    # Born in the correct state: the observe() below only
                    # fires on later editing() changes (it is not
                    # re-triggered by loading a document), and it messages an
                    # input id that does not exist yet the first time it runs
                    # -- this editor lives inside output$main, which is still
                    # showing the landing card at server start.
                    readOnly = !isolate(editing())
                  )
                )
              } else {
                # A plain <textarea> is natively resizable (a drag handle in
                # the bottom-right corner, no CSS needed), unlike Ace -- so
                # this fallback only needs the taller starting point. Same
                # born-in-the-correct-state reasoning as the Ace editor above:
                # the observe() that calls shinyjs::disable()/enable() only
                # reacts to later editing() changes, and messages an input id
                # that is not in the DOM yet the first time it runs -- so this
                # has to start disabled itself when not editing.
                ta <- textAreaInput("raw_yaml_editor",
                  label = NULL,
                  value = isolate(rv$yaml_text), width = "100%", rows = 28
                )
                if (isolate(editing())) ta else shinyjs::disabled(ta)
              }
            )
          )
          do.call(navset_card_tab, Filter(Negate(is.null), panels))
        }
      )
    }
  })

  # --- restore previous session ------------------------------------------
  observeEvent(input$restore_session, {
    sf <- session_file()
    if (is.null(sf) || !file.exists(sf)) {
      return()
    }
    saved <- tryCatch(readRDS(sf), error = function(e) NULL)
    if (is.null(saved)) {
      showNotification("Could not restore the previous session.", type = "error")
      return()
    }
    # Defence in depth behind the per-browser filename: refuse a payload that
    # does not carry this browser's own id, so a stale or planted file under a
    # guessed name cannot be loaded into someone else's session.
    if (!identical(saved$client_id, client_id())) {
      showNotification("Cannot restore a session saved by a different browser.", type = "error")
      return()
    }
    # Prefer the saveRDS-safe dump (arrow tables collected to data.frames);
    # fall back to a legacy `dta` field for older session files.
    restored <- if (!is.null(saved$dump)) {
      tryCatch(dta_restore_session(saved$dump), error = function(e) NULL)
    } else {
      saved$dta
    }
    if (is.null(restored)) {
      showNotification("Could not restore the previous session.", type = "error")
      return()
    }
    rv$dta <- restored
    rv$yaml_text <- saved$yaml_text
    rv$structure <- saved$structure %||% build_structure(restored)
    ups <- saved$uploads %||% list()
    rv$uploads <- lapply(ups, function(recs) {
      if (length(recs) == 0) {
        return(list())
      }
      if (is.character(recs)) {
        lapply(recs, function(f) list(file = f, table = tools::file_path_sans_ext(basename(f))))
      } else {
        recs
      }
    })
    rv$status <- saved$status %||% stats::setNames(
      rep("pending", length(dta_dataset_names(restored))),
      dta_dataset_names(restored)
    )
    rv$active <- saved$active %||% (dta_dataset_names(restored)[1] %||% NULL)
    rv$dataset_only <- isTRUE(saved$dataset_only)
    rv$is_example <- isTRUE(saved$is_example)
    # A session file written before the versioning feature existed has none
    # of these fields. The fallback is to treat the restored document as a
    # gated load -- version_locked = TRUE, editing switched off -- rather
    # than silently editable: read-only-until-versioned is this app's
    # standing posture for anything it did not itself just create, and an
    # older session reopening editable by default would be a quiet regression
    # of that rule rather than a neutral default.
    rv$version_locked <- if (is.null(saved$version_locked)) TRUE else isTRUE(saved$version_locked)
    rv$version_baseline_yaml <- saved$version_baseline_yaml %||% saved$yaml_text
    # NOT %||% -- NULL is a legitimate value here (no version entry opened
    # yet this session), so coalescing it to a default would misattribute a
    # change summary to the wrong version_history entry on the next export.
    rv$version_entry_index <- saved$version_entry_index
    rv$version_note <- saved$version_note %||% ""
    rv$new_version_msg <- NULL
    # Same fallback as version_locked above, for a session file written
    # before this field existed: isTRUE(NULL) is FALSE, so an absent
    # saved$editing restores as not-editing for free -- the conservative
    # default, with no explicit is.null() check needed.
    rv$editing <- isTRUE(saved$editing)
    rv$md_token <- rv$md_token + 1
    rv$contacts_token <- rv$contacts_token + 1
    rv$doc_token <- rv$doc_token + 1
    showNotification("Previous session restored.", type = "message")
  })
}

shinyApp(ui, server)
