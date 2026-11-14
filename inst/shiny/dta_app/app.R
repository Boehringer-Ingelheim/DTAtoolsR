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
# The brand bar and footer are built per request from the library's app config
# (brandbar_ui()/app_footer_ui() in R/ui_components.R).

# Landing drop target glyph: the logo's document-and-tick motif as an outline,
# in the brand greens. Inline SVG so it inherits the sheet's colours and needs
# no extra asset; aria-hidden because the heading beside it carries the text.
landing_drop_glyph <- paste0(
  "<svg class='landing-drop-glyph' viewBox='0 0 48 48' fill='none' ",
  "stroke-width='2.5' stroke-linecap='round' stroke-linejoin='round' ",
  "aria-hidden='true' focusable='false'>",
  "<path class='glyph-doc' d='M12 6h16l10 10v26H12z'/>",
  "<path class='glyph-doc' d='M28 6v10h10'/>",
  "<path class='glyph-doc' d='M18 24h8M18 30h6'/>",
  "<path class='glyph-tick' d='M25 36l4 4 8-9'/>",
  "</svg>"
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
function DTA_syncMsgsDock(d){
  var b = d.querySelector('.msgs-dock-bar');
  if (b) { b.setAttribute('aria-expanded', d.classList.contains('collapsed') ? 'false' : 'true'); }
}
function DTA_toggleMsgsDock(e){
  if (e) { e.stopPropagation(); }
  var d = document.getElementById('dta-msgs-dock');
  if (!d) return;
  d.classList.toggle('collapsed');
  DTA_syncMsgsDock(d);
  if (!d.classList.contains('collapsed')) { window.dispatchEvent(new Event('resize')); }
}
// Keyboard access for the bar (a div holding real buttons, so it cannot itself
// be a <button>): Enter/Space on the bar ONLY, never on the buttons inside it.
function DTA_msgsDockKey(e){
  if (e.target !== e.currentTarget) return;
  if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); DTA_toggleMsgsDock(e); }
}
Shiny.addCustomMessageHandler('dta_msgs_dock', function(action){
  var d = document.getElementById('dta-msgs-dock');
  if (!d) return;
  if (action === 'open') { d.classList.remove('collapsed'); window.dispatchEvent(new Event('resize')); }
  else if (action === 'close') { d.classList.add('collapsed'); }
  else { d.classList.toggle('collapsed'); }
  DTA_syncMsgsDock(d);
});
"

# Quick actions ("All", "None", "Default") for a vocabulary multi-select in the
# template options modal: set the selectize control's value from a link's
# onclick, returning false so the href="#" never navigates.
#
# Client-side rather than one observer per slot, because those slots come and
# go with every modal open: server observers created there would accumulate
# for the life of the session, one set per open, with nothing to destroy them.
# Selectize writes the new value back through Shiny's own binding, so the
# server still sees an ordinary input update -- which is what re-renders the
# "n of m selected" count next to the links.
#
# The link passes ITSELF, not the control's id: the id is built from a slot id
# a template author chose, and splicing that into a JS string literal would
# let a private template run script in the app with one well-placed quote.
# The control is found from the link instead, and the default values ride in
# a data attribute, which htmltools escapes.
vocab_set_js <- "
function DTA_vocabSet(link, what){
  var box = link && link.closest ? link.closest('.tmpl-vocab') : null;
  var el = box ? box.querySelector('select') : null;
  if (!el || !el.selectize) return false;
  var s = el.selectize;
  if (what === 'all') s.setValue(Object.keys(s.options));
  else if (what === 'none') s.clear();
  else s.setValue((link.dataset.vals || '').split('\\u001f').filter(Boolean));
  return false;
}

// The column editor's term checklist (vocab_term_checklist(), R/template_ui.R).
// While a filter is typed the count also says how many rows are shown, which
// is what All/None act on.
function DTA_vocabCount(el){
  var box = el && el.closest ? el.closest('.vocab-pick') : null;
  var c = box ? box.querySelector('.vocab-pick-count') : null;
  if (!c) return;
  var f = box.querySelector('.vocab-pick-filter');
  var shown = [].filter.call(box.querySelectorAll('.checkbox'), function(r){ return r.style.display !== 'none'; }).length;
  c.textContent = box.querySelectorAll('input[type=checkbox]:checked').length + ' of ' +
    box.querySelectorAll('input[type=checkbox]').length + ' selected' +
    (f && f.value.trim() ? ' · ' + shown + ' shown' : '');
}
function DTA_vocabFilter(input){
  var box = input.closest('.vocab-pick');
  if (!box) return;
  // Every word must occur somewhere in the row (code, label or description,
  // in any order): the row's text has markup whitespace between those parts, so
  // one contiguous phrase would miss 'T002 Term'.
  var words = input.value.toLowerCase().split(/\\s+/).filter(Boolean), shown = 0;
  box.querySelectorAll('.checkbox').forEach(function(row){
    var text = row.textContent.toLowerCase();
    var hit = words.every(function(w){ return text.indexOf(w) >= 0; });
    row.style.display = hit ? '' : 'none';
    if (hit) shown++;
  });
  var empty = box.querySelector('.vocab-pick-empty');
  if (empty) empty.style.display = shown ? 'none' : '';
  DTA_vocabCount(input);
}
// Acts on the rows the filter shows, and fires ONE change event: Shiny reads
// the whole group on any change, so one is enough for every box flipped.
function DTA_vocabCheck(link, on){
  var box = link.closest('.vocab-pick');
  if (!box) return false;
  var last = null;
  box.querySelectorAll('.checkbox').forEach(function(row){
    var cb = row.querySelector('input[type=checkbox]');
    if (row.style.display !== 'none' && cb && cb.checked !== on) { cb.checked = on; last = cb; }
  });
  if (last) last.dispatchEvent(new Event('change', {bubbles: true}));
  DTA_vocabCount(link);
  return false;
}
document.addEventListener('change', function(e){
  if (e.target && e.target.closest && e.target.closest('.vocab-pick')) DTA_vocabCount(e.target);
});
// Put the cursor in the term filter when the picker's terms appear, so typing
// starts at once. shiny:value fires BEFORE the new HTML is inserted, hence the
// timeout. Only when a filter exists (more than 10 terms).
if (window.jQuery) jQuery(document).on('shiny:value', function(e){
  if (e.name !== 'col_vocab_terms') return;
  setTimeout(function(){
    var f = document.querySelector('.vocab-pick-filter');
    // Not while the user is choosing the vocabulary: picking another one
    // re-renders the terms and must not take the focus off that control.
    var sel = document.getElementById('col_vocab_ref');
    var box = sel && sel.closest ? sel.closest('.shiny-input-container') : null;
    if (f && !(box && box.contains(document.activeElement))) f.focus();
  }, 0);
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

# Un-suspend the outputs inside a modal once the modal is really on screen.
#
# Shiny suspends an output whose element looked hidden when the client bound
# it, and re-checks that only when something fires an event it listens for --
# shown.bs.tab for a nav_panel(), shown.bs.collapse for an accordion. There is
# no such re-check for a modal. Bootstrap 5 then guarantees the bad snapshot:
# Modal.show() hands _showElement() to the backdrop as a completion callback,
# so .modal() returns with the dialog still display:none, and Shiny binds the
# whole dialog -- body and everything the body's own render inserts -- inside
# that gap. Every output in it is recorded as hidden and suspended forever.
#
# Seen in the wild as "creating templates does not work": the picker modal
# opened with an empty body, because output$template_picker_ui -- the uiOutput
# holding the search box, library filter and the list/detail panes -- was
# suspended and its HTML never sent. "Use this template" then found no
# selection and said so. Every other modal
# whose body is a uiOutput() (Create new, Add dataset, the file/column/rule/
# details editors) was dead the same way, as were the inline *_msg outputs
# that report a rejected value.
#
# shown.bs.modal fires after the dialog is on screen, which is after the body
# and its nested outputs have bound, so ONE sweep covers them all; anything
# inserted later binds against a visible modal and needs no help. The listener
# is delegated from `document` (the event bubbles) so it also covers a modal
# that does not exist yet, and vanilla rather than jQuery so it does not
# depend on Bootstrap's jQuery bridge still being present.
#
# It RE-TAKES the snapshot rather than asserting "visible": an output the modal
# itself hides -- a conditionalPanel, a second step of a two-step body -- is
# meant to stay suspended, and forcing it visible would start rendering it. No
# modal here has one today, which is exactly why a sweep that hard-coded
# `false` would have looked correct until someone added one.
#
# Not outputOptions(suspendWhenHidden = FALSE), which is how the sidebar
# outputs below solve their own version of this: that takes a list of ids, and
# the ids at risk here are not enumerable -- a modal body's own render inserts
# outputs (col_tbl, col_vocab_terms, ...) that bind after the body arrives and
# race the same fade. Naming only the bodies would leave those broken, and
# naming everything would keep the editors' tables re-rendering on every data
# change with no modal open at all.
#
# The same listener also honours an `autofocus` attribute inside the dialog,
# for the same reason it exists at all: Bootstrap 5 moves focus to the dialog
# element itself as part of showing it, discarding whatever the browser's own
# autofocus handling had done, so the attribute only takes effect if something
# re-applies it AFTER the show completes -- and this is already the one thing
# that runs at exactly that moment. It is what puts the caret in the template
# picker's search box when the modal opens with its body already rendered.
modal_unsuspend_js <- "
document.addEventListener('shown.bs.modal', function (ev) {
  if (typeof Shiny === 'undefined' || !ev.target.querySelectorAll) return;
  function hidden(el) {
    for (var n = el; n && n.nodeType === 1; n = n.parentElement) {
      if (window.getComputedStyle(n).display === 'none') return true;
    }
    return false;
  }
  ev.target.querySelectorAll('.shiny-bound-output').forEach(function (el) {
    if (el.id) {
      Shiny.setInputValue('.clientdata_output_' + el.id + '_hidden', hidden(el));
    }
  });
  var f = ev.target.querySelector('[autofocus]');
  if (f) f.focus();
});
"

# Report every modal arriving on or leaving the screen, with the kind a
# showModal() call stamped on it (data-dta-modal, currently only the contact
# editor's 'edit_contact'). The server closes a dialog in exactly one place it
# can see -- its own removeModal() -- while Cancel, Esc, a backdrop click and
# one dialog replacing another all happen in the browser, so without this
# report the server can never know whether the dialog it is about to open is
# already open. edit_contact_flow() (server) is the consumer. Bootstrap fires
# both events on the modal element and they bubble, hence the delegated
# handler; jQuery strips the namespace, so ev.type is 'shown' or 'hidden'.
modal_state_js <- "
$(document).on('shown.bs.modal hidden.bs.modal', '#shiny-modal', function (ev) {
  if (typeof Shiny === 'undefined' || !Shiny.setInputValue) return;
  Shiny.setInputValue('dta_modal_state', {
    event: ev.type,
    kind: this.getAttribute('data-dta-modal') || '',
    id: this.getAttribute('data-dta-modal-id') || '',
    at: Date.now()
  }, {priority: 'event'});
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

# The page is built per request from the library's app config, so a library can
# retitle, recolour and re-brand it (see R/app_config.R).
ui_page <- function(cfg) {
  bslib::page_fluid(
    theme = bi_theme(cfg$colors),
    shinyjs::useShinyjs(),
    tags$head(
      tags$title(cfg$branding$window_title),
      tags$style(bi_css(cfg$colors)),
      tags$script(shiny::HTML(reset_fileinput_js)),
      tags$script(shiny::HTML(msgs_dock_js)),
      tags$script(shiny::HTML(vocab_set_js)),
      tags$script(shiny::HTML(download_trigger_js)),
      tags$script(shiny::HTML(client_id_js)),
      tags$script(shiny::HTML(yaml_ace_resize_js)),
      tags$script(shiny::HTML(modal_unsuspend_js)),
      tags$script(shiny::HTML(modal_state_js)),
      # Unlike the eight above, this one is a function in R/ui_components.R
      # rather than a string here, because its behaviour is worth testing
      # separately -- see click_guard_script() there, and the test file, for why
      # double-click protection cannot live on the server at all. Its position
      # in this list is not load-bearing: it installs a capture-phase listener
      # on `document`, which runs ahead of every element's own handler whenever
      # it was registered.
      click_guard_script()
    ),
    brandbar_ui(cfg),
    div(style = "padding: 18px;", uiOutput("main")),
    app_footer_ui(cfg, dta_pkg_version),
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
}

ui <- function(req) ui_page(dta_app_config())

# ---------------------------------------------------------------------------
# Delivery: the sidebar block, the Delivery view and the ambiguity dialog
#
# Pure builders: the document, the container findings and the last delivery go
# in, tags come out. They hold no reactive state, so the server stays the one
# place that decides when they run. All text goes in through tags$*, which
# escapes it: file names are user-supplied.
# ---------------------------------------------------------------------------

dv_empty_findings <- function() {
  data.frame(
    dataset = character(0), item = character(0), file = character(0),
    container = character(0), arrived = character(0), status = character(0),
    message = character(0), stringsAsFactors = FALSE
  )
}

# The packaging findings of the live document; never throws.
dv_findings <- function(dta) {
  if (is.null(dta)) {
    return(dv_empty_findings())
  }
  f <- tryCatch(DTAtools::container_findings(dta), error = function(e) NULL)
  if (is.data.frame(f)) f else dv_empty_findings()
}

dv_provenance <- function(ds) {
  p <- tryCatch(ds@provenance, error = function(e) NULL)
  if (is.list(p)) p else list()
}

# The items (tables, or file paths of a file dataset) bound in a dataset: the
# keys of its provenance records.
dv_bound_items <- function(ds) {
  if (inherits(ds, "DTAtools::DTADataSetFile")) {
    as.character(tryCatch(ds@file_paths, error = function(e) character(0)))
  } else {
    as.character(names(tryCatch(ds@tables, error = function(e) NULL)))
  }
}

dv_ds_label <- function(dta, id) .ctr_ds_label(id, dta)

# TRUE when something is bound in at least one dataset.
dv_anything_bound <- function(dta) {
  any(vapply(
    dta_dataset_names(dta),
    function(nm) isTRUE(dta_dataset_readiness(dta, nm)$has_data),
    logical(1)
  ))
}

# One entry per container in effect: its row (dta_container_rows()), the datasets
# whose entries it holds, the findings against it and the archive names (the
# first element of the chain) its files arrived inside.
dv_container_states <- function(dta, findings) {
  rows <- dta_container_rows(dta)
  if (length(rows) == 0) {
    return(list())
  }
  ca <- tryCatch(DTAtools::container_assignments(dta), error = function(e) NULL)
  lapply(rows, function(r) {
    mine <- if (is.null(ca)) NULL else ca[!is.na(ca$container) & ca$container == r$key, , drop = FALSE]
    ds_ids <- unique(as.character(mine$dataset))
    # A finding with no single container (unrecorded, several globs) belongs to
    # every container its dataset has entries in.
    f <- findings[
      (!is.na(findings$container) & findings$container == r$filename) |
        (is.na(findings$container) & findings$dataset %in% ds_ids), ,
      drop = FALSE
    ]
    inside <- character(0)
    for (id in ds_ids) {
      prov <- dv_provenance(dta_get_dataset(dta, id))
      for (it in setdiff(names(prov), f$item[f$dataset == id])) {
        chain <- as.character(prov[[it]]$container)
        if (length(chain) && dv_glob_hits(r$filename, chain[[1]])) inside <- c(inside, chain[[1]])
      }
    }
    list(
      row = r, datasets = ds_ids, findings = f, inside = inside,
      n_outside = sum(f$status == "outside"),
      n_unverified = sum(f$status == "unverified"),
      n_unrecorded = sum(f$status == "unrecorded")
    )
  })
}

# The sidebar line of one container: first match wins, worst first.
dv_container_line <- function(st) {
  fn <- st$row$filename
  files <- function(n) .ctr_count(n, "file", "files")
  if (st$n_outside > 0) {
    list(glyph = "\u26A0", cls = "dp-bad", text = paste0(fn, ": ", files(st$n_outside), " not inside it"))
  } else if (st$n_unverified > 0) {
    n <- st$n_unverified
    list(
      glyph = "\u25D0", cls = "dp-warn",
      text = paste0(fn, ": ", files(n), if (n == 1) " checked on its own" else " checked on their own")
    )
  } else if (st$n_unrecorded > 0) {
    list(
      glyph = "?", cls = "dp-warn",
      text = paste0(fn, ": ", files(st$n_unrecorded), ", origin not recorded")
    )
  } else if (length(st$inside) > 0) {
    list(
      glyph = "\u2713", cls = "dp-ok",
      text = paste0(paste(unique(st$inside), collapse = ", "), ": ", files(length(st$inside)))
    )
  } else if (isTRUE(st$row$n_files == 0)) {
    list(glyph = "\u25CB", cls = "dp-none", text = paste0(fn, ": no files assigned"))
  } else {
    list(glyph = "\u25CB", cls = "dp-none", text = paste0(fn, ": not delivered yet"))
  }
}

# The packaging verdict of dataset `id` in the live findings: "outside" (a file
# is not inside the container the agreement requires), "unverified" (checked
# on its own, or its origin is not recorded) or "none".
dv_ds_packaging <- function(findings, id) {
  s <- findings$status[findings$dataset == id]
  if (any(s == "outside")) "outside" else if (length(s) > 0) "unverified" else "none"
}

# "1 file not inside its container, 2 files whose packaging is not verified".
dv_packaging_phrase <- function(findings) {
  n_out <- sum(findings$status == "outside")
  n_unv <- sum(findings$status != "outside")
  paste(c(
    if (n_out > 0) paste(.ctr_count(n_out, "file", "files"), "not inside", if (n_out == 1) "its container" else "their container"),
    if (n_unv > 0) paste(.ctr_count(n_unv, "file", "files"), "whose packaging is not verified")
  ), collapse = ", ")
}

# Rows of the last delivery that nothing was bound for (the "unmatched" count).
dv_n_unmatched <- function(plan) sum(plan$status %in% c("unused", "refused", "ambiguous"))

# The sidebar's per-container lines, the last-delivery line and the manager link.
dv_panel_ui <- function(dta, findings, last, editing = FALSE) {
  states <- dv_container_states(dta, findings)
  lines <- lapply(states, function(st) {
    l <- dv_container_line(st)
    div(
      class = paste("dp-line", l$cls), title = l$text,
      span(class = "dp-glyph", `aria-hidden` = "true", l$glyph),
      span(class = "dp-text", l$text)
    )
  })
  last_line <- if (!is.null(last) && is.data.frame(last$plan)) {
    plan <- last$plan
    div(
      class = "dp-last",
      sprintf(
        "Last delivery, %s: %s loaded, %d unmatched",
        format(last$at %||% Sys.time(), "%H:%M"),
        .ctr_count(sum(plan$status == "loaded"), "file", "files"), dv_n_unmatched(plan)
      ),
      " \u00B7 ", actionLink("delivery_details", "Details")
    )
  }
  link <- if (length(states) > 0 || isTRUE(editing)) {
    div(class = "dp-links", actionLink("open_containers", "Containers\u2026"))
  }
  div(class = "delivery-panel", lines, last_line, link)
}

# How the file(s) of an item reached the document, in the Delivery view's words.
dv_arrival <- function(rec) {
  if (is.null(rec)) {
    return("origin not recorded")
  }
  chain <- as.character(rec$container)
  if (length(chain)) {
    paste("inside", paste(chain, collapse = " > "))
  } else if (identical(rec$via, "file")) {
    "on its own"
  } else {
    "loose"
  }
}

# A routing reason in the words of an app user: the R-only phrases are rewritten.
dv_reason <- function(reason, budget_limited = FALSE) {
  if (is.null(reason) || length(reason) != 1 || is.na(reason)) {
    return("")
  }
  x <- dta_app_reason(reason, budget_limited)
  x <- gsub("; pass route = list\\(\"[^\"]*\" = \"<dataset>\"\\) to choose", "; choose the dataset when asked", x)
  x <- gsub(
    "kept as one file: a type: any entry takes it as a deliverable; to have its files checked, name it as the container of their entries",
    "kept as one file because a file entry takes the archive itself; to have its files checked, assign them to a container",
    x,
    fixed = TRUE
  )
  x
}

dv_result_word <- function(status, reason) {
  switch(status,
    loaded = "loaded",
    refused = "refused",
    ambiguous = "several datasets match",
    opened = if (is.na(reason)) "opened" else "could not be opened",
    ignored = "ignored",
    unused = if (is.na(reason) || identical(reason, "matches no file entry of the specification")) {
      "no matching entry"
    } else {
      "not loaded"
    },
    status
  )
}

# The table of the last delivery: problem rows first, an opened archive as a
# group header over the files it held.
dv_last_table <- function(plan, ca, dta = NULL) {
  for (cn in c("container", "reason", "dataset", "conformant")) {
    if (!cn %in% names(plan)) plan[[cn]] <- NA
  }
  if (!"handler" %in% names(plan)) plan$handler <- NA_integer_
  keep <- plan$status != "opened" | !is.na(plan$reason)
  plan <- plan[keep, , drop = FALSE]
  if (nrow(plan) == 0) {
    return(div(class = "msg-hint", "Nothing in the last delivery was listed."))
  }
  problem <- plan$status %in% c("refused", "ambiguous") |
    (plan$status == "opened" & !is.na(plan$reason)) |
    (plan$status == "loaded" & !is.na(plan$conformant) & !plan$conformant %in% TRUE)
  matched <- vapply(seq_len(nrow(plan)), function(i) {
    if (!identical(plan$status[[i]], "loaded") || is.null(ca) || is.na(plan$dataset[[i]])) {
      return("\u2014")
    }
    e <- ca$expected[ca$dataset == plan$dataset[[i]] & ca$handler == plan$handler[[i]]]
    if (length(e)) e[[1]] else "\u2014"
  }, character(1))
  dataset_txt <- vapply(seq_len(nrow(plan)), function(i) {
    cand <- if ("candidates" %in% names(plan)) as.character(plan$candidates[[i]]) else character(0)
    if (identical(plan$status[[i]], "ambiguous") && length(cand)) {
      paste(vapply(cand, .ctr_ds_label, character(1), dta = dta), collapse = ", ")
    } else if (is.na(plan$dataset[[i]])) {
      "\u2014"
    } else {
      .ctr_ds_label(plan$dataset[[i]], dta)
    }
  }, character(1))
  row_ui <- function(i) {
    tags$tr(
      class = if (problem[[i]]) "dv-problem",
      tags$td(plan$name[[i]]),
      tags$td(if (is.na(plan$container[[i]])) "\u2014" else plan$container[[i]]),
      tags$td(dataset_txt[[i]]),
      tags$td(matched[[i]]),
      tags$td(dv_result_word(plan$status[[i]], plan$reason[[i]])),
      # "loaded" already says the file was loaded: the note starts at the "but".
      tags$td(sub("^loaded, but ", "", dv_reason(plan$reason[[i]])))
    )
  }
  body <- list()
  for (p in c(TRUE, FALSE)) {
    idx <- which(problem == p)
    for (grp in unique(plan$container[idx])) {
      in_grp <- idx[if (is.na(grp)) is.na(plan$container[idx]) else (!is.na(plan$container[idx]) & plan$container[idx] == grp)]
      if (!is.na(grp)) {
        body[[length(body) + 1L]] <- tags$tr(class = "dv-group", tags$td(colspan = 6, paste("Inside", grp)))
      }
      body <- c(body, lapply(in_grp, row_ui))
    }
  }
  tags$table(
    class = "table table-sm dv-table",
    tags$thead(tags$tr(lapply(c("File", "Inside", "Dataset", "Matched", "Result", "Note"), tags$th))),
    tags$tbody(body)
  )
}

# The members of one container: what each dataset that must arrive inside it
# received, and how.
dv_members_table <- function(dta, st, ca) {
  mine <- ca[!is.na(ca$container) & ca$container == st$row$key, , drop = FALSE]
  rows <- lapply(unique(as.character(mine$dataset)), function(id) {
    ds <- dta_get_dataset(dta, id)
    prov <- dv_provenance(ds)
    items <- dv_bound_items(ds)
    arrived <- if (length(items) == 0) "\u2014" else paste(unique(vapply(items, function(it) dv_arrival(prov[[it]]), character(1))), collapse = ", ")
    f <- st$findings[st$findings$dataset == id, , drop = FALSE]
    status <- if (any(f$status == "outside")) {
      "not inside it"
    } else if (any(f$status == "unverified")) {
      "checked on its own"
    } else if (any(f$status == "unrecorded")) {
      "origin not recorded"
    } else if (length(items) > 0) {
      "inside"
    } else {
      "not delivered yet"
    }
    tags$tr(
      tags$td(dv_ds_label(dta, id)),
      tags$td(paste(unique(mine$expected[mine$dataset == id]), collapse = ", ")),
      tags$td(arrived),
      tags$td(status)
    )
  })
  tags$table(
    class = "table table-sm dv-table",
    tags$thead(tags$tr(lapply(c("Dataset", "Expected file", "Arrived as", "Status"), tags$th))),
    tags$tbody(rows)
  )
}

# The Delivery view (main area): the packaging verdict, every container with its
# members, and the routing table of the last delivery.
dv_view_ui <- function(dta, last, findings, active = NULL) {
  ca <- tryCatch(DTAtools::container_assignments(dta), error = function(e) NULL)
  states <- dv_container_states(dta, findings)
  chip <- if (any(findings$status == "outside")) {
    list("status-fail", "Not conforming")
  } else if (any(findings$status %in% c("unverified", "unrecorded"))) {
    list("status-nodata", "Packaging not verified")
  } else if (length(states) == 0) {
    list("status-pending", "No packaging rules")
  } else if (!dv_anything_bound(dta)) {
    list("status-pending", "Nothing delivered yet")
  } else {
    list("status-pass", "Packaging conforms")
  }
  containers <- lapply(states, function(st) {
    r <- st$row
    div(
      class = "dv-container",
      tags$h5(r$filename),
      if (!is.null(r$description) && nzchar(r$description)) tags$p(class = "msg-hint", r$description),
      tags$p(class = "msg-hint", r$summary),
      if (!is.null(ca) && length(st$datasets) > 0) dv_members_table(dta, st, ca)
    )
  })
  plan <- if (!is.null(last) && is.data.frame(last$plan)) last$plan else NULL
  div(
    class = "delivery-view",
    if (!is.null(active) && nzchar(active)) {
      div(class = "dv-back", actionLink("delivery_back", paste0("\u2190 Back to ", dv_ds_label(dta, active))))
    },
    div(
      class = "dv-head",
      tags$h4("Delivery"),
      tags$span(class = paste("status-chip", chip[[1]]), tags$span(class = "status-dot"), chip[[2]])
    ),
    if (length(containers) == 0) {
      tags$p(class = "msg-hint", "The agreement names no containers: every file is judged on its content alone.")
    },
    containers,
    tags$h5("Last delivery"),
    if (is.null(plan)) {
      tags$p(class = "msg-hint", "No delivery has been loaded in this session yet.")
    } else {
      tagList(
        tags$p(
          class = "msg-hint",
          sprintf(
            "Last delivery, %s: %s \u2014 %s loaded, %d unmatched.",
            format(last$at %||% Sys.time(), "%H:%M"), paste(unique(plan$delivered), collapse = ", "),
            .ctr_count(sum(plan$status == "loaded"), "file", "files"), dv_n_unmatched(plan)
          )
        ),
        dv_last_table(plan, ca, dta),
        if (length(last$missing) > 0) {
          tags$p(class = "msg-hint", paste0("Not in the delivery: ", paste(last$missing, collapse = ", "), "."))
        }
      )
    },
    actionButton("check_all_delivery", "Check all", class = "btn btn-primary")
  )
}

# How an entry's expected name reads in prose: a pattern is described by its
# `pattern_description` when it has one (else `fallback`), never by its regex.
dv_prose_name <- function(h, fallback = handler_expected(h)) {
  if (!handler_is_pattern(h)) {
    return(handler_expected(h))
  }
  pd <- tryCatch(h@pattern_description, error = function(e) NULL)
  if (length(pd) == 1 && !is.na(pd) && nzchar(pd)) pd else fallback
}

# The expected name of dataset `id` that the file `name` matches.
dv_expected_for <- function(dta, id, name) {
  for (h in tryCatch(dta_handlers(dta_get_dataset(dta, id)), error = function(e) list())) {
    if (isTRUE(handler_matches(h, name))) {
      return(dv_prose_name(h))
    }
  }
  ""
}

# The ambiguous rows of a plan, one per distinct file name.
dv_ambiguous_rows <- function(plan) {
  amb <- plan[plan$status == "ambiguous", , drop = FALSE]
  lapply(unique(amb$name), function(nm) {
    r <- amb[amb$name == nm, , drop = FALSE]
    list(
      name = nm,
      candidates = unique(unlist(lapply(seq_len(nrow(r)), function(i) as.character(r$candidates[[i]])))),
      inside = unique(ifelse(is.na(r$container), NA_character_, r$container)),
      copies = nrow(r)
    )
  })
}

# The dialog that asks which dataset each ambiguous file is for. Radio ids carry
# the nonce, so a second dialog never inherits the client's cached answers.
dv_ambiguity_modal <- function(dta, rows, nonce, editing = FALSE) {
  n <- length(rows)
  table_rows <- lapply(seq_len(n), function(i) {
    r <- rows[[i]]
    labels <- unname(c(
      vapply(r$candidates, function(cd) {
        ex <- dv_expected_for(dta, cd, r$name)
        if (nzchar(ex)) paste0(dv_ds_label(dta, cd), " \u2014 expects ", ex) else dv_ds_label(dta, cd)
      }, character(1)),
      "Don't load"
    ))
    where <- r$inside[!is.na(r$inside)]
    tags$tr(
      tags$td(
        tags$code(r$name),
        if (r$copies > 1) div(class = "msg-hint", "one answer applies to all copies")
      ),
      tags$td(if (length(where) > 0) paste("in", paste(where, collapse = ", ")) else "\u2014"),
      tags$td(tagAppendAttributes(
        radioButtons(
          sprintf("amb_%s_%d", nonce, i),
          label = NULL, choiceNames = as.list(labels), choiceValues = as.list(c(r$candidates, "")),
          selected = character(0), inline = TRUE
        ),
        role = "radiogroup", `aria-label` = paste("Send", r$name, "to")
      ))
    )
  })
  set_all <- if (n >= 5) {
    cands <- unique(unlist(lapply(rows, function(r) r$candidates)))
    div(
      class = "amb-set-all",
      selectInput(
        "amb_set_all", "Set all to\u2026",
        choices = c(stats::setNames(cands, vapply(cands, function(cd) dv_ds_label(dta, cd), character(1))), "Don't load" = ""),
        selectize = FALSE, width = "auto"
      ),
      actionButton("amb_apply", "Apply", class = "btn btn-outline-secondary")
    )
  }
  modalDialog(
    title = if (n == 1) "Which dataset is this file for?" else "Which dataset are these files for?",
    size = "l",
    if (n == 1) {
      tags$p("This file matches more than one dataset. Choose one, or Don't load.")
    } else {
      tags$p("These files match more than one dataset. Choose one for each, or Don't load.")
    },
    if (isTRUE(editing)) {
      tags$p(class = "msg-hint", "To stop being asked, make the expected names distinct in each dataset's Edit \u25B8 Files.")
    },
    set_all,
    div(
      class = "amb-body",
      tags$table(
        class = "table table-sm dv-table",
        tags$thead(tags$tr(lapply(c("File", "Inside", "Send to"), tags$th))),
        tags$tbody(table_rows)
      )
    ),
    uiOutput("amb_error"),
    footer = div(
      class = "amb-footer",
      actionButton("amb_cancel", "Cancel delivery (nothing is loaded)", class = "btn btn-outline-secondary"),
      actionButton("amb_load", "Load delivery", class = "btn btn-primary")
    ),
    easyClose = FALSE
  )
}

# ---------------------------------------------------------------------------
# Containers: the manager's pure UI pieces and the file-name conflict warnings
# ---------------------------------------------------------------------------

# One sentence per other dataset whose file names overlap with `ds` (the rows of
# DTAtools::file_name_conflicts()); the edit-mode pointer only while editing.
dv_conflict_sentences <- function(conf, ds, editing = FALSE, dta = NULL, in_files_editor = FALSE) {
  if (is.null(conf) || nrow(conf) == 0) {
    return(character(0))
  }
  mine <- conf[conf$dataset_a == ds | conf$dataset_b == ds, , drop = FALSE]
  if (nrow(mine) == 0) {
    return(character(0))
  }
  other <- ifelse(mine$dataset_a == ds, mine$dataset_b, mine$dataset_a)
  first <- !duplicated(other)
  paste0(
    "File names overlap with dataset '", vapply(other[first], .ctr_ds_label, character(1), dta = dta), "' (e.g. '", mine$example[first],
    "'). A delivered file matching both is asked about.",
    if (isTRUE(in_files_editor)) {
      " Make the expected names distinct."
    } else if (isTRUE(editing)) {
      " Edit \u25B8 Files to make them distinct."
    }
  )
}

# The persistent warning for `ds`, or NULL.
dv_conflict_ui <- function(conf, ds, editing = FALSE, dta = NULL, in_files_editor = FALSE) {
  s <- dv_conflict_sentences(conf, ds, editing, dta, in_files_editor)
  if (length(s) == 0) {
    return(NULL)
  }
  div(
    class = "alert alert-warning py-2 dv-conflict", role = "alert",
    lapply(s, function(x) div(x))
  )
}

# The live line under the container's file name: which archives of the last
# delivery it would take. NULL when there is nothing to say yet.
dv_container_match_line <- function(filename, last_delivery) {
  filename <- trimws(as.character(filename %||% "")[1])
  plan <- last_delivery$plan
  if (is.na(filename) || !nzchar(filename) || !is.data.frame(plan) || nrow(plan) == 0) {
    return(NULL)
  }
  archives <- unique(as.character(plan$delivered))
  archives <- archives[!is.na(archives) & dta_is_container_name(archives)]
  hit <- archives[dv_glob_hits(filename, archives)]
  if (length(hit) > 0) {
    div(class = "msg-hint ctr-match", paste0("\u2713 Matches ", paste(hit, collapse = ", "), " from the last delivery."))
  } else if (length(archives) > 0) {
    div(class = "msg-hint ctr-match", paste0("\u2717 Matches none of the last delivery's archives (", paste(archives, collapse = ", "), ")."))
  } else {
    div(class = "msg-hint ctr-match", "The last delivery held no archive to match.")
  }
}

# The read-only list of the document's containers (Metadata tab and manager).
dv_container_list_ui <- function(rows, actions = NULL) {
  if (length(rows) == 0) {
    return(div(class = "msg-hint", "No containers. A container is a .zip archive or .xpt SAS transport file that files must arrive inside."))
  }
  tags$table(
    class = "table table-sm ctr-table",
    tags$thead(tags$tr(
      tags$th("File name"), tags$th("Description"), tags$th("Files"),
      if (!is.null(actions)) tags$th("")
    )),
    tags$tbody(lapply(seq_along(rows), function(i) {
      r <- rows[[i]]
      tags$tr(
        tags$td(tags$code(r$filename), if (isTRUE(r$implied)) div(class = "msg-hint", "defined by file entries (no container record yet)")),
        tags$td(r$description %||% ""),
        tags$td(r$summary),
        if (!is.null(actions)) tags$td(class = "text-nowrap", HTML(actions[[i]]))
      )
    }))
  )
}

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
server <- function(input, output, session) {
  # The deployment's app config (dta-app.yaml in the template library), read once
  # per session. Every feature gate below is enforced here on the server, not
  # only by hiding the control: an input that is not on screen can still be
  # driven over the websocket.
  cfg <- dta_app_config()

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
    agreement_sections_token = 0,
    editing_section = NULL, # list(id) while adding or editing an agreement section
    removing_section = NULL, # stable ID named in the delete confirmation
    doc_token = 0, # bump ONLY on load/reset/restore -> re-render the main layout
    yaml_msg = NULL, # raw-YAML apply result: NULL | list(ok, error)
    load_failure = NULL, # last refused file load: NULL | list(name, text, error, findings, nonce)
    editing_contact = NULL, # list(side, index) while a contact edit modal is open
    editing_contact_nonce = NULL, # data-dta-modal-id of that open contact dialog
    editor_dataset = NULL, # dataset name the file/column/rule editor modal targets
    file_view = "list", # file-handler editor view: "list" | "form"
    file_token = 0, # bump to re-render the file-handler editor body
    file_edit_index = NULL, # index of the handler being edited (NULL = adding new)
    file_prefill = NULL, # list() of the handler fields currently loaded in the form
    file_msg = NULL, # inline file-handler-editor result: NULL | list(ok, error)
    pending_handler_removal = NULL, # list(index, tables) awaiting a remove confirmation
    col_view = "list", # column editor view: "list" | "form" | "vocab"
    col_vocab_ref = NULL, # last vocabulary picked, so the picker reopens on it
    col_vocab_sug = NULL, # vocabulary_suggestion() for the column the picker was opened on
    col_token = 0, # bump to re-render the column editor body
    col_edit_id = NULL, # id of the column being edited (NULL = adding new)
    col_prefill = NULL, # list() of the column fields loaded in the form
    col_search = "", # column-list search text, kept across the editor's re-renders
    rule_view = "list", # rule editor view: "list" | "form"
    rule_token = 0, # bump to re-render the rule editor body
    rule_edit_index = NULL, # index of the rule being edited (NULL = adding new)
    rule_search = "", # rule-list search text, kept across the editor's re-renders
    rule_prefill = NULL, # list() of the rule fields currently loaded in the form
    meta_token = 0, # bump to re-render the dataset-metadata editor body
    meta_prefill = NULL, # list() of the dataset metadata fields loaded in the form
    meta_msg = NULL, # inline dataset-metadata-editor result: NULL | list(ok, error)
    col_msg = NULL, # inline column-editor result: NULL | list(ok, error)
    rule_msg = NULL, # inline rule-editor result: NULL | list(ok, error)
    cond_n = 1L, # condition-builder row count (IF ...)
    then_n = 1L, # condition-builder row count (THEN ...)
    cond_removed = integer(0), # indices of removed (not just cleared) IF rows
    then_removed = integer(0), # indices of removed (not just cleared) THEN rows
    gcond_n = 1L, # grouped condition row count
    gconstr_n = 1L, # grouped constraint row count
    gcond_removed = integer(0), # indices of removed (not just cleared) group condition rows
    gconstr_removed = integer(0), # indices of removed (not just cleared) group constraint rows
    template_ref = NULL, # "id@version" of the creation template chosen in the picker
    template_index = NULL, # index snapshot frozen at "Use this template"
    template_vocab_specs = NULL, # vocabulary slots resolved when the options modal opened
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
    new_document_msg = NULL, # inline new-document-modal result: NULL | list(ok, error)
    last_delivery = NULL, # list(plan, missing, at) of the last committed delivery
    pending_delivery = NULL, # a delivery waiting for the ambiguity dialog's answers
    amb_msg = NULL, # inline error of the ambiguity dialog
    ctr_open = FALSE, # the container manager is on screen (its body renders NULL otherwise)
    ctr_token = 0, # bump to re-render the manager's body
    ctr_nonce = NULL, # data-dta-modal-id of the open manager
    ctr_tab = "containers", # manager tab: "containers" | "assign"
    ctr_view = "list", # containers tab: "list" | "form"
    ctr_rows = list(), # dta_container_rows() as last rendered (row buttons carry an index)
    ctr_edit_key = NULL, # key of the container being edited (NULL = adding)
    ctr_form = list(), # prefill of the container form
    ctr_confirm = NULL, # key awaiting the inline remove confirmation
    ctr_msg = NULL, # inline error of the manager
    ctr_assign = NULL, # list(rows, nonce, base) of the Assign tab as rendered
    main_view = "dataset" # the main area: "dataset" (the slots) | "delivery" (the Delivery view)
  )

  # The single gate for every editing surface. Off by default: rv$editing is
  # created FALSE (above) and isTRUE(FALSE) is FALSE, so the app is read-only
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
  # rv$version_locked survives, but it no longer gates editing and no longer
  # drives any UI. It only records (and is persisted in the autosave) that a
  # LOADED document has not yet had a new version created in this session.
  # A loaded document CAN be edited without creating a new
  # version first -- "Enable edit mode" is a deliberate route to exactly
  # that, recording nothing in the version history when no version entry is
  # open.
  editing <- reactive(isTRUE(rv$editing) && cfg$features$editing)

  # Is the landing page showing? rv$structure is NULL there, and a list once a
  # document is open -- often an EMPTY list, for a document with no datasets,
  # which is why this asks is.null() rather than testing the length. Getting
  # that wrong sends an author who has just created a specification, or who
  # has removed its last dataset, straight back to the landing page.
  #
  # The same question is asked in five places (output$main, output$edit_gate,
  # output$floating_msgs, and the two create-new observers below), and was
  # spelled out by hand at each of them; naming it once means the empty-list
  # distinction is decided in a single spot rather than re-derived five times.
  #
  # Deliberately a plain function and not a reactive. The three renderUI
  # callers must NOT take a dependency on rv$structure -- see output$main,
  # where rebuilding the workspace DOM on every dataset add would reset the
  # active nav tab and every file input -- so they depend on rv$doc_token and
  # wrap this in isolate() themselves. Isolating in here instead would hide
  # that decision from the comments that explain it, and would silently deny
  # a future caller that genuinely wants to follow the value.
  on_landing <- function() is.null(rv$structure)

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
    if (!cfg$features$editing || isolate(on_landing())) {
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
    req(rv$dta, cfg$features$editing)
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
    req(rv$dta, cfg$features$editing)
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
    req(rv$dta, cfg$features$editing)
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
    req(rv$dta, cfg$features$editing)
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
    req(rv$dta, cfg$features$editing)
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
  # The file is named by dta_session_key(), which folds in session$user where
  # the platform authenticates the viewer -- see its comment for why the
  # localStorage id alone is not enough on a shared origin.
  session_file <- function() {
    id <- client_id()
    if (is.null(id)) {
      return(NULL)
    }
    file.path(
      tempdir(),
      paste0("dtatools_app_session_", dta_session_key(id, session$user), ".rds")
    )
  }
  # Bound tables are kept beside the autosave, one Arrow file each, written
  # once per table rather than on every autosave (dta_table_store()).
  table_store <- NULL
  session_tables <- function() {
    sf <- session_file()
    if (is.null(sf)) {
      return(NULL)
    }
    tdir <- sub("[.]rds$", "_tables", sf)
    if (is.null(table_store) || !identical(attr(table_store, "dir"), tdir)) {
      table_store <<- structure(dta_table_store(tdir), dir = tdir)
    }
    table_store
  }

  # Every path this session writes user data to, registered process-wide so
  # the retention sweep spares it while the session is alive. Exports are
  # deleted when the session ends (nothing can download them after that);
  # the autosave, its tables and staged uploads stay for 'Restore previous
  # session' until the sweep ages them out.
  owned <- list(export = character(0), upload = character(0), autosave = character(0))
  own <- function(kind, path) {
    owned[[kind]] <<- unique(c(owned[[kind]], path))
    assign(session$token, unlist(owned, use.names = FALSE), envir = .dta_live_session_paths)
  }
  try(dta_sweep_session_files(), silent = TRUE)
  session$onSessionEnded(function() {
    unlink(owned$export, force = TRUE)
    # Restore re-reads file datasets from their files; tables come back from the
    # autosave's own Arrow copies (dta_restore_session(), dta_table_store()). So
    # only the extracted files a file dataset uses are kept for "Restore previous session".
    keep <- dta_file_dataset_uploads(isolate(rv$dta), isolate(rv$uploads))
    dta_delivery_cleanup(owned$upload, keep, session$token)
    # A delivery still waiting in the Replace dialog: its staged copies go too
    # (its extraction is under a delivery directory no record names).
    pending <- isolate(rv$pending_upload)$delivery
    if (!is.null(pending)) unlink(dirname(pending$copies), recursive = TRUE)
    # ... and one waiting for the ambiguity dialog (its extraction was removed
    # before it asked; only the staged copies are left).
    asking <- isolate(rv$pending_delivery)
    if (!is.null(asking)) unlink(dirname(asking$copies), recursive = TRUE)
    if (exists(session$token, envir = .dta_live_session_paths, inherits = FALSE)) {
      rm(list = session$token, envir = .dta_live_session_paths)
    }
  })

  # Stable id per bound file so its trash button keeps working across renders.
  file_id_env <- new.env(parent = emptyenv()) # "ds\u0001hi\u0001table" -> integer id
  file_id_meta <- new.env(parent = emptyenv()) # id (as chr) -> list(dataset, hi, table)
  file_rm_registry <- new.env(parent = emptyenv()) # button id -> TRUE once observed
  gcond_rm_registry <- new.env(parent = emptyenv()) # grouped-condition remove observers
  gconstr_rm_registry <- new.env(parent = emptyenv()) # grouped-constraint remove observers
  cond_rm_registry <- new.env(parent = emptyenv()) # IF/THEN condition remove observers
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
  # The label a modal title or notification shows for dataset `id`: its name,
  # else the id itself (also when the dataset is already gone).
  ds_title <- function(id) {
    label <- dta_dataset_label(dta_get_dataset(isolate(rv$dta), id))
    if (length(label) == 1 && nzchar(label)) label else id
  }

  build_structure <- function(dta) {
    names_ds <- dta_dataset_names(dta)
    if (length(names_ds) == 0) {
      return(list())
    }
    # Where an entry must arrive is its effective container (its own reference,
    # else the default container), not the raw reference the handler stores.
    ca <- tryCatch(DTAtools::container_assignments(dta), error = function(e) NULL)
    crows <- dta_container_rows(dta)
    descriptions <- stats::setNames(
      lapply(crows, function(r) r$description %||% ""),
      vapply(crows, function(r) r$filename, character(1))
    )
    stats::setNames(lapply(seq_along(names_ds), function(i) {
      ds <- dta_get_dataset(dta, names_ds[i])
      handlers <- lapply(seq_along(dta_handlers(ds)), function(hi) {
        h <- dta_handlers(ds)[[hi]]
        glob <- dta_effective_glob(dta, names_ds[i], hi, ca)
        list(
          expected = handler_expected(h),
          hint = handler_hint(h),
          count = handler_count_label(h),
          min = handler_min(h),
          max = handler_max(h),
          pattern = handler_is_pattern(h),
          # What a slot's prose calls the file: a pattern is described, not printed.
          prose = dv_prose_name(h, "a matching file"),
          container = glob,
          container_description = if (nzchar(glob)) descriptions[[glob]] %||% "" else ""
        )
      })
      list(
        index = i,
        # What the nav shows. Carried here, not read from rv$dta, so the nav
        # re-renders only when a label actually changes -- see meta_save.
        label = dta_dataset_label(ds),
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
    store <- session_tables()
    own("autosave", c(target, attr(store, "dir")))
    saved <- try(saveRDS(
      list(
        # Re-checked by restore_session: the file must have been written for
        # the user now asking for it (NULL == NULL where nobody signs in).
        user = session$user,
        dump = store$dump(isolate(rv$dta)),
        yaml_text = isolate(rv$yaml_text),
        structure = isolate(rv$structure),
        active = isolate(rv$active),
        uploads = isolate(rv$uploads),
        last_delivery = isolate(rv$last_delivery),
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
    # Superseded table files go only once nothing on disk refers to them.
    if (!inherits(saved, "try-error")) store$commit()
  }

  # Tell the user a document that just arrived (load, example, template,
  # restore, applied YAML) still has spec-check findings. Errors cannot get this
  # far on the load paths -- the readers refuse them -- so this is the warning
  # channel; the full list stays in the "Spec check" panel under the Raw YAML
  # editor (output$spec_check), which follows rv$dta.
  notify_spec_check <- function(dta, what = "Spec check") {
    f <- dta_lint(dta)
    if (nrow(f) == 0) {
      return(invisible(f))
    }
    n_err <- sum(f$severity == "error")
    showNotification(
      sprintf(
        "%s: %d error(s), %d warning(s) — see the Raw YAML tab.",
        what, n_err, nrow(f) - n_err
      ),
      type = if (n_err > 0) "error" else "warning", duration = 10
    )
    invisible(f)
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
    # A delivery belongs to the document it was loaded into.
    rv$last_delivery <- NULL
    rv$main_view <- "dataset"
    rv$status <- stats::setNames(rep("pending", length(names_ds)), names_ds)
    rv$active <- if (length(names_ds) > 0) names_ds[1] else NULL
    rv$dataset_only <- isTRUE(dataset_only)
    rv$is_example <- isTRUE(is_example)
    rv$md_token <- rv$md_token + 1
    rv$contacts_token <- rv$contacts_token + 1
    rv$doc_token <- rv$doc_token + 1
    # `versioned` is TRUE for a document LOADED from an existing one (upload,
    # bundled example) as opposed to one just created from a template -- see
    # rv$version_locked records that no version has been created yet, and a
    # baseline is captured only for such a document. rv$version_baseline_yaml
    # is captured from `yaml_text`
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
    # The two callers that CREATE a document in this session -- "Create new"
    # and template creation -- pass TRUE. A document the author just made
    # should be editable right away rather than costing a trip through the
    # Edit menu, and an empty one especially so, since output$add_dataset_ui
    # is gated purely on editing() and offers no "+ Add dataset" control
    # until edit mode is on. A document LOADED from an existing one keeps
    # the default and arrives read-only; it can still be unlocked from the
    # Edit menu without creating a version -- rv$version_locked does not
    # affect whether editing is possible.
    rv$editing <- isTRUE(start_editing) && cfg$features$editing
    rv$yaml_msg <- NULL
    autosave()
    notify_spec_check(dta)
  }

  # Collect option values from the currently-open template modal.
  #
  # A boolean option is a checkboxInput, so its value arrives as TRUE/FALSE.
  # The old Yes/No dropdown's "yes" is still accepted: it costs one comparison
  # and covers a restored autosave, or any caller still setting the input by
  # its former string value.
  #
  # Everything else arrives as the literal string the option's field holds --
  # picked from its suggestions or typed in place, indistinguishable by
  # design, which is why there is no "__custom__" sentinel to read. Only a
  # strict dropdown's "(leave blank)" row still needs translating: it is a
  # real, selectable row precisely so that "no value" is a deliberate choice
  # rather than an empty box the author may just not have reached yet.
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
        val <- isTRUE(val) || identical(as.character(val %||% ""), "yes")
      } else if (identical(as.character(val %||% ""), "__blank__")) {
        val <- ""
      }
      out[[oid]] <- val
    }
    out
  }

  # Collect a user's slot -> profile-id choices from the currently-open
  # options modal. Mirrors collect_template_selections() exactly: an unset
  # control, or one still on the template's own party (the "" row), contributes
  # NOTHING to the returned list -- apply_party_selections() (party_profiles.R)
  # already treats a missing entry as "leave this slot's target untouched".
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

  # The vocabulary-slot counterpart of collect_party_selections(), with one
  # deliberate difference: an EMPTIED control is recorded as character(0), not
  # omitted. The control opens holding the slot's `default:`, so whatever it
  # holds at "Create DTA" is the author's answer -- and the "None" link above
  # it would be a lie if clearing the chips silently put the default back.
  # vocabulary_slot_values() reads character(0) as "no terms", and refuses it
  # when `min:` says so, which is exactly what the emptied control shows.
  #
  # Only slots that actually rendered a control are read: the records
  # show_template_options_modal() resolved when the dialog opened. A slot whose
  # vocabulary could not be resolved has no control and is left out, so
  # creation still falls back to its default and fails naming the vocabulary,
  # rather than reporting "0 selected" against a `min:`.
  collect_vocab_selections <- function() {
    out <- list()
    for (rec in rv$template_vocab_specs$slots %||% list()) {
      if (!is.null(rec$error)) next
      val <- as.character(input[[paste0("tmpl_vocab_", rec$slot$id)]] %||% character(0))
      out[[rec$slot$id]] <- val[!is.na(val) & nzchar(val)]
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

  # Turn the carry-over controls (tmpl_carry_source/_file/_fields, rendered by
  # template_options_modal_body() in R/template_ui.R) into the `carry_over`
  # argument create_dta_from_template() expects, or a clear error for the
  # confirm
  # handler to surface without closing the modal. "none" -- the only option
  # offered when no document is open -- is not an error: it means create the
  # document with no ancestor at all, same as a template with no carry-over
  # feature ever had.
  observeEvent(input$tmpl_carry_file, {
    rv$tmpl_carry_upload <- input$tmpl_carry_file
  })

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
      f <- rv$tmpl_carry_upload
      if (is.null(f) || is.null(f$datapath) || !nzchar(f$datapath)) {
        return(list(ok = FALSE, error = paste("Choose a", cfg$document$abbr, "YAML file to carry metadata over from.")))
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
  # template_create.R) and its vocabularies (vocabulary_resolver(),
  # vocabulary.R) -- NULL only for a caller that never went through the index
  # at all, which no longer happens from this modal, but is kept optional so
  # the function degrades to "no party slots offered" rather than erroring if
  # it ever is.
  #
  # `loaded` is the WHOLE load_template_definition() result rather than just
  # its `def`: the modal's header names the exact id@version, the source it
  # came from and the resolved `extends` chain the author is about to
  # instantiate, and none of those survive into the merged definition.
  #
  # The body itself is template_options_modal_body() (R/template_ui.R), which
  # is a pure function of its arguments and tested as one. What has to stay
  # here is the part that needs the server: one renderText per vocabulary
  # slot, so the "n of m selected" count follows the control as the author
  # edits it -- the quick-action links (DTA_vocabSet, head script above) set
  # the selectize value client-side, Shiny reports the new input, and the
  # count re-renders from it.
  #
  # Re-registering those outputs on every open is deliberate, not a leak:
  # assigning to output[[id]] REPLACES the previous render rather than adding
  # a second one, so the set never grows, and a slot belonging to a template
  # the author has since backed out of simply stops being rendered.
  show_template_options_modal <- function(loaded, index, variant_selections = NULL) {
    def <- loaded$value$def
    rv$template_variant_selections <- variant_selections %||% list()
    # A re-rendered fileInput keeps its old server-side value; the carry-over
    # upload lives in rv instead and starts empty on every open.
    rv$tmpl_carry_upload <- NULL
    if (!is.null(def$variants)) {
      if (is.null(variant_selections)) {
        showModal(modalDialog(
          title = paste0("Create from template: ", def$label),
          p("Choose the platform before configuring the agreement. It determines the record schema and applicable suggestions."),
          template_variant_input(def),
          footer = tagList(
            actionButton("template_options_back", "Back to templates"),
            modalButton("Cancel"),
            actionButton("template_variant_confirm", "Continue", class = "btn btn-primary")
          ),
          easyClose = FALSE
        ))
        return(invisible(NULL))
      }
      def <- resolve_template_variant(def, variant_selections)
    }
    vocab_specs <- template_vocab_slot_specs(def, index)
    # Kept for collect_vocab_selections(): which slots have a control to read.
    rv$template_vocab_specs <- vocab_specs

    for (record in vocab_specs$slots %||% list()) {
      # A slot whose vocabulary could not be resolved renders its error
      # instead of a control, so there is no input to count and no output id
      # for the body to have placed.
      if (!is.null(record$error)) next
      # local() so each renderText closes over ITS OWN slot: without it every
      # one of them would read the loop variable's final value.
      local({
        rec <- record
        iid <- paste0("tmpl_vocab_", rec$slot$id)
        codes <- vapply(rec$items, function(it) as.character(it$value), character(1))
        output[[paste0(iid, "_count")]] <- renderText({
          # What the control holds is what will be applied (see
          # collect_vocab_selections()), so that is what the count describes.
          # An emptied multi-select reaches the server as NULL, the same as
          # one not yet bound: both are "0 of n" -- and the control opens
          # holding the default, so the count never says 0 while chips show.
          sel <- as.character(input[[iid]] %||% character(0))
          sel <- sel[!is.na(sel) & nzchar(sel)]
          # Terms typed by the author in an `open` slot are counted
          # separately: "2 of 5" alone would silently under-report a
          # selection that is in fact larger than the vocabulary offers.
          n_known <- sum(sel %in% codes)
          n_custom <- length(sel) - n_known
          txt <- sprintf("%d of %d selected", n_known, length(codes))
          if (n_custom > 0) paste0(txt, sprintf(", %d custom", n_custom)) else txt
        })
      })
    }

    # The platform chosen in the step before is context for the whole form, so
    # it sits in the sidebar with the template it refines.
    variant_ui <- if (!is.null(def$variants)) {
      div(
        class = "tmpl-aside-block",
        div(class = "section-label", as.character(def$variants$label %||% def$variants$id)),
        p(class = "tmpl-aside-variant", as.character(variant_selections[[def$variants$id]] %||% "")),
        actionButton("template_variant_back", "Change platform",
          class = "btn btn-sm btn-outline-secondary"
        ),
        div(class = "msg-hint", "Changing it resets the agreement options.")
      )
    }
    dlg <- modalDialog(
      title = tagList(
        span(class = "tmpl-eyebrow", paste("New", cfg$document$abbr, "from template")),
        as.character(def$label %||% def$id %||% "template")
      ),
      template_options_modal_body(
        def, loaded, index, vocab_specs,
        has_open_doc = !is.null(rv$dta), aside_extra = variant_ui,
        doc_abbr = cfg$document$abbr
      ),
      footer = tagList(
        actionButton("template_options_back", "← Back to templates",
          class = "btn btn-link tmpl-back"
        ),
        modalButton("Cancel"),
        actionButton("template_create_confirm", paste("Create", cfg$document$abbr), class = "btn btn-primary")
      ),
      size = "xl",
      easyClose = FALSE
    )
    # Bootstrap's scrollable dialog keeps the title and the buttons in view
    # while the form scrolls; modalDialog() has no argument for it.
    showModal(
      htmltools::tagQuery(dlg)$find(".modal-dialog")$addClass(
        "modal-dialog-scrollable tmpl-options-dialog"
      )$allTags()
    )
  }

  # --- landing: load a DTA YAML ------------------------------------------
  observeEvent(input$dta_file, {
    f <- input$dta_file
    req(f)
    txt <- tryCatch(paste(readLines(f$datapath, warn = FALSE), collapse = "\n"),
      error = function(e) NULL
    )
    finish_file_load(dta_read_yaml(f$datapath), txt, f$name)
  })

  # A load that succeeds replaces the document; one that fails changes NOTHING
  # (document, autosave, version state) and opens a dialog that lists every
  # finding beside the file's text, so the author can correct it and load it.
  # Both the upload and the dialog's "Load corrected YAML" end here, so a
  # corrected file is loaded exactly like a file that was good to begin with.
  finish_file_load <- function(res, txt, name) {
    if (!res$ok) {
      showNotification(
        paste("Could not load", cfg$document$abbr, "YAML:", res$error),
        type = "error", duration = 10
      )
      if (!is.null(txt) && nzchar(trimws(txt))) {
        nonce <- (isolate(rv$load_failure$nonce) %||% 0) + 1
        rv$load_failure <- list(
          name = name, text = txt, error = res$error,
          findings = res$findings, nonce = nonce
        )
        showModal(dta_load_failure_modal(rv$load_failure))
      }
      return(invisible(FALSE))
    }
    rv$load_failure <- NULL
    removeModal()
    apply_loaded(res$value, txt,
      dataset_only = isTRUE(res$dataset_only),
      wrapped_dataset = isTRUE(res$wrapped_dataset),
      versioned = TRUE
    )
    showNotification(
      if (isTRUE(res$wrapped_dataset)) {
        paste0("Dataset loaded into a new ", cfg$document$abbr, " \u2014 add metadata to complete it.")
      } else {
        paste(cfg$document$abbr, "loaded.")
      },
      type = "message"
    )
    invisible(TRUE)
  }

  observeEvent(input$load_corrected_yaml, {
    lf <- rv$load_failure
    req(lf)
    txt <- input[[paste0("load_failure_text_", lf$nonce)]] %||% lf$text
    if (!nzchar(trimws(txt))) {
      showNotification("The text is empty — nothing to load.", type = "warning")
      return()
    }
    finish_file_load(dta_read_yaml_text(txt), txt, lf$name)
  })

  observeEvent(input$load_example, {
    req(cfg$features$load_example)
    files <- dta_example_yaml_files()
    if (length(files) == 0) {
      showNotification("No example specifications found.", type = "error")
      return()
    }
    showModal(modalDialog(
      title = paste("Load example", cfg$document$abbr),
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
    req(isTRUE(cfg$features$load_example))
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
  # (on_landing(), defined above). The button only EXISTS there, but its input
  # id outlives the landing DOM, so a delayed or duplicated websocket message
  # could otherwise silently replace a document the user has already loaded
  # and edited. Autosave would leave the last snapshot on disk, but everything
  # done since it is gone, so the cheap guard is worth it even though the
  # message is unlikely.
  #
  # The app's other landing-only inputs -- the YAML upload, the example
  # loader, the two template steps, the restore button -- deliberately do NOT
  # carry this guard, and the asymmetry is a decision rather than an
  # oversight. None of them is reachable from the workspace either, so the
  # guard would not fire for any of them today; and req() cancels SILENTLY, so
  # a future route into one of them from the workspace (drag-and-drop onto an
  # open document, an "open recent" entry) would then do nothing at all, with
  # no error to say why. If it ever has to hold for all of them, on_landing()
  # is the seam to widen -- and the tests that drive those inputs twice in one
  # session would need a confirm_reset() between the two, which is the only
  # sequence a user can actually perform.

  observeEvent(input$create_new, {
    req(on_landing(), cfg$features$create_blank)
    rv$create_new_msg <- NULL
    rv$create_new_token <- rv$create_new_token + 1
    showModal(modalDialog(
      title = paste("Create a new", cfg$document$abbr),
      uiOutput("create_new_body"),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("create_new_confirm", paste("Create", cfg$document$abbr), class = "btn btn-primary")
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
      # This field cannot be called "create_new_version": that id already
      # belongs to the Edit menu's "Create new version" row (the
      # observeEvent(input$create_new_version) handler above). Two controls
      # sharing one input id means typing a version here also fires that
      # row's version-bump observer, and Shiny logs "Duplicate input ID was
      # found" on every use of this modal.
      textInput("create_new_version_value", "Version", value = "1.0", width = "100%"),
      div(
        class = "msg-hint",
        HTML(paste0("The new ", htmltools::htmlEscape(cfg$document$abbr), " starts with <b>no datasets</b>. Add them with <b>+ Add dataset</b> once the workspace opens, and fill in the rest of the metadata on the Metadata tab."))
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
    req(on_landing(), isTRUE(cfg$features$create_blank))
    title <- trimws(as.character(input$create_new_title %||% "")[1])
    version <- trimws(as.character(input$create_new_version_value %||% "")[1])
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
      sprintf("New %s \u201c%s\u201d created.", cfg$document$abbr, title),
      type = "message"
    )
  })


  # --- landing: create new DTA from a template ----------------------------
  #
  # Two-step modal, both steps backed by dta_template_index_cached() (template_
  # index.R) rather than the legacy list_dta_creation_templates()/get_dta_
  # creation_template_path() pair -- those still exist (template_core.R) but
  # have no remaining callers in the app; template files are read through
  # read_dta_creation_template() and load_template_definition(). That is what
  # makes a configured private source (template_sources.R) visible here the
  # same way
  # the packaged demo always was, with no separate code path for either:
  #
  #   1. show_template_picker_modal() -- pick a template and a version.
  #   2. show_template_options_modal() -- fill in options, party slots,
  #      vocabularies and metadata carry-over for the chosen template@version.
  #
  # Step 1 is THREE outputs rather than one, and that split is the design:
  #
  #   template_picker_ui      the frame -- stale-source lines, diagnostics,
  #                           the search box, the library filter and the
  #                           two-pane body. It depends on the index and the
  #                           refresh tick ONLY, and reads the search text and
  #                           the library choice through isolate(), so typing
  #                           never re-renders the box being typed into (which
  #                           would move the caret) and clicking a row never
  #                           re-renders the list under the pointer.
  #   template_picker_list    the rows. Re-renders on a keystroke or a source
  #                           change -- exactly what should, and nothing else.
  #   template_picker_detail  the version dropdown, the resolved `extends`
  #                           chain and what the template will build, for
  #                           whichever row is selected.
  #
  # Which row IS selected lives in reactives rather than being re-derived at
  # each use, because three different things need the same answer -- the
  # detail panel, "Use this template", and the fallback when the user has
  # typed a search but not clicked anything, where the first visible row is
  # implied. "Back to templates" relies on the same reactives still holding
  # after the options modal is dismissed.
  #
  # template_picker_loaded() resolves the definition as soon as a row is
  # selected, not when the button is pressed. That is what lets the detail
  # panel name the exact resolved lineage, and report a template that cannot
  # be used, BEFORE the author commits to it. It costs one YAML read plus a
  # merge, so arrowing through a family stays instant.

  show_template_picker_modal <- function() {
    # Shiny's client keeps the LAST value of every output and re-inserts it the
    # moment the output binds again, before any fresh render arrives -- and
    # then reports the inputs inside that stale copy as if the user had just
    # set them. Left alone, re-opening this modal after Cancel or "Back to
    # templates" re-inserts a frame whose search box holds the text of the
    # previous render and a list whose checked radio is whichever row was
    # checked when the list was last rendered, and the server takes both as
    # new input: the search typed before "Use this template" gone, the
    # selection reverted. Seen happen, not theorised.
    #
    # So the picker's outputs render NOTHING while the modal is closed (a
    # hidden.bs.modal listener in template_picker_scripts() reports the close
    # as input$template_picker_closed), which leaves nothing stale to
    # re-insert, and every open re-renders them fresh from the inputs Shiny is
    # still holding. The tick covers an open while already "open" -- a close
    # whose report never arrived.
    #
    # The search box is the one input that must NOT come back. The server
    # owns the search text (tmpl_search_text) so that an open starts it empty
    # no matter what the browser last reported, and no close report is needed.
    tmpl_picker_open(TRUE)
    tmpl_search_text("")
    tmpl_picker_open_tick(tmpl_picker_open_tick() + 1)
    dlg <- modalDialog(
      title = "Create new from template",
      # Static, so the dialog has its picker width (theme.R) from the first
      # frame instead of jumping once the output arrives.
      div(
        class = "tmpl-picker-modal",
        p("Pick a template, then fill in its options in the next step.", class = "msg-hint"),
        uiOutput("template_picker_ui")
      ),
      # Refresh is housekeeping, not a step: it sits apart on the left, where
      # it is there on every state of the picker, the empty one included.
      footer = tagList(
        actionButton(
          "tmpl_refresh_templates", "Refresh templates",
          class = "btn btn-link tmpl-refresh me-auto"
        ),
        modalButton("Cancel"),
        actionButton("template_select_next", "Use this template", class = "btn btn-primary")
      ),
      size = "xl",
      easyClose = TRUE
    )
    # Scrollable, like the options dialog, so it fills the window height and
    # the list grows with it (theme.R) while the buttons stay in view.
    showModal(
      htmltools::tagQuery(dlg)$find(".modal-dialog")$addClass(
        "modal-dialog-scrollable tmpl-picker-dialog"
      )$allTags()
    )
  }

  # Bumped by "Refresh templates" (below) to force the picker outputs to
  # recompute even though dta_template_index_cached() alone would not be seen
  # as "changed" by the reactive graph -- it is a plain function call backed by
  # a file-local cache (template_index.R), not a reactive value.
  tmpl_refresh_tick <- reactiveVal(0)

  # Bumped by show_template_picker_modal() on every open -- see the comment
  # there for why a re-bound modal cannot be trusted to show current inputs.
  tmpl_picker_open_tick <- reactiveVal(0)

  # Whether the picker modal is on screen. While it is not, its three outputs
  # render NOTHING -- same comment. The close is reported by the browser
  # (template_picker_scripts(), template_ui.R), because Cancel, Escape and a
  # click on the backdrop never reach the server any other way.
  tmpl_picker_open <- reactiveVal(FALSE)
  observeEvent(input$template_picker_closed, tmpl_picker_open(FALSE))

  # What the picker filters by. input$template_search keeps its last value
  # after the dialog closes, so it is only copied in while the box is typing;
  # an open resets this to "" and the freshly rendered, empty box then
  # reports "" itself. Priority: the Enter key sends the text just before it
  # clicks "Use this template", and that click must see it.
  tmpl_search_text <- reactiveVal("")
  observeEvent(input$template_search, tmpl_search_text(input$template_search %||% ""),
    ignoreNULL = FALSE, priority = 10
  )

  # The template "Back to templates" must land on: the one whose options were
  # just open. With the search gone, an implied selection (search + Enter, no
  # click) would otherwise fall back to the first row. Dropped as soon as the
  # list reports a selection, and on every fresh open.
  tmpl_back_id <- reactiveVal(NULL)
  observeEvent(input$template_select_name, tmpl_back_id(NULL),
    ignoreInit = TRUE, priority = 10
  )

  # One row per distinct, non-abstract creation-template id (template_picker_
  # entries(), template_index.R), plus the family/depth/lineage/search columns
  # that make a deviation template visible AS a deviation of its base
  # (template_picker_lineage(), same file).
  template_picker_rows <- reactive({
    tmpl_refresh_tick()
    idx <- dta_template_index_cached()
    template_picker_lineage(idx, template_picker_entries(idx))
  })

  # The library filter is a multi-select, and only the names in it that still
  # have templates count -- template_picker_restrict_sources() (template_
  # index.R) says why, and why the tree is rebuilt rather than filtered. Its
  # own reactive, so a keystroke in the search box re-filters without
  # rebuilding the tree.
  template_picker_scoped <- reactive({
    template_picker_restrict_sources(
      dta_template_index_cached(), template_picker_rows(),
      input$template_select_source
    )
  })

  template_picker_visible <- reactive({
    template_picker_filter(template_picker_scoped(), tmpl_search_text())
  })

  # The user's own choice for as long as it names a row that is still visible,
  # else the first visible row. Falling back rather than clearing is what lets
  # "type a search, press Enter" work with no click in between -- and it is
  # also why a search that hides everything yields NULL, so "Use this
  # template" warns instead of silently creating from a row nobody can see.
  template_picker_selected <- reactive({
    vis <- template_picker_visible()
    if (nrow(vis) == 0) {
      return(NULL)
    }
    chosen <- as.character(tmpl_back_id() %||% input$template_select_name %||% "")
    if (nzchar(chosen) && chosen %in% vis$id) chosen else vis$id[[1]]
  })

  # Same rule one level down: the chosen version while it was chosen FOR this
  # id, else the newest. The version control is re-rendered per template, but
  # Shiny still holds the previous template's choice until the new control
  # reports, and a "1.0" chosen on one template must not carry onto the next
  # -- which may well have a 1.0 of its own -- and silently open an old
  # version nobody picked. So the id a version was chosen for is recorded
  # alongside it, ahead of the outputs that read it (priority).
  tmpl_version_for <- reactiveVal(NULL)
  observeEvent(input$template_select_version,
    {
      tmpl_version_for(isolate(template_picker_selected()))
    },
    priority = 10
  )

  template_picker_version <- reactive({
    id <- template_picker_selected()
    if (is.null(id)) {
      return(NULL)
    }
    vers <- template_id_versions(dta_template_index_cached(), id)
    v <- as.character(input$template_select_version %||% "")
    if (nzchar(v) && v %in% vers && identical(tmpl_version_for(), id)) {
      v
    } else if (length(vers) > 0) {
      vers[[1]]
    } else {
      NULL
    }
  })

  template_picker_loaded <- reactive({
    id <- template_picker_selected()
    v <- template_picker_version()
    if (is.null(id) || is.null(v)) {
      return(NULL)
    }
    load_template_definition(paste0(id, "@", v), index = dta_template_index_cached())
  })

  output$template_picker_ui <- renderUI({
    tmpl_refresh_tick()
    tmpl_picker_open_tick()
    if (!isTRUE(tmpl_picker_open())) {
      return(NULL)
    }
    idx <- dta_template_index_cached()
    sources <- attr(idx, "sources") %||% list()
    rows <- template_picker_rows()

    diagnostics <- template_source_diagnostics_ui(sources, attr(idx, "layout_warnings"))

    if (nrow(rows) == 0) {
      # Private-replaces-public (template_sources.R): with a private source
      # configured and nothing usable in it, the packaged demo is NOT offered
      # as a fallback -- say so plainly, backed by the diagnostics above
      # naming exactly which source(s) failed and why. When the sources loaded
      # but hold no template files, the usual cause is the folder layout.
      all_failed <- length(sources) > 0 &&
        !any(vapply(sources, function(s) isTRUE(s$ok), logical(1)))
      msg <- if (dta_template_private_configured() && all_failed) {
        p(
          class = "msg-hint",
          paste(
            "No templates are available: every configured private template",
            "source failed to load, and the packaged demo template is not",
            "used as a fallback while a private source is configured."
          )
        )
      } else {
        p(
          class = "msg-hint",
          paste(
            "No creation templates were found. A template library keeps its",
            "files in templates/, parties/ and vocabulary/ folders (Word",
            "references in export-templates/, dta-app.yaml at the root) --",
            "check the folder layout of the template source."
          )
        )
      }
      return(tagList(diagnostics, msg))
    }

    # Search and libraries on one toolbar; under it the list and the
    # selected template side by side (stacked on a narrow screen, theme.R), so
    # what was picked is in view the moment it is picked.
    div(
      class = "tmpl-picker",
      tagList(lapply(sources, template_source_status_row)),
      diagnostics,
      div(
        class = "tmpl-search-row",
        template_picker_search_ui(isolate(tmpl_search_text())),
        template_picker_source_ui(
          sort(unique(rows$source_name), method = "radix"),
          isolate(input$template_select_source),
          counts = c(table(rows$source_name))
        )
      ),
      div(
        class = "tmpl-picker-body",
        div(class = "tmpl-picker-main", uiOutput("template_picker_list")),
        div(class = "tmpl-picker-side", uiOutput("template_picker_detail"))
      ),
      template_picker_scripts()
    )
  })

  # `selected` is isolated for the same reason the frame isolates the search
  # text: a click checks the radio in the browser already, so re-rendering the
  # list to say so would only throw away the focus and scroll position the
  # user just established -- and would fight the client-side expand/collapse.
  output$template_picker_list <- renderUI({
    tmpl_picker_open_tick()
    if (!isTRUE(tmpl_picker_open())) {
      return(NULL)
    }
    rows <- template_picker_rows()
    vis <- template_picker_visible()
    tokens <- template_picker_tokens(tmpl_search_text())
    template_picker_list_ui(
      vis,
      selected = isolate(tmpl_back_id() %||% input$template_select_name),
      show_source = length(unique(rows$source_name)) > 1,
      # A search result is a flat list: the matches rarely form whole
      # families, and indenting a child under a parent that was filtered out
      # would place it under nothing. Its `extends` line still names the
      # ancestors, so the context survives the flattening.
      flat = length(tokens) > 0,
      tokens = tokens,
      total = nrow(rows)
    )
  })

  output$template_picker_detail <- renderUI({
    if (!isTRUE(tmpl_picker_open())) {
      return(NULL)
    }
    id <- template_picker_selected()
    if (is.null(id)) {
      return(NULL)
    }
    idx <- dta_template_index_cached()
    template_picker_detail_ui(
      template_picker_loaded(),
      template_id_versions(idx, id),
      template_picker_version(),
      idx
    )
  })

  observeEvent(input$tmpl_refresh_templates, {
    dta_template_index_invalidate()
    tmpl_refresh_tick(tmpl_refresh_tick() + 1)
  })

  observeEvent(input$create_from_template, {
    req(cfg$features$create_from_template)
    idx <- dta_template_index_cached()
    entries <- template_picker_entries(idx)
    if (nrow(entries) == 0 && !dta_template_private_configured()) {
      # No private source configured AND nothing packaged/local either --
      # exactly today's pre-index behaviour: a notification, no modal.
      showNotification(
        paste(
          "No creation templates found. Add *.dta-template.yaml files to the",
          "templates/ folder of a library at ./dta-templates, or point",
          "options(DTAtools.template_dir=) at a library directory of your own",
          "(templates/, parties/, vocabulary/ and export-templates/ folders,",
          "dta-app.yaml at its root)."
        ),
        type = "warning", duration = 8
      )
      return()
    }
    tmpl_back_id(NULL)
    show_template_picker_modal()
  })

  observeEvent(input$template_select_next, {
    req(isTRUE(cfg$features$create_from_template))
    # The definition the detail panel is already showing -- NULL only when no
    # row is visible to select, which is what a search matching nothing looks
    # like from here.
    loaded <- template_picker_loaded()
    if (is.null(loaded)) {
      showNotification("Pick a template first.", type = "warning")
      return()
    }
    if (!isTRUE(loaded$ok)) {
      showNotification(paste("Template is invalid:", loaded$error), type = "error", duration = 10)
      return()
    }
    # Frozen HERE, not re-fetched at confirm time: the options/party/carry-
    # over step must build and create against the EXACT index the user picked
    # from, even if a background "Refresh templates" happens while that step
    # is open. The ref comes off the resolved row rather than off the inputs,
    # so an implied selection (first visible row, never clicked) is recorded
    # as precisely as an explicit one.
    rv$template_ref <- paste0(loaded$value$id, "@", loaded$value$version)
    rv$template_index <- dta_template_index_cached()
    removeModal()
    show_template_options_modal(loaded, rv$template_index)
  })

  # The "render NOTHING while closed" rule above only works if the render
  # actually runs, and a suspended output does not run. An output whose
  # element has left the page can be left suspended -- it is reported hidden
  # the moment its container binds behind a fade, and nothing un-hides an
  # element that is gone -- and then the NULL is held back until the output is
  # visible again, by which time the stale copy has already been re-inserted
  # and its inputs reported (seen happen). Keeping the three live costs
  # nothing: they ARE null whenever they are off screen.
  outputOptions(output, "template_picker_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "template_picker_list", suspendWhenHidden = FALSE)
  outputOptions(output, "template_picker_detail", suspendWhenHidden = FALSE)

  # Back to step 1. show_template_picker_modal() re-renders the picker's three
  # outputs from the inputs Shiny is still holding, so the selection comes
  # back with them -- dismissing a modal removes controls from the DOM, it
  # does not clear their input values. The search text is the exception: every
  # open starts it empty (tmpl_search_text), so the selection is pinned to the
  # template whose options were open instead.
  observeEvent(input$template_options_back, {
    removeModal()
    show_template_picker_modal()
    tmpl_back_id(sub("@.*$", "", rv$template_ref %||% ""))
  })

  observeEvent(input$template_variant_back, {
    req(rv$template_ref)
    loaded <- load_template_definition(rv$template_ref, index = rv$template_index)
    if (!isTRUE(loaded$ok)) {
      showNotification(loaded$error, type = "error", duration = 10)
      return()
    }
    show_template_options_modal(loaded, rv$template_index)
  })

  observeEvent(input$template_variant_confirm, {
    req(rv$template_ref)
    loaded <- load_template_definition(rv$template_ref, index = rv$template_index)
    if (!isTRUE(loaded$ok)) {
      showNotification(loaded$error, type = "error", duration = 10)
      return()
    }
    spec <- loaded$value$def$variants
    if (is.null(spec)) {
      return()
    }
    selections <- stats::setNames(list(input[[paste0("tmpl_variant_", spec$id)]]), spec$id)
    checked <- dta_try(resolve_template_variant(loaded$value$def, selections))
    if (!isTRUE(checked$ok)) {
      showNotification(checked$error, type = "error", duration = 10)
      return()
    }
    show_template_options_modal(loaded, rv$template_index, selections)
  })

  observeEvent(input$template_create_confirm, {
    req(isTRUE(cfg$features$create_from_template), rv$template_ref)
    idx <- rv$template_index

    loaded <- load_template_definition(rv$template_ref, index = idx)
    if (!isTRUE(loaded$ok)) {
      showNotification(paste("Could not load template:", loaded$error), type = "error", duration = 10)
      return() # modal stays open
    }
    def <- loaded$value$def
    resolved <- dta_try(resolve_template_variant(def, rv$template_variant_selections %||% list()))
    if (!isTRUE(resolved$ok)) {
      showNotification(resolved$error, type = "error", duration = 10)
      return()
    }
    def <- resolved$value

    sels <- c(rv$template_variant_selections %||% list(), collect_template_selections(def))
    party_sel <- collect_party_selections(def)
    vocab_sel <- collect_vocab_selections()

    co <- resolve_carry_over()
    if (!isTRUE(co$ok)) {
      showNotification(co$error, type = "error", duration = 8)
      return() # modal stays open
    }

    prov <- template_provenance(
      loaded$value$def, loaded$value,
      lineage = loaded$value$lineage,
      mixins = loaded$value$mixins,
      carried_over_from = co$carried_over_from
    )

    created <- create_dta_from_template(
      def, loaded$value$path, sels,
      index = idx, carry_over = co$carry_over,
      party_selections = party_sel, provenance = prov,
      vocab_selections = vocab_sel
    )
    if (!isTRUE(created$ok)) {
      showNotification(paste("Could not create", cfg$document$abbr, "from template:", created$error),
        type = "error", duration = 10
      )
      return() # LEAVE THE MODAL OPEN so the user does not lose their choices
    }

    yres <- dta_to_yaml_text(created$value)
    yaml_text <- if (isTRUE(yres$ok)) yres$value else ""
    removeModal()
    # versioned deliberately left at its default (FALSE): a template-created
    # document is NEW, not loaded from an existing one, so it is not gated
    # behind the "Create new version" flow. start_editing = TRUE is why the
    # author lands in edit mode straight away, on the strength of the one
    # autosave apply_loaded() already does, instead of arriving read-only and
    # needing a second, corrective persist.
    apply_loaded(created$value, yaml_text,
      dataset_only = FALSE, is_example = FALSE, wrapped_dataset = FALSE,
      start_editing = TRUE
    )
    showNotification(
      paste0("New ", cfg$document$abbr, " created from template \"", as.character(def$label %||% def$id %||% ""), "\"."),
      type = "message"
    )
  })

  # --- dataset navigation (custom list: select + per-dataset check icon) --
  output$dataset_nav_ui <- renderUI({
    req(rv$structure)
    names_ds <- names(rv$structure)
    # No dataset is highlighted while the Delivery view stands in for them.
    active <- if (identical(rv$main_view, "delivery")) NULL else rv$active
    st_map <- rv$status
    fnd <- findings_now()
    div(
      class = "dataset-nav",
      div(class = "section-label", "Datasets"),
      div(
        class = "dataset-nav-list",
        lapply(seq_along(names_ds), function(i) {
          nm <- names_ds[i]
          # A structure restored from a session saved before labels existed
          # has none; the id is the right fallback, as everywhere else.
          label <- rv$structure[[i]]$label %||% nm
          st <- dta_lookup(st_map, nm, "pending")
          # A content pass with a packaging finding is no pass: the row goes
          # amber (not verified) or red (outside its container), in words too.
          pk <- dv_ds_packaging(fnd, nm)
          # Row background + icon encode status: passed (green), failed (red),
          # missing/no-data (orange), not-checked-yet (neutral grey).
          st2 <- switch(status_display(st, pk),
            pass = "pass",
            fail = "fail",
            nodata = "nodata",
            "pending"
          )
          pk_pass <- identical(st, "pass") && !identical(pk, "none")
          ic_ch <- if (pk_pass) {
            if (identical(pk, "outside")) "\u26a0" else "\u25d0"
          } else {
            switch(st,
              pass = "\u2714",
              fail = "\u2716",
              nodata = "\u2716",
              "\u2013"
            )
          }
          ic_cls <- paste0("nav-ic-", if (st2 == "pending") "pending" else st2)
          ic_ttl <- if (pk_pass) {
            if (identical(pk, "outside")) "Content passed, but a file is not inside its container" else "Content passed; packaging not verified"
          } else {
            switch(st,
              pass = "Passed all checks",
              fail = "Validation failed",
              nodata = "No data loaded (missing data)",
              "Not validated yet"
            )
          }
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
                span(class = paste("nav-ic", ic_cls), title = ic_ttl, role = "img", `aria-label` = ic_ttl, ic_ch),
                span(
                  class = "nav-text",
                  span(class = "nav-name", label),
                  # The id stays visible under a name: it is what every
                  # validation message and download refers to.
                  if (!identical(label, nm)) span(class = "nav-id", nm)
                )
              )
            ),
            actionButton(
              paste0("checkds_", i),
              label = HTML("&#x25B6;"),
              class = "btn btn-sm nav-check",
              title = sprintf("Check '%s'", label)
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
              # Any click on the list leaves the Delivery view, also on the
              # dataset that is already active (rv$active does not change then).
              rv$main_view <- "dataset"
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
      textInput("add_ds_id", "ID",
        value = "", width = "100%",
        placeholder = "e.g. dm"
      ),
      div(
        class = "msg-hint", style = "margin:-8px 0 12px;",
        "Identifies the dataset in the specification, in every validation message and in load_file()."
      ),
      textInput("add_ds_name", "Name (optional)",
        value = "", width = "100%",
        placeholder = "e.g. Demographics"
      ),
      textAreaInput("add_ds_description", "Description (optional)",
        value = "", width = "100%", rows = 3,
        placeholder = "What this dataset contains, in words"
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
      id = input$add_ds_id,
      type = input$add_ds_type %||% "tabular",
      name = input$add_ds_name,
      description = input$add_ds_description
    )
    if (!isTRUE(r$ok)) {
      # The modal stays open with everything the user typed still in it, and
      # nothing in rv changes -- the same contract as meta_save() below.
      # Deliberately NOT bumping rv$add_ds_token here: that is add_ds_body's
      # only dependency, and re-rendering would reset every field to "" and
      # the type back to "tabular", making the user retype an ID they can see
      # is wrong rather than correct it in place. output$add_ds_msg reacts to
      # rv$add_ds_msg on its own, so the error still appears.
      rv$add_ds_msg <- list(ok = FALSE, error = r$error)
      return()
    }
    nm <- trimws(as.character(input$add_ds_id)[1])
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
    showNotification(
      sprintf("Dataset '%s' added.", ds_title(nm)),
      type = "message"
    )
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
        tags$p("Dataset ", tags$b(ds_title(ed)), " and its specification will be removed from the document."),
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
    ed_title <- ds_title(ed) # read while the dataset still exists
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
    showNotification(sprintf("Dataset '%s' removed.", ed_title), type = "message")
  })

  # --- upload observers (registered once per handler) ---------------------
  # Does the entry `h` take the container-named file `name` AS a file? Its name
  # must match the entry, and the entry's type must read that kind of file: a
  # .zip only a `type: any` entry, an .xpt a `type: xpt` or `type: any` one.
  takes_as_file <- function(h, name) {
    if (!isTRUE(handler_matches(h, name))) {
      return(FALSE)
    }
    type <- dta_handler_type(h)
    if (grepl("\\.zip$", name, ignore.case = TRUE)) identical(type, "any") else type %in% c("xpt", "any")
  }

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

    # A slot takes THIS dataset's files only. A .zip / .xpt its entry reads as a
    # file is a file; any other one in the drop is a delivery, and then the
    # WHOLE drop goes through load_delivery() for this dataset alone: the
    # archive is opened and only this dataset's files are taken from it.
    is_container <- dta_is_container_name(fileinfo$name)
    if (any(is_container) && !all(vapply(
      as.character(fileinfo$name)[is_container],
      function(nm) takes_as_file(h, nm), logical(1)
    ))) {
      handle_delivery(fileinfo, datasets = dsname, origin = "slot")
      session$sendCustomMessage("dta_reset_fileinput", sprintf("up_%d_%d", ds_idx, hi))
      return()
    }

    # A dropped file will occupy a table named after it (for tabular datasets,
    # dta_bound_item_name() strips the extension (a gz suffix first); for file
    # datasets, the basename with
    # extension is kept). This mapping drives overwrite detection and binds.
    ds_type <- tryCatch(ds@type, error = function(e) NA_character_)
    tbl_of <- function(nm) dta_bound_item_name(ds_type, nm)
    existing <- dta_dataset_table_names(ds) # dataset-wide bound items

    # Only files that can actually bind (non-empty, name matches the handler)
    # take part in the overwrite and count gates: a stray file that merely
    # shares a bound table's stem must neither prompt an overwrite nor remove
    # that table from the slot's count.
    candidate <- vapply(seq_len(nrow(fileinfo)), function(i) {
      sz <- suppressWarnings(as.numeric(fileinfo$size[i]))
      !is.na(sz) && sz > 0 && isTRUE(handler_matches(h, fileinfo$name[i]))
    }, logical(1))

    # Overwrite gate: if a dropped file targets an already-bound table and the
    # user has not confirmed, ask first -- never silently replace bound data.
    if (!isTRUE(overwrite)) {
      dropped_tbls <- vapply(as.character(fileinfo$name)[candidate], tbl_of, character(1))
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
    dropped_tbls <- vapply(as.character(fileinfo$name)[candidate], tbl_of, character(1))
    kept_after <- length(setdiff(slot_tbls, dropped_tbls))
    n_candidates <- sum(candidate)
    if (!is.na(mx) && (kept_after + n_candidates) > mx) {
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
      if (!identical(staged, dp)) own("upload", dirname(staged))
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
      # A table overwritten here may have been bound through ANOTHER handler
      # slot of this dataset: drop it from every slot's records, not just `key`.
      up_keys <- as.character(names(up))
      for (k in up_keys[startsWith(up_keys, paste0(dsname, "||"))]) {
        up[[k]] <- Filter(function(x) !((x$table %||% "") %in% new_tbls), up[[k]])
      }
      cur <- up[[key]] %||% list()
      up[[key]] <- c(cur, loaded_recs)
      rv$uploads <- up
      # A delivery's extracted file this upload just replaced is named by no
      # record any more.
      dta_delivery_cleanup(owned$upload, rv$uploads, session$token)
      st <- rv$status
      st[[dsname]] <- "pending"
      rv$status <- st
      autosave()
    }

    # ONE reconciled outcome with honest counts: never a
    # success message when nothing was actually bound.
    ow <- if (length(overwritten) > 0) {
      sprintf(" (%d overwritten)", length(overwritten))
    } else {
      ""
    }
    # A loose file bound into an entry that must arrive inside a container was
    # loaded to be checked on its own; saying so is the one place that tells
    # the user this is not yet a conforming delivery.
    glob <- if (length(loaded) > 0) dta_effective_glob(rv$dta, dsname, hi) else ""
    if (length(loaded) > 0 && length(rejected) == 0 && nzchar(glob)) {
      one <- length(loaded) == 1
      showNotification(
        sprintf(
          "Loaded %s %s for checking. The agreement requires %s inside %s, so this is not yet a conforming delivery.",
          paste(loaded, collapse = ", "), if (one) "on its own" else "on their own",
          if (one) "it" else "them", glob
        ),
        type = "warning", duration = 10
      )
    } else if (length(loaded) > 0 && length(rejected) == 0) {
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

  # A drop that holds a .zip / .xpt: load it whole into a COPY of the document
  # (load_delivery() routes every file across the datasets in scope), then either
  # commit it or ask first: which dataset a file is for when it matches several
  # (the ambiguity dialog), and once more when it would replace tables already
  # loaded. A copy waiting for Replace sits in rv$pending_upload$delivery until
  # Replace commits it or Cancel deletes its extraction directory.
  #
  # `datasets` limits the delivery to those datasets (a slot's drop); `route` is
  # the answers collected so far (file name -> dataset id, character(0) = do not
  # load); `staged` the already staged copies when this call continues a drop the
  # ambiguity dialog asked about; `origin` is "sidebar" or "slot".
  handle_delivery <- function(fileinfo, datasets = NULL, route = NULL, staged = NULL, origin = "sidebar") {
    if (is.null(staged)) {
      sz <- suppressWarnings(as.numeric(fileinfo$size))
      ok <- !is.na(sz) & sz > 0
      if (!any(ok)) {
        showNotification("The delivery is empty or was not transferred completely.", type = "error")
        return(invisible())
      }
      if (any(!ok)) {
        showNotification(sprintf(
          "Not loaded (empty or not transferred completely): %s",
          paste(fileinfo$name[!ok], collapse = ", ")
        ), type = "warning", duration = 10)
      }
      fileinfo <- fileinfo[ok, , drop = FALSE]
    }
    # Extracted files no record names any more go first, so the budget below
    # counts only what is in use, and "remove or discard, then drop again" works.
    dta_delivery_cleanup(owned$upload, rv$uploads, session$token)
    held <- dta_session_delivery_bytes(owned$upload)
    left <- dta_session_delivery_budget() - held
    if (left <= 0) {
      if (!is.null(staged)) unlink(dirname(staged[staged != fileinfo$datapath]), recursive = TRUE)
      showNotification(
        "This session already holds the maximum of extracted deliveries; remove or discard files loaded from a delivery and drop again, or use Start over.",
        type = "error", duration = 10
      )
      return(invisible())
    }
    if (is.null(staged)) {
      staged <- vapply(seq_len(nrow(fileinfo)), function(r) {
        s <- dta_stage_upload(fileinfo$datapath[r], fileinfo$name[r])
        if (!identical(s, fileinfo$datapath[r])) own("upload", dirname(s))
        s
      }, character(1))
    }
    copies <- staged[staged != fileinfo$datapath] # our own staged copies, never Shiny's upload file
    exdir <- tempfile("dta_upload_delivery_")
    own("upload", exdir)
    res <- withProgress(
      message = "Opening the delivery\u2026", value = 0.3,
      dta_load_delivery(rv$dta, staged, exdir, left, held, route = route, datasets = datasets)
    )

    # Files that match several datasets are a question, not a failure: a delivery
    # that binds nothing but ambiguous files aborts, and its table says so.
    plan <- if (isTRUE(res$ok)) attr(res$value, "last_delivery") else res$plan
    asked <- if (is.data.frame(plan)) plan[plan$status == "ambiguous" & !plan$name %in% names(route), , drop = FALSE] else NULL
    if (!is.null(asked) && nrow(asked) > 0) {
      # Nothing of this pass is kept: the staged copies wait, the answers decide
      # what is extracted. Each round adds at least one name to `route`.
      unlink(exdir, recursive = TRUE)
      nonce <- paste(sample(c(letters, 0:9), 8, replace = TRUE), collapse = "")
      rows <- dv_ambiguous_rows(asked)
      rv$amb_msg <- NULL
      rv$pending_delivery <- list(
        fileinfo = fileinfo, staged = staged, copies = copies, datasets = datasets,
        route = route, rows = rows, nonce = nonce, origin = origin
      )
      showModal(dv_ambiguity_modal(rv$dta, rows, nonce, editing = editing()))
      return(invisible())
    }

    if (!isTRUE(res$ok)) {
      unlink(c(exdir, dirname(copies)), recursive = TRUE)
      zero <- identical(origin, "slot") && is.data.frame(plan) && !any(plan$status %in% c("refused", "ambiguous")) &&
        !any(plan$status == "opened" & !is.na(plan$reason))
      if (zero) {
        archives <- unique(plan$delivered[dta_is_container_name(plan$delivered)])
        ds_id <- datasets[[1]]
        expected <- vapply(dta_handlers(dta_get_dataset(rv$dta, ds_id)), handler_expected, character(1))
        showNotification(sprintf(
          "%s holds no file for %s (expected %s). Nothing was loaded.",
          paste(archives, collapse = ", "), ds_title(ds_id), paste(expected, collapse = ", ")
        ), type = "warning", duration = 10)
      } else {
        showNotification(paste("Delivery not loaded:", dta_app_reason(res$error, isTRUE(res$budget_limited))), type = "error", duration = 10)
      }
      return(invisible())
    }
    d <- list(
      value = res$value, plan = attr(res$value, "last_delivery"), exdir = exdir,
      missing = res$missing, copies = copies, budget_limited = isTRUE(res$budget_limited),
      origin = origin, datasets = datasets,
      loose_names = as.character(fileinfo$name[!dta_is_container_name(fileinfo$name)])
    )
    replaced <- dta_delivery_replaced(rv$dta, d$plan)
    d$n_replaced <- nrow(replaced)
    if (nrow(replaced) > 0) {
      rv$pending_upload <- list(delivery = d)
      showModal(modalDialog(
        title = "Replace loaded files?",
        tags$p("This delivery replaces files that are already loaded:"),
        tags$ul(lapply(seq_len(nrow(replaced)), function(i) {
          tags$li(tags$code(replaced$item[i]), sprintf(" (dataset '%s')", replaced$dataset[i]))
        })),
        tags$p("The replaced tables will be marked as not validated."),
        footer = tagList(
          actionButton("cancel_overwrite", "Cancel"),
          actionButton("confirm_overwrite", "Replace", class = "btn btn-warning")
        ),
        easyClose = FALSE
      ))
      return(invisible())
    }
    commit_delivery(d)
  }

  # The one notification a delivery dropped on a dataset's own slot shows.
  slot_delivery_note <- function(d, over = character(0)) {
    plan <- d$plan
    if (!"conformant" %in% names(plan)) plan$conformant <- NA
    loaded <- plan[plan$status == "loaded", , drop = FALSE]
    ds_id <- d$datasets[[1]]
    archives <- unique(loaded$delivered[dta_is_container_name(loaded$delivered)])
    n_other <- sum(plan$status == "unused")
    text <- sprintf(
      "Loaded only %s's files from %s (%s).",
      ds_title(ds_id), paste(archives, collapse = ", "), paste(loaded$name, collapse = ", ")
    )
    if (n_other > 0) {
      text <- paste0(
        text, " Its other ", .ctr_count(n_other, "file", "files"), " ",
        if (n_other == 1) "was" else "were", " not loaded."
      )
    }
    misfit <- unique(loaded$name[!is.na(loaded$conformant) & !loaded$conformant])
    if (length(misfit) > 0) {
      text <- paste0(text, " Not inside its required container: ", paste(misfit, collapse = ", "), ".")
    }
    refused <- plan[plan$status == "refused", , drop = FALSE]
    if (nrow(refused) > 0) {
      why <- vapply(refused$reason, dv_reason, character(1), USE.NAMES = FALSE)
      text <- paste0(text, " Refused: ", paste0(refused$name, " (", why, ")", collapse = "; "), ".")
    }
    # D7: loose files that travel with the archive are judged as delivered.
    if (length(d$loose_names) > 0) {
      text <- paste0(text, " The loose files in this drop were judged as delivered; drop them alone to check them on their own.")
    }
    if (length(over) > 0) {
      text <- paste0(text, " More files than a slot accepts: ", paste(over, collapse = ", "), ".")
    }
    list(text = text, type = if (length(misfit) + nrow(refused) + length(over) > 0) "warning" else "message")
  }

  # Make a loaded delivery the document: assign it, record which file filled
  # which slot (with the archive it came from and the extracted path the cleanup
  # and the session budget work on), reset the status of every dataset it
  # touched, and tell the user once.
  commit_delivery <- function(d) {
    rv$dta <- d$value
    loaded <- d$plan[d$plan$status == "loaded", , drop = FALSE]
    up <- rv$uploads
    for (i in seq_len(nrow(loaded))) {
      ds <- loaded$dataset[i]
      tbl <- loaded$item[i]
      keys <- as.character(names(up))
      for (k in keys[startsWith(keys, paste0(ds, "||"))]) {
        up[[k]] <- Filter(function(x) !identical(x$table %||% "", tbl), up[[k]])
      }
      key <- paste0(ds, "||", loaded$handler[i])
      from <- !is.na(loaded$container[i])
      up[[key]] <- c(up[[key]] %||% list(), list(list(
        file = loaded$name[i], table = tbl,
        container = if (from) loaded$container[i],
        path = if (from) loaded$path[i]
      )))
    }
    rv$uploads <- up
    st <- rv$status
    for (ds in unique(loaded$dataset)) st[[ds]] <- "pending"
    rv$status <- st
    # Staged copies not bound as files (an opened archive), then every
    # extracted file no record names any more: the files this delivery
    # replaced, and whatever an earlier removal could not delete.
    unlink(dirname(setdiff(d$copies, loaded$path)), recursive = TRUE)
    dta_delivery_cleanup(owned$upload, up, session$token)
    # What the Delivery view and the sidebar's last-delivery line show.
    rv$last_delivery <- list(plan = d$plan, missing = d$missing, at = Sys.time())
    if (identical(d$origin %||% "sidebar", "sidebar")) {
      rv$main_view <- "delivery"
      # The Delivery view lives on the Datasets tab; the Metadata tab may be open.
      bslib::nav_select("main_tabs", "Datasets", session = session)
    }
    autosave()
    note <- if (identical(d$origin, "slot")) {
      slot_delivery_note(d, over = dta_slots_over_max(rv$dta, up))
    } else {
      dta_delivery_note(d$plan, d$n_replaced, d$missing, d$budget_limited,
        over = dta_slots_over_max(rv$dta, up)
      )
    }
    showNotification(note$text, type = note$type, duration = if (identical(note$type, "message")) 5 else 10)
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
    if (is.null(pu)) {
      return()
    }
    if (!is.null(pu$delivery)) commit_delivery(pu$delivery) else handle_upload(pu$ds_idx, pu$hi, pu$fileinfo, overwrite = TRUE)
  })
  observeEvent(input$cancel_overwrite, {
    pu <- rv$pending_upload
    rv$pending_upload <- NULL
    removeModal()
    if (!is.null(pu$delivery)) unlink(c(pu$delivery$exdir, dirname(pu$delivery$copies)), recursive = TRUE)
  })

  # --- delivery: the sidebar's drop zone and the ambiguity dialog ----------
  observeEvent(input$delivery_files,
    {
      req(rv$dta)
      handle_delivery(input$delivery_files)
      session$sendCustomMessage("dta_reset_fileinput", "delivery_files")
    },
    ignoreInit = TRUE
  )

  # The dialog's inline error. It renders NULL while no delivery is asking, and
  # is never suspended: a modal's output keeps the client's last HTML, so a
  # stale message would otherwise greet the next dialog.
  output$amb_error <- renderUI({
    pd <- rv$pending_delivery
    if (is.null(pd) || is.null(rv$amb_msg)) {
      return(NULL)
    }
    div(class = "text-danger", role = "alert", rv$amb_msg)
  })
  outputOptions(output, "amb_error", suspendWhenHidden = FALSE)

  observeEvent(input$amb_load, {
    pd <- rv$pending_delivery
    req(pd)
    answers <- vapply(seq_along(pd$rows), function(i) {
      a <- input[[sprintf("amb_%s_%d", pd$nonce, i)]]
      if (is.null(a) || length(a) != 1 || is.na(a)) NA_character_ else as.character(a)
    }, character(1))
    if (anyNA(answers)) {
      rv$amb_msg <- "Choose a dataset or Don't load for every file."
      return()
    }
    names_asked <- vapply(pd$rows, function(r) r$name, character(1))
    # A LIST: a character vector cannot hold character(0), the answer "no dataset".
    route <- c(
      pd$route %||% list(),
      stats::setNames(lapply(answers, function(a) if (nzchar(a)) a else character(0)), names_asked)
    )
    removeModal()
    rv$pending_delivery <- NULL
    rv$amb_msg <- NULL
    handle_delivery(pd$fileinfo, pd$datasets, route, pd$staged, pd$origin)
  })

  observeEvent(input$amb_cancel, {
    pd <- rv$pending_delivery
    rv$pending_delivery <- NULL
    rv$amb_msg <- NULL
    removeModal()
    if (!is.null(pd)) unlink(dirname(pd$copies), recursive = TRUE)
  })

  # "Set all to ...": answers every row the chosen dataset is a candidate of (every
  # row for Don't load) and leaves the others for the user.
  observeEvent(input$amb_apply, {
    pd <- rv$pending_delivery
    req(pd, !is.null(input$amb_set_all))
    v <- input$amb_set_all
    for (i in seq_along(pd$rows)) {
      if (identical(v, "") || v %in% pd$rows[[i]]$candidates) {
        updateRadioButtons(session, sprintf("amb_%s_%d", pd$nonce, i), selected = v)
      }
    }
  })

  # The sidebar's last-delivery line reopens the Delivery view.
  observeEvent(input$delivery_details, {
    rv$main_view <- "delivery"
    bslib::nav_select("main_tabs", "Datasets", session = session)
  })
  observeEvent(input$check_all_delivery, run_check(NULL))
  observeEvent(input$delivery_back, rv$main_view <- "dataset", ignoreInit = TRUE)

  # --- the container manager (D10) -----------------------------------------
  # One modal, two tabs (containers / assign files). Its body is a uiOutput keyed
  # on rv$ctr_token: it renders NULL while the manager is closed and is never
  # suspended, because a modal's output keeps the client's last HTML and a
  # reopened dialog would otherwise show it (and report its stale inputs) before
  # the server answers. The browser's close is reported by dta_modal_state.
  # Every mutation requires editing(); in view mode the manager only shows.
  ctr_modal_seq <- 0L

  open_container_manager <- function(tab = "containers") {
    req(rv$dta)
    ctr_modal_seq <<- ctr_modal_seq + 1L
    rv$ctr_nonce <- paste0("containers-", ctr_modal_seq)
    rv$ctr_open <- TRUE
    rv$ctr_view <- "list"
    rv$ctr_edit_key <- NULL
    rv$ctr_form <- list()
    rv$ctr_confirm <- NULL
    rv$ctr_msg <- NULL
    if (identical(tab, "assign")) ctr_enter_assign() else rv$ctr_tab <- "containers"
    rv$ctr_token <- rv$ctr_token + 1
    showModal(tagAppendAttributes(
      modalDialog(
        title = "Delivery containers", size = "xl", easyClose = FALSE,
        uiOutput("ctr_modal_body"),
        footer = uiOutput("ctr_modal_footer")
      ),
      `data-dta-modal` = "containers",
      `data-dta-modal-id` = rv$ctr_nonce
    ))
  }

  ctr_close <- function() {
    rv$ctr_open <- FALSE
    rv$ctr_token <- rv$ctr_token + 1
    removeModal()
  }

  # The Assign tab's rows (one per file entry) and the choices every select
  # shares, computed once for the render.
  ctr_enter_assign <- function() {
    dta <- isolate(rv$dta)
    ca <- DTAtools::container_assignments(dta)
    rv$ctr_assign <- list(
      rows = ca[, c("dataset", "handler", "expected", "reference"), drop = FALSE],
      nonce = paste(sample(c(letters, 0:9), 8, replace = TRUE), collapse = ""),
      base = dta_container_choices_base(dta, ca)
    )
    rv$ctr_tab <- "assign"
    rv$ctr_view <- "list"
    rv$ctr_confirm <- NULL
    rv$ctr_msg <- NULL
  }

  # A save's common end: refuse a result that has an error or introduces a
  # file-name conflict (the document stays as it was), else commit it.
  ctr_commit <- function(r, done) {
    if (!isTRUE(r$ok)) {
      rv$ctr_msg <- r$error
      return(FALSE)
    }
    fresh <- dta_new_conflicts(isolate(rv$dta), r$value)
    if (nrow(fresh) > 0) {
      rv$ctr_msg <- dta_conflict_refusal(fresh, isolate(rv$dta))
      return(FALSE)
    }
    rv$dta <- r$value
    rv$structure <- build_structure(rv$dta)
    sync_yaml_text()
    rv$ctr_msg <- NULL
    rv$ctr_confirm <- NULL
    rv$ctr_edit_key <- NULL
    rv$ctr_form <- list()
    rv$ctr_view <- "list"
    showNotification(paste(c(done, r$note), collapse = " "), type = "message")
    TRUE
  }

  # The row a row button's integer index names, among the rows rendered last.
  ctr_row_at <- function(i) {
    i <- suppressWarnings(as.integer(i))
    rows <- isolate(rv$ctr_rows)
    if (length(i) != 1 || is.na(i) || i < 1 || i > length(rows)) NULL else rows[[i]]
  }

  observeEvent(input$open_containers, open_container_manager("containers"), ignoreInit = TRUE)
  observeEvent(input$metadata_manage_containers, open_container_manager("containers"), ignoreInit = TRUE)

  output$ctr_modal_body <- renderUI({
    rv$ctr_token
    if (!isTRUE(isolate(rv$ctr_open)) || is.null(isolate(rv$dta))) {
      return(NULL)
    }
    dta <- isolate(rv$dta)
    ro <- !isolate(editing())
    tab <- isolate(rv$ctr_tab)
    tabs <- tags$ul(
      class = "nav nav-tabs mb-3", role = "tablist",
      tags$li(
        class = "nav-item", role = "presentation",
        actionLink("ctr_tab_containers", "Containers",
          class = paste("nav-link", if (identical(tab, "containers")) "active"),
          role = "tab", `aria-selected` = tolower(identical(tab, "containers"))
        )
      ),
      tags$li(
        class = "nav-item", role = "presentation",
        actionLink("ctr_tab_assign", "Assign files",
          class = paste("nav-link", if (identical(tab, "assign")) "active"),
          role = "tab", `aria-selected` = tolower(identical(tab, "assign"))
        )
      )
    )
    body <- if (identical(tab, "assign")) {
      st <- isolate(rv$ctr_assign)
      rows <- st$rows
      if (nrow(rows) == 0) {
        div(class = "msg-hint", "No files are declared yet. Add the expected files under Edit \u25B8 Files first.")
      } else {
        sel <- lapply(seq_len(nrow(rows)), function(i) {
          ref <- rows$reference[[i]]
          if (is.na(ref)) ref <- ""
          list(
            choices = dta_container_choices(dta, current = ref, base = st$base),
            selected = dta_container_normalise_ref(dta, ref)
          )
        })
        # A dataset's title is on every one of its rows: the filter hides rows, and
        # a row left without its dataset's name would be a bare file name. Only the
        # first row of a dataset says it loudly.
        # Filtering and "Set the shown rows to" are the browser's: the rows of a
        # filtered-out dataset are hidden (not removed), and Apply reports which
        # ones are still shown, so no select loses an unsaved choice.
        shown_js <- paste0(
          "var m=this.closest('.modal-body');var v=[];",
          "m.querySelectorAll('tr[data-ctr-row]').forEach(function(r){if(!r.hidden)v.push(r.getAttribute('data-ctr-row'));});",
          "Shiny.setInputValue('ctr_shown',v.join(','));"
        )
        filter_js <- paste0(
          "var q=this.value.toLowerCase();",
          "this.closest('.modal-body').querySelectorAll('tr[data-ctr-row]').forEach(function(r){",
          "r.hidden=q!==''&&r.getAttribute('data-ctr-text').indexOf(q)<0;});"
        )
        tagList(
          if (!ro) {
            div(
              class = "ctr-assign-tools",
              tags$input(
                type = "search", class = "form-control form-control-sm ctr-filter", id = "ctr_filter",
                placeholder = "Filter by dataset or file", `aria-label` = "Filter the rows by dataset or expected file",
                oninput = filter_js
              ),
              div(
                class = "d-flex gap-2 align-items-end",
                selectInput("ctr_set_all", "Set the shown rows to", choices = st$base, selectize = FALSE, width = "auto"),
                tagAppendAttributes(
                  actionButton("ctr_apply", "Apply", class = "btn btn-outline-secondary mb-3"),
                  onclick = shown_js
                )
              )
            )
          },
          div(
            class = "ctr-assign-scroll",
            tags$table(
              class = "table table-sm ctr-table",
              tags$thead(tags$tr(lapply(c("Dataset", "Expected file", "Delivered inside"), tags$th))),
              tags$tbody(lapply(seq_len(nrow(rows)), function(i) {
                first <- i == 1L || rows$dataset[[i]] != rows$dataset[[i - 1L]]
                s <- sel[[i]]
                title <- ds_title(rows$dataset[[i]])
                tags$tr(
                  `data-ctr-row` = i,
                  `data-ctr-text` = tolower(paste(title, rows$dataset[[i]], rows$expected[[i]])),
                  tags$td(if (first) tags$strong(title) else tags$span(class = "ctr-repeat", title)),
                  tags$td(tags$code(rows$expected[[i]])),
                  tags$td(if (ro) {
                    names(s$choices)[match(s$selected, s$choices)]
                  } else {
                    htmltools::tagQuery(selectInput(sprintf("ctr_a_%s_%d", st$nonce, i),
                      label = NULL, choices = s$choices, selected = s$selected,
                      selectize = FALSE, width = "100%"
                    ))$find("select")$addClass("form-select-sm")$addAttrs(
                      `aria-label` = paste("Delivered inside for", title, rows$expected[[i]])
                    )$allTags()
                  })
                )
              }))
            )
          ),
          uiOutput("ctr_msg")
        )
      }
    } else if (identical(isolate(rv$ctr_view), "form")) {
      pf <- isolate(rv$ctr_form) %||% list()
      tagList(
        textInput("ctr_filename", "File name",
          value = pf$filename %||% "", width = "100%", placeholder = "STUDY_SDTM_*.zip"
        ),
        div(
          class = "msg-hint", style = "margin:-8px 0 10px;",
          "A .zip archive or a .xpt SAS transport file. * stands for any text, e.g. STUDY_SDTM_*.zip (letter case must match)."
        ),
        uiOutput("ctr_match"),
        textAreaInput("ctr_description", "Description",
          value = pf$description %||% "", width = "100%", rows = 3
        ),
        div(class = "msg-hint", style = "margin:-8px 0 10px;", "Shown in the exported agreement."),
        checkboxInput("ctr_default", "All other files must arrive inside it too", value = isTRUE(pf$default)),
        div(class = "msg-hint", style = "margin:-8px 0 10px;", "Files added later are included automatically."),
        uiOutput("ctr_default_warning"),
        uiOutput("ctr_msg"),
        div(
          class = "d-flex justify-content-between mt-2",
          actionButton("ctr_cancel", "Cancel", class = "btn btn-outline-secondary"),
          actionButton("ctr_save", "Save", class = "btn btn-primary")
        )
      )
    } else {
      rows <- dta_container_rows(dta)
      rv$ctr_rows <- rows
      pending <- isolate(rv$ctr_confirm)
      pend_row <- Filter(function(r) identical(r$key, pending), rows)
      tagList(
        if (!ro) {
          div(
            class = "mb-2",
            actionButton("ctr_add", "+ Add container", class = "btn btn-sm btn-outline-primary")
          )
        },
        dv_container_list_ui(
          rows,
          actions = if (!ro && length(rows) > 0) {
            as.list(row_action_buttons("ctr_edit", "ctr_remove", length(rows),
              labels = vapply(rows, function(r) r$filename, character(1))
            ))
          }
        ),
        if (!ro && length(pend_row) > 0) {
          r <- pend_row[[1]]
          div(
            class = "alert alert-warning py-2",
            role = "alert",
            paste0(
              "Remove ", r$filename, "? Its ", .ctr_count(r$n_files, "file", "files"),
              " will no longer have to arrive inside it."
            ),
            div(
              class = "mt-2",
              actionButton("ctr_remove_confirm", "Remove", class = "btn btn-sm btn-danger"),
              actionButton("ctr_remove_keep", "Keep", class = "btn btn-sm btn-outline-secondary")
            )
          )
        },
        uiOutput("ctr_msg")
      )
    }
    tagList(
      tabs,
      if (ro) div(class = "msg-hint mb-2", "View only \u2014 switch on Edit mode to change containers."),
      body
    )
  })
  outputOptions(output, "ctr_modal_body", suspendWhenHidden = FALSE)

  # The dialog's footer follows the view: no Close while the container form is
  # open (its own Cancel and Save are the way out), Cancel and Save of the Assign
  # tab here (the body scrolls, the buttons do not), else Close.
  output$ctr_modal_footer <- renderUI({
    rv$ctr_token
    if (!isTRUE(isolate(rv$ctr_open)) || is.null(isolate(rv$dta))) {
      return(NULL)
    }
    tab <- isolate(rv$ctr_tab)
    if (identical(tab, "containers") && identical(isolate(rv$ctr_view), "form")) {
      return(NULL)
    }
    if (identical(tab, "assign") && isolate(editing()) && nrow(isolate(rv$ctr_assign)$rows) > 0) {
      return(tagList(
        actionButton("ctr_assign_cancel", "Cancel", class = "btn btn-outline-secondary"),
        actionButton("ctr_assign_save", "Save", class = "btn btn-primary")
      ))
    }
    modalButton("Close")
  })
  outputOptions(output, "ctr_modal_footer", suspendWhenHidden = FALSE)

  output$ctr_msg <- renderUI({
    if (!isTRUE(rv$ctr_open) || is.null(rv$ctr_msg)) {
      return(NULL)
    }
    div(class = "text-danger", role = "alert", rv$ctr_msg)
  })
  outputOptions(output, "ctr_msg", suspendWhenHidden = FALSE)

  # The form's live lines (never rendered outside the form).
  ctr_in_form <- function() isTRUE(rv$ctr_open) && identical(rv$ctr_tab, "containers") && identical(rv$ctr_view, "form")
  output$ctr_match <- renderUI({
    if (!ctr_in_form()) {
      return(NULL)
    }
    dv_container_match_line(input$ctr_filename %||% isolate(rv$ctr_form)$filename, isolate(rv$last_delivery))
  })
  outputOptions(output, "ctr_match", suspendWhenHidden = FALSE)

  output$ctr_default_warning <- renderUI({
    if (!ctr_in_form()) {
      return(NULL)
    }
    on <- input$ctr_default %||% isTRUE(isolate(rv$ctr_form)$default)
    if (!isTRUE(on)) {
      return(NULL)
    }
    key <- isolate(rv$ctr_edit_key)
    other <- Filter(function(r) isTRUE(r$default) && !identical(r$key, key), dta_container_rows(isolate(rv$dta)))
    if (length(other) == 0) {
      return(NULL)
    }
    new <- trimws(as.character(input$ctr_filename %||% isolate(rv$ctr_form)$filename %||% "")[1])
    div(
      class = "alert alert-warning py-2", role = "alert",
      paste0(
        "'", other[[1]]$filename, "' currently collects all other files. Saving moves them to '",
        if (is.na(new) || !nzchar(new)) "this container" else new,
        "'; '", other[[1]]$filename, "' will then hold no files."
      )
    )
  })
  outputOptions(output, "ctr_default_warning", suspendWhenHidden = FALSE)

  observeEvent(input$dta_modal_state, {
    st <- input$dta_modal_state
    closed <- identical(st$event, "hidden") && !is.null(rv$ctr_nonce) &&
      identical(as.character(st$id %||% ""), rv$ctr_nonce)
    replaced <- identical(st$event, "shown") && !identical(st$kind, "containers")
    if (isTRUE(rv$ctr_open) && (closed || replaced)) {
      rv$ctr_open <- FALSE
      rv$ctr_nonce <- NULL
      rv$ctr_token <- rv$ctr_token + 1
    }
  })

  observeEvent(input$ctr_tab_containers, {
    req(rv$ctr_open)
    rv$ctr_tab <- "containers"
    rv$ctr_view <- "list"
    rv$ctr_confirm <- NULL
    rv$ctr_msg <- NULL
    rv$ctr_token <- rv$ctr_token + 1
  })
  observeEvent(input$ctr_tab_assign, {
    req(rv$ctr_open, rv$dta)
    ctr_enter_assign()
    rv$ctr_token <- rv$ctr_token + 1
  })

  # -- the containers tab --
  observeEvent(input$ctr_add, {
    req(editing(), rv$ctr_open)
    rv$ctr_edit_key <- NULL
    rv$ctr_form <- list()
    rv$ctr_confirm <- NULL
    rv$ctr_msg <- NULL
    rv$ctr_view <- "form"
    rv$ctr_token <- rv$ctr_token + 1
  })
  observeEvent(input$ctr_edit, {
    req(editing(), rv$ctr_open)
    r <- ctr_row_at(input$ctr_edit)
    req(r)
    rv$ctr_edit_key <- r$key
    rv$ctr_form <- list(filename = r$filename, description = r$description %||% "", default = isTRUE(r$default))
    rv$ctr_confirm <- NULL
    rv$ctr_msg <- NULL
    rv$ctr_view <- "form"
    rv$ctr_token <- rv$ctr_token + 1
  })
  observeEvent(input$ctr_cancel, {
    req(rv$ctr_open)
    rv$ctr_view <- "list"
    rv$ctr_msg <- NULL
    rv$ctr_token <- rv$ctr_token + 1
  })
  observeEvent(input$ctr_save, {
    req(editing(), rv$ctr_open, identical(rv$ctr_view, "form"))
    pf <- rv$ctr_form
    r <- dta_container_save(
      rv$dta, rv$ctr_edit_key,
      input$ctr_filename %||% pf$filename,
      input$ctr_description %||% pf$description,
      input$ctr_default %||% isTRUE(pf$default)
    )
    if (ctr_commit(r, "Container saved.")) rv$ctr_token <- rv$ctr_token + 1
  })
  observeEvent(input$ctr_remove, {
    req(editing(), rv$ctr_open)
    r <- ctr_row_at(input$ctr_remove)
    req(r)
    rv$ctr_confirm <- r$key
    rv$ctr_msg <- NULL
    rv$ctr_token <- rv$ctr_token + 1
  })
  observeEvent(input$ctr_remove_keep, {
    req(rv$ctr_open)
    rv$ctr_confirm <- NULL
    rv$ctr_token <- rv$ctr_token + 1
  })
  observeEvent(input$ctr_remove_confirm, {
    req(editing(), rv$ctr_open, rv$ctr_confirm)
    key <- rv$ctr_confirm
    if (!ctr_commit(dta_container_remove(rv$dta, key), "Container removed.")) {
      rv$ctr_confirm <- NULL
    }
    rv$ctr_token <- rv$ctr_token + 1
  })

  # -- the assign tab --
  observeEvent(input$ctr_apply, {
    req(editing(), rv$ctr_open)
    st <- rv$ctr_assign
    v <- input$ctr_set_all
    req(st, !is.null(v))
    # The browser reports which rows the filter still shows ("1,2,5"; "" = none);
    # without a report every row is meant.
    shown <- input$ctr_shown
    rows_to_set <- if (is.null(shown)) {
      seq_len(nrow(st$rows))
    } else {
      intersect(seq_len(nrow(st$rows)), suppressWarnings(as.integer(strsplit(as.character(shown), ",", fixed = TRUE)[[1]])))
    }
    for (i in rows_to_set) {
      updateSelectInput(session, sprintf("ctr_a_%s_%d", st$nonce, i), selected = v)
    }
  })
  # Cancel leaves the Assign tab for the Containers list; the dialog stays.
  observeEvent(input$ctr_assign_cancel, {
    req(rv$ctr_open)
    rv$ctr_tab <- "containers"
    rv$ctr_view <- "list"
    rv$ctr_confirm <- NULL
    rv$ctr_msg <- NULL
    rv$ctr_token <- rv$ctr_token + 1
  })
  observeEvent(input$ctr_assign_save, {
    req(editing(), rv$ctr_open, identical(rv$ctr_tab, "assign"))
    st <- rv$ctr_assign
    req(st)
    rows <- st$rows
    # A select that did not report keeps the stored reference (NA), and so does
    # one still showing what is stored: only a real choice rewrites an entry.
    vals <- vapply(seq_len(nrow(rows)), function(i) {
      v <- input[[sprintf("ctr_a_%s_%d", st$nonce, i)]]
      if (is.null(v) || length(v) != 1 || is.na(v)) {
        return(NA_character_)
      }
      ref <- rows$reference[[i]]
      if (is.na(ref)) ref <- ""
      if (identical(as.character(v), dta_container_normalise_ref(rv$dta, ref))) NA_character_ else as.character(v)
    }, character(1))
    r <- dta_container_assign(
      rv$dta,
      data.frame(dataset = rows$dataset, handler = rows$handler, value = vals, stringsAsFactors = FALSE)
    )
    if (ctr_commit(r, "Container assignments saved.")) ctr_close()
  })

  # File-name conflicts of the live document, shared by the dataset panel and the
  # Files editor (a bulk source -- opening a document, raw YAML, a template --
  # may bring some in; an edit that would add one is refused where it is made).
  conflicts_now <- reactive({
    req(rv$dta)
    tryCatch(DTAtools::file_name_conflicts(rv$dta), error = function(e) NULL)
  })
  output$dataset_conflict_warning <- renderUI({
    req(rv$active)
    dv_conflict_ui(conflicts_now(), rv$active, editing(), rv$dta)
  })
  output$file_conflict_warning <- renderUI({
    rv$file_token
    ed <- isolate(rv$editor_dataset)
    req(ed)
    dv_conflict_ui(conflicts_now(), ed, editing(), rv$dta, in_files_editor = TRUE)
  })

  output$metadata_containers <- renderUI({
    req(rv$dta)
    dv_container_list_ui(dta_container_rows(rv$dta))
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
    dta_delivery_cleanup(owned$upload, rv$uploads, session$token)
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
      dta_delivery_cleanup(owned$upload, rv$uploads, session$token)
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
  # `quiet = TRUE` is for re-checks that an edit triggers rather than the user
  # pressing Validate: status and messages update as usual, but the "Validation
  # complete" toast and the messages dock stay out of the way of the editor.
  run_check <- function(dataset = NULL, quiet = FALSE) {
    req(rv$dta)
    names_ds <- dta_dataset_names(rv$dta)
    targets <- if (is.null(dataset)) names_ds else intersect(dataset, names_ds)
    if (length(targets) == 0) {
      # targets is empty two different ways, and only one of them deserves a
      # notification. names_ds itself empty means the document has no
      # datasets at all -- e.g. right after "Create new" -- so the sidebar's
      # "Check all datasets" button would otherwise do and say nothing when
      # pressed. The other way, a non-empty `dataset` that intersect() wiped
      # out entirely, means the caller named a dataset that is no longer in
      # the document (renamed or removed out from under it); that is a stale
      # reference rather than a user action to react to, so it keeps today's
      # silent return.
      if (length(names_ds) == 0) {
        showNotification(
          "This specification has no datasets yet \u2014 add one with + Add dataset before validating.",
          type = "warning", duration = 8
        )
      }
      return()
    }

    # Pre-flight: only validate datasets that actually have data bound.
    ready <- Filter(function(nm) dta_dataset_readiness(rv$dta, nm)$has_data, targets)
    nodata <- setdiff(targets, ready)

    # Mark no-data datasets explicitly; they are never reported as passed.
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
    if (quiet) {
      return(invisible())
    }

    # ONE reconciled summary derived from the verified status map.
    st <- unlist(rv$status)
    n_pass <- sum(st[ready] == "pass", na.rm = TRUE)
    n_fail <- sum(st[ready] == "fail", na.rm = TRUE)
    parts <- character(0)
    if (n_pass > 0) parts <- c(parts, sprintf("%d passed", n_pass))
    if (n_fail > 0) parts <- c(parts, sprintf("%d failed", n_fail))
    if (length(nodata) > 0) parts <- c(parts, sprintf("%d skipped (no data)", length(nodata)))
    if (length(parts) == 0) parts <- "nothing to validate"
    summary_txt <- sprintf("Validation complete \u2014 %s.", paste(parts, collapse = ", "))
    # Content and packaging are two verdicts: with a finding against a checked
    # dataset the toast says both, and the content count never stands alone.
    fnd <- isolate(findings_now())
    fnd <- fnd[fnd$dataset %in% ready, , drop = FALSE]
    if (nrow(fnd) > 0) {
      summary_txt <- sprintf(
        "Content: %s; packaging: %s.",
        paste(parts, collapse = ", "), dv_packaging_phrase(fnd)
      )
    }
    # A file-count violation is not visible in the pass/fail tally, so list the
    # reasons. They go in as plain-text children (htmltools escapes them);
    # newlines would not render in a toast.
    count_msgs <- c(
      dta_file_count_messages(rv$dta, datasets = ready),
      dta_container_messages(rv$dta, datasets = ready)
    )
    if (length(count_msgs) > 0) {
      showNotification(
        ui = tagList(summary_txt, tags$ul(lapply(count_msgs, tags$li))),
        type = "warning"
      )
    } else {
      showNotification(
        summary_txt,
        type = if (n_fail > 0) "warning" else "message"
      )
    }

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
  # A spec edit that can change a verdict (column or rule saved/removed) also
  # re-validates data that is already loaded, so the status never reads
  # "pending" for data whose result the new spec already determines. Call it
  # AFTER sync_yaml_text(): a failing check must not hold up the YAML refresh.
  recheck_dataset <- function(ed) {
    if (isTRUE(dta_dataset_readiness(isolate(rv$dta), ed)$has_data)) {
      run_check(ed, quiet = TRUE)
    }
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

  # What every file-handler mutation must do: re-key the dataset's bound uploads
  # when handlers moved (remap_uploads(), only if `map` is given), clear its
  # validation, rebuild the slot structure (which registers the upload
  # observers for a new slot), reset its file inputs, re-render the editor, and
  # re-serialise the YAML.
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
      title = paste("Edit files —", ds_title(ed)),
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
  # so re-clicking the same row still fires). The datatable() call escapes
  # every other column and disables escaping for this one column alone
  # (escape = -ncol(ov)), because everything else in the frame is document
  # text and only this generated column is meant to be raw HTML.
  #
  # `labels` (one per row) name the row in the Edit and Remove buttons' accessible
  # names: a screen reader hears "Edit sdtm_*.zip", not a row of identical pencils.
  row_action_buttons <- function(edit_input, del_input, n,
                                 up_input = NULL, down_input = NULL, labels = NULL) {
    aria <- function(verb, i) {
      if (is.null(labels)) "" else sprintf(" aria-label=\"%s %s\"", verb, htmltools::htmlEscape(labels[[i]], attribute = TRUE))
    }
    vapply(seq_len(n), function(i) {
      up_btn <- if (is.null(up_input)) {
        ""
      } else if (i > 1L) {
        sprintf(
          "<button class=\"btn btn-sm btn-outline-secondary dta-row-btn dta-move-btn\" title=\"Move up\" onclick=\"Shiny.setInputValue('%s', %d, {priority:'event'})\">&#x25B2;</button> ",
          up_input, i
        )
      } else {
        "<button class=\"btn btn-sm btn-outline-secondary dta-row-btn dta-move-btn\" title=\"Move up\" disabled>&#x25B2;</button> "
      }
      down_btn <- if (is.null(down_input)) {
        ""
      } else if (i < n) {
        sprintf(
          "<button class=\"btn btn-sm btn-outline-secondary dta-row-btn dta-move-btn\" title=\"Move down\" onclick=\"Shiny.setInputValue('%s', %d, {priority:'event'})\">&#x25BC;</button> ",
          down_input, i
        )
      } else {
        "<button class=\"btn btn-sm btn-outline-secondary dta-row-btn dta-move-btn\" title=\"Move down\" disabled>&#x25BC;</button> "
      }
      paste0(
        up_btn, down_btn,
        sprintf(
          "<button class=\"btn btn-sm btn-outline-secondary dta-row-btn\" title=\"Edit\"%s onclick=\"Shiny.setInputValue('%s', %d, {priority:'event'})\">&#x270E;</button> ",
          aria("Edit", i), edit_input, i
        ),
        sprintf(
          "<button class=\"btn btn-sm btn-outline-danger dta-row-btn\" title=\"Remove\"%s onclick=\"Shiny.setInputValue('%s', %d, {priority:'event'})\">&#x1F5D1;</button>",
          aria("Remove", i), del_input, i
        )
      )
    }, character(1))
  }

  # A searchable spec list (columns, rules) whose move arrows swap an item with
  # its neighbour in the SPEC -- which is not the row above or below it once
  # the view is searched or sorted. So in either state the arrows are hidden
  # rather than move something the user cannot see; edit and remove carry the
  # spec index and stay correct throughout. reorder_table_ui() wraps the
  # output; reorder_table_callback() toggles .dta-reorder-off on that wrapper
  # and reports the search term as input$<search_input>, which the server's
  # move guards read.
  reorder_table_ui <- function(table_id) {
    div(
      class = "reorder-wrap",
      div(
        class = "spec-hint reorder-off-hint",
        "Reordering is off while the list is searched or sorted. ",
        tags$a(href = "#", class = "reorder-reset", "Show all in spec order")
      ),
      DT::dataTableOutput(table_id)
    )
  }

  reorder_table_callback <- function(search_input) {
    DT::JS(
      "var wrap = $(table.table().container()).closest('.reorder-wrap');",
      "function sync() {",
      "  wrap.toggleClass('dta-reorder-off',",
      "    table.search() !== '' || table.order().length > 0);",
      # DT's own <id>_search report is deduplicated client-side, so re-typing
      # last session's term after a reopen (which resets the server's copy)
      # would never reach the move guard.
      sprintf(
        "  Shiny.setInputValue('%s', table.search(), {priority: 'event'});",
        search_input
      ),
      "}",
      "table.on('search.dt order.dt', sync);",
      "wrap.find('.reorder-reset').off('click').on('click', function(e) {",
      "  e.preventDefault();",
      "  table.search('').order([]).draw();",
      "});",
      "sync();"
    )
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
        uiOutput("file_conflict_warning"),
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
      # The reader settings live in their own sub-list (dta_handler_fields()),
      # so they need their own accessor -- `pf$reader_settings` is empty for a
      # handler being added, and absent entirely for one whose type has none.
      rs <- pf$reader_settings %||% list()
      if (!is.list(rs)) rs <- list()
      gr <- function(k, d) rs[[k]] %||% d
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
      # The "Delivered inside" select: the document's containers, plus the stored
      # reference whatever it is (a select must never fall back to "" and erase it).
      ct_choices <- dta_container_choices(isolate(rv$dta), current = g("container"))
      ct_selected <- dta_container_normalise_ref(isolate(rv$dta), g("container"))
      # Labelled only where the bare token would not explain itself.
      type_labels <- c(
        any = "Any file (not parsed)",
        sas7bdat = "SAS dataset (.sas7bdat)",
        xpt = "SAS transport (.xpt)"
      )
      names(type_choices) <- ifelse(
        type_choices %in% names(type_labels),
        type_labels[type_choices], type_choices
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
          selectizeInput("file_container", "Delivered inside",
            choices = ct_choices, selected = ct_selected, width = "100%",
            # A typed delimiter creates the item at once, so the default ","
            # would cut "a,b.zip" short; no file name holds this one.
            options = list(create = TRUE, allowEmptyOption = TRUE, delimiter = "\u0001")
          ),
          div(class = "msg-hint", style = "margin:-8px 0 10px;", paste(
            "Pick a container, or type a new archive name (e.g. sdtm_*.zip; * = any text, letter case must match) to add one.",
            "Containers are listed under Metadata \u25B8 Containers."
          )),
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
          # How the file is READ. Only a delimited-text type has these, so the
          # panel is the mirror of the endings panel above -- and neither an
          # `any`, a `sas7bdat` nor an `xpt` handler's constructor takes either
          # argument (a SAS dataset declares its own encoding).
          conditionalPanel(
            condition = "input.file_type != 'any' && input.file_type != 'sas7bdat' && input.file_type != 'xpt'",
            layout_columns(
              col_widths = c(6, 6),
              selectizeInput("file_encoding", "File encoding",
                # The handler's own value is prepended so an encoding that came
                # from a document but is not on the shortlist still shows as the
                # selection rather than silently falling back to UTF-8.
                # create = TRUE for the same reason the list is a shortlist:
                # iconv() accepts far more names than are worth listing.
                choices = unique(c(gr("encoding", "UTF-8"), dta_handler_encodings())),
                selected = gr("encoding", "UTF-8"), width = "100%",
                options = list(create = TRUE)
              ),
              div(
                style = "margin-top:26px;",
                checkboxInput("file_newlines_in_values",
                  "Quoted values may contain line breaks",
                  value = isTRUE(gr("newlines_in_values", FALSE))
                )
              )
            ),
            div(
              class = "msg-hint", style = "margin:-8px 0 10px;",
              paste(
                "UTF-8 is what a well-behaved exporter writes; pick or type",
                "another only when the delivered file really is in it.",
                "Line breaks inside quoted values are rare, cost parse speed",
                "and are only needed when the file is large enough for one to",
                "land on a read-block boundary."
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
    ov <- dv_files_overview(isolate(rv$dta), ed)
    if (is.null(ov) || nrow(ov) == 0) {
      ov <- data.frame(
        filename = character(0), type = character(0), container = character(0),
        pattern = character(0), files = character(0), endings = character(0),
        description = character(0),
        stringsAsFactors = FALSE
      )
    }
    ov$Actions <- if (nrow(ov) > 0) {
      row_action_buttons(
        "file_edit_click", "file_del_click", nrow(ov),
        "file_up_click", "file_down_click",
        labels = ov$filename
      )
    } else {
      character(0)
    }
    DT::datatable(
      ov,
      rownames = FALSE, selection = "none", escape = -ncol(ov),
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
    # The form shows two reader settings; the handler may carry more. Start
    # from what it had (empty when a handler is being added, which is what
    # leaves a new one on the constructor defaults) so that editing a file name
    # cannot silently drop a `has_header` or a `quote` the user never saw, then
    # overwrite the two the form does own. dta_set_handler() drops whatever the
    # chosen type's constructor will not take, so an `any` or a `sas7bdat`
    # handler ignores both.
    # Each of the two is overwritten only when its control actually reported a
    # value: an input that was never rendered reads as NULL, and taking that as
    # "unticked" / "blank" would clear a setting the form never showed.
    reader <- isolate(rv$file_prefill)$reader_settings %||% list()
    if (!is.list(reader)) reader <- list()
    if (!is.null(input$file_newlines_in_values)) {
      reader$newlines_in_values <- isTRUE(input$file_newlines_in_values)
    }
    encoding <- trimws(as.character(input$file_encoding %||% "")[1])
    if (nzchar(encoding)) reader$encoding <- encoding
    # "Delivered inside": an empty value means none; a value among the select's
    # choices is stored as it is; the file name of an explicit container is its
    # id; anything else was typed -- a new archive name, which becomes an
    # explicit container (its id is derived, never asked for).
    # A control that never rendered reads as NULL, and taking that as blank
    # would clear a container the form never showed; an explicitly emptied
    # field ("") does clear it.
    dta0 <- isolate(rv$dta)
    dta1 <- dta0
    ct <- input$file_container %||% (isolate(rv$file_prefill)$container %||% "")
    ct <- trimws(as.character(ct)[1])
    if (is.na(ct)) ct <- ""
    if (nzchar(ct) && !ct %in% dta_container_choices(dta0, current = isolate(rv$file_prefill)$container)) {
      by_name <- dta_container_normalise_ref(dta0, ct)
      if (!identical(by_name, ct)) {
        ct <- by_name
      } else {
        made <- dta_container_save(dta0, NULL, ct, NULL, FALSE)
        if (!isTRUE(made$ok)) {
          rv$file_msg <- list(ok = FALSE, error = made$error)
          return()
        }
        dta1 <- made$value
        ct <- made$key
      }
    }
    # The count controls only exist while `pattern` is ticked; without it the
    # class contract fixes the count at exactly 1.
    r <- dta_set_handler(
      dta1, ed,
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
      reader_settings = reader,
      container = ct,
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
    # An edit that would let two datasets take the same file name is refused
    # where it is made; conflicts the document already had do not block it.
    fresh <- dta_new_conflicts(isolate(conflicts_now()) %||% dta0, r$value)
    if (nrow(fresh) > 0) {
      rv$file_msg <- list(ok = FALSE, error = dta_conflict_refusal(fresh, dta0))
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
    # A fresh editor starts unfiltered: input$col_tbl_search still holds
    # whatever the last session of this modal typed, for whichever dataset.
    rv$col_search <- ""
    rv$col_token <- rv$col_token + 1
    showModal(modalDialog(
      title = paste("Edit columns \u2014", ds_title(rv$active)),
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
        reorder_table_ui("col_tbl"),
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
          refs <- paste0(vocabs$id, "@", vocabs$version)
          # Open on the vocabulary meant for THIS column; only when nothing
          # points at one, on the last one used (if it is still offered).
          sug <- isolate(rv$col_vocab_sug)
          last <- isolate(rv$col_vocab_ref)
          tagList(
            selectInput("col_vocab_ref", "Vocabulary",
              choices = stats::setNames(refs, paste0(vocabs$label, " (", refs, ")")),
              selected = sug$ref %||% (if (isTRUE(last %in% refs)) last), width = "100%"
            ),
            if (!is.null(sug)) {
              p(
                paste0(
                  "Suggested: ", sug$ref, " — ",
                  if (identical(sug$reason, "declared")) {
                    paste0("it lists column ", trimws(pf$id %||% ""), ".")
                  } else {
                    "it holds every value this column already lists."
                  }
                ),
                class = "msg-hint"
              )
            },
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
              # The hint below it is what makes the button read as
              # "suggested" (theme.R styles the button off it), and it is the
              # visible, non-colour half of that cue.
              div(
                class = "col-vocab-open",
                actionButton("col_vocab_open", "Choose from vocabulary…",
                  class = "btn btn-sm btn-outline-secondary",
                  `aria-describedby` = "col_vocab_hint"
                ),
                uiOutput("col_vocab_hint")
              )
            ),
            tagList(
              textInput("col_pattern", "Pattern (regex)", value = g("pattern"), width = "100%"),
              textAreaInput("col_examples", "Examples (one per line)",
                value = g("examples"), width = "100%", rows = 2
              )
            )
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
      rownames = FALSE, selection = "none", escape = -ncol(ov),
      class = "display compact", width = "100%",
      options = list(
        pageLength = 8, dom = "ftp", scrollX = TRUE,
        search = list(search = isolate(rv$col_search)),
        columnDefs = list(list(
          orderable = FALSE, searchable = FALSE, targets = ncol(ov) - 1L
        ))
      ),
      callback = reorder_table_callback("col_tbl_search")
    )
  })

  observeEvent(input$col_tbl_search, {
    rv$col_search <- input$col_tbl_search %||% ""
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
      examples = input$col_examples %||% "",
      description = input$col_desc %||% ""
    )
  }

  # The column's current permitted values, as the form holds them: one per line.
  value_lines <- function(x) {
    v <- trimws(strsplit(x %||% "", "\n")[[1]])
    v[nzchar(v)]
  }
  col_prefill_values <- function() value_lines(isolate(rv$col_prefill)$values)

  # --- column editor: say when a vocabulary is made for this column ---
  #
  # Only a DECLARED relation counts (the vocabulary's `columns:` names the
  # column), drifted values included: the picker's other reason, "every value
  # fits", also fires for any column already built from a vocabulary and for
  # short code sets like Y/N, where it would be a lasting claim with nothing
  # behind it. It is one vocabulary_suggestion() call, so the hint and the
  # picker cannot name different vocabularies.
  #
  # Note: this resolves every vocabulary per call (see vocabulary_suggestion()).
  # Memoising the resolver would have to be tied to the index BUILD, not to an
  # equal index, or "Refresh templates" would stop re-reading edited files.
  col_vocab_hint_for <- function(id, values) {
    if (!nzchar(trimws(id))) {
      return(NULL)
    }
    tryCatch(
      {
        idx <- dta_template_index_cached()
        vocabs <- list_template_index_entries(idx, kind = "dta_vocabulary")
        if (NROW(vocabs) == 0) {
          return(NULL)
        }
        refs <- paste0(vocabs$id, "@", vocabs$version)
        sug <- vocabulary_suggestion(id, values, refs, vocabulary_resolver(idx))
        if (!identical(sug$reason, "declared")) {
          return(NULL)
        }
        list(ref = sug$ref, label = as.character(vocabs$label[[match(sug$ref, refs)]]))
      },
      error = function(e) NULL
    )
  }

  col_vocab_hint_val <- reactiveVal(NULL)

  # Whenever the editor changes view or loads a column: from the prefill, which
  # is what the form is about to show.
  observeEvent(rv$col_token, {
    col_vocab_hint_val(
      if (identical(rv$col_view, "form") && editing()) {
        pf <- rv$col_prefill %||% list()
        col_vocab_hint_for(as.character(pf$id %||% ""), col_prefill_values())
      }
    )
  })

  # While the form is open: from what is typed. Created once, here, not per
  # modal; and not while another view shows, where input$col_id still holds the
  # removed form's last value.
  col_form_d <- debounce(reactive(list(input$col_id, input$col_values)), 500)
  observeEvent(col_form_d(),
    {
      req(identical(rv$col_view, "form"), editing())
      col_vocab_hint_val(col_vocab_hint_for(input$col_id %||% "", value_lines(input$col_values)))
    },
    ignoreInit = TRUE
  )

  # No id on the paragraph: the output's own container carries col_vocab_hint,
  # and the button's aria-describedby points at it. The label comes from a
  # template author, so it goes through htmltools' escaping like any text.
  output$col_vocab_hint <- renderUI({
    rv$col_token
    h <- col_vocab_hint_val()
    if (is.null(h)) {
      return(NULL)
    }
    p(
      class = "msg-hint vocab-hint",
      "Suggested vocabulary for this column: ", tags$strong(h$label), paste0(" (", h$ref, ")")
    )
  })

  observeEvent(input$col_vocab_open, {
    req(editing())
    rv$col_prefill <- snapshot_col_form()
    # Once per open, not per render: the body names it and the terms below
    # take its Codes/Labels choice from it.
    idx <- dta_template_index_cached()
    vocabs <- list_template_index_entries(idx, kind = "dta_vocabulary")
    rv$col_vocab_sug <- if (NROW(vocabs) > 0) {
      vocabulary_suggestion(
        rv$col_prefill$id, col_prefill_values(),
        paste0(vocabs$id, "@", vocabs$version), vocabulary_resolver(idx)
      )
    }
    rv$col_msg <- NULL
    rv$col_view <- "vocab"
    rv$col_token <- rv$col_token + 1
  })

  observeEvent(input$col_vocab_cancel, {
    rv$col_msg <- NULL
    rv$col_view <- "form"
    rv$col_token <- rv$col_token + 1
  })

  # The Codes/Labels choice and the terms of whichever vocabulary is selected,
  # pre-ticked with any of them the column already lists, by code or by label
  # -- so re-opening the picker on a column that was built from this vocabulary
  # (its code or its decode half) shows the current selection. When the column
  # lists none of them -- a new column, or a different vocabulary -- every term
  # is ticked, because the whole vocabulary is what a column usually wants.
  #
  # Deliberately not dependent on input$col_vocab_field: flipping codes/labels
  # must not throw away terms the user has already picked. It IS dependent on
  # rv$col_token, which every open of the picker bumps: the suggestion often
  # selects the same vocabulary as last time, so input$col_vocab_ref alone
  # would leave the previous column's ticks on screen.
  output$col_vocab_terms <- renderUI({
    rv$col_token
    ref <- input$col_vocab_ref
    req(ref)
    resolve <- vocabulary_resolver(dta_template_index_cached())
    vocab <- tryCatch(resolve(ref), error = function(e) e)
    if (inherits(vocab, "condition") || is.null(vocab)) {
      msg <- if (inherits(vocab, "condition")) conditionMessage(vocab) else "could not be resolved."
      return(p(paste("Vocabulary", ref, msg), class = "msg-hint"))
    }

    codes <- vapply(vocab$terms %||% list(), function(t) as.character(t$code), character(1))
    if (length(codes) == 0) {
      return(p(paste("Vocabulary", ref, "has no terms."), class = "msg-hint"))
    }
    decodes <- vocabulary_values(vocab, field = "label")
    current <- col_prefill_values()
    hit <- codes %in% current | decodes %in% current
    extras <- setdiff(current, c(codes, decodes))

    # The suggestion's field for the suggested vocabulary; for any other, labels
    # only when the column already holds labels and no codes of it.
    sug <- isolate(rv$col_vocab_sug)
    field <- if (identical(sug$ref, ref)) {
      sug$field
    } else if (any(decodes %in% current) && !any(codes %in% current)) {
      "label"
    } else {
      "code"
    }

    tagList(
      # A vocabulary without labels has nothing to choose between.
      if (!identical(codes, decodes)) {
        radioButtons("col_vocab_field", "Write as",
          choiceNames = list(
            paste0("Codes (e.g. ", codes[[1]], ")"),
            paste0("Labels (e.g. ", decodes[[1]], ")")
          ),
          choiceValues = list("code", "label"),
          selected = field, inline = TRUE
        )
      },
      vocab_term_checklist(
        "col_vocab_terms_sel", vocab$terms,
        # The column's own values stay ticked beside its terms; a column that
        # shares nothing with this vocabulary gets all of it instead.
        selected = if (any(hit)) c(extras, codes[hit]) else codes,
        extras = extras
      )
    )
  })

  observeEvent(input$col_vocab_apply, {
    sel <- as.character(input$col_vocab_terms_sel %||% character(0))
    sel <- sel[!is.na(sel) & nzchar(sel)]
    if (length(sel) == 0) {
      rv$col_msg <- list(ok = FALSE, error = "Select at least one term, or go back without applying.")
      return()
    }
    vocab <- tryCatch(
      vocabulary_resolver(dta_template_index_cached())(input$col_vocab_ref),
      error = function(e) NULL
    )
    codes <- vapply(vocab$terms %||% list(), function(t) t$code, character(1))
    # A vocabulary with no terms renders no checklist, but Shiny keeps the last
    # one's selection: never write that stale selection under this vocabulary.
    if (length(codes) == 0) {
      rv$col_msg <- list(ok = FALSE, error = "This vocabulary has no terms to use.")
      return()
    }
    # The decode half of a code/decode pair (GFTEST beside GFTESTCD) wants the
    # labels. A value the vocabulary does not have stays as it is.
    if (identical(input$col_vocab_field, "label")) {
      hit <- match(sel, codes)
      ok <- !is.na(hit)
      sel[ok] <- as.character(vocabulary_values(vocab, field = "label"))[hit[ok]]
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
    recheck_dataset(ed)
  })

  observeEvent(input$col_up_click, {
    req(editing())
    # The arrows are hidden under a search; refuse one driven over the
    # websocket too. (A sorted view is not reported, but a move there is still
    # a correct move in spec order.)
    req(!nzchar(rv$col_search))
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
    req(!nzchar(rv$col_search))
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
      old_id = isolate(rv$col_edit_id),
      # NULL (field not on the page) keeps the column's examples; otherwise
      # exactly what the field holds, so a stale example can be cleared here.
      examples = if (is.null(input$col_examples)) {
        NULL
      } else {
        strsplit(input$col_examples, "\n", fixed = TRUE)[[1]]
      }
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
    recheck_dataset(ed)
  })

  # ============================= Edit rules ===============================
  # One editable condition row (Column / Operator / Value). The value field is
  # a single text box whose meaning depends on the operator (see the hint under
  # the builder). Values come from the prefill only (fresh renders happen on
  # token bumps where prefill is authoritative; insertUI rows keep DOM state).
  # Labels are rendered on every row; theme.R hides all but the first row's
  # visually (screen readers keep them), so the first remaining row shows them
  # after a removal.
  one_cond_row <- function(prefix, i, cols, pf = list(col = "", op = "equals", val = "")) {
    cid <- paste0(prefix, "_col_", i)
    oid <- paste0(prefix, "_op_", i)
    vid <- paste0(prefix, "_val_", i)
    div(
      id = paste0(prefix, "_row_", i),
      class = "cond-row",
      selectInput(cid, "Column",
        choices = c("(select)" = "", cols),
        selected = pf$col %||% "", width = "100%"
      ),
      selectInput(oid, "Operator",
        choices = dta_condition_operators(),
        selected = pf$op %||% "equals", width = "100%"
      ),
      textInput(vid, "Value",
        value = pf$val %||% "", width = "100%",
        placeholder = "5 | a, b | 0, 99 | true"
      ),
      actionButton(
        paste0(prefix, "_remove_", i),
        "×",
        class = "btn btn-sm btn-outline-danger",
        title = "Remove this row",
        `aria-label` = "Remove this row"
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
    removed <- isolate(rv[[paste0(prefix, "_removed")]])
    out <- list()
    for (i in seq_len(max(1L, n))) {
      if (i %in% removed) next
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
  visible_cond_rows <- function(prefix) {
    removed <- isolate(rv[[paste0(prefix, "_removed")]])
    n <- isolate(rv[[paste0(prefix, "_n")]])
    Filter(function(i) {
      !(i %in% removed) && !is.null(input[[paste0(prefix, "_op_", i)]])
    }, seq_len(max(1L, n)))
  }
  ensure_cond_remove_observer <- function(prefix, i) {
    bid <- paste0(prefix, "_remove_", i)
    if (isTRUE(cond_rm_registry[[bid]])) {
      return(invisible(NULL))
    }
    cond_rm_registry[[bid]] <- TRUE
    observeEvent(input[[bid]],
      {
        if (length(visible_cond_rows(prefix)) <= 1) {
          updateSelectInput(session, paste0(prefix, "_col_", i), selected = "")
          updateSelectInput(session, paste0(prefix, "_op_", i), selected = "equals")
          updateTextInput(session, paste0(prefix, "_val_", i), value = "")
        } else {
          removeUI(selector = paste0("#", prefix, "_row_", i))
          key <- paste0(prefix, "_removed")
          rv[[key]] <- c(isolate(rv[[key]]), i)
        }
      },
      ignoreInit = TRUE
    )
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
      textInput(nid, "Condition name",
        value = pf$name %||% "", width = "100%",
        placeholder = "c1_failed"
      ),
      selectInput(cid, "Column",
        choices = c("(select)" = "", cols),
        selected = pf$col %||% "", width = "100%"
      ),
      selectInput(oid, "Operator",
        choices = dta_condition_operators(),
        selected = pf$op %||% "equals", width = "100%"
      ),
      textInput(vid, "Value",
        value = pf$val %||% "", width = "100%",
        placeholder = "5 | a, b | 0, 99 | true"
      ),
      actionButton(
        bid,
        "×",
        class = "btn btn-sm btn-outline-danger",
        title = "Remove this row",
        `aria-label` = "Remove this row"
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
    removed <- isolate(rv$gcond_removed)
    for (i in seq_len(max(1L, n))) {
      if (i %in% removed) next
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
      textInput(cid, "Constraint id",
        value = pf$id %||% "", width = "100%",
        placeholder = paste0("constraint_", i)
      ),
      selectInput(tid, "Type",
        choices = c("mutually_exclusive", "requires"),
        selected = ctype,
        width = "100%"
      ),
      selectInput(lid, "Left / IF",
        choices = c("(select)" = "", cond_names),
        selected = left_name,
        width = "100%"
      ),
      selectInput(rid, "Right / THEN",
        choices = c("(select)" = "", cond_names),
        selected = right_name,
        width = "100%"
      ),
      selectInput(lsid, "Left/IF scope",
        choices = c("any", "all"),
        selected = pf$left_scope %||% pf$if_scope %||% "any",
        width = "100%"
      ),
      selectInput(rsid, "Right/THEN scope",
        choices = c("any", "all"),
        selected = pf$right_scope %||% pf$then_scope %||% "any",
        width = "100%"
      ),
      textInput(mid, "Message",
        value = pf$message %||% "", width = "100%"
      ),
      actionButton(
        bid,
        "×",
        class = "btn btn-sm btn-outline-danger",
        title = "Remove this row",
        `aria-label` = "Remove this row"
      )
    )
  }

  visible_group_condition_rows <- function() {
    out <- integer(0)
    removed <- isolate(rv$gcond_removed)
    for (i in seq_len(max(1L, isolate(rv$gcond_n)))) {
      if (i %in% removed) next
      if (!is.null(input[[paste0("gcond_name_", i)]])) {
        out <- c(out, i)
      }
    }
    out
  }

  visible_group_constraint_rows <- function() {
    out <- integer(0)
    removed <- isolate(rv$gconstr_removed)
    for (i in seq_len(max(1L, isolate(rv$gconstr_n)))) {
      if (i %in% removed) next
      if (!is.null(input[[paste0("gconstr_type_", i)]])) {
        out <- c(out, i)
      }
    }
    out
  }

  # Distinct non-blank names of the grouped-condition rows still on screen,
  # offered by the constraint Left/Right selectors. Removed rows are skipped
  # explicitly: Shiny keeps a removed input's last value, so without the skip
  # a removed row's name stays selectable and Save rejects it as an unknown
  # condition. rv$gcond_n and rv$gcond_removed are read reactively on purpose
  # (unlike visible_group_condition_rows(), which isolates), so the observer
  # that refreshes the selectors re-runs when a row is removed.
  current_group_condition_names <- function() {
    rows <- setdiff(seq_len(max(1L, rv$gcond_n)), rv$gcond_removed)
    nms <- vapply(rows, function(i) trimws(input[[paste0("gcond_name_", i)]] %||% ""), character(1))
    unique(nms[nzchar(nms)])
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
          rv$gcond_removed <- c(isolate(rv$gcond_removed), i)
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
          rv$gconstr_removed <- c(isolate(rv$gconstr_removed), i)
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
    removed <- isolate(rv$gconstr_removed)
    for (i in seq_len(max(1L, n))) {
      if (i %in% removed) next
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
    rv$cond_removed <- integer(0)
    rv$then_removed <- integer(0)
    rv$gcond_removed <- integer(0)
    rv$gconstr_removed <- integer(0)
    # A fresh editor starts unfiltered (see edit_cols).
    rv$rule_search <- ""
    rv$rule_token <- rv$rule_token + 1
    showModal(modalDialog(
      title = paste("Edit rules \u2014", ds_title(rv$active)),
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
        reorder_table_ui("rule_tbl"),
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
          choices = c("(select)" = "", dta_numeric_column_ids(isolate(rv$dta), ed)),
          selected = (pf$columns %||% "")[1], width = "100%"
        ),
        layout_columns(
          col_widths = c(6, 6),
          textInput("rule_min", "Minimum", value = pf$min %||% "", width = "100%"),
          textInput("rule_max", "Maximum", value = pf$max %||% "", width = "100%")
        ),
        div(
          class = "cond-hint",
          "A range rule checks ONE numeric (Num or Int) column against a minimum and/or maximum."
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
      rownames = FALSE, selection = "none", escape = -ncol(ov),
      class = "display compact", width = "100%",
      options = list(
        pageLength = 8, dom = "ftp", scrollX = TRUE,
        search = list(search = isolate(rv$rule_search)),
        columnDefs = list(list(
          orderable = FALSE, searchable = FALSE, targets = ncol(ov) - 1L
        ))
      ),
      callback = reorder_table_callback("rule_tbl_search")
    )
  })

  observeEvent(input$rule_tbl_search, {
    rv$rule_search <- input$rule_tbl_search %||% ""
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
    rv$cond_removed <- integer(0)
    rv$then_removed <- integer(0)
    rv$gcond_removed <- integer(0)
    rv$gconstr_removed <- integer(0)
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
    rv$cond_removed <- integer(0)
    rv$then_removed <- integer(0)
    rv$gcond_removed <- integer(0)
    rv$gconstr_removed <- integer(0)
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
    recheck_dataset(ed)
  })

  observeEvent(input$rule_up_click, {
    req(editing())
    # Same guard as col_up_click: no move under a search.
    req(!nzchar(rv$rule_search))
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
    req(!nzchar(rv$rule_search))
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
  observe({
    req(identical(rv$rule_view, "form"))
    for (i in seq_len(max(1L, rv$cond_n))) ensure_cond_remove_observer("cond", i)
    for (i in seq_len(max(1L, rv$then_n))) ensure_cond_remove_observer("then", i)
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
    rv$gconstr_n <- isolate(rv$gconstr_n) + 1L
    insertUI("#gconstr_rows",
      where = "beforeEnd",
      ui = one_group_constraint_row(isolate(rv$gconstr_n), current_group_condition_names())
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

    cond_names <- current_group_condition_names()

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
      rv$cond_removed <- integer(0)
      rv$then_removed <- integer(0)
      rv$gcond_removed <- integer(0)
      rv$gconstr_removed <- integer(0)
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
    recheck_dataset(ed)
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
      title = paste("Edit details —", ds_title(ed)),
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
        textInput("meta_id", "Dataset ID",
          value = g("id"), width = "100%",
          placeholder = "vs"
        ),
        div(
          class = "msg-hint", style = "margin:-8px 0 12px;",
          paste(
            "The ID identifies the dataset in the specification, in every",
            "validation message and in the exported documents. Changing it",
            "clears this dataset's validation, because results recorded under",
            "the old ID no longer describe it."
          )
        ),
        textInput("meta_name", "Name",
          value = g("name"), width = "100%",
          placeholder = "Vital signs"
        ),
        div(
          class = "msg-hint", style = "margin:-8px 0 12px;",
          "Shown as the heading of this dataset's page and in the dataset list; the ID is shown when it is empty."
        ),
        textAreaInput("meta_description", "Description",
          value = g("description"), width = "100%", rows = 4,
          placeholder = "What this dataset contains, in words"
        ),
        div(
          class = "msg-hint", style = "margin:-8px 0 12px;",
          "Shown under the heading of this dataset's page."
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
      id = input$meta_id,
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

    new_id <- trimws(as.character(input$meta_id %||% "")[1])
    renamed <- !identical(new_id, ed)
    rv$dta <- r$value

    # ORDER MATTERS: the state migration has to land before anything reads the
    # dataset by id again -- invalidate_dataset() looks its status up by id,
    # and every output keyed off rv$active re-renders as soon as it changes.
    if (renamed) {
      rename_dataset_state(ed, new_id)
      # A new id does not change what the data IS, but every stored validation
      # record carries the dataset id it was checked under, so results left in
      # place would report a dataset that no longer exists. The name,
      # description and template fields take no part in validation, so editing
      # only those deliberately leaves a passed check passed.
      invalidate_dataset(new_id)
    }
    # rv$structure is keyed by id and caches each dataset's label, so an id or
    # name change must reach it or the nav keeps showing the old one. Assigning
    # it re-renders every structure-dependent output (dataset nav, detail panel
    # and its file inputs), which is why the column and rule editors never touch
    # it. Here it is assigned unconditionally on purpose: reactiveValues drops an
    # assignment identical to the stored value, so a description- or
    # template-only edit -- which leaves the structure identical -- still
    # re-renders nothing. It does NOT rebuild the whole workspace: output$main
    # depends only on rv$doc_token (load/reset/restore).
    rv$structure <- build_structure(rv$dta)

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
    if (isolate(on_landing())) {
      return(NULL)
    }
    div(
      id = "dta-msgs-dock", class = "msgs-dock collapsed",
      div(
        class = "msgs-dock-bar", onclick = "DTA_toggleMsgsDock(event)",
        role = "button", tabindex = "0", `aria-expanded` = "false",
        onkeydown = "DTA_msgsDockKey(event)",
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
  # output$floating_msgs: the dock is deliberately rendered once per loaded
  # document so the DT inside stays stable, and therefore cannot react to
  # validation state -- but these buttons must.
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

    # Only the formats the library's config allows get a button.
    fmt_on <- function(f) f %in% cfg$exports$validation_messages
    tagList(
      if (fmt_on("csv")) {
        dl_btn("dl_msgs_csv", "CSV", active_ok, if (active_ok) {
          sprintf("Download the validation messages for %s as CSV", ds)
        } else {
          sprintf("Run a check on %s first -- there are no results to export yet", ds)
        })
      },
      if (fmt_on("tsv")) {
        dl_btn("dl_msgs_tsv", "TSV", active_ok, if (active_ok) {
          sprintf("Download the validation messages for %s as TSV", ds)
        } else {
          sprintf("Run a check on %s first -- there are no results to export yet", ds)
        })
      },
      if (fmt_on("xlsx")) {
        dl_btn("dl_msgs_xlsx", "XLSX", active_ok, if (active_ok) {
          sprintf("Download the validation messages for %s as XLSX", ds)
        } else {
          sprintf("Run a check on %s first -- there are no results to export yet", ds)
        })
      },
      # Named apart from the sidebar's "Validation summary": this is the
      # message-level report, that one is the per-dataset outcome.
      if (fmt_on("html")) {
        dl_btn("dl_msgs_html", "Report", any_ok, if (any_ok) {
          "Download the full validation message report"
        } else {
          "Run a check first -- there are no results to report yet"
        })
      }
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
      req(msgs_active_checked(), "csv" %in% cfg$exports$validation_messages)
      utils::write.csv(csv_formula_safe(msgs_dl_df()), file, row.names = FALSE, na = "")
    }
  )
  output$dl_msgs_tsv <- downloadHandler(
    filename = function() paste0(msgs_dl_base(), ".tsv"),
    content = function(file) {
      req(msgs_active_checked(), "tsv" %in% cfg$exports$validation_messages)
      utils::write.table(
        csv_formula_safe(msgs_dl_df()), file,
        sep = "\t", row.names = FALSE, na = "", qmethod = "double"
      )
    }
  )
  output$dl_msgs_xlsx <- downloadHandler(
    filename = function() paste0(msgs_dl_base(), ".xlsx"),
    content = function(file) {
      req(msgs_active_checked(), "xlsx" %in% cfg$exports$validation_messages)
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
      req(msgs_any_checked(), "html" %in% cfg$exports$validation_messages)
      tryCatch(
        do.call(DTAtools::write_validation_report, c(
          list(rv$dta, file, overwrite = TRUE, quiet = TRUE),
          dta_app_export_args(cfg)[c("document_name", "organisation", "organisation_unit")]
        )),
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
      format = paste0("fit SAS format ", .first_nonempty(r[["columnspec_columnspec"]])),
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
    if (length(sel) == 0 || is.null(m) || nrow(m) == 0) {
      return()
    }
    id <- if ("id" %in% names(m)) m$id[sel] else sel
    show_inspect_modal(rv$active, id)
    # Clear the highlight so clicking the SAME row again re-selects (and so
    # re-opens the report) instead of toggling it off and being ignored.
    DT::selectRows(DT::dataTableProxy("msgs"), NULL)
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
    study <- S7::prop(md, "study")
    trf <- function(k) {
      v <- tr[[k]]
      if (is.null(v)) "" else if (inherits(v, "Date")) format(v, "%Y-%m-%d") else as.character(v)[1]
    }
    # Read-only renders static text instead of form controls. Only the LEAF
    # controls swap; every div, layout_columns, card and section title below is
    # shared, so the tab keeps its shape when edit mode toggles and there is one
    # layout to maintain rather than two.
    #
    # editing() is deliberately NOT isolated -- this render has to re-run when
    # edit mode (editing()) toggles. rv$dta stays isolated, as before, so typing
    # in a field
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
      div(class = "md-section-title", "Study"),
      layout_columns(
        col_widths = c(6, 6),
        f_text("md_study_id", "Study ID", study[["id"]] %||% "", width = "100%"),
        f_text("md_protocol_id", "Protocol ID", study[["protocol_id"]] %||% "", width = "100%")
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
      layout_columns(
        col_widths = c(6, 6),
        f_area("tr_extent", "Transfer extent", trf("extent"), width = "100%", rows = 2),
        f_area("tr_scope", "Transfer scope", trf("scope"), width = "100%", rows = 2)
      ),
      layout_columns(
        col_widths = c(6, 6),
        f_area("tr_packaging", "Packaging", trf("packaging"), width = "100%", rows = 2),
        f_area("tr_security", "Security", trf("security"), width = "100%", rows = 2)
      ),
      div(
        class = "msg-hint", style = "margin:-8px 0 10px;",
        "File names and contents of the delivery archives are defined under Containers below."
      ),
      f_text("tr_checksum", "Checksum requirements", trf("checksum"), width = "100%"),
      layout_columns(
        col_widths = c(6, 6),
        f_area("tr_test_requirements", "Test transfer requirements",
          trf("test_requirements"),
          width = "100%", rows = 3
        ),
        f_area("tr_blinding_requirements", "Blinding requirements",
          trf("blinding_requirements"),
          width = "100%", rows = 3
        )
      ),
      tags$hr(),
      div(
        class = "md-section-title d-flex justify-content-between align-items-center",
        tags$span("Containers"),
        if (!ro) actionButton("metadata_manage_containers", "Manage containers\u2026", class = "btn btn-sm btn-outline-primary")
      ),
      uiOutput("metadata_containers"),
      tags$hr(),
      div(
        class = "md-section-title d-flex justify-content-between align-items-center",
        tags$span("Agreement sections"),
        if (!ro) actionButton("agreement_add", "Add section", class = "btn btn-sm btn-outline-primary")
      ),
      uiOutput("agreement_sections"),
      tags$hr(),
      div(class = "md-section-title", "Error handling & corrections"),
      f_area("md_error_handling", "Error handling",
        getf("error_handling"),
        width = "100%", rows = 2,
        placeholder = "How data/format errors are handled and communicated."
      ),
      if (ro) {
        meta_field_text("Authorized for corrections", paste(dta_authorized_values(dta), collapse = ", "))
      } else {
        tagList(
          selectizeInput("md_authorized", "Authorized for corrections",
            choices = dta_authorized_choices(dta),
            selected = dta_authorized_values(dta),
            multiple = TRUE, width = "100%",
            # delimiter: selectize splits typed and pasted text on its
            # delimiter, which defaults to ",", so "Lastname, Firstname" would
            # become two people. Names do not contain ";".
            options = list(
              create = TRUE, delimiter = ";", plugins = list("remove_button"),
              placeholder = "Pick contacts or type a name"
            )
          ),
          div(
            class = "msg-hint", style = "margin:-8px 0 10px;",
            "Choose Receiver or Supplier contacts, type any other name and press Enter (separate several with ;), or leave empty when nobody is named."
          )
        )
      },
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
  # 700ms debounce, so a value typed just before "Stop editing" is chosen still
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
  save_study <- function(field, value) {
    req(editing(), rv$dta)
    r <- dta_set_study_field(rv$dta, field, value)
    if (r$ok) {
      rv$dta <- r$value
      sync_yaml_text()
    } else {
      showNotification(paste("Could not update study", field, "\u2014", r$error), type = "error")
    }
  }
  for (.field in c("id", "protocol_id")) {
    local({
      FIELD <- .field
      inid <- if (FIELD == "id") "md_study_id" else "md_protocol_id"
      d <- debounce(reactive(input[[inid]]), 700)
      observeEvent(d(), save_study(FIELD, d()), ignoreInit = TRUE)
    })
  }
  title_d <- debounce(reactive(input$md_title), 700)
  version_d <- debounce(reactive(input$md_version), 700)
  header_d <- debounce(reactive(input$md_header), 700)
  errh_d <- debounce(reactive(input$md_error_handling), 700)
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
  # ignoreNULL = FALSE: a multiple selectize emptied by the user sends NULL,
  # which must unset the field. The guard makes a re-render (document load,
  # edit toggle) a no-op instead of a spurious edit that would show in the
  # version-history change summary. It compares as SETS on purpose: selectize
  # reports its initial selection in option (DOM) order -- Receiver, Supplier,
  # then manual entries -- not in the stored order, so identical() would
  # rewrite a document just by opening it. The picker has no drag-to-reorder,
  # so a set comparison cannot miss a real edit. No debounce: selectize emits
  # on discrete picks, not per keystroke.
  observeEvent(input$md_authorized,
    {
      req(rv$dta)
      v <- trimws(as.character(input$md_authorized %||% character(0)))
      v <- unique(v[!is.na(v) & nzchar(v)])
      if (!setequal(v, dta_authorized_values(rv$dta))) save_md("authorized_for_corrections", v)
    },
    ignoreInit = TRUE,
    ignoreNULL = FALSE
  )
  # Contacts feed the picker's options; keep them current while editing.
  observeEvent(rv$contacts_token,
    {
      req(editing(), rv$dta)
      updateSelectizeInput(session, "md_authorized",
        choices = dta_authorized_choices(rv$dta),
        selected = dta_authorized_values(rv$dta)
      )
    },
    ignoreInit = TRUE
  )
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
  for (.field in c("extent", "scope", "packaging", "security", "checksum", "test_requirements", "blinding_requirements")) {
    local({
      FIELD <- .field
      inid <- paste0("tr_", FIELD)
      d <- debounce(reactive(input[[inid]]), 700)
      observeEvent(d(), save_tr(FIELD, d()), ignoreInit = TRUE)
    })
  }

  # --- ordered agreement sections -----------------------------------------
  output$agreement_sections <- renderUI({
    rv$md_token
    rv$agreement_sections_token
    req(isolate(rv$dta))
    sections <- DTAtools::metadata(isolate(rv$dta))@sections
    ro <- !editing()
    if (length(sections) == 0L) {
      return(div(class = "msg-hint", "No agreement sections."))
    }
    tags$ol(
      class = "list-group",
      lapply(seq_along(sections), function(i) {
        id <- names(sections)[[i]]
        section <- sections[[id]]
        tags$li(
          class = "list-group-item",
          div(
            class = "d-flex justify-content-between align-items-center",
            tags$strong(section[["title"]]),
            if (!ro) {
              div(
                class = "d-flex gap-2",
                actionButton(paste0("agreement_edit_", id), "Edit", class = "btn btn-sm btn-outline-primary"),
                if (i > 1L) actionButton(paste0("agreement_up_", id), "Move up", class = "btn btn-sm btn-outline-secondary"),
                if (i < length(sections)) actionButton(paste0("agreement_down_", id), "Move down", class = "btn btn-sm btn-outline-secondary"),
                actionButton(paste0("agreement_remove_", id), "Remove", class = "btn btn-sm btn-outline-danger")
              )
            }
          ),
          div(class = "msg-hint", id),
          div(style = "white-space:pre-wrap;", section[["text"]])
        )
      })
    )
  })

  save_section_result <- function(r) {
    req(editing(), rv$dta)
    if (!r$ok) {
      showNotification(paste("Could not update agreement sections \u2014", r$error), type = "error")
      return(FALSE)
    }
    rv$dta <- r$value
    rv$agreement_sections_token <- rv$agreement_sections_token + 1
    sync_yaml_text()
    TRUE
  }

  agreement_edit_flow <- function(id = NULL) {
    req(editing(), rv$dta)
    section <- if (is.null(id)) list() else DTAtools::metadata(rv$dta)@sections[[id]]
    if (is.null(section)) {
      showNotification("The section being edited no longer exists.", type = "error")
      return()
    }
    rv$editing_section <- list(id = id)
    showModal(modalDialog(
      title = if (is.null(id)) "Add agreement section" else "Edit agreement section",
      textInput("agreement_id", "Stable ID",
        value = id %||% "",
        placeholder = "e.g. transfer_scope"
      ),
      div(class = "msg-hint", "Start with a letter; use only letters, digits and underscores."),
      textInput("agreement_title", "Title", value = section[["title"]] %||% ""),
      textAreaInput("agreement_text", "Text", value = section[["text"]] %||% "", rows = 8, width = "100%"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("agreement_save", "Save section", class = "btn btn-primary")
      ),
      easyClose = TRUE
    ))
  }
  observeEvent(input$agreement_add, agreement_edit_flow())
  observeEvent(input$agreement_save, {
    req(editing(), rv$dta, rv$editing_section)
    r <- dta_save_agreement_section(
      rv$dta, input$agreement_id, input$agreement_title, input$agreement_text,
      previous_id = rv$editing_section$id
    )
    if (save_section_result(r)) {
      rv$editing_section <- NULL
      removeModal()
    }
  })
  observeEvent(input$agreement_remove_confirm, {
    req(editing(), rv$dta, rv$removing_section)
    if (save_section_result(dta_remove_agreement_section(rv$dta, rv$removing_section))) {
      rv$removing_section <- NULL
      removeModal()
    }
  })
  observeEvent(rv$doc_token,
    {
      rv$editing_section <- NULL
      rv$removing_section <- NULL
    },
    ignoreInit = TRUE
  )

  agreement_registry <- new.env(parent = emptyenv())
  observe({
    rv$md_token
    rv$agreement_sections_token
    req(isolate(rv$dta))
    for (id in names(DTAtools::metadata(isolate(rv$dta))@sections)) {
      for (action in c("edit", "remove", "up", "down")) {
        inid <- paste0("agreement_", action, "_", id)
        if (!is.null(agreement_registry[[inid]])) next
        agreement_registry[[inid]] <- TRUE
        local({
          ID <- id
          ACTION <- action
          INID <- inid
          observeEvent(input[[INID]],
            {
              req(editing(), rv$dta)
              if (ACTION == "edit") {
                agreement_edit_flow(ID)
              } else if (ACTION == "remove") {
                section <- DTAtools::metadata(rv$dta)@sections[[ID]]
                if (is.null(section)) {
                  showNotification("The section to remove no longer exists.", type = "error")
                  return()
                }
                rv$removing_section <- ID
                showModal(modalDialog(
                  title = "Remove agreement section?",
                  tagList("Remove ", tags$b(section[["title"]]), " (", ID, ")?"),
                  footer = tagList(
                    modalButton("Cancel"),
                    actionButton("agreement_remove_confirm", "Remove", class = "btn btn-danger")
                  ),
                  easyClose = TRUE
                ))
              } else {
                save_section_result(dta_move_agreement_section(rv$dta, ID, ACTION))
              }
            },
            ignoreInit = TRUE
          )
        })
      }
    }
  })

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
          div(
            class = "d-flex gap-1",
            actionButton(
              paste0("mvup_", side, "_", i), "▲",
              class = "btn btn-sm btn-outline-secondary dta-row-btn dta-move-btn",
              title = "Move up", `aria-label` = "Move up", disabled = i == 1L
            ),
            actionButton(
              paste0("mvdn_", side, "_", i), "▼",
              class = "btn btn-sm btn-outline-secondary dta-row-btn dta-move-btn",
              title = "Move down", `aria-label` = "Move down", disabled = i == length(cs)
            ),
            actionButton(
              paste0("rm_", side, "_", i), "Remove",
              class = "btn btn-sm btn-outline-danger"
            )
          )
        )
      })
    )
  }

  # Open a pre-filled modal to edit one contact's details.
  #
  # A repeat request for the dialog that is already open is dropped, not
  # re-shown. The click guard (click_guard_script(), ui_components.R) swallows
  # a double-click on a healthy server, but a starved one can take longer to
  # acknowledge the first click than the guard holds the link, and then every
  # further click a waiting user makes lands in one burst once the server
  # catches up. Each showModal() in that burst would tear the dialog down and
  # put it back -- the "popped up several times" the guard exists to prevent
  # -- so the second and later requests for the same contact return here.
  #
  # rv$editing_contact is the record of which contact's dialog is open. It is
  # set here, cleared by a successful Save (below), and cleared by the
  # client's modal-state report (dta_modal_state, observer below) for every
  # way a dialog can leave the screen that the server never sees itself:
  # Cancel, Esc, the backdrop, or another dialog taking its place. The
  # data-dta-modal stamp is what lets that report tell this dialog from any
  # other, and the per-open data-dta-modal-id nonce is what lets it tell THIS
  # opening from an earlier one whose 'hidden' report arrives late.
  contact_modal_seq <- 0L
  edit_contact_flow <- function(side, index) {
    req(editing())
    target <- list(side = side, index = index)
    if (identical(rv$editing_contact, target)) {
      return()
    }
    p <- dta_contact_at(isolate(rv$dta), side, index)
    if (is.null(p)) {
      return()
    }
    rv$editing_contact <- target
    contact_modal_seq <<- contact_modal_seq + 1L
    nonce <- paste0("contact-", contact_modal_seq)
    rv$editing_contact_nonce <- nonce
    showModal(tagAppendAttributes(
      modalDialog(
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
      ),
      `data-dta-modal` = "edit_contact",
      `data-dta-modal-id` = nonce
    ))
  }

  # The browser's report of a modal arriving or leaving (modal_state_js, top
  # of this file). The record is cleared only by news about the dialog it
  # describes: that very opening going away ('hidden' with its nonce), or a
  # dialog of another kind being shown in its place. A 'hidden' from any
  # earlier dialog -- including an earlier contact dialog -- can arrive after
  # the server has already opened the next one, and clearing on it would
  # leave Save with no record and nothing to do. A Save clears the record
  # itself; its removeModal()'s 'hidden' then finds nothing to clear.
  observeEvent(input$dta_modal_state, {
    st <- input$dta_modal_state
    closed <- identical(st$event, "hidden") &&
      !is.null(rv$editing_contact_nonce) &&
      identical(as.character(st$id %||% ""), rv$editing_contact_nonce)
    replaced <- identical(st$event, "shown") && !identical(st$kind, "edit_contact")
    if (closed || replaced) {
      rv$editing_contact <- NULL
      rv$editing_contact_nonce <- NULL
    }
  })

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
      rv$editing_contact_nonce <- NULL
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
    # renderUI wrappers below their dependency on editing().
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

  # dynamic edit/move/remove observers for contacts
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
        for (dir in c("up", "dn")) {
          mvid <- paste0("mv", dir, "_", side, "_", i)
          if (is.null(contact_rm_registry[[mvid]])) {
            contact_rm_registry[[mvid]] <- TRUE
            local({
              SIDE <- side
              IDX <- i
              DIR <- if (dir == "up") "up" else "down"
              ID <- mvid
              observeEvent(input[[ID]],
                {
                  req(editing())
                  r <- dta_move_contact(rv$dta, SIDE, IDX, DIR)
                  if (r$ok) {
                    rv$dta <- r$value
                    rv$contacts_token <- rv$contacts_token + 1
                    sync_yaml_text()
                  } else {
                    showNotification(r$error, type = "error")
                  }
                },
                ignoreInit = TRUE
              )
            })
          }
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
      # With a findings table (a refused document) list every finding on its
      # own line; the error text alone is one wrapped paragraph.
      if (is.data.frame(m$findings) && nrow(m$findings) > 0) {
        div(
          class = "yaml-valid err", HTML("&#x2716;"),
          " Not applied — the specification has errors:",
          dta_findings_ui(m$findings)
        )
      } else {
        div(class = "yaml-valid err", HTML("&#x2716;"), " ", m$error)
      }
    }
  })

  # The standing "Spec check": every finding of the loaded document, errors
  # and warnings, kept current as it is edited. Follows rv$dta only -- never
  # rv$doc_token, so it cannot rebuild output$main -- and renders NULL when
  # clean so nothing stale is left in the DOM.
  output$spec_check <- renderUI({
    if (is.null(rv$dta)) {
      return(NULL)
    }
    f <- dta_lint(rv$dta)
    if (nrow(f) == 0) {
      return(NULL)
    }
    div(
      class = "spec-check",
      div(class = "section-label", "Spec check"),
      dta_findings_ui(f)
    )
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
  yaml_editable <- reactive(editing() && isTRUE(cfg$features$raw_yaml_editable))

  output$yaml_edit_hint <- renderUI({
    if (!yaml_editable()) {
      if (editing()) {
        return(div(
          class = "msg-hint",
          HTML("The raw YAML is <b>read-only</b> in this deployment.")
        ))
      }
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
    if (!yaml_editable()) {
      return(NULL)
    }
    div(
      class = "yaml-edit-actions",
      actionButton("apply_yaml", "Apply changes", class = "btn btn-sm btn-primary"),
      actionButton("revert_yaml", "Revert", class = "btn btn-sm btn-outline-secondary")
    )
  })

  observe({
    ro <- !yaml_editable()
    if (requireNamespace("shinyAce", quietly = TRUE)) {
      shinyAce::updateAceEditor(session, "raw_yaml_editor", readOnly = ro)
    } else if (ro) {
      shinyjs::disable("raw_yaml_editor")
    } else {
      shinyjs::enable("raw_yaml_editor")
    }
  })

  observeEvent(input$apply_yaml, {
    req(editing(), cfg$features$raw_yaml_tab, isTRUE(cfg$features$raw_yaml_editable))
    txt <- input$raw_yaml_editor %||% ""
    if (!nzchar(trimws(txt))) {
      rv$yaml_msg <- list(ok = FALSE, error = "The editor is empty — nothing to apply.")
      return()
    }
    res <- dta_read_yaml_text(txt)
    if (!isTRUE(res$ok)) {
      rv$yaml_msg <- list(
        ok = FALSE, error = res$error %||% "Invalid YAML or DTA.",
        findings = res$findings
      )
      showNotification(paste("YAML not applied:", res$error), type = "error", duration = 10)
      return()
    }
    # Smart reconcile: keep uploads for datasets whose file handlers are
    # unchanged, and re-key them (dta_match_handlers()) when handlers changed,
    # unloading only records whose handler is gone. Keep validation only when
    # columns/rules are also unchanged, and drop uploads for datasets that were
    # removed from the document.
    new_dta <- res$value
    old_dta <- isolate(rv$dta)
    old_uploads <- isolate(rv$uploads)
    old_status <- isolate(rv$status)
    prev_active <- isolate(rv$active)
    new_names <- dta_dataset_names(new_dta)
    old_names <- if (is.null(old_dta)) character(0) else dta_dataset_names(old_dta)

    new_uploads <- list()
    recheck <- character(0) # datasets whose data survived a changed spec
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
      if (!specs_same) recheck <- c(recheck, nm)

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
        paste0("Dataset YAML wrapped into a new ", cfg$document$abbr, " (loaded files kept where a slot remains for them).")
      } else {
        paste(cfg$document$abbr, "YAML applied (loaded files kept where a slot remains for them).")
      },
      type = "message"
    )
    notify_spec_check(new_dta)
    # Data kept under a changed spec is re-validated against it, once.
    if (length(recheck) > 0) run_check(recheck)
  })

  # --- export -------------------------------------------------------------
  # helper: write a UTF-8 YAML string to a download file (binary-safe)
  write_yaml_download <- function(text, file) {
    con <- file(file, "wb")
    on.exit(close(con))
    writeLines(enc2utf8(text), con, useBytes = TRUE)
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
    filename = function() dta_yaml_export_filename(rv$dta),
    content = function(file) {
      req(rv$dta, cfg$features$dta_yaml_download)
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
      req(rv$dta, rv$active, cfg$features$dataset_yaml_download)
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
      # No button points here, but the URL is reachable: honour the export gate.
      req(rv$dta, export_choices$allowed("word", word_mode = "builtin"))
      res <- dta_export(export_dta(), file, "docx", doc_args = dta_app_export_args(cfg))
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

  # What the library's config lets the export dialog offer, and the one place
  # the dialog's inputs are turned into an effective choice. The server does
  # not trust the client to have honoured the dialog: a missing input (control
  # not rendered) falls back to the configured default, and a forced PDF is
  # forced here, not only in the markup.
  export_choices <- export_document_options(cfg)
  export_selection <- function() {
    list(
      format = input$export_format %||% export_choices$default_format,
      as_pdf = if (export_choices$pdf_forced) {
        TRUE
      } else if (export_choices$pdf_choice) {
        isTRUE(input$export_as_pdf)
      } else {
        FALSE
      },
      word_mode = input$export_word_mode %||% export_choices$default_mode
    )
  }

  # Open export modal
  observeEvent(input$export_modal_open, {
    req(rv$dta, export_choices$any)
    templates <- export_choices$templates

    modal_content <- div(
      # No heading here: modalDialog() renders `title` as the dialog's own
      # header, so an h4 repeating those words printed the dialog name twice,
      # one line above the other. Every other modal here relies on `title`
      # alone.
      p(paste0("Choose the format and options for your ", cfg$document$abbr, " export.")),
      h5("Format", class = "text-muted"),
      radioButtons("export_format", NULL,
        choices = export_choices$format_choices,
        selected = export_choices$default_format
      ),
      # Markdown options
      conditionalPanel(
        condition = "input.export_format == 'markdown'",
        hr(),
        h5("Markdown Options", class = "text-muted"),
        if (export_choices$pdf_choice) {
          checkboxInput("export_as_pdf", "Export as PDF (via Pandoc)", value = export_choices$default_pdf)
        } else {
          # One kind only: keep the input (the YAML option below reads it) but
          # do not offer a choice. export_selection() decides server-side.
          shinyjs::hidden(checkboxInput("export_as_pdf", NULL, value = export_choices$pdf_forced))
        },
        if (export_choices$pdf_forced) p("The document is exported as PDF (via Pandoc).", class = "text-muted"),
        # A PDF cannot hide the YAML (see export_do), so the option is only
        # offered for a Markdown file.
        conditionalPanel(
          condition = "!input.export_as_pdf",
          checkboxInput("export_include_yaml_md", "Include YAML in document (hidden at end)", value = FALSE)
        )
      ),
      # Word options
      conditionalPanel(
        condition = "input.export_format == 'word'",
        hr(),
        h5("Word Document Options", class = "text-muted"),
        radioButtons("export_word_mode", NULL,
          choices = export_choices$mode_choices,
          selected = export_choices$default_mode
        ),
        conditionalPanel(
          condition = "input.export_word_mode == 'custom'",
          if (length(templates) > 0) {
            selectInput("export_template_select",
              "Available templates:",
              choices = templates,
              selected = templates[[1]]
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
    sel <- export_selection()
    ext <- if (identical(sel$format, "markdown")) {
      if (sel$as_pdf) ".pdf" else ".md"
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
    sel <- export_selection()
    if (!isTRUE(export_choices$allowed(sel$format, sel$as_pdf, sel$word_mode))) {
      showNotification("This export format is not available in this deployment.", type = "error")
      return()
    }

    stem <- dta_export_stem(rv$dta)
    # Computed ONCE: export_dta() re-parses the version baseline and runs a
    # full dta_diff(), and the Word-with-custom-template branch below would
    # otherwise pay for that up to five times in a single export.
    doc <- export_dta()

    tryCatch(
      {
        if (identical(sel$format, "markdown")) {
          # Markdown export
          # Named for what is delivered: Markdown unless a PDF is actually
          # produced below, which renames it.
          filename <- paste0(stem, ".md")
          # The browser fetches the file on a LATER request, so the path it
          # waits on must be unique. Deriving it from title and date meant two
          # untitled exports on the same day shared one path, and whichever
          # session wrote last was the one both downloads received.
          # `filename` still decides what the browser saves it as.
          output_file <- tempfile(pattern = "dta-export-", fileext = ".md")
          own("export", output_file)

          # write_dta() throws on error (caught below); it does not return $ok.
          do.call(DTAtools::write_dta, c(
            list(doc, output_file, format = "md", overwrite = TRUE, quiet = TRUE),
            dta_app_export_args(cfg)
          ))

          # The YAML is embedded as an HTML comment, which only a .md file
          # can hide: the pandoc routes drop it, but the built-in renderer of
          # route 3 would print its lines as text. The checkbox is therefore
          # not offered alongside PDF, and a value it kept from before PDF was
          # ticked is ignored -- also when the PDF falls back to Markdown.
          if (isTRUE(input$export_include_yaml_md) && !sel$as_pdf) {
            md_text <- readLines(output_file, warn = FALSE)
            md_text <- paste(md_text, collapse = "\n")
            md_text <- embed_yaml_markdown(md_text, doc)
            writeLines(enc2utf8(md_text), output_file, useBytes = TRUE)
          }

          # Convert to PDF if requested, best route first:
          #   1. pandoc + a real PDF engine (LaTeX/TinyTeX/wkhtmltopdf) - best
          #      typography when it is available.
          #   2. headless Chrome/Edge printing the rendered HTML - needs no
          #      LaTeX and no extra R packages, just an installed browser.
          #   3. R's own PDF device - needs nothing, so it always runs.
          # Only if all three fail is the Markdown file delivered instead.
          if (sel$as_pdf) {
            pdf_file <- sub("\\.md$", ".pdf", output_file)
            own("export", pdf_file)
            pdf_ok <- FALSE
            has_pandoc <- requireNamespace("rmarkdown", quietly = TRUE) &&
              rmarkdown::pandoc_available()

            # Route 1: pandoc + LaTeX/wkhtmltopdf engine.
            if (has_pandoc && has_pdf_engine()) {
              pdf_ok <- tryCatch(
                {
                  # output_file is markdown built from document text a third
                  # party controls (metadata title, dataset description,
                  # error_handling, contact names, ...): the filter strips
                  # raw HTML, images, maths and metadata, so a `\input{}` or
                  # a `<script>` planted in one cannot reach the PDF or
                  # whatever headless browser renders it, while <br> in a
                  # table cell still becomes a line break. See
                  # pandoc_markdown_filter().
                  md_filter <- pandoc_markdown_filter()
                  tryCatch(
                    rmarkdown::pandoc_convert(
                      input = normalizePath(output_file),
                      from = pandoc_markdown_reader(),
                      to = "pdf",
                      output = pdf_file,
                      options = c("--lua-filter", md_filter)
                    ),
                    finally = unlink(md_filter)
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
                error = function(e) FALSE
              )
            }

            # Route 3: R's own PDF device (dta_markdown_to_pdf(), the same
            # renderer dta_export_pdf() falls back to). Needs no external
            # tool, so a server with neither LaTeX nor a browser -- Posit
            # Connect, typically -- still gets a PDF, in a simpler layout.
            if (!isTRUE(pdf_ok)) {
              pdf_ok <- tryCatch(
                {
                  dta_markdown_to_pdf(readLines(output_file, warn = FALSE), pdf_file)
                  dta_is_pdf(pdf_file)
                },
                error = function(e) FALSE
              )
            }

            if (isTRUE(pdf_ok) && file.exists(pdf_file)) {
              output_file <- pdf_file
              filename <- sub("\\.md$", ".pdf", filename)
            } else {
              # Only PDF is offered here (exports.document has markdown_pdf but not
              # markdown): delivering a .md would hand out a format the
              # deployment chose not to offer, so nothing is delivered.
              if (!"markdown" %in% cfg$exports$document) {
                cli::cli_abort("Could not produce a PDF on this server, and plain Markdown is not available in this deployment.")
              }
              showNotification(
                "Could not produce a PDF on this server. Exporting Markdown instead.",
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
          own("export", output_file)

          if (identical(sel$word_mode, "custom")) {
            template_name <- input$export_template_select
            template_path <- get_template_path(template_name, cfg)
            if (is.null(template_path)) {
              cli::cli_abort("Template file not found. It may have been deleted.")
            }

            # Prepare template variables (dataset/specs content). Always supply
            # {YAML_EMBEDDED} so the placeholder is cleanly filled (or blanked)
            # rather than left as literal text in the document.
            #
            # {DATASETS_DETAIL} is deliberately NOT supplied here: it is now a
            # block placeholder that export_with_template() itself renders as
            # real Word tables. Adding a "{DATASETS_DETAIL}" entry to
            # `variables` would override that block back to flat text, which
            # would silently undo this feature for the app.
            variables <- list(
              "{DATASETS_SUMMARY}" = format_datasets_summary(doc),
              "{YAML_EMBEDDED}" = "",
              "{DOCUMENT_NAME}" = cfg$document$label,
              "{DOCUMENT_ABBREVIATION}" = cfg$document$abbr,
              "{ORGANISATION_NAME}" = cfg$organisation$name,
              "{ORGANISATION_UNIT}" = cfg$organisation$unit
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
              fallback = FALSE,
              overwrite = TRUE
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
            do.call(DTAtools::write_dta, c(
              list(
                doc, output_file,
                format = "docx", overwrite = TRUE, quiet = TRUE,
                include_yaml = !is.null(yaml_text), yaml_text = yaml_text
              ),
              dta_app_export_args(cfg)
            ))
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
        # The message only: as.character(e) also carries the deparsed call,
        # which shows the browser server-side paths and internals.
        showNotification(
          paste("Export failed:", conditionMessage(e)),
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
    if (!isTRUE(ok) || !cfg$exports$validation_summary) {
      return(NULL)
    }
    # The same predicate as the report's banner: a packaging finding means this
    # is not a pass, however well the content validated.
    fnd <- findings_now()
    outside <- any(fnd$status == "outside")
    complete <- length(validated) == length(st) && nrow(fnd) == 0
    # No divider of its own: this button belongs to the sidebar's Export
    # group, whose label sits above the two export buttons it follows.
    tagList(
      downloadButton(
        "dl_validation_summary",
        # The state is in the words, not only the colour.
        if (outside) {
          "Validation summary (FAILED: packaging)"
        } else if (nrow(fnd) > 0) {
          "Validation summary (INCOMPLETE: packaging)"
        } else {
          "Validation summary"
        },
        class = if (outside) {
          "btn btn-outline-danger w-100"
        } else if (complete) {
          "btn btn-success w-100"
        } else {
          "btn btn-warning w-100"
        },
        title = if (outside) {
          paste(
            "Download a summary of this validation.",
            "Files arrived outside the container the agreement requires, so it",
            "will report the validation as failed."
          )
        } else if (complete) {
          "Download a summary certifying this successful validation"
        } else if (nrow(fnd) > 0) {
          paste(
            "Download a summary of this validation.",
            "The packaging of some files is not verified, so it will report the",
            "validation as incomplete rather than passed."
          )
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

  # The summary's HTML, with the packaging findings of the live document: an
  # outside finding fails the banner, an unverified one leaves it incomplete.
  validation_report_html <- function() {
    args <- dta_app_export_args(cfg)
    dta_build_validation_report(
      rv$dta, rv$status,
      document_name = args$document_name, organisation = args$organisation,
      organisation_unit = args$organisation_unit,
      findings = findings_now()
    )
  }

  output$dl_validation_summary <- downloadHandler(
    filename = function() {
      paste0("validation_summary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
    },
    content = function(file) {
      req(rv$dta, cfg$exports$validation_summary)
      writeLines(validation_report_html(), file, useBytes = TRUE)
    }
  )

  # --- reset --------------------------------------------------------------
  observeEvent(input$reset_app, {
    showModal(modalDialog(
      title = "Start over?",
      paste("This clears the loaded", cfg$document$abbr, "and all uploads from this session."),
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
    rv$last_delivery <- NULL
    rv$pending_delivery <- NULL
    rv$main_view <- "dataset"
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
    # "Start over" is the user's own wipe: everything holding their data
    # goes now rather than at the end of the retention window.
    try(unlink(session_file() %||% character(0)), silent = TRUE)
    try(session_tables()$clear(), silent = TRUE)
    # ... except what a second tab that restored the same autosave still owns.
    unlink(dta_not_owned_elsewhere(c(owned$upload, owned$export), session$token),
      recursive = TRUE, force = TRUE
    )
    owned$upload <<- character(0)
    owned$export <<- character(0)
    own("autosave", character(0)) # re-register what is left
    removeModal()
  })

  # --- status / summary outputs ------------------------------------------
  output$dataset_status_line <- renderUI({
    req(rv$active)
    st <- dta_lookup(rv$status, rv$active, "pending")
    # The chip reads the CONTENT status unless a file of this dataset has a
    # packaging finding: then it says so, and never plain Passed.
    pk <- dv_ds_packaging(findings_now(), rv$active)
    s <- rv$structure[[rv$active]]
    type_txt <- if (!is.null(s) && !is.na(s$type)) s$type else ""
    ds <- dta_get_dataset(rv$dta, rv$active)
    desc <- tryCatch(ds@description, error = function(e) NULL) %||% ""
    heading <- dta_dataset_label(ds)
    if (!nzchar(heading)) heading <- rv$active
    div(
      class = paste0("ds-head-block ds-st-", status_display(st, pk)),
      div(
        class = "ds-head",
        tags$h4(class = "ds-desc", heading)
      ),
      div(
        style = "display:flex; align-items:center; gap:8px; margin-bottom:6px; flex-wrap:wrap;",
        # Every dataset carries a tag with its id (alongside the type tag), so
        # the dataset is always clearly identified on the Datasets page --
        # also when the heading above is its name.
        tags$span(class = "dta-name-chip", title = "Dataset ID", rv$active),
        if (nzchar(type_txt)) tags$span(class = "status-chip status-pending", type_txt),
        status_chip(st, pk)
      ),
      if (nzchar(desc)) tags$p(class = "ds-long-desc", desc)
    )
  })

  output$loaded_files <- renderUI({
    req(rv$active, rv$structure)
    rv$uploads
    rv$status # re-render when files or validation state change
    dsname <- rv$active
    s <- rv$structure[[dsname]]
    tstatus <- dta_table_status_map(rv$dta, dsname)
    # Packaging findings of this dataset (the live state, not the last check's).
    fnd <- findings_now()
    fnd <- fnd[fnd$dataset == dsname, , drop = FALSE]
    # The chip a file carries when its packaging is not what the agreement asks.
    finding_chip <- function(table) {
      f <- fnd[fnd$item == table | basename(fnd$item) == table, , drop = FALSE]
      if (nrow(f) == 0) {
        return(NULL)
      }
      f <- f[1, , drop = FALSE]
      need <- if (is.na(f$container)) "" else paste0("; required inside ", f$container)
      txt <- switch(f$status,
        outside = if (identical(f$arrived, "loose")) {
          paste0("\u26A0 delivered loose", need)
        } else {
          paste0("\u26A0 came in ", f$arrived, need)
        },
        unverified = paste0("\u25D0 checked on its own", need),
        paste0("? origin not recorded", need)
      )
      tags$span(class = paste("file-finding", paste0("ff-", f$status)), title = f$message, txt)
    }
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
          if (!is.null(rec$container)) tags$span(class = "file-table", paste0("from ", rec$container)),
          finding_chip(rec$table),
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
        paste("Untitled", cfg$document$abbr)
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
    # Datasets with data bound but never checked, plus datasets with no
    # status at all: everything the three counts above do not cover.
    npend <- max(n - np - nf - nd, 0L)
    # A content pass whose files have a packaging finding is no pass: it moves
    # out of the passed count into the packaging segment (red when a file is
    # outside its container, amber when it is only unverified).
    fnd <- findings_now()
    pk <- vapply(names(st)[st == "pass"], function(id) dv_ds_packaging(fnd, id), character(1))
    npk <- sum(pk != "none")
    np <- np - npk
    pk_cls <- if (any(pk == "outside")) "fail" else "nodata"
    # One bar segment per non-zero state, sized by its share of the datasets.
    seg <- function(k, cls) {
      if (k > 0) span(class = paste("status-seg", cls), style = sprintf("flex: %d 1 0;", k))
    }
    item <- function(k, cls, label) {
      if (k > 0) {
        span(
          class = paste("status-item", cls),
          span(class = "status-dot"), span(class = "status-n", k), label
        )
      }
    }
    div(
      class = "status-summary",
      div(
        class = "status-bar", role = "img",
        `aria-label` = sprintf(
          "%d passed, %d with a packaging issue, %d failed, %d without data, %d not checked",
          np, npk, nf, nd, npend
        ),
        seg(np, "seg-pass"), seg(npk, paste0("seg-", pk_cls)), seg(nf, "seg-fail"),
        seg(nd, "seg-nodata"), seg(npend, "seg-pending")
      ),
      div(
        class = "status-legend",
        span(
          class = "status-total",
          span(class = "status-n", n), if (n == 1) " dataset" else " datasets"
        ),
        item(np, "st-pass", " passed"),
        # Datasets, like every other count here and the bar segment beside it.
        item(npk, paste0("st-", pk_cls), if (npk == 1) " packaging issue" else " packaging issues"),
        item(nf, "st-fail", " failed"),
        item(nd, "st-nodata", " no data"),
        item(npend, "st-pending", " not checked")
      )
    )
  })

  # --- the Delivery block of the sidebar -----------------------------------
  # The packaging findings of the LIVE document, computed once for every reader
  # (the sidebar lines, the loaded-file chips, the Delivery view, the report
  # banner, the summary button). Not check()'s attribute: that one only exists
  # after a run and goes stale with the next edit.
  findings_now <- reactive(dv_findings(rv$dta))

  # The block shows for every document with a dataset: it also holds the
  # sidebar's way into the container manager, which a one-dataset document needs
  # as much as any. A flag the conditionalPanel reads, not a uiOutput:
  # re-rendering the block would rebuild its fileInput and lose the binding. It
  # is never suspended -- the panel is hidden when it is FALSE, and a suspended
  # output would never tell the client it became TRUE.
  delivery_zone_on <- reactive({
    !is.null(rv$dta) && length(dta_dataset_names(rv$dta)) > 0
  })
  # A render function proper (not `output$x <- reactive()`): a test session reads
  # an output through its two-argument call, which a bare reactive does not take.
  output$delivery_zone <- shiny::markRenderFunction(
    uiFunc = function(...) NULL,
    renderFunc = function(shinysession, name, ...) delivery_zone_on()
  )
  outputOptions(output, "delivery_zone", suspendWhenHidden = FALSE)

  output$delivery_panel <- renderUI({
    req(rv$dta)
    dv_panel_ui(rv$dta, findings_now(), rv$last_delivery, editing = editing())
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
  # loads (rv$doc_token bumps), which is the same moment a fresh page
  # visit's own visibility snapshot can be unreliable, so it is included here
  # for the same belt-and-braces reason as the sidebar outputs above.
  for (sidebar_output in c(
    "workspace_header", "summary_metrics", "dataset_nav_ui",
    "add_dataset_ui", "validation_report_ui", "edit_gate",
    "delivery_panel"
  )) {
    outputOptions(output, sidebar_output, suspendWhenHidden = FALSE)
  }

  # --- dataset detail (structure only -> stable file inputs) --------------
  output$dataset_detail <- renderUI({
    # rv$structure is an empty list -- not NULL -- for a document with no
    # datasets, which is exactly what keeps output$main above on the
    # workspace rather than the landing page (on_landing() asks is.null(),
    # deliberately, and not the length). So this is the workspace's own
    # empty state for the no-datasets case, reached both right after
    # "Create new" and after removing a document's last dataset; without it,
    # req(rv$active) below fails silently and the whole Datasets tab body
    # renders nothing.
    req(rv$structure)
    if (length(rv$structure) == 0) {
      return(div(
        class = "msg-hint",
        "This specification has no datasets yet. Use + Add dataset in the sidebar to create the first one, and fill in the rest of the document on the Metadata tab."
      ))
    }
    # The Delivery view stands in for the dataset's slots after a sidebar drop. It
    # reads the document, so this render depends on rv$dta only while it is
    # shown; the slots below must not (a re-render would rebuild their file
    # inputs).
    if (identical(rv$main_view, "delivery")) {
      req(rv$dta)
      return(dv_view_ui(rv$dta, rv$last_delivery, findings_now(), isolate(rv$active)))
    }
    req(rv$active)
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
        # h$container is the EFFECTIVE container: the entry's own, else the default.
        # The zone takes this dataset's files; a loose file is checked on its own.
        in_container <- nzchar(h$container %||% "")
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
              tags$span(class = "slot-count", h$count),
              if (in_container) tags$span(class = "slot-count", paste0("in ", h$container))
            )
          ),
          card_body(
            if (nzchar(h$hint)) div(class = "slot-meta", style = "margin-bottom:8px;", h$hint),
            if (in_container) {
              div(
                class = "slot-meta", style = "margin-bottom:8px;",
                paste0(
                  "Must arrive inside ", h$container, ". Drop that archive on Delivery (sidebar) to load every dataset, or drop ",
                  h$prose, " here to check it on its own."
                ),
                if (nzchar(h$container_description %||% "")) {
                  tags$div(class = "msg-hint", h$container_description)
                }
              )
            },
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
      uiOutput("dataset_conflict_warning"),
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
        if (cfg$features$dataset_yaml_download) {
          downloadButton("dl_ds_yaml", "Export DataSet YAML",
            class = "btn btn-outline-primary",
            title = "Download this dataset's specification as YAML"
          )
        }
      ),
      # div(
      #  class = "msg-hint", style = "margin:-6px 0 12px;",
      #  "Uploads are validated against the file handler as you add them."
      # ),
      # tags$h5("Expected files"),
      div(
        class = "msg-hint", style = "margin:-4px 0 10px;",
        "Drop this dataset's files below. To load a whole delivery for every dataset, drop it on Delivery in the sidebar."
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
    if (isolate(on_landing())) {
      # Landing. Reference input$dta_client_id directly so this re-renders once
      # the browser reports its id and the restore button can appear.
      restore_available <- {
        input$dta_client_id
        sf <- session_file()
        !is.null(sf) && file.exists(sf)
      }
      # The row exists only if the library offers at least one way to start
      # (restoring a session is core, not configurable).
      start_buttons <- list(
        if (cfg$features$create_blank) {
          actionButton("create_new", "Create new", class = "btn btn-outline-primary")
        },
        if (cfg$features$create_from_template) {
          actionButton("create_from_template", "Create new from template", class = "btn btn-primary")
        },
        if (cfg$features$load_example) {
          actionButton("load_example", "Load example", class = "btn btn-outline-primary")
        },
        if (restore_available) {
          actionButton("restore_session", "Restore previous session",
            class = "btn btn-outline-secondary"
          )
        }
      )
      start_buttons <- Filter(Negate(is.null), start_buttons)
      div(
        class = "landing",
        div(
          class = "landing-hero",
          tags$h1(class = "landing-title", cfg$branding$landing_headline),
          p(class = "landing-lede", cfg$branding$landing_lede)
        ),
        div(
          class = "landing-drop",
          HTML(landing_drop_glyph),
          tags$h2(class = "landing-drop-title", if (isTRUE(cfg$document$custom)) paste("Load a", cfg$document$abbr, "file") else "Load a DTA / DTS specification file"),
          div(
            class = "landing-drop-hint",
            "A full DTA with metadata or a standalone dataset specification is accepted. A dataset-only file opens without a Metadata section."
          ),
          div(
            class = "dropzone",
            fileInput("dta_file", "Drop a .yaml / .yml file here, or browse",
              accept = c(".yaml", ".yml"), width = "100%"
            )
          )
        ),
        if (length(start_buttons) > 0) {
          div(
            class = "landing-alt",
            span(class = "landing-alt-label", "Other ways to start"),
            div(class = "landing-alt-actions", start_buttons)
          )
        }
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
          # Under the datasets it feeds, before the check that reads them: the
          # sidebar reads top to bottom as what is expected, load it, check it.
          # Shown for every document with a dataset. The drop zone is static:
          # only the flag behind the panel changes.
          conditionalPanel(
            "output.delivery_zone",
            div(
              class = "delivery-block",
              div(class = "section-label", "Delivery"),
              div(
                class = "dropzone delivery-drop",
                fileInput("delivery_files",
                  label = "Drop or choose the whole delivery",
                  multiple = TRUE, width = "100%"
                )
              ),
              uiOutput("delivery_panel")
            )
          ),
          actionButton("check_all", "Check all datasets", class = "btn btn-primary w-100"),
          tags$hr(),
          div(class = "section-label", "Export"),
          if (cfg$features$dta_yaml_download) {
            downloadButton("dl_yaml", paste("Export", cfg$document$abbr, "YAML"), class = "btn btn-outline-primary w-100")
          },
          if (export_choices$any) {
            actionButton("export_modal_open", paste("Export", cfg$document$abbr), class = "btn btn-primary w-100")
          },
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
            if (cfg$features$raw_yaml_tab) {
              nav_panel(
                "Raw YAML",
                div(
                  class = "yaml-edit-bar",
                  uiOutput("yaml_edit_hint"),
                  uiOutput("yaml_edit_actions")
                ),
                uiOutput("yaml_validation_msg"),
                uiOutput("spec_check"),
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
                      readOnly = !isolate(yaml_editable())
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
                  if (isolate(yaml_editable())) ta else shinyjs::disabled(ta)
                }
              )
            }
          )
          # The id lets the server switch tabs: a sidebar drop opens the Delivery
          # view on Datasets even while Metadata is showing.
          do.call(navset_card_tab, c(list(id = "main_tabs"), Filter(Negate(is.null), panels)))
        }
      )
    }
  })

  # --- restore previous session ------------------------------------------
  # The fallback of a restore whose document cannot be upgraded: the document
  # read again from its YAML text, with the data bound in `old` carried over
  # (and its validation, where the dataset's specification is unchanged). NULL
  # when there is no usable text.
  rebuild_from_yaml <- function(old, yaml_text) {
    if (!is.character(yaml_text) || length(yaml_text) != 1 || !nzchar(yaml_text)) {
      return(NULL)
    }
    r <- dta_read_yaml_text(yaml_text)
    if (!isTRUE(r$ok)) {
      return(NULL)
    }
    new <- r$value
    for (nm in dta_dataset_names(new)) {
      old_ds <- tryCatch(old@datasets[[nm]], error = function(e) NULL)
      if (is.null(old_ds)) next
      same_specs <- identical(dta_specs_signature(old_ds), dta_specs_signature(dta_get_dataset(new, nm)))
      moved <- dta_transfer_bound_data(new, nm, old_ds, keep_validation = same_specs)
      if (isTRUE(moved$ok)) new <- moved$value
    }
    new
  }

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
    # The filename already folds in session$user (dta_session_key()); this
    # is the backstop for a file that reached that name anyway -- e.g. one
    # written while signed in as someone else and then copied or planted.
    if (!identical(saved$user, session$user)) {
      showNotification("Cannot restore a session saved by a different user.", type = "error")
      return()
    }
    # Prefer the saveRDS-safe dump (tables as Arrow files, or data.frames in
    # older session files); fall back to a legacy `dta` field.
    restored <- if (!is.null(saved$dump)) {
      tryCatch(
        dta_restore_session(saved$dump, table_dir = attr(session_tables(), "dir")),
        error = function(e) NULL
      )
    } else {
      saved$dta
    }
    if (is.null(restored)) {
      showNotification("Could not restore the previous session.", type = "error")
      return()
    }
    # A document saved by an older version has the classes it was saved with:
    # re-stamp them with the current ones. When that fails, rebuild the document
    # from its YAML text and carry the bound data over.
    upgraded <- tryCatch(DTAtools::upgrade_dta(restored), error = function(e) NULL)
    from_yaml <- is.null(upgraded)
    if (from_yaml) {
      upgraded <- rebuild_from_yaml(restored, saved$yaml_text)
      if (is.null(upgraded)) {
        showNotification("Could not restore the previous session.", type = "error")
        return()
      }
    }
    restored <- upgraded
    if (!is.null(saved$dump)) {
      store <- session_tables()
      store$adopt(restored, saved$dump)
      own("autosave", c(sf, attr(store, "dir")))
    }
    rv$dta <- restored
    rv$yaml_text <- saved$yaml_text
    # Rebuilt, not read back: what a slot shows (the effective container) follows
    # the restored document, which an older save may not have described.
    rv$structure <- build_structure(restored)
    rv$last_delivery <- saved$last_delivery
    rv$pending_delivery <- NULL
    rv$main_view <- "dataset"
    ups <- saved$uploads %||% list()
    rv$uploads <- lapply(ups, function(recs) {
      if (length(recs) == 0) {
        return(list())
      }
      if (is.character(recs)) {
        # Legacy session files stored a bare character vector of paths. Re-derive
        # the table name with dta_bound_item_name() rather than a bare
        # file_path_sans_ext(): a compression suffix has to come off first, so a
        # restored "x.csv.gz" binds to table "x" exactly as load_file() would.
        lapply(recs, function(f) list(file = f, table = dta_bound_item_name("tabular", f)))
      } else {
        recs
      }
    })
    # The extracted files a restored file dataset re-reads belong to this
    # session again: the retention sweep spares what a live session owns, and
    # cleanup and the budget only look at owned directories.
    roots <- dta_delivery_roots(rv$uploads)
    if (length(roots) > 0) own("upload", roots)
    restored_names <- dta_dataset_names(restored)
    rv$status <- saved$status %||% stats::setNames(
      rep("pending", length(restored_names)),
      restored_names
    )
    # character(0)[1] is not zero-length -- it is a length-one
    # NA_character_ -- and this app's %||% only substitutes for NULL or a
    # genuinely zero-length value, so for a document with no datasets that
    # NA would sail straight through instead of becoming the NULL every
    # other rv$active assignment uses (see apply_loaded() above, which
    # guards the same length before indexing). Left as NA, rv$active leaks
    # into user-facing text -- the validation dock tooltip, download
    # filenames like "NA_validation_messages" -- and the next autosave
    # writes it back out, so once it appears it survives further reloads.
    #
    # `saved$active` is read through the same lens rather than trusted: a
    # session file written before this fix can already CARRY the NA, and
    # %||% would not substitute it there either, so a document saved by the
    # older build would keep leaking the NA no matter how carefully the
    # fallback below is computed. Treating a missing value as no selection
    # is right whatever wrote the file.
    saved_active <- saved$active
    if (length(saved_active) == 1L && is.na(saved_active)) saved_active <- NULL
    rv$active <- saved_active %||%
      (if (length(restored_names) > 0) restored_names[1] else NULL)
    rv$dataset_only <- isTRUE(saved$dataset_only)
    rv$is_example <- isTRUE(saved$is_example)
    # A session file written before the versioning feature existed has none
    # of these fields. The fallback is to treat the restored document as a
    # loaded one that has not yet had a version created (version_locked =
    # TRUE, with a baseline taken from the saved yaml_text below). Editing is
    # restored separately from saved$editing further down.
    rv$version_locked <- if (is.null(saved$version_locked)) TRUE else isTRUE(saved$version_locked)
    # %||% cannot tell "this field is absent because the session file
    # predates the versioning feature" from "the field is present and
    # legitimately NULL" -- and NULL is legitimate here: apply_loaded()
    # stores NULL for a document CREATED in this session (versioned =
    # FALSE), and new_version_confirm() relies on that NULL to re-baseline
    # at the moment of the first version bump. Coalescing an absent field
    # to saved$yaml_text would fold edits made before that bump into the
    # new version's change summary, but only when the author happened to
    # reload in between -- so this reuses is.null(saved$version_locked),
    # the same pre-versioning-file test the line above already makes,
    # rather than reaching for %||%. The version_entry_index assignment
    # just below draws the identical absent-vs-NULL line for the same
    # reason (its comment reads "NOT %||% -- NULL is a legitimate value
    # here").
    rv$version_baseline_yaml <- if (is.null(saved$version_locked)) {
      saved$yaml_text
    } else {
      saved$version_baseline_yaml
    }
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
    rv$editing <- isTRUE(saved$editing) && cfg$features$editing
    rv$md_token <- rv$md_token + 1
    rv$contacts_token <- rv$contacts_token + 1
    rv$doc_token <- rv$doc_token + 1
    showNotification(
      if (from_yaml) {
        "Previous session restored from its YAML text: it was saved by an older version of the app. Run Check again to refresh the results."
      } else {
        "Previous session restored."
      },
      type = if (from_yaml) "warning" else "message", duration = if (from_yaml) 10 else 5
    )
    # A restored RDS object skips every constructor, so it is checked here
    # rather than trusted; the findings stay listed in the Spec check panel.
    notify_spec_check(restored, "Restored session")
  })
}

shinyApp(ui, server)
