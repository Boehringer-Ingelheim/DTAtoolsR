# -----------------------------------------------------------------------------
# Theme: modern Boehringer Ingelheim brand palette
# -----------------------------------------------------------------------------
# NOTE: Replace the hex values below with the official Boehringer Ingelheim
# brand palette where available. The values here are a BI-style green/teal
# family chosen to look on-brand and modern.
#
# Type is IBM Plex Sans (body/headings) and IBM Plex Mono (identifiers/code),
# loaded from Google Fonts at run time with a Segoe UI / system fallback --
# same mechanism as before, just a different family. --bs-font-monospace is
# the single monospace stack the rest of this sheet uses.

BI <- list(
  green = "#00625B", # primary brand green/teal (deep)
  green_dark = "#003D38",
  green_light = "#E6F1EF",
  accent = "#00A886", # brighter accent green
  ink = "#1A2B2A", # near-black text
  grey = "#5B6B6A",
  grey_light = "#F4F7F6",
  # Semantic status colors (harmonized with the BI green family)
  pass = "#1E8E5A",
  pass_bg = "#E5F4EC",
  pass_border = "#9BD3B4",
  fail = "#C0392B",
  fail_bg = "#FBEAE7",
  fail_border = "#E9B4AC",
  pending = "#5B6B6A",
  pending_bg = "#EEF1F1",
  pending_border = "#CDD5D4"
)

# bslib theme --------------------------------------------------------------
bi_theme <- function() {
  bslib::bs_theme(
    version = 5,
    bg = "#FFFFFF",
    fg = BI$ink,
    primary = BI$green,
    secondary = BI$grey,
    success = BI$pass,
    danger = BI$fail,
    base_font = bslib::font_collection(
      bslib::font_google("IBM Plex Sans", wght = c(400, 500, 600), local = FALSE),
      "Segoe UI", "system-ui", "-apple-system", "Roboto", "sans-serif"
    ),
    heading_font = bslib::font_collection(
      bslib::font_google("IBM Plex Sans", wght = c(400, 500, 600), local = FALSE),
      "Segoe UI", "system-ui", "sans-serif"
    ),
    code_font = bslib::font_collection(
      bslib::font_google("IBM Plex Mono", wght = c(400, 500), local = FALSE),
      "ui-monospace", "SFMono-Regular", "Consolas", "Menlo", "monospace"
    ),
    "border-radius" = ".5rem",
    "border-radius-sm" = ".375rem",
    "border-radius-lg" = ".75rem",
    "card-border-radius" = ".75rem",
    "headings-font-weight" = "600",
    "btn-font-weight" = "500"
  )
}

# Extra CSS ----------------------------------------------------------------
bi_css <- function() {
  root <- sprintf(
    ":root {
      --bi-green: %s; --bi-green-dark: %s; --bi-green-light: %s;
      --bi-accent: %s; --bi-ink: %s; --bi-grey: %s; --bi-grey-light: %s;
      --bi-pass: %s; --bi-pass-bg: %s; --bi-pass-border: %s;
      --bi-fail: %s; --bi-fail-bg: %s; --bi-fail-border: %s;
      --bi-pending: %s; --bi-pending-bg: %s; --bi-pending-border: %s;
    }",
    BI$green, BI$green_dark, BI$green_light,
    BI$accent, BI$ink, BI$grey, BI$grey_light,
    BI$pass, BI$pass_bg, BI$pass_border,
    BI$fail, BI$fail_bg, BI$fail_border,
    BI$pending, BI$pending_bg, BI$pending_border
  )
  shiny::HTML(paste0(root, "
    body { background: var(--bi-grey-light); color: var(--bi-ink); }
    /* Low-key inline code -- e.g. a filename or extension inside running
       text (a msg-hint, a modal body). */
    code {
      font-family: var(--bs-font-monospace); font-size: .85em;
      color: var(--bi-green-dark); background: var(--bi-green-light);
      padding: .1em .35em; border-radius: .3em;
    }
    /* Keyboard-only focus rings: :focus-visible so a mouse click never
       shows one, only Tab navigation does. */
    .btn:focus-visible, .action-button:focus-visible,
    .nav-link:focus-visible, a:focus-visible {
      outline: 2px solid var(--bi-accent); outline-offset: 2px; box-shadow: none;
    }
    .form-control:focus, .form-select:focus {
      border-color: var(--bi-accent); box-shadow: 0 0 0 3px rgba(0,168,134,.25);
    }
    /* Flat deep green, no gradient; the accent rule along the bottom edge is
       the logo's own frame colour, carried down into the bar it sits on. */
    .app-brandbar {
      background: var(--bi-green-dark);
      color: #fff; padding: 10px 24px; display: flex; align-items: center;
      gap: 14px; min-height: 60px;
      border-bottom: 3px solid var(--bi-accent); box-shadow: none;
    }
    .app-brandbar .brand-logo { height: 36px; width: auto; display: block; flex: none; border-radius: 8px; }
    .app-brandbar .brand-title { font-weight: 600; font-size: 1.0625rem; letter-spacing: 0; line-height: 1.2; }
    .app-brandbar .brand-sub { opacity: .78; font-size: .8125rem; line-height: 1.3; }
    /* align-items: center -- without it a flex container defaults to
       'stretch', so the Edit menu toggle and the status pill (whose heights
       come from their own content) and the .brand-link pills (padding:
       5px 12px below) end up on different baselines instead of one centre
       line. */
    .app-actions { margin-left: auto; display: flex; align-items: center; gap: 8px; }
    .app-actions .brand-link {
      color: #fff; text-decoration: none; font-weight: 500;
      border: 1px solid rgba(255,255,255,.35);
      background: rgba(255,255,255,.06);
      border-radius: 999px;
      padding: 5px 12px;
      font-size: .8125rem;
      line-height: 1.2;
      white-space: nowrap;
      transition: background .15s ease, border-color .15s ease, color .15s ease;
    }
    .app-actions .brand-link:hover,
    .app-actions .brand-link:focus {
      color: #fff;
      background: rgba(255,255,255,.14);
      border-color: rgba(255,255,255,.6);
      text-decoration: none;
    }
    /* create_new_version_button() is an actionButton(), which renders
       <button class='btn btn-default action-button'> -- none of the
       .brand-link rule above applies to it (that rule targets <a>), and
       Bootstrap's own .btn supplies a background and border of its own on
       top. Re-state the .brand-link look property by property so the
       button reads as the same pill as the links either side of it. */
    .app-actions .brand-action {
      background: rgba(255,255,255,.06);
      border: 1px solid rgba(255,255,255,.35);
      border-radius: 999px;
      padding: 5px 12px;
      font-size: .8125rem;
      line-height: 1.2;
      font-weight: 500;
      color: #fff;
      white-space: nowrap;
      transition: background .15s ease, border-color .15s ease, color .15s ease;
    }
    .app-actions .brand-action:hover,
    .app-actions .brand-action:focus {
      color: #fff;
      background: rgba(255,255,255,.14);
      border-color: rgba(255,255,255,.6);
    }
    /* The edit-menu toggle keeps the .brand-action pill look above, but as
       a Bootstrap dropdown-toggle it also draws its own caret. Bootstrap
       colours that caret's border for a light-background dropdown; on the
       dark brandbar it needs to pick up the pill's own white text instead,
       or it all but disappears against the green. */
    .app-actions .dropdown-toggle::after { border-top-color: currentColor; margin-left: 6px; vertical-align: middle; }
    /* Same pill geometry as .brand-link and .brand-action above, but this
       one is a label rather than a control: a filled, higher-contrast fill
       stands in for the pill outline the interactive elements use, and
       there is no transition and no :hover rule at all. Giving a status
       pill the interactive pills' hover treatment would make it read as
       clickable when there is nothing to click. */
    .app-actions .brand-status {
      border-radius: 999px;
      padding: 5px 12px;
      font-size: .8125rem;
      line-height: 1.2;
      font-weight: 500;
      white-space: nowrap;
      background: rgba(255,255,255,.2);
      border: 1px solid rgba(255,255,255,.5);
      color: #fff;
    }
    /* The menu body opens over the light page below the dark bar, so it
       keeps Bootstrap's own light surface -- only stacking and offset need
       setting here. The bar no longer casts a shadow of its own (it once
       did, which is what first made the z-index necessary), but the menu
       still has to stack above the page content and the sidebar layout
       beneath the bar, so the z-index stays. .app-brandbar also sets
       no overflow, which is what lets the menu escape the bar's own box at
       all -- worth recording because giving .app-brandbar an overflow:
       hidden later would silently clip this menu without this rule itself
       changing at all. */
    .app-actions .dropdown-menu { z-index: 1080; margin-top: 6px; }
    /* edit_gate renders both the edit-menu dropdown and the status pill
       into this one uiOutput() slot, so unlike a typical single-widget
       uiOutput() it can hold two flex children side by side (or none at
       all on the landing page -- see the :empty rule below). shiny-
       html-output's own display is block; left alone that breaks the
       single centre line .app-actions' align-items: center establishes
       for its direct children, because the dropdown/pill inside it would
       be centred within their own block instead of against their
       brandbar siblings. gap: 8px separates those two children from each
       other, matching the gap .app-actions itself uses between its own
       direct children so the whole row reads as one consistent rhythm. */
    .app-actions > .shiny-html-output { display: flex; align-items: center; gap: 8px; }
    /* ...but on the landing page edit_gate renders NULL, and Shiny empties
       the shiny-html-output span without ever removing it: the element
       stays in the DOM with no children at all. The rule above would then
       leave a generated, zero-width flex item sitting first in
       .app-actions, and gap: 8px applies around flex items whether or not
       they have any width -- so the brandbar would carry 8px of blank space
       before the Report issues link with nothing in it. display: none
       removes the box altogether, which is what drops the gap with it.
       Keep this AFTER the rule above: :empty already outranks it on
       specificity, but source order is what makes that safe to rely on.
       NB every line of this CSS block is inside one double-quoted R string
       -- an apostrophe is fine here, a double quote ends the string. */
    .app-actions > .shiny-html-output:empty { display: none; }
    @media (max-width: 900px) {
      .app-brandbar { flex-wrap: wrap; }
      .app-actions {
        width: 100%;
        margin-left: 0;
        justify-content: flex-start;
        flex-wrap: wrap;
      }
      /* Keep the Edit menu toggle and the status pill at their own intrinsic
         size instead of stretching or shrinking when the actions row wraps
         onto a second line. Both now share the slot, so both need it. */
      .app-actions .brand-action,
      .app-actions .brand-status { flex: 0 0 auto; }
    }

    /* Landing: a centred hero and one large drop target instead of a form in
       a card -- the document-with-tick glyph is the page's only illustration. */
    .landing { max-width: 760px; margin: 40px auto 24px; padding: 0 12px; }
    .landing-hero { text-align: center; margin-bottom: 28px; }
    .landing-title {
      font-size: 1.875rem; font-weight: 600; line-height: 1.2; letter-spacing: -.01em;
      color: var(--bi-green-dark); margin: 0 0 12px;
    }
    .landing-lede { font-size: 1rem; color: var(--bi-grey); line-height: 1.55; max-width: 62ch; margin: 0 auto; }
    .landing-drop {
      background: #fff; border: 1px solid var(--bi-pending-border); border-radius: .75rem;
      padding: 28px 28px 24px; text-align: center;
    }
    .landing-drop-glyph { width: 44px; height: 44px; display: block; margin: 0 auto 10px; }
    .landing-drop-glyph .glyph-doc { stroke: var(--bi-green); }
    .landing-drop-glyph .glyph-tick { stroke: var(--bi-accent); }
    .landing-drop-title { font-size: 1.125rem; font-weight: 600; color: var(--bi-ink); margin: 0 0 4px; }
    .landing-drop-hint { font-size: .8125rem; color: var(--bi-grey); margin: 0 auto 18px; max-width: 56ch; }
    .landing-drop .dropzone { max-width: 560px; margin: 0 auto; text-align: center; }
    /* Centred like the glyph, title and hint above it. Bootstrap gives an
       .input-group's .form-control flex: 1 1 auto and a 1% width, so
       justify-content alone would centre nothing. */
    .landing-drop .dropzone .input-group { justify-content: center; }
    .landing-drop .dropzone .form-control { flex: 0 1 auto; width: auto; max-width: 24ch; }
    .landing-alt { display: flex; align-items: center; justify-content: center; gap: 10px 14px; flex-wrap: wrap; margin-top: 22px; }
    .landing-alt-label { font-size: .875rem; color: var(--bi-grey); }
    .landing-alt-actions { display: flex; gap: 8px; flex-wrap: wrap; justify-content: center; }
    /* The drop target is this page's one call to action. The alternatives
       under it keep their own classes (the template button's btn-primary is
       another session's to change), but none of them gets the solid fill
       here, or the eye lands on the secondary row before the target. */
    .landing-alt-actions .btn-primary {
      --bs-btn-color: var(--bi-green); --bs-btn-bg: #fff; --bs-btn-border-color: var(--bi-green);
      --bs-btn-hover-color: #fff; --bs-btn-hover-bg: var(--bi-green); --bs-btn-hover-border-color: var(--bi-green);
      --bs-btn-active-color: #fff; --bs-btn-active-bg: var(--bi-green-dark); --bs-btn-active-border-color: var(--bi-green-dark);
    }
    @media (max-width: 900px) {
      .landing { margin-top: 20px; }
      .landing-title { font-size: 1.5rem; }
    }

    /* Status chips: an outline with a coloured dot, not a tinted fill. The
       tint was the one place the fill language survived the rail redesign,
       and it also failed AA (the pass green on its own tint measures 3.65:1
       at 12px); ink on white is 14.8:1, and the dot carries the colour. */
    .status-chip {
      display: inline-flex; align-items: center; gap: 6px;
      padding: 2px 9px; border-radius: 999px; font-size: .75rem;
      font-weight: 500; border: 1px solid var(--bi-pending-border);
      background: #fff; color: var(--bi-ink); white-space: nowrap;
    }
    .status-pass    { border-color: var(--bi-pass-border); }
    .status-fail    { border-color: var(--bi-fail-border); }
    .status-pending { border-color: var(--bi-pending-border); }
    .status-nodata  { border-color: #EBD9B6; }
    .status-dot { width: 8px; height: 8px; border-radius: 50%; background: currentColor; }
    .status-pass .status-dot    { background: var(--bi-pass); }
    .status-fail .status-dot    { background: var(--bi-fail); }
    .status-pending .status-dot { background: var(--bi-pending); }
    .status-nodata .status-dot  { background: #C77700; }

    /* Upload slot */
    .slot-card { background: #fff; }
    .slot-meta { font-size: .8125rem; color: var(--bi-grey); }
    .slot-expected { font-family: var(--bs-font-monospace); font-size: .8125rem;
      background: var(--bi-green-light); color: var(--bi-green-dark);
      padding: 1px 6px; border-radius: 6px; }
    .slot-ok    { color: var(--bi-pass); font-weight: 600; }
    .slot-warn  { color: #B26A00; font-weight: 600; }
    .slot-example .control-label { font-size: .8125rem; color: var(--bi-grey); font-weight: 500; }
    .slot-card .card-header strong { font-weight: 600; }
    /* Replaces the old bullet-plus-count text appended to .slot-expected
       (3.5, app.R): a quiet pill so the count reads as metadata, not prose. */
    .slot-count {
      display: inline-block; margin-left: 8px; font-size: .75rem; color: var(--bi-grey);
      background: var(--bi-grey-light); border: 1px solid var(--bi-pending-border);
      border-radius: 999px; padding: 1px 8px; vertical-align: middle;
    }

    /* Shiny binds drag-and-drop on fileInput()'s own .input-group and toggles
       shiny-file-input-active / shiny-file-input-over on it while a file is
       dragged over -- so the padded dashed area IS the .input-group itself.
       A bigger wrapper around it would look droppable without being
       droppable: the drop target has to be the element Shiny is actually
       watching. */
    .dropzone .form-group { margin-bottom: 0; }
    /* A fileInput() with no width argument gets Shiny's default 300px
       container; the zone should fill whatever column it sits in, and the
       .slot-example / .landing-drop rules cap that column instead. */
    .dropzone .shiny-input-container { width: 100%; }
    .dropzone .control-label { font-size: .8125rem; font-weight: 500; color: var(--bi-grey); margin-bottom: 6px; }
    .dropzone .input-group {
      display: flex; align-items: center; gap: 10px; width: 100%;
      padding: 14px 16px; border: 2px dashed rgba(0,98,91,.7);
      border-radius: .625rem; background: #fff;
      transition: background .12s ease, border-color .12s ease;
    }
    .dropzone .input-group:hover { border-color: var(--bi-green); }
    .dropzone .input-group.shiny-file-input-active,
    .dropzone .input-group.shiny-file-input-over {
      background: var(--bi-green-light); border-color: var(--bi-accent);
    }
    .dropzone .input-group-btn { margin: 0; }
    .dropzone .btn-file {
      border: 1px solid var(--bi-green) !important; color: var(--bi-green);
      background: #fff; border-radius: .375rem !important; font-weight: 500;
      font-size: .875rem; padding: .35rem .8rem;
    }
    .dropzone .btn-file:hover { background: var(--bi-green-light); }
    /* min-width: 0 lets the read-only filename field shrink inside the flex
       row on a narrow screen (a flex item's default min-width is its content
       width, which would push the Browse button out of the zone instead);
       the ellipsis then trims a long filename rather than clipping it. */
    .dropzone .form-control {
      border: 0 !important; background: transparent !important; box-shadow: none !important;
      padding: 0; color: var(--bi-grey); font-size: .9rem; border-radius: 0;
      min-width: 0; text-overflow: ellipsis;
    }
    /* Suppress the native fileInput progress / 'Upload complete' bar: a finished
       byte transfer is NOT acceptance. Acceptance is shown only by the app's own
       per-slot state after matches_filename() + load_file() succeed and verify. */
    .dropzone .progress, .dropzone .shiny-file-input-progress { display: none !important; }

    .msg-hint { font-size: .8125rem; color: var(--bi-grey); line-height: 1.45; }

    /* bslib layout_sidebar() renders one bordered, rounded outer box
       (.bslib-sidebar-layout) holding the sidebar and the main area -- frame
       inside frame if the sidebar and the tab card each kept a border of
       their own too. Make the outer box invisible and let the sidebar and
       the main tab card each be its own panel instead. */
    .bslib-sidebar-layout {
      border: 0; background: transparent; border-radius: 0;
      --bslib-sidebar-bg: #fff; --bslib-sidebar-fg: var(--bi-ink);
    }
    .bslib-sidebar-layout > .sidebar {
      border: 1px solid var(--bi-pending-border); border-radius: .75rem; background: #fff;
    }
    .bslib-sidebar-layout > .sidebar > .sidebar-content { padding: 44px 18px 18px; gap: 10px; }
    /* The 44px above only exists to clear bslib's collapse toggle, which
       overlays the top RIGHT corner -- so on a wide screen the header just
       keeps clear of it on the right and the two panels start on the same
       line. Below 768px bslib moves the toggle to the top left, where the
       full-width clearance still earns its keep. */
    @media (min-width: 768px) {
      .bslib-sidebar-layout > .sidebar > .sidebar-content { padding-top: 14px; }
      .bslib-sidebar-layout > .sidebar .workspace-header { padding-right: 34px; }
    }
    /* Same fix as the brandbar's .shiny-html-output:empty rule above -- an
       empty uiOutput (add_dataset_ui when not editing, validation_report_ui
       before a check) would otherwise still keep its share of the sidebar's
       column gap. */
    .bslib-sidebar-layout > .sidebar > .sidebar-content > .shiny-html-output:empty { display: none; }
    .bslib-sidebar-layout > .sidebar hr { margin: 8px 0; border-top: 1px solid var(--bi-pending-border); opacity: 1; }
    .bslib-sidebar-layout > .sidebar .btn.w-100 { padding: .45rem .75rem; font-size: .9375rem; }
    /* One filled call to action per surface: only Check all datasets keeps
       the solid fill. Export DTA and the Validation summary download keep
       their Bootstrap classes -- and so their ids, observers and tests --
       but read as outlines here, Export DTA in the brand green and the
       summary in the pass green; the summary's btn-warning state (some
       datasets never checked) keeps an amber edge so an incomplete run still
       looks different from a complete one. Bootstrap 5 buttons are painted
       from their own custom properties, which is what lets a class like
       btn-primary be re-skinned from outside without a specificity fight. */
    .bslib-sidebar-layout > .sidebar #export_modal_open {
      --bs-btn-color: var(--bi-green); --bs-btn-bg: transparent; --bs-btn-border-color: var(--bi-green);
      --bs-btn-hover-color: #fff; --bs-btn-hover-bg: var(--bi-green); --bs-btn-hover-border-color: var(--bi-green);
      --bs-btn-active-color: #fff; --bs-btn-active-bg: var(--bi-green-dark); --bs-btn-active-border-color: var(--bi-green-dark);
    }
    .bslib-sidebar-layout > .sidebar #dl_validation_summary {
      --bs-btn-color: var(--bi-pass); --bs-btn-bg: transparent; --bs-btn-border-color: var(--bi-pass);
      --bs-btn-hover-color: #fff; --bs-btn-hover-bg: var(--bi-pass); --bs-btn-hover-border-color: var(--bi-pass);
      --bs-btn-active-color: #fff; --bs-btn-active-bg: var(--bi-pass); --bs-btn-active-border-color: var(--bi-pass);
    }
    .bslib-sidebar-layout > .sidebar #dl_validation_summary.btn-warning {
      --bs-btn-color: #B26A00; --bs-btn-border-color: #C77700;
      --bs-btn-hover-color: #fff; --bs-btn-hover-bg: #C77700; --bs-btn-hover-border-color: #C77700;
      --bs-btn-active-color: #fff; --bs-btn-active-bg: #B26A00; --bs-btn-active-border-color: #B26A00;
    }
    .bslib-sidebar-layout > .main { padding: 0 0 0 20px; }
    @media (max-width: 767px) { .bslib-sidebar-layout > .main { padding: 12px 0 0; } }

    /* Main tab card + underlined tabs -- four chained classes so this beats
       Bootstrap's own .nav-tabs .nav-link.active on specificity instead of
       needing !important. */
    .bslib-sidebar-layout > .main > .card { border: 1px solid var(--bi-pending-border); border-radius: .75rem; box-shadow: none; }
    .card { border-color: var(--bi-pending-border); }
    .card > .card-header {
      background: #fff; color: var(--bi-ink); font-weight: 600; font-size: .9375rem;
      padding: 10px 16px; border-bottom: 1px solid var(--bi-pending-border);
    }
    .card-header .card-header-tabs { margin: -10px -16px; }
    .card-header .card-header-tabs .nav-link {
      border: 0; border-bottom: 2px solid transparent; border-radius: 0; margin-bottom: -1px;
      padding: 12px 16px; color: var(--bi-grey); font-weight: 500; background: transparent;
    }
    .card-header .card-header-tabs .nav-link:hover { color: var(--bi-green-dark); }
    .card-header .card-header-tabs .nav-link.active {
      color: var(--bi-green-dark); font-weight: 600; background: transparent;
      border-color: transparent; border-bottom-color: var(--bi-green);
    }

    /* Sidebar workspace header -- DTA identity (title / version / date) */
    .workspace-header { margin-bottom: 0; }
    .workspace-header .ws-title {
      font-weight: 600; font-size: 1.0625rem; color: var(--bi-green-dark);
      line-height: 1.25; word-break: break-word;
    }
    .workspace-header .ws-meta {
      font-size: .75rem; color: var(--bi-grey); margin-top: 4px;
      display: flex; gap: 6px; flex-wrap: wrap;
    }
    .workspace-header .ws-pill {
      background: var(--bi-green-light); color: var(--bi-green-dark);
      border-radius: 999px; padding: 1px 9px; font-weight: 600;
    }
    /* Sentence-case group labels: the sidebar's Datasets list and Export
       group, and the Metadata cards' Affiliation/Contacts. */
    .section-label {
      font-size: .75rem; font-weight: 600; letter-spacing: 0;
      text-transform: none; color: var(--bi-grey); margin: 6px 0 4px;
    }

    /* Sidebar status summary: a segmented bar (share of datasets per status)
       plus a legend that only lists the non-zero states. */
    .status-summary { margin: 2px 0 0; }
    .status-bar {
      display: flex; gap: 2px; height: 8px; border-radius: 999px; overflow: hidden;
      background: var(--bi-pending-border);
    }
    .status-seg { display: block; min-width: 6px; }
    .seg-pass { background: var(--bi-pass); }
    .seg-fail { background: var(--bi-fail); }
    .seg-nodata { background: #C77700; }
    .seg-pending { background: var(--bi-pending-border); }
    /* Before anything has been checked the bar would be one grey segment on
       a grey track -- a rule carrying no information -- so it stays hidden
       until the first check gives it something to show; the legend still
       says how many datasets are waiting. */
    .status-bar:has(> .seg-pending:only-child) { display: none; }
    .status-legend {
      display: flex; flex-wrap: wrap; gap: 4px 12px; margin-top: 8px;
      font-size: .8125rem; color: var(--bi-grey);
    }
    .status-legend .status-n { font-weight: 600; color: var(--bi-ink); margin-right: 2px; }
    .status-item { display: inline-flex; align-items: center; gap: 5px; }
    /* .status-dot already exists above for .status-chip (currentColor) --
       these come after it and set the background explicitly instead. */
    .status-item .status-dot { width: 7px; height: 7px; border-radius: 50%; }
    .st-pass .status-dot { background: var(--bi-pass); }
    .st-fail .status-dot { background: var(--bi-fail); }
    .st-nodata .status-dot { background: #C77700; }
    .st-pending .status-dot { background: var(--bi-pending-border); }

    /* Metadata import errors: DTA-level, so they appear nowhere else in the UI
       (the messages dock is per-dataset). Shown above the metadata form. */
    .md-import-warn {
      border: 1px solid var(--bi-fail-border); background: var(--bi-fail-bg);
      color: var(--bi-fail); border-radius: 8px; padding: 10px 14px;
      margin-bottom: 14px; font-size: .86rem;
    }
    .md-import-warn-head { font-weight: 600; margin-bottom: 4px; }
    .md-import-warn ul { margin: 0; padding-left: 18px; }

    /* Raw YAML syntax-highlighted view (dark editor theme) */
    .yaml-view {
      margin: 0; background: #0d1117; color: #c9d1d9; padding: 14px 16px;
      border-radius: 8px; max-height: 70vh; overflow: auto;
      font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
      font-size: .84rem; line-height: 1.55; white-space: pre; tab-size: 2;
    }
    .yaml-view code { font: inherit; color: inherit; background: none; padding: 0; }
    .yaml-view .yml-key     { color: #7ee787; }
    .yaml-view .yml-str     { color: #a5d6ff; }
    .yaml-view .yml-num     { color: #f2cc60; }
    .yaml-view .yml-bool    { color: #ff7b72; }
    .yaml-view .yml-comment { color: #8b949e; font-style: italic; }
    .yaml-view .yml-punct   { color: #c9d1d9; }
    .yaml-view .yml-dash    { color: #ff7b72; }

    /* Editable raw YAML: dark editor textarea + Apply/Revert bar + result banner */
    .yaml-edit-bar { display: flex; justify-content: space-between; align-items: flex-start; gap: 12px; margin-bottom: 8px; }
    .yaml-edit-bar .msg-hint { flex: 1 1 auto; }
    .yaml-edit-actions { flex: none; display: flex; gap: 6px; }
    textarea#raw_yaml_editor {
      background: #0d1117; color: #c9d1d9; border: 1px solid #30363d; border-radius: 8px;
      font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
      font-size: .84rem; line-height: 1.55; tab-size: 2; white-space: pre;
      /* A plain <textarea> is natively resizable (drag handle, bottom-right
         corner) -- min-height only needs to match rows=28's taller starting
         point (app.R), nothing else is needed for the drag itself. */
      min-height: 70vh; overflow: auto;
    }
    textarea#raw_yaml_editor:focus { border-color: var(--bi-accent); box-shadow: 0 0 0 2px rgba(0,168,134,.25); }
    .yaml-valid { font-size: .82rem; padding: 7px 10px; border-radius: 8px; margin-bottom: 8px; border: 1px solid transparent; }
    .yaml-valid.ok  { color: var(--bi-pass); background: var(--bi-pass-bg); border-color: var(--bi-pass-border); }
    .yaml-valid.err { color: var(--bi-fail); background: var(--bi-fail-bg); border-color: var(--bi-fail-border); white-space: pre-wrap; }

    /* Loaded-files list: one row per bound file (name + table + status + trash) */
    .loaded-slot { margin-bottom: 12px; }
    .loaded-slot-head { margin-bottom: 5px; }
    .loaded-file-row {
      display: flex; align-items: center; gap: 10px;
      padding: 7px 10px; margin-bottom: 6px;
      border: 1px solid var(--bi-pending-border); border-left-width: 4px;
      border-radius: .5rem; background: #fff;
    }
    /* :has() reads the status icon's own class for the rail colour; browsers
       without :has() support just keep the neutral border-left above. */
    .loaded-file-row:has(.file-ok) { border-left-color: var(--bi-pass); }
    .loaded-file-row:has(.file-fail) { border-left-color: var(--bi-fail); }
    .loaded-file-row:has(.file-unknown) { border-left-color: #b8860b; }
    .loaded-file-row .file-name { font-weight: 600; color: var(--bi-ink); word-break: break-all; }
    .loaded-file-row .file-table {
      font-family: var(--bs-font-monospace); font-size: .8rem;
      color: var(--bi-green-dark); background: var(--bi-green-light);
      padding: 1px 7px; border-radius: 6px; white-space: nowrap;
    }
    .loaded-file-row .file-status { font-weight: 600; width: 1.2em; text-align: center; flex: none; }
    .loaded-file-row .file-ok      { color: var(--bi-pass); }
    .loaded-file-row .file-fail    { color: var(--bi-fail); }
    .loaded-file-row .file-pending { color: var(--bi-grey); }
    /* Validated, but the import axis was never checked -- amber, so it reads
       as neither the green pass nor the red fail. */
    .loaded-file-row .file-unknown { color: #b8860b; }
    .loaded-file-row .file-remove {
      margin-left: auto; flex: none; color: var(--bi-fail);
      border: none; background: transparent; padding: 2px 7px; line-height: 1; font-size: 1rem;
    }
    .loaded-file-row .file-remove:hover { background: var(--bi-fail-bg); border-radius: 6px; }

    /* Validation messages: compact table + download buttons + top filters */
    .msgs-dl .btn { margin-left: 6px; }
    .msgs-table table.dataTable { font-size: .8rem; }
    .msgs-table table.dataTable td, .msgs-table table.dataTable th { padding: 5px 8px; }
    .msgs-table .dataTables_wrapper { font-size: .82rem; }
    .msgs-table .dataTables_filter input, .msgs-table .dataTables_length select { font-size: .82rem; }
    .msgs-table table.dataTable thead .form-control,
    .msgs-table table.dataTable thead .form-select,
    .msgs-table table.dataTable thead input,
    .msgs-table table.dataTable thead select { font-size: .78rem; padding: 2px 6px; }

    /* Sidebar dataset navigation: the LEFT RAIL and the icon encode status
       (not-checked/pass/fail/no-data); the background encodes selection only
       (a brand tint), so the two never collide. */
    .dataset-nav-list { display: flex; flex-direction: column; gap: 4px; margin-bottom: 2px; }
    .dataset-nav-row {
      display: flex; align-items: center; gap: 8px;
      padding: 6px 6px 6px 10px;
      border: 1px solid transparent; border-left: 4px solid var(--bi-pending-border);
      border-radius: .5rem; background: #fff;
      transition: background .12s ease, border-color .12s ease;
    }
    .dataset-nav-row:hover { background: var(--bi-grey-light); }
    .dataset-nav-row.active {
      background: var(--bi-green-light);
      border-top-color: rgba(0,98,91,.25); border-right-color: rgba(0,98,91,.25);
      border-bottom-color: rgba(0,98,91,.25); box-shadow: none;
    }
    .dataset-nav-row.nav-st-pending { border-left-color: var(--bi-pending-border); }
    .dataset-nav-row.nav-st-pass    { border-left-color: var(--bi-pass); }
    .dataset-nav-row.nav-st-fail    { border-left-color: var(--bi-fail); }
    .dataset-nav-row.nav-st-nodata  { border-left-color: #C77700; }
    .dataset-nav-row .nav-select {
      flex: 1 1 auto; display: flex; align-items: center; gap: 8px;
      text-decoration: none; color: var(--bi-ink); font-weight: 600; overflow: hidden;
    }
    .dataset-nav-row .nav-select:hover { text-decoration: none; }
    .dataset-nav-row .nav-name { word-break: break-word; color: var(--bi-ink); font-weight: 500; }
    .dataset-nav-row.active .nav-name { color: var(--bi-green-dark); font-weight: 600; }
    .nav-ic { flex: none; width: 1.15em; text-align: center; font-weight: 600; font-size: .95rem; }
    .nav-ic-pass    { color: var(--bi-pass); }
    .nav-ic-fail    { color: var(--bi-fail); }
    .nav-ic-nodata  { color: #C77700; }
    .nav-ic-pending { color: var(--bi-pending); }
    .dataset-nav-row .nav-check {
      flex: none; color: var(--bi-green); border: 1px solid transparent;
      background: transparent; padding: 1px 7px; line-height: 1.3;
      border-radius: .375rem; font-size: .8rem;
    }
    .dataset-nav-row .nav-check:hover { background: #fff; border-color: var(--bi-pass-border); }

    /* Add dataset sits directly above Check all datasets (btn-primary, the
       sidebar's actual call to action) -- btn-sm + btn-outline-secondary
       alone still draws a full-width bordered box the same size as that
       button, so this drops the border and dims the text further at rest,
       leaving only a hover/focus state as the affordance that it is
       clickable at all. Reads as a small addition to the list above it, not
       a second primary action. */
    .add-dataset-btn {
      border-color: transparent; background: transparent; color: var(--bi-grey);
      font-size: .82rem; font-weight: 500;
    }
    .add-dataset-btn:hover, .add-dataset-btn:focus {
      border-color: var(--bi-pending-border); background: var(--bi-grey-light);
      color: var(--bi-green-dark);
    }

    /* Non-floating footer: DTAtools version + author + GitHub link. Sits in the
       normal document flow at the bottom of the page (never fixed/floating). */
    .app-footer {
      margin-top: 32px; padding: 16px 24px; border-top: 1px solid var(--bi-pending-border);
      background: #fff; color: var(--bi-grey); font-size: .8125rem;
      display: flex; align-items: center; justify-content: center; gap: 12px; flex-wrap: wrap;
    }
    .app-footer .foot-name { font-weight: 600; color: var(--bi-green-dark); }
    .app-footer .foot-ver {
      background: var(--bi-green-light); color: var(--bi-green-dark);
      border-radius: 999px; padding: 1px 9px; font-weight: 600;
    }
    /* foot-sep is now an empty, aria-hidden span (app.R) -- a plain CSS rule
       draws the 1px divider instead of a Unicode bullet in the markup. */
    .app-footer .foot-sep { display: inline-block; width: 1px; height: 12px; background: var(--bi-pending-border); opacity: 1; }
    .app-footer a { color: var(--bi-green); text-decoration: none; font-weight: 600; }
    .app-footer a:hover { text-decoration: underline; }

    /* Dataset detail header block: wraps .ds-head with the LEFT RAIL for the
       dataset's own status (app.R sets class = ds-head-block ds-st-<status>
       on this wrapper -- pending relies on the base rule's own neutral
       colour, so there is no separate .ds-st-pending). */
    /* The negative left margin hangs the rail in the card body's own 1rem
       gutter, so the heading stays on the same left edge as everything
       below it instead of being pushed 18px inward by rail + padding. */
    .ds-head-block { border-left: 4px solid var(--bi-pending-border); padding: 2px 0 2px 12px; margin: 4px 0 16px -1rem; }
    .ds-head-block.ds-st-pass { border-left-color: var(--bi-pass); }
    .ds-head-block.ds-st-fail { border-left-color: var(--bi-fail); }
    .ds-head-block.ds-st-nodata { border-left-color: #C77700; }
    /* Dataset detail header: dataset DESCRIPTION as the heading, name smaller. */
    .ds-head { display: flex; align-items: baseline; gap: 12px; flex-wrap: wrap; margin: 0 0 6px; }
    .ds-head .ds-desc { margin: 0; font-weight: 600; color: var(--bi-green-dark); line-height: 1.2; font-size: 1.375rem; }
    .ds-head .ds-name {
      font-size: .82rem; color: var(--bi-grey);
      font-family: var(--bs-font-monospace);
    }
    .ds-actions { display: flex; gap: 8px; flex-wrap: wrap; align-items: center; margin-bottom: 14px; }

    /* Dataset Edit menu: one entry point for the three specification editors.
       The rows are icon + title + one line of explanation, so the menu says
       what each editor changes instead of relying on a tooltip the three
       separate buttons used to carry. */
    /* No pseudo-element rules here. test-run-dta-app.R scans every app file
       for namespaced package calls to catch undeclared dependencies, and a CSS
       double-colon pseudo-element reads to that scanner as a package named
       after the selector it follows. */
    .ds-edit-toggle { font-weight: 500; }
    .ds-edit-menu {
      min-width: 19rem;
      padding: 6px;
      margin-top: 6px;
      border: 1px solid var(--bi-pending-border);
      border-radius: .75rem;
      box-shadow: 0 12px 28px rgba(26, 43, 42, .16);
    }
    .ds-edit-menu .dropdown-header {
      padding: 6px 10px 4px;
      font-size: .75rem; font-weight: 600; letter-spacing: 0;
      text-transform: none; color: var(--bi-grey);
    }
    /* white-space:normal so the description wraps instead of stretching the
       menu; Bootstrap sets nowrap on .dropdown-item. */
    .ds-edit-item {
      display: flex; align-items: flex-start; gap: 10px;
      padding: 9px 10px; border-radius: 9px;
      white-space: normal; text-decoration: none;
    }
    .ds-edit-item:hover, .ds-edit-item:focus, .ds-edit-item:active {
      background: var(--bi-green-light);
    }
    .ds-edit-item:focus-visible { outline: 2px solid var(--bi-accent); outline-offset: -2px; }
    .ds-edit-icon {
      flex: 0 0 auto;
      display: inline-flex; align-items: center; justify-content: center;
      width: 30px; height: 30px; border-radius: 8px;
      background: var(--bi-grey-light); font-size: 15px; line-height: 1;
    }
    .ds-edit-item:hover .ds-edit-icon, .ds-edit-item:focus .ds-edit-icon { background: #fff; }
    .ds-edit-text { display: flex; flex-direction: column; min-width: 0; }
    .ds-edit-title { font-weight: 600; color: var(--bi-ink); line-height: 1.25; }
    .ds-edit-item:hover .ds-edit-title, .ds-edit-item:focus .ds-edit-title {
      color: var(--bi-green-dark);
    }
    .ds-edit-desc { font-size: .78rem; color: var(--bi-grey); line-height: 1.3; }
    /* Remove dataset: the one destructive row below the divider, colored
       like the app's other danger affordances (btn-outline-danger,
       .file-remove) so it reads as delete rather than a fifth editor, even
       before the divider above it registers. Same specificity as the
       .ds-edit-item rules above (one class + :hover), so source order alone
       -- these coming after -- is what makes the danger tint win. */
    .ds-edit-item-danger .ds-edit-title { color: var(--bi-fail); }
    .ds-edit-item-danger:hover,
    .ds-edit-item-danger:focus,
    .ds-edit-item-danger:active {
      background: var(--bi-fail-bg);
    }
    .ds-edit-item-danger:hover .ds-edit-title,
    .ds-edit-item-danger:focus .ds-edit-title {
      color: var(--bi-fail);
    }

    /* Modals, menus, notifications share one set of surfaces: rounded
       corners, a soft shadow, and the same heading/body/footer rhythm. */
    .modal-content { border: 0; border-radius: .875rem; box-shadow: 0 24px 60px rgba(26,43,42,.22); }
    .modal-header { padding: 18px 24px 12px; border-bottom: 1px solid var(--bi-pending-border); }
    .modal-title { font-weight: 600; font-size: 1.125rem; color: var(--bi-green-dark); }
    .modal-body { padding: 18px 24px; }
    .modal-body h3, .modal-body h4, .modal-body h5 { font-size: 1rem; font-weight: 600; color: var(--bi-ink); margin: 14px 0 8px; }
    .modal-footer { padding: 12px 24px 18px; border-top: 1px solid var(--bi-pending-border); }
    #shiny-notification-panel .shiny-notification {
      background: #fff; color: var(--bi-ink); border: 1px solid var(--bi-pending-border);
      border-left: 4px solid var(--bi-green); border-radius: .625rem;
      box-shadow: 0 12px 28px rgba(26,43,42,.16);
    }
    #shiny-notification-panel .shiny-notification-error { border-left-color: var(--bi-fail); }
    #shiny-notification-panel .shiny-notification-warning { border-left-color: #C77700; }

    /* Wider inspect modal + a body that wraps/scrolls instead of overflowing. */
    .modal-xl { max-width: 92vw; }
    .dta-inspect-wrap { overflow-x: auto; max-height: 68vh; }
    .dta-inspect-wrap table { width: 100%; }
    .dta-inspect-wrap td, .dta-inspect-wrap th {
      white-space: normal; word-break: break-word; vertical-align: top;
    }

    /* Validation messages: table fills the card width; only the Message column
       wraps (others stay on one line) so the layout is dynamic but full-width. */
    .msgs-table { width: 100%; }
    .msgs-table .dataTables_wrapper { width: 100%; }
    .msgs-table table.dataTable { width: 100% !important; }
    .msgs-table table.dataTable td.dt-nowrap { white-space: nowrap; }
    .msgs-table table.dataTable td.msg-cell { white-space: normal; min-width: 320px; }

    /* Compact edit / delete buttons inside the column & rule editor tables. */
    .dta-row-btn { padding: 1px 7px; margin: 0 2px; line-height: 1.3; }
    .dta-name-chip {
      display: inline-block; padding: 2px 10px; border-radius: 999px;
      font-family: var(--bs-font-monospace);
      font-size: .78rem; background: var(--bi-grey-light);
      color: var(--bi-green-dark); border: 1px solid var(--bi-pending-border);
    }
    .cond-hint { font-size: .78rem; color: var(--bi-grey); margin: 2px 0 8px; }

    /* Editable YAML via the Ace editor: rounded dark frame to match the app,
       user-resizable (resize: vertical -- height only, the wrapper does not
       widen). height (not just min-height) is what makes this a definite
       size for .ace_editor's height:100% below to resolve against; dragging
       the corner handle sets an inline height on THIS element that then
       naturally overrides it, same as it would for a plain <textarea>.
       overflow:auto (rather than the old overflow:hidden, which also
       satisfies CSS's rule that resize needs non-visible overflow, but
       silently clips instead) is the fallback for the instant between a
       drag and yaml_ace_resize_js's ResizeObserver callback catching up. */
    .yaml-ace-wrap {
      border: 1px solid var(--bi-pending-border); border-radius: .625rem; overflow: auto;
      height: 70vh; min-height: 30vh; resize: vertical;
    }
    /* shinyAce::aceEditor(height = ...) sets that value as a fixed INLINE
       style on this exact element (shinyAce's own R source builds
       pre(..., style = paste0('height: ', height))), which a plain
       stylesheet rule can never beat -- only !important does. Without this,
       the editor would stay pinned at its initial height forever while the
       wrapper around it grew or shrank: the editor not tracking the drag,
       leaving a blank gap at the bottom, is the whole bug this rule exists
       to close. See yaml_ace_resize_js (app.R) for the other half: Ace's own
       internal layout, cached at init and untouched by a CSS size change
       alone. */
    .yaml-ace-wrap .ace_editor { height: 100% !important; font-size: 13px; }

    /* Column / rule spec editors (inside the Edit modals). */
    .spec-toolbar { display: flex; gap: 8px; align-items: center; margin-bottom: 10px; flex-wrap: wrap; }
    .spec-hint { font-size: .82rem; color: var(--bi-grey); }
    .spec-form { border: 1px solid var(--bi-pending-border); border-radius: .5rem; padding: 12px; background: #fff; margin-top: 10px; }
    .spec-form .form-group { margin-bottom: 8px; }
    .cond-builder { border: 1px solid var(--bi-pending-border); border-radius: .5rem; padding: 10px; margin-bottom: 10px; background: var(--bi-grey-light); }
    .cond-builder .cond-title { font-weight: 600; font-size: .78rem; text-transform: none; letter-spacing: 0; color: var(--bi-grey); margin-bottom: 6px; }
    .cond-row { display: flex; gap: 8px; align-items: flex-end; margin-bottom: 4px; }
    .cond-row .form-group { margin-bottom: 0; flex: 1 1 auto; }

    /* Metadata: section titles + clickable (editable) contact rows. */
    .md-section-title { font-size: 1.0625rem; font-weight: 600; color: var(--bi-green-dark); margin: 4px 0 10px; }
    .contact-item { cursor: pointer; }
    .contact-item:hover { background: var(--bi-green-light); }
    .contact-item .contact-edit-ic { color: var(--bi-grey); font-size: .78rem; margin-left: 8px; white-space: nowrap; }
    /* Section dividers on the Metadata tab (Details / Sender / Receiver /
       Version history): more air than a default Bootstrap <hr>, so each
       section reads as its own block. */
    .bslib-sidebar-layout .tab-content hr { margin: 20px 0; border-top: 1px solid var(--bi-pending-border); opacity: 1; }

    /* meta_field_text(): the read-only counterpart of a textInput() /
       textAreaInput(), shown on the Metadata tab while edit mode is off.
       Sized to Bootstrap's own .form-label / .form-control rhythm (.25rem
       label margin, 1rem/1.5 value text with .25rem vertical padding
       standing in for the control's border) so the tab occupies the same
       vertical space either way and does not visibly jump when the switch
       flips the fields between plain text and inputs. white-space:pre-wrap
       on the value because one of the fields it replaces is a
       textAreaInput(), whose value can contain newlines that a plain
       (nowrap) block would otherwise collapse. */
    .md-ro-field { margin-bottom: .875rem; }
    .md-ro-label {
      margin-bottom: .25rem; font-size: .8125rem; font-weight: 500; color: var(--bi-grey);
    }
    .md-ro-value {
      font-size: 1rem; line-height: 1.5; color: var(--bi-ink);
      padding: .25rem 0; white-space: pre-wrap; word-break: break-word;
    }

    /* contact_detail_block(): the read-only counterpart of one contact row,
       shown inside a .list-group-item (app.R's render_contacts()) while edit
       mode is off. Same label/value colour split as .md-ro-label/.md-ro-value
       above, just laid out as a compact label-then-value line per field
       rather than the Metadata tab's stacked label-above-value -- several of
       these can sit inside one list row, where the full form-field rhythm
       would be too tall. */
    .contact-detail-head { font-weight: 600; color: var(--bi-ink); margin-bottom: 4px; }
    .contact-detail-field {
      display: flex; gap: 6px; font-size: .86rem; color: var(--bi-ink);
      margin-bottom: 2px;
    }
    .contact-detail-label { color: var(--bi-grey); flex: 0 0 auto; min-width: 96px; }
    .contact-detail-value { word-break: break-word; }
    /* render_contacts() (app.R) lays contacts out in a Bootstrap .list-group;
       tint its default border to match the rest of the sheet. */
    .list-group-item { border-color: var(--bi-pending-border); }
    /* The signature/reviewer flags: same pill treatment as .ws-pill (sidebar
       header) and .foot-ver (footer), so a fact about this contact reads
       consistently with the rest of the app rather than inventing a new look. */
    .contact-detail-flags { margin-top: 6px; display: flex; gap: 6px; flex-wrap: wrap; }
    .contact-detail-flag {
      font-size: .72rem; font-weight: 600; color: var(--bi-green-dark);
      background: var(--bi-green-light); border-radius: 999px; padding: 1px 9px;
    }

    /* Example-file picker: the drop zone grows to at most 560px and the
       'or Load an example file' group sits directly to its right. The
       button is a plain outline: only the drop target itself is dashed now,
       so the two no longer compete for the eye. flex-end lines the group up
       with the bottom of the dashed zone; the padding lifts it back to the
       zone's vertical centre (the zone is about 24px taller than the button). */
    .slot-example { display: flex; gap: 14px; align-items: flex-end; flex-wrap: wrap; }
    .slot-example .dropzone { flex: 1 1 320px; max-width: 560px; }
    .slot-example-or { display: flex; align-items: center; gap: 10px; padding-bottom: 12px; }
    .slot-example-or > span { font-size: .8125rem; color: var(--bi-grey); }
    .slot-example-btn {
      white-space: nowrap;
      border: 1px solid rgba(0,98,91,.7) !important;
      background: #fff !important; color: var(--bi-green-dark) !important;
      border-radius: .5rem; padding: .45rem 1rem; font-weight: 500;
    }
    .slot-example-btn:hover {
      background: var(--bi-green-light) !important;
      border-color: var(--bi-green) !important;
    }

    /* Floating, foldable validation-messages dock pinned to the viewport bottom.
       Collapsed shows only the bar; expanded reveals the messages table. */
    .msgs-dock {
      position: fixed; left: 0; right: 0; bottom: 0; z-index: 1030;
      background: #fff; border-top: 3px solid var(--bi-green);
      box-shadow: 0 -8px 24px rgba(26,43,42,.10);
      display: flex; flex-direction: column; max-height: 62vh;
    }
    .msgs-dock-bar {
      display: flex; align-items: center; gap: 12px; cursor: pointer;
      padding: 8px 20px; background: #fff;
      border-bottom: 1px solid transparent; user-select: none;
    }
    /* The bar only draws its divider once the body below it is actually
       showing -- collapsed, there is nothing to divide it from. */
    .msgs-dock:not(.collapsed) .msgs-dock-bar { border-bottom-color: var(--bi-pending-border); }
    .msgs-dock-title { font-weight: 600; color: var(--bi-green-dark); white-space: nowrap; font-size: .9375rem; }
    .msgs-dock-count {
      background: var(--bi-green-dark); color: #fff; border-radius: 999px;
      padding: 1px 9px; font-size: .74rem; font-weight: 600;
    }
    .msgs-dock-count.zero { background: var(--bi-grey-light); color: var(--bi-ink); border: 1px solid var(--bi-pending-border); }
    .msgs-dock-ds {
      font-family: var(--bs-font-monospace);
      font-size: .78rem; color: var(--bi-green-dark);
      background: var(--bi-grey-light); border: 1px solid var(--bi-pending-border);
      border-radius: 999px; padding: 1px 9px;
    }
    .msgs-dock-actions { margin-left: auto; display: flex; align-items: center; gap: 10px; }
    .msgs-dock-dl { display: flex; gap: 6px; }
    .msgs-dock-chevron { color: var(--bi-green-dark); font-size: .8rem; transition: transform .18s ease; }
    .msgs-dock-body { overflow: auto; padding: 12px 20px 16px; }
    .msgs-dock.collapsed .msgs-dock-body { display: none; }
    .msgs-dock.collapsed .msgs-dock-chevron { transform: rotate(180deg); }
    .msgs-table table.dataTable thead th {
      background: var(--bi-grey-light); color: var(--bi-green-dark); font-weight: 600;
      border-bottom: 1px solid var(--bi-pending-border);
    }
    .msgs-table table.dataTable tbody tr:hover { background: var(--bi-green-light); cursor: pointer; }
    /* Keep the static footer / page content clear of the collapsed dock bar. */
    body { padding-bottom: 56px; }
    /* On a narrow screen the four download buttons no longer fit beside the
       title, count and dataset chip: let the bar wrap onto a second row
       (actions right-aligned under the title) and give the page the extra
       room at the bottom that the taller bar now covers. */
    @media (max-width: 900px) {
      .msgs-dock-bar { flex-wrap: wrap; row-gap: 6px; }
      .msgs-dock-actions { margin-left: 0; width: 100%; justify-content: flex-end; flex-wrap: wrap; }
      body { padding-bottom: 104px; }
    }
    /* Narrow screens: the three tabs fit one row at about 260px, so keep
       them there rather than wrapping the active underline onto a second
       row; the landing alternatives stack full-width. */
    @media (max-width: 600px) {
      .card-header .card-header-tabs { flex-wrap: nowrap; }
      .card-header .card-header-tabs .nav-link { padding: 10px 10px; font-size: .875rem; white-space: nowrap; }
      .landing-alt { flex-direction: column; align-items: stretch; }
      .landing-alt-actions { flex-direction: column; }
      .landing-alt-actions .btn { width: 100%; }
    }
    /* bslib ships its sidebar collapse toggle with border: none -- a bare
       12px chevron on the page ground. Below 768px it is the only way back
       to the sidebar (and to Check all datasets), so it gets an edge. */
    .bslib-sidebar-layout .collapse-toggle {
      border: 1px solid var(--bi-pending-border); background: #fff; color: var(--bi-green-dark);
    }

    /* Rule editor: the type is locked (read-only) when editing an existing
       rule -- it is only chosen when the rule is first created. */
    .rule-type-fixed {
      display: block; padding: 6px 10px; border: 1px solid var(--bi-pending-border);
      border-radius: .5rem; background: var(--bi-grey-light); color: var(--bi-grey);
      font-weight: 500;
    }

    /* Inspect popup: a summary card that names the failing rule/constraint,
       plus a highlighted 'should be' (green) vs 'actual' (red) comparison. */
    .inspect-modal-body { max-height: 72vh; overflow-y: auto; }
    .inspect-summary {
      border: 1px solid var(--bi-pending-border); border-radius: 8px;
      padding: 12px 14px; margin-bottom: 12px; background: var(--bi-grey-light);
    }
    .inspect-summary-head { margin-bottom: 6px; }
    .inspect-badge {
      display: inline-block; padding: 2px 10px; border-radius: 999px;
      font-size: .72rem; font-weight: 600; color: #fff;
    }
    .inspect-badge.rule { background: var(--bi-fail); }
    .inspect-badge.columnspec { background: #b8860b; }
    /* Third validation axis (a value unrepresentable in its declared type).
       Its own hue so it is never mistaken for a rule or schema failure. */
    .inspect-badge.import { background: #1f6feb; }
    .inspect-msg { font-size: 1.02rem; font-weight: 600; margin: 6px 0; }
    .inspect-desc-main { display: flex; gap: 8px; align-items: center; flex-wrap: wrap; }
    .inspect-desc-type {
      font-size: .72rem; background: var(--bi-grey-light); color: var(--bi-grey);
      border-radius: 999px; padding: 1px 8px;
    }
    .inspect-desc-detail {
      font-family: var(--bs-font-monospace);
      font-size: .82rem; color: #333; margin-top: 4px; word-break: break-word;
    }
    .inspect-desc-note { font-size: .78rem; color: var(--bi-grey); margin-top: 3px; }
    .inspect-cmp { display: flex; gap: 12px; flex-wrap: wrap; margin-bottom: 12px; }
    .inspect-box { flex: 1 1 240px; border-radius: 8px; padding: 10px 12px; border: 1px solid; }
    .inspect-expected { background: var(--bi-pass-bg); border-color: var(--bi-pass-border); }
    .inspect-actual { background: var(--bi-fail-bg); border-color: var(--bi-fail); }
    .inspect-box-title { font-weight: 600; margin-bottom: 6px; font-size: .86rem; }
    .inspect-expected .inspect-box-title { color: var(--bi-green-dark); }
    .inspect-actual .inspect-box-title { color: var(--bi-fail); }
    .inspect-should { font-weight: 600; color: var(--bi-green-dark); word-break: break-word; }
    .inspect-actual-val { font-weight: 600; font-size: 1.04rem; color: var(--bi-fail); word-break: break-word; }
    .inspect-actual-loc { font-size: .78rem; color: var(--bi-grey); margin-top: 3px; }
    .inspect-none { color: var(--bi-grey); }
    .inspect-hl-table { width: 100%; border-collapse: collapse; font-size: .82rem; }
    .inspect-hl-table th, .inspect-hl-table td { border: 1px solid var(--bi-pending-border); padding: 3px 7px; text-align: left; }
    .inspect-hl-table th { background: #fff; color: var(--bi-grey); font-weight: 600; }
    .inspect-hl-table td.inspect-hl-val { background: #fff; font-weight: 600; color: var(--bi-fail); }
    .inspect-hl-table td.inspect-hl-row { background: #fff; color: var(--bi-grey); white-space: nowrap; }
    .inspect-details { margin-top: 4px; }
    .inspect-details > summary { cursor: pointer; color: var(--bi-grey); font-size: .82rem; margin-bottom: 6px; }

    /* Busy buttons (see click_guard_script(), ui_components.R).
       Two classes, because the swallowing of repeat clicks starts on the
       first click but the LOOK is delayed ~120ms: .dta-busy is the functional
       state, .dta-busy-shown is the visible one. A button whose work finishes
       inside that window is released without ever having flashed a spinner.

       Deliberately NOT `pointer-events: none`, which is the obvious way to
       write this and is wrong: it takes the button out of hit-testing
       altogether, so the second click of a double-click does not land on the
       button and get cancelled -- it lands on whatever is BEHIND the button,
       which in a toolbar can be another control. Measured in the browser: the
       repeat click arrived retargeted rather than swallowed. Cancelling the
       event on the button itself, in the capture-phase listener, is what
       keeps it from reaching anything at all. */
    .dta-busy { cursor: progress; }
    .dta-busy-shown {
      position: relative;
      /* Hides the label AND the icon (font icons draw with color), leaving
         the button's own width and background untouched so nothing reflows
         under the pointer. The spinner below needs its own colour as a
         result: currentColor is transparent here. */
      color: transparent !important;
    }
    .dta-busy-shown { --dta-busy-ink: var(--bi-ink); }
    .dta-busy-shown.btn-primary, .dta-busy-shown.btn-secondary,
    .dta-busy-shown.btn-success, .dta-busy-shown.btn-danger,
    .dta-busy-shown.btn-warning, .dta-busy-shown.btn-info { --dta-busy-ink: #fff; }
    .dta-busy-shown::after {
      content: ''; position: absolute; top: 50%; left: 50%;
      width: .9em; height: .9em; margin: -.45em 0 0 -.45em;
      border: 2px solid var(--dta-busy-ink);
      border-right-color: transparent;
      border-radius: 50%;
      animation: dta-busy-spin .6s linear infinite;
    }
    @keyframes dta-busy-spin { to { transform: rotate(360deg); } }
    /* A spinner is the only feedback these buttons give, so it stays visible
       when motion is reduced -- it just stops turning. */
    @media (prefers-reduced-motion: reduce) {
      .dta-busy-shown::after { animation: none; opacity: .55; }
    }
    "))
}

# Status chip HTML ---------------------------------------------------------
status_chip <- function(status) {
  status <- match.arg(status, c("pass", "fail", "pending", "nodata"))
  label <- switch(status,
    pass = "Passed",
    fail = "Failed",
    pending = "Not validated",
    nodata = "No data"
  )
  cls <- paste0("status-chip status-", status)
  shiny::span(
    class = cls,
    shiny::span(class = "status-dot"),
    label
  )
}

# Accessible icon + label for a slot's state (not color-only)
slot_state_label <- function(state, detail = NULL) {
  icon <- switch(state,
    ok = "\u2714", # check mark
    warn = "\u26A0", # warning
    empty = "\u2014" # em dash
  )
  cls <- switch(state,
    ok = "slot-ok",
    warn = "slot-warn",
    empty = "slot-meta"
  )
  shiny::span(class = cls, paste0(icon, " ", detail %||% ""))
}

# Lightweight, dependency-free YAML syntax highlighter -> HTML. HTML-escapes
# first, then wraps tokens (keys, strings, numbers, booleans, comments, list
# dashes) in <span class="yml-*"> for the .yaml-view theme. Purely presentational;
# the DTA object -- not this HTML -- remains the source of truth.
yaml_highlight_html <- function(text) {
  esc <- function(s) {
    s <- gsub("&", "&amp;", s, fixed = TRUE)
    s <- gsub("<", "&lt;", s, fixed = TRUE)
    gsub(">", "&gt;", s, fixed = TRUE)
  }
  hl_value <- function(v) {
    lead <- sub("^([[:space:]]*).*$", "\\1", v)
    tv <- substring(v, nchar(lead) + 1L)
    if (!nzchar(tv)) {
      return(v)
    }
    cls <- if (grepl('^".*"$', tv) || grepl("^'.*'$", tv)) {
      "yml-str"
    } else if (grepl("^(true|false|yes|no|on|off|null|~)$", tv, ignore.case = TRUE)) {
      "yml-bool"
    } else if (grepl("^-?[0-9]+(\\.[0-9]+)?$", tv)) {
      "yml-num"
    } else {
      "yml-str"
    }
    paste0(lead, sprintf('<span class="%s">%s</span>', cls, tv))
  }
  lines <- strsplit(text %||% "", "\n", fixed = TRUE)[[1]]
  if (length(lines) == 0) {
    return("")
  }
  out <- vapply(lines, function(line) {
    if (!nzchar(line)) {
      return("")
    }
    e <- esc(line)
    # Whole-line comment.
    if (grepl("^[[:space:]]*#", e)) {
      return(sprintf('<span class="yml-comment">%s</span>', e))
    }
    # Split a trailing inline comment (needs whitespace before '#').
    comment <- ""
    m <- regexpr("[[:space:]]+#.*$", e)
    if (m > 0) {
      comment <- sprintf('<span class="yml-comment">%s</span>', substring(e, m))
      e <- substring(e, 1L, m - 1L)
    }
    # Peel leading indent (+ optional list dashes) so keys stay clean.
    pre <- ""
    dm <- regexpr("^[[:space:]]*(- )*", e)
    if (dm > 0 && attr(dm, "match.length") > 0) {
      lead <- regmatches(e, dm)
      pre <- gsub("- ", '<span class="yml-dash">- </span>', lead, fixed = TRUE)
      e <- substring(e, attr(dm, "match.length") + 1L)
    }
    # key: value
    km <- regexpr("^([^:]+):([[:space:]]|$)", e)
    if (km > 0) {
      key <- sub("^([^:]+):.*$", "\\1", e)
      after <- substring(e, nchar(key) + 2L)
      body <- sprintf(
        '<span class="yml-key">%s</span><span class="yml-punct">:</span>%s',
        key, hl_value(after)
      )
      return(paste0(pre, body, comment))
    }
    paste0(pre, hl_value(e), comment)
  }, character(1), USE.NAMES = FALSE)
  paste(out, collapse = "\n")
}
