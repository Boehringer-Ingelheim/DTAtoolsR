# Tests for the library app configuration (`dta-app.yaml`): the parser, its
# trust-boundary validation, discovery across library roots, and the cached
# accessor. The parser returns problems rather than throwing, so most tests
# assert on the problem keys and on the value that was KEPT.

local_clean_app_config_env <- function(..., .local_envir = parent.frame()) {
  withr::local_envvar(
    c(
      DTATOOLS_TEMPLATE_SOURCES = NA,
      DTATOOLS_TEMPLATE_INCLUDE_BUILTIN = NA,
      DTATOOLS_TEMPLATE_REFRESH_SECONDS = NA,
      DTATOOLS_TEMPLATE_CACHE_DIR = NA,
      ...
    ),
    .local_envir = .local_envir
  )
  withr::local_options(list(DTAtools.template_dir = NULL), .local_envir = .local_envir)
  invalidate <- app_fn("dta_app_config_invalidate")
  invalidate()
  withr::defer(invalidate(), envir = .local_envir)
}

# Write `lines` as dta-app.yaml in a fresh temp dir; returns the file path.
write_app_config <- function(lines, dir = withr::local_tempdir(.local_envir = parent.frame())) {
  path <- file.path(dir, "dta-app.yaml")
  writeLines(lines, path)
  path
}

parse_cfg <- function(lines, dir = withr::local_tempdir(.local_envir = parent.frame())) {
  app_fn("parse_dta_app_config")(write_app_config(lines, dir))
}

problem_keys <- function(res, severity = NULL) {
  p <- res$problems
  if (!is.null(severity)) p <- p[p$severity == severity, ]
  p$key
}

# 1x1 PNG
png_bytes <- as.raw(c(
  0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a, 0x00, 0x00, 0x00, 0x0d,
  0x49, 0x48, 0x44, 0x52, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01
))

test_that("defaults reproduce today's literals, with an empty organisation", {
  d <- app_fn("dta_app_config_defaults")()

  expect_true(is.na(d$source))
  expect_identical(d$organisation$name, "")
  expect_identical(d$organisation$unit, "")
  expect_identical(d$branding$app_title, "DTAtools")
  expect_identical(d$branding$window_title, "DTAtools")
  expect_identical(d$branding$footer_label, "")
  expect_identical(d$branding$logo, "dtatools_logo_small.png")
  expect_identical(d$branding$logo_alt, "DTAtools logo")
  expect_match(d$branding$app_subtitle, "^Validation and authoring of Data Transfer Agreements")
  expect_identical(
    d$branding$landing_headline,
    "Check a data transfer against its specification."
  )
  expect_match(d$branding$landing_lede, "^Open a DTA or DTS YAML file")
  expect_identical(d$colors[["primary"]], "#00625B")
  expect_identical(d$colors[["page_background"]], "#F4F7F6")
  expect_identical(d$colors[["warning"]], "#C77700")
  expect_identical(d$colors[["surface"]], "#FFFFFF")
  expect_length(d$colors, 21)
  expect_identical(
    d$links$report_issue,
    "https://github.com/Boehringer-Ingelheim/DTAtoolsR/issues"
  )
  expect_null(d$links$documentation)
  expect_true("documentation" %in% names(d$links))
  expect_true(all(unlist(d$features)))
  expect_identical(d$exports$default_document, "word")
  expect_identical(d$exports$validation_messages, c("csv", "tsv", "xlsx", "html"))
  expect_identical(nrow(d$exports$word_templates), 0L)
  expect_identical(names(d$exports$word_templates), c("label", "path"))
})

test_that("an empty or comment-only file means defaults with no problems", {
  for (lines in list(character(0), c("# nothing here", ""), "kind: dta_app_config")) {
    res <- parse_cfg(lines)
    expect_identical(nrow(res$problems), 0L)
    expect_identical(res$config, app_fn("dta_app_config_defaults")())
  }
})

test_that("a full valid config round-trips into the normalised shape", {
  dir <- withr::local_tempdir()
  writeBin(png_bytes, file.path(dir, "logo.png"))
  writeBin(as.raw(1:5), file.path(dir, "our_dta.docx"))
  res <- parse_cfg(c(
    "kind: dta_app_config",
    "organisation: {name: Example Institute, unit: Data Management}",
    "branding:",
    "  app_title: MyApp",
    "  app_subtitle: Sub",
    "  window_title: Tab",
    "  logo: logo.png",
    "  logo_alt: My logo",
    "  landing_headline: Hello",
    "  landing_lede: Welcome",
    "colors:",
    "  primary: '#112233'",
    "  accent: '#abc'",
    "links:",
    "  report_issue: https://example.org/issues",
    "  about: false",
    "  documentation: mailto:help@example.org",
    "features:",
    "  start: {load_example: false, create_from_template: true, create_blank: false}",
    "  raw_yaml_tab: false",
    "  dta_yaml_download: false",
    "exports:",
    "  document: [markdown, word_template]",
    "  default_document: word_template",
    "  word_templates:",
    "    - file: our_dta.docx",
    "      label: Our DTA",
    "  bundled_word_templates: false",
    "  validation_messages: [csv]",
    "  validation_summary: false"
  ), dir)

  expect_identical(nrow(res$problems), 0L)
  cfg <- res$config
  expect_identical(cfg$organisation, list(name = "Example Institute", unit = "Data Management"))
  expect_identical(cfg$branding$app_title, "MyApp")
  expect_identical(cfg$branding$window_title, "Tab")
  expect_identical(cfg$branding$logo_alt, "My logo")
  expect_identical(cfg$branding$landing_headline, "Hello")
  expect_identical(cfg$branding$footer_label, "Example Institute \u00b7 Data Management")
  expect_match(cfg$branding$logo, "^data:image/png;base64,")
  expect_identical(cfg$colors[["primary"]], "#112233")
  expect_identical(cfg$colors[["accent"]], "#abc")
  expect_identical(cfg$colors[["fail"]], "#C0392B")
  expect_identical(cfg$links$report_issue, "https://example.org/issues")
  expect_null(cfg$links$about)
  expect_identical(cfg$links$documentation, "mailto:help@example.org")
  expect_identical(cfg$links$repository, "https://github.com/Boehringer-Ingelheim/DTAtoolsR")
  expect_false(cfg$features$load_example)
  expect_true(cfg$features$create_from_template)
  expect_false(cfg$features$create_blank)
  expect_false(cfg$features$raw_yaml_tab)
  expect_false(cfg$features$dta_yaml_download)
  expect_true(cfg$features$dataset_yaml_download)
  expect_identical(cfg$exports$document, c("markdown", "word_template"))
  expect_identical(cfg$exports$default_document, "word_template")
  expect_identical(cfg$exports$word_templates$label, "Our DTA")
  expect_false(cfg$exports$bundled_word_templates)
  expect_identical(cfg$exports$validation_messages, "csv")
  expect_false(cfg$exports$validation_summary)
})

test_that("unknown keys are warnings with dotted keys; known values still apply", {
  res <- parse_cfg(c(
    "bogus: 1",
    "branding: {colour: red, app_title: Kept}",
    "features: {start: {nope: true}}",
    "exports: {word_templates: []}"
  ))
  expect_setequal(
    problem_keys(res, "warning"),
    c("bogus", "branding.colour", "features.start.nope")
  )
  expect_length(problem_keys(res, "error"), 0)
  expect_identical(res$config$branding$app_title, "Kept")
})

test_that("unreadable YAML, wrong kind and non-mappings give one error and defaults", {
  defaults <- app_fn("dta_app_config_defaults")()

  bad_yaml <- parse_cfg(c("organisation: [unclosed", "  : : :"))
  expect_identical(nrow(bad_yaml$problems), 1L)
  expect_identical(bad_yaml$problems$severity, "error")
  expect_identical(bad_yaml$config, defaults)

  wrong_kind <- parse_cfg(c("kind: something_else", "branding: {app_title: X}"))
  expect_identical(problem_keys(wrong_kind, "error"), "kind")
  expect_identical(wrong_kind$config, defaults)

  scalar <- parse_cfg("just a string")
  expect_identical(scalar$problems$severity, "error")
  expect_identical(scalar$config, defaults)

  missing <- app_fn("parse_dta_app_config")(file.path(withr::local_tempdir(), "nope.yaml"))
  expect_identical(missing$problems$severity, "error")
  expect_identical(missing$config, defaults)
})

test_that("wrong types are errors and the key keeps its default", {
  res <- parse_cfg(c(
    "branding: {app_title: [a, b], landing_lede: 5}",
    "features: {editing: 'maybe'}",
    "organisation: {name: {x: 1}}",
    "colors: not-a-mapping"
  ))
  expect_setequal(
    problem_keys(res, "error"),
    c("branding.app_title", "branding.landing_lede", "features.editing", "organisation.name", "colors")
  )
  d <- app_fn("dta_app_config_defaults")()
  expect_identical(res$config$branding$app_title, d$branding$app_title)
  expect_identical(res$config$branding$landing_lede, d$branding$landing_lede)
  expect_true(res$config$features$editing)
  expect_identical(res$config$organisation$name, "")
})

test_that("CSS-injection and non-hex colours are rejected, default kept", {
  res <- parse_cfg(c(
    "colors:",
    "  primary: 'red;}body{'",
    "  accent: red",
    "  text: '#12'",
    "  muted: '#12345G'",
    "  pass: '#FFF'"
  ))
  expect_setequal(
    problem_keys(res, "error"),
    c("colors.primary", "colors.accent", "colors.text", "colors.muted")
  )
  d <- app_fn("dta_app_config_defaults")()$colors
  expect_identical(res$config$colors[["primary"]], d[["primary"]])
  expect_identical(res$config$colors[["text"]], d[["text"]])
  expect_identical(res$config$colors[["pass"]], "#FFF")
  # an unquoted hex is a YAML comment -> null -> reported, not silently dropped
  unq <- parse_cfg(c("colors:", "  primary: #00625B"))
  expect_identical(problem_keys(unq, "error"), "colors.primary")
})

test_that("unsafe links are rejected; false and null hide a link", {
  res <- parse_cfg(c(
    "links:",
    "  report_issue: 'javascript:alert(1)'",
    "  about: 'https://example.org/a b'",
    "  repository: 'https://example.org/\"onmouseover=x'",
    "  documentation: 'HTTPS://Example.org/docs'"
  ))
  expect_setequal(
    problem_keys(res, "error"),
    c("links.report_issue", "links.about", "links.repository")
  )
  d <- app_fn("dta_app_config_defaults")()$links
  expect_identical(res$config$links$report_issue, d$report_issue)
  expect_identical(res$config$links$about, d$about)
  expect_identical(res$config$links$documentation, "HTTPS://Example.org/docs")

  hidden <- parse_cfg(c("links:", "  repository: false", "  about: ~"))
  expect_length(problem_keys(hidden), 0)
  expect_null(hidden$config$links$repository)
  expect_null(hidden$config$links$about)
})

test_that("logo paths that escape the library folder are rejected", {
  dir <- withr::local_tempdir()
  writeBin(png_bytes, file.path(dir, "ok.png"))
  outside <- withr::local_tempfile(fileext = ".png")
  writeBin(png_bytes, outside)
  default_logo <- app_fn("dta_app_config_defaults")()$branding$logo

  for (bad in c(
    "../x.png", "sub/../../x.png", "..\\\\x.png", "/etc/x.png", "\\\\\\\\host\\\\share\\\\x.png",
    "C:/x.png", "~/x.png", gsub("\\\\", "/", outside)
  )) {
    res <- parse_cfg(paste0("branding: {logo: \"", bad, "\"}"), dir)
    expect_identical(problem_keys(res, "error"), "branding.logo", info = bad)
    expect_identical(res$config$branding$logo, default_logo, info = bad)
  }
})

test_that("a logo becomes a data URI that decodes back to the file bytes", {
  dir <- withr::local_tempdir()
  writeBin(png_bytes, file.path(dir, "logo.png"))
  res <- parse_cfg("branding: {logo: logo.png}", dir)

  expect_identical(nrow(res$problems), 0L)
  uri <- res$config$branding$logo
  expect_true(startsWith(uri, "data:image/png;base64,"))
  expect_identical(
    jsonlite::base64_dec(sub("^data:image/png;base64,", "", uri)),
    png_bytes
  )
})

test_that("missing, mistyped and oversized logos are error problems", {
  dir <- withr::local_tempdir()
  writeLines("x", file.path(dir, "logo.txt"))
  writeBin(raw(1024^2 + 1), file.path(dir, "big.png"))
  default_logo <- app_fn("dta_app_config_defaults")()$branding$logo

  for (f in c("missing.png", "logo.txt", "big.png")) {
    res <- parse_cfg(paste0("branding: {logo: ", f, "}"), dir)
    expect_identical(problem_keys(res, "error"), "branding.logo", info = f)
    expect_identical(res$config$branding$logo, default_logo, info = f)
  }
})

test_that("word templates are listed with absolute paths and default labels", {
  dir <- withr::local_tempdir()
  writeBin(as.raw(1:3), file.path(dir, "our_dta.docx"))
  writeBin(as.raw(1:3), file.path(dir, "other.docx"))
  writeLines("x", file.path(dir, "notes.txt"))
  res <- parse_cfg(c(
    "exports:",
    "  word_templates:",
    "    - file: our_dta.docx",
    "    - {file: other.docx, label: Other, extra: 1}",
    "    - file: notes.txt",
    "    - file: ../escape.docx",
    "    - just-a-string"
  ), dir)

  wt <- res$config$exports$word_templates
  expect_identical(wt$label, c("our_dta", "Other"))
  expect_identical(
    wt$path,
    normalizePath(file.path(dir, c("our_dta.docx", "other.docx")), winslash = "/")
  )
  expect_true(all(file.exists(wt$path)))
  expect_setequal(
    problem_keys(res, "error"),
    c(
      "exports.word_templates[3].file", "exports.word_templates[4].file",
      "exports.word_templates[5]"
    )
  )
  expect_identical(problem_keys(res, "warning"), "exports.word_templates[2].extra")
})

test_that("export lists accept only known formats and default_document must be offered", {
  res <- parse_cfg(c(
    "exports:",
    "  document: [word, pdf]",
    "  validation_messages: [csv, ods]"
  ))
  expect_setequal(
    problem_keys(res, "error"),
    c("exports.document", "exports.validation_messages")
  )
  d <- app_fn("dta_app_config_defaults")()
  expect_identical(res$config$exports$document, d$exports$document)
  expect_identical(res$config$exports$validation_messages, d$exports$validation_messages)

  explicit <- parse_cfg(c(
    "exports: {document: [markdown], default_document: word}"
  ))
  expect_identical(problem_keys(explicit, "warning"), "exports.default_document")
  expect_identical(explicit$config$exports$default_document, "markdown")

  # narrowing `document` moves the implicit default without complaint
  implicit <- parse_cfg("exports: {document: [markdown]}")
  expect_identical(nrow(implicit$problems), 0L)
  expect_identical(implicit$config$exports$default_document, "markdown")

  none <- parse_cfg("exports: {document: [], validation_messages: []}")
  expect_identical(nrow(none$problems), 0L)
  expect_identical(none$config$exports$document, character(0))
  expect_null(none$config$exports$default_document)
  expect_identical(none$config$exports$validation_messages, character(0))
})

test_that("editing false forces create_blank false, warning only if it was set true", {
  quiet <- parse_cfg("features: {editing: false}")
  expect_identical(nrow(quiet$problems), 0L)
  expect_false(quiet$config$features$editing)
  expect_false(quiet$config$features$create_blank)
  expect_true(quiet$config$features$load_example)

  explicit <- parse_cfg(c("features:", "  editing: false", "  start: {create_blank: true}"))
  expect_identical(problem_keys(explicit, "warning"), "features.start.create_blank")
  expect_false(explicit$config$features$create_blank)

  off <- parse_cfg(c("features:", "  editing: false", "  start: {create_blank: false}"))
  expect_identical(nrow(off$problems), 0L)
})

test_that("footer_label defaults from the organisation; an explicit value wins", {
  expect_identical(parse_cfg("organisation: {name: Acme}")$config$branding$footer_label, "Acme")
  expect_identical(
    parse_cfg("organisation: {name: Acme, unit: Stats}")$config$branding$footer_label,
    "Acme \u00b7 Stats"
  )
  expect_identical(parse_cfg("organisation: {unit: Stats}")$config$branding$footer_label, "")
  expect_identical(
    parse_cfg(c("organisation: {name: Acme}", "branding: {footer_label: Custom}"))$config$branding$footer_label,
    "Custom"
  )
  expect_identical(
    parse_cfg(c("organisation: {name: Acme}", "branding: {footer_label: ''}"))$config$branding$footer_label,
    ""
  )
})

test_that("window title and logo alt follow app_title unless set", {
  b <- parse_cfg("branding: {app_title: MyApp}")$config$branding
  expect_identical(b$window_title, "MyApp")
  expect_identical(b$logo_alt, "MyApp logo")
})

test_that("dta_app_config_find: first root wins, extras are reported as ignored", {
  find <- app_fn("dta_app_config_find")
  a <- withr::local_tempdir()
  b <- withr::local_tempdir()
  c0 <- withr::local_tempdir()
  writeLines("kind: dta_app_config", file.path(a, "dta-app.yml"))
  writeLines("kind: dta_app_config", file.path(b, "dta-app.yaml"))
  writeLines("kind: dta_app_config", file.path(b, "dta-app.yml"))

  res <- find(c(c0, a, b))
  expect_identical(res$path, normalizePath(file.path(a, "dta-app.yml"), winslash = "/"))
  expect_identical(
    res$ignored,
    normalizePath(file.path(b, c("dta-app.yaml", "dta-app.yml")), winslash = "/")
  )

  none <- find(c(c0, ""))
  expect_true(is.na(none$path))
  expect_length(none$ignored, 0)
  expect_true(is.na(find(character(0))$path))
})

test_that("dta_app_config() reads the configured library, warns once, and can be invalidated", {
  local_clean_app_config_env()
  lib <- withr::local_tempdir()
  lib <- normalizePath(lib, winslash = "/")
  writeLines(c("branding: {app_title: First, colour: red}"), file.path(lib, "dta-app.yaml"))
  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", lib))

  fn <- app_fn("dta_app_config")
  warned <- character(0)
  collect <- function(expr) {
    withCallingHandlers(expr, warning = function(w) {
      warned <<- c(warned, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  }

  cfg <- collect(fn())
  expect_identical(cfg$branding$app_title, "First")
  expect_identical(cfg$source, normalizePath(file.path(lib, "dta-app.yaml"), winslash = "/"))
  expect_true(any(grepl("branding.colour", warned, fixed = TRUE)))

  # cached: no new warnings, and a changed file is not picked up
  n <- length(warned)
  writeLines("branding: {app_title: Second}", file.path(lib, "dta-app.yaml"))
  expect_identical(collect(fn())$branding$app_title, "First")
  expect_length(warned, n)

  app_fn("dta_app_config_invalidate")()
  expect_identical(collect(fn())$branding$app_title, "Second")
  expect_identical(fn(refresh = TRUE)$branding$app_title, "Second")
})

test_that("dta_app_config() warns about extra config files and falls back to defaults without any", {
  local_clean_app_config_env()
  one <- normalizePath(withr::local_tempdir(), winslash = "/")
  two <- normalizePath(withr::local_tempdir(), winslash = "/")
  writeLines("branding: {app_title: One}", file.path(one, "dta-app.yaml"))
  writeLines("branding: {app_title: Two}", file.path(two, "dta-app.yaml"))
  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", one, ";dir:", two))

  fn <- app_fn("dta_app_config")
  warned <- character(0)
  cfg <- withCallingHandlers(fn(), warning = function(w) {
    warned <<- c(warned, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  expect_identical(cfg$branding$app_title, "One")
  expect_true(any(grepl("Ignoring", warned, fixed = TRUE)))

  app_fn("dta_app_config_invalidate")()
  empty <- normalizePath(withr::local_tempdir(), winslash = "/")
  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", empty))
  expect_no_warning(plain <- fn())
  expect_identical(plain, app_fn("dta_app_config_defaults")())
})

# ---- document: what the library calls its documents ---------------------------

test_that("document defaults keep every label as it is today", {
  d <- app_fn("dta_app_config_defaults")()$document
  expect_identical(d$names, "Data Transfer Agreement")
  expect_identical(d$abbreviations, "DTA")
  expect_identical(d$label, "Data Transfer Agreement")
  expect_identical(d$abbr, "DTA")
  expect_false(d$custom)
})

test_that("document accepts a string, and joins a list with ' / ' and '/'", {
  one <- parse_cfg(c("document:", "  name: Data Transmission Specification", "  abbreviation: DTS"))
  expect_equal(nrow(one$problems), 0)
  expect_identical(one$config$document$names, "Data Transmission Specification")
  expect_identical(one$config$document$abbr, "DTS")
  expect_true(one$config$document$custom)

  both <- parse_cfg(c(
    "document:",
    "  name: [Data Transfer Agreement, Data Transmission Specification]",
    "  abbreviation: [DTA, DTS]"
  ))
  expect_equal(nrow(both$problems), 0)
  expect_identical(
    both$config$document$label,
    "Data Transfer Agreement / Data Transmission Specification"
  )
  expect_identical(both$config$document$abbr, "DTA/DTS")
  expect_identical(both$config$document$abbreviations, c("DTA", "DTS"))
})

test_that("a bad document value is reported by key and the default is kept", {
  bad <- function(lines) parse_cfg(lines)

  empty <- bad(c("document:", "  name: \"\""))
  expect_identical(problem_keys(empty, "error"), "document.name")
  expect_identical(empty$config$document$label, "Data Transfer Agreement")

  num <- bad(c("document:", "  abbreviation: 42"))
  expect_identical(problem_keys(num, "error"), "document.abbreviation")
  expect_identical(num$config$document$abbr, "DTA")

  too_many <- bad(c("document:", "  name: [a, b, c, d]"))
  expect_identical(problem_keys(too_many, "error"), "document.name")

  long <- bad(c("document:", "  abbreviation: ABCDEFGHIJKLM"))
  expect_identical(problem_keys(long, "error"), "document.abbreviation")
  ok12 <- bad(c("document:", "  abbreviation: ABCDEFGHIJKL"))
  expect_equal(nrow(ok12$problems), 0)

  blank_entry <- bad(c("document:", "  name: [A, \"\"]"))
  expect_identical(problem_keys(blank_entry, "error"), "document.name")
})

test_that("two document lists of different length are rejected on the abbreviation", {
  res <- parse_cfg(c(
    "document:",
    "  name: [Data Transfer Agreement, Data Transmission Specification]",
    "  abbreviation: [DTA, DTS, X]"
  ))
  expect_identical(problem_keys(res, "error"), "document.abbreviation")
  # The name was fine and is kept; the abbreviation falls back.
  expect_identical(res$config$document$abbr, "DTA")
  expect_identical(res$config$document$names, c("Data Transfer Agreement", "Data Transmission Specification"))

  # A single abbreviation beside a list of names is not a length clash.
  one <- parse_cfg(c("document:", "  name: [A, B]", "  abbreviation: AB"))
  expect_equal(nrow(one$problems), 0)
  expect_identical(one$config$document$abbr, "AB")
})

test_that("an unknown key under document is a warning and document itself is a known section", {
  res <- parse_cfg(c("document:", "  nme: Oops"))
  expect_identical(problem_keys(res, "warning"), "document.nme")
  expect_false(res$config$document$custom)
})

test_that("dta_app_export_args hands the writers the label, abbreviation and a non-empty organisation", {
  cfg <- app_fn("dta_app_config_defaults")()
  args <- app_fn("dta_app_export_args")(cfg)
  expect_identical(args$document_name, "Data Transfer Agreement")
  expect_identical(args$document_abbreviation, "DTA")
  expect_null(args$organisation)
  expect_null(args$organisation_unit)

  cfg$organisation <- list(name = "Acme", unit = "Biometrics")
  cfg$document$label <- "A / B"
  args <- app_fn("dta_app_export_args")(cfg)
  expect_identical(args$organisation, "Acme")
  expect_identical(args$organisation_unit, "Biometrics")
  expect_identical(args$document_name, "A / B")
})

test_that("editing: false forces raw_yaml_editable off, and the parser validates the flag", {
  forced <- parse_cfg(c("features:", "  editing: false", "  raw_yaml_editable: true"))
  expect_false(forced$config$features$raw_yaml_editable)

  off <- parse_cfg("features: {raw_yaml_editable: false}")
  expect_false(off$config$features$raw_yaml_editable)
  expect_true(off$config$features$editing)
  expect_equal(nrow(off$problems), 0)

  bad <- parse_cfg("features: {raw_yaml_editable: maybe}")
  expect_identical(problem_keys(bad, "error"), "features.raw_yaml_editable")
  expect_true(bad$config$features$raw_yaml_editable)

  expect_true(app_fn("dta_app_config_defaults")()$features$raw_yaml_editable)
})

test_that("document and organisation text is collapsed to one line", {
  res <- parse_cfg(c(
    "kind: dta_app_config",
    "organisation:",
    "  name: \"Acme\n## Injected\"",
    "  unit: \"Unit\r\n  Two\"",
    "document:",
    "  name: \"Doc\n# Evil\"",
    "  abbreviation: \"A\nB\""
  ))
  expect_identical(nrow(res$problems), 0L)
  expect_identical(res$config$organisation, list(name = "Acme ## Injected", unit = "Unit Two"))
  expect_identical(res$config$document$names, "Doc # Evil")
  expect_identical(res$config$document$abbreviations, "A B")
})
