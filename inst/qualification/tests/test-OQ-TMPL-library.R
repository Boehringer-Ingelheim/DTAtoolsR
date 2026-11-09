# The template library as the app reads it: Word templates found in
# export-templates/, and the layout problems the template index reports.
# REQ-TMPL-041, REQ-TMPL-043 and REQ-TMPL-044.
#
# These cases reach the app's own functions through the package's engine
# loader (the same technique OQ-TMPL-067 uses), so they are white-box. Every
# expected value is written from the requirement text. The Word files are
# generated: .gitignore keeps real .docx files out of the repository, so every
# library below is built in qa_tempdir(), and a .docx written by tmpl_library()
# carries a {PLACEHOLDER} marker because the discovery skips one that has none.

tmpl_fixture <- function(...) file.path(qa_suite_root(), "fx", "t", ...)

# A Word file with the given paragraphs; its default is a fill-in template.
tmpl_docx <- function(path, lines = c("Agreement {TITLE}", "{VERSION}")) {
  doc <- officer::read_docx()
  for (ln in lines) doc <- officer::body_add_par(doc, ln)
  print(doc, target = path)
  invisible(path)
}

# A library root in a fresh temporary directory with these files (file names
# relative to the root; a directory part is created). A .docx is a fill-in
# template; any other file holds one line of text.
tmpl_library <- function(files, env = parent.frame()) {
  root <- qa_tempdir(env)
  for (f in files) {
    dir.create(dirname(file.path(root, f)), recursive = TRUE, showWarnings = FALSE)
    if (grepl("[.]docx$", f)) tmpl_docx(file.path(root, f)) else writeLines("placeholder", file.path(root, f))
  }
  root
}

tmpl_norm <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)

# ---- REQ-TMPL-041: Word templates found by convention ------------------------

test_that("OQ-TMPL-094 | every .docx directly in export-templates/ is offered, labelled by its file name, in file-name order, and clutter is not | REQ-TMPL-041 | tags: white-box", {
  engine <- getFromNamespace(".dta_template_engine", "DTAtools")()
  root <- tmpl_library(file.path("export-templates", c(
    "b_report.docx", "a_report.docx", "Study Template v1.2.docx",
    ".hidden.docx", "~$a_report.docx", "notes.txt", "README.md", "old.doc"
  )))
  dir.create(file.path(root, "export-templates", "sub"))
  writeLines("placeholder", file.path(root, "export-templates", "sub", "inner.docx"))

  found <- engine$dta_app_config_discover_word_templates(root)
  qa_step(
    "labels are the file names without the extension, in byte order of the file names (capitals sort first)",
    c("Study Template v1.2", "a_report", "b_report"), found$label
  )
  qa_step(
    "each entry's path is the file itself",
    tmpl_norm(file.path(root, "export-templates", c("Study Template v1.2.docx", "a_report.docx", "b_report.docx"))),
    found$path
  )
  qa_step(
    "the dotfile, the lock file, the text, README and .doc files and the file in a sub-folder are not offered",
    character(0),
    intersect(basename(found$path), c(".hidden.docx", "~$a_report.docx", "notes.txt", "README.md", "old.doc", "inner.docx"))
  )
  qa_step("a root without an export-templates/ folder offers nothing", 0L, nrow(engine$dta_app_config_discover_word_templates(tmpl_library("templates/x.txt"))))

  # A Word file with nothing to fill, and one that is not a Word file at all.
  mixed <- tmpl_library(file.path("export-templates", "fill_in.docx"))
  tmpl_docx(file.path(mixed, "export-templates", "styles_only.docx"), "Prose without any marker.")
  writeLines("not a zip archive", file.path(mixed, "export-templates", "corrupt.docx"))
  warned <- character(0)
  found <- withCallingHandlers(
    engine$dta_app_config_discover_word_templates(mixed),
    warning = function(w) {
      warned <<- c(warned, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  qa_step("only the file with a placeholder marker is offered", "fill_in", found$label)
  squashed <- gsub("\\s+", "", warned)
  qa_step("each skipped file raises one warning", 2L, length(warned))
  qa_check(
    "and the warnings name the file without markers and the unreadable one",
    any(grepl("styles_only.docx", squashed, fixed = TRUE)) && any(grepl("corrupt.docx", squashed, fixed = TRUE))
  )
})

test_that("OQ-TMPL-095 | when two roots provide a file of the same name only the earlier root's is offered, and a warning names both | REQ-TMPL-041 | tags: white-box", {
  engine <- getFromNamespace(".dta_template_engine", "DTAtools")()
  first <- tmpl_library(file.path("export-templates", c("shared.docx", "only_first.docx")))
  second <- tmpl_library(file.path("export-templates", c("shared.docx", "only_second.docx")))

  warned <- character(0)
  found <- withCallingHandlers(
    engine$dta_app_config_discover_word_templates(c(first, second)),
    warning = function(w) {
      warned <<- c(warned, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  qa_step(
    "the roots' files follow their precedence order, and the second root's shared.docx is left out",
    c("only_first", "shared", "only_second"), found$label
  )
  qa_step(
    "the offered shared.docx is the first root's",
    tmpl_norm(file.path(first, "export-templates", "shared.docx")),
    found$path[found$label == "shared"]
  )
  qa_step("exactly one warning is raised", 1L, length(warned))
  squashed <- gsub("\\s+", "", warned)
  qa_check(
    "and it names the file of each root",
    grepl(basename(first), squashed, fixed = TRUE) && grepl(basename(second), squashed, fixed = TRUE)
  )

  reversed <- suppressWarnings(engine$dta_app_config_discover_word_templates(c(second, first)))
  qa_step(
    "reversing the roots reverses which one wins",
    tmpl_norm(file.path(second, "export-templates", "shared.docx")),
    reversed$path[reversed$label == "shared"]
  )
})

test_that("OQ-TMPL-096 | a library with no dta-app.yaml still offers its Word templates, and configured entries keep their label and order, first | REQ-TMPL-041 | tags: white-box", {
  engine <- getFromNamespace(".dta_template_engine", "DTAtools")()
  withr::defer(engine$dta_app_config_invalidate())
  config_for <- function(root) {
    withr::local_envvar(c(
      DTATOOLS_TEMPLATE_SOURCES = paste0("qualification=dir:", root),
      DTATOOLS_TEMPLATE_INCLUDE_BUILTIN = "false"
    ))
    engine$dta_app_config(refresh = TRUE)$exports$word_templates
  }

  bare <- tmpl_library(file.path("export-templates", c("x.docx", "y.docx")))
  qa_check("the library has no dta-app.yaml", !file.exists(file.path(bare, "dta-app.yaml")))
  qa_step("without a configuration the files are offered by their stems", c("x", "y"), config_for(bare)$label)

  configured <- tmpl_library(file.path("export-templates", c("a.docx", "b.docx", "c.docx")))
  # b.docx is listed explicitly, so it is offered although it holds no placeholder marker.
  tmpl_docx(file.path(configured, "export-templates", "b.docx"), "Prose without any marker.")
  writeLines(c(
    "kind: dta_app_config",
    "exports:",
    "  word_templates:",
    "    - file: export-templates/b.docx",
    "      label: Custom B"
  ), file.path(configured, "dta-app.yaml"))
  offered <- config_for(configured)
  qa_step(
    "the configured entry comes first under its own label although it has no placeholder marker, the others follow, and b.docx is not offered twice",
    c("Custom B", "a", "c"), offered$label
  )
  qa_step(
    "each entry's path is the file itself",
    tmpl_norm(file.path(configured, "export-templates", c("b.docx", "a.docx", "c.docx"))),
    offered$path
  )
})

# ---- REQ-TMPL-044: containment ----------------------------------------------

test_that("OQ-TMPL-097 | a .docx that is a symbolic link out of the library is not offered, a regular one beside it is | REQ-TMPL-044 | tags: white-box", {
  engine <- getFromNamespace(".dta_template_engine", "DTAtools")()
  root <- tmpl_library(file.path("export-templates", "regular.docx"))
  outside_dir <- tmpl_library("outside.docx")
  outside <- file.path(outside_dir, "outside.docx")
  link <- file.path(root, "export-templates", "link.docx")
  linked <- tryCatch(suppressWarnings(file.symlink(outside, link)), error = function(e) FALSE)
  if (!isTRUE(linked) || !file.exists(link)) {
    testthat::skip("symbolic links cannot be created by this account on this platform")
  }

  found <- engine$dta_app_config_discover_word_templates(root)
  qa_step("only the regular file is offered", "regular", found$label)
})

# ---- REQ-TMPL-043: layout problems found when the index is built ------------

# The index of a library root, with the warning conditions raised on the way.
tmpl_index_of <- function(root, engine) {
  withr::local_envvar(c(
    DTATOOLS_TEMPLATE_SOURCES = paste0("qualification=dir:", root),
    DTATOOLS_TEMPLATE_INCLUDE_BUILTIN = "false"
  ))
  warned <- character(0)
  idx <- withCallingHandlers(
    engine$build_template_index(refresh = TRUE),
    warning = function(w) {
      warned <<- c(warned, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(
    index = idx, notes = attr(idx, "warnings"), layout = attr(idx, "layout_warnings"),
    warned = warned
  )
}

test_that("OQ-TMPL-098 | files in the wrong folder are not indexed and are reported once by name, and a kind folder given as a source is reported once | REQ-TMPL-043 | tags: white-box", {
  engine <- getFromNamespace(".dta_template_engine", "DTAtools")()

  # Fixture: templates/ok.dta-template.yaml (in its folder), a party profile in
  # templates/ and a vocabulary directly in the root.
  root <- tmpl_fixture("c", "template_misplaced")
  built <- tmpl_index_of(root, engine)
  qa_step("only the file in its own folder is indexed", "misplaced_ok", built$index$id)
  qa_step("one note per misplaced file", 2L, length(built$notes))
  qa_check(
    "each note names its file",
    any(grepl("stray.dta-party.yaml", built$notes, fixed = TRUE)) &&
      any(grepl("stray_root.dta-vocabulary.yaml", built$notes, fixed = TRUE))
  )
  qa_check(
    "and the folder the file belongs in",
    any(grepl("parties/", built$notes, fixed = TRUE)) && any(grepl("vocabulary/", built$notes, fixed = TRUE))
  )
  qa_step(
    "each note names the source and the path relative to the source root, with the kind in plain words",
    c(
      "qualification: stray_root.dta-vocabulary.yaml - a vocabulary belongs in vocabulary/",
      "qualification: templates/stray.dta-party.yaml - a party profile belongs in parties/"
    ),
    sort(sub(" [(]library layout.*$", "", built$notes))
  )
  qa_check(
    "and shows neither the absolute path of the library nor an internal kind id",
    !any(grepl(root, built$notes, fixed = TRUE)) && !any(grepl("dta_party_profile|dta_vocabulary", built$notes))
  )
  qa_step("the same notes are kept apart as the layout warnings", sort(built$notes), sort(built$layout))
  qa_step("all layout problems of the build are announced by one warning", 1L, length(built$warned))
  squashed <- gsub("\\s+", "", built$warned)
  qa_check(
    "that warning carries both files",
    grepl("stray.dta-party.yaml", squashed, fixed = TRUE) && grepl("stray_root.dta-vocabulary.yaml", squashed, fixed = TRUE)
  )

  # A clean library raises nothing.
  clean <- tmpl_index_of(tmpl_fixture("lay", "folder"), engine)
  qa_step("a library in the documented layout raises no warning", 0L, length(clean$warned))
  qa_step("and has no note", 0L, length(clean$notes))
  qa_step(
    "and all four of its files are indexed",
    c("layout_dataset", "layout_example", "layout_party", "layout_terms"),
    sort(clean$index$id)
  )

  # The source is pointed one level too deep, at templates/ itself.
  deep <- tmpl_index_of(file.path(tmpl_fixture("c", "library_subfolder_given"), "templates"), engine)
  qa_step("a kind folder given as the source is reported once, not once per file", 1L, length(deep$notes))
  qa_check(
    "as a library sub-folder whose parent should be used",
    grepl("library sub-folder", deep$notes, fixed = TRUE) && grepl("parent directory", deep$notes, fixed = TRUE)
  )
  qa_check(
    "with the advice to reorganise a flat library, which names no absolute path",
    grepl("flat library", deep$notes, fixed = TRUE) && !grepl(root, deep$notes, fixed = TRUE) &&
      startsWith(deep$notes, "qualification: templates/ looks like")
  )
  qa_step("and nothing is indexed from it", 0L, nrow(deep$index))
})
