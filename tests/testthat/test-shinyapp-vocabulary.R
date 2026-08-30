# Coverage for inst/shiny/dta_app/R/vocabulary.R, reached via the
# app_env()/app_fn() harness in helper-shinyapp.R (see that file for why: the
# app's helper files are auto-sourced by Shiny at launch and are not part of
# the package namespace).

Sys.setenv(NOT_CRAN = "true")

# cli wraps a long condition message at the console width, which breaks a naive
# substring match the moment the wrap lands inside the text being asserted on.
# Same helper, same rationale, as test-shinyapp-template-parties.R and
# test-shinyapp-template-inherit.R; duplicated rather than shared for the same
# reason those two duplicate it.
expect_vocab_error_contains <- function(expr, fixed_text) {
  err <- tryCatch(
    {
      expr
      NULL
    },
    error = function(e) e
  )
  testthat::expect_false(is.null(err), info = "expected an error, got none")
  flat <- gsub("\\s+", " ", conditionMessage(err))
  testthat::expect_match(flat, fixed_text, fixed = TRUE)
}

# A minimal but complete `dta_vocabulary` YAML, matching the shape documented
# at the top of vocabulary.R. Every part a test might want to vary is a
# parameter, so a test exercising one bad field does not restate the rest.
write_vocabulary_fixture <- function(
  dir,
  id = "visit",
  version_line = 'version: "1.0"',
  kind_line = "kind: dta_vocabulary",
  type_line = "type: text",
  term_lines = c(
    "terms:",
    "  - code: SCR",
    "    label: Screening",
    "  - code: C1D1",
    "    label: Cycle 1 Day 1",
    "  - code: EOT",
    "    label: End of treatment"
  ),
  extra_lines = character(0),
  filename = NULL
) {
  filename <- filename %||% paste0(id, ".dta-vocabulary.yaml")
  path <- file.path(dir, filename)
  writeLines(
    c(
      kind_line,
      paste0("id: ", id),
      version_line,
      "label: Visit identifiers",
      "description: Standard visit codes",
      type_line,
      term_lines,
      extra_lines
    ),
    path
  )
  path
}

# `%||%` is the app's own; the test file needs it before app_env() is touched.
`%||%` <- function(x, y) if (is.null(x)) y else x


# ---- Reading ---------------------------------------------------------------

test_that("read_vocabulary() returns normalised terms in authored order", {
  read_vocabulary <- app_fn("read_vocabulary")
  dir <- withr::local_tempdir()
  path <- write_vocabulary_fixture(dir)

  res <- read_vocabulary(path)
  expect_true(isTRUE(res$ok))

  vocab <- res$value
  expect_identical(vocab$id, "visit")
  expect_identical(vocab$version, "1.0")
  expect_identical(vocab$type, "text")
  expect_identical(
    vapply(vocab$terms, function(t) t$code, character(1)),
    c("SCR", "C1D1", "EOT")
  )
  expect_identical(vocab$terms[[1]]$label, "Screening")
})

test_that("a bare string in terms: is shorthand for a code-only term", {
  read_vocabulary <- app_fn("read_vocabulary")
  dir <- withr::local_tempdir()
  path <- write_vocabulary_fixture(
    dir,
    term_lines = c("terms:", "  - SCR", "  - C1D1")
  )

  vocab <- read_vocabulary(path)$value
  expect_identical(
    vapply(vocab$terms, function(t) t$code, character(1)),
    c("SCR", "C1D1")
  )
  # No label authored, so the term carries none -- vocabulary_values(field =
  # "label") falls back to the code rather than yielding "".
  expect_null(vocab$terms[[1]]$label)
})

test_that("read_vocabulary() rejects a wrong kind, a missing id, and a missing version", {
  read_vocabulary <- app_fn("read_vocabulary")
  dir <- withr::local_tempdir()

  bad_kind <- read_vocabulary(
    write_vocabulary_fixture(dir, kind_line = "kind: dta_party_profile", filename = "k.dta-vocabulary.yaml")
  )
  expect_false(isTRUE(bad_kind$ok))
  expect_match(bad_kind$error, "must be 'dta_vocabulary'", fixed = TRUE)

  no_version <- read_vocabulary(
    write_vocabulary_fixture(dir, version_line = "label_only: x", filename = "v.dta-vocabulary.yaml")
  )
  expect_false(isTRUE(no_version$ok))
  expect_match(no_version$error, "non-empty 'version'", fixed = TRUE)
})

test_that("read_vocabulary() rejects an unknown type and a vocabulary with nothing in it", {
  read_vocabulary <- app_fn("read_vocabulary")
  dir <- withr::local_tempdir()

  bad_type <- read_vocabulary(
    write_vocabulary_fixture(dir, type_line = "type: categorical", filename = "t.dta-vocabulary.yaml")
  )
  expect_false(isTRUE(bad_type$ok))
  expect_match(bad_type$error, "invalid type 'categorical'", fixed = TRUE)

  empty <- read_vocabulary(
    write_vocabulary_fixture(dir, term_lines = character(0), filename = "e.dta-vocabulary.yaml")
  )
  expect_false(isTRUE(empty$ok))
  expect_match(empty$error, "defines no 'terms'", fixed = TRUE)
})


# ---- The YAML scalar traps -------------------------------------------------

# This is the reason dta_vocabulary_yaml_handlers() is WIDER than the shared
# dta_template_yaml_handlers(). Every value below is silently destroyed by a
# plain parse (and all but one of them by the shared handler set too):
#
#   01   -> 1      leading zero read as octal
#   007  -> 7      same
#   0x1F -> 31     read as hex
#   Y    -> TRUE   YAML 1.1 boolean
#   N    -> FALSE  YAML 1.1 boolean
#   NO   -> FALSE  YAML 1.1 boolean
#
# Zero-padded visit numbers and Y/N flags are two of the most common
# controlled vocabularies in this domain, so this is the single highest-value
# assertion in the file.
test_that("zero-padded, hex-looking and Y/N codes survive reading verbatim", {
  read_vocabulary <- app_fn("read_vocabulary")
  dir <- withr::local_tempdir()
  path <- write_vocabulary_fixture(
    dir,
    id = "codes",
    term_lines = c(
      "terms:",
      "  - code: 01",
      "  - code: 007",
      "  - code: 08",
      "  - code: 0x1F",
      "  - code: 1.10",
      "  - code: Y",
      "  - code: N",
      "  - code: NO"
    )
  )

  vocab <- read_vocabulary(path)$value
  expect_identical(
    vapply(vocab$terms, function(t) t$code, character(1)),
    c("01", "007", "08", "0x1F", "1.10", "Y", "N", "NO")
  )
})

test_that("a numeric vocabulary coerces its codes, and a bad one names the offender", {
  read_vocabulary <- app_fn("read_vocabulary")
  vocabulary_values <- app_fn("vocabulary_values")
  dir <- withr::local_tempdir()

  ok <- read_vocabulary(write_vocabulary_fixture(
    dir,
    id = "grade", type_line = "type: integer",
    term_lines = c("terms:", "  - code: 1", "  - code: 2", "  - code: 3")
  ))$value
  expect_identical(vocabulary_values(ok), c(1L, 2L, 3L))

  bad <- read_vocabulary(write_vocabulary_fixture(
    dir,
    id = "mixed", type_line = "type: integer",
    term_lines = c("terms:", "  - code: 1", "  - code: SCR")
  ))$value
  expect_vocab_error_contains(vocabulary_values(bad), "SCR")
})

test_that("malformed and duplicate terms are dropped in one counted warning each", {
  read_vocabulary <- app_fn("read_vocabulary")
  dir <- withr::local_tempdir()

  malformed <- write_vocabulary_fixture(
    dir,
    id = "bad",
    term_lines = c(
      "terms:",
      "  - code: SCR",
      "  - label: no code at all",
      "  - notacode: nope"
    )
  )
  expect_warning(
    vocab <- read_vocabulary(malformed)$value,
    "malformed"
  )
  expect_identical(vapply(vocab$terms, function(t) t$code, character(1)), "SCR")

  dupes <- write_vocabulary_fixture(
    dir,
    id = "dup",
    term_lines = c(
      "terms:",
      "  - code: SCR",
      "    label: first",
      "  - code: SCR",
      "    label: second"
    )
  )
  expect_warning(
    vocab2 <- read_vocabulary(dupes)$value,
    "duplicate"
  )
  expect_length(vocab2$terms, 1L)
  # The FIRST occurrence wins, so the surviving term keeps the first label.
  expect_identical(vocab2$terms[[1]]$label, "first")
})


# ---- Inheritance -----------------------------------------------------------

# Build a resolver over a directory of vocabulary fixtures, mirroring what
# validate_template() and the app both do: look the id up in a real index.
vocab_resolver_for <- function(dir) {
  read_vocabulary <- app_fn("read_vocabulary")
  function(ref) {
    id <- sub("@.*$", "", ref)
    path <- file.path(dir, paste0(id, ".dta-vocabulary.yaml"))
    if (!file.exists(path)) {
      return(NULL)
    }
    res <- read_vocabulary(path)
    if (!isTRUE(res$ok)) NULL else res$value
  }
}

test_that("extends: adds, removes, and replaces terms in place", {
  read_vocabulary <- app_fn("read_vocabulary")
  resolve_vocabulary_inheritance <- app_fn("resolve_vocabulary_inheritance")
  dir <- withr::local_tempdir()

  write_vocabulary_fixture(dir)
  child_path <- write_vocabulary_fixture(
    dir,
    id = "visit_onc",
    term_lines = character(0),
    extra_lines = c(
      "extends: visit@1.0",
      "add_terms:",
      "  - code: C2D1",
      "    label: Cycle 2 Day 1",
      "  - code: SCR",
      "    label: Screening (oncology)",
      "remove_terms: [EOT]"
    )
  )

  child <- read_vocabulary(child_path)$value
  resolved <- resolve_vocabulary_inheritance(child, vocab_resolver_for(dir))

  # EOT removed; C2D1 appended; SCR replaced IN PLACE, so it keeps position 1
  # rather than moving to the end.
  expect_identical(
    vapply(resolved$terms, function(t) t$code, character(1)),
    c("SCR", "C1D1", "C2D1")
  )
  expect_identical(resolved$terms[[1]]$label, "Screening (oncology)")
})

test_that("a child's own terms: replaces the inherited list outright", {
  read_vocabulary <- app_fn("read_vocabulary")
  resolve_vocabulary_inheritance <- app_fn("resolve_vocabulary_inheritance")
  dir <- withr::local_tempdir()

  write_vocabulary_fixture(dir)
  child_path <- write_vocabulary_fixture(
    dir,
    id = "visit_own",
    term_lines = c("terms:", "  - code: ONLY"),
    extra_lines = "extends: visit@1.0"
  )

  resolved <- resolve_vocabulary_inheritance(
    read_vocabulary(child_path)$value, vocab_resolver_for(dir)
  )
  expect_identical(vapply(resolved$terms, function(t) t$code, character(1)), "ONLY")
})

test_that("remove_terms naming an absent code warns but still resolves", {
  read_vocabulary <- app_fn("read_vocabulary")
  resolve_vocabulary_inheritance <- app_fn("resolve_vocabulary_inheritance")
  dir <- withr::local_tempdir()

  write_vocabulary_fixture(dir)
  child_path <- write_vocabulary_fixture(
    dir,
    id = "visit_stale",
    term_lines = character(0),
    extra_lines = c("extends: visit@1.0", "remove_terms: [GONE]")
  )

  child <- read_vocabulary(child_path)$value
  expect_warning(
    resolved <- resolve_vocabulary_inheritance(child, vocab_resolver_for(dir)),
    "remove_terms"
  )
  expect_identical(
    vapply(resolved$terms, function(t) t$code, character(1)),
    c("SCR", "C1D1", "EOT")
  )
})

test_that("an unresolvable parent and a cycle are both errors", {
  read_vocabulary <- app_fn("read_vocabulary")
  resolve_vocabulary_inheritance <- app_fn("resolve_vocabulary_inheritance")
  dir <- withr::local_tempdir()

  orphan_path <- write_vocabulary_fixture(
    dir,
    id = "orphan", term_lines = character(0),
    extra_lines = c("extends: nosuch@1.0", "add_terms: [X]")
  )
  expect_vocab_error_contains(
    resolve_vocabulary_inheritance(read_vocabulary(orphan_path)$value, vocab_resolver_for(dir)),
    "unresolvable reference"
  )

  # Two vocabularies extending each other. The cycle is caught on the id@version
  # key, so it fires however the reference was spelled at each hop.
  write_vocabulary_fixture(
    dir,
    id = "ping", term_lines = character(0),
    extra_lines = c("extends: pong@1.0", "add_terms: [P]")
  )
  pong_path <- write_vocabulary_fixture(
    dir,
    id = "pong", term_lines = character(0),
    extra_lines = c("extends: ping@1.0", "add_terms: [Q]")
  )
  expect_vocab_error_contains(
    resolve_vocabulary_inheritance(read_vocabulary(pong_path)$value, vocab_resolver_for(dir)),
    "cycle detected"
  )
})


# ---- Selecting -------------------------------------------------------------

test_that("include narrows without reordering, and exclude removes", {
  read_vocabulary <- app_fn("read_vocabulary")
  vocabulary_terms <- app_fn("vocabulary_terms")
  dir <- withr::local_tempdir()
  vocab <- read_vocabulary(write_vocabulary_fixture(dir))$value

  # Listed in a different order from the vocabulary's own; the AUTHORED order
  # is what survives, because that is the order the author curated.
  kept <- vocabulary_terms(vocab, include = c("EOT", "SCR"))
  expect_identical(vapply(kept, function(t) t$code, character(1)), c("SCR", "EOT"))

  dropped <- vocabulary_terms(vocab, exclude = "C1D1")
  expect_identical(vapply(dropped, function(t) t$code, character(1)), c("SCR", "EOT"))
})

test_that("include naming an unknown code is an error, not a silent no-op", {
  read_vocabulary <- app_fn("read_vocabulary")
  vocabulary_terms <- app_fn("vocabulary_terms")
  dir <- withr::local_tempdir()
  vocab <- read_vocabulary(write_vocabulary_fixture(dir))$value

  # "SCRN" is the exact shape a typo for "SCR" takes; silently dropping it
  # would leave a column whose permitted values quietly omit a visit.
  expect_vocab_error_contains(
    vocabulary_terms(vocab, include = c("SCR", "SCRN")),
    "SCRN"
  )
  expect_vocab_error_contains(
    vocabulary_terms(vocab, include = "SCR", exclude = "SCR"),
    "leave no terms"
  )
})

test_that("vocabulary_values() yields codes or labels, falling back to the code", {
  read_vocabulary <- app_fn("read_vocabulary")
  vocabulary_values <- app_fn("vocabulary_values")
  dir <- withr::local_tempdir()

  vocab <- read_vocabulary(write_vocabulary_fixture(
    dir,
    id = "pairs",
    term_lines = c(
      "terms:",
      "  - code: TRNSCPTN",
      "    label: Transcription",
      "  - code: BARE"
    )
  ))$value

  expect_identical(vocabulary_values(vocab), c("TRNSCPTN", "BARE"))
  # A term with no label falls back to its code, so a code column and its
  # decode column always have the same arity.
  expect_identical(vocabulary_values(vocab, field = "label"), c("Transcription", "BARE"))
})


# ---- The column binding ----------------------------------------------------

test_that("normalise_values_from() accepts the shorthand and the full mapping", {
  normalise_values_from <- app_fn("normalise_values_from")

  short <- normalise_values_from("visit@1.0", "VISIT")
  expect_identical(short$vocabulary, "visit@1.0")
  expect_identical(short$field, "code")
  expect_identical(short$include, character(0))

  full <- normalise_values_from(
    list(vocabulary = "visit@1.0", field = "label", include = c("SCR", "EOT")),
    "VISIT"
  )
  expect_identical(full$field, "label")
  expect_identical(full$include, c("SCR", "EOT"))

  expect_null(normalise_values_from(NULL, "VISIT"))
})

test_that("normalise_values_from() rejects a missing vocabulary and a bad field", {
  normalise_values_from <- app_fn("normalise_values_from")

  expect_vocab_error_contains(
    normalise_values_from(list(field = "code"), "VISIT"),
    "VISIT"
  )
  expect_vocab_error_contains(
    normalise_values_from(list(vocabulary = "visit@1.0", field = "decode"), "VISIT"),
    "decode"
  )
})


# ---- Expansion --------------------------------------------------------------

# A plain dataset list of the shape build_dataset_from_template() holds
# mid-flight: `columns` is a list of plain column lists, not DTAColumnSpecs.
vocab_dataset_body <- function(columns) {
  list(
    name = "ds1",
    type = "tabular",
    files = list(filename = "ds1.tsv", type = "tsv"),
    columns = columns
  )
}

test_that("expand_column_vocabularies() replaces the binding with a plain values vector", {
  read_vocabulary <- app_fn("read_vocabulary")
  expand <- app_fn("expand_column_vocabularies")
  dir <- withr::local_tempdir()
  write_vocabulary_fixture(dir)
  resolver <- vocab_resolver_for(dir)

  ds <- vocab_dataset_body(list(
    list(id = "VISIT", type = "SAS Char", values_from = "visit@1.0"),
    list(id = "OTHER", type = "SAS Char")
  ))

  out <- expand(ds, resolver)

  expect_identical(out$columns[[1]]$values, c("SCR", "C1D1", "EOT"))
  # `values_from` is authoring syntax and must NOT survive: it would reach
  # do.call(DTAColumnSpec, x) as an unused argument, and would leak into a
  # produced document that has to be readable without the vocabulary library.
  expect_null(out$columns[[1]]$values_from)
  # An unbound column is untouched.
  expect_null(out$columns[[2]]$values)
})

test_that("expansion honours include, exclude and field: label", {
  expand <- app_fn("expand_column_vocabularies")
  dir <- withr::local_tempdir()
  write_vocabulary_fixture(dir)
  resolver <- vocab_resolver_for(dir)

  ds <- vocab_dataset_body(list(
    list(
      id = "VISIT",
      values_from = list(vocabulary = "visit@1.0", include = c("SCR", "C1D1"))
    ),
    list(
      id = "VISITX",
      values_from = list(vocabulary = "visit@1.0", exclude = "EOT")
    ),
    list(
      id = "VISITTXT",
      values_from = list(vocabulary = "visit@1.0", field = "label")
    )
  ))

  out <- expand(ds, resolver)
  expect_identical(out$columns[[1]]$values, c("SCR", "C1D1"))
  expect_identical(out$columns[[2]]$values, c("SCR", "C1D1"))
  expect_identical(
    out$columns[[3]]$values,
    c("Screening", "Cycle 1 Day 1", "End of treatment")
  )
})

test_that("values_from wins over an existing values, and conflicts with pattern", {
  expand <- app_fn("expand_column_vocabularies")
  dir <- withr::local_tempdir()
  write_vocabulary_fixture(dir)
  resolver <- vocab_resolver_for(dir)

  # The legitimate case this silence exists for: a patch re-binding a column
  # the base template had given a hardcoded list.
  won <- expand(vocab_dataset_body(list(
    list(id = "VISIT", values = c("OLD"), values_from = "visit@1.0")
  )), resolver)
  expect_identical(won$columns[[1]]$values, c("SCR", "C1D1", "EOT"))

  # Caught here rather than left to the DTAColumnSpec validator, whose message
  # names neither the vocabulary nor the template.
  expect_vocab_error_contains(
    expand(vocab_dataset_body(list(
      list(id = "VISIT", pattern = "^S", values_from = "visit@1.0")
    )), resolver),
    "VISIT"
  )
})

test_that("an unresolvable vocabulary names the column, not just the reference", {
  expand <- app_fn("expand_column_vocabularies")
  dir <- withr::local_tempdir()
  resolver <- vocab_resolver_for(dir)

  expect_vocab_error_contains(
    expand(vocab_dataset_body(list(
      list(id = "VISIT", values_from = "nosuch@1.0")
    )), resolver),
    "VISIT"
  )
})

test_that("dataset_has_vocabulary_binding() distinguishes bound from unbound bodies", {
  has_binding <- app_fn("dataset_has_vocabulary_binding")

  expect_false(has_binding(vocab_dataset_body(list(list(id = "A")))))
  expect_false(has_binding(list(name = "empty")))
  expect_true(has_binding(vocab_dataset_body(list(
    list(id = "A"), list(id = "B", values_from = "visit@1.0")
  ))))
})

test_that("vocabulary_resolver() resolves through a real index and caches misses", {
  resolver_for_index <- app_fn("vocabulary_resolver")
  build_index <- app_fn("build_template_index")
  dir <- withr::local_tempdir()
  write_vocabulary_fixture(dir)

  withr::local_envvar(c(
    DTATOOLS_TEMPLATE_SOURCES = paste0("t=dir:", dir),
    DTATOOLS_TEMPLATE_INCLUDE_BUILTIN = ""
  ))
  app_fn("dta_template_index_invalidate")()
  idx <- build_index(refresh = TRUE)

  resolve <- resolver_for_index(idx)
  hit <- resolve("visit@1.0")
  expect_identical(vapply(hit$terms, function(t) t$code, character(1)), c("SCR", "C1D1", "EOT"))

  # A miss must stay a miss on the second call. An environment cannot hold
  # NULL -- assigning NULL removes the binding -- so a naive cache would
  # re-resolve every miss forever; the sentinel is what makes this hold.
  expect_null(resolve("nosuch@1.0"))
  expect_null(resolve("nosuch@1.0"))
})

test_that("the real resolver resolves an inherited chain and still catches a cycle", {
  resolver_for_index <- app_fn("vocabulary_resolver")
  build_index <- app_fn("build_template_index")
  dir <- withr::local_tempdir()

  write_vocabulary_fixture(dir)
  write_vocabulary_fixture(
    dir,
    id = "visit_onc", term_lines = character(0),
    extra_lines = c("extends: visit@1.0", "add_terms: [C2D1]", "remove_terms: [EOT]")
  )
  # A mutual extends: pair.
  write_vocabulary_fixture(
    dir,
    id = "ping", term_lines = character(0),
    extra_lines = c("extends: pong@1.0", "add_terms: [P]")
  )
  write_vocabulary_fixture(
    dir,
    id = "pong", term_lines = character(0),
    extra_lines = c("extends: ping@1.0", "add_terms: [Q]")
  )

  withr::local_envvar(c(
    DTATOOLS_TEMPLATE_SOURCES = paste0("t=dir:", dir),
    DTATOOLS_TEMPLATE_INCLUDE_BUILTIN = ""
  ))
  app_fn("dta_template_index_invalidate")()
  resolve <- resolver_for_index(build_index(refresh = TRUE))

  # Inherited terms must be visible THROUGH the real resolver, not only through
  # a hand-rolled lookup in a test.
  expect_identical(
    vapply(resolve("visit_onc@1.0")$terms, function(t) t$code, character(1)),
    c("SCR", "C1D1", "C2D1")
  )

  # REGRESSION GUARD. The resolver must stay a thin wrapper around ONE entry
  # into resolve_vocabulary_inheritance(), handing it a raw, non-recursive
  # lookup. When the resolver resolved inheritance itself, every hop re-entered
  # at .depth = 0 with an empty .seen, the lineage was discarded, and this pair
  # recursed until R's protection stack overflowed instead of aborting.
  expect_vocab_error_contains(resolve("ping@1.0"), "cycle detected")
})


# ---- End to end through build_dataset_from_template ------------------------

test_that("build_dataset_from_template() expands a binding, including one a patch adds", {
  build_ds <- app_fn("build_dataset_from_template")
  dir <- withr::local_tempdir()
  write_vocabulary_fixture(dir)
  resolver <- vocab_resolver_for(dir)

  def <- list(
    id = "tpl", version = "1.0",
    dataset = vocab_dataset_body(list(
      list(id = "VISIT", type = "SAS Char", values_from = "visit@1.0"),
      list(id = "PLAIN", type = "SAS Char")
    ))
  )

  built <- build_ds(def, resolve_vocab = resolver)
  expect_true(isTRUE(built$ok))
  cols <- built$value$dataset$columns
  expect_identical(cols[[1]]$values, c("SCR", "C1D1", "EOT"))
  expect_null(cols[[1]]$values_from)

  # A binding introduced by the PATCH must also be expanded -- which is why
  # expansion runs after apply_dataset_patch(), not before it.
  patched <- build_ds(
    def,
    patch = list(add_columns = list(
      list(id = "VISIT2", type = "SAS Char", values_from = list(
        vocabulary = "visit@1.0", include = "SCR"
      ))
    )),
    resolve_vocab = resolver
  )
  expect_true(isTRUE(patched$ok))
  added <- Filter(function(c) identical(c$id, "VISIT2"), patched$value$dataset$columns)[[1]]
  expect_identical(added$values, "SCR")
  expect_null(added$values_from)
})

test_that("with no resolver supplied, the cached index is used instead", {
  build_ds <- app_fn("build_dataset_from_template")
  dir <- withr::local_tempdir()
  write_vocabulary_fixture(dir)

  withr::local_envvar(c(
    DTATOOLS_TEMPLATE_SOURCES = paste0("t=dir:", dir),
    DTATOOLS_TEMPLATE_INCLUDE_BUILTIN = ""
  ))
  app_fn("dta_template_index_invalidate")()

  def <- list(
    id = "tpl", version = "1.0",
    dataset = vocab_dataset_body(list(
      list(id = "VISIT", type = "SAS Char", values_from = "visit@1.0")
    ))
  )

  # Requiring every existing call site to learn about the resolver just so a
  # template AUTHOR can bind a column would be the wrong coupling -- the same
  # reasoning create_dta_from_template() gives for building an index on demand.
  built <- build_ds(def)
  expect_true(isTRUE(built$ok), info = built$error)
  expect_identical(built$value$dataset$columns[[1]]$values, c("SCR", "C1D1", "EOT"))
})

test_that("a binding that resolves nowhere fails naming the column and the vocabulary", {
  build_ds <- app_fn("build_dataset_from_template")
  dir <- withr::local_tempdir()

  withr::local_envvar(c(
    DTATOOLS_TEMPLATE_SOURCES = paste0("t=dir:", dir),
    DTATOOLS_TEMPLATE_INCLUDE_BUILTIN = ""
  ))
  app_fn("dta_template_index_invalidate")()

  def <- list(
    id = "tpl", version = "1.0",
    dataset = vocab_dataset_body(list(
      list(id = "VISIT", type = "SAS Char", values_from = "nosuch@1.0")
    ))
  )

  # dta_try() turns this into ok = FALSE rather than throwing.
  built <- build_ds(def)
  expect_false(isTRUE(built$ok))
  expect_match(built$error, "VISIT", fixed = TRUE)
  expect_match(built$error, "nosuch@1.0", fixed = TRUE)
})

test_that("a template with no bindings builds fine without a resolver", {
  build_ds <- app_fn("build_dataset_from_template")

  built <- build_ds(list(
    id = "tpl", version = "1.0",
    dataset = vocab_dataset_body(list(list(id = "PLAIN", type = "SAS Char")))
  ))
  expect_true(isTRUE(built$ok))
})


# ---- Vocabulary slots -------------------------------------------------------

vocab_slot_def <- function(...) {
  modifyList(
    list(
      id = "visit_choice",
      label = "Visits",
      target = "datasets.ds1.columns.VISIT.values",
      vocabulary = "visit@1.0"
    ),
    list(...)
  )
}

test_that("vocabulary_slot_target_parts() accepts only the one supported shape", {
  parts_of <- app_fn("vocabulary_slot_target_parts")

  ok <- parts_of("datasets.ds1.columns.VISIT.values")
  expect_identical(ok$dataset, "ds1")
  expect_identical(ok$column, "VISIT")

  expect_null(parts_of("metadata.supplier"))
  expect_null(parts_of("datasets.ds1.columns.VISIT"))
  expect_null(parts_of("datasets.ds1.rules.VISIT.values"))
  expect_null(parts_of("datasets..columns.VISIT.values"))
  # A dataset or column whose name contains a dot is deliberately not
  # addressable -- the alternative is an escaping grammar in a YAML string.
  expect_null(parts_of("datasets.a.b.columns.VISIT.values"))
})

test_that("normalise_vocabulary_slots() fills defaults and rejects malformed slots", {
  normalise <- app_fn("normalise_vocabulary_slots")

  one <- normalise(list(vocab_slot_def()))[[1]]
  expect_identical(one$dataset, "ds1")
  expect_identical(one$column, "VISIT")
  expect_identical(one$mode, "closed")
  expect_identical(one$field, "code")
  expect_identical(one$min, 0L)

  expect_vocab_error_contains(
    normalise(list(vocab_slot_def(target = "metadata.supplier"))),
    "invalid"
  )
  expect_vocab_error_contains(
    normalise(list(vocab_slot_def(vocabulary = NULL))),
    "vocabulary"
  )
  expect_vocab_error_contains(
    normalise(list(vocab_slot_def(mode = "advisory"))),
    "advisory"
  )
  expect_warning(dropped <- normalise(list(list(target = "datasets.a.columns.b.values"))), "missing or empty")
  expect_length(dropped, 0L)
})

test_that("a slot falls back to its default, and enforces min", {
  normalise <- app_fn("normalise_vocabulary_slots")
  slot_values <- app_fn("vocabulary_slot_values")
  dir <- withr::local_tempdir()
  write_vocabulary_fixture(dir)
  resolver <- vocab_resolver_for(dir)

  slot <- normalise(list(vocab_slot_def(default = c("SCR", "C1D1"))))[[1]]
  expect_identical(slot_values(slot, NULL, resolver), c("SCR", "C1D1"))
  expect_identical(slot_values(slot, "EOT", resolver), "EOT")

  # No selection, no default: the column is left exactly as the dataset
  # template left it, rather than silently emptied.
  bare <- normalise(list(vocab_slot_def()))[[1]]
  expect_null(slot_values(bare, NULL, resolver))

  needs_one <- normalise(list(vocab_slot_def(min = 1)))[[1]]
  expect_vocab_error_contains(slot_values(needs_one, NULL, resolver), "at least 1")
})

test_that("closed mode rejects a code it does not offer; open mode appends it", {
  normalise <- app_fn("normalise_vocabulary_slots")
  slot_values <- app_fn("vocabulary_slot_values")
  dir <- withr::local_tempdir()
  write_vocabulary_fixture(dir)
  resolver <- vocab_resolver_for(dir)

  closed <- normalise(list(vocab_slot_def(include = c("SCR", "C1D1"))))[[1]]
  # A stale selection: the slot's allow-list excludes EOT. Silently dropping
  # it would leave the author believing their choice was applied.
  expect_vocab_error_contains(slot_values(closed, c("SCR", "EOT"), resolver), "EOT")

  # "pick from the vocabulary, but use your own if you want to"
  open <- normalise(list(vocab_slot_def(mode = "open")))[[1]]
  expect_identical(slot_values(open, c("SCR", "MYOWN"), resolver), c("SCR", "MYOWN"))
})

test_that("a slot can write decodes instead of codes", {
  normalise <- app_fn("normalise_vocabulary_slots")
  slot_values <- app_fn("vocabulary_slot_values")
  dir <- withr::local_tempdir()
  write_vocabulary_fixture(dir)
  resolver <- vocab_resolver_for(dir)

  slot <- normalise(list(vocab_slot_def(field = "label")))[[1]]
  expect_identical(slot_values(slot, c("SCR", "EOT"), resolver), c("Screening", "End of treatment"))
})

test_that("overrides write into the addressed column and clear its binding", {
  apply_overrides <- app_fn("apply_vocabulary_slot_overrides")

  ds <- vocab_dataset_body(list(
    list(id = "VISIT", values_from = "visit@1.0"),
    list(id = "OTHER", values = "keep me")
  ))
  out <- apply_overrides(ds, list(
    list(dataset = "ds1", column = "VISIT", values = c("SCR"), slot = "s1")
  ))

  expect_identical(out$columns[[1]]$values, "SCR")
  # The slot's choice supersedes the binding, so the binding must not then be
  # expanded over the top of it.
  expect_null(out$columns[[1]]$values_from)
  expect_identical(out$columns[[2]]$values, "keep me")

  # An override for a different dataset is not this dataset's business.
  untouched <- apply_overrides(ds, list(
    list(dataset = "elsewhere", column = "VISIT", values = "X", slot = "s1")
  ))
  expect_identical(untouched$columns[[1]]$values_from, "visit@1.0")

  expect_vocab_error_contains(
    apply_overrides(ds, list(list(dataset = "ds1", column = "NOPE", values = "X", slot = "s1"))),
    "NOPE"
  )
})


# ---- End to end: a slot through create_dta_from_template -------------------

# A temp source directory holding a vocabulary, a dataset template bound to it,
# and a creation template offering a slot over the same column.
vocab_slot_source_dir <- function(slot_lines) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  write_vocabulary_fixture(dir)

  writeLines(
    c(
      "kind: dta_dataset_template",
      "id: bound_ds",
      "version: \"1.0\"",
      "dataset:",
      "  name: ds1",
      "  type: tabular",
      "  files: {filename: x.csv, type: csv}",
      "  columns:",
      "    - id: VISIT",
      "      type: SAS Char",
      "      values_from: visit@1.0"
    ),
    file.path(dir, "bound_ds.dta-dataset-template.yaml")
  )

  writeLines(
    c(
      "kind: dta_creation_template",
      "id: with_slot",
      "version: \"1.0\"",
      "base:",
      "  metadata:",
      "    title: Slotted",
      "datasets:",
      "  - template: bound_ds@1.0",
      slot_lines
    ),
    file.path(dir, "with_slot.dta-template.yaml")
  )

  dir
}

test_that("a vocabulary slot narrows the bound column at creation time", {
  dir <- vocab_slot_source_dir(c(
    "vocabulary_slots:",
    "  - id: visit_choice",
    "    label: Visits",
    "    target: datasets.ds1.columns.VISIT.values",
    "    vocabulary: visit@1.0",
    "    default: [SCR, C1D1]"
  ))

  withr::local_envvar(c(
    DTATOOLS_TEMPLATE_SOURCES = paste0("t=dir:", dir),
    DTATOOLS_TEMPLATE_INCLUDE_BUILTIN = ""
  ))
  app_fn("dta_template_index_invalidate")()
  idx <- app_fn("build_template_index")(refresh = TRUE)

  load_def <- app_fn("load_template_definition")
  loaded <- load_def("with_slot@1.0", index = idx)
  expect_true(isTRUE(loaded$ok))

  create <- app_fn("create_dta_from_template")

  # No selection: the slot's own default applies.
  defaulted <- create(loaded$value$def, loaded$value$path, index = idx)
  expect_true(isTRUE(defaulted$ok), info = defaulted$error)
  col <- defaulted$value@datasets[[1]]@specs@columns[[1]]
  expect_identical(col@values, c("SCR", "C1D1"))

  # An explicit selection overrides the default.
  chosen <- create(
    loaded$value$def, loaded$value$path,
    index = idx, vocab_selections = list(visit_choice = "EOT")
  )
  expect_true(isTRUE(chosen$ok), info = chosen$error)
  expect_identical(chosen$value@datasets[[1]]@specs@columns[[1]]@values, "EOT")
})

test_that("without a slot, the column binding alone still reaches the built DTA", {
  dir <- vocab_slot_source_dir(character(0))

  withr::local_envvar(c(
    DTATOOLS_TEMPLATE_SOURCES = paste0("t=dir:", dir),
    DTATOOLS_TEMPLATE_INCLUDE_BUILTIN = ""
  ))
  app_fn("dta_template_index_invalidate")()
  idx <- app_fn("build_template_index")(refresh = TRUE)

  loaded <- app_fn("load_template_definition")("with_slot@1.0", index = idx)
  built <- app_fn("create_dta_from_template")(loaded$value$def, loaded$value$path, index = idx)

  expect_true(isTRUE(built$ok), info = built$error)
  expect_identical(
    built$value@datasets[[1]]@specs@columns[[1]]@values,
    c("SCR", "C1D1", "EOT")
  )
})

test_that("a slot naming a dataset the template does not build fails loudly", {
  dir <- vocab_slot_source_dir(c(
    "vocabulary_slots:",
    "  - id: visit_choice",
    "    target: datasets.nosuchds.columns.VISIT.values",
    "    vocabulary: visit@1.0",
    "    default: [SCR]"
  ))

  withr::local_envvar(c(
    DTATOOLS_TEMPLATE_SOURCES = paste0("t=dir:", dir),
    DTATOOLS_TEMPLATE_INCLUDE_BUILTIN = ""
  ))
  app_fn("dta_template_index_invalidate")()
  idx <- app_fn("build_template_index")(refresh = TRUE)

  loaded <- app_fn("load_template_definition")("with_slot@1.0", index = idx)
  built <- app_fn("create_dta_from_template")(loaded$value$def, loaded$value$path, index = idx)

  expect_false(isTRUE(built$ok))
  expect_match(built$error, "nosuchds", fixed = TRUE)
})

test_that("vocabulary_slots merge by id across extends:", {
  merge_inherit <- app_fn("resolve_template_inheritance")

  parent <- list(
    id = "p", version = "1.0",
    vocabulary_slots = list(
      list(id = "a", target = "datasets.ds1.columns.A.values", vocabulary = "v@1.0", default = "X"),
      list(id = "b", target = "datasets.ds1.columns.B.values", vocabulary = "v@1.0")
    )
  )
  child <- list(
    id = "c", version = "1.0", extends = "p@1.0",
    vocabulary_slots = list(list(id = "a", default = "Y"))
  )

  out <- merge_inherit(child, function(ref) list(def = parent, id = "p", version = "1.0"))
  slots <- out$def$vocabulary_slots
  expect_length(slots, 2L)
  # The child overrode one field of slot "a" without restating slot "b" or
  # the rest of "a".
  a <- Filter(function(s) identical(s$id, "a"), slots)[[1]]
  expect_identical(as.character(a$default), "Y")
  expect_identical(as.character(a$target), "datasets.ds1.columns.A.values")
})

test_that("template_provenance() records vocabulary selections as a sibling of selections", {
  prov_fn <- app_fn("template_provenance")

  with_sel <- prov_fn(
    list(id = "t", version = "1.0"), list(id = "t", version = "1.0"),
    selections = list(opt = "x"),
    vocab_selections = list(visit_choice = c("SCR", "EOT"))
  )
  expect_identical(with_sel$vocabulary_selections$visit_choice, c("SCR", "EOT"))
  # A sibling, never an entry inside `selections`: rebase replays `selections`
  # through the template's `options:`, where a slot id is not an option id.
  expect_null(with_sel$selections$visit_choice)

  # Omitted entirely when the template offered no slots, so an unaffected
  # document's provenance block does not grow an empty key.
  without <- prov_fn(list(id = "t", version = "1.0"), list(id = "t", version = "1.0"), selections = list())
  expect_false("vocabulary_selections" %in% names(without))
})


# ---- Index registration ----------------------------------------------------

test_that("dta_vocabulary is an indexed kind with a non-colliding suffix", {
  all_kinds <- app_fn("dta_template_all_kinds")()
  pattern_for <- app_fn("dta_template_kind_pattern")

  expect_true("dta_vocabulary" %in% all_kinds)

  vocab_pat <- pattern_for("dta_vocabulary")
  expect_true(grepl(vocab_pat, "visit.dta-vocabulary.yaml"))
  expect_true(grepl(vocab_pat, "visit.dta-vocabulary.yml"))

  # The four suffixes must stay mutually exclusive: a file of one kind must
  # never be swept up by another kind's pattern.
  names <- c(
    dta_creation_template = "x.dta-template.yaml",
    dta_dataset_template = "x.dta-dataset-template.yaml",
    dta_party_profile = "x.dta-party.yaml",
    dta_vocabulary = "x.dta-vocabulary.yaml"
  )
  for (kind in all_kinds) {
    pat <- pattern_for(kind)
    matched <- names(names)[vapply(names, function(f) grepl(pat, f), logical(1))]
    expect_identical(matched, kind, info = paste("pattern for", kind, "matched", paste(matched, collapse = ", ")))
  }
})

test_that("build_template_index() picks up a vocabulary file", {
  build_index <- app_fn("build_template_index")
  dir <- withr::local_tempdir()
  write_vocabulary_fixture(dir)

  withr::local_envvar(c(
    DTATOOLS_TEMPLATE_SOURCES = paste0("t=dir:", dir),
    DTATOOLS_TEMPLATE_INCLUDE_BUILTIN = ""
  ))
  app_fn("dta_template_index_invalidate")()

  idx <- build_index(refresh = TRUE)
  rows <- idx[idx$kind == "dta_vocabulary", , drop = FALSE]
  expect_identical(rows$id, "visit")
  expect_identical(rows$version, "1.0")
})
