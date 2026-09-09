# Coverage for the picker's family-tree helpers in inst/shiny/dta_app/R/
# template_index.R: template_picker_entries(), template_id_versions(),
# template_picker_lineage(), template_picker_tokens() and
# template_picker_filter(). Reached via the app_env()/app_fn() harness in
# helper-shinyapp.R (see that file for why the app's helper files are not
# part of the package namespace and must be sourced into a private
# environment to be unit tested).
#
# None of the functions under test reads a file, a directory, or a
# DTATOOLS_TEMPLATE_* environment variable -- they operate purely on the
# `index`/`entries` data frames passed in. Every fixture below is therefore a
# plain data.frame built in-process by mk_index()/idx_of(), never a template
# file on disk: that keeps a chain-of-several/cycle/missing-parent fixture to
# one line per template instead of one YAML file per template.

Sys.setenv(NOT_CRAN = "true")

skip_if_not_installed("shiny")
skip_if_not_installed("bslib")
skip_if_not_installed("DT")
skip_if_not_installed("shinyjs")

# The index's column set, read from the app itself rather than spelled out
# here a second time: if build_template_index() ever renames or adds a
# column, mk_index() should fail loudly instead of quietly building rows the
# real index could never produce.
index_columns <- function() app_fn("dta_template_index_columns")()

# One row of the raw template index, in build_template_index()'s own column
# order (enforced by the final re-index below) -- built directly rather than
# through a template file and build_template_index() itself, since nothing
# under test here ever reads one.
mk_index <- function(id, version = "1.0", label = id, description = "",
                     path = NULL, source_name = "pkg", source_kind = "dir",
                     resolved_commit = NA_character_, abstract = FALSE,
                     extends = NA_character_, kind = "dta_creation_template") {
  if (is.null(path)) {
    path <- paste0(id, ".dta-template.yaml")
  }
  row <- data.frame(
    kind = kind, id = id, version = version, label = label,
    description = description, path = path, source_name = source_name,
    source_kind = source_kind, resolved_commit = resolved_commit,
    abstract = abstract, extends = extends, stringsAsFactors = FALSE
  )
  row[, index_columns(), drop = FALSE]
}

# rbind() a handful of mk_index() rows into one index -- named for what it
# reads like at each call site: idx_of(root, child, grandchild).
idx_of <- function(...) do.call(rbind, list(...))

# ---- template_picker_entries() / template_id_versions() --------------------

test_that("template_picker_entries() keeps the top version per id, source then label order", {
  idx <- idx_of(
    mk_index("foo", version = "1.9", label = "Foo Old", source_name = "b_source"),
    mk_index("foo", version = "1.10", label = "Foo New", source_name = "b_source"),
    mk_index("bar", version = "1.0", label = "Bar", source_name = "a_source"),
    mk_index("abstract_one", label = "Abstract One", abstract = TRUE)
  )

  entries <- app_fn("template_picker_entries")(idx)

  # "1.10" outranks "1.9" (numeric_version, not a string compare); the
  # abstract id never appears; "a_source" sorts before "b_source".
  expect_identical(entries$id, c("bar", "foo"))
  expect_identical(entries$version, c("1.0", "1.10"))
  expect_identical(entries$label, c("Bar", "Foo New"))
})

test_that("template_id_versions() lists every version of one id, newest first", {
  idx <- idx_of(
    mk_index("foo", version = "1.9", label = "Foo Old"),
    mk_index("foo", version = "1.10", label = "Foo New")
  )

  vv <- app_fn("template_id_versions")(idx, "foo")

  expect_identical(unname(vv), c("1.10", "1.9"))
  expect_identical(names(vv), c("1.10", "1.9"))
})

# ---- template_picker_lineage() ---------------------------------------------

test_that("template_picker_lineage() builds tree order and keeps an unrelated root separate", {
  # base
  # +-- vendor (-> vendor_study)
  # +-- vendor_two
  # base2 (an unrelated second root whose label starts with the first
  # root's label -- see the WHY comment on sort_key in template_index.R for
  # why a plain, unseparated label concatenation would sort base2 into the
  # middle of base's own family instead of after all of it)
  idx <- idx_of(
    mk_index("base", label = "Base"),
    mk_index("vendor", label = "Vendor", extends = "base"),
    mk_index("vendor_study", label = "Vendor Study", extends = "vendor"),
    mk_index("vendor_two", label = "Vendor Two", extends = "base"),
    mk_index("base2", label = "Base2")
  )
  entries <- app_fn("template_picker_entries")(idx)

  lin <- app_fn("template_picker_lineage")(idx, entries)

  # Tree order: base before its children, "Vendor" before its sibling
  # "Vendor Two" (label order), vendor_study nested right after its parent
  # vendor and before vendor's sibling, and base2 strictly after all of
  # base's family.
  expect_identical(lin$id, c("base", "vendor", "vendor_study", "vendor_two", "base2"))
  expect_identical(lin$depth, c(0L, 1L, 2L, 1L, 0L))
  expect_identical(lin$family, c("base", "base", "base", "base", "base2"))
  expect_identical(lin$lineage, c("", "Base", "Base \u203a Vendor", "Base", ""))
  expect_identical(rownames(lin), as.character(1:5))
})

test_that("template_picker_lineage() gives an abstract root's children depth without indent", {
  idx <- idx_of(
    mk_index("abstract_base", label = "Abstract Base", abstract = TRUE),
    mk_index("child1", label = "Child One", extends = "abstract_base"),
    mk_index("child2", label = "Child Two", extends = "abstract_base")
  )
  entries <- app_fn("template_picker_entries")(idx)
  # The abstract root is never itself an entry to render...
  expect_identical(entries$id, c("child1", "child2"))

  lin <- app_fn("template_picker_lineage")(idx, entries)

  # ...but it still names the family, and depth still counts it as an
  # ancestor -- only `indent` (visible ancestors) excludes it, so a child of
  # an abstract root does not render indented under nothing.
  expect_identical(lin$depth, c(1L, 1L))
  expect_identical(lin$indent, c(0L, 0L))
  expect_identical(lin$family, c("abstract_base", "abstract_base"))
  expect_identical(lin$family_label, c("Abstract Base", "Abstract Base"))
})

test_that("template_picker_lineage() keeps a missing parent's raw id as its own label", {
  idx <- idx_of(mk_index("orphan", label = "Orphan", extends = "does_not_exist"))
  entries <- app_fn("template_picker_entries")(idx)

  lin <- app_fn("template_picker_lineage")(idx, entries)

  expect_identical(lin$depth, 1L)
  expect_identical(lin$lineage, "does_not_exist")
  expect_identical(lin$family, "does_not_exist")
  expect_identical(lin$family_label, "does_not_exist")
})

test_that("template_picker_lineage() strips '@version'/'@latest' before resolving a parent", {
  idx <- idx_of(
    mk_index("base", label = "Base", version = "1.0"),
    mk_index("child_exact", label = "Child Exact", extends = "base@1.0"),
    mk_index("child_latest", label = "Child Latest", extends = "base@latest")
  )
  entries <- app_fn("template_picker_entries")(idx)

  lin <- app_fn("template_picker_lineage")(idx, entries)

  expect_identical(lin$family[lin$id == "child_exact"], "base")
  expect_identical(lin$family[lin$id == "child_latest"], "base")
})

test_that("template_picker_lineage() terminates a two-node extends cycle at a finite depth", {
  idx <- idx_of(
    mk_index("a", label = "A", extends = "b"),
    mk_index("b", label = "B", extends = "a")
  )
  entries <- app_fn("template_picker_entries")(idx)

  lin <- app_fn("template_picker_lineage")(idx, entries)

  max_depth <- get("dta_template_max_inheritance_depth", envir = app_env())
  expect_true(all(lin$depth <= max_depth))
  expect_identical(lin$depth, c(1L, 1L))
})

test_that("template_picker_lineage() returns a zero-row frame with the new columns", {
  idx <- idx_of(mk_index("solo", label = "Solo"))[0, , drop = FALSE]
  entries <- app_fn("template_picker_entries")(idx)
  expect_equal(nrow(entries), 0)

  lin <- app_fn("template_picker_lineage")(idx, entries)

  expect_equal(nrow(lin), 0)
  expect_identical(
    names(lin),
    c(
      index_columns(), "family", "family_label", "depth", "indent",
      "lineage", "search_text", "sort_key"
    )
  )
  expect_type(lin$family, "character")
  expect_type(lin$depth, "integer")
  expect_type(lin$indent, "integer")
})

# ---- template_picker_tokens() -----------------------------------------------

test_that("template_picker_tokens() lower-cases, trims, splits on whitespace, and drops blanks", {
  fn <- app_fn("template_picker_tokens")

  expect_identical(fn("  Foo   Bar  "), c("foo", "bar"))
  expect_identical(fn(""), character(0))
  expect_identical(fn("   "), character(0))
  expect_identical(fn(NULL), character(0))
})

# ---- template_picker_filter() ----------------------------------------------

test_that("template_picker_filter() ANDs tokens, matches ancestor text, and restricts by source", {
  idx <- idx_of(
    mk_index("base", label = "Base", source_name = "pkg"),
    mk_index("vendor", label = "Vendor", extends = "base", source_name = "pkg"),
    mk_index("vendor_study", label = "Vendor Study", extends = "vendor", source_name = "lab"),
    mk_index("other", label = "Other Template", source_name = "pkg")
  )
  entries <- app_fn("template_picker_entries")(idx)
  lin <- app_fn("template_picker_lineage")(idx, entries)
  filter_fn <- app_fn("template_picker_filter")

  # "vendor" is in vendor's own label, and in vendor_study's ancestor label;
  # neither base nor other ever mentions it.
  expect_identical(filter_fn(lin, "vendor")$id, c("vendor", "vendor_study"))

  # the root's own id reaches every family member's search_text (its own
  # fields for `base` itself, an ancestor id for the other two).
  expect_identical(filter_fn(lin, "base")$id, c("base", "vendor", "vendor_study"))

  # two tokens are ANDed, not ORed: "vendor" alone also matches `vendor`
  # itself, but adding "study" narrows to the one row with both.
  expect_identical(filter_fn(lin, "vendor study")$id, "vendor_study")

  # the source filter restricts independently of the search text.
  expect_identical(filter_fn(lin, "", source = "lab")$id, "vendor_study")

  # an empty query keeps everything, in the same (tree) order.
  expect_identical(filter_fn(lin, "")$id, c("base", "vendor", "vendor_study", "other"))
})

test_that("template_picker_filter() matches at the start of a word only", {
  idx <- idx_of(
    mk_index("solo", label = "Stand-alone form", description = "One stand-alone template."),
    mk_index("gf_acme", label = "GF (ACME)", description = "Vendor deviation."),
    mk_index("pruefung", label = "Prüfung", description = "Umlaut label.")
  )
  entries <- app_fn("template_picker_entries")(idx)
  lin <- app_fn("template_picker_lineage")(idx, entries)
  filter_fn <- app_fn("template_picker_filter")

  # "one" is a word of its own in the description ("One stand-alone") but the
  # "one" inside "stand-alone" must not count: only solo has a real one.
  expect_identical(filter_fn(lin, "one")$id, "solo")
  # A word-start hit through a hyphen and an underscore, and a plain prefix.
  expect_identical(filter_fn(lin, "alone")$id, "solo")
  expect_identical(filter_fn(lin, "acme")$id, "gf_acme")
  expect_identical(filter_fn(lin, "sta")$id, "solo")
  # Inside a word, no hit -- including after a non-ASCII letter.
  expect_identical(filter_fn(lin, "lone")$id, character(0))
  expect_identical(filter_fn(lin, "fung")$id, character(0))
  expect_identical(filter_fn(lin, "prüf")$id, "pruefung")
  # Regex metacharacters in a token are literal.
  expect_identical(filter_fn(lin, "(acme)")$id, "gf_acme")
  expect_identical(filter_fn(lin, "s.a")$id, character(0))
})
