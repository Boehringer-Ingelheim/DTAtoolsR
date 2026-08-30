# Coverage for inst/shiny/dta_app/R/template_inherit.R, reached via the
# app_env()/app_fn() harness in helper-shinyapp.R (see that file for why this
# is necessary: the app's helper files are auto-sourced by Shiny at launch and
# are not part of the package namespace).
#
# This file deliberately never touches YAML on disk (other than the one
# throwaway probe of yaml::yaml.load()'s null representation, which is the
# empirical fact the removal semantics below rest on): every fixture is a
# plain R list, and `resolve_ref` is a stub closing over a small named list of
# definitions -- exactly the injection seam template_inherit.R is built
# around, so the not-yet-written template index never has to exist for this
# suite to run.

Sys.setenv(NOT_CRAN = "true")

# cli wraps a long condition message across lines at the console width, which
# breaks a naive substring/regexp match the moment the wrap lands inside the
# very text being asserted on (verified empirically while writing this file --
# a 9-hop chain message wrapped mid-chain). Collapsing whitespace before
# matching keeps the assertion about the MESSAGE CONTENT rather than about
# where cli happened to wrap it.
expect_error_message_contains <- function(expr, fixed_text) {
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

# A resolve_ref() stub backed by a small named list of definitions, as
# specified: `defs` is named by the same string a def's `extends:` field
# would contain, and each element already carries its own id/version, exactly
# as a real template file must.
make_resolver <- function(defs) {
  function(ref) {
    hit <- defs[[ref]]
    if (is.null(hit)) {
      return(NULL)
    }
    list(def = hit, id = hit$id, version = hit$version)
  }
}

# ---- dta_template_merge_value ----------------------------------------------

test_that("dta_template_merge_value() deep-merges nested mappings, child leaf wins", {
  fn <- app_fn("dta_template_merge_value")
  parent <- list(a = 1, nested = list(x = 1, y = 2), untouched = "keep")
  child <- list(nested = list(y = 20, z = 3))

  result <- fn(parent, child)

  expect_equal(result$nested, list(x = 1, y = 20, z = 3))
  expect_equal(result$untouched, "keep")
  expect_equal(result$a, 1)
})

test_that("dta_template_merge_value() leaves a parent value untouched when the child omits its key", {
  fn <- app_fn("dta_template_merge_value")
  parent <- list(keep = "yes", other = 1)
  child <- list(other = 2)

  result <- fn(parent, child)

  expect_equal(result$keep, "yes")
  expect_equal(result$other, 2)
})

test_that("dta_template_merge_value() removes a key the child sets to an explicit NULL", {
  fn <- app_fn("dta_template_merge_value")
  # Built via yaml::yaml.load() rather than list(a = NULL, ...) by hand, so
  # this test exercises the SAME representation the merge logic is designed
  # around: a name present in names(child) mapped to a real NULL element,
  # which is what yaml::yaml.load("a: ~") and "a: null" both produce.
  parent <- list(a = "inherited", b = "kept")
  child <- yaml::yaml.load("a: ~\nb: kept\n")

  expect_true("a" %in% names(child))
  expect_null(child[["a"]])

  result <- fn(parent, child)

  expect_false("a" %in% names(result))
  expect_equal(result$b, "kept")
})

test_that("dta_template_merge_value() replaces an unnamed (sequence) list wholesale, never element-wise", {
  fn <- app_fn("dta_template_merge_value")
  parent <- list(columns = list("A", "B", "C", "D"))
  child <- list(columns = list("X", "Y"))

  result <- fn(parent, child)

  # Exact equality, not merely "same length": an element-wise merge would
  # produce list("X", "Y", "C", "D") for these inputs, which is real data
  # corruption for a column spec list, not a plausible alternate reading.
  expect_identical(result$columns, child$columns)
})

test_that("dta_template_merge_value() lets a scalar replace an inherited list", {
  fn <- app_fn("dta_template_merge_value")
  parent <- list(v = list(1, 2, 3))
  child <- list(v = "now a scalar")

  result <- fn(parent, child)

  expect_identical(result$v, "now a scalar")
})

# ---- dta_template_merge_options ---------------------------------------------

test_that("dta_template_merge_options() deep-merges options sharing an id, child wins", {
  fn <- app_fn("dta_template_merge_options")
  parent_opts <- list(list(id = "title", label = "Old label", target = "metadata.title"))
  child_opts <- list(list(id = "title", label = "New label"))

  result <- fn(parent_opts, child_opts)

  expect_length(result, 1)
  expect_equal(result[[1]]$label, "New label")
  # `target` was never mentioned by the child -- it survives from the parent.
  expect_equal(result[[1]]$target, "metadata.title")
})

test_that("dta_template_merge_options() drops an inherited option when the child sets remove: true", {
  fn <- app_fn("dta_template_merge_options")
  parent_opts <- list(list(id = "a", label = "A"), list(id = "b", label = "B"))
  child_opts <- list(list(id = "a", remove = TRUE))

  result <- fn(parent_opts, child_opts)

  expect_length(result, 1)
  expect_equal(result[[1]]$id, "b")
})

test_that("dta_template_merge_options() appends a new child option after every inherited one", {
  fn <- app_fn("dta_template_merge_options")
  parent_opts <- list(list(id = "a", label = "A"), list(id = "b", label = "B"))
  child_opts <- list(list(id = "b", label = "B-updated"), list(id = "c", label = "C-new"))

  result <- fn(parent_opts, child_opts)

  ids <- vapply(result, function(o) o$id, character(1))
  expect_equal(ids, c("a", "b", "c"))
  expect_equal(result[[2]]$label, "B-updated")
  expect_equal(result[[3]]$label, "C-new")
})

test_that("dta_template_merge_options() honours `order`, and warns on an unknown id in it", {
  fn <- app_fn("dta_template_merge_options")
  parent_opts <- list(list(id = "a"), list(id = "b"), list(id = "c"))

  # expect_warning() returns the captured CONDITION, not the expression's
  # value -- assigning `result` inside the expression (rather than to
  # expect_warning()'s own return) is what actually captures the merge
  # result while still asserting the warning fired.
  expect_warning(
    result <- fn(parent_opts, list(), order = c("c", "a", "no-such-id")),
    regexp = "[Uu]nknown id"
  )

  # Named-in-order ids come first in the requested order; anything left over
  # ("b") keeps its original relative position and follows.
  expect_equal(vapply(result, function(o) o$id, character(1)), c("c", "a", "b"))
})

test_that("dta_template_merge_options() drops an option with no id or an empty id, with a warning", {
  fn <- app_fn("dta_template_merge_options")
  parent_opts <- list(list(id = "a"), list(label = "no id at all"), list(id = "", label = "empty id"))

  expect_warning(result <- fn(parent_opts, list()), regexp = "missing or empty")

  expect_length(result, 1)
  expect_equal(result[[1]]$id, "a")
})

# ---- dta_template_merge_datasets --------------------------------------------

test_that("dta_template_merge_datasets() keys on `as`, ahead of a conflicting `template`", {
  fn <- app_fn("dta_template_merge_datasets")
  parent_ds <- list(list(as = "slot1", template = "unrelated@1.0", val = "p"))
  child_ds <- list(list(as = "slot1", template = "totally-different@9.0", val = "c"))

  result <- fn(parent_ds, child_ds)

  # If `template` (not `as`) had driven the key, these would never match --
  # "unrelated" != "totally-different" -- and the merge would have produced
  # two entries instead of one merged entry with the child's value.
  expect_length(result, 1)
  expect_equal(result[[1]]$val, "c")
})

test_that("dta_template_merge_datasets() keys on `template`, ahead of a conflicting `source`", {
  fn <- app_fn("dta_template_merge_datasets")
  parent_ds <- list(list(template = "gf@1.0", source = "parent.yaml", val = "p"))
  child_ds <- list(list(template = "gf@2.0", source = "child.yaml", val = "c"))

  result <- fn(parent_ds, child_ds)

  expect_length(result, 1)
  expect_equal(result[[1]]$val, "c")
  expect_equal(result[[1]]$template, "gf@2.0")
})

test_that("dta_template_merge_datasets() keys on `source`, ahead of a conflicting `name`", {
  fn <- app_fn("dta_template_merge_datasets")
  parent_ds <- list(list(source = "shared.yaml", name = "parent_name", val = "p"))
  child_ds <- list(list(source = "shared.yaml", name = "child_name", val = "c"))

  result <- fn(parent_ds, child_ds)

  expect_length(result, 1)
  expect_equal(result[[1]]$val, "c")
})

test_that("dta_template_merge_datasets() keys a plain character entry by the string itself", {
  fn <- app_fn("dta_template_merge_datasets")

  same <- fn(list("gf_dataset.yaml"), list("gf_dataset.yaml"))
  expect_length(same, 1)
  expect_identical(same[[1]], "gf_dataset.yaml")

  different <- fn(list("gf_dataset.yaml"), list("other_dataset.yaml"))
  expect_length(different, 2)
})

test_that("`template: gf@3.0` and `template: gf@4.0` resolve to the SAME key and merge", {
  fn <- app_fn("dta_template_merge_datasets")
  parent_ds <- list(list(template = "gf@3.0", a = 1))
  child_ds <- list(list(template = "gf@4.0", b = 2))

  result <- fn(parent_ds, child_ds)

  expect_length(result, 1)
  expect_equal(result[[1]]$template, "gf@4.0")
  expect_equal(result[[1]]$a, 1)
  expect_equal(result[[1]]$b, 2)
})

test_that("dta_template_merge_datasets() drops an inherited entry when the child sets remove: true", {
  fn <- app_fn("dta_template_merge_datasets")
  parent_ds <- list(list(name = "n1"), list(name = "n2"))
  child_ds <- list(list(name = "n1", remove = TRUE))

  result <- fn(parent_ds, child_ds)

  expect_length(result, 1)
  expect_equal(result[[1]]$name, "n2")
})

test_that("dta_template_merge_datasets() appends an unkeyable entry as-is, never merging it", {
  fn <- app_fn("dta_template_merge_datasets")
  # Neither entry has as/template/source/name, and neither is a bare
  # character -- dta_template_dataset_key() has nothing to key either one on.
  parent_ds <- list(list(inline = "parent-only-field"))
  child_ds <- list(list(inline = "child-only-field"))

  result <- fn(parent_ds, child_ds)

  expect_length(result, 2)
  values <- vapply(result, function(x) x$inline, character(1))
  expect_setequal(values, c("parent-only-field", "child-only-field"))
})

# ---- resolve_template_inheritance -------------------------------------------

test_that("resolve_template_inheritance() is a no-op with empty lineage when there is no extends", {
  fn <- app_fn("resolve_template_inheritance")
  def <- list(id = "solo", version = "1.0", base = list(x = 1))

  result <- fn(def, resolve_ref = function(ref) NULL)

  expect_identical(result$def, def)
  expect_identical(result$lineage, character(0))
})

test_that("resolve_template_inheritance() merges a one-level chain and reports lineage 'parent@1.0'", {
  fn <- app_fn("resolve_template_inheritance")
  parent_def <- list(
    id = "parent", version = "1.0",
    base = list(metadata = list(title = "Parent title", header = "Parent header")),
    options = list(list(id = "o1", label = "Inherited option"))
  )
  child_def <- list(
    id = "child", version = "2.0", extends = "parent",
    base = list(metadata = list(title = "Child title"))
  )
  resolver <- make_resolver(list(parent = parent_def))

  result <- fn(child_def, resolver)

  expect_equal(result$lineage, "parent@1.0")
  expect_equal(result$def$base$metadata$title, "Child title") # child override
  expect_equal(result$def$base$metadata$header, "Parent header") # inherited, untouched
  expect_equal(result$def$options[[1]]$label, "Inherited option")
})

test_that("resolve_template_inheritance() applies the grandparent before the parent, on a two-level chain", {
  fn <- app_fn("resolve_template_inheritance")
  grandparent_def <- list(
    id = "gp", version = "1.0",
    base = list(field = "gp_value", untouched_by_anyone = "from_gp")
  )
  parent_def <- list(
    id = "p", version = "1.0", extends = "gp",
    base = list(field = "p_value")
  )
  child_def <- list(id = "c", version = "1.0", extends = "p")
  resolver <- make_resolver(list(gp = grandparent_def, p = parent_def))

  result <- fn(child_def, resolver)

  # Nearest ancestor first.
  expect_equal(result$lineage, c("p@1.0", "gp@1.0"))
  # The parent's override of the grandparent's field survives untouched
  # through the child, which never mentions `field` at all.
  expect_equal(result$def$base$field, "p_value")
  # A field only the grandparent ever set, and neither descendant touches,
  # survives the whole chain.
  expect_equal(result$def$base$untouched_by_anyone, "from_gp")
})

test_that("resolve_template_inheritance() keeps the CHILD's id and version, never the parent's", {
  fn <- app_fn("resolve_template_inheritance")
  parent_def <- list(id = "parent_id", version = "9.9", base = list())
  child_def <- list(id = "child_id", version = "0.1", extends = "parent_id", base = list())
  resolver <- make_resolver(list(parent_id = parent_def))

  result <- fn(child_def, resolver)

  expect_equal(result$def$id, "child_id")
  expect_equal(result$def$version, "0.1")
})

test_that("resolve_template_inheritance() strips `extends` from the merged result", {
  fn <- app_fn("resolve_template_inheritance")
  parent_def <- list(id = "parent", version = "1.0", base = list())
  child_def <- list(id = "child", version = "1.0", extends = "parent", base = list())
  resolver <- make_resolver(list(parent = parent_def))

  result <- fn(child_def, resolver)

  expect_false("extends" %in% names(result$def))
})

test_that("resolve_template_inheritance() aborts a self-referencing cycle, naming the chain", {
  fn <- app_fn("resolve_template_inheritance")
  self_def <- list(id = "loopy", version = "1.0", extends = "loopy")
  resolver <- make_resolver(list(loopy = self_def))

  expect_error_message_contains(fn(self_def, resolver), "loopy@1.0 -> loopy@1.0")
})

test_that("resolve_template_inheritance() aborts an A -> B -> A cycle, naming the full chain", {
  fn <- app_fn("resolve_template_inheritance")
  a_def <- list(id = "A", version = "1.0", extends = "B")
  b_def <- list(id = "B", version = "1.0", extends = "A")
  resolver <- make_resolver(list(A = a_def, B = b_def))

  expect_error_message_contains(fn(a_def, resolver), "A@1.0 -> B@1.0 -> A@1.0")
})

test_that("resolve_template_inheritance() aborts a chain of depth 9, naming the chain", {
  fn <- app_fn("resolve_template_inheritance")
  # T1 extends T2 extends ... extends T10: nine `extends:` hops, one more
  # than the documented limit of 8.
  n <- 10
  defs <- stats::setNames(
    lapply(seq_len(n), function(i) {
      list(
        id = paste0("T", i), version = "1.0",
        extends = if (i < n) paste0("T", i + 1) else NULL
      )
    }),
    paste0("T", seq_len(n))
  )
  resolver <- make_resolver(defs)

  expect_error_message_contains(fn(defs[["T1"]], resolver), "depth limit of 8")
  expect_error_message_contains(fn(defs[["T1"]], resolver), "T1@1.0 -> T2@1.0")
})

test_that("resolve_template_inheritance() succeeds at exactly 8 hops, the documented limit", {
  fn <- app_fn("resolve_template_inheritance")
  n <- 9 # 9 templates == 8 `extends:` hops from U1 down to U9
  defs <- stats::setNames(
    lapply(seq_len(n), function(i) {
      list(
        id = paste0("U", i), version = "1.0",
        extends = if (i < n) paste0("U", i + 1) else NULL
      )
    }),
    paste0("U", seq_len(n))
  )
  resolver <- make_resolver(defs)

  result <- fn(defs[["U1"]], resolver)

  expect_length(result$lineage, 8)
})

test_that("resolve_template_inheritance() aborts on an unresolvable extends reference, naming it and the child's id", {
  fn <- app_fn("resolve_template_inheritance")
  child_def <- list(id = "orphan", version = "1.0", extends = "ghost-parent")

  expect_error_message_contains(fn(child_def, function(ref) NULL), "orphan")
  expect_error_message_contains(fn(child_def, function(ref) NULL), "ghost-parent")
})
