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

# ---- the four states, in every section --------------------------------------

# absent -> inherit, a value -> override, `{}`/`[]` -> empty, `null` -> drop.
# These are walked for EVERY section rather than tested once and assumed for
# the rest: the defect this replaces was exactly one section (`base:`) behaving
# unlike the others, and only a per-section table can catch that class of bug.
#
# Note the fixtures use the `list(key = NULL)` CONSTRUCTOR form for the drop
# state. That is not incidental: `l$key <- NULL` deletes the element, whereas
# `list(key = NULL)` keeps the name with a NULL value -- the same shape
# yaml::read_yaml() produces for `key: ~`, and the only shape that can express
# "written, as null" rather than "never written".

four_state_parent <- function() {
  list(
    id = "parent", version = "1.0",
    base = list(metadata = list(title = "P title", header = "P header")),
    options = list(list(id = "o1", label = "P option")),
    datasets = list(list(as = "d1", template = "dt@1.0")),
    party_slots = list(list(id = "ps1", target = "metadata.supplier")),
    vocabulary_slots = list(list(id = "vs1", target = "x"))
  )
}

four_state_child <- function(...) {
  c(list(id = "child", version = "1.0", extends = "parent"), list(...))
}

four_state_resolve <- function(child) {
  app_fn("resolve_template_inheritance")(child, make_resolver(list(parent = four_state_parent())))
}

test_that("an absent base: inherits the parent's section untouched", {
  result <- four_state_resolve(four_state_child())

  expect_equal(result$def$base$metadata$title, "P title")
  expect_equal(result$def$base$metadata$header, "P header")
})

test_that("a base: value deep-merges, leaving keys the child did not mention", {
  result <- four_state_resolve(
    four_state_child(base = list(metadata = list(title = "C title")))
  )

  expect_equal(result$def$base$metadata$title, "C title")
  expect_equal(result$def$base$metadata$header, "P header")
})

test_that("an explicitly empty base: replaces the parent's section with nothing", {
  result <- four_state_resolve(four_state_child(base = list()))

  expect_true("base" %in% names(result$def))
  expect_length(result$def$base, 0)
})

test_that("a null base: drops the section entirely rather than emptying it", {
  result <- four_state_resolve(four_state_child(base = NULL))

  expect_false("base" %in% names(result$def))
})

# The collections take the same four states. One block, four assertions per
# section, so a section that drifts out of line fails here rather than being
# discovered years later in a template that quietly lost its parent's set.
for (.section in list(
  list(key = "options", added = list(id = "o2", label = "C option"), inherited = "o1"),
  list(key = "party_slots", added = list(id = "ps2", target = "metadata.receiver"), inherited = "ps1"),
  list(key = "vocabulary_slots", added = list(id = "vs2", target = "y"), inherited = "vs1"),
  list(key = "datasets", added = list(as = "d2", template = "dt2@1.0"), inherited = "d1")
)) {
  local({
    sec <- .section

    test_that(paste0("an absent ", sec$key, ": inherits the parent's entries"), {
      result <- four_state_resolve(four_state_child())

      expect_length(result$def[[sec$key]], 1)
    })

    test_that(paste0("a ", sec$key, ": value merges with the inherited entries"), {
      child <- four_state_child()
      child[[sec$key]] <- list(sec$added)

      result <- four_state_resolve(child)

      expect_length(result$def[[sec$key]], 2)
    })

    test_that(paste0("an explicitly empty ", sec$key, ": empties the section"), {
      child <- four_state_child()
      child[[sec$key]] <- list()

      result <- four_state_resolve(child)

      expect_length(result$def[[sec$key]], 0)
      # Still PRESENT, though: empty and dropped are different states, and the
      # section below asserts the drop.
      expect_true(sec$key %in% names(result$def))
    })

    test_that(paste0("a null ", sec$key, ": drops the section entirely"), {
      child <- four_state_child()
      child[sec$key] <- list(NULL)

      result <- four_state_resolve(child)

      expect_false(sec$key %in% names(result$def))
    })
  })
}

test_that("a blank scalar is kept blank and a null scalar is dropped", {
  blank <- four_state_resolve(four_state_child(label = ""))
  dropped <- four_state_resolve(four_state_child(label = NULL))
  inherited <- four_state_resolve(four_state_child())

  # "" is an override that happens to be empty: present, and blank.
  expect_identical(blank$def$label, "")
  # null is not an override at all -- the field is gone.
  expect_false("label" %in% names(dropped$def))
  # and an absent key still inherits, which is what makes the other two mean
  # something distinct.
  expect_false("label" %in% names(inherited$def))
})

# ---- collection verbs -------------------------------------------------------

verb_resolve <- function(section, spec) {
  child <- four_state_child()
  child[[section]] <- spec
  four_state_resolve(child)$def[[section]]
}

test_that("inherit: none replaces the parent's set wholesale", {
  result <- verb_resolve("options", list(
    inherit = "none",
    add = list(list(id = "o2", label = "Only mine"))
  ))

  expect_length(result, 1)
  expect_equal(result[[1]]$id, "o2")
})

test_that("inherit: [ids] keeps only the named subset of the parent's entries", {
  parent <- four_state_parent()
  parent$options <- list(
    list(id = "a"), list(id = "b"), list(id = "c")
  )
  child <- four_state_child(options = list(inherit = c("a", "c")))

  result <- app_fn("resolve_template_inheritance")(
    child, make_resolver(list(parent = parent))
  )

  expect_equal(vapply(result$def$options, function(o) o$id, character(1)), c("a", "c"))
})

test_that("inherit: all is the default, so bare verbs still start from the parent", {
  result <- verb_resolve("options", list(add = list(list(id = "o2"))))

  expect_equal(vapply(result, function(o) o$id, character(1)), c("o1", "o2"))
})

test_that("remove: drops an inherited entry by id", {
  result <- verb_resolve("options", list(remove = "o1"))

  expect_length(result, 0)
})

test_that("remove: aborts on an id that is not inherited rather than no-oping", {
  expect_error_message_contains(
    verb_resolve("options", list(remove = "nope")),
    "unknown id"
  )
})

test_that("modify: merges into an inherited entry, keeping its other fields", {
  result <- verb_resolve("options", list(
    modify = list(list(id = "o1", description = "added"))
  ))

  expect_length(result, 1)
  expect_equal(result[[1]]$label, "P option")
  expect_equal(result[[1]]$description, "added")
})

test_that("modify: aborts on an id that is not inherited, naming add as the remedy", {
  expect_error_message_contains(
    verb_resolve("options", list(modify = list(list(id = "typo")))),
    "no such entry is inherited"
  )
})

test_that("add: aborts when the id is already inherited, naming modify as the remedy", {
  # This is the whole reason the verb form exists: in the bare form the same
  # mistyped id silently becomes an extra entry instead of the modification
  # that was meant.
  expect_error_message_contains(
    verb_resolve("options", list(add = list(list(id = "o1", label = "oops")))),
    "already inherited"
  )
})

test_that("order: sorts the merged set, including entries add: introduced", {
  result <- verb_resolve("options", list(
    add = list(list(id = "o2")),
    order = c("o2", "o1")
  ))

  expect_equal(vapply(result, function(o) o$id, character(1)), c("o2", "o1"))
})

test_that("an unknown key in the verb form is an error, not a silently ignored entry", {
  expect_error_message_contains(
    verb_resolve("options", list(inhrit = "none")),
    "unknown key"
  )
})

test_that("the verb form and the bare form agree wherever both can say the thing", {
  # The compatibility claim is only worth as much as this assertion: adding a
  # new entry is expressible either way, and the two spellings must not drift.
  bare <- verb_resolve("options", list(list(id = "o2", label = "C option")))
  verbs <- verb_resolve("options", list(add = list(list(id = "o2", label = "C option"))))

  expect_identical(bare, verbs)
})

test_that("datasets: verbs key on the dataset identity, not on an id field", {
  # `datasets:` entries have no `id:` -- they are keyed by `as:`/`template:`/
  # `source:`/`name:` -- so the verbs have to run off the same identity
  # function the bare form uses, or they would silently match nothing.
  result <- verb_resolve("datasets", list(remove = "d1"))

  expect_length(result, 0)
})

test_that("an empty inherit: list means none, not everything", {
  # `%||%` in this engine treats a zero-length left operand as absent, so a
  # naive `spec$inherit %||% "all"` turned `inherit: []` into "inherit
  # everything" -- the exact opposite of what an empty id list says, and
  # silently, because inheriting the whole parent set looks like success.
  result <- verb_resolve("options", list(
    inherit = list(),
    add = list(list(id = "o2"))
  ))

  expect_equal(vapply(result, function(o) o$id, character(1)), "o2")
})

test_that("a null inherit: is the same instruction as an empty one", {
  result <- verb_resolve("options", list(inherit = NULL))

  expect_length(result, 0)
})

# ---- sealed ------------------------------------------------------------------

# A seal is enforced by comparing the value at the sealed path before and after
# the merge, so ONE check covers every route a child could take to the field.
# These tests exercise the routes separately for exactly that reason: if the
# implementation ever grows a per-rule guard instead, the route it forgets is
# the one that fails here.

sealed_resolve <- function(sealed, ...) {
  parent <- four_state_parent()
  parent$sealed <- sealed
  app_fn("resolve_template_inheritance")(
    four_state_child(...), make_resolver(list(parent = parent))
  )
}

test_that("dta_template_path_get() addresses a collection entry by its key", {
  # The path vocabulary has to reach inside an unnamed sequence, which
  # list_get_path() cannot do -- that is the whole reason this function exists.
  fn <- app_fn("dta_template_path_get")
  def <- four_state_parent()

  expect_equal(fn(def, "options.o1.label"), "P option")
  expect_equal(fn(def, "base.metadata.title"), "P title")
  # `datasets:` keys on `as:`, not on an id field.
  expect_equal(fn(def, "datasets.d1.template"), "dt@1.0")
  expect_null(fn(def, "options.nope.label"))
  expect_null(fn(def, "base.metadata.title.deeper"))
})

test_that("a sealed path cannot be changed by a base: override", {
  expect_error_message_contains(
    sealed_resolve("base.metadata.title", base = list(metadata = list(title = "C title"))),
    "which an ancestor sealed"
  )
})

test_that("a sealed path cannot be changed by a modify: verb", {
  expect_error_message_contains(
    sealed_resolve(
      "options.o1.label",
      options = list(modify = list(list(id = "o1", label = "Changed")))
    ),
    "which an ancestor sealed"
  )
})

test_that("a sealed entry cannot be dropped by a remove: verb", {
  expect_error_message_contains(
    sealed_resolve("options.o1", options = list(remove = "o1")),
    "which an ancestor sealed"
  )
})

test_that("a sealed field cannot be cancelled by an explicit null", {
  expect_error_message_contains(
    sealed_resolve("base.metadata.header", base = list(metadata = list(header = NULL))),
    "which an ancestor sealed"
  )
})

test_that("sealing a whole section forbids adding to it, not just editing it", {
  expect_error_message_contains(
    sealed_resolve("options", options = list(add = list(list(id = "o2")))),
    "which an ancestor sealed"
  )
})

test_that("a seal constrains its own path only, leaving siblings free", {
  result <- sealed_resolve(
    "base.metadata.title",
    base = list(metadata = list(header = "C header"))
  )

  expect_equal(result$def$base$metadata$title, "P title")
  expect_equal(result$def$base$metadata$header, "C header")
})

test_that("a template's own sealed: binds its descendants, not itself", {
  # A seal that bound its declarer would forbid the template from writing the
  # very field it is sealing, which would make the feature unusable.
  parent <- four_state_parent()
  child <- four_state_child(
    sealed = "base.metadata.title",
    base = list(metadata = list(title = "C title"))
  )

  result <- app_fn("resolve_template_inheritance")(
    child, make_resolver(list(parent = parent))
  )

  expect_equal(result$def$base$metadata$title, "C title")
  expect_equal(result$def$sealed, "base.metadata.title")
})

test_that("a seal survives two hops and binds a grandchild", {
  # The union is what carries it: the middle template neither declares nor
  # violates the seal, and must still pass it down intact.
  grandparent <- list(
    id = "gp", version = "1.0",
    base = list(metadata = list(title = "GP title")),
    sealed = "base.metadata.title"
  )
  parent <- list(
    id = "p", version = "1.0", extends = "gp",
    base = list(metadata = list(header = "P header"))
  )
  child <- list(
    id = "c", version = "1.0", extends = "p",
    base = list(metadata = list(title = "C title"))
  )
  resolver <- make_resolver(list(gp = grandparent, p = parent))

  expect_error_message_contains(
    app_fn("resolve_template_inheritance")(child, resolver),
    "which an ancestor sealed"
  )

  # The middle template itself resolves fine, and carries the seal onward.
  middle <- app_fn("resolve_template_inheritance")(parent, resolver)
  expect_equal(middle$def$sealed, "base.metadata.title")
})

test_that("sealed and required accumulate as a union rather than child-wins", {
  parent <- four_state_parent()
  parent$sealed <- "base.metadata.title"
  parent$required <- "base.metadata.header"
  child <- four_state_child(sealed = "options.o1", required = "options.o1.label")

  result <- app_fn("resolve_template_inheritance")(
    child, make_resolver(list(parent = parent))
  )

  expect_setequal(result$def$sealed, c("base.metadata.title", "options.o1"))
  expect_setequal(result$def$required, c("base.metadata.header", "options.o1.label"))
})

test_that("a template that seals nothing carries no sealed key at all", {
  result <- four_state_resolve(four_state_child())

  expect_false("sealed" %in% names(result$def))
  expect_false("required" %in% names(result$def))
})

test_that("a duplicate dataset key is rejected rather than silently appended", {
  # SEAL BYPASS, pinned. `datasets:` matched each child entry to at most one
  # parent entry and appended the rest unconditionally, so a child could write
  # the SAME key twice: the first entry left the sealed value untouched and the
  # second smuggled the value it actually wanted. dta_template_path_get() takes
  # the first match, so the seal compared equal and passed while the merged
  # definition carried both entries.
  parent <- four_state_parent()
  parent$datasets <- list(list(template = "gf@1.0", options = list(retention = 30)))
  parent$sealed <- "datasets.gf.options.retention"
  child <- four_state_child(datasets = list(
    list(template = "gf@1.0"),
    list(template = "gf@1.0", options = list(retention = 999))
  ))

  expect_error_message_contains(
    app_fn("resolve_template_inheritance")(child, make_resolver(list(parent = parent))),
    "duplicated"
  )
})

test_that("a duplicate option id is rejected too", {
  parent <- four_state_parent()
  child <- four_state_child(options = list(
    list(id = "o2", label = "first"),
    list(id = "o2", label = "second")
  ))

  expect_error_message_contains(
    app_fn("resolve_template_inheritance")(child, make_resolver(list(parent = parent))),
    "duplicated"
  )
})

test_that("entries with no identity key are left alone by the duplicate check", {
  # A `datasets:` entry may legitimately be unkeyable, and several of them are
  # not duplicates of one another -- they are simply unaddressable, which the
  # merge already tolerates.
  fn <- app_fn("dta_template_assert_unique_keys")
  key_of <- app_fn("dta_template_dataset_key")
  items <- list(list(note = "one"), list(note = "two"))

  expect_length(fn(items, "datasets", key_of), 2)
})

test_that("a required list field is unfilled when everything inside it is blank", {
  # `supplier`/`receiver`/`transmission` are lists, so a bare length check
  # accepted `supplier: {affiliation: ""}` as a real answer: present,
  # structurally non-empty, carrying no information. The vignette's headline
  # example is `required: [base.metadata.supplier]`, so this was the case the
  # feature was advertised with.
  fn <- app_fn("dta_template_path_is_filled")

  expect_false(fn(list(affiliation = "")))
  expect_false(fn(list(affiliation = list(name = ""), contacts = list())))
  expect_true(fn(list(affiliation = list(name = "ACME"))))
  expect_false(fn(list()))
  expect_false(fn(""))
  expect_true(fn("ACME"))
  # A logical or numeric value is a real answer, including the falsy ones.
  expect_true(fn(FALSE))
  expect_true(fn(0))
})
