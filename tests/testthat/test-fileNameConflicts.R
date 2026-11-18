# D16 / section 4.14 of the container-manager plan. Expected values come from
# the plan, not from observed output.

# ---- dta_pattern_examples() -----------------------------------------------------

test_that("dta_pattern_examples() gives the examples of the supported subset", {
  expect_identical(dta_pattern_examples("^ae[.]csv$"), "ae.csv")
  ex <- dta_pattern_examples("ae.*\\.xpt")
  expect_true(all(c("ae.xpt", "aea.xpt") %in% ex))
  expect_identical(dta_pattern_examples("^(ae|dm)\\.csv$"), c("ae.csv", "dm.csv"))
  expect_identical(dta_pattern_examples("^lb\\d{2}\\.xpt$"), "lb00.xpt")
  expect_true("a.csv" %in% dta_pattern_examples("[^.]+\\.csv"))
})

test_that("dta_pattern_examples() gives nothing for constructs outside the subset", {
  expect_identical(dta_pattern_examples("(?i)ae.csv"), character(0))
  expect_identical(dta_pattern_examples("^a(?!b).*"), character(0))
  expect_identical(dta_pattern_examples("^(a)\\1$"), character(0))
  expect_identical(dta_pattern_examples("^a++$"), character(0))
  expect_identical(dta_pattern_examples("^\\p{L}+$"), character(0))
})

test_that("dta_pattern_examples() orders shortest first and honours the limit", {
  ex <- dta_pattern_examples("^a{1,5}$", limit = 1L)
  expect_identical(ex, "a")
  ex <- dta_pattern_examples("^(a|bb|ccc)$")
  expect_identical(ex, c("a", "bb", "ccc"))
})

# ---- file_name_conflicts() ----------------------------------------------------------

conflict_cols <- c(
  "dataset_a", "entry_a", "expected_a", "dataset_b", "entry_b", "expected_b", "example"
)

# One dataset per element of `spec`; each element is list(id, files = list(entry
# lists with type, filename, pattern, container, extensions)).
cfn_dta <- function(spec, containers = list()) {
  mk <- function(d) {
    files <- lapply(d$files, function(f) {
      if (is.null(f[["pattern"]])) f[["pattern"]] <- TRUE
      do.call(DTAtools::DTAFileFactory, f)
    })
    if (identical(d[["type"]], "file")) {
      return(DTAtools::DTADataSetFile(id = d$id, files = files))
    }
    DTAtools::DTADataSetTabular(
      id = d$id, specs = DTAtools::create_example_DTAColumnSpecCollection(1), files = files
    )
  }
  DTAtools::DTA(
    datasets = lapply(spec, mk), metadata = DTAtools::create_example_DTAMetaData(),
    containers = containers
  )
}
entry <- function(type, filename, ...) list(type = type, filename = filename, ...)
lb_pair <- function(...) {
  list(
    list(id = "lb", files = list(entry("xpt", "^lb.*\\.xpt$", ...))),
    list(id = "lb_supp", files = list(entry("xpt", "^lb_supp.*\\.xpt$", ...)))
  )
}

test_that("a document without conflicts gives a zero-row frame with the exact columns", {
  out <- file_name_conflicts(cfn_dta(list(
    list(id = "ae", files = list(entry("csv", "^ae[.]csv$"))),
    list(id = "dm", files = list(entry("csv", "^dm[.]csv$")))
  )))
  expect_identical(names(out), conflict_cols)
  expect_identical(nrow(out), 0L)
  expect_type(out$entry_a, "integer")
})

test_that("a non-DTA is refused", {
  expect_error(file_name_conflicts(list()), "must be a")
})

test_that("lb and lb_supp conflict on the shortest example both take", {
  out <- file_name_conflicts(cfn_dta(lb_pair()))
  expect_identical(names(out), conflict_cols)
  expect_identical(nrow(out), 1L)
  expect_identical(out$dataset_a, "lb")
  expect_identical(out$entry_a, 1L)
  expect_identical(out$expected_a, "^lb.*\\.xpt$")
  expect_identical(out$dataset_b, "lb_supp")
  expect_identical(out$entry_b, 1L)
  expect_identical(out$example, "lb_supp.xpt")
})

test_that("a free literal in a third dataset shadows its own name only", {
  spec <- c(lb_pair(), list(list(
    id = "other", files = list(entry("xpt", "lb_supp.xpt", pattern = FALSE))
  )))
  out <- file_name_conflicts(cfn_dta(spec))
  expect_identical(nrow(out), 1L)
  expect_identical(out$example, "lb_suppa.xpt")
})

test_that("two entries that take a name only together overlap on it", {
  out <- file_name_conflicts(cfn_dta(list(
    list(id = "ae", files = list(entry("csv", "^a.*[.]csv$"))),
    list(id = "dm", files = list(entry("csv", "^.*e[.]csv$")))
  )))
  expect_identical(nrow(out), 1L)
  expect_identical(out$example, "ae.csv")
})

test_that("literal entries are never part of a conflict", {
  same <- cfn_dta(list(
    list(id = "ae", files = list(entry("csv", "ae.csv", pattern = FALSE))),
    list(id = "dm", files = list(entry("csv", "ae.csv", pattern = FALSE)))
  ))
  expect_identical(nrow(file_name_conflicts(same)), 0L)
  vs_pattern <- cfn_dta(list(
    list(id = "ae", files = list(entry("csv", "ae.csv", pattern = FALSE))),
    list(id = "dm", files = list(entry("csv", ".*\\.csv")))
  ))
  expect_identical(nrow(file_name_conflicts(vs_pattern)), 0L)
})

test_that("entries of one dataset never conflict with each other", {
  out <- file_name_conflicts(cfn_dta(list(
    list(id = "lb", files = list(entry("xpt", "^lb.*\\.xpt$"), entry("xpt", "^lb_supp.*\\.xpt$")))
  )))
  expect_identical(nrow(out), 0L)
})

test_that("containers decide whether two entries can tie", {
  with_containers <- function(glob_a, glob_b, containers) {
    spec <- lb_pair()
    spec[[1]]$files[[1]]$container <- glob_a
    spec[[2]]$files[[1]]$container <- glob_b
    file_name_conflicts(cfn_dta(spec, containers))
  }
  expect_identical(nrow(with_containers("sdtm.zip", "adam.zip", list())), 0L)
  hit <- with_containers("STUDY_*.zip", "STUDY_SDTM_*.zip", list())
  expect_identical(nrow(hit), 1L)
  expect_identical(hit$example, "lb_supp.xpt")
  expect_identical(nrow(with_containers("same.zip", "same.zip", list())), 1L)
  expect_identical(nrow(with_containers("sdtm.zip", NULL, list())), 0L)
  expect_identical(nrow(with_containers(NULL, "sdtm.zip", list())), 0L)
  # references by id resolve to the container's file name
  by_id <- with_containers(
    "a", "b", list(DTAtools::DTAContainer("a", "STUDY_*.zip"), DTAtools::DTAContainer("b", "STUDY_SDTM_*.zip"))
  )
  expect_identical(nrow(by_id), 1L)
})

test_that("two entries under the default container conflict; 'none' leaves it", {
  default <- list(DTAtools::DTAContainer("d", "delivery.zip", default = TRUE))
  expect_identical(nrow(file_name_conflicts(cfn_dta(lb_pair(), default))), 1L)
  spec <- lb_pair()
  spec[[2]]$files[[1]]$container <- "none"
  expect_identical(nrow(file_name_conflicts(cfn_dta(spec, default))), 0L)
})

test_that("containers that do not overlap stay apart although a third one links the scope", {
  # sdtm_*.zip and adam_*.zip share no archive; *.zip overlaps both, so all three
  # form one scope, but
  # ties are not transitive.
  spec <- lb_pair()
  spec[[1]]$files[[1]]$container <- "sdtm_*.zip"
  spec[[2]]$files[[1]]$container <- "adam_*.zip"
  spec[[3]] <- list(id = "x", files = list(entry("csv", "^zzz$", container = "*.zip")))
  expect_identical(nrow(file_name_conflicts(cfn_dta(spec))), 0L)
})

test_that("extension-restricted any entries conflict on the name with the extension", {
  spec <- list(
    list(id = "a", type = "file", files = list(entry("any", "^define", extensions = "xml"))),
    list(id = "b", type = "file", files = list(entry("any", "^define", extensions = "xml")))
  )
  out <- file_name_conflicts(cfn_dta(spec))
  expect_identical(nrow(out), 1L)
  expect_identical(out$example, "define.xml")
})

test_that("an overlap that only a look-around hides is not found", {
  spec <- list(
    list(id = "a", files = list(entry("csv", "^a(?!b).*[.]csv$"))),
    list(id = "b", files = list(entry("csv", "^.*b[.]csv$")))
  )
  expect_identical(nrow(file_name_conflicts(cfn_dta(spec))), 0L)
})

test_that("rows are radix-sorted and carry the pattern description when there is one", {
  spec <- list(
    list(id = "z", files = list(entry("csv", "^q.*[.]csv$"))),
    list(id = "m", files = list(entry("csv", "^q.*[.]csv$", pattern_description = "q files"))),
    list(id = "a", files = list(entry("csv", "^q.*[.]csv$")))
  )
  out <- file_name_conflicts(cfn_dta(spec))
  expect_identical(nrow(out), 3L)
  expect_identical(out$dataset_a, c("a", "a", "m"))
  expect_identical(out$dataset_b, c("m", "z", "z"))
  expect_identical(out$expected_b[[1]], "q files")
  expect_identical(out$example, rep("q.csv", 3))
})

test_that("sixty pattern entries in one scope are checked in under two seconds", {
  spec <- lapply(1:60, function(i) {
    list(id = sprintf("d%02d", i), files = list(entry("csv", sprintf("^p%02d_.*[.]csv$", i))))
  })
  dta <- cfn_dta(spec)
  elapsed <- system.time(out <- file_name_conflicts(dta))[["elapsed"]]
  expect_identical(nrow(out), 0L)
  expect_lt(elapsed, 2)
})

test_that("the transitivity fixture really merges the three globs into one scope", {
  expect_true(dta_globs_overlap("sdtm_*.zip", "*.zip"))
  expect_true(dta_globs_overlap("adam_*.zip", "*.zip"))
  expect_false(dta_globs_overlap("sdtm_*.zip", "adam_*.zip"))
})

test_that("sixty entries that all overlap are checked in under two seconds", {
  spec <- lapply(1:60, function(i) {
    list(id = sprintf("d%02d", i), files = list(entry("csv", "^.*[.]csv$")))
  })
  elapsed <- system.time(out <- file_name_conflicts(cfn_dta(spec)))[["elapsed"]]
  expect_identical(nrow(out), as.integer(choose(60, 2)))
  expect_true(all(out$example == ".csv")) # `*` takes its lower count 0 first
  expect_lt(elapsed, 2)
})

test_that("nested repeats do not blow up the example generation", {
  for (p in c("^((x|y){50}){50}$", "^(((x|y){10}){10}){10}$")) {
    elapsed <- system.time(ex <- dta_pattern_examples(p))[["elapsed"]]
    expect_lt(elapsed, 1)
    expect_true(all(stringr::str_detect(ex, p)))
    expect_true(all(nchar(ex) <= 255L))
  }
})

test_that("a bounded repeat with a large upper bound still gives examples", {
  # [A-Za-z0-9_] -> first member "A" (4.14); count lo = 1 -> "A"
  expect_identical(dta_pattern_examples("^[A-Za-z0-9_]{1,64}[.]csv$", limit = 1L), "A.csv")
})
