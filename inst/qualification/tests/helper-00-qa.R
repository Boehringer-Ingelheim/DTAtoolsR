# Evidence helpers for the qualification suite.
#
# A testthat expectation normally records only whether something held. That is
# the right trade for a developer suite and the wrong one for an audit, where
# the reviewer's question is not "did it pass" but "what did you expect, what
# did you get, and where is the code that decided". `qa_step()` answers all
# three by putting the expected and observed values into the expectation's own
# message, which testthat then stores with the source reference of the calling
# line.
#
# Everything here is deliberately small and dependency-free. These files are
# installed with the package and run on machines that may have nothing but R,
# testthat and the package's own imports.
#
# The name is numbered because testthat sources helper files in alphabetical
# order, and every other helper in this suite may use what is defined here --
# at load time, not only inside a test. Left as `helper-qa.R` it sorted after
# `helper-generators.R` and `helper-parity.R`, and a single load-time call to
# `qa_tempdir()` in one of them aborted every stage of the suite at once.

# ---- run context ------------------------------------------------------------

qa_config <- function() {
  getOption("DTAtools.qualification") %||% list(
    run_id = "adhoc", bundle_dir = NULL, scale = "quick",
    root = NULL, seed = 20260101L, log = NULL, tester = "unknown",
    perf_floor = NULL
  )
}

qa_scale <- function() qa_config()$scale %||% "quick"

qa_seed <- function() qa_config()$seed %||% 20260101L

qa_bundle_dir <- function() qa_config()$bundle_dir

# The suite root is where the requirements and the deviation register live.
# Under a real run the runner passes it; when a file is sourced by hand during
# development the tests directory's parent is the same place.
qa_suite_root <- function() {
  root <- qa_config()$root
  if (!is.null(root) && dir.exists(root)) {
    return(root)
  }
  candidate <- normalizePath("..", winslash = "/", mustWork = FALSE)
  if (file.exists(file.path(candidate, "deviations.yaml"))) {
    return(candidate)
  }
  system.file("qualification", package = "DTAtools")
}

# Row counts per tier. The tiers exist because a qualification run is
# deliberate and infrequent while a development loop is neither; the numbers
# are the same shape of work at three different sizes, never different work.
qa_tier_rows <- function(scale = qa_scale()) {
  switch(scale,
    quick = 1e4L,
    standard = c(1e4L, 1e5L, 1e6L),
    full = c(1e4L, 1e5L, 1e6L, 1e7L),
    1e4L
  )
}

qa_parity_seeds <- function(scale = qa_scale()) {
  switch(scale,
    quick = 25L,
    standard = 200L,
    full = 1000L,
    25L
  )
}

# ---- recording a step -------------------------------------------------------

qa_format_value <- function(x, limit = 300L) {
  text <- if (is.data.frame(x)) {
    sprintf("%dx%d data.frame", nrow(x), ncol(x))
  } else if (is.null(x)) {
    "NULL"
  } else if (is.atomic(x)) {
    paste(format(x), collapse = ", ")
  } else {
    paste(utils::capture.output(utils::str(x, max.level = 1)), collapse = "; ")
  }
  if (nchar(text) > limit) paste0(substr(text, 1, limit - 3), "...") else text
}

#' Record one expected-versus-actual comparison as audit evidence.
#'
#' Emits exactly one testthat expectation whose message carries the
#' description, the expected value and the observed value, so the evidence
#' files can show all three without the reviewer having to read the test code.
#' Never throws: a failing step is recorded and the test continues, because a
#' qualification run should report every deviation it finds rather than the
#' first.
qa_step <- function(description, expected, actual,
                    ok = identical(expected, actual), tolerance = NULL) {
  if (!is.null(tolerance)) {
    ok <- isTRUE(all.equal(expected, actual, tolerance = tolerance))
  }
  message <- sprintf(
    "%s | expected: %s | actual: %s",
    description, qa_format_value(expected), qa_format_value(actual)
  )
  if (isTRUE(ok)) {
    testthat::succeed(message)
  } else {
    testthat::fail(message)
  }
  invisible(isTRUE(ok))
}

#' Record a boolean check that has no natural expected value.
qa_check <- function(description, ok, detail = NULL) {
  message <- paste0(
    description, " | expected: TRUE | actual: ", isTRUE(ok),
    if (!is.null(detail)) paste0(" | ", qa_format_value(detail)) else ""
  )
  if (isTRUE(ok)) testthat::succeed(message) else testthat::fail(message)
  invisible(isTRUE(ok))
}

# `pending` is accepted as well as `open`: an area's tests and the promotion of
# the register entry they bind land in the same change, and which of the two
# the reader writes first should not matter. The meta check still requires
# every `open` entry to be bound, so a promoted entry without a test is caught.
qa_open_deviations <- function() {
  path <- file.path(qa_suite_root(), "deviations.yaml")
  if (!file.exists(path)) {
    return(character(0))
  }
  doc <- yaml::read_yaml(path)
  ids <- vapply(doc$deviations %||% list(), function(d) {
    if ((d$status %||% "open") %in% c("open", "pending")) {
      d$id %||% NA_character_
    } else {
      NA_character_
    }
  }, character(1))
  ids[!is.na(ids)]
}

#' Bind a registered defect to this run.
#'
#' `expr` must evaluate to `TRUE` while the defect is still observable. The
#' test asserts the defect, not the correct behaviour, and passing means the
#' register still describes the software. When the defect stops reproducing the
#' step FAILS: a register entry that no longer matches reality is worse than no
#' entry, because it tells a reader the software is broken in a way it is not.
qa_known_deviation <- function(id, expr) {
  open <- qa_open_deviations()
  if (!id %in% open) {
    cli::cli_abort(c(
      "{.val {id}} is not an open deviation.",
      i = "Open deviations: {.val {open}}.",
      i = "Register it in {.path inst/qualification/deviations.yaml} first."
    ))
  }
  label <- paste(deparse(substitute(expr)), collapse = " ")
  reproduced <- tryCatch(isTRUE(expr), error = function(e) {
    structure(FALSE, reason = conditionMessage(e))
  })
  if (isTRUE(reproduced)) {
    testthat::succeed(sprintf("%s reproduced: %s", id, qa_format_value(label, 200)))
  } else {
    testthat::fail(sprintf(
      "%s no longer reproduces (%s); close it in deviations.yaml and promote the test to assert the correct behaviour.",
      id, attr(reproduced, "reason") %||% "the condition was FALSE"
    ))
  }
  invisible(isTRUE(reproduced))
}

# ---- skipping, with a reason the report can print ---------------------------

qa_requires <- function(...) {
  for (pkg in c(...)) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      testthat::skip(paste0("dependency absent: ", pkg))
    }
  }
  invisible(TRUE)
}

qa_skip_unless_scale <- function(tier) {
  order <- c(quick = 1L, standard = 2L, full = 3L)
  if (order[[qa_scale()]] < order[[tier]]) {
    testthat::skip(paste0("scale tier: ", tier, " not selected"))
  }
  invisible(TRUE)
}

qa_skip_unless_memory_gb <- function(gb) {
  if (!requireNamespace("ps", quietly = TRUE)) {
    return(invisible(TRUE))
  }
  free <- tryCatch(ps::ps_system_memory()$avail / 1024^3, error = function(e) NA_real_)
  if (!is.na(free) && free < gb) {
    testthat::skip(sprintf("insufficient memory: %.1f GB free, %g GB required", free, gb))
  }
  invisible(TRUE)
}

qa_skip_unless_browser <- function() {
  qa_requires("shinytest2", "chromote")
  found <- tryCatch(chromote::find_chrome(), error = function(e) NA_character_)
  if (length(found) != 1 || is.na(found) || !nzchar(found)) {
    testthat::skip("browser absent")
  }
  invisible(found)
}

# ---- files ------------------------------------------------------------------

# Everything a test writes goes here. On a target system the working directory
# is inside the package library, which is read-only and must stay that way.
qa_tempdir <- function(env = parent.frame()) {
  # Named with tempfile(), which is unique per process, and NOT from the random
  # stream: the suite seeds that stream deliberately so a randomised failure can
  # be reproduced, which means two stages of one run would otherwise draw the
  # same "random" name and the second would work in a directory the first had
  # already scheduled for deletion.
  dir <- tempfile("qa-")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  withr::defer(unlink(dir, recursive = TRUE, force = TRUE), envir = env)
  normalizePath(dir, winslash = "/", mustWork = TRUE)
}

qa_artifact_dir <- function(tc_id, env = parent.frame()) {
  bundle <- qa_bundle_dir()
  if (is.null(bundle)) {
    return(qa_tempdir(env))
  }
  dir <- file.path(bundle, "artifacts", tc_id)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}

qa_hash_file <- function(path) {
  fun <- if (exists("sha256sum", asNamespace("tools"))) {
    get("sha256sum", asNamespace("tools"))
  } else {
    tools::md5sum
  }
  unname(fun(path))
}

# ---- performance ------------------------------------------------------------

# Appended row by row rather than collected and written at the end, so a run
# killed during the largest tier still leaves the measurements it already made.
# `rows` is the count the caller wrote, not the count the engine reported.
# check() on a transfer object passes rows = NA_real_ to its own benchmark
# deliberately (R/DTA-class.R: there is no cheap row total across every dataset
# at that level), so the engine's rows_per_sec is NA for every measurement this
# suite takes -- see LIM-009. The generator knows exactly how many rows it
# wrote, so the throughput is derived from that instead, and
# `throughput_source` records which of the two the figure came from so a
# reviewer is never left guessing.
qa_perf <- function(tc_id, tier, rows, path, metrics) {
  bundle <- qa_bundle_dir()
  if (is.null(bundle) || is.null(metrics) || nrow(metrics) == 0) {
    return(invisible(NULL))
  }
  engine_rate <- suppressWarnings(as.numeric(metrics$rows_per_sec[[1]]))
  elapsed <- suppressWarnings(as.numeric(metrics$elapsed_sec[[1]]))
  source <- "engine"
  if (!is.finite(engine_rate) && is.finite(elapsed) && elapsed > 0 &&
    is.finite(rows) && rows > 0) {
    engine_rate <- rows / elapsed
    source <- "suite"
  }
  metrics$rows_per_sec <- engine_rate
  record <- cbind(
    data.frame(
      tc_id = tc_id, tier = tier, n_rows = rows, path = path,
      throughput_source = source,
      stringsAsFactors = FALSE
    ),
    metrics
  )
  file <- file.path(bundle, "results", "performance.csv")
  # The runner creates this directory before the stages start, but the helper
  # does not rely on that: an ordering it depends on and does not state is one
  # a later change can break silently, and the symptom would be a performance
  # stage that errors on a connection rather than one that says what is wrong.
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  utils::write.table(
    record, file,
    sep = ",", row.names = FALSE, col.names = !file.exists(file),
    append = file.exists(file), qmethod = "double"
  )
  invisible(record)
}

# The rows this run has recorded so far, so a test can assert on the evidence
# that will reach the report rather than on the value it just computed. Reading
# the file back is the point: a figure that never made it to disk is not
# evidence, however correct it was in memory.
qa_perf_recorded <- function(tc_id = NULL) {
  bundle <- qa_bundle_dir()
  empty <- data.frame(
    tc_id = character(0), rows_per_sec = numeric(0),
    throughput_source = character(0), stringsAsFactors = FALSE
  )
  if (is.null(bundle)) {
    return(empty)
  }
  file <- file.path(bundle, "results", "performance.csv")
  if (!file.exists(file)) {
    return(empty)
  }
  df <- utils::read.csv(file, stringsAsFactors = FALSE)
  if (!is.null(tc_id)) {
    df <- df[df$tc_id %in% tc_id, , drop = FALSE]
  }
  df
}

# ---- subprocesses -----------------------------------------------------------

# A separate R process is the only way to test something the current session
# has already decided: the locale it started in, the thread count Arrow
# initialised with, a fresh library search path. `callr` would be neater but is
# not a dependency, and `Rscript` is guaranteed to be here.
qa_subprocess <- function(code, envvars = character(), timeout_sec = 1800) {
  script <- tempfile(fileext = ".R")
  result_file <- tempfile(fileext = ".rds")
  on.exit(unlink(c(script, result_file)), add = TRUE)

  writeLines(
    c(
      sprintf("value <- local(%s)", paste(deparse(substitute(code)), collapse = "\n")),
      sprintf("saveRDS(value, %s)", encodeString(result_file, quote = '"'))
    ),
    script
  )
  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  status <- withr::with_envvar(
    envvars,
    system2(
      rscript,
      c("--vanilla", "--no-init-file", shQuote(normalizePath(script, winslash = "/"))),
      stdout = TRUE, stderr = TRUE, timeout = timeout_sec
    )
  )
  if (!file.exists(result_file)) {
    stop(paste(c("The subprocess produced no result:", status), collapse = "\n"))
  }
  readRDS(result_file)
}

# ---- comparing validation outcomes ------------------------------------------

# `messages()` carries a sequential id that depends on how many messages came
# before it, and row order that depends on which engine produced it. Neither is
# a fact about the data, so both are removed before two runs are compared.
qa_messages_norm <- function(x, ...) {
  msgs <- as.data.frame(messages(x, ...))
  keep <- intersect(
    c("dataset", "target", "source", "rule_id", "column", "keyword", "row", "message"),
    names(msgs)
  )
  msgs <- msgs[, keep, drop = FALSE]
  if (nrow(msgs) == 0) {
    return(msgs)
  }
  msgs <- msgs[do.call(order, lapply(msgs, as.character)), , drop = FALSE]
  rownames(msgs) <- NULL
  msgs
}

qa_counts <- function(x, ...) {
  msgs <- qa_messages_norm(x, ...)
  if (nrow(msgs) == 0) {
    return(data.frame(
      source = character(0), target = character(0), keyword = character(0),
      n = integer(0), stringsAsFactors = FALSE
    ))
  }
  key <- data.frame(
    source = as.character(msgs$source),
    target = as.character(msgs$target),
    keyword = as.character(msgs$keyword %||% NA_character_),
    stringsAsFactors = FALSE
  )
  key$keyword[is.na(key$keyword)] <- ""
  out <- stats::aggregate(list(n = rep(1L, nrow(key))), by = key, FUN = sum)
  out <- out[order(out$source, out$target, out$keyword), , drop = FALSE]
  rownames(out) <- NULL
  out
}
