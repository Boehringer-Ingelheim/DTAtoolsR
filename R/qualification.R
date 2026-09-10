# Qualification runner.
#
# The package's own test suite lives in `tests/`, which is not installed, is
# not traced to requirements, and records no expected-versus-actual evidence.
# None of that is a criticism of it: it is a developer suite and it is good at
# being one. It just cannot answer the question a pharmaceutical quality unit
# asks before software is allowed near a clinical transfer -- does the copy
# installed *on this machine* do what its specification says?
#
# This file answers that question by running a second suite, shipped under
# `inst/qualification/` and therefore installed with the package, against the
# installed namespace, and by writing down everything needed to re-check the
# answer without trusting the tool that produced it: what ran, where its code
# sits (file and line), what was expected, what happened, in what environment,
# and a hash of every file so that tampering is visible.
#
# The vocabulary here is deliberately "qualification", never "validation".
# Validation is this package's domain word -- checking a data transfer against
# its specification -- and reusing it for the software lifecycle would make
# both meanings unreadable. Only formal document titles keep the regulatory
# spelling.

# `self` is bound by R6 inside the reporter's methods, which R CMD check has no
# way to see. Declared here so the note it would otherwise raise cannot mask a
# genuinely undefined variable in this file later.
utils::globalVariables("self")

# ---- vocabulary -------------------------------------------------------------

qual_stages <- function() c("IQ", "OQ", "PQ")

qual_scales <- function() c("full", "standard", "quick")

qual_formats <- function() c("md", "html", "docx", "pdf")

# The grammar every qualification test title must satisfy. It carries the test
# case id, a human title, and the requirements the case verifies, so the
# traceability matrix is derived from the tests themselves rather than
# maintained beside them, where it would rot.
qual_title_pattern <- function() {
  paste0(
    "^(IQ|OQ|PQ)-([A-Z]{2,8})-([0-9]{3})",
    " \\| ([^|]+?)",
    " \\| (REQ-[A-Z]{2,8}-[0-9]{3}(?: REQ-[A-Z]{2,8}-[0-9]{3})*)",
    "(?: \\| tags: ([a-z-]+(?:,[a-z-]+)*))?$"
  )
}

qual_allowed_tags <- function() {
  c("white-box", "subprocess", "browser", "slow", "scale-standard", "scale-full")
}

qual_risks <- function() c("high", "medium", "low")

qual_categories <- function() {
  c(
    "functional", "interface", "installation", "performance",
    "integrity", "security", "documentation"
  )
}

# ---- run state --------------------------------------------------------------

# The runner reaches the test files through an option rather than through
# arguments, because testthat gives a test file no way to receive one. The
# `qa_*()` helpers in the suite read it; everything they need to know about the
# run -- where artefacts go, which scale tier, which seed -- is in here.
qual_config <- function() {
  getOption("DTAtools.qualification")
}

qual_root <- function(dir = NULL) {
  root <- dir %||% system.file("qualification", package = "DTAtools")
  if (!nzchar(root) || !dir.exists(root)) {
    cli::cli_abort(c(
      "The qualification suite is not available.",
      i = "Expected it at {.path {root}}.",
      i = "Reinstall {.pkg DTAtools}; the suite ships in {.path inst/qualification}."
    ))
  }
  root
}

qual_run_id <- function(version = utils::packageVersion("DTAtools"),
                        time = Sys.time()) {
  sprintf("QUAL-%s-%s", version, format(time, "%Y%m%d-%H%M%S", tz = "UTC"))
}

# ---- contemporaneous log ----------------------------------------------------

# Written as events happen and flushed on every line, so a run that dies
# half-way still leaves usable evidence of how far it got. That is what
# "contemporaneous" buys: a log assembled at the end is a reconstruction, and a
# reconstruction is what an auditor discounts.
qual_log_open <- function(path) {
  file(path, open = "at", encoding = "UTF-8")
}

qual_log <- function(event, ..., con = qual_config()$log) {
  # The log is deliberately closed before the hash manifest is computed, so
  # calls after that point are expected and must be silent rather than fatal.
  if (is.null(con) || !isTRUE(qual_safe(isOpen(con), FALSE))) {
    return(invisible(NULL))
  }
  fields <- vapply(
    list(...),
    function(x) gsub("[\t\r\n]", " ", paste(format(x), collapse = " ")),
    character(1)
  )
  line <- paste(
    c(format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z"), event, fields),
    collapse = "\t"
  )
  writeLines(line, con)
  flush(con)
  invisible(line)
}

# The reporter exists only to make the log live. `test_dir()` returns every
# expectation anyway, so nothing here is load-bearing for the results: if R6 is
# missing the run still produces a complete bundle, just without the
# blow-by-blow trace.
qual_reporter <- function(con) {
  if (is.null(con) || !requireNamespace("R6", quietly = TRUE)) {
    return("silent")
  }
  generator <- R6::R6Class(
    "QualReporter",
    inherit = testthat::Reporter,
    public = list(
      con = NULL,
      initialize = function(con = NULL, ...) {
        super$initialize(...)
        self$con <- con
      },
      start_file = function(filename) {
        qual_log("file_start", filename, con = self$con)
      },
      start_test = function(context, test) {
        qual_log("test_start", test %||% "(file level)", con = self$con)
      },
      add_result = function(context, test, result) {
        qual_log(
          "expectation",
          test %||% "(file level)",
          sub("^expectation_", "", class(result)[[1]]),
          substr(conditionMessage(result), 1, 500),
          con = self$con
        )
      },
      end_test = function(context, test) {
        qual_log("test_end", test %||% "(file level)", con = self$con)
      },
      end_file = function() {
        qual_log("file_end", "", con = self$con)
      }
    )
  )
  generator$new(con = con)
}

# ---- test discovery ---------------------------------------------------------

qual_parse_test_name <- function(name) {
  empty <- list(
    name = name, ok = FALSE, tc_id = NA_character_, stage = NA_character_,
    area = NA_character_, seq = NA_integer_, title = NA_character_,
    req_ids = character(0), tags = character(0)
  )
  if (length(name) != 1 || is.na(name)) {
    return(empty)
  }
  m <- regmatches(name, regexec(qual_title_pattern(), name, perl = TRUE))[[1]]
  if (length(m) == 0) {
    return(empty)
  }
  tags <- if (nzchar(m[[7]])) strsplit(m[[7]], ",", fixed = TRUE)[[1]] else character(0)
  list(
    name = name,
    ok = TRUE,
    tc_id = paste(m[[2]], m[[3]], m[[4]], sep = "-"),
    stage = m[[2]],
    area = m[[3]],
    seq = as.integer(m[[4]]),
    title = trimws(m[[5]]),
    req_ids = strsplit(m[[6]], " ", fixed = TRUE)[[1]],
    tags = tags
  )
}

qual_discovery_row <- function(file, line, name) {
  parsed <- qual_parse_test_name(name)
  data.frame(
    file = basename(file),
    line = line,
    name = name %||% NA_character_,
    ok = parsed$ok,
    tc_id = parsed$tc_id,
    stage = parsed$stage,
    area = parsed$area,
    seq = parsed$seq,
    title = parsed$title,
    req_ids = paste(parsed$req_ids, collapse = " "),
    tags = paste(parsed$tags, collapse = ","),
    stringsAsFactors = FALSE
  )
}

qual_discovery_empty <- function() {
  data.frame(
    file = character(0), line = integer(0), name = character(0),
    ok = logical(0), tc_id = character(0), stage = character(0),
    area = character(0), seq = integer(0), title = character(0),
    req_ids = character(0), tags = character(0), stringsAsFactors = FALSE
  )
}

# Discovery is static: the files are parsed, never sourced. A test case never
# reached because the file above it threw would otherwise be invisible, and
# "the suite passed" would quietly mean "the half of it that ran passed".
# Meta check M10 compares this list against what actually executed.
qual_discover_tests <- function(tests_dir) {
  files <- sort(list.files(tests_dir, pattern = "^test-.*\\.[Rr]$", full.names = TRUE))
  rows <- list()
  for (f in files) {
    exprs <- tryCatch(parse(f, keep.source = TRUE), error = function(e) NULL)
    if (is.null(exprs)) {
      rows[[length(rows) + 1]] <- qual_discovery_row(f, NA_integer_, NA_character_)
      next
    }
    refs <- utils::getSrcref(exprs)
    for (i in seq_along(exprs)) {
      call <- exprs[[i]]
      if (!is.call(call) || !identical(as.character(call[[1]])[[1]], "test_that")) {
        next
      }
      desc <- if (length(call) >= 2) call[[2]] else NULL
      line <- if (is.null(refs) || is.null(refs[[i]])) {
        NA_integer_
      } else {
        as.integer(refs[[i]])[[1]]
      }
      rows[[length(rows) + 1]] <- qual_discovery_row(
        f, line,
        if (is.character(desc) && length(desc) == 1) desc else NA_character_
      )
    }
  }
  if (length(rows) == 0) {
    return(qual_discovery_empty())
  }
  do.call(rbind, rows)
}

# ---- running a stage --------------------------------------------------------

qual_stage_filter <- function(stage, filter = NULL) {
  if (is.null(filter)) {
    return(paste0("^", stage, "-"))
  }
  sprintf("^%s-.*(%s)", stage, filter)
}

# `load_package = "none"` together with `package = "DTAtools"` runs the tests in
# a clone of the installed namespace: internals stay reachable for the few
# white-box cases, `library()` is not called a second time, and -- the reason it
# matters on a target system -- the tests see exactly the installed code rather
# than a source tree.
qual_run_stage <- function(stage, tests_dir, filter = NULL, reporter = "silent") {
  qual_log("stage_start", stage)
  results <- tryCatch(
    withr::with_envvar(
      c(TESTTHAT_PARALLEL = "FALSE", TESTTHAT_EDITION = "3"),
      testthat::test_dir(
        tests_dir,
        filter = qual_stage_filter(stage, filter),
        reporter = reporter,
        env = NULL,
        load_helpers = TRUE,
        stop_on_failure = FALSE,
        stop_on_warning = FALSE,
        package = "DTAtools",
        load_package = "none"
      )
    ),
    error = function(e) {
      # testthat aborts the whole stage for two very different reasons: a
      # filter that matches no file, which is merely an empty stage, and a
      # helper that fails to load, which silently removes every test the stage
      # had. Both arrive here as one error, and the second must never be
      # reported as a clean run of nothing -- so the abort is recorded as a
      # failing result rather than swallowed.
      qual_log("stage_error", stage, conditionMessage(e))
      structure(conditionMessage(e), class = "qual_stage_abort")
    }
  )
  if (inherits(results, "qual_stage_abort")) {
    qual_log("stage_end", stage, "aborted")
    return(results)
  }
  qual_log("stage_end", stage, length(results %||% list()))
  results
}

# A stage that could not run at all, rendered as one failing record so the
# verdict and the report both show it.
qual_stage_abort_row <- function(stage, message) {
  data.frame(
    stage = stage, file = NA_character_, tc_id = paste0(stage, " (stage)"),
    name = NA_character_, title = "the stage could not be executed",
    req_ids = "", tags = "", n_expectations = 1L, n_pass = 0L, n_fail = 0L,
    n_error = 1L, n_skip = 0L, n_warning = 0L, status = "file_error",
    skip_reason = NA_character_, real_sec = NA_real_,
    stringsAsFactors = FALSE
  )
}

# ---- turning testthat results into evidence ---------------------------------

# testthat reports a file that threw outside any test case with a NULL test
# name in the reporter and an NA one in the returned results. Both mean the
# same thing and both have to be recognised, or a broken file is silently
# counted as an ordinary test.
qual_is_file_level <- function(item) {
  test <- item$test
  is.null(test) || length(test) != 1 || is.na(test)
}

qual_expectation_type <- function(x) {
  cls <- class(x)[[1]]
  if (grepl("^expectation_", cls)) sub("^expectation_", "", cls) else cls
}

qual_srcref_file <- function(x) {
  ref <- x[["srcref"]]
  if (is.null(ref)) {
    return(NA_character_)
  }
  src <- attr(ref, "srcfile")
  if (is.null(src) || is.null(src$filename)) {
    return(NA_character_)
  }
  basename(src$filename)
}

qual_srcref_line <- function(x) {
  ref <- x[["srcref"]]
  if (is.null(ref)) {
    return(NA_integer_)
  }
  as.integer(ref)[[1]]
}

qual_tests_empty <- function() {
  data.frame(
    stage = character(0), file = character(0), tc_id = character(0),
    name = character(0), title = character(0), req_ids = character(0),
    tags = character(0), n_expectations = integer(0), n_pass = integer(0),
    n_fail = integer(0), n_error = integer(0), n_skip = integer(0),
    n_warning = integer(0), status = character(0), skip_reason = character(0),
    real_sec = numeric(0), stringsAsFactors = FALSE
  )
}

qual_tests_df <- function(results, stage) {
  if (is.null(results) || length(results) == 0) {
    return(qual_tests_empty())
  }
  rows <- lapply(results, function(item) {
    exps <- item$results %||% list()
    types <- vapply(exps, qual_expectation_type, character(1))
    parsed <- qual_parse_test_name(item$test %||% NA_character_)

    # A file that throws outside `test_that()` is reported by testthat with a
    # NULL test name. Recording it as its own failing row is what stops a
    # broken file from silently removing its test cases from the suite.
    file_level <- qual_is_file_level(item)
    status <- if (file_level || any(types == "error")) {
      if (file_level) "file_error" else "error"
    } else if (any(types == "failure")) {
      "fail"
    } else if (any(types == "skip")) {
      "skip"
    } else {
      "pass"
    }
    skip_reason <- if (any(types == "skip")) {
      sub("^Reason: ", "", conditionMessage(exps[[which(types == "skip")[[1]]]]))
    } else {
      NA_character_
    }
    data.frame(
      stage = stage,
      file = basename(item$file %||% NA_character_),
      tc_id = if (file_level) {
        basename(item$file %||% NA_character_)
      } else {
        parsed$tc_id %||% NA_character_
      },
      name = item$test %||% NA_character_,
      title = parsed$title %||% NA_character_,
      req_ids = paste(parsed$req_ids, collapse = " "),
      tags = paste(parsed$tags, collapse = ","),
      n_expectations = length(exps),
      n_pass = sum(types == "success"),
      n_fail = sum(types == "failure"),
      n_error = sum(types == "error"),
      n_skip = sum(types == "skip"),
      n_warning = sum(types == "warning"),
      status = status,
      skip_reason = skip_reason,
      real_sec = as.numeric(item$real %||% NA_real_),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

qual_expectations_df <- function(results, stage) {
  empty <- data.frame(
    stage = character(0), tc_id = character(0), seq = integer(0),
    type = character(0), message = character(0), file = character(0),
    line = integer(0), stringsAsFactors = FALSE
  )
  if (is.null(results) || length(results) == 0) {
    return(empty)
  }
  rows <- list()
  for (item in results) {
    parsed <- qual_parse_test_name(item$test %||% NA_character_)
    tc_id <- if (qual_is_file_level(item)) {
      basename(item$file %||% NA_character_)
    } else {
      parsed$tc_id %||% NA_character_
    }
    exps <- item$results %||% list()
    for (i in seq_along(exps)) {
      rows[[length(rows) + 1]] <- data.frame(
        stage = stage,
        tc_id = tc_id,
        seq = i,
        type = qual_expectation_type(exps[[i]]),
        message = gsub("[\r\n]+", " ", conditionMessage(exps[[i]])),
        file = qual_srcref_file(exps[[i]]),
        line = qual_srcref_line(exps[[i]]),
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0) {
    return(empty)
  }
  do.call(rbind, rows)
}

# ---- requirements and deviations --------------------------------------------

qual_yaml_files <- function(root) {
  sort(list.files(
    file.path(root, "requirements"),
    pattern = "^REQ-[A-Z]+\\.ya?ml$", full.names = TRUE
  ))
}

qual_req_empty <- function() {
  data.frame(
    id = character(0), area = character(0), area_title = character(0),
    text = character(0), risk = character(0), category = character(0),
    covers = I(list()), source = character(0), file = character(0),
    stringsAsFactors = FALSE
  )
}

# The schema is enforced here rather than in a separate linter because a
# requirement file that does not parse must stop a run: an unreadable
# requirement is an untested requirement, and the traceability matrix would
# otherwise report full coverage of a set it silently truncated.
qual_requirements_df <- function(root, api = NULL) {
  files <- qual_yaml_files(root)
  problems <- character(0)
  rows <- list()

  for (f in files) {
    doc <- tryCatch(yaml::read_yaml(f), error = function(e) {
      problems <<- c(problems, sprintf("%s: not valid YAML (%s)", basename(f), conditionMessage(e)))
      NULL
    })
    if (is.null(doc)) next

    area <- doc$area %||% NA_character_
    stem <- sub("^REQ-", "", sub("\\.ya?ml$", "", basename(f)))
    if (!identical(area, stem)) {
      problems <- c(problems, sprintf(
        "%s: 'area' is %s but the file name says %s", basename(f), area %||% "absent", stem
      ))
    }
    for (req in doc$requirements %||% list()) {
      id <- req$id %||% NA_character_
      bad <- character(0)
      if (is.na(id) || !grepl("^REQ-[A-Z]{2,8}-[0-9]{3}$", id)) {
        bad <- c(bad, "id is missing or malformed")
      }
      if (!is.na(id) && !is.na(area) && !startsWith(id, paste0("REQ-", area, "-"))) {
        bad <- c(bad, "id does not carry the file's area")
      }
      if (!length(req$text %||% NULL) || !nzchar(req$text %||% "")) {
        bad <- c(bad, "text is empty")
      }
      if (!isTRUE(req$risk %in% qual_risks())) {
        bad <- c(bad, sprintf("risk must be one of %s", paste(qual_risks(), collapse = "/")))
      }
      if (!isTRUE(req$category %in% qual_categories())) {
        bad <- c(bad, "category is not one of the permitted values")
      }
      covers <- unlist(req$covers %||% character(0), use.names = FALSE)
      if (length(covers) == 0) {
        bad <- c(bad, "covers must name at least one exported symbol")
      }
      if (length(bad)) {
        problems <- c(problems, sprintf("%s [%s]: %s", basename(f), id, paste(bad, collapse = "; ")))
        next
      }
      rows[[length(rows) + 1]] <- data.frame(
        id = id, area = area, area_title = doc$title %||% NA_character_,
        text = req$text, risk = req$risk, category = req$category,
        covers = I(list(covers)), source = req$source %||% NA_character_,
        file = basename(f), stringsAsFactors = FALSE
      )
    }
  }

  if (length(problems)) {
    cli::cli_abort(c(
      "The qualification requirements do not satisfy their schema.",
      stats::setNames(problems, rep("x", length(problems)))
    ))
  }
  if (length(rows) == 0) {
    return(qual_req_empty())
  }
  out <- do.call(rbind, rows)
  dup <- out$id[duplicated(out$id)]
  if (length(dup)) {
    cli::cli_abort("Duplicate requirement id{?s}: {.val {unique(dup)}}.")
  }
  out[order(out$id), , drop = FALSE]
}

qual_deviations_df <- function(root) {
  path <- file.path(root, "deviations.yaml")
  empty_dev <- data.frame(
    id = character(0), title = character(0), status = character(0),
    severity = character(0), affects = character(0), observed = character(0),
    expected = character(0), impact = character(0), workaround = character(0),
    reference = character(0), opened = character(0), stringsAsFactors = FALSE
  )
  empty_lim <- data.frame(
    id = character(0), title = character(0), affects = character(0),
    reference = character(0), consequence = character(0), stringsAsFactors = FALSE
  )
  if (!file.exists(path)) {
    return(list(deviations = empty_dev, limitations = empty_lim))
  }
  doc <- yaml::read_yaml(path)

  devs <- lapply(doc$deviations %||% list(), function(d) {
    data.frame(
      id = d$id %||% NA_character_,
      title = d$title %||% NA_character_,
      status = d$status %||% "open",
      severity = d$severity %||% NA_character_,
      affects = paste(unlist(d$affects %||% character(0)), collapse = " "),
      observed = d$observed %||% NA_character_,
      expected = d$expected %||% NA_character_,
      impact = d$impact %||% NA_character_,
      workaround = d$workaround %||% NA_character_,
      reference = d$reference %||% NA_character_,
      opened = as.character(d$opened %||% NA_character_),
      # A register entry that says "closed" without saying when is an
      # incomplete record: the date is what ties the closure to the release
      # that made it true.
      closed = as.character(d$closed %||% NA_character_),
      stringsAsFactors = FALSE
    )
  })
  lims <- lapply(doc$limitations %||% list(), function(d) {
    data.frame(
      id = d$id %||% NA_character_,
      title = d$title %||% NA_character_,
      affects = paste(unlist(d$affects %||% character(0)), collapse = " "),
      reference = d$reference %||% NA_character_,
      consequence = d$consequence %||% NA_character_,
      stringsAsFactors = FALSE
    )
  })
  list(
    deviations = if (length(devs)) do.call(rbind, devs) else empty_dev,
    limitations = if (length(lims)) do.call(rbind, lims) else empty_lim
  )
}

# A deviation is bound to the run by the expectation `qa_known_deviation()`
# emits. Reproduction is the evidence that the defect register still describes
# the software; a defect that stopped reproducing is a register entry that has
# become a lie, which is why the run fails on it rather than quietly passing.
qual_deviation_status <- function(dev_df, exp_df) {
  if (nrow(dev_df) == 0) {
    return(data.frame(
      id = character(0), status_run = character(0), tc_id = character(0),
      message = character(0), stringsAsFactors = FALSE
    ))
  }
  rows <- lapply(seq_len(nrow(dev_df)), function(i) {
    id <- dev_df$id[[i]]
    # Anchored on the phrasing `qa_known_deviation()` emits, not merely on the
    # id: a step whose description happened to open with a deviation id would
    # otherwise be mistaken for the binding and could report a defect as
    # reproduced when nothing had tested it.
    marker <- paste0("^", id, " (reproduced|no longer reproduces)")
    hit <- exp_df[grepl(marker, exp_df$message), , drop = FALSE]
    status <- if (nrow(hit) == 0) {
      "not_executed"
    } else if (any(hit$type == "success")) {
      "reproduced"
    } else {
      "not_reproduced"
    }
    data.frame(
      id = id,
      status_run = status,
      tc_id = if (nrow(hit)) hit$tc_id[[1]] else NA_character_,
      message = if (nrow(hit)) hit$message[[1]] else NA_character_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# ---- traceability -----------------------------------------------------------

qual_traceability <- function(req_df, disc_df, tests_df, stages_run) {
  pairs <- list()
  for (i in seq_len(nrow(disc_df))) {
    ids <- strsplit(disc_df$req_ids[[i]] %||% "", " ", fixed = TRUE)[[1]]
    ids <- ids[nzchar(ids)]
    if (length(ids) == 0) next
    tc <- disc_df$tc_id[[i]]
    executed <- tests_df[!is.na(tests_df$tc_id) & tests_df$tc_id == tc, , drop = FALSE]
    status <- if (nrow(executed) == 0) "not_executed" else executed$status[[1]]
    for (id in ids) {
      pairs[[length(pairs) + 1]] <- data.frame(
        req_id = id, tc_id = tc, stage = disc_df$stage[[i]],
        status = status, stringsAsFactors = FALSE
      )
    }
  }
  long <- if (length(pairs)) {
    do.call(rbind, pairs)
  } else {
    data.frame(
      req_id = character(0), tc_id = character(0), stage = character(0),
      status = character(0), stringsAsFactors = FALSE
    )
  }

  per_req <- lapply(seq_len(nrow(req_df)), function(i) {
    id <- req_df$id[[i]]
    mine <- long[long$req_id == id, , drop = FALSE]
    status <- if (nrow(mine) == 0) {
      "uncovered"
    } else if (any(mine$status %in% c("fail", "error", "file_error"))) {
      "failed"
    } else if (any(mine$status == "pass")) {
      "verified"
    } else if (all(mine$status == "not_executed")) {
      "not_executed"
    } else {
      "not_verified"
    }
    data.frame(
      req_id = id, area = req_df$area[[i]], risk = req_df$risk[[i]],
      n_tc = nrow(mine), tc_ids = paste(mine$tc_id, collapse = " "),
      status = status, stringsAsFactors = FALSE
    )
  })
  structure(
    long,
    requirements = if (length(per_req)) {
      do.call(rbind, per_req)
    } else {
      data.frame(
        req_id = character(0), area = character(0), risk = character(0),
        n_tc = integer(0), tc_ids = character(0), status = character(0),
        stringsAsFactors = FALSE
      )
    }
  )
}

# ---- meta consistency -------------------------------------------------------

qual_meta_row <- function(id, description, ok, detail = "") {
  data.frame(
    check_id = id, description = description, ok = isTRUE(ok),
    detail = detail, stringsAsFactors = FALSE
  )
}

# These checks are the suite's own alibi. Every one of them answers a way in
# which a green run could still be worthless: untagged tests, requirements
# nothing verifies, an API that grew a function nobody wrote a requirement for,
# snapshots standing in for expected values, or a file-level error that quietly
# removed half a stage.
qual_meta_checks <- function(req_df, dev_df, lim_df, disc_df, tests_df, exp_df,
                             api, index, tests_dir, stages_run, filter = NULL,
                             performance = NULL, perf_floor = NULL) {
  rows <- list()
  add <- function(...) rows[[length(rows) + 1]] <<- qual_meta_row(...)

  bad_names <- disc_df[!disc_df$ok, , drop = FALSE]
  add(
    "M1", "Every test_that() title matches the qualification grammar",
    nrow(bad_names) == 0,
    if (nrow(bad_names)) {
      paste(sprintf("%s:%s", bad_names$file, bad_names$line), collapse = "; ")
    } else {
      ""
    }
  )

  ids <- disc_df$tc_id[disc_df$ok]
  dup <- unique(ids[duplicated(ids)])
  add("M2", "Test case ids are unique", length(dup) == 0, paste(dup, collapse = ", "))

  referenced <- unique(unlist(strsplit(disc_df$req_ids[disc_df$ok], " ")))
  referenced <- referenced[nzchar(referenced %||% "")]
  unknown <- setdiff(referenced, req_df$id)
  add(
    "M3", "Every requirement id referenced by a test exists",
    length(unknown) == 0, paste(unknown, collapse = ", ")
  )

  uncovered <- setdiff(req_df$id, referenced)
  add(
    "M4", "Every requirement is referenced by at least one test case",
    length(uncovered) == 0, paste(uncovered, collapse = ", ")
  )

  covered_symbols <- unique(unlist(req_df$covers))
  missing_cover <- setdiff(api, covered_symbols)
  add(
    "M5", "Every exported symbol is covered by at least one requirement",
    length(missing_cover) == 0, paste(missing_cover, collapse = ", ")
  )

  not_exported <- setdiff(covered_symbols, api)
  add(
    "M6", "Every symbol named in 'covers' is an exported symbol",
    length(not_exported) == 0, paste(not_exported, collapse = ", ")
  )

  open_devs <- dev_df$id[dev_df$status == "open"]
  unbound <- setdiff(open_devs, qual_discover_deviation_refs(tests_dir))
  add(
    "M7", "Every open deviation is bound by a qa_known_deviation() call",
    length(unbound) == 0, paste(unbound, collapse = ", ")
  )

  snap <- qual_find_snapshot_calls(tests_dir)
  add(
    "M8", "The suite contains no snapshot expectations",
    length(snap) == 0, paste(snap, collapse = "; ")
  )

  all_ids <- c(dev_df$id, lim_df$id)
  dup_dev <- unique(all_ids[duplicated(all_ids)])
  add(
    "M9", "Deviation and limitation ids are unique",
    length(dup_dev) == 0, paste(dup_dev, collapse = ", ")
  )

  # Only the files this run actually selected. The check exists to catch a
  # file-level error that swallowed the cases below it; a filter that excluded
  # a file is a deliberate choice, and reporting every excluded case here would
  # bury the one that matters under hundreds that do not.
  selected <- disc_df$ok & disc_df$stage %in% stages_run
  if (!is.null(filter)) {
    context <- sub("\\.[Rr]$", "", sub("^test-", "", disc_df$file))
    selected <- selected & grepl(filter, context)
  }
  expected_tcs <- disc_df$tc_id[selected]
  missing_tcs <- setdiff(expected_tcs, tests_df$tc_id)
  add(
    "M10", "Every test case this run selected was executed",
    length(missing_tcs) == 0,
    paste(utils::head(missing_tcs, 20), collapse = ", ")
  )

  min_counts <- index$areas %||% list()
  short <- character(0)
  for (entry in min_counts) {
    area <- entry$area
    if (is.null(area)) {
      next
    }
    minimum <- entry$min_tests %||% 0
    have <- sum(disc_df$ok & disc_df$area == area)
    if (have < minimum) {
      short <- c(short, sprintf("%s has %d of %d", area, have, minimum))
    }
  }
  add(
    "M11", "Every area meets its minimum test-case count",
    length(short) == 0, paste(short, collapse = "; ")
  )

  declared_version <- as.character(index$package_version %||% NA_character_)
  actual_version <- as.character(utils::packageVersion("DTAtools"))
  add(
    "M12", "The suite was written for the installed package version",
    identical(declared_version, actual_version),
    sprintf("suite %s, installed %s", declared_version, actual_version)
  )

  bad_tags <- setdiff(
    unique(unlist(strsplit(disc_df$tags[disc_df$ok], ",", fixed = TRUE))),
    c(qual_allowed_tags(), "")
  )
  bad_tags <- bad_tags[!is.na(bad_tags) & nzchar(bad_tags)]
  add(
    "M13", "Every test tag is one of the permitted tags",
    length(bad_tags) == 0, paste(bad_tags, collapse = ", ")
  )

  # The floor is what the caller asked this hardware to reach. It is checked
  # here rather than inside a test case because it is a property of the run
  # and not of any one behaviour: the same measurement is acceptable on the
  # machine the package was developed on and unacceptable on the server it is
  # being qualified for, and only the person ordering the run knows which.
  add_perf_floor(add, performance, perf_floor, stages_run, filter)

  do.call(rbind, rows)
}

# M14 in its own function so that the reasoning about an unset floor stays
# next to it. An absent floor is not a pass with nothing to say: it is the
# documented default, and the report has to distinguish "measured and within
# the floor" from "measured, with no floor to hold it to".
add_perf_floor <- function(add, performance, perf_floor, stages_run, filter = NULL) {
  if (is.null(perf_floor)) {
    add(
      "M14", "Measured throughput reaches the floor the run was given",
      TRUE, "no floor was given; throughput is recorded but not judged"
    )
    return(invisible(NULL))
  }
  if (!"PQ" %in% stages_run) {
    add(
      "M14", "Measured throughput reaches the floor the run was given",
      TRUE, "the performance stage was not part of this run"
    )
    return(invisible(NULL))
  }
  rates <- if (is.data.frame(performance) && "rows_per_sec" %in% names(performance)) {
    stats::setNames(
      suppressWarnings(as.numeric(performance$rows_per_sec)),
      performance$tc_id
    )
  } else {
    numeric(0)
  }
  rates <- rates[is.finite(rates)]
  if (length(rates) == 0) {
    # A filtered run selected whichever cases the operator asked for, and none
    # of them measured anything. That is the filter doing its job, not a
    # failure; an unfiltered run that measured nothing is a different matter,
    # because then the floor was given and quietly went unchecked.
    filtered <- !is.null(filter)
    add(
      "M14", "Measured throughput reaches the floor the run was given",
      filtered,
      if (filtered) {
        "the run's filter selected no performance measurement to hold to the floor"
      } else {
        sprintf(
          "a floor of %s rows/sec was given but the run measured no throughput to hold to it",
          format(perf_floor, scientific = FALSE)
        )
      }
    )
    return(invisible(NULL))
  }
  below <- rates[rates < perf_floor]
  add(
    "M14", "Measured throughput reaches the floor the run was given",
    length(below) == 0,
    if (length(below) == 0) {
      sprintf(
        "%d measurement%s at or above %s rows/sec; slowest %s",
        length(rates), if (length(rates) == 1) "" else "s",
        format(perf_floor, scientific = FALSE),
        format(round(min(rates)), scientific = FALSE)
      )
    } else {
      paste(
        sprintf("%s: %s rows/sec", names(below), format(round(below), scientific = FALSE)),
        collapse = "; "
      )
    }
  )
  invisible(NULL)
}

# Detected by parsing rather than by searching the text. A test that checks
# this very rule has to name the function it forbids, and a text search cannot
# tell that mention from a use -- it would report the check that enforces the
# rule as the first violation of it.
qual_find_snapshot_calls <- function(tests_dir) {
  files <- sort(list.files(tests_dir, pattern = "\\.[Rr]$", full.names = TRUE))
  hits <- character(0)
  for (f in files) {
    exprs <- tryCatch(parse(f, keep.source = TRUE), error = function(e) NULL)
    if (is.null(exprs)) next
    pd <- utils::getParseData(exprs)
    if (is.null(pd)) next
    calls <- pd[pd$token == "SYMBOL_FUNCTION_CALL" &
      grepl("^expect_snapshot", pd$text), , drop = FALSE]
    if (nrow(calls)) {
      hits <- c(hits, sprintf(
        "%s:%s", basename(f), paste(unique(calls$line1), collapse = ",")
      ))
    }
  }
  hits
}

qual_grep_sources <- function(tests_dir, pattern) {
  files <- list.files(tests_dir, pattern = "\\.[Rr]$", full.names = TRUE)
  hits <- character(0)
  for (f in files) {
    lines <- readLines(f, warn = FALSE)
    where <- grep(pattern, lines, fixed = TRUE)
    if (length(where)) {
      hits <- c(hits, sprintf("%s:%s", basename(f), paste(where, collapse = ",")))
    }
  }
  hits
}

# Only a real binding counts. Reading every mention of an identifier would let
# a deviation merely discussed in a comment satisfy the check that it is
# tested, which is exactly the gap this check exists to close -- and comments
# about deviations are common here, because that is where the reasoning lives.
qual_discover_deviation_refs <- function(tests_dir) {
  files <- sort(list.files(tests_dir, pattern = "\\.[Rr]$", full.names = TRUE))
  ids <- character(0)
  for (f in files) {
    exprs <- tryCatch(parse(f, keep.source = TRUE), error = function(e) NULL)
    if (is.null(exprs)) next
    pd <- utils::getParseData(exprs)
    if (is.null(pd)) next

    calls <- pd[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text == "qa_known_deviation", , drop = FALSE]
    strings <- pd[pd$token == "STR_CONST", , drop = FALSE]
    for (i in seq_len(nrow(calls))) {
      # The first string literal at or after the call is its identifier
      # argument; a comment carries no STR_CONST token, so it cannot be one.
      following <- strings[
        strings$line1 > calls$line1[[i]] |
          (strings$line1 == calls$line1[[i]] & strings$col1 > calls$col1[[i]]), ,
        drop = FALSE
      ]
      following <- following[order(following$line1, following$col1), , drop = FALSE]
      if (nrow(following) == 0) next
      id <- gsub("^[\"']|[\"']$", "", following$text[[1]])
      if (grepl("^DEV-[0-9]{3}$", id)) {
        ids <- c(ids, id)
      }
    }
  }
  unique(ids)
}

# ---- environment ------------------------------------------------------------

qual_safe <- function(expr, default = NA) {
  tryCatch(expr, error = function(e) default, warning = function(w) default)
}

# Parsed here rather than with `tools:::.split_dependencies()`, which is
# internal and would break the package on an R release that renames it. The
# question this answers is the one an installation qualification exists for:
# is every declared dependency present, and is it new enough?
qual_dependency_check <- function() {
  desc <- utils::packageDescription("DTAtools")
  rows <- list()
  for (field in c("Depends", "Imports", "Suggests")) {
    raw <- desc[[field]]
    if (is.null(raw) || is.na(raw)) next
    for (entry in trimws(strsplit(raw, ",")[[1]])) {
      if (!nzchar(entry)) next
      pkg <- trimws(sub("\\s*\\(.*$", "", entry))
      constraint <- regmatches(entry, regexec("\\(([^)]*)\\)", entry))[[1]]
      required <- if (length(constraint) > 1) trimws(constraint[[2]]) else NA_character_
      required_version <- if (is.na(required)) {
        NA_character_
      } else {
        trimws(sub("^[><=!]+", "", required))
      }
      installed <- if (identical(pkg, "R")) {
        paste0(R.version$major, ".", R.version$minor)
      } else {
        as.character(qual_safe(utils::packageVersion(pkg), NA))
      }
      satisfied <- if (is.na(installed)) {
        FALSE
      } else if (is.na(required_version)) {
        TRUE
      } else {
        isTRUE(utils::compareVersion(installed, required_version) >= 0)
      }
      rows[[length(rows) + 1]] <- data.frame(
        package = pkg, field = field, required_version = required_version,
        installed_version = installed, installed = !is.na(installed),
        satisfied = satisfied, stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0) {
    return(data.frame(
      package = character(0), field = character(0),
      required_version = character(0), installed_version = character(0),
      installed = logical(0), satisfied = logical(0), stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

# `l10n_info()` reports the native encoding under different names on different
# platforms: `codeset` on Unix, `codepage` on Windows, and neither is present
# on both. Reading only one of them records "unknown" on half the systems this
# package is deployed to, which is precisely the half a qualification report
# should be able to describe.
qual_native_encoding <- function() {
  info <- l10n_info()
  if (isTRUE(info[["UTF-8"]])) {
    return("UTF-8")
  }
  if (!is.null(info[["codeset"]]) && nzchar(info[["codeset"]])) {
    return(info[["codeset"]])
  }
  if (!is.null(info[["codepage"]]) && !is.na(info[["codepage"]])) {
    return(paste0("CP", info[["codepage"]]))
  }
  if (isTRUE(info[["Latin-1"]])) {
    return("latin1")
  }
  Sys.getlocale("LC_CTYPE")
}

qual_browser_binary <- function() {
  for (var in c("CHROMOTE_CHROME", "DTATOOLS_CHROME")) {
    value <- Sys.getenv(var, "")
    if (nzchar(value) && file.exists(value)) {
      return(value)
    }
  }
  found <- qual_safe(chromote::find_chrome(), NA_character_)
  if (length(found) == 1 && !is.na(found) && nzchar(found)) found else NA_character_
}

qual_environment <- function(tester, run_id, started_at) {
  desc <- utils::packageDescription("DTAtools")
  backend <- qual_safe(dta_pdf_backend(), NULL)
  arrow_info <- qual_safe(arrow::arrow_info(), NULL)
  list(
    run = list(id = run_id, tester = tester, started_at = format(started_at, "%Y-%m-%dT%H:%M:%S%z")),
    package = list(
      version = as.character(utils::packageVersion("DTAtools")),
      sha = desc$RemoteSha %||% desc$GithubSHA1 %||% NA_character_,
      built = desc$Built %||% NA_character_,
      packaged = desc$Packaged %||% NA_character_,
      library_path = dirname(system.file(package = "DTAtools")),
      loaded_via = if (isTRUE(qual_safe(pkgload::is_dev_package("DTAtools"), FALSE))) {
        "pkgload"
      } else {
        "installed"
      },
      md5_check = qual_safe(tools::checkMD5sums("DTAtools"), NA)
    ),
    r = list(
      version = R.version.string,
      platform = R.version$platform,
      arch = R.version$arch,
      os = utils::osVersion %||% NA_character_
    ),
    system = list(
      locale = Sys.getlocale(),
      timezone = qual_safe(Sys.timezone(), NA_character_),
      encoding = qual_native_encoding(),
      hostname = unname(Sys.info()[["nodename"]]),
      user = unname(Sys.info()[["user"]]),
      pid = Sys.getpid(),
      wd = getwd(),
      lib_paths = .libPaths()
    ),
    arrow = list(
      version = as.character(arrow_info$version %||% NA_character_),
      dataset = isTRUE(arrow_info$capabilities[["dataset"]]),
      acero = isTRUE(arrow_info$capabilities[["acero"]]),
      parquet = isTRUE(arrow_info$capabilities[["parquet"]]),
      gzip = isTRUE(arrow_info$capabilities[["gzip"]])
    ),
    tooling = list(
      pandoc = isTRUE(qual_safe(rmarkdown::pandoc_available(), FALSE)),
      pandoc_version = as.character(qual_safe(rmarkdown::pandoc_version(), NA_character_)),
      pdf_backend = backend$name %||% NA_character_,
      browser = qual_browser_binary(),
      shinytest2 = requireNamespace("shinytest2", quietly = TRUE)
    ),
    options = qual_option_snapshot(),
    dependencies = qual_dependency_check()
  )
}

qual_option_snapshot <- function() {
  all_opts <- options()
  mine <- all_opts[grepl("^DTAtools\\.", names(all_opts))]
  mine[["DTAtools.qualification"]] <- NULL
  lapply(mine, function(x) if (is.atomic(x) && length(x) <= 5) x else class(x))
}

# ---- developer test evidence ------------------------------------------------

# The unit suite is supplementary evidence, never a substitute: it proves the
# developers tested the code, not that this installation behaves. It is
# reported when it is reachable and named as absent when it is not, because an
# auditor should never have to guess why a section is missing.
qual_unit_tests <- function(mode = c("auto", "never", "always"), scale = "standard",
                            timeout_sec = 3600) {
  mode <- match.arg(mode)
  if (identical(mode, "never")) {
    return(list(run = FALSE, reason = "disabled by include_unit_tests = \"never\"", summary = NULL))
  }
  dir <- system.file("tests", "testthat", package = "DTAtools")
  if (!nzchar(dir) && isTRUE(qual_safe(pkgload::is_dev_package("DTAtools"), FALSE))) {
    dir <- file.path(qual_safe(pkgload::pkg_path(), ""), "tests", "testthat")
  }
  if (!nzchar(dir) || !dir.exists(dir)) {
    return(list(
      run = FALSE,
      reason = "the package was installed without its tests (use R CMD INSTALL --install-tests)",
      summary = NULL
    ))
  }
  if (identical(mode, "auto") && identical(scale, "quick")) {
    return(list(run = FALSE, reason = "skipped at scale = \"quick\"", summary = NULL))
  }
  qual_log("unit_tests", "start", dir)
  res <- qual_unit_tests_run(dir, timeout_sec)
  if (is.null(res$results)) {
    return(list(run = FALSE, reason = res$reason, summary = NULL))
  }
  df <- res$results
  list(
    run = TRUE,
    reason = NA_character_,
    summary = list(
      n_tests = nrow(df),
      passed = sum(df$passed %||% 0),
      failed = sum(df$failed %||% 0),
      errors = sum(df$error %||% FALSE),
      skipped = sum(df$skipped %||% FALSE),
      path = dir
    ),
    # Named, not just counted. "3 failed" with nothing to look up is the kind
    # of line a reviewer has to leave the document to resolve, and a developer
    # suite run from an installed library can fail for reasons that have
    # nothing to do with the installation being sound.
    failures = res$failures,
    results = df
  )
}

# The developer suite runs in its own process, for two reasons. It is bounded:
# a qualification is run unattended on a server, and a developer test that
# blocks -- on a browser it cannot find, a socket that never answers -- would
# otherwise hang the run with no output and no verdict. And it is isolated: the
# developer suite sets options, changes directory and loads packages, none of
# which should be able to reach the session that is producing the evidence.
#
# Returns the per-file result frame, or NULL when the suite did not finish.
qual_unit_tests_run <- function(dir, timeout_sec) {
  script <- tempfile(fileext = ".R")
  out <- tempfile(fileext = ".rds")
  err <- tempfile(fileext = ".txt")
  on.exit(unlink(c(script, out, err)), add = TRUE)

  # The child has to reach the same package the parent is exercising, and the
  # two cases differ: an installed library is found through the search path,
  # a source tree only through pkgload. Getting this wrong does not fail
  # loudly -- it reports the developer suite as unrunnable -- so both are
  # written out rather than assumed.
  dev_path <- if (isTRUE(qual_safe(pkgload::is_dev_package("DTAtools"), FALSE))) {
    qual_safe(pkgload::pkg_path(), "")
  } else {
    ""
  }
  load_line <- if (nzchar(dev_path)) {
    sprintf("pkgload::load_all(%s, quiet = TRUE)", qual_deparse_chr(dev_path))
  } else {
    "loadNamespace(\"DTAtools\")"
  }

  writeLines(
    c(
      sprintf(".libPaths(%s)", qual_deparse_chr(.libPaths())),
      "Sys.setenv(TESTTHAT_PARALLEL = \"FALSE\")",
      load_line,
      sprintf(
        "res <- testthat::test_dir(%s, reporter = \"silent\", stop_on_failure = FALSE,",
        qual_deparse_chr(dir)
      ),
      "  package = \"DTAtools\", load_package = \"none\")",
      "fails <- do.call(rbind, lapply(res, function(r) {",
      "  bad <- Filter(function(e) inherits(e, c(\"expectation_failure\", \"expectation_error\")), r$results)",
      "  if (!length(bad)) return(NULL)",
      "  data.frame(",
      "    file = r$file,",
      "    test = if (is.null(r$test) || is.na(r$test)) \"(file level)\" else r$test,",
      "    type = sub(\"^expectation_\", \"\", vapply(bad, function(e) class(e)[[1]], \"\")),",
      "    message = substr(gsub(\"[\r\n\t]\", \" \", vapply(bad, conditionMessage, \"\")), 1, 400),",
      "    stringsAsFactors = FALSE)",
      "}))",
      sprintf(
        "saveRDS(list(summary = as.data.frame(res), failures = fails), %s)",
        qual_deparse_chr(out)
      )
    ),
    script
  )

  rscript <- file.path(
    R.home("bin"),
    if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
  )
  started <- Sys.time()
  status <- qual_safe(
    system2(
      rscript, c("--no-init-file", "--no-save", shQuote(normalizePath(script, winslash = "/"))),
      stdout = FALSE, stderr = err, timeout = timeout_sec
    ),
    NA_integer_
  )
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))

  payload <- if (file.exists(out)) qual_safe(readRDS(out), NULL) else NULL
  if (!is.null(payload$summary)) {
    return(list(
      results = payload$summary,
      failures = payload$failures,
      reason = NA_character_
    ))
  }

  # A reason a reader can act on. "It did not run" with no cause is the kind of
  # line that turns into a question at a review meeting.
  timed_out <- elapsed >= timeout_sec * 0.95
  detail <- qual_safe(utils::head(readLines(err, warn = FALSE), 3), character(0))
  detail <- detail[nzchar(detail)]
  reason <- if (timed_out) {
    sprintf(
      "the developer suite did not finish within %d seconds and was stopped",
      as.integer(timeout_sec)
    )
  } else {
    paste0(
      sprintf(
        "the developer suite could not be executed (exit status %s)",
        if (is.na(status)) "unknown" else as.character(status)
      ),
      if (length(detail)) paste0(": ", paste(detail, collapse = " ")) else ""
    )
  }
  list(results = NULL, failures = NULL, reason = reason)
}

# deparse() of a character vector, in a form a child process reads back
# identically whatever the path separators or the native encoding.
qual_deparse_chr <- function(x) {
  paste0("c(", paste(encodeString(x, quote = "\""), collapse = ", "), ")")
}

# ---- verdict ----------------------------------------------------------------

qual_verdict <- function(tests_df, meta_df, dev_status, partial) {
  failed <- sum(tests_df$status %in% c("fail", "error", "file_error"))
  meta_failed <- sum(!meta_df$ok)
  stale <- sum(dev_status$status_run == "not_reproduced")
  verdict <- if (failed > 0 || meta_failed > 0 || stale > 0) {
    "FAIL"
  } else if (any(tests_df$status == "skip")) {
    "PASS WITH NOT-EXECUTED TESTS"
  } else {
    "PASS"
  }
  if (isTRUE(partial)) paste(verdict, "(PARTIAL)") else verdict
}

# ---- hashing ----------------------------------------------------------------

qual_hash_algorithm <- function() {
  if (exists("sha256sum", asNamespace("tools"))) "sha256" else "md5"
}

qual_hash_one <- function(path) {
  algo <- qual_hash_algorithm()
  fun <- if (identical(algo, "sha256")) {
    get("sha256sum", asNamespace("tools"))
  } else {
    tools::md5sum
  }
  unname(fun(path))
}

# Written last and covering everything else, so the manifest is a statement
# about a finished bundle. The report quotes the hash of results.json, which is
# computed before rendering: that binds the human-readable document to the
# machine-readable evidence it describes.
qual_hash_bundle <- function(dir) {
  algo <- qual_hash_algorithm()
  name <- if (identical(algo, "sha256")) "SHA256SUMS" else "MD5SUMS"
  manifest <- file.path(dir, name)

  # The paths are recorded relative to the bundle, so that `sha256sum -c` works
  # from the bundle directory on any machine. Deriving them by stripping the
  # bundle path from an absolute one is what a first attempt does, and it fails
  # exactly where it matters: on Windows the two halves disagree about the
  # slash. `list.files()` already returns relative paths, so it is asked for
  # those and the absolute form is rebuilt only to read the bytes.
  rel <- sort(list.files(dir, recursive = TRUE, all.files = FALSE))
  rel <- rel[!basename(rel) %in% c("SHA256SUMS", "MD5SUMS")]
  hashes <- vapply(file.path(dir, rel), qual_hash_one, character(1), USE.NAMES = FALSE)
  con <- file(manifest, open = "wb")
  on.exit(close(con), add = TRUE)
  writeLines(sprintf("%s  %s", hashes, rel), con, sep = "\n")
  list(
    algorithm = algo, manifest = manifest,
    files = data.frame(file = rel, hash = hashes, stringsAsFactors = FALSE)
  )
}

# ---- writing the bundle -----------------------------------------------------

qual_write_csv <- function(x, path) {
  utils::write.csv(x, path, row.names = FALSE, fileEncoding = "UTF-8", na = "")
  invisible(path)
}

# `covers` and the other list columns are flattened for CSV but kept as arrays
# in JSON, so the CSVs open in a spreadsheet without surprises while the JSON
# stays machine-readable.
qual_flatten <- function(x) {
  for (nm in names(x)) {
    if (is.list(x[[nm]])) {
      x[[nm]] <- vapply(x[[nm]], function(v) paste(unlist(v), collapse = " "), character(1))
    }
  }
  x
}

# testthat's per-test frame holds a list column of the expectation objects
# themselves -- conditions, which have no JSON representation. They belong in
# the CSV, flattened, and not in results.json at all: what a machine reader
# wants from this section is the counts and the names of what did not pass.
qual_unit_tests_json <- function(unit) {
  if (is.null(unit)) {
    return(NULL)
  }
  unit$results <- NULL
  unit
}

# One evidence file. A failure is logged and reported, never fatal: by the time
# the bundle is written the stages have already run, and aborting here would
# throw away the whole run's evidence over one awkward column.
qual_write_evidence <- function(x, path) {
  ok <- !is.null(qual_safe(qual_write_csv(qual_flatten(x), path), NULL))
  if (!ok) {
    qual_log("write_failed", basename(path))
  }
  ok
}

qual_write_bundle <- function(x, dir) {
  results_dir <- file.path(dir, "results")
  dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

  qual_write_evidence(x$tests, file.path(results_dir, "tests.csv"))
  qual_write_evidence(x$expectations, file.path(results_dir, "expectations.csv"))
  qual_write_evidence(x$traceability, file.path(results_dir, "traceability.csv"))
  qual_write_evidence(x$requirements, file.path(results_dir, "requirements.csv"))
  qual_write_evidence(x$deviations, file.path(results_dir, "deviations.csv"))
  qual_write_evidence(x$limitations, file.path(results_dir, "limitations.csv"))
  qual_write_evidence(x$meta_checks, file.path(results_dir, "meta_checks.csv"))
  qual_write_evidence(attr(x$traceability, "requirements"), file.path(results_dir, "coverage.csv"))
  if (!is.null(x$performance) && nrow(x$performance) > 0) {
    qual_write_evidence(x$performance, file.path(results_dir, "performance.csv"))
  }
  # Written whenever the developer suite ran, so that its result is auditable
  # in the same way the stages are rather than existing only as four numbers
  # in the report. testthat's own frame carries list columns, which is why it
  # goes through the same flattening every other evidence frame does.
  if (isTRUE(x$unit_tests$run) && !is.null(x$unit_tests$results)) {
    qual_write_evidence(x$unit_tests$results, file.path(results_dir, "unit-tests.csv"))
  }

  jsonlite::write_json(
    x$environment, file.path(results_dir, "environment.json"),
    auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null", digits = NA
  )
  writeLines(
    utils::capture.output(print(utils::sessionInfo())),
    file.path(results_dir, "sessioninfo.txt")
  )

  payload <- list(
    schema_version = 1L,
    generator = list(
      package = "DTAtools",
      version = as.character(utils::packageVersion("DTAtools")),
      r_version = R.version.string
    ),
    run = x$run,
    package = x$environment$package,
    summary = x$summary,
    tests = x$tests,
    expectations = x$expectations,
    requirements = x$requirements,
    coverage = attr(x$traceability, "requirements"),
    traceability = x$traceability,
    deviations = x$deviations,
    deviation_status = x$deviation_status,
    limitations = x$limitations,
    meta_checks = x$meta_checks,
    performance = x$performance,
    unit_tests = qual_unit_tests_json(x$unit_tests),
    environment = x$environment
  )
  json_path <- file.path(results_dir, "results.json")
  # Guarded for the same reason the CSVs are: every stage has already run by
  # this point, and an object that will not serialise must not cost the run
  # its report and its manifest as well as this one file.
  written <- qual_safe(
    jsonlite::write_json(
      payload, json_path,
      auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null", digits = NA
    ),
    NULL
  )
  if (is.null(written) && !file.exists(json_path)) {
    qual_log("write_failed", basename(json_path))
    writeLines("{}", json_path)
  }
  json_path
}

qual_write_summary_txt <- function(x, dir) {
  algo <- toupper(x$run$hash_algorithm)
  lines <- c(
    sprintf("DTAtools qualification %s", x$run$id),
    "",
    sprintf("Verdict          : %s", x$verdict),
    sprintf("Package          : DTAtools %s", x$environment$package$version),
    sprintf("Executed by      : %s on %s", x$run$tester, x$environment$system$hostname),
    sprintf("Started / ended  : %s / %s", x$run$started_at, x$run$finished_at),
    sprintf("Stages run       : %s", paste(x$run$stages_run, collapse = ", ")),
    sprintf("Scale tier       : %s", x$run$scale),
    "",
    sprintf(
      "Test cases       : %d executed, %d passed, %d failed, %d errored, %d not executed",
      nrow(x$tests), sum(x$tests$status == "pass"),
      sum(x$tests$status == "fail"),
      sum(x$tests$status %in% c("error", "file_error")),
      sum(x$tests$status == "skip")
    ),
    sprintf(
      "Requirements     : %d total, %d verified, %d failed, %d not verified",
      nrow(x$requirements),
      sum(attr(x$traceability, "requirements")$status == "verified"),
      sum(attr(x$traceability, "requirements")$status == "failed"),
      sum(attr(x$traceability, "requirements")$status %in% c("not_verified", "not_executed", "uncovered"))
    ),
    sprintf("Meta checks      : %d of %d passed", sum(x$meta_checks$ok), nrow(x$meta_checks)),
    "",
    sprintf("%s of results.json: %s", algo, x$run$results_json_hash),
    "",
    "To verify this bundle has not been altered, run from this directory:",
    sprintf("  %ssum -c %s", x$run$hash_algorithm, basename(x$run$hash_manifest)),
    "",
    "The signed report is report/qualification-report.md (and the rendered",
    "formats beside it). The machine-readable evidence is results/results.json."
  )
  path <- file.path(dir, "SUMMARY.txt")
  writeLines(lines, path)
  path
}

# ---- the S3 result object ---------------------------------------------------

new_dta_qualification <- function(...) {
  structure(list(...), class = "dta_qualification")
}

#' @export
print.dta_qualification <- function(x, ...) {
  ok <- startsWith(x$verdict, "PASS")
  cli::cli_h1("DTAtools qualification")
  if (ok) {
    cli::cli_alert_success("Verdict: {.strong {x$verdict}}")
  } else {
    cli::cli_alert_danger("Verdict: {.strong {x$verdict}}")
  }
  cli::cli_text("Run {.val {x$run$id}} on DTAtools {.val {x$environment$package$version}}")
  for (stage in x$run$stages_run) {
    rows <- x$tests[x$tests$stage == stage, , drop = FALSE]
    cli::cli_li(sprintf(
      "%s: %d passed, %d failed, %d errored, %d not executed",
      stage, sum(rows$status == "pass"), sum(rows$status == "fail"),
      sum(rows$status %in% c("error", "file_error")), sum(rows$status == "skip")
    ))
  }
  failed_meta <- sum(!x$meta_checks$ok)
  if (failed_meta > 0) {
    cli::cli_alert_warning("{failed_meta} meta-consistency check{?s} failed.")
  }
  cli::cli_text("Evidence bundle: {.path {x$bundle_dir}}")
  invisible(x)
}

#' @export
summary.dta_qualification <- function(object, ...) {
  per_stage <- object$summary$per_stage
  print(per_stage)
  bad <- object$tests[object$tests$status != "pass", , drop = FALSE]
  if (nrow(bad) > 0) {
    cli::cli_h2("Test cases not passing")
    for (i in seq_len(nrow(bad))) {
      first <- object$expectations[
        object$expectations$tc_id == bad$tc_id[[i]] &
          object$expectations$type != "success", ,
        drop = FALSE
      ]
      cli::cli_li(sprintf(
        "%s [%s] %s", bad$tc_id[[i]], bad$status[[i]],
        if (nrow(first)) substr(first$message[[1]], 1, 120) else ""
      ))
    }
  }
  invisible(per_stage)
}

# ---- the entry points -------------------------------------------------------

#' Qualification requirements shipped with the package
#'
#' Reads the requirement specifications that the qualification suite verifies.
#' Each requirement states one testable property of the software, carries a
#' risk rating and a category, and names the exported symbols it constrains.
#' The suite's test titles reference these identifiers, which is how the
#' traceability matrix in a qualification report is derived rather than
#' maintained by hand.
#'
#' @param area Optional character. Restrict the result to one functional area,
#'   for example `"VAL"`. Case-insensitive. `NULL` returns every requirement.
#' @param dir Optional character. Directory holding the qualification suite.
#'   Defaults to the copy installed with the package.
#'
#' @return A data frame with one row per requirement and the columns `id`,
#'   `area`, `area_title`, `text`, `risk`, `category`, `covers` (a list column
#'   of exported symbol names), `source` and `file`.
#'
#' @seealso [run_qualification()] executes the suite that verifies them.
#' @export
#' @examples
#' reqs <- qualification_requirements()
#' nrow(reqs)
#' head(reqs[, c("id", "risk", "category")])
qualification_requirements <- function(area = NULL, dir = NULL) {
  root <- qual_root(dir)
  api <- qual_read_api(root)
  out <- qual_requirements_df(root, api)
  if (!is.null(area)) {
    out <- out[toupper(out$area) %in% toupper(area), , drop = FALSE]
  }
  out
}

qual_read_api <- function(root) {
  path <- file.path(root, "requirements", "_api.yaml")
  if (!file.exists(path)) {
    return(character(0))
  }
  unlist(yaml::read_yaml(path)$exports %||% character(0), use.names = FALSE)
}

qual_read_index <- function(root) {
  path <- file.path(root, "requirements", "_index.yaml")
  if (!file.exists(path)) {
    return(list())
  }
  yaml::read_yaml(path)
}

#' Qualify the installed package on this system
#'
#' Runs the qualification suite that ships with the package and writes an
#' evidence bundle: a Validation Summary Report, machine-readable results, a
#' requirements traceability matrix, a record of the environment, a
#' contemporaneous run log and a hash manifest covering all of it.
#'
#' The suite is separate from the package's developer tests. It runs against
#' the *installed* namespace, so what it reports is the behaviour of the copy
#' on this machine, and every step records what was expected alongside what
#' happened. It is meant to be run by hand when the package is deployed onto a
#' target system, and its output is meant to be reviewed and signed.
#'
#' Three stages are executed, following the usual qualification vocabulary.
#' Installation qualification (`"IQ"`) records what is installed and checks it
#' against what should be. Operational qualification (`"OQ"`) exercises each
#' documented behaviour against explicit expected values. Performance
#' qualification (`"PQ"`) runs end-to-end workflows and measures behaviour at
#' scale.
#'
#' @param output_dir Character. Directory to create the evidence bundle in. A
#'   run-specific subdirectory is created inside it; an existing bundle is
#'   never overwritten.
#' @param stages Character vector of stages to run, any of `"IQ"`, `"OQ"` and
#'   `"PQ"`. Pass `character()` to build the static parts of the bundle
#'   (requirements, coverage, environment) without executing tests.
#' @param scale One of `"full"`, `"standard"` or `"quick"`. The tier controls
#'   how much data the performance stage generates and how many randomised
#'   cases the differential tests draw. `"full"` is the default because a
#'   qualification run is deliberate and infrequent; `"quick"` exists for
#'   development and continuous integration.
#' @param formats Character vector of report formats. Markdown is always
#'   written. HTML and Word are produced when pandoc is available, PDF when a
#'   PDF backend is (see [dta_pdf_backend()]); a format that cannot be produced
#'   is recorded as such rather than failing the run.
#' @param filter Optional regular expression restricting which test files run.
#'   A filtered run is marked partial, because it cannot support a complete
#'   claim about the software.
#' @param tester Character. Who is performing the run. Recorded in the report.
#' @param reviewer Optional character. Who is reviewing it. Recorded in the
#'   report's approval block.
#' @param include_unit_tests One of `"auto"`, `"never"` or `"always"`. Whether
#'   to also run the developer test suite as supplementary evidence. It is only
#'   reachable when the package was installed with its tests.
#' @param perf_floor Optional numeric. A throughput in rows per second that the
#'   performance stage must reach. `NULL` records throughput without judging
#'   it, which is the right default when the target hardware is unknown.
#' @param seed Integer. Seed for the randomised differential cases, so a run is
#'   reproducible.
#' @param quiet Logical. Suppress the progress and summary output.
#'
#' @return Invisibly, an object of class `dta_qualification` with the verdict,
#'   the per-test and per-expectation evidence, the traceability matrix, the
#'   deviation register, the meta-consistency checks, the environment record
#'   and the path to the bundle. Its `print()` method summarises the run.
#'
#' @seealso [qualification_requirements()] for the requirements being verified.
#' @export
#' @examples
#' # A short installation-only run, which needs no test data.
#' bundle <- run_qualification(
#'   tempfile("qualification-"),
#'   stages = "IQ", scale = "quick", formats = "md", quiet = TRUE
#' )
#' bundle$verdict
run_qualification <- function(output_dir,
                              stages = c("IQ", "OQ", "PQ"),
                              scale = c("full", "standard", "quick"),
                              formats = c("md", "html", "docx", "pdf"),
                              filter = NULL,
                              tester = unname(Sys.info()[["user"]]),
                              reviewer = NULL,
                              include_unit_tests = c("auto", "never", "always"),
                              perf_floor = NULL,
                              seed = 20260101L,
                              quiet = FALSE) {
  scale <- match.arg(scale)
  include_unit_tests <- match.arg(include_unit_tests)

  if (!is.character(output_dir) || length(output_dir) != 1 || is.na(output_dir)) {
    cli::cli_abort("{.arg output_dir} must be a single directory path.")
  }
  if (!is.character(stages) || !all(stages %in% qual_stages())) {
    cli::cli_abort("{.arg stages} must be a subset of {.val {qual_stages()}}.")
  }
  if (!is.character(formats) || !all(formats %in% qual_formats())) {
    cli::cli_abort("{.arg formats} must be a subset of {.val {qual_formats()}}.")
  }
  if (!is.null(filter) && (!is.character(filter) || length(filter) != 1)) {
    cli::cli_abort("{.arg filter} must be {.code NULL} or a single regular expression.")
  }
  for (pkg in c("testthat", "withr")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      cli::cli_abort(c(
        "The qualification suite needs {.pkg {pkg}}.",
        i = "Install it with {.run install.packages(\"{pkg}\")}."
      ))
    }
  }
  formats <- union("md", formats)
  stages <- intersect(qual_stages(), stages)

  root <- qual_root()
  tests_dir <- file.path(root, "tests")
  started_at <- Sys.time()
  run_id <- qual_run_id(time = started_at)
  bundle_dir <- file.path(output_dir, run_id)
  if (dir.exists(bundle_dir)) {
    cli::cli_abort("An evidence bundle already exists at {.path {bundle_dir}}.")
  }
  dir.create(file.path(bundle_dir, "results"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(bundle_dir, "report"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(bundle_dir, "artifacts"), recursive = TRUE, showWarnings = FALSE)

  log_con <- qual_log_open(file.path(bundle_dir, "run.log"))
  on.exit(qual_safe(close(log_con), NULL), add = TRUE)

  previous <- getOption("DTAtools.qualification")
  options(DTAtools.qualification = list(
    run_id = run_id, bundle_dir = bundle_dir, scale = scale, root = root,
    seed = seed, log = log_con, tester = tester, perf_floor = perf_floor
  ))
  on.exit(options(DTAtools.qualification = previous), add = TRUE)

  qual_log("run_start", run_id, tester, scale, paste(stages, collapse = ","))
  if (!quiet) {
    cli::cli_alert_info("Qualification run {.val {run_id}} ({scale} tier)")
  }

  environment_record <- qual_environment(tester, run_id, started_at)
  qual_log("env", environment_record$r$version, environment_record$system$hostname)

  index <- qual_read_index(root)
  api <- qual_read_api(root)
  req_df <- qual_requirements_df(root, api)
  devs <- qual_deviations_df(root)
  disc_df <- qual_discover_tests(tests_dir)

  tests_df <- qual_tests_empty()
  exp_df <- qual_expectations_df(NULL, NA_character_)
  for (stage in stages) {
    if (!quiet) cli::cli_alert_info("Running {stage} ...")
    results <- qual_run_stage(stage, tests_dir, filter, qual_reporter(log_con))
    if (inherits(results, "qual_stage_abort")) {
      if (!quiet) {
        cli::cli_alert_danger("{stage} could not run: {as.character(results)}")
      }
      tests_df <- rbind(tests_df, qual_stage_abort_row(stage, as.character(results)))
      exp_df <- rbind(exp_df, data.frame(
        stage = stage, tc_id = paste0(stage, " (stage)"), seq = 1L,
        type = "error", message = as.character(results),
        file = NA_character_, line = NA_integer_, stringsAsFactors = FALSE
      ))
      next
    }
    tests_df <- rbind(tests_df, qual_tests_df(results, stage))
    exp_df <- rbind(exp_df, qual_expectations_df(results, stage))
  }

  unit <- if (length(stages) > 0) {
    # Announced, because it is the one part of the run that reports nothing
    # while it works and can take longer than the stages above it put together.
    if (!quiet && !identical(include_unit_tests, "never")) {
      cli::cli_alert_info("Running the developer test suite as supplementary evidence ...")
    }
    qual_unit_tests(include_unit_tests, scale)
  } else {
    list(run = FALSE, reason = "no stages were executed", summary = NULL)
  }
  qual_log(
    "unit_tests", "end",
    if (isTRUE(unit$run)) "completed" else unit$reason %||% "not run"
  )

  dev_status <- qual_deviation_status(devs$deviations, exp_df)
  performance <- qual_read_performance(bundle_dir)
  meta_df <- qual_meta_checks(
    req_df, devs$deviations, devs$limitations, disc_df, tests_df, exp_df,
    api, index, tests_dir, stages, filter, performance, perf_floor
  )
  trace <- qual_traceability(req_df, disc_df, tests_df, stages)
  partial <- !is.null(filter) || !identical(stages, qual_stages())
  verdict <- qual_verdict(tests_df, meta_df, dev_status, partial)

  finished_at <- Sys.time()

  per_stage <- do.call(rbind, lapply(stages, function(stage) {
    rows <- tests_df[tests_df$stage == stage, , drop = FALSE]
    data.frame(
      stage = stage, n_tests = nrow(rows), pass = sum(rows$status == "pass"),
      fail = sum(rows$status == "fail"),
      error = sum(rows$status %in% c("error", "file_error")),
      skip = sum(rows$status == "skip"),
      seconds = round(sum(rows$real_sec, na.rm = TRUE), 1),
      stringsAsFactors = FALSE
    )
  }))
  coverage <- attr(trace, "requirements")

  x <- new_dta_qualification(
    run = list(
      id = run_id, tester = tester, reviewer = reviewer %||% NA_character_,
      started_at = format(started_at, "%Y-%m-%dT%H:%M:%S%z"),
      finished_at = format(finished_at, "%Y-%m-%dT%H:%M:%S%z"),
      stages_requested = stages, stages_run = stages, scale = scale,
      filter = filter %||% NA_character_, partial = partial,
      formats_requested = formats, seed = seed,
      hash_algorithm = qual_hash_algorithm()
    ),
    verdict = verdict,
    summary = list(
      verdict = verdict,
      per_stage = per_stage %||% data.frame(),
      n_requirements = nrow(req_df),
      n_verified = sum(coverage$status == "verified"),
      n_failed = sum(coverage$status == "failed"),
      n_not_verified = sum(coverage$status == "not_verified"),
      n_not_executed = sum(coverage$status == "not_executed"),
      n_uncovered = sum(coverage$status == "uncovered"),
      n_deviations_open = sum(devs$deviations$status == "open"),
      n_reproduced = sum(dev_status$status_run == "reproduced"),
      n_not_reproduced = sum(dev_status$status_run == "not_reproduced")
    ),
    tests = tests_df, expectations = exp_df, discovered = disc_df,
    requirements = req_df, traceability = trace,
    deviations = devs$deviations, limitations = devs$limitations,
    deviation_status = dev_status, meta_checks = meta_df,
    performance = performance, unit_tests = unit,
    environment = environment_record, bundle_dir = bundle_dir
  )

  json_path <- qual_write_bundle(x, bundle_dir)
  x$run$results_json_hash <- qual_hash_one(json_path)

  report_paths <- qual_write_report(x, bundle_dir, formats)
  x$run$formats_rendered <- report_paths
  x$run$hash_manifest <- if (identical(qual_hash_algorithm(), "sha256")) {
    "SHA256SUMS"
  } else {
    "MD5SUMS"
  }
  qual_write_summary_txt(x, bundle_dir)

  # results.json is rewritten once so that it records the hash of the report it
  # is bound to and the formats actually produced. Its own hash, quoted in the
  # report and in SUMMARY.txt, is the one taken before this rewrite.
  jsonlite::write_json(
    x$run, file.path(bundle_dir, "results", "run.json"),
    auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null"
  )
  # Order matters here and is the whole reason the manifest verifies. The log
  # is the last file still being written, so it is finished and closed BEFORE
  # its hash is taken. Hashing it first and then appending the closing lines
  # leaves a manifest that reports the bundle as altered on a run where nothing
  # was, and an auditor running the documented `sha256sum -c` would see a
  # failure produced by the tool itself.
  qual_log("hash", qual_hash_algorithm(), "manifest taken after this line")
  qual_log("run_end", verdict)
  qual_safe(close(log_con), NULL)

  hashes <- qual_hash_bundle(bundle_dir)
  x$files <- hashes$files

  if (!quiet) print(x)
  invisible(x)
}

qual_read_performance <- function(bundle_dir) {
  path <- file.path(bundle_dir, "results", "performance.csv")
  if (!file.exists(path)) {
    return(data.frame())
  }
  utils::read.csv(path, stringsAsFactors = FALSE)
}
