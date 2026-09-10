# The Validation Summary Report.
#
# Assembled as Markdown in R rather than knitted from a template, for one
# practical reason: a target system may have no pandoc, no LaTeX and no
# rmarkdown, and the report is the deliverable. Markdown always gets written;
# HTML, Word and PDF are produced when the tooling for them happens to exist,
# and their absence is recorded in the report itself rather than failing a run
# that otherwise succeeded.
#
# The document is written to be read by someone who was not present when it
# ran: it states what was expected before it states what happened, it names the
# file and line of every step, and it never reports a conclusion the evidence
# files do not also contain.

# ---- small Markdown helpers -------------------------------------------------

qual_md_escape <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  gsub("\\|", "\\\\|", gsub("[\r\n]+", " ", x))
}

qual_md_table <- function(df, align = NULL) {
  if (is.null(df) || nrow(df) == 0) {
    return("_No entries._")
  }
  cols <- names(df)
  body <- vapply(seq_len(nrow(df)), function(i) {
    paste0("| ", paste(vapply(cols, function(cn) {
      qual_md_escape(df[[cn]][[i]])
    }, character(1)), collapse = " | "), " |")
  }, character(1))
  c(
    paste0("| ", paste(qual_md_escape(cols), collapse = " | "), " |"),
    paste0("| ", paste(rep(align %||% "---", length(cols)), collapse = " | "), " |"),
    body
  )
}

qual_md_kv <- function(pairs) {
  qual_md_table(data.frame(
    Item = names(pairs),
    Value = vapply(pairs, function(x) paste(format(x), collapse = ", "), character(1)),
    stringsAsFactors = FALSE
  ))
}

qual_truncate <- function(x, n = 120) {
  x <- as.character(x)
  ifelse(is.na(x), "", ifelse(nchar(x) > n, paste0(substr(x, 1, n - 3), "..."), x))
}

# ---- the report -------------------------------------------------------------

qual_report_md <- function(x) {
  env <- x$environment
  coverage <- attr(x$traceability, "requirements")
  algo <- toupper(x$run$hash_algorithm)

  out <- c(
    "# DTAtools Software Qualification Report",
    "",
    sprintf("**Verdict: %s**", x$verdict),
    "",
    qual_md_kv(stats::setNames(
      list(
        x$run$id,
        sprintf("DTAtools %s", env$package$version),
        env$package$sha,
        env$package$library_path,
        env$package$loaded_via,
        x$run$tester,
        env$system$hostname,
        x$run$started_at,
        x$run$finished_at,
        paste(x$run$stages_run, collapse = ", "),
        x$run$scale,
        x$run$filter,
        x$run$results_json_hash %||% NA_character_
      ),
      c(
        "Run identifier", "Package", "Source revision", "Installed at",
        "Loaded from", "Executed by", "Host", "Started", "Finished",
        "Stages", "Scale tier", "Filter",
        sprintf("%s of results.json", algo)
      )
    )),
    "",
    "## 1. Approval",
    "",
    "This report records the qualification of the software identified above on",
    "the system identified above. It is complete only when signed.",
    "",
    qual_md_table(data.frame(
      Role = c("Performed by", "Reviewed by", "Approved by"),
      Name = c(x$run$tester, x$run$reviewer %||% "", ""),
      Signature = c("", "", ""),
      Date = c("", "", ""),
      stringsAsFactors = FALSE
    )),
    "",
    "## 2. Purpose, scope and approach",
    "",
    qual_report_approach(x),
    "",
    "## 3. Installed system and environment",
    "",
    qual_md_kv(list(
      "R" = env$r$version,
      "Platform" = env$r$platform,
      "Operating system" = env$r$os,
      "Locale" = env$system$locale,
      "Time zone" = env$system$timezone,
      "Character encoding" = env$system$encoding,
      "Arrow" = sprintf(
        "%s (dataset: %s, acero: %s, parquet: %s)",
        env$arrow$version, env$arrow$dataset, env$arrow$acero, env$arrow$parquet
      ),
      "Installed-file checksums" = env$package$md5_check,
      "pandoc" = if (isTRUE(env$tooling$pandoc)) env$tooling$pandoc_version else "not available",
      "PDF backend" = env$tooling$pdf_backend,
      "Browser for app tests" = env$tooling$browser
    )),
    "",
    "### 3.1 Declared dependencies",
    "",
    qual_md_table(env$dependencies),
    "",
    "### 3.2 Package options in force",
    "",
    qual_md_kv(env$options),
    ""
  )

  for (stage in x$run$stages_run) {
    out <- c(out, qual_report_stage(x, stage), "")
  }

  out <- c(
    out,
    "## 7. Requirements traceability",
    "",
    sprintf(
      "%d requirements; %d verified, %d failed, %d not verified, %d uncovered.",
      x$summary$n_requirements, x$summary$n_verified, x$summary$n_failed,
      x$summary$n_not_verified + x$summary$n_not_executed, x$summary$n_uncovered
    ),
    "",
    qual_md_table(qual_report_trace_table(x, coverage)),
    "",
    "## 8. Deviations",
    "",
    "### 8.1 Known deviations reproduced by this run",
    "",
    "A registered defect that no longer reproduces fails the run: the register",
    "has stopped describing the software and must be updated. That obligation",
    "applies to the entries whose state is `open`. An entry marked `pending` is",
    "one this suite documents but does not itself reproduce, so its result in",
    "the run column carries no verdict either way.",
    "",
    qual_md_table(qual_report_dev_table(x)),
    "",
    "### 8.2 New failures observed in this run",
    "",
    qual_md_table(qual_report_failures(x)),
    "",
    "### 8.3 Documented limitations",
    "",
    qual_md_table(x$limitations[, c("id", "title", "consequence"), drop = FALSE]),
    "",
    "## 9. Tests not executed",
    "",
    qual_md_table(qual_report_skips(x)),
    "",
    "## 10. Meta-consistency checks",
    "",
    "These check the suite itself: that every test is traced, every requirement",
    "is exercised, every exported function is claimed by a requirement, and that",
    "no expected value was taken from a stored snapshot of the software's own",
    "output.",
    "",
    qual_md_table(x$meta_checks),
    "",
    "## 11. Developer test evidence",
    "",
    qual_report_unit(x),
    "",
    "## 12. Evidence inventory",
    "",
    sprintf(
      "Every file below is covered by `%s`, written after the run finished.",
      x$run$hash_manifest %||% "the hash manifest"
    ),
    sprintf("Verify with `%ssum -c %s` from the bundle directory.", x$run$hash_algorithm, x$run$hash_manifest %||% ""),
    "",
    "## 13. Appendix: requirement texts",
    "",
    qual_md_table(x$requirements[, c("id", "risk", "category", "text"), drop = FALSE]),
    ""
  )
  out
}

qual_report_approach <- function(x) {
  c(
    "The software under qualification is a custom application that validates",
    "tabular data transfers against written Data Transfer Specifications. It is",
    "treated as GAMP 5 category 5, and is qualified in three stages.",
    "",
    "- **Installation qualification (IQ)** records what is installed on this",
    "  system and checks it against what should be: version, build, dependency",
    "  versions, installed-file checksums and the capabilities of the runtime.",
    "- **Operational qualification (OQ)** exercises each documented behaviour",
    "  against expected values written before execution, one requirement at a",
    "  time.",
    "- **Performance qualification (PQ)** runs end-to-end workflows on realistic",
    "  data, including at scale, and records timing and memory behaviour.",
    "",
    sprintf(
      "This run executed %s at the %s scale tier.",
      paste(x$run$stages_run, collapse = ", "), x$run$scale
    ),
    # Each stage owns a fixed section number, so that section 5 means the same
    # thing in every report ever produced. A stage that did not run therefore
    # leaves a gap in the numbering rather than shifting everything after it.
    if (length(x$run$stages_run) < length(qual_stages())) {
      sprintf(
        paste(
          "Sections are numbered by stage (4 installation, 5 operational,",
          "6 performance), so the section%s for the stage%s not run in this",
          "run (%s) %s absent rather than renumbered."
        ),
        if (length(qual_stages()) - length(x$run$stages_run) > 1) "s" else "",
        if (length(qual_stages()) - length(x$run$stages_run) > 1) "s" else "",
        paste(setdiff(qual_stages(), x$run$stages_run), collapse = ", "),
        if (length(qual_stages()) - length(x$run$stages_run) > 1) "are" else "is"
      )
    } else {
      "All three stages ran, so sections 4, 5 and 6 are all present."
    },
    if (isTRUE(x$run$partial)) {
      paste(
        "This run was **partial**: a filter or a stage selection was applied, so",
        "it does not support a complete claim about the software."
      )
    } else {
      "No filter was applied, so the run covers the whole suite."
    },
    "",
    "Evidence is recorded as it is produced. Each test case states its expected",
    "value and the value observed, and each is anchored to the file and line of",
    "the code that produced it. Nothing in this report is asserted that the",
    "machine-readable evidence files do not also contain."
  )
}

qual_report_stage <- function(x, stage) {
  number <- switch(stage,
    IQ = "4",
    OQ = "5",
    PQ = "6"
  )
  title <- switch(stage,
    IQ = "Installation qualification",
    OQ = "Operational qualification",
    PQ = "Performance qualification"
  )
  rows <- x$tests[x$tests$stage == stage, , drop = FALSE]
  table <- data.frame(
    `Test case` = rows$tc_id,
    Title = qual_truncate(rows$title, 70),
    Requirements = rows$req_ids,
    Result = rows$status,
    Checks = sprintf("%d/%d", rows$n_pass, rows$n_expectations),
    Seconds = round(rows$real_sec, 2),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  out <- c(
    sprintf("## %s. %s (%s)", number, title, stage),
    "",
    sprintf(
      "%d test cases: %d passed, %d failed, %d errored, %d not executed.",
      nrow(rows), sum(rows$status == "pass"), sum(rows$status == "fail"),
      sum(rows$status %in% c("error", "file_error")), sum(rows$status == "skip")
    ),
    "",
    qual_md_table(table)
  )
  if (identical(stage, "PQ") && !is.null(x$performance) && nrow(x$performance) > 0) {
    out <- c(
      out, "",
      sprintf("### %s.1 Measured performance", number), "",
      qual_md_table(x$performance)
    )
  }
  detail <- qual_report_stage_detail(x, rows)
  if (length(detail)) {
    out <- c(out, "", sprintf("### %s.2 Steps requiring attention", number), "", detail)
  }
  out
}

# Only the steps a reviewer has to look at are expanded. A passing test case is
# adequately evidenced by its row in the table above and its entry in
# expectations.csv; reprinting every successful comparison would bury the ones
# that matter.
qual_report_stage_detail <- function(x, rows) {
  bad <- rows[rows$status %in% c("fail", "error", "file_error"), , drop = FALSE]
  if (nrow(bad) == 0) {
    return(character(0))
  }
  out <- character(0)
  for (i in seq_len(nrow(bad))) {
    exps <- x$expectations[
      x$expectations$tc_id == bad$tc_id[[i]] & x$expectations$type != "success", ,
      drop = FALSE
    ]
    out <- c(
      out,
      sprintf("**%s** (%s) - %s", bad$tc_id[[i]], bad$status[[i]], bad$title[[i]]),
      ""
    )
    for (j in seq_len(nrow(exps))) {
      out <- c(out, sprintf(
        "- `%s:%s` [%s] %s",
        exps$file[[j]], exps$line[[j]], exps$type[[j]], exps$message[[j]]
      ))
    }
    out <- c(out, "")
  }
  out
}

qual_report_trace_table <- function(x, coverage) {
  if (nrow(coverage) == 0) {
    return(coverage)
  }
  text <- x$requirements$text[match(coverage$req_id, x$requirements$id)]
  data.frame(
    Requirement = coverage$req_id,
    Risk = coverage$risk,
    Statement = qual_truncate(text, 90),
    `Test cases` = coverage$tc_ids,
    Status = coverage$status,
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

qual_report_dev_table <- function(x) {
  if (nrow(x$deviations) == 0) {
    return(x$deviations)
  }
  status <- x$deviation_status$status_run[match(x$deviations$id, x$deviation_status$id)]
  data.frame(
    Deviation = x$deviations$id,
    Title = qual_truncate(x$deviations$title, 70),
    Severity = x$deviations$severity,
    State = x$deviations$status,
    Closed = x$deviations$closed,
    `This run` = status,
    Affects = x$deviations$affects,
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

qual_report_failures <- function(x) {
  bad <- x$tests[x$tests$status %in% c("fail", "error", "file_error"), , drop = FALSE]
  if (nrow(bad) == 0) {
    return(NULL)
  }
  first <- vapply(bad$tc_id, function(id) {
    e <- x$expectations[x$expectations$tc_id == id & x$expectations$type != "success", , drop = FALSE]
    if (nrow(e)) sprintf("%s:%s %s", e$file[[1]], e$line[[1]], qual_truncate(e$message[[1]], 140)) else ""
  }, character(1))
  data.frame(
    `Test case` = bad$tc_id,
    Requirements = bad$req_ids,
    Result = bad$status,
    Evidence = unname(first),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

qual_report_skips <- function(x) {
  skipped <- x$tests[x$tests$status == "skip", , drop = FALSE]
  discovered <- x$discovered[
    x$discovered$ok & !(x$discovered$tc_id %in% x$tests$tc_id), ,
    drop = FALSE
  ]
  rows <- list()
  if (nrow(skipped)) {
    rows[[length(rows) + 1]] <- data.frame(
      `Test case` = skipped$tc_id, Requirements = skipped$req_ids,
      Reason = skipped$skip_reason, check.names = FALSE, stringsAsFactors = FALSE
    )
  }
  if (nrow(discovered)) {
    reason <- ifelse(
      discovered$stage %in% x$run$stages_run,
      if (is.na(x$run$filter)) "not executed" else "excluded by the run's filter",
      "the stage was not part of this run"
    )
    rows[[length(rows) + 1]] <- data.frame(
      `Test case` = discovered$tc_id, Requirements = discovered$req_ids,
      Reason = reason, check.names = FALSE, stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0) {
    return(NULL)
  }
  do.call(rbind, rows)
}

qual_report_unit <- function(x) {
  unit <- x$unit_tests
  if (!isTRUE(unit$run)) {
    return(c(
      "The developer test suite was not executed by this run.",
      "",
      sprintf("Reason: %s.", unit$reason),
      "",
      "It is supplementary evidence in any case: it demonstrates that the code",
      "was tested during development, not that this installation behaves as",
      "specified. That claim rests on the stages above."
    ))
  }
  s <- unit$summary
  head <- c(
    "The developer test suite was executed as supplementary evidence, in a",
    "separate R process with a time bound. It therefore cannot have influenced",
    "the stages above, and a developer test that blocks cannot stop this run.",
    "",
    "It is supplementary in what it shows, too: that the code was tested during",
    "development, not that this installation behaves as specified. The latter",
    "claim rests on the stages above, and nothing here contributes to the",
    "verdict.",
    "",
    qual_md_kv(list(
      "Location" = s$path,
      "Test blocks" = s$n_tests,
      "Assertions passed" = s$passed,
      "Assertions failed" = s$failed,
      "Errors" = s$errors,
      "Skipped" = s$skipped
    )),
    "",
    "The full per-test result is `results/unit-tests.csv`."
  )
  failures <- unit$failures
  if (is.null(failures) || nrow(failures) == 0) {
    return(head)
  }
  # A count with no names leaves the reviewer nowhere to go. These are named so
  # that each can be assessed -- a developer test can fail against an installed
  # library for reasons that say nothing about the installation, and the only
  # way to tell is to read the one that failed.
  c(
    head,
    "",
    sprintf(
      "%d developer assertion%s did not pass. They are listed here to be assessed, not because they change the verdict:",
      nrow(failures), if (nrow(failures) == 1) "" else "s"
    ),
    "",
    qual_md_table(data.frame(
      File = failures$file,
      Test = qual_truncate(failures$test, 60),
      Result = failures$type,
      Message = qual_truncate(failures$message, 150),
      check.names = FALSE, stringsAsFactors = FALSE
    ))
  )
}

# ---- rendering --------------------------------------------------------------

qual_write_report <- function(x, bundle_dir, formats) {
  md_path <- file.path(bundle_dir, "report", "qualification-report.md")
  writeLines(qual_report_md(x), md_path, useBytes = FALSE)
  qual_log("render", "md", md_path)
  rendered <- list(md = md_path, html = NA_character_, docx = NA_character_, pdf = NA_character_)

  wanted <- setdiff(formats, "md")
  if (length(wanted) == 0) {
    return(rendered)
  }
  if (!requireNamespace("rmarkdown", quietly = TRUE) ||
    !isTRUE(qual_safe(rmarkdown::pandoc_available(), FALSE))) {
    qual_log("render", "skipped", "pandoc is not available")
    return(rendered)
  }

  for (fmt in intersect(c("html", "docx"), wanted)) {
    out <- qual_safe(
      rmarkdown::render(
        md_path,
        output_format = if (identical(fmt, "html")) {
          rmarkdown::html_document(self_contained = TRUE, toc = TRUE, toc_depth = 2)
        } else {
          rmarkdown::word_document(toc = TRUE)
        },
        output_file = paste0("qualification-report.", fmt),
        quiet = TRUE
      ),
      NA_character_
    )
    rendered[[fmt]] <- out
    qual_log("render", fmt, out)
  }

  if ("pdf" %in% wanted) {
    rendered$pdf <- qual_render_pdf(md_path, rendered$docx)
    qual_log("render", "pdf", rendered$pdf %||% "not produced")
  }
  rendered
}

# Two routes to a PDF, because the evidence decides which one can work.
#
# Converting the Word document is preferred: it is the package's own path and
# it reproduces the document a reviewer already read. Where no office suite is
# installed that conversion goes through LaTeX, and the default engine cannot
# set a character outside its input encoding -- so one mathematical symbol in a
# rule's message, which is ordinary in a specification, silently costs the
# entire PDF. Rendering the Markdown through a Unicode-capable engine is the
# fallback for exactly that case, and a report that cannot be produced at all
# is recorded as absent rather than failing the run.
qual_render_pdf <- function(md_path, docx_path) {
  pdf <- file.path(dirname(md_path), "qualification-report.pdf")

  usable_docx <- !is.null(docx_path) && !is.na(docx_path) && file.exists(docx_path)
  if (usable_docx && !is.null(qual_safe(dta_pdf_backend(), NULL))) {
    via_docx <- qual_safe(
      {
        .convert_docx_to_pdf(docx_path, pdf)
        pdf
      },
      NA_character_
    )
    if (!is.na(via_docx) && file.exists(via_docx)) {
      return(via_docx)
    }
    qual_log("render", "pdf", "the Word conversion failed; trying a Unicode engine")
  }

  for (engine in c("xelatex", "lualatex")) {
    out <- qual_safe(
      rmarkdown::render(
        md_path,
        output_format = rmarkdown::pdf_document(latex_engine = engine),
        output_file = basename(pdf), quiet = TRUE
      ),
      NA_character_
    )
    if (!is.na(out) && file.exists(out)) {
      return(out)
    }
  }
  NA_character_
}
