#!/usr/bin/env Rscript
#
# Cost of rule evaluation by rule type.
#
# The existing baseline uses only 3 simple rules and NO grouped rule, so it
# reports the rules axis at ~6% of runtime. This is a blind spot: rule cost
# scales with the number of rules, and grouped rules are evaluated per group
# with a full-width data frame copy per group, so their cost scales with group
# cardinality rather than with row count. This benchmark makes rule cost
# visible and attributes it BY RULE TYPE, so that optimisation work can be
# judged against a baseline.
#
# Usage:
#   Rscript benchmarks/bench_rules.R
#   Rscript benchmarks/bench_rules.R --rows 10000,50000 --groups 500,2000
#   Rscript benchmarks/bench_rules.R --rows 5000 --groups 500 --out benchmarks/rules_test.csv
#   Rscript benchmarks/bench_rules.R --violation-rate 0.02
#
# Not part of the package: benchmarks/ is in .Rbuildignore.

suppressMessages(pkgload::load_all(quiet = TRUE))

# ---- arguments --------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(flag, default) {
  hit <- which(args == flag)
  if (length(hit) == 0 || hit[[1]] == length(args)) {
    return(default)
  }
  args[[hit[[1]] + 1]]
}

row_counts <- as.numeric(strsplit(arg_value("--rows", "50000,200000"), ",")[[1]])
group_cardinalities <- as.numeric(strsplit(arg_value("--groups", "5000,20000"), ",")[[1]])
out_path <- arg_value("--out", "benchmarks/rules.csv")
violation_rate <- as.numeric(arg_value("--violation-rate", "0.01"))

# ---- fixture generation -----------------------------------------------------

# Clinical-shaped frame with deliberately rule-heavy structure.
# Group cardinality (number of DISTINCT subjects) is separate from row count
# to stress test the grouped rule path.
#
# Real DTA/DTS transfer data violates rarely: a well-behaved dataset has few
# or no violations. Sampling GFREASND/GFSTAT/GFORRES uniformly at random per
# ROW (as this fixture used to) makes 12-22% of GROUPS violate the grouped
# rules, which is unrepresentative and means the "group_condition" timing is
# dominated by violation-MESSAGE construction rather than by rule EVALUATION.
# Instead, values for the columns the grouped rules read are constructed to be
# GROUP-CONSISTENT (i.e. clean by construction for almost every group), and
# only `violation_rate` of groups are then deliberately corrupted -- this
# matches the regime the lazy-message optimisation is meant for.
make_frame <- function(n_rows, n_groups, violation_rate = 0.01) {
  set.seed(42)


  # SUBJIDN: n_rows rows drawn from n_groups distinct subjects
  subj_ids <- sprintf("S%07d", seq_len(n_groups))
  subj_sample <- sample(subj_ids, n_rows, replace = TRUE)

  # VISIT: sampled from 8 visits
  visits <- sprintf("V%02d", 1:8)
  visit_sample <- sample(visits, n_rows, replace = TRUE)

  # AGE: character holding mostly numbers but ~1% unconvertible
  # (these are expensive to coerce)
  age_vals <- sample(c(
    as.character(sample(18:75, floor(0.99 * n_rows), replace = TRUE)),
    c("ninety", "N/A", ">65", "unknown", "<18")
  ), n_rows, replace = TRUE)

  # GFREFID: identifier
  gfrefid_sample <- sprintf("R%05d", sample.int(99999, n_rows, replace = TRUE))

  # ---- group-consistent GFSTAT / GFREASND / GFORRES ------------------------
  #
  # The two grouped rules read these columns:
  #   * "status_result_by_visit"          groups by SUBJIDN + VISIT
  #   * "per_subject_visit_cardinality"   groups by SUBJIDN
  #
  # Values are constructed per subj-visit group so that almost every group is
  # clean by construction, and only `violation_rate` of groups are
  # deliberately corrupted to violate one of the two rules.
  group_key <- paste(subj_sample, visit_sample, sep = "\u001f")
  group_idx <- split(seq_len(n_rows), group_key)
  n_sv_groups <- length(group_idx)

  # Per subj-visit group scenario:
  #  - "clean_no_failure": GFREASND empty throughout -> trivially satisfies
  #    both constraints on "status_result_by_visit".
  #  - "clean_failure": one row properly reports a failure (GFREASND =
  #    "FAILED", that row's GFSTAT = "NOT DONE", no GFORRES reported anywhere
  #    in the group) -- satisfies both constraints without being vacuous.
  #  - "violate_mutex": one row reports a failure while another (or the same,
  #    when the group has only one row) reports a result -> breaks the
  #    mutually_exclusive constraint.
  #  - "violate_requires": a failure is reported but no row in the group has
  #    GFSTAT == "NOT DONE" -> breaks the requires constraint.
  clean_failure_rate <- 0.15
  scenario_probs <- c(
    clean_no_failure = max(0, 1 - violation_rate - clean_failure_rate),
    clean_failure = clean_failure_rate,
    violate_mutex = violation_rate / 2,
    violate_requires = violation_rate / 2
  )
  sv_scenario <- sample(
    names(scenario_probs), n_sv_groups,
    replace = TRUE, prob = scenario_probs
  )
  names(sv_scenario) <- names(group_idx)

  GFSTAT <- character(n_rows)
  GFREASND <- character(n_rows)
  GFORRES <- character(n_rows)
  subj_has_failure <- stats::setNames(logical(n_groups), subj_ids)

  for (gname in names(group_idx)) {
    rows <- group_idx[[gname]]
    n <- length(rows)
    subj <- subj_sample[rows[1]]
    scenario <- sv_scenario[[gname]]

    stat <- sample(c("DONE", "NOT DONE"), n, replace = TRUE)
    reasnd <- rep("", n)
    orres <- character(n)
    has_val <- sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.4, 0.6))
    orres[has_val] <- as.character(round(stats::runif(sum(has_val)) * 1000, 2))

    if (identical(scenario, "clean_failure")) {
      fail_row <- sample.int(n, 1)
      reasnd[fail_row] <- "FAILED"
      stat[fail_row] <- "NOT DONE"
      orres[] <- "" # no reporting anywhere in this group -> mutex stays clean
      subj_has_failure[[subj]] <- TRUE
    } else if (identical(scenario, "violate_mutex")) {
      fail_row <- sample.int(n, 1)
      reasnd[fail_row] <- "FAILED"
      report_row <- if (n > 1) sample(setdiff(seq_len(n), fail_row), 1) else fail_row
      orres[report_row] <- as.character(round(stats::runif(1) * 1000, 2))
      stat[fail_row] <- "NOT DONE"
      subj_has_failure[[subj]] <- TRUE
    } else if (identical(scenario, "violate_requires")) {
      fail_row <- sample.int(n, 1)
      reasnd[fail_row] <- "FAILED"
      stat[] <- "DONE" # no row is "NOT DONE" -> requires constraint breaks
      orres[] <- ""
      subj_has_failure[[subj]] <- TRUE
    }
    # "clean_no_failure": reasnd stays empty for every row -> trivially clean.

    GFSTAT[rows] <- stat
    GFREASND[rows] <- reasnd
    GFORRES[rows] <- orres
  }

  # "per_subject_visit_cardinality" (group_by = SUBJIDN, requires: any row
  # with GFREASND non-empty => any row in the SAME SUBJECT (any visit) with
  # GFSTAT == "DONE"). For every subject that has a failure anywhere, a DONE
  # row is guaranteed to exist elsewhere in the subject, UNLESS that subject
  # is chosen (at `violation_rate`) to deliberately violate this rule too.
  subjects_with_failure <- names(subj_has_failure)[subj_has_failure]
  n_fail_subj <- length(subjects_with_failure)
  violate_rule2 <- stats::setNames(logical(0), character(0))
  if (n_fail_subj > 0) {
    violate_rule2 <- stats::setNames(
      sample(
        c(TRUE, FALSE), n_fail_subj,
        replace = TRUE, prob = c(violation_rate, 1 - violation_rate)
      ),
      subjects_with_failure
    )
    for (subj in subjects_with_failure) {
      subj_rows <- which(subj_sample == subj)
      if (isTRUE(violate_rule2[[subj]])) {
        GFSTAT[subj_rows] <- "NOT DONE"
      } else if (!any(GFSTAT[subj_rows] == "DONE")) {
        GFSTAT[subj_rows[1]] <- "DONE"
      }
    }
  }

  # Base frame with the semantically important columns
  base <- data.frame(
    SUBJIDN = subj_sample,
    VISIT = visit_sample,
    GFREFID = gfrefid_sample,
    AGE = age_vals,
    GFSTAT = GFSTAT,
    GFREASND = GFREASND,
    GFORRES = GFORRES,
    stringsAsFactors = FALSE
  )

  # Filler columns to make the frame realistically wide.
  # Grouped rule path copies ALL columns per group, so width matters.
  n_filler <- 20 - ncol(base)
  if (n_filler > 0) {
    filler <- lapply(seq_len(n_filler), function(i) {
      if (i %% 2 == 0) {
        sprintf("TXT%05d", sample.int(99999, n_rows, replace = TRUE))
      } else {
        as.character(round(stats::runif(n_rows) * 1000, 3))
      }
    })
    names(filler) <- sprintf("FILL%02d", seq_len(n_filler))
    base <- cbind(base, as.data.frame(filler, stringsAsFactors = FALSE))
  }

  # Measured (realised) violation fractions, attached so the caller can print
  # them -- the benchmark is meant to be self-documenting about the regime it
  # actually measured, since small-group edge cases (a subj-visit group of
  # size 1 cannot violate the mutex constraint at all) mean the realised
  # fraction is not identical to the requested `violation_rate`. Attached
  # after `cbind()`, which does not reliably preserve custom attributes.
  attr(base, "measured_violation_fractions") <- list(
    status_result_by_visit = mean(sv_scenario %in% c("violate_mutex", "violate_requires")),
    per_subject_visit_cardinality = if (n_fail_subj > 0) mean(violate_rule2) else 0
  )

  base
}

make_specs <- function(frame) {
  # Column specs for the semantically important columns
  cols <- list(
    DTAColumnSpec(
      id = "SUBJIDN", type = "SAS Char", length = 8,
      nullable = FALSE, pattern = "^S[0-9]{7}$"
    ),
    DTAColumnSpec(
      id = "VISIT", type = "SAS Char", length = 3,
      nullable = FALSE, values = sprintf("V%02d", 1:8)
    ),
    DTAColumnSpec(
      id = "GFREFID", type = "SAS Char", length = 5,
      nullable = FALSE
    ),
    DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE),
    DTAColumnSpec(
      id = "GFSTAT", type = "SAS Char", length = 8,
      nullable = FALSE, values = c("DONE", "NOT DONE")
    ),
    DTAColumnSpec(id = "GFREASND", type = "SAS Char", length = 10, nullable = TRUE),
    DTAColumnSpec(id = "GFORRES", type = "SAS Num", nullable = TRUE)
  )

  # Filler column specs
  filler_names <- setdiff(names(frame), vapply(cols, function(x) x@id, character(1)))
  filler_specs <- lapply(filler_names, function(nm) {
    DTAColumnSpec(id = nm, type = "SAS Char", length = 12, nullable = TRUE)
  })

  all_cols <- c(cols, filler_specs)

  # Rule set is deliberately rule-heavy.
  rules <- list(
    # Range rules (3-5 on numeric-ish columns)
    DTARuleColRange(
      id = "age_valid_range",
      columns = "AGE",
      range = c(18, 100)
    ),
    DTARuleColRange(
      id = "gforres_positive",
      columns = "GFORRES",
      min = 0,
      max = 1000
    ),
    DTARuleColRange(
      id = "age_adult",
      columns = "AGE",
      range = c(18, 75)
    ),

    # Conditional rules (3-5, including ones with min/max and multiple operators)
    DTARuleColCondition(
      id = "age_and_status_combo",
      condition = list(AGE = list(greater_equal = 65)),
      then = list(GFSTAT = list(equals = "DONE"))
    ),
    DTARuleColCondition(
      id = "reason_implies_result",
      condition = list(GFREASND = list(empty = FALSE)),
      then = list(GFORRES = list(empty = TRUE))
    ),
    DTARuleColCondition(
      id = "complex_condition_multi_op",
      condition = list(VISIT = list(`in` = c("V01", "V02", "V03"))),
      then = list(AGE = list(range = c(18, 65)), GFSTAT = list(not_equals = ""))
    ),

    # Unique rules (2: single-column and multi-column)
    DTARuleColUnique(
      id = "subj_visit_unique",
      columns = c("SUBJIDN", "VISIT")
    ),
    DTARuleColUnique(
      id = "gfrefid_unique",
      columns = "GFREFID"
    ),

    # Grouped condition rules (2: different group_by, each with constraints)
    DTARuleGroupCondition(
      id = "status_result_by_visit",
      group_by = c("SUBJIDN", "VISIT"),
      conditions = list(
        c_failed = list(GFREASND = list(empty = FALSE)),
        c_reported = list(GFREASND = list(empty = TRUE), GFORRES = list(empty = FALSE)),
        c_not_done = list(GFSTAT = list(equals = "NOT DONE"))
      ),
      constraints = list(
        list(type = "mutually_exclusive", left = "c_failed", right = "c_reported"),
        list(type = "requires", `if` = "c_failed", then = "c_not_done")
      )
    ),
    DTARuleGroupCondition(
      id = "per_subject_visit_cardinality",
      group_by = "SUBJIDN",
      conditions = list(
        c_has_failed = list(GFREASND = list(empty = FALSE)),
        c_has_success = list(GFSTAT = list(equals = "DONE"))
      ),
      constraints = list(
        list(type = "requires", `if` = "c_has_failed", then = "c_has_success")
      )
    )
  )

  DTAColumnSpecCollection(
    columns = stats::setNames(
      all_cols,
      vapply(all_cols, function(x) x@id, character(1))
    ),
    rules = rules
  )
}

# ---- measurement helpers ---------------------------------------------------

# Elapsed seconds plus peak R heap over the call.
# gc(reset = TRUE) sets the recorded maximum back to current usage; the gc()
# after the call therefore reports the peak reached *during* it. Column 6 is
# "max used (Mb)", summed over the Ncells and Vcells rows.
timed <- function(label, expr) {
  invisible(gc(reset = TRUE, full = TRUE))
  t0 <- proc.time()[["elapsed"]]
  value <- force(expr)
  elapsed <- proc.time()[["elapsed"]] - t0
  peak_mb <- sum(gc(full = TRUE)[, 6])
  list(
    label = label,
    seconds = elapsed,
    peak_mb = peak_mb,
    value = value
  )
}

# ---- run benchmarks --------------------------------------------------------

message("DTAtools rule evaluation benchmark")
message(sprintf("R %s", getRversion()))
message(sprintf("requested violation rate: %s", violation_rate))
message("")

measured_fractions <- list()

results <- do.call(rbind, lapply(row_counts, function(n_rows) {
  do.call(rbind, lapply(group_cardinalities, function(n_groups) {
    message(sprintf(
      "n_rows = %s, n_groups = %s",
      format(n_rows, big.mark = ","),
      format(n_groups, big.mark = ",")
    ))

    frame <- make_frame(n_rows, n_groups, violation_rate = violation_rate)
    fractions <- attr(frame, "measured_violation_fractions")
    combo_key <- sprintf("rows=%s,groups=%s", n_rows, n_groups)
    measured_fractions[[combo_key]] <<- fractions
    message(sprintf(
      "  measured violating-group fraction: status_result_by_visit = %.4f, per_subject_visit_cardinality = %.4f",
      fractions$status_result_by_visit,
      fractions$per_subject_visit_cardinality
    ))

    specs <- make_specs(frame)
    n_cols <- ncol(frame)

    # All rules together
    all_rules_result <- timed("rules_all", {
      apply_rules(specs@rules, frame, verbose = FALSE)
    })

    # Range rules only
    range_rules <- Filter(function(r) methods::is(r, "DTAtools::DTARuleColRange"), specs@rules)
    range_result <- timed("range", {
      apply_rules(range_rules, frame, verbose = FALSE)
    })

    # Conditional rules only
    cond_rules <- Filter(function(r) methods::is(r, "DTAtools::DTARuleColCondition"), specs@rules)
    cond_result <- timed("col_condition", {
      apply_rules(cond_rules, frame, verbose = FALSE)
    })

    # Unique rules only
    unique_rules <- Filter(function(r) methods::is(r, "DTAtools::DTARuleColUnique"), specs@rules)
    unique_result <- timed("unique", {
      apply_rules(unique_rules, frame, verbose = FALSE)
    })

    # Grouped condition rules only
    group_rules <- Filter(function(r) methods::is(r, "DTAtools::DTARuleGroupCondition"), specs@rules)
    group_result <- timed("group_condition", {
      apply_rules(group_rules, frame, verbose = FALSE)
    })

    stages <- list(all_rules_result, range_result, cond_result, unique_result, group_result)

    out <- data.frame(
      n_rows = n_rows,
      n_cols = n_cols,
      n_groups = n_groups,
      stage = vapply(stages, function(s) s$label, character(1)),
      seconds = round(vapply(stages, function(s) s$seconds, numeric(1)), 3),
      peak_mb = round(vapply(stages, function(s) s$peak_mb, numeric(1)), 1),
      stringsAsFactors = FALSE
    )

    print(out[, c("stage", "seconds", "peak_mb")], row.names = FALSE)
    message("")

    out
  }))
}))

# ---- summary ----------------------------------------------------------------

message(strrep("-", 70))
message("share of total rule time by rule type, largest input")

largest <- results[results$n_rows == max(results$n_rows) & results$n_groups == max(results$n_groups), ]
# rules_all is the sum of the others, so exclude it from the share calculation
rules_breakdown <- largest[largest$stage != "rules_all", ]
total_rules_time <- largest[largest$stage == "rules_all", "seconds"]

rules_breakdown$share <- round(100 * rules_breakdown$seconds / total_rules_time, 1)
rules_breakdown_sorted <- rules_breakdown[order(-rules_breakdown$share), ]
print(rules_breakdown_sorted[, c("stage", "seconds", "share")], row.names = FALSE)

message("")
message(strrep("-", 70))
message("measured violating-group fractions by (n_rows, n_groups)")
fraction_rows <- do.call(rbind, lapply(names(measured_fractions), function(key) {
  f <- measured_fractions[[key]]
  data.frame(
    combo = key,
    status_result_by_visit = round(f$status_result_by_visit, 4),
    per_subject_visit_cardinality = round(f$per_subject_visit_cardinality, 4),
    stringsAsFactors = FALSE
  )
}))
print(fraction_rows, row.names = FALSE)

message("")
message(strrep("-", 70))

utils::write.csv(results, out_path, row.names = FALSE)
message(sprintf("wrote %s", out_path))
