# Fixture generation with a closed-form expected result.
#
# The point of this file is that `qa_oracle()` never asks the software what the
# answer is. Defects are injected at positions this code chooses, one kind per
# row, and the expected error counts follow arithmetically from the injection
# plan. That is what makes "correctness at scale" a real claim: ten million
# rows must produce exactly the same small set of errors as ten thousand,
# because the same defects were placed in both.
#
# The defect counts are therefore constant across tiers. Only the amount of
# clean data around them grows.

# ---- the specification ------------------------------------------------------

# Shaped like a real transfer: a handful of constrained columns that the checks
# actually read, padded with filler so the row width, and therefore the reading
# cost, is realistic.
qa_specs <- function(n_filler = 7L) {
  columns <- list(
    SUBJID = DTAColumnSpec(
      id = "SUBJID", type = "SAS Char", length = 8, nullable = FALSE,
      pattern = "^S[0-9]{7}$"
    ),
    SEX = DTAColumnSpec(
      id = "SEX", type = "SAS Char", length = 1, nullable = FALSE,
      values = c("M", "F")
    ),
    DOMAIN = DTAColumnSpec(
      id = "DOMAIN", type = "SAS Char", length = 2, nullable = FALSE,
      values = "GF"
    ),
    AGE = DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE),
    VISIT = DTAColumnSpec(id = "VISIT", type = "SAS Char", length = 3, nullable = FALSE)
  )
  if (n_filler > 0) {
    filler <- lapply(seq_len(n_filler), function(i) {
      DTAColumnSpec(
        id = sprintf("FILL%02d", i), type = "SAS Char", length = 12,
        nullable = TRUE
      )
    })
    names(filler) <- vapply(filler, function(x) x@id, character(1))
    columns <- c(columns, filler)
  }
  DTAColumnSpecCollection(
    columns = columns,
    rules = list(
      DTARuleColRange(id = "age_range", columns = "AGE", range = c(18, 70)),
      DTARuleColUnique(id = "subjid_unique", columns = "SUBJID"),
      DTARuleColCondition(
        id = "adult_visit",
        condition = list(AGE = list(greater_equal = 18)),
        then = list(VISIT = list(not_equals = "V00"))
      )
    )
  )
}

# ---- the injection plan -----------------------------------------------------

# How many of each defect to inject. Constant across tiers on purpose: see the
# note at the top of the file.
qa_defects <- function() {
  c(
    sex_enum = 3L,
    subjid_long = 1L,
    subjid_pattern = 2L,
    subjid_na = 1L,
    domain_const = 1L,
    age_text = 2L,
    subjid_dup = 2L,
    age_range = 4L,
    visit_v00 = 3L
  )
}

# Positions are spread across the whole file rather than clustered, so that at
# any batch size the defects fall in different batches and the boundary
# behaviour of the streaming engine is exercised rather than assumed. One row
# carries at most one injected defect, which is what keeps the oracle additive.
qa_defect_plan <- function(n_rows, defects = qa_defects()) {
  total <- sum(defects)
  if (n_rows < total + 2L) {
    cli::cli_abort(
      "n_rows must be at least {total + 2} to hold {total} disjoint defects."
    )
  }
  slots <- unique(round(seq(2, n_rows - 1, length.out = total)))
  if (length(slots) < total) {
    cli::cli_abort("Could not place {total} defects in {n_rows} rows.")
  }
  positions <- list()
  taken <- 0L
  for (kind in names(defects)) {
    n <- defects[[kind]]
    positions[[kind]] <- slots[seq.int(taken + 1L, taken + n)]
    taken <- taken + n
  }
  positions
}

# ---- the expected result ----------------------------------------------------

# Derived from the plan by arithmetic, never from a run.
#
# Two subtleties are encoded here rather than discovered later. A value that is
# too long for its column also fails the pattern, so one injected long id
# produces two column-specification errors. And the rule axis counts rules
# violated, not rows violating them, so each of the three rules contributes at
# most one error however many rows break it.
qa_oracle <- function(defects = qa_defects()) {
  columnspec <- c(
    SEX_enum = defects[["sex_enum"]],
    SUBJID_maxLength = defects[["subjid_long"]],
    SUBJID_pattern = defects[["subjid_long"]] + defects[["subjid_pattern"]],
    SUBJID_type = defects[["subjid_na"]],
    DOMAIN_const = defects[["domain_const"]]
  )
  # `age_text` is deliberately absent from this line. A value the reader cannot
  # parse is reported on the import axis only; the range rule reading that
  # column treats it as missing rather than as a violation, so one bad cell is
  # never counted on two axes.
  rules <- c(
    age_range = as.integer(defects[["age_range"]] > 0),
    subjid_unique = as.integer(defects[["subjid_dup"]] > 0),
    adult_visit = as.integer(defects[["visit_v00"]] > 0)
  )
  list(
    n_columnspec_errors = sum(columnspec),
    n_rule_errors = sum(rules),
    n_import_errors = defects[["age_text"]],
    by_keyword = columnspec,
    by_rule = rules,
    ok = FALSE
  )
}

# The same expectation in the shape `qa_counts()` returns, so a run can be
# compared with it directly.
qa_oracle_counts <- function(target, defects = qa_defects()) {
  expected <- qa_oracle(defects)
  parts <- strsplit(names(expected$by_keyword), "_", fixed = TRUE)
  cs <- data.frame(
    source = "columnspec",
    target = target,
    keyword = vapply(parts, function(p) paste(p[-1], collapse = "_"), character(1)),
    n = as.integer(expected$by_keyword),
    stringsAsFactors = FALSE
  )
  cs$column <- vapply(parts, `[[`, character(1), 1)
  cs <- stats::aggregate(list(n = cs$n), by = cs[, c("source", "target", "keyword")], FUN = sum)
  rows <- rbind(
    cs,
    data.frame(
      source = "import", target = target, keyword = "not_convertible",
      n = as.integer(expected$n_import_errors), stringsAsFactors = FALSE
    ),
    data.frame(
      source = "rule", target = target, keyword = "",
      n = as.integer(expected$n_rule_errors), stringsAsFactors = FALSE
    )
  )
  rows <- rows[rows$n > 0, , drop = FALSE]
  rows <- rows[order(rows$source, rows$target, rows$keyword), , drop = FALSE]
  rownames(rows) <- NULL
  rows
}

# ---- the data ---------------------------------------------------------------

# Every column is generated as text, because that is what a CSV holds and the
# reader is part of what is being qualified. `offset` lets a large file be
# produced in chunks without the whole thing ever being in memory; the subject
# ids stay unique across chunks because they are derived from the absolute row
# number.
qa_frame <- function(n_rows, seed = 1L, n_filler = 7L,
                     plan = qa_defect_plan(n_rows), offset = 0L,
                     n_total = n_rows) {
  set.seed(seed + offset)
  index <- seq_len(n_rows) + offset

  frame <- data.frame(
    SUBJID = sprintf("S%07d", index),
    SEX = rep(c("M", "F"), length.out = n_rows),
    DOMAIN = rep("GF", n_rows),
    AGE = as.character(rep(c(18, 25, 40, 55, 70), length.out = n_rows)),
    VISIT = rep(sprintf("V%02d", 1:8), length.out = n_rows),
    stringsAsFactors = FALSE
  )
  if (n_filler > 0) {
    for (i in seq_len(n_filler)) {
      frame[[sprintf("FILL%02d", i)]] <- sprintf("TXT%05d", (index * i) %% 99999L)
    }
  }

  # `plan` is in absolute row numbers; only those inside this chunk apply.
  local_row <- function(absolute) {
    hit <- absolute[absolute > offset & absolute <= offset + n_rows]
    hit - offset
  }
  at <- lapply(plan, local_row)

  frame$SEX[at$sex_enum] <- "X"
  frame$SUBJID[at$subjid_long] <- "THIS-SUBJID-IS-TOO-LONG"
  frame$SUBJID[at$subjid_pattern] <- "BADPAT1"
  frame$SUBJID[at$subjid_na] <- NA_character_
  frame$DOMAIN[at$domain_const] <- "ZZ"
  frame$AGE[at$age_text] <- "abc"
  frame$AGE[at$age_range] <- "99"
  # The condition rule only applies to adults, so the row has to be one.
  frame$VISIT[at$visit_v00] <- "V00"
  frame$AGE[at$visit_v00] <- "30"
  # Duplicated against the first row of the file, which every chunk can name
  # without holding it.
  frame$SUBJID[at$subjid_dup] <- "S0000001"

  frame
}

qa_write_csv <- function(frame, path, gzip = FALSE,
                         header_style = c("plain", "quoted", "padded"),
                         eol = c("\n", "\r\n"), na = "", quote_all = FALSE,
                         append = FALSE) {
  header_style <- match.arg(header_style)
  eol <- match.arg(eol)

  quote_field <- function(x) {
    x <- ifelse(is.na(x), na, as.character(x))
    if (quote_all || any(grepl('[,"\n]', x))) {
      paste0('"', gsub('"', '""', x), '"')
    } else {
      x
    }
  }
  header <- switch(header_style,
    plain = names(frame),
    quoted = paste0('"', names(frame), '"'),
    padded = paste0('" ', names(frame), ' "')
  )
  body <- do.call(paste, c(lapply(frame, quote_field), sep = ","))
  lines <- if (append) body else c(paste(header, collapse = ","), body)

  con <- if (gzip) gzfile(path, if (append) "ab" else "wb") else file(path, if (append) "ab" else "wb")
  on.exit(close(con), add = TRUE)
  writeChar(paste0(paste(lines, collapse = eol), eol), con, eos = NULL)
  path
}

# Written in chunks so that the largest tier never materialises in memory. The
# expected result is the same as for any other size, which is the whole point:
# see the note at the top of the file.
qa_write_large_csv <- function(n_rows, path, seed = 1L, n_filler = 7L,
                               chunk_rows = 1e6, gzip = FALSE) {
  plan <- qa_defect_plan(n_rows)
  starts <- seq(0, n_rows - 1, by = chunk_rows)
  for (i in seq_along(starts)) {
    offset <- starts[[i]]
    size <- min(chunk_rows, n_rows - offset)
    chunk <- qa_frame(
      size,
      seed = seed, n_filler = n_filler, plan = plan,
      offset = offset, n_total = n_rows
    )
    qa_write_csv(chunk, path, gzip = gzip, append = i > 1)
  }
  list(path = path, rows = n_rows, plan = plan, expected = qa_oracle())
}

# ---- assembling a dataset ---------------------------------------------------

qa_dta <- function(path, specs = qa_specs(), name = "ds", stream = "never") {
  ds <- DTADataSetTabular(
    name = name, specs = specs,
    files = list(DTAFileCSV(filename = basename(path)))
  )
  load_file(ds, file = path, handler_index = 1, stream = stream)
}
