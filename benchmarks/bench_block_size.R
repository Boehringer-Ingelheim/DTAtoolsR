#!/usr/bin/env Rscript
#
# What does a larger read block actually cost?
#
# `DTAtools.stream_block_size` is the one knob that moves a delimited scan's
# peak memory: `batch_rows` only slices a batch that is already larger, so on a
# delimited file the block IS the batch. The default was Arrow's own 1 MiB for
# as long as nobody had measured the trade, which left the choice resting on
# the phrase "a proportional cost in resident memory" -- true, but silent about
# the constant, and the constant is what decides whether a default is safe on a
# small machine.
#
# This benchmark measures the constant. It sweeps the block size over one
# fixture and reports, for each:
#
#   * rows per batch -- what the block buys, and whether a batch clears
#     `DTAtools.stream_arrow_numeric_min_rows` so the Arrow numeric parse can
#     engage at all;
#   * `arrow::default_memory_pool()$max_memory` -- the C++ read-ahead and
#     accumulator high-water mark, which is the honest memory figure;
#   * the process peak working set / RSS -- what the machine actually has to
#     find;
#   * elapsed seconds.
#
# EACH BLOCK SIZE RUNS IN A FRESH R PROCESS. Arrow's pool has no reset -- its
# `max_memory` is a per-process high-water mark -- and so is the OS peak
# working set. Measured in one process the second block size would inherit the
# first's peak and every row after the first would be a lie. This is why the
# script re-invokes itself rather than looping.
#
# Every replicate is written to benchmarks/block_size.csv, not a summary of
# them. The memory columns repeat exactly and need no replication to be
# believed; the elapsed column does not, and a claim about speed made from one
# run of each is how a benchmark comes to assert a difference smaller than its
# own noise. Anything quoted from this file elsewhere in the package should be
# checkable against the rows, which means the rows have to be there.
#
# Usage:
#   Rscript benchmarks/bench_block_size.R
#   Rscript benchmarks/bench_block_size.R --rows 2000000 --cols 60
#   Rscript benchmarks/bench_block_size.R --blocks 1,4,8,16 --reps 5
#
# The `--cols` sweep is worth running before trusting the default on a wide
# delivery: rows per batch is about the block divided by the width of a row, so
# a file at 2 KB a row gets a tenth of the batch this fixture does out of the
# same 8 MiB, and correspondingly less of the benefit for the same memory.

args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(flag, default) {
  hit <- which(args == flag)
  if (length(hit) == 0 || hit[[1]] == length(args)) {
    return(default)
  }
  args[[hit[[1]] + 1]]
}

n_rows <- as.integer(arg_value("--rows", "1000000"))
n_cols <- as.integer(arg_value("--cols", "20"))
batch_rows <- as.integer(arg_value("--batch", "131072"))
blocks_mib <- as.numeric(strsplit(arg_value("--blocks", "1,2,4,8,16,32,64"), ",")[[1]])
reps <- as.integer(arg_value("--reps", "3"))
fixture <- arg_value("--file", file.path(tempdir(), sprintf("block_bench_%d_%d.csv", n_rows, n_cols)))

# ---- fixture -----------------------------------------------------------------
# The same shape as bench_streaming.R: a dirty fraction producing real
# violations, a uniqueness rule and a grouped rule, so the accumulators that
# make up the pool's floor are actually exercised. A clean file would report a
# floor that no real check ever sees.

make_file <- function(path, n_rows, n_cols, dirty = 0.02) {
  if (file.exists(path)) {
    return(path)
  }
  set.seed(7)
  base <- data.frame(
    ID = sprintf("S%07d", seq_len(n_rows)),
    SUBJID = sprintf("SUBJ%010d", seq_len(n_rows)),
    SITE = sprintf("SITE%06d", ((seq_len(n_rows) - 1L) %% max(1L, n_rows %/% 50L)) + 1L),
    SEX = sample(c("M", "F"), n_rows, replace = TRUE),
    AGE = sample(18:70, n_rows, replace = TRUE),
    stringsAsFactors = FALSE
  )
  n_dirty <- max(1L, as.integer(n_rows * dirty))
  dirty_rows <- sample.int(n_rows, n_dirty, replace = FALSE)
  base$AGE[dirty_rows] <- sample(c(-1, 100), n_dirty, replace = TRUE)
  base$ID[dirty_rows] <- sprintf("S%010d", seq_len(n_dirty) + 10000L)

  n_filler <- max(0L, n_cols - ncol(base))
  if (n_filler > 0) {
    filler <- lapply(
      seq_len(n_filler),
      function(i) sprintf("TXT%05d", sample.int(99999, n_rows, TRUE))
    )
    names(filler) <- sprintf("FILL%02d", seq_len(n_filler))
    base <- cbind(base, as.data.frame(filler, stringsAsFactors = FALSE))
  }
  utils::write.csv(base, path, row.names = FALSE)
  path
}

make_specs <- function(path) {
  header <- names(utils::read.csv(path, nrows = 1))
  cols <- list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE),
    DTAColumnSpec(id = "SUBJID", type = "SAS Char", length = 13, nullable = FALSE),
    DTAColumnSpec(id = "SITE", type = "SAS Char", length = 10, nullable = FALSE),
    DTAColumnSpec(id = "SEX", type = "SAS Char", length = 1, nullable = FALSE, values = c("M", "F")),
    DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
  )
  filler <- setdiff(header, c("ID", "SUBJID", "SITE", "SEX", "AGE"))
  cols <- c(cols, lapply(filler, function(nm) {
    DTAColumnSpec(id = nm, type = "SAS Char", length = 12, nullable = TRUE)
  }))

  DTAColumnSpecCollection(
    columns = stats::setNames(cols, vapply(cols, function(x) x@id, character(1))),
    rules = list(
      DTARuleColRange(id = "age_range", columns = "AGE", range = c(18, 70)),
      DTARuleColUnique(id = "subjid_unique", columns = "SUBJID"),
      DTARuleGroupCondition(
        id = "site_age_extremes",
        group_by = "SITE",
        conditions = list(
          under_age = list(AGE = list(less = 18)),
          over_age = list(AGE = list(greater = 70))
        ),
        constraints = list(
          list(type = "mutually_exclusive", left = "under_age", right = "over_age")
        )
      )
    )
  )
}

mb <- function(x) round(as.numeric(x) / 1024^2, 1)

# ps reports `peak_wset` on Windows and current `rss` everywhere; the peak is
# the one worth having, so it is preferred where the platform offers it.
process_peak_mb <- function() {
  info <- tryCatch(ps::ps_memory_info(), error = function(e) NULL)
  if (is.null(info)) {
    return(NA_real_)
  }
  mb(if ("peak_wset" %in% names(info)) info[["peak_wset"]] else info[["rss"]])
}

process_rss_mb <- function() {
  info <- tryCatch(ps::ps_memory_info(), error = function(e) NULL)
  if (is.null(info)) NA_real_ else mb(info[["rss"]])
}

pool_peak_mb <- function() {
  tryCatch(mb(arrow::default_memory_pool()$max_memory), error = function(e) NA_real_)
}

# ---- one block size, in this process -----------------------------------------
# Reached only via the self-invocation below, never from the sweep driver.

if ("--one" %in% args) {
  suppressMessages(pkgload::load_all(quiet = TRUE))
  block <- as.numeric(arg_value("--one", NA))
  options(DTAtools.stream_block_size = block)

  path <- make_file(fixture, n_rows, n_cols)
  specs <- make_specs(path)

  # Taken before anything has scanned, so that what it is subtracted from is
  # the validation's peak and not the probe's. The probe therefore runs LAST,
  # after every memory figure has been read.
  before_peak <- process_peak_mb()
  invisible(gc(reset = TRUE, full = TRUE))

  started <- proc.time()[["elapsed"]]
  validate_file_stream(specs, path, batch_rows = batch_rows, verbose = FALSE)
  elapsed <- proc.time()[["elapsed"]] - started

  pool_mb <- pool_peak_mb()
  peak_mb <- process_peak_mb()
  rss_mb <- process_rss_mb()

  # What a batch IS at this block size. A Dataset says nothing about how it
  # will be batched until it is scanned, so this has to consume a reader.
  dataset <- dta_open_normalized_dataset(path, delim = ",", quote = "\"", has_header = TRUE)
  reader <- arrow::Scanner$create(dataset, batch_size = batch_rows)$ToRecordBatchReader()
  seen <- integer(0)
  repeat {
    batch <- reader$read_next_batch()
    if (is.null(batch)) break
    seen <- c(seen, batch$num_rows)
  }

  # The last batch is whatever was left over, so it says nothing about the
  # block; including it would report a file as failing the numeric gate on the
  # strength of its remainder alone. BOTH ends of the full-batch range are
  # reported: the minimum answers "does every batch clear the gate", the
  # maximum answers "does any batch clear it", and the two are different
  # questions -- at 4 MiB on the reference file neither end reaches 20,000,
  # which is a stronger statement than the minimum alone can make.
  full_batches <- if (length(seen) > 1) seen[-length(seen)] else seen

  # Tagged, because the parent folds this process's stderr into its stdout to
  # keep failures visible, and R will happily append a package warning after
  # the number the parent came for. Reading "the last line" then reads the
  # warning -- a locale-dependent string -- and the run is reported as having
  # produced no result at all.
  cat(sprintf(
    "RESULT,%.0f,%d,%.0f,%.0f,%.0f,%.1f,%.1f,%.1f,%.1f,%.1f\n",
    block / 1024^2,
    length(seen),
    mean(seen),
    min(full_batches),
    max(full_batches),
    elapsed,
    pool_mb,
    peak_mb,
    peak_mb - before_peak,
    rss_mb
  ))
  quit(save = "no", status = 0)
}

# ---- the sweep ---------------------------------------------------------------

suppressMessages(pkgload::load_all(quiet = TRUE))
path <- make_file(fixture, n_rows, n_cols)
size_mb <- file.info(path)$size / 1024^2
min_rows <- getOption("DTAtools.stream_arrow_numeric_min_rows", 20000L)

message("What does a larger read block cost?")
message(sprintf(
  "arrow %s | %d cpu threads | %s rows x %d cols, %.1f MB (%.0f bytes a row)",
  utils::packageVersion("arrow"),
  arrow::cpu_count(),
  format(n_rows, big.mark = ","),
  n_cols,
  size_mb,
  size_mb * 1024^2 / n_rows
))
message(sprintf(
  "batch_rows = %s | arrow numeric parse engages at %s rows a batch | %d replicates",
  format(batch_rows, big.mark = ","),
  format(min_rows, big.mark = ","),
  reps
))
message("")

# Read the fixture once and throw it away, so that the first block size
# measured is not the only one charged for pulling the file off disk. Skipping
# this made the smallest block look 70% slower than it is -- the sweep starts
# at the smallest block, so the cold read lands entirely on that row and reads
# as a property of the block size.
invisible(readBin(path, "raw", file.info(path)$size))
invisible(gc(full = TRUE))

rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
if (!file.exists(rscript)) {
  stop("cannot find Rscript at ", rscript, " -- the sweep re-invokes this script and has nothing to run")
}

# The child must load packages the same way the parent did, and the two
# workflows differ: under renv the project library comes from .Rprofile, which
# `--vanilla` would skip, leaving the child unable to find arrow and reporting
# it as a bad block size. Outside renv `--vanilla` is what keeps a stale
# .Rprofile out of the measurement. Neither is right for both, so the parent's
# own situation decides.
child_flags <- if (nzchar(Sys.getenv("RENV_PROJECT"))) {
  c("--no-save", "--no-restore")
} else {
  "--vanilla"
}

run_one <- function(mib, rep) {
  block <- mib * 1024^2
  # stderr is folded into stdout rather than discarded: a child that cannot
  # load a package fails with a message, and swallowing it leaves "no result"
  # as the only symptom of a problem that has nothing to do with the block.
  out <- system2(
    rscript,
    c(
      child_flags,
      shQuote(normalizePath(file.path("benchmarks", "bench_block_size.R"))),
      "--one", format(block, scientific = FALSE),
      "--rows", n_rows, "--cols", n_cols, "--batch", batch_rows,
      "--file", shQuote(path)
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  tagged <- grep("^RESULT,", out, value = TRUE)
  if (length(tagged) != 1) {
    message(sprintf("  %s MiB rep %d: no result. The child said:", mib, rep))
    message(paste0("    ", utils::tail(out, 8), collapse = "\n"))
    return(NULL)
  }
  parts <- suppressWarnings(as.numeric(
    strsplit(sub("^RESULT,", "", tagged), ",", fixed = TRUE)[[1]]
  ))
  if (length(parts) != 10 || anyNA(parts[1:6])) {
    message(sprintf("  %s MiB rep %d: unparseable result: %s", mib, rep, tagged))
    return(NULL)
  }
  row <- data.frame(
    block_mib = parts[[1]],
    rep = rep,
    batches = as.integer(parts[[2]]),
    mean_rows = as.integer(parts[[3]]),
    min_rows = as.integer(parts[[4]]),
    max_rows = as.integer(parts[[5]]),
    seconds = parts[[6]],
    arrow_pool_mb = parts[[7]],
    process_peak_mb = parts[[8]],
    scan_added_mb = parts[[9]],
    rss_after_mb = parts[[10]],
    arrow_parse = parts[[4]] >= min_rows,
    stringsAsFactors = FALSE
  )
  print(row, row.names = FALSE)
  row
}

# Replicates matter for one column only. The pool peak is deterministic --
# repeat it and the same number comes back -- but elapsed time on a desktop is
# not, and a single timing is how a benchmark ends up asserting a speedup it
# cannot reproduce. Every replicate is kept in the CSV rather than reduced to a
# summary here, so that the spread stays auditable after the fact.
rows <- unlist(
  lapply(seq_len(reps), function(rep) lapply(blocks_mib, run_one, rep = rep)),
  recursive = FALSE
)

results <- do.call(rbind, rows)
if (is.null(results) || length(unique(results$block_mib)) < 2) {
  stop("nothing to compare: fewer than two block sizes produced a result")
}
results <- results[order(results$block_mib, results$rep), , drop = FALSE]

message("")
message(strrep("-", 72))

# One row per block size, the replicates reduced. Median rather than mean for
# time, because the distribution is one-sided: a run can be delayed by
# something else on the machine, never hurried by it.
by_block <- do.call(rbind, lapply(split(results, results$block_mib), function(g) {
  data.frame(
    block_mib = g$block_mib[[1]],
    pool_mb = stats::median(g$arrow_pool_mb),
    pool_spread = max(g$arrow_pool_mb) - min(g$arrow_pool_mb),
    seconds = stats::median(g$seconds),
    seconds_lo = min(g$seconds),
    seconds_hi = max(g$seconds),
    min_rows = g$min_rows[[1]],
    max_rows = g$max_rows[[1]],
    arrow_parse = g$arrow_parse[[1]],
    stringsAsFactors = FALSE
  )
}))
by_block <- by_block[order(by_block$block_mib), , drop = FALSE]
print(by_block, row.names = FALSE)
message("")

message(sprintf(
  "pool peak varied by at most %.1f MB across %d replicates -- it is deterministic;",
  max(by_block$pool_spread), reps
))
message("time is not, so it is reported as a median with its full range.")
message("")

# Segment slopes, not one line through the ends. The pool has a knee: below
# the point where read-ahead dominates it is mostly the accumulator floor, and
# quoting a single average across the whole sweep would attribute the shallow
# early segments to the steep later ones.
if (nrow(by_block) > 1) {
  message("marginal cost of the next MiB of block:")
  for (i in seq_len(nrow(by_block) - 1L)) {
    lo <- by_block[i, ]
    hi <- by_block[i + 1L, ]
    message(sprintf(
      "  %5.0f -> %5.0f MiB : %5.1f MB per MiB   (pool %.0f -> %.0f MB)",
      lo$block_mib, hi$block_mib,
      (hi$pool_mb - lo$pool_mb) / (hi$block_mib - lo$block_mib),
      lo$pool_mb, hi$pool_mb
    ))
  }
  message(sprintf(
    "  against %d cpu threads -- where read-ahead dominates, the marginal cost",
    arrow::cpu_count()
  ))
  message("  is close to one block in flight per thread.")
}
message("")

engaged <- by_block[by_block$arrow_parse, , drop = FALSE]
if (nrow(engaged) > 0) {
  message(sprintf(
    "smallest block whose every batch clears the arrow numeric gate (%s rows): %.0f MiB",
    format(min_rows, big.mark = ","),
    min(engaged$block_mib)
  ))
  below <- by_block[!by_block$arrow_parse, , drop = FALSE]
  if (nrow(below) > 0) {
    widest <- below[nrow(below), ]
    message(sprintf(
      "  at %.0f MiB the LARGEST full batch was %s rows, so no batch clears it",
      widest$block_mib, format(widest$max_rows, big.mark = ",")
    ))
  }
} else {
  message("no block size tested produced batches large enough for the arrow numeric parse")
}

capped <- by_block[duplicated(by_block$max_rows), , drop = FALSE]
if (nrow(capped) > 0) {
  message(sprintf(
    "batch_rows (%s) becomes binding at %.0f MiB: past there the block buys read-ahead and no rows",
    format(batch_rows, big.mark = ","),
    min(capped$block_mib)
  ))
}

message("")
message("The figure to weigh a default against is scan_added_mb -- what the scan")
message("adds to the process peak -- read together with arrow_pool_mb. Elapsed")
message("time saturates long before memory does, so the largest block that is")
message("still faster is rarely the one worth defaulting to.")

utils::write.csv(results, file.path("benchmarks", "block_size.csv"), row.names = FALSE)
message("")
message("wrote benchmarks/block_size.csv")
