#!/usr/bin/env Rscript
#
# Does streaming actually bound memory?
#
# The central claim of the streaming work is that peak memory is governed by
# the batch size rather than by the size of the input. That is a claim about
# memory, not speed, and it is the one that decides whether an 80 GB file is
# checkable at all - so it is worth measuring rather than asserting.
#
# The test is a scaling one. If the claim holds, the eager path's peak memory
# grows with the file and the streaming path's does not. A single measurement
# at one size would show nothing.
#
# This benchmark exercises the streaming path's actual error-accumulation
# machinery: a dirty fraction of rows produce real validation violations, which
# are retained in error sinks. Two rule types - uniqueness constraints (keyed
# on SUBJID) and group conditions (grouped by SITE) - exercise the two
# accumulators that leaked memory via R symbol-table interning.
#
# Previous runs used a fixture with no violations and no keyed/grouped rules,
# which silently bypassed the accumulators. Three real, measured memory bugs
# went undetected as a result.
#
# Usage:
#   Rscript benchmarks/bench_streaming.R
#   Rscript benchmarks/bench_streaming.R --rows 100000,400000,1600000 --cols 20

suppressMessages(pkgload::load_all(quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default) {
  hit <- which(args == flag)
  if (length(hit) == 0 || hit[[1]] == length(args)) {
    return(default)
  }
  args[[hit[[1]] + 1]]
}

row_counts <- as.numeric(strsplit(arg_value("--rows", "50000,200000,800000"), ",")[[1]])
n_cols <- as.integer(arg_value("--cols", "20"))
batch_rows <- as.integer(arg_value("--batch", "16384"))
dirty_fraction <- as.numeric(arg_value("--dirty", "0.02"))

# ---- fixture -----------------------------------------------------------------

make_file <- function(n_rows, n_cols, dirty = 0.02) {
  set.seed(7)
  base <- data.frame(
    ID = sprintf("S%07d", seq_len(n_rows)),
    SUBJID = sprintf("SUBJ%010d", seq_len(n_rows)),
    # Roughly n_rows/50 distinct sites, so the grouped accumulator holds a
    # number of groups that GROWS with the input -- which is the whole point of
    # measuring it. Fixed width, because a site id wider than the declared
    # length would flood the run with length violations that are an artefact of
    # the fixture rather than of anything under test.
    SITE = sprintf("SITE%06d", ((seq_len(n_rows) - 1L) %% max(1L, n_rows %/% 50L)) + 1L),
    SEX = sample(c("M", "F"), n_rows, replace = TRUE),
    AGE = sample(18:70, n_rows, replace = TRUE),
    stringsAsFactors = FALSE
  )

  # Corrupt a fraction of rows to produce real validation errors. This exercises
  # the error-accumulation sinks, which were previously unbounded or leaked memory.
  if (dirty > 0 && dirty < 1) {
    n_dirty <- max(1L, as.integer(n_rows * dirty))
    dirty_rows <- sample.int(n_rows, n_dirty, replace = FALSE)

    # Write AGE values outside the 18-70 range to violate the range rule.
    base$AGE[dirty_rows] <- sample(c(-1, 100), n_dirty, replace = TRUE)

    # Write ID values longer than the declared 8 characters to violate the length constraint.
    base$ID[dirty_rows] <- sprintf("S%010d", seq_len(n_dirty) + 10000)
  }

  n_filler <- max(0L, n_cols - ncol(base))
  if (n_filler > 0) {
    filler <- lapply(seq_len(n_filler), function(i) sprintf("TXT%05d", sample.int(99999, n_rows, TRUE)))
    names(filler) <- sprintf("FILL%02d", seq_len(n_filler))
    base <- cbind(base, as.data.frame(filler, stringsAsFactors = FALSE))
  }

  path <- file.path(tempdir(), sprintf("stream_bench_%d.csv", n_rows))
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
      # Two genuinely different conditions, so the constraint is violated only
      # by a site that received both kinds of corruption -- not by every site.
      # A constraint that every group breaks would make the run measure the
      # cost of ASSEMBLING violation messages rather than the cost of holding
      # the group accumulator, which is the thing under test. The any/any
      # `mutually_exclusive` shape is also the one `fail_fast` can decide
      # mid-scan, so this exercises that path too.
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

# Get the process's resident set size in MB, cross-platform. Returns NA_real_ if
# it cannot be determined. Windows uses PowerShell; Linux reads /proc/self/status;
# macOS uses ps. This measures actual memory in use by the process, including the
# C++ heap that gc() does not report.
get_rss_mb <- function() {
  pid <- Sys.getpid()
  tryCatch({
    if (.Platform$OS.type == "windows") {
      # Windows: use PowerShell to get WorkingSet64 (RSS in bytes)
      output <- system2(
        "powershell",
        c("-NoProfile", "-Command", sprintf("(Get-Process -Id %d).WorkingSet64", pid)),
        stdout = TRUE,
        stderr = FALSE
      )
      if (length(output) > 0 && !is.na(as.numeric(output[1]))) {
        as.numeric(output[1]) / 1024^2
      } else {
        NA_real_
      }
    } else if (file.exists("/proc/self/status")) {
      # Linux: VmRSS, the CURRENT resident size -- deliberately not VmHWM.
      # VmHWM is a high-water mark that never decreases for the life of the
      # process, so reading it would make the after-gc figure below incapable
      # of ever falling back toward baseline: the run would report the same
      # apparent leak whether or not memory was actually reclaimed, which is
      # precisely the distinction this benchmark exists to draw. Windows'
      # WorkingSet64 and macOS' `ps -o rss=` are both current, so this keeps
      # all three platforms measuring the same quantity.
      content <- readLines("/proc/self/status", warn = FALSE)
      vmrss_line <- grep("^VmRSS:", content, value = TRUE)
      if (length(vmrss_line) > 0) {
        # VmRSS is in kB; convert to MB
        kb <- as.numeric(sub("^VmRSS:\\s+([0-9]+).*", "\\1", vmrss_line))
        kb / 1024
      } else {
        NA_real_
      }
    } else {
      # macOS: use ps to get RSS
      output <- system2(
        "ps",
        c("-o", "rss=", "-p", pid),
        stdout = TRUE,
        stderr = FALSE
      )
      if (length(output) > 0 && !is.na(as.numeric(output[1]))) {
        # ps reports RSS in kB; convert to MB
        as.numeric(output[1]) / 1024
      } else {
        NA_real_
      }
    }
  }, error = function(e) NA_real_)
}

# Peak R heap during a call. gc(reset = TRUE) sets the recorded maximum back to
# current; the gc() afterwards therefore reports the peak reached during it.
# Column 6 is "max used (Mb)", summed over the Ncells and Vcells rows.
peak_during <- function(expr) {
  invisible(gc(reset = TRUE, full = TRUE))
  t0 <- proc.time()[["elapsed"]]
  force(expr)
  elapsed <- proc.time()[["elapsed"]] - t0
  list(peak_mb = sum(gc(full = TRUE)[, 6]), seconds = elapsed, rss_mb = get_rss_mb())
}

# ---- run ---------------------------------------------------------------------

message("Does streaming bound memory?")
message(sprintf("arrow %s | batch_rows = %s", utils::packageVersion("arrow"), format(batch_rows, big.mark = ",")))
message("")

results <- do.call(rbind, lapply(row_counts, function(n) {
  n <- as.integer(n)
  path <- make_file(n, n_cols, dirty = dirty_fraction)
  specs <- make_specs(path)
  mb <- file.info(path)$size / 1024^2

  # Both paths must do the SAME work or the comparison is meaningless. The
  # streaming driver types each batch against the specs, so the eager path
  # types the table too - otherwise streaming is charged for work its rival
  # never does.
  eager <- peak_during({
    tbl <- dta_read_delim_normalized(
      path,
      delim = ",",
      quote = "\"",
      has_header = TRUE,
      specs = specs
    )
    typed <- dta_coerce_table_to_specs(as.data.frame(tbl), specs)
    validate_table_detailed(specs, typed$table, verbose = FALSE)
  })

  streamed <- peak_during(
    validate_file_stream(specs, path, batch_rows = batch_rows, verbose = FALSE)
  )

  # After the streamed run, force a full garbage collection twice and measure
  # RSS again. A memory leak shows up as RSS that does not return to baseline -
  # specifically, the symbol-table leak will not be reclaimed, as gc() does not
  # report it as freed.
  invisible(gc(full = TRUE))
  invisible(gc(full = TRUE))
  stream_rss_after_gc_mb <- get_rss_mb()

  # What each path has to HOLD, which is the thing actually at stake. Peak heap
  # at these sizes is dominated by transient allocation and by the ~200 MB the
  # loaded package already occupies; retention is what decides whether a file
  # larger than memory can be checked at all.
  tbl <- dta_read_delim_normalized(
    path,
    delim = ",",
    quote = "\"",
    has_header = TRUE,
    specs = specs
  )
  eager_hold_mb <- as.numeric(utils::object.size(as.data.frame(tbl))) / 1024^2
  one_batch <- as.data.frame(tbl$Slice(0, min(batch_rows, tbl$num_rows)))
  stream_hold_mb <- as.numeric(utils::object.size(one_batch)) / 1024^2
  rm(tbl, one_batch)

  unlink(path)

  out <- data.frame(
    n_rows = n,
    mb_on_disk = round(mb, 1),
    eager_hold_mb = round(eager_hold_mb, 1),
    stream_hold_mb = round(stream_hold_mb, 1),
    eager_peak_mb = round(eager$peak_mb, 1),
    eager_rss_mb = round(eager$rss_mb, 1),
    stream_peak_mb = round(streamed$peak_mb, 1),
    stream_rss_mb = round(streamed$rss_mb, 1),
    stream_rss_after_gc_mb = round(stream_rss_after_gc_mb, 1),
    eager_s = round(eager$seconds, 2),
    stream_s = round(streamed$seconds, 2),
    stringsAsFactors = FALSE
  )
  print(out, row.names = FALSE)
  out
}))

message("")
message(strrep("-", 66))

first <- results[1, ]
last <- results[nrow(results), ]
growth <- function(a, b) if (a <= 0) NA_real_ else round(b / a, 2)

message(sprintf(
  "input grew %.1fx (%s -> %s rows)",
  last$n_rows / first$n_rows,
  format(first$n_rows, big.mark = ","),
  format(last$n_rows, big.mark = ",")
))
message("")
message("  what each path must HOLD:")
message(sprintf("    eager  grew %sx  (%s -> %s MB)", growth(first$eager_hold_mb, last$eager_hold_mb), first$eager_hold_mb, last$eager_hold_mb))
message(sprintf("    stream grew %sx  (%s -> %s MB)", growth(first$stream_hold_mb, last$stream_hold_mb), first$stream_hold_mb, last$stream_hold_mb))
message("")
message("  peak R heap during the run:")
message(sprintf("    eager  grew %sx", growth(first$eager_peak_mb, last$eager_peak_mb)))
message(sprintf("    stream grew %sx", growth(first$stream_peak_mb, last$stream_peak_mb)))
message("")
message("  resident set size (RSS, including C++ heap):")
message(sprintf("    eager  RSS: %s -> %s MB", first$eager_rss_mb, last$eager_rss_mb))
message(sprintf("    stream RSS: %s -> %s MB", first$stream_rss_mb, last$stream_rss_mb))
message(sprintf("    stream RSS after gc(): %s -> %s MB", first$stream_rss_after_gc_mb, last$stream_rss_after_gc_mb))
message("")
message("Retention is the figure that decides feasibility: it is what the process")
message("must fit in memory at once. Peak heap at these sizes is dominated by")
message("transient allocation and by the ~200 MB the loaded package already")
message("occupies, so it discriminates poorly until the input is large enough for")
message("retention to dominate - which is exactly the regime this work is for.")
message("")
message("R heap (peak_mb) measures the garbage-collected heap. RSS (rss_mb)")
message("measures actual resident memory, including Arrow's C++ buffer pool and any")
message("interned symbols. If stream_rss_mb grows while stream_peak_mb is flat, the")
message("C++ pool is growing. If stream_rss_after_gc_mb does NOT return to baseline")
message("after full gc, that is the signature of symbol-table interning - a permanent")
message("leak through R's symbol lookup cache.")
message("")
message("The RSS columns are what this benchmark previously lacked. Reporting only")
message("the R heap is what let a permanent symbol-table leak and Arrow's C++ pool")
message("both go unmeasured for three releases. At small input sizes RSS is still")
message("dominated by the loaded package and by allocator high-water marks, so run")
message("this at a size where retention dominates before reading much into it.")

utils::write.csv(results, file.path("benchmarks", "streaming.csv"), row.names = FALSE)
message("")
message("wrote benchmarks/streaming.csv")
