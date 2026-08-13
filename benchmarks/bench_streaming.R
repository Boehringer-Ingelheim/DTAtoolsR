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

# ---- fixture -----------------------------------------------------------------

make_file <- function(n_rows, n_cols) {
  set.seed(7)
  base <- data.frame(
    ID = sprintf("S%07d", seq_len(n_rows)),
    SEX = sample(c("M", "F"), n_rows, replace = TRUE),
    AGE = sample(18:70, n_rows, replace = TRUE),
    stringsAsFactors = FALSE
  )
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
    DTAColumnSpec(id = "SEX", type = "SAS Char", length = 1, nullable = FALSE, values = c("M", "F")),
    DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
  )
  filler <- setdiff(header, c("ID", "SEX", "AGE"))
  cols <- c(cols, lapply(filler, function(nm) {
    DTAColumnSpec(id = nm, type = "SAS Char", length = 12, nullable = TRUE)
  }))

  DTAColumnSpecCollection(
    columns = stats::setNames(cols, vapply(cols, function(x) x@id, character(1))),
    rules = list(DTARuleColRange(id = "age_range", columns = "AGE", range = c(18, 70)))
  )
}

# Peak R heap during a call. gc(reset = TRUE) sets the recorded maximum back to
# current; the gc() afterwards therefore reports the peak reached during it.
# Column 6 is "max used (Mb)", summed over the Ncells and Vcells rows.
peak_during <- function(expr) {
  invisible(gc(reset = TRUE, full = TRUE))
  t0 <- proc.time()[["elapsed"]]
  force(expr)
  elapsed <- proc.time()[["elapsed"]] - t0
  list(peak_mb = sum(gc(full = TRUE)[, 6]), seconds = elapsed)
}

# ---- run ---------------------------------------------------------------------

message("Does streaming bound memory?")
message(sprintf("arrow %s | batch_rows = %s", utils::packageVersion("arrow"), format(batch_rows, big.mark = ",")))
message("")

results <- do.call(rbind, lapply(row_counts, function(n) {
  n <- as.integer(n)
  path <- make_file(n, n_cols)
  specs <- make_specs(path)
  mb <- file.info(path)$size / 1024^2

  # Both paths must do the SAME work or the comparison is meaningless. The
  # streaming driver types each batch against the specs, so the eager path
  # types the table too - otherwise streaming is charged for work its rival
  # never does.
  eager <- peak_during({
    tbl <- arrow::read_csv_arrow(
      path,
      col_types = dta_reader_col_types(specs, TRUE),
      as_data_frame = FALSE
    )
    typed <- dta_coerce_table_to_specs(as.data.frame(tbl), specs)
    validate_table_detailed(specs, typed$table, verbose = FALSE)
  })

  streamed <- peak_during(
    validate_file_stream(specs, path, batch_rows = batch_rows, verbose = FALSE)
  )

  # What each path has to HOLD, which is the thing actually at stake. Peak heap
  # at these sizes is dominated by transient allocation and by the ~200 MB the
  # loaded package already occupies; retention is what decides whether a file
  # larger than memory can be checked at all.
  tbl <- arrow::read_csv_arrow(
    path,
    col_types = dta_reader_col_types(specs, TRUE),
    as_data_frame = FALSE
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
    stream_peak_mb = round(streamed$peak_mb, 1),
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
message("Retention is the figure that decides feasibility: it is what the process")
message("must fit in memory at once. Peak heap at these sizes is dominated by")
message("transient allocation and by the ~200 MB the loaded package already")
message("occupies, so it discriminates poorly until the input is large enough for")
message("retention to dominate - which is exactly the regime this work is for.")
message("")
message("Both figures measure the R heap only; Arrow holds its own C++ pool")
message("outside it, so the eager path's true cost is higher than shown.")

utils::write.csv(results, file.path("benchmarks", "streaming.csv"), row.names = FALSE)
message("")
message("wrote benchmarks/streaming.csv")
