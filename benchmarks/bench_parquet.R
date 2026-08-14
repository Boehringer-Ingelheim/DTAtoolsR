#!/usr/bin/env Rscript
#
# Is caching a file as Parquet worth it?
#
# The claim is that a file validated more than once should be converted, so the
# repeats read a columnar format instead of re-parsing text. That claim has a
# break-even point - the conversion is not free - and the useful output of this
# script is where that point sits, not a single ratio.
#
# Usage:
#   Rscript benchmarks/bench_parquet.R
#   Rscript benchmarks/bench_parquet.R --rows 2000000 --cols 30

suppressMessages(pkgload::load_all(quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default) {
  hit <- which(args == flag)
  if (length(hit) == 0 || hit[[1]] == length(args)) {
    return(default)
  }
  args[[hit[[1]] + 1]]
}

n_rows <- as.integer(arg_value("--rows", "500000"))
n_cols <- as.integer(arg_value("--cols", "20"))

set.seed(11)
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

csv <- file.path(tempdir(), "bench_parquet.csv")
utils::write.csv(base, csv, row.names = FALSE)

cols <- list(
  DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE),
  DTAColumnSpec(id = "SEX", type = "SAS Char", length = 1, nullable = FALSE, values = c("M", "F")),
  DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
)
cols <- c(cols, lapply(setdiff(names(base), c("ID", "SEX", "AGE")), function(nm) {
  DTAColumnSpec(id = nm, type = "SAS Char", length = 12, nullable = TRUE)
}))
specs <- DTAColumnSpecCollection(
  columns = stats::setNames(cols, vapply(cols, function(x) x@id, character(1))),
  rules = list(DTARuleColRange(id = "age_range", columns = "AGE", range = c(18, 70)))
)

timed <- function(expr) {
  t0 <- proc.time()[["elapsed"]]
  force(expr)
  proc.time()[["elapsed"]] - t0
}

message("Is the Parquet cache worth it?")
message(sprintf("arrow %s | %s rows x %d cols", utils::packageVersion("arrow"), format(n_rows, big.mark = ","), n_cols))
message("")

csv_mb <- file.info(csv)$size / 1024^2
convert_s <- timed(cache_as_parquet(specs, csv, cache_path = file.path(tempdir(), "bench_parquet_cache")))
cache <- file.path(tempdir(), "bench_parquet_cache")
cache_mb <- sum(file.info(list.files(cache, recursive = TRUE, full.names = TRUE))$size) / 1024^2

# Two runs each; the first pays cold file-cache costs on both sides equally.
invisible(validate_file_stream(specs, csv, verbose = FALSE))
csv_s <- timed(validate_file_stream(specs, csv, verbose = FALSE))
invisible(validate_file_stream(specs, cache, verbose = FALSE))
parquet_s <- timed(validate_file_stream(specs, cache, verbose = FALSE))

message(sprintf("  csv on disk        : %.1f MB", csv_mb))
message(sprintf("  parquet on disk    : %.1f MB  (%.2fx)", cache_mb, cache_mb / csv_mb))
message("")
message(sprintf("  one-off conversion : %.2f s", convert_s))
message(sprintf("  validate from csv  : %.2f s", csv_s))
message(sprintf("  validate from cache: %.2f s  (%.2fx)", parquet_s, csv_s / parquet_s))
message("")

saving <- csv_s - parquet_s
if (saving <= 0) {
  message("  BREAK-EVEN: never. Validating the cache is not faster here, so the")
  message("  conversion is pure cost and the cache is not worth writing.")
} else {
  break_even <- convert_s / saving
  message(sprintf("  BREAK-EVEN: after %.1f validations.", break_even))
  message("  Below that the conversion costs more than it saves.")
}

unlink(csv)
unlink(cache, recursive = TRUE)
