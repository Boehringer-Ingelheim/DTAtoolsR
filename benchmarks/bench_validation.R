#!/usr/bin/env Rscript
#
# Per-stage cost of the current validation pipeline (P0).
#
# The point of this script is not "validation is slow" - it is *which stage* is
# slow, measured rather than assumed, so the Arrow rewrite can be judged
# against a baseline instead of against an intuition. It reports each stage
# separately and extrapolates to the 80 GB target.
#
# Usage:
#   Rscript benchmarks/bench_validation.R
#   Rscript benchmarks/bench_validation.R --rows 10000,50000,200000 --cols 20
#   Rscript benchmarks/bench_validation.R --rows 1000000            # slow
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

row_counts <- as.numeric(strsplit(arg_value("--rows", "10000,50000,200000"), ",")[[1]])
n_cols <- as.integer(arg_value("--cols", "20"))
target_gb <- as.numeric(arg_value("--target-gb", "80"))

# ---- fixture generation -----------------------------------------------------

# A shape close to real transfer data: a few constrained columns that the
# checks actually read, padded with filler so row width - and therefore JSON
# serialisation cost - is realistic.
make_frame <- function(n_rows, n_cols) {
  set.seed(42)

  base <- data.frame(
    SUBJID = sprintf("S%07d", seq_len(n_rows)),
    SEX = sample(c("M", "F"), n_rows, replace = TRUE),
    DOMAIN = rep("GF", n_rows),
    AGE = as.character(sample(18:70, n_rows, replace = TRUE)),
    VISIT = sample(sprintf("V%02d", 1:8), n_rows, replace = TRUE),
    stringsAsFactors = FALSE
  )

  n_filler <- max(0L, n_cols - ncol(base))
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

  base
}

make_specs <- function(frame) {
  cols <- list(
    DTAColumnSpec(
      id = "SUBJID", type = "SAS Char", length = 8,
      nullable = FALSE, pattern = "^S[0-9]{7}$"
    ),
    DTAColumnSpec(
      id = "SEX", type = "SAS Char", length = 1,
      nullable = FALSE, values = c("M", "F")
    ),
    DTAColumnSpec(
      id = "DOMAIN", type = "SAS Char", length = 2,
      nullable = FALSE, values = "GF"
    ),
    DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE),
    DTAColumnSpec(id = "VISIT", type = "SAS Char", length = 3, nullable = FALSE)
  )

  filler_names <- setdiff(names(frame), vapply(cols, function(x) x@id, character(1)))
  filler_specs <- lapply(filler_names, function(nm) {
    DTAColumnSpec(id = nm, type = "SAS Char", length = 12, nullable = TRUE)
  })

  all_cols <- c(cols, filler_specs)

  DTAColumnSpecCollection(
    columns = stats::setNames(
      all_cols,
      vapply(all_cols, function(x) x@id, character(1))
    ),
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

# ---- measurement ------------------------------------------------------------

# Elapsed seconds plus peak R heap over the call.
#
# gc(reset = TRUE) sets the recorded maximum back to current usage; the gc()
# after the call therefore reports the peak reached *during* it. Column 6 is
# "max used (Mb)", summed over the Ncells and Vcells rows.
#
# This is peak total heap, not a delta, and it excludes memory Arrow holds in
# its own C++ pool - which is exactly where the large allocations live. Treat
# it as a floor on R-side cost, not as total process memory.
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

bench_one <- function(n_rows, n_cols) {
  message(sprintf("  building fixture: %s rows x %d cols", format(n_rows, big.mark = ","), n_cols))
  frame <- make_frame(n_rows, n_cols)
  specs <- make_specs(frame)

  path <- file.path(tempdir(), sprintf("bench_%d.csv", n_rows))
  utils::write.csv(frame, path, row.names = FALSE, quote = TRUE)
  bytes <- file.info(path)$size

  stages <- list()

  # 1. read -------------------------------------------------------------------
  read <- timed("read", {
    arrow::read_csv_arrow(
      path,
      col_types = dta_reader_col_types(specs, TRUE),
      as_data_frame = FALSE
    )
  })
  stages[[length(stages) + 1]] <- read
  tbl <- read$value

  # 2. import typing (Arrow -> R -> Arrow round trip) -------------------------
  coerce <- timed("coerce", dta_coerce_table_to_specs(tbl, specs))
  stages[[length(stages) + 1]] <- coerce

  # 3. materialise to an R data frame -----------------------------------------
  materialise <- timed("materialise", as.data.frame(tbl))
  stages[[length(stages) + 1]] <- materialise
  df <- materialise$value

  # 4. schema axis ------------------------------------------------------------
  # Mirrors validationFunctions.R:111-205. Replicated rather than called
  # because the loop is inline in validate_table_detailed() and cannot be timed
  # separately without changing product code.
  schema <- timed("schema_json", {
    schema_json <- as_json_schema(specs)
    obj <- jsonvalidate::json_schema$new(schema_json)
    chunks <- split(df, ceiling(seq_len(nrow(df)) / 5000))
    for (chunk in chunks) {
      json_data <- jsonlite::toJSON(
        chunk,
        dataframe = "rows", auto_unbox = TRUE, na = "null"
      )
      obj$validate(json_data, verbose = TRUE, greedy = TRUE)
    }
    length(chunks)
  })
  stages[[length(stages) + 1]] <- schema

  # 5. rules axis -------------------------------------------------------------
  rules <- timed("rules", apply_schema_rules(specs@rules, df, verbose = FALSE))
  stages[[length(stages) + 1]] <- rules

  unlink(path)

  data.frame(
    n_rows = n_rows,
    n_cols = n_cols,
    mb_on_disk = round(bytes / 1024^2, 2),
    stage = vapply(stages, function(s) s$label, character(1)),
    seconds = round(vapply(stages, function(s) s$seconds, numeric(1)), 3),
    peak_mb = round(vapply(stages, function(s) s$peak_mb, numeric(1)), 1),
    stringsAsFactors = FALSE
  )
}

# ---- run --------------------------------------------------------------------

message("DTAtools validation baseline")
message(sprintf("arrow %s | R %s", utils::packageVersion("arrow"), getRversion()))
message("")

results <- do.call(rbind, lapply(row_counts, function(n) {
  message(sprintf("n_rows = %s", format(n, big.mark = ",")))
  out <- bench_one(as.integer(n), n_cols)
  print(out[, c("stage", "seconds", "peak_mb")], row.names = FALSE)
  message("")
  out
}))

# ---- summary ----------------------------------------------------------------

message(strrep("-", 62))
message("share of total time by stage, largest input")

largest <- results[results$n_rows == max(results$n_rows), ]
largest$share <- round(100 * largest$seconds / sum(largest$seconds), 1)
print(largest[order(-largest$share), c("stage", "seconds", "share")], row.names = FALSE)

message("")
message(strrep("-", 62))
message(sprintf("extrapolation to %g GB", target_gb))

# Linear extrapolation. It is a floor, not a forecast: the stages that
# materialise the whole table degrade worse than linearly once the working set
# stops fitting in RAM, and one of them is quadratic in error count.
mb_per_s <- sum(largest$mb_on_disk[[1]]) / sum(largest$seconds)
hours <- (target_gb * 1024 / mb_per_s) / 3600
message(sprintf("  throughput at largest input : %.1f MB/s", mb_per_s))
message(sprintf("  linear projection           : %.1f hours", hours))
message("  (a floor - stages that materialise the table degrade superlinearly")
message("   past the point the working set stops fitting in RAM)")

out_path <- file.path("benchmarks", "baseline.csv")
utils::write.csv(results, out_path, row.names = FALSE)
message("")
message(sprintf("wrote %s", out_path))
