# Generators for the randomised eager/streaming parity campaign in
# test-OQ-STREAM-parity.R.
#
# Ported from the package's own developer suite
# (tests/testthat/test-streaming-parity.R), which is not installed and so is
# not available to a qualification run against an installed copy of the
# package. Everything here keeps the original's shape; every name gets a
# `qp_` prefix so it cannot collide with a helper from another area sourced
# into the same test session.
#
# `stream = "always"` is a memory profile, not a different answer. Everything
# else in the suite compares the two paths on fixtures chosen by hand, which
# can only find the divergences someone thought to write down; this file
# generates the file, the specification and the rules together from a seed, so
# the campaign can find the ones nobody did.
#
# The divergence that motivated the original: with specs supplied, the lazy
# reader pinned every column to text while the eager one left an UNDECLARED
# column to Arrow's inference. A uniqueness rule over such a column then saw
# the doubles 1.5 and 1.5 in memory -- a duplicate -- and the strings "1.5"
# and "1.50" when streamed -- not a duplicate. Same file, two verdicts, no
# error anywhere. `XKEY` below is that column, and it is in every generated
# case; the reduced, named form of the case is its own test in
# test-OQ-STREAM-parity.R (REQ-STREAM-021), not a generator.

# Every generated file is written under one qa_tempdir() for the whole
# campaign rather than one per case: the campaign can run to 1000 cases at the
# full tier, and a directory per case would multiply that by the cost of
# qa_tempdir()'s own bookkeeping for no benefit -- each file is still deleted
# as soon as both engines have read it (see qp_file_results() below).
#
# Bound to globalenv() rather than left at qa_tempdir()'s default: this file
# is sourced once, at top level, as a helper -- there is no enclosing
# test_that() frame for the deferred cleanup to attach to, and without an
# explicit env the directory was removed the moment sourcing finished, before
# the first test_that() in test-OQ-STREAM-parity.R ever ran. globalenv()
# outlives every test file in the run, which is what a campaign shared across
# several test_that() blocks in this file needs.
qp_dir <- qa_tempdir(env = globalenv())

# ---- the file-based generator ------------------------------------------------

# Column specifications the generator draws from. ID and AGE are always taken
# (the rules key on them); the rest make the shape vary.
qp_column_pool <- function() {
  list(
    ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE),
    SITE = DTAColumnSpec(id = "SITE", type = "SAS Char", length = 4, nullable = TRUE),
    SEX = DTAColumnSpec(
      id = "SEX", type = "SAS Char", length = 1,
      nullable = FALSE, values = c("M", "F")
    ),
    CODE = DTAColumnSpec(id = "CODE", type = "SAS Char", nullable = TRUE),
    AGE = DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE),
    CNT = DTAColumnSpec(id = "CNT", type = "SAS Int", nullable = TRUE)
  )
}

# Every value is generated as TEXT, because that is what a CSV holds. The
# defects are injected at fixed positions rather than randomly: a defect that
# appears only for some seeds is a defect this file does not really test.
qp_column_values <- function(name, n) {
  if (n == 0) {
    return(character(0))
  }

  switch(name,
    ID = {
      v <- sprintf("S%04d", seq_len(n))
      if (n >= 4) v[[4]] <- v[[3]] # duplicate key
      if (n >= 5) v[[5]] <- "THIS-ID-IS-FAR-TOO-LONG" # over the declared length
      if (n >= 6) v[[6]] <- NA_character_ # missing in a non-nullable column
      v
    },
    SITE = rep(c("S01", "S02", NA_character_), length.out = n),
    SEX = {
      v <- rep(c("M", "F"), length.out = n)
      if (n >= 3) v[[3]] <- "X" # outside the permitted set
      v
    },
    CODE = rep(c("AB123", "", "zz"), length.out = n),
    AGE = {
      v <- as.character(rep(c(17, 18, 45, 70, 71), length.out = n))
      if (n >= 2) v[[2]] <- "abc" # unconvertible
      if (n >= 7) v[[7]] <- "<0.5" # censored text in a numerically compared column
      if (n >= 8) v[[8]] <- "" # missing
      v
    },
    CNT = {
      v <- as.character(rep(0:5, length.out = n))
      if (n >= 9) v[[9]] <- "0.01" # a fraction in a declared Int column
      v
    },
    cli::cli_abort("No generator for column {name}.")
  )
}

# One to three rules, always including a uniqueness rule -- that is the axis the
# reader contract broke.
qp_rules <- function(chosen) {
  candidates <- list(
    DTARuleColUnique(
      id = "id_unique",
      columns = if ("SITE" %in% chosen) c("ID", "SITE") else "ID"
    ),
    DTARuleColUnique(id = "xkey_unique", columns = "XKEY"),
    DTARuleColRange(id = "age_range", columns = "AGE", range = c(18, 70)),
    DTARuleColCondition(
      id = "adult_site",
      condition = list(AGE = list(greater_equal = 18)),
      then = list(ID = list(empty = FALSE))
    )
  )

  extra <- sample(2:4, sample(0:2, 1))
  candidates[c(1, extra)]
}

# One generated case: a file on disk, the specification it is meant to satisfy,
# and the name the handler declares.
qp_case <- function(seed) {
  set.seed(seed)

  pool <- qp_column_pool()
  optional <- setdiff(names(pool), c("ID", "AGE"))
  chosen <- c("ID", "AGE", sample(optional, sample(seq_len(4), 1)))
  chosen <- names(pool)[names(pool) %in% chosen]

  n <- sample(0:400, 1)

  columns <- lapply(chosen, function(name) qp_column_values(name, n))
  names(columns) <- chosen

  # Two columns no specification mentions. XKEY is read by a rule and holds the
  # values whose numeric and textual identities differ; XPAD is read by nothing
  # at all, and is here because an unread column must not change a verdict
  # either.
  columns$XKEY <- if (n == 0) {
    character(0)
  } else {
    rep(c("1.5", "1.50", "2", "2.0", "0", "-0", "", "NaN"), length.out = n)
  }
  columns$XPAD <- if (n == 0) character(0) else sprintf("p%03d", seq_len(n))

  frame <- as.data.frame(columns, stringsAsFactors = FALSE)

  rules <- qp_rules(chosen)

  specs <- DTAColumnSpecCollection(
    columns = pool[chosen],
    rules = rules
  )

  path <- qp_write(frame, seed, sample(c("plain", "quoted", "padded"), 1))
  declared <- basename(path)

  if (sample(c(TRUE, FALSE), 1)) {
    path <- qp_gzip(path)
  }

  list(path = path, declared = declared, specs = specs, rows = n)
}

# The header is written plain, quoted or padded: cleaning it is what used to
# make the eager reader read the file twice, and a header the two readers clean
# differently is a divergence in itself.
qp_write <- function(frame, seed, header_style) {
  path <- file.path(qp_dir, sprintf("parity_%03d.csv", seed))

  header <- switch(header_style,
    plain = names(frame),
    quoted = paste0('"', names(frame), '"'),
    padded = paste0('" ', names(frame), ' "')
  )

  rows <- if (nrow(frame) == 0) {
    character(0)
  } else {
    apply(frame, 1, function(row) paste(ifelse(is.na(row), "", row), collapse = ","))
  }

  writeLines(c(paste(header, collapse = ","), rows), path)
  path
}

qp_gzip <- function(path) {
  gz <- paste0(path, ".gz")
  bytes <- readBin(path, "raw", n = file.size(path))
  con <- gzfile(gz, "wb")
  writeBin(bytes, con)
  close(con)
  unlink(path)
  gz
}

# ---- running one case on one path -------------------------------------------

# Sorted by every column, as text: two paths may legitimately report the same
# errors in a different order (a scan reports per batch), and an order
# difference is not a disagreement about the data.
qp_sort <- function(errors) {
  if (nrow(errors) == 0) {
    return(errors)
  }
  keys <- lapply(errors, as.character)
  out <- errors[do.call(order, keys), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# `batch_rows` is exposed (rather than left at check()'s own default, as the
# main campaign leaves it) so that the batch-boundary cases in
# test-OQ-STREAM-parity.R can place a defect at a chosen offset from a chosen
# batch size. It is harmless for the eager path: check() documents batch_rows
# as ignored for a table held in memory.
qp_run <- function(case, stream, batch_rows = getOption("DTAtools.stream_batch_rows", 131072L)) {
  ds <- DTADataSetTabular(
    name = "parity",
    specs = case$specs,
    files = list(DTAFileCSV(filename = case$declared))
  )
  ds <- load_file(ds, file = case$path, handler_index = 1, stream = stream)
  checked <- check(ds, quiet = TRUE, persist = FALSE, batch_rows = batch_rows)

  table_name <- names(tables(checked))[[1]]
  status <- validation_status(checked)
  # Timestamps and run ids differ between two runs of the same data by
  # construction; everything else in the row is the verdict.
  status <- status[
    , setdiff(names(status), c("validated_at", "run_id", "validation_run")),
    drop = FALSE
  ]

  list(
    status = status,
    errors = qp_sort(as.data.frame(validation_errors(checked, table_name))),
    n_import_errors = checked@validation_store[[table_name]]$n_import_errors
  )
}

# A small memoising wrapper. The assertion loop and the coverage assertions in
# test-OQ-STREAM-parity.R read the SAME generated-and-run campaign; without
# this each test_that() would regenerate and revalidate it, doubling (or more)
# the cost of the file for no new evidence.
qp_cache <- function(build) {
  cache <- NULL
  cache_n <- NULL
  function(n) {
    if (is.null(cache) || !identical(cache_n, n)) {
      cache <<- build(n)
      cache_n <<- n
    }
    cache
  }
}

# The n generated file-based cases, run once on each engine. `n` is
# qa_parity_seeds(): 25 at the quick tier, 1000 at the full one.
qp_file_results <- qp_cache(function(n) {
  lapply(seq_len(n), function(seed) {
    case <- qp_case(seed)
    result <- list(
      case = case,
      eager = qp_run(case, "never"),
      lazy = qp_run(case, "always")
    )
    unlink(case$path)
    result
  })
})

# ---- the R-typed generator --------------------------------------------------
#
# Everything above starts from a FILE, so every column reaches both engines as
# text and the reader decides what it becomes. A table built in R never passes
# a reader at all: its columns arrive already typed, and the two engines see
# that typing at different moments -- the whole-column engine gets the table
# the constructor coerced once, the streaming engine coerces each batch as it
# arrives. Both must still reach the same verdict.
#
# The pinned Int-narrowing defect (DEV-004 in deviations.yaml, affecting
# REQ-TYPE-002, not this area) is the one place they cannot: `SAS Int` narrows
# to R `integer` only when every value in hand is whole and inside the integer
# range, and "in hand" is the whole column on one path and one batch on the
# other, so a column that is fractional or out of range in only SOME batches is
# typed differently by the two. It is kept out of this generator by giving
# every `SAS Int` column whole values well inside the integer range, so the two
# agree by construction rather than by luck.

qp_memory_pool <- function() {
  list(
    ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE),
    SITE = DTAColumnSpec(id = "SITE", type = "SAS Char", length = 4, nullable = TRUE),
    SEX = DTAColumnSpec(
      id = "SEX", type = "SAS Char", length = 1,
      nullable = FALSE, values = c("M", "F")
    ),
    AGE = DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE),
    CNT = DTAColumnSpec(id = "CNT", type = "SAS Int", nullable = TRUE)
  )
}

# The declared columns, each in the R type a caller would naturally build it in
# rather than as text. Defects sit at fixed positions, for the reason the file
# generator gives: one that appears only for some seeds is one this file does
# not really test.
qp_memory_values <- function(name, n) {
  switch(name,
    ID = {
      v <- sprintf("S%04d", seq_len(n))
      if (n >= 4) v[[4]] <- v[[3]] # duplicate key
      if (n >= 5) v[[5]] <- "THIS-ID-IS-FAR-TOO-LONG" # over the declared length
      if (n >= 6) v[[6]] <- NA_character_ # missing in a non-nullable column
      v
    },
    SITE = rep(c("S01", "S02", NA_character_), length.out = n),
    # A factor, not a character vector: a declared `Char` column is never
    # coerced, so whatever R type it arrived in is what both engines validate.
    SEX = {
      v <- rep(c("M", "F"), length.out = n)
      if (n >= 3) v[[3]] <- "X" # outside the permitted set
      factor(v, levels = c("M", "F", "X"))
    },
    AGE = {
      v <- rep(c(17, 18, 45, 70, 71), length.out = n)
      if (n >= 8) v[[8]] <- NA_real_ # missing in a nullable column
      v
    },
    # Whole and far inside the integer range in EVERY batch -- see the note on
    # the pinned Int-narrowing defect above.
    CNT = as.integer(rep(0:5, length.out = n)),
    cli::cli_abort("No generator for column {name}.")
  )
}

# Columns no specification mentions, in the R types a constructed table carries.
# XKEY is numeric and read by a uniqueness rule; the rest are read by nothing,
# and are here because a column a rule never touches must not change a verdict
# either -- including the types that only exist in R (`Date`, `POSIXct`,
# `factor`, `logical`, and `integer64` where bit64 is installed).
qp_memory_extras <- function(n) {
  extras <- list(
    XKEY = rep(c(1.5, 1.5, 2, 2, 0, -0.5, NA_real_, 3), length.out = n),
    FLAG = rep(c(TRUE, FALSE, NA), length.out = n),
    DAY = as.Date("2026-01-01") + rep(0:6, length.out = n),
    WHEN = as.POSIXct("2026-01-01 00:00:00", tz = "UTC") +
      rep(c(0, 3600, 86400), length.out = n),
    XPAD = if (n == 0) character(0) else sprintf("p%03d", seq_len(n))
  )

  if (requireNamespace("bit64", quietly = TRUE)) {
    extras$BIG <- bit64::as.integer64(rep(c(1, 2, 3), length.out = n))
  }

  extras
}

qp_case_memory <- function(seed) {
  # Offset from the file generator's seeds so the two draw different shapes
  # rather than the same one twice.
  set.seed(1000L + seed)

  pool <- qp_memory_pool()
  optional <- setdiff(names(pool), c("ID", "AGE"))
  chosen <- c("ID", "AGE", sample(optional, sample(seq_len(3), 1)))
  chosen <- names(pool)[names(pool) %in% chosen]

  n <- sample(0:400, 1)

  columns <- lapply(chosen, function(name) qp_memory_values(name, n))
  names(columns) <- chosen
  columns <- c(columns, qp_memory_extras(n))

  frame <- as.data.frame(columns, stringsAsFactors = FALSE)

  list(
    frame = frame,
    specs = DTAColumnSpecCollection(
      columns = pool[chosen],
      rules = qp_rules(chosen)
    ),
    rows = n,
    # Small, so a 400-row case is scanned in many batches and every
    # batch-boundary decision is actually taken more than once.
    batch_rows = sample(c(3L, 7L, 64L), 1),
    seed = seed
  )
}

# Both engines reached through check(), so the two results are the same shape
# and are assembled by the same code. Holding the frame directly puts it on the
# whole-column engine; holding a RecordBatchReader over it puts it on the
# streaming one -- the internal dispatch then calls the streaming engine with
# `coerce = TRUE` and the reader's own column names, which is the call this
# comparison is about.
#
# `max_errors = Inf` on both sides: the default cap retains a bounded prefix,
# and two paths that report errors in a different order would then retain
# different rows and disagree about retention rather than about the data.
qp_run_memory <- function(case, engine) {
  holding <- if (identical(engine, "stream")) {
    dta_as_batch_reader(case$frame, batch_rows = case$batch_rows)
  } else {
    case$frame
  }

  ds <- DTADataSetTabular(
    name = "parity_memory",
    specs = case$specs,
    tables = list(t = holding)
  )
  checked <- check(ds, persist = FALSE, quiet = TRUE, max_errors = Inf)

  status <- validation_status(checked)
  status <- status[
    , setdiff(names(status), c("validated_at", "run_id", "validation_run")),
    drop = FALSE
  ]

  list(
    status = status,
    errors = qp_sort(as.data.frame(validation_errors(checked, "t"))),
    n_import_errors = checked@validation_store[["t"]]$n_import_errors
  )
}

# The n generated R-typed cases, run once on each engine.
qp_memory_results <- qp_cache(function(n) {
  lapply(seq_len(n), function(seed) {
    case <- qp_case_memory(seed)
    list(
      case = case,
      memory = qp_run_memory(case, "memory"),
      stream = qp_run_memory(case, "stream")
    )
  })
})
