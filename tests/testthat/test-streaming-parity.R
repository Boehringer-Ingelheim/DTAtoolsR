# Randomised parity between the in-memory and the streaming path.
#
# `stream = "always"` is a memory profile, not a different answer. Everything
# else in the suite compares the two paths on fixtures chosen by hand, which
# can only find the divergences someone thought to write down; this file
# generates the file, the specification and the rules together from a seed, so
# it can find the ones nobody did.
#
# The divergence that motivated it: with specs supplied, the lazy reader pinned
# every column to text while the eager one left an UNDECLARED column to Arrow's
# inference. A uniqueness rule over such a column then saw the doubles 1.5 and
# 1.5 in memory -- a duplicate -- and the strings "1.5" and "1.50" when
# streamed -- not a duplicate. Same file, two verdicts, no error anywhere.
# `XKEY` below is that column, and it is in every generated case.

# ---- the generator ----------------------------------------------------------

# Column specifications the generator draws from. ID and AGE are always taken
# (the rules key on them); the rest make the shape vary.
parity_column_pool <- function() {
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
parity_column_values <- function(name, n) {
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

# One generated case: a file on disk, the specification it is meant to satisfy,
# and the name the handler declares.
parity_case <- function(seed) {
  set.seed(seed)

  pool <- parity_column_pool()
  optional <- setdiff(names(pool), c("ID", "AGE"))
  chosen <- c("ID", "AGE", sample(optional, sample(seq_len(4), 1)))
  chosen <- names(pool)[names(pool) %in% chosen]

  n <- sample(0:400, 1)

  columns <- lapply(chosen, function(name) parity_column_values(name, n))
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

  rules <- parity_rules(chosen)

  specs <- DTAColumnSpecCollection(
    columns = pool[chosen],
    rules = rules
  )

  path <- parity_write(frame, seed, sample(c("plain", "quoted", "padded"), 1))
  declared <- basename(path)

  if (sample(c(TRUE, FALSE), 1)) {
    path <- parity_gzip(path)
  }

  list(path = path, declared = declared, specs = specs, rows = n)
}

# One to three rules, always including a uniqueness rule -- that is the axis the
# reader contract broke.
parity_rules <- function(chosen) {
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

# The header is written plain, quoted or padded: cleaning it is what used to
# make the eager reader read the file twice, and a header the two readers clean
# differently is a divergence in itself.
parity_write <- function(frame, seed, header_style) {
  path <- file.path(tempdir(), sprintf("parity_%03d.csv", seed))

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

parity_gzip <- function(path) {
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
parity_sort <- function(errors) {
  if (nrow(errors) == 0) {
    return(errors)
  }
  keys <- lapply(errors, as.character)
  out <- errors[do.call(order, keys), , drop = FALSE]
  rownames(out) <- NULL
  out
}

parity_run <- function(case, stream) {
  ds <- DTADataSetTabular(
    name = "parity",
    specs = case$specs,
    files = list(DTAFileCSV(filename = case$declared))
  )
  ds <- load_file(ds, file = case$path, handler_index = 1, stream = stream)
  checked <- check(ds, quiet = TRUE, persist = FALSE)

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
    errors = parity_sort(as.data.frame(validation_errors(checked, table_name))),
    n_import_errors = checked@validation_store[[table_name]]$n_import_errors
  )
}

# The 25 cases, generated and run once. Both tests below read the same result,
# because generating and validating each case costs about a third of a second
# and doing it twice would put this file over its time budget for nothing.
parity_results <- local({
  cache <- NULL

  function() {
    if (!is.null(cache)) {
      return(cache)
    }

    cache <<- lapply(seq_len(25), function(seed) {
      case <- parity_case(seed)
      list(
        case = case,
        eager = parity_run(case, "never"),
        lazy = parity_run(case, "always")
      )
    })

    for (entry in cache) {
      unlink(entry$case$path)
    }

    cache
  }
})

# ---- the test ---------------------------------------------------------------

test_that("the two paths agree on every generated case", {
  for (entry in parity_results()) {
    case <- entry$case
    label <- sprintf(
      "seed for %s (%d rows)",
      basename(case$path), case$rows
    )

    expect_identical(entry$lazy$status, entry$eager$status, info = label)
    expect_identical(entry$lazy$errors, entry$eager$errors, info = label)
    expect_identical(
      entry$lazy$n_import_errors, entry$eager$n_import_errors,
      info = label
    )
  }
})

test_that("the generated cases actually exercise the axes they claim to", {
  # Without this the test above would pass on 25 clean files and prove nothing.
  seen <- list(columnspec = FALSE, rule = FALSE, import = FALSE)

  for (entry in parity_results()) {
    errors <- entry$eager$errors

    if (nrow(errors) > 0) {
      sources <- as.character(errors$source)
      seen$columnspec <- seen$columnspec || any(sources == "columnspec")
      seen$rule <- seen$rule || any(sources == "rule")
    }
    seen$import <- seen$import || entry$eager$n_import_errors > 0
  }

  expect_true(seen$columnspec)
  expect_true(seen$rule)
  expect_true(seen$import)
})

test_that("the generator varies the shape it claims to vary", {
  cases <- lapply(parity_results(), function(entry) entry$case)

  n_columns <- vapply(cases, function(x) length(x$specs@columns), integer(1))
  n_rules <- vapply(cases, function(x) length(x$specs@rules), integer(1))
  gzipped <- vapply(cases, function(x) grepl("[.]gz$", x$path), logical(1))

  expect_gte(min(n_columns), 3)
  expect_lte(max(n_columns), 6)
  expect_gte(min(n_rules), 1)
  expect_lte(max(n_rules), 3)
  # Both deliveries have to occur, or half the reader is untested here.
  expect_true(any(gzipped))
  expect_true(any(!gzipped))
})

test_that("the two paths agree on a file with no rows and on one with a single row", {
  # The generator draws a row count and never happened to draw the boundaries,
  # which are where a reader is most likely to differ: a header-only file has
  # no batch at all, and a one-row file has exactly one.
  for (n in c(0, 1)) {
    columns <- list(
      ID = parity_column_values("ID", n),
      AGE = parity_column_values("AGE", n),
      XKEY = if (n == 0) character(0) else "1.50",
      XPAD = if (n == 0) character(0) else "p001"
    )
    frame <- as.data.frame(columns, stringsAsFactors = FALSE)
    path <- parity_write(frame, 900 + n, "padded")
    withr::defer(unlink(path))

    case <- list(
      path = path,
      declared = basename(path),
      specs = DTAColumnSpecCollection(
        columns = parity_column_pool()[c("ID", "AGE")],
        rules = parity_rules(c("ID", "AGE"))
      ),
      rows = n
    )

    eager <- parity_run(case, "never")
    lazy <- parity_run(case, "always")

    expect_identical(lazy$status, eager$status, info = paste(n, "rows"))
    expect_identical(lazy$errors, eager$errors, info = paste(n, "rows"))
    expect_identical(
      lazy$n_import_errors, eager$n_import_errors,
      info = paste(n, "rows")
    )
  }
})

test_that("an undeclared column read by a rule is keyed identically on both paths", {
  # The reduced form of the generator's core case, kept separately so that a
  # failure names the mechanism rather than a seed. "1.5" and "1.50" are one
  # numeric value and two strings; the answer has to be the same either way the
  # file was loaded, and -- because nothing declares XKEY -- that answer is now
  # the textual one on both.
  path <- file.path(tempdir(), "parity_undeclared_key.csv")
  withr::defer(unlink(path))
  writeLines(c("ID,XKEY", "A001,1.5", "A002,1.50", "A003,2"), path)

  specs <- DTAColumnSpecCollection(
    columns = list(
      ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE)
    ),
    rules = list(DTARuleColUnique(id = "xkey_unique", columns = "XKEY"))
  )

  case <- list(path = path, declared = basename(path), specs = specs, rows = 3)
  eager <- parity_run(case, "never")
  lazy <- parity_run(case, "always")

  expect_identical(lazy$status, eager$status)
  expect_identical(lazy$errors, eager$errors)
  # Three distinct strings, so no duplicate on either path.
  expect_true(eager$status$ok)
  expect_identical(eager$status$n_rule_errors, 0L)
})
