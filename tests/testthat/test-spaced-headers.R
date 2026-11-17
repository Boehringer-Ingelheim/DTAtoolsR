# Column ids that are header text with spaces or punctuation: "Subject ID",
# "Visit Date (UTC)", "Dose mg/kg". A column id is the exact text of the header,
# so every reader, both holdings (in memory and streamed), the rules and the
# YAML writer have to carry the name through unchanged. Each case below writes a
# real CSV, so a name that a reader, the header cleaner or a rule mangled shows
# up as a finding on the wrong column or as a missing and an undeclared one.
#
# The ids are the ones the design checked, each one an edge for some stage:
# "%Change" and "{id}" are YAML indicators, "a.b" and "x+y" are punctuation,
# "O'Brien" holds an apostrophe, and `Length 5"` ends in a bare double quote
# that the header cleaner must not mistake for a wrapper.

sh_ids <- c(
  "Subject ID", "Visit Date (UTC)", "Dose mg/kg", "%Change", "a.b",
  "x+y", "{id}", "O'Brien", "Length 5\""
)
# Which of them are text (with an allowed set) and which are numbers.
sh_is_text <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE)
sh_n <- 4L

# The value in row `i` of column `j`; written out by a formula, so no value
# comes from the software under test.
sh_value <- function(j, i) {
  if (sh_is_text[[j]]) sprintf("%s%d", LETTERS[[j]], i) else as.character(10L * j + i)
}

sh_clean_columns <- function() {
  stats::setNames(
    lapply(seq_along(sh_ids), function(j) vapply(seq_len(sh_n), function(i) sh_value(j, i), "")),
    sh_ids
  )
}

# RFC 4180: a field with a comma, a quote or a line break is quoted, and a
# quote inside it is doubled. So the header `Length 5"` is written `"Length 5"""`.
sh_csv_field <- function(x) {
  quote_it <- grepl("[,\"\n]", x)
  x[quote_it] <- paste0("\"", gsub("\"", "\"\"", x[quote_it], fixed = TRUE), "\"")
  x
}

# Write the columns (a named list of character vectors) as a CSV, byte for byte,
# and return its path.
sh_write_csv <- function(columns, env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".csv", .local_envir = env)
  rows <- vapply(
    seq_along(columns[[1]]),
    function(i) paste(sh_csv_field(vapply(columns, function(x) x[[i]], "")), collapse = ","),
    ""
  )
  text <- paste0(paste(c(paste(sh_csv_field(names(columns)), collapse = ","), rows), collapse = "\n"), "\n")
  con <- file(path, "wb")
  on.exit(close(con))
  writeBin(charToRaw(enc2utf8(text)), con)
  path
}

sh_column_specs <- function(ids = sh_ids) {
  stats::setNames(
    lapply(ids, function(id) {
      j <- match(id, sh_ids)
      if (sh_is_text[[j]]) {
        DTAColumnSpec(
          id = id, type = "SAS Char", nullable = FALSE,
          values = vapply(seq_len(sh_n), function(i) sh_value(j, i), "")
        )
      } else {
        DTAColumnSpec(id = id, type = "SAS Num", nullable = FALSE)
      }
    }),
    ids
  )
}

sh_check <- function(path, specs, stream) {
  ds <- DTADataSetTabular(
    id = "t", specs = specs,
    files = list(DTAFileCSV(filename = basename(path)))
  )
  ds <- load_file(ds, file = path, handler_index = 1, name = "t", stream = stream)
  check(ds, quiet = TRUE, persist = FALSE)
}

sh_messages <- function(ds) as.data.frame(messages(ds))

sh_streams <- c("never", "always")

# ---- the specification accepts the names ------------------------------------

test_that("every id builds a column spec that keeps the id verbatim", {
  for (id in sh_ids) {
    expect_identical(DTAColumnSpec(id = id, type = "SAS Char")@id, id, info = id)
  }
  specs <- DTAColumnSpecCollection(columns = sh_column_specs())
  expect_identical(names(specs@columns), sh_ids)
  expect_identical(unname(vapply(specs@columns, function(x) x@id, "")), sh_ids)
})

# ---- a clean file ----------------------------------------------------------------

test_that("a clean file whose headers hold spaces and punctuation has no findings, in memory and streamed", {
  path <- sh_write_csv(sh_clean_columns())
  for (stream in sh_streams) {
    ds <- sh_check(path, DTAColumnSpecCollection(columns = sh_column_specs()), stream)
    expect_true(validation_status(ds)$ok, info = stream)
    expect_identical(nrow(sh_messages(ds)), 0L, info = stream)
    # the reader presented every header as spelled, in file order
    expect_identical(names(tables(ds)[[1]]), sh_ids, info = stream)
  }
})

# ---- one bad cell in one column ----------------------------------------------------

test_that("one bad cell is exactly one finding, on its own column and row, in both holdings", {
  for (j in seq_along(sh_ids)) {
    row <- ((j - 1L) %% sh_n) + 1L
    columns <- sh_clean_columns()
    # text outside the allowed set; an empty cell in a required number column
    columns[[j]][[row]] <- if (sh_is_text[[j]]) "ZZ" else ""
    path <- sh_write_csv(columns)
    for (stream in sh_streams) {
      ds <- sh_check(path, DTAColumnSpecCollection(columns = sh_column_specs()), stream)
      info <- paste0(sh_ids[[j]], " (", stream, ")")
      found <- sh_messages(ds)
      expect_false(validation_status(ds)$ok, info = info)
      expect_identical(nrow(found), 1L, info = info)
      expect_identical(found$column, sh_ids[[j]], info = info)
      expect_equal(found$row, row, info = info)
      expect_identical(found$source, "columnspec", info = info)
    }
  }
})

# ---- names that differ only by whitespace are different names -------------------

test_that("a header with a double space does not match a spec id with one space", {
  path <- sh_write_csv(list(`Subject  ID` = c("A1", "A2"), `Dose mg/kg` = c("31", "32")))
  specs <- DTAColumnSpecCollection(columns = sh_column_specs(c("Subject ID", "Dose mg/kg")))
  for (stream in sh_streams) {
    found <- sh_messages(sh_check(path, specs, stream))
    # the declared name is missing from each row, the file's own name is undeclared
    expect_true(
      all(c("required", "additionalProperties") %in% found$keyword),
      info = stream
    )
    expect_identical(found$column[found$keyword == "additionalProperties"], "Subject  ID", info = stream)
    expect_equal(sort(found$row[found$keyword == "required"]), c(1, 2), info = stream)
  }
})

test_that("a spec id with a double space matches the header with a double space, in both holdings", {
  path <- sh_write_csv(list(`Subject  ID` = c("A1", "A2")))
  specs <- DTAColumnSpecCollection(columns = list(
    `Subject  ID` = DTAColumnSpec(id = "Subject  ID", type = "SAS Char", nullable = FALSE)
  ))
  for (stream in sh_streams) {
    ds <- sh_check(path, specs, stream)
    expect_true(validation_status(ds)$ok, info = stream)
    expect_identical(names(tables(ds)[[1]]), "Subject  ID", info = stream)
  }
})

test_that("a header written with surrounding spaces and quotes still names the trimmed id", {
  # The readers trim a header's edges, which is why an id may not start or end with a space.
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("  Subject ID  ,\" Dose mg/kg \"", "A1,31", "A2,32"), path)
  specs <- DTAColumnSpecCollection(columns = sh_column_specs(c("Subject ID", "Dose mg/kg")))
  for (stream in sh_streams) {
    ds <- sh_check(path, specs, stream)
    expect_true(validation_status(ds)$ok, info = stream)
    expect_identical(names(tables(ds)[[1]]), c("Subject ID", "Dose mg/kg"), info = stream)
  }
})

# ---- rules on spaced names, memory against stream ---------------------------------

test_that("a two-column col_unique key on spaced names is judged identically in memory and streamed", {
  rules <- list(DTARuleColUnique(id = "ukey", columns = c("Subject ID", "Visit Date (UTC)")))
  specs <- DTAColumnSpecCollection(
    columns = sh_column_specs(c("Subject ID", "Visit Date (UTC)")),
    rules = rules
  )
  # (A1, B1) occurs in rows 1 and 4; no single column is unique on its own
  dup <- sh_write_csv(list(
    `Subject ID` = c("A1", "A1", "A2", "A1"),
    `Visit Date (UTC)` = c("B1", "B2", "B1", "B1")
  ))
  clean <- sh_write_csv(list(
    `Subject ID` = c("A1", "A1", "A2", "A2"),
    `Visit Date (UTC)` = c("B1", "B2", "B1", "B2")
  ))

  found <- lapply(stats::setNames(sh_streams, sh_streams), function(stream) {
    sh_messages(sh_check(dup, specs, stream))
  })
  expect_identical(found$never, found$always)
  expect_identical(found$never$source, "rule")
  expect_identical(found$never$rule_id, "ukey")
  expect_identical(nrow(found$never), 1L)

  for (stream in sh_streams) {
    expect_true(validation_status(sh_check(clean, specs, stream))$ok, info = stream)
  }
})

test_that("a col_range on a spaced numeric name is judged identically in memory and streamed", {
  rules <- list(DTARuleColRange(id = "rg", columns = "Dose mg/kg", min = 0, max = 100))
  specs <- DTAColumnSpecCollection(columns = sh_column_specs("Dose mg/kg"), rules = rules)
  over <- sh_write_csv(list(`Dose mg/kg` = c("10", "150", "30", "40")))
  within <- sh_write_csv(list(`Dose mg/kg` = c("10", "20", "30", "40")))

  found <- lapply(stats::setNames(sh_streams, sh_streams), function(stream) {
    sh_messages(sh_check(over, specs, stream))
  })
  expect_identical(found$never, found$always)
  expect_identical(found$never$rule_id, "rg")
  expect_identical(nrow(found$never), 1L)

  for (stream in sh_streams) {
    expect_true(validation_status(sh_check(within, specs, stream))$ok, info = stream)
  }
})

test_that("a col_condition across spaced names is judged identically in memory and streamed", {
  rules <- list(DTARuleColCondition(
    id = "cc",
    condition = list(`Subject ID` = list(equals = "A1")),
    then = list(`x+y` = list(less = 5))
  ))
  specs <- DTAColumnSpecCollection(
    columns = sh_column_specs(c("Subject ID", "x+y")),
    rules = rules
  )
  # row 2 has Subject ID A1 and x+y 9, which the THEN refuses
  path <- sh_write_csv(list(`Subject ID` = c("A2", "A1", "A2", "A1"), `x+y` = c("9", "9", "9", "1")))
  # the same two columns, no IF row breaks the THEN
  ok <- sh_write_csv(list(`Subject ID` = c("A2", "A1", "A2", "A1"), `x+y` = c("9", "4", "9", "1")))

  found <- lapply(stats::setNames(sh_streams, sh_streams), function(stream) {
    sh_messages(sh_check(path, specs, stream))
  })
  expect_identical(found$never, found$always)
  expect_identical(found$never$rule_id, "cc")
  expect_identical(nrow(found$never), 1L)
  for (stream in sh_streams) {
    expect_true(validation_status(sh_check(ok, specs, stream))$ok, info = stream)
  }
})

test_that("a col_unique rule on each spaced name flags exactly the columns that repeat a value", {
  rules <- lapply(seq_along(sh_ids), function(j) {
    DTARuleColUnique(id = sprintf("u%02d", j), columns = sh_ids[[j]])
  })
  specs <- DTAColumnSpecCollection(columns = sh_column_specs(), rules = rules)
  repeating <- seq(1L, length(sh_ids), by = 2L)
  columns <- sh_clean_columns()
  for (j in repeating) {
    columns[[j]][[2L]] <- columns[[j]][[1L]]
  }
  path <- sh_write_csv(columns)

  found <- lapply(stats::setNames(sh_streams, sh_streams), function(stream) {
    sh_messages(sh_check(path, specs, stream))
  })
  expect_identical(found$never, found$always)
  expect_identical(sort(found$never$rule_id), sprintf("u%02d", repeating))
  expect_true(all(found$never$source == "rule"))
})

# ---- the YAML writer and reader ------------------------------------------------------

test_that("write_columns_to_yaml and import_specs_from_yaml give back identical ids and rule columns", {
  specs <- DTAColumnSpecCollection(
    columns = sh_column_specs(),
    rules = list(
      DTARuleColUnique(id = "ukey", columns = c("Subject ID", "Visit Date (UTC)")),
      DTARuleColRange(id = "rg", columns = "%Change", min = 0, max = 100)
    )
  )
  f <- withr::local_tempfile(fileext = ".yaml")
  write_columns_to_yaml(specs, f)

  back <- suppressMessages(import_specs_from_yaml(f))

  expect_identical(names(back@columns), sh_ids)
  expect_identical(unname(vapply(back@columns, function(x) x@id, "")), sh_ids)
  expect_identical(back@rules[[1]]@columns, c("Subject ID", "Visit Date (UTC)"))
  expect_identical(back@rules[[2]]@columns, "%Change")
})

test_that("a plain spaced id is written without quotes and a YAML indicator is quoted", {
  specs <- DTAColumnSpecCollection(columns = sh_column_specs(c("Subject ID", "Dose mg/kg", "%Change", "{id}")))
  f <- withr::local_tempfile(fileext = ".yaml")
  write_columns_to_yaml(specs, f)

  lines <- trimws(readLines(f, warn = FALSE))

  expect_true("id: Subject ID" %in% lines)
  expect_true("id: Dose mg/kg" %in% lines)
  # a leading % or { would not parse as text: the writer quotes it (either quote style)
  expect_true(any(grepl("^id: ['\"]%Change['\"]$", lines)))
  expect_true(any(grepl("^id: ['\"]\\{id\\}['\"]$", lines)))
})

test_that("a YAML document that declares spaced ids lints clean and loads, ids as written", {
  p <- withr::local_tempfile(fileext = ".yaml")
  writeLines(c(
    "metadata:", "  title: T", "  version: '1'",
    "datasets:",
    "  - id: d",
    "    type: tabular",
    "    files: { filename: \"d.csv\", type: csv }",
    "    columns:",
    "      - id: Subject ID",
    "        type: SAS Char",
    "        nullable: No",
    "      - id: Visit Date (UTC)",
    "        type: SAS Char",
    "        nullable: No",
    "      - id: \"%Change\"",
    "        type: SAS Num",
    "        nullable: No",
    "    rules:",
    "      - id: ukey",
    "        type: col_unique",
    "        columns: [Subject ID, Visit Date (UTC)]"
  ), p)

  expect_identical(nrow(lint_dta(p)), 0L)
  dta <- suppressWarnings(suppressMessages(read_dta_from_yaml(p)))
  specs <- dta@datasets[["d"]]@specs
  expect_identical(names(specs@columns), c("Subject ID", "Visit Date (UTC)", "%Change"))
  expect_identical(specs@rules[[1]]@columns, c("Subject ID", "Visit Date (UTC)"))
})

test_that("the JSON writer and reader keep spaced ids too", {
  specs <- DTAColumnSpecCollection(columns = sh_column_specs())
  f <- withr::local_tempfile(fileext = ".json")
  write_columns_to_json(specs, f)

  back <- suppressMessages(import_specs_from_json(f))

  expect_identical(names(back@columns), sh_ids)
})
