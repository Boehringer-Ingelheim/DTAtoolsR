#' @title DTADataSetTabular Class
#' @description Handles tabular data with column specifications and rules.
#' @import S7
#' @importFrom cli cli_alert_info cli_abort
#' @importFrom arrow arrow_table
#' @importFrom tools md5sum
#' @param name Character. Name of the container.
#' @param specs A DTAColumnSpecCollection object specifying the column specs.
#' @param files A list of DTAFile objects specifying input file information.
#' @param tables A named list of tabular objects. A materialised entry
#'   (data.frame or Arrow Table) is typed by `specs` and converted to an Arrow
#'   Table before storage. A lazy entry (Arrow Dataset, `arrow_dplyr_query`, or
#'   RecordBatchReader) is stored exactly as given -- it is typed and checked
#'   for import issues later, at scan time, in `check()`.
#' @param description Character or NA. Free-text description of the dataset.
#' @param template_source Character or NA. Source of the template used to
#'   generate the dataset specification.
#' @param template_version Character or NA. Version of the template used.
#' @param template_date Character or NA. Date of the template used.
#' @details
#' A table handed to this constructor is a table already in memory, which is
#' the case [check()]'s retained-error cap is least suited to. `max_errors`
#' (default `getOption("DTAtools.max_errors", 10000L)`) exists because on a
#' large dirty file retention is one row per bad cell and an unbounded error
#' frame exhausts memory; a table small enough to have been built in R has
#' nothing to protect. Check such a table with `check(..., max_errors = Inf)`
#' when the complete per-cell detail is wanted -- there is no spill on the
#' materialising path, so detail dropped by the cap is recovered only by
#' checking again. The cap never affects the answer: the reported counts and
#' the verdict on every axis are exact at any cap, including the default.
#' @return An object of class DTADataSetTabular
#' @examples
#' # Create sample tables
#' table1 <- data.frame(STUDYID = c("1234", "1234", "1234"), VISIT = c("V03", "V03", "EOT"))
#' table2 <- data.frame(STUDYID = c("1234", "1234", "1234"), VISIT = c("EOT", "V05", "EOT"))
#'
#' # List of tables
#' tables <- list(table1 = table1, table2 = table2)
#'
#' # Create the DTADataSetTabular object
#' data_obj <- DTADataSetTabular(
#'   name = "example",
#'   specs = create_example_DTAColumnSpecCollection(1),
#'   tables = tables
#' )
#' @export
DTADataSetTabular <- S7::new_class(
  "DTADataSetTabular",
  parent = DTADataSet,
  constructor = function(
    name,
    specs,
    tables = list(),
    files = list(),
    description = NULL,
    template_source = NULL,
    template_version = NULL,
    template_date = NULL
  ) {
    if (inherits(files, "DTAtools::DTAFile")) {
      files <- list(files)
    }

    # Type every table by its column specs before it is stored, so the declared
    # type -- not the reader's per-column inference -- decides what each column
    # holds. Values that cannot be represented become NA and are recorded as
    # import issues, both here and on the table itself.
    #
    # A lazy holding (Dataset / arrow_dplyr_query / RecordBatchReader -- the same
    # set the class validator admits, via dta_table_is_lazy()) is passed through
    # UNTOUCHED: neither coerced nor collected. dta_coerce_table_to_specs()
    # materialises its input with as.data.frame(), and arrow::as_arrow_table()
    # COLLECTS a Dataset/query and DRAINS a RecordBatchReader -- so doing either
    # here would silently pull a table too large for memory into memory at
    # construction time, which is exactly what admitting a lazy holding exists to
    # avoid. Nothing is lost by skipping it: the lazy load_file() path already
    # pins column types when it opens the file, and check() applies
    # dta_coerce_table_to_specs() itself, per batch, while scanning -- so the
    # table is typed at scan time either way.
    is_lazy <- vapply(tables, dta_table_is_lazy, logical(1))

    # A data frame is coerced AS a data frame and converted to Arrow exactly
    # once, at the end. Converting it up front instead made
    # dta_coerce_table_to_specs() see an Arrow Table, which it immediately
    # turned back into a data frame with as.data.frame() and then rebuilt as a
    # Table -- three conversions of the whole table where one is needed, and on
    # a 1e6 x 20 frame that detour was half the construction cost and a copy of
    # the data in peak heap. The only thing it bought was the content stamp,
    # which the coercion applies only to an Arrow input; it is applied here
    # instead, from the same dta_table_content_hash() of the same coerced frame
    # (issues attribute included), so a frame-built table and an Arrow-built one
    # carrying the same data are stamped identically and check() skips both.
    #
    # That identity holds only while the frame survives the Arrow round trip
    # unchanged, because dta_table_change_signal() falls back to hashing
    # as.data.frame() of the stored Table. dta_frame_is_arrow_stable() names the
    # types for which it does not (integer64, difftime, a POSIXct with no named
    # timezone); such a frame takes the original route, where the stamp is again
    # taken from the round-tripped frame.
    #
    # Anything else materialised -- an Arrow Table, a RecordBatch -- keeps the
    # original route too: the coercion stamps the Table it returns, and
    # as_arrow_table() below both normalises a RecordBatch to a Table (the class
    # validator admits only Tables) and is a no-op for a Table.
    #
    # A lazy holding (Dataset / arrow_dplyr_query / RecordBatchReader) is not in
    # `tables[!is_lazy]` at all and is passed through untouched, for the reason
    # given above.
    coerced <- lapply(tables[!is_lazy], function(tbl) {
      if (dta_frame_is_arrow_stable(tbl)) {
        result <- dta_coerce_table_to_specs(tbl, specs)
        return(list(
          table = dta_stamp_table_hash(
            arrow::as_arrow_table(result$table),
            dta_table_content_hash(result$table)
          ),
          issues = result$issues
        ))
      }

      result <- dta_coerce_table_to_specs(arrow::as_arrow_table(tbl), specs)
      list(table = arrow::as_arrow_table(result$table), issues = result$issues)
    })

    # Transform the materialised entries to arrow tables; lazy entries keep the
    # class they arrived with.
    tables[!is_lazy] <- lapply(coerced, function(result) result$table)

    import_issues <- lapply(coerced, function(result) result$issues)
    import_issues <- import_issues[
      vapply(import_issues, function(issues) nrow(issues) > 0, logical(1))
    ]
    # Subsetting a named list keeps the (empty) names attribute, and a named
    # empty list is not `identical()` to `list()`. Normalise, so "no import
    # issues" is one value rather than two.
    if (length(import_issues) == 0) {
      import_issues <- list()
    }

    new_object(
      .parent = DTADataSet(
        name = name,
        type = "tabular",
        files = files,
        template_source = template_source,
        template_version = template_version,
        template_date = template_date,
        description = description
      ),
      specs = specs,
      tables = tables,
      validation_index = list(),
      validation_store = list(),
      import_issues = import_issues,
      validation_artifact_dir = NULL
    )
  },
  properties = list(
    specs = class_DTAColumnSpecCollection,
    tables = S7::new_property(S7::class_list, default = list()),
    validation_index = S7::new_property(S7::class_list, default = list()),
    validation_store = S7::new_property(S7::class_list, default = list()),
    # Import issues detected while typing a table, keyed by table name.
    import_issues = S7::new_property(S7::class_list, default = list()),
    validation_artifact_dir = class_character_or_null
  ),
  validator = function(self) {
    # check if all elements of list self@tables inherit from "Table"
    if (!inherits(self@specs, "DTAtools::DTAColumnSpecCollection")) {
      cli_abort("Property 'specs' must be of class 'DTAColumnSpecCollection'")
    }

    # A tabular dataset parses every file it is given, so each handler has to be
    # one that can actually read: it supplies the separator, header flag and
    # quoting `read_file()`/`open_file()` need, and only a DTAFileTabular
    # carries them. A reader-less handler (DTAFileAny, or a bare DTAFile) would
    # construct happily here and then abort deep inside the read with
    # "This method is not implemented", naming the wrong problem long after the
    # document that caused it was accepted.
    #
    # This is also what keeps `type: any` out of a tabular dataset without
    # threading the dataset's type down through dta_file_handlers_from_list()
    # into DTAFileFactory(): the handler is judged by the dataset that holds it,
    # which is the only place that knows whether it will be parsed.
    if (length(self@files) > 0) {
      readable <- vapply(
        self@files,
        function(file_info) inherits(file_info, "DTAtools::DTAFileTabular"),
        logical(1)
      )
      if (!all(readable)) {
        offending <- vapply(
          self@files[!readable],
          function(file_info) {
            paste(tryCatch(file_info@filename, error = function(e) "<unnamed>"),
              collapse = ", "
            )
          },
          character(1)
        )
        cli_abort(c(
          "A tabular dataset can only hold file handlers that can be read.",
          x = "Handler{?s} {.field {offending}} {?is/are} not tabular.",
          i = "Use a {.cls DTAFileCSV} or {.cls DTAFileTSV} here, or declare the dataset as {.code type: file} if its files are not parsed."
        ))
      }
    }

    # A table may be materialised (an Arrow Table) or lazy (a Dataset, a query
    # over one, or a batch reader). The lazy forms exist so a file larger than
    # memory can be validated by scanning it, which an in-memory Table forbids
    # by construction. Everything downstream either scans batches or converts,
    # so both are usable; what is rejected is a plain data frame or a list,
    # which would silently skip Arrow entirely.
    if (length(self@tables) > 0) {
      acceptable <- vapply(
        self@tables,
        function(x) {
          inherits(x, "Table") ||
            inherits(x, "Dataset") ||
            inherits(x, "arrow_dplyr_query") ||
            inherits(x, "RecordBatchReader")
        },
        logical(1)
      )
      if (!all(acceptable)) {
        cli_abort(
          "All elements of 'tables' must be an Arrow {.cls Table}, {.cls Dataset}, {.cls arrow_dplyr_query} or {.cls RecordBatchReader}."
        )
      }
    }

    # if(length(self@tables) > 0 && !all(sapply(self@tables, function(x) inherits(x, "arrow::ArrowTabular")))) {
    #  cli_abort("All elements of 'tables' must be of class 'arrow::ArrowTabular'")
    # }

    # check if list holding the validation index and validation store are of the same length
    # if(length(self@validation_index) != length(self@validation_store)) {
    #  cli_abort("Properties 'validation_index' and 'validation_store' must be of the same length")
    # }

    # Deliberately NOT dir.exists(): the property remembers WHERE artifacts were
    # written, and the directory it names is temporary by default. An object
    # saved with saveRDS() and restored in a later session -- or simply after
    # tempdir() was cleaned -- then carries a path that no longer exists, and
    # requiring the directory here made EVERY subsequent modification of that
    # object abort (S7 revalidates on each property assignment), so it could no
    # longer be loaded into, cleared, or even checked with persist = FALSE.
    # check(persist = TRUE) creates the directory itself, which is the only
    # moment its existence actually matters.
    if (
      !is.null(self@validation_artifact_dir) &&
        (!is.character(self@validation_artifact_dir) ||
          length(self@validation_artifact_dir) != 1 ||
          is.na(self@validation_artifact_dir))
    ) {
      cli_abort("Property 'validation_artifact_dir' must be a single directory path or NULL")
    }

    # if tables are present, check if the column names of the tables match the column names in the specs if specs are present
    # if(length(self@tables) > 0 && !is.null(self@specs)) {
    #  spec_column_names <- sapply(self@specs@columns, function(x) x@name)
    #  for(table in self@tables) {
    #    table_column_names <- colnames(table)
    #    if(!all(spec_column_names %in% table_column_names)) {
    #      cli_abort("Column names in 'specs' do not match column names in 'tables'")
    #    }
    #  }
    # }

    # if list of tables is present then list of validation index and store cannot be larger than the list of tables
    if (length(self@tables) > 0 && (length(self@validation_index) > length(self@tables) || length(self@validation_store) > length(self@tables) || length(self@import_issues) > length(self@tables))) {
      cli_abort("Properties 'validation_index', 'validation_store' and 'import_issues' cannot be larger than the number of tables in 'tables'")
    }
  }
)


#' @title Get Column by ID Method
#' @description
#' Method to get a column format by its ID from the collection.
#' @param x An object of class DTADataSetTabular
#' @param ... Additional named arguments:
#'   \describe{
#'     \item{id}{Character. The ID of the column to retrieve.}
#'   }
#' @return A DTAColumnSpec object corresponding to the specified ID.
#' @examples
#' ds <- create_example_DTADataSetTabular(2)
#' colspec(ds, "STUDYID")
#' @usage colspec(x, ...)
#' @name colspec
# colspec <- new_generic("colspec", "x") # was already initialized

#' @export
method(colspec, DTADataSetTabular) <- function(x, id) {
  return(colspec(x@specs, id))
}


#' @title Get DTAColumnSpecCollection (specs) from DTADataSetTabular Object
#' @description
#' Method to extract the full DTAColumnSpecCollection from a DTADataSetTabular object.
#' @param x An object of class DTADataSet.
#' @param ... Additional arguments (not used).
#' @return A DTAColumnSpecCollection object.
#' @examples
#' ds <- create_example_DTADataSetTabular(2)
#' specs(ds)
#' @name specs-DTADataSetTabular
#' @export
specs <- new_generic("specs", "x")

#' @export
method(specs, DTADataSetTabular) <- function(x) {
  return(x@specs)
}


#' @title Get table from DTADataSetTabular Object
#' @description
#' Extract a table from the tables in a DTADataSetTabular object.
#' @param x An object of class DTADataSet.
#' @param id Character or numeric. Name or index of the table to retrieve.
#' @param ... Not used by current methods; reserved for future extensions.
#' @return An Arrow Table object.
#' @importFrom cli cli_abort
#' @examples
#' ds <- create_example_DTADataSetTabular(2)
#' get_table(ds, 1)
#' get_table(ds, "tab1")
#' @name get_table-DTADataSetTabular
#' @export
get_table <- new_generic("get_table", "x", function(x, id = 1, ...) {
  S7_dispatch()
})

#' @export
method(get_table, DTADataSetTabular) <- function(x, id = 1) {
  tables <- x@tables

  if (length(tables) == 0) {
    cli::cli_abort("No tables found in the container.")
  }

  if (is.character(id)) {
    if (!id %in% names(tables)) {
      cli::cli_abort("No table named {id} found in the container.")
    }
    return(tables[[id]])
  }

  if (is.numeric(id)) {
    # A single, non-missing whole number. Without this, `[[` on a fractional
    # index truncates silently (id = 1.9 would return table 1), `id < 1 ||
    # id > length(tables)` on `id = NA` raises an opaque "missing value where
    # TRUE/FALSE needed", and a length-2 id raises a generic "length > 1"
    # condition error that names none of the above as the actual problem.
    if (length(id) != 1 || is.na(id) || id != trunc(id)) {
      cli::cli_abort("Argument 'id' must be a single, non-missing whole number.")
    }
    if (id < 1 || id > length(tables)) {
      cli::cli_abort("Index {id} is out of bounds.")
    }
    return(tables[[id]])
  }

  cli::cli_abort("Argument 'id' must be a character (name) or numeric (index).")
}


#' @title List of tables labels within DTADataSetTabular Object
#' @description
#' Method to get a all tables labels within a DTADataSetTabular Object.
#' @param object An object of class DTADataSetTabular
#' @param ... Additional arguments (not used).
#' @return A character vector with table names.
#' @examples
#' ds <- create_example_DTADataSetTabular(2)
#' labels(ds)
#' @name labels
#' @export
# `labels` already exists as a base R (S3) generic, so this extends it rather
# than replacing it -- exactly the pattern already used for `names`/`print`
# below and in DTAColumnSpecCollection-class.R. Unconditionally creating a
# brand-new S7 generic under this name would, once exported, mask
# base::labels() entirely for every class once the package is attached, and
# the new generic has no fallback method for anything but
# DTADataSetTabular -- `labels()` on an lm/dendrogram/etc. would then abort
# with "Can't find method" instead of falling back to base's own dispatch.
if (!exists("labels", mode = "function")) {
  labels <- new_generic("labels", "x")
}
#' @export
# Method formals must match base::labels' own formals -- function(object, ...)
# -- rather than the function(x) used for names()/print() below. Those two
# match base::names()/base::print() because those generics happen to declare
# their dispatch argument as `x`; base::labels() declares it as `object`.
# R CMD check's S3 generic/method consistency check compares the registered
# method's formals against the real base generic's formals (arg names and
# all), so mismatching here reintroduces the WARNING this fix removes. S7
# does not enforce or care about the argument name -- `method<-` merely calls
# registerS3method() for base S3 generics like this one -- so this is purely
# to satisfy R CMD check.
method(labels, DTADataSetTabular) <- function(object, ...) {
  return(names(object@tables))
}

#' @title Write DTA Table to File
#' @description
#' Write a named DTA table saved in a DTADataSetTabular object to a file.
#' @importFrom dplyr arrange across everything
#' @importFrom magrittr %>%
#' @importFrom utils write.table
#' @importFrom R.utils gzip
#' @importFrom cli cli_alert_info cli_alert_success format_message cli_abort
#' @importFrom tools md5sum
#' @export
#' @param DTADataSetTabular An object of class DTADataSet.
#' @param table Character. The name of the table within the DTADataSetTabular object to write.
#' @param filename Character. The name of the file to write to.
#' @param arrange_by Character vector. Columns to arrange the table by. NULL, Table won't be arranged. "all" (Default) -> Table will be arranged by all columns.
#' @param arrange_desc Logical. Whether to arrange the table in descending order. Default is FALSE.
#' @param sep Character. The field separator string. Default is ",".
#' @param na Character. The string to use for missing values in the data. Default is "".
#' @param row.names Logical. Row names provided.
#' @param quote Logical. Use of quotes
#' @param overwrite Logical. Whether to overwrite the file if it exists. Default
#'   is FALSE, so a call against an existing path aborts unless `overwrite =
#'   TRUE` is passed explicitly.
#' @param compression Character. Compression method, either "none" or "gzip". Default is "none".
#' @param get_md5sum Logical. Whether to calculate and print the MD5 checksum of the file. MD5SUM and number of rows and columns of file will be also saved in an additional file. Default is TRUE.
#' @param write_md5sum_to_file Logical. Whether to calculate and print the MD5 checksum of the file. MD5SUM and number of rows and columns of file will be also saved in an additional file. Default is TRUE.
#' @param quiet Logical. If TRUE, suppresses console output. Default is FALSE.
#' @param ... Additional arguments passed to write.table.
#' @return NULL. The function writes the table to a file.
#' @examples
#' ds <- create_example_DTADataSetTabular(2)
#' out_file <- tempfile(fileext = ".tsv")
#' write_table_to_file(
#'   ds,
#'   table = "tab1",
#'   filename = out_file,
#'   sep = "\t",
#'   arrange_by = c("STUDYID", "VISIT")
#' )
#' unlink(out_file)
write_table_to_file <- function(
  DTADataSetTabular,
  table,
  filename,
  arrange_by = "all",
  arrange_desc = FALSE,
  sep = "\t",
  na = "",
  row.names = FALSE,
  overwrite = FALSE,
  quote = FALSE,
  compression = c("none", "gzip"),
  get_md5sum = TRUE,
  write_md5sum_to_file = TRUE,
  quiet = FALSE,
  ...
) {
  compression <- match.arg(compression)

  # Check if the table exists in the DTADataSetTabular
  if (!table %in% names(DTADataSetTabular@tables)) {
    cli::cli_abort(c(
      "Table name not found!",
      x = "Table with the name '{table}' not found in the DTADataSetTabular object.",
      i = "Use 'labels(<DTADataSetTabular>)' to print names of all tables in object."
    ))
  }

  table_data <- DTADataSetTabular@tables[[table]]

  # Arrange the table by specified columns
  if (!is.null(arrange_by)) {
    if (length(arrange_by) == 1 && identical(arrange_by, "all")) {
      if (!isTRUE(quiet)) {
        cli::cli_alert_info("Arrange table by all columns.")
      }
      if (isTRUE(arrange_desc)) {
        table_data <- table_data %>%
          dplyr::arrange(dplyr::across(dplyr::everything(), dplyr::desc))
      } else {
        table_data <- table_data %>%
          dplyr::arrange(dplyr::across(dplyr::everything()))
      }
    } else {
      if (!isTRUE(quiet)) {
        cli::cli_alert_info("Arrange table by {arrange_by}.")
      }
      if (isTRUE(arrange_desc)) {
        table_data <- table_data %>%
          dplyr::arrange(dplyr::across(dplyr::all_of(arrange_by), dplyr::desc))
      } else {
        table_data <- table_data %>%
          dplyr::arrange(!!!rlang::syms(arrange_by))
      }
    }
  }

  # Materialise explicitly, with the original column names preserved.
  # write.table()'s own `if (!is.data.frame(x) && !is.matrix(x)) x <-
  # data.frame(x)` would otherwise coerce an Arrow Table/arrow_dplyr_query by
  # calling base data.frame() on it directly, whose default check.names = TRUE
  # rewrites a non-syntactic column name ("Subject ID" -> "Subject.ID",
  # "2024 VAL" -> "X2024.VAL") -- so the written header would silently stop
  # matching the specs the dataset was validated against. Arrow's own
  # as.data.frame() method does not mangle names (it builds the frame via
  # `$to_data_frame()`, not via `data.frame()`), but check.names = FALSE is
  # passed anyway to say so explicitly. Once table_data is already a
  # data.frame, write.table()'s guard above is a no-op.
  table_data <- as.data.frame(table_data, check.names = FALSE)

  # Check if the file exists and handle overwrite
  if (file.exists(filename) && !overwrite) {
    cli::cli_abort(c(
      "{filename} already exists!",
      i = "Specify 'overwrite = TRUE' to overwrite the current file."
    ))
  }

  # Write the table to a file
  if (compression == "gzip") {
    if (!isTRUE(quiet)) {
      cli::cli_alert_info("Write table in gzip format to {filename}.")
    }
    temp_file <- tempfile()
    write.table(
      table_data,
      file = temp_file,
      sep = sep,
      na = na,
      row.names = row.names,
      quote = quote,
      ...
    )
    R.utils::gzip(temp_file, destname = filename, overwrite = overwrite)
  } else {
    if (!isTRUE(quiet)) {
      cli::cli_alert_info("Write table to {filename}.")
    }
    write.table(
      table_data,
      file = filename,
      na = na,
      row.names = row.names,
      sep = sep,
      quote = quote,
      ...
    )
  }

  # Print a success message
  if (!isTRUE(quiet)) {
    cli::cli_alert_success("File {filename} written successfully.")
  }

  # Calculate md5sum
  if (get_md5sum) {
    md5sum <- write_metadata(
      filename,
      table_data,
      write_to_file = write_md5sum_to_file,
      quiet = quiet
    )
  } else {
    md5sum <- NA
  }

  invisible(list(
    tables = table_data,
    table = table,
    md5sum = md5sum
  ))
}


#' @title Get column specs from DTADataSetTabular Object
#' @description
#' Method to get columns specifications from DTADataSetTabular
#' @param x An object of class DTADataSetTabular
#' @param ... Not used by current methods; reserved for future extensions.
#' @return A list with metadata information
#' @examples
#' library(DTAtools)
#' ds <- create_example_DTADataSetTabular()
#' columns(ds)
#' @name columns
#' @export
columns <- new_generic("columns", "x")
#' @export
method(columns, DTADataSetTabular) <- function(x) {
  return(x@specs@columns)
}

#' @title Get Rules
#' @description
#' Method to get Rules from DTADataSet.
#' @param x An object of class DTADataSetTabular
#' @return A list of DTARule objects, or NULL if no rules are defined.
#' @examples
#' ds <- create_example_DTADataSetTabular(2)
#' rules(ds)
#' @name rules
#' @export
method(rules, DTADataSetTabular) <- function(x, ...) {
  return(x@specs@rules)
}


#' @title Create Example DTADataSetTabular
#' @description
#' S7 method to create and return an example DTADataSetTabular object.
#' @importFrom cli cli_abort
#' @param index Integer. Index of the example to create.
#'
#' @return An example DTADataSetTabular object.
#' @examples
#' library(DTAtools)
#' create_example_DTADataSetTabular()
#' @export
create_example_DTADataSetTabular <- function(index = 1) {
  # Create sample tables
  table1 <- arrow_table(data.frame(
    STUDYID = c("STUDY001", "STUDY001", "STUDY001"),
    SUBJID = c("001", "002", "003"),
    VISIT = c("SCREENING", "BASELINE", "WEEK_4"),
    AGE = c(25, 34, 29)
  ))

  table2 <- arrow_table(data.frame(
    STUDYID = c("STUDY001", "STUDY001", "STUDY001"),
    SUBJID = c("001", "002", "003"),
    PARAM = c("HEIGHT", "WEIGHT", "BMI"),
    AVAL = c(175.2, 68.5, 22.3)
  ))

  switch(index,
    `1` = {
      DTADataSetTabular(
        name = "example_container_specs_without_data",
        specs = create_example_DTAColumnSpecCollection(1),
      )
    },
    `2` = {
      DTADataSetTabular(
        name = "demographics",
        specs = create_example_DTAColumnSpecCollection(1),
        tables = list("tab1" = table1)
      )
    },
    `3` = {
      DTADataSetTabular(
        name = "vitals",
        specs = create_example_DTAColumnSpecCollection(2),
        tables = list("tab2" = table2)
      )
    },
    cli::cli_abort("No example available for the provided index.")
  )
}

#' @title Print Method for DTADataSetTabular
#' @description Print a summary of a DTADataSetTabular object.
#' @param x A DTADataSetTabular object.
#' @importFrom cli cli_alert_info cli_alert cli_text
#' @importFrom stringr str_c str_glue
#' @examples
#' library(DTAtools)
#' print(create_example_DTADataSetTabular())
#' @name print
#' @export
method(print, DTADataSetTabular) <- function(x, ...) {
  cli::cli_div(theme = list(span.emph = list(color = "orange")))
  cli_text("<{.emph DTADataSetTabular}> : {.field {x@name}}")

  print_info(x)

  if (!is.null(x@specs)) {
    cli_alert_info("Specs:")
    cli_alert("columns ({length(x@specs@columns)}): {column_preview(x@specs)}")
    cli_alert("rules ({length(x@specs@rules)}): {rule_preview(x@specs)}")
  } else {
    cli_alert_info("Specs: none")
  }

  n_targets <- length(x@tables)

  if (n_targets > 0) {
    table_names <- names(x@tables)

    if (n_targets > 5) {
      shown_names <- c(table_names[1:4], "...", table_names[n_targets])
    } else {
      shown_names <- table_names
    }

    # The names are INTERPOLATED, never pasted into the markup: cli parses
    # `{...}` in the string it is handed, so a table called `a{b}` (from a
    # delivered `a{b}.csv`) took print() down with "Could not evaluate cli `{}`
    # expression". Braces inside an interpolated value are escaped by cli
    # itself. cli_vec() only restores the separators the paste produced -- cli
    # would otherwise write "a, b and c" where this has always written
    # "a, b, c".
    shown <- cli::cli_vec(
      shown_names,
      list("vec-sep" = ", ", "vec-last" = ", ")
    )
    cli_alert_info("Tables ({n_targets}): {.field {shown}}")
  } else {
    cli_alert_info("Tables: {.emph none}")
  }

  invisible(x)
}


#' @title Print Short Information for DTADataSetTabular
#' @description
#' Prints short information about a \code{DTADataSetTabular} object.
#'
#' @param x A \code{DTADataSetTabular} object whose information is to be printed.
#'
#' @details
#' This method displays the template source, version, and date if available. It also summarizes the file information entries, indicating if none are present.
#'
#' @importFrom cli cli_alert_info cli_alert
#' @importFrom stringr str_c str_glue
#' @return No return value. This function is called for its side effects
#'   (printing to the console).
#'
#' @seealso
#' \code{\link{DTADataSetTabular}}
#'
#' @examples
#' library(DTAtools)
#' ds <- create_example_DTADataSetTabular()
#' print_short_info(ds)
#' @name print_short_info
#' @export
method(print_short_info, DTADataSetTabular) <- function(x, ...) {
  # super(print_short_info, x)
  method(print_short_info, DTADataSet)(x)
  if (!is.null(x@specs)) {
    cli_alert(
      "Specs: {length(x@specs@columns)} columns, {length(x@specs@rules)}, rules"
    )
  } else {
    cli_alert("Specs: none")
  }

  n_targets <- length(x@tables)

  if (n_targets > 0) {
    cli_alert("Tables: ({n_targets})")
  } else {
    cli_alert("Tables: {.emph none}")
  }

  return(invisible(x))
}


#' @title loads file
#' @description
#' Load the content of the file into dataset
#' @param x An object of class DTADataSet
#' @param ... Additional named arguments:
#'   \describe{
#'     \item{file}{file to be loaded}
#'     \item{handler_index}{Single character or numeric index selecting the file
#'       handler within the dataset. Defaults to \code{1}.}
#'     \item{name}{Name the table is stored under. Defaults to the file's base
#'       name with any compression suffix and then the extension removed, so
#'       \code{x.csv} and \code{x.csv.gz} name the same table and a compressed
#'       redelivery replaces the earlier one instead of adding a second.
#'       Loading under a name that is already bound drops that table's
#'       validation state, so no report shows the replaced table's verdict.}
#'     \item{stream}{whether to keep the file lazy rather than reading it into
#'       memory. See \code{\link{load_file}}.}
#'   }
#' @return object of class DTADataSet with loaded data
#' @examples
#' file_handler <- DTAFileCSV(filename = "clinical_data.csv")
#' ds <- DTADataSetTabular(
#'   name = "demo",
#'   specs = create_example_DTAColumnSpecCollection(1),
#'   files = list(file_handler)
#' )
#' file <- system.file("extdata", "clinical_data.csv", package = "DTAtools")
#' ds <- load_file(ds, file = file, handler_index = 1)
#' names(tables(ds))
#'
#' # The same file kept lazy: nothing is read until check() scans it.
#' ds_lazy <- load_file(
#'   ds,
#'   file = file, handler_index = 1, stream = "always"
#' )
#' names(tables(ds_lazy))
#' @usage load_file(x, ...)
#' @rdname load_file
#' @export
method(load_file, DTADataSetTabular) <- function(
  x,
  file,
  # Defaulted to 1, as the documentation has always promised and as the file
  # dataset's method already does. Without the default a caller who omitted it
  # hit R's own "argument is missing" from inside the guard below.
  handler_index = 1,
  # The compression suffix is stripped BEFORE the extension, so `x.csv` and
  # `x.csv.gz` name the same table. Stripping only the last extension made a
  # gzipped redelivery land under `x.csv` -- a second table beside `x`, with the
  # first one's data and verdict left standing next to it.
  name = tools::file_path_sans_ext(dta_strip_compression_extension(basename(file))),
  stream = getOption("DTAtools.stream", "auto")
) {
  # Shared with the file dataset's method rather than restated. The guard this
  # replaces was `handler_index < 1 || handler_index > length(x@files)`, run on
  # a value that could be NULL, NA, length > 1 or character -- so `"1"` reached
  # files() as a NAME and aborted with "The following datasets not found: 1",
  # even though a character index is documented as accepted.
  handler_index <- dta_resolve_file_handler_index(handler_index, x@files)

  handler <- files(x, handler_index)

  # Validated here, before `name` (whose default is derived from `file`) is
  # first forced by the replacement guard below: a non-scalar `file` would
  # otherwise reach that `if` as a length > 1 condition and die with R's own
  # message instead of this package's.
  file <- dta_assert_single_file_path(file, "load_file")
  if (!is.character(name) || length(name) != 1 || is.na(name) || !nzchar(name)) {
    cli::cli_abort(
      "{.fn load_file} requires {.arg name} to be a single non-missing, non-empty string."
    )
  }

  # Whatever is bound under this name now is about to be replaced, so any
  # verdict recorded against it describes data that is no longer here. Left
  # behind, validation_status() keeps reporting the replaced table's result
  # under the new table's name until the next check() happens to run -- the file
  # dataset's method already clears its counterpart on replacement, and this
  # mirrors it. Import issues are re-derived below on both branches.
  if (name %in% names(x@tables)) {
    x@validation_index[[name]] <- NULL
    x@validation_store[[name]] <- NULL
  }

  # This is where a dataset's specs and its file handler meet, so it is the only
  # place that can tell the reader what the columns are: `read_file()` dispatches
  # on the handler alone and a bare `DTAFile` has no specification to consult.
  #
  # The specs are needed in *both* halves of the read, for damage that happens at
  # different times:
  #
  # * At parse time, because the reader infers a column's type from its contents
  #   before any code here sees the data. A column of quoted subject ids reads as
  #   an integer and "007" arrives as 7, with the leading zeros already gone --
  #   which no later guard can undo. Passing the specs pins the columns declared
  #   as text to text.
  # * At coercion time, because inference also runs the other way: one
  #   unparseable cell turns a whole declared-numeric column into text. Applying
  #   the declared type makes the column a number, that one cell NA, and that one
  #   cell an import error.
  if (dta_resolve_stream_mode(stream, file)) {
    # Lazy: the table is a scan plan, not data. The second half of the read --
    # coercion, and the import issues it finds -- cannot happen here, because
    # there are no rows yet. It happens per batch inside check(), which is the
    # only thing that ever pulls rows through. So import_issues stays empty
    # until check() has run; the issues are the same ones, found later.
    x@tables[[name]] <- open_file(handler, file, specs = x@specs)
    x@import_issues[[name]] <- NULL

    return(x)
  }

  coerced <- dta_coerce_table_to_specs(
    handler |> read_file(file, specs = x@specs),
    x@specs
  )

  x@tables[[name]] <- coerced$table

  # Canonical copy on the dataset, keyed by table name. The same frame also
  # rides on the table itself, so a change in the issues changes the table hash
  # and check() cannot skip revalidation with a stale result.
  if (nrow(coerced$issues) > 0) {
    x@import_issues[[name]] <- coerced$issues
  } else {
    x@import_issues[[name]] <- NULL
  }

  x
}

# The files a lazily held table plans to scan that are no longer there.
#
# Only an Arrow `Dataset` is inspected. That is what `load_file(stream =
# "always")` produces, it is the only holding whose backing files are knowable
# without consuming it (the file list is read from the plan, not from disk), and
# it is the one `dta_table_change_signal()` already fingerprints the same way. A
# materialised Table has no files; a reader or a query is left to the scanner,
# which is where any other failure has always surfaced.
#
# `dta_dataset_source_files()`, not `$files`, for the same reason the change
# signal uses it: a dataset over a non-UTF-8 delivery scans a transcoded COPY,
# and the copy is neither what the user delivered nor what they can act on. Read
# from `$files` this reported an absent delivery by a `tempdir()` path the user
# never chose, and reported an absent COPY -- which is the session's own
# housekeeping, not a delivery failure -- as though the data had gone. A missing
# copy is instead re-made by `dta_refresh_transcoded_dataset()`, which runs
# immediately before this in `check()`.
#
# Returns `character(0)` -- "nothing missing, carry on" -- for every other
# holding and whenever the file list cannot be read at all. An unfamiliar
# holding is a reason to fall back to scanning, not a reason to fail.
#' @keywords internal
dta_missing_table_files <- function(table) {
  if (!inherits(table, "Dataset")) {
    return(character(0))
  }

  files <- dta_dataset_source_files(table)
  if (length(files) == 0) {
    return(character(0))
  }

  files[!file.exists(files)]
}

# The status row of a table checked against zero column specs. There is nothing
# to check it against, so it is neither valid nor invalid.
#
# `ok = NA`, never FALSE, is what keeps this from reading as a data failure:
# n_valid counts `ok == TRUE` and n_invalid counts `ok == FALSE` (see
# dta_results_from_status() in R/validationReporting.R), both with
# na.rm = TRUE, so an NA row is excluded from both tallies rather than counted
# as either. That is the whole point -- a dataset with no column specification
# must report as INCOMPLETE, not as a clean "VALIDATION PASSED", which is
# exactly what it did before: zero specs meant zero errors on every axis.
#' @keywords internal
dta_unspecified_validation_row <- function(table_name, target_type = "table") {
  # Delegates to the shared row builder rather than restating the schema. There
  # were already three copies of this column set in the package; a fourth here
  # would mean an eleventh status column has to be added in four places, and
  # missing one makes rbind() inside validation_status() abort the first time a
  # dataset holds one specified and one unspecified table.
  #
  # `ok = NA` is passed explicitly because the builder's default is
  # isTRUE(entry$ok), which would flatten NA to FALSE -- see the comment there
  # for why NA rather than FALSE is the correct verdict for "unspecified".
  dta_validation_result_to_row(
    table_name = table_name,
    status = "unspecified",
    target_type = target_type,
    ok = NA,
    index_entry = list(
      ok = NA,
      validated_at = NA_character_,
      run_id = NA_character_,
      validation_run = NA_character_,
      n_columnspec_errors = NA_integer_,
      n_rule_errors = NA_integer_,
      n_import_errors = NA_integer_
    )
  )
}

#' @title Validation Status for DTADataSetTabular
#' @description Returns a compact status table for validated tables.
#' @param x A \'DTADataSetTabular\' object.
#' @param ... Additional arguments:
#'   \describe{
#'     \item{tables}{NULL (default), character table names, or numeric table indices.}
#'   }
#' @return A data.frame with validation status per table.
#' @usage validation_status(x, ...)
#' @name validation_status
#' @export
validation_status <- S7::new_generic("validation_status", "x")

#' @export
S7::method(validation_status, DTADataSetTabular) <- function(x, tables = NULL) {
  target_tables <- dta_table_id_to_names(x, tables)

  rows <- lapply(target_tables, function(table_name) {
    entry <- x@validation_index[[table_name]]
    if (is.null(entry)) {
      # Delegates to the shared row builder rather than a fourth inline copy of
      # this column set -- see dta_unspecified_validation_row(), which the call
      # below mirrors exactly (only the status string differs). `ok = NA` is
      # passed explicitly for the same reason it is there: the builder's
      # default is isTRUE(index_entry$ok), which would flatten this NA to
      # FALSE and misreport "never checked" as "failed".
      return(dta_validation_result_to_row(
        table_name = table_name,
        status = "not_validated",
        target_type = "table",
        ok = NA,
        index_entry = list(
          ok = NA,
          validated_at = NA_character_,
          run_id = NA_character_,
          validation_run = NA_character_,
          n_columnspec_errors = NA_integer_,
          n_rule_errors = NA_integer_,
          n_import_errors = NA_integer_
        )
      ))
    }

    # A table checked while `specs@columns` was empty is tagged in its index
    # entry rather than routed through dta_validation_result_to_row(): that
    # helper forces `ok = isTRUE(index_entry$ok)`, which would turn the
    # deliberate `NA` back into `FALSE` and misreport "incomplete" as
    # "failed".
    if (identical(entry$status, "unspecified")) {
      return(dta_unspecified_validation_row(table_name = table_name, target_type = "table"))
    }

    dta_validation_result_to_row(
      table_name = table_name,
      status = "validated",
      index_entry = entry
    )
  })

  # rbind() of an empty list is NULL, and check(DTA) calls nrow() on whatever
  # this returns -- so a dataset with no tables bound yet has to answer with an
  # empty FRAME carrying the real columns, not with nothing at all. The file
  # dataset's method has always done this; the tabular one used to abort long
  # before reaching here, taking the whole DTA's report down with it.
  if (length(rows) == 0) {
    return(dta_file_empty_status_row(character(0), target_type = "table"))
  }

  do.call(rbind, rows)
}


#' @title Retrieve Validation Errors for One Table
#' @description
#' Returns detailed validation output for one table, either from in-memory
#' store or from persisted artifact.
#' @param x A \'DTADataSetTabular\' object.
#' @param ... Additional arguments:
#'   \describe{
#'     \item{table}{Character or numeric table identifier.}
#'     \item{source}{Character. One of \'auto\', \'memory\', or \'artifact\'.}
#'   }
#' @return A list with detailed validation output, of class
#'   \code{dta_validation_details}. Use \code{as.data.frame()} on it to get one
#'   row per reported error.
#' @usage validation_errors(x, ...)
#' @name validation_errors
#' @export
validation_errors <- S7::new_generic("validation_errors", "x")

#' @export
S7::method(validation_errors, DTADataSetTabular) <- function(
  x,
  table,
  source = c("auto", "memory", "artifact")
) {
  source <- match.arg(source)
  table_name <- dta_table_id_to_names(x, table)
  table_name <- table_name[[1]]

  if (source %in% c("auto", "memory")) {
    in_memory <- x@validation_store[[table_name]]
    if (!is.null(in_memory)) {
      # Migrated on read as well: a store entry can have been restored from a
      # session that predates the import axis.
      return(dta_as_validation_details(dta_migrate_validation_details(in_memory)))
    }
  }

  entry <- x@validation_index[[table_name]]
  if (is.null(entry) || is.null(entry$artifact_path)) {
    cli::cli_abort(
      "No validation artifact available for table '{table_name}'. Run check() first with persist = TRUE."
    )
  }

  if (!file.exists(entry$artifact_path)) {
    cli::cli_abort(
      "Validation artifact for table '{table_name}' does not exist at '{entry$artifact_path}'."
    )
  }

  # Tagged and migrated on read, so memory and artifact results stay identical.
  dta_as_validation_details(
    dta_migrate_validation_details(readRDS(entry$artifact_path))
  )
}


#' @title Clear Validation State
#' @description Clears in-memory validation state for one or all tables.
#' @param x A \'DTADataSetTabular\' object.
#' @param ... Additional arguments:
#'   \describe{
#'     \item{tables}{NULL (default), character table names, or numeric table indices.}
#'     \item{remove_artifacts}{Logical. If \'TRUE\', delete artifact files for selected tables.}
#'   }
#' @return Invisibly returns \'x\'.
#' @usage clear_validation(x, ...)
#' @name clear_validation
#' @export
clear_validation <- S7::new_generic("clear_validation", "x")

#' @export
S7::method(clear_validation, DTADataSetTabular) <- function(
  x,
  tables = NULL,
  remove_artifacts = FALSE
) {
  target_tables <- dta_table_id_to_names(x, tables)

  for (table_name in target_tables) {
    entry <- x@validation_index[[table_name]]

    if (remove_artifacts && !is.null(entry) && !is.null(entry$artifact_path)) {
      if (file.exists(entry$artifact_path)) {
        unlink(entry$artifact_path)
      }
    }

    x@validation_index[[table_name]] <- NULL
    x@validation_store[[table_name]] <- NULL
    x@import_issues[[table_name]] <- NULL
  }

  invisible(x)
}


#' @title Invalidate Validation Due to Spec Changes
#' @description
#' Marks validation as outdated for specified tables when specs are changed.
#' This is a helper function called automatically when specs are updated.
#' @param x A \'DTADataSetTabular\' object.
#' @param tables NULL (default) to invalidate all tables, or character/numeric
#'   table identifiers.
#' @return Invisibly returns \'x\'.
#' @keywords internal
invalidate_by_spec_change <- function(x, tables = NULL) {
  target_tables <- dta_table_id_to_names(x, tables)

  for (table_name in target_tables) {
    entry <- x@validation_index[[table_name]]
    if (!is.null(entry)) {
      # Mark specs_hash as NULL to force re-validation on next check() call
      entry$specs_hash <- NULL
      x@validation_index[[table_name]] <- entry
    }
    # Import issues were derived under the old specs; drop them with the rest.
    x@import_issues[[table_name]] <- NULL
  }

  invisible(x)
}


#' @title Check DTADataSetTabular Tables
#' @description
#' Validates all tables or a specific table within a DTADataSetTabular object,
#' prints a validation summary to the console, and updates the object's
#' validation state.
#' @param x A \code{DTADataSetTabular} object.
#' @param ... Additional named arguments:
#'   \describe{
#'     \item{tables}{NULL (default), character table names, or numeric table
#'       indices. If NULL and `tab` is also NULL, checks all tables.}
#'     \item{tab}{Character table name or numeric table index (optional). If
#'       provided, checks only this single table. Cannot be used together
#'       with `tables`.}
#'     \item{force}{Logical. If TRUE, forces re-validation even if unchanged.
#'       Default is FALSE.}
#'     \item{persist}{Logical. If TRUE (default), persists validation
#'       artifacts to disk.}
#'     \item{artifact_dir}{Character or NULL. Optional output directory for
#'       persisted validation artifacts.}
#'     \item{quiet}{Logical. If TRUE, suppresses console output. Default is FALSE.}
#'     \item{batch_rows}{Integer. Rows per batch when scanning a table that was
#'       loaded with \code{stream = "always"}. Ignored for a table held in
#'       memory. Defaults to
#'       \code{getOption("DTAtools.stream_batch_rows", 131072L)}. On a delimited
#'       file a batch is one Arrow read block of about
#'       \code{getOption("DTAtools.stream_block_size")} bytes (1 MiB by
#'       default), and \code{batch_rows} only \emph{caps} a batch that is
#'       already larger. Peak memory during such a scan is therefore governed by
#'       the block size times Arrow's read-ahead, not by \code{batch_rows}.}
#'     \item{max_errors}{Integer, \code{Inf}, or NULL to hold everything in
#'       memory. Cap on the number of per-cell errors whose detail is held in
#'       RAM while scanning. Defaults to
#'       \code{getOption("DTAtools.max_errors", 10000L)}; the default is finite
#'       because retention is one row per bad cell, so an unbounded cap
#'       exhausts memory on a large dirty file exactly as holding the data
#'       would. The reported \emph{counts} and the verdict are exact either
#'       way -- the cap decides how much detail is kept, never what the answer
#'       is -- and rows past the cap spill to a session-temporary store that
#'       \code{\link{collect_full_errors}()} reassembles. It applies to a table
#'       held in memory as well, where it bounds retained detail only: there is
#'       no spill there, so the dropped rows are recovered only by checking
#'       again with a larger cap. A table constructed in R and checked straight
#'       away, whose complete detail is wanted, should therefore pass
#'       \code{max_errors = Inf} -- it is already in memory, so the cap buys
#'       nothing and costs the detail.}
#'     \item{fail_fast}{Logical, default FALSE. Stop at the first batch that
#'       shows any problem instead of scanning to the end. On a table large
#'       enough to take hours, this answers \emph{is this valid?} without paying
#'       for a full pass -- the difference between seconds and hours when the
#'       data fails early. The report is then explicitly incomplete: it carries a
#'       \code{partial_scan} attribute, only rules that actually failed are
#'       listed, and axes that could not be settled report NA rather than TRUE,
#'       because a rule that has not failed yet has not passed. Ignored for a
#'       table held in memory.}
#'     \item{on_missing_column}{One of \code{"scan"} (default) or
#'       \code{"stop"}. A column the specs require but the table lacks is
#'       decidable from the column names alone. \code{"scan"} preserves existing
#'       behaviour and reports the absence once per row -- which on a 60 GB table
#'       means reading all of it to restate one fact hundreds of millions of
#'       times. \code{"stop"} reports it structurally and reads nothing; the
#'       result carries a \code{structural_only} attribute so no reader mistakes
#'       it for a verdict on the rows. Falls back to scanning when the columns
#'       cannot be determined without consuming the table.
#'
#'       Unlike \code{fail_fast} and \code{use_threads}, this is \emph{not}
#'       ignored for a table held in memory: the column names are known there
#'       too, so the gate applies to every holding.}
#'     \item{use_threads}{Logical, default TRUE. Whether Arrow's Scanner uses
#'       multiple threads for I/O and decompression while scanning. Arrow
#'       buffers batches ahead of R in its own C++ pool, outside the R heap and
#'       invisible to \code{gc()}, so FALSE is the lever to reach for when
#'       resident memory rather than speed is what binds. Ignored for a table
#'       held in memory.}
#'   }
#' @return Invisibly returns the updated \code{DTADataSetTabular} object `x`,
#'   with \code{validation_index}/\code{validation_store} updated and a
#'   \code{"last_validation_summary"} attribute set. Use
#'   \code{validation_status()} on the returned object to obtain the
#'   validation summary data.frame.
#' @importFrom cli cli_h3 cli_alert_info cli_alert_success cli_alert_danger
#' @examples
#' ds <- create_example_DTADataSetTabular(2)
#' # Check all tables
#' ds <- check(ds)
#' # Check specific table
#' ds <- check(ds, tables = "tab1")
#' @usage check(x, ...)
#' @name check
#' @export
S7::method(check, DTADataSetTabular) <- function(
  x,
  tables = NULL,
  tab = NULL,
  force = FALSE,
  persist = TRUE,
  artifact_dir = NULL,
  quiet = FALSE,
  validation_run = NULL,
  batch_rows = getOption("DTAtools.stream_batch_rows", 131072L),
  max_errors = getOption("DTAtools.max_errors", 10000L),
  fail_fast = FALSE,
  on_missing_column = c("scan", "stop"),
  use_threads = TRUE
) {
  # Matched here rather than only downstream so a typo is reported once, before
  # any table is touched, instead of after the first one has been scanned.
  on_missing_column <- match.arg(on_missing_column, c("scan", "stop"))

  # Handle single table vs multiple tables
  if (!is.null(tab) && !is.null(tables)) {
    cli::cli_abort("Cannot specify both 'tab' and 'tables' parameters. Use 'tab' for single table, 'tables' for multiple.")
  }

  # If single table is specified, use it; otherwise use tables parameter
  if (!is.null(tab)) {
    target_table_indices <- dta_table_id_to_names(x, tab)
    tables_to_check <- target_table_indices
    single_table_mode <- TRUE
  } else {
    tables_to_check <- tables
    single_table_mode <- FALSE
  }

  # Validate structure (from parent class)
  x <- S7::method(check, DTADataSet)(
    x,
    force = force,
    persist = persist,
    artifact_dir = artifact_dir,
    quiet = quiet,
    validation_run = validation_run
  )

  if (is.null(validation_run)) {
    validation_run <- dta_new_validation_run_id()
  }

  # Table-specific validation logic (override parent)
  target_tables <- dta_table_id_to_names(x, tables_to_check)
  specs_hash <- dta_hash_object(as.list(x@specs))
  output_rows <- list()

  # A dataset with no table bound is the ordinary state of a specification
  # whose data has not been delivered yet. It has to REPORT that -- and let
  # check(DTA) count it as work that did not happen -- rather than abort, which
  # is what it did before: one undelivered dataset took down the check(),
  # results() and messages() of every other dataset in the same DTA. The
  # summary attribute is a zero-row FRAME, never NULL, so nrow()/rbind() on it
  # keep working.
  if (length(target_tables) == 0) {
    if (!isTRUE(quiet)) {
      cli::cli_alert_warning(
        "Dataset {.field {x@name}} has no tables loaded; nothing was checked."
      )
    }

    attr(x, "last_validation_summary") <- dta_file_empty_status_row(
      character(0),
      target_type = "table"
    )

    return(invisible(x))
  }

  if (persist) {
    if (is.null(artifact_dir)) {
      artifact_dir <- if (!is.null(x@validation_artifact_dir)) {
        x@validation_artifact_dir
      } else {
        dta_default_validation_artifact_dir(x)
      }
    }
    dir.create(artifact_dir, recursive = TRUE, showWarnings = FALSE)
    x@validation_artifact_dir <- artifact_dir
  }

  for (idx in seq_along(target_tables)) {
    table_name <- target_tables[idx]
    current_table <- x@tables[[table_name]]

    # A dataset whose specs declare zero columns has nothing to check a table
    # against -- `dta_validate_any_table()` would run and report a hollow
    # pass, the exact "VALIDATION PASSED certificate covering ZERO checks"
    # this branch exists to prevent. `ok = NA`, not `FALSE`: this is not a
    # data failure, it is the absence of a specification, and `n_valid`/
    # `n_invalid` (both computed with `na.rm = TRUE`) must skip this row
    # rather than count it as either a pass or a fail. The dataset instead
    # reports as incomplete. Checked ahead of the `force`/`unchanged` skip
    # logic below because there is no validation run to skip -- there was
    # never one to begin with.
    if (length(x@specs@columns) == 0) {
      if (!isTRUE(quiet)) {
        cli::cli_alert_warning(
          "Dataset {.field {x@name}} declares no columns in its specs; table {.field {table_name}} was not checked."
        )
      }

      x@validation_index[[table_name]] <- list(status = "unspecified")

      output_rows[[length(output_rows) + 1]] <- dta_unspecified_validation_row(
        table_name = table_name,
        target_type = "table"
      )
      next
    }

    # Deliberately NOT as.data.frame() here. A lazy table is lazy precisely
    # because materialising it is not affordable, and hashing it to decide
    # whether to skip it would spend more than validating it costs.
    table_hash <- dta_table_change_signal(current_table)

    previous <- x@validation_index[[table_name]]

    # A RecordBatchReader is consumed by its first scan. Re-validating one
    # yields zero batches, and a zero-batch stream is indistinguishable from
    # clean data -- so the second check() would silently overwrite a real
    # verdict with a hollow ok = TRUE. Refusing is the only honest answer.
    if (inherits(current_table, "RecordBatchReader") && !is.null(previous)) {
      cli::cli_abort(c(
        "Table {.field {table_name}} is held as a {.cls RecordBatchReader}, which its previous validation consumed.",
        i = "Re-load the table (or hold it as an Arrow Table or Dataset) to check it again; a reader can be scanned exactly once."
      ))
    }

    # A NULL signal means identity could not be established, so the table is
    # assumed changed. Without the explicit NULL guard two unidentifiable
    # tables would compare equal and the second would be skipped.
    #
    # `!isTRUE(previous$partial)` closes a second gap: a run that stopped early
    # (fail_fast) or never read a row (on_missing_column = "stop") records real
    # counts, but they are the counts of a PARTIAL scan, not the whole table.
    # Without this, a later plain check() would see an unchanged table hash and
    # skip outright -- reporting status "skipped" with those partial counts
    # presented as if they were the final totals. A partial result therefore
    # never satisfies the skip, so the next check() always rescans in full.
    unchanged <- !is.null(previous) &&
      !is.null(table_hash) &&
      identical(previous$table_hash, table_hash) &&
      identical(previous$specs_hash, specs_hash) &&
      !isTRUE(previous$partial)

    if (!force && unchanged) {
      previous$validation_run <- validation_run
      x@validation_index[[table_name]] <- previous

      output_rows[[length(output_rows) + 1]] <- dta_validation_result_to_row(
        table_name = table_name,
        status = "skipped",
        index_entry = previous,
        target_type = "table"
      )
      next
    }

    # A lazy table over a non-UTF-8 delivery is a scan plan over a UTF-8 COPY
    # of it, and a copy does not follow what it was copied from. The change
    # signal above is keyed on the delivery (see `dta_dataset_source_files()`),
    # so an edited file opens the skip gate exactly as it should -- and then the
    # scan below read the STALE copy and reported the old data's verdict as a
    # fresh one, clean row count and all. Re-opening here makes the plan follow
    # the delivery again.
    #
    # After the skip gate rather than before it, so that an unchanged table --
    # the common case, and the one the gate exists for -- never pays even the
    # fingerprint check. An unchanged delivery that does reach this returns the
    # same object, so the cost on the rescan path is two `file.info()` fields.
    #
    # The signal is taken again only when the object really was replaced: it
    # carries the schema, and a re-delivery is free to have a different one.
    # Recording the stale schema's signal would leave a hash that never recurs,
    # and the table would be rescanned on every check() thereafter.
    refreshed <- dta_refresh_transcoded_dataset(current_table)
    if (!identical(refreshed, current_table)) {
      current_table <- refreshed
      x@tables[[table_name]] <- refreshed
      table_hash <- dta_table_change_signal(current_table)
    }

    # A lazy table is a scan plan over files, not data, and the plan holds only
    # while those files are there. Scanning one whose file has since been
    # deleted or moved raises an Arrow IOError from deep inside the scanner,
    # which took the whole check() down -- so one cleaned-up delivery destroyed
    # the verdicts of every other table in the dataset. The absence is reported
    # as THIS table's failure instead, in the same file-presence shape a file
    # dataset uses for an undelivered target. The skip gate above has already
    # run, and the change signal of a Dataset whose files are gone is stable
    # (NA size, NA mtime), so a repeated check() skips exactly as it would for
    # an undelivered file target rather than re-reporting the same absence.
    absent_files <- dta_missing_table_files(current_table)
    if (length(absent_files) > 0) {
      absence <- list(
        ok = FALSE,
        message = sprintf(
          "Table '%s' cannot be read: file '%s' not found.",
          table_name, absent_files[[1]]
        )
      )
      details <- dta_file_validation_details(absence)

      if (!isTRUE(quiet)) {
        # Interpolated: both the table name and the path are arbitrary text and
        # a `{` in either would be parsed as a cli expression.
        absence_message <- absence$message
        cli::cli_alert_danger("{absence_message}")
      }

      validated_at <- Sys.time()
      index_entry <- list(
        validated_at = validated_at,
        ok = FALSE,
        table_hash = table_hash,
        specs_hash = specs_hash,
        n_columnspec_errors = details$n_columnspec_errors,
        n_rule_errors = details$n_rule_errors,
        n_import_errors = details$n_import_errors,
        run_id = format(validated_at, "%Y%m%dT%H%M%OS3"),
        validation_run = validation_run,
        # Nothing was read, so there is nothing to persist -- exactly as the
        # file dataset does for a target that never arrived.
        artifact_path = NULL,
        partial = FALSE
      )

      x@validation_index[[table_name]] <- index_entry
      x@validation_store[[table_name]] <- details
      # Derived from a table that could not be read, so whatever was recorded
      # at load time no longer describes anything.
      x@import_issues[[table_name]] <- NULL

      if (single_table_mode) {
        attr(x, "last_validation_details") <- dta_as_validation_details(details)
      }

      output_rows[[length(output_rows) + 1]] <- dta_validation_result_to_row(
        table_name = table_name,
        status = "validated",
        index_entry = index_entry,
        target_type = "table"
      )
      next
    }

    # Output table name/index under investigation
    if (!isTRUE(quiet)) {
      cli::cli_text()
      # The table name is INTERPOLATED, never pasted into the format string:
      # cli parses `{...}` in what it is handed, so a table called `a{b}` (from
      # a delivered `a{b}.csv`) aborted the entire run with "Could not evaluate
      # cli `{}` expression". Braces inside an interpolated value are escaped
      # by cli itself.
      if (single_table_mode) {
        cli::cli_rule("Validating table: {table_name}")
      } else {
        n_target_tables <- length(target_tables)
        cli::cli_rule("Table {idx} of {n_target_tables}: {table_name}")
      }
    }

    details <- dta_validate_any_table(
      x@specs,
      current_table,
      verbose = !isTRUE(quiet),
      batch_rows = batch_rows,
      max_errors = max_errors,
      use_threads = use_threads,
      fail_fast = fail_fast,
      on_missing_column = on_missing_column
    )

    # A materialised table has its import issues recorded already, at import
    # time (load_file() / the constructor). A lazy table does not -- its
    # load_file() branch documents that they are "found later" by check(), but
    # until this is written that promise was never kept and @import_issues
    # stayed empty forever, which is what left the Shiny app (which reads
    # ds@import_issues) showing nothing for a streamed table with bad cells.
    # `details$import_errors` is in the same row/column/raw/declared_type/
    # reason shape dta_coerce_table_to_specs() produces for the eager path, so
    # it drops in as the same kind of value.
    #
    # `import_typing_errors` is preferred over `import_errors` where the
    # streaming result carries it: that field is the import-TYPING axis alone,
    # which is exactly what the eager path records at load time, whereas
    # `import_errors` merges typing with everything else the scan folded into
    # the import axis. Taking the merged frame put a different KIND of value in
    # @import_issues depending on how the table happened to be held. The
    # fallback keeps working against a result (or a restored store entry) that
    # predates the split.
    #
    # The field's PRESENCE decides, not its contents. Testing the value for
    # NULL could not tell an empty typing axis from a result written before the
    # field existed, and fell back to the merged frame for both -- so a streamed
    # table whose only import errors were rule-time ones recorded those rows
    # here while the same file loaded eagerly recorded none. The streaming path
    # now always carries the field, so its absence means exactly one thing.
    if (dta_table_is_lazy(current_table)) {
      import_issues <- if ("import_typing_errors" %in% names(details)) {
        details$import_typing_errors
      } else {
        details$import_errors
      }

      x@import_issues[[table_name]] <- if (
        is.data.frame(import_issues) && nrow(import_issues) > 0
      ) {
        import_issues
      } else {
        NULL
      }
    }

    artifact_path <- NULL
    validated_at <- Sys.time()
    run_id <- format(validated_at, "%Y%m%dT%H%M%OS3")

    if (persist) {
      safe_table <- gsub("[^A-Za-z0-9_-]", "_", table_name)
      table_dir <- file.path(artifact_dir, safe_table)
      dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
      artifact_path <- file.path(table_dir, paste0(run_id, ".rds"))
      saveRDS(details, artifact_path)
    }

    index_entry <- list(
      validated_at = validated_at,
      ok = isTRUE(details$ok),
      table_hash = table_hash,
      specs_hash = specs_hash,
      n_columnspec_errors = details$n_columnspec_errors,
      n_rule_errors = details$n_rule_errors,
      n_import_errors = details$n_import_errors,
      run_id = run_id,
      validation_run = validation_run,
      artifact_path = artifact_path,
      # Whether this result came from a fail_fast run that stopped at the first
      # problem, or an on_missing_column = "stop" run that read no rows at all.
      # Read by the skip gate above: a partial result must never be mistaken
      # for a complete one on the next check().
      partial = isTRUE(attr(details, "partial_scan")) || isTRUE(attr(details, "structural_only"))
    )

    x@validation_index[[table_name]] <- index_entry
    x@validation_store[[table_name]] <- details

    # Attach details for single table mode.
    #
    # Tagged on the way out. `dta_validate_any_table()` returns a tagged result
    # for a lazy table and an untagged one for a materialised table, so without
    # this the attribute's class would depend on how the table happened to be
    # held -- and `as.data.frame()` would work on one and fail on the other.
    if (single_table_mode) {
      attr(x, "last_validation_details") <- dta_as_validation_details(details)
    }

    output_rows[[length(output_rows) + 1]] <- dta_validation_result_to_row(
      table_name = table_name,
      status = "validated",
      index_entry = index_entry,
      target_type = "table"
    )
  }

  summary_df <- do.call(rbind, output_rows)
  attr(x, "last_validation_summary") <- summary_df

  # Summary output
  val_status <- validation_status(x, tables = tables_to_check)

  if (!isTRUE(quiet)) {
    if (nrow(val_status) > 0) {
      n_valid <- sum(val_status$ok == TRUE, na.rm = TRUE)
      n_invalid <- sum(val_status$ok == FALSE, na.rm = TRUE)
      n_total <- nrow(val_status)

      if (!single_table_mode) {
        cli::cli_text()
      }

      table_word <- if (n_total == 1) "table" else "tables"

      if (n_invalid > 0) {
        cli::cli_alert_danger("{n_valid} of {n_total} {table_word} valid")
      } else if (n_valid < n_total) {
        # No table failed, but not every table was actually checked either --
        # e.g. a table validated against zero column specs, status
        # "unspecified", `ok = NA`. `n_valid == n_total` is required for a
        # clean pass; falling into the success branch here is precisely the
        # "VALIDATION PASSED certificate covering ZERO checks" this status
        # exists to prevent.
        n_unchecked <- n_total - n_valid
        cli::cli_alert_warning(
          "{n_valid} of {n_total} {table_word} valid; {n_unchecked} not checked"
        )
      } else {
        cli::cli_alert_success("{n_total} {table_word} passed validation")
      }
    }
  }

  # Return the updated dataset so validation state is not lost
  invisible(x)
}
