#' @title DTAFileTabular Class Constructor
#'
#' @description
#' Defines the S7 class \code{C}, which extends \code{DTAFile}
#' to represent metadata and configuration for Delim (Tab-Separated Values)
#'  data files.
#'
#' @param filename Character vector of file names or regular expression patterns
#'   to match files.
#' @param pattern Logical; if \code{TRUE}, \code{filename} is treated as a regex
#'   pattern. Default is \code{FALSE}.
#' @param pattern_description Character or \code{NULL}; human-readable
#'   description of the \code{filename} pattern.
#' @param number_of_files Numeric or \code{NULL}; maximum number of files
#'   expected. Default is \code{1}.
#' @param min_number_of_files Numeric or \code{NULL}; minimum number of files
#'   expected.
#' @param max_number_of_files Numeric or \code{NULL}; maximum number of files
#'   expected.
#' @param info Character or \code{NULL}; free-text description of the file.
#' @param missing_values Character. String representing missing values in the
#'   file. Default is \code{""}.
#' @param sep Character. Field separator used in the TSV file.
#'  Defaults to tab ("\\t").
#' @param has_header Logical; \code{TRUE} if the first row is a header. Default
#'   is \code{TRUE}.
#' @param quote Character or \code{NULL}; quoting character for fields. Default
#'   is \code{'"'}.
#' @param newlines_in_values Logical; \code{TRUE} if a quoted field may contain
#'   a line break. Default is \code{FALSE}, which is Arrow's own default and
#'   what every file written by a well-behaved exporter satisfies.
#'
#'   The setting exists because a quoted line break is only a problem when it
#'   straddles one of the fixed-size blocks the file is read in (see
#'   \code{DTAtools.stream_block_size}): a small file with quoted newlines reads
#'   correctly either way, and the same data past the first block fails with
#'   "CSV parse error: straddling object straddles two block boundaries". Turning
#'   this on makes the reader hold a block open until the quoted value ends, at
#'   some cost in parse speed -- which is why it is not the default.
#' @param encoding Character; the character encoding of the file, as accepted by
#'   \code{\link[base]{iconv}}. Default is \code{"UTF-8"}.
#'
#'   Both readers honour it, by different means. The in-memory reader passes it
#'   to Arrow, which re-encodes by wrapping the input stream it owns. The
#'   dataset scanner has no such step -- it opens its own files -- so a
#'   non-UTF-8 file is instead converted once, streaming and in bounded memory,
#'   to a UTF-8 copy under \code{\link[base]{tempdir}()} that the scan reads;
#'   the copy is cached for the session, so repeated loads of an unchanged file
#'   convert once, and a re-delivery replaces it rather than adding to it. Both
#'   paths therefore produce the same verdict for the same file, which is the
#'   point -- including for a quoted value containing a line break, which is
#'   copied byte for byte rather than normalised.
#'
#'   The cost is disk and one pass, not memory. Conversion runs at an order of
#'   20 MB/s -- 21 MB/s measured here on a 172 MiB, 1,000,000 x 20 latin1 CSV,
#'   and 19 MB/s of decoded output when the source is gzip-compressed -- and the
#'   copy is the size of the decoded text: roughly the size of the source for latin1
#'   that is mostly ASCII, up to twice it for text that is wholly accented, and
#'   the full decoded size for a \code{.gz} source. A very large non-UTF-8
#'   delivery needs that much free space under \code{tempdir()}; declaring
#'   \code{"UTF-8"} for a file that really is UTF-8 costs nothing at all. The
#'   block the conversion works in is
#'   \code{options(DTAtools.transcode_block_bytes = )}, 4 MiB by default; it
#'   bounds the buffer and has no effect on the copy.
#'
#'   The wide encodings (UTF-16, UTF-32, UCS-2, UCS-4) are the exception: their
#'   characters can contain a newline byte, so the file cannot be converted in
#'   blocks cut at newlines and \code{\link{open_file}()} refuses it, naming
#'   \code{stream = "never"} as the way to read it. A name this platform's
#'   \code{\link[base]{iconv}()} does not know (\code{"latin-1"} for
#'   \code{"latin1"}, say) is refused too, before the file is opened.
#'
#' @name DTAFileTabular-class
#' @return An object of class \code{DTAFileTabular}.
#'
#' @seealso \code{\link{DTAFile}}
#'
#' @export
DTAFileTabular <- S7::new_class(
  "DTAFileTabular",
  parent = DTAFile,
  constructor = function(
    filename,
    pattern = FALSE,
    pattern_description = NULL,
    number_of_files = NULL,
    min_number_of_files = NULL,
    max_number_of_files = NULL,
    info = NULL,
    missing_values = "",
    sep = "\t",
    has_header = TRUE,
    quote = '"',
    newlines_in_values = FALSE,
    encoding = "UTF-8"
  ) {
    new_object(
      .parent = DTAFile(
        filename = filename,
        number_of_files = number_of_files,
        min_number_of_files = min_number_of_files,
        max_number_of_files = max_number_of_files,
        info = info,
        pattern = pattern,
        pattern_description = pattern_description
      ),
      sep = sep,
      has_header = has_header,
      quote = quote,
      missing_values = missing_values,
      # Both defaults are chosen so that every handler built before these
      # properties existed -- and every YAML that does not mention them --
      # keeps reading its file exactly as it did.
      newlines_in_values = newlines_in_values,
      encoding = encoding
    )
  },
  properties = list(
    sep = class_character,
    has_header = class_logical,
    quote = class_character,
    missing_values = class_character,
    newlines_in_values = class_logical,
    encoding = class_character
  ),
  validator = function(self) {
    # Collect every violation: returning the messages from separate `if`
    # blocks would silently discard all but the last one.
    problems <- character()

    # `nchar(NA_character_)` is `NA`, not an error -- but `NA != 1` is also
    # `NA`, and handing that to the `||` chain's `if` aborts with the opaque
    # "missing value where TRUE/FALSE needed" instead of this check's own
    # message. `is.na()` is checked first (after the length-1 guard makes it
    # safe to call), so `nchar()` never runs on a value it cannot answer for.
    if (
      !is.character(self@sep) ||
        length(self@sep) != 1 ||
        is.na(self@sep) ||
        nchar(self@sep) != 1
    ) {
      problems <- c(problems, "'sep' must be a single character.")
    }

    if (!is.logical(self@has_header) || length(self@has_header) != 1) {
      problems <- c(problems, "'has_header' must be a single logical value.")
    }

    if (
      !is.character(self@quote) ||
        length(self@quote) != 1 ||
        is.na(self@quote) ||
        nchar(self@quote) != 1
    ) {
      problems <- c(problems, "'quote' must be a single character.")
    }

    # Same shape as the `has_header` check, plus the NA guard: this value ends
    # up in `arrow::csv_parse_options()`, which rejects NA with a message about
    # its own argument rather than about the handler.
    if (
      !is.logical(self@newlines_in_values) ||
        length(self@newlines_in_values) != 1 ||
        is.na(self@newlines_in_values)
    ) {
      problems <- c(
        problems,
        "'newlines_in_values' must be a single non-missing logical value."
      )
    }

    if (
      !is.character(self@encoding) ||
        length(self@encoding) != 1 ||
        is.na(self@encoding) ||
        !nzchar(self@encoding)
    ) {
      problems <- c(problems, "'encoding' must be a single non-empty string.")
    }

    if (length(problems) == 0) {
      NULL
    } else {
      problems
    }
  }
)


#' @title Read File for DTAFileTabular Objects
#' @name read_file_execution
#' @description
#' \code{DTAFileTabular} is a virtual class. This method needs to be
#' implemented in derived classes like \code{DTAFileTSV},
#' \code{DTAFileCSV} or \code{DTAFileDelim}.
#' @importFrom cli cli_abort
#' @param x A \code{DTAFileTabular} object containing file reading parameters.
#' @param ... A single `file` argument: character string specifying the path
#'   to the file to be read.
#' @return A tibble containing the contents of the file if the filename
#' matches; otherwise, returns \code{NULL}.
#' @usage read_file_execution(x, ...)
method(read_file_execution, DTAFileTabular) <- function(x, ...) {
  cli::cli_abort(
    "This method is not implemented. You need to
  use an object of a class which is derived from DTAFileTabular class."
  )
}

#' @title Open File Lazily for DTAFileTabular Objects
#' @name open_file_execution
#' @description
#' \code{DTAFileTabular} is a virtual class. This method needs to be
#' implemented in derived classes like \code{DTAFileTSV},
#' \code{DTAFileCSV} or \code{DTAFileDelim}, each of which knows its own
#' delimiter.
#' @importFrom cli cli_abort
#' @param x A \code{DTAFileTabular} object containing file reading parameters.
#' @param ... A single `file` argument: character string specifying the path
#'   to the file to be opened.
#' @return An \code{arrow::Dataset}.
#' @usage open_file_execution(x, ...)
method(open_file_execution, DTAFileTabular) <- function(x, ...) {
  cli::cli_abort(
    "This method is not implemented. You need to
  use an object of a class which is derived from DTAFileTabular class."
  )
}

#' @title Reader Arguments Passed Through `...`
#' @description
#' `read_file_execution()` and `open_file_execution()` dispatch on `x` alone and
#' take everything else through `...`, so each concrete method has to pull its
#' arguments back out. Doing that as `list(...)[[1]]` makes the file the *first*
#' argument by position, which silently picks up `specs` if a caller ever names
#' its arguments in the other order. Removing the named entries first leaves the
#' file as the only positional one, so all six methods agree on what they were
#' given however the call was written.
#' @param ... The arguments the calling method was called with.
#' @param .caller Name of the generic to blame in the error message. Both
#'   `read_file_execution()` and `open_file_execution()` are exported, so a
#'   caller can reach this failure through either -- and naming the wrong one
#'   sends them looking in the wrong place.
#' @return A list with `file` (the path) and `specs` (a
#'   `DTAColumnSpecCollection`, or `NULL` when the caller supplied none).
#' @keywords internal
dta_reader_args <- function(..., .caller = "read_file_execution") {
  args <- list(...)
  nms <- names(args)

  if (is.null(nms)) {
    nms <- rep("", length(args))
  }

  # Exact matching: `[[` with a character index partial-matches only when asked
  # to, unlike `$`, which would let a stray `spec = ` argument bind to `specs`.
  pick <- function(name) {
    index <- which(nms == name)
    if (length(index) == 0) NULL else args[[index[[1]]]]
  }

  specs <- pick("specs")
  file <- pick("file")

  if (is.null(file)) {
    positional <- args[!nzchar(nms)]

    if (length(positional) == 0) {
      # `{(.caller)}`, not `{.caller}`: cli >= 3.4 reads a `{}` expression
      # starting with a dot as a style name, not a variable. The parentheses
      # say this is the variable. The dot on the parameter itself stays, so it
      # cannot collide with a `...` argument of the same name.
      cli::cli_abort("{.fn {(.caller)}} requires a {.arg file} argument.")
    }

    file <- positional[[1]]
  }

  list(file = file, specs = specs)
}

#' @title Apply the Column-Name Cleaning Rule to a Table
#' @description
#' Renames an already-read table's columns with [dta_clean_column_names()].
#' Neither reader needs this any more -- both are told the cleaned names when
#' the file is opened, by [dta_delim_reader_plan()] -- but it remains the
#' shortest way to state the rule on an object that has already been read, and
#' the tests exercise it directly.
#' @param table_obj Any object with `names()` and `names<-`.
#' @return `table_obj` with cleaned names.
#' @keywords internal
dta_normalize_column_names <- function(table_obj) {
  current_names <- names(table_obj)
  cleaned_names <- dta_clean_column_names(current_names)

  if (!identical(current_names, cleaned_names)) {
    names(table_obj) <- cleaned_names
  }

  table_obj
}

#' @title The Column-Name Cleaning Rule
#' @description
#' Strips surrounding quotes and whitespace from header names. Applied once, by
#' [dta_delim_reader_plan()], to the names both readers are then given -- so the
#' identical file cannot present different column names depending on how it
#' happened to be loaded, and cannot match the specs on only one path.
#' @param x Character vector of raw column names.
#' @return The cleaned character vector.
#' @keywords internal
dta_clean_column_names <- function(x) {
  trimws(gsub('^\\s*"|"\\s*$', "", x))
}

#' @title Parse Settings Declared by a File Handler
#' @description
#' Pulls the two real-world parse settings off a `DTAFileTabular` handler, with
#' the defaults that apply when there is no handler at all -- which is the case
#' for the entry points that open a path directly
#' (`dta_open_validation_dataset()`, `cache_as_parquet()`).
#' @param handler A `DTAFileTabular` (or subclass), or `NULL`.
#' @return A list with `newlines_in_values` (single logical) and `encoding`
#'   (single string).
#' @keywords internal
dta_reader_parse_settings <- function(handler = NULL) {
  defaults <- list(newlines_in_values = FALSE, encoding = "UTF-8")

  if (is.null(handler)) {
    return(defaults)
  }

  # tryCatch, not `%||%`: a handler predating these properties -- one restored
  # from a serialised object, say -- has no such property at all, and reaching
  # for it is an error rather than a NULL.
  pick <- function(name, default) {
    value <- tryCatch(S7::prop(handler, name), error = function(e) NULL)
    if (length(value) != 1 || is.na(value)) default else value
  }

  list(
    newlines_in_values = isTRUE(pick("newlines_in_values", FALSE)),
    encoding = as.character(pick("encoding", "UTF-8"))
  )
}

#' @title Is This Encoding Already UTF-8?
#' @description
#' The single test every reader uses to decide whether an encoding needs doing
#' anything about, so that `"utf-8"`, `"UTF-8"` and `"Utf-8"` cannot be
#' answered three different ways in three places.
#' @param encoding A single encoding name.
#' @return `TRUE` when the file needs no conversion.
#' @keywords internal
dta_encoding_is_utf8 <- function(encoding) {
  identical(toupper(encoding), "UTF-8")
}

#' @title Is This Encoding One That Cannot Be Split on Newline Bytes?
#' @description
#' [dta_transcode_to_utf8()] converts a file in blocks cut at their last `0x0A`
#' byte. That is sound for every ASCII-compatible encoding -- UTF-8, the whole
#' latin/windows family, and any other single-byte code page -- because `0x0A`
#' there is always the line feed and never part of another character.
#'
#' It is *not* sound for the wide encodings: in UTF-16LE the two bytes of an
#' ordinary character can be `0x0A 0x00`, so splitting on `0x0A` cuts
#' characters in half. Such a file is refused rather than converted, because a
#' plausible-looking wrong answer about someone's data is worse than a clear
#' no.
#' @param encoding A single encoding name.
#' @return `TRUE` for the UTF-16/32 and UCS-2/4 families, in the spellings
#'   `iconv` accepts for them.
#' @keywords internal
dta_encoding_is_wide <- function(encoding) {
  grepl("^(utf|ucs)[-_]?(16|32|2|4)", tolower(encoding))
}

# ---- reading a file that is not UTF-8 ---------------------------------------

#' @title A UTF-8 Copy of a File Declared in Another Encoding
#' @description
#' Arrow re-encodes by wrapping the INPUT STREAM, which only the in-memory
#' reader owns: a dataset opens its own files, so `csv_read_options(encoding =)`
#' is accepted and then silently ignored on the lazy path. This converts the
#' bytes once instead, into a temporary UTF-8 copy the scanner can read --
#' replacing the refusal that used to make every non-UTF-8 delivery
#' unstreamable, however large it was.
#'
#' @section Bounded memory:
#' The file is read through a connection in blocks of
#' [dta_transcode_block_bytes()] bytes, converted with [base::iconv()] and
#' written straight back out, so what the peak cost follows is the block, not
#' the file: linear time, and memory that a 100 GB delivery does not change. A
#' `.gz` source is read through [base::gzfile()], so a compressed delivery is
#' never expanded on disk first.
#'
#' Measured rather than asserted, against a session baseline of 116 Mb: the
#' 172 MiB reference input peaked at 254 Mb with a 1 MiB block, 260 Mb at the
#' 4 MiB default and 488 Mb at 16 MiB -- the block is what moves it. Fifteen
#' times less input (11 MiB) at the default block peaked at 184 Mb, so what the
#' file adds is the string cache between collections rather than anything held:
#' a 15-fold larger source cost 1.4 times the increment, where holding it would
#' have cost 15. The copy is byte-identical whether the block holds 1 KiB,
#' 64 KiB or 4 MiB.
#'
#' @section What the copy is, exactly:
#' The same bytes, re-encoded, and nothing else: the header included and
#' unaltered, every line ending exactly as delivered, no terminator added to a
#' last line that carried none. The copy is byte-identical to what
#' `iconv(from = encoding, to = "UTF-8")` of the whole file would produce, and
#' the two readers therefore see the same value in every cell -- including a
#' quoted value containing CRLF, or a lone CR, which are the two forms a
#' line-oriented converter cannot preserve.
#'
#' That is what the block boundary is for. Each block is trimmed back to its
#' last `0x0A` byte and the remainder carried into the next one, so the
#' converter never cuts a line and never has to decide what a line ending is; a
#' line longer than one block simply grows the buffer until a newline arrives.
#' The pieces a block is then converted in (see [dta_transcode_spans()]) are cut
#' on newline bytes for the same reason, and rejoined verbatim, so they are a
#' fact about how fast iconv runs and not about what the copy contains.
#' A block whose bytes are not valid in the declared encoding, or which holds a
#' `0x00` byte (no delimited file has any use for one, and it is the signature
#' of a binary file or of a wide encoding declared as a narrow one), aborts the
#' conversion naming the byte offset at which the trouble starts.
#'
#' @section Cost:
#' One pass, single-threaded, of an order of 20 MB/s. Measured on this
#' package's reference input -- a 172 MiB, 1,000,000 x 20 latin1 CSV -- it ran
#' at 21 MB/s (three runs spanning 8.2-8.4 s), against 18-19 MB/s for a
#' line-based converter over the same file. Byte fidelity is therefore not paid
#' for in speed, but only because the block is handed to [base::iconv()] in
#' pieces: iconv is markedly slower on one very long string than on a vector of
#' short ones (12 MB/s for a 4 MiB string, 33 MB/s for the same bytes as 64 KiB
#' pieces), and converting a block whole made this the slower of the two.
#'
#' A gzip-compressed source is read through [base::gzfile()] and costs about a
#' tenth more per megabyte of *output* (19 MB/s decoded, from 7 MB/s of
#' compressed input).
#'
#' Rates on another machine will differ; the shape will not. What matters for
#' sizing is that the pass is linear in the file and pays nothing per column,
#' and that it happens once per session per file rather than once per
#' `check()`.
#'
#' The copy is the size of the decoded text: about the size of the source for
#' latin1 that is mostly ASCII, up to twice it for text that is wholly
#' accented, and the full decoded size for a `.gz` source. Free space under
#' `tempdir()`, not memory, is therefore what a very large non-UTF-8 delivery
#' needs.
#'
#' @param path Character path to the file, optionally gzip-compressed.
#' @param encoding The encoding the file is declared to be in, as accepted by
#'   [base::iconv()]. A name this platform cannot convert from, and a wide
#'   encoding (see [dta_encoding_is_wide()]), are both refused before the file
#'   is opened.
#' @return The path of a UTF-8 copy under `tempdir()`. Cached for the session
#'   on the source's path, size and modification time together with the
#'   declared encoding, so repeated loads of an unchanged file convert once; a
#'   new copy of a re-delivered file supersedes the old one, which is deleted.
#' @keywords internal
dta_transcode_to_utf8 <- function(path, encoding) {
  # Both refusals happen before the file is opened. An encoding name iconv()
  # cannot use, and one whose characters can contain a newline byte, are
  # properties of the SPECIFICATION rather than of the delivery: reporting them
  # costs nothing and needs no I/O to establish.
  dta_check_encoding_supported(encoding, path)

  if (dta_encoding_is_wide(encoding)) {
    cli::cli_abort(c(
      "A file declaring {.val {encoding}} cannot be converted block by block.",
      "x" = "{.path {path}} would have to be split on the byte {.val 0x0A}, which is part of an ordinary character in this encoding.",
      "i" = "Load it with {.code stream = \"never\"}, or convert the file to UTF-8 first."
    ))
  }

  source <- normalizePath(path, winslash = "/", mustWork = FALSE)

  key <- dta_hash_object(list(
    path = source,
    # Metadata, not contents: hashing the bytes to decide whether to convert
    # them would cost the very read this exists to perform only once.
    size = file.size(path),
    mtime = file.mtime(path),
    encoding = encoding
  ))

  if (exists(key, envir = `__DTAtools_transcode_cache__`, inherits = FALSE)) {
    cached <- get(key, envir = `__DTAtools_transcode_cache__`, inherits = FALSE)
    # The copy lives under tempdir(), which the session owns but does not
    # police. A cleaner that removed it must send us back to converting, not
    # to an Arrow error about a file that is not there.
    if (is.list(cached) && file.exists(cached$copy)) {
      return(cached$copy)
    }
  }

  destination <- tempfile(fileext = ".csv")
  block <- dta_transcode_block_bytes()

  input <- if (tolower(tools::file_ext(path)) %in% dta_compression_extensions()) {
    gzfile(path, open = "rb")
  } else {
    file(path, open = "rb")
  }
  on.exit(close(input), add = TRUE)

  output <- file(destination, open = "wb")
  on.exit(close(output), add = TRUE)

  # A half-written copy must not outlive the failure that produced it, and must
  # never be reachable through the cache -- so the entry is recorded only after
  # the last block has been written.
  finished <- FALSE
  on.exit(if (!finished) unlink(destination), add = TRUE)

  # A double, for the reason given above `dta_narrow_count()`: a file long
  # enough to matter here runs an integer counter past its range.
  bytes_done <- 0

  # Bytes left over from the previous block because no line ended in them. The
  # copy is byte-faithful precisely because this is carried rather than
  # terminated: nothing here decides what a line ending is.
  carry <- raw(0)

  repeat {
    chunk <- readBin(input, "raw", n = block)
    if (length(chunk) == 0) {
      break
    }

    buffer <- if (length(carry) == 0) chunk else c(carry, chunk)
    cut <- dta_last_newline(buffer)

    if (is.na(cut)) {
      # A single line longer than one block. Hold everything and read on: the
      # alternative is cutting a line in half, and with it any multi-byte
      # character that happens to straddle the cut.
      carry <- buffer
      next
    }

    dta_transcode_write_block(
      buffer[seq_len(cut)], output, encoding, path, bytes_done
    )
    bytes_done <- bytes_done + cut
    carry <- if (cut < length(buffer)) {
      buffer[(cut + 1L):length(buffer)]
    } else {
      raw(0)
    }
  }

  # A last line with no terminator: written exactly as it arrived, with none
  # added. The in-memory reader does not invent one either.
  if (length(carry) > 0) {
    dta_transcode_write_block(carry, output, encoding, path, bytes_done)
  }

  finished <- TRUE
  # An earlier copy of the SAME delivery is now superseded, whatever fingerprint
  # or encoding it was made under. Left in place it would keep a second full-size
  # copy of the file alive under `tempdir()` for the rest of the session, once
  # per re-delivery.
  dta_transcode_cache_evict(source)
  assign(
    key,
    list(source = source, copy = destination),
    envir = `__DTAtools_transcode_cache__`
  )
  destination
}

#' @title Refuse an Encoding Name This Platform Cannot Convert From
#' @description
#' A misspelled encoding (`"latin-1"`, `"cp-1252"`) used to surface as
#' [base::iconv()]'s own error -- rendered in the system language, naming
#' neither the file nor the handler that declared it, and reaching the user
#' from somewhere in the middle of a conversion. Asking iconv the question up
#' front turns it into this package's own message, before the file is opened.
#' @param encoding The declared encoding name.
#' @param path The file it was declared for, for the message.
#' @return `invisible(TRUE)`; aborts otherwise.
#' @keywords internal
dta_check_encoding_supported <- function(encoding, path) {
  # `""` rather than the `character(0)` that reads more naturally: iconv() short
  # circuits an empty vector without ever asking the platform for a converter,
  # so `iconv(character(0), from = "cp-1252", to = "UTF-8")` returns
  # `character(0)` and validates precisely nothing. One empty string is the
  # smallest input that actually opens the conversion.
  supported <- tryCatch(
    {
      iconv("", from = encoding, to = "UTF-8")
      TRUE
    },
    error = function(e) FALSE
  )

  if (isTRUE(supported)) {
    return(invisible(TRUE))
  }

  cli::cli_abort(c(
    "{.val {encoding}} is not an encoding this platform can convert from.",
    "x" = "{.path {path}} is declared as {.val {encoding}}.",
    "i" = "{.code iconvlist()} lists the names the platform accepts; R also takes its own aliases {.val latin1} and {.val UTF-8}."
  ))
}

#' @title The Last Newline Byte in a Block
#' @description
#' Where [dta_transcode_to_utf8()] cuts a block so that no line is split. The
#' search runs backwards in doubling windows rather than over the whole block,
#' because a block is megabytes and a line is bytes: `which(buffer == 0x0A)`
#' allocates a logical vector the size of the block to answer a question that a
#' 64 KiB tail answers for every real delimited file.
#' @param bytes A raw vector.
#' @return The index of the last `0x0A`, or `NA_integer_` when there is none.
#' @keywords internal
dta_last_newline <- function(bytes) {
  n <- length(bytes)
  if (n == 0) {
    return(NA_integer_)
  }

  newline <- as.raw(0x0a)
  window <- min(n, 65536L)

  repeat {
    from <- n - window + 1L
    hits <- which(bytes[from:n] == newline)
    if (length(hits) > 0) {
      return(from + hits[[length(hits)]] - 1L)
    }
    if (window >= n) {
      return(NA_integer_)
    }
    window <- as.integer(min(n, window * 2))
  }
}

# How large a piece of a block [base::iconv()] is handed at a time. NOT the
# block, which is what bounds memory: this is the string length at which iconv
# is fastest, and the block is cut into pieces of about this size before being
# converted.
#
# The two are separate because iconv's cost is not linear in the length of the
# string it is given. Measured on the 161 MiB reference input, one 4 MiB string
# converts at 12 MB/s and the same bytes as 64 KiB pieces at 33 MB/s -- the
# whole difference between this converter being slower than the line-based one
# it replaced and being half again faster. The plateau is wide (4 KiB to
# 256 KiB are all within 10% of each other), so this is not a tuned number and
# does not want tuning.
`__DTAtools_transcode_piece_bytes__` <- 65536L

#' @title Where a Block Can Be Cut Without Splitting a Line
#' @description
#' Spans of about 64 KiB, each ending at a `0x0A`, covering the block exactly
#' once (see the constant above). [base::iconv()] is handed the pieces as a
#' character vector rather than the block as one string, which is much faster,
#' and their conversions concatenate to exactly the conversion of the whole
#' block: the cuts fall on newline bytes, which no ASCII-compatible encoding
#' puts inside a character.
#'
#' The last span runs to the end of the block whatever is there, so a block
#' with no newline in it at all -- a single line longer than the block -- is
#' one span and is converted as it was before.
#' @param bytes The raw block.
#' @return A list of `starts` and `ends`, both integer and the same length.
#' @keywords internal
dta_transcode_spans <- function(bytes) {
  n <- length(bytes)
  piece <- `__DTAtools_transcode_piece_bytes__`

  if (n <= piece) {
    return(list(starts = 1L, ends = n))
  }

  # Bounded by n/piece, so the growth is over tens of elements, not millions.
  starts <- integer(0)
  ends <- integer(0)
  from <- 1L

  while (n - from + 1L > piece) {
    # Searched in the window rather than over the block: `which(bytes == 0x0a)`
    # would walk all 4 MiB to answer a question the next 64 KiB answers.
    at <- dta_last_newline(bytes[from:(from + piece - 1L)])

    if (is.na(at)) {
      # A line longer than one piece. Rather than cut it, let the remainder of
      # the block be one span -- the same thing the block loop does with a line
      # longer than a block.
      break
    }

    starts <- c(starts, from)
    ends <- c(ends, from + at - 1L)
    from <- from + at
  }

  list(starts = c(starts, from), ends = c(ends, n))
}

#' @title Convert One Block and Write It
#' @description
#' The body of [dta_transcode_to_utf8()]'s loop: the two ways a block can fail
#' are both this function's, so that the loop above states only how the file is
#' cut into blocks.
#' @param bytes The raw block, ending at a line boundary.
#' @param output An open binary connection.
#' @param encoding The declared source encoding.
#' @param path The source file, for the messages.
#' @param offset How many source bytes precede this block, so that a failure can
#'   be located in the file rather than in the block.
#' @return `invisible(NULL)`; aborts on an undecodable block.
#' @keywords internal
dta_transcode_write_block <- function(bytes, output, encoding, path, offset) {
  spans <- dta_transcode_spans(bytes)

  # rawToChar() is the one thing here that a NUL byte defeats -- an R string
  # cannot hold one -- and its own error is a base-R message in the system
  # language about a "raw vector", which says nothing about the delivery. The
  # scan for the offending byte runs only on this path, where one more pass over
  # one block costs nothing.
  text <- tryCatch(
    vapply(
      seq_along(spans$starts),
      function(i) rawToChar(bytes[spans$starts[[i]]:spans$ends[[i]]]),
      character(1)
    ),
    error = function(e) NULL
  )

  if (is.null(text)) {
    at <- dta_format_count(offset + which(bytes == as.raw(0x00))[[1]])
    cli::cli_abort(c(
      "{.path {path}} holds a {.val 0x00} byte, which text cannot.",
      "x" = "The first one is at byte {at}.",
      "i" = "A delimited file has no use for a NUL byte: this is usually a binary file, or a wide encoding declared as a narrow one."
    ))
  }

  converted <- iconv(text, from = encoding, to = "UTF-8")

  # iconv() answers NA for text it cannot decode, which means the declared
  # encoding is wrong for this file. Writing the NA would replace a whole piece
  # of rows with the two characters "NA" and dropping it would lose them;
  # neither is a thing to do silently to someone's data.
  if (anyNA(converted)) {
    at <- dta_format_count(dta_first_undecodable_byte(bytes, encoding, offset))
    cli::cli_abort(c(
      "{.path {path}} cannot be decoded as {.val {encoding}}.",
      "x" = "The bytes at offset {at} are not valid {.val {encoding}}.",
      "i" = "A byte offset, not a line number: the file is converted in blocks rather than a line at a time.",
      "i" = "Declare the encoding the file was actually written in."
    ))
  }

  # Written piece by piece rather than pasted back into one string: the
  # concatenation is what the file gets either way, and one 4 MiB string per
  # block is an allocation with nothing to show for it.
  for (piece in converted) {
    writeBin(charToRaw(piece), output)
  }
  invisible(NULL)
}

#' @title Where in a Block the Decoding First Fails
#' @description
#' A block is converted in pieces of tens of thousands of bytes, so
#' [base::iconv()] answers NA for a whole piece and says nothing about where in
#' it the trouble is. "Somewhere in a 60 GB file" is not actionable, so the
#' failed block alone is re-converted a
#' line at a time to find the first line that does not decode. It runs only
#' after a conversion has already failed, and it is the last thing that happens
#' before the abort.
#' @param bytes The raw block that failed.
#' @param encoding The declared source encoding.
#' @param offset How many source bytes precede this block.
#' @return The 1-based byte offset in the file at which the first undecodable
#'   line begins, or the block's own first byte when no single line reproduces
#'   the failure.
#' @keywords internal
dta_first_undecodable_byte <- function(bytes, encoding, offset) {
  breaks <- which(bytes == as.raw(0x0a))
  starts <- c(1L, breaks + 1L)
  ends <- c(breaks, length(bytes))

  for (i in seq_along(starts)) {
    if (starts[[i]] > ends[[i]]) {
      next
    }
    line <- tryCatch(
      rawToChar(bytes[starts[[i]]:ends[[i]]]),
      error = function(e) NULL
    )
    if (is.null(line)) {
      next
    }
    if (is.na(iconv(line, from = encoding, to = "UTF-8"))) {
      return(offset + starts[[i]])
    }
  }

  offset + 1
}

#' @title Drop Every Cached Copy of One Delivery
#' @description
#' A re-delivered file is a new cache key, and the copy made for the previous
#' one is then unreachable but still on disk -- a second full-size copy under
#' `tempdir()` for the rest of the session, once per re-delivery. This deletes
#' it and drops its entry, so a session holds at most one copy per delivered
#' path.
#'
#' A lazily held Dataset scanning a copy that is evicted this way is not left
#' broken: `dta_refresh_transcoded_dataset()` re-opens a dataset whose copy has
#' gone, and it was in any case reading a copy of data that has since been
#' replaced.
#' @param source The normalised path of the delivered file.
#' @return `invisible(NULL)`.
#' @keywords internal
dta_transcode_cache_evict <- function(source) {
  cache <- `__DTAtools_transcode_cache__`

  for (key in ls(envir = cache, all.names = TRUE)) {
    entry <- get(key, envir = cache, inherits = FALSE)
    if (!is.list(entry) || !identical(entry$source, source)) {
      next
    }
    unlink(entry$copy)
    rm(list = key, envir = cache)
  }

  invisible(NULL)
}

#' @title Attribute Name Carrying the File a Dataset Was Opened From
#' @description
#' The R attribute under which [dta_open_normalized_dataset()] records the
#' ORIGINAL path when what it actually opened was a transcoded copy.
#'
#' `dta_table_change_signal()` fingerprints a lazy dataset by the files behind
#' it -- their paths, sizes and modification times. Left to `$files`, a
#' transcoded dataset would be fingerprinted by the temporary copy, whose mtime
#' is when the conversion ran: the same unchanged delivery would then look like
#' a different table in every session, and `check()` would revalidate it every
#' time. The user's file is the thing that has or has not changed, so the
#' user's file is what the signal must describe.
#'
#' An R attribute on the arrow object, exactly as the table content stamp is
#' (see `dta_table_hash_key`): an arrow `Dataset` is an R6 object, i.e. an
#' environment, so the attribute is shared by every reference to that dataset
#' and absent from anything Arrow builds anew from it.
#' @keywords internal
dta_dataset_source_key <- "dta_dataset_source"

#' @title Record the File a Dataset Was Really Opened From
#' @param dataset An `arrow::Dataset`, or any other object (returned
#'   unchanged).
#' @param files Character. The path(s) the caller wants the dataset identified
#'   by.
#' @return `dataset`, stamped where that was possible.
#' @keywords internal
dta_stamp_dataset_source <- function(dataset, files) {
  if (!inherits(dataset, "Dataset")) {
    return(dataset)
  }
  if (!is.character(files) || length(files) == 0 || any(!nzchar(files))) {
    return(dataset)
  }

  # The stamp is an optimisation, never a requirement: an arrow build that
  # refuses the attribute yields an unstamped dataset, which is identified by
  # its own `$files` exactly as before this existed.
  tryCatch(
    {
      attr(dataset, dta_dataset_source_key) <- files
      dataset
    },
    error = function(e) dataset
  )
}

#' @title The Files a Dataset Should Be Identified By
#' @description
#' The stamp left by [dta_stamp_dataset_source()] when there is one, and the
#' dataset's own `$files` otherwise. This is what `dta_table_change_signal()`
#' fingerprints, so that a dataset scanning a transcoded copy is recognised by
#' the delivered file rather than by the copy.
#' @param x An `arrow::Dataset`.
#' @return A character vector of paths, empty when none can be determined.
#' @keywords internal
dta_dataset_source_files <- function(x) {
  stamped <- attr(x, dta_dataset_source_key, exact = TRUE)

  if (is.character(stamped) && length(stamped) > 0 && all(nzchar(stamped))) {
    return(stamped)
  }

  tryCatch(x$files, error = function(e) character(0))
}

#' @title Attribute Name Carrying What It Takes to Re-Open a Transcoded Dataset
#' @description
#' The R attribute under which [dta_open_normalized_dataset()] records how to
#' make itself again: the delivered path, the fingerprint (size and
#' modification time) the copy was converted from, and the argument list the
#' dataset was opened with.
#'
#' It exists because a transcoded dataset is a scan plan over a copy of the
#' delivery rather than over the delivery itself, and a copy does not follow
#' what it was copied from. Left to itself, an edited delivery changed the
#' change signal (which is keyed on the delivery, see [dta_dataset_source_key]),
#' opened `check()`'s skip gate, and was then answered by rescanning the STALE
#' copy -- reporting the old data's verdict as a fresh one.
#'
#' On the arrow object rather than beside it, for the same reason the source
#' stamp is: a `Dataset` is an R6 object, so the attribute travels with every
#' reference to it, including the one held in `x@tables`, and cannot be
#' separated from the dataset it describes by anything that merely passes the
#' dataset around.
#' @keywords internal
dta_dataset_transcode_key <- "dta_dataset_transcode"

#' @title The Identity of a Delivered File, Cheaply
#' @description
#' Size and modification time -- the same two facts the transcode cache keys on
#' and `dta_table_change_signal()` fingerprints a dataset by. Contents are
#' deliberately not consulted: reading the file to find out whether it needs
#' reading is the cost this exists to avoid.
#' @param path A file path.
#' @return A list of `size` and `mtime`, both `NA` for a file that is not there.
#' @keywords internal
dta_source_fingerprint <- function(path) {
  list(size = file.size(path), mtime = file.mtime(path))
}

#' @title Record How to Re-Open a Transcoded Dataset
#' @param dataset An `arrow::Dataset`, or any other object (returned
#'   unchanged).
#' @param plan A list of `path` (the delivered file), `fingerprint` (from
#'   [dta_source_fingerprint()]) and `args` (the argument list
#'   [dta_open_normalized_dataset()] was called with).
#' @return `dataset`, stamped where that was possible.
#' @keywords internal
dta_stamp_dataset_transcode <- function(dataset, plan) {
  if (!inherits(dataset, "Dataset") || !is.list(plan)) {
    return(dataset)
  }

  # As with the source stamp: an arrow build that refuses the attribute yields
  # an unstamped dataset, which behaves exactly as one did before this existed
  # -- it scans the copy it was opened on and never refreshes.
  tryCatch(
    {
      attr(dataset, dta_dataset_transcode_key) <- plan
      dataset
    },
    error = function(e) dataset
  )
}

#' @title Make a Transcoded Dataset Follow Its Delivery
#' @description
#' Returns the dataset unchanged unless it is scanning a transcoded copy whose
#' delivery has since changed, or whose copy has gone; in either case the file
#' is converted again (through the cache, so an unchanged delivery already
#' converted in this session costs nothing) and re-opened with the argument list
#' the first open recorded.
#'
#' Called from `check()` after the skip gate, so an unchanged table never
#' reaches it and the cost on the common path is one `attr()` lookup. Re-opening
#' rather than patching: the new delivery has its own header, which has to be
#' read, cleaned and checked for collisions exactly as the first one was, and
#' its own schema, which the re-opened dataset must carry.
#'
#' A delivery that has VANISHED is returned unchanged rather than reported here.
#' `check()`'s missing-file guard is the one place in the package that answers
#' for an absent delivery, it answers by the delivered path (see
#' `dta_missing_table_files()`), and two places reporting the same absence
#' differently is how the two would drift apart.
#' @param table A table holding of any kind.
#' @return The same object, or a freshly opened `arrow::Dataset`.
#' @keywords internal
dta_refresh_transcoded_dataset <- function(table) {
  plan <- attr(table, dta_dataset_transcode_key, exact = TRUE)

  if (!is.list(plan) || !is.character(plan$path) || length(plan$path) != 1) {
    return(table)
  }

  if (!file.exists(plan$path)) {
    return(table)
  }

  # The copy is under tempdir(), which the session owns but does not police, and
  # which `dta_transcode_cache_evict()` itself prunes. A dataset whose copy has
  # gone is re-made rather than left to fail inside the scanner.
  copies <- tryCatch(table$files, error = function(e) character(0))
  copy_present <- length(copies) > 0 && all(file.exists(copies))

  if (copy_present && identical(dta_source_fingerprint(plan$path), plan$fingerprint)) {
    return(table)
  }

  do.call(dta_open_normalized_dataset, plan$args)
}

#' @title The Header Names of a Delimited File
#' @description
#' Opens the file for its header alone. An `arrow::Dataset` reads its schema
#' from the first block and nothing else, so the cost is one block of I/O
#' whatever the file weighs: a median of 10 ms on a 169 MB CSV, against 5 ms on
#' a two-row one.
#'
#' Arrow reports a header it cannot use -- two columns with the same name, most
#' commonly -- as a schema failure whose message does not mention the header at
#' all ("Could not read schema ... Is this a 'csv' file?"). It is re-raised here
#' naming the file and the likely cause, because this is the first point at
#' which the file is touched and therefore where the user will be looking.
#' @param path Character path to the delimited file.
#' @param parse_options An `arrow::CsvParseOptions`.
#' @param has_header Logical; whether the first line names the columns.
#' @param encoding The declared file encoding.
#' @return A character vector of raw (uncleaned) column names.
#' @keywords internal
dta_delim_header_names <- function(path, parse_options, has_header, encoding = "UTF-8") {
  read_options <- if (isTRUE(has_header)) {
    arrow::csv_read_options(block_size = dta_stream_block_size())
  } else {
    arrow::csv_read_options(
      block_size = dta_stream_block_size(),
      autogenerate_column_names = TRUE
    )
  }

  dataset <- tryCatch(
    arrow::open_delim_dataset(
      path,
      parse_options = parse_options,
      read_options = read_options
    ),
    error = function(e) {
      reason <- conditionMessage(e)
      cli::cli_abort(c(
        "Cannot read the column names of {.path {path}}.",
        "x" = "{reason}",
        "i" = "Repeated or unparseable names in the first line are the usual cause."
      ))
    }
  )

  raw_names <- names(dataset$schema)

  # The dataset reader has no re-encoding step (see dta_open_normalized_dataset())
  # so a non-ASCII header arrives here as raw bytes mislabelled UTF-8. The
  # in-memory reader DOES re-encode, and would otherwise be handed names that
  # disagree with the ones it read for itself. The lazy path reaches this with
  # `encoding = "UTF-8"` and the path of an already-transcoded copy, so its
  # header is converted exactly once, here or there, never twice.
  if (!dta_encoding_is_utf8(encoding)) {
    converted <- tryCatch(
      iconv(raw_names, from = encoding, to = "UTF-8"),
      error = function(e) NULL
    )
    if (!is.null(converted)) {
      raw_names <- ifelse(is.na(converted), raw_names, converted)
    }
  }

  raw_names
}

#' @title One Reader Configuration for Both Paths
#' @description
#' Everything the eager and the lazy reader need to read the same file the same
#' way, derived once. Both are then handed the SAME column names, the same row
#' to start at and the same column types; they differ only in when the bytes are
#' read.
#'
#' @section Why the names are supplied rather than parsed:
#' A header can need cleaning -- `" AGE "`, `"\"AGE\""` -- and the cleaned name
#' is what the specification declares. An `arrow::Dataset` has no `names<-`, so
#' the lazy path could never rename after the fact; and renaming through
#' `dplyr` returns an `arrow_dplyr_query`, which has no `$files`, so
#' [dta_table_change_signal()] could not fingerprint it and `check()` would
#' revalidate the table on every run. Supplying the cleaned names at open time
#' and skipping the header line is the only form that works for both, so both
#' use it -- including for a header that needed no cleaning, so that there is
#' one code path rather than two.
#'
#' The header is read once, by [dta_delim_header_names()]. Before this was
#' factored out, the eager reader read a file with a padded header TWICE: once
#' to learn the names, then again with the declared types pinned.
#'
#' @section Why every column is read as text:
#' When `specs` is supplied, EVERY column is pinned to `utf8` -- not only the
#' declared ones. Three reasons, in order of how badly they bite:
#'
#' 1. **The two paths must agree.** A lazy dataset infers its schema from the
#'    first block and locks it in, so this reader has always pinned everything
#'    to text; the eager reader used to leave an undeclared column to Arrow's
#'    inference. The same undeclared column was then a `double` in memory and a
#'    string when streamed, and a uniqueness rule over it counted `1.5` and
#'    `1.50` as one key on one path and two on the other -- the same file, two
#'    verdicts.
#' 2. **A scan cannot afford a wrong guess.** A column that looks integer-only
#'    in the first block but holds `"0.01"` far down aborts the whole read --
#'    `CSV conversion error to int64: invalid value '0.01'` -- potentially hours
#'    into a multi-hundred-million-row scan. Text never fails that way.
#' 3. **Inference destroys data.** A quoted `"007"` id arrives as the integer 7
#'    before any code in this package sees it.
#'
#' Reading as text loses nothing: [dta_coerce_table_to_specs()] and the rules'
#' own strict numeric conversion do all the real typing in R, and report what
#' they could not convert instead of aborting.
#'
#' When `specs` is `NULL` -- a bare `read_file()` on a handler, with no
#' specification to honour -- inference is left alone, exactly as before.
#'
#' @param path Character path to the delimited file.
#' @param specs A `DTAColumnSpecCollection` or `NULL`.
#' @param delim,quote,has_header Delimited-text parse options.
#' @param na Character vector of strings to read as missing, or `NULL`
#'   (the default) to keep Arrow's own default (`c("", "NA")`) unchanged.
#' @param handler A `DTAFileTabular` (or subclass) whose `newlines_in_values`
#'   and `encoding` apply, or `NULL` for the defaults.
#' @param encoding Character or `NULL` (the default). Overrides the encoding
#'   the handler declares. The one caller that supplies it is
#'   [dta_open_normalized_dataset()], which has already converted the file to a
#'   UTF-8 copy and is planning a read of *that*: the handler still says
#'   `latin1`, and honouring it here would decode the copy's header a second
#'   time. `NULL` leaves the handler in charge, which is every other route.
#' @return A list with `path`, `column_names`, `skip`, `col_types`,
#'   `parse_options`, `encoding` and `na`.
#' @keywords internal
dta_delim_reader_plan <- function(
  path,
  specs = NULL,
  delim = ",",
  quote = '"',
  has_header = TRUE,
  na = NULL,
  handler = NULL,
  encoding = NULL
) {
  settings <- dta_reader_parse_settings(handler)

  if (!is.null(encoding)) {
    settings$encoding <- encoding
  }

  # Spelled out rather than left to arrow's readr-flavoured translation, which
  # is what `delim = `/`quote = ` go through: every value below is the one that
  # translation produces, so the only behaviour that changes is the one setting
  # this package now exposes.
  parse_options <- arrow::csv_parse_options(
    delimiter = delim,
    quoting = nzchar(quote),
    quote_char = quote,
    double_quote = TRUE,
    escaping = FALSE,
    escape_char = "\\",
    newlines_in_values = settings$newlines_in_values,
    ignore_empty_lines = TRUE
  )

  raw_names <- dta_delim_header_names(
    path,
    parse_options = parse_options,
    has_header = has_header,
    encoding = settings$encoding
  )

  # With no header there is nothing to clean: Arrow's generated names (f0,
  # f1, ...) are used as-is.
  cleaned <- if (isTRUE(has_header)) dta_clean_column_names(raw_names) else raw_names

  duplicated_names <- unique(cleaned[duplicated(cleaned)])
  if (length(duplicated_names) > 0) {
    # Reachable only by cleaning: `A` and `" A"` are two names to Arrow and one
    # afterwards. Supplying the collided names to the reader would fail deep
    # inside Arrow with a message that never mentions the trimming.
    cli::cli_abort(c(
      "Cleaning the header of {.path {path}} left repeated column names.",
      "x" = "Repeated after trimming quotes and spaces: {.val {duplicated_names}}.",
      "i" = "Give the columns distinct names in the file."
    ))
  }

  col_types <- if (is.null(specs)) {
    NULL
  } else {
    # Built from fields rather than named arguments: a header may legitimately
    # carry an empty name, and an empty argument name is positional.
    arrow::schema(lapply(cleaned, function(nm) arrow::field(nm, arrow::utf8())))
  }

  list(
    path = path,
    column_names = cleaned,
    # The header line was consumed to learn the names; skipping it is what
    # keeps it from being parsed a second time as a data row.
    skip = as.integer(isTRUE(has_header)),
    col_types = col_types,
    parse_options = parse_options,
    encoding = settings$encoding,
    na = na
  )
}

#' @title Open a Delimited File Lazily, With Clean Column Names
#' @description
#' The lazy half of [dta_delim_reader_plan()]: the plan says how the file is to
#' be read, this opens it as an `arrow::Dataset` so nothing is read until a scan
#' asks for it. Its eager twin is [dta_read_delim_normalized()]; the two are
#' given the identical names, skip and column types, so a file cannot present
#' itself differently depending on which one loaded it.
#'
#' The scan's read block -- and with it the size of a batch, and peak memory --
#' comes from [dta_stream_block_size()]; see there for why `batch_rows` cannot
#' do that job on a delimited file.
#'
#' @section A file that is not UTF-8:
#' Arrow re-encodes by wrapping the INPUT STREAM, which only the in-memory
#' reader owns: a dataset opens its own files, so `csv_read_options(encoding =)`
#' is accepted here and then silently ignored -- the bytes come back either as
#' `binary` or, with UTF-8 checking off, as a string of undecoded bytes. Either
#' way the same file would validate differently depending on how it was loaded,
#' which is the one outcome this reader exists to prevent.
#'
#' This used to be answered by refusing, which made every non-UTF-8 delivery
#' unstreamable however large it was. It is now answered by
#' [dta_transcode_to_utf8()]: the file is converted once, in bounded memory, to
#' a UTF-8 copy under `tempdir()`, and the scan reads that. The copy is cached
#' per session, so a second `load_file()` on an unchanged file reuses it.
#'
#' The returned dataset is stamped twice. With the ORIGINAL path (see
#' [dta_dataset_source_key]), so that `dta_table_change_signal()` identifies the
#' delivered file rather than the temporary copy, whose modification time is
#' merely when the conversion happened to run; and with everything needed to
#' open it again (see [dta_dataset_transcode_key]), so that
#' [dta_refresh_transcoded_dataset()] can make the scan follow a delivery that
#' changes after it was loaded rather than rescanning a copy of what it used to
#' hold.
#'
#' @param path Character path to the delimited file.
#' @param specs A `DTAColumnSpecCollection` or `NULL`. When supplied, every
#'   column is read as text; see [dta_delim_reader_plan()].
#' @param delim,quote,has_header Delimited-text parse options.
#' @param na Character vector of strings to read as missing, or `NULL`
#'   (the default) to keep Arrow's own default (`c("", "NA")`) unchanged.
#' @param handler A `DTAFileTabular` (or subclass) whose `newlines_in_values`
#'   and `encoding` apply, or `NULL` for the defaults.
#' @return An `arrow::Dataset`.
#' @keywords internal
dta_open_normalized_dataset <- function(
  path,
  specs = NULL,
  delim = ",",
  quote = '"',
  has_header = TRUE,
  na = NULL,
  handler = NULL
) {
  settings <- dta_reader_parse_settings(handler)

  # Decided before the plan does any I/O, because it decides which file the
  # plan reads.
  source_path <- path
  transcoded <- !dta_encoding_is_utf8(settings$encoding)
  fingerprint <- NULL

  if (transcoded) {
    # Taken BEFORE the conversion, so that what is recorded is the state of the
    # delivery the copy was made from. Taken after, a file rewritten while the
    # conversion ran would be recorded as already converted.
    source_path <- normalizePath(source_path, winslash = "/", mustWork = FALSE)
    fingerprint <- dta_source_fingerprint(source_path)
    path <- dta_transcode_to_utf8(source_path, settings$encoding)
  }

  plan <- dta_delim_reader_plan(
    path,
    specs = specs,
    delim = delim,
    quote = quote,
    has_header = has_header,
    na = na,
    handler = handler,
    # The copy is UTF-8 by construction; the handler still declares the
    # source's encoding, and honouring that here would decode the copy twice.
    encoding = if (transcoded) "UTF-8" else NULL
  )

  open_args <- list(
    path,
    parse_options = plan$parse_options,
    read_options = arrow::csv_read_options(
      column_names = plan$column_names,
      skip_rows = plan$skip,
      block_size = dta_stream_block_size()
    ),
    col_types = plan$col_types
  )

  if (!is.null(plan$na)) {
    open_args$na <- plan$na
  }

  dataset <- do.call(arrow::open_delim_dataset, open_args)

  if (!transcoded) {
    # Nothing was substituted, so the dataset's own `$files` already names the
    # delivered file: leaving it unstamped keeps a UTF-8 load byte-for-byte the
    # load it was before transcoding existed.
    return(dataset)
  }

  dataset <- dta_stamp_dataset_source(dataset, source_path)

  # Everything needed to make this dataset again from whatever the delivery
  # holds NOW. The argument list is this function's own, not
  # `arrow::open_delim_dataset()`'s: a re-delivery may have a different header,
  # and the column names, skip and column types below are all derived from the
  # header that was there at the time. Re-entering here re-derives them; reusing
  # the arrow-level arguments would pin the new file to the old file's columns.
  dta_stamp_dataset_transcode(dataset, list(
    path = source_path,
    fingerprint = fingerprint,
    args = list(
      path = source_path,
      specs = specs,
      delim = delim,
      quote = quote,
      has_header = has_header,
      na = na,
      handler = handler
    )
  ))
}

#' @title Read a Delimited File Eagerly, With Clean Column Names
#' @description
#' The eager half of [dta_delim_reader_plan()]: same names, same skip, same
#' column types as [dta_open_normalized_dataset()] gives the scanner, read into
#' memory as an Arrow `Table` instead of left on disk.
#'
#' A non-UTF-8 `encoding` is honoured here by Arrow itself: this reader owns
#' the input stream it reads through, and `csv_read_options(encoding = )`
#' re-encodes that stream. That is the one thing the lazy opener cannot do --
#' a dataset opens its own files -- which is why it converts the file to a
#' UTF-8 copy first (see [dta_transcode_to_utf8()]) and reads that instead. Two
#' mechanisms, one result: the same bytes decode the same way whichever reader
#' loaded them.
#'
#' @param path Character path to the delimited file.
#' @param delim,quote,has_header Delimited-text parse options.
#' @param specs A `DTAColumnSpecCollection` or `NULL`. When supplied, every
#'   column is read as text; see [dta_delim_reader_plan()].
#' @param na Character vector of strings to read as missing, or `NULL`
#'   (the default) to keep Arrow's own default (`c("", "NA")`) unchanged.
#' @param handler A `DTAFileTabular` (or subclass) whose `newlines_in_values`
#'   and `encoding` apply, or `NULL` for the defaults.
#' @return An Arrow `Table`.
#' @keywords internal
dta_read_delim_normalized <- function(
  path,
  delim,
  quote,
  has_header,
  specs = NULL,
  na = NULL,
  handler = NULL
) {
  plan <- dta_delim_reader_plan(
    path,
    specs = specs,
    delim = delim,
    quote = quote,
    has_header = has_header,
    na = na,
    handler = handler
  )

  read_args <- list(
    path,
    parse_options = plan$parse_options,
    # `read_options` wins over the `col_names`/`skip` arguments, which is why
    # they are not passed at all: two sources for one setting is how the two
    # readers drifted apart in the first place.
    read_options = arrow::csv_read_options(
      column_names = plan$column_names,
      skip_rows = plan$skip,
      encoding = plan$encoding
    ),
    col_types = plan$col_types,
    as_data_frame = FALSE
  )

  if (!is.null(plan$na)) {
    read_args$na <- plan$na
  }

  do.call(arrow::read_delim_arrow, read_args)
}

#' @title Missing-Value Markers for a Tabular Handler
#' @description
#' Maps a `DTAFileTabular` handler's declared `missing_values` onto the `na`
#' argument the readers expect. An empty cell is always treated as missing
#' regardless of what a handler declares; anything in `missing_values` adds to
#' that rather than replacing it.
#' @param x A `DTAFileTabular` object (or subclass).
#' @return A character vector to pass as `na`, or `NULL` when the handler
#'   declares no missing-value markers -- `NULL` lets the reader keep Arrow's
#'   own default (`c("", "NA")`) rather than narrowing it.
#' @keywords internal
dta_reader_na_values <- function(x) {
  declared <- x@missing_values

  # The property DEFAULTS to "" on every handler, so a bare "" (or NA) is
  # "nothing declared", not a declaration that only the empty cell is missing.
  # Without this filter every existing handler would silently narrow Arrow's
  # default missing set (`""` and `"NA"`) down to just `""`, turning literal
  # "NA" text into values -- a global behaviour change no one asked for.
  declared <- declared[!is.na(declared) & nzchar(declared)]

  if (length(declared) == 0) {
    return(NULL)
  }

  unique(c("", declared))
}


#' Print Information About a DTAFile Object
#'
#' This method prints detailed information about a \code{DTAFile} object, including its filename, pattern, and the number of files associated with it. The information is displayed using the \code{cli} package for formatted output.
#'
#' @importFrom cli cli_alert_info cli_alert
#'
#' @param x A \code{DTAFile} object whose information is to be printed.
#'
#' @return The input object \code{x}, returned invisibly.
#'
#' @details
#' The function displays the filename and pattern of the \code{DTAFile} object. It also prints the minimum and maximum number of files, or a single value if both are equal and set; an unset bound prints as "unbounded".
#'
#' @examples
#' dta_file <- DTAFileCSV(filename = "data.csv")
#' print_info(dta_file)
#'
#' @name print_info
#' @seealso \code{\link{DTAFile}}
#' @export
# `inherits = FALSE` scopes this lookup to this package's namespace; without
# it, an attached package exporting a plain function of the same name would
# make this guard skip creating the generic. See `R/00_helpers.R` for the
# full account.
if (!exists("print_info", mode = "function", inherits = FALSE)) {
  print_info <- new_generic("print_info", "x")
}
method(print_info, DTAFileTabular) <- function(x) {
  # S7::super() is unused throughout this codebase (see
  # dta_matches_filename_base()), so the parent's filename/pattern lines are
  # repeated here rather than reached through it. The number-of-files logic
  # is NOT repeated, though -- that one is buggy enough (NULL-unsafe) to be
  # worth sharing instead of copying; see dta_print_file_count().
  cli::cli_alert_info("Filename: {x@filename}")
  cli::cli_alert("Pattern: {x@pattern}")
  dta_print_file_count(x)
  cli::cli_alert("Separator: {x@sep}")
  cli::cli_alert("Has header: {x@has_header}")
  cli::cli_alert("Quote: {x@quote}")
  cli::cli_alert("Newlines in values: {x@newlines_in_values}")
  cli::cli_alert("Encoding: {x@encoding}")

  invisible(x)
}
