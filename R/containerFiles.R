# Container layer: reads, checks, extracts and splits the two kinds of
# container a delivery may hold, zip archives and SAS transport (XPORT)
# libraries. Internal only: no routing, no binding, no exports, no global state.
# Names reach cli text only as values (see `.cli_escape()`), never pasted.

`__DTAtools_container_chunk__` <- 8388640L # 104,858 records of 80 bytes

dta_container_limits <- function() {
  # One literal getOption() per option: IQ-ENV-020 reads these call sites.
  # max_members bounds three counts: entries in one zip, members in one
  # library, and files one load_delivery() call creates. The two byte defaults
  # are far above any delivery seen (they are numbers so they can be tightened;
  # the app does).
  members <- getOption("DTAtools.container_max_members", 10000L)
  bytes <- getOption("DTAtools.container_max_bytes", 1e13)
  member_bytes <- getOption("DTAtools.container_max_member_bytes", 1e13)
  ratio <- getOption("DTAtools.container_max_ratio", 1100)
  check <- function(value, name) {
    if (!is.numeric(value) || length(value) != 1 || is.na(value) || value <= 0) {
      cli::cli_abort(
        c(
          "{.code options({name} = )} must be a single positive number.",
          "x" = "It is currently {.val {value}}."
        ),
        class = "dta_container_bad_option"
      )
    }
    as.numeric(value)
  }
  list(
    max_members = check(members, "DTAtools.container_max_members"),
    max_bytes = check(bytes, "DTAtools.container_max_bytes"),
    max_member_bytes = check(member_bytes, "DTAtools.container_max_member_bytes"),
    max_ratio = check(ratio, "DTAtools.container_max_ratio")
  )
}

dta_container_kind <- function(name) {
  n <- tolower(basename(as.character(name)))
  out <- rep(NA_character_, length(n))
  out[grepl("\\.zip$", n)] <- "zip"
  out[grepl("\\.xpt(\\.gz)?$", n)] <- "xpt"
  out
}

dta_container_too_large <- function(path, limit, value, allowed) {
  unit <- switch(limit,
    DTAtools.container_max_members = " entries or files",
    DTAtools.container_max_ratio = ":1",
    " bytes"
  )
  value_text <- paste0(dta_format_count(value), unit)
  allowed_text <- paste0(dta_format_count(allowed), unit)
  cli::cli_abort(
    c(
      "{.file {basename(path)}} is refused: it exceeds {.code {limit}}.",
      "x" = "{value_text} against a limit of {allowed_text}.",
      "i" = "Raise the limit with {.code options({limit} = )} in R if this delivery is genuine."
    ),
    class = c("dta_container_too_large", "dta_container_error"),
    path = path, limit = limit, value = value, allowed = allowed
  )
}

# ---- zip: names ----------------------------------------------------------------
dta_zip_entry_basename <- function(entry) sub(".*[/\\\\]", "", entry)

dta_zip_entry_segments <- function(entry) {
  strsplit(sub("[/\\\\]$", "", entry), "[/\\\\]")[[1]]
}

dta_zip_entry_problem <- function(name) {
  if (!nzchar(name)) {
    return("has an empty name")
  }
  if (!validUTF8(name)) {
    return("has a name that is not valid text")
  }
  if (grepl("[[:cntrl:]]", name)) {
    return("contains a control character")
  }
  if (grepl("^[/\\\\]", name)) {
    return("is an absolute path")
  }
  if (grepl("^[A-Za-z]:", name)) {
    return("starts with a drive letter")
  }
  if (grepl(":", name, fixed = TRUE)) {
    return("contains a colon")
  }
  if (grepl("[<>\"|?*]", name)) {
    return("contains a character Windows does not allow in file names")
  }
  segs <- dta_zip_entry_segments(name)
  if (length(segs) == 0 || any(segs == "")) {
    return("has an empty path segment")
  }
  if (any(segs %in% c(".", ".."))) {
    return("contains a '.' or '..' path segment")
  }
  if (any(grepl("[. ]$", segs))) {
    return("has a path segment ending in a dot or a space")
  }
  sup <- c("\u00b9", "\u00b2", "\u00b3") # superscript one, two, three
  reserved <- c(
    "CON", "PRN", "AUX", "NUL", "CONIN$", "CONOUT$",
    paste0("COM", c(0:9, sup)), paste0("LPT", c(0:9, sup))
  )
  if (any(toupper(sub("\\..*$", "", segs)) %in% reserved)) {
    return("uses a reserved Windows device name")
  }
  if (any(nchar(segs, type = "bytes") > 255)) {
    return("has a path segment longer than 255 bytes")
  }
  NA_character_
}

# The rules that make a name hostile wherever it is: extracted, it would land
# outside, or name something other than, the directory it is written into.
# Such a name refuses the whole archive, claimed or not. Every other problem of
# dta_zip_entry_problem() only makes the name impossible to extract on some
# file system; an entry like that is refused only when something needs it.
dta_zip_entry_escape <- function(name) {
  if (!nzchar(name)) {
    return("has an empty name")
  }
  if (!validUTF8(name)) {
    return("has a name that is not valid text")
  }
  if (grepl("^[/\\\\]", name)) {
    return("is an absolute path")
  }
  if (grepl("^[A-Za-z]:", name)) {
    return("starts with a drive letter")
  }
  segs <- dta_zip_entry_segments(name)
  if (length(segs) == 0 || any(segs == "")) {
    return("has an empty path segment")
  }
  if (any(segs %in% c(".", ".."))) {
    return("contains a '.' or '..' path segment")
  }
  NA_character_
}

# Refuses `path` naming the entries and what is wrong with each. Names are
# shown through encodeString(), so a control character prints as an escape
# instead of breaking the message. `hostile = FALSE`: the names are only ones
# some file systems cannot hold, and something needs them.
dta_zip_refuse_unsafe <- function(path, members, problems, hostile = TRUE) {
  lines <- .cli_escape(paste0(encodeString(members), ": ", problems))
  if (length(lines) > 10) lines <- c(lines[1:10], paste("and", length(lines) - 10, "more"))
  names(lines) <- rep("x", length(lines))
  head <- if (hostile) {
    "{.file {basename(path)}} is refused: {cli::qty(length(members))}{?an entry/entries} would be unsafe to extract."
  } else {
    "{.file {basename(path)}} is refused: {cli::qty(length(members))}{?an entry/entries} it needs {?has a name/have names} that some file systems (Windows) cannot write."
  }
  cli::cli_abort(
    c(head, lines),
    class = c("dta_container_unsafe_member", "dta_container_error"),
    path = path, members = members, problems = problems
  )
}

# System files: macOS resource forks (__MACOSX/), dot files (.DS_Store) and the
# Finder's custom-folder-icon file, named "Icon" plus a carriage return.
dta_zip_entry_ignored <- function(entry) {
  segs <- dta_zip_entry_segments(entry)
  any(segs == "__MACOSX") || any(startsWith(segs, ".")) || any(segs == "Icon\r")
}

dta_le_u16 <- function(b) sum(as.numeric(b) * c(1, 256))
dta_le_u32 <- function(b) sum(as.numeric(b) * c(1, 256, 65536, 16777216))

# APPNOTE.TXT 4.3.16 (EOCD; total entries at offset 10), 4.3.15 (ZIP64 locator,
# 20 bytes before the EOCD; record offset at 8), 4.3.14 (ZIP64 EOCD record;
# total entries at offset 32). When a locator is present the ZIP64 count is used
# whatever the 16-bit field says, because that is what the zip library uses.
dta_zip_entry_count <- function(path) {
  unreadable <- function(reason) {
    cli::cli_abort(
      c("{.file {basename(path)}} cannot be read as a zip archive.", "x" = "{reason}"),
      class = c("dta_container_unreadable", "dta_container_error"), path = path, reason = reason
    )
  }
  size <- file.size(path)
  if (is.na(size) || size < 22) unreadable("It is too short to be a zip archive.")
  n_tail <- min(size, 65557)
  con <- file(path, "rb")
  on.exit(close(con))
  seek(con, size - n_tail)
  tl <- readBin(con, "raw", n_tail)
  hits <- grepRaw(as.raw(c(0x50, 0x4b, 0x05, 0x06)), tl, fixed = TRUE, all = TRUE)
  hits <- hits[hits + 21 <= length(tl)]
  if (length(hits) == 0) unreadable("It has no end-of-central-directory record.")
  e <- hits[[length(hits)]]
  loc <- e - 20
  if (loc >= 1 && identical(tl[loc:(loc + 3)], as.raw(c(0x50, 0x4b, 0x06, 0x07)))) {
    rec_off <- dta_le_u32(tl[(loc + 8):(loc + 11)]) + dta_le_u32(tl[(loc + 12):(loc + 15)]) * 2^32
    seek(con, rec_off)
    rec <- readBin(con, "raw", 56)
    if (length(rec) < 56 || !identical(rec[1:4], as.raw(c(0x50, 0x4b, 0x06, 0x06)))) {
      unreadable("Its ZIP64 directory record is damaged.")
    }
    return(dta_le_u32(rec[33:36]) + dta_le_u32(rec[37:40]) * 2^32)
  }
  dta_le_u16(tl[(e + 10):(e + 11)])
}

# The zip library's reason for a failure, without what must not reach a user:
# the archive's server path (as given and as normalised, with either kind of
# separator) and the C source location the text ends with, e.g.
# " @rzip.c:115 (R_zip_list)". Only the archive's base name is left.
dta_zip_reason <- function(e, path) {
  full <- normalizePath(path, mustWork = FALSE)
  spellings <- unique(c(
    path, gsub("\\", "/", path, fixed = TRUE), gsub("/", "\\", path, fixed = TRUE),
    full, gsub("\\", "/", full, fixed = TRUE), gsub("/", "\\", full, fixed = TRUE)
  ))
  reason <- conditionMessage(e)
  for (s in spellings[nzchar(spellings)]) reason <- gsub(s, basename(path), reason, fixed = TRUE)
  sub(" @[^ ]+:[0-9]+ \\([^()]*\\)$", "", reason)
}

dta_zip_inspect <- function(path, limits = dta_container_limits()) {
  n <- dta_zip_entry_count(path)
  if (n > limits$max_members) dta_container_too_large(path, "DTAtools.container_max_members", n, limits$max_members)
  l <- tryCatch(zip::zip_list(path), error = function(e) {
    reason <- dta_zip_reason(e, path)
    cli::cli_abort(
      c("{.file {basename(path)}} cannot be read as a zip archive.", "x" = "{reason}"),
      class = c("dta_container_unreadable", "dta_container_error"), path = path, reason = reason
    )
  })
  if (!all(c("filename", "uncompressed_size", "type", "encryption") %in% names(l))) {
    reason <- "The installed zip package is too old to inspect archives safely; zip 3.0.0 or later is required."
    cli::cli_abort(
      c("{.file {basename(path)}} cannot be read as a zip archive.", "x" = "{reason}"),
      class = c("dta_container_unreadable", "dta_container_error"), path = path, reason = reason
    )
  }
  if (nrow(l) > limits$max_members) dta_container_too_large(path, "DTAtools.container_max_members", nrow(l), limits$max_members)
  entry <- enc2utf8(as.character(l$filename))
  type <- as.character(l$type)
  # A hostile name or a symbolic link refuses the archive before anything else.
  escape <- vapply(entry, dta_zip_entry_escape, character(1), USE.NAMES = FALSE)
  escape[is.na(escape) & type == "symlink"] <- "is a symbolic link"
  if (any(!is.na(escape))) dta_zip_refuse_unsafe(path, entry[!is.na(escape)], escape[!is.na(escape)])
  # Any other problem goes with the entry; routing refuses the archive only if
  # something needs that entry (dta_delivery_open_zip()).
  problems <- vapply(entry, dta_zip_entry_problem, character(1), USE.NAMES = FALSE)
  enc <- as.character(l$encryption)
  if (any(enc != "none")) {
    members <- entry[enc != "none"]
    shown <- encodeString(members)
    cli::cli_abort(
      c(
        "{.file {basename(path)}} is refused: {.file {shown}} {?is/are} encrypted.",
        "i" = "Encrypted archives are not supported; ask for an unencrypted delivery."
      ),
      class = c("dta_container_encrypted", "dta_container_error"), path = path, members = members
    )
  }
  is_dir <- type == "directory" | grepl("[/\\\\]$", entry)
  files <- entry[!is_dir]
  key <- tolower(gsub("\\", "/", files, fixed = TRUE))
  dup <- key %in% key[duplicated(key)]
  if (any(dup)) {
    members <- files[dup]
    shown <- encodeString(members)
    cli::cli_abort(
      "{.file {basename(path)}} is refused: {.val {shown}} name the same file once letter case is ignored.",
      class = c("dta_container_duplicate_member", "dta_container_error"), path = path, members = members
    )
  }
  total <- sum(as.numeric(l$uncompressed_size))
  ratio <- total / max(as.numeric(file.size(path)), 1)
  if (ratio > limits$max_ratio) dta_container_too_large(path, "DTAtools.container_max_ratio", ratio, limits$max_ratio)
  data.frame(
    entry = files,
    name = dta_zip_entry_basename(files),
    size = as.numeric(l$uncompressed_size[!is_dir]),
    ignored = vapply(files, dta_zip_entry_ignored, logical(1), USE.NAMES = FALSE),
    problem = problems[!is_dir],
    stringsAsFactors = FALSE
  )
}

# Each entry goes into a fresh directory of its own (e00001, e00002, ...), so
# nothing already on disk can be reached through another spelling (a Windows
# 8.3 alias, a case variant). junkpaths = TRUE for every name without a
# backslash. A backslash name (Windows PowerShell 5.1 Compress-Archive) can only
# be found by the zip library with junkpaths = FALSE, which writes its folders:
# that goes to a scratch directory beside the entry's own one, and the one file
# found there is moved in. Every name has passed dta_zip_entry_escape() (no
# '.'/'..' segment, either separator), which is what makes junkpaths = FALSE
# safe here, and dta_zip_entry_problem() (routing refuses the archive rather
# than extract an entry with a problem). `dest` is the container's private directory: on any failure all of
# it is deleted, so nothing of a refused container stays on disk.
dta_zip_extract <- function(path, entries, sizes, dest) {
  out <- character(length(entries))
  for (i in seq_along(entries)) {
    member <- entries[[i]]
    d <- file.path(dest, sprintf("e%05d", i))
    dir.create(d, recursive = TRUE, showWarnings = FALSE, mode = "0700")
    target <- file.path(d, dta_zip_entry_basename(member))
    junk <- !grepl("\\", member, fixed = TRUE)
    into <- if (junk) d else file.path(dest, sprintf("x%05d", i))
    dir.create(into, showWarnings = FALSE, mode = "0700")
    err <- tryCatch(
      {
        zip::unzip(path, files = member, exdir = into, junkpaths = junk, overwrite = FALSE)
        NULL
      },
      error = function(e) e
    )
    got <- list.files(into, recursive = TRUE, all.files = TRUE, full.names = TRUE)
    if (!junk) {
      if (is.null(err) && length(got) == 1) file.rename(got, target)
      unlink(into, recursive = TRUE)
    }
    ok <- is.null(err) && length(got) == 1 && file.exists(target) && isTRUE(file.size(target) == sizes[[i]])
    if (!ok) {
      unlink(dest, recursive = TRUE)
      shown <- encodeString(member)
      if (is.null(err)) {
        cli::cli_abort(
          "{.file {shown}} in {.file {basename(path)}} is damaged: it could not be extracted to the size the archive declares.",
          class = c("dta_container_corrupt", "dta_container_error"), path = path, member = member
        )
      }
      # zip's text names the entry and the archive, which the first line
      # already does: keep what follows the archive name. No `parent`: it would
      # print zip's text a second time, with the full path.
      reason <- dta_zip_reason(err, path)
      at <- regexpr(paste0("`", basename(path), "`: "), reason, fixed = TRUE)
      if (at > 0) reason <- substring(reason, at + attr(at, "match.length"))
      hint <- if (any(utf8ToInt(member) > 127, na.rm = TRUE)) {
        c("i" = "Its name is not stored as UTF-8 in the archive; re-create the archive with a tool that writes UTF-8 names.")
      }
      cli::cli_abort(
        c("{.file {shown}} in {.file {basename(path)}} could not be extracted.", "x" = "{reason}", hint),
        class = c("dta_container_corrupt", "dta_container_error"), path = path, member = member
      )
    }
    out[[i]] <- target
  }
  out
}

# ---- SAS transport ---------------------------------------------------------------
dta_gzip_file <- function(path) identical(readBin(path, "raw", 2L), as.raw(c(0x1f, 0x8b)))

# readBin() of up to `n` bytes. A gzip stream that was cut short usually just
# ends early, but zlib sometimes warns ("invalid or incomplete compressed
# data") or makes readBin() fail ("error reading from the connection"). Both
# are caught here and returned as NULL, so the callers report damage instead of
# letting an unclassed, translated condition escape.
dta_read_stream <- function(con, n) {
  damaged <- FALSE
  b <- withCallingHandlers(
    tryCatch(
      readBin(con, "raw", n),
      error = function(e) {
        damaged <<- TRUE
        raw(0)
      }
    ),
    warning = function(w) {
      damaged <<- TRUE
      invokeRestart("muffleWarning")
    }
  )
  if (damaged) NULL else b
}

# TRUE when the file holds a second gzip member: a gzip header (magic, deflate,
# no reserved flag bits, a known XFL and OS byte) anywhere after the first one.
dta_gzip_has_second_member <- function(path, chunk = 4194304) {
  magic <- as.raw(c(0x1f, 0x8b, 0x08))
  con <- file(path, "rb")
  on.exit(close(con))
  carry <- raw(0)
  consumed <- 0
  repeat {
    b <- readBin(con, "raw", chunk)
    buf <- c(carry, b)
    first <- consumed - length(carry) # file offset of buf[1]
    consumed <- consumed + length(b)
    for (h in grepRaw(magic, buf, fixed = TRUE, all = TRUE)) {
      # A header cut by the end of `buf` is whole in the next round (`carry`).
      if (first + h - 1 == 0 || h + 9 > length(buf)) next
      flg <- as.integer(buf[h + 3])
      xfl <- as.integer(buf[h + 8])
      os <- as.integer(buf[h + 9])
      if (flg < 32L && xfl %in% c(0L, 2L, 4L) && (os <= 13L || os == 255L)) {
        return(TRUE)
      }
    }
    if (length(b) < chunk) {
      return(FALSE)
    }
    carry <- buf[(length(buf) - 8):length(buf)]
  }
}

# Does the gzip trailer (the last four bytes: the inflated length modulo 2^32)
# agree with the `inflated` bytes the stream gave? A stream cut short ends in
# arbitrary compressed bytes and all but never agrees. A file of several gzip
# members (bgzip and `cat a.gz b.gz` write those) has only its last member's
# length there; when the two differ, a second member header in the file means
# the trailer says nothing about the whole, so the check passes: a cut in such
# a file is still caught by a zlib warning or a partial record, but not by this.
dta_gzip_trailer_ok <- function(path, inflated) {
  size <- file.size(path)
  if (is.na(size) || size < 18) {
    return(FALSE)
  }
  con <- file(path, "rb")
  seek(con, size - 4)
  trailer <- readBin(con, "raw", 4)
  close(con)
  dta_le_u32(trailer) == inflated %% 2^32 || dta_gzip_has_second_member(path)
}

dta_xpt_members <- function(path, limits = dta_container_limits(), chunk = `__DTAtools_container_chunk__`) {
  if (chunk %% 80 != 0 || chunk < 400) {
    cli::cli_abort("{.arg chunk} must be a multiple of 80 and at least 400.", .internal = TRUE)
  }
  corrupt <- function(member, why) {
    cli::cli_abort(
      c("{.file {basename(path)}} is damaged.", "x" = "{why}"),
      class = c("dta_container_corrupt", "dta_container_error"), path = path, member = member
    )
  }
  read_chunk <- function(n) {
    b <- dta_read_stream(con, n)
    if (is.null(b)) corrupt("compressed data", "The compressed file is truncated or damaged.")
    b
  }
  gz <- dta_gzip_file(path)
  con <- gzfile(path, "rb")
  on.exit(close(con))
  head <- read_chunk(240)
  if (length(head) >= 14 && identical(head[1:14], charToRaw("**COMPRESSED**"))) {
    cli::cli_abort(
      c(
        "{.file {basename(path)}} is not a SAS transport (XPORT) file.",
        "i" = "It is a SAS CPORT file, a different format that cannot be read here."
      ),
      class = c("dta_xpt_not_transport", "dta_container_error"), path = path
    )
  }
  lib5 <- charToRaw("HEADER RECORD*******LIBRARY HEADER RECORD!!!!!!!")
  lib8 <- charToRaw("HEADER RECORD*******LIBV8   HEADER RECORD!!!!!!!")
  version <- if (length(head) >= 48 && identical(head[1:48], lib5)) {
    5L
  } else if (length(head) >= 48 && identical(head[1:48], lib8)) {
    8L
  } else {
    NA_integer_
  }
  if (is.na(version) || length(head) < 240) {
    cli::cli_abort(
      "{.file {basename(path)}} is not a SAS transport (XPORT) file.",
      class = c("dta_xpt_not_transport", "dta_container_error"), path = path
    )
  }
  marker <- function(v5, v8) charToRaw(sprintf("HEADER RECORD*******%s HEADER RECORD!!!!!!!", if (version == 5L) v5 else v8))
  mem <- marker("MEMBER ", "MEMBV8 ")
  dsc <- marker("DSCRPTR", "DSCPTV8")
  nam <- marker("NAMESTR", "NAMSTV8")
  obs <- marker("OBS    ", "OBSV8  ")
  last <- if (version == 5L) 16L else 40L
  # A member is a member header record followed by a descriptor header (+80)
  # and a NAMESTR header (+320), and its observations are announced by an OBS
  # header record somewhere after its NAMESTR records (at +400 at the earliest).
  # `buf` carries the previous chunk's last five records, so a header whose
  # following records have not arrived yet is examined again next round. All
  # offsets stay multiples of 80.
  carry <- raw(0)
  base <- 240
  starts <- numeric()
  members <- character()
  awaiting <- FALSE # the latest member has not shown its OBS header yet
  repeat {
    b <- read_chunk(chunk)
    eof <- length(b) < chunk
    buf <- c(carry, b)
    buf_base <- base - length(carry)
    hits <- grepRaw(mem, buf, fixed = TRUE, all = TRUE)
    hits <- hits[(hits - 1) %% 80 == 0]
    # Where this round's OBS header records lie; looked for only when a member
    # is waiting for one, so the data of a large member is scanned once.
    obs_at <- NULL
    obs_positions <- function() {
      if (is.null(obs_at)) {
        hit <- grepRaw(obs, buf, fixed = TRUE, all = TRUE)
        obs_at <<- buf_base + hit[(hit - 1) %% 80 == 0] - 1
      }
      obs_at
    }
    defer <- Inf # where the first header this round could not judge yet lies
    for (h in hits) {
      off <- buf_base + h - 1
      if (off %in% starts) next
      if (h + 399 > length(buf)) {
        if (!eof) {
          defer <- min(defer, off)
          next
        }
        # The stream ends before the records a member needs. A following
        # record that exists and is not a descriptor header makes this data.
        if (h + 127 <= length(buf) && !identical(buf[(h + 80):(h + 127)], dsc)) next
        corrupt("member header", "The file ends inside a member header: it is truncated.")
      }
      if (!identical(buf[(h + 80):(h + 127)], dsc)) next
      if (!identical(buf[(h + 320):(h + 367)], nam)) next
      # Trailing blanks and NUL bytes are padding; a NUL inside the name is not.
      nm_raw <- buf[(h + 168):(h + 160 + last - 1)]
      pad <- nm_raw %in% as.raw(c(0x00, 0x20))
      nm_raw <- nm_raw[seq_len(if (all(pad)) 0L else max(which(!pad)))]
      nm <- if (any(nm_raw == as.raw(0))) NA_character_ else rawToChar(nm_raw)
      if (is.na(nm) || !validUTF8(nm) || !grepl("^[A-Za-z_][A-Za-z0-9_]{0,31}$", nm, useBytes = TRUE)) {
        cli::cli_abort(
          c(
            "{.file {basename(path)}} is refused: {cli::qty(1)}{?an entry/entries} would be unsafe to extract.",
            "x" = "A member name is not a SAS dataset name."
          ),
          class = c("dta_container_unsafe_member", "dta_container_error"),
          path = path, members = "<member name>", problems = "is not a SAS dataset name"
        )
      }
      # The member becomes a file <name>.xpt: a member named CON or COM1 would
      # name a device on Windows, so the zip entry rules apply to it too.
      bad <- dta_zip_entry_problem(paste0(tolower(nm), ".xpt"))
      if (!is.na(bad)) {
        cli::cli_abort(
          c(
            "{.file {basename(path)}} is refused: a member would be written under a name that some file systems (Windows) cannot write.",
            "x" = "Member {.val {nm}} {bad}."
          ),
          class = c("dta_container_unsafe_member", "dta_container_error"),
          path = path, members = nm, problems = bad
        )
      }
      # The previous member's OBS header must lie before this member's header.
      if (awaiting && !any(obs_positions() >= starts[length(starts)] + 400 & obs_positions() < off)) {
        corrupt(members[length(members)], "A member has no OBS header record: the file is truncated or damaged.")
      }
      awaiting <- TRUE
      starts <- c(starts, off)
      members <- c(members, nm)
      if (length(starts) > limits$max_members) {
        dta_container_too_large(path, "DTAtools.container_max_members", length(starts), limits$max_members)
      }
    }
    if (awaiting && any(obs_positions() >= starts[length(starts)] + 400 & obs_positions() < defer)) awaiting <- FALSE
    base <- base + length(b)
    # No ratio check here: one deflate stream cannot exceed about 1032:1, below
    # the ratio cap, so only the byte cap can stop a gzip stream.
    if (gz && base > limits$max_bytes) dta_container_too_large(path, "DTAtools.container_max_bytes", base, limits$max_bytes)
    if (eof) break
    carry <- buf[(length(buf) - 399):length(buf)]
  }
  if (gz && !dta_gzip_trailer_ok(path, base)) {
    corrupt("compressed data", "The compressed file is truncated or damaged: its length does not match its trailer.")
  }
  if (base %% 80 != 0) corrupt("last record", "The file ends inside an 80-byte record: it is truncated.")
  # What this cannot see: a V5 library cut inside a member's observations on a
  # record boundary. The rows carry no count, so a short run of them looks the
  # same as a complete one.
  if (awaiting) corrupt(members[length(members)], "A member has no OBS header record: the file is truncated or damaged.")
  o <- order(starts)
  starts <- starts[o]
  members <- members[o]
  key <- tolower(members)
  dup <- key %in% key[duplicated(key)]
  if (any(dup)) {
    dm <- members[dup]
    cli::cli_abort(
      "{.file {basename(path)}} is refused: {.val {dm}} name the same file once letter case is ignored.",
      class = c("dta_container_duplicate_member", "dta_container_error"), path = path, members = dm
    )
  }
  data.frame(
    member = members, name = paste0(key, ".xpt", recycle0 = TRUE), start = starts,
    end = if (length(starts)) c(starts[-1], base) else numeric(0),
    version = rep(version, length(starts)), stringsAsFactors = FALSE
  )
}

# `dest` is the library's private directory: on any failure all of it is
# deleted, so nothing of a refused library stays on disk.
dta_xpt_split <- function(path, members, dest, chunk = `__DTAtools_container_chunk__`) {
  dir.create(dest, recursive = TRUE, showWarnings = FALSE, mode = "0700")
  out <- file.path(dest, members$name)
  corrupt <- function(member, why = NULL) {
    unlink(dest, recursive = TRUE)
    cli::cli_abort(
      c(
        "{.file {basename(path)}} is damaged.",
        "x" = if (is.null(why)) "It ended before member {.val {member}} did." else "{why}"
      ),
      class = c("dta_container_corrupt", "dta_container_error"), path = path, member = member
    )
  }
  gz <- dta_gzip_file(path)
  con <- gzfile(path, "rb")
  on.exit(close(con))
  lib <- dta_read_stream(con, 240)
  if (length(lib) < 240) corrupt("library header", "It ended inside the library header.")
  pos <- 240
  for (k in order(members$start)) {
    if (file.exists(out[k])) {
      dm <- members$member[k]
      unlink(dest, recursive = TRUE)
      cli::cli_abort(
        "{.file {basename(path)}} is refused: {.val {dm}} name the same file once letter case is ignored.",
        class = c("dta_container_duplicate_member", "dta_container_error"), path = path, members = dm
      )
    }
    # A read that returns nothing (NULL after a zlib warning, raw(0) at the end
    # of the stream) means the stream ended before the member did.
    while (pos < members$start[k]) {
      b <- dta_read_stream(con, min(members$start[k] - pos, chunk))
      if (!length(b)) corrupt(members$member[k])
      pos <- pos + length(b)
    }
    oc <- file(out[k], "wb")
    writeBin(lib, oc)
    while (pos < members$end[k]) {
      b <- dta_read_stream(con, min(members$end[k] - pos, chunk))
      if (!length(b)) {
        close(oc)
        corrupt(members$member[k])
      }
      writeBin(b, oc)
      pos <- pos + length(b)
    }
    close(oc)
  }
  # When the last part ended the stream, its trailer must agree with the length.
  if (gz) {
    rest <- dta_read_stream(con, 1)
    if (is.null(rest)) corrupt("compressed data", "The compressed file is truncated or damaged.")
    if (!length(rest) && !dta_gzip_trailer_ok(path, pos)) {
      corrupt("compressed data", "The compressed file is truncated or damaged: its length does not match its trailer.")
    }
  }
  out
}
