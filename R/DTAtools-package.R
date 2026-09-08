#' @keywords internal
#'
#' @section The shape of a working session:
#' A Data Transfer Agreement (DTA) is a written contract about a data
#' delivery; a Data Transfer Specification (DTS) is the machine-readable part
#' of it. This package turns that specification into something a delivery can
#' be checked against, and turns it back into a document that can be signed.
#'
#' A typical session runs in four steps.
#'
#' \describe{
#'   \item{Read the specification}{\code{\link{read_dta_from_yaml}()} builds a
#'     \code{\link{DTA}} from a YAML file; \code{\link{dta_from_list}()} does
#'     the same from an already-parsed list. For a single dataset's columns,
#'     \code{\link{import_specs_from_yaml}()} and
#'     \code{\link{specs_from_list}()} build a
#'     \code{\link{DTAColumnSpecCollection}}.}
#'   \item{Bind the delivered files}{\code{\link{load_file}()} attaches a
#'     delivered file to the dataset that declares it. Its \code{stream}
#'     argument decides whether the file is read into memory or left on disk
#'     and scanned in batches, which is what lets a file larger than memory be
#'     validated at all.}
#'   \item{Validate}{\code{\link{check}()} evaluates every column
#'     specification and rule and records the verdict on the object. For a
#'     one-off check with no \code{DTA} around it, \code{\link{validate_table}()}
#'     takes a data frame and \code{\link{validate_file_stream}()} takes a path.}
#'   \item{Read the verdict, then report it}{\code{\link{validation_status}()}
#'     for the summary, \code{\link{results}()} and \code{\link{messages}()}
#'     for the detail, \code{\link{inspect}()} for one message at a time, and
#'     \code{\link{write_validation_report}()} for a standalone HTML report.}
#' }
#'
#' @section Producing the document:
#' \code{\link{write_dta}()} exports the agreement as DOCX, PDF or Markdown,
#' and \code{\link{write_dataset_metadata}()} does the same for one dataset.
#' \code{\link{export_with_template}()} fills a Word template the reader
#' authored instead of the built-in layout;
#' \code{\link{dta_template_placeholders}()} lists what such a template may
#' refer to. PDF output needs an external converter --
#' \code{\link{dta_pdf_backend}()} reports which one this machine will use.
#'
#' @section Classes:
#' A \code{\link{DTA}} holds \code{\link{DTAMetaData}} and a named list of
#' datasets. A dataset is either a \code{\link{DTADataSetTabular}}, whose files
#' are parsed and validated against a
#' \code{\link{DTAColumnSpecCollection}} of \code{\link{DTAColumnSpec}} objects
#' and a list of \code{\link{DTARule}} objects, or a
#' \code{\link{DTADataSetFile}}, whose files are only checked for arrival.
#' Each dataset declares its files through a \code{\link{DTAFile}} handler:
#' \code{\link{DTAFileCSV}}, \code{\link{DTAFileTSV}} or
#' \code{\link{DTAFileDelim}} for something that is read,
#' \code{\link{DTAFileAny}} for something that is not.
#'
#' @section Authoring and templates:
#' \code{\link{run_dta_app}()} launches a Shiny interface over all of the
#' above. \code{\link{create_template_repo}()} scaffolds a private template
#' repository, and \code{\link{validate_template}()} lints one in CI without
#' starting the app. See \code{vignette("DTAtools")} for a walkthrough and
#' \code{vignette("private-templates")} for the template system.
#'
#' @section Options:
#' \describe{
#'   \item{\code{DTAtools.stream}, \code{DTAtools.stream_threshold}}{The
#'     session default for \code{load_file(stream =)}, and the size in bytes
#'     above which \code{"auto"} keeps a file lazy (512 MB by default).}
#'   \item{\code{DTAtools.stream_block_size}}{Bytes per Arrow read block on a
#'     delimited file, 1 MiB by default. This, times Arrow's read-ahead, is
#'     what governs peak memory during a scan.}
#'   \item{\code{DTAtools.max_errors}}{How many per-cell error rows are held in
#'     memory, 10000 by default. Counts and the verdict are exact whatever it
#'     is set to; see \code{\link{collect_full_errors}()}.}
#'   \item{\code{DTAtools.use_arrow_compute},
#'     \code{DTAtools.arrow_min_rows}}{Opt in to Arrow's compute kernels for
#'     rule evaluation, and the table size below which the R path is used
#'     anyway. See \code{\link{set_dta_compute_threads}()}.}
#'   \item{\code{DTAtools.benchmark}}{Attach runtime and memory metrics to each
#'     validation; see \code{\link{validation_benchmark}()}.}
#' }
"_PACKAGE"
