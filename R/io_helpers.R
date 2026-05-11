# ============================================================================
# R/io_helpers.R
#
# Low-level I/O helpers for the IFRS9 ETL.
#
# Why this file exists:
#   The IFRSIN folder contains a mix of file formats. Some files have a .xlsx
#   extension and ARE proper Office Open XML, but some have a .xls extension
#   and are actually HTML tables exported by Oracle SQL*Plus (charset
#   WINDOWS-1256). We must dispatch on file content, not on extension.
#
# Public functions:
#   detect_input_format(path) -> one of c("xlsx","xls_binary","html","csv","unknown")
#   read_input_file(path)     -> tibble (column names preserved as-is)
# ============================================================================


#' Detect the format of an input file based on its leading bytes / first chars.
#'
#' @param path Path to the file
#' @return Single string: "xlsx", "xls_binary", "html", "csv", or "unknown"
detect_input_format <- function(path) {
  if (!file.exists(path)) {
    stop("File not found: ", path)
  }
  con <- file(path, "rb")
  on.exit(close(con))
  bytes <- readBin(con, "raw", n = 64)
  if (length(bytes) == 0) return("unknown")

  hex_prefix <- paste0(format(as.hexmode(as.integer(bytes)), width = 2), collapse = "")

  # Office Open XML / xlsx / xlsm  (ZIP magic: 50 4B 03 04)
  if (startsWith(hex_prefix, "504b0304")) return("xlsx")

  # Compound File Binary / legacy .xls  (D0 CF 11 E0 A1 B1 1A E1)
  if (startsWith(hex_prefix, "d0cf11e0a1b11ae1")) return("xls_binary")

  # HTML-as-xls (Oracle SQL*Plus, etc.)
  txt <- tryCatch(
    rawToChar(bytes[bytes != as.raw(0)]),
    error = function(e) ""
  )
  txt_lower <- tolower(trimws(txt))
  if (startsWith(txt_lower, "<!doctype") ||
      startsWith(txt_lower, "<html")     ||
      startsWith(txt_lower, "<?xml")) {
    return("html")
  }

  # If we got here and bytes are all printable ASCII / common UTF-8, assume CSV
  if (all(bytes == as.raw(9) | bytes == as.raw(10) |
          bytes == as.raw(13) | bytes >= as.raw(32))) {
    return("csv")
  }

  "unknown"
}


#' Read a single tabular input file, auto-detecting format.
#'
#' Handles:
#'   - xlsx / xlsm  (proper Office Open XML)               -> readxl::read_xlsx
#'   - .xls binary  (legacy Compound File Binary)          -> readxl::read_xls
#'   - .xls HTML    (Oracle SQL*Plus exports)              -> read_html_table
#'   - .csv         (plain text)                            -> readr::read_csv
#'
#' Column names are preserved as found in the source file. Any cleanup or
#' standardisation happens later in the transformation step (Phase D).
#'
#' @param path  Path to the file
#' @param sheet Sheet name or index (xlsx/xls only). Default = 1.
#' @return A tibble.
read_input_file <- function(path, sheet = 1) {
  fmt <- detect_input_format(path)

  raw <- switch(
    fmt,
    xlsx       = readxl::read_xlsx(path, sheet = sheet, .name_repair = "unique_quiet"),
    xls_binary = readxl::read_xls (path, sheet = sheet, .name_repair = "unique_quiet"),
    html       = read_html_table(path),
    csv        = readr::read_csv(path, show_col_types = FALSE,
                                 name_repair = "unique_quiet"),
    stop(sprintf("Cannot read file (format=%s): %s", fmt, path))
  )

  # Strip a duplicate header row when the first data row is identical
  # (case- and whitespace-insensitive) to the column names. This is a
  # common artifact in Excel exports that have a frozen header pane and
  # in Oracle SQL*Plus HTML exports that wrap headers in <thead> AND
  # repeat them as the first <tr> of <tbody>. Without this, the schema
  # apply step would coerce the duplicate row to NA and the row count
  # would be off by one.
  raw <- .strip_duplicate_header_row(raw, file_label = basename(path))

  raw
}


# Drop the first row if it's a verbatim repeat of the column names.
# The match is case-insensitive and ignores leading/trailing whitespace.
# A row is only stripped if EVERY non-NA column matches its header AND
# at least 50% of columns are non-NA (so a sparsely-populated row with
# accidental matches doesn't trigger the strip).
.strip_duplicate_header_row <- function(df, file_label = "<unknown>") {
  if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) return(df)
  hdr <- tolower(trimws(as.character(colnames(df))))
  first_row_chr <- vapply(df[1, , drop = FALSE], function(x) {
    if (length(x) == 0) NA_character_ else as.character(x[[1]])
  }, character(1))
  first_norm <- tolower(trimws(first_row_chr))
  non_na <- !is.na(first_norm) & nzchar(first_norm)
  if (sum(non_na) < ceiling(ncol(df) / 2)) return(df)
  if (all(first_norm[non_na] == hdr[non_na])) {
    message(sprintf(
      "[read_input_file] Stripped duplicate header row from %s",
      file_label))
    return(df[-1, , drop = FALSE])
  }
  df
}


#' Read an HTML-disguised-as-xls file as a tibble.
#'
#' Oracle SQL*Plus exports a single <table> wrapped in <html>. We pick the
#' first/largest table on the page. Encoding is forced to WINDOWS-1256
#' which matches the meta tag in the SQL*Plus exports we have. If no table
#' is found, an error is raised.
#'
#' @param path Path to the .xls (HTML) file
#' @return A tibble
read_html_table <- function(path) {
  doc <- xml2::read_html(path, encoding = "WINDOWS-1256")
  tables <- rvest::html_table(doc, header = TRUE, trim = TRUE, fill = TRUE)
  if (length(tables) == 0) {
    stop("No HTML table found in: ", path)
  }
  # If multiple tables, pick the one with the most rows
  if (length(tables) > 1) {
    sizes <- vapply(tables, nrow, integer(1))
    tables <- tables[which.max(sizes)]
  }
  tibble::as_tibble(tables[[1]], .name_repair = "unique_quiet")
}
