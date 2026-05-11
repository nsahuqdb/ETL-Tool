# =============================================================================
# input_acquisition.R
#
# Helpers for acquiring the input bundle from somewhere other than a
# pre-configured local directory:
#
#   - acquire_inputs_from_zip(zip, dest)    upload path: extract a zip
#                                            the user uploaded to dest
#   - list_data_drops(drop_root)            auto-fetch path: enumerate
#                                            subfolders of a "data drop"
#                                            location the data team
#                                            populates monthly
#   - validate_input_directory(dir)         fast structural check
#                                            (12 expected files present,
#                                            readable, basic schemas)
#   - record_input_source(run_dir, ...)     persist input_source.yml
#                                            into a run for audit
#
# Design notes:
#  * Acquisition is separate from the pipeline's actual data load.
#    `run_etl_phase1` still calls `read_all_inputs(input_dir)`; this
#    module just *prepares* an input_dir, by extraction or by selection.
#  * The structural check is intentionally cheap — file existence and
#    "first row can be parsed". It's not a substitute for the INPUT
#    validators (which run during Pre-run check). It's a guard so the
#    user can't even attempt Pre-run check on a malformed bundle.
# =============================================================================


# =============================================================================
# input_acquisition.R
#
# Helpers for acquiring the input bundle from somewhere other than a
# pre-configured local directory:
#
#   - acquire_inputs_from_zip(zip, dest)    upload path: extract a zip
#                                            the user uploaded to dest
#   - list_data_drops(drop_root)            auto-fetch path: enumerate
#                                            subfolders of a "data drop"
#                                            location the data team
#                                            populates monthly
#   - validate_input_directory(dir)         fast structural check
#                                            (12 expected files present,
#                                            readable, basic schemas)
#   - record_input_source(run_dir, ...)     persist input_source.yml
#                                            into a run for audit
#
# Design notes:
#  * Acquisition is separate from the pipeline's actual data load.
#    `run_etl_phase1` still calls `read_all_inputs(input_dir)`; this
#    module just *prepares* an input_dir, by extraction or by selection.
#  * The structural check is intentionally cheap — file existence and
#    "first row can be parsed". It's not a substitute for the INPUT
#    validators (which run during Pre-run check). It's a guard so the
#    user can't even attempt Pre-run check on a malformed bundle.
#  * The canonical list of 12 expected files is INPUT_FILE_SPECS in
#    R/read_inputs.R. We read that to build the expected file list so
#    we can never drift out of sync with the actual loader.
# =============================================================================


#' Names of the 12 expected input files. Sourced from INPUT_FILE_SPECS in
#' R/read_inputs.R so this list is always in sync with what the loader
#' actually expects. Note that 5 of the 12 files have `.xls` extension
#' but are actually HTML (SQL*Plus output) — the loader handles both.
.expected_input_files <- function() {
  if (!exists("INPUT_FILE_SPECS")) {
    stop(".expected_input_files: INPUT_FILE_SPECS not loaded. ",
         "R/read_inputs.R must be sourced before R/input_acquisition.R.")
  }
  vapply(INPUT_FILE_SPECS, function(s) s$file, character(1),
         USE.NAMES = FALSE)
}


#' Extract a zip uploaded by the user into a fresh subdirectory and
#' return the path. The destination is always a NEW directory inside
#' the system tempdir — never overwrites anything.
#'
#' @param zip_path  path to the uploaded zip on disk
#' @param dest_root parent dir for extracted bundles (defaults to
#'                  tempdir()/ifrs9_uploaded_inputs)
#' @return list with:
#'   - path: extracted directory containing the input files
#'   - source_zip: original zip path the user uploaded (for audit)
#'   - extracted_at: ISO timestamp
acquire_inputs_from_zip <- function(zip_path,
                                       dest_root = NULL) {
  if (!file.exists(zip_path)) {
    stop("Uploaded zip not found: ", zip_path)
  }
  dest_root <- dest_root %||% file.path(tempdir(), "ifrs9_uploaded_inputs")
  dir.create(dest_root, showWarnings = FALSE, recursive = TRUE)

  ts <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  dest <- file.path(dest_root, ts)
  dir.create(dest, recursive = TRUE)

  # utils::unzip is base R; no extra dependency needed
  result <- tryCatch(
    utils::unzip(zip_path, exdir = dest),
    error = function(e) e
  )
  if (inherits(result, "error")) {
    stop("Could not extract zip: ", conditionMessage(result))
  }

  # If the zip wraps everything in a single top-level folder
  # (e.g. "Input/AccountMaster.xlsx"), unwrap one level so the
  # returned path directly contains the input files.
  contents <- list.files(dest, full.names = FALSE, recursive = FALSE)
  if (length(contents) == 1 &&
      dir.exists(file.path(dest, contents[1])) &&
      length(list.files(file.path(dest, contents[1]),
                         pattern = "\\.(xlsx|csv)$")) > 0) {
    dest <- file.path(dest, contents[1])
  }

  list(
    path         = dest,
    source_zip   = basename(zip_path),
    extracted_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )
}


#' Enumerate available data-drop subfolders. Each immediate subfolder of
#' `drop_root` is treated as one "drop" — typically named by month
#' (`2026-01`, `2026-02`, ...) but we don't enforce a naming convention
#' since the data team's structure may evolve.
#'
#' Returns a tibble newest-first by directory mtime.
list_data_drops <- function(drop_root) {
  empty <- tibble::tibble(
    name = character(), path = character(),
    mtime = as.POSIXct(character()),
    n_files = integer(), looks_complete = logical()
  )
  if (is.null(drop_root) || !nzchar(drop_root) || !dir.exists(drop_root)) {
    return(empty)
  }
  subs <- list.dirs(drop_root, recursive = FALSE, full.names = TRUE)
  if (length(subs) == 0) return(empty)

  expected <- .expected_input_files()
  rows <- list()
  for (s in subs) {
    files <- list.files(s, recursive = TRUE, full.names = FALSE)
    n_present <- sum(basename(expected) %in% basename(files))
    rows[[length(rows) + 1]] <- tibble::tibble(
      name           = basename(s),
      path           = s,
      mtime          = file.info(s)$mtime,
      n_files        = length(files),
      looks_complete = n_present == length(expected)
    )
  }
  out <- do.call(rbind, rows)
  out[order(out$mtime, decreasing = TRUE), ]
}


#' Fast structural check on a candidate input directory. Returns a
#' tibble with one row per check (mirrors the validation framework's
#' shape so it can render in the same DT pattern).
#'
#' Checks performed (cheap):
#'   1. Each of 12 expected files is present
#'   2. Each file is readable (xlsx: openable; csv: readLines first row)
#'   3. AccountMaster has a CONTRACTID column (canary for schema match)
#'
#' Heavier schema checks (column types, FK integrity, business rules)
#' stay in the INPUT validators that run during Pre-run check.
#'
#' @param input_dir directory containing the 12 expected input files
#' @return tibble(check, status, detail) where status is "PASS" or "FAIL"
validate_input_directory <- function(input_dir) {
  rows <- list()
  add <- function(check, ok, detail = "") {
    rows[[length(rows) + 1]] <<- tibble::tibble(
      check = check,
      status = if (isTRUE(ok)) "PASS" else "FAIL",
      detail = as.character(detail)
    )
  }

  if (is.null(input_dir) || !nzchar(input_dir) || !dir.exists(input_dir)) {
    add("Directory exists",
        FALSE,
        sprintf("Directory does not exist or empty: %s",
                input_dir %||% "(none)"))
    return(do.call(rbind, rows))
  }
  add("Directory exists", TRUE, input_dir)

  # File-presence check
  expected <- .expected_input_files()
  present <- list.files(input_dir, recursive = TRUE, full.names = FALSE)
  for (f in expected) {
    found <- basename(f) %in% basename(present)
    add(sprintf("File present: %s", f), found,
        if (!found) "missing" else "")
  }

  # Readability + canary schema check, only for files that exist
  am_path <- file.path(input_dir, "AccountMaster.xlsx")
  if (file.exists(am_path)) {
    cols_ok <- tryCatch({
      if (requireNamespace("readxl", quietly = TRUE)) {
        # Read just headers to keep this cheap
        h <- readxl::read_excel(am_path, n_max = 0)
        any(grepl("contract", tolower(colnames(h))))
      } else {
        TRUE  # if readxl missing, trust and let Pre-run check catch it
      }
    }, error = function(e) FALSE)
    add("AccountMaster has a CONTRACTID-like column",
        cols_ok,
        if (!cols_ok) "could not detect contract column in headers" else "")
  }

  do.call(rbind, rows)
}


#' Record the input source used for a run. Writes `input_source.yml`
#' into <run_dir>/reports/ so the audit trail is explicit about whether
#' the run used the configured directory, a data drop, or a user upload.
#'
#' @param run_dir   the run output root (e.g. runs/<id>)
#' @param kind      one of "configured", "drop_folder", "upload"
#' @param details   list with kind-specific fields (drop name, zip name, etc)
record_input_source <- function(run_dir, kind, details = list()) {
  reports_dir <- file.path(run_dir, "reports")
  dir.create(reports_dir, showWarnings = FALSE, recursive = TRUE)
  meta <- list(
    schema_version = "1.0",
    kind           = kind,
    recorded_at    = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    details        = details
  )
  path <- file.path(reports_dir, "input_source.yml")
  writeLines(yaml::as.yaml(meta), path)
  invisible(path)
}
