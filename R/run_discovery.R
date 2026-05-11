# =============================================================================
# run_discovery.R
#
# Read-only helpers used by the Shiny app to enumerate past runs and
# retrieve their metadata. The pipeline writes runs to disk; the Shiny
# app reads them back. No coupling between the two beyond the on-disk
# layout.
#
# Expected layout (with keep_history=TRUE):
#   <runs_dir>/
#     2026-05-06_10-30-17/
#       Output/                  # 18 CSVs
#       reports/
#         manifest.json
#         validation.csv         (and validation.md)
#         reconciliation.md      (if reference_outputs configured)
#         mismatches/            (per-file diff CSVs)
#         run_summary.md
#     2026-05-06_14-12-04/
#     ...
#
# Public API:
#   runs_dir_default()                — resolve the runs directory
#   list_runs(runs_dir)               — tibble of run metadata
#   read_run_manifest(run_path)       — list of manifest contents
#   read_run_validation(run_path)     — tibble or NULL
#   read_run_reconciliation(run_path) — list(verdict_path, mismatches_dir)
#   list_run_outputs(run_path)        — character vector of CSV paths
# =============================================================================


#' Resolve the active runs directory using the same precedence as run_etl().
#'
#' Order:
#'   1. argument
#'   2. options("ifrs9.runs_dir")
#'   3. environment IFRS9_RUNS_DIR
#'   4. "runs"  (relative to working directory)
runs_dir_default <- function(runs_dir = NULL) {
  runs_dir %||%
    getOption("ifrs9.runs_dir",
              default = Sys.getenv("IFRS9_RUNS_DIR", "runs"))
}


#' List all runs found under the runs directory, newest-first.
#'
#' Each row corresponds to one run subdirectory containing a manifest.json.
#' Subdirectories without a manifest are skipped silently — the app never
#' shows broken half-runs.
#'
#' @return tibble with columns:
#'   run_id, path, started_at, finished_at, duration_seconds,
#'   user, code_sha, snapshot_label, snapshot_status,
#'   n_outputs, n_validation_failures, has_reconciliation
list_runs <- function(runs_dir = NULL) {
  runs_dir <- runs_dir_default(runs_dir)
  empty <- tibble::tibble(
    run_id = character(), path = character(),
    started_at = character(), finished_at = character(),
    duration_seconds = numeric(),
    user = character(), code_sha = character(),
    snapshot_label = character(), snapshot_status = character(),
    n_outputs = integer(),
    n_validation_failures = integer(),
    has_reconciliation = logical()
  )
  if (!dir.exists(runs_dir)) return(empty)

  subs <- list.dirs(runs_dir, recursive = FALSE, full.names = TRUE)
  rows <- list()
  for (s in subs) {
    mp <- file.path(s, "reports", "manifest.json")
    if (!file.exists(mp)) next
    m <- tryCatch(jsonlite::fromJSON(mp, simplifyVector = TRUE),
                   error = function(e) NULL)
    if (is.null(m)) next

    n_outputs <- length(m$outputs %||% list())
    n_fail <- 0L
    val_csv <- file.path(s, "reports", "validation.csv")
    if (file.exists(val_csv)) {
      v <- tryCatch(utils::read.csv(val_csv, stringsAsFactors = FALSE),
                     error = function(e) NULL)
      if (!is.null(v) && nrow(v) > 0 && "passed" %in% names(v)) {
        n_fail <- sum(!as.logical(v$passed))
      }
    }
    has_recon <- file.exists(file.path(s, "reports", "reconciliation.md"))

    rows[[length(rows) + 1]] <- tibble::tibble(
      run_id           = m$run$run_id %||% basename(s),
      path             = s,
      started_at       = m$run$started_at %||% NA_character_,
      finished_at      = m$run$finished_at %||% NA_character_,
      duration_seconds = as.numeric(m$run$duration_seconds %||% NA),
      user             = m$run$user %||% NA_character_,
      code_sha         = m$run$code_sha %||% NA_character_,
      snapshot_label   = m$snapshot$label %||% NA_character_,
      snapshot_status  = m$snapshot$status %||% NA_character_,
      n_outputs        = as.integer(n_outputs),
      n_validation_failures = as.integer(n_fail),
      has_reconciliation = has_recon
    )
  }
  if (length(rows) == 0) return(empty)
  out <- do.call(rbind, rows)
  # Newest-first
  out[order(out$started_at, decreasing = TRUE), ]
}


#' Read a run's full manifest.json.
read_run_manifest <- function(run_path) {
  mp <- file.path(run_path, "reports", "manifest.json")
  if (!file.exists(mp)) return(NULL)
  tryCatch(jsonlite::fromJSON(mp, simplifyVector = TRUE),
           error = function(e) NULL)
}


#' Read a run's validation.csv into a tibble. Returns NULL if missing.
read_run_validation <- function(run_path) {
  vp <- file.path(run_path, "reports", "validation.csv")
  if (!file.exists(vp)) return(NULL)
  tryCatch(tibble::as_tibble(utils::read.csv(vp, stringsAsFactors = FALSE,
                                              na.strings = "")),
           error = function(e) NULL)
}


#' Read recon paths for a run.
#' Returns a list with fields:
#'   md_path: path to reconciliation.md (or NULL)
#'   mismatches_dir: path to per-file diff CSVs (or NULL)
#'   mismatches: tibble(file, path, size_bytes) for each mismatch CSV
read_run_reconciliation <- function(run_path) {
  md   <- file.path(run_path, "reports", "reconciliation.md")
  miss <- file.path(run_path, "reports", "mismatches")
  out <- list(
    md_path        = if (file.exists(md))   md   else NULL,
    mismatches_dir = if (dir.exists(miss))  miss else NULL,
    mismatches     = tibble::tibble(file = character(),
                                     path = character(),
                                     size_bytes = numeric())
  )
  if (dir.exists(miss)) {
    files <- list.files(miss, pattern = "\\.csv$", full.names = TRUE)
    if (length(files) > 0) {
      out$mismatches <- tibble::tibble(
        file       = basename(files),
        path       = files,
        size_bytes = as.numeric(file.info(files)$size)
      )
    }
  }
  out
}


#' List output CSVs for a run.
list_run_outputs <- function(run_path) {
  d <- file.path(run_path, "Output")
  if (!dir.exists(d)) return(character())
  list.files(d, pattern = "\\.csv$", full.names = TRUE)
}
