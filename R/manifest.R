# =============================================================================
# manifest.R
#
# Per-run manifest for audit. Writes a single JSON file capturing:
#
#   - run timestamp + duration
#   - config snapshot (model_config.yml, run_config.yml as nested objects)
#   - input file hashes + row counts (so re-runs can detect input changes)
#   - override file hashes (rating, stage, restructuring) + active row count
#   - output file paths + row counts + content hashes
#   - any warnings/errors collected during the run
#
# The manifest is the single source of truth for "what produced this output
# directory". Two runs with byte-identical manifests should produce
# byte-identical outputs (modulo timestamps).
# =============================================================================


#' Compute SHA-256 of a file's contents (hex string).
#' Returns NA if the file doesn't exist.
.file_hash <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  if (requireNamespace("digest", quietly = TRUE)) {
    digest::digest(file = path, algo = "sha256")
  } else {
    # Lightweight fallback: use file size + mtime if digest isn't installed.
    # Not cryptographic but adequate for change detection.
    info <- file.info(path)
    sprintf("size=%d;mtime=%s", info$size, format(info$mtime, "%Y-%m-%dT%H:%M:%S"))
  }
}


#' Count rows in a CSV without loading the whole file.
.csv_rows <- function(path) {
  if (!file.exists(path)) return(NA_integer_)
  con <- file(path, "r")
  on.exit(close(con))
  n <- 0L
  repeat {
    chunk <- readLines(con, n = 10000L, warn = FALSE)
    if (length(chunk) == 0) break
    n <- n + length(chunk)
  }
  # Subtract 1 for header (assumes well-formed CSV)
  max(0L, n - 1L)
}


#' Build the input-files block.
.manifest_inputs <- function(input_dir) {
  out <- list()
  for (key in names(INPUT_FILE_SPECS)) {
    path <- file.path(input_dir, INPUT_FILE_SPECS[[key]]$file)
    out[[key]] <- list(
      file       = INPUT_FILE_SPECS[[key]]$file,
      exists     = file.exists(path),
      size_bytes = if (file.exists(path)) file.info(path)$size else NA_integer_,
      sha256     = .file_hash(path)
    )
  }
  out
}


#' Build the override-files block.
.manifest_overrides <- function(static_dir = "data-raw/static") {
  out <- list()
  for (key in OVERRIDE_FILES) {
    spec <- STATIC_FILE_SPECS[[key]]
    path <- file.path(static_dir, spec$file)
    n_rows <- if (file.exists(path)) {
      tryCatch(.csv_rows(path), error = function(e) NA_integer_)
    } else NA_integer_
    out[[key]] <- list(
      file       = spec$file,
      exists     = file.exists(path),
      n_rows     = n_rows,
      sha256     = .file_hash(path)
    )
  }
  out
}


#' Build the outputs block — paths, row counts, hashes for every CSV in
#' the output directory.
.manifest_outputs <- function(output_dir) {
  if (!dir.exists(output_dir)) return(list())
  csvs <- list.files(output_dir, pattern = "\\.csv$", full.names = FALSE)
  out <- list()
  for (f in csvs) {
    p <- file.path(output_dir, f)
    out[[f]] <- list(
      n_rows = .csv_rows(p),
      size_bytes = file.info(p)$size,
      sha256 = .file_hash(p)
    )
  }
  out
}


#' Write a per-run manifest JSON file.
#'
#' @param run_dir       directory where output CSVs were written
#' @param input_dir     directory where input files live
#' @param run_cfg       loaded run config
#' @param model_cfg     loaded model config
#' @param run_started   POSIXct when the run started
#' @param run_finished  POSIXct when the run finished (defaults to Sys.time())
#' @param messages      character vector of any warnings/notes from the run
#' @param static_dir    static reference dir (for override hashing)
#' @param snapshot_meta optional snapshot metadata (from
#'                       read_snapshot_metadata). Recorded in manifest.
#' @param code_sha      optional code SHA at run time. Recorded in manifest.
#' @param run_id        optional run identifier (timestamp string). Recorded
#'                       in manifest. Used by the Shiny app to refer to the
#'                       run across files / audit log.
#' @return invisibly returns the manifest path
write_manifest <- function(run_dir, input_dir, run_cfg, model_cfg,
                            run_started,
                            run_finished = Sys.time(),
                            messages = character(),
                            static_dir = "data-raw/static",
                            snapshot_meta = NULL,
                            code_sha = NULL,
                            run_id = NULL) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("write_manifest requires the 'jsonlite' package")
  }

  manifest <- list(
    schema_version = "1.0",
    run = list(
      run_id      = run_id %||% NA,
      started_at  = format(run_started, "%Y-%m-%dT%H:%M:%S%z"),
      finished_at = format(run_finished, "%Y-%m-%dT%H:%M:%S%z"),
      duration_seconds = as.numeric(
        difftime(run_finished, run_started, units = "secs")),
      r_version   = R.version.string,
      hostname    = Sys.info()[["nodename"]],
      user        = Sys.info()[["user"]],
      code_sha    = code_sha %||% NA
    ),
    snapshot = if (!is.null(snapshot_meta)) {
                  list(
                    label                = snapshot_meta$label,
                    status               = snapshot_meta$status,
                    created_at           = snapshot_meta$created_at,
                    created_by           = snapshot_meta$created_by,
                    parent               = snapshot_meta$parent,
                    code_sha_at_creation = snapshot_meta$code_sha_at_creation
                  )
                } else NULL,
    config = list(
      model_config = model_cfg,
      run_config   = run_cfg
    ),
    inputs    = .manifest_inputs(input_dir),
    overrides = .manifest_overrides(static_dir),
    outputs   = .manifest_outputs(file.path(run_dir, "Output")),
    messages  = messages
  )

  reports_dir <- file.path(run_dir, "reports")
  dir.create(reports_dir, showWarnings = FALSE, recursive = TRUE)
  manifest_path <- file.path(reports_dir, "manifest.json")

  writeLines(
    jsonlite::toJSON(manifest, auto_unbox = TRUE, pretty = TRUE,
                     na = "string", null = "null"),
    manifest_path
  )

  invisible(manifest_path)
}
