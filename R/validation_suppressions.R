# =============================================================================
# validation_suppressions.R
#
# Per-snapshot list of validator IDs the user has explicitly accepted as
# OK. A suppressed finding still RUNS and is recorded; it just gets
# effective_severity downgraded to "INFO" so gating doesn't halt the run.
#
# This is the audit-friendly answer to "we know this validator is firing
# and it's fine for our environment". Each suppression carries a reason
# and an approver. Future runs of the same snapshot honour the
# suppression; new snapshots start with an empty list (suppressions do
# not carry forward).
#
# File format (config_snapshots/<label>/config/validation_suppressions.yml,
# OR config/validation_suppressions.yml when running off live config):
#
#     schema_version: "1.0"
#     suppressions:
#       - validator_id: INPUT_AccountMaster_dpd_nonneg
#         reason: "Two contracts have legacy negative DPD values from a
#                  data-entry error in 2019. Already in the deferred-fix
#                  ticket #1234. Acceptable for Q1 close."
#         approved_by: priya
#         approved_at: 2026-04-15T10:00:00+0300
#         valid_until: 2026-12-31      # optional; auto-expires past this date
#
# Public API:
#   load_suppressions(path)      -> tibble of effective suppressions
#   active_suppression_ids(t)    -> character vector of validator IDs
#                                    (after expiry filtering)
#   add_suppression(...)         -> append a row, write atomically
#   remove_suppression(id, ...)  -> mark superseded; never delete
# =============================================================================


#' Load the suppressions file. Returns an empty tibble if the file doesn't
#' exist or has no entries (this is the common case — suppressions are
#' rare).
#'
#' @param path path to validation_suppressions.yml.
load_suppressions <- function(path) {
  empty <- tibble::tibble(
    validator_id = character(),
    reason       = character(),
    approved_by  = character(),
    approved_at  = character(),
    valid_until  = character()
  )
  if (is.null(path) || !file.exists(path)) return(empty)
  raw <- tryCatch(yaml::read_yaml(path), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$suppressions) ||
      length(raw$suppressions) == 0) {
    return(empty)
  }
  rows <- lapply(raw$suppressions, function(s) {
    tibble::tibble(
      validator_id = as.character(s$validator_id %||% NA_character_),
      reason       = as.character(s$reason       %||% ""),
      approved_by  = as.character(s$approved_by  %||% ""),
      approved_at  = as.character(s$approved_at  %||% ""),
      valid_until  = as.character(s$valid_until  %||% "")
    )
  })
  do.call(rbind, rows)
}


#' Filter a suppressions tibble down to the validator IDs that are
#' CURRENTLY active (i.e. not expired).
#'
#' @param suppressions tibble from load_suppressions()
#' @param as_of        Date — the run date. Defaults to today.
#' @return character vector of validator IDs.
active_suppression_ids <- function(suppressions, as_of = Sys.Date()) {
  if (is.null(suppressions) || nrow(suppressions) == 0) return(character())
  vu <- suppressions$valid_until
  parsed <- suppressWarnings(as.Date(vu))
  active <- (is.na(parsed) | parsed >= as_of)
  unique(suppressions$validator_id[active &
                                     !is.na(suppressions$validator_id) &
                                     nzchar(suppressions$validator_id)])
}


#' Append a new suppression to the file (creates the file if needed).
#' Writes atomically. The audit log gets a "suppression_add" event.
#'
#' @param path          file path
#' @param validator_id  validator ID to suppress
#' @param reason        free-text justification (required)
#' @param approved_by   username of approver (required)
#' @param valid_until   optional date "YYYY-MM-DD"; NULL = no expiry
add_suppression <- function(path, validator_id, reason, approved_by,
                             valid_until = NULL) {
  if (!nzchar(validator_id)) stop("validator_id required")
  if (!nzchar(reason))       stop("reason required (audit trail)")
  if (!nzchar(approved_by))  stop("approved_by required (audit trail)")

  existing <- if (file.exists(path)) yaml::read_yaml(path) else
                list(schema_version = "1.0", suppressions = list())
  if (is.null(existing$suppressions)) existing$suppressions <- list()

  new_entry <- list(
    validator_id = validator_id,
    reason       = reason,
    approved_by  = approved_by,
    approved_at  = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )
  if (!is.null(valid_until) && nzchar(valid_until)) {
    new_entry$valid_until <- as.character(valid_until)
  }
  existing$suppressions[[length(existing$suppressions) + 1]] <- new_entry

  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  tmp <- paste0(path, ".tmp")
  writeLines(yaml::as.yaml(existing), tmp)
  file.rename(tmp, path)

  if (exists("audit_event", mode = "function")) {
    audit_event(list(
      event        = "suppression_add",
      validator_id = validator_id,
      reason       = reason,
      approved_by  = approved_by,
      valid_until  = valid_until %||% NA_character_
    ))
  }
  invisible(path)
}
