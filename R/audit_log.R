# =============================================================================
# audit_log.R
#
# Append-only structured audit log. Every meaningful event in the system —
# snapshot creation/promotion, override edit, run start/finish, validation
# warning acknowledgement, sign-off — gets one JSON line in the log.
#
# Design choices:
#   - One JSON object per line (jsonl). Easy to grep / tail / pipe to
#     downstream log aggregation.
#   - Append-only: never edit existing lines. If something needs to be
#     "undone", that's a new event referencing the original.
#   - Timestamps in ISO 8601 with timezone offset.
#   - Default location: logs/etl_audit.jsonl. Configurable via
#     options(ifrs9.audit_log = "/path/to/file"). Test runs can point this
#     at a temp file.
#
# Event schema (minimum fields):
#   ts             ISO 8601 timestamp (auto-set)
#   event          short event type, e.g. "snapshot_create", "override_edit"
#   user           username (auto-detected from Sys.info()[["user"]] if absent)
#   <event-specific fields>
#
# Public API:
#   audit_event(payload)       append one event
#   read_audit_log()           tibble of all events
#   audit_log_path()           current file path
# =============================================================================


#' Path to the active audit log. Configurable via options or env var.
audit_log_path <- function() {
  p <- getOption("ifrs9.audit_log",
                 default = Sys.getenv("IFRS9_AUDIT_LOG", "logs/etl_audit.jsonl"))
  p
}


#' Append one event to the audit log. Auto-fills ts and user if absent.
#'
#' @param payload  named list with at least an `event` field
#' @return invisibly returns the path written to
audit_event <- function(payload) {
  if (!is.list(payload) || is.null(payload$event)) {
    stop("audit_event: payload must be a list with an `event` field")
  }
  if (is.null(payload$ts)) {
    payload$ts <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  }
  if (is.null(payload$user)) {
    payload$user <- Sys.info()[["user"]] %||% "unknown"
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    # Soft-fail: write a plain text representation if jsonlite is missing
    line <- paste(names(payload),
                  vapply(payload, function(v) as.character(v)[1], character(1)),
                  sep = "=", collapse = " ")
  } else {
    line <- jsonlite::toJSON(payload, auto_unbox = TRUE, na = "null")
  }
  path <- audit_log_path()
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  con <- file(path, open = "a", encoding = "UTF-8")
  on.exit(close(con))
  writeLines(line, con = con)
  invisible(path)
}


#' Read the audit log into a tibble. Each row is one event.
read_audit_log <- function(path = audit_log_path()) {
  if (!file.exists(path)) {
    return(tibble::tibble())
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite is required to read the JSON-line audit log")
  }
  lines <- readLines(path, warn = FALSE)
  lines <- lines[nzchar(lines)]
  if (length(lines) == 0) return(tibble::tibble())
  rows <- lapply(lines, function(l) {
    tryCatch(jsonlite::fromJSON(l, simplifyVector = TRUE),
             error = function(e) list(event = "<parse_error>",
                                       raw = l, error = conditionMessage(e)))
  })
  # Convert list-of-lists to a tibble with all keys union-ed
  all_keys <- unique(unlist(lapply(rows, names)))
  cols <- lapply(all_keys, function(k) {
    vapply(rows, function(r) {
      v <- r[[k]]
      if (is.null(v)) NA_character_ else as.character(v)[1]
    }, character(1))
  })
  names(cols) <- all_keys
  tibble::as_tibble(cols)
}
