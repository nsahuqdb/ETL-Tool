# ============================================================================
# R/validate_inputs.R
#
# Lightweight validation of the inputs list returned by read_all_inputs().
#
# Deliberately kept minimal — heavier business validation (e.g. customer
# IDs reconciling across files, amounts being non-negative, etc.) belongs
# in the transformation layer in Phase D. What we check HERE is just:
#
#   1. Every expected file is present and readable
#   2. Every tibble has at least one row
#   3. Every tibble has at least one column
#   4. The EXTRACTDA column (where present) has consistent values
#
# Returns a tibble of issues. Empty tibble = all good.
# ============================================================================


#' Validate a result from read_all_inputs().
#'
#' @param inputs Named list from read_all_inputs()
#' @return Tibble with columns (file, severity, message). Empty if all OK.
validate_inputs <- function(inputs) {
  issues <- list()

  add_issue <- function(file, severity, message) {
    issues[[length(issues) + 1L]] <<- tibble::tibble(
      file = file, severity = severity, message = message
    )
  }

  for (name in names(INPUT_FILE_SPECS)) {
    df <- inputs[[name]]

    # 1. presence
    if (is.null(df)) {
      add_issue(name, "ERROR", "missing or unreadable")
      next
    }

    # 2. shape
    if (nrow(df) == 0) {
      add_issue(name, "WARNING", "0 rows read")
    }
    if (ncol(df) == 0) {
      add_issue(name, "ERROR", "0 columns read")
      next
    }

    # 3. EXTRACTDA / EXTRACTDATE consistency
    extract_col <- intersect(c("EXTRACTDA", "EXTRACTDATE", "ExtractDate"), colnames(df))
    if (length(extract_col) == 1L) {
      vals <- unique(df[[extract_col]])
      vals <- vals[!is.na(vals) & nzchar(as.character(vals))]
      if (length(vals) > 1L) {
        add_issue(name, "WARNING",
                  sprintf("%s column has %d distinct values: %s",
                          extract_col, length(vals),
                          paste(utils::head(vals, 5), collapse = " | ")))
      }
    }
  }

  if (length(issues) == 0L) {
    return(tibble::tibble(
      file = character(), severity = character(), message = character()
    ))
  }
  dplyr::bind_rows(issues)
}


#' Check a specific list of column names exists in a tibble.
#' Helper for downstream phases (not used in Phase B itself).
#'
#' @param df Tibble to check
#' @param required Character vector of required column names
#' @return Character vector of MISSING column names (empty if all present)
check_required_columns <- function(df, required) {
  setdiff(required, colnames(df))
}
