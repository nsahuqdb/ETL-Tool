# =============================================================================
# code_version.R
#
# Code version is captured separately from config snapshots — they evolve
# at different rates. The pair (snapshot_id, code_sha) together forms the
# full reproducibility key for a run.
#
# Functions:
#   get_current_code_sha()        -> "abc1234..." or "uncommitted-<short>"
#   code_status()                 -> list(sha, dirty, branch, last_commit_at)
#   compare_code_to_snapshot(snapshot) -> warning if they differ
# =============================================================================


#' Return the current Git SHA of the working tree, or a synthetic value
#' if not in a Git repo or git isn't available.
#'
#' @param fail_silently if TRUE and git is unavailable, return NA_character_
get_current_code_sha <- function(fail_silently = TRUE) {
  if (Sys.which("git") == "") {
    if (fail_silently) return(NA_character_)
    stop("git not found in PATH")
  }
  sha <- tryCatch(
    suppressWarnings(system2("git", c("rev-parse", "HEAD"),
                              stdout = TRUE, stderr = FALSE)),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  if (is.null(sha) || length(sha) == 0 || !nzchar(sha[1])) {
    if (fail_silently) return(NA_character_)
    stop("git rev-parse HEAD failed")
  }
  trimws(sha[1])
}


#' Return current Git status: SHA, dirty flag, branch name, last commit time.
code_status <- function() {
  if (Sys.which("git") == "") {
    return(list(sha = NA_character_, dirty = NA, branch = NA_character_,
                last_commit_at = NA_character_, available = FALSE))
  }
  safe_git <- function(args) {
    tryCatch(
      suppressWarnings(system2("git", args, stdout = TRUE, stderr = FALSE)),
      error = function(e) character(),
      warning = function(w) character()
    )
  }
  sha <- safe_git(c("rev-parse", "HEAD"))
  branch <- safe_git(c("rev-parse", "--abbrev-ref", "HEAD"))
  status_lines <- safe_git(c("status", "--porcelain"))
  last_at <- safe_git(c("log", "-1", "--format=%cI"))
  list(
    sha            = if (length(sha) > 0)    trimws(sha[1])    else NA_character_,
    dirty          = length(status_lines) > 0,
    branch         = if (length(branch) > 0) trimws(branch[1]) else NA_character_,
    last_commit_at = if (length(last_at) > 0) trimws(last_at[1]) else NA_character_,
    available      = TRUE
  )
}


#' Check whether the current code SHA matches the snapshot's recorded SHA.
#' Emits a warning if they differ. Used at the start of run_etl() so a
#' user running an old snapshot with newer code knows what they're doing.
compare_code_to_snapshot <- function(snapshot_meta, verbose = TRUE) {
  if (is.null(snapshot_meta) || is.null(snapshot_meta$code_sha_at_creation)) {
    return(invisible(list(match = NA, current = NA_character_,
                           snapshot = NA_character_)))
  }
  current <- get_current_code_sha()
  recorded <- snapshot_meta$code_sha_at_creation
  if (is.na(current) || is.na(recorded)) {
    return(invisible(list(match = NA, current = current, snapshot = recorded)))
  }
  match <- identical(current, recorded)
  if (!match && verbose) {
    warning(sprintf(
      "[code-version] snapshot '%s' was created with code SHA %s; ",
      snapshot_meta$label %||% "<unlabeled>",
      substr(recorded, 1, 12)),
      sprintf("current code SHA is %s. ", substr(current, 1, 12)),
      "Output may differ from the snapshot's expected results.",
      call. = FALSE)
  }
  invisible(list(match = match, current = current, snapshot = recorded))
}
