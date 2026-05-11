# =============================================================================
# run_approval.R
#
# Run-level approval workflow. After H12, a run lands in
# pending_approval status when phase 2 finishes. A reviewer must
# explicitly approve or reject it before it's "official".
#
# Run status is stored at <run_dir>/reports/run_status.yml. Each
# transition is also recorded in the audit log.
#
# Approval is run-scoped (not override-scoped). The approver effectively
# accepts the entire run including any overrides applied mid-run.
#
# Functions:
#   read_run_status(run_path)
#   list_runs_pending_approval(runs_dir)
#   approve_run(run_path, approver, reason)
#   reject_run(run_path, approver, reason)
# =============================================================================


RUN_STATUSES <- c("running", "pending_checker", "approved", "rejected",
                   # Legacy alias kept for older runs:
                   "pending_approval")


read_run_status <- function(run_path) {
  p <- file.path(run_path, "reports", "run_status.yml")
  if (!file.exists(p)) return(NULL)
  tryCatch(yaml::read_yaml(p), error = function(e) NULL)
}


#' List runs that are currently awaiting checker approval, OR runs
#' that lack a run_status.yml entirely (treated as `unknown` so they
#' surface for diagnosis instead of being silently filtered out).
#'
#' Accepts both the new "pending_checker" status and the legacy
#' "pending_approval" status (older runs).
list_runs_pending_approval <- function(runs_dir = NULL) {
  all_runs <- list_runs(runs_dir)
  if (nrow(all_runs) == 0) return(all_runs)
  status <- vapply(all_runs$path, function(p) {
    s <- read_run_status(p)
    if (is.null(s)) "unknown" else (s$status %||% "unknown")
  }, character(1))
  all_runs$status <- status
  pending_states <- c("pending_checker", "pending_approval", "unknown")
  all_runs[status %in% pending_states, , drop = FALSE]
}


#' List runs whose approval has been decided (approved or rejected).
#' Returns one row per run with denormalised requester/decider info pulled
#' from the run_status.yml transitions:
#'   - status                : "approved" | "rejected"
#'   - requested_by          : user who created the run (first transition)
#'   - requested_at          : when the run was submitted (first transition)
#'   - request_comment       : reason on the first transition
#'   - decided_by            : approver/rejecter
#'   - decided_at
#'   - decision_comment      : the approve/reject reason
#'   - n_overrides_total     : sum across the three override files
list_runs_decided <- function(runs_dir = NULL) {
  all_runs <- list_runs(runs_dir)
  empty <- all_runs[FALSE, , drop = FALSE]
  empty$status <- character()
  empty$requested_by <- character()
  empty$requested_at <- character()
  empty$request_comment <- character()
  empty$decided_by <- character()
  empty$decided_at <- character()
  empty$decision_comment <- character()
  empty$n_overrides_total <- integer()
  if (nrow(all_runs) == 0) return(empty)

  out <- list()
  for (i in seq_len(nrow(all_runs))) {
    p <- all_runs$path[i]
    s <- read_run_status(p)
    if (is.null(s)) next
    cur <- s$status %||% "unknown"
    if (!cur %in% c("approved", "rejected")) next

    trans <- s$transitions %||% list()
    if (length(trans) == 0) next

    # First transition is the request (to pending_approval); the LAST
    # transition with to == cur is the deciding transition.
    first <- trans[[1]]
    decided <- NULL
    for (t in rev(trans)) {
      if (!is.null(t$to) && t$to == cur) { decided <- t; break }
    }
    if (is.null(decided)) decided <- list(at = NA, by = NA, reason = NA)

    n_ov <- 0L
    ov_dir <- file.path(p, "overrides")
    if (dir.exists(ov_dir)) {
      for (f in list.files(ov_dir, "\\.csv$", full.names = TRUE)) {
        df <- tryCatch(utils::read.csv(f, stringsAsFactors = FALSE),
                        error = function(e) NULL)
        if (!is.null(df)) n_ov <- n_ov + nrow(df)
      }
    }

    row <- all_runs[i, , drop = FALSE]
    row$status            <- cur
    row$requested_by      <- first$by      %||% NA_character_
    row$requested_at      <- first$at      %||% NA_character_
    row$request_comment   <- first$reason  %||% NA_character_
    row$decided_by        <- decided$by    %||% NA_character_
    row$decided_at        <- decided$at    %||% NA_character_
    row$decision_comment  <- decided$reason %||% NA_character_
    row$n_overrides_total <- n_ov
    out[[length(out) + 1]] <- row
  }
  if (length(out) == 0) return(empty)
  res <- do.call(rbind, out)
  # Most-recent decision first
  res[order(res$decided_at, decreasing = TRUE), ]
}


#' Add status to a list_runs() result. Used by the Runs page to surface
#' the approval state in the main table.
annotate_runs_with_status <- function(runs_tbl) {
  if (nrow(runs_tbl) == 0) {
    runs_tbl$status <- character()
    return(runs_tbl)
  }
  runs_tbl$status <- vapply(runs_tbl$path, function(p) {
    s <- read_run_status(p)
    if (is.null(s)) "unknown" else (s$status %||% "unknown")
  }, character(1))
  runs_tbl
}


# =============================================================================
# Approval state machine — 2-stage maker / checker
#
# States:
#   pending_checker    : run is finished and waiting for a checker to
#                         approve. (For backward compat, "pending_approval"
#                         from older runs is treated as "pending_checker".)
#   approved           : checker has signed off. Run is exportable.
#   rejected           : checker has rejected. Terminal — fix and re-run.
#
# The "maker" (the user who ran the pipeline) is implicit; no separate
# Submit step. The maker is read from manifest.json's `user` field at
# transition time so we can enforce separation of duties.
#
# Separation of duties (config: approval.enforce_separation_of_duties):
#   When TRUE, refuses to approve a run if the approver is the same
#   user as the maker. Reject is allowed regardless (rejecting your
#   own work is fine — that's just discarding a candidate).
#
# audit_event names retained for compatibility:
#   run_approved  - whenever a run reaches `approved`
#   run_rejected  - whenever a run reaches `rejected`
# =============================================================================


# Treat legacy "pending_approval" as the new "pending_checker".
.normalize_status <- function(s) {
  if (is.null(s)) return("unknown")
  if (s == "pending_approval") return("pending_checker")
  s
}


# Reads the run's MAKER (the user who ran the pipeline) from manifest.json.
# Used to enforce separation of duties. Returns NA_character_ if unknown.
.maker_for_run <- function(run_path) {
  manifest <- file.path(run_path, "manifest.json")
  if (!file.exists(manifest)) return(NA_character_)
  m <- tryCatch(jsonlite::fromJSON(manifest), error = function(e) NULL)
  if (is.null(m)) return(NA_character_)
  m$user %||% m$run$user %||% NA_character_
}


# Reads the project's approval config block (the live config.yml's
# `approval:` block). Used at transition time to decide whether to
# enforce separation of duties.
.approval_cfg <- function() {
  cfg_path <- getOption("ifrs9.config_path",
                          file.path(getOption("ifrs9.project_root", getwd()),
                                    "config.yml"))
  if (!file.exists(cfg_path)) {
    return(list(enforce_separation_of_duties = FALSE))
  }
  cfg <- tryCatch(yaml::read_yaml(cfg_path), error = function(e) NULL)
  if (is.null(cfg) || is.null(cfg$approval)) {
    return(list(enforce_separation_of_duties = FALSE))
  }
  list(
    enforce_separation_of_duties =
      isTRUE(cfg$approval$enforce_separation_of_duties)
  )
}


.transition_run <- function(run_path, target, by, reason) {
  if (!target %in% c("approved", "rejected")) {
    stop("transition target must be 'approved' or 'rejected'")
  }
  if (is.null(reason) || !nzchar(reason)) {
    stop("reason is required when approving or rejecting a run")
  }
  meta <- read_run_status(run_path)
  if (is.null(meta)) {
    stop("run_status.yml not found for: ", run_path)
  }
  current <- .normalize_status(meta$status %||% "unknown")
  if (current != "pending_checker") {
    stop(sprintf("Cannot transition run from status '%s'; only ",
                  meta$status %||% "unknown"),
         "pending_checker runs are eligible.")
  }

  # Separation of duties: refuse approval if maker == approver and the
  # config flag is on. Reject is always allowed regardless.
  if (target == "approved") {
    cfg <- .approval_cfg()
    if (isTRUE(cfg$enforce_separation_of_duties)) {
      maker <- .maker_for_run(run_path)
      if (!is.na(maker) && nzchar(maker) &&
          identical(tolower(maker), tolower(as.character(by %||% "")))) {
        stop(sprintf(
          "Separation of duties is enforced: user '%s' ran this pipeline ",
          by),
          "and cannot also approve it. A different user must approve.")
      }
    }
  }

  meta$status <- target
  trans <- list(
    at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    by = by, to = target, reason = reason
  )
  meta$transitions <- c(meta$transitions, list(trans))

  # Atomic write
  p <- file.path(run_path, "reports", "run_status.yml")
  tmp <- paste0(p, ".tmp")
  writeLines(yaml::as.yaml(meta), tmp)
  file.rename(tmp, p)

  audit_event(list(
    event   = if (target == "approved") "run_approved" else "run_rejected",
    run_id  = meta$run_id,
    user    = by,
    reason  = reason
  ))

  invisible(meta)
}


approve_run <- function(run_path, approver, reason) {
  .transition_run(run_path, "approved", approver, reason)
}


reject_run <- function(run_path, approver, reason) {
  .transition_run(run_path, "rejected", approver, reason)
}


# =============================================================================
# Snapshot-level approval list helpers
#
# Snapshots have their own lifecycle managed in R/snapshots.R via
# promote_snapshot(). These helpers surface them in the unified Approval
# queue alongside run-level approvals.
#
#   list_snapshots_pending()   - status %in% c("pending_final", "pending")
#   list_snapshots_decided()   - status %in% c("approved", "archived")
#                                ("rejected" snapshots go back to draft, not
#                                 a separate status, so they don't appear in
#                                 history; we surface "archived" as the
#                                 historical decided state)
#
# Returns enriched tibbles with the same audit-trail shape used for run
# approvals (requested_by, requested_at, request_comment, decided_by,
# decided_at, decision_comment), reconstructed from snapshot.yml.
# =============================================================================


#' Snapshot pending list. Same idea as list_runs_pending_approval but for
#' snapshot promotions.
list_snapshots_pending <- function(snapshots_root = NULL) {
  snapshots_root <- snapshots_root %||%
    getOption("ifrs9.snapshots_dir",
              file.path(getOption("ifrs9.project_root", getwd()),
                        "config_snapshots"))
  s <- list_snapshots(snapshots_root)
  if (nrow(s) == 0) return(s)
  s[!is.na(s$status) & s$status %in% c("pending_final", "pending"),
    , drop = FALSE]
}


#' Snapshot history (approved + archived). Pulls the approval-trail
#' fields directly from each snapshot.yml since list_snapshots() doesn't
#' surface them.
list_snapshots_decided <- function(snapshots_root = NULL) {
  snapshots_root <- snapshots_root %||%
    getOption("ifrs9.snapshots_dir",
              file.path(getOption("ifrs9.project_root", getwd()),
                        "config_snapshots"))

  empty <- tibble::tibble(
    label             = character(), status = character(),
    description       = character(),
    requested_by      = character(), requested_at     = character(),
    request_comment   = character(),
    decided_by        = character(), decided_at       = character(),
    decision_comment  = character(),
    code_sha          = character()
  )
  if (!dir.exists(snapshots_root)) return(empty)

  labels <- list.dirs(snapshots_root, recursive = FALSE, full.names = FALSE)
  out <- list()
  for (lbl in labels) {
    meta <- tryCatch(read_snapshot_metadata(lbl, snapshots_root),
                     error = function(e) NULL)
    if (is.null(meta)) next
    cur <- meta$status %||% NA_character_
    if (!isTRUE(cur %in% c("approved", "archived"))) next

    out[[length(out) + 1]] <- tibble::tibble(
      label             = meta$label %||% lbl,
      status            = cur,
      description       = meta$description %||% NA_character_,
      requested_by      = meta$created_by %||% NA_character_,
      requested_at      = meta$created_at %||% NA_character_,
      request_comment   = meta$description %||% NA_character_,
      decided_by        = meta$approved_by %||% NA_character_,
      decided_at        = meta$approved_at %||% NA_character_,
      decision_comment  = meta$approval_reason %||% NA_character_,
      code_sha          = meta$code_sha_at_creation %||% NA_character_
    )
  }
  if (length(out) == 0) return(empty)
  res <- do.call(rbind, out)
  res[order(res$decided_at, decreasing = TRUE), ]
}
