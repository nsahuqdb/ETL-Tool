# ============================================================================
# R/validation.R
#
# Validation framework. Used by all phases (input, transform, derived output,
# reconciliation) to run consistent checks and produce a structured report.
#
# A validator is created with `make_validator()` and consists of:
#   id          — short stable string, e.g. "INPUT_AccountMaster_present"
#   severity    — "ERROR" | "WARN" | "INFO"
#   description — human-readable one-liner
#   fn          — function that returns list(passed = logical(1),
#                                            details = list-or-NULL)
#
# Suites are flat lists of validators. Run a suite with `run_validation_suite()`
# which returns a results tibble (one row per validator) and prints a summary.
#
# Conventions:
#   - ERROR  -> the run cannot continue (or the result is not trustworthy)
#   - WARN   -> the run can continue but a human should look
#   - INFO   -> diagnostic / observational; never blocks
# ============================================================================


#' Create a validator object.
#'
#' @param id          Short stable identifier (e.g.
#'                    "INPUT_AccountMaster_present"). MUST be stable across
#'                    code releases — IDs are referenced from
#'                    validation_suppressions.yml and from audit logs.
#' @param severity    "ERROR", "WARN", or "INFO".
#' @param description One-line description shown next to the [PASS|WARN|FAIL]
#'                    tag in console output and the validation report.
#' @param fn          Function (...) -> list(passed, details). The arguments
#'                    are passed in by run_validation_suite().
#' @param context     Optional grouping label, e.g. "AccountMaster".
#' @param rationale   Optional longer explanation of why this check matters.
#'                    Shown in the Shiny detail pane when a row is expanded.
#'                    Blank by default.
#' @param remediation Optional one-liner telling the user what to do when this
#'                    check fires (e.g. "Re-export AccountMaster.xlsx with
#'                    the missing column" or "Add a customer_id override
#'                    via the Overrides page"). Shown next to the finding
#'                    in Shiny.
#' @param tags        Character vector of tags (free-form). Two reserved tags:
#'                      "pre_run" — eligible for pre-run quick-check (input
#'                                  stage only)
#'                      "blocker" — even WARN-severity findings of this kind
#'                                  should not be silently passed; surface
#'                                  prominently in UI even if user has set
#'                                  on_validation_error=warn.
#' @param suppressible logical(1) — whether this validator can be suppressed
#'                    via validation_suppressions.yml. Default TRUE. Set
#'                    FALSE for invariants that should never be silenced
#'                    (e.g. "input file is present", "schema is valid").
make_validator <- function(id, severity, description, fn,
                            context = NA_character_,
                            rationale = "",
                            remediation = "",
                            tags = character(),
                            suppressible = TRUE) {
  stopifnot(
    is.character(id), length(id) == 1,
    severity %in% c("ERROR", "WARN", "INFO"),
    is.character(description), length(description) == 1,
    is.function(fn),
    is.character(rationale), length(rationale) <= 1,
    is.character(remediation), length(remediation) <= 1,
    is.character(tags),
    is.logical(suppressible), length(suppressible) == 1
  )
  structure(list(
    id           = id,
    severity     = severity,
    description  = description,
    fn           = fn,
    context      = context,
    rationale    = if (length(rationale) == 0) "" else rationale,
    remediation  = if (length(remediation) == 0) "" else remediation,
    tags         = tags,
    suppressible = suppressible
  ), class = "ifrs9_validator")
}


#' Run a single validator against args.
#'
#' If `fn` raises an error, it's caught and reported as failed with the
#' Run a single validator against args. Errors are caught and turned into a
#' failed result.
#'
#' @param v       Validator object.
#' @param args    Named list of args passed to v$fn.
#' @param suppressions character vector of validator IDs marked suppressed
#'                in the active suppressions file. A suppressed failure is
#'                still recorded but `effective_severity` is downgraded to
#'                "INFO" (so gating doesn't trip on it).
.run_one <- function(v, args, suppressions = character()) {
  res <- tryCatch(
    do.call(v$fn, args),
    error = function(e) {
      list(passed = FALSE,
           details = list(error = conditionMessage(e)))
    }
  )
  if (!is.list(res) || !"passed" %in% names(res)) {
    res <- list(passed = FALSE,
                details = list(error = "validator returned non-standard result"))
  }
  passed <- isTRUE(res$passed)
  details <- if (!passed) res$details else NULL

  is_suppressed <- isTRUE(v$suppressible) && (v$id %in% suppressions)
  effective_severity <- if (!passed && is_suppressed) "INFO" else v$severity

  tibble::tibble(
    id                  = v$id,
    severity            = v$severity,
    effective_severity  = effective_severity,
    context             = v$context %||% NA_character_,
    description         = v$description,
    rationale           = v$rationale %||% "",
    remediation         = v$remediation %||% "",
    tags                = list(v$tags %||% character()),
    suppressible        = isTRUE(v$suppressible),
    suppressed          = is_suppressed,
    passed              = passed,
    details             = list(details)
  )
}


#' Run a list of validators against shared args.
#'
#' @param suite_name Display name (e.g. "INPUT").
#' @param validators List of validator objects.
#' @param args       Named list passed to each validator's fn via do.call.
#' @param verbose    Print one line per validator as it runs.
#' @param suppressions character vector of validator IDs to suppress (their
#'                     failures are still recorded but with
#'                     effective_severity = "INFO" so gating doesn't trip).
#' @return tibble of results (one row per validator).
run_validation_suite <- function(suite_name, validators, args = list(),
                                  verbose = TRUE,
                                  suppressions = character()) {
  empty_result <- tibble::tibble(
    id = character(), severity = character(),
    effective_severity = character(),
    context = character(), description = character(),
    rationale = character(), remediation = character(),
    tags = list(), suppressible = logical(),
    suppressed = logical(),
    passed = logical(), details = list()
  )

  if (length(validators) == 0) {
    if (verbose) cat(sprintf("\n  [%s] no validators registered\n", suite_name))
    return(empty_result)
  }

  if (verbose) {
    cat(sprintf("\n========== VALIDATION :: %s (%d checks) ==========\n",
                suite_name, length(validators)))
  }

  results <- lapply(validators, .run_one, args = args, suppressions = suppressions)
  results <- do.call(rbind, results)

  if (verbose) {
    for (i in seq_len(nrow(results))) {
      r <- results[i, ]
      if (r$passed) {
        status <- "PASS"
      } else if (isTRUE(r$suppressed)) {
        # Failed but explicitly suppressed by user
        status <- "SUPPR"
      } else {
        status <- if (r$severity == "ERROR") {
          "FAIL"
        } else if (r$severity == "WARN") {
          "WARN"
        } else {
          "INFO"
        }
      }
      cat(sprintf("  [%-5s] %-50s  %s\n",
                  status,
                  paste0(r$id, if (!is.na(r$context)) sprintf(" (%s)", r$context) else ""),
                  r$description))
      # If failed, surface the first detail message
      if (!r$passed && !is.null(r$details[[1]])) {
        d <- r$details[[1]]
        if (!is.null(d$error)) {
          cat(sprintf("           -> error: %s\n", d$error))
        } else if (!is.null(d$message)) {
          cat(sprintf("           -> %s\n", d$message))
        } else {
          # show first line of stringified details
          s <- paste(capture.output(str(d, max.level = 1, give.attr = FALSE)),
                      collapse = " | ")
          if (nchar(s) > 150) s <- paste0(substr(s, 1, 150), "...")
          cat(sprintf("           -> %s\n", s))
        }
      }
    }

    # Counts use effective_severity so suppressed failures show as INFO.
    n_pass <- sum(results$passed)
    n_total <- nrow(results)
    n_suppr <- sum(!results$passed & results$suppressed)
    n_err <- sum(!results$passed & !results$suppressed &
                   results$effective_severity == "ERROR")
    n_warn <- sum(!results$passed & !results$suppressed &
                    results$effective_severity == "WARN")
    n_info <- sum(!results$passed & !results$suppressed &
                    results$effective_severity == "INFO")
    cat(sprintf("\n  %s summary: %d/%d passed  |  %d ERROR, %d WARN, %d INFO failures, %d suppressed\n",
                suite_name, n_pass, n_total, n_err, n_warn, n_info, n_suppr))
  }

  results
}


#' Combine results from multiple suites into one tibble.
combine_validation_results <- function(...) {
  parts <- list(...)
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (length(parts) == 0) {
    return(tibble::tibble(id = character(), severity = character(),
                          context = character(), description = character(),
                          passed = logical(), details = list()))
  }
  do.call(rbind, parts)
}


#' Did the suite pass at the requested severity? Default: no ERROR failures.
suite_passed <- function(results, max_severity = "ERROR") {
  if (nrow(results) == 0) return(TRUE)
  rank <- c("INFO" = 1, "WARN" = 2, "ERROR" = 3)
  threshold <- rank[[max_severity]]
  bad <- !results$passed & rank[results$severity] >= threshold
  !any(bad)
}


#' Decide whether a validation failure should halt the pipeline.
#'
#' Behaviour driven by run_cfg$run$on_validation_error (default "stop"):
#'   "stop"   — any ERROR-level failure raises stop(); WARN/INFO continue
#'   "warn"   — any failure emits a warning; never halts
#'   "ignore" — silent (still recorded in the result tibble)
#'
#' This is what a CLI / batch run uses. A future Shiny gate would intercept
#' the result tibble before this is called and let the user acknowledge
#' WARN-level findings before continuing.
.gate_on_validation <- function(results, stage, run_cfg, log_msg = NULL,
                                  run_id = NULL) {
  policy <- (run_cfg$run$on_validation_error %||% "stop")
  log_msg <- log_msg %||% function(...) {}

  if (nrow(results) == 0) return(invisible(TRUE))

  # Suppressed failures had effective_severity downgraded in .run_one.
  # Gate on effective_severity so a user-suppressed ERROR doesn't halt
  # the run while still being recorded in the report.
  active <- !results$passed & !results$suppressed
  n_err  <- sum(active & results$effective_severity == "ERROR")
  n_warn <- sum(active & results$effective_severity == "WARN")
  n_info <- sum(active & results$effective_severity == "INFO")
  n_suppr <- sum(!results$passed & results$suppressed)

  log_msg("[run_etl] %s validation: %d ERROR, %d WARN, %d INFO, %d suppressed",
          stage, n_err, n_warn, n_info, n_suppr)

  # Audit: log the numeric summary (full report goes to validation.csv)
  if (exists("audit_event", mode = "function")) {
    audit_event(list(
      event        = "validation_summary",
      run_id       = run_id %||% NA_character_,
      stage        = stage,
      n_total      = nrow(results),
      n_pass       = sum(results$passed),
      n_error      = n_err,
      n_warn       = n_warn,
      n_info       = n_info,
      n_suppressed = n_suppr,
      policy       = policy
    ))
  }

  if (n_err > 0) {
    failed_ids <- results$id[active & results$effective_severity == "ERROR"]
    msg <- sprintf("[%s] %d ERROR-level validation failure(s): %s",
                   stage, n_err,
                   paste(head(failed_ids, 5), collapse = ", "))
    if (policy == "stop") {
      stop(msg, call. = FALSE)
    } else if (policy == "warn") {
      warning(msg, call. = FALSE)
    }
  }
  invisible(TRUE)
}


#' Write a structured validation report (Markdown).
#' Groups by stage -> severity, lists each finding with id + description +
#' detail message.
write_validation_report <- function(results, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  if (nrow(results) == 0) {
    writeLines(c("# Validation Report", "", "(no validators were run)"), path)
    return(invisible(path))
  }

  rank <- c("ERROR" = 3, "WARN" = 2, "INFO" = 1, "PASS" = 0)

  active <- !results$passed & !(results$suppressed %||% rep(FALSE, nrow(results)))
  n_err  <- sum(active & (results$effective_severity %||% results$severity) == "ERROR")
  n_warn <- sum(active & (results$effective_severity %||% results$severity) == "WARN")
  n_info <- sum(active & (results$effective_severity %||% results$severity) == "INFO")
  n_suppr <- sum(!results$passed & (results$suppressed %||% rep(FALSE, nrow(results))))

  lines <- c(
    "# Validation Report",
    sprintf("Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    sprintf("Total checks: %d", nrow(results)),
    sprintf("Passed: %d  |  ERROR: %d  |  WARN: %d  |  INFO: %d  |  Suppressed: %d",
            sum(results$passed), n_err, n_warn, n_info, n_suppr),
    ""
  )

  for (stg in unique(results$stage %||% rep("(no stage)", nrow(results)))) {
    rs <- results[results$stage == stg, ]
    lines <- c(lines, sprintf("## %s (%d checks)", stg, nrow(rs)), "")

    failed <- rs[!rs$passed, ]
    if (nrow(failed) == 0) {
      lines <- c(lines, "- All checks passed.", "")
      next
    }
    failed <- failed[order(-rank[failed$severity], failed$id), ]
    for (i in seq_len(nrow(failed))) {
      r <- failed[i, ]
      d <- r$details[[1]]
      msg <- if (is.null(d)) {
        ""
      } else if (!is.null(d$message)) {
        d$message
      } else if (!is.null(d$error)) {
        paste0("error: ", d$error)
      } else {
        paste(capture.output(str(d, max.level = 1, give.attr = FALSE)),
              collapse = " | ")
      }
      sev_tag <- if (isTRUE(r$suppressed)) {
        sprintf("%s — SUPPRESSED", r$severity)
      } else {
        as.character(r$severity)
      }
      header <- sprintf("- **[%s]** `%s` %s%s",
                        sev_tag, r$id,
                        if (!is.na(r$context)) sprintf("(%s) ", r$context) else "",
                        r$description)
      block <- header
      if (nzchar(msg)) block <- c(block, sprintf("  - %s", msg))
      rat <- r$rationale %||% ""
      if (nzchar(rat))   block <- c(block, sprintf("  - _Rationale:_ %s", rat))
      rem <- r$remediation %||% ""
      if (nzchar(rem))   block <- c(block, sprintf("  - _Remediation:_ %s", rem))
      lines <- c(lines, block)
    }
    lines <- c(lines, "")
  }

  writeLines(lines, path)
  invisible(path)
}


#' Write a flat CSV of all validation results (one row per check).
#' Useful for Shiny tables and machine post-processing.
write_validation_csv <- function(results, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  if (nrow(results) == 0) {
    utils::write.csv(
      data.frame(stage = character(), id = character(),
                  severity = character(),
                  effective_severity = character(),
                  context = character(),
                  description = character(),
                  rationale = character(),
                  remediation = character(),
                  suppressed = logical(),
                  passed = logical(),
                  message = character()),
      path, row.names = FALSE)
    return(invisible(path))
  }
  msg <- vapply(results$details, function(d) {
    if (is.null(d)) return("")
    if (!is.null(d$message)) return(as.character(d$message))
    if (!is.null(d$error))   return(paste0("error: ", as.character(d$error)))
    s <- paste(capture.output(str(d, max.level = 1, give.attr = FALSE)),
                collapse = " | ")
    if (nchar(s) > 500) s <- paste0(substr(s, 1, 500), "...")
    s
  }, character(1))
  out <- data.frame(
    stage              = results$stage %||% NA_character_,
    id                 = results$id,
    severity           = results$severity,
    effective_severity = results$effective_severity %||% results$severity,
    context            = results$context,
    description        = results$description,
    rationale          = results$rationale %||% "",
    remediation        = results$remediation %||% "",
    suppressed         = results$suppressed %||% FALSE,
    passed             = results$passed,
    message            = msg,
    stringsAsFactors = FALSE
  )
  utils::write.csv(out, path, row.names = FALSE, na = "")
  invisible(path)
}


# Provide a NULL-coalesce operator if not already defined
if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x
}
