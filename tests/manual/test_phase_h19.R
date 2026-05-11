# ============================================================================
# tests/manual/test_phase_h19.R
#
# Phase H19 — full pipeline check + reconciliation against the V4 Excel
# reference outputs. Run from the project root:
#
#     setwd("path/to/ifrs9_etl")
#     source("tests/manual/test_phase_h19.R")
#
# This is the "review" script — sourcing it loads every module, runs the
# pre-run check, runs the pipeline end-to-end, then prints a per-stage
# validation summary plus a reconciliation table.
#
# It does NOT quit R. It does NOT exit on errors. Failures are surfaced as
# normal R messages so you can inspect the resulting `pre`, `result`, and
# `verdict` objects in the workspace afterwards. If the pipeline crashes,
# the error message is printed and the script continues so partial state
# is still inspectable.
# ============================================================================


# ---- Source the pipeline -------------------------------------------------
# source_pipeline() lives in R/run_etl.R, so we source that first and let
# it pull in everything else in the right order.
source("R/run_etl.R")
source_pipeline()


cat("\n========== H19 :: pre_run_check ==========\n")
# Runs CONFIG + STATIC + INPUT validators tagged "pre_run". Look at `pre`
# in the workspace afterwards for the full per-validator detail. Failed
# CONFIG validators are usually the first thing to fix (paths, missing
# keys); failed STATIC validators usually mean the static reference data
# needs attention; INPUT failures point at the IFRSIN files themselves.
pre <- tryCatch(
  pre_run_check(config_path = "config.yml", verbose = TRUE),
  error = function(e) {
    cat("pre_run_check crashed:", conditionMessage(e), "\n")
    NULL
  }
)
if (!is.null(pre) && nrow(pre) > 0) {
  cat(sprintf("\npre_run_check: %d/%d validators passed\n",
              sum(as.logical(pre$passed)), nrow(pre)))
  fails <- pre[!as.logical(pre$passed), , drop = FALSE]
  if (nrow(fails) > 0) {
    cat("\nFailures:\n")
    for (i in seq_len(nrow(fails))) {
      cat(sprintf("  [%s] %s\n        %s\n",
                  as.character(fails$severity[i]),
                  fails$id[i],
                  fails$message[i] %||% ""))
    }
  }
}


cat("\n========== H19 :: run_etl (full pipeline) ==========\n")
# Reconciliation runs automatically when run_cfg$paths$reference_outputs
# is set in config.yml. Output goes to runs/<run_id>/ unless you pass
# output_root. Set keep_history = TRUE so this run shows up in the Runs
# tab if you launch the app afterwards.
result <- tryCatch(
  run_etl(
    config_path  = "config.yml",
    keep_history = TRUE,
    reconcile    = TRUE,
    verbose      = TRUE
  ),
  error = function(e) {
    cat("run_etl crashed:", conditionMessage(e), "\n")
    NULL
  }
)


# ---- Validation summary by stage -----------------------------------------
if (!is.null(result) && !is.null(result$validation) && nrow(result$validation) > 0) {
  v <- result$validation
  cat("\n========== Validation summary ==========\n")
  by_stage <- split(v, v$stage)
  for (st in names(by_stage)) {
    s <- by_stage[[st]]
    n_pass <- sum(as.logical(s$passed))
    n_err  <- sum(!s$passed & s$severity == "ERROR" & !(s$suppressed %||% FALSE))
    n_warn <- sum(!s$passed & s$severity == "WARN"  & !(s$suppressed %||% FALSE))
    n_supp <- sum(s$suppressed %||% FALSE)
    cat(sprintf("  %-12s  %d/%d passed   ERR=%d  WARN=%d  SUPPR=%d\n",
                st, n_pass, nrow(s), n_err, n_warn, n_supp))
  }
  fails <- v[!as.logical(v$passed) & !(v$suppressed %||% FALSE), , drop = FALSE]
  if (nrow(fails) > 0) {
    cat("\nNon-suppressed failures:\n")
    for (i in seq_len(nrow(fails))) {
      cat(sprintf("  [%s | %s] %s\n        %s\n",
                  as.character(fails$severity[i]),
                  as.character(fails$stage[i]),
                  fails$id[i],
                  fails$message[i] %||% ""))
    }
  }
}


# ---- Reconciliation summary ---------------------------------------------
verdict <- NULL
if (!is.null(result) && !is.null(result$reconciliation) &&
    !is.null(result$reconciliation$verdict)) {
  verdict <- result$reconciliation$verdict
  cat("\n========== Reconciliation (vs reference) ==========\n")
  cat("Reference:", result$reconciliation$reference_dir %||% "?", "\n\n")
  print(verdict, n = nrow(verdict))
  if (!is.null(result$reconciliation$md_path)) {
    cat("\nDetailed report:", result$reconciliation$md_path, "\n")
  }
} else {
  cat("\n(no reconciliation — set paths$reference_outputs in config.yml ",
      "to enable comparison vs the Excel tool's outputs)\n", sep = "")
}


cat("\n========== Done ==========\n")
cat("Workspace now contains:\n")
cat("  pre      - tibble of pre_run_check results\n")
cat("  result   - list returned by run_etl (validation, recon, paths, ...)\n")
cat("  verdict  - reconciliation verdict tibble (NULL if no recon)\n")
cat("\nInspect with: View(pre), View(result$validation), View(verdict)\n")
