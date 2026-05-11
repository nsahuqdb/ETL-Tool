# ============================================================================
# tests/manual/test_phase_h5.R
#
# Phase H5/H6 — exercise the run_etl() orchestrator end-to-end and surface
# any regressions vs the V4 bundle.
#
# Run from project root:
#     setwd("path/to/ifrs9_etl")
#     source("tests/manual/test_phase_h5.R")
# ============================================================================

# Source every module in dependency order. source_pipeline lives in
# R/run_etl.R, but we need to source that first. Once run_etl.R is sourced
# it provides the helper that sources everything else.
source("R/run_etl.R")
source_pipeline()


cat("\n========== H8 :: pre_run_check (fast feedback path) ==========\n")
# Runs ONLY input validators tagged 'pre_run', without running the rest of
# the pipeline. Equivalent to what the Shiny load page will call before the
# user clicks "Run".
pre <- pre_run_check(config_path = "config.yml", verbose = TRUE)
cat(sprintf("\npre_run_check: %d/%d validators passed\n",
            sum(pre$passed), nrow(pre)))


cat("\n========== PHASE H5 :: run_etl ==========\n")
# Use test_output/ as the root so we preserve parity with the previous
# test runner's layout (test_output/Output, test_output/reports/...).
result <- run_etl(config_path = "config.yml",
                   output_root = "test_output",
                   verbose = TRUE)


# ---------------------------------------------------------------------------
# Summarize the run (still useful to print to console even though
# write_summary_markdown writes the file)
# ---------------------------------------------------------------------------
cat("\n========== PHASE H5 :: summarize ==========\n")
output_summary <- summarize_output_dir(file.path(result$run_dir, "Output"))
cat(sprintf("Summarized %d output files\n", length(output_summary)))

write_summary_markdown(
  output_summary,
  file.path(result$run_dir, "reports", "run_summary.md")
)


# ---------------------------------------------------------------------------
# Validation summary table
# ---------------------------------------------------------------------------
if (!is.null(result$validation) && nrow(result$validation) > 0) {
  cat("\n========== VALIDATION SUMMARY ==========\n")
  v <- result$validation
  v$.suppr <- v$suppressed %||% rep(FALSE, nrow(v))
  agg <- aggregate(
    cbind(n          = rep(1L, nrow(v)),
          fail       = as.integer(!v$passed),
          err        = as.integer(!v$passed & !v$.suppr & v$effective_severity == "ERROR"),
          warn       = as.integer(!v$passed & !v$.suppr & v$effective_severity == "WARN"),
          info       = as.integer(!v$passed & !v$.suppr & v$effective_severity == "INFO"),
          suppressed = as.integer(!v$passed & v$.suppr)) ~ stage,
    data = v, FUN = sum)
  print(agg)

  failed <- v[!v$passed, c("stage", "id", "severity", "effective_severity",
                            "suppressed", "context", "description")]
  if (nrow(failed) > 0) {
    cat("\nFailed checks:\n")
    print(failed, row.names = FALSE)
  } else {
    cat("\nAll checks passed.\n")
  }
  cat(sprintf("\nFull validation report: %s\n",
              file.path(result$run_dir, "reports", "validation.md")))
  cat(sprintf("Validation CSV (machine-readable): %s\n",
              file.path(result$run_dir, "reports", "validation.csv")))
}


# ---------------------------------------------------------------------------
# Reconciliation summary (if run_etl ran it)
# ---------------------------------------------------------------------------
if (!is.null(result$reconciliation)) {
  cat("\n========== PHASE H5 :: reconciliation ==========\n")
  cat("Reconciliation verdict:\n")
  print(result$reconciliation$verdict)
  cat(sprintf("\nMismatch CSVs in: %s\n",
              file.path(result$run_dir, "reports", "mismatches")))
} else {
  cat("\n(no reconciliation - set run_cfg$paths$reference_outputs to enable)\n")
}


cat(sprintf("\nManifest: %s\n", result$manifest_path))
cat(sprintf("Run took %.1f seconds\n", result$duration_seconds))
