# tests/manual/test_phase_h10_with_history.R
#
# H10 :: end-to-end run with keep_history=TRUE. Used to populate the
# runs/ directory so the Shiny app has data to display.
#
# Usage from project root:
#   source("tests/manual/test_phase_h10_with_history.R")
#
# After running, launch the app:
#   shiny::runApp("app", launch.browser = TRUE)

source("R/run_etl.R")
source_pipeline()

cat("\n========== H10 :: run_etl with keep_history=TRUE ==========\n")
result <- run_etl(
  config_path  = "config.yml",
  keep_history = TRUE,
  verbose      = TRUE
)

cat(sprintf("\nRun ID: %s\n", result$run_id))
cat(sprintf("Output root: %s\n", result$run_dir))
cat(sprintf("Manifest:    %s\n", result$manifest_path))
cat(sprintf("Duration:    %.1fs\n", result$duration_seconds))

if (!is.null(result$validation) && nrow(result$validation) > 0) {
  v <- result$validation
  cat(sprintf("Validation:  %d/%d passed (%d failures)\n",
              sum(v$passed), nrow(v), sum(!v$passed)))
}

cat("\nList of runs now visible to the Shiny app:\n")
print(list_runs()[, c("run_id", "started_at", "duration_seconds",
                       "n_validation_failures", "snapshot_label")])

cat("\nLaunch the app with:\n  shiny::runApp(\"app\", launch.browser = TRUE)\n")
