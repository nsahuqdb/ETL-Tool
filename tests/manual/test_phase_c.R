# ============================================================================
# tests/manual/test_phase_c.R
#
# Smoke test for Phase C (static + config loaders).
#
# Run from the project root:
#
#     setwd("path/to/ifrs9_etl")
#     source("tests/manual/test_phase_c.R")
#
# What it does:
#   1. Sources Phase C R files
#   2. Loads run config (config.yml) and reports paths
#   3. Loads model config and prints MEV table + GCC distribution
#   4. Loads all 10 static reference items and reports shapes
#   5. Runs all 3 validators and reports any issues
#
# What "PASS" looks like:
#   - All loaders return non-NULL
#   - 10 static items present, with row counts matching what Phase A produced
#   - All 3 validators return empty issue tibbles
# ============================================================================

source("R/load_config.R")
source("R/load_static.R")
source("R/validate_config.R")

cat("\n========== PHASE C :: load_run_config ==========\n")
run_cfg <- load_run_config("config.yml")
cat(sprintf("  input_dir       : %s\n", run_cfg$paths$input_dir))
cat(sprintf("  output_dir      : %s\n", run_cfg$paths$output_dir))
cat(sprintf("  static_dir      : %s\n", run_cfg$paths$static_dir))
cat(sprintf("  model_config    : %s\n", run_cfg$paths$model_config))
cat(sprintf("  extract_date    : %s\n", run_cfg$run$extract_date))


cat("\n========== PHASE C :: load_model_config ==========\n")
model_cfg <- load_model_config(run_cfg$paths$model_config)
cat(sprintf("  model name      : %s\n", model_cfg$model$name))
cat(sprintf("  ttc_anchor_pd   : %s\n", model_cfg$model$ttc_anchor_pd))
cat(sprintf("  max_maturity    : %s\n", model_cfg$model$max_maturity))
cat(sprintf("  unrated_fallback: %s\n", model_cfg$unrated_fallback_rating))
cat(sprintf("  GCC growth dist : mean=%.4f, sd=%.4f\n",
            model_cfg$gcc_growth_distribution$mean,
            model_cfg$gcc_growth_distribution$standard_deviation))

cat("\n  MEVs:\n")
mev_df <- dplyr::bind_rows(lapply(model_cfg$model$mevs, function(m) {
  tibble::tibble(name = m$name,
                 intercept = m$intercept,
                 coefficient = m$coefficient,
                 weight = m$weight,
                 sd = m$standard_deviation,
                 unit_mult = m$stress_unit_multiplier)
}))
print(mev_df)
cat(sprintf("  weight sum check: %.6f\n",
            sum(vapply(model_cfg$model$mevs, function(m) m$weight, numeric(1)))))


cat("\n========== PHASE C :: load_static_reference ==========\n")
static <- load_static_reference(run_cfg$paths$static_dir)
for (key in names(static)) {
  obj <- static[[key]]
  if (is.data.frame(obj)) {
    cat(sprintf("  %-30s  %d rows x %d cols\n", key, nrow(obj), ncol(obj)))
  } else if (is.list(obj)) {
    cat(sprintf("  %-30s  named list with %d entries: %s\n",
                key, length(obj), paste(names(obj), collapse = ", ")))
  } else {
    cat(sprintf("  %-30s  unexpected type: %s\n", key, class(obj)[1]))
  }
}

cat("\n  Sample peeks:\n")
cat("  scenario_severity:\n")
print(static$scenario_severity)
cat("\n  staging_thresholds (named list):\n")
print(static$staging_thresholds)
cat("\n  collective_assessment_rules:\n")
print(static$collective_assessment_rules)


cat("\n========== PHASE C :: validators ==========\n")

cat("\n[1/3] validate_static_reference():\n")
issues_s <- validate_static_reference(static)
if (nrow(issues_s) == 0) cat("  PASS - 0 issues\n") else print(issues_s)

cat("\n[2/3] validate_model_config():\n")
issues_m <- validate_model_config(model_cfg)
if (nrow(issues_m) == 0) cat("  PASS - 0 issues\n") else print(issues_m)

cat("\n[3/3] validate_consistency(static, model_cfg):\n")
issues_x <- validate_consistency(static, model_cfg)
if (nrow(issues_x) == 0) cat("  PASS - 0 issues\n") else print(issues_x)

cat("\n========== PHASE C COMPLETE ==========\n")
