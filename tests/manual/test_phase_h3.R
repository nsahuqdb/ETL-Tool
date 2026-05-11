# ============================================================================
# tests/manual/test_phase_h3.R
#
# Smoke test for Phase H3 — derived-output validators.
#
# Runs the full pipeline through Phase F and applies all three validator
# suites to date (input, transform, derived).
#
# Run from project root:
#     setwd("path/to/ifrs9_etl")
#     source("tests/manual/test_phase_h3.R")
# ============================================================================

source("R/io_helpers.R")
source("R/load_config.R")
source("R/load_static.R")
source("R/read_inputs.R")
source("R/transform_lending.R")
source("R/lending_portfolio_view.R")
source("R/transform_investments.R")
source("R/investment_portfolio_view.R")
source("R/macro_model.R")
source("R/pd_term_structure.R")
source("R/lifetime_parameter_other.R")
source("R/build_stpd.R")
source("R/validation.R")
source("R/validators_input.R")
source("R/validators_transform.R")
source("R/validators_derived.R")


cat("\n========== PHASE H3 :: setup ==========\n")
run_cfg   <- load_run_config("config.yml")
model_cfg <- load_model_config(run_cfg$paths$model_config)
static    <- load_static_reference(run_cfg$paths$static_dir)


# Build country-weighted GCC GDP history (same as test_phase_f).
build_gcc_history <- function(static) {
  growth <- static$gcc_real_gdp_growth
  prices <- static$gcc_gdp_current_prices
  g <- tidyr::pivot_wider(growth, names_from = country, values_from = value)
  p <- tidyr::pivot_wider(prices, names_from = country, values_from = value)
  yrs <- intersect(g$year, p$year)
  yrs <- yrs[yrs >= 1982]
  out <- numeric(length(yrs))
  for (i in seq_along(yrs)) {
    y <- yrs[i]
    g_row <- as.numeric(g[g$year == y, setdiff(names(g), "year")])
    p_row <- as.numeric(p[p$year == y, setdiff(names(p), "year")])
    valid <- !is.na(g_row) & !is.na(p_row)
    if (sum(valid) < 2) { out[i] <- NA_real_; next }
    w <- p_row[valid] / sum(p_row[valid])
    out[i] <- sum(g_row[valid] * w)
  }
  out[!is.na(out)]
}
gcc_history <- build_gcc_history(static)

model_inputs <- load_model_inputs(
  path                = run_cfg$paths$model_inputs,
  model_cfg           = model_cfg,
  scenarios           = static$scenario_severity,
  gcc_history         = gcc_history,
  non_oil_gdp_history = static$non_oil_gdp_history$value
)

inputs <- read_all_inputs(run_cfg$paths$input_dir, verbose = FALSE)
cat(sprintf("  loaded %d input files, %d static reference items\n",
            length(inputs), length(static)))


# ---------------------------------------------------------------------------
# Phase D — transformations + portfolio views
# ---------------------------------------------------------------------------
cat("\n========== PHASE H3 :: build Phase D outputs ==========\n")
trans_l <- build_transformation_lending(inputs, static, model_cfg, run_cfg)
out_l   <- build_lending_portfolio_view(trans_l, inputs, static, model_cfg)
cm_view <- out_l$portfolio
trans_l <- out_l$transformation

trans_i  <- build_transformation_investments(inputs, static, model_cfg, run_cfg)
out_i    <- build_investment_portfolio_view(trans_i, inputs, static)
inv_view <- out_i$portfolio
trans_i  <- out_i$transformation

cat(sprintf("  trans_l rows:  %d  cm_view rows:  %d\n", nrow(trans_l), nrow(cm_view)))
cat(sprintf("  trans_i rows:  %d  inv_view rows: %d\n", nrow(trans_i), nrow(inv_view)))


# ---------------------------------------------------------------------------
# Phase F — derived outputs
# ---------------------------------------------------------------------------
cat("\n========== PHASE H3 :: build Phase F outputs ==========\n")
ltpo <- build_lifetime_parameter_other(inputs$RepaymentSchedule,
                                        trans_l, run_cfg)

scenarios <- static$scenario_severity[, c("scenario", "severity_z")]
internal_ts <- build_pd_term_structure(
  ttc_pd_table = static$ttc_pd_table,
  scenarios    = scenarios,
  model_cfg    = model_cfg,
  model_inputs = model_inputs,
  rating_type  = "Internal"
)
external_ts <- build_pd_term_structure(
  ttc_pd_table = static$ttc_pd_table_external,
  scenarios    = scenarios,
  model_cfg    = model_cfg,
  model_inputs = model_inputs,
  gcc_history  = gcc_history,
  rating_type  = "External"
)
portfolios <- tibble::tribble(
  ~portfolio,         ~rating_type,
  "Business Finance", "Internal",
  "Off BS",           "Internal",
  "Al Dhameen",       "Internal",
  "Tasdeer",          "Internal",
  "Banks and Fis",    "External",
  "Investments",      "External"
)
ratings_combined <- dplyr::bind_rows(
  static$master_rating_scale[static$master_rating_scale$rating_type=="Internal",
                              c("rating","hierarchy")],
  static$master_rating_scale[static$master_rating_scale$rating_type=="External",
                              c("rating","hierarchy")]
)
stpd <- build_stpd(
  internal_term_structure   = internal_ts,
  external_term_structure   = external_ts,
  internal_scenario_weights = model_inputs$internal_scenario_weights,
  external_scenario_weights = model_inputs$external_scenario_weights,
  portfolios                = portfolios,
  ratings                   = ratings_combined,
  run_cfg                   = run_cfg,
  max_month                 = 600
)
cat(sprintf("  ltpo rows: %d, stpd rows: %d\n", nrow(ltpo), nrow(stpd)))


# ---------------------------------------------------------------------------
# Run all three validator suites
# ---------------------------------------------------------------------------
input_results <- run_validation_suite(
  "INPUT", build_input_validators(),
  args = list(inputs = inputs, static = static, run_cfg = run_cfg),
  verbose = TRUE
)

transform_results <- validate_transform_outputs(
  trans_l = trans_l, cm_view = cm_view,
  trans_i = trans_i, inv_view = inv_view,
  static = static, model_cfg = model_cfg
)

derived_results <- validate_derived_outputs(
  ltpo = ltpo, stpd = stpd, model_inputs = model_inputs,
  trans_l = trans_l, static = static, model_cfg = model_cfg
)


# ---------------------------------------------------------------------------
# Combined summary
# ---------------------------------------------------------------------------
all_results <- combine_validation_results(input_results, transform_results,
                                           derived_results)
cat("\n========== PHASE H3 SUMMARY ==========\n")

n_total <- nrow(all_results)
n_pass  <- sum(all_results$passed)
n_err   <- sum(!all_results$passed & all_results$severity == "ERROR")
n_warn  <- sum(!all_results$passed & all_results$severity == "WARN")
n_info  <- sum(!all_results$passed & all_results$severity == "INFO")

cat(sprintf("  %d/%d validators passed across INPUT + TRANSFORM + DERIVED\n",
            n_pass, n_total))
cat(sprintf("    ERROR failures: %d\n", n_err))
cat(sprintf("    WARN  failures: %d\n", n_warn))
cat(sprintf("    INFO  failures: %d\n", n_info))

cat("\n  Per-suite breakdown:\n")
suites <- list(INPUT = input_results, TRANSFORM = transform_results,
                DERIVED = derived_results)
for (nm in names(suites)) {
  r <- suites[[nm]]
  cat(sprintf("    %-10s  %d/%d passed  (%d ERROR, %d WARN failures)\n",
              nm,
              sum(r$passed), nrow(r),
              sum(!r$passed & r$severity == "ERROR"),
              sum(!r$passed & r$severity == "WARN")))
}

if (n_err == 0) {
  cat("\n  PHASE H3 :: PASS  (no ERROR-severity failures)\n")
} else {
  cat("\n  PHASE H3 :: FAIL\n")
  failed <- all_results$id[!all_results$passed & all_results$severity == "ERROR"]
  cat(sprintf("  Failed (ERROR): %s\n", paste(failed, collapse = ", ")))
}

.last_validation_results <- all_results
cat("\n  Combined results stored in .last_validation_results.\n")
