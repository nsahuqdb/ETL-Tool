# ============================================================================
# tests/manual/test_phase_h2.R
#
# Smoke test for Phase H2 — transform-stage validators.
#
# Run from project root:
#     setwd("path/to/ifrs9_etl")
#     source("tests/manual/test_phase_h2.R")
#
# What it does:
#   1. Loads inputs + static (Phases B/C).
#   2. Builds transformation_lending, lending portfolio view,
#      transformation_investments, investment portfolio view (Phase D).
#   3. Runs the input-stage validators (Phase H1) — keeps a single rolled-up
#      report.
#   4. Runs the transform-stage validators (Phase H2) — 22 checks.
#   5. Prints a combined summary across both suites.
# ============================================================================

source("R/io_helpers.R")
source("R/load_config.R")
source("R/load_static.R")
source("R/read_inputs.R")
source("R/transform_lending.R")
source("R/lending_portfolio_view.R")
source("R/transform_investments.R")
source("R/investment_portfolio_view.R")
source("R/validation.R")
source("R/validators_input.R")
source("R/validators_transform.R")


cat("\n========== PHASE H2 :: setup ==========\n")
run_cfg   <- load_run_config("config.yml")
model_cfg <- load_model_config(run_cfg$paths$model_config)
static    <- load_static_reference(run_cfg$paths$static_dir)
inputs    <- read_all_inputs(run_cfg$paths$input_dir, verbose = FALSE)
cat(sprintf("  loaded %d input files, %d static reference items\n",
            length(inputs), length(static)))


# ---------------------------------------------------------------------------
# Phase D — transformations + portfolio views
# ---------------------------------------------------------------------------
cat("\n========== PHASE H2 :: build Phase D outputs ==========\n")
trans_l <- build_transformation_lending(inputs, static, model_cfg, run_cfg)
out_l   <- build_lending_portfolio_view(trans_l, inputs, static, model_cfg)
cm_view <- out_l$portfolio
trans_l <- out_l$transformation

trans_i  <- build_transformation_investments(inputs, static, model_cfg, run_cfg)
out_i    <- build_investment_portfolio_view(trans_i, inputs, static)
inv_view <- out_i$portfolio
trans_i  <- out_i$transformation

cat(sprintf("  trans_l rows:  %d (lending contracts)\n",  nrow(trans_l)))
cat(sprintf("  cm_view rows:  %d (lending customers)\n",  nrow(cm_view)))
cat(sprintf("  trans_i rows:  %d (investment accounts)\n", nrow(trans_i)))
cat(sprintf("  inv_view rows: %d\n",                       nrow(inv_view)))


# ---------------------------------------------------------------------------
# Phase H1 — input-stage validators
# ---------------------------------------------------------------------------
input_validators <- build_input_validators()
input_results <- run_validation_suite(
  "INPUT", input_validators,
  args = list(inputs = inputs, static = static, run_cfg = run_cfg),
  verbose = TRUE
)


# ---------------------------------------------------------------------------
# Phase H2 — transform-stage validators
# ---------------------------------------------------------------------------
transform_results <- validate_transform_outputs(
  trans_l   = trans_l,
  cm_view   = cm_view,
  trans_i   = trans_i,
  inv_view  = inv_view,
  static    = static,
  model_cfg = model_cfg
)


# ---------------------------------------------------------------------------
# Combined summary
# ---------------------------------------------------------------------------
all_results <- combine_validation_results(input_results, transform_results)
cat("\n========== PHASE H2 SUMMARY ==========\n")

n_total <- nrow(all_results)
n_pass  <- sum(all_results$passed)
n_err   <- sum(!all_results$passed & all_results$severity == "ERROR")
n_warn  <- sum(!all_results$passed & all_results$severity == "WARN")
n_info  <- sum(!all_results$passed & all_results$severity == "INFO")

cat(sprintf("  %d/%d validators passed across INPUT + TRANSFORM\n", n_pass, n_total))
cat(sprintf("    ERROR failures: %d\n", n_err))
cat(sprintf("    WARN  failures: %d\n", n_warn))
cat(sprintf("    INFO  failures: %d\n", n_info))

# Per-suite breakdown
cat("\n  Per-suite breakdown:\n")
suites <- list(INPUT = input_results, TRANSFORM = transform_results)
for (nm in names(suites)) {
  r <- suites[[nm]]
  cat(sprintf("    %-10s  %d/%d passed  (%d ERROR, %d WARN failures)\n",
              nm,
              sum(r$passed), nrow(r),
              sum(!r$passed & r$severity == "ERROR"),
              sum(!r$passed & r$severity == "WARN")))
}

if (n_err == 0) {
  cat("\n  PHASE H2 :: PASS  (no ERROR-severity failures)\n")
} else {
  cat("\n  PHASE H2 :: FAIL\n")
  failed <- all_results$id[!all_results$passed & all_results$severity == "ERROR"]
  cat(sprintf("  Failed (ERROR): %s\n", paste(failed, collapse = ", ")))
}

# Make results available for inspection
.last_validation_results <- all_results
cat("\n  Combined results stored in .last_validation_results.\n")
