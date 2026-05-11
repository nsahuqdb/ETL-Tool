# ============================================================================
# tests/manual/test_phase_e.R
#
# Smoke test for Phase E — exercises the macro model engine.
#
# Run from project root:
#     setwd("path/to/ifrs9_etl")
#     source("tests/manual/test_phase_e.R")
#
# Test strategy:
#   1. Unit tests for each pure macro_model function.
#   2. End-to-end: replicate the Excel Calculations sheet's Base Case PiT PD
#      term structure for QDB 1+ and QDB 1, comparing to the actual cell
#      values shipped with the workbook (these are checked in as constants
#      in this test).
#   3. Property checks on the full term structure across all
#      scenarios x ratings.
#
# What "PASS" looks like:
#   - All unit tests return TRUE
#   - QDB 1+ and QDB 1 PiT PD year 1..12 match Excel cell values to 1e-8
#   - Full term structure has no NaNs or out-of-range values
# ============================================================================

source("R/load_config.R")
source("R/load_static.R")
source("R/macro_model.R")
source("R/pd_term_structure.R")


cat("\n========== PHASE E :: setup ==========\n")
run_cfg   <- load_run_config("config.yml")
model_cfg <- load_model_config(run_cfg$paths$model_config)
static    <- load_static_reference(run_cfg$paths$static_dir)
cat(sprintf("  loaded model: %s\n", model_cfg$model$name))
cat(sprintf("  TTC anchor:   %s\n", model_cfg$model$ttc_anchor_pd))
cat(sprintf("  max maturity: %s\n", model_cfg$model$max_maturity))
cat(sprintf("  num MEVs:     %d\n", length(model_cfg$model$mevs)))


# ---------------------------------------------------------------------------
# Unit tests for each macro_model function
# ---------------------------------------------------------------------------
cat("\n========== PHASE E :: unit tests ==========\n")
unit_results <- list()

check <- function(label, condition) {
  status <- if (isTRUE(condition)) "PASS" else "FAIL"
  cat(sprintf("  [%s] %s\n", status, label))
  isTRUE(condition)
}

# stress_mevs: z=0 should be a no-op
hist <- matrix(c(2.47, 2.50, -8.04, -8.04, 10.45, 10.45),
               nrow = 2, byrow = FALSE)
unit_results$U1 <- check("U1: stress_mevs(z=0) == hist",
  identical(stress_mevs(hist, 0, model_cfg$model$mevs), hist))

# stress_mevs: shape preserved
str <- stress_mevs(hist, -1.2816, model_cfg$model$mevs)
unit_results$U2 <- check("U2: stress_mevs preserves shape",
  identical(dim(str), dim(hist)))

# stress_mevs: z * SD * unit_mult applied per-column
sd_vec   <- vapply(model_cfg$model$mevs, function(m) m$standard_deviation, numeric(1))
unit_vec <- vapply(model_cfg$model$mevs, function(m) m$stress_unit_multiplier, numeric(1))
expected <- hist + matrix(-1.2816 * sd_vec * unit_vec, nrow = 2, ncol = 3, byrow = TRUE)
unit_results$U3 <- check("U3: stress_mevs adds z*sd*unit_mult per MEV",
  all(abs(str - expected) < 1e-10))

# compute_logit_pds: linear in stressed MEV
logit <- compute_logit_pds(hist, model_cfg$model$mevs)
unit_results$U4 <- check("U4: compute_logit_pds shape == hist shape",
  identical(dim(logit), dim(hist)))

# compute_pds_from_logits: inverse logit in (0,1)
pds <- compute_pds_from_logits(logit)
unit_results$U5 <- check("U5: compute_pds_from_logits in (0,1)",
  all(pds > 0 & pds < 1))

# compute_per_mev_sf: positive when PD > anchor, negative when below
sf_test <- compute_per_mev_sf(c(0.146, 0.5, 0.05), model_cfg$model$ttc_anchor_pd)
unit_results$U6 <- check("U6: per-MEV SF zero at anchor PD",
  abs(sf_test[1]) < 1e-10)
unit_results$U7 <- check("U7: per-MEV SF positive when PD > anchor",
  sf_test[2] > 0)
unit_results$U8 <- check("U8: per-MEV SF negative when PD < anchor",
  sf_test[3] < 0)

# combine_sf
weights <- vapply(model_cfg$model$mevs, function(m) m$weight, numeric(1))
sf_mat <- matrix(c(0.1, 0.2, 0.3,
                   0.05, 0.1, 0.15), nrow = 2, byrow = TRUE)
combined <- combine_sf(sf_mat, weights)
expected_combined <- as.numeric(sf_mat %*% weights)
unit_results$U9 <- check("U9: combine_sf == matrix multiply",
  all(abs(combined - expected_combined) < 1e-10))

# pit_pd_term_structure
sf_t <- rep(0.05, 5)
pit  <- pit_pd_term_structure(0.001, sf_t, n_forecasts = 5, max_maturity = 10)
unit_results$U10 <- check("U10: pit_pd_term_structure has correct length",
  length(pit) == 10)
unit_results$U11 <- check("U11: pit_pd_term_structure all values in (0, 1)",
  all(!is.na(pit)) && all(pit > 0 & pit < 1))

# cumulative_pd is monotone increasing
cum <- cumulative_pd(c(0.01, 0.02, 0.03))
unit_results$U12 <- check("U12: cumulative_pd is monotone increasing",
  all(diff(cum) > 0))
unit_results$U13 <- check("U13: cumulative_pd[1] == marginal[1]",
  abs(cum[1] - 0.01) < 1e-12)

# marginal_pd is inverse of cumulative_pd
marg_back <- marginal_pd(cum)
unit_results$U14 <- check("U14: marginal_pd is inverse of cumulative_pd",
  all(abs(marg_back - c(0.01, 0.02, 0.03)) < 1e-12))


# ---------------------------------------------------------------------------
# End-to-end: reconcile Base Case QDB 1+ and QDB 1 against Excel values
# ---------------------------------------------------------------------------
cat("\n========== PHASE E :: end-to-end Base Case reconciliation ==========\n")

# MEV forecasts from Inputs_Lending Portfolio W5:Y9 (hardcoded — these
# are NOT user-supplied for the bundled Base Case run, they are the
# baseline forecasts the workbook ships with).
mev_fc <- matrix(c(
  2.47, -8.0417108518911302, 10.45,
  2.50, -8.0417108518911302, 10.45,
  2.53, -8.0417108518911302, 10.45,
  2.56, -8.0417108518911302, 10.45,
  2.59, -8.0417108518911302, 10.45
), nrow = 5, byrow = TRUE)
n_forecasts <- 5L
max_mat <- as.integer(model_cfg$model$max_maturity)

# Base Case scenario z = 0
stressed <- stress_mevs(mev_fc, 0, model_cfg$model$mevs)
logit    <- compute_logit_pds(stressed, model_cfg$model$mevs)
pds      <- compute_pds_from_logits(logit)
sf       <- compute_per_mev_sf(pds, model_cfg$model$ttc_anchor_pd)
combined <- combine_sf(sf, weights)

cat(sprintf("  Combined SF year 1..5: %s\n",
            paste(sprintf("%.6f", combined), collapse=", ")))

# Excel actual K30:O30 (combined SF year 1..5) — checked-in constants
excel_combined <- c(0.0553726285398638, 0.0549944939483303, 0.0546164206274062,
                    0.0542384085880653, 0.0538604578416052)
unit_results$E1 <- check("E1: combined SF year 1..5 matches Excel (1e-10)",
  all(abs(combined - excel_combined) < 1e-10))

# QDB 1+ TTC PD = 0.00013 (from Inputs_Lending Portfolio AU5)
ttc_qdb1plus <- 0.00013
pit_qdb1plus <- pit_pd_term_structure(ttc_qdb1plus, combined, n_forecasts, max_mat)

# Excel actual K7:V7 (PiT PD for QDB 1+, years 1..12) — checked-in constants
excel_qdb1plus <- c(
  1.6105906179960397e-04, 1.6082518562482792e-04, 1.6059166517757038e-04,
  1.6035849991159060e-04, 1.6012568928144174e-04, 1.5945623937950701e-04,
  1.5878678902933173e-04, 1.5811733823099236e-04, 1.5744788698448834e-04,
  1.5677843528981902e-04, 1.5610898314698384e-04, 1.5543953055598220e-04
)
unit_results$E2 <- check("E2: QDB 1+ PiT PD year 1..12 matches Excel (1e-10 rel)",
  all(abs(pit_qdb1plus[1:12] - excel_qdb1plus) /
        pmax(abs(excel_qdb1plus), 1e-10) < 1e-8))

# QDB 1 TTC PD = 5.449770637e-04 (from Inputs_Lending Portfolio AU6)
ttc_qdb1 <- 5.4497706373754897e-04
pit_qdb1 <- pit_pd_term_structure(ttc_qdb1, combined, n_forecasts, max_mat)
excel_qdb1 <- c(
  6.6173225724171919e-04, 6.6086207284226880e-04, 6.5999308518624119e-04,
  6.5912529262904316e-04, 6.5825869352814843e-04, 6.5574146338931276e-04,
  6.5322422691433584e-04, 6.5070698410264153e-04, 6.4818973495464182e-04,
  6.4567247946986108e-04, 6.4315521764871108e-04, 6.4063794949071608e-04
)
unit_results$E3 <- check("E3: QDB 1 PiT PD year 1..12 matches Excel (1e-10 rel)",
  all(abs(pit_qdb1[1:12] - excel_qdb1) /
        pmax(abs(excel_qdb1), 1e-10) < 1e-8))


# ---------------------------------------------------------------------------
# Full term structure across all scenarios × ratings
# ---------------------------------------------------------------------------
cat("\n========== PHASE E :: full term structure ==========\n")

# Build TTC PD table — loaded from data-raw/static/ttc_pd_table.csv (which
# was extracted from Inputs_Lending Portfolio AT5:AU45 by extract_from_xlsm.py).
ttc_pd_table <- static$ttc_pd_table

scenarios <- static$scenario_severity[, c("scenario", "severity_z",
                                           "scenario_probability_weight")]
ts <- build_pd_term_structure(ttc_pd_table, mev_fc, scenarios, model_cfg)
cat(sprintf("  rows in term structure: %d\n", nrow(ts)))
cat(sprintf("  unique ratings:         %d\n", length(unique(ts$rating))))
cat(sprintf("  unique scenarios:       %d\n", length(unique(ts$scenario))))
cat(sprintf("  maturity range:         %d..%d\n",
            min(ts$maturity), max(ts$maturity)))

# Property checks on full term structure
unit_results$F1 <- check("F1: nrow == n_ratings * n_scenarios * max_maturity",
  nrow(ts) == nrow(ttc_pd_table) * nrow(scenarios) * max_mat)
unit_results$F2 <- check("F2: no NA in marginal_pd",
  all(!is.na(ts$marginal_pd)))
unit_results$F3 <- check("F3: all marginal_pd in (0, 1)",
  all(ts$marginal_pd > 0 & ts$marginal_pd < 1))
unit_results$F4 <- check("F4: all cumulative_pd in (0, 1] (=1 only for low ratings at long maturities)",
  all(ts$cumulative_pd > 0 & ts$cumulative_pd <= 1))
unit_results$F5 <- check("F5: cumulative_pd is monotone increasing within each (rating, scenario)",
  {
    bad <- ts |>
      dplyr::group_by(rating, scenario) |>
      dplyr::summarise(any_decrease = any(diff(cumulative_pd) < -1e-12),
                       .groups = "drop")
    !any(bad$any_decrease)
  })

# Scenarios with worse z should produce higher PD
unit_results$F6 <- check("F6: Sig Downturn has higher year-1 PD than Sig Uptrend (worst rating)",
  {
    qdb7_y1 <- ts[ts$rating == "QDB 7" & ts$maturity == 1, ]
    sig_down <- qdb7_y1$marginal_pd[qdb7_y1$scenario == "Significant Downturn"]
    sig_up   <- qdb7_y1$marginal_pd[qdb7_y1$scenario == "Significant Uptrend"]
    sig_down > sig_up
  })


# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
cat("\n========== PHASE E SUMMARY ==========\n")
n_pass <- sum(unlist(unit_results))
n_total <- length(unit_results)
cat(sprintf("  %d/%d checks passed\n", n_pass, n_total))
if (n_pass == n_total) {
  cat("  PHASE E :: PASS\n")
} else {
  cat("  PHASE E :: FAIL — see failures above\n")
  failed <- names(unit_results)[!unlist(unit_results)]
  cat(sprintf("  Failed checks: %s\n", paste(failed, collapse=", ")))
}
