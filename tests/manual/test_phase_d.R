# ============================================================================
# tests/manual/test_phase_d.R
#
# Smoke test for Phase D — runs the full lending + investment transformation
# pipelines against the sample inputs in input/.
#
# Run from the project root:
#
#     setwd("path/to/ifrs9_etl")
#     source("tests/manual/test_phase_d.R")
#
# What it does:
#   1. Loads config + static reference (uses Phase B+C loaders)
#   2. Reads inputs (uses Phase B readers)
#   3. Builds Transformation_lending  (Pass 1-3)
#   4. Builds Inputs_Lending Portfolio view  (Pass 4-6)
#   5. Builds Transformation_investments
#   6. Builds Inputs_Investment Portfolio view
#   7. Runs property checks against both sides
#
# What "PASS" looks like:
#   - All builders return non-NULL
#   - Customer/account counts match input row counts
#   - Total exposure > 0
#   - All property checks return TRUE
# ============================================================================

source("R/io_helpers.R")
source("R/read_inputs.R")
source("R/load_config.R")
source("R/load_static.R")
source("R/transform_lending.R")
source("R/lending_portfolio_view.R")
source("R/transform_investments.R")
source("R/investment_portfolio_view.R")

cat("\n========== PHASE D :: setup ==========\n")
run_cfg   <- load_run_config("config.yml")
model_cfg <- load_model_config(run_cfg$paths$model_config)
static    <- load_static_reference(run_cfg$paths$static_dir)
cat(sprintf("  loaded %d static reference items\n", length(static)))

inputs <- read_all_inputs(run_cfg$paths$input_dir)
cat(sprintf("  loaded %d input files\n", length(inputs)))


cat("\n========== PHASE D :: build_transformation_lending ==========\n")
trans_l <- build_transformation_lending(inputs, static, model_cfg, run_cfg)
cat(sprintf("  rows: %d\n", nrow(trans_l)))
cat(sprintf("  columns: %d\n", ncol(trans_l)))
cat(sprintf("  total exposure: $%s\n", format(sum(trans_l$exposure_amount), big.mark=",", nsmall=2)))

cat("\n  Sector breakdown of contracts:\n")
print(table(trans_l$sector, useNA = "always"))

cat("\n  Rating distribution (per contract):\n")
print(table(trans_l$rating, useNA = "always"))


cat("\n========== PHASE D :: build_lending_portfolio_view ==========\n")
out <- build_lending_portfolio_view(trans_l, inputs, static, model_cfg)
cm_view <- out$portfolio
trans_l <- out$transformation  # now has override-dependent columns filled
cat(sprintf("  customers: %d\n", nrow(cm_view)))
cat(sprintf("  total exposure: $%s\n", format(sum(cm_view$exposure_total), big.mark=",", nsmall=2)))

cat("\n  Stage distribution:\n")
print(table(cm_view$stage_final))


cat("\n========== PHASE D :: build_transformation_investments ==========\n")
trans_i <- build_transformation_investments(inputs, static, model_cfg, run_cfg)
cat(sprintf("  rows: %d\n", nrow(trans_i)))
cat(sprintf("  total exposure: $%s\n", format(sum(trans_i$exposure_amount), big.mark=",", nsmall=2)))

cat("\n  Rating distribution (current):\n")
print(table(trans_i$rating_current, useNA = "always"))


cat("\n========== PHASE D :: build_investment_portfolio_view ==========\n")
out_i <- build_investment_portfolio_view(trans_i, inputs, static)
inv_view <- out_i$portfolio
trans_i <- out_i$transformation
cat(sprintf("  accounts: %d\n", nrow(inv_view)))

cat("\n  Stage distribution:\n")
print(table(inv_view$stage_final))


cat("\n========== PHASE D :: PROPERTY CHECKS ==========\n")

# Property check helper
check <- function(label, condition) {
  status <- if (isTRUE(condition)) "PASS" else "FAIL"
  cat(sprintf("  [%s] %s\n", status, label))
  isTRUE(condition)
}

results <- list()

# --- Lending properties ---
results$L1 <- check("L1: all customer_ids non-NA",
                    all(!is.na(cm_view$customer_id)))
have_contracts <- cm_view$customer_id %in% trans_l$customer_id
results$L2 <- check("L2: rating_current populated for customers with contracts",
                    all(!is.na(cm_view$rating_current[have_contracts])))
results$L3 <- check("L3: stage in {Stage 1, Stage 2, Stage 3}",
                    all(cm_view$stage_final %in% c("Stage 1","Stage 2","Stage 3")))
results$L4 <- check("L4: DPD>90 -> Stage 3",
                    all(cm_view$stage_final[cm_view$dpd_status > 90] == "Stage 3"))
clean_low_dpd <- cm_view$dpd_status <= 60 &
                 cm_view$restructuring_final == "" &
                 cm_view$watchlist_status == ""
results$L5 <- check("L5: clean low-DPD -> Stage 1",
                    all(cm_view$stage_final[clean_low_dpd] == "Stage 1"))
results$L6 <- check("L6: watchlist -> Stage 2 or 3",
                    all(cm_view$stage_final[cm_view$watchlist_status == "Watchlist"]
                        %in% c("Stage 2","Stage 3")))
results$L7 <- check("L7: restructured -> Stage 2 or 3",
                    all(cm_view$stage_final[cm_view$restructuring_final == "Restructured"]
                        %in% c("Stage 2","Stage 3")))
results$L8 <- check("L8: total exposure > 0", sum(cm_view$exposure_total) > 0)
results$L9 <- check("L9: rating hierarchy populated for every contract",
                    all(!is.na(trans_l$rating_hierarchy)))
results$L10 <- check("L10: Pass 6 back-fill — is_default_final populated",
                     all(!is.na(trans_l$is_default_final)))
results$L11 <- check("L11: Pass 6 back-fill — rating_after_override populated",
                     all(!is.na(trans_l$rating_after_override) |
                         is.na(trans_l$rating)))

# --- Investment properties ---
results$I1 <- check("I1: all account_ids non-NA",
                    all(!is.na(trans_i$account_id)))
results$I2 <- check("I2: rating_current populated",
                    all(!is.na(trans_i$rating_current)))
results$I3 <- check("I3: rating_hierarchy populated",
                    all(!is.na(trans_i$rating_hierarchy)))
results$I4 <- check("I4: stage in {Stage 1, Stage 2}",
                    all(inv_view$stage_final %in% c("Stage 1","Stage 2")))
top_tier <- trans_i$rating_hierarchy <= 4
results$I5 <- check("I5: top-tier (h<=4) -> Stage 1",
                    all(inv_view$stage_final[top_tier] == "Stage 1"))
results$I6 <- check("I6: total exposure > 0",
                    sum(trans_i$exposure_amount) > 0)
results$I7 <- check("I7: all rating_current values are valid",
                    all(trans_i$rating_current %in%
                        static$master_rating_scale$rating))
results$I8 <- check("I8: Pass 6 back-fill — rating_after_override populated",
                    all(!is.na(trans_i$rating_after_override)))


cat("\n========== PHASE D SUMMARY ==========\n")
n_pass <- sum(unlist(results))
n_total <- length(results)
cat(sprintf("  %d/%d property checks passed\n", n_pass, n_total))
if (n_pass == n_total) {
  cat("  PHASE D :: PASS\n")
} else {
  cat("  PHASE D :: FAIL — see failures above\n")
  failed <- names(results)[!unlist(results)]
  cat(sprintf("  Failed checks: %s\n", paste(failed, collapse=", ")))
}
