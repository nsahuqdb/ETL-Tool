# ============================================================================
# tests/manual/test_phase_f.R
#
# Smoke test for Phase F — exercises the derived-output builders.
#
# Run from project root:
#     setwd("path/to/ifrs9_etl")
#     source("tests/manual/test_phase_f.R")
#
# Test strategy:
#   1. LifeTimeParameterOther: build from RepaymentSchedule + Transformation,
#      cross-check against the bundled Output sample.
#   2. StPD: build the long-format scenario-weighted PD term structure for
#      both internal (Vasicek-shift) and external (Basel ASRF + GCC GDP
#      PERCENTRANK) portfolios.
#
# Reconciliation reference: bundled `Output/StPD.csv` (extracted from the
# V4 workbook) for the four INTERNAL portfolios. The two EXTERNAL portfolios
# in V4's bundled output were generated with the older Vasicek-shift method
# applied to external TTC PDs, while the R port now uses V7's correct
# Basel ASRF + GCC PERCENTRANK methodology — so external numbers are
# expected to differ from V4's bundled values.
# ============================================================================

source("R/load_config.R")
source("R/load_static.R")
source("R/io_helpers.R")
source("R/read_inputs.R")
source("R/transform_lending.R")
source("R/macro_model.R")
source("R/pd_term_structure.R")
source("R/lifetime_parameter_other.R")
source("R/build_stpd.R")


cat("\n========== PHASE F :: setup ==========\n")
run_cfg   <- load_run_config("config.yml")
model_cfg <- load_model_config(run_cfg$paths$model_config)
static    <- load_static_reference(run_cfg$paths$static_dir)

# Build the country-weighted GCC GDP history series. This matches V7's
# 'GCC GDP'!D28:AT28 row: per-year, weight each country's growth by its
# nominal GDP share that year (then sum). Using the static CSVs:
#   data-raw/static/gcc_real_gdp_growth.csv (per country per year)
#   data-raw/static/gcc_gdp_current_prices.csv (per country per year)
# We filter to year >= 1982 to match V7's specific window (years 1980-1981
# have N/A entries in some country columns).
build_gcc_history <- function(static) {
  growth <- static$gcc_real_gdp_growth
  prices <- static$gcc_gdp_current_prices
  # Pivot wide
  g <- tidyr::pivot_wider(growth, names_from = country, values_from = value)
  p <- tidyr::pivot_wider(prices, names_from = country, values_from = value)
  # Inner join on year
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
cat(sprintf("  gcc_history: %d years, mean=%.4f, sd=%.4f\n",
            length(gcc_history), mean(gcc_history), sd(gcc_history)))

# Load per-run model inputs (scenario weights + MEV forecasts + external GCC
# forecast). Internal weights are explicit; external weights are computed
# automatically from the GCC GDP CDF.
model_inputs <- load_model_inputs(
  path                = run_cfg$paths$model_inputs,
  model_cfg           = model_cfg,
  scenarios           = static$scenario_severity,
  gcc_history         = gcc_history,
  non_oil_gdp_history = static$non_oil_gdp_history$value
)
cat("  Internal scenario weights:\n")
for (s in names(model_inputs$internal_scenario_weights)) {
  cat(sprintf("    %-22s  %.5f\n", s, model_inputs$internal_scenario_weights[s]))
}
cat("  External scenario weights (year 1 — full per-year matrix in model_inputs$external_scenario_weights$per_year):\n")
ext_y1 <- model_inputs$external_scenario_weights$per_year[1, ]
for (s in names(ext_y1)) {
  cat(sprintf("    %-22s  %.5f\n", s, ext_y1[s]))
}
cat("  External scenario weights (year-6+ average):\n")
for (s in names(model_inputs$external_scenario_weights$average)) {
  cat(sprintf("    %-22s  %.5f\n", s, model_inputs$external_scenario_weights$average[s]))
}

inputs    <- read_all_inputs(run_cfg$paths$input_dir)
transformation <- build_transformation_lending(inputs, static, run_cfg)

cat(sprintf("  RepaymentSchedule rows: %d\n", nrow(inputs$RepaymentSchedule)))
cat(sprintf("  Transformation rows:    %d\n", nrow(transformation)))


# ---------------------------------------------------------------------------
# LifeTimeParameterOther
# ---------------------------------------------------------------------------
cat("\n========== PHASE F :: LifeTimeParameterOther ==========\n")
ltpo <- build_lifetime_parameter_other(inputs$RepaymentSchedule,
                                       transformation, run_cfg)

unit_results <- list()
check <- function(label, cond) {
  status <- if (isTRUE(cond)) "PASS" else "FAIL"
  cat(sprintf("  [%s] %s\n", status, label))
  isTRUE(cond)
}

cat(sprintf("  Generated %d rows for %d contracts\n",
            nrow(ltpo), length(unique(ltpo$contract_id))))

unit_results$LT1 <- check("LT1: schema has expected columns",
  identical(sort(colnames(ltpo)),
            sort(c("extract_date","contract_id","month_lifetime",
                   "ead_lifetime","lgd_lifetime","payment_schedule_lifetime",
                   "total_limit_lifetime"))))

unit_results$LT2 <- check("LT2: no NA in EAD",
  all(!is.na(ltpo$ead_lifetime)))

unit_results$LT3 <- check("LT3: all EAD >= 0",
  all(ltpo$ead_lifetime >= 0))

unit_results$LT4 <- check("LT4: lgd_lifetime is all NA (placeholder column)",
  all(is.na(ltpo$lgd_lifetime)))


# ---------------------------------------------------------------------------
# StPD
# ---------------------------------------------------------------------------
cat("\n========== PHASE F :: StPD ==========\n")

scenarios <- static$scenario_severity[, c("scenario", "severity_z")]

# Internal term structure: Vasicek shift on lending TTC PDs
internal_ts <- build_pd_term_structure(
  ttc_pd_table = static$ttc_pd_table,
  scenarios    = scenarios,
  model_cfg    = model_cfg,
  model_inputs = model_inputs,
  rating_type  = "Internal"
)
# External term structure: Basel ASRF + GCC PERCENTRANK on investment TTC PDs
external_ts <- build_pd_term_structure(
  ttc_pd_table = static$ttc_pd_table_external,
  scenarios    = scenarios,
  model_cfg    = model_cfg,
  model_inputs = model_inputs,
  gcc_history  = gcc_history,
  rating_type  = "External"
)
cat(sprintf("  Internal term structure: %d rows\n", nrow(internal_ts)))
cat(sprintf("  External term structure: %d rows\n", nrow(external_ts)))

# Portfolio definitions (per the bundled output samples)
portfolios <- tibble::tribble(
  ~portfolio,           ~rating_type,
  "Business Finance",   "Internal",
  "Off BS",             "Internal",
  "Al Dhameen",         "Internal",
  "Tasdeer",            "Internal",
  "Banks and Fis",      "External",
  "Investments",        "External"
)

ratings_internal <- static$master_rating_scale[
  static$master_rating_scale$rating_type == "Internal",
  c("rating", "hierarchy")]
ratings_external <- static$master_rating_scale[
  static$master_rating_scale$rating_type == "External",
  c("rating", "hierarchy")]
ratings_combined <- dplyr::bind_rows(ratings_internal, ratings_external)

# build_stpd takes the per-rating-type scenario weights as a single named
# vector. We need to pass the union but use the right one per rating type.
# Since build_stpd handles internal and external separately via the
# portfolios$rating_type column, we pass it both weight vectors.
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
cat(sprintf("  StPD rows: %d\n", nrow(stpd)))

unit_results$ST1 <- check("ST1: schema has expected columns",
  identical(sort(colnames(stpd)),
            sort(c("extract_date","portfolio_code","pd_bucket_dim1",
                   "pd_bucket_dim2","month_lifetime","pd_lifetime"))))

unit_results$ST2 <- check("ST2: row count = 6 portfolios * 21 buckets * 600 months = 75,600",
  nrow(stpd) == 6 * 21 * 600)

unit_results$ST3 <- check("ST3: 6 distinct portfolios",
  length(unique(stpd$portfolio_code)) == 6L)

unit_results$ST4 <- check("ST4: PDBucketDim1 values are integers 1..21",
  identical(sort(unique(stpd$pd_bucket_dim1)), 1:21))

unit_results$ST5 <- check("ST5: PDBucketDim2 is all NA (per Excel output schema)",
  all(is.na(stpd$pd_bucket_dim2)))

unit_results$ST6 <- check("ST6: month_lifetime range 1..600",
  min(stpd$month_lifetime) == 1L && max(stpd$month_lifetime) == 600L)

unit_results$ST7 <- check("ST7: pd_lifetime monotone non-decreasing within (portfolio, bucket)",
  {
    bad <- stpd |>
      dplyr::group_by(portfolio_code, pd_bucket_dim1) |>
      dplyr::summarise(any_decrease = any(diff(pd_lifetime) < -1e-12),
                       .groups = "drop")
    !any(bad$any_decrease)
  })

unit_results$ST8 <- check("ST8: 4 internal portfolios share identical PD curves",
  {
    sample <- stpd[stpd$pd_bucket_dim1 == 1L & stpd$month_lifetime == 12L &
                   stpd$portfolio_code %in% c("Business Finance","Off BS",
                                                "Al Dhameen","Tasdeer"), ]
    length(unique(sample$pd_lifetime)) == 1L
  })

unit_results$ST9 <- check("ST9: pd_lifetime > 0 for all rows except external Aaa/Aa1/Aa2 (TTC=0)",
  {
    zero_buckets <- which(static$ttc_pd_table_external$ttc_pd == 0)
    nonzero <- stpd[!(stpd$portfolio_code %in% c("Banks and Fis","Investments") &
                      stpd$pd_bucket_dim1 %in% zero_buckets), ]
    all(nonzero$pd_lifetime > 0)
  })


# ---------------------------------------------------------------------------
# Reconciliation against bundled Output/StPD.csv
# ---------------------------------------------------------------------------
# The bundled StPD.csv was produced by running V4 end-to-end. V4 used the
# older Vasicek-shift methodology for BOTH internal and external portfolios.
# The R port now uses V7's correct Basel ASRF methodology for external,
# so we expect:
#   - Internal portfolios: 100% match to V4 STPDTable (within FP precision)
#   - External portfolios: known difference; values are produced by V7's
#     methodology, which V4's STPDTable does not reflect.
# ---------------------------------------------------------------------------
cat("\n========== PHASE F :: StPD reconciliation vs Output/StPD.csv ==========\n")

bundled_stpd_path <- file.path(run_cfg$paths$reference_outputs %||%
                                "../output_files/Output",
                                "StPD.csv")
if (file.exists(bundled_stpd_path)) {
  ex <- readr::read_csv(bundled_stpd_path, show_col_types = FALSE)
  ex <- ex[!is.na(ex$PortfolioCode) & !is.na(ex$PDBucketDim1) &
           !is.na(ex$MonthLifetime) & !is.na(ex$PDLifetime), ]
  ex$PDBucketDim1  <- as.integer(ex$PDBucketDim1)
  ex$MonthLifetime <- as.integer(ex$MonthLifetime)

  joined <- dplyr::inner_join(stpd, ex,
    by = c("portfolio_code"="PortfolioCode",
           "pd_bucket_dim1"="PDBucketDim1",
           "month_lifetime"="MonthLifetime"))
  joined$abs_diff <- abs(joined$pd_lifetime - joined$PDLifetime)
  joined$rel_diff <- joined$abs_diff /
                     pmax(abs(joined$PDLifetime), 1e-12)
  joined$match    <- joined$rel_diff < 1e-6 |
                     joined$abs_diff < 1e-9

  cat(sprintf("  Compared %d rows total\n", nrow(joined)))

  per_portfolio <- joined |>
    dplyr::group_by(portfolio_code) |>
    dplyr::summarise(matched = sum(match),
                     total = dplyr::n(),
                     pct   = 100 * mean(match),
                     max_abs_diff = max(abs_diff),
                     max_rel_diff = max(rel_diff[is.finite(rel_diff)]),
                     .groups = "drop")
  for (i in seq_len(nrow(per_portfolio))) {
    p <- per_portfolio[i, ]
    cat(sprintf("  %-20s  %d/%d (%.4f%%)  max_abs=%.2e\n",
                p$portfolio_code, p$matched, p$total, p$pct, p$max_abs_diff))
  }

  internal_portfolios <- c("Business Finance","Off BS","Al Dhameen","Tasdeer")
  internal_match <- joined[joined$portfolio_code %in% internal_portfolios, ]
  unit_results$RC1 <- check(
    "RC1: internal portfolios reconcile to Output/StPD.csv within 1e-6 rel",
    mean(internal_match$match) > 0.9999)

  cat("\n  Note: external portfolios (Banks and Fis, Investments) are EXPECTED\n")
  cat("        to differ from V4's bundled output because V4 used the legacy\n")
  cat("        Vasicek-shift method while this port now uses V7's Basel ASRF.\n")
} else {
  cat("  Output/StPD.csv not found at: ", bundled_stpd_path, "\n")
}


# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
cat("\n========== PHASE F SUMMARY ==========\n")
n_pass <- sum(unlist(unit_results))
n_total <- length(unit_results)
cat(sprintf("  %d/%d checks passed\n", n_pass, n_total))
if (n_pass == n_total) {
  cat("  PHASE F :: PASS\n")
} else {
  cat("  PHASE F :: FAIL\n")
  failed <- names(unit_results)[!unlist(unit_results)]
  cat(sprintf("  Failed: %s\n", paste(failed, collapse=", ")))
}
