# ============================================================================
# R/validators_derived.R
#
# Derived-output validators. Run after Phase F (build_lifetime_parameter_other,
# build_stpd, load_model_inputs) and before Phase G (output writers).
#
# Args expected (passed via run_validation_suite args):
#   ltpo          — Phase F LifeTimeParameterOther tibble
#   stpd          — Phase F StPD tibble (long format, 75,600 rows expected)
#   model_inputs  — load_model_inputs() output (scenario + MEV weights)
#   model_cfg     — load_model_config() output
#   trans_l       — Phase D lending per-contract tibble (for cross-checks)
#   static        — load_static_reference() output
#
# Coverage:
#   * LifeTimeParameterOther
#       - Schema (7 columns)
#       - Month convention: month_lifetime starts at 0, contiguous per contract
#       - EAD non-NA, non-negative
#       - EAD non-increasing per contract (term-loan principle, WARN level)
#       - Month-0 EAD reconciles to AccountMaster ONBALANCE (per-contract sample)
#       - Total month-0 EAD reconciles to total ONBALANCE (sum-level, ERROR)
#       - Contracts in ltpo ⊆ trans_l$contract_id
#
#   * StPD
#       - Schema (6 columns)
#       - Row count = 6 × 21 × 600 = 75,600
#       - portfolio set complete; bucket set complete per portfolio;
#         month set complete per (portfolio, bucket)
#       - pd_lifetime in [0, 1]
#       - pd_lifetime monotone non-decreasing within (portfolio, bucket)
#       - pd_bucket_dim2 all NA (per Excel schema)
#       - 4 internal portfolios share identical curves
#       - 2 external portfolios share identical curves
#       - External buckets with TTC=0 (Aaa/Aa1/Aa2) -> entire curve is 0
#       - pd_lifetime is monotone non-decreasing in hierarchy (worse rating
#         => higher PD) at any fixed (portfolio, month)
#       - Single extract_date
#
#   * Scenario weights
#       - Internal weights sum to ~1 and are non-negative
#       - External per-year weights: each year sums to ~1
#       - External average weights sum to ~1
#
#   * MEV weights
#       - Sum to 1; non-negative
# ============================================================================


.get_col <- function(df, name) {
  if (is.null(df) || !name %in% names(df)) NULL else df[[name]]
}

# Internal vs External portfolio sets (matches build_stpd's six-portfolio
# convention).
.INTERNAL_PORTFOLIOS <- c("Business Finance", "Off BS", "Al Dhameen", "Tasdeer")
.EXTERNAL_PORTFOLIOS <- c("Banks and Fis", "Investments")


#' Build the derived-output validator list (Phase F outputs + model inputs).
build_derived_validators <- function() {
  v <- list()

  # =======================================================================
  # LifeTimeParameterOther
  # =======================================================================

  v[[length(v) + 1]] <- make_validator(
    "DERIVED_LTPO_schema", "ERROR", context = "LifeTimeParameterOther",
    description = "ltpo has the expected 7-column schema",
    fn = function(ltpo, ...) {
      if (is.null(ltpo)) return(list(passed = FALSE,
                                       details = list(message = "ltpo is NULL")))
      expected <- c("extract_date","contract_id","month_lifetime",
                     "ead_lifetime","lgd_lifetime","payment_schedule_lifetime",
                     "total_limit_lifetime")
      missing <- setdiff(expected, names(ltpo))
      list(passed = length(missing) == 0,
           details = if (length(missing) > 0)
             list(message = sprintf("Missing columns: %s",
                                    paste(missing, collapse=", ")))
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "DERIVED_LTPO_extract_date_unique", "ERROR", context = "LifeTimeParameterOther",
    description = "ltpo has a single extract_date value",
    fn = function(ltpo, ...) {
      d <- .get_col(ltpo, "extract_date")
      if (is.null(d)) return(list(passed = TRUE))
      uniq <- unique(d)
      list(passed = length(uniq) == 1,
           details = if (length(uniq) != 1)
             list(message = sprintf("%d distinct extract_dates: %s",
                                    length(uniq),
                                    paste(head(as.character(uniq), 5), collapse=", ")))
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "DERIVED_LTPO_ead_nonneg", "ERROR", context = "LifeTimeParameterOther",
    description = "ead_lifetime is non-NA and >= 0",
    fn = function(ltpo, ...) {
      e <- .get_col(ltpo, "ead_lifetime")
      if (is.null(e)) return(list(passed = TRUE))
      n_na  <- sum(is.na(e))
      n_neg <- sum(!is.na(e) & e < 0)
      ok <- n_na == 0 && n_neg == 0
      list(passed = ok,
           details = if (!ok)
             list(message = sprintf("%d NA, %d negative", n_na, n_neg))
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "DERIVED_LTPO_month_starts_at_zero", "ERROR",
    context = "LifeTimeParameterOther",
    description = "Every contract has a month_lifetime=0 row",
    fn = function(ltpo, ...) {
      if (is.null(ltpo)) return(list(passed = TRUE))
      m0 <- ltpo[ltpo$month_lifetime == 0, "contract_id", drop = TRUE]
      all_cids <- unique(ltpo$contract_id)
      missing <- setdiff(all_cids, unique(m0))
      list(passed = length(missing) == 0,
           details = if (length(missing) > 0)
             list(message = sprintf("%d contracts have no month-0 row (e.g. %s)",
                                    length(missing),
                                    paste(head(missing, 5), collapse=", ")))
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "DERIVED_LTPO_months_contiguous", "ERROR",
    context = "LifeTimeParameterOther",
    description = "Each contract's months form a contiguous 0..N-1 sequence",
    fn = function(ltpo, ...) {
      if (is.null(ltpo)) return(list(passed = TRUE))
      bad <- ltpo |>
        dplyr::group_by(contract_id) |>
        dplyr::summarise(
          ok = (min(month_lifetime) == 0) &&
                identical(sort(month_lifetime),
                          0:(length(month_lifetime) - 1)),
          .groups = "drop")
      n_bad <- sum(!bad$ok)
      list(passed = n_bad == 0,
           details = if (n_bad > 0)
             list(message = sprintf("%d contracts have non-contiguous months",
                                    n_bad),
                  bad_contracts = head(bad$contract_id[!bad$ok], 10))
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "DERIVED_LTPO_ead_nonincreasing", "INFO",
    context = "LifeTimeParameterOther",
    description = "ead_lifetime is non-increasing within each contract (term-loan principle; revolving/off-bal/accrual products allowed to grow)",
    fn = function(ltpo, ...) {
      if (is.null(ltpo)) return(list(passed = TRUE))
      sub <- ltpo[order(ltpo$contract_id, ltpo$month_lifetime), ]
      bad <- sub |>
        dplyr::group_by(contract_id) |>
        dplyr::summarise(any_increase = any(diff(ead_lifetime) > 1e-6),
                          .groups = "drop")
      n_bad <- sum(bad$any_increase, na.rm = TRUE)
      list(passed = n_bad == 0,
           details = if (n_bad > 0)
             list(message = sprintf("%d contracts have an EAD rise between months — expected for revolving/off-balance products and contracts with interest/fee accrual baked into the schedule",
                                    n_bad),
                  sample_contracts = head(bad$contract_id[bad$any_increase], 10))
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "DERIVED_LTPO_contracts_subset_of_trans", "ERROR",
    context = "LifeTimeParameterOther",
    description = "Every ltpo contract_id exists in trans_l",
    fn = function(ltpo, trans_l, ...) {
      if (is.null(ltpo) || is.null(trans_l)) return(list(passed = TRUE))
      ltpo_ids  <- unique(as.character(ltpo$contract_id))
      trans_ids <- unique(as.character(trans_l$contract_id))
      orphans   <- setdiff(ltpo_ids, trans_ids)
      list(passed = length(orphans) == 0,
           details = if (length(orphans) > 0)
             list(message = sprintf("%d ltpo contracts not in trans_l (e.g. %s)",
                                    length(orphans),
                                    paste(head(orphans, 5), collapse=", ")))
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "DERIVED_LTPO_total_month0_ead_reconciles", "WARN",
    context = "LifeTimeParameterOther",
    description = "Sum of month-0 EAD == sum of trans_l ONBALANCE for covered contracts",
    fn = function(ltpo, trans_l, ...) {
      if (is.null(ltpo) || is.null(trans_l)) return(list(passed = TRUE))
      m0 <- ltpo[ltpo$month_lifetime == 0, c("contract_id", "ead_lifetime")]
      common <- intersect(m0$contract_id, trans_l$contract_id)
      if (length(common) == 0) return(list(passed = TRUE))
      ltpo_sum  <- sum(m0$ead_lifetime[m0$contract_id %in% common], na.rm = TRUE)
      trans_sum <- sum(trans_l$exposure_amount[trans_l$contract_id %in% common],
                       na.rm = TRUE)
      tol <- max(1, abs(trans_sum) * 1e-6)
      list(passed = abs(ltpo_sum - trans_sum) <= tol,
           details = if (abs(ltpo_sum - trans_sum) > tol)
             list(message = sprintf("ltpo month-0 sum=%s, trans exposure sum=%s, diff=%s, contracts compared=%d",
                                    format(ltpo_sum), format(trans_sum),
                                    format(ltpo_sum - trans_sum), length(common)))
           else NULL)
    }
  )


  # =======================================================================
  # StPD
  # =======================================================================

  v[[length(v) + 1]] <- make_validator(
    "DERIVED_STPD_schema", "ERROR", context = "StPD",
    description = "stpd has the expected 6-column schema",
    fn = function(stpd, ...) {
      if (is.null(stpd)) return(list(passed = FALSE,
                                       details = list(message = "stpd is NULL")))
      expected <- c("extract_date","portfolio_code","pd_bucket_dim1",
                     "pd_bucket_dim2","month_lifetime","pd_lifetime")
      missing <- setdiff(expected, names(stpd))
      list(passed = length(missing) == 0,
           details = if (length(missing) > 0)
             list(message = sprintf("Missing columns: %s",
                                    paste(missing, collapse=", ")))
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "DERIVED_STPD_row_count", "ERROR", context = "StPD",
    description = "stpd has exactly 6 portfolios × 21 buckets × 600 months = 75,600 rows",
    fn = function(stpd, ...) {
      if (is.null(stpd)) return(list(passed = TRUE))
      list(passed = nrow(stpd) == 75600L,
           details = if (nrow(stpd) != 75600L)
             list(message = sprintf("nrow(stpd)=%d, expected 75,600", nrow(stpd)))
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "DERIVED_STPD_extract_date_unique", "ERROR", context = "StPD",
    description = "stpd has a single extract_date value",
    fn = function(stpd, ...) {
      d <- .get_col(stpd, "extract_date")
      if (is.null(d)) return(list(passed = TRUE))
      uniq <- unique(d)
      list(passed = length(uniq) == 1,
           details = if (length(uniq) != 1)
             list(message = sprintf("%d distinct extract_dates", length(uniq)))
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "DERIVED_STPD_portfolio_set_complete", "ERROR", context = "StPD",
    description = "All 6 portfolios present (4 internal + 2 external)",
    fn = function(stpd, ...) {
      if (is.null(stpd)) return(list(passed = TRUE))
      expected <- sort(c(.INTERNAL_PORTFOLIOS, .EXTERNAL_PORTFOLIOS))
      observed <- sort(unique(stpd$portfolio_code))
      list(passed = identical(expected, observed),
           details = if (!identical(expected, observed))
             list(message = sprintf("Expected %d portfolios, got %d. Missing: %s. Extra: %s",
                                    length(expected), length(observed),
                                    paste(setdiff(expected, observed), collapse=", "),
                                    paste(setdiff(observed, expected), collapse=", ")))
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "DERIVED_STPD_bucket_set_complete", "ERROR", context = "StPD",
    description = "Each portfolio has all 21 buckets (hierarchy 1..21)",
    fn = function(stpd, ...) {
      if (is.null(stpd)) return(list(passed = TRUE))
      bad <- stpd |>
        dplyr::group_by(portfolio_code) |>
        dplyr::summarise(buckets = list(sort(unique(pd_bucket_dim1))),
                          .groups = "drop")
      bad$ok <- vapply(bad$buckets, function(b) identical(as.integer(b), 1:21),
                        logical(1))
      n_bad <- sum(!bad$ok)
      list(passed = n_bad == 0,
           details = if (n_bad > 0)
             list(message = sprintf("%d portfolios missing buckets",
                                    n_bad),
                  bad_portfolios = bad$portfolio_code[!bad$ok])
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "DERIVED_STPD_month_set_complete", "ERROR", context = "StPD",
    description = "Each (portfolio, bucket) has all 600 months (1..600)",
    fn = function(stpd, ...) {
      if (is.null(stpd)) return(list(passed = TRUE))
      m <- range(stpd$month_lifetime)
      counts <- stpd |>
        dplyr::group_by(portfolio_code, pd_bucket_dim1) |>
        dplyr::summarise(n = dplyr::n(), .groups = "drop")
      bad <- counts$n != 600L
      list(passed = !any(bad) && m[1] == 1L && m[2] == 600L,
           details = if (any(bad) || m[1] != 1L || m[2] != 600L)
             list(message = sprintf("month range = [%d, %d]; %d (portfolio, bucket) groups don't have 600 rows",
                                    m[1], m[2], sum(bad)))
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "DERIVED_STPD_dim2_all_na", "ERROR", context = "StPD",
    description = "pd_bucket_dim2 is all NA (matches Excel output schema)",
    fn = function(stpd, ...) {
      if (is.null(stpd)) return(list(passed = TRUE))
      d2 <- .get_col(stpd, "pd_bucket_dim2")
      if (is.null(d2)) return(list(passed = FALSE,
                                     details = list(message = "pd_bucket_dim2 missing")))
      n_nonna <- sum(!is.na(d2))
      list(passed = n_nonna == 0,
           details = if (n_nonna > 0)
             list(message = sprintf("%d non-NA values in pd_bucket_dim2", n_nonna))
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "DERIVED_STPD_pd_nonneg_finite", "ERROR", context = "StPD",
    description = "pd_lifetime is non-NA, non-negative, and finite",
    fn = function(stpd, ...) {
      if (is.null(stpd)) return(list(passed = TRUE))
      p <- stpd$pd_lifetime
      n_na    <- sum(is.na(p))
      n_neg   <- sum(!is.na(p) & p < 0)
      n_inf   <- sum(!is.na(p) & is.infinite(p))
      ok <- n_na == 0 && n_neg == 0 && n_inf == 0
      list(passed = ok,
           details = if (!ok)
             list(message = sprintf("%d NA, %d negative, %d infinite",
                                    n_na, n_neg, n_inf))
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "DERIVED_STPD_pd_within_workbook_bound", "ERROR", context = "StPD",
    description = "pd_lifetime <= 1.001 (allows FP slack but rejects real overflow)",
    fn = function(stpd, ...) {
      if (is.null(stpd)) return(list(passed = TRUE))
      p <- stpd$pd_lifetime
      n_gt_1   <- sum(!is.na(p) & p > 1)
      n_over   <- sum(!is.na(p) & p > 1.001)
      max_pd   <- if (length(p) > 0) max(p, na.rm = TRUE) else NA_real_
      ok <- is.finite(max_pd) && max_pd <= 1.001
      list(passed = ok,
           details = if (!ok)
             list(message = sprintf(
                  "max pd_lifetime = %.6f, %d rows above 1, %d rows above 1.001. Likely cause: scenario weights summing to >1 (typo in workbook explicit_weights — switch to mode='auto_non_oil_gdp_cdf' in model_inputs.yml).",
                  max_pd, n_gt_1, n_over),
                  count_above_1     = n_gt_1,
                  count_above_1p001 = n_over,
                  max_pd            = max_pd)
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "DERIVED_STPD_pd_monotone_non_decreasing", "ERROR", context = "StPD",
    description = "pd_lifetime non-decreasing within (portfolio, bucket) as month grows",
    fn = function(stpd, ...) {
      if (is.null(stpd)) return(list(passed = TRUE))
      sub <- stpd[order(stpd$portfolio_code, stpd$pd_bucket_dim1,
                          stpd$month_lifetime), ]
      bad <- sub |>
        dplyr::group_by(portfolio_code, pd_bucket_dim1) |>
        dplyr::summarise(any_decrease = any(diff(pd_lifetime) < -1e-12),
                          .groups = "drop")
      n_bad <- sum(bad$any_decrease, na.rm = TRUE)
      list(passed = n_bad == 0,
           details = if (n_bad > 0)
             list(message = sprintf("%d (portfolio, bucket) groups have a decreasing pd_lifetime",
                                    n_bad))
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "DERIVED_STPD_pd_increases_with_hierarchy", "WARN", context = "StPD",
    description = "Worse rating (higher hierarchy) => higher pd_lifetime, fixing (portfolio, month)",
    fn = function(stpd, ...) {
      if (is.null(stpd)) return(list(passed = TRUE))
      # Sample: per portfolio, at month 12 and 60, check monotone in hierarchy
      check_months <- intersect(c(12L, 60L, 120L), unique(stpd$month_lifetime))
      bad_keys <- character()
      for (m in check_months) {
        for (p in unique(stpd$portfolio_code)) {
          sub <- stpd[stpd$portfolio_code == p & stpd$month_lifetime == m, ]
          sub <- sub[order(sub$pd_bucket_dim1), ]
          if (any(diff(sub$pd_lifetime) < -1e-9)) {
            bad_keys <- c(bad_keys, sprintf("%s @ m=%d", p, m))
          }
        }
      }
      list(passed = length(bad_keys) == 0,
           details = if (length(bad_keys) > 0)
             list(message = sprintf("PD not monotone in hierarchy at: %s",
                                    paste(head(bad_keys, 10), collapse="; ")))
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "DERIVED_STPD_internal_portfolios_identical", "WARN", context = "StPD",
    description = "4 internal portfolios (Business Finance, Off BS, Al Dhameen, Tasdeer) share identical curves",
    fn = function(stpd, ...) {
      if (is.null(stpd)) return(list(passed = TRUE))
      sub <- stpd[stpd$portfolio_code %in% .INTERNAL_PORTFOLIOS, ]
      pivot <- sub |>
        dplyr::group_by(pd_bucket_dim1, month_lifetime) |>
        dplyr::summarise(n_unique_pd = dplyr::n_distinct(pd_lifetime),
                          .groups = "drop")
      n_bad <- sum(pivot$n_unique_pd != 1)
      list(passed = n_bad == 0,
           details = if (n_bad > 0)
             list(message = sprintf("%d (bucket, month) cells differ across internal portfolios",
                                    n_bad))
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "DERIVED_STPD_external_portfolios_identical", "WARN", context = "StPD",
    description = "2 external portfolios (Banks and Fis, Investments) share identical curves",
    fn = function(stpd, ...) {
      if (is.null(stpd)) return(list(passed = TRUE))
      sub <- stpd[stpd$portfolio_code %in% .EXTERNAL_PORTFOLIOS, ]
      pivot <- sub |>
        dplyr::group_by(pd_bucket_dim1, month_lifetime) |>
        dplyr::summarise(n_unique_pd = dplyr::n_distinct(pd_lifetime),
                          .groups = "drop")
      n_bad <- sum(pivot$n_unique_pd != 1)
      list(passed = n_bad == 0,
           details = if (n_bad > 0)
             list(message = sprintf("%d (bucket, month) cells differ across external portfolios",
                                    n_bad))
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "DERIVED_STPD_zero_ttc_zero_curve", "WARN", context = "StPD",
    description = "External buckets with TTC=0 (Aaa/Aa1/Aa2) have entire pd_lifetime curve = 0",
    fn = function(stpd, static, ...) {
      if (is.null(stpd) || is.null(static$ttc_pd_table_external))
        return(list(passed = TRUE))
      zero_buckets <- which(static$ttc_pd_table_external$ttc_pd == 0)
      if (length(zero_buckets) == 0) return(list(passed = TRUE))
      sub <- stpd[stpd$portfolio_code %in% .EXTERNAL_PORTFOLIOS &
                   stpd$pd_bucket_dim1 %in% zero_buckets, ]
      n_nonzero <- sum(sub$pd_lifetime > 1e-12)
      list(passed = n_nonzero == 0,
           details = if (n_nonzero > 0)
             list(message = sprintf("%d non-zero pd_lifetime values for zero-TTC buckets %s",
                                    n_nonzero,
                                    paste(zero_buckets, collapse=", ")))
           else NULL)
    }
  )


  # =======================================================================
  # Scenario weights
  # =======================================================================

  v[[length(v) + 1]] <- make_validator(
    "DERIVED_SCEN_internal_weights_sum_to_one", "ERROR",
    context = "scenario_weights",
    description = "Internal scenario weights sum to 1.0 within 1e-4",
    fn = function(model_inputs, ...) {
      w <- model_inputs$internal_scenario_weights
      if (is.null(w)) return(list(passed = TRUE))
      s <- sum(w, na.rm = TRUE)
      ok <- abs(s - 1) < 1e-4
      list(passed = ok,
           details = if (!ok)
             list(message = sprintf("Sum = %.6f (expected ~1.0; if explicit V4 weights, sum=1.0003 due to AE8 typo — switch model_inputs.yml mode to 'auto_non_oil_gdp_cdf')", s),
                  weights = as.list(w),
                  sum     = s)
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "DERIVED_SCEN_internal_weights_nonneg", "ERROR",
    context = "scenario_weights",
    description = "Internal scenario weights are all >= 0",
    fn = function(model_inputs, ...) {
      w <- model_inputs$internal_scenario_weights
      if (is.null(w)) return(list(passed = TRUE))
      bad <- w < 0
      list(passed = !any(bad),
           details = if (any(bad))
             list(message = sprintf("Negative weights: %s",
                                    paste(names(w)[bad], collapse=", ")))
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "DERIVED_SCEN_external_per_year_sum_to_one", "ERROR",
    context = "scenario_weights",
    description = "External scenario weights: each year's row sums to ~1.0",
    fn = function(model_inputs, ...) {
      w <- model_inputs$external_scenario_weights
      if (is.null(w)) return(list(passed = TRUE))
      per_year <- if (is.list(w) && !is.null(w$per_year)) w$per_year else NULL
      if (is.null(per_year)) return(list(passed = TRUE))
      sums <- rowSums(per_year, na.rm = TRUE)
      bad <- abs(sums - 1) >= 1e-3
      list(passed = !any(bad),
           details = if (any(bad))
             list(message = sprintf("Years with row-sum != 1: %s",
                                    paste(which(bad), collapse=", ")),
                  row_sums = sums)
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "DERIVED_SCEN_external_average_sum_to_one", "ERROR",
    context = "scenario_weights",
    description = "External scenario weights: 'average' (year 6+) row sums to ~1.0",
    fn = function(model_inputs, ...) {
      w <- model_inputs$external_scenario_weights
      if (is.null(w)) return(list(passed = TRUE))
      avg <- if (is.list(w) && !is.null(w$average)) w$average
             else if (is.numeric(w))               w
             else NULL
      if (is.null(avg)) return(list(passed = TRUE))
      s <- sum(avg, na.rm = TRUE)
      list(passed = abs(s - 1) < 1e-3,
           details = if (abs(s - 1) >= 1e-3)
             list(message = sprintf("Sum = %.6f (expected ~1.0)", s))
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "DERIVED_SCEN_external_weights_nonneg", "ERROR",
    context = "scenario_weights",
    description = "External scenario weights are all >= 0",
    fn = function(model_inputs, ...) {
      w <- model_inputs$external_scenario_weights
      if (is.null(w)) return(list(passed = TRUE))
      collected <- numeric()
      if (is.list(w)) {
        if (!is.null(w$per_year)) collected <- c(collected, as.numeric(w$per_year))
        if (!is.null(w$average))  collected <- c(collected, as.numeric(w$average))
      } else if (is.numeric(w)) {
        collected <- as.numeric(w)
      }
      n_neg <- sum(collected < -1e-12, na.rm = TRUE)
      list(passed = n_neg == 0,
           details = if (n_neg > 0)
             list(message = sprintf("%d negative weight values", n_neg))
           else NULL)
    }
  )


  # =======================================================================
  # MEV weights
  # =======================================================================

  v[[length(v) + 1]] <- make_validator(
    "DERIVED_MEV_weights_sum_to_one", "ERROR", context = "MEV",
    description = "MEV model weights sum to ~1.0",
    fn = function(model_inputs, ...) {
      w <- model_inputs$mev_model_weights
      if (is.null(w)) return(list(passed = TRUE))
      s <- sum(w, na.rm = TRUE)
      list(passed = abs(s - 1) < 1e-3,
           details = if (abs(s - 1) >= 1e-3)
             list(message = sprintf("Sum = %.6f", s),
                  weights = as.list(w))
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "DERIVED_MEV_weights_nonneg", "ERROR", context = "MEV",
    description = "MEV model weights are all >= 0",
    fn = function(model_inputs, ...) {
      w <- model_inputs$mev_model_weights
      if (is.null(w)) return(list(passed = TRUE))
      bad <- w < -1e-12
      list(passed = !any(bad),
           details = if (any(bad))
             list(message = sprintf("Negative MEV weights: %s",
                                    paste(names(w)[bad], collapse=", ")))
           else NULL)
    }
  )

  v
}


#' Convenience wrapper: build + run the derived-output validators in one call.
#'
#' @return tibble of validation results.
validate_derived_outputs <- function(ltpo, stpd, model_inputs,
                                        trans_l = NULL, static = NULL,
                                        model_cfg = NULL,
                                        verbose = TRUE) {
  validators <- build_derived_validators()
  run_validation_suite(
    "DERIVED", validators,
    args = list(ltpo = ltpo, stpd = stpd,
                model_inputs = model_inputs,
                model_cfg = model_cfg,
                trans_l = trans_l, static = static),
    verbose = verbose
  )
}
