# ============================================================================
# R/build_stpd.R
#
# Build the StPD output table — one row per
# (portfolio, rating_hierarchy, month_lifetime).
#
# Replicates the Excel `STPDTable` sheet logic, with V7 production methodology:
#
#   1. Take per-scenario annual marginal PD term structures (one for internal
#      ratings using Vasicek; one for external ratings using Basel ASRF +
#      PERCENTRANK.EXC of GCC GDP).
#   2. Apply DIFFERENT scenario weights to each:
#        - Internal:  internal_scenario_weights (auto_normal_cdf or explicit)
#        - External:  external_scenario_weights (auto_gcc_cdf or explicit)
#   3. Convert annual to monthly:
#        monthly_marg[r, m] = weighted_pd[r, ceil(m/12)] / 12
#   4. Cumulative sum:
#        pd_cumulative[r, m] = sum_{i=1..m} monthly_marg[r, i]
#   5. Replicate per portfolio. The 6 portfolios are:
#        Internal-rated:  Business Finance, Off BS, Al Dhameen, Tasdeer
#        External-rated:  Banks and Fis, Investments
#
# Public API:
#   apply_scenario_weights(annual_term_structure, scenario_weights)
#     -> tibble (rating, maturity, weighted_marginal_pd)
#   convert_to_monthly_stpd(weighted_annual, max_month)
#     -> tibble (rating, month_lifetime, monthly_marginal_pd, pd_lifetime)
#   build_stpd(internal_term_structure, external_term_structure,
#              internal_scenario_weights, external_scenario_weights,
#              portfolios, ratings, run_cfg, max_month = 600)
#     -> tibble (extract_date, portfolio_code, pd_bucket_dim1,
#                pd_bucket_dim2, month_lifetime, pd_lifetime)
# ============================================================================


#' Apply per-scenario weights to a long-format annual PD term structure.
#'
#' Replicates the V4 internal formula at Calculations!K252:
#'   = AE5*K137 + AE6*K160 + AE7*K183 + AE8*K206 + AE9*K229
#' where AE5..AE9 is the SAME single weight vector for every maturity column.
#'
#' For external rated portfolios, V7 applies DIFFERENT weights per year
#' (see apply_scenario_weights_per_year() below).
#'
#' @param annual_term_structure tibble — output of build_pd_term_structure()
#'        with cols (rating, scenario, maturity, marginal_pd, cumulative_pd).
#' @param scenario_weights named numeric vector, names = scenarios. Same
#'        weights applied to every maturity.
#' @return tibble (rating, maturity, weighted_marginal_pd)
apply_scenario_weights <- function(annual_term_structure, scenario_weights) {
  ts <- annual_term_structure
  scenarios_in_ts <- unique(ts$scenario)
  missing <- setdiff(scenarios_in_ts, names(scenario_weights))
  if (length(missing) > 0) {
    stop("scenario_weights is missing entries for: ",
         paste(missing, collapse = ", "))
  }
  ts$weight  <- as.numeric(scenario_weights[ts$scenario])
  ts$contrib <- ts$marginal_pd * ts$weight
  agg <- aggregate(contrib ~ rating + maturity, data = ts, FUN = sum)
  names(agg)[3] <- "weighted_marginal_pd"
  tibble::as_tibble(agg)
}


#' Apply YEAR-SPECIFIC scenario weights to an annual PD term structure.
#'
#' Replicates the V7 external formula pattern in Calculations columns
#' BM..DJ rows 242..272:
#'
#'   year 1 (col BM):    weights = D69:H69  (year-1 weights)
#'   year 2 (col BN):    weights = D70:H70  (year-2 weights)
#'   year 3 (col BO):    weights = D71:H71  (year-3 weights)
#'   year 4 (col BP):    weights = D72:H72  (year-4 weights)
#'   year 5 (col BQ):    weights = D73:H73  (year-5 weights)
#'   year 6+ (cols BR..DJ): weights = D74:H74 = AVERAGE(D69:D73) etc.
#'
#' @param annual_term_structure tibble (rating, scenario, maturity, marginal_pd, ...)
#' @param weights_per_year matrix [n_forecast_years × n_scenarios]
#' @param weights_average  named numeric vector — applied to maturities
#'                         > n_forecast_years
#' @return tibble (rating, maturity, weighted_marginal_pd)
apply_scenario_weights_per_year <- function(annual_term_structure,
                                              weights_per_year,
                                              weights_average) {
  ts <- annual_term_structure
  scenarios_in_ts <- unique(ts$scenario)
  missing <- setdiff(scenarios_in_ts, colnames(weights_per_year))
  if (length(missing) > 0) {
    stop("weights_per_year is missing entries for: ",
         paste(missing, collapse = ", "))
  }
  n_forecast_years <- nrow(weights_per_year)

  # Build a (maturity x scenario) weight lookup matrix
  max_mat <- max(ts$maturity)
  W <- matrix(NA_real_, nrow = max_mat, ncol = ncol(weights_per_year),
              dimnames = list(NULL, colnames(weights_per_year)))
  for (m in seq_len(max_mat)) {
    if (m <= n_forecast_years) {
      W[m, ] <- weights_per_year[m, ]
    } else {
      W[m, ] <- weights_average[colnames(weights_per_year)]
    }
  }

  ts$weight  <- W[cbind(ts$maturity, match(ts$scenario, colnames(W)))]
  ts$contrib <- ts$marginal_pd * ts$weight
  agg <- aggregate(contrib ~ rating + maturity, data = ts, FUN = sum)
  names(agg)[3] <- "weighted_marginal_pd"
  tibble::as_tibble(agg)
}


#' Convert an annual scenario-weighted PD curve to a monthly cumulative curve.
#'
#' Per Excel STPDTable!F4 formula:
#'   monthly_marginal[m] = annual_marginal[ceil(m/12)] / 12
#'   pd_lifetime[m]      = sum_{i=1..m} monthly_marginal[i]
#'
#' The simple-cumsum methodology can produce values slightly above 1.0 because:
#'   (a) V4 explicit weights have a transcription typo (sum=1.0003 vs 1.0).
#'   (b) V7 per-year external weights vary year-to-year, so the cumsum across
#'       scenarios isn't bounded by the per-year weight-row sum.
#' Both are documented in model_inputs.yml. PDs are probabilities by
#' definition, so we cap at 1.0 here. The cap loses ~0.03%-1.0% relative to
#' the unclamped cumsum for low-rated long-maturity buckets, but produces
#' valid probabilities suitable for IFRS9 ECL downstream.
#'
#' @section TODO — methodology review (deferred):
#' The workbook uses `=SUM(...)` (simple cumsum) for `STPDTable` columns,
#' which is the first-order approximation of the survival formula
#' `=1 - PRODUCT(1 - ...)`. The two agree for small marginals but diverge
#' for low-rated long-maturity buckets where annual marginals are large.
#' The survival formula `1 - cumprod(1 - monthly_marg)` is mathematically
#' correct (PDs strictly in [0,1] without needing a cap) and is what most
#' credit-risk literature uses. Decision deferred:
#'   - Current: cumsum + cap at 1.0 (matches V4 workbook formula faithfully)
#'   - Future option: replace cumsum with `1 - cumprod(1 - monthly_marg)`
#' Cross-check the QDB IFRS9 Macroeconomic Variable Models PDF for the
#' definitive lifetime-PD specification before changing.
#'
#' @param weighted_annual tibble (rating, maturity, weighted_marginal_pd) where
#'        maturity is in YEARS.
#' @param max_month maximum month to produce (e.g. 600 = 50 years × 12)
#' @param pd_cap upper bound for pd_lifetime (default 1.0). Set to Inf to
#'        replicate V4's unclamped cumsum exactly.
#' @return tibble (rating, month_lifetime, monthly_marginal_pd, pd_lifetime)
convert_to_monthly_stpd <- function(weighted_annual, max_month, pd_cap = 1.0) {
  ratings <- unique(weighted_annual$rating)
  out_chunks <- vector("list", length(ratings))

  for (i in seq_along(ratings)) {
    r <- ratings[i]
    sub <- weighted_annual[weighted_annual$rating == r, ]
    sub <- sub[order(sub$maturity), ]
    annual_marg <- sub$weighted_marginal_pd

    months   <- seq_len(max_month)
    year_idx <- ceiling(months / 12)
    year_idx[year_idx > length(annual_marg)] <- length(annual_marg)

    monthly_marg <- annual_marg[year_idx] / 12
    cum <- cumsum(monthly_marg)
    cum <- pmin(cum, pd_cap)

    out_chunks[[i]] <- tibble::tibble(
      rating              = r,
      month_lifetime      = as.integer(months),
      monthly_marginal_pd = monthly_marg,
      pd_lifetime         = cum
    )
  }
  dplyr::bind_rows(out_chunks)
}


#' Build the StPD output table.
#'
#' Combines the internal and external term structures, applying DIFFERENT
#' scenario weights to each per V7 production methodology, then replicates
#' the resulting curves per portfolio.
#'
#' @param internal_term_structure tibble — output of build_pd_term_structure(
#'        rating_type="Internal").
#' @param external_term_structure tibble — output of build_pd_term_structure(
#'        rating_type="External"). May be NULL, in which case external
#'        portfolios will use the internal curve as a placeholder.
#' @param internal_scenario_weights named numeric vector — per-scenario weights
#'        applied to internal term structure (typically auto_normal_cdf).
#' @param external_scenario_weights named numeric vector — per-scenario weights
#'        applied to external term structure (typically auto_gcc_cdf). Required
#'        only when external_term_structure is non-NULL.
#' @param portfolios tibble (portfolio, rating_type) — maps each portfolio to
#'        either "Internal" or "External" rating type.
#' @param ratings tibble (rating, hierarchy) — maps rating names to
#'        PDBucketDim1 hierarchy values.
#' @param run_cfg output of load_run_config()
#' @param max_month maximum month to produce (default 600 = 50 years * 12)
#' @return tibble (extract_date, portfolio_code, pd_bucket_dim1,
#'         pd_bucket_dim2, month_lifetime, pd_lifetime)
build_stpd <- function(internal_term_structure,
                       external_term_structure,
                       internal_scenario_weights,
                       external_scenario_weights = NULL,
                       portfolios,
                       ratings,
                       run_cfg,
                       max_month = 600) {

  reporting_dt <- normalise_extract_date(run_cfg$run$extract_date)

  # --- Internal: apply internal_scenario_weights ---
  weighted_int <- apply_scenario_weights(internal_term_structure,
                                          internal_scenario_weights)
  monthly_int  <- convert_to_monthly_stpd(weighted_int, max_month)

  # --- External: per-year weights for years 1..N, average for years N+1.. ---
  if (is.null(external_term_structure)) {
    monthly_ext <- monthly_int   # placeholder
  } else {
    if (is.null(external_scenario_weights)) {
      stop("external_scenario_weights is required when external_term_structure is supplied")
    }
    if (!is.list(external_scenario_weights) ||
         is.null(external_scenario_weights$per_year) ||
         is.null(external_scenario_weights$average)) {
      stop("external_scenario_weights must be a list with $per_year and $average components")
    }
    weighted_ext <- apply_scenario_weights_per_year(
      external_term_structure,
      weights_per_year = external_scenario_weights$per_year,
      weights_average  = external_scenario_weights$average)
    monthly_ext <- convert_to_monthly_stpd(weighted_ext, max_month)
  }

  # --- Map rating to hierarchy ---
  hierarchy_lkp <- setNames(as.integer(ratings$hierarchy), ratings$rating)

  out_chunks <- vector("list", nrow(portfolios))

  for (i in seq_len(nrow(portfolios))) {
    portfolio  <- portfolios$portfolio[i]
    rating_typ <- portfolios$rating_type[i]
    monthly    <- if (rating_typ == "Internal") monthly_int else monthly_ext

    monthly$pd_bucket_dim1 <- hierarchy_lkp[monthly$rating]
    monthly <- monthly[!is.na(monthly$pd_bucket_dim1), ]

    out_chunks[[i]] <- tibble::tibble(
      extract_date    = reporting_dt,
      portfolio_code  = portfolio,
      pd_bucket_dim1  = as.integer(monthly$pd_bucket_dim1),
      pd_bucket_dim2  = NA_integer_,
      month_lifetime  = monthly$month_lifetime,
      pd_lifetime     = monthly$pd_lifetime
    )
  }

  out <- dplyr::bind_rows(out_chunks)
  out <- out[order(out$portfolio_code, out$pd_bucket_dim1, out$month_lifetime), ]
  out
}
