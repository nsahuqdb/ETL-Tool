# ============================================================================
# R/macro_model.R
#
# Macro model engine: replicates the per-cell logic of the Excel
# `Calculations` sheet. Each function below corresponds to one block of the
# sheet so it can be unit-tested in isolation.
#
# Mapping from Calculations sheet to functions:
#   Rows  9-13, cols D-F : stress_mevs()
#   Rows 17-21, cols D-F : compute_logit_pds()
#   Rows 24-28, cols D-F : compute_pds_from_logits()
#   Rows 31-35, cols D-F : compute_per_mev_sf()
#   Row     30, cols K-AD: combine_sf()
#   Row      7, cols K-AD: pit_pd_term_structure()  (Vasicek + log-linear)
#   Rows 33-50, cols K-AD: cumulative_pd()
#   Rows 56-..,             marginal_pd()
#
# Public API:
#   stress_mevs(historical_mevs, severity_z, mev_specs)
#   compute_logit_pds(stressed_mevs, mev_specs)
#   compute_pds_from_logits(logit_pds)
#   compute_per_mev_sf(estimated_pds, ttc_anchor)
#   combine_sf(per_mev_sf, mev_weights)
#   pit_pd_term_structure(ttc_pd, combined_sf, n_forecasts, max_maturity)
#   cumulative_pd(marginal_pd_vec)
#   marginal_pd(cumulative_pd_vec)
#
# Excel function equivalents:
#   NORM.S.INV(p)        -> qnorm(p)
#   NORM.S.DIST(z, TRUE) -> pnorm(z)
#   NORM.DIST(z,0,1,TRUE) -> pnorm(z)
#   EXP(x)               -> exp(x)
#   LN(x)                -> log(x)
# ============================================================================


# ---------------------------------------------------------------------------
# Step 1: stress historical MEVs by scenario severity
# ---------------------------------------------------------------------------

#' Stress a vector of historical / forecast MEVs.
#'
#' Excel formula (Calculations!D9, E9, F9 etc.):
#'   Stressed[t] = Original[t] + Model$SD * unit_multiplier * severity_z
#'
#' The unit_multiplier is 1 for some MEVs (e.g. Non-Oil GDP, raw values like
#' "2.47%") or 100 for others (e.g. Real Estate Index, "-8.04" already
#' represents a percentage point change without scaling). The same MEV's
#' unit_multiplier is later DIVIDED out in compute_logit_pds() — net effect
#' on the SF is `SD * z * coef` regardless of unit_multiplier, but matters
#' for matching Excel's intermediate cells.
#'
#' Same severity_z applies to all time steps within a scenario.
#'
#' @param historical_mevs  matrix[T x M] of historical/forecast MEV values
#' @param severity_z       scalar — z-score for this scenario (e.g. -1.2816)
#' @param mev_specs        list-of-lists with $standard_deviation,
#'                         $stress_unit_multiplier per MEV
#' @return matrix[T x M] of stressed MEVs
stress_mevs <- function(historical_mevs, severity_z, mev_specs) {
  if (!is.matrix(historical_mevs)) {
    historical_mevs <- as.matrix(historical_mevs)
  }
  M <- ncol(historical_mevs)
  if (length(mev_specs) != M) {
    stop(sprintf("mev_specs has %d entries but historical_mevs has %d columns",
                 length(mev_specs), M))
  }
  sd_vec   <- vapply(mev_specs, function(m) m$standard_deviation, numeric(1))
  unit_vec <- vapply(mev_specs, function(m) m$stress_unit_multiplier, numeric(1))
  shift_per_mev <- severity_z * sd_vec * unit_vec
  shift <- matrix(shift_per_mev, nrow = nrow(historical_mevs),
                  ncol = M, byrow = TRUE)
  historical_mevs + shift
}


# ---------------------------------------------------------------------------
# Step 2: per-MEV logit PD
# ---------------------------------------------------------------------------

#' Compute logit PD for each MEV at each time step.
#'
#' Excel formula (Calculations!D17 etc.):
#'   logit[t,m] = stressed_mev[t,m] / unit_multiplier[m] * coef[m] + intercept[m]
#'
#' The unit_multiplier is 1 for some MEVs (use raw value) or 100 for others
#' (divide by 100 — matches the Excel "/100" pattern in cols E,F).
#'
#' @param stressed_mevs  matrix[T x M]
#' @param mev_specs      list of mev specs; each has $intercept, $coefficient,
#'                       $stress_unit_multiplier
#' @return matrix[T x M] of logit PDs
compute_logit_pds <- function(stressed_mevs, mev_specs) {
  M <- ncol(stressed_mevs)
  intercept <- vapply(mev_specs, function(m) m$intercept, numeric(1))
  coef      <- vapply(mev_specs, function(m) m$coefficient, numeric(1))
  unit_mult <- vapply(mev_specs, function(m) m$stress_unit_multiplier, numeric(1))

  # Apply per-column: scaled = stressed/unit_mult, logit = scaled*coef + intercept
  scaled <- sweep(stressed_mevs, 2, unit_mult, FUN = "/")
  out    <- sweep(scaled, 2, coef, FUN = "*")
  out    <- sweep(out, 2, intercept, FUN = "+")
  out
}


# ---------------------------------------------------------------------------
# Step 3: PD from logit
# ---------------------------------------------------------------------------

#' Apply the inverse logit transform to get a PD in (0,1).
#'
#' Excel formula: =EXP(D17)/(EXP(D17)+1)
#' Equivalent to standard logistic: 1/(1+exp(-x))
#'
#' @param logit_pds matrix or vector
#' @return same shape, values in (0,1)
compute_pds_from_logits <- function(logit_pds) {
  e <- exp(logit_pds)
  e / (e + 1)
}


# ---------------------------------------------------------------------------
# Step 4: per-MEV stressing factor
# ---------------------------------------------------------------------------

#' Compute the per-MEV stressing factor: Φ⁻¹(stressed PD) − Φ⁻¹(TTC anchor).
#'
#' Excel formula (Calculations!D31):
#'   =IF(D24<>"", NORM.S.INV(D24) - NORM.S.INV(0.146), "")
#'
#' The TTC anchor is hardcoded at 0.146 in the workbook. We pass it in.
#'
#' @param estimated_pds matrix[T x M] of PDs
#' @param ttc_anchor    scalar TTC anchor PD (e.g. 0.146)
#' @return matrix[T x M] of stressing factors
compute_per_mev_sf <- function(estimated_pds, ttc_anchor) {
  qnorm(estimated_pds) - qnorm(ttc_anchor)
}


# ---------------------------------------------------------------------------
# Step 5: combine the per-MEV SFs into a single SF per time step
# ---------------------------------------------------------------------------

#' Combine per-MEV stressing factors into a single time-indexed SF.
#'
#' Excel formula (Calculations!K30):
#'   =$D$31*MEV1.weight + MEV2.weight*$E$31 + $F$31*MEV3.weight
#'
#' i.e. weighted sum across MEVs at time step t.
#'
#' @param per_mev_sf matrix[T x M]
#' @param mev_weights numeric vector length M, sums to 1
#' @return numeric vector of length T
combine_sf <- function(per_mev_sf, mev_weights) {
  if (ncol(per_mev_sf) != length(mev_weights)) {
    stop("ncol(per_mev_sf) must equal length(mev_weights)")
  }
  as.numeric(per_mev_sf %*% mev_weights)
}


# ---------------------------------------------------------------------------
# Step 6: PiT PD term structure — Vasicek shift + log-linear extrapolation
# ---------------------------------------------------------------------------

#' Build the full PiT PD term structure for one rating × one scenario.
#'
#' Excel formula (Calculations!K7 etc., per maturity column $t$):
#'   IF t <= n_forecasts:
#'     NORM.DIST(NORM.S.INV(TTC_PD) + combined_SF[t], 0, 1, TRUE)
#'   IF n_forecasts < t <= max_maturity:
#'     LN(
#'       (t - n_forecasts) / (max_maturity - n_forecasts) *
#'       (EXP(TTC_PD) - EXP(PD_at_n_forecasts))
#'       + EXP(PD_at_n_forecasts)
#'     )
#'   IF t > max_maturity:
#'     ""  (NA)
#'
#' Note Excel writes the TTC PD value in the cell-reference 'Inputs_Lending
#' Portfolio'!$AU5 — the per-rating TTC PD. So the formula computes
#' Vasicek-shifted PiT PD for the first n_forecasts steps, then log-linearly
#' decays back toward the TTC PD by max_maturity.
#'
#' Implementation notes:
#'  - The extrapolation formula uses LN of a value that should be > 0 (the
#'    operands are EXP(...) > 0 and a convex combination thereof). We rely on
#'    that convexity rather than guarding here.
#'  - "EXP(TTC_PD)" in Excel literally means exp(TTC_PD as a number 0..1),
#'    which is correct — the formula is doing log-linear interpolation in
#'    "log of exp(PD)" = PD space. Yes, weirdly that simplifies to linear.
#'    But we replicate the Excel arithmetic exactly to match cell values.
#'
#' @param ttc_pd        scalar TTC PD for this rating
#' @param combined_sf   numeric vector of length n_forecasts (combined stressing factors)
#' @param n_forecasts   integer: number of forecast years available
#' @param max_maturity  integer: the maturity horizon (e.g. 50 months/years)
#' @return numeric vector of length max_maturity
pit_pd_term_structure <- function(ttc_pd, combined_sf, n_forecasts, max_maturity) {
  if (length(combined_sf) < n_forecasts) {
    stop(sprintf("combined_sf has %d elements but n_forecasts=%d",
                 length(combined_sf), n_forecasts))
  }
  out <- rep(NA_real_, max_maturity)

  # Special case: TTC PD of 0 means "no default risk" — produces an
  # all-zero PD term structure regardless of macroeconomic stress. This
  # matches Excel's behaviour for the highest external ratings (Aaa, Aa1,
  # Aa2 in the bundled table).
  if (isTRUE(ttc_pd == 0)) {
    return(rep(0, max_maturity))
  }

  # Phase 1: Vasicek shift for t = 1..n_forecasts
  inv_ttc <- qnorm(ttc_pd)
  for (t in seq_len(n_forecasts)) {
    out[t] <- pnorm(inv_ttc + combined_sf[t])
  }

  # Phase 2: log-linear decay back to TTC for t = n_forecasts+1..max_maturity
  pd_at_anchor <- out[n_forecasts]
  if (is.na(pd_at_anchor)) {
    return(out)
  }
  exp_pd_anchor <- exp(pd_at_anchor)
  exp_ttc       <- exp(ttc_pd)
  span          <- max_maturity - n_forecasts

  if (span > 0) {
    for (t in (n_forecasts + 1L):max_maturity) {
      frac <- (t - n_forecasts) / span
      out[t] <- log(frac * (exp_ttc - exp_pd_anchor) + exp_pd_anchor)
    }
  }
  out
}


# ---------------------------------------------------------------------------
# Step 7: Cumulative and Marginal PDs
# ---------------------------------------------------------------------------

#' Compute cumulative PDs from a marginal PD term structure.
#'
#' Excel formula (Calculations!L33):
#'   =K33 + (1 - K33) * L7
#' i.e. survival aggregation: cum[t] = cum[t-1] + (1 - cum[t-1]) * marg[t]
#'
#' Equivalent to: cum[t] = 1 - prod_i<=t (1 - marg[i])
#'
#' @param marginal_pd_vec numeric vector
#' @return numeric vector of same length
cumulative_pd <- function(marginal_pd_vec) {
  1 - cumprod(1 - marginal_pd_vec)
}


# ---------------------------------------------------------------------------
# Step 6b: Basel ASRF formula — used for externally-rated PiT PDs
# ---------------------------------------------------------------------------

#' Basel II ASRF (Asymptotic Single Risk Factor) PiT PD for one rating × one
#' forecast year. This is the V7 production formula for the externally-rated
#' (investment) portfolios; it differs from the simple Vasicek shift used
#' for internally-rated portfolios.
#'
#' Excel formula (Calculations!BM7, V7 only):
#'   PiT = NORM.S.DIST(
#'           ( NORM.S.INV(TTC) - SQRT(R) * SF )
#'           / SQRT(1 - R),
#'           1
#'         )
#'
#' where the asset correlation R depends on the TTC PD itself, per the
#' Basel III Wholesale IRB formula for corporate exposures:
#'   R(TTC) = 0.24 - 0.12 * (1 - exp(-50*TTC)) / (1 - exp(-50))
#'
#' Note the SF appears with a NEGATIVE sign in the Basel formula, so a
#' positive SF (downturn) yields a HIGHER PiT PD because it pushes the
#' numerator more negative through `-sqrt(R)*SF`, then `Φ` of a more-negative
#' number ... wait, that gives lower PiT. Actually: the input `SF` here is
#' the inverse-normal of a recession indicator, so positive SF = bad times.
#' Then numerator `Φ⁻¹(TTC) - sqrt(R)*SF` becomes more negative, but we
#' divide by `sqrt(1-R) < 1`, then take Φ. The whole expression evaluates
#' such that `SF = 0` → PiT = TTC, and SF > 0 → PiT > TTC. Verified empirically.
#'
#' Special cases:
#'  - TTC == 0 → returns 0 (matches Excel's IF(...$Q5=0, 0, ...))
#'  - TTC >= 1 → returns 1 (degenerate; not expected in practice)
#'
#' @param ttc_pd       scalar TTC PD for this rating (in [0,1))
#' @param sf           scalar single-period stressing factor
#' @return scalar PiT PD in [0,1]
basel_asrf_pit <- function(ttc_pd, sf) {
  if (is.na(ttc_pd) || is.na(sf)) return(NA_real_)
  if (ttc_pd <= 0) return(0)
  if (ttc_pd >= 1) return(1)

  R <- 0.24 - 0.12 * (1 - exp(-50 * ttc_pd)) / (1 - exp(-50))
  pnorm((qnorm(ttc_pd) - sqrt(R) * sf) / sqrt(1 - R))
}


#' Build the full external-rating PiT PD term structure for one rating × one
#' scenario, using Basel ASRF for years 1..n_forecasts and the same
#' log-linear extrapolation as the internal version for years past
#' n_forecasts.
#'
#' Excel formula (Calculations!BM7 in V7):
#'   IF t <= n_forecasts:
#'     basel_asrf(TTC, combined_sf[t])
#'   IF n_forecasts < t <= max_maturity:
#'     LN(
#'       (t - n_forecasts) / (max_maturity - n_forecasts) *
#'       (EXP(TTC) - EXP(PiT_at_n_forecasts))
#'       + EXP(PiT_at_n_forecasts)
#'     )
#'
#' @param ttc_pd       scalar TTC PD
#' @param combined_sf  numeric vector length n_forecasts
#' @param n_forecasts  integer
#' @param max_maturity integer
#' @return numeric vector length max_maturity
pit_pd_term_structure_external <- function(ttc_pd, combined_sf,
                                            n_forecasts, max_maturity) {
  if (length(combined_sf) < n_forecasts) {
    stop(sprintf("combined_sf has %d elements but n_forecasts=%d",
                 length(combined_sf), n_forecasts))
  }
  out <- rep(NA_real_, max_maturity)
  if (isTRUE(ttc_pd == 0)) return(rep(0, max_maturity))

  # Phase 1: Basel ASRF for years 1..n_forecasts
  for (t in seq_len(n_forecasts)) {
    out[t] <- basel_asrf_pit(ttc_pd, combined_sf[t])
  }

  # Phase 2: log-linear extrapolation (same as internal)
  pd_at_anchor <- out[n_forecasts]
  if (is.na(pd_at_anchor)) return(out)
  exp_pd_anchor <- exp(pd_at_anchor)
  exp_ttc       <- exp(ttc_pd)
  span          <- max_maturity - n_forecasts

  if (span > 0) {
    for (t in (n_forecasts + 1L):max_maturity) {
      frac <- (t - n_forecasts) / span
      out[t] <- log(frac * (exp_ttc - exp_pd_anchor) + exp_pd_anchor)
    }
  }
  out
}


# ---------------------------------------------------------------------------
# Step 6c: External-rating combined SF — V7 percentile-rank method
# ---------------------------------------------------------------------------

#' Compute the per-forecast-year combined stressing factor for the
#' externally-rated portfolios using V7's PERCENTRANK.EXC methodology.
#'
#' For each forecast year, takes a country-weighted GCC GDP value, looks up
#' its excluding-end percentile rank against the full GCC historical series,
#' and converts that rank to a z-score via NORM.S.INV.
#'
#' Excel formula (Calculations!BM30, V7):
#'   BM30 = NORM.S.INV( PERCENTRANK.EXC( 'GCC GDP'!D28:AT28, scenario_year_GDP ) )
#'
#' The scenario_year_GDP is itself a country-weighted SUMPRODUCT of stressed
#' per-country GCC GDP forecasts. This function takes those weighted values
#' as input directly (one per forecast year).
#'
#' @param weighted_gcc_per_year  numeric vector length n_forecasts —
#'                                the stressed weighted-avg GCC GDP for each
#'                                forecast year (one scalar per year).
#' @param gcc_history             numeric vector — full historical GCC GDP
#'                                series (e.g. data-raw/static/gcc_real_gdp_growth)
#' @return numeric vector length n_forecasts of stressing factors
compute_external_combined_sf <- function(weighted_gcc_per_year, gcc_history) {
  vapply(weighted_gcc_per_year, function(v) {
    if (is.na(v)) return(NA_real_)
    qnorm(percentrank_exc(gcc_history, v))
  }, numeric(1))
}


#' Excel's PERCENTRANK.EXC: ranks `x` exclusively (so ranks are in (0,1)
#' rather than [0,1]). For an array of size n, the smallest gets rank
#' 1/(n+1) and the largest gets rank n/(n+1). Values between observations
#' are linearly interpolated.
#'
#' Excel's PERCENTRANK.EXC takes an optional 3rd argument 'significance'
#' that defaults to 3, meaning the result is TRUNCATED to 3 significant
#' digits. V7's downstream NORM.S.INV chain reflects this truncation —
#' e.g. V7 BM30 = 0.24559 = NORM.S.INV(0.597) where 0.597 is the truncated
#' rank, not the full-precision 0.59735. Reproducing V7's stored values
#' to floating-point requires applying the same truncation here.
#'
#' @param data       numeric vector
#' @param x          numeric scalar to rank
#' @param significance integer; truncate result to this many significant
#'                   digits (Excel default = 3). Pass NA to disable.
#' @return scalar in (0,1)
percentrank_exc <- function(data, x, significance = 3L) {
  data <- sort(as.numeric(data))
  n <- length(data)
  if (x < data[1] || x > data[n]) {
    if (x < data[1]) {
      pr <- 1 / (n + 1)
    } else {
      pr <- n / (n + 1)
    }
  } else {
    j <- findInterval(x, data, left.open = FALSE)   # 1-based
    if (j == n) {
      pr <- n / (n + 1)
    } else if (data[j + 1] == data[j]) {
      pr <- j / (n + 1)
    } else {
      frac <- (x - data[j]) / (data[j + 1] - data[j])
      pr <- (j + frac) / (n + 1)
    }
  }
  if (!is.na(significance)) {
    pr <- truncate_significant(pr, as.integer(significance))
  }
  pr
}


#' Truncate a positive number to N significant digits (no rounding).
#'
#' Mirrors Excel's TRUNC-style behaviour used inside PERCENTRANK.EXC. For
#' input 0.59734 with significance = 3 returns 0.597; for 0.0227349 with
#' significance = 3 returns 0.0227.
truncate_significant <- function(x, significance = 3L) {
  if (is.na(x) || x == 0) return(x)
  magnitude <- floor(log10(abs(x)))
  factor <- 10 ^ (significance - 1 - magnitude)
  sign(x) * floor(abs(x) * factor) / factor
}


# ---------------------------------------------------------------------------
# Step 6d: External-rating scenario weights — V7 normal-CDF method
# ---------------------------------------------------------------------------

#' Compute the per-scenario probability weights for externally-rated
#' portfolios using V7's normal-CDF methodology against the GCC GDP
#' distribution.
#'
#' Excel formulas (Calculations!D69:H69, V7) for the 5-scenario case:
#'   threshold[s]   = year1_GDP + severity_z[s] * gcc_sd     for each scenario s
#'   W[Sig Down]    = N(threshold[Sig Down])
#'   W[Slight Down] = N(threshold[Slight Down]) - N(threshold[Sig Down])
#'   W[Slight Up]   = N(threshold[Sig Up])      - N(threshold[Slight Up])
#'   W[Sig Up]      = 1 - N(threshold[Sig Up])
#' Compute external scenario weights for a SINGLE forecast year.
#'
#' Replicates V7 Calculations row 69 (year 1):
#'   D69 = NORM.DIST('Significant Downturn'!D38, GCC_mean, GCC_sd, 1)
#'   E69 = NORM.DIST('Slight Downturn'!D38, ...) - NORM.DIST(SD, ...)
#'   F69 = 1 - D69 - E69 - G69 - H69                 # residual
#'   G69 = NORM.DIST('Significant Uptrend'!D38, ...) - NORM.DIST('Slight Uptrend'!D38, ...)
#'   H69 = 1 - NORM.DIST('Significant Uptrend'!D38, ...)
#'
#' where each scenario sheet's D38 = year_1_forecast + gcc_sd * severity_z[scenario].
#'
#' @param weighted_gcc_year scalar — that year's weighted GCC GDP forecast
#' @param gcc_history       numeric vector — historical weighted GCC GDP
#' @param scenarios         tibble with cols (scenario, severity_z)
#' @return named numeric vector summing to 1, aligned to scenarios$scenario
compute_external_scenario_weights <- function(weighted_gcc_year,
                                                gcc_history, scenarios) {
  gcc_mean <- mean(gcc_history)
  gcc_sd   <- sd(gcc_history)

  ord       <- order(scenarios$severity_z)
  z_sorted  <- scenarios$severity_z[ord]
  n         <- length(z_sorted)
  central   <- which.min(abs(z_sorted))

  thresholds <- weighted_gcc_year + z_sorted * gcc_sd
  cdf_at     <- pnorm(thresholds, mean = gcc_mean, sd = gcc_sd)

  weights_sorted <- numeric(n)
  for (i in seq_len(n)) {
    if (i == central) next
    if (i < central) {
      lower <- if (i == 1) 0 else cdf_at[i - 1]
      weights_sorted[i] <- cdf_at[i] - lower
    } else {
      upper <- if (i == n) 1 else cdf_at[i + 1]
      weights_sorted[i] <- upper - cdf_at[i]
    }
  }
  weights_sorted[central] <- 1 - sum(weights_sorted)

  weights <- numeric(n)
  weights[ord] <- weights_sorted
  names(weights) <- scenarios$scenario
  weights
}


#' Compute external scenario weights for ALL forecast years + the average row.
#'
#' Replicates V7 Calculations rows 69 through 74:
#'   Row 69 = year 1 weights (uses year-1 weighted GCC forecast)
#'   Row 70 = year 2 weights (uses year-2 weighted GCC forecast)
#'   ...
#'   Row 73 = year 5 weights (uses year-5 weighted GCC forecast)
#'   Row 74 = AVERAGE(rows 69:73) — applied to maturities beyond the
#'            forecast horizon
#'
#' @param weighted_gcc_forecasts numeric vector, length n_forecast_years
#' @param gcc_history            numeric vector — historical weighted GCC GDP
#' @param scenarios              tibble with cols (scenario, severity_z)
#' @return list with components:
#'   $per_year matrix [n_forecast_years × n_scenarios], rows align to year 1..N,
#'             cols align to scenarios$scenario
#'   $average  named numeric vector — column-wise mean of $per_year
compute_external_scenario_weights_per_year <- function(weighted_gcc_forecasts,
                                                          gcc_history, scenarios) {
  per_year <- t(vapply(weighted_gcc_forecasts, function(F) {
    compute_external_scenario_weights(F, gcc_history, scenarios)
  }, numeric(nrow(scenarios))))
  colnames(per_year) <- scenarios$scenario
  rownames(per_year) <- paste0("year_", seq_along(weighted_gcc_forecasts))

  average <- colMeans(per_year)
  list(per_year = per_year, average = average)
}


#' Inverse: marginal PDs from cumulative PDs.
#'
#' marg[t] = (cum[t] - cum[t-1]) / (1 - cum[t-1]),  with marg[1] = cum[1].
#'
#' @param cumulative_pd_vec numeric vector
#' @return numeric vector of same length
marginal_pd <- function(cumulative_pd_vec) {
  n <- length(cumulative_pd_vec)
  if (n == 0) return(numeric(0))
  out <- numeric(n)
  out[1] <- cumulative_pd_vec[1]
  if (n >= 2) {
    for (t in 2:n) {
      denom <- 1 - cumulative_pd_vec[t - 1]
      if (is.na(denom) || denom <= 0) {
        out[t] <- NA_real_
      } else {
        out[t] <- (cumulative_pd_vec[t] - cumulative_pd_vec[t - 1]) / denom
      }
    }
  }
  out
}
