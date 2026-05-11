# ============================================================================
# R/pd_term_structure.R
#
# Orchestrates the macro model pipeline. Dispatches to one of two engines
# depending on rating type:
#
#   "Internal" — lending portfolios (Business Finance, Off BS, Al Dhameen,
#                Tasdeer). Uses the Vasicek-shift formula:
#                  PiT = N(N⁻¹(TTC) + SF)
#                where SF is a weighted combination of per-MEV stressing
#                factors derived from the regression models.
#
#   "External" — investment portfolios (Banks and Fis, Investments). Uses
#                the Basel ASRF formula (V7 production):
#                  PiT = N( (N⁻¹(TTC) - sqrt(R) * SF) / sqrt(1-R) )
#                  R   = 0.24 - 0.12 * (1 - exp(-50*TTC)) / (1 - exp(-50))
#                where SF is derived from PERCENTRANK.EXC of a country-
#                weighted GCC GDP forecast against the historical GCC GDP
#                series.
#
# Public API:
#   build_pd_term_structure(ttc_pd_table, scenarios, model_cfg,
#                           model_inputs, gcc_history,
#                           rating_type = c("Internal","External"))
#     -> long-format tibble:
#        (rating, scenario, maturity, marginal_pd, cumulative_pd)
#
# Inputs:
#   ttc_pd_table  : tibble with cols (rating, ttc_pd) — one row per rating.
#                   For Internal: from data-raw/static/ttc_pd_table.csv
#                                 (Inputs_Lending Portfolio AT5:AU25).
#                   For External: from data-raw/static/ttc_pd_table_external.csv
#                                 (Inputs_Investment Portfolio P5:Q25).
#
#   scenarios     : tibble with cols (scenario, severity_z) — from
#                   static$scenario_severity. Order defines the canonical
#                   scenario ordering used everywhere downstream.
#
#   model_cfg     : load_model_config() output. Provides MEV specs,
#                   ttc_anchor_pd, max_maturity, n_forecasts.
#
#   model_inputs  : load_model_inputs() output. Provides per-run
#                   scenario weights (separate for internal/external),
#                   MEV forecasts, and external GCC GDP forecast.
#
#   gcc_history   : numeric vector of historical GCC GDP growth values,
#                   typically static$gcc_real_gdp_growth$value (ALL years
#                   ALL countries, or the cross-country average per year —
#                   matches the V7 GCC GDP!D28:AT28 series).
#
# Output:
#   long-format tibble (rating, scenario, maturity, marginal_pd, cumulative_pd)
# ============================================================================


#' Build the full PD term structure across all scenarios × ratings.
#'
#' @param ttc_pd_table tibble (rating, ttc_pd)
#' @param scenarios tibble (scenario, severity_z)
#' @param model_cfg model config from load_model_config()
#' @param model_inputs from load_model_inputs()
#' @param gcc_history numeric vector — only required for rating_type="External"
#' @param rating_type one of "Internal" or "External"
#' @return long-format tibble (rating, scenario, maturity, marginal_pd, cumulative_pd)
build_pd_term_structure <- function(ttc_pd_table,
                                     scenarios,
                                     model_cfg,
                                     model_inputs,
                                     gcc_history = NULL,
                                     rating_type = c("Internal", "External")) {
  rating_type <- match.arg(rating_type)
  max_maturity <- as.integer(model_cfg$model$max_maturity)

  # Filter ttc_pd_table to ratings with usable values.
  valid_ratings <- ttc_pd_table[!is.na(ttc_pd_table$ttc_pd) &
                                ttc_pd_table$ttc_pd >= 0 &
                                ttc_pd_table$ttc_pd < 1, ]
  if (nrow(valid_ratings) == 0) {
    stop("ttc_pd_table contains no valid TTC PDs")
  }

  # Per-scenario combined SF. The shape and source differ by rating_type.
  combined_sf_by_scenario <- compute_combined_sf_by_scenario(
    scenarios     = scenarios,
    model_cfg     = model_cfg,
    model_inputs  = model_inputs,
    gcc_history   = gcc_history,
    rating_type   = rating_type
  )

  # Per-scenario PiT-PD-builder dispatch
  build_pit <- if (rating_type == "Internal") {
    pit_pd_term_structure
  } else {
    pit_pd_term_structure_external
  }
  n_forecasts <- as.integer(model_cfg$model$n_forecasts %||% 5)

  out_chunks <- list()
  for (i in seq_len(nrow(scenarios))) {
    sc_name <- scenarios$scenario[i]
    combined_sf <- combined_sf_by_scenario[[sc_name]]

    for (j in seq_len(nrow(valid_ratings))) {
      rating <- valid_ratings$rating[j]
      ttc    <- valid_ratings$ttc_pd[j]

      pit  <- build_pit(
        ttc_pd        = ttc,
        combined_sf   = combined_sf,
        n_forecasts   = n_forecasts,
        max_maturity  = max_maturity
      )
      cum  <- cumulative_pd(pit)
      marg <- diff(c(0, cum))

      out_chunks[[length(out_chunks) + 1L]] <- tibble::tibble(
        rating       = rating,
        scenario     = sc_name,
        maturity     = seq_len(max_maturity),
        marginal_pd  = marg,
        cumulative_pd = cum
      )
    }
  }
  dplyr::bind_rows(out_chunks)
}


#' Compute the combined stressing factor time series for each scenario.
#'
#' For Internal: stress MEVs by severity_z, run logit regressions, compute
#'   per-MEV stressing factors, weight-combine via mev_model_weights.
#' For External: shift the weighted GCC GDP forecast by severity_z * gcc_sd,
#'   then PERCENTRANK.EXC against gcc_history and convert to z-score.
#'
#' @return named list keyed by scenario name; each element is a numeric
#'         vector of length n_forecasts.
compute_combined_sf_by_scenario <- function(scenarios, model_cfg,
                                              model_inputs, gcc_history,
                                              rating_type) {
  if (rating_type == "Internal") {
    mev_specs    <- model_cfg$model$mevs
    mev_weights  <- model_inputs$mev_model_weights
    ttc_anchor   <- model_cfg$model$ttc_anchor_pd
    mev_forecasts <- model_inputs$mev_forecasts

    out <- list()
    for (i in seq_len(nrow(scenarios))) {
      sc_name <- scenarios$scenario[i]
      sc_z    <- scenarios$severity_z[i]
      stressed_mevs <- stress_mevs(mev_forecasts, sc_z, mev_specs)
      logit_pds     <- compute_logit_pds(stressed_mevs, mev_specs)
      estimated_pds <- compute_pds_from_logits(logit_pds)
      per_mev_sf    <- compute_per_mev_sf(estimated_pds, ttc_anchor)
      out[[sc_name]] <- combine_sf(per_mev_sf, mev_weights)
    }
    return(out)
  }

  # rating_type == "External"
  if (is.null(gcc_history)) {
    stop("rating_type='External' requires gcc_history to be passed to build_pd_term_structure()")
  }
  gcc_fc <- model_inputs$external_gcc_forecast
  gcc_sd <- sd(gcc_history)

  out <- list()
  for (i in seq_len(nrow(scenarios))) {
    sc_name <- scenarios$scenario[i]
    sc_z    <- scenarios$severity_z[i]
    # Stress each year's weighted-avg GCC GDP forecast by sc_z * gcc_sd
    stressed_gcc <- gcc_fc + sc_z * gcc_sd
    out[[sc_name]] <- compute_external_combined_sf(stressed_gcc, gcc_history)
  }
  out
}


#' Helper: build the MEV forecast matrix from a long-format tibble.
#' (Kept for backward compatibility with earlier code; new callers should
#' just use model_inputs$mev_forecasts directly.)
make_mev_forecast_matrix <- function(mev_forecasts_long, model_cfg) {
  mev_names <- vapply(model_cfg$model$mevs, function(m) m$name, character(1))
  years <- sort(unique(mev_forecasts_long$year))
  out <- matrix(NA_real_, nrow = length(years), ncol = length(mev_names))
  colnames(out) <- mev_names

  for (m_idx in seq_along(mev_names)) {
    rows <- mev_forecasts_long[mev_forecasts_long$mev_name == mev_names[m_idx], ]
    out[, m_idx] <- rows$value[match(years, rows$year)]
  }
  out
}
