# ============================================================================
# R/load_config.R
#
# Loaders for three YAML config files:
#
#   config.yml              - run-time paths, run params, logging
#   config/model_config.yml - macro model parameters (3 MEVs and constants)
#   config/model_inputs.yml - per-run scenario weights + MEV forecasts
#                             + external GCC GDP forecast
#
# Public API:
#   load_run_config(path)    -> list(paths, run, logging)
#   load_model_config(path)  -> list(model)
#   load_model_inputs(path, model_cfg, scenarios, gcc_history)
#                            -> list(internal_scenario_weights,
#                                    external_scenario_weights,
#                                    external_gcc_forecast,
#                                    mev_model_weights, mev_forecasts)
# ============================================================================


#' Null-coalescing operator (R doesn't have one built-in)
`%||%` <- function(a, b) if (is.null(a)) b else a


#' Load the top-level run configuration.
load_run_config <- function(path = "config.yml") {
  if (!file.exists(path)) {
    stop("Run config not found: ", path)
  }
  cfg <- yaml::read_yaml(path)

  required_top <- c("paths", "run", "logging")
  missing_top <- setdiff(required_top, names(cfg))
  if (length(missing_top) > 0) {
    stop("Run config is missing top-level keys: ",
         paste(missing_top, collapse = ", "))
  }

  required_paths <- c("input_dir", "output_dir", "static_dir",
                      "model_config", "model_inputs")
  missing_paths <- setdiff(required_paths, names(cfg$paths))
  if (length(missing_paths) > 0) {
    stop("Run config paths block is missing: ",
         paste(missing_paths, collapse = ", "))
  }

  # Resolve every relative path under `paths:` to an absolute path
  # anchored at the config file's own directory. This makes the loaded
  # config self-contained — downstream code (load_static_reference,
  # read_all_inputs, etc.) can rely on getting absolute paths even when
  # the working directory changes between load and use (which Shiny
  # does on every reactive evaluation in some versions). Without this
  # the pipeline breaks unpredictably under the Shiny app.
  cfg_dir <- normalizePath(dirname(path), mustWork = FALSE)
  for (k in names(cfg$paths)) {
    v <- cfg$paths[[k]]
    if (!is.null(v) && is.character(v) && length(v) == 1 &&
        nzchar(v) && !.is_absolute_path(v)) {
      cfg$paths[[k]] <- normalizePath(file.path(cfg_dir, v), mustWork = FALSE)
    }
  }

  cfg
}


#' Internal: is a path absolute?
#' Handles both Unix ("/...") and Windows (e.g. "C:/...", "C:\\...", "\\\\...")
#' style absolute paths.
.is_absolute_path <- function(p) {
  if (length(p) != 1 || !is.character(p) || !nzchar(p)) return(FALSE)
  grepl("^(/|~|[A-Za-z]:[/\\\\]|\\\\\\\\)", p)
}


#' Load the macro model configuration.
#'
#' Resolution order:
#'   1. If `path` exists and the new model registry exists, read `path`
#'      to get the selected model id (run.internal_model preferred,
#'      run.model_id as fallback) and resolve via models.yml.
#'   2. If a legacy `model_config.yml`-shaped file is passed, parse it
#'      directly (legacy mode — emits a deprecation warning).
#'
#' Both paths return the same shape: list(model = list(name, ttc_anchor_pd,
#' max_maturity, n_forecasts, mevs = list(...))).
#'
#' @param path  Either a `model_config.yml` (legacy) or `config.yml` (new).
#'              For the new path, the file must point at models.yml +
#'              variable_dictionary.yml via `paths:`.
load_model_config <- function(path) {
  if (!file.exists(path)) {
    stop("Model config not found: ", path)
  }
  cfg <- yaml::read_yaml(path)

  # Apply the same path-resolution logic as load_run_config: any relative
  # path under cfg$paths is resolved relative to this config file's
  # directory. Without this, variable_dictionary.yml + models.yml lookups
  # break under Shiny (where cwd != project root).
  cfg_dir <- normalizePath(dirname(path), mustWork = FALSE)
  if (!is.null(cfg$paths) && is.list(cfg$paths)) {
    for (k in names(cfg$paths)) {
      v <- cfg$paths[[k]]
      if (!is.null(v) && is.character(v) && length(v) == 1 &&
          nzchar(v) && !.is_absolute_path(v)) {
        cfg$paths[[k]] <- normalizePath(file.path(cfg_dir, v),
                                          mustWork = FALSE)
      }
    }
  }

  # ---- New routing: config.yml referencing models.yml -----------------
  if (!is.null(cfg$paths$models) || !is.null(cfg$run$internal_model)) {
    models_path <- cfg$paths$models %||%
                    normalizePath(file.path(cfg_dir, "config", "models.yml"),
                                   mustWork = FALSE)
    var_dict_path <- cfg$paths$variable_dictionary %||%
                       normalizePath(file.path(cfg_dir, "config",
                                                 "variable_dictionary.yml"),
                                       mustWork = FALSE)
    model_id <- cfg$run$internal_model %||% cfg$run$model_id
    if (is.null(model_id)) {
      stop("config.yml has paths$models but no run$internal_model selected")
    }
    return(resolve_model_config(model_id,
                                 variable_dictionary_path = var_dict_path,
                                 models_path = models_path))
  }

  # ---- Legacy: direct model_config.yml --------------------------------
  warning("Reading legacy model_config.yml directly. ",
          "Migrate to config/models.yml + config/variable_dictionary.yml ",
          "(see docs/h7_design_notes.md).", call. = FALSE)

  if (is.null(cfg$model)) {
    stop("Model config is missing top-level key: model")
  }

  required_model <- c("name", "ttc_anchor_pd", "max_maturity", "mevs")
  missing_model <- setdiff(required_model, names(cfg$model))
  if (length(missing_model) > 0) {
    stop("Model config $model is missing keys: ",
         paste(missing_model, collapse = ", "))
  }

  required_mev <- c("name", "intercept", "coefficient", "p_value",
                    "weight", "standard_deviation", "stress_unit_multiplier")
  for (i in seq_along(cfg$model$mevs)) {
    mev <- cfg$model$mevs[[i]]
    missing_mev <- setdiff(required_mev, names(mev))
    if (length(missing_mev) > 0) {
      stop(sprintf("Model config $model$mevs[[%d]] is missing keys: %s",
                   i, paste(missing_mev, collapse = ", ")))
    }
  }

  cfg
}


#' Load per-run model inputs.
#'
#' Resolves all "mode" blocks against their dependencies, returning literal
#' numeric vectors / matrices ready for the macro model engine.
#'
#' @param path Path to model_inputs.yml
#' @param model_cfg Output of load_model_config()
#' @param scenarios tibble (scenario, severity_z, ...) — typically
#'        static$scenario_severity. Drives the order of returned weights.
#' @param gcc_history numeric vector of historical GCC real GDP growth values.
#'        Required when external_scenario_weights.mode = "auto_gcc_cdf"
#'        (which is the V7 production default).
#' @return list with elements:
#'   $internal_scenario_weights : named numeric vector, length = nrow(scenarios)
#'   $external_scenario_weights : named numeric vector, length = nrow(scenarios)
#'   $external_gcc_forecast     : numeric vector, length = n_forecasts
#'   $mev_model_weights         : numeric vector, length = number of MEVs
#'   $mev_forecasts             : matrix [n_forecasts × n_mevs]
load_model_inputs <- function(path, model_cfg, scenarios,
                                gcc_history = NULL,
                                non_oil_gdp_history = NULL) {
  if (!file.exists(path)) {
    stop("Model inputs config not found: ", path)
  }
  cfg <- yaml::read_yaml(path)

  required_top <- c("internal_scenario_weights", "external_scenario_weights",
                    "external_gcc_country_growth", "external_gcc_country_prices",
                    "mev_model_weights", "mev_forecasts")
  missing_top <- setdiff(required_top, names(cfg))
  if (length(missing_top) > 0) {
    stop("Model inputs is missing top-level keys: ",
         paste(missing_top, collapse = ", "))
  }

  # ----- 1. external GCC weighted forecast — computed in code per V7 Calc!D62:H66 -----
  #
  # weighted_GCC[year y] = SUMPRODUCT(country_weight[*, y], country_growth[*, y])
  # where country_weight[c, y] = country_price[c, y] / Σ_c country_price[*, y]
  ext_gcc_fc <- compute_country_weighted_gcc_forecast(
    cfg$external_gcc_country_growth,
    cfg$external_gcc_country_prices
  )

  # ----- 1b. mev_forecasts (resolved early so internal weights can use the
  #            Non-Oil GDP forecast component for auto_non_oil_gdp_cdf) -----
  #
  # POSITIONAL ASSUMPTION: column 1 of the forecast matrix corresponds to
  # the FIRST mev_component in the active model. With the
  # variable-dictionary refactor (H7), models declare their MEVs in a
  # specific order; mev_forecasts.yml columns must follow the same order.
  # The current internal_v4_production model puts VAR_NON_OIL_GDP_GROWTH
  # first, so column 1 is Non-Oil GDP. If a future model reorders its
  # components, mev_forecasts.yml MUST be reordered to match. A future
  # hardening pass should index forecast columns by variable_id from the
  # dictionary (kind=model_inputs_yaml + column_index in dictionary)
  # instead of relying on position.
  fc_block <- cfg$mev_forecasts$forecasts
  if (is.null(fc_block) || length(fc_block) == 0) {
    stop("mev_forecasts.forecasts is empty")
  }
  years <- as.integer(names(fc_block))
  if (any(is.na(years)) || !identical(sort(years), seq_along(years))) {
    stop("mev_forecasts.forecasts must have integer year keys 1..n contiguous")
  }
  forecast_mat <- do.call(rbind,
                           lapply(fc_block[order(years)],
                                  function(row) as.numeric(unlist(row))))
  n_mev <- length(model_cfg$model$mevs)
  if (ncol(forecast_mat) != n_mev) {
    stop(sprintf("mev_forecasts has %d MEVs per year but model_config has %d",
                 ncol(forecast_mat), n_mev))
  }

  n_int_fc <- as.integer(cfg$internal_scenario_weights$n_forecast_years %||% 2L)
  non_oil_gdp_fc <- forecast_mat[seq_len(min(n_int_fc, nrow(forecast_mat))), 1]

  # ----- 2. internal_scenario_weights -----
  internal_weights <- resolve_internal_scenario_weights(
    block                 = cfg$internal_scenario_weights,
    scenarios             = scenarios,
    non_oil_gdp_history   = non_oil_gdp_history,
    non_oil_gdp_forecasts = non_oil_gdp_fc,
    block_name            = "internal_scenario_weights"
  )

  # ----- 3. external_scenario_weights (per-year + average) -----
  external_weights <- resolve_external_scenario_weights(
    block                  = cfg$external_scenario_weights,
    scenarios              = scenarios,
    gcc_history            = gcc_history,
    weighted_gcc_forecasts = ext_gcc_fc,
    block_name             = "external_scenario_weights"
  )

  # ----- 4. mev_model_weights -----
  mmw_block <- cfg$mev_model_weights
  mode_mmw  <- mmw_block$mode %||% "from_model_config"
  if (mode_mmw == "from_model_config") {
    mev_model_weights <- vapply(model_cfg$model$mevs,
                                 function(m) as.numeric(m$weight),
                                 numeric(1))
  } else if (mode_mmw == "auto_p_value") {
    mev_model_weights <- compute_mev_weights_p_value(model_cfg$model$mevs)
  } else {
    stop("Unknown mev_model_weights mode: ", mode_mmw,
         " (expected 'from_model_config' or 'auto_p_value')")
  }

  if (length(ext_gcc_fc) != nrow(forecast_mat)) {
    warning(sprintf(
      "external_gcc_country_growth has %d years but mev_forecasts has %d. The model will use the shorter of the two.",
      length(ext_gcc_fc), nrow(forecast_mat)))
  }

  list(
    internal_scenario_weights = internal_weights,
    external_scenario_weights = external_weights,
    external_gcc_forecast     = ext_gcc_fc,
    mev_model_weights         = mev_model_weights,
    mev_forecasts             = forecast_mat
  )
}


#' Compute country-weighted GCC GDP forecast per year (V7 Calc!D62:H66 logic).
#'
#' For each forecast year y:
#'   weight[c, y] = country_price[c, y] / Σ_c country_price[*, y]
#'   weighted[y]  = Σ_c weight[c, y] * country_growth[c, y]
#'
#' The two YAML blocks must have the SAME country names and the SAME number
#' of forecast years per country.
#'
#' @param country_growth named list  country -> numeric vector, length n_years
#' @param country_prices named list  country -> numeric vector, length n_years
#' @return numeric vector of length n_years
compute_country_weighted_gcc_forecast <- function(country_growth, country_prices) {
  if (length(country_growth) == 0) {
    stop("external_gcc_country_growth is empty")
  }
  countries_g <- names(country_growth)
  countries_p <- names(country_prices)
  if (!setequal(countries_g, countries_p)) {
    stop("Country lists for growth and prices must be identical. ",
         "Mismatch: ", paste(setdiff(union(countries_g, countries_p),
                                       intersect(countries_g, countries_p)),
                              collapse = ", "))
  }
  countries <- countries_g

  growth_mat <- do.call(rbind, lapply(countries, function(c) {
    as.numeric(unlist(country_growth[[c]]))
  }))
  price_mat <- do.call(rbind, lapply(countries, function(c) {
    as.numeric(unlist(country_prices[[c]]))
  }))
  rownames(growth_mat) <- rownames(price_mat) <- countries

  if (any(dim(growth_mat) != dim(price_mat))) {
    stop("growth and prices matrices have mismatched dimensions: ",
         paste(dim(growth_mat), collapse = "x"), " vs ",
         paste(dim(price_mat), collapse = "x"))
  }

  n_years <- ncol(growth_mat)
  out <- numeric(n_years)
  for (y in seq_len(n_years)) {
    w <- price_mat[, y] / sum(price_mat[, y])
    out[y] <- sum(w * growth_mat[, y])
  }
  out
}


#' Internal helper: resolve INTERNAL scenario_weights block (one weight per scenario).
resolve_internal_scenario_weights <- function(block, scenarios,
                                                 non_oil_gdp_history,
                                                 non_oil_gdp_forecasts,
                                                 block_name) {
  mode <- block$mode %||% "explicit"

  if (mode == "explicit") {
    if (is.null(block$explicit_weights)) {
      stop(sprintf("%s.mode='explicit' but no explicit_weights provided", block_name))
    }
    raw <- unlist(block$explicit_weights)
    weights <- as.numeric(raw)
    names(weights) <- names(raw)
    missing_scenarios <- setdiff(scenarios$scenario, names(weights))
    if (length(missing_scenarios) > 0) {
      stop(sprintf("%s.explicit_weights is missing entries for: %s",
                    block_name, paste(missing_scenarios, collapse = ", ")))
    }
    weights <- weights[scenarios$scenario]

  } else if (mode == "auto_non_oil_gdp_cdf") {
    if (is.null(non_oil_gdp_history)) {
      stop(sprintf("%s.mode='auto_non_oil_gdp_cdf' requires non_oil_gdp_history", block_name))
    }
    if (is.null(non_oil_gdp_forecasts) || length(non_oil_gdp_forecasts) == 0) {
      stop(sprintf("%s.mode='auto_non_oil_gdp_cdf' requires non_oil_gdp_forecasts", block_name))
    }
    weights <- compute_internal_scenario_weights(
      historical_series = non_oil_gdp_history,
      forecasts         = non_oil_gdp_forecasts,
      scenarios         = scenarios
    )
  } else {
    stop(sprintf("Unknown %s mode: %s (expected 'explicit' or 'auto_non_oil_gdp_cdf')",
                  block_name, mode))
  }

  if (abs(sum(weights) - 1) > 0.01) {
    warning(sprintf("%s sums to %.4f (expected ~1.0)", block_name, sum(weights)))
  }
  weights
}


#' Internal helper: resolve EXTERNAL scenario_weights block.
#'
#' Returns a list:
#'   $per_year matrix [n_forecast_years × n_scenarios] — V7 rows D69:H69 (year 1)
#'             through D73:H73 (year 5). Used for the first n_forecast_years
#'             columns of the external StPD term structure.
#'   $average  named numeric vector — V7 row D74:H74 = colMeans($per_year).
#'             Used for all maturities beyond n_forecast_years.
resolve_external_scenario_weights <- function(block, scenarios, gcc_history,
                                                 weighted_gcc_forecasts,
                                                 block_name) {
  mode <- block$mode %||% "explicit"

  if (mode == "explicit") {
    if (is.null(block$explicit_weights)) {
      stop(sprintf("%s.mode='explicit' but no explicit_weights provided", block_name))
    }
    raw <- unlist(block$explicit_weights)
    weights <- as.numeric(raw)
    names(weights) <- names(raw)
    missing_scenarios <- setdiff(scenarios$scenario, names(weights))
    if (length(missing_scenarios) > 0) {
      stop(sprintf("%s.explicit_weights is missing entries for: %s",
                    block_name, paste(missing_scenarios, collapse = ", ")))
    }
    weights <- weights[scenarios$scenario]
    # explicit mode: same weights for every year, average == year-1
    n_yrs <- length(weighted_gcc_forecasts)
    per_year <- matrix(rep(weights, each = n_yrs), nrow = n_yrs,
                        dimnames = list(paste0("year_", seq_len(n_yrs)),
                                          names(weights)))
    avg <- weights
    return(list(per_year = per_year, average = avg))
  }

  if (mode == "auto_gcc_cdf") {
    if (is.null(gcc_history)) {
      stop(sprintf("%s.mode='auto_gcc_cdf' requires gcc_history", block_name))
    }
    if (is.null(weighted_gcc_forecasts) || length(weighted_gcc_forecasts) == 0) {
      stop(sprintf("%s.mode='auto_gcc_cdf' requires weighted_gcc_forecasts", block_name))
    }
    res <- compute_external_scenario_weights_per_year(
      weighted_gcc_forecasts = weighted_gcc_forecasts,
      gcc_history            = gcc_history,
      scenarios              = scenarios
    )
    if (any(abs(rowSums(res$per_year) - 1) > 0.01)) {
      warning(sprintf("%s per-year rows do not sum to 1", block_name))
    }
    return(res)
  }

  stop(sprintf("Unknown %s mode: %s (expected 'explicit' or 'auto_gcc_cdf')",
                block_name, mode))
}


#' Compute INTERNAL scenario probability weights per the QDB IFRS 9 Scenarios
#' Probabilities methodology (auxiliary spreadsheet
#' "IFRS_9_Scenarios_Probabilities_2025_V1.xlsx").
#'
#' Algorithm (Calculations sheet, exact formulas):
#'   1. Compute μ, σ from the historical Non-Oil GDP series (2015-2024).
#'        μ = AVERAGE(B2:B11),  σ = STDEV.S(B2:B11)
#'   2. For each forecast year y in {2025, 2026, ...} with forecast F[y]:
#'      For each scenario s with severity_z[s], compute:
#'        threshold[s,y] = F[y] + σ * severity_z[s]
#'      Probabilities for year y, evaluated under N(μ, σ):
#'        P(Sig Down,    y) = Φ(thr_SD ; μ, σ)
#'        P(Slight Down, y) = Φ(thr_SLD; μ, σ) - Φ(thr_SD ; μ, σ)
#'        P(Slight Up,   y) = Φ(thr_SU ; μ, σ) - Φ(thr_SLU; μ, σ)
#'        P(Sig Up,      y) = 1 - Φ(thr_SU ; μ, σ)
#'        P(Base,        y) = 1 - sum(others)        # residual
#'   3. Final weight[s] = mean over forecast years of P(s, y).
#'
#' This is the canonical method that produces V4's hardcoded weights:
#'   [0.15084, 0.17918, 0.47989, 0.11942, 0.07067]
#'
#' Note: assumes the 5-scenario layout with one central (z=0) scenario and
#' two matched ±z pairs around it. Scenarios are ordered by their severity_z
#' internally; output is reordered to match scenarios$scenario.
#'
#' @param historical_series numeric vector of historical Non-Oil GDP values
#' @param forecasts         numeric vector of forecast values, one per year
#' @param scenarios         tibble with cols (scenario, severity_z)
#' @return named numeric vector summing to 1, aligned to scenarios$scenario
compute_internal_scenario_weights <- function(historical_series,
                                                 forecasts, scenarios) {
  mu    <- mean(historical_series)
  sigma <- sd(historical_series)

  ord       <- order(scenarios$severity_z)
  z_sorted  <- scenarios$severity_z[ord]
  n         <- length(z_sorted)
  central_idx <- which.min(abs(z_sorted))

  # Per-year probabilities under N(μ, σ)
  per_year_probs <- vapply(forecasts, function(F) {
    thr <- F + sigma * z_sorted
    cdf <- pnorm(thr, mean = mu, sd = sigma)
    p   <- numeric(n)
    for (i in seq_len(n)) {
      if (i == central_idx) next   # filled by residual below
      if (i < central_idx) {
        # Lower-tail scenarios: band = [Φ(thr[i-1]), Φ(thr[i]))
        lower <- if (i == 1) 0 else cdf[i - 1]
        p[i]  <- cdf[i] - lower
      } else {
        # Upper-tail scenarios: band = [Φ(thr[i]), Φ(thr[i+1]))
        lower <- cdf[i]
        upper <- if (i == n) 1 else cdf[i + 1]
        p[i]  <- upper - lower
      }
    }
    p[central_idx] <- 1 - sum(p)
    p
  }, numeric(n))   # matrix [n_scenarios × n_years]

  # Final = average over forecast years
  if (is.matrix(per_year_probs)) {
    weights_sorted <- rowMeans(per_year_probs)
  } else {
    weights_sorted <- per_year_probs   # single forecast year
  }

  weights <- numeric(n)
  weights[ord] <- weights_sorted
  names(weights) <- scenarios$scenario
  weights
}


# Legacy: deprecated. Kept so any old code referencing this name still
# resolves; the actual production internal-weights method is
# compute_internal_scenario_weights() above.
compute_scenario_weights_normal_cdf <- function(scenarios) {
  ord <- order(scenarios$severity_z)
  scenarios_sorted <- scenarios[ord, ]
  z <- scenarios_sorted$severity_z
  n <- length(z)

  midpoints <- (z[-1] + z[-n]) / 2
  thresholds <- c(-Inf, midpoints, Inf)

  weights_sorted <- pnorm(thresholds[-1]) - pnorm(thresholds[-length(thresholds)])
  names(weights_sorted) <- scenarios_sorted$scenario
  weights_sorted[scenarios$scenario]
}


#' Compute MEV model weights from p-values per the methodology PDF page 16.
#'
#' The PDF formula uses the highest p-value MEV (REPI in their data) as
#' the denominator anchor. We generalise by picking max(p) as the anchor.
#'
#' @param mevs list-of-lists where each element has $p_value
#' @return numeric vector summing to 1
compute_mev_weights_p_value <- function(mevs) {
  pvals <- vapply(mevs, function(m) as.numeric(m$p_value), numeric(1))
  if (any(pvals <= 0)) {
    stop("MEV p-values must all be > 0 to compute auto p-value weights")
  }
  base_p <- max(pvals)
  raw <- base_p / pvals
  raw / sum(raw)
}
