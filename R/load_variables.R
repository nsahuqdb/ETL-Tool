# =============================================================================
# load_variables.R
#
# Loaders for the variable dictionary and model registry. Produces a
# `model_cfg`-compatible object so existing consumers (pd_term_structure,
# macro_model, build_stpd) work without changes.
#
# Key API:
#   load_variable_dictionary(path)        -> list of variable specs by ID
#   load_models(path)                     -> list of model specs + horizons
#   resolve_model(models, model_id, vars) -> model_cfg shape used by code
#
# The "model_cfg shape" produced by resolve_model() matches the legacy
# config/model_config.yml structure:
#   list(
#     model = list(
#       name             = char,
#       ttc_anchor_pd    = num,
#       max_maturity     = int,
#       n_forecasts      = int,
#       mevs = list of list(
#         name, intercept, coefficient, p_value, weight,
#         standard_deviation, stress_unit_multiplier
#       )
#     )
#   )
#
# This indirection keeps the impact surface small: only this loader knows
# about variable_dictionary.yml and models.yml; everything else still
# treats `model_cfg` as a flat config object.
# =============================================================================


#' Load the variable dictionary YAML.
#'
#' @param path  path to variable_dictionary.yml
#' @return  named list keyed by variable id (VAR_*); each entry is the
#'          parsed YAML block plus a normalised `id` field.
load_variable_dictionary <- function(path = "config/variable_dictionary.yml") {
  if (!file.exists(path)) {
    stop("variable_dictionary.yml not found at: ", path)
  }
  raw <- yaml::read_yaml(path)
  if (is.null(raw$variables) || length(raw$variables) == 0) {
    stop("variable_dictionary.yml has no `variables:` block")
  }
  out <- list()
  for (var_id in names(raw$variables)) {
    if (!grepl("^VAR_[A-Z0-9_]+$", var_id)) {
      stop(sprintf("variable id '%s' must match ^VAR_[A-Z0-9_]+$", var_id))
    }
    spec <- raw$variables[[var_id]]
    spec$id <- var_id
    # Defaults
    if (is.null(spec$stress_unit_multiplier)) spec$stress_unit_multiplier <- 1
    if (is.null(spec$deprecated))             spec$deprecated <- FALSE
    out[[var_id]] <- spec
  }
  out
}


#' Load the model registry YAML.
#'
#' @param path  path to models.yml
#' @return list with entries: ttc_anchor_pd, horizons, models (named list).
load_models <- function(path = "config/models.yml") {
  if (!file.exists(path)) {
    stop("models.yml not found at: ", path)
  }
  raw <- yaml::read_yaml(path)
  if (is.null(raw$models) || length(raw$models) == 0) {
    stop("models.yml has no `models:` block")
  }
  if (is.null(raw$ttc_anchor_pd)) {
    stop("models.yml missing top-level `ttc_anchor_pd`")
  }
  if (is.null(raw$horizons)) {
    stop("models.yml missing top-level `horizons`")
  }
  raw
}


#' Compute weights from p-values per PDF methodology page 16:
#'    w_i = (1/p_i) / sum_j (1/p_j)
#' Components with NA / zero / non-numeric p-values are dropped (weight = 0).
.weights_from_p_values <- function(p_values) {
  v <- suppressWarnings(as.numeric(p_values))
  ok <- !is.na(v) & v > 0
  inv <- numeric(length(v))
  inv[ok] <- 1 / v[ok]
  total <- sum(inv)
  if (total == 0) return(rep(0, length(v)))
  inv / total
}


#' Resolve a model by id into the legacy model_cfg shape consumed by
#' pd_term_structure.R / macro_model.R.
#'
#' @param models     output of load_models()
#' @param model_id   key under models$models, e.g. "internal_v4_production"
#' @param dictionary output of load_variable_dictionary()
#' @return list(model = list(name, ttc_anchor_pd, max_maturity, n_forecasts,
#'         mevs = list(...))) — the same shape pd_term_structure currently uses.
resolve_model <- function(models, model_id, dictionary) {
  if (!model_id %in% names(models$models)) {
    stop(sprintf("model_id '%s' not found in models.yml. Available: %s",
                 model_id, paste(names(models$models), collapse = ", ")))
  }
  m <- models$models[[model_id]]
  comps <- m$mev_components %||% list()

  # Look up each component variable in the dictionary; pull units / scale /
  # display name into the resolved record so downstream code never has to
  # cross-reference dictionary itself.
  resolved_mevs <- vector("list", length(comps))
  raw_p_values <- numeric(length(comps))
  for (i in seq_along(comps)) {
    c <- comps[[i]]
    var_id <- c$variable
    if (is.null(var_id)) {
      stop(sprintf("model '%s' component %d has no `variable:` field",
                   model_id, i))
    }
    if (!var_id %in% names(dictionary)) {
      stop(sprintf("model '%s' references unknown variable '%s'. Add it to variable_dictionary.yml",
                   model_id, var_id))
    }
    if (isTRUE(dictionary[[var_id]]$deprecated)) {
      warning(sprintf("model '%s' uses deprecated variable '%s'",
                      model_id, var_id), call. = FALSE)
    }
    spec <- dictionary[[var_id]]
    raw_p_values[i] <- if (is.null(c$p_value)) NA_real_ else as.numeric(c$p_value)
    resolved_mevs[[i]] <- list(
      # legacy `name` field used by macro_model — set to display_name
      name                   = spec$display_name,
      variable_id            = var_id,
      intercept              = c$intercept,
      coefficient            = c$coefficient,
      p_value                = c$p_value,
      weight                 = c$weight,           # may be NULL → derive below
      standard_deviation     = c$standard_deviation,
      # carry through dictionary metadata so tools (Shiny / logs) can
      # render units without a second lookup
      units                  = spec$units,
      scale                  = spec$scale,
      stress_unit_multiplier = spec$stress_unit_multiplier %||% 1
    )
  }

  # Derive weights for any component that left weight = NULL.
  weights_explicit <- vapply(resolved_mevs, function(x) {
    if (is.null(x$weight)) NA_real_ else as.numeric(x$weight)
  }, numeric(1))
  if (any(is.na(weights_explicit))) {
    derived <- .weights_from_p_values(raw_p_values)
    for (i in seq_along(resolved_mevs)) {
      if (is.na(weights_explicit[i])) {
        resolved_mevs[[i]]$weight <- derived[i]
      }
    }
  }

  list(
    model = list(
      name             = paste0(model_id, " — ", m$description %||% ""),
      id               = model_id,
      portfolios       = m$portfolios,
      rating_type      = m$rating_type,
      ttc_anchor_pd    = models$ttc_anchor_pd,
      max_maturity     = models$horizons$max_maturity,
      n_forecasts      = models$horizons$n_forecasts,
      mevs             = resolved_mevs,
      calibration      = m$calibration
    )
  )
}


#' Convenience: load dictionary + models + resolve a model name in one call.
#'
#' @param model_id  e.g. "internal_v4_production"
#' @return  same shape as legacy load_model_config()
resolve_model_config <- function(model_id,
                                  variable_dictionary_path = "config/variable_dictionary.yml",
                                  models_path = "config/models.yml") {
  dict <- load_variable_dictionary(variable_dictionary_path)
  models <- load_models(models_path)
  resolve_model(models, model_id, dict)
}
