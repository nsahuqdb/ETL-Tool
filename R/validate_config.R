# ============================================================================
# R/validate_config.R
#
# Cross-cutting validators for the static reference data + model config.
# These are the checks that catch SILENTLY BROKEN config — wrong values that
# parse fine but make the macro model produce nonsense.
#
# Run after load_static_reference() and load_model_config() but before any
# downstream phase touches the data.
#
# Public API:
#   validate_static_reference(static)   -> tibble of issues
#   validate_model_config(model_cfg)    -> tibble of issues
#   validate_consistency(static, model_cfg) -> tibble of issues
#
# Each returns a tibble of (check, severity, message). Empty = all pass.
# ============================================================================


# --- helpers ----------------------------------------------------------------

new_issue_log <- function() {
  list()
}

add_issue <- function(log, check, severity, message) {
  log[[length(log) + 1L]] <- tibble::tibble(
    check = check, severity = severity, message = message
  )
  log
}

finalise_issues <- function(log) {
  if (length(log) == 0L) {
    return(tibble::tibble(check = character(),
                          severity = character(),
                          message = character()))
  }
  dplyr::bind_rows(log)
}

approx_equal <- function(x, y, tol = 1e-6) {
  isTRUE(abs(x - y) < tol)
}


# --- validate_static_reference ----------------------------------------------

#' Validate the bundle returned by load_static_reference().
#'
#' Checks that catch silently broken data:
#'  - scenario_severity: probability weights sum to 1.0
#'  - scenario_severity: severity z-scores symmetric around 0
#'  - master_rating_scale: hierarchy is contiguous 1..N within rating_type
#'  - master_rating_downgrade: downgrade target exists in master_rating_scale
#'  - staging_thresholds: required keys present and numeric where expected
#'
#' @param static Output of load_static_reference()
#' @return Tibble of issues
validate_static_reference <- function(static) {
  log <- new_issue_log()

  # ------ scenario_severity ------------------------------------------------
  ss <- static$scenario_severity
  if (!is.null(ss)) {
    weight_sum <- sum(ss$scenario_probability_weight, na.rm = TRUE)
    if (!approx_equal(weight_sum, 1.0, tol = 1e-4)) {
      log <- add_issue(log, "scenario_severity.weights", "ERROR",
        sprintf("scenario_probability_weight sum = %.6f (expected 1.0)", weight_sum))
    }
    # Z-scores symmetric around 0
    z <- sort(ss$severity_z)
    if (length(z) == 5L) {
      sym_err <- abs(z[1] + z[5]) + abs(z[2] + z[4]) + abs(z[3])
      if (sym_err > 1e-4) {
        log <- add_issue(log, "scenario_severity.symmetry", "WARNING",
          sprintf("severity_z not symmetric around 0 (sum of mirror pairs = %.6f)", sym_err))
      }
    }
    # Base case present
    if (!"Base Case" %in% ss$scenario) {
      log <- add_issue(log, "scenario_severity.base_case", "ERROR",
        "'Base Case' scenario row is missing")
    }
  }

  # ------ master_rating_scale ----------------------------------------------
  mrs <- static$master_rating_scale
  if (!is.null(mrs)) {
    for (rt in unique(mrs$rating_type)) {
      hier <- sort(mrs$hierarchy[mrs$rating_type == rt])
      expected <- seq_along(hier)
      if (!identical(as.integer(hier), as.integer(expected))) {
        log <- add_issue(log, paste0("master_rating_scale.hierarchy.", rt), "WARNING",
          sprintf("hierarchy for rating_type='%s' is not contiguous 1..N (got %s)",
                  rt, paste(hier, collapse = ",")))
      }
    }
  }

  # ------ master_rating_downgrade ------------------------------------------
  mrd <- static$master_rating_downgrade
  if (!is.null(mrd) && !is.null(mrs)) {
    valid_ratings <- unique(mrs$rating)
    bad_targets <- setdiff(unique(mrd$rating_after_1_notch_downgrade), valid_ratings)
    bad_targets <- bad_targets[!is.na(bad_targets) & nzchar(bad_targets)]
    if (length(bad_targets) > 0) {
      log <- add_issue(log, "master_rating_downgrade.targets", "ERROR",
        sprintf("downgrade target(s) not in master_rating_scale: %s",
                paste(bad_targets, collapse = ", ")))
    }
    # Source ratings should also exist
    bad_sources <- setdiff(unique(mrd$rating), valid_ratings)
    if (length(bad_sources) > 0) {
      log <- add_issue(log, "master_rating_downgrade.sources", "WARNING",
        sprintf("downgrade source(s) not in master_rating_scale: %s",
                paste(bad_sources, collapse = ", ")))
    }
  }

  # ------ staging_thresholds -----------------------------------------------
  st <- static$staging_thresholds
  if (!is.null(st)) {
    required_keys <- c("dpd_stage2_threshold_days", "maturity_extension_days")
    missing_keys <- setdiff(required_keys, names(st))
    if (length(missing_keys) > 0) {
      log <- add_issue(log, "staging_thresholds.keys", "ERROR",
        sprintf("missing keys: %s", paste(missing_keys, collapse = ", ")))
    }
    # DPD threshold should be in (0, 90)
    if (!is.null(st$dpd_stage2_threshold_days)) {
      v <- st$dpd_stage2_threshold_days
      if (!is.numeric(v) || v <= 0 || v >= 90) {
        log <- add_issue(log, "staging_thresholds.dpd_range", "ERROR",
          sprintf("dpd_stage2_threshold_days must be in (0, 90), got %s", v))
      }
    }
  }

  finalise_issues(log)
}


# --- validate_model_config --------------------------------------------------

#' Validate the macro model config bundle.
#'
#' Checks:
#'  - MEV weights sum to 1.0
#'  - ttc_anchor_pd is in (0, 1)
#'  - max_maturity is positive integer
#'  - all standard_deviations are positive
#'  - GCC distribution mean / sd are finite, sd > 0
#'  - stress_unit_multiplier is one of the supported values (1 or 100)
#'
#' @param model_cfg Output of load_model_config()
#' @return Tibble of issues
validate_model_config <- function(model_cfg) {
  log <- new_issue_log()

  # MEV weight sum
  weights <- vapply(model_cfg$model$mevs, function(m) m$weight, numeric(1))
  if (!approx_equal(sum(weights), 1.0, tol = 1e-6)) {
    log <- add_issue(log, "model.mev_weights", "ERROR",
      sprintf("MEV weights sum to %.6f, expected 1.0", sum(weights)))
  }

  # ttc_anchor_pd
  ttc <- model_cfg$model$ttc_anchor_pd
  if (!is.numeric(ttc) || ttc <= 0 || ttc >= 1) {
    log <- add_issue(log, "model.ttc_anchor_pd", "ERROR",
      sprintf("ttc_anchor_pd must be in (0,1), got %s", ttc))
  }

  # max_maturity
  mm <- model_cfg$model$max_maturity
  if (!is.numeric(mm) || mm <= 0 || mm != as.integer(mm)) {
    log <- add_issue(log, "model.max_maturity", "ERROR",
      sprintf("max_maturity must be positive integer, got %s", mm))
  }

  # MEV per-element sanity
  for (i in seq_along(model_cfg$model$mevs)) {
    m <- model_cfg$model$mevs[[i]]
    if (!is.numeric(m$standard_deviation) || m$standard_deviation <= 0) {
      log <- add_issue(log, sprintf("model.mevs[%d].standard_deviation", i), "ERROR",
        sprintf("MEV '%s' standard_deviation must be > 0, got %s",
                m$name, m$standard_deviation))
    }
    if (!m$stress_unit_multiplier %in% c(1, 100)) {
      log <- add_issue(log, sprintf("model.mevs[%d].stress_unit_multiplier", i),
        "WARNING",
        sprintf("MEV '%s' stress_unit_multiplier=%s is unusual (expected 1 or 100)",
                m$name, m$stress_unit_multiplier))
    }
  }

  # GCC distribution
  gcc <- model_cfg$gcc_growth_distribution
  if (!is.finite(gcc$mean) || !is.finite(gcc$standard_deviation) ||
      gcc$standard_deviation <= 0) {
    log <- add_issue(log, "gcc_growth_distribution", "ERROR",
      sprintf("invalid mean=%s sd=%s",
              gcc$mean, gcc$standard_deviation))
  }

  finalise_issues(log)
}


# --- validate_consistency ---------------------------------------------------

#' Cross-validation between static reference and model config.
#'
#' Checks the two layers agree:
#'  - unrated_fallback_rating from model_cfg exists in master_rating_scale
#'  - scenarios in scenario_severity match the count expected by max_maturity logic
#'
#' @param static    Output of load_static_reference()
#' @param model_cfg Output of load_model_config()
#' @return Tibble of issues
validate_consistency <- function(static, model_cfg) {
  log <- new_issue_log()

  # Segment fallback ratings exist in master_rating_scale
  if (!is.null(static$segment_fallback_ratings)) {
    fb <- static$segment_fallback_ratings$fallback_rating
    valid <- static$master_rating_scale$rating
    bad <- setdiff(fb[!is.na(fb) & nzchar(fb)], valid)
    if (length(bad) > 0) {
      log <- add_issue(log, "consistency.segment_fallback", "ERROR",
        sprintf("segment fallback rating(s) not in master_rating_scale: %s",
                paste(bad, collapse = ", ")))
    }
  }

  # TTC PD table: ratings must exist in master_rating_scale, ttc_pd in (0,1)
  if (!is.null(static$ttc_pd_table)) {
    valid <- static$master_rating_scale$rating
    bad_r <- setdiff(static$ttc_pd_table$rating, valid)
    if (length(bad_r) > 0) {
      log <- add_issue(log, "consistency.ttc_pd_table.ratings", "WARNING",
        sprintf("rating(s) in ttc_pd_table not in master_rating_scale: %s",
                paste(bad_r, collapse = ", ")))
    }
    bad_pd <- static$ttc_pd_table$ttc_pd
    bad_pd <- bad_pd[!is.na(bad_pd) & (bad_pd <= 0 | bad_pd >= 1)]
    if (length(bad_pd) > 0) {
      log <- add_issue(log, "consistency.ttc_pd_table.values", "ERROR",
        sprintf("ttc_pd_table contains %d values outside (0,1)",
                length(bad_pd)))
    }
  }

  # Number of scenarios should be 5 (the Excel tool hardcodes 5 in
  # GeneratePDTermStructureInternal: For i = 1 To 5)
  n_scen <- nrow(static$scenario_severity)
  if (n_scen != 5L) {
    log <- add_issue(log, "consistency.n_scenarios", "WARNING",
      sprintf("expected 5 scenarios, found %d (Excel tool's PD generation loop is hardcoded to 5)",
              n_scen))
  }

  finalise_issues(log)
}
