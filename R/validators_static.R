# ============================================================================
# R/validators_static.R
#
# Validators for static reference files and the run config itself.
# Until H18, the pre-run check only inspected INPUT files. But config drift
# and missing/empty static reference files cause silent damage downstream
# (e.g. a missing master_rating_scale row would make StPD bucketing wrong
# in subtle ways). These validators surface those problems during the
# pre-run check, before phase 1 even reads any inputs.
#
# Public:
#   build_static_validators()
#   build_config_validators()
#
# Both are tagged "pre_run" so they run alongside the input validators.
#
# All validators take args = list(inputs, static, run_cfg, config_path,
#                                  snapshot_meta, ...) and return
#                          list(passed, details = list(message), severity).
# ============================================================================


# Helper: a validator wrapper compatible with run_validation_suite. Mirrors
# `make_validator` from validators_input.R; we duplicate so this file can
# stand alone.
.make_static_validator <- function(id, severity = "ERROR", context = "STATIC",
                                    description, rationale = NA_character_,
                                    remediation = NA_character_,
                                    tags = c("pre_run"),
                                    suppressible = TRUE,
                                    fn) {
  list(
    id = id, severity = severity, context = context,
    description = description, rationale = rationale,
    remediation = remediation, tags = tags,
    suppressible = suppressible, fn = fn
  )
}


#' Validators for static reference files. Runs after `load_static_reference`
#' has produced the `static` list; we check that each expected file produced
#' a non-empty tibble with the right column shape.
#'
#' This catches: corrupted CSVs, empty files (header-only), wrong column
#' names from a manual edit, accidentally-deleted files, and files where
#' the encoding broke during a copy.
build_static_validators <- function() {
  # Required static files. Optional ones (from STATIC_FILE_SPECS$optional
  # = TRUE) are exempt from the "must have rows" rule.
  required_files <- c(
    "off_balance_products", "industry_sector_mapping",
    "collective_assessment_rules", "product_portfolio_mapping",
    "staging_thresholds", "master_rating_scale",
    "master_rating_downgrade", "scenario_severity",
    "ttc_pd_table", "portfolios"
  )

  v <- list()
  for (key in required_files) {
    local({
      k <- key
      v[[length(v) + 1]] <<- .make_static_validator(
        id = sprintf("STATIC_%s_present", k),
        severity = "ERROR",
        context = "STATIC",
        description = sprintf("Static reference file '%s' is loaded with at least one row",
                              k),
        rationale = paste0(
          "Static reference data drives bucketing, rating-scale lookups, ",
          "scenario severity, and TTC PD assignment. A missing or empty ",
          "file produces silently wrong results downstream — a row that ",
          "should have an investment-grade rating gets NA, a stage-2 ",
          "trigger gets missed, etc."),
        remediation = paste0(
          "Restore the file from your reference data source. The shape ",
          "expected is in STATIC_FILE_SPECS in R/load_static.R."),
        tags = c("pre_run"),
        suppressible = FALSE,
        fn = function(static, ...) {
          df <- static[[k]]
          if (is.null(df)) {
            return(list(passed = FALSE,
                         details = list(message = sprintf(
                           "Static file '%s' did not load — check static_dir.", k))))
          }
          # Generic "has data" check — some files (e.g. staging_thresholds)
          # load as a named list rather than a tibble, so nrow() returns
          # NULL and we need a structure-agnostic check.
          n <- if (is.data.frame(df))      nrow(df)
               else if (is.list(df))      length(df)
               else if (is.atomic(df))    length(df)
               else                       NA_integer_
          if (is.na(n) || n == 0) {
            return(list(passed = FALSE,
                         details = list(message = sprintf(
                           "Static file '%s' is empty.", k))))
          }
          list(passed = TRUE)
        }
      )
    })
  }

  # ---- master_rating_scale: must include the IFRS9 default rating(s) -----
  v[[length(v) + 1]] <- .make_static_validator(
    id = "STATIC_master_rating_scale_has_default",
    severity = "WARN",
    context = "STATIC",
    description = "master_rating_scale.csv contains the default rating buckets used downstream",
    rationale = paste0(
      "Stage classification and ECL allocation rely on the rating scale ",
      "containing the standard default rating codes — `QDB 9` on the QDB ",
      "internal scale and `C` on the external (Moody's-equivalent) scale. ",
      "If those rows are missing, defaulted contracts get NA when their ",
      "rating is looked up and are silently dropped from Stage 3."),
    remediation = paste0(
      "Verify master_rating_scale.csv has rows for the default ratings ",
      "used in your scale. Default QDB grade is 'QDB 9'; default external ",
      "grade is 'C' (Moody's-equivalent)."),
    tags = c("pre_run"),
    suppressible = TRUE,
    fn = function(static, ...) {
      df <- static$master_rating_scale
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
        return(list(passed = TRUE))
      }
      ratings <- toupper(as.character(df$rating %||% character()))
      # Match any of: "QDB 9", "QDB9", "C" (Moody's default), "Ca", "Caa3",
      # "D", "Default" — covers QDB internal + Moody's external + generic
      patterns <- c("^QDB ?9$", "^C$", "^CA$", "^CAA3$",
                    "^D$", "^DEFAULT$")
      has_default <- any(vapply(patterns,
                                  function(p) any(grepl(p, ratings)),
                                  logical(1)))
      list(passed = has_default,
           details = if (!has_default) {
                       list(message = paste0(
                         "No default-rating row in master_rating_scale.csv ",
                         "— expected one of: QDB 9, C, Ca, Caa3, D, Default"))
                     } else NULL)
    }
  )

  # ---- ttc_pd_table: pd values are in [0, 1] -----------------------------
  v[[length(v) + 1]] <- .make_static_validator(
    id = "STATIC_ttc_pd_in_unit_interval",
    severity = "ERROR",
    context = "STATIC",
    description = "All ttc_pd_table values are probabilities in [0, 1]",
    rationale = paste0(
      "TTC PDs are probabilities. Values outside [0,1] indicate the file ",
      "was edited as percentages (e.g. 1.5 meaning 1.5%) without being ",
      "converted to the decimal form the pipeline expects, which would ",
      "produce nonsensical ECLs."),
    remediation = "Convert any percentage values to decimals (divide by 100).",
    tags = c("pre_run"),
    suppressible = TRUE,
    fn = function(static, ...) {
      df <- static$ttc_pd_table
      if (is.null(df) || nrow(df) == 0) return(list(passed = TRUE))
      pd_cols <- grep("(?i)pd", colnames(df), value = TRUE)
      if (length(pd_cols) == 0) return(list(passed = TRUE))
      for (cn in pd_cols) {
        x <- suppressWarnings(as.numeric(df[[cn]]))
        x <- x[!is.na(x)]
        if (length(x) > 0 && (min(x) < 0 || max(x) > 1)) {
          return(list(passed = FALSE,
                       details = list(message = sprintf(
                         "Column '%s' has values outside [0,1] (range %.4f .. %.4f)",
                         cn, min(x), max(x)))))
        }
      }
      list(passed = TRUE)
    }
  )

  # ---- scenario_severity: probabilities sum to 1 -------------------------
  v[[length(v) + 1]] <- .make_static_validator(
    id = "STATIC_scenario_probs_sum_to_one",
    severity = "WARN",
    context = "STATIC",
    description = "Scenario probabilities sum to ~1 (within 0.001 tolerance)",
    rationale = paste0(
      "ECL is a probability-weighted sum across scenarios. If the weights ",
      "don't sum to 1, the resulting ECL is silently scaled wrong."),
    remediation = "Adjust scenario_severity.csv weights to sum to 1.000.",
    tags = c("pre_run"),
    suppressible = TRUE,
    fn = function(static, ...) {
      df <- static$scenario_severity
      if (is.null(df) || nrow(df) == 0) return(list(passed = TRUE))
      prob_col <- intersect(c("probability", "weight", "prob"), colnames(df))
      if (length(prob_col) == 0) return(list(passed = TRUE))
      x <- suppressWarnings(as.numeric(df[[prob_col[1]]]))
      x <- x[!is.na(x)]
      if (length(x) == 0) return(list(passed = TRUE))
      total <- sum(x)
      ok <- abs(total - 1.0) < 0.001
      list(passed = ok,
           details = if (!ok) {
                       list(message = sprintf(
                         "Scenario probabilities sum to %.4f (expected ~1.000)",
                         total))
                     } else NULL)
    }
  )

  v
}


#' Validators for the run config itself. These check shape — that required
#' keys exist and point at things that exist on disk. They run after
#' `load_run_config` has parsed the file (which itself enforces top-level
#' keys), so we mostly check path resolution and presence.
build_config_validators <- function() {
  v <- list()

  # ---- Required path entries point at existing files/directories -------
  v[[length(v) + 1]] <- .make_static_validator(
    id = "CONFIG_required_paths_exist",
    severity = "ERROR",
    context = "CONFIG",
    description = "Required paths in config.yml's `paths:` block resolve to existing files / directories",
    rationale = paste0(
      "Some path entries describe runtime locations (input_dir, ",
      "output_dir, runs_dir) that may legitimately not exist yet — ",
      "e.g. when inputs are uploaded via the app, or the runs/ folder ",
      "is created on first run. But static_dir + the model/dictionary ",
      "YAMLs MUST exist; their absence indicates a broken project."),
    remediation = "Ensure static_dir and the model/dictionary YAML paths in config.yml exist on disk.",
    tags = c("pre_run"),
    suppressible = FALSE,
    fn = function(run_cfg, ...) {
      if (is.null(run_cfg$paths)) {
        return(list(passed = FALSE,
                     details = list(message = "config.yml has no `paths:` block")))
      }
      # Only these paths are mandatory at pre-run time. Everything else
      # (input_dir, output_dir, runs_dir, data_drop_root, reference_outputs,
      # log_file) is either created at run time or overridable.
      required_keys <- c("static_dir", "variable_dictionary",
                          "models", "model_inputs")
      missing <- character()
      for (k in required_keys) {
        v <- run_cfg$paths[[k]]
        if (is.null(v) || !is.character(v) || length(v) != 1 || !nzchar(v)) {
          missing <- c(missing, sprintf("paths$%s = (unset)", k))
          next
        }
        if (!file.exists(v) && !dir.exists(v)) {
          missing <- c(missing, sprintf("paths$%s = '%s'", k, v))
        }
      }
      list(passed = length(missing) == 0,
           details = if (length(missing) > 0) {
                       list(message = paste("Missing/unresolvable required paths:",
                                             paste(missing, collapse = "; ")))
                     } else NULL)
    }
  )

  # ---- Soft-warn for optional paths ------------------------------------
  v[[length(v) + 1]] <- .make_static_validator(
    id = "CONFIG_optional_paths_resolve",
    severity = "WARN",
    context = "CONFIG",
    description = "Optional paths (input_dir, reference_outputs) resolve to existing locations, if set",
    rationale = paste0(
      "input_dir is used for the configured-source flow; if it doesn't ",
      "exist, you must use the upload or data-drop flow. ",
      "reference_outputs enables reconciliation against the Excel tool ",
      "output; if set but missing, no reconciliation will run."),
    remediation = "Either point at the right location, or leave unset if you don't need it.",
    tags = c("pre_run"),
    suppressible = TRUE,
    fn = function(run_cfg, ...) {
      if (is.null(run_cfg$paths)) return(list(passed = TRUE))
      missing <- character()
      for (k in c("input_dir", "reference_outputs", "data_drop_root")) {
        v <- run_cfg$paths[[k]]
        if (is.null(v) || !is.character(v) || length(v) != 1 || !nzchar(v)) next
        if (!file.exists(v) && !dir.exists(v)) {
          missing <- c(missing, sprintf("paths$%s = '%s'", k, v))
        }
      }
      list(passed = length(missing) == 0,
           details = if (length(missing) > 0) {
                       list(message = paste("Optional paths set but not found:",
                                             paste(missing, collapse = "; ")))
                     } else NULL)
    }
  )

  # ---- Required `run` block keys -----------------------------------------
  v[[length(v) + 1]] <- .make_static_validator(
    id = "CONFIG_run_block_complete",
    severity = "ERROR",
    context = "CONFIG",
    description = "config.yml's `run:` block has the keys phase1 expects (internal_model, extract_date)",
    rationale = paste0(
      "The pipeline picks the model from run$internal_model and uses ",
      "run$extract_date to time-align the input snapshot. Missing either ",
      "produces a confusing failure during model resolution."),
    remediation = "Add internal_model and extract_date under config.yml run:",
    tags = c("pre_run"),
    suppressible = FALSE,
    fn = function(run_cfg, ...) {
      missing <- character()
      if (is.null(run_cfg$run)) {
        return(list(passed = FALSE,
                     details = list(message = "config.yml has no `run:` block")))
      }
      for (k in c("internal_model", "extract_date")) {
        v <- run_cfg$run[[k]]
        if (is.null(v) || !nzchar(as.character(v))) {
          missing <- c(missing, k)
        }
      }
      list(passed = length(missing) == 0,
           details = if (length(missing) > 0) {
                       list(message = paste("Missing run.* keys:",
                                             paste(missing, collapse = ", ")))
                     } else NULL)
    }
  )

  v
}
