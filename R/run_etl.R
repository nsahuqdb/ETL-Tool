# =============================================================================
# run_etl.R
#
# Top-level orchestrator. ONE entry point that the test runner, CLI tools,
# and Shiny all call. Equivalent to:
#
#   1. Load config / static / overrides / inputs (with manifest-friendly hashing)
#   2. Build transformation tibbles (lending + investments)
#   3. Build portfolio views (applies rating/stage/restructuring overrides)
#   4. Build derived outputs (PD term structure, lifetime params, StPD)
#   5. Build intermediate tibbles (customer_flags, collateral, ACA, inv customers)
#   6. Write 18 output CSVs
#   7. Write per-run manifest (timestamps, hashes, totals)
#   8. Optionally run reconciliation against a reference output dir
#
# Returns a single results object capturing inputs, intermediate tibbles,
# manifest path, output paths, reconciliation summary. Shiny uses this to
# show the UI; CLI / tests just inspect the manifest path.
#
# Key design rule: NO global state. Every input is a parameter. The function
# is reentrant — Shiny can call it on every override-CSV save and get a
# fully fresh run.
# =============================================================================

# %||% helper used throughout this file. Define early in case a caller
# sources this file in isolation.
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}


#' Build the country-weighted GCC GDP history from static reference tables.
#'
#' Pulled out of the test runner so run_etl can call it. Internal helper.
.build_gcc_history <- function(static) {
  growth <- static$gcc_real_gdp_growth
  prices <- static$gcc_gdp_current_prices
  g <- tidyr::pivot_wider(growth, names_from = country, values_from = value)
  p <- tidyr::pivot_wider(prices, names_from = country, values_from = value)
  yrs <- intersect(g$year, p$year); yrs <- yrs[yrs >= 1982]
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


#' Default StPD portfolios x rating-types.
.default_stpd_portfolios <- function() {
  tibble::tribble(
    ~portfolio,         ~rating_type,
    "Business Finance", "Internal",
    "Off BS",           "Internal",
    "Al Dhameen",       "Internal",
    "Tasdeer",          "Internal",
    "Banks and Fis",    "External",
    "Investments",      "External"
  )
}


#' Run the full IFRS9 ETL pipeline end-to-end.
#'
#' @param config_path path to run config (config.yml). Default = "config.yml".
#' @param output_root override the output-root directory. The orchestrator
#'        creates `<root>/Output/` for the 18 CSVs and `<root>/reports/`
#'        for manifest, summary, mismatches. If NULL, uses
#'        run_cfg$paths$output_root (preferred) or run_cfg$paths$output_dir
#'        for backwards compatibility. Defaults to "test_output" if neither
#'        is set.
#' @param reconcile if TRUE and run_cfg$paths$reference_outputs is set,
#'        run reconciliation against the reference dir and dump mismatches.
#'        Default TRUE.
#' @param verbose if TRUE, print stage-by-stage progress.
#' @param snapshot label of a snapshot under config_snapshots/ (or full path
#'        to a snapshot directory). When set, ALL config + static paths are
#'        resolved from inside the snapshot and `config_path` is ignored.
#'        The snapshot's recorded code SHA is compared against the current
#'        code SHA; a warning is emitted if they differ.
#' @param run_validation if TRUE (default), run input/transform/derived
#'        validators at each gate. Set FALSE to skip — useful for very
#'        fast iteration during development. Validation results are
#'        always returned in the result list when this is TRUE.
#' @param keep_history if TRUE, write outputs to a timestamped subdirectory
#'        under `runs_dir/<timestamp>/` (or `<output_root>/runs/<timestamp>/`
#'        if `runs_dir` is not set). Each run is preserved on disk so the
#'        Shiny app can browse run history. Default FALSE (the legacy
#'        single-folder mode used by tests). Set TRUE for production
#'        / Shiny-friendly runs.
#' @param runs_dir optional path to a `runs/` directory holding multiple
#'        timestamped runs. Only consulted when keep_history=TRUE. Falls
#'        back to `getOption("ifrs9.runs_dir", "runs")` if NULL.
#' @return list with:
#'   - run_dir        : output root dir (with /Output, /reports subdirs)
#'   - output_paths   : named list of all output CSV paths
#'   - manifest_path  : path to manifest.json
#'   - reconciliation : NULL or the result of reconcile_output_dirs()
#'   - intermediates  : the working tibbles (cm_view, trans_l, etc.) so
#'                      Shiny can display them without a re-run
#'   - validation     : tibble of all validation results (NULL if skipped)
#'   - duration_seconds
#'   - snapshot       : snapshot metadata (if a snapshot was used)
#'   - code_sha       : current code SHA at run time
#'   - run_id         : unique identifier for this run
run_etl <- function(config_path = "config.yml",
                     output_root = NULL,
                     reconcile = TRUE,
                     verbose = TRUE,
                     snapshot = NULL,
                     run_validation = TRUE,
                     keep_history = FALSE,
                     runs_dir = NULL) {

  run_started <- Sys.time()
  msg_log <- character()
  log_msg <- function(...) {
    s <- sprintf(...)
    msg_log <<- c(msg_log, s)
    if (verbose) message(s)
  }

  # ---- Snapshot resolution (optional) ----------------------------------
  snapshot_meta <- NULL
  if (!is.null(snapshot)) {
    snapshot_meta <- read_snapshot_metadata(snapshot)
    sp <- snapshot_paths(snapshot)
    log_msg("[run_etl] using snapshot %s (status=%s)",
             snapshot_meta$label %||% snapshot, snapshot_meta$status %||% "?")
    config_path <- sp$config_yml
    # Code-version check
    cv <- compare_code_to_snapshot(snapshot_meta, verbose = TRUE)
    if (!is.null(cv$current) && !is.na(cv$current)) {
      log_msg("[run_etl] code sha=%s; snapshot recorded sha=%s",
               substr(cv$current, 1, 12), substr(cv$snapshot %||% "", 1, 12))
    }
  }

  # ---- Load config / static / inputs -----------------------------------
  log_msg("[run_etl] loading config from %s", config_path)
  run_cfg   <- load_run_config(config_path)

  # When a snapshot is in use, override every path in run_cfg$paths to point
  # inside the snapshot. This guarantees the run cannot accidentally read
  # live files even if config.yml inside the snapshot has stale paths.
  if (!is.null(snapshot_meta)) {
    sp <- snapshot_paths(snapshot)
    run_cfg$paths$static_dir          <- sp$static_dir
    run_cfg$paths$variable_dictionary <- sp$variable_dictionary
    run_cfg$paths$models              <- sp$models
    run_cfg$paths$model_inputs        <- sp$model_inputs
    # input_dir stays as configured (inputs are NOT part of the snapshot —
    # they are the raw source data that varies per run)
  }

  # Load model config — the loader routes to models.yml + variable_dictionary.yml
  # when the new paths are present in config.yml; otherwise falls back to legacy.
  model_cfg <- load_model_config(config_path)
  static    <- load_static_reference(run_cfg$paths$static_dir)

  current_code_sha <- tryCatch(get_current_code_sha(),
                                error = function(e) NA_character_)

  # Run identifier — derived from start time. Stable for the duration of
  # this function call. Referenced by audit events, manifest, and the
  # output-path resolution below.
  run_id <- format(run_started, "%Y-%m-%d_%H-%M-%S")

  # ---- Audit: log run start --------------------------------------------
  audit_event(list(
    event       = "run_start",
    run_id      = run_id,
    snapshot    = snapshot_meta$label %||% NA_character_,
    code_sha    = current_code_sha,
    config_path = config_path,
    started_at  = format(run_started, "%Y-%m-%dT%H:%M:%S%z")
  ))

  # ---- Output location resolution -------------------------------------
  # Two modes:
  #   keep_history = FALSE (default; test harness): single folder, gets
  #     overwritten on next run. Path = output_root.
  #   keep_history = TRUE (Shiny-friendly):  <runs_dir>/<run_id>/.
  #     Each run preserved indefinitely; the Shiny app browses runs_dir.
  #
  # IMPORTANT: runs_dir is INDEPENDENT of output_root. They serve different
  # purposes — output_root is a single overwriting folder for batch /
  # legacy use; runs_dir is a permanent archive of timestamped runs for
  # the app. Conflating them causes runs to land where the app doesn't
  # look. Resolution order for runs_dir, when keep_history=TRUE:
  #   1. argument runs_dir
  #   2. run_cfg$paths$runs_dir
  #   3. options("ifrs9.runs_dir")  (set in app.R)
  #   4. environment IFRS9_RUNS_DIR
  #   5. "runs"  (relative to working directory)
  if (isTRUE(keep_history)) {
    base <- runs_dir %||%
             run_cfg$paths$runs_dir %||%
             getOption("ifrs9.runs_dir",
                        default = Sys.getenv("IFRS9_RUNS_DIR", "runs"))
    output_root <- file.path(base, run_id)
    log_msg("[run_etl] keep_history=TRUE -> %s", output_root)
  } else {
    # Single-folder mode (legacy / test harness)
    output_root <- output_root %||%
                    run_cfg$paths$output_root %||%
                    run_cfg$paths$output_dir  %||%
                    "test_output"
  }
  output_dir  <- file.path(output_root, "Output")
  reports_dir <- file.path(output_root, "reports")
  dir.create(output_dir,  showWarnings = FALSE, recursive = TRUE)
  dir.create(reports_dir, showWarnings = FALSE, recursive = TRUE)

  log_msg("[run_etl] building macro inputs")
  gcc_history <- .build_gcc_history(static)
  model_inputs <- load_model_inputs(
    path                = run_cfg$paths$model_inputs,
    model_cfg           = model_cfg,
    scenarios           = static$scenario_severity,
    gcc_history         = gcc_history,
    non_oil_gdp_history = static$non_oil_gdp_history$value
  )

  log_msg("[run_etl] reading inputs from %s", run_cfg$paths$input_dir)
  inputs <- read_all_inputs(run_cfg$paths$input_dir, verbose = FALSE)

  # ---- Suppressions -----------------------------------------------------
  # Resolved with the same precedence as other config files: snapshot
  # > run_cfg path > default. The list is shared by all three validation
  # gates. A suppression downgrades a failed validator's effective_severity
  # to "INFO" so gating doesn't halt — but the failure is still recorded.
  suppressions_path <- run_cfg$paths$validation_suppressions %||%
                        file.path(dirname(config_path),
                                   "config", "validation_suppressions.yml")
  if (!is.null(snapshot_meta)) {
    suppressions_path <- file.path(snapshot_meta$path,
                                    "config", "validation_suppressions.yml")
  }
  suppressions_tbl <- load_suppressions(suppressions_path)
  suppression_ids  <- active_suppression_ids(suppressions_tbl)
  if (length(suppression_ids) > 0) {
    log_msg("[run_etl] %d active validation suppression(s)",
             length(suppression_ids))
  }

  # ---- VALIDATION GATE 1: inputs ---------------------------------------
  # Runs all validators registered in validators_input.R. Behaviour on
  # failure is controlled by run_cfg$run$on_validation_error
  # (default "stop"; "warn" continues with a warning; "ignore" is silent).
  v_inputs <- NULL
  if (isTRUE(run_validation)) {
    log_msg("[run_etl] validation: inputs")
    v_inputs <- run_validation_suite(
      "INPUT",
      build_input_validators(),
      args = list(inputs = inputs, static = static, run_cfg = run_cfg),
      verbose = verbose,
      suppressions = suppression_ids
    )
    .gate_on_validation(v_inputs, "INPUT", run_cfg, log_msg, run_id = run_id)
  } else {
    log_msg("[run_etl] validation: SKIPPED (run_validation=FALSE)")
  }

  # ---- Transformation + portfolio views ---------------------------------
  log_msg("[run_etl] transformation: lending")
  trans_l <- build_transformation_lending(inputs, static, model_cfg, run_cfg)
  out_l   <- build_lending_portfolio_view(trans_l, inputs, static, model_cfg)
  cm_view <- out_l$portfolio
  trans_l <- out_l$transformation

  log_msg("[run_etl] transformation: investments")
  trans_i  <- build_transformation_investments(inputs, static, model_cfg, run_cfg)
  out_i    <- build_investment_portfolio_view(trans_i, inputs, static)
  inv_view <- out_i$portfolio
  trans_i  <- out_i$transformation

  # ---- VALIDATION GATE 2: transformation ------------------------------
  v_transform <- NULL
  if (isTRUE(run_validation)) {
    log_msg("[run_etl] validation: transformation")
    v_transform <- run_validation_suite(
      "TRANSFORM",
      build_transform_validators(),
      args = list(trans_l = trans_l, cm_view = cm_view,
                  trans_i = trans_i, inv_view = inv_view,
                  static = static, model_cfg = model_cfg),
      verbose = verbose,
      suppressions = suppression_ids
    )
    .gate_on_validation(v_transform, "TRANSFORM", run_cfg, log_msg, run_id = run_id)
  }

  # ---- Derived outputs -------------------------------------------------
  log_msg("[run_etl] derived: lifetime parameter other")
  ltpo <- build_lifetime_parameter_other(inputs$RepaymentSchedule,
                                           trans_l, run_cfg)

  log_msg("[run_etl] derived: PD term structures")
  scenarios <- static$scenario_severity[, c("scenario", "severity_z")]
  internal_ts <- build_pd_term_structure(static$ttc_pd_table, scenarios,
                                          model_cfg, model_inputs,
                                          rating_type = "Internal")
  external_ts <- build_pd_term_structure(static$ttc_pd_table_external, scenarios,
                                          model_cfg, model_inputs,
                                          gcc_history = gcc_history,
                                          rating_type = "External")

  log_msg("[run_etl] derived: StPD")
  ratings_combined <- dplyr::bind_rows(
    static$master_rating_scale[static$master_rating_scale$rating_type=="Internal",
                                c("rating","hierarchy")],
    static$master_rating_scale[static$master_rating_scale$rating_type=="External",
                                c("rating","hierarchy")]
  )
  stpd <- build_stpd(
    internal_term_structure   = internal_ts,
    external_term_structure   = external_ts,
    internal_scenario_weights = model_inputs$internal_scenario_weights,
    external_scenario_weights = model_inputs$external_scenario_weights,
    portfolios                = .default_stpd_portfolios(),
    ratings                   = ratings_combined,
    run_cfg                   = run_cfg,
    max_month                 = 600
  )

  # ---- VALIDATION GATE 3: derived outputs -----------------------------
  v_derived <- NULL
  if (isTRUE(run_validation)) {
    log_msg("[run_etl] validation: derived outputs")
    v_derived <- run_validation_suite(
      "DERIVED",
      build_derived_validators(),
      args = list(ltpo = ltpo, stpd = stpd, model_inputs = model_inputs,
                  trans_l = trans_l, static = static, model_cfg = model_cfg),
      verbose = verbose,
      suppressions = suppression_ids
    )
    .gate_on_validation(v_derived, "DERIVED", run_cfg, log_msg, run_id = run_id)
  }

  # ---- Aggregate validation report ------------------------------------
  validation_results <- NULL
  if (isTRUE(run_validation)) {
    parts <- list()
    if (!is.null(v_inputs))    parts[[length(parts) + 1]] <- dplyr::mutate(v_inputs,    stage = "INPUT")
    if (!is.null(v_transform)) parts[[length(parts) + 1]] <- dplyr::mutate(v_transform, stage = "TRANSFORM")
    if (!is.null(v_derived))   parts[[length(parts) + 1]] <- dplyr::mutate(v_derived,   stage = "DERIVED")
    if (length(parts) > 0) {
      validation_results <- dplyr::bind_rows(parts)
      write_validation_report(validation_results,
                              file.path(reports_dir, "validation.md"))
      write_validation_csv(validation_results,
                           file.path(reports_dir, "validation.csv"))
      log_msg("[run_etl] validation report -> %s",
              file.path(reports_dir, "validation.md"))
    }
  }

  # ---- Intermediates ----------------------------------------------------
  log_msg("[run_etl] building intermediates")
  customer_flags                  <- build_customer_flags(cm_view, inputs$CustomerStagingFlag)
  collateral_tbl                  <- build_collateral_tbl(inputs$Collateral)
  account_collateral_allocation_tbl <- build_account_collateral_allocation_tbl(
                                          inputs$AccountCollateralAllocation)
  investment_customers            <- build_investment_customers(trans_i)

  # ---- Write outputs ----------------------------------------------------
  log_msg("[run_etl] writing 18 outputs to %s", output_dir)
  output_paths <- write_all_outputs(
    static                        = static,
    run_cfg                       = run_cfg,
    lending_accounts              = trans_l,
    investment_accounts           = trans_i,
    customers                     = cm_view,
    investment_customers          = investment_customers,
    customer_flags                = customer_flags,
    collateral                    = collateral_tbl,
    account_collateral_allocation = account_collateral_allocation_tbl,
    raw_account_master            = inputs$AccountMaster,
    raw_investment_master         = inputs$AccountMasterInvestments,
    lifetime_parameter_other      = ltpo,
    stpd                          = stpd,
    output_dir                    = output_dir
  )

  # ---- Manifest ---------------------------------------------------------
  log_msg("[run_etl] writing manifest")
  manifest_path <- write_manifest(
    run_dir       = output_root,
    input_dir     = run_cfg$paths$input_dir,
    run_cfg       = run_cfg,
    model_cfg     = model_cfg,
    run_started   = run_started,
    run_finished  = Sys.time(),
    messages      = msg_log,
    static_dir    = run_cfg$paths$static_dir,
    snapshot_meta = snapshot_meta,
    code_sha      = current_code_sha,
    run_id        = run_id
  )

  # ---- Optional reconciliation ------------------------------------------
  reconciliation <- NULL
  ref_dir <- run_cfg$paths$reference_outputs
  if (isTRUE(reconcile) && !is.null(ref_dir) && dir.exists(ref_dir)) {
    log_msg("[run_etl] reconciling against %s", ref_dir)
    reconciliation <- tryCatch(
      reconcile_output_dirs(actual_dir = output_dir,
                            reference_dir = ref_dir,
                            key_spec = default_key_spec()),
      error = function(e) {
        log_msg("[run_etl] reconciliation failed: %s", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(reconciliation)) {
      tryCatch({
        write_reconciliation_markdown(
          reconciliation,
          file.path(reports_dir, "reconciliation.md"))
        dump_mismatches(
          reconciliation,
          actual_dir    = output_dir,
          reference_dir = ref_dir,
          dir           = file.path(reports_dir, "mismatches"))
      }, error = function(e) {
        log_msg("[run_etl] writing recon markdown failed: %s", conditionMessage(e))
      })
    }
  }

  run_finished <- Sys.time()
  duration <- as.numeric(difftime(run_finished, run_started, units = "secs"))
  log_msg("[run_etl] done in %.1fs", duration)

  audit_event(list(
    event            = "run_finish",
    run_id           = run_id,
    snapshot         = snapshot_meta$label %||% NA_character_,
    code_sha         = current_code_sha,
    duration_seconds = duration,
    output_dir       = output_dir,
    manifest_path    = manifest_path,
    n_outputs        = length(output_paths),
    finished_at      = format(run_finished, "%Y-%m-%dT%H:%M:%S%z")
  ))

  invisible(list(
    run_dir          = output_root,
    output_paths     = output_paths,
    manifest_path    = manifest_path,
    reconciliation   = reconciliation,
    validation       = validation_results,
    intermediates    = list(
      run_cfg              = run_cfg,
      model_cfg            = model_cfg,
      static               = static,
      inputs               = inputs,
      trans_l              = trans_l,
      trans_i              = trans_i,
      cm_view              = cm_view,
      inv_view             = inv_view,
      ltpo                 = ltpo,
      stpd                 = stpd,
      customer_flags       = customer_flags,
      collateral_tbl       = collateral_tbl,
      account_collateral_allocation_tbl = account_collateral_allocation_tbl,
      investment_customers = investment_customers
    ),
    snapshot         = snapshot_meta,
    code_sha         = current_code_sha,
    run_id           = run_id,
    duration_seconds = duration,
    messages         = msg_log
  ))
}


#' Pre-run quick check — a fast feedback path for Shiny's load page.
#'
#' Loads inputs and runs ONLY the INPUT-stage validators tagged "pre_run"
#' (or all INPUT validators if no validator carries that tag). Skips
#' transforms, derived outputs, and writes. Returns a tibble of results
#' that Shiny can display before the user commits to a full run.
#'
#' @param config_path     path to config.yml (or snapshot config.yml)
#' @param snapshot        optional snapshot label (overrides config_path
#'                        like in run_etl())
#' @param tag             validator tag to filter on. Default "pre_run".
#'                        Pass NULL or "" to run all INPUT validators.
#' @param verbose         print progress to console.
#' @return tibble with the same shape as run_etl()$validation, restricted
#'         to the validators that ran.
pre_run_check <- function(config_path = "config.yml",
                           snapshot = NULL,
                           tag = "pre_run",
                           verbose = TRUE,
                           input_dir_override = NULL) {
  if (!is.null(snapshot)) {
    sp <- snapshot_paths(snapshot)
    config_path <- sp$config_yml
  }
  run_cfg <- load_run_config(config_path)
  if (!is.null(snapshot)) {
    sp <- snapshot_paths(snapshot)
    run_cfg$paths$static_dir <- sp$static_dir
  }
  static <- load_static_reference(run_cfg$paths$static_dir)
  # Honor an upload / drop-folder override the same way phase1 does.
  effective_input_dir <- if (!is.null(input_dir_override) &&
                              nzchar(input_dir_override)) {
    if (verbose) message(sprintf("[pre_run_check] input_dir override: %s",
                                  input_dir_override))
    input_dir_override
  } else {
    run_cfg$paths$input_dir
  }
  inputs <- read_all_inputs(effective_input_dir, verbose = FALSE)

  # H18: pre-run check now spans three layers — config, static, inputs.
  # Earlier builds only ran INPUT validators here, which meant a
  # corrupted static reference file or a config with bad paths slipped
  # through and only failed deep in phase 1.
  validators_input  <- build_input_validators()
  validators_static <- if (exists("build_static_validators",
                                    mode = "function")) {
    build_static_validators()
  } else list()
  validators_config <- if (exists("build_config_validators",
                                    mode = "function")) {
    build_config_validators()
  } else list()
  validators <- c(validators_config, validators_static, validators_input)

  if (!is.null(tag) && nzchar(tag)) {
    validators <- Filter(function(v) tag %in% (v$tags %||% character()),
                          validators)
    if (length(validators) == 0 && verbose) {
      message(sprintf(
        "[pre_run_check] no validators carry tag '%s'; running all INPUT validators",
        tag))
      validators <- build_input_validators()
    }
  }

  suppressions_path <- run_cfg$paths$validation_suppressions %||%
                        file.path(dirname(config_path),
                                   "config", "validation_suppressions.yml")
  suppression_ids <- active_suppression_ids(load_suppressions(suppressions_path))

  results <- run_validation_suite(
    "PRE_RUN",
    validators,
    args = list(inputs = inputs, static = static, run_cfg = run_cfg),
    verbose = verbose,
    suppressions = suppression_ids
  )

  if (exists("audit_event", mode = "function")) {
    audit_event(list(
      event   = "pre_run_check",
      n_total = nrow(results),
      n_pass  = sum(results$passed),
      n_fail  = sum(!results$passed)
    ))
  }
  invisible(results)
}


#' Source all R/ files. Convenience for scripts/Shiny that need every
#' module loaded before calling run_etl().
#'
#' @param r_dir path to R/ directory (default = "R")
source_pipeline <- function(r_dir = "R") {
  # Order matters: schemas before read_inputs, helpers before users.
  files <- c(
    "io_helpers.R",
    "audit_log.R",
    "code_version.R",
    "snapshots.R",
    "load_config.R",
    "load_variables.R",
    "load_static.R",
    "input_schemas.R",
    "read_inputs.R",
    "input_acquisition.R",
    "run_export.R",
    "transform_lending.R",
    "lending_portfolio_view.R",
    "transform_investments.R",
    "investment_portfolio_view.R",
    "macro_model.R",
    "pd_term_structure.R",
    "lifetime_parameter_other.R",
    "build_stpd.R",
    "build_intermediates.R",
    "output_writers.R",
    "reconciliation.R",
    "validation.R",
    "validation_suppressions.R",
    "validators_input.R",
    "validators_static.R",
    "validators_transform.R",
    "validators_derived.R",
    "run_discovery.R",
    "run_approval.R",
    "manifest.R",
    "run_etl.R",
    "run_etl_phased.R"
  )
  for (f in files) {
    p <- file.path(r_dir, f)
    if (file.exists(p)) source(p)
  }
  invisible(files)
}
