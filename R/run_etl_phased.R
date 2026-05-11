# =============================================================================
# run_etl_phased.R
#
# Two-phase orchestrator for the H12 mid-run override workflow.
#
#   run_etl_phase1(...)  -> state object; pipeline paused after lending
#                            portfolio view (cm_view ready, overrideable)
#   run_etl_phase2(state, overrides_applied = ...)  -> writes outputs,
#                            manifest, validation csv; run lands as
#                            "pending_approval"
#
# The original run_etl() in run_etl.R remains as a one-shot helper for
# scripts and tests that don't need the pause. It internally calls
# phase1 + phase2 with no overrides.
#
# Why two phases instead of inlining the pause into run_etl():
#   - The Shiny app must hand control back to the user mid-run, then
#     resume. R doesn't have first-class coroutines, so the cleanest
#     option is to break the function into two and have the app hold
#     the intermediate state in a reactiveVal between calls.
#   - Phase 1 is "compute what would be"; phase 2 is "apply overrides
#     and finalize". The boundary is principled — anything before is
#     deterministic given inputs+config; anything after depends on
#     user decisions.
# =============================================================================


#' Phase 1: load + validate + transform. Pauses after lending portfolio view.
#'
#' Returns a state list with everything phase 2 will need. The Shiny app
#' holds this in a reactiveVal between phase 1 and phase 2.
#'
#' @inheritParams run_etl
#' @return list with:
#'   - run_id, run_started, run_dir, output_dir, reports_dir
#'   - run_cfg, model_cfg, model_inputs, static, inputs
#'   - cm_view, trans_l, inv_view, trans_i        (portfolio views; pause here)
#'   - gcc_history
#'   - validation        (input + transform results combined)
#'   - suppression_ids   (re-used in phase 2)
#'   - msg_log
#'   - snapshot_meta, current_code_sha
run_etl_phase1 <- function(config_path = "config.yml",
                            output_root = NULL,
                            verbose = TRUE,
                            snapshot = NULL,
                            run_validation = TRUE,
                            keep_history = TRUE,
                            runs_dir = NULL,
                            input_dir_override = NULL,
                            input_source_meta = NULL,
                            run_type = "unofficial") {
  if (!run_type %in% c("official", "unofficial")) {
    stop("run_type must be 'official' or 'unofficial'")
  }

  run_started <- Sys.time()
  msg_log <- character()
  log_msg <- function(...) {
    s <- sprintf(...)
    msg_log <<- c(msg_log, s)
    if (verbose) message(s)
  }

  # ---- Snapshot resolution ---------------------------------------------
  snapshot_meta <- NULL
  if (!is.null(snapshot)) {
    snapshot_meta <- read_snapshot_metadata(snapshot)
    sp <- snapshot_paths(snapshot)
    log_msg("[phase1] using snapshot %s (status=%s)",
             snapshot_meta$label %||% snapshot, snapshot_meta$status %||% "?")

    # Snapshot completeness check — refuse to run against a broken
    # snapshot rather than silently produce wrong results.
    # First: the four required files must exist on disk.
    required_files <- list(
      "config/config.yml"              = sp$config_yml,
      "config/variable_dictionary.yml" = sp$variable_dictionary,
      "config/models.yml"              = sp$models,
      "config/model_inputs.yml"        = sp$model_inputs
    )
    missing_files <- names(required_files)[!file.exists(unlist(required_files))]
    static_ok <- dir.exists(sp$static_dir) &&
                  length(list.files(sp$static_dir)) > 0
    if (!static_ok) missing_files <- c(missing_files, "static/ (empty or missing)")

    # Second: the paths inside config.yml must resolve to real files
    # when read from the snapshot's location. Snapshots created in
    # earlier builds embedded the live `config.yml` verbatim, but its
    # `variable_dictionary: config/variable_dictionary.yml` style
    # paths only work when config.yml is at the project root. Inside
    # the snapshot, config.yml lives in config/, so those paths
    # double up to `config/config/...` and don't resolve. The H18d+
    # create_snapshot rewrites the paths during copy; older snapshots
    # need to be re-created.
    bad_paths <- character()
    if (length(missing_files) == 0) {
      cfg_yaml <- tryCatch(yaml::read_yaml(sp$config_yml),
                            error = function(e) NULL)
      if (!is.null(cfg_yaml) && !is.null(cfg_yaml$paths)) {
        cfg_dir_in_snap <- dirname(sp$config_yml)
        for (k in c("variable_dictionary", "models", "model_inputs")) {
          v <- cfg_yaml$paths[[k]]
          if (!is.null(v) && is.character(v) && nzchar(v)) {
            resolved <- if (.is_absolute_path(v)) {
              v
            } else {
              file.path(cfg_dir_in_snap, v)
            }
            if (!file.exists(resolved)) {
              bad_paths <- c(bad_paths,
                              sprintf("paths$%s = '%s' resolves to %s (not found)",
                                      k, v, resolved))
            }
          }
        }
      }
    }

    if (length(missing_files) > 0 || length(bad_paths) > 0) {
      detail <- character()
      if (length(missing_files) > 0) {
        detail <- c(detail,
                     "Missing files:",
                     paste("  -", missing_files))
      }
      if (length(bad_paths) > 0) {
        detail <- c(detail,
                     "Broken internal path references in config.yml:",
                     paste("  -", bad_paths))
      }
      stop(paste0(
        "Snapshot '", snapshot_meta$label %||% snapshot,
        "' is incomplete or has bad path references. ",
        "This snapshot was created in an earlier build before the ",
        "path-rewrite fix. Recovery: go to Manage Snapshots, create ",
        "a new draft from the live config (which now correctly ",
        "rewrites internal paths), promote it through tested -> ",
        "pending_final -> approved, then re-run.\n",
        paste(detail, collapse = "\n")))
    }

    config_path <- sp$config_yml
    cv <- compare_code_to_snapshot(snapshot_meta, verbose = TRUE)
    if (!is.null(cv$current) && !is.na(cv$current)) {
      log_msg("[phase1] code sha=%s; snapshot recorded sha=%s",
               substr(cv$current, 1, 12), substr(cv$snapshot %||% "", 1, 12))
    }
  }

  # ---- Load config / static / inputs -----------------------------------
  log_msg("[phase1] loading config from %s", config_path)
  run_cfg <- load_run_config(config_path)
  if (!is.null(snapshot_meta)) {
    sp <- snapshot_paths(snapshot)
    # Snapshot-internal paths: snapshot OWNS this content. Override
    # to point inside the snapshot.
    run_cfg$paths$static_dir          <- sp$static_dir
    run_cfg$paths$variable_dictionary <- sp$variable_dictionary
    run_cfg$paths$models              <- sp$models
    run_cfg$paths$model_inputs        <- sp$model_inputs
    # Project-environment paths: describe WHERE the run executes on
    # this machine, not WHAT it computes. They must come from the
    # LIVE environment, not the snapshot — otherwise runs made with
    # a snapshot resolve `runs_dir: "runs"` as `<snapshot>/config/runs`
    # and end up writing their outputs INSIDE the snapshot.
    proj_root <- getOption("ifrs9.project_root", getwd())
    live_cfg_path <- file.path(proj_root, "config.yml")
    live_cfg <- if (file.exists(live_cfg_path)) {
      tryCatch(load_run_config(live_cfg_path),
                error = function(e) NULL)
    } else NULL
    if (!is.null(live_cfg)) {
      for (k in c("input_dir", "output_dir", "runs_dir", "data_drop_root")) {
        v <- live_cfg$paths[[k]]
        if (!is.null(v)) run_cfg$paths[[k]] <- v
      }
    }
  }
  model_cfg <- load_model_config(config_path)
  static    <- load_static_reference(run_cfg$paths$static_dir)

  current_code_sha <- tryCatch(get_current_code_sha(),
                                error = function(e) NA_character_)

  run_id <- format(run_started, "%Y-%m-%d_%H-%M-%S")

  audit_event(list(
    event       = "run_start",
    run_id      = run_id,
    snapshot    = snapshot_meta$label %||% NA_character_,
    code_sha    = current_code_sha,
    config_path = config_path,
    started_at  = format(run_started, "%Y-%m-%dT%H:%M:%S%z")
  ))

  # ---- Output location resolution -------------------------------------
  if (isTRUE(keep_history)) {
    base <- runs_dir %||%
             run_cfg$paths$runs_dir %||%
             getOption("ifrs9.runs_dir",
                        default = Sys.getenv("IFRS9_RUNS_DIR", "runs"))
    output_root <- file.path(base, run_id)
    log_msg("[phase1] keep_history=TRUE -> %s", output_root)
  } else {
    output_root <- output_root %||%
                    run_cfg$paths$output_root %||%
                    run_cfg$paths$output_dir  %||%
                    "test_output"
  }
  output_dir  <- file.path(output_root, "Output")
  reports_dir <- file.path(output_root, "reports")
  dir.create(output_dir,  showWarnings = FALSE, recursive = TRUE)
  dir.create(reports_dir, showWarnings = FALSE, recursive = TRUE)

  # Persist input-source provenance into the run BEFORE we attempt to
  # read inputs — that way even if input read fails, the audit trail
  # records what was attempted. If no source meta was passed (legacy
  # callers like the single-shot tests), default to "configured".
  if (is.null(input_source_meta)) {
    input_source_meta <- list(kind = "configured",
                                details = list(path = run_cfg$paths$input_dir))
  }
  record_input_source(run_dir = output_root,
                       kind    = input_source_meta$kind,
                       details = input_source_meta$details %||% list())

  # Freeze the config and static-reference files used by this run into
  # runs/<id>/config_used/. This makes the run self-describing — even
  # if the live config or static files change tomorrow, the run's
  # canonical record stays intact. The export-package builder copies
  # this folder verbatim, so a reviewer six months later sees exactly
  # what was active at run time, not what's currently on disk.
  config_used_dir <- file.path(output_root, "config_used")
  dir.create(config_used_dir, showWarnings = FALSE, recursive = TRUE)
  .freeze_config_used <- function() {
    cu_cfg <- file.path(config_used_dir, "config")
    cu_st  <- file.path(config_used_dir, "static")
    dir.create(cu_cfg, showWarnings = FALSE, recursive = TRUE)
    dir.create(cu_st,  showWarnings = FALSE, recursive = TRUE)
    # Source: when a snapshot is used, point at its frozen tree (the
    # snapshot is the config that ran). Otherwise we copy from the
    # LIVE project: the project's `config/` folder (model + dictionary
    # YAMLs) plus the top-level `config.yml` (run config).
    #
    # WARNING: do NOT use `dirname(config_path)` here. For live runs,
    # config_path is "config.yml" so dirname() is "." — copying that
    # would dump the ENTIRE project tree (app/, R/, runs/, etc.) into
    # the run's config_used folder.
    proj_root <- getOption("ifrs9.project_root", getwd())
    if (!is.null(snapshot_meta)) {
      src_cfg_dir   <- file.path(snapshot_meta$path, "config")
      src_static    <- file.path(snapshot_meta$path, "static")
      src_run_cfg   <- NULL  # already inside src_cfg_dir for snapshots
    } else {
      src_cfg_dir   <- file.path(proj_root, "config")
      src_static    <- run_cfg$paths$static_dir
      src_run_cfg   <- file.path(proj_root, "config.yml")
    }
    if (dir.exists(src_cfg_dir)) {
      files <- list.files(src_cfg_dir, recursive = TRUE, full.names = TRUE)
      rels  <- list.files(src_cfg_dir, recursive = TRUE, full.names = FALSE)
      for (i in seq_along(files)) {
        target <- file.path(cu_cfg, rels[i])
        dir.create(dirname(target), showWarnings = FALSE, recursive = TRUE)
        file.copy(files[i], target, overwrite = TRUE)
      }
    }
    # Top-level config.yml lives at the project root for live runs;
    # copy it next to the model/dictionary YAMLs (matches the snapshot
    # layout).
    if (!is.null(src_run_cfg) && file.exists(src_run_cfg)) {
      file.copy(src_run_cfg, file.path(cu_cfg, "config.yml"),
                 overwrite = TRUE)
    }
    if (dir.exists(src_static)) {
      files <- list.files(src_static, recursive = TRUE, full.names = TRUE)
      rels  <- list.files(src_static, recursive = TRUE, full.names = FALSE)
      for (i in seq_along(files)) {
        target <- file.path(cu_st, rels[i])
        dir.create(dirname(target), showWarnings = FALSE, recursive = TRUE)
        file.copy(files[i], target, overwrite = TRUE)
      }
    }
    # Drop a small marker so we know what was copied and from where.
    marker <- list(
      schema_version  = "1.0",
      kind            = if (!is.null(snapshot_meta)) "snapshot" else "live",
      snapshot_label  = snapshot_meta$label %||% NA_character_,
      source_config   = src_cfg_dir,
      source_run_cfg  = src_run_cfg %||% "(in source_config)",
      source_static   = src_static,
      frozen_at       = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
    )
    writeLines(yaml::as.yaml(marker),
                file.path(config_used_dir, "config_used.yml"))
  }
  freeze_err <- tryCatch({ .freeze_config_used(); NULL },
                          error = function(e) conditionMessage(e))
  if (!is.null(freeze_err)) {
    log_msg("[phase1] WARNING: could not freeze config_used: %s",
             freeze_err)
  } else {
    log_msg("[phase1] froze config + static into runs/<id>/config_used/")
  }

  log_msg("[phase1] building macro inputs")
  gcc_history <- .build_gcc_history(static)
  model_inputs <- load_model_inputs(
    path                = run_cfg$paths$model_inputs,
    model_cfg           = model_cfg,
    scenarios           = static$scenario_severity,
    gcc_history         = gcc_history,
    non_oil_gdp_history = static$non_oil_gdp_history$value
  )

  # Resolve the actual input directory: either the override (from a
  # zip upload or a chosen data drop folder) or the configured default.
  effective_input_dir <- if (!is.null(input_dir_override) &&
                              nzchar(input_dir_override)) {
    log_msg("[phase1] input_dir override: %s", input_dir_override)
    input_dir_override
  } else {
    run_cfg$paths$input_dir
  }
  log_msg("[phase1] reading inputs from %s", effective_input_dir)
  inputs <- read_all_inputs(effective_input_dir, verbose = FALSE)

  # ---- Suppressions ---------------------------------------------------
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
    log_msg("[phase1] %d active validation suppression(s)",
             length(suppression_ids))
  }

  # ---- VALIDATION GATE 1: inputs --------------------------------------
  v_inputs <- NULL
  if (isTRUE(run_validation)) {
    log_msg("[phase1] validation: inputs")
    v_inputs <- run_validation_suite(
      "INPUT", build_input_validators(),
      args = list(inputs = inputs, static = static, run_cfg = run_cfg),
      verbose = verbose, suppressions = suppression_ids
    )
    .gate_on_validation(v_inputs, "INPUT", run_cfg, log_msg, run_id = run_id)
  }

  # ---- Transformation + portfolio views -------------------------------
  log_msg("[phase1] transformation: lending")
  trans_l <- build_transformation_lending(inputs, static, model_cfg, run_cfg)
  out_l   <- build_lending_portfolio_view(trans_l, inputs, static, model_cfg)
  cm_view <- out_l$portfolio
  trans_l <- out_l$transformation

  log_msg("[phase1] transformation: investments")
  trans_i  <- build_transformation_investments(inputs, static, model_cfg, run_cfg)
  out_i    <- build_investment_portfolio_view(trans_i, inputs, static)
  inv_view <- out_i$portfolio
  trans_i  <- out_i$transformation

  # ---- VALIDATION GATE 2: transformation ------------------------------
  v_transform <- NULL
  if (isTRUE(run_validation)) {
    log_msg("[phase1] validation: transformation")
    v_transform <- run_validation_suite(
      "TRANSFORM", build_transform_validators(),
      args = list(trans_l = trans_l, cm_view = cm_view,
                  trans_i = trans_i, inv_view = inv_view,
                  static = static, model_cfg = model_cfg),
      verbose = verbose, suppressions = suppression_ids
    )
    .gate_on_validation(v_transform, "TRANSFORM", run_cfg, log_msg,
                         run_id = run_id)
  }

  # ---- Aggregate validation so far (just stages 1 + 2) ----------------
  validation_so_far <- NULL
  if (isTRUE(run_validation)) {
    parts <- list()
    if (!is.null(v_inputs))    parts[[length(parts) + 1]] <- dplyr::mutate(v_inputs,    stage = "INPUT")
    if (!is.null(v_transform)) parts[[length(parts) + 1]] <- dplyr::mutate(v_transform, stage = "TRANSFORM")
    if (length(parts) > 0) validation_so_far <- dplyr::bind_rows(parts)
  }

  audit_event(list(
    event   = "run_phase1_complete",
    run_id  = run_id,
    n_customers_lending     = nrow(cm_view %||% data.frame()),
    n_accounts_investments  = nrow(inv_view %||% data.frame()),
    paused_for_overrides    = TRUE
  ))

  list(
    run_id            = run_id,
    run_started       = run_started,
    run_dir           = output_root,
    output_dir        = output_dir,
    reports_dir       = reports_dir,
    config_path       = config_path,
    run_cfg           = run_cfg,
    model_cfg         = model_cfg,
    model_inputs      = model_inputs,
    static            = static,
    inputs            = inputs,
    cm_view           = cm_view,
    trans_l           = trans_l,
    inv_view          = inv_view,
    trans_i           = trans_i,
    gcc_history       = gcc_history,
    validation        = validation_so_far,
    suppression_ids   = suppression_ids,
    msg_log           = msg_log,
    snapshot_meta     = snapshot_meta,
    current_code_sha  = current_code_sha,
    run_validation    = run_validation,
    verbose           = verbose,
    run_type          = run_type
  )
}


#' Phase 2: apply user-supplied overrides, run derived outputs, write
#' everything to disk, mark run as "pending_approval".
#'
#' Override application strategy: each override CSV is written to
#' `<run_dir>/overrides/{rating,stage,restructuring}_overrides.csv`. The
#' override values are then merged into the existing `cm_view` / `trans_l`
#' tables produced by phase 1 — we DO NOT re-run the transformation, we
#' just rewrite the affected columns. This is correct because the three
#' overrides only change downstream outputs (StPD bucket assignment via
#' rating, ECL stage classification via stage), not the upstream calculation.
#'
#' @param state         output of run_etl_phase1()
#' @param overrides     list with optional fields:
#'                        rating        — data.frame(customer_id, override_rating, reason, ...)
#'                        stage         — data.frame(customer_id, override_stage, reason, ...)
#'                        restructuring — data.frame(customer_id, override_restructuring, reason, ...)
#'                      Each frame uses the same audit columns convention as
#'                      data-raw/static/customer_*_overrides.csv.
#' @param reconcile     if TRUE and a reference dir is configured, run
#'                      reconciliation against it.
#' @return same shape as the legacy run_etl() result list, plus a
#'         `pending_approval` status indicator.
run_etl_phase2 <- function(state, overrides = list(), reconcile = TRUE) {
  msg_log <- state$msg_log
  log_msg <- function(...) {
    s <- sprintf(...)
    msg_log <<- c(msg_log, s)
    if (isTRUE(state$verbose)) message(s)
  }

  run_id     <- state$run_id
  run_cfg    <- state$run_cfg
  model_cfg  <- state$model_cfg
  static     <- state$static
  inputs     <- state$inputs
  trans_l    <- state$trans_l
  cm_view    <- state$cm_view
  inv_view   <- state$inv_view
  trans_i    <- state$trans_i
  output_root <- state$run_dir
  output_dir  <- state$output_dir
  reports_dir <- state$reports_dir

  # ---- Apply overrides ------------------------------------------------
  overrides <- overrides %||% list()
  applied <- list(rating = 0L, stage = 0L, restructuring = 0L)

  # Persist override CSVs to the run directory regardless of whether they
  # have rows — so the run is always self-describing.
  ov_dir <- file.path(output_root, "overrides")
  dir.create(ov_dir, showWarnings = FALSE, recursive = TRUE)

  user_id <- Sys.info()[["user"]] %||% "unknown"
  ts_now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")

  .normalise_override_frame <- function(df, value_col, prior_lookup,
                                         keep_only = FALSE) {
    if (is.null(df) || nrow(df) == 0) {
      return(data.frame(
        customer_id    = character(),
        value          = character(),
        reason         = character(),
        created_by     = character(),
        created_at     = character(),
        prior_value    = character(),
        source_run_id  = character(),
        stringsAsFactors = FALSE
      ))
    }
    cid <- as.character(df$customer_id)
    val <- as.character(df[[value_col]])
    reason <- as.character(df$reason %||% rep("", length(cid)))
    if (any(!nzchar(reason))) {
      stop("Every override row must have a non-empty reason")
    }
    data.frame(
      customer_id   = cid,
      value         = val,
      reason        = reason,
      created_by    = ifelse(nzchar(df$created_by %||% ""),
                              df$created_by, user_id),
      created_at    = ifelse(nzchar(df$created_at %||% ""),
                              df$created_at, ts_now),
      prior_value   = prior_lookup(cid),
      source_run_id = run_id,
      stringsAsFactors = FALSE
    )
  }

  # Rating override: prior_value = currently calculated rating in trans_l
  rating_override_df <- .normalise_override_frame(
    overrides$rating, "override_rating",
    prior_lookup = function(cid) {
      m <- match(cid, as.character(trans_l$customer_id))
      ifelse(is.na(m), "", as.character(trans_l$rating_after_override[m]))
    }
  )
  utils::write.csv(rating_override_df,
                    file.path(ov_dir, "rating_overrides.csv"),
                    row.names = FALSE, na = "")
  applied$rating <- nrow(rating_override_df)

  # Stage override: prior = cm_view$stage_final
  stage_override_df <- .normalise_override_frame(
    overrides$stage, "override_stage",
    prior_lookup = function(cid) {
      m <- match(cid, as.character(cm_view$customer_id))
      ifelse(is.na(m), "", as.character(cm_view$stage_final[m]))
    }
  )
  utils::write.csv(stage_override_df,
                    file.path(ov_dir, "stage_overrides.csv"),
                    row.names = FALSE, na = "")
  applied$stage <- nrow(stage_override_df)

  # Restructuring override: prior = cm_view$restructuring_final
  restruct_override_df <- .normalise_override_frame(
    overrides$restructuring, "override_restructuring",
    prior_lookup = function(cid) {
      m <- match(cid, as.character(cm_view$customer_id))
      ifelse(is.na(m), "", as.character(cm_view$restructuring_final[m]))
    }
  )
  utils::write.csv(restruct_override_df,
                    file.path(ov_dir, "restructuring_overrides.csv"),
                    row.names = FALSE, na = "")
  applied$restructuring <- nrow(restruct_override_df)

  log_msg("[phase2] overrides: rating=%d, stage=%d, restructuring=%d",
           applied$rating, applied$stage, applied$restructuring)

  audit_event(list(
    event                   = "run_overrides_applied",
    run_id                  = run_id,
    n_rating_overrides       = applied$rating,
    n_stage_overrides        = applied$stage,
    n_restructuring_overrides = applied$restructuring
  ))

  # ---- Apply overrides to in-memory tables -----------------------------
  # Apply override values directly to the master columns. The downstream
  # outputs read:
  #   - trans_l$rating_worst       (drives AccountMaster_1.Rating per account)
  #   - cm_view$rating_final       (drives the customer-level views)
  #   - cm_view$stage_final        (drives customer_flags.is_default/is_local3,
  #                                 which drive CustomerStagingFlag_1)
  #   - cm_view$restructuring_final(drives customer_flags.is_local1)
  # We rewrite the master columns; build_customer_flags (called below)
  # reads cm_view directly so it picks up the overrides automatically.
  if (applied$rating > 0) {
    # Update trans_l: every contract belonging to the overridden customer
    # gets the new rating. trans_l has one row per contract; the override
    # is at customer level so we match-and-broadcast.
    cust_to_rating <- setNames(rating_override_df$value,
                                rating_override_df$customer_id)
    cust_id_chr <- as.character(trans_l$customer_id)
    hit <- cust_id_chr %in% names(cust_to_rating)
    if (any(hit)) {
      trans_l$rating_worst[hit] <- cust_to_rating[cust_id_chr[hit]]
      # rating_after_override mirrors rating_worst for the overridden ones,
      # since the user-specified value is now the final per-customer rating.
      trans_l$rating_after_override[hit] <- cust_to_rating[cust_id_chr[hit]]
    }
    # cm_view: one row per customer; just set rating_final directly.
    m <- match(rating_override_df$customer_id,
                as.character(cm_view$customer_id))
    keep <- !is.na(m)
    if (any(keep)) {
      cm_view$rating_final[m[keep]] <- rating_override_df$value[keep]
    }
  }
  if (applied$stage > 0) {
    m <- match(stage_override_df$customer_id,
                as.character(cm_view$customer_id))
    keep <- !is.na(m)
    if (any(keep)) {
      cm_view$stage_final[m[keep]] <- stage_override_df$value[keep]
    }
  }
  if (applied$restructuring > 0) {
    m <- match(restruct_override_df$customer_id,
                as.character(cm_view$customer_id))
    keep <- !is.na(m)
    if (any(keep)) {
      cm_view$restructuring_final[m[keep]] <- restruct_override_df$value[keep]
    }
  }

  # ---- Derived outputs -------------------------------------------------
  log_msg("[phase2] derived: lifetime parameter other")
  ltpo <- build_lifetime_parameter_other(inputs$RepaymentSchedule,
                                           trans_l, run_cfg)

  log_msg("[phase2] derived: PD term structures")
  scenarios <- static$scenario_severity[, c("scenario", "severity_z")]
  internal_ts <- build_pd_term_structure(static$ttc_pd_table, scenarios,
                                          model_cfg, state$model_inputs,
                                          rating_type = "Internal")
  external_ts <- build_pd_term_structure(static$ttc_pd_table_external, scenarios,
                                          model_cfg, state$model_inputs,
                                          gcc_history = state$gcc_history,
                                          rating_type = "External")

  log_msg("[phase2] derived: StPD")
  ratings_combined <- dplyr::bind_rows(
    static$master_rating_scale[static$master_rating_scale$rating_type=="Internal",
                                c("rating","hierarchy")],
    static$master_rating_scale[static$master_rating_scale$rating_type=="External",
                                c("rating","hierarchy")]
  )
  stpd <- build_stpd(
    internal_term_structure   = internal_ts,
    external_term_structure   = external_ts,
    internal_scenario_weights = state$model_inputs$internal_scenario_weights,
    external_scenario_weights = state$model_inputs$external_scenario_weights,
    portfolios                = .default_stpd_portfolios(),
    ratings                   = ratings_combined,
    run_cfg                   = run_cfg,
    max_month                 = 600
  )

  # ---- VALIDATION GATE 3: derived ------------------------------------
  v_derived <- NULL
  if (isTRUE(state$run_validation)) {
    log_msg("[phase2] validation: derived outputs")
    v_derived <- run_validation_suite(
      "DERIVED", build_derived_validators(),
      args = list(ltpo = ltpo, stpd = stpd,
                  model_inputs = state$model_inputs,
                  trans_l = trans_l, static = static, model_cfg = model_cfg),
      verbose = state$verbose, suppressions = state$suppression_ids
    )
    .gate_on_validation(v_derived, "DERIVED", run_cfg, log_msg, run_id = run_id)
  }

  validation_results <- state$validation
  if (!is.null(v_derived)) {
    validation_results <- dplyr::bind_rows(
      validation_results,
      dplyr::mutate(v_derived, stage = "DERIVED")
    )
  }
  if (!is.null(validation_results)) {
    write_validation_report(validation_results,
                             file.path(reports_dir, "validation.md"))
    write_validation_csv(validation_results,
                          file.path(reports_dir, "validation.csv"))
  }

  # ---- Build intermediates and write outputs -------------------------
  log_msg("[phase2] building intermediates")
  customer_flags                    <- build_customer_flags(cm_view, inputs$CustomerStagingFlag)
  collateral_tbl                    <- build_collateral_tbl(inputs$Collateral)
  account_collateral_allocation_tbl <- build_account_collateral_allocation_tbl(
                                          inputs$AccountCollateralAllocation)
  investment_customers              <- build_investment_customers(trans_i)

  log_msg("[phase2] writing 18 outputs to %s", output_dir)
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

  # ---- Manifest -------------------------------------------------------
  log_msg("[phase2] writing manifest")
  manifest_path <- write_manifest(
    run_dir = output_root, input_dir = run_cfg$paths$input_dir,
    run_cfg = run_cfg, model_cfg = model_cfg,
    run_started = state$run_started, run_finished = Sys.time(),
    messages = msg_log,
    static_dir = run_cfg$paths$static_dir,
    snapshot_meta = state$snapshot_meta,
    code_sha = state$current_code_sha,
    run_id = run_id
  )

  # ---- Write run-status file (so Approval queue can list it) ----------
  # Status depends on run type:
  #   - official runs land as `pending_checker` (need approval workflow)
  #   - unofficial runs land as `unofficial` (terminal — no approval)
  # The maker's identity (user who ran the pipeline) is in manifest.json's
  # `user` field; the approval helpers read it for separation-of-duties
  # enforcement on official runs.
  run_type_val <- state$run_type %||% "official"  # legacy compat: pre-H18 runs are official
  initial_status <- if (run_type_val == "official") {
    "pending_checker"
  } else {
    "unofficial"
  }
  initial_reason <- if (run_type_val == "official") {
    "Run completed; awaiting checker approval."
  } else {
    "Unofficial run \u2014 no approval required (terminal)."
  }
  status_path <- file.path(reports_dir, "run_status.yml")
  status_meta <- list(
    schema_version = "1.0",
    run_id         = run_id,
    status         = initial_status,
    run_type       = run_type_val,
    transitions    = list(
      list(at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
           by = user_id, to = initial_status,
           reason = initial_reason)
    ),
    overrides_applied = applied,
    snapshot_label    = state$snapshot_meta$label %||% NA_character_
  )
  status_write_ok <- tryCatch({
    writeLines(yaml::as.yaml(status_meta), status_path)
    TRUE
  }, error = function(e) {
    log_msg("[phase2] CRITICAL: failed to write run_status.yml: %s",
             conditionMessage(e))
    log_msg("[phase2] -> run will not appear in Approval queue. ",
             "Investigate runs/<run_id>/reports/.")
    FALSE
  })

  audit_event(list(
    event             = if (run_type_val == "official")
                          "run_pending_checker" else "run_unofficial",
    run_id            = run_id,
    run_type          = run_type_val,
    n_outputs         = length(output_paths),
    overrides_applied = applied
  ))

  # ---- Reconciliation (optional) -------------------------------------
  reconciliation <- NULL
  ref_dir <- run_cfg$paths$reference_outputs
  if (isTRUE(reconcile) && !is.null(ref_dir) && dir.exists(ref_dir)) {
    log_msg("[phase2] reconciling against %s", ref_dir)
    reconciliation <- tryCatch(
      reconcile_output_dirs(actual_dir = output_dir,
                            reference_dir = ref_dir,
                            key_spec = default_key_spec()),
      error = function(e) {
        log_msg("[phase2] reconciliation failed: %s", conditionMessage(e))
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
        log_msg("[phase2] writing recon markdown failed: %s", conditionMessage(e))
      })
    }
  }

  run_finished <- Sys.time()
  duration <- as.numeric(difftime(run_finished, state$run_started, units = "secs"))
  log_msg("[phase2] done in %.1fs", duration)

  audit_event(list(
    event            = "run_finish",
    run_id           = run_id,
    snapshot         = state$snapshot_meta$label %||% NA_character_,
    code_sha         = state$current_code_sha,
    duration_seconds = duration,
    output_dir       = output_dir,
    manifest_path    = manifest_path,
    n_outputs        = length(output_paths),
    finished_at      = format(run_finished, "%Y-%m-%dT%H:%M:%S%z")
  ))

  invisible(list(
    run_id           = run_id,
    run_dir          = output_root,
    output_paths     = output_paths,
    manifest_path    = manifest_path,
    reconciliation   = reconciliation,
    validation       = validation_results,
    overrides_applied = applied,
    status           = initial_status,
    run_type         = run_type_val,
    duration_seconds = duration,
    snapshot         = state$snapshot_meta,
    code_sha         = state$current_code_sha,
    messages         = msg_log
  ))
}
