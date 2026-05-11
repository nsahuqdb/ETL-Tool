# ============================================================================
# R/load_static.R
#
# Load the static reference CSVs from data-raw/static/ into a named list of
# tibbles. These are the reference tables extracted once from the Excel tool
# (see data-raw/extract_from_xlsm.py).
#
# Public API:
#   load_static_reference(static_dir)
#       -> named list of 10 elements (9 tibbles + 1 named list for thresholds)
#
# Mapping back to the Excel tool:
#   off_balance_products       <-> Assumptions B3:C9
#   industry_sector_mapping    <-> Assumptions E3:G148
#   collective_assessment_rules<-> Assumptions I:J  (3 sector blocks, long form)
#   product_portfolio_mapping  <-> Assumptions Q3:R13
#   staging_thresholds         <-> Assumptions L3, N3, O3   (returned as named list)
#   master_rating_scale        <-> MasterRatingScale B4:D24 + F25:G45
#   master_rating_downgrade    <-> MasterRatingScale J4:K45
#   scenario_severity          <-> Inputs_Lending Portfolio AA5:AE9
#   gcc_real_gdp_growth        <-> GCC GDP A2:AS8
#   gcc_gdp_current_prices     <-> GCC GDP A11:AS17
#   non_oil_gdp_history        <-> 'IFRS 9 Scenarios Probabilities — 2025 V1'
#                                  Calculations sheet B2:B11 (historical
#                                  Non-Oil GDP 2015-2024 used to derive μ, σ
#                                  for INTERNAL scenario probability weights).
# ============================================================================


# Manifest. Each entry: name -> list(file, columns, key_value).
# - file:      filename inside static_dir
# - columns:   character vector of expected column names
# - key_value: TRUE for the staging_thresholds special case (returned as
#              named list rather than tibble). Default FALSE.
STATIC_FILE_SPECS <- list(
  off_balance_products = list(
    file    = "off_balance_products.csv",
    columns = c("product_code", "lic_input_code")
  ),
  industry_sector_mapping = list(
    file    = "industry_sector_mapping.csv",
    columns = c("industry_code", "industry_description", "sector")
  ),
  collective_assessment_rules = list(
    file    = "collective_assessment_rules.csv",
    columns = c("sector", "dpd_threshold", "rating")
  ),
  product_portfolio_mapping = list(
    file    = "product_portfolio_mapping.csv",
    columns = c("product_type", "portfolio", "source")
  ),
  staging_thresholds = list(
    file      = "staging_thresholds.csv",
    columns   = c("key", "value", "description"),
    key_value = TRUE
  ),
  master_rating_scale = list(
    file    = "master_rating_scale.csv",
    columns = c("rating", "rating_type", "external_equivalent", "hierarchy")
  ),
  master_rating_downgrade = list(
    file    = "master_rating_downgrade.csv",
    columns = c("rating", "rating_after_1_notch_downgrade")
  ),
  scenario_severity = list(
    file    = "scenario_severity.csv",
    columns = c("scenario", "severity_z", "description")
  ),
  gcc_real_gdp_growth = list(
    file    = "gcc_real_gdp_growth.csv",
    columns = c("country", "year", "value")
  ),
  gcc_gdp_current_prices = list(
    file    = "gcc_gdp_current_prices.csv",
    columns = c("country", "year", "value")
  ),
  segment_fallback_ratings = list(
    file    = "segment_fallback_ratings.csv",
    columns = c("segment", "fallback_rating")
  ),
  ttc_pd_table = list(
    file    = "ttc_pd_table.csv",
    columns = c("rating", "ttc_pd")
  ),
  ttc_pd_table_external = list(
    file    = "ttc_pd_table_external.csv",
    columns = c("rating", "ttc_pd")
  ),
  non_oil_gdp_history = list(
    file    = "non_oil_gdp_history.csv",
    columns = c("year", "value")
  ),
  fx_rates = list(
    file    = "fx_rates.csv",
    columns = c("currency_code", "fx_rate")
  ),
  portfolios = list(
    file    = "portfolios.csv",
    columns = c("portfolio_code", "description", "rating_type")
  ),
  collateral_types = list(
    file    = "collateral_types.csv",
    columns = c("collateral_type_id", "external_collateral_type",
                "description", "haircut_general", "haircut_12m",
                "haircut_24m", "realization_period")
  ),
  customer_rating_overrides = list(
    file    = "customer_rating_overrides.csv",
    columns = c("customer_id", "override_rating", "reason",
                "created_by", "created_at",
                "approved_by", "approved_at",
                "status", "prior_value", "source_run_id"),
    optional = TRUE
  ),
  customer_stage_overrides = list(
    file    = "customer_stage_overrides.csv",
    columns = c("customer_id", "override_stage", "reason",
                "created_by", "created_at",
                "approved_by", "approved_at",
                "status", "prior_value", "source_run_id"),
    optional = TRUE
  ),
  customer_restructuring_overrides = list(
    file    = "customer_restructuring_overrides.csv",
    columns = c("customer_id", "override_restructuring", "reason",
                "created_by", "created_at",
                "approved_by", "approved_at",
                "status", "prior_value", "source_run_id"),
    optional = TRUE
  )
)


# Override files Shiny can write back. Listed separately because the
# orchestrator reloads these every run (their hashes go into the manifest)
# and Shiny mutates them through write_override(). Only rows with
# status == "approved" are applied at run time — see active_overrides().
OVERRIDE_FILES <- c(
  "customer_rating_overrides",
  "customer_stage_overrides",
  "customer_restructuring_overrides"
)


#' Filter an override tibble to active rows (status == "approved").
#'
#' Used by the portfolio-view step to apply only approved overrides while
#' the underlying CSV may contain draft / pending / revoked history.
#'
#' @param overrides_df  data.frame loaded from one of the override CSVs.
#' @return tibble with only approved rows. NULL-safe.
active_overrides <- function(overrides_df) {
  if (is.null(overrides_df) || nrow(overrides_df) == 0) return(overrides_df)
  if (!"status" %in% colnames(overrides_df)) {
    # Legacy schema (no status col) — assume all approved.
    return(overrides_df)
  }
  overrides_df[
    !is.na(overrides_df$status) &
      tolower(trimws(as.character(overrides_df$status))) == "approved",
    , drop = FALSE]
}


#' Write an override CSV (used by Shiny edits and CLI tools).
#'
#' Validates against the registered columns spec and writes atomically
#' (write to .tmp, then rename) so a partial write can never corrupt
#' the source of truth. The audit columns (created_by, created_at,
#' status etc.) are NOT auto-filled by this function — Shiny is
#' responsible for setting them. Missing audit columns become empty
#' strings on write so the file shape stays consistent.
#'
#' @param key    one of OVERRIDE_FILES
#' @param df     data.frame to write
#' @param static_dir static reference directory (default data-raw/static)
#' @return invisibly returns the path written
write_override <- function(key, df, static_dir = "data-raw/static") {
  if (!key %in% OVERRIDE_FILES) {
    stop(sprintf("write_override: '%s' is not an override file. Allowed: %s",
                 key, paste(OVERRIDE_FILES, collapse = ", ")))
  }
  spec <- STATIC_FILE_SPECS[[key]]
  if (is.null(spec)) {
    stop(sprintf("write_override: no spec for '%s'", key))
  }
  if (!is.data.frame(df)) {
    stop("write_override: df must be a data.frame")
  }
  # Allow callers to pass only the required columns — fill in the rest
  # as empty so the file shape remains stable.
  for (col in spec$columns) {
    if (!(col %in% colnames(df))) df[[col]] <- ""
  }
  # Reorder to spec columns + drop extras (keeps file diff-clean).
  df <- df[, spec$columns, drop = FALSE]

  if (!dir.exists(static_dir)) {
    dir.create(static_dir, showWarnings = FALSE, recursive = TRUE)
  }
  path <- file.path(static_dir, spec$file)
  tmp  <- paste0(path, ".tmp")
  utils::write.csv(df, tmp, row.names = FALSE, na = "")
  file.rename(tmp, path)
  invisible(path)
}


#' Load all static reference data from CSVs.
#'
#' All files are required. If any file is missing or has the wrong columns
#' an error is raised — the run cannot proceed without complete reference
#' data.
#'
#' staging_thresholds is returned as a NAMED LIST (key -> typed value)
#' rather than a tibble, because it is logically a small set of scalar
#' parameters. Numeric strings get parsed to double; everything else stays
#' character.
#'
#' Rating labels are normalised on load: the source workbook has both
#' "QDB 1+" (MasterRatingScale) and "QDB 1 +" (TTC table, Inputs_Lending
#' Portfolio) for the same rating. We collapse internal whitespace
#' so all downstream code can match on a single canonical form.
#'
#' @param static_dir Path to data-raw/static/
#' @return Named list of 11 elements
load_static_reference <- function(static_dir) {
  if (!dir.exists(static_dir)) {
    stop("Static reference directory does not exist: ", static_dir)
  }

  result <- vector("list", length(STATIC_FILE_SPECS))
  names(result) <- names(STATIC_FILE_SPECS)

  for (key in names(STATIC_FILE_SPECS)) {
    spec <- STATIC_FILE_SPECS[[key]]
    path <- file.path(static_dir, spec$file)
    is_optional <- isTRUE(spec$optional)
    if (!file.exists(path)) {
      if (is_optional) {
        # Return an empty tibble with the expected schema
        result[[key]] <- tibble::as_tibble(setNames(
          replicate(length(spec$columns), character(0), simplify = FALSE),
          spec$columns
        ))
        next
      }
      stop("Static reference file missing: ", path)
    }

    df <- tryCatch(
      readr::read_csv(path, show_col_types = FALSE,
                      name_repair = "unique_quiet",
                      comment = "#"),
      error = function(e) NULL
    )
    # An empty-but-present file (header only) parses to a 0-row tibble or
    # may fail to parse — handle both for optional files.
    if (is.null(df) || nrow(df) == 0) {
      if (is_optional) {
        result[[key]] <- tibble::as_tibble(setNames(
          replicate(length(spec$columns), character(0), simplify = FALSE),
          spec$columns
        ))
        next
      }
      if (is.null(df)) stop("Could not parse: ", path)
    }

    missing_cols <- setdiff(spec$columns, colnames(df))
    if (length(missing_cols) > 0) {
      stop(sprintf("File %s is missing expected columns: %s",
                   spec$file, paste(missing_cols, collapse = ", ")))
    }

    if (isTRUE(spec$key_value)) {
      result[[key]] <- flatten_key_value_csv(df)
    } else {
      df <- normalise_rating_columns(df)
      result[[key]] <- df
    }
  }

  result
}


#' Normalise any column whose name suggests it contains rating labels.
#'
#' Collapses internal whitespace ("QDB 1 +" -> "QDB 1+") so labels match
#' across the various source tables. Applied to columns named "rating",
#' "rating_after_1_notch_downgrade", "fallback_rating", or anything ending
#' in "_rating".
normalise_rating_columns <- function(df) {
  rating_cols <- grep("(^rating$|^rating_after_|fallback_rating|_rating$)",
                      colnames(df), value = TRUE)
  for (col in rating_cols) {
    if (is.character(df[[col]])) {
      df[[col]] <- normalise_rating_label(df[[col]])
    }
  }
  df
}


#' Normalise a rating label.
#'
#' "QDB 1 +"  -> "QDB 1+"
#' "QDB 1+"   -> "QDB 1+"
#' "QDB 1 -"  -> "QDB 1-"
#' "Baa 3"    -> "Baa3"   (defensive — not seen in current data)
#'
#' @param x character vector
#' @return character vector, same length
normalise_rating_label <- function(x) {
  out <- as.character(x)
  # Trim leading/trailing whitespace
  out <- trimws(out)
  # Collapse multiple internal whitespaces to a single space
  out <- gsub("\\s+", " ", out)
  # Remove space immediately before + or -
  out <- gsub("\\s+([+-])$", "\\1", out)
  out
}


#' Convert a key/value/description tibble into a typed named list.
#'
#' Numeric strings (matching ^-?[0-9]+(\\.[0-9]+)?$) become doubles;
#' everything else stays as character. Description column is dropped.
#'
#' @param df Tibble with columns key, value, description
#' @return Named list
flatten_key_value_csv <- function(df) {
  out <- list()
  for (i in seq_len(nrow(df))) {
    key <- as.character(df$key[i])
    raw <- as.character(df$value[i])
    if (is.na(raw)) {
      out[[key]] <- NA
      next
    }
    if (grepl("^-?[0-9]+(\\.[0-9]+)?$", raw)) {
      out[[key]] <- as.numeric(raw)
    } else {
      out[[key]] <- raw
    }
  }
  out
}
