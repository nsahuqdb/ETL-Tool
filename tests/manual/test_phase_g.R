# ============================================================================
# tests/manual/test_phase_g.R
#
# Smoke test for Phase G — writes all 18 output CSVs to ./test_output/Output/
# and validates them against the bundled reference at /home/claude/work/output_files/Output/
# (or the path configured in run_cfg$paths$reference_outputs).
#
# Run from project root:
#     setwd("path/to/ifrs9_etl")
#     source("tests/manual/test_phase_g.R")
#
# What it does:
#   1. Sets up Phases B/C/D/F (loads config + static, reads inputs, builds
#      transformations, builds LifeTimeParameterOther + StPD).
#   2. Calls write_all_outputs() — writes the 18 CSVs to ./test_output/Output/.
#   3. For the 6 STATIC config CSVs: byte-diffs against bundled. Expects
#      100% match.
#   4. For the 10 PER-RECORD CSVs: row-count + header check.
#   5. For the 2 DERIVED CSVs (LifeTimeParameterOther, StPD): row-count +
#      header check.
#
# What "PASS" looks like:
#   - All 18 files exist.
#   - 6 static CSVs byte-match bundled.
#   - 10 per-record + 2 derived CSVs have correct headers and >0 rows.
# ============================================================================

source("R/io_helpers.R")
source("R/load_config.R")
source("R/load_static.R")
source("R/read_inputs.R")
source("R/transform_lending.R")
source("R/lending_portfolio_view.R")
source("R/transform_investments.R")
source("R/investment_portfolio_view.R")
source("R/macro_model.R")
source("R/pd_term_structure.R")
source("R/lifetime_parameter_other.R")
source("R/build_stpd.R")
source("R/output_writers.R")


cat("\n========== PHASE G :: setup ==========\n")
run_cfg   <- load_run_config("config.yml")
model_cfg <- load_model_config(run_cfg$paths$model_config)
static    <- load_static_reference(run_cfg$paths$static_dir)

# Build country-weighted GCC GDP history (same as test_phase_f)
build_gcc_history <- function(static) {
  growth <- static$gcc_real_gdp_growth
  prices <- static$gcc_gdp_current_prices
  g <- tidyr::pivot_wider(growth, names_from = country, values_from = value)
  p <- tidyr::pivot_wider(prices, names_from = country, values_from = value)
  yrs <- intersect(g$year, p$year)
  yrs <- yrs[yrs >= 1982]
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
gcc_history <- build_gcc_history(static)

model_inputs <- load_model_inputs(
  path                = run_cfg$paths$model_inputs,
  model_cfg           = model_cfg,
  scenarios           = static$scenario_severity,
  gcc_history         = gcc_history,
  non_oil_gdp_history = static$non_oil_gdp_history$value
)

inputs <- read_all_inputs(run_cfg$paths$input_dir)
cat(sprintf("  loaded %d input files\n", length(inputs)))


# ---------------------------------------------------------------------------
# Phase D — transformations + portfolio views
# ---------------------------------------------------------------------------
cat("\n========== PHASE G :: build Phase D outputs ==========\n")
trans_l    <- build_transformation_lending(inputs, static, model_cfg, run_cfg)
out_l      <- build_lending_portfolio_view(trans_l, inputs, static, model_cfg)
cm_view    <- out_l$portfolio
trans_l    <- out_l$transformation

trans_i    <- build_transformation_investments(inputs, static, model_cfg, run_cfg)
out_i      <- build_investment_portfolio_view(trans_i, inputs, static)
inv_view   <- out_i$portfolio
trans_i    <- out_i$transformation

cat(sprintf("  trans_l rows:  %d (lending contracts)\n", nrow(trans_l)))
cat(sprintf("  cm_view rows:  %d (lending customers)\n", nrow(cm_view)))
cat(sprintf("  trans_i rows:  %d (investment contracts)\n", nrow(trans_i)))
cat(sprintf("  inv_view rows: %d (investment 'customers')\n", nrow(inv_view)))


# ---------------------------------------------------------------------------
# Phase F — derived outputs
# ---------------------------------------------------------------------------
cat("\n========== PHASE G :: build Phase F outputs ==========\n")

ltpo <- build_lifetime_parameter_other(inputs$RepaymentSchedule,
                                       trans_l, run_cfg)

scenarios <- static$scenario_severity[, c("scenario", "severity_z")]

internal_ts <- build_pd_term_structure(
  ttc_pd_table = static$ttc_pd_table,
  scenarios    = scenarios,
  model_cfg    = model_cfg,
  model_inputs = model_inputs,
  rating_type  = "Internal"
)
external_ts <- build_pd_term_structure(
  ttc_pd_table = static$ttc_pd_table_external,
  scenarios    = scenarios,
  model_cfg    = model_cfg,
  model_inputs = model_inputs,
  gcc_history  = gcc_history,
  rating_type  = "External"
)

portfolios_for_stpd <- tibble::tribble(
  ~portfolio,         ~rating_type,
  "Business Finance", "Internal",
  "Off BS",           "Internal",
  "Al Dhameen",       "Internal",
  "Tasdeer",          "Internal",
  "Banks and Fis",    "External",
  "Investments",      "External"
)
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
  portfolios                = portfolios_for_stpd,
  ratings                   = ratings_combined,
  run_cfg                   = run_cfg,
  max_month                 = 600
)

cat(sprintf("  ltpo rows: %d\n", nrow(ltpo)))
cat(sprintf("  stpd rows: %d\n", nrow(stpd)))


# ---------------------------------------------------------------------------
# Build the staging-flag tibble for write_customer_staging_flag_1.
# Reads the raw CustomerStagingFlag input directly. Bundled output uses
# uppercase column names that map to TRUE/FALSE/blank.
# ---------------------------------------------------------------------------
csf_raw <- inputs$CustomerStagingFlag
# Defensive: convert to lowercase column-name access
nm <- toupper(names(csf_raw))
get_col <- function(col_letter, fallback = NA) {
  idx <- which(nm == col_letter | names(csf_raw) == col_letter)
  if (length(idx) == 0) return(rep(fallback, nrow(csf_raw)))
  csf_raw[[idx[1]]]
}

# CustomerStagingFlag.xlsx columns (per inspection):
#   A=ExtractDate B=CustomerId C=IsDefault D=IsWatchlist E=IsInsolvency
#   F=IsDefaultInGCC G=IsLocal1 H=IsLocal2 I=IsLocal3 J=IsLocal4 K=IsLocal5 L=IsLocal6
to_bool <- function(x) {
  if (is.logical(x)) return(x)
  s <- toupper(trimws(as.character(x)))
  ifelse(s %in% c("TRUE","T","1","Y","YES"), TRUE,
   ifelse(s %in% c("FALSE","F","0","N","NO"), FALSE, NA))
}

customer_flags <- tibble::tibble(
  customer_id      = csf_raw[[2]],
  is_default       = to_bool(csf_raw[[3]]),
  is_watchlist     = to_bool(csf_raw[[4]]),
  is_insolvency    = to_bool(csf_raw[[5]]),
  is_default_in_gcc = to_bool(csf_raw[[6]]),
  is_local1        = to_bool(csf_raw[[7]]),
  is_local2        = to_bool(csf_raw[[8]]),
  is_local3        = to_bool(csf_raw[[9]]),
  is_local4        = to_bool(csf_raw[[10]]),
  is_local5        = to_bool(csf_raw[[11]]),
  is_local6        = to_bool(csf_raw[[12]])
)
cat(sprintf("  customer_flags rows: %d\n", nrow(customer_flags)))


# ---------------------------------------------------------------------------
# Build collateral + allocation tibbles from raw inputs
# ---------------------------------------------------------------------------
coll_raw  <- inputs$Collateral
acoll_raw <- inputs$AccountCollateralAllocation

# Collateral.xlsx columns: A=ExtractDate B=CollateralId C=ParentCollateralId
#   D=CollateralCode E=CollateralTypeId F=CollateralValue G=CollateralCurrency
# We need: collateral_id, collateral_type_id, currency, value
collateral_tbl <- tibble::tibble(
  collateral_id      = as.character(coll_raw[[2]]),
  collateral_type_id = as.integer(coll_raw[[5]]),
  currency           = if (ncol(coll_raw) >= 7) as.character(coll_raw[[7]]) else "QAR",
  value              = as.numeric(coll_raw[[6]])
)

# AccountCollateralAllocation columns: A=ExtractDate B=CollateralId
#   C=ContractId D=AllocationPercentage
account_collateral_allocation_tbl <- tibble::tibble(
  collateral_id         = as.character(acoll_raw[[2]]),
  contract_id           = as.character(acoll_raw[[3]]),
  allocation_percentage = as.numeric(acoll_raw[[4]])
)

cat(sprintf("  collateral rows:  %d\n", nrow(collateral_tbl)))
cat(sprintf("  allocation rows:  %d\n", nrow(account_collateral_allocation_tbl)))


# ---------------------------------------------------------------------------
# Investment customers tibble — for Customer/Origination_2 writers.
# Use inv_view (one row per investment account/'customer') filtered to unique.
# Both AccountId and CustomerId are 1..73 in the bundled output (one-to-one).
# ---------------------------------------------------------------------------
investment_customers <- tibble::tibble(
  customer_id_inv = trans_i$customer_id_inv
)


# ---------------------------------------------------------------------------
# Phase G — write all 18 outputs
# ---------------------------------------------------------------------------
cat("\n========== PHASE G :: write_all_outputs ==========\n")
output_dir <- "test_output/Output"

paths <- write_all_outputs(
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

cat(sprintf("  wrote %d files to %s/\n", length(paths), output_dir))
for (n in names(paths)) {
  size_kb <- file.info(paths[[n]])$size / 1024
  cat(sprintf("    %-32s %8.1f KB\n", paste0(n, ".csv"), size_kb))
}


# ---------------------------------------------------------------------------
# Validation
# ---------------------------------------------------------------------------
cat("\n========== PHASE G :: validation ==========\n")

unit_results <- list()
check <- function(label, cond) {
  status <- if (isTRUE(cond)) "PASS" else "FAIL"
  cat(sprintf("  [%s] %s\n", status, label))
  isTRUE(cond)
}

# Bundled reference path. Try multiple common locations.
ref_dir <- run_cfg$paths$reference_outputs %||% NA_character_
candidates <- c(
  ref_dir,
  "../output_files/Output",
  "../Output",
  "Output",
  "reference/Output",
  "/home/claude/work/output_files/Output"
)
ref_dir <- NA_character_
for (cand in candidates) {
  if (!is.na(cand) && dir.exists(cand)) {
    ref_dir <- normalizePath(cand)
    break
  }
}

cat(sprintf("  output dir:    %s\n", normalizePath(output_dir)))
if (is.na(ref_dir)) {
  cat("  reference dir: <not found> — STATIC byte-diff checks will be SKIPPED.\n")
  cat("  To enable byte-diff: place the bundled Output/ folder at one of:\n")
  for (cand in candidates[!is.na(candidates)]) {
    cat(sprintf("    - %s\n", cand))
  }
  cat("    or set paths.reference_outputs in config.yml.\n")
} else {
  cat(sprintf("  reference dir: %s\n", ref_dir))
}
cat("\n")


# ----- Static config writers: byte-diff -----
static_files <- c("FxRate.csv", "Portfolios.csv", "PortfolioRatingType.csv",
                   "RatingTypes.csv", "Ratings.csv", "CollateralType.csv")

for (f in static_files) {
  out_path <- file.path(output_dir, f)
  if (!file.exists(out_path)) {
    unit_results[[paste0("STATIC_", f)]] <- check(
      sprintf("STATIC %-26s [output missing]", f), FALSE)
    next
  }

  if (is.na(ref_dir)) {
    # No reference available — skip (don't count as fail)
    cat(sprintf("  [SKIP] STATIC %-26s [no reference, output exists at %s]\n",
                f, out_path))
    next
  }

  ref_path <- file.path(ref_dir, f)
  if (!file.exists(ref_path)) {
    cat(sprintf("  [SKIP] STATIC %-26s [reference file not found at %s]\n",
                f, ref_path))
    next
  }
  ref_bytes <- readBin(ref_path, "raw", file.info(ref_path)$size)
  out_bytes <- readBin(out_path, "raw", file.info(out_path)$size)
  match_ok <- identical(ref_bytes, out_bytes)
  unit_results[[paste0("STATIC_", f)]] <- check(
    sprintf("STATIC %-26s byte-match (%d vs %d bytes)", f,
            length(out_bytes), length(ref_bytes)),
    match_ok)
}


# ----- Per-record + derived writers: header + row-count check -----
record_specs <- list(
  AccountMaster_1             = list(rows = nrow(trans_l),
                                       header = c("ExtractDate","ContractId","LimId","CustomerId",
                                                  "PortfolioCode","AccountCode","AccountType")),
  AccountMaster_2             = list(rows = nrow(trans_i),
                                       header = c("ExtractDate","ContractId","LimId","CustomerId",
                                                  "PortfolioCode","AccountCode","AccountType")),
  CustomerMaster_1            = list(rows = nrow(cm_view),
                                       header = c("ExtractDate","CustomerId","PortfolioCode",
                                                  "OrganizationalUnitCode")),
  CustomerMaster_2            = list(rows = nrow(investment_customers),
                                       header = c("ExtractDate","CustomerId","PortfolioCode",
                                                  "BusinessUnitCode")),
  CustomerStagingFlag_1       = list(rows = nrow(customer_flags),
                                       header = c("ExtractDate","CustomerId","IsDefault","IsWatchlist")),
  CustomerStagingFlag_2       = list(rows = nrow(investment_customers),
                                       header = c("ExtractDate","CustomerId","IsDefault","IsWatchlist")),
  Origination_1               = list(rows = nrow(trans_l),
                                       header = c("EXTRACTDATE","ContractId","OriginationPD12M")),
  Origination_2               = list(rows = nrow(trans_i),
                                       header = c("EXTRACTDATE","ContractId","OriginationPD12M")),
  Collateral                  = list(rows = nrow(collateral_tbl),
                                       header = c("ExtractDate","CollateralId","ParentCollateralId")),
  AccountCollateralAllocation = list(rows = nrow(account_collateral_allocation_tbl),
                                       header = c("ExtractDate","CollateralId","ContractId",
                                                  "AllocationPercentage")),
  LifeTimeParameterOther      = list(rows = nrow(ltpo),
                                       header = c("ExtractDate","ContractId","MonthLifetime",
                                                  "EADLifetime","LGDLifetime",
                                                  "PaymentScheduleLifetime","TotalLimitLifetime")),
  StPD                        = list(rows = nrow(stpd),
                                       header = c("ExtractDate","PortfolioCode","PDBucketDim1",
                                                  "PDBucketDim2","MonthLifetime","PDMarginal","PDCumulative"))
)

for (name in names(record_specs)) {
  spec <- record_specs[[name]]
  out_path <- file.path(output_dir, paste0(name, ".csv"))
  if (!file.exists(out_path)) {
    unit_results[[paste0("REC_", name)]] <- check(
      sprintf("REC    %-26s [missing]", name), FALSE)
    next
  }

  # Read header + count rows
  con <- file(out_path, "rb")
  raw <- readBin(con, "raw", file.info(out_path)$size)
  close(con)

  # Split on \r\n (or \n if no \r\n)
  txt <- rawToChar(raw)
  lines <- strsplit(txt, "\r\n", fixed = TRUE)[[1]]
  if (length(lines) == 1) lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  if (length(lines) > 0 && lines[length(lines)] == "") {
    lines <- lines[-length(lines)]
  }

  n_data_rows <- length(lines) - 1
  header_csv  <- if (length(lines) > 0) lines[1] else ""
  header_cols <- strsplit(header_csv, ",", fixed = TRUE)[[1]]

  # Header check: spec$header values must all appear in actual header (in order)
  hdr_ok <- length(spec$header) <= length(header_cols) &&
             all(header_cols[seq_along(spec$header)] == spec$header)
  rows_ok <- n_data_rows == spec$rows

  unit_results[[paste0("REC_", name, "_HDR")]] <- check(
    sprintf("REC    %-26s header check", name), hdr_ok)
  unit_results[[paste0("REC_", name, "_ROW")]] <- check(
    sprintf("REC    %-26s row count = %d (expected %d)",
             name, n_data_rows, spec$rows),
    rows_ok)

  # CRLF check on first 2 rows (header + first data row)
  has_crlf <- grepl("\r\n", txt, fixed = TRUE)
  unit_results[[paste0("REC_", name, "_CRLF")]] <- check(
    sprintf("REC    %-26s CRLF line endings", name), has_crlf)
}


# ----- Optional: byte-diff of derived files against bundle (informational) -----
cat("\n========== PHASE G :: optional byte-diff vs bundled ==========\n")
if (is.na(ref_dir)) {
  cat("  <reference dir not found, skipped>\n")
} else {
  optional_diff <- c("LifeTimeParameterOther.csv", "StPD.csv",
                      "AccountMaster_1.csv", "AccountMaster_2.csv",
                      "CustomerMaster_1.csv", "CustomerMaster_2.csv",
                      "CustomerStagingFlag_1.csv", "CustomerStagingFlag_2.csv",
                      "Origination_1.csv", "Origination_2.csv",
                      "Collateral.csv", "AccountCollateralAllocation.csv")
  for (f in optional_diff) {
    ref_path <- file.path(ref_dir, f)
    out_path <- file.path(output_dir, f)
    if (!file.exists(ref_path) || !file.exists(out_path)) {
      cat(sprintf("  %-32s [n/a]\n", f)); next
    }
    ref_size <- file.info(ref_path)$size
    out_size <- file.info(out_path)$size
    ref_bytes <- readBin(ref_path, "raw", ref_size)
    out_bytes <- readBin(out_path, "raw", out_size)
    if (identical(ref_bytes, out_bytes)) {
      cat(sprintf("  %-32s MATCH\n", f))
    } else {
      cat(sprintf("  %-32s DIFFER (mine=%d, ref=%d)\n", f, out_size, ref_size))
    }
  }
}


# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
cat("\n========== PHASE G SUMMARY ==========\n")
n_pass <- sum(unlist(unit_results))
n_total <- length(unit_results)
cat(sprintf("  %d/%d checks passed\n", n_pass, n_total))
if (n_pass == n_total) {
  cat("  PHASE G :: PASS\n")
} else {
  cat("  PHASE G :: FAIL\n")
  failed <- names(unit_results)[!unlist(unit_results)]
  cat(sprintf("  Failed: %s\n", paste(failed, collapse = ", ")))
}
