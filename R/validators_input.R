# ============================================================================
# R/validators_input.R
#
# Input-stage validators. Run after read_all_inputs() but before any
# transformation. Each validator gets the inputs list (and optionally static
# reference data) via run_validation_suite(args = list(...)).
#
# Coverage:
#   * Presence of all 12 input files
#   * Header schema (required columns by position)
#   * Row counts > 0
#   * Required ID columns are non-empty
#   * Date columns parse
#   * Numeric columns are numeric and finite
#   * Uniqueness of primary keys
#   * Foreign key integrity (CustomerId in AccountMaster ⊆ CustomerMaster, etc.)
#   * Domain checks (currency in known set, ratings in scale, etc.)
#
# Designed to be exhaustive and noisy — each finding is one validator with a
# specific id, so failures are easy to triage.
# ============================================================================


# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# NOTE — input column schemas live in R/input_schemas.R since H6.
# A previous version of this file defined a parallel INPUT_SCHEMAS list
# that mapped column positions to expected uppercase header names. That
# duplicated the new schema layer and (worse) silently overrode the
# canonical definition when this file was sourced after input_schemas.R.
# It has been removed. The canonical INPUT_SCHEMAS in input_schemas.R is
# the only source of truth for column shape; INPUT_FILE_SPECS in
# read_inputs.R is the source of truth for filenames.
# ---------------------------------------------------------------------------


# ---------------------------------------------------------------------------
# Helper utilities (internal)
# ---------------------------------------------------------------------------

.is_input_present <- function(inputs, key) {
  !is.null(inputs[[key]]) && is.data.frame(inputs[[key]]) && nrow(inputs[[key]]) > 0
}

.col_to_upper <- function(name) toupper(trimws(as.character(name)))

# Find the column index for a given header in a tibble whose names may be
# the original uppercase ("CUSTOMERID"), the canonical snake_case
# ("customer_id"), or a mangled readxl name. Returns NA if not found.
.find_col_idx <- function(df, target_upper) {
  if (is.null(df)) return(NA_integer_)
  # Mapping from uppercase -> canonical snake_case used by INPUT_SCHEMAS.
  upper_to_canonical <- c(
    "CUSTOMERID"          = "customer_id",
    "CUSTOMERNAME"        = "customer_name",
    "CONTRACTID"          = "contract_id",
    "ACCOUNTTYPE"         = "account_type",
    "PORTFOLIOCODE"       = "portfolio_code",
    "OPENDATE"            = "open_date",
    "PASTDUEDAYS"         = "past_due_days",
    "PASTDUE_DAYS"        = "past_due_days",
    "ONBALANCE"           = "on_balance",
    "OFFBALANCE"          = "off_balance",
    "MATURITYDATE"        = "maturity_date",
    "MATURITYDAT"         = "maturity_date",     # truncated SQL export
    "EIR"                 = "eir",
    "NOMINALINTERESTRATE" = "nominal_int_rate",
    "PAYMENTFREQUENCY"    = "payment_frequency",
    "PAYMENT_FRE"         = "payment_frequency", # truncated SQL export
    "PAYMENTTYPEID"       = "payment_type_id",
    "DEFERRALPERIOD"      = "deferral_period",
    "CCF"                 = "ccf",
    "EAD"                 = "ead",
    "RATING"              = "rating",
    "STAGE"               = "stage",
    "IMPAIRMENTAMOUNT"    = "impairment_amount",
    "ISDEFAULT"           = "is_default",
    "ISWATCHLIST"         = "is_watchlist",
    "ISINSOLVENCY"        = "is_insolvency",
    "ISDEFAULTINGCC"      = "is_default_in_gcc",
    "ISLOCAL1"            = "is_local1",
    "COLLATERALID"        = "collateral_id",
    "COLLATERALVALUE"     = "value",
    "ALLOCATIONPERCENTAGE" = "allocation_percentage",
    "CURRENCYCODE"        = "currency_code",
    "CUR"                 = "currency_code",      # truncated SQL export
    "EXTRACTDA"           = "extract_date",
    "EXTRACTDATE"         = "extract_date",
    "INDUSTRYCODE"        = "industry_code",
    "INDUSTRYDESCRIPTION" = "industry_description",
    "PD12M"               = "origination_pd_12m"
  )
  candidates <- c(target_upper, upper_to_canonical[target_upper])
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  cn_upper <- vapply(names(df), .col_to_upper, character(1))
  cn_lower <- tolower(names(df))
  for (cand in candidates) {
    hit <- which(cn_upper == toupper(cand) | cn_lower == tolower(cand))
    if (length(hit) > 0) return(hit[1])
  }
  NA_integer_
}


# Robust date parser that handles every format the IFRSIN inputs use.
#
# Inputs encountered in practice:
#   * POSIXct/POSIXt   — readxl gives this for date-formatted .xlsx cells
#   * Date             — same, sometimes
#   * numeric (Excel serial)         — values in 1..100,000 (1900..2173 ish)
#   * numeric (Unix epoch seconds)   — values around 10^9 (~1973..)
#   * character "YYYY-MM-DD"
#   * character "DD-MMM-YY"          — Oracle SQL*Plus format like "31-DEC-25"
#   * character "MM/DD/YYYY" or "DD/MM/YYYY"
#
# Returns a Date vector of the same length, with NA where parsing failed.
.parse_any_date <- function(x) {
  n <- length(x)
  if (n == 0) return(as.Date(character(0)))

  # Already a date type?
  if (inherits(x, c("POSIXct", "POSIXt"))) return(as.Date(x))
  if (inherits(x, "Date"))                  return(x)

  out <- rep(as.Date(NA), n)

  # Numeric branch: Excel serial vs Unix epoch seconds, by magnitude.
  x_num <- suppressWarnings(as.numeric(as.character(x)))
  excel_range <- !is.na(x_num) & x_num >= 1     & x_num < 100000
  unix_range  <- !is.na(x_num) & x_num >= 1e8   & x_num < 1e11
  out[excel_range] <- as.Date(x_num[excel_range], origin = "1899-12-30")
  out[unix_range]  <- as.Date(as.POSIXct(x_num[unix_range],
                                            origin = "1970-01-01", tz = "UTC"))

  # Character branch for whatever's left
  unparsed <- is.na(out) & !is.na(x)
  if (any(unparsed)) {
    s <- as.character(x)
    for (fmt in c("%Y-%m-%d", "%d-%b-%y", "%d-%B-%Y",
                   "%m/%d/%Y", "%d/%m/%Y", "%Y%m%d")) {
      to_try <- unparsed
      if (!any(to_try)) break
      parsed <- suppressWarnings(as.Date(s[to_try], format = fmt))
      idx_orig <- which(to_try)
      good     <- !is.na(parsed)
      out[idx_orig[good]] <- parsed[good]
      unparsed[idx_orig[good]] <- FALSE
    }
  }

  out
}


# ---------------------------------------------------------------------------
# Validators
# ---------------------------------------------------------------------------

#' Build the full input-stage validator list.
#'
#' Returns a list of make_validator() objects to feed into
#' run_validation_suite("INPUT", validators, args = list(inputs = inputs,
#'                                                      static = static)).
build_input_validators <- function() {
  v <- list()

  # ---- Presence checks (one per expected file) ----
  # Note: file paths come from INPUT_FILE_SPECS (the loader registry),
  # not INPUT_SCHEMAS (which holds the canonical-column schemas).
  for (key in names(INPUT_FILE_SPECS)) {
    local({
      k <- key
      file_name <- INPUT_FILE_SPECS[[k]]$file
      v[[length(v) + 1]] <<- make_validator(
        id          = sprintf("INPUT_%s_present", k),
        severity    = "ERROR",
        context     = k,
        description = sprintf("%s is present and non-empty", file_name),
        rationale   = paste0(
          "If the file is missing, downstream transforms cannot run. ",
          "Most input files are produced by the upstream SQL extracts; a ",
          "missing file usually means the extract step failed or wrote to ",
          "the wrong location."),
        remediation = sprintf(paste0(
          "Confirm '%s' exists in the configured input directory and ",
          "contains at least one data row. Re-run the SQL extract if needed."),
          file_name),
        tags        = c("pre_run"),
        suppressible = FALSE,         # never silently ignore a missing file
        fn = function(inputs, ...) {
          ok <- .is_input_present(inputs, k)
          list(passed = ok,
               details = if (!ok) {
                           list(message = sprintf("%s is missing or has zero rows",
                                                  file_name))
                         } else NULL)
        }
      )
    })
  }

  # NOTE: The previous "header position" validator loop (validators that
  # checked col names at expected positions) was removed in H7. Its job
  # is now done by apply_input_schema() at LOAD time — every input read
  # passes through the schema layer in read_inputs.R, which produces
  # clearer errors when a column is missing or mistyped, and does so
  # before any validator runs. Keeping a duplicate validator here added
  # no signal and broke when INPUT_SCHEMAS changed shape.

  # ---- AccountMaster: ContractId uniqueness ----
  v[[length(v) + 1]] <- make_validator(
    "INPUT_AccountMaster_contractid_unique", "ERROR", context = "AccountMaster",
    description = "Every CONTRACTID in AccountMaster.xlsx is unique",
    rationale = paste0(
      "ContractId is the primary key for the lending pipeline. ",
      "Duplicates would cause double-counting in exposure aggregation, ",
      "incorrect customer-level rollups, and broken joins to repayment ",
      "schedule and origination data."),
    remediation = paste0(
      "Investigate the upstream extract for the duplicate contract(s). ",
      "If duplicates are intentional (e.g. multi-currency variants), the ",
      "extract should be filtered to one row per ContractId before this ",
      "pipeline reads it."),
    tags = c("pre_run"),
    suppressible = FALSE,
    fn = function(inputs, ...) {
      df <- inputs$AccountMaster
      if (is.null(df) || nrow(df) == 0) return(list(passed = TRUE))
      idx <- .find_col_idx(df, "CONTRACTID")
      if (is.na(idx)) return(list(passed = FALSE,
                                    details = list(message = "CONTRACTID column not found")))
      ids <- as.character(df[[idx]])
      dup <- ids[duplicated(ids)]
      list(passed = length(dup) == 0,
           details = if (length(dup) > 0)
             list(message = sprintf("%d duplicate ContractIds (e.g. %s)",
                                    length(dup),
                                    paste(head(unique(dup), 5), collapse=", ")),
                  duplicates = unique(dup))
           else NULL)
    }
  )

  # ---- AccountMaster: CONTRACTID never blank ----
  v[[length(v) + 1]] <- make_validator(
    "INPUT_AccountMaster_contractid_nonblank", "ERROR", context = "AccountMaster",
    description = "Every row in AccountMaster.xlsx has a non-blank CONTRACTID",
    fn = function(inputs, ...) {
      df <- inputs$AccountMaster
      if (is.null(df)) return(list(passed = TRUE))
      idx <- .find_col_idx(df, "CONTRACTID")
      if (is.na(idx)) return(list(passed = FALSE,
                                    details = list(message = "CONTRACTID column not found")))
      ids <- as.character(df[[idx]])
      bad <- is.na(ids) | !nzchar(trimws(ids))
      list(passed = !any(bad),
           details = if (any(bad))
             list(message = sprintf("%d rows have blank ContractId", sum(bad)),
                  bad_rows = which(bad))
           else NULL)
    }
  )

  # ---- AccountMaster: ONBALANCE non-negative numeric ----
  v[[length(v) + 1]] <- make_validator(
    "INPUT_AccountMaster_onbalance_nonneg", "WARN", context = "AccountMaster",
    description = "ONBALANCE in AccountMaster.xlsx is numeric and non-negative",
    fn = function(inputs, ...) {
      df <- inputs$AccountMaster
      if (is.null(df)) return(list(passed = TRUE))
      idx <- .find_col_idx(df, "ONBALANCE")
      if (is.na(idx)) return(list(passed = FALSE,
                                    details = list(message = "ONBALANCE column not found")))
      x <- suppressWarnings(as.numeric(as.character(df[[idx]])))
      n_na  <- sum(is.na(x))
      n_neg <- sum(!is.na(x) & x < 0)
      ok <- n_na == 0 && n_neg == 0
      list(passed = ok,
           details = if (!ok)
             list(message = sprintf("%d non-numeric, %d negative", n_na, n_neg))
           else NULL)
    }
  )

  # ---- AccountMaster: OPENDATE parseable as a date (any supported format) ----
  v[[length(v) + 1]] <- make_validator(
    "INPUT_AccountMaster_opendate_parses", "ERROR", context = "AccountMaster",
    description = "OPENDATE column in AccountMaster.xlsx parses as a date",
    fn = function(inputs, ...) {
      df <- inputs$AccountMaster
      if (is.null(df)) return(list(passed = TRUE))
      idx <- .find_col_idx(df, "OPENDATE")
      if (is.na(idx)) return(list(passed = FALSE,
                                    details = list(message = "OPENDATE column not found")))
      raw    <- df[[idx]]
      parsed <- .parse_any_date(raw)
      n_unparseable <- sum(is.na(parsed) & !is.na(raw))
      list(passed = n_unparseable == 0,
           details = if (n_unparseable > 0)
             list(message = sprintf("%d unparseable OpenDate values", n_unparseable),
                  sample_unparseable = head(raw[is.na(parsed) & !is.na(raw)], 5))
           else NULL)
    }
  )

  # ---- AccountMaster: maturity_date >= open_date ----
  v[[length(v) + 1]] <- make_validator(
    "INPUT_AccountMaster_maturity_after_open", "WARN", context = "AccountMaster",
    description = "MaturityDate >= OpenDate for every contract",
    rationale = paste0(
      "A maturity date earlier than the open date suggests a data-entry ",
      "error or a corrupted record. Such contracts will produce nonsensical ",
      "term-to-maturity calculations downstream and may end up with negative ",
      "MaturityFromExtract values in AccountMaster_1.csv."),
    remediation = paste0(
      "Investigate the affected contracts in the upstream system. Either ",
      "correct the maturity_date in source, or add a contract-level override ",
      "if business has accepted these as legacy records."),
    fn = function(inputs, ...) {
      df <- inputs$AccountMaster
      if (is.null(df)) return(list(passed = TRUE))
      o_idx <- .find_col_idx(df, "OPENDATE")
      m_idx <- .find_col_idx(df, "MATURITYDATE")
      if (is.na(o_idx) || is.na(m_idx)) {
        return(list(passed = FALSE,
                    details = list(message = "open_date or maturity_date column missing")))
      }
      o <- .parse_any_date(df[[o_idx]])
      m <- .parse_any_date(df[[m_idx]])
      bad <- !is.na(o) & !is.na(m) & m < o
      n_bad <- sum(bad)
      list(passed = n_bad == 0,
           details = if (n_bad > 0)
             list(message = sprintf("%d contracts have maturity_date < open_date", n_bad))
           else NULL)
    }
  )

  # ---- CustomerMaster: CUSTOMERID uniqueness ----
  v[[length(v) + 1]] <- make_validator(
    "INPUT_CustomerMaster_customerid_unique", "ERROR", context = "CustomerMaster",
    description = "Every CUSTOMERID in CustomerMaster.xlsx is unique",
    fn = function(inputs, ...) {
      df <- inputs$CustomerMaster
      if (is.null(df)) return(list(passed = TRUE))
      idx <- .find_col_idx(df, "CUSTOMERID")
      if (is.na(idx)) return(list(passed = FALSE,
                                    details = list(message = "CUSTOMERID column not found")))
      ids <- as.character(df[[idx]])
      dup <- ids[duplicated(ids)]
      list(passed = length(dup) == 0,
           details = if (length(dup) > 0)
             list(message = sprintf("%d duplicate CustomerIds", length(dup)),
                  duplicates = unique(dup))
           else NULL)
    }
  )

  # ---- Foreign key: AccountMaster.CustomerId ⊆ CustomerMaster.CustomerId ----
  v[[length(v) + 1]] <- make_validator(
    "INPUT_AccountMaster_customer_fk", "WARN", context = "AccountMaster",
    description = "Every CUSTOMERID in AccountMaster.xlsx exists in CustomerMaster.xlsx",
    fn = function(inputs, ...) {
      am <- inputs$AccountMaster
      cm <- inputs$CustomerMaster
      if (is.null(am) || is.null(cm)) return(list(passed = TRUE))
      am_cust_idx <- .find_col_idx(am, "CUSTOMERID")
      cm_cust_idx <- .find_col_idx(cm, "CUSTOMERID")
      if (is.na(am_cust_idx) || is.na(cm_cust_idx)) {
        return(list(passed = FALSE,
                    details = list(message = "CustomerId column missing in one of the files")))
      }
      am_ids <- unique(as.character(am[[am_cust_idx]]))
      cm_ids <- unique(as.character(cm[[cm_cust_idx]]))
      orphans <- setdiff(am_ids, cm_ids)
      list(passed = length(orphans) == 0,
           details = if (length(orphans) > 0)
             list(message = sprintf("%d CustomerIds in AccountMaster have no CustomerMaster row (e.g. %s)",
                                    length(orphans),
                                    paste(head(orphans, 5), collapse=", ")),
                  orphans = orphans)
           else NULL)
    }
  )

  # ---- AccountCollateralAllocation: AllocationPercentage in [0, 100] ----
  # Raw input is stored as a percentage (e.g. 51.71 = 51.71%), NOT a fraction.
  # The transformation later divides by 100 to produce the fraction in
  # the bundled Output/AccountCollateralAllocation.csv.
  v[[length(v) + 1]] <- make_validator(
    "INPUT_ACA_allocation_in_percent_range", "WARN",
    context = "AccountCollateralAllocation",
    description = "AllocationPercentage (raw, in % units) in [0, 100]",
    fn = function(inputs, ...) {
      df <- inputs$AccountCollateralAllocation
      if (is.null(df)) return(list(passed = TRUE))
      idx <- .find_col_idx(df, "ALLOCATIONPERCENTAGE")
      if (is.na(idx)) return(list(passed = FALSE,
                                    details = list(message = "AllocationPercentage column not found")))
      x <- suppressWarnings(as.numeric(as.character(df[[idx]])))
      n_bad <- sum(!is.na(x) & (x < 0 | x > 100.0001))
      n_na  <- sum(is.na(x))
      ok <- n_bad == 0 && n_na == 0
      list(passed = ok,
           details = if (!ok)
             list(message = sprintf("%d outside [0,100], %d non-numeric", n_bad, n_na))
           else NULL)
    }
  )

  # ---- AccountCollateralAllocation: ContractId ⊆ AccountMaster.ContractId ----
  v[[length(v) + 1]] <- make_validator(
    "INPUT_ACA_contract_fk", "WARN",
    context = "AccountCollateralAllocation",
    description = "Every ContractId in AccountCollateralAllocation exists in AccountMaster",
    rationale = paste0(
      "Orphan rows in ACA point at contracts that no longer exist in the ",
      "live portfolio (closed, written off, or never extracted). They ",
      "occupy collateral capacity that won't be allocated to anything, ",
      "but otherwise don't affect the run because the join in the LGD ",
      "step keeps only AM-side rows."),
    remediation = paste0(
      "Optional cleanup at source: have the SQL extract filter ACA to ",
      "ContractIds that exist in AccountMaster. If the orphan count is ",
      "stable across runs and accepted by the team, suppress this validator ",
      "via Shiny with a justification."),
    fn = function(inputs, ...) {
      aca <- inputs$AccountCollateralAllocation
      am  <- inputs$AccountMaster
      if (is.null(aca) || is.null(am)) return(list(passed = TRUE))
      aca_idx <- .find_col_idx(aca, "CONTRACTID")
      am_idx  <- .find_col_idx(am,  "CONTRACTID")
      if (is.na(aca_idx) || is.na(am_idx)) return(list(passed = FALSE,
        details = list(message = "ContractId missing in one of the files")))
      aca_ids <- unique(as.character(aca[[aca_idx]]))
      am_ids  <- unique(as.character(am[[am_idx]]))
      orphans <- setdiff(aca_ids, am_ids)
      list(passed = length(orphans) == 0,
           details = if (length(orphans) > 0)
             list(message = sprintf("%d ContractIds in ACA absent from AccountMaster",
                                    length(orphans)),
                  orphans = head(orphans, 50))
           else NULL)
    }
  )

  # ---- AccountCollateralAllocation: total allocation per ContractId <= 100% ----
  # Sum the (raw) percentages per ContractId. Allow a tiny FP slack.
  v[[length(v) + 1]] <- make_validator(
    "INPUT_ACA_total_allocation_per_contract", "WARN",
    context = "AccountCollateralAllocation",
    description = "Sum of allocation_percentage per ContractId <= 100.1% (raw input in % units)",
    rationale = paste0(
      "More than 100% allocation indicates either (a) duplicate ACA rows for ",
      "the same (ContractId, CollateralId) pair, or (b) a percentage that ",
      "wasn't normalised. The downstream LGD calculation assumes allocations ",
      "are a partition of the contract's collateral coverage; over-allocated ",
      "rows will overstate covered exposure."),
    remediation = paste0(
      "Investigate the listed contracts in source. Common cause: legacy ACA ",
      "rows for closed-then-reopened contracts. Either de-dup at the source ",
      "extract or accept and suppress with a documented reason."),
    fn = function(inputs, ...) {
      df <- inputs$AccountCollateralAllocation
      if (is.null(df)) return(list(passed = TRUE))
      cid_idx <- .find_col_idx(df, "CONTRACTID")
      pct_idx <- .find_col_idx(df, "ALLOCATIONPERCENTAGE")
      if (is.na(cid_idx) || is.na(pct_idx)) return(list(passed = FALSE,
        details = list(message = "ContractId or AllocationPercentage column missing")))
      cids <- as.character(df[[cid_idx]])
      pcts <- suppressWarnings(as.numeric(as.character(df[[pct_idx]])))
      tot  <- tapply(pcts, cids, sum, na.rm = TRUE)
      bad  <- names(tot)[tot > 100.1]
      list(passed = length(bad) == 0,
           details = if (length(bad) > 0)
             list(message = sprintf("%d ContractIds have total allocation > 100%% (e.g. %s)",
                                    length(bad),
                                    paste(head(bad, 5), collapse=", ")),
                  worst_contracts = head(sort(tot[bad], decreasing = TRUE), 10))
           else NULL)
    }
  )

  # ---- RepaymentSchedule: every ContractId in AccountMaster has at least one row ----
  v[[length(v) + 1]] <- make_validator(
    "INPUT_RS_coverage", "WARN", context = "RepaymentSchedule",
    description = "Every active lending ContractId has a row in RepaymentSchedule.xlsx",
    rationale = paste0(
      "Contracts without a repayment schedule fall back to a single-bucket ",
      "lifetime EAD curve in LifeTimeParameterOther — exposure stays flat ",
      "until maturity. This is correct for revolving / off-balance / ",
      "guarantee products that don't amortise, but a TERM-LOAN missing ",
      "from RepaymentSchedule is a data error: it would be modelled as ",
      "non-amortising which over-states EAD."),
    remediation = paste0(
      "Confirm the missing contracts are revolving/off-balance/guarantee ",
      "products (those don't have a schedule by design). If any term loans ",
      "are in the missing set, fix the upstream extract."),
    fn = function(inputs, ...) {
      rs <- inputs$RepaymentSchedule
      am <- inputs$AccountMaster
      if (is.null(rs) || is.null(am)) return(list(passed = TRUE))
      rs_ids <- unique(as.character(rs[[1]]))
      am_idx <- .find_col_idx(am, "CONTRACTID")
      if (is.na(am_idx)) return(list(passed = FALSE,
        details = list(message = "AccountMaster CONTRACTID not found")))
      am_ids <- unique(as.character(am[[am_idx]]))
      missing <- setdiff(am_ids, rs_ids)
      list(passed = length(missing) == 0,
           details = if (length(missing) > 0)
             list(message = sprintf("%d AccountMaster ContractIds have no RepaymentSchedule rows",
                                    length(missing)),
                  missing = head(missing, 20))
           else NULL)
    }
  )

  # ---- Currency code domain ----
  v[[length(v) + 1]] <- make_validator(
    "INPUT_AccountMaster_currency_known", "WARN", context = "AccountMaster",
    description = "Currency codes are in {QAR, USD}",
    rationale = paste0(
      "Currency codes outside the known set will fall back to QAR in ",
      "downstream FX conversion, silently mis-stating exposure for foreign-",
      "currency contracts."),
    remediation = paste0(
      "Confirm that fx_rates.csv covers any new currency. If a new currency ",
      "is genuinely needed, add it to fx_rates.csv and update this validator's ",
      "expected set."),
    fn = function(inputs, static, ...) {
      df <- inputs$AccountMaster
      if (is.null(df)) return(list(passed = TRUE))
      idx <- .find_col_idx(df, "CURRENCYCODE")
      if (is.na(idx)) return(list(passed = FALSE,
        details = list(message = "currency_code column not found")))
      vals  <- toupper(trimws(as.character(df[[idx]])))
      vals  <- vals[!is.na(vals) & nzchar(vals)]
      known <- if (!is.null(static$fx_rates)) toupper(static$fx_rates$currency_code) else c("QAR","USD")
      bad   <- setdiff(unique(vals), known)
      list(passed = length(bad) == 0,
           details = if (length(bad) > 0)
             list(message = sprintf("Unknown currency codes: %s",
                                    paste(bad, collapse=", ")))
           else NULL)
    }
  )

  # ---- Investments: rating in external rating scale ----
  v[[length(v) + 1]] <- make_validator(
    "INPUT_AccountMasterInvestments_rating_known", "WARN",
    context = "AccountMasterInvestments",
    description = "RATING in AccountMasterInvestments is in the external rating scale (or blank)",
    fn = function(inputs, static, ...) {
      df <- inputs$AccountMasterInvestments
      if (is.null(df)) return(list(passed = TRUE))
      idx <- .find_col_idx(df, "RATING")
      if (is.na(idx)) return(list(passed = FALSE,
                                    details = list(message = "RATING column not found")))
      vals <- trimws(as.character(df[[idx]]))
      vals <- vals[!is.na(vals) & nzchar(vals)]
      ext <- if (!is.null(static$master_rating_scale))
        static$master_rating_scale$rating[static$master_rating_scale$rating_type == "External"]
      else character()
      bad <- setdiff(unique(vals), ext)
      list(passed = length(bad) == 0,
           details = if (length(bad) > 0)
             list(message = sprintf("%d unknown ratings: %s",
                                    length(bad),
                                    paste(head(bad, 10), collapse=", ")))
           else NULL)
    }
  )

  # ---- Single extract date across all inputs (compared on parsed Date,
  #      not raw string — different files use different representations)
  v[[length(v) + 1]] <- make_validator(
    "INPUT_consistent_extract_date", "WARN", context = NA_character_,
    description = "All inputs resolve to the same EXTRACTDA date",
    fn = function(inputs, ...) {
      ed_dates <- list()
      ed_raws  <- list()
      for (key in names(inputs)) {
        df <- inputs[[key]]
        if (is.null(df) || nrow(df) == 0) next
        idx <- .find_col_idx(df, "EXTRACTDA")
        if (is.na(idx)) next
        raw_vals <- df[[idx]]
        # Only need a sample to find the value(s)
        raw_unique <- raw_vals[!is.na(raw_vals)]
        if (length(raw_unique) == 0) next
        raw_unique <- raw_unique[!duplicated(raw_unique)]
        if (length(raw_unique) > 5) raw_unique <- raw_unique[1:5]
        d <- .parse_any_date(raw_unique)
        ed_dates[[key]] <- as.character(unique(d[!is.na(d)]))
        ed_raws[[key]]  <- as.character(raw_unique)
      }
      uniq_dates <- unique(unlist(ed_dates))
      list(passed = length(uniq_dates) <= 1,
           details = if (length(uniq_dates) > 1)
             list(message = sprintf("Multiple distinct dates after parsing: %s",
                                    paste(uniq_dates, collapse = ", ")),
                  per_file_parsed = ed_dates,
                  per_file_raw    = ed_raws)
           else NULL)
    }
  )

  # ---- Inputs' EXTRACTDA matches the run_cfg reporting date ----
  v[[length(v) + 1]] <- make_validator(
    "INPUT_extract_date_matches_run_cfg", "ERROR", context = NA_character_,
    description = "Inputs' EXTRACTDA equals run_cfg$run$extract_date (reporting date)",
    rationale = paste0(
      "Every output CSV stamps EXTRACTDATE from the input data, but ",
      "downstream consumers (regulatory reporting, dashboards) treat the ",
      "configured run.extract_date as authoritative. A mismatch silently ",
      "produces outputs with a stale reporting date label."),
    remediation = paste0(
      "Either re-extract input files for the configured date, or update ",
      "run.extract_date in config.yml to match the snapshot of input data ",
      "you actually have."),
    tags = c("pre_run"),
    fn = function(inputs, run_cfg, ...) {
      if (is.null(run_cfg) || is.null(run_cfg$run$extract_date)) {
        return(list(passed = TRUE))
      }
      cfg_date <- as.Date(run_cfg$run$extract_date)
      input_dates <- character()
      for (key in names(inputs)) {
        df <- inputs[[key]]
        if (is.null(df) || nrow(df) == 0) next
        idx <- .find_col_idx(df, "EXTRACTDA")
        if (is.na(idx)) next
        rv <- df[[idx]]
        rv <- rv[!is.na(rv)]
        if (length(rv) == 0) next
        d <- .parse_any_date(rv[1])
        if (!is.na(d)) input_dates <- c(input_dates, as.character(d))
      }
      uniq <- unique(input_dates)
      ok <- length(uniq) >= 1 && all(as.Date(uniq) == cfg_date)
      list(passed = ok,
           details = if (!ok)
             list(message = sprintf(
                  "config extract_date=%s vs input EXTRACTDA dates: %s",
                  cfg_date, paste(uniq, collapse = ", ")))
             else NULL)
    }
  )

  v
}
