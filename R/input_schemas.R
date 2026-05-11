# =============================================================================
# input_schemas.R
#
# Per-file column SCHEMAS for the 12 input files. Every downstream module
# accesses input columns through CANONICAL names defined here. The schema
# is responsible for:
#
#   1. Locating each canonical column in the raw read (named header,
#      positional fallback, or an alternative-names list).
#   2. Coercing the column to the expected type (integer, numeric,
#      character, Date, logical) at LOAD TIME — so we fail loudly and
#      early if a column is mis-typed (e.g. readxl's logical-vs-numeric
#      sniffer giving us all-NA for what should be integer codes).
#   3. Validating presence: required columns must be found.
#
# Canonical column naming convention: lowercase snake_case (Tidyverse
# style). E.g. customer_id, contract_id, past_due_days, on_balance.
#
# Public entry point:
#   apply_input_schema(raw_df, file_key, schema = INPUT_SCHEMAS) -> tibble
#       Returns a tibble with canonical names + types.
#
# read_inputs.R wraps each file load with apply_input_schema() so that
# transformation/derived/output modules NEVER need to look up positional
# columns or worry about readxl-renamed names.
# =============================================================================


# ---------------------------------------------------------------------------
# Schema entry construction
# ---------------------------------------------------------------------------

#' Construct a single column-schema entry.
#'
#' @param canonical    canonical (output) column name, lowercase snake_case
#' @param sources      character vector of candidate source-column names to
#'                     match against the raw `names(df)`. First match wins.
#'                     If NULL, schema relies entirely on `position`.
#' @param position     1-based positional fallback if no `sources` match.
#'                     If NULL, the column is considered required-by-name
#'                     only.
#' @param type         expected output type: one of
#'                     "integer","numeric","character","date","logical".
#' @param required     logical — error if not findable. Default TRUE.
#' @param description  short description for error messages.
col_spec <- function(canonical, sources = NULL, position = NULL,
                     type = "character", required = TRUE,
                     description = "") {
  if (is.null(sources) && is.null(position)) {
    stop(sprintf("col_spec(%s): need at least one of sources/position",
                 canonical))
  }
  list(canonical = canonical, sources = sources, position = position,
       type = type, required = required, description = description)
}


# ---------------------------------------------------------------------------
# Type coercion
# ---------------------------------------------------------------------------

.coerce_to_type <- function(x, type, canonical) {
  if (length(x) == 0) return(x)
  switch(
    type,
    integer   = suppressWarnings(as.integer(x)),
    numeric   = suppressWarnings(as.numeric(x)),
    character = as.character(x),
    logical   = {
      # Accept TRUE/FALSE, 1/0, "1"/"0", "TRUE"/"FALSE", "Y"/"N", "YES"/"NO"
      if (is.logical(x)) return(x)
      s <- toupper(trimws(as.character(x)))
      out <- rep(NA, length(s))
      out[s %in% c("TRUE","T","1","Y","YES")]  <- TRUE
      out[s %in% c("FALSE","F","0","N","NO")] <- FALSE
      out
    },
    date = {
      # Accept Excel serial dates (numeric origin 1899-12-30), Date,
      # POSIXct, or character "YYYY-MM-DD"/"M/D/YYYY".
      if (inherits(x, "Date")) return(x)
      if (inherits(x, "POSIXct")) return(as.Date(x))
      if (is.numeric(x)) {
        return(as.Date(x, origin = "1899-12-30"))
      }
      s <- as.character(x)
      out <- rep(as.Date(NA), length(s))
      # Try numeric first (excel serial as text)
      sn <- suppressWarnings(as.numeric(s))
      ok_n <- !is.na(sn) & sn > 0
      out[ok_n] <- as.Date(sn[ok_n], origin = "1899-12-30")
      # Then iso / slash-date fallback
      ok_i <- is.na(out) & grepl("^\\d{4}-\\d{1,2}-\\d{1,2}", s)
      out[ok_i] <- suppressWarnings(as.Date(s[ok_i]))
      ok_s <- is.na(out) & grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", s)
      out[ok_s] <- suppressWarnings(as.Date(s[ok_s], format = "%m/%d/%Y"))
      out
    },
    stop(sprintf("Unknown type '%s' for column '%s'", type, canonical))
  )
}


# ---------------------------------------------------------------------------
# Apply schema to a raw tibble
# ---------------------------------------------------------------------------

#' Resolve one column from raw data per its schema.
#'
#' Returns the resolved (typed) vector, or NULL if not found and not required.
#' Errors if required and not found.
.resolve_one_column <- function(raw_df, spec, file_label) {
  found_idx <- NULL

  # 1. Try named sources in order
  if (!is.null(spec$sources)) {
    for (nm in spec$sources) {
      if (nm %in% names(raw_df)) {
        found_idx <- which(names(raw_df) == nm)[1]
        break
      }
    }
  }

  # 2. Positional fallback
  if (is.null(found_idx) && !is.null(spec$position) &&
      ncol(raw_df) >= spec$position) {
    found_idx <- spec$position
  }

  if (is.null(found_idx)) {
    if (isTRUE(spec$required)) {
      stop(sprintf(
        "[%s] required column '%s' not found (sources=[%s], position=%s, ncol=%d, names=[%s])",
        file_label, spec$canonical,
        paste(spec$sources, collapse = ","),
        if (is.null(spec$position)) "NA" else spec$position,
        ncol(raw_df),
        paste(names(raw_df), collapse = ",")))
    }
    return(NULL)
  }

  raw_col <- raw_df[[found_idx]]
  out <- .coerce_to_type(raw_col, spec$type, spec$canonical)

  # Sanity check: numeric/integer cols that come back ALL NA from a
  # non-empty input often signal readxl type misclassification (the
  # Collateral "P" -> <lgl> bug). Flag with a clear warning so the
  # operator can investigate. Don't error — there are legitimate
  # all-NA columns too.
  if (spec$type %in% c("integer", "numeric") &&
      length(out) > 0 && all(is.na(out)) && any(!is.na(raw_col))) {
    warning(sprintf(
      "[%s] column '%s' (sourced from raw col '%s' at position %d) has %d non-NA raw values but ALL coerced to NA — possible type misclassification.",
      file_label, spec$canonical, names(raw_df)[found_idx], found_idx,
      sum(!is.na(raw_col))), call. = FALSE)
  }
  out
}


#' Apply a full schema to a raw tibble.
#'
#' @param raw_df    tibble from readxl/readr/html
#' @param schema    list of col_spec(...) entries
#' @param file_label  string for error messages (e.g. "Collateral.xlsx")
#' @return tibble with canonical columns and types
apply_input_schema <- function(raw_df, schema, file_label = "<input>") {
  if (is.null(raw_df)) {
    stop(sprintf("[%s] raw input is NULL", file_label))
  }
  if (nrow(raw_df) == 0) {
    # Return an empty tibble with the right columns/types
    result <- list()
    for (spec in schema) {
      result[[spec$canonical]] <- .coerce_to_type(character(0), spec$type,
                                                  spec$canonical)
    }
    return(tibble::as_tibble(result))
  }

  result <- list()
  for (spec in schema) {
    val <- .resolve_one_column(raw_df, spec, file_label)
    if (!is.null(val)) {
      result[[spec$canonical]] <- val
    } else {
      # Optional missing column: emit a typed NA-filled column so
      # downstream code can still reference it.
      result[[spec$canonical]] <- .coerce_to_type(rep(NA, nrow(raw_df)),
                                                  spec$type, spec$canonical)
    }
  }
  tibble::as_tibble(result)
}


# ---------------------------------------------------------------------------
# The schemas — ONE place to specify how each input file maps to canonical
# columns. Add new files / new columns here.
# ---------------------------------------------------------------------------

INPUT_SCHEMAS <- list(

  # AccountMaster.xlsx — lending portfolio account-level master
  AccountMaster = list(
    col_spec("extract_date",       sources = "EXTRACTDA",        position = 1,  type = "date"),
    col_spec("contract_id",        sources = "CONTRACTID",       position = 2,  type = "character"),
    col_spec("lim_id",             sources = "LIMID",            position = 3,  type = "character", required = FALSE),
    col_spec("customer_id",        sources = "CUSTOMERID",       position = 4,  type = "character"),
    col_spec("portfolio_code",     sources = "PORTFOLIOCODE",    position = 5,  type = "character"),
    col_spec("account_type",       sources = "ACCOUNTTYPE",      position = 6,  type = "character"),
    col_spec("impairment_amount",  sources = "IMPAIRMENTAMOUNT", position = 7,  type = "numeric", required = FALSE),
    col_spec("stage",              sources = "STAGE",            position = 8,  type = "character", required = FALSE),
    col_spec("open_date",          sources = "OPENDATE",         position = 9,  type = "date"),
    col_spec("past_due_days",      sources = c("PASTDUEDAYS","PASTDUE_DAYS"), position = 11, type = "integer"),
    col_spec("off_balance",        sources = "OFFBALANCE",       position = 17, type = "numeric"),
    col_spec("on_balance",         sources = "ONBALANCE",        position = 18, type = "numeric"),
    col_spec("ead",                sources = "EAD",              position = 19, type = "numeric", required = FALSE),
    col_spec("ccf",                sources = "CCF",              position = 20, type = "numeric", required = FALSE),
    col_spec("maturity_date",      sources = c("MATURITYDATE","MATURITYDAT"), position = 21, type = "date"),
    col_spec("eir",                sources = "EIR",              position = 23, type = "numeric", required = FALSE),
    col_spec("payment_frequency",  sources = c("PAYMENTFREQUENCY","PAYMENT_FRE"), position = 36, type = "integer", required = FALSE),
    col_spec("currency_code",      sources = c("CURRENCYCODE","CUR"), position = 37, type = "character", required = FALSE),
    col_spec("deferral_period",    sources = "DEFERRALPERIOD",   position = 38, type = "numeric", required = FALSE),
    col_spec("nominal_int_rate",   sources = "NOMINALINTERESTRATE", position = 39, type = "numeric", required = FALSE),
    col_spec("payment_type_id",    sources = "PAYMENTTYPEID",    position = 40, type = "integer", required = FALSE)
  ),

  # AccountMasterInvestments.xlsx — investment portfolio account-level
  # Headers are heavily truncated ("L","P","P","I","O" etc.) so positional
  # access is the rule rather than the exception.
  AccountMasterInvestments = list(
    col_spec("extract_date",   sources = "EXTRACTDA",     position = 1,  type = "date"),
    col_spec("account_id",     sources = "CONTRACTID",    position = 2,  type = "character"),
    col_spec("customer_id",    sources = "CUSTOMERID",    position = 4,  type = "character"),
    col_spec("portfolio_code", sources = "PORTFOLIOCODE", position = 5,  type = "character"),
    col_spec("account_type",   sources = "ACCOUNTTYPE",   position = 6,  type = "character"),
    col_spec("open_date",      sources = "OPENDATE",      position = 9,  type = "date"),
    col_spec("rating",         sources = "RATING",        position = 10, type = "character"),
    col_spec("past_due_days",  sources = "PASTDUEDAYS",   position = 11, type = "integer", required = FALSE),
    col_spec("on_balance",     sources = "ONBALANCE",     position = 18, type = "numeric"),
    col_spec("maturity_date",  sources = c("MATURITYDATE","MATURITYDAT"), position = 21, type = "date"),
    col_spec("eir",            sources = "EIR",           position = 23, type = "numeric", required = FALSE),
    col_spec("payment_frequency", sources = c("PAYMENTFREQUENCY","PAYMENT_FRE"), position = 36, type = "integer", required = FALSE),
    col_spec("currency_code",  sources = c("CURRENCYCODE","CUR"), position = 37, type = "character", required = FALSE),
    col_spec("nominal_int_rate", sources = "NOMINALINTERESTRATE", position = 39, type = "numeric", required = FALSE),
    col_spec("payment_type_id", sources = "PAYMENTTYPEID", position = 40, type = "integer", required = FALSE),
    col_spec("ccf",            sources = "CCF",           position = 20, type = "numeric", required = FALSE)
  ),

  # CustomerMaster.xlsx — clean named headers
  CustomerMaster = list(
    col_spec("extract_date",   sources = "EXTRACTDA",       position = 1, type = "date"),
    col_spec("customer_id",    sources = "CUSTOMERID",      position = 2, type = "character"),
    col_spec("portfolio_code", sources = "PORTFOLIOCODE",   position = 3, type = "character", required = FALSE),
    col_spec("customer_name",  sources = "CUSTOMERNAME",    position = 4, type = "character", required = FALSE),
    col_spec("customer_limit", sources = "CUSTOMERLIMIT",   position = 5, type = "numeric",   required = FALSE),
    col_spec("rating",         sources = "RATING",          position = 6, type = "character"),
    col_spec("past_due_days",  sources = c("PASTDUE_DAYS","PASTDUEDAYS"), position = 7, type = "integer", required = FALSE),
    col_spec("is_individual_assessment",
                               sources = "ISINDIVIDUALASSESSMENT", position = 10, type = "logical", required = FALSE)
  ),

  # CustomerMasterInvestments.xls — HTML SQL*Plus export
  # Only the columns we use; the file has many junk cols.
  CustomerMasterInvestments = list(
    col_spec("extract_date", sources = "EXTRACTDA",   position = 1, type = "date", required = FALSE),
    col_spec("customer_id",  sources = "CUSTOMERID",  position = 2, type = "character"),
    col_spec("rating",       sources = "RATING",      position = 6, type = "character", required = FALSE),
    col_spec("customer_name", sources = "CUSTOMERNAME", position = 4, type = "character", required = FALSE)
  ),

  # CustomerStagingFlag.xlsx — duplicate "I" headers but the named ones we
  # need (CUSTOMERID, ISWATCHLIST, ISLOCAL1) are unique.
  CustomerStagingFlag = list(
    col_spec("extract_date",  sources = "EXTRACTDA",   position = 1, type = "date"),
    col_spec("customer_id",   sources = "CUSTOMERID",  position = 2, type = "character"),
    col_spec("is_default",    sources = "ISDEFAULT",   position = 3, type = "integer", required = FALSE),
    col_spec("is_watchlist",  sources = "ISWATCHLIST", position = 4, type = "integer"),
    col_spec("is_insolvency", sources = "ISINSOLVENCY", position = 5, type = "integer", required = FALSE),
    col_spec("is_default_in_gcc", sources = "ISDEFAULTINGCC", position = 6, type = "integer", required = FALSE),
    col_spec("is_local1",     sources = "ISLOCAL1",   position = 7, type = "integer")
  ),

  # CustomerStagingFlagInvestments.xls — HTML; same structure as lending
  CustomerStagingFlagInvestments = list(
    col_spec("customer_id",   sources = "CUSTOMERID",  position = 2, type = "character"),
    col_spec("is_default",    sources = "ISDEFAULT",   position = 3, type = "integer", required = FALSE),
    col_spec("is_watchlist",  sources = "ISWATCHLIST", position = 4, type = "integer", required = FALSE),
    col_spec("is_local1",     sources = "ISLOCAL1",    position = 7, type = "integer", required = FALSE)
  ),

  # Collateral.xlsx — readxl misclassifies col C as <lgl>; col D ("CO") has
  # the same data and is parsed as <dbl>. Use col D.
  Collateral = list(
    col_spec("extract_date",       sources = "EXTRACTDA",       position = 1, type = "date"),
    col_spec("collateral_id",      sources = "COLLATERALID",    position = 2, type = "character"),
    col_spec("collateral_type_id", sources = "CO",              position = 4, type = "integer",
             description = "readxl parses raw col C ('P') as <lgl>; col D ('CO') has same data as <dbl>. Use col D."),
    col_spec("currency",           sources = "COL",             position = 5, type = "character"),
    col_spec("value",              sources = "COLLATERALVALUE", position = 6, type = "numeric")
  ),

  # AccountCollateralAllocation.xlsx — clean enough
  AccountCollateralAllocation = list(
    col_spec("extract_date",          sources = "EXTRACTDA",            position = 1, type = "date"),
    col_spec("collateral_id",         sources = "COLLATERALID",         position = 2, type = "character"),
    col_spec("contract_id",           sources = "CONTRACTID",           position = 3, type = "character"),
    col_spec("allocation_percentage", sources = "ALLOCATIONPERCENTAGE", position = 4, type = "numeric")
  ),

  # RepaymentSchedule.xlsx — drives EAD curve; column names are SQL aliases.
  RepaymentSchedule = list(
    col_spec("contract_id",   sources = "KEY_1",      position = 1, type = "character"),
    col_spec("post_date",     sources = "POST_DATE",  position = 2, type = "date"),
    col_spec("start_date",    sources = c("START_DATE","START_DAT"), position = 3, type = "date"),
    col_spec("principal_due", sources = "PRINCE_DUE", position = 4, type = "numeric"),
    col_spec("projected_interest", sources = "PROJ_INT", position = 5, type = "numeric"),
    col_spec("repayment",     sources = "REPAYMENT",  position = 6, type = "numeric"),
    col_spec("balance",       sources = "BALANCE",    position = 7, type = "numeric")
  ),

  # IndustryCode.xls — HTML; (customer_id, industry_code, industry_description)
  IndustryCode = list(
    col_spec("customer_id",      sources = "CUSTOMERID",  position = 1, type = "character"),
    col_spec("industry_code",    sources = "INDUSTRYCODE", position = 2, type = "character", required = FALSE),
    col_spec("industry_description", sources = "INDUSTRYDESCRIPTION", position = 3, type = "character", required = FALSE)
  ),

  # Origination.xls / OriginationInvestments.xls — HTML. Schema is the same.
  Origination = list(
    col_spec("contract_id",          sources = c("CONTRACTID","KEY_1"), position = 1, type = "character"),
    col_spec("origination_pd_12m",   sources = "PD12M",      position = 2, type = "numeric", required = FALSE),
    col_spec("origination_rating",   sources = "RATING",     position = 3, type = "character", required = FALSE),
    col_spec("origination_dpd",      sources = "PASTDUEDAYS", position = 4, type = "integer", required = FALSE),
    col_spec("origination_watchlist", sources = "ISWATCHLIST", position = 5, type = "integer", required = FALSE)
  ),

  OriginationInvestments = list(
    col_spec("contract_id",          sources = c("CONTRACTID","KEY_1"), position = 1, type = "character"),
    col_spec("origination_pd_12m",   sources = "PD12M",      position = 2, type = "numeric", required = FALSE),
    col_spec("origination_rating",   sources = "RATING",     position = 3, type = "character", required = FALSE)
  )
)
