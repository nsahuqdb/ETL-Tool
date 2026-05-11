# ============================================================================
# R/output_writers.R
#
# Writers for the 18 output CSVs that the downstream IFRS9 system expects.
# Files split into three groups:
#
#   A. Static config (6 files)          — one row of static data per portfolio,
#                                         rating, etc.; same content every run
#                                         except for the ExtractDate column.
#       FxRate.csv, Portfolios.csv, PortfolioRatingType.csv, RatingTypes.csv,
#       Ratings.csv, CollateralType.csv
#
#   B. Per-record (10 files)            — derived from input data via the
#                                         transformations in Phase B-D.
#       AccountMaster_1.csv, AccountMaster_2.csv,
#       CustomerMaster_1.csv, CustomerMaster_2.csv,
#       CustomerStagingFlag_1.csv, CustomerStagingFlag_2.csv,
#       Origination_1.csv, Origination_2.csv,
#       Collateral.csv, AccountCollateralAllocation.csv
#
#   C. Derived (2 files) [Phase F]      — the term-structure outputs.
#       LifeTimeParameterOther.csv, StPD.csv
#
# Output conventions (matched against bundled Output/*.csv):
#   - CRLF (\r\n) line endings.
#   - ExtractDate format: MM/DD/YYYY (e.g. "12/31/2025").
#   - Empty cells emitted as zero-length strings between commas.
#   - No quoting of any field.
#   - Decimal precision varies by file — see write_*() docstring for each.
# ============================================================================


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

#' Format an extract_date Date object as MM/DD/YYYY (Excel/Windows style).
format_extract_date <- function(d) {
  if (is.null(d) || all(is.na(d))) return("")
  format(as.Date(d), "%m/%d/%Y")
}


#' Format a Date column for output (MM/DD/YYYY).
format_date_col <- function(x) {
  ifelse(is.na(x), "", format(as.Date(x), "%m/%d/%Y"))
}


#' Write a tibble to CSV with CRLF line endings, no quoting, blank for NA.
#' Numeric columns must already be coerced to character if exact precision
#' matters (use formatC/sprintf upstream of this call).
#'
#' @param df          tibble. Column order is preserved.
#' @param path        output path
#' @param header      character vector of column names to write (must equal
#'                    ncol(df)). Allows non-snake_case headers in the file
#'                    while keeping snake_case in the tibble.
write_csv_crlf <- function(df, path, header = colnames(df)) {
  if (length(header) != ncol(df)) {
    stop(sprintf("write_csv_crlf: header length %d != ncol(df) %d",
                 length(header), ncol(df)))
  }

  # Format every column to character. NA (real or literal "NA"/"NaN") -> "".
  fmt <- function(col) {
    if (inherits(col, "Date")) {
      out <- format_date_col(col)
    } else if (is.logical(col)) {
      out <- ifelse(is.na(col), "",
                     ifelse(col, "TRUE", "FALSE"))
    } else {
      out <- as.character(col)
      out[is.na(out)] <- ""
    }
    # Scrub the literal strings "NA" and "NaN" — these can leak from R
    # conversions (paste(NA), as.character of factor levels named "NA",
    # etc.) and the bundled CSVs use "" for missing values.
    out[out == "NA"]  <- ""
    out[out == "NaN"] <- ""
    out
  }

  m <- vapply(df, fmt, character(nrow(df)))
  if (!is.matrix(m)) m <- matrix(m, nrow = nrow(df))
  colnames(m) <- header

  con <- file(path, open = "wb")
  on.exit(close(con))
  writeLines(paste(header, collapse = ","), con, sep = "\r\n")
  for (i in seq_len(nrow(m))) {
    writeLines(paste(m[i, ], collapse = ","), con, sep = "\r\n")
  }
  invisible(path)
}


#' Format a numeric vector with a fixed number of decimal places, NA -> "".
#' This is the workhorse for matching the exact decimal precision of bundled
#' CSVs (e.g. "1.00" not "1", "46041286.6000" not "46041286.6").
#'
#' Defensive against non-numeric inputs: coerces character/integer/list to
#' numeric (silently producing NAs for unparseable values) before formatting.
fmt_numeric <- function(x, decimals = NULL) {
  # Unlist first in case x is a list-column (e.g., from openxlsx with mixed types)
  if (is.list(x)) {
    x <- unlist(x, use.names = FALSE)
  }
  # Factor levels with numeric labels: convert via character first.
  # as.numeric(factor) would give level codes, not values.
  if (is.factor(x)) {
    x <- as.character(x)
  }
  # Coerce to numeric; non-numeric strings become NA
  x_num <- suppressWarnings(as.numeric(x))

  if (is.null(decimals)) {
    out <- format(x_num, scientific = FALSE, trim = TRUE)
  } else {
    out <- formatC(x_num, format = "f", digits = decimals)
  }
  out[is.na(x_num)] <- ""
  out
}


#' Convenience: build the output directory if needed and return a path
#' joiner closure.
ensure_output_dir <- function(output_dir) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  function(name) file.path(output_dir, name)
}


# ---------------------------------------------------------------------------
# Group A: 6 static config writers
# ---------------------------------------------------------------------------

#' Write FxRate.csv (2 rows: QAR=1, USD=3.645).
write_fx_rate <- function(static, run_cfg, output_dir) {
  d <- format_extract_date(run_cfg$run$extract_date)
  fx <- static$fx_rates
  out <- data.frame(
    ExtractDate  = rep(d, nrow(fx)),
    CurrencyCode = fx$currency_code,
    FXRate       = fmt_numeric(fx$fx_rate),
    stringsAsFactors = FALSE
  )
  path <- file.path(output_dir, "FxRate.csv")
  write_csv_crlf(out, path,
                  header = c("ExtractDate", "CurrencyCode", "FXRate"))
  path
}


#' Write Portfolios.csv (6 rows).
write_portfolios <- function(static, run_cfg, output_dir) {
  d <- format_extract_date(run_cfg$run$extract_date)
  p <- static$portfolios
  out <- data.frame(
    ExtractDate   = rep(d, nrow(p)),
    PortfolioCode = p$portfolio_code,
    Description   = p$description,
    stringsAsFactors = FALSE
  )
  path <- file.path(output_dir, "Portfolios.csv")
  write_csv_crlf(out, path,
                  header = c("ExtractDate", "PortfolioCode", "Description"))
  path
}


#' Write PortfolioRatingType.csv (one row per portfolio, mapping to RatingType
#' 1 (Internal) or 2 (External)).
write_portfolio_rating_type <- function(static, run_cfg, output_dir) {
  d <- format_extract_date(run_cfg$run$extract_date)
  p <- static$portfolios
  out <- data.frame(
    ExtractDate   = rep(d, nrow(p)),
    PortfolioCode = p$portfolio_code,
    RatingType    = as.integer(p$rating_type),
    stringsAsFactors = FALSE
  )
  path <- file.path(output_dir, "PortfolioRatingType.csv")
  write_csv_crlf(out, path,
                  header = c("ExtractDate", "PortfolioCode", "RatingType"))
  path
}


#' Write RatingTypes.csv (2 rows: 1=Internal Rating, 2=External Rating).
write_rating_types <- function(run_cfg, output_dir) {
  d <- format_extract_date(run_cfg$run$extract_date)
  out <- data.frame(
    ExtractDate = c(d, d),
    RatingType  = c(1L, 2L),
    Description = c("Internal Rating", "External Rating"),
    stringsAsFactors = FALSE
  )
  path <- file.path(output_dir, "RatingTypes.csv")
  write_csv_crlf(out, path,
                  header = c("ExtractDate", "RatingType", "Description"))
  path
}


#' Write Ratings.csv: 21 internal + 21 external = 42 rows.
#'
#' Description column convention (from bundled file):
#'   - Internal: "Desc<tier>" where tier is 1..9 derived from the rating
#'     name (e.g., "QDB 1+" → "Desc1", "QDB 7" → "Desc7").
#'   - External: "Desc<hierarchy>" where hierarchy is 1..21
#'     (e.g., Aaa → "Desc1", C → "Desc21").
#'
#' Hierarchy column is read directly from master_rating_scale (already 1..21
#' per rating_type).
write_ratings <- function(static, run_cfg, output_dir) {
  d   <- format_extract_date(run_cfg$run$extract_date)
  mrs <- static$master_rating_scale
  mrs <- mrs[order(match(mrs$rating_type, c("Internal","External")),
                    mrs$hierarchy), ]

  # Description per row
  desc <- character(nrow(mrs))
  for (i in seq_len(nrow(mrs))) {
    if (mrs$rating_type[i] == "Internal") {
      tier <- regmatches(mrs$rating[i], regexpr("[0-9]+", mrs$rating[i]))
      desc[i] <- paste0("Desc", tier)
    } else {
      desc[i] <- paste0("Desc", mrs$hierarchy[i])
    }
  }

  rating_type_int <- ifelse(mrs$rating_type == "Internal", 1L, 2L)

  out <- data.frame(
    ExtractDate = rep(d, nrow(mrs)),
    Rating      = mrs$rating,
    RatingType  = rating_type_int,
    Description = desc,
    Hierarchy   = as.integer(mrs$hierarchy),
    stringsAsFactors = FALSE
  )
  path <- file.path(output_dir, "Ratings.csv")
  write_csv_crlf(out, path,
                  header = c("ExtractDate", "Rating", "RatingType",
                             "Description", "Hierarchy"))
  path
}


#' Write CollateralType.csv (26 rows). All RealizationPeriod, Haircut12M,
#' Haircut24M are blank in the bundled file.
write_collateral_type <- function(static, run_cfg, output_dir) {
  d <- format_extract_date(run_cfg$run$extract_date)
  ct <- static$collateral_types
  out <- data.frame(
    ExtractDate            = rep(d, nrow(ct)),
    CollateralTypeId       = as.integer(ct$collateral_type_id),
    ExternalCollateralType = as.integer(ct$external_collateral_type),
    Description            = ct$description,
    HaircutGeneral         = fmt_numeric(ct$haircut_general, decimals = 2),
    Haircut12M             = fmt_numeric(ct$haircut_12m,     decimals = 2),
    Haircut24M             = fmt_numeric(ct$haircut_24m,     decimals = 2),
    RealizationPeriod      = fmt_numeric(ct$realization_period),
    stringsAsFactors = FALSE
  )
  path <- file.path(output_dir, "CollateralType.csv")
  write_csv_crlf(out, path,
                  header = c("ExtractDate", "CollateralTypeId",
                             "ExternalCollateralType", "Description",
                             "HaircutGeneral", "Haircut12M", "Haircut24M",
                             "RealizationPeriod"))
  path
}


#' Convenience: write all 6 static config CSVs and return a named character
#' vector of paths.
write_all_static_outputs <- function(static, run_cfg, output_dir) {
  ensure_output_dir(output_dir)
  c(
    FxRate              = write_fx_rate(static, run_cfg, output_dir),
    Portfolios          = write_portfolios(static, run_cfg, output_dir),
    PortfolioRatingType = write_portfolio_rating_type(static, run_cfg, output_dir),
    RatingTypes         = write_rating_types(run_cfg, output_dir),
    Ratings             = write_ratings(static, run_cfg, output_dir),
    CollateralType      = write_collateral_type(static, run_cfg, output_dir)
  )
}


# ---------------------------------------------------------------------------
# Group B: 10 per-record writers
# ---------------------------------------------------------------------------
#
# These take the Phase D-transformed tibble and (optionally) the raw load
# tibble for fields not currently captured in the transform. The raw tibble
# joins via contract_id_raw → CONTRACTID.
#
# Most fields in the bundled output are BLANK (Stage, PD12M, IsPOCI, LGDRate,
# Principal, etc.). We emit them blank too. Only the Phase D / raw fields
# are filled.
#
# Decimal precision is matched per-file as observed in bundled output:
#   AccountMaster_1: OnBalance free-form, EIR/NomInt 8 dp
#   AccountMaster_2: OnBalance/OffBalance 4 dp, EIR 10 dp, NomInt 5 dp
#   Collateral.csv:  CollateralValue 4 dp
#   AccountCollateralAllocation.csv: AllocationPercentage 4 dp
# ---------------------------------------------------------------------------


# Helper: 42-column AccountMaster row builder shared by _1 and _2.
.build_account_master_rows <- function(d, n,
                                          contract_id, customer_id,
                                          portfolio_code, account_type,
                                          open_date, rating, past_due_days,
                                          off_balance, on_balance,
                                          maturity_date, eir, payment_freq,
                                          currency_code, deferral_period,
                                          nominal_int_rate, payment_type_id,
                                          ccf,
                                          on_balance_dp = NULL,
                                          off_balance_dp = NULL,
                                          eir_dp = 8,
                                          nominal_int_rate_dp = 8) {
  data.frame(
    ExtractDate          = rep(d, n),
    ContractId           = as.character(contract_id),
    LimId                = rep("", n),
    CustomerId           = as.character(customer_id),
    PortfolioCode        = as.character(portfolio_code),
    AccountCode          = rep("", n),
    AccountType          = as.character(account_type),
    ImpairmentAmount     = rep("", n),
    OriginalECLOnbal     = rep("", n),
    OriginalECLOfbal     = rep("", n),
    # V4 AccountMasterLoad!K formula: IF(account_type = "Tasdeer", 2, "")
    Stage                = ifelse(!is.na(account_type) & account_type == "Tasdeer",
                                  "2", ""),
    OpenDate             = format_date_col(open_date),
    Rating               = as.character(rating),
    PastDueDays          = fmt_numeric(past_due_days),
    PD12M                = rep("", n),
    PDLifetimeValue      = rep("", n),
    IsPOCI               = rep("", n),
    LGDRate              = rep("", n),
    LoanToValue          = rep("", n),
    OffBalance           = fmt_numeric(off_balance, decimals = off_balance_dp),
    OnBalance            = fmt_numeric(on_balance,  decimals = on_balance_dp),
    EAD                  = rep("", n),
    CCF                  = fmt_numeric(ccf),
    MaturityDate         = format_date_col(maturity_date),
    ExpectedMaturityDate = rep("", n),
    EIR                  = fmt_numeric(eir, decimals = eir_dp),
    Principal            = rep("", n),
    PrincipalOverdue     = rep("", n),
    InterestAccrued      = rep("", n),
    InterestOverdue      = rep("", n),
    Fee                  = rep("", n),
    FeeOverdue           = rep("", n),
    Penalty              = rep("", n),
    PenaltyOverdue       = rep("", n),
    Commission           = rep("", n),
    CommissionOverdue    = rep("", n),
    Other                = rep("", n),
    OtherOverdue         = rep("", n),
    PaymentFrequency     = fmt_numeric(payment_freq),
    CurrencyCode         = as.character(currency_code),
    DeferralPeriod       = fmt_numeric(deferral_period),
    NominalInterestRate  = fmt_numeric(nominal_int_rate, decimals = nominal_int_rate_dp),
    PaymentTypeId        = fmt_numeric(payment_type_id),
    stringsAsFactors = FALSE
  )
}


.account_master_header <- c(
  "ExtractDate", "ContractId", "LimId", "CustomerId", "PortfolioCode",
  "AccountCode", "AccountType", "ImpairmentAmount", "OriginalECLOnbal",
  "OriginalECLOfbal", "Stage", "OpenDate", "Rating", "PastDueDays",
  "PD12M", "PDLifetimeValue", "IsPOCI", "LGDRate", "LoanToValue",
  "OffBalance", "OnBalance", "EAD", "CCF", "MaturityDate",
  "ExpectedMaturityDate", "EIR", "Principal", "PrincipalOverdue",
  "InterestAccrued", "InterestOverdue", "Fee", "FeeOverdue", "Penalty",
  "PenaltyOverdue", "Commission", "CommissionOverdue", "Other",
  "OtherOverdue", "PaymentFrequency", "CurrencyCode", "DeferralPeriod",
  "NominalInterestRate", "PaymentTypeId"
)


#' Write AccountMaster_1.csv (lending).
#'
#' @param lending_accounts  output of transform_lending() (one row per contract).
#' @param raw_account_master raw_load$account_master tibble — used to pull
#'        OnBalance, OffBalance, CCF, PaymentTypeId by joining on
#'        contract_id_raw → CONTRACTID.
#' @param static       static reference (for product_portfolio_mapping).
write_account_master_1 <- function(lending_accounts, raw_account_master,
                                       run_cfg, output_dir, static) {
  d <- format_extract_date(run_cfg$run$extract_date)
  la <- lending_accounts

  # Join raw fields by raw contract id. Schema canonical column names
  # (lowercase snake_case): contract_id, on_balance, off_balance, ccf.
  # Note: PaymentTypeId is computed in transform_lending (3 if PaymentFrequency
  # is empty, else 4 — matches V4 Transformation!BF formula). We don't pull
  # it from the raw input.
  raw <- raw_account_master[, c("contract_id", "on_balance", "off_balance",
                                "ccf")]
  names(raw) <- c("contract_id_raw", "on_balance", "off_balance", "ccf")
  joined <- merge(la, raw, by = "contract_id_raw",
                   all.x = TRUE, sort = FALSE)
  # Restore original order
  joined <- joined[match(la$contract_id_raw, joined$contract_id_raw), ]

  # PortfolioCode = VLOOKUP(account_type, product_portfolio_mapping)
  # default "Business Finance" if not found (per V4 AccountMasterLoad!E
  # IFERROR fallback).
  ppm <- static$product_portfolio_mapping
  portfolio_lkp <- if (!is.null(ppm) && nrow(ppm) > 0) {
    setNames(as.character(ppm$portfolio), as.character(ppm$product_type))
  } else {
    character(0)
  }
  portfolio_code <- portfolio_lkp[as.character(joined$account_type)]
  portfolio_code[is.na(portfolio_code) | !nzchar(portfolio_code)] <-
    "Business Finance"

  out <- .build_account_master_rows(
    d = d, n = nrow(joined),
    contract_id      = joined$contract_id,
    customer_id      = joined$customer_id,
    portfolio_code   = portfolio_code,
    account_type     = joined$account_type,
    open_date        = joined$open_date,
    # AccountMaster_1.Rating maps to Transformation!AE which is the
    # per-customer worst Internal label (after override). This is
    # `rating_worst` from transform_lending, not `rating` (which is the
    # per-contract Z step).
    rating           = joined$rating_worst,
    # AccountMaster_1.PastDueDays maps to V4 AccountMasterLoad!N which
    # references Transformation!AG = MAX(IF customer_id matches, AF) —
    # the per-customer worst DPD across contracts. Use past_dues_worst,
    # not past_dues_days (per-contract).
    past_due_days    = joined$past_dues_worst,
    off_balance      = as.numeric(joined$off_balance),
    on_balance       = as.numeric(joined$on_balance),
    maturity_date    = joined$maturity_date,
    eir              = joined$eir,
    payment_freq     = joined$payment_frequency_months,
    currency_code    = joined$currency,
    deferral_period  = joined$deferral_period,
    nominal_int_rate = joined$nominal_int_rate,
    payment_type_id  = joined$payment_type,
    ccf              = joined$ccf
  )

  path <- file.path(output_dir, "AccountMaster_1.csv")
  write_csv_crlf(out, path, header = .account_master_header)
  path
}


#' Write AccountMaster_2.csv (investment).
#'
#' @param investment_accounts output of transform_investments().
#' @param raw_investment_master raw_load$investment_master tibble.
write_account_master_2 <- function(investment_accounts, raw_investment_master,
                                       run_cfg, output_dir) {
  d <- format_extract_date(run_cfg$run$extract_date)
  ia <- investment_accounts

  # Investments use AccountId as ContractId. Most rate/fee fields aren't in
  # the investment input — bundled output uses 0.0420000000 (10 dp) etc. so
  # a literal pass-through with empty for missing.
  raw <- raw_investment_master
  # Schema canonical columns: on_balance, eir, nominal_int_rate,
  # payment_type_id, ccf, payment_frequency, currency_code.
  # AccountMasterInvestments doesn't have an off_balance column — default 0.

  on_balance      <- if ("on_balance"        %in% names(raw)) raw$on_balance        else NA_real_
  off_balance     <- rep(0, nrow(raw))
  eir             <- if ("eir"               %in% names(raw)) raw$eir               else NA_real_
  nominal_rate    <- if ("nominal_int_rate"  %in% names(raw)) raw$nominal_int_rate  else NA_real_

  # Bundle convention: EIR / NominalInterestRate are decimal fractions
  # (0.042) not whole-percent (4.2). Auto-detect: if any value > 1, the
  # column was stored in percent → divide by 100.
  eir_n <- suppressWarnings(as.numeric(eir))
  if (any(!is.na(eir_n) & eir_n > 1)) eir_n <- eir_n / 100
  eir <- eir_n
  nom_n <- suppressWarnings(as.numeric(nominal_rate))
  if (any(!is.na(nom_n) & nom_n > 1)) nom_n <- nom_n / 100
  nominal_rate <- nom_n
  payment_type_id <- if ("payment_type_id"  %in% names(raw)) raw$payment_type_id  else NA_integer_
  ccf             <- if ("ccf"              %in% names(raw)) raw$ccf              else 0
  payment_freq    <- if ("payment_frequency" %in% names(raw)) raw$payment_frequency else NA_integer_
  currency        <- if ("currency_code"    %in% names(raw)) raw$currency_code    else "QAR"
  deferral        <- rep(0, nrow(raw))

  # PortfolioCode rule (V4 AccountMasterInvLoad!E2):
  #   IF(account_type = "Banks and Fis", "Banks and Fis", "Investments")
  inv_portfolio_code <- ifelse(
    !is.na(ia$account_type) & ia$account_type == "Banks and Fis",
    "Banks and Fis", "Investments")

  # Investments AccountMaster_2 uses 4dp on OnBalance/OffBalance, 10dp on EIR,
  # 5dp on NominalInterestRate.
  out <- .build_account_master_rows(
    d = d, n = nrow(ia),
    contract_id      = ia$account_id,
    customer_id      = ia$customer_id_inv,
    portfolio_code   = inv_portfolio_code,
    account_type     = ia$account_type,
    open_date        = ia$open_date,
    rating           = ia$rating_current,
    past_due_days    = rep(0, nrow(ia)),
    off_balance      = as.numeric(off_balance),
    on_balance       = as.numeric(on_balance),
    maturity_date    = ia$maturity_date,
    eir              = as.numeric(eir),
    payment_freq     = payment_freq,
    currency_code    = currency,
    deferral_period  = deferral,
    nominal_int_rate = as.numeric(nominal_rate),
    payment_type_id  = payment_type_id,
    ccf              = ccf,
    on_balance_dp       = 4,
    off_balance_dp      = 4,
    eir_dp              = 10,
    nominal_int_rate_dp = 5
  )

  path <- file.path(output_dir, "AccountMaster_2.csv")
  write_csv_crlf(out, path, header = .account_master_header)
  path
}


#' Write CustomerMaster_1.csv (lending). All non-CustomerId fields are blank
#' in the bundled output except IsIndividualAssessment which is "FALSE".
#'
#' @param customers  Phase D customer-level tibble (column: customer_id).
write_customer_master_1 <- function(customers, run_cfg, output_dir) {
  d <- format_extract_date(run_cfg$run$extract_date)
  n <- nrow(customers)
  out <- data.frame(
    ExtractDate            = rep(d, n),
    CustomerId             = as.character(customers$customer_id),
    PortfolioCode          = rep("", n),
    OrganizationalUnitCode = rep("", n),
    CustomerCode           = rep("", n),
    CustomerName           = rep("", n),
    CustomerLimit          = rep("", n),
    Rating                 = rep("", n),
    PastDueDays            = rep("", n),
    PD12M                  = rep("", n),
    PDLifetimeValue        = rep("", n),
    IsIndividualAssessment = rep("FALSE", n),
    stringsAsFactors = FALSE
  )
  path <- file.path(output_dir, "CustomerMaster_1.csv")
  write_csv_crlf(out, path,
                  header = c("ExtractDate", "CustomerId", "PortfolioCode",
                             "OrganizationalUnitCode", "CustomerCode",
                             "CustomerName", "CustomerLimit", "Rating",
                             "PastDueDays", "PD12M", "PDLifetimeValue",
                             "IsIndividualAssessment"))
  path
}


#' Write CustomerMaster_2.csv (investment). Same as _1 but with
#' BusinessUnitCode header (instead of OrganizationalUnitCode).
write_customer_master_2 <- function(investment_customers, run_cfg, output_dir) {
  d <- format_extract_date(run_cfg$run$extract_date)
  n <- nrow(investment_customers)
  out <- data.frame(
    ExtractDate            = rep(d, n),
    CustomerId             = as.character(investment_customers$customer_id_inv),
    PortfolioCode          = rep("", n),
    BusinessUnitCode       = rep("", n),
    CustomerCode           = rep("", n),
    CustomerName           = rep("", n),
    CustomerLimit          = rep("", n),
    Rating                 = rep("", n),
    PastDueDays            = rep("", n),
    PD12M                  = rep("", n),
    PDLifetimeValue        = rep("", n),
    IsIndividualAssessment = rep("FALSE", n),
    stringsAsFactors = FALSE
  )
  path <- file.path(output_dir, "CustomerMaster_2.csv")
  write_csv_crlf(out, path,
                  header = c("ExtractDate", "CustomerId", "PortfolioCode",
                             "BusinessUnitCode", "CustomerCode",
                             "CustomerName", "CustomerLimit", "Rating",
                             "PastDueDays", "PD12M", "PDLifetimeValue",
                             "IsIndividualAssessment"))
  path
}


#' Build a CustomerStagingFlag row (lending or investment).
#'
#' Bundled file: most cells blank except IsDefault and IsLocal1 which are
#' usually FALSE, and IsWatchlist sometimes TRUE. We map from Phase D's
#' is_default / is_watchlist flags. IsLocal1..6 are not currently captured
#' so emit blank, which matches many bundled rows.
.build_staging_flag_rows <- function(d, n, customer_id,
                                       is_default, is_watchlist,
                                       is_insolvency = NA, is_default_in_gcc = NA,
                                       is_local1 = NA, is_local2 = NA,
                                       is_local3 = NA, is_local4 = NA,
                                       is_local5 = NA, is_local6 = NA) {
  bool_chr <- function(v) ifelse(is.na(v), "", ifelse(as.logical(v), "TRUE", "FALSE"))
  data.frame(
    ExtractDate     = rep(d, n),
    CustomerId      = as.character(customer_id),
    IsDefault       = bool_chr(is_default),
    IsWatchlist     = bool_chr(is_watchlist),
    IsInsolvency    = bool_chr(is_insolvency),
    IsDefaultInGCC  = bool_chr(is_default_in_gcc),
    IsLocal1        = bool_chr(is_local1),
    IsLocal2        = bool_chr(is_local2),
    IsLocal3        = bool_chr(is_local3),
    IsLocal4        = bool_chr(is_local4),
    IsLocal5        = bool_chr(is_local5),
    IsLocal6        = bool_chr(is_local6),
    stringsAsFactors = FALSE
  )
}


.staging_flag_header <- c("ExtractDate", "CustomerId", "IsDefault",
                            "IsWatchlist", "IsInsolvency", "IsDefaultInGCC",
                            "IsLocal1", "IsLocal2", "IsLocal3", "IsLocal4",
                            "IsLocal5", "IsLocal6")


#' Write CustomerStagingFlag_1.csv (lending).
#'
#' @param customer_flags  one-row-per-customer tibble with columns at least
#'        (customer_id, is_default, is_watchlist).
write_customer_staging_flag_1 <- function(customer_flags, run_cfg, output_dir) {
  d <- format_extract_date(run_cfg$run$extract_date)
  cf <- customer_flags
  # V4 CustomerStagingFlagLoad column mapping (decoded from sheet11):
  #   IsDefault       -> per-customer worst DPD > 90 (V4 BL formula)
  #                      Sourced via cf$is_default (which is DPD>90 from
  #                      lending_portfolio_view's stage_final = Stage 3).
  #   IsWatchlist     -> input col D
  #   IsInsolvency    -> blank in bundle for all rows
  #   IsDefaultInGCC  -> blank in bundle for all rows
  #   IsLocal1        -> "Restructured" override flag (V4 BT,
  #                      VLOOKUP Inputs_Lending Portfolio col J/10)
  #   IsLocal2        -> always FALSE in bundle
  #   IsLocal3        -> "Stage 2" override flag (V4 BW,
  #                      VLOOKUP Inputs_Lending Portfolio col P/16)
  #                      We don't have V4's per-customer Stage 2 override
  #                      sheet, so we approximate with the COMPUTED
  #                      stage_final = "Stage 2" indicator. Wired through
  #                      cf$is_local3 from the test runner / orchestrator.
  #   IsLocal4..6     -> blank
  out <- .build_staging_flag_rows(
    d = d, n = nrow(cf),
    customer_id       = cf$customer_id,
    is_default        = cf$is_default,
    is_watchlist      = cf$is_watchlist,
    is_insolvency     = rep(NA, nrow(cf)),
    is_default_in_gcc = rep(NA, nrow(cf)),
    is_local1         = cf$is_local1,
    is_local2         = rep(FALSE, nrow(cf)),
    is_local3         = if (!is.null(cf$is_local3)) cf$is_local3 else cf$is_local1,
    is_local4         = rep(NA, nrow(cf)),
    is_local5         = rep(NA, nrow(cf)),
    is_local6         = rep(NA, nrow(cf))
  )
  path <- file.path(output_dir, "CustomerStagingFlag_1.csv")
  write_csv_crlf(out, path, header = .staging_flag_header)
  path
}


#' Write CustomerStagingFlag_2.csv (investment).
write_customer_staging_flag_2 <- function(investment_customers, run_cfg, output_dir) {
  d  <- format_extract_date(run_cfg$run$extract_date)
  ic <- investment_customers
  n  <- nrow(ic)
  out <- .build_staging_flag_rows(
    d = d, n = n,
    customer_id   = ic$customer_id_inv,
    is_default    = rep(FALSE, n),
    is_watchlist  = rep(NA, n),
    is_local1     = rep(FALSE, n)
  )
  path <- file.path(output_dir, "CustomerStagingFlag_2.csv")
  write_csv_crlf(out, path, header = .staging_flag_header)
  path
}


#' Origination headers — note EXTRACTDATE is uppercase here (V4 quirk).
.origination_header <- c("EXTRACTDATE", "ContractId", "OriginationPD12M",
                           "OriginationRating", "IsOriginationPastDueDaysStage2",
                           "IsOriginationIsWatchlistStage2",
                           "IsOriginationIsInsolvencyStage2",
                           "IsOriginationIsDefaultInGCCStage2",
                           "IsOriginationIsLocal1Stage2",
                           "IsOriginationIsLocal2Stage2",
                           "IsOriginationIsLocal3Stage2",
                           "IsOriginationIsLocal4Stage2",
                           "IsOriginationIsLocal5Stage2",
                           "IsOriginationIsLocal6Stage2")


#' Build Origination rows. Bundled file: all flags BLANK in every row.
.build_origination_rows <- function(d, contract_id) {
  n <- length(contract_id)
  data.frame(
    EXTRACTDATE                       = rep(d, n),
    ContractId                        = as.character(contract_id),
    OriginationPD12M                  = rep("", n),
    OriginationRating                 = rep("", n),
    IsOriginationPastDueDaysStage2    = rep("", n),
    IsOriginationIsWatchlistStage2    = rep("", n),
    IsOriginationIsInsolvencyStage2   = rep("", n),
    IsOriginationIsDefaultInGCCStage2 = rep("", n),
    IsOriginationIsLocal1Stage2       = rep("", n),
    IsOriginationIsLocal2Stage2       = rep("", n),
    IsOriginationIsLocal3Stage2       = rep("", n),
    IsOriginationIsLocal4Stage2       = rep("", n),
    IsOriginationIsLocal5Stage2       = rep("", n),
    IsOriginationIsLocal6Stage2       = rep("", n),
    stringsAsFactors = FALSE
  )
}


#' Write Origination_1.csv (lending) — one row per contract.
write_origination_1 <- function(lending_accounts, run_cfg, output_dir) {
  d <- format_extract_date(run_cfg$run$extract_date)
  out <- .build_origination_rows(d, lending_accounts$contract_id)
  path <- file.path(output_dir, "Origination_1.csv")
  write_csv_crlf(out, path, header = .origination_header)
  path
}


#' Write Origination_2.csv (investment) — one row per investment account.
write_origination_2 <- function(investment_accounts, run_cfg, output_dir) {
  d <- format_extract_date(run_cfg$run$extract_date)
  out <- .build_origination_rows(d, investment_accounts$account_id)
  path <- file.path(output_dir, "Origination_2.csv")
  write_csv_crlf(out, path, header = .origination_header)
  path
}


#' Write Collateral.csv. 7 cols — CollateralValue 4 dp.
#'
#' @param collateral  Phase B/D collateral tibble with cols at least
#'        (collateral_id, collateral_type_id, currency, value).
write_collateral <- function(collateral, run_cfg, output_dir) {
  d <- format_extract_date(run_cfg$run$extract_date)
  n <- nrow(collateral)
  out <- data.frame(
    ExtractDate         = rep(d, n),
    CollateralId        = as.character(collateral$collateral_id),
    ParentCollateralId  = rep("", n),
    CollateralCode      = rep("", n),
    CollateralTypeId    = fmt_numeric(as.integer(collateral$collateral_type_id)),
    CollateralCurrency  = as.character(collateral$currency),
    CollateralValue     = fmt_numeric(as.numeric(collateral$value), decimals = 4),
    stringsAsFactors = FALSE
  )
  path <- file.path(output_dir, "Collateral.csv")
  write_csv_crlf(out, path,
                  header = c("ExtractDate", "CollateralId",
                             "ParentCollateralId", "CollateralCode",
                             "CollateralTypeId", "CollateralCurrency",
                             "CollateralValue"))
  path
}


#' Write AccountCollateralAllocation.csv. AllocationPercentage 4 dp.
#'
#' NB: bundled file is truncated at 65,534 rows by Excel. Our output may
#' have more rows; we emit them all.
#'
#' @param account_collateral_allocation  tibble with cols
#'        (collateral_id, contract_id, allocation_percentage).
write_account_collateral_allocation <- function(account_collateral_allocation,
                                                  run_cfg, output_dir) {
  d <- format_extract_date(run_cfg$run$extract_date)
  aca <- account_collateral_allocation
  n <- nrow(aca)

  # Bundle convention: AllocationPercentage is a decimal fraction (0.57)
  # not a whole-percent number (57). Detect the input scale and divide
  # by 100 if values look like percent (any value > 1).
  pct <- as.numeric(aca$allocation_percentage)
  if (any(!is.na(pct) & pct > 1)) {
    pct <- pct / 100
  }

  out <- data.frame(
    ExtractDate          = rep(d, n),
    CollateralId         = as.character(aca$collateral_id),
    ContractId           = as.character(aca$contract_id),
    AllocationPercentage = fmt_numeric(pct, decimals = 4),
    stringsAsFactors = FALSE
  )
  path <- file.path(output_dir, "AccountCollateralAllocation.csv")
  write_csv_crlf(out, path,
                  header = c("ExtractDate", "CollateralId", "ContractId",
                             "AllocationPercentage"))
  path
}


# ---------------------------------------------------------------------------
# Group C: 2 derived writers (Phase F outputs)
# ---------------------------------------------------------------------------


#' Write LifeTimeParameterOther.csv.
#'
#' Bundled file columns (7): ExtractDate, ContractId, MonthLifetime,
#' EADLifetime, LGDLifetime, PaymentScheduleLifetime, TotalLimitLifetime.
#' LGDLifetime, PaymentScheduleLifetime, TotalLimitLifetime are blank in the
#' bundled file (placeholder columns).
#'
#' @param lifetime_parameter_other Phase F output tibble with columns
#'   (extract_date, contract_id, month_lifetime, ead_lifetime, lgd_lifetime,
#'    payment_schedule_lifetime, total_limit_lifetime).
write_lifetime_parameter_other <- function(lifetime_parameter_other, run_cfg, output_dir) {
  lpo <- lifetime_parameter_other
  d <- format_extract_date(run_cfg$run$extract_date)
  n <- nrow(lpo)

  out <- data.frame(
    ExtractDate             = rep(d, n),
    ContractId              = as.character(lpo$contract_id),
    MonthLifetime           = fmt_numeric(as.integer(lpo$month_lifetime)),
    EADLifetime             = fmt_numeric(as.numeric(lpo$ead_lifetime), decimals = 4),
    LGDLifetime             = fmt_numeric(lpo$lgd_lifetime, decimals = 4),
    PaymentScheduleLifetime = fmt_numeric(lpo$payment_schedule_lifetime, decimals = 4),
    TotalLimitLifetime      = fmt_numeric(lpo$total_limit_lifetime, decimals = 4),
    stringsAsFactors = FALSE
  )
  path <- file.path(output_dir, "LifeTimeParameterOther.csv")
  write_csv_crlf(out, path,
                  header = c("ExtractDate", "ContractId", "MonthLifetime",
                             "EADLifetime", "LGDLifetime",
                             "PaymentScheduleLifetime", "TotalLimitLifetime"))
  path
}


#' Write StPD.csv.
#'
#' Bundled file columns: ExtractDate, PortfolioCode, PDBucketDim1,
#' PDBucketDim2 (always blank), MonthLifetime, PDMarginal, PDCumulative.
#' Phase F's stpd tibble has pd_lifetime (cumulative). Marginal at month m
#' is pd_lifetime[m] - pd_lifetime[m-1] within each (portfolio, hierarchy).
write_stpd <- function(stpd, run_cfg, output_dir) {
  d <- format_extract_date(run_cfg$run$extract_date)

  stpd <- stpd[order(stpd$portfolio_code, stpd$pd_bucket_dim1,
                      stpd$month_lifetime), ]

  out <- data.frame(
    ExtractDate   = rep(d, nrow(stpd)),
    PortfolioCode = as.character(stpd$portfolio_code),
    PDBucketDim1  = fmt_numeric(as.integer(stpd$pd_bucket_dim1)),
    PDBucketDim2  = rep("", nrow(stpd)),
    MonthLifetime = fmt_numeric(as.integer(stpd$month_lifetime)),
    PDLifetime    = fmt_numeric(as.numeric(stpd$pd_lifetime), decimals = 16),
    stringsAsFactors = FALSE
  )
  path <- file.path(output_dir, "StPD.csv")
  write_csv_crlf(out, path,
                  header = c("ExtractDate", "PortfolioCode", "PDBucketDim1",
                             "PDBucketDim2", "MonthLifetime", "PDLifetime"))
  path
}


# ---------------------------------------------------------------------------
# Master orchestrator
# ---------------------------------------------------------------------------

#' Write ALL 18 output CSVs to output_dir, returning a named character vector
#' of paths. Components:
#'
#'   inputs:
#'     static                       — output of load_static()
#'     run_cfg                      — output of load_run_config()
#'     lending_accounts             — Phase D transform_lending() output
#'     investment_accounts          — Phase D transform_investments() output
#'     customers                    — Phase D customer-level lending tibble
#'     investment_customers         — Phase D customer-level investment tibble
#'     customer_flags               — Phase D customer staging flag tibble
#'     collateral                   — Phase B/D collateral tibble
#'     account_collateral_allocation — Phase B/D allocation tibble
#'     raw_account_master           — raw load tibble
#'     raw_investment_master        — raw load tibble
#'     lifetime_parameter_other     — Phase F output
#'     stpd                         — Phase F output
write_all_outputs <- function(static, run_cfg,
                                lending_accounts, investment_accounts,
                                customers, investment_customers,
                                customer_flags,
                                collateral, account_collateral_allocation,
                                raw_account_master, raw_investment_master,
                                lifetime_parameter_other, stpd,
                                output_dir) {
  ensure_output_dir(output_dir)
  paths <- write_all_static_outputs(static, run_cfg, output_dir)
  c(
    paths,
    AccountMaster_1             = write_account_master_1(lending_accounts, raw_account_master, run_cfg, output_dir, static),
    AccountMaster_2             = write_account_master_2(investment_accounts, raw_investment_master, run_cfg, output_dir),
    CustomerMaster_1            = write_customer_master_1(customers, run_cfg, output_dir),
    CustomerMaster_2            = write_customer_master_2(investment_customers, run_cfg, output_dir),
    CustomerStagingFlag_1       = write_customer_staging_flag_1(customer_flags, run_cfg, output_dir),
    CustomerStagingFlag_2       = write_customer_staging_flag_2(investment_customers, run_cfg, output_dir),
    Origination_1               = write_origination_1(lending_accounts, run_cfg, output_dir),
    Origination_2               = write_origination_2(investment_accounts, run_cfg, output_dir),
    Collateral                  = write_collateral(collateral, run_cfg, output_dir),
    AccountCollateralAllocation = write_account_collateral_allocation(account_collateral_allocation, run_cfg, output_dir),
    LifeTimeParameterOther      = write_lifetime_parameter_other(lifetime_parameter_other, run_cfg, output_dir),
    StPD                        = write_stpd(stpd, run_cfg, output_dir)
  )
}
