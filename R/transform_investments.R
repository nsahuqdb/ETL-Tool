# ============================================================================
# R/transform_investments.R
#
# Build the per-account Investment Portfolio transformation.
#
# Mirrors Transformation columns CA..CJ (the Investment block) +
# Inputs_Investment Portfolio columns A..K. Structurally simpler than
# the lending side: per-account (NOT per-customer), no DPD-based staging,
# no restructuring/watchlist concept, single-segment fallback rating.
#
# Excel sheets being replicated:
#   - Transformation columns CA..CJ  (one row per investment account)
#   - Inputs_Investment Portfolio    (one row per investment account)
#
# Public API:
#   build_transformation_investments(inputs, static, model_cfg, run_cfg)
#   build_investment_portfolio_view(transformation_inv, inputs, static,
#                                    rating_overrides = NULL,
#                                    stage_overrides = NULL,
#                                    apply_one_notch_downgrade = FALSE)
#
# Mapping doc: doc/transformation_mapping.md (Investment section).
# ============================================================================


# ---------------------------------------------------------------------------
# Investment rating chain (Transformation!CD/CE)
# ---------------------------------------------------------------------------

#' Derive the per-account investment rating.
#'
#' Excel formula (Transformation!CE):
#'   IF AccountID empty -> ""
#'   ELSE IF AccountMasterInv!RATING is in MasterRatingScale!J16:J45 (the
#'        external rating subset for investments):
#'     use AccountMasterInv!RATING
#'   ELSE: use Inputs_Investment Portfolio!N5 (segment fallback "Baa3")
#'
#' Also note Excel CD: =IF(MATCH(rating, MasterRatingScale!J16:J45) is NA, "", rating)
#' i.e. only ratings in the SUBSET J16:J45 count. The full MasterRatingScale!J
#' column has all 42 downgrade-source ratings (internal + external); the
#' J16:J45 slice is the EXTERNAL ones only.
#'
#' Implementation: we treat any rating in master_rating_scale where rating_type
#' = 'External' as a valid investment external rating.
#'
#' @param raw_rating       chr  rating from AccountMasterInvestments column J
#' @param external_ratings chr  vector of valid external ratings
#' @param fallback_rating  chr  scalar — the investment-segment fallback (e.g. "Baa3")
#' @return chr vector
derive_rating_investment <- function(raw_rating, external_ratings, fallback_rating) {
  out <- raw_rating
  in_external <- !is.na(raw_rating) & raw_rating %in% external_ratings
  out[!in_external] <- fallback_rating
  out
}


# ---------------------------------------------------------------------------
# Build Transformation (investment block)
# ---------------------------------------------------------------------------

#' Build the per-account investment Transformation table.
#'
#' Output columns mirror Transformation!CA..CJ:
#'   account_id            CA - investment account ID
#'   customer_id_inv       CC - customer ID associated with the account
#'   rating_external_raw   CD - rating from input file, if in external scale
#'   rating_current        CE - rating used (with fallback applied)
#'   rating_at_origination CH - rating from OriginationInvestments file
#'   rating_hierarchy      CG - hierarchy of rating_current  (external ranks)
#'   rating_origination_hierarchy CI - hierarchy of rating_at_origination,
#'                              defaults to rating_hierarchy if origination
#'                              rating is missing
#'   account_type          investment AccountType (e.g. "Banks and Fis")
#'   exposure_amount       on-balance exposure
#'   maturity_date         with the same maturity-extension rule as lending
#'
#' Override-dependent columns (CF, CJ) are filled in
#' build_investment_portfolio_view() at Pass 6.
#'
#' Note: the external rating hierarchy comes from the *external_equivalent*
#' column of master_rating_scale. The Excel formula CG2:
#'   =VLOOKUP(rating, MasterRatingScale!C3:D24, 2, FALSE)
#' uses col C (external_equivalent) of the INTERNAL block to map a Moody's
#' external rating to a hierarchy 1..21. We replicate that here.
#'
#' @param inputs    output of read_all_inputs()
#' @param static    output of load_static_reference()
#' @param model_cfg output of load_model_config()
#' @param run_cfg   output of load_run_config()
#' @return Tibble — one row per investment account
build_transformation_investments <- function(inputs, static, model_cfg, run_cfg) {
  ami  <- inputs$AccountMasterInvestments
  cmi  <- inputs$CustomerMasterInvestments
  oi   <- inputs$OriginationInvestments

  if (is.null(ami) || nrow(ami) == 0) {
    # Investment book may legitimately be empty — return an empty tibble
    # with the right schema so downstream code still works.
    return(tibble::tibble(
      extract_date            = as.Date(character()),
      account_id              = character(),
      customer_id_inv         = character(),
      account_type            = character(),
      open_date               = as.Date(character()),
      raw_rating              = character(),
      rating_external_raw     = character(),
      rating_current          = character(),
      rating_at_origination   = character(),
      rating_hierarchy        = integer(),
      rating_origination_hierarchy = integer(),
      exposure_amount         = double(),
      maturity_date           = as.Date(character()),
      rating_after_override   = character(),
      rating_with_downgrade   = character()
    ))
  }

  # ---- Schema canonical column access -----------------------------------
  # AccountMasterInvestments schema canonical cols: extract_date,
  # account_id, customer_id, account_type, open_date, rating, on_balance,
  # maturity_date, eir, payment_frequency, currency_code, nominal_int_rate,
  # payment_type_id, ccf.
  extract_date <- normalise_extract_date(ami$extract_date)
  account_id   <- as.character(ami$account_id)
  customer_id  <- as.character(ami$customer_id)
  account_type <- as.character(ami$account_type)
  open_date    <- normalise_extract_date(ami$open_date)
  raw_rating   <- as.character(ami$rating)

  exposure_amount <- suppressWarnings(as.numeric(ami$on_balance))
  exposure_amount[is.na(exposure_amount)] <- 0

  raw_maturity <- normalise_extract_date(ami$maturity_date)
  reporting_dt <- if (length(extract_date) > 0) max(extract_date, na.rm = TRUE) else as.Date(NA)
  ext_days <- as.integer(static$staging_thresholds$maturity_extension_days)
  maturity_date <- raw_maturity
  needs_ext <- !is.na(raw_maturity) & !is.na(reporting_dt) &
               raw_maturity < reporting_dt
  maturity_date[needs_ext] <- reporting_dt + ext_days

  # ---- Investment rating chain ------------------------------------------
  ext_scale <- static$master_rating_scale$rating[
    static$master_rating_scale$rating_type == "External"
  ]
  rating_external_raw <- ifelse(raw_rating %in% ext_scale,
                                raw_rating, NA_character_)

  inv_fallback <- static$segment_fallback_ratings$fallback_rating[
    static$segment_fallback_ratings$segment == "Investment Portfolio"
  ]
  if (length(inv_fallback) == 0 || is.na(inv_fallback)) {
    stop("segment_fallback_ratings.csv is missing 'Investment Portfolio' row")
  }
  rating_current <- derive_rating_investment(
    raw_rating       = raw_rating,
    external_ratings = ext_scale,
    fallback_rating  = inv_fallback
  )

  # ---- Origination rating ------------------------------------------------
  # OriginationInvestments schema canonical cols: contract_id, origination_rating
  if (!is.null(oi) && nrow(oi) > 0) {
    oi_account <- as.character(oi$contract_id)
    oi_rating  <- as.character(oi$origination_rating)
    oi_rating[oi_rating == "0" | oi_rating == "" | is.na(oi_rating)] <- NA_character_
    rating_at_origination <- oi_rating[match(account_id, oi_account)]
  } else {
    rating_at_origination <- rep(NA_character_, length(account_id))
  }

  # ---- Hierarchies via external_equivalent column -----------------------
  # Excel formula CG2:
  #   =VLOOKUP(rating, MasterRatingScale!C3:D24, 2, FALSE)
  # uses INTERNAL block's C column (external_equivalent) -> D column (hierarchy)
  # to map a Moody's-style rating (e.g. "Baa3") to a 1..21 hierarchy.
  internal_block <- static$master_rating_scale[
    static$master_rating_scale$rating_type == "Internal", ]
  ext_to_h <- setNames(as.integer(internal_block$hierarchy),
                       internal_block$external_equivalent)
  rating_hierarchy <- as.integer(ext_to_h[rating_current])
  rating_origination_hierarchy <- as.integer(ext_to_h[rating_at_origination])
  # Excel: if origination rating missing, default hierarchy to current
  # =IF(CH="", CG, VLOOKUP(...))
  rating_origination_hierarchy[is.na(rating_origination_hierarchy)] <-
    rating_hierarchy[is.na(rating_origination_hierarchy)]

  # ---- Assemble ---------------------------------------------------------
  tibble::tibble(
    extract_date            = extract_date,
    account_id              = account_id,
    customer_id_inv         = customer_id,
    account_type            = account_type,
    open_date               = open_date,
    raw_rating              = raw_rating,
    rating_external_raw     = rating_external_raw,
    rating_current          = rating_current,
    rating_at_origination   = rating_at_origination,
    rating_hierarchy        = rating_hierarchy,
    rating_origination_hierarchy = rating_origination_hierarchy,
    exposure_amount         = exposure_amount,
    maturity_date           = maturity_date,
    # Override-dependent (filled at Pass 6):
    rating_after_override   = NA_character_,
    rating_with_downgrade   = NA_character_
  )
}
