# ============================================================================
# R/transform_lending.R
#
# Build the per-contract `Transformation` table and the per-customer
# `Inputs_Lending Portfolio` table for the LENDING side of the book.
#
# Mapping doc: doc/transformation_mapping.md (READ THIS FIRST)
# Excel sheets being replicated:
#   - Transformation               (one row per contract)
#   - Inputs_Lending Portfolio     (one row per customer)
#
# Public API:
#   normalise_extract_date(x)
#   apply_id_substitutions(contract_ids, off_balance_products)
#   derive_rating_lending(...)
#   build_transformation_lending(inputs, static, model_cfg, run_cfg)
#   build_lending_portfolio_view(transformation, inputs, static, overrides)
#
# Conventions:
#   - All public functions take typed tibbles in, return typed tibbles out.
#   - Empty / missing values are NA (NOT empty string), per R conventions.
#     Conversion to "" for output happens in Phase G writers.
#   - Dates are class Date.
#   - All amounts are double.
# ============================================================================


# ---------------------------------------------------------------------------
# Date normalisation
# ---------------------------------------------------------------------------

#' Normalise an extract-date column from any of the source formats.
#'
#' xlsx files give R proper Date or POSIXct values via readxl. SQL*Plus HTML
#' files give strings like "31-DEC-25". This helper accepts either and
#' returns class Date.
#'
#' @param x A vector of dates as either Date, POSIXct, or character
#' @return Date vector, same length as x. Unparseable values become NA.
normalise_extract_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))

  s <- trimws(as.character(x))
  s[s == "" | is.na(s)] <- NA_character_

  out <- rep(as.Date(NA), length(s))
  ok  <- !is.na(s)

  # Try yyyy-mm-dd first (xlsx-as-string)
  out[ok] <- suppressWarnings(as.Date(s[ok], format = "%Y-%m-%d"))

  # Then dd-MMM-yy (SQL*Plus format like 31-DEC-25)
  miss <- ok & is.na(out)
  if (any(miss)) {
    out[miss] <- suppressWarnings(as.Date(s[miss], format = "%d-%b-%y"))
  }

  # Then dd-MMM-yyyy
  miss <- ok & is.na(out)
  if (any(miss)) {
    out[miss] <- suppressWarnings(as.Date(s[miss], format = "%d-%b-%Y"))
  }

  out
}


# ---------------------------------------------------------------------------
# Contract ID transformation
# ---------------------------------------------------------------------------

#' Apply the off-balance-product code substitution to contract IDs.
#'
#' Implements Transformation!F: contract IDs that are pure numeric are kept
#' as-is. Non-numeric IDs (off-balance products like "0000112FGG000413")
#' have characters at positions 8-10 ("FGG", "DEN", etc.) replaced with
#' the corresponding numeric LIC code from off_balance_products.
#'
#' Excel formula:
#'   =IFERROR(B*1, LEFT(B,7) & VLOOKUP(MID(B,8,3), Assumptions!B3:C15, 2)
#'                 & RIGHT(B,3))
#'
#' @param contract_ids   Character vector of raw contract IDs
#' @param off_balance_products  Tibble with cols (product_code, lic_input_code)
#' @return Character vector of transformed IDs (still strings — even pure
#'         numeric ones, since they may not fit in int32)
apply_id_substitutions <- function(contract_ids, off_balance_products) {
  s <- as.character(contract_ids)
  out <- s

  # Build named lookup: product_code -> lic_input_code
  lkp <- setNames(as.character(off_balance_products$lic_input_code),
                  off_balance_products$product_code)

  # Excel V4 formula:
  #   =IFERROR(B*1, (LEFT(B,7) & VLOOKUP(MID(B,8,3), Assumptions!B3:C15, 2)
  #                  & RIGHT(B,6)) * 1)
  #
  # Notes:
  #   * Both branches end in `*1` -> coerce to numeric -> drops leading zeros.
  #   * RIGHT(B, 6) keeps the LAST SIX characters (positions 11..16 in a
  #     16-char id like "0000112FGG000471"). It is NOT RIGHT(B, 3).
  is_pure_num <- !is.na(suppressWarnings(as.numeric(s)))
  needs_sub   <- !is_pure_num & nchar(s) >= 10

  # Pure numeric: drop leading zeros via numeric coercion
  if (any(is_pure_num)) {
    out[is_pure_num] <- as.character(as.numeric(s[is_pure_num]))
  }

  if (any(needs_sub)) {
    code <- substr(s[needs_sub], 8, 10)
    sub_val <- lkp[code]
    sub_val[is.na(sub_val)] <- code[is.na(sub_val)]
    raw <- paste0(
      substr(s[needs_sub], 1, 7),
      sub_val,
      substr(s[needs_sub], nchar(s[needs_sub]) - 5, nchar(s[needs_sub]))
    )
    raw_num <- suppressWarnings(as.numeric(raw))
    out_sub <- ifelse(!is.na(raw_num), as.character(raw_num), raw)
    out[needs_sub] <- out_sub
  }
  out
}


# ---------------------------------------------------------------------------
# Sector lookup from industry description
# ---------------------------------------------------------------------------

#' Look up sector from industry description (or the 4-digit code prefix).
#'
#' Excel: =IFERROR(VLOOKUP(LEFT(industry_desc, 4), Assumptions!F2:G201, 2,
#'                         FALSE), "")
#'
#' Where Assumptions!F:G is the (industry_code, sector) mapping. Returns
#' NA_character_ if no match (Excel returns "").
#'
#' @param industry_desc        Character vector e.g. "2395 MANUFACTURE OF ARTICLE"
#' @param industry_sector_mapping  Tibble (industry_code, industry_description, sector)
#' @return Character vector of sectors (NA where no match)
lookup_sector <- function(industry_desc, industry_sector_mapping) {
  s <- as.character(industry_desc)
  prefix <- substr(s, 1, 4)
  m <- match(prefix, industry_sector_mapping$industry_code)
  ifelse(is.na(m), NA_character_, industry_sector_mapping$sector[m])
}


# ---------------------------------------------------------------------------
# Rating derivation — the multi-step chain (Transformation!Z)
# ---------------------------------------------------------------------------

#' Derive the per-contract rating per the Excel rating chain.
#'
#' Excel formula Z2 (full chain — see doc/transformation_mapping.md):
#'   IF F2 = "" -> ""                         (no contract id)
#'   IFERROR(
#'     IF Y2 not in external scale:           (no external rating)
#'       IF sector = "Agriculture":  VLOOKUP(dpd_worst, Assumptions!I6:J9 , 2, TRUE)
#'       IF sector = "Fisheries":   VLOOKUP(dpd_worst, Assumptions!I13:J16, 2, TRUE)
#'       IF sector = "Livestock":   VLOOKUP(dpd_worst, Assumptions!I20:J23, 2, TRUE)
#'       ELSE
#'         IF account_type = "Al Dhameen": Inputs_Lending Portfolio!T6 (fallback for Al Dhameen)
#'         ELSE:                            Inputs_Lending Portfolio!T5 (fallback for unrated)
#'     ELSE Y2,
#'   /* IFERROR fallback when sector lookups fail */
#'   Inputs_Lending Portfolio!T5)
#'
#' VLOOKUP with TRUE means "find largest dpd_threshold <= dpd_worst".
#'
#' @param external_rating       chr  external rating (NA if customer not externally rated)
#' @param sector                chr  Agriculture / Fisheries / Livestock / other / NA
#' @param past_dues_worst       num  worst DPD across customer's contracts
#' @param account_type          chr  e.g. "Al Dhameen", "Direct Lending"
#' @param collective_rules      tibble (sector, dpd_threshold, rating)
#' @param segment_fallback      named character vector — names are segment
#'                              labels e.g. "Unrated Customer (Internal Rating)"
#'                              and "Al Dhameen Customers", values are ratings
#' @return chr vector
derive_rating_lending <- function(external_rating, sector, past_dues_worst,
                                  account_type, collective_rules,
                                  segment_fallback) {
  n <- length(external_rating)
  out <- rep(NA_character_, n)

  # 1. External rating wins if present
  has_ext <- !is.na(external_rating) & nzchar(external_rating)
  out[has_ext] <- external_rating[has_ext]

  needs_lookup <- !has_ext

  # 2. Sector-specific collective assessment
  for (sec in unique(collective_rules$sector)) {
    # Strip trailing " Sector" — Excel matches just "Agriculture", "Fisheries",
    # "Livestock" against col T (which the formula writes as "Manufacturing"
    # etc.). Note the Excel formula has both "Lifestock" (Assumptions block)
    # and "Livestock" (Z formula) — we match case-insensitively.
    sec_short <- sub("\\s*Sector\\s*$", "", sec, ignore.case = TRUE)
    rule <- collective_rules[collective_rules$sector == sec, ]
    rule <- rule[order(rule$dpd_threshold), ]
    rows_in_sector <- needs_lookup & !is.na(sector) &
      (tolower(sector) == tolower(sec_short) |
       tolower(sector) == tolower(sub("Lifestock", "Livestock",
                                      sec_short, ignore.case = TRUE)))
    if (any(rows_in_sector)) {
      idx <- findInterval(past_dues_worst[rows_in_sector],
                          rule$dpd_threshold)
      idx[idx == 0] <- 1L
      out[rows_in_sector] <- rule$rating[idx]
    }
  }

  # 3. Segment-level fallback for everything still missing
  fb_unrated <- segment_fallback[["Unrated Customer (Internal Rating)"]]
  fb_dhameen <- segment_fallback[["Al Dhameen Customers"]]
  still_missing <- needs_lookup & is.na(out)
  is_dhameen <- !is.na(account_type) & tolower(account_type) == "al dhameen"
  out[still_missing &  is_dhameen] <- fb_dhameen
  out[still_missing & !is_dhameen] <- fb_unrated

  out
}


# ---------------------------------------------------------------------------
# Build the Transformation table
# ---------------------------------------------------------------------------

#' Build the per-contract Transformation table (lending portfolio).
#'
#' Mirrors Pass 1-3 from doc/transformation_mapping.md. Override-dependent
#' columns (AD, AE, BL, BM, BT, BW) are filled in
#' build_lending_portfolio_view() at Pass 6 — we leave them NA here.
#'
#' @param inputs    output of read_all_inputs() — list of tibbles
#' @param static    output of load_static_reference()
#' @param model_cfg output of load_model_config()
#' @param run_cfg   output of load_run_config()
#' @return Tibble with one row per contract
build_transformation_lending <- function(inputs, static, model_cfg, run_cfg) {
  am  <- inputs$AccountMaster
  cm  <- inputs$CustomerMaster
  ind <- inputs$IndustryCode

  if (is.null(am) || nrow(am) == 0) {
    stop("AccountMaster is empty — cannot build Transformation")
  }

  # ---- Date normalisation ------------------------------------------------
  # Schema returns canonical lowercase snake_case columns with proper types.
  # extract_date is already a Date vector; normalise_extract_date handles
  # both Date and other inputs gracefully.
  extract_date <- normalise_extract_date(am$extract_date)

  # ---- Contract ID transformation ----------------------------------------
  contract_id <- apply_id_substitutions(am$contract_id, static$off_balance_products)

  # ---- Customer ID -------------------------------------------------------
  customer_id <- as.character(am$customer_id)

  # ---- Account type ------------------------------------------------------
  account_type <- as.character(am$account_type)

  # ---- Industry -> Sector -----------------------------------------------
  # IndustryCode schema canonical columns: customer_id, industry_description.
  if (!is.null(ind) && nrow(ind) > 0) {
    ind_lkp <- setNames(as.character(ind$industry_description),
                        as.character(ind$customer_id))
    qdb_industry_desc <- ind_lkp[customer_id]
  } else {
    qdb_industry_desc <- rep(NA_character_, length(customer_id))
  }
  sector <- lookup_sector(qdb_industry_desc, static$industry_sector_mapping)

  # ---- Open date ---------------------------------------------------------
  open_date <- normalise_extract_date(am$open_date)

  # ---- DPD per contract --------------------------------------------------
  past_dues_days <- suppressWarnings(as.numeric(am$past_due_days))
  past_dues_days[is.na(past_dues_days)] <- 0

  # ---- Per-customer worst DPD -------------------------------------------
  worst_dpd <- ave(past_dues_days, customer_id, FUN = max)

  # ---- Exposure (ON_BALANCE) --------------------------------------------
  exposure_amount <- suppressWarnings(as.numeric(am$on_balance))
  exposure_amount[is.na(exposure_amount)] <- 0

  # ---- Maturity date with extension rule --------------------------------
  raw_maturity <- normalise_extract_date(am$maturity_date)
  reporting_dt <- if (length(extract_date) > 0) max(extract_date, na.rm = TRUE) else as.Date(NA)
  ext_days <- as.integer(static$staging_thresholds$maturity_extension_days)
  maturity_date <- raw_maturity
  # V4 Transformation!AM formula uses strictly less than (`<`):
  #   IF(raw_maturity < extract_date, extract_date + ext_days, raw_maturity)
  # So a contract maturing exactly ON the extract date is NOT extended.
  needs_ext <- !is.na(raw_maturity) & !is.na(reporting_dt) &
               raw_maturity < reporting_dt
  maturity_date[needs_ext] <- reporting_dt + ext_days

  # ---- EIR ---------------------------------------------------------------
  # V4 'Inputs_Lending Portfolio'!AL5:AN15 — fallback EIRs computed at
  # runtime from the current run's data. The table has 11 account_types
  # in two groups:
  #
  #   Self-mean group (Business Finance portfolio):
  #     Long Term, Ijara, Murabaha, Short Term, Istinsa, Tawaraq, Forward Ijara
  #     fallback_eir[t] = AVERAGEIF(account_type == t, AP)   where AP = EIR/100
  #
  #   Mean-of-self-means group (Off BS / Al Dhameen / Tasdeer portfolios):
  #     Al Dhameen, LG, LC, Tasdeer
  #     fallback_eir[t] = AVERAGE(self_mean values above)
  #
  # We derive the grouping from product_portfolio_mapping (Business Finance
  # vs the rest) instead of a hardcoded list.
  #
  # Excel chain:
  #   AP = IF(EIR=0, "", EIR/100)
  #   AQ = IF(AP="", VLOOKUP(account_type, AL5:AN15, 3) ELSE AVERAGE(AM9:AM15)
  #          ELSE AP)
  # NominalInterestRate (BC) = AQ directly.
  # Schema canonical column: am$eir (numeric, units = "raw %" e.g. 4.2).
  eir_raw <- suppressWarnings(as.numeric(am$eir))
  eir <- ifelse(is.na(eir_raw) | eir_raw == 0, NA_real_, eir_raw / 100)

  if (any(is.na(eir))) {
    # 1. Self-mean per account_type, restricted to Business Finance products
    bf_types <- character(0)
    if (!is.null(static$product_portfolio_mapping)) {
      ppm <- static$product_portfolio_mapping
      bf_types <- as.character(
        ppm$product_type[ppm$portfolio == "Business Finance"]
      )
    }

    self_means <- tapply(eir, account_type, function(x) {
      vals <- x[!is.na(x)]
      if (length(vals) == 0) NA_real_ else mean(vals)
    })

    # 2. mean-of-self-means uses ONLY Business-Finance product types
    bf_self_means <- self_means[names(self_means) %in% bf_types]
    bf_self_means <- bf_self_means[!is.na(bf_self_means)]
    mean_of_means <- if (length(bf_self_means) > 0) {
      mean(bf_self_means)
    } else {
      mean(eir, na.rm = TRUE)
    }

    # 3. Build per-type fallback: BF product types get their self_mean,
    #    everything else gets mean_of_means.
    fill <- numeric(length(account_type))
    for (i in seq_along(account_type)) {
      at <- account_type[i]
      if (is.na(at) || !nzchar(at)) {
        fill[i] <- mean_of_means
      } else if (at %in% bf_types && !is.na(self_means[at])) {
        fill[i] <- self_means[at]
      } else {
        fill[i] <- mean_of_means
      }
    }
    eir[is.na(eir)] <- fill[is.na(eir)]
  }

  # ---- Customer rating via the chain ------------------------------------
  # Schema canonical columns: cm$customer_id, cm$rating (V4
  # CustomerMasterExtract!F is the source for Transformation!Y).
  if (!is.null(cm) && nrow(cm) > 0) {
    cm_rating <- setNames(as.character(cm$rating),
                          as.character(cm$customer_id))
    cust_rating_raw <- cm_rating[customer_id]
  } else {
    cust_rating_raw <- rep(NA_character_, length(customer_id))
  }
  # The Y formula matches against MasterRatingScale!F$4:F$45 which is the
  # combined Internal+External label column. So a customer whose rating is
  # stored as an Internal QDB label (e.g. 'QDB 9') still passes the Y test.
  combined_scale <- static$master_rating_scale$rating
  external_rating <- ifelse(cust_rating_raw %in% combined_scale,
                            cust_rating_raw, NA_character_)

  # Build segment fallback lookup
  seg_fb <- setNames(static$segment_fallback_ratings$fallback_rating,
                     static$segment_fallback_ratings$segment)

  rating <- derive_rating_lending(
    external_rating  = external_rating,
    sector           = sector,
    past_dues_worst  = worst_dpd,
    account_type     = account_type,
    collective_rules = static$collective_assessment_rules,
    segment_fallback = seg_fb
  )

  # ---- Rating hierarchy + per-customer worst rating ---------------------
  rating_lkp <- setNames(static$master_rating_scale$hierarchy,
                         static$master_rating_scale$rating)
  rating_hierarchy <- as.integer(rating_lkp[rating])

  # AB: per customer, max of AA across contracts
  rating_hierarchy_worst <- as.integer(ave(
    rating_hierarchy, customer_id,
    FUN = function(x) if (all(is.na(x))) NA_integer_ else max(x, na.rm = TRUE)
  ))

  # AC: rating string at that worst hierarchy index
  # Excel: =INDEX(MasterRatingScale!B4:B24, AB)
  # i.e. the INTERNAL rating list, indexed by hierarchy.
  internal_scale <- static$master_rating_scale[
    static$master_rating_scale$rating_type == "Internal", ]
  internal_scale <- internal_scale[order(internal_scale$hierarchy), ]
  rating_worst <- internal_scale$rating[rating_hierarchy_worst]

  # ---- AD/AE: apply customer-level rating overrides ---------------------
  # Mirrors V4's `Inputs_Lending Portfolio!A:F` per-customer override table.
  # In the V4 production data only one override exists (CIF 31884 -> QDB 1).
  # The override table is stored as a static CSV today
  # (data-raw/static/customer_rating_overrides.csv) and will be writable by
  # users via Shiny later.
  #
  # Column AW5 of `Inputs_Lending Portfolio` is "No downgrade" in V4, so the
  # AE downgrade step is a no-op. If/when AW5 changes to a downgrade mode,
  # add a model_config flag and apply static$master_rating_downgrade here.
  if (!is.null(static$customer_rating_overrides) &&
      nrow(static$customer_rating_overrides) > 0) {
    ovr <- active_overrides(static$customer_rating_overrides)
    if (!is.null(ovr) && nrow(ovr) > 0) {
      ovr_lkp <- setNames(as.character(ovr$override_rating),
                          as.character(ovr$customer_id))
      ovr_hit <- ovr_lkp[as.character(customer_id)]
      has_ovr <- !is.na(ovr_hit) & nzchar(ovr_hit)
      if (any(has_ovr)) {
        rating_worst[has_ovr] <- ovr_hit[has_ovr]
        # Recompute the per-customer worst hierarchy from the overridden label
        ovr_hier <- as.integer(rating_lkp[ovr_hit[has_ovr]])
        rating_hierarchy_worst[has_ovr] <- ovr_hier
      }
    }
  }

  # ---- Customer staging flags (read inputs) ------------------------------
  # Schema canonical columns: customer_id, is_watchlist (V4 col D),
  # is_local1 (V4 col G = restructuring flag).
  csf <- inputs$CustomerStagingFlag
  if (!is.null(csf) && nrow(csf) > 0) {
    csf_id <- as.character(csf$customer_id)

    cs_lkp_wl <- setNames(suppressWarnings(as.integer(csf$is_watchlist)), csf_id)
    is_watchlist <- as.integer(cs_lkp_wl[customer_id])
    is_watchlist[is.na(is_watchlist)] <- 0L

    cs_lkp_rs <- setNames(suppressWarnings(as.integer(csf$is_local1)), csf_id)
    is_restructured <- as.integer(cs_lkp_rs[customer_id])
    is_restructured[is.na(is_restructured)] <- 0L
  } else {
    is_watchlist    <- rep(0L, length(customer_id))
    is_restructured <- rep(0L, length(customer_id))
  }

  # ---- Default flag from DPD --------------------------------------------
  # Excel BL: VLOOKUP(customer_id, J:AG, 24) = worst_dpd > 90 -> 1 else 0
  is_default <- as.integer(worst_dpd > 90)

  # ---- Payment frequency / type -----------------------------------------
  # Schema canonical column: am$payment_frequency.
  payment_frequency_months <- suppressWarnings(as.numeric(am$payment_frequency))
  payment_frequency_months[is.na(payment_frequency_months) |
                           payment_frequency_months == 0] <- NA_real_

  payment_type <- ifelse(is.na(payment_frequency_months), 3L, 4L)

  # ---- Currency, deferral, nominal interest rate -------------------------
  currency <- as.character(am$currency_code)
  currency[is.na(currency) | !nzchar(currency)] <- "QAR"

  deferral_period <- suppressWarnings(as.numeric(am$deferral_period))
  deferral_period[is.na(deferral_period)] <- 0

  nominal_int_rate <- eir  # per Excel BC = AQ

  # ---- Assemble ----------------------------------------------------------
  tibble::tibble(
    extract_date            = extract_date,
    contract_id_raw         = as.character(am$contract_id),
    contract_id             = contract_id,
    customer_id             = customer_id,
    account_type            = account_type,
    qdb_industry_desc       = qdb_industry_desc,
    sector                  = sector,
    open_date               = open_date,
    external_rating         = external_rating,
    rating                  = rating,
    rating_hierarchy        = rating_hierarchy,
    rating_hierarchy_worst  = rating_hierarchy_worst,
    rating_worst            = rating_worst,
    past_dues_days          = past_dues_days,
    past_dues_worst         = worst_dpd,
    exposure_amount         = exposure_amount,
    maturity_date           = maturity_date,
    eir                     = eir,
    payment_frequency_months = payment_frequency_months,
    payment_type            = payment_type,
    currency                = currency,
    deferral_period         = deferral_period,
    nominal_int_rate        = nominal_int_rate,
    is_watchlist            = is_watchlist,
    is_restructured         = is_restructured,
    is_default              = is_default,
    # Override-dependent fields (filled at Pass 6)
    rating_after_override   = NA_character_,
    rating_with_downgrade   = NA_character_,
    is_default_final        = NA_integer_,
    is_restructured_final   = NA_integer_,
    is_stage2               = NA_integer_
  )
}
