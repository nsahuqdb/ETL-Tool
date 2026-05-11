# ============================================================================
# R/validators_transform.R
#
# Transform-stage validators. Run after Phase D (build_transformation_lending,
# build_lending_portfolio_view, build_transformation_investments,
# build_investment_portfolio_view) but before derived outputs (Phase F).
#
# Args expected by every validator (passed via run_validation_suite args):
#   trans_l    — lending per-contract tibble (build_transformation_lending output)
#   cm_view    — lending per-customer view  (build_lending_portfolio_view$portfolio)
#   trans_i    — investment per-account tibble
#   inv_view   — investment per-account view
#   static     — load_static_reference() output
#   model_cfg  — load_model_config() output
#
# Coverage:
#   * Per-contract & per-account uniqueness
#   * Domain checks (rating in scale, account_type known, currency known)
#   * Numeric bounds (exposure >= 0, DPD >= 0, hierarchy 1..21)
#   * Stage rules (DPD>90 -> Stage 3, watchlist/restructured -> Stage 2/3,
#                  clean low-DPD -> Stage 1)
#   * Cross-table reconciliation (customer count, exposure totals)
#   * Investment-specific stage rule (top-tier h<=4 -> Stage 1)
# ============================================================================


# Helper: safe-get a tibble column (returns NULL if missing).
.get_col <- function(df, name) {
  if (is.null(df) || !name %in% names(df)) NULL else df[[name]]
}


#' Build the transform-stage validator list (Phase D outputs).
build_transform_validators <- function() {
  v <- list()

  # ------------------------------------------------------------------------
  # Lending: per-contract transformation (trans_l)
  # ------------------------------------------------------------------------

  v[[length(v) + 1]] <- make_validator(
    "TRANS_LEND_contract_id_unique", "ERROR", context = "trans_lending",
    description = "Every contract_id in trans_lending is unique",
    fn = function(trans_l, ...) {
      ids <- .get_col(trans_l, "contract_id")
      if (is.null(ids)) return(list(passed = TRUE))
      dup <- ids[duplicated(ids)]
      list(passed = length(dup) == 0,
           details = if (length(dup) > 0)
             list(message = sprintf("%d duplicate contract_id (e.g. %s)",
                                    length(dup),
                                    paste(head(unique(dup), 5), collapse=", ")))
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "TRANS_LEND_customer_id_populated", "ERROR", context = "trans_lending",
    description = "Every contract has a non-NA customer_id",
    fn = function(trans_l, ...) {
      ids <- .get_col(trans_l, "customer_id")
      if (is.null(ids)) return(list(passed = TRUE))
      n_na <- sum(is.na(ids) | !nzchar(as.character(ids)))
      list(passed = n_na == 0,
           details = if (n_na > 0)
             list(message = sprintf("%d contracts have NA/blank customer_id", n_na))
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "TRANS_LEND_rating_populated", "ERROR", context = "trans_lending",
    description = "Every contract has a non-NA rating after Pass 6 back-fill",
    fn = function(trans_l, ...) {
      r <- .get_col(trans_l, "rating")
      if (is.null(r)) return(list(passed = TRUE))
      n_na <- sum(is.na(r))
      list(passed = n_na == 0,
           details = if (n_na > 0)
             list(message = sprintf("%d contracts have NA rating", n_na))
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "TRANS_LEND_rating_in_internal_scale", "ERROR", context = "trans_lending",
    description = "Every rating is in the Internal portion of master_rating_scale",
    fn = function(trans_l, static, ...) {
      r <- .get_col(trans_l, "rating")
      if (is.null(r)) return(list(passed = TRUE))
      mrs <- static$master_rating_scale
      ok_set <- mrs$rating[mrs$rating_type == "Internal"]
      bad <- setdiff(unique(r[!is.na(r)]), ok_set)
      list(passed = length(bad) == 0,
           details = if (length(bad) > 0)
             list(message = sprintf("Unknown ratings: %s", paste(bad, collapse=", ")))
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "TRANS_LEND_hierarchy_in_range", "ERROR", context = "trans_lending",
    description = "rating_hierarchy is in 1..21 for every contract",
    fn = function(trans_l, ...) {
      h <- .get_col(trans_l, "rating_hierarchy")
      if (is.null(h)) return(list(passed = TRUE))
      bad <- !is.na(h) & (h < 1 | h > 21)
      list(passed = !any(bad),
           details = if (any(bad))
             list(message = sprintf("%d contracts have hierarchy outside 1..21",
                                    sum(bad)))
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "TRANS_LEND_exposure_nonneg", "WARN", context = "trans_lending",
    description = "exposure_amount is non-NA and >= 0 for every contract",
    fn = function(trans_l, ...) {
      e <- .get_col(trans_l, "exposure_amount")
      if (is.null(e)) return(list(passed = TRUE))
      n_na  <- sum(is.na(e))
      n_neg <- sum(!is.na(e) & e < 0)
      ok <- n_na == 0 && n_neg == 0
      list(passed = ok,
           details = if (!ok)
             list(message = sprintf("%d NA, %d negative", n_na, n_neg))
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "TRANS_LEND_total_exposure_positive", "ERROR", context = "trans_lending",
    description = "Total lending exposure > 0",
    fn = function(trans_l, ...) {
      e <- .get_col(trans_l, "exposure_amount")
      if (is.null(e)) return(list(passed = TRUE))
      tot <- sum(e, na.rm = TRUE)
      list(passed = tot > 0,
           details = if (tot <= 0)
             list(message = sprintf("Total exposure = %s", format(tot)))
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "TRANS_LEND_dpd_nonneg", "WARN", context = "trans_lending",
    description = "past_dues_days >= 0 for every contract",
    fn = function(trans_l, ...) {
      d <- .get_col(trans_l, "past_dues_days")
      if (is.null(d)) return(list(passed = TRUE))
      bad <- !is.na(d) & d < 0
      list(passed = !any(bad),
           details = if (any(bad))
             list(message = sprintf("%d contracts have negative DPD", sum(bad)))
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "TRANS_LEND_product_type_known", "INFO", context = "trans_lending",
    description = "account_type (product type) is in product_portfolio_mapping",
    rationale = paste0(
      "When a product type is missing from the mapping, contracts of that ",
      "type fall back to the default 'Business Finance' portfolio. That ",
      "may or may not be correct — Bai Al Wadiya, Microfinance, Greenhouse, ",
      "Fisheries, and Paid guarantees might genuinely belong to a different ",
      "portfolio with different StPD curves."),
    remediation = paste0(
      "Either add the missing product types to ",
      "data-raw/static/product_portfolio_mapping.csv with their correct ",
      "portfolio assignment (Business Finance / Off BS / Al Dhameen / ",
      "Tasdeer / Banks and Fis / Investments), or confirm the default fallback ",
      "is intended and suppress this validator."),
    fn = function(trans_l, static, ...) {
      a <- .get_col(trans_l, "account_type")
      if (is.null(a)) return(list(passed = TRUE))
      mapping <- static$product_portfolio_mapping
      if (is.null(mapping) || nrow(mapping) == 0) {
        return(list(passed = FALSE,
                    details = list(message = "product_portfolio_mapping is empty")))
      }
      known <- mapping$product_type
      observed <- unique(a[!is.na(a) & nzchar(a)])
      bad <- setdiff(observed, known)
      list(passed = length(bad) == 0,
           details = if (length(bad) > 0)
             list(message = sprintf("%d product types not in product_portfolio_mapping: %s",
                                    length(bad), paste(bad, collapse=", ")),
                  unmapped_product_types = bad)
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "TRANS_LEND_portfolio_mapping_complete", "WARN", context = "trans_lending",
    description = "Every contract's account_type maps to a portfolio (no orphans)",
    rationale = paste0(
      "Direct consequence of TRANS_LEND_product_type_known. If product ",
      "types are missing from the mapping, the contracts of that type don't ",
      "get a real portfolio assignment — they fall through to the default ",
      "'Business Finance' bucket. Output AccountMaster_1.csv will have ",
      "those contracts mis-bucketed."),
    remediation = paste0(
      "See TRANS_LEND_product_type_known. Once product_portfolio_mapping.csv ",
      "is updated to cover all observed product types, this finding clears."),
    fn = function(trans_l, static, ...) {
      a <- .get_col(trans_l, "account_type")
      if (is.null(a)) return(list(passed = TRUE))
      mapping <- static$product_portfolio_mapping
      if (is.null(mapping)) return(list(passed = TRUE))
      known <- mapping$product_type
      orphan_rows <- !is.na(a) & nzchar(a) & !(a %in% known)
      n_orphan <- sum(orphan_rows)
      list(passed = n_orphan == 0,
           details = if (n_orphan > 0) {
             counts <- table(a[orphan_rows])
             list(message = sprintf(
                 "%d contracts have product types not mapped to a portfolio: %s",
                 n_orphan,
                 paste(sprintf("%s (%d)", names(counts), as.integer(counts)),
                       collapse = ", ")),
                  orphan_contract_count = n_orphan,
                  by_product_type = as.list(counts))
           } else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "TRANS_LEND_pass6_overrides_filled", "ERROR", context = "trans_lending",
    description = "Pass 6 back-fills (rating_after_override, is_default_final) populated",
    fn = function(trans_l, ...) {
      r <- .get_col(trans_l, "rating_after_override")
      d <- .get_col(trans_l, "is_default_final")
      missing <- character()
      if (is.null(r) || any(is.na(r))) missing <- c(missing, "rating_after_override")
      if (is.null(d) || any(is.na(d))) missing <- c(missing, "is_default_final")
      ok <- length(missing) == 0
      list(passed = ok,
           details = if (!ok)
             list(message = sprintf("Unfilled: %s", paste(missing, collapse=", ")))
           else NULL)
    }
  )


  # ------------------------------------------------------------------------
  # Lending: portfolio view (cm_view) — per-customer
  # ------------------------------------------------------------------------

  v[[length(v) + 1]] <- make_validator(
    "TRANS_LENDPV_customer_id_unique", "ERROR", context = "lending_portfolio_view",
    description = "Every customer_id in lending portfolio view is unique",
    fn = function(cm_view, ...) {
      ids <- .get_col(cm_view, "customer_id")
      if (is.null(ids)) return(list(passed = TRUE))
      dup <- ids[duplicated(ids)]
      list(passed = length(dup) == 0,
           details = if (length(dup) > 0)
             list(message = sprintf("%d duplicate customer_id", length(dup)))
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "TRANS_LENDPV_stage_in_set", "ERROR", context = "lending_portfolio_view",
    description = "stage_final is in {Stage 1, Stage 2, Stage 3}",
    fn = function(cm_view, ...) {
      s <- .get_col(cm_view, "stage_final")
      if (is.null(s)) return(list(passed = TRUE))
      bad <- setdiff(unique(s[!is.na(s)]), c("Stage 1","Stage 2","Stage 3"))
      list(passed = length(bad) == 0,
           details = if (length(bad) > 0)
             list(message = sprintf("Unknown stages: %s", paste(bad, collapse=", ")))
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "TRANS_LENDPV_dpd_gt_90_implies_stage3", "ERROR",
    context = "lending_portfolio_view",
    description = "DPD > 90 implies stage_final = Stage 3",
    fn = function(cm_view, ...) {
      s <- .get_col(cm_view, "stage_final")
      d <- .get_col(cm_view, "dpd_status")
      if (is.null(s) || is.null(d)) return(list(passed = TRUE))
      bad <- !is.na(d) & d > 90 & s != "Stage 3"
      list(passed = !any(bad),
           details = if (any(bad))
             list(message = sprintf("%d customers have DPD>90 but stage != Stage 3",
                                    sum(bad)))
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "TRANS_LENDPV_clean_low_dpd_stage1", "WARN",
    context = "lending_portfolio_view",
    description = "Clean low-DPD customers (DPD<=60, no watchlist, not restructured) -> Stage 1",
    fn = function(cm_view, static, ...) {
      s <- .get_col(cm_view, "stage_final")
      d <- .get_col(cm_view, "dpd_status")
      r <- .get_col(cm_view, "restructuring_final")
      w <- .get_col(cm_view, "watchlist_status")
      if (is.null(s) || is.null(d) || is.null(r) || is.null(w))
        return(list(passed = TRUE))
      threshold <- 60
      if (!is.null(static$staging_thresholds$dpd_stage2_threshold_days)) {
        threshold <- static$staging_thresholds$dpd_stage2_threshold_days
      }
      clean <- !is.na(d) & d <= threshold &
               (is.na(r) | r == "") &
               (is.na(w) | w == "")
      bad <- clean & s != "Stage 1"
      list(passed = !any(bad),
           details = if (any(bad))
             list(message = sprintf("%d clean low-DPD customers not in Stage 1",
                                    sum(bad)))
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "TRANS_LENDPV_watchlist_implies_stage_2_or_3", "ERROR",
    context = "lending_portfolio_view",
    description = "watchlist_status='Watchlist' implies stage in {Stage 2, Stage 3}",
    fn = function(cm_view, ...) {
      s <- .get_col(cm_view, "stage_final")
      w <- .get_col(cm_view, "watchlist_status")
      if (is.null(s) || is.null(w)) return(list(passed = TRUE))
      bad <- !is.na(w) & w == "Watchlist" & !s %in% c("Stage 2","Stage 3")
      list(passed = !any(bad),
           details = if (any(bad))
             list(message = sprintf("%d watchlist customers not in Stage 2/3",
                                    sum(bad)))
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "TRANS_LENDPV_restructured_implies_stage_2_or_3", "ERROR",
    context = "lending_portfolio_view",
    description = "restructuring_final='Restructured' implies stage in {Stage 2, Stage 3}",
    fn = function(cm_view, ...) {
      s <- .get_col(cm_view, "stage_final")
      r <- .get_col(cm_view, "restructuring_final")
      if (is.null(s) || is.null(r)) return(list(passed = TRUE))
      bad <- !is.na(r) & r == "Restructured" & !s %in% c("Stage 2","Stage 3")
      list(passed = !any(bad),
           details = if (any(bad))
             list(message = sprintf("%d restructured customers not in Stage 2/3",
                                    sum(bad)))
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "TRANS_LENDPV_customer_count_matches_trans", "ERROR",
    context = "lending_portfolio_view",
    description = "cm_view row count == distinct customer_id in trans_lending",
    fn = function(trans_l, cm_view, ...) {
      if (is.null(trans_l) || is.null(cm_view)) return(list(passed = TRUE))
      n_cm    <- nrow(cm_view)
      n_trans <- length(unique(trans_l$customer_id))
      list(passed = n_cm == n_trans,
           details = if (n_cm != n_trans)
             list(message = sprintf("cm_view=%d, distinct trans customers=%d",
                                    n_cm, n_trans))
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "TRANS_LENDPV_exposure_reconciles", "ERROR",
    context = "lending_portfolio_view",
    description = "Sum of cm_view exposure_total == sum of trans_lending exposure_amount",
    fn = function(trans_l, cm_view, ...) {
      if (is.null(trans_l) || is.null(cm_view)) return(list(passed = TRUE))
      a <- sum(.get_col(cm_view, "exposure_total"),  na.rm = TRUE)
      b <- sum(.get_col(trans_l, "exposure_amount"), na.rm = TRUE)
      tol <- max(1e-2, abs(b) * 1e-9)
      list(passed = abs(a - b) <= tol,
           details = if (abs(a - b) > tol)
             list(message = sprintf("cm_view sum=%s, trans sum=%s, diff=%s",
                                    format(a), format(b), format(a - b)))
           else NULL)
    }
  )


  # ------------------------------------------------------------------------
  # Investment: per-account transformation (trans_i)
  # ------------------------------------------------------------------------

  v[[length(v) + 1]] <- make_validator(
    "TRANS_INV_account_id_unique", "ERROR", context = "trans_investments",
    description = "Every account_id in trans_investments is unique",
    fn = function(trans_i, ...) {
      ids <- .get_col(trans_i, "account_id")
      if (is.null(ids)) return(list(passed = TRUE))
      dup <- ids[duplicated(ids)]
      list(passed = length(dup) == 0,
           details = if (length(dup) > 0)
             list(message = sprintf("%d duplicate account_id", length(dup)))
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "TRANS_INV_rating_populated", "ERROR", context = "trans_investments",
    description = "Every investment has a non-NA rating_current",
    fn = function(trans_i, ...) {
      r <- .get_col(trans_i, "rating_current")
      if (is.null(r)) return(list(passed = TRUE))
      n_na <- sum(is.na(r))
      list(passed = n_na == 0,
           details = if (n_na > 0)
             list(message = sprintf("%d investments have NA rating_current", n_na))
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "TRANS_INV_rating_in_external_scale", "ERROR", context = "trans_investments",
    description = "Every rating_current is in the External portion of master_rating_scale",
    fn = function(trans_i, static, ...) {
      r <- .get_col(trans_i, "rating_current")
      if (is.null(r)) return(list(passed = TRUE))
      mrs <- static$master_rating_scale
      ok_set <- mrs$rating[mrs$rating_type == "External"]
      bad <- setdiff(unique(r[!is.na(r)]), ok_set)
      list(passed = length(bad) == 0,
           details = if (length(bad) > 0)
             list(message = sprintf("Unknown ratings: %s", paste(bad, collapse=", ")))
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "TRANS_INV_hierarchy_in_range", "ERROR", context = "trans_investments",
    description = "rating_hierarchy is in 1..21 for every investment",
    fn = function(trans_i, ...) {
      h <- .get_col(trans_i, "rating_hierarchy")
      if (is.null(h)) return(list(passed = TRUE))
      bad <- !is.na(h) & (h < 1 | h > 21)
      list(passed = !any(bad),
           details = if (any(bad))
             list(message = sprintf("%d investments have hierarchy outside 1..21",
                                    sum(bad)))
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "TRANS_INV_exposure_nonneg", "WARN", context = "trans_investments",
    description = "exposure_amount is non-NA and >= 0 for every investment",
    fn = function(trans_i, ...) {
      e <- .get_col(trans_i, "exposure_amount")
      if (is.null(e)) return(list(passed = TRUE))
      n_na  <- sum(is.na(e))
      n_neg <- sum(!is.na(e) & e < 0)
      ok <- n_na == 0 && n_neg == 0
      list(passed = ok,
           details = if (!ok)
             list(message = sprintf("%d NA, %d negative", n_na, n_neg))
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "TRANS_INV_total_exposure_positive", "ERROR", context = "trans_investments",
    description = "Total investment exposure > 0",
    fn = function(trans_i, ...) {
      e <- .get_col(trans_i, "exposure_amount")
      if (is.null(e)) return(list(passed = TRUE))
      tot <- sum(e, na.rm = TRUE)
      list(passed = tot > 0,
           details = if (tot <= 0)
             list(message = sprintf("Total exposure = %s", format(tot)))
           else NULL)
    }
  )


  # ------------------------------------------------------------------------
  # Investment: portfolio view (inv_view)
  # ------------------------------------------------------------------------

  v[[length(v) + 1]] <- make_validator(
    "TRANS_INVPV_account_count_matches_trans", "ERROR",
    context = "investment_portfolio_view",
    description = "inv_view row count == nrow(trans_investments)",
    fn = function(trans_i, inv_view, ...) {
      if (is.null(trans_i) || is.null(inv_view)) return(list(passed = TRUE))
      list(passed = nrow(inv_view) == nrow(trans_i),
           details = if (nrow(inv_view) != nrow(trans_i))
             list(message = sprintf("inv_view=%d, trans_i=%d",
                                    nrow(inv_view), nrow(trans_i)))
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "TRANS_INVPV_stage_in_set", "ERROR", context = "investment_portfolio_view",
    description = "stage_final is in {Stage 1, Stage 2, Stage 3}",
    fn = function(inv_view, ...) {
      s <- .get_col(inv_view, "stage_final")
      if (is.null(s)) return(list(passed = TRUE))
      bad <- setdiff(unique(s[!is.na(s)]), c("Stage 1","Stage 2","Stage 3"))
      list(passed = length(bad) == 0,
           details = if (length(bad) > 0)
             list(message = sprintf("Unknown stages: %s", paste(bad, collapse=", ")))
           else NULL)
    }
  )

  v[[length(v) + 1]] <- make_validator(
    "TRANS_INVPV_top_tier_implies_stage1", "WARN",
    context = "investment_portfolio_view",
    description = "Top-tier investments (rating_hierarchy <= 4) -> Stage 1",
    fn = function(trans_i, inv_view, ...) {
      h <- .get_col(trans_i, "rating_hierarchy")
      s <- .get_col(inv_view, "stage_final")
      if (is.null(h) || is.null(s) || nrow(inv_view) != nrow(trans_i)) {
        return(list(passed = TRUE))
      }
      bad <- !is.na(h) & h <= 4 & s != "Stage 1"
      list(passed = !any(bad),
           details = if (any(bad))
             list(message = sprintf("%d top-tier investments not in Stage 1",
                                    sum(bad)))
           else NULL)
    }
  )

  v
}


#' Convenience wrapper: build + run the transform validators in one call.
#'
#' @return tibble of validation results.
validate_transform_outputs <- function(trans_l, cm_view,
                                          trans_i, inv_view,
                                          static, model_cfg = NULL,
                                          verbose = TRUE) {
  validators <- build_transform_validators()
  run_validation_suite(
    "TRANSFORM", validators,
    args = list(trans_l = trans_l, cm_view = cm_view,
                trans_i = trans_i, inv_view = inv_view,
                static  = static,  model_cfg = model_cfg),
    verbose = verbose
  )
}
