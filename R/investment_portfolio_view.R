# ============================================================================
# R/investment_portfolio_view.R
#
# Build the per-account Inputs_Investment Portfolio view.
#
# Replicates the SICR-based (Significant Increase in Credit Risk) staging
# logic specific to the investment book — fundamentally different from
# lending which uses DPD-based staging.
#
# Public API:
#   apply_sicr_staging(rating_hierarchy, rating_origination_hierarchy)
#   build_investment_portfolio_view(transformation_inv, inputs, static,
#                                    rating_overrides = NULL,
#                                    stage_overrides = NULL,
#                                    apply_one_notch_downgrade = FALSE)
#     -> list(portfolio = tibble, transformation = updated tibble)
#
# Excel staging formula (Inputs_Investment Portfolio!H5):
#   IF account_id = "" -> ""
#   IF rating_hierarchy <= 4 -> "Stage 1"             (top 4 ratings always Stage 1)
#   IF rating_hierarchy < 11 ->
#      IF (rating_hierarchy - origination_hierarchy) >= 2 -> "Stage 2"
#      ELSE "Stage 1"
#   ELSE
#      IF (rating_hierarchy - origination_hierarchy) >= 1 -> "Stage 2"
#      ELSE "Stage 1"
#
# Note: there is NO Stage 3 produced by this formula. The investment book
# uses migration-based staging only; defaulted bonds would need to be
# moved to Stage 3 via the staging_override mechanism.
# ============================================================================


#' Apply SICR-based staging.
#'
#' @param rating_hierarchy             integer (current rating's hierarchy)
#' @param rating_origination_hierarchy integer (origination rating's hierarchy)
#' @return chr vector of "Stage 1" / "Stage 2"
apply_sicr_staging <- function(rating_hierarchy, rating_origination_hierarchy) {
  # Hierarchy increases as rating worsens — so positive migration = downgrade
  migration <- rating_hierarchy - rating_origination_hierarchy

  out <- rep("Stage 1", length(rating_hierarchy))

  # Top 4 ratings → always Stage 1 (regardless of migration)
  in_top_tier <- !is.na(rating_hierarchy) & rating_hierarchy <= 4

  # Investment-grade range (5..10) → 2-notch threshold
  in_ig <- !is.na(rating_hierarchy) & rating_hierarchy > 4 & rating_hierarchy < 11
  out[in_ig & !is.na(migration) & migration >= 2] <- "Stage 2"

  # Junk range (11+) → 1-notch threshold
  in_junk <- !is.na(rating_hierarchy) & rating_hierarchy >= 11
  out[in_junk & !is.na(migration) & migration >= 1] <- "Stage 2"

  # Top tier preserved as Stage 1 (already initialised)
  out[in_top_tier] <- "Stage 1"

  # Where hierarchy is NA we leave Stage 1 as the default; downstream caller
  # can decide whether to flag. (This matches Excel's behaviour where blank
  # account_id gives "" but hierarchy=NA gives Stage 1 by short-circuit.)
  out
}


#' Build the Inputs_Investment Portfolio view.
#'
#' @param transformation_inv          Output of build_transformation_investments()
#' @param inputs                      Output of read_all_inputs()
#' @param static                      Output of load_static_reference()
#' @param rating_overrides            Tibble (account_id, rating_override) or NULL
#'                                    (NB: keyed by ACCOUNT, not customer)
#' @param stage_overrides             Tibble (account_id, stage_override) or NULL
#' @param apply_one_notch_downgrade   Logical scalar, default FALSE
#' @return list(portfolio = tibble, transformation = updated tibble)
build_investment_portfolio_view <- function(transformation_inv,
                                             inputs,
                                             static,
                                             rating_overrides         = NULL,
                                             stage_overrides          = NULL,
                                             apply_one_notch_downgrade = FALSE) {

  if (nrow(transformation_inv) == 0L) {
    # Empty input -> empty output
    portfolio <- tibble::tibble(
      account_id              = character(),
      exposure                = double(),
      rating_current          = character(),
      rating_override         = character(),
      rating_final            = character(),
      rating_override_status  = character(),
      rating_at_origination   = character(),
      stage_pre_override      = character(),
      stage_override          = character(),
      stage_final             = character(),
      stage_override_status   = character()
    )
    return(list(portfolio = portfolio, transformation = transformation_inv))
  }

  account_id     <- transformation_inv$account_id
  rating_current <- transformation_inv$rating_current

  # ---- Rating override --------------------------------------------------
  ro <- normalise_overrides(rating_overrides, "rating_override")
  rating_override <- rep(NA_character_, length(account_id))
  rating_override[match(ro$customer_id, account_id)] <- ro$override_value
  # NB: normalise_overrides expects "customer_id" — we reuse it for accounts
  # by passing the account_id under that name. (This is a pragmatic shortcut;
  # if it ever bites we'll add a separate normalise_overrides_by_account.)

  rating_final <- ifelse(is.na(rating_override) | rating_override == "",
                         rating_current, rating_override)
  rating_override_status <- ifelse(
    is.na(rating_override) | rating_override == "",
    "No Rating Override",
    paste0("Rating Overridden from ", rating_current, " to ", rating_override)
  )

  # ---- SICR-based staging (pre-override) --------------------------------
  stage_pre <- apply_sicr_staging(
    rating_hierarchy             = transformation_inv$rating_hierarchy,
    rating_origination_hierarchy = transformation_inv$rating_origination_hierarchy
  )

  ro3 <- normalise_overrides(stage_overrides, "stage_override")
  stage_override <- rep(NA_character_, length(account_id))
  stage_override[match(ro3$customer_id, account_id)] <- ro3$override_value
  stage_final <- ifelse(is.na(stage_override) | stage_override == "",
                        stage_pre, stage_override)
  stage_override_status <- ifelse(
    stage_pre == stage_final,
    "No Staging Override",
    paste0("Staging Overridden from ", stage_pre, " to ", stage_override)
  )

  # ---- Assemble portfolio view ------------------------------------------
  portfolio <- tibble::tibble(
    account_id              = account_id,
    exposure                = transformation_inv$exposure_amount,
    rating_current          = rating_current,
    rating_override         = rating_override,
    rating_final            = rating_final,
    rating_override_status  = rating_override_status,
    rating_at_origination   = transformation_inv$rating_at_origination,
    stage_pre_override      = stage_pre,
    stage_override          = stage_override,
    stage_final             = stage_final,
    stage_override_status   = stage_override_status
  )

  # ---- Pass 6: back-fill override-dependent Transformation columns ------
  rating_after_override <- rating_final
  if (apply_one_notch_downgrade) {
    rating_with_downgrade <- apply_one_notch(rating_after_override,
                                              static$master_rating_downgrade)
  } else {
    rating_with_downgrade <- rating_after_override
  }

  transformation_inv$rating_after_override <- rating_after_override
  transformation_inv$rating_with_downgrade <- rating_with_downgrade

  list(portfolio = portfolio, transformation = transformation_inv)
}
