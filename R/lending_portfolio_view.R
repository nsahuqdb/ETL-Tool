# ============================================================================
# R/lending_portfolio_view.R
#
# Build the per-customer Inputs_Lending Portfolio view.
#
# Replicates Pass 4-6 of doc/transformation_mapping.md:
#   - Pass 4: per-customer aggregation from Transformation
#   - Pass 5: apply user overrides (rating, restructuring, staging)
#   - Pass 6: back-fill the override-dependent columns of Transformation
#
# Public API:
#   build_lending_portfolio_view(transformation, inputs, static, model_cfg,
#                                rating_overrides = NULL,
#                                restructuring_overrides = NULL,
#                                stage_overrides = NULL,
#                                apply_one_notch_downgrade = FALSE)
#     -> list(portfolio = tibble, transformation = updated tibble)
#
# Override tibbles all have the same shape: (customer_id, override_value).
# Pass NULL or an empty tibble (the default) for "no overrides" — matches the
# Excel tool's blank-override-row behaviour.
# ============================================================================


#' Apply the IFRS9 staging rule.
#'
#' Excel formula (Inputs_Lending Portfolio!N5):
#'   IF DPD > 90 -> "Stage 3"
#'   ELSE IF (restructuring_final = "Restructured" OR
#'            (DPD > dpd_stage2_threshold AND DPD <= 90) OR
#'            watchlist_status = "Watchlist") -> "Stage 2"
#'   ELSE "Stage 1"
#'
#' @param dpd                 Numeric vector — past_dues_worst per customer
#' @param restructuring_final chr vector — "Restructured" or ""
#' @param watchlist_status    chr vector — "Watchlist" or ""
#' @param dpd_stage2_threshold numeric scalar (default 60)
#' @return chr vector
apply_staging_rule <- function(dpd, restructuring_final, watchlist_status,
                               dpd_stage2_threshold) {
  out <- rep("Stage 1", length(dpd))
  is_restructured <- !is.na(restructuring_final) & restructuring_final == "Restructured"
  is_watchlist    <- !is.na(watchlist_status)    & watchlist_status    == "Watchlist"
  is_dpd_2        <- !is.na(dpd) & dpd > dpd_stage2_threshold & dpd <= 90
  out[is_restructured | is_watchlist | is_dpd_2] <- "Stage 2"
  out[!is.na(dpd) & dpd > 90] <- "Stage 3"
  out
}


#' Apply a final stage override.
#'
#' Excel: IF override = "" -> stage_pre; IF stage_pre = "Stage 3" OR
#'        override = "Stage 3" -> "Stage 3"; ELSE override
#'
#' i.e. the override can downgrade (Stage 1 -> 2) but cannot un-Stage-3
#' a default. (The "Backward Transition" name in Excel is misleading;
#' it actually only allows DOWNgrades.)
apply_stage_override <- function(stage_pre, stage_override) {
  out <- stage_pre
  has_override <- !is.na(stage_override) & nzchar(stage_override)
  out[has_override] <- stage_override[has_override]
  # Stage 3 sticks
  out[stage_pre == "Stage 3"] <- "Stage 3"
  out
}


#' Apply a 1-notch rating downgrade (used when global flag is set).
#'
#' Excel: VLOOKUP(rating, MasterRatingScale!J:K, 2)
#' If the rating isn't in the downgrade table the original is returned.
apply_one_notch <- function(rating, downgrade_tbl) {
  lkp <- setNames(downgrade_tbl$rating_after_1_notch_downgrade,
                  downgrade_tbl$rating)
  out <- lkp[rating]
  out[is.na(out)] <- rating[is.na(out)]
  unname(out)
}


#' Validate an overrides tibble.
#'
#' Returns a normalised tibble (customer_id chr, override_value chr) with
#' rows where override_value is NA or "" removed. NULL or zero-row input
#' returns a zero-row tibble.
normalise_overrides <- function(overrides, value_col) {
  if (is.null(overrides) || nrow(overrides) == 0) {
    return(tibble::tibble(customer_id = character(),
                          override_value = character()))
  }
  if (!"customer_id" %in% colnames(overrides)) {
    stop("overrides must have a 'customer_id' column")
  }
  if (!value_col %in% colnames(overrides)) {
    stop(sprintf("overrides must have a '%s' column", value_col))
  }
  out <- tibble::tibble(
    customer_id    = as.character(overrides$customer_id),
    override_value = as.character(overrides[[value_col]])
  )
  out <- out[!is.na(out$override_value) & nzchar(out$override_value), ]
  out
}


#' Build the Inputs_Lending Portfolio view.
#'
#' @param transformation          Output of build_transformation_lending()
#' @param inputs                  Output of read_all_inputs()
#' @param static                  Output of load_static_reference()
#' @param model_cfg               Output of load_model_config()
#' @param rating_overrides        Tibble (customer_id, rating_override) or NULL
#' @param restructuring_overrides Tibble (customer_id, restructuring_override) or NULL
#' @param stage_overrides         Tibble (customer_id, stage_override) or NULL
#' @param apply_one_notch_downgrade Logical scalar, default FALSE
#' @return list(portfolio = tibble, transformation = updated tibble)
build_lending_portfolio_view <- function(transformation,
                                          inputs,
                                          static,
                                          model_cfg,
                                          rating_overrides         = NULL,
                                          restructuring_overrides  = NULL,
                                          stage_overrides          = NULL,
                                          apply_one_notch_downgrade = FALSE) {

  # Default stage_overrides to the static-loaded customer_stage_overrides
  # table (Shiny will write user changes there before triggering a rerun).
  # Only rows with status == "approved" are applied — see active_overrides().
  if (is.null(stage_overrides) &&
      !is.null(static$customer_stage_overrides) &&
      nrow(static$customer_stage_overrides) > 0) {
    so <- active_overrides(static$customer_stage_overrides)
    if (!is.null(so) && nrow(so) > 0) {
      stage_overrides <- tibble::tibble(
        customer_id    = as.character(so$customer_id),
        stage_override = as.character(so$override_stage)
      )
    }
  }

  # Default restructuring_overrides similarly. Only approved rows apply.
  if (is.null(restructuring_overrides) &&
      !is.null(static$customer_restructuring_overrides) &&
      nrow(static$customer_restructuring_overrides) > 0) {
    ro <- active_overrides(static$customer_restructuring_overrides)
    if (!is.null(ro) && nrow(ro) > 0) {
      restructuring_overrides <- tibble::tibble(
        customer_id            = as.character(ro$customer_id),
        restructuring_override = as.character(ro$override_restructuring)
      )
    }
  }

  cm <- inputs$CustomerMaster
  if (is.null(cm) || nrow(cm) == 0) {
    stop("CustomerMaster is empty — cannot build customer view")
  }

  customer_id   <- as.character(cm$customer_id)
  customer_name <- as.character(cm$customer_name)

  # ---- Pass 4: per-customer aggregation ---------------------------------
  # First-row-per-customer view of Transformation
  trans_first <- transformation[!duplicated(transformation$customer_id), ]
  by_cust <- match(customer_id, trans_first$customer_id)

  # Exposure: SUMIF (Transformation.exposure_amount by customer_id)
  exposure_total <- tapply(transformation$exposure_amount,
                           transformation$customer_id, sum, na.rm = TRUE)
  exposure_total <- as.numeric(exposure_total[customer_id])
  exposure_total[is.na(exposure_total)] <- 0

  # Current rating = rating_worst per customer (Transformation!AC)
  rating_current <- trans_first$rating_worst[by_cust]

  # Restructuring tag: if any contract for this customer has is_restructured = 1
  restructuring_any <- tapply(transformation$is_restructured,
                              transformation$customer_id,
                              function(x) any(x == 1, na.rm = TRUE))
  restructuring_current <- ifelse(restructuring_any[customer_id],
                                  "Restructured", "")

  # Watchlist tag: if any contract flagged
  watchlist_any <- tapply(transformation$is_watchlist,
                          transformation$customer_id,
                          function(x) any(x == 1, na.rm = TRUE))
  watchlist_status <- ifelse(watchlist_any[customer_id], "Watchlist", "")

  # DPD status = worst_dpd per customer
  dpd_status <- trans_first$past_dues_worst[by_cust]
  dpd_status[is.na(dpd_status)] <- 0

  # ---- Pass 5: overrides + final values ---------------------------------
  ro <- normalise_overrides(rating_overrides, "rating_override")
  rating_override <- rep(NA_character_, length(customer_id))
  rating_override[match(ro$customer_id, customer_id)] <- ro$override_value
  rating_final <- ifelse(is.na(rating_override) | rating_override == "",
                         rating_current, rating_override)
  rating_override_status <- ifelse(
    is.na(rating_override) | rating_override == "",
    "No Rating Override",
    paste0("Rating Overridden from ", rating_current, " to ", rating_override)
  )

  ro2 <- normalise_overrides(restructuring_overrides, "restructuring_override")
  restructuring_override <- rep(NA_character_, length(customer_id))
  restructuring_override[match(ro2$customer_id, customer_id)] <- ro2$override_value
  # Excel: IF H="Restructured" AND I!="commercial reasons" -> Restructured else ""
  restructuring_final <- ifelse(
    restructuring_current == "Restructured" &
      (is.na(restructuring_override) |
        !grepl("commercial reasons", restructuring_override, ignore.case = TRUE)),
    "Restructured", ""
  )
  restructuring_override_status <- ifelse(
    restructuring_current == restructuring_final,
    "No Restructuring Override",
    "Restructuring Due to Commercial Reasons"
  )

  # Staging rule (pre-override)
  stage_pre <- apply_staging_rule(
    dpd                  = dpd_status,
    restructuring_final  = restructuring_final,
    watchlist_status     = watchlist_status,
    dpd_stage2_threshold = static$staging_thresholds$dpd_stage2_threshold_days
  )

  ro3 <- normalise_overrides(stage_overrides, "stage_override")
  stage_override <- rep(NA_character_, length(customer_id))
  stage_override[match(ro3$customer_id, customer_id)] <- ro3$override_value
  stage_final <- apply_stage_override(stage_pre, stage_override)
  stage_override_status <- ifelse(
    stage_pre == stage_final,
    "No Staging Override",
    paste0("Staging Overridden from ", stage_pre, " to ", stage_override)
  )

  # ---- Assemble portfolio view ------------------------------------------
  portfolio <- tibble::tibble(
    customer_id                  = customer_id,
    customer_name                = customer_name,
    exposure_total               = exposure_total,
    rating_current               = rating_current,
    rating_override              = rating_override,
    rating_final                 = rating_final,
    rating_override_status       = rating_override_status,
    restructuring_current        = restructuring_current,
    restructuring_override       = restructuring_override,
    restructuring_final          = restructuring_final,
    restructuring_override_status = restructuring_override_status,
    watchlist_status             = watchlist_status,
    dpd_status                   = dpd_status,
    stage_pre_override           = stage_pre,
    stage_override               = stage_override,
    stage_final                  = stage_final,
    stage_override_status        = stage_override_status
  )

  # ---- Pass 6: back-fill override-dependent Transformation columns -------
  cust_lkp <- function(field) {
    setNames(portfolio[[field]], portfolio$customer_id)[transformation$customer_id]
  }

  rating_after_override <- unname(cust_lkp("rating_final"))

  if (apply_one_notch_downgrade) {
    rating_with_downgrade <- apply_one_notch(rating_after_override,
                                             static$master_rating_downgrade)
  } else {
    rating_with_downgrade <- rating_after_override
  }

  is_stage2_final        <- as.integer(unname(cust_lkp("stage_final")) == "Stage 2")
  is_default_final       <- as.integer(unname(cust_lkp("stage_final")) == "Stage 3")
  is_restructured_final  <- as.integer(unname(cust_lkp("restructuring_final")) == "Restructured")

  transformation$rating_after_override   <- rating_after_override
  transformation$rating_with_downgrade   <- rating_with_downgrade
  transformation$is_default_final        <- is_default_final
  transformation$is_restructured_final   <- is_restructured_final
  transformation$is_stage2               <- is_stage2_final

  list(portfolio = portfolio, transformation = transformation)
}
