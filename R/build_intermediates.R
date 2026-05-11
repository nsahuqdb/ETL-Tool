# =============================================================================
# build_intermediates.R
#
# Pure builder functions for intermediate tibbles that sit between the
# transformation step and the output writers. These were previously inlined
# in tests/manual/test_phase_h5.R; moving them into one module so the
# orchestrator (run_etl) and Shiny share the same code path.
#
# Each function takes its inputs explicitly — no env lookups, no global state
# — so they're trivially unit-testable and Shiny-reactive.
# =============================================================================


# ---------------------------------------------------------------------------
# Customer-level flags table (used by CustomerStagingFlag_1 writer)
# ---------------------------------------------------------------------------
#'
#' Rebuilds the per-customer flag table from the lending portfolio view and
#' the raw CustomerStagingFlag input. Schema canonical columns are assumed
#' (i.e. inputs already passed through apply_input_schema).
#'
#' Output columns:
#'   customer_id, is_default, is_watchlist, is_local1, is_local3
#'
#' is_default   - cm_view$stage_final == "Stage 3"
#' is_watchlist - direct from input (col D, ISWATCHLIST)
#' is_local1    - cm_view$restructuring_final == "Restructured"
#' is_local3    - cm_view$stage_final == "Stage 2" (computed staging proxy
#'                for V4's Inputs_Lending Portfolio col P override)
#'
#' Other 11-column flag fields (is_insolvency, is_default_in_gcc, is_local2,
#' is_local4..6) are left blank by the writer to match the V4 bundle convention.
build_customer_flags <- function(cm_view, csf_raw) {
  if (is.null(cm_view) || nrow(cm_view) == 0) {
    return(tibble::tibble(
      customer_id  = character(),
      is_default   = logical(),
      is_watchlist = logical(),
      is_local1    = logical(),
      is_local3    = logical()
    ))
  }

  # Watchlist: lookup from raw CSF input by customer_id
  if (!is.null(csf_raw) && nrow(csf_raw) > 0) {
    watchlist_lkp <- setNames(
      as.logical(as.integer(csf_raw$is_watchlist) == 1L),
      as.character(csf_raw$customer_id)
    )
    is_watchlist <- watchlist_lkp[as.character(cm_view$customer_id)]
    is_watchlist[is.na(is_watchlist)] <- FALSE
  } else {
    is_watchlist <- rep(FALSE, nrow(cm_view))
  }

  tibble::tibble(
    customer_id  = cm_view$customer_id,
    is_default   = !is.na(cm_view$stage_final) &
                    cm_view$stage_final == "Stage 3",
    is_watchlist = is_watchlist,
    is_local1    = !is.na(cm_view$restructuring_final) &
                    cm_view$restructuring_final == "Restructured",
    is_local3    = !is.na(cm_view$stage_final) &
                    cm_view$stage_final == "Stage 2"
  )
}


# ---------------------------------------------------------------------------
# Collateral tibble — schema-typed pass-through
# ---------------------------------------------------------------------------
#' Inputs assumed schema-applied (canonical columns: collateral_id,
#' collateral_type_id, currency, value).
#'
#' This is mostly a pass-through plus a defensive currency fallback to "QAR"
#' when readxl returns the column all-NA.
build_collateral_tbl <- function(coll_raw) {
  if (is.null(coll_raw) || nrow(coll_raw) == 0) {
    return(tibble::tibble(
      collateral_id      = character(),
      collateral_type_id = integer(),
      currency           = character(),
      value              = numeric()
    ))
  }
  out <- tibble::tibble(
    collateral_id      = as.character(coll_raw$collateral_id),
    collateral_type_id = as.integer(coll_raw$collateral_type_id),
    currency           = as.character(coll_raw$currency),
    value              = as.numeric(coll_raw$value)
  )
  if (all(is.na(out$currency) | !nzchar(out$currency))) {
    out$currency <- rep("QAR", nrow(out))
  }
  out
}


# ---------------------------------------------------------------------------
# AccountCollateralAllocation tibble — schema-typed pass-through
# ---------------------------------------------------------------------------
build_account_collateral_allocation_tbl <- function(acoll_raw) {
  if (is.null(acoll_raw) || nrow(acoll_raw) == 0) {
    return(tibble::tibble(
      collateral_id         = character(),
      contract_id           = character(),
      allocation_percentage = numeric()
    ))
  }
  tibble::tibble(
    collateral_id         = as.character(acoll_raw$collateral_id),
    contract_id           = as.character(acoll_raw$contract_id),
    allocation_percentage = as.numeric(acoll_raw$allocation_percentage)
  )
}


# ---------------------------------------------------------------------------
# Investment customers tibble — single-column for now, kept as a function
# in case we add columns later
# ---------------------------------------------------------------------------
build_investment_customers <- function(trans_i) {
  if (is.null(trans_i) || nrow(trans_i) == 0) {
    return(tibble::tibble(customer_id_inv = character()))
  }
  tibble::tibble(customer_id_inv = trans_i$customer_id_inv)
}
