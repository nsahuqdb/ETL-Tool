# ============================================================================
# R/lifetime_parameter_other.R
#
# Build the LifeTimeParameterOther output table — one row per
# (contract, month_remaining), giving the monthly EAD curve.
#
# Source: RepaymentSchedule input file + AccountMaster (for current exposure).
#
# Replicates the Excel `RepaymentScheduleTransform` sheet logic + pivot table:
#   1. Per scheduled payment, compute EAD = BALANCE + REPAYMENT (the balance
#      *before* this payment is applied).
#   2. Compute MonthLifetime = months from reporting date to START_DATE.
#   3. Per contract, find max MonthLifetime (call it `end_month`).
#   4. For each m in 0..(end_month-1):
#      - If m < first_schedule_month: EAD = current exposure (AccountMaster.ONBALANCE)
#      - Else: EAD = (BALANCE+REPAYMENT) at the *first* scheduled payment with
#              MonthLifetime > m.
#
# Public API:
#   build_lifetime_parameter_other(repayment_schedule, transformation, run_cfg)
#     -> tibble (extract_date, contract_id, month_lifetime, ead_lifetime,
#                lgd_lifetime, payment_schedule_lifetime, total_limit_lifetime)
#
# Note: lgd_lifetime, payment_schedule_lifetime, total_limit_lifetime are
# always blank in the Excel output (seem reserved for a different
# product/IFRS calc engine downstream). We keep them as NA columns to match
# the schema.
# ============================================================================


#' Build the LifeTimeParameterOther output table.
#'
#' @param repayment_schedule  tibble — RepaymentSchedule input
#'        (cols KEY_1, POST_DATE, START_DAT, PRINCE_DUE, PROJ_INT,
#'         REPAYMENT, BALANCE)
#' @param transformation      tibble — output of build_transformation_lending(),
#'        used for current exposure (column $exposure_amount keyed by
#'        $contract_id)
#' @param run_cfg             output of load_run_config()
#' @return tibble with cols (extract_date, contract_id, month_lifetime,
#'         ead_lifetime, lgd_lifetime, payment_schedule_lifetime,
#'         total_limit_lifetime). Long format, one row per
#'         (contract, month).
build_lifetime_parameter_other <- function(repayment_schedule,
                                            transformation,
                                            run_cfg) {

  if (is.null(repayment_schedule) || nrow(repayment_schedule) == 0) {
    return(empty_lifetime_parameter_other())
  }

  reporting_dt <- normalise_extract_date(run_cfg$run$extract_date)
  if (is.na(reporting_dt)) {
    stop("run_cfg$run$extract_date is invalid")
  }

  # ---- Stage 1: per-payment EAD + MonthLifetime -------------------------
  # Use position-based access since the export uses cryptic short names:
  #   col 1 = KEY_1 (contract id)
  #   col 3 = START_DAT (next payment date)
  #   col 6 = REPAYMENT (this period's payment amount)
  #   col 7 = BALANCE (outstanding after this payment)
  # Schema canonical columns (see INPUT_SCHEMAS$RepaymentSchedule):
  #   contract_id, post_date, start_date, principal_due, projected_interest,
  #   repayment, balance.
  rs <- tibble::tibble(
    contract_id = as.character(repayment_schedule$contract_id),
    start_date  = normalise_extract_date(repayment_schedule$start_date),
    repayment   = as.numeric(repayment_schedule$repayment),
    balance     = as.numeric(repayment_schedule$balance)
  )
  # Drop rows where contract_id is NA (the extract may have trailing blanks)
  rs <- rs[!is.na(rs$contract_id) & nzchar(rs$contract_id), ]
  rs <- rs[!is.na(rs$start_date), ]

  rs$repayment[is.na(rs$repayment)] <- 0
  rs$balance[is.na(rs$balance)]     <- 0

  # Excel: EAD = BALANCE + REPAYMENT
  rs$ead <- rs$balance + rs$repayment

  # MonthLifetime per Excel:
  # =(YEAR(start) - YEAR(reporting))*12 + MONTH(start) - MONTH(reporting)
  rs$month_lifetime <-
    (lubridate::year(rs$start_date)  - lubridate::year(reporting_dt))  * 12 +
    (lubridate::month(rs$start_date) - lubridate::month(reporting_dt))

  # Drop placeholder historical schedule entries (e.g., START_DAT = 1930-01-01)
  # that produce negative month_lifetime values. These are common when the
  # source system uses a sentinel date for the final balloon payment of a
  # fully-amortising loan — keeping them confuses the first_month
  # calculation. Note we keep month_lifetime == 0 entries: those are real
  # payments due in the current reporting period.
  rs <- rs[rs$month_lifetime >= 0, ]

  # Sort by contract, then by month_lifetime (so first row per contract is
  # the earliest-month payment)
  rs <- rs[order(rs$contract_id, rs$month_lifetime), ]

  # ---- Stage 2: per-contract end_month + first_schedule_month -----------
  # end_month per Excel pivot = MAX(MonthLifetime). first_schedule_month =
  # MIN (smallest schedule month for this contract).
  agg <- aggregate(
    cbind(end_month = rs$month_lifetime, first_month = rs$month_lifetime) ~
      contract_id, data = rs,
    FUN = function(x) c(max(x), min(x))
  )
  # aggregate returns a matrix in each col — flatten
  end_first <- do.call(rbind, lapply(seq_len(nrow(agg)), function(i) {
    c(end = agg$end_month[i, 1], first = agg$first_month[i, 2])
  }))
  agg <- tibble::tibble(
    contract_id = agg$contract_id,
    end_month   = as.integer(end_first[, "end"]),
    first_month = as.integer(end_first[, "first"])
  )

  # Keep only contracts with strictly-positive end_month — defensive guard
  # in case any contract has all-historic schedules.
  agg <- agg[agg$end_month > 0, ]
  rs  <- rs[rs$contract_id %in% agg$contract_id, ]

  # ---- Stage 3: build long-format per-month table -----------------------
  # For each contract: months are 0..(end_month-1).
  # EAD lookup at month m:
  #   if m < first_month: current exposure
  #   else: ead at the first schedule row with month_lifetime > m
  exposure_lkp <- setNames(transformation$exposure_amount,
                           transformation$contract_id)

  out_chunks <- vector("list", nrow(agg))

  for (i in seq_len(nrow(agg))) {
    cid          <- agg$contract_id[i]
    end_m        <- agg$end_month[i]
    first_m      <- agg$first_month[i]

    sub <- rs[rs$contract_id == cid, ]
    sched_months <- sub$month_lifetime
    sched_eads   <- sub$ead

    months <- 0:(end_m - 1)
    eads   <- numeric(length(months))

    cur_exp <- exposure_lkp[[cid]]
    if (is.null(cur_exp) || is.na(cur_exp)) cur_exp <- 0

    for (m_idx in seq_along(months)) {
      m <- months[m_idx]
      # Excel formula at RepaymentScheduleTransform!T4:
      #   IF R=0:                       use current exposure (always)
      #   IF R < first_schedule_month:  use current exposure
      #   ELSE:                         look up schedule at next entry > m
      # The R=0 special case matters when a contract has placeholder
      # schedule entries with bogus historical dates (negative
      # month_lifetime), which would make first_schedule_month < 0 and
      # bypass the second clause.
      if (m == 0L || m < first_m) {
        eads[m_idx] <- cur_exp
      } else {
        # First scheduled payment with month_lifetime > m
        nxt <- which(sched_months > m)
        if (length(nxt) == 0L) {
          # Beyond all schedules — fall back to last EAD (defensive)
          eads[m_idx] <- sched_eads[length(sched_eads)]
        } else {
          eads[m_idx] <- sched_eads[nxt[1]]
        }
      }
    }

    out_chunks[[i]] <- tibble::tibble(
      extract_date              = reporting_dt,
      contract_id               = cid,
      month_lifetime            = as.integer(months),
      ead_lifetime              = eads,
      lgd_lifetime              = NA_real_,
      payment_schedule_lifetime = NA_real_,
      total_limit_lifetime      = NA_real_
    )
  }

  dplyr::bind_rows(out_chunks)
}


#' Empty schema for LifeTimeParameterOther.
empty_lifetime_parameter_other <- function() {
  tibble::tibble(
    extract_date              = as.Date(character()),
    contract_id               = character(),
    month_lifetime            = integer(),
    ead_lifetime              = double(),
    lgd_lifetime              = double(),
    payment_schedule_lifetime = double(),
    total_limit_lifetime      = double()
  )
}
