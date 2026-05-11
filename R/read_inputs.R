# ============================================================================
# R/read_inputs.R
#
# Per-file readers for the 12 IFRSIN inputs, plus an orchestrator
# read_all_inputs() that returns a named list of tibbles.
#
# Mapping to the Excel tool:
#   read_all_inputs()  <==>  ExtractData() macro in mod1.txt, which calls
#   the 12 *Extract subs (AccountMasterExtract, CustomerMasterExtract, ...).
#   Each *Extract sub does: open the file from \\mvlicb01\IFRSIN\, copy the
#   used range, paste-values into the *Extract sheet. We do the equivalent
#   here: read the file, return a tibble.
#
# Column names are kept as found in the source. Some files (the SQL*Plus
# HTML exports especially) have cryptic / single-letter column names because
# the upstream SQL aliases were short. That is fine for now — the
# transformation layer in Phase D references columns by position via a
# mapping table, just like the Excel tool's downstream sheets do.
# ============================================================================


# Each entry: name -> list(file, expected_format, description).
# expected_format is informational only — actual reading uses content
# detection so a misnamed file still works.
INPUT_FILE_SPECS <- list(
  AccountMaster                   = list(file = "AccountMaster.xlsx",
                                         expected_format = "xlsx",
                                         description = "Lending portfolio account-level master"),
  AccountMasterInvestments        = list(file = "AccountMasterInvestments.xlsx",
                                         expected_format = "xlsx",
                                         description = "Investment portfolio account-level master"),
  CustomerMaster                  = list(file = "CustomerMaster.xlsx",
                                         expected_format = "xlsx",
                                         description = "Lending portfolio customer-level master"),
  CustomerMasterInvestments       = list(file = "CustomerMasterInvestments.xls",
                                         expected_format = "html",
                                         description = "Investment portfolio customer-level master (SQL*Plus HTML)"),
  CustomerStagingFlag             = list(file = "CustomerStagingFlag.xlsx",
                                         expected_format = "xlsx",
                                         description = "Lending portfolio staging flags (watchlist, default, ...)"),
  CustomerStagingFlagInvestments  = list(file = "CustomerStagingFlagInvestments.xls",
                                         expected_format = "html",
                                         description = "Investment portfolio staging flags (SQL*Plus HTML)"),
  Collateral                      = list(file = "Collateral.xlsx",
                                         expected_format = "xlsx",
                                         description = "Collateral master"),
  AccountCollateralAllocation     = list(file = "AccountCollateralAllocation.xlsx",
                                         expected_format = "xlsx",
                                         description = "Allocation of collateral to contracts"),
  RepaymentSchedule               = list(file = "RepaymentSchedule.xlsx",
                                         expected_format = "xlsx",
                                         description = "Per-contract repayment schedule (drives EAD curve)"),
  IndustryCode                    = list(file = "IndustryCode.xls",
                                         expected_format = "html",
                                         description = "Customer -> industry code mapping (SQL*Plus HTML)"),
  Origination                     = list(file = "Origination.xls",
                                         expected_format = "html",
                                         description = "Lending portfolio origination snapshot (SQL*Plus HTML)"),
  OriginationInvestments          = list(file = "OriginationInvestments.xls",
                                         expected_format = "html",
                                         description = "Investment portfolio origination snapshot (SQL*Plus HTML)")
)


#' Read all 12 IFRSIN input files into a named list of tibbles.
#'
#' Each file is read raw (preserving original column names from the
#' source XLSX/HTML/CSV), then has its column schema applied — see
#' R/input_schemas.R::INPUT_SCHEMAS. After this step, every tibble has
#' canonical lowercase snake_case column names with explicit types,
#' regardless of how readxl named or typed the raw columns.
#'
#' Files that do not exist are returned as NULL with a warning so the
#' caller can decide whether to abort or continue.
#'
#' @param input_dir Path to the IFRSIN folder
#' @param verbose   If TRUE (default), prints progress with message().
#' @param apply_schema If TRUE (default), apply INPUT_SCHEMAS to rename
#'                  + retype columns. Set FALSE to inspect raw output.
#' @return Named list of length(INPUT_FILE_SPECS).
read_all_inputs <- function(input_dir, verbose = TRUE, apply_schema = TRUE) {
  if (!dir.exists(input_dir)) {
    stop("Input directory does not exist: ", input_dir)
  }

  result <- vector("list", length(INPUT_FILE_SPECS))
  names(result) <- names(INPUT_FILE_SPECS)

  for (key in names(INPUT_FILE_SPECS)) {
    spec <- INPUT_FILE_SPECS[[key]]
    path <- file.path(input_dir, spec$file)

    if (!file.exists(path)) {
      warning(sprintf("Input file not found: %s -> NULL", path), call. = FALSE)
      result[[key]] <- NULL
      next
    }

    if (verbose) message(sprintf("Reading %-32s  (%s)", spec$file, spec$expected_format))
    raw <- tryCatch(
      read_input_file(path),
      error = function(e) {
        warning(sprintf("Failed to read %s: %s", spec$file, conditionMessage(e)),
                call. = FALSE)
        NULL
      }
    )

    if (is.null(raw)) {
      result[[key]] <- NULL
      next
    }

    if (apply_schema && !is.null(INPUT_SCHEMAS[[key]])) {
      result[[key]] <- tryCatch(
        apply_input_schema(raw, INPUT_SCHEMAS[[key]],
                           file_label = spec$file),
        error = function(e) {
          warning(sprintf("Schema apply failed for %s: %s",
                          spec$file, conditionMessage(e)), call. = FALSE)
          # Preserve raw so caller can debug
          raw
        }
      )
    } else {
      result[[key]] <- raw
    }
  }

  result
}


#' One-line summary of read_all_inputs() result. Useful for logging.
#'
#' @param inputs Output of read_all_inputs()
#' @return Tibble with one row per input file: name, rows, cols, status
summarise_inputs <- function(inputs) {
  rows <- lapply(names(INPUT_FILE_SPECS), function(key) {
    df <- inputs[[key]]
    if (is.null(df)) {
      tibble::tibble(name = key, rows = NA_integer_, cols = NA_integer_,
                     status = "missing")
    } else {
      tibble::tibble(name = key,
                     rows = nrow(df),
                     cols = ncol(df),
                     status = "ok")
    }
  })
  dplyr::bind_rows(rows)
}
