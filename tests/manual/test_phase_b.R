# ============================================================================
# tests/manual/test_phase_b.R
#
# Smoke test for Phase B (input readers).
#
# Run from the project root:
#
#     setwd("path/to/ifrs9_etl")
#     source("tests/manual/test_phase_b.R")
#
# Pre-requisites:
#   - The sample IFRSIN files are in `input/` (the zip we shipped already
#     places them there).
#   - The R packages in DESCRIPTION are installed:
#         install.packages(c("readxl","rvest","xml2","dplyr","tibble",
#                            "readr","yaml"))
#
# What it does:
#   1. Sources the three Phase-B R files
#   2. Calls read_all_inputs() against ./input/
#   3. Prints a one-line-per-file summary (rows x cols, column heads)
#   4. Runs validate_inputs() and prints any issues
#
# What "PASS" looks like:
#   12 input files all read with non-zero rows, validate_inputs() returns
#   an empty tibble, no warnings about missing files.
# ============================================================================

source("R/io_helpers.R")
source("R/read_inputs.R")
source("R/validate_inputs.R")

cat("========== PHASE B :: read_all_inputs ==========\n\n")
inputs <- read_all_inputs("input")

cat("\n========== INPUT SUMMARY ==========\n")
summary_df <- summarise_inputs(inputs)
print(summary_df, n = Inf)

cat("\n========== COLUMN HEADS (first 8 cols of each) ==========\n")
for (name in names(inputs)) {
  df <- inputs[[name]]
  if (is.null(df)) {
    cat(sprintf("\n%-32s : <missing>\n", name))
    next
  }
  cat(sprintf("\n%-32s : %d x %d\n", name, nrow(df), ncol(df)))
  cat(sprintf("    cols : %s%s\n",
              paste(head(colnames(df), 8), collapse = ", "),
              if (ncol(df) > 8) paste0(", ...(+", ncol(df) - 8, " more)") else ""))
  if (nrow(df) > 0) {
    sample_row <- as.character(df[1, seq_len(min(6, ncol(df)))])
    sample_row <- substr(sample_row, 1, 18)
    cat(sprintf("    row1 : %s\n", paste(sample_row, collapse = " | ")))
  }
}

cat("\n\n========== VALIDATION ==========\n")
issues <- validate_inputs(inputs)
if (nrow(issues) == 0) {
  cat("PASS - all 12 inputs read cleanly, no issues found.\n")
} else {
  cat(sprintf("Found %d issue(s):\n", nrow(issues)))
  print(issues, n = Inf)
}
