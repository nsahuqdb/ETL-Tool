# ============================================================================
# tests/manual/test_phase_h1.R
#
# Smoke test for Phase H1 — validation framework + input validators.
#
# Run from project root:
#     setwd("path/to/ifrs9_etl")
#     source("tests/manual/test_phase_h1.R")
#
# What it does:
#   1. Loads inputs and static reference (Phases B/C).
#   2. Builds the input validator suite via build_input_validators().
#   3. Runs the suite via run_validation_suite("INPUT", ...).
#   4. Prints a per-validator status line and a summary.
#
# What "PASS" looks like:
#   - All ERROR-severity validators pass
#   - WARN/INFO failures are tolerated; they're logged for inspection
# ============================================================================

source("R/io_helpers.R")
source("R/load_config.R")
source("R/load_static.R")
source("R/read_inputs.R")
source("R/validation.R")
source("R/validators_input.R")


cat("\n========== PHASE H1 :: setup ==========\n")
run_cfg <- load_run_config("config.yml")
static  <- load_static_reference(run_cfg$paths$static_dir)
inputs  <- read_all_inputs(run_cfg$paths$input_dir, verbose = FALSE)

cat(sprintf("  loaded %d input files, %d static reference items\n",
            length(inputs), length(static)))


# ---------------------------------------------------------------------------
# Run input validators
# ---------------------------------------------------------------------------
validators <- build_input_validators()
cat(sprintf("  built %d input validators\n", length(validators)))

results <- run_validation_suite(
  "INPUT", validators,
  args = list(inputs = inputs, static = static, run_cfg = run_cfg),
  verbose = TRUE
)


# ---------------------------------------------------------------------------
# Severity rollup
# ---------------------------------------------------------------------------
cat("\n========== PHASE H1 SUMMARY ==========\n")
n_total <- nrow(results)
n_pass  <- sum(results$passed)
n_err   <- sum(!results$passed & results$severity == "ERROR")
n_warn  <- sum(!results$passed & results$severity == "WARN")
n_info  <- sum(!results$passed & results$severity == "INFO")

cat(sprintf("  %d/%d validators passed\n", n_pass, n_total))
cat(sprintf("    ERROR failures: %d\n", n_err))
cat(sprintf("    WARN  failures: %d\n", n_warn))
cat(sprintf("    INFO  failures: %d\n", n_info))

if (n_err == 0) {
  cat("\n  PHASE H1 :: PASS  (no ERROR-severity failures)\n")
} else {
  cat("\n  PHASE H1 :: FAIL\n")
  failed <- results$id[!results$passed & results$severity == "ERROR"]
  cat(sprintf("  Failed (ERROR): %s\n", paste(failed, collapse = ", ")))
}

# Make results available for inspection
.last_validation_results <- results
cat("\n  Results stored in .last_validation_results for inspection.\n")
