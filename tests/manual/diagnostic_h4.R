# ============================================================================
# tests/manual/diagnostic_h4.R
#
# Phase H4 diagnostic — characterize the 134 contracts present in the R
# port's AccountMaster_1 output but missing from the bundled
# Output/AccountMaster_1.csv.
#
# The goal is to identify the filter pattern(s) the V4 workbook applies in
# its AccountMasterLoad sheet so we can replicate them.
#
# Run from project root:
#     setwd("path/to/ifrs9_etl")
#     source("tests/manual/diagnostic_h4.R")
# ============================================================================

source("R/io_helpers.R")
source("R/load_config.R")
source("R/load_static.R")
source("R/read_inputs.R")
source("R/transform_lending.R")
source("R/lending_portfolio_view.R")
source("R/output_writers.R")


cat("\n========== H4 diagnostic :: setup ==========\n")
run_cfg   <- load_run_config("config.yml")
model_cfg <- load_model_config(run_cfg$paths$model_config)
static    <- load_static_reference(run_cfg$paths$static_dir)
inputs    <- read_all_inputs(run_cfg$paths$input_dir, verbose = FALSE)


# ---------------------------------------------------------------------------
# Build the lending transformation + portfolio view (the inputs to AM_1 writer)
# ---------------------------------------------------------------------------
trans_l <- build_transformation_lending(inputs, static, model_cfg, run_cfg)
out_l   <- build_lending_portfolio_view(trans_l, inputs, static, model_cfg)
cm_view <- out_l$portfolio
trans_l <- out_l$transformation

cat(sprintf("  trans_l rows: %d  (R-port AM_1 candidate count)\n", nrow(trans_l)))


# ---------------------------------------------------------------------------
# Load bundled AccountMaster_1.csv to compare contract IDs
# ---------------------------------------------------------------------------
bundle_path <- NULL
candidates <- c(
  "../output_files/Output/AccountMaster_1.csv",
  "../Output/AccountMaster_1.csv",
  "Output/AccountMaster_1.csv",
  "reference/Output/AccountMaster_1.csv"
)
for (p in candidates) if (file.exists(p)) { bundle_path <- p; break }
if (is.null(bundle_path)) {
  stop("Could not locate bundled AccountMaster_1.csv. Searched: ",
       paste(candidates, collapse=", "))
}
cat(sprintf("  bundle path: %s\n", bundle_path))

bundle <- read.csv(bundle_path, stringsAsFactors = FALSE, check.names = FALSE)
bundle_cids <- as.character(bundle$ContractId)
cat(sprintf("  bundle rows: %d, distinct ContractIds: %d\n",
            nrow(bundle), length(unique(bundle_cids))))


# ---------------------------------------------------------------------------
# Determine R-port ContractId → bundle ContractId mapping
# (V4 transforms FGG → 3 etc. via assumption table)
# ---------------------------------------------------------------------------
# Apply the same id-substitution rule the writer uses. The writer outputs
# `contract_id` (already substituted), so trans_l$contract_id should match
# bundle directly.
rport_cids  <- as.character(trans_l$contract_id)
extra_cids  <- setdiff(rport_cids, bundle_cids)   # the ones we filter or shouldn't have

cat(sprintf("\n  R-port has %d contracts NOT in bundle (the 134 to characterize)\n",
            length(extra_cids)))
cat(sprintf("  R-port also missing %d contracts that ARE in bundle\n",
            length(setdiff(bundle_cids, rport_cids))))

if (length(extra_cids) == 0) {
  cat("  No gap! R port matches bundle.\n")
  return(invisible(NULL))
}


# ---------------------------------------------------------------------------
# Profile the extras
# ---------------------------------------------------------------------------
extras <- trans_l[trans_l$contract_id %in% extra_cids, ]
cat("\n========== EXTRA CONTRACT PROFILE ==========\n")

# 1. Account-type breakdown
cat("\n--- account_type counts ---\n")
print(sort(table(extras$account_type), decreasing = TRUE))

# 2. ContractId raw vs substituted (look for short numeric ids)
if ("contract_id_raw" %in% names(extras)) {
  raw <- extras$contract_id_raw
} else {
  raw <- extras$contract_id
}
cat("\n--- contract_id length distribution ---\n")
print(table(nchar(as.character(raw))))

# 3. OpenDate distribution
if ("open_date" %in% names(extras)) {
  cat("\n--- open_date distribution (top values) ---\n")
  od_vals <- as.character(extras$open_date)
  print(head(sort(table(od_vals), decreasing = TRUE), 10))
}

# 4. MaturityDate distribution
if ("maturity_date" %in% names(extras)) {
  cat("\n--- maturity_date distribution (top values) ---\n")
  md_vals <- as.character(extras$maturity_date)
  print(head(sort(table(md_vals), decreasing = TRUE), 10))
}

# 5. exposure
if ("exposure_amount" %in% names(extras)) {
  cat("\n--- exposure_amount summary ---\n")
  print(summary(extras$exposure_amount))
}

# 6. Sample 20 rows
cat("\n--- sample 20 extras (key columns) ---\n")
keep_cols <- intersect(c("contract_id", "contract_id_raw", "customer_id",
                          "account_type", "open_date", "maturity_date",
                          "exposure_amount", "rating", "past_dues_days"),
                        names(extras))
print(head(extras[, keep_cols], 20))


# 7. Check if extras have OpenDate matching the extract date (open in same period)
if ("open_date" %in% names(extras)) {
  extract_dt <- run_cfg$run$extract_date
  cat(sprintf("\n--- extract date: %s ---\n", as.character(extract_dt)))
  od_pst <- as.Date(extras$open_date)
  cat(sprintf("  extras with open_date == extract_date: %d\n",
              sum(od_pst == as.Date(extract_dt), na.rm = TRUE)))
  cat(sprintf("  extras with open_date > extract_date - 30d: %d\n",
              sum(od_pst > (as.Date(extract_dt) - 30), na.rm = TRUE)))
}


# 8. Save full extras list for inspection
out_path <- "test_output/h4_extras.csv"
dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
write.csv(extras[, keep_cols], out_path, row.names = FALSE)
cat(sprintf("\n  Full %d-row extras dumped to %s\n", nrow(extras), out_path))
