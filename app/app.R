# =============================================================================
# app/app.R
#
# Read-only Shiny app for browsing IFRS9 ETL pipeline runs, snapshots,
# validation findings, audit log, and outputs.
#
# Architecture:
#   - This file launches the app; it is intentionally tiny.
#   - UI and server are split into per-page modules under app/modules/.
#   - The pipeline R/ code is sourced at startup so the app can call
#     run_discovery, snapshots, audit_log, and load_static helpers
#     without re-implementing them.
#
# Run locally:
#   setwd("path/to/ifrs9_etl")
#   shiny::runApp("app")
#
# Deployment (Posit Connect / Shiny Server):
#   The app expects the project root as the working directory at runtime
#   so relative paths (`R/`, `runs/`, `config_snapshots/`, `logs/`) resolve.
#   On Posit Connect, deploy the entire project; the server will set CWD
#   to the deployment root automatically.
#
# Configuration (all optional, with sensible defaults):
#   options(ifrs9.runs_dir       = "runs")            # where run_etl writes
#   options(ifrs9.snapshots_dir  = "config_snapshots")
#   options(ifrs9.audit_log      = "logs/etl_audit.jsonl")
# =============================================================================

# ---- Required packages --------------------------------------------------
# NOTE: every package the app (or any sourced R/ file) needs MUST be
# library()'d explicitly here so Posit Connect's rsconnect::writeManifest
# detects them. The static analyzer in rsconnect only follows library()
# and pkg::fun() references in files it scans. Our R/ pipeline code is
# loaded via dynamic source() in source_pipeline() — those files are not
# walked. So any `readr::*`, `readxl::*`, `rvest::*` references inside
# R/ would be invisible to rsconnect without explicit library() calls
# here. Keep this list in sync with R/*.R imports.
required_pkgs <- c("shiny", "bslib", "DT", "jsonlite", "yaml", "tibble",
                   "markdown", "readr", "readxl", "rvest", "xml2",
                   "lubridate")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace,
                                      logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Missing packages required by the Shiny app: ",
       paste(missing_pkgs, collapse = ", "),
       "\nInstall with:  install.packages(c(",
       paste(sprintf("'%s'", missing_pkgs), collapse = ", "),
       "))")
}

library(shiny)
library(bslib)
library(DT)
library(jsonlite)
library(yaml)
library(tibble)
library(markdown)
library(readr)
library(readxl)
library(rvest)
library(xml2)
library(lubridate)


# ---- Resolve project root + source pipeline -----------------------------
# Pipeline modules expose helpers used by the app's modules:
#   list_runs, read_run_manifest, read_run_validation,
#   read_run_reconciliation, list_run_outputs   (run_discovery.R)
#   list_snapshots, read_snapshot_metadata, snapshot_paths,
#   diff_snapshots                              (snapshots.R)
#   read_audit_log                              (audit_log.R)
.app_root <- function() {
  # When run via shiny::runApp("app"), cwd is project root.
  # When run via runApp() from inside app/, cwd is app/. Detect both.
  if (file.exists("R/run_etl.R")) return(getwd())
  if (file.exists("../R/run_etl.R")) return(normalizePath(".."))
  stop("Cannot locate project root. Run shiny::runApp('app') from the ",
       "ifrs9_etl/ project root.")
}
.project_root <- .app_root()

# Pin every "where to look" path to absolute paths derived from project_root.
# Without this, a relative path like "runs" resolves against whatever
# getwd() is at the moment the module reads it — which differs between
# Rscript, RStudio, shiny::runApp(), and Posit Connect. Setting the
# options here once at startup makes the rest of the app cwd-independent
# for the helpers we control.
options(
  ifrs9.project_root   = .project_root,
  ifrs9.runs_dir       = file.path(.project_root, "runs"),
  ifrs9.snapshots_dir  = file.path(.project_root, "config_snapshots"),
  ifrs9.audit_log      = file.path(.project_root, "logs", "etl_audit.jsonl")
)

# ---- File upload limit -----------------------------------------------------
# Default Shiny upload cap is 5 MB, way too small for an IFRS9 input
# bundle (the AccountCollateralAllocation and RepaymentSchedule xlsx
# files alone routinely run tens of MB each). We default to 500 MB and
# let a deployer override via config.yml::run.max_upload_size_mb.
local({
  size_mb <- 500
  cfg_path <- file.path(.project_root, "config.yml")
  if (file.exists(cfg_path)) {
    cfg <- tryCatch(yaml::read_yaml(cfg_path), error = function(e) NULL)
    cfg_size <- cfg$run$max_upload_size_mb
    if (!is.null(cfg_size) &&
        is.numeric(cfg_size) &&
        cfg_size > 0) {
      size_mb <- cfg_size
    }
  }
  options(shiny.maxRequestSize = size_mb * 1024 * 1024)
  message(sprintf("[app] upload size limit: %d MB", size_mb))
})

# Move the working directory to the project root for the duration of the
# Shiny session. Several pipeline functions use bare relative paths (e.g.
# `config_path = "config.yml"` is the default for run_etl) and aren't
# parameterised on a project-root option. Setting cwd here ONCE makes
# every default resolve correctly. On Posit Connect the deploy bundle
# is the project root and cwd is set by the platform — this line is a
# no-op there but harmless.
setwd(.project_root)

local({
  r_dir <- file.path(.project_root, "R")
  files_in_order <- c(
    "io_helpers.R", "audit_log.R", "code_version.R", "snapshots.R",
    "load_config.R", "load_variables.R", "load_static.R",
    "input_schemas.R", "read_inputs.R",
    "input_acquisition.R",
    "run_export.R",
    "transform_lending.R", "lending_portfolio_view.R",
    "transform_investments.R", "investment_portfolio_view.R",
    "macro_model.R", "pd_term_structure.R",
    "lifetime_parameter_other.R", "build_stpd.R",
    "build_intermediates.R", "output_writers.R",
    "reconciliation.R",
    "validation.R", "validation_suppressions.R",
    "validators_input.R", "validators_static.R",
    "validators_transform.R", "validators_derived.R",
    "run_discovery.R",
    "run_approval.R",
    "manifest.R",
    "run_etl.R",
    "run_etl_phased.R"
  )
  # Fail LOUDLY if any R/ file is missing. Previously this loop silently
  # skipped missing files, which on Posit Connect would let the app boot
  # without (e.g.) validators_input.R being sourced — and only later
  # surface as a confusing "could not find function build_input_validators"
  # when the user clicked Pre-run check. Better to refuse to start.
  missing_r_files <- character()
  for (f in files_in_order) {
    p <- file.path(r_dir, f)
    if (!file.exists(p)) {
      missing_r_files <- c(missing_r_files, f)
      next
    }
    source(p, local = FALSE)
  }
  if (length(missing_r_files) > 0) {
    stop("The following pipeline files are missing from the deployment:\n  ",
         paste(missing_r_files, collapse = "\n  "),
         "\n\nThe app cannot run without them.\n",
         "If you are deploying to Posit Connect, verify R/ is included in ",
         "your manifest.json — see manifest.json's `files` block.\n",
         "Looked under: ", r_dir)
  }
})


# ---- Static hints for Posit Connect file detection ---------------------
# rsconnect::writeManifest() walks the project for literal source("R/foo.R")
# calls and bundles whichever files it finds. Our REAL sourcing happens in
# the for loop above via file.path(r_dir, f), which is dynamic and invisible
# to the static analyzer — so without this block, several R/ files get
# silently dropped from the deployment bundle and the app crashes at boot
# with "could not find function ...".
#
# This block never executes (if (FALSE) ensures that). Its only purpose is
# to give the static analyzer literal strings to detect. Keep in sync with
# files_in_order above.
if (FALSE) {
  source("R/io_helpers.R")
  source("R/audit_log.R")
  source("R/code_version.R")
  source("R/snapshots.R")
  source("R/load_config.R")
  source("R/load_variables.R")
  source("R/load_static.R")
  source("R/input_schemas.R")
  source("R/read_inputs.R")
  source("R/input_acquisition.R")
  source("R/run_export.R")
  source("R/transform_lending.R")
  source("R/lending_portfolio_view.R")
  source("R/transform_investments.R")
  source("R/investment_portfolio_view.R")
  source("R/macro_model.R")
  source("R/pd_term_structure.R")
  source("R/lifetime_parameter_other.R")
  source("R/build_stpd.R")
  source("R/build_intermediates.R")
  source("R/output_writers.R")
  source("R/reconciliation.R")
  source("R/validation.R")
  source("R/validation_suppressions.R")
  source("R/validators_input.R")
  source("R/validators_static.R")
  source("R/validators_transform.R")
  source("R/validators_derived.R")
  source("R/run_discovery.R")
  source("R/run_approval.R")
  source("R/manifest.R")
  source("R/run_etl.R")
  source("R/run_etl_phased.R")
}


# ---- Source page modules -------------------------------------------------
local({
  mods <- list.files(file.path(.project_root, "app", "modules"),
                     pattern = "\\.R$", full.names = TRUE)
  for (m in mods) source(m, local = FALSE)
})


# ---- Top-level UI --------------------------------------------------------
ui <- page_navbar(
  title = tagList(
    tags$img(src = "qdb_logo.jpg", height = "32px",
             class = "qdb-navbar-logo",
             style = "margin-right: 12px; margin-top: -4px;"),
    tags$span("IFRS9 ETL Runs",
              style = "font-weight: 600; vertical-align: middle;")
  ),
  theme = bs_theme(version = 5, bootswatch = "flatly",
                   primary = "#5b1f6e"),
  fillable = FALSE,
  header = tags$head(
    # Browser tab favicon
    tags$link(rel = "icon", type = "image/jpeg",
              href = "qdb_logo.jpg"),
    tags$style(HTML("
      /* The logo is dark purple on a JPG with white background. The
         flatly navbar is dark, so the logo's white background creates
         an ugly white block AND the dark text is hard to read. The
         filter chain below: (1) brightness(0) makes every pixel
         black, (2) invert(1) flips it to white, (3) the JPG's white
         background, also inverted, becomes black — which we hide via
         mix-blend-mode: screen so it lets the navbar show through. */
      .qdb-navbar-logo {
        filter: brightness(0) invert(1);
        mix-blend-mode: screen;
      }
      .small-muted { color: #6c757d; font-size: 0.85em; }
      .pill { display: inline-block; padding: 0.15em 0.5em; border-radius: 0.4em;
              font-size: 0.8em; font-weight: 600; }
      .pill-error    { background: #dc3545; color: white; }
      .pill-warn     { background: #fd7e14; color: white; }
      .pill-info     { background: #0dcaf0; color: white; }
      .pill-pass     { background: #198754; color: white; }
      .pill-suppr    { background: #6c757d; color: white; }
      .pill-approved { background: #198754; color: white; }
      .pill-pending  { background: #fd7e14; color: white; }
      .pill-draft    { background: #6c757d; color: white; }
      .pill-archived { background: #adb5bd; color: white; }
      .pill-tested        { background: #0d6efd; color: white; }
      .pill-pending_final { background: #fd7e14; color: white; }
      .pill-rejected      { background: #dc3545; color: white; }
      .narrow-table th, .narrow-table td { font-size: 0.85em; padding: 0.4em; }
      /* Subtle QDB-purple accents on cards */
      .card-header { background-color: #f6f3f8; border-bottom: 1px solid #e0d4e6; }
      .btn-primary { background-color: #5b1f6e; border-color: #5b1f6e; }
      .btn-primary:hover { background-color: #4a1758; border-color: #4a1758; }
    "))
  ),
  nav_panel("Runs",        mod_runs_ui("runs")),
  nav_panel("Run pipeline",mod_run_trigger_ui("run_trigger")),
  nav_panel("Approval queue", mod_approval_queue_ui("approval")),
  nav_panel("Snapshots",   mod_snapshots_ui("snapshots")),
  nav_panel("Manage snapshots", mod_snapshot_manager_ui("snapshot_manager")),
  nav_panel("Edit snapshot", mod_snapshot_editor_ui("snapshot_editor")),
  nav_panel("Suppressions", mod_suppressions_ui("suppressions")),
  nav_panel("Audit log",   mod_audit_log_ui("audit")),
  nav_spacer(),
  nav_panel("Help", mod_help_ui("help"), icon = icon("circle-question")),
  nav_item(
    # The code SHA shown here is captured at app startup. If you pull
    # new code without restarting Shiny, this banner will be stale —
    # restart to refresh.
    local({
      sha <- tryCatch(get_current_code_sha(), error = function(e) NA_character_)
      sha_short <- if (is.na(sha)) "?" else substr(sha, 1, 8)
      sha_full  <- if (is.na(sha)) "(SHA unavailable)" else sha
      tags$span(class = "small-muted",
                style = "margin-right: 1.2em;",
                title = paste0("Full code SHA: ", sha_full),
                "code: ", tags$code(sha_short))
    })
  ),
  nav_item(
    tags$span(class = "small-muted",
              sprintf("user: %s", Sys.info()[["user"]] %||% "unknown"))
  )
)


# ---- Top-level server ----------------------------------------------------
server <- function(input, output, session) {
  # Cross-module refresh signals. When one module changes snapshots
  # (create / promote / approve / reject), it bumps this counter; other
  # modules that show snapshot lists observe this value and re-read
  # from disk. Without this, the Run pipeline page's dropdown only
  # reflects the snapshot state at app start, missing any approvals
  # done during the session.
  session$userData$snapshots_changed <- reactiveVal(0)
  
  # Same pattern for runs: when the Run pipeline finishes phase 2,
  # the Runs page and Approval queue need to know so they re-read
  # the runs/ directory and pick up the new run. When approve/reject
  # happens, the Runs page (status pill column) needs to re-read.
  session$userData$runs_changed <- reactiveVal(0)
  
  mod_runs_server("runs",
                  on_select_run = function(run_path) {})
  mod_run_trigger_server("run_trigger")
  mod_approval_queue_server("approval")
  mod_snapshots_server("snapshots")
  mod_snapshot_manager_server("snapshot_manager")
  mod_snapshot_editor_server("snapshot_editor")
  mod_suppressions_server("suppressions")
  mod_audit_log_server("audit")
  mod_help_server("help")
}


# ---- Launch (when sourced via shiny::runApp("app")) ---------------------
shinyApp(ui = ui, server = server)