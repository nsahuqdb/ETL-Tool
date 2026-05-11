# tests/manual/launch_app.R
#
# Convenience launcher for the read-only Shiny app. Run from the project root:
#
#   setwd("path/to/ifrs9_etl")
#   source("tests/manual/launch_app.R")
#
# Or from R directly:
#
#   shiny::runApp("app", launch.browser = TRUE)
#
# Configuration:
#   options(ifrs9.runs_dir       = "runs")              # default
#   options(ifrs9.snapshots_dir  = "config_snapshots")  # default
#   options(ifrs9.audit_log      = "logs/etl_audit.jsonl")  # default

if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Install the shiny package first:  install.packages('shiny')")
}

shiny::runApp("app", launch.browser = TRUE)
