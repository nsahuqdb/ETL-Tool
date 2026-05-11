# Shiny app — H10 (read) + H11 (write)

A Shiny app to inspect and operate the IFRS9 ETL pipeline.

## Pages

| Page              | Read/Write | What it does |
|-------------------|------------|---|
| Runs              | Read       | Past runs newest-first; drill into manifest, validation, reconciliation, output preview. |
| Run pipeline      | **Write**  | Pick a snapshot (or live config), run pre-check, trigger a full pipeline run. |
| Snapshots         | Read       | Browse all snapshots with status badges and per-snapshot file viewer. |
| Manage snapshots  | **Write**  | Create new snapshots; promote drafts through draft → pending → approved → archived. |
| Suppressions      | **Write**  | Add validation suppressions with required reason + approver; auto-lapse on optional valid_until. |
| Audit log         | Read       | Filterable view of `logs/etl_audit.jsonl` with per-event human summary. |

## How runs get into the app

The pipeline writes runs to disk; the app reads them back. There is no
database. Refreshing the page re-reads from disk.

For a run to show up in the Runs page, `run_etl()` must be invoked with
`keep_history = TRUE`. That writes outputs to a timestamped subdirectory
under the configured runs directory:

```r
run_etl(config_path = "config.yml", keep_history = TRUE)
# -> writes to: runs/2026-05-06_10-30-17/Output/...
#                runs/2026-05-06_10-30-17/reports/manifest.json
#                runs/2026-05-06_10-30-17/reports/validation.csv
```

The default `keep_history = FALSE` preserves single-folder behaviour for
the existing test harness — tests overwrite `test_output/` on each run.
Production / Shiny-friendly runs should pass `keep_history = TRUE`.

## Configuration

The app reads the following options at startup. All have sensible defaults
so plain `shiny::runApp("app")` works out of the box.

| Option                | Default              | What it controls |
|-----------------------|----------------------|---|
| `ifrs9.runs_dir`      | `"runs"`             | Where the Runs page looks for runs |
| `ifrs9.snapshots_dir` | `"config_snapshots"` | Where the Snapshots page looks |
| `ifrs9.audit_log`     | `"logs/etl_audit.jsonl"` | Source for the Audit log page |

These match the defaults used by the pipeline modules
(`run_etl`, `snapshots`, `audit_log`), so no extra wiring is needed
unless the deployment puts these elsewhere.

## Running locally

From the project root:

```r
setwd("path/to/ifrs9_etl")
shiny::runApp("app", launch.browser = TRUE)
```

Or:

```r
source("tests/manual/launch_app.R")
```

## Required packages

```r
install.packages(c("shiny", "bslib", "DT", "jsonlite",
                    "yaml", "tibble", "markdown", "readr"))
```

Optional but recommended:

```r
install.packages("shinyAce")
```

`shinyAce` upgrades the YAML editor on the **Edit snapshot** page from a
plain textarea to a real code editor with line numbers and YAML syntax
highlighting. When YAML fails to parse, the error message references
specific line numbers — without `shinyAce` you can't see them.

## User identity

The app reads `Sys.info()[["user"]]` and shows it in the navbar. There is
no login UI. For deployed instances, real authentication should live in
the deployment platform (Posit Connect, Shiny Server with auth proxy,
nginx with SSO, etc.) — those platforms set `Sys.info()[["user"]]` to
the authenticated principal.

## Architecture

```
app/
├── app.R                       # entry: ui + server + shinyApp(...)
└── modules/
    ├── mod_runs.R              # Runs table + run-detail tabs
    ├── mod_snapshots.R         # Snapshots table + metadata + file viewer
    └── mod_audit_log.R         # Filterable audit log
```

Each page is a Shiny module. Modules namespace their inputs/outputs so
they can't collide. The pipeline R/ code is sourced once at app startup
and module servers call its helpers (`list_runs`, `read_run_manifest`,
`list_snapshots`, `read_audit_log`, ...) — no separate "data layer" in
the app.

## Deployment to Posit Connect / Shiny Server

The app expects the project root as the working directory at runtime so
relative paths (`R/`, `runs/`, `config_snapshots/`, `logs/`) resolve.
Posit Connect handles this automatically when you deploy the entire
project.

For a Shiny Server deployment:

```bash
# /srv/shiny-server/ifrs9-browser/app -> symlink to /path/to/ifrs9_etl/app
# AND make sure /path/to/ifrs9_etl is the runtime working directory
```

Or set `Sys.setenv(IFRS9_RUNS_DIR = "/srv/data/ifrs9/runs")` in the
deployed `Renviron` and the app will read from the absolute path.

## What this app currently does NOT do

- It does **not** edit override CSVs (rating overrides, stage overrides). That UI is H11b.
- It does **not** run pipelines asynchronously — the UI locks during a run (~30s). H12 if it becomes a real issue.
- It does **not** authenticate users — auth is the deployment platform's job. The app reads `Sys.info()[["user"]]` and uses it for audit-trail attribution.

## What's next (H11b preview)

- Override editor for customer rating + stage overrides. Tabular editor that
  enforces the audit columns (created_by, approved_by, status, etc.) and
  routes drafts through the same approval gate snapshots use.
