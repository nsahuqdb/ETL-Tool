# Operator runbook — IFRS9 ETL

This document is for the user who **runs** the IFRS9 ETL day-to-day.
It does not assume R or developer knowledge. Skip the "Getting started"
section below if someone has already set up the app for you — head
straight to "Daily workflow".

## Quick reference

| I want to... | Page |
|---|---|
| Run the pipeline for a new period | Run pipeline |
| See what runs have happened | Runs |
| Review a finished run and approve it | Approval queue → Pending |
| Look up an old approval decision | Approval queue → History |
| Browse archived configurations | Snapshots |
| Create a new named configuration version | Manage snapshots |
| Mark a known-failing validator as accepted | Suppressions |
| Forensic trace of who did what when | Audit log |

## Getting started

### One-time setup (per machine)

1. Install R 4.x and the required packages:
   ```r
   install.packages(c("shiny", "bslib", "DT", "jsonlite", "yaml",
                      "tibble", "markdown", "readr", "dplyr"))
   ```
2. Place the period's input files in `input/`. Filenames must match
   what `config.yml` expects (`AccountMaster.xlsx`, `CustomerMaster.xlsx`,
   etc.).
3. From R, set the working directory to the project root and launch
   the app:
   ```r
   setwd("path/to/ifrs9_etl")
   shiny::runApp("app", launch.browser = TRUE)
   ```

### Per-period checklist (recommended)

Before clicking Run pipeline:

1. Confirm `input/` contains the latest extracts.
2. Confirm `config.yml` has the right `extract_date`. Run-time
   validators check that the inputs' `EXTRACTDA` columns match.
3. Decide whether to run against an approved snapshot or live config.
   Production runs should generally use a snapshot.

## Daily workflow

### 1. Pre-run check

Run pipeline page → top button **Pre-run check**. Takes ~5 seconds.
Result is a table of every input-stage validator with a coloured pill
in the status column:

- **PASS** — green; the validator was satisfied
- **WARN** — yellow; validator failed but is non-blocking. Run still
  allowed
- **ERROR** — red; validator failed at blocking severity. Run is
  disabled until you fix the input or add a suppression
- **SUPPR** — grey; validator failed but is suppressed by an entry in
  `validation_suppressions.yml`. Treated as PASS for gating.

If any ERROR row appears, the **Start run** button is disabled. Address
the underlying issue (most often an input file problem or schema
mismatch) and re-run pre-check.

### 2. Start run

The pipeline runs Phase 1: load inputs, validate, transform lending and
investment portfolios, calculate per-customer rating/stage/restructuring,
validate the transformation. Takes ~10 seconds. The UI shows a progress
bar.

When Phase 1 finishes the page changes to the **pause page**.

### 3. Pause page — review and override

You'll see a filterable table of every customer with their **calculated**
rating, stage, and restructuring values. The table is searchable; ID
columns get a text search box, numeric measures (exposure, DPD) get a
range slider.

To add an override:

1. Click a customer row.
2. The **Add override** form below shows the calculated values and
   dropdowns for the override values.
3. The dropdowns are constrained:
   - **Rating**: all 21 internal QDB ratings, ordered best-to-worst.
   - **Stage**: only worsening transitions. From Stage 1 you can go to
     2 or 3; from Stage 2 only to 3; Stage 3 has no override.
   - **Restructuring**: only the opposite direction (Restructured ↔ Not
     Restructured).
4. Type a **reason**. Required — every override has an audit reason.
5. Click **Add override**.

The right-hand card shows pending overrides accumulating. You can add
overrides for as many customers as needed.

To start over: **Cancel run**.

### 4. Continue

Click **Continue (apply overrides + finish)**. The pipeline runs
Phase 2: applies your overrides to the master columns, runs the derived
calculations (LTPO, PD term structures, StPD), validates derived
outputs, writes 18 output CSVs, writes manifest and run status. Takes
~20-25 seconds. UI is locked during this.

When it finishes the page shows a summary card with the run ID,
duration, and override count. The run is now `pending_approval`.

### 5. Approve or reject

Switch to **Approval queue → Pending**. Your run appears in the table.
Click it.

The detail block shows three tabs:

- **Validation** — every validator from all three stages with status pill
- **Overrides applied** — the three override CSVs as plain tables
- **Outputs** — list of the 18 output files with sizes

Below: an **Approve / Reject** card. Type a reason (required) and click
the action button.

After approval, the run leaves the Pending tab. To find it again, use
**Approval queue → History** or **Runs**.

## Browsing past runs

### Runs page

Newest-first table of every run that's been kept on disk. Columns:
run ID, started time, duration, user, snapshot, output count, validation
failure count, reconciliation flag, code SHA prefix.

Click a row → 5 tabs:

- **Manifest** — full metadata: when, who, on what code, against what
  snapshot, with what inputs
- **Validation** — every validator's verdict
- **Overrides** — the three override files for this run
- **Reconciliation** — if a reference output dir is configured, the
  diff against the bundled V4 outputs
- **Outputs** — pick any of the 18 output files; first 2,000 rows of
  each are previewed in a paginated DataTable with column filters

### Approval queue → History

Decision-focused view. Every approved or rejected run as a row, newest
decision first. Columns include the four-way audit trail:

- requested_by, requested_at, request_comment
- decided_by, decided_at, decision_comment

Click a history row → audit-trail card unfolds with the full comments
in pre-formatted blocks (multi-line preserved). Same Validation /
Overrides applied / Outputs tabs as the Pending detail.

## Managing configuration

### Snapshots — read-only browse

A snapshot is a frozen, named version of `config/` and `data-raw/static/`.
Created so a run from months ago can be reproduced exactly with the same
config, even if the live `config/` has moved on.

Use the **Snapshots** page to browse what snapshots exist and view
their contents (read-only file viewer with relative-path labels).

Status meanings:

- `draft` — created but not finalised; can still be edited (in a future
  release)
- `pending` — submitted for review
- `approved` — accepted; ready for production runs
- `archived` — older approved snapshot, kept for audit but not actively used

### Manage snapshots — write-capable

To create a new snapshot:

1. Page → **Create snapshot** card → fill label (letters, digits, `.`,
   `_`, `-` only), description (required, audit trail), optionally pick
   a parent snapshot.
2. Click **Create snapshot**. The current `config/` and `data-raw/static/`
   are copied to `config_snapshots/<label>/`.

To promote a snapshot through its lifecycle:

1. **Promote snapshot** card → pick the snapshot.
2. Action buttons appear matching legal transitions (`draft → pending`,
   `pending → approved` or `pending → draft`, `approved → archived`).
3. **Approval requires a non-empty reason**. Promotion to `approved`
   captures `approved_by` automatically from your OS username.
4. Click the target-status button.

The status pill in the metadata panel updates immediately.

## Suppressions

Use sparingly. A suppression marks a specific validator as "we know
it's failing and that's OK, here's why". The validator still runs and
the finding is still in `validation.csv`; it just stops blocking the
run.

To add: **Suppressions** page → fill validator ID (copy from the
catalog on the right), reason (required), optionally set a `valid_until`
date for auto-expiry. Click **Add suppression**.

The catalog on the right shows failed validators from the most recent
run, grouped by stage. Approver is auto-filled from your username.

## Audit log

Append-only JSON-line log at `logs/etl_audit.jsonl`. Every meaningful
action records an event:

- `run_start`, `run_phase1_complete`, `run_overrides_applied`,
  `run_pending_approval`, `run_finish`, `run_approved`, `run_rejected`
- `validation_summary` (one per stage per run)
- `pre_run_check`
- `snapshot_create`, `snapshot_promote`
- `suppression_add`

The Audit log page shows a 5-column view: ts, event, user, run_id,
summary. The `summary` is a one-line human description per event type.

Three filter dropdowns at the top: **Event type**, **Run ID**, **User**.
For tracing what happened in one run, filter by Run ID — you'll see
all 8-10 events from that run sorted newest-first.

## Things that can go wrong

### "Pre-run failed: Run config not found: config.yml"

The Shiny session's working directory isn't the project root. Restart
the app, ensuring you `setwd()` to the project root before
`shiny::runApp()`. The app pins absolute paths at startup so this
shouldn't recur within a session.

### "Pre-run failed: Static reference directory does not exist"

Same root cause. The fix in v_H11c made `load_run_config` resolve all
paths relative to the config file itself, so this should not happen
on current code. If it does, the config file is reaching the loader
with a relative path that doesn't resolve — check that
`config.yml` actually exists at the path the error reports.

### Run hangs at "Phase 2: derived + write outputs"

Normal during StPD computation. Wait up to 30 seconds. If it never
finishes, check the R console for the actual error — the modal-progress
UI catches the error and surfaces it as a notification.

### Run completed but my override doesn't appear in AccountMaster.csv

This was a real bug fixed in v_H12d. If you're running older code,
upgrade. Otherwise: confirm the override is in
`runs/<run_id>/overrides/rating_overrides.csv`; if YES but the output
still has the old rating, the bug has resurfaced — please report.

### Run doesn't appear in Runs page after Continue

Phase 2 didn't reach `write_manifest()`. The most likely cause is a
crash in derived calculations (LTPO or StPD). The directory exists
but is incomplete. Check `runs/<latest_id>/reports/` — you'll see
which files are missing. Investigate the R console for the error;
delete the half-written run if you don't need it.

### Browser closed during pause page

State is lost. The run_id was logged but never finished; you'll see
`run_start` and `run_phase1_complete` in audit but no `run_finish`.
Start a fresh run; nothing else needs cleanup.

### Approve button is greyed out / nothing happens

You haven't typed a decision reason. Required field; non-empty check.

## File system map

```
ifrs9_etl/
├── config/                       # live configuration
│   ├── config.yml
│   ├── models.yml
│   ├── variable_dictionary.yml
│   ├── model_inputs.yml
│   └── validation_suppressions.yml
├── data-raw/static/              # live static reference
│   ├── master_rating_scale.csv
│   ├── ttc_pd_table.csv
│   └── ... (etc.)
├── input/                        # period's raw extracts
│   ├── AccountMaster.xlsx
│   ├── CustomerMaster.xlsx
│   └── ... (etc.)
├── runs/                         # one folder per run, kept indefinitely
│   └── <run_id>/                 # e.g. 2026-05-06_14-31-38/
│       ├── Output/               # 18 output CSVs
│       ├── overrides/
│       │   ├── rating_overrides.csv
│       │   ├── stage_overrides.csv
│       │   └── restructuring_overrides.csv
│       └── reports/
│           ├── manifest.json
│           ├── run_status.yml
│           ├── validation.csv
│           ├── validation.md
│           ├── reconciliation.md     (if reference dir configured)
│           └── mismatches/
├── config_snapshots/             # frozen named configurations
│   └── <label>/
│       ├── config/
│       ├── static/
│       └── snapshot.yml
├── logs/
│   └── etl_audit.jsonl
└── R/                            # pipeline code (you don't normally touch this)
```

Runs are kept indefinitely. The `runs/` directory will grow over time —
each run is a few hundred KB. To clean up old runs, just delete the
relevant subdirectories; they are otherwise inert.

## Who-can-do-what (informal)

The app does not enforce roles. Identity is taken from the OS username
at session start; that's what gets recorded in audit, override files,
and snapshot metadata. Real role enforcement (creator ≠ approver, etc.)
is a deployment-platform concern (Posit Connect groups, SSO, etc.).

In the current setup all users can:
- Run the pipeline and add overrides
- Approve and reject any pending run, including their own
- Create, edit, promote snapshots
- Add suppressions

For team practice we recommend (but don't enforce in code) that the
person who clicks Continue at the end of Phase 2 is NOT the same person
who approves the run.
