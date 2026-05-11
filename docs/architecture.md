# Architecture — IFRS9 ETL R Port

A top-down view of how the IFRS9 ETL R port is organized. If you're
reviewing the codebase for the first time, start here.

For the *why* of specific design decisions, see:
- `docs/h12_design.md` — the mid-run pause + run-level approval workflow
- `docs/h7_design_notes.md` — the variable dictionary + audit columns + Shiny prep
- `docs/versioning_model.md` — the snapshot + code-SHA two-axis versioning
- `docs/static_provenance.md` — provenance of every static reference file

## What this software does

The IFRS9 ETL R port reproduces the calculations of a legacy Excel
workbook (V4, with V7 methodology updates) used by Qatar Development
Bank for IFRS9 Expected Credit Loss reporting. The pipeline:

1. Reads 12 input CSVs from a regulator-supplied data extract
2. Loads ~25 static reference tables (rating scale, portfolio mapping,
   etc.) from `data-raw/static/`
3. Calculates per-customer rating, stage, and restructuring status
4. Lets a credit officer override these per-customer with required
   reasons (mid-run pause)
5. Calculates derived outputs (StPD term structure, lifetime parameters)
6. Writes 18 standard output CSVs that load into the bank's downstream
   ECL reporting system
7. Captures the full audit trail — who did what, when, with what reason
   — for regulatory review

The R port replaces a process where the credit officer opened the
Excel workbook, pasted in raw data, made manual cell edits where
business knowledge contradicted the calculation, and exported CSVs.
Excel had no audit trail, no reproducibility, and no validation.

## Top-level file layout

```
ifrs9_etl/
├── R/                      # all pipeline code (no R package)
├── app/                    # Shiny operator UI (read+write)
│   ├── app.R
│   └── modules/            # one module per page
├── config/                 # active (live) configuration
│   ├── config.yml
│   ├── models.yml
│   ├── model_inputs.yml
│   ├── variable_dictionary.yml
│   └── validation_suppressions.yml
├── config_snapshots/       # frozen named versions of config + static
├── data-raw/static/        # static reference tables (rating scale, etc.)
├── input/                  # the user's data extract (NOT in git)
├── runs/                   # timestamped run outputs (NOT in git)
│   └── <YYYY-MM-DD_HH-MM-SS>/
│       ├── Output/         # the 18 standard CSVs
│       ├── overrides/      # per-run override files
│       └── reports/        # manifest, validation, recon, status
├── logs/etl_audit.jsonl    # append-only event log
├── tests/manual/           # human-driven test runners
└── docs/                   # this file and friends
```

## Pipeline stages

The pipeline runs in five stages. The stage boundary between **portfolio
view** and **derived outputs** is where H12's mid-run pause lives.

```
Inputs (12 CSVs)  +  Static reference (25 tables)  +  Config (4 YAMLs)
       │
       ▼
   ┌─────────────────────────────────────────────────────┐
   │  Stage 1: Load + validate inputs                    │
   │    R/read_inputs.R, R/input_schemas.R               │
   │    R/validators_input.R                             │
   │    Validation gate 1: 27 INPUT validators           │
   └─────────────────────────────────────────────────────┘
       │
       ▼
   ┌─────────────────────────────────────────────────────┐
   │  Stage 2: Transformation + portfolio view           │
   │    R/transform_lending.R         (6 passes)         │
   │    R/lending_portfolio_view.R                       │
   │    R/transform_investments.R                        │
   │    R/investment_portfolio_view.R                    │
   │    Outputs: trans_l, cm_view, trans_i, inv_view     │
   │    Validation gate 2: 28 TRANSFORM validators       │
   └─────────────────────────────────────────────────────┘
       │
       ▼  ◄─ Phase 1 ends here. cm_view is shown to user.
       │     User adds rating/stage/restructuring overrides per-customer.
       ▼
   ┌─────────────────────────────────────────────────────┐
   │  Stage 3: Apply user overrides                      │
   │    R/run_etl_phased.R (phase 2 first half)          │
   │    Rewrites trans_l$rating_worst, cm_view$stage_final│
   │    cm_view$restructuring_final for affected rows.   │
   │    No re-running of transformation.                 │
   └─────────────────────────────────────────────────────┘
       │
       ▼
   ┌─────────────────────────────────────────────────────┐
   │  Stage 4: Derived outputs                           │
   │    R/lifetime_parameter_other.R                     │
   │    R/pd_term_structure.R                            │
   │    R/build_stpd.R                                   │
   │    Validation gate 3: 29 DERIVED validators         │
   └─────────────────────────────────────────────────────┘
       │
       ▼
   ┌─────────────────────────────────────────────────────┐
   │  Stage 5: Build intermediates + write outputs       │
   │    R/build_intermediates.R (customer flags, etc.)   │
   │    R/output_writers.R                               │
   │    Writes 18 CSVs + manifest.json                   │
   │    Run lands as `pending_approval`                  │
   └─────────────────────────────────────────────────────┘
       │
       ▼
   ┌─────────────────────────────────────────────────────┐
   │  Stage 6 (out of pipeline): reviewer approval       │
   │    R/run_approval.R                                 │
   │    UI: app/modules/mod_approval_queue.R             │
   │    `pending_approval → approved | rejected`         │
   └─────────────────────────────────────────────────────┘
```

## Two entry points

### `run_etl()` — single-shot
For automation, tests, and batch contexts. Calls phase 1 + phase 2
inline with no override hook. Lives in `R/run_etl.R`. Used by
`tests/manual/test_phase_h*.R`. Honors override CSVs from the legacy
location `data-raw/static/customer_*_overrides.csv` for backward-compat.

### `run_etl_phase1()` + `run_etl_phase2()` — Shiny-driven
For interactive use through the operator UI. Lives in
`R/run_etl_phased.R`. Phase 1 returns a state object the Shiny app
holds in a `reactiveVal`; phase 2 takes that state plus user-collected
overrides and finishes the run. Override CSVs from
`data-raw/static/customer_*_overrides.csv` are NOT applied — only the
overrides the user added through the UI count.

## Two axes of versioning

Configuration and code are versioned independently.

- **Snapshot** = a frozen copy of `config/` + `data-raw/static/`.
  Lives at `config_snapshots/<label>/`. Has its own lifecycle:
  `draft → pending → approved → archived`.

- **Code SHA** = the git SHA of the `R/` and `app/` source.
  Captured automatically per run via `R/code_version.R`.

A run is reproduced by `(snapshot_label, code_sha, override_replay_source)`.
The first two specify the configuration and the implementation; the third
specifies the user decisions that went into the run.

See `docs/versioning_model.md` for full detail.

## Where overrides live

Three override files per run, stored at `runs/<run_id>/overrides/`:

| File                            | What it changes                       | Master column it writes to              |
|---------------------------------|---------------------------------------|------------------------------------------|
| `rating_overrides.csv`          | Per-customer rating                   | `trans_l$rating_worst` (per-contract broadcast) + `cm_view$rating_final` |
| `stage_overrides.csv`           | Per-customer ECL stage (1/2/3)        | `cm_view$stage_final`                    |
| `restructuring_overrides.csv`   | Per-customer restructuring flag       | `cm_view$restructuring_final`            |

Each row carries the audit columns: `customer_id, value, reason,
created_by, created_at, prior_value, source_run_id`.

UI constraints (enforced in `app/modules/mod_run_trigger.R`):
- Rating: any of the 21 internal QDB ratings
- Stage: only worsening allowed (Stage 1 → 2/3, Stage 2 → 3, Stage 3 → none)
- Restructuring: flip to the opposite value

Approval is at the **run** level, not the row level. A reviewer signs
off on the run as a whole; the override files are part of what they
approve.

## Audit trail

Every meaningful action writes one JSON line to `logs/etl_audit.jsonl`.
Event types:

| Event                  | When                                                 |
|------------------------|-----------------------------------------------------|
| `run_start`            | `run_etl_phase1` enters                              |
| `validation_summary`   | After each of 3 validation gates                     |
| `run_phase1_complete`  | Phase 1 returns; pipeline paused                     |
| `run_overrides_applied`| User clicks Continue; phase 2 starts                 |
| `run_pending_approval` | Phase 2 finishes; outputs written                    |
| `run_finish`           | Phase 2 returns                                      |
| `run_approved`         | Reviewer clicks Approve                              |
| `run_rejected`         | Reviewer clicks Reject                               |
| `snapshot_create`      | New snapshot created                                 |
| `snapshot_promote`     | Snapshot lifecycle transition                        |
| `suppression_add`      | Validator suppression added                          |
| `pre_run_check`        | Pre-run validation triggered                         |

All run-related events carry the same `run_id`. Filter by run_id in the
Audit log page to reconstruct the full lifecycle of any run.

## Validation framework

Three gates, ~85 validators total. Implemented in
`R/validation.R` (engine), `R/validators_input.R`, `R/validators_transform.R`,
`R/validators_derived.R` (rules), `R/validation_suppressions.R` (suppression
support).

Each validator carries: `id`, `severity` (ERROR/WARN/INFO), `description`,
`rationale`, `remediation`, `tags` (e.g. `pre_run` for fast-feedback gate).

Gates are configurable via `config.yml::run.on_validation_error` —
`stop` (default), `warn`, or `continue`. `pre_run_check()` runs only
validators tagged `pre_run` for the Shiny pre-run page.

Suppressions live in `config/validation_suppressions.yml` and are
managed through the Suppressions tab in the UI. A suppressed validator
still runs and is recorded; its severity is downgraded for gating
purposes only.

## Operator UI structure

`app/app.R` is intentionally tiny. It pins absolute paths via `options()`
at startup (so cwd-resets in Shiny don't break helpers), sources `R/`
and `app/modules/`, and assembles the navbar. Each page is a Shiny
module under `app/modules/`:

| Module                    | Page              | Read/Write |
|---------------------------|-------------------|------------|
| `mod_runs.R`              | Runs              | Read       |
| `mod_run_trigger.R`       | Run pipeline      | **Write**  |
| `mod_approval_queue.R`    | Approval queue    | **Write**  |
| `mod_snapshots.R`         | Snapshots         | Read       |
| `mod_snapshot_manager.R`  | Manage snapshots  | **Write**  |
| `mod_suppressions.R`      | Suppressions      | **Write**  |
| `mod_audit_log.R`         | Audit log         | Read       |

The `Run pipeline` module is the most complex — a state machine with
three states (`idle`, `phase1_done`, `phase2_done`) and three reactive
override buffers. See `docs/h12_design.md` for the rationale.

## What's NOT here (intentionally)

- **No async runs.** Phase 2 takes ~20s; the UI locks. Acceptable for
  now. Refactor when it becomes annoying.
- **No snapshot editor.** Snapshots can be created and promoted but
  the configuration files inside cannot be edited through the UI.
  This is the planned H13.
- **No automatic schema migration.** If we change the format of
  override CSVs or `manifest.json`, old runs may not display correctly.
  We've avoided this need so far by being careful, but there's no
  migration tooling.
- **No multi-user concurrency control.** Two users hitting Run at the
  same moment will get two distinct run_ids and two valid runs;
  there's no lock. Two users editing a snapshot draft simultaneously
  will last-write-wins. Acceptable for current team size.

## How to extend

To add a new validator: edit `R/validators_input.R` (or transform/derived).
The framework auto-discovers them through `build_input_validators()`.

To add a new override type: edit `run_etl_phased.R` to load the override
CSV, and add a new section to the override editor in `mod_run_trigger.R`.

To add a new page to the UI: drop a `mod_<name>.R` in `app/modules/`,
add the nav_panel + server line in `app/app.R`. Modules are
namespaced so they can't collide.

To add a new audit event: call `audit_event(list(event="...", ...))`
from anywhere in `R/`. The `R/audit_log.R` writer handles concurrency
and atomic appends.
