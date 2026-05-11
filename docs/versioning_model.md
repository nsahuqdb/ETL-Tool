# Versioning Model

This pipeline tracks two independent version axes. Together they form the
full reproducibility key for any run.

## The two axes

```
                    ┌───────────────────────────┐
                    │     SNAPSHOT (config)      │
                    │  config/  + data-raw/static/  │
                    │  ─────────────────────────── │
                    │  variable_dictionary.yml      │
                    │  models.yml  (incl. all model │
                    │     definitions)              │
                    │  model_inputs.yml             │
                    │  config.yml                   │
                    │  *.csv  (every static + every │
                    │     override file)            │
                    └───────────────────────────────┘
                                   │
                                   │  paired at run time
                                   ▼
                    ┌──────────────────────────────┐
                    │       CODE VERSION            │
                    │       (Git SHA of R/)          │
                    └────────────────────────────────┘
```

A run is identified by `(snapshot_id, code_sha)`.

## Why two axes, not one?

Config and code change at different rates and for different reasons.

| Axis        | Who changes it     | When                               | How                                                 |
|-------------|--------------------|------------------------------------|-----------------------------------------------------|
| Snapshot    | Analyst / quant    | Each reporting cycle, recalibration | Edit YAML / CSV in Shiny → save new snapshot       |
| Code        | Developer          | Bug fix, feature, methodology       | Git commit + PR + CI                                |

Forcing them into one version axis would mean either every config
refresh creates a code version (silly) or every code commit invalidates
all existing snapshots (worse). Two axes lets each evolve at its
natural pace while still allowing exact reproduction.

## Snapshot lifecycle

```
   draft  ──────►  pending  ──────►  approved  ──────►  archived
  (mutable)       (frozen,         (frozen,            (superseded by
                  awaiting         in production)       a newer approved
                  approval)                              snapshot)
                       │
                       └──────────►  draft (rejected — back to editing)
```

- **draft** — user can mutate any file inside the snapshot dir
- **pending** — frozen; reviewer cannot mutate; can reject (back to
  draft) or approve (to approved)
- **approved** — production. Cannot be modified ever again. Older
  approved snapshots stay readable forever.
- **archived** — manually marked as superseded. No functional effect;
  signals to UI not to show by default.

State transitions and approver identities are recorded in the audit log
(`logs/etl_audit.jsonl`).

## Filesystem layout

```
project_root/
├── config/                            ← LIVE config (drafts edit here)
│   ├── config.yml
│   ├── variable_dictionary.yml
│   ├── models.yml
│   └── model_inputs.yml
├── data-raw/static/                   ← LIVE static (drafts edit here)
│   ├── *.csv
│   └── ...
├── config_snapshots/                  ← snapshot bundles
│   ├── 2026-Q1-final/
│   │   ├── snapshot.yml               ← metadata
│   │   ├── changelog.md               ← human-written
│   │   ├── changes.json               ← machine diff vs parent
│   │   ├── config/                    ← frozen copy
│   │   └── static/                    ← frozen copy
│   └── 2026-Q2-draft-1/
│       └── ...
└── logs/
    └── etl_audit.jsonl                ← append-only event log
```

## Snapshot metadata (snapshot.yml)

```yaml
schema_version: "1.0"
label: 2026-Q1-final
status: approved                      # draft | pending | approved | archived
description: Q1 2026 final close — recalibrated MEV coefficients
created_by: anand
created_at: 2026-04-15T10:23:00+0300
parent: 2026-Q1-draft-3               # lineage
code_sha_at_creation: abc1234567...   # SHA of R/ when snapshot was made
approved_by: priya                    # null while draft/pending
approved_at: 2026-04-16T09:01:00+0300
approval_reason: "Aligned with calibration team output dated 2026-04-14"
```

## How a run resolves paths

Without a snapshot, paths come from the live `config.yml`:
```r
result <- run_etl(config_path = "config.yml")
```

With a snapshot, all config + static paths are resolved INSIDE the
snapshot directory; `config_path` is overridden:
```r
result <- run_etl(snapshot = "2026-Q1-final")
```

Inputs (`AccountMaster.xlsx` etc.) are NEVER part of a snapshot.
Inputs are the per-cycle source data; only configs and static
reference are versioned.

## Code-version compatibility

When a snapshot is selected, `run_etl` compares the current code SHA to
the snapshot's `code_sha_at_creation` and emits a warning if they
differ. Output may differ from what the snapshot originally produced.

To reproduce a historical run exactly:
```bash
git checkout <snapshot.code_sha_at_creation>
Rscript -e 'source("R/run_etl.R"); source_pipeline(); run_etl(snapshot = "2026-Q1-final")'
```

A future Shiny app will offer a dropdown for both axes and check them
out automatically before running.

## Schema versioning

Every YAML file carries a `schema_version` field at the top. When a
file format changes incompatibly, the major version increments and
the loader either upgrades automatically or refuses. This protects
old snapshots when newer code reads them.

Current schema versions:

| File                       | Version |
|----------------------------|---------|
| variable_dictionary.yml    | 1.0     |
| models.yml                 | 1.0     |
| snapshot.yml               | 1.0     |
| manifest.json              | 1.0     |

## Audit log

`logs/etl_audit.jsonl` (configurable via `options(ifrs9.audit_log = ...)`)
is append-only. Every relevant action emits one JSON event:

```json
{"ts":"2026-05-06T10:00:00+0300","event":"snapshot_create","user":"anand","snapshot":"2026-Q2-draft-1","parent":"2026-Q1-final","description":"Refresh forecasts"}
{"ts":"2026-05-06T10:05:13+0300","event":"snapshot_promote","user":"priya","snapshot":"2026-Q2-draft-1","from_status":"pending","to_status":"approved","reason":"Reviewed by quant team"}
{"ts":"2026-05-06T10:15:00+0300","event":"run_start","user":"anand","snapshot":"2026-Q2-draft-1","code_sha":"abc1234..."}
{"ts":"2026-05-06T10:15:42+0300","event":"run_finish","user":"anand","snapshot":"2026-Q2-draft-1","duration_seconds":42.1,"n_outputs":18}
```

Events are never deleted or edited. If something needs to be undone,
that's a new event referencing the original.

## Overrides are NOT part of snapshots (since H12)

This was the design through H7–H11, but H12 moved overrides out of
snapshots entirely. The reasoning is captured in `h12_design.md`; the
short version is that override decisions need calculated context, so they
have to be made mid-run. That made approval at the override-row level
incoherent, and approval at the run level the natural unit instead.

What this means for snapshots:

- A snapshot freezes `config/` and `data-raw/static/` only.
- `data-raw/static/customer_*_overrides.csv` files still exist on disk
  for backward-compat with the legacy single-shot `run_etl()` path used
  by tests, but the production phased path (`run_etl_phase1` +
  `run_etl_phase2`) ignores them.
- Per-run overrides live at `runs/<run_id>/overrides/` and are written
  by phase 2. They are immutable after the run completes.

What this means for reproducibility:

- A run is reproduced by `(snapshot_id, code_sha, run_id_to_replay_overrides_from)`.
- The first two specify the configuration; the third specifies the
  overrides. All three are recorded in the run manifest.
- If you re-run the same `(snapshot_id, code_sha)` with the same overrides,
  outputs are bit-identical (modulo timestamp fields).

## Why this is robust

1. **Reproducibility:** `(snapshot_id, code_sha)` fully determines a run.
2. **Auditability:** every change is captured in two places — the
   snapshot file content, AND the audit log.
3. **Approval workflow:** state transitions are explicit.
   Cannot accidentally promote a draft to production.
4. **Lineage:** every snapshot points to its parent, forming a tree.
   Ancestry of any production snapshot is fully traceable.
5. **Code/config decoupling:** team members with different roles can
   work on each axis without blocking each other.

## Things this design intentionally does NOT do

- **No automatic upgrade between schema versions.** If a major version
  changes, old snapshots may stop running on new code. Migrate
  explicitly.
- **No git-style branches/merges.** Snapshots fork from a parent but
  don't merge back. Two parallel drafts that need to combine require a
  manual third snapshot copying from both.
- **No file-level history within a snapshot.** A snapshot is one frozen
  state. To see the history, look at the chain of snapshots.
- **No automatic retention/expiry.** Old snapshots stay forever unless
  manually removed.
