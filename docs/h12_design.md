# H12 design notes — mid-run pause + run-level approval

This doc captures the design decisions made in H12 of the IFRS9 ETL R port:
the pipeline pauses for user overrides mid-run, and approval happens at the
run level rather than the override level. It exists so a reviewer landing
on this codebase cold can understand *why* the architecture looks the way
it does, and so future maintainers don't accidentally undo decisions that
were deliberate.

## TL;DR

- The pipeline runs in two phases. Phase 1 produces calculated values
  (rating, stage, restructuring) per customer and pauses. Phase 2 applies
  user overrides and finalises outputs.
- Overrides are per-run, not global. They live in
  `runs/<run_id>/overrides/{rating,stage,restructuring}_overrides.csv`.
- Approval is per-run. A reviewer signs off on the entire run including
  any overrides applied during it. This is enforced through a
  `run_status.yml` lifecycle: `pending_approval → approved | rejected`.
- Snapshots no longer freeze override CSVs. They freeze configuration
  (`config/`, `data-raw/static/`) only.

## What problem this solves

The V4 Excel workbook supported overrides via direct cell edits. A user
could open the workbook, look at the calculated columns, and manually
adjust ratings or stages where business knowledge contradicted the
mechanical calculation. The R port needed an equivalent capability without
losing the audit trail Excel never had.

Three properties were non-negotiable:

1. **Override decisions need calculated values as context.** Telling a user
   "override customer X to QDB 1" without showing what the calculated
   rating currently is forces them to either guess or rerun the pipeline
   twice (once to see, once to apply). That's friction at the workflow's
   most sensitive point.

2. **Every override needs a captured reason and approver.** Excel had
   neither. Whatever we built had to record who decided what and why.

3. **Runs must be reviewable as a unit.** A reviewer signs off on the
   model output that gets sent to finance/regulators, not on individual
   override rows in isolation. The artifact that lands in their inbox is
   "this run produced these 18 CSVs" — that's what they accept or reject.

## The design space we considered

### Option A — Mid-run pause for decisions (chosen)

The pipeline runs Phase 1 (load + validate + transform + portfolio view),
then pauses with `cm_view` ready. The Shiny app shows the calculated
rating/stage/restructuring per customer. The user adds overrides directly,
each with a required reason. When they click Continue, Phase 2 applies the
overrides, runs the derived steps (LTPO, PD term structures, StPD), writes
the 18 outputs, and lands the run as `pending_approval`.

A reviewer separately approves or rejects the whole run.

**Pros:**
- Override decisions are made with calculated values visible, exactly the
  way a credit officer thinks about them.
- The natural unit of approval — the model output — matches what reviewers
  actually care about.
- Override files become per-run artifacts. Each run is a self-contained
  audit packet: inputs + config + overrides + outputs.

**Cons:**
- Runs are not directly reproducible: a future replay needs the exact
  override file from the original run, not just the snapshot. We solve
  this by treating "run + its override files" as the reproducibility unit
  (see "Reproducibility" below).
- The Shiny session must hold intermediate state between Phase 1 and
  Phase 2. We use a `reactiveVal` for this — no async, no database. A
  user closing the browser between phases drops their work.

### Option B — Edit-then-run cycle (rejected)

The user edits override CSVs to disk, promotes them through a
draft → pending → approved lifecycle (parallel to snapshots), then runs
the pipeline. The pipeline reads only `status == approved` rows.

**Why rejected:** the user can't see the calculated value before deciding
to override it. They'd either need to run the pipeline once with no
overrides to see the calculations, then a second time with overrides
applied — or they'd need to override blindly. The former is wasteful, the
latter is dangerous (reviewers can't catch override mistakes that look
locally reasonable but contradict the calculation).

This was actually our initial design and we shipped it through H10/H11
before recognising the gap. The override CSVs in `data-raw/static/` are
remnants of that era; see "Legacy override files" below.

### Option C — Run, browse, override, re-run (rejected)

The pipeline runs to completion with no overrides. The user browses the
output, decides where to override, edits an override file, and triggers a
new run that produces a corrected output.

**Why rejected:** every override-bearing run requires two full pipeline
executions. With ~30 second runs, that's a 60-second feedback loop per
override decision. Worse, the two outputs (with and without override)
both exist on disk, creating "which is the real one?" confusion.

## Why approval is run-level, not override-level

Earlier designs put approval on individual override rows: each override
goes through `draft → pending → approved` and the pipeline only applies
approved rows. We rejected this in H12 because:

1. **Reviewers approve outputs, not inputs.** When a reviewer signs off,
   they're saying "I accept this run's CSVs as the model output for
   period X." Whether that involved 0 overrides or 50 doesn't change
   what they're approving. The approval should attach to the output.

2. **Override-level approval forces sequenced workflows.** Under the
   row-level model, you'd need: user edits override → user submits for
   approval → approver approves → pipeline run. That's three handoffs.
   Under run-level approval: user runs pipeline, makes overrides
   mid-run, approver reviews the result. One handoff.

3. **The audit trail still holds.** Each override row captures
   `created_by, created_at, prior_value, reason, source_run_id`. The
   reviewer sees exactly what was overridden and why. They just don't
   approve each row individually.

A consequence: a reviewer rejecting a run effectively rejects every
override applied during that run. There's no "approve some, reject
others" — the unit of decision is the whole run. If the user wants
different overrides, they re-run.

## How overrides actually flow to the output

This took two iterations to get right; recording it here so future
maintainers don't make the same mistake.

The lending pipeline has multiple "rating" columns:

- `trans_l$rating_worst` — the **master** per-contract rating after
  the legacy static-CSV override pass. AccountMaster_1.csv reads this
  via `joined$rating_worst` in the output writer.
- `trans_l$rating_after_override` — a derived view; mirrors `rating_worst`
  for overridden customers.
- `cm_view$rating_final` — the customer-level final rating used by
  downstream stage-decision logic and the StPD bucket lookup.

The first H12 implementation rewrote only `rating_after_override`, which
the output writer doesn't read. Result: overrides recorded in the audit
file but NOT in the output CSVs. The fix in `run_etl_phase2` is to
rewrite `trans_l$rating_worst` (broadcast to every contract belonging
to the overridden customer, since the override is at customer level)
AND `cm_view$rating_final`.

For stage and restructuring, the master columns are simpler:
`cm_view$stage_final` and `cm_view$restructuring_final`. The
`build_customer_flags()` helper runs after the override block in Phase 2
and reads these columns directly, so customer staging flags pick up
overrides automatically.

The general pattern: **find the master column the output writer reads,
overwrite that one**. Don't trust intermediate "after_override" columns
that look like they should propagate downstream — verify they actually do.

## Override dropdown constraints

The Shiny override editor enforces business rules on what override
values are offered:

- **Rating** — the 21 internal QDB ratings drawn from
  `master_rating_scale.csv` (`rating_type == "Internal"`), ordered by
  hierarchy best-to-worst. Lending is all-internal in this workbook;
  external ratings are for investments only and not user-overrideable.
- **Stage** — only worsening transitions allowed. Stage 1 → {Stage 2,
  Stage 3}; Stage 2 → {Stage 3}; Stage 3 has no override option (an
  italic note explains why). This matches IFRS 9 staging semantics:
  a credit officer can override "this customer is worse than the
  mechanical calculation suggests" but cannot override the other way.
- **Restructuring** — flip-only. If currently "Restructured", offer
  "Not Restructured" and vice versa.

These constraints are enforced in the UI; the R code in
`run_etl_phase2` does not re-check them. A power user calling
`run_etl_phase2(state, overrides=list(...))` directly could bypass
them. We chose UI-level enforcement because the override file format
itself doesn't have a place to record "this transition was vetted as
legal" — easier to make it impossible to enter an illegal override
than to validate every override row.

## Approval History tab

`mod_approval_queue.R` exposes two tabs:

- **Pending** — runs in `pending_approval`. Reviewer-actionable.
- **History** — every run that has been approved or rejected. Read-only
  audit view.

The History view denormalises each run's `run_status.yml` transitions:
the first transition (`to=pending_approval`) is the requester; the last
transition matching the current status is the deciding one. So the
History row carries `requested_by, requested_at, request_comment,
decided_by, decided_at, decision_comment` — the four-way audit trail
without any schema changes.

## What snapshots freeze (and what they don't)

Snapshots freeze configuration that should be reproducible:

- `config/config.yml` — pipeline configuration (paths, run policy, staging
  thresholds)
- `config/models.yml` — model registry
- `config/variable_dictionary.yml` — macroeconomic variable definitions
- `config/model_inputs.yml` — scenario weights, forward MEV forecasts
- `data-raw/static/` — TTC PD tables, master rating scale, scenario
  severity, GCC GDP history, etc.

Snapshots **do not** freeze:

- Input data (`input/*.xlsx`) — these are the period's raw extracts; they
  vary every run by design
- Override CSVs — these are per-run, stored under `runs/<run_id>/overrides/`,
  not part of the snapshot

This is a deliberate change from earlier iterations. Initially snapshots
were going to freeze override CSVs as part of the "authoritative
configuration set". When we moved to run-level approval (overrides become
per-run artifacts), the override files no longer belong inside snapshots.

## Reproducibility

A run is identified by:

- **Code SHA** — git SHA of the pipeline code at run time. Recorded in
  manifest and audit.
- **Snapshot label** — the `config_snapshots/<label>/` used (or "live config"
  if none). Recorded in manifest, audit, run-status.
- **Input directory contents** — input filenames + sizes recorded in
  manifest. Not hashed, but enough to detect "did the inputs change".
- **Override files** — at `runs/<run_id>/overrides/`. Per-run; cannot
  be implied from the snapshot.

To replay a run identically, you need all four. We don't have a
`run_etl(replay = "<run_id>")` shortcut yet — that's H15+ work — but the
information is captured.

A run with zero overrides IS reproducible from snapshot + code SHA alone.
A run with overrides needs its override files preserved. This is why
runs go to `runs/<run_id>/` and are kept indefinitely (no auto-cleanup).

## Legacy override files

`data-raw/static/customer_*_overrides.csv` exists with three files:
`customer_rating_overrides.csv`, `customer_stage_overrides.csv`,
`customer_restructuring_overrides.csv`. These date from the H7-H10 era
when overrides were configuration (frozen in snapshots).

The phased orchestrator (`run_etl_phase2`) does NOT read them. The
legacy `run_etl()` (used by the test runner) still does, for backward
compatibility with `tests/manual/test_phase_h5.R`.

In a future phase we should:
- Move these files to `data-raw/legacy/` to make their status clear
- Drop the legacy reads from `transform_lending` and `lending_portfolio_view`
  once no test runner depends on them

## State management in the Shiny app

Phase 1 → Phase 2 state lives in `app/modules/mod_run_trigger.R` as
`reactiveVal`s. There are three:

- `phase1_state` — the full state list returned by `run_etl_phase1()`,
  including `cm_view`, `trans_l`, `inv_view`, `trans_i`, `static`, etc.
  This is the in-memory snapshot of "where we paused".
- `overrides_rating` / `overrides_stage` / `overrides_restr` — the user's
  pending override rows, accumulated as they click "Add override".
- `phase2_result` — populated when Phase 2 finishes, drives the summary
  page.

A `phase_state` reactiveVal acts as the state-machine selector:
`"idle" | "phase1_done" | "phase2_done"`. `output$page_body` is a
`renderUI` that switches on it.

This works well for one user. For multi-user concurrent runs, each
session has its own state — Shiny's per-session model handles isolation
naturally. The only shared state is on disk (the `runs/` directory and
audit log), and writes there are atomic (write-temp + rename).

## Failure modes and what happens

| Failure | What happens |
|---|---|
| User closes browser between Phase 1 and Phase 2 | Run state is lost. No partial files written. The `run_id` was reserved but no `runs/<run_id>/` directory exists. Audit has `run_start` and `run_phase1_complete` but no `run_finish`. |
| Phase 1 errors (e.g. validation halt) | `run_etl_phase1()` throws. The Shiny notification surfaces the error. State stays at `idle`. No partial outputs. |
| Phase 2 errors (e.g. derived calculation crashes) | Run directory exists with whatever was written before the crash. `run_status.yml` is NOT written, so it doesn't appear in the Approval queue. Audit has `run_overrides_applied` but no `run_finish`. |
| User adds an override for a customer that doesn't exist in `cm_view` | `match()` returns NA, the override is silently ignored. Override file still records the row (with empty `prior_value`). This is a known minor bug; should produce a warning. |
| Reviewer approves a run, then realises a mistake | No reverse. The audit log has both the approve event and any subsequent investigation. To "undo" a run, the team would re-run with corrected overrides; the original approved run remains in history. |

## Things H12 explicitly defers

- **Async Phase 2.** Phase 2 takes ~25-30s on a typical workbook. The UI
  locks during this. Acceptable for now; could move to `promises`+`future`
  if it becomes an irritation.
- **Override on investment customers.** The pause page only shows lending
  customers. Investment-side rating/stage isn't user-overrideable. The
  V4 workbook didn't have an investment override flow either.
- **Editing overrides after Continue.** Once Phase 2 starts, overrides
  are committed. To change them, the user cancels the run and starts over.
  No "edit and resume" path.
- **Investments-side reconciliation drift** — flagged in NOTES.md;
  non-blocker.

## Files

| File | Role |
|---|---|
| `R/run_etl_phased.R` | `run_etl_phase1()` + `run_etl_phase2()` |
| `R/run_approval.R` | `read_run_status`, `list_runs_pending_approval`, `list_runs_decided`, `approve_run`, `reject_run` |
| `R/run_etl.R` | Legacy single-shot orchestrator (preserved for tests) |
| `app/modules/mod_run_trigger.R` | Run pipeline page (idle → pause → summary state machine) |
| `app/modules/mod_approval_queue.R` | Pending + History tabs |
| `runs/<run_id>/overrides/*.csv` | Per-run override audit files |
| `runs/<run_id>/reports/run_status.yml` | Run lifecycle status |
