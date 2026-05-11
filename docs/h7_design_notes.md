# H7+ Design Notes (captured 2026-05-06, updated post-provenance review)

Working notes for upcoming refactors and the Shiny build. None of this is
implemented yet — captured here so we don't lose context between sessions.

---

## H7 IMPLEMENTATION STATUS — variable dictionary + model registry

**STATUS: implemented 2026-05-06.** Files added:

- `config/variable_dictionary.yml` — 6 variables registered:
  - VAR_NON_OIL_GDP_GROWTH
  - VAR_QATAR_REAL_ESTATE_INDEX_GROWTH
  - VAR_QATAR_DOMESTIC_CREDIT_GROWTH
  - VAR_GCC_REAL_GDP_GROWTH (synthetic, country-weighted)
  - VAR_GCC_REAL_GDP_GROWTH_BY_COUNTRY (panel input)
  - VAR_GCC_NOMINAL_GDP_BY_COUNTRY (panel weight)

- `config/models.yml` — 3 models registered:
  - `internal_v4_production` — V4 byte-equivalent, weight=[1,0,0]
  - `internal_3var_pdf` — same coefs, weights derived from p-values
  - `external_gcc_v7` — single-MEV external

- `R/load_variables.R` — loaders + resolver. `resolve_model_config(model_id)`
  produces the legacy `model_cfg$model$mevs` shape so existing consumers
  (pd_term_structure, macro_model) didn't change.

- `config.yml` updated:
  - paths.variable_dictionary, paths.models added
  - run.internal_model: "internal_v4_production"
  - run.external_model: "external_gcc_v7"

- `load_model_config()` now routes: if config.yml has `paths$models`
  defined, resolves via models.yml; otherwise reads legacy file with a
  deprecation warning.

- Metadata headers added to:
  - `data-raw/static/non_oil_gdp_history.csv`
  - `data-raw/static/gcc_real_gdp_growth.csv`
  - `data-raw/static/gcc_gdp_current_prices.csv`
  - `read_csv` calls now pass `comment = "#"` so headers are ignored.

- `config/model_config.yml.deprecated.yml` is the archive copy.

**Verified equivalence:** internal_v4_production resolves to the exact
same numeric fields as the legacy `model_config.yml`. Only the display
name differs (cosmetic — old "Non-Oil Real GDP (% Change)" vs new
"Non-Oil Real GDP Growth (%)"). Verified safe — no code paths match
on this string.

**Outstanding hardening:** mev_forecasts.yml column order is still
positionally aligned to mev_components order. Documented in
`load_config.R::load_model_inputs`. Future work: index forecast columns
by variable_id from the dictionary (kind=model_inputs_yaml +
column_index field is already in the dictionary entries; just need to
wire it through).

---

## Provenance corrections applied (2026-05-06)

User flagged five mappings in the original provenance doc that needed
fixing. All verified by direct V4 cell inspection and corrected in
`docs/static_provenance.md`:

| File | Was (wrong) | Corrected |
|---|---|---|
| `ttc_pd_table.csv` | `Assumptions!H4:I24` | `Inputs_Lending Portfolio!AT5:AU25` |
| `ttc_pd_table_external.csv` | `Assumptions!K4:L24` | `Inputs_Investment Portfolio!P5:Q25` |
| `collateral_types.csv` | `Assumptions!T4:Z29` | `Inputs_Lending Portfolio!AG4:AJ26` |
| `product_portfolio_mapping.csv` | `Assumptions!Y:Z` | `Inputs_Lending Portfolio!AL5:AP15` |
| `off_balance_products.csv` | "VBA + manual" | `Assumptions!B3:C10` (clean source — WTO=8 we added, not in V4) |

**Cleanup performed:**
- Deleted `gcc_real_gdp_forecast.csv` and `gcc_nominal_gdp_forecast.csv` —
  duplicates of `model_inputs.yml.external_gcc_country_growth/prices`.
- Override CSVs migrated to audit-column schema (see below).

---

## Override schema with audit columns

Now in effect across all three override files. Schema:

```
customer_id, <override_field>, reason,
created_by, created_at, approved_by, approved_at,
status, prior_value, source_run_id
```

`active_overrides()` helper in `load_static.R` filters to `status == "approved"`
at run time. Other statuses (draft / pending_approval / superseded / revoked)
remain in the file for audit history but don't affect output.

**Existing V4 rating override (CIF 31884) backfilled** with
`created_by=v4_import`, `status=approved`, `created_at=2026-05-06T00:00:00Z`.

---

## 1. Variable dictionary for the macro PD model

**Why:** when the model is recalibrated or the active variable set changes
(e.g. switching from "Non-Oil GDP only" to a 3-variable blend), today we
have to edit code in multiple places. We want config-only changes.

**Proposed:** a single `config/variable_dictionary.yml` that defines every
macroeconomic variable used anywhere in the pipeline by a stable code
(VAR_001, VAR_002, ...). Models then reference variables by code, not by
free-text name.

```yaml
# config/variable_dictionary.yml
variables:
  VAR_001:
    name: "Non-Oil Real GDP (% Change)"
    units: percentage_points
    scale: native     # value is already the percentage (e.g. 4.44 = 4.44%)
    stress_unit_multiplier: 1
    history_source: data-raw/static/non_oil_gdp_history.csv
    forecast_source: model_inputs.mev_forecasts.column_index_or_var_id
    description: "Qatar Non-Oil Real GDP year-on-year growth"

  VAR_002:
    name: "Qatari Real Estate Index Growth"
    units: fractional
    scale: percent_to_fraction   # value 0.05 means 5%
    stress_unit_multiplier: 100
    history_source: null   # no historical series tracked
    forecast_source: model_inputs.mev_forecasts.col_2

  VAR_003:
    name: "Qatari Domestic Credit Growth"
    units: fractional
    scale: percent_to_fraction
    stress_unit_multiplier: 100

  VAR_GCC_GDP_GROWTH:
    name: "Country-weighted GCC Real GDP Growth"
    units: percentage_points
    scale: native
    stress_unit_multiplier: 1
    derivation: "SUMPRODUCT(country_growth, country_gdp_share)"
    history_source: data-raw/static/gcc_real_gdp_growth.csv
    forecast_source: model_inputs.external_gcc_country_growth
    weight_source:   model_inputs.external_gcc_country_prices
```

Then model definitions become:

```yaml
# config/models.yml
models:
  internal_3var:
    description: "PDF methodology — 3-variable blended"
    portfolios: [Business Finance, Off BS, Al Dhameen, Tasdeer]
    rating_type: Internal
    ttc_anchor_pd: 0.146
    mev_components:
      - variable: VAR_001
        intercept: -1.5475
        coefficient: -0.0414
        p_value: 0.08
        standard_deviation: 9.8877
        weight: 0.55     # or null → derive from p_value
      - variable: VAR_002
        intercept: -1.7120
        coefficient: -0.9005
        p_value: 0.165
        standard_deviation: 0.2195
        weight: 0.27
      - variable: VAR_003
        intercept: -1.2712
        coefficient: -4.5845
        p_value: 0.241
        standard_deviation: 0.1602
        weight: 0.18

  internal_v4_production:
    description: "V4 production — Non-Oil GDP only"
    portfolios: [Business Finance, Off BS, Al Dhameen, Tasdeer]
    rating_type: Internal
    ttc_anchor_pd: 0.146
    mev_components:
      - variable: VAR_001
        intercept: -1.5475
        coefficient: -0.0414
        p_value: 0.08
        standard_deviation: 9.8877
        weight: 1.0

  external_gcc:
    description: "External portfolios — GCC GDP single variable"
    portfolios: [Banks and Fis, Investments]
    rating_type: External
    mev_components:
      - variable: VAR_GCC_GDP_GROWTH
        # ... etc
```

Selecting which model to run becomes one line in `config.yml`:

```yaml
run:
  internal_model: internal_v4_production    # or internal_3var
  external_model: external_gcc
```

**Benefit:** to recalibrate, edit one block in `models.yml`. To add a new
variable, register it in `variable_dictionary.yml` and reference its code.
No code changes anywhere.

**Action:** plan implementation in H7. Migration path:
1. Author the two YAMLs from existing `model_config.yml` + `model_inputs.yml`.
2. Refactor `pd_term_structure.R` and `macro_model.R` to look variables up
   by code.
3. Keep current YAMLs as a thin shim that re-emits the new format, so
   existing test outputs don't change.

---

## 2. Shiny app architecture

### Two main flows
1. **Run a pipeline** — load → review static + config → confirm → run
   → see validation → see outputs → reconciliation.
2. **Manage configurations** — view / edit / version static and config
   files; compare snapshots; pin a snapshot for re-run.

### Versioned snapshots
- A snapshot = a directory under `config_snapshots/<timestamp>_<label>/`
  containing copies of every file in `data-raw/static/` and `config/`,
  plus a `manifest.yml` with metadata (who, when, why, code git SHA).
- Naming convention: `2026-Q1-final/`, `2026-Q2-draft/`, etc.
- Run flow: user selects a snapshot from a dropdown, run_etl reads from
  that snapshot directory instead of the live `data-raw/static/` and
  `config/`.
- Code version: each snapshot also stores the Git commit SHA of the
  R code at the time it was created. If the user selects an older
  snapshot today, we WARN if the current code commit differs from the
  one stored — and offer to checkout the matching code (or proceed
  with current code at user's risk).

### Step-by-step run UI
Walk-through layout (one panel at a time, "Confirm & continue" buttons):

1. **Load** — show selected snapshot, input file list with sizes/hashes,
   override file row counts. User confirms.
2. **Static review** — show every static CSV in editable DataTables
   read-only at first; user can click "Edit" on any row, edits go into
   the next snapshot. User confirms.
3. **Config review** — same for `model_config.yml`, `model_inputs.yml`.
4. **Pre-run validation** — runs the input validators (currently 28 in
   `validators_input.R`). Display a table:
     - PASS rows: green checkmark, hidden by default behind "show passed"
     - WARN rows: yellow row, full message, user must acknowledge
     - ERROR rows: red row, "Stop" — user cannot proceed
5. **Run** — progress bar, log stream, validators run after each major
   stage (transform/derived) with the same severity gating.
6. **Output review** — 18-tab DataTable for each output CSV; user can
   spot-check a file. Reconciliation results in a side panel.
7. **Sign-off** — user types reason and clicks "Approve". Approved run
   gets a permanent snapshot under `runs/<timestamp>_approved/`.

### Audit log
A single append-only log file at `logs/etl_audit.log` (rotate daily).
Every action gets a timestamped entry:

```
2026-05-06T14:23:01Z  user=anand  event=snapshot_select  snapshot=2026-Q1-final
2026-05-06T14:23:42Z  user=anand  event=static_edit  file=customer_rating_overrides.csv  before_hash=ab12... after_hash=cd34... change="+1 row"
2026-05-06T14:24:05Z  user=anand  event=run_start     snapshot=2026-Q1-final  code_sha=abc1234
2026-05-06T14:24:18Z  user=anand  event=validation    severity=WARN  validator=v_pd_consistency  message="..."  acknowledged=true
2026-05-06T14:24:55Z  user=anand  event=run_finish    output_dir=runs/2026-05-06_142401/  duration=37s  status=ok
2026-05-06T14:25:11Z  user=anand  event=approve       run_dir=runs/2026-05-06_142401/  reason="Q1 close"
```

Format suggestion: structured (one JSON object per line — easier to
filter / search later).

### Override-edit workflow
- Override edits in the UI go to the active snapshot's CSV, NOT the
  live `data-raw/static/` files.
- The active snapshot is "in draft" until the user clicks "Save snapshot
  as v2026-Q1-draft-2".
- Multiple users can have parallel drafts; one snapshot is "promoted"
  to production via a separate Approve action.

### Static editing scope
- Override CSVs (rating, stage, restructuring) — full edit support, add/
  remove rows.
- Other statics — edit cell values only, NOT structure (no add/remove
  columns). Rationale: column changes need a code change anyway.
- Big tables (master_rating_scale, ttc_pd_table) — show + edit, but
  these should change rarely, so add an extra confirmation step.
- Calibration outputs (TTC PD, MEV coefficients) — should NOT be edited
  by hand from the UI. Add an "Import calibration results" workflow that
  takes a calibration output file and overwrites the relevant fields.
  Hand-edit is allowed but requires typing the word "OVERRIDE" in a box.

---

## 3. Validation gating

Currently we have 85 validators across 3 modules. They all run, none
of them stop the pipeline. Need to:

1. **Add a severity field** to each validator: ERROR / WARN / INFO.
2. **Stage gating** — group validators by which stage they should run
   after (input, transformed, derived). Run each group, halt on ERROR
   if any in that group failed.
3. **Acknowledgement** — WARN-level findings should be displayed and
   require explicit user "Acknowledge & continue". Decision recorded
   in audit log.
4. **Suppression rules** — sometimes a known WARN is acceptable (e.g.
   "9653 trailing empty rows in LifeTimeParameterOther" — that's the
   V4 bundle's quirk, not ours). Allow per-snapshot suppression: a
   `validation_suppressions.yml` listing the validator IDs to ignore
   with a justification.

---

## 4. Other Shiny improvements

- **Diff snapshots** — pick two snapshots, see field-by-field deltas
  (textual diff for YAMLs, row-level diff for CSVs).
- **Re-run with edits** — view a past run, click "Re-run with current
  edits" to clone its snapshot, apply edits, run.
- **Failure replay** — store the manifest + intermediate tibbles for
  failed runs, so a user can come back to a failed run and continue
  from the last-good stage rather than from scratch.
- **Tooltip everywhere** — every static value should have a tooltip
  saying where it came from (using `static_provenance.md` content as
  the source).
- **Read-only viewer mode** — non-admin users should be able to inspect
  any past run + manifest but not edit/run.
- **"What-if"** — let user clone the current snapshot, change one or two
  values, run, compare outputs side-by-side without affecting the live
  snapshot.

---

## 5. Things to clean up before Shiny

These are small refactors that will make the Shiny build easier:

1. **Drop static CSVs that duplicate YAML** — `gcc_real_gdp_forecast.csv`
   and `gcc_nominal_gdp_forecast.csv` overlap with
   `model_inputs.yml.external_gcc_country_growth/prices`. Pick one.
2. **Consolidate `portfolios.csv` into config** — it's small and
   methodology-tied, doesn't belong in static reference.
3. **Promote the variable dictionary to first-class** before Shiny needs
   to render variable lists.
4. **Per-validator metadata** — author short descriptions for each
   validator so the Shiny UI can show meaningful messages.

---

## Phase sequence

- **H7** — variable dictionary + model registry refactor (no behavior
  change, just structural). Migrate `model_config.yml` content into
  `models.yml` + `variable_dictionary.yml`.
- **H8** — validation severity + gating. Per-validator metadata.
- **H9** — config snapshot system (filesystem layout, snapshot manager).
- **H10** — Shiny app: read-only first (browse, inspect). Add edit +
  run later.
- **H11** — Shiny app: edit, run, sign-off.
- **H12** — Investigations side: investment-side (`*_2`) reconciliation
  drift, any deferred methodology items.
