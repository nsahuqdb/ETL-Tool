# Static Data & Config Provenance (CORRECTED 2026-05-06)

This document maps every static CSV and YAML config value to its source in
the V4 / V7 Excel workbook. Use this as a checklist before signing off a
Shiny config-snapshot release.

**Correction note (2026-05-06):** Earlier draft had several values mapped
to `Assumptions` sheet that actually live in `Inputs_Lending Portfolio` or
`Inputs_Investment Portfolio`. The corrected mapping below was verified by
direct cell inspection of V4.

---

## Stability legend

- **STATIC** — Authoritative reference data. Changes infrequently.
- **CALIBRATED** — Output of a model calibration run.
- **REFRESHED** — Updated each reporting cycle.
- **OPERATOR** — Edited by analyst per case (overrides).

---

## Static CSVs in `data-raw/static/`

### Master Reference Tables

| File | V4 source | Stability |
|---|---|---|
| `master_rating_scale.csv` | `MasterRatingScale!B4:G45` (sheet 14) | STATIC |
| `staging_thresholds.csv` | `Assumptions!E5,E6,E7` and `Assumptions!L3` | STATIC |
| `industry_sector_mapping.csv` | `Assumptions!E:G` (146 industry codes) | STATIC |
| `scenario_severity.csv` | `Inputs_Lending Portfolio!AC5:AD9` (NORM.S.INV constants) | STATIC |
| `fx_rates.csv` | `FxRateLoad` sheet (sheet 22) | STATIC |
| `portfolios.csv` | Hardcoded enumeration of 6 portfolios | STATIC |
| `non_oil_gdp_history.csv` | Qatar Planning & Stats Authority (external) | REFRESHED |

### Calibration Outputs (lives in Inputs_Lending Portfolio / Inputs_Investment Portfolio)

| File | V4 source | Stability |
|---|---|---|
| **`ttc_pd_table.csv`** (Internal QDB) | **`Inputs_Lending Portfolio!AT5:AU25`** (Rating Grade + PD 12-m) | **CALIBRATED** |
| **`ttc_pd_table_external.csv`** (Moody's) | **`Inputs_Investment Portfolio!P5:Q25`** (Rating Grade + PD 12-m) | **CALIBRATED** |
| **`collateral_types.csv`** | **`Inputs_Lending Portfolio!AG4:AJ26`** (Collateral ID, Collateral Type, Assessment Guidance, Collateral Value) | **CALIBRATED** (haircuts) |
| **`master_rating_downgrade.csv`** | `MasterRatingScale!J:K` lookup table — but the "apply downgrade" toggle is **`Inputs_Lending Portfolio!AW5`** ("No downgrade" in V4) | STATIC + CALIBRATED |
| **`product_portfolio_mapping.csv`** | **`Inputs_Lending Portfolio!AL5:AP15`** (cols AL=product type, AP=AccountType / portfolio). Plus 5 team-added rows for product types observed in production but absent from V4: Bai Al Wadiya, Fisheries, Greenhouse, Microfinance, Paid guarantees → all → Business Finance (decided 2026-05-06). Each row carries a `source` column ("v4" or "team_added") so the audit trail is in the file itself. | STATIC |

### Methodology / Assumptions sheet

| File | V4 source | Stability |
|---|---|---|
| **`off_balance_products.csv`** | **`Assumptions!B3:C10`** (8 rows: APG=1, BGA=2, FGG=3, ILC=4, PGG=5, DHG=6, TIG=7, WTO=8 — all from V4) | STATIC |
| **`segment_fallback_ratings.csv`** | **`Inputs_Lending Portfolio!S5:T6`** for lending (Unrated→QDB 5, Al Dhameen→QDB 6) + **`Inputs_Investment Portfolio!N5`** for investments (Baa3) | STATIC |
| `collective_assessment_rules.csv` | `Assumptions!I4:K15` (sheet 6) | STATIC |

### Macro / GDP Data (REFRESHED)

| File | Source |
|---|---|
| `gcc_real_gdp_growth.csv` | IMF WEO database (history) |
| `gcc_gdp_current_prices.csv` | IMF WEO database (history) |
| `gcc_real_gdp_forecast.csv` | **DELETE** — duplicated `model_inputs.yml.external_gcc_country_growth` |
| `gcc_nominal_gdp_forecast.csv` | **DELETE** — duplicated `model_inputs.yml.external_gcc_country_prices` |

### Operator Overrides (audit-tracked, edited via Shiny)

See "Override schema with audit columns" below for full column list.

| File | V4 source |
|---|---|
| `customer_rating_overrides.csv` | `Inputs_Lending Portfolio!E5:G` (Rating Override col E + Rating Override Status col G) |
| `customer_stage_overrides.csv` | `Inputs_Lending Portfolio!O5:Q` (Backward Transition Override col O + Staging Override Status col Q) |
| `customer_restructuring_overrides.csv` | `Inputs_Lending Portfolio!I5:K` (Restructuring due to Financial difficulty col I + Restructuring Override Status col K) |

---

## Configs (`config/*.yml`)

### `config/model_config.yml`

| Key | V4 source | Stability |
|---|---|---|
| `model.name` | Free-text descriptive | OPERATOR |
| `model.ttc_anchor_pd: 0.146` | `Calculations!D31` constant | CALIBRATED |
| `model.max_maturity: 50` | `Calculations!H3` | STATIC |
| `model.n_forecasts: 5` | `Calculations!G3` | STATIC |
| `model.mevs[*].name` | `Model Specifications!C5:C7` (sheet 48) | STATIC |
| `model.mevs[*].intercept` | `Model Specifications!D5:D7` | CALIBRATED |
| `model.mevs[*].coefficient` | `Model Specifications!E5:E7` | CALIBRATED |
| `model.mevs[*].p_value` | `Model Specifications!F5:F7` | CALIBRATED |
| `model.mevs[*].weight` | `Model Specifications!G5:G7` | CALIBRATED (V4: [1,0,0]) |
| `model.mevs[*].standard_deviation` | `Model Specifications!H5:H7` | CALIBRATED |
| `model.mevs[*].stress_unit_multiplier` | Hardcoded by us (1 for GDP, 100 for percentage MEVs) | STATIC |

### `config/model_inputs.yml`

| Key | V4 source | Stability |
|---|---|---|
| `internal_scenario_weights.mode` | Methodology choice | STATIC |
| `internal_scenario_weights.n_forecast_years` | `Inputs_Lending Portfolio!AE2` | STATIC |
| `internal_scenario_weights.explicit_weights` | `Inputs_Lending Portfolio!AE5:AE9` | REFRESHED |
| `external_scenario_weights.mode` | Methodology choice | STATIC |
| `external_gcc_country_growth` | `Calculations!D38:H43` (V7) | **REFRESHED** — single source of truth |
| `external_gcc_country_prices` | `Calculations!D46:H51` (V7) | **REFRESHED** — single source of truth |
| `mev_model_weights.mode` | Methodology choice | STATIC |
| `mev_forecasts.forecasts` | `Inputs_Lending Portfolio!W5:Y9` | REFRESHED every cycle |

### `config.yml`

Paths only. STATIC per deployment.

---

## Override schema (with audit columns)

All three override CSVs share a common audit-trail header.

```
customer_id,<override_field>,reason,created_by,created_at,approved_by,approved_at,status,prior_value,source_run_id
```

Per-file `<override_field>` name:

| File | override_field column |
|---|---|
| `customer_rating_overrides.csv` | `override_rating` |
| `customer_stage_overrides.csv` | `override_stage` |
| `customer_restructuring_overrides.csv` | `override_restructuring` |

Field semantics:

| Column | Type | Required | Description |
|---|---|---|---|
| `customer_id` | char | yes | CIF being overridden |
| `<override_field>` | char | yes | The override value (e.g. "QDB 1") |
| `reason` | char | yes | Free-text justification |
| `created_by` | char | yes | Username of person creating the override |
| `created_at` | datetime ISO 8601 | yes | When override was first created |
| `approved_by` | char | optional | Username of approver (empty until approved) |
| `approved_at` | datetime ISO 8601 | optional | When approval happened |
| `status` | enum | yes | One of: `draft`, `pending_approval`, `approved`, `superseded`, `revoked` |
| `prior_value` | char | optional | Value before this override (enables unwind) |
| `source_run_id` | char | optional | Run-id where prior value was observed |

**Only rows with `status = approved` are applied at run time.** Rows with
other statuses stay in the file for audit history but don't affect output.

---

## Cleanup actions taken

1. **Deleted** `gcc_real_gdp_forecast.csv` and `gcc_nominal_gdp_forecast.csv`
   from `data-raw/static/`. YAML is the single source of truth.
2. **Override CSVs migrated** to new audit-column schema. Existing V4 rating
   override row (CIF 31884) backfilled with `created_by=v4_import`,
   `status=approved`, `created_at=2026-05-06T00:00:00`.

---

## Verified DERIVED leakage status

After corrections, no DERIVED-leakage candidates remain.
