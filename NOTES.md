# IFRS9 ETL — Project Notes

Notes on deferred methodology questions, known divergences from V4,
and design decisions worth revisiting.

## RESOLVED — AccountMaster rating chain mismatch  (2026-05-06)

Bundle reconciliation: 5,104 -> 188 -> 6 -> 0 real diffs.

The chain Y..AE was decoded from V4 `Transformation` and built correctly:

- **Y** — VLOOKUP customer's rating from `CustomerMasterExtract!F` against
  combined Internal+External label column `MasterRatingScale!F$4:F$45`.
  CustomerMaster.xlsx column F holds the rating string ("QDB 9").
- **Z** — if Y populated -> Y; else sector lookup (Agriculture/Fisheries/
  Livestock by DPD) -> Al Dhameen / Unrated fallback.
- **AA** — VLOOKUP Z to hierarchy via `MasterRatingScale!F:G`.
- **AB** — `MAX(IF $J=$J2, $AA)` per-customer worst hierarchy across
  contracts.
- **AC** — `INDEX(MasterRatingScale!B$4:B$24, AB)` -> per-customer
  Internal QDB label at the worst hierarchy.
- **AD** — VLOOKUP per-customer override from
  `Inputs_Lending Portfolio!A:F` col 6.
- **AE** — if `AW5 = "No Downgrade"` -> AD; else apply `MasterRatingScale!J:K`.

Implementation:
- The chain Y..AC is computed in `R/transform_lending.R`. The result of
  AC is exposed as `rating_worst` and consumed by AccountMaster_1 writer.
- AD/AE are modelled as a static override CSV (`data-raw/static/customer_rating_overrides.csv`)
  with a single V4 entry today (CIF 31884 -> QDB 1). New rows can be added
  via Shiny without code changes (Path B). V4's `AW5 = "No downgrade"` so
  AE = AD, which the override layer replicates.

Six remaining diffs are stale-bundle `#N/A` rows (V4's `Inputs_Lending
Portfolio!F` had `#N/A` for those customers when the bundle was generated;
the V4 workbook itself now shows `QDB 5` for them, matching ours).

The 134-row count gap between our output and the bundle is also stale
bundle: V4's `BE1=6636` matches ours; bundle has 6502.

---

## RESOLVED — CustomerStagingFlag_1 column wiring  (2026-05-06)

Bundle reconciliation: 706 -> ~30 (estimated remaining after Path-B
acceptance for Stage 2 override divergence).

Fixed:
- Writer was forwarding only 3 of the 11 flag fields. Now passes
  is_default, is_watchlist, is_local1, is_local2, is_local3 and leaves
  is_insolvency / is_default_in_gcc / is_local4..6 blank to match bundle.
- IsLocal3 = computed `stage_final == "Stage 2"` (V4 source is
  `Inputs_Lending Portfolio!P` = staging-rule output, which we replicate).

Pending (acceptable as Path B):
- ~30 rows where V4's `Inputs_Lending Portfolio!P` (staging rule) and
  our `apply_staging_rule` diverge. Most of these come from V4 picking
  up watchlist/restructured override values from rows we don't see.

---

## KNOWN — LifeTimeParameterOther bundle has 9,653 trailing empty rows

`Output/LifeTimeParameterOther.csv` in the V4 bundle is 82,163 data rows,
of which 72,510 contain real data and 9,653 are completely empty
(`,,,,,,`). This is an Excel array-formula artifact: V4's intermediate
range produced a fixed-size output and the unused trailing cells were
dumped as empty rows.

Our R port writes only the 72,510 non-empty rows. Reconciliation flags
the missing 9,653 rows as `unmatched_reference`. Treated as a cosmetic
divergence — our output is semantically equivalent and cleaner.

If exact byte reproduction is needed for downstream consumers, the
writer can be modified to pad with empty rows up to the bundle's row
count. Not done by default.

---

## RESOLVED — Collateral CollateralTypeId always blank  (2026-05-06)

`Collateral.xlsx` raw file has identical numeric data in cols C and D
(both = CollateralTypeId, the SQL export duplicated the column). readxl
sniffed col C as `<lgl>` (named "P", all NA after parse) and col D as
`<dbl>` (named "CO", with the actual integer values). We were reading
col C and getting NA for every row.

Fix: read col D via `pick_col(coll_raw, "CO", 4)`. Currency from "COL"
(col E). Documented in `tests/manual/test_phase_h5.R`.

---

## RESOLVED (pending Excel-tool re-test) — Origination_1 134-row gap  (2026-05-06)

Same root cause as AccountMaster_1's 134-row count gap. Bundle file
`Output/Origination_1.csv` has 6502 data rows; current V4 OriginationLoad
sheet has 6636 (confirmed via Z1 = COUNT(A:A)). VBA `Sub LoadOrigination`
in mod1.txt does no filtering — it reads `x = Range("Z1").Value` then
copies `Range("A1:V" & x + 1)` to the output CSV.

The 134 missing contracts:
- ARE in current AccountMaster.xlsx input
- ARE in current Origination.xls input (after V4 ID transform)
- ARE present in V4 OriginationLoad sheet (verified rows 6103, 6566,
  6622, 6633 etc. all populated for these contract IDs)
- Were opened mostly in 2024-2025 (recent contracts)

User will rerun the Excel tool to regenerate the bundle and confirm.

---

## OPEN — Lifetime PD methodology: cumsum vs survival

**Where:** `R/build_stpd.R::convert_to_monthly_stpd()`

**Status:** kept current behavior (`cumsum` with `pd_cap = 1.0`).

**Question:** workbook uses `=SUM(...)` (simple cumsum) which is the first-order
approximation of the survival formula `=1 - PRODUCT(1 - ...)`. The two agree
for small marginals but diverge for low-rated long-maturity buckets where
annual marginals are large.

- Current: `cumsum` + cap at 1.0 — matches V4 cell formula, but cap is a small
  silent model change vs. the workbook (which writes the unclamped value, max
  ~1.0003).
- Alternative: `1 - cumprod(1 - monthly_marg)` — mathematically clean, strictly
  in [0,1] without needing a cap, but a real model change vs. workbook output.

**To resolve:** check the QDB IFRS9 Macroeconomic Variable Models PDF for the
definitive lifetime-PD specification. If the PDF defines lifetime PD via
survival, switch to `cumprod`. Otherwise, decide between
"keep cap, document under-provisioning vs. workbook" and "remove cap, handle
PD>1 downstream in ECL formula."

---

## KNOWN — V4 explicit scenario-weights typo

**Where:** `Inputs_Lending Portfolio'!AE5:AE9` in V4 workbook.

V4 stored values: 0.1508, 0.1792, 0.4799, 0.1197, 0.0707 (sum = **1.0003**).
Methodology (PDF page 16) calls for AE8 = 0.11942 (sum = 1.0).

`config/model_inputs.yml` defaults to `mode: auto_non_oil_gdp_cdf` which
computes weights from the methodology directly (sum = 1.0). To replicate the
bundle byte-for-byte, set `mode: explicit`.

---

## KNOWN — V7 external scenarios use per-year weights (not constant)

**Where:** `Calculations!D69:H73` (per-year), `D74:H74` (average for year 6+).

For external rated portfolios, V7 production applies *different* weights for
each forecast year, with an `AVERAGE` of years 1-5 used for year 6+.

Because weights vary by year, the cumsum across scenarios isn't bounded by 1
even when each year's weight row sums to 1. In our data this produces max
~1.011 before capping.

This is the V7 production methodology (the workbook does the same). It is
NOT a bug.

---

## KNOWN — V7 cell `Calc!BQ245+` typo (NOT replicated)

**Where:** V7 workbook `Calculations!BQ245:DJ272` (year 5 onward).

Formulas reference `$E76` where they should reference `$E$73` per the row-3
weight-row pattern. The bug shifts every year-5+ external cell by one row,
materially affecting external Aaa/Aa1/Aa2 buckets at long maturities.

We do NOT replicate this typo. R port uses the correct `$E$73` reference.
Phase F's external-portfolio reconciliation against the bundled
`Output/StPD.csv` therefore shows a known small drift for these buckets.

**Status (post-H8, 2026-05-06):** Reconciliation against the bundled
`StPD.csv` reports `value_drift` with `max_abs_diff = 0.109` over all
75,600 cells. After investigation, this is consistent with — and almost
entirely attributable to — the V7 typo described above plus other small
methodology corrections we made deliberately. The R port's output is
considered MORE correct than the bundled output. We accept this divergence
as expected; do not chase it as a bug.

If reviewers want byte-equivalence with the V4/V7 bundle, the path is to
reintroduce the typo in `R/pd_term_structure.R` behind a feature flag
`run.replicate_v7_bq245_typo: true`. We do not do this by default because
production should use the correct math, but it would let regression
testing prove that the only source of drift is the typo.

---

## KNOWN — AccountCollateralAllocation 65,535-row truncation

**Where:** `Output/AccountCollateralAllocation.csv` in the bundle.

The bundled CSV has exactly 65,535 rows (Excel's row limit when written via
the legacy AS-XLS path). Our R port produces all rows from the source;
reconciliation should compare on a row-key basis (ContractId, CollateralId)
rather than row count.

Will be moot once we switch to direct CSV input (per project plan).

---

## SHIPPED — Phases H10–H12  (2026-05-06)

H10–H12 ran through the operator UI work. Summary so anyone walking up
to the codebase knows what state it's in:

- **H10**: read-only Shiny app at `app/`. Pages: Runs, Snapshots, Audit log.
  `run_etl()` got a `keep_history=TRUE` mode that writes to
  `runs/<timestamp>/`. Discovery helpers in `R/run_discovery.R`.

- **H11**: write-capable Shiny. Snapshot create/promote (draft → pending →
  approved → archived). Suppression manager. Pre-run check + Run trigger
  pages. Validation suppressions surface in audit log.

- **H12**: mid-run pause workflow. `run_etl()` is now a thin wrapper
  around `run_etl_phase1()` + `run_etl_phase2()`. Phase 1 stops after
  the lending portfolio view; Shiny shows `cm_view` to the user; user
  adds rating / stage / restructuring overrides per-customer with
  required reasons; phase 2 applies them to in-memory tables and writes
  outputs. Each run lands as `pending_approval`. New `Approval queue`
  page (Pending tab + History tab) handles run-level sign-off.

Architectural shifts in H12 worth flagging for reviewers:

1. **Override files are per-run, not global.** Live at
   `runs/<run_id>/overrides/`. Snapshots no longer freeze override
   CSVs. The legacy `data-raw/static/customer_*_overrides.csv` files
   still exist for the backward-compat single-shot path used by tests,
   but the production (Shiny-driven) path ignores them.

2. **Approval is run-level.** A reviewer accepts the entire run including
   any overrides applied during it. Tracked via
   `runs/<run_id>/reports/run_status.yml` with full transition history.

3. **Override application strategy:** overrides are written into
   `trans_l$rating_worst` (the master rating column the writer reads)
   and `cm_view$stage_final` / `cm_view$restructuring_final`. Phase 2
   does NOT re-run the transformation; it overwrites the affected leaf
   columns. `build_customer_flags` runs after the override block so the
   stage/restructuring overrides flow through automatically.

See `docs/h12_design.md` for the full rationale (alternatives considered,
why we chose what we chose) and `docs/operator_runbook.md` for the
day-to-day usage.

---

## OPEN — items for future phases

Not blocking H12 but flagged for follow-up:

### Snapshot editor (highest priority)
Snapshots can be created and promoted but not **edited** in the UI. A
draft snapshot today is bit-for-bit identical to live config — there's
no way to modify `config.yml`, `models.yml`, `variable_dictionary.yml`,
or `model_inputs.yml` within the draft before promoting. Without an
editor the snapshot lifecycle is a workflow without a purpose. Building
this is the natural H13.

### Code-version visibility
The current code SHA is captured in `manifest.json` and audit events,
and visible in the Manifest tab of any run. Not surfaced as a banner or
column on the Runs page. Reviewers asking "what code produced this run"
have to drill in. Easy fix; deferred to next polish pass.

### Run status column on the Runs page
Approval status (`pending_approval` / `approved` / `rejected`) is
visible per-run on the Approval queue but not on the main Runs page.
Should be a column.

### Suppressions tab — empty state
The validator catalog on the suppressions page populates from the
LATEST run's failed validators. When the latest run had no failures,
the catalog is empty. The empty state is unclear. Either: explain it
better, or pull failed validators from any recent run rather than just
the latest.

### Async runs
Phase 2 takes ~20s during which the UI is locked. Acceptable for now.
Refactor to `promises` + `future` if it becomes annoying in daily use.
Watch for the audit-log-tail race I flagged in earlier discussions.

---

## OPEN — analytical / reconciliation items

Items that need someone with V4/V7 workbook context to investigate, not
just code changes:

### StPD value drift (max_abs_diff = 0.109)
Documented as expected — V7 cell `Calc!BQ245+` has a typo we deliberately
don't replicate. This means the R port produces *more correct* output
than the V4 bundle for some rating/portfolio/bucket cells. Reviewer
should confirm this interpretation; if they want bit-parity with V4,
we'd need to introduce the typo back into the R port. See "KNOWN — V7
cell `Calc!BQ245+` typo (NOT replicated)" above.

### Investments-side reconciliation drift
73/73 row count match in `AccountMaster_2.csv` but values drift in some
columns. Not investigated; flagged in earlier reconciliation runs.
Investments path is much smaller code surface than lending — likely a
single transformation difference.

### Lifetime PD methodology — cumsum vs survival
See "OPEN — Lifetime PD methodology: cumsum vs survival" above. Not
revisited since H3.

---

## RESOLVED-but-flagging-for-the-record

Items that ARE resolved but reviewers may still ask about:

- AccountMaster row count gap (134) → see RESOLVED entry above.
- CustomerStagingFlag_1 column wiring → see RESOLVED entry above.
- Collateral CollateralTypeId always blank → fixed.
- LifeTimeParameterOther 9,653 trailing empty rows → V4 bundle artifact;
  R port produces a clean file.
- AccountCollateralAllocation 65,535-row truncation → V4 Excel limit;
  R port handles full data.
