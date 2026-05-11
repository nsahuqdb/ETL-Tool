# Lending Portfolio — Transformation logic mapping

This document maps every column of the Excel `Transformation` sheet to the R
implementation in `R/transform_lending.R`. It is the single source of truth
for Phase D. If a column behaves differently in R than in Excel, this doc
is wrong — fix the doc and the code together.

## Source

- `Transformation` sheet rows 2..N (one row per contract, lending side: cols A..BZ)
- `Transformation` sheet cols CA..CJ (investment block, one row per account)
- `Inputs_Lending Portfolio` sheet rows 5..N (one row per customer)
- `Inputs_Investment Portfolio` sheet rows 5..N (one row per investment account)
- `Updated_ETL_File__Test__V7_-_2025-NS.xlsm`

## Inputs to Transformation (one row per contract = one row per AccountMasterExtract row)

| Excel col   | Excel name                      | Source                                         | R column         | Logic                                                                                                                                      |
| ----------- | ------------------------------- | ---------------------------------------------- | ---------------- | ------------------------------------------------------------------------------------------------------------------------------------------ |
| A           | DateExt                         | AccountMasterExtract!A                         | extract_date_raw | passthrough                                                                                                                                |
| B           | DateTrans                       | =A*1                                           | extract_date     | parse to Date                                                                                                                              |
| E           | ContractIDExtract               | AccountMasterExtract!B                         | contract_id_raw  | passthrough                                                                                                                                |
| F           | ContractIDTransform             | numeric, else suffix-replaced via product code | contract_id      | If numeric → use as-is. Else: `LEFT(7) + VLOOKUP(MID(8,3), Assumptions!B3:C15, 2) + RIGHT(3)` — converts off-balance product codes to numeric IDs. |
| G           | ContractIDUnique                | =COUNTIF(F:F, F)                               | (used only for validation) | duplicate check                                                                                                                            |
| J           | CustomerIDTransform             | AccountMasterExtract!D                         | customer_id      | passthrough                                                                                                                                |
| Q           | AccountType                     | AccountMasterExtract!F                         | account_type     | passthrough (e.g. "Al Dhameen", "Direct Lending")                                                                                          |
| S           | QDBIndustryExt                  | VLOOKUP(customer_id, IndustryCodeExtract, 3)   | qdb_industry_desc | from IndustryCode file, may be empty if not found                                                                                          |
| T           | QDBIndustryTrans                | VLOOKUP(LEFT(S,4), Assumptions!F2:G201, 2)     | sector           | first 4 chars of industry desc → sector. **Falls back to "Others" if not found.**                                                          |
| V           | OpenDateTrans                   | AccountMasterExtract!I * 1                     | open_date        | parse to Date                                                                                                                              |
| Y           | RatingExt                       | VLOOKUP(customer_id, CustomerMasterExtract!B:F, 5) intersected with MasterRatingScale!F4:F45 (external ratings) | external_rating | only set if customer's rating from CustomerMaster is in the EXTERNAL rating list |
| Z           | RatingTrans                     | complex: see notes below                       | rating           | Multi-step. See "Rating derivation" below.                                                                                                 |
| AA          | RatingHierarchy                 | VLOOKUP(Z, MasterRatingScale!F:G, 2)           | rating_hierarchy | numeric hierarchy of derived rating                                                                                                        |
| AB          | RatingHierarchyMax              | MAX of AA over rows where customer_id matches  | rating_hierarchy_worst | per customer: take max hierarchy across all contracts (worst rating wins)                                                                  |
| AC          | RatingUnique                    | INDEX(MasterRatingScale!B4:B24, AB)            | rating_worst     | rating string at the worst hierarchy index                                                                                                 |
| AD          | RatingTransOverride             | VLOOKUP(customer_id, Inputs_Lending Portfolio!A:F, 6) | rating_after_override | reads back the Final Rating from Inputs_Lending Portfolio (which itself = Override OR Current). User-driven manual override.               |
| AE          | Rating_OneNotchDowngrade        | If sheet's `Inputs_Lending Portfolio!AW5 = "No Downgrade"`: AD; else VLOOKUP(AD, MasterRatingScale!J:K, 2) | rating_with_downgrade | switch driven by AW5 in Inputs_Lending Portfolio (see `apply_one_notch_downgrade` flag) |
| AF          | PastDuesExt                     | AccountMasterExtract!K (default 0 if blank)    | past_dues_days   | DPD per contract                                                                                                                           |
| AG          | PastDuesTrans                   | MAX of AF over rows where customer_id matches  | past_dues_worst  | per customer: max DPD across all contracts                                                                                                 |
| AJ          | OnBalanceSheetExt               | AccountMasterExtract!R                         | exposure_amount  | impairment / exposure amount                                                                                                               |
| AM          | MaturityDateExt + extension     | If AccountMasterExtract!U < ext_date: ext_date + maturity_extension_days; else U | maturity_date    | implements the `If Maturity Date <= Reporting Date → +365` rule                                                                            |
| AP/AQ       | EIR Extract / EIRTransform      | If AccountMasterExtract!W = 0: VLOOKUP(account_type, Inputs_Lending Portfolio!AL:AN, 3) else W/100 | eir              | if EIR missing in source, use the per-AccountType average from Inputs_Lending Portfolio                                                    |
| AT          | PaymentFrequencyTrans           | AccountMasterExtract!AJ (blank → "")           | payment_frequency_months | months between payments                                                                                                                    |
| AW          | CurrencyVal_Format              | LEN(AccountMasterExtract!AK)                   | currency_len     | currency code length (validation only)                                                                                                     |
| AZ          | DeferralPeriodExt               | AccountMasterExtract!AL                        | deferral_period  | passthrough                                                                                                                                |
| BC          | NominalInterestRateTrans        | = AQ (= EIR)                                   | nominal_int_rate | currently same as EIR                                                                                                                      |
| BF          | PaymentTypeTrans                | If AT="" → 3 (bullet) else 4 (regular)         | payment_type     | derived flag                                                                                                                               |
| BI          | CustomerIDExt (StagingFlag)     | CustomerStagingFlagExtract!B                   | customer_id_sf   | (used to look up flags only — not propagated)                                                                                              |
| BL          | IsDefaultTrans                  | =IF(VLOOKUP(BI, J:AG, 24, FALSE) > 90, 1, 0)   | is_default       | DPD > 90 ⇒ default. Note: 24th col offset from J is AG = past_dues_worst.                                                                  |
| BM          | IsDefaultOverride               | =IF(VLOOKUP(BI, Inputs_Lending Portfolio!A:P, 16, FALSE) = "Stage 3", 1, 0) | is_default_final | reads back from staging output (post-override) — Stage 3 ⇒ default                                                                         |
| BP          | IsWatchlistExt                  | CustomerStagingFlagExtract!D                   | is_watchlist     | 0/1 flag                                                                                                                                   |
| BS          | IsRestructuringExt              | CustomerStagingFlagExtract!G                   | is_restructured  | 0/1 flag                                                                                                                                   |
| BT          | IsRestructuringOverride         | =IF(VLOOKUP(BI, Inputs_Lending Portfolio!A:J, 10, FALSE) = "Restructured", 1, 0) | is_restructured_final | reads back the Final Restructuring Tag from Inputs_Lending Portfolio (commercial-reasons override may have cleared it)                     |
| BW          | IsLocal3                        | =IF(VLOOKUP(BI, Inputs_Lending Portfolio!A:P, 16, FALSE) = "Stage 2", 1, 0) | is_stage2        | 1 if final stage = Stage 2                                                                                                                 |
| BY          | IsTasdeer                       | (computed — likely Tasdeer flag from staging)  | is_tasdeer       | TBD on detailed inspection                                                                                                                 |

## Rating derivation (Transformation!Z) — the critical one

```
IF F = "" -> ""                                 (no contract id ⇒ blank row)
ELSE try the chain (IFERROR returns next on failure):
  1. IF the customer has an external rating (Y) found in the external rating list:
        use Y
     ELSE fall through to sectoral lookup:
  2. IF sector = "Agriculture":
        VLOOKUP(past_dues_worst, Assumptions!I6:J9, 2, TRUE)
     ELSE IF sector = "Fisheries":
        VLOOKUP(past_dues_worst, Assumptions!I12:J15, 2, TRUE)
     ELSE IF sector = "Lifestock":
        VLOOKUP(past_dues_worst, Assumptions!I18:J21, 2, TRUE)
     ELSE:
        the customer's rating from CustomerMasterExtract!F (the internal rating column)
```

In R: `derive_rating()` in `R/transform_lending.R`.

## Inputs_Lending Portfolio (one row per CUSTOMER, not per contract)

| Excel col | Excel name                    | R column                  | Logic                                                                                              |
| --------- | ----------------------------- | ------------------------- | -------------------------------------------------------------------------------------------------- |
| A         | CIC - Lending Portfolio       | customer_id               | from CustomerMasterExtract                                                                         |
| B         | Name of Customer              | customer_name             | from CustomerMasterExtract                                                                         |
| C         | Exposure                      | exposure_total            | = SUMIF(Transformation!customer_id = A, exposure_amount)                                           |
| D         | Current Rating                | rating_current            | = VLOOKUP(customer_id, Transformation!J:AC, 20) ⇒ Transformation.rating_worst (col AC)             |
| E         | Rating Override               | rating_override           | **manual user input** (blank in default run)                                                       |
| F         | Final Rating                  | rating_final              | = IF(E="", D, E)                                                                                   |
| G         | Rating Override Status        | rating_override_status    | descriptive: "No Rating Override" or "Rating Overridden from X to Y"                               |
| H         | Current Restructuring Tag     | restructuring_current     | = IF(VLOOKUP(customer_id, Transformation!BI:BS, 11) = 1, "Restructured", "")  → IsRestructuringExt |
| I         | Restructuring due to FD Override | restructuring_override | **manual user input** (typically blank or "Restructuring due to commercial reasons")               |
| J         | Final Restructuring Tag       | restructuring_final       | = IF(H="Restructured" AND I≠"commercial reasons", "Restructured", "")                              |
| K         | Restructuring Override Status | restructuring_override_status | descriptive                                                                                        |
| L         | Watchlist Status              | watchlist_status          | = IF(VLOOKUP(customer_id, Transformation!BI:BP, 8) = 1, "Watchlist", "")                           |
| M         | DPD Status                    | dpd_status                | = VLOOKUP(customer_id, Transformation!J:AG, 24) ⇒ past_dues_worst                                  |
| N         | Staging as per rules          | stage_pre_override        | DPD>90⇒Stage 3, else (Restructured OR DPD in (60,90] OR Watchlist) ⇒ Stage 2, else Stage 1         |
| O         | Backward Transition Override  | stage_override            | **manual user input** (only allowed to downgrade)                                                  |
| P         | Staging post override         | stage_final               | = IF(O="", N, IF(N="Stage 3" OR O="Stage 3", "Stage 3", O))                                        |
| Q         | Staging Override Status       | stage_override_status     | descriptive                                                                                        |
| AW5       | Rating Downgrade switch       | apply_one_notch_downgrade | global switch: "No downgrade" | "Apply downgrade" — drives Transformation!AE                                |

## Manual override columns (default = blank)

In Phase D we expose these as function arguments, defaulting to NULL/blank so the
default behaviour matches the Excel tool's blank-override-row default state:

- `rating_overrides`: optional tibble (customer_id, rating_override)
- `restructuring_overrides`: optional tibble (customer_id, restructuring_override)
- `stage_overrides`: optional tibble (customer_id, stage_override)
- `apply_one_notch_downgrade`: logical scalar, default FALSE

Later (Shiny phase) these become user-editable tables in the UI.

## Circular dependencies — how Excel handles them, how R does not

The Excel formulas have several cycles (Transformation reads back from Inputs_Lending
Portfolio, which reads from Transformation). Excel resolves these with iterative
calculation (the workbook has `Iterations = 1000` enabled).

In R we resolve them by computing in the correct order with explicit passes:

1. **Pass 1**: Build base Transformation (cols A,E,F,J,Q,S,T,V,Y,AF,AG,AJ,AM,AP-AZ,BC,BF,BI,BP,BS).
2. **Pass 2**: Per-customer aggregations (cols AB, AG worst-of, AC).
3. **Pass 3**: Initial rating Z, hierarchy AA, hierarchy max AB, rating_worst AC.
4. **Pass 4**: Build Inputs_Lending Portfolio cols A-N (without overrides applied).
5. **Pass 5**: Apply user overrides (E, I, O) → cols F, J, P, Q, G, K.
6. **Pass 6**: Back-fill Transformation cols AD (rating_after_override), AE (with downgrade), BL/BM/BT/BW (final flags).

Cycle is broken because the override columns at pass 5 only depend on inputs from pass 4.

## Investment block (Transformation columns CA..CJ + Inputs_Investment Portfolio)

The investment book is structurally simpler than lending — per-account, no
DPD-based staging, no restructuring/watchlist concept. Staging is purely
SICR-based (Significant Increase in Credit Risk via rating migration).

| Excel col   | Excel name                  | Source                                          | R column                       | Logic |
| ----------- | --------------------------- | ----------------------------------------------- | ------------------------------ | ----- |
| CA          | AccountIDInvtExt            | AccountMasterInvExtract!B                       | account_id                     | passthrough |
| CC          | CustomerIDInvExt            | AccountMasterInvExtract!D                       | customer_id_inv                | passthrough |
| CD          | RatingInvCurrentExt         | AccountMasterInvExtract!J intersected with MasterRatingScale!J16:J45 (external subset) | rating_external_raw            | only set if rating is in External scale |
| CE          | RatingInvTrans              | IF CD = "" → Inputs_Investment Portfolio!N5 (Baa3 fallback), else CD | rating_current                 | apply segment fallback for unrated investments |
| CF          | RatingInvTransOverride      | VLOOKUP(account_id & "_" & customer_id, Inputs_Investment Portfolio!A:E, 5) | rating_after_override          | reads back Final Rating (override OR Current) |
| CG          | RatingInvHierarchy          | VLOOKUP(CF, MasterRatingScale!C3:D24, 2)        | rating_hierarchy               | hierarchy of post-override rating, via external_equivalent column |
| CH          | RatingInvOriginationExt     | VLOOKUP(account_id, OriginationInvExtract!B:N, 3) → col D | rating_at_origination          | rating at origination, blank if 0 |
| CI          | RatingInvOriginationHier    | IF CH = "" → CG, ELSE VLOOKUP(CH, MasterRatingScale!C3:D24, 2) | rating_origination_hierarchy   | hierarchy at origination (or current if missing) |
| CJ          | RatingInv_OneNotchDowngrade | If sheet's S5 = "No Downgrade": CF, else VLOOKUP(CF, MasterRatingScale!J:K, 2) | rating_with_downgrade          | global switch driven by S5 in Inputs_Investment Portfolio |

### Investment staging rule (Inputs_Investment Portfolio!H5)

```
IF account_id = "" → ""
IF rating_hierarchy <= 4 → "Stage 1"          (top 4 ratings always Stage 1)
IF rating_hierarchy < 11 →
   IF (rating_hierarchy - origination_hierarchy) >= 2 → "Stage 2"
   ELSE "Stage 1"
ELSE
   IF (rating_hierarchy - origination_hierarchy) >= 1 → "Stage 2"
   ELSE "Stage 1"
```

There is **NO Stage 3** path in the investment staging formula. Defaulted
bonds would need to be moved to Stage 3 via the manual `staging_override`
mechanism. Different from lending where DPD>90 forces Stage 3.

### Inputs_Investment Portfolio columns

| Excel col | R column                | Logic |
| --------- | ----------------------- | ----- |
| A         | account_id              | from AccountMasterInvExtract |
| B         | exposure                | from AccountMasterInvExtract!R (col 18) |
| C         | rating_current          | = Transformation!CE (with fallback applied) |
| D         | rating_override         | manual user input |
| E         | rating_final            | IF D="" → C, ELSE D |
| F         | rating_override_status  | descriptive |
| G         | rating_at_origination   | = Transformation!CH |
| H         | stage_pre_override      | SICR rule (above) |
| I         | stage_override          | manual user input |
| J         | stage_final             | IF I="" → H, ELSE I |
| K         | stage_override_status   | descriptive |
| S5        | apply_one_notch_downgrade | global switch — same as lending AW5 but per investment book |

### Segment fallback ratings

All in `data-raw/static/segment_fallback_ratings.csv`:

| Segment                              | Fallback rating | Source cell                            |
| ------------------------------------ | --------------- | -------------------------------------- |
| Unrated Customer (Internal Rating)   | QDB 5           | Inputs_Lending Portfolio!T5           |
| Al Dhameen Customers                 | QDB 6           | Inputs_Lending Portfolio!T6           |
| Investment Portfolio                 | Baa3            | Inputs_Investment Portfolio!N5        |

### Important note: `rating_hierarchy` lookup uses different columns for lending vs investment

Lending side uses `MasterRatingScale!F4:G45` (the EXTERNAL block's rating
column F → hierarchy column G). Investment side uses `MasterRatingScale!C3:D24`
(the INTERNAL block's external_equivalent column C → hierarchy column D),
because investment ratings are Moody's-style (Aaa, Aa1, ..., Baa3, ...) which
are stored in the external_equivalent column of the internal block.

This is reflected in our R code:
- `transform_lending.R` builds rating_hierarchy via `master_rating_scale$hierarchy[match(rating, master_rating_scale$rating)]` (with rating_type filter implicit).
- `transform_investments.R` builds rating_hierarchy via `master_rating_scale$hierarchy[match(rating, master_rating_scale$external_equivalent)]` against the Internal block.
