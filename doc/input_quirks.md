# Input file quirks observed during Phase B validation

These are characteristics of the source IFRSIN files that the R port needs
to be aware of. Discovered while validating the readers against the bundled
sample inputs (extract date 2025-12-31).

## 1. Two date formats coexist

| File source                        | EXTRACTDA format     |
| ---------------------------------- | -------------------- |
| `.xlsx` files (proper Excel)       | `2025-12-31` (Date)  |
| `.xls` files (SQL*Plus HTML)       | `31-DEC-25` (string) |

The transformation layer in **Phase D** must normalise both to a single
`Date` type. We will use `lubridate::dmy_*` for the SQL*Plus format and
`as.Date` for the xlsx format with auto-detection.

## 2. Cryptic column names in SQL*Plus exports

Some columns come back with single-letter or short names because the upstream
SQL aliases were short. Examples:

| File                                | Cryptic columns observed                     |
| ----------------------------------- | -------------------------------------------- |
| `CustomerMasterInvestments.xls`     | `P`, `C`, `C.1`, `R`, `P.1`, `P.2`, `P.3`     |
| `CustomerStagingFlagInvestments.xls`| `I` through `I.9`                            |
| `Origination.xls`                   | `O`, `O.1`, `I`, `I.1` … `I.7`                |
| `OriginationInvestments.xls`        | same as Origination                          |

Phase B (current) preserves these names as-is — matching the Excel tool's
behaviour, which references columns by position when copying into the
`*Extract` sheets. **Phase D** will apply a position-based column map per
file to produce clean, semantic names.

This is not a bug we need to fix upstream; it is an artefact of the SQL*Plus
`COLUMN <name> HEADING '<short>'` pattern. If/when the source pipeline
moves to CSV (per your roadmap) we will get the proper SQL aliases back.

## 3. AccountCollateralAllocation has a stray duplicate-header row

Row 50,000 (1-indexed) contains the literal string `"EXTRACTDA"` in the
EXTRACTDA column instead of a date. This is a data-quality issue in the
source export, not in our reader.

The validator flags it as `WARNING` (multiple distinct EXTRACTDA values).
We pass the row through unchanged here; **Phase D** will drop any row where
EXTRACTDA does not parse as a valid date.

## 4. RepaymentSchedule and AccountCollateralAllocation max-out at 65,535 rows

Both files are exported at 65,535 rows — one less than the legacy
Excel-97 row limit (65,536) — even though there is more data downstream.
This is a known limitation of the SQL*Plus → .xls export pipeline.

If real production data ever exceeds 65,535 rows, those exports will be
silently truncated. The R port cannot detect or fix that; it must be
addressed at the source-export layer (the team's plan to move to CSV
mid-term will fix it).

## 5. Encoding

The SQL*Plus HTML exports declare `charset=WINDOWS-1256` (Arabic). We pass
this explicitly to `xml2::read_html(..., encoding = "WINDOWS-1256")` in
`R/io_helpers.R::read_html_table`. Customer names in the lending portfolio
may contain Arabic characters; the R tibble will hold them as native UTF-8
once read.
