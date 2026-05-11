"""
Extract static reference data from the Excel ETL tool into the project's
data-raw/static and config folders. Run once whenever the underlying
Excel reference data changes.

Usage:
    python data-raw/extract_from_xlsm.py path/to/Updated_ETL_File__Test__V7_-_2025-NS.xlsm

What it produces:
    data-raw/static/off_balance_products.csv
    data-raw/static/industry_sector_mapping.csv
    data-raw/static/collective_assessment_rules.csv
    data-raw/static/product_portfolio_mapping.csv
    data-raw/static/master_rating_scale.csv
    data-raw/static/master_rating_downgrade.csv
    data-raw/static/scenario_severity.csv
    data-raw/static/gcc_real_gdp_growth.csv
    data-raw/static/gcc_gdp_current_prices.csv
    data-raw/static/staging_thresholds.csv
    config/model_config.yml

Dependencies: openpyxl (reads cell values), pyyaml (writes config).

Why Python and not R? This is a one-time extraction that reads inside the
.xlsm including hand-curated layout (multi-table sheets, named ranges).
The output CSVs/YAML are what the R code consumes — no R dependency on
the original .xlsm.
"""
import sys
import os
import re
import json
import csv
import zipfile
import xml.etree.ElementTree as ET
from pathlib import Path

NS = '{http://schemas.openxmlformats.org/spreadsheetml/2006/main}'

# ---------------------------------------------------------------------------
# Low-level XML helpers
# ---------------------------------------------------------------------------
def col_letter_to_num(letters):
    n = 0
    for c in letters:
        n = n * 26 + (ord(c.upper()) - ord('A') + 1)
    return n

def parse_ref(ref):
    m = re.match(r'([A-Z]+)(\d+)', ref)
    return col_letter_to_num(m.group(1)), int(m.group(2))

def load_xlsm(xlsm_path):
    """Unzip xlsm to a temp dir and return (sheet_to_xml_path, shared_strings, tmp_dir)."""
    import tempfile
    tmp_dir = Path(tempfile.mkdtemp(prefix="ifrs9_extract_"))
    with zipfile.ZipFile(xlsm_path) as z:
        z.extractall(tmp_dir)

    # shared strings
    shared = []
    ss_path = tmp_dir / "xl" / "sharedStrings.xml"
    if ss_path.exists():
        ss_root = ET.parse(ss_path).getroot()
        for si in ss_root.findall(NS + 'si'):
            ts = si.findall('.//' + NS + 't')
            shared.append(''.join((t.text or '') for t in ts))

    # sheet name -> xml file
    with open(tmp_dir / "xl" / "_rels" / "workbook.xml.rels") as f:
        rels_xml = f.read()
    rels = dict(re.findall(
        r'Id="(rId\d+)"\s+Type="[^"]+/worksheet"\s+Target="([^"]+)"', rels_xml))
    with open(tmp_dir / "xl" / "workbook.xml") as f:
        wb_xml = f.read()
    sheets = re.findall(
        r'<sheet\s+name="([^"]+)"\s+sheetId="\d+"\s+r:id="(rId\d+)"', wb_xml)
    sheet_to_path = {
        name: tmp_dir / "xl" / rels[rid] for name, rid in sheets if rid in rels
    }
    return sheet_to_path, shared, tmp_dir

def dump_sheet(xml_path, shared):
    """Stream-parse a worksheet, returning {(col, row): value} dict."""
    cells = {}
    for event, elem in ET.iterparse(xml_path, events=('end',)):
        if elem.tag == NS + 'row':
            for c in elem.findall(NS + 'c'):
                ref = c.get('r')
                col, row = parse_ref(ref)
                t = c.get('t', 'n')
                v = c.find(NS + 'v')
                val = None
                if t == 's' and v is not None:
                    val = shared[int(v.text)]
                elif v is not None:
                    val = v.text
                cells[(col, row)] = val
            elem.clear()
    return cells

def write_csv(path, header, rows):
    """Write a CSV. rows is iterable of dicts or tuples matching header."""
    path.parent.mkdir(parents=True, exist_ok=True)
    with open(path, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow(header)
        for r in rows:
            if isinstance(r, dict):
                w.writerow([r.get(h, "") for h in header])
            else:
                w.writerow(r)
    print(f"  wrote {path} ({len(rows)} rows)")

def num(v):
    """Return float if possible, else None."""
    if v is None or v == "":
        return None
    try:
        return float(v)
    except (TypeError, ValueError):
        return None

# ---------------------------------------------------------------------------
# Per-sheet extractors
# ---------------------------------------------------------------------------
def extract_off_balance_products(cells):
    """Assumptions cols B:C, rows 3-9."""
    rows = []
    for r in range(3, 10):
        code = cells.get((2, r))   # col B
        lic  = cells.get((3, r))   # col C
        if code:
            rows.append({"product_code": code, "lic_input_code": int(float(lic))})
    return rows

def extract_industry_sector_mapping(cells):
    """Assumptions cols E:G, rows 3-end. Skip the 'Others' fallback row that has
       only G filled; it's a default sector when code doesn't match."""
    rows = []
    others_default = None
    r = 3
    while True:
        desc = cells.get((5, r))   # col E
        code = cells.get((6, r))   # col F
        sect = cells.get((7, r))   # col G
        if desc is None and code is None and sect is None:
            r += 1
            if r > 200:
                break
            continue
        if desc is None and code is None and sect is not None:
            others_default = sect
        elif code is not None:
            rows.append({
                "industry_code": str(code).strip(),
                "industry_description": (desc or "").strip(),
                "sector": (sect or "").strip(),
            })
        r += 1
        if r > 200:
            break
    return rows, others_default

def extract_collective_assessment(cells):
    """Assumptions cols I:J. Layout repeats per sector:
         Row N:    sector name in I
         Row N+1:  'DPD' / 'Rating' headers
         Rows N+2..N+5:  DPD threshold / Rating
       Returns long form: sector, dpd_threshold, rating
    """
    rows = []
    current_sector = None
    in_table = False
    r = 1
    while r < 60:
        i_val = cells.get((9, r))   # col I
        j_val = cells.get((10, r))  # col J
        if i_val and "Sector" in str(i_val):
            current_sector = str(i_val).strip()
            in_table = False
        elif i_val == "DPD" and j_val == "Rating":
            in_table = True
        elif in_table and current_sector and i_val is not None and j_val:
            try:
                dpd = int(float(i_val))
            except (TypeError, ValueError):
                in_table = False
                r += 1
                continue
            rows.append({
                "sector": current_sector,
                "dpd_threshold": dpd,
                "rating": str(j_val).strip(),
            })
        r += 1
    return rows

def extract_product_portfolio_mapping(cells):
    """Assumptions cols Q:R, rows 3-13."""
    rows = []
    for r in range(3, 20):
        prod = cells.get((17, r))     # col Q
        port = cells.get((18, r))     # col R
        if prod:
            rows.append({"product_type": str(prod).strip(),
                         "portfolio":    str(port or "").strip()})
    return rows

def extract_staging_thresholds(cells):
    """Assumptions: DPD threshold (L3), Maturity rule (N3) + days (O3)."""
    rows = [
        {"key": "dpd_stage2_threshold_days",
         "value": str(int(float(cells.get((12, 3))))),
         "description": "DPD upper bound for Stage 2 (Stage 1 if DPD <=, Stage 2 if > and <= 90, Stage 3 if > 90)"},
        {"key": "maturity_extension_rule",
         "value": str(cells.get((14, 3)) or ""),
         "description": "Trigger condition for extending Maturity Date"},
        {"key": "maturity_extension_days",
         "value": str(int(float(cells.get((15, 3))))),
         "description": "Number of days to extend the Maturity Date when the rule fires"},
    ]
    return rows

def extract_master_rating_scale(cells):
    """MasterRatingScale: combined long form internal/external + hierarchy.
       cols B,C,D have internal mapping (rows 4-24)
       cols F,G have combined ratings list (rows 4-45) — 21 internal + 21 external
       Output: rating, rating_type, external_equiv, hierarchy
    """
    rows = []
    # Internal block (rows 4-24)
    for r in range(4, 25):
        internal = cells.get((2, r))
        external = cells.get((3, r))
        hier     = cells.get((4, r))
        if internal:
            rows.append({
                "rating": str(internal).strip(),
                "rating_type": "Internal",
                "external_equivalent": str(external or "").strip(),
                "hierarchy": int(float(hier)) if hier else None,
            })
    # External block (rows 25-45 in cols F,G)
    for r in range(25, 46):
        rating = cells.get((6, r))
        hier   = cells.get((7, r))
        if rating:
            rows.append({
                "rating": str(rating).strip(),
                "rating_type": "External",
                "external_equivalent": str(rating).strip(),
                "hierarchy": int(float(hier)) if hier else None,
            })
    return rows

def extract_rating_downgrade(cells):
    """MasterRatingScale cols J:K rows 4-45 — 1-notch downgrade map."""
    rows = []
    for r in range(4, 46):
        rating = cells.get((10, r))   # col J
        downgr = cells.get((11, r))   # col K
        if rating:
            rows.append({
                "rating": str(rating).strip(),
                "rating_after_1_notch_downgrade": str(downgr or "").strip(),
            })
    return rows

def extract_segment_fallback_ratings(lending_cells, investment_cells):
    """Inputs_Lending Portfolio S5:T6 + Inputs_Investment Portfolio M5:N5
    hold the segment-level fallback ratings:
       Lending T5 = Unrated Customer (Internal Rating)  — non-Al-Dhameen
       Lending T6 = Al Dhameen Customers
       Investment N5 = Investment Portfolio (the only investment segment)

    Used when the rating chain falls through (no external rating, no sector
    collective rating, no internal rating).
    """
    return {
        "Unrated Customer (Internal Rating)": lending_cells.get((20, 5)),
        "Al Dhameen Customers":               lending_cells.get((20, 6)),
        "Investment Portfolio":               investment_cells.get((14, 5)),
    }

def extract_scenario_severity(lending_cells):
    """Inputs_Lending Portfolio cols AA:AE rows 5-9 — 5 scenarios with z + weight."""
    rows = []
    for r in range(5, 10):
        scen = lending_cells.get((27, r))   # AA
        guide = lending_cells.get((28, r))  # AB
        sev_label = lending_cells.get((29, r))  # AC
        z = lending_cells.get((30, r))      # AD
        w = lending_cells.get((31, r))      # AE
        if scen:
            rows.append({
                "scenario": str(scen).strip(),
                "assessment_guidance": str(guide or "").strip(),
                "severity_label": str(sev_label or "").strip(),
                "severity_z": float(z) if z is not None else None,
                "scenario_probability_weight": float(w) if w is not None else None,
            })
    return rows

def extract_gcc_gdp(cells, block_label_row, data_start_row, data_end_row):
    """Generic helper for the historical GDP blocks.
       block_label_row: row containing the title (e.g. 1 for Real GDP Growth)
       data_start_row, data_end_row: country rows
       Returns long form: country, year, value
    """
    # Year header is on the row immediately under the block label
    year_row = block_label_row + 1
    years = {}
    for c in range(2, 100):
        v = cells.get((c, year_row))
        if v is None or v == "":
            continue
        try:
            years[c] = int(float(v))
        except (TypeError, ValueError):
            pass
    rows = []
    for r in range(data_start_row, data_end_row + 1):
        country = cells.get((1, r))
        if not country:
            continue
        for c, yr in years.items():
            v = cells.get((c, r))
            if v is None or v == "" or v == "N/A":
                continue
            try:
                fv = float(v)
            except (TypeError, ValueError):
                continue
            rows.append({"country": str(country).strip(),
                         "year": yr, "value": fv})
    return rows

def extract_gcc_distribution(cells):
    """B30 and B31 are the mean and SD of the composite GCC Real GDP Growth.
       Used by the macro model for externally-rated weights.
    """
    return {"mean": float(cells.get((2, 30))),
            "standard_deviation": float(cells.get((2, 31)))}

def extract_model_specifications(cells):
    """Model Specifications sheet, rows 5-7. Despite headers starting at row 4,
       the data block actually begins at column C (col 3), not column A.
       Layout: C=name, D=Intercept, E=Coefficient, F=P-Value, G=Weight, H=SD.
       Returns list of MEV configs.
    """
    mevs = []
    # Real Estate and Credit have a *100 unit conversion in the Calculations
    # formulas (E9, F9 use *100; D9 doesn't). We capture that explicitly.
    unit_multiplier = {0: 1, 1: 100, 2: 100}
    for i, r in enumerate(range(5, 8)):
        name      = cells.get((3, r))   # col C
        intercept = cells.get((4, r))   # col D
        coef      = cells.get((5, r))   # col E
        pval      = cells.get((6, r))   # col F
        weight    = cells.get((7, r))   # col G
        sd        = cells.get((8, r))   # col H
        mevs.append({
            "name": str(name).strip() if name else f"MEV{i+1}",
            "intercept": float(intercept),
            "coefficient": float(coef),
            "p_value": float(pval),
            "weight": float(weight),
            "standard_deviation": float(sd),
            "stress_unit_multiplier": unit_multiplier[i],
        })
    return mevs

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------
def main(xlsm_path, project_root):
    project_root = Path(project_root)
    static_dir = project_root / "data-raw" / "static"
    config_dir = project_root / "config"
    static_dir.mkdir(parents=True, exist_ok=True)
    config_dir.mkdir(parents=True, exist_ok=True)

    print(f"Loading {xlsm_path} ...")
    sheet_paths, shared, tmp_dir = load_xlsm(xlsm_path)
    print(f"Found {len(sheet_paths)} sheets.")

    print("\n--- Assumptions sheet ---")
    a = dump_sheet(sheet_paths["Assumptions"], shared)

    rows = extract_off_balance_products(a)
    write_csv(static_dir / "off_balance_products.csv",
              ["product_code", "lic_input_code"], rows)

    industry_rows, others_default = extract_industry_sector_mapping(a)
    write_csv(static_dir / "industry_sector_mapping.csv",
              ["industry_code", "industry_description", "sector"], industry_rows)
    print(f"  (default sector for unmapped codes: {others_default!r})")

    write_csv(static_dir / "collective_assessment_rules.csv",
              ["sector", "dpd_threshold", "rating"],
              extract_collective_assessment(a))

    write_csv(static_dir / "product_portfolio_mapping.csv",
              ["product_type", "portfolio"],
              extract_product_portfolio_mapping(a))

    write_csv(static_dir / "staging_thresholds.csv",
              ["key", "value", "description"],
              extract_staging_thresholds(a))

    print("\n--- MasterRatingScale sheet ---")
    m = dump_sheet(sheet_paths["MasterRatingScale"], shared)
    write_csv(static_dir / "master_rating_scale.csv",
              ["rating", "rating_type", "external_equivalent", "hierarchy"],
              extract_master_rating_scale(m))
    write_csv(static_dir / "master_rating_downgrade.csv",
              ["rating", "rating_after_1_notch_downgrade"],
              extract_rating_downgrade(m))

    print("\n--- Inputs_Lending Portfolio (severity table) ---")
    lp = dump_sheet(sheet_paths["Inputs_Lending Portfolio"], shared)
    write_csv(static_dir / "scenario_severity.csv",
              ["scenario", "assessment_guidance", "severity_label",
               "severity_z", "scenario_probability_weight"],
              extract_scenario_severity(lp))

    print("\n--- Inputs_Investment Portfolio (segment fallback) ---")
    ip = dump_sheet(sheet_paths["Inputs_Investment Portfolio"], shared)

    seg_fb = extract_segment_fallback_ratings(lp, ip)
    seg_fb_rows = [{"segment": k, "fallback_rating": v} for k, v in seg_fb.items()]
    write_csv(static_dir / "segment_fallback_ratings.csv",
              ["segment", "fallback_rating"], seg_fb_rows)
    print(f"  segment fallback ratings: {seg_fb}")

    # TTC PD table — Inputs_Lending Portfolio AT5:AU45.
    # Per-rating-grade 12-month through-the-cycle PD, used as the input
    # to the macro model (Phase E). One PD per rating grade — same set
    # of 21 ratings as in master_rating_scale (Internal block).
    ttc_rows = []
    for r in range(5, 50):
        rg = lp.get((46, r))
        pd_v = lp.get((47, r))
        if rg and pd_v and str(rg).strip():
            ttc_rows.append({"rating": rg, "ttc_pd": repr(float(pd_v))})
    write_csv(static_dir / "ttc_pd_table.csv",
              ["rating", "ttc_pd"], ttc_rows)
    print(f"  TTC PD table: {len(ttc_rows)} rating grades")

    # External TTC PD table — Inputs_Investment Portfolio P5:Q25.
    # Used by external-rated portfolios (Banks and Fis, Investments) when
    # building their PD term structure via the same macro model machinery.
    ext_ttc_rows = []
    for r in range(5, 26):
        rg = ip.get((16, r))
        pd_v = ip.get((17, r))
        if rg and pd_v is not None and str(rg).strip():
            ext_ttc_rows.append({"rating": rg, "ttc_pd": repr(float(pd_v))})
    write_csv(static_dir / "ttc_pd_table_external.csv",
              ["rating", "ttc_pd"], ext_ttc_rows)
    print(f"  External TTC PD table: {len(ext_ttc_rows)} rating grades")

    print("\n--- GCC GDP sheet ---")
    g = dump_sheet(sheet_paths["GCC GDP"], shared)
    write_csv(static_dir / "gcc_real_gdp_growth.csv",
              ["country", "year", "value"],
              extract_gcc_gdp(g, block_label_row=1,
                              data_start_row=3, data_end_row=8))
    write_csv(static_dir / "gcc_gdp_current_prices.csv",
              ["country", "year", "value"],
              extract_gcc_gdp(g, block_label_row=10,
                              data_start_row=12, data_end_row=17))
    gcc_dist = extract_gcc_distribution(g)
    print(f"  GCC growth distribution: mean={gcc_dist['mean']}, "
          f"sd={gcc_dist['standard_deviation']}")

    print("\n--- Model Specifications sheet ---")
    ms = dump_sheet(sheet_paths["Model Specifications"], shared)
    mevs = extract_model_specifications(ms)
    for m in mevs:
        print(f"  MEV: {m['name']}")
        print(f"       intercept={m['intercept']}, coef={m['coefficient']}, "
              f"weight={m['weight']}, sd={m['standard_deviation']}")

    print("\n--- writing config/model_config.yml ---")
    write_model_config(config_dir / "model_config.yml",
                       mevs=mevs,
                       gcc_dist=gcc_dist)

    print("\n--- writing data-raw/README.md ---")
    write_dataraw_readme(project_root / "data-raw" / "README.md")

    print("\nAll done.")
    # Cleanup
    import shutil
    shutil.rmtree(tmp_dir, ignore_errors=True)


def write_model_config(path, mevs, gcc_dist):
    """Write model_config.yml. We hand-format the YAML so the file is easy to
       read and review in PRs (PyYAML's default formatter is uglier).

       Note: segment fallback ratings live in
       data-raw/static/segment_fallback_ratings.csv, not in this YAML.
    """
    lines = []
    lines.append("# Macro model + run parameters")
    lines.append("# Source: 'Model Specifications', 'GCC GDP' and 'Calculations' sheets")
    lines.append("# of Updated_ETL_File__Test__V7_-_2025-NS.xlsm. Regenerated by")
    lines.append("# data-raw/extract_from_xlsm.py — do not edit by hand unless you")
    lines.append("# also update the source workbook.")
    lines.append("")
    lines.append("model:")
    lines.append('  name: "3-Variable Model"')
    lines.append("")
    lines.append("  # Through-the-cycle anchor PD used in the stressing-factor formula:")
    lines.append("  #   SF[t,mev] = NORM.S.INV(PD[t,mev]) - NORM.S.INV(ttc_anchor_pd)")
    lines.append("  # Source: hardcoded constant 0.146 in Calculations!D31 formula.")
    lines.append("  ttc_anchor_pd: 0.146")
    lines.append("")
    lines.append("  # Maximum maturity (months) for the PD term structure output.")
    lines.append("  # Source: Calculations!H3 (default 50).")
    lines.append("  max_maturity: 50")
    lines.append("")
    lines.append("  # Number of MEV horizons supplied per run. Recomputed per run from")
    lines.append("  # the runtime MEV forecast (see Calculations!G3).")
    lines.append("  # max_available_horizons: 5  # informational only — not read at runtime")
    lines.append("")
    lines.append("  # Macroeconomic variables and their satellite-model parameters.")
    lines.append("  # Source: 'Model Specifications' sheet rows 5-7.")
    lines.append("  mevs:")
    for m in mevs:
        lines.append(f'    - name: "{m["name"]}"')
        lines.append(f'      intercept: {m["intercept"]!r}')
        lines.append(f'      coefficient: {m["coefficient"]!r}')
        lines.append(f'      p_value: {m["p_value"]!r}')
        lines.append(f'      weight: {m["weight"]!r}')
        lines.append(f'      standard_deviation: {m["standard_deviation"]!r}')
        lines.append(f'      stress_unit_multiplier: {m["stress_unit_multiplier"]}')
        lines.append("")
    lines.append("# GCC composite Real GDP Growth distribution stats")
    lines.append("# Used in the externally-rated weight calculation:")
    lines.append("#   weight[s] = NORM.DIST(scenario_gdp[s], gcc_growth.mean,")
    lines.append("#                         gcc_growth.standard_deviation, TRUE)")
    lines.append("# Source: GCC GDP!B30 (mean) and GCC GDP!B31 (sd) — derived from the")
    lines.append("# historical data in data-raw/static/gcc_real_gdp_growth.csv.")
    lines.append("gcc_growth_distribution:")
    lines.append(f"  mean: {gcc_dist['mean']!r}")
    lines.append(f"  standard_deviation: {gcc_dist['standard_deviation']!r}")
    lines.append("")
    with open(path, "w") as f:
        f.write("\n".join(lines))
    print(f"  wrote {path}")


def write_dataraw_readme(path):
    txt = """# data-raw/

Reference data extracted from the Excel ETL tool. **Committed to git** —
the R code reads from `data-raw/static/` at run time.

## Provenance

Generated by `data-raw/extract_from_xlsm.py` reading from
`Updated_ETL_File__Test__V7_-_2025-NS.xlsm`. Regenerate by running:

```
python data-raw/extract_from_xlsm.py path/to/<workbook>.xlsm
```

## Files

| File                                          | Source in workbook                                          |
| --------------------------------------------- | ----------------------------------------------------------- |
| `static/off_balance_products.csv`             | Assumptions B3:C9                                            |
| `static/industry_sector_mapping.csv`          | Assumptions E3:G_end                                         |
| `static/collective_assessment_rules.csv`      | Assumptions I:J (sectoral DPD-rating sub-tables)             |
| `static/product_portfolio_mapping.csv`        | Assumptions Q3:R13                                           |
| `static/staging_thresholds.csv`               | Assumptions L3, N3, O3                                       |
| `static/master_rating_scale.csv`              | MasterRatingScale B4:D24 + F25:G45                           |
| `static/master_rating_downgrade.csv`          | MasterRatingScale J4:K45                                     |
| `static/scenario_severity.csv`                | Inputs_Lending Portfolio AA5:AE9                             |
| `static/gcc_real_gdp_growth.csv`              | GCC GDP A2:AS8 (long-form: country/year/value)               |
| `static/gcc_gdp_current_prices.csv`           | GCC GDP A11:AS17                                             |
| `../config/model_config.yml`                  | Model Specifications + GCC GDP B30:B31 + MasterRatingScale O4 |

## Things this does NOT capture

- The five **scenario MEV forecasts** (e.g. `Significant Downturn` rows 9-13).
  These are user inputs that change every run, not static reference data.
  They will be supplied per-run via runtime configuration in Phase E.
- The **derived PD term structures** in the Calculations sheet. These are
  *outputs* of the macro model, not inputs.
- Any **cell formulas**. Only computed values are extracted.
"""
    with open(path, "w") as f:
        f.write(txt)
    print(f"  wrote {path}")


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print(__doc__)
        sys.exit(1)
    xlsm = sys.argv[1]
    root = sys.argv[2] if len(sys.argv) > 2 else Path(__file__).parent.parent
    main(xlsm, root)
