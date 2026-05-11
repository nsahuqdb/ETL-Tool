# IFRS9 ETL — R port

R port of the Excel-based IFRS9 ETL tool (`Updated_ETL_File__Test__V7_-_2025-NS.xlsm`).

The tool reads bank-portfolio data exported from the core banking system (an `IFRSIN` folder of Oracle/Excel exports), applies IFRS9 staging rules and a 3-variable satellite macro model to produce monthly PD term structures, and writes a fixed set of CSV files to an `IFRSOUT` folder for the downstream LIC tool.

## Folder layout

```
ifrs9_etl/
├── README.md            this file
├── config.yml           run-time paths (where to read inputs from, where to write outputs)
├── DESCRIPTION          package-style metadata (used when we promote this to an R package)
├── .gitignore
│
├── R/                   all R source code (one file per logical module — populated in later phases)
│
├── data-raw/            REFERENCE DATA extracted from the Excel tool, committed to git
│   ├── README.md        explains the provenance of every file
│   └── static/          CSVs: industry mapping, rating scale, GCC GDP, etc.
│
├── config/              CONFIG that may change per-run but is not raw input data
│   └── model_config.yml the satellite macro model: coefficients, weights, severity z-scores, anchors
│
├── input/               RAW INPUT FILES at run-time (mirrors the Excel tool's IFRSIN folder)
│                        gitignored — drop AccountMaster.xlsx, etc. here before running
│
├── output/              RUN-TIME OUTPUT FILES (mirrors the Excel tool's IFRSOUT folder)
│                        gitignored — the 18 CSVs land here after a run
│
├── tests/               unit tests + reconciliation tests against the Excel tool
│
└── doc/                 additional design / mapping documentation
```

## How it maps to the Excel tool

| Excel concept                                  | R-port location                               |
| ---------------------------------------------- | --------------------------------------------- |
| `IFRSIN` folder                                | `input/`                                       |
| `IFRSOUT` folder                               | `output/`                                      |
| `Assumptions` sheet                            | `data-raw/static/*.csv`                        |
| `MasterRatingScale` sheet                      | `data-raw/static/master_rating_scale.csv`      |
| `Model Specifications` sheet                   | `config/model_config.yml`                      |
| `Inputs_Lending Portfolio` cells AA4:AE9       | `data-raw/static/scenario_severity.csv`        |
| `GCC GDP` sheet                                | `data-raw/static/gcc_*.csv`                    |
| 5 scenario sheets (MEV forecast)               | runtime input — provided per-run via config    |
| `Calculations` sheet                           | `R/macro_model.R` (Phase E)                    |
| `Transformation` sheet                         | `R/transform.R` (Phase D)                      |
| `RepaymentScheduleTransform` sheet             | `R/ead_curve.R` (Phase F)                      |
| `*Extract` sheets (12 of them)                 | `R/read_inputs.R` (Phase B)                    |
| `*Load` sheets (18 of them) + `Load*` macros   | `R/write_outputs.R` (Phase G)                  |
| `LoadData`, `ExtractData`, `TransformatData`   | `R/run_etl.R` orchestrator (Phase H)           |

## Running

To be defined in Phase H. Tentative entry-point:

```r
source("R/run_etl.R")
run_etl(config_path = "config.yml")
```

## Status

Phase A complete (skeleton + static reference extracted). All other phases pending.
