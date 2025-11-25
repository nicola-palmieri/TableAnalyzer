# Demo-ready input files

The following datasets were opened to confirm they load cleanly and provide representative outputs for Shiny demos. Sheet names and row/column counts come from inspecting the workbooks.

## Example and reference files

- **data/Data at necropsy_all groups_all days_Table Analyzer.xlsx**
  - Sheet: `All` (144 rows × 18 cols)
  - Format: long table with treatment, breed, weight, clinical observations, and viral load metrics already tidy.
  - Expected output: loads directly with the "Example dataset" option and exposes multiple numeric outcomes for analysis/visualization.

- **data/toy_animal_trial_data_long.xlsx**
  - Sheet: `Sheet1` (120 rows × 12 cols)
  - Format: tidy long measurements for treatment/diet/day with multiple numeric endpoints.
  - Expected output: should render immediately in the long-format upload flow with no type disambiguation needed.

- **data/toy_animal_trial_data_long_withNA.xlsx**
  - Sheet: `Sheet1` (120 rows × 12 cols)
  - Format: same schema as the long toy data, but with injected `NA` values to exercise missing-data handling.
  - Expected output: upload validates after null checks and allows testing of downstream imputation/filters.

- **data/toy_animal_trial_data_wide.xlsx**
  - Sheet: `Wide_PoultryTrial` (121 rows × 15 cols)
  - Format: two-header-row wide layout with response names on row 1 and replicate identifiers on row 2.
  - Expected output: wide-to-long conversion should create a `Replicate` column, reshape the measurement blocks, and surface unique variable columns without duplicates.

- **data/toy_animal_trial_repeated.xlsx**
  - Sheet: `Sheet1` (270 rows × 4 cols)
  - Format: long repeated-measures table keyed by `ChickenID`, `Treatment`, and `Week`.
  - Expected output: uploads cleanly for longitudinal visualizations; useful for faceting by treatment over time.

- **data/Pegi field investigation eggs swabs_PgV5utr_PgVA_PgC.xlsx**
  - Sheets: `Swabs` (48 rows × 9 cols) and `Eggs`
  - Format: ct/ΔCT assay results by farm/pool/sex.
  - Expected output: each sheet should load as a long-format dataset; categorical grouping for farm/pool with numeric viral metrics.
