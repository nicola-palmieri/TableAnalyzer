# Table Analyzer — User Guide

This guide walks you through using Table Analyzer step by step. It is written for users with basic statistics knowledge. The goal is to get you from data upload to results and downloads with clear explanations of what each panel shows.

---

## Quick start

1. Install R packages listed in the README (`install.packages(c(...))`).
2. From the repo root, run `shiny::runApp(".")`.
3. Use the tabs in order: **Upload → Filter → Analyze → Visualize**.

Tip: If a categorical variable does not appear in dropdowns, go to the Upload tab, click **Edit column types**, and change it from Numeric to Categorical.

---

## Data formats

- **Long format (preferred):** One row per measurement. Columns include identifiers (e.g., Subject, Group) and numeric measurements.
- **Wide format:** Two header rows: first row = variable/response names, second row = replicate labels. The app reshapes this to long format for you.
- **Example files:** `data/toy_animal_trial_data_long.xlsx` and `data/toy_animal_trial_data_wide.xlsx`. Use them to see the expected structure.
- **Missing values:** Allowed. Analyses drop rows with missing values only where needed.
- **File types:** `.xlsx`, `.xls`, `.xlsm`. Maximum upload size is set by `shiny.maxRequestSize` (see `app.R`).

---

## Upload tab (step 1)

1. Choose **Data source**:
   - **Upload (long format):** Standard tall table.
   - **Upload (wide format):** Two header rows; the app will reshape to long.
   - **Example dataset:** Loads the bundled demo.
2. If uploading, select your Excel file. Pick the **Sheet** once detected.
3. For wide format, set **Replicate column name** (defaults to `Replicate`) and the app will convert to long.
4. Review the preview table and validation messages.
5. **Edit column types:** Click **Edit column types** to reclassify numeric-looking variables with few distinct values as Categorical.  
   - If you cannot find a categorical variable in later dropdowns, come back here and change its type to **Categorical**.

What happens under the hood:
- Wide sheets are reshaped; duplicate measurements trigger an error.
- Character/factor columns get ordered levels (numeric-aware sorting).
- Data is stored for downstream tabs once the preview is shown.

---

## Filter tab (step 2)

1. Select which columns to keep.
2. Adjust filters:
   - Numeric: range sliders.
   - Categorical: multi-select pickers.
   - Logical: checkboxes.
3. The filtered preview updates live; this subset is passed to analyses.

Tips:
- Use filtering to remove obvious outliers before modeling.
- If no rows remain, relax filters or check for missing values.

---

## Analyze tab (step 3)

Pick a module and fill its inputs. Each run processes the filtered data (and stratifies if you chose a grouping variable).

- **Descriptive statistics:** Select categorical and numeric variables. Outputs: counts, missingness, top categories, means/SD, quantiles, coefficients of variation, outlier counts, distribution guess, skewness, kurtosis. Stratification (optional) repeats summaries per group.
- **One-way ANOVA:** Choose responses (numeric) and one factor. Optional stratification repeats the full analysis per group.
- **Two-way ANOVA:** Choose responses plus two factors; interaction is included. Optional stratification repeats per group.
- **Linear Model (LM):** Choose one response, categorical predictors, numeric covariates, and optional two-way interactions. Optional stratification and exports to `.docx`.
- **Linear Mixed Model (LMM):** Same as LM plus a random-intercept factor. Uses `lmerTest` for tests; exports to `.docx`.
- **Pairwise correlation:** Select numeric variables; optional stratification computes correlations per group.
- **Principal Component Analysis (PCA):** Select numeric variables; rows missing any selected variable are removed. Outputs scores, loadings, and explained variance.

General steps:
1. Pick variables (responses, factors, covariates).
2. (Optional) Choose a stratification variable to run per-group analyses.
3. Click **Show results**. Use **Download results** where available.

---

## Visualize tab (step 4)

Shows charts tailored to the latest analysis:
- Descriptive: distribution plots.
- ANOVA: group comparisons and interaction plots.
- Correlation: `GGally::ggpairs` grid.
- PCA: biplots with optional loadings and grouping colors.
- LM/LMM: diagnostics (fitted vs residuals, Q-Q).

You can adjust plot options in the analysis modules; saved charts download as PNG (300 dpi).

---

## Interpreting outputs (plain-language)

### Descriptive summary
- **variable:** Column name (and group if stratified).
- **n_missing / complete_rate:** Missing count and percent complete.
- **n_unique / n_singletons:** Category counts (factors).
- **top_counts:** Most common categories with counts.
- **mean / sd / p0 / p25 / p50 / p75 / p100:** Average, spread, and percentiles (numeric).
- **cv:** Coefficient of variation (% of mean; higher = more variability).
- **outliers:** Count beyond 1.5 × IQR.
- **distribution:** Best-fitting family among Normal, Log-normal, Gamma, Weibull, Exponential.
- **skewness / kurtosis:** Shape diagnostics (0 ≈ Normal).

### ANOVA, LM, LMM
- **Effect/Term:** Predictors tested.
- **F/Chi-square, df, Pr(>F) or p-value:** Test statistic and significance.
- **Coefficients table:** Estimates, standard errors, t/z, p-values.
- **Random effects (LMM):** Grouping factor variance and SD.
- **ICC (LMM):** Intra-class correlation (share of variance due to groups).

### Correlation
- **Correlation matrix:** Pairwise coefficients (uses pairwise complete observations).
- **GGpairs grid:** Scatterplots, densities, and correlation annotations.

### PCA
- **Explained variance:** Percent variance by component.
- **Scores plot:** Samples in PC space.
- **Loadings:** Variable contributions; optional arrows/labels.

---

## Downloads

- **Summary text:** Descriptive stats as plain text.
- **ANOVA/LM/LMM:** Tables plus `.docx` reports (LM/LMM).
- **Plots:** PNG files (300 dpi) with configurable size.

---

## Troubleshooting and tips

- **Categorical variable missing from dropdowns:** Go to **Upload → Edit column types** and set it to **Categorical**, then rerun.
- **Wide upload errors about duplicates:** Ensure header pairs (variable, replicate) are unique.
- **Too many missing rows:** Check filters; consider removing variables with heavy missingness.
- **Stratification drops data:** Verify the chosen group has more than one non-missing level in filtered data.
- **Performance:** Large Excel files load more slowly; reduce columns or rows if possible.

Need more help? Open an issue with your dataset description (no sensitive data) and the steps you took.
