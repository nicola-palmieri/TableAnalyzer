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
- **File types:** `.xlsx`, `.xls`, `.xlsm`. Maximum upload size: ~200 MB (as configured in `app.R`).

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
   - Numeric: min/max boxes.
   - Categorical: multi-select pickers.
   - Logical: checkboxes.
3. NA handling:
   - **Drop rows with any NA in selected columns:** when checked, any row containing an NA in the columns you choose will be removed.
   - **Columns to check for NA:** pick which columns to enforce for NA removal (defaults to all).
4. The filtered preview updates live; this subset is passed to analyses.

Tips:
- Use filtering to remove obvious outliers before modeling.
- If no rows remain, relax filters or check for missing values.

---

## Analyze tab (step 3)

General layout:
- **Select analysis type:** Top dropdown picks the module.
- **Advanced options → Stratify by:** Choose a grouping variable and (optionally) which levels to keep; analyses repeat per stratum.
- **Show results / Download results:** Run the analysis and export the outputs (text or `.docx` where available).

Module walkthroughs and widgets:
- **Descriptive statistics**
  - *Categorical variables:* Multi-select of factors/characters to count.
  - *Numeric variables:* Multi-select of numeric columns for means/SD/quantiles.
  - *Show summary / Download summary:* Compute and export the text summary.
- **One-way ANOVA**
  - *Response variables:* Choose one or more numeric outcomes.
  - *Categorical predictor:* Choose the grouping factor.
  - *Order of levels (first = reference):* Reorder factor levels; first becomes reference.
  - *Stratify by (advanced):* Optional group variable + level picker.
  - *Show results / Download results:* Fit ANOVA and export tables/post-hoc/diagnostics.
- **Two-way ANOVA**
  - *Response variables:* One or more numeric outcomes.
  - *Categorical predictor 1 (x-axis):* First factor.
  - *Categorical predictor 2 (lines):* Second factor.
  - *Order of levels (first = reference) for each factor:* Set the level order separately.
  - *Stratify by (advanced):* Optional; repeats the analysis per stratum.
  - *Show results / Download results:* Fit models and export.
- **Linear Model (LM)**
  - *Response variable(s):* One or more numeric outcomes.
  - *Categorical predictors:* Factors to include as fixed effects; each gets a level-order control (reference = first level).
  - *Numeric predictors:* Continuous covariates.
  - *Interactions:* Choose two-way interactions among categorical predictors.
  - *Formula preview:* Shows the model formula that will be fitted.
  - *Stratify by (advanced):* Optional per-group fits.
  - *Show results / Download results:* Fit LM(s), show summaries and diagnostics, export `.docx`.
- **Linear Mixed Model (LMM)**
  - Same as LM plus:
  - *Random effect(s) (categorical):* Grouping factors for random intercepts.
  - *Nest random effects in selection order:* Treat selected random effects as nested.
  - *Show results / Download results:* Fit LMM(s), show fixed/random effects, ICC, diagnostics, export `.docx`.
- **Pairwise correlation**
  - *Numeric variables:* Select ≥2 numeric columns for the matrix.
  - *Stratify by (advanced):* Optional; computes a matrix per group.
  - *Show correlation matrix / Download results:* Compute correlations (and later plots) and export text.
- **Principal Component Analysis (PCA)**
  - *Numeric variables:* Select ≥2 numeric columns; rows missing any selected variable are dropped for PCA.
  - Stratification is disabled (PCA uses one common coordinate system).
  - *Show PCA summary / Download results:* Compute PCA, view summary/loadings, see excluded-row counts, export text.

---

## Visualize tab (step 4)

The tab shows only the visuals that match the most recent analysis. Each panel has **Apply changes** (to rebuild the plot) and **Download plot** (PNG, 300 dpi).

### Descriptive visualizations
- **Plot type:** Categorical barplots, numeric boxplots, or numeric histograms.
- **Categorical barplots:** Toggle counts vs proportions; set panel width/height; choose grid rows/cols; customize colors (by stratification group); base font size; optional common legend with position.
- **Numeric boxplots:** Toggle raw points and outlier highlighting; choose a column to label outliers; set panel size; grid rows/cols; color palette; base font size; optional common legend.
- **Numeric histograms:** Toggle density vs counts; set panel size; grid rows/cols; color palette; base font size; optional common legend.

### ANOVA visualizations (one-way, two-way)
- **Plot type:** Lineplots (mean ± SE), barplots (mean ± SE), or boxplots.
- **Lineplot options:** Connect means, show jittered points (one-way); connect means, dodge grouped means (two-way).
- **Axis/options:** Share y-axis across subplots, jitter density (two-way), subplot size, layout controls for arranging responses/strata.
- **Styling:** Color palette, base font size, optional common legend (two-way).

### Pairwise correlation visualizations
- **Plot type:** Pairwise scatterplot matrix (`GGally::ggpairs`).
- **Size and layout:** Set plot width/height; if stratified, choose grid rows/cols for multiple matrices.
- **Styling:** Color palette by group, base font size.

### PCA visualization
- **Colour points by:** Grouping variable (limited to ≤10 levels); optional group ellipses.
- **Label points by / Label size:** Pick a column for point labels and adjust text size.
- **Shape by:** Grouping variable for point shapes (≤10 levels).
- **Facet by:** Split the biplot into panels by a grouping variable.
- **Layout:** Choose rows/cols when faceting; set plot width/height.
- **Loadings:** Toggle arrows for variable loadings; adjust loading arrow scale.

If a control references a grouping variable that does not appear, return to **Upload → Edit column types** and set that column to **Categorical**, then rerun the analysis.

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
