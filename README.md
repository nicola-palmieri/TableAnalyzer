# 📊 Table Analyzer

Table Analyzer is a modular R/Shiny application for analysing tabular datasets with modern statistical methods and generating publication-ready visualizations. It can process multiple variables in parallel, apply optional stratification, and produce fully auto-arranged plots without any manual positioning. Launching the app (via app.R) provides focused modules for uploading data, configuring analyses, refining inputs, and reviewing results — all powered by R directly in the browser.

---

## ✨ Highlights

- **Excel-native uploads**
  - Accepts long-format workbooks or wide-format plates with two header rows (response × replicate). Wide sheets are reshaped automatically and validated for duplicate measurements.
  - Bundled demo datasets illustrate both layouts and can be loaded instantly from the UI.
  - Ambiguous numeric columns (≤10 distinct values) can be re-typed as categorical factors directly in the upload panel.
- **Interactive filtering**
  - Choose any subset of columns, then refine rows with auto-generated range sliders (numeric), checkboxes (logical), or multi-select pickers (categorical).
  - The filtered preview updates live and feeds downstream modules.
- **Analysis hub**
  - Modules: Descriptive statistics, One-way ANOVA, Two-way ANOVA, Linear Model (LM), Linear Mixed Model (LMM), Pairwise Correlation, and Principal Component Analysis (PCA).
  - ANOVA, LM, and LMM modules accept multiple responses and fit them as independent models; each run reports formulas, tidy summaries, Type-III ANOVA tables, downloadable `.docx` reports (LM/LMM) with formatted coefficients, random-effects variance, and ICC, plus optional per-analysis stratification (running the full analysis separately for every level of a chosen grouping variable).
- **Visualization gallery**
  - Dedicated panels mirror the active analysis: descriptive dashboards, ANOVA interaction plots, LM/LMM diagnostics, correlation pair grids (`GGally::ggpairs`), and PCA biplots with optional loadings.
  - Built-in color palettes can be customized per grouping level.
- **Reproducibility first**
  - Model formulas and factor level orders are always explicit.
  - Stratified analyses repeat the full pipeline per subgroup, and download bundles include every table shown in the UI.

---

## 🧭 App workflow

1. **Upload** (Tab “1️⃣ Upload”)
   - Select the example dataset or upload Excel workbooks (`.xlsx`, `.xls`, `.xlsm`).
   - For wide layouts, Table Analyzer reshapes the sheet to tidy long format after reconciling multi-row headers.
   - Review validation messages and the live preview before proceeding.
2. **Filter** (Tab “2️⃣ Filter”)
   - Pick the columns you care about and adjust numeric ranges or factor selections to create the analysis-ready subset.
3. **Analyze** (Tab “3️⃣ Analyze”)
   - Choose a module and configure responses, predictors, covariates, interactions, stratification, and (for LMM) random intercepts.
   - Click **Show results** to run the model; export everything with **Download all results**.
4. **Visualize** (Tab “4️⃣ Visualize”)
   - Explore plots tailored to the active analysis, including multi-panel layouts for stratified fits and customizable color themes.

---

## 🔧 Installation & local launch

```r
# Install packages (run once)
install.packages(c(
  "bslib", "dplyr", "DT", "emmeans", "fitdistrplus", "flextable",
  "GGally", "ggplot2", "lmerTest", "officer", "patchwork",
  "readxl", "shiny", "skimr", "tidyr", "zoo"
))

# Launch the app from the repository root
shiny::runApp(".")
```

## 📂 Data expectations

- Columns are automatically typed as numeric or categorical based on their contents on upload, so you can load tidy-long files or two-row-header wide sheets without pre-cleaning.
- Ambiguous numeric columns (numbers with ≤10 distinct values) are flagged in the Upload tab so you can choose whether to treat them as continuous or categorical; that choice controls whether descriptive statistics return means/SDs or counts/percentages and whether linear models encode the variable as a covariate or a set of indicator levels.
- Missing values are accepted—modules fall back to complete-case subsets where necessary.
- Stratification is optional across modules; it works with categorical variables that have up to about ten levels.

---

## 📦 Exports & reporting

- Every module exposes a “Download all results” button that bundles the text outputs currently displayed; model tables use publication-style formatting with top and bottom rules for the header and a closing border at the table foot.
- LM/LMM exports generate Word (`.docx`) reports with ANOVA tables, model coefficients, random-effects variance (if applicable), and ICC summaries rendered with the same publication-style borders.
- All plots download as publication-ready PNG files (300 dpi) with customizable width and height; PCA, correlation, and descriptive visuals can also be saved via each plot’s built-in controls.

---
## 🔍 Transparency for users and reviewers

Every analysis tab in Table Analyzer maps directly to familiar R functions. The table below summarizes what is executed and which options the UI fills in on your behalf.

| Module | Core R routines | Key options populated by the app |
| --- | --- | ---|
| Descriptive statistics | `skimr::skim()` for overall and stratified summaries; `dplyr::summarise()` to add coefficients of variation, outlier counts, and five-number summaries. | Selected categorical variables, numeric variables, optional stratification factor. Missing values are dropped column-wise for each summary. |
| One-way ANOVA | `stats::lm(response ~ factor)` with Type-III tests from `car::Anova(type = 3)`; reference-vs-others contrasts via `emmeans::contrast(..., method = "trt.vs.ctrl")` (Dunnett adjustment). | Responses chosen in the UI, single categorical predictor with user-defined level order, optional stratification factor. |
| Two-way ANOVA | `stats::lm(response ~ factor1 * factor2)` with Type-III tables from `car::Anova()` and nested reference-vs-others contrasts for each factor using `emmeans::contrast(..., method = "trt.vs.ctrl")` (Dunnett-style comparisons within each group). | Responses plus two categorical predictors and their level orders; optional stratification variable drives per-stratum models. |
| Linear model (LM) | `stats::lm(response ~ predictors)`; inference combines `car::Anova(type = 3)` and `summary.lm()`, with residual plots based on fitted values, residuals, and Q-Q diagnostics from `stats`. | Numeric response, categorical predictors, numeric covariates, optional two-way interactions. Stratification fits one model per subgroup. |
| Linear mixed model (LMM) | `lmerTest::lmer(response ~ fixed + covariates + interactions + (1\|random))`; fixed-effect tests via `anova(type = 3)` from **lmerTest**, ICC derived from `lme4::VarCorr()`; residual checks mirror the LM module. | Same inputs as LM plus a random-intercept factor. Stratification runs a separate mixed model for each level. |
| Pairwise correlations | `stats::cor(..., use = "pairwise.complete.obs")` for coefficients; visual diagnostics via `GGally::ggpairs()` with correlation, scatter, and density panels. | Numeric variables selected in the UI and optional stratification factor; each stratum is analyzed independently. |
| Principal component analysis | `stats::prcomp(center = TRUE, scale. = TRUE)` applied to complete cases for the selected columns, with variance summaries from `summary(prcomp)` and loadings displayed. | Numeric variables selected for PCA. Rows missing any selected variable are removed before fitting, and the excluded count is reported. |
