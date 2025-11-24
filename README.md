# 📊 Table Analyzer

Table Analyzer is a modular R/Shiny application that walks researchers from raw spreadsheets to publication-ready figures and tables. The v1.00 app ships with dedicated tabs for uploading, filtering, modeling, and visualization so you can run end-to-end analyses without leaving the browser.

### Current status

- Stable Shiny app focused on interactive descriptive statistics, ANOVA (one- and two-way), linear and mixed models, correlations, and PCA.
- Built-in demo datasets let you try the workflow immediately; Excel uploads support both tidy long and two-row-header wide formats.
- Export buttons mirror what you see in the UI—model summaries, Type-III ANOVA tables, diagnostic plots, and Word reports for LM/LMM modules.

---

## ✨ Highlights

- **Excel-native uploads**
  - Accepts long-format workbooks or wide-format plates with two header rows (response × replicate). Wide sheets are reshaped automatically and validated for duplicate measurements.
  - Bundled demo datasets illustrate both layouts and can be loaded instantly from the UI.
  - Ambiguous numeric columns (≤10 distinct values) can be re-typed as categorical factors directly in the upload panel, and column names are cleaned automatically with `janitor::clean_names()` preprocessing.
- **Interactive filtering**
  - Choose any subset of columns, then refine rows with auto-generated range sliders (numeric), checkboxes (logical), or multi-select pickers (categorical).
  - The filtered preview updates live and feeds downstream modules.
- **Analysis hub**
  - Modules: Descriptive statistics, One-way ANOVA, Two-way ANOVA, Linear Model (LM), Linear Mixed Model (LMM), Pairwise Correlation, and Principal Component Analysis (PCA).
  - ANOVA, LM, and LMM modules accept multiple responses and fit them as independent models; each run reports formulas, tidy summaries, Type-III ANOVA tables, downloadable `.docx` reports (LM/LMM) with formatted coefficients, random-effects variance, and ICC, plus optional per-analysis stratification.
- **Visualization gallery**
  - Dedicated panels mirror the active analysis: descriptive dashboards, PCA biplots with optional loadings, correlation pair grids (`GGally::ggpairs`), and ANOVA interaction plots.
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
# Install core packages (run once)
install.packages(c(
  "bslib", "car", "digest", "dplyr", "DT", "emmeans", "fitdistrplus",
  "flextable", "GGally", "ggplot2", "ggsignif", "janitor", "lmerTest",
  "officer", "patchwork", "readxl", "shiny", "shinyjqui", "skimr",
  "tidyr", "zoo"
  # Optional: ggrepel for PCA loadings labels
))

# Launch the app from the repository root
shiny::runApp(".")
```

## 📂 Data expectations

- Numeric responses should be stored in numeric columns.
- Factors can be provided as factors or characters; level order controls in the Analysis tab set the reference.
- Missing values are accepted—modules fall back to complete-case subsets where necessary.
- **Stratification** is optional but available across modules; for best readability, keep stratum levels to ≲10.

---

## 📦 Exports & reporting

- Every module exposes a “Download all results” button that bundles the text outputs currently displayed.
- LM/LMM exports generate Word (`.docx`) reports with ANOVA tables, model coefficients, random-effects variance (if applicable), and ICC summaries.
- PCA, correlation, and descriptive visuals can be saved via each plot’s built-in download controls.

---

## 🧪 Development notes

- Regression exports rely on `flextable` and `officer`; install these packages to avoid runtime errors.
- Wide-format ingestion is safeguarded by unit tests in `tests/test_convert_wide_to_long.R`. Run them with:
  ```bash
  Rscript tests/test_convert_wide_to_long.R
  ```
- Helper scripts in `dev/` illustrate layout prototypes and can be sourced during development, but are not required for production use.

---

## 📝 License

MIT (or update with your project’s chosen license).

---

## 🙏 Acknowledgments

Built by the Table Analyzer team. Inspired by best practices for transparent statistical reporting and reproducible research.

---
## 🔍 Transparency for users and reviewers

Every analysis tab in Table Analyzer maps directly to familiar R functions. The table below summarizes what is executed and which options the UI fills in on your behalf.

| Module | Core R routines | Key options populated by the app |
| --- | --- | ---|
| Descriptive statistics | `skimr::skim()` for overall and stratified summaries; `dplyr::summarise()` to add coefficients of variation, outlier counts, and five-number summaries. | Selected categorical variables, numeric variables, optional stratification factor. Missing values are dropped column-wise for each summary. |
| One-way ANOVA | `stats::lm(response ~ factor)` with Type-III tests from `car::Anova(type = 3)`; pairwise group comparisons via `emmeans::emmeans()` + `contrast(..., method = "pairwise", adjust = "tukey")`. | Responses chosen in the UI, single categorical predictor with user-defined level order, optional stratification factor. |
| Two-way ANOVA | `stats::lm(response ~ factor1 * factor2)` with Type-III tables from `car::Anova()` and Tukey-adjusted marginal means for each main effect through `emmeans`. | Responses plus two categorical predictors and their level orders; optional stratification variable drives per-stratum models. |
| Linear model (LM) | `stats::lm(response ~ predictors)`; inference combines `car::Anova(type = 3)` and `summary.lm()`, with residual plots based on fitted values, residuals, and Q-Q diagnostics from `stats`. | Numeric response, categorical predictors, numeric covariates, optional two-way interactions. Stratification fits one model per subgroup. |
| Linear mixed model (LMM) | `lmerTest::lmer(response ~ fixed + covariates + interactions + (1\|random))`; fixed-effect tests via `anova(type = 3)` from **lmerTest**, ICC derived from `lme4::VarCorr()`; residual checks mirror the LM module. | Same inputs as LM plus a random-intercept factor. Stratification runs a separate mixed model for each level. |
| Pairwise correlations | `stats::cor(..., use = "pairwise.complete.obs")` for coefficients; visual diagnostics via `GGally::ggpairs()` with correlation, scatter, and density panels. | Numeric variables selected in the UI and optional stratification factor; each stratum is analyzed independently. |
| Principal component analysis | `stats::prcomp(center = TRUE, scale. = TRUE)` applied to complete cases for the selected columns, with variance summaries from `summary(prcomp)` and loadings displayed. | Numeric variables selected for PCA. Rows missing any selected variable are removed before fitting, and the excluded count is reported. |

When exporting results, each module bundles the rendered tables, model summaries, and diagnostic plots based on these function calls, enabling straightforward reproduction and review.

---

## 📚 How to cite Table Analyzer

If Table Analyzer supports your research, please cite it so others can discover the tool:

**Palmieri, N.** (2025). *Table Analyzer: Turn your tabular data into publication-ready tables and plots* (Version 1.00). Available at [https://github.com/nicola-palmieri/TableAnalyzer](https://github.com/nicola-palmieri/TableAnalyzer).


