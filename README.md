# TableAnalyzer v1.10

TableAnalyzer is an R/Shiny app for exploring Excel datasets, fitting statistical
models and creating publication-ready figures and reports. It is the twin companion
to [MicrobiomeAnalyzer](https://github.com/nicola-palmieri/MicrobiomeAnalyzer).

---

## Open the app

R runs on your computer; the browser displays the Shiny interface. Follow the
installation instructions below. The former shinyapps.io deployment has been retired.

---

## Highlights

- **Excel-native uploads**
  - Accepts long-format workbooks or wide-format plates with two header rows (response × replicate). Wide sheets are reshaped automatically and validated for duplicate measurements.
  - Bundled demo datasets illustrate both layouts and can be loaded instantly from the UI.
  - Numeric columns can be re-typed as categorical factors using the column type editor in the Upload tab.
- **Interactive filtering**
  - Choose any subset of columns, then refine rows with auto-generated range sliders (numeric), checkboxes (logical), or multi-select pickers (categorical).
  - The filtered preview updates live and feeds downstream modules.
- **Analysis hub**
  - Modules: Descriptive statistics, One-way ANOVA, Two-way ANOVA, Linear Model (LM), Linear Mixed Model (LMM), Pairwise Correlation, and Principal Component Analysis (PCA).
  - ANOVA, LM, and LMM modules accept multiple responses and fit them as independent models; each run reports formulas, tidy summaries, and Type-III ANOVA tables, plus optional per-analysis stratification (running the full analysis separately for every level of a chosen grouping variable).
  - ANOVA/LM/LMM exports generate `.docx` reports; LM/LMM reports add formatted coefficients, random-effects variance, and ICC summaries.
- **Visualization gallery**
  - Dedicated panels mirror the active analysis: descriptive dashboards, ANOVA interaction plots, LM/LMM diagnostics, correlation pair grids (`GGally::ggpairs`), and PCA biplots with optional loadings.
  - Built-in color palettes can be customized per grouping level.
- **Reproducibility first**
  - Model formulas and factor level orders are always explicit.
  - Stratified analyses repeat the full pipeline per subgroup, and download bundles include every table shown in the UI.

---

## App workflow

1. **Upload:** choose an example or upload an Excel workbook; review the preview and column types. Wide-format plates are reshaped to long format.
2. **Filter:** optionally select columns and refine rows before analysis.
3. **Analyze:** select a method, configure its variables and study design, then run the analysis.
4. **Visualize:** inspect the fitted analysis, customize its figures and download available plots or reports.

---

## User guide

Read the [User guide](docs/user-guide.md) for input examples, controls and analysis workflows.

---

## Installation & local launch

Install R first, then open the extracted or cloned app folder in R/RStudio. Your
working directory must be the repository root, containing `app.R` and `scripts/`.
Development currently uses R 4.6.2.

```r
# One-time setup
source("scripts/install_dependencies.R")

# Start the local app
shiny::runApp(".", host = "127.0.0.1", launch.browser = TRUE)
```

Setup installs missing CRAN packages and the included NicoShinyAppTemplate source.
Internet is needed for missing CRAN packages. Once setup is complete, the Shiny
app runs locally in your browser and works offline.

Legends default to the right, including after Reset. Other legend positions remain available.

### Included shared template

Both apps include `dependencies/NicoShinyAppTemplate` and a version/file-hash
manifest at `dependencies/NicoShinyAppTemplate.json`. Users do not need a separate
template download, GitHub token or access to the template repository.

The [template repository](https://github.com/nicola-palmieri/NicoShinyAppTemplate)
is the maintained source. The app copy is generated for distribution; do not edit
it independently. See the [bundled package documentation](dependencies/NicoShinyAppTemplate/README.md).

---

## Data expectations

- Columns are automatically typed as numeric or categorical based on their contents on upload, so you can load tidy-long files or two-row-header wide sheets without pre-cleaning.
- Missing values are accepted—modules fall back to complete-case subsets where necessary.
- Stratification is optional; choose from the eligible categorical variables offered by the selected analysis.
- Maximum upload size: ~200 MB (configured in `app.R`).

---

## Exports & reporting

- Figures can be downloaded as PNG files at **300 dpi** with configurable dimensions.
- Supported analyses export Word (`.docx`) reports, including relevant summaries and model tables.
- LM/LMM reports include coefficients; mixed-model reports also include random-effect and ICC summaries.
- Select the analysis and plotting options appropriate to your data before exporting.

---

## Development & tests

```r
install.packages(c("openxlsx", "testthat"))
source("tests/run_tests.R")
```

Tests use synthetic datasets and temporary Excel workbooks.

Application modules live under `R/`; their data handling and statistical methods
remain app-specific. Shared presentation is maintained in NicoShinyAppTemplate.

In the shared AI Academy workspace, edit the central template checkout and run
`Rscript ../../scripts/sync_shared_template.R` from the app folder before
publishing updated app snapshots. Standalone users do not need that workspace.

The bundled package tests can be run from either app folder:

```r
install.packages(c("testthat", "pkgload"))
testthat::test_local("dependencies/NicoShinyAppTemplate")
```

Development checks use synthetic examples and fixtures; passing tests does not
establish that a model is appropriate for every study.

---

## How to cite

If you use Table Analyzer in a publication, please cite it as:

> Palmieri, N. (2026). *Table Analyzer* (Version 1.10) [Computer software]. Zenodo. https://doi.org/10.5281/zenodo.19233119

For a development checkout, also record the exact repository commit used.

---

## Transparency for users and reviewers

Every analysis tab in Table Analyzer maps directly to familiar R functions. The table below summarizes what is executed and which options the UI fills in on your behalf.

| Module | Core R routines | Key options populated by the app |
| --- | --- | ---|
| Descriptive statistics | `skimr::skim()` for overall and stratified summaries; `dplyr::summarise()` to add coefficients of variation, outlier counts, and five-number summaries. | Selected categorical variables, numeric variables, optional stratification factor. Missing values are dropped column-wise for each summary. |
| One-way ANOVA | `stats::lm(response ~ factor)` with sum-to-zero contrasts and Type-III tests from `car::Anova(type = 3)`; reference-vs-others contrasts via `emmeans::contrast(..., method = "trt.vs.ctrl")` (Dunnett adjustment). | Responses chosen in the UI, single categorical predictor with user-defined level order, optional stratification factor. |
| Two-way ANOVA | `stats::lm(response ~ factor1 * factor2)` with sum-to-zero contrasts and Type-III tables from `car::Anova()`; nested reference-vs-others contrasts use `emmeans::contrast(..., method = "trt.vs.ctrl")`. | Responses plus two categorical predictors and their level orders; optional stratification variable drives per-stratum models. |
| Linear model (LM) | `stats::lm(response ~ predictors)` with sum-to-zero contrasts for categorical predictors; inference combines `car::Anova(type = 3)` and `summary.lm()`, with residual and Q-Q diagnostics from `stats`. | Numeric response, categorical predictors, numeric covariates, optional two-way interactions. Stratification fits one model per subgroup. |
| Linear mixed model (LMM) | `lmerTest::lmer(response ~ fixed + covariates + interactions + (1\|random))` with sum-to-zero contrasts for categorical fixed effects; fixed-effect tests use `anova(type = 3)`, and ICC is derived from `lme4::VarCorr()`. | Same inputs as LM plus a random-intercept factor. Stratification runs a separate mixed model for each level. |
| Pairwise correlations | `stats::cor(..., use = "pairwise.complete.obs")` for coefficients; visual diagnostics via `GGally::ggpairs()` with correlation, scatter, and density panels. | Numeric variables selected in the UI and optional stratification factor; each stratum is analyzed independently. |
| Principal component analysis | `stats::prcomp(center = TRUE, scale. = TRUE)` applied to complete cases for the selected columns, with variance summaries from `summary(prcomp)` and loadings displayed. | Numeric variables selected for PCA. Rows missing any selected variable are removed before fitting, and the excluded count is reported. |
