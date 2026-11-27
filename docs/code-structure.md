# TableAnalyzer code structure

TableAnalyzer follows the same flat module convention as MicrobiomeAnalyzer.
Both apps consume `VetmeduniAppTemplate`; its source is maintained once in the
workspace `dependencies/` directory.

| File pattern | Responsibility |
| --- | --- |
| `R/00_shared_dependencies.R` | Identical bridge to the workspace dependency loader, or installed package. |
| `R/shared_app_config.R` | App identity, version and local branding assets, consumed by navbar and Home. |
| `R/mod_<page>_ui.R` | Page controls and result regions. Shared components render the shell, sidebars and expanders. |
| `R/mod_<page>_main.R` | Shiny server coordination, reactive state, execution and downloads. |
| `R/mod_<page>_analysis.R` | Pure scientific calculations where separate helpers are useful, currently descriptive summaries. |
| `R/mod_<page>_plot.R` | Pure figure builders for descriptive charts and PCA. |
| `R/mod_upload_data.R` | Workbook preprocessing and conversion from wide to long tables. |
| `R/shared_anova_*.R` | Table-specific ANOVA models, results, layouts and figures shared by both ANOVA pages. |
| `R/shared_regression_*.R` | Table-specific LM/LMM formulas, fits, diagnostics and report exports. |
| `R/shared_*.R` | App analysis helpers or thin adapters preserving existing control IDs and scientific defaults. |

The app calls `mod_home_ui/server`, `mod_upload_ui/server`, `mod_filter_ui/server`,
`mod_analyze_ui/server` and `mod_visualize_ui/server`. Their legacy public names
remain aliases so existing integrations keep working. Statistical and figure
function names remain stable. Former `module_*.R` entrypoints are now the
corresponding `mod_*_ui.R` and `mod_*_main.R` files; former `submodule_*.R`,
`anova_shared_*.R` and `regression_shared_*.R` helpers use the `shared_*` prefix.
Pure analysis/plot functions have the companion files described above.

The navbar and browser-tab icon use `www/logo.svg`; the shared template clips
navbar logos to a 26px square with 6px rounded corners. The Home banner uses the
decorative `www/home-analysis.svg`: transparent artwork with fine institutional green, gray
and burgundy strokes, soft fills and an orbital composition matching MicrobiomeAnalyzer.
Both assets are configured in `shared_app_config.R` and passed to the shared helpers.
The SVG icon preserves the original table/chart/magnifier illustration, embedded from the existing JPEG and mapped to derived Vetmeduni green shades. Icon geometry and navbar dimensions remain unchanged. All six institutional/neutral colors, semantic feedback and focus styling are supplied by the shared template; scientific plot colors remain app-owned.

Figures use the shared theme without plot titles or subtitles. Subgroup labels
use `plot_panel_label()` facet strips instead; categorical response names appear
on their axes. UI headings and statistical footnotes remain separate from figure titles.

Home uses the shared banner → workflow → informational analysis-card layout.
The banner description mirrors MicrobiomeAnalyzer: “Create publication-ready statistics and plots from an Excel table. Explore your data, compare groups and fit statistical models.” Both descriptions lead with the outcome and input, followed by app-specific capabilities.
Its workflow mirrors Upload, Filter, Analyze and Visualize; cards have an icon
beside their title and a description below, without links or buttons.

Downloads use the package's `app_download_ui()` in `app_result_header_ui()` at
the top right of each plot/results panel. Analysis UI lists expose `download`
alongside `config` and `results`; the coordinator mounts each method's download
in its header. Reset stays in sidebars; handlers and availability remain app-owned.

Analysis configurations use persistent per-method scaffolds: each lightweight
control structure is created once, and only the selected method is visible.
The selected method's server initializes lazily through the existing cache;
changing methods does not remount a shared configuration output. Scientific
input IDs and the explicit Run analysis action stay unchanged.

Visualize also keeps five lightweight method panels mounted. One active-type
flag exposes only the selected method after its analysis has valid results;
data, selection and Run guards retain their existing empty states. Each server
registers lazily once for the session and consumes changing data through its
existing reactives and reset tokens. Switching methods hides panels without
unbinding their nested controls and outputs.
Optional plot controls (colors, layout, legend and outlier selectors) clear
normally when model or data metadata is unavailable. PCA metadata falls back to
the current filtered data during method switches. `optional_plot_metadata`
handles these control states; actual plot/model validation remains separate and
technical errors propagate. Lightweight optional control outputs remain current
inside closed expanders and hidden method panels; plot outputs keep their normal
hidden-state suspension.

The package owns `app_navbar`, `app_home_ui`, `app_page_ui`, `app_layout`,
`app_sidebar`, `app_expander`, the contextual help behavior, color dropdowns,
figure size widgets, and plot themes. `shared_theme.R`, `shared_base_size.R`,
`shared_subplot_size.R`, `shared_colors.R` and `shared_ui_table.R` adapt legacy
Table signatures to this public API. They contain no copied common styles,
scripts, theme implementations or generic Home markup.

Plot adapters select the shared theme kind without local axis/grid replacements.
Style sections, loading indicators, optional-control lifecycle and preview-table
defaults also delegate to the package; Table supplies its existing control IDs,
legend content, metadata type checks and table paging/column policies.
Upload/Filter retain the shared compact table toolbar/footer. Analysis type uses
the shared `app-complete-choices` class so every method is visible when opened;
grouped dropdowns share one white surface. Narrow-screen pagination keeps the
current page with Previous/Next rather than crowding all numbered buttons.

Sidebar controls follow the same sequence as the twin app: Analysis setup,
Plot options (including available grid layout controls), Plot size, Colors,
Text and legend, then Reset and downloads. Valid plot appearance changes update
automatically; statistical model execution remains an explicit Run analysis
action. Upload and Filter keep their controls directly visible, without expanders;
workbook-specific controls appear only for the relevant data source. Dropdowns
must show or scroll through every available option, including controls in modals.
Statistical analysis keeps its model setup and optional
stratification; Visualize keeps its method specific gallery. Existing input IDs,
statistical inputs and methods, model formulas, validation and report logic are
preserved. Common font and grouped color defaults come from the shared package;
Table retains its explicit context-specific settings and custom overrides.

`tests/` continues to exercise the Table scientific and reactive behavior.
`tests/helper-shared-dependencies.R` resolves the package in the same way as the
app. Cross-app presentation and browser tests are maintained in the workspace
integration suite.

## Color consistency checks

Run `Rscript scripts/review_colors.R [evidence-directory]` from the app root. It checks all seven analysis pages, all available visualization choices, shared feedback colors, the column-type modal, and desktop/tablet/mobile layouts. Screenshot/contrast evidence defaults to the system temporary folder. `--method=METHOD` repeats a selected analysis after a focused correction while retaining the previous case report. The runtime app does not load the audit helper.

Institutional colors belong to VetmeduniAppTemplate; blue/navy values in plot builders and regression diagnostics are deliberate scientific colors and remain unchanged. The old JPEG is retained as source artwork inside the green SVG icon. Unused development preview images are not application chrome.

The 2026-11-27 review covered 72 page/viewport cases (all seven analyses and all available plot types), with no sampled interface-text contrast failures or page overflow. Shared navbar, Home banner, workspace, wordmark and primary-button colors matched MicrobiomeAnalyzer exactly. Representative current screenshots are in `docs/assets/` and the guide. Statistical calculations and scientific palettes were not changed.
