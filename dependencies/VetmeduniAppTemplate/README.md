# VetmeduniAppTemplate

Shared presentation components for MicrobiomeAnalyzer, TableAnalyzer and future Shiny apps. Branding, navigation, home cards, compact controls, accessible help, figure styling and PNG rendering live here; data processing and statistical models stay in each app.

This package has its own repository: [nicola-palmieri/VetmeduniAppTemplate](https://github.com/nicola-palmieri/VetmeduniAppTemplate). Install it once before starting a consuming app:

```r
install.packages("remotes")
remotes::install_github("nicola-palmieri/VetmeduniAppTemplate", upgrade = "never")
```

The repository is private, so installation needs a GitHub account with access and
authentication configured for remotes (normally `GITHUB_PAT` in the environment).
Keep credentials outside source files. The command above installs the current
default branch; apps should pin a tested commit or release for reproducibility.
No GitHub access is needed to run an app after its packages are installed.

For a downloaded source checkout, run `install.packages(".", repos = NULL, type = "source")`
from this package folder. In the AI Academy workspace, the shared loader uses
`pkgload::load_all()` with `dependencies/VetmeduniAppTemplate`, so both apps use the
same development source directly. Shared CSS and JavaScript are supplied by
`app_dependencies()` as an `htmlDependency`; applications do not copy them into
their own `www/` directories.

Restart both apps after shared-source edits; hard-refresh the browser when developing CSS/JavaScript. For a shared release, increase the version in DESCRIPTION and install/rebuild the apps with that version. Asset URLs use the package version automatically, so a release does not reuse an older stylesheet cache.

`app_navbar()`, `app_home_ui()`, `app_sidebar()` and `app_layout()` provide the common shell. All branding and home content are arguments. Use `app_expander()`, `app_dimension_controls()`, `app_numeric_input()`, `app_help_ui()` and `app_color_dropdown()` for controls. `app_plot_actions_ui()` supplies sidebar Reset controls; put `app_download_ui()` inside `app_result_header_ui()` at the top right of the corresponding plot/results panel. Downloads use a compact 32px icon with a descriptive title and accessible label; apps decide whether to show export dimensions. Existing app-owned availability rules still apply. `plot_theme()`, `app_colours()`, `content_plot_size()` and `save_plot_png()` provide the common figure contract: logical screen pixels at 96 dpi, exported at 300 dpi. App-specific help can register a resolver with `window.VetmeduniAppHelp.register(function(id, label, sharedReference) { ... })` or provide explicit `data-help` attributes.

Run package checks with `testthat::test_local(".")` from this package folder. `inst/examples/gallery/app.R` is a runnable gallery; `inst/templates/` is the module skeleton used by `create_app()`.

Use `app_busy_options()` in the document head for common loading indicators. `app_metric()` and `app_metric_strip()` display app-calculated summaries; `columns` optionally supplies CSS grid tracks. `app_data_section(title, ..., actions, id, icon)` wraps a data card. `app_table_options()` supplies bounded, accessible DT defaults (DT is optional); override search, column and paging policies with its `options` argument. It does not decide whether the table runs on the server.

`app_plot_style_ui()` accepts `colors`, `font` and `legend` UI slots; explicit `NULL` omits a slot. Use `app_optional_value()` only for lightweight optional metadata: Shiny silent validation becomes `NULL`, while unexpected errors propagate. `app_keep_outputs_current(output, ...)` keeps specified control outputs current inside closed expanders; do not use it for expensive models or plots. App-specific type checks remain local.

Use `mod_export_controls_ui(id)` in the sidebar and `mod_export_actions_ui(id)` in the result header with `mod_export_server()`. The combined `mod_export_ui()` and download arguments to `app_plot_actions_ui()` are deprecated compatibility routes. The starter/gallery demonstrate an `app_plot_snapshot()` shared by preview, dimensions and PNG export. Choose the appropriate `plot_theme(kind=...)` instead of overriding general axes or grids in app code.

Create an app with `VetmeduniAppTemplate::create_app("path/to/NewAnalyzer", name = "NewAnalyzer", main = "New", accent = "Analyzer")`. This creates `app.R`, the shared dependency loader, app configuration and separate Home UI/server modules under `R/`, plus launch instructions. The destination must be empty. Keep app code flat under `R/`, using `mod_<page>_ui.R` and `mod_<page>_main.R`; reusable presentation belongs in this package.

The following shared interface contract applies to consuming apps. Scientific controls and calculations remain app-specific.

Grouped dropdowns have a continuous white surface without spacer bands. Wrap short method selectors in `div(class="app-complete-choices", ...)` to show all options without an internal scrollbar; long data-driven lists retain bounded scrolling. Shared table toolbars keep search and page size together when space permits. Below 576px, pagination shows Previous, the current page and Next; the info text retains the full row count.

| Area | Shared contract |
|---|---|
| Theme and brand | Local system fonts; burgundy `#af2144` navigation and a green `#286e53` banner, white navigation text, white panels, black body text and burgundy actions and a light neutral workspace. Home retains a green (#286e53) banner with a burgundy (#af2144) start button, with white type, a pale green wordmark tint and the current institutional logo in white. Active navigation uses a compact white tab with dark burgundy text; hover adds only a subtle translucent tint. Dropdown carets remain separate from the selection indicator. Navbar logos are 26px square with 6px rounded corners. All app names, versions, artwork and copy are supplied as arguments. |
| Sidebar layout | 330px desktop; 290px at widths up to 1100px. Below 768px, always-open sidebars appear before results. Sidebar content padding is 14px. Dropdowns can extend beyond the panel; long option lists scroll within their own menu. Upload and Filter controls stay directly visible without expanders. |
| Compact controls | Standard inputs are 30px high with 13px type. Dropdown fields stay white and the same height when opened/focused; menus use white, compact 30px rows and scroll for long lists. Multi-select chip wrapping stays available. Disabled buttons use a solid pale-gray background, muted text and no active hover treatment. Paired dimensions have 16px gutters; categorical orders appear immediately below their selectors. |
| Upload metrics | Summary strips use black `#000000`, white values, pale labels and subtle light dividers; table panels stay white. |
| Home | Green banner, burgundy start button and the bundled Vetmeduni Austria logo in white, numbered workflow, then informational analysis cards. Workflow steps mirror each app's navbar. Cards contain an icon beside the title and a description below, without categories, links or buttons. |
| Expanders | Primary grouping/model sections start open; secondary appearance, size and export options start closed. Keep section order consistent and avoid nested expanders. Reset/download actions stay outside. Use a shared 12px gap from the final expander to the first Run or Reset button, and between consecutive sidebar action buttons (including dynamically rendered Upload actions). |
| Control lifecycle | Keep control panels mounted when switching methods; change their visibility and choices rather than replacing parents containing bound outputs. Unavailable optional metadata clears widgets normally; model and figure validation stays in the analysis. |
| Help | Help belongs to labels, with 24px hit areas and 16px outlined circles. Do not add automatic help icons to action buttons. Use explicit app text or a registered app resolver (return false to omit generic help for a control); opening help never changes an input. |
| Reset | Each app owns a local reset that also works before data are loaded. Reset that page's controls/results while preserving uploaded data, shared filters and independent analysis sections. |
| Figures | No plot titles or subtitles; keep axis/legend labels, facet/group strips and statistical footnotes. Use `plot_panel_label()` for single-panel subgroup labels instead of plot titles. Default base font size is 13pt; legends default to the right, including after Reset. Dimensions are logical screen pixels at 96dpi; PNG export uses the same renderer/layout scaled to 300dpi. Use content-based figure sizes and horizontal scrolling, with matching preview and export dimensions. |
| Categorical colors | `app_colours()` preserves the original first 20 colors and extends deterministically without a category-count cap. Apps own custom color state; identical custom colors are permitted. Legacy palette-size constants describe the base palette, not eligibility limits. |
| New apps | Use `create_app()` and the shared navbar/Home/sidebar/control functions. Put data validation, analysis, fitted models and scientific interpretation in the application. |

No open-source licence has been selected. The reserved `maintainer@example.invalid` address is syntactic placeholder metadata for private local builds; replace it with a real contact only if the package is published.

## Vetmeduni identity

Branding follows the supplied [color guide](inst/branding/farbschema_vetmeduni.pdf) and [Vetmeduni website](https://www.vetmeduni.ac.at/). See [branding sources](inst/branding/README.md) for exact HEX values and logo provenance. `inst/www/branding.css` owns the visual identity; `app_home_ui(institution = FALSE)` can omit the institutional logo. All assets are local. The new package name is `VetmeduniAppTemplate`; environment overrides are `VETMEDUNI_APP_TEMPLATE_MODE` and `VETMEDUNI_APP_TEMPLATE_PATH`. Reinstall with the app setup script after upgrading from the former package name.
