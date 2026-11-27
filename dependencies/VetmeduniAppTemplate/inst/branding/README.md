# Vetmeduni branding sources

`farbschema_vetmeduni.pdf` is the supplied institutional color reference, moved here from the workspace root without modification. Screen colors use the PDF's HEX values:

| Color | HEX | App use |
|---|---|---|
| Burgundy | `#af2144` | Primary actions, links, wordmark accent and focus |
| Green | `#286e53` | Supporting accents and success states |
| Copper | `#a86044` | Supporting accents and warnings |
| Gray | `#757575` | Supporting accents |
| Black / white | `#000000` / `#ffffff` | Logo, typography and surfaces |

The PDF's copper RGB entry differs from its HEX entry; the apps consistently use the printed HEX value `#a86044`. Light neutral surfaces and interaction tints supplement these colors. Scientific category palettes and user-selected plot colors retain their existing behavior.

The [Vetmeduni website](https://www.vetmeduni.ac.at/), inspected on 2026-11-27, supplies the reference for white surfaces, black typography, simple borders and colored accents. Its current Vetmeduni Austria SVG wordmark is bundled as `inst/www/vetmeduni-logo.svg`; paths and proportions are preserved. Home retains a Vetmeduni green (#286e53) banner with a Vetmeduni burgundy (#af2144) start button; white logo/type and light tints of the institutional accents provide contrast. CSS displays the original black logo as white without changing its paths. The logo remains Vetmeduni's institutional identity, not part of an app's own icon or a replacement for it.

`inst/www/branding.css` applies this shared identity to both applications. The logo and all styling are served locally; opening the optional university link is the only action that visits the university website. The navbar's `institution = FALSE` option omits the institutional link/logo.

The white institutional logo sits at the navbar far right. On mobile, the logo stays in the navbar beside the menu toggle; the banner contains no institutional logo.

Semantic accents are green for success/running, copper for warnings, burgundy for errors and gray for information. `R/semantic_colors.R` owns the accents plus dark text, pale backgrounds and borders. `inst/www/semantic-colors.css` consumes those definitions. The approved normal-state component colors are retained; focused controls receive an opaque outline. Scientific palettes and selectable plot colors are excluded from institutional recoloring.

## Cross-application review (2026-11-27)

Both applications share identical rendered navbar, Home banner, wordmark, workspace and primary-button colors. Shared feedback states and notifications meet normal-text contrast requirements. The browser audit sampled normal interface text across 24 MicrobiomeAnalyzer and 72 TableAnalyzer page/viewport cases (1440, 768 and 390 px), with no contrast failures or page overflow. TableAnalyzer coverage includes all seven analyses and all available visualization choices; LM/LMM diagnostics are checked within Analyze and their existing Visualize placeholders remain unchanged.

Corrections: TableAnalyzer now uses an SVG icon/favicon preserving its original embedded illustration in institutional green tones; contradictory guide copy was aligned. Shared modal focus now matches sidebar focus, low-contrast explanatory hints were darkened, and duplicate semantic CSS rules were consolidated. SVG plot colors, selectable swatches, blue/navy scientific plot styling and unused developer preview artwork were deliberately preserved. No scientific calculation or plot-builder source was modified.

Representative screenshots live in each app's `docs/assets/`. Each app's `scripts/review_colors.R` creates full browser evidence using the development-only `inst/review/colors.js` helper. Package contrast tests, bundled installation checks and source/installed integration checks cover the common dependency.
