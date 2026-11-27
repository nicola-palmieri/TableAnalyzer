# Vetmeduni branding sources

`farbschema_vetmeduni.pdf` is the supplied institutional color reference, moved here from the workspace root without modification. Screen colors use the PDF's HEX values:

| Color | HEX | App use |
|---|---|---|
| Burgundy | `#af2144` | Primary actions, links, wordmark accent and focus |
| Green | `#286e53` | Supporting accents and success states |
| Copper | `#a86044` | Supporting accents and warnings |
| Gray | `#757575` | Supporting accents |
| Black / white | `#000000` / `#ffffff` | Logo, typography, surfaces and summary strips |

The PDF's copper RGB entry differs from its HEX entry; the apps consistently use the printed HEX value `#a86044`. Light neutral surfaces and interaction tints supplement these colors. Scientific category palettes and user-selected plot colors retain their existing behavior.

The [Vetmeduni website](https://www.vetmeduni.ac.at/), inspected on 2026-11-27, supplies the reference for white surfaces, black typography, simple borders and colored accents. Its current Vetmeduni Austria SVG wordmark is bundled as `inst/www/vetmeduni-logo.svg`; paths and proportions are preserved. Home retains a Vetmeduni green (#286e53) banner with a Vetmeduni burgundy (#af2144) start button; white logo/type and light tints of the institutional accents provide contrast. CSS displays the original black logo as white without changing its paths. The logo remains Vetmeduni's institutional identity, not part of an app's own icon or a replacement for it.

`inst/www/branding.css` applies this shared identity to both applications. The logo and all styling are served locally; opening the optional university link is the only action that visits the university website. Home's `institution = FALSE` option omits the institutional link/logo when using the component without it.

The white institutional logo sits at the banner top right. On mobile, a reserved top row prevents overlap with the app title.
