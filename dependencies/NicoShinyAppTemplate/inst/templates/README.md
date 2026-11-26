# New analysis application

Start from R with `shiny::runApp("path/to/this/directory")`, or open this directory and run `shiny::runApp()`.

Install NicoShinyAppTemplate first. Within the shared workspace the loader finds `dependencies/load_NicoShinyAppTemplate.R` and uses the development checkout; elsewhere it uses the installed package.

```r
install.packages("remotes")
remotes::install_github("nicola-palmieri/NicoShinyAppTemplate", upgrade = "never")
```

Private repository access requires authentication configured for remotes. Pin a
tested commit or release when distributing your app; running the installed app
does not contact GitHub.

Edit `R/shared_app_config.R` for identity and version. Home copy and navigation live in `R/mod_home_ui.R` and `R/mod_home_main.R`. Add analysis pages under `R/mod_<page>_ui.R` and `R/mod_<page>_main.R`; scientific functions stay in their owning application. Shared CSS, controls and figure presentation come from NicoShinyAppTemplate.
