# ===============================================================
# 🧪 Table Analyzer — Standalone App
# ===============================================================

library(bslib)
library(dplyr)
library(DT)
library(emmeans)
library(fitdistrplus)
library(flextable)
library(GGally)
library(ggplot2)
library(lmerTest)
library(officer)
library(patchwork)
library(readxl)
library(shiny)
library(skimr)
library(tidyr)
library(zoo)

options(shiny.autoreload = TRUE)
options(shiny.maxRequestSize = 200 * 1024^2)

for (f in list.files("R", full.names = TRUE, pattern = "\\.R$")) source(f)

# ---------------------------------------------------------------
# UI
# ---------------------------------------------------------------
ui <- navbarPage(
  title = tagList(icon("table"), "Table Analyzer"),
  id = "main_nav",
  theme = bs_theme(bootswatch = "flatly"),
  
  # ---- Custom CSS (copied from website) ----
  header = tags$head(
    tags$style(HTML("
      .container-fluid { max-width: 100%; margin: auto; }
      .hero {
        background: #ecf0f1;
        border: 1px solid #dce4ec;
        border-radius: 16px;
        padding: 40px 24px;
        margin-top: 20px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.05);
      }
      h1, h2, h3 { margin-top: 0.4rem; }
      .section { margin-top: 18px; }
      .card { border-radius: 16px; box-shadow: 0 4px 10px rgba(0,0,0,0.05); }
      .nav-tabs > li > a { font-weight: 500; }
      .empty-state { max-width: 420px; margin-left: auto; margin-right: auto; }
      .empty-state-icon { font-size: 3rem; line-height: 1; }
      .empty-state h4 { font-weight: 600; }
      .ta-help-tooltip { cursor: help; display: inline-block; width: 100%; }
      .home-wrapper {
        min-height: calc(100vh - 180px);
        display: flex;
        align-items: center;
        justify-content: center;
      }
      .home-wrapper .hero {
        max-width: 760px;
        width: 100%;
      }
      a.nav-link.disabled {
        pointer-events: none;
        color: #6c757d !important;
        opacity: 0.7;
      }
      .home-steps h5 {
        font-weight: 600;
      }
      .home-steps p {
        margin-bottom: 0;
        color: #6c757d;
      }
      pre.shiny-text-output {
        white-space: pre;
        overflow-x: auto;
        font-family: Fira Mono, Source Code Pro, Monaco, monospace;
        font-size: 0.9rem;
      }
    ")),
    tags$script(HTML("
      const tabManager = function(tab, disable) {
        var selector = 'a[data-value=\"' + tab + '\"]';
        var $tab = $(selector);
        if (disable) {
          $tab.addClass('disabled');
          $tab.attr('aria-disabled', 'true');
        } else {
          $tab.removeClass('disabled');
          $tab.attr('aria-disabled', 'false');
        }
      };

      // Disable downstream tabs immediately on load to avoid flicker before Shiny initializes.
      document.addEventListener('DOMContentLoaded', function() {
        ['filter_tab', 'analysis_tab', 'visualize_tab'].forEach(function(tab) {
          tabManager(tab, true);
        });
      });

      Shiny.addCustomMessageHandler('toggleTabState', function(data) {
        tabManager(data.tab, data.disable);
      });

      $(document).on('click', 'a.nav-link.disabled', function(e) {
        e.preventDefault();
        return false;
      });
    "))
  ),

  tabPanel(
    title = tagList(icon("home"), " Home"),
    value = "home_tab",
    home_ui("home")
  ),

  tabPanel(
    title = tagList(icon("upload"), " Upload"),
    value = "upload_tab",
    fluidPage(upload_ui("upload"))
  ),
  tabPanel(
    title = tagList(icon("filter"), " Filter"),
    value = "filter_tab",
    fluidPage(filter_ui("filter"))
  ),
  tabPanel(
    title = tagList(icon("square-poll-horizontal"), " Analyze"),
    value = "analysis_tab",
    fluidPage(analysis_ui("analysis"))
  ),
  tabPanel(
    title = tagList(icon("chart-area"), " Visualize"),
    value = "visualize_tab",
    fluidPage(visualize_ui("visualize"))
  ),
)

# ---------------------------------------------------------------
# SERVER
# ---------------------------------------------------------------
server <- function(input, output, session) {
  home_server("home")
  uploaded  <- upload_server("upload")
  filtered  <- filter_server("filter", uploaded)
  analyzed  <- analysis_server("analysis", filtered)
  visualize_server("visualize", filtered, analyzed)

  observe({
    has_data <- !is.null(uploaded())
    tabs <- c("filter_tab", "analysis_tab", "visualize_tab")

    lapply(tabs, function(tab) {
      session$sendCustomMessage(
        "toggleTabState",
        list(tab = tab, disable = !has_data)
      )
    })

    if (!has_data && input$main_nav %in% tabs) {
      updateNavbarPage(session, "main_nav", selected = "upload_tab")
    }
  })
}

# ---------------------------------------------------------------
# LAUNCH
# ---------------------------------------------------------------
shinyApp(ui, server)
