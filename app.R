# ===============================================================
# 🧪 Table Analyzer — Standalone App
# ===============================================================

library(bslib)
library(car)
library(dplyr)
library(DT)
library(emmeans)
library(fitdistrplus)
library(flextable)
library(GGally)
library(ggplot2)
library(ggrepel)
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
  
  # ---- Custom CSS ----
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "theme-table.css"),
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
  visualize_server("visualize", filtered, analyzed$results, analyzed$selection)

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
