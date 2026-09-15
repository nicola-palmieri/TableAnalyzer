# ===============================================================
# 🧪 Table Analyzer — Standalone App
# ===============================================================

library(bslib)
library(bsicons)
library(car)
library(dplyr)
library(DT)
library(emmeans)
library(fitdistrplus)
library(flextable)
library(GGally)
library(ggplot2)
library(ggrepel)
library(janitor)
library(lmerTest)
library(officer)
library(patchwork)
library(readxl)
library(shiny)
library(skimr)
library(tidyr)
library(zoo)

options(shiny.autoreload = interactive())
options(shiny.maxRequestSize = 200 * 1024^2)

for (f in list.files("R", full.names = TRUE, pattern = "\\.R$")) source(f)

# ---------------------------------------------------------------
# UI
# ---------------------------------------------------------------
ui <- navbarPage(
  title = tagList(
    tags$img(
      src = "logo.jpeg",
      class = "ta-navbar-logo",
      alt = "Table Analyzer logo"
    ),
    "Table Analyzer"
  ),
  id = "main_nav",
  theme = bs_theme(bootswatch = "flatly"),
  
  # ---- Custom CSS ----
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "theme-table.css"),
    tags$script(HTML("
      const tabManager = function(tab, disable) {
        var selector = 'a[data-value=\"' + tab + '\"]';
        var $tab = $(selector);
        var $li = $tab.closest('li');
        if (disable) {
          $tab.addClass('disabled');
          $tab.attr('aria-disabled', 'true');
          $li.addClass('disabled');
        } else {
          $tab.removeClass('disabled');
          $tab.attr('aria-disabled', 'false');
          $li.removeClass('disabled');
        }
      };

      // Disable downstream tabs immediately on load to avoid flicker before Shiny initializes.
      document.addEventListener('DOMContentLoaded', function() {
        ['filter_view', 'analysis_tab', 'results_tab'].forEach(function(tab) {
          tabManager(tab, true);
        });
      });

      Shiny.addCustomMessageHandler('toggleTabState', function(data) {
        tabManager(data.tab, data.disable);
      });

      Shiny.addCustomMessageHandler('configureDemoAnalysis', function(data) {
        var setSelectValue = function(id, value) {
          var element = document.getElementById(id);
          if (!element) return false;
          if (element.selectize) {
            element.selectize.setValue(value);
          } else {
            $(element).val(value).trigger('change');
          }
          return true;
        };

        setSelectValue('analysis-analysis_type', 'One-way ANOVA');

        var attempts = 0;
        var configure = function() {
          attempts += 1;
          var multi = document.getElementById('analysis-anova1-response-multi_resp');
          var group = document.getElementById('analysis-anova1-group');
          if (!multi || !group) {
            if (attempts < 200) return setTimeout(configure, 50);
            Shiny.setInputValue('demo_configuration_failed', Date.now(), {priority: 'event'});
            return;
          }

          setSelectValue('analysis-anova1-group', 'treatment');
          setSelectValue('analysis-anova1-order', ['Control', 'DrugA', 'DrugB']);
          if (!multi.checked) multi.click();

          var configureResponses = function() {
            attempts += 1;
            var response = document.getElementById('analysis-anova1-response-response');
            if (!response || !response.multiple) {
              if (attempts < 200) return setTimeout(configureResponses, 50);
              Shiny.setInputValue('demo_configuration_failed', Date.now(), {priority: 'event'});
              return;
            }

            setSelectValue('analysis-anova1-response-response', data.responses);
            Shiny.setInputValue('demo_run_started', Date.now(), {priority: 'event'});
            document.getElementById('analysis-anova1-run').click();
          };
          configureResponses();
        };
        configure();
      });

      $(document).on('click', 'a.nav-link.disabled', function(e) {
        e.preventDefault();
        return false;
      });

      $(document).on(
        'click',
        '.tab-pane[data-value=\"plots_view\"] a.shiny-download-link',
        function() {
          Shiny.setInputValue(
            'anonymous_usage_event',
            { event: 'plot_downloaded', nonce: Date.now() },
            { priority: 'event' }
          );
        }
      );
    "))
  ),

  tabPanel(
    title = tagList(icon("home"), " Home"),
    value = "home_tab",
    home_ui("home")
  ),

  tabPanel(
    title = tagList(icon("table-list"), " Data"),
    value = "data_tab",
    fluidPage(
      tabsetPanel(
        id = "data_views",
        tabPanel(
          title = tagList(icon("upload"), " Upload"),
          value = "upload_view",
          upload_ui("upload")
        ),
        tabPanel(
          title = tagList(icon("filter"), " Filter"),
          value = "filter_view",
          filter_ui("filter")
        )
      )
    )
  ),
  tabPanel(
    title = tagList(icon("square-poll-horizontal"), " Analyze"),
    value = "analysis_tab",
    fluidPage(analysis_ui("analysis"))
  ),
  tabPanel(
    title = tagList(icon("file-lines"), " Results"),
    value = "results_tab",
    fluidPage(
      tabsetPanel(
        id = "result_views",
        tabPanel(
          title = tagList(icon("table"), " Results"),
          value = "results_view",
          analysis_results_ui("analysis")
        ),
        tabPanel(
          title = tagList(icon("chart-area"), " Plots"),
          value = "plots_view",
          visualize_ui("visualize")
        )
      )
    )
  ),
)

# ---------------------------------------------------------------
# SERVER
# ---------------------------------------------------------------
server <- function(input, output, session) {
  telemetry_setting <- tolower(Sys.getenv("TABLEANALYZER_TELEMETRY", "true"))
  track_event <- new_usage_tracker(
    enabled = !telemetry_setting %in% c("0", "false", "off", "no")
  )
  track_event("session_started")

  demo_requested <- home_server("home")
  uploaded  <- upload_server("upload", track_event = track_event)
  filtered  <- filter_server("filter", uploaded)
  analyzed  <- analysis_server("analysis", filtered, track_event = track_event)
  visualize_server("visualize", filtered, analyzed$results, analyzed$selection)

  observeEvent(input$anonymous_usage_event, {
    event <- input$anonymous_usage_event$event
    if (identical(event, "plot_downloaded")) track_event(event)
  })

  demo_stage <- reactiveVal("idle")
  demo_responses <- c("cortisol", "glucose", "heart_rate")

  configure_demo_analysis <- function() {
    demo_stage("configuring")
    session$onFlushed(function() {
      session$sendCustomMessage(
        "configureDemoAnalysis",
        list(responses = demo_responses)
      )
    }, once = TRUE)
  }

  observeEvent(demo_requested(), {
    req(demo_requested() > 0)
    showNotification("Loading the guided example...", type = "message", duration = 3)
    updateNavbarPage(session, "main_nav", selected = "data_tab")
    updateTabsetPanel(session, "data_views", selected = "upload_view")

    if (identical(input[["upload-data_source"]], "example")) {
      demo_stage("resetting")
      updateRadioButtons(session, "upload-data_source", selected = "long")
    } else {
      demo_stage("loading")
      updateRadioButtons(session, "upload-data_source", selected = "example")
    }
  }, ignoreInit = TRUE)

  observeEvent(input[["upload-data_source"]], {
    if (identical(demo_stage(), "resetting") &&
        identical(input[["upload-data_source"]], "long")) {
      demo_stage("loading")
      updateRadioButtons(session, "upload-data_source", selected = "example")
    }
  }, ignoreInit = TRUE)

  observeEvent(uploaded(), {
    if (!identical(demo_stage(), "loading") || is.null(uploaded())) return()
    updateNavbarPage(session, "main_nav", selected = "analysis_tab")
    configure_demo_analysis()
  })

  observeEvent(input$demo_run_started, {
    demo_stage("running")
  })

  observeEvent(input$demo_configuration_failed, {
    demo_stage("idle")
    showNotification(
      "The demo could not configure the analysis. Please try again.",
      type = "error",
      duration = 6
    )
  })

  observe({
    has_data <- !is.null(uploaded())
    has_results <- isTRUE(analyzed$has_results())

    lapply(c("filter_view", "analysis_tab"), function(tab) {
      session$sendCustomMessage(
        "toggleTabState",
        list(tab = tab, disable = !has_data)
      )
    })
    session$sendCustomMessage(
      "toggleTabState",
      list(tab = "results_tab", disable = !has_results)
    )

    if (!has_data && input$main_nav %in% c("analysis_tab", "results_tab")) {
      updateNavbarPage(session, "main_nav", selected = "data_tab")
      updateTabsetPanel(session, "data_views", selected = "upload_view")
    } else if (has_data && !has_results && identical(input$main_nav, "results_tab")) {
      updateNavbarPage(session, "main_nav", selected = "analysis_tab")
    }
  })

  observeEvent(analyzed$has_results(), {
    if (!isTRUE(analyzed$has_results())) return()
    demo_ready <- identical(demo_stage(), "running")
    updateNavbarPage(session, "main_nav", selected = "results_tab")
    updateTabsetPanel(
      session,
      "result_views",
      selected = if (demo_ready) "plots_view" else "results_view"
    )
    if (demo_ready) {
      demo_stage("complete")
      showNotification(
        "Demo ready: three outcomes compared across treatment groups.",
        type = "message",
        duration = 5
      )
    }
  }, ignoreInit = TRUE)
}

# ---------------------------------------------------------------
# LAUNCH
# ---------------------------------------------------------------
shinyApp(ui, server)
