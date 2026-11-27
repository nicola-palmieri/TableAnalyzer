# ===============================================================
# 🧪 Visualization Module — One-way ANOVA (automatic appearance updates)
# ===============================================================

visualize_oneway_ui <- function(id) {
  ns <- NS(id)
  
  VetmeduniAppTemplate::app_layout(
    sidebar = VetmeduniAppTemplate::app_sidebar(
      title = "One-way ANOVA",
      description = "Select visualization type and adjust subplot layout, axis scaling, and figure size.",
      id = ns("sidebar"),
      VetmeduniAppTemplate::app_expander("Analysis setup", open = TRUE, id = ns("grouping_section"),
      
      selectInput(
        ns("plot_type"),
        label = "Select visualization type",
        choices = c(
          "Line plots (mean and SE)" = "lineplot_mean_se",
          "Bar plots (mean and SE)"  = "barplot_mean_se",
          "Boxplots"              = "boxplot"
        ),
        selected = "lineplot_mean_se"
      )
      ),
      VetmeduniAppTemplate::app_expander("Plot options", id = ns("options_section"),
      
      conditionalPanel(
        condition = sprintf("input['%s'] === 'lineplot_mean_se'", ns("plot_type")),
        fluidRow(
          column(6, checkboxInput(ns("lineplot_show_lines"),  "Connect means with lines", value = TRUE)),
          column(6, checkboxInput(ns("lineplot_show_jitter"), "Overlay jittered data",  value = FALSE))
        )
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] === 'boxplot'", ns("plot_type")),
        with_help_tooltip(
          checkboxInput(ns("boxplot_show_jitter"), "Overlay jittered data", value = FALSE),
          "Overlay raw observations with horizontal jitter."
        )
      ),

      checkboxInput(
        ns("share_y_axis"),
        "Use common y-axis across plots",
        value = FALSE
      )
      ,
      uiOutput(ns("layout_controls"))
      ),
      subplot_size_ui(ns),
      
      ta_plot_style_ui(ns, default = VetmeduniAppTemplate::PLOT_BASE_SIZE),
      
      
      VetmeduniAppTemplate::app_plot_actions_ui(ns)
    ),
    
    div(
      class = "ta-main-panel",
      VetmeduniAppTemplate::app_result_header_ui("Plots", VetmeduniAppTemplate::app_download_ui(ns)),
      uiOutput(ns("plot_container"))
    )
  )
}
