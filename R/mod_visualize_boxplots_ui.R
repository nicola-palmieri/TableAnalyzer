# ===============================================================
# 🟦 Descriptive Visualization — Numeric Boxplots
# ===============================================================

visualize_numeric_boxplots_ui <- function(id) {
  ns <- NS(id)
  tagList(
    VetmeduniAppTemplate::app_expander("Plot options", id = ns("options_section"),
    with_help_tooltip(
      checkboxInput(ns("show_points"), "Overlay jittered data", FALSE),
      "Overlay raw observations with horizontal jitter."
    ),
    with_help_tooltip(
      checkboxInput(ns("show_outliers"), "Highlight boxplot outliers", FALSE),
      "Highlight points that fall outside the typical range."
    ),
    
    conditionalPanel(
      condition = sprintf("input['%s']", ns("show_outliers")),
      uiOutput(ns("outlier_label_ui"))
    )
    ,
      plot_grid_ui(
      id = ns("plot_grid"),
      rows_help = "Choose how many rows of plots to display.",
      cols_help = "Choose how many columns of plots to display."
    )
      ),
    subplot_size_ui(
      ns,
      width_value = 400,
      height_value = 300,
      width_help = "Set the width of each boxplot panel in pixels.",
      height_help = "Set the height of each boxplot panel in pixels."
    ),
    
    
    ta_plot_style_ui(ns, default = VetmeduniAppTemplate::PLOT_BASE_SIZE,
        help_text = "Adjust the base font size used for boxplot text.",
        legend = uiOutput(ns("common_legend_controls"))),
    
    VetmeduniAppTemplate::app_plot_actions_ui(ns)
  )
}

visualize_numeric_boxplots_plot_ui <- function(id) {
  ns <- NS(id)
  tagList(VetmeduniAppTemplate::app_result_header_ui("Plots",
    VetmeduniAppTemplate::app_download_ui(ns)), uiOutput(ns("plot_container")))
}
