# ===============================================================
# 🟦 Descriptive Visualization — Categorical Barplots
# ===============================================================

visualize_categorical_barplots_ui <- function(id) {
  ns <- NS(id)
  tagList(
    NicoShinyAppTemplate::app_expander("Plot options", id = ns("options_section"),
    with_help_tooltip(
      checkboxInput(ns("show_proportions"), "Show proportions instead of counts", FALSE),
      "Switch between raw counts and percentages for each category."
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
      width_help = "Set the width of each categorical plot in pixels.",
      height_help = "Set the height of each categorical plot in pixels."
    ),
    ta_plot_style_ui(ns, default = NicoShinyAppTemplate::PLOT_BASE_SIZE,
        help_text = "Adjust the base font size used for barplot text elements.",
        legend = uiOutput(ns("common_legend_controls"))),
    
    NicoShinyAppTemplate::app_plot_actions_ui(ns)
  )
}


visualize_categorical_barplots_plot_ui <- function(id) {
  ns <- NS(id)
  tagList(NicoShinyAppTemplate::app_result_header_ui("Plots",
    NicoShinyAppTemplate::app_download_ui(ns)), uiOutput(ns("plot_container")))
}
