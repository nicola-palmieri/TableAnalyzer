# ===============================================================
# 🧪 Visualization Module — Two-way ANOVA (automatic appearance updates)
# ===============================================================

visualize_twoway_ui <- function(id) {
  ns <- NS(id)
  
  VetmeduniAppTemplate::app_layout(
    sidebar = VetmeduniAppTemplate::app_sidebar(
      title = "Two-way ANOVA",
      description = "Select visualization type and adjust subplot layout, axis scaling, and figure size.",
      id = ns("sidebar"),
      VetmeduniAppTemplate::app_expander("Analysis setup", open = TRUE, id = ns("grouping_section"),
      
      with_help_tooltip(
        selectInput(
          ns("plot_type"),
          label = "Select visualization type",
          choices = c(
            "Line plots (mean and SE)" = "lineplot_mean_se",
            "Bar plots (mean and SE)"  = "barplot_mean_se",
            "Boxplots"              = "boxplot"
          ),
          selected = "lineplot_mean_se"
        ),
        "Pick the chart style you prefer for viewing group means and uncertainty."
      )
      ),
      VetmeduniAppTemplate::app_expander("Plot options", id = ns("options_section"),
      
      conditionalPanel(
        condition = sprintf("input['%s'] === 'lineplot_mean_se'", ns("plot_type")),
        fluidRow(
          column(
            6,
            with_help_tooltip(
              checkboxInput(
                ns("lineplot_show_lines"),
                "Connect means with lines",
                value = TRUE
              ),
              "Draw connecting lines between group means."
            )
          ),
          column(
            6,
            with_help_tooltip(
              checkboxInput(
                ns("lineplot_use_dodge"),
                "Dodge grouped means",
                value = FALSE
              ),
              "Offset the level means of the second factor along the x-axis."
            )
          )
        )
      ),
      
      uiOutput(ns("axis_and_jitter"))
      ,
      uiOutput(ns("layout_controls"))
      ),
      subplot_size_ui(ns),
      
      ta_plot_style_ui(ns, default = VetmeduniAppTemplate::PLOT_BASE_SIZE,
        help_text = "Adjust the base font size used for the ANOVA plots.",
        legend = uiOutput(ns("common_legend_controls"))),
      
      
      VetmeduniAppTemplate::app_plot_actions_ui(ns)
    ),
    
    div(
      class = "ta-main-panel",
      VetmeduniAppTemplate::app_result_header_ui("Plots", VetmeduniAppTemplate::app_download_ui(ns)),
      uiOutput(ns("plot_container"))
    )
  )
}
