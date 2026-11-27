# ===============================================================
# Visualization Module - PCA (Biplot)
# ===============================================================

# Helper to detect categorical columns ----------------------------------------
.is_categorical <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x)
}

.pca_aesthetic_choices <- function(data, max_levels = NULL) {
  if (missing(data) || is.null(data) || !is.data.frame(data) || ncol(data) == 0) {
    return(c("None" = "None"))
  }

  keep <- vapply(data, .is_categorical, logical(1))
  cat_cols <- names(data)[keep]

  if (!is.null(max_levels) && length(cat_cols) > 0) {
    within_limit <- vapply(cat_cols, function(column_name) {
      column <- data[[column_name]]
      if (is.null(column)) {
        return(FALSE)
      }
      if (is.factor(column)) {
        levels_count <- length(levels(base::droplevels(column)))
      } else {
        unique_values <- unique(as.character(column[!is.na(column)]))
        levels_count <- length(unique_values)
      }
      levels_count <= max_levels
    }, logical(1))
    cat_cols <- cat_cols[within_limit]
  }

  if (length(cat_cols) == 0) {
    return(c("None" = "None"))
  }

  c("None" = "None", stats::setNames(cat_cols, cat_cols))
}

visualize_pca_ui <- function(id, filtered_data = NULL) {
  ns <- NS(id)
  all_choices <- .pca_aesthetic_choices(filtered_data)

  VetmeduniAppTemplate::app_layout(
    sidebar = VetmeduniAppTemplate::app_sidebar(
      title = "PCA",
      description = "Visualize multivariate structure using a PCA biplot.",
      id = ns("sidebar"),
      VetmeduniAppTemplate::app_expander("Analysis setup", open = TRUE, id = ns("grouping_section"),
      with_help_tooltip(
        selectInput(
          ns("plot_type"),
          label = "Select visualization type",
          choices = c("PCA biplot" = "biplot"),
          selected = "biplot"
        ),
        "Pick how the PCA results should be displayed."
      )
      ),
      VetmeduniAppTemplate::app_expander("Plot options", id = ns("options_section"),
      fluidRow(
        column(
          6,
          with_help_tooltip(
            uiOutput(ns("pca_color_ui")),
            "Colour the samples using a grouping variable to spot patterns. Variables with more than 10 categories are not allowed."
          )
        ),
        column(
          6,
          conditionalPanel(
            condition = sprintf("input['%s'] != 'None'", ns("pca_color")),
            with_help_tooltip(
              div(
                style = "padding-top: 24px;",
                checkboxInput(
                  ns("show_ellipses"),
                  label = "Show group ellipses",
                  value = FALSE
                )
              ),
              "Draw confidence ellipses around each colour group."
            )
          )
        )
      ),
      fluidRow(
        column(
          6,
          with_help_tooltip(
            selectInput(
              ns("pca_label"),
              label = "Label points by",
              choices = all_choices,
              selected = "None"
            ),
            "Add text labels from a column to identify each sample."
          )
        ),
        column(
          6,
          with_help_tooltip(
            numericInput(
              ns("pca_label_size"),
              label = "Label size",
              value = 2,
              min = 0.5,
              max = 6,
              step = 0.5
            ),
            "Control how large the text labels assigned to points appear on the plot."
          )
        )
      ),
      with_help_tooltip(
        uiOutput(ns("pca_shape_ui")),
        "Change the point shapes using a grouping variable for extra contrast. Variables with more than 10 categories are not allowed."
      ),
      with_help_tooltip(
        selectInput(
          ns("facet_var"),
          label = "Facet by variable",
          choices = all_choices,
          selected = "None"
        ),
        "Split the plot into small multiples based on a grouping variable."
      ),
      fluidRow(
        column(
          6,
          with_help_tooltip(
            div(
              style = "padding-top: 24px;",
              checkboxInput(
                ns("show_loadings"),
                label = "Show loadings",
                value = FALSE
              )
            ),
            "Display arrows that show how each original variable contributes to the components."
          )
        ),
        column(
          6,
          with_help_tooltip(
            numericInput(
              ns("loading_scale"),
              label = "Loading arrow scale",
              value = 1.2, min = 0.1, max = 5, step = 0.1
            ),
            "Stretch or shrink the loading arrows to make them easier to read."
          )
        )
      )
      ,
      uiOutput(ns("layout_controls"))
      ),
      subplot_size_ui(
        ns,
        width_value = 800,
        height_value = 600,
        width_label = "Plot width (px)",
        height_label = "Plot height (px)",
        width_help = "Set the width of the PCA plot in pixels.",
        height_help = "Set the height of the PCA plot in pixels."
      ),
      ta_plot_style_ui(ns, default = VetmeduniAppTemplate::PLOT_BASE_SIZE,
        help_text = "Adjust the base font size used for PCA plots."),
      
      VetmeduniAppTemplate::app_plot_actions_ui(ns)
    ),
    div(
      class = "ta-main-panel",
      VetmeduniAppTemplate::app_result_header_ui("Plots", VetmeduniAppTemplate::app_download_ui(ns)),
      uiOutput(ns("plot_container"))
    )
  )
}
