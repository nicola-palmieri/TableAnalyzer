# Table-only adapters preserve app metadata checks and input IDs.
optional_plot_metadata <- function(value, type = NULL) {
  metadata <- VetmeduniAppTemplate::app_optional_value(value)
  if (!is.null(metadata) && !is.null(type) && !identical(metadata$type, type)) return(NULL)
  metadata
}

keep_optional_ui_current <- VetmeduniAppTemplate::app_keep_outputs_current

ta_plot_style_ui <- function(ns, default = VetmeduniAppTemplate::PLOT_BASE_SIZE,
                             help_text = "Adjust the base font size used for plot text.",
                             legend = NULL) {
  VetmeduniAppTemplate::app_plot_style_ui(
    ns, base_size = default,
    colors = add_color_customization_ui(ns),
    font = base_size_ui(ns, default = default, help_text = help_text),
    legend = legend
  )
}
