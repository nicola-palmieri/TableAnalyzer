# ===============================================================
# 🎨 Module for colors customization
# ===============================================================

# ---- Palette ----
basic_color_palette <- VetmeduniAppTemplate::barplot_color_palette

# ---- UI ----
add_color_customization_ui <- function(ns) {
  uiOutput(ns("color_custom_ui"))
}

# ---- Server ----
add_color_customization_server <- function(ns,
                                           input,
                                           output,
                                           data,
                                           color_var_reactive,
                                           multi_group = TRUE,
                                           level_order_reactive = NULL,
                                           reset_token = NULL) {
  default_color <- unname(basic_color_palette[[1]])
  # ---- Dynamic UI ----
  output$color_custom_ui <- renderUI({
    # Missing data/model metadata is an ordinary navigation or upload-clear
    # state for optional controls. Technical errors still propagate normally.
    dataset <- optional_plot_metadata(data())
    if (is.null(dataset)) return(NULL)

    color_var <- optional_plot_metadata(color_var_reactive()) %||% ""
    level_override <- if (is.null(level_order_reactive)) NULL else optional_plot_metadata(level_order_reactive())
    reset_id <- if (is.null(reset_token)) NULL else resolve_reactive(reset_token)
    id_suffix <- if (is.null(reset_id)) "" else paste0("_", reset_id)

    # Single color UI shown when multi-group off or no color_var available
    if (!isTRUE(multi_group) || color_var %in% c("", "None")) {
      single_id <- paste0("single_color", id_suffix)
      single_selected <- isolate(input[[single_id]]) %||% default_color
      tagList(
        with_help_tooltip(tagList(
          tags$label(`for` = ns(paste0(single_id, "_button")), class = "control-label", "Plot color"),
          color_dropdown_input(
            ns,
            single_id,
            basic_color_palette,
            ncol = 4,
            selected = single_selected
          )),
          "Choose the colour used for the entire plot."
        )
      )
    } else {
      render_color_inputs(ns, function() dataset, color_var, level_override, input, id_suffix = id_suffix)
    }
  })
  keep_optional_ui_current(output, "color_custom_ui")

  # ---- Reactive color mapping ----
  reactive({
    reset_id <- if (is.null(reset_token)) NULL else resolve_reactive(reset_token)
    id_suffix <- if (is.null(reset_id)) "" else paste0("_", reset_id)

    # --- Single-color mode (no grouping variable or disabled) ---
    if (!isTRUE(multi_group)) {
      return(input[[paste0("single_color", id_suffix)]] %||% default_color)
    }

    color_var <- color_var_reactive() %||% ""
    if (color_var %in% c("", "None")) {
      return(input[[paste0("single_color", id_suffix)]] %||% default_color)
    }

    dataset <- data()
    req(dataset)

    if (!color_var %in% names(dataset)) {
      return(input[[paste0("single_color", id_suffix)]] %||% default_color)
    }

    values <- dataset[[color_var]]
    as_factor <- as.factor(values)
    lvls <- levels(as_factor)
    unique_vals <- unique(as.character(as_factor))
    unique_vals <- unique_vals[!is.na(unique_vals)]

    level_override <- if (is.null(level_order_reactive)) NULL else level_order_reactive()
    if (!is.null(level_override) && length(level_override) > 0) {
      level_override <- unique(as.character(level_override[!is.na(level_override)]))
      remainder <- setdiff(unique_vals, level_override)
      lvls <- c(level_override, remainder)
    } else if (length(lvls) == 0) {
      lvls <- unique_vals
    }

    base_palette <- resolve_palette_for_levels(lvls)

    cols <- vapply(seq_along(lvls), function(i) {
      input[[paste0("col_", color_var, "_", i, id_suffix)]] %||% base_palette[i]
    }, character(1))

    names(cols) <- lvls
    cols
  })
}

# ===============================================================
# 🎨 UI helper to assign colors per level of a factor
# ===============================================================

render_color_inputs <- function(ns, data, color_var, level_order = NULL, input = NULL, id_suffix = "") {
  if (is.null(color_var) || color_var == "None") return(NULL)
  if (!color_var %in% names(data())) return(NULL)

  values <- data()[[color_var]]
  raw_levels <- if (is.factor(values)) levels(values) else unique(as.character(values))
  raw_levels <- raw_levels[!is.na(raw_levels)]

  if (!is.null(level_order) && length(level_order) > 0) {
    level_order <- unique(as.character(level_order[!is.na(level_order)]))
    remainder <- setdiff(raw_levels, level_order)
    lvls <- c(level_order, remainder)
  } else {
    lvls <- raw_levels
  }

  default_palette <- resolve_palette_for_levels(lvls)

  tagList(
    lapply(seq_along(lvls), function(i) {
      selected <- default_palette[i]
      if (!is.null(input)) {
        selected <- isolate(input[[paste0("col_", color_var, "_", i, id_suffix)]]) %||% default_palette[i]
      }
      tags$div(
        style = "margin-bottom: 8px;",
        with_help_tooltip(tagList(
          tags$label(`for` = ns(paste0("col_", color_var, "_", i, id_suffix, "_button")),
            class = "control-label", lvls[i]),
          color_dropdown_input(
            ns,
            id = paste0("col_", color_var, "_", i, id_suffix),
            palette = basic_color_palette,
            ncol = 4,
            selected = selected
          )),
          sprintf("Pick the colour that will represent %s in the plot.", lvls[i])
        )
      )
    })
  )
}

# ===============================================================
# 🎨 Compact dropdown-style color picker (4x4 grid)
# ===============================================================

color_dropdown_input <- function(ns, id = "color_choice", palette = basic_color_palette,
                                 ncol = 4, selected = NULL) {
  VetmeduniAppTemplate::app_color_dropdown(
    ns, id = id, palette = palette, ncol = ncol, selected = selected
  )
}

with_help_tooltip <- function(widget, text) {
  VetmeduniAppTemplate::app_help_ui(widget, text)
}

resolve_palette_for_levels <- function(levels, custom = NULL) {
  if (is.null(levels) || length(levels) == 0) {
    return(resolve_single_color())
  }

  unique_levels <- unique(as.character(levels))
  n_levels <- length(unique_levels)

  if (!is.null(custom) && length(custom) > 0) {
    if (!is.null(names(custom))) {
      ordered <- custom[unique_levels]
      if (all(!is.na(ordered))) {
        return(ordered)
      }
    } else if (length(custom) >= n_levels) {
      return(stats::setNames(custom[seq_len(n_levels)], unique_levels))
    }
  }

  # The twin apps share their categorical defaults. Table retains cycling for
  # analyses with more than 20 groups; the picker remains the named 16 colors.
  palette <- rep(VetmeduniAppTemplate::APP_PALETTE, length.out = n_levels)

  stats::setNames(palette, unique_levels)
}

resolve_single_color <- function(custom = NULL) {
  if (!is.null(custom) && length(custom) > 0) {
    candidate <- unname(custom[[1]])
    if (!is.null(candidate) && nzchar(candidate)) {
      return(candidate)
    }
  }
  basic_color_palette[1]
}
