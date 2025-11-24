# ===============================================================
# 🟦 Descriptive Visualization — Categorical Barplots
# ===============================================================

visualize_categorical_barplots_ui <- function(id) {
  ns <- NS(id)
  tagList(
    with_help_tooltip(
      checkboxInput(ns("show_proportions"), "Show proportions instead of counts", FALSE),
      "Switch between raw counts and percentages for each category."
    ),
    subplot_size_ui(
      ns,
      width_help = "Set the width of each categorical plot in pixels.",
      height_help = "Set the height of each categorical plot in pixels."
    ),
    plot_grid_ui(
      id = ns("plot_grid"),
      rows_help = "Choose how many rows of plots to display.",
      cols_help = "Choose how many columns of plots to display."
    ),
    fluidRow(
      column(6, add_color_customization_ui(ns, multi_group = TRUE)),
      column(6, tagList(
        base_size_ui(
          ns,
          default = 13,
          help_text = "Adjust the base font size used for barplot text elements."
        ),
        br(),
        uiOutput(ns("common_legend_controls"))
      ))
    ),
    br(),
    fluidRow(
      column(
        6,
        actionButton(
          ns("apply_plot"), 
          "Apply changes",
          width = "100%"
        )
      ),
      column(
        6,
        downloadButton(
          ns("download_plot"),
          "Download plot",
          style = "width: 100%;"
        )
      )
    )
  )
}


visualize_categorical_barplots_plot_ui <- function(id) {
  ns <- NS(id)
  div(
    uiOutput(ns("grid_warning")),
    plotOutput(ns("plot"), width = "100%", height = "auto")
  )
}


visualize_categorical_barplots_server <- function(id, filtered_data, summary_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    stored <- reactiveValues(
      plot = NULL,
      warning = NULL,
      layout = NULL,
      plot_width = 400,
      plot_height = 300
    )
    
    df <- reactive(filtered_data())
    
    grid <- plot_grid_server("plot_grid", cols_max = 100L)
    base_size <- base_size_server(input, default = 13)
    
    color_var <- reactive({
      info <- summary_info()
      dat <- df()
      g <- resolve_reactive(info$group_var)
      if (is.null(info) || is.null(dat)) return(NULL)
      if (is.null(g) || !g %in% names(dat)) return(NULL)
      g
    })
    
    custom_colors <- add_color_customization_server(
      ns, input, output, df, color_var, multi_group = TRUE
    )
    
    # --------------------------
    # Common legend logic
    # --------------------------
    legend_state <- reactiveValues(
      enabled = FALSE,
      position = "bottom"
    )
    
    observeEvent(input$use_common_legend, {
      legend_state$enabled <- isTRUE(input$use_common_legend)
    })
    
    observeEvent(input$common_legend_position, {
      legend_state$position <- input$common_legend_position
    })
    
    output$common_legend_controls <- renderUI({
      allow <- !is.null(color_var())
      if (!allow) return(NULL)
      
      checkbox <- with_help_tooltip(
        checkboxInput(
          ns("use_common_legend"), 
          "Use common legend",
          value = legend_state$enabled
        ),
        "Merge legends across subplots."
      )
      
      pos <- NULL
      if (legend_state$enabled) {
        pos <- with_help_tooltip(
          selectInput(
            ns("common_legend_position"),
            "Legend position",
            c("Bottom" = "bottom", "Right" = "right", "Top" = "top", "Left" = "left"),
            selected = legend_state$position
          ),
          "Choose legend placement."
        )
      }
      
      tagList(checkbox, pos)
    })
    
    # ==========================================================
    # APPLY LOGIC — the only place where the plot is computed
    # ==========================================================
    observeEvent(input$apply_plot, {
      data <- df()
      info <- summary_info()
      
      stored$plot_width  <- input$plot_width
      stored$plot_height <- input$plot_height
      
      if (is.null(info) || is.null(data) || nrow(data) == 0) {
        stored$warning <- "No data available."
        stored$plot <- NULL
        return()
      }
      
      s_vars <- resolve_reactive(info$selected_vars)
      g_var  <- resolve_reactive(info$group_var)
      strata_levels <- resolve_reactive(info$strata_levels)
      
      res <- build_descriptive_categorical_plot(
        df = data,
        selected_vars = s_vars,
        group_var = g_var,
        strata_levels = strata_levels,
        show_proportions = input$show_proportions,
        nrow_input = grid$rows(),
        ncol_input = grid$cols(),
        fill_colors = custom_colors(),
        base_size = base_size(),
        common_legend = legend_state$enabled,
        legend_position = if (legend_state$enabled) legend_state$position else NULL
      )

      stored$plot    <- res$plot
      stored$warning <- res$warning
      stored$layout  <- res$layout

      apply_grid_defaults_if_empty(
        input,
        session,
        "plot_grid",
        res$defaults,
        n_items = res$panels
      )
    })
    
    # -------------------------
    # UI: warnings
    # -------------------------
    output$grid_warning <- renderUI({
      if (!is.null(stored$warning))
        div(class = "alert alert-warning", stored$warning)
    })
    
    # -------------------------
    # RENDER PLOT
    # -------------------------
    output$plot <- renderPlot({
      p <- stored$plot
      if (is.null(p)) return(NULL)
      print(p)
    },
    width = function() {
      lay <- stored$layout
      if (is.null(lay)) return(600)
      nc <- (lay$ncol %||% 1)
      stored$plot_width * nc
    },
    height = function() {
      lay <- stored$layout
      if (is.null(lay)) return(600)
      nr <- (lay$nrow %||% 1)
      stored$plot_height * nr
    },
    res = 96)
    
    # -------------------------
    # DOWNLOAD BUTTON
    # -------------------------
    output$download_plot <- downloadHandler(
      filename = function() paste0("categorical_barplots_", Sys.Date(), ".png"),
      content = function(file) {
        p <- stored$plot
        req(!is.null(p))
        
        lay <- stored$layout
        nc <- (lay$ncol %||% 1)
        nr <- (lay$nrow %||% 1)
        
        w_in <- (stored$plot_width  * nc) / 96
        h_in <- (stored$plot_height * nr) / 96
        
        ggsave(
          file, p,
          dpi = 300,
          width = w_in, height = h_in,
          units = "in", limitsize = FALSE
        )
      }
    )
  })
}

apply_value_scale <- function(plot, show_proportions) {
  if (isTRUE(show_proportions)) {
    plot + scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, 1)
    )
  } else {
    plot + scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))
  }
}

build_descriptive_categorical_plot <- function(df,
                                               selected_vars = NULL,
                                               group_var = NULL,
                                               strata_levels = NULL,
                                               show_proportions = FALSE,
                                               nrow_input = NULL,
                                               ncol_input = NULL,
                                               fill_colors = NULL,
                                               base_size = 13,
                                               common_legend = FALSE,
                                               legend_position = NULL) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
  
  factor_vars <- names(df)[vapply(df, function(x) {
    is.character(x) || is.factor(x) || is.logical(x)
  }, logical(1))]
  
  if (!is.null(selected_vars) && length(selected_vars) > 0) {
    factor_vars <- intersect(factor_vars, selected_vars)
  }
  if (length(factor_vars) == 0) return(NULL)
  
  if (!is.null(group_var) && group_var %in% names(df)) {
    df[[group_var]] <- as.character(df[[group_var]])
    df[[group_var]][is.na(df[[group_var]]) | trimws(df[[group_var]]) == ""] <- "Missing"
    
    if (!is.null(strata_levels) && length(strata_levels) > 0) {
      keep_levels <- unique(strata_levels)
      df <- df[df[[group_var]] %in% keep_levels, , drop = FALSE]
      if (nrow(df) == 0) return(NULL)
      df[[group_var]] <- factor(df[[group_var]], levels = keep_levels)
    } else {
      df[[group_var]] <- factor(df[[group_var]], levels = unique(df[[group_var]]))
    }
  } else {
    group_var <- NULL
  }
  
  plots <- lapply(factor_vars, function(var) {
    group_col <- if (!is.null(group_var) && !identical(group_var, var)) group_var else NULL
    cols_to_use <- c(var, group_col)
    cols_to_use <- cols_to_use[cols_to_use %in% names(df)]
    var_data <- df[, cols_to_use, drop = FALSE]
    
    var_data[[var]] <- as.character(var_data[[var]])
    keep <- !is.na(var_data[[var]]) & trimws(var_data[[var]]) != ""
    if (!any(keep)) return(NULL)
    var_data <- var_data[keep, , drop = FALSE]
    
    level_order <- if (is.factor(df[[var]])) {
      as.character(levels(df[[var]]))
    } else {
      unique(var_data[[var]])
    }
    var_data[[var]] <- factor(var_data[[var]], levels = level_order)
    
    y_label <- if (isTRUE(show_proportions)) "Proportion" else "Count"
    
    if (!is.null(group_col)) {
      var_data[[group_col]] <- droplevels(var_data[[group_col]])
      count_df <- dplyr::count(var_data, .data[[var]], .data[[group_col]], name = "count")
      if (nrow(count_df) == 0) return(NULL)

      if (isTRUE(show_proportions)) {
        count_df <- count_df |>
          dplyr::group_by(.data[[group_col]]) |>
          dplyr::mutate(total = sum(.data$count, na.rm = TRUE)) |>
          dplyr::mutate(value = ifelse(.data$total > 0, .data$count / .data$total, 0)) |>
          dplyr::ungroup()
        count_df$total <- NULL
      } else {
        count_df <- dplyr::mutate(count_df, value = .data$count)
      }

      count_df[[var]] <- factor(as.character(count_df[[var]]), levels = level_order)
      group_levels <- levels(droplevels(var_data[[group_col]]))
      count_df[[group_col]] <- factor(as.character(count_df[[group_col]]), levels = group_levels)

      palette <- resolve_palette_for_levels(group_levels, custom = fill_colors)
      group_dodge <- position_dodge(width = 0.75)

      p <- ggplot(count_df, aes(x = .data[[var]], y = .data$value, fill = .data[[group_col]])) +
        geom_col(position = group_dodge, width = 0.65) +
        scale_fill_manual(values = palette) +
        ta_plot_theme(base_size = base_size) +
        labs(title = var, x = NULL, y = y_label, fill = group_col) +
        theme(
          plot.title = element_text(size = base_size, face = "bold", hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(color = "#9ca3af"),
          axis.ticks = element_line(color = "#9ca3af")
        )

      apply_value_scale(p, show_proportions)
    } else {
      count_df <- dplyr::count(var_data, .data[[var]], name = "count")
      if (nrow(count_df) == 0) return(NULL)

      total <- sum(count_df$count, na.rm = TRUE)
      if (isTRUE(show_proportions) && total > 0) {
        count_df$value <- count_df$count / total
      } else {
        count_df$value <- count_df$count
      }

      count_df[[var]] <- factor(as.character(count_df[[var]]), levels = level_order)

      single_fill <- if (!is.null(fill_colors) && length(fill_colors) > 0) {
        fill_colors[1]
      } else {
        resolve_single_color()
      }

      p <- ggplot(count_df, aes(x = .data[[var]], y = .data$value)) +
        geom_col(fill = single_fill, width = 0.65) +
        ta_plot_theme(base_size = base_size) +
        labs(title = var, x = NULL, y = y_label) +
        theme(
          plot.title = element_text(size = base_size, face = "bold", hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(color = "#9ca3af"),
          axis.ticks = element_line(color = "#9ca3af")
        )

      apply_value_scale(p, show_proportions)
    }
  })
  
  plots <- Filter(Negate(is.null), plots)
  if (length(plots) == 0) return(NULL)
  
  n_panels <- length(plots)
  defaults <- compute_default_grid(n_panels)

  layout <- basic_grid_layout(
    rows = suppressWarnings(as.numeric(nrow_input)),
    cols = suppressWarnings(as.numeric(ncol_input)),
    default_rows = defaults$rows,
    default_cols = defaults$cols
  )

  validation <- validate_grid(n_panels, layout$nrow, layout$ncol)

  combined <- NULL
  if (isTRUE(validation$valid)) {
    combined <- patchwork::wrap_plots(plots, nrow = layout$nrow, ncol = layout$ncol) +
      patchwork::plot_annotation(
        theme = theme(plot.title = element_text(size = base_size, face = "bold", hjust = 0.5))
      )

    combined <- apply_common_legend_layout(
      combined,
      legend_position = legend_position,
      collect_guides = isTRUE(common_legend)
    )
  }

  list(
    plot = combined,
    layout = list(nrow = layout$nrow, ncol = layout$ncol),
    panels = n_panels,
    warning = validation$message,
    defaults = defaults
  )
}

