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

  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Step 4 — Visualize principal component analysis (PCA)"),
      p("Visualize multivariate structure using a PCA biplot."),
      hr(),
      with_help_tooltip(
        selectInput(
          ns("plot_type"),
          label = "Select visualization type",
          choices = c("PCA biplot" = "biplot"),
          selected = "biplot"
        ),
        "Pick how the PCA results should be displayed."
      ),
      with_help_tooltip(
        uiOutput(ns("pca_color_ui")),
        "Colour the samples using a grouping variable to spot patterns. Variables with more than 10 categories are not allowed."
      ),
      with_help_tooltip(
        uiOutput(ns("pca_shape_ui")),
        "Change the point shapes using a grouping variable for extra contrast. Variables with more than 10 categories are not allowed."
      ),
      with_help_tooltip(
        selectInput(
          ns("pca_label"),
          label = "Label points by",
          choices = all_choices,
          selected = "None"
        ),
        "Add text labels from a column to identify each sample."
      ),
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
      uiOutput(ns("layout_controls")),
      with_help_tooltip(
        checkboxInput(
          ns("show_loadings"),
          label = "Show loadings",
          value = FALSE
        ),
        "Display arrows that show how each original variable contributes to the components."
      ),
      with_help_tooltip(
        numericInput(
          ns("loading_scale"),
          label = "Loading arrow scale",
          value = 1.2, min = 0.1, max = 5, step = 0.1
        ),
        "Stretch or shrink the loading arrows to make them easier to read."
      ),
      fluidRow(
        column(
          width = 6,
          with_help_tooltip(
            numericInput(
              ns("plot_width"),
              label = "Plot width (px)",
              value = 800,
              min = 200,
              max = 2000,
              step = 50
            ),
            "Set the width of the PCA plot in pixels."
          )
        ),
        column(
          width = 6,
          with_help_tooltip(
            numericInput(
              ns("plot_height"),
              label = "Plot height (px)",
              value = 600,
              min = 200,
              max = 2000,
              step = 50
            ),
            "Set the height of the PCA plot in pixels."
          )
        )
      ),
      fluidRow(
        column(6, add_color_customization_ui(ns, multi_group = TRUE)),
        column(6, base_size_ui(
          ns,
          default = 13,
          help_text = "Adjust the base font size used for PCA plots."
        ))
      ),
      br(),
      fluidRow(
        column(6, actionButton(ns("apply_plot"), "Apply changes", width = "100%")),
        column(6, with_help_tooltip(
          downloadButton(ns("download_plot"), "Download plot", style = "width: 100%;"),
          "Save the PCA figure as an image file."
        ))
      )
    ),
    mainPanel(
      width = 8,
      h4("Plots"),
      uiOutput(ns("plot_warning")),
      plotOutput(ns("plot"))
    )
  )
}

visualize_pca_server <- function(id, filtered_data, model_fit) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # -- Reactives ------------------------------------------------------------
    model_info <- reactive({
      info <- model_fit()
      validate(need(!is.null(info) && identical(info$type, "pca"), "Run PCA first."))
      info
    })

    pca_entry <- reactive({
      info <- model_info()
      entry <- info$model
      validate(need(!is.null(entry), "PCA model missing."))
      entry
    })

    validate_choice <- function(value, pool) {
      if (is.null(value) || identical(value, "None") || !nzchar(value)) {
        return(NULL)
      }
      if (length(pool) == 0 || !(value %in% pool)) {
        return(NULL)
      }
      value
    }

    color_data <- reactive({
      entry <- pca_entry()
      if (!is.null(entry) && !is.null(entry$data)) {
        entry$data
      } else {
        resolve_reactive(filtered_data)
      }
    })

    available_choices_all <- reactive(.pca_aesthetic_choices(color_data()))

    valid_column <- function(var) {
      if (is.null(var) || identical(var, "None") || !nzchar(var)) {
        return(NULL)
      }
      data <- color_data()
      if (is.null(data) || !is.data.frame(data) || !var %in% names(data)) {
        return(NULL)
      }
      var
    }

    color_var_reactive <- reactive(valid_column(input$pca_color))

    color_level_order <- reactive({
      var <- color_var_reactive()
      if (is.null(var)) {
        return(NULL)
      }

      data <- color_data()
      if (is.null(data) || !is.data.frame(data) || !var %in% names(data)) {
        return(NULL)
      }

      column <- data[[var]]
      levels_vec <- if (is.factor(column)) {
        levels(base::droplevels(column))
      } else {
        unique(as.character(column[!is.na(column)]))
      }

      levels_vec <- as.character(levels_vec)
      levels_vec <- levels_vec[!is.na(levels_vec) & nzchar(levels_vec)]
      if (!length(levels_vec)) NULL else levels_vec
    })

    custom_colors <- add_color_customization_server(
      ns = ns,
      input = input,
      output = output,
      data = color_data,
      color_var_reactive = color_var_reactive,
      multi_group = TRUE,
      level_order_reactive = color_level_order
    )

    base_size <- base_size_server(
      input = input,
      default = 14
    )

    max_levels <- if (exists("MAX_STRATIFICATION_LEVELS")) MAX_STRATIFICATION_LEVELS else 10L

    level_check <- function(var) {
      if (is.null(var) || identical(var, "None") || !nzchar(var)) {
        return(list(levels = NULL, valid = TRUE, message = NULL))
      }

      data <- color_data()
      if (is.null(data) || !is.data.frame(data) || !var %in% names(data)) {
        return(list(levels = NULL, valid = TRUE, message = NULL))
      }

      column <- data[[var]]
      values <- if (is.factor(column)) {
        levels(base::droplevels(column))
      } else {
        unique(as.character(column[!is.na(column)]))
      }

      values <- values[!is.na(values) & nzchar(values)]
      n_levels <- length(values)

      list(
        levels = values,
        valid = n_levels <= max_levels,
        message = if (n_levels > max_levels) sprintf("'%s' has too many levels (%d > %d).", var, n_levels, max_levels) else NULL
      )
    }

    output$pca_color_ui <- renderUI({
      choices <- available_choices_all()
      pool <- unname(choices)
      selected <- if (!is.null(input$pca_color) && input$pca_color %in% pool) input$pca_color else "None"
      check <- level_check(selected)

      tagList(
        selectInput(
          ns("pca_color"),
          label = "Color points by",
          choices = choices,
          selected = selected
        ),
        if (!is.null(check$message)) div(class = "text-danger small", check$message)
      )
    })

    output$pca_shape_ui <- renderUI({
      choices <- available_choices_all()
      pool <- unname(choices)
      selected <- if (!is.null(input$pca_shape) && input$pca_shape %in% pool) input$pca_shape else "None"
      check <- level_check(selected)

      tagList(
        selectInput(
          ns("pca_shape"),
          label = "Shape points by",
          choices = choices,
          selected = selected
        ),
        if (!is.null(check$message)) div(class = "text-danger small", check$message)
      )
    })

    facet_grid_inputs <- plot_grid_server("facet_grid")

    observeEvent(available_choices_all(), {
      all_choices <- available_choices_all()

      update_input <- function(id, current, choices) {
        pool <- unname(choices)
        selected <- if (!is.null(current) && current %in% pool) current else "None"
        updateSelectInput(session, id, choices = choices, selected = selected)
      }

      update_input("pca_label", input$pca_label, all_choices)
      update_input("facet_var", input$facet_var, all_choices)
    }, ignoreNULL = FALSE)

    output$layout_controls <- renderUI({
      facet_info <- facet_selection()
      if (is.null(facet_info$var) || length(facet_info$levels) <= 1) {
        return(NULL)
      }

      plot_grid_ui(
        id = session$ns("facet_grid"),
        rows_help = "Decide how many rows of panels to show when faceting the PCA plot.",
        cols_help = "Decide how many columns of panels to show when faceting the PCA plot."
      )
    })
    empty_facet <- list(var = NULL, levels = NULL, column = NULL)

    facet_selection <- reactive({
      data <- color_data()
      if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
        return(empty_facet)
      }

      facet_var <- valid_column(input$facet_var)
      if (is.null(facet_var)) {
        return(empty_facet)
      }

      column <- data[[facet_var]]
      if (is.null(column)) {
        return(empty_facet)
      }

      levels <- if (is.factor(column)) {
        levels(droplevels(column))
      } else {
        unique(as.character(column[!is.na(column)]))
      }

      levels <- levels[!is.na(levels)]

      list(var = facet_var, levels = levels, column = column)
    })

    build_message_panel <- function(title, message, show_title = TRUE) {
      has_title <- isTRUE(show_title) && !is.null(title) && nzchar(title)
      ggplot() +
        ta_plot_theme_void() +
        annotate("text", x = 0.5, y = 0.5, label = message, size = 4, hjust = 0.5, vjust = 0.5) +
        coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off") +
        labs(title = if (has_title) title else NULL) +
        theme(plot.title = if (has_title) element_text(size = 14, face = "bold", hjust = 0.5) else element_blank())
    }

    sanitize_suffix <- function(value) {
      value <- value[1]
      safe <- gsub("[^A-Za-z0-9]+", "_", value)
      safe <- gsub("_+", "_", safe)
      safe <- gsub("^_|_$", "", safe)
      if (!nzchar(safe)) {
        "facet"
      } else {
        tolower(safe)
      }
    }

    plot_info <- eventReactive(input$apply_plot, {
      req(isTRUE(input$apply_plot > 0))
      req(input$plot_type)
      validate(need(input$plot_type == "biplot", "Unsupported plot type."))

      entry <- pca_entry()
      plot_w <- ifelse(is.na(input$plot_width) || input$plot_width <= 0, 800, input$plot_width)
      plot_h <- ifelse(is.na(input$plot_height) || input$plot_height <= 0, 600, input$plot_height)

      empty_result <- function(message) {
        defaults <- compute_default_grid(1L)
        layout <- basic_grid_layout(rows = 1, cols = 1, default_rows = 1, default_cols = 1)
        list(
          plot = build_message_panel(title = NULL, message = message, show_title = FALSE),
          layout = layout,
          facet_levels = NULL,
          panels = 1L,
          warning = NULL,
          defaults = defaults,
          facet_var = NULL,
          plot_width = plot_w,
          plot_height = plot_h
        )
      }

      if (is.null(entry) || is.null(entry$model)) {
        message <- if (!is.null(entry$message) && nzchar(entry$message)) entry$message else "No PCA results available."
        return(empty_result(message))
      }

      if (is.null(entry$model$x) || nrow(entry$model$x) < 2) {
        return(empty_result("PCA scores not available."))
      }

      data <- entry$data
      if (is.null(data) || nrow(data) == 0) {
        return(empty_result("PCA data unavailable."))
      }

      all_pool <- unname(available_choices_all())
      color_var <- validate_choice(input$pca_color, all_pool)
      shape_var <- validate_choice(input$pca_shape, all_pool)
      label_var <- validate_choice(input$pca_label, all_pool)
      label_size <- ifelse(is.null(input$pca_label_size) || is.na(input$pca_label_size), 2, input$pca_label_size)
      show_loadings <- isTRUE(input$show_loadings)
      loading_scale <- ifelse(is.null(input$loading_scale) || is.na(input$loading_scale), 1.2, input$loading_scale)

      max_levels <- if (exists("MAX_STRATIFICATION_LEVELS")) MAX_STRATIFICATION_LEVELS else 10L

      validate_levels <- function(var) {
        if (is.null(var) || !var %in% names(data)) {
          return(NULL)
        }

        column <- data[[var]]
        values <- if (is.factor(column)) {
          levels(base::droplevels(column))
        } else {
          unique(as.character(column[!is.na(column)]))
        }

        values <- values[!is.na(values) & nzchar(values)]
        n_levels <- length(values)

        validate(need(
          n_levels <= max_levels,
          sprintf("'%s' has too many levels (%d > %d).", var, n_levels, max_levels)
        ))

        values
      }

      facet_info <- facet_selection()
      facet_var <- facet_info$var
      facet_levels <- facet_info$levels
      facet_column <- facet_info$column

      subset_list <- if (is.null(facet_var)) {
        list(All = seq_len(nrow(data)))
      } else if (length(facet_levels) > 0) {
        facet_values <- as.character(if (is.factor(facet_column)) droplevels(facet_column) else facet_column)
        stats::setNames(lapply(facet_levels, function(level) {
          which(!is.na(facet_values) & facet_values == level)
        }), facet_levels)
      } else {
        list(`No data` = integer())
      }

      color_check <- level_check(color_var)
      shape_check <- level_check(shape_var)

      validate(need(color_check$valid, color_check$message))
      validate(need(shape_check$valid, shape_check$message))

      scores <- as.data.frame(entry$model$x[, 1:2, drop = FALSE])
      names(scores)[1:2] <- c("PC1", "PC2")

      adjust_limits <- function(values) {
        if (length(values) != 2 || any(!is.finite(values))) {
          return(c(-1, 1))
        }
        rng <- range(values)
        if (diff(rng) == 0) {
          center <- rng[1]
          width <- if (abs(center) < 1) 1 else abs(center) * 0.1
          return(c(center - width, center + width))
        }
        rng
      }

      x_limits <- adjust_limits(range(scores$PC1, na.rm = TRUE))
      y_limits <- adjust_limits(range(scores$PC2, na.rm = TRUE))

      plot_list <- lapply(names(subset_list), function(key) {
        idx <- subset_list[[key]]
        if (length(idx) == 0) {
          return(build_message_panel(title = key, message = "No data available for this facet.", show_title = TRUE))
        }

        local_color <- if (!is.null(color_var) && color_var %in% names(data)) color_var else NULL
        local_shape <- if (!is.null(shape_var) && shape_var %in% names(data)) shape_var else NULL
        local_label <- if (!is.null(label_var) && label_var %in% names(data)) label_var else NULL

        plot_obj <- build_pca_biplot(
          pca_obj = entry$model,
          data = data,
          color_var = local_color,
          shape_var = local_shape,
          label_var = local_label,
          label_size = label_size,
          show_loadings = show_loadings,
          loading_scale = loading_scale,
          custom_colors = custom_colors(),
          subset_rows = idx,
          color_levels = color_levels,
          x_limits = x_limits,
          y_limits = y_limits,
          base_size = base_size()
        )

        if (!is.null(facet_var)) {
          plot_obj <- plot_obj + ggtitle(key) + theme(plot.title = element_text(size = 14, face = "bold"))
        }

        plot_obj
      })
      names(plot_list) <- names(subset_list)

      plot_list <- Filter(Negate(is.null), plot_list)
      validate(need(length(plot_list) > 0, "No PCA plots available."))

      panel_count <- length(plot_list)
      defaults <- compute_default_grid(panel_count)

      use_custom_layout <- !is.null(facet_var) && panel_count > 1
      rows_input <- if (use_custom_layout) facet_grid_inputs$rows() else NA
      cols_input <- if (use_custom_layout) facet_grid_inputs$cols() else NA

      layout <- basic_grid_layout(
        rows = rows_input,
        cols = cols_input,
        default_rows = defaults$rows,
        default_cols = defaults$cols
      )

      validation <- validate_grid(panel_count, layout$nrow, layout$ncol)

      combined <- NULL
      if (isTRUE(validation$valid)) {
        combined <- patchwork::wrap_plots(
          plotlist = plot_list,
          nrow = layout$nrow,
          ncol = layout$ncol
        ) +
          patchwork::plot_layout(guides = "collect")
      }

      list(
        plot = combined,
        layout = layout,
        facet_levels = if (!is.null(facet_var)) names(plot_list) else NULL,
        panels = panel_count,
        warning = validation$message,
        defaults = defaults,
        facet_var = facet_var,
        plot_width = plot_w,
        plot_height = plot_h
      )
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    observeEvent(plot_info(), {
      info <- plot_info()
      apply_grid_defaults_if_empty(input, session, "facet_grid", info$defaults, n_items = info$panels)
    }, ignoreNULL = TRUE)

    plot_dimensions <- reactive({
      info <- plot_info()
      if (is.null(info)) {
        return(list(w = 800, h = 600))
      }

      layout <- info$layout

      plot_w <- info$plot_width %||% 800
      plot_h <- info$plot_height %||% 600

      ncol <- if (!is.null(layout$ncol)) max(1, layout$ncol) else 1
      nrow <- if (!is.null(layout$nrow)) max(1, layout$nrow) else 1

      list(w = plot_w * ncol, h = plot_h * nrow)
    })
    
    output$plot_warning <- renderUI({
      info <- plot_info()
      if (is.null(info)) return(NULL)
      if (!is.null(info$warning)) {
        div(class = "alert alert-warning", info$warning)
      } else {
        NULL
      }
    })

    output$plot <- renderPlot({
      info <- plot_info()
      if (is.null(info) || !is.null(info$warning) || is.null(info$plot)) return(NULL)
      info$plot
    },
    width = function() plot_dimensions()$w,
    height = function() plot_dimensions()$h,
    res = 96)
    
    output$download_plot <- downloadHandler(
      filename = function() {
        info <- plot_info()
        facet_var <- info$facet_var
        suffix <- if (!is.null(facet_var)) {
          paste0("_facet_", sanitize_suffix(facet_var))
        } else {
          "_global"
        }
        paste0("pca_biplot", suffix, "_", Sys.Date(), ".png")
      },
      content = function(file) {
        info <- plot_info()
        req(!is.null(info))
        req(is.null(info$warning))
        s <- plot_dimensions()
        ggsave(
          filename = file,
          plot = info$plot,
          device = "png",
          dpi = 300,
          width  = s$w / 96,
          height = s$h / 96,
          units = "in",
          limitsize = FALSE
        )
      }
    )
  })
}


build_pca_biplot <- function(pca_obj, data, color_var = NULL, shape_var = NULL,
                             label_var = NULL, label_size = 2,
                             show_loadings = FALSE, loading_scale = 1.2,
                             custom_colors = NULL, subset_rows = NULL,
                             color_levels = NULL, x_limits = NULL,
                             y_limits = NULL, base_size = 13) {
  stopifnot(!is.null(pca_obj$x))

  scores <- as.data.frame(pca_obj$x[, 1:2])
  names(scores)[1:2] <- c("PC1", "PC2")

  if (!is.null(subset_rows)) {
    subset_rows <- unique(subset_rows)
    subset_rows <- subset_rows[subset_rows >= 1 & subset_rows <= nrow(scores)]
    scores <- scores[subset_rows, , drop = FALSE]
    if (!is.null(data)) {
      data <- data[subset_rows, , drop = FALSE]
    }
  }

  var_exp <- 100 * (pca_obj$sdev^2 / sum(pca_obj$sdev^2))
  x_lab <- sprintf("PC1 (%.1f%%)", var_exp[1])
  y_lab <- sprintf("PC2 (%.1f%%)", var_exp[2])

  if (!is.null(data) && nrow(data) == nrow(scores)) {
    plot_data <- cbind(scores, data)
  } else {
    plot_data <- scores
  }
  
  if (!is.null(label_var) && !identical(label_var, "") && !is.null(plot_data[[label_var]])) {
    label_values <- as.character(plot_data[[label_var]])
    label_values[is.na(label_values) | trimws(label_values) == ""] <- NA_character_
    if (any(!is.na(label_values))) {
      plot_data$label_value <- label_values
    } else {
      label_var <- NULL
    }
  } else {
    label_var <- NULL
  }
  
  if (!is.null(color_var) && !is.null(plot_data[[color_var]])) {
    if (is.null(color_levels)) {
      color_levels <- if (is.factor(plot_data[[color_var]])) {
        levels(plot_data[[color_var]])
      } else {
        unique(as.character(plot_data[[color_var]]))
      }
    }
    color_levels <- unique(color_levels[!is.na(color_levels)])
    plot_data[[color_var]] <- factor(as.character(plot_data[[color_var]]), levels = color_levels)
  }
  
  aes_mapping <- aes(x = PC1, y = PC2)
  if (!is.null(color_var)) aes_mapping <- modifyList(aes_mapping, aes(color = .data[[color_var]]))
  if (!is.null(shape_var)) aes_mapping <- modifyList(aes_mapping, aes(shape = .data[[shape_var]]))
  
  single_color <- resolve_single_color(custom_colors)
  g <- ggplot(plot_data, aes_mapping) +
    geom_point(
      size = 3,
      shape = if (is.null(shape_var)) 16 else NULL,
      color = if (is.null(color_var)) single_color else NULL
    ) +
    ta_plot_theme(base_size = base_size) +
    labs(
      x = x_lab,
      y = y_lab,
      color = if (!is.null(color_var)) color_var else NULL,
      shape = if (!is.null(shape_var)) shape_var else NULL
    ) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "#9ca3af"),
      axis.ticks = element_line(color = "#9ca3af"),
      legend.position = "right"
    )
  
  if (!is.null(color_var)) {
    palette <- resolve_palette_for_levels(levels(plot_data[[color_var]]), custom = custom_colors)
    g <- g + scale_color_manual(values = palette)
  }
  
  if (!is.null(x_limits) || !is.null(y_limits)) {
    g <- g + coord_cartesian(xlim = x_limits, ylim = y_limits)
  }

  if (!is.null(label_var)) {
    g <- g + ggrepel::geom_text_repel(
      aes(label = label_value),
      color = if (is.null(color_var)) single_color else NULL,
      size = label_size,
      max.overlaps = Inf,
      min.segment.length = 0,
      box.padding = 0.3,
      point.padding = 0.2,
      segment.size = 0.2,
      na.rm = TRUE
    )
  }
  
  # ---- Loadings as arrows (optional) ----
  if (isTRUE(show_loadings) && !is.null(pca_obj$rotation)) {
    R <- as.data.frame(pca_obj$rotation[, 1:2, drop = FALSE])
    R$variable <- rownames(pca_obj$rotation)
    
    # scale arrows to score space
    rx <- diff(range(pca_obj$x[, 1], na.rm = TRUE))
    ry <- diff(range(pca_obj$x[, 2], na.rm = TRUE))
    sx <- ifelse(is.finite(rx) && rx > 0, rx, 1)
    sy <- ifelse(is.finite(ry) && ry > 0, ry, 1)
    
    arrows_df <- transform(
      R,
      x = 0, y = 0,
      xend = PC1 * sx * loading_scale,
      yend = PC2 * sy * loading_scale
    )
    
    g <- g +
      geom_segment(
        data = arrows_df,
        aes(x = x, y = y, xend = xend, yend = yend),
        inherit.aes = FALSE,
        arrow = grid::arrow(length = grid::unit(0.02, "npc")),
        linewidth = 0.4,
        color = "grey30"
      ) +
      ggrepel::geom_text_repel(
        data = arrows_df,
        aes(x = xend, y = yend, label = variable),
        inherit.aes = FALSE,
        size = 3,
        color = "grey20",
        max.overlaps = Inf,
        segment.size = 0.2,
        box.padding = 0.2,
        point.padding = 0.2
      )
  }
  
  g
}
