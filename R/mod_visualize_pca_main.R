# mod_visualize_pca: server coordination and reactive state.
visualize_pca_server <- function(id, filtered_data, model_fit, reset_trigger = reactive(NULL)) {
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
      # Metadata controls can remain mounted while another analysis is active.
      # Their data fallback must not emit a PCA validation error during that
      # normal transition; actual plot/model validation still uses pca_entry().
      entry <- optional_plot_metadata(pca_entry())
      if (!is.null(entry) && !is.null(entry$data)) {
        entry$data
      } else {
        optional_plot_metadata(resolve_reactive(filtered_data))
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

    local_reset_token <- reactiveVal(NULL)
    color_reset_token <- reactive({
      local <- local_reset_token()
      external <- reset_trigger()
      if (is.null(local)) external else paste0(external %||% "initial", "_local_", local)
    })

    custom_colors <- add_color_customization_server(
      ns = ns,
      input = input,
      output = output,
      data = color_data,
      color_var_reactive = color_var_reactive,
      multi_group = TRUE,
      level_order_reactive = color_level_order,
      reset_token = color_reset_token
    )

    base_size <- base_size_server(
      input = input,
      default = VetmeduniAppTemplate::PLOT_BASE_SIZE
    )

    subplot_limits <- subplot_size_defaults()
    plot_width_default <- 800
    plot_height_default <- 600

    last_reset_token <- reactiveVal(NULL)
    pending_reset_token <- reactiveVal(NULL)

    reset_pca_inputs <- function() {
      updateSelectInput(session, "plot_type", selected = "biplot")
      updateSelectInput(session, "pca_color", selected = "None")
      updateSelectInput(session, "pca_shape", selected = "None")
      updateSelectInput(session, "pca_label", selected = "None")
      updateSelectInput(session, "facet_var", selected = "None")
      updateNumericInput(session, "pca_label_size", value = 2)
      updateCheckboxInput(session, "show_ellipses", value = FALSE)
      updateCheckboxInput(session, "show_loadings", value = FALSE)
      updateNumericInput(session, "loading_scale", value = 1.2)
      updateNumericInput(session, "plot_width", value = 800)
      updateNumericInput(session, "plot_height", value = 600)
      updateNumericInput(session, "plot_base_size", value = VetmeduniAppTemplate::PLOT_BASE_SIZE)
      updateNumericInput(session, "facet_grid-rows", value = NA)
      updateNumericInput(session, "facet_grid-cols", value = NA)
      facet_grid_inputs$reset()
    }

    observeEvent(input$reset, {
      reset_pca_inputs()
      local_reset_token((isolate(local_reset_token()) %||% 0L) + 1L)
    }, ignoreInit = TRUE)

    observeEvent(reset_trigger(), {
      token <- reset_trigger()
      if (is.null(token) || identical(token, last_reset_token())) return()
      info <- tryCatch(model_fit(), shiny.silent.stop = function(e) NULL)
      if (is.null(info) || !identical(info$type, "pca")) {
        pending_reset_token(token)
        return()
      }

      session$onFlushed(function() {
        reset_pca_inputs()
        last_reset_token(token)
        pending_reset_token(NULL)

      }, once = TRUE)
    }, ignoreInit = TRUE)
    
    observeEvent(model_fit(), {
      info <- tryCatch(model_fit(), shiny.silent.stop = function(e) NULL)
      if (is.null(info) || !identical(info$type, "pca")) return()

      token <- pending_reset_token()
      if (!is.null(token) && !identical(token, last_reset_token())) {
        session$onFlushed(function() {
          reset_pca_inputs()
          last_reset_token(token)
          pending_reset_token(NULL)

        }, once = TRUE)
        return()
      }

    }, ignoreInit = FALSE)
    
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

    facet_grid_inputs <- single_grid_settings(input, session, "facet_grid")

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
      info <- optional_plot_metadata(model_fit(), "pca")
      if (is.null(info)) return(NULL)
      facet_info <- facet_selection()
      if (is.null(facet_info$var) || length(facet_info$levels) <= 1) {
        return(NULL)
      }

      plot_grid_ui(
        id = session$ns("facet_grid"),
        rows_value = isolate(facet_grid_inputs$cache$rows) %||% NA,
        cols_value = isolate(facet_grid_inputs$cache$cols) %||% NA,
        rows_help = "Decide how many rows of panels to show when faceting the PCA plot.",
        cols_help = "Decide how many columns of panels to show when faceting the PCA plot."
      )
    })
    keep_optional_ui_current(output, "pca_color_ui", "pca_shape_ui", "layout_controls")
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
      p <- ggplot() +
        ta_plot_theme_void() +
        annotate("text", x = 0.5, y = 0.5, label = message, size = 4, hjust = 0.5, vjust = 0.5) +
        coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off")
      VetmeduniAppTemplate::plot_panel_label(p, if (has_title) title else NULL, base_size())
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

    plot_info <- VetmeduniAppTemplate::app_plot_snapshot(function() {
      VetmeduniAppTemplate::app_plot_numeric(input$plot_base_size, VetmeduniAppTemplate::PLOT_BASE_SIZE, min = 6, max = 30, label = "Base size")
      req(input$plot_type)
      validate(need(input$plot_type == "biplot", "Unsupported plot type."))

      entry <- pca_entry()
      plot_w <- input$plot_width
      plot_w <- VetmeduniAppTemplate::app_plot_numeric(
        plot_w,
        default = plot_width_default,
        min = subplot_limits$width$min,
        max = subplot_limits$width$max,
        label = "Plot width"
      )
      plot_h <- input$plot_height
      plot_h <- VetmeduniAppTemplate::app_plot_numeric(
        plot_h,
        default = plot_height_default,
        min = subplot_limits$height$min,
        max = subplot_limits$height$max,
        label = "Plot height"
      )

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
      label_size <- VetmeduniAppTemplate::app_plot_numeric(input$pca_label_size, 2,
        min = 0.5, max = 6, label = "Label size")
      show_loadings <- isTRUE(input$show_loadings)
      loading_scale <- VetmeduniAppTemplate::app_plot_numeric(input$loading_scale, 1.2,
        min = 0.1, max = 5, label = "Loading arrow scale")

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
          show_ellipses = isTRUE(input$show_ellipses),
          loading_scale = loading_scale,
          custom_colors = custom_colors(),
          subset_rows = idx,
          x_limits = x_limits,
          y_limits = y_limits,
          base_size = base_size()
        )

        if (!is.null(facet_var)) {
          plot_obj <- VetmeduniAppTemplate::plot_panel_label(plot_obj,key,base_size())
        }

        plot_obj
      })
      names(plot_list) <- names(subset_list)

      plot_list <- Filter(Negate(is.null), plot_list)
      validate(need(length(plot_list) > 0, "No PCA plots available."))

      panel_count <- length(plot_list)
      defaults <- compute_default_grid(panel_count, plot_w, plot_h)

      recommended <- facet_grid_inputs$resolve(panel_count, plot_w, plot_h)
      use_custom_layout <- !is.null(facet_var) && panel_count > 1
      rows_input <- if (use_custom_layout) recommended$rows else NA
      cols_input <- if (use_custom_layout) recommended$cols else NA

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
    }, ready = function() {
      info <- model_fit()
      !is.null(info) && identical(info$type, "pca")
    })

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

    output$plot_container <- renderUI({
      info <- plot_info()
      if (is.null(info) || is.null(info$plot)) {
        if (!is.null(info) && !is.null(info$warning)) {
          return(div(uiOutput(ns("plot_warning"))))
        }
        return(NULL)
      }

      tagList(
        uiOutput(ns("plot_warning")),
        VetmeduniAppTemplate::content_plot_ui(ns("plot"), list(width = plot_dimensions()$w, height = plot_dimensions()$h))
      )
    })

    output$plot <- renderPlot({
      info <- plot_info()
      if (is.null(info) || is.null(info$plot)) return(NULL)
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
        s <- plot_dimensions()
        VetmeduniAppTemplate::save_plot_png(info$plot, file,
          width_px = s$w, height_px = s$h, dpi = 300)
      }
    )
  })
}
