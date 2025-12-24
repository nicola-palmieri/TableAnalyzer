# ===============================================================
# 🟦 Descriptive Visualization — Numeric Histograms
# ===============================================================

visualize_numeric_histograms_ui <- function(id) {
  ns <- NS(id)
  tagList(
    with_help_tooltip(
      checkboxInput(ns("use_density"), "Show density instead of count", FALSE),
      "Switch between showing counts or densities for each histogram."
    ),
    
    subplot_size_ui(
      ns,
      width_help  = "Set the width of each histogram panel in pixels.",
      height_help = "Set the height of each histogram panel in pixels."
    ),
    
    plot_grid_ui(
      id = ns("plot_grid"),
      rows_help = "Choose how many rows of histograms to display.",
      cols_help = "Choose how many columns of histograms to display."
    ),
    
    fluidRow(
      column(6, add_color_customization_ui(ns, multi_group = TRUE)),
      column(6, tagList(
        base_size_ui(
          ns,
          default = 13,
          help_text = "Adjust the base font size used for histogram labels."
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


visualize_numeric_histograms_plot_ui <- function(id) {
  ns <- NS(id)
  div(
    uiOutput(ns("grid_warning")),
    plotOutput(ns("plot"), width = "100%", height = "auto")
  )
}


visualize_numeric_histograms_server <- function(id, filtered_data, summary_info, is_active = NULL) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # ================================================================
    # Stored state (same logic as the ANOVA / barplots / boxplots)
    # ================================================================
    stored <- reactiveValues(
      plot = NULL,
      warning = NULL,
      layout = NULL,
      plot_width  = 400,
      plot_height = 300
    )
    
    df <- reactive(filtered_data())
    
    grid <- plot_grid_server("plot_grid", cols_max = 100L)
    base_size <- base_size_server(input, default = 13)
    
    # ================================================================
    # Color grouping
    # ================================================================
    color_var <- reactive({
      info <- summary_info()
      g <- resolve_reactive(info$group_var)
      dat <- df()
      if (is.null(g) || is.null(dat) || !g %in% names(dat)) return(NULL)
      g
    })
    
    custom_colors <- add_color_customization_server(
      ns, input, output, df, color_var, multi_group = TRUE
    )
    
    # ================================================================
    # Common legend
    # ================================================================
    legend_state <- reactiveValues(enabled = FALSE, position = "bottom")
    
    observeEvent(input$use_common_legend, {
      legend_state$enabled <- isTRUE(input$use_common_legend)
    })
    
    observeEvent(input$common_legend_position, {
      legend_state$position <- input$common_legend_position
    })
    
    output$common_legend_controls <- renderUI({
      if (is.null(color_var())) return(NULL)
      
      checkbox <- with_help_tooltip(
        checkboxInput(
          ns("use_common_legend"),
          "Use common legend",
          value = legend_state$enabled
        ),
        "Merge color legends across histogram panels."
      )
      
      pos <- NULL
      if (legend_state$enabled) {
        pos <- with_help_tooltip(
          selectInput(
            ns("common_legend_position"),
            "Legend position",
            c("Bottom"="bottom","Right"="right","Top"="top","Left"="left"),
            selected = legend_state$position
          ),
          "Choose placement for the combined legend."
        )
      }
      
      tagList(checkbox, pos)
    })
    
    # ================================================================
    # APPLY - compute histogram only when user clicks
    # ================================================================
    pending_auto <- reactiveVal(FALSE)

    auto_render_if_ready <- function() {
      info <- summary_info()
      if (is.null(info$type) || !identical(info$type, "descriptive")) return(FALSE)
      if (!is.null(is_active) && !isTRUE(is_active())) return(FALSE)
      if (is.null(input$plot_width) || is.null(input$plot_height)) {
        pending_auto(TRUE)
        return(FALSE)
      }
      pending_auto(FALSE)
      compute_plot(allow_reset = TRUE)
      TRUE
    }

    compute_plot <- function(allow_reset = FALSE) {
      stored$plot_width  <- input$plot_width %||% 400
      stored$plot_height <- input$plot_height %||% 300
      
      data <- df()
      info <- summary_info()
      
      if (is.null(data) || is.null(info) || nrow(data) == 0) {
        stored$warning <- "No data available."
        stored$plot <- NULL
        return()
      }
      
      s_vars <- resolve_reactive(info$selected_vars)
      g_var  <- resolve_reactive(info$group_var)
      strata_levels <- resolve_reactive(info$strata_levels)
      
      processed <- resolve_reactive(info$processed_data)
      dat <- if (!is.null(processed)) processed else data
      
      build_plot <- function(rows, cols) {
        build_descriptive_numeric_histogram(
          df = dat,
          selected_vars = s_vars,
          group_var = g_var,
          strata_levels = strata_levels,
          use_density = input$use_density,
          nrow_input = rows,
          ncol_input = cols,
          custom_colors = custom_colors(),
          base_size = base_size(),
          common_legend = legend_state$enabled,
          legend_position = if (legend_state$enabled) legend_state$position else NULL
        )
      }

      is_grid_warning <- function(msg) {
        !is.null(msg) && grepl("Grid", msg, fixed = TRUE)
      }

      res <- build_plot(grid$rows(), grid$cols())
      if (isTRUE(allow_reset) && !is.null(res) && is_grid_warning(res$warning)) {
        res <- build_plot(res$defaults$rows, res$defaults$cols)
      }

      stored$plot    <- res$plot
      stored$layout  <- res$layout
      stored$warning <- res$warning

      apply_grid_defaults_if_empty(
        input,
        session,
        "plot_grid",
        res$defaults,
        n_items = res$panels
      )
    }

    observeEvent(input$apply_plot, {
      compute_plot(allow_reset = FALSE)
    })

    observeEvent(summary_info(), {
      auto_render_if_ready()
    }, ignoreInit = FALSE)
    
    if (!is.null(is_active)) {
      observeEvent(is_active(), {
        auto_render_if_ready()
      }, ignoreInit = FALSE)
    }

    observeEvent(list(input$plot_width, input$plot_height), {
      if (isTRUE(pending_auto())) {
        auto_render_if_ready()
      }
    }, ignoreInit = TRUE)
    
    # ================================================================
    # Warning box
    # ================================================================
    output$grid_warning <- renderUI({
      if (!is.null(stored$warning))
        div(class = "alert alert-warning", stored$warning)
    })
    
    # ================================================================
    # RENDER HISTOGRAM
    # ================================================================
    output$plot <- renderPlot({
      p <- stored$plot
      if (is.null(p)) return(NULL)
      print(p)
    },
    width = function() {
      lay <- stored$layout
      if (is.null(lay)) return(600)
      stored$plot_width * (lay$ncol %||% 1)
    },
    height = function() {
      lay <- stored$layout
      if (is.null(lay)) return(600)
      stored$plot_height * (lay$nrow %||% 1)
    },
    res = 96)
    
    # ================================================================
    # DOWNLOAD — matches what user sees
    # ================================================================
    output$download_plot <- downloadHandler(
      filename = function() paste0("numeric_histograms_", Sys.Date(), ".png"),
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
          units = "in",
          limitsize = FALSE
        )
      }
    )
  })
}


build_descriptive_numeric_histogram <- function(df,
                                                selected_vars = NULL,
                                                group_var = NULL,
                                                strata_levels = NULL,
                                                use_density = FALSE,
                                                nrow_input = NULL,
                                                ncol_input = NULL,
                                                custom_colors = NULL,
                                                base_size = 13,
                                                common_legend = FALSE,
                                                legend_position = NULL) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)

  num_vars <- names(Filter(is.numeric, df))
  if (!is.null(selected_vars) && length(selected_vars) > 0) {
    num_vars <- intersect(num_vars, selected_vars)
  }
  if (length(num_vars) == 0) return(NULL)

  if (is.null(group_var) || !group_var %in% names(df)) {
    group_var <- NULL
  } else {
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
  }

  plots <- lapply(num_vars, function(var) {
    cols <- intersect(c(var, group_var), names(df))
    plot_data <- df[, cols, drop = FALSE]

    keep <- is.finite(plot_data[[var]])
    keep[is.na(keep)] <- FALSE
    plot_data <- plot_data[keep, , drop = FALSE]
    if (nrow(plot_data) == 0) return(NULL)

    if (!is.null(group_var)) {
      plot_data[[group_var]] <- droplevels(plot_data[[group_var]])
    }

    density_mode <- isTRUE(use_density) && length(unique(plot_data[[var]])) > 1
    base <- ggplot(plot_data, aes(x = .data[[var]]))
    y_label <- if (density_mode) "Density" else "Count"

    if (!is.null(group_var)) {
      group_levels <- levels(plot_data[[group_var]])
      palette <- resolve_palette_for_levels(group_levels, custom = custom_colors)
      if (density_mode) {
        base <- base +
          geom_density(aes(color = .data[[group_var]], fill = .data[[group_var]]), alpha = 0.3) +
          scale_color_manual(values = palette) +
          scale_fill_manual(values = palette) +
          labs(color = group_var, fill = group_var)
      } else {
        base <- base +
          geom_histogram(
            aes(fill = .data[[group_var]]),
            position = "identity",
            alpha = 0.5,
            bins = 30,
            color = "black",
            linewidth = 0.2
          ) +
          scale_fill_manual(values = palette) +
          labs(fill = group_var)
      }
    } else {
      single_color <- resolve_single_color(custom_colors)
      if (density_mode) {
        base <- base + geom_density(fill = single_color, color = single_color, alpha = 0.35)
      } else {
        base <- base +
          geom_histogram(
            fill = single_color,
            color = "black",
            bins = 30,
            linewidth = 0.2
          )
      }
    }

    base +
      ta_plot_theme(base_size = base_size) +
      labs(x = var, y = y_label) +
      scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "#9ca3af"),
        axis.ticks = element_line(color = "#9ca3af")
      )
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
  combined <- if (isTRUE(validation$valid)) {
    patchwork::wrap_plots(plots, nrow = layout$nrow, ncol = layout$ncol)
  } else {
    NULL
  }

  if (!is.null(combined)) {
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
