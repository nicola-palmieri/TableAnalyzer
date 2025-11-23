# ===============================================================
# 🟦 Descriptive Visualization — Numeric Boxplots
# ===============================================================

visualize_numeric_boxplots_ui <- function(id) {
  ns <- NS(id)
  tagList(
    with_help_tooltip(
      checkboxInput(ns("show_points"), "Show individual data points", TRUE),
      "Add the raw observations on top of each boxplot."
    ),
    with_help_tooltip(
      checkboxInput(ns("show_outliers"), "Highlight boxplot outliers", FALSE),
      "Highlight points that fall outside the typical range."
    ),
    
    conditionalPanel(
      condition = sprintf("input['%s']", ns("show_outliers")),
      uiOutput(ns("outlier_label_ui"))
    ),
    
    subplot_size_ui(
      ns,
      width_value = 200,
      height_value = 800,
      width_help = "Control how wide each boxplot panel should be.",
      height_help = "Control how tall each boxplot panel should be."
    ),
    
    plot_grid_ui(
      id = ns("plot_grid"),
      rows_help = "Choose how many rows of plots to display.",
      cols_help = "Choose how many columns of plots to display.",
      cols_max = 100L
    ),
    
    fluidRow(
      column(6, add_color_customization_ui(ns, multi_group = TRUE)),
      column(6, tagList(
        base_size_ui(
          ns,
          default = 13,
          help_text = "Adjust the base font size used for boxplot text."
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

visualize_numeric_boxplots_plot_ui <- function(id) {
  ns <- NS(id)
  div(
    uiOutput(ns("grid_warning")),
    plotOutput(ns("plot"), width = "100%", height = "auto")
  )
}


visualize_numeric_boxplots_server <- function(id, filtered_data, summary_info, is_active = NULL) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    #======================================================
    # Stored state (same pattern as ANOVA + Barplots)
    #======================================================
    stored <- reactiveValues(
      plot = NULL,
      warning = NULL,
      layout = NULL,
      plot_width = 200,
      plot_height = 800
    )
    
    df <- reactive(filtered_data())
    
    grid <- plot_grid_server("plot_grid", cols_max = 100L)
    base_size <- base_size_server(input, default = 13)
    
    #======================================================
    # Outlier label dropdown (depends on data)
    #======================================================
    output$outlier_label_ui <- renderUI({
      dat <- df()
      if (is.null(dat)) return(NULL)
      
      cat_cols <- names(dat)[vapply(dat, \(x) is.character(x) || is.factor(x) || is.logical(x), logical(1))]
      cat_cols <- sort(unique(cat_cols))
      
      current <- isolate(input$outlier_label)
      if (is.null(current) || !current %in% cat_cols) current <- ""
      
      with_help_tooltip(
        selectInput(
          ns("outlier_label"),
          "Label outliers by",
          choices = c("None" = "", stats::setNames(cat_cols, cat_cols)),
          selected = current
        ),
        "Choose a column to annotate the highlighted outliers."
      )
    })
    
    #======================================================
    # Color grouping
    #======================================================
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
    
    #======================================================
    # Common legend toggle
    #======================================================
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
        "Merge legends across panels."
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
    
    #======================================================
    # APPLY BUTTON — the *only* place where plots are computed
    #======================================================
    observeEvent(input$apply_plot, {
      
      stored$plot_width  <- input$plot_width
      stored$plot_height <- input$plot_height
      
      data <- df()
      info <- summary_info()
      
      if (is.null(data) || is.null(info) || nrow(data) == 0) {
        stored$warning <- "No data available."
        stored$plot <- NULL
        return()
      }
      
      s_vars <- resolve_reactive(info$selected_vars)
      g_var  <- resolve_reactive(info$group_var)
      processed <- resolve_reactive(info$processed_data)
      dat <- if (!is.null(processed)) processed else data
      
      res <- build_descriptive_numeric_boxplot(
        df = dat,
        selected_vars = s_vars,
        group_var = g_var,
        show_points = input$show_points,
        show_outliers = input$show_outliers,
        outlier_label_var = validate_outlier_label(input$outlier_label),
        nrow_input = grid$rows(),
        ncol_input = grid$cols(),
        custom_colors = custom_colors(),
        base_size = base_size(),
        common_legend = legend_state$enabled,
        legend_position = if (legend_state$enabled) legend_state$position else NULL
      )

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
    })
    
    #======================================================
    # warnings
    #======================================================
    output$grid_warning <- renderUI({
      if (!is.null(stored$warning))
        div(class = "alert alert-warning", stored$warning)
    })
    
    #======================================================
    # RENDER PLOT
    #======================================================
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
    
    #======================================================
    # DOWNLOAD BUTTON (same behavior as ANOVA + categorical)
    #======================================================
    output$download_plot <- downloadHandler(
      filename = function() paste0("numeric_boxplots_", Sys.Date(), ".png"),
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
          width = w_in,
          height = h_in,
          units = "in",
          limitsize = FALSE
        )
      }
    )
  })
}


build_descriptive_numeric_boxplot <- function(df,
                                              selected_vars = NULL,
                                              group_var = NULL,
                                              show_points = TRUE,
                                              show_outliers = FALSE,
                                              outlier_label_var = NULL,
                                              nrow_input = NULL,
                                              ncol_input = NULL,
                                              custom_colors = NULL,
                                              base_size = 13,
                                              common_legend = FALSE,
                                              legend_position = NULL) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)

  num_vars <- names(df)[vapply(df, is.numeric, logical(1))]
  if (!is.null(selected_vars) && length(selected_vars) > 0) {
    num_vars <- intersect(num_vars, selected_vars)
  }
  if (length(num_vars) == 0) return(NULL)
  
  # ensure discrete x if grouped
  if (!is.null(group_var) && group_var %in% names(df)) {
    df[[group_var]] <- as.factor(df[[group_var]])
  } else {
    group_var <- NULL
  }
  
  plots <- lapply(num_vars, function(var) {
    vec <- df[[var]]
    if (all(is.na(vec))) return(NULL)

    if (!is.null(group_var)) {
      group_levels <- levels(df[[group_var]])
      palette <- resolve_palette_for_levels(group_levels, custom = custom_colors)
      p <- ggplot(df, aes(x = .data[[group_var]], y = .data[[var]], fill = .data[[group_var]])) +
        geom_boxplot(outlier.shape = NA, width = 0.6) +
        scale_fill_manual(values = palette) +
        ta_plot_theme(base_size = base_size) +
        labs(x = NULL, y = var) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(color = "#9ca3af"),
          axis.ticks = element_line(color = "#9ca3af")
        )

      needs_color_scale <- FALSE
      if (isTRUE(show_points)) {
        p <- p + geom_jitter(
          aes(color = .data[[group_var]]),
          width = 0.2,
          alpha = 0.5,
          size = 1
        )
        needs_color_scale <- TRUE
      }

      if (isTRUE(show_outliers)) {
        outliers <- prepare_boxplot_outliers(
          data = df,
          value_col = var,
          group_col = group_var,
          label_col = outlier_label_var
        )
        if (has_rows(outliers)) {
          p <- p + geom_point(
            data = outliers,
            aes(x = x, y = y, color = group),
            inherit.aes = FALSE,
            size = 2.5,
            show.legend = FALSE
          )
          needs_color_scale <- TRUE

          label_data <- filter_labeled_outliers(outliers)
          if (has_rows(label_data)) {
            p <- p + ggrepel::geom_text_repel(
              data = label_data,
              aes(x = x, y = y, label = label, color = group),
              inherit.aes = FALSE,
              size = 3,
              max.overlaps = Inf,
              min.segment.length = 0,
              box.padding = 0.3,
              point.padding = 0.2,
              show.legend = FALSE
            )
          }
        }
      }

      if (needs_color_scale) {
        p <- p + scale_color_manual(values = palette, guide = "none")
      }
    } else {
      single_color <- resolve_single_color(custom_colors)
      p <- ggplot(df, aes(x = factor(1), y = .data[[var]])) +
        geom_boxplot(fill = single_color, width = 0.3) +
        ta_plot_theme(base_size = base_size) +
        labs(x = NULL, y = var) +
        theme(
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(color = "#9ca3af"),
          axis.ticks = element_line(color = "#9ca3af")
        )

      if (isTRUE(show_points)) {
        p <- p + geom_jitter(color = single_color, width = 0.05, alpha = 0.5, size = 1)
      }

      if (isTRUE(show_outliers)) {
        outliers <- prepare_boxplot_outliers(
          data = df,
          value_col = var,
          label_col = outlier_label_var
        )
        if (has_rows(outliers)) {
          p <- p + geom_point(
            data = outliers,
            aes(x = x, y = y),
            inherit.aes = FALSE,
            color = single_color,
            size = 2.5,
            show.legend = FALSE
          )

          label_data <- filter_labeled_outliers(outliers)
          if (has_rows(label_data)) {
            p <- p + ggrepel::geom_text_repel(
              data = label_data,
              aes(x = x, y = y, label = label),
              inherit.aes = FALSE,
              size = 3,
              color = single_color,
              max.overlaps = Inf,
              min.segment.length = 0,
              box.padding = 0.3,
              point.padding = 0.2,
              show.legend = FALSE
            )
          }
        }
      }
    }

    if (inherits(p, "gg")) p else NULL
  })
  
  # keep only valid ggplots
  plots <- Filter(Negate(is.null), plots)
  if (length(plots) == 0) return(NULL)

  n_panels <- length(plots)
  defaults <- list(
    rows = 1L,
    cols = max(1L, as.integer(n_panels))
  )

  layout <- basic_grid_layout(
    rows = suppressWarnings(as.numeric(nrow_input)),
    cols = suppressWarnings(as.numeric(ncol_input)),
    default_rows = defaults$rows,
    default_cols = defaults$cols,
    max_cols = max(100L, as.integer(defaults$cols))
  )

  validation <- validate_grid(n_panels, layout$nrow, layout$ncol)

  combined <- NULL
  if (isTRUE(validation$valid)) {
    combined <- patchwork::wrap_plots(plots, nrow = layout$nrow, ncol = layout$ncol)

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


has_rows <- function(x) {
  is.data.frame(x) && nrow(x) > 0
}


compute_outlier_bounds <- function(values) {
  if (is.null(values)) return(NULL)
  values <- values[!is.na(values)]
  if (!length(values)) return(NULL)

  stats <- stats::quantile(values, probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE)
  if (anyNA(stats)) return(NULL)

  iqr <- stats[2] - stats[1]
  list(
    lower = stats[1] - 1.5 * iqr,
    upper = stats[2] + 1.5 * iqr
  )
}


clean_outlier_labels <- function(values) {
  if (is.null(values)) return(character())
  out <- as.character(values)
  out[is.na(out) | trimws(out) == ""] <- NA_character_
  out
}


filter_labeled_outliers <- function(outliers) {
  if (!has_rows(outliers) || !"label" %in% names(outliers)) {
    return(NULL)
  }
  labeled <- outliers[!is.na(outliers$label) & nzchar(outliers$label), , drop = FALSE]
  if (has_rows(labeled)) labeled else NULL
}


validate_outlier_label <- function(label_input) {
  if (is.null(label_input) || !nzchar(label_input)) {
    return(NULL)
  }
  label_input
}


prepare_boxplot_outliers <- function(data,
                                     value_col,
                                     group_col = NULL,
                                     label_col = NULL) {
  if (is.null(data) || !is.data.frame(data) || !value_col %in% names(data)) {
    return(NULL)
  }

  extract_labels <- function(df, idx) {
    if (is.null(label_col) || !label_col %in% names(df)) {
      return(rep(NA_character_, length(idx)))
    }
    clean_outlier_labels(df[[label_col]][idx])
  }

  if (!is.null(group_col) && group_col %in% names(data)) {
    grouped <- data
    grouped[[group_col]] <- droplevels(as.factor(grouped[[group_col]]))
    group_levels <- levels(grouped[[group_col]])
    split_data <- split(grouped, grouped[[group_col]], drop = TRUE)

    out_list <- lapply(group_levels, function(lvl) {
      subset <- split_data[[lvl]]
      if (is.null(subset)) return(NULL)

      bounds <- compute_outlier_bounds(subset[[value_col]])
      if (is.null(bounds)) return(NULL)

      idx <- which(subset[[value_col]] < bounds$lower | subset[[value_col]] > bounds$upper)
      if (!length(idx)) return(NULL)

      data.frame(
        x = factor(rep(lvl, length(idx)), levels = group_levels),
        y = subset[[value_col]][idx],
        group = factor(rep(lvl, length(idx)), levels = group_levels),
        label = extract_labels(subset, idx),
        stringsAsFactors = FALSE
      )
    })

    out_list <- Filter(has_rows, out_list)
    if (!length(out_list)) return(NULL)

    outliers <- do.call(rbind, out_list)
    rownames(outliers) <- NULL
    return(outliers)
  }

  bounds <- compute_outlier_bounds(data[[value_col]])
  if (is.null(bounds)) return(NULL)

  idx <- which(data[[value_col]] < bounds$lower | data[[value_col]] > bounds$upper)
  if (!length(idx)) return(NULL)

  data.frame(
    x = factor(rep(1, length(idx))),
    y = data[[value_col]][idx],
    group = NA,
    label = extract_labels(data, idx),
    stringsAsFactors = FALSE
  )
}

