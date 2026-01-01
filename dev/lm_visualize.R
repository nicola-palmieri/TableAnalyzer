# ===============================================================
# 🧪 Visualization Module — LM / LMM
# ===============================================================

visualize_lm_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      class = "ta-sidebar",
      width = 4,
      h4("Step 4 — Visualize linear model"),
      p("Simple visualizations for LM/LMM results. For complex models, use the coefficient forest."),
      hr(),
      selectInput(
        ns("plot_type"),
        "Select visualization type:",
        choices = c(
          "Auto (best effort)"   = "auto",
          "Scatter + lm fit"     = "scatter",
          "Coefficient forest"   = "forest"
        ),
        selected = "auto"
      ),
      # Dynamic sub-controls depending on plot type
      uiOutput(ns("sub_controls")),
      # Layout controls (strata/response grids), same pattern as ANOVA
      uiOutput(ns("layout_controls")),
      fluidRow(
        column(6, numericInput(ns("plot_width"),  "Subplot width (px)",  value = 400, min = 200, max = 2000, step = 50)),
        column(6, numericInput(ns("plot_height"), "Subplot height (px)", value = 300, min = 200, max = 2000, step = 50))
      ),
      # Color customization; auto-switch between single / grouped
      add_color_customization_ui(ns, multi_group = TRUE),  # server decides when it's single/group
      br(),
      downloadButton(ns("download_plot"), "Download plot", style = "width: 100%;")
    ),
    mainPanel(
      width = 8,
      h4("Plots"),
      plotOutput(ns("plot"), height = "auto")
    )
  )
}

visualize_lm_server <- function(id, filtered_data, model_fit) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    df  <- reactive(filtered_data())
    mod <- reactive(model_fit())   # regression_server output (has fits/flat_models/attr(engine)) :contentReference[oaicite:6]{index=6}
    
    layout_state <- initialize_layout_state(input, session)  # shared layout mgmt :contentReference[oaicite:7]{index=7}
    
    # ---- Extract first successful model and its terms ----
    primary_model <- reactive({
      m <- mod()
      if (is.null(m) || is.null(m$flat_models) || length(m$flat_models) == 0) return(NULL)
      # take the first available fitted object
      fm <- m$flat_models[[1]]$model
      if (is.null(fm)) return(NULL)
      fm
    })
    
    # Determine a sensible default: 1 numeric x (if present)
    default_numeric_x <- reactive({
      mobj <- primary_model()
      dat  <- df()
      if (is.null(mobj) || is.null(dat)) return(NULL)
      tl   <- attr(terms(mobj), "term.labels")
      if (is.null(tl) || length(tl) == 0) return(NULL)
      num_vars <- names(dat)[vapply(dat, is.numeric, logical(1))]
      cand <- intersect(tl, num_vars)
      if (length(cand) > 0) cand[1] else NULL
    })
    
    # Determine a simple grouping factor if one exists among terms
    default_group <- reactive({
      mobj <- primary_model()
      dat  <- df()
      if (is.null(mobj) || is.null(dat)) return(NULL)
      tl   <- attr(terms(mobj), "term.labels")
      if (is.null(tl) || length(tl) == 0) return(NULL)
      fac_vars <- names(dat)[vapply(dat, function(x) is.factor(x) || is.character(x), logical(1))]
      cand <- intersect(tl, fac_vars)
      if (length(cand) > 0) cand[1] else NULL
    })
    
    # ---- Sub-controls depending on plot type ----
    output$sub_controls <- renderUI({
      data <- df()
      mobj <- primary_model()
      if (is.null(data) || nrow(data) == 0 || is.null(mobj)) {
        return(helpText("Run the LM/LMM analysis first."))
      }
      
      if (identical(input$plot_type, "scatter")) {
        num_vars <- names(data)[vapply(data, is.numeric, logical(1))]
        fac_vars <- names(data)[vapply(data, function(x) is.factor(x) || is.character(x), logical(1))]
        tagList(
          selectInput(ns("xvar"), "X (numeric, in model):",
                      choices = intersect(num_vars, attr(terms(mobj), "term.labels")),
                      selected = default_numeric_x()),
          selectInput(ns("group"), "Group (optional, in model):",
                      choices = c("None", intersect(fac_vars, attr(terms(mobj), "term.labels"))),
                      selected = if (is.null(default_group())) "None" else default_group())
        )
      } else {
        NULL
      }
    })
    
    # ---- Color customization: auto single/group based on active grouping ----
    color_var_reactive <- reactive({
      # Only relevant for scatter if grouping present; forest always multi-color by default palette
      if (identical(input$plot_type, "scatter")) {
        g <- input$group
        if (!is.null(g) && nzchar(g) && !identical(g, "None")) return(g)
      }
      NULL
    })
    
    custom_colors <- add_color_customization_server(
      ns = ns, input = input, output = output,
      data = df,
      color_var_reactive = color_var_reactive,
      multi_group = TRUE
    )  # returns either a single color or a named palette, reusing your helper :contentReference[oaicite:8]{index=8}
    
    # ---- Build per-(response × stratum) plots from the regression result ----
    # Helpers
    
    model_grid <- reactive({
      m <- mod()
      if (is.null(m) || is.null(m$fits)) return(NULL)
      # fits[[response]]$strata[[k]]$model  (or single Overall)  :contentReference[oaicite:9]{index=9}
      m$fits
    })
    
    # Create a coefficient-forest plot for one model
    build_forest_one <- function(model_obj, title = NULL) {
      tb <- tryCatch(broom::tidy(model_obj, conf.int = TRUE), error = function(e) NULL)
      if (is.null(tb)) return(NULL)
      
      # keep fixed effects; drop intercept and random-structure rows by pattern
      keep <- !grepl("^\\(Intercept\\)$", tb$term)
      tb <- tb[keep, , drop = FALSE]
      if (!all(c("estimate","conf.low","conf.high") %in% names(tb))) return(NULL)
      
      tb$term <- factor(tb$term, levels = rev(tb$term))
      p <- ggplot(tb, aes(x = .data$estimate, y = .data$term)) +
        geom_point() +
        geom_errorbarh(aes(xmin = .data$conf.low, xmax = .data$conf.high), height = 0) +
        geom_vline(xintercept = 0, linetype = 2, linewidth = 0.3, color = "grey40") +
        theme_minimal(base_size = 13) +
        labs(x = "Estimate (95% CI)", y = NULL)
      if (!is.null(title)) p <- p + ggplot2::labs(title = title)
      p
    }
    
    # Create a scatter+fit plot for one response using X (+ optional group)
    build_scatter_fit_one <- function(data, response, xvar, group = NULL, title = NULL, line_colors = NULL) {
      if (is.null(data) || nrow(data) == 0) return(NULL)
      if (is.null(response) || is.null(xvar)) return(NULL)
      if (!is.numeric(data[[xvar]])) return(NULL)
      
      # drop NA rows for plotting
      keep <- is.finite(data[[response]]) & is.finite(data[[xvar]])
      dd <- data[keep, c(response, xvar, group), drop = FALSE]
      if (nrow(dd) == 0) return(NULL)
      
      # aesthetics
      if (!is.null(group) && nzchar(group) && group %in% names(dd)) {
        dd[[group]] <- droplevels(factor(dd[[group]]))
        palette <- if (!is.null(line_colors) && length(line_colors) > 0) {
          resolve_palette_for_levels(levels(dd[[group]]), custom = line_colors)
        } else {
          resolve_palette_for_levels(levels(dd[[group]]))
        }
        ggplot(dd, aes(x = .data[[xvar]], y = .data[[response]], color = .data[[group]])) +
          geom_point(alpha = 0.6) +
          geom_smooth(method = "lm", se = TRUE) +
          scale_color_manual(values = palette) +
          theme_minimal(base_size = 13) +
          labs(x = xvar, y = response, color = group, title = title)
      } else {
        col <- if (is.character(line_colors)) line_colors else resolve_single_color()
        ggplot(dd, aes(x = .data[[xvar]], y = .data[[response]])) +
          geom_point(alpha = 0.6, color = col) +
          geom_smooth(method = "lm", se = TRUE, color = col) +
          theme_minimal(base_size = 13) +
          labs(x = xvar, y = response, title = title)
      }
    }
    
    # Choose "best effort" visualization for one model and response
    pick_auto_plot <- function(model_obj, data, response, line_colors) {
      tl <- attr(terms(model_obj), "term.labels")
      if (is.null(tl) || length(tl) == 0) {
        # intercept-only → forest (trivial)
        return(build_forest_one(model_obj, response))
      }
      num_vars <- names(data)[vapply(data, is.numeric, logical(1))]
      fac_vars <- names(data)[vapply(data, function(x) is.factor(x) || is.character(x), logical(1))]
      
      x_cand  <- intersect(tl, num_vars)
      g_cand  <- intersect(tl, fac_vars)
      
      if (length(x_cand) >= 1 && length(g_cand) <= 1) {
        x <- x_cand[1]
        g <- if (length(g_cand) == 1) g_cand[1] else NULL
        return(build_scatter_fit_one(data, response, x, g, response, line_colors))
      }
      
      if (length(x_cand) == 0 && length(g_cand) >= 1) {
        # all factors → show group means ± SE for the first factor
        fac <- g_cand[1]
        dd <- data[, c(response, fac), drop = FALSE]
        dd[[fac]] <- droplevels(factor(dd[[fac]]))
        stats <- dd |>
          dplyr::group_by(.data[[fac]]) |>
          dplyr::summarise(
            mean = mean(.data[[response]], na.rm = TRUE),
            se   = stats::sd(.data[[response]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[response]]))),
            .groups = "drop"
          )
        palette <- resolve_palette_for_levels(levels(dd[[fac]]))
        return(
          ggplot(stats, aes(x = .data[[fac]], y = .data$mean, group = 1)) +
            geom_line(color = palette[1]) +
            geom_point(size = 3, color = palette[1]) +
            geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.15, color = palette[1]) +
            theme_minimal(base_size = 13) +
            labs(x = fac, y = "Mean ± SE", title = response)
        )
      }
      
      # fallback → forest
      build_forest_one(model_obj, response)
    }
    
    # ---- Generate the full plot grid (by strata × response) ----
    plot_info <- reactive({
      info <- mod()
      data <- df()
      validate(need(!is.null(info) && !is.null(info$fits), "No LM/LMM results to visualize."))
      validate(need(!is.null(data) && nrow(data) > 0, "No data available for plotting."))
      
      fits <- model_grid()
      responses <- names(fits)
      
      # detect strata variable + order if present (same pattern as other viz modules)
      has_strata <- FALSE
      strata_levels <- NULL
      # Find the first response entry to probe
      if (length(responses) > 0) {
        fe <- fits[[responses[1]]]
        has_strata <- isTRUE(fe$stratified)
        if (has_strata) {
          strata_levels <- vapply(fe$strata, function(s) s$display, character(1))
        }
      }
      
      # layout assembly
      make_panel <- function(response, stratum_label = NULL, model_obj) {
        if (is.null(model_obj)) return(NULL)
        
        # decide the active plot type
        ptype <- input$plot_type
        line_colors <- custom_colors()
        
        if (identical(ptype, "forest")) {
          return(build_forest_one(model_obj, if (is.null(stratum_label)) response else paste(response, "—", stratum_label)))
        }
        
        if (identical(ptype, "scatter")) {
          x  <- input$xvar %||% default_numeric_x()
          gp <- input$group
          if (!is.null(gp) && identical(gp, "None")) gp <- NULL
          return(build_scatter_fit_one(
            data = data, response = response, xvar = x, group = gp,
            title = if (is.null(stratum_label)) response else paste(response, "—", stratum_label),
            line_colors = line_colors
          ))
        }
        
        # auto
        pick_auto_plot(model_obj, data, response,
                       line_colors = custom_colors())
      }
      
      # build panels
      panels <- list()
      if (!has_strata) {
        for (resp in responses) {
          mobj <- fits[[resp]]$strata[[1]]$model
          p <- make_panel(resp, NULL, mobj)
          if (!is.null(p)) panels[[resp]] <- p
        }
      } else {
        # build nested list: names = strata, inside multiple responses
        for (resp in responses) {
          for (s in fits[[resp]]$strata) {
            if (is.null(s$model)) next
            label <- s$display
            key   <- paste(label, resp, sep = " :: ")
            panels[[key]] <- make_panel(resp, label, s$model)
          }
        }
      }
      
      panels <- Filter(Negate(is.null), panels)
      validate(need(length(panels) > 0, "No plot could be generated for the fitted models."))
      
      # grid layout (responses × strata) via common helper
      layout <- resolve_grid_layout(
        n_items   = length(panels),
        rows_input = layout_state$effective_input("resp_rows"),
        cols_input = layout_state$effective_input("resp_cols")
      )
      
      combined <- patchwork::wrap_plots(plotlist = panels, nrow = layout$nrow, ncol = layout$ncol) +
        patchwork::plot_layout(guides = "collect")
      
      list(
        plot = combined,
        layout = layout
      )
    })
    
    observe_layout_synchronization(plot_info, layout_state, session)  # keep inputs in sync with computed layout :contentReference[oaicite:10]{index=10}
    
    # ---- Layout controls UI (mirrors ANOVA helpers) ----
    output$layout_controls <- renderUI({
      # We reuse the simple grid controls (responses grid). Use same labels as your shared ANOVA UI.
      tagList(
        h5("Grid layout:"),
        fluidRow(
          column(6, numericInput(ns("resp_rows"), "Grid rows",  value = isolate(layout_state$default_ui_value(input$resp_rows)), min = 1, max = 10, step = 1)),
          column(6, numericInput(ns("resp_cols"), "Grid columns", value = isolate(layout_state$default_ui_value(input$resp_cols)), min = 1, max = 10, step = 1))
        )
      )
    })
    
    # ---- Render & download ----
    plot_obj <- reactive({
      info <- plot_info()
      validate(need(!is.null(info$plot), "No LM/LMM plot available."))
      info$plot
    })
    
    plot_size <- reactive({
      info <- plot_info()
      lw <- suppressWarnings(as.numeric(input$plot_width));  if (is.na(lw) || lw <= 0) lw <- 400
      lh <- suppressWarnings(as.numeric(input$plot_height)); if (is.na(lh) || lh <= 0) lh <- 300
      list(w = lw * max(1, info$layout$ncol), h = lh * max(1, info$layout$nrow))
    })
    
    output$plot <- renderPlot({
      req(plot_obj())
      plot_obj()
    },
    width  = function() plot_size()$w,
    height = function() plot_size()$h,
    res = 96)
    
    output$download_plot <- downloadHandler(
      filename = function() paste0("lm_plot_", Sys.Date(), ".png"),
      content = function(file) {
        p <- plot_obj(); s <- plot_size()
        ggsave(file, plot = p, device = "png", dpi = 300,
               width = s$w / 96, height = s$h / 96, units = "in", limitsize = FALSE)
      }
    )
  })
}
