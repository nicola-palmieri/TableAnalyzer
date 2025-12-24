# ===============================================================
# 🎨 Module for colors customization
# ===============================================================

# ---- Palette ----
basic_color_palette <- c(
  "steelblue" = "#4682B4",
  "red"       = "#FF0000",
  "green"     = "#008000",  # ✅ web green (not ggplot's neon)
  "blue"      = "#0000FF",
  "orange"    = "#FFA500",
  "purple"    = "#800080",
  "brown"     = "#A52A2A",
  "gold"      = "#FFD700",
  "pink"      = "#FF69B4",
  "cyan"      = "#00FFFF",
  "magenta"   = "#FF00FF",
  "yellow"    = "#FFFF00",
  "black"     = "#000000",
  "gray"      = "#808080",
  "darkgreen" = "#006400",
  "darkred"   = "#8B0000"
)

# ---- UI ----
add_color_customization_ui <- function(ns, multi_group = TRUE) {
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
  default_color <- "steelblue"

  # ---- Dynamic UI ----
  output$color_custom_ui <- renderUI({
    req(data())

    color_var <- color_var_reactive() %||% ""
    level_override <- if (is.null(level_order_reactive)) NULL else level_order_reactive()
    reset_id <- if (is.null(reset_token)) NULL else resolve_reactive(reset_token)
    id_suffix <- if (is.null(reset_id)) "" else paste0("_", reset_id)

    # Single color UI shown when multi-group off or no color_var available
    if (!isTRUE(multi_group) || color_var %in% c("", "None")) {
      single_id <- paste0("single_color", id_suffix)
      single_selected <- isolate(input[[single_id]]) %||% default_color
      tagList(
        h5("Color"),
        with_help_tooltip(
          color_dropdown_input(
            ns,
            single_id,
            basic_color_palette,
            ncol = 4,
            selected = single_selected
          ),
          "Choose the colour used for the entire plot."
        )
      )
    } else {
      render_color_inputs(ns, data, color_var, level_override, input, id_suffix = id_suffix)
    }
  })

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

    base_palette <- rep(basic_color_palette, length.out = length(lvls))

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

  default_palette <- rep(basic_color_palette, length.out = length(lvls))

  tagList(
    h5("Colors"),
    lapply(seq_along(lvls), function(i) {
      selected <- default_palette[i]
      if (!is.null(input)) {
        selected <- isolate(input[[paste0("col_", color_var, "_", i, id_suffix)]]) %||% default_palette[i]
      }
      tags$div(
        style = "margin-bottom: 8px;",
        tags$label(lvls[i], style = "display:block; margin-bottom: 4px;"),
        with_help_tooltip(
          color_dropdown_input(
            ns,
            id = paste0("col_", color_var, "_", i, id_suffix),
            palette = basic_color_palette,
            ncol = 4,
            selected = selected
          ),
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
  selected_color <- if (is.null(selected)) palette[1] else selected

  cell_size <- 26
  gap_size <- 4
  padding_size <- 3
  button_size <- cell_size + (2 * padding_size)
  button_height <- button_size
  dropdown_width <- cell_size * ncol + gap_size * (ncol - 1) + 2 * padding_size
  dropdown_top <- button_height + 4

  tagList(
    tags$style(HTML(sprintf(
        "        .color-dropdown {
          position: relative;
          display: inline-block;
          user-select: none;
          width: %dpx;
        }
        .color-dropdown-button {
          width: 100%%;
          border: 1px solid #ccc;
          border-radius: 6px;
          padding: %dpx;
          background-color: #fff;
          display: inline-flex;
          align-items: center;
          justify-content: center;
          cursor: pointer;
          box-shadow: 0 1px 2px rgba(0,0,0,0.05);
        }
        .color-dropdown-swatch {
          flex: 0 0 %dpx;
          width: %dpx;
          height: %dpx;
          border-radius: 4px;
          border: 1px solid #ccc;
          box-sizing: border-box;
        }
        .color-dropdown-grid {
          display: none;
          position: absolute;
          top: %dpx;
          left: 0;
          z-index: 999;
          background: #fff;
          border: 1px solid #ccc;
          border-radius: 6px;
          padding: %dpx;
          box-shadow: 0 4px 12px rgba(0,0,0,0.08);
          width: %dpx;
          grid-template-columns: repeat(%d, %dpx);
          gap: %dpx;
          box-sizing: border-box;
        }
        .color-dropdown-grid.open {
          display: grid;
        }
        .color-cell {
          width: %dpx;
          height: %dpx;
          border-radius: 4px;
          cursor: pointer;
          border: 1px solid #ccc;
          box-sizing: border-box;
          transition: transform 0.1s ease;
        }
        .color-cell:hover {
          transform: scale(1.05);
        }
      ",
      button_size, padding_size, cell_size, cell_size, cell_size,
      dropdown_top, padding_size, dropdown_width, ncol, cell_size, gap_size,
      cell_size, cell_size
    ))),
    tags$div(
      class = "color-dropdown",
      tags$div(
        id = ns(paste0(id, "_button")),
        class = "color-dropdown-button",
        tags$span(
          class = "color-dropdown-swatch",
          style = sprintf("background-color:%s;", selected_color)
        )
      ),
      tags$div(
        id = ns(paste0(id, "_grid")),
        class = "color-dropdown-grid",
        lapply(names(palette), function(col_name) {
          hex <- palette[[col_name]]
          tags$div(
            class = "color-cell",
            title = col_name,  # tooltip shows readable color name
            style = sprintf("background-color:%s;", hex),
            onclick = sprintf(
              "      var button = $('#%s_button');
      button.find('.color-dropdown-swatch').css('background-color','%s');
      $('#%s_grid').removeClass('open');
      Shiny.setInputValue('%s','%s',{priority:'event'});
      ",
              ns(id), hex, ns(id), ns(id), hex
            )
          )
        })

      )
    ),
    tags$script(HTML(sprintf(
        "        $('#%s_button').on('click', function(e){
          e.stopPropagation();
          var grid = $('#%s_grid');
          $('.color-dropdown-grid').not(grid).removeClass('open');
          grid.toggleClass('open');
        });
        $(document).on('click', function(){
          $('.color-dropdown-grid').removeClass('open');
        });
        Shiny.setInputValue('%s','%s',{priority:'event'});
      ",
      ns(id), ns(id), ns(id), selected_color
    )))
  )
}

# ===============================================================
# 🛈 Consistent tooltip helper for UI widgets
# ===============================================================

with_help_tooltip <- function(widget, text) {
  tags$div(
    class = "ta-help-tooltip",
    title = text,
    widget
  )
}

# ===============================================================
# 🎨 Palette resolution helpers
# ===============================================================

resolve_palette_for_levels <- function(levels, custom = NULL) {
  if (is.null(levels) || length(levels) == 0) {
    return(resolve_single_color())
  }

  unique_levels <- unique(as.character(levels))
  palette_size <- length(basic_color_palette)
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

  if (n_levels <= palette_size) {
    palette <- basic_color_palette[seq_len(n_levels)]
  } else {
    repeats <- ceiling(n_levels / palette_size)
    palette <- rep(basic_color_palette, repeats)[seq_len(n_levels)]
  }

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
