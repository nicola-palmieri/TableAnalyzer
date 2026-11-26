# Shared compact color controls for bar plots, alpha diversity and PCoA.
barplot_color_palette <- c(
  steelblue = '#4682B4', red = '#FF0000', green = '#008000', blue = '#0000FF',
  orange = '#FFA500', purple = '#800080', brown = '#A52A2A', gold = '#FFD700',
  pink = '#FF69B4', cyan = '#00FFFF', magenta = '#FF00FF', yellow = '#FFFF00',
  black = '#000000', gray = '#808080', darkgreen = '#006400', darkred = '#8B0000'
)

barplot_color_dropdown <- function(ns, id = 'color_choice', palette = barplot_color_palette,
                                 ncol = 4, selected = NULL) {
  selected_color <- if (is.null(selected)) unname(palette[1]) else selected
  shiny::div(class = 'color-dropdown', `data-input-id` = ns(id),
    `data-selected` = selected_color, `data-columns` = ncol,
    style = sprintf('--color-columns:%d;', ncol),
    shiny::tags$button(type = 'button', id = ns(paste0(id, '_button')),
      class = 'color-dropdown-button', `aria-label` = paste('Choose colour; current', selected_color),
      `aria-haspopup` = 'menu', `aria-expanded` = 'false',
      `aria-controls` = ns(paste0(id, '_grid')),
      shiny::span(class = 'color-dropdown-swatch', `aria-hidden` = 'true',
        style = sprintf('background-color:%s;', selected_color))),
    shiny::div(id = ns(paste0(id, '_grid')), class = 'color-dropdown-grid',
      role = 'menu', `aria-label` = 'Colour choices',
      lapply(names(palette), function(col_name) {
        hex <- palette[[col_name]]
        shiny::tags$button(type = 'button', class = 'color-cell', role = 'menuitemradio',
          tabindex = '-1', title = col_name, `aria-label` = paste(col_name, hex),
          `aria-checked` = if (identical(unname(hex), unname(selected_color))) 'true' else 'false',
          `data-color` = hex, style = sprintf('background-color:%s;', hex))
      })))
}
