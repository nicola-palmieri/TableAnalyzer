# Shared categorical palette: retain the original initial colours and extend
# deterministically when more categories are requested (2026-11-27 review).
# Used for phyla (bar plot, heatmap phylum strip) and every "colour by" control.
#
# Source: R's built-in "Polychrome 36" (grDevices::palette.colors) without its two
# grey tones (#5A5156, #E4E1E3), first 20 colours. Default special colours:
# Other #BBBBBB, untested #D9D9D9, Unassigned #000000; apps may customise them.
# Not colour-blind safe (no 20-colour palette is); replaces Okabe-Ito (ADR-007).

APP_PALETTE_MAX <- 20L

APP_PALETTE <- local({
  x <- unname(grDevices::palette.colors(palette = "Polychrome 36"))
  x <- x[!toupper(x) %in% c("#5A5156", "#E4E1E3")]
  x[seq_len(APP_PALETTE_MAX)]
})

#' Colours for categorical levels in the given order, with no category-count cap.
app_colours <- function(levels) {
  levels <- unique(as.character(levels))
  n <- length(levels)
  palette <- APP_PALETTE
  if (n > length(palette)) {
    # Golden-angle hues give deterministic extensions without changing earlier
    # assignments when more categories are added. No graphical cardinality limit.
    i <- seq_len(n-length(palette))
    palette <- c(palette,grDevices::hcl(h=(i*137.507764)%%360,c=55,l=65,fixup=TRUE))
  }
  stats::setNames(palette[seq_along(levels)], levels)
}

# Compatibility alias used by the PCoA drawing controls.
PCOA_MAX_COLOUR_LEVELS <- APP_PALETTE_MAX
