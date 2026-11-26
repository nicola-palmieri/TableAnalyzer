# One colour palette for the whole app (decided by the researcher on 2026-11-24:
# "just always use a palette that supports up to 20 colors for simplicity").
# Used for phyla (bar plot, heatmap phylum strip) and every "colour by" control.
#
# Source: R's built-in "Polychrome 36" (grDevices::palette.colors) without its two
# grey tones (#5A5156, #E4E1E3), first 20 colours. Greys are reserved: "Other"
# #BBBBBB, not tested (heatmap) #D9D9D9; black #000000 is "Unassigned".
# Not colour-blind safe (no 20-colour palette is); replaces Okabe-Ito (ADR-007).

APP_PALETTE_MAX <- 20L

APP_PALETTE <- local({
  x <- unname(grDevices::palette.colors(palette = "Polychrome 36"))
  x <- x[!toupper(x) %in% c("#5A5156", "#E4E1E3")]
  x[seq_len(APP_PALETTE_MAX)]
})

#' Colours for categorical levels, in the given order (at most 20).
#' Returns a named character vector; stops with a user_error above 20 levels.
app_colours <- function(levels) {
  levels <- unique(as.character(levels))
  if (length(levels) > APP_PALETTE_MAX) {
    stop(structure(class = c("user_error", "error", "condition"), list(
      message = sprintf("%d groups, but at most %d colours are available.", length(levels), APP_PALETTE_MAX),
      call = NULL)))
  }
  stats::setNames(APP_PALETTE[seq_along(levels)], levels)
}

# Compatibility alias used by the PCoA drawing controls.
PCOA_MAX_COLOUR_LEVELS <- APP_PALETTE_MAX
