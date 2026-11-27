# Compact TableAnalyzer ordering controls (FR-32). All levels stay included.
level_order_key <- function(var) {
  if (grepl("^[A-Za-z0-9_]+$", var)) var else paste0("x", paste(as.character(as.hexmode(utf8ToInt(var))), collapse = ""))
}

merge_level_order <- function(current, default) {
  if (is.null(current)) return(default)
  c(current[current %in% default], default[!default %in% current])
}

level_order_label <- function(reference = FALSE) {
  if (isTRUE(reference)) "Order of levels (first = reference)" else "Order of levels"
}

mod_level_order_ui <- function(id, slot = NULL) {
  shiny::div(class = "level-order", shiny::uiOutput(shiny::NS(id)(
    if (is.null(slot)) "lists" else paste0("slot_", slot))))
}

# placements: reactive named list slot -> variables shown beneath that selector.
# Repeated placements share one order/reference on this page.
# reference accepts TRUE/FALSE for all placements, or the names of reference slots.
mod_level_order_server <- function(id, variables, placements = NULL, reset = NULL, reference = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    orders <- shiny::reactiveVal(list())
    wired <- character()
    slots <- character()
    if (!is.null(reset)) shiny::observeEvent(reset(), {
      vars <- variables()
      orders(lapply(vars, as.character))
    }, ignoreInit = TRUE)
    places <- shiny::reactive({
      if (is.null(placements)) list(all = names(variables())) else placements()
    })
    shiny::observe({
      vars <- variables()
      old <- shiny::isolate(orders())
      new <- stats::setNames(lapply(names(vars), function(v) {
        merge_level_order(old[[v]], as.character(vars[[v]]))
      }), names(vars))
      if (!identical(new, old)) orders(new)
    })
    render_slot <- function(slot) {
      force(slot)
      has_reference <- if (is.logical(reference)) isTRUE(reference) else slot %in% reference
      shiny::renderUI({
        if (!is.null(reset)) reset()
        ord <- orders()
        vars <- intersect(places()[[slot]], names(ord))
        shiny::tagList(lapply(vars, function(v) {
          shiny::div(class = "level-order-control",
            shiny::selectInput(session$ns(paste0("order_", slot, "_", level_order_key(v))),
              level_order_label(has_reference),
              choices = ord[[v]], selected = ord[[v]], multiple = TRUE)
          )
        }))
      })
    }
    shiny::observe({
      p <- places()
      for (slot in setdiff(names(p), slots)) {
        output[[if (slot == "all") "lists" else paste0("slot_", slot)]] <- render_slot(slot)
        slots <<- c(slots, slot)
      }
      for (slot in names(p)) for (v in p[[slot]]) {
        key <- paste0("order_", slot, "_", level_order_key(v))
        if (key %in% wired) next
        local({
          var <- v
          input_id <- key
          shiny::observeEvent(input[[input_id]], {
            val <- as.character(input[[input_id]])
            cur <- shiny::isolate(orders())
            expected <- cur[[var]]
            # A temporarily removed chip never subsets observations or changes
            # reference: only a complete permutation commits an order.
            if (length(expected) && length(val) == length(expected) &&
                !anyDuplicated(val) && setequal(val, expected) && !identical(val, expected)) {
              cur[[var]] <- val
              orders(cur)
            }
          }, ignoreInit = FALSE)
        })
        wired <<- c(wired, key)
      }
    })
    shiny::reactive(orders())
  })
}
