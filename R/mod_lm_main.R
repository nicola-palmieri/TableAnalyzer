# mod_lm: server coordination and reactive state.
lm_server <- function(id, data, reset_trigger = reactive(NULL)) {
  regression_server(id, data, "lm", allow_multi_response = TRUE, reset_trigger = reset_trigger)
}
