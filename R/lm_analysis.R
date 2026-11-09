# ===============================================================
# 🧮 Linear Model (LM) — fixed effects only
# ===============================================================

lm_ui <- function(id) regression_ui(id, "lm", allow_multi_response = TRUE)

lm_server <- function(id, data, reset_trigger = reactive(NULL)) {
  regression_server(id, data, "lm", allow_multi_response = TRUE, reset_trigger = reset_trigger)
}
