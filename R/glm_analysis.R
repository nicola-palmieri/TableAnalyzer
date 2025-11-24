# ===============================================================
# 🔬 Generalized Linear Model (GLM)
# ===============================================================

glm_ui <- function(id) regression_ui(id, "glm", allow_multi_response = FALSE, compact = TRUE)

glm_server <- function(id, data) regression_server(id, data, "glm", allow_multi_response = FALSE, compact = TRUE)
