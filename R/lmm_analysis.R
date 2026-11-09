# ===============================================================
# 🧬 Linear Mixed Model (LMM) — random intercepts
# ===============================================================

lmm_ui <- function(id) regression_ui(id, "lmm", allow_multi_response = TRUE)

lmm_server <- function(id, data) regression_server(id, data, "lmm", allow_multi_response = TRUE)

# ===============================================================
# 🧩 Helpers for LMM
# ===============================================================

compute_icc <- function(model) {
  if (!inherits(model, "merMod")) return(NULL)

  vc <- as.data.frame(lme4::VarCorr(model))
  variances <- vc[is.na(vc$var2), , drop = FALSE]
  total_variance <- sum(variances$vcov)
  random <- variances[variances$grp != "Residual", , drop = FALSE]
  if (nrow(random) == 0 || !is.finite(total_variance) || total_variance <= 0) return(NULL)

  # Each grouping factor contributes a share of the same total variance.
  data.frame(
    Group = random$grp,
    ICC = random$vcov / total_variance,
    stringsAsFactors = FALSE
  )
}
