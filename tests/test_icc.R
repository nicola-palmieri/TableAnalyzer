library(testthat)

source("../R/lmm_analysis.R")

test_that("ICC uses total variance for single, crossed and nested intercepts", {
  set.seed(321)
  data <- expand.grid(
    site = factor(seq_len(8)),
    subject = factor(seq_len(12)),
    replicate = seq_len(3)
  )
  data$y <- rnorm(8, sd = 2)[data$site] +
    rnorm(12, sd = 3)[data$subject] + rnorm(nrow(data))

  formulas <- list(
    y ~ 1 + (1 | site),
    y ~ 1 + (1 | site) + (1 | subject),
    y ~ 1 + (1 | site / subject)
  )

  for (formula in formulas) {
    model <- lme4::lmer(formula, data)
    variances <- as.data.frame(lme4::VarCorr(model))
    random <- variances[variances$grp != "Residual", ]
    result <- compute_icc(model)

    expect_equal(result$Group, random$grp)
    expect_equal(result$ICC, random$vcov / sum(variances$vcov))
    expect_lte(sum(result$ICC), 1)
  }
})
