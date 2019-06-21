context("test-gbm.R -- Lrnr_gbm")

library(gbm)
data(cpp_imputed)
covars <- c(
  "apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs",
  "sexn"
)
outcome <- "haz"
task <- sl3_Task$new(cpp_imputed,
  covariates = covars,
  outcome = outcome
)

test_that("Lrnr_gbm produces results matching those of gbm::gbm.fit", {
  # get predictions from Lrnr_* wrapper
  set.seed(4738)
  lrnr_gbm <- make_learner(Lrnr_gbm)
  fit <- lrnr_gbm$train(task)
  preds <- fit$predict(task)

  # get predictions from classic implementation
  set.seed(4738)
  fit_classic <- gbm::gbm.fit(
    x = task$X, y = task$Y, n.trees = 10000,
    interaction.depth = 2, shrinkage = 0.001,
    distribution = "gaussian", verbose = FALSE
  )
  preds_classic <- predict(fit_classic,
    newdata = task$X, type = "response",
    n.trees = 10000
  )

  # check equality of predictions
  expect_equal(preds, preds_classic)
})
