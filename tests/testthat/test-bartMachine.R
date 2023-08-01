context("test-bartMachine.R -- Lrnr_bartMachine")

library(bartMachine)

test_that("Lrnr_bartMachine produces warning when java parameters are not set", {
  expect_warning(Lrnr_bartMachine$new())
})

data(cpp_imputed)
covs <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
outcome <- "haz"
task <- sl3_Task$new(cpp_imputed, covariates = covs, outcome = outcome)

test_that("Lrnr_bartMachine produces results matching those of bartMachine::bartMachine", {
  # sl3 fit
  lrnr_bartMachine <- suppressWarnings(Lrnr_bartMachine$new(
    seed = 196, verbose = FALSE
  ))
  fit_sl3 <- lrnr_bartMachine$train(task)
  preds_sl3 <- fit_sl3$predict(task)
  rmse_sl3 <- sqrt(mean((preds_sl3 - task$Y)^2))

  # classic fit
  fit_classic <- bartMachine::bartMachine(
    X = data.frame(task$X), y = task$Y, seed = 196, verbose = FALSE
  )
  preds_classic <- as.numeric(predict(fit_classic, new_data = task$X))
  rmse_classic <- sqrt(mean((preds_classic - task$Y)^2))

  # check equality
  expect_equal(rmse_sl3, rmse_classic, tolerance = 0.1)
})

test_that("Lrnr_bartMachine does not fail when cross-validated", {
  lrnr_bartMachine <- suppressWarnings(make_learner(
    Lrnr_bartMachine,
    verbose = FALSE
  ))

  cv_lrnr_bartMachine <- Lrnr_cv$new(lrnr_bartMachine)
  fit_cv <- cv_lrnr_bartMachine$train(task)
  preds_cv <- fit_cv$predict(task)

  expect_equal(length(preds_cv), nrow(task$data))
})
