context("test-bartMachine.R -- Lrnr_bartMachine")

library(bartMachine)
data(cpp_imputed)

test_that("Lrnr_bartMachine produces results matching those of bartMachine", {
  cpp_test_task <- sl3_Task$new(
    cpp_imputed,
    covariates = c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs"),
    outcome = "haz"
  )
  # sl3 fit
  lrnr_bartMachine <- suppressWarnings(
    Lrnr_bartMachine$new(seed = 196, verbose = FALSE)
  )
  fit_sl3 <- lrnr_bartMachine$train(cpp_test_task)
  preds_sl3 <- fit_sl3$predict(cpp_test_task)
  rmse_sl3 <- sqrt(mean((preds_sl3 - cpp_test_task$Y)^2))

  # classic fit
  fit_classic <- bartMachine::bartMachine(
    X = data.frame(cpp_test_task$X), y = cpp_test_task$Y, seed = 196, verbose = F
  )
  preds_classic <- as.numeric(predict(fit_classic, new_data = cpp_test_task$X))
  rmse_classic <- sqrt(mean((preds_classic - cpp_test_task$Y)^2))

  # check equality
  expect_equal(rmse_sl3, rmse_classic, tolerance = 0.1)
})

test_that("Lrnr_bartMachine does not fail when cross-validated", {
  cpp_test_task <- sl3_Task$new(
    cpp_imputed,
    covariates = c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs"),
    outcome = "haz"
  )
  lrnr_bartMachine <- suppressWarnings(Lrnr_bartMachine$new(verbose = F))
  cv_lrnr_bartMachine <- Lrnr_cv$new(lrnr_bartMachine)
  fit_cv <- cv_lrnr_bartMachine$train(cpp_test_task)
  preds_cv <- fit_cv$predict(cpp_test_task)

  expect_equal(length(preds_cv), cpp_test_task$nrow)
})
