context("test-bartMachine.R -- Lrnr_bartMachine")

if (is.null(getOption("java.parameters"))) {
  test_that("Lrnr_bartMachine warns when java parameters are not set", {
    expect_warning(Lrnr_bartMachine$new())
  })
}

test_that("Lrnr_bartMachine produces results matching those of bartMachine::bartMachine", {
  cpp_task <- sl3_Task$new(
    data = cpp_imputed, 
    covariates =  c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn"), 
    outcome = "haz"
  )
  
  # sl3 fit
  set.seed(123)
  lrnr_bartMachine <- suppressWarnings(Lrnr_bartMachine$new(
    seed = 196, verbose = FALSE
  ))
  fit_sl3 <- lrnr_bartMachine$train(cpp_task)
  preds_sl3 <- fit_sl3$predict()
  mse_sl3 <- mean((cpp_task$Y - preds_sl3)^2)
  
  # classic fit
  X <- data.frame(cpp_task$X)
  y <- cpp_task$Y
  fit_classic <- bartMachine::bartMachine(
    X = X, y = y, seed = 196, verbose = FALSE
  )
  preds_classic <- as.numeric(predict(fit_classic, new_data = X))
  mse_classic <- mean((cpp_task$Y - preds_classic)^2)
  
  # check equality
  expect_equal(mse_sl3, mse_classic, tolerance = 0.01)
})

test_that("Lrnr_bartMachine can be cross-validated", {
  cpp_task <- sl3_Task$new(
    data = cpp_imputed, 
    covariates =  c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn"), 
    outcome = "haz",
    folds = 2
  )
  # test Lrnr_bartMachine does not fail when cross-validated
  lrnr_bartMachine <- Lrnr_bartMachine$new(verbose = FALSE)
  cv_lrnr_bartMachine <- Lrnr_cv$new(lrnr_bartMachine)
  fit_cv <- cv_lrnr_bartMachine$train(cpp_task)
  preds_cv <- fit_cv$predict()
  expect_equal(length(preds_cv), nrow(cpp_imputed))
})
