context("test-bartMachine.R -- Lrnr_bartMachine")
skip_on_cran()

library(bartMachine)
data(cpp_imputed)

covars <- c("bmi", "parity", "mage", "sexn")
outcome <- "haz"
task <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome)

test_that("Lrnr_bartMachine produces results matching those of bartMachine::bartMachine", {
  bartMachine::set_bart_machine_num_cores(1)
  # sl3 fit

  lrnr_bartMachine <- suppressWarnings(Lrnr_bartMachine$new(
    seed = 196, verbose = FALSE
  ))

  set.seed(196)
  fit_sl3 <- lrnr_bartMachine$train(task)
  preds_sl3 <- fit_sl3$predict(task)


  # classic fit
  set.seed(196)
  fit_classic <- bartMachine::bartMachine(
    X = data.frame(task$X), y = task$Y, seed = 196, verbose = F
  )
  preds_classic <- as.numeric(predict(fit_classic, new_data = task$X))

  # check equality
  expect_equal(preds_classic, preds_sl3)
})


test_that("Lrnr_bartMachine does not fail when cross-validated", {
  lrnr_bartMachine <- suppressWarnings(Lrnr_bartMachine$new(verbose = F))
  cv_lrnr_bartMachine <- Lrnr_cv$new(lrnr_bartMachine)
  fit_cv <- cv_lrnr_bartMachine$train(task)
  preds_cv <- fit_cv$predict(task)

  expect_equal(length(preds_cv), task$nrow)
})

