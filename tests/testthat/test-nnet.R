context("test-nnet.R -- Lrnr_nnet")

set.seed(1)

data(cpp_imputed)
covars <- c("bmi", "parity", "mage", "sexn")
outcome <- "haz"
task <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome)

test_that("Lrnr_nnet gives the correct output for non-binomial outcome", {
  lrnr_nnet <- Lrnr_nnet$new(
    linout = TRUE, size = 10, maxit = 1000,
    trace = FALSE
  )
  fit <- lrnr_nnet$train(task)
  preds <- fit$predict(task)
  expect_equal(task$nrow, length(preds))
})


test_that("Lrnr_nnet gives the correct output for binomial outcome", {
  covars <- c("bmi", "parity", "mage")
  task <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = "sexn")
  lrnr_nnet <- Lrnr_nnet$new(linout = TRUE, size = 10, maxit = 1000, trace = FALSE)
  fit <- lrnr_nnet$train(task)
  preds <- fit$predict(task)
  expect_equal(task$nrow, length(preds))
})
