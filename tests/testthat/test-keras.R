context("test-keras.R -- time series methods")

if (FALSE) {
  setwd("..")
  setwd("..")
  getwd()
  library("devtools")
  document()
  load_all("./") # load all R files in /R and datasets in /data. Ignores NAMESPACE:
  # devtools::check() # runs full check
  setwd("..")
  install("sl3", build_vignettes = FALSE, dependencies = FALSE) # INSTALL W/ devtools:
}

library(reticulate)
set.seed(1)

skip_if_no_foo <- function() {
  have_foo <- reticulate::py_module_available("keras.models")

  if (!have_foo) {
    skip("keras.models not available for testing")
  } else {
    reticulate::import("keras.models")
  }
}

trend_all <- 11:130 + rnorm(120, sd = 2)
trend_all <- data.frame(data = trend_all)
task <- sl3_Task$new(trend_all, covariates = "data", outcome = "data")

test_that("Lrnr_lstm does what we expect", {
  skip_if_no_foo()
  lstm_learner <- Lrnr_lstm$new(epochs = 1)
  lstm_fit <- lstm_learner$train(task)
  lstm_preds <- lstm_fit$predict(task)

  # expect_true(sum(lstm_preds)-28.95605 < 10^(-1))
  expect_equal(length(lstm_preds), nrow(task$X) - 5)
})

test_that("Lrnr_bilstm does what we expect", {
  skip_if_no_foo()
  bilstm_learner <- Lrnr_bilstm$new(epochs = 1)
  bilstm_fit <- bilstm_learner$train(task)
  bilstm_preds <- bilstm_fit$predict(task)

  # expect_true(sum(bilstm_preds)-118.5766 < 10^(-1))
  expect_equal(length(bilstm_preds), nrow(task$X) - 5)
})
