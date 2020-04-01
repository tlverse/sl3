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
library(sl3)
library(testthat)

set.seed(1)

trend_all <- 11:130 + rnorm(120, sd = 2)
trend_all <- data.frame(data = trend_all)

folds <- origami::make_folds(trend_all$data,
  fold_fun = folds_rolling_window, window_size = 50,
  validation_size = 10, gap = 0, batch = 20
)
task <- sl3_Task$new(trend_all, covariates = "data", outcome = "data", folds = folds)

# See which environments reticulate can see:
# reticulate:::conda_list()
# use_condaenv("r-reticulate")
# import("scipy")
# import("tensorflow")

# Install keras:
# install.packages("keras")
# keras::install_keras()

test_that("Lrnr_lstm does what we expect", {
  have_foo <- reticulate::py_module_available("keras.models")

  if (!(have_foo)) {
    skip("keras.models not available for testing")
  } else {
    reticulate::import("keras.models")

    lstm_learner <- Lrnr_lstm$new(epochs = 1)
    lstm_fit <- lstm_learner$train(task)
    lstm_preds <- lstm_fit$predict(task)

    # At epochs=1 (saves time) the prediction is too variable to be tested
    # expect_true(sum(lstm_preds)-28.95605 < 10^(-1))
    expect_equal(length(lstm_preds), nrow(task$X) - 5)
  }
})

test_that("Lrnr_bilstm does what we expect", {
  have_foo <- reticulate::py_module_available("keras.models")

  if (!(have_foo)) {
    skip("keras.models not available for testing")
  } else {
    reticulate::import("keras.models")

    bilstm_learner <- Lrnr_bilstm$new(epochs = 1)
    bilstm_fit <- bilstm_learner$train(task)
    bilstm_preds <- bilstm_fit$predict(task)

    # At epochs=1 (saves time) the prediction is too variable to be tested
    # expect_true(sum(bilstm_preds)-118.5766 < 10^(-1))
    expect_equal(length(bilstm_preds), nrow(task$X) - 5)
  }
})
