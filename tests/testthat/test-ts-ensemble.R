context("test-ts-ensemble.R -- time series methods with SuperLearner")

library(data.table)
library(origami)
library(sl3)
library(R6)
library(delayed)

# Load data
data <- matrix(arima.sim(model = list(ar = c(.9, -.2)), n = 50), nrow = 50, ncol = 1)
data <- as.data.table(data)
names(data) <- "cnt"

folds <- origami::make_folds(data$cnt,
  fold_fun = folds_rolling_window, window_size = 30,
  validation_size = 7, gap = 5, batch = 5
)

# Generate task:
task_ts <- sl3_Task$new(
  data = data, covariates = c(),
  outcome = "cnt", outcome_type = "continuous", folds = folds
)

# Generate library:
lrnr_arima <- Lrnr_arima$new()
lrnr_tsdyn_linear <- Lrnr_tsDyn$new(learner = "linear", m = 1)
lrnr_expsmooth <- Lrnr_expSmooth$new()

stack_ts <- Stack$new(lrnr_arima, lrnr_tsdyn_linear, lrnr_expsmooth)

test_that("Lrnr_cv works with time-series data", {
  cv_stack_ts <- Lrnr_cv$new(stack_ts, full_fit = TRUE)
  fit_cv_ts <- cv_stack_ts$train(task_ts)

  cv_preds_ts <- fit_cv_ts$predict_fold(task_ts, "validation")
  # cv_preds_full_ts <- fit_cv_ts$predict_fold(task_ts, fold_number = "full")

  expect_true(nrow(cv_preds_ts) == 14)
  # expect_true(nrow(cv_preds_full_ts)==50)
  # expect_true(nrow(cv_preds_ts_new)==11)
})

test_that("Lrnr_sl works with time-series data", {
  sl_ts <- make_learner(Lrnr_sl, list(lrnr_arima, lrnr_tsdyn_linear, lrnr_expsmooth))
  fit_ts <- sl_ts$train(task_ts)

  cv_preds_ts <- fit_ts$predict_fold(task_ts, "validation")
  # cv_preds_full_ts <- fit_ts$predict_fold(task_ts, fold_number = "full")

  expect_true(length(cv_preds_ts) == 14)
  # expect_true(length(cv_preds_full_ts)==50)
})
