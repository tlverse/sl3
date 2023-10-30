context("test-arima-tsDyn.R -- ARIMA and tsDyn")

library(origami)
library(data.table)
library(testthat)

set.seed(1)
attach(list(lag = stats::lag), name = "stats_lag_test_kludge", warn.conflicts = FALSE)
data(bsds)
bsds <- bsds[1:50, ]

data <- as.data.table(bsds)
data[, time := .I]

outcome <- "cnt"

folds <- origami::make_folds(data,
  fold_fun = folds_rolling_window, window_size = 20,
  validation_size = 15, gap = 0, batch = 10
)

node_list <- list(outcome = outcome, time = "time")

task <- sl3_Task$new(data, nodes = node_list, folds = folds)
train_task <- training(task, fold = task$folds[[1]])
valid_task <- validation(task, fold = task$folds[[1]])


test_that("Lrnr_arima gives expected values with auto.arima", {
  arima_learner <- Lrnr_arima$new()
  arima_fit <- arima_learner$train(train_task)
  arima_preds <- arima_fit$predict(valid_task)

  arima_fit_2 <- forecast::auto.arima(train_task$Y)
  arima_preds_2 <- predict(arima_fit_2)
  arima_preds_2 <- as.numeric(arima_preds_2$pred)
  arima_preds_2 <- structure(arima_preds_2, names = 1)

  expect_true(sum(arima_preds[1] - arima_preds_2) < 10^-1)
})

test_that("Lrnr_arima gives expected values with arima order set", {
  arima_learner <- Lrnr_arima$new(order = c(3, 1, 6))
  arima_fit <- arima_learner$train(train_task)
  arima_preds <- arima_fit$predict(valid_task)

  arima_fit_2 <- arima(train_task$Y, order = c(3, 1, 6))
  arima_preds_2 <- predict(arima_fit_2)
  arima_preds_2 <- as.numeric(arima_preds_2$pred)
  arima_preds_2 <- structure(arima_preds_2, names = 1)

  expect_true(sum(arima_preds[1] - arima_preds_2) < 10^-1)
})

test_that("Lrnr_tsDyn with multiple different models, univariate", {

  # AR(m) model
  tsDyn_learner <- Lrnr_tsDyn$new(learner = "linear", m = 1)
  fit_1 <- tsDyn_learner$train(train_task)
  fit_1_preds <- fit_1$predict(valid_task)

  fit_2 <- tsDyn::linear(train_task$Y, m = 1)
  fit_2_preds <- predict(fit_2)
  fit_2_preds <- as.numeric(fit_2_preds)
  fit_2_preds <- structure(fit_2_preds)

  expect_true(sum(fit_1_preds[1] - fit_2_preds) < 10^-1)

  # Logistic Smooth Transition autoregressive model
  tsDyn_learner <- Lrnr_tsDyn$new(learner = "lstar", m = 1, trace = FALSE)
  fit_1 <- tsDyn_learner$train(train_task)
  fit_1_preds <- fit_1$predict(valid_task)

  fit_2 <- tsDyn::lstar(train_task$Y, m = 1, trace = FALSE)
  fit_2_preds <- predict(fit_2)
  fit_2_preds <- as.numeric(fit_2_preds)
  fit_2_preds <- structure(fit_2_preds)

  expect_true(sum(fit_1_preds[1] - fit_2_preds) < 10^-1)
})

test_that("Lrnr_tsDyn with multiple different models, multivariate", {

  # Estimate multivariate threshold VAR

  # Define new data:
  covars <- c("temp", "atemp")
  outcome <- c("temp", "atemp")

  data <- bsds[1:50, c("temp", "atemp")]

  task <- sl3_Task$new(data, covariates = covars, outcome = outcome, folds = folds)
  train_task <- training(task, fold = task$folds[[1]])
  valid_task <- validation(task, fold = task$folds[[1]])


  tsDyn_learner <- Lrnr_tsDyn$new(learner = "lineVar", lag = 2)
  fit_1 <- tsDyn_learner$train(train_task)
  fit_1_preds <- fit_1$predict(valid_task)

  fit_2 <- tsDyn::lineVar(train_task$X, lag = 2)
  fit_2_preds <- predict(fit_2)
  fit_2_preds <- as.numeric(fit_2_preds)
  fit_2_preds <- structure(fit_2_preds)

  expect_true(sum(fit_1_preds[1] - fit_2_preds[1]) < 10^-1)

  # Estimation of Vector error correction model (VECM)
  tsDyn_learner <- Lrnr_tsDyn$new(learner = "VECM", lag = 2, type = "linear")
  params <- tsDyn_learner$params
  fit_1 <- tsDyn_learner$train(train_task)
  fit_1_preds <- fit_1$predict(valid_task)

  fit_2 <- tsDyn::VECM(task$X, lag = 2)
  fit_2_preds <- predict(fit_2)
  fit_2_preds <- as.numeric(fit_2_preds)
  fit_2_preds <- structure(fit_2_preds)

  expect_true(sum(fit_1_preds[1] - fit_2_preds[1]) < 10^-1)

  # Multivariate Threshold autoregressive model (TVAR)
  # tsDyn_learner <- Lrnr_tsDyn$new(learner="TVAR", lag=2, model="TAR", thDelay=1, trim=0.1)
  # params<-tsDyn_learner$params
  # fit_1 <- tsDyn_learner$train(task)

  # fit_2 <- tsDyn::TVAR(task$X, lag=2)
})

test_that("Lrnr_arima with external regressors", {

  # Define new data:
  data <- bsds
  data$atemp2 <- data$atemp
  covars <- c("atemp", "casual", "registered")
  covars_dups <- c("casual", "registered", "atemp2", "atemp")
  outcome <- c("temp")
  task <- sl3_Task$new(
    data,
    covariates = covars, outcome = outcome, folds = folds
  )
  task_duplicate_covs <- sl3_Task$new(
    data,
    covariates = covars_dups, outcome = outcome, folds = folds
  )
  train_task <- training(task, fold = task$folds[[1]])
  valid_task <- validation(task, fold = task$folds[[1]])
  valid_task_duplicate_covs <- validation(task_duplicate_covs, task$folds[[2]])

  arima_lrnr <- Lrnr_arima$new()
  fit <- arima_lrnr$train(train_task)
  preds <- fit$predict(valid_task)
  expect_warning(preds_newX <- fit$predict(valid_task_duplicate_covs))

  cv_arima_lrnr <- Lrnr_cv$new(arima_lrnr)
  fit_cv <- cv_arima_lrnr$train(task)
  preds_cv_newX <- fit_cv$predict(task_duplicate_covs)

  node_list <- list(outcome = outcome)
  task_no_covs <- sl3_Task$new(data, nodes = node_list, folds = folds)
  train_task_no_covs <- training(task_no_covs, fold = task_no_covs$folds[[1]])
  fit_no_covs <- arima_lrnr$train(train_task_no_covs)
  expect_warning(fit_no_covs$predict(valid_task))
})
