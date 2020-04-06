context("test-arima.R -- ARIMA and tsDyn")

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

library(origami)

set.seed(1)
attach(list(lag = stats::lag), name = "stats_lag_test_kludge", warn.conflicts = FALSE)
data(bsds)

data <- as.data.table(bsds$cnt)
data <- data[1:50, ]
data[, time := .I]
names(data)[1] <- "cnt"

covars <- c("cnt")
outcome <- "cnt"

folds <- origami::make_folds(data,
  fold_fun = folds_rolling_window, window_size = 20,
  validation_size = 15, gap = 0, batch = 10
)

node_list <- list(outcome = outcome, covariates = covars, time = "time")
task <- sl3_Task$new(data, nodes = node_list, folds = folds)

test_that("Lrnr_arima gives expected values with auto.arima", {
  arima_learner <- Lrnr_arima$new()
  arima_fit <- arima_learner$train(task)
  arima_preds <- arima_fit$predict(task)

  arima_fit_2 <- forecast::auto.arima(data$cnt)
  arima_preds_2 <- predict(arima_fit_2)
  arima_preds_2 <- as.numeric(arima_preds_2$pred)
  arima_preds_2 <- structure(arima_preds_2, names = 1)

  expect_true(sum(arima_preds[1] - arima_preds_2) < 10^-1)
})

test_that("Lrnr_arima gives expected values with arima order set", {
  arima_learner <- Lrnr_arima$new(order = c(3, 1, 6))
  arima_fit <- arima_learner$train(task)
  arima_preds <- arima_fit$predict(task)

  arima_fit_2 <- arima(data$cnt, order = c(3, 1, 6))
  arima_preds_2 <- predict(arima_fit_2)
  arima_preds_2 <- as.numeric(arima_preds_2$pred)
  arima_preds_2 <- structure(arima_preds_2, names = 1)

  expect_true(sum(arima_preds[1] - arima_preds_2) < 10^-1)
})

test_that("Lrnr_tsDyn with multiple different models, univariate", {

  # AR(m) model
  tsDyn_learner <- Lrnr_tsDyn$new(learner = "linear", m = 1)
  fit_1 <- tsDyn_learner$train(task)
  fit_1_preds <- fit_1$predict(task)

  fit_2 <- tsDyn::linear(data$cnt, m = 1)
  fit_2_preds <- predict(fit_2)
  fit_2_preds <- as.numeric(fit_2_preds)
  fit_2_preds <- structure(fit_2_preds)

  expect_true(sum(fit_1_preds[1] - fit_2_preds) < 10^-1)

  # Logistic Smooth Transition autoregressive model
  tsDyn_learner <- Lrnr_tsDyn$new(learner = "lstar", m = 1)
  fit_1 <- tsDyn_learner$train(task)
  fit_1_preds <- fit_1$predict(task)

  fit_2 <- tsDyn::lstar(data$cnt, m = 1)
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

  tsDyn_learner <- Lrnr_tsDyn$new(learner = "lineVar", lag = 2)
  fit_1 <- tsDyn_learner$train(task)
  fit_1_preds <- fit_1$predict(task)

  fit_2 <- tsDyn::lineVar(task$X, lag = 2)
  fit_2_preds <- predict(fit_2)
  fit_2_preds <- as.numeric(fit_2_preds)
  fit_2_preds <- structure(fit_2_preds)

  expect_true(sum(fit_1_preds[1] - fit_2_preds[1]) < 10^-1)

  # Estimation of Vector error correction model (VECM)
  tsDyn_learner <- Lrnr_tsDyn$new(learner = "VECM", lag = 2, type = "linear")
  params <- tsDyn_learner$params
  fit_1 <- tsDyn_learner$train(task)
  fit_1_preds <- fit_1$predict(task)

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
