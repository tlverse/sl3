context("test-garch.R -- Lrnr_rugarch")

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

options(rgl.useNULL = TRUE)
set.seed(1)

data(bsds)
covars <- c("cnt")
outcome <- "cnt"

folds <- origami::make_folds(bsds,
  fold_fun = folds_rolling_window, window_size = 50,
  validation_size = 10, gap = 0, batch = 200
)
task <- sl3_Task$new(bsds, covariates = covars, outcome = outcome, folds = folds)

test_that("Lrnr_rugarch gives expected values with no model specification", {
  garch_learner <- Lrnr_rugarch$new()
  garch_fit <- garch_learner$train(task)
  garch_preds <- garch_fit$predict(task)

  garch_spec <- rugarch::ugarchspec()
  garch_fit_2 <- rugarch::ugarchfit(garch_spec, bsds$cnt)
  garch_preds_2 <- rugarch::ugarchforecast(garch_fit_2, n.ahead = 1)
  garch_preds_2 <- as.numeric(garch_preds_2@forecast$seriesFor)
  garch_preds_2 <- structure(garch_preds_2, names = 1)

  expect_true(sum(garch_preds[1] - garch_preds_2) < 10^(-1))
})

test_that("Lrnr_rugarch gives expected values with higher order ARMA and GARCH", {
  garch_learner <- Lrnr_rugarch$new(
    variance.model = list(model = "sGARCH", garchOrder = c(2, 2)),
    mean.model = list(armaOrder = c(2, 2), include.mean = TRUE),
    distribution.model = "sstd"
  )
  garch_fit <- garch_learner$train(task)
  garch_preds <- garch_fit$predict(task)

  garch_spec <- rugarch::ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(2, 2)),
    mean.model = list(armaOrder = c(2, 2), include.mean = TRUE), distribution.model = "sstd"
  )
  garch_fit_2 <- rugarch::ugarchfit(garch_spec, bsds$cnt)
  garch_preds_2 <- rugarch::ugarchforecast(garch_fit_2, n.ahead = 1)
  garch_preds_2 <- as.numeric(garch_preds_2@forecast$seriesFor)
  garch_preds_2 <- structure(garch_preds_2, names = 1)

  expect_true(sum(garch_preds[1] - garch_preds_2) < 10^(-1))
})

test_that("Lrnr_rugarch gives expected values when ran with a fixed parameter", {
  garch_learner <- Lrnr_rugarch$new(fixed.pars = list(beta1 = 0.86))
  garch_fit <- garch_learner$train(task)
  garch_preds <- garch_fit$predict(task)

  garch_spec <- rugarch::ugarchspec(fixed.pars = list(beta1 = 0.86))
  garch_fit_2 <- rugarch::ugarchfit(garch_spec, bsds$cnt)
  garch_preds_2 <- rugarch::ugarchforecast(garch_fit_2, n.ahead = 1)
  garch_preds_2 <- as.numeric(garch_preds_2@forecast$seriesFor)
  garch_preds_2 <- structure(garch_preds_2, names = 1)

  expect_true(sum(garch_fit$fit_object@model$pars[9] - 0.86) < 10^(-1))
  expect_true(sum(garch_preds[1] - garch_preds_2) < 10^(-1))
})

test_that("Lrnr_rugarch gives expected values with external regressors", {
  covars <- c("cnt", "workingday")
  outcome <- "cnt"
  task <- sl3_Task$new(bsds, covariates = covars, outcome = outcome, folds = folds)

  garch_learner <- Lrnr_rugarch$new(variance.model = list(
    model = "sGARCH", garchOrder = c(1, 1),
    submodel = NULL, external.regressors = as.matrix(task$X[, 2]), variance.targeting = FALSE
  ))
  garch_fit <- garch_learner$train(task)
  garch_preds <- garch_fit$predict(task)

  garch_spec <- rugarch::ugarchspec(variance.model = list(
    model = "sGARCH",
    garchOrder = c(1, 1), submodel = NULL,
    external.regressors = as.matrix(bsds$workingday),
    variance.targeting = FALSE
  ))
  garch_fit_2 <- rugarch::ugarchfit(garch_spec, bsds$cnt)
  garch_preds_2 <- rugarch::ugarchforecast(garch_fit_2, n.ahead = 1)
  garch_preds_2 <- as.numeric(garch_preds_2@forecast$seriesFor)
  garch_preds_2 <- structure(garch_preds_2, names = 1)

  expect_true(sum(garch_preds[1] - garch_preds_2) < 10^(-1))
})
