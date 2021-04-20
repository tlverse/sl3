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

library(origami)
library(data.table)

options(rgl.useNULL = TRUE)
set.seed(1)

data(bsds)

folds <- make_folds(bsds,
  fold_fun = folds_rolling_window, window_size = 500,
  validation_size = 100, gap = 0, batch = 50
)

task <- sl3_Task$new(
  data = bsds, folds = folds,
  covariates = c("weekday", "temp"),
  outcome = "cnt"
)

train_task <- training(task, fold = task$folds[[1]])
valid_task <- validation(task, fold = task$folds[[1]])

test_that("Lrnr_rugarch gives expected values with no model specification", {
  garch_learner <- Lrnr_rugarch$new()
  garch_fit <- garch_learner$train(train_task)
  garch_preds <- garch_fit$predict(valid_task)

  # garch_spec <- rugarch::ugarchspec()
  # garch_fit_2 <- rugarch::ugarchfit(garch_spec, bsds$cnt)
  # garch_preds_2 <- rugarch::ugarchforecast(garch_fit_2, n.ahead = 1)
  # garch_preds_2 <- as.numeric(garch_preds_2@forecast$seriesFor)
  # garch_preds_2 <- structure(garch_preds_2, names = 1)

  expect_true(length(garch_preds) == 100)
})

test_that("Lrnr_rugarch gives expected values with higher order ARMA and GARCH", {
  garch_learner <- Lrnr_rugarch$new(
    variance.model = list(model = "sGARCH", garchOrder = c(2, 2)),
    mean.model = list(armaOrder = c(2, 2), include.mean = TRUE),
    distribution.model = "sstd"
  )
  garch_fit <- garch_learner$train(train_task)
  garch_preds <- garch_fit$predict(valid_task)

  expect_true(length(garch_preds) == 100)
})
