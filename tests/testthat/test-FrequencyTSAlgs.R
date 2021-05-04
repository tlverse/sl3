context("test-FrequencyTSAlgs.R -- Lrnr_HarmonicReg")

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
library(sl3)

set.seed(1)

data(bsds)
data <- as.data.table(bsds)
data[, time := .I]

covars <- c("temp", "windspeed")
outcome <- "cnt"

node_list <- list(outcome = outcome, covariates = covars, time = "time")
folds <- origami::make_folds(bsds,
  fold_fun = folds_rolling_window, window_size = 50,
  validation_size = 10, gap = 0, batch = 200
)
task <- sl3_Task$new(data, nodes = node_list, folds = folds)

train_task <- training(task, fold = task$folds[[1]])
valid_task <- validation(task, fold = task$folds[[1]])

test_that("Automatic Lrnr_expSmooth gives expected number of predictions", {
  HarReg_learner <- Lrnr_HarmonicReg$new(K = 7, freq = 105)
  HarReg_fit <- HarReg_learner$train(train_task)
  HarReg_preds <- HarReg_fit$predict(valid_task)

  # bsds_cnt <- ts(bsds$cnt, frequency = 105)
  # HarReg_fit_2 <- forecast::tslm(bsds_cnt ~ fourier(bsds_cnt, K = 7))
  # HarReg_preds_2 <- forecast::forecast(HarReg_fit_2, data.frame(forecast::fourier(bsds_cnt, K = 7, h = 1)))
  # HarReg_preds_2 <- as.numeric(HarReg_preds_2$mean)
  # HarReg_preds_2 <- structure(HarReg_preds_2, names = 1)

  expect_true(length(HarReg_preds) == 10)
})
