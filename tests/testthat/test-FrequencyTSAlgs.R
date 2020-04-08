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

set.seed(1)
attach(list(lag = stats::lag), name = "stats_lag_test_kludge", warn.conflicts = FALSE)
data(bsds)
bsds<-bsds[1:50,]

data <- as.data.table(bsds)
data[, time := .I]

outcome <- "cnt"

folds <- origami::make_folds(data,
                             fold_fun = folds_rolling_window, window_size = 20,
                             validation_size = 15, gap = 0, batch = 10
)

node_list <- list(outcome = outcome, time = "time")

task <- sl3_Task$new(data, nodes = node_list, folds = folds)

test_that("Lrnr_HarmonicReg gives expected values", {
  HarReg_learner <- Lrnr_HarmonicReg$new(K = 7, freq = 105)
  HarReg_fit <- HarReg_learner$train(task)
  HarReg_preds <- HarReg_fit$predict(task)

  bsds_cnt <- ts(bsds$cnt, frequency = 105)
  HarReg_fit_2 <- forecast::tslm(bsds_cnt ~ fourier(bsds_cnt, K = 7))
  HarReg_preds_2 <- forecast::forecast(HarReg_fit_2, data.frame(forecast::fourier(bsds_cnt, K = 7, h = 1)))
  HarReg_preds_2 <- as.numeric(HarReg_preds_2$mean)
  HarReg_preds_2 <- structure(HarReg_preds_2, names = 1)

  expect_true(sum(HarReg_preds[1] - HarReg_preds_2) < 10^-1)
})
