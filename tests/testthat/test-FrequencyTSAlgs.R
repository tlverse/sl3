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

set.seed(1)

data(bsds)
covars <- c("cnt")
outcome <- "cnt"

task <- sl3_Task$new(bsds, covariates = covars, outcome = outcome)

test_that("Lrnr_HarmonicReg gives expected values", {
  HarReg_learner <- Lrnr_HarmonicReg$new(n.ahead = 1, K = 7, freq = 105)
  HarReg_fit <- HarReg_learner$train(task)
  HarReg_preds <- HarReg_fit$predict(task)

  bsds_cnt <- ts(bsds$cnt, frequency = 105)
  HarReg_fit_2 <- forecast::tslm(bsds_cnt ~ fourier(bsds_cnt, K = 7))
  HarReg_preds_2 <- forecast::forecast(HarReg_fit_2, data.frame(forecast::fourier(bsds_cnt, K = 7, h = 1)))
  HarReg_preds_2 <- as.numeric(HarReg_preds_2$mean)
  HarReg_preds_2 <- structure(HarReg_preds_2, names = 1)

  expect_true(sum(HarReg_preds - HarReg_preds_2) < 10^(-10))
})
