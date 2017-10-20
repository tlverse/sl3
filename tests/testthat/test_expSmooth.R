context("test_expSmooth.R -- Lrnr_expSmooth")

if (FALSE) {
  setwd("..")
  setwd("..")
  getwd()
  library("devtools")
  document()
  load_all("./")  # load all R files in /R and datasets in /data. Ignores NAMESPACE:
  # devtools::check() # runs full check
  setwd("..")
  install("sl3", build_vignettes = FALSE, dependencies = FALSE)  # INSTALL W/ devtools:
}

library(testthat)
library(sl3)
set.seed(1)

data(bsds)
covars <- c("cnt")
outcome <- "cnt"

task <- sl3_Task$new(bsds, covariates = covars, outcome = outcome)

test_that("Automatic Lrnr_expSmooth gives expected values", {
  expSmooth_learner <- Lrnr_expSmooth$new(n.ahead=5)
  expSmooth_fit <- expSmooth_learner$train(task)
  expSmooth_preds <-  expSmooth_fit$predict(task)
  
  expSmooth_fit_2 <- forecast::ets(bsds$cnt)
  expSmooth_preds_2 <- forecast::forecast(expSmooth_fit_2, h = 5)
  expSmooth_preds_2 <- as.numeric(expSmooth_preds_2$mean)
  expSmooth_preds_2  <- structure(expSmooth_preds_2, names=1:5)

  expect_true(sum(expSmooth_preds - expSmooth_preds_2) < 10^(-10))
  expect_true(all.equal(expSmooth_preds_2, expSmooth_preds))
})
