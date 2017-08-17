context("Test ARIMA sl3")

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
library(origami)
set.seed(1)

data(bsds)
covars <- c("cnt")
outcome <- "cnt"

task <- sl3_Task$new(bsds, covariates = covars, outcome = outcome)
task$nodes$covariates

test_that("Lrnr_arima gives expected values with auto.arima", {
  arima_learner <- Lrnr_arima$new()
  arima_fit <- arima_learner$train(task)
  arima_preds <- arima_fit$predict(task)
  
  arima_fit_2 <- forecast::auto.arima(bsds$cnt)
  arima_preds_2 <- predict(arima_fit_2)
  
  expect_true(sum(arima_preds$pred[1]- arima_preds_2$pred[1]) < 10^(-10))
  expect_true(all.equal(arima_preds_2, arima_preds))
})

test_that("Lrnr_arima gives expected values with arima order set", {
  arima_learner <- Lrnr_arima$new(order=c(3,1,6))
  arima_fit <- arima_learner$train(task)
  arima_preds <- arima_fit$predict(task)
  
  arima_fit_2 <- arima(bsds$cnt, order=c(3,1,6))
  arima_preds_2 <- predict(arima_fit_2)
  
  
  expect_true(sum(arima_preds$pred[1]- arima_preds_2$pred[1]) < 10^(-10))
  expect_true(all.equal(arima_preds_2, arima_preds))
})

test_that("Lrnr_arima with further forecasts", {
  arima_learner <- Lrnr_arima$new(n.ahead=5)
  arima_fit <- arima_learner$train(task)
  arima_preds <- arima_fit$predict(task)
  
  arima_fit_2 <- forecast::auto.arima(bsds$cnt)
  arima_preds_2 <- predict(arima_fit_2, n.ahead=5)
  
  expect_true(sum(arima_preds$pred- arima_preds_2$pred) < 10^(-10))
  expect_true(all.equal(arima_preds_2, arima_preds))
})













