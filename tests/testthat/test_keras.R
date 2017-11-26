context("test_keras.R -- time series methods")

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
library(reticulate)
reticulate::import("keras.models")
library(kerasR)
library(keras)
set.seed(1)

trend_all<-11:130 + rnorm(120,sd=2)
trend_all<-data.frame(data=trend_all)
task <- sl3_Task$new(trend_all, covariates = "data", outcome = "data")

test_that("Lrnr_lstm does what we expect", {
  lstm_learner <- Lrnr_lstm$new(epochs = 10)
  lstm_fit <- lstm_learner$train(task)
  lstm_preds <- lstm_fit$predict(task)
  
  expect_true(sum(lstm_preds)-537.0385 < 10^(-10))
})

test_that("Lrnr_bilstm does what we expect", {
  bilstm_learner <- Lrnr_bilstm$new(epochs = 10)
  bilstm_fit <- bilstm_learner$train(task)
  bilstm_preds <- bilstm_fit$predict(task)

  expect_true(sum(bilstm_preds)-3724.96 < 10^(-10))
})
