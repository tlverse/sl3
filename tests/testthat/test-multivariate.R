library(testthat)
context("test_multivariate.R -- Basic Multivariate functionality")

options(sl3.verbose = TRUE)
library(sl3)
library(origami)
library(SuperLearner)

n <- 1000
p <- 5
pY <- 3
W <- matrix(rnorm(n * p), nrow = n)
colnames(W) <- sprintf("W%d", seq_len(p))
Y <- matrix(rnorm(n * pY, 0, 0.2) + W[, 1], nrow = n)
colnames(Y) <- sprintf("Y%d", seq_len(pY))
data <- data.table(W, Y)
covariates <- grep("W", names(data), value = TRUE)
outcomes <- grep("Y", names(data), value = TRUE)

task <- sl3_Task$new(data.table::copy(data), covariates = covariates, outcome = outcomes)
mv_learner <- make_learner(Lrnr_multivariate, make_learner(Lrnr_glm_fast))
mv_fit <- mv_learner$train(task)
preds <- mv_fit$predict(task)
preds <- unpack_predictions(preds)
test_that("Lrnr_mv preds are the correct dimensions", {
  expect_equal(ncol(preds), pY)
  expect_equal(nrow(preds), n)
})
