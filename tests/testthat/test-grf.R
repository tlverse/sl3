context("test grf: Generalized Random Forests")

if (FALSE) {
  setwd("..")
  getwd()
  library("devtools")
  document()
  load_all("./") # load all R files in /R and datasets in /data. Ignores NAMESPACE:
  setwd("..")
  install("sl3", build_vignettes = FALSE, dependencies = FALSE) # INSTALL W/ devtools:
}

library(testthat)
library(sl3)

set.seed(11)

# Generate some data.
n <- 2000
p <- 20
X <- matrix(rnorm(n * p), n, p)
Y <- X[, 1] * rnorm(n)

data <- cbind.data.frame(Y = Y, X = X)

covars <- names(data)[2:ncol(data)]
outcome <- names(data)[1]

task <- sl3_Task$new(data, covariates = covars, outcome = outcome)

test_that("Lrnr_grf gives the expected output", {
  grf_learner <- Lrnr_grf$new()
  grf_fit <- grf_learner$train(task)
  mean_pred_sl3 <- mean(grf_fit$predict(task))

  expect_true(mean_pred_sl3 < 0.5)
})
