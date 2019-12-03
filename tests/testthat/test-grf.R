context("test grf: Generalized Random Forests")

if (FALSE) {
  setwd("..")
  getwd()
  library("devtools")
  document()
  # load all R files in /R and datasets in /data. Ignores NAMESPACE:
  load_all("./")
  setwd("..")
  # INSTALL W/ devtools:
  install("sl3", build_vignettes = FALSE, dependencies = FALSE)
}

# Preliminaries
library(grf)
set.seed(11)

# Generate some data
n <- 2000
p <- 20
X <- matrix(rnorm(n * p), n, p)
Y <- X[, 1] * rnorm(n)
data <- cbind.data.frame(Y = Y, X = X)

# Make sl3 Task
covars <- names(data)[2:ncol(data)]
outcome <- names(data)[1]
task <- sl3_Task$new(data, covariates = covars, outcome = outcome)

test_that("Lrnr_grf gives the expected output", {
  set.seed(496)
  # GRF learner class
  grf_learner <- Lrnr_grf$new()
  grf_fit <- grf_learner$train(task)
  grf_pred <- grf_fit$predict(task)

  set.seed(496)
  # GRF package
  grf_pkg <- grf::quantile_forest(X = X, Y = Y)
  grf_pkg_pred <- predict(grf_pkg)

  # test equivalence
  expect_equal(grf_pred, grf_pkg_pred[,2])
})
