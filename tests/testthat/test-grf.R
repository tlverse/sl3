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
set.seed(791)

# Generate some data
n <- 50
p <- 10
X <- matrix(rnorm(n * p), n, p)
X.test <- matrix(0, 101, p)
X.test[, 1] <- seq(-2, 2, length.out = 101)
Y <- X[, 1] * rnorm(n)
data <- data.frame(list(Y = Y, X = X))

# Make sl3 Task
covars <- names(data)[2:ncol(data)]
outcome <- names(data)[1]
task <- sl3_Task$new(data, covariates = covars, outcome = outcome)

test_that("Lrnr_grf predictions match those of grf::quantile_forest", {
  seed_int <- 496L
  set.seed(seed_int)
  # GRF learner class
  grf_learner <- Lrnr_grf$new(seed = seed_int)
  grf_fit <- grf_learner$train(task)
  grf_pred <- grf_fit$predict(task)

  set.seed(seed_int)
  # GRF package
  grf_pkg <- grf::quantile_forest(
    X = X, Y = Y, seed = seed_int,
    num.threads = 1L
  )
  grf_pkg_pred_out <- predict(
    grf_pkg, quantiles = grf_fit$params$quantiles_pred
  )
  grf_pkg_pred <- as.numeric(grf_pkg_pred_out$predictions)

  # test equivalence
  expect_equal(grf_pred, grf_pkg_pred)
})
