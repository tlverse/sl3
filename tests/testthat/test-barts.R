context("test barts: bartMachine and dbart")

library(dbarts)

if (FALSE) {
  setwd("..")
  getwd()
  library("devtools")
  document()
  # load all R files in /R and datasets in /data. Ignores NAMESPACE:
  load_all("./")
  setwd("..")
  install("sl3",
    build_vignettes = FALSE,
    dependencies = FALSE
  ) # INSTALL W/ devtools:
}

set.seed(99)
generateFriedmanData <- function(n = 100, sigma = 1.0) {
  f <- function(x) {
    10 * sin(pi * x[, 1] * x[, 2]) + 20 * (x[, 3] - 0.5)^2 +
      10 * x[, 4] + 5 * x[, 5]
  }
  x <- matrix(runif(n * 10), n, 10)
  y <- rnorm(n, f(x), sigma)
  return(list(x = x, y = y))
}

set.seed(11)
testData <- generateFriedmanData()
x <- data.frame(testData$x)
names(x) <- paste("Covs", seq_along(1:ncol(x)), sep = "_")
y <- data.frame(y = testData$y)

covars <- names(x)
outcome <- names(y)

data <- cbind.data.frame(y, x)
task <- sl3_Task$new(data, covariates = covars, outcome = outcome)

test_that("Lrnr_dbarts with continuous outcome works", {
  dbart_learner <- Lrnr_dbarts$new(ndpost = 200)
  dbart_fit <- dbart_learner$train(task)
  mean_pred_sl3 <- mean(dbart_fit$predict(task))
  expect_true(mean_pred_sl3 < 20)
})

## Classification 

generateProbitData <- function() {
  n <- 800
  beta <- c(0.12, -0.89, 0.3)
  p <- 3
  
  set.seed(0)
  X <- matrix(rnorm(p * n), n, p)
  
  mu <- pnorm(X %*% beta)
  Z <- rbinom(n, 1, mu)
  
  return(list(X = X, Z = Z, p = mu))
}

testData <- generateProbitData()
x <- data.frame(testData$X)
names(x) <- paste("Covs", seq_along(1:ncol(x)), sep = "_")
y <- data.frame(y = testData$Z)

covars <- names(x)
outcome <- names(y)

data <- cbind.data.frame(y, x)
task <- sl3_Task$new(data, covariates = covars, outcome = outcome)

test_that("Lrnr_dbarts with binary outcome works", {
  dbart_learner <- Lrnr_dbarts$new()
  dbart_fit <- dbart_learner$train(task)
  mean_pred_sl3 <- mean(dbart_fit$predict(task))
  expect_true(mean_pred_sl3 < 20)
})
