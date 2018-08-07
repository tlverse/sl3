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

## generate Friedman data
# set.seed(11)
# n  = 200
# p = 5
# X = data.frame(matrix(runif(n * p), ncol = p))
# y = 10 * sin(pi* X[ ,1] * X[,2]) +20 * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)
# data<-cbind.data.frame(y,X)

# covars=names(data)[2:6]
# outcome=names(data)[1]

# task <- sl3_Task$new(data, covariates = covars, outcome = outcome)

# test_that("Lrnr_bartMachine gives the same thing as bartMachine", {

#  bart_learner<-Lrnr_bartMachine$new()
#  bart_fit<-bart_learner$train(task)
#  mean_pred_sl3<-mean(bart_fit$predict(task))

## build BART regression model
# bart_machine = bartMachine(X, y)
# mean_pred<-mean(predict(bart_machine, X))

# expect_true(mean_pred_sl3 - mean_pred < 0.5)
# expect_true(bart_fit$.__enclos_env__$private$.fit_object$PseudoRsq < 10)
# })

set.seed(11)
testData <- generateFriedmanData()
x <- data.frame(testData$x)
names(x) <- paste("Covs", seq_along(1:ncol(x)), sep = "_")
y <- data.frame(y = testData$y)

covars <- names(x)
outcome <- names(y)

data <- cbind.data.frame(y, x)
task <- sl3_Task$new(data, covariates = covars, outcome = outcome)

test_that("Lrnr_dbarts works", {
  dbart_learner <- Lrnr_dbarts$new(ndpost = 1)
  dbart_fit <- dbart_learner$train(task)
  mean_pred_sl3 <- mean(dbart_fit$predict(task))
  expect_true(mean_pred_sl3 < 20)
})
