context("test-dbarts.R -- Lrnr_dbarts")

library(dbarts)

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

test_that("Lrnr_dbarts produces results matching those of dbarts::barts", {
  # get predictions from Lrnr_* wrapper
  set.seed(123)
  lrnr_dbarts <- make_learner(Lrnr_dbarts, verbose = FALSE)
  fit <- lrnr_dbarts$train(task)
  preds <- fit$predict(task)
  rmse_sl3 <- sqrt(mean((preds - task$Y)^2))

  # get predictions from classic implementation
  set.seed(123)
  fit_classic <- dbarts::bart(
    x.train = data.frame(task$X), y.train = task$Y, keeptrees = TRUE,
    ndpost = 500, verbose = FALSE
  )

  preds_classic <- rowMeans(t(predict(fit_classic, newdata = task$X)))
  rmse_classic <- sqrt(mean((preds_classic - task$Y)^2))

  # check equality of predictions
  expect_equal(rmse_sl3, rmse_classic, tolerance = 0.1)
})


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


# test_that("Lrnr_dbarts can be serialized with appropriate param", {
#   dbart_learner_serializable <- Lrnr_dbarts$new(serializeable=TRUE)
#   dbarts_s_fit <- dbart_learner_serializable$train(task)
#
#   preds <- dbarts_s_fit$predict()
#   tmp <- tempfile()
#   save(dbarts_s_fit, file=tmp)
#   rm(dbarts_s_fit)
#   load(tmp)
#   preds_from_serialized <- dbarts_s_fit$predict()
#   tmp <- tempfile()
#
#   db_fit <- dbarts_s_fit$fit_object
#   z <- db_fit$fit$storeState()
#   invisible(db_fit$fit$state)
#   predict(db_fit, as.matrix(task$X))
#   save(db_fit, file=tmp)
#   rm(db_fit)
#   load(tmp)
#   predict(db_fit, as.matrix(task$X))
#   expect_equal(preds, preds_from_serialized)
# })
