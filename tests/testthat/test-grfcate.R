context("test grfcate.R -- Generalized Random Forests")

test_that("Lrnr_grfcate binary A preds match those of grf::causal_forest", {
  library(grf)

  # Generate some data
  set.seed(791)
  n <- 500
  p <- 10
  X <- matrix(rnorm(n * p), n, p)
  A <- rbinom(n, 1, 0.15)
  X <- cbind(A, X)
  X.test <- matrix(0, 101, p)
  X.test[, 1] <- seq(-2, 2, length.out = 101)
  Y <- X[, 1] * rnorm(n) + 0.5 * A # added a constant treatment effect
  d <- data.frame(list(Y = Y, X = X))
  names(d)[2] <- "A"
  task <- sl3_Task$new(d, covariates = names(d)[-1], outcome = names(d)[1])

  seed_int <- 496L
  set.seed(seed_int)
  # GRF learner class
  grfcate_learner <- Lrnr_grfcate$new(A = "A", seed = seed_int, num.threads = 1L)
  grfcate_fit <- grfcate_learner$train(task)
  grfcate_pred <- grfcate_fit$predict(task)
  set.seed(seed_int)
  grfcate_pkg <- grf::causal_forest(
    X = X[, -1], W = X[, 1], Y = Y, seed = seed_int, num.threads = 1L
  )
  grfcate_pkg_pred_out <- predict(grfcate_pkg)
  grfcate_pkg_pred <- as.numeric(grfcate_pkg_pred_out$predictions)

  # test equivalence
  expect_equal(grfcate_pred, grfcate_pkg_pred)
})

test_that("Lrnr_grfcate continuous A preds match those of grf::causal_forest", {
  library(grf)

  # Generate some data
  set.seed(791)
  n <- 500
  p <- 10
  X <- matrix(rnorm(n * p), n, p)
  A <- rnorm(n)
  X <- cbind(A, X)
  X.test <- matrix(0, 101, p)
  X.test[, 1] <- seq(-2, 2, length.out = 101)
  Y <- X[, 1] * rnorm(n) + 0.5 * A
  d <- data.frame(list(Y = Y, X = X))
  names(d)[2] <- "A"
  task <- sl3_Task$new(d, covariates = names(d)[-1], outcome = names(d)[1])

  seed_int <- 496L
  set.seed(seed_int)
  # GRF learner class
  grfcate_learner <- Lrnr_grfcate$new(A = "A", seed = seed_int, num.threads = 1L)
  grfcate_fit <- grfcate_learner$train(task)
  grfcate_pred <- grfcate_fit$predict(task)
  set.seed(seed_int)
  grfcate_pkg <- grf::causal_forest(
    X = X[, -1], W = X[, 1], Y = Y, seed = seed_int, num.threads = 1L
  )
  grfcate_pkg_pred_out <- predict(grfcate_pkg)
  grfcate_pkg_pred <- as.numeric(grfcate_pkg_pred_out$predictions)

  # test equivalence
  expect_equal(grfcate_pred, grfcate_pkg_pred)
})

test_that("Lrnr_grfcate errors when A not in covariates", {
  data(mtcars)
  task <- sl3_Task$new(
    mtcars,
    covariates = c("cyl", "disp", "hp", "drat"), outcome = "mpg"
  )

  grfcate_learner <- Lrnr_grfcate$new(A = "vs", num.threads = 1L)
  expect_error(grfcate_learner$train(task))
})

test_that("Lrnr_grfcate errors when prediction task missing covariates", {
  data(mtcars)
  task <- sl3_Task$new(
    mtcars,
    covariates = c("cyl", "disp", "hp", "drat", "vs"), outcome = "mpg"
  )
  task2 <- sl3_Task$new(
    mtcars,
    covariates = c("cyl", "disp", "hp", "vs")
  )
  grfcate_learner <- Lrnr_grfcate$new(A = "vs", num.threads = 1L)
  grfcate_fit <- grfcate_learner$train(task)
  expect_error(grfcate_fit$predict(task2))
})

# test_that("Lrnr_grfcate warns when prediction task has extra covariates", {
#   data(mtcars)
#   task <- sl3_Task$new(
#     mtcars, covariates = c("cyl", "disp", "hp", "vs"), outcome = "mpg"
#   )
#   task2 <- sl3_Task$new(
#     mtcars, covariates = c("cyl", "disp", "hp", "drat", "vs")
#   )
#   grfcate_learner <- Lrnr_grfcate$new(A = "vs", num.threads = 1L)
#   grfcate_fit <- grfcate_learner$train(task)
#   expect_warning(preds <- grfcate_fit$predict(task2))
# })
