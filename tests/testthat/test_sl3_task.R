library(sl3)
library(testthat)

context("Basic sl3_Task properties")
# define test dataset
data(mtcars)
covariates <- c("cyl", "disp", "hp", "drat", "wt", "qsec",
              "vs", "am", "gear", "carb")
outcome <- "mpg"
task <- sl3_Task$new(mtcars, covariates = covariates, outcome = outcome)


test_that("task$X returns appropriate data",{
  X <- task$X
  expect_equal(dim(X), c(nrow(mtcars),length(covariates)))
  expect_equal(names(X), covariates)
})

test_that("task$Y returns appropriate data",{
  Y <- task$Y
  expect_length(Y,nrow(mtcars))
  expect_equal(Y, mtcars[[outcome]])
})

test_that("task$weights returns appropriate data",{
  weights <- task$weights
  expect_length(weights,nrow(mtcars))
  expect_true(all(weights==1))
})

test_that("task subsetting works",{
  subset_vector <- 1:10
  subsetted <- task[subset_vector]
  expect_equal(subsetted$X, task$X[subset_vector])
})


empty_task <- sl3_Task$new(mtcars, covariates = NULL, outcome = NULL)

test_that("task$X_intercept works for empty X (intercept-only)", {
  expect_equal(nrow(empty_task$X_intercept),nrow(mtcars))
})

test_that("task errors for empty Y (intercept-only)", {
  expect_error(empty_task$Y)
})

