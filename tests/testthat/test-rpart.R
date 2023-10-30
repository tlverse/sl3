context("test-Lrnr_rpart.R -- General testing for Rpart")

library(sl3)
library(testthat)
library(rpart)

# define test dataset
data(mtcars)
task <- sl3_Task$new(mtcars, covariates = c(
  "cyl", "disp", "hp", "drat", "wt", "qsec",
  "vs", "am", "gear", "carb"
), outcome = "mpg")

task2 <- sl3_Task$new(mtcars, covariates = c(
  "cyl", "disp", "hp", "drat", "wt", "qsec",
  "vs", "am", "gear", "carb"
), outcome = "mpg")

interactions <- list(c("cyl", "disp"), c("hp", "drat"))
task_with_interactions <- task$add_interactions(interactions)
task2 <- task2$add_interactions(interactions)

test_learner <- function(learner, task, ...) {
  # test learner definition this requires that a learner can be instantiated with
  # only default arguments. Not sure if this is a reasonable requirement
  learner_obj <- learner$new(...)
  print(sprintf("Testing Learner: %s", learner_obj$name))
  # test learner training
  fit_obj <- learner_obj$train(task)
  test_that("Learner can be trained on data", expect_true(fit_obj$is_trained))

  # test learner prediction
  train_preds <- fit_obj$predict()
  test_that("Learner can generate training set predictions", expect_equal(
    sl3:::safe_dim(train_preds)[1],
    length(task$Y)
  ))

  holdout_preds <- fit_obj$predict(task2)
  test_that("Learner can generate holdout set predictions", expect_equal(
    train_preds,
    holdout_preds
  ))

  # test learner chaining
  chained_task <- fit_obj$chain()
  test_that("Chaining returns a task", expect_true(is(chained_task, "sl3_Task")))
  test_that("Chaining returns the correct number of rows", expect_equal(
    nrow(chained_task$X),
    nrow(task$X)
  ))
}

## test rpart learner:
test_learner(Lrnr_rpart, task)
test_learner(Lrnr_rpart, task2)

test_that("Lrnr_rpart predictions match those from rpart", {
  ## instantiate Lrnr_rpart, train on task, and predict on task
  lrnr_rpart <- Lrnr_rpart$new()
  fit_lrnr_rpart <- lrnr_rpart$train(task)
  prd_lrnr_rpart <- fit_lrnr_rpart$predict()

  ## fit rpart using the data from the task
  fit_rpart <- rpart(mpg ~ ., data = task$data)
  prd_rpart <- predict(fit_rpart)

  ## test equivalence of prediction from Lrnr_rpart and rpart::rpart
  expect_equal(prd_lrnr_rpart, prd_rpart)
})

# try to reproduce https://github.com/tlverse/sl3/issues/230
library(sl3)
library(testthat)
library(rpart)

# define test dataset
data(mtcars)
task <- sl3_Task$new(mtcars, covariates = c(
  "cyl", "disp", "hp", "drat", "wt", "qsec",
  "vs", "am", "gear", "carb"
), outcome = "mpg")


lrnr_rpart <- Lrnr_rpart$new()
lrnr_mean <- Lrnr_mean$new()
stack <- Stack$new(lrnr_rpart, lrnr_mean)

stack_fit <- stack$train(task)
predict <- stack_fit$predict()
