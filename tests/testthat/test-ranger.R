context("test-ranger.R -- General testing for Ranger")

library(sl3)
library(testthat)
library(ranger)

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

## test ranger learner:
test_learner(Lrnr_ranger, task)
test_learner(Lrnr_ranger, task2)

test_that("Lrnr_ranger predictions match those from ranger", {
  ## instantiate Lrnr_ranger, train on task, and predict on task
  lrnr_ranger <- Lrnr_ranger$new(seed = 123)
  fit_lrnr_ranger <- lrnr_ranger$train(task)
  prd_lrnr_ranger <- fit_lrnr_ranger$predict()

  ## fit ranger using the data from the task
  fit_ranger <- ranger(mpg ~ .,
    num.trees = lrnr_ranger$params$num.trees,
    write.forest = lrnr_ranger$params$write.forest,
    importance = lrnr_ranger$params$importance,
    num.threads = lrnr_ranger$params$num.threads,
    seed = 123,
    data = task$data
  )
  prd_ranger <- predict(fit_ranger, data = task$data)[[1]]

  ## test equivalence of prediction from Lrnr_ranger and ranger::ranger
  expect_equal(prd_lrnr_ranger, prd_ranger)
})

test_that("Naive test of Lrnr_ranger weights", {
  data(mtcars)
  covariates <- colnames(mtcars)[-1]
  mtcars$weights <- c(1, 1, rep(1 / 3, nrow(mtcars) - 2))
  task <- sl3_Task$new(mtcars,
    covariates = covariates, outcome = "mpg",
    weights = "weights"
  )

  ## instantiate Lrnr_ranger, train on task, and predict on task
  lrnr_ranger <- Lrnr_ranger$new(seed = 123)
  fit_lrnr_ranger <- lrnr_ranger$train(task)
  prd_lrnr_ranger <- fit_lrnr_ranger$predict()

  ## fit ranger using the data from the task
  data <- cbind(task$Y, task$X)
  colnames(data)[1] <- task$nodes$outcome
  dependent.variable.name <- task$nodes$outcome
  fit_ranger <- ranger(
    data = data,
    seed = 123,
    dependent.variable.name = dependent.variable.name,
    case.weights = task$weights
  )
  prd_ranger <- predict(fit_ranger, data = task$data)[[1]]

  ## test equivalence of prediction from Lrnr_ranger and ranger::ranger
  expect_equal(prd_lrnr_ranger, prd_ranger)
})
