context("test-xgboost.R -- General testing for Xgboost")

library(sl3)
library(xgboost)

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
  # test learner definition: this requires that a learner can be instantiated
  # with only default arguments. Not sure if this is a reasonable requirement
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
  test_that("Chaining returns a task", {
    expect_true(is(chained_task, "sl3_Task"))
  })
  test_that("Chaining returns the correct number of rows", {
    expect_equal(nrow(chained_task$X), nrow(task$X))
  })
}

## test xgboost learner:
options(sl3.verbose = TRUE)
test_learner(Lrnr_xgboost, task)
test_learner(Lrnr_xgboost, task2)

test_that("Lrnr_xgboost predictions match xgboost's: continuous outcome", {
  ## instantiate Lrnr_xgboost, train on task, and predict on task
  set.seed(73964)
  lrnr_xgboost <- Lrnr_xgboost$new()
  fit_lrnr_xgboost <- lrnr_xgboost$train(task)
  prd_lrnr_xgboost <- fit_lrnr_xgboost$predict()

  ## fit xgboost using the data from the task
  set.seed(73964)
  fit_xgboost <- xgboost(data = as.matrix(task$X), label = task$Y,
                         nrounds = lrnr_xgboost$params$nrounds,
                         nthread = lrnr_xgboost$params$nthread)
  prd_xgboost <- predict(fit_xgboost, as.matrix(task$X))

  ## test equivalence of prediction from Lrnr_xgboost and xgboost::xgboost
  expect_equal(prd_lrnr_xgboost, prd_xgboost)
})

test_that("Lrnr_xgboost predictions match xgboost's: binary outcome", {
  ## create task with binary outcome
  task <- sl3_Task$new(mtcars, covariates = c(
    "cyl", "disp", "hp", "drat", "wt", "qsec",
    "vs", "mpg", "gear", "carb"
  ), outcome = "am")

  ## instantiate Lrnr_xgboost, train on task, and predict on task
  set.seed(73964)
  lrnr_xgboost <- Lrnr_xgboost$new()
  fit_lrnr_xgboost <- lrnr_xgboost$train(task)
  prd_lrnr_xgboost <- fit_lrnr_xgboost$predict()

  ## fit xgboost using the data from the task
  set.seed(73964)
  fit_xgboost <- xgboost(data = as.matrix(task$X), label = task$Y,
                         nrounds = lrnr_xgboost$params$nrounds,
                         nthread = lrnr_xgboost$params$nthread,
                         objective = "binary:logistic",
                         eval_metric = "logloss")
  prd_xgboost <- predict(fit_xgboost, as.matrix(task$X))

  ## test equivalence of prediction from Lrnr_xgboost and xgboost::xgboost
  expect_equal(prd_lrnr_xgboost, prd_xgboost)
})

test_that("Naive test of Lrnr_xgboost weights", {
  data(mtcars)
  covariates <- colnames(mtcars)[-1]
  mtcars$weights <- c(1, 1, rep(1 / 3, nrow(mtcars) - 2))
  task <- sl3_Task$new(mtcars,
    covariates = covariates, outcome = "mpg",
    weights = "weights"
  )

  ## instantiate Lrnr_xgboost, train on task, and predict on task
  lrnr_xgboost <- Lrnr_xgboost$new()
  set.seed(73964)
  fit_lrnr_xgboost <- lrnr_xgboost$train(task)
  prd_lrnr_xgboost <- fit_lrnr_xgboost$predict()

  ## fit xgboost using the data from the task
  set.seed(73964)
  fit_xgboost <- xgboost(data = as.matrix(task$X), label = task$Y,
                         nrounds = lrnr_xgboost$params$nrounds,
                         nthread = lrnr_xgboost$params$nthread,
                         weight = task$weights)
  prd_xgboost <- predict(fit_xgboost, as.matrix(task$X))

  ## test equivalence of prediction from Lrnr_xgboost and xgboost::xgboost
  expect_equal(prd_lrnr_xgboost, prd_xgboost)
})
