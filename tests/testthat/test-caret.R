context("test-caret.R -- General testing for Caret package support")

library(sl3)
library(testthat)
library(caret)

# define test dataset
data(mtcars)
task <- sl3_Task$new(mtcars, covariates = c(
  "cyl", "disp", "hp", "drat", "wt", "qsec",
  "vs", "am", "gear", "carb"
), outcome = "mpg")

mtcars$mpg_binary <- as.numeric(mtcars$mpg < 19)
task_binaryY <- sl3_Task$new(mtcars, covariates = c(
  "cyl", "disp", "hp", "drat", "wt", "qsec",
  "vs", "am", "gear", "carb"
), outcome = "mpg_binary")

mtcars$mpg_categorical <- as.factor(ifelse(
  mtcars$mpg < 17, "low", ifelse(mtcars$mpg < 21.5, "medium", "high")
))
task_catY <- sl3_Task$new(mtcars, covariates = c(
  "cyl", "disp", "hp", "drat", "wt", "qsec",
  "vs", "am", "gear", "carb"
), outcome = "mpg_categorical")

test_learner <- function(learner, task, ...) {
  learner_obj <- learner$new(...)
  task2 <- task$clone()
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

## test caret learner with the example of Random Forest:
op <- options(sl3.verbose = TRUE)
options(op)
test_learner(Lrnr_caret, task, method = "rf")
test_learner(Lrnr_caret, task_binaryY, method = "rf")
test_learner(Lrnr_caret, task_catY, method = "rf")
test_learner(Lrnr_caret, task, method = "xgbLinear")
test_learner(Lrnr_caret, task_binaryY, method = "xgbLinear")
test_learner(Lrnr_caret, task_catY, method = "xgbLinear")

test_that("Lrnr_caret RF match caret RF preds for continuous outcome", {
  ## instantiate Lrnr_caret, train on task, and predict on task
  lrnr_caret_rf <- Lrnr_caret$new(method = "rf")
  set.seed(1530)
  fit_lrnr_caret_rf <- lrnr_caret_rf$train(task)
  prd_lrnr_caret_rf <- fit_lrnr_caret_rf$predict()
  mse_lrnr_caret <- mean((task$Y - prd_lrnr_caret_rf)^2)

  ## fit caret RF using the data from the task
  set.seed(1530)
  fit_caret_rf <- caret::train(
    x = task$X, y = task$Y, method = "rf", metric = "RMSE",
    trControl = caret::trainControl(
      method = "cv",
      indexOut = fit_lrnr_caret_rf$fit_object$control$indexOut
    )
  )
  prd_caret_rf <- as.numeric(predict(fit_caret_rf, newdata = task$X))
  mse_caret <- mean((task$Y - prd_caret_rf)^2)

  expect_equal(mse_lrnr_caret, mse_caret, tolerance = 0.1)
})

test_that("Lrnr_caret RF match caret RF preds for binary classification", {
  ## instantiate Lrnr_caret, train on task, and predict on task
  lrnr_caret_rf <- Lrnr_caret$new(method = "rf")
  set.seed(1530)
  fit_lrnr_caret_rf <- lrnr_caret_rf$train(task_binaryY)
  prd_lrnr_caret_rf <- fit_lrnr_caret_rf$predict()
  mse_lrnr_caret_rf <- mean((prd_lrnr_caret_rf - task_binaryY$Y)^2)

  ## fit caret RF using the data from the task
  set.seed(1530)
  fit_caret_rf <- suppressWarnings(caret::train(
    x = task_binaryY$X, y = as.factor(task_binaryY$Y), method = "rf",
    metric = "Accuracy",
    trControl = caret::trainControl(
      method = "cv", indexOut = fit_lrnr_caret_rf$fit_object$control$indexOut
    )
  ))
  prd_caret_rf <- as.numeric(
    predict(fit_caret_rf, newdata = task$X, type = "prob")[, 2]
  )
  mse_caret_rf <- mean((prd_caret_rf - task_binaryY$Y)^2)

  expect_equal(mse_caret_rf, mse_lrnr_caret_rf, tolerance = 0.01)
})

test_that("Lrnr_caret RF preds match caret RF preds for categorical outcome", {
  ## instantiate Lrnr_caret, train on task, and predict on task
  lrnr_caret_rf <- Lrnr_caret$new(method = "rf")
  set.seed(1530)
  fit_lrnr_caret_rf <- lrnr_caret_rf$train(task_catY)
  prd_lrnr_caret_rf <- fit_lrnr_caret_rf$predict()
  prd_lrnr_caret_rf <- unlist(lapply(prd_lrnr_caret_rf, function(x) which.max(x[[1]])))

  ## fit caret RF using the data from the task
  set.seed(1530)
  fit_caret_rf <- suppressWarnings(caret::train(
    x = task_catY$X, y = task_catY$Y, method = "rf", metric = "Accuracy",
    trControl = caret::trainControl(
      method = "cv",
      indexOut = fit_lrnr_caret_rf$fit_object$control$indexOut
    )
  ))
  prd_caret_rf <- pack_predictions(
    predict(fit_caret_rf, newdata = task$X, type = "prob")
  )
  prd_caret_rf <- unlist(lapply(prd_caret_rf, function(x) which.max(x[[1]])))
  
  expect_equal(prd_lrnr_caret_rf, prd_caret_rf)
})

test_that("Lrnr_caret RF preds match caret RF preds for binary regression", {
  ## instantiate Lrnr_caret, train on task, and predict on task
  lrnr_caret_rf <- Lrnr_caret$new(
    method = "rf", metric = "RMSE",
    factor_binary_outcome = FALSE
  )
  set.seed(1530)
  fit_lrnr_caret_rf <- lrnr_caret_rf$train(task_binaryY)
  prd_lrnr_caret_rf <- as.numeric(fit_lrnr_caret_rf$predict() > 0.5)

  ## fit caret RF using the data from the task
  set.seed(1530)
  fit_caret_rf <- suppressWarnings(caret::train(
    x = task_binaryY$X, y = task_binaryY$Y, method = "rf",
    metric = "RMSE", trControl = caret::trainControl(
      method = "cv",
      indexOut = fit_lrnr_caret_rf$fit_object$control$indexOut
    )
  ))
  prd_caret_rf <- as.numeric(predict(fit_caret_rf, newdata = task$X) > 0.5)

  expect_equal(prd_lrnr_caret_rf, prd_caret_rf)
})
