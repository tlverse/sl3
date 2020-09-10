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
test_learner(Lrnr_caret, task, algorithm = "rf")

test_that("Lrnr_caret:RF predictions match those from RF", {
  ## instantiate Lrnr_caret, train on task, and predict on task
  lrnr_caret_rf <- Lrnr_caret$new(algorithm = "rf")
  set.seed(1530)
  fit_lrnr_caret_rf <- lrnr_caret_rf$train(task)
  prd_lrnr_caret_rf <- fit_lrnr_caret_rf$predict()

  ## fit RF using the data from the task
  set.seed(1530)
  fit_caret_rf <- caret::train(
    x = task$X, y = task$Y, method = "rf", metric = "RMSE",
    trControl = caret::trainControl(method = "CV")
  )
  prd_caret_rf <- as.numeric(predict(fit_caret_rf, newdata = task$X))

  ## test equivalence of prediction from Lrnr_svm and svm::svm
  expect_equal(prd_lrnr_caret_rf, prd_caret_rf)
})

task_binaryY <- sl3_Task$new(mtcars, covariates = c(
  "cyl", "disp", "hp", "drat", "wt", "qsec", "am", "gear", "carb"
), outcome = "vs")
test_learner(Lrnr_caret, task_binaryY, algorithm = "rf")
