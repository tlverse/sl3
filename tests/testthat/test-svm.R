context("test-svm.R -- General testing for Support Vector Machines")

library(sl3)
library(testthat)
library(e1071)

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

task_binaryY <- sl3_Task$new(mtcars, covariates = c(
  "cyl", "disp", "hp", "drat", "wt", "qsec",
  "mpg", "am", "gear", "carb"
), outcome = "vs")

test_learner <- function(learner, task, binary_task = F, ...) {
  # test learner definition this requires that a learner can be instantiated
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

  if (!binary_task) {
    holdout_preds <- fit_obj$predict(task2)
    test_that("Learner can generate holdout set predictions", expect_equal(
      train_preds,
      holdout_preds
    ))
  }

  # test learner chaining
  chained_task <- fit_obj$chain()
  test_that("Chaining returns a task", {
    expect_true(is(chained_task, "sl3_Task"))
  })
  test_that("Chaining returns the correct number of rows", {
    expect_equal(nrow(chained_task$X), nrow(task$X))
  })
}


## test svm learner:
test_learner(Lrnr_svm, task)
test_learner(Lrnr_svm, task2)
test_learner(Lrnr_svm, task_binaryY, binary_task = T)

test_that("Lrnr_svm predictions match those from svm", {
  ## instantiate Lrnr_svm, train on task, and predict on task
  lrnr_svm <- Lrnr_svm$new()
  fit_lrnr_svm <- lrnr_svm$train(task)
  prd_lrnr_svm <- fit_lrnr_svm$predict()

  ## fit svm using the data from the task
  fit_svm <- svm(
    x = task$X, y = task$Y, scale = TRUE, kernel = "radial",
    fitted = TRUE, probability = FALSE
  )
  prd_svm <- as.numeric(predict(fit_svm, newdata = task$X))

  ## test equivalence of prediction from Lrnr_svm and svm::svm
  expect_equal(prd_lrnr_svm, prd_svm)
})
