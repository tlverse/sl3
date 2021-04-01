context("test-lightgbm.R -- General testing for LightGBM")

library(lightgbm)

# define test dataset
data(cpp_imputed)
covars <- c("bmi", "parity", "mage", "sexn")
outcome <- "haz"
task <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome)

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

## test lightgbm learner:
options(sl3.verbose = TRUE)
test_learner(Lrnr_lightgbm, task)

test_that("Lrnr_lightgbm predictions match lightgbm's: continuous outcome", {
  ## instantiate Lrnr_lightgbm, train on task, and predict on task
  set.seed(73964)
  lrnr_lightgbm <- Lrnr_lightgbm$new()
  fit_lrnr_lightgbm <- lrnr_lightgbm$train(task)
  prd_lrnr_lightgbm <- fit_lrnr_lightgbm$predict()

  ## fit lightgbm using the data from the task
  set.seed(73964)
  fit_lightgbm <- lightgbm(
    data = as.matrix(task$X), label = task$Y,
    nrounds = lrnr_lightgbm$params$nrounds,
    nthread = lrnr_lightgbm$params$nthread
  )
  prd_lightgbm <- predict(fit_lightgbm, as.matrix(task$X))

  ## test equivalence of prediction from Lrnr_lightgbm and lightgbm::lightgbm
  expect_equal(prd_lrnr_lightgbm, prd_lightgbm)
})

test_that("Lrnr_lightgbm predictions match lightgbm's: binary outcome", {
  ## create task with binary outcome
  task <- sl3_Task$new(mtcars, covariates = c(
    "cyl", "disp", "hp", "drat", "wt", "qsec",
    "vs", "mpg", "gear", "carb"
  ), outcome = "am")

  ## instantiate Lrnr_lightgbm, train on task, and predict on task
  set.seed(73964)
  lrnr_lightgbm <- Lrnr_lightgbm$new()
  fit_lrnr_lightgbm <- lrnr_lightgbm$train(task)
  prd_lrnr_lightgbm <- fit_lrnr_lightgbm$predict()

  ## fit lightgbm using the data from the task
  set.seed(73964)
  fit_lightgbm <- lightgbm(
    data = as.matrix(task$X), label = task$Y,
    nrounds = lrnr_lightgbm$params$nrounds,
    nthread = lrnr_lightgbm$params$nthread,
    objective = "binary:logistic",
    eval_metric = "logloss"
  )
  prd_lightgbm <- predict(fit_lightgbm, as.matrix(task$X))

  ## test equivalence of prediction from Lrnr_lightgbm and lightgbm::lightgbm
  expect_equal(prd_lrnr_lightgbm, prd_lightgbm)
})

test_that("Cursory test of Lrnr_lightgbm with weights", {
  data(mtcars)
  covariates <- colnames(mtcars)[-1]
  mtcars$weights <- c(1, 1, rep(1 / 3, nrow(mtcars) - 2))
  task <- sl3_Task$new(mtcars,
    covariates = covariates, outcome = "mpg",
    weights = "weights"
  )

  ## instantiate Lrnr_lightgbm, train on task, and predict on task
  lrnr_lightgbm <- Lrnr_lightgbm$new()
  set.seed(73964)
  fit_lrnr_lightgbm <- lrnr_lightgbm$train(task)
  prd_lrnr_lightgbm <- fit_lrnr_lightgbm$predict()

  ## fit lightgbm using the data from the task
  set.seed(73964)
  fit_lightgbm <- lightgbm(
    data = as.matrix(task$X), label = task$Y,
    nrounds = lrnr_lightgbm$params$nrounds,
    nthread = lrnr_lightgbm$params$nthread,
    weight = task$weights
  )
  prd_lightgbm <- predict(fit_lightgbm, as.matrix(task$X))

  ## test equivalence of prediction from Lrnr_lightgbm and lightgbm::lightgbm
  expect_equal(prd_lrnr_lightgbm, prd_lightgbm)
})
