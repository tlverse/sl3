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
  print(sprintf("Testing Learner: %s", learner_obj$name))

  # test learner training
  test_that("Learner can be trained on data", {
    skip_on_os("windows")
    learner_obj <- learner$new(...)
    fit_obj <- learner_obj$train(task)
    expect_true(fit_obj$is_trained)
  })

  # test learner prediction
  test_that("Learner can generate training set predictions", {
    skip_on_os("windows")
    train_preds <- fit_obj$predict()
    expect_equal(
      sl3:::safe_dim(train_preds)[1], length(task$Y)
    )
  })

  # test learner chaining
  test_that("Chaining returns a task", {
    skip_on_os("windows")
    chained_task <- fit_obj$chain()
    expect_true(is(chained_task, "sl3_Task"))
  })
  test_that("Chaining returns the correct number of rows", {
    skip_on_os("windows")
    expect_equal(nrow(chained_task$X), nrow(task$X))
  })
}

## test lightgbm learner:
options(sl3.verbose = TRUE)
test_learner(Lrnr_lightgbm, task)

test_that("Lrnr_lightgbm predictions match lightgbm's: continuous outcome", {
  skip_on_os("windows")
  ## instantiate Lrnr_lightgbm, train on task, and predict on task
  set.seed(73964)
  lrnr_lightgbm <- Lrnr_lightgbm$new()
  fit_lrnr_lightgbm <- lrnr_lightgbm$train(task)
  prd_lrnr_lightgbm <- fit_lrnr_lightgbm$predict()

  ## fit lightgbm using the data from the task
  set.seed(73964)
  lgb_data <- lgb.Dataset(
    data = as.matrix(task$X),
    label = as.numeric(task$Y)
  )
  fit_lightgbm <- lgb.train(
    params = lrnr_lightgbm$params,
    obj = "regression",
    data = lgb_data
  )
  prd_lightgbm <- predict(fit_lightgbm, as.matrix(task$X))

  ## test equivalence of prediction from Lrnr_lightgbm and lightgbm::lightgbm
  expect_equal(prd_lrnr_lightgbm, prd_lightgbm)
})

test_that("Lrnr_lightgbm predictions match lightgbm's: binary outcome", {
  skip_on_os("windows")
  ## create task with binary outcome
  covars <- c("bmi", "haz", "mage", "sexn")
  outcome <- "smoked"
  task <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome)

  ## instantiate Lrnr_lightgbm, train on task, and predict on task
  set.seed(73964)
  lrnr_lightgbm <- Lrnr_lightgbm$new(num_leaves = 30L)
  fit_lrnr_lightgbm <- lrnr_lightgbm$train(task)
  prd_lrnr_lightgbm <- fit_lrnr_lightgbm$predict()

  ## fit lightgbm using the data from the task
  set.seed(73964)
  lgb_data <- lgb.Dataset(
    data = as.matrix(task$X),
    label = as.numeric(task$Y)
  )
  fit_lightgbm <- lgb.train(
    params = lrnr_lightgbm$params,
    obj = "binary",
    eval = "binary_logloss",
    data = lgb_data
  )
  prd_lightgbm <- predict(fit_lightgbm, as.matrix(task$X))

  ## test equivalence of prediction from Lrnr_lightgbm and lightgbm::lightgbm
  expect_equal(prd_lrnr_lightgbm, prd_lightgbm)
})

test_that("Lrnr_lightgbm predictions match lightgbm's: categorical outcome", {
  skip_on_os("windows")
  ## create task with binary outcome
  covars <- c("bmi", "haz", "mage", "sexn")
  outcome <- "parity"
  cpp_imputed[[outcome]] <- as.numeric(as.factor(cpp_imputed[[outcome]]))
  task <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome)

  ## instantiate Lrnr_lightgbm, train on task, and predict on task
  set.seed(73964)
  lrnr_lightgbm <- Lrnr_lightgbm$new(num_leaves = 40L)
  fit_lrnr_lightgbm <- lrnr_lightgbm$train(task)
  prd_lrnr_lightgbm <- unpack_predictions(fit_lrnr_lightgbm$predict())

  ## fit lightgbm using the data from the task
  set.seed(73964)
  lgb_data <- lgb.Dataset(
    data = as.matrix(task$X),
    label = as.numeric(task$Y) - 1L
  )
  fit_lightgbm <- lgb.train(
    params = lrnr_lightgbm$params,
    num_class = as.integer(length(unique(task$Y))),
    obj = "multiclass",
    eval = "multi_error",
    data = lgb_data
  )
  prd_lightgbm <- predict(fit_lightgbm, as.matrix(task$X), reshape = TRUE)

  ## test equivalence of prediction from Lrnr_lightgbm and lightgbm::lightgbm
  expect_equal(prd_lrnr_lightgbm, prd_lightgbm)
})

test_that("Cursory test of Lrnr_lightgbm with weights", {
  skip_on_os("windows")
  ## create task, continuous outcome with observation-level weights
  covars <- c("bmi", "parity", "mage", "sexn")
  outcome <- "haz"
  cpp_imputed$weights <- runif(nrow(cpp_imputed), 0.3, 0.7)
  task <- sl3_Task$new(cpp_imputed,
    covariates = covars, outcome = outcome,
    weights = "weights"
  )

  ## instantiate Lrnr_lightgbm, train on task, and predict on task
  set.seed(73964)
  lrnr_lightgbm <- Lrnr_lightgbm$new()
  fit_lrnr_lightgbm <- lrnr_lightgbm$train(task)
  prd_lrnr_lightgbm <- fit_lrnr_lightgbm$predict()

  ## fit lightgbm using the data from the task
  set.seed(73964)
  lgb_data <- lgb.Dataset(
    data = as.matrix(task$X),
    label = as.numeric(task$Y),
    weight = as.numeric(task$weights)
  )
  fit_lightgbm <- lgb.train(
    params = lrnr_lightgbm$params,
    obj = "regression",
    data = lgb_data
  )
  prd_lightgbm <- predict(fit_lightgbm, as.matrix(task$X))

  ## test equivalence of prediction from Lrnr_lightgbm and lightgbm::lightgbm
  expect_equal(prd_lrnr_lightgbm, prd_lightgbm)
})
