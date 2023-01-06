context("test-delayed_sl3.R -- manually delay learner fit")
library(future)
library(testthat)

data(cpp_imputed)
task <- sl3_Task$new(
  cpp_imputed, 
  covariates = c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn"),
  outcome = "haz")

lrnr_glmnet <- Lrnr_glmnet$new()
lrnr_glm_fast <- Lrnr_glm_fast$new()
lrnr_rf <- Lrnr_randomForest$new()
nnls_metalearner <- Lrnr_nnls$new()
stack <- Stack$new(lrnr_glm_fast, lrnr_glmnet, lrnr_rf)
sl <- Lrnr_sl$new(learners = stack, metalearner = nnls_metalearner)

set.seed(123)
test_that("FutureJob using delayed is reproducible", {
  set.seed(123)
  test_delayed <- delayed_learner_train(sl, task)
  sched <- Scheduler$new(test_delayed, FutureJob)
  fit_delayed <- sched$compute()
  preds_delayed <- fit_delayed$predict()
  
  set.seed(123)
  options(sl3.enable.future = TRUE) # the default
  fit <- sl$train(task)
  preds <- fit$predict()
  expect_equal(as.numeric(preds_delayed), as.numeric(preds))
})

set.seed(123)
test_that("SequentialJob is reproducible", {
  set.seed(123)
  test_delayed <- delayed_learner_train(sl, task)
  sched <- Scheduler$new(test_delayed, SequentialJob)
  fit_delayed <- sched$compute()
  preds_delayed <- fit_delayed$predict()
  
  set.seed(123)
  options(sl3.enable.future = FALSE)
  fit <- sl$train(task)
  preds <- fit$predict()
  expect_equal(as.numeric(preds_delayed), as.numeric(preds))
})
