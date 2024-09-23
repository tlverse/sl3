context("test-delayed_sl3.R -- manually delay learner fit")
skip_on_cran()
if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  return()
}

library(future)
library(testthat)
plan(sequential)

data(cpp_imputed)
task <- sl3_Task$new(
  cpp_imputed,
  covariates = c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn"),
  outcome = "haz"
)

lrnr_rf <- Lrnr_randomForest$new()

test_that("FutureJob using delayed is reproducible", {
  set.seed(123)
  test_delayed <- delayed_learner_train(lrnr_rf, task)
  sched <- Scheduler$new(test_delayed, FutureJob)
  fit_delayed <- sched$compute()
  preds_delayed <- fit_delayed$predict()

  set.seed(123)
  options(sl3.enable.future = TRUE) # the default
  fit <- lrnr_rf$train(task)
  preds <- fit$predict()
  expect_equal(as.numeric(preds_delayed), as.numeric(preds))
})

test_that("SequentialJob is reproducible", {
  set.seed(123)
  test_delayed <- delayed_learner_train(lrnr_rf, task)
  sched <- Scheduler$new(test_delayed, SequentialJob)
  fit_delayed <- sched$compute()
  preds_delayed <- fit_delayed$predict()

  set.seed(123)
  options(sl3.enable.future = FALSE)
  fit <- lrnr_rf$train(task)
  preds <- fit$predict()
  expect_equal(as.numeric(preds_delayed), as.numeric(preds))
})
