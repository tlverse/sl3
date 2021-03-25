library(testthat)
context("test_modify_task.R -- Modify Task")

options(sl3.verbose = TRUE)
library(sl3)
library(origami)
library(SuperLearner)

data(cpp_imputed)
covars <- c(
  "apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs",
  "sexn"
)
outcome <- "haz"
task <- sl3_Task$new(cpp_imputed,
  covariates = covars, outcome = outcome,
  folds = make_folds(cpp_imputed, V=2)
)

glm_learner <- Lrnr_glm$new()
glmnet_learner <- Lrnr_glmnet$new(nlambda = 5)
learners <- Stack$new(glm_learner, glmnet_learner)
sl <- make_learner(Lrnr_sl, learners)
cv_sl <- make_learner(Lrnr_cv, sl)


change_folds <- function(task, V, ...){
  # do whatever to change the folds as necessary
  vfolds <- make_folds(task$nrow, V=V)
  new_task <- task$next_in_chain(folds=vfolds)
  return(new_task)
}

force_5fold <- make_learner(Lrnr_modify_task, change_folds, V=5)
sl_5fold <- make_learner(Pipeline, force_5fold, sl)
cv_sl_5fold <- make_learner(Lrnr_cv, sl_5fold)

fit <- cv_sl$train(task)
fit_5fold_internal <- cv_sl_5fold$train(task)

test_that("modify task can modify folds for downstream learners",{
  internal_folds <- length(fit$fit_object$fold_fits[[1]]$training_task$folds)
  internal_5folds <- length(fit_5fold_internal$fit_object$fold_fits[[1]]$fit_object$learner_fits[[2]]$training_task$folds)
  expect_equal(internal_folds, 10)
  expect_equal(internal_5folds, 5)
})
