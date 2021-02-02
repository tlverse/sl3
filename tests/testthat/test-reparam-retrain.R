library(testthat)
context("test_reparameterize-retrain.R -- Learner reparameterization & retraining")

library(sl3)
library(origami)

data(cpp_imputed)
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
outcome <- "haz"
task <- sl3_Task$new(data.table::copy(cpp_imputed), covariates = covars, outcome = outcome)

glm_learner <- Lrnr_glm$new()
new_params <- list(covariates = setdiff(covars, "sexn"))
glm_sub <- glm_learner$reparameterize(new_params)

full_fit <- glm_learner$train(task)
sub_fit <- glm_sub$train(task)
test_that("We can reparameterize an untrained model", {
  expect_equal(setdiff(names(coef(full_fit)), names(coef(sub_fit))), "sexn")
})

reparam_lrnr <- full_fit$reparameterize(new_params)
reparam_fit <- reparam_lrnr$train(task)
test_that("We can reparameterize a trained model and refit", {
  expect_equal(setdiff(names(coef(full_fit)), names(coef(reparam_fit))), "sexn")
})

new_covars_task <- sl3_Task$new(data.table::copy(cpp_imputed),
  covariates = covars[-7], outcome = outcome
)
test_that("We cannot retrain a model on a new task with train", {
  expect_error(full_fit$train(new_covars_task))
})

new_covars_task_fit <- full_fit$retrain(new_covars_task)
test_that("We can retrain a model on a new task with new covariates", {
  expect_equal(setdiff(names(coef(full_fit)), names(coef(new_covars_task_fit))), "sexn")
  expect_equal(coef(reparam_fit), coef(new_covars_task_fit))
})

new_outcome_type_task <- sl3_Task$new(data.table::copy(cpp_imputed),
  covariates = covars[-7], outcome = "sexn"
)
new_outcome_type_task_fit <- full_fit$retrain(new_outcome_type_task)
test_that("We can retrain a model on a new task with new covariates and outcome", {
  expect_true(new_outcome_type_task_fit$is_trained)
  expect_equal(new_outcome_type_task_fit$training_task$outcome_type$type, "binomial")
})
