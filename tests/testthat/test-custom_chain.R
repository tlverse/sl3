library(testthat)
context("test_pipeline.R -- Basic pipeline functionality")

library(sl3)
library(origami)
library(SuperLearner)
library(data.table)

data(cpp_imputed)
covars <- c(
  "apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs",
  "sexn"
)
outcome <- "haz"
task <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome)

glm_learner <- Lrnr_glm$new()

#### custom chain
<<<<<<< HEAD:tests/testthat/test_custom_chain.R
chain_custom <- function(learner, task) {
  preds <- learner$predict(task)
=======
chain_custom <- function(self, task) {
  preds <- self$predict(task)
>>>>>>> e38c43dae505af5bb8f02db5cf41d7d893eaae67:tests/testthat/test-custom_chain.R
  pred_dt <- data.table(preds)

  setnames(pred_dt, names(pred_dt), learner$name)

  # add predictions as new column
<<<<<<< HEAD:tests/testthat/test_custom_chain.R
  new_col_names <- task$add_columns(learner$fit_uuid, pred_dt)
=======
  new_col_names <- task$add_columns(pred_dt, self$fit_uuid)
>>>>>>> e38c43dae505af5bb8f02db5cf41d7d893eaae67:tests/testthat/test-custom_chain.R
  # prediction becomes outcome
  return(task$next_in_chain(
    outcome = names(pred_dt),
    column_names = new_col_names
  ))
}

# use new function custom_chain as the action upon calling $chain
glm_learner$custom_chain(chain_custom)
glm_fit <- glm_learner$train(task)

# check that the outcome in the chain task matches what's produced by predict
# NOTE: this is important because the outcome of the chain task is what's going
#       to get passed along, hence it should match the vanilla $predict result
test_that("Outcome in task produced by $chain matches result from $predict", {
  expect_equal(glm_fit$chain()$Y, glm_preds <- glm_fit$predict())
})
