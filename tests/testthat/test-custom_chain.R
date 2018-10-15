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
chain_custom <- function(self, task) {
  preds <- self$predict(task)
  pred_dt <- data.table(preds)

  setnames(pred_dt, names(pred_dt), self$name)

  # add predictions as new column
  new_col_names <- task$add_columns(pred_dt, self$fit_uuid)
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
