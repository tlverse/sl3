context("test_hal9001.R -- Lrnr_hal9001")

if (FALSE) {
  setwd("..")
  setwd("..")
  getwd()
  library("devtools")
  document()
  load_all("./") # load all R files in /R and datasets in /data. Ignores NAMESPACE:
  devtools::check() # runs full check
  setwd("..")
  install("sl3", build_vignettes = FALSE, dependencies = FALSE) # INSTALL W/ devtools:
}

library(testthat)
library(sl3)
# library(data.table) library(origami)
library(SuperLearner)
set.seed(1)

data(cpp_imputed)
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
outcome <- "haz"

task <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome)

interactions <- list(c("apgar1", "apgar5"))
task_with_interactions <- task$add_interactions(interactions)

# test_that("Lrnr_hal9001 works with empty X (intercept-only)", {
#   fglm_learner <- Lrnr_hal9001$new()
#   empty_task <- sl3_Task$new(cpp_imputed, covariates = NULL, outcome = outcome)
#   fGLM_fit <- fglm_learner$base_train(empty_task)
#   fglm_preds <- fGLM_fit$predict()
# })

# test_that("Lrnr_glm and Lrnr_hal9001 works with empty X (intercept-only)", {
#   glm_learner <- Lrnr_glm$new()
#   fglm_learner <- Lrnr_hal9001$new()
#   GLM_fit <- glm_learner$train(task)
#   glm_preds <- GLM_fit$predict()
#   fGLM_fit <- fglm_learner$train(task)
#   fglm_preds <- fGLM_fit$predict()
#   expect_true(all.equal(as.vector(glm_preds), as.vector(fglm_preds)))
# })

test_that("Lrnr_hal9001 trains on a subset of covariates (predictors)", {
  fglm_learner <- Lrnr_hal9001$new(covariates = c("apgar1", "apgar5", "apgar1_apgar5"))
  fGLM_fit <- fglm_learner$train(task_with_interactions)
  
  # print(fGLM_fit) str(fGLM_fit$params)
  fglm_preds_3 <- fGLM_fit$predict()
  
  # expect_true(sum(fglm_preds_3 - glm_preds_3) < 10 ^ (-10))
  # expect_true(all.equal(as.vector(glm_preds_3), as.vector(fglm_preds_3)))
})