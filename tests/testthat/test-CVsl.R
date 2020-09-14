library(testthat)
context("test_CV_sl.R -- cross-validated SL functionality")

options(sl3.verbose = TRUE)
library(sl3)
library(origami)
library(SuperLearner)

data(cpp_imputed)
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
outcome <- "haz"
task <- sl3_Task$new(data.table::copy(cpp_imputed),
  covariates = covars,
  outcome = outcome
)

glm_learner <- Lrnr_glm$new()
glmnet_learner <- Lrnr_pkg_SuperLearner$new("SL.glmnet")
subset_apgar <- Lrnr_subset_covariates$new(covariates = c("apgar1", "apgar5"))
learners <- list(glm_learner, glmnet_learner, subset_apgar)
sl1 <- make_learner(Lrnr_sl, learners, glm_learner)

sl1_fit <- sl1$train(task)
CVsl_fit <- CV_lrnr_sl(sl1_fit, task, loss_squared_error)
