library(testthat)
context("test_cv_sl.R -- cross-validated super learner")

options(sl3.verbose = TRUE)
library(sl3)
library(origami)
library(SuperLearner)

data(cpp_imputed)
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
outcome <- "haz"
task <- sl3_Task$new(data.table::copy(cpp_imputed),
  covariates = covars,
  outcome = outcome,
  folds = origami::make_folds(cpp_imputed, V = 3)
)
glm_learner <- Lrnr_glm$new()
glmnet_learner <- Lrnr_pkg_SuperLearner$new("SL.glmnet")
subset_apgar <- Lrnr_subset_covariates$new(covariates = c("apgar1", "apgar5"))
learners <- list(glm_learner, glmnet_learner, subset_apgar)
sl <- make_learner(Lrnr_sl, learners, glm_learner)
sl_fit <- sl$train(task)
cv_sl_fit <- cv_sl(sl_fit, task, loss_squared_error)
