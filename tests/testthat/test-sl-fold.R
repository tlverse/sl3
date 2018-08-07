library(testthat)
context("test_sl.R -- Basic Lrnr_sl functionality")

options(sl3.verbose = TRUE)
library(sl3)
library(origami)
library(SuperLearner)

data(cpp_imputed)
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
outcome <- "haz"
task <- sl3_Task$new(data.table::copy(cpp_imputed), covariates = covars, outcome = outcome)

glm_learner <- Lrnr_glm$new()
glmnet_learner <- Lrnr_pkg_SuperLearner$new("SL.glmnet")
subset_apgar <- Lrnr_subset_covariates$new(covariates = c("apgar1", "apgar5"))
learners <- list(glm_learner, glmnet_learner, subset_apgar)
sl1 <- make_learner(Lrnr_sl, learners, glm_learner)

sl_fit <- sl1$train(task)

fold1_predict <- sl_fit$predict_fold(task,1)
validation_predict <- sl_fit$predict_fold(task,0)
expect_false(all(fold1_predict==validation_predict))
expect_true(any(fold1_predict==validation_predict))

glm_fit <- glm_learner$train(task)
expect_warning(glm_fold1_predict <- glm_fit$predict_fold(task,1))
