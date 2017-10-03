library(sl3)
library(testthat)
library(origami)



data(cpp)
cpp <- cpp[!is.na(cpp[, "haz"]), ]
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
cpp[is.na(cpp)] <- 0
outcome <- "haz"
task <- sl3_Task$new(cpp, covariates = covars, outcome = outcome)

# 
# test_that("task will self-generate folds for 10-fold CV", expect_length(task$folds, 
#     10))
# 
# glm_learner <- Lrnr_glm$new()
# cv_glm <- Lrnr_cv$new(glm_learner)
# cv_glm_fit <- cv_glm$train(task)
# cv_chained <- cv_glm_fit$chain()
# 
# test_that("Lrnr_cv will use folds from task", expect_equal(task$folds, cv_glm_fit$fit_object$folds))
# folds <- task$folds
# training_task_1 <- training(task, fold=folds[[1]])
# validation_task_1 <- validation(task, fold=folds[[1]])
# glm_fit_1 <- glm_learner$train(training_task_1)
# validation_chain_1 <- glm_fit_1$base_chain(validation_task_1)
# internal_data <- task$data
# validation_internal_data <- validation(internal_data, fold=folds[[1]])
# z1 <- validation_internal_data$`85ca2e8c-a872-11e7-ab7d-a45e60d0422b_predictions`
# z2 <- validation_internal_data$`85e25e62-a872-11e7-ab7d-a45e60d0422b_predictions`
# 
# folds <- make_folds(cpp, V = 5)
# task_2 <- sl3_Task$new(cpp, covariates = covars, outcome = outcome, folds = folds)
# test_that("task will accept custom folds", expect_length(task_2$folds, 5))
# 
# 
# cv_glm_2 <- Lrnr_cv$new(glm_learner, folds = make_folds(cpp, V = 10))
# cv_glm_fit_2 <- cv_glm_2$train(task_2)
# test_that("Lrnr_cv can override folds from task", expect_equal(cv_glm_fit_2$params$folds, 
#     cv_glm_fit_2$fit_object$folds))
# 
# 
