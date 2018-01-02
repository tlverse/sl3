context("test-pca.R -- Lrnr_pca with and without Pipelines")
library(origami)

# data
data(cpp_imputed)
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
outcome <- "haz"
task <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome)

test_that("task will self-generate folds for 10-fold CV", expect_length(
  task$folds,
  10
))

# define learners
glm_learner <- Lrnr_glm$new()
pca_learner <- Lrnr_pca$new()
pca_and_glm <- Pipeline$new(pca_learner, glm_learner)
stack <- Stack$new(pca_and_glm, glm_learner)
stack_fit <- stack$train(task)
stack_fit_out <- stack_fit$predict()

# test_that("Lrnr_cv will use folds from task",
# expect_equal(task$folds, cv_glm_fit$fit_object$folds))

# folds <- make_folds(cpp_imputed, V = 5)
# task_2 <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome, folds = folds)
# test_that("task will accept custom folds", expect_length(task_2$folds, 5))

# cv_glm_2 <- Lrnr_cv$new(glm_learner, folds = make_folds(cpp_imputed, V = 10))
# cv_glm_fit_2 <- cv_glm_2$train(task_2)
# test_that("Lrnr_cv can override folds from task", expect_equal(
# cv_glm_fit_2$params$folds,
# cv_glm_fit_2$fit_object$folds
# ))
