library(sl3)
library(testthat)
library(origami)



data(cpp)
cpp <- cpp[!is.na(cpp[, "haz"]), ]
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
cpp[is.na(cpp)] <- 0
outcome <- "haz"
task <- sl3_Task$new(cpp, covariates = covars, outcome = outcome)

test_that("task will self-generate folds for 10-fold CV", expect_length(task$folds, 
    10))

folds <- make_folds(cpp, V = 5)
task2 <- sl3_Task$new(cpp, covariates = covars, outcome = outcome, folds = folds)
test_that("task will accept custom folds", expect_length(task2$folds, 5))

glm_learner <- Lrnr_glm$new()
cv_glm <- Lrnr_cv$new(glm_learner)
cv_glm_fit <- cv_glm$train(task2)
test_that("Lrnr_cv will use folds from task", expect_equal(task2$folds, cv_glm_fit$fit_object$folds))

cv_glm$params$folds <- make_folds(cpp, V = 10)
cv_glm_fit2 <- cv_glm$train(task2)
test_that("Lrnr_cv can override folds from task", expect_equal(cv_glm_fit2$params$folds, 
    cv_glm_fit2$fit_object$folds))
