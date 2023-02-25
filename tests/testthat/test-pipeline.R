context("test-pipeline.R -- Basic pipeline functionality")
library(origami)
library(SuperLearner)
library(data.table)

data(cpp_imputed)
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
outcome <- "haz"
task <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome)

glm_learner <- Lrnr_glm$new()

test_screen_pipe <- function(screen_name_SuperLearner) {
  set.seed(123)
  screen_learner <- Lrnr_pkg_SuperLearner_screener$new(screen_name_SuperLearner)
  screen_glm <- make_learner(Pipeline, screen_learner, glm_learner)
  fit <- screen_glm$train(task)
  
  expect_equal(fit$fit_object$learner_fits[[1]]$fit_object$selected,
               names(fit$fit_object$learner_fits$Lrnr_glm_TRUE$coefficients)[-1])
}

test_that("Pipeline pipes selected covariates from screening algorithms", {
  screens <- c("screen.glmnet", "screen.corP", "screen.corRank", 
               "screen.randomForest", "screen.SIS")
  lapply(screens, test_screen_pipe)
})
