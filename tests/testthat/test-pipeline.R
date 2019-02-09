context("test-pipeline.R -- Basic pipeline functionality")
library(origami)
library(SuperLearner)
library(data.table)

data(cpp_imputed)
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
outcome <- "haz"
task <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome)

screen_glmnet <- Lrnr_pkg_SuperLearner_screener$new("screen.glmnet")
glm_learner <- Lrnr_glm$new()
screen_glm <- make_learner(Pipeline, screen_glmnet, glm_learner)
fit <- screen_glm$train(task)
