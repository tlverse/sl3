library(testthat)
context("test_pipeline.R -- Basic pipeline functionality")

library(sl3)
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

chain_y <- function(learner, task){
 preds <- learner$predict(task) 
 pred_dt <- data.table(preds)
 if(ncol(pred_dt)>1){
   stop("chain_y should only be used with learners that return a single prediction per observation")
 }
 
 setnames(pred_dt, names(pred_dt), learner$name)
 
 # add predictions as new column
 new_col_names <- task$add_columns(learner$fit_uuid, pred_dt)
 # prediction becomes outcome
 return(task$next_in_chain(outcome = names(pred_dt), column_names = new_col_names))
}

glm_outcome <- customize_chain(glm_learner, chain_y)
pipe2 = make_learner(Pipeline, glm_outcome, glm_learner)
fit2 <- pipe2$train(task)

