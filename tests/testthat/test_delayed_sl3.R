setwd("~/Dropbox/gates/sl3")
load_all()
library(delayed)
library(testthat)
library(SuperLearner)
library(future)
library(future.batchtools)
context("Delayed sl3")

plan(sequential)

data(cpp)
cpp <- cpp[!is.na(cpp[, "haz"]), ]
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
cpp[is.na(cpp)] <- 0
# cpp <- cpp[sample(nrow(cpp),10000,replace=T),]
outcome <- "haz"
# cpp <- cpp[1:150, ]

task <- sl3_Task$new(cpp, covariates = covars, outcome = outcome)

sl_screen_glmnet <- Lrnr_pkg_SuperLearner_screener$new("screen.glmnet")
sl_glmnet <- Lrnr_pkg_SuperLearner$new("SL.glmnet")
random_forest <- Lrnr_randomForest$new()
glm_fast <- Lrnr_glm_fast$new()
# xgb <- Lrnr_xgboost(nrounds=50)

stack <- Stack$new(random_forest, sl_glmnet, glm_fast)
pipeline <- Pipeline$new(sl_screen_glmnet, stack)
cv_rf <- Lrnr_cv$new(random_forest, folds=task$folds)
cv_stack <- Lrnr_cv$new(stack, folds=task$folds)
cv_pipeline <- Lrnr_cv$new(pipeline, folds=task$folds)

library(parallel)
test <- delayed_learner_train(cv_stack, task)
# plan(multicore, workers=16)
# plan(multisession, persistent = TRUE, workers=16)
# plan(sequential)
# system.time({
#   sched <- Scheduler$new(test, FutureJob, nworkers=16, verbose = TRUE)
#   cv_fit <- sched$compute()
# })
# 

# workers <- replicate(16, MCWorker$new())
# test <- delayed_learner_train(cv_pipeline, task)
# system.time({
#   sched <- Scheduler$new(test, WorkerJob, workers, verbose=TRUE)
#   cv_fit <- sched$compute()
# })

test <- delayed_learner_train(cv_pipeline, task)
system.time({
  sched <- Scheduler$new(test, SequentialJob)
  cv_fit <- sched$compute()
})
