library(delayed)
library(testthat)
library(SuperLearner)
library(future)
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
nnls_lrnr <- Lrnr_nnls$new()
# xgb <- Lrnr_xgboost(nrounds=50)

sl <- Lrnr_sl$new(list(random_forest, sl_glmnet, glm_fast), nnls_lrnr)

test <- delayed_learner_train(sl, task)
system.time({
  sched <- Scheduler$new(test, SequentialJob)
  cv_fit <- sched$compute()
})

# test <- delayed_learner_train(sl, task)
# plan(multicore, workers=2)
# system.time({
#   sched <- Scheduler$new(test, FutureJob, nworkers=2, verbose = TRUE)
#   cv_fit <- sched$compute()
# })
# options(mc.cores=16)
# system.time({
# mcSuperLearner(task$Y, as.data.frame(task$X), newX = NULL, family = gaussian(), SL.library=c("SL.glmnet","SL.randomForest","SL.glm"),
#              method = "method.NNLS", id = NULL, verbose = FALSE,
#              control = list(), cvControl = list(), obsWeights = NULL, env = parent.frame())
# })