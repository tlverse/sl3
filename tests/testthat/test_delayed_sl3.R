library(delayed)
library(testthat)
library(SuperLearner)
library(future)
context("test_delayed_sl3.R -- manually delay learner fit")

plan(sequential)

data(cpp_imputed)
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
outcome <- "haz"

task <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome)

learners <- list(
  rf <- make_learner(Lrnr_randomForest),
  glmnet <- make_learner(Lrnr_glmnet),
  glm <- make_learner(Lrnr_glm_fast)
)

nnls_metalearner <- make_learner(Lrnr_nnls)
# xgb <- Lrnr_xgboost(nrounds=50)

sl <- make_learner(Lrnr_sl, learners, nnls_metalearner)

# sl3 sequential
test <- delayed_learner_train(sl, task)
system.time({
  sched <- Scheduler$new(test, SequentialJob)
  cv_fit <- sched$compute()
})
