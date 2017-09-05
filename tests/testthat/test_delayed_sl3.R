library(delayed)
# library(sl3)
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
cpp <- cpp[1:150, ]

task <- sl3_Task$new(cpp, covariates = covars, outcome = outcome)


sl_screen_glmnet <- Lrnr_pkg_SuperLearner_screener$new("screen.glmnet")
sl_random_forest <- Lrnr_pkg_SuperLearner$new("SL.randomForest")

sl_screen_fit <- delayed_learner_train(sl_screen_glmnet,task)
sl_screen_chained <- delayed_learner_fit_chain(sl_screen_fit)

# SuperLearner(task$Y,task$X, SL.library="SL.randomForest")
rf_fits <- lapply(1:10,function(x){delayed_learner_train(sl_random_forest,sl_screen_chained)})
glm_fast <- Lrnr_glm_fast$new()


stack <- Stack$new(sl_random_forest, glm_fast)
pipeline <- Pipeline$new(sl_screen_glmnet, stack)
cv_stack <- Lrnr_cv$new(pipeline, folds=task$folds)
# pretrain <- get("private",environment(stack$train))$.pretrain(sl_screen_chained)

test <- delayed_learner_train(cv_stack, task)
system.time({
  cv_fit <- test$compute()
})
cv_fit$predict()
# stack_fit <- test$compute()
# stack_fit$predict()
# 
# rf_preds <- lapply(rf_fits,function(rf_fit){delayed_learner_fit_predict(rf_fit,task)})
# rf_preds[[1]]$compute()
# 
# rf_fits[[1]]$state
# # rf_stack <- do.call(delayed_stack_learner_fits,rf_fits)
# plot(rf_stack$make_graph())
# 
# rf_fits[[1]]$state
# 
# mean_preds <- function(...){
#   mat=cbind(...)
#   rowMeans(mat)
# }
# 
# delayed_mp <- delayed_fun(mean_preds)
# # debugonce(delayed_mp)
# 
# 
# result <- do.call(delayed_mp,rf_preds)
# system.time({
# agg_preds=result$compute()
# })
# 

plot(test)
