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
cpp <- cpp[sample(nrow(cpp),10000,replace=T),]
outcome <- "haz"
# cpp <- cpp[1:150, ]

task <- sl3_Task$new(cpp, covariates = covars, outcome = outcome)

sl_screen_glmnet <- Lrnr_pkg_SuperLearner_screener$new("screen.glmnet")
sl_glmnet <- Lrnr_pkg_SuperLearner$new("SL.glmnet")
sl_random_forest <- Lrnr_pkg_SuperLearner$new("SL.randomForest")
sl_glm <- Lrnr_pkg_SuperLearner$new("SL.glm")
random_forest <- Lrnr_randomForest$new()
glm_fast <- Lrnr_glm_fast$new()
nnls_lrnr <- Lrnr_nnls$new()
# xgb <- Lrnr_xgboost(nrounds=50)

sl <- Lrnr_sl$new(list(sl_random_forest, sl_glmnet, sl_glm), nnls_lrnr)

#sl3 sequential
plan(sequential)
test <- delayed_learner_train(sl, task)
system.time({
  sched <- Scheduler$new(test, SequentialJob)
  cv_fit <- sched$compute()
})
# user  system elapsed 
# 227.038   5.566 234.053 

#sl3 multicore (hyperthreaded)
test <- delayed_learner_train(sl, task)
plan(multicore, workers=4)
system.time({
  sched <- Scheduler$new(test, FutureJob, nworkers=4, verbose = FALSE)
  cv_fit <- sched$compute()
})
# user  system elapsed 
# 332.150  27.066 110.942 

#sl3 multicore (not hyperthreaded)
test <- delayed_learner_train(sl, task)
system.time({
  sched <- Scheduler$new(test, FutureJob, nworkers=2, verbose = FALSE)
  cv_fit <- sched$compute()
})
# user  system elapsed 
# 211.840  20.079 141.281 


#sl3 multisession (hyperthreaded)
test <- delayed_learner_train(sl, task)
plan(multisession, workers=4)
system.time({
  sched <- Scheduler$new(test, FutureJob, nworkers=4, verbose = FALSE)
  cv_fit <- sched$compute()
})
# user  system elapsed 
# 122.682  26.707 211.257 

#sl3 multicore, reduce fit size (hyperthreaded)
options("sl3.save.training" = FALSE)
sl <- Lrnr_sl$new(list(sl_random_forest, sl_glmnet, sl_glm), nnls_lrnr, keep_extra = FALSE)

test <- delayed_learner_train(sl, task)
plan(multicore, workers=4)
system.time({
  sched <- Scheduler$new(test, FutureJob, nworkers=4, verbose = FALSE)
  cv_fit <- sched$compute()
})
# user  system elapsed 
# 343.318  22.030 105.594  



#SuperLearner sequential
system.time({
  SuperLearner(task$Y, as.data.frame(task$X), newX = NULL, family = gaussian(), SL.library=c("SL.glmnet","SL.randomForest","SL.glm"),
                 method = "method.NNLS", id = NULL, verbose = FALSE,
                 control = list(), cvControl = list(), obsWeights = NULL, env = parent.frame())
})
# user  system elapsed 
# 226.006  11.005 239.479

#SuperLearner multicore
options(mc.cores=4)
system.time({
mcSuperLearner(task$Y, as.data.frame(task$X), newX = NULL, family = gaussian(), SL.library=c("SL.glmnet","SL.randomForest","SL.glm"),
             method = "method.NNLS", id = NULL, verbose = FALSE,
             control = list(), cvControl = list(), obsWeights = NULL, env = parent.frame())
})
# user  system elapsed 
# 284.488  17.208 117.978 

#SuperLearner multisession
library(parallel)
cl <- makeCluster(4, type = "PSOCK") # can use different types here
clusterSetRNGStream(cl, iseed = 2343)
system.time({
snowSuperLearner(cluster=cl, task$Y, as.data.frame(task$X), newX = NULL, family = gaussian(), SL.library=c("SL.glmnet","SL.randomForest","SL.glm"),
               method = "method.NNLS", id = NULL, verbose = FALSE,
               control = list(), cvControl = list(), obsWeights = NULL)
})
#also doesn't work