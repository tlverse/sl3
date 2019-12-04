library(delayed)
library(testthat)
library(SuperLearner)
library(future)
context("Delayed sl3")

ncores <- future::availableCores() / 2

plan(sequential)

data(cpp)
cpp <- cpp[!is.na(cpp[, "haz"]), ]
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
cpp[is.na(cpp)] <- 0
cpp <- cpp[sample(nrow(cpp), 10000, replace = T), ]
# cpp <- cpp[1:150, ]
outcome <- "haz"


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

# sl3 sequential
plan(sequential)
test <- delayed_learner_train(sl, task)
system.time({
  sched <- Scheduler$new(test, SequentialJob)
  cv_fit <- sched$compute()
})
# macbook
# user  system elapsed
# 227.038   5.566 234.053
# 16 core compute server
# user  system elapsed
# 266.308   2.796 269.078

# sl3 multicore (hyperthreaded)
test <- delayed_learner_train(sl, task)
plan(multicore, workers = ncores * 2)
system.time({
  sched <- Scheduler$new(test, FutureJob, nworkers = ncores * 2, verbose = FALSE)
  cv_fit <- sched$compute()
})
# macbook
# user  system elapsed
# 332.150  27.066 110.942
# compute server
# user  system elapsed
# 337.640  11.104  37.863

# sl3 multicore (not hyperthreaded)
test <- delayed_learner_train(sl, task)
system.time({
  sched <- Scheduler$new(test, FutureJob, nworkers = ncores, verbose = FALSE)
  cv_fit <- sched$compute()
})
# macbook
# user  system elapsed
# 211.840  20.079 141.281
# compute server
# user  system elapsed
# 303.492  12.964  33.448

# sl3 multisession (not hyperthreaded)
test <- delayed_learner_train(sl, task)
plan(multisession, workers = ncores)
# cl <- future:::ClusterRegistry("get")
# clusterCall(cl, function(){options("sl.save.training"=FALSE)})
system.time({
  sched <- Scheduler$new(test, FutureJob, nworkers = ncores, verbose = FALSE)
  # delayed:::plot_delayed_shiny(sched)
  cv_fit <- sched$compute()
})
# macbook
# user  system elapsed
# 13.402   4.377 171.005
# compute server
# user  system elapsed
# 8.496   1.572  39.115

# sl3 multicore, reduce fit size (hyperthreaded)
options("sl3.save.training" = FALSE)
sl <- Lrnr_sl$new(list(sl_random_forest, sl_glmnet, sl_glm), nnls_lrnr, keep_extra = FALSE)

test <- delayed_learner_train(sl, task)
plan(multicore, workers = ncores * 2)
system.time({
  sched <- Scheduler$new(test, FutureJob, nworkers = ncores * 2, verbose = FALSE)
  cv_fit <- sched$compute()
})
# macbook
# user  system elapsed
# 343.318  22.030 105.594
# compute server
# user  system elapsed
# 315.908  19.340  33.920

# SuperLearner sequential
system.time({
  SuperLearner(task$Y, as.data.frame(task$X),
    newX = NULL, family = gaussian(), SL.library = c("SL.glmnet", "SL.randomForest", "SL.glm"),
    method = "method.NNLS", id = NULL, verbose = FALSE,
    control = list(), cvControl = list(), obsWeights = NULL, env = parent.frame()
  )
})
# macbook
# user  system elapsed
# 226.006  11.005 239.479
# compute server
# user  system elapsed
# 262.088   1.544 263.614

# SuperLearner multicore
options(mc.cores = ncores * 2)
system.time({
  mcSuperLearner(task$Y, as.data.frame(task$X),
    newX = NULL, family = gaussian(), SL.library = c("SL.glmnet", "SL.randomForest", "SL.glm"),
    method = "method.NNLS", id = NULL, verbose = FALSE,
    control = list(), cvControl = list(), obsWeights = NULL, env = parent.frame()
  )
})
# macbook
# user  system elapsed
# 284.488  17.208 117.978
# compute server
# user  system elapsed
# 273.076   9.604  58.310

# SuperLearner multisession
library(parallel)
cl <- makeCluster(ncores * 2, type = "PSOCK") # can use different types here
clusterSetRNGStream(cl, iseed = 2343)
system.time({
  snowSuperLearner(
    cluster = cl, task$Y, as.data.frame(task$X), newX = NULL, family = gaussian(), SL.library = c("SL.glmnet", "SL.randomForest", "SL.glm"),
    method = "method.NNLS", id = NULL, verbose = FALSE,
    control = list(), cvControl = list(), obsWeights = NULL
  )
})
# doesn't work
