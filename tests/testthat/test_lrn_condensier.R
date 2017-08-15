# library(sl3)
library(testthat)
# library(h2o)
# h2o::h2o.init(nthread = 1);

#define test dataset
data(mtcars)
task=sl3_Task$new(mtcars,covariates=c("cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb"),outcome="mpg")
task2=sl3_Task$new(mtcars,covariates=c("cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb"),outcome="mpg")

test_learner=function(learner, task, ...){
  #test learner definition
  #this requires that a learner can be instantiated with only default arguments. Not sure if this is a reasonable requirement
  learner_obj=Lrnr_condensier$new(...)
  print(sprintf("Testing Learner: %s",learner_obj$name))

  #test learner training
  fit_obj=learner_obj$train(task)
  test_that("Learner can be trained on data",expect_true(fit_obj$is_trained))

  #test learner prediction
  train_preds=fit_obj$predict()
  # print(train_preds)
  test_that("Learner can generate training set predictions",expect_equal(sl3:::safe_dim(train_preds)[1],nrow(task$X)))

  holdout_preds=fit_obj$predict(task2)
  # print(holdout_preds)
  test_that("Learner can generate holdout set predictions",expect_equal(train_preds[["likelihood"]],holdout_preds[["likelihood"]]))

  #test learner chaining
  chained_task=fit_obj$chain()
  test_that("Chaining returns a task",expect_true(is(chained_task,"sl3_Task")))
  test_that("Chaining returns the correct number of rows",expect_equal(nrow(chained_task$X),nrow(task$X)))
}

options(sl.verbose = FALSE)

system.time(test_learner(Lrnr_condensier, task))
## currently fails:
# system.time(test_learner(Lrnr_condensier, task, bin_estimator = Lrnr_glm$new(family = "binomial")))

system.time(test_learner(Lrnr_condensier, task, bin_estimator = condensier::speedglmR6$new()))

## For some reason some of the predicted values (likelihood) are > 1. This should never happen!!
system.time(test_learner(Lrnr_condensier, task, nbins = 10))
system.time(test_learner(Lrnr_condensier, task, nbins = 10, bin_estimator = condensier::speedglmR6$new()))

## currently fails (response cannot be constant), need to have a glm fall-back:
# system.time(test_learner(Lrnr_condensier, task, bin_estimator = Lrnr_h2o_glm$new(family = "binomial")))
## currently fails (response cannot be constant), need to have a glm fall-back:
# system.time(test_learner(Lrnr_condensier, task, bin_estimator = Lrnr_h2o_grid$new(algorithm = "gbm")))


# test_learner(Lrnr_glm_fast, task)
# test_learner(Lrnr_h2o_glm, task)
# test_learner(Lrnr_h2o_grid, task, algorithm = "glm")
# test_learner(Lrnr_h2o_grid, task, algorithm = "gbm")
# test_learner(Lrnr_h2o_grid, task, algorithm = "randomForest")
# test_learner(Lrnr_h2o_grid, task, algorithm = "kmeans")
# test_learner(Lrnr_h2o_grid, task, algorithm = "deeplearning")
# ## todo: works only with categorical (factor) outcomes, need separate host of tests for classification
# # test_learner(Lrnr_h2o_grid, task, algorithm = "naivebayes")
# test_learner(Lrnr_h2o_grid, task, algorithm = "pca", k = 2, impute_missing = TRUE)

# h2o::h2o.shutdown(prompt = FALSE); Sys.sleep(3)
