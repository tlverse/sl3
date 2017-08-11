library(sl3)
library(testthat)
library(h2o)
h2o::h2o.init(nthread = 1); Sys.sleep(1)

#define test dataset
data(mtcars)
task=Learner_Task$new(mtcars,covariates=c("cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb"),outcome="mpg")
task2=Learner_Task$new(mtcars,covariates=c("cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb"),outcome="mpg")

test_learner=function(learner, task, ...){

	#test learner definition
  #this requires that a learner can be instantiated with only default arguments. Not sure if this is a reasonable requirement
	learner_obj=learner$new(...)
	print(sprintf("Testing Learner: %s",learner_obj$name))

	#test learner training
  fit_obj=learner_obj$train(task)
  test_that("Learner can be trained on data",expect_true(fit_obj$is_trained))

	#test learner prediction
  train_preds=fit_obj$predict()
  test_that("Learner can generate training set predictions",expect_equal(sl3:::safe_dim(train_preds)[1],nrow(task$X)))

  holdout_preds=fit_obj$predict(task2)
  test_that("Learner can generate holdout set predictions",expect_equal(train_preds,holdout_preds))

  #test learner chaining
  chained_task=fit_obj$chain()
  test_that("Chaining returns a task",expect_true(is(chained_task,"Learner_Task")))
  test_that("Chaining returns the correct number of rows",expect_equal(nrow(chained_task$X),nrow(task$X)))
}

test_learner(GLM_Learner, task)
test_learner(GLMfast_Learner, task)
test_learner(h2o_GLM_Learner, task)
test_learner(h2o_grid_Learner, task, algorithm = "glm")
test_learner(h2o_grid_Learner, task, algorithm = "gbm")
test_learner(h2o_grid_Learner, task, algorithm = "randomForest")
test_learner(h2o_grid_Learner, task, algorithm = "kmeans")
test_learner(h2o_grid_Learner, task, algorithm = "deeplearning")
## todo: works only with categorical (factor) outcomes, need separate host of tests for classification
# test_learner(h2o_grid_Learner, task, algorithm = "naivebayes")
test_learner(h2o_grid_Learner, task, algorithm = "pca", k = 2, impute_missing = TRUE)

h2o::h2o.shutdown(prompt = FALSE)