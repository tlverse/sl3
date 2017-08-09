#define test dataset
data(mtcars)
task=Learner_Task$new(mtcars,covariates=c("cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb"),outcome="mpg")
task2=Learner_Task$new(mtcars,covariates=c("cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb"),outcome="mpg")


test_learner=function(learner, task){

	#test learner definition
	learner_obj=learner$new()
	print(sprintf("Testing Learner: %s",learner_obj$name))

	#test learner training
  fit_obj=learner_obj$train(task)
  test_that("Learner can be trained on data",expect_true(fit_obj$is_trained))

	#test learner prediction
  train_preds=fit_obj$predict()
  test_that("Learner can generate training set predictions",expect_equal(length(train_preds),nrow(task$X)))

  holdout_preds=fit_obj$predict(task2)
  test_that("Learner can generate holdout set predictions",expect_equal(train_preds,holdout_preds))

  #test learner chaining

}

test_learner(GLM_Learner, task)
