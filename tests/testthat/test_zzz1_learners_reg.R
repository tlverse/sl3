library(sl3)
library(testthat)
# library(h2o)
# h2o::h2o.init(nthread = 1)

# define test dataset
data(mtcars)
task <- sl3_Task$new(mtcars, covariates = c("cyl", "disp", "hp", "drat", "wt", "qsec",
    "vs", "am", "gear", "carb"), outcome = "mpg")
task2 <- sl3_Task$new(mtcars, covariates = c("cyl", "disp", "hp", "drat", "wt", "qsec",
    "vs", "am", "gear", "carb"), outcome = "mpg")

test_learner <- function(learner, task, ...) {

    # test learner definition this requires that a learner can be instantiated with
    # only default arguments. Not sure if this is a reasonable requirement
    learner_obj <- learner$new(...)
    print(sprintf("Testing Learner: %s", learner_obj$name))

    # test learner training
    fit_obj <- learner_obj$train(task)
    test_that("Learner can be trained on data", expect_true(fit_obj$is_trained))

    # test learner prediction
    train_preds <- fit_obj$predict()
    test_that("Learner can generate training set predictions", expect_equal(sl3:::safe_dim(train_preds)[1],
        nrow(task$X)))

    holdout_preds <- fit_obj$predict(task2)
    test_that("Learner can generate holdout set predictions", expect_equal(train_preds,
        holdout_preds))

    # test learner chaining
    chained_task <- fit_obj$chain()
    test_that("Chaining returns a task", expect_true(is(chained_task, "sl3_Task")))
    test_that("Chaining returns the correct number of rows", expect_equal(nrow(chained_task$X),
        nrow(task$X)))
}

test_learner(Lrnr_glm, task)
test_learner(Lrnr_glm_fast, task)
# test_learner(Lrnr_h2o_glm, task)
# test_learner(Lrnr_h2o_grid, task, algorithm = "glm")
# test_learner(Lrnr_h2o_grid, task, algorithm = "gbm")
# test_learner(Lrnr_h2o_grid, task, algorithm = "randomForest")
# test_learner(Lrnr_h2o_grid, task, algorithm = "kmeans")
# test_learner(Lrnr_h2o_grid, task, algorithm = "deeplearning")

## test h2o classifiers and mutator:
# test_learner(Lrnr_h2o_classifier, task, algorithm = "naivebayes")
# test_learner(Lrnr_h2o_mutator, task, algorithm = "pca", k = 3, impute_missing = TRUE)
# h2o::h2o.shutdown(prompt = FALSE)
# Sys.sleep(3)

## test xgboost learner:
op <- options(sl3.verbose = TRUE)
test_learner(Lrnr_xgboost, task, nrounds = 10)  ## nrounds is always needed
test_learner(Lrnr_xgboost, task, nrounds = 10, objective = "reg:linear")  ## linear link function
# # test_learner(Lrnr_xgboost, task, nrounds = 50, objective = 'reg:logistic') ##
# logit-linear link function, need [0,1] outcomes
test_learner(Lrnr_xgboost, task, nrounds = 50, booster = "gblinear")  ## GLM, i.e., use linear model for at each boosting stage
test_learner(Lrnr_xgboost, task, nrounds = 10, booster = "gbtree")  ## GBM (default), i.e., use tree model for at each boosting stage
# test_learner(Lrnr_xgboost, task, nrounds = 50, booster = 'dart') ## another
# type of tree model
test_learner(Lrnr_xgboost, task, nrounds = 10, booster = "gbtree", covariates = c("cyl",
    "disp", "drat"), interactions = list(c("cyl", "disp"), c("hp", "drat")))
options(sl3.verbose = FALSE)
test_learner(Lrnr_xgboost, task, nrounds = 50)

options(op)


