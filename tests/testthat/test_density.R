library(sl3)
library(testthat)
context("test_density.R -- Lrnr_condensier")

# define test dataset
data(mtcars)
task <- sl3_Task$new(mtcars, covariates = c("cyl", "disp", "hp", "drat", "wt", "qsec",
    "vs", "am", "gear", "carb"), outcome = "mpg")
task2 <- sl3_Task$new(mtcars, covariates = c("cyl", "disp", "hp", "drat", "wt", "qsec",
    "vs", "am", "gear", "carb"), outcome = "mpg")

test_learner_dens <- function(learner, task, ...) {
    # test learner definition this requires that a learner can be instantiated with
    # only default arguments. Not sure if this is a reasonable requirement
    learner_obj <- Lrnr_condensier$new(...)
    print(sprintf("Testing Learner: %s", learner_obj$name))

    # test learner training
    fit_obj <- learner_obj$train(task)
    test_that("Learner can be trained on data", expect_true(fit_obj$is_trained))

    # test learner prediction
    train_preds <- fit_obj$predict()
    # print(train_preds)
    test_that("Learner can generate training set predictions", expect_equal(sl3:::safe_dim(train_preds)[1],
        nrow(task$X)))

    holdout_preds <- fit_obj$predict(task2)
    # print(holdout_preds)
    test_that("Learner can generate holdout set predictions", expect_equal(train_preds[["likelihood"]],
        holdout_preds[["likelihood"]]))

    # test learner chaining
    chained_task <- fit_obj$chain()
    test_that("Chaining returns a task", expect_true(is(chained_task, "sl3_Task")))
    test_that("Chaining returns the correct number of rows", expect_equal(nrow(chained_task$X),
        nrow(task$X)))
    return(train_preds)
}

op <- options(sl3.verbose = FALSE)

lrn_1 <- Lrnr_condensier$new(nbins = 6,
                            bin_method = "equal.len",
                            pool = FALSE,
                            bin_estimator = condensier::speedglmR6$new())
fit_lrn_1 <- lrn_1$base_train(task)

## w/ speedglm:
system.time(res_speedglm <- test_learner_dens(Lrnr_condensier, task))
system.time(res_speedglm <- test_learner_dens(Lrnr_condensier, task, covariates = c("cyl",
    "disp")))
# cbind(res_speedglm, mtcars) regular GLM:
system.time(res_GLM <- test_learner_dens(Lrnr_condensier, task, nbins = 6, bin_estimator = Lrnr_glm$new(family = "binomial")))
# cbind(res_GLM, mtcars) original speedglm learner class from condensier:
system.time(res_speedglm2 <- test_learner_dens(Lrnr_condensier, task, bin_estimator = condensier::speedglmR6$new()))
# cbind(res_speedglm2, mtcars) w/ xgboost, ***** seems to fail on travis ***** :
# system.time(res_XGB <- test_learner_dens(Lrnr_condensier, task, bin_estimator =
# Lrnr_xgboost$new(nrounds = 50, objective = 'reg:logistic') ) ) cbind(res_XGB, mtcars)
system.time(res_nbins <- test_learner_dens(Lrnr_condensier, task, nbins = 5))
# system.time(res_nbins2 <- test_learner_dens(Lrnr_condensier, task, nbins = 10,
# bin_estimator = condensier::speedglmR6$new())) cbind(res_nbins2, mtcars)

## h2o binary learners for density estimation:
# options(sl3.verbose = TRUE)
# Sys.sleep(1)
# h2o::h2o.init(nthread = 1)
# Sys.sleep(1)
# # ## some fits fail (response cannot be constant), in which case it engages the
# # glm fall-back:
# system.time(res_h2oGLM <- test_learner_dens(Lrnr_condensier, task, nbins = 3, max_n_bin = 50,
#                                             bin_estimator = Lrnr_h2o_glm$new(family = "binomial")))
# # cbind(res_h2oGLM, mtcars) ## some fits fail (response cannot be constant), in
# # which case it engages the glm fall-back: system.time(res_h2oGBM <-
# # test_learner_dens(Lrnr_condensier, task, bin_estimator =
# # Lrnr_h2o_grid$new(algorithm = 'gbm', distribution = 'bernoulli')))
# # cbind(res_h2oGBM, mtcars)
# h2o::h2o.shutdown(prompt = FALSE)
# Sys.sleep(3)
options(op)



