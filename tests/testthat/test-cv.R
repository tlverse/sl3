context("test-cv.R -- Cross-validation fold handling")
library(origami)

data(cpp_imputed)
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
outcome <- "haz"
task <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome)

test_that("task will self-generate folds for 10-fold CV", expect_length(
  task$folds,
  10
))

glm_learner <- Lrnr_glm$new()
cv_glm <- Lrnr_cv$new(glm_learner, full_fit = TRUE)
cv_glm_fit <- cv_glm$train(task)
# debug_predict(cv_glm_fit)
cv_glm_fit$predict()
test_that("Lrnr_cv will use folds from task", {
  expect_equal(task$folds, cv_glm_fit$fit_object$folds)
})

folds <- make_folds(cpp_imputed, V = 5)
task_2 <- sl3_Task$new(cpp_imputed,
  covariates = covars, outcome = outcome,
  folds = folds
)
test_that("task will accept custom folds", expect_length(task_2$folds, 5))

test_that("we can generate predictions", {
  expect_equal(length(cv_glm_fit$predict()), task_2$nrow)
})

cv_glm_2 <- Lrnr_cv$new(glm_learner, folds = make_folds(cpp_imputed, V = 10))
cv_glm_fit_2 <- cv_glm_2$train(task_2)
cv_glm_fit_2$cv_risk(loss_squared_error)
test_that("Lrnr_cv can override folds from task", {
  expect_equal(cv_glm_fit_2$params$folds, cv_glm_fit_2$fit_object$folds)
})

glm_fit <- glm_learner$train(task)
test_that(
  "Lrnr_cv$predict_fold can generate full sample predictions",
  expect_equal(
    cv_glm_fit$predict_fold(task, "full"),
    glm_fit$predict(task)
  )
)

test_that(
  "Lrnr_cv$predict_fold can generate split specific predictions",
  expect_equal(
    cv_glm_fit$predict_fold(task, 1),
    cv_glm_fit$fit_object$fold_fits[[1]]$predict(task)
  )
)

test_that(
  "Lrnr_cv$predict_fold can generate cross-validated predictions",
  expect_equal(
    cv_glm_fit$predict_fold(task, "validation"),
    cv_glm_fit$predict(task)
  )
)

test_that("Lrnr_cv$predict_fold throws an error on a bad fold_number", {
  expect_error(cv_glm_fit$predict_fold(task, "junk"))
})


#### verify cv risk for timeseries context
library(origami)
trend_all <- 11:130 + rnorm(120, sd = 2)
trend_all <- data.frame(data = trend_all)

folds <- origami::make_folds(trend_all$data,
  fold_fun = folds_rolling_window, window_size = 50,
  validation_size = 10, gap = 0, batch = 5
)

lrnr_glm <- make_learner(Lrnr_glm)
lrnr_mean <- make_learner(Lrnr_mean)
sl <- make_learner(Lrnr_sl, list(lrnr_glm, lrnr_mean))
task <- sl3_Task$new(trend_all,
  covariates = "data",
  outcome = "data", folds = folds
)
fit <- sl$train(task)
fit$predict_fold(task, "validation")
cv_risk_table <- fit$cv_risk(loss_squared_error)

# GLM should be perfect here because outcome=covariate
expect_equal(cv_risk_table$coefficients[[1]], 1)
expect_equal(cv_risk_table$risk[[1]], 0)

################################# test LOOCV ###################################
test_loocv_learner <- function(learner, loocv_task, ...) {
  # test learner definition this requires that a learner can be instantiated with
  # only default arguments. Not sure if this is a reasonable requirement
  learner_obj <- make_learner(learner, ...)
  print(sprintf("Testing LOOCV with Learner: %s", learner_obj$name))
  cv_learner <- Lrnr_cv$new(learner_obj, full_fit = TRUE)

  # test learner training
  fit_obj <- cv_learner$train(loocv_task)
  test_that("Learner can be trained on data", expect_true(fit_obj$is_trained))

  # test learner prediction
  train_preds <- fit_obj$predict()
  test_that("Learner can generate training set predictions", expect_equal(
    sl3:::safe_dim(train_preds)[1],
    length(loocv_task$Y)
  ))

  # test learner chaining
  chained_task <- fit_obj$chain()
  test_that("Chaining returns a task", expect_true(is(chained_task, "sl3_Task")))
  test_that("Chaining returns the correct number of rows", expect_equal(
    nrow(chained_task$X),
    nrow(loocv_task$X)
  ))

  preds_fold1 <- fit_obj$predict_fold(loocv_task, 1)
  preds_full <- fit_obj$predict_fold(loocv_task, "full")
  preds_valid <- fit_obj$predict_fold(loocv_task, "validation")
  validation_task <- validation(loocv_task, fold = loocv_task$folds[[1]])
  validation_preds <- fit_obj$fit_object$fold_fits[[1]]$predict(validation_task)
  test_that("Learners do not error under LOOCV", {
    expect_false(any(is.na(preds_valid)))
    expect_false(any(is.na(preds_fold1)))
    expect_false(any(is.na(preds_full)))
    expect_false(any(is.na(validation_preds)))
  })
}

# make task
smol_d <- cpp_imputed[1:20, ]
expect_warning(
  loocv_folds <- make_folds(n = smol_d, fold_fun = folds_vfold, V = nrow(smol_d))
)

loocv_task <- sl3_Task$new(
  smol_d,
  covariates = covars, outcome = outcome, folds = loocv_folds
)

# get learners
cont_learners <- sl3::sl3_list_learners("continuous")
bin_learners <- sl3::sl3_list_learners("binomial")
# bin_learners[-which(bin_learners %in% cont_learners)] 0
ts <- sl3::sl3_list_learners("timeseries")
screen <- sl3::sl3_list_learners("screener")
wrap <- sl3::sl3_list_learners("wrapper")
h2o <- sl3::sl3_list_learners("h2o")
learners <- cont_learners[-which(cont_learners %in% c(ts, screen, wrap, h2o))]

# test all learners
result <- lapply(learners, test_loocv_learner, loocv_task)
# Failed on Lrnr_gam_NULL_NULL_GCV.Cp
# Error in (function (formula, family = gaussian(), data = list(), weights = NULL,  :
# Model has more coefficients than data

error_idx <- grep("Lrnr_gam", learners)
learners2 <- learners[-error_idx]
result2 <- lapply(learners2[error_idx:length(learners2)], test_loocv_learner, loocv_task)
# Failed on Lrnr_gbm_10000_2_0.001
# Error in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli",  :
# The data set is too small or the subsampling rate is too large: `nTrain * bag.fraction <= n.minobsinnode`

error_idx <- grep("Lrnr_gbm", learners2)
learners3 <- learners2[-error_idx]
result3 <- lapply(learners3[error_idx:length(learners3)], test_loocv_learner, loocv_task)
# Failed on Lrnr_hal9001_3_glmnet_10_TRUE_NULL_TRUE_FALSE_NULL_TRUE
# Error in h(simpleError(msg, call)) :
#   error in evaluating the argument 'x' in selecting a method for function 'as.matrix': error in evaluating the argument 'x' in selecting a method for function 'cbind2': data is too long

error_idx <- grep("Lrnr_hal", learners3)
learners4 <- learners3[-error_idx]
result4 <- lapply(learners4[error_idx:length(learners4)], test_loocv_learner, loocv_task)
# all pass
