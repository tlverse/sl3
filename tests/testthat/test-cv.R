context("test-cv.R -- Cross-validation fold handling")

library(origami)
options(java.parameters = "-Xmx2500m")
data(cpp_imputed)
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
outcome <- "haz"
task <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome)

test_that("Task will self-generate 10-fold CV folds", {
  expect_length(task$folds, 10)
})

glm_learner <- Lrnr_glm$new()
cv_glm <- Lrnr_cv$new(glm_learner, full_fit = TRUE)
cv_glm_fit <- cv_glm$train(task)
# debug_predict(cv_glm_fit)
preds <- cv_glm_fit$predict()

test_that("Lrnr_cv will use folds from task", {
  expect_equal(task$folds, cv_glm_fit$fit_object$folds)
})

folds <- make_folds(cpp_imputed, V = 5)
task_2 <- sl3_Task$new(cpp_imputed, covars, outcome, folds = folds)
test_that("Task will accept custom folds", expect_length(task_2$folds, 5))

test_that("We can generate predictions", {
  expect_equal(length(cv_glm_fit$predict()), task_2$nrow)
})

cv_glm_2 <- Lrnr_cv$new(glm_learner, folds = make_folds(cpp_imputed, V = 10))
cv_glm_fit_2 <- cv_glm_2$train(task_2)
risk <- cv_glm_fit_2$cv_risk(loss_squared_error)
test_that("Lrnr_cv can override folds from task", {
  expect_equal(cv_glm_fit_2$params$folds, cv_glm_fit_2$fit_object$folds)
})

glm_fit <- glm_learner$train(task)
test_that("Lrnr_cv$predict_fold can generate full sample predictions", {
  expect_equal(cv_glm_fit$predict_fold(task, "full"), glm_fit$predict(task))
})


test_that("Lrnr_cv$predict_fold can generate split specific predictions", {
  expect_equal(
    cv_glm_fit$predict_fold(task, 1),
    cv_glm_fit$fit_object$fold_fits[[1]]$predict(task)
  )
})

test_that("Lrnr_cv$predict_fold can generate cross-validated predictions", {
  expect_equal(
    cv_glm_fit$predict_fold(task, "validation"),
    cv_glm_fit$predict(task)
  )
})

test_that("Lrnr_cv$predict_fold throws an error on a bad fold_number", {
  expect_error(suppressWarnings(cv_glm_fit$predict_fold(task, "junk")))
})


##################### verify cv risk for timeseries context ####################
trend_all <- 11:130 + rnorm(120, sd = 2)
trend_all <- data.frame(data = trend_all)

folds <- origami::make_folds(trend_all$data,
  fold_fun = folds_rolling_window, window_size = 50,
  validation_size = 10, gap = 0, batch = 5
)

lrnr_glm <- make_learner(Lrnr_glm)
lrnr_mean <- make_learner(Lrnr_mean)
sl <- make_learner(Lrnr_sl, learners = list(lrnr_glm, lrnr_mean))
task <- sl3_Task$new(trend_all, "data", "data", folds = folds)
fit <- sl$train(task)
preds <- fit$predict_fold(task, "validation")
cv_risk_table <- fit$cv_risk(loss_squared_error)

# GLM should be perfect here because outcome = covariate
test_that("GLM is perfect when outcome = covariate", {
  expect_equal(cv_risk_table$coefficients[[1]], 1)
  expect_equal(cv_risk_table$MSE[[1]], 0)
})

################################# test LOOCV ###################################
test_loocv_learner <- function(learner, loocv_task, ...) {
  learner_obj <- make_learner(learner, ...)
  print(sprintf("Testing LOOCV with Learner: %s", learner_obj$name))
  cv_learner <- Lrnr_cv$new(learner_obj, full_fit = TRUE)

  print("Testing training")
  # test learner training
  fit_obj <- cv_learner$train(loocv_task)
  test_that("Learner can be trained on data", expect_true(fit_obj$is_trained))

  # test learner prediction
  print("Testing predict")
  train_preds <- fit_obj$predict()
  test_that("Learner can generate training set predictions", expect_equal(
    sl3:::safe_dim(train_preds)[1],
    length(loocv_task$Y)
  ))

  # test learner chaining
  chained_task <- fit_obj$chain()
  test_that("Chaining returns a task", expect_true(is(
    chained_task,
    "sl3_Task"
  )))
  test_that("Chaining returns the correct number of rows", expect_equal(
    nrow(chained_task$X),
    nrow(loocv_task$X)
  ))

  preds_fold1 <- fit_obj$predict_fold(loocv_task, 1)
  preds_full <- fit_obj$predict_fold(loocv_task, "full")
  preds_valid <- fit_obj$predict_fold(loocv_task, "validation")
  validation_task <- validation(loocv_task, fold = loocv_task$folds[[1]])
  validation_preds <-
    fit_obj$fit_object$fold_fits[[1]]$predict(validation_task)
  test_that("Learners do not error under LOOCV", {
    expect_false(any(is.na(preds_valid)))
    expect_false(any(is.na(preds_fold1)))
    expect_false(any(is.na(preds_full)))
    expect_false(any(is.na(validation_preds)))
  })
}

# make task with LOOCV
d <- cpp_imputed[1:50, ]
expect_warning({
  loocv_folds <- make_folds(n = d, fold_fun = folds_vfold, V = 50)
})
loocv_task <- sl3_Task$new(d, covars, outcome, folds = loocv_folds)

# get learners
cont_learners <- sl3::sl3_list_learners("continuous")
bin_learners <- sl3::sl3_list_learners("binomial")
# bin_learners[-which(bin_learners %in% cont_learners)] 0
ts <- sl3::sl3_list_learners("timeseries")
screen <- sl3::sl3_list_learners("screener")
wrap <- sl3::sl3_list_learners("wrapper")
h2o <- sl3::sl3_list_learners("h2o")
learners <- cont_learners[-which(cont_learners %in% c(ts, screen, wrap, h2o))]

# remove glm_semiparametric, as LOOCV is tested w it in test-glm-semiparametric
learners <- learners[!(learners == "Lrnr_glm_semiparametric")]

# remove LightGBM on Windows
if (Sys.info()["sysname"] == "Windows") {
  learners <- learners[!(learners == "Lrnr_lightgbm")]
}

# test all relevant learners
learners <- learners[!(learners == "Lrnr_grfcate")]
lapply(learners, test_loocv_learner, loocv_task)
test_loocv_learner("Lrnr_grfcate", loocv_task, A = "apgar1")
