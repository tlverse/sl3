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

test_that("Lrnr_cv will use folds from task", expect_equal(task$folds, cv_glm_fit$fit_object$folds))

folds <- make_folds(cpp_imputed, V = 5)
task_2 <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome, folds = folds)
test_that("task will accept custom folds", expect_length(task_2$folds, 5))

test_that("we can generate predictions", expect_equal(length(cv_glm_fit$predict()), task_2$nrow))

cv_glm_2 <- Lrnr_cv$new(glm_learner, folds = make_folds(cpp_imputed, V = 10))
cv_glm_fit_2 <- cv_glm_2$train(task_2)
cv_glm_fit_2$cv_risk(loss_squared_error)
test_that("Lrnr_cv can override folds from task", expect_equal(
  cv_glm_fit_2$params$folds,
  cv_glm_fit_2$fit_object$folds
))

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
task <- sl3_Task$new(trend_all, covariates = "data", outcome = "data", folds = folds)
fit <- sl$train(task)
fit$predict_fold(task, "validation")
cv_risk_table <- fit$cv_risk(loss_squared_error)

# GLM should be perfect here because outcome=covariate
expect_equal(cv_risk_table$coefficients[[1]], 1)
expect_equal(cv_risk_table$risk[[1]], 0)
