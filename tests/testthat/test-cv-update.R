library(sl3)
library(delayed)
library(origami)
library(devtools)
library(assertthat)
library(digest)

data(cpp_imputed)
covars <-
  c(
    "apgar1",
    "apgar5",
    "parity",
    "gagebrth",
    "mage",
    "meducyrs",
    "sexn"
  )
outcome <- "haz"

data <- cpp_imputed[1:100, ]
folds <- origami::make_folds(
  data,
  fold_fun = folds_rolling_window,
  window_size = 25,
  validation_size = 25,
  gap = 0,
  batch = 25
)
task <- make_sl3_Task(
  data = data,
  covariates = covars,
  outcome = outcome,
  folds = folds
)

data_more <- cpp_imputed[1:200, ]
folds_more <- origami::make_folds(
  data_more,
  fold_fun = folds_rolling_window,
  window_size = 25,
  validation_size = 25,
  gap = 0,
  batch = 25
)
folds_more <- tail(folds_more, 5)

task_moredata <- make_sl3_Task(
  data = data_more,
  covariates = covars,
  outcome = outcome,
  folds = folds_more
)

lrnr_glm <- make_learner(Lrnr_glm)
lrnr_mean <- make_learner(Lrnr_mean)
lrnr_xgboost <- make_learner(Lrnr_xgboost, verbose = 0)

stack <- make_learner(
  Stack,
  lrnr_glm, lrnr_mean, lrnr_xgboost
)

cv_stack <- Lrnr_cv$new(stack)
cv_fit <- cv_stack$train(task)

updated_cv_fit <- cv_fit$update(task_moredata, drop_old = TRUE)
cv_refit <- cv_stack$train(task_moredata)

# debug_predict(updated_cv_fit)
preds_updated <- updated_cv_fit$predict()
preds_updated_subset <- preds_updated[1:25, ]
preds_old <- cv_fit$predict()
preds_old_subset <- preds_old[51:75, ]

preds_refit <- cv_refit$predict()

test_that(
  "Lrnr_cv update matches refit of Lrnr_cv",
  expect_equal(preds_updated, preds_refit)
)

test_that(
  "Update matches old fit as appropriate",
  expect_equal(preds_updated_subset, preds_old_subset)
)
