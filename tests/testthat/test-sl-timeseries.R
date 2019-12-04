context("test-sl-timeseries.R -- Super Learner for time-series")

library(sl3)
library(delayed)
library(origami)
library(devtools)
library(assertthat)
library(digest)

data(cpp_imputed)
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
outcome <- "haz"
data <- cpp_imputed[1:100, ]

folds <- origami::make_folds(data,
  fold_fun = folds_rolling_window, window_size = 25,
  validation_size = 25, gap = 0, batch = 10
)
task <- make_sl3_Task(
  data = data,
  covariates = covars,
  outcome = outcome, folds = folds
)

lrnr_glm <- make_learner(Lrnr_glm)
lrnr_mean <- make_learner(Lrnr_mean)
lrnr_glmnet <- make_learner(Lrnr_glmnet)

stack <- make_learner(
  Stack,
  lrnr_glm, lrnr_mean, lrnr_glmnet
)
metalearner <- make_learner(Lrnr_nnls)

sl <- make_learner(Lrnr_sl,
  learners = stack,
  metalearner = metalearner
)
sl_fit <- sl$train(task)
preds <- sl_fit$predict_fold(task, "validation")

test_that("validation set for time-series is as expected", expect_equal(length(preds), 150))
