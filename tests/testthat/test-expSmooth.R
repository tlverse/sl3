context("test-expSmooth.R -- Lrnr_expSmooth")

library(origami)

set.seed(1)
data(bsds)
covars <- c("temp", "windspeed")
outcome <- "cnt"

folds <- origami::make_folds(bsds,
  fold_fun = folds_rolling_window, window_size = 50,
  validation_size = 10, gap = 0, batch = 200
)
task <- sl3_Task$new(bsds, covariates = covars, outcome = outcome, folds = folds)
train_task <- training(task, fold = task$folds[[1]])
valid_task <- validation(task, fold = task$folds[[1]])

test_that("Automatic Lrnr_expSmooth gives expected values", {
  expSmooth_learner <- Lrnr_expSmooth$new()
  expSmooth_fit <- expSmooth_learner$train(train_task)
  expSmooth_preds <- expSmooth_fit$predict(valid_task)

  expSmooth_fit_2 <- forecast::ets(train_task$Y)
  expSmooth_preds_2 <- forecast::forecast(expSmooth_fit_2)
  expSmooth_preds_2 <- as.numeric(expSmooth_preds_2$mean)
  expSmooth_preds_2 <- structure(expSmooth_preds_2)

  # predictions should be exactly the same
  expect_true(sum(expSmooth_preds_2[1] - expSmooth_preds[1]) < 10^-1)
})
