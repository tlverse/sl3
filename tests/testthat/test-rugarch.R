library(origami)
library(data.table)
library(rugarch)

test_that("Lrnr_rugarch predictions are the same as classic implementation", {
  data(bsds)

  # make folds appropriate for time-series cross-validation
  folds <- make_folds(bsds,
    fold_fun = folds_rolling_window, window_size = 500,
    validation_size = 100, gap = 0, batch = 50
  )

  # build task by passing in external folds structure
  task <- sl3_Task$new(
    data = bsds,
    folds = folds,
    covariates = c(
      "weekday", "temp"
    ),
    outcome = "cnt"
  )

  # create tasks for training and validation
  train_task <- training(task, fold = task$folds[[1]])
  valid_task <- validation(task, fold = task$folds[[1]])

  lrnr_rugarch <- Lrnr_rugarch$new()

  # sl3 implementation
  fit_sl3 <- lrnr_rugarch$train(train_task)
  pred_sl3 <- fit_sl3$predict(valid_task)

  # classical implementation
  fit_classic <- rugarch::ugarchfit(ugarchspec(), train_task$X)
  pred_classic <- ugarchforecast(fit_classic,
    data = valid_task$X,
    n.ahead = ts_get_pred_horizon(
      train_task,
      valid_task
    )
  )
  pred_classic <- as.numeric(pred_classic@forecast$seriesFor)
  expect_equal(pred_sl3, pred_classic)
})
