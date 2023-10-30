context("test-ts_weights.R -- Lrnr_ts_weights")

library(origami)
library(data.table)

lrnr_earth <- Lrnr_earth$new()
test_that("Lrnr_ts_weights errors when learner does not support weights", {
  expect_error(Lrnr_ts_weights$new(lrnr_earth))
})

metalrnr <- Lrnr_solnp$new(metalearner_linear, loss_squared_error)
test_that("Lrnr_ts_weights errors when rate/window not provided", {
  expect_error(Lrnr_ts_weights$new(metalrnr))
})

test_that("Lrnr_ts_weights errors when delay_decay provided without rate", {
  expect_error(
    Lrnr_ts_weights$new(metalrnr, window = 1, delay_decay = 5)
  )
})

window1_metalrnr <- Lrnr_ts_weights$new(metalrnr, window = 1)
window40_metalrnr <- Lrnr_ts_weights$new(metalrnr, window = 40)
decay_metalrnr <- Lrnr_ts_weights$new(metalrnr, rate = 0.05)
delay_decay_metalrnr <- Lrnr_ts_weights$new(metalrnr, rate = 0.05, delay_decay = 40)

lrnr_glm <- Lrnr_glm$new()
lrnr_mean <- Lrnr_mean$new()
lrnr_earth <- Lrnr_earth$new()
stack <- make_learner(Stack, lrnr_glm, lrnr_mean, lrnr_earth)
window1_sl <- Lrnr_sl$new(stack, window1_metalrnr)
window40_sl <- Lrnr_sl$new(stack, window40_metalrnr)
decay_sl <- Lrnr_sl$new(stack, decay_metalrnr)
delay_decay_sl <- Lrnr_sl$new(stack, delay_decay_metalrnr)

data(bsds)
bsds <- bsds[1:500, ]

data <- as.data.table(bsds)
data[, time := .I]
outcome <- "cnt"
covars <- c("registered", "temp", "windspeed", "hum", "weekday")
node_list <- list(outcome = outcome, time = "time", covariates = covars)

folds <- make_folds(
  data[1:400, ],
  fold_fun = folds_rolling_window, window_size = 50,
  validation_size = 50, gap = 0, batch = 50
)
training_task <- sl3_Task$new(data[1:400, ], nodes = node_list, folds = folds)

folds <- make_folds(
  data[401:500, ],
  fold_fun = folds_rolling_window, window_size = 50,
  validation_size = 50, gap = 0, batch = 50
)
forecast_task <- sl3_Task$new(data[401:500, ], nodes = node_list, folds = folds)

test_that("Lrnr_ts_weights fits SL differently for different windows/rates", {
  window1_fit <- window1_sl$train(training_task)
  window1_preds <- window1_fit$predict(forecast_task)

  window40_fit <- window40_sl$train(training_task)
  window40_preds <- window40_fit$predict(forecast_task)

  decay_fit <- decay_sl$train(training_task)
  decay_preds <- decay_fit$predict(forecast_task)

  delay_decay_fit <- delay_decay_sl$train(training_task)
  delay_decay_preds <- delay_decay_fit$predict(forecast_task)

  expect_false(identical(window40_preds, window1_preds))
  expect_false(identical(window1_preds, decay_preds))
  expect_false(identical(decay_preds, delay_decay_preds))
})

test_that("Lrnr_ts_weights naming works", {
  metalrnr <- Lrnr_solnp$new(metalearner_linear, loss_squared_error, name = "sol")
  window1_metalrnr <- Lrnr_ts_weights$new(metalrnr, window = 1)
  match <- grepl("^Lrnr_ts_weights.*\\_sol$", window1_metalrnr$name)
  expect_true(match)

  window1_metalrnr <- Lrnr_ts_weights$new(metalrnr, window = 1, name = "ts")
  expect_equal(window1_metalrnr$name, "ts")
})

test_that("Lrnr_ts_weights errors when time is not in task", {
  nodes <- list(outcome = outcome, covariates = covars)
  folds <- make_folds(
    data,
    fold_fun = folds_rolling_window, window_size = 10,
    validation_size = 5, gap = 0, batch = 5
  )
  task_no_time <- sl3_Task$new(data, nodes = nodes, folds = folds)
  expect_error(window1_sl$train(task_no_time))
})
