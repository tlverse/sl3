context("test-ts_weights.R -- Lrnr_ts_weights")

library(origami)
library(data.table)

lrnr_earth <- Lrnr_earth$new()
test_that("Lrnr_ts_weights errors when learner does not support weights",{
  expect_error(Lrnr_ts_weights$new(lrnr_earth))
})

metalrnr <- Lrnr_solnp$new(metalearner_linear, loss_squared_error)
test_that("Lrnr_ts_weights errors when rate/window not provided",{
  expect_error(Lrnr_ts_weights$new(metalrnr))
})

test_that("Lrnr_ts_weights errors when delay_decay provided without rate",{
  expect_error(
    Lrnr_ts_weights$new(metalrnr, window=1, delay_decay=5)
  )
})

window1_metalrnr <- Lrnr_ts_weights$new(metalrnr, window=1)
window5_metalrnr <- Lrnr_ts_weights$new(metalrnr, window=5)
decay_metalrnr <- Lrnr_ts_weights$new(metalrnr, rate=0.05)
delay_decay_metalrnr <- Lrnr_ts_weights$new(metalrnr, rate=0.05, delay_decay=5)

arima_aicc <- Lrnr_arima$new(ic="aic")    
arima_bic <- Lrnr_arima$new(ic="bic")
lrnr_glm <- Lrnr_glm$new()
lrnr_lasso <- Lrnr_glmnet$new()
stack <- make_learner(Stack, arima_aicc, arima_bic, lrnr_lasso, lrnr_glm)
window1_sl <- Lrnr_sl$new(stack, window1_metalrnr) 
window5_sl <- Lrnr_sl$new(stack, window5_metalrnr) 
decay_sl <- Lrnr_sl$new(stack, decay_metalrnr) 
delay_decay_sl <- Lrnr_sl$new(stack, delay_decay_metalrnr) 

data(bsds)
bsds <- bsds[1:50, ]

data <- as.data.table(bsds)
data[, time := .I]
outcome <- "cnt"
covars <- c("registered", "temp", "windspeed", "hum","weekday")
node_list <- list(outcome = outcome, time = "time", covariates = covars)

folds <- make_folds(
  data[1:35,], fold_fun = folds_rolling_window, window_size = 10, 
  validation_size = 5, gap = 0, batch = 5
)
training_task <- sl3_Task$new(data[1:35,], nodes = node_list, folds = folds)

folds <- make_folds(
  data[36:50,], fold_fun = folds_rolling_window, window_size = 10, 
  validation_size = 5, gap = 0, batch = 5
)
forecast_task <-  sl3_Task$new(data[36:50,], nodes = node_list, folds = folds)

test_that("Lrnr_ts_weights fits SL differently for different windows/rates",{
  
  window1_fit <- suppressWarnings(window1_sl$train(training_task))
  window1_preds <- suppressWarnings(window1_fit$predict(forecast_task))
  
  window5_fit <- suppressWarnings(window5_sl$train(training_task))
  window5_preds <- suppressWarnings(window5_fit$predict(forecast_task))
  
  decay_fit <- suppressWarnings(decay_sl$train(training_task))
  decay_preds <- suppressWarnings(decay_fit$predict(forecast_task))
  
  delay_decay_fit <- suppressWarnings(delay_decay_sl$train(training_task))
  delay_decay_preds <- suppressWarnings(delay_decay_fit$predict(forecast_task))
  
  expect_false(all(identical(window5_preds, window1_preds)))
  expect_false(all(identical(window1_preds, decay_preds)))
  expect_false(all(identical(decay_preds, delay_decay_preds)))
})

test_that("Lrnr_ts_weights naming works", {
  metalrnr <- Lrnr_solnp$new(metalearner_linear, loss_squared_error, name="sol")
  window1_metalrnr <- Lrnr_ts_weights$new(metalrnr, window=1)
  expect_equal(window1_metalrnr$name, "Lrnr_ts_weights_1_NULL_NULL_sol")
  
  window1_metalrnr <- Lrnr_ts_weights$new(metalrnr, window=1, name="ts")
  expect_equal(window1_metalrnr$name, "ts")
})
  
test_that("Lrnr_ts_weights errors when time is not in task", {
  nodes <- list(outcome = outcome, covariates = covars)
  folds <- make_folds(
    data, fold_fun = folds_rolling_window, window_size = 10, 
    validation_size = 5, gap = 0, batch = 5
  )
  task_no_time <- sl3_Task$new(data, nodes=nodes, folds=folds)
  expect_error(suppressWarnings(window1_sl$train(task_no_time)))
})
