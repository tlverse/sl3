context("test_multiple_ts.R -- Lrnr_multiple_ts")

library(data.table)
library(origami)
library(sl3)
library(R6)
library(delayed)

set.seed(49753)

#Simulate simple AR(2) process
data <- matrix(arima.sim(model=list(ar=c(.9,-.2)),n=200), nrow = 50, ncol = 4)
data <- as.data.table(data)
setnames(data, paste("Series", 1:ncol(data), sep = "_"))
data[, time := .I]

#Generate folds for single time-series
folds <- origami::make_folds(data,
                             fold_fun = folds_rolling_window, window_size = 20,
                             validation_size = 15, gap = 0, batch = 10
)

#Generate the task
node_list <- list(covariates= names(data)[1:4], time = "time")
task <- sl3_Task$new(data = data, nodes = node_list, folds = folds)

#Learners
arima_lrnr <- Lrnr_arima$new()
exp_smooth_lrnr <- Lrnr_expSmooth$new()
learners <- list(arima_lrnr, exp_smooth_lrnr)
sl <- make_learner(Lrnr_sl, learners)

test_that("Lrnr_multiple_ts fits each time-series separately", {
  multiple_ts_arima <- Lrnr_multiple_ts$new(
    learner = arima_lrnr
  )
  multiple_ts_arima_fit <- multiple_ts_arima$train(task)
  multiple_ts_arima_preds <- multiple_ts_arima_fit$predict()
  
  sub_data_s1<-multiple_ts_arima_fit$fit_object$Series_1$training_task$data
  sub_data_f1<-multiple_ts_arima_fit$fit_object$Series_1$training_task$folds
  
  expect_true(length(multiple_ts_arima_preds)==200)
  expect_true(length(multiple_ts_arima_fit$fit_object)==4)
  expect_true(nrow(sub_data_s1)==50)
  expect_true(length(sub_data_f1[[1]]$validation_set)==15)
  expect_true(length(sub_data_f1[[2]]$validation_set)==15)
})
