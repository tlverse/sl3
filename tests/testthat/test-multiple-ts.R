context("test_multiple_ts.R -- Lrnr_multiple_ts")

library(data.table)
library(origami)
library(sl3)
library(R6)
library(delayed)
library(dplyr)

set.seed(49753)
if(packageVersion("origami")>"1.0.3"){
  # Simulate simple AR(2) process
  data <- matrix(arima.sim(model = list(ar = c(.9, -.2)), n = 200))
  id <- c(rep("Series_1", 50), rep("Series_2", 50), rep("Series_3", 50), rep("Series_4", 50))
  
  data <- data.frame(data)
  data$id <- as.factor(id)
  
  data <- data %>%
    group_by(id) %>%
    dplyr::mutate(time = 1:n())
  
  data$W1 <- rbinom(200, 1, 0.6)
  data$W2 <- rbinom(200, 1, 0.2)
  
  data <- as.data.table(data)
  
  folds <- origami::make_folds(data,
    t = max(data$time),
    id = data$id,
    time = data$time,
    fold_fun = folds_rolling_window_pooled,
    window_size = 20,
    validation_size = 15,
    gap = 0,
    batch = 10
  )
  
  
  task <- sl3_Task$new(
    data = data, outcome = "data",
    time = "time", id = "id",
    covariates = c("W1", "W2"),
    folds = folds
  )
  
  train_task <- training(task, fold = task$folds[[1]])
  valid_task <- validation(task, fold = task$folds[[1]])
  
  
  # Learners
  lrnr_lasso <- make_learner(Lrnr_glmnet, alpha = 1)
  lrnr_mean <- make_learner(Lrnr_mean)
  lrnr_arima <- Lrnr_arima$new()
  lrnr_exp_smooth <- Lrnr_expSmooth$new()
  
  multiple_ts_arima <- Lrnr_multiple_ts$new(
    learner = lrnr_arima
  )
  
  
  stack <- make_learner(Stack, unlist(list(lrnr_mean,multiple_ts_arima),
                                      recursive = TRUE))
  
  
  test_that("Lrnr_multiple_ts fits each time-series separately", {
    multiple_ts_arima_fit <- multiple_ts_arima$train(train_task)
    multiple_ts_arima_preds <- multiple_ts_arima_fit$predict(valid_task)
  
    sub_data_s1 <- multiple_ts_arima_fit$fit_object$Series_1$training_task$data
    sub_data_f1 <- multiple_ts_arima_fit$fit_object$Series_1$training_task$folds
  
    expect_true(length(multiple_ts_arima_preds) == 60)
    expect_true(length(multiple_ts_arima_fit$fit_object) == 4)
    expect_true(nrow(sub_data_s1) == 20)
    #
    # expect_true(length(sub_data_f1[[1]]$validation_set) == 15)
    # expect_true(length(sub_data_f1[[2]]$validation_set) == 15)
  })
  
  test_that("Lrnr_multiple_ts fits multiple learners separately", {
    fit_stack <- stack$train(train_task)
    pred_stack <- fit_stack$predict(valid_task)
  
    expect_true(ncol(pred_stack) == 2)
    expect_true(nrow(pred_stack) == 60)
  })
}