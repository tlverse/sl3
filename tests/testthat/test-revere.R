library(sl3)
library(data.table)
context("test-revere.R -- Split Specific SuperLearner")

# generate simulated data
n <- 1000
p <- 2
EY <- function(X) {
  X[, 2]
}

VY <- function(X) {
  (X[, 1])
}

gen_data <- function(n = 1000, p = 2) {
  X <- matrix(runif(n * p, 0, 2 * pi), nrow = n)
  colnames(X) <- paste("X", seq_len(p), sep = "")
  EYn <- EY(X)
  VYn <- VY(X)
  Y <- rnorm(n, mean = EYn, sd = sqrt(VYn))
  data <- data.table(X, Y, EY = EYn, VY = VYn)
  return(data)
}

data <- gen_data(n, p)

# fit SL to data
covariates <- grep("X", names(data), value = T)
task <- make_sl3_Task(data, covariates = covariates, outcome = "Y")

lib <- make_learner_stack(
  "Lrnr_mean",
  "Lrnr_glm_fast",
  "Lrnr_xgboost"
)


linear_metalearner <- make_learner(Lrnr_solnp, metalearner_linear, loss_squared_error)
lrnr_sl <- make_learner(Lrnr_sl, lib, linear_metalearner)
fit <- lrnr_sl$train(task)

# make the revere task
fold_number <- "validation"
revere_generator <- function(fit) {
  fun <- function(fold_number, task) {
    preds <- fit$predict_fold(task, fold_number)
    squared_error <- data.table(squared_error = (preds - task$Y)^2)

    new_columns <- task$add_columns(squared_error)
    revere_task <- task$next_in_chain(outcome = "squared_error", column_names = new_columns)

    return(revere_task)
  }

  return(fun)
}

revere_task_fun <- revere_generator(fit)

revere_task <- sl3_revere_Task$new(generator_fun = revere_task_fun, task = task)

# debugonce(drop_offsets_chain)
# debug_train(lrnr_sl,once=TRUE)
# sl3_debug_mode()
# debug_train(lib$params$learners[[1]], once=FALSE)
revere_v_fit <- lrnr_sl$train(revere_task)


revere_v_fit_preds <- revere_v_fit$predict_fold(revere_task, "validation")
revere_v_mse <- mean((data$VY - revere_v_fit_preds)^2)
revere_v_bias <- mean(data$VY - revere_v_fit_preds)
