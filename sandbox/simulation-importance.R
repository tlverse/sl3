library(data.table)
library(sl3)
library(foreach)
library(doParallel)
library(tidyverse)

options("scipen" = 1, "digits" = 3)

# ==============================================================================
calc_risk <- function(lrnr, loss, data, covariates, outcome, seed = 4591){
  
  ################################## baseline risk #############################
  
  # risk w all X under both fold_number options in sl3 varimp
  task <- make_sl3_Task(data = data, covariates = covariates, outcome = outcome)
  if(any(grepl("Lrnr_cv", class(lrnr)))){ 
    lrnr <- lrnr
  } else {
    lrnr <- Lrnr_cv$new(lrnr, full_fit = TRUE)
  }
  fit <- lrnr$train(task)
  full_pred <- fit$predict_fold(task, "full")
  cv_pred <- fit$predict_fold(task, "validation")
  risk_cv <- mean(loss(cv_pred, task$Y))
  risk_full <- mean(loss(full_pred, task$Y))
  
  ########################## remove + refit approach ###########################
  set.seed(seed)
  risk_remX <- lapply(seq_along(covariates), function(i){
    task <- make_sl3_Task(data = data, covariates = covariates[-i], 
                          outcome = outcome)
    fit <- lrnr$train(task)
    cv_pred <- fit$predict_fold(training_task, "validation")
    full_pred <- fit$predict_fold(training_task, "full")
    risk_cv <- mean(loss(cv_pred, training_task$Y))
    risk_full <- mean(loss(full_pred, training_task$Y))
    list(risk_cv = risk_cv, risk_full = risk_full)
  })
  
  risk_remX_cv <- lapply(risk_remX, '[[', 'risk_cv')
  risk_ratio_remX_cv <- as.numeric(t(lapply(risk_remX_cv, function(x) x/risk_cv)))
  risk_diff_remX_cv <- as.numeric(t(lapply(risk_remX_cv, function(x) x-risk_cv)))
  
  risk_remX_full <- lapply(risk_remX, '[[', 'risk_full')
  risk_ratio_remX_full <- as.numeric(t(lapply(risk_remX_full, function(x) x/risk_full)))
  risk_diff_remX_full <- as.numeric(t(lapply(risk_remX_full, function(x) x-risk_full)))
  
  ################################ permutation approach ########################
  set.seed(seed)
  risk_permX <- lapply(covariates, function(i){
    scrambled_col <- data.table(sample(unlist(data[, i, with=F]), nrow(data)))
    names(scrambled_col) <- i
    scrambled_col_names <- task$add_columns(scrambled_col)
    scrambled_task <- task$next_in_chain(column_names = scrambled_col_names)
    pred_cv <- fit$predict_fold(scrambled_task, "validation")
    pred_full <- fit$predict_fold(scrambled_task, "full")
    risk_cv <- mean(loss(pred_cv, scrambled_task$Y))
    risk_full <- mean(loss(pred_full, scrambled_task$Y))
    list(risk_cv = risk_cv, risk_full = risk_full)
  })
  
  risk_permX_cv <- lapply(risk_permX, '[[', 'risk_cv')
  risk_ratio_permX_cv <- as.numeric(t(lapply(risk_permX_cv, function(x) x/risk_cv)))
  risk_diff_permX_cv <- as.numeric(t(lapply(risk_permX_cv, function(x) x-risk_cv)))
  
  risk_permX_full <- lapply(risk_permX, '[[', 'risk_full')
  risk_ratio_permX_full <- as.numeric(t(lapply(risk_permX_full, function(x) x/risk_full)))
  risk_diff_permX_full <- as.numeric(t(lapply(risk_permX_full, function(x) x-risk_full)))
  
  result <- data.table(rbind(risk_ratio_remX_cv, risk_ratio_permX_cv, 
                             risk_ratio_remX_full, risk_ratio_permX_full, 
                             risk_diff_remX_cv, risk_diff_permX_cv, 
                             risk_diff_remX_full, risk_diff_permX_full))
  colnames(result) <- covariates
  data.table(
    importance_metric = c(rep("risk_ratio", 4), rep("risk_difference", 4)), 
    method = c("remove+refit","permute","remove+refit","permute",
               "remove+refit","permute","remove+refit","permute"),
    evaluation = c("validation","validation","full","full",
                   "validation","validation","full","full"),
    result
  )
}
# ==============================================================================
# ==============================================================================
calc_performance <- function(risk_dt, covariates, true_risk_ratio, 
                             true_risk_difference){
  
  res_ratio <- risk_dt[importance_metric == "risk_ratio", covariates, with=F]
  res_diff <- risk_dt[importance_metric == "risk_difference", covariates, with=F]
  mse_ratio <- (res_ratio-true_risk_ratio)^2
  mse_diff <- (res_diff-true_risk_difference)^2
  
  dt_ratio <- risk_dt[importance_metric == "risk_ratio", -covariates, with=F]
  dt_diff <- risk_dt[importance_metric == "risk_difference", -covariates, with=F]
  rbind(data.table(dt_ratio, mse_ratio), data.table(dt_diff, mse_diff))
}
# ==============================================================================
# ==============================================================================
gen_data <- function(n, binary_outcome = FALSE, linear_response = FALSE){
  
  X1 <- runif(n, 0, 1) 
  X2 <- rnorm(n) 
  X3 <- rbinom(n, 1, 0.5)
  X4 <- rnorm(n, 0, .5) 
  X5 <- rbinom(n, 1, 0.25)
  
  if(linear_response){
    Y <- X1 + X2 + X3 + rnorm(n)
  } else {
    Y <- log(X1 + 1) + X2*X3 + rnorm(n)
  }
  
  if(binary_outcome){
    Y <- rbinom(n, 1, plogis(Y))
  }
  data.table(X1, X2, X3, X4, X5, Y)
}
# ==============================================================================

# call test truth and eval MSE over 1,000 simulations across various n training

# predictions & risk on test data, what we're calling the truth
set.seed(317)
big_training_data <- gen_data(1e6, linear_response = T)
big_test_data <- gen_data(1e6, linear_response = T)
covariates <- paste0("X", 1:5)
outcome <- "Y"
loss <- loss_squared_error
training_task <- make_sl3_Task(data = big_training_data, 
                               covariates = covariates, 
                               outcome = outcome)
lrnr <- Lrnr_glm$new()
fit <- lrnr$train(training_task)

test_task <- make_sl3_Task(data = big_test_data, covariates = covariates, 
                           outcome = outcome)
pred <- fit$predict(test_task)
true_risk <- mean(loss(pred, test_task$Y))
true_risk_remX <- lapply(seq_along(covariates), function(i){
  training_task <- make_sl3_Task(
    data = big_training_data, covariates = covariates[-i], outcome = outcome
  )
  test_task <- make_sl3_Task(
    data = big_test_data, covariates = covariates[-i], outcome = outcome
  )
  fit <- lrnr$train(training_task)
  pred <- fit$predict(test_task)
  mean(loss(pred, test_task$Y))
})
names(true_risk_remX) <- covariates
true_risk_ratio <- t(unlist(lapply(true_risk_remX, function(x) x/true_risk)))
true_risk_diff <- t(unlist(lapply(true_risk_remX, function(x) x-true_risk)))

# various sample sizes for testing
n_sequence <- c(50, 100, 500, 1000, 5000)
mse_covariate_summary <- list()
mse_summary <- list()
risk_summary <- list()

# each sample size considers B simulations 
B <- 100
RNGkind(sample.kind = "Rejection")
set.seed(4917)
bootstrap_seeds <- sample(1:2^15, B)

for(i in 1:length(n_sequence)){
  
  n <- n_sequence[i]
  cat("\n STARTING SIMULATIONS WITH n =", n, "\n")
  
  # registerDoParallel(cores = (detectCores()-1))
  registerDoParallel(cores = 1)
  getDoParWorkers()
  Bres <- foreach(b = 1:B) %dopar% {
    print(paste0("Starting simulation ", b))
    set.seed(bootstrap_seeds[b])
    d <- gen_data(n, linear_response = T)
    risk_tbl <- calc_risk(lrnr, loss, d, covariates, outcome, bootstrap_seeds[b])
    mse_tbl <- calc_performance(risk_tbl, covariates, true_risk_ratio, true_risk_diff)
    list(risk_tbl = risk_tbl, mse_tbl = mse_tbl)
  }
  
  ############################# summarize MSE ##################################
  mse_all <- do.call(rbind, lapply(Bres, '[[', 'mse_tbl'))
  mse_cov_summary <- data.table(
    mse_all %>% 
      dplyr::group_by(importance_metric, method, evaluation) %>%
      dplyr::summarize_at(c("X1","X2","X3","X4","X5"), mean)
  )
  sumMSE <- rowSums(mse_cov_summary[, covariates, with=F])
  mse_summary[[i]] <- data.table(n = rep(n, nrow(mse_cov_summary)), 
                                 mse_cov_summary[, -covariates, with=F], 
                                 sumMSE)
  
  colnames(mse_cov_summary) <- paste0(colnames(mse_cov_summary), "_MSE")
  mse_covariate_summary[[i]] <- data.table(n = rep(n, nrow(mse_cov_summary)), 
                                           mse_cov_summary)
  
  ############################# summarize risk #################################
  risk_all <- do.call(rbind, lapply(Bres, '[[', 'risk_tbl'))
  
  risk_covariate_summary <- data.table(
    risk_all %>% 
      dplyr::group_by(importance_metric, method, evaluation) %>%
      dplyr::summarize_at(c("X1","X2","X3","X4","X5"), mean)
  )
  mean_risk <- risk_covariate_summary[,covariates,with=F]
  colnames(mean_risk) <- paste0("mean_risk_", colnames(mean_risk))
  
  risk_covariate_summary <- data.table(
    risk_all %>% 
      dplyr::group_by(importance_metric, method, evaluation) %>%
      dplyr::summarize_at(c("X1","X2","X3","X4","X5"), median)
  )
  median_risk <- risk_covariate_summary[,covariates,with=F]
  colnames(median_risk) <- paste0("median_risk_", colnames(median_risk))
  
  risk_covariate_summary <- data.table(
    risk_all %>% 
      dplyr::group_by(importance_metric, method, evaluation) %>%
      dplyr::summarize_at(c("X1","X2","X3","X4","X5"), var)
  )
  var_risk <- risk_covariate_summary[,covariates,with=F]
  colnames(var_risk) <- paste0("var_risk_", colnames(var_risk))
  
  risk_summary[[i]] <- data.table(n = rep(n, nrow(var_risk)),
                                  risk_covariate_summary[,-covariates,with=F], 
                                  mean_risk, median_risk, var_risk)
}

mse_covariate_summary <- do.call(rbind, mse_covariate_summary)
write.csv(mse_covariate_summary, )
mse_summary <- do.call(rbind, mse_summary)
risk_summary <- do.call(rbind, risk_summary)

