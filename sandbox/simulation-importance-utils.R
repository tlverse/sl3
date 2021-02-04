library(data.table)
library(foreach)
library(doParallel)
library(tidyverse)
library(ggplot2)
require(lattice)
library(sl3)

options("scipen" = 1, "digits" = 3)

# ==============================================================================
# main function that calls all other functions in this script
# ==============================================================================

# gen_data() is a function that generates a table

# if covariates is NULL, all non-outcome columns are considered covariates

# irrelevant_covariates is a character vector of irrelevant covariates

# relevant_covariates_rank is a named list of length number of relevant 
# covariates, with names being the relevant covariates. Each element contains
# a valid rank for the covariate, ex/ list("X1"=1, "X2"=c(2,3), "X3"=c(2,3)).

run_simulation_sequence <- function(bootstrap_seeds, gen_data, lrnr, save_path,
                                    irrelevant_covariates, relevant_covariates_rank,
                                    N = 1e6, n_sequence = c(50, 100, 500, 1000, 5000),
                                    loss = loss_squared_error, outcome = "Y", 
                                    covariates = NULL, cores = 1, 
                                    calculateMSE = TRUE){
  
  t <- proc.time()
  
  stopifnot(dir.exists(save_path))
  
  if(is.null(covariates)){
    d <- gen_data(1)
    covariates <- colnames(d)[-which(colnames(d) == outcome)]
  }
  
  len_supplied <- length(irrelevant_covariates) + length(relevant_covariates_rank)
  stopifnot(length(covariates) == len_supplied)
  
  if(calculateMSE){
    # establish truth with very large training and test data
    cat("\n CALCULATING TRUE IMPORTANCE WRT BIG N =", N, "\n")
    training_dt <- gen_data(N)
    test_dt <- gen_data(N)
    truths <- calc_truth(big_training_data = training_dt, big_test_data = test_dt, 
                         covariates = covariates, outcome = outcome, loss = loss, 
                         lrnr = lrnr)
    mse_covariate_summary <- list()
    mse_summary <- list()                   
  }
  
  risk_summary <- list()
  rank_summary <- list()
  # each sample size considers B simulations 
  B <- length(bootstrap_seeds)
  for(i in 1:length(n_sequence)){
    
    n <- n_sequence[i]
    cat("\n STARTING SIMULATIONS WITH n =", n, "\n")
    
    registerDoParallel(cores = cores)
    getDoParWorkers()
    Bres <- foreach(b = 1:B) %dopar% {
      print(paste0("Starting simulation ", b))
      set.seed(bootstrap_seeds[b])
      d <- gen_data(n)
      risk_dt <- calc_importance(lrnr = lrnr, loss = loss, data = d, 
                                 covariates = covariates, outcome = outcome, 
                                 seed = bootstrap_seeds[b])
      rank_check_dt <- check_rank(risk_dt = risk_dt, 
                                  irrelevant_covariates = irrelevant_covariates,
                                  relevant_covariates_rank = relevant_covariates_rank)
      if(calculateMSE){
        mse_dt <- calc_mse(risk_dt = risk_dt, 
                           true_risk_ratio = truths[["risk_ratio"]], 
                           true_risk_difference = truths[["risk_difference"]])
      } else {
        mse_dt <- NULL
      }            
      return(list(risk_dt = risk_dt, mse_dt = mse_dt, rank_check_dt = rank_check_dt))
    }
    n_res <- compile_simulation_results(Bres, n, covariates, calculateMSE)
    risk_summary[[i]] <- n_res[["risk_summary"]]
    rank_summary[[i]] <- n_res[["rank_summary"]]
    if(calculateMSE){
      mse_covariate_summary[[i]] <- n_res[["mse_covariate_summary"]]
      mse_summary[[i]] <- n_res[["mse_summary"]]
    }
  }
  
  # save & plot results 
  risk_summ <- do.call(rbind, risk_summary)
  write.csv(risk_summ, row.names = F, file = paste0(save_path, "risk_summary.csv"))
  rank_summ <- do.call(rbind, rank_summary)
  write.csv(rank_summ, row.names = F, file = paste0(save_path, "rank_summary.csv"))
  if(calculateMSE){
    mse_summ <- do.call(rbind, mse_summary)
    write.csv(mse_summ, row.names = F, file = paste0(save_path, "mse_summary.csv"))
    mse_cov_summ <- do.call(rbind, mse_covariate_summary)
    write.csv(mse_cov_summ, row.names = F, file = paste0(save_path, "mse_covariate_summary.csv"))
    plot_results(save_path, risk_summ, rank_summ, mse_cov_summ, mse_summ)
  } else {
    plot_results(save_path, risk_summ, rank_summ)
  }

  timer <- proc.time() - t
  cat("\n DONE! \n\n timer: \n")
  print(timer)
}

# ==============================================================================
# get "true" importance using large training data & large independent test data
# ==============================================================================
calc_truth <- function(big_training_data, big_test_data, covariates, outcome,
                       loss, lrnr, seed = 317){
  # predictions & risk on test data, what we're calling the truth
  set.seed(seed)
  training_task <- make_sl3_Task(data = big_training_data, 
                                 covariates = covariates, 
                                 outcome = outcome)
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
  return(list(risk_ratio = true_risk_ratio, risk_difference = true_risk_diff))
}
# ==============================================================================
# function to calculate the variable importance
# ==============================================================================
calc_importance <- function(lrnr, loss, data, covariates, outcome, seed = 4591){
  
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
    new_task <- make_sl3_Task(data = data, covariates = covariates[-i], 
                              outcome = outcome)
    refit <- lrnr$train(new_task)
    cv_pred <- refit$predict_fold(new_task, "validation")
    full_pred <- refit$predict_fold(new_task, "full")
    risk_cv <- mean(loss(cv_pred, new_task$Y))
    risk_full <- mean(loss(full_pred, new_task$Y))
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
# functions to calculate performance metrics
# ==============================================================================
calc_mse <- function(risk_dt, true_risk_ratio, true_risk_difference){
  not_covs <- c("importance_metric", "method", "evaluation")
  covs <- colnames(risk_dt[, -not_covs, with=F])
  
  res_ratio <- risk_dt[importance_metric == "risk_ratio", covs, with=F]
  res_diff <- risk_dt[importance_metric == "risk_difference", covs, with=F]
  mse_ratio <- (res_ratio-true_risk_ratio)^2
  mse_diff <- (res_diff-true_risk_difference)^2
  
  dt_ratio <- risk_dt[importance_metric == "risk_ratio", -covs, with=F]
  dt_diff <- risk_dt[importance_metric == "risk_difference", -covs, with=F]
  rbind(data.table(dt_ratio, mse_ratio), data.table(dt_diff, mse_diff))
}

check_rank <- function(risk_dt, irrelevant_covariates, relevant_covariates_rank){
  
  not_covs <- c("importance_metric", "method", "evaluation")
  cov_risk_dt <- risk_dt[, -not_covs, with=F]
  
  # irrelevant covariates are ranked last
  max_irrel_risk <- apply(cov_risk_dt[, irrelevant_covariates, with=F], 1, max)
  min_rel_risk <- apply(cov_risk_dt[, -irrelevant_covariates, with=F], 1, min) 
  irrel_covs_last <- ifelse(min_rel_risk > max_irrel_risk, 1, 0)
  
  # relevant covariates are ranked appropriately
  rank_dt <- data.table(apply(cov_risk_dt, 2, rank))
  colnames(rank_dt) <- colnames(cov_risk_dt)
  mat <- matrix(nrow = nrow(rank_dt), ncol = length(relevant_covariates_rank))
  for(i in 1:length(relevant_covariates_rank)){
    cov <- names(relevant_covariates_rank)[i]
    true_rank <- relevant_covariates_rank[[i]]
    cov_rank <- rank_dt[[cov]]
    mat[,i] <- ifelse(cov_rank %in% true_rank, 1, 0)
  }
  rank_correct_dt <- cbind(data.table(mat), irrel_covs_last)
  data.table(
    risk_dt[, not_covs, with=F], 
    prop_rank_correct = rowSums(rank_correct_dt)/ncol(rank_correct_dt),
    rank_correct = ifelse(rowSums(rank_correct_dt) == ncol(rank_correct_dt), 1, 0)
  )
}

# ==============================================================================
# functions to compile & plot the results across the n-specific simulations
# ==============================================================================
compile_simulation_results <- function(simulation_results, n, covariates, 
                                       hasMSE){
  
  if(hasMSE){
    mse_all <- do.call(rbind, lapply(simulation_results, '[[', 'mse_dt'))
    ############################# summarize MSE ##################################
    mse_cov_summary <- data.table(
      mse_all %>% 
        dplyr::group_by(importance_metric, method, evaluation) %>%
        dplyr::summarize_at(covariates, mean)
    )
    mse_covariate_summary <- data.table(n = rep(n, nrow(mse_cov_summary)), 
                                        mse_cov_summary)
                                        
    sumMSE <- rowSums(mse_cov_summary[, covariates, with=F])
    mse_summary <- data.table(n = rep(n, nrow(mse_cov_summary)), 
                              mse_cov_summary[, -covariates, with=F], sumMSE)
  }
  
  ############################# summarize rank #################################
  rank_all <- do.call(rbind, lapply(simulation_results, '[[', 'rank_check_dt'))
  rank_summary <- data.table(
    rank_all %>% 
      dplyr::group_by(importance_metric, method, evaluation) %>%
      dplyr::summarize_at(c("prop_rank_correct", "rank_correct"), mean)
  )
  rank_summary <- data.table(n = rep(n, nrow(rank_summary)), rank_summary)
  
  ############################# summarize risk #################################
  risk_all <- do.call(rbind, lapply(simulation_results, '[[', 'risk_dt'))
  risk_covariate_summary <- data.table(
    risk_all %>% 
      dplyr::group_by(importance_metric, method, evaluation) %>%
      dplyr::summarize_at(covariates, mean)
  )
  mean_risk <- risk_covariate_summary[, covariates, with=F]
  colnames(mean_risk) <- paste0("mean_risk_", colnames(mean_risk))
  
  risk_covariate_summary <- data.table(
    risk_all %>% 
      dplyr::group_by(importance_metric, method, evaluation) %>%
      dplyr::summarize_at(covariates, median)
  )
  median_risk <- risk_covariate_summary[, covariates, with=F]
  colnames(median_risk) <- paste0("median_risk_", colnames(median_risk))
  
  risk_summary <- data.table(n = rep(n, nrow(mean_risk)),
                             risk_covariate_summary[, -covariates, with=F], 
                             mean_risk, median_risk)
  if(hasMSE){
    return(list(rank_summary = rank_summary, mse_summary = mse_summary, 
                mse_covariate_summary = mse_covariate_summary,
                risk_summary = risk_summary))
  } else {
    return(list(rank_summary = rank_summary, risk_summary = risk_summary))
  }
}

# mse_covariate_summary, mse_summary, and risk_summary
# are the data tables generated by compile_simulation_results function
plot_results <- function(path, risk_summary, rank_summary, 
                         mse_covariate_summary = NULL, mse_summary = NULL){
  
  # folders to save plots of the importance metric itself, summarized over sims
  dir.create(paste0(path, "importance_plots"))
  dir.create(paste0(path, "importance_plots/risk_difference"))
  dir.create(paste0(path, "importance_plots/risk_ratio"))
  
  risk_summary$type <- paste(risk_summary$method, risk_summary$evaluation, sep = "_")
  not_covs <- c("n", "importance_metric", "method", "evaluation", "type")
  covs <- colnames(risk_summary)[-which(colnames(risk_summary) %in% not_covs)]
  ratio_tbl <- risk_summary[risk_summary$importance_metric == "risk_ratio", ]
  diff_tbl <- risk_summary[risk_summary$importance_metric == "risk_difference", ]
  split <- length(covs)/2
  
  for(i in 1:length(covs)){
    cov <- covs[i]
    
    if(i <= split){
      ylabs <- c("Mean Risk Difference", "Mean Risk Ratio")
    } else {
      ylabs <- c("Median Risk Difference", "Median Risk Ratio")
    }
    
    diff_tbl$y <- diff_tbl[,which(colnames(diff_tbl) == cov)]
    diff_plot <- ggplot(diff_tbl, aes(x = n, y = y, color = type)) +
      geom_point() + 
      geom_line(linetype = "dotted") + 
      theme(legend.position = "bottom", legend.title = element_blank()) +
      labs(x = "Sample Size", y = ylabs[1], 
           title = "sl3 Importance Evaluation", 
           subtitle = "MSEs of Risk Difference Metric over all Simulations")
    
    ratio_tbl$y <- ratio_tbl[,which(colnames(ratio_tbl) == cov)]
    ratio_plot <- ggplot(ratio_tbl, aes(x = n, y = y, color = type)) +
      geom_point() + 
      geom_line(linetype = "dotted") + 
      theme(legend.position = "bottom", legend.title = element_blank()) +
      labs(x = "Sample Size", y = ylabs[2], 
           title = "sl3 Importance Evaluation", 
           subtitle = "MSEs of Risk Ratio Metric over all Simulations")
    
    pdf(paste0(path, "importance_plots/risk_difference/", cov, ".pdf"))
    print(diff_plot)
    dev.off()
    
    pdf(paste0(path, "importance_plots/risk_ratio/", cov, ".pdf"))
    print(ratio_plot)
    dev.off()
  }
  
  # main rank summary plot
  rank_summary$type <- paste0(rank_summary$importance_metric, "_", 
                              rank_summary$method, "_", rank_summary$evaluation)
  plot <- ggplot(rank_summary, aes(x = n, y = prop_rank_correct, color = type)) +
    geom_point() + 
    geom_line(linetype = "dotted") + 
    theme(legend.title = element_blank()) +
    labs(x = "Sample Size", y = "Correct Rank Proportion", 
         title = "sl3 Importance Evaluation", 
         subtitle = "Correct Rank Proportion over all Simulations")
  pdf(paste0(path, "rank_proportion.pdf"))
  print(plot)
  dev.off()
  
  plot <- ggplot(rank_summary, aes(x = n, y = rank_correct, color = type)) +
    geom_point() + 
    geom_line(linetype = "dotted") + 
    theme(legend.title = element_blank()) +
    labs(x = "Sample Size", y = "Correct Rank Coverage", 
         title = "sl3 Importance Evaluation", 
         subtitle = "Correct Rank Coverage over all Simulations")
  pdf(paste0(path, "rank_coverage.pdf"))
  print(plot)
  dev.off()
  
  if(!is.null(mse_covariate_summary)){
    # folders to save performance plots
    dir.create(paste0(path, "MSE_plots"))
    dir.create(paste0(path, "MSE_plots/risk_difference"))
    dir.create(paste0(path, "MSE_plots/risk_ratio"))
    
    # main MSE summary plot, MSE across all covariates combined
    mse_summary$type <- paste(mse_summary$method, mse_summary$evaluation, sep = "_")
    diff_tbl <- mse_summary[mse_summary$importance_metric == "risk_difference", ]
    plot1 <- ggplot(diff_tbl, aes(x = n, y = sumMSE, color = type)) +
      geom_point() + 
      geom_line(linetype = "dotted") + 
      theme(legend.position = "bottom", legend.title = element_blank()) +
      labs(x = "Sample Size", y = "MSE", 
           title = "sl3 Importance Evaluation", 
           subtitle = "MSEs of Risk Difference Metric over all Simulations")
    pdf(paste0(path, "MSE_plots/risk_difference/allX.pdf"))
    print(plot1)
    dev.off()
    
    ratio_tbl <- mse_summary[mse_summary$importance_metric == "risk_ratio", ]
    plot2 <- ggplot(ratio_tbl, aes(x = n, y = sumMSE, color = type)) +
      geom_point() + 
      geom_line(linetype = "dotted") + 
      theme(legend.position = "bottom", legend.title = element_blank()) +
      labs(x = "Sample Size", y = "MSE", 
           title = "sl3 Importance Evaluation", 
           subtitle = "MSEs of Risk Ratio Metric over all Simulations")
    pdf(paste0(path, "MSE_plots/risk_ratio/allX.pdf"))
    print(plot2)
    dev.off()
    
    mse_covariate_summary$type <- paste(mse_covariate_summary$method, 
                                        mse_covariate_summary$evaluation, 
                                        sep = "_")
    ratio_tbl <- mse_covariate_summary[mse_covariate_summary$importance_metric == "risk_ratio", ]
    diff_tbl <- mse_covariate_summary[mse_covariate_summary$importance_metric == "risk_difference", ]
    covs <- colnames(mse_covariate_summary)[-which(colnames(mse_covariate_summary) %in% not_covs)]
    for(i in 1:length(covs)){
      cov <- covs[i]
      
      diff_tbl$y <- diff_tbl[,which(colnames(diff_tbl) == cov)]
      diff_plot <- ggplot(diff_tbl, aes(x = n, y = y, color = type)) +
        geom_point() + 
        geom_line(linetype = "dotted") + 
        theme(legend.position = "bottom", legend.title = element_blank()) +
        labs(x = "Sample Size", y = "MSE", 
             title = "sl3 Importance Evaluation", 
             subtitle = "MSEs of Risk Difference Metric over all Simulations")
      
      ratio_tbl$y <- ratio_tbl[,which(colnames(ratio_tbl) == cov)]
      ratio_plot <- ggplot(ratio_tbl, aes(x = n, y = y, color = type)) +
        geom_point() + 
        geom_line(linetype = "dotted") + 
        theme(legend.position = "bottom", legend.title = element_blank()) +
        labs(x = "Sample Size", y = "MSE", 
             title = "sl3 Importance Evaluation", 
             subtitle = "MSEs of Risk Ratio Metric over all Simulations")
      
      pdf(paste0(path, "MSE_plots/risk_difference/", cov, ".pdf"))
      print(diff_plot)
      dev.off()
      
      pdf(paste0(path, "MSE_plots/risk_ratio/", cov, ".pdf"))
      print(ratio_plot)
      dev.off()
    }
  }
}
