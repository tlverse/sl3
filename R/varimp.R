#' Variable Importance Measures
#'
#' Function that takes a learner fitted task and loss function to generate a
#' table where each row corresponds to the risk when we omit a covariate minus
#' the risk when we do not omit the covariate. Specifically, we calculate the
#' risk ratio (default), or difference, between the learner fit with a scrambled 
#' covariate and the learner fit with the true covariate, across all covariates.
#'
#' @param fit learner fitted task
#' @param loss loss function (see loss_functions.R)
#' @param fold_number either "full" for full fit vim, "validation" (default) for
#'                    cross-validated vim, or a positive integer for 
#'                    fold-specific vim
#' @param type either "ratio" (default) for the ratio of scrambled risk over the 
#'             true risk, or "difference" for the difference between the 
#'             scrambled risk and the true risk
#' @return A table of variable importance for each covariate, X, where a higher 
#'         risk ratio/difference indicates a more important covariate in 
#'         predicting the outcome
#'
#' @name varimp
#'
#' @export
#' @importFrom stats runif
#' @keywords variable importance
#
varimp <- function(fit, loss, fold_number = "validation", type = "ratio") {
  task <- fit$training_task
  Y <- task$Y

  preds <- fit$predict_fold(task, fold_number = fold_number)
  risk <- mean(loss(preds, Y))

  X <- task$nodes$covariates
  dat <- task$data

  importance <- lapply(X, function(i) {
    # scramble cov column and give it the same name as the raw cov col
    scrambled_col <- data.table(sample(
      unlist(dat[, i, with = FALSE]),
      nrow(dat)
    ))
    names(scrambled_col) <- i

    # replace raw col with scrambled col in the task
    scrambled_col_names <- task$add_columns(scrambled_col)
    scrambled_col_task <- task$next_in_chain(column_names = scrambled_col_names)

    # obtain preds on the scrambled col task
    scrambled_sl_preds <- fit$predict_fold(scrambled_col_task, fold_number)

    # risk on scrambled col task
    risk_scrambled <- mean(loss(scrambled_sl_preds, Y))

    if(type == "ratio") {
      varimp_metric <- risk_scrambled/risk
    } else if (type == "difference") {
      varimp_metric <- risk_scrambled - risk
    }
    return(varimp_metric)
  })

  names(importance) <- X
  if(type == "ratio"){
    results <- data.table(
      X = names(importance), 
      risk_ratio = unlist(importance)
      )
    results_ordered <- results[order(-results$risk_ratio)]
  } else if (type == "difference") {
    results <- data.table(
      X = names(importance),
      risk_difference = unlist(importance)
      )
    results_ordered <- results[order(-results$risk_difference)]
  }
  return(results_ordered)
}
