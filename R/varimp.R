#' Variable Importance Measures
#'
#' Function that takes a learner fitted task and loss function to generate a
#' table in which each row corresponds to the risk difference between when a
#' covariate is omitted and when that covariate is included. Specifically, we
#' calculate the risk ratio (default), or risk difference, between a learner
#' fit with a permuted covariate and the learner fit with the observed values
#' of that covariate, across all covariates.
#'
#' @param fit Learner fitted task.
#' @param loss Loss function (see \code{\link{loss_functions}}).
#' @param fold_number Either \code{"full"} for a variable importance measure
#'  with the full fit or \code{"validation"} (default) for a cross-validated
#'  variable importance measure, or a positive integer for a fold-specific
#'  variable importance measure.
#' @param type Either \code{"ratio"} (default) for the ratio of permuted risk
#'  over the observed risk, or \code{"difference"} for the difference between
#'  the permuted risk and the observed risk.
#'
#' @importFrom stats runif
#'
#' @return A table of variable importance for each covariate X, where a higher
#'  risk ratio/difference indicates that a given covariate is more important in
#'  predicting the outcome.
#'
#' @name varimp
#' @keywords variable importance
#'
#' @export
varimp <- function(fit,
                   loss,
                   fold_number = "validation",
                   type = c("ratio", "difference")) {
  # set defaults
  type <- match.arg(type)

  # extract task and data
  task <- fit$training_task
  dat <- task$data
  X <- task$nodes$covariates
  Y <- task$Y

  # get predictions and risk
  preds <- fit$predict_fold(task, fold_number = fold_number)
  risk <- mean(loss(preds, Y))

  importance <- lapply(X, function(i) {
    # scramble cov column and give it the same name as the raw cov col
    scrambled_col <- data.table(sample(
      unlist(dat[, i, with = FALSE]),
      nrow(dat)
    ))
    names(scrambled_col) <- i

    # replace raw col with scrambled col in the task
    scrambled_col_names <- task$add_columns(scrambled_col)
    scrambled_col_task <-
      task$next_in_chain(column_names = scrambled_col_names)

    # obtain preds on the scrambled col task
    scrambled_sl_preds <- fit$predict_fold(scrambled_col_task, fold_number)

    # risk on scrambled col task
    risk_scrambled <- mean(loss(scrambled_sl_preds, Y))

    if (type == "ratio") {
      varimp_metric <- risk_scrambled / risk
    } else if (type == "difference") {
      varimp_metric <- risk_scrambled - risk
    }
    return(varimp_metric)
  })

  names(importance) <- X
  if (type == "ratio") {
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
