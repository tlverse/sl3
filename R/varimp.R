#' Variable Importance Measure
#'
#' Function that takes a learner fitted task and loss function to generate a
#' table where each row corresponds to the risk when we omit a covariate minus
#' the risk when we do not omit the covariate. Specifically, we calculate the
#' risk difference between the learner fit with a scrambled covariate and the
#' leaner fit with the true covariate, across all covariates.
#'
#' @param fit A learner fitted task
#' @param loss A loss function (see loss_functions.R)
#' @param fold_number either "full" for full fit vim, "validation" for
#'             cross-validated vim, or a positive integer for fold-specific vim
#' @return A table of risk differences for each covariate, where a higher risk
#'         difference indicates a more important covariate in predicting Y
#'
#' @name varimp
#'
#' @export
#' @importFrom stats runif
#' @keywords variable importance
varimp <- function(fit, loss, fold_number = "validation") {
  task <- fit$training_task
  Y <- task$Y

  preds <- fit$predict_fold(task, fold_number = fold_number)
  risk <- mean(loss(Y, preds))


  X <- task$nodes$covariates
  dat <- task$data

  risk_diffs <- lapply(X, function(i) {
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
    risk_scrambled <- mean(loss(Y, scrambled_sl_preds))

    # calculate risk difference
    rd <- risk_scrambled - risk
    return(rd)
  })

  names(risk_diffs) <- X
  results <- data.table(
    X = names(risk_diffs),
    risk_diff = unlist(risk_diffs)
  )
  results_ord <- results[ order(-results$risk_diff) ]
  return(results_ord)
}
