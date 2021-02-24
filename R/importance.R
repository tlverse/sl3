#' Variable Importance
#'
#' Function that takes a cross-validated fit (i.e., cross-validated learner that
#' has already been trained on a task), which could be a cross-validated single
#' learner or super learner, to generate a loss-based variable importance
#' measure for each predictor, where the predictors are the covariates in the
#' trained task. This function generates a \code{data.table} in which each row
#' corresponds to the risk difference or risk ratio between the following
#' two risks: the risk when a predictor is permuted or removed, and the original
#' risk (i.e., when all predictors are included). A higher risk ratio/difference
#' corresponds to a more important predictor. A plot can be generated from the
#' returned \code{data.table} by calling companion function
#' \code{plot_importance}.
#'
#' @param fit A trained cross-validated learner (e.g., cv stack, super learner),
#'  from which cross-validated predictions can be generated.
#' @param loss The loss function for evaluating the risk. Defaults according to
#'  outcome type: squared error loss for continuous outcomes, and negative
#'  log-likelihood loss for discrete outcomes. See \code{\link{loss_functions}}.
#' @param fold_number The fold number to use for obtaining the predictions
#'  from the fit. Either a positive integer for obtaining predictions from a
#'  specific fold's fit; \code{"full"} for obtaining predictions from a fit on
#'  all of the data, or \code{"validation"} (default) for obtaining
#'  cross-validated predictions, where the data used for training and prediction
#'  never overlaps across the folds. Note that if a positive integer or
#'  \code{"full"} is supplied, then there will be overlap between the  data
#'  used for training and prediction.
#' @param type Which method should be used to obscure the relationship between
#'  the covariate and the outcome. When type is \code{"remove"} (default), each
#'  covariate is removed from the task and the cross-validated learner is refit
#'  to this modified task and then predictions are obtained from this refit.
#'  When type is \code{"permute"}, each covariate is permuted (sampled without
#'  replacement), and then predictions are obtained from this modified data with
#'  one permuted covariate.
#' @param importance_metric Either \code{"ratio"} (default), which returns
#'  the risk with the permuted/removed X divided by observed risk with all X; or
#'  \code{"difference"}, which returns the difference between the risk with the
#'  permuted/removed X and the observed risk.
#'
#' @importFrom data.table data.table
#'
#' @return A \code{data.table} of variable importance for each covariate.
#'
#' @name importance
#' @keywords variable importance
#'
#' @export
importance <- function(fit, loss = NULL, fold_number = "validation",
                       type = c("remove", "permute"),
                       importance_metric = c("ratio", "difference")) {

  # check arguments
  if (!fit$is_trained) {
    stop("Fit is not trained.")
  }

  # set defaults
  importance_metric <- match.arg(importance_metric)
  type <- match.arg(type)

  # extract task and data
  task <- fit$training_task
  d <- task$data
  X <- task$nodes$covariates
  Y <- task$Y

  if (is.null(loss)) {
    outcome_type <- task$outcome_type$type
    if (outcome_type %in% c("constant", "binomial")) {
      loss <- loss_loglik_binomial
    } else if (outcome_type == "categorical") {
      loss <- loss_loglik_multinomial
    } else if (outcome_type == "continuous") {
      loss <- loss_squared_error
    } else if (outcome_type == "multivariate") {
      loss <- loss_squared_error_multivariate
    } else {
      stop(paste0(
        "No default loss for outcome type ", outcome_type,
        ". Please specify your own."
      ))
    }
  }

  # get predictions and risk
  pred <- fit$predict_fold(task, fold_number = fold_number)
  original_risk <- mean(loss(pred, Y))

  # X-length list of importance scores
  res_list <- lapply(X, function(x) {
    if (type == "permute") {
      # permute covariate x, and give it the same name as the original x
      x_permuted <- data.table(sample(unlist(d[, x, with = FALSE]), nrow(d)))
      names(x_permuted) <- x
      # replace original x with permuted x, and update task with permuted x
      x_permuted_name <- task$add_columns(x_permuted)
      task_x_permuted <- task$next_in_chain(column_names = x_permuted_name)
      # obtain predictions & risk on the new task with permuted x
      x_permuted_pred <- fit$predict_fold(task_x_permuted, fold_number)
      no_x_risk <- mean(loss(x_permuted_pred, Y))
    } else if (type == "remove") {
      # modify learner to not include covariate x
      x_removed_lrnr <- fit$reparameterize(list(covariates = setdiff(X, x)))
      x_removed_fit <- x_removed_lrnr$train(task)
      x_removed_pred <- x_removed_fit$predict_fold(task, fold_number)
      no_x_risk <- mean(loss(x_removed_pred, Y))
    }
    # evaluate importance
    if (importance_metric == "ratio") {
      return(no_x_risk / original_risk)
    } else if (importance_metric == "difference") {
      return(no_x_risk - original_risk)
    }
  })
  names(res_list) <- X

  # importance results ordered by decreasing importance
  if (importance_metric == "ratio") {
    res <- data.table(X = names(res_list), risk_ratio = unlist(res_list))
    return(res[order(-res$risk_ratio)])
  } else if (importance_metric == "difference") {
    res <- data.table(X = names(res_list), risk_difference = unlist(res_list))
    return(res[order(-res$risk_difference)])
  }
}

#' Variable Importance Plot
#'
#' @param x The 2-column \code{data.table} returned by the call to
#'  \code{importance}, where the first column is the predictors and the second
#'  col
#' @param nvar The maximum number of predictors to be plotted. Defaults to 30.
#' @param ... Other parameters passed to \code{\link[graphics]{dotchart}}.
#'
#' @return A \code{\link[graphics]{dotchart}} of variable importance.
#'
#' @importFrom graphics dotchart
#'
#' @name importance_plot
#' @keywords variable importance
#'
#' @export
importance_plot <- function(x, nvar = min(30, nrow(x)), ...) {

  # get the importance metric
  if (grepl("ratio", colnames(x)[2])) {
    xlab <- "Risk Ratio"
  } else if (grepl("difference", colnames(x)[2])) {
    xlab <- "Risk Difference"
  }

  # sort by decreasing importance
  x_sorted <- x[order(-x[, 2])]
  # subset to include most at most nvar
  x_sorted <- x_sorted[1:(min(nvar, nrow(x_sorted))), ]
  # sort by increasing importance
  x_sorted <- x_sorted[order(x_sorted[, 2])]

  # make dotchart with most important variables on top
  # x_sorted[[2]] is importance scores & x_sorted[[1]] is covariate names
  dotchart(
    x = x_sorted[[2]], labels = x_sorted[[1]],
    xlab = xlab, ylab = "",
    xlim = c(min(x_sorted[[2]]), max(x_sorted[[2]])), ...
  )
}
