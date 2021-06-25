#' Variable Importance
#'
#' Function that takes a cross-validated fit (i.e., cross-validated learner that
#' has already been trained on a task), which could be a cross-validated single
#' learner or super learner, to generate a risk-based variable importance
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
#' @param eval_fun The evaluation function (risk or loss function) for
#'  evaluating the risk. Defaults vary based on the outcome type: squared error
#'  loss for continuous outcomes, and negative log-likelihood loss for discrete
#'  outcomes. See \code{\link{loss_functions}} and \code{\link{risk_functions}}
#'  for options.
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
importance <- function(fit, eval_fun = NULL, fold_number = "validation",
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

  if (is.null(eval_fun)) {
    outcome_type <- task$outcome_type$type
    if (outcome_type %in% c("constant", "binomial")) {
      eval_fun <- loss_loglik_binomial
    } else if (outcome_type == "categorical") {
      eval_fun <- loss_loglik_multinomial
    } else if (outcome_type == "continuous") {
      eval_fun <- loss_squared_error
    } else if (outcome_type == "multivariate") {
      eval_fun <- loss_squared_error_multivariate
    } else {
      stop(paste0(
        "No default eval_fun for outcome type ", outcome_type,
        ". Please specify your own."
      ))
    }
  }

  # get predictions and risk
  pred <- fit$predict_fold(task, fold_number = fold_number)
  eval_result <- eval_fun(pred, Y)
  if (!is.null(attr(eval_result, "risk"))) {
    original_risk <- eval_result
    if (!is.null(attr(eval_result, "transform"))) {
      type <- attr(eval_result, "transform")
      original_risk <- transform_risk(eval_result, type)
      # note that original_risk now is not a risk, since it's been transformed,
      # but we keep the name as is to streamline the functionality
    }
  } else {
    losses <- eval_result
    original_risk <- mean(eval_result)
  }

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
      no_x_eval_result <- eval_fun(x_permuted_pred, Y)
    } else if (type == "remove") {
      # modify learner to not include covariate x
      x_removed_lrnr <- fit$reparameterize(list(covariates = setdiff(X, x)))
      x_removed_fit <- x_removed_lrnr$train(task)
      x_removed_pred <- x_removed_fit$predict_fold(task, fold_number)
      no_x_eval_result <- eval_fun(x_removed_pred, Y)
    }

    if (!is.null(attr(no_x_eval_result, "risk"))) {
      no_x_risk <- no_x_eval_result
      if (!is.null(attr(no_x_eval_result, "transform"))) {
        no_x_risk <- transform_risk(no_x_eval_result, type)
        # note that no_x_eval_result now is not a risk, since it's been
        # transformed, but we keep the name as is to streamline things
      }
    } else {
      no_x_losses <- no_x_eval_result
      no_x_risk <- mean(no_x_losses)
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
  result <- data.table(covariate = names(res_list), metric = unlist(res_list))
  result <- result[order(-result$metric)]

  # name the importance metric appropriately
  metric_name <- paste0("risk_", importance_metric)
  if (!is.null(attr(eval_result, "name"))) {
    metric_name <- gsub("risk", attr(eval_result, "name"), metric_name)
  }
  colnames(result)[2] <- metric_name

  return(result)
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
  xlab <- colnames(x)[2]

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
